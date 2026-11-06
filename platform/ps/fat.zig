//! FAT12/FAT16/FAT32 for the PS app (bare metal, no allocator), over any
//! block device with 512-byte blocks; reading all three, and on FAT32 (every
//! SD card the board takes) making files and folders (side track K, F1:
//! the board makes its saves, states and movies when it first needs them):
//!
//!   const Fat = fat.Fs(Dev);   // Dev: pub fn readBlock(*Dev, lba: u32, *[512]u8) !void
//!   var fs = try Fat.mount(&dev);
//!   var it = fs.iterate(fs.root());
//!   while (try it.next()) |e| ... e.name(), e.is_dir, e.size
//!   const n = try fs.readFile(entry, &buf);
//!   const e = try fs.create(dir, "name.sav", size);  // Dev: + writeBlock
//!   const d = try fs.mkdir(dir, "saves");
//!
//! Writing (FAT32 only): clusters are taken from the free ones (from the
//! FSInfo sector's hint, contiguous where it can), their data zeroed, the
//! chain written to every FAT copy, and the directory entry (a long name
//! and its 8.3 alias, in one sector) last: a write cut off (the power)
//! leaves at worst clusters no file uses ("lost clusters", which fsck
//! frees), never an entry pointing at a half-made chain. FSInfo's free
//! count is set to unknown at the first write. No delete, no growing.
//!
//! Source: Microsoft, "FAT32 File System Specification" (fatgen103), version
//! 1.03: the BPB fields, the FAT type from the cluster count (section 3.5:
//! < 4085 clusters is FAT12, < 65525 FAT16, else FAT32), FAT entry
//! encodings (section 4), directory entries (section 6) and long names
//! (Microsoft's "Long File Name" format, also in fatgen103 section 7, with
//! its checksum). The volume is found either at LBA 0 ("superfloppy", no
//! partition table) or in the first FAT partition of an MBR (partition
//! types 0x01, 0x04, 0x06, 0x0B, 0x0C, 0x0E).
//!
//! Limits: 512-byte sectors only (every SD card), only the first FAT copy is
//! read, names are kept to `max_name` characters (longer ones are cut, so
//! `find` can't match them), non-ASCII characters of long names read as
//! '?'. Lower-case 8.3 names (the Windows NT case bits in byte 12 of the
//! entry, which mtools and Windows write instead of a long name) are shown
//! lower-case. Damaged volumes give errors (`error.BadCluster`,
//! `error.ChainLoop`, ...), never a panic or an endless loop.

const std = @import("std");

pub const FatType = enum { fat12, fat16, fat32 };

pub const max_name = 63;

pub const Entry = struct {
    name_buf: [max_name]u8 = undefined,
    name_len: u8 = 0,
    /// The 8.3 name as "NAME.EXT" (case bits applied).
    short_buf: [12]u8 = undefined,
    short_len: u8 = 0,
    is_dir: bool = false,
    size: u32 = 0,
    first_cluster: u32 = 0,
    /// where its 8.3 directory entry is: the sector, the byte in it
    /// (`rewrite` changes the size there)
    at_lba: u32 = 0,
    at_off: u16 = 0,

    /// The long name if there is a valid one, else the 8.3 name.
    pub fn name(e: *const Entry) []const u8 {
        return e.name_buf[0..e.name_len];
    }

    pub fn shortName(e: *const Entry) []const u8 {
        return e.short_buf[0..e.short_len];
    }
};

/// A directory: its first cluster, or 0 for the fixed FAT12/FAT16 root.
pub const Dir = struct { cluster: u32 };

pub fn Fs(comptime Dev: type) type {
    return struct {
        const Self = @This();

        dev: *Dev,
        fat_type: FatType,
        /// Absolute LBAs of the first FAT, the fixed root directory
        /// (FAT12/16) and cluster 2.
        fat_lba: u32,
        root_lba: u32,
        root_sectors: u32,
        data_lba: u32,
        root_cluster: u32,
        sec_per_clus: u32,
        cluster_count: u32,
        /// writing: the FATs (their count and size in sectors), the FSInfo
        /// sector (0: none)
        fat_count: u32 = 1,
        fat_size: u32 = 0,
        fsinfo_lba: u32 = 0,

        buf: [512]u8 = undefined,
        buf_lba: ?u32 = null,
        fat_buf: [512]u8 = undefined,
        fat_buf_lba: ?u32 = null,
        /// writing: fat_buf changed and not yet on the card; the next free
        /// cluster to look at; FSInfo's free count made unknown
        fat_dirty: bool = false,
        alloc_hint: u32 = 2,
        fsinfo_done: bool = false,

        pub fn mount(dev: *Dev) !Self {
            var fs: Self = undefined;
            try fs.mountInto(dev);
            return fs;
        }

        /// `mount` in place: the PS app's file system is a global, and a
        /// struct literal of all of it (its two 512-byte buffers too) is
        /// kept whole in the OCM as the value to copy from (1092 bytes,
        /// twice; side K, 2026-10-18). So each field is set on its own:
        /// a new field must be set here too (the count below says so).
        pub fn mountInto(fs: *Self, dev: *Dev) !void {
            comptime std.debug.assert(@typeInfo(Self).@"struct".fields.len == 19);
            var b: [512]u8 = undefined;
            try dev.readBlock(0, &b);
            var vol: u32 = 0;
            if (!isBootSector(&b)) {
                if (b[510] != 0x55 or b[511] != 0xAA) return error.NoFilesystem;
                vol = for (0..4) |i| {
                    const p = b[446 + 16 * i ..][0..16];
                    const start = le32(p[8..12]);
                    switch (p[4]) {
                        0x01, 0x04, 0x06, 0x0B, 0x0C, 0x0E => if (start != 0) break start,
                        else => {},
                    }
                } else return error.NoFilesystem;
                try dev.readBlock(vol, &b);
                if (!isBootSector(&b)) return error.NoFilesystem;
            }
            if (le16(b[11..13]) != 512) return error.UnsupportedSectorSize;

            const spc: u32 = b[13];
            const rsvd: u32 = le16(b[14..16]);
            const nfats: u32 = b[16];
            const root_ents: u32 = le16(b[17..19]);
            const tot16: u32 = le16(b[19..21]);
            const fsz16: u32 = le16(b[22..24]);
            const tot: u32 = if (tot16 != 0) tot16 else le32(b[32..36]);
            const fsz: u32 = if (fsz16 != 0) fsz16 else le32(b[36..40]);
            const root_secs = (root_ents * 32 + 511) / 512;
            if (fsz == 0) return error.NoFilesystem;
            const meta = @as(u64, rsvd) + @as(u64, nfats) * fsz + root_secs;
            if (meta >= tot) return error.NoFilesystem;
            const clusters: u32 = @intCast((tot - meta) / spc);
            const ft: FatType = if (clusters < 4085) .fat12 else if (clusters < 65525) .fat16 else .fat32;
            const root_clus: u32 = if (ft == .fat32) le32(b[44..48]) else 0;
            if (ft == .fat32 and (root_ents != 0 or root_clus < 2 or root_clus >= clusters + 2))
                return error.NoFilesystem;
            if (ft != .fat32 and root_ents == 0) return error.NoFilesystem;

            const fsinfo: u32 = if (ft == .fat32) le16(b[48..50]) else 0;
            fs.dev = dev;
            fs.fat_type = ft;
            fs.fat_lba = vol + rsvd;
            fs.root_lba = vol + rsvd + nfats * fsz;
            fs.root_sectors = root_secs;
            fs.data_lba = @intCast(vol + meta);
            fs.root_cluster = root_clus;
            fs.sec_per_clus = spc;
            fs.cluster_count = clusters;
            fs.fat_count = nfats;
            fs.fat_size = fsz;
            fs.fsinfo_lba = if (fsinfo != 0 and fsinfo != 0xFFFF and fsinfo < rsvd) vol + fsinfo else 0;
            fs.buf_lba = null;
            fs.fat_buf_lba = null;
            fs.fat_dirty = false;
            fs.alloc_hint = 2;
            fs.fsinfo_done = false;
            // the next-free hint, if FSInfo has a plausible one
            if (fs.fsinfo_lba != 0) {
                try dev.readBlock(fs.fsinfo_lba, &b);
                if (le32(b[0..4]) == 0x41615252 and le32(b[484..488]) == 0x61417272) {
                    const hint = le32(b[492..496]);
                    if (hint >= 2 and hint < clusters + 2) fs.alloc_hint = hint;
                } else fs.fsinfo_lba = 0;
            }
        }

        pub fn root(fs: *const Self) Dir {
            return .{ .cluster = fs.root_cluster };
        }

        /// The directory an entry names (a ".." style cluster 0 means the
        /// root).
        pub fn openDir(fs: *const Self, e: Entry) Dir {
            return if (e.first_cluster == 0) fs.root() else .{ .cluster = e.first_cluster };
        }

        pub fn iterate(fs: *Self, dir: Dir) Iterator {
            return .{ .fs = fs, .cluster = dir.cluster };
        }

        /// The entry called `name` in `dir` (long or 8.3 name, ignoring
        /// case), or null.
        pub fn find(fs: *Self, dir: Dir, name: []const u8) !?Entry {
            var it = fs.iterate(dir);
            while (try it.next()) |e| {
                if (std.ascii.eqlIgnoreCase(e.name(), name) or std.ascii.eqlIgnoreCase(e.shortName(), name)) return e;
            }
            return null;
        }

        /// Reads the start of a file into `out`: min(size, out.len) bytes,
        /// which it returns.
        pub fn readFile(fs: *Self, e: Entry, out: []u8) !usize {
            var r = fs.reader(e);
            var done: usize = 0;
            while (done < out.len) {
                const chunk = try r.next() orelse break;
                const k = @min(chunk.len, out.len - done);
                @memcpy(out[done..][0..k], chunk[0..k]);
                done += k;
            }
            return done;
        }

        /// A file a sector at a time, for files larger than any buffer the
        /// PS app has (a 256 KiB cartridge: OCM is 192 KiB).
        pub fn reader(fs: *Self, e: Entry) Reader {
            return .{ .fs = fs, .cluster = e.first_cluster, .left = e.size };
        }

        pub const Reader = struct {
            fs: *Self,
            cluster: u32,
            /// sector in the cluster
            sec: u32 = 0,
            left: u32,
            steps: u32 = 0,

            /// The next up to 512 bytes of the file (valid until the next
            /// call that reads the card), or null at its end.
            pub fn next(r: *Reader) !?[]const u8 {
                if (r.left == 0) return null;
                if (r.sec == r.fs.sec_per_clus) {
                    r.steps += 1;
                    if (r.steps > r.fs.cluster_count) return error.ChainLoop;
                    r.cluster = try r.fs.nextCluster(r.cluster) orelse return error.ShortChain;
                    r.sec = 0;
                }
                try r.fs.checkCluster(r.cluster);
                const data = try r.fs.sector(r.fs.clusterLba(r.cluster) + r.sec);
                r.sec += 1;
                const k: u32 = @min(512, r.left);
                r.left -= k;
                return data[0..k];
            }
        };

        /// A file as runs of consecutive card blocks, for multi-block
        /// reads (side track F: a 4 MB bitstream in a few commands instead
        /// of 8000). Clusters that follow each other on the card are
        /// merged, up to `max_blocks` per run.
        pub fn runs(fs: *Self, e: Entry, max_blocks: u32) Runs {
            return .{ .fs = fs, .cluster = e.first_cluster, .left = e.size, .max = @max(max_blocks, fs.sec_per_clus) };
        }

        pub const Run = struct {
            lba: u32,
            blocks: u32,
            /// bytes of the file in these blocks (the last block of the
            /// file may be partly past its end)
            bytes: u32,
        };

        pub const Runs = struct {
            fs: *Self,
            /// next cluster to read (0: done)
            cluster: u32,
            left: u32,
            max: u32,
            steps: u32 = 0,

            pub fn next(r: *Runs) !?Run {
                if (r.left == 0) return null;
                const fs = r.fs;
                const start = r.cluster;
                try fs.checkCluster(start);
                const spc = fs.sec_per_clus;
                var n: u32 = 1; // clusters in this run
                var c = start;
                while (true) {
                    if (n * spc * 512 >= r.left) {
                        r.cluster = 0;
                        break;
                    }
                    r.steps += 1;
                    if (r.steps > fs.cluster_count) return error.ChainLoop;
                    const nx = try fs.nextCluster(c) orelse return error.ShortChain;
                    if (nx != c + 1 or (n + 1) * spc > r.max) {
                        r.cluster = nx;
                        break;
                    }
                    c = nx;
                    n += 1;
                }
                const bytes = @min(r.left, n * spc * 512);
                r.left -= bytes;
                return .{ .lba = fs.clusterLba(start), .blocks = (bytes + 511) / 512, .bytes = bytes };
            }
        };

        pub const Iterator = struct {
            fs: *Self,
            /// Current cluster (0: the fixed root).
            cluster: u32,
            /// Sector in the cluster (or in the fixed root), entry in it.
            sec: u32 = 0,
            ent: u32 = 0,
            steps: u32 = 0,
            done: bool = false,
            /// Long name being collected: the sequence number expected
            /// next (0: none), its checksum, its characters.
            lfn_next: u8 = 0,
            lfn_sum: u8 = 0,
            lfn_len: u16 = 0,
            lfn: [20 * 13]u8 = undefined,

            pub fn next(it: *Iterator) !?Entry {
                while (!it.done) {
                    const lba = try it.position() orelse {
                        it.done = true;
                        break;
                    };
                    const sec = try it.fs.sector(lba);
                    const d = sec[it.ent * 32 ..][0..32];
                    it.ent += 1;
                    if (d[0] == 0x00) {
                        it.done = true;
                        break;
                    }
                    if (d[0] == 0xE5) {
                        it.lfn_next = 0;
                        continue;
                    }
                    const attr = d[11];
                    if (attr & 0x3F == 0x0F) {
                        it.longPart(d);
                        continue;
                    }
                    const lfn_ok = it.lfn_next == 1 and it.lfn_sum == shortSum(d[0..11]);
                    it.lfn_next = 0;
                    if (attr & 0x08 != 0 or d[0] == '.') continue;
                    var e: Entry = .{
                        .is_dir = attr & 0x10 != 0,
                        .size = le32(d[28..32]),
                        .first_cluster = (if (it.fs.fat_type == .fat32) @as(u32, le16(d[20..22])) << 16 else 0) | le16(d[26..28]),
                        .at_lba = lba,
                        .at_off = @intCast((it.ent - 1) * 32),
                    };
                    e.short_len = formatShort(d, &e.short_buf);
                    if (lfn_ok) {
                        const k = @min(it.lfn_len, max_name);
                        @memcpy(e.name_buf[0..k], it.lfn[0..k]);
                        e.name_len = @intCast(k);
                    } else {
                        @memcpy(e.name_buf[0..e.short_len], e.short_buf[0..e.short_len]);
                        e.name_len = e.short_len;
                    }
                    return e;
                }
                return null;
            }

            /// LBA of the current entry's sector, moving on to the next
            /// sector or cluster when the current one is used up; null at
            /// the end of the directory.
            fn position(it: *Iterator) !?u32 {
                const fs = it.fs;
                if (it.ent == 16) {
                    it.ent = 0;
                    it.sec += 1;
                }
                if (it.cluster == 0) {
                    return if (it.sec < fs.root_sectors) fs.root_lba + it.sec else null;
                }
                if (it.sec == fs.sec_per_clus) {
                    it.sec = 0;
                    it.steps += 1;
                    if (it.steps > fs.cluster_count) return error.ChainLoop;
                    it.cluster = try fs.nextCluster(it.cluster) orelse return null;
                }
                try fs.checkCluster(it.cluster);
                return fs.clusterLba(it.cluster) + it.sec;
            }

            fn longPart(it: *Iterator, d: *const [32]u8) void {
                const seq = d[0] & 0x1F;
                if (d[0] & 0x40 != 0) {
                    if (seq == 0 or seq > 20) {
                        it.lfn_next = 0;
                        return;
                    }
                    it.lfn_sum = d[13];
                    it.lfn_len = @as(u16, seq) * 13;
                } else if (it.lfn_next == 0 or seq != it.lfn_next - 1 or d[13] != it.lfn_sum) {
                    it.lfn_next = 0;
                    return;
                }
                it.lfn_next = seq;
                const offs = [13]u8{ 1, 3, 5, 7, 9, 14, 16, 18, 20, 22, 24, 28, 30 };
                const base = (@as(u16, seq) - 1) * 13;
                for (offs, 0..) |o, i| {
                    const ch = le16(d[o..][0..2]);
                    const pos = base + @as(u16, @intCast(i));
                    if (ch == 0x0000 or ch == 0xFFFF) {
                        if (pos < it.lfn_len) it.lfn_len = pos;
                        break;
                    }
                    it.lfn[pos] = if (ch >= 0x20 and ch < 0x7F) @intCast(ch) else '?';
                }
            }
        };

        /// The card block holding the start of a file (its first cluster's
        /// first sector): the one write the PS app does to the file
        /// system, rewriting a small file in place (side track F, the last
        /// used system); the FAT and directories are never written.
        pub fn firstBlock(fs: *const Self, e: Entry) !u32 {
            if (e.is_dir or e.size == 0) return error.NoData;
            try fs.checkCluster(e.first_cluster);
            return fs.clusterLba(e.first_cluster);
        }

        fn clusterLba(fs: *const Self, c: u32) u32 {
            return fs.data_lba + (c - 2) * fs.sec_per_clus;
        }

        fn checkCluster(fs: *const Self, c: u32) !void {
            if (c < 2 or c >= fs.cluster_count + 2) return error.BadCluster;
        }

        /// The cluster after `c` in its chain, or null at the end of it.
        fn nextCluster(fs: *Self, c: u32) !?u32 {
            try fs.checkCluster(c);
            const v: u32, const eoc: u32, const bad: u32 = switch (fs.fat_type) {
                .fat12 => blk: {
                    const off = c + c / 2;
                    const w = @as(u32, try fs.fatByte(off)) | @as(u32, try fs.fatByte(off + 1)) << 8;
                    break :blk .{ if (c & 1 != 0) w >> 4 else w & 0xFFF, 0xFF8, 0xFF7 };
                },
                .fat16 => .{ @as(u32, try fs.fatByte(c * 2)) | @as(u32, try fs.fatByte(c * 2 + 1)) << 8, 0xFFF8, 0xFFF7 },
                .fat32 => blk: {
                    const sec = try fs.fatSector(c * 4 / 512);
                    break :blk .{ le32(sec[c * 4 % 512 ..][0..4]) & 0x0FFF_FFFF, 0x0FFF_FFF8, 0x0FFF_FFF7 };
                },
            };
            if (v >= eoc) return null;
            if (v == bad) return error.BadCluster;
            try fs.checkCluster(v);
            return v;
        }

        fn fatByte(fs: *Self, off: u32) !u8 {
            return (try fs.fatSector(off / 512))[off % 512];
        }

        fn fatSector(fs: *Self, n: u32) !*const [512]u8 {
            const lba = fs.fat_lba + n;
            if (fs.fat_buf_lba != lba) {
                if (comptime @hasDecl(Dev, "writeBlock")) try fs.flushFat();
                fs.fat_buf_lba = null;
                try fs.dev.readBlock(lba, &fs.fat_buf);
                fs.fat_buf_lba = lba;
            }
            return &fs.fat_buf;
        }

        fn sector(fs: *Self, lba: u32) !*const [512]u8 {
            if (fs.buf_lba != lba) {
                fs.buf_lba = null;
                try fs.dev.readBlock(lba, &fs.buf);
                fs.buf_lba = lba;
            }
            return &fs.buf;
        }

        // --- Writing (FAT32; side track K, F1) --------------------------

        /// A file of `size` bytes (zeros) called `name` in `dir`.
        /// error.Exists if `dir` has one (any case), error.CardFull,
        /// error.ReadOnly on FAT12/FAT16.
        pub fn create(fs: *Self, dir: Dir, name: []const u8, size: u32) !Entry {
            try fs.writable(dir, name);
            const per = fs.sec_per_clus * 512;
            const n = (size + per - 1) / per;
            const first = if (n == 0) 0 else try fs.allocChain(n);
            try fs.flushFat();
            try fs.addEntry(dir, name, 0x20, first, size);
            return (try fs.find(dir, name)) orelse error.NotWritten;
        }

        /// A file's contents replaced by `data`, its size made data.len
        /// (the board's key map, written from the controls menu; side K,
        /// 2026-10-18): its chain grown by clusters as needed (never
        /// shortened: a smaller file keeps its clusters), the bytes
        /// written, then the directory entry's size (and first cluster,
        /// if it had none). Cut off before the entry: the old size over
        /// the new bytes. FAT32 only. The entry as it is now.
        pub fn rewrite(fs: *Self, e: Entry, data: []const u8) !Entry {
            if (fs.fat_type != .fat32) return error.ReadOnly;
            if (e.is_dir or e.at_lba == 0) return error.NotAFile;
            const per = fs.sec_per_clus * 512;
            const need: u32 = @intCast((data.len + per - 1) / per);
            // the chain's length and last cluster; grown to `need`
            var first = e.first_cluster;
            var have: u32 = 0;
            var last: u32 = 0;
            var c: ?u32 = if (first == 0) null else first;
            while (c) |x| : (c = try fs.nextCluster(x)) {
                try fs.checkCluster(x);
                have += 1;
                if (have > fs.cluster_count) return error.ChainLoop;
                last = x;
            }
            if (need > have) {
                try fs.fsinfoUnknown();
                const more = try fs.allocChain(need - have);
                if (last == 0) first = more else try fs.setFat(last, more);
                try fs.flushFat();
            }
            // the bytes, sector by sector along the chain
            var b: [512]u8 = undefined;
            var off: usize = 0;
            c = if (data.len == 0) null else first;
            while (c) |x| : (c = try fs.nextCluster(x)) {
                for (0..fs.sec_per_clus) |k| {
                    if (off >= data.len) break;
                    const n = @min(512, data.len - off);
                    @memcpy(b[0..n], data[off..][0..n]);
                    @memset(b[n..], 0);
                    try fs.writeSector(fs.clusterLba(x) + @as(u32, @intCast(k)), &b);
                    off += n;
                }
                if (off >= data.len) break;
            }
            // the entry (no bytes: no cluster, the chain freed below)
            if (need == 0) first = 0;
            b = (try fs.sector(e.at_lba)).*;
            const d = b[e.at_off..][0..32];
            std.mem.writeInt(u32, d[28..32], @intCast(data.len), .little);
            std.mem.writeInt(u16, d[20..22], @truncate(first >> 16), .little);
            std.mem.writeInt(u16, d[26..28], @truncate(first), .little);
            try fs.writeSector(e.at_lba, &b);
            // a smaller file: the clusters past its new end freed (a chain
            // must be as long as its size; cut off before this: a chain
            // longer than the size, which fsck shortens, as lost clusters)
            if (need < have) {
                try fs.fsinfoUnknown();
                var k: u32 = 0;
                var x: ?u32 = e.first_cluster;
                while (x) |cl| : (k += 1) {
                    const nx = try fs.nextCluster(cl);
                    if (k + 1 == need) try fs.setFat(cl, 0x0FFF_FFFF) else if (k + 1 > need) try fs.setFat(cl, 0);
                    x = nx;
                }
                try fs.flushFat();
            }
            var out = e;
            out.size = @intCast(data.len);
            out.first_cluster = first;
            return out;
        }

        /// A folder called `name` in `dir`, empty.
        pub fn mkdir(fs: *Self, dir: Dir, name: []const u8) !Dir {
            try fs.writable(dir, name);
            const c = try fs.allocChain(1);
            // "." and "..": the parent as cluster 0 if it is the root
            var b = [_]u8{0} ** 512;
            const parent = if (dir.cluster == fs.root_cluster) 0 else dir.cluster;
            dotEntry(b[0..32], ".          ", c);
            dotEntry(b[32..64], "..         ", parent);
            try fs.writeSector(fs.clusterLba(c), &b);
            try fs.flushFat();
            try fs.addEntry(dir, name, 0x10, c, 0);
            return .{ .cluster = c };
        }

        fn writable(fs: *Self, dir: Dir, name: []const u8) !void {
            if (fs.fat_type != .fat32) return error.ReadOnly;
            if (name.len == 0 or name.len > max_name) return error.BadName;
            for (name) |ch| if (ch < 0x20 or ch >= 0x7F or std.mem.indexOfScalar(u8, "\"*/:<>?\\|", ch) != null) return error.BadName;
            if (try fs.find(dir, name) != null) return error.Exists;
            try fs.fsinfoUnknown();
        }

        /// FSInfo's free count and hint made "unknown" (0xFFFFFFFF) before
        /// the first allocation, so no reader trusts a stale count.
        fn fsinfoUnknown(fs: *Self) !void {
            if (fs.fsinfo_done or fs.fsinfo_lba == 0) return;
            var b: [512]u8 = undefined;
            try fs.dev.readBlock(fs.fsinfo_lba, &b);
            std.mem.writeInt(u32, b[488..492], 0xFFFF_FFFF, .little);
            std.mem.writeInt(u32, b[492..496], 0xFFFF_FFFF, .little);
            try fs.writeSector(fs.fsinfo_lba, &b);
            fs.fsinfo_done = true;
        }

        /// A chain of `n` clusters, zeroed, in the FATs (fat_buf: flushFat
        /// writes the last sector); its first cluster.
        fn allocChain(fs: *Self, n: u32) !u32 {
            var first: u32 = 0;
            var prev: u32 = 0;
            var left = n;
            while (left > 0) {
                const start, const len = try fs.freeRun(left);
                var i: u32 = 0;
                while (i < len) : (i += 1) {
                    const c = start + i;
                    try fs.zeroCluster(c);
                    try fs.setFat(c, if (i + 1 < len) c + 1 else 0x0FFF_FFFF);
                }
                if (prev != 0) try fs.setFat(prev, start);
                if (first == 0) first = start;
                prev = start + len - 1;
                left -= len;
            }
            return first;
        }

        /// The first free cluster from the hint on (round the volume once)
        /// and how many free ones follow it, up to `want`.
        fn freeRun(fs: *Self, want: u32) !struct { u32, u32 } {
            const end = fs.cluster_count + 2;
            var c = fs.alloc_hint;
            var seen: u32 = 0;
            while (seen < fs.cluster_count) : (seen += 1) {
                if (c >= end) c = 2;
                if (try fs.fatEntry(c) == 0) {
                    var len: u32 = 1;
                    while (len < want and c + len < end and try fs.fatEntry(c + len) == 0) len += 1;
                    fs.alloc_hint = c + len;
                    return .{ c, len };
                }
                c += 1;
            }
            return error.CardFull;
        }

        fn fatEntry(fs: *Self, c: u32) !u32 {
            const sec = try fs.fatSector(c * 4 / 512);
            return le32(sec[c * 4 % 512 ..][0..4]) & 0x0FFF_FFFF;
        }

        /// Cluster c's FAT entry (the top 4 bits kept, as fatgen103 asks).
        fn setFat(fs: *Self, c: u32, v: u32) !void {
            _ = try fs.fatSector(c * 4 / 512);
            const at = fs.fat_buf[c * 4 % 512 ..][0..4];
            std.mem.writeInt(u32, at, (le32(at) & 0xF000_0000) | (v & 0x0FFF_FFFF), .little);
            fs.fat_dirty = true;
        }

        /// The changed FAT sector to every FAT copy.
        pub fn flushFat(fs: *Self) !void {
            if (!fs.fat_dirty) return;
            const lba = fs.fat_buf_lba.?;
            for (0..fs.fat_count) |k| try fs.dev.writeBlock(lba + @as(u32, @intCast(k)) * fs.fat_size, &fs.fat_buf);
            fs.fat_dirty = false;
        }

        fn zeroCluster(fs: *Self, c: u32) !void {
            const zeros = [_]u8{0} ** 512;
            for (0..fs.sec_per_clus) |k| try fs.writeSector(fs.clusterLba(c) + @as(u32, @intCast(k)), &zeros);
        }

        /// A block written, the read cache kept in step.
        fn writeSector(fs: *Self, lba: u32, b: *const [512]u8) !void {
            try fs.dev.writeBlock(lba, b);
            if (fs.buf_lba == lba) fs.buf = b.*;
        }

        /// The directory entry for `name` (its long-name entries and the 8.3
        /// alias, in one sector): free slots found or the folder grown by a
        /// cluster; slots past the end marker that come before them marked
        /// deleted, so the folder doesn't end before the new entry.
        fn addEntry(fs: *Self, dir: Dir, name: []const u8, attr: u8, cluster: u32, size: u32) !void {
            var short: [11]u8 = undefined;
            try fs.alias(dir, name, &short);
            const lfn: u32 = @intCast((name.len + 12) / 13);
            const k = lfn + 1;
            const lba, const ent = try fs.freeSlots(dir, k);
            try fs.unmarkEnd(dir, lba, ent);
            var b = (try fs.sector(lba)).*;
            const sum = shortSum(&short);
            for (0..lfn) |i| {
                const seq: u8 = @intCast(lfn - i);
                lfnEntry(b[(ent + i) * 32 ..][0..32], name, seq, i == 0, sum);
            }
            const d = b[(ent + lfn) * 32 ..][0..32];
            @memset(d, 0);
            @memcpy(d[0..11], &short);
            d[11] = attr;
            std.mem.writeInt(u16, d[16..18], dos_date, .little);
            std.mem.writeInt(u16, d[18..20], dos_date, .little);
            std.mem.writeInt(u16, d[20..22], @truncate(cluster >> 16), .little);
            std.mem.writeInt(u16, d[24..26], dos_date, .little);
            std.mem.writeInt(u16, d[26..28], @truncate(cluster), .little);
            std.mem.writeInt(u32, d[28..32], size, .little);
            try fs.writeSector(lba, &b);
        }

        /// `k` free slots in one sector of `dir` (deleted, or past the end
        /// marker): their sector and first slot; the folder grown by a
        /// zeroed cluster if it has none.
        fn freeSlots(fs: *Self, dir: Dir, k: u32) !struct { u32, u32 } {
            var c = dir.cluster;
            var steps: u32 = 0;
            while (true) {
                try fs.checkCluster(c);
                for (0..fs.sec_per_clus) |si| {
                    const lba = fs.clusterLba(c) + @as(u32, @intCast(si));
                    const b = try fs.sector(lba);
                    var run: u32 = 0;
                    for (0..16) |e| {
                        const free = b[e * 32] == 0x00 or b[e * 32] == 0xE5;
                        run = if (free) run + 1 else 0;
                        if (run == k) return .{ lba, @as(u32, @intCast(e)) + 1 - k };
                    }
                }
                steps += 1;
                if (steps > fs.cluster_count) return error.ChainLoop;
                c = try fs.nextCluster(c) orelse {
                    const nc = try fs.allocChain(1);
                    try fs.setFat(c, nc);
                    try fs.flushFat();
                    return .{ fs.clusterLba(nc), 0 };
                };
            }
        }

        /// Every end-marker slot (0x00) of `dir` before slot `ent` of
        /// sector `lba` marked deleted (0xE5): a folder ends at its first
        /// 0x00, which mustn't come before the new entry.
        fn unmarkEnd(fs: *Self, dir: Dir, lba: u32, ent: u32) !void {
            var c = dir.cluster;
            var steps: u32 = 0;
            while (true) {
                for (0..fs.sec_per_clus) |si| {
                    const l = fs.clusterLba(c) + @as(u32, @intCast(si));
                    var b = (try fs.sector(l)).*;
                    var changed = false;
                    const upto: u32 = if (l == lba) ent else 16;
                    for (0..upto) |e| if (b[e * 32] == 0x00) {
                        b[e * 32] = 0xE5;
                        changed = true;
                    };
                    if (changed) try fs.writeSector(l, &b);
                    if (l == lba) return;
                }
                steps += 1;
                if (steps > fs.cluster_count) return error.ChainLoop;
                c = try fs.nextCluster(c) orelse return error.NotWritten;
            }
        }

        /// An 8.3 alias no entry of `dir` has: the name's first characters
        /// upper-cased, "~n", its extension's first three.
        fn alias(fs: *Self, dir: Dir, name: []const u8, out: *[11]u8) !void {
            const dot = std.mem.lastIndexOfScalar(u8, name, '.');
            const base_src = if (dot) |d| (if (d == 0) name else name[0..d]) else name;
            const ext_src = if (dot) |d| (if (d == 0) "" else name[d + 1 ..]) else "";
            var base: [8]u8 = undefined;
            var bn: usize = 0;
            for (base_src) |ch| {
                if (bn == base.len) break;
                if (ch == ' ' or ch == '.') continue;
                base[bn] = shortChar(ch);
                bn += 1;
            }
            if (bn == 0) {
                base[0] = '_';
                bn = 1;
            }
            var ext = [_]u8{' '} ** 3;
            var en: usize = 0;
            for (ext_src) |ch| {
                if (en == 3) break;
                if (ch == ' ' or ch == '.') continue;
                ext[en] = shortChar(ch);
                en += 1;
            }
            var n: u32 = 1;
            while (n < 1_000_000) : (n += 1) {
                var tail_buf: [8]u8 = undefined;
                const tail = std.fmt.bufPrint(&tail_buf, "~{d}", .{n}) catch unreachable;
                const keep = @min(bn, 8 - tail.len);
                @memset(out, ' ');
                @memcpy(out[0..keep], base[0..keep]);
                @memcpy(out[keep..][0..tail.len], tail);
                @memcpy(out[8..11], &ext);
                if (!try fs.hasShort(dir, out)) return;
            }
            return error.BadName;
        }

        fn hasShort(fs: *Self, dir: Dir, short: *const [11]u8) !bool {
            var d = [_]u8{0} ** 32;
            @memcpy(d[0..11], short);
            var buf: [12]u8 = undefined;
            const want = buf[0..formatShort(&d, &buf)];
            var it = fs.iterate(dir);
            while (try it.next()) |e| if (std.ascii.eqlIgnoreCase(e.shortName(), want)) return true;
            return false;
        }
    };
}

/// 2026-01-01, the date every entry the board writes gets (it has no clock).
const dos_date: u16 = (2026 - 1980) << 9 | 1 << 5 | 1;

fn dotEntry(d: []u8, name: *const [11]u8, cluster: u32) void {
    @memset(d, 0);
    @memcpy(d[0..11], name);
    d[11] = 0x10;
    std.mem.writeInt(u16, d[16..18], dos_date, .little);
    std.mem.writeInt(u16, d[20..22], @truncate(cluster >> 16), .little);
    std.mem.writeInt(u16, d[24..26], dos_date, .little);
    std.mem.writeInt(u16, d[26..28], @truncate(cluster), .little);
}

/// A long-name entry (fatgen103, section 7): part `seq` (1 = the first 13
/// characters), the last part flagged 0x40; after the name 0x0000, then
/// 0xFFFF.
fn lfnEntry(d: *[32]u8, name: []const u8, seq: u8, last: bool, sum: u8) void {
    @memset(d, 0);
    d[0] = seq | if (last) @as(u8, 0x40) else 0;
    d[11] = 0x0F;
    d[13] = sum;
    const offs = [13]u8{ 1, 3, 5, 7, 9, 14, 16, 18, 20, 22, 24, 28, 30 };
    for (offs, 0..) |o, j| {
        const idx = (@as(usize, seq) - 1) * 13 + j;
        const ch: u16 = if (idx < name.len) name[idx] else if (idx == name.len) 0x0000 else 0xFFFF;
        std.mem.writeInt(u16, d[o..][0..2], ch, .little);
    }
}

/// A character as an 8.3 name may hold it: upper case, letters, digits and
/// $%'-_@~`!(){}^#&; anything else '_'.
fn shortChar(ch: u8) u8 {
    if (std.ascii.isAlphanumeric(ch)) return std.ascii.toUpper(ch);
    return if (std.mem.indexOfScalar(u8, "$%'-_@~`!(){}^#&", ch) != null) ch else '_';
}

/// A FAT boot sector: a jump instruction, then a plausible BPB.
fn isBootSector(b: *const [512]u8) bool {
    if (b[0] != 0xEB and b[0] != 0xE9) return false;
    const bps = le16(b[11..13]);
    const spc = b[13];
    return bps >= 512 and bps <= 4096 and std.math.isPowerOfTwo(bps) and
        spc != 0 and std.math.isPowerOfTwo(spc) and le16(b[14..16]) != 0 and b[16] != 0;
}

/// The long-name checksum of an 8.3 name (fatgen103, section 7).
fn shortSum(n: *const [11]u8) u8 {
    var s: u8 = 0;
    for (n) |c| s = (s >> 1 | s << 7) +% c;
    return s;
}

/// "NAME.EXT" from an 8.3 directory entry, lower-cased where the NT case
/// bits say so (byte 12: bit 3 name, bit 4 extension). Returns the length.
fn formatShort(d: *const [32]u8, out: *[12]u8) u8 {
    var n: u8 = 0;
    for (d[0..8], 0..) |c0, i| {
        if (c0 == ' ') break;
        const c = if (i == 0 and c0 == 0x05) 0xE5 else c0;
        out[n] = if (d[12] & 0x08 != 0) std.ascii.toLower(c) else c;
        n += 1;
    }
    if (d[8] != ' ') {
        out[n] = '.';
        n += 1;
        for (d[8..11]) |c| {
            if (c == ' ') break;
            out[n] = if (d[12] & 0x10 != 0) std.ascii.toLower(c) else c;
            n += 1;
        }
    }
    return n;
}

fn le16(b: *const [2]u8) u16 {
    return std.mem.readInt(u16, b, .little);
}

fn le32(b: *const [4]u8) u32 {
    return std.mem.readInt(u32, b, .little);
}

// --- Tests: images from tools/mkfat-test.sh in build/ps-test/ ---------------

const testing = std.testing;

/// A disk image in memory.
const MemDev = struct {
    data: []const u8,

    pub fn readBlock(d: *MemDev, lba: u32, buf: *[512]u8) !void {
        const off = @as(u64, lba) * 512;
        if (off + 512 > d.data.len) return error.OutOfRange;
        @memcpy(buf, d.data[@intCast(off)..][0..512]);
    }
};

const TestFs = Fs(MemDev);

fn loadImage(name: []const u8) ![]u8 {
    var path_buf: [64]u8 = undefined;
    const path = try std.fmt.bufPrint(&path_buf, "build/ps-test/{s}", .{name});
    return std.Io.Dir.cwd().readFileAlloc(testing.io, path, testing.allocator, .limited(64 << 20)) catch |e| {
        std.debug.print("fat.zig tests: can't read {s} ({s}); run tools/mkfat-test.sh first\n", .{ path, @errorName(e) });
        return e;
    };
}

fn expectPattern(data: []const u8, len: usize, m: u8, a: u8) !void {
    try testing.expectEqual(len, data.len);
    for (data, 0..) |b, i| {
        const want: u8 = @truncate(i * m + a);
        if (b != want) {
            std.debug.print("byte {d}: got {x}, want {x}\n", .{ i, b, want });
            return error.TestUnexpectedResult;
        }
    }
}

fn readNamed(fs: *TestFs, dir: Dir, name: []const u8, buf: []u8) ![]u8 {
    const e = (try fs.find(dir, name)) orelse {
        std.debug.print("not found: {s}\n", .{name});
        return error.TestUnexpectedResult;
    };
    try testing.expect(!e.is_dir);
    return buf[0..try fs.readFile(e, buf)];
}

/// Sorted names of a directory, joined with '|'.
fn listing(fs: *TestFs, dir: Dir, out: []u8) ![]const u8 {
    var names: [16][max_name]u8 = undefined;
    var lens: [16]usize = undefined;
    var n: usize = 0;
    var it = fs.iterate(dir);
    while (try it.next()) |e| {
        @memcpy(names[n][0..e.name().len], e.name());
        lens[n] = e.name().len;
        n += 1;
    }
    var idx: [16]usize = undefined;
    for (0..n) |i| idx[i] = i;
    const Ctx = struct {
        names: *[16][max_name]u8,
        lens: *[16]usize,
        fn lt(c: @This(), x: usize, y: usize) bool {
            return std.mem.lessThan(u8, c.names[x][0..c.lens[x]], c.names[y][0..c.lens[y]]);
        }
    };
    std.mem.sort(usize, idx[0..n], Ctx{ .names = &names, .lens = &lens }, Ctx.lt);
    var w: usize = 0;
    for (idx[0..n], 0..) |k, i| {
        if (i != 0) {
            out[w] = '|';
            w += 1;
        }
        @memcpy(out[w..][0..lens[k]], names[k][0..lens[k]]);
        w += lens[k];
    }
    return out[0..w];
}

/// The checks every image passes: its CHIP-8 directory, BIG.BIN and the
/// fragmented FRAG.BIN.
fn checkImage(name: []const u8, ft: FatType, chip8_dir: []const u8, big_len: usize) !TestFs {
    const img = try loadImage(name);
    defer testing.allocator.free(img);
    var dev: MemDev = .{ .data = img };
    var fs = try TestFs.mount(&dev);
    try testing.expectEqual(ft, fs.fat_type);

    // the directory itself, found ignoring case
    const d = (try fs.find(fs.root(), "chip8")).?;
    try testing.expect(d.is_dir);
    try testing.expectEqualStrings(chip8_dir, d.name());
    const dir = fs.openDir(d);
    var lb: [512]u8 = undefined;
    const want_list = if (ft == .fat32)
        "PONG.CH8|Space Invaders [David Winter].ch8|brix.ch8|games|many"
    else
        "PONG.CH8|Space Invaders [David Winter].ch8|brix.ch8|games";
    try testing.expectEqualStrings(want_list, try listing(&fs, dir, &lb));

    var buf: [200_000]u8 = undefined;
    try expectPattern(try readNamed(&fs, dir, "SPACE INVADERS [david winter].CH8", &buf), 1000, 7, 3);
    try expectPattern(try readNamed(&fs, dir, "spacei~1.ch8", &buf), 1000, 7, 3); // its 8.3 name
    try expectPattern(try readNamed(&fs, dir, "pong.ch8", &buf), 246, 13, 5);
    try expectPattern(try readNamed(&fs, dir, "BRIX.CH8", &buf), 280, 3, 1);
    const games = fs.openDir((try fs.find(dir, "Games")).?);
    try expectPattern(try readNamed(&fs, games, "Tetris [Fran Dachille, 1991].ch8", &buf), 494, 7, 3);
    try expectPattern(try readNamed(&fs, fs.root(), "big.bin", &buf), big_len, 7, 3);
    try expectPattern(try readNamed(&fs, fs.root(), "FRAG.BIN", &buf), 5000, 13, 5);
    // a short buffer gets the start of the file
    try expectPattern(try readNamed(&fs, fs.root(), "big.bin", buf[0..700]), 700, 7, 3);
    try testing.expectEqual(@as(?Entry, null), try fs.find(dir, "missing.ch8"));

    // firstBlock: the raw block there is the start of the file
    for ([_][]const u8{ "big.bin", "FRAG.BIN" }) |nm| {
        const e = (try fs.find(fs.root(), nm)).?;
        var blk: [512]u8 = undefined;
        try dev.readBlock(try fs.firstBlock(e), &blk);
        const head = try readNamed(&fs, fs.root(), nm, buf[0..512]);
        try testing.expectEqualSlices(u8, head, &blk);
    }
    try testing.expectError(error.NoData, fs.firstBlock(d));

    // runs: the file's blocks, as readFile reads them; a fragmented file
    // gives more than one run, and runs stop at max_blocks
    for ([_][]const u8{ "big.bin", "FRAG.BIN", "BIG.BIN" }) |nm| {
        const e = (try fs.find(fs.root(), nm)).?;
        for ([_]u32{ 65535, 3 }) |max| {
            var got: [200_000]u8 = undefined;
            var n: usize = 0;
            var count: usize = 0;
            var rs = fs.runs(e, max);
            while (try rs.next()) |run| {
                count += 1;
                try testing.expect(run.blocks <= @max(max, fs.sec_per_clus));
                try testing.expectEqual((run.bytes + 511) / 512, run.blocks);
                var blk: [512]u8 = undefined;
                for (0..run.blocks) |k| {
                    try dev.readBlock(run.lba + @as(u32, @intCast(k)), &blk);
                    const take = @min(512, run.bytes - k * 512);
                    @memcpy(got[n..][0..take], blk[0..take]);
                    n += take;
                }
            }
            try testing.expectEqual(@as(usize, e.size), n);
            const want = try readNamed(&fs, fs.root(), nm, &buf);
            try testing.expectEqualSlices(u8, want, got[0..n]);
            if (std.mem.eql(u8, nm, "FRAG.BIN")) try testing.expect(count >= 2);
            if (max == 3 and fs.sec_per_clus == 1) try testing.expect(count >= (e.size + 1535) / 1536);
        }
    }

    // FRAG.BIN really is fragmented
    const frag = (try fs.find(fs.root(), "FRAG.BIN")).?;
    var c = frag.first_cluster;
    var gaps: usize = 0;
    while (try fs.nextCluster(c)) |n| : (c = n) {
        if (n != c + 1) gaps += 1;
    }
    try testing.expect(gaps > 0);
    return fs;
}

test "FAT12 superfloppy" {
    _ = try checkImage("fat12.img", .fat12, "CHIP8", 20000);
}

test "FAT16 in an MBR partition" {
    _ = try checkImage("fat16.img", .fat16, "chip8", 60000);
}

test "FAT32 in an MBR partition, a directory over many clusters" {
    const img = try loadImage("fat32.img");
    defer testing.allocator.free(img);
    _ = try checkImage("fat32.img", .fat32, "Chip8", 200000);

    var dev: MemDev = .{ .data = img };
    var fs = try TestFs.mount(&dev);
    const many = fs.openDir((try fs.find(fs.openDir((try fs.find(fs.root(), "CHIP8")).?), "many")).?);
    var seen = [_]bool{false} ** 100;
    var it = fs.iterate(many);
    var n: usize = 0;
    var buf: [16]u8 = undefined;
    while (try it.next()) |e| : (n += 1) {
        const nm = e.name();
        try testing.expect(std.mem.startsWith(u8, nm, "game number ") and std.mem.endsWith(u8, nm, ".ch8"));
        const k = try std.fmt.parseInt(usize, nm[12..15], 10);
        try testing.expect(!seen[k]);
        seen[k] = true;
        var want: [16]u8 = undefined;
        const w = try std.fmt.bufPrint(&want, "game {d:0>3}\n", .{k});
        try testing.expectEqualStrings(w, buf[0..try fs.readFile(e, &buf)]);
    }
    try testing.expectEqual(@as(usize, 100), n);
    // the directory spans more than one cluster
    try testing.expect(try fs.nextCluster(many.cluster) != null);
}

test "garbage and truncated images fail cleanly" {
    for ([_][]const u8{ "garbage.img", "truncated.img" }) |name| {
        const img = try loadImage(name);
        defer testing.allocator.free(img);
        var dev: MemDev = .{ .data = img };
        try testing.expect(std.meta.isError(TestFs.mount(&dev)));
    }
    var zeros = [_]u8{0} ** 2048;
    var dev: MemDev = .{ .data = &zeros };
    try testing.expectError(error.NoFilesystem, TestFs.mount(&dev));
}

test "damaged chains: a loop and a bad cluster" {
    const img = try loadImage("fat16.img");
    defer testing.allocator.free(img);
    var dev: MemDev = .{ .data = img };
    var fs = try TestFs.mount(&dev);
    const e = (try fs.find(fs.root(), "BIG.BIN")).?;
    const fat_off = @as(usize, fs.fat_lba) * 512;

    // point the file's second cluster back at its first: a loop
    const c1 = e.first_cluster;
    const c2 = (try fs.nextCluster(c1)).?;
    std.mem.writeInt(u16, img[fat_off + c2 * 2 ..][0..2], @intCast(c1), .little);
    fs.fat_buf_lba = null;
    var buf: [60000]u8 = undefined;
    // the read stops at the size; the loop shows when the chain is walked
    // as a directory
    var it = fs.iterate(.{ .cluster = c1 });
    while (it.next()) |_| {} else |err| try testing.expectEqual(error.ChainLoop, err);

    // mark it bad
    std.mem.writeInt(u16, img[fat_off + c2 * 2 ..][0..2], 0xFFF7, .little);
    fs.fat_buf_lba = null;
    try testing.expectError(error.BadCluster, fs.readFile(e, &buf));
    try testing.expectError(error.BadCluster, drainRuns(&fs, e));
    // end the chain early
    std.mem.writeInt(u16, img[fat_off + c2 * 2 ..][0..2], 0xFFFF, .little);
    fs.fat_buf_lba = null;
    try testing.expectError(error.ShortChain, fs.readFile(e, &buf));
    try testing.expectError(error.ShortChain, drainRuns(&fs, e));
    // a loop that the file's size walks into: runs stop, no endless loop
    std.mem.writeInt(u16, img[fat_off + c2 * 2 ..][0..2], @intCast(c1), .little);
    fs.fat_buf_lba = null;
    var big = e;
    big.size = 0xFFFF_0000;
    try testing.expectError(error.ChainLoop, drainRuns(&fs, big));
}

fn drainRuns(fs: *TestFs, e: Entry) !void {
    var rs = fs.runs(e, 65535);
    while (try rs.next()) |_| {}
}

test "long name checksum and 8.3 formatting" {
    // fatgen103's algorithm, by hand for "PONG    CH8"
    var s: u8 = 0;
    for ("PONG    CH8") |c| s = @as(u8, @truncate((@as(u16, s & 1) << 7) + (s >> 1))) +% c;
    try testing.expectEqual(s, shortSum("PONG    CH8"));
    var d = [_]u8{' '} ** 32;
    @memcpy(d[0..11], "BRIX    CH8");
    d[12] = 0x18;
    var out: [12]u8 = undefined;
    try testing.expectEqualStrings("brix.ch8", out[0..formatShort(&d, &out)]);
    @memcpy(d[0..11], "README     ");
    d[12] = 0;
    try testing.expectEqualStrings("README", out[0..formatShort(&d, &out)]);
}

// --- Tests: writing (side track K, F1a) --------------------------------------

/// An image under a layer of written blocks (the image itself unchanged);
/// writes fail after `limit` of them (a write cut off: the power).
const OverlayDev = struct {
    base: []const u8,
    written: std.AutoHashMap(u32, [512]u8),
    writes: u32 = 0,
    limit: u32 = std.math.maxInt(u32),

    fn init(base: []const u8) OverlayDev {
        return .{ .base = base, .written = .init(testing.allocator) };
    }
    fn deinit(d: *OverlayDev) void {
        d.written.deinit();
    }
    pub fn readBlock(d: *OverlayDev, lba: u32, buf: *[512]u8) !void {
        if (d.written.get(lba)) |b| {
            buf.* = b;
            return;
        }
        const off = @as(u64, lba) * 512;
        if (off + 512 > d.base.len) return error.OutOfRange;
        @memcpy(buf, d.base[@intCast(off)..][0..512]);
    }
    pub fn writeBlock(d: *OverlayDev, lba: u32, buf: *const [512]u8) !void {
        if (d.writes == d.limit) return error.CardFailed;
        d.writes += 1;
        try d.written.put(lba, buf.*);
    }
    /// The image with the writes in it.
    fn image(d: *OverlayDev, out: []u8) void {
        @memcpy(out, d.base);
        var it = d.written.iterator();
        while (it.next()) |kv| @memcpy(out[@as(usize, kv.key_ptr.*) * 512 ..][0..512], kv.value_ptr);
    }
};

const WFs = Fs(OverlayDev);

/// A file's bytes, through its chain (zeros where nothing was written).
fn fileBytes(fs: *WFs, e: Entry, out: []u8) ![]u8 {
    return out[0..try fs.readFile(e, out)];
}

test "writing FAT32: folders and files made, read back after a fresh mount; the rest untouched" {
    const img = try loadImage("fat32.img");
    defer testing.allocator.free(img);
    var dev = OverlayDev.init(img);
    defer dev.deinit();
    {
        var fs = try WFs.mount(&dev);
        const saves = try fs.mkdir(fs.root(), "saves");
        const gb = try fs.mkdir(saves, "gb");
        const e = try fs.create(gb, "Puzzle (Rev 1) [a].sav", 8192);
        try testing.expectEqual(@as(u32, 8192), e.size);
        // its bytes written through its runs, as the board writes a save
        var rs = fs.runs(e, 65535);
        var k: u32 = 0;
        while (try rs.next()) |run| for (0..run.blocks) |i| {
            var b: [512]u8 = undefined;
            for (&b, 0..) |*x, j| x.* = @truncate((k * 512 + j) * 7 + 3);
            try dev.writeBlock(run.lba + @as(u32, @intCast(i)), &b);
            k += 1;
        };
        _ = try fs.create(fs.root(), "empty.txt", 0);
        try testing.expectError(error.Exists, fs.create(gb, "PUZZLE (REV 1) [A].SAV", 10));
        try testing.expectError(error.BadName, fs.create(gb, "a/b", 10));
        // a folder grown past its first cluster (one sector: 16 slots, two
        // a file here)
        const many = try fs.mkdir(fs.root(), "Many Movies");
        var nb: [32]u8 = undefined;
        for (0..30) |i| _ = try fs.create(many, try std.fmt.bufPrint(&nb, "movie number {d}.mv1", .{i}), 600 + @as(u32, @intCast(i)));
    }
    // a fresh mount: everything there
    var fs = try WFs.mount(&dev);
    const gb = fs.openDir((try fs.find(fs.openDir((try fs.find(fs.root(), "SAVES")).?), "gb")).?);
    const e = (try fs.find(gb, "puzzle (rev 1) [a].sav")).?;
    var buf: [200_000]u8 = undefined;
    try expectPattern(try fileBytes(&fs, e, &buf), 8192, 7, 3);
    try testing.expectEqualStrings("PUZZLE~1.SAV", e.shortName());
    try testing.expectEqual(@as(u32, 0), (try fs.find(fs.root(), "empty.txt")).?.size);
    const many = fs.openDir((try fs.find(fs.root(), "many movies")).?);
    try testing.expect(try fs.nextCluster(many.cluster) != null);
    var n: usize = 0;
    var it = fs.iterate(many);
    while (try it.next()) |m| : (n += 1) {
        const i = try std.fmt.parseInt(u32, m.name()[13 .. m.name().len - 4], 10);
        try testing.expectEqual(600 + i, m.size);
        try testing.expect(std.mem.allEqual(u8, try fileBytes(&fs, m, &buf), 0));
    }
    try testing.expectEqual(@as(usize, 30), n);
    // what was there is as it was
    try expectPattern(try readNamedW(&fs, fs.root(), "big.bin", &buf), 200000, 7, 3);
    try expectPattern(try readNamedW(&fs, fs.root(), "FRAG.BIN", &buf), 5000, 13, 5);
    // rewritten (the board's keymap.conf): an empty file given bytes, a
    // file grown past its clusters, one made smaller; read back after a
    // fresh mount
    {
        var text: [9000]u8 = undefined;
        for (&text, 0..) |*x, j| x.* = @truncate(j * 11 + 1);
        const per = fs.sec_per_clus * 512;
        var r = try fs.rewrite((try fs.find(fs.root(), "empty.txt")).?, text[0..700]);
        try testing.expectEqual(@as(u32, 700), r.size);
        r = try fs.rewrite((try fs.find(many, "movie number 3.mv1")).?, text[0 .. per * 2 + 5]);
        try testing.expectEqual(per * 2 + 5, r.size);
        r = try fs.rewrite((try fs.find(many, "movie number 4.mv1")).?, text[0..10]);
        r = try fs.rewrite((try fs.find(many, "movie number 5.mv1")).?, text[0..0]);
        try testing.expectEqual(@as(u32, 0), r.first_cluster);
        try testing.expectError(error.NotAFile, fs.rewrite((try fs.find(fs.root(), "saves")).?, text[0..1]));
        var fs2 = try WFs.mount(&dev);
        const many2 = fs2.openDir((try fs2.find(fs2.root(), "many movies")).?);
        try testing.expectEqualSlices(u8, text[0..700], try readNamedW(&fs2, fs2.root(), "empty.txt", &buf));
        try testing.expectEqualSlices(u8, text[0 .. per * 2 + 5], try readNamedW(&fs2, many2, "movie number 3.mv1", &buf));
        try testing.expectEqualSlices(u8, text[0..10], try readNamedW(&fs2, many2, "movie number 4.mv1", &buf));
        try expectPattern(try readNamedW(&fs2, fs2.root(), "big.bin", &buf), 200000, 7, 3);
        try expectPattern(try readNamedW(&fs2, fs2.root(), "FRAG.BIN", &buf), 5000, 13, 5);
    }
    // the image for fsck.fat (make test-zig)
    const out = try testing.allocator.alloc(u8, img.len);
    defer testing.allocator.free(out);
    dev.image(out);
    try std.Io.Dir.cwd().writeFile(testing.io, .{ .sub_path = "build/ps-test/fat32-written.img", .data = out });
    // FAT12/16 stay read-only
    const img16 = try loadImage("fat16.img");
    defer testing.allocator.free(img16);
    var dev16 = OverlayDev.init(img16);
    defer dev16.deinit();
    var fs16 = try WFs.mount(&dev16);
    try testing.expectError(error.ReadOnly, fs16.create(fs16.root(), "x.bin", 10));
}

fn readNamedW(fs: *WFs, dir: Dir, name: []const u8, buf: []u8) ![]u8 {
    const e = (try fs.find(dir, name)) orelse return error.TestUnexpectedResult;
    return buf[0..try fs.readFile(e, buf)];
}

test "writing FAT32: cut off after any write, a file is there whole or not at all, the rest untouched" {
    const img = try loadImage("fat32.img");
    defer testing.allocator.free(img);
    // how many writes a whole run takes: a folder, a file in it (big
    // enough for several FAT sectors), one in a full folder (it grows)
    const Run = struct {
        fn go(dev: *OverlayDev) !void {
            var fs = try WFs.mount(dev);
            const d = try fs.mkdir(fs.root(), "states");
            _ = try fs.create(d, "a game with a long name.st1", 300 * 512);
            const many = fs.openDir((try fs.find(fs.openDir((try fs.find(fs.root(), "CHIP8")).?), "many")).?);
            _ = try fs.create(many, "one more game.ch8", 100);
        }
    };
    var full = OverlayDev.init(img);
    defer full.deinit();
    try Run.go(&full);
    const total = full.writes;
    try testing.expect(total > 10);
    var buf: [200_000]u8 = undefined;
    var cut: u32 = 0;
    while (cut <= total) : (cut += 1) {
        var dev = OverlayDev.init(img);
        defer dev.deinit();
        dev.limit = cut;
        if (Run.go(&dev)) |_| {
            try testing.expectEqual(total, cut);
        } else |err| try testing.expectEqual(error.CardFailed, err);
        dev.limit = std.math.maxInt(u32);
        var fs = try WFs.mount(&dev);
        // the new ones: absent, or there with their whole (zeroed) chain
        if (try fs.find(fs.root(), "states")) |de| {
            try testing.expect(de.is_dir);
            if (try fs.find(fs.openDir(de), "a game with a long name.st1")) |f| {
                try testing.expectEqual(@as(u32, 300 * 512), f.size);
                try testing.expect(std.mem.allEqual(u8, try fileBytes(&fs, f, &buf), 0));
            }
        }
        const many = fs.openDir((try fs.find(fs.openDir((try fs.find(fs.root(), "CHIP8")).?), "many")).?);
        var count: usize = 0;
        var it = fs.iterate(many);
        while (try it.next()) |_| count += 1;
        try testing.expect(count == 100 or count == 101);
        if (try fs.find(many, "one more game.ch8")) |f| try testing.expectEqual(@as(u32, 100), f.size);
        // the old files read as before
        try expectPattern(try readNamedW(&fs, fs.root(), "big.bin", &buf), 200000, 7, 3);
        try expectPattern(try readNamedW(&fs, fs.root(), "FRAG.BIN", &buf), 5000, 13, 5);
    }
}

test "rewriting FAT32: cut off after any write, the file has its old size or its new one, the rest untouched" {
    const img = try loadImage("fat32.img");
    defer testing.allocator.free(img);
    var text: [3000]u8 = undefined;
    for (&text, 0..) |*x, j| x.* = @truncate(j * 5 + 2);
    const Run = struct {
        fn go(dev: *OverlayDev, t: []const u8) !void {
            var fs = try WFs.mount(dev);
            _ = try fs.rewrite((try fs.find(fs.root(), "FRAG.BIN")).?, t);
        }
    };
    var full = OverlayDev.init(img);
    defer full.deinit();
    try Run.go(&full, &text);
    const total = full.writes;
    var buf: [200_000]u8 = undefined;
    var cut: u32 = 0;
    while (cut <= total) : (cut += 1) {
        var dev = OverlayDev.init(img);
        defer dev.deinit();
        dev.limit = cut;
        if (Run.go(&dev, &text)) |_| try testing.expectEqual(total, cut) else |err| try testing.expectEqual(error.CardFailed, err);
        dev.limit = std.math.maxInt(u32);
        var fs = try WFs.mount(&dev);
        // the old size (over some new bytes), or the new size and bytes
        // (cut off while the tail's clusters were freed: still the new)
        const f = (try fs.find(fs.root(), "FRAG.BIN")).?;
        if (f.size == text.len) {
            try testing.expectEqualSlices(u8, &text, try fileBytes(&fs, f, &buf));
        } else try testing.expectEqual(@as(u32, 5000), f.size);
        if (cut == total) try testing.expectEqual(@as(u32, text.len), f.size);
        try expectPattern(try readNamedW(&fs, fs.root(), "big.bin", &buf), 200000, 7, 3);
    }
}

test "writing: the 8.3 alias and the long-name entries" {
    var d: [32]u8 = undefined;
    lfnEntry(&d, "Puzzle (Rev 1).sav", 2, true, 0x5A);
    try testing.expectEqual(@as(u8, 0x42), d[0]);
    try testing.expectEqual(@as(u8, 0x0F), d[11]);
    try testing.expectEqual(@as(u8, 0x5A), d[13]);
    try testing.expectEqual(@as(u16, ')'), le16(d[1..3])); // character 13, the second part's first
    // the name ends in this part: 0x0000 after it, then 0xFFFF
    try testing.expectEqual(@as(u16, 'v'), le16(d[9..11]));
    try testing.expectEqual(@as(u16, 0x0000), le16(d[14..16]));
    try testing.expectEqual(@as(u16, 0xFFFF), le16(d[16..18]));
    try testing.expectEqual(@as(u8, '_'), shortChar('+'));
    try testing.expectEqual(@as(u8, 'A'), shortChar('a'));
    try testing.expectEqual(@as(u8, '~'), shortChar('~'));
}
