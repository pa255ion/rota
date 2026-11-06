//! Battery saves (side track I, phase 1; docs/systems/gameboy.md, "Side
//! track I"): a ROM's save file on the SD card into the system's
//! cartridge RAM before the game starts, and the RAM back into the same
//! file while the game runs. The system table says how big a save is and
//! where the RAM sits in the link map (menu.Save); relay.zig decides when.
//!
//! The file is `/saves/<system folder>/<path below it>/<name>.sav`
//! (`savePath`), made at full size by `make sd`, so the PS only rewrites
//! its blocks in place, as `last.txt`: the FAT and the directories are
//! never written, and a block whose bytes didn't change isn't written
//! either. A file of the wrong size is not used.
//!
//! Reading the RAM back: the save control's bit 0 puts cartridge RAM
//! reads on PS_IN's byte channel (Gb.Loader); each data byte the PS
//! writes into the read window reads the next RAM byte, which arrives
//! with a flip of the channel's toggle. The game keeps running (the RAM's
//! second port), and the dirty flag (PS_IN bit 6) is cleared first, so a
//! write the game makes meanwhile marks the RAM dirty again.
//!
//! Generic over the file system (fat.Fs), the card (readBlock and
//! writeBlock) and the link (pl.Link), so the tests below run it on a card
//! image made by `make sd`'s tools with a model of the Game Boy's side.

const std = @import("std");
const menu = @import("menu.zig");
const fat = @import("fat.zig");

pub const max_path = 128;

/// The save file's path for the ROM `rom_name` listed in the menu's
/// folder `dir_path` ("/gb/games", the card's own spelling): "/saves" in
/// front, ".sav" for the ending. Null if it doesn't fit.
pub fn savePath(buf: []u8, dir_path_in: []const u8, rom_name: []const u8) ?[]const u8 {
    // the Game Boy's folders copied into /gbc/gb (tools/mksd.sh): the Game
    // Boy's save, so one game has one save on both systems (the user,
    // 2026-10-08)
    const dir_path = sharedGbPath(dir_path_in);
    const dot = std.mem.lastIndexOfScalar(u8, rom_name, '.') orelse rom_name.len;
    const sep: []const u8 = if (dir_path.len > 0 and dir_path[dir_path.len - 1] == '/') "" else "/";
    const lead: []const u8 = if (dir_path.len > 0 and dir_path[0] == '/') "" else "/";
    return std.fmt.bufPrint(buf, "/{s}{s}{s}{s}{s}.sav", .{ menu.saves_dir, lead, dir_path, sep, rom_name[0..dot] }) catch null;
}

/// "/gbc/gb/x" -> "/gb/x" (any case); anything else as it is.
fn sharedGbPath(p: []const u8) []const u8 {
    const pre = "/gbc/gb";
    if (p.len >= pre.len and std.ascii.eqlIgnoreCase(p[0..pre.len], pre) and (p.len == pre.len or p[pre.len] == '/'))
        return p[4..]; // "/gb..."
    return p;
}

pub const Found = union(enum) {
    /// the save file, of the right size
    file: fat.Entry,
    /// none there (the card wasn't made by `make sd` with this ROM)
    missing,
    /// there, but not `size` bytes: not used
    wrong_size: u32,
};

/// The file at `path` (folders from the root; any case), checked for size.
pub fn lookup(fs: anytype, path: []const u8, size: u32) !Found {
    const e = (try find(fs, path)) orelse return .missing;
    return if (e.size == size) .{ .file = e } else .{ .wrong_size = e.size };
}

/// The file at `path`, made (zeros, `size` bytes) with any folders it
/// needs if it isn't there (side track K, F1: the board makes its saves,
/// states and movies when it first needs them; fat.zig's create and
/// mkdir). One that is there is returned as it is: the caller checks its
/// size.
pub fn makeFile(fs: anytype, path: []const u8, size: u32) !fat.Entry {
    var dir = fs.root();
    var it = std.mem.tokenizeScalar(u8, path, '/');
    while (it.next()) |part| {
        const e = try fs.find(dir, part);
        if (it.peek() == null) {
            if (e) |f| return if (f.is_dir) error.NotAFile else f;
            return fs.create(dir, part, size);
        }
        dir = if (e) |d| (if (d.is_dir) fs.openDir(d) else return error.NotAFile) else try fs.mkdir(dir, part);
    }
    return error.BadName;
}

/// The file at `path` (folders from the root; any case), whatever its
/// size (a movie slot's, side track O); null if there is none.
pub fn find(fs: anytype, path: []const u8) !?fat.Entry {
    var dir = fs.root();
    var it = std.mem.tokenizeScalar(u8, path, '/');
    while (it.next()) |part| {
        const e = (try fs.find(dir, part)) orelse return null;
        if (it.peek() == null) return if (e.is_dir) null else e;
        if (!e.is_dir) return null;
        dir = fs.openDir(e);
    }
    return null;
}

/// The RAM part of a save file of `size` bytes: all of it, or all but the
/// clock's footer.
pub fn ramBytes(size: u32, clock: bool) u32 {
    return if (clock) size -| menu.clock_footer else size;
}

/// The clock's ten start bytes from a footer (each register's low byte).
pub fn clockFromFooter(f: *const [menu.clock_footer]u8) [10]u8 {
    var c: [10]u8 = undefined;
    for (&c, 0..) |*b, i| b.* = f[4 * i];
    return c;
}

/// A footer from the clock's ten bytes (time stamp 0, see menu.Save.clock).
pub fn footerFromClock(c: *const [10]u8) [menu.clock_footer]u8 {
    var f = [_]u8{0} ** menu.clock_footer;
    for (c, 0..) |b, i| f[4 * i] = b;
    return f;
}

/// The save file into cartridge RAM (while the core is held): its RAM
/// part, and with a clock the footer's ten bytes into the clock's start
/// values. Returns the bytes read.
/// `noinline`: inlined into the relay's menu code, its reader result once sat at an
/// odd stack offset read with a word load (tools/check-ps-align.sh, side
/// track I; as protocol.zig's `hex`).
pub noinline fn load(fs: anytype, e: fat.Entry, link: anytype, spec: menu.Save, clock: bool) !u32 {
    var r = fs.reader(e);
    const ram = ramBytes(e.size, clock);
    var foot = [_]u8{0} ** menu.clock_footer;
    link.setAddr(spec.ram_addr);
    var n: u32 = 0;
    while (try r.next()) |chunk| {
        // the RAM's bytes go out, the footer's are kept
        const k: u32 = @intCast(@min(chunk.len, ram -| n));
        link.data(chunk[0..k]);
        for (chunk[k..], 0..) |b, i| {
            const at = n + k + @as(u32, @intCast(i)) - ram;
            if (at < foot.len) foot[at] = b;
        }
        n += @intCast(chunk.len);
    }
    if (clock) link.write(spec.clock_load_addr, &clockFromFooter(&foot));
    return n;
}

/// No save to load: `size` bytes of zeros, what a blank save file holds
/// (`make sd`), so a battery RAM never starts with another game's data
/// (nor its clock with another's time).
pub fn clear(link: anytype, spec: menu.Save, size: u32, clock: bool) void {
    link.fill(spec.ram_addr, 0, ramBytes(size, clock));
    if (clock) link.fill(spec.clock_load_addr, 0, 10);
}

pub const Written = struct {
    /// blocks of the file looked at
    blocks: u32 = 0,
    /// of those, rewritten (the others held the same bytes)
    written: u32 = 0,
};

/// Cartridge RAM back into its save file `e` (while the game runs): the
/// dirty flag cleared, the RAM read a block at a time over the byte
/// channel, each block of the file compared and rewritten if different.
/// `buf` and `old` are two block buffers. The byte channel goes back to
/// the system's own bytes at the end, also after an error.
pub fn writeBack(fs: anytype, card: anytype, e: fat.Entry, link: anytype, spec: menu.Save, clock: bool, buf: *[512]u8, old: *[512]u8) !Written {
    link.write(spec.clear_addr, &.{0});
    link.write(spec.ctrl_addr, &.{1});
    defer link.write(spec.ctrl_addr, &.{0});
    // the cached block of the file system may be one of the file's
    defer fs.buf_lba = null;
    var toggle = link.inputs().toggle;
    // the clock first (its first read takes the snapshot), then the RAM
    var foot = [_]u8{0} ** menu.clock_footer;
    if (clock) {
        var c: [10]u8 = undefined;
        link.setAddr(spec.clock_read_addr);
        for (&c) |*b| {
            link.data(&.{0});
            b.* = try nextByte(link, &toggle);
        }
        foot = footerFromClock(&c);
    }
    const ram = ramBytes(e.size, clock);
    link.setAddr(spec.read_addr);
    var w: Written = .{};
    var left = e.size;
    var off: u32 = 0;
    var runs = fs.runs(e, 0xFFFF);
    while (try runs.next()) |run| {
        var k: u32 = 0;
        while (k < run.blocks and left > 0) : (k += 1) {
            const n = @min(left, 512);
            for (buf[0..n], 0..) |*b, i| {
                const at = off + @as(u32, @intCast(i));
                if (at < ram) {
                    link.data(&.{0});
                    b.* = try nextByte(link, &toggle);
                } else b.* = foot[at - ram];
            }
            off += n;
            try card.readBlock(run.lba + k, old);
            w.blocks += 1;
            if (!std.mem.eql(u8, old[0..n], buf[0..n])) {
                @memcpy(old[0..n], buf[0..n]);
                try card.writeBlock(run.lba + k, old);
                w.written += 1;
            }
            left -= n;
        }
    }
    return w;
}

/// The next byte of the channel: the toggle flips within a few PL clocks
/// of the read's strobe; 1 ms without one is an error (no such PL).
fn nextByte(link: anytype, toggle: *bool) !u8 {
    var i: u32 = 0;
    while (i < 1000) : (i += 1) {
        const in = link.inputs();
        if (in.toggle != toggle.*) {
            toggle.* = in.toggle;
            return in.byte;
        }
        link.port.delayUs(1);
    }
    return error.NoAnswer;
}

// --- Tests: the card image of `make test-zig` (tools/mksd.sh with a
// battery cartridge and a stored save) and a model of Gb.Top's side -----------
const testing = std.testing;
const pl = @import("pl.zig");

test "save paths: /saves, the menu's folder, the ROM's name with .sav" {
    var b: [max_path]u8 = undefined;
    try testing.expectEqualStrings("/saves/gb/games/x.sav", savePath(&b, "/gb/games", "x.gb").?);
    try testing.expectEqualStrings("/saves/GB/Some Game (Rev 1).sav", savePath(&b, "/GB", "Some Game (Rev 1).gb").?);
    try testing.expectEqualStrings("/saves/gb/a.b.sav", savePath(&b, "/gb/", "a.b.gbc").?);
    try testing.expectEqualStrings("/saves/noext.sav", savePath(&b, "/", "noext").?);
    // the GBC's copies of the Game Boy's folders share the Game Boy's saves
    try testing.expectEqualStrings("/saves/gb/games/local/Some Game.sav", savePath(&b, "/gbc/gb/games/local", "Some Game.gb").?);
    try testing.expectEqualStrings("/saves/GB/x.sav", savePath(&b, "/GBC/GB", "x.gb").?); // (the card ignores case)
    // but not the GBC's own folders, nor names that only start alike
    try testing.expectEqualStrings("/saves/gbc/games/ucity.sav", savePath(&b, "/gbc/games", "ucity.gbc").?);
    try testing.expectEqualStrings("/saves/gbc/gba/x.sav", savePath(&b, "/gbc/gba", "x.gb").?);
    var small: [12]u8 = undefined;
    try testing.expect(savePath(&small, "/gb/games", "x.gb") == null);
}

/// The Game Boy's link map as Gb.Top decodes it (Gb.Loader): cartridge
/// RAM written while held, its read window, the save control, the dirty
/// flag; PS_IN's byte channel and bit 6.
const GbModel = struct {
    cram: [0x8000]u8 = [_]u8{0xEE} ** 0x8000,
    strobe: bool = false,
    addr: u24 = 0,
    hold: bool = false,
    read_back: bool = false,
    dirty: bool = false,
    byte: u8 = 0,
    toggle: bool = false,
    /// writes the model refused (outside the map, or RAM while running)
    ignored: u32 = 0,
    /// MBC3's clock: its start values (written while held) and now
    clock_init: [10]u8 = [_]u8{0xEE} ** 10,
    clock_now: [10]u8 = [_]u8{0} ** 10,

    pub fn write(m: *GbModel, v: u32) void {
        m.hold = v & (1 << 26) != 0;
        const st = v & (1 << 24) != 0;
        if (st == m.strobe) return;
        m.strobe = st;
        const d: u8 = @truncate(v >> 16);
        if (v & (1 << 25) != 0) {
            m.addr = m.addr << 8 | d;
            return;
        }
        const a = m.addr;
        m.addr +%= 1;
        if (a >= 0x100000 and a < 0x108000 and m.hold) {
            m.cram[a - 0x100000] = d;
        } else if (a >= 0x120000 and a < 0x128000) {
            if (m.read_back) {
                m.byte = m.cram[a - 0x120000];
                m.toggle = !m.toggle;
            }
        } else if (a >= 0x140010 and a < 0x14001A and m.hold) {
            m.clock_init[a - 0x140010] = d;
        } else if (a >= 0x140020 and a < 0x14002A) {
            if (m.read_back) {
                m.byte = m.clock_now[a - 0x140020];
                m.toggle = !m.toggle;
            }
        } else if (a == 0x140000) {
            m.read_back = d & 1 != 0;
        } else if (a == 0x140001) {
            m.dirty = false;
        } else m.ignored += 1;
    }
    pub fn read(m: *GbModel) u32 {
        return @as(u32, m.byte) << 24 | 0x02 << 16 | @as(u32, @intFromBool(m.dirty)) << 6 | @as(u32, @intFromBool(m.toggle)) << 5;
    }
    pub fn delayUs(_: *GbModel, _: u32) void {}
};

/// A card image in memory that counts its block writes.
const ImgDev = struct {
    data: []u8,
    writes: std.ArrayList(u32) = .empty,

    pub fn readBlock(d: *ImgDev, lba: u32, buf: *[512]u8) !void {
        const off = @as(usize, lba) * 512;
        if (off + 512 > d.data.len) return error.OutOfRange;
        @memcpy(buf, d.data[off..][0..512]);
    }
    pub fn writeBlock(d: *ImgDev, lba: u32, buf: *const [512]u8) !void {
        const off = @as(usize, lba) * 512;
        if (off + 512 > d.data.len) return error.OutOfRange;
        @memcpy(d.data[off..][0..512], buf);
        try d.writes.append(testing.allocator, lba);
    }
};

const gb = menu.systems[1];

/// The stored save tools/mksd.sh put on the image (Makefile test-zig):
/// byte i = (i * 5 + 1) & 0xFF.
fn storedByte(i: usize) u8 {
    return @truncate(i * 5 + 1);
}

test "makeFile: a file made with its folders, then found as it is; fsck-clean (make test-zig)" {
    const gpa = testing.allocator;
    const img = try std.Io.Dir.cwd().readFileAlloc(testing.io, "build/ps-test/sd.img", gpa, .limited(1 << 27));
    defer gpa.free(img);
    var dev: ImgDev = .{ .data = img };
    defer dev.writes.deinit(gpa);
    var fs = try fat.Fs(ImgDev).mount(&dev);
    const e = try makeFile(&fs, "/states/gb/new folder/Some Game (Rev 1).st2", 54000);
    try testing.expectEqual(@as(u32, 54000), e.size);
    const writes = dev.writes.items.len;
    const again = try makeFile(&fs, "/STATES/gb/New Folder/some game (rev 1).st2", 1);
    try testing.expectEqual(e.first_cluster, again.first_cluster);
    try testing.expectEqual(writes, dev.writes.items.len);
    // a fresh mount finds it, zeros
    var fs2 = try fat.Fs(ImgDev).mount(&dev);
    const f = switch (try lookup(&fs2, "/states/gb/new folder/Some Game (Rev 1).st2", 54000)) {
        .file => |x| x,
        else => return error.TestUnexpectedResult,
    };
    var r = fs2.reader(f);
    var n: usize = 0;
    while (try r.next()) |chunk| : (n += chunk.len) try testing.expect(std.mem.allEqual(u8, chunk, 0));
    try testing.expectEqual(@as(usize, 54000), n);
    try testing.expectError(error.NotAFile, makeFile(&fs2, "/states/gb", 10));
    try std.Io.Dir.cwd().writeFile(testing.io, .{ .sub_path = "build/ps-test/sd-made.img", .data = img });
}

test "a save from the card into cartridge RAM, the RAM back into the same blocks; nothing else changes" {
    const gpa = testing.allocator;
    const img = try std.Io.Dir.cwd().readFileAlloc(testing.io, "build/ps-test/sd.img", gpa, .limited(1 << 27));
    defer gpa.free(img);
    const orig = try gpa.dupe(u8, img);
    defer gpa.free(orig);
    var dev: ImgDev = .{ .data = img };
    defer dev.writes.deinit(gpa);
    var fs = try fat.Fs(ImgDev).mount(&dev);
    const spec = gb.save.?;

    // the cartridges make sd gave a slot: count.gb (8 KiB, a stored save)
    // and blank.gb (32 KiB, none stored: zeros); plain.gb has no battery
    var pb: [max_path]u8 = undefined;
    const e = switch (try lookup(&fs, savePath(&pb, "/gb/games", "count.gb").?, 0x2000)) {
        .file => |f| f,
        else => return error.TestUnexpectedResult,
    };
    switch (try lookup(&fs, savePath(&pb, "/gb/games", "blank.gb").?, 0x8000)) {
        .file => |f| {
            var z: [0x8000]u8 = undefined;
            try testing.expectEqual(@as(usize, 0x8000), try fs.readFile(f, &z));
            for (z) |b| try testing.expectEqual(@as(u8, 0), b);
        },
        else => return error.TestUnexpectedResult,
    }
    try testing.expect(try lookup(&fs, savePath(&pb, "/gb/games", "plain.gb").?, 0x2000) == .missing);
    try testing.expectEqual(Found{ .wrong_size = 0x2000 }, try lookup(&fs, savePath(&pb, "/GB/Games", "COUNT.GB").?, 0x8000));

    // into cartridge RAM, while held
    var model: GbModel = .{};
    var link: pl.Link(GbModel) = .{ .port = &model };
    link.setHold(true);
    try testing.expectEqual(@as(u32, 0x2000), try load(&fs, e, &link, spec, false));
    link.setHold(false);
    for (model.cram[0..0x2000], 0..) |b, i| try testing.expectEqual(storedByte(i), b);
    try testing.expectEqual(@as(u8, 0xEE), model.cram[0x2000]);
    try testing.expectEqual(@as(u32, 0), model.ignored);

    // nothing changed: every block read, none written
    model.dirty = true;
    var buf: [512]u8 = undefined;
    var old: [512]u8 = undefined;
    const w0 = try writeBack(&fs, &dev, e, &link, spec, false, &buf, &old);
    try testing.expectEqual(Written{ .blocks = 16, .written = 0 }, w0);
    try testing.expect(!model.dirty and !model.read_back);

    // the game changes three bytes in two blocks
    model.cram[0] = 0xAA;
    model.cram[0x1FFF] = 0xBB;
    model.cram[0x1FFE] = 0xCC;
    const w1 = try writeBack(&fs, &dev, e, &link, spec, false, &buf, &old);
    try testing.expectEqual(Written{ .blocks = 16, .written = 2 }, w1);

    // the file holds the RAM; every other block of the card is unchanged
    var file: [0x2000]u8 = undefined;
    try testing.expectEqual(@as(usize, 0x2000), try fs.readFile(e, &file));
    try testing.expectEqualSlices(u8, model.cram[0..0x2000], &file);
    const first = try fs.firstBlock(e);
    var changed: usize = 0;
    var lba: usize = 0;
    while (lba * 512 < img.len) : (lba += 1) {
        if (std.mem.eql(u8, img[lba * 512 ..][0..512], orig[lba * 512 ..][0..512])) continue;
        changed += 1;
        try testing.expect(lba >= first and lba < first + 16);
    }
    try testing.expectEqual(@as(usize, 2), changed);
    // for fsck.fat (Makefile test-zig)
    try std.Io.Dir.cwd().writeFile(testing.io, .{ .sub_path = "build/ps-test/sd-saved.img", .data = img });
}

test "MBC3's clock: the footer into the clock's start values, the clock back into the footer" {
    const gpa = testing.allocator;
    const img = try std.Io.Dir.cwd().readFileAlloc(testing.io, "build/ps-test/sd.img", gpa, .limited(1 << 27));
    defer gpa.free(img);
    var dev: ImgDev = .{ .data = img };
    defer dev.writes.deinit(gpa);
    var fs = try fat.Fs(ImgDev).mount(&dev);
    const spec = gb.save.?;
    var pb: [max_path]u8 = undefined;
    // clock.gb (make test-zig): MBC3+TIMER+RAM+BATTERY, 32 KiB + the footer
    const e = (try lookup(&fs, savePath(&pb, "/gb/games", "clock.gb").?, 0x8000 + menu.clock_footer)).file;
    var model: GbModel = .{};
    var link: pl.Link(GbModel) = .{ .port = &model };
    link.setHold(true);
    try testing.expectEqual(@as(u32, 0x8030), try load(&fs, e, &link, spec, true));
    link.setHold(false);
    try testing.expectEqualSlices(u8, &([_]u8{0} ** 10), &model.clock_init); // a blank save: 00:00:00
    try testing.expectEqual(@as(u32, 0), model.ignored);
    // the game ran: RAM and clock changed
    model.cram[0x7FFF] = 0x5A;
    model.clock_now = .{ 30, 15, 7, 0x2A, 0x41, 29, 15, 7, 0x2A, 0x41 };
    var buf: [512]u8 = undefined;
    var old: [512]u8 = undefined;
    const w = try writeBack(&fs, &dev, e, &link, spec, true, &buf, &old);
    try testing.expectEqual(Written{ .blocks = 65, .written = 2 }, w);
    var file: [0x8030]u8 = undefined;
    try testing.expectEqual(@as(usize, 0x8030), try fs.readFile(e, &file));
    try testing.expectEqual(@as(u8, 0x5A), file[0x7FFF]);
    // the footer: 32-bit little-endian registers, then a zero time stamp
    try testing.expectEqualSlices(u8, &[_]u8{ 30, 0, 0, 0, 15, 0, 0, 0, 7, 0, 0, 0, 0x2A, 0, 0, 0, 0x41, 0, 0, 0 }, file[0x8000..0x8014]);
    try testing.expectEqual(@as(u8, 29), file[0x8014]);
    try testing.expectEqualSlices(u8, &([_]u8{0} ** 8), file[0x8028..0x8030]);
    // and in again: the clock starts where it was
    link.setHold(true);
    _ = try load(&fs, e, &link, spec, true);
    link.setHold(false);
    try testing.expectEqualSlices(u8, &model.clock_now, &model.clock_init);
    try testing.expectEqual(@as(u32, 0x8000), ramBytes(0x8030, true));
    try testing.expectEqual(@as(u32, 0x2000), ramBytes(0x2000, false));
}

test "write-back without a PL answering stops with an error and writes no block" {
    const Mute = struct {
        last: u32 = 0,
        pub fn write(m: *@This(), v: u32) void {
            m.last = v;
        }
        pub fn read(_: *@This()) u32 {
            return 0;
        }
        pub fn delayUs(_: *@This(), _: u32) void {}
    };
    const gpa = testing.allocator;
    const img = try std.Io.Dir.cwd().readFileAlloc(testing.io, "build/ps-test/sd.img", gpa, .limited(1 << 27));
    defer gpa.free(img);
    var dev: ImgDev = .{ .data = img };
    defer dev.writes.deinit(gpa);
    var fs = try fat.Fs(ImgDev).mount(&dev);
    var pb: [max_path]u8 = undefined;
    const e = (try lookup(&fs, savePath(&pb, "/gb/games", "count.gb").?, 0x2000)).file;
    var mute: Mute = .{};
    var link: pl.Link(Mute) = .{ .port = &mute };
    var buf: [512]u8 = undefined;
    var old: [512]u8 = undefined;
    try testing.expectError(error.NoAnswer, writeBack(&fs, &dev, e, &link, gb.save.?, false, &buf, &old));
    try testing.expectEqual(@as(usize, 0), dev.writes.items.len);
}
