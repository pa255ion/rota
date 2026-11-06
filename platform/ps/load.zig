//! Loading a ROM file into a system over the PS link, for the menu
//! (relay.zig): the system table's checks, then the file streamed a block
//! at a time (fat.zig's Reader) to the system's load address (pl.zig's
//! Link), zeros up to its maximum if it wants them. Generic over the
//! reader and the link, so the desktop tests below run it on FAT images
//! with a model of the PL decoder.

const menu = @import("menu.zig");

pub const Prepared = union(enum) {
    /// the file's first block: go ahead with `stream`
    head: []const u8,
    /// a reason not to load it (shown on the menu's status line)
    refused: []const u8,
};

/// Size limit and the system's own check, on the first block. `r` is a
/// fresh reader of the file (`next() !?[]const u8`).
pub fn prepare(s: *const menu.System, size: u32, r: anytype) !Prepared {
    if (size > s.max_size) return .{ .refused = "Too big for this system." };
    const head = try r.next() orelse &[_]u8{};
    if (s.check) |check| if (check(head, size)) |why| return .{ .refused = why };
    return .{ .head = head };
}

/// Hold, the file (its first block `head`, then the rest of `r`), zeros
/// up to `max_size` if the system wants them, release. Returns the bytes
/// loaded. On a read error the hold stays on: no half-loaded ROM runs.
pub fn stream(s: *const menu.System, head: []const u8, r: anytype, link: anytype) !u32 {
    const n = try streamHeld(s, head, r, link);
    link.setHold(false);
    return n;
}

/// `stream` without the release: the core stays held, for more writes
/// before it starts (a battery save, save.zig).
pub fn streamHeld(s: *const menu.System, head: []const u8, r: anytype, link: anytype) !u32 {
    link.setHold(true);
    link.write(s.load_addr, head);
    var n: u32 = @intCast(head.len);
    while (try r.next()) |chunk| {
        link.data(chunk);
        n += @intCast(chunk.len);
    }
    if (s.clear_rest) {
        var k = n;
        while (k < s.max_size) : (k += 1) link.data(&.{0});
    }
    return n;
}

/// For a system whose ROM lives in DDR (`System.ddr_addr`; side track I,
/// the Game Boy's cartridge): the file `e` straight from the card into
/// memory, in runs of consecutive blocks (multi-block reads, as for
/// bitstreams: an 8 MiB cartridge is 16384 blocks). The caller holds the
/// system (its cache of DDR is emptied meanwhile) and releases it after.
/// `mem` has `write(addr: u32, bytes: []const u8)`. Returns the bytes.
pub fn streamDdr(s: *const menu.System, fs: anytype, card: anytype, e: fat.Entry, mem: anytype) !u32 {
    return streamTo(s.ddr_addr.?, fs, card, e, mem);
}

/// What `loadCompanion` did.
pub const CompanionLoad = union(enum) {
    /// the card has none (the system runs without it)
    none,
    /// in DDR: its size
    loaded: u32,
    /// on the card, but unusable: why (nothing loaded; the caller doesn't
    /// start the cartridge)
    refused: []const u8,
};

/// A system's companion file (`System.companion`, M6: the Master System's
/// BIOS) from its folder into DDR, if the card has it; before the ROM, with
/// the system held.
pub fn loadCompanion(s: *const menu.System, fs: anytype, card: anytype, mem: anytype) !CompanionLoad {
    const c = s.companion orelse return .none;
    const d = (try fs.find(fs.root(), s.dir)) orelse return .none;
    if (!d.is_dir) return .none;
    const e = (try fs.find(fs.openDir(d), c.name)) orelse return .none;
    if (e.is_dir) return .none;
    if (c.check(e.size)) |why| return .{ .refused = why };
    return .{ .loaded = try streamTo(c.ddr_addr, fs, card, e, mem) };
}

/// `loadCompanion` with the system held, and held it stays, whatever the
/// outcome: after `.none` or `.loaded` the caller loads the cartridge and
/// lets go; after `.refused` or a read error no game may run on (the old
/// one's save is no longer written back, and on a read error part of DDR
/// is the new file's), so the caller says so and starting a game lets go
/// of the hold as always (side K's review, 2026-10-14; as side L's
/// savestate.loadHeld).
pub fn loadCompanionHeld(s: *const menu.System, fs: anytype, card: anytype, mem: anytype, link: anytype) !CompanionLoad {
    link.setHold(true);
    return loadCompanion(s, fs, card, mem);
}

/// The file `e` into memory from `addr` (see `streamDdr`).
pub fn streamTo(addr: u32, fs: anytype, card: anytype, e: fat.Entry, mem: anytype) !u32 {
    const Sink = struct {
        mem: @TypeOf(mem),
        addr: u32,
        left: u32 = 0,
        pub fn block(k: *@This(), b: *const [512]u8) void {
            const n = @min(512, k.left);
            k.mem.write(k.addr, b[0..n]);
            k.addr += n;
            k.left -= n;
        }
    };
    var sink: Sink = .{ .mem = mem, .addr = addr };
    var runs = fs.runs(e, 0xFFFF);
    while (try runs.next()) |run| {
        sink.left = run.bytes;
        try card.readBlocks(run.lba, @intCast(run.blocks), &sink);
    }
    return e.size;
}

/// After `streamDdr`, for a system with `ddr_link` (side track D, the NES):
/// those parts of the file over the link, each to `load_addr` + its
/// offset, read back from the file's copy in DDR (`file`). The core is
/// still held. Returns the bytes sent.
pub fn sendFromDdr(s: *const menu.System, parts: [2]menu.Range, file: []const u8, link: anytype) u32 {
    var n: u32 = 0;
    for (parts) |p| {
        // a part running past the file's end is clipped to it (side track
        // Q: the 800XL's executables, whose length its first block can't
        // tell; side K agreed, 2026-10-21); one starting at or past it is
        // skipped
        if (p.len == 0 or p.off >= file.len) continue;
        const len = @min(p.len, @as(u32, @intCast(file.len)) - p.off);
        link.write(s.load_addr + @as(u24, @intCast(p.off)), file[p.off..][0..len]);
        n += len;
    }
    return n;
}

/// A system's `ddr_image` (side track Q, side K agreed 2026-10-26: the
/// 800XL's tapes) once its file is in DDR: `media` is the media region
/// from the system's `ddr_addr` on, the file its first `size` bytes; the
/// file is copied to `scratch` (an offset into `media`), its image written
/// at the start. The size there then: the image's, or the file's as it is
/// (no hook, a size of 0, an image that would reach `scratch`, or a file
/// that doesn't fit after it).
pub fn ddrImageIn(di: ?menu.DdrImage, media: []u8, size: u32, scratch: usize) u32 {
    const d = di orelse return size;
    if (size > media.len or scratch > media.len or size > media.len - scratch) return size;
    const n = d.size(media[0..size]);
    if (n == 0 or n > scratch) return size;
    const copy = media[scratch..][0..size];
    @memcpy(copy, media[0..size]);
    d.write(copy, media[0..n]);
    return @intCast(n);
}

// --- Tests: FAT images (tools/mkfat-test.sh) and a PL decoder model ---------
const std = @import("std");
const testing = std.testing;
const fat = @import("fat.zig");
const pl = @import("pl.zig");

const FileDev = struct {
    data: []const u8,
    /// multi-block reads (a file's data, not its directory) fail
    fail_data: bool = false,
    pub fn readBlock(d: *FileDev, lba: u32, buf: *[512]u8) !void {
        const off = @as(usize, lba) * 512;
        if (off + 512 > d.data.len) return error.OutOfRange;
        @memcpy(buf, d.data[off..][0..512]);
    }
    /// as sd.zig's multi-block read: each block to `sink.block`
    pub fn readBlocks(d: *FileDev, lba: u32, count: u16, sink: anytype) !void {
        if (d.fail_data) return error.ReadFailed;
        var b: [512]u8 = undefined;
        var k: u32 = 0;
        while (k < count) : (k += 1) {
            try d.readBlock(lba + k, &b);
            sink.block(&b);
        }
    }
};

/// Decodes the PS link like Platform.Ps.Link.linkStep and keeps writes
/// made while hold is high (as a system's loader does).
const PlModel = struct {
    mem: []u8,
    strobe: bool = false,
    addr: u24 = 0,
    held_writes: u32 = 0,
    loose_writes: u32 = 0,
    hold: bool = false,
    holds: u32 = 0,

    pub fn write(m: *PlModel, v: u32) void {
        const h = v & (1 << 26) != 0;
        if (h and !m.hold) m.holds += 1;
        m.hold = h;
        const st = v & (1 << 24) != 0;
        if (st == m.strobe) return;
        m.strobe = st;
        const d: u8 = @truncate(v >> 16);
        if (v & (1 << 25) != 0) {
            m.addr = m.addr << 8 | d;
            return;
        }
        if (m.hold and m.addr < m.mem.len) {
            m.mem[m.addr] = d;
            m.held_writes += 1;
        } else m.loose_writes += 1;
        m.addr +%= 1;
    }
    pub fn read(_: *PlModel) u32 {
        return 0;
    }
    pub fn delayUs(_: *PlModel, _: u32) void {}
};

fn pattern(i: usize, m: usize, a: usize) u8 {
    return @truncate(i * m + a);
}

fn loadFrom(img: []const u8, dir: []const u8, name: []const u8, s: *const menu.System, mem: []u8) !struct { Prepared, u32, PlModel } {
    var dev: FileDev = .{ .data = img };
    var fs = try fat.Fs(FileDev).mount(&dev);
    const d = if (dir.len == 0) fs.root() else fs.openDir((try fs.find(fs.root(), dir)).?);
    const e = (try fs.find(d, name)).?;
    var r = fs.reader(e);
    var model: PlModel = .{ .mem = mem };
    var link: pl.Link(PlModel) = .{ .port = &model };
    const p = try prepare(s, e.size, &r);
    const n = switch (p) {
        .head => |h| try stream(s, h, &r, &link),
        .refused => 0,
    };
    return .{ p, n, model };
}

test "a big file (BIG.BIN, many clusters) streams to the load address, nothing else written" {
    const img = try std.Io.Dir.cwd().readFileAlloc(testing.io, "build/ps-test/fat32.img", testing.allocator, .limited(1 << 27));
    defer testing.allocator.free(img);
    const sys: menu.System = .{ .id = 99, .name = "t", .dir = "", .exts = &.{".bin"}, .load_addr = 0x100, .max_size = 0x40000 };
    const mem = try testing.allocator.alloc(u8, 0x40100);
    defer testing.allocator.free(mem);
    @memset(mem, 0xEE);
    const p, const n, const model = try loadFrom(img, "", "BIG.BIN", &sys, mem);
    try testing.expect(p == .head);
    try testing.expectEqual(@as(u32, 200000), n);
    // BIG.BIN on fat32.img: byte i = (i * 7 + 3) & 0xFF (tools/mkfat-test.sh)
    for (0..n) |i| if (mem[0x100 + i] != pattern(i, 7, 3)) return error.TestUnexpectedResult;
    try testing.expectEqual(@as(u8, 0xEE), mem[0xFF]); // before: untouched
    try testing.expectEqual(@as(u8, 0xEE), mem[0x100 + n]); // no clearing asked for
    try testing.expectEqual(n, model.held_writes);
    try testing.expectEqual(@as(u32, 0), model.loose_writes);
    try testing.expectEqual(@as(u32, 1), model.holds);
    try testing.expect(!model.hold); // released
}

test "CHIP-8: the ROM, then zeros to the end of RAM" {
    const img = try std.Io.Dir.cwd().readFileAlloc(testing.io, "build/ps-test/fat16.img", testing.allocator, .limited(1 << 27));
    defer testing.allocator.free(img);
    var mem = [_]u8{0xEE} ** 0x1000;
    const p, const n, _ = try loadFrom(img, "chip8", "PONG.CH8", &menu.systems[0], &mem);
    try testing.expect(p == .head);
    try testing.expectEqual(@as(u32, 246), n);
    for (0..n) |i| try testing.expectEqual(pattern(i, 13, 5), mem[0x200 + i]);
    for (mem[0x200 + n ..]) |b| try testing.expectEqual(@as(u8, 0), b);
    for (mem[0..0x200]) |b| try testing.expectEqual(@as(u8, 0xEE), b); // the font area stays
}

test "refusals: too big, and the system's own check, write nothing" {
    const img = try std.Io.Dir.cwd().readFileAlloc(testing.io, "build/ps-test/fat32.img", testing.allocator, .limited(1 << 27));
    defer testing.allocator.free(img);
    var mem = [_]u8{0xEE} ** 0x1000;
    // BIG.BIN is 200000 bytes: more than CHIP-8's 3584
    const p1, _, const m1 = try loadFrom(img, "", "BIG.BIN", &menu.systems[0], &mem);
    try testing.expectEqualStrings("Too big for this system.", p1.refused);
    try testing.expectEqual(@as(u32, 0), m1.holds);
    // as a Game Boy cartridge: header byte 0x147 = (0x147 * 7 + 3) & 0xFF = 0xF4, no such MBC
    var big = try testing.allocator.alloc(u8, 0x40000);
    defer testing.allocator.free(big);
    const p2, _, const m2 = try loadFrom(img, "", "BIG.BIN", &menu.systems[1], big[0..]);
    try testing.expectEqualStrings("Cartridge type not supported (MBC).", p2.refused);
    try testing.expectEqual(@as(u32, 0), m2.holds);
}

test "into DDR (the Game Boy, side track I): a fragmented file and a big one, byte for byte, nothing past them" {
    const img = try std.Io.Dir.cwd().readFileAlloc(testing.io, "build/ps-test/fat32.img", testing.allocator, .limited(1 << 27));
    defer testing.allocator.free(img);
    const Mem = struct {
        bytes: []u8,
        base: u32,
        pub fn write(m: *@This(), addr: u32, b: []const u8) void {
            @memcpy(m.bytes[addr - m.base ..][0..b.len], b);
        }
    };
    const buf = try testing.allocator.alloc(u8, 0x40000);
    defer testing.allocator.free(buf);
    var dev: FileDev = .{ .data = img };
    var fs = try fat.Fs(FileDev).mount(&dev);
    const sys: menu.System = .{ .id = 99, .name = "t", .dir = "", .exts = &.{".bin"}, .load_addr = 0, .max_size = 0x40000, .ddr_addr = 0x0200_0000 };
    // FRAG.BIN: its cluster chain is not contiguous (tools/mkfat-test.sh)
    for ([_]struct { []const u8, u32, u8, u8 }{ .{ "FRAG.BIN", 5000, 13, 5 }, .{ "BIG.BIN", 200000, 7, 3 } }) |f| {
        const name, const len, const m, const a = f;
        @memset(buf, 0xEE);
        var mem: Mem = .{ .bytes = buf, .base = 0x0200_0000 };
        const e = (try fs.find(fs.root(), name)).?;
        try testing.expectEqual(len, try streamDdr(&sys, &fs, &dev, e, &mem));
        for (0..len) |i| if (buf[i] != pattern(i, m, a)) return error.TestUnexpectedResult;
        try testing.expectEqual(@as(u8, 0xEE), buf[len]);
    }
}

test "NES (side track D): after the file is in DDR, its header and CHR go over the link, PRG doesn't" {
    // a file as Kirby's Adventure: 16 + 512 KiB PRG + 256 KiB CHR
    const len = 16 + 0x80000 + 0x40000;
    const file = try testing.allocator.alloc(u8, len);
    defer testing.allocator.free(file);
    for (file, 0..) |*b, i| b.* = pattern(i, 7, 3);
    @memcpy(file[0..16], &[_]u8{ 0x4E, 0x45, 0x53, 0x1A, 32, 32, 0x40, 0, 0, 0, 0, 0, 0, 0, 0, 0 });
    const sys = &menu.systems[2];
    try testing.expectEqualStrings("NES", sys.name);
    try testing.expect(sys.check.?(file[0..512], len) == null);
    const mem = try testing.allocator.alloc(u8, len);
    defer testing.allocator.free(mem);
    @memset(mem, 0xEE);
    var model: PlModel = .{ .mem = mem };
    var link: pl.Link(PlModel) = .{ .port = &model };
    link.setHold(true);
    const n = sendFromDdr(sys, sys.ddr_link.?(file[0..512]), file, &link);
    try testing.expectEqual(@as(u32, 16 + 0x40000), n);
    try testing.expectEqualSlices(u8, file[0..16], mem[0..16]);
    try testing.expectEqualSlices(u8, file[16 + 0x80000 ..], mem[16 + 0x80000 ..]);
    for (mem[16 .. 16 + 0x80000]) |b| try testing.expectEqual(@as(u8, 0xEE), b); // PRG: DDR only
    try testing.expectEqual(n, model.held_writes);
}

test "a companion file (M6: the Master System's BIOS): none, a good one into DDR with its banks in the word, a bad size refused" {
    const img = try std.Io.Dir.cwd().readFileAlloc(testing.io, "build/ps-test/fat32.img", testing.allocator, .limited(1 << 27));
    defer testing.allocator.free(img);
    const Mem = struct {
        bytes: []u8,
        base: u32,
        writes: u32 = 0,
        pub fn write(m: *@This(), addr: u32, b: []const u8) void {
            @memcpy(m.bytes[addr - m.base ..][0..b.len], b);
            m.writes += 1;
        }
    };
    const buf = try testing.allocator.alloc(u8, 0x50000);
    defer testing.allocator.free(buf);
    var dev: FileDev = .{ .data = img };
    var fs = try fat.Fs(FileDev).mount(&dev);
    const sms = for (menu.systems) |x| {
        if (x.id == 8) break x;
    } else return error.TestUnexpectedResult;
    try testing.expectEqualStrings("Master System", sms.name);
    // the row as the card's folders (tools/mkfat-test.sh): /sms a 32 KiB
    // BIOS, /smsbad one of 20000 bytes, /smsnone none
    var row = sms;
    // none: nothing written, the word's bits 7-3 stay 0
    row.dir = "smsnone";
    var mem: Mem = .{ .bytes = buf, .base = 0x0210_0000 };
    try testing.expect(try loadCompanion(&row, &fs, &dev, &mem) == .none);
    try testing.expectEqual(@as(u32, 0), mem.writes);
    // a good one: byte for byte at 0x02100000, 2 banks in the word
    row.dir = "sms";
    @memset(buf, 0xEE);
    const r = try loadCompanion(&row, &fs, &dev, &mem);
    try testing.expectEqual(@as(u32, 32768), r.loaded);
    for (0..32768) |i| if (buf[i] != pattern(i, 11, 7)) return error.TestUnexpectedResult;
    try testing.expectEqual(@as(u8, 0xEE), buf[32768]);
    const w = row.companion.?.word(row.ddr_info.?.of(&([_]u8{0} ** 0x10000)), r.loaded);
    try testing.expectEqual(@as(u8, 2 << 3), w[0] & 0xF8);
    // a bad size: refused, nothing written
    row.dir = "smsbad";
    mem.writes = 0;
    const b = try loadCompanion(&row, &fs, &dev, &mem);
    try testing.expect(b == .refused);
    try testing.expectEqual(@as(u32, 0), mem.writes);
    // every other system has no companion
    for (menu.systems) |o| if (o.id != 8) try testing.expect(o.companion == null);
}

test "a companion refused or failing to read leaves the system held; a good one or none too, for the cartridge (side K's review)" {
    const img = try std.Io.Dir.cwd().readFileAlloc(testing.io, "build/ps-test/fat32.img", testing.allocator, .limited(1 << 27));
    defer testing.allocator.free(img);
    const Mem = struct {
        bytes: []u8,
        pub fn write(m: *@This(), addr: u32, b: []const u8) void {
            @memcpy(m.bytes[addr - 0x0210_0000 ..][0..b.len], b);
        }
    };
    const buf = try testing.allocator.alloc(u8, 0x50000);
    defer testing.allocator.free(buf);
    const pmem = try testing.allocator.alloc(u8, 1 << 16);
    defer testing.allocator.free(pmem);
    var dev: FileDev = .{ .data = img };
    var fs = try fat.Fs(FileDev).mount(&dev);
    var row = for (menu.systems) |x| {
        if (x.id == 8) break x;
    } else return error.TestUnexpectedResult;
    var mem: Mem = .{ .bytes = buf };
    const cases = [_]struct { dir: []const u8, fail: bool }{
        .{ .dir = "smsbad", .fail = false }, // refused
        .{ .dir = "sms", .fail = true }, // a read error
        .{ .dir = "sms", .fail = false }, // loaded
        .{ .dir = "smsnone", .fail = false }, // none
    };
    for (cases, 0..) |c, i| {
        var model: PlModel = .{ .mem = pmem };
        var link: pl.Link(PlModel) = .{ .port = &model };
        link.setHold(false);
        row.dir = c.dir;
        dev.fail_data = c.fail;
        const r = loadCompanionHeld(&row, &fs, &dev, &mem, &link);
        switch (i) {
            0 => try testing.expect((try r) == .refused),
            1 => try testing.expectError(error.ReadFailed, r),
            2 => try testing.expectEqual(@as(u32, 32768), (try r).loaded),
            else => try testing.expect((try r) == .none),
        }
        try testing.expect(model.hold);
    }
}

test "sendFromDdr: a part running past the file's end sends the file's rest; one past the end nothing" {
    const Link = struct {
        got: [64]u8 = undefined,
        n: usize = 0,
        at: u24 = 0,
        fn write(l: *@This(), a: u24, bytes: []const u8) void {
            l.at = a;
            @memcpy(l.got[l.n..][0..bytes.len], bytes);
            l.n += bytes.len;
        }
    };
    const sys: menu.System = .{ .id = 99, .name = "t", .dir = "", .exts = &.{".bin"}, .load_addr = 0x100, .max_size = 0x40000, .ddr_addr = 0x0200_0000 };
    const file = "0123456789";
    var l: Link = .{};
    try std.testing.expectEqual(@as(u32, 7), sendFromDdr(&sys, .{ .{ .off = 3, .len = 0x10000 }, .{ .off = 10, .len = 4 } }, file, &l));
    try std.testing.expectEqualStrings("3456789", l.got[0..l.n]);
    try std.testing.expectEqual(@as(u24, 0x103), l.at);
}

// side track Q: the ddr_image fallbacks (side K's review, 2026-10-26)
fn doubled(file: []const u8) usize {
    return if (file.len > 0 and file[0] == 'T') 2 * file.len else 0;
}
fn writeDoubled(file: []const u8, out: []u8) void {
    for (out, 0..) |*b, i| b.* = file[i / 2];
}
const doubling: menu.DdrImage = .{ .size = doubled, .write = writeDoubled };

test "ddrImageIn: the image written at the start, its size returned" {
    var media = [_]u8{0} ** 64;
    @memcpy(media[0..3], "Tab");
    try testing.expectEqual(@as(u32, 6), ddrImageIn(doubling, &media, 3, 32));
    try testing.expectEqualSlices(u8, "TTaabb", media[0..6]);
}

test "ddrImageIn: the file as it is with no hook, a size of 0, an image reaching the scratch, a file not fitting after it" {
    var media = [_]u8{0} ** 64;
    @memcpy(media[0..3], "Tab");
    try testing.expectEqual(@as(u32, 3), ddrImageIn(null, &media, 3, 32));
    @memcpy(media[0..3], "xab");
    try testing.expectEqual(@as(u32, 3), ddrImageIn(doubling, &media, 3, 32));
    @memcpy(media[0..3], "Tab");
    // an image of 6 reaching a scratch at 5
    try testing.expectEqual(@as(u32, 3), ddrImageIn(doubling, &media, 3, 5));
    // a file of 40 doesn't fit after a scratch at 32 (64 bytes)
    var big = [_]u8{'T'} ** 64;
    try testing.expectEqual(@as(u32, 40), ddrImageIn(doubling, &big, 40, 32));
    try testing.expectEqualSlices(u8, "Tab", media[0..3]);
}
