//! Writing an SD card image through the board (`make sd`, for users without
//! a card reader): the image (tools/mksd.sh: MBR + FAT32 with the ROMs) is
//! cut into the relay's SD commands (platform/ps/protocol.zig). Runs of
//! zero blocks become one `Z` (the PS writes the zeros itself), every other
//! block a `D` with its 512 bytes. Only the start of the image is sent: up
//! to the last non-zero block or the end of the last allocated cluster
//! (read from the image's FAT: a file of zeros needs its zeros written),
//! whichever is later. Past that is free space of the file system, whose
//! old contents on the card don't matter (its FAT entries are zero).
//!
//! Side track G phase 2: the same range in chunks of protocol.chunk_blocks
//! blocks (`chunks`). For each, link asks the relay for the hash of what
//! the card holds there (C) and skips the chunk if it is the image's;
//! otherwise a zero chunk is a Z and any other an M (one multi-block
//! write). A card written before takes seconds instead of minutes; a new
//! card, a multi-block write per 8 KiB instead of a round trip per block.

const std = @import("std");
const protocol = @import("protocol");

pub const block = 512;

pub const Op = union(enum) {
    zero: struct { lba: u32, count: u32 },
    write: u32,
};

/// The commands for `image` (a whole number of blocks), in order.
pub fn plan(gpa: std.mem.Allocator, image: []const u8) ![]Op {
    var ops: std.ArrayList(Op) = .empty;
    errdefer ops.deinit(gpa);
    const end = sendEnd(image);
    if (end == 0) return ops.toOwnedSlice(gpa);
    var i: u32 = 0;
    while (i < end) {
        if (isZero(blk(image, i))) {
            var j = i;
            while (j < end and isZero(blk(image, j))) j += 1;
            try ops.append(gpa, .{ .zero = .{ .lba = i, .count = j - i } });
            i = j;
        } else {
            try ops.append(gpa, .{ .write = i });
            i += 1;
        }
    }
    return ops.toOwnedSlice(gpa);
}

/// The block after the last allocated cluster of a FAT16/FAT32 volume in
/// the image's first MBR partition (or at block 0), or 0 if there's none.
/// BPB fields as in fatgen103 (the PS's reader, platform/ps/fat.zig, has
/// the full story).
pub fn allocatedEnd(image: []const u8) u32 {
    if (image.len < 1024) return 0;
    const mbr = image[0..512];
    var start: u32 = 0;
    if (mbr[510] == 0x55 and mbr[511] == 0xAA and !isBootSector(mbr)) {
        const t = mbr[446 + 4];
        if (t != 0x06 and t != 0x0B and t != 0x0C and t != 0x0E and t != 0x04) return 0;
        start = rd32(mbr, 446 + 8);
    }
    if (@as(usize, start + 1) * block > image.len) return 0;
    const bs = blk(image, start);
    if (!isBootSector(bs) or rd16(bs, 11) != 512) return 0;
    const spc: u32 = bs[13];
    const reserved: u32 = rd16(bs, 14);
    const nfats: u32 = bs[16];
    const root_entries: u32 = rd16(bs, 17);
    const fat_size: u32 = if (rd16(bs, 22) != 0) rd16(bs, 22) else rd32(bs, 36);
    const total: u32 = if (rd16(bs, 19) != 0) rd16(bs, 19) else rd32(bs, 32);
    if (spc == 0 or nfats == 0 or fat_size == 0) return 0;
    const root_blocks = (root_entries * 32 + 511) / 512;
    const data_start = start + reserved + nfats * fat_size + root_blocks;
    const clusters = (total - (data_start - start)) / spc;
    if (clusters < 4085) return 0; // FAT12: not used by mksd.sh
    const fat32 = clusters >= 65525;
    const fat_off = @as(usize, start + reserved) * block;
    var last: u32 = 0;
    var c: u32 = 2;
    while (c < clusters + 2) : (c += 1) {
        const v = if (fat32) rd32(image, fat_off + c * 4) & 0x0FFF_FFFF else rd16(image, fat_off + c * 2);
        if (v != 0) last = c;
    }
    if (last == 0) return 0;
    return data_start + (last - 1) * spc;
}

fn isBootSector(b: *const [block]u8) bool {
    return b[510] == 0x55 and b[511] == 0xAA and (b[0] == 0xEB or b[0] == 0xE9) and rd16(b, 11) == 512;
}

fn rd16(b: []const u8, off: usize) u32 {
    return std.mem.readInt(u16, b[off..][0..2], .little);
}

fn rd32(b: []const u8, off: usize) u32 {
    return std.mem.readInt(u32, b[off..][0..4], .little);
}

pub fn blk(image: []const u8, lba: u32) *const [block]u8 {
    return image[@as(usize, lba) * block ..][0..block];
}

fn isZero(b: *const [block]u8) bool {
    return std.mem.allEqual(u8, b, 0);
}

/// The blocks of `image` that are sent: up to the last non-zero block or
/// the end of the last allocated cluster, whichever is later.
pub fn sendEnd(image: []const u8) u32 {
    const n: u32 = @intCast(image.len / block);
    var last: ?u32 = null;
    var i: u32 = 0;
    while (i < n) : (i += 1) if (!isZero(blk(image, i))) {
        last = i;
    };
    return @min(n, @max(if (last) |l| l + 1 else 0, allocatedEnd(image)));
}

/// A run of up to protocol.chunk_blocks blocks (side track G phase 2).
pub const Chunk = struct {
    lba: u32,
    count: u8,
    /// every byte zero: sent as Z, not M
    zero: bool,
};

/// `sendEnd(image)` blocks in chunks, aligned to protocol.chunk_blocks.
pub fn chunks(gpa: std.mem.Allocator, image: []const u8) ![]Chunk {
    var out: std.ArrayList(Chunk) = .empty;
    errdefer out.deinit(gpa);
    const end = sendEnd(image);
    var lba: u32 = 0;
    while (lba < end) {
        const n: u32 = @min(end - lba, protocol.chunk_blocks);
        const bytes = image[@as(usize, lba) * block ..][0 .. n * block];
        try out.append(gpa, .{ .lba = lba, .count = @intCast(n), .zero = std.mem.allEqual(u8, bytes, 0) });
        lba += n;
    }
    return out.toOwnedSlice(gpa);
}

pub fn chunkBytes(image: []const u8, c: Chunk) []const u8 {
    return image[@as(usize, c.lba) * block ..][0 .. @as(usize, c.count) * block];
}

/// "C<lba><count>": the card's hash of the chunk.
pub fn hashLine(buf: []u8, c: Chunk) []const u8 {
    return std.fmt.bufPrint(buf, "C{X:0>8}{X:0>2}\n", .{ c.lba, c.count }) catch unreachable;
}

/// "M<lba><count><sum>" (the chunk's bytes follow), or for a zero chunk
/// "Z<lba><count>".
pub fn writeLine(buf: []u8, c: Chunk, image: []const u8) []const u8 {
    if (c.zero) return std.fmt.bufPrint(buf, "Z{X:0>8}{X:0>8}\n", .{ c.lba, @as(u32, c.count) }) catch unreachable;
    var s: u16 = 0;
    for (chunkBytes(image, c)) |x| s = protocol.checksum(s, x);
    return std.fmt.bufPrint(buf, "M{X:0>8}{X:0>2}{X:0>4}\n", .{ c.lba, c.count, s }) catch unreachable;
}

pub fn checksum(b: *const [block]u8) u16 {
    var s: u16 = 0;
    for (b) |x| s = protocol.checksum(s, x);
    return s;
}

/// The command line for an op (without the payload of a write).
pub fn line(buf: []u8, op: Op, image: []const u8) []const u8 {
    return switch (op) {
        .zero => |z| std.fmt.bufPrint(buf, "Z{X:0>8}{X:0>8}\n", .{ z.lba, z.count }) catch unreachable,
        .write => |l| std.fmt.bufPrint(buf, "D{X:0>8}{X:0>4}\n", .{ l, checksum(blk(image, l)) }) catch unreachable,
    };
}

// --- Tests ---------------------------------------------------------------------
const testing = std.testing;

fn testImage(gpa: std.mem.Allocator) ![]u8 {
    // blocks: 0 data, 1-3 zero, 4 data, 5 data, 6-9 zero, 10 data, 11-15 zero
    const img = try gpa.alloc(u8, 16 * block);
    @memset(img, 0);
    for ([_]u32{ 0, 4, 5, 10 }) |b| {
        for (img[b * block ..][0..block], 0..) |*x, k| x.* = @truncate(b * 31 + k);
    }
    img[10 * block] = 0; // a block that is mostly, but not all, zero
    return img;
}

test "plan: zero runs and data blocks, nothing after the last data block" {
    const gpa = testing.allocator;
    const img = try testImage(gpa);
    defer gpa.free(img);
    const ops = try plan(gpa, img);
    defer gpa.free(ops);
    const want = [_]Op{
        .{ .write = 0 },                         .{ .zero = .{ .lba = 1, .count = 3 } },
        .{ .write = 4 },                         .{ .write = 5 },
        .{ .zero = .{ .lba = 6, .count = 4 } }, .{ .write = 10 },
    };
    try testing.expectEqualSlices(Op, &want, ops);
    const empty = [_]u8{0} ** (4 * block);
    const none = try plan(gpa, &empty);
    defer gpa.free(none);
    try testing.expectEqual(@as(usize, 0), none.len);
}

test "end to end: the wire bytes through the relay's parser rebuild the image on a dirty card" {
    const gpa = testing.allocator;
    const img = try testImage(gpa);
    defer gpa.free(img);
    const ops = try plan(gpa, img);
    defer gpa.free(ops);
    // the bytes link sends
    var wire: std.ArrayList(u8) = .empty;
    defer wire.deinit(gpa);
    try wire.appendSlice(gpa, "S\n");
    for (ops) |op| {
        var b: [32]u8 = undefined;
        try wire.appendSlice(gpa, line(&b, op, img));
        if (op == .write) try wire.appendSlice(gpa, blk(img, op.write));
    }
    // the relay's side: parser, payload, checksum, writes to a card full
    // of old data
    var card = [_]u8{0xA5} ** (32 * block);
    var p: protocol.Parser = .{};
    var opened = false;
    var i: usize = 0;
    while (i < wire.items.len) : (i += 1) {
        const cmd = p.feed(wire.items[i]) orelse continue;
        switch (cmd) {
            .sd_open => opened = true,
            .sd_zero => |z| @memset(card[z.lba * block ..][0 .. z.count * block], 0),
            .sd_write => |w| {
                const payload = wire.items[i + 1 ..][0..block];
                i += block;
                try testing.expectEqual(w.sum, checksum(payload));
                @memcpy(card[w.lba * block ..][0..block], payload);
            },
            else => return error.UnexpectedCommand,
        }
    }
    try testing.expect(opened);
    try testing.expectEqualSlices(u8, img[0 .. 11 * block], card[0 .. 11 * block]);
}

test "chunks: aligned runs of chunk_blocks up to the end, zero chunks marked" {
    const gpa = testing.allocator;
    const n = 40;
    const img = try gpa.alloc(u8, n * block);
    defer gpa.free(img);
    @memset(img, 0);
    img[3 * block] = 1; // chunk 0 has data
    img[37 * block + 5] = 2; // the last data block: 37, so the end is 38
    const cs = try chunks(gpa, img);
    defer gpa.free(cs);
    const want = [_]Chunk{
        .{ .lba = 0, .count = 16, .zero = false },
        .{ .lba = 16, .count = 16, .zero = true },
        .{ .lba = 32, .count = 6, .zero = false },
    };
    try testing.expectEqualSlices(Chunk, &want, cs);
    var b: [40]u8 = undefined;
    try testing.expectEqualStrings("C0000001010\n", hashLine(&b, cs[1]));
    try testing.expectEqualStrings("Z0000001000000010\n", writeLine(&b, cs[1], img));
    try testing.expectEqualStrings("M0000002006", writeLine(&b, cs[2], img)[0..11]);
}

test "end to end with chunks: hashes, M and Z through the relay's parser; unchanged chunks skipped" {
    const gpa = testing.allocator;
    const n = 40;
    const img = try gpa.alloc(u8, n * block);
    defer gpa.free(img);
    for (img, 0..) |*x, k| x.* = if (k / block >= 16 and k / block < 32) 0 else @truncate(k * 13 + 1);
    // the card: old data; chunk 0 already holds the image's bytes
    var card = [_]u8{0xA5} ** (48 * block);
    @memcpy(card[0 .. 16 * block], img[0 .. 16 * block]);
    const cs = try chunks(gpa, img);
    defer gpa.free(cs);
    var p: protocol.Parser = .{};
    var sent: u32 = 0;
    var skipped: u32 = 0;
    for (cs) |c| {
        // C: the relay hashes the card's chunk
        var lb: [40]u8 = undefined;
        var cmd: ?protocol.Command = null;
        for (hashLine(&lb, c)) |x| cmd = p.feed(x) orelse cmd;
        const h = cmd.?.sd_hash;
        const on_card = protocol.chunkHash(card[h.lba * block ..][0 .. @as(usize, h.count) * block]);
        if (on_card == protocol.chunkHash(chunkBytes(img, c))) {
            skipped += 1;
            continue;
        }
        cmd = null;
        for (writeLine(&lb, c, img)) |x| cmd = p.feed(x) orelse cmd;
        switch (cmd.?) {
            .sd_zero => |z| @memset(card[z.lba * block ..][0 .. z.count * block], 0),
            .sd_multi => |m| {
                const payload = chunkBytes(img, c);
                var s: u16 = 0;
                for (payload) |x| s = protocol.checksum(s, x);
                try testing.expectEqual(m.sum, s);
                @memcpy(card[m.lba * block ..][0..payload.len], payload);
            },
            else => return error.UnexpectedCommand,
        }
        sent += 1;
    }
    try testing.expectEqual(@as(u32, 1), skipped);
    try testing.expectEqual(@as(u32, 2), sent);
    try testing.expectEqualSlices(u8, img, card[0 .. n * block]);
}

