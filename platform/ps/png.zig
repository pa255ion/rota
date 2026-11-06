//! PNG files for screenshots (side track O, O.4c): an RGB picture, 8 bits a
//! channel, as a PNG of uncompressed ("stored") deflate blocks, made 512
//! bytes at a time, without a buffer for the picture or the file (the PS
//! app's memory: the picture stays in DDR, `Source`).
//!
//! The file: the signature, IHDR, one IDAT chunk with the zlib stream
//! (RFC 1950: header 0x78 0x01, the scanlines' bytes in stored blocks of up
//! to 65535 (RFC 1951, section 3.2.4: BFINAL/BTYPE 00, LEN, NLEN), then
//! Adler-32), IEND. Each scanline is filter type 0 then its pixels' R G B.
//! The size is known before the first byte (`size`): the board makes the
//! file at its size (fat.zig's create), then fills its blocks.
const std = @import("std");

const signature = [8]u8{ 0x89, 'P', 'N', 'G', '\r', '\n', 0x1A, '\n' };
const max_stored = 65535;

/// Bytes of the scanlines (filter byte and R G B each pixel).
fn rawLen(w: u32, h: u32) u32 {
    return h * (1 + 3 * w);
}

fn blocks(w: u32, h: u32) u32 {
    return @max(1, (rawLen(w, h) + max_stored - 1) / max_stored);
}

fn zlibLen(w: u32, h: u32) u32 {
    return 2 + rawLen(w, h) + 5 * blocks(w, h) + 4;
}

/// The file's size for a w x h picture.
pub fn size(w: u32, h: u32) u32 {
    return 8 + (12 + 13) + (12 + zlibLen(w, h)) + 12;
}

/// The picture's bytes: `rgb(i)` is pixel i (raster order).
pub fn Writer(comptime Source: type) type {
    return struct {
        const Self = @This();
        src: Source,
        w: u32,
        h: u32,
        /// bytes of the file made so far
        pos: u32 = 0,
        /// the raw (scanline) byte next, and the pixel it is from
        raw: u32 = 0,
        px: [3]u8 = undefined,
        crc: std.hash.Crc32 = .init(),
        adler: std.hash.Adler32 = .{},

        pub fn init(src: Source, w: u32, h: u32) Self {
            return .{ .src = src, .w = w, .h = h };
        }

        pub fn done(s: *const Self) bool {
            return s.pos == size(s.w, s.h);
        }

        /// The next up to 512 bytes into `out`; how many (512 but the last).
        pub fn next(s: *Self, out: *[512]u8) usize {
            var n: usize = 0;
            while (n < 512 and !s.done()) : (n += 1) out[n] = s.byte();
            return n;
        }

        fn byte(s: *Self) u8 {
            const w = s.w;
            const h = s.h;
            const p = s.pos;
            s.pos += 1;
            const ihdr_at: u32 = 8;
            const idat_at: u32 = ihdr_at + 25;
            const zl = zlibLen(w, h);
            const iend_at: u32 = idat_at + 12 + zl;
            if (p < ihdr_at) return signature[p];
            if (p < idat_at) {
            const q = p - ihdr_at;
            return s.chunkByte(q, "IHDR", 13, if (q >= 8 and q < 8 + 13) ihdrByte(w, h, q - 8) else 0);
        }
            if (p < iend_at) {
                const q = p - idat_at;
                if (q < 8 or q >= 8 + zl) return s.chunkByte(q, "IDAT", zl, 0);
                return s.chunkByte(q, "IDAT", zl, s.zlibByte(q - 8));
            }
            return s.chunkByte(p - iend_at, "IEND", 0, 0);
        }

        /// Byte `q` of a chunk of `len` data bytes: length, type, data (the
        /// one given), CRC (over type and data).
        fn chunkByte(s: *Self, q: u32, comptime ty: *const [4]u8, len: u32, data: u8) u8 {
            if (q < 4) {
                if (q == 0) s.crc = .init();
                return @truncate(len >> @intCast(8 * (3 - q)));
            }
            if (q < 8 + len) {
                const b = if (q < 8) ty[q - 4] else data;
                s.crc.update(&.{b});
                return b;
            }
            const c = s.crc.final();
            return @truncate(c >> @intCast(8 * (3 - (q - 8 - len))));
        }

        /// Byte `z` of the zlib stream.
        fn zlibByte(s: *Self, z: u32) u8 {
            const raw_len = rawLen(s.w, s.h);
            if (z == 0) return 0x78;
            if (z == 1) return 0x01; // 0x7801: no compression, check bits right
            const zl = zlibLen(s.w, s.h);
            if (z >= zl - 4) {
                const a = s.adler.adler;
                return @truncate(a >> @intCast(8 * (3 - (z - (zl - 4)))));
            }
            // blocks: 5 header bytes, then their data
            const off = z - 2;
            const blk = off / (5 + max_stored);
            const in = off % (5 + max_stored);
            const start = blk * max_stored;
            const len: u32 = @min(max_stored, raw_len - start);
            if (in < 5) {
                const last = start + len == raw_len;
                return switch (in) {
                    0 => @intFromBool(last),
                    1 => @truncate(len),
                    2 => @truncate(len >> 8),
                    3 => @truncate(~len),
                    else => @truncate(~len >> 8),
                };
            }
            const b = s.rawByte();
            s.adler.update(&.{b});
            return b;
        }

        /// The next scanline byte (they come in order).
        fn rawByte(s: *Self) u8 {
            const line = 1 + 3 * s.w;
            const k = s.raw;
            s.raw += 1;
            const col = k % line;
            if (col == 0) return 0; // filter: none
            const c = (col - 1) % 3;
            if (c == 0) s.px = s.src.rgb((k / line) * s.w + (col - 1) / 3);
            return s.px[c];
        }
    };
}

fn ihdrByte(w: u32, h: u32, i: u32) u8 {
    return switch (i) {
        0...3 => @truncate(w >> @intCast(8 * (3 - i))),
        4...7 => @truncate(h >> @intCast(8 * (7 - i))),
        8 => 8, // bit depth
        9 => 2, // colour type: RGB
        else => 0, // compression, filter, interlace
    };
}

// --- tests: the file read back by an independent reader ---------------------

const TestSource = struct {
    w: u32,
    pub fn rgb(s: TestSource, i: u32) [3]u8 {
        return .{ @truncate(i *% 7), @truncate(i / s.w *% 13 + 1), @truncate(i ^ 0x5A) };
    }
};

fn makeFile(gpa: std.mem.Allocator, w: u32, h: u32) ![]u8 {
    var wr = Writer(TestSource).init(.{ .w = w }, w, h);
    const out = try gpa.alloc(u8, size(w, h));
    var at: usize = 0;
    var blk: [512]u8 = undefined;
    while (!wr.done()) {
        const n = wr.next(&blk);
        @memcpy(out[at..][0..n], blk[0..n]);
        at += n;
    }
    try std.testing.expectEqual(out.len, at);
    return out;
}

/// The chunks checked (lengths, CRCs) and the scanlines out of the stored
/// blocks (headers, NLEN, Adler-32).
fn readBack(gpa: std.mem.Allocator, file: []const u8, w: u32, h: u32) ![]u8 {
    try std.testing.expectEqualSlices(u8, &signature, file[0..8]);
    var at: usize = 8;
    var zlib: std.ArrayList(u8) = .empty;
    defer zlib.deinit(gpa);
    var seen_iend = false;
    while (at < file.len) {
        const len = std.mem.readInt(u32, file[at..][0..4], .big);
        const ty = file[at + 4 ..][0..4];
        const data = file[at + 8 ..][0..len];
        const crc = std.mem.readInt(u32, file[at + 8 + len ..][0..4], .big);
        var c = std.hash.Crc32.init();
        c.update(ty);
        c.update(data);
        try std.testing.expectEqual(c.final(), crc);
        if (std.mem.eql(u8, ty, "IHDR")) {
            try std.testing.expectEqual(w, std.mem.readInt(u32, data[0..4], .big));
            try std.testing.expectEqual(h, std.mem.readInt(u32, data[4..8], .big));
            try std.testing.expectEqualSlices(u8, &.{ 8, 2, 0, 0, 0 }, data[8..13]);
        } else if (std.mem.eql(u8, ty, "IDAT")) {
            try zlib.appendSlice(gpa, data);
        } else if (std.mem.eql(u8, ty, "IEND")) seen_iend = true;
        at += 12 + len;
    }
    try std.testing.expect(seen_iend);
    const z = zlib.items;
    try std.testing.expectEqual(@as(u16, 0), std.mem.readInt(u16, z[0..2], .big) % 31);
    var raw: std.ArrayList(u8) = .empty;
    errdefer raw.deinit(gpa);
    var p: usize = 2;
    while (true) {
        const final = z[p] & 1;
        try std.testing.expectEqual(@as(u8, 0), z[p] >> 1); // stored
        const len = std.mem.readInt(u16, z[p + 1 ..][0..2], .little);
        const nlen = std.mem.readInt(u16, z[p + 3 ..][0..2], .little);
        try std.testing.expectEqual(len, ~nlen);
        try raw.appendSlice(gpa, z[p + 5 ..][0..len]);
        p += 5 + @as(usize, len);
        if (final == 1) break;
    }
    var a: std.hash.Adler32 = .{};
    a.update(raw.items);
    try std.testing.expectEqual(a.adler, std.mem.readInt(u32, z[p..][0..4], .big));
    try std.testing.expectEqual(z.len, p + 4);
    return raw.toOwnedSlice(gpa);
}

test "a PNG of stored blocks: chunks, CRCs, zlib, Adler-32 and every pixel right; its size known before" {
    const gpa = std.testing.allocator;
    for ([_][2]u32{ .{ 160, 144 }, .{ 256, 240 }, .{ 1, 1 }, .{ 7, 3 } }) |d| {
        const w, const h = d;
        const f = try makeFile(gpa, w, h);
        defer gpa.free(f);
        try std.testing.expectEqual(@as(usize, size(w, h)), f.len);
        const raw = try readBack(gpa, f, w, h);
        defer gpa.free(raw);
        try std.testing.expectEqual(@as(usize, rawLen(w, h)), raw.len);
        const src: TestSource = .{ .w = w };
        for (0..h) |y| {
            const line = raw[y * (1 + 3 * w) ..][0 .. 1 + 3 * w];
            try std.testing.expectEqual(@as(u8, 0), line[0]);
            for (0..w) |x| {
                const want = src.rgb(@intCast(y * w + x));
                try std.testing.expectEqualSlices(u8, &want, line[1 + 3 * x ..][0..3]);
            }
        }
    }
    // 160 x 144: two stored blocks (69264 bytes of scanlines)
    try std.testing.expectEqual(@as(u32, 2), blocks(160, 144));
    // one for make test-zig to decode with Python's zlib (an independent
    // inflater)
    const f = try makeFile(gpa, 160, 144);
    defer gpa.free(f);
    try std.Io.Dir.cwd().writeFile(std.testing.io, .{ .sub_path = "build/ps-test/shot.png", .data = f });
}
