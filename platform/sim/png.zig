//! Minimal PNG for the simulation harness: write 8-bit RGB images, and read
//! back the 8-bit RGB/RGBA, non-interlaced PNGs that the harness and
//! JuicyPixels (the Haskell golden tests) write.

const std = @import("std");
const flate = std.compress.flate;

const signature = "\x89PNG\r\n\x1a\n";

pub const Image = struct {
    width: u32,
    height: u32,
    /// RGB8, raster order.
    rgb: []u8,

    pub fn deinit(img: *Image, gpa: std.mem.Allocator) void {
        gpa.free(img.rgb);
    }
};

/// Encode `rgb` (width * height * 3 bytes) as a PNG. Caller frees.
pub fn encode(gpa: std.mem.Allocator, width: u32, height: u32, rgb: []const u8) ![]u8 {
    std.debug.assert(rgb.len == @as(usize, width) * height * 3);

    // Scanlines with filter type 0 (none).
    const stride = @as(usize, width) * 3;
    const raw = try gpa.alloc(u8, height * (stride + 1));
    defer gpa.free(raw);
    for (0..height) |y| {
        raw[y * (stride + 1)] = 0;
        @memcpy(raw[y * (stride + 1) + 1 ..][0..stride], rgb[y * stride ..][0..stride]);
    }

    var z: std.Io.Writer.Allocating = try .initCapacity(gpa, 1 << 16);
    defer z.deinit();
    {
        const window = try gpa.alloc(u8, flate.max_window_len);
        defer gpa.free(window);
        var c = try flate.Compress.init(&z.writer, window, .zlib, .level_1);
        try c.writer.writeAll(raw);
        try c.finish();
    }

    var out: std.Io.Writer.Allocating = try .initCapacity(gpa, z.written().len + 64);
    errdefer out.deinit();
    try out.writer.writeAll(signature);
    var ihdr: [13]u8 = undefined;
    std.mem.writeInt(u32, ihdr[0..4], width, .big);
    std.mem.writeInt(u32, ihdr[4..8], height, .big);
    ihdr[8] = 8; // bit depth
    ihdr[9] = 2; // colour type: RGB
    ihdr[10] = 0; // deflate
    ihdr[11] = 0; // adaptive filtering
    ihdr[12] = 0; // no interlace
    try chunk(&out.writer, "IHDR", &ihdr);
    try chunk(&out.writer, "IDAT", z.written());
    try chunk(&out.writer, "IEND", "");
    return out.toOwnedSlice();
}

fn chunk(w: *std.Io.Writer, kind: *const [4]u8, data: []const u8) !void {
    try w.writeInt(u32, @intCast(data.len), .big);
    try w.writeAll(kind);
    try w.writeAll(data);
    var crc = std.hash.Crc32.init();
    crc.update(kind);
    crc.update(data);
    try w.writeInt(u32, crc.final(), .big);
}

pub const DecodeError = error{ NotPng, Unsupported, Corrupt };

/// Decode an 8-bit RGB or RGBA PNG (alpha is dropped). Caller frees.
pub fn decode(gpa: std.mem.Allocator, bytes: []const u8) !Image {
    if (!std.mem.startsWith(u8, bytes, signature)) return error.NotPng;
    var pos: usize = signature.len;
    var width: u32 = 0;
    var height: u32 = 0;
    var bpp: usize = 0;
    var idat: std.ArrayList(u8) = .empty;
    defer idat.deinit(gpa);
    while (true) {
        if (pos + 12 > bytes.len) return error.Corrupt;
        const len = std.mem.readInt(u32, bytes[pos..][0..4], .big);
        const kind = bytes[pos + 4 ..][0..4];
        if (pos + 12 + len > bytes.len) return error.Corrupt;
        const data = bytes[pos + 8 ..][0..len];
        pos += 12 + len;
        if (std.mem.eql(u8, kind, "IHDR")) {
            if (len != 13) return error.Corrupt;
            width = std.mem.readInt(u32, data[0..4], .big);
            height = std.mem.readInt(u32, data[4..8], .big);
            if (data[8] != 8 or data[10] != 0 or data[11] != 0 or data[12] != 0) return error.Unsupported;
            bpp = switch (data[9]) {
                2 => 3,
                6 => 4,
                else => return error.Unsupported,
            };
        } else if (std.mem.eql(u8, kind, "IDAT")) {
            try idat.appendSlice(gpa, data);
        } else if (std.mem.eql(u8, kind, "IEND")) {
            break;
        }
    }
    if (bpp == 0) return error.Corrupt;

    const stride = @as(usize, width) * bpp;
    const raw = try gpa.alloc(u8, height * (stride + 1));
    defer gpa.free(raw);
    {
        var in: std.Io.Reader = .fixed(idat.items);
        const window = try gpa.alloc(u8, flate.max_window_len);
        defer gpa.free(window);
        var d: flate.Decompress = .init(&in, .zlib, window);
        d.reader.readSliceAll(raw) catch return error.Corrupt;
    }

    // Undo the per-scanline filters in place; `prev` is the previous
    // unfiltered line (zeros for the first).
    const zero = try gpa.alloc(u8, stride);
    defer gpa.free(zero);
    @memset(zero, 0);
    for (0..height) |y| {
        const line = raw[y * (stride + 1) + 1 ..][0..stride];
        const prev = if (y == 0) zero else raw[(y - 1) * (stride + 1) + 1 ..][0..stride];
        const filter = raw[y * (stride + 1)];
        for (0..stride) |i| {
            const a: u8 = if (i >= bpp) line[i - bpp] else 0;
            const b: u8 = prev[i];
            const c: u8 = if (i >= bpp) prev[i - bpp] else 0;
            line[i] +%= switch (filter) {
                0 => 0,
                1 => a,
                2 => b,
                3 => @intCast((@as(u16, a) + b) / 2),
                4 => paeth(a, b, c),
                else => return error.Corrupt,
            };
        }
    }

    const rgb = try gpa.alloc(u8, @as(usize, width) * height * 3);
    for (0..height) |y| {
        const line = raw[y * (stride + 1) + 1 ..][0..stride];
        for (0..width) |x| {
            @memcpy(rgb[(y * width + x) * 3 ..][0..3], line[x * bpp ..][0..3]);
        }
    }
    return .{ .width = width, .height = height, .rgb = rgb };
}

fn paeth(a: u8, b: u8, c: u8) u8 {
    const p = @as(i16, a) + b - c;
    const pa = @abs(p - a);
    const pb = @abs(p - b);
    const pc = @abs(p - c);
    if (pa <= pb and pa <= pc) return a;
    if (pb <= pc) return b;
    return c;
}

test "encode then decode round-trips" {
    const gpa = std.testing.allocator;
    var rgb: [5 * 3 * 3]u8 = undefined;
    for (&rgb, 0..) |*p, i| p.* = @intCast((i * 37) & 0xFF);
    const bytes = try encode(gpa, 5, 3, &rgb);
    defer gpa.free(bytes);
    var img = try decode(gpa, bytes);
    defer img.deinit(gpa);
    try std.testing.expectEqual(@as(u32, 5), img.width);
    try std.testing.expectEqual(@as(u32, 3), img.height);
    try std.testing.expectEqualSlices(u8, &rgb, img.rgb);
}

test "paeth predictor" {
    try std.testing.expectEqual(@as(u8, 10), paeth(10, 20, 20));
    try std.testing.expectEqual(@as(u8, 20), paeth(10, 20, 10));
    try std.testing.expectEqual(@as(u8, 20), paeth(10, 20, 5));
    try std.testing.expectEqual(@as(u8, 2), paeth(1, 2, 0));
}
