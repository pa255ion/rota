//! The 800XL's cassettes on the board (side track Q, cassette depth 1;
//! PLAN.md "Depth 1 on the board"): a .cas file made into the tape image
//! the PL reads ("A800.Media.casImageFile", "A800.Core.casAnswer"): a
//! 16-byte header ("FUJI" and zeros: the PL's loader knows a tape by it),
//! then a 256-byte slot per data record (its length, at most 255, then its
//! bytes), then an empty slot. Used by the SD menu (relay.zig, through
//! `menu.System.ddr_image`) and by `link --load` (platform/host/link.zig).
//!
//! A .cas is chunks of a 4-byte type, a 16-bit length and a 16-bit aux
//! value (little-endian), then the body: "FUJI" first, "baud", and "data"
//! the records; other chunks (FSK, turbo) are skipped, as the spec skips
//! them ("A800.Media.parseCas"). A chunk cut short ends the tape there.

const std = @import("std");

const header = 16;
const slot = 256;

fn isTape(file: []const u8) bool {
    return file.len >= 8 and std.mem.eql(u8, file[0..4], "FUJI");
}

/// The data records' bodies, in order.
const Records = struct {
    file: []const u8,
    at: usize = 0,

    fn next(r: *Records) ?[]const u8 {
        while (r.at + 8 <= r.file.len) {
            const c = r.file[r.at..];
            const len = std.mem.readInt(u16, c[4..6], .little);
            if (8 + @as(usize, len) > c.len) return null;
            r.at += 8 + len;
            if (std.mem.eql(u8, c[0..4], "data")) return c[8..][0..len];
        }
        return null;
    }
};

/// The image's size for `file`; 0 if it isn't a tape (it goes into DDR
/// as it is).
pub fn imageSize(file: []const u8) usize {
    if (!isTape(file)) return 0;
    var r: Records = .{ .file = file };
    var n: usize = 0;
    while (r.next()) |_| n += 1;
    return header + slot * (n + 1);
}

/// The image of `file` into `out` (`imageSize(file)` bytes).
pub fn writeImage(file: []const u8, out: []u8) void {
    @memset(out, 0);
    @memcpy(out[0..4], "FUJI");
    var r: Records = .{ .file = file };
    var k: usize = 0;
    while (r.next()) |rec| : (k += 1) {
        const s = out[header + slot * k ..][0..slot];
        const n = @min(rec.len, slot - 1);
        s[0] = @intCast(n);
        @memcpy(s[1..][0..n], rec[0..n]);
    }
}

// --- tests (make test-zig) ---------------------------------------------------

const testing = std.testing;

fn chunk(out: *std.ArrayList(u8), tag: []const u8, aux: u16, body: []const u8) !void {
    try out.appendSlice(testing.allocator, tag);
    var w: [4]u8 = undefined;
    std.mem.writeInt(u16, w[0..2], @intCast(body.len), .little);
    std.mem.writeInt(u16, w[2..4], aux, .little);
    try out.appendSlice(testing.allocator, &w);
    try out.appendSlice(testing.allocator, body);
}

test "a tape: its header, one slot a data record (other chunks skipped), an empty slot last" {
    var f: std.ArrayList(u8) = .empty;
    defer f.deinit(testing.allocator);
    try chunk(&f, "FUJI", 0, "");
    try chunk(&f, "baud", 600, "");
    try chunk(&f, "data", 20000, &[_]u8{ 0x55, 0x55, 0xFC, 1, 2, 3 });
    try chunk(&f, "fsk ", 0, &[_]u8{ 9, 9 });
    var long: [300]u8 = undefined;
    for (&long, 0..) |*b, i| b.* = @truncate(i);
    try chunk(&f, "data", 250, &long);
    const n = imageSize(f.items);
    try testing.expectEqual(@as(usize, 16 + 256 * 3), n);
    const img = try testing.allocator.alloc(u8, n);
    defer testing.allocator.free(img);
    writeImage(f.items, img);
    try testing.expectEqualSlices(u8, "FUJI" ++ [_]u8{0} ** 12, img[0..16]);
    try testing.expectEqualSlices(u8, &[_]u8{ 6, 0x55, 0x55, 0xFC, 1, 2, 3, 0 }, img[16..24]);
    // a record over 255 bytes: cut (as A800.Media.casImage)
    try testing.expectEqual(@as(u8, 255), img[16 + 256]);
    try testing.expectEqualSlices(u8, long[0..255], img[16 + 256 + 1 ..][0..255]);
    try testing.expectEqual(@as(u8, 0), img[16 + 512]);
}

test "not a tape: 0 (into DDR as it is); a chunk cut short ends the tape" {
    try testing.expectEqual(@as(usize, 0), imageSize("CART\x00\x00\x00\x01"));
    var f: std.ArrayList(u8) = .empty;
    defer f.deinit(testing.allocator);
    try chunk(&f, "FUJI", 0, "");
    try chunk(&f, "data", 0, &[_]u8{ 1, 2 });
    try f.appendSlice(testing.allocator, "data\x40\x00\x00\x00\x01");
    try testing.expectEqual(@as(usize, 16 + 256 * 2), imageSize(f.items));
}
