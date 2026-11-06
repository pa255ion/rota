//! `make sd` end to end on a real image: tools/mksd.sh makes
//! build/ps-test/sd.img (from a few generated ROMs, see the Makefile's
//! test-zig); the plan (sdimage.zig) is applied to a card full of old data,
//! and the PS app's FAT reader (platform/ps/fat.zig) must then see exactly
//! the files of the image. This checks that blocks past the plan's end
//! don't matter. (It first failed: a file of zeros, allocated last, lay
//! past the last non-zero block; the plan now ends after the last
//! allocated cluster.)
//!
//!   zig build test (build.zig: modules protocol and fat)

const std = @import("std");
const fat = @import("fat");
const sdimage = @import("sdimage.zig");

const MemDev = struct {
    data: []const u8,
    pub fn readBlock(d: *MemDev, lba: u32, buf: *[512]u8) !void {
        const off = @as(usize, lba) * 512;
        if (off + 512 > d.data.len) return error.OutOfRange;
        @memcpy(buf, d.data[off..][0..512]);
    }
};
const Fs = fat.Fs(MemDev);

/// "name size first-bytes" per entry of /chip8 and its subfolders, sorted.
fn listing(gpa: std.mem.Allocator, data: []const u8) ![]const u8 {
    var dev: MemDev = .{ .data = data };
    var fs = try Fs.mount(&dev);
    var out: std.ArrayList(u8) = .empty;
    errdefer out.deinit(gpa);
    const top = (try fs.find(fs.root(), "chip8")) orelse return error.NoChip8;
    var stack: [8]fat.Dir = undefined;
    stack[0] = fs.openDir(top);
    var n: usize = 1;
    while (n > 0) {
        n -= 1;
        var it = fs.iterate(stack[n]);
        while (try it.next()) |e| {
            if (e.is_dir) {
                stack[n] = fs.openDir(e);
                n += 1;
                try out.print(gpa, "{s}/\n", .{e.name()});
                continue;
            }
            var buf: [4096]u8 = undefined;
            const k = try fs.readFile(e, &buf);
            var h: u32 = 0;
            for (buf[0..k]) |b| h = h *% 31 +% b;
            try out.print(gpa, "{s} {d} {x}\n", .{ e.name(), e.size, h });
        }
    }
    return out.toOwnedSlice(gpa);
}

test "the planned writes on a dirty card give the image's files" {
    const gpa = std.testing.allocator;
    const io = std.testing.io;
    const image = try std.Io.Dir.cwd().readFileAlloc(io, "build/ps-test/sd.img", gpa, .limited(1 << 28));
    defer gpa.free(image);
    const ops = try sdimage.plan(gpa, image);
    defer gpa.free(ops);

    const card = try gpa.alloc(u8, image.len);
    defer gpa.free(card);
    for (card, 0..) |*b, i| b.* = @truncate(i *% 2654435761 >> 13); // old data
    var written: usize = 0;
    for (ops) |op| switch (op) {
        .zero => |z| {
            @memset(card[z.lba * 512 ..][0 .. z.count * 512], 0);
            written += z.count;
        },
        .write => |l| {
            @memcpy(card[l * 512 ..][0..512], sdimage.blk(image, l));
            written += 1;
        },
    };
    // only the start of the 64 MiB is written
    try std.testing.expect(written * 512 < image.len / 8);

    const want = try listing(gpa, image);
    defer gpa.free(want);
    const got = try listing(gpa, card);
    defer gpa.free(got);
    try std.testing.expect(std.mem.indexOf(u8, want, "Space Invaders [test].ch8") != null);
    try std.testing.expectEqualStrings(want, got);
}
