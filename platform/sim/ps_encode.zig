//! ps-encode: PS link scripts (ps.zig's format) as the harness plays them,
//! hashed (side track K, D3). tools/ci-cache.py keys layer 4 runs on
//! these bytes rather than on the script's text or on pl.zig's source, so
//! a comment in either costs no run, and a change to how pl.zig encodes a
//! write reruns every run with a script.
//!
//!   ps-encode FILE...       "<sha256> FILE" per script (on stderr)
//!   ps-encode --fingerprint "<sha256> --fingerprint": a built-in script
//!                           with every command, for runs whose script is
//!                           generated while they run
//!
//! The hash covers each phase's GPIO word and length (u32 each, little
//! endian), in order. A script that doesn't compile prints "bad FILE" and
//! the program exits 1.

const std = @import("std");
const ps = @import("ps.zig");

/// Every command of the format once: a pl.zig change that alters any
/// encoding changes this script's hash.
const reference =
    \\hold 1
    \\bytes 0x000100 00017f80ff
    \\hold 0
    \\ctrl 1
    \\text 0 0 Reference: 0123456789
    \\inv 1 2 inverse
    \\bar 29 4 bar
    \\wait 10
    \\ctrl 2
    \\ctrl 0
    \\
;

fn digest(gpa: std.mem.Allocator, script: []const u8) ![32]u8 {
    const phases = try ps.compile(gpa, script);
    defer gpa.free(phases);
    var h = std.crypto.hash.sha2.Sha256.init(.{});
    for (phases) |p| {
        var b: [8]u8 = undefined;
        std.mem.writeInt(u32, b[0..4], p.word, .little);
        std.mem.writeInt(u32, b[4..8], p.ticks, .little);
        h.update(&b);
    }
    return h.finalResult();
}

pub fn main(init: std.process.Init) !void {
    const gpa = init.gpa;
    const io = init.io;
    const args = try init.minimal.args.toSlice(init.arena.allocator());
    const cwd = std.Io.Dir.cwd();
    var ok = true;
    for (args[1..]) |a| {
        const d = if (std.mem.eql(u8, a, "--fingerprint"))
            digest(gpa, reference)
        else blk: {
            const text = try cwd.readFileAlloc(io, a, gpa, .limited(16 << 20));
            defer gpa.free(text);
            break :blk digest(gpa, text);
        };
        if (d) |x| {
            std.debug.print("{x} {s}\n", .{ &x, a });
        } else |_| {
            std.debug.print("bad {s}\n", .{a});
            ok = false;
        }
    }
    if (!ok) std.process.exit(1);
}

test "the reference script compiles; comments don't change a script's hash" {
    const gpa = std.testing.allocator;
    _ = try digest(gpa, reference);
    const a = try digest(gpa, "ctrl 1\ntext 0 0 hi\n");
    const b = try digest(gpa, "# a comment\nctrl 1\ntext 0 0 hi\n");
    const c = try digest(gpa, "ctrl 1\ntext 0 0 ho\n");
    try std.testing.expectEqual(a, b);
    try std.testing.expect(!std.mem.eql(u8, &a, &c));
}
