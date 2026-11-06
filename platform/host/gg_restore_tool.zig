//! gg-restore (side track N, N.1): the Game Gear's and Master System's restore routine for a
//! state table, as the PS app builds it (platform/ps/systems/gg_restore.zig), for
//! the tests that run it on the spec and in Clash simulation.
//!   gg-restore TABLE OVERLAY   (TABLE: 0x140 bytes; OVERLAY written, 0x800)
//! Prints "end 0xNNNN tstates N" (on stderr), or the refusal and exits 1.

const std = @import("std");
const gg_restore = @import("gg_restore");

pub fn main(init: std.process.Init) !void {
    const io = init.io;
    const arena = init.arena.allocator();
    const args = try init.minimal.args.toSlice(arena);
    if (args.len != 3) {
        std.debug.print("usage: gg-restore TABLE OVERLAY\n", .{});
        std.process.exit(2);
    }
    const cwd = std.Io.Dir.cwd();
    const data = try cwd.readFileAlloc(io, args[1], arena, .limited(1 << 16));
    if (data.len != gg_restore.table_size) {
        std.debug.print("gg-restore: {s}: {d} bytes, a table has {d}\n", .{ args[1], data.len, gg_restore.table_size });
        std.process.exit(1);
    }
    var ovl: [gg_restore.overlay_size]u8 = undefined;
    const r = gg_restore.build(data[0..gg_restore.table_size], &ovl) catch |e| {
        std.debug.print("gg-restore: refused: {s}\n", .{@errorName(e)});
        std.process.exit(1);
    };
    try cwd.writeFile(io, .{ .sub_path = args[2], .data = &ovl });
    std.debug.print("end 0x{X:0>4} tstates {d}\n", .{ r.end, r.tstates });
}
