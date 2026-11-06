//! nes-restore (side track N, N.7): the NES's restore routine for a state
//! table, as the PS app builds it (platform/ps/systems/nes_restore.zig), for
//! the tests that run it on the spec and in Clash simulation.
//!   nes-restore TABLE RAM CIRAM OAM OVERLAY
//! (TABLE 0x100 bytes, RAM and CIRAM 0x800, OAM 0x100; OVERLAY written,
//! 0x800.) Prints "end 0xNNNN cycles N" (on stderr), or the refusal and
//! exits 1.

const std = @import("std");
const nes_restore = @import("nes_restore");

pub fn main(init: std.process.Init) !void {
    const io = init.io;
    const arena = init.arena.allocator();
    const args = try init.minimal.args.toSlice(arena);
    if (args.len != 6) {
        std.debug.print("usage: nes-restore TABLE RAM CIRAM OAM OVERLAY\n", .{});
        std.process.exit(2);
    }
    const cwd = std.Io.Dir.cwd();
    const sizes = [4]usize{ nes_restore.table_size, nes_restore.ram_size, nes_restore.ciram_size, nes_restore.oam_size };
    var files: [4][]u8 = undefined;
    for (0..4) |i| {
        files[i] = try cwd.readFileAlloc(io, args[1 + i], arena, .limited(1 << 16));
        if (files[i].len != sizes[i]) {
            std.debug.print("nes-restore: {s}: {d} bytes, not {d}\n", .{ args[1 + i], files[i].len, sizes[i] });
            std.process.exit(1);
        }
    }
    var ovl: [nes_restore.overlay_size]u8 = undefined;
    const r = nes_restore.build(files[0][0..nes_restore.table_size], files[1][0..nes_restore.ram_size], files[2][0..nes_restore.ciram_size], files[3][0..nes_restore.oam_size], &ovl) catch |e| {
        std.debug.print("nes-restore: refused: {s}\n", .{@errorName(e)});
        std.process.exit(1);
    };
    try cwd.writeFile(io, .{ .sub_path = args[5], .data = &ovl });
    std.debug.print("end 0x{X:0>4} cycles {d}\n", .{ r.end, r.cycles });
}
