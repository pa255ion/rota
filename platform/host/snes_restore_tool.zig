//! snes-restore (M7 step 7.15a): the Super Nintendo's restore routine for
//! a state table, as the PS app builds it (platform/ps/systems/
//! snes_restore.zig), for the tests that run it on the spec and in Clash
//! simulation.
//!   snes-restore TABLE FILL OVERLAY
//! (TABLE 520 bytes; FILL 160: the DMA registers 4300h-437Fh, then OAM's
//! high table; OVERLAY written, 0x800.) Prints "end 0xNNNN
//! cycles N" (on stderr), or the refusal and exits 1.

const std = @import("std");
const snes_restore = @import("snes_restore");

pub fn main(init: std.process.Init) !void {
    const io = init.io;
    const arena = init.arena.allocator();
    const args = try init.minimal.args.toSlice(arena);
    if (args.len != 4) {
        std.debug.print("usage: snes-restore TABLE FILL OVERLAY\n", .{});
        std.process.exit(2);
    }
    const cwd = std.Io.Dir.cwd();
    const sizes = [2]usize{ snes_restore.table_size, snes_restore.fill_size };
    var files: [2][]u8 = undefined;
    for (0..2) |i| {
        files[i] = try cwd.readFileAlloc(io, args[1 + i], arena, .limited(1 << 16));
        if (files[i].len != sizes[i]) {
            std.debug.print("snes-restore: {s}: {d} bytes, not {d}\n", .{ args[1 + i], files[i].len, sizes[i] });
            std.process.exit(1);
        }
    }
    // (the restore test's mutants: SNES_RESTORE_MUTANT=n)
    if (init.environ_map.get("SNES_RESTORE_MUTANT")) |m| snes_restore.mutant = std.fmt.parseInt(u32, m, 10) catch 0;
    var ovl: [snes_restore.overlay_size]u8 = undefined;
    const r = snes_restore.build(files[0], files[1], &ovl) catch |e| {
        std.debug.print("snes-restore: refused: {s}\n", .{@errorName(e)});
        std.process.exit(1);
    };
    try cwd.writeFile(io, .{ .sub_path = args[3], .data = &ovl });
    // the checkpoints, for the tests (stdout): "mark PC FRAME LINE CLOCK"
    var buf: [64]u8 = undefined;
    var out = std.Io.File.stdout().writer(io, &buf);
    for (r.marks[0..r.nmarks]) |m| try out.interface.print("mark {d} {d} {d} {d}\n", .{ m.pc, m.frame, m.vc, m.hc });
    try out.interface.flush();
    std.debug.print("end 0x{X:0>4} cycles {d}\n", .{ r.end, r.cycles });
}
