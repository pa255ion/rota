//! The YM2413 oracle for sms-spec (M6 step 6.3, the user's choice
//! 2026-10-11): Nuked-OPLL (nukeykt, GPL-2.0-or-later; fetched at a pinned
//! commit by tools/fetch-roms.sh into roms/sms/nuked-opll, built here, never
//! committed) run as a black box. Our own model (Sms.Spec.Fm) is written
//! from docs/systems/ym2413.md, not from this code; sms-spec compares the
//! two pin by pin.
//!
//!   opll-oracle PROGRAM OUT CYCLES
//!
//! PROGRAM: register writes, 6 bytes each, sorted by cycle: the internal
//! cycle (u32, little-endian) before whose clock the write is presented,
//! the port (0 address, 1 data) and the byte. OUT: for every one of CYCLES
//! internal cycles (18 per sample), the MO and RO pins as they were before
//! that cycle's clock, two little-endian i16.

const std = @import("std");
const c = @cImport({
    @cInclude("stdio.h");
    @cInclude("opll.h");
});

fn fail(comptime fmt: []const u8, args: anytype) noreturn {
    std.debug.print("opll-oracle: " ++ fmt ++ "\n", args);
    std.process.exit(1);
}

pub fn main(init: std.process.Init) !void {
    const args = try init.minimal.args.toSlice(init.arena.allocator());
    if (args.len != 4) fail("usage: opll-oracle PROGRAM OUT CYCLES", .{});
    const cycles = std.fmt.parseInt(u32, args[3], 10) catch fail("CYCLES: not a number: {s}", .{args[3]});

    const in = c.fopen(args[1].ptr, "rb") orelse fail("can't read {s}", .{args[1]});
    defer _ = c.fclose(in);
    const out = c.fopen(args[2].ptr, "wb") orelse fail("can't write {s}", .{args[2]});
    defer _ = c.fclose(out);

    var chip: c.opll_t = undefined;
    c.OPLL_Reset(&chip, c.opll_type_ym2413);

    var rec: [6]u8 = undefined;
    var have = c.fread(&rec, 1, 6, in) == 6;
    var buf: [2]i32 = undefined;
    var pins: [4]u8 = undefined;
    var cyc: u32 = 0;
    while (cyc < cycles) : (cyc += 1) {
        while (have and std.mem.readInt(u32, rec[0..4], .little) == cyc) {
            c.OPLL_Write(&chip, rec[4], rec[5]);
            have = c.fread(&rec, 1, 6, in) == 6;
        }
        if (have and std.mem.readInt(u32, rec[0..4], .little) < cyc) fail("PROGRAM not sorted at cycle {d}", .{cyc});
        c.OPLL_Clock(&chip, &buf);
        std.mem.writeInt(i16, pins[0..2], @intCast(buf[0]), .little);
        std.mem.writeInt(i16, pins[2..4], @intCast(buf[1]), .little);
        if (c.fwrite(&pins, 1, 4, out) != 4) fail("write failed", .{});
    }
}
