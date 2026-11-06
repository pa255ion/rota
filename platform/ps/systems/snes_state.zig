//! The Super Nintendo's save states (M7 step 7.14c): its Spec for
//! savestate.zig, Snes.StateHooks's link map (docs/ARCHITECTURE.md,
//! reserved 2026-10-23), the direct path as CHIP-8's (no restore routine).
//! The record is the core's register chain (Snes.Core.CoreRegs, 1101
//! bytes): every access to it shifts it a byte, so it is read once round,
//! in order, and written the same way, which savestate.zig's record does.
//! SRAM goes through the battery saves' windows (the row's `save`).

const std = @import("std");
const savestate = @import("../savestate.zig");

/// Snes.StateHooks: the state mode as the others'; the windows (write
/// 0x200000, read 0x300000: the pad-2 bytes are at 0x160000); after the
/// chain CGRAM (a colour low byte first, written with its high one), OAM's
/// low table, the DMA registers, the DSP's samples (likewise in pairs),
/// ARAM, VRAM (both chips) and WRAM.
pub const spec: savestate.Spec = .{
    .mode_addr = 0x140050,
    .wbase = 0x200000,
    .rbase = 0x300000,
    .status = 0x1F00,
    .sections = &.{
        .{ .addr = 0x01000, .len = 0x200 },
        .{ .addr = 0x01200, .len = 0x200 },
        .{ .addr = 0x01400, .len = 0x80 },
        .{ .addr = 0x01500, .len = 0x200 },
        .{ .addr = 0x10000, .len = 0x10000 },
        .{ .addr = 0x20000, .len = 0x10000 },
        .{ .addr = 0x40000, .len = 0x20000 },
    },
    .cram = sramWhole,
};

/// SRAM in a state: the PL's whole 32 KiB, whatever the header says (the
/// header is past the first block, `head`; a cartridge's RAM without a
/// battery is in the state too). Snes.Machine takes its writes while
/// frozen.
fn sramWhole(_: []const u8) u32 {
    return 0x8000;
}

test "the sections fit the windows, below the status none but the chain" {
    for (spec.sections) |s| {
        try std.testing.expect(s.addr >= 0x1000 and s.addr + s.len <= 0x80000);
        try std.testing.expect(s.addr + s.len <= spec.status or s.addr > spec.status);
    }
    try std.testing.expect(@as(u32, spec.wbase) + 0x80000 <= spec.rbase);
}
