//! The Super Nintendo's save states (M7 step 7.14c): its Spec for
//! savestate.zig, Snes.StateHooks's link map (docs/ARCHITECTURE.md,
//! reserved 2026-10-23), the direct path as CHIP-8's (no restore routine).
//! The record is the core's register chain (Snes.Core.CoreRegs, 1101
//! bytes): every access to it shifts it a byte, so it is read once round,
//! in order, and written the same way, which savestate.zig's record does.
//! SRAM goes through the battery saves' windows (the row's `save`).

const std = @import("std");
const savestate = @import("../savestate.zig");
const snes_restore = @import("snes_restore.zig");

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
    // (the CPU build's: its generator's 8 KiB of code fit the OCM since side
    // K's M1 moved .bss to DDR, 2026-11-03)
    .alt = &spec_cpu,
    .alt_version = 2,
};

/// The CPU build's (step 7.15d; SNES_STATES=cpu, the status's version 2:
/// Snes.StateHooks): a load through the 65C816's routine. The record is
/// the table window (0x00000, read only: the table, then OAM's high
/// table, 552 bytes); the sound module's record is the chain at 0x00800
/// (Snes.Core.apuChainBytes, 428 bytes: a section, written back as it
/// was read, a byte a shift), then the memories as the direct build's.
/// The routine borrows DMA channel 0 and puts back every DMA register
/// (the section's bytes, a fill) and OAM's high table (the table's last
/// 32).
pub const spec_cpu: savestate.Spec = .{
    .mode_addr = 0x140050,
    .wbase = 0x200000,
    .rbase = 0x300000,
    .status = 0x1F00,
    .sections = &.{
        .{ .addr = chain_addr, .len = chain_len },
        .{ .addr = 0x01000, .len = 0x200 },
        .{ .addr = 0x01200, .len = 0x200 },
        .{ .addr = dma_addr, .len = 0x80 },
        .{ .addr = 0x01500, .len = 0x200 },
        .{ .addr = 0x10000, .len = 0x10000 },
        .{ .addr = 0x20000, .len = 0x10000 },
        .{ .addr = 0x40000, .len = 0x20000 },
    },
    .cram = sramWhole,
    .restore = .{
        .table_size = table_window,
        .overlay_size = snes_restore.overlay_size,
        .overlay_addr = 0x6000,
        .end_addr = 0x1F10,
        .build = build,
        // the DMA registers, just after the table (in the overlay's room,
        // which `build` copies them out of first)
        .fills = &.{.{ .addr = dma_addr, .table = table_window, .len = 0x80 }},
    },
};

/// The CPU build's table window: the table and OAM's high table.
pub const table_window: u16 = snes_restore.table_size + 32;
const chain_addr: u32 = 0x00800;
/// Snes.Core.apuChainBytes (the sound module's record and its padding)
const chain_len: u32 = 428;
const dma_addr: u32 = 0x01400;

/// The routine for a slot's table into the overlay (savestate.prepare:
/// the DMA registers in the overlay's first 128 bytes): its end address,
/// or null for a state it can't place.
fn build(table: []const u8, overlay: []u8) ?u16 {
    if (table.len < table_window or overlay.len < snes_restore.overlay_size) return null;
    var fill: [snes_restore.fill_size]u8 = undefined;
    @memcpy(fill[0..0x80], overlay[0..0x80]);
    @memcpy(fill[0x80..], table[snes_restore.table_size..table_window]);
    const r = snes_restore.build(table[0..snes_restore.table_size], &fill, overlay[0..snes_restore.overlay_size]) catch return null;
    return r.end;
}

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

test "the CPU build's spec: picked by version 2, its sections in the windows, off the overlay and its arm" {
    var two = spec;
    two.alt = &spec_cpu;
    try std.testing.expect(savestate.pick(two, 2).restore != null);
    try std.testing.expect(savestate.pick(two, 1).restore == null);
    try std.testing.expect(savestate.pick(two, null).restore == null);
    for (spec_cpu.sections) |sec| {
        try std.testing.expect(sec.addr >= table_window and sec.addr + sec.len <= 0x80000);
        try std.testing.expect(sec.addr + sec.len <= spec_cpu.status or sec.addr > spec_cpu.status);
        try std.testing.expect(sec.addr + sec.len <= 0x1F10 or sec.addr >= 0x6800);
    }
    try std.testing.expect(chain_addr + chain_len <= 0x01000);
    const rs = spec_cpu.restore.?;
    try std.testing.expectEqual(@as(usize, table_window + 0x800), rs.work());
    try std.testing.expect(rs.fills[0].len <= rs.overlay_size);
}

test "the CPU build's build: a state out of V-blank or too short refused" {
    var table = [_]u8{0} ** table_window;
    var overlay = [_]u8{0xAA} ** snes_restore.overlay_size;
    std.mem.writeInt(u32, table[snes_restore.F.vc * 4 ..][0..4], 100, .little);
    try std.testing.expectEqual(@as(?u16, null), build(&table, &overlay));
    try std.testing.expectEqual(@as(?u16, null), build(table[0..10], &overlay));
}
