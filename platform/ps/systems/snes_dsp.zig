//! The DSP-1's firmware on the card (M7 step 7.16d, docs/systems/snes.md,
//! "The DSP-1"): the user's own dump, never fetched or committed, as
//! `dsp1.rom` in the system's folder (`make sd SNES_DSP1=FILE`). The menu's
//! row loads it into DDR at 0x02700000 before the cartridge, as the Master
//! System's BIOS (menu.Companion); the DSP-1 bitstream's core reads it from
//! there at reset into the chip's program and data ROMs (Snes.Dsp1). The
//! base bitstream ignores it. The cartridge word is left as it is.

const menu = @import("../menu.zig");

pub const dsp1: menu.Companion = .{
    .name = "dsp1.rom",
    .ddr_addr = 0x0270_0000,
    .max_size = 0x2000,
    .check = check,
    .word = word,
};

/// 8 KiB, the program ROM then the data ROM, little-endian ("newer" dumps:
/// Snes.Dsp1 reads that form only).
pub fn check(size: u32) ?[]const u8 {
    if (size != 0x2000) return "dsp1.rom: not 8 KiB (the newer, little-endian dumps)";
    return null;
}

pub fn word(w: [2]u8, size: u32) [2]u8 {
    _ = size;
    return w;
}

const std = @import("std");
const testing = std.testing;

test "the firmware: 8 KiB only; the word unchanged" {
    try testing.expect(check(0x2000) == null);
    try testing.expect(check(0x2800) != null);
    try testing.expect(check(0) != null);
    try testing.expectEqual([2]u8{ 0x12, 0x34 }, word(.{ 0x12, 0x34 }, 0x2000));
    try testing.expectEqual(@as(u32, 0x0270_0000), dsp1.ddr_addr);
}
