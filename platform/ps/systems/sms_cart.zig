//! The Master System's BIOS on the card (M6 step 6.7,
//! docs/systems/mastersystem.md, "The PS side"): the user's own dump,
//! never fetched or committed, as `bios.bin` in the system's folder (not
//! `.sms`: the menu lists those as games). menu.zig's row loads it into DDR
//! at 0x02100000 (after the cartridge's 1 MiB; the machine reads it there
//! as ROM from 100000h) before the cartridge, and the cartridge word
//! (gg_cart.zig's) then carries its size: bits 7-3 its 16 KiB banks, 0 for
//! none (Sms.Machine).

const menu = @import("../menu.zig");

pub const bios: menu.Companion = .{
    .name = "bios.bin",
    .ddr_addr = 0x0210_0000,
    .max_size = 256 * 1024,
    .check = check,
    .word = word,
};

/// A BIOS of 16 KiB times a power of two, up to 256 KiB (the machine's
/// bank mask: the export BIOSes of the SMS 2 are 128 or 256 KiB).
pub fn check(size: u32) ?[]const u8 {
    if (size == 0 or size % 0x4000 != 0) return "bios.bin: not a multiple of 16 KiB";
    const banks = size / 0x4000;
    if (banks & (banks - 1) != 0) return "bios.bin: not 16 KiB x 2^n";
    if (size > bios.max_size) return "bios.bin: more than 256 KiB";
    return null;
}

/// The word with the BIOS's banks in bits 7-3 (0: no BIOS).
pub fn word(w: [2]u8, size: u32) [2]u8 {
    const banks: u8 = @intCast(size / 0x4000);
    return .{ (w[0] & 0x07) | (banks << 3), w[1] };
}

const std = @import("std");
const testing = std.testing;

test "the BIOS's sizes and its banks in the word" {
    try testing.expect(check(0x4000) == null);
    try testing.expect(check(0x20000) == null);
    try testing.expect(check(0x40000) == null);
    try testing.expect(check(0x2000) != null); // 8 KiB: not 16 KiB x 2^n
    try testing.expect(check(0xC000) != null); // 48 KiB
    try testing.expect(check(0x80000) != null); // 512 KiB
    try testing.expect(check(20000) != null);
    try testing.expectEqual([2]u8{ 0x81, 0x03 }, word(.{ 0x01, 0x03 }, 0x40000)); // 16 banks, Codemasters kept
    try testing.expectEqual([2]u8{ 0x40, 0x0F }, word(.{ 0x00, 0x0F }, 0x20000)); // 8 banks
    try testing.expectEqual([2]u8{ 0x01, 0x07 }, word(.{ 0x01, 0x07 }, 0)); // none
}
