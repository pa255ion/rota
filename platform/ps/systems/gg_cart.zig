//! The Game Gear's cartridge (M5 step 5.9, docs/systems/gamegear.md, "SD
//! menu"): the ROM goes into DDR (menu.zig's row, `ddr_addr`), and the PL
//! needs two facts the ROM doesn't reliably state, its mapper and its size
//! in 16 KiB banks. They go as one 16-bit word into the Game Gear's link
//! map while the core is held (Gg.Machine, `cartAddr`): bit 0 the mapper
//! (1 = Codemasters), bits 15-8 the banks - 1 (a power of two), low byte
//! first. The same word as `gg-image`'s build/gg/cart.bin, and the mapper
//! rule is Gg.Spec.detectMapper's.

const std = @import("std");

/// Where the word goes in the Game Gear's link map (Gg.Machine.cartAddr).
pub const link_addr: u24 = 0x000000;

/// The largest ROM: 64 banks of 16 KiB (the mapper's 6 bank bits).
pub const max_size: u32 = 1 << 20;

/// A look at the file before loading (menu.zig `check`).
pub fn check(head: []const u8, size: u32) ?[]const u8 {
    _ = head;
    if (size < 0x400) return "too small for a Game Gear ROM";
    if (size > max_size) return "more than 1 MiB";
    return null;
}

/// The banks: the ROM's 16 KiB blocks rounded up to a power of two.
pub fn banks(size: usize) u32 {
    const blocks: u32 = @intCast(@max(1, (size + 0x3FFF) / 0x4000));
    return std.math.ceilPowerOfTwoAssert(u32, blocks);
}

/// Codemasters' mapper: a valid Codemasters header (its checksum and
/// inverse at 7FE6h/7FE8h, zeros at 7FEAh-7FEFh), or code in the first
/// 32 KiB writing the mapper's 8000h register (LD (8000h),A) more than
/// Sega's FFFFh.
pub fn codemasters(rom: []const u8) bool {
    const byteAt = struct {
        fn at(r: []const u8, i: usize) u8 {
            return if (i < r.len) r[i] else 0;
        }
    }.at;
    const wordAt = struct {
        fn at(r: []const u8, i: usize) u32 {
            return @as(u32, byteAt(r, i)) | @as(u32, byteAt(r, i + 1)) << 8;
        }
    }.at;
    const sum = wordAt(rom, 0x7FE6);
    var zeros = true;
    for (0x7FEA..0x7FF0) |i| zeros = zeros and byteAt(rom, i) == 0;
    const header = rom.len >= 0x8000 and sum != 0 and (sum + wordAt(rom, 0x7FE8)) % 0x10000 == 0 and zeros;
    const first = rom[0..@min(rom.len, 0x8000)];
    const n8000 = std.mem.count(u8, first, &.{ 0x32, 0x00, 0x80 });
    const nffff = std.mem.count(u8, first, &.{ 0x32, 0xFF, 0xFF });
    const writes = n8000 > nffff + 2 or (n8000 > 0 and nffff == 0);
    return header or writes;
}

/// Battery saves (M5 step 5.10; Gg.Save, the Game Boy's protocol): the
/// link addresses of cartridge RAM (loaded while held), its read window,
/// the save control and the dirty flag's clear.
pub const save_ram_addr: u24 = 0x100000;
pub const save_read_addr: u24 = 0x120000;
pub const save_ctrl_addr: u24 = 0x140000;
pub const save_clear_addr: u24 = 0x140001;

/// A save file's size: the cartridge RAM's 32 KiB for every ROM. A Sega
/// header has no field for cartridge RAM (and lies past the first 512
/// bytes anyway), so each ROM gets a slot; a game that never writes
/// cartridge RAM never sets the dirty flag, so its slot is never written.
pub fn saveSize(head: []const u8) u32 {
    _ = head;
    return 0x8000;
}

/// The word's two bytes, low first, for the whole ROM as loaded.
pub fn word(rom: []const u8) [2]u8 {
    return .{ @intFromBool(codemasters(rom)), @intCast(banks(rom.len) - 1) };
}

test "banks: 16 KiB blocks rounded up to a power of two" {
    try std.testing.expectEqual(@as(u32, 1), banks(0x400));
    try std.testing.expectEqual(@as(u32, 2), banks(0x8000));
    try std.testing.expectEqual(@as(u32, 4), banks(0x10000));
    try std.testing.expectEqual(@as(u32, 8), banks(0x14000));
    try std.testing.expectEqual(@as(u32, 64), banks(1 << 20));
}

test "the word: Sega mapper, 64 KiB (as gg-image's cart.bin for our ROMs)" {
    var rom = [_]u8{0} ** 0x10000;
    rom[0x100] = 0x32; // LD (FFFFh),A
    rom[0x101] = 0xFF;
    rom[0x102] = 0xFF;
    try std.testing.expectEqual([2]u8{ 0, 3 }, word(&rom));
}

test "the word: Codemasters by its header, and by its register writes" {
    var rom = [_]u8{0} ** 0x20000;
    rom[0x7FE6] = 0x34;
    rom[0x7FE7] = 0x12;
    rom[0x7FE8] = 0xCC; // 10000h - 1234h = EDCCh
    rom[0x7FE9] = 0xED;
    try std.testing.expectEqual([2]u8{ 1, 7 }, word(&rom));
    var rom2 = [_]u8{0} ** 0x8000;
    rom2[0x200] = 0x32; // LD (8000h),A
    rom2[0x201] = 0x00;
    rom2[0x202] = 0x80;
    try std.testing.expectEqual([2]u8{ 1, 1 }, word(&rom2));
}

test "saves: every ROM gets the 32 KiB of cartridge RAM" {
    try std.testing.expectEqual(@as(u32, 0x8000), saveSize(&.{}));
}

test "check: sizes" {
    try std.testing.expect(check(&.{}, 0x100) != null);
    try std.testing.expect(check(&.{}, 0x8000) == null);
    try std.testing.expect(check(&.{}, (1 << 20) + 1) != null);
}
