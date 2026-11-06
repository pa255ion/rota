//! The Game Boy Color's colours for DMG cartridges (side track I, phase 3;
//! docs/systems/gameboy.md, "Palettes for DMG cartridges"). The CGB's boot
//! ROM gives about 90 of Nintendo's DMG games their own palette, found by
//! the title's checksum (and for 29 of them its 4th letter), and lets the
//! player pick one of 12 with a direction and A or B held at power-on. We
//! have no boot ROM: the PS app picks the palette before the game starts
//! and writes it into Gb.Loader's palette window (0x140030, while the core
//! is held; Gb.Core takes it at the hand-off). `Choice`: auto (the title's
//! palette, else our default), our default (the boot demo "rota"'s Game Boy
//! shades), or one of the 12 by its button combination.
//!
//! The tables are SameBoy's boot ROM's (BootROMs/cgb_boot.asm, Expat
//! licence, copied as data: TitleChecksums, Dups4thLetterArray,
//! PalettePerChecksum, PaletteCombinations, Palettes, KeyCombinationPalettes
//! 1 to 12), which reproduce the CGB boot ROM's results; generated from it
//! by hand once (the file is in the git-ignored local/reference/sameboy/).
const std = @import("std");
const testing = std.testing;

/// Where Gb.Loader takes the palette: BG palette 0, OBJ 0, OBJ 1, four
/// 15-bit colours each, little-endian (24 bytes).
pub const load_addr: u24 = 0x140030;
pub const bytes = 24;

/// Our default (the user, 2026-10-07): the boot demo's Game Boy shades,
/// lightest first, paper #F4ECD8, amber #F7A41D, teal #22A699, ink
/// #0E0B14, each channel the nearest 5-bit value (Gb.Core and
/// Gb.Spec.Machine have the same four).
pub const default_colours = [4]u16{ 0x6BBE, 0x129E, 0x4E84, 0x0822 };

/// The title checksums the boot ROM knows; from `first_dup` on, the 4th
/// letter of the title decides too (`dup_letters`).
const checksums = [_]u8{
    0x00, 0x88, 0x16, 0x36, 0xD1, 0xDB, 0xF2, 0x3C, 0x8C, 0x92, 0x3D, 0x5C, 0x58, 0xC9, 0x3E, 0x70,
    0x1D, 0x59, 0x69, 0x19, 0x35, 0xA8, 0x14, 0xAA, 0x75, 0x95, 0x99, 0x34, 0x6F, 0x15, 0xFF, 0x97,
    0x4B, 0x90, 0x17, 0x10, 0x39, 0xF7, 0xF6, 0xA2, 0x49, 0x4E, 0x43, 0x68, 0xE0, 0x8B, 0xF0, 0xCE,
    0x0C, 0x29, 0xE8, 0xB7, 0x86, 0x9A, 0x52, 0x01, 0x9D, 0x71, 0x9C, 0xBD, 0x5D, 0x6D, 0x67, 0x3F,
    0x6B, 0xB3, 0x46, 0x28, 0xA5, 0xC6, 0xD3, 0x27, 0x61, 0x18, 0x66, 0x6A, 0xBF, 0x0D, 0xF4, 0xB3,
    0x46, 0x28, 0xA5, 0xC6, 0xD3, 0x27, 0x61, 0x18, 0x66, 0x6A, 0xBF, 0x0D, 0xF4, 0xB3,
};
const first_dup = 65;
const dup_letters = "BEFAARBEKEK R-URAR INAILICE R";
/// Per checksum: the palette combination.
const per_checksum = [_]u8{
    0, 4, 5, 35, 34, 3, 31, 15, 10, 5, 19, 36, 7, 37, 30, 44, 21, 32, 31, 20, 5, 33, 13, 14,
    5, 29, 5, 18, 9, 3, 2, 26, 25, 25, 41, 42, 26, 45, 42, 45, 36, 38, 26, 42, 30, 41, 34, 34,
    5, 42, 6, 5, 33, 25, 42, 42, 40, 2, 16, 25, 42, 42, 5, 0, 39, 36, 22, 25, 6, 32, 12, 36,
    11, 39, 18, 39, 24, 31, 50, 17, 46, 6, 27, 0, 47, 41, 41, 0, 0, 19, 34, 23, 18, 29,
};
/// Combinations: the first colour of OBJ 0, OBJ 1 and BG in `colours`.
const combinations = [_][3]u8{
    .{ 16, 16, 116 }, .{ 72, 72, 72 }, .{ 80, 80, 80 }, .{ 96, 96, 96 }, .{ 36, 36, 36 }, .{ 0, 0, 0 },
    .{ 108, 108, 108 }, .{ 20, 20, 20 }, .{ 48, 48, 48 }, .{ 104, 104, 104 }, .{ 64, 32, 32 }, .{ 16, 112, 112 },
    .{ 16, 8, 8 }, .{ 12, 16, 16 }, .{ 16, 116, 116 }, .{ 112, 16, 112 }, .{ 8, 68, 8 }, .{ 64, 64, 32 },
    .{ 16, 16, 28 }, .{ 16, 16, 72 }, .{ 16, 16, 80 }, .{ 76, 76, 36 }, .{ 15, 15, 44 }, .{ 68, 68, 8 },
    .{ 16, 16, 8 }, .{ 16, 16, 12 }, .{ 112, 112, 0 }, .{ 12, 12, 0 }, .{ 0, 0, 4 }, .{ 72, 88, 72 },
    .{ 80, 88, 80 }, .{ 96, 88, 96 }, .{ 64, 88, 32 }, .{ 68, 16, 52 }, .{ 111, 0, 56 }, .{ 111, 16, 60 },
    .{ 76, 91, 36 }, .{ 64, 112, 40 }, .{ 16, 92, 112 }, .{ 68, 88, 8 }, .{ 16, 0, 8 }, .{ 16, 112, 12 },
    .{ 112, 12, 0 }, .{ 12, 112, 16 }, .{ 84, 112, 16 }, .{ 12, 112, 0 }, .{ 100, 12, 112 }, .{ 0, 112, 32 },
    .{ 16, 12, 112 }, .{ 112, 12, 24 }, .{ 16, 112, 116 }, .{ 120, 120, 120 }, .{ 124, 124, 124 }, .{ 112, 16, 4 },
    .{ 0, 0, 8 },
};
/// 32 palettes of four colours (the last two are SameBoy's own; only the
/// raw combinations reach into them, as on the CGB).
const colours = [_]u16{
    0x7FFF, 0x32BF, 0x00D0, 0x0000, 0x639F, 0x4279, 0x15B0, 0x04CB,
    0x7FFF, 0x6E31, 0x454A, 0x0000, 0x7FFF, 0x1BEF, 0x0200, 0x0000,
    0x7FFF, 0x421F, 0x1CF2, 0x0000, 0x7FFF, 0x5294, 0x294A, 0x0000,
    0x7FFF, 0x03FF, 0x012F, 0x0000, 0x7FFF, 0x03EF, 0x01D6, 0x0000,
    0x7FFF, 0x42B5, 0x3DC8, 0x0000, 0x7E74, 0x03FF, 0x0180, 0x0000,
    0x67FF, 0x77AC, 0x1A13, 0x2D6B, 0x7ED6, 0x4BFF, 0x2175, 0x0000,
    0x53FF, 0x4A5F, 0x7E52, 0x0000, 0x4FFF, 0x7ED2, 0x3A4C, 0x1CE0,
    0x03ED, 0x7FFF, 0x255F, 0x0000, 0x036A, 0x021F, 0x03FF, 0x7FFF,
    0x7FFF, 0x01DF, 0x0112, 0x0000, 0x231F, 0x035F, 0x00F2, 0x0009,
    0x7FFF, 0x03EA, 0x011F, 0x0000, 0x299F, 0x001A, 0x000C, 0x0000,
    0x7FFF, 0x027F, 0x001F, 0x0000, 0x7FFF, 0x03E0, 0x0206, 0x0120,
    0x7FFF, 0x7EEB, 0x001F, 0x7C00, 0x7FFF, 0x3FFF, 0x7E00, 0x001F,
    0x7FFF, 0x03FF, 0x001F, 0x0000, 0x03FF, 0x001F, 0x000C, 0x0000,
    0x7FFF, 0x033F, 0x0193, 0x0000, 0x0000, 0x4200, 0x037F, 0x7FFF,
    0x7FFF, 0x7E8C, 0x7C00, 0x0000, 0x7FFF, 0x1BEF, 0x6180, 0x0000,
    0x7FFF, 0x7FEA, 0x7D5F, 0x0000, 0x4778, 0x3290, 0x1D87, 0x0861,
};
/// The 12 button combinations, as the boot ROM lists them, and their
/// combination.
pub const manual = [_]struct { name: []const u8, combo: u8 }{
    .{ .name = "Right", .combo = 1 },
    .{ .name = "Left", .combo = 48 },
    .{ .name = "Up", .combo = 5 },
    .{ .name = "Down", .combo = 8 },
    .{ .name = "Right + A", .combo = 0 },
    .{ .name = "Left + A", .combo = 40 },
    .{ .name = "Up + A", .combo = 43 },
    .{ .name = "Down + A", .combo = 3 },
    .{ .name = "Right + B", .combo = 6 },
    .{ .name = "Left + B", .combo = 7 },
    .{ .name = "Up + B", .combo = 28 },
    .{ .name = "Down + B", .combo = 49 },
};

/// The choices, in the menu's order: 0 auto, 1 our default, 2 + i
/// `manual[i]`.
pub const Choice = u8;
pub const choices: Choice = 2 + manual.len;

/// The menu's label for a choice.
pub fn label(c: Choice) []const u8 {
    return switch (c) {
        0 => "Auto",
        1 => "Default",
        else => manual[c - 2].name,
    };
}

/// The boot ROM's combination for a cartridge header, or null: not a
/// Nintendo game (old licensee 0x01, or 0x33 with new licensee "01"), or a
/// title it doesn't know (combination 0, its own default, counts as not
/// known: our default then).
pub fn titleCombination(head: []const u8) ?u8 {
    if (head.len < 0x150) return null;
    const old = head[0x14B];
    const nintendo = old == 0x01 or (old == 0x33 and head[0x144] == '0' and head[0x145] == '1');
    if (!nintendo) return null;
    var sum: u8 = 0;
    for (head[0x134..0x144]) |b| sum +%= b;
    for (checksums, 0..) |c, i| {
        if (c != sum) continue;
        if (i >= first_dup and head[0x137] != dup_letters[i - first_dup]) continue;
        const k = per_checksum[i];
        return if (k == 0) null else k;
    }
    return null;
}

/// The 24 bytes for `load_addr`: the palettes for this cartridge and choice.
pub fn paletteBytes(head: []const u8, c: Choice) [bytes]u8 {
    var out: [bytes]u8 = undefined;
    const combo: ?u8 = switch (c) {
        0 => titleCombination(head),
        1 => null,
        else => manual[c - 2].combo,
    };
    // BG, OBJ 0, OBJ 1 (the combination lists OBJ 0, OBJ 1, BG)
    for (0..3) |p| {
        for (0..4) |k| {
            const v: u16 = if (combo) |i| colours[combinations[i][(p + 2) % 3] + k] else default_colours[k];
            out[p * 8 + k * 2] = @truncate(v);
            out[p * 8 + k * 2 + 1] = @truncate(v >> 8);
        }
    }
    return out;
}

fn header(title: []const u8, old: u8, new: [2]u8) [0x150]u8 {
    var h = [_]u8{0} ** 0x150;
    @memcpy(h[0x134..][0..title.len], title);
    h[0x14B] = old;
    h[0x144] = new[0];
    h[0x145] = new[1];
    return h;
}

test "Some Game gets the boot ROM's palette (checksum 0x61, 4th letter E)" {
    const h = header("POKEMON BLUE", 0x33, "01".*);
    try testing.expectEqual(@as(?u8, 11), titleCombination(&h));
    const b = paletteBytes(&h, 0);
    // BG = palette 28 (#7FFF, #7E8C, #7C00, #0000), OBJ 0 = palette 4
    try testing.expectEqualSlices(u8, &.{ 0xFF, 0x7F, 0x8C, 0x7E, 0x00, 0x7C, 0x00, 0x00 }, b[0..8]);
    try testing.expectEqualSlices(u8, &.{ 0xFF, 0x7F, 0x1F, 0x42, 0xF2, 0x1C, 0x00, 0x00 }, b[8..16]);
    try testing.expectEqualSlices(u8, b[0..8], b[16..24]);
}

test "the 4th letter tells apart titles with the same checksum" {
    // SUPER MARIOLAND: 0x46 with 'E'; the same sum with another 4th letter
    // is a title the boot ROM doesn't know
    var h = header("SUPER MARIOLAND", 0x01, .{ 0, 0 });
    try testing.expectEqual(@as(?u8, 22), titleCombination(&h));
    h[0x137] = 'X';
    h[0x138] -%= 'X' -% 'E'; // keep the sum
    try testing.expect(titleCombination(&h) == null);
}

test "not Nintendo's, or unknown: our default; manual choices" {
    const h = header("MEGA MAN 2", 0x08, .{ 0, 0 });
    try testing.expect(titleCombination(&h) == null);
    const b = paletteBytes(&h, 0);
    try testing.expectEqualSlices(u8, &.{ 0xBE, 0x6B, 0x9E, 0x12, 0x84, 0x4E, 0x22, 0x08 }, b[0..8]);
    try testing.expectEqualSlices(u8, &paletteBytes(&h, 1), &b);
    // Left + B (manual 10, combination 7): palette 5 everywhere
    try testing.expectEqualStrings("Left + B", label(2 + 9));
    const lb = paletteBytes(&h, 2 + 9);
    try testing.expectEqualSlices(u8, &.{ 0xFF, 0x7F, 0x94, 0x52, 0x4A, 0x29, 0x00, 0x00 }, lb[0..8]);
    try testing.expectEqual(@as(usize, 14), choices);
}
