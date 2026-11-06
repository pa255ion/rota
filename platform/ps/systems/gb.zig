//! The Game Boy: the menu's row, its cartridge check and save sizes (side track K, D4: one file per system;
//! its track owns it; platform/ps/systems.zig lists them).

const std = @import("std");
const menu = @import("../menu.zig");
const clock_footer = menu.clock_footer;
const savestate = menu.savestate;
const testing = std.testing;

/// Game Boy cartridges: ROM from 0 in Gb.Top's map (Gb.Loader), the
/// 256 KiB block RAM (bigger ones need the DDR path, side track C)
pub const system: menu.System = .{
    .id = 2, .name = "Game Boy", .dir = "gb", .exts = &.{ ".gb", ".gbc" }, .load_addr = 0, .max_size = 8 << 20, .check = gbCheck,
    .state = savestate.gb, // side track L, L.7: the Arty's bitstreams (WithStateHooks)
    .movies = true, // side track O, O.1: Gb.Machine's frame-synchronous input
    .screen = .{ .w = 160, .h = 144 }, // side track O, O.4: Gb.Machine's capture
    .volume = true, // side track L: Platform.Audio.Volume in its tops
    .speed_top = 5, // side track L, L.4: 125 %, measured: the core tops out at 1.44x
    .ddr_addr = 0x0200_0000,
    .menu_combo = .{ .c = false, .d = true, .l = false, .r = false, .u = true }, .menu_combo_name = "Up+Down",
    // cartridge RAM in Gb.Loader's map (side track I)
    .save = .{
        .size = gbSaveSize, .ram_addr = 0x100000, .read_addr = 0x120000, .ctrl_addr = 0x140000, .clear_addr = 0x140001,
        .clock = gbClock, .clock_load_addr = 0x140010, .clock_read_addr = 0x140020,
    },
};

/// The cartridge header (Pan Docs, "The Cartridge Header"): what
/// Gb.Core.cartFromHeader supports (no MBC, MBC1, MBC2, MBC3, MBC5), and
/// no Game Boy Color-only cartridges (0x143 = 0xC0). The size limit (8
/// MiB, MBC5's largest) is the table's `max_size`.
pub fn gbCheck(head: []const u8, size: u32) ?[]const u8 {
    if (size < 0x150 or head.len < 0x150) return "Not a Game Boy ROM (too short).";
    if (head[0x143] == 0xC0) return "Game Boy Color only.";
    return switch (head[0x147]) {
        0x00, 0x01, 0x02, 0x03, 0x05, 0x06, 0x08, 0x09, 0x0F...0x13, 0x19...0x1E => null,
        else => "Cartridge type not supported (MBC).",
    };
}

/// The save file of a Game Boy cartridge (side track I): its RAM for the
/// battery types the core runs (0x03 MBC1, 0x09 no MBC, 0x10 and 0x13
/// MBC3, 0x1B and 0x1E MBC5, each +RAM+BATTERY: header byte 0x149, 2 = 8
/// KiB, 3 = 32 KiB, 4 = 128 KiB, 5 = 64 KiB; 0x06 MBC2+BATTERY: its 512
/// half-bytes, one per byte), plus the clock's footer for MBC3 with the
/// clock (0x0F without RAM, 0x10); 0 for the rest.
pub fn gbSaveSize(head: []const u8) u32 {
    if (head.len < 0x150) return 0;
    const ram: u32 = switch (head[0x147]) {
        0x06 => 512,
        0x0F => 0,
        0x03, 0x09, 0x10, 0x13, 0x1B, 0x1E => switch (head[0x149]) {
            2 => 0x2000,
            3 => 0x8000,
            4 => 0x20000,
            5 => 0x10000,
            else => 0,
        },
        else => return 0,
    };
    const total = ram + if (gbClock(head)) @as(u32, clock_footer) else 0;
    return total;
}

/// MBC3 with its clock: TIMER+BATTERY (0x0F, 0x10).
pub fn gbClock(head: []const u8) bool {
    return head.len >= 0x150 and (head[0x147] == 0x0F or head[0x147] == 0x10);
}

test "Game Boy header check: supported MBCs, no CGB-only, not too short" {
    var head = [_]u8{0} ** 512;
    for ([_]u8{ 0x00, 0x01, 0x03, 0x08, 0x19, 0x1B, 0x1E }) |t| {
        head[0x147] = t;
        try testing.expect(gbCheck(&head, 0x8000) == null);
    }
    for ([_]u8{ 0x05, 0x06, 0x0F, 0x10, 0x13 }) |t| { // MBC2, MBC3 (phase 2)
        head[0x147] = t;
        try testing.expect(gbCheck(&head, 0x8000) == null);
    }
    for ([_]u8{ 0x0B, 0x1F, 0x20, 0xFC, 0xFF }) |t| { // MMM01, camera, MBC6, ...
        head[0x147] = t;
        try testing.expect(gbCheck(&head, 0x8000) != null);
    }
    head[0x147] = 0x01;
    head[0x143] = 0x80; // works on both: fine
    try testing.expect(gbCheck(&head, 0x8000) == null);
    head[0x143] = 0xC0;
    try testing.expectEqualStrings("Game Boy Color only.", gbCheck(&head, 0x8000).?);
    head[0x143] = 0;
    try testing.expect(gbCheck(head[0..0x100], 0x100) != null);
}

test "Game Boy save sizes: battery types the core runs, their RAM, MBC2's 512, MBC3's clock footer" {
    var head = [_]u8{0} ** 0x150;
    head[0x147] = 0x03;
    head[0x149] = 2;
    try testing.expectEqual(@as(u32, 0x2000), gbSaveSize(&head));
    head[0x149] = 3;
    try testing.expectEqual(@as(u32, 0x8000), gbSaveSize(&head));
    for ([_]u8{ 0x09, 0x1B, 0x1E }) |t| {
        head[0x147] = t;
        try testing.expectEqual(@as(u32, 0x8000), gbSaveSize(&head));
    }
    // RAM but no battery; no RAM
    for ([_]u8{ 0x02, 0x08, 0x1A, 0x12, 0x05, 0x01, 0x11 }) |t| {
        head[0x147] = t;
        try testing.expectEqual(@as(u32, 0), gbSaveSize(&head));
    }
    head[0x147] = 0x03;
    for ([_]u8{ 0, 1 }) |r| {
        head[0x149] = r;
        try testing.expectEqual(@as(u32, 0), gbSaveSize(&head));
    }
    head[0x149] = 4;
    try testing.expectEqual(@as(u32, 0x20000), gbSaveSize(&head));
    head[0x149] = 5;
    try testing.expectEqual(@as(u32, 0x10000), gbSaveSize(&head));
    // MBC2: its own RAM whatever 0x149 says; MBC3 with the clock: + 48
    head[0x147] = 0x06;
    try testing.expectEqual(@as(u32, 512), gbSaveSize(&head));
    head[0x147] = 0x10;
    head[0x149] = 3;
    try testing.expectEqual(@as(u32, 0x8000 + 48), gbSaveSize(&head));
    try testing.expect(gbClock(&head));
    head[0x147] = 0x0F;
    try testing.expectEqual(@as(u32, 48), gbSaveSize(&head));
    head[0x147] = 0x13;
    try testing.expectEqual(@as(u32, 0x8000), gbSaveSize(&head));
    try testing.expect(!gbClock(&head));
    try testing.expectEqual(@as(u32, 0), gbSaveSize(head[0..0x100]));
}
