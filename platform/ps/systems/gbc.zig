//! The Game Boy Color: the menu's row and its cartridge check (side track K, D4: one file per system;
//! its track owns it; platform/ps/systems.zig lists them).

const std = @import("std");
const menu = @import("../menu.zig");
const savestate = menu.savestate;
const gb = @import("gb.zig");
const gbCheck = gb.gbCheck;
const gbSaveSize = gb.gbSaveSize;
const gbClock = gb.gbClock;
const testing = std.testing;

/// The Game Boy Color (side track I, phase 3; gbc.bit, Gb.TopCgb): the
/// Game Boy's map and loading (cartridge in DDR), Color-only cartridges
/// too, DMG ones in compatibility mode (their palette from the menu's
/// "Palette:" line, gbc_palette.zig), battery saves under /saves/gbc.
pub const system: menu.System = .{
    .id = 6, .name = "Game Boy Color", .dir = "gbc", .exts = &.{ ".gbc", ".gb" }, .load_addr = 0, .max_size = 8 << 20, .check = gbcCheck,
    .state = savestate.gb, // side track L, L.7: the Arty's bitstreams (WithStateHooks)
    .movies = true, // side track O, O.1: Gb.Machine's frame-synchronous input
    .screen = .{ .w = 160, .h = 144 }, // side track O, O.4: Gb.Machine's capture
    .volume = true, // side track L: Platform.Audio.Volume in its tops
    .speed_top = 4, // side track L, L.4: slow-down only; in double speed the core tops out at 1.125x
    .ddr_addr = 0x0200_0000, .palette = true,
    // battery saves as the Game Boy's (the same cartridge RAM map in
    // Gb.Machine), under /saves/gbc
    .save = .{
        .size = gbSaveSize, .ram_addr = 0x100000, .read_addr = 0x120000, .ctrl_addr = 0x140000, .clear_addr = 0x140001,
        .clock = gbClock, .clock_load_addr = 0x140010, .clock_read_addr = 0x140020,
    },
    .menu_combo = .{ .c = false, .d = true, .l = false, .r = false, .u = true }, .menu_combo_name = "Up+Down",
};

/// The Game Boy Color's check: 'gbCheck' without the Color-only rule.
pub fn gbcCheck(head: []const u8, size: u32) ?[]const u8 {
    if (size < 0x150 or head.len < 0x150) return "Not a Game Boy ROM (too short).";
    return switch (head[0x147]) {
        0x00, 0x01, 0x02, 0x03, 0x05, 0x06, 0x08, 0x09, 0x0F...0x13, 0x19...0x1E => null,
        else => "Cartridge type not supported (MBC).",
    };
}

test "gbcCheck takes Color-only cartridges, still checks the MBC" {
    var head = [_]u8{0} ** 0x150;
    head[0x143] = 0xC0;
    head[0x147] = 0x1B; // MBC5+RAM+BATTERY (µCity)
    try testing.expect(gbcCheck(&head, 0x8000) == null);
    try testing.expect(gbCheck(&head, 0x8000) != null);
    head[0x147] = 0x22; // MBC7
    try testing.expect(gbcCheck(&head, 0x8000) != null);
    try testing.expect(gbcCheck(head[0..0x100], 0x100) != null);
}
