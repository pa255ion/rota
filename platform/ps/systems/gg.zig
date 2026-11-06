//! The Sega Game Gear: the menu's row (side track K, D4: one file per system;
//! its track owns it; platform/ps/systems.zig lists them).

const std = @import("std");
const menu = @import("../menu.zig");
const state_board = @import("state_board.zig");
const gg_cart = @import("gg_cart.zig");
const gg_state = @import("gg_state.zig");

/// The Sega Game Gear (M5 step 5.9; gg.bit, Gg.Machine): the ROM into
/// DDR as the Game Boy's, then its mapper and size as a word into the
/// link map (gg_cart.zig), which the core takes when the hold falls.
/// Up+Down as the Game Boy.
pub const system: menu.System = .{
    .id = 7, .name = "Game Gear", .dir = "gg", .exts = &.{".gg"}, .load_addr = 0, .max_size = gg_cart.max_size, .check = gg_cart.check,
    .volume = true, // side track L: Platform.Audio.Volume in its tops
    .speed_top = 4, // side track L, L.4: slow-down only; the core tops out at 1.17x
    .ddr_addr = 0x0200_0000, .ddr_info = .{ .addr = gg_cart.link_addr, .of = gg_cart.word },
    .state = state_board.spec(gg_state.spec), // side track N, N.3: the Arty's bitstreams (WithStateHooks)
    .save = .{
        .size = gg_cart.saveSize, .ram_addr = gg_cart.save_ram_addr, .read_addr = gg_cart.save_read_addr,
        .ctrl_addr = gg_cart.save_ctrl_addr, .clear_addr = gg_cart.save_clear_addr,
    },
    .menu_combo = .{ .c = false, .d = true, .l = false, .r = false, .u = true }, .menu_combo_name = "Up+Down",
};
