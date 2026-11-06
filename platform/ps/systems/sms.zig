//! The Sega Master System: the menu's row (side track K, D4: one file per system;
//! its track owns it; platform/ps/systems.zig lists them).

const std = @import("std");
const menu = @import("../menu.zig");
const state_board = @import("state_board.zig");
const gg_cart = @import("gg_cart.zig");
const gg_state = @import("gg_state.zig");
const sms_cart = @import("sms_cart.zig");

/// The Sega Master System (M6 step 6.7; sms.bit, Sms.Machine): as the
/// Game Gear's (the same cartridge word, gg_cart.zig; the same link map
/// for saves), and a BIOS from /sms/bios.bin if the card has one (the
/// user's own dump; sms_cart.zig: into DDR at 0x02100000, its size in
/// the word's bits 7-3). Up+Down as the Game Gear.
pub const system: menu.System = .{
    .id = 8, .name = "Master System", .dir = "sms", .exts = &.{".sms"}, .load_addr = 0, .max_size = gg_cart.max_size, .check = gg_cart.check,
    .volume = true, // M6, as side track L's Game Gear: Platform.Audio.Volume in Sms.Machine
    .speed_top = 4, // slow-down only, as the Game Gear (the same core and ROM port)
    .ddr_addr = 0x0200_0000, .ddr_info = .{ .addr = gg_cart.link_addr, .of = gg_cart.word }, .companion = sms_cart.bios,
    .state = state_board.spec(gg_state.spec), // side track N, N.3: the Arty's bitstreams (WithStateHooks)
    .save = .{
        .size = gg_cart.saveSize, .ram_addr = gg_cart.save_ram_addr, .read_addr = gg_cart.save_read_addr,
        .ctrl_addr = gg_cart.save_ctrl_addr, .clear_addr = gg_cart.save_clear_addr,
    },
    .menu_combo = .{ .c = false, .d = true, .l = false, .r = false, .u = true }, .menu_combo_name = "Up+Down",
};
