//! CHIP-8: the menu's row (side track K, D4: one file per system;
//! its track owns it; platform/ps/systems.zig lists them).

const std = @import("std");
const menu = @import("../menu.zig");
const state_board = @import("state_board.zig");
const chip8_state = @import("chip8_state.zig");

pub const system: menu.System = .{ .id = 1, .name = "CHIP-8", .dir = "chip8", .exts = &.{ ".ch8", ".c8" }, .load_addr = 0x200, .max_size = 0x1000 - 0x200, .clear_rest = true,
    .state = state_board.spec(chip8_state.spec), // side track N, N.10: the Arty's bitstream (Chip8.StateHooks)
    .save = chip8_state.save, // no battery saves: the state's read-back control only
};
