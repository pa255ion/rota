//! The Super Nintendo: the menu's row (side track K, D4: one file per
//! system; its track owns it; platform/ps/systems.zig lists them).

const std = @import("std");
const menu = @import("../menu.zig");
const snes_cart = @import("snes_cart.zig");
const state_board = @import("state_board.zig");
const snes_state = @import("snes_state.zig");

/// The Super Nintendo (M7 step 7.11; snes.bit, Snes.Machine): the ROM
/// file into DDR as the Game Gear's, then the header's facts as a word
/// into the link map (snes_cart.zig: the mapping, the sizes, a copier's
/// header for the PL to skip), which the core takes when the hold falls.
/// SRAM saved as the Game Gear's cartridge RAM (Snes.Save). Up+Down as
/// the Game Gear (the Arty has no centre button).
pub const system: menu.System = .{
    .id = 9, .name = "Super Nintendo", .dir = "snes", .exts = &.{ ".sfc", ".smc" }, .load_addr = 0, .max_size = snes_cart.max_size, .check = snes_cart.check,
    .volume = true, // Platform.Audio.Volume in Snes.Machine
    .ddr_addr = 0x0200_0000, .ddr_info = .{ .addr = snes_cart.link_addr, .of = snes_cart.word },
    .check_ddr = snes_cart.checkDdr, // a chip cartridge refused (the header is past the first block)
    .keys_hi = 0x16_0000, // pad 2: the key word's bits 16-27 (Snes.Machine)
    .save = .{
        .size = snes_cart.saveSize, .ram_addr = snes_cart.save_ram_addr, .read_addr = snes_cart.save_read_addr,
        .ctrl_addr = snes_cart.save_ctrl_addr, .clear_addr = snes_cart.save_clear_addr,
        .size_of_rom = snes_cart.saveSizeOfRom, // the header's SRAM, with a battery
    },
    .state = state_board.spec(snes_state.spec), // step 7.14c: the Arty's bitstream (Snes.StateHooks)
    .menu_combo = .{ .c = false, .d = true, .l = false, .r = false, .u = true }, .menu_combo_name = "Up+Down",
};

test {
    _ = snes_state;
}

test "the row: ID 9, /snes, both file names" {
    try std.testing.expectEqual(@as(u8, 9), system.id);
    try std.testing.expectEqualStrings("snes", system.dir);
    try std.testing.expectEqual(@as(usize, 2), system.exts.len);
}
