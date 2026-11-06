//! The systems the menu knows (side track K, D4), one file each in
//! platform/ps/systems/ (its row, its file check and save hook: its
//! track's file). A new system adds its file and one line here, in the
//! order of its ID (the menu's order); IDs are reserved in
//! docs/ARCHITECTURE.md first.

const menu = @import("menu.zig");

pub const all = [_]menu.System{
    @import("systems/chip8.zig").system,
    @import("systems/gb.zig").system,
    @import("systems/nes.zig").system,
    @import("systems/a800xl.zig").system,
    @import("systems/gbc.zig").system,
    @import("systems/gg.zig").system,
    @import("systems/sms.zig").system,
    @import("systems/snes.zig").system,
};

test {
    _ = @import("systems/chip8.zig");
    _ = @import("systems/gb.zig");
    _ = @import("systems/nes.zig");
    _ = @import("systems/a800xl.zig");
    _ = @import("systems/gbc.zig");
    _ = @import("systems/gg.zig");
    _ = @import("systems/sms.zig");
    _ = @import("systems/gg_cart.zig");
    _ = @import("systems/sms_cart.zig");
    _ = @import("systems/snes.zig");
    _ = @import("systems/snes_cart.zig");
}
