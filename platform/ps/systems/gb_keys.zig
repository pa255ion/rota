//! The Game Boy's key map: its buttons and their default bindings
//! (side track K, D4: one file per system, its track's; keymap.zig
//! lists the systems, its sections, and reads the file format).

const keymap = @import("../keymap.zig");
const Button = keymap.Button;
const Binding = keymap.Binding;
const Map = keymap.Map;
const up = keymap.dpad_up;
const down = keymap.dpad_down;
const left = keymap.dpad_left;
const right = keymap.dpad_right;

pub const title = "Game Boy";

/// Game Boy (docs/systems/gameboy.md, "Input"): bit 0 Right, 1 Left, 2 Up,
/// 3 Down, 4 A, 5 B, 6 Select, 7 Start. The NES uses the same bits
/// (Nes.Top reorders them for $4016; docs/systems/nes.md, "Input").
pub const buttons = [_]Button{
    .{ .name = "up", .bit = 2 },    .{ .name = "down", .bit = 3 }, .{ .name = "left", .bit = 1 },  .{ .name = "right", .bit = 0 },
    .{ .name = "b", .bit = 5 },     .{ .name = "a", .bit = 4 },    .{ .name = "start", .bit = 7 }, .{ .name = "select", .bit = 6 },
};

pub fn defaults(m: *Map) void {
    const t = comptime [8]Binding{
        .of(&([_][]const u8{"KEY_W"} ++ up)),
        .of(&([_][]const u8{"KEY_S"} ++ down)),
        .of(&([_][]const u8{"KEY_A"} ++ left)),
        .of(&([_][]const u8{"KEY_D"} ++ right)),
        .of(&.{ "KEY_J", "BTN_SOUTH" }), // b
        .of(&.{ "KEY_K", "BTN_EAST" }), // a
        .of(&.{ "KEY_L", "BTN_START" }), // start
        .of(&.{ "KEY_LEFTSHIFT", "BTN_SELECT" }), // select
    };
    m.bind[0..8].* = t;
}
