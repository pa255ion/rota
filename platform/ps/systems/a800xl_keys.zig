//! The Atari 800XL's key map: its buttons and their default bindings
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

pub const title = "Atari 800XL";

/// Atari 800XL (M4 step 4.8): joystick 1, the bits A800.Loader takes in
/// its joystick byte (0 up, 1 down, 2 left, 3 right, 4 trigger). The
/// keyboard itself is a fixed layout (a800_keys.zig), not a map.
pub const buttons = [_]Button{
    .{ .name = "up", .bit = 0 },   .{ .name = "down", .bit = 1 }, .{ .name = "left", .bit = 2 },
    .{ .name = "right", .bit = 3 }, .{ .name = "trigger", .bit = 4 },
};

/// the keypad (the letters and arrows type on the 800XL), the
/// gamepad's D-pad, left stick and Cross
pub fn defaults(m: *Map) void {
    const t = comptime [5]Binding{
        .of(&([_][]const u8{"KEY_KP8"} ++ up)),
        .of(&([_][]const u8{ "KEY_KP2", "KEY_KP5" } ++ down)),
        .of(&([_][]const u8{"KEY_KP4"} ++ left)),
        .of(&([_][]const u8{"KEY_KP6"} ++ right)),
        .of(&.{ "KEY_KP0", "KEY_RIGHTALT", "BTN_SOUTH" }), // trigger
    };
    m.bind[0..5].* = t;
}
