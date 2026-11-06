//! The Master System's key map: its buttons and their default bindings
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

pub const title = "Master System";

/// Master System (M6 step 6.7; Sms.Machine.smsPsKeys): pad 1 as the Game
/// Gear's bits (0 right, 1 left, 2 up, 3 down, 4 button 2, 5 button 1),
/// 7 Pause; pad 2 the same in bits 8-13. (The SMS 2 has no Reset button.)
pub const buttons = [_]Button{
    .{ .name = "up", .bit = 2 },    .{ .name = "down", .bit = 3 },  .{ .name = "left", .bit = 1 },  .{ .name = "right", .bit = 0 },
    .{ .name = "1", .bit = 5 },     .{ .name = "2", .bit = 4 },     .{ .name = "pause", .bit = 7 },
    .{ .name = "up2", .bit = 10 },  .{ .name = "down2", .bit = 11 }, .{ .name = "left2", .bit = 9 }, .{ .name = "right2", .bit = 8 },
    .{ .name = "1_2", .bit = 13 },  .{ .name = "2_2", .bit = 12 },
};

/// pad 1 as the Game Gear's (L = Pause); pad 2 on the arrow keys,
/// ',' = 1 and '.' = 2 (the user, 2026-10-11)
pub fn defaults(m: *Map) void {
    const t = comptime [13]Binding{
        .of(&([_][]const u8{"KEY_W"} ++ up)),
        .of(&([_][]const u8{"KEY_S"} ++ down)),
        .of(&([_][]const u8{"KEY_A"} ++ left)),
        .of(&([_][]const u8{"KEY_D"} ++ right)),
        .of(&.{ "KEY_J", "BTN_SOUTH" }), // 1
        .of(&.{ "KEY_K", "BTN_EAST" }), // 2
        .of(&.{ "KEY_L", "BTN_START" }), // pause
        .of(&.{"KEY_UP"}),
        .of(&.{"KEY_DOWN"}),
        .of(&.{"KEY_LEFT"}),
        .of(&.{"KEY_RIGHT"}),
        .of(&.{"KEY_COMMA"}), // pad 2's 1
        .of(&.{"KEY_DOT"}), // pad 2's 2
    };
    m.bind[0..13].* = t;
}
