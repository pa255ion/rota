//! The CHIP-8's key map: its buttons and their default bindings
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

pub const title = "CHIP-8";

/// CHIP-8: keys 0 to F, bit = key. Listed in keypad order (1 2 3 C / 4 5 6
/// D / 7 8 9 E / A 0 B F).
pub const buttons = [_]Button{
    .{ .name = "1", .bit = 0x1 }, .{ .name = "2", .bit = 0x2 }, .{ .name = "3", .bit = 0x3 }, .{ .name = "c", .bit = 0xC },
    .{ .name = "4", .bit = 0x4 }, .{ .name = "5", .bit = 0x5 }, .{ .name = "6", .bit = 0x6 }, .{ .name = "d", .bit = 0xD },
    .{ .name = "7", .bit = 0x7 }, .{ .name = "8", .bit = 0x8 }, .{ .name = "9", .bit = 0x9 }, .{ .name = "e", .bit = 0xE },
    .{ .name = "a", .bit = 0xA }, .{ .name = "0", .bit = 0x0 }, .{ .name = "b", .bit = 0xB }, .{ .name = "f", .bit = 0xF },
};

pub fn defaults(m: *Map) void {
    const t = comptime [16]Binding{
        .of(&.{ "KEY_1", "BTN_NORTH" }), // 1
        .of(&([_][]const u8{ "KEY_2", "KEY_UP" } ++ up)), // 2
        .of(&.{"KEY_3"}), // 3
        .of(&.{ "KEY_4", "BTN_WEST" }), // C
        .of(&([_][]const u8{ "KEY_Q", "KEY_LEFT" } ++ left)), // 4
        .of(&.{ "KEY_W", "KEY_SPACE", "BTN_SOUTH" }), // 5
        .of(&([_][]const u8{ "KEY_E", "KEY_RIGHT" } ++ right)), // 6
        .of(&.{"KEY_R"}), // D
        .of(&.{"KEY_A"}), // 7
        .of(&([_][]const u8{ "KEY_S", "KEY_DOWN" } ++ down)), // 8
        .of(&.{"KEY_D"}), // 9
        .of(&.{ "KEY_F", "BTN_TL" }), // E
        .of(&.{ "KEY_Z", "BTN_START" }), // A
        .of(&.{ "KEY_X", "BTN_SELECT" }), // 0
        .of(&.{ "KEY_C", "BTN_EAST" }), // B
        .of(&.{ "KEY_V", "BTN_TR" }), // F
    };
    m.bind[0..16].* = t;
}
