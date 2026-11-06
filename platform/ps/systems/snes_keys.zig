//! The Super Nintendo's key map: its buttons and their default bindings
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

pub const title = "Super Nintendo";

/// Super Nintendo (M7 step 7.11; Snes.Machine): pad 1 in the PS link's
/// user bits as the pad's own order, 11 B, 10 Y, 9 Select, 8 Start,
/// 7 up, 6 down, 5 left, 4 right, 3 A, 2 X, 1 L, 0 R; pad 2 the same
/// order in the key word's bits 16-27 (the relay writes them to the link
/// map's pad 2 bytes: menu.System.keys_hi).
pub const buttons = [_]Button{
    .{ .name = "up", .bit = 7 },     .{ .name = "down", .bit = 6 },  .{ .name = "left", .bit = 5 },  .{ .name = "right", .bit = 4 },
    .{ .name = "b", .bit = 11 },     .{ .name = "a", .bit = 3 },     .{ .name = "y", .bit = 10 },    .{ .name = "x", .bit = 2 },
    .{ .name = "l", .bit = 1 },      .{ .name = "r", .bit = 0 },     .{ .name = "start", .bit = 8 }, .{ .name = "select", .bit = 9 },
    .{ .name = "up2", .bit = 23 },   .{ .name = "down2", .bit = 22 }, .{ .name = "left2", .bit = 21 }, .{ .name = "right2", .bit = 20 },
    .{ .name = "b2", .bit = 27 },    .{ .name = "a2", .bit = 19 },   .{ .name = "y2", .bit = 26 },   .{ .name = "x2", .bit = 18 },
    .{ .name = "l2", .bit = 17 },    .{ .name = "r2", .bit = 16 },   .{ .name = "start2", .bit = 24 }, .{ .name = "select2", .bit = 25 },
};

/// The Game Boy's keys where the two share buttons (W A S D, J = B,
/// K = A, L = Start, Left Shift = Select), and U = Y, I = X, Q = L,
/// E = R. Gamepad: the buttons where the SNES pad has them (Cross = B,
/// Circle = A, Square = Y, Triangle = X), the shoulders L and R. Pad 2 on
/// the keyboard as the Master System's pad 2 (the user, 2026-10-11): the
/// arrow keys, ',' = B, '.' = A; the rest on the keypad (4 = Y, 5 = X,
/// 7 = L, 9 = R, Enter = Start, + = Select).
pub fn defaults(m: *Map) void {
    const t = comptime [24]Binding{
        .of(&([_][]const u8{"KEY_W"} ++ up)),
        .of(&([_][]const u8{"KEY_S"} ++ down)),
        .of(&([_][]const u8{"KEY_A"} ++ left)),
        .of(&([_][]const u8{"KEY_D"} ++ right)),
        .of(&.{ "KEY_J", "BTN_SOUTH" }), // b
        .of(&.{ "KEY_K", "BTN_EAST" }), // a
        .of(&.{ "KEY_U", "BTN_WEST" }), // y
        .of(&.{ "KEY_I", "BTN_NORTH" }), // x
        .of(&.{ "KEY_Q", "BTN_TL" }), // l
        .of(&.{ "KEY_E", "BTN_TR" }), // r
        .of(&.{ "KEY_L", "BTN_START" }), // start
        .of(&.{ "KEY_LEFTSHIFT", "BTN_SELECT" }), // select
        .of(&.{"KEY_UP"}),
        .of(&.{"KEY_DOWN"}),
        .of(&.{"KEY_LEFT"}),
        .of(&.{"KEY_RIGHT"}),
        .of(&.{"KEY_COMMA"}), // pad 2's b
        .of(&.{"KEY_DOT"}), // a
        .of(&.{"KEY_KP4"}), // y
        .of(&.{"KEY_KP5"}), // x
        .of(&.{"KEY_KP7"}), // l
        .of(&.{"KEY_KP9"}), // r
        .of(&.{"KEY_KPENTER"}), // start
        .of(&.{"KEY_KPPLUS"}), // select
    };
    m.bind[0..24].* = t;
}

test "both pads: 24 buttons, each its own bit, pad 2's at 16-27" {
    const std = @import("std");
    try std.testing.expectEqual(@as(usize, 24), buttons.len);
    var seen: u32 = 0;
    for (buttons, 0..) |b, i| {
        const bit = @as(u32, 1) << b.bit;
        try std.testing.expect(seen & bit == 0);
        seen |= bit;
        if (i >= 12) try std.testing.expect(b.bit >= 16 and b.bit <= 27);
    }
    try std.testing.expectEqual(@as(u32, 0x0FFF_0FFF), seen);
}
