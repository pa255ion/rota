//! The Game Gear's key map: its buttons and their default bindings
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

pub const title = "Game Gear";

/// Game Gear (M5 step 5.9): the Game Boy's bits, so the PL decodes it as
/// the Game Boy's keys (Gg.ArtyTop, Gg.Top): 0 right, 1 left, 2 up, 3 down,
/// 4 button 2 (the Game Boy's A), 5 button 1 (B), 7 Start; no Select.
pub const buttons = [_]Button{
    .{ .name = "up", .bit = 2 }, .{ .name = "down", .bit = 3 }, .{ .name = "left", .bit = 1 }, .{ .name = "right", .bit = 0 },
    .{ .name = "1", .bit = 5 },  .{ .name = "2", .bit = 4 },    .{ .name = "start", .bit = 7 },
};

/// the Game Boy's keys (the user, 2026-10-07: "the same layout as
/// the GB"): J = button 1 (the Game Boy's B), K = button 2 (A),
/// L = Start; Cross = 1, Circle = 2, Options = Start
pub fn defaults(m: *Map) void {
    const t = comptime [7]Binding{
        .of(&([_][]const u8{"KEY_W"} ++ up)),
        .of(&([_][]const u8{"KEY_S"} ++ down)),
        .of(&([_][]const u8{"KEY_A"} ++ left)),
        .of(&([_][]const u8{"KEY_D"} ++ right)),
        .of(&.{ "KEY_J", "BTN_SOUTH" }), // 1
        .of(&.{ "KEY_K", "BTN_EAST" }), // 2
        .of(&.{ "KEY_L", "BTN_START" }), // start
    };
    m.bind[0..7].* = t;
}
