//! The Atari 800XL's keyboard from a USB keyboard (M4 step 4.8): a boot
//! report's keys as the 800XL's key matrix, SHIFT, CONTROL, BREAK and the
//! console keys, in the bytes A800.Loader takes at `base` (the relay
//! writes them over the PS link whenever they change; the joystick byte
//! comes from the `[a800xl]` key map, keymap.zig).
//!
//! The layout is positional where the 800XL has the key, otherwise by
//! what the key means (docs/systems/atari800xl.md, "Input"):
//!
//! * letters, digits, Space, Return (and keypad Enter), Backspace, Tab,
//!   Caps Lock, `- = ; , . /` as themselves; `'` is `+`, `\` is `*`,
//!   `[` and `]` are `<` and `>`; `` ` `` is ESC (Esc opens the menu);
//! * Shift is SHIFT, Ctrl is CONTROL; F1 HELP, F2 START, F3 SELECT,
//!   F4 OPTION, F6 the inverse key, F8 and Pause BREAK;
//! * the arrows are CONTROL with `- = + *` (the 800XL's cursor keys),
//!   Delete is CONTROL+Backspace, Insert CONTROL+`>`.

const std = @import("std");
const keymap = @import("keymap.zig");
const usb_keys = @import("usb_keys.zig");
/// The USB keyboard's codes (and through it the key maps and the board's
/// system table), for a program whose module is this file (side track Q:
/// the desktop emulator; a file can't be in two modules).
pub const usb = usb_keys;

/// A800.Loader.keysBase: bytes 0-7 the matrix (bit n mod 8 of byte n / 8
/// = key code n), 8 SHIFT/CONTROL/BREAK, 9 START/SELECT/OPTION, 10 the
/// joystick (up, down, left, right, trigger).
pub const base: u24 = 0x100000;
pub const len = 11;

const Action = union(enum) {
    key: u6,
    /// the key with CONTROL
    ctrl_key: u6,
    shift,
    control,
    brk,
    /// 0 START, 1 SELECT, 2 OPTION
    console: u2,
};

/// Linux key name -> what it does on the 800XL (key codes: A800.Keyboard,
/// "Mapping the Atari" $D209).
const named = [_]struct { []const u8, Action }{
    .{ "KEY_L", .{ .key = 0x00 } },          .{ "KEY_J", .{ .key = 0x01 } },          .{ "KEY_SEMICOLON", .{ .key = 0x02 } },
    .{ "KEY_K", .{ .key = 0x05 } },          .{ "KEY_APOSTROPHE", .{ .key = 0x06 } }, .{ "KEY_BACKSLASH", .{ .key = 0x07 } },
    .{ "KEY_O", .{ .key = 0x08 } },          .{ "KEY_P", .{ .key = 0x0A } },          .{ "KEY_U", .{ .key = 0x0B } },
    .{ "KEY_ENTER", .{ .key = 0x0C } },      .{ "KEY_KPENTER", .{ .key = 0x0C } },    .{ "KEY_I", .{ .key = 0x0D } },
    .{ "KEY_MINUS", .{ .key = 0x0E } },      .{ "KEY_EQUAL", .{ .key = 0x0F } },      .{ "KEY_V", .{ .key = 0x10 } },
    .{ "KEY_F1", .{ .key = 0x11 } },         .{ "KEY_C", .{ .key = 0x12 } },          .{ "KEY_B", .{ .key = 0x15 } },
    .{ "KEY_X", .{ .key = 0x16 } },          .{ "KEY_Z", .{ .key = 0x17 } },          .{ "KEY_4", .{ .key = 0x18 } },
    .{ "KEY_3", .{ .key = 0x1A } },          .{ "KEY_6", .{ .key = 0x1B } },          .{ "KEY_GRAVE", .{ .key = 0x1C } },
    .{ "KEY_5", .{ .key = 0x1D } },          .{ "KEY_2", .{ .key = 0x1E } },          .{ "KEY_1", .{ .key = 0x1F } },
    .{ "KEY_COMMA", .{ .key = 0x20 } },      .{ "KEY_SPACE", .{ .key = 0x21 } },      .{ "KEY_DOT", .{ .key = 0x22 } },
    .{ "KEY_N", .{ .key = 0x23 } },          .{ "KEY_M", .{ .key = 0x25 } },          .{ "KEY_SLASH", .{ .key = 0x26 } },
    .{ "KEY_F6", .{ .key = 0x27 } },         .{ "KEY_R", .{ .key = 0x28 } },          .{ "KEY_E", .{ .key = 0x2A } },
    .{ "KEY_Y", .{ .key = 0x2B } },          .{ "KEY_TAB", .{ .key = 0x2C } },        .{ "KEY_T", .{ .key = 0x2D } },
    .{ "KEY_W", .{ .key = 0x2E } },          .{ "KEY_Q", .{ .key = 0x2F } },          .{ "KEY_9", .{ .key = 0x30 } },
    .{ "KEY_0", .{ .key = 0x32 } },          .{ "KEY_7", .{ .key = 0x33 } },          .{ "KEY_BACKSPACE", .{ .key = 0x34 } },
    .{ "KEY_8", .{ .key = 0x35 } },          .{ "KEY_LEFTBRACE", .{ .key = 0x36 } },  .{ "KEY_RIGHTBRACE", .{ .key = 0x37 } },
    .{ "KEY_F", .{ .key = 0x38 } },          .{ "KEY_H", .{ .key = 0x39 } },          .{ "KEY_D", .{ .key = 0x3A } },
    .{ "KEY_CAPSLOCK", .{ .key = 0x3C } },   .{ "KEY_G", .{ .key = 0x3D } },          .{ "KEY_S", .{ .key = 0x3E } },
    .{ "KEY_A", .{ .key = 0x3F } },
    .{ "KEY_UP", .{ .ctrl_key = 0x0E } },    .{ "KEY_DOWN", .{ .ctrl_key = 0x0F } },  .{ "KEY_LEFT", .{ .ctrl_key = 0x06 } },
    .{ "KEY_RIGHT", .{ .ctrl_key = 0x07 } }, .{ "KEY_DELETE", .{ .ctrl_key = 0x34 } }, .{ "KEY_INSERT", .{ .ctrl_key = 0x37 } },
    .{ "KEY_LEFTSHIFT", .shift },            .{ "KEY_RIGHTSHIFT", .shift },
    .{ "KEY_LEFTCTRL", .control },           .{ "KEY_RIGHTCTRL", .control },
    .{ "KEY_F8", .brk },                     .{ "KEY_PAUSE", .brk },
    .{ "KEY_F2", .{ .console = 0 } },        .{ "KEY_F3", .{ .console = 1 } },        .{ "KEY_F4", .{ .console = 2 } },
};

/// Indexed by Linux key code (below 0x100: every key a boot keyboard has).
const table: [0x100]?Action = blk: {
    @setEvalBranchQuota(400_000);
    var t = [_]?Action{null} ** 0x100;
    for (named) |n| {
        const code = keymap.parseInput(n[0]) orelse @compileError("unknown key " ++ n[0]);
        if (t[code] != null) @compileError("key given twice: " ++ n[0]);
        t[code] = n[1];
    }
    break :blk t;
};

fn apply(out: *[len]u8, code: keymap.Input) void {
    if (code >= table.len) return;
    const a = table[code] orelse return;
    switch (a) {
        .key => |k| out[k / 8] |= @as(u8, 1) << @intCast(k % 8),
        .ctrl_key => |k| {
            out[k / 8] |= @as(u8, 1) << @intCast(k % 8);
            out[8] |= 2;
        },
        .shift => out[8] |= 1,
        .control => out[8] |= 2,
        .brk => out[8] |= 4,
        .console => |c| out[9] |= @as(u8, 1) << c,
    }
}

/// The bytes for a boot report (modifiers, reserved, six usages) and the
/// joystick bits (the `[a800xl]` map's word: up, down, left, right,
/// trigger in bits 0-4).
pub fn bytes(report: *const [8]u8, joy: u16) [len]u8 {
    var out = [_]u8{0} ** len;
    for (0..8) |b| {
        if (report[0] & (@as(u8, 1) << @intCast(b)) != 0) {
            if (usb_keys.usageCode(@intCast(0xE0 + b))) |c| apply(&out, c);
        }
    }
    for (report[2..8]) |u| {
        if (u == 0 or u == usb_keys.rollover) continue;
        if (usb_keys.usageCode(u)) |c| apply(&out, c);
    }
    out[10] = @truncate(joy & 0x1F);
    return out;
}

const testing = std.testing;

fn mkReport(mods: u8, keys: []const u8) [8]u8 {
    var r = [_]u8{0} ** 8;
    r[0] = mods;
    @memcpy(r[2 .. 2 + keys.len], keys);
    return r;
}

test "letters and digits are the 800XL's keys (A = 0x3F, 1 = 0x1F, L = 0x00)" {
    const b = bytes(&mkReport(0, &.{ 0x04, 0x1E, 0x0F }), 0); // A, 1, L
    try testing.expectEqualSlices(u8, &.{ 0x01, 0, 0, 0x80, 0, 0, 0, 0x80, 0, 0, 0 }, &b);
}

test "Shift and Ctrl are SHIFT and CONTROL; the arrows add CONTROL" {
    try testing.expectEqual(@as(u8, 1), bytes(&mkReport(0x02, &.{}), 0)[8]); // left Shift
    try testing.expectEqual(@as(u8, 2), bytes(&mkReport(0x10, &.{}), 0)[8]); // right Ctrl
    const up = bytes(&mkReport(0, &.{0x52}), 0); // Up: CONTROL + '-' (0x0E)
    try testing.expectEqual(@as(u8, 2), up[8]);
    try testing.expectEqual(@as(u8, 1 << 6), up[1]);
}

test "F2-F4 the console keys, F8 BREAK, Esc nothing (the menu), the joystick byte" {
    const b = bytes(&mkReport(0, &.{ 0x3B, 0x3D, 0x41, 0x29 }), 0x13); // F2, F4, F8, Esc
    try testing.expectEqual(@as(u8, 0b101), b[9]);
    try testing.expectEqual(@as(u8, 4), b[8]);
    for (b[0..8]) |x| try testing.expectEqual(@as(u8, 0), x);
    try testing.expectEqual(@as(u8, 0x13), b[10]);
}

test "a rollover report holds nothing extra" {
    const b = bytes(&mkReport(0, &.{ 0x01, 0x01, 0x01, 0x01, 0x01, 0x01 }), 0);
    for (b) |x| try testing.expectEqual(@as(u8, 0), x);
}
