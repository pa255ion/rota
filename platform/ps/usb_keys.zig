//! A USB keyboard's boot reports as the emulated system's keys (side track
//! E, step 4): HID keyboard usages translated to the Linux input codes the
//! key maps use (keymap.zig, shared with the desktop's `link`), so one
//! `keymap.conf` serves both.
//!
//! A boot report (HID 1.11 appendix B.1) is 8 bytes: modifier bits (usage
//! 0xE0 + bit), a reserved byte, then up to six key usages, 0 = none. When
//! more keys are down than fit, all six read ErrorRollOver (0x01): that
//! report says nothing about which keys, so the last good state stays.
//!
//! The usage-to-name table is our own, from the HID Usage Tables 1.12,
//! section 10 (Keyboard/Keypad page 0x07), naming each usage after the
//! Linux key it means (<linux/input-event-codes.h>, via input_codes.zig).
//! Usages it doesn't list (international and language keys, the rarely
//! seen 0x66 and up) are ignored.

const std = @import("std");
const keymap = @import("keymap.zig");
/// The key maps and the board's system table, for a program whose module
/// is this file (side track Q: the desktop emulator, which also needs the
/// usages' codes and the save sizes; a file can't be in two modules).
pub const maps = keymap;
pub const board_menu = menu;

/// Usage 0x04 + i (Keyboard a and A, ...) to the Linux key name.
const names_from_04 = [_][]const u8{
    "KEY_A",          "KEY_B",         "KEY_C",          "KEY_D", // 0x04
    "KEY_E",          "KEY_F",         "KEY_G",          "KEY_H", // 0x08
    "KEY_I",          "KEY_J",         "KEY_K",          "KEY_L", // 0x0C
    "KEY_M",          "KEY_N",         "KEY_O",          "KEY_P", // 0x10
    "KEY_Q",          "KEY_R",         "KEY_S",          "KEY_T", // 0x14
    "KEY_U",          "KEY_V",         "KEY_W",          "KEY_X", // 0x18
    "KEY_Y",          "KEY_Z",         "KEY_1",          "KEY_2", // 0x1C
    "KEY_3",          "KEY_4",         "KEY_5",          "KEY_6", // 0x20
    "KEY_7",          "KEY_8",         "KEY_9",          "KEY_0", // 0x24
    "KEY_ENTER",      "KEY_ESC",       "KEY_BACKSPACE",  "KEY_TAB", // 0x28
    "KEY_SPACE",      "KEY_MINUS",     "KEY_EQUAL",      "KEY_LEFTBRACE", // 0x2C
    "KEY_RIGHTBRACE", "KEY_BACKSLASH", "KEY_BACKSLASH",  "KEY_SEMICOLON", // 0x30 (0x32: Non-US # and ~)
    "KEY_APOSTROPHE", "KEY_GRAVE",     "KEY_COMMA",      "KEY_DOT", // 0x34
    "KEY_SLASH",      "KEY_CAPSLOCK",  "KEY_F1",         "KEY_F2", // 0x38
    "KEY_F3",         "KEY_F4",        "KEY_F5",         "KEY_F6", // 0x3C
    "KEY_F7",         "KEY_F8",        "KEY_F9",         "KEY_F10", // 0x40
    "KEY_F11",        "KEY_F12",       "KEY_SYSRQ",      "KEY_SCROLLLOCK", // 0x44 (0x46: PrintScreen)
    "KEY_PAUSE",      "KEY_INSERT",    "KEY_HOME",       "KEY_PAGEUP", // 0x48
    "KEY_DELETE",     "KEY_END",       "KEY_PAGEDOWN",   "KEY_RIGHT", // 0x4C
    "KEY_LEFT",       "KEY_DOWN",      "KEY_UP",         "KEY_NUMLOCK", // 0x50
    "KEY_KPSLASH",    "KEY_KPASTERISK", "KEY_KPMINUS",   "KEY_KPPLUS", // 0x54
    "KEY_KPENTER",    "KEY_KP1",       "KEY_KP2",        "KEY_KP3", // 0x58
    "KEY_KP4",        "KEY_KP5",       "KEY_KP6",        "KEY_KP7", // 0x5C
    "KEY_KP8",        "KEY_KP9",       "KEY_KP0",        "KEY_KPDOT", // 0x60
    "KEY_102ND",      "KEY_COMPOSE", // 0x64 (Non-US \ and |, Application)
};

/// Usage 0xE0 + bit (the modifier byte's bits).
const modifier_names = [8][]const u8{
    "KEY_LEFTCTRL", "KEY_LEFTSHIFT", "KEY_LEFTALT", "KEY_LEFTMETA",
    "KEY_RIGHTCTRL", "KEY_RIGHTSHIFT", "KEY_RIGHTALT", "KEY_RIGHTMETA",
};

const first_usage = 0x04;
pub const last_usage = first_usage + names_from_04.len - 1;

/// The Linux input code of each usage (index = usage), 0 = none.
const codes: [256]keymap.Input = blk: {
    @setEvalBranchQuota(400_000);
    var t = [_]keymap.Input{0} ** 256;
    for (names_from_04, first_usage..) |n, u| t[u] = keymap.parseInput(n) orelse @compileError("unknown key " ++ n);
    for (modifier_names, 0xE0..) |n, u| t[u] = keymap.parseInput(n) orelse @compileError("unknown key " ++ n);
    break :blk t;
};

/// The Linux input code for a keyboard usage, null if not a key we know.
pub fn usageCode(usage: u8) ?keymap.Input {
    const c = codes[usage];
    return if (c == 0) null else c;
}

pub const rollover: u8 = 0x01;

/// The key pressed between two boot reports (the controls menu waits for
/// one; side K, 2026-10-18): a modifier first, then a key slot; null if
/// none was (a release, a rollover report, a key we have no name for).
pub fn newKey(before: *const [8]u8, now: *const [8]u8) ?keymap.Input {
    const mods = now[0] & ~before[0];
    if (mods != 0) return codes[0xE0 + @as(u8, @ctz(mods))];
    if (now[2] == rollover) return null;
    for (now[2..8]) |u| {
        if (u == 0 or std.mem.indexOfScalar(u8, before[2..8], u) != null) continue;
        if (usageCode(u)) |c| return c;
    }
    return null;
}

/// What the keyboard holds for the ROM menu (step 5): the five directions
/// the board buttons give (menu.Buttons turns them into events with the
/// same repeat), and the key that opens and closes the menu.
pub const Nav = packed struct(u8) {
    up: bool = false,
    down: bool = false,
    left: bool = false,
    right: bool = false,
    center: bool = false,
    menu: bool = false,
    _pad: u2 = 0,
};

/// The navigation keys every keyboard has, whatever the system's map:
/// the arrows; Enter, keypad Enter and Space open; Backspace goes back.
const nav_fixed = blk: {
    @setEvalBranchQuota(100_000);
    const P = struct { name: []const u8, nav: Nav };
    const list = [_]P{
        .{ .name = "KEY_UP", .nav = .{ .up = true } },
        .{ .name = "KEY_DOWN", .nav = .{ .down = true } },
        .{ .name = "KEY_LEFT", .nav = .{ .left = true } },
        .{ .name = "KEY_BACKSPACE", .nav = .{ .left = true } },
        .{ .name = "KEY_RIGHT", .nav = .{ .right = true } },
        .{ .name = "KEY_ENTER", .nav = .{ .center = true } },
        .{ .name = "KEY_KPENTER", .nav = .{ .center = true } },
        .{ .name = "KEY_SPACE", .nav = .{ .center = true } },
    };
    var t: [list.len]struct { code: keymap.Input, nav: Nav } = undefined;
    for (list, 0..) |p, i| t[i] = .{ .code = keymap.parseInput(p.name).?, .nav = p.nav };
    break :blk t;
};

/// Follows a boot keyboard's reports and gives the key word for one
/// system's map.
pub const Keyboard = struct {
    lookup: keymap.Lookup,
    /// which kind of system the map is for: its own keys move in the menu
    /// too (D-pad, A, B; CHIP-8's 2 4 6 8 5)
    system: keymap.System,
    /// the `[global] menu` keys
    menu: keymap.Binding = keymap.default_menu,
    /// the last report that said which keys are down (word-aligned: the
    /// compiler zeroes and copies it with word accesses, which fault on
    /// an unaligned address with the MMU off; tools/check-ps-align.sh)
    report: [8]u8 align(4) = [_]u8{0} ** 8,
    word: keymap.Word = 0,

    pub fn init(map: *const keymap.Map) Keyboard {
        return .{ .lookup = keymap.Lookup.init(map), .system = map.system };
    }

    /// Another map (the system changed, or keymap.conf was read): the
    /// keys held now count with it.
    /// `init` in place (the relay's keyboard: no Lookup copied by value
    /// on the PS's stack).
    pub fn reset(k: *Keyboard, map: *const keymap.Map) void {
        k.lookup.fill(map);
        k.system = map.system;
        k.menu = keymap.default_menu;
        k.report = [_]u8{0} ** 8;
        k.word = 0;
    }

    pub fn setMap(k: *Keyboard, map: *const keymap.Map) void {
        k.lookup.fill(map);
        k.system = map.system;
        k.word = k.wordOf(&k.report);
    }

    /// What is held for the menu, from the last report.
    pub fn nav(k: *const Keyboard) Nav {
        var n: Nav = .{};
        const r = &k.report;
        for (r[2..8]) |u| if (usageCode(u)) |c| {
            for (nav_fixed) |f| if (f.code == c) {
                n = @bitCast(@as(u8, @bitCast(n)) | @as(u8, @bitCast(f.nav)));
            };
            if (k.menu.has(c)) n.menu = true;
        };
        for (0..8) |bit| if (r[0] & (@as(u8, 1) << @intCast(bit)) != 0) {
            if (k.menu.has(codes[0xE0 + bit])) n.menu = true;
        };
        // the system's own buttons, through its map
        const w = k.word;
        switch (k.system) {
            .gb, .nes, .gg, .sms => {
                // bit 0 Right, 1 Left, 2 Up, 3 Down, 4 A, 5 B (keymap.zig;
                // the Game Gear's 2 and 1)
                if (w & (1 << 2) != 0) n.up = true;
                if (w & (1 << 3) != 0) n.down = true;
                if (w & (1 << 1) != 0) n.left = true;
                if (w & (1 << 0) != 0) n.right = true;
                if (w & (1 << 4) != 0) n.center = true;
                if (w & (1 << 5) != 0) n.left = true;
            },
            .snes => {
                // (M7) the pad's bits (snes_keys.zig): 7 up, 6 down, 5 left,
                // 4 right, 3 A to open, 11 B back (as the Game Boy's B)
                if (w & (1 << 7) != 0) n.up = true;
                if (w & (1 << 6) != 0) n.down = true;
                if (w & (1 << 5) != 0) n.left = true;
                if (w & (1 << 4) != 0) n.right = true;
                if (w & (1 << 3) != 0) n.center = true;
                if (w & (1 << 11) != 0) n.left = true;
            },
            .a800xl => {
                // the joystick (keymap.zig): 0 up, 1 down, 2 left, 3 right, 4 trigger
                if (w & (1 << 0) != 0) n.up = true;
                if (w & (1 << 1) != 0) n.down = true;
                if (w & (1 << 2) != 0) n.left = true;
                if (w & (1 << 3) != 0) n.right = true;
                if (w & (1 << 4) != 0) n.center = true;
            },
            .chip8 => {
                // the keypad's 2 4 6 8 as directions, 5 to open
                if (w & (1 << 2) != 0) n.up = true;
                if (w & (1 << 8) != 0) n.down = true;
                if (w & (1 << 4) != 0) n.left = true;
                if (w & (1 << 6) != 0) n.right = true;
                if (w & (1 << 5) != 0) n.center = true;
            },
        }
        return n;
    }

    /// A new report (at least the first 3 bytes; boot reports are 8).
    /// Returns the key word: bit b = the system's button with bit b held.
    pub fn feed(k: *Keyboard, r: []const u8) keymap.Word {
        if (r.len < 3) return k.word;
        var full = [_]u8{0} ** 8;
        @memcpy(full[0..@min(r.len, 8)], r[0..@min(r.len, 8)]);
        // phantom state: all slots ErrorRollOver, keep what we had
        if (full[2] == rollover) return k.word;
        k.report = full;
        k.word = k.wordOf(&full);
        return k.word;
    }

    /// Whether a key of `b` is held in the last report (side track O: the
    /// frame advance keys).
    pub fn holds(k: *const Keyboard, b: *const keymap.Binding) bool {
        const r = &k.report;
        for (r[2..8]) |u| if (usageCode(u)) |c| {
            if (b.has(c)) return true;
        };
        for (0..8) |bit| if (r[0] & (@as(u8, 1) << @intCast(bit)) != 0) {
            if (b.has(codes[0xE0 + bit])) return true;
        };
        return false;
    }

    /// Nothing held (the keyboard went away).
    pub fn release(k: *Keyboard) void {
        k.report = [_]u8{0} ** 8;
        k.word = 0;
    }

    fn wordOf(k: *const Keyboard, r: *const [8]u8) keymap.Word {
        var w: keymap.Word = 0;
        for (0..8) |bit| if (r[0] & (@as(u8, 1) << @intCast(bit)) != 0) {
            w |= k.lookup.mask[codes[0xE0 + bit]];
        };
        for (r[2..8]) |u| if (usageCode(u)) |c| {
            w |= k.lookup.mask[c];
        };
        return w;
    }
};

// --- Tests ----------------------------------------------------------------

const testing = std.testing;
const fixtures = @import("usb_fixtures.zig");
const menu = @import("menu.zig");
const pl = @import("pl.zig");

fn report(mods: u8, keys: []const u8) [8]u8 {
    var r = [_]u8{0} ** 8;
    r[0] = mods;
    @memcpy(r[2 .. 2 + keys.len], keys);
    return r;
}

/// The menu's buttons as the relay makes them from a Nav.
fn asButtons(n: Nav) pl.Buttons {
    return .{ .u = n.up, .d = n.down, .l = n.left, .r = n.right, .c = n.center };
}

test "navigation: arrows, Enter, Backspace on any map; Esc is the menu key" {
    const m = keymap.defaults(.chip8);
    var k = Keyboard.init(&m);
    _ = k.feed(&report(0, &.{0x52}));
    try testing.expectEqual(Nav{ .up = true }, k.nav());
    _ = k.feed(&report(0, &.{0x51}));
    try testing.expectEqual(Nav{ .down = true }, k.nav());
    _ = k.feed(&report(0, &.{0x2A})); // Backspace
    try testing.expectEqual(Nav{ .left = true }, k.nav());
    _ = k.feed(&report(0, &.{0x28})); // Enter
    try testing.expectEqual(Nav{ .center = true }, k.nav());
    _ = k.feed(&report(0, &.{0x29})); // Esc
    try testing.expectEqual(Nav{ .menu = true }, k.nav());
    _ = k.feed(&report(0, &.{}));
    try testing.expectEqual(Nav{}, k.nav());
}

test "navigation through the system's own keys: Game Boy WASD/K/J, CHIP-8 keypad 2 4 6 8 5" {
    const gb = keymap.defaults(.gb);
    var k = Keyboard.init(&gb);
    _ = k.feed(&report(0, &.{0x1A})); // W = up
    try testing.expectEqual(Nav{ .up = true }, k.nav());
    _ = k.feed(&report(0, &.{0x0E})); // K = A
    try testing.expectEqual(Nav{ .center = true }, k.nav());
    _ = k.feed(&report(0, &.{0x0D})); // J = B: back
    try testing.expectEqual(Nav{ .left = true }, k.nav());
    const c8 = keymap.defaults(.chip8);
    k.setMap(&c8);
    _ = k.feed(&report(0, &.{0x16})); // S = keypad 8 = down
    try testing.expectEqual(Nav{ .down = true }, k.nav());
    _ = k.feed(&report(0, &.{0x1A})); // W = keypad 5 = open
    try testing.expectEqual(Nav{ .center = true }, k.nav());
}

test "holds: the frame advance keys (F11, F10 by default; HID 0x44, 0x43)" {
    var c = keymap.Config.initDefaults();
    var k = Keyboard.init(c.map(.gb));
    _ = k.feed(&report(0, &.{0x44}));
    try testing.expect(k.holds(&c.frame_advance));
    try testing.expect(!k.holds(&c.frame_run));
    _ = k.feed(&report(0, &.{ 0x04, 0x43 }));
    try testing.expect(k.holds(&c.frame_run));
    try testing.expect(!k.holds(&c.frame_advance));
}

test "the menu key from [global] in keymap.conf, a modifier too" {
    var err: keymap.ParseError = undefined;
    const c = try keymap.parse("[global]\nmenu = KEY_F1 KEY_RIGHTCTRL\n", &err);
    const m = keymap.defaults(.nes);
    var k = Keyboard.init(&m);
    k.menu = c.menu;
    _ = k.feed(&report(0, &.{0x29})); // Esc: no longer the menu key
    try testing.expectEqual(Nav{}, k.nav());
    _ = k.feed(&report(0, &.{0x3A})); // F1
    try testing.expect(k.nav().menu);
    _ = k.feed(&report(0x10, &.{})); // Right Ctrl
    try testing.expect(k.nav().menu);
}

test "keyboard events drive the ROM menu like the board buttons, with the same repeat" {
    // a menu with three ROMs and a folder (menu.zig is pure state)
    var mn: menu.Menu = undefined;
    mn.init(menu.systemById(1).?);
    mn.begin();
    _ = mn.add("games", true, 0, 0);
    _ = mn.add("a.ch8", false, 10, 1);
    _ = mn.add("b.ch8", false, 10, 2);
    _ = mn.add("c.ch8", false, 10, 3);
    mn.done(0);
    const m = keymap.defaults(.chip8);
    var k = Keyboard.init(&m);
    var b: menu.Buttons = .{};
    var t: u64 = 0;
    const Step = struct { r: [8]u8, ms: u64 };
    // Down, released; Down held 400 + 90 ms (one repeat); Enter
    const steps = [_]Step{
        .{ .r = report(0, &.{0x51}), .ms = 1 },
        .{ .r = report(0, &.{}), .ms = 1 },
        .{ .r = report(0, &.{0x51}), .ms = 1 },
        .{ .r = report(0, &.{0x51}), .ms = 400 },
        .{ .r = report(0, &.{0x51}), .ms = 89 },
        .{ .r = report(0, &.{}), .ms = 1 },
        .{ .r = report(0, &.{0x28}), .ms = 1 },
    };
    var actions: [8]menu.Action = undefined;
    var n: usize = 0;
    for (steps) |s| {
        _ = k.feed(&s.r);
        var dt: u64 = 0;
        while (dt < s.ms) : (dt += 1) {
            t += 1;
            const a = mn.handle(b.poll(t, asButtons(k.nav())));
            if (a != .none) {
                actions[n] = a;
                n += 1;
            }
        }
    }
    // down to a.ch8, down to b.ch8, repeat to c.ch8, Enter loads it
    try testing.expectEqual(@as(usize, 4), n);
    try testing.expectEqual(@as(usize, 3), mn.cursor);
    try testing.expectEqual(menu.Action{ .load = 3 }, actions[3]);
}


test "newKey: the key pressed between two reports (the controls menu)" {
    const in = keymap.parseInput;
    const none = [_]u8{0} ** 8;
    try testing.expectEqual(in("KEY_A"), newKey(&none, &.{ 0, 0, 0x04, 0, 0, 0, 0, 0 }));
    // A held, Q pressed beside it
    try testing.expectEqual(in("KEY_Q"), newKey(&.{ 0, 0, 0x04, 0, 0, 0, 0, 0 }, &.{ 0, 0, 0x04, 0x14, 0, 0, 0, 0 }));
    // a modifier: left shift (bit 1)
    try testing.expectEqual(in("KEY_LEFTSHIFT"), newKey(&none, &.{ 0x02, 0, 0, 0, 0, 0, 0, 0 }));
    // Enter, the arrows: keys like any other here
    try testing.expectEqual(in("KEY_ENTER"), newKey(&none, &.{ 0, 0, 0x28, 0, 0, 0, 0, 0 }));
    // releases, rollover, unknown usages: nothing
    try testing.expectEqual(@as(?keymap.Input, null), newKey(&.{ 0, 0, 0x04, 0, 0, 0, 0, 0 }, &none));
    try testing.expectEqual(@as(?keymap.Input, null), newKey(&none, &.{ 0, 0, rollover, rollover, rollover, rollover, rollover, rollover }));
    try testing.expectEqual(@as(?keymap.Input, null), newKey(&none, &.{ 0, 0, 0xFF, 0, 0, 0, 0, 0 }));
}

test "usage table: letters, digits, arrows, modifiers, and the gaps" {
    try testing.expectEqual(keymap.parseInput("KEY_A").?, usageCode(0x04).?);
    try testing.expectEqual(keymap.parseInput("KEY_Z").?, usageCode(0x1D).?);
    try testing.expectEqual(keymap.parseInput("KEY_1").?, usageCode(0x1E).?);
    try testing.expectEqual(keymap.parseInput("KEY_0").?, usageCode(0x27).?);
    try testing.expectEqual(keymap.parseInput("KEY_ESC").?, usageCode(0x29).?);
    try testing.expectEqual(keymap.parseInput("KEY_SPACE").?, usageCode(0x2C).?);
    try testing.expectEqual(keymap.parseInput("KEY_F12").?, usageCode(0x45).?);
    try testing.expectEqual(keymap.parseInput("KEY_UP").?, usageCode(0x52).?);
    try testing.expectEqual(keymap.parseInput("KEY_KPDOT").?, usageCode(0x63).?);
    try testing.expectEqual(keymap.parseInput("KEY_COMPOSE").?, usageCode(0x65).?);
    try testing.expectEqual(keymap.parseInput("KEY_LEFTSHIFT").?, usageCode(0xE1).?);
    try testing.expectEqual(keymap.parseInput("KEY_RIGHTMETA").?, usageCode(0xE7).?);
    try testing.expectEqual(@as(?keymap.Input, null), usageCode(0x00));
    try testing.expectEqual(@as(?keymap.Input, null), usageCode(rollover));
    try testing.expectEqual(@as(?keymap.Input, null), usageCode(0x66));
    try testing.expectEqual(@as(?keymap.Input, null), usageCode(0xE8));
}

test "the recorded typing, through the default CHIP-8 map" {
    // "hello claude code": with the CHIP-8 defaults E = key 6, A = key 7,
    // D = key 9, C = key B (QWERTY block = keypad); h, l, o, u unbound
    const m = keymap.defaults(.chip8);
    var k = Keyboard.init(&m);
    const want = [_]u16{
        0, 0, // h
        1 << 6, 0, // e
        0, 0, 0, 0, 0, 0, // l l o
        1 << 5, 0, // space = key 5
        1 << 0xB, 0, // c
        0, 0, // l
        1 << 7, 0, // a
        0, 0, // u
        1 << 9, 0, // d
        1 << 6, 0, // e
        1 << 5, 0, // space
        1 << 0xB, 0, // c
        0, 0, // o
        1 << 9, 0, // d
        1 << 6, 0, // e
    };
    for (fixtures.mini_keyboard_typing, want) |r, w| try testing.expectEqual(w, k.feed(&r));
}

test "several keys and modifiers at once (Game Boy map); rollover keeps the state" {
    const m = keymap.defaults(.gb);
    var k = Keyboard.init(&m);
    // W (up) + K (A) + Left Shift (select)
    const r1 = [8]u8{ 0x02, 0, 0x1A, 0x0E, 0, 0, 0, 0 };
    try testing.expectEqual(@as(u16, (1 << 2) | (1 << 4) | (1 << 6)), k.feed(&r1));
    // too many keys: all ErrorRollOver, nothing changes
    const ro = [8]u8{ 0x02, 0, 1, 1, 1, 1, 1, 1 };
    try testing.expectEqual(@as(u16, (1 << 2) | (1 << 4) | (1 << 6)), k.feed(&ro));
    // W released
    const r2 = [8]u8{ 0x02, 0, 0x0E, 0, 0, 0, 0, 0 };
    try testing.expectEqual(@as(u16, (1 << 4) | (1 << 6)), k.feed(&r2));
    k.release();
    try testing.expectEqual(@as(u16, 0), k.word);
}

test "a key map from the card replaces the defaults, keys held keep counting" {
    var err: keymap.ParseError = undefined;
    const c = try keymap.parse("[gb]\na = KEY_SPACE\n", &err);
    var defaults = keymap.defaults(.gb);
    var k = Keyboard.init(&defaults);
    const space = [8]u8{ 0, 0, 0x2C, 0, 0, 0, 0, 0 };
    try testing.expectEqual(@as(u16, 0), k.feed(&space));
    var cc = c;
    k.setMap(cc.map(.gb));
    try testing.expectEqual(@as(u16, 1 << 4), k.word);
}

test "make keymap's output, read on the PS, binds exactly as on the desktop" {
    // the desktop writes the file (keymap.write); the PS parses the same
    // bytes with the same code: identical maps, for every system
    var c = keymap.Config.initDefaults();
    c.map(.nes).bind[4] = .{};
    try c.map(.nes).bind[4].add(keymap.parseInput("KEY_X").?);
    var buf: [4096]u8 = undefined;
    var w: std.Io.Writer = .fixed(&buf);
    try keymap.write(&w, &c);
    var err: keymap.ParseError = undefined;
    const back = try keymap.parse(w.buffered(), &err);
    for (std.enums.values(keymap.System)) |s| try testing.expect(back.maps[@intFromEnum(s)].eql(&c.maps[@intFromEnum(s)]));
}
