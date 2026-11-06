//! Which desktop inputs are which buttons of the emulated system (bit k of
//! the key word = button k), as data: one table of logical buttons per
//! system, default bindings, and a plain-text config file that overrides
//! them (side track B; `make keymap`, `link --remap`, remap.zig).
//!
//! An input is a Linux input event code, named as in
//! <linux/input-event-codes.h> (input_codes.zig): a key or button
//! (`KEY_K`, `BTN_SOUTH`, `BTN_DPAD_UP`) or one direction of an axis
//! (`ABS_HAT0X-`, `ABS_Y+`). An axis direction is held while the axis is
//! past half way from its centre towards that end (for a D-pad hat,
//! -1..1, any non-zero value). Key codes without a name are written
//! `0x1A2`, axes without one `ABS_0x29+`.
//!
//! The file, by default `$XDG_CONFIG_HOME/emulation_clash/keymap.conf`
//! (`~/.config/...`), one section per system:
//!
//!     # comment
//!     [gb]
//!     a = KEY_K BTN_EAST
//!     select = KEY_RIGHTSHIFT
//!     start =
//!
//! A button's line replaces its default bindings (`start =` unbinds it);
//! buttons and systems the file doesn't mention keep their defaults.
//!
//! One more section, `[global]`, holds what isn't a system's button:
//! `menu = KEY_ESC` (the default), the key that opens the ROM menu from a
//! game on the board's USB keyboard (side track E; the desktop `link`
//! doesn't use it); `splash_on_switch`; and the frame advance keys (side
//! track O, O.3), `frame_advance = KEY_F11` (the first press pauses the
//! game, each one after runs it one frame) and `frame_run = KEY_F10`, on
//! the board's keyboard and in `link`; `screenshot = KEY_F12` (side track
//! O, O.4: the running game's next frame into the card's /screens). Any
//! error (unknown section, button or input name, a button given twice, an
//! input listed twice on one line, a line outside a section, too many
//! inputs) is reported with its line number, and `link` refuses to start.

const std = @import("std");
const codes = @import("input_codes.zig");

pub const System = enum {
    chip8,
    gb,
    nes,
    a800xl,
    gg,
    sms,
    snes,

    pub fn title(s: System) []const u8 {
        return switch (s) {
            .chip8 => chip8_keys.title,
            .gb => gb_keys.title,
            .nes => "NES",
            .a800xl => a800xl_keys.title,
            .gg => gg_keys.title,
            .sms => sms_keys.title,
            .snes => snes_keys.title,
        };
    }
};

/// A system's key word: bit b = the button with `bit` b held. 32 bits
/// (M7's request, 2026-10-16: the SNES's two 12-bit pads); bits 0 to 15
/// go to the PL as PS_GPIO's input word, 16 to 31 as link writes
/// (menu.System.keys_hi).
pub const Word = u32;

pub const Button = struct { name: []const u8, bit: u5 };

// Each system's buttons, defaults and title, in its own file (side track
// K, D4); the NES uses the Game Boy's (the same bits and keys).
const chip8_keys = @import("systems/chip8_keys.zig");
const gb_keys = @import("systems/gb_keys.zig");
const a800xl_keys = @import("systems/a800xl_keys.zig");
const gg_keys = @import("systems/gg_keys.zig");
const sms_keys = @import("systems/sms_keys.zig");
const snes_keys = @import("systems/snes_keys.zig");

pub fn buttons(s: System) []const Button {
    return switch (s) {
        .chip8 => &chip8_keys.buttons,
        .gb, .nes => &gb_keys.buttons,
        .a800xl => &a800xl_keys.buttons,
        .gg => &gg_keys.buttons,
        .sms => &sms_keys.buttons,
        .snes => &snes_keys.buttons,
    };
}

pub fn buttonIndex(s: System, name: []const u8) ?usize {
    for (buttons(s), 0..) |b, i| if (std.ascii.eqlIgnoreCase(b.name, name)) return i;
    return null;
}

/// One input: a key/button code (0 to 0x2FF), or an axis direction,
/// 0x300 + 2 * axis + (1 for +).
pub const Input = u16;
pub const key_codes = 0x300;
pub const axis_count = 64;
pub const input_count = key_codes + 2 * axis_count;

pub fn axisInput(axis: u6, positive: bool) Input {
    return key_codes + 2 * @as(Input, axis) + @intFromBool(positive);
}

/// A gamepad input (a `BTN_*` code or an axis) rather than a keyboard key:
/// what the remap utility replaces when a pad input is pressed.
pub fn isPad(in: Input) bool {
    if (in >= key_codes) return true;
    const n = keyName(in) orelse return false;
    return std.mem.startsWith(u8, n, "BTN_");
}

fn keyName(code: u16) ?[]const u8 {
    for (codes.keys) |k| if (k.canonical and k.code == code) return k.name;
    return null;
}

fn axisName(code: u16) ?[]const u8 {
    for (codes.axes) |a| if (a.canonical and a.code == code) return a.name;
    return null;
}

/// The input's name as the file writes it (`buf`: 32 bytes is enough).
/// By hand, not std.fmt: the PS app writes key maps too (the controls
/// menu), and its OCM has no room for the formatter (side K, 2026-10-18).
pub fn formatInput(in: Input, buf: []u8) []const u8 {
    if (in < key_codes) {
        if (keyName(in)) |n| return n;
        return hexName(buf, "0x", in, 3, "");
    }
    const axis = (in - key_codes) / 2;
    const sign: []const u8 = if ((in - key_codes) % 2 == 1) "+" else "-";
    if (axisName(axis)) |n| {
        @memcpy(buf[0..n.len], n);
        buf[n.len] = sign[0];
        return buf[0 .. n.len + 1];
    }
    return hexName(buf, "ABS_0x", axis, 2, sign);
}

/// head, `digits` upper-case hex digits of v, tail.
fn hexName(buf: []u8, head: []const u8, v: u16, digits: usize, tail: []const u8) []const u8 {
    @memcpy(buf[0..head.len], head);
    for (0..digits) |k| buf[head.len + k] = "0123456789ABCDEF"[(v >> @intCast(4 * (digits - 1 - k))) & 0xF];
    @memcpy(buf[head.len + digits ..][0..tail.len], tail);
    return buf[0 .. head.len + digits + tail.len];
}

pub fn parseInput(s: []const u8) ?Input {
    if (s.len > 1 and (s[s.len - 1] == '+' or s[s.len - 1] == '-')) {
        const base = s[0 .. s.len - 1];
        const positive = s[s.len - 1] == '+';
        for (codes.axes) |a| if (std.mem.eql(u8, a.name, base)) return axisInput(@intCast(a.code), positive);
        if (std.mem.startsWith(u8, base, "ABS_0x")) {
            const a = std.fmt.parseInt(u6, base[6..], 16) catch return null;
            return axisInput(a, positive);
        }
        return null;
    }
    for (codes.keys) |k| if (std.mem.eql(u8, k.name, s)) return k.code;
    if (std.mem.startsWith(u8, s, "0x")) {
        const c = std.fmt.parseInt(u16, s[2..], 16) catch return null;
        if (c > 0 and c < key_codes) return c;
    }
    return null;
}

pub const max_bindings = 8;
/// buttons a system may have (`Map.bind`): one per bit of `Word`
pub const max_buttons = @bitSizeOf(Word);

pub const Binding = struct {
    inputs: [max_bindings]Input = undefined,
    n: u8 = 0,

    pub fn slice(b: *const Binding) []const Input {
        return b.inputs[0..b.n];
    }

    pub fn has(b: *const Binding, in: Input) bool {
        return std.mem.indexOfScalar(Input, b.slice(), in) != null;
    }

    pub fn add(b: *Binding, in: Input) error{TooMany}!void {
        if (b.has(in)) return;
        if (b.n == max_bindings) return error.TooMany;
        b.inputs[b.n] = in;
        b.n += 1;
    }

    /// `in` taken out (the others keep their order).
    pub fn remove(b: *Binding, in: Input) void {
        var k: u8 = 0;
        for (b.slice()) |x| if (x != in) {
            b.inputs[k] = x;
            k += 1;
        };
        b.n = k;
    }

    /// Its keyboard keys taken out, its pad inputs kept.
    pub fn clearKeys(b: *Binding) void {
        var k: u8 = 0;
        for (b.slice()) |x| if (isPad(x)) {
            b.inputs[k] = x;
            k += 1;
        };
        b.n = k;
    }

    pub fn of(comptime names: []const []const u8) Binding {
        @setEvalBranchQuota(200_000);
        var b: Binding = .{};
        inline for (names) |n| b.add(comptime parseInput(n) orelse @compileError("unknown input " ++ n)) catch unreachable;
        return b;
    }
};

/// One system's map: the bindings of each of its buttons, indexed like
/// `buttons(system)`.
pub const Map = struct {
    system: System,
    /// one per button, by its index in `buttons` (32: as many as a key
    /// word's bits, M7's request: the SNES's two pads need 24)
    bind: [max_buttons]Binding = [_]Binding{.{}} ** max_buttons,

    pub fn eql(a: *const Map, b: *const Map) bool {
        if (a.system != b.system) return false;
        for (0..buttons(a.system).len) |i| {
            if (!std.mem.eql(Input, a.bind[i].slice(), b.bind[i].slice())) return false;
        }
        return true;
    }
};

/// The compiled-in maps of `link` before side track B, as data.
///
/// CHIP-8 keyboard: the usual layout plus arrows = 2/8/4/6 and space = 5
/// (the common move/fire keys, e.g. Space Invaders: 4/6 move, 5 fires)
///
///   1 2 3 4        1 2 3 C
///   Q W E R   ->   4 5 6 D
///   A S D F        7 8 9 E
///   Z X C V        A 0 B F
///
/// CHIP-8 gamepad (PS4 names; hid-playstation uses the generic codes):
/// Cross 5, Circle B, Square C, Triangle 1, L1 E, R1 F, Options A, Share 0;
/// D-pad and left stick 2/8/4/6. E/F/A move and select in the Timendus
/// test menus; B is 7-beep's key.
///
/// Atari 800XL joystick (M4 step 4.8): the keypad's 8 2 4 6 (and 5 down),
/// 0 or Right Alt the trigger; the gamepad's D-pad, left stick, Cross.
/// The rest of the keyboard types on the 800XL (a800_keys.zig).
///
/// Game Boy and NES keyboard, as confirmed by the user (2026-10-02 and
/// 2026-10-03): W A S D for the D-pad, J = B, K = A, L = Start, Left
/// Shift = Select. Gamepad: Circle = A, Cross = B, Share = Select,
/// Options = Start, D-pad and left stick = D-pad.
pub fn defaults(s: System) Map {
    var m: Map = .{ .system = s };
    switch (s) {
        .chip8 => chip8_keys.defaults(&m),
        .gb, .nes => gb_keys.defaults(&m),
        .a800xl => a800xl_keys.defaults(&m),
        .gg => gg_keys.defaults(&m),
        .sms => sms_keys.defaults(&m),
        .snes => snes_keys.defaults(&m),
    }
    return m;
}

/// The D-pad, hat and left stick inputs every pad system binds to its
/// directions (the systems' key files use them).
pub const dpad_up = [_][]const u8{ "BTN_DPAD_UP", "ABS_HAT0Y-", "ABS_Y-" };
pub const dpad_down = [_][]const u8{ "BTN_DPAD_DOWN", "ABS_HAT0Y+", "ABS_Y+" };
pub const dpad_left = [_][]const u8{ "BTN_DPAD_LEFT", "ABS_HAT0X-", "ABS_X-" };
pub const dpad_right = [_][]const u8{ "BTN_DPAD_RIGHT", "ABS_HAT0X+", "ABS_X+" };

/// The `[global]` section's default `menu` key.
pub const default_menu: Binding = .of(&.{"KEY_ESC"});
/// Frame advance (side track O, O.3): the first press pauses the game,
/// each one after runs it one frame; `frame_run` lets it run again.
pub const default_frame_advance: Binding = .of(&.{"KEY_F11"});
pub const default_frame_run: Binding = .of(&.{"KEY_F10"});
/// A screenshot (side track O, O.4) into the card's /screens.
pub const default_screenshot: Binding = .of(&.{"KEY_F12"});

/// Every system's map, and the `[global]` settings.
pub const Config = struct {
    maps: [std.enums.values(System).len]Map,
    /// keys that open the ROM menu from a game (the board's USB keyboard)
    menu: Binding = default_menu,
    /// the boot splash (side track J) after a switch on the Systems menu
    /// too, not only at power-on (on by default: the user, 2026-10-08)
    splash_on_switch: bool = true,
    /// frame advance (side track O, O.3), on the board's USB keyboard and
    /// in link
    frame_advance: Binding = default_frame_advance,
    frame_run: Binding = default_frame_run,
    screenshot: Binding = default_screenshot,

    pub fn initDefaults() Config {
        var c: Config = undefined;
        c.setDefaults();
        return c;
    }

    /// In place (the PS app: with 32 buttons a map a Config is about 4 KiB;
    /// a copy by value on its stack failed the stack check).
    pub fn setDefaults(c: *Config) void {
        c.menu = default_menu;
        c.splash_on_switch = true;
        c.frame_advance = default_frame_advance;
        c.frame_run = default_frame_run;
        c.screenshot = default_screenshot;
        for (std.enums.values(System)) |s| c.maps[@intFromEnum(s)] = defaults(s);
    }

    pub fn map(c: *Config, s: System) *Map {
        return &c.maps[@intFromEnum(s)];
    }
};

pub const ParseError = struct {
    line: usize,
    msg: []const u8,
    buf: [160]u8 = undefined,

    pub fn set(e: *ParseError, line: usize, comptime fmt: []const u8, args: anytype) error{Invalid} {
        e.line = line;
        e.msg = std.fmt.bufPrint(&e.buf, fmt, args) catch "line too long";
        return error.Invalid;
    }
};

/// Read a key-map file over the defaults. On error.Invalid, `err` says
/// which line and why.
pub fn parse(text: []const u8, err: *ParseError) error{Invalid}!Config {
    var c: Config = undefined;
    try parseInto(text, err, &c);
    return c;
}

/// `parse` into `c` (M6, side track K's choice): a Config holds a map per
/// system, so returning one by value put two copies on the PS's stack
/// (the relay parses into a static one). `c`'s old contents don't matter.
pub fn parseInto(text: []const u8, err: *ParseError, c: *Config) error{Invalid}!void {
    c.setDefaults();
    var cur: ?System = null;
    var in_global = false;
    var menu_given = false;
    var splash_given = false;
    var advance_given = false;
    var shot_given = false;
    var run_given = false;
    var given = [_]u32{0} ** std.enums.values(System).len; // buttons with a line, per system
    var it = std.mem.splitScalar(u8, text, '\n');
    var ln: usize = 0;
    while (it.next()) |raw| {
        ln += 1;
        const hash = std.mem.indexOfScalar(u8, raw, '#') orelse raw.len;
        const l = std.mem.trim(u8, raw[0..hash], " \t\r");
        if (l.len == 0) continue;
        if (l[0] == '[') {
            if (l[l.len - 1] != ']') return err.set(ln, "a section is '[name]'", .{});
            const name = std.mem.trim(u8, l[1 .. l.len - 1], " \t");
            in_global = std.mem.eql(u8, name, "global");
            if (in_global) continue;
            cur = std.meta.stringToEnum(System, name) orelse
                return err.set(ln, "unknown system '{s}' (chip8, gb, nes, a800xl, gg, sms, snes; or global)", .{name});
            continue;
        }
        if (in_global) {
            const eq = std.mem.indexOfScalar(u8, l, '=') orelse return err.set(ln, "expected 'setting = inputs'", .{});
            const sname = std.mem.trim(u8, l[0..eq], " \t");
            if (std.ascii.eqlIgnoreCase(sname, "splash_on_switch")) {
                if (splash_given) return err.set(ln, "[global] setting 'splash_on_switch' is given twice", .{});
                splash_given = true;
                const v = std.mem.trim(u8, l[eq + 1 ..], " \t");
                c.splash_on_switch = if (std.ascii.eqlIgnoreCase(v, "on")) true else if (std.ascii.eqlIgnoreCase(v, "off")) false else return err.set(ln, "splash_on_switch is 'on' or 'off', not '{s}'", .{v});
                continue;
            }
            if (std.ascii.eqlIgnoreCase(sname, "frame_advance") or std.ascii.eqlIgnoreCase(sname, "frame_run")) {
                const adv = std.ascii.eqlIgnoreCase(sname, "frame_advance");
                const seen = if (adv) &advance_given else &run_given;
                if (seen.*) return err.set(ln, "[global] setting '{s}' is given twice", .{if (adv) "frame_advance" else "frame_run"});
                seen.* = true;
                (if (adv) &c.frame_advance else &c.frame_run).* = try parseInputs(l[eq + 1 ..], ln, err);
                continue;
            }
            if (std.ascii.eqlIgnoreCase(sname, "screenshot")) {
                if (shot_given) return err.set(ln, "[global] setting 'screenshot' is given twice", .{});
                shot_given = true;
                c.screenshot = try parseInputs(l[eq + 1 ..], ln, err);
                continue;
            }
            if (!std.ascii.eqlIgnoreCase(sname, "menu")) return err.set(ln, "[global] has no setting '{s}' (menu, splash_on_switch, frame_advance, frame_run, screenshot)", .{sname});
            if (menu_given) return err.set(ln, "[global] setting 'menu' is given twice", .{});
            menu_given = true;
            c.menu = try parseInputs(l[eq + 1 ..], ln, err);
            continue;
        }
        const s = cur orelse return err.set(ln, "'{s}' before the first [system] line", .{l});
        const eq = std.mem.indexOfScalar(u8, l, '=') orelse return err.set(ln, "expected 'button = inputs'", .{});
        const bname = std.mem.trim(u8, l[0..eq], " \t");
        const bi = buttonIndex(s, bname) orelse return err.set(ln, "{s} has no button '{s}'", .{ s.title(), bname });
        const bit = @as(u32, 1) << @intCast(bi);
        if (given[@intFromEnum(s)] & bit != 0) return err.set(ln, "{s} button '{s}' is given twice", .{ s.title(), bname });
        given[@intFromEnum(s)] |= bit;
        c.map(s).bind[bi] = try parseInputs(l[eq + 1 ..], ln, err);
    }
}

/// The inputs after a line's '='.
fn parseInputs(text: []const u8, ln: usize, err: *ParseError) error{Invalid}!Binding {
    var b: Binding = .{};
    var words = std.mem.tokenizeAny(u8, text, " \t");
    while (words.next()) |w| {
        const in = parseInput(w) orelse return err.set(ln, "unknown input '{s}' (Linux input code names: KEY_K, BTN_SOUTH, ABS_HAT0X-)", .{w});
        if (b.has(in)) return err.set(ln, "'{s}' is listed twice", .{w});
        b.add(in) catch return err.set(ln, "more than {d} inputs for one button", .{max_bindings});
    }
    return b;
}

/// One system's section, as `parse` reads it: every button, in table order.
/// (writeAll only, no print: the PS app writes it too, as formatInput.)
pub fn writeMap(w: *std.Io.Writer, m: *const Map) std.Io.Writer.Error!void {
    try w.writeAll("[");
    try w.writeAll(@tagName(m.system));
    try w.writeAll("]\n");
    for (buttons(m.system), 0..) |b, i| try writeLine(w, b.name, &m.bind[i]);
}

/// "name = IN IN\n"
fn writeLine(w: *std.Io.Writer, name: []const u8, b: *const Binding) std.Io.Writer.Error!void {
    try w.writeAll(name);
    try w.writeAll(" =");
    for (b.slice()) |in| {
        var nb: [32]u8 = undefined;
        try w.writeAll(" ");
        try w.writeAll(formatInput(in, &nb));
    }
    try w.writeAll("\n");
}

pub fn write(w: *std.Io.Writer, c: *const Config) std.Io.Writer.Error!void {
    try w.writeAll(
        \\# Desktop key maps for link (make keys / make keymap; platform/ps/keymap.zig).
        \\# One section per system; "button = inputs", inputs named by their Linux
        \\# input event codes (KEY_K, BTN_SOUTH, ABS_HAT0X-). A button left out keeps
        \\# its default; "button =" unbinds it. [global] menu: the key that opens the
        \\# ROM menu from a game on the board's USB keyboard.
        \\
    );
    try w.writeAll("\n[global]\n");
    try writeLine(w, "menu", &c.menu);
    // only when off (the default is on): files without it stay byte for byte
    if (!c.splash_on_switch) try w.writeAll("splash_on_switch = off\n");
    // the frame advance keys only when not the defaults, likewise
    for ([_]struct { []const u8, *const Binding, Binding }{
        .{ "frame_advance", &c.frame_advance, default_frame_advance },
        .{ "frame_run", &c.frame_run, default_frame_run },
        .{ "screenshot", &c.screenshot, default_screenshot },
    }) |f| {
        const name, const b, const d = f;
        if (std.mem.eql(Input, b.slice(), d.slice())) continue;
        try writeLine(w, name, b);
    }
    for (&c.maps) |*m| {
        try w.writeAll("\n");
        try writeMap(w, m);
    }
}

/// The board's controls menu (side K, 2026-10-18): button i's keyboard key
/// made `key` alone (its pad inputs kept), and `key` taken off the
/// system's other buttons. False if the button's pad inputs leave no room.
pub fn setKey(m: *Map, i: usize, key: Input) bool {
    for (0..buttons(m.system).len) |j| m.bind[j].remove(key);
    m.bind[i].clearKeys();
    m.bind[i].add(key) catch return false;
    return true;
}

/// The first line of a key map the board wrote (the controls menu): make
/// sd and make sd-sync copy such a file into the desktop's, without this
/// line (`fromBoard`), so the next card written keeps the board's changes.
pub const board_mark = "# changed on the board (Options, Controls): make sd / sd-sync copy it to the desktop\n";

/// The whole config as the board writes it: `board_mark`, then `write`.
pub fn writeBoard(w: *std.Io.Writer, c: *const Config) std.Io.Writer.Error!void {
    try w.writeAll(board_mark);
    try write(w, c);
}

/// A file the board wrote, without its mark; null for any other.
pub fn fromBoard(text: []const u8) ?[]const u8 {
    if (!std.mem.startsWith(u8, text, board_mark)) return null;
    return text[board_mark.len..];
}

/// An input bound to more than one button of a system.
pub const Duplicate = struct { input: Input, first: usize, second: usize };

/// The first `out.len` inputs of `m` bound to two buttons.
pub fn duplicates(m: *const Map, out: []Duplicate) usize {
    var n: usize = 0;
    const nb = buttons(m.system).len;
    for (0..nb) |i| for (m.bind[i].slice()) |in| for (i + 1..nb) |j| {
        if (m.bind[j].has(in) and n < out.len) {
            out[n] = .{ .input = in, .first = i, .second = j };
            n += 1;
        }
    };
    return n;
}

/// A map compiled for the input loop: the key word for each input.
pub const Lookup = struct {
    mask: [input_count]Word = [_]Word{0} ** input_count,

    pub fn init(m: *const Map) Lookup {
        var l: Lookup = undefined;
        l.fill(m);
        return l;
    }

    /// In place (the PS app: with 32-bit masks a Lookup is 3.5 KiB, and a
    /// copy of it by value overflowed the stack check, M7's request).
    pub fn fill(l: *Lookup, m: *const Map) void {
        @memset(&l.mask, 0);
        for (buttons(m.system), 0..) |b, i| for (m.bind[i].slice()) |in| {
            l.mask[in] |= @as(Word, 1) << b.bit;
        };
    }
};

/// Which way an axis points: -1, 0 or 1 (past half way from the centre of
/// min..max towards that end; for a hat, -1..1, any non-zero value).
pub fn axisDir(value: i32, min: i32, max: i32) i2 {
    if (max <= min) return 0;
    const mid = @divTrunc(min + max, 2);
    const dead = @divTrunc(max - min, 4);
    if (value < mid - dead) return -1;
    if (value > mid + dead) return 1;
    return 0;
}

// ---------------------------------------------------------------------------

const testing = std.testing;

/// `link`'s compiled-in maps before side track B, copied verbatim (codes
/// from <linux/input-event-codes.h>): the defaults must equal them.
const old = struct {
    var system: enum { chip8, gb } = .chip8;

    fn keyboardKey(code: u16) ?u4 {
        if (system == .gb) return switch (code) {
            32 => 0, 30 => 1, 17 => 2, 31 => 3, 37 => 4, 36 => 5, 42 => 6, 38 => 7,
            else => null,
        };
        return switch (code) {
            2 => 0x1, 3 => 0x2, 4 => 0x3, 5 => 0xC,
            16 => 0x4, 17 => 0x5, 18 => 0x6, 19 => 0xD,
            30 => 0x7, 31 => 0x8, 32 => 0x9, 33 => 0xE,
            44 => 0xA, 45 => 0x0, 46 => 0xB, 47 => 0xF,
            103 => 0x2, 108 => 0x8, 105 => 0x4, 106 => 0x6,
            57 => 0x5,
            else => null,
        };
    }

    fn gamepadButton(code: u16) ?u4 {
        if (system == .gb) return switch (code) {
            0x131 => 4, 0x130 => 5, 0x13a => 6, 0x13b => 7,
            0x220 => 2, 0x221 => 3, 0x222 => 1, 0x223 => 0,
            else => null,
        };
        return switch (code) {
            0x130 => 0x5, 0x131 => 0xB, 0x134 => 0xC, 0x133 => 0x1,
            0x136 => 0xE, 0x137 => 0xF, 0x13b => 0xA, 0x13a => 0x0,
            0x220 => 0x2, 0x221 => 0x8, 0x222 => 0x4, 0x223 => 0x6,
            else => null,
        };
    }

    fn dir(comptime chip8_key: u4, comptime gb_bit: u4) u16 {
        return switch (system) {
            .chip8 => @as(u16, 1) << chip8_key,
            .gb => @as(u16, 1) << gb_bit,
        };
    }

    fn hat(x: i32, y: i32) u16 {
        var k: u16 = 0;
        if (x < 0) k |= dir(0x4, 1);
        if (x > 0) k |= dir(0x6, 0);
        if (y < 0) k |= dir(0x2, 2);
        if (y > 0) k |= dir(0x8, 3);
        return k;
    }

    fn stick(x: i32, y: i32, min: i32, max: i32) u16 {
        if (max <= min) return 0;
        const mid = @divTrunc(min + max, 2);
        const dead = @divTrunc(max - min, 4);
        var k: u16 = 0;
        if (x < mid - dead) k |= dir(0x4, 1);
        if (x > mid + dead) k |= dir(0x6, 0);
        if (y < mid - dead) k |= dir(0x2, 2);
        if (y > mid + dead) k |= dir(0x8, 3);
        return k;
    }
};

fn axesWord(l: *const Lookup, x: i32, y: i32, hx: i32, hy: i32, min: i32, max: i32) Word {
    var k: Word = 0;
    for ([_]struct { u6, i32, i32, i32 }{ .{ 0, x, min, max }, .{ 1, y, min, max }, .{ 0x10, hx, -1, 1 }, .{ 0x11, hy, -1, 1 } }) |a| {
        const d = axisDir(a[1], a[2], a[3]);
        if (d != 0) k |= l.mask[axisInput(a[0], d > 0)];
    }
    return k;
}

test "defaults equal the old compiled-in maps, for every system" {
    for ([_]System{ .chip8, .gb, .nes }) |s| {
        old.system = if (s == .chip8) .chip8 else .gb;
        const m = defaults(s);
        const l = Lookup.init(&m);
        // keyboards used keyboardKey, gamepads gamepadButton; the two never
        // overlapped (keys below 0x100, pad buttons above), so one table
        // for every device gives the same keys
        for (0..key_codes) |c| {
            const code: u16 = @intCast(c);
            const kb = old.keyboardKey(code);
            const gp = old.gamepadButton(code);
            try testing.expect(kb == null or gp == null);
            if (code < 0x100) try testing.expect(gp == null) else try testing.expect(kb == null);
            const want: Word = if (kb orelse gp) |k| @as(Word, 1) << k else 0;
            try testing.expectEqual(want, l.mask[code]);
        }
        // hat and stick, over a grid of positions
        var x: i32 = -1;
        while (x <= 256) : (x += 7) {
            var y: i32 = -1;
            while (y <= 256) : (y += 11) {
                const hx: i32 = @mod(x, 3) - 1;
                const hy: i32 = @mod(y, 3) - 1;
                try testing.expectEqual(old.hat(hx, hy) | old.stick(x, y, 0, 255), axesWord(&l, x, y, hx, hy, 0, 255));
                try testing.expectEqual(old.hat(hx, hy) | old.stick(x * 256 - 32768, y * 256 - 32768, -32768, 32767), axesWord(&l, x * 256 - 32768, y * 256 - 32768, hx, hy, -32768, 32767));
            }
        }
        // no other axis is bound
        for (0..axis_count) |a| if (a != 0 and a != 1 and a != 0x10 and a != 0x11) {
            try testing.expectEqual(@as(u16, 0), l.mask[axisInput(@intCast(a), false)] | l.mask[axisInput(@intCast(a), true)]);
        };
    }
}

test "the default file gives the defaults, and no default binds an input twice" {
    var buf: [8192]u8 = undefined;
    var w: std.Io.Writer = .fixed(&buf);
    const d = Config.initDefaults();
    try write(&w, &d);
    var e: ParseError = undefined;
    var c = try parse(w.buffered(), &e);
    for (std.enums.values(System)) |s| {
        try testing.expect(c.map(s).eql(&d.maps[@intFromEnum(s)]));
        var dup: [4]Duplicate = undefined;
        try testing.expectEqual(@as(usize, 0), duplicates(c.map(s), &dup));
    }
    // and an empty file too
    c = try parse("", &e);
    for (std.enums.values(System)) |s| try testing.expect(c.map(s).eql(&d.maps[@intFromEnum(s)]));
}

test "the controls menu: setKey moves a key to a button, keeps its pad inputs; the board's file, read back" {
    var c = Config.initDefaults();
    const gb = c.map(.gb);
    const a = buttonIndex(.gb, "a").?;
    const b = buttonIndex(.gb, "b").?;
    const key_k = parseInput("KEY_K").?;
    const key_q = parseInput("KEY_Q").?;
    // the defaults: a has K (and a pad button), b doesn't
    try testing.expect(gb.bind[a].has(key_k));
    const pads = for (gb.bind[a].slice()) |x| {
        if (isPad(x)) break true;
    } else false;
    try testing.expect(pads);
    // K to b: off a, a's pad inputs stay, b's keyboard keys replaced
    try testing.expect(setKey(gb, b, key_k));
    try testing.expect(!gb.bind[a].has(key_k));
    try testing.expect(gb.bind[b].has(key_k));
    for (gb.bind[b].slice()) |x| try testing.expect(x == key_k or isPad(x));
    for (gb.bind[a].slice()) |x| try testing.expect(isPad(x));
    try testing.expect(setKey(gb, a, key_q));
    var dup: [4]Duplicate = undefined;
    try testing.expectEqual(@as(usize, 0), duplicates(gb, &dup));
    // a button full of pad inputs: no room
    gb.bind[a] = .{};
    for (0..max_bindings) |i| try gb.bind[a].add(axisInput(@intCast(i), true));
    try testing.expect(!setKey(gb, a, key_q));
    gb.bind[a] = .{};
    try testing.expect(setKey(gb, a, key_q));
    // the board's file: marked, read back as written, under the PS app's
    // 4 KiB read buffer (relay.keymap_buf) with every system written
    var buf: [8192]u8 = undefined;
    var w: std.Io.Writer = .fixed(&buf);
    try writeBoard(&w, &c);
    const text = w.buffered();
    try testing.expect(text.len < 4096);
    try testing.expect(fromBoard("[gb]\na = KEY_K\n") == null);
    var e: ParseError = undefined;
    var back = try parse(text, &e);
    try testing.expect(back.map(.gb).eql(gb));
    back = try parse(fromBoard(text).?, &e);
    try testing.expect(back.map(.gb).eql(gb));
}

test "round trip of a changed map: write, parse, write again" {
    var c = Config.initDefaults();
    const gb = c.map(.gb);
    gb.bind[buttonIndex(.gb, "a").?] = .{};
    try gb.bind[buttonIndex(.gb, "a").?].add(parseInput("KEY_SPACE").?);
    try gb.bind[buttonIndex(.gb, "a").?].add(axisInput(0x29, true)); // an axis without a name
    try gb.bind[buttonIndex(.gb, "a").?].add(0x2FE); // a key without a name
    gb.bind[buttonIndex(.gb, "start").?] = .{};
    c.map(.chip8).bind[0] = Binding.of(&.{ "ABS_RZ+", "BTN_TRIGGER_HAPPY3" });
    var b1: [8192]u8 = undefined;
    var w1: std.Io.Writer = .fixed(&b1);
    try write(&w1, &c);
    var e: ParseError = undefined;
    var c2 = try parse(w1.buffered(), &e);
    for (std.enums.values(System)) |s| try testing.expect(c2.map(s).eql(c.map(s)));
    var b2: [8192]u8 = undefined;
    var w2: std.Io.Writer = .fixed(&b2);
    try write(&w2, &c2);
    try testing.expectEqualStrings(w1.buffered(), w2.buffered());
    try testing.expect(std.mem.indexOf(u8, w1.buffered(), "a = KEY_SPACE ABS_0x29+ 0x2FE\n") != null);
    try testing.expect(std.mem.indexOf(u8, w1.buffered(), "start =\n") != null);
}

test "a partial file: other buttons and systems keep their defaults; aliases" {
    var e: ParseError = undefined;
    var c = try parse(
        \\# my map
        \\[ nes ]
        \\  A = KEY_SPACE BTN_A   # BTN_A is BTN_SOUTH
        \\select=
        \\
    , &e);
    const d = Config.initDefaults();
    const nes = c.map(.nes);
    try testing.expectEqualSlices(Input, &.{ parseInput("KEY_SPACE").?, 0x130 }, nes.bind[buttonIndex(.nes, "a").?].slice());
    try testing.expectEqual(@as(u8, 0), nes.bind[buttonIndex(.nes, "select").?].n);
    try testing.expect(nes.bind[0].n == d.maps[2].bind[0].n);
    try testing.expect(c.map(.gb).eql(&d.maps[1]));
    try testing.expect(c.map(.chip8).eql(&d.maps[0]));
    var buf: [32]u8 = undefined;
    try testing.expectEqualStrings("BTN_SOUTH", formatInput(0x130, &buf));
}

test "every error, with its line" {
    const cases = [_]struct { text: []const u8, line: usize, msg: []const u8 }{
        .{ .text = "a = KEY_K\n", .line = 1, .msg = "before the first [system]" },
        .{ .text = "\n[n64]\n", .line = 2, .msg = "unknown system 'n64'" },
        .{ .text = "[gb\n", .line = 1, .msg = "a section is" },
        .{ .text = "[gb]\n\n# x\nturbo = KEY_T\n", .line = 4, .msg = "Game Boy has no button 'turbo'" },
        .{ .text = "[chip8]\ng = KEY_G\n", .line = 2, .msg = "CHIP-8 has no button 'g'" },
        .{ .text = "[gb]\na KEY_K\n", .line = 2, .msg = "expected 'button = inputs'" },
        .{ .text = "[gb]\na = KEY_KK\n", .line = 2, .msg = "unknown input 'KEY_KK'" },
        .{ .text = "[gb]\na = key_k\n", .line = 2, .msg = "unknown input 'key_k'" },
        .{ .text = "[gb]\na = ABS_NOPE+\n", .line = 2, .msg = "unknown input 'ABS_NOPE+'" },
        .{ .text = "[gb]\na = 0x300\n", .line = 2, .msg = "unknown input '0x300'" },
        .{ .text = "[gb]\na = KEY_K KEY_K\n", .line = 2, .msg = "'KEY_K' is listed twice" },
        .{ .text = "[gb]\na = KEY_K\n[nes]\na = KEY_K\n[gb]\nA = KEY_J\n", .line = 6, .msg = "button 'A' is given twice" },
        .{ .text = "[gb]\na = KEY_1 KEY_2 KEY_3 KEY_4 KEY_5 KEY_6 KEY_7 KEY_8 KEY_9\n", .line = 2, .msg = "more than 8 inputs" },
    };
    for (cases) |cs| {
        var e: ParseError = undefined;
        try testing.expectError(error.Invalid, parse(cs.text, &e));
        try testing.expectEqual(cs.line, e.line);
        if (std.mem.indexOf(u8, e.msg, cs.msg) == null) {
            std.debug.print("got '{s}', want '{s}'\n", .{ e.msg, cs.msg });
            return error.TestUnexpectedResult;
        }
    }
}

test "[global] frame_advance and frame_run: F11 and F10 by default, set, written only when changed" {
    var e: ParseError = undefined;
    var c = try parse("", &e);
    try testing.expect(c.frame_advance.has(parseInput("KEY_F11").?));
    try testing.expect(c.frame_run.has(parseInput("KEY_F10").?));
    c = try parse("[global]\nframe_advance = KEY_F8 KEY_KPPLUS\nframe_run =\n", &e);
    try testing.expect(c.frame_advance.has(parseInput("KEY_KPPLUS").?));
    try testing.expect(!c.frame_advance.has(parseInput("KEY_F11").?));
    try testing.expectEqual(@as(usize, 0), c.frame_run.slice().len);
    var buf: [8192]u8 = undefined;
    var w: std.Io.Writer = .fixed(&buf);
    try write(&w, &c);
    try testing.expect(std.mem.indexOf(u8, w.buffered(), "frame_advance = KEY_F8 KEY_KPPLUS\n") != null);
    try testing.expect(std.mem.indexOf(u8, w.buffered(), "frame_run =\n") != null);
    const back = try parse(w.buffered(), &e);
    try testing.expect(back.frame_advance.has(parseInput("KEY_F8").?));
    // the defaults leave the file as before
    var w2: std.Io.Writer = .fixed(&buf);
    try write(&w2, &Config.initDefaults());
    try testing.expect(std.mem.indexOf(u8, w2.buffered(), "frame_") == null);
    try testing.expectError(error.Invalid, parse("[global]\nframe_run = KEY_F1\nframe_run = KEY_F2\n", &e));
}

test "[global] menu: default Esc, set, unset, written and read back; its errors" {
    var e: ParseError = undefined;
    var c = try parse("", &e);
    try testing.expectEqualSlices(Input, &.{parseInput("KEY_ESC").?}, c.menu.slice());
    c = try parse("[global]\nmenu = KEY_F1 KEY_PAUSE\n[gb]\na = KEY_K\n", &e);
    try testing.expectEqualSlices(Input, &.{ parseInput("KEY_F1").?, parseInput("KEY_PAUSE").? }, c.menu.slice());
    c = try parse("[global]\nmenu =\n", &e);
    try testing.expectEqual(@as(u8, 0), c.menu.n);
    // round trip
    c.menu = Binding.of(&.{"KEY_HOME"});
    var buf: [8192]u8 = undefined;
    var w: std.Io.Writer = .fixed(&buf);
    try write(&w, &c);
    try testing.expect(std.mem.indexOf(u8, w.buffered(), "[global]\nmenu = KEY_HOME\n") != null);
    const back = try parse(w.buffered(), &e);
    try testing.expectEqualSlices(Input, c.menu.slice(), back.menu.slice());
    // errors
    try testing.expectError(error.Invalid, parse("[global]\nturbo = KEY_T\n", &e));
    try testing.expectEqual(@as(usize, 2), e.line);
    try testing.expectError(error.Invalid, parse("[global]\nmenu = KEY_A\nmenu = KEY_B\n", &e));
    try testing.expectEqual(@as(usize, 3), e.line);
    try testing.expectError(error.Invalid, parse("[global]\nmenu = KEY_NOPE\n", &e));
    // a [global] line doesn't count as a system section for what follows
    try testing.expectError(error.Invalid, parse("[global]\n[gb]\n[global]\na = KEY_K\n", &e));
    try testing.expectEqual(@as(usize, 4), e.line);
}

test "[global] splash_on_switch: on by default, off, written and read back; its errors" {
    var e: ParseError = undefined;
    var c = try parse("", &e);
    try testing.expect(c.splash_on_switch);
    var buf: [8192]u8 = undefined;
    var w: std.Io.Writer = .fixed(&buf);
    try write(&w, &c);
    try testing.expect(std.mem.indexOf(u8, w.buffered(), "splash") == null);
    c = try parse("[global]\nsplash_on_switch = Off\n", &e);
    try testing.expect(!c.splash_on_switch);
    w = .fixed(&buf);
    try write(&w, &c);
    try testing.expect(std.mem.indexOf(u8, w.buffered(), "\nsplash_on_switch = off\n") != null);
    try testing.expect(!(try parse(w.buffered(), &e)).splash_on_switch);
    c = try parse("[global]\nmenu = KEY_F1\nsplash_on_switch = on\n", &e);
    try testing.expect(c.splash_on_switch);
    try testing.expectError(error.Invalid, parse("[global]\nsplash_on_switch = yes\n", &e));
    try testing.expectEqual(@as(usize, 2), e.line);
    try testing.expectError(error.Invalid, parse("[global]\nsplash_on_switch = on\nsplash_on_switch = off\n", &e));
    try testing.expectEqual(@as(usize, 3), e.line);
}

test "duplicates within one system" {
    var e: ParseError = undefined;
    var c = try parse("[gb]\na = KEY_K KEY_J\n[chip8]\n1 = KEY_J\n", &e); // J is also b (default)
    var dup: [4]Duplicate = undefined;
    const n = duplicates(c.map(.gb), &dup);
    try testing.expectEqual(@as(usize, 1), n);
    try testing.expectEqual(parseInput("KEY_J").?, dup[0].input);
    try testing.expectEqualStrings("b", buttons(.gb)[dup[0].first].name);
    try testing.expectEqualStrings("a", buttons(.gb)[dup[0].second].name);
    try testing.expectEqual(@as(usize, 0), duplicates(c.map(.nes), &dup)); // other systems unaffected
    try testing.expectEqual(@as(usize, 0), duplicates(c.map(.chip8), &dup)); // J isn't bound in CHIP-8
    // the lookup ORs both buttons for a duplicated input
    const l = Lookup.init(c.map(.gb));
    try testing.expectEqual(@as(u16, 1 << 4 | 1 << 5), l.mask[parseInput("KEY_J").?]);
}

test "input names: pad or keyboard, generated table sane" {
    try testing.expect(isPad(parseInput("BTN_SOUTH").?));
    try testing.expect(isPad(parseInput("BTN_DPAD_UP").?));
    try testing.expect(isPad(parseInput("ABS_HAT0X-").?));
    try testing.expect(!isPad(parseInput("KEY_K").?));
    try testing.expect(!isPad(0x2FE));
    // one canonical name per code
    for (codes.keys, 0..) |a, i| for (codes.keys[i + 1 ..]) |b| {
        try testing.expect(!(a.canonical and b.canonical and a.code == b.code));
    };
    for (codes.keys) |k| try testing.expect(k.code > 0 and k.code < key_codes);
    for (codes.axes) |a| try testing.expect(a.code < axis_count);
    try testing.expectEqual(@as(?Input, 37), parseInput("KEY_K"));
    try testing.expectEqual(@as(?Input, axisInput(0x11, false)), parseInput("ABS_HAT0Y-"));
}

test "axis directions" {
    try testing.expectEqual(@as(i2, 0), axisDir(0, -1, 1));
    try testing.expectEqual(@as(i2, -1), axisDir(-1, -1, 1));
    try testing.expectEqual(@as(i2, 1), axisDir(255, 0, 255));
    try testing.expectEqual(@as(i2, 0), axisDir(150, 0, 255)); // inside the dead zone
    try testing.expectEqual(@as(i2, 0), axisDir(5, 0, 0)); // no range
}

test "parseInto into a Config already filled gives what parse gives (no leftovers)" {
    var perr: ParseError = undefined;
    const text = "[gg]\nup = KEY_I\n[sms]\nup2 = KEY_KP8\n";
    const want = try parse(text, &perr);
    var c: Config = undefined;
    try parseInto("[gb]\na = KEY_Q\n[sms]\npause = KEY_P\n[global]\nsplash_on_switch = off\n", &perr, &c);
    try parseInto(text, &perr, &c);
    try std.testing.expect(std.meta.eql(want, c));
}

test {
    _ = chip8_keys;
    _ = gb_keys;
    _ = a800xl_keys;
    _ = gg_keys;
    _ = sms_keys;
    _ = snes_keys;
}
