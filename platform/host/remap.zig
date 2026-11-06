//! The key-map file on disk and the remap utility (`make keymap SYSTEM=…`,
//! `link --remap`; side track B). Maps, defaults and the file format are
//! in keymap.zig.
//!
//! The utility shows the map in force, then asks for each button in turn.
//! Bindings come from the input devices (evdev, as `link` reads them, so
//! no window is needed); the controls come from the terminal: Enter ends
//! the button (alone: keep it as it is), Backspace forgets this button's
//! presses, Ctrl-C quits without writing. Keys pressed replace the
//! button's keyboard bindings, gamepad inputs (BTN_*, axes) its gamepad
//! bindings, so remapping the keyboard keeps the pad and the other way
//! round. Enter, keypad Enter and Backspace can't be bound here (only in
//! the file). An input already bound to another button of the system is
//! flagged at once and listed again before writing. The whole file is
//! rewritten (every system, every button; comments are not kept).

const std = @import("std");
const linux = std.os.linux;
const keymap = @import("keymap");
const evdev = @import("evdev.zig");
const sys = @import("sys.zig");

const KEY_BACKSPACE = 14;
const KEY_ENTER = 28;
const KEY_KPENTER = 96;

// --- the file ----------------------------------------------------------------

/// `$XDG_CONFIG_HOME/emulation_clash/keymap.conf`, or under `~/.config`.
pub fn defaultPath(env: *const std.process.Environ.Map, buf: []u8) ?[:0]const u8 {
    if (env.get("XDG_CONFIG_HOME")) |x| if (x.len > 0 and x[0] == '/')
        return std.fmt.bufPrintZ(buf, "{s}/emulation_clash/keymap.conf", .{x}) catch null;
    const home = env.get("HOME") orelse return null;
    return std.fmt.bufPrintZ(buf, "{s}/.config/emulation_clash/keymap.conf", .{home}) catch null;
}

pub const max_file = 64 * 1024;

/// The file's text, or null if there is none. Errors: unreadable or too big.
pub fn readFile(path: [*:0]const u8, buf: []u8) error{ Unreadable, TooBig }!?[]const u8 {
    const rc = linux.open(path, .{ .ACCMODE = .RDONLY, .CLOEXEC = true }, 0);
    switch (linux.errno(rc)) {
        .SUCCESS => {},
        .NOENT => return null,
        else => return error.Unreadable,
    }
    const fd: i32 = @intCast(rc);
    defer sys.close(fd);
    var n: usize = 0;
    while (true) {
        if (n == buf.len) return error.TooBig;
        const got = sys.read(fd, buf[n..]) catch return error.Unreadable;
        if (got == 0) break;
        n += got;
    }
    return buf[0..n];
}

/// Write `text` to `path` (through a temporary file and a rename),
/// creating the directories above it.
pub fn writeFile(path: [:0]const u8, text: []const u8) error{Failed}!void {
    var dir_buf: [std.fs.max_path_bytes]u8 = undefined;
    var i: usize = 1;
    while (std.mem.indexOfScalarPos(u8, path, i, '/')) |slash| : (i = slash + 1) {
        const dir = std.fmt.bufPrintZ(&dir_buf, "{s}", .{path[0..slash]}) catch return error.Failed;
        const rc = linux.mkdir(dir, 0o755);
        switch (linux.errno(rc)) {
            .SUCCESS, .EXIST => {},
            else => return error.Failed,
        }
    }
    var tmp_buf: [std.fs.max_path_bytes]u8 = undefined;
    const tmp = std.fmt.bufPrintZ(&tmp_buf, "{s}.new", .{path}) catch return error.Failed;
    const rc = linux.open(tmp, .{ .ACCMODE = .WRONLY, .CREAT = true, .TRUNC = true, .CLOEXEC = true }, 0o644);
    const fd: i32 = @intCast(sys.check(rc) catch return error.Failed);
    var rest = text;
    while (rest.len > 0) {
        const n = sys.write(fd, rest) catch {
            sys.close(fd);
            return error.Failed;
        };
        rest = rest[n..];
    }
    _ = linux.fsync(fd);
    sys.close(fd);
    _ = sys.check(linux.rename(tmp, path)) catch return error.Failed;
}

pub const Took = enum { not_board, same, copied };

/// The card's keymap.conf, `card` (side K, 2026-10-18: the board's
/// Controls page writes it, marked `keymap.board_mark`): if the board
/// wrote it, its text without the mark becomes the desktop's file at
/// `path` (the old one kept as `path.bak`), so the next card written
/// (make sd, make sd-sync) keeps the board's changes; nothing for a file
/// the board didn't write, or when they already agree. One that doesn't
/// parse is refused (error.Invalid, with `err` set).
pub fn takeBoard(path: [:0]const u8, card: []const u8, err: *keymap.ParseError) error{ Invalid, Unreadable, TooBig, Failed }!Took {
    const text = keymap.fromBoard(card) orelse return .not_board;
    _ = keymap.parse(text, err) catch return error.Invalid;
    var old_buf: [max_file]u8 = undefined;
    const old = try readFile(path, &old_buf);
    if (old) |o| {
        if (std.mem.eql(u8, o, text)) return .same;
        var bak_buf: [std.fs.max_path_bytes]u8 = undefined;
        const bak = std.fmt.bufPrintZ(&bak_buf, "{s}.bak", .{path}) catch return error.Failed;
        try writeFile(bak, o);
    }
    try writeFile(path, text);
    return .copied;
}

pub fn formatConfig(c: *const keymap.Config, buf: []u8) []const u8 {
    var w: std.Io.Writer = .fixed(buf);
    keymap.write(&w, c) catch unreachable; // 3 systems x 16 buttons x 8 names fit
    return w.buffered();
}

/// Print a system's map as the file has it, indented, then its duplicates.
pub fn show(m: *const keymap.Map) void {
    var buf: [4096]u8 = undefined;
    var w: std.Io.Writer = .fixed(&buf);
    keymap.writeMap(&w, m) catch unreachable;
    var lines = std.mem.splitScalar(u8, std.mem.trimEnd(u8, w.buffered(), "\n"), '\n');
    _ = lines.next(); // the [system] line
    while (lines.next()) |l| std.debug.print("  {s}\n", .{l});
    warnDuplicates(m);
}

pub fn warnDuplicates(m: *const keymap.Map) void {
    var dup: [32]keymap.Duplicate = undefined;
    const n = keymap.duplicates(m, &dup);
    const names = keymap.buttons(m.system);
    for (dup[0..n]) |d| {
        var nb: [32]u8 = undefined;
        std.debug.print("keymap: warning: {s} is bound to both {s} and {s} ({s})\n", .{ keymap.formatInput(d.input, &nb), names[d.first].name, names[d.second].name, m.system.title() });
    }
}

// --- the remap session (no I/O: tested with scripted events) -----------------

pub const Session = struct {
    map: keymap.Map,
    button: usize = 0,
    pending: keymap.Binding = .{},

    pub fn done(s: *const Session) bool {
        return s.button >= keymap.buttons(s.map.system).len;
    }

    pub fn current(s: *const Session) keymap.Button {
        return keymap.buttons(s.map.system)[s.button];
    }

    /// The input a device event newly presses, if any: a key or button
    /// going down (not a repeat), or an axis moving into a direction.
    /// Applies the event to the device.
    pub fn event(d: *evdev.Device, e: evdev.Event) ?keymap.Input {
        switch (e.type) {
            evdev.EV_KEY => {
                _ = d.apply(e);
                if (e.value != 1 or e.code >= keymap.key_codes) return null;
                return switch (e.code) {
                    KEY_ENTER, KEY_KPENTER, KEY_BACKSPACE => null, // the controls
                    else => e.code,
                };
            },
            evdev.EV_ABS => {
                if (e.code >= keymap.axis_count) return null;
                const a: u6 = @intCast(e.code);
                const before = d.axisDir(a);
                _ = d.apply(e);
                const after = d.axisDir(a);
                const rest = keymap.axisDir(d.abs_rest[a], d.abs_min[a], d.abs_max[a]);
                if (after == 0 or after == before or after == rest) return null;
                return keymap.axisInput(a, after > 0);
            },
            else => return null,
        }
    }

    pub const Press = enum { added, again, too_many };

    pub fn press(s: *Session, in: keymap.Input) Press {
        if (s.pending.has(in)) return .again;
        s.pending.add(in) catch return .too_many;
        return .added;
    }

    /// Another button of the system the input is bound to (in the map as
    /// it is so far), if any.
    pub fn boundElsewhere(s: *const Session, in: keymap.Input) ?[]const u8 {
        for (keymap.buttons(s.map.system), 0..) |b, i| {
            if (i != s.button and s.map.bind[i].has(in)) return b.name;
        }
        return null;
    }

    pub fn undo(s: *Session) void {
        s.pending = .{};
    }

    /// End this button: keyboard presses replace its keyboard bindings,
    /// pad presses its pad bindings (none: unchanged). Keyboard first in the
    /// result. Returns false if it had to drop inputs over the limit.
    pub fn confirm(s: *Session) bool {
        var all = true;
        if (s.pending.n > 0) s.map.bind[s.button] = merge(s.map.bind[s.button], s.pending, &all);
        s.pending = .{};
        s.button += 1;
        return all;
    }
};

/// A button's binding after presses: keyboard presses replace its keyboard
/// bindings, pad presses its pad bindings (keyboard first). `all` is
/// cleared if inputs over the limit were dropped. (Also the settings
/// screen's, settings.zig.)
pub fn merge(old: keymap.Binding, pending: keymap.Binding, all: *bool) keymap.Binding {
    var kb = false;
    var pad = false;
    for (pending.slice()) |in| {
        if (keymap.isPad(in)) pad = true else kb = true;
    }
    var b: keymap.Binding = .{};
    for ([_]bool{ false, true }) |want_pad| {
        const from = if ((want_pad and pad) or (!want_pad and kb)) pending.slice() else old.slice();
        for (from) |in| if (keymap.isPad(in) == want_pad) {
            b.add(in) catch {
                all.* = false;
            };
        };
    }
    return b;
}

// --- the interactive part ----------------------------------------------------

var stop = std.atomic.Value(bool).init(false);

fn onSignal(_: linux.SIG) callconv(.c) void {
    stop.store(true, .monotonic);
}

fn bindingText(b: *const keymap.Binding, buf: []u8) []const u8 {
    var w: std.Io.Writer = .fixed(buf);
    for (b.slice(), 0..) |in, i| {
        var nb: [32]u8 = undefined;
        w.print("{s}{s}", .{ if (i == 0) "" else " ", keymap.formatInput(in, &nb) }) catch break;
    }
    if (b.n == 0) w.writeAll("(nothing)") catch {};
    return w.buffered();
}

fn prompt(s: *const Session) void {
    var tb: [512]u8 = undefined;
    std.debug.print("keymap: [{d}/{d}] {s} = {s}\n", .{ s.button + 1, keymap.buttons(s.map.system).len, s.current().name, bindingText(&s.map.bind[s.button], &tb) });
}

pub const Terminal = struct {
    saved: ?linux.termios = null,

    /// stdin without echo and line editing (when it's a terminal), so the
    /// keys pressed for bindings aren't shown or left for the shell.
    pub fn raw() Terminal {
        var t: linux.termios = undefined;
        if (linux.errno(linux.tcgetattr(0, &t)) != .SUCCESS) return .{};
        var r = t;
        r.lflag.ICANON = false;
        r.lflag.ECHO = false;
        r.cc[@intFromEnum(linux.V.MIN)] = 1;
        r.cc[@intFromEnum(linux.V.TIME)] = 0;
        _ = linux.tcsetattr(0, .NOW, &r);
        return .{ .saved = t };
    }

    pub fn restore(t: *Terminal) void {
        const saved = t.saved orelse return;
        _ = sys.ioctl(0, linux.T.CFLSH, 0) catch {}; // TCIFLUSH: drop typed keys
        _ = linux.tcsetattr(0, .NOW, &saved);
    }
};

/// `link --remap`: remap `system` in `config` and write the whole config to
/// `path`. Returns false if the user quit without writing.
pub fn run(config: *keymap.Config, system: keymap.System, path: [:0]const u8, grab: bool, only: ?[]const u8) bool {
    const max_devices = 32;
    var devices: [max_devices]evdev.Device = undefined;
    var n_dev: usize = 0;
    for (0..64) |i| {
        if (n_dev == max_devices) break;
        if (evdev.openDevice(@intCast(i), grab, only)) |d| {
            devices[n_dev] = d;
            n_dev += 1;
            std.debug.print("keymap: + {s} ({s}, event{d})\n", .{ d.nameSlice(), @tagName(d.kind), i });
        }
    }
    defer for (devices[0..n_dev]) |*d| d.close();
    if (n_dev == 0) std.debug.print("keymap: no keyboard or gamepad readable in /dev/input (the input group?)\n", .{});

    var s: Session = .{ .map = config.map(system).* };
    std.debug.print("keymap: {s}, now:\n", .{system.title()});
    show(&s.map);
    std.debug.print(
        \\keymap: for each button press the keys and/or gamepad inputs to bind, then Enter.
        \\keymap: Enter alone keeps the button; Backspace forgets its presses; Ctrl-C quits without writing.
        \\keymap: keys replace its keyboard bindings, pad inputs its gamepad bindings.
        \\
    , .{});

    var act: linux.Sigaction = .{ .handler = .{ .handler = onSignal }, .mask = linux.sigemptyset(), .flags = 0 };
    _ = linux.sigaction(linux.SIG.INT, &act, null);
    _ = linux.sigaction(linux.SIG.TERM, &act, null);
    var term = Terminal.raw();
    defer term.restore();

    prompt(&s);
    var finishing = false; // all buttons asked: Enter writes
    while (!stop.load(.monotonic)) {
        var fds: [max_devices + 1]linux.pollfd = undefined;
        fds[0] = .{ .fd = 0, .events = linux.POLL.IN, .revents = 0 };
        for (devices[0..n_dev], 0..) |d, i| fds[i + 1] = .{ .fd = d.fd, .events = linux.POLL.IN, .revents = 0 };
        _ = sys.poll(fds[0 .. n_dev + 1], 200);

        for (devices[0..n_dev], 0..) |*d, i| {
            if (fds[i + 1].revents & linux.POLL.IN == 0) continue;
            var evs: [64]evdev.Event = undefined;
            while (true) {
                const n = sys.read(d.fd, std.mem.sliceAsBytes(&evs)) catch break;
                if (n == 0) break;
                for (evs[0 .. n / @sizeOf(evdev.Event)]) |e| {
                    const in = Session.event(d, e) orelse continue;
                    if (finishing) continue;
                    var nb: [32]u8 = undefined;
                    const name = keymap.formatInput(in, &nb);
                    switch (s.press(in)) {
                        .added => if (s.boundElsewhere(in)) |other|
                            std.debug.print("keymap:   + {s} (also bound to {s})\n", .{ name, other })
                        else
                            std.debug.print("keymap:   + {s}\n", .{name}),
                        .again => {},
                        .too_many => std.debug.print("keymap:   {s} ignored: {d} inputs at most\n", .{ name, keymap.max_bindings }),
                    }
                }
            }
        }

        if (fds[0].revents & (linux.POLL.IN | linux.POLL.HUP) != 0) {
            var b: [64]u8 = undefined;
            const n = sys.read(0, &b) catch 0;
            if (n == 0) {
                std.debug.print("keymap: end of input; nothing written\n", .{});
                return false;
            }
            for (b[0..n]) |c| switch (c) {
                '\r', '\n' => {
                    if (finishing) {
                        config.map(system).* = s.map;
                        var fb: [32 * 1024]u8 = undefined;
                        writeFile(path, formatConfig(config, &fb)) catch {
                            std.debug.print("keymap: can't write {s}\n", .{path});
                            return false;
                        };
                        std.debug.print("keymap: written to {s}\n", .{path});
                        return true;
                    }
                    var tb: [512]u8 = undefined;
                    const name = s.current().name;
                    const changed = s.pending.n > 0;
                    if (!s.confirm()) std.debug.print("keymap:   more than {d} inputs: the last ones dropped\n", .{keymap.max_bindings});
                    std.debug.print("keymap:   {s} {s} {s}\n", .{ name, if (changed) "->" else "kept:", bindingText(&s.map.bind[s.button - 1], &tb) });
                    if (s.done()) {
                        finishing = true;
                        std.debug.print("keymap: {s}, new:\n", .{system.title()});
                        show(&s.map);
                        std.debug.print("keymap: Enter writes {s}; Ctrl-C quits without writing\n", .{path});
                    } else prompt(&s);
                },
                0x7f, 0x08 => if (!finishing) {
                    s.undo();
                    std.debug.print("keymap:   (presses forgotten)\n", .{});
                },
                else => {},
            };
        }
    }
    std.debug.print("\nkeymap: quit; nothing written\n", .{});
    return false;
}

// ---------------------------------------------------------------------------

const testing = std.testing;

const Script = struct {
    s: Session,
    kb: evdev.Device = .{ .fd = -1, .kind = .keyboard, .index = 0 },
    pad: evdev.Device,
    t: i64 = 0,

    fn init(m: keymap.Map) Script {
        var pad: evdev.Device = .{ .fd = -1, .kind = .gamepad, .index = 1, .axes = 1 << 0 | 1 << 1 | 1 << 0x10 | 1 << 0x11 | 1 << 2 };
        for ([_]u6{ 0, 1, 2 }) |a| {
            pad.abs_min[a] = 0;
            pad.abs_max[a] = 255;
            pad.abs_value[a] = 128;
            pad.abs_rest[a] = 128;
        }
        pad.abs_value[2] = 0; // a trigger, resting at its minimum
        pad.abs_rest[2] = 0;
        return .{ .s = .{ .map = m }, .pad = pad };
    }

    fn ev(sc: *Script, d: *evdev.Device, ty: u16, code: u16, value: i32) void {
        sc.t += 1;
        if (Session.event(d, .{ .sec = 0, .usec = sc.t, .type = ty, .code = code, .value = value })) |in| _ = sc.s.press(in);
    }
    fn key(sc: *Script, code: u16) void {
        sc.ev(&sc.kb, evdev.EV_KEY, code, 1);
        sc.ev(&sc.kb, evdev.EV_KEY, code, 2); // auto-repeat
        sc.ev(&sc.kb, evdev.EV_KEY, code, 0);
    }
    fn btn(sc: *Script, code: u16) void {
        sc.ev(&sc.pad, evdev.EV_KEY, code, 1);
        sc.ev(&sc.pad, evdev.EV_KEY, code, 0);
    }
    fn axis(sc: *Script, code: u16, value: i32, rest: i32) void {
        sc.ev(&sc.pad, evdev.EV_ABS, code, value);
        sc.ev(&sc.pad, evdev.EV_ABS, code, rest);
    }
};

fn input(comptime name: []const u8) keymap.Input {
    return comptime keymap.parseInput(name).?;
}

test "remap flow, scripted: keep, keyboard only, pad only, both, undo, axes, controls" {
    var sc = Script.init(keymap.defaults(.gb));
    const d = keymap.defaults(.gb);
    // up: Enter alone keeps it
    _ = sc.s.confirm();
    // down: arrow key replaces only the keyboard part (KEY_S)
    sc.key(input("KEY_DOWN"));
    try testing.expect(sc.s.confirm());
    // left: a stick push (ABS_X-) and a second press of it: pad part replaced
    sc.axis(0, 10, 128);
    sc.axis(0, 5, 128);
    _ = sc.s.confirm();
    // right: the hat, then Backspace (undo), then nothing: unchanged
    sc.axis(0x10, 1, 0);
    sc.s.undo();
    _ = sc.s.confirm();
    // b: Enter and Backspace keys themselves are never bound; J kept
    sc.key(KEY_ENTER);
    sc.key(KEY_BACKSPACE);
    _ = sc.s.confirm();
    // a: keyboard and pad at once
    sc.key(input("KEY_SPACE"));
    sc.btn(input("BTN_SOUTH"));
    sc.axis(2, 255, 0); // the trigger pressed (ABS_Z+); resting at 0 never counts as ABS_Z-
    _ = sc.s.confirm();
    // start, select: kept
    _ = sc.s.confirm();
    try testing.expect(!sc.s.done());
    _ = sc.s.confirm();
    try testing.expect(sc.s.done());

    const m = &sc.s.map;
    try testing.expect(std.mem.eql(keymap.Input, d.bind[0].slice(), m.bind[0].slice()));
    try testing.expectEqualSlices(keymap.Input, &.{ input("KEY_DOWN"), input("BTN_DPAD_DOWN"), input("ABS_HAT0Y+"), input("ABS_Y+") }, m.bind[1].slice());
    try testing.expectEqualSlices(keymap.Input, &.{ input("KEY_A"), input("ABS_X-") }, m.bind[2].slice());
    try testing.expect(std.mem.eql(keymap.Input, d.bind[3].slice(), m.bind[3].slice()));
    try testing.expect(std.mem.eql(keymap.Input, d.bind[4].slice(), m.bind[4].slice()));
    try testing.expectEqualSlices(keymap.Input, &.{ input("KEY_SPACE"), input("BTN_SOUTH"), input("ABS_Z+") }, m.bind[5].slice());
    // BTN_SOUTH is still b's too: a duplicate, reported
    var dup: [4]keymap.Duplicate = undefined;
    try testing.expectEqual(@as(usize, 1), keymap.duplicates(m, &dup));
    try testing.expectEqual(input("BTN_SOUTH"), dup[0].input);
}

test "remap: pressed input flagged when another button has it; too many" {
    var sc = Script.init(keymap.defaults(.chip8));
    sc.key(input("KEY_X")); // button "1" first; X is 0's
    try testing.expectEqualStrings("0", sc.s.boundElsewhere(input("KEY_X")).?);
    try testing.expectEqual(@as(?[]const u8, null), sc.s.boundElsewhere(input("KEY_1")));
    sc.s.undo();
    for (0..keymap.max_bindings) |i| try testing.expectEqual(Session.Press.added, sc.s.press(@intCast(30 + i)));
    try testing.expectEqual(Session.Press.too_many, sc.s.press(60));
    try testing.expectEqual(Session.Press.again, sc.s.press(30));
    // 8 new keys + the old BTN_NORTH: one over, dropped
    try testing.expect(!sc.s.confirm());
    try testing.expectEqual(@as(u8, keymap.max_bindings), sc.s.map.bind[0].n);
}

test "file: default path, write and read back" {
    var arena: std.heap.ArenaAllocator = .init(testing.allocator);
    defer arena.deinit();
    var env: std.process.Environ.Map = .init(arena.allocator());
    var pb: [256]u8 = undefined;
    try testing.expectEqual(@as(?[:0]const u8, null), defaultPath(&env, &pb));
    try env.put("HOME", "/home/u");
    try testing.expectEqualStrings("/home/u/.config/emulation_clash/keymap.conf", defaultPath(&env, &pb).?);
    try env.put("XDG_CONFIG_HOME", "relative"); // not absolute: ignored, as the spec says
    try testing.expectEqualStrings("/home/u/.config/emulation_clash/keymap.conf", defaultPath(&env, &pb).?);
    try env.put("XDG_CONFIG_HOME", "/x/cfg");
    try testing.expectEqualStrings("/x/cfg/emulation_clash/keymap.conf", defaultPath(&env, &pb).?);

    // a fresh directory under the test's tmp (zig test runs in the cwd)
    const path = "build/keymap-test/a/b/keymap.conf";
    var c = keymap.Config.initDefaults();
    c.map(.nes).bind[0] = .{};
    var fb: [32 * 1024]u8 = undefined;
    try writeFile(path, formatConfig(&c, &fb));
    var rb: [max_file]u8 = undefined;
    const text = (try readFile(path, &rb)).?;
    var e: keymap.ParseError = undefined;
    var back = try keymap.parse(text, &e);
    try testing.expect(back.map(.nes).eql(c.map(.nes)));
    try testing.expectEqual(@as(?[]const u8, null), try readFile("build/keymap-test/none.conf", &rb));
}

test "takeBoard: the board's file copied (the old one kept as .bak), others and the same left alone, a bad one refused" {
    const path = "build/keymap-test/take/keymap.conf";
    const bak = "build/keymap-test/take/keymap.conf.bak";
    _ = linux.unlink(path);
    _ = linux.unlink(bak);
    var e: keymap.ParseError = undefined;
    var c = keymap.Config.initDefaults();
    _ = keymap.setKey(c.map(.gb), keymap.buttonIndex(.gb, "a").?, keymap.parseInput("KEY_Q").?);
    var fb: [32 * 1024]u8 = undefined;
    var w: std.Io.Writer = .fixed(&fb);
    try keymap.writeBoard(&w, &c);
    const card = w.buffered();
    // the desktop's own file (not the board's) stays put
    try testing.expectEqual(Took.not_board, try takeBoard(path, "[gb]\na = KEY_Z\n", &e));
    try testing.expectEqual(@as(?[]const u8, null), try readFile(path, fb[card.len..]));
    // none on the desktop yet: copied, without the mark
    try testing.expectEqual(Took.copied, try takeBoard(path, card, &e));
    var rb: [max_file]u8 = undefined;
    try testing.expectEqualStrings(keymap.fromBoard(card).?, (try readFile(path, &rb)).?);
    try testing.expectEqual(Took.same, try takeBoard(path, card, &e));
    // a desktop edit since: replaced by the board's, kept as .bak
    try writeFile(path, "[gb]\na = KEY_Z\n");
    try testing.expectEqual(Took.copied, try takeBoard(path, card, &e));
    try testing.expectEqualStrings("[gb]\na = KEY_Z\n", (try readFile(bak, &rb)).?);
    // a damaged one: refused, the desktop's untouched
    try testing.expectError(error.Invalid, takeBoard(path, keymap.board_mark ++ "[gb]\nnope = KEY_Z\n", &e));
    try testing.expectEqualStrings(keymap.fromBoard(card).?, (try readFile(path, &rb)).?);
}
