//! The settings screen (`make settings`, `link --settings`; the user's
//! request, 2026-10-22): a small full-screen terminal UI over the key-map
//! file (keymap.zig's format, remap.zig's file helpers), for every system
//! and the `[global]` settings, in place of editing the file by hand.
//!
//! Two pages: a system's buttons (Left/Right picks the system) and the
//! global settings (Tab switches). Up/Down move; Enter on a button starts a
//! capture: press the keys and/or gamepad inputs (read from the input
//! devices, as `link --remap` does), then Enter. Keys pressed replace the
//! button's keyboard bindings, pad inputs its gamepad bindings
//! (remap.merge); Backspace cancels the capture. On a button: `d` clears
//! it, `r` resets the whole system to its defaults. On the global page,
//! Space toggles `splash_on_switch`. `s` writes the file, `q` quits (twice
//! with changes not written). A button whose input another button of the
//! same system also has is marked, with the other's name. The whole file
//! is rewritten on `s` (every system; comments are not kept, as with
//! --remap). Enter, keypad Enter and Backspace can't be bound here.

const std = @import("std");
const linux = std.os.linux;
const keymap = @import("keymap");
const evdev = @import("evdev.zig");
const remap = @import("remap.zig");
const sys = @import("sys.zig");

/// The rows of the global page.
pub const Global = enum { menu, frame_advance, frame_run, screenshot, splash_on_switch };

pub const Page = enum { buttons, global };

/// The screen's state (no I/O: tested with scripted keys and presses).
pub const Ui = struct {
    config: keymap.Config,
    system: keymap.System,
    page: Page = .buttons,
    cursor: usize = 0,
    /// a capture in progress: the presses so far
    capture: ?keymap.Binding = null,
    dirty: bool = false,
    /// `q` once with changes not written
    quit_asked: bool = false,
    note: []const u8 = "",

    pub fn rows(u: *const Ui) usize {
        return switch (u.page) {
            .buttons => keymap.buttons(u.system).len,
            .global => std.enums.values(Global).len,
        };
    }

    /// The binding the cursor is on (none: the splash toggle).
    pub fn binding(u: *Ui) ?*keymap.Binding {
        return switch (u.page) {
            .buttons => &u.config.map(u.system).bind[u.cursor],
            .global => switch (@as(Global, @enumFromInt(u.cursor))) {
                .menu => &u.config.menu,
                .frame_advance => &u.config.frame_advance,
                .frame_run => &u.config.frame_run,
                .screenshot => &u.config.screenshot,
                .splash_on_switch => null,
            },
        };
    }

    pub fn move(u: *Ui, down: bool) void {
        const n = u.rows();
        u.cursor = if (down) (u.cursor + 1) % n else (u.cursor + n - 1) % n;
    }

    pub fn nextSystem(u: *Ui, forward: bool) void {
        const all = std.enums.values(keymap.System);
        const i = @intFromEnum(u.system);
        u.system = all[if (forward) (i + 1) % all.len else (i + all.len - 1) % all.len];
        u.cursor = 0;
    }

    pub fn switchPage(u: *Ui) void {
        u.page = if (u.page == .buttons) .global else .buttons;
        u.cursor = 0;
    }

    pub fn startCapture(u: *Ui) void {
        if (u.binding() == null) {
            u.toggleSplash();
            return;
        }
        u.capture = .{};
        u.note = "press keys and/or gamepad inputs, then Enter (Backspace cancels)";
    }

    /// An input pressed on a device while capturing.
    pub fn press(u: *Ui, in: keymap.Input) void {
        if (u.capture) |*c| c.add(in) catch {
            u.note = "too many inputs for one button: the rest ignored";
        };
    }

    pub fn finishCapture(u: *Ui) void {
        const c = u.capture orelse return;
        u.capture = null;
        if (c.n == 0) {
            u.note = "nothing pressed: unchanged";
            return;
        }
        const b = u.binding().?;
        var all = true;
        b.* = remap.merge(b.*, c, &all);
        u.dirty = true;
        u.note = if (all) "changed (s writes the file)" else "changed; inputs over the limit dropped";
    }

    pub fn cancelCapture(u: *Ui) void {
        u.capture = null;
        u.note = "cancelled";
    }

    pub fn clear(u: *Ui) void {
        const b = u.binding() orelse return;
        b.* = .{};
        u.dirty = true;
        u.note = "cleared (s writes the file)";
    }

    pub fn resetSystem(u: *Ui) void {
        if (u.page != .buttons) return;
        u.config.map(u.system).* = keymap.defaults(u.system);
        u.dirty = true;
        u.note = "the system's defaults (s writes the file)";
    }

    pub fn toggleSplash(u: *Ui) void {
        u.config.splash_on_switch = !u.config.splash_on_switch;
        u.dirty = true;
        u.note = "changed (s writes the file)";
    }

    /// Another button of this system sharing an input with button `i`.
    pub fn clash(u: *Ui, i: usize) ?[]const u8 {
        const m = u.config.map(u.system);
        const names = keymap.buttons(u.system);
        for (m.bind[i].slice()) |in| {
            for (names, 0..) |b, j| {
                if (j != i and m.bind[j].has(in)) return b.name;
            }
        }
        return null;
    }

    /// A key from the terminal (`Key`), when not capturing; true: quit.
    pub fn key(u: *Ui, k: Key) bool {
        if (u.capture != null) {
            switch (k) {
                .enter => u.finishCapture(),
                .backspace => u.cancelCapture(),
                else => {},
            }
            return false;
        }
        if (k != .quit) u.quit_asked = false;
        switch (k) {
            .up => u.move(false),
            .down => u.move(true),
            .left => if (u.page == .buttons) u.nextSystem(false),
            .right => if (u.page == .buttons) u.nextSystem(true),
            .tab => u.switchPage(),
            .enter => u.startCapture(),
            .space => if (u.page == .global and u.binding() == null) u.toggleSplash(),
            .clear => u.clear(),
            .reset => u.resetSystem(),
            .quit => {
                if (!u.dirty or u.quit_asked) return true;
                u.quit_asked = true;
                u.note = "changes not written: q again quits without writing, s writes";
            },
            .save, .backspace, .none => {},
        }
        return false;
    }
};

pub const Key = enum { up, down, left, right, tab, enter, space, backspace, clear, reset, save, quit, none };

/// The terminal's bytes as keys (arrows as ESC [ A..D).
pub fn keys(bytes: []const u8, out: []Key) usize {
    var n: usize = 0;
    var i: usize = 0;
    while (i < bytes.len and n < out.len) : (i += 1) {
        const c = bytes[i];
        const k: Key = switch (c) {
            0x1b => blk: {
                if (i + 2 < bytes.len and bytes[i + 1] == '[') {
                    i += 2;
                    break :blk switch (bytes[i]) {
                        'A' => .up,
                        'B' => .down,
                        'C' => .right,
                        'D' => .left,
                        else => .none,
                    };
                }
                break :blk .none;
            },
            '\t' => .tab,
            '\r', '\n' => .enter,
            ' ' => .space,
            0x7f, 0x08 => .backspace,
            'd' => .clear,
            'r' => .reset,
            's' => .save,
            'q', 0x03 => .quit,
            'k' => .up,
            'j' => .down,
            'h' => .left,
            'l' => .right,
            else => .none,
        };
        if (k != .none) {
            out[n] = k;
            n += 1;
        }
    }
    return n;
}

fn bindingText(b: *const keymap.Binding, w: *std.Io.Writer) void {
    for (b.slice(), 0..) |in, i| {
        var nb: [32]u8 = undefined;
        w.print("{s}{s}", .{ if (i == 0) "" else " ", keymap.formatInput(in, &nb) }) catch return;
    }
    if (b.n == 0) w.writeAll("(nothing)") catch {};
}

/// The whole screen into `w` (ANSI: home, clear, reverse video on the cursor).
pub fn render(u: *Ui, path: []const u8, w: *std.Io.Writer) void {
    w.writeAll("\x1b[H\x1b[2J") catch return;
    w.print("emulation_clash settings   {s}{s}\r\n", .{ path, if (u.dirty) "  (changed, not written)" else "" }) catch return;
    switch (u.page) {
        .buttons => {
            w.writeAll("[Tab] global settings   ") catch return;
            for (std.enums.values(keymap.System)) |s| {
                if (s == u.system) w.print("\x1b[7m {s} \x1b[0m", .{s.title()}) catch return else w.print(" {s} ", .{s.title()}) catch return;
            }
            w.writeAll("\r\n\r\n") catch return;
            for (keymap.buttons(u.system), 0..) |b, i| {
                const on = i == u.cursor;
                w.print("{s}{s:<8}\x1b[0m ", .{ if (on) "\x1b[7m" else "", b.name }) catch return;
                if (on and u.capture != null) {
                    w.writeAll("\x1b[1m> ") catch return;
                    bindingText(&u.capture.?, w);
                    w.writeAll("\x1b[0m") catch return;
                } else bindingText(&u.config.map(u.system).bind[i], w);
                if (u.clash(i)) |other| w.print("   \x1b[33m! also {s}\x1b[0m", .{other}) catch return;
                w.writeAll("\r\n") catch return;
            }
        },
        .global => {
            w.writeAll("[Tab] the systems' buttons   global settings\r\n\r\n") catch return;
            for (std.enums.values(Global), 0..) |g, i| {
                const on = i == u.cursor;
                w.print("{s}{s:<16}\x1b[0m ", .{ if (on) "\x1b[7m" else "", @tagName(g) }) catch return;
                switch (g) {
                    .splash_on_switch => w.writeAll(if (u.config.splash_on_switch) "on" else "off") catch return,
                    else => {
                        const saved = u.cursor;
                        u.cursor = i;
                        const b = u.binding().?;
                        u.cursor = saved;
                        if (on and u.capture != null) {
                            w.writeAll("\x1b[1m> ") catch return;
                            bindingText(&u.capture.?, w);
                            w.writeAll("\x1b[0m") catch return;
                        } else bindingText(b, w);
                    },
                }
                w.writeAll("\r\n") catch return;
            }
        },
    }
    w.writeAll("\r\n") catch return;
    w.print("\x1b[2m{s}\x1b[0m\r\n", .{u.note}) catch return;
    w.writeAll("\x1b[2mUp/Down move  Left/Right system  Enter rebind  d clear  r system defaults  Space toggle  s write  q quit\x1b[0m\r\n") catch return;
}

var stop = std.atomic.Value(bool).init(false);

fn onSignal(_: linux.SIG) callconv(.c) void {
    stop.store(true, .monotonic);
}

/// `link --settings`: the screen over `config` (read from `path`, or the
/// defaults), starting at `system`. Returns false on an error writing.
pub fn run(config: keymap.Config, system: keymap.System, path: [:0]const u8, only: ?[]const u8) bool {
    const max_devices = 32;
    var devices: [max_devices]evdev.Device = undefined;
    var n_dev: usize = 0;
    for (0..64) |i| {
        if (n_dev == max_devices) break;
        if (evdev.openDevice(@intCast(i), false, only)) |d| {
            devices[n_dev] = d;
            n_dev += 1;
        }
    }
    defer for (devices[0..n_dev]) |*d| d.close();

    var u: Ui = .{ .config = config, .system = system };
    if (n_dev == 0) u.note = "no keyboard or gamepad readable in /dev/input (the input group?): rebinding needs one";

    var act: linux.Sigaction = .{ .handler = .{ .handler = onSignal }, .mask = linux.sigemptyset(), .flags = 0 };
    _ = linux.sigaction(linux.SIG.INT, &act, null);
    _ = linux.sigaction(linux.SIG.TERM, &act, null);
    var term = remap.Terminal.raw();
    defer {
        term.restore();
        _ = sys.write(1, "\x1b[0m\r\n") catch {};
    }

    var ok = true;
    var screen: [16 * 1024]u8 = undefined;
    while (!stop.load(.monotonic)) {
        var w: std.Io.Writer = .fixed(&screen);
        render(&u, path, &w);
        _ = sys.write(1, w.buffered()) catch {};

        var fds: [max_devices + 1]linux.pollfd = undefined;
        fds[0] = .{ .fd = 0, .events = linux.POLL.IN, .revents = 0 };
        for (devices[0..n_dev], 0..) |d, i| fds[i + 1] = .{ .fd = d.fd, .events = linux.POLL.IN, .revents = 0 };
        _ = sys.poll(fds[0 .. n_dev + 1], 500);

        // the devices: always read (their state stays current), presses
        // kept while capturing
        for (devices[0..n_dev], 0..) |*d, i| {
            if (fds[i + 1].revents & linux.POLL.IN == 0) continue;
            var evs: [64]evdev.Event = undefined;
            while (true) {
                const n = sys.read(d.fd, std.mem.sliceAsBytes(&evs)) catch break;
                if (n == 0) break;
                for (evs[0 .. n / @sizeOf(evdev.Event)]) |e| {
                    const in = remap.Session.event(d, e) orelse continue;
                    if (u.capture != null) u.press(in);
                }
            }
        }

        if (fds[0].revents & (linux.POLL.IN | linux.POLL.HUP) != 0) {
            var b: [64]u8 = undefined;
            const n = sys.read(0, &b) catch 0;
            if (n == 0) break;
            var ks: [64]Key = undefined;
            for (ks[0..keys(b[0..n], &ks)]) |k| {
                if (k == .save and u.capture == null) {
                    var fb: [32 * 1024]u8 = undefined;
                    if (remap.writeFile(path, remap.formatConfig(&u.config, &fb))) {
                        u.dirty = false;
                        u.note = "written";
                    } else |_| {
                        u.note = "can't write the file";
                        ok = false;
                    }
                    continue;
                }
                if (u.key(k)) return ok;
            }
        }
    }
    return ok;
}

// ---------------------------------------------------------------------------

const testing = std.testing;

fn input(comptime name: []const u8) keymap.Input {
    return comptime keymap.parseInput(name).?;
}

test "the terminal's bytes as keys" {
    var ks: [8]Key = undefined;
    const n = keys("\x1b[A\x1b[Bx\td\r q", &ks);
    try testing.expectEqualSlices(Key, &.{ .up, .down, .tab, .clear, .enter, .space, .quit }, ks[0..n]);
}

test "rebinding a button: keys replace the keys, the pad stays" {
    var u: Ui = .{ .config = keymap.Config.initDefaults(), .system = .snes };
    const b = keymap.buttonIndex(.snes, "b").?;
    u.cursor = b;
    _ = u.key(.enter);
    u.press(input("KEY_KP5"));
    _ = u.key(.enter);
    const got = u.config.map(.snes).bind[b];
    try testing.expect(got.has(input("KEY_KP5")));
    try testing.expect(!got.has(input("KEY_J")));
    try testing.expect(got.has(input("BTN_SOUTH")));
    try testing.expect(u.dirty);
}

test "a clash is shown, a capture cancels, a system resets" {
    var u: Ui = .{ .config = keymap.Config.initDefaults(), .system = .snes };
    const b = keymap.buttonIndex(.snes, "b").?;
    try testing.expect(u.clash(b) == null);
    u.cursor = b;
    _ = u.key(.enter);
    u.press(input("KEY_K")); // A's key
    _ = u.key(.enter);
    try testing.expectEqualStrings("a", u.clash(b).?);
    _ = u.key(.enter);
    u.press(input("KEY_X"));
    _ = u.key(.backspace);
    try testing.expect(u.config.map(.snes).bind[b].has(input("KEY_K")));
    _ = u.key(.reset);
    try testing.expect(u.config.map(.snes).eql(&keymap.defaults(.snes)));
}

test "the global page: the splash toggles, the menu key rebinds" {
    var u: Ui = .{ .config = keymap.Config.initDefaults(), .system = .gb };
    _ = u.key(.tab);
    try testing.expectEqual(Page.global, u.page);
    u.cursor = @intFromEnum(Global.splash_on_switch);
    _ = u.key(.space);
    try testing.expect(!u.config.splash_on_switch);
    u.cursor = @intFromEnum(Global.menu);
    _ = u.key(.enter);
    u.press(input("KEY_F1"));
    _ = u.key(.enter);
    try testing.expect(u.config.menu.has(input("KEY_F1")));
    try testing.expect(!u.config.menu.has(input("KEY_ESC")));
}

test "q with changes not written asks once" {
    var u: Ui = .{ .config = keymap.Config.initDefaults(), .system = .nes };
    try testing.expect(!u.key(.clear) and u.dirty);
    try testing.expect(!u.key(.quit));
    try testing.expect(u.key(.quit));
    var v: Ui = .{ .config = keymap.Config.initDefaults(), .system = .nes };
    try testing.expect(v.key(.quit));
}

test "systems cycle both ways" {
    var u: Ui = .{ .config = keymap.Config.initDefaults(), .system = .chip8 };
    _ = u.key(.left);
    try testing.expectEqual(std.enums.values(keymap.System)[std.enums.values(keymap.System).len - 1], u.system);
    _ = u.key(.right);
    try testing.expectEqual(keymap.System.chip8, u.system);
}

test "the screen renders every system's page" {
    var u: Ui = .{ .config = keymap.Config.initDefaults(), .system = .chip8 };
    var buf: [16 * 1024]u8 = undefined;
    for (std.enums.values(keymap.System)) |s| {
        u.system = s;
        var w: std.Io.Writer = .fixed(&buf);
        render(&u, "keymap.conf", &w);
        try testing.expect(std.mem.indexOf(u8, w.buffered(), s.title()) != null);
    }
    _ = u.key(.tab);
    var w: std.Io.Writer = .fixed(&buf);
    render(&u, "keymap.conf", &w);
    try testing.expect(std.mem.indexOf(u8, w.buffered(), "splash_on_switch") != null);
}
