//! Linux input devices (/dev/input/event*): keyboards and gamepads, read
//! directly. Unlike a window toolkit this needs no focus and reports key
//! repeats explicitly (value 2), so press and release are exact; every
//! event carries a kernel timestamp. Needs read access to /dev/input
//! (the `input` group).

const std = @import("std");
const linux = std.os.linux;
const sys = @import("sys.zig");
const keymap = @import("keymap");

pub const Event = extern struct {
    sec: i64,
    usec: i64,
    type: u16,
    code: u16,
    value: i32,
};

pub const EV_SYN = 0;
pub const EV_KEY = 1;
pub const EV_ABS = 3;
const ABS_HAT0X = 0x10;
const ABS_HAT3Y = 0x17;

fn ioc(dir: u32, nr: u32, size: u32) u32 {
    return (dir << 30) | (size << 16) | (@as(u32, 'E') << 8) | nr;
}
fn eviocgbit(ev: u32, len: u32) u32 {
    return ioc(2, 0x20 + ev, len);
}
fn eviocgname(len: u32) u32 {
    return ioc(2, 0x06, len);
}
fn eviocgabs(abs: u32) u32 {
    return ioc(2, 0x40 + abs, 24);
}
const EVIOCGRAB = ioc(1, 0x90, 4);

const AbsInfo = extern struct { value: i32, min: i32, max: i32, fuzz: i32, flat: i32, res: i32 };

pub const Kind = enum { keyboard, gamepad };

pub const Device = struct {
    fd: i32,
    kind: Kind,
    index: u8,
    name: [80]u8 = [_]u8{0} ** 80,
    held: std.StaticBitSet(keymap.key_codes) = .initEmpty(),
    /// Gamepads only: the axes followed (bit = ABS_* code), their values
    /// and ranges (EVIOCGABS).
    axes: u64 = 0,
    abs_value: [keymap.axis_count]i32 = [_]i32{0} ** keymap.axis_count,
    abs_min: [keymap.axis_count]i32 = [_]i32{-1} ** keymap.axis_count,
    abs_max: [keymap.axis_count]i32 = [_]i32{1} ** keymap.axis_count,
    /// Values at open (the remap utility never takes a resting direction,
    /// e.g. a trigger at its minimum, for a press).
    abs_rest: [keymap.axis_count]i32 = [_]i32{0} ** keymap.axis_count,

    pub fn nameSlice(d: *const Device) []const u8 {
        return std.mem.sliceTo(&d.name, 0);
    }

    /// Which way axis `a` points now (keymap.axisDir).
    pub fn axisDir(d: *const Device, a: u6) i2 {
        if (d.axes & (@as(u64, 1) << a) == 0) return 0;
        return keymap.axisDir(d.abs_value[a], d.abs_min[a], d.abs_max[a]);
    }

    /// The system's key word for what this device holds right now: every
    /// held key or button, and on gamepads every axis direction, through
    /// the map in force.
    pub fn keys(d: *const Device, l: *const keymap.Lookup) keymap.Word {
        var k: keymap.Word = 0;
        var it = d.held.iterator(.{});
        while (it.next()) |code| k |= l.mask[code];
        var axes = d.axes;
        while (axes != 0) : (axes &= axes - 1) {
            const a: u6 = @intCast(@ctz(axes));
            const dir = d.axisDir(a);
            if (dir != 0) k |= l.mask[keymap.axisInput(a, dir > 0)];
        }
        return k;
    }

    /// Whether this device holds an input of `b` (side track O: the frame
    /// advance keys).
    pub fn holds(d: *const Device, b: *const keymap.Binding) bool {
        for (b.slice()) |in| {
            if (in < keymap.key_codes) {
                if (d.held.isSet(in)) return true;
            } else {
                var axes = d.axes;
                while (axes != 0) : (axes &= axes - 1) {
                    const a: u6 = @intCast(@ctz(axes));
                    const dir = d.axisDir(a);
                    if (dir != 0 and keymap.axisInput(a, dir > 0) == in) return true;
                }
            }
        }
        return false;
    }

    /// Apply one event. Returns its kernel timestamp (us) if it may have
    /// changed the keys.
    pub fn apply(d: *Device, e: Event) ?i64 {
        switch (e.type) {
            EV_KEY => {
                if (e.code >= keymap.key_codes) return null;
                switch (e.value) {
                    0 => d.held.unset(e.code),
                    1 => d.held.set(e.code),
                    else => return null, // 2 = auto-repeat: nothing changes
                }
            },
            EV_ABS => {
                if (e.code >= keymap.axis_count or d.axes & (@as(u64, 1) << @intCast(e.code)) == 0) return null;
                d.abs_value[e.code] = e.value;
            },
            else => return null,
        }
        return e.sec * 1_000_000 + e.usec;
    }

    pub fn close(d: *Device) void {
        sys.close(d.fd);
    }
};

fn testBit(bits: []const u8, n: usize) bool {
    return n / 8 < bits.len and (bits[n / 8] >> @intCast(n % 8)) & 1 != 0;
}

/// Open /dev/input/event<index> if it is a keyboard (has the letter Q) or a
/// gamepad (has BTN_SOUTH); null for anything else (mice, touchpads, the
/// PS4 pad's motion sensors, ...). A gamepad's axes are followed with the
/// range it reports; a hat whose range can't be read counts as -1..1.
pub const test_pad_name = "link test pad";

/// With `only`, just devices whose name contains it (`link --device`);
/// without, every device except tools/test_link.py's virtual pad.
pub fn openDevice(index: u8, grab_gamepads: bool, only: ?[]const u8) ?Device {
    var path_buf: [32]u8 = undefined;
    const path = std.fmt.bufPrintZ(&path_buf, "/dev/input/event{d}", .{index}) catch return null;
    const fd = sys.open(path, false) catch return null;
    var keybits = [_]u8{0} ** (keymap.key_codes / 8);
    _ = sys.ioctl(fd, eviocgbit(EV_KEY, keybits.len), @intFromPtr(&keybits)) catch {
        sys.close(fd);
        return null;
    };
    const kind: Kind = if (testBit(&keybits, 0x130)) .gamepad else if (testBit(&keybits, 16)) .keyboard else {
        sys.close(fd);
        return null;
    };
    var d: Device = .{ .fd = fd, .kind = kind, .index = index };
    _ = sys.ioctl(fd, eviocgname(d.name.len - 1), @intFromPtr(&d.name)) catch {};
    // tools/test_link.py's virtual pad only when asked for: a `make keys`
    // running meanwhile would grab it and send its presses to the board
    const wanted = if (only) |o| std.mem.indexOf(u8, d.nameSlice(), o) != null else !std.mem.startsWith(u8, d.nameSlice(), test_pad_name);
    if (!wanted) {
        sys.close(fd);
        return null;
    }
    if (kind == .gamepad) {
        var absbits = [_]u8{0} ** (keymap.axis_count / 8);
        if (sys.ioctl(fd, eviocgbit(EV_ABS, absbits.len), @intFromPtr(&absbits))) |_| {} else |_| {
            absbits[ABS_HAT0X / 8] = 0x03; // HAT0X, HAT0Y
        }
        for (0..keymap.axis_count) |a| {
            if (!testBit(&absbits, a)) continue;
            var info: AbsInfo = undefined;
            if (sys.ioctl(fd, eviocgabs(@intCast(a)), @intFromPtr(&info))) |_| {
                d.abs_value[a] = info.value;
                d.abs_rest[a] = info.value;
                d.abs_min[a] = info.min;
                d.abs_max[a] = info.max;
            } else |_| if (a < ABS_HAT0X or a > ABS_HAT3Y) continue;
            d.axes |= @as(u64, 1) << @intCast(a);
        }
        // Exclusive: games or Steam on the desktop don't see the pad meanwhile.
        if (grab_gamepads) _ = sys.ioctl(fd, EVIOCGRAB, 1) catch {};
    }
    return d;
}

test "event layout matches the kernel's struct input_event (x86_64)" {
    try std.testing.expectEqual(@as(usize, 24), @sizeOf(Event));
}

test "device key state: press, repeat, release, hat, stick (CHIP-8 defaults)" {
    const m = keymap.defaults(.chip8);
    const l = keymap.Lookup.init(&m);
    var d: Device = .{ .fd = -1, .kind = .gamepad, .index = 0, .axes = 1 << 0x10 | 1 << 0 };
    d.abs_min[0] = 0;
    d.abs_max[0] = 255;
    d.abs_value[0] = 128;
    _ = d.apply(.{ .sec = 1, .usec = 5, .type = EV_KEY, .code = 0x130, .value = 1 });
    try std.testing.expectEqual(@as(u16, 1 << 5), d.keys(&l));
    try std.testing.expectEqual(@as(?i64, null), d.apply(.{ .sec = 1, .usec = 6, .type = EV_KEY, .code = 0x130, .value = 2 }));
    _ = d.apply(.{ .sec = 1, .usec = 7, .type = EV_ABS, .code = ABS_HAT0X, .value = 1 });
    try std.testing.expectEqual(@as(u16, 1 << 5 | 1 << 6), d.keys(&l));
    try std.testing.expectEqual(@as(?i64, 1_000_008), d.apply(.{ .sec = 1, .usec = 8, .type = EV_KEY, .code = 0x130, .value = 0 }));
    try std.testing.expectEqual(@as(u16, 1 << 6), d.keys(&l));
    _ = d.apply(.{ .sec = 1, .usec = 9, .type = EV_ABS, .code = 0, .value = 3 }); // stick left
    try std.testing.expectEqual(@as(u16, 1 << 6 | 1 << 4), d.keys(&l));
    // an axis the device doesn't have is ignored
    try std.testing.expectEqual(@as(?i64, null), d.apply(.{ .sec = 1, .usec = 9, .type = EV_ABS, .code = 1, .value = 0 }));
    try std.testing.expectEqual(@as(i2, -1), d.axisDir(0));
    try std.testing.expectEqual(@as(i2, 0), d.axisDir(1));
}
