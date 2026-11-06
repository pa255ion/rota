//! The in-game options (side track L): the volume and the emulation speed,
//! as the PS sends them to the PL's settings registers (pl.zig,
//! `volume_addr`/`speed_addr`; Platform.Ps.Link's `PsSettings`), and their
//! text on the menu's Options page. Pure data: no hardware access.

const std = @import("std");

/// 0 is silence, 10 the level every system had before (unity gain).
pub const volume_max: u4 = 10;
/// The speed in quarters of the real machine's: 2 is 50 %, 4 is 100 %.
pub const speed_min: u4 = 2;
pub const speed_normal: u4 = 4;
pub const speed_max: u4 = 8;

/// What the PL's settings registers hold. The defaults are the PL's reset
/// values, so a PS that never writes them changes nothing.
pub const Settings = struct {
    volume: u4 = volume_max,
    speed: u4 = speed_normal,
};

/// One step down or up, stopping at the ends (`top` is the system's
/// fastest speed, from its table entry).
pub fn volumeStep(v: u4, up: bool) u4 {
    return if (up) @min(v +| 1, volume_max) else v -| 1;
}

pub fn speedStep(s: u4, up: bool, top: u4) u4 {
    const t = @max(speed_normal, @min(top, speed_max));
    return if (up) @min(s +| 1, t) else @max(s -| 1, speed_min);
}

// The texts and the file are written by hand, not with std.fmt: the PS
// app's OCM is full to the last few hundred bytes (each std.fmt use for a
// new type adds code), 2026-10-11.

/// `n` (at most 999) in decimal at the start of `buf`: its length.
fn decimal(n: u32, buf: []u8) usize {
    var d: [3]u8 = undefined;
    var k: usize = 0;
    var v = n;
    while (true) {
        d[k] = '0' + @as(u8, @intCast(v % 10));
        k += 1;
        v /= 10;
        if (v == 0 or k == d.len) break;
    }
    for (0..k) |i| buf[i] = d[k - 1 - i];
    return k;
}

/// `parts` one after the other into `buf` (it must be long enough).
fn concat(buf: []u8, parts: []const []const u8) []const u8 {
    var n: usize = 0;
    for (parts) |p| {
        @memcpy(buf[n..][0..p.len], p);
        n += p.len;
    }
    return buf[0..n];
}

/// "Volume: ######.... 6" (a bar of `volume_max` cells, then the level).
pub fn volumeText(v: u4, buf: []u8) []const u8 {
    var bar: [volume_max]u8 = undefined;
    for (&bar, 0..) |*c, i| c.* = if (i < v) '#' else '.';
    var num: [3]u8 = undefined;
    return concat(buf, &.{ "Volume: ", &bar, " ", num[0..decimal(v, &num)] });
}

/// "Speed: 75%" (quarters to per cent).
pub fn speedText(s: u4, buf: []u8) []const u8 {
    var num: [3]u8 = undefined;
    return concat(buf, &.{ "Speed: ", num[0..decimal(@as(u32, s) * 25, &num)], "%" });
}

/// `/settings.txt` (PLAN.md L.3): the volume kept on the card. `make sd`
/// makes it `file_size` bytes, "volume=10" padded with spaces and a newline
/// at the end, so the PS rewrites its one block in place (as last.txt). The
/// speed is not kept: every game starts at 100 %.
pub const file = "settings.txt";
pub const file_size = 64;

/// The volume in the file's text, null if there is none or it is bad (the
/// default stays). Lines `key=value`; other keys are ignored.
pub fn parseVolume(text: []const u8) ?u4 {
    var start: usize = 0;
    while (start < text.len) {
        const end = std.mem.indexOfScalarPos(u8, text, start, '\n') orelse text.len;
        const l = std.mem.trim(u8, text[start..end], " \t\r\x00");
        start = end + 1;
        const eq = std.mem.indexOfScalar(u8, l, '=') orelse continue;
        if (!std.mem.eql(u8, std.mem.trim(u8, l[0..eq], " \t"), "volume")) continue;
        const t = std.mem.trim(u8, l[eq + 1 ..], " \t");
        if (t.len == 0 or t.len > 2) return null;
        var v: u8 = 0;
        for (t) |c| {
            if (c < '0' or c > '9') return null;
            v = v * 10 + (c - '0');
        }
        return if (v <= volume_max) @intCast(v) else null;
    }
    return null;
}

/// Writes the settings into the start of the file's block (`size` bytes
/// of it): false if they don't fit.
pub fn writeFile(block: *[512]u8, size: u32, s: Settings) bool {
    var line: [16]u8 = undefined;
    var num: [3]u8 = undefined;
    const t = concat(&line, &.{ "volume=", num[0..decimal(s.volume, &num)] });
    if (size == 0 or size > block.len or t.len + 1 > size) return false;
    @memset(block[0..size], ' ');
    @memcpy(block[0..t.len], t);
    block[size - 1] = '\n';
    return true;
}

const testing = std.testing;

test "settings.txt: the volume read back, other keys and spaces ignored, bad ones refused" {
    try testing.expectEqual(@as(?u4, 7), parseVolume("volume=7" ++ " " ** 54 ++ "\n"));
    try testing.expectEqual(@as(?u4, 0), parseVolume("# mine\nspeed=3\n volume = 0 \r\n"));
    try testing.expectEqual(@as(?u4, 10), parseVolume("volume=10\n"));
    try testing.expectEqual(@as(?u4, null), parseVolume("volume=11\n"));
    try testing.expectEqual(@as(?u4, null), parseVolume("volume=loud\n"));
    try testing.expectEqual(@as(?u4, null), parseVolume(""));
    try testing.expectEqual(@as(?u4, null), parseVolume("volumes=3\n"));
}

test "settings.txt: written in place keeping its size, read back; the rest of the block untouched" {
    var b: [512]u8 = [_]u8{0xEE} ** 512;
    try testing.expect(writeFile(&b, file_size, .{ .volume = 3, .speed = 6 }));
    try testing.expectEqualStrings("volume=3", b[0..8]);
    try testing.expectEqual(@as(u8, ' '), b[8]);
    try testing.expectEqual(@as(u8, '\n'), b[file_size - 1]);
    try testing.expectEqual(@as(u8, 0xEE), b[file_size]);
    try testing.expectEqual(@as(?u4, 3), parseVolume(b[0..file_size]));
    try testing.expect(writeFile(&b, 10, .{ .volume = 10 }));
    try testing.expect(!writeFile(&b, 9, .{ .volume = 10 }));
    try testing.expect(!writeFile(&b, 0, .{}));
    try testing.expect(!writeFile(&b, 513, .{}));
}

test "the defaults are the PL's reset values" {
    const s: Settings = .{};
    try testing.expectEqual(@as(u4, 10), s.volume);
    try testing.expectEqual(@as(u4, 4), s.speed);
}

test "volume: steps stop at 0 and 10" {
    try testing.expectEqual(@as(u4, 0), volumeStep(0, false));
    try testing.expectEqual(@as(u4, 1), volumeStep(0, true));
    try testing.expectEqual(@as(u4, 9), volumeStep(10, false));
    try testing.expectEqual(@as(u4, 10), volumeStep(10, true));
}

test "speed: from 50 % up to the system's own limit, never past 200 %" {
    try testing.expectEqual(@as(u4, 2), speedStep(2, false, 6));
    try testing.expectEqual(@as(u4, 3), speedStep(2, true, 6));
    try testing.expectEqual(@as(u4, 6), speedStep(6, true, 6));
    try testing.expectEqual(@as(u4, 4), speedStep(4, true, 4));
    try testing.expectEqual(@as(u4, 8), speedStep(8, true, 15));
    // a limit below 100 % still allows 100 %
    try testing.expectEqual(@as(u4, 4), speedStep(3, true, 0));
}

test "texts" {
    var b: [40]u8 = undefined;
    try testing.expectEqualStrings("Volume: ######.... 6", volumeText(6, &b));
    try testing.expectEqualStrings("Volume: .......... 0", volumeText(0, &b));
    try testing.expectEqualStrings("Speed: 50%", speedText(2, &b));
    try testing.expectEqualStrings("Speed: 175%", speedText(7, &b));
}
