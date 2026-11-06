//! The PS side of the PS link for the Verilator harness (vga-sim --ps
//! FILE): a script of link writes (the format of Platform.Sim.Overlay,
//! which renders the matching golden) played on PS_GPIO through the PS
//! app's own encoder (platform/ps/pl.zig, module "pl"), with its timing: one
//! pixel clock per tick, so its 2 us waits are 50 ticks.
//!
//!   text R C some text     characters from cell (R, C)
//!   inv R C some text      the same in inverse video
//!   bar R C some text      row R all inverse spaces, then the text from C
//!   ctrl N                 the control register (bit 0 overlay, bit 1 pause,
//!                          bit 2 a frame-advance step, side track O)
//!   hold 0|1               the hold line
//!   keys N                 the live input word, PS_GPIO bits 0 to 15
//!                          (side track O: a key pressed mid-frame)
//!   bytes ADDR HEX         bytes (two hex digits each) written from ADDR
//!                          (a cartridge: tools/ps-load-script.sh)
//!   wait US                the lines stay as they are for US microseconds
//!                          (a test running in the PL: systems/m0e)

const std = @import("std");
const pl = @import("pl");

/// A PS_GPIO word held for `ticks` pixel clocks.
pub const Phase = struct { word: u32, ticks: u32 };

/// The GPIO "port" pl.Link writes to: it records phases.
const Recorder = struct {
    gpa: std.mem.Allocator,
    phases: std.ArrayList(Phase) = .empty,

    pub fn write(r: *Recorder, v: u32) void {
        r.phases.append(r.gpa, .{ .word = v, .ticks = 1 }) catch @panic("out of memory");
    }
    pub fn delayUs(r: *Recorder, us: u32) void {
        r.phases.items[r.phases.items.len - 1].ticks += us * 25;
    }
    pub fn read(_: *Recorder) u32 {
        return 0;
    }
};

pub const Error = error{ BadScript, OutOfMemory };

/// The bytes of a hex file (two digits each, any whitespace between
/// pairs or digits), for --expect-ps-hex in both harnesses (caller frees
/// with gpa). A digit that isn't hex, or an odd one left over, is
/// error.BadHex.
pub fn parseHex(gpa: std.mem.Allocator, text: []const u8) error{ BadHex, OutOfMemory }![]u8 {
    var out: std.ArrayList(u8) = .empty;
    errdefer out.deinit(gpa);
    var digits: [2]u8 = undefined;
    var k: usize = 0;
    for (text) |c| {
        if (std.ascii.isWhitespace(c)) continue;
        digits[k] = c;
        k += 1;
        if (k == 2) {
            try out.append(gpa, std.fmt.parseInt(u8, &digits, 16) catch return error.BadHex);
            k = 0;
        }
    }
    if (k != 0) return error.BadHex;
    return out.toOwnedSlice(gpa);
}

/// --expect-ps-hex: the byte channel's bytes `got` against the hex file
/// `text` (read from `path`): exactly equal. Prints the verdict as both
/// harnesses do.
pub fn hexReport(gpa: std.mem.Allocator, path: []const u8, text: []const u8, got: []const u8) !bool {
    const want = parseHex(gpa, text) catch |err| switch (err) {
        error.BadHex => {
            std.debug.print("ps: FAIL {s}: not hex\n", .{path});
            return false;
        },
        else => |e| return e,
    };
    defer gpa.free(want);
    if (std.mem.eql(u8, want, got)) {
        std.debug.print("ps: byte channel: {d} byte(s), identical to {s}\n", .{ got.len, path });
        return true;
    }
    std.debug.print("ps: FAIL byte channel: got {x}, expected {x} ({s})\n", .{ got, want, path });
    return false;
}

/// The phases for a script (caller frees with gpa).
pub fn compile(gpa: std.mem.Allocator, script: []const u8) Error![]Phase {
    var rec: Recorder = .{ .gpa = gpa };
    errdefer rec.phases.deinit(gpa);
    var link: pl.Link(Recorder) = .{ .port = &rec };
    var lines = std.mem.splitScalar(u8, script, '\n');
    var n: usize = 0;
    while (lines.next()) |raw| {
        n += 1;
        const l = std.mem.trimEnd(u8, raw, "\r");
        var it = std.mem.tokenizeScalar(u8, l, ' ');
        const op = it.next() orelse continue;
        if (op[0] == '#') continue;
        if (std.mem.eql(u8, op, "ctrl")) {
            const v = num(u8, it.next(), n) catch return error.BadScript;
            link.write(pl.control_addr, &.{v});
        } else if (std.mem.eql(u8, op, "keys")) {
            const v = num(u16, it.next(), n) catch return error.BadScript;
            link.setUser(v);
        } else if (std.mem.eql(u8, op, "hold")) {
            const v = num(u1, it.next(), n) catch return error.BadScript;
            link.setHold(v == 1);
        } else if (std.mem.eql(u8, op, "bytes")) {
            const a = num(u24, it.next(), n) catch return error.BadScript;
            const hex = it.next() orelse return bad(n);
            if (hex.len % 2 != 0) return bad(n);
            const buf = try gpa.alloc(u8, hex.len / 2);
            defer gpa.free(buf);
            _ = std.fmt.hexToBytes(buf, hex) catch return bad(n);
            link.write(a, buf);
        } else if (std.mem.eql(u8, op, "wait")) {
            const us = num(u32, it.next(), n) catch return error.BadScript;
            if (rec.phases.items.len == 0) rec.write(0);
            rec.delayUs(us);
        } else if (std.mem.eql(u8, op, "text") or std.mem.eql(u8, op, "inv") or std.mem.eql(u8, op, "bar")) {
            const r = num(u8, it.next(), n) catch return error.BadScript;
            const c = num(u8, it.next(), n) catch return error.BadScript;
            if (r >= pl.rows or c >= pl.cols) return bad(n);
            // the text: after the third word and one space
            const text = if (it.index < l.len) l[it.index + 1 ..] else "";
            var row: [pl.cols]u8 = undefined;
            const inv: u8 = if (std.mem.eql(u8, op, "text")) 0 else 0x80;
            const len = @min(text.len, pl.cols - c);
            for (text[0..len], 0..) |ch, i| row[c + i] = (ch & 0x7F) | inv;
            const base = pl.overlay_base + @as(u24, r) * pl.cols;
            if (std.mem.eql(u8, op, "bar")) {
                @memset(row[0..c], ' ' | 0x80);
                @memset(row[c + len ..], ' ' | 0x80);
                link.write(base, &row);
            } else {
                link.write(base + c, row[c .. c + len]);
            }
        } else return bad(n);
    }
    return rec.phases.toOwnedSlice(gpa);
}

fn num(comptime T: type, s: ?[]const u8, n: usize) !T {
    return std.fmt.parseInt(T, s orelse return bad(n), 0) catch bad(n);
}

fn bad(n: usize) Error {
    std.debug.print("ps: script line {d} not understood\n", .{n});
    return error.BadScript;
}

/// Plays phases one tick at a time.
pub const Player = struct {
    phases: []const Phase,
    i: usize = 0,
    left: u32 = 0,
    word: u32 = 0,

    /// The PS_GPIO word for the next tick.
    pub fn tick(p: *Player) u32 {
        if (p.left == 0 and p.i < p.phases.len) {
            p.word = p.phases[p.i].word;
            p.left = p.phases[p.i].ticks;
            p.i += 1;
        }
        if (p.left > 0) p.left -= 1;
        return p.word;
    }

    pub fn done(p: *const Player) bool {
        return p.i == p.phases.len and p.left == 0;
    }
};

test "script to link words: a decoder sees the writes" {
    const gpa = std.testing.allocator;
    const phases = try compile(gpa, "# x\ntext 1 2 Hi\nbar 0 38 ab\nctrl 3\nhold 1\n");
    defer gpa.free(phases);
    // decode like Platform.Ps.Link.linkStep
    var st = false;
    var addr: u24 = 0;
    var got: std.AutoHashMapUnmanaged(u24, u8) = .empty;
    defer got.deinit(gpa);
    var p: Player = .{ .phases = phases };
    var ticks: usize = 0;
    var last: u32 = 0;
    while (!p.done()) {
        const w = p.tick();
        ticks += 1;
        last = w;
        const s = w & (1 << 24) != 0;
        if (s == st) continue;
        st = s;
        const d: u8 = @truncate(w >> 16);
        if (w & (1 << 25) != 0) addr = addr << 8 | d else {
            try got.put(gpa, addr, d);
            addr +%= 1;
        }
    }
    try std.testing.expectEqual(@as(u8, 'H'), got.get(pl.overlay_base + 42).?);
    try std.testing.expectEqual(@as(u8, 'i'), got.get(pl.overlay_base + 43).?);
    try std.testing.expectEqual(@as(u8, 0xA0), got.get(pl.overlay_base + 0).?);
    try std.testing.expectEqual(@as(u8, 'a' | 0x80), got.get(pl.overlay_base + 38).?);
    try std.testing.expectEqual(@as(u8, 'b' | 0x80), got.get(pl.overlay_base + 39).?);
    try std.testing.expectEqual(@as(u8, 3), got.get(pl.control_addr).?);
    try std.testing.expect(last & (1 << 26) != 0);
    // 2 + 40 + 1 bytes and 3 address bytes per write, 100 ticks each
    try std.testing.expect(ticks > (43 + 9) * 100);
    try std.testing.expectError(error.BadScript, compile(gpa, "text 30 0 x\n"));
    try std.testing.expectError(error.BadScript, compile(gpa, "frob\n"));
}

test "bytes: a run of bytes from an address, odd or bad hex refused" {
    const gpa = std.testing.allocator;
    const phases = try compile(gpa, "hold 1\nbytes 0x10 4e45531A\nhold 0\n");
    defer gpa.free(phases);
    var st = false;
    var addr: u24 = 0;
    var got: [4]u8 = undefined;
    var n: usize = 0;
    var p: Player = .{ .phases = phases };
    while (!p.done()) {
        const w = p.tick();
        const s = w & (1 << 24) != 0;
        if (s == st) continue;
        st = s;
        const d: u8 = @truncate(w >> 16);
        if (w & (1 << 25) != 0) addr = addr << 8 | d else {
            try std.testing.expectEqual(@as(u24, 0x10) + @as(u24, @intCast(n)), addr);
            got[n] = d;
            n += 1;
            addr +%= 1;
        }
    }
    try std.testing.expectEqualSlices(u8, &.{ 0x4E, 0x45, 0x53, 0x1A }, got[0..n]);
    try std.testing.expectError(error.BadScript, compile(gpa, "bytes 0 abc\n"));
    try std.testing.expectError(error.BadScript, compile(gpa, "bytes 0 zz\n"));
}

test "parseHex: pairs across any spacing; bad or odd digits refused" {
    const gpa = std.testing.allocator;
    const b = try parseHex(gpa, "53 41\n5\t6 45\n");
    defer gpa.free(b);
    try std.testing.expectEqualSlices(u8, "SAVE", b);
    const none = try parseHex(gpa, " \n");
    defer gpa.free(none);
    try std.testing.expectEqual(@as(usize, 0), none.len);
    try std.testing.expectError(error.BadHex, parseHex(gpa, "4g"));
    try std.testing.expectError(error.BadHex, parseHex(gpa, "414"));
}

test "hexReport: identical passes; a byte more, less or different fails" {
    const gpa = std.testing.allocator;
    try std.testing.expect(try hexReport(gpa, "t.hex", "0f 2a", &.{ 0x0F, 0x2A }));
    try std.testing.expect(!try hexReport(gpa, "t.hex", "0f 2a", &.{0x0F}));
    try std.testing.expect(!try hexReport(gpa, "t.hex", "0f 2a", &.{ 0x0F, 0x2A, 0 }));
    try std.testing.expect(!try hexReport(gpa, "t.hex", "0f 2a", &.{ 0x0F, 0x2B }));
    try std.testing.expect(!try hexReport(gpa, "t.hex", "0f 2", &.{0x0F}));
}

test "keys: the live input word in bits 0 to 15, the rest of the word kept" {
    const gpa = std.testing.allocator;
    const phases = try compile(gpa, "hold 1\nkeys 0x0011\nkeys 0x80\n");
    defer gpa.free(phases);
    try std.testing.expectEqual(@as(u32, 0x0011), phases[1].word & 0xFFFF);
    try std.testing.expect(phases[1].word & (1 << 26) != 0); // still held
    try std.testing.expectEqual(@as(u32, 0x0080), phases[2].word & 0xFFFF);
    try std.testing.expectError(error.BadScript, compile(gpa, "keys 0x10000\n"));
}
