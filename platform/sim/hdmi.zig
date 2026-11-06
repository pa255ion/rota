//! HDMI sink for the layer 4 harness: what the ADV7511 would make of the
//! 16-bit bus (PLAN.md M0d step 5). Framing comes from the bus itself: a
//! falling HD_VSYNC starts a frame, each HD_DE run is a line, and a frame
//! is complete after `height` lines of `width` pixels. Decoded frame 0 is
//! the first complete one after the first VSYNC edge, as in vga.zig.
//!
//! The pixels are turned into RGB the way Platform.Sim.Adv7511 defines it
//! (change both together): HD_D[15:8] = chroma (Cb on even pixels, Cr on
//! odd ones), HD_D[7:0] = Y; zero-order 4:2:2 -> 4:4:4 (both pixels of a
//! pair use its Cb and Cr); then the colour space converter with the
//! coefficients from the register image the design wrote over I2C.
//!
//! Checks on the way: every DE run is `width` pixels, every frame
//! `height` runs, the bus is black (Y 16, C 128) outside DE, and HD_CLK is
//! the pixel clock inverted (low after its rising edge, high after its
//! falling edge) once the design is out of reset.

const std = @import("std");
const vtop = @import("vtop.zig");
const vga = @import("vga.zig");

pub const Sink = struct {
    width: u32,
    height: u32,
    /// Y and C of the frame being filled, raster order.
    ycc: []u8,
    /// The last completed frame as RGB8 (filled by `toRgb`).
    rgb: []u8,

    de: u8 = 0,
    vs: u8 = 1,
    line: ?u32 = null,
    x: u32 = 0,
    clock: u64 = 0,
    clk_seen: bool = false,
    /// Clock of the VSYNC edge that began the frame being filled.
    frame_start: u64 = 0,

    lines_total: u64 = 0,
    bad_line_length: vga.Violation = .{},
    bad_frame_lines: vga.Violation = .{},
    bad_blanking: vga.Violation = .{},
    bad_clock: vga.Violation = .{},

    pub fn init(gpa: std.mem.Allocator, width: u32, height: u32) !Sink {
        return .{
            .width = width,
            .height = height,
            .ycc = try gpa.alloc(u8, width * height * 2),
            .rgb = try gpa.alloc(u8, width * height * 3),
        };
    }

    pub fn deinit(s: *Sink, gpa: std.mem.Allocator) void {
        gpa.free(s.ycc);
        gpa.free(s.rgb);
    }

    /// Feed one clock. Returns true when a frame has just been completed
    /// (its raw data is in `ycc` until the next call).
    pub fn feed(s: *Sink, h: vtop.Hdmi) bool {
        const now = s.clock;
        s.clock += 1;

        // HD_CLK toggles once the design is out of reset; from then on it
        // must be the inverted pixel clock.
        if (h.clk_after_fall == 1) s.clk_seen = true;
        if (s.clk_seen and !(h.clk_after_fall == 1 and h.clk_after_rise == 0))
            s.bad_clock.record(now, @as(u64, h.clk_after_fall) * 2 + h.clk_after_rise);

        const vs_fall = s.vs == 1 and h.vs == 0;
        const de_rise = s.de == 0 and h.de == 1;
        const de_fall = s.de == 1 and h.de == 0;
        s.vs = h.vs;
        s.de = h.de;

        if (de_fall) {
            if (s.line != null and s.x != s.width) s.bad_line_length.record(now, s.x);
        }
        if (vs_fall) {
            if (s.line) |n| if (n != s.height) s.bad_frame_lines.record(now, n);
            s.line = 0;
            s.x = 0;
            s.frame_start = now;
        }
        if (h.de == 0) {
            if (h.d != 0x8010) s.bad_blanking.record(now, h.d);
            return false;
        }
        const n = s.line orelse return false;
        if (de_rise) s.x = 0;
        if (n < s.height and s.x < s.width) {
            const i = (n * s.width + s.x) * 2;
            s.ycc[i] = @truncate(h.d); // Y
            s.ycc[i + 1] = @truncate(h.d >> 8); // C
        }
        s.x += 1;
        if (s.x == s.width) {
            s.lines_total += 1;
            s.line = n + 1;
            if (n + 1 == s.height) return true;
        }
        return false;
    }

    /// Convert the frame in `ycc` into `rgb` with the CSC registers of `regs`.
    pub fn toRgb(s: *Sink, regs: *const [256]u8) void {
        var y: u32 = 0;
        while (y < s.height) : (y += 1) {
            var x: u32 = 0;
            while (x < s.width) : (x += 2) {
                const i = (y * s.width + x) * 2;
                const cb = s.ycc[i + 1];
                const cr = if (x + 1 < s.width) s.ycc[i + 3] else 128;
                const o = (y * s.width + x) * 3;
                csc(regs, s.ycc[i], cb, cr, s.rgb[o..][0..3]);
                if (x + 1 < s.width) csc(regs, s.ycc[i + 2], cb, cr, s.rgb[o + 3 ..][0..3]);
            }
        }
    }

    /// Print the checks. Returns false if any failed.
    pub fn report(s: *const Sink) bool {
        std.debug.print("hdmi: {d} clocks, {d} complete lines\n", .{ s.clock, s.lines_total });
        var ok = true;
        ok = check("DE run length", s.width, "pixels", s.bad_line_length) and ok;
        ok = check("lines per frame", s.height, "lines", s.bad_frame_lines) and ok;
        ok = check("bus outside DE", 0x8010, "(= 0x8010: Y 16, C 128; value = HD_D)", s.bad_blanking) and ok;
        ok = check("HD_CLK = pixel clock inverted", 2, "(after fall, after rise) = (1, 0)", s.bad_clock) and ok;
        if (!s.clk_seen) {
            std.debug.print("hdmi: FAIL HD_CLK never toggled\n", .{});
            ok = false;
        }
        return ok;
    }

    fn check(name: []const u8, expect: u64, unit: []const u8, v: vga.Violation) bool {
        if (v.count == 0) {
            std.debug.print("hdmi:   {s}: {d} {s}: ok\n", .{ name, expect, unit });
            return true;
        }
        std.debug.print("hdmi:   {s}: expected {d} {s}: FAIL {d} time(s), first at clock {d} (got {d})\n", .{ name, expect, unit, v.count, v.first_clock, v.first_value });
        return false;
    }
};

/// Platform.Sim.Adv7511.cscRgb: PG equations 1 to 3 in integers.
pub fn csc(regs: *const [256]u8, y: u8, cb: u8, cr: u8, out: *[3]u8) void {
    const mode: u5 = @intCast(@min(2, (regs[0x18] >> 5) & 3));
    const ins = [3]i64{ @as(i64, cr) << 4, @as(i64, y) << 4, @as(i64, cb) << 4 };
    for (0..3) |k| {
        var acc: i64 = 0;
        for (0..3) |j| acc += coef(regs, 4 * k + j) * ins[j];
        const out12 = ((acc >> 12) + coef(regs, 4 * k + 3)) << mode;
        out[k] = @intCast(std.math.clamp(out12 >> 4, 0, 255));
    }
}

fn coef(regs: *const [256]u8, i: usize) i64 {
    const v: i64 = (@as(i64, regs[0x18 + 2 * i] & 0x1F) << 8) | regs[0x19 + 2 * i];
    return if (v >= 4096) v - 8192 else v;
}

test "csc with PG Table 35 (as written by the design): black, white, red" {
    var regs = [_]u8{0} ** 256;
    const t35 = [_]u8{ 0xE6, 0x69, 0x04, 0xAC, 0x00, 0x00, 0x1C, 0x81, 0x1C, 0xBC, 0x04, 0xAD, 0x1E, 0x6E, 0x02, 0x20, 0x1F, 0xFE, 0x04, 0xAD, 0x08, 0x1A, 0x1B, 0xA9 };
    @memcpy(regs[0x18..][0..24], &t35);
    var o: [3]u8 = undefined;
    csc(&regs, 16, 128, 128, &o);
    try std.testing.expect(o[0] <= 2 and o[1] <= 2 and o[2] <= 2);
    csc(&regs, 235, 128, 128, &o);
    try std.testing.expect(o[0] >= 253 and o[1] >= 253 and o[2] >= 253);
    csc(&regs, 81, 90, 240, &o); // BT.601 red
    try std.testing.expect(o[0] >= 250 and o[1] <= 3 and o[2] <= 3);
}

test "sink: framing, black outside DE, inverted clock" {
    var s = try Sink.init(std.testing.allocator, 4, 2);
    defer s.deinit(std.testing.allocator);
    var frames: u32 = 0;
    var clk: u32 = 0;
    while (clk < 8 * 5 * 3) : (clk += 1) {
        const x = clk % 8;
        const yy = (clk / 8) % 5;
        const de: u8 = if (x < 4 and yy < 2) 1 else 0;
        const vs: u8 = if (yy == 3) 0 else 1;
        const d: u16 = if (de == 1) @intCast(0x8000 | (16 + x + 4 * yy)) else 0x8010;
        if (s.feed(.{ .d = d, .de = de, .hs = 1, .vs = vs, .clk_after_fall = 1, .clk_after_rise = 0, .scl_low = 0, .sda_low = 0 })) {
            frames += 1;
            for (0..8) |i| try std.testing.expectEqual(@as(u8, @intCast(16 + i)), s.ycc[i * 2]);
        }
    }
    try std.testing.expectEqual(@as(u32, 2), frames);
    try std.testing.expect(s.report());
}
