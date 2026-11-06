//! VGA sink: decodes pin levels, one sample per pixel clock, into frames,
//! the way a monitor would, and checks the sync timing on the way.
//!
//! The frame decoding is the same algorithm as the Haskell decoder in
//! platform/src/Platform/Sim/Frame.hs, so layer 3 (Clash simulation) and
//! layer 4 (this harness) produce identical frames for the same design and
//! can share golden images:
//!
//! * a falling HSYNC edge restarts the pixel counter `hc` (0 on the edge's
//!   clock) and advances the line counter `line`;
//! * a falling VSYNC edge sets `line` to 0 and drops any partial frame;
//! * the pixel at `(hc, line)` is column `hc - h_start` of row
//!   `line - v_start`, where h_start = HSYNC pulse + back porch (clocks) and
//!   v_start = VSYNC pulse + back porch (lines). Syncs are negative.
//!
//! A frame is complete when its last visible pixel arrives. Frames drawn
//! before the first VSYNC edge are never seen, so for a generator that starts
//! at pixel (0, 0) after reset, decoded frame 0 is generated frame 1.

const std = @import("std");

/// One clock's worth of outputs from the shim (vtop.cpp, `struct vtop_out`).
pub const Pins = extern struct {
    ld: u8,
    r: u8,
    g: u8,
    b: u8,
    hs: u8,
    vs: u8,
};

pub const Mode = struct {
    h_total: u32,
    h_sync: u32,
    h_start: u32,
    width: u32,
    v_total: u32,
    v_sync: u32,
    v_start: u32,
    height: u32,
};

/// Platform.Video.VgaTiming.vga640x480.
pub const vga640x480: Mode = .{
    .h_total = 800,
    .h_sync = 96,
    .h_start = 96 + 48,
    .width = 640,
    .v_total = 525,
    .v_sync = 2,
    .v_start = 2 + 33,
    .height = 480,
};

/// A timing check that failed: how often, and the clock of the first time.
pub const Violation = struct {
    count: u64 = 0,
    first_clock: u64 = 0,
    first_value: u64 = 0,

    pub fn record(v: *Violation, clock: u64, value: u64) void {
        if (v.count == 0) {
            v.first_clock = clock;
            v.first_value = value;
        }
        v.count += 1;
    }
};

pub const Decoder = struct {
    mode: Mode,
    /// The frame being filled, RGB8 in raster order. Complete when `feed`
    /// returns true; valid until the next call.
    pixels: []u8,
    /// The colour inputs are 8 bits a channel already (the Arty's TMDS
    /// sink, tmds.zig), not the VGA DAC's 4.
    wide: bool = false,

    hs: u8 = 0,
    vs: u8 = 0,
    hc: u32 = 0,
    line: ?u32 = null,
    len: u32 = 0,

    clock: u64 = 0,
    hs_fall: ?u64 = null,
    vs_fall: ?u64 = null,
    lines_since_vs: u32 = 0,

    hs_edges: u64 = 0,
    vs_edges: u64 = 0,
    bad_h_period: Violation = .{},
    bad_h_width: Violation = .{},
    bad_v_period: Violation = .{},
    bad_v_width: Violation = .{},
    /// Non-black colour outside the visible area (as the sink sees it).
    bad_blanking: Violation = .{},

    pub fn init(gpa: std.mem.Allocator, mode: Mode) !Decoder {
        return .{ .mode = mode, .pixels = try gpa.alloc(u8, mode.width * mode.height * 3) };
    }

    pub fn deinit(d: *Decoder, gpa: std.mem.Allocator) void {
        gpa.free(d.pixels);
    }

    /// Feed one clock. Returns true when a frame has just been completed.
    pub fn feed(d: *Decoder, p: Pins) bool {
        const m = d.mode;
        const now = d.clock;
        d.clock += 1;

        const hs_fall = d.hs == 1 and p.hs == 0;
        const hs_rise = d.hs == 0 and p.hs == 1;
        const vs_fall = d.vs == 1 and p.vs == 0;
        const vs_rise = d.vs == 0 and p.vs == 1;
        d.hs = p.hs;
        d.vs = p.vs;

        // Timing checks.
        if (hs_fall) {
            d.hs_edges += 1;
            if (d.hs_fall) |t| if (now - t != m.h_total) d.bad_h_period.record(now, now - t);
            d.hs_fall = now;
            d.lines_since_vs += 1;
        }
        if (hs_rise) {
            if (d.hs_fall) |t| if (now - t != m.h_sync) d.bad_h_width.record(now, now - t);
        }
        if (vs_fall) {
            d.vs_edges += 1;
            if (d.vs_fall != null and d.lines_since_vs != m.v_total)
                d.bad_v_period.record(now, d.lines_since_vs);
            d.vs_fall = now;
            d.lines_since_vs = 0;
        }
        if (vs_rise) {
            if (d.vs_fall) |t| if (now - t != m.v_sync * m.h_total) d.bad_v_width.record(now, now - t);
        }

        // Frame decoding (the same steps as Platform.Sim.Frame.decodeFrames).
        d.hc = if (hs_fall) 0 else d.hc + 1;
        if (vs_fall) {
            d.line = 0;
            d.len = 0;
        }
        if (hs_fall) {
            if (d.line) |*n| n.* += 1;
        }
        const n = d.line orelse return false;
        const visible = n >= m.v_start and n - m.v_start < m.height and
            d.hc >= m.h_start and d.hc - m.h_start < m.width;
        if (!visible) {
            if (p.r != 0 or p.g != 0 or p.b != 0) d.bad_blanking.record(now, d.hc);
            return false;
        }
        const i = d.len * 3;
        d.pixels[i + 0] = if (d.wide) p.r else widen(p.r);
        d.pixels[i + 1] = if (d.wide) p.g else widen(p.g);
        d.pixels[i + 2] = if (d.wide) p.b else widen(p.b);
        d.len += 1;
        if (d.len == m.width * m.height) {
            d.len = 0;
            return true;
        }
        return false;
    }

    /// Print the timing checks. Returns false if any failed.
    pub fn report(d: *const Decoder) bool {
        const m = d.mode;
        std.debug.print("vga: {d} clocks, {d} HSYNC and {d} VSYNC pulses\n", .{ d.clock, d.hs_edges, d.vs_edges });
        var ok = true;
        ok = check("HSYNC period", m.h_total, "clocks", d.bad_h_period) and ok;
        ok = check("HSYNC width", m.h_sync, "clocks", d.bad_h_width) and ok;
        ok = check("VSYNC period", m.v_total, "lines", d.bad_v_period) and ok;
        ok = check("VSYNC width", m.v_sync * m.h_total, "clocks", d.bad_v_width) and ok;
        ok = check("blanking colour", 0, "(non-black pixels; value = hc)", d.bad_blanking) and ok;
        if (d.vs_edges == 0) {
            std.debug.print("vga: FAIL no VSYNC pulse seen\n", .{});
            ok = false;
        }
        return ok;
    }

    fn check(name: []const u8, expect: u64, unit: []const u8, v: Violation) bool {
        if (v.count == 0) {
            std.debug.print("vga:   {s}: {d} {s}: ok\n", .{ name, expect, unit });
            return true;
        }
        std.debug.print("vga:   {s}: expected {d} {s}: FAIL {d} time(s), first at clock {d} (got {d})\n", .{ name, expect, unit, v.count, v.first_clock, v.first_value });
        return false;
    }
};

/// 4-bit DAC level to 8 bits (0xF -> 0xFF), as Platform.Sim.Frame.rgbPixel.
fn widen(x: u8) u8 {
    return (x & 0xF) * 17;
}

fn testPins(hs: u8, vs: u8, c: u8) Pins {
    return .{ .ld = 0, .r = c, .g = c, .b = c, .hs = hs, .vs = vs };
}

test "decodes a tiny mode and checks its timing" {
    // 4x2 visible, h: 4 + 1 fp + 2 sync + 1 bp = 8, v: 2 + 1 fp + 1 sync + 1 bp = 5.
    const m: Mode = .{ .h_total = 8, .h_sync = 2, .h_start = 3, .width = 4, .v_total = 5, .v_sync = 1, .v_start = 2, .height = 2 };
    var d = try Decoder.init(std.testing.allocator, m);
    defer d.deinit(std.testing.allocator);
    var frames: u32 = 0;
    var clk: u32 = 0;
    while (clk < 8 * 5 * 3) : (clk += 1) {
        const x = clk % 8;
        const y = (clk / 8) % 5;
        const hs: u8 = if (x >= 5 and x < 7) 0 else 1;
        const vs: u8 = if (y == 3) 0 else 1;
        const c: u8 = if (x < 4 and y < 2) @intCast(1 + x + 4 * y) else 0;
        if (d.feed(testPins(hs, vs, c))) {
            frames += 1;
            for (0..8) |i| try std.testing.expectEqual(@as(u8, @intCast(1 + i)) * 17, d.pixels[i * 3]);
        }
    }
    try std.testing.expectEqual(@as(u32, 2), frames);
    try std.testing.expect(d.bad_h_period.count == 0 and d.bad_h_width.count == 0);
    try std.testing.expect(d.bad_v_period.count == 0 and d.bad_v_width.count == 0);
    try std.testing.expectEqual(@as(u64, 0), d.bad_blanking.count);
}
