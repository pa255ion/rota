//! The Arty harness's tone check (side track G phase 2, step 6;
//! arty.zig): the aud_pwm line averaged per block of pixel clocks, and the
//! frequency of the tone in those averages. A file of its own so that
//! `zig build test` runs its test without the Verilator model.

const std = @import("std");

/// Pixel clocks per averaged sample of the PWM line (25.17623 MHz / 524 =
/// 48.05 kHz).
pub const pwm_block = 524;

/// The tone in `pcm` (ones per block): rising crossings of the mean with a
/// hysteresis of 1/8 of the half-swing, over the second half, in Hz.
pub fn toneHz(pcm: []const u16) f64 {
    return toneHzAt(pcm, @as(f64, pwm_block) * 39720e-12);
}

/// The same for samples `period` seconds apart (the HDMI audio's).
pub fn toneHzAt(pcm: []const u16, period: f64) f64 {
    const half = pcm[pcm.len / 2 ..];
    if (half.len < 2) return 0;
    var lo: u16 = std.math.maxInt(u16);
    var hi: u16 = 0;
    var sum: u64 = 0;
    for (half) |v| {
        lo = @min(lo, v);
        hi = @max(hi, v);
        sum += v;
    }
    const mean: f64 = @as(f64, @floatFromInt(sum)) / @as(f64, @floatFromInt(half.len));
    const hyst: f64 = @as(f64, @floatFromInt(hi - lo)) / 16.0;
    if (hyst == 0) return 0;
    var above = @as(f64, @floatFromInt(half[0])) > mean;
    var first: ?usize = null;
    var last: usize = 0;
    var n: u32 = 0;
    for (half, 0..) |v, i| {
        const x = @as(f64, @floatFromInt(v));
        if (!above and x > mean + hyst) {
            above = true;
            if (first == null) first = i else n += 1;
            last = i;
        } else if (above and x < mean - hyst) above = false;
    }
    const f0 = first orelse return 0;
    if (n == 0) return 0;
    const seconds = @as(f64, @floatFromInt(last - f0)) * period;
    return @as(f64, @floatFromInt(n)) / seconds;
}

test "toneHz: a square wave's frequency from block averages" {
    var buf: [4800]u16 = undefined;
    // 48.05 kHz blocks; 440 Hz: a period of about 109.2 blocks
    const rate = 1.0 / (pwm_block * 39720e-12);
    for (&buf, 0..) |*v, i| {
        const t = @as(f64, @floatFromInt(i)) / rate;
        v.* = if (@mod(t * 440.0, 1.0) < 0.5) 400 else 120;
    }
    const f = toneHz(&buf);
    try std.testing.expect(@abs(f - 440.0) < 2.0);
}

