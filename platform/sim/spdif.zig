//! S/PDIF receiver for the layer 4 harness (PLAN.md step 2.9): decodes
//! HD_SPDIF as the ADV7511 would, from the line sampled twice per pixel
//! clock (after each edge; the design drives it from an ODDR).
//!
//! Written from IEC 60958, independently of Platform.Audio.Spdif:
//! biphase-mark cells at 6.144 MHz (48 kHz × 128) are recovered from the
//! run lengths (a run is 1, 2 or 3 cells; one cell is 8.19 half clocks).
//! A run of 3 cells only occurs in a preamble: B (3 1 1 3), M (3 3 1 1),
//! W (3 2 1 2). After a preamble, 28 slots: a 2-cell run is a 0, two
//! 1-cell runs a 1. Each subframe's parity (slots 4-31) must be even, a
//! left subframe (B or M) must come before each W, and B every 192
//! frames. The sample is slots 12-27 (16 bits; the design sends 16-bit
//! samples in a 24-bit word).

const std = @import("std");

/// Half pixel clock (DomPix, 39720 ps) over one cell (1 / 6.144 MHz).
const cells_per_half: f64 = 19.86e-9 * 6.144e6;

pub const Receiver = struct {
    level: u1 = 0,
    run: u32 = 0,
    started: bool = false,

    // parser
    pre: [4]u8 = undefined,
    pre_n: u8 = 0,
    in_pre: bool = false,
    kind: u8 = 0, // 'B', 'M', 'W' of the subframe being read, 0 = hunting
    bits: u32 = 0,
    nbits: u5 = 0,
    half_one: bool = false,
    pre_cells: u8 = 0,

    left: ?i16 = null,
    since_b: ?u32 = null,

    /// Decoded stereo frames (left, right).
    frames: std.ArrayList([2]i16) = .empty,
    errors: u32 = 0,
    first_error: ?[]const u8 = null,
    blocks: u32 = 0,

    pub fn deinit(r: *Receiver, gpa: std.mem.Allocator) void {
        r.frames.deinit(gpa);
    }

    fn fail(r: *Receiver, why: []const u8) void {
        r.errors += 1;
        if (r.first_error == null) r.first_error = why;
        r.kind = 0;
        r.in_pre = false;
        r.left = null;
    }

    /// Feed one half clock's line level.
    pub fn feed(r: *Receiver, gpa: std.mem.Allocator, v: u1) !void {
        if (v == r.level) {
            r.run += 1;
            return;
        }
        // The line idles low until the transmitter starts: skip that run.
        if (r.started) try r.cells(gpa, r.run);
        r.started = true;
        r.level = v;
        r.run = 1;
    }

    fn cells(r: *Receiver, gpa: std.mem.Allocator, halves: u32) !void {
        const c: u32 = @intFromFloat(@round(@as(f64, @floatFromInt(halves)) * cells_per_half));
        if (c < 1 or c > 3) return r.fail("run not 1 to 3 cells long");
        if (c == 3 and !r.in_pre) {
            // a new preamble
            if (r.kind != 0 or r.half_one) {
                r.fail("preamble inside a subframe");
            }
            r.in_pre = true;
            r.pre[0] = 3;
            r.pre_n = 1;
            r.pre_cells = 3;
            return;
        }
        if (r.in_pre) {
            r.pre[r.pre_n] = @intCast(c);
            r.pre_n += 1;
            r.pre_cells += @intCast(c);
            if (r.pre_cells < 8) {
                if (r.pre_n == 4) r.fail("bad preamble");
                return;
            }
            if (r.pre_cells > 8 or r.pre_n != 4) return r.fail("bad preamble");
            const p = r.pre;
            r.kind = if (p[1] == 1 and p[2] == 1 and p[3] == 3) 'B' else if (p[1] == 3 and p[2] == 1 and p[3] == 1) 'M' else if (p[1] == 2 and p[2] == 1 and p[3] == 2) 'W' else 0;
            r.in_pre = false;
            if (r.kind == 0) return r.fail("bad preamble");
            r.bits = 0;
            r.nbits = 0;
            r.half_one = false;
            return;
        }
        if (r.kind == 0) return; // hunting for a preamble
        switch (c) {
            2 => {
                if (r.half_one) return r.fail("half a 1 slot");
                try r.slot(gpa, 0);
            },
            1 => {
                if (r.half_one) {
                    r.half_one = false;
                    try r.slot(gpa, 1);
                } else r.half_one = true;
            },
            else => return r.fail("3-cell run in data"),
        }
    }

    fn slot(r: *Receiver, gpa: std.mem.Allocator, b: u1) !void {
        r.bits |= @as(u32, b) << r.nbits;
        if (r.nbits < 27) {
            r.nbits += 1;
            return;
        }
        // slots 4-31 complete (bit i = slot i + 4)
        const kind = r.kind;
        r.kind = 0;
        if (@popCount(r.bits) % 2 != 0) return r.fail("odd parity");
        const smp: i16 = @bitCast(@as(u16, @truncate(r.bits >> 8)));
        switch (kind) {
            'B', 'M' => {
                if (kind == 'B') {
                    if (r.since_b) |n| {
                        if (n != 192) return r.fail("B not every 192 frames");
                    }
                    r.since_b = 0;
                    r.blocks += 1;
                }
                r.left = smp;
            },
            else => {
                const l = r.left orelse return r.fail("W without a left subframe");
                r.left = null;
                try r.frames.append(gpa, .{ l, smp });
                if (r.since_b) |*n| n.* += 1;
            },
        }
    }

    /// Frequency of the left channel from rising zero crossings (with
    /// hysteresis), skipping the first `skip` frames; 0 if fewer than two.
    pub fn frequency(r: *const Receiver, skip: usize) f64 {
        var first: ?usize = null;
        var last: usize = 0;
        var n: usize = 0;
        var high = false;
        if (r.frames.items.len <= skip) return 0;
        for (r.frames.items[skip..], skip..) |f, i| {
            if (!high and f[0] > 1000) {
                high = true;
                if (first == null) first = i else n += 1;
                last = i;
            } else if (high and f[0] < -1000) high = false;
        }
        if (n == 0) return 0;
        return @as(f64, @floatFromInt(n)) * 48000.0 / @as(f64, @floatFromInt(last - first.?));
    }

    /// The frames as a 16-bit stereo 48 kHz WAV file.
    pub fn wav(r: *const Receiver, gpa: std.mem.Allocator) ![]u8 {
        const data_len: u32 = @intCast(r.frames.items.len * 4);
        var out = try gpa.alloc(u8, 44 + data_len);
        const hdr = out[0..44];
        @memcpy(hdr[0..4], "RIFF");
        std.mem.writeInt(u32, hdr[4..8], 36 + data_len, .little);
        @memcpy(hdr[8..16], "WAVEfmt ");
        std.mem.writeInt(u32, hdr[16..20], 16, .little);
        std.mem.writeInt(u16, hdr[20..22], 1, .little); // PCM
        std.mem.writeInt(u16, hdr[22..24], 2, .little);
        std.mem.writeInt(u32, hdr[24..28], 48000, .little);
        std.mem.writeInt(u32, hdr[28..32], 48000 * 4, .little);
        std.mem.writeInt(u16, hdr[32..34], 4, .little);
        std.mem.writeInt(u16, hdr[34..36], 16, .little);
        @memcpy(hdr[36..40], "data");
        std.mem.writeInt(u32, hdr[40..44], data_len, .little);
        for (r.frames.items, 0..) |f, i| {
            std.mem.writeInt(i16, out[44 + i * 4 ..][0..2], f[0], .little);
            std.mem.writeInt(i16, out[44 + i * 4 + 2 ..][0..2], f[1], .little);
        }
        return out;
    }
};

test "a hand-made subframe pair decodes" {
    const gpa = std.testing.allocator;
    var r: Receiver = .{};
    defer r.deinit(gpa);
    // cells -> halves: 8 or 9 halves per cell, alternating
    var cells_buf: [512]u1 = undefined;
    var n: usize = 0;
    var lvl: u1 = 0;
    const put = struct {
        fn f(buf: []u1, k: *usize, l: u1) void {
            buf[k.*] = l;
            k.* += 1;
        }
    }.f;
    for (0..2) |sub| {
        const pre: []const u8 = if (sub == 0) "11101000" else "11100100";
        for (pre) |ch| put(&cells_buf, &n, if (ch == '1') ~lvl else lvl);
        lvl = cells_buf[n - 1];
        // sample 0x1234 left, -2 right, in slots 12-27
        const smp: u16 = if (sub == 0) 0x1234 else 0xFFFE;
        var bits: u32 = @as(u32, smp) << 8;
        if (@popCount(bits) % 2 != 0) bits |= 1 << 27;
        for (0..28) |i| {
            lvl = ~lvl;
            put(&cells_buf, &n, lvl);
            if ((bits >> @intCast(i)) & 1 == 1) lvl = ~lvl;
            put(&cells_buf, &n, lvl);
        }
    }
    // a trailing preamble start so the last run ends
    put(&cells_buf, &n, ~lvl);
    for (cells_buf[0..n], 0..) |c, i| {
        const reps: usize = if (i % 5 == 0) 9 else 8;
        for (0..reps) |_| try r.feed(gpa, c);
    }
    try std.testing.expectEqual(@as(u32, 0), r.errors);
    try std.testing.expectEqual(@as(usize, 1), r.frames.items.len);
    try std.testing.expectEqual([2]i16{ 0x1234, -2 }, r.frames.items[0]);
}
