//! TMDS sink (side track G, layer 4): turns the Arty's four HDMI TX lines,
//! sampled twice per 5x clock (after the falling and after the rising
//! edge, so one sample per bit), back into pixels and syncs, the way a
//! DVI receiver would, and checks the transmitter on the way.
//!
//! * Symbol boundaries come from the clock channel (lane 3), which sends
//!   0000011111 bit 0 first: a symbol starts with the first 1 after a 0.
//!   Every symbol must be exactly 10 samples long.
//! * Each lane's symbol is decoded as DVI 1.0, 3.2.2 says: the four
//!   control symbols, else data (bit 9 undoes the inversion, bit 8 picks
//!   XOR or XNOR).
//! * Each data symbol must be exactly what a DVI encoder would send for
//!   its byte from the lane's running disparity (`encode`, written from
//!   the specification, independent of Platform.Video.Tmds), so a wrong
//!   DC-balance decision fails the run even though it would decode.
//! * All three lanes must agree on data vs. control; lanes 1 and 2 only
//!   send control 00. Lane 0's control bits are (C1, C0) = (VSYNC, HSYNC).
//!
//! The result per symbol is a vga.Pins with 8-bit colour (black outside
//! the visible area, the last control period's sync levels during data),
//! for vga.Decoder with `wide = true`. The n lines must be the
//! complement of the p lines in every sample.
//!
//! HDMI mode (side track G phase 3): eight control symbols with channels 1
//! and 2 at 01 and 01 are a data island's preamble: then two guard band
//! symbols, packets of 32 TERC4 symbols (each packet's header and
//! subpackets reassembled, their BCH ECC checked: `Hdmi`), and two guard
//! band symbols; channel 1 at 01 and channel 2 at 00 is the video
//! preamble, then the two video guard band symbols, then video. During an
//! island channel 0's TERC4 bits 0 and 1 are HSYNC and VSYNC. Audio
//! Sample packets' samples are collected (`Hdmi.audio`) for the tone
//! check, ACR's N and CTS kept, InfoFrames' checksums checked. A DVI
//! stream has neither preamble and is decoded as before.

const std = @import("std");
const vga = @import("vga.zig");

/// What one lane sent in one symbol.
pub const Symbol = union(enum) {
    data: u8,
    control: struct { c1: u1, c0: u1 },
};

/// The control symbols, bit 9 first as DVI 1.0 writes them.
pub fn controlSymbol(c1: u1, c0: u1) u10 {
    return switch (@as(u2, c1) << 1 | c0) {
        0b00 => 0b1101010100,
        0b01 => 0b0010101011,
        0b10 => 0b0101010100,
        0b11 => 0b1010101011,
    };
}

pub fn decode(s: u10) Symbol {
    inline for (.{ .{ 0, 0 }, .{ 0, 1 }, .{ 1, 0 }, .{ 1, 1 } }) |c| {
        if (s == controlSymbol(c[0], c[1])) return .{ .control = .{ .c1 = c[0], .c0 = c[1] } };
    }
    const low: u8 = @truncate(s);
    const qm: u8 = if (s & (1 << 9) != 0) ~low else low;
    const is_xor = s & (1 << 8) != 0;
    var d: u8 = qm & 1;
    var i: u3 = 1;
    while (true) : (i += 1) {
        const prev: u1 = @truncate(qm >> (i - 1));
        const cur: u1 = @truncate(qm >> i);
        const b: u1 = if (is_xor) prev ^ cur else ~(prev ^ cur);
        d |= @as(u8, b) << i;
        if (i == 7) break;
    }
    return .{ .data = d };
}

/// The DVI 1.0 encoder for one data byte (figure 3-5): the disparity
/// before, the symbol, the disparity after.
pub fn encode(cnt: *i8, d: u8) u10 {
    const n1d = @popCount(d);
    const use_xnor = n1d > 4 or (n1d == 4 and d & 1 == 0);
    var qm: u8 = d & 1;
    var i: u3 = 1;
    while (true) : (i += 1) {
        const prev: u1 = @truncate(qm >> (i - 1));
        const cur: u1 = @truncate(d >> i);
        const b: u1 = if (use_xnor) ~(prev ^ cur) else prev ^ cur;
        qm |= @as(u8, b) << i;
        if (i == 7) break;
    }
    const qm8: u10 = if (use_xnor) 0 else 1;
    const n1: i8 = @popCount(qm);
    const n0: i8 = 8 - n1;
    var q: u10 = undefined;
    if (cnt.* == 0 or n1 == n0) {
        q = (if (qm8 == 1) @as(u10, 0) else 1 << 9) | qm8 << 8 | @as(u10, if (qm8 == 1) qm else ~qm);
        cnt.* += if (qm8 == 0) n0 - n1 else n1 - n0;
    } else if ((cnt.* > 0 and n1 > n0) or (cnt.* < 0 and n0 > n1)) {
        q = 1 << 9 | qm8 << 8 | @as(u10, ~qm);
        cnt.* += 2 * @as(i8, @intCast(qm8)) + (n0 - n1);
    } else {
        q = qm8 << 8 | @as(u10, qm);
        cnt.* += -2 * @as(i8, @intCast(1 - qm8)) + (n1 - n0);
    }
    return q;
}

/// TERC4 (HDMI 1.4b table 5-5), bit 0 sent first; index = the 4 data bits.
pub const terc4_table = [16]u10{
    0b1010011100, 0b1001100011, 0b1011100100, 0b1011100010,
    0b0101110001, 0b0100011110, 0b0110001110, 0b0100111100,
    0b1011001100, 0b0100111001, 0b0110011100, 0b1011000110,
    0b1010001110, 0b1001110001, 0b0101100011, 0b1011000011,
};

pub fn terc4Decode(s: u10) ?u4 {
    for (terc4_table, 0..) |t, i| if (t == s) return @intCast(i);
    return null;
}

const island_guard_12: u10 = 0b0100110011; // channels 1 and 2
const video_guard = [3]u10{ 0b1011001100, 0b0100110011, 0b1011001100 };

/// The BCH ECC of `n` bits of `v`, bit 0 first (generator
/// 1 + x^6 + x^7 + x^8, as an LFSR: HDMI 1.4b 5.2.3.5).
pub fn bchEcc(v: u64, n: u7) u8 {
    var e: u8 = 0;
    var i: u7 = 0;
    while (i < n) : (i += 1) {
        const b: u8 = @intCast((v >> @intCast(i)) & 1);
        e = (e >> 1) ^ (if ((e ^ b) & 1 != 0) @as(u8, 0x83) else 0);
    }
    return e;
}

/// What the islands carried (HDMI mode).
pub const Hdmi = struct {
    islands: u64 = 0,
    packets: [256]u32 = [_]u32{0} ** 256,
    bad_ecc: vga.Violation = .{},
    bad_checksum: vga.Violation = .{},
    bad_parity: vga.Violation = .{},
    bad_island: vga.Violation = .{},
    acr_n: ?u32 = null,
    acr_cts: ?u32 = null,
    blocks: u32 = 0,
    /// the audio samples (left, right), in order
    audio: std.ArrayList([2]i16) = .empty,
    gpa: ?std.mem.Allocator = null,

    pub fn deinit(h: *Hdmi) void {
        if (h.gpa) |g| h.audio.deinit(g);
    }

    /// A packet's 32 pixels of nibbles (lanes 0..2).
    fn packet(h: *Hdmi, now: u64, nib: *const [32][3]u4) void {
        var hdr: u32 = 0;
        var sub = [4]u64{ 0, 0, 0, 0 };
        for (nib, 0..) |n, i| {
            hdr |= @as(u32, (n[0] >> 2) & 1) << @intCast(i);
            for (0..4) |k| {
                sub[k] |= @as(u64, (n[1] >> @intCast(k)) & 1) << @intCast(2 * i);
                sub[k] |= @as(u64, (n[2] >> @intCast(k)) & 1) << @intCast(2 * i + 1);
            }
        }
        if (bchEcc(hdr, 24) != @as(u8, @truncate(hdr >> 24))) h.bad_ecc.record(now, hdr);
        for (sub) |sp| if (bchEcc(sp, 56) != @as(u8, @truncate(sp >> 56))) h.bad_ecc.record(now, sp);
        const ty: u8 = @truncate(hdr);
        h.packets[ty] += 1;
        const byte = struct {
            fn of(v: u64, i: u6) u8 {
                return @truncate(v >> (8 * i));
            }
        }.of;
        switch (ty) {
            0x01 => {
                h.acr_cts = @as(u32, byte(sub[0], 1) & 0xF) << 16 | @as(u32, byte(sub[0], 2)) << 8 | byte(sub[0], 3);
                h.acr_n = @as(u32, byte(sub[0], 4) & 0xF) << 16 | @as(u32, byte(sub[0], 5)) << 8 | byte(sub[0], 6);
            },
            0x02 => {
                const present: u8 = @truncate(hdr >> 8);
                const bflags: u8 = @truncate(hdr >> 20);
                for (0..4) |k| {
                    if ((present >> @intCast(k)) & 1 == 0) continue;
                    if ((bflags >> @intCast(k)) & 1 != 0) h.blocks += 1;
                    const sp = sub[k];
                    const l: u24 = @truncate(sp);
                    const r: u24 = @truncate(sp >> 24);
                    const fl = byte(sp, 6);
                    // even parity over the 24 bits and V, U, C, P
                    if ((@popCount(l) + @popCount(fl & 0x0F)) % 2 != 0) h.bad_parity.record(now, l);
                    if ((@popCount(r) + @popCount(fl & 0xF0)) % 2 != 0) h.bad_parity.record(now, r);
                    if (h.gpa) |g| h.audio.append(g, .{ @bitCast(@as(u16, @truncate(l >> 8))), @bitCast(@as(u16, @truncate(r >> 8))) }) catch {};
                }
            },
            0x82, 0x84 => {
                // header and the bytes up to the length sum to 0
                var sum: u32 = byte(hdr, 0) +% byte(hdr, 1) +% byte(hdr, 2);
                const len = byte(hdr, 2);
                var i: u32 = 0;
                while (i <= len) : (i += 1) sum +%= byte(sub[i / 7], @intCast(i % 7));
                if (sum & 0xFF != 0) h.bad_checksum.record(now, ty);
            },
            else => {},
        }
    }

    pub fn report(h: *const Hdmi) bool {
        std.debug.print("hdmi: {d} data island(s); packets: {d} audio sample, {d} ACR (N {?d}, CTS {?d}), {d} AVI, {d} Audio InfoFrame, {d} null; {d} sample(s), {d} block start(s)\n", .{ h.islands, h.packets[2], h.packets[1], h.acr_n, h.acr_cts, h.packets[0x82], h.packets[0x84], h.packets[0], h.audio.items.len, h.blocks });
        var ok = true;
        inline for (.{ .{ "packet ECC", "bad_ecc" }, .{ "InfoFrame checksums", "bad_checksum" }, .{ "sample parity", "bad_parity" }, .{ "island structure (guard bands, TERC4)", "bad_island" } }) |c| {
            const v = @field(h, c[1]);
            if (v.count == 0) {
                std.debug.print("hdmi:   {s}: ok\n", .{c[0]});
            } else {
                std.debug.print("hdmi:   {s}: FAIL {d} time(s), first at symbol {d}\n", .{ c[0], v.count, v.first_clock });
                ok = false;
            }
        }
        return ok;
    }
};

const Mode = enum { normal, island_lead, packet, island_trail, video_guard };

pub const Sink = struct {
    /// The bits of the symbol being received, per lane, bit 0 first.
    acc: [4]u10 = .{ 0, 0, 0, 0 },
    /// Samples since the last symbol boundary (null before the first).
    n: ?u32 = null,
    prev_clk: u1 = 1,
    disparity: [3]i8 = .{ 0, 0, 0 },
    hs: u8 = 1,
    vs: u8 = 1,
    // HDMI mode
    hdmi: Hdmi = .{},
    mode: Mode = .normal,
    island_pre: u32 = 0,
    video_pre: u32 = 0,
    left: u8 = 0,
    nib: [32][3]u4 = undefined,
    pix: u6 = 0,

    symbols: u64 = 0,
    bad_length: vga.Violation = .{},
    bad_pair: vga.Violation = .{},
    bad_encoding: vga.Violation = .{},
    bad_mix: vga.Violation = .{},
    samples: u64 = 0,

    /// One sample of the p lines (bit i = lane i; lane 3 the clock) and
    /// the n lines. Returns the pins of a symbol when one has just ended.
    pub fn feed(s: *Sink, p: u4, n: u4) ?vga.Pins {
        const now = s.samples;
        s.samples += 1;
        if (p != ~n) s.bad_pair.record(now, n);
        const clk: u1 = @truncate(p >> 3);
        defer s.prev_clk = clk;
        var out: ?vga.Pins = null;
        if (clk == 1 and s.prev_clk == 0) {
            if (s.n) |len| {
                if (len == 10) out = s.symbol(now) else s.bad_length.record(now, len);
            }
            s.n = 0;
        }
        if (s.n) |*len| {
            if (len.* < 10) {
                for (0..4) |lane| s.acc[lane] |= @as(u10, (p >> @intCast(lane)) & 1) << @intCast(len.*);
            }
            len.* += 1;
        }
        return out;
    }

    /// Pins for a blanking symbol with the syncs from channel 0's TERC4
    /// bits (in an island).
    fn islandPins(s: *Sink, now: u64) vga.Pins {
        const d = terc4Decode(s.acc[0]) orelse blk: {
            s.hdmi.bad_island.record(now, s.acc[0]);
            break :blk 0;
        };
        s.hs = d & 1;
        s.vs = (d >> 1) & 1;
        return .{ .ld = 0, .r = 0, .g = 0, .b = 0, .hs = s.hs, .vs = s.vs };
    }

    fn isIslandGuard(s: *const Sink) bool {
        if (s.acc[1] != island_guard_12 or s.acc[2] != island_guard_12) return false;
        const d = terc4Decode(s.acc[0]) orelse return false;
        return d >> 2 == 0b11;
    }

    fn symbol(s: *Sink, now: u64) vga.Pins {
        s.symbols += 1;
        defer s.acc = .{ 0, 0, 0, 0 };
        switch (s.mode) {
            .island_lead => {
                if (!s.isIslandGuard()) s.hdmi.bad_island.record(now, s.acc[1]);
                s.mode = .packet;
                s.pix = 0;
                return s.islandPins(now);
            },
            .packet => {
                if (s.pix == 0 and s.isIslandGuard()) {
                    // the trailing guard band
                    s.mode = .island_trail;
                    return s.islandPins(now);
                }
                for (0..3) |lane| s.nib[s.pix][lane] = terc4Decode(s.acc[lane]) orelse blk: {
                    s.hdmi.bad_island.record(now, s.acc[lane]);
                    break :blk 0;
                };
                const pins = s.islandPins(now);
                s.pix += 1;
                if (s.pix == 32) {
                    s.hdmi.packet(now, &s.nib);
                    s.pix = 0;
                }
                return pins;
            },
            .island_trail => {
                if (!s.isIslandGuard()) s.hdmi.bad_island.record(now, s.acc[1]);
                s.mode = .normal;
                return s.islandPins(now);
            },
            .video_guard => {
                for (0..3) |lane| if (s.acc[lane] != video_guard[lane]) s.hdmi.bad_island.record(now, s.acc[lane]);
                s.left -= 1;
                if (s.left == 0) s.mode = .normal;
                s.disparity = .{ 0, 0, 0 };
                return .{ .ld = 0, .r = 0, .g = 0, .b = 0, .hs = s.hs, .vs = s.vs };
            },
            .normal => {},
        }
        // a run of preamble symbols ends with a guard band
        const c1 = decode(s.acc[1]);
        const c2 = decode(s.acc[2]);
        const ctl01 = Symbol{ .control = .{ .c1 = 0, .c0 = 1 } };
        const ctl00 = Symbol{ .control = .{ .c1 = 0, .c0 = 0 } };
        const is_ctl = decode(s.acc[0]) == .control and c1 == .control and c2 == .control;
        if (!is_ctl and s.island_pre >= 8) {
            s.island_pre = 0;
            s.hdmi.islands += 1;
            if (!s.isIslandGuard()) s.hdmi.bad_island.record(now, s.acc[1]);
            s.mode = .island_lead;
            return s.islandPins(now);
        }
        if (!is_ctl and s.video_pre >= 8) {
            s.video_pre = 0;
            for (0..3) |lane| if (s.acc[lane] != video_guard[lane]) s.hdmi.bad_island.record(now, s.acc[lane]);
            s.mode = .video_guard;
            s.left = 1;
            s.disparity = .{ 0, 0, 0 };
            return .{ .ld = 0, .r = 0, .g = 0, .b = 0, .hs = s.hs, .vs = s.vs };
        }
        if (is_ctl and std.meta.eql(c1, ctl01) and std.meta.eql(c2, ctl01)) {
            s.island_pre += 1;
            s.video_pre = 0;
        } else if (is_ctl and std.meta.eql(c1, ctl01) and std.meta.eql(c2, ctl00)) {
            s.video_pre += 1;
            s.island_pre = 0;
        } else {
            s.island_pre = 0;
            s.video_pre = 0;
        }
        if (is_ctl and !std.meta.eql(c1, ctl00)) {
            // a preamble symbol: blanking with the syncs, nothing else to check
            const c = decode(s.acc[0]).control;
            s.hs = c.c0;
            s.vs = c.c1;
            s.disparity = .{ 0, 0, 0 };
            return .{ .ld = 0, .r = 0, .g = 0, .b = 0, .hs = s.hs, .vs = s.vs };
        }
        var syms: [3]Symbol = undefined;
        for (0..3) |lane| syms[lane] = decode(s.acc[lane]);
        const data = syms[0] == .data;
        for (syms[1..]) |y| {
            if ((y == .data) != data) s.bad_mix.record(now, s.acc[0]);
            if (y == .control and (y.control.c1 != 0 or y.control.c0 != 0)) s.bad_mix.record(now, s.acc[1]);
        }
        if (!data) {
            const c = syms[0].control;
            s.hs = c.c0;
            s.vs = c.c1;
            s.disparity = .{ 0, 0, 0 };
            return .{ .ld = 0, .r = 0, .g = 0, .b = 0, .hs = s.hs, .vs = s.vs };
        }
        var rgb: [3]u8 = .{ 0, 0, 0 };
        for (0..3) |lane| {
            if (syms[lane] != .data) continue;
            const d = syms[lane].data;
            if (encode(&s.disparity[lane], d) != s.acc[lane]) s.bad_encoding.record(now, s.acc[lane]);
            rgb[lane] = d;
        }
        return .{ .ld = 0, .r = rgb[2], .g = rgb[1], .b = rgb[0], .hs = s.hs, .vs = s.vs };
    }

    /// Print the checks. Returns false if any failed.
    pub fn report(s: *const Sink) bool {
        std.debug.print("tmds: {d} sample(s), {d} symbol(s)\n", .{ s.samples, s.symbols });
        var ok = true;
        ok = check("symbol length (10 samples)", s.bad_length) and ok;
        ok = check("n = complement of p", s.bad_pair) and ok;
        ok = check("data symbols as a DVI encoder sends them", s.bad_encoding) and ok;
        ok = check("lanes agree (data/control; lanes 1, 2 control 00)", s.bad_mix) and ok;
        if (s.symbols == 0) {
            std.debug.print("tmds: FAIL no symbols (no clock channel?)\n", .{});
            ok = false;
        }
        return ok;
    }

    fn check(name: []const u8, v: vga.Violation) bool {
        if (v.count == 0) {
            std.debug.print("tmds:   {s}: ok\n", .{name});
            return true;
        }
        std.debug.print("tmds:   {s}: FAIL {d} time(s), first at sample {d} (value {d})\n", .{ name, v.count, v.first_clock, v.first_value });
        return false;
    }
};

test "decode undoes encode for every byte from every disparity reached" {
    var seen = [_]bool{false} ** 19;
    var cnts = std.ArrayList(i8).empty;
    defer cnts.deinit(std.testing.allocator);
    try cnts.append(std.testing.allocator, 0);
    seen[9] = true;
    var k: usize = 0;
    while (k < cnts.items.len) : (k += 1) {
        var x: u16 = 0;
        while (x < 256) : (x += 1) {
            var c = cnts.items[k];
            const q = encode(&c, @intCast(x));
            try std.testing.expectEqual(Symbol{ .data = @intCast(x) }, decode(q));
            try std.testing.expect(c >= -8 and c <= 8);
            const idx: usize = @intCast(c + 9);
            if (!seen[idx]) {
                seen[idx] = true;
                try cnts.append(std.testing.allocator, c);
            }
        }
    }
    try std.testing.expectEqual(@as(usize, 9), cnts.items.len); // -8, -6, .., 8
}

test "control symbols decode to their bits; 0x00 from 0 encodes to 0100000000" {
    try std.testing.expectEqual(Symbol{ .control = .{ .c1 = 1, .c0 = 0 } }, decode(0b0101010100));
    try std.testing.expectEqual(Symbol{ .control = .{ .c1 = 0, .c0 = 0 } }, decode(0b1101010100));
    var c: i8 = 0;
    try std.testing.expectEqual(@as(u10, 0b0100000000), encode(&c, 0));
}

test "sink: symbols from a serialised stream, aligned on the clock lane" {
    var s: Sink = .{};
    // lane 0: control (VSYNC 1, HSYNC 0), then the byte 0x00; lanes 1, 2:
    // control 00, then 0x00; clock lane 0000011111.
    var stream: [4][3]u10 = undefined;
    for (0..3) |lane| stream[lane] = .{ if (lane == 0) controlSymbol(1, 0) else controlSymbol(0, 0), controlSymbol(0, 0), 0b0100000000 };
    stream[0][1] = controlSymbol(1, 0);
    stream[3] = .{ 0b0000011111, 0b0000011111, 0b0000011111 };
    var got: [3]?vga.Pins = .{ null, null, null };
    var g: usize = 0;
    // a few samples of clock-lane 0 first, as after reset
    _ = s.feed(0, 0xF);
    for (0..3) |sym| {
        for (0..10) |bit| {
            var p: u4 = 0;
            for (0..4) |lane| p |= @as(u4, @intCast((stream[lane][sym] >> @intCast(bit)) & 1)) << @intCast(lane);
            if (s.feed(p, ~p)) |pins| {
                got[g] = pins;
                g += 1;
            }
        }
    }
    if (s.feed(0b1000, 0b0111)) |pins| { // the next symbol's first bit ends the last one
        got[g] = pins;
        g += 1;
    }
    try std.testing.expectEqual(@as(usize, 3), g);
    try std.testing.expectEqual(@as(u8, 0), got[2].?.r);
    try std.testing.expectEqual(@as(u64, 0), s.bad_encoding.count);
    try std.testing.expectEqual(@as(u8, 0), got[0].?.hs);
    try std.testing.expectEqual(@as(u8, 1), got[0].?.vs);
    try std.testing.expectEqual(@as(u64, 0), s.bad_length.count + s.bad_pair.count + s.bad_mix.count);
}

test "HDMI: a data island with one ACR packet, its preamble and guard bands; N, CTS and the ECC come back" {
    var s: Sink = .{};
    defer s.hdmi.deinit();
    // the packet: header 01 00 00, every subpacket 00 00 62 56 00 18 00
    // (CTS 25174, N 6144), with their ECCs
    const hdr0: u64 = 0x01;
    const hdr: u32 = @intCast(hdr0 | @as(u64, bchEcc(hdr0, 24)) << 24);
    const sb0: u64 = 0x00_18_00_56_62_00_00; // SB0 in the low byte
    const sub: u64 = sb0 | @as(u64, bchEcc(sb0, 56)) << 56;
    var syms: [64][3]u10 = undefined;
    var n: usize = 0;
    const ctl = struct {
        fn c(c1: u1, c0: u1) u10 {
            return controlSymbol(c1, c0);
        }
    }.c;
    for (0..4) |_| {
        syms[n] = .{ ctl(1, 1), ctl(0, 0), ctl(0, 0) };
        n += 1;
    }
    for (0..8) |_| {
        syms[n] = .{ ctl(1, 1), ctl(0, 1), ctl(0, 1) };
        n += 1;
    }
    const guard = [3]u10{ terc4_table[0b1111], island_guard_12, island_guard_12 };
    syms[n] = guard;
    syms[n + 1] = guard;
    n += 2;
    for (0..32) |i| {
        const b0: u4 = @intCast((if (i != 0) @as(u4, 8) else 0) | @as(u4, @intCast((hdr >> @intCast(i)) & 1)) << 2 | 0b11);
        var b1: u4 = 0;
        var b2: u4 = 0;
        for (0..4) |k| {
            b1 |= @as(u4, @intCast((sub >> @intCast(2 * i)) & 1)) << @intCast(k);
            b2 |= @as(u4, @intCast((sub >> @intCast(2 * i + 1)) & 1)) << @intCast(k);
        }
        syms[n] = .{ terc4_table[b0], terc4_table[b1], terc4_table[b2] };
        n += 1;
    }
    syms[n] = guard;
    syms[n + 1] = guard;
    n += 2;
    for (0..4) |_| {
        syms[n] = .{ ctl(1, 1), ctl(0, 0), ctl(0, 0) };
        n += 1;
    }
    _ = s.feed(0, 0xF);
    for (syms[0..n]) |sym| {
        for (0..10) |bit| {
            var p: u4 = @as(u4, @intCast((@as(u10, 0b0000011111) >> @intCast(bit)) & 1)) << 3;
            for (0..3) |lane| p |= @as(u4, @intCast((sym[lane] >> @intCast(bit)) & 1)) << @intCast(lane);
            _ = s.feed(p, ~p);
        }
    }
    _ = s.feed(0b1000, 0b0111);
    try std.testing.expectEqual(@as(u64, 1), s.hdmi.islands);
    try std.testing.expectEqual(@as(u32, 1), s.hdmi.packets[1]);
    try std.testing.expectEqual(@as(?u32, 6144), s.hdmi.acr_n);
    try std.testing.expectEqual(@as(?u32, 25174), s.hdmi.acr_cts);
    try std.testing.expectEqual(@as(u64, 0), s.hdmi.bad_ecc.count + s.hdmi.bad_island.count);
    try std.testing.expectEqual(@as(u64, 0), s.bad_mix.count + s.bad_encoding.count + s.bad_length.count);
}
