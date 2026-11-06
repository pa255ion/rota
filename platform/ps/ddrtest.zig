//! The PS side of side track C's DDR test (step 3): drives M0e.Tester
//! (systems/m0e) over the PS link, for the relay's T command (`make
//! ddr-test`). For each run it writes the words the PL will read into DDR
//! (or keeps what a W command loaded), starts the PL, optionally keeps the
//! CPU busy on DDR meanwhile, then reads the PL's result bytes back and
//! compares its checksum with its own over the same words.
//!
//! The PL's registers and result bytes: M0e.Tester. Result bytes come back
//! on PS_IN's byte channel, one per selection (a link write at 0x010000 + i,
//! then the toggle flips).
//!
//! Generic over the link's GPIO port and over the DDR (`Mem`: read64,
//! write64, nowUs), so the tests below run it against a model of the PL.

const pl = @import("pl.zig");

/// One run: where, how (M0e.Tester's mode byte), how many words.
pub const Run = struct {
    base: u32,
    mode: u8,
    words: u32,
    /// the CPU reads and writes DDR while the PL runs
    busy: bool = false,
    /// read what is in DDR (a W command's data) instead of writing the
    /// test pattern first
    keep: bool = false,
};

/// The standard suite (T with no arguments).
pub const suite = [_]Run{
    // one word after another: mostly hits, with prefetch
    .{ .base = 0x0100_0000, .mode = 0x00, .words = 65536 },
    .{ .base = 0x0100_0000, .mode = 0x00, .words = 65536, .busy = true },
    // one word per line: every request is a new line, prefetch ahead
    .{ .base = 0x0100_0000, .mode = 0x02, .words = 16384 },
    // every other line: prefetch reads lines nobody uses; plain misses
    .{ .base = 0x0100_0000, .mode = 0x03, .words = 16384 },
    .{ .base = 0x0100_0000, .mode = 0x03, .words = 16384, .busy = true },
    // random words in 1 MiB and in 64 MiB
    .{ .base = 0x0100_0000, .mode = 0x80, .words = 16384 },
    .{ .base = 0x0100_0000, .mode = 0x80, .words = 16384, .busy = true },
    .{ .base = 0x0400_0000, .mode = 0x80, .words = 16384 },
};

/// M0e's system ID on PS_IN (Platform.Ps.Link.systemM0e).
pub const system_m0e: u8 = 4;
/// Latency buckets (M0e.Tester.bucket): 0 to 31, then powers of two.
pub const buckets = 43;
/// The PL's clock: the pixel clock, 25.175 MHz (39.72 ns).
pub const clock_ps: u64 = 39_720;
/// Where the PL can't read (OCM is mapped low) or the CPU writes nothing.
pub const ddr_low: u32 = 0x0010_0000;
pub const ddr_end: u32 = 0x2000_0000;
/// The CPU's own DDR traffic during a busy run.
pub const busy_base: u32 = 0x1000_0000;
pub const busy_words: u32 = 1 << 17;

pub const Results = struct {
    status: u8,
    sum: u32,
    cycles: u32,
    min: u16,
    max: u16,
    count: u32,
    hist: [buckets]u32,
};

/// The DDR's test pattern (the Verilator harness's too: platform/sim/axi.zig).
pub fn pattern(addr: u32) u64 {
    return (@as(u64, addr & ~@as(u32, 7)) *% 0x9E3779B97F4A7C15) ^ 0x0123456789ABCDEF;
}

/// M0e.Tester.checksumStep.
pub fn checksumStep(c: u32, w: u64) u32 {
    return ((c << 1) | (c >> 31)) ^ @as(u32, @truncate(w)) ^ @as(u32, @truncate(w >> 32));
}

/// M0e.Tester.xorshift32 (seeded with rngSeed).
pub const rng_seed: u32 = 0x2545F491;
pub fn xorshift32(x0: u32) u32 {
    var x = x0;
    x ^= x << 13;
    x ^= x >> 17;
    x ^= x << 5;
    return x;
}

/// The mask for random addresses: the smallest power of two that holds
/// the run's words, minus one.
pub fn randomMask(words: u32) u32 {
    var span: u32 = 8;
    while (span < words *% 8 and span < (1 << 28)) span <<= 1;
    return span - 1;
}

/// The addresses a run reads, in order (as M0e.Tester makes them).
pub const Addrs = struct {
    run: Run,
    i: u32 = 0,
    rng: u32 = rng_seed,

    pub fn next(a: *Addrs) ?u32 {
        if (a.i >= a.run.words) return null;
        defer a.i += 1;
        if (a.run.mode & 0x80 != 0) {
            const x = a.rng;
            a.rng = xorshift32(a.rng);
            return a.run.base +% (x & randomMask(a.run.words) & ~@as(u32, 7));
        }
        const stride = @as(u32, 8) << @intCast(a.run.mode & 7);
        return a.run.base +% a.i *% stride;
    }

    /// One past the highest byte the run can read.
    pub fn end(run: Run) u64 {
        if (run.mode & 0x80 != 0) return @as(u64, run.base) + randomMask(run.words) + 1;
        const stride = @as(u64, 8) << @intCast(run.mode & 7);
        return @as(u64, run.base) + @as(u64, run.words) * stride;
    }
};

/// Why a run can't be made, or null.
pub fn check(run: Run) ?[]const u8 {
    if (run.base < ddr_low or run.base & 7 != 0) return "base below 1 MiB or not 8-byte aligned";
    if (Addrs.end(run) > ddr_end) return "the run reaches past the 512 MiB of DDR";
    if (run.mode & 0x78 != 0) return "mode: bits 3 to 6 must be 0";
    if (run.busy and Addrs.end(run) > busy_base and run.base < busy_base + 8 * busy_words)
        return "overlaps the CPU's busy area (0x10000000, 1 MiB)";
    return null;
}

pub fn Tester(comptime Port: type, comptime Mem: type) type {
    return struct {
        const Self = @This();
        link: *pl.Link(Port),
        mem: *Mem,
        byte_in: pl.ByteIn = .{},

        /// Fill (unless `keep`), start, wait (busy or not), read the
        /// results; also the PS's own checksum and the fill time.
        pub fn go(t: *Self, run: Run) !Report {
            var rep: Report = .{ .run = run };
            // the words, and the checksum the PL should get
            const t0 = t.mem.nowUs();
            var a: Addrs = .{ .run = run };
            var sum: u32 = 0;
            while (a.next()) |addr| {
                if (!run.keep) t.mem.write64(addr, pattern(addr));
                sum = checksumStep(sum, t.mem.read64(addr));
            }
            rep.fill_us = t.mem.nowUs() - t0;
            rep.expect = sum;

            var regs: [17]u8 = undefined;
            le32(regs[0..4], run.base);
            le32(regs[4..8], run.words);
            regs[8] = run.mode;
            @memset(regs[9..12], 0);
            le32(regs[12..16], if (run.mode & 0x80 != 0) randomMask(run.words) else 0);
            regs[16] = 1; // start
            t.link.write(0, &regs);

            // wait for the end: status bit 1, or bit 2 (AXI error)
            const start = t.mem.nowUs();
            var n: u32 = 0;
            while (true) {
                if (run.busy) t.mem.hammer(&n);
                const s = try t.byte(0);
                if (s & 0x06 != 0) break;
                if (t.mem.nowUs() - start > 5_000_000) return error.Timeout;
            }
            rep.busy_words = n;
            rep.res = try t.results();
            return rep;
        }

        /// Result byte i (select it, wait for the toggle).
        pub fn byte(t: *Self, i: u8) !u8 {
            _ = t.byte_in.poll(t.link.inputs()); // learn the toggle
            t.link.write(0x010000 + @as(u24, i), &.{0});
            return t.next();
        }

        fn next(t: *Self) !u8 {
            var tries: u32 = 0;
            while (tries < 1000) : (tries += 1) {
                if (t.byte_in.poll(t.link.inputs())) |b| return b;
                t.link.port.delayUs(1);
            }
            return error.NoAnswer;
        }

        pub fn results(t: *Self) !Results {
            var b: [32 + 4 * buckets]u8 = undefined;
            b[0] = try t.byte(0);
            // the rest one after another: the link address advances
            var i: usize = 1;
            while (i < b.len) : (i += 1) {
                t.link.data(&.{0});
                b[i] = try t.next();
            }
            var r: Results = .{
                .status = b[0],
                .sum = rd32(b[4..8]),
                .cycles = rd32(b[8..12]),
                .min = rd16(b[12..14]),
                .max = rd16(b[14..16]),
                .count = rd32(b[16..20]),
                .hist = undefined,
            };
            for (&r.hist, 0..) |*h, k| h.* = rd32(b[32 + 4 * k ..][0..4]);
            return r;
        }
    };
}

pub const Report = struct {
    run: Run,
    res: Results = undefined,
    expect: u32 = 0,
    fill_us: u64 = 0,
    busy_words: u32 = 0,

    pub fn ok(r: Report) bool {
        return r.res.status & 0x06 == 0x02 and r.res.sum == r.expect and r.res.count == r.run.words;
    }

    /// Text for the desktop, one "t ..." line at a time through `put`.
    pub fn print(r: Report, put: anytype) void {
        var buf: [160]u8 = undefined;
        const ns = struct {
            fn f(clocks: u64) u64 {
                return clocks * clock_ps / 1000;
            }
        }.f;
        put.line(fmt(&buf, "t run base={x:0>8} mode={x:0>2} words={d}{s}{s}", .{ r.run.base, r.run.mode, r.run.words, if (r.run.busy) " cpu-busy" else "", if (r.run.keep) " loaded-data" else "" }));
        put.line(fmt(&buf, "t   {s}: pl sum {x:0>8}, ps sum {x:0>8}, {d} words, status {x:0>2}", .{ if (r.ok()) "ok" else "FAIL", r.res.sum, r.expect, r.res.count, r.res.status }));
        const bytes = @as(u64, r.res.count) * 8;
        const t_ns = ns(r.res.cycles);
        const mbs = if (t_ns == 0) 0 else bytes * 1000 / t_ns;
        put.line(fmt(&buf, "t   {d} clocks ({d} us): {d} MB/s to the core; latency {d} to {d} clocks ({d} to {d} ns)", .{ r.res.cycles, t_ns / 1000, mbs, r.res.min, r.res.max, ns(r.res.min), ns(r.res.max) }));
        // the histogram, non-empty buckets only
        var line: [160]u8 = undefined;
        var len: usize = 0;
        const head = "t   latency (clocks: count)";
        @memcpy(line[0..head.len], head);
        len = head.len;
        for (r.res.hist, 0..) |h, k| {
            if (h == 0) continue;
            var item: [40]u8 = undefined;
            const lo: u32 = if (k < 32) @intCast(k) else @as(u32, 1) << @intCast(k - 32 + 5);
            const s = if (k < 32)
                fmt(&item, " {d}: {d}", .{ lo, h })
            else
                fmt(&item, " {d}-{d}: {d}", .{ lo, 2 * lo - 1, h });
            if (len + s.len > line.len) {
                put.line(line[0..len]);
                @memcpy(line[0..5], "t    ");
                len = 5;
            }
            @memcpy(line[len..][0..s.len], s);
            len += s.len;
        }
        put.line(line[0..len]);
        const fill_mbs = if (r.fill_us == 0) 0 else bytes / r.fill_us;
        if (!r.run.keep) put.line(fmt(&buf, "t   cpu wrote and read back {d} bytes in {d} us ({d} MB/s; the GPIO link takes 4 us per byte)", .{ bytes, r.fill_us, fill_mbs }));
        if (r.run.busy) put.line(fmt(&buf, "t   cpu meanwhile: {d} words read and written in DDR", .{r.busy_words}));
    }
};

fn fmt(buf: []u8, comptime f: []const u8, args: anytype) []const u8 {
    return @import("std").fmt.bufPrint(buf, f, args) catch buf[0..0];
}

fn le32(b: *[4]u8, v: u32) void {
    b.* = .{ @truncate(v), @truncate(v >> 8), @truncate(v >> 16), @truncate(v >> 24) };
}

fn rd32(b: *const [4]u8) u32 {
    return @as(u32, b[0]) | @as(u32, b[1]) << 8 | @as(u32, b[2]) << 16 | @as(u32, b[3]) << 24;
}

fn rd16(b: *const [2]u8) u16 {
    return @as(u16, b[0]) | @as(u16, b[1]) << 8;
}

// --- Tests: a model of M0e.Tester behind the link ----------------------------
const std = @import("std");
const testing = std.testing;

/// The PL: link writes decoded as Platform.Ps.Link does, M0e.Tester's
/// registers, a run that completes at once (reading `mem`), the byte
/// channel.
const ModelM0e = struct {
    mem: *ModelMem,
    strobe: bool = false,
    addr: u24 = 0,
    regs: [17]u8 = [_]u8{0} ** 17,
    results: [256]u8 = [_]u8{0} ** 256,
    byte: u8 = 0,
    toggle: bool = false,

    pub fn write(m: *ModelM0e, v: u32) void {
        const st = v & (1 << 24) != 0;
        if (st == m.strobe) return;
        m.strobe = st;
        const d: u8 = @truncate(v >> 16);
        if (v & (1 << 25) != 0) {
            m.addr = (m.addr << 8) | d;
            return;
        }
        const a = m.addr;
        m.addr +%= 1;
        if (a < 17) {
            m.regs[a] = d;
            if (a == 16 and d & 1 != 0) m.runTest();
        } else if (a >= 0x010000 and a < 0x010100) {
            m.byte = m.results[a - 0x010000];
            m.toggle = !m.toggle;
        }
    }
    pub fn read(m: *ModelM0e) u32 {
        return @as(u32, m.byte) << 24 | @as(u32, 4) << 16 | (if (m.toggle) @as(u32, 1 << 5) else 0);
    }
    pub fn delayUs(_: *ModelM0e, _: u32) void {}

    fn runTest(m: *ModelM0e) void {
        const run: Run = .{ .base = rd32(m.regs[0..4]), .words = rd32(m.regs[4..8]), .mode = m.regs[8] };
        std.debug.assert(rd32(m.regs[12..16]) == (if (run.mode & 0x80 != 0) randomMask(run.words) else 0));
        var a: Addrs = .{ .run = run };
        var sum: u32 = 0;
        while (a.next()) |addr| sum = checksumStep(sum, m.mem.read64(addr));
        @memset(&m.results, 0);
        m.results[0] = 0x02;
        le32(m.results[4..8], sum);
        le32(m.results[8..12], 1234);
        m.results[12] = 1;
        m.results[14] = 40;
        le32(m.results[16..20], run.words);
        le32(m.results[32 + 4 * 1 ..][0..4], run.words - 3);
        le32(m.results[32 + 4 * 33 ..][0..4], 3);
    }
};

const ModelMem = struct {
    words: std.AutoHashMapUnmanaged(u32, u64) = .empty,
    t: u64 = 0,
    hammered: u32 = 0,

    pub fn write64(m: *ModelMem, a: u32, v: u64) void {
        m.words.put(testing.allocator, a, v) catch unreachable;
    }
    pub fn read64(m: *ModelMem, a: u32) u64 {
        return m.words.get(a) orelse 0;
    }
    pub fn nowUs(m: *ModelMem) u64 {
        m.t += 1;
        return m.t;
    }
    pub fn hammer(m: *ModelMem, n: *u32) void {
        m.hammered += 1;
        n.* += 4;
    }
};

const Capture = struct {
    text: std.ArrayList(u8) = .empty,
    pub fn line(c: *Capture, s: []const u8) void {
        c.text.appendSlice(testing.allocator, s) catch unreachable;
        c.text.append(testing.allocator, '\n') catch unreachable;
    }
};

test "a run: pattern written, PL started with the registers, results read back and checked" {
    var mem: ModelMem = .{};
    defer mem.words.deinit(testing.allocator);
    var pl_model: ModelM0e = .{ .mem = &mem };
    var link: pl.Link(ModelM0e) = .{ .port = &pl_model };
    var t: Tester(ModelM0e, ModelMem) = .{ .link = &link, .mem = &mem };
    for ([_]Run{
        .{ .base = 0x0100_0000, .mode = 0, .words = 300 },
        .{ .base = 0x0100_0000, .mode = 3, .words = 50, .busy = true },
        .{ .base = 0x0100_0000, .mode = 0x80, .words = 200 },
    }) |run| {
        const rep = try t.go(run);
        try testing.expect(rep.ok());
        try testing.expectEqual(run.words, rep.res.count);
        try testing.expectEqual(@as(u32, 3), rep.res.hist[33]);
        try testing.expectEqual(@as(u16, 40), rep.res.max);
        var cap: Capture = .{};
        defer cap.text.deinit(testing.allocator);
        rep.print(&cap);
        try testing.expect(std.mem.indexOf(u8, cap.text.items, "ok: pl sum") != null);
        try testing.expect(std.mem.indexOf(u8, cap.text.items, " 1: ") != null);
        try testing.expect(std.mem.indexOf(u8, cap.text.items, " 64-127: 3") != null);
    }
    try testing.expect(mem.hammered > 0);
}

test "kept data: the PS's checksum is over what is in DDR" {
    var mem: ModelMem = .{};
    defer mem.words.deinit(testing.allocator);
    for (0..64) |i| mem.write64(0x0200_0000 + 8 * @as(u32, @intCast(i)), i * 0x0101010101);
    var pl_model: ModelM0e = .{ .mem = &mem };
    var link: pl.Link(ModelM0e) = .{ .port = &pl_model };
    var t: Tester(ModelM0e, ModelMem) = .{ .link = &link, .mem = &mem };
    const rep = try t.go(.{ .base = 0x0200_0000, .mode = 0, .words = 64, .keep = true });
    try testing.expect(rep.ok());
    try testing.expectEqual(@as(u64, 63 * 0x0101010101), mem.read64(0x0200_0000 + 8 * 63));
}

test "addresses and the mask as M0e.Tester makes them" {
    var a: Addrs = .{ .run = .{ .base = 0x0100_0000, .mode = 3, .words = 3 } };
    try testing.expectEqual(@as(?u32, 0x0100_0000), a.next());
    try testing.expectEqual(@as(?u32, 0x0100_0040), a.next());
    try testing.expectEqual(@as(?u32, 0x0100_0080), a.next());
    try testing.expectEqual(@as(?u32, null), a.next());
    try testing.expectEqual(@as(u32, 0xFFFF), randomMask(8192));
    try testing.expectEqual(@as(u32, 0x1FFFF), randomMask(8193));
    // the first random address: rng_seed masked (systems/m0e/test/ps/ddr.ps, run 2)
    var r: Addrs = .{ .run = .{ .base = 0x0100_0000, .mode = 0x80, .words = 256 } };
    try testing.expectEqual(@as(?u32, 0x0100_0000 + (rng_seed & 0x7FF & ~@as(u32, 7))), r.next());
    try testing.expectEqual(@as(u32, 0x2545F491), rng_seed);
}

test "the layer 4 golden's checksums (systems/m0e/test/golden/ps_bytes.hex)" {
    // run 1: 512 sequential words; run 2: 256 random words with mask 0xFFFFF
    var a: Addrs = .{ .run = .{ .base = 0x0100_0000, .mode = 0, .words = 512 } };
    var s: u32 = 0;
    while (a.next()) |addr| s = checksumStep(s, pattern(addr));
    try testing.expectEqual(@as(u32, 0x7EC7F8E6), s);
    var x: u32 = rng_seed;
    s = 0;
    for (0..256) |_| {
        const addr = 0x0100_0000 + (x & 0xFFFFF & ~@as(u32, 7));
        x = xorshift32(x);
        s = checksumStep(s, pattern(addr));
    }
    try testing.expectEqual(@as(u32, 0x1794C75F), s);
}

test "runs that can't be made" {
    try testing.expect(check(.{ .base = 0x0008_0000, .mode = 0, .words = 1 }) != null);
    try testing.expect(check(.{ .base = 0x1FFF_FFF8, .mode = 0, .words = 2 }) != null);
    try testing.expect(check(.{ .base = 0x0100_0000, .mode = 0x08, .words = 1 }) != null);
    try testing.expect(check(.{ .base = 0x0FF0_0000, .mode = 0, .words = 1 << 18, .busy = true }) != null);
    for (suite) |run| try testing.expectEqual(@as(?[]const u8, null), check(run));
}
