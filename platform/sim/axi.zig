//! The Zynq HP port and its DDR for the Verilator harness (side track C,
//! step 2): an AXI3 read slave with random timing, the Zig twin of
//! Platform.Mem.Sim. Tops with the HP0 read channels (HP0_ARVALID, ...;
//! M0e.Top) get it on those ports.
//!
//! Per clock it decides, from a seeded xorshift64, whether ARREADY is
//! high, how long an accepted burst waits for its first beat, and the
//! gaps between beats; it holds up to `depth` bursts. The memory is a
//! function of the address (`memWord`, the same as m0e-unit's), so a
//! test's checksum is known in advance; or, where `Config.file` covers it
//! (vga-sim --ddr-file ADDR=FILE, side track I: a Game Boy cartridge in
//! DDR), the file's bytes.
//!
//! It also checks the master: an AR held until ARREADY with the same
//! payload, 8-byte aligned, inside the 512 MiB of DDR, no burst across
//! 4 KiB; anything else is counted in `violations` (the first one kept
//! in `first_violation`).
//!
//! Timing in the harness (main.zig): the slave's outputs go into the model
//! before a clock, and `step` then gets the master's outputs as they were
//! before that clock: a transfer happens where VALID and READY were both
//! high at the edge.

const std = @import("std");

/// What the master drives (HP0_ARVALID, HP0_ARADDR, HP0_ARLEN, HP0_RREADY).
pub const MasterOut = extern struct {
    arvalid: u8,
    arlen: u8,
    rready: u8,
    araddr: u32,
};

/// What the slave drives (HP0_ARREADY, HP0_RVALID, HP0_RDATA, HP0_RRESP,
/// HP0_RLAST).
pub const SlaveOut = extern struct {
    arready: u8 = 0,
    rvalid: u8 = 0,
    rresp: u8 = 0,
    rlast: u8 = 0,
    rdata: u64 = 0,
};

/// The DDR's contents: a word made from its (aligned) address.
pub fn memWord(addr: u32) u64 {
    return (@as(u64, addr & ~@as(u32, 7)) *% 0x9E3779B97F4A7C15) ^ 0x0123456789ABCDEF;
}

pub const Config = struct {
    /// most clocks ARREADY stays low after a handshake
    ar_wait: u32 = 5,
    /// most clocks from the AR to the first beat, beyond one
    latency: u32 = 20,
    /// most idle clocks between beats
    gap: u32 = 3,
    /// bursts accepted ahead (at most `max_depth`)
    depth: u32 = 2,
    seed: u64 = 1,
    /// a file in DDR from `file_base` (outside it: `memWord`)
    file: []const u8 = &.{},
    file_base: u32 = 0,
};

const max_depth = 8;
const ddr_size: u64 = 512 << 20;

const Burst = struct { addr: u32, beats: u32, wait: u32 };

pub const Slave = struct {
    cfg: Config,
    rng: u64,
    ar_wait: u32 = 0,
    gap: u32 = 0,
    queue: [max_depth]Burst = undefined,
    n: u32 = 0,
    out: SlaveOut = .{},
    /// the AR the master held last clock without ARREADY
    held: ?MasterOut = null,
    // statistics
    bursts: u64 = 0,
    beats: u64 = 0,
    violations: u64 = 0,
    first_violation: ?[]const u8 = null,

    pub fn init(cfg: Config) Slave {
        var s: Slave = .{ .cfg = cfg, .rng = cfg.seed | 1 };
        s.out = s.outputs();
        return s;
    }

    fn rand(s: *Slave, max: u32) u32 {
        s.rng ^= s.rng << 13;
        s.rng ^= s.rng >> 7;
        s.rng ^= s.rng << 17;
        return if (max == 0) 0 else @intCast(s.rng % (@as(u64, max) + 1));
    }

    fn violation(s: *Slave, what: []const u8) void {
        s.violations += 1;
        if (s.first_violation == null) s.first_violation = what;
    }

    /// The word at an (8-byte aligned) address: the file's bytes where it
    /// covers them (little-endian, as the PS writes DDR), else `memWord`.
    pub fn word(s: *const Slave, addr: u32) u64 {
        const a = addr & ~@as(u32, 7);
        const f = s.cfg.file;
        if (f.len == 0 or a < s.cfg.file_base or a - s.cfg.file_base >= f.len) return memWord(a);
        var w: u64 = 0;
        const off = a - s.cfg.file_base;
        for (0..8) |i| {
            const b: u64 = if (off + i < f.len) f[off + i] else 0;
            w |= b << @intCast(8 * i);
        }
        return w;
    }

    /// The outputs from the state.
    fn outputs(s: *const Slave) SlaveOut {
        var o: SlaveOut = .{ .arready = @intFromBool(s.ar_wait == 0 and s.n < @min(s.cfg.depth, max_depth)) };
        if (s.n > 0 and s.queue[0].wait == 0 and s.gap == 0) {
            const b = s.queue[0];
            o.rvalid = 1;
            o.rdata = s.word(b.addr);
            o.rlast = @intFromBool(b.beats == 1);
        }
        return o;
    }

    /// One clock edge: `m` is what the master drove before it.
    pub fn step(s: *Slave, m: MasterOut) void {
        const o = s.out;
        // the burst at the head: a beat goes out, the others count down
        const sent = o.rvalid != 0 and m.rready != 0;
        if (sent) {
            s.beats += 1;
            s.queue[0].addr +%= 8;
            s.queue[0].beats -= 1;
            if (s.queue[0].beats == 0) {
                std.mem.copyForwards(Burst, s.queue[0 .. s.n - 1], s.queue[1..s.n]);
                s.n -= 1;
            }
            s.gap = s.rand(s.cfg.gap);
        } else if (s.gap > 0) s.gap -= 1;
        for (s.queue[0..s.n], 0..) |*b, i| {
            if (b.wait > 0 and !(sent and i == 0)) b.wait -= 1;
        }
        // the master's AR
        if (s.held) |h| {
            if (m.arvalid == 0 or m.araddr != h.araddr or m.arlen != h.arlen)
                s.violation("AR dropped or changed before ARREADY");
        }
        s.held = null;
        if (m.arvalid != 0) {
            const len: u32 = @as(u32, m.arlen & 15) + 1;
            if (m.araddr & 7 != 0) s.violation("ARADDR not 8-byte aligned");
            if (@as(u64, m.araddr) + 8 * len > ddr_size) s.violation("burst outside DDR");
            if ((m.araddr & 0xFFF) + 8 * len > 0x1000) s.violation("burst crosses 4 KiB");
            if (o.arready != 0) {
                s.queue[s.n] = .{ .addr = m.araddr, .beats = len, .wait = s.rand(s.cfg.latency) };
                s.n += 1;
                s.bursts += 1;
                s.ar_wait = s.rand(s.cfg.ar_wait);
            } else s.held = m;
        }
        if (o.arready == 0 and s.ar_wait > 0) s.ar_wait -= 1;
        s.out = s.outputs();
    }
};

// --- Writing (side track O, O.4: screenshots, Platform.Video.Capture) -------

/// What a writer drives (HP0_AWVALID, AWADDR, AWLEN, WVALID, WDATA, WSTRB,
/// WLAST, BREADY).
pub const WrMasterOut = extern struct {
    awvalid: u8 = 0,
    awlen: u8 = 0,
    wvalid: u8 = 0,
    wlast: u8 = 0,
    wstrb: u8 = 0,
    bready: u8 = 0,
    awaddr: u32 = 0,
    wdata: u64 = 0,
};

/// What the slave drives (HP0_AWREADY, WREADY, BVALID, BRESP).
pub const WrSlaveOut = extern struct {
    awready: u8 = 0,
    wready: u8 = 0,
    bvalid: u8 = 0,
    bresp: u8 = 0,
};

/// An AXI3 write slave with the read slave's random timing (`ar_wait` for
/// AWREADY, `gap` for WREADY, `latency` before a burst's response, `depth`
/// bursts ahead), the Zig twin of Platform.Mem.Sim.axiWrSlave; the bytes
/// written kept (`mem`, strobes honoured). It checks the master: AW and W
/// held with the same payload until their READY, AWADDR 8-byte aligned and
/// inside DDR, no burst across 4 KiB, WLAST on a burst's last beat only.
pub const WrSlave = struct {
    cfg: Config,
    rng: u64,
    aw_wait: u32 = 0,
    w_gap: u32 = 0,
    /// accepted bursts: next beat's address, beats left
    queue: [max_depth]struct { addr: u32, left: u32 } = undefined,
    n: u32 = 0,
    /// clocks before each pending response
    resp: [64]u32 = undefined,
    nresp: u32 = 0,
    mem: std.AutoHashMap(u32, u8),
    out: WrSlaveOut = .{},
    held_aw: ?WrMasterOut = null,
    held_w: ?WrMasterOut = null,
    bursts: u64 = 0,
    beats: u64 = 0,
    violations: u64 = 0,
    first_violation: ?[]const u8 = null,

    pub fn init(gpa: std.mem.Allocator, cfg: Config) WrSlave {
        var s: WrSlave = .{ .cfg = cfg, .rng = (cfg.seed ^ 0x5DEECE66D) | 1, .mem = .init(gpa) };
        s.out = s.outputs();
        return s;
    }

    pub fn deinit(s: *WrSlave) void {
        s.mem.deinit();
    }

    fn rand(s: *WrSlave, max: u32) u32 {
        s.rng ^= s.rng << 13;
        s.rng ^= s.rng >> 7;
        s.rng ^= s.rng << 17;
        return if (max == 0) 0 else @intCast(s.rng % (@as(u64, max) + 1));
    }

    fn violation(s: *WrSlave, what: []const u8) void {
        s.violations += 1;
        if (s.first_violation == null) s.first_violation = what;
    }

    /// A byte written, or null.
    pub fn byte(s: *const WrSlave, addr: u32) ?u8 {
        return s.mem.get(addr);
    }

    fn outputs(s: *const WrSlave) WrSlaveOut {
        return .{
            .awready = @intFromBool(s.aw_wait == 0 and s.n < @min(s.cfg.depth, max_depth)),
            .wready = @intFromBool(s.w_gap == 0 and s.n > 0),
            .bvalid = @intFromBool(s.nresp > 0 and s.resp[0] == 0),
        };
    }

    /// One clock edge: `m` is what the master drove before it.
    pub fn step(s: *WrSlave, m: WrMasterOut) !void {
        const o = s.out;
        // B
        if (o.bvalid != 0 and m.bready != 0) {
            std.mem.copyForwards(u32, s.resp[0 .. s.nresp - 1], s.resp[1..s.nresp]);
            s.nresp -= 1;
        }
        for (s.resp[0..s.nresp]) |*w| {
            if (w.* > 0) w.* -= 1;
        }
        // W
        if (s.held_w) |h| {
            if (m.wvalid == 0 or m.wdata != h.wdata or m.wstrb != h.wstrb or m.wlast != h.wlast)
                s.violation("W dropped or changed before WREADY");
        }
        s.held_w = null;
        if (m.wvalid != 0) {
            if (o.wready != 0) {
                const b = &s.queue[0];
                for (0..8) |k| if (m.wstrb & (@as(u8, 1) << @intCast(k)) != 0)
                    try s.mem.put(b.addr + @as(u32, @intCast(k)), @truncate(m.wdata >> @intCast(8 * k)));
                if ((m.wlast != 0) != (b.left == 1)) s.violation("WLAST not on the burst's last beat");
                s.beats += 1;
                b.addr +%= 8;
                b.left -= 1;
                if (b.left == 0) {
                    std.mem.copyForwards(@TypeOf(s.queue[0]), s.queue[0 .. s.n - 1], s.queue[1..s.n]);
                    s.n -= 1;
                    if (s.nresp == s.resp.len) return error.TooManyResponses;
                    s.resp[s.nresp] = s.rand(s.cfg.latency);
                    s.nresp += 1;
                }
                s.w_gap = s.rand(s.cfg.gap);
            } else {
                s.held_w = m;
                if (s.w_gap > 0) s.w_gap -= 1;
            }
        } else if (s.w_gap > 0) s.w_gap -= 1;
        // AW
        if (s.held_aw) |h| {
            if (m.awvalid == 0 or m.awaddr != h.awaddr or m.awlen != h.awlen)
                s.violation("AW dropped or changed before AWREADY");
        }
        s.held_aw = null;
        if (m.awvalid != 0) {
            const len: u32 = @as(u32, m.awlen & 15) + 1;
            if (m.awaddr & 7 != 0) s.violation("AWADDR not 8-byte aligned");
            if (@as(u64, m.awaddr) + 8 * len > ddr_size) s.violation("write burst outside DDR");
            if ((m.awaddr & 0xFFF) + 8 * len > 0x1000) s.violation("write burst crosses 4 KiB");
            if (o.awready != 0) {
                s.queue[s.n] = .{ .addr = m.awaddr, .left = len };
                s.n += 1;
                s.bursts += 1;
                s.aw_wait = s.rand(s.cfg.ar_wait);
            } else s.held_aw = m;
        }
        if (o.awready == 0 and s.aw_wait > 0) s.aw_wait -= 1;
        s.out = s.outputs();
    }
};

test "writes: one-beat and longer bursts land in memory, strobes honoured, under random timing" {
    var s = WrSlave.init(std.testing.allocator, .{ .seed = 7, .latency = 9, .gap = 2, .ar_wait = 3 });
    defer s.deinit();
    // a master: AW and W of each burst offered together, held until taken
    const bursts = [_]struct { addr: u32, len: u8 }{ .{ .addr = 0x1000_0000, .len = 0 }, .{ .addr = 0x1000_0040, .len = 3 }, .{ .addr = 0x1000_0008, .len = 0 } };
    var bi: usize = 0;
    var beat: u8 = 0;
    var aw_done = false;
    var clock: u32 = 0;
    var m: WrMasterOut = .{ .bready = 1 };
    while (true) : (clock += 1) {
        if (clock > 10_000) return error.Timeout;
        // this edge's handshakes, from what was driven before it
        const o = s.out;
        if (m.awvalid != 0 and o.awready != 0) aw_done = true;
        if (m.wvalid != 0 and o.wready != 0) {
            if (beat == bursts[bi].len) {
                bi += 1;
                beat = 0;
                aw_done = false;
            } else beat += 1;
        }
        try s.step(m);
        if (bi == bursts.len) break;
        // what the master drives next
        const b = bursts[bi];
        m = .{ .bready = 1, .awvalid = @intFromBool(!aw_done), .awaddr = b.addr, .awlen = b.len, .wvalid = 1, .wdata = 0x0102030405060708 +% @as(u64, beat), .wstrb = if (bi == 2) 0xF0 else 0xFF, .wlast = @intFromBool(beat == b.len) };
    }
    try std.testing.expectEqual(@as(u64, 0), s.violations);
    try std.testing.expectEqual(@as(?u8, 0x08), s.byte(0x1000_0000));
    try std.testing.expectEqual(@as(?u8, 0x01), s.byte(0x1000_0007));
    try std.testing.expectEqual(@as(?u8, 0x0B), s.byte(0x1000_0058)); // the 4th beat: ...08 + 3
    // the third: only the upper half (strobes 0xF0)
    try std.testing.expectEqual(@as(?u8, null), s.byte(0x1000_0008));
    try std.testing.expectEqual(@as(?u8, 0x04), s.byte(0x1000_000C));
}

test "writes: an AW across 4 KiB, a W dropped before WREADY, a misplaced WLAST are violations" {
    var s = WrSlave.init(std.testing.allocator, .{ .ar_wait = 0 });
    defer s.deinit();
    try s.step(.{ .awvalid = 1, .awlen = 3, .awaddr = 0x1000_0FF0, .bready = 1 });
    try std.testing.expectEqual(@as(u64, 1), s.violations);
    var t = WrSlave.init(std.testing.allocator, .{ .depth = 0 }); // never ready
    defer t.deinit();
    try t.step(.{ .wvalid = 1, .wdata = 5, .wstrb = 0xFF, .wlast = 1, .bready = 1 });
    try t.step(.{ .wvalid = 0, .bready = 1 });
    try std.testing.expectEqual(@as(u64, 1), t.violations);
    var u = WrSlave.init(std.testing.allocator, .{ .ar_wait = 0, .gap = 0 });
    defer u.deinit();
    try u.step(.{ .awvalid = 1, .awlen = 1, .awaddr = 0x1000_0000, .bready = 1 });
    try u.step(.{ .wvalid = 1, .wdata = 1, .wstrb = 0xFF, .wlast = 1, .bready = 1 }); // WLAST on the first of two
    try std.testing.expectEqual(@as(u64, 1), u.violations);
}

/// A master in a test: one burst at a time, RREADY always high.
fn testRun(cfg: Config, addrs: []const u32, words: []u64) !void {
    var s = Slave.init(cfg);
    var m: MasterOut = .{ .arvalid = 0, .arlen = 3, .rready = 0, .araddr = 0 };
    var i: usize = 0;
    var got: usize = 0;
    var clock: u32 = 0;
    var waiting = false;
    while (got < 4 * addrs.len) : (clock += 1) {
        if (clock > 100_000) return error.Timeout;
        const before = m;
        const o = s.out;
        // the master's next outputs, from its state and what it sees now
        if (!waiting and i < addrs.len) {
            m = .{ .arvalid = 1, .arlen = 3, .rready = 1, .araddr = addrs[i] };
        }
        if (before.arvalid != 0 and o.arready != 0) {
            waiting = true;
            i += 1;
            m.arvalid = 0;
        }
        if (o.rvalid != 0 and before.rready != 0) {
            words[got] = o.rdata;
            got += 1;
            if (o.rlast != 0) waiting = false;
        }
        s.step(before);
    }
    try std.testing.expectEqual(@as(u64, 0), s.violations);
    try std.testing.expectEqual(@as(u64, addrs.len), s.bursts);
}

test "bursts come back with the memory's words, in order, under random timing" {
    const addrs = [_]u32{ 0x0100_0000, 0x0100_0020, 0x0200_0FE0, 0x0000_1000 };
    var words: [16]u64 = undefined;
    for ([_]u64{ 1, 2, 3, 99 }) |seed| {
        try testRun(.{ .seed = seed, .latency = 30 }, &addrs, &words);
        for (addrs, 0..) |a, k| {
            for (0..4) |b| try std.testing.expectEqual(memWord(a + 8 * @as(u32, @intCast(b))), words[4 * k + b]);
        }
    }
}

test "a burst across 4 KiB or an unaligned address is a violation" {
    var s = Slave.init(.{ .ar_wait = 0 });
    s.step(.{ .arvalid = 1, .arlen = 3, .rready = 1, .araddr = 0x0100_0FF0 });
    try std.testing.expectEqual(@as(u64, 1), s.violations);
    s = Slave.init(.{ .ar_wait = 0 });
    s.step(.{ .arvalid = 1, .arlen = 0, .rready = 1, .araddr = 0x0100_0004 });
    try std.testing.expectEqual(@as(u64, 1), s.violations);
}

test "an AR dropped before ARREADY is a violation" {
    var s = Slave.init(.{ .depth = 0 }); // never ready
    s.step(.{ .arvalid = 1, .arlen = 3, .rready = 1, .araddr = 0x0100_0000 });
    s.step(.{ .arvalid = 0, .arlen = 3, .rready = 1, .araddr = 0 });
    try std.testing.expectEqual(@as(u64, 1), s.violations);
}

test "a file in DDR: its bytes little-endian where it is, memWord elsewhere" {
    const f = [_]u8{ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };
    const s = Slave.init(.{ .file = &f, .file_base = 0x0200_0000 });
    try std.testing.expectEqual(@as(u64, 0x0807060504030201), s.word(0x0200_0000));
    try std.testing.expectEqual(@as(u64, 0x0807060504030201), s.word(0x0200_0005));
    try std.testing.expectEqual(@as(u64, 0x0A09), s.word(0x0200_0008));
    try std.testing.expectEqual(memWord(0x0200_0010), s.word(0x0200_0010));
    try std.testing.expectEqual(memWord(0x01FF_FFF8), s.word(0x01FF_FFF8));
}

test "memWord matches m0e-unit's" {
    // systems/m0e/test/Main.hs: resize a * 0x9E3779B97F4A7C15 `xor` 0x0123456789ABCDEF
    try std.testing.expectEqual(@as(u64, 0x0123456789ABCDEF), memWord(0));
    const k: u64 = 0x9E3779B97F4A7C15;
    try std.testing.expectEqual((k *% 8) ^ 0x0123456789ABCDEF, memWord(8));
}
