//! The NES's restore routine (side track N, PLAN.md N.7): loading a state
//! through the machine's own 2A03, as the 800XL's (a800_restore.zig). The
//! PS holds the core (its reset: the whole console at power-on, the PPU at
//! line 0, dot 0, the CPU starting its reset sequence), writes RAM,
//! nametable RAM and the cartridge's RAMs through its windows, puts the
//! overlay this file builds into the core's ROM overlay and lets the core
//! go. Every CPU read of $0000-$1FFF and $6000-$FFFF then answers the
//! overlay's byte (the address modulo `overlay_size`), the vectors and the
//! DMA's reads included, until the instruction that read the address
//! `build` returns (the routine's RTI) has ended; the next fetch is the
//! game's. RTI takes P and PC from the overlay's stack page; OAM DMA reads
//! the overlay's page 2, a copy of the saved OAM.
//!
//! The routine is straight-line code, generated per state, its cycles
//! known exactly: the PPU runs three dots a CPU cycle from power-on, a
//! frame a dot short where rendering was on over an odd frame's end, so
//! the position of every cycle follows (`posOf`). In order:
//!   1. the preamble: the DMC's rate written before each of its first few
//!      timer reloads (`dmcPlan`: the reload takes the rate then, so the
//!      timer's phase and the output unit's bit count come out as saved),
//!      then its saved rate; $4017 = $40 if the frame counter's IRQ flag
//!      was clear (it stays clear; else the power-on sequence sets it);
//!   2. for the other cycle parity, one more odd-frame skip: the window of
//!      step 4 over the end of frame 1 (`emitWindow`);
//!   3. the controllers' strobe; the board's registers (MMC1's serial
//!      writes, its shift register's bits last; the bank registers;
//!      MMC3's banks and IRQ counter: latch, reload and an A12 rise
//!      through $2006; MMC5's registers but its IRQ's); palette RAM; the
//!      $2007 read buffer (a nametable byte written with it, read, written
//!      back); OAM by DMA; the APU's registers and enables; t as v must
//!      become;
//!   4. the window: rendering on from the pre-render line of the frame
//!      before the saved one (dot 230) to line 240 of the saved one (not a
//!      rendering line: no OAM corruption seeded), which puts t into v
//!      (vertical bits at dots 280-304: v's bit 14, fine Y's top, has no
//!      other way in; then 240 lines of Y increments: t's vertical bits are
//!      chosen to end there, `vertBack`; horizontal bits at each dot 257,
//!      then coarse X on twice, dots 328, 336); the frame before a dot
//!      short if odd. The saved frame's picture is the window's;
//!   5. t, fine x and w as saved (and v through $2006, bit 14 lost, if no
//!      t reaches it: coarse Y 30 or 31); MMC5's IRQ (pending acknowledged
//!      first: the window's fetches may have raised it);
//!   6. the frame counter's $4017 write, before the window or after step 5,
//!      on the cycle its sequencer then counts from to the saved value;
//!   7. the tail, after line 241's flag: $2002 read if the flag was clear;
//!      PPUCTRL (an NMI if it enables one with the flag set: taken, its
//!      three pushes put back); w; PPUMASK, OAMADDR; the I/O latch (a
//!      $2002 write); the DMC's buffer if it was full (a 1-byte sample from
//!      the overlay, after the output unit last took a byte: `dmcFetch`);
//!      $4015 read (the frame counter's flag clear); S (for RTI's pulls),
//!      A, X, Y; RTI, its last cycle the saved one (`t.line`, `t.dot`,
//!      `t.odd`, `t.parity`).
//! What it can't put back (approximate by design, the user 2026-10-14):
//! the APU's length counters (a load is a table value), envelope, sweep,
//! linear counter and timer phases, the noise shift register, the DMC's
//! output shift register and silence flag (its level then) and its IRQ
//! flag; the ages of the PPU's I/O latch bits (they fade after a while:
//! fresh), its sprite 0 hit and
//! overflow flags, PPUCTRL's nametable bits if t's differ; MMC3's A12 line
//! (low after the window); MMC5's IRQ pending, in-frame flag and line
//! counter; the 6502's B flag; the sequencer's phase of a frame counter
//! never written since power-on (one cycle off); the read buffer with
//! every nametable in MMC5's ExRAM or fill.
//!
//! The state table's offsets are Nes.StateTable's; the test below checks
//! them against systems/nes/test/golden/state-table.txt.

const std = @import("std");

pub const table_size = 0x100;
pub const overlay_size = 0x800;
pub const ram_size = 0x800;
pub const ciram_size = 0x800;
pub const oam_size = 0x100;

const line_dots: u32 = 341;
const frame_lines: u32 = 262;
pub const frame_dots: u32 = line_dots * frame_lines;

pub const t = struct {
    pub const a = 0x00;
    pub const x = 0x01;
    pub const y = 0x02;
    pub const s = 0x03;
    pub const p = 0x04;
    pub const pc = 0x05;
    pub const line = 0x08;
    pub const dot = 0x0A;
    pub const odd = 0x0C;
    pub const parity = 0x0D;
    pub const strobe = 0x0E;
    pub const ctrl = 0x10;
    pub const mask = 0x11;
    pub const status = 0x12;
    pub const oamaddr = 0x13;
    pub const v = 0x14;
    pub const t = 0x16;
    pub const finex = 0x18;
    pub const w = 0x19;
    pub const buf = 0x1A;
    pub const latch = 0x1B;
    pub const pal = 0x20;
    pub const apu = 0x40;
    pub const enabled = 0x54;
    pub const frame = 0x55;
    pub const seq = 0x56;
    pub const frameirq = 0x58;
    pub const dmcirq = 0x59;
    pub const lengths = 0x5A;
    pub const mapper = 0x60;
    pub const mirror = 0x61;
    pub const board = 0x62;
    pub const mmc1 = 0x64;
    pub const bank = 0x6A;
    pub const mmc3 = 0x70;
    pub const mmc5 = 0x80;
    pub const mmc5line = 0xA0;
    pub const dmccount = 0xA8;
    pub const dmcbits = 0xAA;
    pub const dmcfull = 0xAB;
    pub const dmcbuf = 0xAC;
    pub const dmcshift = 0xAD;
    pub const dmcsilence = 0xAE;
};

/// Where the code starts (overlay offset 0x300: 0x100-0x1FF is the stack
/// page, RTI's P and PC; 0x200-0x2FF OAM's copy, the DMA's page 2).
const code_addr: u16 = 0xFB00;
const code_end: u16 = 0xFFFA;
const stack_ovl: u16 = 0x100;
const oam_ovl: u16 = 0x200;
const oam_page: u8 = 0x02;

/// The first line a state is taken at, and the last (Nes.Spec.SaveState's
/// rest: room after line 241's flag for the tail).
const first_line: u32 = 243;
const last_line: u32 = 260;

/// The interrupt sequence's cycles.
const nmi_cycles: u32 = 7;

/// The frame counter: a sequence's cycles (mode 0, mode 1).
const seq_period = [2]u32{ 29830, 37282 };

pub const Error = error{
    /// the routine doesn't fit the overlay
    TooLong,
    /// the saved position isn't one a state is taken at
    BadPosition,
    /// no frame within reach puts the routine's writes where they must be
    NoSchedule,
};

pub const Built = struct {
    /// the address whose read ends the overlay (the RTI's opcode)
    end: u16,
    /// CPU cycles from the release to RTI's last cycle
    cycles: u32,
};

/// The PPU's place: frame, line, dot (the dot just run).
pub const Pos = struct { frame: u32, line: u32, dot: u32 };

/// Dots run by the end of CPU cycle n (n from 1): three a cycle, the PPU a
/// master clock tick behind (Nes.Spec.Machine's alignment).
pub fn dotsAfter(n: u32) u32 {
    return 3 * n - 1;
}

/// ... and by a write's access in cycle n (tick 7 of 12).
pub fn dotsAtWrite(n: u32) u32 {
    return 3 * n - 2;
}

/// Frames a dot short: the ones whose end had rendering on (odd frames).
const Skips = struct {
    f: [2]u32 = .{ 0, 0 },
    n: usize = 0,
    fn add(self: *Skips, g: u32) void {
        self.f[self.n] = g;
        self.n += 1;
    }
    /// Dots run before frame g starts.
    fn start(self: *const Skips, g: u32) u32 {
        var r = g * frame_dots;
        for (self.f[0..self.n]) |j| {
            if (j < g) r -= 1;
        }
        return r;
    }
};

/// The position after `e` dots.
pub fn posOf(e: u32, skips: *const Skips) Pos {
    var g = e / frame_dots;
    while (skips.start(g + 1) <= e) g += 1;
    while (g > 0 and skips.start(g) > e) g -= 1;
    const r = e - skips.start(g);
    return .{ .frame = g, .line = r / line_dots, .dot = r % line_dots };
}

const Emit = struct {
    ovl: *[overlay_size]u8,
    pc: u16 = code_addr,
    /// CPU cycles so far (after the reset sequence's 7)
    cyc: u32 = 0,
    err: bool = false,

    /// The cycle the last instruction ended on (its write's, for a store).
    fn now(self: *const Emit) u32 {
        return 7 + self.cyc;
    }
    fn byte(self: *Emit, v: u8) void {
        if (self.pc >= code_end) {
            self.err = true;
            return;
        }
        self.ovl[self.pc % overlay_size] = v;
        self.pc += 1;
    }
    fn op(self: *Emit, bytes: []const u8, cyc: u32) void {
        for (bytes) |v| self.byte(v);
        self.cyc += cyc;
    }
    fn lda(self: *Emit, v: u8) void {
        self.op(&.{ 0xA9, v }, 2);
    }
    fn sta(self: *Emit, a: u16) void {
        self.op(&.{ 0x8D, @truncate(a), @truncate(a >> 8) }, 4);
    }
    fn bit(self: *Emit, a: u16) void {
        self.op(&.{ 0x2C, @truncate(a), @truncate(a >> 8) }, 4);
    }
    fn w(self: *Emit, a: u16, v: u8) void {
        self.lda(v);
        self.sta(a);
    }
    /// $2006 twice: v (and t) = a, w = 0.
    fn ppuAddr(self: *Emit, a: u16) void {
        self.w(0x2006, @truncate(a >> 8));
        self.w(0x2006, @truncate(a));
    }
    /// OAM DMA from page `pg`: the CPU halted for 513 cycles, 514 if the
    /// write was on an even (put) cycle (the alignment cycle).
    fn oamDma(self: *Emit, pg: u8) void {
        self.w(0x4014, pg);
        self.cyc += if (self.now() % 2 == 0) 514 else 513;
    }
    /// Exactly `n` CPU cycles (X, Y and the flags change), any n but 1:
    /// LDY #h; LDX #0; DEX; BNE; DEY; BNE (1286h + 1) for long ones,
    /// LDX #c; DEX; BNE (5c + 1) for the bulk, NOP (2) and BIT $00 (3) for
    /// the rest; a loop never straddles a page (its branch would take a
    /// cycle more).
    fn delay(self: *Emit, n: u32) void {
        std.debug.assert(n != 1);
        var left = n;
        while (left >= 1286 + 1 + 11) {
            if ((self.pc >> 8) != ((self.pc + 10) >> 8)) {
                self.op(&.{0xEA}, 2);
                left -= 2;
                continue;
            }
            const h: u32 = @min((left - 12) / 1286, 256);
            self.op(&.{ 0xA0, @truncate(h), 0xA2, 0x00, 0xCA, 0xD0, 0xFD, 0x88, 0xD0, 0xF8 }, 1286 * h + 1);
            left -= 1286 * h + 1;
        }
        while (left >= 11) {
            const dex = self.pc + 2;
            if ((dex >> 8) != ((dex + 3) >> 8)) {
                self.op(&.{0xEA}, 2);
                left -= 2;
                continue;
            }
            var c: u32 = @min((left - 1) / 5, 256);
            if (left - (5 * c + 1) == 1) c -= 1;
            self.op(&.{ 0xA2, @truncate(c), 0xCA, 0xD0, 0xFD }, 5 * c + 1);
            left -= 5 * c + 1;
        }
        std.debug.assert(left != 1);
        if (left % 2 == 1) {
            self.op(&.{ 0x24, 0x00 }, 3);
            left -= 3;
        }
        while (left > 0) : (left -= 2) self.op(&.{0xEA}, 2);
    }
    /// A write (LDA #v; STA a) on a cycle from `lo` to `hi`, the first after
    /// a delay that isn't 1; false if none is left.
    fn writeCycle(self: *Emit, lo: u32, hi: u32, a: u16, v: u8) bool {
        var n: u32 = @max(lo, self.now() + 6);
        while (n <= hi) : (n += 1) {
            const d = n - 6 - self.now();
            if (d == 1) continue;
            self.delay(d);
            self.w(a, v);
            return true;
        }
        return false;
    }
    /// A write (LDA #v; STA a) whose access falls on a cycle n with dots
    /// lo to hi run by it (`dotsAtWrite`), the first such after a delay
    /// that isn't 1; false if none is left.
    fn writeIn(self: *Emit, lo: u32, hi: u32, a: u16, v: u8) bool {
        var n: u32 = @max((lo + 2 + 2) / 3, self.now() + 6);
        while (dotsAtWrite(n) <= hi) : (n += 1) {
            if (dotsAtWrite(n) < lo) continue;
            const d = n - 6 - self.now();
            if (d == 1) continue;
            self.delay(d);
            self.w(a, v);
            return true;
        }
        return false;
    }
};

/// NTSC DMC rates: CPU cycles per output bit (its timer counts half of
/// them, on odd cycles).
const dmc_rates = [16]u16{ 428, 380, 340, 320, 286, 254, 226, 214, 190, 160, 142, 128, 106, 84, 72, 54 };

/// The DMC timer's first reload after power-on (its count starts at 213).
const dmc_first: u32 = 427;

/// Reloads at other rates a plan has at most: enough for every count and
/// bit (a period split in two shorter ones of the same sum, as 428 into
/// 214 and 214, moves the bits without the phase).
const max_detours = 12;

/// The DMC's rates at its first `d` reloads, before the saved one takes
/// over: the timer's reloads (each a bit of the output unit) then fall so
/// that at cycle `n_end` its count and bits left are the saved ones.
const DmcPlan = struct {
    d: u8 = 0,
    p: [max_detours]u4 = [_]u4{0} ** max_detours,
    /// the cycle of reload i (from 0), the saved rate `rs` after the plan's
    fn at(self: *const DmcPlan, i: u32, rs: u32) u32 {
        var e = dmc_first;
        var k: u32 = 0;
        while (k < @min(i, self.d)) : (k += 1) e += dmc_rates[self.p[k]];
        return if (i > self.d) e + (i - self.d) * rs else e;
    }
};

/// The timer's count and the bits left at cycle `n` under `plan` (rate
/// `rs` after it), and the cycle of the last reload that emptied the bits
/// (the output unit took the buffer then).
fn dmcAt(plan: *const DmcPlan, rs: u32, n: u32) ?struct { count: u32, bits: u32, took: u32 } {
    const e = plan.at(plan.d, rs);
    if (n < e) return null;
    const after = (n - e) / rs;
    const last = e + after * rs;
    const reloads = plan.d + 1 + after; // reloads up to n, from power-on
    const bits = if (reloads % 8 == 0) 8 else 8 - reloads % 8;
    // the last reload that emptied the bits: (8 - bits) % 8 reloads back
    const took_i = reloads - (8 - bits) % 8;
    return .{ .count = rs / 2 - 1 - (n - last) / 2, .bits = bits, .took = plan.at(took_i - 1, rs) };
}

/// A plan (at most `max_detours` reloads at other rates: `a` at one rate,
/// `b` at a second, `c` at a third) that leaves the saved count and bits at
/// `n_end`; null if none. What it leaves depends only on its length and its
/// rates' sum, so three rates reach what any mix would here (the coverage
/// test). No table: the PS app's stacks are small.
fn dmcPlan(rs: u32, n_end: u32, count: u32, bits: u32) ?DmcPlan {
    var d: u8 = 0;
    while (d <= max_detours) : (d += 1) {
        for (0..16) |ra| {
            for (ra..16) |rb| {
                for (rb..16) |rc| {
                    var a: u8 = 0;
                    while (a <= d) : (a += 1) {
                        var b: u8 = 0;
                        while (a + b <= d) : (b += 1) {
                            var plan = DmcPlan{ .d = d };
                            for (0..d) |k| plan.p[k] = @intCast(if (k < a) ra else if (k < a + b) rb else rc);
                            if (dmcAt(&plan, rs, n_end)) |r| {
                                if (r.count == count and r.bits == bits) return plan;
                            }
                        }
                    }
                }
            }
        }
    }
    return null;
}

fn get16(tb: *const [table_size]u8, o: usize) u16 {
    return @as(u16, tb[o]) | (@as(u16, tb[o + 1]) << 8);
}

/// Length counter loads by index (bits 7-3 of a channel's fourth register).
const length_table = [32]u8{
    10, 254, 20, 2,  40, 4,  80, 6,  160, 8,  60, 10, 14, 12, 26, 14,
    12, 16,  24, 18, 48, 20, 96, 22, 192, 24, 72, 26, 16, 28, 32, 30,
};

/// The index whose load is nearest `len` (the length counters are
/// approximate: a load is a table value).
fn lengthIndex(len: u8) u8 {
    var best: u8 = 0;
    var bd: u32 = 0xFFFF;
    for (length_table, 0..) |v, i| {
        const d: u32 = if (v > len) v - len else len - v;
        if (d < bd) {
            bd = d;
            best = @intCast(i);
        }
    }
    return best;
}

/// The nametable page (0 or 1 of the console's RAM) nametable `q` (0-3)
/// uses, from the board's registers as the table has them; null where it
/// isn't the console's (MMC5's ExRAM or fill).
fn ntPage(tb: *const [table_size]u8, q: u2) ?u1 {
    const h: u1 = @truncate(q >> 1);
    const vv: u1 = @truncate(q);
    return switch (tb[t.mapper]) {
        1 => switch (tb[t.mmc1 + 2] & 3) {
            0 => 0,
            1 => 1,
            2 => vv,
            else => h,
        },
        7 => if (tb[t.bank] & 0x10 != 0) 1 else 0,
        4 => if (tb[t.mmc3 + 9] & 1 != 0) h else vv,
        5 => switch ((tb[t.mmc5 + 5] >> (@as(u3, q) * 2)) & 3) {
            0 => 0,
            1 => 1,
            else => null,
        },
        else => if (tb[t.mirror] == 1) vv else h,
    };
}

/// The MMC3's IRQ counter as the routine's writes and A12 rises leave it.
const Mmc3 = struct {
    count: u8 = 0,
    reload: bool = false,
    latch: u8 = 0,
    en: bool = false,
    flag: bool = false,
    fn rise(m: *Mmc3) void {
        m.count = if (m.count == 0 or m.reload) m.latch else m.count - 1;
        m.reload = false;
        if (m.count == 0 and m.en) m.flag = true;
    }
};

/// What the saved MMC3 counter is: count, reload, latch, enable, pending.
fn mmc3Saved(tb: *const [table_size]u8) Mmc3 {
    const b = t.mmc3;
    return .{ .latch = tb[b + 10], .count = tb[b + 11], .reload = tb[b + 12] != 0, .en = tb[b + 13] != 0, .flag = tb[b + 14] != 0 };
}

/// An A12 rise through v: low ($0000: the palette's $3Fxx left it high),
/// high ($1000), low again; each long enough for the M2 filter.
fn a12Pulse(e: *Emit) void {
    e.ppuAddr(0x0000);
    e.ppuAddr(0x1000);
    e.ppuAddr(0x0000);
}

/// Whether v comes through the window (step 4): with its bit 14 set (no
/// write sets it) and a t that leads there; else $2006 sets it (step 5).
fn windowed(tb: *const [table_size]u8) bool {
    const vv = get16(tb, t.v);
    return vv & 0x4000 != 0 and vertBack(vv) != null;
}

/// Whether step 5's $2006 is an A12 rise (v's bit 12) the MMC3 counts: the
/// counter's reload waits for it.
fn riseInPost(tb: *const [table_size]u8) bool {
    return tb[t.mapper] == 4 and !windowed(tb) and get16(tb, t.v) & 0x1000 != 0;
}

/// MMC3's IRQ counter, before step 5 (after the palette, whose $3Fxx
/// addresses are A12 rises too): the pending flag first if it was set
/// (enabled, a clock to 0), then the count, from the latch with a reload
/// pending, by a rise now or by step 5's.
fn mmc3Pre(e: *Emit, tb: *const [table_size]u8, sim: *Mmc3) void {
    const want = mmc3Saved(tb);
    if (want.flag) {
        e.sta(0xE001); // (any value)
        sim.en = true;
        e.w(0xC000, 0);
        sim.latch = 0;
        e.sta(0xC001);
        sim.count = 0;
        sim.reload = true;
        a12Pulse(e);
        sim.rise();
    }
    e.w(0xC000, want.count);
    sim.latch = want.count;
    e.sta(0xC001);
    sim.count = 0;
    sim.reload = true;
    if (!riseInPost(tb)) {
        a12Pulse(e);
        sim.rise();
    }
}

/// ... and after step 5's $2006: the latch, a reload pending, the enable.
fn mmc3Post(e: *Emit, tb: *const [table_size]u8, sim: *Mmc3) void {
    const want = mmc3Saved(tb);
    if (riseInPost(tb)) sim.rise();
    e.w(0xC000, want.latch);
    sim.latch = want.latch;
    if (want.reload) {
        e.sta(0xC001);
        sim.count = 0;
        sim.reload = true;
    }
    if (want.en and !sim.en) {
        e.sta(0xE001);
        sim.en = true;
    }
}

/// Step 3's board registers (MMC3's IRQ counter and MMC5's IRQ apart).
fn boardRegs(e: *Emit, tb: *const [table_size]u8) void {
    switch (tb[t.mapper]) {
        1 => {
            // control, CHR 0, CHR 1, PRG: five serial writes each, bit 0
            // first; then the shift register's bits (the oldest first)
            const regs = [4]u16{ 0x8000, 0xA000, 0xC000, 0xE000 };
            for (regs, 0..) |a, i| {
                const v = tb[t.mmc1 + 2 + i];
                for (0..5) |b| e.w(a, (v >> @intCast(b)) & 1);
            }
            const sh = tb[t.mmc1];
            const n: u8 = @min(tb[t.mmc1 + 1], 4);
            var i: u8 = 0;
            while (i < n) : (i += 1) e.w(0x8000, (sh >> @intCast(5 - n + i)) & 1);
        },
        2, 3, 7 => e.w(0x8000, tb[t.bank]),
        4 => {
            const b = t.mmc3;
            for (0..8) |i| {
                e.w(0x8000, @intCast(i));
                e.w(0x8001, tb[b + 1 + i]);
            }
            e.w(0x8000, tb[b]);
            e.w(0xA000, tb[b + 9]);
        },
        5 => {
            const b = t.mmc5;
            for (0..8) |i| e.w(@intCast(0x5100 + i), tb[b + i]);
            for (0..5) |i| e.w(@intCast(0x5113 + i), tb[b + 8 + i]);
            // the CHR set written last ($5128-$512B or $5120-$5127) last
            const a_set = [_]usize{ 0, 1, 2, 3, 4, 5, 6, 7 };
            const b_set = [_]usize{ 8, 9, 10, 11 };
            if (tb[b + 25] != 0) {
                for (a_set) |i| e.w(@intCast(0x5120 + i), tb[b + 13 + i]);
                for (b_set) |i| e.w(@intCast(0x5120 + i), tb[b + 13 + i]);
            } else {
                for (b_set) |i| e.w(@intCast(0x5120 + i), tb[b + 13 + i]);
                for (a_set) |i| e.w(@intCast(0x5120 + i), tb[b + 13 + i]);
            }
            e.w(0x5205, tb[b + 30]);
            e.w(0x5206, tb[b + 31]);
        },
        else => {},
    }
}

/// The PPU's fine and coarse Y increment (dot 256 of a rendering line).
fn incY(v: u16) u16 {
    if (v & 0x7000 != 0x7000) return v + 0x1000;
    var y = (v & 0x3E0) >> 5;
    var flip: u16 = 0;
    if (y == 29) {
        y = 0;
        flip = 0x800;
    } else if (y == 31) {
        y = 0;
    } else y += 1;
    return ((v & ~@as(u16, 0x73E0)) | (y << 5)) ^ flip;
}

/// Vertical bits of a scroll address: coarse Y, the vertical nametable
/// bit, fine Y.
const vert_bits: u16 = 0x7BE0;

/// The vertical bits t needs for v's after the window's 240 increments
/// (lines 0-239), or null where none leads there (coarse Y 30 or 31).
fn vertBack(vv: u16) ?u16 {
    var c: u16 = 0;
    while (c <= vert_bits) : (c += 1) {
        if (c & ~vert_bits != 0) continue;
        var x = c;
        for (0..240) |_| x = incY(x);
        if (x & vert_bits == vv & vert_bits) return c;
    }
    return null;
}

/// Coarse X (bits 0-4) and the horizontal nametable bit (10) of a
/// scroll address, one tile back (the PPU's increment undone).
fn coarseXBack(a: u16) u16 {
    return if (a & 0x1F == 0) (a | 0x1F) ^ 0x400 else a - 1;
}

/// $2005 twice and $2000's nametable bits: t = `tt`, fine x = `x`, w = 0
/// (PPUCTRL's other bits `ctrl`).
fn setT(e: *Emit, tt: u16, x: u8, ctrl: u8) void {
    e.w(0x2005, @truncate(((tt & 0x1F) << 3) | x));
    e.w(0x2005, @truncate(((tt >> 12) & 7) | (((tt >> 5) & 0x1F) << 3)));
    e.w(0x2000, ctrl | @as(u8, @truncate((tt >> 10) & 3)));
}

/// Step 3 from the strobe on, into `e`.
fn setup(e: *Emit, tb: *const [table_size]u8, ciram: *const [ciram_size]u8, sim: *Mmc3) void {
    e.w(0x4016, tb[t.strobe] & 1);
    boardRegs(e, tb);
    // palette RAM (PPUCTRL is 0: $2007 adds 1)
    e.ppuAddr(0x3F00);
    for (0..32) |i| {
        const j: usize = if (i & 0x13 == 0x10) i & 0x0F else i;
        e.w(0x2007, tb[t.pal + j]);
    }
    if (tb[t.mapper] == 4) mmc3Pre(e, tb, sim);
    // the read buffer: a byte of the console's nametable RAM written with
    // it, read (into the buffer), written back
    var q: u2 = 0;
    while (true) : (q += 1) {
        if (ntPage(tb, q)) |pg| {
            const a: u16 = 0x2000 + @as(u16, q) * 0x400;
            e.ppuAddr(a);
            e.w(0x2007, tb[t.buf]);
            e.ppuAddr(a);
            e.op(&.{ 0xAD, 0x07, 0x20 }, 4); // LDA $2007
            e.ppuAddr(a);
            e.w(0x2007, ciram[@as(usize, pg) * 0x400]);
            break;
        }
        if (q == 3) break;
    }
    // OAM, by DMA from the overlay's copy (OAMADDR 0: it wraps back to 0)
    e.w(0x2003, 0);
    e.oamDma(oam_page);
    // the APU: the channels with a length loaded first, the registers, then
    // the enables as saved (one with a length of 0 gets none)
    var with_len: u8 = 0;
    for (0..4) |ch| {
        if (tb[t.lengths + ch] != 0) with_len |= @as(u8, 1) << @intCast(ch);
    }
    e.w(0x4015, (tb[t.enabled] & 0x0F) & with_len);
    for (0..0x14) |i| {
        const a: u16 = @intCast(0x4000 + i);
        switch (i) {
            0x09, 0x0D => {},
            0x03, 0x07, 0x0B, 0x0F => {
                const ch = i / 4;
                e.w(a, (lengthIndex(tb[t.lengths + ch]) << 3) | (tb[t.apu + i] & 7));
            },
            else => e.w(a, tb[t.apu + i]),
        }
    }
    e.w(0x4015, tb[t.enabled] & 0x0F);
    // t for the window: v's vertical bits 240 increments back, coarse X
    // two tiles back (the window's increments at dots 328 and 336 bring it
    // on); PPUCTRL's other bits 0 (pattern tables at $0000, 8x8 sprites: no
    // A12 rise in the window)
    if (windowed(tb)) {
        const vv = get16(tb, t.v);
        const h = coarseXBack(coarseXBack(vv)) & ~vert_bits;
        setT(e, (vertBack(vv) orelse 0) | h, tb[t.finex] & 7, 0);
    }
}

/// Step 5: v (unless the window put it there: then the PPU's A12 is the
/// last fetch's, low) and MMC3's counter after it; t, x and w as saved
/// (PPUCTRL without its NMI bit for now); MMC5's IRQ.
fn post(e: *Emit, tb: *const [table_size]u8, sim: *Mmc3) void {
    if (!windowed(tb)) e.ppuAddr(get16(tb, t.v));
    if (tb[t.mapper] == 4) mmc3Post(e, tb, sim);
    setT(e, get16(tb, t.t), tb[t.finex] & 7, tb[t.ctrl] & 0x7C);
    if (tb[t.mapper] == 5) {
        const b = t.mmc5;
        e.bit(0x5204); // acknowledges a pending IRQ
        e.w(0x5203, tb[b + 26]);
        e.w(0x5204, if (tb[b + 27] != 0) 0x80 else 0);
    }
}

/// The DMC's buffer fetched again (step 7, if it was full): a 1-byte
/// sample at $C040 (the overlay's offset 0x40 holds the byte), no IRQ, no
/// loop, its load DMA taking the CPU's reads of the NOPs after the $4015
/// write for 3 cycles (its get on the third); the registers back. The
/// cycle of the get into `got`.
fn dmcFetch(e: *Emit, tb: *const [table_size]u8, got: *u32) void {
    const rate = tb[t.apu + 0x10] & 0x0F;
    e.w(0x4010, rate);
    e.w(0x4012, 0x01);
    e.w(0x4013, 0x00);
    e.w(0x4015, (tb[t.enabled] & 0x0F) | 0x10);
    const c = e.now();
    const due = c + (if (c % 2 == 0) @as(u32, 3) else 4);
    got.* = due + 2;
    e.op(&.{ 0xEA, 0xEA, 0xEA }, 6);
    e.cyc += 3;
    e.w(0x4012, tb[t.apu + 0x12]);
    e.w(0x4013, tb[t.apu + 0x13]);
    e.w(0x4010, tb[t.apu + 0x10]);
}

/// Step 7, into `e` (the NMI's vector set in `ovl` if it takes one; `pad`
/// NOPs before the DMC's fetch, the cycle of its get into `got`).
fn emitTail(e: *Emit, tb: *const [table_size]u8, ram: *const [ram_size]u8, pad: u32, got: *u32) void {
    const ctrl = tb[t.ctrl];
    const vbl = tb[t.status] & 0x80 != 0;
    if (!vbl) e.bit(0x2002);
    const tt = get16(tb, t.t);
    e.w(0x2000, (ctrl & 0xFC) | @as(u8, @truncate((tt >> 10) & 3)));
    if (vbl and ctrl & 0x80 != 0) {
        // the NMI: taken after the next instruction; its vector is the
        // code after it; its pushes ($01FD-$01FB: S is $FD) put back
        e.op(&.{0xEA}, 2);
        e.cyc += nmi_cycles;
        e.ovl[0x7FA] = @truncate(e.pc);
        e.ovl[0x7FB] = @truncate(e.pc >> 8);
        for ([_]u16{ 0x1FD, 0x1FC, 0x1FB }) |a| e.w(0x0100 | (a & 0xFF), ram[a]);
    }
    if (tb[t.w] != 0) e.w(0x2005, @truncate(((tt & 0x1F) << 3) | (tb[t.finex] & 7)));
    e.w(0x2001, tb[t.mask]);
    e.w(0x2003, tb[t.oamaddr]);
    // the I/O latch: $2002 is read-only, its write drives the latch alone
    // (each bit's age since then is approximate: fresh)
    e.w(0x2002, tb[t.latch]);
    if (tb[t.dmcfull] != 0) {
        for (0..pad) |_| e.op(&.{0xEA}, 2);
        dmcFetch(e, tb, got);
    }
    // last before the registers: a sequence the routine's $4017 started
    // sets the flag up to 22 cycles before the end (Nes.Spec.SaveState's
    // rest: the saved count at least 24)
    if (tb[t.frameirq] == 0 and tb[t.frame] & 0xC0 == 0) e.bit(0x4015);
    e.op(&.{ 0xA2, tb[t.s] -% 3 }, 2); // LDX #S-3
    e.op(&.{0x9A}, 2); // TXS
    e.lda(tb[t.a]);
    e.op(&.{ 0xA2, tb[t.x] }, 2);
    e.op(&.{ 0xA0, tb[t.y] }, 2);
    e.op(&.{0x40}, 6); // RTI
}

/// Rendering on from frame g's pre-render line, dot 230-252 (the PPU sees
/// PPUMASK's write three dots on, before dot 257's copy), to the next
/// frame's line 240, dot 0-60 (not a rendering line: turning off there
/// seeds no OAM corruption). False if a write can't be placed so.
fn emitWindow(e: *Emit, g: u32, skips: *const Skips) bool {
    const pre = skips.start(g) + 261 * line_dots;
    if (!e.writeIn(pre + 230, pre + 252, 0x2001, 0x08)) return false;
    const post_line = skips.start(g + 1) + 240 * line_dots;
    return e.writeIn(post_line, post_line + 60, 0x2001, 0x00);
}

/// The restore routine for a state table, into `ovl`, with the saved RAM
/// (the NMI's pushes put back), nametable RAM (the read buffer's byte put
/// back) and OAM (the DMA's page).
pub fn build(tb: *const [table_size]u8, ram: *const [ram_size]u8, ciram: *const [ciram_size]u8, oam: *const [oam_size]u8, ovl: *[overlay_size]u8) Error!Built {
    const line: u32 = get16(tb, t.line);
    const dot: u32 = get16(tb, t.dot);
    if (line < first_line or line > last_line or dot >= line_dots) return error.BadPosition;
    const pos = line * line_dots + dot;
    const odd: u32 = tb[t.odd] & 1;
    const parity: u32 = tb[t.parity] & 1;
    const mode: u1 = @truncate(tb[t.frame] >> 7);
    const seq: u32 = get16(tb, t.seq);
    const flag_kept = tb[t.frameirq] != 0;
    const rs: u32 = dmc_rates[tb[t.apu + 0x10] & 0x0F];
    const dmc_count: u32 = get16(tb, t.dmccount);
    const dmc_bits: u32 = tb[t.dmcbits];
    const dmc_full = tb[t.dmcfull] != 0;
    // the DMC's buffer byte, for its fetch at $C040
    ovl[0x040] = tb[t.dmcbuf];

    const win = windowed(tb);
    var pad: u32 = 0;
    while (pad < (if (dmc_full) @as(u32, 4) else 1)) : (pad += 1) {
    // the tail's cycles (into a scratch copy)
    var tail_buf: [overlay_size]u8 = undefined;
    @memset(&tail_buf, 0);
    var tail = Emit{ .ovl = &tail_buf };
    var got_scratch: u32 = 0;
    emitTail(&tail, tb, ram, pad, &got_scratch);
    const tail_cyc = tail.cyc;

    var f: u32 = 1;
    while (f < 64) : (f += 1) {
        if (f % 2 != odd) continue;
        for ([_]bool{ false, true }) |extra| {
            // the frames a dot short: frame 1 for the parity, and the
            // window's frame f - 1 if odd
            var skips = Skips{};
            if (extra) {
                if (f <= 2 or (win and f - 1 <= 1)) continue;
                skips.add(1);
            }
            if (win and (f - 1) % 2 == 1) skips.add(f - 1);
            const num = skips.start(f) + pos + 1;
            if (num % 3 != 0) continue;
            const n_end = num / 3;
            if (n_end % 2 != parity) continue;
            if (n_end < tail_cyc + 8) continue;
            const n_ts = n_end - tail_cyc;
            const pt = posOf(dotsAfter(n_ts), &skips);
            if (pt.frame != f or pt.line * line_dots + pt.dot < 241 * line_dots + 3) continue;

            // the DMC's reloads for the saved timer and bits at n_end
            const plan = dmcPlan(rs, n_end, dmc_count, dmc_bits) orelse continue;

            // steps 1 to 3
            @memset(ovl, 0);
            ovl[0x040] = tb[t.dmcbuf];
            var e = Emit{ .ovl = ovl };
            // the DMC's rates (no IRQ, no loop): each written before the
            // reload that takes it, after the one before
            e.w(0x4010, if (plan.d > 0) @as(u8, plan.p[0]) else tb[t.apu + 0x10] & 0x0F);
            var ok = true;
            var k_r: u32 = 1;
            while (k_r <= plan.d) : (k_r += 1) {
                const rate: u8 = if (k_r < plan.d) @as(u8, plan.p[k_r]) else tb[t.apu + 0x10] & 0x0F;
                if (!e.writeCycle(plan.at(k_r - 1, rs) + 1, plan.at(k_r, rs) - 1, 0x4010, rate)) ok = false;
            }
            if (!ok) continue;
            var reset0: ?u32 = null;
            if (!flag_kept) {
                e.w(0x4017, 0x40);
                reset0 = e.now() + (if (e.now() % 2 == 0) @as(u32, 3) else 4);
            }
            if (extra and !emitWindow(&e, 1, &skips)) continue;
            var sim = Mmc3{};
            setup(&e, tb, ciram, &sim);
            if (e.err) return error.TooLong;
            const mark = e;

            // step 6's $4017 write c_w: its sequencer reset (d = 3 cycles
            // later on an even cycle, 4 on an odd) leaves the saved count at
            // n_end, after k sequences; R even (a sequencer never reset since
            // power-on) is a cycle off. Before the window (`early`) or after
            // step 5.
            var k: u32 = 0;
            while (k < 3) : (k += 1) {
                if (n_end < seq + k * seq_period[mode] + 8) break;
                var r = n_end - seq - k * seq_period[mode];
                if (r % 2 == 0) r += 1;
                for ([_]u32{ 3, 4 }) |d| {
                    const c_w = r - d;
                    if ((c_w % 2 == 0) != (d == 3)) continue;
                    if (flag_kept and c_w <= 29830) continue;
                    if (reset0) |r0| {
                        // the preamble's sequencer, inhibited, sets the flag
                        // on its last two cycles and clears it on the third:
                        // the new reset must not cut that short
                        const c = (c_w + d - 1 - r0) % seq_period[0];
                        if (c == 29828 or c == 29829) continue;
                    }
                    for ([_]bool{ false, true }) |early| {
                        e = mark;
                        if (early) {
                            if (c_w < e.now() + 6) continue;
                            const d1 = c_w - 6 - e.now();
                            if (d1 == 1) continue;
                            e.delay(d1);
                            e.w(0x4017, tb[t.frame] & 0xC0);
                        }
                        if (win and !emitWindow(&e, f - 1, &skips)) continue;
                        var sim2 = sim;
                        post(&e, tb, &sim2);
                        if (!early) {
                            if (c_w < e.now() + 6) continue;
                            const d1 = c_w - 6 - e.now();
                            if (d1 == 1) continue;
                            e.delay(d1);
                            e.w(0x4017, tb[t.frame] & 0xC0);
                        }
                        if (n_ts < e.now()) continue;
                        const d2 = n_ts - e.now();
                        if (d2 == 1) continue;
                        e.delay(d2);
                        const tail_start = e.pc;
                        var got: u32 = 0;
                        emitTail(&e, tb, ram, pad, &got);
                        if (e.err) return error.TooLong;
                        std.debug.assert(e.pc - tail_start == tail.pc - code_addr);
                        std.debug.assert(e.now() == n_end);
                        if (dmc_full) {
                            // the byte lands after the output unit last took
                            // one, not on a reload nor 1 or 2 cycles before
                            // one (the DMC's DMA quirks: Nes.Spec.Apu)
                            const at_end = dmcAt(&plan, rs, n_end).?;
                            if (got <= at_end.took + 2) continue;
                            const e_d = plan.at(plan.d, rs);
                            const ph = (got - e_d) % rs;
                            if (ph == 0 or ph == rs - 1 or ph == rs - 2) continue;
                        }
                        // the vectors, RTI's pulls, OAM's copy
                        ovl[0x7FC] = @truncate(code_addr);
                        ovl[0x7FD] = @truncate(code_addr >> 8);
                        ovl[0x7FE] = @truncate(code_addr);
                        ovl[0x7FF] = @truncate(code_addr >> 8);
                        const sp = tb[t.s];
                        ovl[stack_ovl | @as(u16, sp -% 2)] = tb[t.p];
                        ovl[stack_ovl | @as(u16, sp -% 1)] = tb[t.pc];
                        ovl[stack_ovl | @as(u16, sp)] = tb[t.pc + 1];
                        @memcpy(ovl[oam_ovl..][0..oam_size], oam);
                        return .{ .end = e.pc - 1, .cycles = n_end };
                    }
                }
            }
        }
    }
    }
    return error.NoSchedule;
}

// --- tests (zig build test) ---------------------------------------------

/// CPU cycles from `from` to RTI's end (at `end`), by running the few
/// instructions the routine uses (X and Y for the delay loops; OAM DMA's
/// halt; an NMI after the NOP that follows a PPUCTRL write enabling one,
/// if `nmi`).
fn countCycles(ovl: *const [overlay_size]u8, from: u16, end: u16, nmi: bool) !u32 {
    var pc: u16 = from;
    var x: u8 = 0;
    var y: u8 = 0;
    var a: u8 = 0;
    var z = false;
    var n: u32 = 7;
    var nmi_next = false;
    var nmi_done = false;
    while (true) {
        const nmi_now = nmi_next;
        nmi_next = false;
        const o = ovl[pc % overlay_size];
        const b1 = ovl[(pc + 1) % overlay_size];
        const b2 = ovl[(pc + 2) % overlay_size];
        switch (o) {
            0xA9 => {
                a = b1;
                n += 2;
                pc += 2;
            },
            0xA0 => {
                y = b1;
                n += 2;
                pc += 2;
            },
            0x88 => {
                y -%= 1;
                z = y == 0;
                n += 2;
                pc += 1;
            },
            0xA2 => {
                x = b1;
                n += 2;
                pc += 2;
            },
            0xCA => {
                x -%= 1;
                z = x == 0;
                n += 2;
                pc += 1;
            },
            0x8D => {
                n += 4;
                pc += 3;
                const addr = @as(u16, b1) | (@as(u16, b2) << 8);
                if (addr == 0x4014) n += if (n % 2 == 0) 514 else 513;
                if (addr == 0x4015 and a & 0x10 != 0) n += 3;
                if (addr == 0x2000 and a & 0x80 != 0 and nmi and !nmi_done) {
                    nmi_next = true;
                    nmi_done = true;
                }
            },
            0xAD, 0x2C => {
                n += 4;
                pc += 3;
            },
            0xEA => {
                n += 2;
                pc += 1;
                if (nmi_now) {
                    n += nmi_cycles;
                    pc = @as(u16, ovl[0x7FA]) | (@as(u16, ovl[0x7FB]) << 8);
                }
            },
            0x9A => {
                n += 2;
                pc += 1;
            },
            0x24 => {
                n += 3;
                pc += 2;
            },
            0xD0 => {
                if (!z) {
                    const next = pc + 2;
                    const to: u16 = @intCast(@as(i32, next) + @as(i8, @bitCast(b1)));
                    n += if ((to >> 8) != (next >> 8)) 4 else 3;
                    pc = to;
                } else {
                    n += 2;
                    pc += 2;
                }
            },
            0x40 => {
                if (pc != end) return error.EndElsewhere;
                return n + 6;
            },
            else => return error.UnknownOpcode,
        }
    }
}

fn sampleTable() [table_size]u8 {
    var tb = [_]u8{0} ** table_size;
    tb[t.a] = 0x12;
    tb[t.x] = 0x34;
    tb[t.y] = 0x56;
    tb[t.s] = 0xF0;
    tb[t.p] = 0x24;
    tb[t.pc] = 0x78;
    tb[t.pc + 1] = 0x9A;
    tb[t.line] = 250;
    tb[t.dot] = 100;
    tb[t.ctrl] = 0x90;
    tb[t.mask] = 0x1E;
    tb[t.status] = 0x80;
    tb[t.v] = 0x34;
    tb[t.v + 1] = 0x12;
    tb[t.frame] = 0x40;
    tb[t.dmccount] = 100;
    tb[t.dmcbits] = 8;
    return tb;
}

const zeros_ram = [_]u8{0} ** ram_size;
const zeros_ciram = [_]u8{0} ** ciram_size;
const zeros_oam = [_]u8{0} ** oam_size;

test "the PPU's position: three dots a cycle, the skip a dot short" {
    const none = Skips{};
    var one = Skips{};
    one.add(1);
    try std.testing.expectEqual(Pos{ .frame = 0, .line = 0, .dot = 2 }, posOf(dotsAfter(1), &none));
    try std.testing.expectEqual(Pos{ .frame = 1, .line = 0, .dot = 0 }, posOf(frame_dots, &none));
    try std.testing.expectEqual(Pos{ .frame = 1, .line = 261, .dot = 340 }, posOf(2 * frame_dots - 1, &none));
    try std.testing.expectEqual(Pos{ .frame = 2, .line = 0, .dot = 0 }, posOf(2 * frame_dots - 1, &one));
    try std.testing.expectEqual(Pos{ .frame = 1, .line = 261, .dot = 339 }, posOf(2 * frame_dots - 2, &one));
    try std.testing.expectEqual(Pos{ .frame = 3, .line = 0, .dot = 5 }, posOf(3 * frame_dots - 1 + 5, &one));
}

test "the delay is exact, any length but 1" {
    var buf: [overlay_size]u8 = undefined;
    for ([_]u32{ 0, 2, 3, 4, 5, 6, 7, 10, 11, 12, 13, 1281, 1282, 1286, 1287, 1298, 1299, 5000, 29000, 60001, 200000 }) |d| {
        for ([_]u16{ code_addr, code_addr + 0xF6, code_addr + 0xFC, code_addr + 0xFE }) |at| {
            @memset(&buf, 0);
            var e = Emit{ .ovl = &buf, .pc = at };
            e.delay(d);
            e.op(&.{0x40}, 6);
            try std.testing.expectEqual(7 + d + 6, try countCycles(&buf, at, e.pc - 1, false));
        }
    }
}

test "the routine ends on the saved position, every line, dot, frame and cycle parity" {
    var ovl: [overlay_size]u8 = undefined;
    var line: u16 = first_line;
    while (line <= last_line) : (line += 1) {
        var dot: u16 = 0;
        while (dot < line_dots) : (dot += 7) {
            for ([_]u8{ 0, 1 }) |odd| {
                for ([_]u8{ 0, 1 }) |par| {
                    for ([_]u8{ 0x80, 0x00 }) |status| {
                        var tb = sampleTable();
                        tb[t.line] = @truncate(line);
                        tb[t.line + 1] = @truncate(line >> 8);
                        tb[t.dot] = @truncate(dot);
                        tb[t.dot + 1] = @truncate(dot >> 8);
                        tb[t.odd] = odd;
                        tb[t.parity] = par;
                        tb[t.status] = status;
                        const r = build(&tb, &zeros_ram, &zeros_ciram, &zeros_oam, &ovl) catch |er| {
                            std.debug.print("line {d} dot {d} odd {d} parity {d} status {x}: {s} (table line {d} dot {d})\n", .{ line, dot, odd, par, status, @errorName(er), get16(&tb, t.line), get16(&tb, t.dot) });
                            return er;
                        };
                        try std.testing.expectEqual(@as(u32, par), r.cycles % 2);
                        try std.testing.expectEqual(r.cycles, try countCycles(&ovl, code_addr, r.end, status != 0));
                    }
                }
            }
        }
    }
}

test "the frame counter's reset leaves the saved count" {
    var ovl: [overlay_size]u8 = undefined;
    for ([_]u16{ 30, 31, 200, 5000, 29000, 29823 }) |seq| {
        for ([_]u8{ 0x00, 0x40, 0x80, 0xC0 }) |fr| {
            var tb = sampleTable();
            tb[t.seq] = @truncate(seq);
            tb[t.seq + 1] = @truncate(seq >> 8);
            tb[t.frame] = fr;
            const r = try build(&tb, &zeros_ram, &zeros_ciram, &zeros_oam, &ovl);
            _ = r;
        }
    }
}

test "the DMC's timer and bits: a plan for every count and bit" {
    for ([_]u4{ 0, 5, 9, 15 }) |ri| {
        const rs: u32 = dmc_rates[ri];
        for ([_]u32{ 90001, 150000, 150001 }) |n_end| {
            var count: u32 = 0;
            while (count < rs / 2) : (count += 5) {
                var bits: u32 = 1;
                while (bits <= 8) : (bits += 1) {
                    const plan = dmcPlan(rs, n_end, count, bits) orelse {
                        std.debug.print("rate {d} n_end {d} count {d} bits {d}: no plan\n", .{ rs, n_end, count, bits });
                        return error.NoPlan;
                    };
                    const r = dmcAt(&plan, rs, n_end).?;
                    try std.testing.expectEqual(count, r.count);
                    try std.testing.expectEqual(bits, r.bits);
                }
            }
        }
    }
}

test "a full DMC buffer: fetched in the tail" {
    var ovl: [overlay_size]u8 = undefined;
    var tb = sampleTable();
    tb[t.dmcfull] = 1;
    tb[t.dmcbuf] = 0xA5;
    const r = try build(&tb, &zeros_ram, &zeros_ciram, &zeros_oam, &ovl);
    try std.testing.expectEqual(@as(u8, 0xA5), ovl[0x040]);
    try std.testing.expectEqual(r.cycles, try countCycles(&ovl, code_addr, r.end, true));
}

test "MMC1: its registers and a part-filled shift register, replayed" {
    // Nes.Spec.Mapper's serial port: bit 0 into bit 4, shifted down; the
    // fifth write stores the 5 bits by its address
    for ([_]u8{ 0, 1, 2, 3, 4 }) |count| {
        var buf: [overlay_size]u8 = undefined;
        @memset(&buf, 0);
        var tb = sampleTable();
        tb[t.mapper] = 1;
        tb[t.mmc1] = 0x1A & ~(@as(u8, 0x1F) >> @intCast(count)); // the bits shifted in so far
        tb[t.mmc1 + 1] = count;
        tb[t.mmc1 + 2] = 0x1D;
        tb[t.mmc1 + 3] = 0x05;
        tb[t.mmc1 + 4] = 0x12;
        tb[t.mmc1 + 5] = 0x0B;
        var e = Emit{ .ovl = &buf };
        boardRegs(&e, &tb);
        var sh: u8 = 0;
        var n: u8 = 0;
        var regs = [4]u8{ 0x0C, 0, 0, 0 };
        var pc: u16 = code_addr;
        while (pc < e.pc) : (pc += 5) {
            // LDA #v; STA a
            const v = buf[(pc + 1) % overlay_size];
            const a = @as(u16, buf[(pc + 3) % overlay_size]) | (@as(u16, buf[(pc + 4) % overlay_size]) << 8);
            sh = (sh >> 1) | ((v & 1) << 4);
            n += 1;
            if (n == 5) {
                regs[(a >> 13) & 3] = sh;
                sh = 0;
                n = 0;
            }
        }
        try std.testing.expectEqualSlices(u8, tb[t.mmc1 + 2 .. t.mmc1 + 6], &regs);
        try std.testing.expectEqual(count, n);
        try std.testing.expectEqual(tb[t.mmc1], sh);
    }
}

test "positions a state isn't taken at are refused" {
    var ovl: [overlay_size]u8 = undefined;
    for ([_][2]u16{ .{ 100, 3 }, .{ 241, 20 }, .{ 242, 300 }, .{ 261, 0 }, .{ 250, 341 } }) |p| {
        var tb = sampleTable();
        tb[t.line] = @truncate(p[0]);
        tb[t.dot] = @truncate(p[1]);
        tb[t.dot + 1] = @truncate(p[1] >> 8);
        try std.testing.expectError(error.BadPosition, build(&tb, &zeros_ram, &zeros_ciram, &zeros_oam, &ovl));
    }
}

test "MMC3: the counter, reload, enable and pending flag as saved" {
    var ovl: [overlay_size]u8 = undefined;
    for ([_]u16{ 0x0000, 0x1234 }) |vv| {
        for ([_][5]u8{ .{ 5, 9, 0, 0, 0 }, .{ 0, 9, 1, 1, 0 }, .{ 3, 7, 0, 1, 1 }, .{ 0, 0, 0, 1, 1 }, .{ 0, 4, 1, 0, 0 } }) |c| {
            var tb = sampleTable();
            tb[t.mapper] = 4;
            tb[t.v] = @truncate(vv);
            tb[t.v + 1] = @truncate(vv >> 8);
            tb[t.mmc3 + 11] = c[0];
            tb[t.mmc3 + 10] = c[1];
            tb[t.mmc3 + 12] = c[2];
            tb[t.mmc3 + 13] = c[3];
            tb[t.mmc3 + 14] = c[4];
            _ = try build(&tb, &zeros_ram, &zeros_ciram, &zeros_oam, &ovl);
            var e = Emit{ .ovl = &ovl };
            var sim = Mmc3{};
            mmc3Pre(&e, &tb, &sim);
            mmc3Post(&e, &tb, &sim);
            const want = mmc3Saved(&tb);
            try std.testing.expectEqual(want.count, sim.count);
            try std.testing.expectEqual(want.reload, sim.reload);
            try std.testing.expectEqual(want.latch, sim.latch);
            try std.testing.expectEqual(want.en, sim.en);
            try std.testing.expectEqual(want.flag, sim.flag);
        }
    }
}

test "the offsets are Nes.StateTable's (systems/nes/test/golden/state-table.txt)" {
    const golden = try std.Io.Dir.cwd().readFileAlloc(std.testing.io, "systems/nes/test/golden/state-table.txt", std.testing.allocator, .limited(1 << 16));
    defer std.testing.allocator.free(golden);
    var it = std.mem.tokenizeScalar(u8, golden, '\n');
    var seen: usize = 0;
    while (it.next()) |ln| {
        var f = std.mem.tokenizeScalar(u8, ln, ' ');
        const name = f.next() orelse continue;
        if (std.mem.eql(u8, name, "size")) {
            try std.testing.expectEqual(@as(usize, table_size), try std.fmt.parseInt(usize, f.next().?, 0));
            continue;
        }
        const off = try std.fmt.parseInt(usize, f.next().?, 0);
        var lower: [16]u8 = undefined;
        const low = std.ascii.lowerString(&lower, name);
        inline for (@typeInfo(t).@"struct".decls) |d| {
            if (std.mem.eql(u8, d.name, low)) {
                try std.testing.expectEqual(off, @as(usize, @field(t, d.name)));
                seen += 1;
            }
        }
    }
    try std.testing.expectEqual(@typeInfo(t).@"struct".decls.len, seen);
}
