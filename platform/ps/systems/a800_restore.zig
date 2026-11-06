//! The Atari 800XL's restore routine (side track N, PLAN.md N.4): loading a
//! state through the machine's own 6502, as the Game Boy's
//! (gb_restore.zig) and the Game Gear's (gg_restore.zig). The PS holds the
//! core (its reset: the whole machine at power-on, ANTIC at line 0, cycle
//! 0, the 6502 starting its reset sequence), writes RAM through its
//! window, puts the overlay this file builds into the core's ROM overlay
//! and lets the core go. Every CPU read outside the I/O area ($D000-$D7FF)
//! then answers the overlay's byte (the address modulo `overlay_size`),
//! the reset vector included, until the instruction that read the address
//! `build` returns (the routine's RTI) has ended; the next fetch is the
//! game's. ANTIC's DMA never sees the overlay, and the routine writes no
//! memory, only I/O: RTI takes P and PC from the overlay's stack page.
//!
//! The routine is straight-line code, generated per state, its cycles
//! known exactly: with DMACTL 0 (as the reset leaves it, until the late
//! writes) ANTIC takes only its nine refresh cycles a line (25 to 57,
//! every fourth), so the machine cycle of each CPU cycle follows
//! (`machineOf`):
//!   1. the executable loader: its port read on to the saved position
//!      in the file (n reads of $D5FE), then its stub cartridge and its
//!      page off ($D5FE, $D5FF written) if the saved state had them gone
//!      (a game started from an INIT segment never returns to the
//!      loader: its page stays; without an executable these do nothing);
//!   2. the PIA: directions, outputs, then CA2's and CB2's edge, flag and
//!      CB2 level put back by a few control writes before the last
//!      (`piaCtl`);
//!   3. GTIA: positions, sizes, the data latches (GRAFPn, GRAFM: what
//!      the objects show, the DMA's or the CPU's), colours, PRIOR, VDELAY,
//!      GRACTL, CONSOL;
//!   4. POKEY: AUDCTL, AUDF, AUDC, SKCTL, STIMER, SKRES, SEROUT if its
//!      byte waits (SKCTL gives it no clock);
//!   5. ANTIC: CHACTL, DLIST, HSCROL, VSCROL, PMBASE, CHBASE, and NMIRES
//!      if the saved NMIST shows the vertical blank's NMI (line 248 then
//!      sets it again);
//!   6. a delay, so that RTI's last cycle is the saved cycle (`t.line`,
//!      `t.cycle`): in the frame whose line 248 comes after the setup;
//!   7. the tail, after line 248's NMI decision: DMACTL, HITCLR, IRQEN,
//!      NMIRES if NMIST shows no NMI, NMIEN; S (for RTI's pulls), A, X,
//!      Y; RTI.
//! What it can't put back (approximate by design, the user 2026-10-14):
//! POKEY's counters, polys (RANDOM), 15 and 64 kHz phases, IRQST, SKSTAT
//! and KBCODE; GTIA's collisions (HITCLR clears them), trigger latch and
//! the P/M shifters (they line up within a line); ANTIC's memory scan
//! counter, previous instruction and hires flag (vertical blank keeps
//! them from the display list's end: a list starting without LMS, or not
//! ending in JVB), the light pen registers, the frame count; the 6502's B
//! flag.
//!
//! The state table's offsets are A800.StateTable's; the test below
//! checks them against systems/a800xl/test/golden/state-table.txt.

const std = @import("std");

pub const table_size = 0x80;
pub const overlay_size = 0x800;

const line_cycles: u32 = 114;
const frame_lines: u32 = 262;
pub const frame_cycles: u32 = line_cycles * frame_lines;

pub const t = struct {
    pub const a = 0x00;
    pub const x = 0x01;
    pub const y = 0x02;
    pub const s = 0x03;
    pub const p = 0x04;
    pub const pc = 0x05;
    pub const line = 0x08;
    pub const cycle = 0x0A;
    pub const nmist = 0x0B;
    pub const dmactl = 0x0C;
    pub const chactl = 0x0D;
    pub const dlist = 0x0E;
    pub const hscrol = 0x10;
    pub const vscrol = 0x11;
    pub const pmbase = 0x12;
    pub const chbase = 0x13;
    pub const nmien = 0x14;
    pub const penh = 0x15;
    pub const penv = 0x16;
    pub const irprev = 0x17;
    pub const msc = 0x18;
    pub const hires = 0x1A;
    pub const loader = 0x1B;
    pub const xexpos = 0x1C;
    pub const pactl = 0x20;
    pub const pbctl = 0x21;
    pub const ora = 0x22;
    pub const ddra = 0x23;
    pub const orb = 0x24;
    pub const ddrb = 0x25;
    pub const edges = 0x26;
    pub const cb2 = 0x27;
    pub const audf = 0x30;
    pub const audc = 0x34;
    pub const audctl = 0x38;
    pub const skctl = 0x39;
    pub const irqen = 0x3A;
    pub const irqst = 0x3B;
    pub const skstat = 0x3C;
    pub const kbcode = 0x3D;
    pub const serout = 0x3E;
    pub const serial = 0x3F;
    pub const gtia = 0x40;
    pub const latch = 0x60;
    pub const coll = 0x68;
    pub const triglatch = 0x70;
};

/// Where the code starts (its first byte's overlay offset 0x200: the stack
/// page's offsets, 0x100-0x1FF, hold RTI's P and PC).
const code_addr: u16 = 0xFA00;
const code_end: u16 = 0xFFFA;
const stack_ovl: u16 = 0x100;

pub const Error = error{
    /// the routine doesn't fit the overlay
    TooLong,
    /// the saved position isn't one a state is taken at
    BadPosition,
    /// the loader port's position past 64 KiB
    BadLoader,
};

pub const Built = struct {
    /// the address whose read ends the overlay (the RTI's opcode)
    end: u16,
    /// machine cycles from the release to RTI's last cycle
    cycles: u32,
};

/// Whether machine cycle x of a line is a refresh cycle.
fn refresh(x: u32) bool {
    return x >= 25 and x <= 57 and (x - 25) % 4 == 0;
}

/// CPU cycles among machine cycles 1 to m (cycle 0, the power-on, isn't run).
pub fn cpuCount(m: u32) u32 {
    const lines = (m + 1) / line_cycles;
    const part = (m + 1) % line_cycles;
    var n: u32 = lines * (line_cycles - 9) + part;
    var x: u32 = 25;
    while (x <= 57) : (x += 4) {
        if (x < part) n -= 1;
    }
    return n - 1;
}

/// The machine cycle of CPU cycle n (from 1).
pub fn machineOf(n: u32) u32 {
    var m: u32 = (n / (line_cycles - 9)) * line_cycles;
    while (cpuCount(m) < n) m += 1;
    while (m > 0 and cpuCount(m - 1) >= n) m -= 1;
    return m;
}

const Emit = struct {
    ovl: *[overlay_size]u8,
    pc: u16 = code_addr,
    /// CPU cycles so far (after the reset sequence's 7)
    cyc: u32 = 0,
    err: bool = false,

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
    fn w(self: *Emit, a: u16, v: u8) void {
        self.lda(v);
        self.sta(a);
    }
    /// `n` reads of the loader port ($D5FE), Y and X counting: a 256-read
    /// loop run n / 256 times, then one of n % 256. A block never
    /// straddles a page (its branches would take a cycle more).
    fn reads(self: *Emit, n: u16) void {
        const h = n >> 8;
        const r = n & 0xFF;
        if (h > 0) {
            while ((self.pc >> 8) != ((self.pc + 12) >> 8)) self.op(&.{0xEA}, 2);
            self.op(&.{ 0xA0, @truncate(h) }, 2);
            // L1: LDX #0; L2: LDA $D5FE; DEX; BNE L2; DEY; BNE L1
            self.op(&.{ 0xA2, 0x00, 0xAD, 0xFE, 0xD5, 0xCA, 0xD0, 0xFA, 0x88, 0xD0, 0xF5 }, 2310 * @as(u32, h) - 1);
        }
        if (r > 0) {
            while ((self.pc >> 8) != ((self.pc + 8) >> 8)) self.op(&.{0xEA}, 2);
            // LDX #r; L: LDA $D5FE; DEX; BNE L
            self.op(&.{ 0xA2, @truncate(r), 0xAD, 0xFE, 0xD5, 0xCA, 0xD0, 0xFA }, 9 * @as(u32, r) + 1);
        }
    }
    /// Exactly `n` CPU cycles (X and the flags change), any n but 1:
    /// LDX #c; DEX; BNE (5c + 1) for the bulk, NOP (2) and BIT $00 (3) for
    /// the rest; a loop never straddles a page (its branch would take a
    /// cycle more).
    fn delay(self: *Emit, n: u32) void {
        std.debug.assert(n != 1);
        var left = n;
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
};

fn get16(tb: *const [table_size]u8, o: usize) u16 {
    return @as(u16, tb[o]) | (@as(u16, tb[o + 1]) << 8);
}

/// CA2's or CB2's state in the PIA: the edge waiting to show, the flag
/// (control bit 6), CB2 as driven (0 floating, 1 high, 2 low).
const PiaLine = struct { edge: bool = false, flag: bool = false, cb2: u2 = 0 };

/// A control write's effect on it (A800.Spec.Machine's piaWrite).
fn piaStep(port_b: bool, s: PiaLine, v: u8) PiaLine {
    const m = v & 0x38;
    var r = s;
    if (!port_b) {
        switch (m) {
            0x30 => r.edge = true,
            0x10, 0x18 => {
                if (s.edge) r.flag = true;
                r.edge = false;
            },
            0x20, 0x38 => {},
            else => r.edge = false,
        }
    } else {
        if (m < 0x20) {
            if (s.edge) r.flag = true;
            r.edge = false;
            r.cb2 = 0;
        } else if (m == 0x20) {
            r.edge = false;
        } else if (m == 0x28) {
            r.edge = false;
            r.cb2 = 1;
        } else if (m == 0x30) {
            r.edge = false;
            r.cb2 = 2;
        } else {
            if (s.cb2 == 2) r.edge = true;
            r.cb2 = 1;
        }
    }
    if (v & 0x20 != 0) r.flag = false;
    return r;
}

/// The control writes ($D302 / $D303) that leave CA2's or CB2's line as
/// saved, the saved control (bits 0-5) last: up to three before it, found
/// by trying them (the reset left edge, flag and CB2 clear, and the
/// direction and output writes before kept them so). The earlier writes
/// keep bits 0-2 as the last has them. None found (a state no writes
/// reach): the last alone.
fn piaCtl(e: *Emit, port_b: bool, ctl: u8, want: PiaLine) void {
    const reg: u16 = if (port_b) 0xD303 else 0xD302;
    const final = ctl & 0x3F;
    const keep = final & 0x07;
    const modes = [_]u8{ 0x00, 0x10, 0x20, 0x28, 0x30, 0x38 };
    const eq = struct {
        fn f(pb: bool, a: PiaLine, b: PiaLine) bool {
            return a.edge == b.edge and a.flag == b.flag and (!pb or a.cb2 == b.cb2);
        }
    }.f;
    var n: usize = 0;
    while (n <= 3) : (n += 1) {
        var idx = [_]usize{0} ** 3;
        const total = std.math.pow(usize, modes.len, n);
        var k: usize = 0;
        while (k < total) : (k += 1) {
            var q = k;
            var s = PiaLine{};
            for (0..n) |i| {
                idx[i] = q % modes.len;
                q /= modes.len;
                s = piaStep(port_b, s, keep | modes[idx[i]]);
            }
            s = piaStep(port_b, s, final);
            if (eq(port_b, s, want)) {
                for (0..n) |i| e.w(reg, keep | modes[idx[i]]);
                e.w(reg, final);
                return;
            }
        }
    }
    e.w(reg, final);
}

/// The restore routine for a state table, into `ovl`.
pub fn build(tb: *const [table_size]u8, ovl: *[overlay_size]u8) Error!Built {
    const line = get16(tb, t.line);
    const cyc = tb[t.cycle];
    const vblank = line >= 248 or line < 8;
    if (line >= frame_lines or cyc >= line_cycles or !vblank or refresh(cyc))
        return error.BadPosition;
    @memset(ovl, 0);
    // the reset vector; RTI's pulls
    ovl[0xFFFC % overlay_size] = @truncate(code_addr);
    ovl[0xFFFD % overlay_size] = @truncate(code_addr >> 8);
    const sp = tb[t.s];
    ovl[stack_ovl | @as(u16, sp -% 2)] = tb[t.p];
    ovl[stack_ovl | @as(u16, sp -% 1)] = tb[t.pc];
    ovl[stack_ovl | @as(u16, sp)] = tb[t.pc + 1];

    const xexpos: u32 = @as(u32, tb[t.xexpos]) | (@as(u32, tb[t.xexpos + 1]) << 8) | (@as(u32, tb[t.xexpos + 2]) << 16);
    if (xexpos > 0xFFFF) return error.BadLoader;

    var e = Emit{ .ovl = ovl };
    // 1. the loader: its port read on to the saved position, then the
    // stub cartridge and the page off if they were
    e.reads(@intCast(xexpos));
    if (tb[t.loader] & 1 == 0) e.sta(0xD5FE);
    if (tb[t.loader] & 2 == 0) e.sta(0xD5FF);
    // 2. the PIA: direction (bit 2 clear), output (set), the controls
    const edges = tb[t.edges];
    e.w(0xD302, 0x00);
    e.w(0xD300, tb[t.ddra]);
    e.w(0xD302, 0x04);
    e.w(0xD300, tb[t.ora]);
    e.w(0xD303, 0x00);
    e.w(0xD301, tb[t.ddrb]);
    e.w(0xD303, 0x04);
    e.w(0xD301, tb[t.orb]);
    piaCtl(&e, false, tb[t.pactl], .{ .edge = edges & 1 != 0, .flag = tb[t.pactl] & 0x40 != 0 });
    piaCtl(&e, true, tb[t.pbctl], .{ .edge = edges & 2 != 0, .flag = tb[t.pbctl] & 0x40 != 0, .cb2 = @truncate(tb[t.cb2]) });
    // 3. GTIA
    for (0x00..0x0D) |k| e.w(@intCast(0xD000 + k), tb[t.gtia + k]);
    for (0..4) |k| e.w(@intCast(0xD00D + k), tb[t.latch + k]);
    {
        var m: u8 = 0;
        for (0..4) |k| m |= (tb[t.latch + 4 + k] >> 6) << @intCast(2 * k);
        e.w(0xD011, m);
    }
    for (0x12..0x1E) |k| e.w(@intCast(0xD000 + k), tb[t.gtia + k]);
    e.w(0xD01F, tb[t.gtia + 0x1F]);
    // 4. POKEY
    e.w(0xD208, tb[t.audctl]);
    for (0..4) |k| {
        e.w(@intCast(0xD200 + 2 * k), tb[t.audf + k]);
        e.w(@intCast(0xD201 + 2 * k), tb[t.audc + k]);
    }
    e.w(0xD20F, tb[t.skctl]);
    e.sta(0xD209); // STIMER
    e.sta(0xD20A); // SKRES: SKSTAT's latched errors clear (bits 5-7; set, no write reaches)
    // a byte waiting in SEROUT with no clock to send it (A800.Spec.Pokey's pokeyQuiet)
    if (tb[t.serial] & 1 != 0) e.w(0xD20D, tb[t.serout]);
    // 5. ANTIC
    e.w(0xD401, tb[t.chactl]);
    const dl = get16(tb, t.dlist);
    e.w(0xD402, @truncate(dl));
    e.w(0xD403, @truncate(dl >> 8));
    e.w(0xD404, tb[t.hscrol]);
    e.w(0xD405, tb[t.vscrol]);
    e.w(0xD407, tb[t.pmbase]);
    e.w(0xD409, tb[t.chbase]);
    const vbi_shown = tb[t.nmist] & 0x40 != 0;
    const reset_shown = tb[t.nmist] & 0x20 != 0;
    if (vbi_shown and !reset_shown) e.sta(0xD40F); // NMIRES: line 248 sets bit 6 again
    const setup_end = 7 + e.cyc;

    // the tail into a scratch copy first, for its length
    var tail_buf: [overlay_size]u8 = undefined;
    var tail = Emit{ .ovl = &tail_buf };
    emitTail(&tail, tb);
    const tail_cyc = tail.cyc;

    // 6. the frame: the first whose saved cycle comes after the setup, the
    // delay and the tail, with line 248's NMI decision (cycle 9) after the
    // setup and before the tail
    var f: u32 = 0;
    const end_m: u32, const dly: u32 = while (f < 64) : (f += 1) {
        const k = f * frame_cycles + @as(u32, line) * line_cycles + cyc;
        const n = cpuCount(k);
        if (n < setup_end + tail_cyc) continue;
        const d = n - setup_end - tail_cyc;
        if (d == 1) continue;
        const after = line > 248 or (line == 248 and cyc > 9);
        if (!after and f == 0) continue;
        const k248 = (if (after) f else f - 1) * frame_cycles + 248 * line_cycles + 9;
        if (k248 <= machineOf(setup_end)) continue;
        if (machineOf(n - tail_cyc + 1) <= k248) continue;
        break .{ k, d };
    } else return error.BadPosition;
    e.delay(dly);
    const tail_start = e.pc;
    emitTail(&e, tb);
    if (e.err) return error.TooLong;
    std.debug.assert(e.pc - tail_start == tail.pc - code_addr);
    std.debug.assert(machineOf(7 + e.cyc) == end_m);
    return .{ .end = e.pc - 1, .cycles = end_m };
}

/// Step 7: the late writes, then the registers, RTI last.
fn emitTail(e: *Emit, tb: *const [table_size]u8) void {
    e.w(0xD400, tb[t.dmactl]);
    e.sta(0xD01E); // HITCLR
    e.w(0xD20E, tb[t.irqen]);
    if (tb[t.nmist] & 0x60 == 0) e.sta(0xD40F); // NMIRES
    e.w(0xD40E, tb[t.nmien]);
    e.op(&.{ 0xA2, tb[t.s] -% 3 }, 2); // LDX #S-3
    e.op(&.{0x9A}, 2); // TXS
    e.lda(tb[t.a]);
    e.op(&.{ 0xA2, tb[t.x] }, 2);
    e.op(&.{ 0xA0, tb[t.y] }, 2);
    e.op(&.{0x40}, 6); // RTI
}

// --- tests (zig build test) ---------------------------------------------

/// CPU cycles from `from` to RTI's end (at `end`), by running the few
/// instructions the routine uses (X for the delay loops).
fn countCycles(ovl: *const [overlay_size]u8, from: u16, end: u16) !u32 {
    var port_reads: u32 = 0;
    return countReads(ovl, from, end, &port_reads);
}

/// The same, counting the loader port's reads into `port_reads`.
fn countReads(ovl: *const [overlay_size]u8, from: u16, end: u16, port_reads: *u32) !u32 {
    var pc: u16 = from;
    var x: u8 = 0;
    var y: u8 = 0;
    var z = false;
    var n: u32 = 0;
    while (true) {
        const o = ovl[pc % overlay_size];
        switch (o) {
            0xA9 => {
                n += 2;
                pc += 2;
            },
            0xA0 => {
                y = ovl[(pc + 1) % overlay_size];
                n += 2;
                pc += 2;
            },
            0x88 => {
                y -%= 1;
                z = y == 0;
                n += 2;
                pc += 1;
            },
            0xAD => {
                if (ovl[(pc + 1) % overlay_size] == 0xFE and ovl[(pc + 2) % overlay_size] == 0xD5) port_reads.* += 1;
                n += 4;
                pc += 3;
            },
            0xA2 => {
                x = ovl[(pc + 1) % overlay_size];
                n += 2;
                pc += 2;
            },
            0x8D => {
                n += 4;
                pc += 3;
            },
            0xEA, 0x9A => {
                n += 2;
                pc += 1;
            },
            0x24 => {
                n += 3;
                pc += 2;
            },
            0xCA => {
                x -%= 1;
                z = x == 0;
                n += 2;
                pc += 1;
            },
            0xD0 => {
                if (!z) {
                    const next = pc + 2;
                    const to: u16 = @intCast(@as(i32, next) + @as(i8, @bitCast(ovl[(pc + 1) % overlay_size])));
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
    tb[t.cycle] = 100;
    tb[t.nmist] = 0x5F;
    tb[t.dmactl] = 0x22;
    tb[t.nmien] = 0x40;
    return tb;
}

test "the CPU's cycles and the machine's" {
    // the first line: cycles 1-24 the CPU's, 25 a refresh
    try std.testing.expectEqual(@as(u32, 24), cpuCount(24));
    try std.testing.expectEqual(@as(u32, 24), cpuCount(25));
    try std.testing.expectEqual(@as(u32, 26), machineOf(25));
    // a whole line: 105 of 114
    try std.testing.expectEqual(@as(u32, 113 - 9), cpuCount(113));
    try std.testing.expectEqual(@as(u32, 113 - 9 + 105), cpuCount(113 + 114));
    var n: u32 = 1;
    while (n < 3 * 105 * 262) : (n += 37) {
        const m = machineOf(n);
        try std.testing.expect(!refresh(m % line_cycles));
        try std.testing.expectEqual(n, cpuCount(m));
    }
}

test "the delay is exact, any length but 1" {
    var buf: [overlay_size]u8 = undefined;
    for ([_]u32{ 0, 2, 3, 4, 5, 6, 7, 10, 11, 12, 13, 1281, 1282, 1283, 1284, 5000, 29000, 60001 }) |d| {
        for ([_]u16{ code_addr, code_addr + 0xF8, code_addr + 0xFC, code_addr + 0xFE }) |at| {
            @memset(&buf, 0);
            var e = Emit{ .ovl = &buf, .pc = at };
            e.delay(d);
            e.op(&.{0x40}, 6);
            try std.testing.expectEqual(d + 6, try countCycles(&buf, at, e.pc - 1));
        }
    }
}

test "the routine ends on the saved cycle, after line 248" {
    var ovl: [overlay_size]u8 = undefined;
    for ([_][2]u16{ .{ 250, 100 }, .{ 248, 64 }, .{ 261, 113 }, .{ 0, 0 }, .{ 3, 60 }, .{ 7, 113 } }) |pos| {
        var tb = sampleTable();
        tb[t.line] = @truncate(pos[0]);
        tb[t.line + 1] = @truncate(pos[0] >> 8);
        tb[t.cycle] = @truncate(pos[1]);
        const r = try build(&tb, &ovl);
        try std.testing.expectEqual(@as(u32, pos[0]), (r.cycles / line_cycles) % frame_lines);
        try std.testing.expectEqual(@as(u32, pos[1]), r.cycles % line_cycles);
        try std.testing.expectEqual(cpuCount(r.cycles) - 7, try countCycles(&ovl, code_addr, r.end));
        // the reset vector, RTI's pulls
        try std.testing.expectEqual(@as(u8, 0x00), ovl[0x7FC]);
        try std.testing.expectEqual(@as(u8, 0xFA), ovl[0x7FD]);
        try std.testing.expectEqual(tb[t.p], ovl[0x1EE]);
        try std.testing.expectEqual(tb[t.pc], ovl[0x1EF]);
        try std.testing.expectEqual(tb[t.pc + 1], ovl[0x1F0]);
    }
}

test "the loader port is read on to the saved position" {
    var ovl: [overlay_size]u8 = undefined;
    for ([_]u32{ 0, 1, 255, 256, 257, 4000, 40000, 65535 }) |pos| {
        var tb = sampleTable();
        tb[t.xexpos] = @truncate(pos);
        tb[t.xexpos + 1] = @truncate(pos >> 8);
        tb[t.loader] = 3;
        const r = try build(&tb, &ovl);
        var reads: u32 = 0;
        try std.testing.expectEqual(cpuCount(r.cycles) - 7, try countReads(&ovl, code_addr, r.end, &reads));
        try std.testing.expectEqual(pos, reads);
        try std.testing.expectEqual(@as(u32, 250), (r.cycles / line_cycles) % frame_lines);
    }
    var tb = sampleTable();
    tb[t.xexpos + 2] = 1;
    try std.testing.expectError(error.BadLoader, build(&tb, &ovl));
}

test "positions a state isn't taken at are refused" {
    var ovl: [overlay_size]u8 = undefined;
    for ([_][2]u16{ .{ 100, 3 }, .{ 248, 20 }, .{ 250, 29 }, .{ 262, 0 } }) |pos| {
        var tb = sampleTable();
        tb[t.line] = @truncate(pos[0]);
        tb[t.line + 1] = @truncate(pos[0] >> 8);
        tb[t.cycle] = @truncate(pos[1]);
        try std.testing.expectError(error.BadPosition, build(&tb, &ovl));
    }
}

test "the offsets are A800.StateTable's (systems/a800xl/test/golden/state-table.txt)" {
    const golden = try std.Io.Dir.cwd().readFileAlloc(std.testing.io, "systems/a800xl/test/golden/state-table.txt", std.testing.allocator, .limited(1 << 16));
    defer std.testing.allocator.free(golden);
    var it = std.mem.tokenizeScalar(u8, golden, '\n');
    var seen: usize = 0;
    while (it.next()) |line| {
        var f = std.mem.tokenizeScalar(u8, line, ' ');
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

test "PIA: every edge, flag and CB2 level a control write sequence reaches" {
    // what three writes and a last one can reach, the search finds
    var buf: [overlay_size]u8 = undefined;
    const modes = [_]u8{ 0x00, 0x08, 0x10, 0x18, 0x20, 0x28, 0x30, 0x38 };
    for ([_]bool{ false, true }) |pb| {
        for (modes) |a| {
            for (modes) |b| {
                for (modes) |c| {
                    const s = piaStep(pb, piaStep(pb, piaStep(pb, .{}, a), b), c);
                    @memset(&buf, 0);
                    var e = Emit{ .ovl = &buf };
                    const ctl = c | (if (s.flag) @as(u8, 0x40) else 0);
                    piaCtl(&e, pb, ctl, s);
                    // replay the writes the routine got
                    var r = PiaLine{};
                    var pc: u16 = code_addr;
                    while (pc < e.pc) : (pc += 5) r = piaStep(pb, r, buf[(pc + 1) % overlay_size]);
                    try std.testing.expectEqual(s.edge, r.edge);
                    try std.testing.expectEqual(s.flag, r.flag);
                    if (pb) try std.testing.expectEqual(s.cb2, r.cb2);
                }
            }
        }
    }
}
