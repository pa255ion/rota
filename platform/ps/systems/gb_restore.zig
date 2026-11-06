//! The Game Boy's restore routine (side track L, PLAN.md L.6c): loading a
//! state through the machine's own CPU. The PS holds the core (its reset:
//! the post-boot state, the CPU about to fetch 0x0100), writes the
//! memories through the block RAMs' ports, puts the overlay this file
//! builds into the core's ROM overlay and releases the core. Every read in
//! ROM space (0x0000-0x7FFF) then answers the overlay's byte (at the
//! address modulo `overlay_size`) until the read of the address `build`
//! returns; the CPU runs the routine, which writes the state back the way
//! a game would, and its last instruction jumps to the saved PC.
//!
//! The routine is straight-line code, generated per state, so its time is
//! known to the M-cycle:
//!   1. the sound unit (off, wave RAM, on, the registers; a channel that
//!      was on is triggered again with its length timer and envelope
//!      volume: its phase and envelope timer are not restored), serial,
//!      P1, TMA, TAC, the PPU's registers, OAM by OAM DMA from the
//!      overlay's page 0, the MBC (and MBC3's clock: the latched registers,
//!      a latch, the live ones), IE;
//!   2. the Game Boy Color (L.6c.4): in CGB mode VBK, SVBK, RP, FF72 to
//!      FF75, OPRI, HDMA1 to HDMA4 (not HDMA5: no transfer starts), both
//!      palettes (64 bytes each through BCPD and OCPD, then BCPS and OCPS;
//!      neither our core nor the spec blocks them in mode 3), KEY1's armed
//!      bit; in compatibility mode only what it takes (FF72, FF73, FF75,
//!      BCPS, OCPS: the palettes are the PS's, as at the reset);
//!   3. the PPU's dot phase and the CPU's speed: the PPU after the core's
//!      reset runs on from the hand-off (line 153, dot 400), 4 dots an
//!      M-cycle (2 in double speed). The routine tracks it M-cycle for
//!      M-cycle and picks a way there: nothing; the LCD switched off and on
//!      (line 0, dot 3 after the write); a double-speed round trip (KEY1,
//!      STOP: 2050 M-cycles' pause, the PPU at the new speed) with an even
//!      or odd number of M-cycles in it; or both. A double-speed state ends
//!      in double speed. The LCD off needs nothing more;
//!   4. a delay, the DIV write (the system counter from 0), a second
//!      delay: together they put the PPU at the saved line and dot and the
//!      system counter at its saved value when the game resumes;
//!   5. the tail: SP to the overlay's AF bytes, POP AF, BC, DE, TIMA
//!      (less the increments still to come), IF, HL, SP, EI or DI, JP.
//! What it can't put back (approximate by design, the user 2026-10-12):
//! the sound channels' phases and the frame sequencer's step, MBC3's
//! sub-second counter, a serial transfer in progress, the value 0xFF46
//! reads (the overlay's page, 0, after the DMA), IF bits that the PPU sets
//! in the tail's last 12 M-cycles, an HBlank DMA stopped half way (HDMA5's
//! blocks left).
//!
//! The state table's offsets are Gb.StateTable's; the test below checks
//! them against systems/gb/test/golden/state-table.txt, which gb-spec
//! checks against the Haskell side.

const std = @import("std");

pub const table_size = 0x220;
pub const overlay_size = 0x800;

// the state table (Gb.StateTable)
pub const t = struct {
    pub const a = 0x00;
    pub const f = 0x01;
    pub const b = 0x02;
    pub const c = 0x03;
    pub const d = 0x04;
    pub const e = 0x05;
    pub const h = 0x06;
    pub const l = 0x07;
    pub const sp = 0x08;
    pub const pc = 0x0A;
    pub const ime = 0x0C;
    pub const model = 0x0D;
    pub const mbc = 0x0E;
    pub const div = 0x10;
    pub const tima = 0x12;
    pub const tma = 0x13;
    pub const tac = 0x14;
    pub const @"if" = 0x15;
    pub const ie = 0x16;
    pub const sb = 0x17;
    pub const sc = 0x18;
    pub const p1 = 0x19;
    pub const ly = 0x1A;
    pub const dot = 0x1B;
    pub const lcdc = 0x1D;
    pub const stat = 0x1E;
    pub const scy = 0x1F;
    pub const scx = 0x20;
    pub const lyc = 0x21;
    pub const bgp = 0x22;
    pub const obp0 = 0x23;
    pub const obp1 = 0x24;
    pub const wy = 0x25;
    pub const wx = 0x26;
    pub const dma = 0x27;
    pub const romb = 0x28;
    pub const bank2 = 0x2A;
    pub const ramen = 0x2B;
    pub const mode = 0x2C;
    pub const rtc_latch = 0x2D;
    pub const rtc = 0x2E;
    pub const rtc_l = 0x33;
    pub const nr = 0x40;
    pub const nr52 = 0x56;
    pub const ch_on = 0x57;
    pub const ch_len = 0x58;
    pub const ch_vol = 0x60;
    pub const wave = 0x70;
    pub const key1 = 0x80;
    pub const vbk = 0x81;
    pub const svbk = 0x82;
    pub const bcps = 0x83;
    pub const ocps = 0x84;
    pub const opri = 0x85;
    pub const ff72 = 0x86;
    pub const ff73 = 0x87;
    pub const ff74 = 0x88;
    pub const ff75 = 0x89;
    pub const rp = 0x8A;
    pub const hdma_src = 0x8B;
    pub const hdma_dst = 0x8D;
    pub const oam = 0x100;
    pub const pal = 0x1A0;
};

/// Where the routine starts: the core's reset leaves the CPU fetching here.
pub const start = 0x0100;
/// The overlay's page 0 holds OAM (the DMA's source), then the AF bytes.
const af_addr = 0x00A0;
/// The PPU after the reset: line 153, dot 400 (Gb.Spec.Machine's hand-off).
const hand_off = 153 * 456 + 400;
const frame_dots = 154 * 456;
const frame_cycles = frame_dots / 4;
/// The tail's M-cycles (its instructions, the JP's included).
const tail_cycles = 35;

pub const Error = error{
    /// an MBC this routine doesn't know
    UnknownMbc,
    /// the PPU's position can't be reached from the reset (or out of range)
    PpuPhase,
    /// the routine doesn't fit the overlay
    TooLong,
    /// the system counter off its 4-step phase
    DivPhase,
    /// double speed without CGB mode
    Speed,
};

pub const Built = struct {
    /// the address whose read ends the overlay (the JP's last byte)
    end: u16,
    /// M-cycles from the fetch at 0x0100 to the fetch at the saved PC
    cycles: u32,
};

/// The PPU as the routine moves it: on or off, its position in the frame
/// (dots, before the next M-cycle's tick), double speed.
const Ppu = struct {
    on: bool = true,
    pos: u32 = hand_off,
    ds: bool = false,
    fn step(p: Ppu) u32 {
        return if (p.ds) 2 else 4;
    }
};

const Emit = struct {
    ovl: *[overlay_size]u8,
    /// the next byte's address (in the CPU's view)
    pc: u16 = start,
    /// the next M-cycle, counted from the fetch at 0x0100 (0)
    cyc: u32 = 0,
    ppu: Ppu = .{},
    err: bool = false,

    fn byte(self: *Emit, v: u8) void {
        if (self.pc >= overlay_size) {
            self.err = true;
            return;
        }
        self.ovl[self.pc] = v;
        self.pc += 1;
    }
    /// M-cycles going by: the PPU's dots (each M-cycle's tick comes
    /// before its access)
    fn tick(self: *Emit, n: u32) void {
        self.cyc += n;
        if (self.ppu.on) self.ppu.pos = @intCast((@as(u64, self.ppu.pos) + @as(u64, n) * self.ppu.step()) % frame_dots);
    }
    fn op(self: *Emit, bytes: []const u8, cycles: u32) void {
        for (bytes) |v| self.byte(v);
        self.tick(cycles);
    }
    /// LD A,n; LDH (r),A: the write in the last M-cycle (after its tick);
    /// LCDC's bit 7 moves the PPU
    fn ldh(self: *Emit, r: u8, v: u8) void {
        self.op(&.{ 0x3E, v }, 2);
        self.op(&.{ 0xE0, r }, 3);
        if (r == 0x40) {
            const on = v & 0x80 != 0;
            if (self.ppu.on and !on) self.ppu = .{ .on = false, .pos = 0, .ds = self.ppu.ds };
            if (!self.ppu.on and on) self.ppu = .{ .on = true, .pos = 3, .ds = self.ppu.ds };
        }
    }
    /// LD A,n; LD (nn),A
    fn ld(self: *Emit, a: u16, v: u8) void {
        self.op(&.{ 0x3E, v }, 2);
        self.op(&.{ 0xEA, @truncate(a), @truncate(a >> 8) }, 4);
    }
    fn nops(self: *Emit, n: u32) void {
        for (0..n) |_| self.op(&.{0x00}, 1);
    }
    /// Exactly `n` M-cycles (A, BC and F change): LD BC,k then DEC BC;
    /// LD A,B; OR C; JR NZ (7 a turn, 6 the last), 7k + 2; NOPs for the
    /// rest and for anything under 9.
    fn delay(self: *Emit, n: u32) void {
        if (n < 9) return self.nops(n);
        const k = @min((n - 2) / 7, 0xFFFF);
        self.op(&.{ 0x01, @truncate(k), @truncate(k >> 8) }, 3);
        self.op(&.{ 0x0B, 0x78, 0xB1, 0x20, 0xFB }, 7 * k - 1);
        self.nops(n - (7 * k + 2));
    }
    /// The speed switch (CGB mode): KEY1 armed, STOP (its two M-cycles at
    /// the old speed), then 2050 M-cycles' pause at the new one
    /// (Gb.Spec.Machine.stopInstr).
    fn speedSwitch(self: *Emit) void {
        self.ldh(0x4D, 0x01);
        self.op(&.{ 0x10, 0x00 }, 2);
        self.ppu.ds = !self.ppu.ds;
        self.tick(2050);
    }
};

fn get16(tb: *const [table_size]u8, o: usize) u16 {
    return @as(u16, tb[o]) | (@as(u16, tb[o + 1]) << 8);
}

/// TAC's timer input (Gb.Spec.Machine.timerSignal): enabled and the
/// selected system counter bit.
fn timerSignal(tac: u8, c: u16) bool {
    const bit: u4 = switch (tac & 3) {
        0 => 9,
        1 => 3,
        2 => 5,
        else => 7,
    };
    return tac & 4 != 0 and (c >> bit) & 1 == 1;
}

/// A way to the saved dot phase and speed (step 3 of the routine).
const Way = struct {
    round_trip: bool = false,
    /// an odd number of M-cycles in the round trip's double speed
    odd: bool = false,
    to_ds: bool = false,
    /// the LCD off and on at the end
    toggle: bool = false,
    /// ... or inside the round trip (2 dots an M-cycle after it)
    toggle_ds: bool = false,
};

/// The restore routine for a state table, into `ovl`.
pub fn build(tb: *const [table_size]u8, ovl: *[overlay_size]u8) Error!Built {
    const gbc = tb[t.model] & 1 != 0;
    const cgb = gbc and tb[t.model] & 2 != 0;
    const ds = cgb and tb[t.key1] & 0x80 != 0;
    if (tb[t.key1] & 0x80 != 0 and !cgb) return error.Speed;
    const ly: u32 = tb[t.ly];
    const dot: u32 = get16(tb, t.dot);
    if (ly > 153 or dot > 455) return error.PpuPhase;
    const div = get16(tb, t.div);
    if (div % 4 != 0) return error.DivPhase;
    const ways_dmg = [_]Way{ .{}, .{ .toggle = true } };
    const ways_cgb = [_]Way{
        .{},                                        .{ .toggle = true },
        .{ .round_trip = true },                    .{ .round_trip = true, .odd = true },
        .{ .round_trip = true, .toggle = true },    .{ .round_trip = true, .odd = true, .toggle = true },
        .{ .round_trip = true, .toggle_ds = true }, .{ .round_trip = true, .odd = true, .toggle_ds = true },
    };
    const ways_ds = [_]Way{ .{ .to_ds = true }, .{ .to_ds = true, .toggle = true } };
    const ways: []const Way = if (ds) &ways_ds else if (cgb) &ways_cgb else &ways_dmg;
    for (ways) |w| {
        if (try attempt(tb, ovl, w, cgb, gbc, ly * 456 + dot, div)) |r| return r;
    }
    return error.PpuPhase;
}

/// One way: the routine, if it reaches the saved PPU position with it.
fn attempt(tb: *const [table_size]u8, ovl: *[overlay_size]u8, w: Way, cgb: bool, gbc: bool, pos: u32, div: u16) Error!?Built {
    @memset(ovl, 0);
    @memcpy(ovl[0..160], tb[t.oam..][0..160]);
    ovl[af_addr] = tb[t.f] & 0xF0;
    ovl[af_addr + 1] = tb[t.a];
    const lcd_on = tb[t.lcdc] & 0x80 != 0;

    var e = Emit{ .ovl = ovl };
    // the speed first (its pauses are long; DIV is set later anyway)
    if (w.round_trip) {
        e.speedSwitch();
        if (w.toggle_ds) {
            e.ldh(0x40, tb[t.lcdc] & 0x7F);
            e.ldh(0x40, tb[t.lcdc]);
        }
        if (w.odd) e.nops(1);
        e.speedSwitch();
    }
    if (w.to_ds) e.speedSwitch();

    // the sound unit: off (clears its registers), wave RAM, on
    e.ldh(0x26, 0x00);
    for (0..16) |i| e.ldh(@intCast(0x30 + i), tb[t.wave + i]);
    const nr = tb[t.nr..][0..22];
    if (tb[t.nr52] & 0x80 != 0) {
        e.ldh(0x26, 0x80);
        e.ldh(0x24, nr[0x14]);
        e.ldh(0x25, nr[0x15]);
        // each channel: its registers, the length timer by NRx1, the
        // envelope's volume by NRx2 for the trigger, then NRx2 as written
        const ch_regs = [4]u8{ 0x10, 0x15, 0x1A, 0x1F }; // NRx0's place
        for (0..4) |k| {
            const r = ch_regs[k];
            const on = (tb[t.ch_on] >> @intCast(k)) & 1 != 0;
            const full: u16 = if (k == 2) 256 else 64;
            const l: u16 = get16(tb, t.ch_len + 2 * k);
            const lv: u8 = @truncate((full -% l) & (full - 1));
            if (k == 0 or k == 2) e.ldh(r, nr[r - 0x10]); // NR10, NR30 (no NR20, NR40)
            const r1 = r + 1;
            const nrx1 = nr[r1 - 0x10];
            e.ldh(r1, if (k == 2) lv else (nrx1 & 0xC0) | lv);
            const r2 = r + 2;
            // (the volume only while the DAC stays on: a channel whose
            // envelope ran down to 0 keeps NRx2's own)
            const with_vol = (tb[t.ch_vol + k] << 4) | (nr[r2 - 0x10] & 0x0F);
            if (k == 2) e.ldh(r2, nr[r2 - 0x10]) else e.ldh(r2, if (with_vol & 0xF8 != 0) with_vol else nr[r2 - 0x10]);
            e.ldh(r + 3, nr[r + 3 - 0x10]);
            e.ldh(r + 4, (nr[r + 4 - 0x10] & 0x47) | @as(u8, if (on) 0x80 else 0));
            if (k != 2) e.ldh(r2, nr[r2 - 0x10]);
        }
    } else {
        // off: the DMG still takes the length timers
        for ([4]u8{ 0x11, 0x16, 0x1B, 0x20 }, 0..) |r, k| {
            const full: u16 = if (k == 2) 256 else 64;
            e.ldh(r, @truncate((full -% get16(tb, t.ch_len + 2 * k)) & (full - 1)));
        }
    }
    // serial, P1, the timer's TMA and TAC
    e.ldh(0x01, tb[t.sb]);
    e.ldh(0x02, tb[t.sc]);
    e.ldh(0x00, tb[t.p1]);
    e.ldh(0x06, tb[t.tma]);
    e.ldh(0x07, tb[t.tac]);
    // the PPU's registers; OAM by DMA from the overlay's page 0 (162
    // M-cycles: it starts two after the write)
    for ([_][2]u8{ .{ 0x42, tb[t.scy] }, .{ 0x43, tb[t.scx] }, .{ 0x45, tb[t.lyc] }, .{ 0x47, tb[t.bgp] }, .{ 0x48, tb[t.obp0] }, .{ 0x49, tb[t.obp1] }, .{ 0x4A, tb[t.wy] }, .{ 0x4B, tb[t.wx] }, .{ 0x41, tb[t.stat] } }) |r| e.ldh(r[0], r[1]);
    e.ldh(0x46, 0x00);
    e.delay(162);
    // the Game Boy Color's registers and palettes
    if (cgb) {
        e.ldh(0x4F, tb[t.vbk]);
        e.ldh(0x70, tb[t.svbk]);
        e.ldh(0x56, tb[t.rp]);
        e.ldh(0x74, tb[t.ff74]);
        e.ldh(0x6C, tb[t.opri]);
        const src = get16(tb, t.hdma_src);
        const dst = get16(tb, t.hdma_dst);
        e.ldh(0x51, @truncate(src >> 8));
        e.ldh(0x52, @truncate(src));
        e.ldh(0x53, @truncate(dst >> 8));
        e.ldh(0x54, @truncate(dst));
        e.ldh(0x68, 0x80);
        for (0..64) |i| e.ldh(0x69, tb[t.pal + i]);
        e.ldh(0x6A, 0x80);
        for (0..64) |i| e.ldh(0x6B, tb[t.pal + 64 + i]);
    }
    if (gbc) {
        e.ldh(0x72, tb[t.ff72]);
        e.ldh(0x73, tb[t.ff73]);
        e.ldh(0x75, tb[t.ff75]);
        e.ldh(0x68, tb[t.bcps]);
        e.ldh(0x6A, tb[t.ocps]);
    }
    // the MBC
    const ramen: u8 = if (tb[t.ramen] != 0) 0x0A else 0x00;
    const romb = get16(tb, t.romb);
    switch (tb[t.mbc]) {
        0 => {},
        1 => {
            e.ld(0x6000, tb[t.mode]);
            e.ld(0x4000, tb[t.bank2]);
            e.ld(0x2000, @truncate(romb));
            e.ld(0x0000, ramen);
        },
        2 => {
            e.ld(0x0100, @truncate(romb));
            e.ld(0x0000, ramen);
        },
        3, 4 => {
            if (tb[t.mbc] == 4) {
                // the latched registers, latched from the live ones, then
                // the live ones (writing seconds restarts the second)
                e.ld(0x0000, 0x0A);
                for (0..5) |r| {
                    e.ld(0x4000, @intCast(8 + r));
                    e.ld(0xA000, tb[t.rtc_l + r]);
                }
                e.ld(0x6000, 0x00);
                e.ld(0x6000, 0x01);
                for (0..5) |r| {
                    e.ld(0x4000, @intCast(8 + r));
                    e.ld(0xA000, tb[t.rtc + r]);
                }
            }
            e.ld(0x6000, tb[t.rtc_latch]);
            e.ld(0x2000, @truncate(romb));
            e.ld(0x4000, tb[t.bank2]);
            e.ld(0x0000, ramen);
        },
        5 => {
            e.ld(0x2000, @truncate(romb));
            e.ld(0x3000, @truncate(romb >> 8));
            e.ld(0x4000, tb[t.bank2]);
            e.ld(0x0000, ramen);
        },
        else => return error.UnknownMbc,
    }
    e.ldh(0xFF, tb[t.ie]);
    if (cgb) e.ldh(0x4D, tb[t.key1] & 0x01);
    // LCDC last: off and on again for the other dot phases
    if (w.toggle) e.ldh(0x40, tb[t.lcdc] & 0x7F);
    e.ldh(0x40, tb[t.lcdc]);
    const s0 = e.ppu;

    // the delays. After the DIV write (in M-cycle w) the system counter is
    // 4 × (the M-cycles since), so B + tail_cycles = DIV / 4; the PPU
    // (step dots an M-cycle) must reach the saved dot when the game
    // resumes, A + 3 + B + tail_cycles M-cycles from here
    const b_cyc: u32 = @intCast(@mod(@as(i64, div / 4) - tail_cycles, 16384));
    var a_cyc: u32 = 0;
    if (lcd_on) {
        if (!s0.on) return null;
        const st = s0.step();
        const gap: u32 = @intCast(@mod(@as(i64, pos) - @as(i64, s0.pos), frame_dots));
        if (gap % st != 0) return null;
        const cycles_per_frame: i64 = frame_dots / st;
        a_cyc = @intCast(@mod(@as(i64, gap / st) - 3 - @as(i64, b_cyc) - tail_cycles, cycles_per_frame));
    } else if (s0.on) return null;
    e.delay(a_cyc);
    e.op(&.{ 0xE0, 0x04 }, 3); // DIV: the system counter from 0
    e.delay(b_cyc);

    // the tail (tail_cycles M-cycles)
    const tail0 = e.cyc;
    const f = tail0 + tail_cycles;
    // TIMA: written in M-cycle tail0 + 17; the increments in the M-cycles
    // after it, up to the fetch, are still to come
    var inc: u8 = 0;
    {
        var k: u32 = tail0 + 18;
        while (k < f) : (k += 1) {
            const c: u16 = div -% @as(u16, @truncate(4 * (f - k)));
            if (timerSignal(tb[t.tac], c) and !timerSignal(tb[t.tac], c +% 4)) inc += 1;
        }
    }
    const tima = if (tb[t.tima] >= inc) tb[t.tima] - inc else tb[t.tima];
    e.op(&.{ 0x31, @truncate(af_addr), af_addr >> 8 }, 3); // LD SP,af
    e.op(&.{0xF1}, 3); // POP AF
    e.op(&.{ 0x01, tb[t.c], tb[t.b] }, 3);
    e.op(&.{ 0x11, tb[t.e], tb[t.d] }, 3);
    e.op(&.{ 0x21, 0x05, 0xFF }, 3);
    e.op(&.{ 0x36, tima }, 3); // TIMA in M-cycle tail0 + 17
    e.op(&.{ 0x21, 0x0F, 0xFF }, 3);
    e.op(&.{ 0x36, tb[t.@"if"] & 0x1F }, 3);
    e.op(&.{ 0x21, tb[t.l], tb[t.h] }, 3);
    e.op(&.{ 0x31, tb[t.sp], tb[t.sp + 1] }, 3);
    e.op(&.{if (tb[t.ime] != 0) 0xFB else 0xF3}, 1);
    e.op(&.{ 0xC3, tb[t.pc], tb[t.pc + 1] }, 4);
    if (e.err) return error.TooLong;
    std.debug.assert(e.cyc == f);
    if (lcd_on) std.debug.assert(e.ppu.pos == pos);
    return .{ .end = e.pc - 1, .cycles = f };
}

// --- tests (zig build test) ---------------------------------------------

/// M-cycles of the routine's code from `start` to the JP's end, by
/// running the few instructions it uses (BC for the delay loops).
fn countCycles(ovl: *const [overlay_size]u8, end: u16) !u32 {
    var pc: u16 = start;
    var bc: u16 = 0;
    var z = false;
    var n: u32 = 0;
    while (true) {
        const o = ovl[pc];
        switch (o) {
            0x00, 0x78, 0xF3, 0xFB => { // NOP, LD A,B, DI, EI
                n += 1;
                pc += 1;
            },
            0xB1 => { // OR C
                z = ((bc >> 8) | (bc & 0xFF)) == 0;
                n += 1;
                pc += 1;
            },
            0x0B => { // DEC BC
                bc -%= 1;
                n += 2;
                pc += 1;
            },
            0x3E => { // LD A,n
                n += 2;
                pc += 2;
            },
            0xE0, 0x36 => { // LDH (n),A; LD (HL),n
                n += 3;
                pc += 2;
            },
            0x20 => { // JR NZ,e
                if (!z) {
                    n += 3;
                    pc = @intCast(@as(i32, pc) + 2 + @as(i8, @bitCast(ovl[pc + 1])));
                } else {
                    n += 2;
                    pc += 2;
                }
            },
            0x01 => { // LD BC,nn
                bc = @as(u16, ovl[pc + 1]) | (@as(u16, ovl[pc + 2]) << 8);
                n += 3;
                pc += 3;
            },
            0x11, 0x21, 0x31 => {
                n += 3;
                pc += 3;
            },
            0xF1 => {
                n += 3;
                pc += 1;
            },
            0xEA => {
                n += 4;
                pc += 3;
            },
            0x10 => { // STOP: the speed switch's pause after it
                n += 2 + 2050;
                pc += 2;
            },
            0xC3 => {
                if (pc + 2 != end) return error.EndElsewhere;
                return n + 4;
            },
            else => return error.UnknownOpcode,
        }
    }
}

fn sampleTable() [table_size]u8 {
    var tb = [_]u8{0} ** table_size;
    tb[t.a] = 0x12;
    tb[t.f] = 0xB0;
    tb[t.sp] = 0xFE;
    tb[t.sp + 1] = 0xDF;
    tb[t.pc] = 0x50;
    tb[t.pc + 1] = 0x01;
    tb[t.mbc] = 1;
    tb[t.ly] = 144;
    tb[t.dot] = 8;
    tb[t.lcdc] = 0x91;
    tb[t.div] = 0x00;
    tb[t.div + 1] = 0xAB;
    tb[t.tac] = 0x05;
    tb[t.tima] = 0x80;
    tb[t.nr52] = 0x80;
    tb[t.ch_on] = 0x01;
    tb[t.ch_len] = 10;
    tb[t.ch_len + 4] = 0;
    tb[t.ch_len + 5] = 1;
    return tb;
}

test "the routine's M-cycles are what build says, for both LCD paths and the LCD off" {
    var ovl: [overlay_size]u8 = undefined;
    var tb = sampleTable();
    for ([_][3]u16{ .{ 144, 8, 0x91 }, .{ 150, 403, 0x91 }, .{ 0, 0, 0x11 }, .{ 153, 400, 0x80 } }) |c| {
        tb[t.ly] = @intCast(c[0]);
        tb[t.dot] = @truncate(c[1]);
        tb[t.dot + 1] = @intCast(c[1] >> 8);
        tb[t.lcdc] = @intCast(c[2]);
        const r = try build(&tb, &ovl);
        try std.testing.expectEqual(r.cycles, try countCycles(&ovl, r.end));
        try std.testing.expectEqualSlices(u8, tb[t.oam..][0..160], ovl[0..160]);
    }
}

test "delays are exact for every length" {
    var ovl: [overlay_size]u8 = undefined;
    for (0..2000) |n| {
        @memset(&ovl, 0);
        var e = Emit{ .ovl = &ovl };
        e.delay(@intCast(n));
        e.op(&.{ 0xC3, 0, 0 }, 4);
        try std.testing.expectEqual(e.cyc, try countCycles(&ovl, e.pc - 1));
    }
}

test "the DMG: dot phases 1 and 2 are out of reach; double speed is refused" {
    var ovl: [overlay_size]u8 = undefined;
    var tb = sampleTable();
    tb[t.dot] = 9;
    try std.testing.expectError(error.PpuPhase, build(&tb, &ovl));
    tb[t.dot] = 10;
    try std.testing.expectError(error.PpuPhase, build(&tb, &ovl));
    tb = sampleTable();
    tb[t.key1] = 0x80;
    try std.testing.expectError(error.Speed, build(&tb, &ovl));
}

test "the GBC in CGB mode: every dot phase, both speeds, the cycles as counted" {
    var ovl: [overlay_size]u8 = undefined;
    var tb = sampleTable();
    tb[t.model] = 3;
    for (tb[t.pal..][0..128], 0..) |*b, i| b.* = @truncate(i * 3 + 1);
    for ([_]u8{ 0x00, 0x80 }) |key1| {
        tb[t.key1] = key1;
        for (0..4) |ph| {
            tb[t.dot] = @intCast(56 + ph);
            const r = try build(&tb, &ovl);
            try std.testing.expectEqual(r.cycles, try countCycles(&ovl, r.end));
        }
    }
}

test "the GBC in compatibility mode: as the DMG (phases 0 and 3)" {
    var ovl: [overlay_size]u8 = undefined;
    var tb = sampleTable();
    tb[t.model] = 1;
    tb[t.dot] = 56;
    _ = try build(&tb, &ovl);
    tb[t.dot] = 59;
    _ = try build(&tb, &ovl);
    tb[t.dot] = 57;
    try std.testing.expectError(error.PpuPhase, build(&tb, &ovl));
}

test "the offsets are Gb.StateTable's (systems/gb/test/golden/state-table.txt)" {
    const golden = try std.Io.Dir.cwd().readFileAlloc(std.testing.io, "systems/gb/test/golden/state-table.txt", std.testing.allocator, .limited(1 << 16));
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
            var dl: [16]u8 = undefined;
            const dn = std.mem.replaceOwned(u8, std.testing.allocator, d.name, "_", "") catch unreachable;
            defer std.testing.allocator.free(dn);
            if (std.mem.eql(u8, std.ascii.lowerString(&dl, dn), low)) {
                try std.testing.expectEqual(off, @as(usize, @field(t, d.name)));
                seen += 1;
            }
        }
    }
    try std.testing.expectEqual(@typeInfo(t).@"struct".decls.len, seen);
}
