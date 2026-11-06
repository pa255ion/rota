//! The Game Gear's and Master System's restore routine (side track N,
//! PLAN.md N.1): loading a state through the machine's own Z80, as the
//! Game Boy's (gb_restore.zig). The PS holds the core (its reset: the Z80
//! about to fetch 0x0000, IFF 0, IM 0, T 0), writes RAM, VRAM and
//! cartridge RAM through its windows, puts the overlay this file builds
//! into the core's ROM overlay and lets the core go. Every memory read
//! below 0xC000 then answers the overlay's byte (the address modulo
//! `overlay_size`) until the read of the address `build` returns, the
//! routine's JP; the next fetch is the game's.
//!
//! The routine is straight-line code, generated per state, its T-states
//! and R's count known exactly:
//!   1. the VDP: R0-R10, CRAM (64 bytes on the Game Gear, 32 converted
//!      back to the Master System's 6-bit colours), the Game Gear's CRAM
//!      latch;
//!   2. the PSG: the tone periods, the volumes, the noise control (it
//!      resets the LFSR: approximate), then the latched register;
//!   3. the Game Gear's ports 01h-06h, or in Master System mode 3Fh and
//!      (last) 3Eh;
//!   4. the mapper (Sega: FFFCh-FFFFh, then RAM's DFFCh-DFFFh written
//!      back; Codemasters: 0000h, 4000h, 8000h);
//!   5. a delay that puts the T-states since power-on at their saved value
//!      modulo two frames (`t.time`): the VDP's position and the PSG's
//!      clock follow from it. At least the rest of a frame long: the VDP
//!      draws the game's frame whole before it resumes (its sprite flags);
//!   6. the tail: the VDP's status read (if no flag was set: it clears
//!      them), its address, code and control latch; AF', BC', DE', HL',
//!      IX, IY, I, IM, BC, DE, HL, R (so that it counts on to its saved
//!      value), AF, SP, EI or DI, JP. A halted CPU (its PC past the HALT)
//!      gets JP PC - 1: the game's own HALT runs again, its fetch the 4
//!      T-states and the one R count the alignment leaves for it.
//! What it can't put back (approximate by design, the user 2026-10-14):
//! the PSG's counters, outputs and LFSR; the VDP's read buffer in a write
//! mode, its H latch, a pending line interrupt, and the status's sprite
//! overflow and collision flags when no frame of the saved state sets them
//! (they stay set until read, from whichever frame); MEMPTR (WZ); the FM
//! unit (not yet restored at all: N.1 leaves it as the reset left it).
//!
//! The state table's offsets are Gg.StateTable's; the test below checks
//! them against systems/gg/test/golden/state-table.txt.

const std = @import("std");

pub const table_size = 0x140;
pub const overlay_size = 0x800;

/// T-states the time counts modulo: two frames of 262 lines of 228.
pub const t_phase: u32 = 2 * frame_t;
const frame_t: u32 = 228 * 262;

pub const t = struct {
    pub const a = 0x00;
    pub const f = 0x01;
    pub const b = 0x02;
    pub const c = 0x03;
    pub const d = 0x04;
    pub const e = 0x05;
    pub const h = 0x06;
    pub const l = 0x07;
    pub const af2 = 0x08;
    pub const bc2 = 0x0A;
    pub const de2 = 0x0C;
    pub const hl2 = 0x0E;
    pub const ix = 0x10;
    pub const iy = 0x12;
    pub const sp = 0x14;
    pub const pc = 0x16;
    pub const i = 0x18;
    pub const r = 0x19;
    pub const iff1 = 0x1A;
    pub const iff2 = 0x1B;
    pub const im = 0x1C;
    pub const flags = 0x1D;
    pub const wz = 0x1E;
    pub const time = 0x20;
    pub const halted = 0x23;
    pub const map = 0x24;
    pub const cmram = 0x28;
    pub const memctl = 0x29;
    pub const ioctl = 0x2A;
    pub const ramtail = 0x2C;
    pub const ggregs = 0x30;
    pub const biosmap = 0x38;
    pub const vdpregs = 0x40;
    pub const vdpaddr = 0x4B;
    pub const vdpcode = 0x4D;
    pub const vdplatched = 0x4E;
    pub const vdplatch = 0x4F;
    pub const vdpbuf = 0x50;
    pub const vdpcramlatch = 0x51;
    pub const vdpstatus = 0x52;
    pub const vdplinepend = 0x53;
    pub const vdplinecnt = 0x54;
    pub const vdphlatch = 0x55;
    pub const psgperiod = 0x60;
    pub const psgnoise = 0x66;
    pub const psgvol = 0x67;
    pub const psglatch = 0x6B;
    pub const psglfsr = 0x6C;
    pub const fmctl = 0x70;
    pub const fmregs = 0x80;
    pub const cram = 0x100;
};

/// The flags byte (`t.flags`).
const flag_sms = 0x01;
const flag_codemasters = 0x04;

/// Where POP AF finds AF' and AF (above the code, under the overlay's end).
const af_addr: u16 = 0x07F0;

pub const Error = error{
    /// the routine doesn't fit the overlay
    TooLong,
    /// a time phase out of range
    BadTime,
};

pub const Built = struct {
    /// the address whose read ends the overlay (the JP's last byte)
    end: u16,
    /// T-states from the reset's first fetch to the game's first fetch
    tstates: u32,
};

const Emit = struct {
    ovl: *[overlay_size]u8,
    pc: u16 = 0,
    /// T-states so far
    ts: u32 = 0,
    /// opcode fetches (M1) so far: R counts them
    m1: u32 = 0,
    err: bool = false,

    fn byte(self: *Emit, v: u8) void {
        if (self.pc >= af_addr) {
            self.err = true;
            return;
        }
        self.ovl[self.pc] = v;
        self.pc += 1;
    }
    fn op(self: *Emit, bytes: []const u8, ts: u32, m1: u32) void {
        for (bytes) |v| self.byte(v);
        self.ts += ts;
        self.m1 += m1;
    }
    fn ldA(self: *Emit, v: u8) void {
        self.op(&.{ 0x3E, v }, 7, 1);
    }
    fn out(self: *Emit, port: u8, v: u8) void {
        self.ldA(v);
        self.op(&.{ 0xD3, port }, 11, 1);
    }
    fn st(self: *Emit, a: u16, v: u8) void {
        self.ldA(v);
        self.op(&.{ 0x32, @truncate(a), @truncate(a >> 8) }, 13, 1);
    }
    fn ld16(self: *Emit, opcode: u8, v: u16) void {
        self.op(&.{ opcode, @truncate(v), @truncate(v >> 8) }, 10, 1);
    }
    /// Exactly `n` T-states (A, BC and F change), `n` from 18 up or one of
    /// the sums of 4s and 7s below it: NOPs (4) and LD A,n (7) for the
    /// rest; LD BC,k and a 26-T loop (21 the last turn) for the bulk.
    fn delay(self: *Emit, n: u32) void {
        var left = n;
        if (left >= 18 + 26 + 5) {
            const k: u32 = @min((left - 5 - 18) / 26, 0xFFFF);
            self.op(&.{ 0x01, @truncate(k), @truncate(k >> 8) }, 10, 1);
            self.op(&.{ 0x0B, 0x78, 0xB1, 0x20, 0xFB }, 26 * k - 5, 4 * k);
            left -= 26 * k + 5;
        }
        // 4a + 7b = left
        var b7: u32 = 0;
        while (b7 * 7 <= left and (left - b7 * 7) % 4 != 0) b7 += 1;
        std.debug.assert(b7 * 7 <= left);
        for (0..b7) |_| self.ldA(0);
        for (0..(left - b7 * 7) / 4) |_| self.op(&.{0x00}, 4, 1);
    }
};

/// Whether `n` T-states can be spent exactly by `Emit.delay`.
fn delayable(n: u32) bool {
    if (n >= 18) return true;
    return switch (n) {
        0, 4, 7, 8, 11, 12, 14, 15, 16 => true,
        else => false,
    };
}

fn get16(tb: *const [table_size]u8, o: usize) u16 {
    return @as(u16, tb[o]) | (@as(u16, tb[o + 1]) << 8);
}

/// The Master System's 6-bit colour back from CRAM's two bytes (each
/// 2-bit channel was kept times 5: Gg.Spec.Vdp).
fn smsColour(lo: u8, hi: u8) u8 {
    return ((lo & 0x0F) / 5) | (((lo >> 4) / 5) << 2) | ((hi & 0x0F) / 5) << 4;
}

/// The restore routine for a state table, into `ovl`.
pub fn build(tb: *const [table_size]u8, ovl: *[overlay_size]u8) Error!Built {
    const sms = tb[t.flags] & flag_sms != 0;
    const codemasters = tb[t.flags] & flag_codemasters != 0;
    const saved_t: u32 = @as(u32, tb[t.time]) | (@as(u32, tb[t.time + 1]) << 8) | (@as(u32, tb[t.time + 2]) << 16);
    if (saved_t >= t_phase) return error.BadTime;
    @memset(ovl, 0);
    ovl[af_addr] = tb[t.af2];
    ovl[af_addr + 1] = tb[t.af2 + 1];
    ovl[af_addr + 2] = tb[t.f];
    ovl[af_addr + 3] = tb[t.a];

    var e = Emit{ .ovl = ovl };
    // the VDP's registers: value, then 80h + the register
    for (0..11) |k| {
        e.out(0xBF, tb[t.vdpregs + k]);
        e.out(0xBF, 0x80 | @as(u8, @intCast(k)));
    }
    // CRAM from 0 (code 3)
    e.out(0xBF, 0x00);
    e.out(0xBF, 0xC0);
    if (sms) {
        for (0..32) |k| e.out(0xBE, smsColour(tb[t.cram + 2 * k], tb[t.cram + 2 * k + 1]));
    } else {
        for (0..64) |k| e.out(0xBE, tb[t.cram + k]);
        // the even byte latched, as saved (an even write changes nothing else)
        e.out(0xBF, 0x00);
        e.out(0xBF, 0xC0);
        e.out(0xBE, tb[t.vdpcramlatch]);
    }
    // the PSG (port 7Fh): periods, volumes, noise, then the latched register
    for (0..3) |c| {
        const p = get16(tb, t.psgperiod + 2 * c);
        e.out(0x7F, 0x80 | (@as(u8, @intCast(2 * c)) << 4) | @as(u8, @truncate(p & 0x0F)));
        e.out(0x7F, @truncate((p >> 4) & 0x3F));
    }
    for (0..4) |c| e.out(0x7F, 0x80 | (@as(u8, @intCast(2 * c + 1)) << 4) | (tb[t.psgvol + c] & 0x0F));
    e.out(0x7F, 0xE0 | (tb[t.psgnoise] & 0x07));
    {
        const lr = tb[t.psglatch] & 7;
        const low: u8 = if (lr % 2 == 1) tb[t.psgvol + lr / 2] & 0x0F else if (lr == 6) tb[t.psgnoise] & 0x07 else @truncate(get16(tb, t.psgperiod + lr) & 0x0F);
        e.out(0x7F, 0x80 | (lr << 4) | low);
    }
    // the Game Gear's ports, or the Master System's controls
    if (sms) {
        e.out(0x3F, tb[t.ioctl]);
    } else {
        for (1..7) |k| e.out(@intCast(k), tb[t.ggregs + k]);
    }
    // the mapper
    if (codemasters) {
        e.st(0x0000, tb[t.map + 1]);
        e.st(0x4000, tb[t.map + 2]);
        if (tb[t.cmram] != 0) e.st(0x4000, 0x80);
        e.st(0x8000, tb[t.map + 3]);
    } else {
        for (0..4) |k| e.st(@intCast(0xFFFC + k), tb[t.map + k]);
        for (0..4) |k| e.st(@intCast(0xDFFC + k), tb[t.ramtail + k]);
    }
    if (sms) e.out(0x3E, tb[t.memctl]);

    // the tail's code into a scratch copy first, for its length in T
    // (it doesn't depend on where it goes)
    var tail_buf: [overlay_size]u8 = undefined;
    var tail = Emit{ .ovl = &tail_buf };
    emitTail(&tail, tb);
    const tail_ts = tail.ts;

    // the delay: the T-states at the game's first fetch = the saved ones
    // (mod two frames)
    const halt_t: i64 = if (tb[t.halted] != 0) 4 else 0;
    var dly: u32 = @intCast(@mod(@as(i64, saved_t) - halt_t - @as(i64, e.ts) - @as(i64, tail_ts), t_phase));
    // the frame the game resumes in must start after the setup: the VDP
    // then draws it whole with the restored registers and sprites, and
    // sets the sprite overflow and collision flags as the game's frame did
    while (!delayable(dly) or ((e.ts + dly + tail_ts) / frame_t) * frame_t < e.ts) dly += t_phase;
    e.delay(dly);
    const tail_start = e.pc;
    emitTail(&e, tb);
    if (e.err) return error.TooLong;
    std.debug.assert(e.pc - tail_start == tail.pc);
    std.debug.assert(@as(u32, @intCast(@mod(@as(i64, e.ts) + halt_t, t_phase))) == saved_t);
    return .{ .end = e.pc - 1, .tstates = e.ts };
}

/// Step 6: the VDP's port state, then the registers, the JP last.
fn emitTail(e: *Emit, tb: *const [table_size]u8) void {
    // the status read clears the flags, the line interrupt and the latch
    if (tb[t.vdpstatus] & 0xE0 == 0) e.op(&.{ 0xDB, 0xBF }, 11, 1);
    const addr = get16(tb, t.vdpaddr) & 0x3FFF;
    const code = tb[t.vdpcode] & 3;
    if (code == 0) {
        // read mode: the address before, which prefetches the buffer
        const a0 = (addr -% 1) & 0x3FFF;
        e.out(0xBF, @truncate(a0));
        e.out(0xBF, @truncate(a0 >> 8));
    } else {
        e.out(0xBF, @truncate(addr));
        e.out(0xBF, @as(u8, @truncate(addr >> 8)) | (code << 6));
    }
    if (tb[t.vdplatched] != 0) e.out(0xBF, tb[t.vdplatch]);
    // the registers
    e.ld16(0x31, af_addr); // LD SP,af
    e.op(&.{0xF1}, 10, 1); // POP AF: AF'
    e.op(&.{0x08}, 4, 1); // EX AF,AF'
    e.ld16(0x01, get16(tb, t.bc2));
    e.ld16(0x11, get16(tb, t.de2));
    e.ld16(0x21, get16(tb, t.hl2));
    e.op(&.{0xD9}, 4, 1); // EXX
    const ix = get16(tb, t.ix);
    const iy = get16(tb, t.iy);
    e.op(&.{ 0xDD, 0x21, @truncate(ix), @truncate(ix >> 8) }, 14, 2);
    e.op(&.{ 0xFD, 0x21, @truncate(iy), @truncate(iy >> 8) }, 14, 2);
    e.ldA(tb[t.i]);
    e.op(&.{ 0xED, 0x47 }, 9, 2); // LD I,A
    e.op(&.{ 0xED, switch (tb[t.im] & 3) {
        1 => 0x56,
        2 => 0x5E,
        else => 0x46,
    } }, 8, 2);
    e.ld16(0x01, @as(u16, tb[t.c]) | (@as(u16, tb[t.b]) << 8));
    e.ld16(0x11, @as(u16, tb[t.e]) | (@as(u16, tb[t.d]) << 8));
    e.ld16(0x21, @as(u16, tb[t.l]) | (@as(u16, tb[t.h]) << 8));
    // R: after LD R,A four more fetches (POP AF, LD SP, EI/DI, JP; and the
    // HALT's) count its low 7 bits on
    const halted = tb[t.halted] != 0;
    const rs = tb[t.r];
    e.ldA((rs & 0x80) | ((rs -% @as(u8, if (halted) 5 else 4)) & 0x7F));
    e.op(&.{ 0xED, 0x4F }, 9, 2); // LD R,A
    e.op(&.{0xF1}, 10, 1); // POP AF
    e.ld16(0x31, get16(tb, t.sp));
    e.op(&.{if (tb[t.iff1] != 0) 0xFB else 0xF3}, 4, 1);
    const target = if (halted) get16(tb, t.pc) -% 1 else get16(tb, t.pc);
    e.op(&.{ 0xC3, @truncate(target), @truncate(target >> 8) }, 10, 1);
}

// --- tests (zig build test) ---------------------------------------------

/// T-states of the routine from 0 to the JP's end, by running the few
/// instructions it uses (BC for the delay loop).
fn countT(ovl: *const [overlay_size]u8, end: u16) !u32 {
    var pc: u16 = 0;
    var bc: u16 = 0;
    var z = false;
    var n: u32 = 0;
    while (true) {
        const o = ovl[pc];
        switch (o) {
            0x00, 0x08, 0xD9, 0x78, 0xF3, 0xFB => {
                n += 4;
                pc += 1;
            },
            0xB1 => {
                z = ((bc >> 8) | (bc & 0xFF)) == 0;
                n += 4;
                pc += 1;
            },
            0x0B => {
                bc -%= 1;
                n += 6;
                pc += 1;
            },
            0x3E => {
                n += 7;
                pc += 2;
            },
            0xD3, 0xDB => {
                n += 11;
                pc += 2;
            },
            0x20 => {
                if (!z) {
                    n += 12;
                    pc = @intCast(@as(i32, pc) + 2 + @as(i8, @bitCast(ovl[pc + 1])));
                } else {
                    n += 7;
                    pc += 2;
                }
            },
            0x01 => {
                bc = @as(u16, ovl[pc + 1]) | (@as(u16, ovl[pc + 2]) << 8);
                n += 10;
                pc += 3;
            },
            0x11, 0x21, 0x31 => {
                n += 10;
                pc += 3;
            },
            0xF1 => {
                n += 10;
                pc += 1;
            },
            0x32 => {
                n += 13;
                pc += 3;
            },
            0xDD, 0xFD => {
                n += 14;
                pc += 4;
            },
            0xED => {
                n += if (ovl[pc + 1] == 0x47 or ovl[pc + 1] == 0x4F) 9 else 8;
                pc += 2;
            },
            0xC3 => {
                if (pc + 2 != end) return error.EndElsewhere;
                return n + 10;
            },
            else => return error.UnknownOpcode,
        }
    }
}

fn sampleTable() [table_size]u8 {
    var tb = [_]u8{0} ** table_size;
    tb[t.a] = 0x12;
    tb[t.f] = 0x44;
    tb[t.sp] = 0xF0;
    tb[t.sp + 1] = 0xDF;
    tb[t.pc] = 0x34;
    tb[t.pc + 1] = 0x12;
    tb[t.r] = 0x85;
    tb[t.im] = 1;
    tb[t.iff1] = 1;
    tb[t.iff2] = 1;
    for (0..11) |k| tb[t.vdpregs + k] = @intCast(k * 3);
    for (0..64) |k| tb[t.cram + k] = @intCast(k);
    tb[t.time] = 0x34;
    tb[t.time + 1] = 0x12;
    tb[t.time + 2] = 0x01;
    return tb;
}

test "the routine's T-states are what build says, on the Game Gear and the Master System, both mappers" {
    var ovl: [overlay_size]u8 = undefined;
    var tb = sampleTable();
    for ([_]u8{ 0, flag_sms, flag_codemasters, flag_sms | flag_codemasters }) |fl| {
        for ([_]u8{ 0, 1, 2 }) |code| {
            tb[t.flags] = fl;
            tb[t.vdpcode] = code;
            tb[t.vdpstatus] = if (code == 1) 0x80 else 0;
            tb[t.vdplatched] = code & 1;
            const r = try build(&tb, &ovl);
            try std.testing.expectEqual(r.tstates, try countT(&ovl, r.end));
            try std.testing.expectEqual(@as(u32, 0x11234), r.tstates % t_phase);
        }
    }
}

test "delays are exact for every length they take" {
    var ovl: [overlay_size]u8 = undefined;
    for (0..3000) |n| {
        if (!delayable(@intCast(n))) continue;
        @memset(&ovl, 0);
        var e = Emit{ .ovl = &ovl };
        e.delay(@intCast(n));
        e.op(&.{ 0xC3, 0, 0 }, 10, 1);
        try std.testing.expectEqual(e.ts, try countT(&ovl, e.pc - 1));
    }
}

test "the Master System's colours come back from CRAM's conversion" {
    // --BBGGRR 0x1B: R 3, G 2, B 1 -> lo = 15 | 10 << 4, hi = 5
    try std.testing.expectEqual(@as(u8, 0x1B), smsColour(15 | (10 << 4), 5));
}

test "the offsets are Gg.StateTable's (systems/gg/test/golden/state-table.txt)" {
    const golden = try std.Io.Dir.cwd().readFileAlloc(std.testing.io, "systems/gg/test/golden/state-table.txt", std.testing.allocator, .limited(1 << 16));
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
        // the golden's primes (AF') are the Zig names' 2s (af2)
        const low = std.ascii.lowerString(&lower, name);
        var key: [16]u8 = undefined;
        var kn: usize = 0;
        for (low) |ch| {
            key[kn] = if (ch == '\'') '2' else ch;
            kn += 1;
        }
        inline for (@typeInfo(t).@"struct".decls) |d| {
            if (std.mem.eql(u8, d.name, key[0..kn])) {
                try std.testing.expectEqual(off, @as(usize, @field(t, d.name)));
                seen += 1;
            }
        }
    }
    try std.testing.expectEqual(@typeInfo(t).@"struct".decls.len, seen);
}
