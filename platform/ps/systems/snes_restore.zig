//! The Super Nintendo's restore routine (M7 step 7.15a; docs/systems/snes.md,
//! "Save states through the CPU"): for a state table (Snes.Spec.SaveState,
//! Snes.StateTap later), the 65C816's code for the ROM overlay. The PS
//! holds the core, writes the memories, the overlay and its end address,
//! lets go; the CPU starts from its reset vector in the overlay, puts the
//! 5A22's and the PPU's registers back, waits, and ends with an RTI (from
//! the overlay's bytes at the saved stack) on the saved master clock: the
//! line, the dot and the field. A state it can't place is refused.
//!
//! The overlay answers the CPU's reads of bank 00h, 8000h-FFFFh (its bytes
//! mod 800h: the code from 8000h, the vectors at FFE0h-FFFFh) and
//! 0000h-1FFFh (mod 800h, bit 10 inverted: the stack bytes the routine
//! pulls, PLD's, PLB's and PLP's; the last JML's pointer is in the code). Nothing is pushed but an NMI's entry, with S in
//! 2200h-3FFFh, where writes go nowhere.
//!
//! Timing is the spec's ("Snes.Spec.Machine", "W65c816.Spec"), clock by
//! clock: 8 clocks for the overlay, 6 for 2100h-21FFh and 4200h-5FFFh, 12
//! for 4000h-41FFh; a read 4 clocks before its cycle's end, a write at its
//! end; the refresh's 40 clocks at 538; 1364-clock lines (1360 for line
//! 240 of the odd field without interlace), 262 lines (263 for the even
//! field with interlace); V-blank's NMI flag at clock 2 of its first line;
//! the interrupt lines polled at each cycle's end, as the CPU does.
//!
//! What the routine can't put back (the restore test leaves them out,
//! "loose"): the VRAM read latch (it is VRAM's word at the address after
//! the address write), the open bus (the RTI's last byte), STAT77's sprite
//! flags, HDMA's per-frame channel flags (set again at line 0), the pads'
//! auto-read and serial registers (read again from the pads), and the data
//! ports' last bytes (2104h, 2118h, 2119h, 2122h as written), and VRAM's
//! and CGRAM's address registers as written (2116h, 2117h, 2121h: the
//! addresses themselves, which move on with the data ports, are put back).

const std = @import("std");

pub const table_size: usize = 520;
pub const overlay_size: usize = 0x800;
/// the slot's bytes the routine writes: the DMA registers (4300h-437Fh:
/// the routine's DMA, which moves OAM's address on, borrows channel 0 and
/// puts it back), then OAM's high table (32 bytes: registers in the
/// circuit, written through 2104h)
pub const fill_size: usize = 128 + 32;

/// The table's fields (4 bytes each; Snes.Spec.SaveState's order).
pub const F = struct {
    pub const A = 0;
    pub const X = 1;
    pub const Y = 2;
    pub const S = 3;
    pub const D = 4;
    pub const DBR = 5;
    pub const PBR = 6;
    pub const PC = 7;
    pub const P = 8;
    pub const E = 9;
    pub const run = 10;
    pub const nmi_line = 11;
    pub const nmi_need = 12;
    pub const poll_cur = 13;
    pub const poll_prev = 14;
    pub const pending = 15;
    pub const hc = 16;
    pub const vc = 17;
    pub const field = 18;
    pub const mdr = 19;
    pub const nmitimen = 20;
    pub const rdnmi = 21;
    pub const timeup = 22;
    pub const wrio = 23;
    pub const mpya = 24;
    pub const shift = 25;
    pub const div = 26;
    pub const htime = 27;
    pub const vtime = 28;
    pub const hdmaen = 29;
    pub const memsel = 30;
    pub const rddiv = 31;
    pub const rdmpy = 32;
    pub const joy1 = 33;
    pub const joy_busy = 37;
    pub const strobe = 38;
    pub const ser1 = 39;
    pub const ser2 = 40;
    pub const vaddr = 41;
    pub const vmain = 42;
    pub const vlatch = 43;
    pub const cgadd = 44;
    pub const cg_latch = 45;
    pub const cg_flip = 46;
    pub const oamadd = 47;
    pub const oam_latch = 48;
    pub const oam_rel = 49;
    pub const wmadd = 50;
    pub const hdma_do = 51;
    pub const hdma_done = 52;
    pub const hlatch = 53;
    pub const vlatch2 = 54;
    pub const latched = 55;
    pub const m7_prev = 56;
    pub const hflip = 57;
    pub const vflip = 58;
    pub const bg_prev = 59;
    pub const bg_prev2 = 60;
    pub const ppu = 61; // 2100h + r at ppu + r, r 0 to 42h
    pub const stat77 = 128;
    pub const mc8 = 129;
};

/// The restore test's mutants (the host tool sets it from
/// SNES_RESTORE_MUTANT; 0, none, everywhere else): each leaves one part of
/// the routine out or wrong, and some case must fail for each.
pub var mutant: u32 = 0;

pub const Error = error{
    /// the saved moment isn't one the routine can end on (not in V-blank)
    BadPosition,
    /// the routine, its stack bytes and the vectors don't fit the overlay
    TooLong,
    /// no schedule lands the routine's end on the saved clock
    NoSchedule,
    /// (schedule failures, by phase: the counter latch, the NMI, the tail)
    LatchTime,
    NmiTime,
    TailTime,
    /// the multiplier's and divider's results come from no one operation
    Alu,
    /// the stack the RTI pulls from isn't in 0000h-1FFFh
    Stack,
    /// a 16-bit emulation-mode state with PBR not 0, or the like
    Registers,
    /// OAM's address and latch: no write sequence gives them
    Oam,
    /// the CPU stopped (STP): no routine resumes it
    Stopped,
    /// BG1's scroll and M7HOFS (or VOFS) don't come from the same bytes
    Scroll,
};

pub const Result = struct { end: u16, cycles: u32, marks: [max_marks]Mark = undefined, nmarks: usize = 0 };

/// A checkpoint of the routine (for the tests): an instruction's address
/// and the moment it starts, as the generator's simulation has it.
pub const Mark = struct { pc: u32, frame: u32, vc: u32, hc: u32 };
pub const max_marks = 12;

/// The overlay's byte for a pull from 0000h-1FFFh: mod 800h, bit 10
/// inverted (Snes.Spec.Machine's Overlay: a stack near 1FFFh off the vectors).
fn lowOff(addr: u32) u32 {
    return (addr & 0x7FF) ^ 0x400;
}

fn field(t: []const u8, i: usize) u32 {
    return std.mem.readInt(u32, t[4 * i ..][0..4], .little);
}

// --- the machine's time and the registers that change it ---------------------

const Sim = struct {
    hc: u32 = 0,
    vc: u32 = 0,
    fld: u1 = 0,
    frame: u32 = 0,
    setini: u8 = 0,
    inidisp: u8 = 0,
    // 5A22
    rdnmi: bool = false,
    nmitimen: u8 = 0,
    timeup: bool = false,
    htime: u32 = 0,
    vtime: u32 = 0,
    wrio: u8 = 0xFF,
    // the counter latch
    hlatch: u32 = 0,
    vlatch: u32 = 0,
    latched: bool = false,
    hflip: bool = false,
    vflip: bool = false,
    // OAM's address and its reload at V-blank
    oamadd: u32 = 0,
    oam_rel: u32 = 0,
    // the CPU's interrupt lines (W65c816.Spec's endCycle)
    nmi_line: bool = false,
    nmi_need: bool = false,
    poll_cur: u2 = 0,
    poll_prev: u2 = 0,
    pending: u2 = 0,
    iflag: bool = true,
    // the routine's own count of cycles; the master clock since power-on
    cycles: u32 = 0,
    mc: u64 = 0,
    // a DMA owed (channel 0's bytes), started before the next cycle
    dma_bytes: u32 = 0,
    dma_count: u32 = 0,

    fn vbs(s: *const Sim) u32 {
        return if (s.setini & 0x04 != 0) 240 else 225;
    }
    fn lineLen(s: *const Sim) u32 {
        return if (s.vc == 240 and s.fld == 1 and s.setini & 1 == 0) 1360 else 1364;
    }
    fn linesPerFrame(s: *const Sim) u32 {
        return if (s.setini & 1 != 0 and s.fld == 0) 263 else 262;
    }
    fn irqPos(s: *const Sim) ?u32 {
        return switch (s.nmitimen & 0x30) {
            0x00 => null,
            0x20 => 10,
            else => s.htime * 4 + 14,
        };
    }
    fn nextEvent(s: *const Sim) u32 {
        const es = [_]u32{ 2, 538, s.lineLen() };
        var best: u32 = std.math.maxInt(u32);
        for (es) |e| if (e > s.hc and e < best) {
            best = e;
        };
        if (s.irqPos()) |p| if (p > s.hc and p < best) {
            best = p;
        };
        return best;
    }
    /// Snes.Spec.Machine's tick: n clocks with every event on the way.
    fn tick(s: *Sim, n0: u32) void {
        var n = n0;
        while (n > 0) {
            const next = s.nextEvent();
            const d = @min(n, next - s.hc);
            s.hc += d;
            s.mc += d;
            n -= d;
            if (s.hc == next) s.event();
        }
    }
    fn event(s: *Sim) void {
        if (s.hc == s.lineLen()) {
            s.hc = 0;
            if (s.vc + 1 >= s.linesPerFrame()) {
                s.vc = 0;
                s.fld ^= 1;
                s.frame += 1;
            } else s.vc += 1;
            return;
        }
        if (s.hc == 2 and s.vc == s.vbs()) {
            s.rdnmi = true;
            s.oamadd = s.oam_rel;
        }
        if (s.hc == 2 and s.vc == 0) s.rdnmi = false;
        if (s.hc == 538) s.tick(if (mutant == 1) 38 else 40);
        if (s.irqPos()) |p| {
            if (p == s.hc and (s.nmitimen & 0x20 == 0 or s.vc == s.vtime)) s.timeup = true;
        }
    }
    fn endCycle(s: *Sim) void {
        const nmi = s.rdnmi and s.nmitimen & 0x80 != 0;
        if (nmi and !s.nmi_line) s.nmi_need = true;
        s.nmi_line = nmi;
        s.poll_prev = s.poll_cur;
        s.poll_cur = @as(u2, @intFromBool(s.nmi_need)) | (if (s.timeup and !s.iflag) @as(u2, 2) else 0);
        s.cycles += 1;
    }
    fn speed(s: *const Sim, a: u32) u32 {
        _ = s;
        const bank = a >> 16;
        const off = a & 0xFFFF;
        if (bank >= 0x40 and bank < 0x80) return 8;
        if (bank >= 0xC0) return 8; // (MEMSEL: the routine runs in bank 00)
        if (off < 0x2000) return 8;
        if (off < 0x4000) return 6;
        if (off < 0x4200) return 12;
        if (off < 0x6000) return 6;
        return 8;
    }
    /// Snes.Spec.Machine's dmaEdge: the clock brought to a multiple of 8,
    /// 8 clocks, then the channel's 8 and 8 a byte (2138h's reads move
    /// OAM's address on)
    fn dmaEdge(s: *Sim) void {
        if (s.dma_bytes == 0) return;
        const n = s.dma_bytes;
        s.dma_bytes = 0;
        s.tick(@intCast((8 - s.mc % 8) % 8));
        s.tick(8);
        s.tick(8);
        var k: u32 = 0;
        while (k < n) : (k += 1) {
            s.oamadd = (s.oamadd + 1) & 0x3FF;
            s.tick(8);
        }
    }
    fn read(s: *Sim, a: u32) void {
        s.dmaEdge();
        const sp = s.speed(a);
        s.tick(sp - 4);
        s.readEffect(a & 0xFFFF);
        s.tick(4);
        s.endCycle();
    }
    fn write(s: *Sim, a: u32, v: u8) void {
        s.dmaEdge();
        s.tick(s.speed(a));
        s.writeEffect(a & 0xFFFF, v);
        s.endCycle();
    }
    fn idle(s: *Sim) void {
        s.dmaEdge();
        s.tick(6);
        s.endCycle();
    }
    fn hdot(hc: u32) u32 {
        if (hc < 1292) return hc / 4;
        if (hc < 1298) return 323;
        if (hc < 1310) return 324 + (hc - 1298) / 4;
        if (hc < 1316) return 327;
        return 328 + (hc - 1316) / 4;
    }
    fn latchCounters(s: *Sim) void {
        s.hlatch = hdot(s.hc);
        s.vlatch = s.vc;
        s.latched = true;
    }
    fn readEffect(s: *Sim, a: u32) void {
        switch (a) {
            0x4210 => s.rdnmi = false,
            0x4211 => s.timeup = false,
            0x2137 => s.latchCounters(),
            0x213C => s.hflip = !s.hflip,
            0x213D => s.vflip = !s.vflip,
            0x213F => {
                s.latched = false;
                s.hflip = false;
                s.vflip = false;
            },
            else => {},
        }
    }
    fn writeEffect(s: *Sim, a: u32, v: u8) void {
        switch (a) {
            0x2100 => s.inidisp = v,
            0x2133 => s.setini = v,
            0x2102 => {
                s.oam_rel = (s.oam_rel & 0x200) | (@as(u32, v) << 1);
                s.oamadd = s.oam_rel;
            },
            0x2103 => {
                s.oam_rel = ((@as(u32, v) & 1) << 9) | (s.oam_rel & 0x1FE);
                s.oamadd = s.oam_rel;
            },
            0x2104 => s.oamadd = (s.oamadd + 1) & 0x3FF,
            0x4200 => {
                s.nmitimen = v;
                if (v & 0x30 == 0) s.timeup = false;
            },
            0x4201 => {
                if (s.wrio & 0x80 != 0 and v & 0x80 == 0) s.latchCounters();
                s.wrio = v;
            },
            0x420B => if (v & 1 != 0) {
                s.dma_bytes = s.dma_count;
            },
            0x4305 => s.dma_count = (s.dma_count & 0xFF00) | v,
            0x4306 => s.dma_count = (s.dma_count & 0xFF) | (@as(u32, v) << 8),
            0x4207 => s.htime = (s.htime & 0x100) | v,
            0x4208 => s.htime = (s.htime & 0xFF) | ((@as(u32, v) & 1) << 8),
            0x4209 => s.vtime = (s.vtime & 0x100) | v,
            0x420A => s.vtime = (s.vtime & 0xFF) | ((@as(u32, v) & 1) << 8),
            else => {},
        }
    }
    /// clocks since power-on, frames counted from the routine's start
    fn time(s: *const Sim) u64 {
        // (frames of different lengths: kept as a position, compared as one)
        return (@as(u64, s.frame) << 32) | (@as(u64, s.vc) << 16) | s.hc;
    }
};

// --- the code ----------------------------------------------------------------

/// The assembler's states: static, not on the stack (each is over 4 KiB,
/// the PS app's frames are up to 2 KiB of a 16 KiB stack; step 7.15d). One
/// for each place that keeps a state or tries a copy: none of them is in
/// use twice at once (the generator isn't reentrant).
var st_main: Asm = undefined;
var st_wait: Asm = undefined;
var st_read_b: Asm = undefined;
var st_read_c: Asm = undefined;
var st_finish: Asm = undefined;
/// the overlay of the attempt that worked ('Built')
var built_ovl: [overlay_size]u8 = undefined;

/// A fresh state, field by field (a struct literal of this size would be a
/// 4 KiB template in the image's read-only data).
fn resetAsm(a: *Asm) void {
    @memset(&a.ovl, 0);
    @memset(&a.used, false);
    a.pc = 0x8000;
    a.sim = .{};
    a.m8 = true;
    a.x8 = true;
    a.emu = true;
    a.s = 0x01FF;
    a.nmi_vector_set = false;
    a.rti_at = 0;
    a.nmarks = 0;
    a.err = null;
}

const Asm = struct {
    ovl: [overlay_size]u8 = [_]u8{0} ** overlay_size,
    used: [overlay_size]bool = [_]bool{false} ** overlay_size,
    /// the next instruction's address (bank 00h, 8000h + offset)
    pc: u32 = 0x8000,
    sim: Sim = .{},
    m8: bool = true,
    x8: bool = true,
    emu: bool = true,
    s: u32 = 0x01FF,
    nmi_vector_set: bool = false,
    /// the RTI's opcode address (the overlay's end)
    rti_at: u32 = 0,
    marks: [max_marks]Mark = undefined,
    nmarks: usize = 0,
    err: ?Error = null,

    fn mark(a: *Asm) void {
        if (a.nmarks < max_marks) {
            a.marks[a.nmarks] = .{ .pc = a.pc, .frame = a.sim.frame, .vc = a.sim.vc, .hc = a.sim.hc };
            a.nmarks += 1;
        }
    }
    fn reserve(a: *Asm, off: usize) void {
        a.used[off & 0x7FF] = true;
    }
    fn free(a: *const Asm, off: u32, n: u32) bool {
        var i: u32 = 0;
        while (i < n) : (i += 1) if (a.used[(off + i) & 0x7FF]) return false;
        return true;
    }
    /// Room for n bytes at pc, else a BRA over what's taken (each
    /// instruction leaves two free bytes after it: room for that BRA).
    fn room(a: *Asm, n: u32) void {
        if (a.free(a.pc & 0x7FF, n + 2)) return;
        // find the next run of n + 2 free bytes
        var o: u32 = (a.pc & 0x7FF) + 2;
        while (o + n < 0x7E0 and !a.free(o, n + 2)) o += 1;
        if (o + n >= 0x7E0 or !a.free(a.pc & 0x7FF, 2)) {
            a.err = Error.TooLong;
            return;
        }
        const rel = o - ((a.pc & 0x7FF) + 2);
        if (rel > 127) {
            a.err = Error.TooLong;
            return;
        }
        a.byte(0x80);
        a.byte(@intCast(rel));
        // BRA: op, offset, an internal cycle
        a.sim.read(a.pc - 2);
        a.sim.read(a.pc - 1);
        a.sim.idle();
        a.boundary();
        a.pc = 0x8000 + o;
    }
    fn byte(a: *Asm, v: u8) void {
        const o = a.pc & 0x7FF;
        if (o >= 0x7E0) {
            a.err = Error.TooLong;
            return;
        }
        a.ovl[o] = v;
        a.used[o] = true;
        a.pc += 1;
    }
    /// Between instructions: an NMI owed is taken here (its entry from the
    /// overlay's vector, which points at the next instruction).
    fn boundary(a: *Asm) void {
        // (the step's end: its pending interrupt is the poll before the
        // last cycle's; the next step takes an NMI owed)
        a.sim.pending = a.sim.poll_prev;
        if (a.sim.pending & 1 != 0) {
            a.sim.nmi_need = false;
            a.sim.idle();
            a.sim.idle();
            // pushes: PBR (native), PCH, PCL, P, at S (in 2200h-3FFFh)
            const n: u32 = if (a.emu) 3 else 4;
            var k: u32 = 0;
            while (k < n) : (k += 1) {
                a.sim.write(a.s, 0);
                a.s = if (a.emu) 0x100 | ((a.s -% 1) & 0xFF) else (a.s -% 1) & 0xFFFF;
            }
            a.sim.iflag = true;
            const vec: u32 = if (a.emu) 0xFFFA else 0xFFEA;
            a.sim.read(vec);
            a.sim.read(vec + 1);
            // the vector: the next instruction
            if (a.nmi_vector_set) {
                a.err = Error.NoSchedule;
            }
            a.ovl[vec & 0x7FF] = @intCast(a.pc & 0xFF);
            a.ovl[(vec + 1) & 0x7FF] = @intCast((a.pc >> 8) & 0xFF);
            a.nmi_vector_set = true;
            a.sim.pending = a.sim.poll_prev;
        }
    }
    fn opFetch(a: *Asm, op: u8, len: u32) void {
        a.room(len);
        a.sim.read(a.pc);
        a.byte(op);
    }
    fn operand(a: *Asm, v: u8) void {
        a.sim.read(a.pc);
        a.byte(v);
    }
    fn implied(a: *Asm, op: u8) void {
        a.opFetch(op, 1);
        a.sim.idle();
        a.boundary();
    }
    fn clc(a: *Asm) void {
        a.implied(0x18);
    }
    fn sec(a: *Asm) void {
        a.implied(0x38);
    }
    fn nop(a: *Asm) void {
        a.implied(0xEA);
    }
    fn xce(a: *Asm, carry: bool) void {
        a.implied(0xFB);
        a.emu = carry;
        if (carry) {
            a.m8 = true;
            a.x8 = true;
            a.s = 0x100 | (a.s & 0xFF);
        }
    }
    fn txs(a: *Asm, x: u32) void {
        a.implied(0x9A);
        a.s = if (a.emu) 0x100 | (x & 0xFF) else x & 0xFFFF;
    }
    fn dex(a: *Asm) void {
        a.implied(0xCA);
    }
    fn rep(a: *Asm, v: u8) void {
        a.opFetch(0xC2, 2);
        a.operand(v);
        a.sim.idle();
        if (v & 0x20 != 0 and !a.emu) a.m8 = false;
        if (v & 0x10 != 0 and !a.emu) a.x8 = false;
        a.boundary();
    }
    fn sep(a: *Asm, v: u8) void {
        a.opFetch(0xE2, 2);
        a.operand(v);
        a.sim.idle();
        if (v & 0x20 != 0) a.m8 = true;
        if (v & 0x10 != 0) a.x8 = true;
        a.boundary();
    }
    fn ldaImm(a: *Asm, v: u16) void {
        a.opFetch(0xA9, if (a.m8) 2 else 3);
        a.operand(@intCast(v & 0xFF));
        if (!a.m8) a.operand(@intCast(v >> 8));
        a.boundary();
    }
    fn ldxImm(a: *Asm, v: u16) void {
        a.opFetch(0xA2, if (a.x8) 2 else 3);
        a.operand(@intCast(v & 0xFF));
        if (!a.x8) a.operand(@intCast(v >> 8));
        a.boundary();
    }
    fn ldyImm(a: *Asm, v: u16) void {
        a.opFetch(0xA0, if (a.x8) 2 else 3);
        a.operand(@intCast(v & 0xFF));
        if (!a.x8) a.operand(@intCast(v >> 8));
        a.boundary();
    }
    /// STA abs (8-bit A): the register write
    fn sta(a: *Asm, addr: u16, v: u8) void {
        a.opFetch(0x8D, 3);
        a.operand(@intCast(addr & 0xFF));
        a.operand(@intCast(addr >> 8));
        a.sim.write(addr, v);
        a.boundary();
    }
    /// LDA #v; STA addr (8-bit A)
    fn put(a: *Asm, addr: u16, v: u8) void {
        a.ldaImm(v);
        a.sta(addr, v);
    }
    /// LDA abs (8-bit A): a register read for its effect
    fn lda(a: *Asm, addr: u16) void {
        a.opFetch(0xAD, 3);
        a.operand(@intCast(addr & 0xFF));
        a.operand(@intCast(addr >> 8));
        a.sim.read(addr);
        a.boundary();
    }
    fn plb(a: *Asm) void {
        a.opFetch(0xAB, 1);
        a.sim.idle();
        a.sim.idle();
        a.s = (a.s + 1) & 0xFFFF;
        a.sim.read(a.s);
        a.boundary();
    }
    fn pld(a: *Asm) void {
        a.opFetch(0x2B, 1);
        a.sim.idle();
        a.sim.idle();
        a.s = (a.s + 1) & 0xFFFF;
        a.sim.read(a.s);
        a.s = (a.s + 1) & 0xFFFF;
        a.sim.read(a.s);
        a.boundary();
    }
    /// LDX #n; DEX; BNE back (X 16-bit): 36n - 6 clocks after the LDX
    /// (without the refresh)
    fn loop(a: *Asm, n: u16) void {
        if (n == 0) return;
        a.ldxImm(n);
        a.room(3);
        const top = a.pc;
        var k: u32 = n;
        // the loop's bytes, once
        a.sim.read(a.pc);
        a.byte(0xCA);
        a.sim.idle();
        a.boundary();
        a.sim.read(a.pc);
        a.byte(0xD0);
        a.sim.read(a.pc);
        a.byte(@intCast((top -% (a.pc + 1)) & 0xFF));
        k -= 1;
        if (k > 0) a.sim.idle(); // taken
        a.boundary();
        while (k > 0) : (k -= 1) {
            a.sim.read(top);
            a.sim.idle();
            a.boundary();
            a.sim.read(top + 1);
            a.sim.read(top + 2);
            if (k > 1) a.sim.idle();
            a.boundary();
        }
    }
    /// LDX #n; LDA addr; DEX; BNE back (X 16-bit): n reads of addr
    fn readLoop(a: *Asm, addr: u16, n: u16) void {
        if (n == 0) return;
        a.ldxImm(n);
        a.room(6);
        const top = a.pc;
        var k: u32 = n;
        while (k > 0) : (k -= 1) {
            const first = k == n;
            // LDA addr
            a.sim.read(top);
            if (first) a.byte(0xAD);
            a.sim.read(top + 1);
            if (first) a.byte(@intCast(addr & 0xFF));
            a.sim.read(top + 2);
            if (first) a.byte(@intCast(addr >> 8));
            a.sim.read(addr);
            a.boundary();
            // DEX
            a.sim.read(top + 3);
            if (first) a.byte(0xCA);
            a.sim.idle();
            a.boundary();
            // BNE top
            a.sim.read(top + 4);
            if (first) a.byte(0xD0);
            a.sim.read(top + 5);
            if (first) a.byte(@intCast((top -% (top + 6)) & 0xFF));
            if (k > 1) a.sim.idle();
            a.boundary();
        }
        a.pc = top + 6;
    }

};

// --- the solvers -------------------------------------------------------------

const Pair = struct { reg: u8, lo: u8, hi: u8 };

/// The scroll and Mode 7 registers' write pairs in an order that leaves
/// their latches (BG's old bytes, Mode 7's) as saved.
fn scrollPairs(t: []const u8, out: *[16]Pair) Error!usize {
    const ppu = struct {
        fn r(tt: []const u8, i: u8) u32 {
            return field(tt, F.ppu + i);
        }
    };
    const m7_prev: u8 = @truncate(field(t, F.m7_prev));
    const bg_prev: u8 = @truncate(field(t, F.bg_prev));
    const bg_prev2: u8 = @truncate(field(t, F.bg_prev2));
    // 210Dh/210Eh: their bytes are M7HOFS's and M7VOFS's (40h, 41h)
    const s40 = ppu.r(t, 0x40);
    const s41 = ppu.r(t, 0x41);
    if (ppu.r(t, 0x0D) != s40 & 0x3FF or ppu.r(t, 0x0E) != s41 & 0x3FF) return Error.Scroll;
    // the pairs' bytes (BG's high byte free above its 2 bits: set later)
    var pairs: [14]Pair = undefined;
    var n: usize = 0;
    for ([_]u8{ 0x1B, 0x1C, 0x1D, 0x1E, 0x1F, 0x20 }) |r| {
        const v = ppu.r(t, r);
        pairs[n] = .{ .reg = r, .lo = @truncate(v), .hi = @truncate(v >> 8) };
        n += 1;
    }
    pairs[n] = .{ .reg = 0x0D, .lo = @truncate(s40), .hi = @truncate(s40 >> 8) };
    n += 1;
    pairs[n] = .{ .reg = 0x0E, .lo = @truncate(s41), .hi = @truncate(s41 >> 8) };
    n += 1;
    for ([_]u8{ 0x0F, 0x10, 0x11, 0x12, 0x13, 0x14 }) |r| {
        const v = ppu.r(t, r);
        pairs[n] = .{ .reg = r, .lo = @truncate(v), .hi = @truncate(v >> 8) };
        n += 1;
    }
    const isM7 = struct {
        fn f(r: u8) bool {
            return r <= 0x0E or r >= 0x1B;
        }
    }.f;
    const isBg = struct {
        fn f(r: u8) bool {
            return r >= 0x0D and r <= 0x14;
        }
    }.f;
    // X: the last Mode 7 pair, Z the last HOFS one, Y the last BG one
    for (pairs[0..n]) |px| {
        if (!isM7(px.reg) or px.hi != m7_prev) continue;
        for (pairs[0..n]) |pz| {
            if (!isBg(pz.reg) or pz.reg & 1 == 0) continue;
            for (pairs[0..n]) |py| {
                if (!isBg(py.reg)) continue;
                if (py.reg & 1 == 1 and py.reg != pz.reg) continue;
                // the order: the rest, then X (if neither Y nor Z), Z, Y
                var o: usize = 0;
                for (pairs[0..n]) |p| {
                    if (p.reg == px.reg or p.reg == py.reg or p.reg == pz.reg) continue;
                    out[o] = p;
                    o += 1;
                }
                if (px.reg != py.reg and px.reg != pz.reg) {
                    out[o] = px;
                    o += 1;
                }
                if (pz.reg != py.reg) {
                    out[o] = pz;
                    o += 1;
                }
                out[o] = py;
                o += 1;
                // the high bytes left free: BG's above its 2 bits
                const zi = if (pz.reg != py.reg) o - 2 else o - 1;
                if (!fixHigh(&out[zi], bg_prev2)) continue;
                if (!fixHigh(&out[o - 1], bg_prev)) continue;
                if (pz.reg == py.reg and bg_prev != bg_prev2) continue;
                if (!checkLatches(out[0..o], m7_prev, bg_prev, bg_prev2)) continue;
                return o;
            }
        }
    }
    return Error.Scroll;
}

/// A BG pair's high byte made the latch's value, where only its low 2 bits
/// count (210Dh/210Eh: Mode 7 takes all 8).
fn fixHigh(p: *Pair, want: u8) bool {
    if (p.hi == want) return true;
    if (p.reg == 0x0D or p.reg == 0x0E) return false;
    if (p.hi & 3 != want & 3) return false;
    p.hi = want;
    return true;
}

fn checkLatches(ps: []const Pair, m7_prev: u8, bg_prev: u8, bg_prev2: u8) bool {
    var m7: ?u8 = null;
    var bg: ?u8 = null;
    var bg2: ?u8 = null;
    for (ps) |p| {
        if (p.reg <= 0x0E or p.reg >= 0x1B) m7 = p.hi;
        if (p.reg >= 0x0D and p.reg <= 0x14) {
            bg = p.hi;
            if (p.reg & 1 == 1) bg2 = p.hi;
        }
    }
    return (m7 orelse 0) == m7_prev and (bg orelse 0) == bg_prev and (bg2 orelse 0) == bg_prev2;
}

/// The multiplier's and divider's last operation, replayed (from the
/// shifter: a multiply leaves it MPYB << 8 and RDDIV MPYB, a divide DIVB).
const AluPlan = struct {
    mul: ?struct { a: u8, b: u8 } = null,
    div: ?struct { x: u16, b: u8 } = null,
};

fn aluPlan(t: []const u8) Error!AluPlan {
    const shift = field(t, F.shift) & 0xFFFFFF;
    const rddiv = field(t, F.rddiv) & 0xFFFF;
    const rdmpy = field(t, F.rdmpy) & 0xFFFF;
    // a multiply last
    if (rddiv <= 0xFF and shift == rddiv << 8) {
        const b: u32 = rddiv;
        const ok = if (b == 0) rdmpy == 0 else rdmpy % b == 0 and rdmpy / b <= 0xFF;
        if (ok) return .{ .mul = .{ .a = if (b == 0) 0 else @intCast(rdmpy / b), .b = @intCast(b) } };
    }
    // a divide last
    if (shift <= 0xFF) {
        const b: u32 = shift;
        if (b == 0) {
            if (rddiv == 0xFFFF) return .{ .div = .{ .x = @intCast(rdmpy), .b = 0 } };
        } else if (rdmpy < b) {
            const x = rddiv * b + rdmpy;
            if (x <= 0xFFFF) return .{ .div = .{ .x = @intCast(x), .b = @intCast(b) } };
        }
    }
    return Error.Alu;
}

// --- the routine -------------------------------------------------------------

/// The overlay for table @t (and the slot's bytes it writes, `fill_size`),
/// its end address (the RTI's opcode) and the routine's cycles.
pub fn build(t: []const u8, fill: []const u8, overlay: *[overlay_size]u8) Error!Result {
    if (t.len < table_size or fill.len < fill_size) return Error.TooLong;
    const vc = field(t, F.vc);
    const vbs: u32 = if (field(t, F.ppu + 0x33) & 0x04 != 0) 240 else 225;
    if (vc <= vbs or vc > 259) return Error.BadPosition;
    if (field(t, F.run) == 2) return Error.Stopped;
    // the frames: the end's has the saved field (frame k has field k mod 2)
    const fld: u32 = field(t, F.field) & 1;
    var end_frame: u32 = fld;
    var last: Error = Error.NoSchedule;
    while (end_frame < 6) : (end_frame += 2) {
        var lf: u32 = if (end_frame == 0) 0 else end_frame - 1;
        while (lf <= end_frame) : (lf += 1) {
            if (attempt(t, fill, end_frame, lf)) |r| {
                overlay.* = built_ovl;
                return .{ .end = r.end, .cycles = r.cycles, .marks = r.marks, .nmarks = r.nmarks };
            } else |e| switch (e) {
                Error.NoSchedule, Error.LatchTime, Error.NmiTime, Error.TailTime => {
                    last = e;
                },
                else => return e,
            }
        }
    }
    return last;
}

/// (its overlay in `built_ovl`)
const Built = struct { end: u16, cycles: u32, marks: [max_marks]Mark, nmarks: usize };

fn attempt(t: []const u8, fill: []const u8, end_frame: u32, latch_frame: u32) Error!Built {
    const dma = fill[0..128];
    const a = &st_main;
    resetAsm(a);
    // the bytes the routine pulls: P (PLP's, at the saved stack: S one
    // below it), DBR and D (PLB's and PLD's, at q); the JML's pointer; the
    // vectors
    const e = field(t, F.E) != 0;
    const s_saved = field(t, F.S) & 0xFFFF;
    const s_plp = if (e) 0x100 | ((s_saved -% 1) & 0xFF) else (s_saved -% 1) & 0xFFFF;
    const p_addr = if (e) 0x100 | (s_saved & 0xFF) else s_saved;
    // (DBR's byte just below: PLB pulls it with S one below that)
    if (p_addr >= 0x2000 or s_plp >= 0x2000) return Error.Stack;
    // (not on a vector the routine needs: the reset's, the NMI's)
    for ([_]u32{ p_addr, s_plp }) |addr| switch (lowOff(addr)) {
        0x7FC, 0x7FD, 0x7EA, 0x7EB => return Error.Stack,
        else => {},
    };
    a.reserve(lowOff(p_addr));
    a.reserve(lowOff(s_plp));
    var i: usize = 0x7E0;
    while (i < 0x800) : (i += 1) a.used[i] = true;
    // the JML's pointer: three bytes below the vectors
    var ptr: u32 = 0x7DD;
    while (ptr > 0x700 and !a.free(ptr, 3)) ptr -= 3;
    a.reserve(ptr);
    a.reserve(ptr + 1);
    a.reserve(ptr + 2);
    // q: two free bytes below 2000h for PLD's pulls (q + 1, q + 2)
    var q: u32 = 0x1F00;
    while (q > 0x100 and !(a.free(lowOff(q + 1), 1) and a.free(lowOff(q + 2), 1))) q -= 4;
    a.reserve(lowOff(q + 1));
    a.reserve(lowOff(q + 2));
    a.ovl[lowOff(q + 1)] = @truncate(field(t, F.D));
    a.ovl[lowOff(q + 2)] = @truncate(field(t, F.D) >> 8);
    // the reset vector: 8000h; the reset's two internal cycles and its
    // vector's reads
    a.ovl[0x7FC] = 0x00;
    a.ovl[0x7FD] = 0x80;
    a.sim.idle();
    a.sim.idle();
    a.sim.read(0xFFFC);
    a.sim.read(0xFFFD);
    // native, A 8-bit, X 16-bit; S where an NMI's pushes go nowhere
    a.mark();
    a.clc();
    a.xce(false);
    a.rep(0x10);
    a.x8 = false;
    // Y and D now (nothing below uses them): D by PLD from q, S back after
    a.ldyImm(@truncate(field(t, F.Y)));
    a.ldxImm(@intCast(q));
    a.txs(q);
    if (mutant != 2) a.pld();
    a.ldxImm(0x3FFF);
    a.txs(0x3FFF);
    a.put(0x2133, @truncate(field(t, F.ppu + 0x33)));
    a.put(0x2100, 0x80);
    // the PPU's plain registers
    var r: u8 = 0x01;
    while (r < 0x40) : (r += 1) {
        switch (r) {
            0x02, 0x03, 0x04, 0x0D...0x14, 0x15, 0x16, 0x17, 0x18, 0x19, 0x1B...0x20, 0x21, 0x22, 0x32, 0x33 => continue,
            else => {},
        }
        const v: u8 = @truncate(field(t, F.ppu + r));
        // (as at power-on: no write)
        if (v != 0) a.put(0x2100 + @as(u16, r), v);
    }
    // the scrolls and Mode 7
    var pairs: [16]Pair = undefined;
    const np = try scrollPairs(t, &pairs);
    for (pairs[0 .. if (mutant == 3) np - 1 else np]) |p| {
        a.put(0x2100 + @as(u16, p.reg), p.lo);
        a.put(0x2100 + @as(u16, p.reg), p.hi);
    }
    // COLDATA (42h): its three channels
    const col = field(t, F.ppu + 0x42);
    a.put(0x2132, @intCast(0x20 | (col & 0x1F)));
    a.put(0x2132, @intCast(0x40 | ((col >> 5) & 0x1F)));
    a.put(0x2132, @intCast(0x80 | ((col >> 10) & 0x1F)));
    // VRAM's port: VMAIN, then the address (its prefetch)
    if (mutant != 4) a.put(0x2115, @truncate(field(t, F.vmain)));
    const vaddr = field(t, F.vaddr);
    a.put(0x2116, @truncate(vaddr));
    a.put(0x2117, @truncate(vaddr >> 8));
    // CGRAM's address and its latch (forced blank: no drawing): the latch
    // keeps its byte after a colour's write, so it is set first (a write
    // with the flip-flop clear takes no colour), the address after (which
    // clears the flip-flop), the latch again if the flip-flop was set
    const cg_latch: u8 = @truncate(field(t, F.cg_latch));
    const cg_flip = field(t, F.cg_flip) != 0;
    if (cg_latch != 0 or cg_flip) {
        a.put(0x2121, 0);
        a.put(0x2122, cg_latch);
    }
    a.put(0x2121, @truncate(field(t, F.cgadd)));
    if (cg_flip and mutant != 5) a.put(0x2122, cg_latch);
    // OAM's high table (registers in the circuit): from byte 200h
    a.put(0x2102, 0x00);
    a.put(0x2103, 0x01);
    var hi: u16 = 0;
    while (hi < 32) : (hi += 1) {
        const v = fill[128 + hi];
        a.put(0x2104, v);
    }
    // OAM's latch (its address after V-blank's reload, below)
    const oam_latch: u8 = @truncate(field(t, F.oam_latch));
    if (oam_latch != 0) {
        a.put(0x2102, 0);
        a.put(0x2103, 0);
        a.put(0x2104, oam_latch);
    }
    // WRAM's port
    const wm = field(t, F.wmadd);
    a.put(0x2181, @truncate(wm));
    a.put(0x2182, @truncate(wm >> 8));
    a.put(0x2183, @truncate(wm >> 16));
    // the 5A22: WRIO, MEMSEL, the multiplier and divider, the IRQ timer
    if (mutant != 6) a.put(0x4201, @truncate(field(t, F.wrio)));
    a.put(0x420D, @truncate(field(t, F.memsel)));
    const alu: AluPlan = if (mutant == 7) .{} else try aluPlan(t);
    if (alu.mul) |m| {
        a.put(0x4202, m.a);
        a.put(0x4203, m.b);
        aluWait(a);
    }
    if (alu.div) |d| {
        a.put(0x4204, @truncate(d.x));
        a.put(0x4205, @truncate(d.x >> 8));
        a.put(0x4206, d.b);
        aluWait(a);
    }
    a.put(0x4202, @truncate(field(t, F.mpya)));
    const dv = field(t, F.div);
    a.put(0x4204, @truncate(dv));
    a.put(0x4205, @truncate(dv >> 8));
    const ht = field(t, F.htime);
    const vt = field(t, F.vtime);
    a.put(0x4207, @truncate(ht));
    a.put(0x4208, @truncate(ht >> 8));
    a.put(0x4209, @truncate(vt));
    a.put(0x420A, @truncate(vt >> 8));
    if (mutant != 8) a.put(0x4016, @truncate(field(t, F.strobe)));
    if (a.err) |er| return er;
    // the counter latch: at its saved line and dot, in frame latch_frame
    const hl = field(t, F.hlatch);
    const vl = field(t, F.vlatch2);
    const latched = field(t, F.latched) != 0;
    const hf = field(t, F.hflip) != 0;
    const vf = field(t, F.vflip) != 0;
    if (latched or hl != 0 or vl != 0 or hf or vf) {
        // LDA 2137h: its read is 2 clocks into its fourth cycle
        a.mark();
        if (!waitRead(a, latch_frame, vl, hl)) return Error.LatchTime;
        a.mark();
        a.lda(0x2137);
        if (a.sim.hlatch != hl or a.sim.vlatch != vl) return Error.LatchTime;
        if (!latched and mutant != 9) a.lda(0x213F);
        if (hf) a.lda(0x213C);
        if (vf) a.lda(0x213D);
    }
    // NMITIMEN: before V-blank's start when its NMI is on (the NMI is
    // taken, as the saved machine's was), so the end frame's V-blank
    const en: u8 = @truncate(field(t, F.nmitimen));
    const vbs = a.sim.vbs();
    if (!before(&a.sim, end_frame, vbs - 2, 0)) return Error.NmiTime;
    // NMITIMEN at the end frame's start: the IRQ timer then fires through
    // the frame as it did for the game (TIMEUP as saved, checked at the
    // end), the pads' auto-read comes at V-blank; with its NMI on, the NMI
    // is taken (as the saved machine's was) at V-blank's start
    if (!waitTo(a, end_frame, 0, 200)) return Error.NmiTime;
    a.mark();
    a.put(0x4200, en);
    if (en & 0x80 != 0) {
        if (!waitTo(a, end_frame, vbs - 2, 0)) return Error.NmiTime;
        // the NMI, taken at a boundary: NOPs until it is
        var guard: u32 = 0;
        while (!a.nmi_vector_set) : (guard += 1) {
            if (guard > 400) return Error.NmiTime;
            a.nop();
        }
        a.mark();
    } else {
        if (!waitTo(a, end_frame, vbs, 10)) return Error.NmiTime;
    }
    if (field(t, F.rdnmi) == 0 and mutant != 10) a.lda(0x4210);
    // OAM's address, after V-blank's reload
    if (mutant != 11) oamAddress(a, t, dma);
    if (mutant != 12) a.put(0x420C, @truncate(field(t, F.hdmaen)));
    a.put(0x2100, @truncate(field(t, F.ppu + 0x00)));
    // TIMEUP: as saved
    if (a.sim.timeup and field(t, F.timeup) == 0 and mutant != 13) a.lda(0x4211);
    if (a.err) |er| return er;
    // the tail, padded so its RTI ends on the saved clock
    const target_hc = field(t, F.hc);
    const wai = field(t, F.run) == 1;
    return finish(a, t, q, s_plp, p_addr, ptr, end_frame, target_hc, wai);
}

fn aluWait(a: *Asm) void {
    // 16 cycles: the divider's (a NOP is 2)
    var k: u32 = 0;
    while (k < 8) : (k += 1) a.nop();
}

/// Is the sim before (frame, line, clock)?
fn before(s: *const Sim, frame: u32, line: u32, hc: u32) bool {
    if (s.frame != frame) return s.frame < frame;
    if (s.vc != line) return s.vc < line;
    return s.hc < hc;
}

/// Pad (loops, then NOPs) until (frame, line, hc) or a little after;
/// false if past or not reached.
fn waitTo(a: *Asm, frame: u32, line: u32, hc: u32) bool {
    var pass: u32 = 0;
    while (pass < 6 and before(&a.sim, frame, line, hc)) : (pass += 1) {
        const left = clocksTo(&a.sim, frame, line, hc);
        if (left <= 1500) break;
        // a loop to about 700 clocks short (36 clocks an iteration, a
        // line's refresh 40 more), shortened on a copy while it overshoots
        var n: u32 = @min((left - 700) * 1364 / (1404 * 36), 0xFFFF);
        while (n > 0) {
            const b = &st_wait;
            b.* = a.*;
            b.loop(@intCast(n));
            if (before(&b.sim, frame, line, hc)) {
                a.* = b.*;
                break;
            }
            n -= @max(n / 8, 1);
        }
    }
    var guard: u32 = 0;
    while (before(&a.sim, frame, line, hc)) : (guard += 1) {
        if (guard > 300) return false;
        a.nop();
    }
    return a.err == null;
}

/// Clocks from the sim's moment to (frame, line, hc), lines as 1364, a
/// frame as the sim's (262 or 263 lines).
fn clocksTo(s: *const Sim, frame: u32, line: u32, hc: u32) u32 {
    const lpf: u64 = s.linesPerFrame();
    const here = (@as(u64, s.frame) * lpf + s.vc) * 1364 + s.hc;
    const there = (@as(u64, frame) * lpf + line) * 1364 + hc;
    return if (there > here) @intCast(@min(there - here, 1 << 30)) else 0;
}

/// Pad so that the next instruction's fourth cycle reads at (frame, v, h's
/// dot): an LDA abs from the overlay, its read 2 clocks into its last cycle.
fn waitRead(a: *Asm, frame: u32, v: u32, dot: u32) bool {
    // a target clock for the read: the dot's first clock
    const hc_t = dotClock(dot);
    // try paddings: wait to ~300 clocks before, then fine steps
    const lead: u32 = 300;
    var start_line = v;
    var start_hc: u32 = 0;
    if (hc_t >= lead) start_hc = hc_t - lead else if (v > 0) {
        start_line = v - 1;
        start_hc = 1364 - (lead - hc_t);
    }
    if (!waitTo(a, frame, start_line, start_hc)) {
        return false;
    }
    // fine: NOPs (14) and LDA #s (16) until the read lands on the dot
    var nops: u32 = 0;
    while (nops < 30) : (nops += 1) {
        var ldas: u32 = 0;
        while (ldas < 30) : (ldas += 1) {
            const b = &st_read_b;
            b.* = a.*;
            var k: u32 = 0;
            while (k < nops) : (k += 1) b.nop();
            k = 0;
            while (k < ldas) : (k += 1) b.ldaImm(0);
            const c = &st_read_c;
            c.* = b.*;
            c.lda(0x2137);
            if (c.sim.latched and c.sim.hlatch == dot and c.sim.vlatch == v and c.sim.frame == frame) {
                a.* = b.*;
                return true;
            }
        }
    }
    return false;
}

fn dotClock(dot: u32) u32 {
    if (dot < 323) return dot * 4;
    if (dot == 323) return 1292;
    if (dot < 327) return 1298 + (dot - 324) * 4;
    if (dot == 327) return 1310;
    return 1316 + (dot - 328) * 4;
}

/// OAM's address as saved (its latch set above): OAMADD to the reload
/// address, then a DMA of 2138h's reads (B bus to A, its A address
/// 2100h, where a DMA writes nothing) that moves it on; channel 0's
/// registers put back as saved.
fn oamAddress(a: *Asm, t: []const u8, dma: []const u8) void {
    const rel = field(t, F.oam_rel);
    const add = field(t, F.oamadd);
    a.put(0x2102, @truncate(rel >> 1));
    a.put(0x2103, @truncate(rel >> 9));
    const n: u32 = (add -% rel) & 0x3FF;
    if (n == 0) return;
    a.put(0x4300, 0x88);
    a.put(0x4301, 0x38);
    a.put(0x4302, 0x00);
    a.put(0x4303, 0x21);
    a.put(0x4304, 0x00);
    a.put(0x4305, @truncate(n));
    a.put(0x4306, @truncate(n >> 8));
    a.put(0x420B, 0x01);
    var r: u16 = 0;
    while (r < 7) : (r += 1) a.put(0x4300 + r, dma[r]);
}

/// The tail (DBR and D from q, the registers, S, RTI) after padding that
/// lands its end on the saved clock (or, for a state in WAI, its WAI's
/// end there: the RTI returns to the WAI).
fn finish(a: *Asm, t: []const u8, q: u32, s_plp: u32, p_addr: u32, ptr: u32, end_frame: u32, target_hc: u32, wai: bool) Error!Built {
    const target_vc = field(t, F.vc);
    const e = field(t, F.E) != 0;
    const pbr = field(t, F.PBR) & 0xFF;
    if (e and pbr != 0) return Error.Registers;
    const pc_end = if (wai) (field(t, F.PC) -% 1) & 0xFFFF else field(t, F.PC) & 0xFFFF;
    // DBR for the PLB, P for the PLP, PBR:PC for the JML
    a.ovl[lowOff(s_plp)] = @truncate(field(t, F.DBR));
    a.ovl[lowOff(p_addr)] = @truncate(field(t, F.P));
    a.ovl[ptr] = @truncate(pc_end);
    a.ovl[ptr + 1] = @truncate(pc_end >> 8);
    a.ovl[ptr + 2] = @truncate(pbr);
    // the WAI's own cycles after the RTI (its opcode at PBR:PC - 1)
    var wai_clocks: u32 = 0;
    if (wai) {
        const op_addr = (pbr << 16) | pc_end;
        const memsel = field(t, F.memsel) & 1 != 0;
        wai_clocks = opSpeed(op_addr, memsel) + 12;
    }
    // padding: a coarse loop, then NOPs and LDA #s, searched
    const coarse_lines: u32 = 4;
    const start_line = if (target_vc >= coarse_lines) target_vc - coarse_lines else 0;
    if (!waitTo(a, end_frame, start_line, 0)) return Error.TailTime;
    var loops: u32 = 0;
    while (loops < 200) : (loops += 1) {
        var nops: u32 = 0;
        while (nops < 12) : (nops += 1) {
            var ldas: u32 = 0;
            while (ldas < 12) : (ldas += 1) {
                const b = &st_finish;
                b.* = a.*;
                if (loops > 0) b.loop(@intCast(loops));
                var j: u32 = 0;
                while (j < nops) : (j += 1) b.nop();
                j = 0;
                while (j < ldas) : (j += 1) b.ldaImm(0);
                tail(b, t, q, s_plp, ptr, e);
                if (b.err) |er| {
                    if (er == Error.TooLong) return er;
                    continue;
                }
                // the end: right after the RTI (or the WAI's end)
                var s = b.sim;
                if (wai) s.tick(wai_clocks);
                // (TIMEUP as saved: the timer may fire again after its read)
                if (s.frame == end_frame and s.vc == target_vc and s.hc == target_hc and s.timeup == (field(t, F.timeup) != 0)) {
                    // (a frame whose clock's phase is another: the next pair)
                    if (s.mc % 8 != field(t, F.mc8) and mutant != 14) return Error.TailTime;
                    const end: u16 = @intCast(b.rti_at);
                    built_ovl = b.ovl;
                    return .{ .end = end, .cycles = b.sim.cycles, .marks = b.marks, .nmarks = b.nmarks };
                }
                if (!before(&s, end_frame, target_vc, target_hc)) break;
            }
        }
    }
    return Error.TailTime;
}

fn opSpeed(a: u32, memsel: bool) u32 {
    const bank = a >> 16;
    const off = a & 0xFFFF;
    if (bank >= 0x40 and bank < 0x80) return 8;
    if (bank >= 0xC0) return if (memsel) 6 else 8;
    if (off < 0x2000) return 8;
    if (off < 0x4000) return 6;
    if (off < 0x4200) return 12;
    if (off < 0x6000) return 6;
    if (off < 0x8000) return 8;
    if (bank >= 0x80 and memsel) return 6;
    return 8;
}

fn tail(a: *Asm, t: []const u8, q: u32, s_plp: u32, ptr: u32, e: bool) void {
    _ = q;
    a.mark();
    a.rep(0x30);
    a.ldaImm(@truncate(field(t, F.A)));
    // S one below DBR's byte: PLB pulls it, then S is the PLP's
    a.ldxImm(@intCast((s_plp -% 1) & 0xFFFF));
    a.txs((s_plp -% 1) & 0xFFFF);
    a.plb();
    a.ldxImm(@truncate(field(t, F.X)));
    if (e) {
        a.sec();
        a.xce(true);
    }
    // PLP: op, two internal cycles, the pull (P, from the saved stack)
    a.opFetch(0x28, 1);
    a.sim.idle();
    a.sim.idle();
    a.s = if (e) 0x100 | ((a.s + 1) & 0xFF) else (a.s + 1) & 0xFFFF;
    a.sim.read(a.s);
    a.boundary();
    // JML [ptr]: op, the pointer's address, its three bytes (bank 00h)
    a.room(3);
    a.rti_at = a.pc;
    a.sim.read(a.pc);
    a.byte(0xDC);
    const pa: u32 = 0x8000 + ptr;
    a.operand(@intCast(pa & 0xFF));
    a.operand(@intCast(pa >> 8));
    a.sim.read(pa);
    a.sim.read(pa + 1);
    a.sim.read(pa + 2);
}

test "the table's size: 130 fields" {
    try std.testing.expectEqual(@as(usize, 130 * 4), table_size);
    try std.testing.expectEqual(@as(usize, 129), F.mc8);
}
