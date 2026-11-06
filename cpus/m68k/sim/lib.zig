//! The Clash 68000 as a library (side track P, P.5; cpus/m68k/sim/m68k.h):
//! Verilator's model of m68k_top (vm68k.cpp) clocked here, its bus
//! transactions answered by the caller's callbacks, clocks and instructions
//! counted from the first instruction after the reset. For the user's
//! benchmark project, and for m68k-sim (main.zig). No wait states: DTACK is
//! asserted on every bus cycle, so a transaction takes the chip's clocks.

const std = @import("std");

const Vm = opaque {};
const Out = extern struct {
    kind: u8,
    fc: u8,
    is_byte: u8,
    done: u8,
    istart: u8,
    stopped: u8,
    halted: u8,
    pad: u8,
    addr: u32,
    dout: u32,
    dbg: u32,
};
extern fn vm68k_new() *Vm;
extern fn vm68k_free(v: *Vm) void;
extern fn vm68k_peek(v: *Vm, dtack_n: c_int, berr_n: c_int, vpa_n: c_int, ipl_n: c_int, dbg_sel: c_int, o: *Out) void;
extern fn vm68k_tick(v: *Vm, rst: c_int, dtack_n: c_int, berr_n: c_int, vpa_n: c_int, ipl_n: c_int, din: u16) void;

/// A read (a byte: in the low 8 bits) at a 24-bit address, its function code.
pub const ReadFn = *const fn (user: ?*anyopaque, addr: u32, fc: u8, is_byte: bool) callconv(.c) u16;
/// A write (a byte: the low 8 bits).
pub const WriteFn = *const fn (user: ?*anyopaque, addr: u32, fc: u8, is_byte: bool, value: u16) callconv(.c) void;
/// The interrupt acknowledge for a level: the vector number, or -1 for the
/// autovector (VPA).
pub const IackFn = *const fn (user: ?*anyopaque, level: u8) callconv(.c) i32;

pub const M68k = struct {
    vm: *Vm,
    read: ReadFn,
    write: WriteFn,
    iack: ?IackFn = null,
    user: ?*anyopaque,
    ipl: u8 = 0,
    clocks: u64 = 0,
    instructions: u64 = 0,
};

// the debug port's registers (M68kTop.hs)
const reg_sr = 18;

pub export fn m68k_new(read: ReadFn, write: WriteFn, user: ?*anyopaque) ?*M68k {
    const m = std.heap.c_allocator.create(M68k) catch return null;
    m.* = .{ .vm = vm68k_new(), .read = read, .write = write, .user = user };
    return m;
}

pub export fn m68k_free(m: *M68k) void {
    vm68k_free(m.vm);
    std.heap.c_allocator.destroy(m);
}

pub export fn m68k_set_iack(m: *M68k, f: ?IackFn) void {
    m.iack = f;
}

/// The interrupt level on IPL2-IPL0 (0 none, 7 non-maskable).
pub export fn m68k_set_ipl(m: *M68k, level: u8) void {
    m.ipl = level & 7;
}

fn iplN(m: *M68k) c_int {
    return @as(c_int, ~m.ipl & 7);
}

fn peek(m: *M68k, sel: c_int) Out {
    var o: Out = undefined;
    vm68k_peek(m.vm, 0, 1, 1, iplN(m), sel, &o);
    return o;
}

/// One clock: a transaction that ends on it answered through the callbacks.
fn clock(m: *M68k, o: Out) void {
    var din: u16 = 0;
    var vpa_n: c_int = 1;
    if (o.done != 0) {
        const byte = o.is_byte != 0;
        switch (o.kind) {
            0 => din = m.read(m.user, o.addr, o.fc, byte),
            1 => m.write(m.user, o.addr, o.fc, byte, @truncate(o.dout)),
            2 => { // TAS: the byte read, written back with bit 7 set
                din = m.read(m.user, o.addr, o.fc, true) & 0xFF;
                m.write(m.user, o.addr, o.fc, true, din | 0x80);
            },
            4 => {
                const level: u8 = @truncate((o.addr >> 1) & 7);
                const v: i32 = if (m.iack) |f| f(m.user, level) else -1;
                if (v < 0) vpa_n = 0 else din = @truncate(@as(u32, @bitCast(v)) & 0xFF);
            },
            else => {},
        }
    }
    vm68k_tick(m.vm, 0, 0, 1, vpa_n, iplN(m), din);
}

/// The reset (SSP and PC from the vectors at 0 and 4, read through the
/// callbacks), up to the first instruction; the counts start there.
pub export fn m68k_reset(m: *M68k) void {
    vm68k_tick(m.vm, 1, 1, 1, 1, iplN(m), 0);
    vm68k_tick(m.vm, 1, 1, 1, 1, iplN(m), 0);
    var n: u32 = 0;
    while (n < 1000) : (n += 1) {
        const o = peek(m, 0);
        if (o.istart != 0) break;
        clock(m, o);
    }
    m.clocks = 0;
    m.instructions = 0;
}

/// Stopped (STOP) with no interrupt to wake it: the level not above SR's
/// mask (level 7 always wakes it once).
fn asleep(m: *M68k, o: Out) bool {
    if (o.stopped == 0) return false;
    const mask: u8 = @truncate((peek(m, reg_sr).dbg >> 8) & 7);
    return m.ipl == 0 or (m.ipl < 7 and m.ipl <= mask);
}

/// Run until the next instruction starts (an interrupt's or a trace's
/// processing counted with the instruction before it), the CPU stops or
/// halts: the clocks taken.
pub export fn m68k_step(m: *M68k) u64 {
    const c0 = m.clocks;
    var first = true;
    while (true) {
        const o = peek(m, 0);
        if (o.halted != 0 or asleep(m, o)) break;
        if (o.istart != 0) {
            if (!first) break;
            m.instructions += 1;
        }
        first = false;
        clock(m, o);
        m.clocks += 1;
    }
    return m.clocks - c0;
}

/// Run until the CPU stops (STOP, nothing to wake it) or halts, or until
/// the first instruction start at or past @limit clocks from now (the
/// instruction that crosses it run whole): the clocks taken.
pub export fn m68k_run(m: *M68k, limit: u64) u64 {
    const c0 = m.clocks;
    while (true) {
        const o = peek(m, 0);
        if (o.halted != 0 or asleep(m, o)) break;
        if (o.istart != 0) {
            if (m.clocks - c0 >= limit) break;
            m.instructions += 1;
        }
        clock(m, o);
        m.clocks += 1;
    }
    return m.clocks - c0;
}

pub export fn m68k_clocks(m: *M68k) u64 {
    return m.clocks;
}

pub export fn m68k_instructions(m: *M68k) u64 {
    return m.instructions;
}

pub export fn m68k_stopped(m: *M68k) bool {
    return peek(m, 0).stopped != 0;
}

pub export fn m68k_halted(m: *M68k) bool {
    return peek(m, 0).halted != 0;
}

/// A register: 0-7 D0-D7, 8-15 A0-A7 (A7 the active stack pointer), 16
/// USP, 17 SSP, 18 SR, 19 PC, 20 IR.
pub export fn m68k_reg(m: *M68k, n: u8) u32 {
    return peek(m, n).dbg;
}
