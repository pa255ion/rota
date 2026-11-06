//! m68k-sim: a 68000 program on the Clash core's Verilator model (side
//! track P, P.5; lib.zig), with m68k-run's options and its output, so the
//! two can be diffed (make m68k-sim-test does). The image is loaded at --at
//! (default 0) into 16 MiB of RAM; without --pc the CPU starts from the
//! reset vectors in it (SSP at 0, PC at 4), with --pc ADDR [--sp ADDR]
//! (default 0x8000) there (the reset's vector reads answered with them). It
//! runs until STOP, a halt, or the limit (--cycles N, default 10^9; or
//! --instructions N), then prints the counts and the registers.
//!
//!   m68k-sim prog.bin --at 0x1000 --pc 0x1000 --sp 0x8000

const std = @import("std");
const m68k = @import("m68k");

const Mem = struct {
    ram: []u8,
    // the reset's vectors from --pc/--sp (read before the first instruction)
    vec: ?[8]u8 = null,
};

fn read(user: ?*anyopaque, addr: u32, fc: u8, is_byte: bool) callconv(.c) u16 {
    _ = fc;
    const mem: *Mem = @ptrCast(@alignCast(user.?));
    const a = addr & 0xFF_FFFF;
    const b = struct {
        fn at(mm: *Mem, x: u32) u8 {
            if (mm.vec) |v| if (x < 8) return v[x];
            return mm.ram[x & 0xFF_FFFF];
        }
    };
    if (is_byte) return b.at(mem, a);
    return @as(u16, b.at(mem, a)) << 8 | b.at(mem, a + 1);
}

fn write(user: ?*anyopaque, addr: u32, fc: u8, is_byte: bool, value: u16) callconv(.c) void {
    _ = fc;
    const mem: *Mem = @ptrCast(@alignCast(user.?));
    const a = addr & 0xFF_FFFF;
    if (is_byte) {
        mem.ram[a] = @truncate(value);
    } else {
        mem.ram[a] = @truncate(value >> 8);
        mem.ram[(a + 1) & 0xFF_FFFF] = @truncate(value);
    }
}

fn usage() noreturn {
    std.debug.print("usage: m68k-sim IMAGE [--at ADDR] [--pc ADDR [--sp ADDR]] [--cycles N | --instructions N]\n", .{});
    std.process.exit(2);
}

fn num(s: []const u8) u64 {
    return std.fmt.parseInt(u64, s, 0) catch usage();
}

pub fn main(init: std.process.Init) !void {
    const io = init.io;
    const arena = init.arena.allocator();
    const args = try init.minimal.args.toSlice(arena);
    var image: ?[]const u8 = null;
    var at: u32 = 0;
    var pc: ?u32 = null;
    var sp: u32 = 0x8000;
    var cycles: u64 = 1_000_000_000;
    var instructions: ?u64 = null;
    var i: usize = 1;
    while (i < args.len) : (i += 1) {
        const a = args[i];
        if (std.mem.eql(u8, a, "--at") or std.mem.eql(u8, a, "--pc") or std.mem.eql(u8, a, "--sp") or
            std.mem.eql(u8, a, "--cycles") or std.mem.eql(u8, a, "--instructions"))
        {
            i += 1;
            if (i >= args.len) usage();
            const v = num(args[i]);
            if (std.mem.eql(u8, a, "--at")) at = @truncate(v) else if (std.mem.eql(u8, a, "--pc")) pc = @truncate(v) else if (std.mem.eql(u8, a, "--sp")) sp = @truncate(v) else if (std.mem.eql(u8, a, "--cycles")) cycles = v else instructions = v;
        } else if (image == null) image = a else usage();
    }
    const cwd = std.Io.Dir.cwd();
    const img = try cwd.readFileAlloc(io, image orelse usage(), arena, .limited(16 << 20));
    var mem = Mem{ .ram = try arena.alloc(u8, 16 << 20) };
    @memset(mem.ram, 0);
    for (img, 0..) |byte, k| mem.ram[(at + k) & 0xFF_FFFF] = byte;
    if (pc) |p| mem.vec = .{
        @truncate(sp >> 24), @truncate(sp >> 16), @truncate(sp >> 8), @truncate(sp),
        @truncate(p >> 24),  @truncate(p >> 16),  @truncate(p >> 8),  @truncate(p),
    };

    const cpu = m68k.m68k_new(read, write, &mem) orelse return error.OutOfMemory;
    defer m68k.m68k_free(cpu);
    m68k.m68k_reset(cpu);
    mem.vec = null;
    if (instructions) |n| {
        while (m68k.m68k_instructions(cpu) < n and !m68k.m68k_stopped(cpu) and !m68k.m68k_halted(cpu))
            _ = m68k.m68k_step(cpu);
    } else _ = m68k.m68k_run(cpu, cycles);

    const stop = if (m68k.m68k_halted(cpu)) "Halted" else if (m68k.m68k_stopped(cpu)) "Stopped" else "LimitReached";
    var buf: [256]u8 = undefined;
    var out = std.Io.File.stdout().writer(io, &buf);
    const w = &out.interface;
    try w.print("cycles {d}\ninstructions {d}\nstop {s}\n", .{ m68k.m68k_clocks(cpu), m68k.m68k_instructions(cpu), stop });
    for (0..8) |r| try w.print("{s}d{d}=0x{x}", .{ if (r == 0) "" else " ", r, m68k.m68k_reg(cpu, @intCast(r)) });
    try w.print("\n", .{});
    for (0..8) |r| try w.print("{s}a{d}=0x{x}", .{ if (r == 0) "" else " ", r, m68k.m68k_reg(cpu, @intCast(8 + r)) });
    try w.print("\npc=0x{x} sr=0x{x} usp=0x{x} ssp=0x{x}\n", .{ m68k.m68k_reg(cpu, 19), m68k.m68k_reg(cpu, 18), m68k.m68k_reg(cpu, 16), m68k.m68k_reg(cpu, 17) });
    try w.flush();
}
