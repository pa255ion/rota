//! A Z80 disassembler (side track R, R.2: rota-dbg's disassembly), in Zilog
//! syntax, hex with $, the undocumented instructions included (SLL, the
//! IXH/IXL halves, DDCB's register copies, ED's mirrors). Decoded by the
//! opcode's fields x (bits 7-6), y (5-3), z (2-0), p (y >> 1), q (y & 1),
//! as in "Decoding Z80 opcodes" (z80.info), which the tables follow.

const std = @import("std");

pub const Insn = struct {
    /// bytes it takes (1 to 4)
    len: u8,
    text: [32]u8 = undefined,
    text_len: u8 = 0,

    pub fn str(i: *const Insn) []const u8 {
        return i.text[0..i.text_len];
    }
};

/// The instruction at `pc` in `mem` (bytes from `pc` on; at least 4, or
/// what's left, read as 0 past its end).
pub fn decode(mem: []const u8, pc: u16) Insn {
    var d: Dec = .{ .mem = mem, .pc = pc };
    d.run();
    var out: Insn = .{ .len = d.pos };
    const t = d.w.buffered();
    @memcpy(out.text[0..t.len], t);
    out.text_len = @intCast(t.len);
    return out;
}

const r_names = [8][]const u8{ "B", "C", "D", "E", "H", "L", "(HL)", "A" };
const rp_names = [4][]const u8{ "BC", "DE", "HL", "SP" };
const rp2_names = [4][]const u8{ "BC", "DE", "HL", "AF" };
const cc_names = [8][]const u8{ "NZ", "Z", "NC", "C", "PO", "PE", "P", "M" };
const alu_names = [8][]const u8{ "ADD A,", "ADC A,", "SUB ", "SBC A,", "AND ", "XOR ", "OR ", "CP " };
const rot_names = [8][]const u8{ "RLC", "RRC", "RL", "RR", "SLA", "SRA", "SLL", "SRL" };
const im_names = [8][]const u8{ "0", "0", "1", "2", "0", "0", "1", "2" };
const bli_names = [4][4][]const u8{
    .{ "LDI", "CPI", "INI", "OUTI" },
    .{ "LDD", "CPD", "IND", "OUTD" },
    .{ "LDIR", "CPIR", "INIR", "OTIR" },
    .{ "LDDR", "CPDR", "INDR", "OTDR" },
};

const Index = enum { hl, ix, iy };

const Dec = struct {
    mem: []const u8,
    pc: u16,
    pos: u8 = 0,
    buf: [32]u8 = undefined,
    w: std.Io.Writer = undefined,
    ix: Index = .hl,
    /// DD/FD's displacement, read once (before the opcode for DDCB)
    disp: ?i8 = null,

    fn byte(d: *Dec) u8 {
        const v = if (d.pos < d.mem.len) d.mem[d.pos] else 0;
        d.pos += 1;
        return v;
    }

    fn word(d: *Dec) u16 {
        const lo: u16 = d.byte();
        return lo | (@as(u16, d.byte()) << 8);
    }

    fn put(d: *Dec, comptime fmt: []const u8, args: anytype) void {
        d.w.print(fmt, args) catch {};
    }

    fn run(d: *Dec) void {
        d.w = .fixed(&d.buf);
        var op = d.byte();
        // DD/FD prefixes: the last one counts
        while (op == 0xDD or op == 0xFD) {
            d.ix = if (op == 0xDD) .ix else .iy;
            op = d.byte();
        }
        switch (op) {
            0xCB => if (d.ix == .hl) d.cb(d.byte()) else {
                d.disp = @bitCast(d.byte());
                d.cb(d.byte());
            },
            0xED => {
                d.ix = .hl;
                d.ed(d.byte());
            },
            else => d.main(op),
        }
    }

    fn hlName(d: *Dec) []const u8 {
        return switch (d.ix) {
            .hl => "HL",
            .ix => "IX",
            .iy => "IY",
        };
    }

    /// (HL), or (IX+d) with its displacement read now
    fn memOperand(d: *Dec) void {
        if (d.ix == .hl) return d.put("(HL)", .{});
        const dv: i8 = d.disp orelse blk: {
            const v: i8 = @bitCast(d.byte());
            d.disp = v;
            break :blk v;
        };
        if (dv < 0) d.put("({s}-${X:0>2})", .{ d.hlName(), @as(u8, @intCast(-@as(i16, dv))) }) else d.put("({s}+${X:0>2})", .{ d.hlName(), @as(u8, @intCast(dv)) });
    }

    /// register r (H and L as IXH/IXL under a prefix, unless `plain`:
    /// beside an (IX+d) operand they stay H and L)
    fn reg(d: *Dec, r: u3, plain: bool) void {
        if (r == 6) return d.memOperand();
        if (d.ix != .hl and !plain and (r == 4 or r == 5)) {
            return d.put("{s}{s}", .{ d.hlName(), if (r == 4) "H" else "L" });
        }
        d.put("{s}", .{r_names[r]});
    }

    fn rp(d: *Dec, p: u2) []const u8 {
        return if (p == 2) d.hlName() else rp_names[p];
    }

    fn rp2(d: *Dec, p: u2) []const u8 {
        return if (p == 2) d.hlName() else rp2_names[p];
    }

    fn rel(d: *Dec) u16 {
        const e: i8 = @bitCast(d.byte());
        return d.pc +% d.pos +% @as(u16, @bitCast(@as(i16, e)));
    }

    fn main(d: *Dec, op: u8) void {
        const x: u2 = @intCast(op >> 6);
        const y: u3 = @intCast((op >> 3) & 7);
        const z: u3 = @intCast(op & 7);
        const p: u2 = @intCast(y >> 1);
        const q = y & 1 == 1;
        switch (x) {
            0 => switch (z) {
                0 => switch (y) {
                    0 => d.put("NOP", .{}),
                    1 => d.put("EX AF,AF'", .{}),
                    2 => d.put("DJNZ ${X:0>4}", .{d.rel()}),
                    3 => d.put("JR ${X:0>4}", .{d.rel()}),
                    else => d.put("JR {s},${X:0>4}", .{ cc_names[y - 4], d.rel() }),
                },
                1 => if (!q) d.put("LD {s},${X:0>4}", .{ d.rp(p), d.word() }) else d.put("ADD {s},{s}", .{ d.hlName(), d.rp(p) }),
                2 => switch (y) {
                    0 => d.put("LD (BC),A", .{}),
                    1 => d.put("LD A,(BC)", .{}),
                    2 => d.put("LD (DE),A", .{}),
                    3 => d.put("LD A,(DE)", .{}),
                    4 => d.put("LD (${X:0>4}),{s}", .{ d.word(), d.hlName() }),
                    5 => d.put("LD {s},(${X:0>4})", .{ d.hlName(), d.word() }),
                    6 => d.put("LD (${X:0>4}),A", .{d.word()}),
                    7 => d.put("LD A,(${X:0>4})", .{d.word()}),
                },
                3 => d.put("{s} {s}", .{ if (q) "DEC" else "INC", d.rp(p) }),
                4, 5 => {
                    d.put("{s} ", .{if (z == 4) "INC" else "DEC"});
                    d.reg(y, false);
                },
                6 => {
                    d.put("LD ", .{});
                    d.reg(y, false);
                    d.put(",${X:0>2}", .{d.byte()});
                },
                7 => d.put("{s}", .{([8][]const u8{ "RLCA", "RRCA", "RLA", "RRA", "DAA", "CPL", "SCF", "CCF" })[y]}),
            },
            1 => if (z == 6 and y == 6) d.put("HALT", .{}) else {
                // LD r,(IX+d) and LD (IX+d),r keep H and L as they are
                const mem = z == 6 or y == 6;
                d.put("LD ", .{});
                d.reg(y, mem);
                d.put(",", .{});
                d.reg(z, mem);
            },
            2 => {
                d.put("{s}", .{alu_names[y]});
                d.reg(z, false);
            },
            3 => switch (z) {
                0 => d.put("RET {s}", .{cc_names[y]}),
                1 => if (!q) d.put("POP {s}", .{d.rp2(p)}) else switch (p) {
                    0 => d.put("RET", .{}),
                    1 => d.put("EXX", .{}),
                    2 => d.put("JP ({s})", .{d.hlName()}),
                    3 => d.put("LD SP,{s}", .{d.hlName()}),
                },
                2 => d.put("JP {s},${X:0>4}", .{ cc_names[y], d.word() }),
                3 => switch (y) {
                    0 => d.put("JP ${X:0>4}", .{d.word()}),
                    1 => unreachable, // CB, taken before
                    2 => d.put("OUT (${X:0>2}),A", .{d.byte()}),
                    3 => d.put("IN A,(${X:0>2})", .{d.byte()}),
                    4 => d.put("EX (SP),{s}", .{d.hlName()}),
                    5 => d.put("EX DE,HL", .{}),
                    6 => d.put("DI", .{}),
                    7 => d.put("EI", .{}),
                },
                4 => d.put("CALL {s},${X:0>4}", .{ cc_names[y], d.word() }),
                5 => if (!q) d.put("PUSH {s}", .{d.rp2(p)}) else d.put("CALL ${X:0>4}", .{d.word()}),
                6 => d.put("{s}${X:0>2}", .{ alu_names[y], d.byte() }),
                7 => d.put("RST ${X:0>2}", .{@as(u8, y) * 8}),
            },
        }
    }

    fn cb(d: *Dec, op: u8) void {
        const x: u2 = @intCast(op >> 6);
        const y: u3 = @intCast((op >> 3) & 7);
        const z: u3 = @intCast(op & 7);
        switch (x) {
            0 => d.put("{s} ", .{rot_names[y]}),
            1 => d.put("BIT {d},", .{y}),
            2 => d.put("RES {d},", .{y}),
            3 => d.put("SET {d},", .{y}),
        }
        if (d.ix == .hl) return d.reg(z, false);
        // DDCB/FDCB: the operand (IX+d); a register besides z != 6 gets a
        // copy of the result (undocumented; BIT has none)
        d.memOperand();
        if (z != 6 and x != 1) d.put(",{s}", .{r_names[z]});
    }

    fn ed(d: *Dec, op: u8) void {
        const x: u2 = @intCast(op >> 6);
        const y: u3 = @intCast((op >> 3) & 7);
        const z: u3 = @intCast(op & 7);
        const p: u2 = @intCast(y >> 1);
        const q = y & 1 == 1;
        if (x == 1) switch (z) {
            0 => if (y == 6) d.put("IN (C)", .{}) else d.put("IN {s},(C)", .{r_names[y]}),
            1 => if (y == 6) d.put("OUT (C),0", .{}) else d.put("OUT (C),{s}", .{r_names[y]}),
            2 => d.put("{s} HL,{s}", .{ if (q) "ADC" else "SBC", rp_names[p] }),
            3 => if (!q) d.put("LD (${X:0>4}),{s}", .{ d.word(), rp_names[p] }) else d.put("LD {s},(${X:0>4})", .{ rp_names[p], d.word() }),
            4 => d.put("NEG", .{}),
            5 => d.put("{s}", .{if (y == 1) "RETI" else "RETN"}),
            6 => d.put("IM {s}", .{im_names[y]}),
            7 => d.put("{s}", .{([8][]const u8{ "LD I,A", "LD R,A", "LD A,I", "LD A,R", "RRD", "RLD", "NOP", "NOP" })[y]}),
        } else if (x == 2 and z <= 3 and y >= 4) {
            d.put("{s}", .{bli_names[y - 4][z]});
        } else {
            // the rest of ED's page: no instruction (two-byte NOPs)
            d.put("NOP*", .{});
        }
    }
};

fn expectDis(bytes: []const u8, want: []const u8, len: u8) !void {
    const i = decode(bytes, 0x1000);
    try std.testing.expectEqualStrings(want, i.str());
    try std.testing.expectEqual(len, i.len);
}

test "the base page" {
    try expectDis(&.{0x00}, "NOP", 1);
    try expectDis(&.{ 0x01, 0x34, 0x12 }, "LD BC,$1234", 3);
    try expectDis(&.{ 0x3E, 0x7F }, "LD A,$7F", 2);
    try expectDis(&.{0x76}, "HALT", 1);
    try expectDis(&.{0x7E}, "LD A,(HL)", 1);
    try expectDis(&.{0x86}, "ADD A,(HL)", 1);
    try expectDis(&.{ 0xFE, 0x10 }, "CP $10", 2);
    try expectDis(&.{ 0xC3, 0x00, 0x80 }, "JP $8000", 3);
    try expectDis(&.{ 0xCD, 0x38, 0x00 }, "CALL $0038", 3);
    try expectDis(&.{ 0x20, 0xFE }, "JR NZ,$1000", 2); // a loop on itself
    try expectDis(&.{ 0x10, 0x02 }, "DJNZ $1004", 2);
    try expectDis(&.{0xFF}, "RST $38", 1);
    try expectDis(&.{0xE3}, "EX (SP),HL", 1);
    try expectDis(&.{ 0xD3, 0xBF }, "OUT ($BF),A", 2);
    try expectDis(&.{0x08}, "EX AF,AF'", 1);
    try expectDis(&.{ 0x32, 0xFF, 0xDF }, "LD ($DFFF),A", 3);
    try expectDis(&.{0xF5}, "PUSH AF", 1);
}

test "CB, ED" {
    try expectDis(&.{ 0xCB, 0x7F }, "BIT 7,A", 2);
    try expectDis(&.{ 0xCB, 0x36 }, "SLL (HL)", 2);
    try expectDis(&.{ 0xCB, 0xC6 }, "SET 0,(HL)", 2);
    try expectDis(&.{ 0xED, 0xB0 }, "LDIR", 2);
    try expectDis(&.{ 0xED, 0x56 }, "IM 1", 2);
    try expectDis(&.{ 0xED, 0x4D }, "RETI", 2);
    try expectDis(&.{ 0xED, 0x52 }, "SBC HL,DE", 2);
    try expectDis(&.{ 0xED, 0x73, 0x00, 0xC0 }, "LD ($C000),SP", 4);
    try expectDis(&.{ 0xED, 0x78 }, "IN A,(C)", 2);
    try expectDis(&.{ 0xED, 0x71 }, "OUT (C),0", 2);
    try expectDis(&.{ 0xED, 0x00 }, "NOP*", 2);
}

test "DD/FD: IX and IY, their halves, displacements, DDCB" {
    try expectDis(&.{ 0xDD, 0x21, 0x00, 0xC0 }, "LD IX,$C000", 4);
    try expectDis(&.{ 0xDD, 0x7E, 0x05 }, "LD A,(IX+$05)", 3);
    try expectDis(&.{ 0xFD, 0x77, 0xFE }, "LD (IY-$02),A", 3);
    try expectDis(&.{ 0xDD, 0x66, 0x01 }, "LD H,(IX+$01)", 3); // H stays H
    try expectDis(&.{ 0xDD, 0x7C }, "LD A,IXH", 2);
    try expectDis(&.{ 0xFD, 0x85 }, "ADD A,IYL", 2);
    try expectDis(&.{ 0xDD, 0x36, 0x02, 0x55 }, "LD (IX+$02),$55", 4);
    try expectDis(&.{ 0xDD, 0xE9 }, "JP (IX)", 2);
    try expectDis(&.{ 0xDD, 0xCB, 0x03, 0x46 }, "BIT 0,(IX+$03)", 4);
    try expectDis(&.{ 0xFD, 0xCB, 0xFF, 0xC6 }, "SET 0,(IY-$01)", 4);
    try expectDis(&.{ 0xDD, 0xCB, 0x00, 0x00 }, "RLC (IX+$00),B", 4);
    try expectDis(&.{ 0xDD, 0x09 }, "ADD IX,BC", 2);
    try expectDis(&.{ 0xDD, 0xED, 0xB0 }, "LDIR", 3); // ED drops the prefix
}
