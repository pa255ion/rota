//! I2C slave for the layer 4 harness: the simulated ADV7511's register
//! interface, seen on the wires one clock at a time. The protocol handling
//! is a port of the Haskell slave model (platform/src/Platform/Sim/I2c.hs),
//! and the power-on registers are those of Platform.Sim.Adv7511
//! (`chipDefaults`, HPD and monitor sense always high); change them
//! together.
//!
//! A write transaction sets the register pointer with its first data byte
//! and writes the bytes after it (pointer incrementing); a read returns
//! bytes from the pointer until the master NACKs. Writes are logged.

const std = @import("std");

pub const address: u8 = 0x39; // 7-bit, PD/AD low (Platform.Video.Adv7511.Table)

pub const Write = struct { reg: u8, value: u8 };

const Target = enum { addr, ptr, data };

const Mode = union(enum) {
    idle,
    recv: struct { target: Target, bits: u4, value: u8 },
    ack: struct { acked: bool, next: ?Target }, // next == null: send
    send: struct { bit: u3, value: u8 },
    master_ack: ?bool,
};

pub const Slave = struct {
    regs: [256]u8,
    ptr: u8 = 0,
    mode: Mode = .idle,
    prev_scl: u1 = 1,
    prev_sda: u1 = 1,
    /// Pulls SDA low in the next clock.
    sda_low: bool = false,

    log: [4096]Write = undefined,
    log_len: usize = 0,
    starts: u64 = 0,
    framing_errors: u64 = 0,

    pub fn init() Slave {
        var s: Slave = .{ .regs = [_]u8{0} ** 256 };
        s.regs[0x00] = 0x14; // revision (chipRevision)
        s.regs[0x41] = 0x50; // powered down
        s.regs[0x42] = 0x60; // HPD and monitor sense high
        return s;
    }

    /// One clock, seeing the bus (wired AND of master and slave) as it is
    /// in this clock.
    pub fn step(s: *Slave, scl: u1, sda: u1) void {
        defer {
            s.prev_scl = scl;
            s.prev_sda = sda;
        }
        if (scl == 1 and s.prev_scl == 1 and s.prev_sda == 1 and sda == 0) { // START
            s.mode = .{ .recv = .{ .target = .addr, .bits = 0, .value = 0 } };
            s.sda_low = false;
            s.starts += 1;
            return;
        }
        if (scl == 1 and s.prev_scl == 1 and s.prev_sda == 0 and sda == 1) { // STOP
            s.mode = .idle;
            s.sda_low = false;
            return;
        }
        if (s.prev_scl == 0 and scl == 1) s.rising(sda);
        if (s.prev_scl == 1 and scl == 0) s.falling();
    }

    fn rising(s: *Slave, sda: u1) void {
        switch (s.mode) {
            .recv => |*r| {
                if (r.bits == 8) {
                    s.framing_errors += 1;
                    return;
                }
                r.value = (r.value << 1) | sda;
                r.bits += 1;
            },
            .master_ack => |*a| if (a.* == null) {
                a.* = sda == 0;
            },
            else => {},
        }
    }

    fn falling(s: *Slave) void {
        switch (s.mode) {
            .recv => |r| if (r.bits == 8) s.byteDone(r.target, r.value),
            .ack => |a| {
                if (!a.acked) {
                    s.mode = .idle;
                    s.sda_low = false;
                } else if (a.next) |t| {
                    s.mode = .{ .recv = .{ .target = t, .bits = 0, .value = 0 } };
                    s.sda_low = false;
                } else s.startSend();
            },
            .send => |*d| {
                if (d.bit == 7) {
                    s.mode = .{ .master_ack = null };
                    s.sda_low = false;
                } else {
                    d.bit += 1;
                    s.sda_low = (d.value >> (7 - @as(u3, d.bit))) & 1 == 0;
                }
            },
            .master_ack => |a| {
                if (a == true) s.startSend() else {
                    s.mode = .idle;
                    s.sda_low = false;
                }
            },
            .idle => {},
        }
    }

    fn byteDone(s: *Slave, target: Target, v: u8) void {
        switch (target) {
            .addr => {
                if (v >> 1 != address) {
                    s.mode = .{ .ack = .{ .acked = false, .next = null } };
                    return;
                }
                s.ackWith(if (v & 1 == 1) null else .ptr);
            },
            .ptr => {
                s.ptr = v;
                s.ackWith(.data);
            },
            .data => {
                if (s.ptr != 0x42 and s.ptr != 0x00) s.regs[s.ptr] = v; // read-only: status, revision
                if (s.log_len < s.log.len) {
                    s.log[s.log_len] = .{ .reg = s.ptr, .value = v };
                    s.log_len += 1;
                }
                s.ptr +%= 1;
                s.ackWith(.data);
            },
        }
    }

    fn ackWith(s: *Slave, next: ?Target) void {
        s.mode = .{ .ack = .{ .acked = true, .next = next } };
        s.sda_low = true;
    }

    fn startSend(s: *Slave) void {
        const b = s.regs[s.ptr];
        s.mode = .{ .send = .{ .bit = 0, .value = b } };
        s.sda_low = b & 0x80 == 0;
        s.ptr +%= 1;
    }

    pub fn writes(s: *const Slave) []const Write {
        return s.log[0..s.log_len];
    }
};

// A tiny bit-banged master for the tests: each call is one clock.
const TestBus = struct {
    s: Slave = Slave.init(),
    fn put(b: *TestBus, scl: u1, sda: u1) u1 {
        const line: u1 = if (sda == 0 or b.s.sda_low) 0 else 1;
        b.s.step(scl, line);
        return line;
    }
    fn start(b: *TestBus) void {
        _ = b.put(1, 1);
        _ = b.put(1, 0);
        _ = b.put(0, 0);
    }
    fn stop(b: *TestBus) void {
        _ = b.put(0, 0);
        _ = b.put(1, 0);
        _ = b.put(1, 1);
    }
    fn bit(b: *TestBus, v: u1) u1 {
        _ = b.put(0, v);
        const got = b.put(1, v);
        _ = b.put(0, v);
        return got;
    }
    fn byte(b: *TestBus, v: u8) bool {
        var i: u4 = 0;
        while (i < 8) : (i += 1) _ = b.bit(@intCast((v >> @intCast(7 - i)) & 1));
        return b.bit(1) == 0;
    }
    fn readByte(b: *TestBus, ack: bool) u8 {
        var v: u8 = 0;
        var i: u4 = 0;
        while (i < 8) : (i += 1) v = (v << 1) | b.bit(1);
        _ = b.bit(if (ack) 0 else 1);
        return v;
    }
};

test "write two registers, read one back, wrong address NACKed" {
    var b: TestBus = .{};
    b.start();
    try std.testing.expect(b.byte(address << 1));
    try std.testing.expect(b.byte(0x98));
    try std.testing.expect(b.byte(0x03));
    try std.testing.expect(b.byte(0x04)); // 0x99
    b.stop();
    b.start();
    try std.testing.expect(b.byte(address << 1));
    try std.testing.expect(b.byte(0x41));
    b.start(); // repeated
    try std.testing.expect(b.byte((address << 1) | 1));
    try std.testing.expectEqual(@as(u8, 0x50), b.readByte(false));
    b.stop();
    b.start();
    try std.testing.expect(!b.byte(0x10 << 1));
    b.stop();
    try std.testing.expectEqualSlices(Write, &.{ .{ .reg = 0x98, .value = 0x03 }, .{ .reg = 0x99, .value = 0x04 } }, b.s.writes());
    try std.testing.expectEqual(@as(u64, 0), b.s.framing_errors);
}
