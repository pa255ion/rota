//! The PS side of the PS <-> PL link (the PL side and the line layout:
//! platform/src/Platform/Ps/Link.hs). PS -> PL, EMIO GPIO 0 to 31: bits 0
//! to 15 system-specific (CHIP-8 keys), 16 to 23 a data byte, 24 strobe
//! (toggled once per byte), 25 address byte, 26 hold. PL -> PS, EMIO GPIO
//! 32 to 63: buttons (bits 0 to 4: C, D, L, R, U), switches (8 to 15),
//! system ID (16 to 23), and the byte channel: a byte (24 to 31) and its
//! toggle (bit 5), which flips once per byte (`ByteIn`, `Lines`).
//!
//! Generic over the GPIO port (`write(u32)`, `read() u32`, `delayUs(u32)`):
//! hw.GpioPort on the board, a model of the PL decoder in the tests.

/// Shared addresses (Platform.Ps.Link).
pub const overlay_base: u24 = 0xF0_0000;
pub const control_addr: u24 = 0xF0_1000;
/// The settings registers (side track L, `PsSettings`; options.zig): the
/// volume, then the speed at the next address.
pub const volume_addr: u24 = 0xF0_1001;
pub const speed_addr: u24 = 0xF0_1002;
/// The movie register (side track O, Platform.Ps.FrameInput): bit 0, the
/// input word latched at each frame start.
pub const movie_addr: u24 = 0xF0_1003;
/// The capture register (side track O, O.4, Platform.Video.Capture): a
/// sequence number (1 to 255) asks for a screenshot.
pub const capture_addr: u24 = 0xF0_1004;
pub const cols = 40;
pub const rows = 30;

const bit_strobe: u32 = 1 << 24;
const bit_addr: u32 = 1 << 25;
const bit_hold: u32 = 1 << 26;

pub const Buttons = packed struct(u5) { c: bool, d: bool, l: bool, r: bool, u: bool };

pub const Inputs = struct {
    buttons: Buttons,
    switches: u8,
    system: u8,
    /// the byte channel: the last byte, and its toggle
    byte: u8,
    toggle: bool,
    /// bits 6 and 7: the system's own flags (the Game Boy: bit 0 here,
    /// PS_IN bit 6, cartridge RAM written since the PS cleared it)
    flags: u2 = 0,

    pub fn decode(w: u32) Inputs {
        return .{
            .buttons = @bitCast(@as(u5, @truncate(w))),
            .switches = @truncate(w >> 8),
            .system = @truncate(w >> 16),
            .byte = @truncate(w >> 24),
            .toggle = w & (1 << 5) != 0,
            .flags = @truncate(w >> 6),
        };
    }

    /// PS_IN bit 7: flips at each of the core's frame starts (side track
    /// O; 0 in a MOVIES=0 build).
    pub fn frame(i: Inputs) bool {
        return i.flags & 2 != 0;
    }
};

/// The byte channel's receiver: a byte each time the toggle flips. The
/// first look only learns the toggle (a byte sent before the PS app
/// started is not news).
pub const ByteIn = struct {
    toggle: ?bool = null,

    pub fn poll(b: *ByteIn, in: Inputs) ?u8 {
        const t = b.toggle orelse {
            b.toggle = in.toggle;
            return null;
        };
        if (in.toggle == t) return null;
        b.toggle = in.toggle;
        return in.byte;
    }
};

/// Bytes into lines of text for the desktop: a line ends at '\n' (a '\r'
/// is dropped), when it is full, or by `flush` (the relay flushes a part
/// line after a pause). Other control bytes show as '.'.
pub const Lines = struct {
    buf: [64]u8 = undefined,
    len: usize = 0,

    /// Add a byte; a finished line, if this ended one (valid until the
    /// next call).
    pub fn add(l: *Lines, c: u8) ?[]const u8 {
        if (c == '\r') return null;
        if (c == '\n') return l.take();
        l.buf[l.len] = if (c >= 0x20 and c < 0x7F) c else '.';
        l.len += 1;
        if (l.len == l.buf.len) return l.take();
        return null;
    }

    pub fn flush(l: *Lines) ?[]const u8 {
        return if (l.len == 0) null else l.take();
    }

    fn take(l: *Lines) []const u8 {
        const n = l.len;
        l.len = 0;
        return l.buf[0..n];
    }
};

/// The control register: bit 0 the overlay, bit 1 the pause, bit 2 a
/// frame-advance step (side track O, O.3: a write with it runs the paused
/// core to its next frame start; it doesn't stay set).
pub const Control = struct { overlay: bool = false, pause: bool = false, step: bool = false };

pub fn Link(comptime Port: type) type {
    return struct {
        const Self = @This();
        port: *Port,
        /// Everything on the PS -> PL lines; written as a whole.
        out: u32 = 0,

        fn put(self: *Self, v: u32) void {
            self.out = v;
            self.port.write(v);
        }

        pub fn setUser(self: *Self, v: u16) void {
            self.put((self.out & 0xFFFF_0000) | v);
        }

        pub fn setHold(self: *Self, on: bool) void {
            self.put(if (on) self.out | bit_hold else self.out & ~bit_hold);
        }

        /// One byte: data (and the address flag) first, then 2 us later the
        /// strobe toggle, then 2 us more, so the PL's synchronised strobe
        /// edge (2 to 3 clocks of 40 ns) always sees settled data.
        fn strobe(self: *Self, b: u8, is_addr: bool) void {
            var v = (self.out & ~(@as(u32, 0xFF) << 16 | bit_addr)) | @as(u32, b) << 16;
            if (is_addr) v |= bit_addr;
            self.put(v);
            self.port.delayUs(2);
            self.put(v ^ bit_strobe);
            self.port.delayUs(2);
        }

        pub fn setAddr(self: *Self, addr: u24) void {
            self.strobe(@truncate(addr >> 16), true);
            self.strobe(@truncate(addr >> 8), true);
            self.strobe(@truncate(addr), true);
        }

        /// Data bytes at the current address (it advances by one per byte).
        pub fn data(self: *Self, bytes: []const u8) void {
            for (bytes) |b| self.strobe(b, false);
        }

        pub fn write(self: *Self, addr: u24, bytes: []const u8) void {
            self.setAddr(addr);
            self.data(bytes);
        }

        pub fn fill(self: *Self, addr: u24, b: u8, n: u32) void {
            self.setAddr(addr);
            var i: u32 = 0;
            while (i < n) : (i += 1) self.strobe(b, false);
        }

        pub fn control(self: *Self, c: Control) void {
            self.write(control_addr, &.{@as(u8, @intFromBool(c.overlay)) | @as(u8, @intFromBool(c.pause)) << 1 | @as(u8, @intFromBool(c.step)) << 2});
        }

        /// The movie register's sync bit (side track O): on, the core sees
        /// the input word latched at each of its frame starts.
        pub fn movieSync(self: *Self, on: bool) void {
            self.write(movie_addr, &.{@intFromBool(on)});
        }

        /// The volume and the speed (options.zig's `Settings`), in one
        /// write (the address advances). Takes any struct with the two
        /// fields: pl.zig is also a module of its own (the harness's), so
        /// it doesn't import options.zig.
        pub fn settings(self: *Self, o: anytype) void {
            self.write(volume_addr, &.{ @as(u8, o.volume), @as(u8, o.speed) });
        }

        /// Read until two reads agree: the PS samples each EMIO bit
        /// through its own synchroniser, so a read right at a change could
        /// mix the old byte with the new toggle (they change at most every
        /// millisecond or so).
        pub fn inputs(self: *Self) Inputs {
            var w = self.port.read();
            while (true) {
                const v = self.port.read();
                if (v == w) return Inputs.decode(v);
                w = v;
            }
        }
    };
}

// --- Tests: a model of the PL decoder (Platform.Ps.Link.linkStep) --------------
const std = @import("std");
const testing = std.testing;

const ModelPl = struct {
    strobe_level: bool = false,
    addr: u24 = 0,
    mem: std.AutoHashMapUnmanaged(u24, u8) = .empty,
    last: u32 = 0,
    in_word: u32 = 0,

    pub fn write(m: *ModelPl, v: u32) void {
        m.last = v;
        const st = v & bit_strobe != 0;
        if (st == m.strobe_level) return;
        m.strobe_level = st;
        const d: u8 = @truncate(v >> 16);
        if (v & bit_addr != 0) {
            m.addr = (m.addr << 8) | d;
        } else {
            m.mem.put(testing.allocator, m.addr, d) catch unreachable;
            m.addr +%= 1;
        }
    }
    pub fn read(m: *ModelPl) u32 {
        return m.in_word;
    }
    pub fn delayUs(_: *ModelPl, _: u32) void {}
};

test "writes, fills and control arrive at their addresses" {
    var m: ModelPl = .{};
    defer m.mem.deinit(testing.allocator);
    var l: Link(ModelPl) = .{ .port = &m };
    l.write(0x000200, "ABC");
    l.fill(overlay_base + 1198, ' ', 2);
    l.control(.{ .overlay = true, .pause = true });
    try testing.expectEqual(@as(u8, 'A'), m.mem.get(0x200).?);
    try testing.expectEqual(@as(u8, 'C'), m.mem.get(0x202).?);
    try testing.expectEqual(@as(u8, ' '), m.mem.get(overlay_base + 1199).?);
    try testing.expectEqual(@as(u8, 3), m.mem.get(control_addr).?);
    try testing.expectEqual(@as(usize, 6), m.mem.count());
}

test "control: a frame-advance step is bit 2, beside the pause" {
    var m: ModelPl = .{};
    defer m.mem.deinit(testing.allocator);
    var l: Link(ModelPl) = .{ .port = &m };
    l.control(.{ .pause = true, .step = true });
    try testing.expectEqual(@as(u8, 6), m.mem.get(control_addr).?);
}

test "the movie register; the frame toggle in PS_IN bit 7" {
    var m: ModelPl = .{};
    defer m.mem.deinit(testing.allocator);
    var l: Link(ModelPl) = .{ .port = &m };
    l.movieSync(true);
    try testing.expectEqual(@as(u8, 1), m.mem.get(movie_addr).?);
    l.movieSync(false);
    try testing.expectEqual(@as(u8, 0), m.mem.get(movie_addr).?);
    try testing.expect(Inputs.decode(0x0F020080).frame());
    try testing.expect(!Inputs.decode(0x0F020040).frame());
}

test "settings: volume and speed at their own addresses" {
    var m: ModelPl = .{};
    defer m.mem.deinit(testing.allocator);
    var l: Link(ModelPl) = .{ .port = &m };
    l.settings(.{ .volume = @as(u4, 7), .speed = @as(u4, 3) });
    try testing.expectEqual(@as(u8, 7), m.mem.get(volume_addr).?);
    try testing.expectEqual(@as(u8, 3), m.mem.get(speed_addr).?);
    try testing.expectEqual(@as(usize, 2), m.mem.count());
}

test "keys and hold leave the link lines alone, and the other way round" {
    var m: ModelPl = .{};
    defer m.mem.deinit(testing.allocator);
    var l: Link(ModelPl) = .{ .port = &m };
    l.setUser(0xBEEF);
    l.setHold(true);
    l.write(0x000300, "x");
    try testing.expectEqual(@as(u32, 0xBEEF), m.last & 0xFFFF);
    try testing.expect(m.last & bit_hold != 0);
    l.setHold(false);
    l.setUser(0);
    try testing.expectEqual(@as(u32, 0), m.last & (0xFFFF | bit_hold));
    try testing.expectEqual(@as(u8, 'x'), m.mem.get(0x300).?);
}

test "inputs decode" {
    const i = Inputs.decode(0x0001A519);
    try testing.expect(i.buttons.c and !i.buttons.d and !i.buttons.l and i.buttons.r and i.buttons.u);
    try testing.expectEqual(@as(u8, 0xA5), i.switches);
    try testing.expectEqual(@as(u8, 1), i.system);
    try testing.expect(!i.toggle);
    // the same word as Platform.Ps.Link's psInWith test
    const g = Inputs.decode(0x50020120);
    try testing.expectEqual(@as(u8, 0x50), g.byte);
    try testing.expect(g.toggle);
    try testing.expectEqual(@as(u8, 2), g.system);
    try testing.expectEqual(@as(u5, 0), @as(u5, @bitCast(g.buttons)));
    try testing.expectEqual(@as(u2, 0), g.flags);
    // the Game Boy's save test's PS_IN at the end (ci-gb): dirty flag set
    try testing.expectEqual(@as(u2, 1), Inputs.decode(0x0F020040).flags);
}

test "byte channel: one byte per toggle flip, nothing at the first look" {
    var b: ByteIn = .{};
    const w = struct {
        fn f(byte: u8, t: bool) Inputs {
            return Inputs.decode(@as(u32, byte) << 24 | (if (t) @as(u32, 1 << 5) else 0));
        }
    }.f;
    try testing.expectEqual(@as(?u8, null), b.poll(w('x', true))); // learns
    try testing.expectEqual(@as(?u8, null), b.poll(w('x', true)));
    try testing.expectEqual(@as(?u8, 'P'), b.poll(w('P', false)));
    try testing.expectEqual(@as(?u8, null), b.poll(w('P', false)));
    try testing.expectEqual(@as(?u8, 'a'), b.poll(w('a', true)));
    try testing.expectEqual(@as(?u8, 'a'), b.poll(w('a', false))); // the same byte twice
}

test "lines: newline ends one, CR dropped, full lines split, flush" {
    var l: Lines = .{};
    for ("Pass") |c| try testing.expectEqual(@as(?[]const u8, null), l.add(c));
    try testing.expectEqual(@as(?[]const u8, null), l.add('\r'));
    try testing.expectEqualStrings("Pass", l.add('\n').?);
    try testing.expectEqualStrings("", l.add('\n').?);
    _ = l.add(1);
    try testing.expectEqualStrings(".", l.flush().?);
    try testing.expectEqual(@as(?[]const u8, null), l.flush());
    var n: usize = 0;
    for (0..64) |_| {
        if (l.add('z')) |line| n = line.len;
    }
    try testing.expectEqual(@as(usize, 64), n);
}

test "inputs: reads until two agree" {
    const Flaky = struct {
        words: []const u32,
        i: usize = 0,
        pub fn read(f: *@This()) u32 {
            const w = f.words[@min(f.i, f.words.len - 1)];
            f.i += 1;
            return w;
        }
        pub fn write(_: *@This(), _: u32) void {}
        pub fn delayUs(_: *@This(), _: u32) void {}
    };
    // a torn read (new toggle, old byte), then the settled word
    var f: Flaky = .{ .words = &.{ 0x41020120, 0x42020100, 0x42020100 } };
    var l: Link(Flaky) = .{ .port = &f };
    const in = l.inputs();
    try testing.expectEqual(@as(u8, 0x42), in.byte);
    try testing.expect(!in.toggle);
}
