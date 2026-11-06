//! The relay's line protocol (desktop → PS over the USB-UART, J14).
//!
//!   K<4 hex>\n          the full key state, bit k = CHIP-8 key k held
//!   K<8 hex>\n          the same, 32 bits (a system with more buttons)
//!   H1\n / H0\n         hold the PL design (loading) / release it (run)
//!   L<3 hex><4 hex>\n   load: address, length; exactly that many raw bytes
//!                       follow the newline; the reply echoes the address
//!                       in the same width ("l<3 hex><4 hex><sum>")
//!   L<6 hex><4 hex>\n   the same with a full 24-bit address of the PS
//!                       link (Platform.Ps.Link; Game Boy cartridges). The
//!                       PL always gets three address bytes. (M2's
//!                       M<5 hex><4 hex> is gone since side track A.)
//!   P\n                 ping: the relay answers "p" (for measuring latency)
//!   F1\n / F0\n         (side track O, O.3) frame advance: pause the game
//!                       (the first time) or run it one frame (paused); run
//!                       it again. Replies "f1" / "f0"
//!   G\n                 (side track O, O.4) a screenshot of the running game
//!                       into the card's /screens: "g<its path>" or "x<reason>"
//!   B<n>\n              switch the UART after the reply ("b<n>", sent at
//!                       the old rate): n = 0 115200, 1 921600, 2 460800,
//!                       3 230400, 4 2000000 (side track G phase 2: the
//!                       Arty's FTDI bridge). Above 115200 the relay falls back to it
//!                       if nothing valid arrives for 0.6 s
//!
//! Writing the SD card (`make sd`, for users without a card reader; every
//! block is read back and compared before the reply):
//!
//!   S\n                 open the card and allow writes: "s<type>" (0 SDSC
//!                       1.x, 1 SDSC 2.0, 2 SDHC) or "x<reason>"
//!   D<8 hex><4 hex>\n   write block LBA; exactly 512 raw bytes follow, whose
//!                       checksum() is the 4 hex digits: "d<lba>" or
//!                       "x<lba> <reason>"
//!   Z<8 hex><8 hex>\n   fill blocks LBA .. LBA + count - 1 with zeros:
//!                       "z<lba><count>" or "x<lba> <reason>"
//!   M<8 hex><2 hex><4 hex>\n
//!                       (side track G phase 2) write `count` (1 to
//!                       chunk_blocks) blocks from LBA in one multi-block
//!                       write; count * 512 raw bytes follow, whose
//!                       checksum() is the 4 hex digits; read back:
//!                       "m<lba>" or "x<lba> <reason>"
//!   C<8 hex><2 hex>\n   the chunkHash of `count` blocks from LBA as they
//!                       are on the card: "c<lba><16 hex>" or "x<lba>
//!                       <reason>" (make sd sends only chunks that differ)
//!   R<8 hex>\n          read block LBA (side track I: `make saves-pull`
//!                       copies /saves off the card before `make sd`
//!                       erases it): "r<lba><4 hex sum>" and exactly 512
//!                       raw bytes after its newline, or "x<lba> <reason>".
//!                       A save the running game hasn't written back yet
//!                       is written first
//!   O\n                 (re)open the ROM menu, reading the card again: "o"
//!                       (not M: that is the Game Boy's 20-bit load)
//!
//! DDR (side track C; `make ddr-test`, `make ddr-load`):
//!
//!   T\n                 the DDR test suite (ddrtest.zig) on an M0e
//!                       bitstream: "t ..." lines, then "t end" (or
//!                       "x <reason>")
//!   T<8 hex base><2 hex mode><6 hex words><1 hex flags>\n
//!                       one run (M0e.Tester's mode; flags bit 0: the CPU
//!                       busy on DDR meanwhile, bit 1: read the data in
//!                       DDR instead of writing the test pattern)
//!   W<8 hex addr><6 hex len>\n
//!                       write DDR: exactly len raw bytes follow; replies
//!                       "w<addr><len><sum>" (sum as for L) or "x <reason>"
//!   Y<8 hex DDR addr><6 hex link addr><6 hex len>\n
//!                       (side track D) copy len bytes from DDR onto the
//!                       PS link at that address, while held: "y<6 hex link
//!                       addr><6 hex len><4 hex sum>", or "x ..." for an
//!                       address outside DDR
//!
//! and, outside lines, the compact key frame (3 bytes instead of 6):
//!
//!   0x80 | k[15:14], k[13:7], k[6:0]
//!
//! or, for a key word wider than 16 bits (M7's request: the SNES's two
//! pads; link sends it only when bits 16 to 31 aren't all 0, so a relay
//! from before never sees one from a system it knows), the wide frame:
//!
//!   0x84 | k[31:30], k[29:23], k[22:16], k[15:9], k[8:2], k[1:0] << 5
//!
//! (a first byte of exactly 0x84 to 0x87; any other byte >= 0x80 starts a
//! short frame, as before)
//!
//! Text never contains bytes >= 0x80, so the first byte marks the frame;
//! a frame interrupted by a byte >= 0x80 restarts there.
//!
//! Hex digits in either case; \r is ignored. Anything else up to the next
//! \n is ignored. The desktop resends the key state about 30 times per
//! second, so a lost or garbled K line costs one update, not a stuck key.

pub const Command = union(enum) {
    keys: u32,
    hold: bool,
    load: struct { addr: u24, len: u16, wide: bool = false },
    ping,
    /// frame advance (side track O, O.3): true = a step, false = run
    frame: bool,
    /// a screenshot (side track O, O.4)
    screenshot,
    baud: u3,
    sd_open,
    sd_write: struct { lba: u32, sum: u16 },
    /// M: `count` (1 to `chunk_blocks`) blocks from `lba`, written with one
    /// multi-block write, read back (side track G phase 2: make sd)
    sd_multi: struct { lba: u32, count: u8, sum: u16 },
    /// C: the hash (`chunkHash`) of `count` blocks from `lba` as they are on
    /// the card, so the desktop sends only what differs
    sd_hash: struct { lba: u32, count: u8 },
    sd_zero: struct { lba: u32, count: u32 },
    sd_read: u32,
    menu,
    ddr_suite,
    ddr_run: struct { base: u32, mode: u8, words: u24, flags: u4 },
    ddr_write: struct { addr: u32, len: u24 },
    /// Y: `len` bytes from DDR at `ddr` onto the PS link at `link` (side
    /// track D: an NES cartridge's header and CHR ROM after its file went
    /// into DDR with W, as the SD menu does it; the relay strobes them out
    /// itself, faster than the UART could bring them at 2 Mbaud)
    ddr_link: struct { ddr: u32, link: u24, len: u24 },
};

/// Bit rates for B<n>. B4 (side track G phase 2): 2 Mbaud, which both the
/// Zynq's 50 MHz UART reference (/ (5 * 5)) and the Arty's FTDI bridge
/// make exactly; the ZedBoard's Cypress bridge can't, and link falls back.
pub const rates = [5]u32{ 115200, 921600, 460800, 230400, 2000000 };

/// The most blocks one M or C command carries (8 KiB: the relay's buffer).
pub const chunk_blocks = 16;

/// The hash of a run of blocks for C: 64-bit FNV-1a over the bytes (the
/// same function on the desktop, the relay and tools/test_link.py's fake
/// relay; a chance match of two different 8 KiB chunks is about 2^-64).
pub const ChunkHash = std.hash.Fnv1a_64;

pub fn chunkHash(bytes: []const u8) u64 {
    return ChunkHash.hash(bytes);
}

/// The compact key frame for a key state.
pub fn keyFrame(k: u16) [3]u8 {
    return .{ 0x80 | @as(u8, @intCast(k >> 14)), @intCast((k >> 7) & 0x7F), @intCast(k & 0x7F) };
}

/// The wide key frame (bits 16 to 31 too).
pub fn keyFrameWide(k: u32) [6]u8 {
    return .{
        0x84 | @as(u8, @intCast(k >> 30)),
        @intCast((k >> 23) & 0x7F),
        @intCast((k >> 16) & 0x7F),
        @intCast((k >> 9) & 0x7F),
        @intCast((k >> 2) & 0x7F),
        @intCast((k & 0x3) << 5),
    };
}

/// The frame link sends for a key state: the short one while it fits.
pub fn keyFrameFor(k: u32, buf: *[6]u8) []const u8 {
    if (k <= 0xFFFF) {
        buf[0..3].* = keyFrame(@intCast(k));
        return buf[0..3];
    }
    buf.* = keyFrameWide(k);
    return buf;
}

pub const Parser = struct {
    buf: [24]u8 = undefined, // the longest line: Y, 21 characters (side track D)
    len: usize = 0,
    /// The line didn't fit; skip to the next \n.
    overflow: bool = false,
    /// Bytes of a compact key frame still to come (0: not in a frame).
    frame_left: u3 = 0,
    frame: u32 = 0,
    /// the frame in progress is the wide one
    wide: bool = false,

    /// Feed one received byte. Returns the command when a complete, valid
    /// line has just ended. After a `load`, the caller reads the payload
    /// itself; the parser starts on a fresh line.
    pub fn feed(p: *Parser, c: u8) ?Command {
        if (c >= 0x80) {
            // exactly 0x84 to 0x87; any other high byte starts a short one,
            // as before (the J14 bridge's stray 0xFE at power-on)
            p.wide = c & 0xFC == 0x84;
            p.frame = if (p.wide) @as(u32, c & 0x03) << 30 else @as(u32, c & 0x03) << 14;
            p.frame_left = if (p.wide) 5 else 2;
            return null;
        }
        if (p.frame_left != 0) {
            p.frame_left -= 1;
            if (p.wide) {
                // 23, 16, 9, 2, then the last byte's bits 6 and 5
                if (p.frame_left == 0) {
                    p.frame |= @as(u32, c >> 5) & 0x3;
                } else p.frame |= @as(u32, c) << (@as(u5, p.frame_left - 1) * 7 + 2);
            } else p.frame |= @as(u32, c) << (@as(u5, p.frame_left) * 7);
            if (p.frame_left == 0) return .{ .keys = p.frame };
            return null;
        }
        if (c == '\r') return null;
        if (c != '\n') {
            if (p.len < p.buf.len) {
                p.buf[p.len] = c;
                p.len += 1;
            } else p.overflow = true;
            return null;
        }
        defer {
            p.len = 0;
            p.overflow = false;
        }
        if (p.overflow or p.len == 0) return null;
        const line = p.buf[0..p.len];
        switch (line[0]) {
            'K' => if (line.len == 5 or line.len == 9) {
                if (hex(line[1..])) |v| return .{ .keys = v };
            },
            'H' => if (line.len == 2) switch (line[1]) {
                '0' => return .{ .hold = false },
                '1' => return .{ .hold = true },
                else => {},
            },
            'P' => if (line.len == 1) return .ping,
            'G' => if (line.len == 1) return .screenshot,
            'F' => if (line.len == 2) switch (line[1]) {
                '0' => return .{ .frame = false },
                '1' => return .{ .frame = true },
                else => {},
            },
            'S' => if (line.len == 1) return .sd_open,
            'O' => if (line.len == 1) return .menu,
            'T' => if (line.len == 1) {
                return .ddr_suite;
            } else if (line.len == 18) {
                const b = hex(line[1..9]) orelse return null;
                const m = hex(line[9..11]) orelse return null;
                const n = hex(line[11..17]) orelse return null;
                const f = hex(line[17..18]) orelse return null;
                return .{ .ddr_run = .{ .base = b, .mode = @intCast(m), .words = @intCast(n), .flags = @intCast(f) } };
            },
            'W' => if (line.len == 15) {
                const a = hex(line[1..9]) orelse return null;
                const n = hex(line[9..15]) orelse return null;
                return .{ .ddr_write = .{ .addr = a, .len = @intCast(n) } };
            },
            'Y' => if (line.len == 21) {
                const d = hex(line[1..9]) orelse return null;
                const l = hex(line[9..15]) orelse return null;
                const n = hex(line[15..21]) orelse return null;
                return .{ .ddr_link = .{ .ddr = d, .link = @intCast(l), .len = @intCast(n) } };
            },
            'D' => if (line.len == 13) {
                const a = hex(line[1..9]) orelse return null;
                const s = hex(line[9..13]) orelse return null;
                return .{ .sd_write = .{ .lba = a, .sum = @intCast(s) } };
            },
            'R' => if (line.len == 9) {
                const a = hex(line[1..9]) orelse return null;
                return .{ .sd_read = a };
            },
            'Z' => if (line.len == 17) {
                const a = hex(line[1..9]) orelse return null;
                const n = hex(line[9..17]) orelse return null;
                return .{ .sd_zero = .{ .lba = a, .count = n } };
            },
            'B' => if (line.len == 2 and line[1] >= '0' and line[1] <= '4') return .{ .baud = @intCast(line[1] - '0') },
            'M' => if (line.len == 15) {
                const a = hex(line[1..9]) orelse return null;
                const n = hex(line[9..11]) orelse return null;
                const s = hex(line[11..15]) orelse return null;
                if (n == 0 or n > chunk_blocks) return null;
                return .{ .sd_multi = .{ .lba = a, .count = @intCast(n), .sum = @intCast(s) } };
            },
            'C' => if (line.len == 11) {
                const a = hex(line[1..9]) orelse return null;
                const n = hex(line[9..11]) orelse return null;
                if (n == 0 or n > chunk_blocks) return null;
                return .{ .sd_hash = .{ .lba = a, .count = @intCast(n) } };
            },
            'L' => if (line.len == 8 or line.len == 11) {
                const w = line.len - 5; // address digits: 3 or 6
                const a = hex(line[1 .. 1 + w]) orelse return null;
                const n = hex(line[1 + w ..]) orelse return null;
                return .{ .load = .{ .addr = @intCast(a), .len = @intCast(n), .wide = w == 6 } };
            },
            else => {},
        }
        return null;
    }
};

/// Inline over a scalar on purpose: as a call returning `?u32` (through a
/// result slot in memory on ARM), the PS app's build once placed that
/// slot at an odd stack address and read it with a word load, which
/// faults with the MMU off (tools/check-ps-align.sh caught it, side track
/// F, Zig 0.16).
inline fn hex(s: []const u8) ?u32 {
    const v = hexValue(s);
    return if (v < 0) null else @intCast(v);
}

/// The value of up to 8 hex digits, or -1 if one isn't.
fn hexValue(s: []const u8) i64 {
    var v: u32 = 0;
    for (s) |h| {
        const d: u32 = switch (h) {
            '0'...'9' => h - '0',
            'a'...'f' => h - 'a' + 10,
            'A'...'F' => h - 'A' + 10,
            else => return -1,
        };
        v = v << 4 | d;
    }
    return v;
}

/// When the relay falls back to 115200 baud: above it, after `quiet` ticks
/// without valid traffic. A command counts when it is parsed, and a load
/// also when its last byte has arrived: a long load (16 KiB takes 0.71 s at
/// 230400 baud) must not look like silence (board bug, M2 step 2.8: the
/// relay dropped to 115200 between two cartridge blocks).
pub const Activity = struct {
    last: u64,

    pub fn command(a: *Activity, now: u64) void {
        a.last = now;
    }

    pub fn loadDone(a: *Activity, now: u64) void {
        a.last = now;
    }

    /// Any command has just finished: the same reasoning as `loadDone`,
    /// for the SD card commands (a zero-fill of 2047 blocks took 9 s on
    /// the board, and the relay dropped to 115200 right after it, under
    /// the desktop's next command; make sd, M1b stage C).
    pub fn commandDone(a: *Activity, now: u64) void {
        a.last = now;
    }

    pub fn fallBack(a: Activity, now: u64, baud: u3, quiet: u64) bool {
        return baud != 0 and now - a.last > quiet;
    }
};

/// The checksum the relay reports after a load: sum of the bytes, mod 2^16.
pub fn checksum(prev: u16, b: u8) u16 {
    return prev +% b;
}

const std = @import("std");
const testing = std.testing;

fn feedAll(p: *Parser, s: []const u8) ?Command {
    var last: ?Command = null;
    for (s) |c| if (p.feed(c)) |v| {
        last = v;
    };
    return last;
}

test "key lines" {
    var p: Parser = .{};
    try testing.expectEqual(Command{ .keys = 0x0004 }, feedAll(&p, "K0004\n").?);
    try testing.expectEqual(Command{ .keys = 0xBEEF }, feedAll(&p, "KbeEf\r\n").?);
    try testing.expectEqual(Command{ .keys = 0 }, feedAll(&p, "K0000\n").?);
}

test "hold and load lines" {
    var p: Parser = .{};
    try testing.expectEqual(Command{ .hold = true }, feedAll(&p, "H1\n").?);
    try testing.expectEqual(Command{ .hold = false }, feedAll(&p, "H0\r\n").?);
    const l = feedAll(&p, "L2000E00\n").?;
    try testing.expectEqual(@as(u12, 0x200), l.load.addr);
    try testing.expectEqual(@as(u16, 0xE00), l.load.len);
    try testing.expectEqual(@as(u24, 0x1FF), feedAll(&p, "L1ff0001\n").?.load.addr);
    try testing.expect(!l.load.wide);
    const w = feedAll(&p, "L03C0004000\n").?;
    try testing.expectEqual(@as(u24, 0x3C000), w.load.addr);
    try testing.expectEqual(@as(u16, 0x4000), w.load.len);
    try testing.expect(w.load.wide);
    try testing.expectEqual(@as(u24, 0xF01000), feedAll(&p, "LF010000001\n").?.load.addr);
    try testing.expectEqual(@as(?Command, null), feedAll(&p, "L03C000400\n")); // 10: neither form
    try testing.expectEqual(@as(?Command, null), feedAll(&p, "M3C0004000\n")); // M is gone
}

test "a long load is activity: no fallback right after it" {
    const quiet: u64 = 600;
    var a: Activity = .{ .last = 0 };
    a.command(1000); // M line parsed
    // the payload takes 710 ticks; at its end, the relay is not quiet
    a.loadDone(1710);
    try testing.expect(!a.fallBack(1711, 3, quiet));
    try testing.expect(a.fallBack(1710 + quiet + 1, 3, quiet));
    try testing.expect(!a.fallBack(1710 + quiet + 1, 0, quiet)); // already at 115200
}

test "a long SD command is activity too" {
    const quiet: u64 = 600;
    var a: Activity = .{ .last = 0 };
    a.command(1000); // Z line parsed
    a.commandDone(10_000); // 9000 ticks of zero-fill
    try testing.expect(!a.fallBack(10_001, 3, quiet));
    try testing.expect(a.fallBack(10_000 + quiet + 1, 3, quiet));
}

test "frame advance lines" {
    var p: Parser = .{};
    try std.testing.expectEqual(@as(?Command, null), feedAll(&p, "F1"));
    try std.testing.expectEqual(@as(?Command, .{ .frame = true }), p.feed('\n'));
    _ = feedAll(&p, "F0");
    try std.testing.expectEqual(@as(?Command, .{ .frame = false }), p.feed('\n'));
    _ = feedAll(&p, "F2");
    try std.testing.expectEqual(@as(?Command, null), p.feed('\n'));
}

test "ping and baud lines" {
    var p: Parser = .{};
    try testing.expectEqual(Command.ping, feedAll(&p, "P\n").?);
    try testing.expectEqual(Command{ .baud = 1 }, feedAll(&p, "B1\n").?);
    try testing.expectEqual(Command{ .baud = 0 }, feedAll(&p, "B0\r\n").?);
    try testing.expectEqual(Command{ .baud = 3 }, feedAll(&p, "B3\n").?);
    try testing.expectEqual(Command{ .baud = 4 }, feedAll(&p, "B4\n").?);
    try testing.expectEqual(@as(?Command, null), feedAll(&p, "B5\n"));
    try testing.expectEqual(@as(?Command, null), feedAll(&p, "PP\n"));
    try testing.expectEqual(@as(u32, 2_000_000), rates[4]);
}

test "M and C lines (side track G phase 2): counts 1 to chunk_blocks" {
    var p: Parser = .{};
    try testing.expectEqual(Command{ .sd_multi = .{ .lba = 0x800, .count = 16, .sum = 0xBEEF } }, feedAll(&p, "M0000080010BEEF\n").?);
    try testing.expectEqual(Command{ .sd_multi = .{ .lba = 1, .count = 1, .sum = 0 } }, feedAll(&p, "M00000001010000\n").?);
    try testing.expectEqual(@as(?Command, null), feedAll(&p, "M0000000000BEEF\n"));
    try testing.expectEqual(@as(?Command, null), feedAll(&p, "M0000000011BEEF\n"));
    try testing.expectEqual(Command{ .sd_hash = .{ .lba = 0x12345678, .count = 16 } }, feedAll(&p, "C1234567810\n").?);
    try testing.expectEqual(@as(?Command, null), feedAll(&p, "C1234567800\n"));
    try testing.expectEqual(@as(?Command, null), feedAll(&p, "C123456781\n"));
    // the hash is 64-bit FNV-1a (tools/test_link.py has its own copy)
    try testing.expectEqual(@as(u64, 0xcbf29ce484222325), chunkHash(""));
    try testing.expectEqual(@as(u64, 0xaf63dc4c8601ec8c), chunkHash("a"));
}

test "garbage is ignored and resynchronises at the next newline" {
    var p: Parser = .{};
    try testing.expectEqual(@as(?Command, null), feedAll(&p, "hello\n"));
    try testing.expectEqual(@as(?Command, null), feedAll(&p, "K12345678901234\n"));
    try testing.expectEqual(@as(?Command, null), feedAll(&p, "K12G4\n"));
    try testing.expectEqual(@as(?Command, null), feedAll(&p, "K123\n"));
    try testing.expectEqual(@as(?Command, null), feedAll(&p, "H2\n"));
    try testing.expectEqual(@as(?Command, null), feedAll(&p, "L20G0001\n"));
    try testing.expectEqual(@as(?Command, null), feedAll(&p, "L200001\n"));
    try testing.expectEqual(Command{ .keys = 0x0010 }, feedAll(&p, "noise noise\nK0010\n").?);
}

test "a stray byte >= 0x80 swallows a ping; link's resync sequence first saves it" {
    // side track F, on the board after an SD boot: the J14 bridge's stray
    // byte opened a key frame, "P\n" became its data ("kA80A")
    var p: Parser = .{};
    try testing.expectEqual(Command{ .keys = 0xA80A }, feedAll(&p, "\xfeP\n").?);
    // link.zig sends a frame with no keys and a newline before each ping;
    // after any junk (a high byte, half a frame, half a line) the ping works
    const resync = [_]u8{ 0x80, 0x00, 0x00, '\n' };
    for ([_][]const u8{ "\xfe", "\xfe\x12", "\x81\x05", "K12", "K1\xfe" }) |junk| {
        var q: Parser = .{};
        _ = feedAll(&q, junk);
        var last: ?Command = null;
        for (resync ++ "P\n") |c| if (q.feed(c)) |v| {
            last = v;
        };
        try testing.expectEqual(Command.ping, last.?);
    }
}

test "wide key frames: 32 bits, the short one while it fits; K with 8 digits" {
    var p: Parser = .{};
    for ([_]u32{ 0x10000, 0x0FFF0000, 0xFFFFFFFF, 0x80000001, 0x00010002, 0x12345678 }) |k| {
        try testing.expectEqual(Command{ .keys = k }, feedAll(&p, &keyFrameWide(k)).?);
        var b: [6]u8 = undefined;
        try testing.expectEqual(@as(usize, 6), keyFrameFor(k, &b).len);
    }
    var b: [6]u8 = undefined;
    try testing.expectEqualSlices(u8, &keyFrame(0x1234), keyFrameFor(0x1234, &b));
    try testing.expectEqual(Command{ .keys = 0xABCD1234 }, feedAll(&p, "KABCD1234\n").?);
    try testing.expectEqual(Command{ .keys = 0x1234 }, feedAll(&p, "K1234\n").?);
    // a wide frame interrupted by another frame starts again there
    const cut = keyFrameWide(0xFFFF0000)[0..3] ++ keyFrame(0x0042);
    try testing.expectEqual(Command{ .keys = 0x0042 }, feedAll(&p, cut).?);
}

test "compact key frames, also between and inside text" {
    var p: Parser = .{};
    for ([_]u16{ 0, 1, 0x20, 0x8001, 0xFFFF, 0x4080 }) |k| {
        try testing.expectEqual(Command{ .keys = k }, feedAll(&p, &keyFrame(k)).?);
    }
    // a frame in the middle of a text line, then the line continues fine
    const mixed = "K0" ++ keyFrame(0x1234) ++ "\nP\n";
    try testing.expectEqual(Command.ping, feedAll(&p, mixed).?);
    // an interrupted frame restarts at the new marker
    const two = [_]u8{ 0x81, 0x05 } ++ keyFrame(0x0042);
    try testing.expectEqual(Command{ .keys = 0x0042 }, feedAll(&p, &two).?);
}

test "SD card lines" {
    var p: Parser = .{};
    try testing.expectEqual(Command.sd_open, feedAll(&p, "S\n").?);
    try testing.expectEqual(Command.menu, feedAll(&p, "O\r\n").?);
    const d = feedAll(&p, "D00000800BEEF\n").?;
    try testing.expectEqual(@as(u32, 0x800), d.sd_write.lba);
    try testing.expectEqual(@as(u16, 0xBEEF), d.sd_write.sum);
    const z = feedAll(&p, "Zffffffff00020000\n").?;
    try testing.expectEqual(@as(u32, 0xFFFFFFFF), z.sd_zero.lba);
    try testing.expectEqual(@as(u32, 0x20000), z.sd_zero.count);
    try testing.expectEqual(@as(?Command, null), feedAll(&p, "D0000080BEEF\n"));
    try testing.expectEqual(@as(?Command, null), feedAll(&p, "Z00000000000200000\n"));
    try testing.expectEqual(@as(?Command, null), feedAll(&p, "SS\n"));
    try testing.expectEqual(@as(?Command, null), feedAll(&p, "D0000080GBEEF\n"));
    try testing.expectEqual(@as(u32, 0x12AB), feedAll(&p, "R000012ab\n").?.sd_read);
    try testing.expectEqual(@as(?Command, null), feedAll(&p, "R12AB\n"));
}

test "DDR lines" {
    var p: Parser = .{};
    try testing.expectEqual(Command.ddr_suite, feedAll(&p, "T\n").?);
    const r = feedAll(&p, "T0100000080004000\r\n");
    try testing.expectEqual(@as(?Command, null), r); // 16 characters: not a run
    const t = feedAll(&p, "T01000000800040003\n").?;
    try testing.expectEqual(@as(u32, 0x0100_0000), t.ddr_run.base);
    try testing.expectEqual(@as(u8, 0x80), t.ddr_run.mode);
    try testing.expectEqual(@as(u24, 0x4000), t.ddr_run.words);
    try testing.expectEqual(@as(u4, 3), t.ddr_run.flags);
    const w = feedAll(&p, "W0200000001ABCD\n").?;
    try testing.expectEqual(@as(u32, 0x0200_0000), w.ddr_write.addr);
    try testing.expectEqual(@as(u24, 0x01ABCD), w.ddr_write.len);
    try testing.expectEqual(@as(?Command, null), feedAll(&p, "W0200000001ABC\n"));
    const y = feedAll(&p, "Y02080010008010040000\n").?;
    try testing.expectEqual(@as(u32, 0x0208_0010), y.ddr_link.ddr);
    try testing.expectEqual(@as(u24, 0x008010), y.ddr_link.link);
    try testing.expectEqual(@as(u24, 0x040000), y.ddr_link.len);
    try testing.expectEqual(@as(?Command, null), feedAll(&p, "Y020800100080100400\n"));
    try testing.expectEqual(@as(?Command, null), feedAll(&p, "T0100000080004000G\n"));
}

test "checksum wraps" {
    var c: u16 = 0xFFFF;
    c = checksum(c, 2);
    try testing.expectEqual(@as(u16, 1), c);
}
