//! Programming the PL from the PS (side track F, step 2): a bitstream file
//! from the SD card, through DDR, into the PL's configuration logic over
//! the device configuration interface's PCAP DMA. No JTAG, no FSBL.
//!
//! The file on the card is Vivado's own `.bit` (`/systems/<name>.bit`): a
//! short header of tagged fields (design name, part, date, time, data
//! length; the Xilinx "bit" format, as documented by the open-source tools
//! that read it), then the configuration data as big-endian 32-bit words.
//! PCAP takes words little-endian from memory, so every word is
//! byte-swapped on the way into DDR: the sync word 0xAA995566 then reads
//! as such (the Linux zynq-fpga driver demands the same, "a byte swapped
//! .bin file"). The header is checked first: the part must be this
//! board's, the data whole words that fit the DDR buffer, and the sync word
//! must come within the first words.
//!
//! The PCAP sequence: UG585 v1.8.1 6.4.4 ("Configure and Reconfigure the
//! PL via AXI-PCAP Bridge") and the devcfg registers (appendix B.16), in
//! the order the Linux zynq-fpga driver uses around it: the PL's AXI resets
//! asserted and the PS-to-PL level shifters off (0xA keeps PL-to-PS only;
//! 0 would also cut what the PS reads) before the PL is cleared, then
//! PROG_B pulsed while watching INIT, interrupts cleared, PCAP selected at
//! full rate without loopback, one DMA command (source address + 1: hold
//! DONE until PCAP has taken the data too; destination 0xFFFFFFFF), then
//! D_P_DONE, then the PL's DONE; afterwards what `ps7_post_config` does:
//! level shifters on, PL resets released. The control register is changed
//! with read-modify-write: it also holds the debug enables the boot ROM set
//! (the DAP: JTAG debugging must keep working).

const std = @import("std");

// --- The bitstream file ----------------------------------------------------------

/// Where the swapped data goes in DDR: 8 MiB from 384 MiB, clear of the
/// USB host's memory (0x00100000) and of the DDR test (0x01000000 to
/// 0x10FFFFFF, M0e only; docs/ARCHITECTURE.md, "DDR regions in use"). A
/// Zynq-7020 bitstream is 4045564 bytes uncompressed.
pub const ddr_addr: u32 = 0x1800_0000;
pub const ddr_size: u32 = 8 << 20;

/// This board's part, as Vivado writes it into the header's field b.
pub const zedboard_part = "7z020clg484";

pub const Header = struct {
    /// field a: "chip8_zynq;UserID=0XFFFFFFFF;Version=2026.1"
    design: []const u8,
    /// field b: "7z020clg484"
    part: []const u8,
    /// fields c and d
    date: []const u8,
    time: []const u8,
    /// where the configuration data starts in the file, and its length
    data_offset: u32,
    data_len: u32,

    /// The design name up to its first ';' ("chip8_zynq").
    pub fn name(h: Header) []const u8 {
        return h.design[0 .. std.mem.indexOfScalar(u8, h.design, ';') orelse h.design.len];
    }
};

pub const HeaderError = error{
    /// not a .bit file (the fixed first field is missing)
    NotBit,
    /// a field is missing, out of order or runs past the bytes given
    BadHeader,
    /// for another device
    WrongPart,
    /// the data length is not whole words, doesn't match the file's size,
    /// or doesn't fit the DDR buffer
    BadLength,
};

/// The fixed start of every .bit file: a 9-byte field (length 0x0009), then
/// a length of 1 before the first key, 'a'.
const bit_magic = [_]u8{ 0x00, 0x09, 0x0F, 0xF0, 0x0F, 0xF0, 0x0F, 0xF0, 0x0F, 0xF0, 0x00, 0x00, 0x01 };

/// Parses the header from the start of the file (`first`: at least the
/// whole header; one 512-byte sector is plenty) and checks it against the
/// file's size and the board's part.
pub fn parseHeader(first: []const u8, file_size: u32, part: []const u8) HeaderError!Header {
    if (first.len < bit_magic.len or !std.mem.eql(u8, first[0..bit_magic.len], &bit_magic)) return error.NotBit;
    var at: usize = bit_magic.len;
    var fields: [4][]const u8 = undefined;
    for ("abcd", 0..) |key, i| {
        if (at + 3 > first.len or first[at] != key) return error.BadHeader;
        const len = @as(usize, first[at + 1]) << 8 | first[at + 2];
        at += 3;
        if (at + len > first.len) return error.BadHeader;
        var s = first[at .. at + len];
        if (s.len > 0 and s[s.len - 1] == 0) s = s[0 .. s.len - 1];
        fields[i] = s;
        at += len;
    }
    if (at + 5 > first.len or first[at] != 'e') return error.BadHeader;
    const data_len = std.mem.readInt(u32, first[at + 1 ..][0..4], .big);
    at += 5;
    if (!std.mem.eql(u8, fields[1], part)) return error.WrongPart;
    if (data_len == 0 or data_len % 4 != 0 or data_len > ddr_size or
        @as(u64, at) + data_len != file_size) return error.BadLength;
    return .{
        .design = fields[0],
        .part = fields[1],
        .date = fields[2],
        .time = fields[3],
        .data_offset = @intCast(at),
        .data_len = data_len,
    };
}

/// The sync word, which must come within the first `sync_within` words of
/// the data (after the dummy words and the bus width pattern, UG470).
pub const sync_word: u32 = 0xAA99_5566;
pub const sync_within = 64;

/// Copies the configuration data into memory a chunk at a time, each
/// big-endian word stored as a native (little-endian) one; the chunks may
/// split words anywhere. `Mem` has `store(index: u32, word: u32)`.
pub fn Swapper(comptime Mem: type) type {
    return struct {
        const Self = @This();
        mem: *Mem,
        words: u32 = 0,
        part: u32 = 0,
        part_len: u2 = 0,
        /// word index of the sync word, once seen in the first words
        sync_at: ?u32 = null,

        pub fn feed(s: *Self, bytes: []const u8) void {
            for (bytes) |b| {
                s.part = s.part << 8 | b;
                if (s.part_len == 3) {
                    if (s.sync_at == null and s.words < sync_within and s.part == sync_word) s.sync_at = s.words;
                    s.mem.store(s.words, s.part);
                    s.words += 1;
                    s.part_len = 0;
                } else s.part_len += 1;
            }
        }
    };
}

/// The blocks of a .bit file as a multi-block read hands them over
/// (sd.readBlocks, per run of fat.runs): the header's bytes skipped, the
/// rest through the Swapper; bytes past the run's end (the file's last
/// block) ignored. The caller sets `left` to the run's bytes before each
/// run.
pub fn BlockSink(comptime Mem: type) type {
    return struct {
        const Self = @This();
        sw: Swapper(Mem),
        /// header bytes still to skip
        skip: u32,
        /// bytes of the file in the rest of this run
        left: u32,

        pub fn block(s: *Self, b: *const [512]u8) void {
            const n = @min(512, s.left);
            s.left -= n;
            const k = @min(n, s.skip);
            s.skip -= k;
            s.sw.feed(b[k..n]);
        }
    };
}

// --- The PCAP sequence ------------------------------------------------------------

const slcr_lock = 0xF800_0004;
const slcr_unlock = 0xF800_0008;
const slcr_fpga_rst_ctrl = 0xF800_0240;
const slcr_lvl_shftr_en = 0xF800_0900;

const devcfg = 0xF800_7000;
pub const dc_ctrl = devcfg + 0x00;
pub const dc_int_sts = devcfg + 0x0C;
pub const dc_status = devcfg + 0x14;
pub const dc_dma_src = devcfg + 0x18;
pub const dc_dma_dst = devcfg + 0x1C;
pub const dc_dma_src_len = devcfg + 0x20;
pub const dc_dma_dst_len = devcfg + 0x24;
pub const dc_unlock = devcfg + 0x34;
pub const dc_mctrl = devcfg + 0x80;

pub const unlock_word: u32 = 0x757B_DF0D; // UNLOCK: anything else locks devcfg up
pub const ctrl_prog_b: u32 = 1 << 30;
pub const ctrl_pcap_pr: u32 = 1 << 27;
pub const ctrl_pcap_mode: u32 = 1 << 26;
pub const ctrl_quarter_rate: u32 = 1 << 25;
pub const status_cmd_q_full: u32 = 1 << 31;
pub const status_pcfg_init: u32 = 1 << 4;
pub const mctrl_pcap_lpbk: u32 = 1 << 4;
pub const int_d_p_done: u32 = 1 << 12;
pub const int_pcfg_done: u32 = 1 << 2;
/// AXI timeouts and errors, RX FIFO overflow, DMA command error and queue
/// overflow, PCAP-to-DMA length error, HMAC error (INT_STS)
pub const int_errors: u32 = (0xF << 20) | (1 << 18) | (1 << 15) | (1 << 14) | (1 << 11) | (1 << 6);

/// Level shifters during configuration: PL-to-PS only (FSBL's LVL_PL_PS,
/// Linux's LVL_SHFTR_ENABLE_PS_TO_PL); all on afterwards.
pub const lvl_during: u32 = 0xA;
pub const lvl_after: u32 = 0xF;

pub const Error = error{
    /// PCFG_INIT didn't follow PROG_B (the PL isn't powered, or the
    /// configuration controller is stuck)
    InitTimeout,
    /// the DMA command queue is full
    QueueFull,
    /// no D_P_DONE: the DMA didn't finish
    DmaTimeout,
    /// the DMA reported an error (INT_STS bits in `Pcap.err_bits`)
    DmaError,
    /// the data went in, but the PL's DONE didn't rise: a bad bitstream
    NoDone,
};

/// `Bus`: read32(addr), write32(addr, v) on the PS's registers, nowUs().
pub fn Pcap(comptime Bus: type) type {
    return struct {
        const Self = @This();
        bus: *Bus,
        /// INT_STS when it failed, for the log
        err_bits: u32 = 0,

        fn rd(p: *Self, a: u32) u32 {
            return p.bus.read32(a);
        }
        fn wr(p: *Self, a: u32, v: u32) void {
            p.bus.write32(a, v);
        }

        /// Waits until (reg & mask != 0) == set, at most `us`.
        fn wait(p: *Self, reg: u32, mask: u32, set: bool, us: u64) bool {
            const t = p.bus.nowUs();
            while (true) {
                if ((p.rd(reg) & mask != 0) == set) return true;
                if (p.bus.nowUs() - t > us) return false;
            }
        }

        fn slcr(p: *Self, a: u32, v: u32) void {
            p.wr(slcr_unlock, 0xDF0D);
            p.wr(a, v);
        }

        /// Clears the PL and configures it with `words` words at `addr`
        /// (already swapped, Swapper). On an error the PL is left empty or
        /// half configured: DONE stays low, the level shifters stay off.
        pub fn program(p: *Self, addr: u32, words: u32) Error!void {
            p.err_bits = 0;
            p.wr(dc_unlock, unlock_word);
            // the PL's AXI resets on, PS-to-PL signals off while it changes
            p.slcr(slcr_fpga_rst_ctrl, 0xF);
            p.slcr(slcr_lvl_shftr_en, 0);
            p.slcr(slcr_lvl_shftr_en, lvl_during);
            // PCAP selected at full rate, without loopback (6.4.4 steps 1, 4)
            var ctrl = p.rd(dc_ctrl);
            ctrl = (ctrl | ctrl_pcap_pr | ctrl_pcap_mode) & ~ctrl_quarter_rate;
            p.wr(dc_ctrl, ctrl | ctrl_prog_b);
            p.wr(dc_mctrl, p.rd(dc_mctrl) & ~mctrl_pcap_lpbk);
            // clear the PL: PROG_B high, low until INIT drops, high until
            // INIT is back (housecleaning done, ready for data; step 2, 3)
            if (!p.wait(dc_status, status_pcfg_init, true, 100_000)) return error.InitTimeout;
            p.wr(dc_ctrl, ctrl & ~ctrl_prog_b);
            if (!p.wait(dc_status, status_pcfg_init, false, 100_000)) return error.InitTimeout;
            p.wr(dc_ctrl, ctrl | ctrl_prog_b);
            if (!p.wait(dc_status, status_pcfg_init, true, 100_000)) return error.InitTimeout;
            // every interrupt flag cleared, PCFG_DONE among them (step 2c, 5)
            p.wr(dc_int_sts, 0xFFFF_FFFF);
            if (p.rd(dc_status) & status_cmd_q_full != 0) return error.QueueFull;
            // one DMA command; it is queued when DMA_DEST_LEN is written
            p.wr(dc_dma_src, addr | 1);
            p.wr(dc_dma_dst, 0xFFFF_FFFF);
            p.wr(dc_dma_src_len, words);
            p.wr(dc_dma_dst_len, words);
            // 4 MB at 400 MB/s: about 10 ms; a generous limit
            const ok = p.wait(dc_int_sts, int_d_p_done | int_errors, true, 2_000_000);
            const sts = p.rd(dc_int_sts);
            if (sts & int_errors != 0) {
                p.err_bits = sts;
                return error.DmaError;
            }
            if (!ok) {
                p.err_bits = sts;
                return error.DmaTimeout;
            }
            if (!p.wait(dc_int_sts, int_pcfg_done, true, 100_000)) {
                p.err_bits = p.rd(dc_int_sts);
                return error.NoDone;
            }
            // ps7_post_config: level shifters on, PL resets released
            p.slcr(slcr_lvl_shftr_en, lvl_after);
            p.slcr(slcr_fpga_rst_ctrl, 0);
        }
    };
}

/// The PS's registers on the board.
pub const MmioBus = struct {
    pub fn read32(_: *MmioBus, a: u32) u32 {
        return @as(*volatile u32, @ptrFromInt(a)).*;
    }
    pub fn write32(_: *MmioBus, a: u32, v: u32) void {
        @as(*volatile u32, @ptrFromInt(a)).* = v;
    }
    pub fn nowUs(_: *MmioBus) u64 {
        const hi_p: *volatile u32 = @ptrFromInt(0xF8F0_0204);
        const lo_p: *volatile u32 = @ptrFromInt(0xF8F0_0200);
        while (true) {
            const hi = hi_p.*;
            const lo = lo_p.*;
            if (hi_p.* == hi) return (@as(u64, hi) << 32 | lo) * 3 / 1000;
        }
    }
};

/// DDR on the board (MMU off: uncached, strongly ordered, so the stores are
/// in DDR before the DMA starts).
pub const DdrMem = struct {
    pub fn store(_: *DdrMem, i: u32, w: u32) void {
        @as(*volatile u32, @ptrFromInt(ddr_addr + 4 * i)).* = w;
    }
};

// --- Tests ----------------------------------------------------------------------

const testing = std.testing;

/// A .bit file's bytes: header fields, then `data`.
fn makeBit(buf: []u8, design: []const u8, part: []const u8, data: []const u8) []u8 {
    var n: usize = 0;
    @memcpy(buf[0..bit_magic.len], &bit_magic);
    n = bit_magic.len;
    for ([_][]const u8{ design, part, "2026/10/04", "12:00:00" }, "abcd") |s, key| {
        buf[n] = key;
        std.mem.writeInt(u16, buf[n + 1 ..][0..2], @intCast(s.len + 1), .big);
        n += 3;
        @memcpy(buf[n..][0..s.len], s);
        n += s.len;
        buf[n] = 0;
        n += 1;
    }
    buf[n] = 'e';
    std.mem.writeInt(u32, buf[n + 1 ..][0..4], @intCast(data.len), .big);
    n += 5;
    @memcpy(buf[n..][0..data.len], data);
    return buf[0 .. n + data.len];
}

/// Dummy words, the bus width pattern, the sync word, a few commands
/// (UG470 table 5-* layout), as they start every 7-series bitstream.
const sample_data = [_]u8{
    0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
    0x00, 0x00, 0x00, 0xBB, 0x11, 0x22, 0x00, 0x44,
    0xFF, 0xFF, 0xFF, 0xFF, 0xAA, 0x99, 0x55, 0x66,
    0x20, 0x00, 0x00, 0x00, 0x30, 0x02, 0x20, 0x01,
};

const ArrayMem = struct {
    w: [4096]u32 = undefined,
    pub fn store(m: *ArrayMem, i: u32, v: u32) void {
        m.w[i] = v;
    }
};

test "header: fields, name, data offset and length" {
    var buf: [256]u8 = undefined;
    const f = makeBit(&buf, "chip8_zynq;UserID=0XFFFFFFFF;Version=2026.1", zedboard_part, &sample_data);
    const h = try parseHeader(f, @intCast(f.len), zedboard_part);
    try testing.expectEqualStrings("chip8_zynq", h.name());
    try testing.expectEqualStrings("7z020clg484", h.part);
    try testing.expectEqualStrings("2026/10/04", h.date);
    try testing.expectEqual(@as(u32, sample_data.len), h.data_len);
    try testing.expectEqualSlices(u8, &sample_data, f[h.data_offset..]);
}

test "header: what is refused" {
    var buf: [256]u8 = undefined;
    const f = makeBit(&buf, "nes_zynq", zedboard_part, &sample_data);
    const size: u32 = @intCast(f.len);
    // the Arty's part
    try testing.expectError(error.WrongPart, parseHeader(f, size, "7z020clg400"));
    // not a .bit file (a .bin, a ROM)
    try testing.expectError(error.NotBit, parseHeader(f[bit_magic.len + 3 + 9 + 5 ..], size, zedboard_part));
    try testing.expectError(error.NotBit, parseHeader("NES\x1a", 4, zedboard_part));
    // the header cut short
    try testing.expectError(error.BadHeader, parseHeader(f[0..30], size, zedboard_part));
    // a truncated file, a longer one
    try testing.expectError(error.BadLength, parseHeader(f, size - 4, zedboard_part));
    try testing.expectError(error.BadLength, parseHeader(f, size + 1, zedboard_part));
    // data not whole words
    const g = makeBit(&buf, "x", zedboard_part, sample_data[0..30]);
    try testing.expectError(error.BadLength, parseHeader(g, @intCast(g.len), zedboard_part));
    // a key out of order
    var h = makeBit(&buf, "x", zedboard_part, &sample_data);
    h[bit_magic.len + 3 + 2] = 'c';
    try testing.expectError(error.BadHeader, parseHeader(h, @intCast(h.len), zedboard_part));
}

test "swapper: words byte-swapped across any chunking, the sync word found" {
    for ([_]usize{ 1, 3, 5, 512 }) |chunk| {
        var mem: ArrayMem = .{};
        var s: Swapper(ArrayMem) = .{ .mem = &mem };
        var i: usize = 0;
        while (i < sample_data.len) : (i += chunk) s.feed(sample_data[i..@min(i + chunk, sample_data.len)]);
        try testing.expectEqual(@as(u32, sample_data.len / 4), s.words);
        try testing.expectEqual(@as(u32, 0xFFFF_FFFF), mem.w[0]);
        try testing.expectEqual(@as(u32, 0x0000_00BB), mem.w[2]);
        try testing.expectEqual(@as(u32, 0x1122_0044), mem.w[3]);
        try testing.expectEqual(sync_word, mem.w[5]);
        try testing.expectEqual(@as(?u32, 5), s.sync_at);
        // as PCAP reads it: the bytes in memory are 66 55 99 AA
        try testing.expectEqualSlices(u8, &.{ 0x66, 0x55, 0x99, 0xAA }, std.mem.asBytes(&mem.w[5]));
    }
    // no sync word: none found
    var mem: ArrayMem = .{};
    var s: Swapper(ArrayMem) = .{ .mem = &mem };
    s.feed(sample_data[0..16]);
    try testing.expectEqual(@as(?u32, null), s.sync_at);
}

test "block sink: a file in runs of blocks, header skipped, the tail block's garbage ignored" {
    var data: [3000]u8 = undefined;
    @memcpy(data[0..sample_data.len], &sample_data);
    for (data[sample_data.len..], sample_data.len..) |*b, i| b.* = @truncate(i * 7);
    var buf: [4096]u8 = undefined;
    const f = makeBit(&buf, "gb_zynq", zedboard_part, &data);
    const h = try parseHeader(f, @intCast(f.len), zedboard_part);
    // the expected words, straight through the swapper
    var want: ArrayMem = .{};
    var ws: Swapper(ArrayMem) = .{ .mem = &want };
    ws.feed(f[h.data_offset..]);
    // runs of 1, 2, 3 blocks; the last block of the file padded with junk
    var padded = [_]u8{0xEE} ** (8 * 512);
    @memcpy(padded[0..f.len], f);
    for ([_]usize{ 1, 2, 3, 8 }) |per_run| {
        var got: ArrayMem = .{};
        var s: BlockSink(ArrayMem) = .{ .sw = .{ .mem = &got }, .skip = h.data_offset, .left = 0 };
        var at: usize = 0;
        while (at < f.len) {
            const bytes = @min(per_run * 512, f.len - at);
            s.left = @intCast(bytes);
            var k: usize = 0;
            while (k * 512 < bytes) : (k += 1) s.block(padded[at + k * 512 ..][0..512]);
            at += bytes;
        }
        try testing.expectEqual(ws.words, s.sw.words);
        try testing.expectEqualSlices(u32, want.w[0..ws.words], got.w[0..s.sw.words]);
        try testing.expectEqual(@as(?u32, 5), s.sw.sync_at);
    }
}

/// The Arty Z7-20's part (board_arty_z7_20.zig's pl_part; the relay passes
/// its board's).
const arty_part = "7z020clg400";

/// A real bitstream: its header parses for `part` (its data length the
/// file's), names the design, is at most the XC7Z020's uncompressed length
/// (Vivado compresses them since 2026-10-16), and the swapper takes exactly
/// the header's words and finds the sync word in them.
fn checkRealBit(bytes: []const u8, part: []const u8, name: []const u8) !void {
    const h = try parseHeader(bytes[0..@min(512, bytes.len)], @intCast(bytes.len), part);
    try testing.expect(std.mem.indexOf(u8, name, h.name()) != null);
    try testing.expect(h.data_len <= 4045564);
    const Count = struct {
        n: u32 = 0,
        pub fn store(c: *@This(), _: u32, _: u32) void {
            c.n += 1;
        }
    };
    var c: Count = .{};
    var s: Swapper(Count) = .{ .mem = &c };
    s.feed(bytes[h.data_offset..]);
    try testing.expectEqual(h.data_len / 4, c.n);
    try testing.expect(s.sync_at != null);
}

/// The .bit in a tar the bitstream cache wrote (Python's tarfile, ustar
/// headers): its name and bytes, or null.
fn bitInTar(tar: []const u8) ?struct { name: []const u8, data: []const u8 } {
    var at: usize = 0;
    while (at + 512 <= tar.len) {
        const hdr = tar[at..][0..512];
        if (hdr[0] == 0) return null;
        const name = std.mem.sliceTo(hdr[0..100], 0);
        const size = std.fmt.parseInt(usize, std.mem.trim(u8, std.mem.sliceTo(hdr[124..136], 0), " "), 8) catch return null;
        const data_at = at + 512;
        if (data_at + size > tar.len) return null;
        if (hdr[156] == '0' or hdr[156] == 0) {
            if (std.mem.endsWith(u8, name, ".bit")) return .{ .name = name, .data = tar[data_at..][0..size] };
        }
        at = data_at + (size + 511) / 512 * 512;
    }
    return null;
}

test "bitInTar: a member found by its ending, the rest skipped" {
    var tar = [_]u8{0} ** (512 * 5);
    @memcpy(tar[0..7], "reports");
    @memcpy(tar[124..135], "00000000000");
    tar[156] = '5';
    @memcpy(tar[512..521], "a_top.bit");
    @memcpy(tar[512 + 124 .. 512 + 135], "00000000003");
    tar[512 + 156] = '0';
    @memcpy(tar[1024..1027], "xyz");
    const b = bitInTar(&tar).?;
    try testing.expectEqualStrings("a_top.bit", b.name);
    try testing.expectEqualStrings("xyz", b.data);
    try testing.expect(bitInTar(tar[0..512]) == null);
}

test "the build's own bitstreams parse, swap and have the sync word (skipped without them)" {
    const io = testing.io;
    const gpa = testing.allocator;
    var found: u32 = 0;
    // every system's on both boards in this worktree's build/ (the Arty's
    // under build/arty-z7-20/ since 2026-10-06)
    const names = [_][]const u8{ "chip8", "gb", "gbc", "nes", "a800xl", "gg", "sms", "m0e" };
    for (names) |n| {
        for ([_]struct { []const u8, []const u8 }{ .{ "build", zedboard_part }, .{ "build/arty-z7-20", arty_part } }) |b| {
            const dir, const part = b;
            const path = try std.fmt.allocPrint(gpa, "{s}/{s}/vivado/{s}_zynq.bit", .{ dir, n, n });
            defer gpa.free(path);
            const bytes = std.Io.Dir.cwd().readFileAlloc(io, path, gpa, .limited(16 << 20)) catch continue;
            defer gpa.free(bytes);
            found += 1;
            try checkRealBit(bytes, part, path);
        }
    }
    // ... and the bitstream cache's builds that passed their gates (side
    // track K, 2026-10-15: shared by every worktree, so this runs wherever
    // a .bit was built in the last 14 days), a few of them
    const env = testing.environ;
    const base = if (env.getPosix("CI_CACHE_DIR")) |d| try gpa.dupe(u8, d) else if (env.getPosix("XDG_CACHE_HOME")) |x|
        try std.fmt.allocPrint(gpa, "{s}/emulation_clash/ci-cache", .{x})
    else if (env.getPosix("HOME")) |h| try std.fmt.allocPrint(gpa, "{s}/.cache/emulation_clash/ci-cache", .{h}) else null;
    if (base) |bd| {
        defer gpa.free(bd);
        const bit_dir = try std.fmt.allocPrint(gpa, "{s}/bit", .{bd});
        defer gpa.free(bit_dir);
        if (std.Io.Dir.cwd().openDir(io, bit_dir, .{ .iterate = true })) |d| {
            var dir = d;
            defer dir.close(io);
            var it = dir.iterate();
            var tried: u32 = 0;
            while (try it.next(io)) |e| {
                if (tried == 8) break;
                if (!std.mem.endsWith(u8, e.name, ".tar")) continue;
                const tar = dir.readFileAlloc(io, e.name, gpa, .limited(32 << 20)) catch continue;
                defer gpa.free(tar);
                const b = bitInTar(tar) orelse continue;
                tried += 1;
                found += 1;
                // the part says which board's: try each
                checkRealBit(b.data, zedboard_part, b.name) catch try checkRealBit(b.data, arty_part, b.name);
            }
        } else |_| {}
    }
    if (found == 0) return error.SkipZigTest;
}

/// The device configuration interface and the SLCR registers PCAP uses,
/// with the rules it enforces (UG585 6.4.4, B.16).
const Fake = struct {
    regs: std.AutoHashMap(u32, u32),
    time: u64 = 0,
    slcr_unlocked: bool = false,
    pl_done: bool = false,
    /// faults to simulate
    init_stuck: bool = false,
    bad_bitstream: bool = false,
    dma_error: bool = false,
    /// what the DMA was given
    dma: ?struct { src: u32, dst: u32, src_len: u32, dst_len: u32 } = null,
    violations: u32 = 0,
    /// the level shifters when the DMA ran
    lvl_at_dma: u32 = 0,

    fn init() Fake {
        var f: Fake = .{ .regs = std.AutoHashMap(u32, u32).init(testing.allocator) };
        // as the boot ROM leaves them: PCAP selected, debug (DAP) enabled
        f.set(dc_ctrl, 0x0C00_6000 | 0x7 | (1 << 3));
        f.set(dc_status, 0x4000_0820 | status_pcfg_init);
        f.set(dc_mctrl, (1 << 23) | mctrl_pcap_lpbk);
        f.set(dc_int_sts, 0);
        f.set(slcr_lvl_shftr_en, 0xF);
        f.set(slcr_fpga_rst_ctrl, 0);
        return f;
    }
    fn deinit(f: *Fake) void {
        f.regs.deinit();
    }
    fn get(f: *Fake, a: u32) u32 {
        return f.regs.get(a) orelse 0;
    }
    fn set(f: *Fake, a: u32, v: u32) void {
        f.regs.put(a, v) catch unreachable;
    }

    pub fn nowUs(f: *Fake) u64 {
        f.time += 1;
        return f.time;
    }
    pub fn read32(f: *Fake, a: u32) u32 {
        return f.get(a);
    }
    pub fn write32(f: *Fake, a: u32, v: u32) void {
        switch (a) {
            slcr_unlock => f.slcr_unlocked = v == 0xDF0D,
            slcr_lock => f.slcr_unlocked = false,
            slcr_lvl_shftr_en, slcr_fpga_rst_ctrl => {
                if (!f.slcr_unlocked) f.violations += 1 else f.set(a, v);
            },
            dc_unlock => if (v != unlock_word) {
                f.violations += 100; // the illegal access state
            },
            dc_ctrl => {
                const old = f.get(dc_ctrl);
                // the debug enables must survive
                if (v & 0x7F != old & 0x7F) f.violations += 1;
                if (old & ctrl_prog_b != 0 and v & ctrl_prog_b == 0) {
                    // PL cleared: INIT low, DONE low
                    f.set(dc_status, f.get(dc_status) & ~status_pcfg_init);
                    f.pl_done = false;
                }
                if (old & ctrl_prog_b == 0 and v & ctrl_prog_b != 0 and !f.init_stuck)
                    f.set(dc_status, f.get(dc_status) | status_pcfg_init);
                f.set(dc_ctrl, v);
            },
            dc_int_sts => {
                // write 1 to clear, except while the condition holds
                var s = f.get(dc_int_sts) & ~v;
                if (f.pl_done) s |= int_pcfg_done;
                f.set(dc_int_sts, s);
            },
            dc_dma_dst_len => {
                f.set(a, v);
                f.dma = .{ .src = f.get(dc_dma_src), .dst = f.get(dc_dma_dst), .src_len = f.get(dc_dma_src_len), .dst_len = v };
                f.lvl_at_dma = f.get(slcr_lvl_shftr_en);
                const c = f.get(dc_ctrl);
                // PCAP selected, full rate, no loopback, the PL ready
                if (c & ctrl_pcap_mode == 0 or c & ctrl_pcap_pr == 0 or c & ctrl_quarter_rate != 0) f.violations += 1;
                if (f.get(dc_mctrl) & mctrl_pcap_lpbk != 0) f.violations += 1;
                if (f.get(dc_status) & status_pcfg_init == 0) f.violations += 1;
                if (f.dma_error) {
                    f.set(dc_int_sts, f.get(dc_int_sts) | (1 << 21));
                    return;
                }
                f.set(dc_int_sts, f.get(dc_int_sts) | int_d_p_done | (1 << 13));
                if (!f.bad_bitstream) {
                    f.pl_done = true;
                    f.set(dc_int_sts, f.get(dc_int_sts) | int_pcfg_done);
                }
            },
            else => f.set(a, v),
        }
    }
};

test "PCAP: the sequence, the DMA command, level shifters and resets around it" {
    var f = Fake.init();
    defer f.deinit();
    var p: Pcap(Fake) = .{ .bus = &f };
    try p.program(ddr_addr, 1011391);
    try testing.expectEqual(@as(u32, 0), f.violations);
    const d = f.dma.?;
    try testing.expectEqual(ddr_addr | 1, d.src);
    try testing.expectEqual(@as(u32, 0xFFFF_FFFF), d.dst);
    try testing.expectEqual(@as(u32, 1011391), d.src_len);
    try testing.expectEqual(@as(u32, 1011391), d.dst_len);
    // PS-to-PL off while the PL changed; all on afterwards, resets released
    try testing.expectEqual(lvl_during, f.lvl_at_dma);
    try testing.expectEqual(lvl_after, f.get(slcr_lvl_shftr_en));
    try testing.expectEqual(@as(u32, 0), f.get(slcr_fpga_rst_ctrl));
    try testing.expect(f.pl_done);
    // again (switching systems): the PL is cleared and loaded once more
    f.dma = null;
    try p.program(ddr_addr, 100);
    try testing.expectEqual(@as(u32, 100), f.dma.?.src_len);
    try testing.expectEqual(@as(u32, 0), f.violations);
}

test "PCAP: failures are reported, not hung on" {
    {
        var f = Fake.init();
        defer f.deinit();
        f.init_stuck = true;
        var p: Pcap(Fake) = .{ .bus = &f };
        try testing.expectError(error.InitTimeout, p.program(ddr_addr, 100));
        try testing.expectEqual(@as(?@TypeOf(f.dma.?), null), f.dma);
    }
    {
        var f = Fake.init();
        defer f.deinit();
        f.bad_bitstream = true;
        var p: Pcap(Fake) = .{ .bus = &f };
        try testing.expectError(error.NoDone, p.program(ddr_addr, 100));
        // the PL stays cut off
        try testing.expectEqual(lvl_during, f.get(slcr_lvl_shftr_en));
    }
    {
        var f = Fake.init();
        defer f.deinit();
        f.dma_error = true;
        var p: Pcap(Fake) = .{ .bus = &f };
        try testing.expectError(error.DmaError, p.program(ddr_addr, 100));
        try testing.expect(p.err_bits & (1 << 21) != 0);
    }
}
