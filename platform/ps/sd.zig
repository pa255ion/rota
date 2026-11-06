//! SD card driver for the Zynq-7000 SD0 controller (bare metal, polled):
//! PIO through the buffer data port, no DMA, no interrupts; the 4-bit bus
//! when a check read agrees (else 1-bit), single-block writes, single- and
//! multi-block reads (side track F: bitstreams). Enough for loading ROMs from a FAT file
//! system (`readBlock` has the block-device shape the FAT reader takes) and
//! for writing a card image sent from the desktop (`writeBlock`, for users
//! without a card reader: relay.zig, `make sd`).
//!
//! The controller is SDHCI-compatible, version 2.00 (UG585 chapter 13; SD
//! Host Controller Simplified Specification, "SDHC" below, sections cited
//! as SDHC 2.2.x); card commands from the SD Physical Layer Simplified
//! Specification ("PLS", section 4). Registers sit at 0xE010_0000 (SD0).
//!
//! What the caller must have set up (on the ZedBoard, Vivado's ps7_init
//! does all of it): MIO 40 to 45 routed to SDIO0, card detect on MIO 47 and
//! write protect on MIO 46 (SDIO0_WP_CD_SEL), the SDIO reference clock at
//! 50 MHz (`base_clock_hz`) and the SDI0 AMBA clock on (APER_CLK_CTRL bit
//! 10). For `MmioBus.nowUs` the Cortex-A9 global timer must be running
//! (relay.zig starts it).
//!
//! Only 32-bit register accesses: 8- and 16-bit fields are changed by
//! read-modify-write of their word.
//!
//! The driver is generic over its register bus, `Sd(Bus)`, so that the host
//! tests at the end of this file can run it against a fake controller. A
//! Bus has `read32(*Bus, off: u32) u32`, `write32(*Bus, off: u32, v: u32)`
//! and `nowUs(*Bus) u64`.

const std = @import("std");

/// SD0 register base (UG585 appendix B, "SD Controller (sdio)").
pub const base: usize = 0xE010_0000;

/// SDIO reference clock from ps7_init (SDIO_CLK_CTRL = 0x1401: IO PLL
/// 1000 MHz / 20).
pub const base_clock_hz: u32 = 50_000_000;

// --- Registers (SDHC 2.2, offsets from `base`) ---------------------------
const reg_blk = 0x04; // block size [11:0], block count [31:16]   (2.2.2, 2.2.3)
const reg_arg = 0x08; // command argument                         (2.2.4)
const reg_cmd = 0x0C; // transfer mode [15:0], command [31:16]    (2.2.5, 2.2.6)
const reg_resp0 = 0x10; // response bits 39:8 of a 48-bit reply    (2.2.7)
const reg_data = 0x20; // buffer data port                          (2.2.8)
const reg_present = 0x24; // present state                          (2.2.9)
const reg_host = 0x28; // host control [7:0], power control [15:8] (2.2.10, 2.2.11)
const reg_clock = 0x2C; // clock control [15:0], timeout control [19:16],
//                         software reset [26:24]               (2.2.14 to 2.2.16)
const reg_status = 0x30; // normal [15:0], error [31:16]; write 1 to clear (2.2.17, 2.2.18)
const reg_status_en = 0x34; // status enables                         (2.2.19, 2.2.20)
const reg_signal_en = 0x38; // interrupt signal enables                (2.2.21, 2.2.22)

// Present state (2.2.9)
const ps_cmd_inhibit: u32 = 1 << 0;
const ps_dat_inhibit: u32 = 1 << 1;
const ps_card_inserted: u32 = 1 << 16;
const ps_card_stable: u32 = 1 << 17;
/// Write protect switch pin level (2.2.9 bit 19): 1 = writes allowed.
const ps_write_enabled: u32 = 1 << 19;

// Power control (2.2.11): bus voltage 3.3 V in [11:9], bus power on in 8.
const pwr_3v3: u32 = 0b111 << 9;
const pwr_on: u32 = 1 << 8;

// Clock control (2.2.14)
const clk_internal_en: u32 = 1 << 0;
const clk_internal_stable: u32 = 1 << 1;
const clk_sd_en: u32 = 1 << 2;
const clk_div_mask: u32 = 0xFF << 8;
// Timeout control (2.2.15): data timeout TMCLK x 2^27, the largest.
const timeout_mask: u32 = 0xF << 16;
const timeout_max: u32 = 0xE << 16;
// Software reset (2.2.16)
const rst_all: u32 = 1 << 24;
const rst_cmd: u32 = 1 << 25;
const rst_dat: u32 = 1 << 26;

// Normal interrupt status (2.2.17) and error interrupt status (2.2.18)
const st_cmd_done: u32 = 1 << 0;
const st_xfer_done: u32 = 1 << 1;
const st_buf_write_ready: u32 = 1 << 4;
const st_buf_read_ready: u32 = 1 << 5;
const st_error: u32 = 1 << 15;
const st_err_cmd_timeout: u32 = 1 << 16;
const st_err_data: u32 = 0x7 << 20; // data timeout, data CRC, data end bit

/// SDCLK divisor field values. SDHC 2.00 allows only powers of two in the
/// 8-bit "SDCLK frequency select" field: N gives base / (2N) (2.2.14).
/// 0x40: 50 MHz / 128 = 390.6 kHz for identification (at most 400 kHz,
/// PLS 4.2); 0x01: 25 MHz, default speed.
const div_ident: u8 = 0x40;
const div_transfer: u8 = 0x01;

// --- Commands (PLS 4.7.4; command register layout SDHC 2.2.6) -----------
// [13:8] index, [5] data present, [4] index check, [3] CRC check,
// [1:0] response: 0 none, 1 136 bits, 2 48 bits, 3 48 bits with busy.
const cmd0_go_idle: u16 = 0x0000;
const cmd2_all_send_cid: u16 = 0x0209; // R2
const cmd3_send_rca: u16 = 0x031A; // R6
const cmd7_select: u16 = 0x071B; // R1b
const cmd8_send_if_cond: u16 = 0x081A; // R7
const cmd16_set_blocklen: u16 = 0x101A; // R1
const cmd12_stop: u16 = 0x0C1B; // R1b
const cmd17_read_single: u16 = 0x113A; // R1, data
const cmd18_read_multi: u16 = 0x123A; // R1, data
const cmd24_write_single: u16 = 0x183A; // R1, data
const cmd25_write_multi: u16 = 0x193A; // R1, data
const cmd55_app_cmd: u16 = 0x371A; // R1
const acmd41_sd_send_op_cond: u16 = 0x2902; // R3: no CRC, no index
const acmd6_set_bus_width: u16 = 0x061A; // R1

/// Host control (2.2.10): data transfer width, 1 = 4-bit.
const host_4bit: u32 = 1 << 1;

/// Transfer mode (2.2.5): read (card to host) [4], block count enable [1].
const mode_read_single: u32 = 0x0012;
/// Multi-block read: also multi-block [5] and auto CMD12 [2] (the
/// controller stops the card after the last block).
const mode_read_multi: u32 = 0x0036;
/// Write (host to card): direction bit [4] clear, block count enable [1].
const mode_write_single: u32 = 0x0002;
/// Multi-block write: also multi-block [5] and auto CMD12 [2] (the
/// controller stops the card after the last block; side track G phase 2,
/// make sd's batched writes).
const mode_write_multi: u32 = 0x0026;

/// CMD8 argument: 2.7 to 3.6 V [11:8] = 1, check pattern 0xAA (PLS 4.3.13).
const if_cond_arg: u32 = 0x1AA;
/// ACMD41: the 2.7 to 3.6 V window of the OCR (PLS 5.1), and HCS [30].
const ocr_window: u32 = 0x00FF_8000;
const ocr_hcs: u32 = 1 << 30;
const ocr_ready: u32 = 1 << 31;
/// OCR bits 20 and 21: 3.2 to 3.4 V, which includes the board's 3.3 V.
const ocr_3v3: u32 = 0x3 << 20;

pub const Error = error{
    /// The lock switch on the card is set (writes only).
    WriteProtected,
    /// No card in the slot.
    NoCard,
    /// The card didn't answer a command, or the controller didn't finish
    /// in time.
    Timeout,
    /// CRC, end bit or index error on a command response.
    CommandError,
    /// Data timeout, CRC or end bit error on a read.
    DataError,
    /// The card answered, but not as this driver can use it (wrong CMD8
    /// echo, no 3.3 V in its OCR, a byte address past 4 GiB).
    Unsupported,
};

pub const CardType = enum {
    /// SD 1.x standard capacity: byte addressing.
    sdsc_v1,
    /// SD 2.0 standard capacity: byte addressing.
    sdsc_v2,
    /// SDHC/SDXC: block addressing.
    sdhc,
};

pub fn Sd(comptime Bus: type) type {
    return struct {
        const Self = @This();

        bus: *Bus,
        card: CardType,
        rca: u16,
        /// the 4-bit bus is in use (`tryWide`)
        wide: bool = false,

        /// Whether the controller sees a card (present state, 2.2.9).
        pub fn cardPresent(bus: *Bus) bool {
            return bus.read32(reg_present) & ps_card_inserted != 0;
        }

        /// Reset the controller and bring the card to the transfer state
        /// (PLS 4.2, figure 4-2), then switch to 25 MHz.
        pub fn init(bus: *Bus) Error!Self {
            var self = Self{ .bus = bus, .card = .sdsc_v1, .rca = 0 };

            // Reset everything (SDHC 3.2.? "software reset for all").
            self.setBits(reg_clock, rst_all);
            try self.waitClear(reg_clock, rst_all, 100_000);

            // Bus voltage first, then bus power (SDHC 3.3).
            self.write(reg_host, (self.read(reg_host) & ~@as(u32, 0xFF00)) | pwr_3v3);
            self.setBits(reg_host, pwr_on);

            // Data timeout, status bits on (we poll them; no interrupt
            // signals).
            self.write(reg_clock, (self.read(reg_clock) & ~timeout_mask) | timeout_max);
            self.write(reg_status_en, 0xFFFF_FFFF);
            self.write(reg_signal_en, 0);

            // Card detect: after the reset the controller debounces the
            // detect line again before "card state stable" (2.2.9 bit 17)
            // is set.
            // to verify on the board: how long the Zynq takes to report
            // stable after a reset (allowing 0.5 s); if it never does, we
            // go by "card inserted" alone.
            const t0 = self.bus.nowUs();
            while (self.read(reg_present) & ps_card_stable == 0 and self.bus.nowUs() - t0 < 500_000) {}
            if (!cardPresent(bus)) return error.NoCard;

            try self.setClock(div_ident);
            // At least 74 clocks before the first command (PLS 6.4.1):
            // 1 ms is about 390.
            self.delayUs(1000);

            _ = try self.command(cmd0_go_idle, 0, 0);

            // CMD8: a 2.0 card echoes the argument; a 1.x card doesn't
            // answer (PLS 4.2.2).
            var v2 = true;
            if (self.command(cmd8_send_if_cond, if_cond_arg, 0)) |r7| {
                if (r7 & 0xFFF != if_cond_arg) return error.Unsupported;
            } else |err| switch (err) {
                error.Timeout => v2 = false,
                else => return err,
            }

            // ACMD41 until the card leaves busy, for at most 1 s (PLS
            // 4.2.3); a 2.0 host sets HCS.
            const arg = ocr_window | (if (v2) ocr_hcs else 0);
            const t1 = self.bus.nowUs();
            var ocr: u32 = 0;
            while (true) {
                _ = try self.command(cmd55_app_cmd, 0, 0);
                ocr = try self.command(acmd41_sd_send_op_cond, arg, 0);
                if (ocr & ocr_ready != 0) break;
                if (self.bus.nowUs() - t1 > 1_000_000) return error.Timeout;
                self.delayUs(1000);
            }
            if (ocr & ocr_3v3 == 0) return error.Unsupported;
            self.card = if (!v2) .sdsc_v1 else if (ocr & ocr_hcs != 0) .sdhc else .sdsc_v2;

            _ = try self.command(cmd2_all_send_cid, 0, 0);
            self.rca = @truncate(try self.command(cmd3_send_rca, 0, 0) >> 16);
            _ = try self.command(cmd7_select, @as(u32, self.rca) << 16, 0);
            // SDHC blocks are always 512 bytes; standard capacity cards
            // default to their own block length (PLS 4.3.14).
            if (self.card != .sdhc) _ = try self.command(cmd16_set_blocklen, 512, 0);

            try self.setClock(div_transfer);
            self.tryWide();
            return self;
        }

        /// The 4-bit bus (ACMD6, PLS 4.7.4; host control bit 1, SDHC
        /// 2.2.10): four times the 1-bit rate (side track F). Block 0 is
        /// read both ways first; on an error or any difference the card
        /// and the controller go back to 1 bit, so a surprise in the
        /// wiring or the controller costs speed, never data.
        fn tryWide(self: *Self) void {
            var narrow: [512]u8 = undefined;
            self.readBlock(0, &narrow) catch return;
            if (self.setWidth(true)) |_| {
                var four: [512]u8 = undefined;
                if (self.readBlock(0, &four)) |_| {
                    if (std.mem.eql(u8, &narrow, &four)) {
                        self.wide = true;
                        return;
                    }
                } else |_| {}
            } else |_| {}
            self.setWidth(false) catch {};
        }

        fn setWidth(self: *Self, four: bool) Error!void {
            // the card first (CMD55 with its address: it is selected)
            _ = try self.command(cmd55_app_cmd, @as(u32, self.rca) << 16, 0);
            _ = try self.command(acmd6_set_bus_width, if (four) 2 else 0, 0);
            const h = self.read(reg_host);
            self.write(reg_host, if (four) h | host_4bit else h & ~host_4bit);
        }

        /// Read one 512-byte block (CMD17, PLS 4.3.3; SDHC 3.7.2.1 without
        /// DMA). `lba` is the block number on the card.
        pub fn readBlock(self: *Self, lba: u32, buf: *[512]u8) Error!void {
            if (!cardPresent(self.bus)) return error.NoCard;
            const addr = if (self.card == .sdhc)
                lba
            else
                std.math.mul(u32, lba, 512) catch return error.Unsupported;

            self.write(reg_blk, 512 | 1 << 16);
            _ = try self.command(cmd17_read_single, addr, mode_read_single);

            try self.waitStatus(st_buf_read_ready, 500_000, error.DataError);
            self.write(reg_status, st_buf_read_ready);
            var i: usize = 0;
            while (i < 128) : (i += 1) {
                std.mem.writeInt(u32, buf[i * 4 ..][0..4], self.read(reg_data), .little);
            }
            try self.waitStatus(st_xfer_done, 500_000, error.DataError);
            self.write(reg_status, st_xfer_done);
        }

        /// Read `count` consecutive blocks from `lba` (CMD18 with auto
        /// CMD12, PLS 4.3.3; SDHC 3.7.2.1 without DMA): the card's access
        /// time is paid once for the run instead of once per block (side
        /// track F: a bitstream read block by block took 6 s). Each block
        /// goes to `sink.block(*const [512]u8)` as it arrives. On an error
        /// part way the card is stopped (CMD12), so the next read works.
        pub fn readBlocks(self: *Self, lba: u32, count: u16, sink: anytype) Error!void {
            if (count == 0) return;
            if (!cardPresent(self.bus)) return error.NoCard;
            const addr = if (self.card == .sdhc)
                lba
            else
                std.math.mul(u32, lba, 512) catch return error.Unsupported;

            self.write(reg_blk, 512 | @as(u32, count) << 16);
            _ = self.command(cmd18_read_multi, addr, mode_read_multi) catch |e| return self.stopAfter(e);
            var buf: [512]u8 = undefined;
            var k: u32 = 0;
            while (k < count) : (k += 1) {
                self.waitStatus(st_buf_read_ready, 500_000, error.DataError) catch |e| return self.stopAfter(e);
                self.write(reg_status, st_buf_read_ready);
                var i: usize = 0;
                while (i < 128) : (i += 1) {
                    std.mem.writeInt(u32, buf[i * 4 ..][0..4], self.read(reg_data), .little);
                }
                sink.block(&buf);
            }
            self.waitStatus(st_xfer_done, 500_000, error.DataError) catch |e| return self.stopAfter(e);
            self.write(reg_status, st_xfer_done);
        }

        /// A multi-block read failed: stop the card's transmission (CMD12,
        /// PLS 4.3.3), whatever state it is in, then report `e`.
        fn stopAfter(self: *Self, e: Error) Error {
            _ = self.command(cmd12_stop, 0, 0) catch {};
            return e;
        }

        /// Write one 512-byte block (CMD24, PLS 4.3.4; SDHC 3.7.2.1
        /// without DMA). Returns once the card has finished programming it:
        /// for writes, transfer complete comes after the card's busy signal
        /// on DAT0 ends (SDHC 2.2.17 bit 1). Callers that care read the
        /// block back (relay.zig does).
        pub fn writeBlock(self: *Self, lba: u32, buf: *const [512]u8) Error!void {
            const ps = self.read(reg_present);
            if (ps & ps_card_inserted == 0) return error.NoCard;
            // to verify on the board: the lock switch level reaches bit 19
            // through MIO 46 (SDIO0_WP_CD_SEL); 1 = unlocked.
            if (ps & ps_write_enabled == 0) return error.WriteProtected;
            const addr = if (self.card == .sdhc)
                lba
            else
                std.math.mul(u32, lba, 512) catch return error.Unsupported;

            self.write(reg_blk, 512 | 1 << 16);
            _ = try self.command(cmd24_write_single, addr, mode_write_single);

            try self.waitStatus(st_buf_write_ready, 500_000, error.DataError);
            self.write(reg_status, st_buf_write_ready);
            var i: usize = 0;
            while (i < 128) : (i += 1) {
                self.write(reg_data, std.mem.readInt(u32, buf[i * 4 ..][0..4], .little));
            }
            // programming can take a while (PLS 4.6.2.2: up to 250 ms)
            try self.waitStatus(st_xfer_done, 1_000_000, error.DataError);
            self.write(reg_status, st_xfer_done);
        }

        /// Write `data` (a whole number of blocks, at most 65535) to the
        /// blocks from `lba` (CMD25 with auto CMD12, PLS 4.3.4; SDHC
        /// 3.7.2.1 without DMA): the card's programming time is paid once
        /// for the run (make sd's 16-block batches, side track G phase 2).
        /// Returns once the card has finished; on an error part way the
        /// card is stopped (CMD12). Callers read the blocks back.
        pub fn writeBlocks(self: *Self, lba: u32, data: []const u8) Error!void {
            const count = data.len / 512;
            if (count == 0 or data.len % 512 != 0 or count > 0xFFFF) return error.Unsupported;
            if (count == 1) return self.writeBlock(lba, data[0..512]);
            const ps = self.read(reg_present);
            if (ps & ps_card_inserted == 0) return error.NoCard;
            if (ps & ps_write_enabled == 0) return error.WriteProtected;
            const addr = if (self.card == .sdhc)
                lba
            else
                std.math.mul(u32, lba, 512) catch return error.Unsupported;

            self.write(reg_blk, 512 | @as(u32, @intCast(count)) << 16);
            _ = self.command(cmd25_write_multi, addr, mode_write_multi) catch |e| return self.stopAfter(e);
            var k: usize = 0;
            while (k < count) : (k += 1) {
                self.waitStatus(st_buf_write_ready, 500_000, error.DataError) catch |e| return self.stopAfter(e);
                self.write(reg_status, st_buf_write_ready);
                const blk = data[k * 512 ..][0..512];
                var i: usize = 0;
                while (i < 128) : (i += 1) {
                    self.write(reg_data, std.mem.readInt(u32, blk[i * 4 ..][0..4], .little));
                }
            }
            // programming can take a while (PLS 4.6.2.2: up to 250 ms a block
            // in the worst case; in practice the card buffers the run)
            self.waitStatus(st_xfer_done, 2_000_000, error.DataError) catch |e| return self.stopAfter(e);
            self.write(reg_status, st_xfer_done);
        }

        // --- internals ---------------------------------------------------

        fn read(self: *Self, off: u32) u32 {
            return self.bus.read32(off);
        }

        fn write(self: *Self, off: u32, v: u32) void {
            self.bus.write32(off, v);
        }

        fn setBits(self: *Self, off: u32, bits: u32) void {
            self.write(off, self.read(off) | bits);
        }

        fn delayUs(self: *Self, us: u64) void {
            const t0 = self.bus.nowUs();
            while (self.bus.nowUs() - t0 < us) {}
        }

        fn waitClear(self: *Self, off: u32, bits: u32, timeout_us: u64) Error!void {
            const t0 = self.bus.nowUs();
            while (self.read(off) & bits != 0) {
                if (self.bus.nowUs() - t0 > timeout_us) return error.Timeout;
            }
        }

        /// Change the SD clock: stop it, set the divisor, wait for the
        /// internal clock to be stable, start it (SDHC 3.2.3).
        fn setClock(self: *Self, div: u8) Error!void {
            var c = self.read(reg_clock) & ~(clk_sd_en | rst_all | rst_cmd | rst_dat);
            self.write(reg_clock, c);
            c = (c & ~clk_div_mask) | @as(u32, div) << 8 | clk_internal_en;
            self.write(reg_clock, c);
            const t0 = self.bus.nowUs();
            while (self.read(reg_clock) & clk_internal_stable == 0) {
                if (self.bus.nowUs() - t0 > 20_000) return error.Timeout;
            }
            self.write(reg_clock, c | clk_sd_en);
        }

        /// After an error: clear every status bit and reset the command
        /// and data lines (SDHC 3.10, error recovery).
        fn recover(self: *Self) void {
            self.write(reg_status, 0xFFFF_FFFF);
            self.setBits(reg_clock, rst_cmd | rst_dat);
            self.waitClear(reg_clock, rst_cmd | rst_dat, 100_000) catch {};
        }

        /// Wait for a normal status bit; an error status ends the wait with
        /// the matching error (after recovery), so does `timeout_us`.
        fn waitStatus(self: *Self, bit: u32, timeout_us: u64, comptime on_err: Error) Error!void {
            const t0 = self.bus.nowUs();
            while (true) {
                const s = self.read(reg_status);
                if (s & st_error != 0) {
                    self.recover();
                    if (s & st_err_data != 0) return error.DataError;
                    if (s & st_err_cmd_timeout != 0) return error.Timeout;
                    return on_err;
                }
                if (s & bit != 0) return;
                if (self.bus.nowUs() - t0 > timeout_us) {
                    self.recover();
                    return error.Timeout;
                }
            }
        }

        /// Issue one command (SDHC 3.7.1) and return response bits 39:8
        /// (RESP[31:0]). For R1b, also wait for the end of busy (transfer
        /// complete).
        fn command(self: *Self, cmd: u16, arg: u32, mode: u32) Error!u32 {
            const data = cmd & 0x20 != 0;
            const busy = cmd & 0x3 == 0x3;
            const inhibit = ps_cmd_inhibit | (if (data or busy) ps_dat_inhibit else 0);
            try self.waitClear(reg_present, inhibit, 100_000);

            self.write(reg_status, 0xFFFF_FFFF);
            self.write(reg_arg, arg);
            self.write(reg_cmd, @as(u32, cmd) << 16 | mode);

            const t0 = self.bus.nowUs();
            while (true) {
                const s = self.read(reg_status);
                if (s & st_error != 0) {
                    self.recover();
                    if (s & st_err_cmd_timeout != 0) return error.Timeout;
                    if (s & st_err_data != 0) return error.DataError;
                    return error.CommandError;
                }
                if (s & st_cmd_done != 0) break;
                if (self.bus.nowUs() - t0 > 100_000) {
                    self.recover();
                    return error.Timeout;
                }
            }
            self.write(reg_status, st_cmd_done);
            const resp = self.read(reg_resp0);
            if (busy) {
                // to verify on the board: transfer complete marks the end
                // of busy for R1b without data (SDHC 2.2.17 bit 1).
                try self.waitStatus(st_xfer_done, 500_000, error.Timeout);
                self.write(reg_status, st_xfer_done);
            }
            return resp;
        }
    };
}

/// The real controller, through volatile pointers; time from the
/// Cortex-A9 global timer (333.333 MHz on the ZedBoard preset, so
/// microseconds = ticks * 3 / 1000).
pub const MmioBus = struct {
    const gtimer_lo = 0xF8F0_0200;
    const gtimer_hi = 0xF8F0_0204;

    pub fn read32(_: *MmioBus, off: u32) u32 {
        const p: *volatile u32 = @ptrFromInt(base + off);
        return p.*;
    }

    pub fn write32(_: *MmioBus, off: u32, v: u32) void {
        const p: *volatile u32 = @ptrFromInt(base + off);
        p.* = v;
    }

    pub fn nowUs(_: *MmioBus) u64 {
        const hi_p: *volatile u32 = @ptrFromInt(gtimer_hi);
        const lo_p: *volatile u32 = @ptrFromInt(gtimer_lo);
        while (true) {
            const hi = hi_p.*;
            const lo = lo_p.*;
            if (hi_p.* == hi) return (@as(u64, hi) << 32 | lo) * 3 / 1000;
        }
    }
};

// --- Host tests: the driver against a fake controller ----------------------

const testing = std.testing;

/// A fake SDHCI controller with a card behind it: enough of SDHC 2.2 and
/// the PLS card state machine (idle -> ready -> ident -> stby -> tran) to
/// catch wrong command words, out-of-order commands, wrong clock settings
/// and byte/block addressing mix-ups. Every register access and every
/// nowUs call advances time by 1 us. Anything the real hardware would
/// reject is counted in `violations`.
const Fake = struct {
    const Kind = enum { sdhc, sdsc_v2, sdsc_v1 };
    const State = enum { idle, ready, ident, stby, tran };

    const Card = struct {
        kind: Kind,
        image: []u8,
        never_ready: bool = false,
        locked: bool = false,
        /// Reads still to fail with a data CRC error.
        corrupt_reads: u32 = 0,
        state: State = .idle,
        app: bool = false,
        cmd8: bool = false,
        polls: u32 = 0,
        blocklen_set: bool = false,
        rca: u16 = 0xB00F,
        /// the card's bus width is 4 (ACMD6)
        wide: bool = false,
        /// faults: the card ignores ACMD6; or DAT1 to DAT3 are broken, so
        /// 4-bit data arrives wrong (with a valid CRC: only a compare
        /// catches it)
        no_wide: bool = false,
        wide_broken: bool = false,
    };

    regs: [64]u32 = [_]u32{0} ** 64,
    time: u64 = 0,
    card: ?Card,
    /// Card detect is stable 2 ms after this.
    debounce_from: u64 = 0,
    buf: [512]u8 = undefined,
    pos: usize = 0,
    reading: bool = false,
    /// a CMD24 is waiting for its data
    writing: bool = false,
    /// blocks of the current write still to come (CMD25: from the count)
    write_left: u32 = 0,
    write_offset: usize = 0,
    writes: u32 = 0,
    violations: u32 = 0,
    last_read_arg: ?u32 = null,
    /// blocks of the current read still to come (CMD18: from the count)
    multi_left: u32 = 0,
    read_offset: usize = 0,
    /// CMD12s seen
    stops: u32 = 0,

    fn fillBuf(f: *Fake) void {
        const card = f.card.?;
        for (&f.buf, 0..) |*b, i| {
            b.* = if (f.read_offset + i < card.image.len) card.image[f.read_offset + i] else 0;
            if (card.wide and card.wide_broken) b.* ^= 0x5A;
        }
        f.pos = 0;
    }

    fn bad(f: *Fake, comptime msg: []const u8, args: anytype) void {
        std.debug.print("fake sdhci: " ++ msg ++ "\n", args);
        f.violations += 1;
    }

    fn r(f: *Fake, off: u32) *u32 {
        return &f.regs[off >> 2];
    }

    pub fn nowUs(f: *Fake) u64 {
        f.time += 1;
        return f.time;
    }

    pub fn read32(f: *Fake, off: u32) u32 {
        f.time += 1;
        switch (off) {
            reg_present => {
                var v: u32 = 0;
                if (f.card != null) {
                    v |= ps_card_inserted | 1 << 18;
                    if (f.time >= f.debounce_from + 2000) v |= ps_card_stable;
                    if (!f.card.?.locked) v |= ps_write_enabled;
                }
                if (f.reading or f.writing) v |= ps_dat_inhibit;
                return v;
            },
            reg_clock => {
                // software reset bits read as set once, then clear
                const v = f.r(reg_clock).*;
                f.r(reg_clock).* &= ~(rst_all | rst_cmd | rst_dat);
                return (v & ~clk_internal_stable) | (if (v & clk_internal_en != 0) clk_internal_stable else 0);
            },
            reg_status => {
                const s = f.r(reg_status).*;
                return s | (if (s >> 16 != 0) st_error else 0);
            },
            reg_data => {
                if (!f.reading or f.pos >= 512) {
                    f.bad("data port read with no data", .{});
                    return 0;
                }
                if (f.pos == 0 and f.r(reg_status).* & st_buf_read_ready != 0)
                    f.bad("data port read before buffer read ready was acknowledged", .{});
                const w = std.mem.readInt(u32, f.buf[f.pos..][0..4], .little);
                f.pos += 4;
                if (f.pos == 512) {
                    if (f.multi_left > 1) {
                        // CMD18: the next block (auto CMD12 after the last)
                        f.multi_left -= 1;
                        f.read_offset += 512;
                        f.fillBuf();
                        f.r(reg_status).* |= st_buf_read_ready;
                    } else {
                        f.multi_left = 0;
                        f.reading = false;
                        f.r(reg_status).* |= st_xfer_done;
                    }
                }
                return w;
            },
            else => return f.r(off).*,
        }
    }

    pub fn write32(f: *Fake, off: u32, v: u32) void {
        f.time += 1;
        switch (off) {
            reg_status => f.r(reg_status).* &= ~v,
            reg_data => {
                if (!f.writing or f.pos >= 512 or f.r(reg_status).* & st_buf_write_ready != 0) {
                    f.bad("data port write with no CMD24 buffer ready (or not acknowledged)", .{});
                    return;
                }
                std.mem.writeInt(u32, f.buf[f.pos..][0..4], v, .little);
                f.pos += 4;
                if (f.pos == 512) {
                    const card = &f.card.?;
                    @memcpy(card.image[f.write_offset..][0..512], &f.buf);
                    f.writes += 1;
                    if (f.write_left > 1) {
                        // CMD25: the next block's buffer
                        f.write_left -= 1;
                        f.write_offset += 512;
                        f.pos = 0;
                        if (f.write_offset + 512 > card.image.len) {
                            f.bad("CMD25 ran past the end of the card", .{});
                            f.writing = false;
                            return;
                        }
                        f.r(reg_status).* |= st_buf_write_ready;
                    } else {
                        f.writing = false;
                        f.write_left = 0;
                        f.r(reg_status).* |= st_xfer_done;
                    }
                }
            },
            reg_clock => {
                if (v & rst_all != 0) {
                    f.regs = [_]u32{0} ** 64;
                    f.reading = false;
                    f.debounce_from = f.time;
                }
                if (v & rst_dat != 0) {
                    f.reading = false;
                    f.writing = false;
                }
                f.r(reg_clock).* = v;
            },
            reg_cmd => {
                f.r(reg_cmd).* = v;
                f.execute(@truncate(v >> 16), v & 0xFFFF, f.r(reg_arg).*);
            },
            else => f.r(off).* = v,
        }
    }

    fn sdClockHz(f: *Fake) ?u32 {
        const c = f.r(reg_clock).*;
        if (c & clk_internal_en == 0 or c & clk_sd_en == 0) return null;
        const n = (c >> 8) & 0xFF;
        if (n != 0 and n & (n - 1) != 0) {
            f.bad("divisor 0x{X} is not a power of two (SDHC 2.00)", .{n});
            return null;
        }
        return if (n == 0) base_clock_hz else base_clock_hz / (2 * n);
    }

    fn expectWord(f: *Fake, index: u32, got: u16, want: u16) bool {
        if (got != want) f.bad("CMD{d}: command word 0x{X:0>4}, expected 0x{X:0>4}", .{ index, got, want });
        return got == want;
    }

    const Reply = union(enum) { none, ok: u32, ok_busy: u32 };

    fn execute(f: *Fake, cmd: u16, mode: u32, arg: u32) void {
        const st = f.r(reg_status);
        const pwr = f.r(reg_host).*;
        if (pwr & pwr_on == 0 or pwr & (0b111 << 9) != pwr_3v3) {
            f.bad("command with bus power off or not 3.3 V", .{});
            st.* |= st_err_cmd_timeout;
            return;
        }
        if (f.r(reg_status_en).* & 0x007F_0023 != 0x007F_0023) f.bad("status bits not enabled", .{});
        const hz = f.sdClockHz() orelse {
            f.bad("command with the SD clock off", .{});
            st.* |= st_err_cmd_timeout;
            return;
        };
        const card = if (f.card) |*c| c else {
            st.* |= st_err_cmd_timeout;
            return;
        };
        const index: u32 = (cmd >> 8) & 0x3F;
        const app = card.app;
        card.app = false;
        if (card.state != .stby and card.state != .tran and hz > 400_000)
            f.bad("CMD{d} during identification at {d} Hz", .{ index, hz });

        const reply: Reply = switch (index) {
            0 => blk: {
                _ = f.expectWord(0, cmd, cmd0_go_idle);
                card.* = .{ .kind = card.kind, .image = card.image, .never_ready = card.never_ready, .locked = card.locked, .corrupt_reads = card.corrupt_reads, .rca = card.rca, .no_wide = card.no_wide, .wide_broken = card.wide_broken };
                break :blk .none;
            },
            8 => blk: {
                _ = f.expectWord(8, cmd, cmd8_send_if_cond);
                if (card.kind == .sdsc_v1) break :blk .none; // illegal for 1.x: no answer
                if (card.state != .idle) {
                    f.bad("CMD8 outside idle", .{});
                    break :blk .none;
                }
                card.cmd8 = true;
                break :blk .{ .ok = arg & 0xFFF };
            },
            55 => blk: {
                _ = f.expectWord(55, cmd, cmd55_app_cmd);
                // addressed once the card has an RCA (PLS 4.3.9)
                if ((card.state == .stby or card.state == .tran) and arg >> 16 != card.rca) {
                    f.bad("CMD55 with RCA 0x{X} in {s}", .{ arg >> 16, @tagName(card.state) });
                    break :blk .none;
                }
                card.app = true;
                break :blk .{ .ok = 1 << 5 };
            },
            41 => blk: {
                if (!app) {
                    f.bad("CMD41 without CMD55", .{});
                    break :blk .none;
                }
                _ = f.expectWord(41, cmd, acmd41_sd_send_op_cond);
                if (card.state != .idle) {
                    f.bad("ACMD41 outside idle", .{});
                    break :blk .none;
                }
                var ocr: u32 = ocr_window;
                const hcs_ok = card.kind != .sdhc or (card.cmd8 and arg & ocr_hcs != 0);
                card.polls += 1;
                if (!card.never_ready and hcs_ok and card.polls >= 3) {
                    ocr |= ocr_ready;
                    if (card.kind == .sdhc) ocr |= ocr_hcs;
                    card.state = .ready;
                }
                break :blk .{ .ok = ocr };
            },
            2 => blk: {
                _ = f.expectWord(2, cmd, cmd2_all_send_cid);
                if (card.state != .ready) {
                    f.bad("CMD2 outside ready", .{});
                    break :blk .none;
                }
                card.state = .ident;
                break :blk .{ .ok = 0x1234_5678 };
            },
            3 => blk: {
                _ = f.expectWord(3, cmd, cmd3_send_rca);
                if (card.state != .ident) {
                    f.bad("CMD3 outside ident", .{});
                    break :blk .none;
                }
                card.state = .stby;
                break :blk .{ .ok = @as(u32, card.rca) << 16 | 0x0500 };
            },
            7 => blk: {
                _ = f.expectWord(7, cmd, cmd7_select);
                if (card.state != .stby or arg >> 16 != card.rca) {
                    f.bad("CMD7 in {s} with RCA 0x{X}", .{ @tagName(card.state), arg >> 16 });
                    break :blk .none;
                }
                card.state = .tran;
                break :blk .{ .ok_busy = 0x0700 };
            },
            16 => blk: {
                _ = f.expectWord(16, cmd, cmd16_set_blocklen);
                if (card.state != .tran) f.bad("CMD16 outside tran", .{});
                if (card.kind == .sdhc) f.bad("CMD16 sent to an SDHC card (harmless, but unexpected)", .{});
                if (arg != 512) f.bad("CMD16 block length {d}", .{arg});
                card.blocklen_set = true;
                break :blk .{ .ok = 0x0900 };
            },
            6 => blk: {
                if (!app) {
                    f.bad("CMD6 (switch function) not expected: ACMD6 needs CMD55", .{});
                    break :blk .none;
                }
                _ = f.expectWord(6, cmd, acmd6_set_bus_width);
                if (card.state != .tran) f.bad("ACMD6 outside tran", .{});
                if (card.no_wide) break :blk .none;
                switch (arg & 3) {
                    0 => card.wide = false,
                    2 => card.wide = true,
                    else => f.bad("ACMD6 argument {d}", .{arg}),
                }
                break :blk .{ .ok = 0x0920 };
            },
            12 => blk: {
                _ = f.expectWord(12, cmd, cmd12_stop);
                if (card.state != .tran) f.bad("CMD12 outside tran", .{});
                f.reading = false;
                f.multi_left = 0;
                f.stops += 1;
                break :blk .{ .ok_busy = 0x0900 };
            },
            17, 18 => {
                const multi = index == 18;
                _ = f.expectWord(index, cmd, if (multi) cmd18_read_multi else cmd17_read_single);
                if (card.state != .tran) {
                    f.bad("CMD{d} outside tran", .{index});
                    st.* |= st_err_cmd_timeout;
                    return;
                }
                if (card.kind != .sdhc and !card.blocklen_set) f.bad("CMD{d} on SDSC before CMD16", .{index});
                if (hz < 20_000_000) f.bad("CMD{d} at {d} Hz: clock never raised", .{ index, hz });
                if (mode != (if (multi) mode_read_multi else mode_read_single)) f.bad("CMD{d} transfer mode 0x{X}", .{ index, mode });
                const blk = f.r(reg_blk).*;
                if (blk & 0xFFF != 512 or (if (multi) blk >> 16 == 0 else blk >> 16 != 1))
                    f.bad("CMD{d} block size/count 0x{X}", .{ index, blk });
                f.multi_left = if (multi) blk >> 16 else 1;
                f.last_read_arg = arg;
                const offset: usize = if (card.kind == .sdhc) @as(usize, arg) * 512 else blk: {
                    if (arg % 512 != 0) f.bad("unaligned byte address {d}", .{arg});
                    break :blk arg;
                };
                f.r(reg_resp0).* = 0x0900;
                st.* |= st_cmd_done;
                if (card.corrupt_reads > 0) {
                    card.corrupt_reads -= 1;
                    st.* |= 1 << 21; // data CRC error
                    return;
                }
                // host and card on different bus widths: garbage, CRC error
                if ((f.r(reg_host).* & host_4bit != 0) != card.wide) {
                    st.* |= 1 << 21;
                    return;
                }
                f.read_offset = offset;
                f.fillBuf();
                f.reading = true;
                st.* |= st_buf_read_ready;
                return;
            },
            24, 25 => {
                const multi = index == 25;
                _ = f.expectWord(index, cmd, if (multi) cmd25_write_multi else cmd24_write_single);
                if (card.state != .tran) {
                    f.bad("CMD{d} outside tran", .{index});
                    st.* |= st_err_cmd_timeout;
                    return;
                }
                if (card.locked) f.bad("CMD{d} to a write-protected card", .{index});
                if (card.kind != .sdhc and !card.blocklen_set) f.bad("CMD{d} on SDSC before CMD16", .{index});
                if (mode != (if (multi) mode_write_multi else mode_write_single)) f.bad("CMD{d} transfer mode 0x{X}", .{ index, mode });
                const blk = f.r(reg_blk).*;
                if (blk & 0xFFF != 512 or (if (multi) blk >> 16 < 2 else blk >> 16 != 1))
                    f.bad("CMD{d} block size/count 0x{X}", .{ index, blk });
                const offset: usize = if (card.kind == .sdhc) @as(usize, arg) * 512 else arg;
                if (offset % 512 != 0 or offset + 512 * @as(usize, blk >> 16) > card.image.len) {
                    f.bad("CMD{d} address {d} outside the card", .{ index, arg });
                    st.* |= 1 << 20; // data timeout
                    return;
                }
                f.r(reg_resp0).* = 0x0900;
                st.* |= st_cmd_done | st_buf_write_ready;
                f.write_offset = offset;
                f.write_left = blk >> 16;
                f.pos = 0;
                f.writing = true;
                return;
            },
            else => blk: {
                f.bad("unexpected CMD{d}", .{index});
                break :blk .none;
            },
        };
        switch (reply) {
            // no reply: a command that expects one times out
            .none => st.* |= if (cmd & 0x3 == 0) st_cmd_done else st_err_cmd_timeout,
            .ok => |v| {
                f.r(reg_resp0).* = v;
                st.* |= st_cmd_done;
            },
            .ok_busy => |v| {
                f.r(reg_resp0).* = v;
                st.* |= st_cmd_done | st_xfer_done;
            },
        }
    }
};

const TestSd = Sd(Fake);

fn testImage() [4 * 512]u8 {
    var img: [4 * 512]u8 = undefined;
    for (&img, 0..) |*b, i| b.* = @truncate(i * 7 + (i >> 9) * 13 + 1);
    return img;
}

fn expectBlock(sd: *TestSd, img: []const u8, lba: u32) !void {
    var buf: [512]u8 = undefined;
    try sd.readBlock(lba, &buf);
    try testing.expectEqualSlices(u8, img[lba * 512 ..][0..512], &buf);
}

test "SDHC 2.0 card: block addressing" {
    var img = testImage();
    var f = Fake{ .card = .{ .kind = .sdhc, .image = &img } };
    var sd = try TestSd.init(&f);
    try testing.expectEqual(CardType.sdhc, sd.card);
    try testing.expectEqual(@as(u16, 0xB00F), sd.rca);
    try expectBlock(&sd, &img, 3);
    try testing.expectEqual(@as(?u32, 3), f.last_read_arg);
    try expectBlock(&sd, &img, 0);
    try testing.expectEqual(@as(u32, 0), f.violations);
}

/// Collects the blocks a multi-block read hands over.
const Collect = struct {
    out: [4 * 512]u8 = undefined,
    n: usize = 0,
    pub fn block(c: *Collect, b: *const [512]u8) void {
        @memcpy(c.out[c.n * 512 ..][0..512], b);
        c.n += 1;
    }
};

test "multi-block reads: CMD18 with auto CMD12, the blocks in order (SDHC and SDSC); single reads after" {
    for ([_]Fake.Kind{ .sdhc, .sdsc_v2 }) |kind| {
        var img = testImage();
        var f = Fake{ .card = .{ .kind = kind, .image = &img } };
        var sd = try TestSd.init(&f);
        var c: Collect = .{};
        try sd.readBlocks(1, 3, &c);
        try testing.expectEqual(@as(usize, 3), c.n);
        try testing.expectEqualSlices(u8, img[512..], c.out[0 .. 3 * 512]);
        try testing.expectEqual(@as(?u32, if (kind == .sdhc) 1 else 512), f.last_read_arg);
        c = .{};
        try sd.readBlocks(2, 1, &c);
        try testing.expectEqualSlices(u8, img[1024..1536], c.out[0..512]);
        try sd.readBlocks(0, 0, &c); // nothing
        try expectBlock(&sd, &img, 3);
        try testing.expectEqual(@as(u32, 0), f.stops);
        try testing.expectEqual(@as(u32, 0), f.violations);
    }
}

test "4-bit bus: on for every card type; back to 1 bit if the card ignores it or the data differs" {
    for ([_]Fake.Kind{ .sdhc, .sdsc_v2, .sdsc_v1 }) |kind| {
        var img = testImage();
        var f = Fake{ .card = .{ .kind = kind, .image = &img } };
        var sd = try TestSd.init(&f);
        try testing.expect(sd.wide);
        try testing.expect(f.card.?.wide);
        try testing.expect(f.r(reg_host).* & host_4bit != 0);
        try expectBlock(&sd, &img, 2);
        try testing.expectEqual(@as(u32, 0), f.violations);
    }
    for ([_]bool{ false, true }) |broken| {
        var img = testImage();
        var f = Fake{ .card = .{ .kind = .sdhc, .image = &img, .no_wide = !broken, .wide_broken = broken } };
        var sd = try TestSd.init(&f);
        try testing.expect(!sd.wide);
        try testing.expect(!f.card.?.wide);
        try testing.expect(f.r(reg_host).* & host_4bit == 0);
        try expectBlock(&sd, &img, 1);
        var c: Collect = .{};
        try sd.readBlocks(0, 4, &c);
        try testing.expectEqualSlices(u8, &img, &c.out);
        try testing.expectEqual(@as(u32, 0), f.violations);
    }
}

test "a multi-block read that fails is stopped (CMD12); the next one works" {
    var img = testImage();
    var f = Fake{ .card = .{ .kind = .sdhc, .image = &img } };
    var sd = try TestSd.init(&f);
    f.card.?.corrupt_reads = 1;
    var c: Collect = .{};
    try testing.expectError(error.DataError, sd.readBlocks(0, 4, &c));
    try testing.expectEqual(@as(u32, 1), f.stops);
    c = .{};
    try sd.readBlocks(0, 4, &c);
    try testing.expectEqualSlices(u8, &img, &c.out);
    try testing.expectEqual(@as(u32, 0), f.violations);
}

test "SDSC 2.0 card: byte addressing" {
    var img = testImage();
    var f = Fake{ .card = .{ .kind = .sdsc_v2, .image = &img } };
    var sd = try TestSd.init(&f);
    try testing.expectEqual(CardType.sdsc_v2, sd.card);
    try expectBlock(&sd, &img, 2);
    try testing.expectEqual(@as(?u32, 1024), f.last_read_arg);
    try testing.expectEqual(@as(u32, 0), f.violations);
}

test "SD 1.x card: no CMD8 reply" {
    var img = testImage();
    var f = Fake{ .card = .{ .kind = .sdsc_v1, .image = &img } };
    var sd = try TestSd.init(&f);
    try testing.expectEqual(CardType.sdsc_v1, sd.card);
    try expectBlock(&sd, &img, 1);
    try testing.expectEqual(@as(?u32, 512), f.last_read_arg);
    try testing.expectEqual(@as(u32, 0), f.violations);
}

test "no card" {
    var f = Fake{ .card = null };
    try testing.expect(!TestSd.cardPresent(&f));
    try testing.expectError(error.NoCard, TestSd.init(&f));
    try testing.expectEqual(@as(u32, 0), f.violations);
}

test "a card that stays busy times out after about 1 s" {
    var img = testImage();
    var f = Fake{ .card = .{ .kind = .sdhc, .image = &img, .never_ready = true } };
    try testing.expectError(error.Timeout, TestSd.init(&f));
    try testing.expect(f.time >= 1_000_000 and f.time < 3_000_000);
    try testing.expectEqual(@as(u32, 0), f.violations);
}

test "a data CRC error, then the next read works" {
    var img = testImage();
    var f = Fake{ .card = .{ .kind = .sdhc, .image = &img } };
    var sd = try TestSd.init(&f);
    f.card.?.corrupt_reads = 1;
    var buf: [512]u8 = undefined;
    try testing.expectError(error.DataError, sd.readBlock(1, &buf));
    try expectBlock(&sd, &img, 1);
    try expectBlock(&sd, &img, 2);
    try testing.expectEqual(@as(u32, 0), f.violations);
}

test "card removed after init" {
    var img = testImage();
    var f = Fake{ .card = .{ .kind = .sdhc, .image = &img } };
    var sd = try TestSd.init(&f);
    f.card = null;
    var buf: [512]u8 = undefined;
    try testing.expectError(error.NoCard, sd.readBlock(0, &buf));
}

test "writes land at the right block (SDHC and SDSC), read back, other blocks untouched" {
    for ([_]Fake.Kind{ .sdhc, .sdsc_v2, .sdsc_v1 }) |kind| {
        var img = testImage();
        const orig = img;
        var f = Fake{ .card = .{ .kind = kind, .image = &img } };
        var sd = try TestSd.init(&f);
        var blk: [512]u8 = undefined;
        for (&blk, 0..) |*b, i| b.* = @truncate(i ^ 0x5A);
        try sd.writeBlock(2, &blk);
        try testing.expectEqual(@as(u32, 1), f.writes);
        try testing.expectEqualSlices(u8, &blk, img[1024..1536]);
        try testing.expectEqualSlices(u8, orig[0..1024], img[0..1024]);
        try testing.expectEqualSlices(u8, orig[1536..], img[1536..]);
        var back: [512]u8 = undefined;
        try sd.readBlock(2, &back);
        try testing.expectEqualSlices(u8, &blk, &back);
        try testing.expectEqual(@as(u32, 0), f.violations);
    }
}

test "write-protected card: error, nothing written" {
    var img = testImage();
    var f = Fake{ .card = .{ .kind = .sdhc, .image = &img, .locked = true } };
    var sd = try TestSd.init(&f);
    const blk = [_]u8{0} ** 512;
    try testing.expectError(error.WriteProtected, sd.writeBlock(0, &blk));
    try testing.expectEqual(@as(u32, 0), f.writes);
    try testing.expectEqual(@as(u32, 0), f.violations);
}

test "a write past the end of the card fails, the next write works" {
    var img = testImage();
    var f = Fake{ .card = .{ .kind = .sdhc, .image = &img } };
    var sd = try TestSd.init(&f);
    const blk = [_]u8{0xEE} ** 512;
    try testing.expectError(error.DataError, sd.writeBlock(4, &blk));
    f.violations -= 1; // the fake counts the out-of-range address
    try sd.writeBlock(3, &blk);
    try testing.expectEqualSlices(u8, &blk, img[1536..2048]);
    try testing.expectEqual(@as(u32, 0), f.violations);
}

test "multi-block writes (CMD25, auto CMD12): the blocks in order (SDHC and SDSC), others untouched" {
    for ([_]Fake.Kind{ .sdhc, .sdsc_v2, .sdsc_v1 }) |kind| {
        var img = testImage();
        const orig = img;
        var f = Fake{ .card = .{ .kind = kind, .image = &img } };
        var sd = try TestSd.init(&f);
        var data: [3 * 512]u8 = undefined;
        for (&data, 0..) |*b, i| b.* = @truncate(i * 7 + 3);
        try sd.writeBlocks(1, &data);
        try testing.expectEqual(@as(u32, 3), f.writes);
        try testing.expectEqualSlices(u8, &data, img[512..2048]);
        try testing.expectEqualSlices(u8, orig[0..512], img[0..512]);
        // one block is a plain CMD24
        try sd.writeBlocks(0, data[0..512]);
        try testing.expectEqualSlices(u8, data[0..512], img[0..512]);
        try testing.expectEqual(@as(u32, 0), f.violations);
    }
}

test "multi-block writes: write-protected card, nothing written; past the end, the next write works" {
    var img = testImage();
    var f = Fake{ .card = .{ .kind = .sdhc, .image = &img, .locked = true } };
    var sd = try TestSd.init(&f);
    const data = [_]u8{0x11} ** (2 * 512);
    try testing.expectError(error.WriteProtected, sd.writeBlocks(0, &data));
    try testing.expectEqual(@as(u32, 0), f.writes);
    var img2 = testImage();
    var g = Fake{ .card = .{ .kind = .sdhc, .image = &img2 } };
    var sd2 = try TestSd.init(&g);
    try testing.expectError(error.DataError, sd2.writeBlocks(3, &data));
    g.violations -= 1; // the fake counts the out-of-range address
    try sd2.writeBlocks(2, &data);
    try testing.expectEqualSlices(u8, &data, img2[1024..2048]);
    try testing.expectEqual(@as(u32, 0), g.violations);
}
