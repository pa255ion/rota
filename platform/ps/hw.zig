//! Zynq PS basics for the bare-metal app (Cortex-A9 core 0, in OCM):
//! registers, the global timer, the desktop's UART (ZedBoard: UART1, J14)
//! and the EMIO GPIO banks. Register addresses and bits from the Zynq-7000
//! TRM (UG585, appendix B). What differs between boards (the UART, the
//! timer's rate) comes from the board module (board_<board>.zig, chosen by
//! `zig build ps-app -Dboard=`; side track G phase 2).

const std = @import("std");
const board = @import("board");

// --- Registers -------------------------------------------------------------
pub fn reg(addr: usize) *volatile u32 {
    return @ptrFromInt(addr);
}

const slcr_unlock = 0xF800_0008;
const slcr_aper_clk_ctrl = 0xF800_012C; // bit 22: GPIO, 21: UART1, 20: UART0, 10: SDIO 0
const slcr_lvl_shftr_en = 0xF800_0900; // PS-PL level shifters

const uart = board.uart_base;
const uart_cr = uart + 0x00;
const uart_mr = uart + 0x04;
const uart_idr = uart + 0x0C;
const uart_baudgen = uart + 0x18;
const uart_sr = uart + 0x2C; // bit 1: RX empty, bit 4: TX full
const uart_fifo = uart + 0x30;
const uart_bauddiv = uart + 0x34;

const gpio = 0xE000_A000;
const gpio_data2 = gpio + 0x048; // bank 2 = EMIO 0..31 (PS -> PL)
const gpio_data3_ro = gpio + 0x06C; // bank 3 = EMIO 32..63 (PL -> PS)
const gpio_dirm2 = gpio + 0x284;
const gpio_oen2 = gpio + 0x288;
const gpio_dirm3 = gpio + 0x2C4;

const gtimer_lo = 0xF8F0_0200; // global timer, CPU_3x2x clock
const gtimer_hi = 0xF8F0_0204;
const gtimer_ctrl = 0xF8F0_0208;
/// The board's PL part, for checking a bitstream's header (pcap.zig).
pub const pl_part = board.pl_part;

/// CPU_3x2x, half the CPU clock (ZedBoard 666.67 MHz, Arty 650 MHz).
pub const gtimer_hz: u64 = board.gtimer_hz;

/// Clocks (GPIO, the UART, SD 0; ps7_init turns them on already), level
/// shifters (only with a configured PL, `pl`; booted from the SD card the
/// PL starts empty), global timer, UART, EMIO GPIO bank 2 as outputs (all
/// low), bank 3 as inputs.
pub fn init(pl: bool) void {
    reg(slcr_unlock).* = 0xDF0D;
    reg(slcr_aper_clk_ctrl).* |= (1 << 22) | (@as(u32, 1) << board.uart_aper_clk_bit) | (1 << 10);
    if (pl) reg(slcr_lvl_shftr_en).* = 0xF;
    reg(gtimer_ctrl).* = 1;

    uartInit();
    usbPhyReset();
    reg(gpio_dirm2).* = 0xFFFF_FFFF;
    reg(gpio_oen2).* = 0xFFFF_FFFF;
    reg(gpio_dirm3).* = 0;
    gpioWrite(0);
}

/// The USB PHY's reset on a PS pin (the Arty's USB3320 on MIO 46, active
/// low; side track G phase 2): an output, low for 1 ms, then high, so the
/// PHY starts from its reset state and its ULPI clock runs before the USB
/// driver starts. ps7_init leaves the pin a GPIO (MIO_PIN_46 =
/// 0x00001200: L3_SEL GPIO, 3.3 V). Boards whose reset is a PL pin
/// (the ZedBoard's) have no pin here, and nothing is compiled.
fn usbPhyReset() void {
    const pin = board.usb_phy_reset_mio orelse return;
    // MIO 32..53 are GPIO bank 1 (UG585 14.2.1): DATA_1, DIRM_1, OEN_1
    comptime std.debug.assert(pin >= 32 and pin < 54);
    const bit: u5 = @intCast(pin - 32);
    const gpio_data1 = gpio + 0x044;
    const gpio_dirm1 = gpio + 0x244;
    const gpio_oen1 = gpio + 0x248;
    reg(gpio_data1).* &= ~(@as(u32, 1) << bit);
    reg(gpio_dirm1).* |= @as(u32, 1) << bit;
    reg(gpio_oen1).* |= @as(u32, 1) << bit;
    delayTicks(gtimer_hz / 1000);
    reg(gpio_data1).* |= @as(u32, 1) << bit;
    delayTicks(gtimer_hz / 1000);
}

// --- Global timer ------------------------------------------------------------
pub fn now() u64 {
    while (true) {
        const hi = reg(gtimer_hi).*;
        const lo = reg(gtimer_lo).*;
        if (reg(gtimer_hi).* == hi) return @as(u64, hi) << 32 | lo;
    }
}

pub fn delayTicks(t: u64) void {
    const start = now();
    while (now() - start < t) {}
}

pub fn ms(n: u64) u64 {
    return gtimer_hz / 1000 * n;
}

// --- UART (ZedBoard UART1, Arty UART0) -------------------------------------
/// UART bit rate, by protocol.rates index: 115200 (power-on, the fallback),
/// 921600, 460800, 230400, 2000000. With the 50 MHz reference: 50e6 /
/// (62 * 7) = 115207; codes 1 to 3 are 50e6 / (CD * 6) with CD = 9, 18,
/// 36: 925926, 462963, 231481 (+0.47 %, inside the usual 2 % tolerance);
/// code 4 (side track G phase 2) is 50e6 / (5 * 5) = 2000000 exactly.
pub fn uartBaud(code: u3) void {
    // Let the last reply leave completely before the rate changes: TX FIFO
    // empty (SR bit 3, TEMPTY) only means the last character has started;
    // the transmitter is idle once TACTIVE (bit 11) clears. Then a margin
    // of two characters at 115200 (174 us), in case the desktop end is
    // still reading. Without this the "b1" reply arrived garbled.
    while (reg(uart_sr).* & 0x08 == 0) {}
    while (reg(uart_sr).* & 0x800 != 0) {}
    delayTicks(gtimer_hz / 5000);
    reg(uart_cr).* = 0x28; // TX and RX disabled while the divisors change
    reg(uart_baudgen).* = switch (code) {
        0 => 62,
        1 => 9,
        2 => 18,
        3 => 36,
        else => 5,
    };
    reg(uart_bauddiv).* = switch (code) {
        0 => 6,
        1, 2, 3 => 5,
        else => 4,
    };
    reg(uart_cr).* = 0x03; // reset both paths (and their FIFOs), as in uartInit
    while (reg(uart_cr).* & 0x03 != 0) {}
    reg(uart_cr).* = 0x114; // RX and TX enabled, stop break
}

fn uartInit() void {
    reg(uart_idr).* = 0x1FFF; // no interrupts
    reg(uart_cr).* = 0x28; // TX and RX disabled
    reg(uart_cr).* = 0x03; // reset both paths
    while (reg(uart_cr).* & 0x03 != 0) {}
    reg(uart_mr).* = 0x20; // 8 data bits, no parity, 1 stop bit, ref clock
    // 50 MHz UART reference clock (ps7_init): 50e6 / (62 * (6 + 1)) = 115207 baud
    reg(uart_baudgen).* = 62;
    reg(uart_bauddiv).* = 6;
    reg(uart_cr).* = 0x114; // RX and TX enabled, stop break
}

pub fn putc(c: u8) void {
    while (reg(uart_sr).* & 0x10 != 0) {}
    reg(uart_fifo).* = c;
}

pub fn puts(s: []const u8) void {
    for (s) |c| putc(c);
}

pub fn getc() ?u8 {
    if (reg(uart_sr).* & 0x02 != 0) return null;
    return @truncate(reg(uart_fifo).*);
}

/// Next received byte, or null after `timeout` timer ticks without one.
pub fn getcTimeout(timeout: u64) ?u8 {
    const start = now();
    while (now() - start < timeout) {
        if (getc()) |c| return c;
    }
    return null;
}

pub fn hex4(v: u16) [4]u8 {
    const d = "0123456789ABCDEF";
    return .{ d[v >> 12], d[(v >> 8) & 0xF], d[(v >> 4) & 0xF], d[v & 0xF] };
}

pub fn hex8(v: u32) [8]u8 {
    const hi = hex4(@truncate(v >> 16));
    const lo = hex4(@truncate(v));
    return hi ++ lo;
}

pub fn putDec(v: u32) void {
    var buf: [10]u8 = undefined;
    var i: usize = buf.len;
    var x = v;
    while (true) {
        i -= 1;
        buf[i] = '0' + @as(u8, @intCast(x % 10));
        x /= 10;
        if (x == 0) break;
    }
    puts(buf[i..]);
}

// --- EMIO GPIO -----------------------------------------------------------------
pub fn gpioWrite(v: u32) void {
    reg(gpio_data2).* = v;
}

pub fn gpioRead() u32 {
    return reg(gpio_data3_ro).*;
}

/// The EMIO GPIO banks as the PL link (pl.zig) uses them.
pub const GpioPort = struct {
    pub fn write(_: *GpioPort, v: u32) void {
        gpioWrite(v);
    }
    pub fn read(_: *GpioPort) u32 {
        return gpioRead();
    }
    pub fn delayUs(_: *GpioPort, us: u32) void {
        delayTicks(gtimer_hz / 1_000_000 * us);
    }
};
