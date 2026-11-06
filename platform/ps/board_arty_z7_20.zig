//! What the PS app needs to know about the Arty Z7-20 (side track G
//! phase 2): the board module of `zig build ps-app -Dboard=arty-z7-20`.
//! Values from the Arty's PS7 configuration (boards/arty-z7-20/ps7.tcl:
//! Digilent's preset, docs/ARTYZ7.md), checked against the generated
//! ps7_init (make ps7-init BOARD=arty-z7-20, clocks.txt).

pub const name = "arty-z7-20";

/// The PL's part as Vivado writes it into a .bit header's field b (seen in
/// build/arty-z7-20/*/vivado/*.bit): the relay loads only bitstreams for
/// it from the card (pcap.zig), never a ZedBoard one (another package,
/// another pinout).
pub const pl_part = "7z020clg400";

/// The micro-USB port's FTDI chip (its second channel; the first is
/// JTAG): PS UART0 on MIO 14/15 (UG585 appendix B: UART0 at 0xE0000000;
/// APER_CLK_CTRL bit 20). Its reference clock is 50 MHz as on the
/// ZedBoard (ps7.tcl overrides the preset's 100), so hw.zig's divisors
/// hold for both.
pub const uart_base: usize = 0xE000_0000;
pub const uart_aper_clk_bit: u5 = 20;

/// CPU_3x2x, half the CPU clock: 650 MHz here (ARM PLL 50 MHz x 26 / 2;
/// 666.67 isn't reachable from the 50 MHz crystal with the preset's
/// dividers).
pub const gtimer_hz: u64 = 325_000_000;

/// The USB3320's reset (active low) is PS pin MIO 46 (the preset's
/// PCW_USB0_RESET_IO), which the app drives itself (side track G phase 2,
/// step 4).
pub const usb_phy_reset_mio: ?u6 = 46;
