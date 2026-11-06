//! What the PS app needs to know about the ZedBoard (side track G phase
//! 2): the board module of `zig build ps-app -Dboard=zedboard`, the
//! default. Its Arty counterpart is board_arty_z7_20.zig; hw.zig and the
//! USB setup read these instead of hard-coding one board.

pub const name = "zedboard";

/// The PL's part as Vivado writes it into a .bit header's field b: the
/// relay loads only bitstreams for it from the card (pcap.zig).
pub const pl_part = "7z020clg484";

/// The desktop's USB-UART (J14, a Cypress bridge): PS UART1 on MIO 48/49
/// (UG585 appendix B: UART1 at 0xE0001000; APER_CLK_CTRL bit 21).
pub const uart_base: usize = 0xE000_1000;
pub const uart_aper_clk_bit: u5 = 21;

/// The global timer runs at CPU_3x2x, half the CPU clock: 666.67 MHz on
/// the ZedBoard preset (Vivado's zed sample ps7_init).
pub const gtimer_hz: u64 = 333_333_333;

/// The USB PHY's (TUSB1210's) reset is a PL pin here (G17, OTG_RESETN),
/// which every PS wrapper ties high (boards/zedboard/usb.xdc): nothing
/// for the PS to drive.
pub const usb_phy_reset_mio: ?u6 = null;
