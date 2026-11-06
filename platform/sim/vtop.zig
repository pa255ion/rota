//! The C ABI of the Verilator model's shim (vtop.cpp).

const vga = @import("vga.zig");
const axi = @import("axi.zig");

pub const Vtop = opaque {};
pub extern fn vtop_new() *Vtop;
pub extern fn vtop_free(v: *Vtop) void;
/// btn: bit 0 = BTNC, 1 = BTND, 2 = BTNL, 3 = BTNR, 4 = BTNU; sw: bit i = SWi.
pub extern fn vtop_set_inputs(v: *Vtop, btn: u8, sw: u8) void;
/// One GCLK period; `out` gets the outputs after the rising edge.
pub extern fn vtop_tick(v: *Vtop, out: *vga.Pins) void;

/// struct vtop_hdmi in vtop.cpp.
pub const Hdmi = extern struct {
    d: u16,
    de: u8,
    hs: u8,
    vs: u8,
    clk_after_fall: u8,
    clk_after_rise: u8,
    scl_low: u8,
    sda_low: u8,
    /// HD_SPDIF (tops with HDMI audio): the line in the second half of
    /// the previous clock, then in the first half of this one.
    spdif_after_fall: u8 = 0,
    spdif_after_rise: u8 = 0,
};
/// 1 if the model has the PS link ports PS_GPIO/PS_IN (built with -DVTOP_PS).
pub extern fn vtop_has_ps() c_int;
/// PS_GPIO from the next clock on (no-op without the PS link).
pub extern fn vtop_set_ps(v: *Vtop, gpio: u32) void;
/// PS_IN after the last clock (0 without the PS link).
pub extern fn vtop_ps_in(v: *Vtop) u32;
/// 1 if the model has HD_SPDIF (built with -DVTOP_SPDIF).
pub extern fn vtop_has_spdif() c_int;
/// 1 if the model has the HDMI ports (built with -DVTOP_HDMI).
pub extern fn vtop_has_hdmi() c_int;
/// As vtop_tick, with the I2C wire levels applied first; `hdmi` gets the
/// HDMI outputs (all zero for tops without HDMI).
pub extern fn vtop_tick_hdmi(v: *Vtop, scl: u8, sda: u8, out: *vga.Pins, hdmi: *Hdmi) void;

/// 1 if the model has the HP0 read channels (built with -DVTOP_AXI).
pub extern fn vtop_has_axi() c_int;
/// 1 if the top has PS_RESETN (the PS's PL reset; -DVTOP_PSRST)
pub extern fn vtop_has_ps_reset() c_int;
/// PS_RESETN's level (1 = running)
pub extern fn vtop_set_ps_resetn(v: *Vtop, level: u8) void;
/// The AXI slave's outputs, from the next clock on (no-op without them).
pub extern fn vtop_set_axi(v: *Vtop, s: *const axi.SlaveOut) void;
/// The AXI master's outputs after the last clock.
pub extern fn vtop_axi_out(v: *Vtop, m: *axi.MasterOut) void;
