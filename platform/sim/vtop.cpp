// C ABI around the Verilator model of a board top (docs/TESTING.md, layer 4).
//
// This is the only C++ in the harness (CLAUDE.md rule 9): Verilator only
// generates C++, so the model is wrapped here and everything else lives in
// Zig (main.zig). Verilator runs with `--prefix Vtop`, so the class is
// always Vtop whatever the system. Every top with VGA has the same ports
// (GCLK, BTNC/D/L/R/U, SW, LD, VGA_R/G/B, VGA_HS, VGA_VS; the board XDC
// names), so one shim serves them all.
//
// Tops with HDMI (M0d onwards: HD_CLK, HD_D, HD_DE, HD_HSYNC, HD_VSYNC and
// the open-drain HD_SCL/HD_SDA) are built with -DVTOP_HDMI (the Makefile
// checks the Clash output for HD_D). Verilator runs with
// --pins-inout-enables, so an inout pin P is an input P (the level on the
// wire) and outputs P__out / P__en (what the design drives). Without
// VTOP_HDMI, vtop_tick_hdmi only ticks and reports no HDMI. Tops with
// HDMI audio (HD_SPDIF, from an ODDR) are also built with -DVTOP_SPDIF.
//
// Tops with the PS link (PS_GPIO in, PS_IN out; Platform.Ps.Link) are built
// with -DVTOP_PS, so the harness can play the PS (platform/sim/ps.zig).
// Without it, vtop_set_ps does nothing and vtop_ps_in reads 0.

#include <cstdint>

#include "Vtop.h"
#include "verilated.h"

struct vtop {
    VerilatedContext ctx;
    Vtop *top;
};

struct vtop_out {
    uint8_t ld, r, g, b, hs, vs;
};

// HDMI outputs after the rising edge, HD_CLK after both edges, and what
// the design drives on the I2C lines (1 = pulls the line low).
struct vtop_hdmi {
    uint16_t d;
    uint8_t de, hs, vs;
    uint8_t clk_after_fall, clk_after_rise;
    uint8_t scl_low, sda_low;
    uint8_t spdif_after_fall, spdif_after_rise;
};

extern "C" {

vtop *vtop_new(void) {
    vtop *v = new vtop;
    v->top = new Vtop{&v->ctx};
    v->top->GCLK = 0;
    v->top->eval();
    return v;
}

void vtop_free(vtop *v) {
    v->top->final();
    delete v->top;
    delete v;
}

// btn: bit 0 = BTNC, 1 = BTND, 2 = BTNL, 3 = BTNR, 4 = BTNU. sw: bit i = SWi.
void vtop_set_inputs(vtop *v, uint8_t btn, uint8_t sw) {
    v->top->BTNC = (btn >> 0) & 1;
    v->top->BTND = (btn >> 1) & 1;
    v->top->BTNL = (btn >> 2) & 1;
    v->top->BTNR = (btn >> 3) & 1;
    v->top->BTNU = (btn >> 4) & 1;
    v->top->SW = sw;
}

// One full GCLK period (falling, then rising edge); *out gets the outputs
// as they are after the rising edge.
void vtop_tick(vtop *v, vtop_out *out) {
    v->top->GCLK = 0;
    v->top->eval();
    v->top->GCLK = 1;
    v->top->eval();
    out->ld = v->top->LD;
    out->r = v->top->VGA_R;
    out->g = v->top->VGA_G;
    out->b = v->top->VGA_B;
    out->hs = v->top->VGA_HS;
    out->vs = v->top->VGA_VS;
}

int vtop_has_ps(void) {
#ifdef VTOP_PS
    return 1;
#else
    return 0;
#endif
}

// PS_GPIO from the next clock on.
void vtop_set_ps(vtop *v, uint32_t gpio) {
#ifdef VTOP_PS
    v->top->PS_GPIO = gpio;
#else
    (void)v;
    (void)gpio;
#endif
}

uint32_t vtop_ps_in(vtop *v) {
#ifdef VTOP_PS
    return v->top->PS_IN;
#else
    (void)v;
    return 0;
#endif
}

// Tops with the HP0 read channels (M0e.Top, side track C) are built with
// -DVTOP_AXI: the harness plays the HP port (platform/sim/axi.zig).
// struct axi_master / axi_slave are axi.zig's MasterOut / SlaveOut.
struct axi_master {
    uint8_t arvalid, arlen, rready;
    uint32_t araddr;
};
struct axi_slave {
    uint8_t arready, rvalid, rresp, rlast;
    uint64_t rdata;
};

int vtop_has_axi(void) {
#ifdef VTOP_AXI
    return 1;
#else
    return 0;
#endif
}

// The PS's PL reset (PS_RESETN, FCLK_RESET0_N on the board; ZedBoard tops
// beside the PS, -DVTOP_PSRST; side track K for side track I's reset-race
// fix, as vtop_arty.cpp's): high (running) unless the harness holds it.
int vtop_has_ps_reset(void) {
#ifdef VTOP_PSRST
    return 1;
#else
    return 0;
#endif
}

void vtop_set_ps_resetn(vtop *v, uint8_t level) {
#ifdef VTOP_PSRST
    v->top->PS_RESETN = level & 1;
#else
    (void)v;
    (void)level;
#endif
}

// The slave's outputs, from the next clock on.
void vtop_set_axi(vtop *v, const axi_slave *s) {
#ifdef VTOP_AXI
    v->top->HP0_ARREADY = s->arready;
    v->top->HP0_RVALID = s->rvalid;
    v->top->HP0_RDATA = s->rdata;
    v->top->HP0_RRESP = s->rresp;
    v->top->HP0_RLAST = s->rlast;
#else
    (void)v;
    (void)s;
#endif
}

// The master's outputs after the last clock.
void vtop_axi_out(vtop *v, axi_master *m) {
#ifdef VTOP_AXI
    m->arvalid = v->top->HP0_ARVALID;
    m->araddr = v->top->HP0_ARADDR;
    m->arlen = v->top->HP0_ARLEN;
    m->rready = v->top->HP0_RREADY;
#else
    (void)v;
    *m = axi_master{};
#endif
}

int vtop_has_hdmi(void) {
#ifdef VTOP_HDMI
    return 1;
#else
    return 0;
#endif
}

int vtop_has_spdif(void) {
#if defined(VTOP_HDMI) && defined(VTOP_SPDIF)
    return 1;
#else
    return 0;
#endif
}

// As vtop_tick, with the I2C wire levels (scl, sda: 1 = high) applied
// before the clock; *hdmi gets the HDMI side (all zero without VTOP_HDMI).
void vtop_tick_hdmi(vtop *v, uint8_t scl, uint8_t sda, vtop_out *out, vtop_hdmi *hdmi) {
#ifdef VTOP_HDMI
    v->top->HD_SCL = scl;
    v->top->HD_SDA = sda;
    v->top->GCLK = 0;
    v->top->eval();
    hdmi->clk_after_fall = v->top->HD_CLK;
#ifdef VTOP_SPDIF
    hdmi->spdif_after_fall = v->top->HD_SPDIF;
#endif
    v->top->GCLK = 1;
    v->top->eval();
    hdmi->clk_after_rise = v->top->HD_CLK;
#ifdef VTOP_SPDIF
    hdmi->spdif_after_rise = v->top->HD_SPDIF;
#else
    hdmi->spdif_after_fall = 0;
    hdmi->spdif_after_rise = 0;
#endif
    hdmi->d = v->top->HD_D;
    hdmi->de = v->top->HD_DE;
    hdmi->hs = v->top->HD_HSYNC;
    hdmi->vs = v->top->HD_VSYNC;
    hdmi->scl_low = v->top->HD_SCL__en && !v->top->HD_SCL__out;
    hdmi->sda_low = v->top->HD_SDA__en && !v->top->HD_SDA__out;
    out->ld = v->top->LD;
    out->r = v->top->VGA_R;
    out->g = v->top->VGA_G;
    out->b = v->top->VGA_B;
    out->hs = v->top->VGA_HS;
    out->vs = v->top->VGA_VS;
#else
    (void)scl;
    (void)sda;
    *hdmi = vtop_hdmi{};
    vtop_tick(v, out);
#endif
}

}
