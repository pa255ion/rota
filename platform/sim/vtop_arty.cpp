// C ABI around the Verilator model of an Arty Z7-20 top (side track G,
// docs/TESTING.md, layer 4); the Arty's counterpart of vtop.cpp, whose
// ports are the ZedBoard's. Every Arty top has the board XDC's names:
// clk, btn[3:0], sw[1:0], led[3:0]; tops with HDMI (built with
// -DARTY_TMDS, the Makefile checks the Clash output for hdmi_tx_clk_p)
// also hdmi_tx_clk_p/n and hdmi_tx_d_p/n[2:0]; tops with the RGB LED's
// green (-DARTY_LED4G) led4_g; tops with the PS link (-DARTY_PS, side
// track G phase 2) PS_GPIO and PS_IN, as vtop.cpp's -DVTOP_PS; tops with
// the HP0 read channels (-DARTY_AXI: the Game Boy's cartridge in DDR) the
// same AXI side as vtop.cpp's -DVTOP_AXI, played by platform/sim/axi.zig.
//
// One tick is one period of clk, which the MMCM's stand-in
// (boards/sim/arty_mmcm.v) treats as the 5x clock: the pixel clock is
// clk / 5, and the serialisers' stand-in (boards/sim/tmds_out.v) sends
// one bit after each edge. Verilator runs with `--prefix Vtop`.

#include <cstdint>

#include "Vtop.h"
#include "verilated.h"

struct vtop {
    VerilatedContext ctx;
    Vtop *top;
};

// The HDMI lines after the falling, then after the rising edge (bit i =
// lane i, lane 3 = the clock pair), and the LEDs after the rising edge.
struct arty_out {
    uint8_t p_after_fall, n_after_fall;
    uint8_t p_after_rise, n_after_rise;
    uint8_t led, led4_g;
    uint8_t aud_pwm;  // after the rising edge (-DARTY_AUDIO; phase 2 step 6)
};

extern "C" {

vtop *arty_new(void) {
    vtop *v = new vtop;
    v->top = new Vtop{&v->ctx};
    v->top->clk = 0;
    v->top->eval();
    return v;
}

void arty_free(vtop *v) {
    v->top->final();
    delete v->top;
    delete v;
}

int arty_has_audio(void) {
#ifdef ARTY_AUDIO
    return 1;
#else
    return 0;
#endif
}

int arty_has_tmds(void) {
#ifdef ARTY_TMDS
    return 1;
#else
    return 0;
#endif
}

int arty_has_ps(void) {
#ifdef ARTY_PS
    return 1;
#else
    return 0;
#endif
}

// PS_GPIO from the next clock on (nothing without the PS link).
void arty_set_ps(vtop *v, uint32_t gpio) {
#ifdef ARTY_PS
    v->top->PS_GPIO = gpio;
#else
    (void)v;
    (void)gpio;
#endif
}

uint32_t arty_ps_in(vtop *v) {
#ifdef ARTY_PS
    return v->top->PS_IN;
#else
    (void)v;
    return 0;
#endif
}

// struct axi_master / axi_slave are axi.zig's MasterOut / SlaveOut.
struct axi_master {
    uint8_t arvalid, arlen, rready;
    uint32_t araddr;
};
struct axi_slave {
    uint8_t arready, rvalid, rresp, rlast;
    uint64_t rdata;
};

int arty_has_axi(void) {
#ifdef ARTY_AXI
    return 1;
#else
    return 0;
#endif
}

// The slave's outputs, from the next HP0 clock (pixel clock) on.
void arty_set_axi(vtop *v, const axi_slave *s) {
#ifdef ARTY_AXI
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

// The master's outputs now.
void arty_axi_out(vtop *v, axi_master *m) {
#ifdef ARTY_AXI
    m->arvalid = v->top->HP0_ARVALID;
    m->araddr = v->top->HP0_ARADDR;
    m->arlen = v->top->HP0_ARLEN;
    m->rready = v->top->HP0_RREADY;
#else
    (void)v;
    *m = axi_master{};
#endif
}

// HP0's write channels (side track O, O.4: screenshots): axi.zig's
// WrMasterOut / WrSlaveOut.
struct axi_wr_master {
    uint8_t awvalid, awlen, wvalid, wlast, wstrb, bready;
    uint32_t awaddr;
    uint64_t wdata;
};
struct axi_wr_slave {
    uint8_t awready, wready, bvalid, bresp;
};

int arty_has_axi_w(void) {
#ifdef ARTY_AXI_W
    return 1;
#else
    return 0;
#endif
}

void arty_set_axi_w(vtop *v, const axi_wr_slave *s) {
#ifdef ARTY_AXI_W
    v->top->HP0_AWREADY = s->awready;
    v->top->HP0_WREADY = s->wready;
    v->top->HP0_BVALID = s->bvalid;
    v->top->HP0_BRESP = s->bresp;
#else
    (void)v;
    (void)s;
#endif
}

void arty_axi_w_out(vtop *v, axi_wr_master *m) {
#ifdef ARTY_AXI_W
    m->awvalid = v->top->HP0_AWVALID;
    m->awaddr = v->top->HP0_AWADDR;
    m->awlen = v->top->HP0_AWLEN;
    m->wvalid = v->top->HP0_WVALID;
    m->wdata = v->top->HP0_WDATA;
    m->wstrb = v->top->HP0_WSTRB;
    m->wlast = v->top->HP0_WLAST;
    m->bready = v->top->HP0_BREADY;
#else
    (void)v;
    *m = axi_wr_master{};
#endif
}

// The PS's PL reset (PS_RESETN, FCLK_RESET0_N on the board; tops beside
// the PS, -DARTY_PSRST): high (running) unless the harness holds it.
void arty_set_ps_resetn(vtop *v, uint8_t level) {
#ifdef ARTY_PSRST
    v->top->PS_RESETN = level & 1;
#else
    (void)v;
    (void)level;
#endif
}

int arty_has_ps_reset(void) {
#ifdef ARTY_PSRST
    return 1;
#else
    return 0;
#endif
}

// btn: bit i = BTNi; sw: bit i = SWi.
void arty_set_inputs(vtop *v, uint8_t btn, uint8_t sw) {
    v->top->btn = btn & 0xF;
    v->top->sw = sw & 0x3;
}

#ifdef ARTY_TMDS
static void tmds_lines(vtop *v, uint8_t *p, uint8_t *n) {
    *p = (v->top->hdmi_tx_d_p & 7) | (v->top->hdmi_tx_clk_p << 3);
    *n = (v->top->hdmi_tx_d_n & 7) | (v->top->hdmi_tx_clk_n << 3);
}
#endif

// One clk period (falling, then rising edge).
void arty_tick(vtop *v, arty_out *out) {
    v->top->clk = 0;
    v->top->eval();
#ifdef ARTY_TMDS
    tmds_lines(v, &out->p_after_fall, &out->n_after_fall);
#endif
    v->top->clk = 1;
    v->top->eval();
#ifdef ARTY_TMDS
    tmds_lines(v, &out->p_after_rise, &out->n_after_rise);
#else
    out->p_after_fall = out->n_after_fall = out->p_after_rise = out->n_after_rise = 0;
#endif
    out->led = v->top->led;
#ifdef ARTY_LED4G
    out->led4_g = v->top->led4_g;
#else
    out->led4_g = 0;
#endif
#ifdef ARTY_AUDIO
    out->aud_pwm = v->top->aud_pwm;
#else
    out->aud_pwm = 0;
#endif
}

}  // extern "C"
