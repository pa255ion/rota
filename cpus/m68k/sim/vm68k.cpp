// The C ABI around Verilator's model of m68k_top (cpus/m68k/hdl/M68kTop.hs;
// side track P, P.5), for the Zig library (cpus/m68k/sim/lib.zig). C++ only
// because Verilator's model is (CLAUDE.md, rule 9): a thin shim, the model's
// ports as plain arguments. Built by `zig c++` (Makefile, m68k-sim), so it
// shares libc++ with the Zig side.

#include "Vm68k.h"
#include "verilated.h"

#include <cstdint>

struct vm68k {
  VerilatedContext ctx;
  Vm68k *top;
};

// The outputs (m68k_top's): see M68kTop.hs.
struct vm68k_out {
  uint8_t kind, fc, is_byte, done, istart, stopped, halted, pad;
  uint32_t addr;
  uint32_t dout;
  uint32_t dbg;
};

static void inputs(vm68k *v, int dtack_n, int berr_n, int vpa_n, int ipl_n, uint16_t din, int dbg_sel) {
  v->top->dtack_n = dtack_n;
  v->top->berr_n = berr_n;
  v->top->vpa_n = vpa_n;
  v->top->ipl_n = ipl_n;
  v->top->din = din;
  v->top->dbg_sel = dbg_sel;
}

extern "C" {

// A new model, in reset (vm68k_tick's rst releases it).
vm68k *vm68k_new() {
  auto *v = new vm68k;
  v->top = new Vm68k{&v->ctx};
  v->top->clk = 0;
  v->top->en = 1;
  v->top->rst = 1;
  inputs(v, 1, 1, 1, 7, 0, 0);
  v->top->eval();
  return v;
}

void vm68k_free(vm68k *v) {
  v->top->final();
  delete v->top;
  delete v;
}

// The outputs with these lines, before the clock (done is combinational:
// it depends on DTACK, BERR and VPA).
void vm68k_peek(vm68k *v, int dtack_n, int berr_n, int vpa_n, int ipl_n, int dbg_sel, vm68k_out *o) {
  inputs(v, dtack_n, berr_n, vpa_n, ipl_n, v->top->din, dbg_sel);
  v->top->clk = 0;
  v->top->eval();
  o->kind = v->top->kind;
  o->fc = v->top->fc;
  o->is_byte = v->top->is_byte;
  o->done = v->top->done;
  o->istart = v->top->istart;
  o->stopped = v->top->stopped;
  o->halted = v->top->halted;
  o->addr = v->top->addr;
  o->dout = v->top->dout;
  o->dbg = v->top->dbg;
}

// One clock (a rising edge) with these inputs. The low half of the clock
// is vm68k_peek's evaluation: the inputs set here only feed the registers,
// so the edge alone evaluates them (a third of the time, the model being
// the core's whole step function).
void vm68k_tick(vm68k *v, int rst, int dtack_n, int berr_n, int vpa_n, int ipl_n, uint16_t din) {
  v->top->rst = rst;
  inputs(v, dtack_n, berr_n, vpa_n, ipl_n, din, v->top->dbg_sel);
  v->top->clk = 1;
  v->top->eval();
}

}
