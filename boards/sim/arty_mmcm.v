// Simulation stand-in for boards/arty-z7-20/arty_mmcm.v (the Arty's
// MMCM). Used only by Verilator (make sim / make lint BOARD=arty-z7-20),
// never by Vivado; found through -y boards/sim.
//
// Hand-written (CLAUDE.md rule 4) because MMCME2_BASE only exists inside
// Vivado. The harness (platform/sim/arty.zig) drives clk_in at the
// serialiser's rate, one tick per 5x clock: clk_5x is clk_in, and clk_pix
// is clk_in divided by 5 (high for 2 of the 5 periods, rising with a
// clk_in rising edge). `locked` rises at the first clk_pix edge after
// `reset` is released, like the MMCM's (much later) lock.

`timescale 100fs/100fs  // as Clash's output, so Verilator sees one timescale
`default_nettype none
module arty_mmcm
    ( input  wire clk_in
    , input  wire reset
    , output wire clk_pix
    , output wire clk_5x
    , output reg  locked
    );
  reg [2:0] cnt;
  reg       pix;
  initial begin
    cnt = 3'd0;
    pix = 1'b0;
    locked = 1'b0;
  end
  assign clk_5x  = clk_in;
  assign clk_pix = pix;
  always @(posedge clk_in) begin
    cnt <= (cnt == 3'd4) ? 3'd0 : cnt + 3'd1;
    pix <= (cnt == 3'd4) || (cnt == 3'd0);
  end
  always @(posedge pix or posedge reset)
    if (reset) locked <= 1'b0;
    else       locked <= 1'b1;
endmodule
