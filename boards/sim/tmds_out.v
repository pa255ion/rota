// Simulation stand-in for boards/arty-z7-20/tmds_out.v (OSERDESE2 pairs
// and OBUFDS). Used only by Verilator, never by Vivado; found through
// -y boards/sim.
//
// Hand-written (CLAUDE.md rule 4): the device primitives only exist inside
// Vivado. Behaviour, as the real stage: each channel's 10-bit symbol is
// taken at clk_pix and sent bit 0 first, two bits per clk_5x period (DDR):
// the bit after a rising edge, then the bit after the falling edge.
// Channel 3 is the TMDS clock, 0000011111. n is the complement of p. The
// latency (symbols leave a few clk_5x periods after clk_pix takes them)
// differs from the real OSERDESE2's, which nothing depends on: the
// harness finds symbol boundaries from the clock channel.

`timescale 100fs/100fs  // as Clash's output, so Verilator sees one timescale
`default_nettype none
module tmds_out
    ( input  wire       clk_pix
    , input  wire       clk_5x
    , input  wire       rst
    , input  wire [9:0] d0
    , input  wire [9:0] d1
    , input  wire [9:0] d2
    , output wire [3:0] p
    , output wire [3:0] n
    );
  reg [9:0] h0, h1, h2;      // the CLKDIV-side register
  reg [9:0] s0, s1, s2, s3;  // the shift registers (CLK side)
  reg [2:0] ph;
  initial begin
    h0 = 10'd0; h1 = 10'd0; h2 = 10'd0;
    s0 = 10'd0; s1 = 10'd0; s2 = 10'd0; s3 = 10'd0;
    ph = 3'd0;
  end
  always @(posedge clk_pix) begin
    h0 <= d0;
    h1 <= d1;
    h2 <= d2;
  end
  // ph is 0 one clk_5x period after each clk_pix edge (the reset is
  // released at one), when h0..h2 are settled. In reset every line is
  // low, as OSERDESE2's OQ is.
  always @(posedge clk_5x) begin
    ph <= (rst || ph == 3'd4) ? 3'd0 : ph + 3'd1;
    if (rst) begin
      s0 <= 10'd0;
      s1 <= 10'd0;
      s2 <= 10'd0;
      s3 <= 10'd0;
    end else if (ph == 3'd0) begin
      s0 <= h0;
      s1 <= h1;
      s2 <= h2;
      s3 <= 10'b0000011111;
    end else begin
      s0 <= s0 >> 2;
      s1 <= s1 >> 2;
      s2 <= s2 >> 2;
      s3 <= s3 >> 2;
    end
  end
  assign p = clk_5x ? {s3[0], s2[0], s1[0], s0[0]} : {s3[1], s2[1], s1[1], s0[1]};
  assign n = ~p;
endmodule
