// TMDS output stage for the Arty Z7-20's HDMI TX connector (side track G
// step 4): four 10:1 serialisers (OSERDESE2 master/slave pairs, DDR at 5x
// the pixel clock) and TMDS_33 differential buffers. Channels 0..2 carry
// the 10-bit symbols from Platform.Video.Tmds; channel 3 is the TMDS clock
// (the constant symbol 0000011111: five ones, then five zeros, one period
// per pixel).
//
// Hand-written (CLAUDE.md rule 4): OSERDESE2 cascading and OBUFDS are
// device primitives that Clash has no library functions for. Bit order:
// OSERDESE2 sends D1 first and TMDS sends bit 0 first (DVI 1.0, 3.2.2), so
// the master's D1..D8 take bits 0..7 and the slave's D3/D4 bits 8/9
// (UG471, "Width Expansion").
//
// Instantiated by Platform.Board.ArtyZ7.tmdsOut (a Clash blackbox), read
// by build.tcl through BOARD_WRAPPER (systems/*/arty-z7-20.mk). Verilator
// uses boards/sim/tmds_out.v instead.

`default_nettype none
module tmds_out
    ( input  wire       clk_pix
    , input  wire       clk_5x    // from the same MMCM, 5x clk_pix
    , input  wire       rst       // active high, synchronous to clk_pix
    , input  wire [9:0] d0        // blue + syncs
    , input  wire [9:0] d1        // green
    , input  wire [9:0] d2        // red
    , output wire [3:0] p         // 0..2: data channels, 3: clock
    , output wire [3:0] n
    );
  wire [9:0] sym [0:3];
  assign sym[0] = d0;
  assign sym[1] = d1;
  assign sym[2] = d2;
  assign sym[3] = 10'b0000011111;

  genvar i;
  generate
    for (i = 0; i < 4; i = i + 1) begin : ch
      wire q, s1, s2;
      OSERDESE2 #(
        .DATA_RATE_OQ   ("DDR"),
        .DATA_RATE_TQ   ("SDR"),
        .DATA_WIDTH     (10),
        .SERDES_MODE    ("MASTER"),
        .TRISTATE_WIDTH (1),
        .TBYTE_CTL      ("FALSE"),
        .TBYTE_SRC      ("FALSE")
      ) master (
        .OQ (q), .OFB (), .TQ (), .TFB (), .SHIFTOUT1 (), .SHIFTOUT2 (), .TBYTEOUT (),
        .CLK (clk_5x), .CLKDIV (clk_pix),
        .D1 (sym[i][0]), .D2 (sym[i][1]), .D3 (sym[i][2]), .D4 (sym[i][3]),
        .D5 (sym[i][4]), .D6 (sym[i][5]), .D7 (sym[i][6]), .D8 (sym[i][7]),
        .OCE (1'b1), .RST (rst),
        .SHIFTIN1 (s1), .SHIFTIN2 (s2),
        .T1 (1'b0), .T2 (1'b0), .T3 (1'b0), .T4 (1'b0), .TBYTEIN (1'b0), .TCE (1'b0)
      );
      OSERDESE2 #(
        .DATA_RATE_OQ   ("DDR"),
        .DATA_RATE_TQ   ("SDR"),
        .DATA_WIDTH     (10),
        .SERDES_MODE    ("SLAVE"),
        .TRISTATE_WIDTH (1),
        .TBYTE_CTL      ("FALSE"),
        .TBYTE_SRC      ("FALSE")
      ) slave (
        .OQ (), .OFB (), .TQ (), .TFB (), .SHIFTOUT1 (s1), .SHIFTOUT2 (s2), .TBYTEOUT (),
        .CLK (clk_5x), .CLKDIV (clk_pix),
        .D1 (1'b0), .D2 (1'b0), .D3 (sym[i][8]), .D4 (sym[i][9]),
        .D5 (1'b0), .D6 (1'b0), .D7 (1'b0), .D8 (1'b0),
        .OCE (1'b1), .RST (rst),
        .SHIFTIN1 (1'b0), .SHIFTIN2 (1'b0),
        .T1 (1'b0), .T2 (1'b0), .T3 (1'b0), .T4 (1'b0), .TBYTEIN (1'b0), .TCE (1'b0)
      );
      OBUFDS #(.IOSTANDARD ("TMDS_33")) obuf (.I (q), .O (p[i]), .OB (n[i]));
    end
  endgenerate
endmodule
