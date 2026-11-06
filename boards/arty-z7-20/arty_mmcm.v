// Arty Z7-20 clock plan (side track G step 3): 125 MHz in, the pixel
// clock and 5x the pixel clock (for the TMDS serialisers) out.
//
// Hand-written (CLAUDE.md rule 4) because the two outputs must be exactly
// 1:5 and phase-aligned for OSERDESE2's CLKDIV/CLK, and Clash's
// clockWizard can't promise that: it asks the Clocking Wizard for each
// output's frequency separately, and for DomPix's 25.17623 MHz and 5x
// that it picks M = 24.25, D = 3, O0 = 40.125, O1 = 8 (25.182 and
// 126.302 MHz, not 1:5; tried 2026-10-04). These values are the ones the
// wizard itself picks when asked for 25.17361 and 125.86806 MHz:
//
//   VCO  = 125 MHz * 18.125 / 2 = 1132.8125 MHz  (-1 part: 600..1200 MHz)
//   PFD  = 125 MHz / 2          = 62.5 MHz       (10..450 MHz)
//   pix  = VCO / 45             = 25.17361 MHz   (39.724 ns)
//   5x   = VCO / 9              = 125.86806 MHz  (7.945 ns)
//
// That is 55 ppm below VGA's 25.175 MHz and 104 ppm below the ZedBoard's
// 25.17623 MHz (Platform.Domains: DomPix's period stays 39720 ps; the
// difference is recorded in docs/ARTYZ7.md). Vivado derives both output
// clocks from these parameters for timing.
//
// Instantiated by Platform.Board.ArtyZ7.artyMmcm (a Clash blackbox) and
// read by build.tcl through BOARD_WRAPPER (systems/*/arty-z7-20.mk).
// Verilator uses boards/sim/arty_mmcm.v instead.

`default_nettype none
module arty_mmcm
    ( input  wire clk_in      // 125 MHz, pin H16
    , input  wire reset       // active high, asynchronous
    , output wire clk_pix
    , output wire clk_5x
    , output wire locked      // asynchronous; synchronise before use
    );
  wire fb, pix_unbuf, x5_unbuf;

  MMCME2_BASE #(
    .BANDWIDTH        ("OPTIMIZED"),
    .CLKIN1_PERIOD    (8.000),
    .DIVCLK_DIVIDE    (2),
    .CLKFBOUT_MULT_F  (18.125),
    .CLKFBOUT_PHASE   (0.0),
    .CLKOUT0_DIVIDE_F (45.000),
    .CLKOUT0_PHASE    (0.0),
    .CLKOUT0_DUTY_CYCLE (0.5),
    .CLKOUT1_DIVIDE   (9),
    .CLKOUT1_PHASE    (0.0),
    .CLKOUT1_DUTY_CYCLE (0.5),
    .STARTUP_WAIT     ("FALSE")
  ) mmcm (
    .CLKIN1   (clk_in),
    .CLKFBIN  (fb),
    .CLKFBOUT (fb),
    .CLKFBOUTB(),
    .CLKOUT0  (pix_unbuf),
    .CLKOUT0B (),
    .CLKOUT1  (x5_unbuf),
    .CLKOUT1B (),
    .CLKOUT2  (),
    .CLKOUT2B (),
    .CLKOUT3  (),
    .CLKOUT3B (),
    .CLKOUT4  (),
    .CLKOUT5  (),
    .CLKOUT6  (),
    .LOCKED   (locked),
    .PWRDWN   (1'b0),
    .RST      (reset)
  );

  BUFG pix_bufg (.I(pix_unbuf), .O(clk_pix));
  BUFG x5_bufg  (.I(x5_unbuf),  .O(clk_5x));
endmodule
