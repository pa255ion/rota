// CHIP-8 on the Arty Z7-20 with its PS (side track G phase 2, step 3):
// the Arty's Zynq PS7 (IP "zynq_ps", boards/arty-z7-20/ps7.tcl:
// Digilent's preset plus the ZedBoard's EMIO GPIO) and chip8_arty_top
// (Chip8.ArtyTop). The Arty's counterpart of boards/zedboard/chip8_zynq.v.
//
// Hand-written (CLAUDE.md rule 4 allows Xilinx primitive wrappers in
// boards/): the PS7 has to reach the top level with its fixed DDR and MIO
// pins as inout ports, which is plain wiring here but awkward to express
// from Clash. No logic lives in this file. The PS link
// (Platform.Ps.Link): EMIO GPIO 0 to 31 from the PS app
// (platform/ps/relay.zig, built for this board), 32 to 63 to it
// (chip8_arty_top's PS_IN). Board pin names as in arty-z7-20.xdc and
// hdmi_tx.xdc; the PS pins need no constraints of ours.
//
// Differences from the ZedBoard's: the PS7 IP has USB 0 enabled (the
// Arty's ps7_init comes from this configuration and has to set USB up),
// which adds three PL-side ports: no power fault (USB0_VBUS_PWRFAULT
// low), the other two unused. The USB PHY's reset is a PS pin here
// (MIO 46), so there is no OTG_RESETN. The DDR ports are 32 bits wide
// as on the ZedBoard (the IP's ports don't change with the bus width);
// the Arty's 16-bit DDR uses the low half.
`default_nettype none
module chip8_zynq
    ( input  wire       clk
    , input  wire [3:0] btn
    , input  wire [1:0] sw
    , output wire [3:0] led
    , output wire       hdmi_tx_clk_p, hdmi_tx_clk_n
    , output wire [2:0] hdmi_tx_d_p, hdmi_tx_d_n
      // mono sound (audio.xdc; phase 2 step 6)
    , output wire       aud_pwm, aud_sd
      // PS fixed I/O and DDR
    , inout  wire [53:0] MIO
    , inout  wire        PS_CLK, PS_PORB, PS_SRSTB
    , inout  wire [14:0] DDR_Addr
    , inout  wire [2:0]  DDR_BankAddr
    , inout  wire        DDR_CAS_n, DDR_CKE, DDR_Clk, DDR_Clk_n, DDR_CS_n
    , inout  wire        DDR_DRSTB, DDR_ODT, DDR_RAS_n, DDR_WEB
    , inout  wire [3:0]  DDR_DM
    , inout  wire [31:0] DDR_DQ
    , inout  wire [3:0]  DDR_DQS, DDR_DQS_n
    , inout  wire        DDR_VRN, DDR_VRP
    );

  wire [63:0] gpio_o;
  wire [31:0] ps_in;
  wire        ps_resetn;

  zynq_ps ps_i
    ( .GPIO_I       ({ps_in, 32'b0})
    , .GPIO_O       (gpio_o)
    , .GPIO_T       ()
    , .USB0_PORT_INDCTL    ()
    , .USB0_VBUS_PWRSELECT ()
    , .USB0_VBUS_PWRFAULT  (1'b0)
      // the SD card's write protect, through EMIO (ps7.tcl): active high
      // (1 = protected: tying it high made the card read-only, the Arty,
      // 2026-10-06); the Arty has no switch, so always writable
    , .SDIO0_WP     (1'b0)
      // the PS's PL reset, the design's reset too (ps7.tcl, artyClocksPs)
    , .FCLK_RESET0_N (ps_resetn)
    , .MIO          (MIO)
    , .PS_CLK       (PS_CLK)
    , .PS_PORB      (PS_PORB)
    , .PS_SRSTB     (PS_SRSTB)
    , .DDR_Addr     (DDR_Addr)
    , .DDR_BankAddr (DDR_BankAddr)
    , .DDR_CAS_n    (DDR_CAS_n)
    , .DDR_CKE      (DDR_CKE)
    , .DDR_Clk      (DDR_Clk)
    , .DDR_Clk_n    (DDR_Clk_n)
    , .DDR_CS_n     (DDR_CS_n)
    , .DDR_DRSTB    (DDR_DRSTB)
    , .DDR_ODT      (DDR_ODT)
    , .DDR_RAS_n    (DDR_RAS_n)
    , .DDR_WEB      (DDR_WEB)
    , .DDR_DM       (DDR_DM)
    , .DDR_DQ       (DDR_DQ)
    , .DDR_DQS      (DDR_DQS)
    , .DDR_DQS_n    (DDR_DQS_n)
    , .DDR_VRN      (DDR_VRN)
    , .DDR_VRP      (DDR_VRP)
    );

  chip8_arty_top chip8_i
    ( .clk (clk)
    , .PS_RESETN (ps_resetn)
    , .btn (btn)
    , .sw (sw)
    , .PS_GPIO (gpio_o[31:0])
    , .led (led)
    , .hdmi_tx_clk_p (hdmi_tx_clk_p), .hdmi_tx_clk_n (hdmi_tx_clk_n)
    , .hdmi_tx_d_p (hdmi_tx_d_p), .hdmi_tx_d_n (hdmi_tx_d_n)
    , .PS_IN (ps_in)
    , .aud_pwm (aud_pwm), .aud_sd (aud_sd)
    );
endmodule
