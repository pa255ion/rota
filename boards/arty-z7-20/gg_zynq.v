// The Game Gear on the Arty Z7-20 with its PS (M5 step 5.9): the Arty's
// Zynq PS7 with HP0 (IP "zynq_ps", boards/arty-z7-20/ps7_hp.tcl; the
// cartridge ROM is in DDR, read by Gg.Machine) and gg_arty_top
// (Gg.ArtyTop). The PS7 instance is gb_zynq.v's (side track G), HP0
// tie-offs included. The PS link (EMIO GPIO) carries the keys (step 5.9,
// the Game Boy's layout), the menu and loading (the PS app's /gg), and the
// system ID.
//
// Hand-written (CLAUDE.md rule 4 allows Xilinx primitive wrappers in
// boards/): the PS7 has to reach the top level with its fixed DDR and MIO
// pins as inout ports, which is plain wiring here. No logic lives in this
// file.
`default_nettype none
module gg_zynq
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

  // The PS link (Platform.Ps.Link), 64 EMIO lines: 0 to 31 PS -> PL (the
  // desktop's and the USB keyboard's keys, the menu's pause), 32 to 63
  // PL -> PS (gg_arty_top's PS_IN: buttons, switches, system ID 7).
  wire [63:0] gpio_o;
  wire [31:0] ps_gpio = gpio_o[31:0];
  wire [31:0] ps_in;
  wire        ps_resetn;

  // HP0, read side (gg_arty_top) and its clock
  wire        hp_aclk;
  wire        hp_arvalid, hp_arready, hp_rvalid, hp_rready, hp_rlast;
  wire [31:0] hp_araddr;
  wire [3:0]  hp_arlen;
  wire [63:0] hp_rdata;
  wire [1:0]  hp_rresp;

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
      // HP0: the read channels from gg_arty_top, fixed fields per
      // Platform.Mem.Axi
    , .S_AXI_HP0_ACLK    (hp_aclk)
    , .S_AXI_HP0_ARVALID (hp_arvalid)
    , .S_AXI_HP0_ARREADY (hp_arready)
    , .S_AXI_HP0_ARADDR  (hp_araddr)
    , .S_AXI_HP0_ARLEN   (hp_arlen)
    , .S_AXI_HP0_ARSIZE  (3'b011)
    , .S_AXI_HP0_ARBURST (2'b01)
    , .S_AXI_HP0_ARCACHE (4'b0011)
    , .S_AXI_HP0_ARPROT  (3'b000)
    , .S_AXI_HP0_ARLOCK  (2'b00)
    , .S_AXI_HP0_ARQOS   (4'b0000)
    , .S_AXI_HP0_ARID    (6'd0)
    , .S_AXI_HP0_RVALID  (hp_rvalid)
    , .S_AXI_HP0_RREADY  (hp_rready)
    , .S_AXI_HP0_RDATA   (hp_rdata)
    , .S_AXI_HP0_RRESP   (hp_rresp)
    , .S_AXI_HP0_RLAST   (hp_rlast)
    , .S_AXI_HP0_RID     ()
    , .S_AXI_HP0_RCOUNT  ()
    , .S_AXI_HP0_RACOUNT ()
    , .S_AXI_HP0_RDISSUECAP1_EN (1'b0)
      // HP0, write side: unused
    , .S_AXI_HP0_AWVALID (1'b0)
    , .S_AXI_HP0_AWREADY ()
    , .S_AXI_HP0_AWADDR  (32'd0)
    , .S_AXI_HP0_AWLEN   (4'd0)
    , .S_AXI_HP0_AWSIZE  (3'b011)
    , .S_AXI_HP0_AWBURST (2'b01)
    , .S_AXI_HP0_AWCACHE (4'b0011)
    , .S_AXI_HP0_AWPROT  (3'b000)
    , .S_AXI_HP0_AWLOCK  (2'b00)
    , .S_AXI_HP0_AWQOS   (4'b0000)
    , .S_AXI_HP0_AWID    (6'd0)
    , .S_AXI_HP0_WVALID  (1'b0)
    , .S_AXI_HP0_WREADY  ()
    , .S_AXI_HP0_WDATA   (64'd0)
    , .S_AXI_HP0_WSTRB   (8'd0)
    , .S_AXI_HP0_WLAST   (1'b0)
    , .S_AXI_HP0_WID     (6'd0)
    , .S_AXI_HP0_BVALID  ()
    , .S_AXI_HP0_BREADY  (1'b1)
    , .S_AXI_HP0_BRESP   ()
    , .S_AXI_HP0_BID     ()
    , .S_AXI_HP0_WCOUNT  ()
    , .S_AXI_HP0_WACOUNT ()
    , .S_AXI_HP0_WRISSUECAP1_EN (1'b0)
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

  gg_arty_top gg_i
    ( .clk (clk)
    , .PS_RESETN (ps_resetn)
    , .btn (btn)
    , .sw (sw)
    , .PS_GPIO (ps_gpio)
    , .HP0_ARREADY (hp_arready)
    , .HP0_RVALID (hp_rvalid)
    , .HP0_RDATA (hp_rdata)
    , .HP0_RRESP (hp_rresp)
    , .HP0_RLAST (hp_rlast)
    , .led (led)
    , .hdmi_tx_clk_p (hdmi_tx_clk_p), .hdmi_tx_clk_n (hdmi_tx_clk_n)
    , .hdmi_tx_d_p (hdmi_tx_d_p), .hdmi_tx_d_n (hdmi_tx_d_n)
    , .aud_pwm (aud_pwm), .aud_sd (aud_sd)
    , .HP0_ACLK (hp_aclk)
    , .HP0_ARVALID (hp_arvalid)
    , .HP0_ARADDR (hp_araddr)
    , .HP0_ARLEN (hp_arlen)
    , .HP0_RREADY (hp_rready)
    , .PS_IN (ps_in)
    );
endmodule
