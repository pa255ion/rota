// ZedBoard top for the Game Boy Color with the PS: the Clash design (gbc_top) plus
// the Zynq PS7 with its HP0 port (IP "zynq_ps", boards/zedboard/ps7_hp.tcl;
// side track I phase 2: the cartridge ROM is in DDR), as m0e_zynq.v.
//
// Hand-written (CLAUDE.md rule 4 allows Xilinx primitive wrappers in
// boards/): the PS7 has to reach the top level with its fixed DDR and MIO
// pins as inout ports, which is plain wiring here but awkward to express
// from Clash. No logic lives in this file. The PS drives the Game Boy's
// buttons, the cartridge loader and the menu overlay through EMIO GPIO 0
// to 31 and reads buttons, switches and the system ID on 32 to 63
// (platform/ps/relay.zig, layout in Platform.Ps.Link); gbc_top synchronises
// them (Gb.TopCgb, Gb.Top). HP0 as in m0e_zynq.v: gbc_top drives ARVALID, ARADDR,
// ARLEN and RREADY and gets ARREADY and the R channel; the fixed AR fields
// (Platform.Mem.Axi) and the unused write channels are constants here, and
// HP0 is clocked by gbc_top's HP0_ACLK (its pixel clock). Board pin names
// as in zedboard.xdc, vga.xdc and hdmi.xdc; the PS pins need no
// constraints of ours.
`default_nettype none
module gbc_zynq
    ( input  wire       GCLK
    , input  wire       BTNC, BTND, BTNL, BTNR, BTNU
    , input  wire [7:0] SW
    , output wire [7:0] LD
    , output wire [3:0] VGA_R, VGA_G, VGA_B
    , output wire       VGA_HS, VGA_VS
    , output wire        HD_CLK
    , output wire [15:0] HD_D
    , output wire        HD_DE, HD_HSYNC, HD_VSYNC
    , inout  wire        HD_SCL, HD_SDA
    , output wire        HD_SPDIF
      // the USB PHY's reset (usb.xdc)
    , output wire        OTG_RESETN
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

  // The PS link (Platform.Ps.Link), 64 EMIO lines: 0 to 31 PS -> PL
  // (buttons from the desktop, cartridge loading, the menu overlay), 32 to
  // 63 PL -> PS (gbc_top's PS_IN: buttons, switches, system ID 6).
  wire [63:0] gpio_o;
  wire [31:0] ps_gpio = gpio_o[31:0];
  wire [31:0] ps_in;
  wire        ps_resetn;

  // HP0, read side (gbc_top) and its clock
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
      // the PS's PL reset, the design's reset too (ps7_hp_rst.tcl,
      // Platform.Clocks.pixelClockGenPs: the Game Boy's reset race)
    , .FCLK_RESET0_N (ps_resetn)
      // HP0: the read channels from gbc_top, fixed fields per
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

  gbc_top gb_i
    ( .GCLK (GCLK)
    , .PS_RESETN (ps_resetn)
    , .BTNC (BTNC), .BTND (BTND), .BTNL (BTNL), .BTNR (BTNR), .BTNU (BTNU)
    , .SW (SW)
    , .PS_GPIO (ps_gpio)
    , .HP0_ARREADY (hp_arready)
    , .HP0_RVALID (hp_rvalid)
    , .HP0_RDATA (hp_rdata)
    , .HP0_RRESP (hp_rresp)
    , .HP0_RLAST (hp_rlast)
    , .LD (LD)
    , .VGA_R (VGA_R), .VGA_G (VGA_G), .VGA_B (VGA_B)
    , .VGA_HS (VGA_HS), .VGA_VS (VGA_VS)
    , .HD_CLK (HD_CLK), .HD_D (HD_D), .HD_DE (HD_DE)
    , .HD_HSYNC (HD_HSYNC), .HD_VSYNC (HD_VSYNC)
    , .HD_SCL (HD_SCL), .HD_SDA (HD_SDA)
    , .HD_SPDIF (HD_SPDIF)
    , .PS_IN (ps_in)
    , .HP0_ACLK (hp_aclk)
    , .HP0_ARVALID (hp_arvalid)
    , .HP0_ARADDR (hp_araddr)
    , .HP0_ARLEN (hp_arlen)
    , .HP0_RREADY (hp_rready)
    );
    // The USB-OTG PHY (TUSB1210) out of reset whenever this design is
    // loaded: its reset pin is on the PL (usb.xdc); the PS app sets it up
    // over ULPI (platform/ps/usb.zig).
    assign OTG_RESETN = 1'b1;
endmodule
