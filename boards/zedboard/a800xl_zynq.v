// ZedBoard top for the Atari 800XL with the PS (M4 step 4.8): the Clash
// design (a800xl_top) plus the Zynq PS7 (IP "zynq_ps",
// boards/zedboard/ps7.tcl), as nes_zynq.v.
//
// Hand-written (CLAUDE.md rule 4 allows Xilinx primitive wrappers in
// boards/): the PS7 has to reach the top level with its fixed DDR and MIO
// pins as inout ports, which is plain wiring here but awkward to express
// from Clash. No logic lives in this file. The PS drives the 800XL's
// keyboard, the program loader and the menu overlay through EMIO GPIO 0
// to 31 and reads buttons, switches and the system ID on 32 to 63
// (platform/ps/relay.zig, layout in Platform.Ps.Link); a800xl_top synchronises
// them (A800.Top). Board pin names as in zedboard.xdc, vga.xdc and hdmi.xdc;
// the PS pins need no constraints of ours.
`default_nettype none
module a800xl_zynq
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
  // (the keyboard, program loading, the menu overlay), 32 to
  // 63 PL -> PS (a800xl_top's PS_IN: buttons, switches, system ID 5).
  wire [63:0] gpio_o;
  wire [31:0] ps_gpio = gpio_o[31:0];
  wire [31:0] ps_in;

  zynq_ps ps_i
    ( .GPIO_I       ({ps_in, 32'b0})
    , .GPIO_O       (gpio_o)
    , .GPIO_T       ()
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

  a800xl_top a800xl_i
    ( .GCLK (GCLK)
    , .BTNC (BTNC), .BTND (BTND), .BTNL (BTNL), .BTNR (BTNR), .BTNU (BTNU)
    , .SW (SW)
    , .PS_GPIO (ps_gpio)
    , .LD (LD)
    , .VGA_R (VGA_R), .VGA_G (VGA_G), .VGA_B (VGA_B)
    , .VGA_HS (VGA_HS), .VGA_VS (VGA_VS)
    , .HD_CLK (HD_CLK), .HD_D (HD_D), .HD_DE (HD_DE)
    , .HD_HSYNC (HD_HSYNC), .HD_VSYNC (HD_VSYNC)
    , .HD_SCL (HD_SCL), .HD_SDA (HD_SDA)
    , .HD_SPDIF (HD_SPDIF)
    , .PS_IN (ps_in)
    );
    // The USB-OTG PHY (TUSB1210) out of reset whenever this design is
    // loaded: its reset pin is on the PL (usb.xdc); the PS app sets it up
    // over ULPI (platform/ps/usb.zig).
    assign OTG_RESETN = 1'b1;
endmodule
