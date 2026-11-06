# ZedBoard reference (for this project)

> Since 2026-10-06 the ZedBoard is the second board; the Arty Z7-20 is
> the main one (docs/ARTYZ7.md, CLAUDE.md "Boards"). Everything here
> still holds; its tops keep building and stay in `make ci`.

> The pin tables below were first written from memory. On 2026-10-02 every
> pin listed here was checked against the master XDC
> (`boards/zedboard/reference/zedboard_master.xdc`, the Avnet/Digilent file
> provided by the user) and matched. The constraints actually used are
> `boards/zedboard/zedboard.xdc` and `vga.xdc`, derived from the master and
> checked against it by `tools/check-xdc.py` on every `make test`. If this
> page and those files disagree, the XDC files win.

## Device

- The user's board is **ZedBoard Rev F** (printed on the PCB).
- Zynq-7000 **XC7Z020-1CLG484C**: dual Cortex-A9 (PS) plus Artix-7-class PL
- PL resources: ~53k LUTs, ~106k FFs, 140 × RAMB36 (4.9 Mbit ≈ 560 KiB), 220 DSP48
- 512 MiB DDR3 attached to the **PS**. The PL reaches it only through the
  AXI HP/GP ports.

## Clocks

| Signal | Pin | Notes |
|---|---|---|
| `GCLK` | Y9 | 100 MHz oscillator, bank 13, LVCMOS33 |

The pixel clock comes from an MMCM (`Clash.Xilinx.ClockGen.clockWizard`, or a
hand-instantiated `MMCME2_BASE` black box). 25.175 MHz is the standard
640×480@60 clock. 25.0 MHz (100/4) is accepted by most monitors and is
simpler for the very first bring-up.

## User I/O

| Resource | Pins | Bank / standard |
|---|---|---|
| LEDs LD0 to LD7 | T22, T21, U22, U21, V22, W22, U19, U14 | bank 33, LVCMOS33 |
| Buttons BTNC/D/L/R/U | P16, R16, N15, R18, T18 | bank 34, LVCMOS25 (VADJ) |
| Switches SW0 to SW7 | F22, G22, H22, F21, H19, H18, H17, M15 | banks 34/35, LVCMOS25 (VADJ) |

**VADJ = 2.5 V (confirmed on this board).** Banks 34 and 35 run at the
voltage selected by the VADJ jumper J18. On this board it is set to
**2.5 V**, so every pin in banks 34/35 (buttons, switches) uses
**`LVCMOS25`**. The 3.3 V position on J18 is deliberately unpopulated by
Digilent; don't populate it. The fixed 3.3 V banks 13 and 33 (GCLK, LEDs,
VGA, Pmod JA/JB) use `LVCMOS33`.

The master XDC (`boards/zedboard/reference/zedboard_master.xdc`) sets banks
34/35 to `LVCMOS18` by default in its bank-wide IOSTANDARD lines at the end
of the file. The derived files set IOSTANDARD per port instead, with
`LVCMOS25` for banks 34/35; `tools/check-xdc.py` enforces this.

### Port names (shared by every top-level design)

| Port | Pins | Notes |
|---|---|---|
| `GCLK` | Y9 | 100 MHz in |
| `LD[7:0]` | LD0 to LD7 | master `LDn` = bit n |
| `BTNC`, `BTND`, `BTNL`, `BTNR`, `BTNU` | as above | master names |
| `SW[7:0]` | SW0 to SW7 | master `SWn` = bit n |
| `VGA_R[3:0]`, `VGA_G[3:0]`, `VGA_B[3:0]`, `VGA_HS`, `VGA_VS` | `vga.xdc` | master `VGA_Rn` = bit n−1 |

What each switch or button *means* is up to each top-level design, not the
board (for example SW7 is the LED view select in the M0a top, but the
OLED mode select in the CHIP-8 top).

## Configuration and boot

- **Boot mode: JTAG** for development. Jumpers JP8, JP9 and JP10 are all on
  GND. The PL is programmed over the PROG USB port (J17) by `make prog`.
- **DONE LED (LD12, blue)** lights once the PL is configured. With no
  bitstream loaded the PL is empty, so the buttons and user LEDs do
  nothing. That's normal, not a fault.
- **Boot mode: SD card** (side track F): JP8 on GND, JP9 and JP10 on 3V3
  (MIO[5:3] = 110). Verified 2026-10-04 in the HW user's guide (Rev. 2.2,
  table 18 and the jumper table: JP8 = MIO[3], JP9 = MIO[4], JP10 =
  MIO[5]) and UG585 v1.8.1 table 6-4 (SD card: BOOT_MODE pins MIO[5],
  MIO[4], MIO[3] = 1, 1, 0). JP7 (MIO[2], JTAG chain) and JP11 (MIO[6],
  PLL) stay on GND. The boot ROM reads `BOOT.BIN` from the root of the
  card's first partition, FAT16/32 (UG585 6.3.7; `make sd`'s card is one
  FAT32 partition from 1 MiB). Moving the jumpers is the user's action,
  with the board off. **`make prog` still works with the jumpers on SD**
  (checked on the board 2026-10-04: DONE lit, with J17 connected), so
  they can stay there for development.
- **J14 after power-on:** the USB-UART bridge is powered by the board, so
  it re-enumerates at each power-on and sends one stray byte to the PS.
  `link` resynchronises the relay's parser before each ping (side track
  F); before that fix, the first `make ping` after an SD boot failed.

## Video

### VGA (first output)

12-bit (4:4:4) resistor-ladder DAC on bank 33 (LVCMOS33):

| Signal | Pins |
|---|---|
| R[3:0] | V20, U20, V19, V18 |
| G[3:0] | AB22, AA22, AB21, AA21 |
| B[3:0] | Y21, Y20, AB20, AB19 |
| HSYNC / VSYNC | AA19 / Y19 |

The master names them `VGA_R1` to `VGA_R4`; `vga.xdc` maps `VGA_Rn` to
`VGA_R[n-1]`, assuming `VGA_R4` is the MSB of the ladder. That assumption is
not yet checked against the schematic: confirm it in the M0b monitor test.

640×480@60 timing (pixel clock 25.175 MHz):

| | Visible | Front porch | Sync | Back porch | Total | Polarity |
|---|---|---|---|---|---|---|
| Horizontal | 640 | 16 | 96 | 48 | 800 | negative |
| Vertical | 480 | 10 | 2 | 33 | 525 | negative |

### HDMI (milestone M0d)

An **Analog Devices ADV7511** transmitter drives the HDMI Type A
connector (HW user's guide v2.2, section 2.4.1). The FPGA never sees TMDS:
the PL sends parallel video to the chip and configures it over I2C, and
the chip does the TMDS encoding. Facts, with sources:

- **Video bus:** 16 data lines, wired to the chip's D[23:8], so the input
  format is **16-bit YCbCr 4:2:2** (input ID 1 with separate HSYNC/VSYNC,
  or ID 2 with embedded syncs); RGB 4:4:4 would need 24 or 36 lines and is
  not possible here. Plus DE, HSYNC, VSYNC and the pixel clock (HD_CLK),
  all from PL pins. Sources: HW user's guide 2.4.1, ADI's ADV7511
  reference-design user guide (ZED: 16-bit YCbCr 4:2:2, separate syncs),
  ADV7511 Hardware User's Guide (input formats).
- **Bus layout, cross-check from ADI's reference HDL** (analogdevicesinc/hdl
  `library/axi_hdmi_tx/axi_hdmi_tx_core.v`, `library/common/ad_ss_444to422.v`,
  `projects/adv7511/zed/system_top.v`, fetched 2026-10-02 into the
  git-ignored `local/reference/adi-hdl/`; facts only, no code taken):
  `hdmi_data[15:8]` = chroma, `[7:0]` = Y, chroma **Cb first** on each
  line (`CR_CB_N = 0`), the 16 bits go straight to HD_D[15:0]; the pixel
  clock leaves through an ODDR. ADI filters chroma with [1 2 1] / 4, we
  average pixel pairs (`Platform.Video.Ycbcr422`). **Confirmed** by the
  ADV7511 Hardware User's Guide, Table 7 (input ID 1, right-justified,
  style 1, 16 bit: Cb/Cr[7:0] on D[23:16], Y[7:0] on D[15:8]).
- **Input timing:** the chip samples on HD_CLK's rising edge and needs
  1 ns set-up, 0.7 ns hold (HW guide, AC specifications; adjustable in
  0.4 ns steps by register 0xBA). We forward the pixel clock inverted
  through an ODDR, so data has half a period either side; `hdmi.xdc`
  states the figures as output delays (M0d build: 18.4 ns set-up, 18.7 ns
  hold slack).
- **Configuration:** I2C (HD-SCL, HD-SDA) on PL pins, so the PL can
  configure the chip without the PS. Interrupt output HD-INT on a PL pin.
  The chip handles hot-plug detect itself and reports it in a register.
  7-bit I2C address **0x39** (0x72 as an 8-bit write address): ADI's own
  ZedBoard device tree (`zynq-zed-adv7511.dtsi`, `hdmi@39`) uses it, and
  the Programming Guide (4.1) gives 0x72 for PD/AD low at power-up, with
  a 200 ms wait before the address is valid. The same
  tree gives ADI's input settings for this board: 8 bits per component,
  YCbCr 4:2:2, 1× clock, style 1, right-justified, clock delay 0
  (`Platform.Video.Adv7511.Table`).
- **Audio:** the chip takes S/PDIF and I2S; the ZedBoard wires one
  S/PDIF line to it (HD_SPDIF, U15 in the master XDC; `hdmi_audio.xdc`).
  HDMI audio needs HDMI mode with infoframes. Used by the Game Boy (step
  2.9): `Platform.Audio.Spdif` sends 48 kHz stereo through an ODDR, and
  `adv7511HdmiAudioTable` (`Platform.Video.Adv7511.Table`, every entry
  cited) sets HDMI mode, S/PDIF input, N = 6144 with measured CTS, and
  the AVI and audio infoframes. No MCLK is needed for S/PDIF.
- **Colour:** the chip has a programmable colour space converter, so it
  can turn the 4:2:2 YCbCr input back into RGB for the monitor (needed in
  DVI mode, where sinks expect RGB).
- Pin numbers: only from the master XDC (`hdmi.xdc`, derived and checked
  by `tools/check-xdc.py` like the other files). The HW guide's table
  lists HD-INT at W16 and HD-SCL at AA18; that is a cross-check, not a
  source. Two disagreements with the master XDC (checked 2026-10-02), where
  the XDC wins: the guide puts the HDMI signals in "Bank 35 (3.3V)", the
  XDC in bank 33 (fixed 3.3 V, `LVCMOS33`; bank 35 is VADJ = 2.5 V on this
  board); and the guide lists HD-D1 at AA12, the XDC at AA13.

Needed in the PL (all written in Clash for this project, PLAN.md M0d): an
I2C master and an init sequencer with HPD handling, an RGB → YCbCr
converter with 4:2:2 subsampling, and a forwarded pixel clock (ODDR).
640×480@60 at the existing 25.176 MHz pixel clock is CEA-861 VIC 1; later
720p needs 74.25 MHz from the MMCM.

Note for the Arty Z7-20: its HDMI port is wired straight to FPGA pins
(TMDS_33), so there a TMDS encoder and 10:1 serialisers are needed
instead, and none of the ADV7511 work carries over below the pixel-stream
interface.

## Audio

- **ADAU1761** codec, connected to the PL through I2S plus I2C for
  configuration. Line out and headphone jacks. Needs an I2C init sequencer
  (shared with HDMI) and an I2S transmitter, so it comes after milestone 1.
- **For CHIP-8's beeper:** a square wave on a Pmod pin into a Pmod AMP2 or a
  piezo is enough to start with.

## Expansion

- **Pmod JA to JD:** 4 × 12-pin PL Pmods. JA/JB are bank 13 (3.3 V); JC/JD
  are differential-capable.
  Planned uses: Pmod KYPD (4×4 hex keypad, a perfect fit for CHIP-8),
  NES/SNES controller adapter, Pmod USBUART (ROM loader stage B), and
  Pmod AMP2.
- **FMC LPC:** unused.
- **OLED:** Univision/Wisechip UG-2832HSWEG04, 128×32 monochrome, SSD1306
  controller on 4-wire SPI (write only), bank 13 (3.3 V, `LVCMOS33`).
  Pins (master XDC, matching Table 11 of Avnet's ZedBoard Hardware User's
  Guide v2.2, section 2.4.4; a local copy, not in git, may be in
  `local/reference/`): `OLED_DC` U10 (D/C#), `OLED_RES`
  U9 (RES#, active low), `OLED_SCLK` AB12, `OLED_SDIN` AA12, `OLED_VBAT`
  U11, `OLED_VDD` U12. CS# is pulled low on the board, so there's no
  chip-select pin.
  - **Power control is active low** (read from the Rev D.2 schematic,
    sheet 6; the user's board is **Rev F**, whose schematic wasn't
    available, but a mismatch would only leave the panel unpowered):
    `OLED_VDD` and `OLED_VBAT` each drive the gate of a P-channel MOSFET (Q3, Q2) whose source is on VCC3V3 and whose drain feeds the
    panel's VDD / VBAT pin. A 10 kΩ pull-up to 3.3 V on each gate (R134,
    R132) keeps the panel off while the pin is high or undriven (for
    example before configuration). Drive **0** to switch a supply on. This
    is the same as on the Pmod OLED.
  - Power-up order (SSD1306 / Pmod OLED reference): VDD on, reset pulse,
    init commands, VBAT on, wait 100 ms, display on. Power-down: display
    off, VBAT off, wait 100 ms, VDD off.
  - Constraints: `boards/zedboard/oled.xdc` (checked by `tools/check-xdc.py`).
    Driver: `Platform.Video.Oled` (SPI mode 3 at the pixel clock / 8, about
    3.1 MHz; horizontal addressing, 512-byte frames streamed continuously).
    Display orientation (segment remap A0/A1, COM scan C0/C8) is not yet
    confirmed for this board: the driver defaults to Digilent's Pmod OLED
    setting (A1, C8), and the M0c top lets SW0/SW1 flip it at run time.
    The driver never runs the power-down sequence: when the PL is
    reprogrammed or powered off, the pins float and the gate pull-ups cut
    VDD and VBAT together. Digilent's demos do the same.
  - The on-board CHIP-8 display (PLAN.md M0c, step 7). CHIP-8's 64×32 fits
    either 1:1 in half the panel or stretched 2× horizontally (not 2× in
    both directions, which would be 128×64); SW7 selects in the CHIP-8 top.

## PS side (in use since milestone 1: keyboard relay and ROM loading; SD card menu since M1b stage C)

- USB-UART (J14, Cypress CY7C64225) is wired to **PS UART1 on MIO 48/49**
  (HW user's guide v2.2, section 2.3.2). The PL can't reach it at all
  without PS software: EMIO would only connect the UART *controller* to PL
  pins, and the bridge chip isn't on PL pins.
- USB OTG (J13, TI TUSB1210 ULPI PHY on MIO 28 to 39, PS usb0): host mode
  needs JP2 shorted (5 V Vbus out) and JP3 shorted (Vbus capacitor), plus a
  USB host stack on the PS (side track E). Avnet's guide warns of a
  TUSB1210/Zynq ULPI timing incompatibility; it came from the PHY's old
  datasheet (minimum output delay 0.1 ns against the Zynq's 1.0 ns hold
  requirement), and TI's revised datasheet specifies 1.2 ns. The Rev D.2
  errata also report that plugging in a USB or Ethernet cable after boot
  resets the board; this board does not (side track E, 2026-10-04: the
  keyboard receiver replugged on J13 while the PS app ran). A keyboard receiver plugs in through a micro-USB OTG
  adapter (micro-B plug to USB-A socket).
  Side track E, step 1 (2026-10-04): Vivado's ZedBoard `ps7_init` already
  muxes MIO 28 to 39 to usb0 (ULPI, LVCMOS18) and turns on its AMBA clock.
  The HW guide's table 5 puts `OTG_RESET_B` (the PHY's reset) in "MIO Bank
  1/501" without naming a pin; in fact it is a **PL pin, G17** (bank 35,
  `OTG_RESETN` in the master XDC), reachable from MIO 7 only after
  shorting R145 (not done). Undriven, the PHY stays in reset: no ULPI
  clock, "USB: no PHY" (seen on the board 2026-10-04; also reported on
  the meta-xilinx list, Dec. 2014). Every PS wrapper ties it high
  (`boards/zedboard/usb.xdc`, `assign OTG_RESETN = 1'b1`). Vbus on J13 comes from the PHY (OTG control
  DrvVbus + DrvVbusExternal), not from PORTSC1.PP, which only drives an
  EMIO signal; the over-current flag from the TPS2051 is on a PL pin
  (`OTG_VBUS_OC`, bank 34, L16) and is not used.
- PS bring-up without an FSBL or SD card, over JTAG (`make prog` for a
  system with `PS_APP`, `boards/zedboard/prog_ps.tcl` through xsdb): system
  reset, PL bitstream, the ZedBoard `ps7_init.tcl` that ships with Vivado
  (`data/embeddedsw/sample_sdt/zed/sdt/`, UART1 at 115200 with a 50 MHz
  reference clock, CPU at 666.67 MHz), `ps7_post_config` (level shifters
  on), then the app into OCM at 0x0 on core 0. The PL side instantiates the
  PS7 as the `processing_system7` IP (`boards/zedboard/ps7.tcl`, module
  `zynq_ps`) inside a hand-written wrapper (`chip8_zynq.v`).
- On the desktop, J14 enumerates as a CDC-ACM serial port (`/dev/ttyACM*`;
  `make keys` uses the Cypress entry in `/dev/serial/by-id/`). Access needs
  the `dialout` group. The user's bridge ("Cypress-USB2UART-Ver1.0G")
  works at 230400 baud but not at 460800 or 921600 (measured 2026-10-02).
- SD card (J12) on PS SD 0: MIO 40 to 45, card detect on MIO 47, write
  protect on MIO 46, 50 MHz reference clock (IO PLL / 20); all set by the
  same `ps7_init.tcl` (`SDIO_CLK_CTRL` = 0x1401, MIO pins 0x380). The
  PS app reads ROMs from it (`platform/ps/sd.zig`, `fat.zig`). Booting
  from the card is not used yet: boot mode stays JTAG until side track F
  (our own first-stage loader, PL configured from the card).
- EMIO GPIO: 64 lines, 0 to 31 PS → PL and 32 to 63 PL → PS (the PS link,
  `docs/ARCHITECTURE.md`).
- JTAG over USB (J17), used for programming and the ILA.
