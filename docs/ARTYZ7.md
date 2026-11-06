# Arty Z7-20 reference (side track G)

> **The main development board since 2026-10-06** (the user's decision;
> CLAUDE.md "Boards"): board checks go here first; the ZedBoard
> (docs/ZEDBOARD.md) stays supported.

> The second board (PLAN.md, "Side track G"). Pins come only from
> Digilent's master XDC, `boards/arty-z7-20/reference/arty_z7_20_master.xdc`
> (Rev. B); the constraints used are `boards/arty-z7-20/*.xdc`, checked
> pin by pin and IOSTANDARD by IOSTANDARD against the master by
> `tools/check-xdc.py` on every `make test`. If this page and those files
> disagree, the XDC files win. Phase 1 (the PL side) is done and
> board-checked (2026-10-06); phase 2 (the PS) is next (PLAN.md).

## Device

- Zynq-7000 **xc7z020clg400-1**: the same PL as the ZedBoard's
  (xc7z020clg484-1) in a smaller package. Verified 2026-10-04 from
  Digilent's Vivado board file (`reference/digilent-board-files/board.xml`,
  `part_name`; MIT licence, from github.com/Digilent/vivado-boards,
  `new/board_files/arty-z7-20/A.0/`; `preset.xml` there holds the PS7
  settings phase 2 will need). `boards/arty-z7-20/build.tcl` sets it.
- JTAG/UART: one micro-USB port, an FT2232H (`0403:6010`, "Digilent
  Adept USB Device"; needs its own udev rule, docs/TOOLCHAIN.md); the
  user's board is cable **<serial>** in hw_server.
- **Boot mode, JP4:** it was on QSPI as shipped (the boot ROM then runs
  Digilent's demo from the flash after every reset); **on JTAG since
  2026-10-06** for development (the user's change), like the ZedBoard's
  JTAG setting. `prog_ps.tcl` halts core 0 after the reset anyway, so a
  QSPI boot can't get in the way. **On SD since 2026-10-06** (phase 2
  step 5, board-checked): the Arty boots its relay from the card's
  BOOT.BIN (`make boot-image BOARD=arty-z7-20`, `make sd`) and starts
  the last system; `make prog` still works with JP4 there.
- Every PL bank is 3.3 V: LVCMOS33 everywhere, TMDS_33 for the HDMI pairs
  (no VADJ question as on the ZedBoard).

## Building for it

Everything goes through the same targets with `BOARD=arty-z7-20` (the
default stays `zedboard`, whose paths and outputs don't change):

```sh
make hdl  SYSTEM=m0b BOARD=arty-z7-20
make sim  SYSTEM=m0b BOARD=arty-z7-20 FRAMES=3     # TMDS received and decoded
make bit  SYSTEM=m0b BOARD=arty-z7-20
make prog SYSTEM=m0b BOARD=arty-z7-20              # ask first (layer 6)
```

Settings come from `systems/<name>/arty-z7-20.mk`, outputs go to
`build/arty-z7-20/<name>/`. Systems with an Arty top so far: `m0a`,
`m0b`, `chip8`. Each Arty top is a separate Clash top-level,
`<Name>.ArtyTop`, next to the ZedBoard's `<Name>.Top`, reusing its logic.

## Clocks

| Signal | Pin | Notes |
|---|---|---|
| `clk` | H16 | 125 MHz (8.00 ns in the master), `Dom125` |

One MMCM (`boards/arty-z7-20/arty_mmcm.v`, behind the Clash blackbox
`Platform.Board.ArtyZ7.artyMmcm`) makes the pixel clock and 5× it for
the TMDS serialisers:

| | Value |
|---|---|
| M, D | 18.125, 2 (PFD 62.5 MHz, VCO 1132.8125 MHz; the -1 part allows 600 to 1200) |
| O0 → pixel clock | 45 → **25.17361 MHz** (39.724 ns) |
| O1 → 5× clock | 9 → 125.86806 MHz (7.945 ns) |

Why not `clockWizard`: Clash asks the Clocking Wizard for each output's
frequency on its own; for DomPix's 25.17623 MHz and 5× that, the wizard
picks M = 24.25, D = 3, O0 = 40.125, O1 = 8, i.e. 25.182 and 126.302 MHz,
not 1:5, which the serialisers need. The values above are the ones the
wizard itself picks when asked for 25.17361 and 125.86806 MHz (checked
2026-10-04, Vivado 2026.1); Vivado's clocks report shows 39.724 and
7.945 ns.

**Frequency error.** Systems keep `DomPix` (nominally 39720 ps,
25.17623 MHz, the ZedBoard's clock), so on the Arty everything that counts
pixel clocks runs **104 ppm (0.0104 %) slower** than on the ZedBoard, and
55 ppm slower than VGA's 25.175 MHz:

| | ZedBoard | Arty Z7-20 |
|---|---|---|
| Pixel clock | 25.17623 MHz | 25.17361 MHz |
| Frame rate (800 × 525) | 59.9434 Hz | 59.9372 Hz |
| CHIP-8 instructions, timers | 700 Hz / 10 kHz / 60 Hz | the same, −0.0104 % |
| Game Boy, NES (`DomPix` with clock enables, once ported) | as built | −0.0104 % speed |
| Sound pitch (all systems) | as built | −0.18 cent (1 cent ≈ 578 ppm), inaudible |

Monitors accept it: HDMI/DVI allows ±0.5 % on the pixel clock.

## User I/O

| Resource | Pins | Notes |
|---|---|---|
| LEDs LD0 to LD3 (`led[3:0]`) | R14, P14, N16, M14 | |
| RGB LED LD4 green (`led4_g`) | G17 | `rgb_led.xdc`; full drive, no PWM |
| Buttons BTN0 to BTN3 (`btn[3:0]`) | D19, D20, L20, L19 | |
| Switches SW0, SW1 (`sw[1:0]`) | M20, M19 | |

How each top maps them:

| Top | Buttons | Switches | LEDs |
|---|---|---|---|
| `M0a.ArtyTop`, `M0b.ArtyTop` | echoed on LD0 to LD3 | SW1 = view select (M0a's SW7): on, LD0/LD1 show SW0/SW1 | heartbeat on LD4 green |
| `Chip8.ArtyTop` | BTN0..3 = keys 4, 8, 2, 6 | SW0 = speed (ZedBoard SW5), SW1 = shift (BTNC): keys A, F, E, B | LD0 halted, LD1 key wait, LD2 vblank wait, LD3 sound |
| `Gb.ArtyTop` (phase 2) | BTN0..3 = Left, Down, Up, Right; BTN1 + BTN2 held = the menu | SW0 = palette (grey/green), SW1 = A | LD0 cartridge OK, LD1 frame toggle, LD2 loader hold, LD3 heartbeat |
| `Nes.ArtyTop` (phase 2) | BTN0..3 = Left, Down, Up, Right; BTN1 + BTN2 held = the menu | SW1 = A | as the Game Boy's |

In every top with the PS link the same five reach the PS for the SD
menu, in `PS_IN`'s button bits C, D, L, R, U: SW1 as C, BTN1 as D, BTN0
as L, BTN3 as R, BTN2 as U; the switches in its switch bits. CHIP-8's
menu combination is C alone, i.e. SW1 switched on for a second with no
button pressed (switch it back off in the menu: it is C, "open"). The
USB keyboard's Esc (phase 2 step 4) will be the easier way.

CHIP-8's quirks are fixed at VIP (the ZedBoard's SW0 to SW4 off); the
program is baked into the bitstream (`ROM=…`, loader stage A), as there
is no PS link on the Arty yet.

## HDMI output

The HDMI TX connector's TMDS pairs are FPGA pins (bank 35, TMDS_33),
unlike the ZedBoard's, which go through the ADV7511:

| Port | P / N pins |
|---|---|
| `hdmi_tx_clk_p/n` | L16 / L17 |
| `hdmi_tx_d_p/n[0]` (blue + syncs) | K17 / K18 |
| `hdmi_tx_d_p/n[1]` (green) | K19 / J19 |
| `hdmi_tx_d_p/n[2]` (red) | J18 / H18 |

HPD, CEC and DDC are not used. The path: the platform's pixel stream
(timing + `Rgb888`) → `Platform.Video.Tmds` (DVI 1.0 8b/10b with running
disparity, control symbols with HSYNC/VSYNC on channel 0 in blanking;
registered) → `boards/arty-z7-20/tmds_out.v` (per channel an OSERDESE2
master/slave pair, DDR at 5× the pixel clock, bit 0 first, and an
OBUFDS; channel 3 the clock symbol 0000011111). It is DVI: no audio, no
data islands (HDMI audio, our own, is phase 2 or later). The output
pairs have a false path in `hdmi_tx.xdc`: the sink recovers the clock
from the clock pair.

**The output is J11, "HDMI OUT"** (confirmed by the user at the first
board check, 2026-10-06; the master's "HDMI TX"); J10 is HDMI IN.

## Simulation (layer 4)

Arty tops have their own shim and harness (`platform/sim/vtop_arty.cpp`,
`arty.zig`, built as `arty-sim` by `zig build sim-arty`). The MMCM and
serialisers have Verilator stand-ins in `boards/sim/` (`arty_mmcm.v`:
5× clock = `clk`, pixel clock = `clk` / 5; `tmds_out.v`: the same
symbols out, bit 0 first, one bit per edge). The harness ticks `clk`
five times per pixel, samples the lines after each edge and hands them
to the TMDS sink (`tmds.zig`), which aligns on the clock pair, decodes
each lane, **re-encodes every data symbol with its own DVI encoder and
fails on any difference** (a wrong DC-balance choice would still decode
to the right picture), and feeds `vga.zig`'s decoder in 8-bit mode. TMDS
is lossless, so frames must equal the layer 3 / VGA goldens exactly:
M0b frame 2 and CHIP-8's IBM logo do.

## JTAG cable selection

Both boards are xc7z020s, so the JTAG chain can't tell them apart.
`make prog` first runs `tools/jtag-pick.sh`, which lists the connected
cables (`boards/zedboard/jtag_cables.tcl`, read-only) and picks one by the
serial in the git-ignored `local/boards.mk`:

```make
JTAG_SERIAL_zedboard   = <serial>   # the user's ZedBoard (J17), seen 2026-10-04
JTAG_SERIAL_arty-z7-20 = <its serial>   # make jtag-cables BOARD=arty-z7-20 with only the Arty plugged in
```

Rules: a board with a serial gets exactly that cable if it's connected,
else refusal; without one, the only connected cable, unless it is another
board's; several cables and no serial: refusal. `make jtag-cables
[BOARD=…]` shows the cables and the choice without programming
(`tools/test-jtag-pick.sh`, part of `make test`, checks the rules).
**Until `local/boards.mk` names both serials, keep only one board's USB
cable connected.**

## The PS (side track G phase 2)

Branch `side-arty-ps`; step list in PLAN.md ("Phase 2 step list").

- **PS7 configuration** (`boards/arty-z7-20/ps7.tcl`): Digilent's
  `ps7_preset`, all 771 parameters read from the board file, then ours:
  64 EMIO GPIO (the PS link), no GP0 master or fabric clock, the UART
  reference at 50 MHz (the preset's 100 would change the app's baud
  divisors). Digilent's DQS-to-clock delays for lanes 2 and 3 are
  slightly negative (-0.009, -0.033 ns), which Vivado reports as
  critical warnings PSU-1/PSU-2; those lanes don't exist on the 16-bit
  bus, so the values stay and the two IDs are lowered to warnings.
  `ps7_hp.tcl` adds HP0 (the Game Boy's cartridge).
- **ps7_init** (`make ps7-init BOARD=arty-z7-20`, Vivado, IP only):
  `build/arty-z7-20/ps7/ps7_init.c/.h/.tcl`, `clocks.txt`,
  `zynq_ps_ports.txt`. CPU 650 MHz (ARM PLL x26 / 2), IO PLL 1000 MHz,
  UART0 and SD 0 at 50 MHz, DDR3 at 525 MHz. `make ps7-check` (xsdb, no
  bitstream): ps7_init on the board and a DDR test over the 512 MiB;
  **passed 2026-10-06**.
- **The PS app** (`zig build ps-app -Dboard=arty-z7-20`,
  `build/arty-z7-20/ps/relay.elf`): `platform/ps/board_arty_z7_20.zig`
  (UART0 at 0xE0000000, global timer 325 MHz, the USB PHY's reset on
  MIO 46), linked with the generated ps7_init.c. The ZedBoard's build
  (`board_zedboard.zig`, the default) is byte-identical to before.
- **The PS7 IP's ports** are the ZedBoard's plus three for USB 0
  (enabled, since the Arty's ps7_init has to set USB up):
  `USB0_VBUS_PWRFAULT` is tied low, `USB0_PORT_INDCTL` and
  `USB0_VBUS_PWRSELECT` are left open. The DDR ports stay 32 bits wide.
- **Wrappers** (`boards/arty-z7-20/{chip8,gb,nes}_zynq.v`, the synthesis
  tops): the PS7 and the system's Arty top; the Game Boy's with HP0, as
  `boards/zedboard/gb_zynq.v`. `make prog BOARD=arty-z7-20` goes through
  xsdb (`boards/arty-z7-20/prog_ps.tcl` = the ZedBoard's script) with the
  Arty's ps7_init and app. `make keys/load/ping` use the FTDI's second
  interface (`/dev/serial/by-id/*Digilent*-if01-port0`).
- **The Arty tops** (`Gb.ArtyTop`, `Nes.ArtyTop`) repeat `Gb.Top`'s and
  `Nes.Top`'s machines line for line with the Arty's ends (side tracks I
  and D own those files, so the machines aren't factored out yet); a
  change there has to be repeated here. `make ci`'s `ci-arty` builds and
  simulates them.
- **Layer 4**: the Arty harness plays `--ps` scripts, has HP0's AXI slave
  (`--ddr-file`, stepped once per pixel clock, i.e. in ticks 4, 9, ...,
  where the stand-in MMCM's pixel clock rises) and `--expect-ps-text`.
  The NES's colours are 8 bits a channel, its goldens the VGA DAC's 4, so
  its frames are compared on the top 4 bits (`--golden-4bit`; the TMDS
  stream itself is still checked symbol by symbol).
- **xsdb**: the cable's properties (`jtag_cable_serial`) appear a moment
  after `connect`, so `prog_ps.tcl` and `ps7_check.tcl` retry each target
  selection for up to 4 s; `prog_ps.tcl` halts core 0 right after the
  system reset (JP4 on QSPI made the boot ROM start the flash's demo).
- **`make sd BOARD=arty-z7-20`**: the Arty's bitstreams in `/systems`,
  no Mooneye emulator-only ROMs by default (26 of 35 MiB). The card's
  write protect comes through EMIO (`SDIO0_WP`, active high, tied low in
  every wrapper: the Arty has no switch). Over the Arty's FTDI bridge
  link talks at 2 Mbaud; chunks the card already holds are skipped
  (docs/TESTING.md).
- **The design's reset is the PS's PL reset** (2026-10-06): ps7.tcl
  enables `FCLK_RESET0_N`, every wrapper wires it to the top's
  `PS_RESETN`, and `artyClocksPs` holds the pixel-clock reset while it's
  low. The PS releases it only after the PS-PL level shifters are on
  (ps7_post_config; `pcap.zig` after a menu load). Before, the Game Boy
  started its first HP0 read the moment the PL came up and could issue
  it while only the PS-to-PL shifters were on: the read never reached
  the PS and the core waited forever (a white screen, some power-ons
  only). A JTAG look at the hung board showed the cartridge correct in
  DDR and the PS link's hold off, so the fault was on the PL side.
  Layer 4: `arty-sim --ps-reset N` holds `PS_RESETN` and fails on any
  HP0 read meanwhile (in `ci-arty`). The ZedBoard's `gb_zynq.v` has the
  same race (side track I's file).

## Sound (side track G phase 2, step 6)

Mono on the Arty's audio jack: `aud_pwm` (R18) into the board's
low-pass filter and amplifier, `aud_sd` (T17) high to enable it
(`boards/arty-z7-20/audio.xdc`). Both are plain push-pull outputs, as in
Digilent's out-of-box design for the board (`Digilent/Arty-Z7-20-OOB`,
`Audio_Stream_PWM`). `Platform.Audio.Pwm` is a first-order delta-sigma
modulator at the pixel clock: a signed 16-bit sample in, one bit per
clock out, whose density of ones follows the sample within one bit;
silence is a steady 50 % pattern.

| Top | Sample |
|---|---|
| `Chip8.ArtyTop` | ±6000 square, the 440 Hz beeper, while the sound timer runs |
| `Gb.ArtyTop` | (L + R) × 32 of the APU (Gb.Top's S/PDIF has each × 64) |
| `Nes.ArtyTop` | `Nes.Top`'s mixer and filters (`audioFilter`), mono |

Layer 4: `arty-sim --expect-tone HZ` averages `aud_pwm` over 524 pixel
clocks (about 48 kHz) and measures the tone; `ci-arty` checks Timendus's
`7-beep` (440.0 Hz, key B held), `tone.gb` (439.8 Hz) and `tone.nes`
(440.4 Hz), the pitches `ci-gb` and `ci-nes` hear over the ZedBoard's
S/PDIF. HDMI audio on the Arty (TMDS data islands, our own) is later.

Board-checked 2026-10-06 (the user: all three systems). Headphones hear
it on the **left ear only**: one pin, so the jack's wiring decides (most
likely only its tip carries the signal; not checked against the
schematic). A mono adapter or a speaker gives both ears.

## HDMI audio (side track G phase 3)

The Arty's CHIP-8, Game Boy and NES send HDMI, not DVI: data islands in
every line's horizontal blanking carry the sound (M0b stays DVI).

- `Platform.Video.HdmiPacket`: TERC4, guard bands, the packets' BCH ECC
  (generator 1 + x^6 + x^7 + x^8), Null, ACR, Audio Sample (2-ch L-PCM,
  IEC 60958 bits as the ZedBoard's S/PDIF), AVI InfoFrame (RGB, VIC 1,
  4:3), Audio InfoFrame. From HDMI 1.4b 5.2 / CEA-861 as open
  descriptions give them.
- `Platform.Video.Hdmi`: per line, the island preamble at 652, guard
  bands, packet A (the samples since the last, up to 4; 48 kHz is 1.53
  a line) at 662, packet B (AVI InfoFrame on line 0, Audio InfoFrame on
  line 1, ACR otherwise) at 694, then before an active line the video
  preamble (790) and guard band (798).
- **Rate:** N = 6144, CTS = 25174 (`Platform.Board.ArtyZ7.artyCts`, from
  the Arty's 25.17361 MHz); the 48 kHz strobe adds N per pixel and fires
  at 128 x CTS, so the sink's rebuilt rate is exactly the samples' rate
  (47 999.2 Hz).
- **Samples:** CHIP-8 the beep on both channels; Game Boy stereo, each
  channel x 64; NES the mix on both. The PWM jack still plays the mono
  mix.
- **Layer 4:** the TMDS sink decodes the islands (ECC, checksums,
  parity, guard bands) and collects the samples; `arty-sim
  --expect-hdmi-tone`. `ci-arty`: every picture still identical, the
  three tones in the HDMI audio as on the PWM pin.

