# CHIP-8

Target: the **original COSMAC VIP CHIP-8**, with switch-selectable quirks for
the common later interpretations (CHIP-48/SUPER-CHIP behaviour). SUPER-CHIP's
128×64 mode and XO-CHIP are out of scope for milestone 1. They are possible
later extensions.

## Machine summary

| Item | Value |
|---|---|
| RAM | 4 KiB; programs load at `0x200`; font at `0x050` to `0x09F` |
| Registers | V0 to VF (8-bit; VF is the flag), I (12/16-bit), PC (12-bit), SP |
| Stack | 16 × 12-bit return addresses |
| Timers | delay and sound, 8-bit, decremented at 60 Hz |
| Display | 64×32 × 1 bit; sprites are XORed; VF = collision |
| Input | 16-key hex keypad |
| Opcodes | 35, all 16-bit big-endian |

References: Cowgod's technical reference, Tobias V. Langhoff's "Guide to
making a CHIP-8 emulator", and the Timendus test suite's quirks
documentation.

## Switch allocation

All eight switches are allocated. None is left for input, so keypad
shifting uses a button (BTNC, confirmed with the user) instead (see
"Input").

Switch roles are defined per top-level design, not by the board or the
platform: this table applies to the CHIP-8 top only. The milestone 0
bring-up tops reuse the same switches differently (for example, in the
M0a top SW7 selects whether the LEDs show the buttons or the switches;
see PLAN.md, milestone 0 step 5a).

| Switch | Function |
|---|---|
| SW0 to SW4 | quirk bits 0 to 4 (`ciConfig[4:0]`, table below) |
| SW5 | instruction speed: off = slow (~700 Hz, games), on = fast (~10 kHz, test ROMs) |
| SW6 | debug-byte select: which byte of the 32-bit `coDebug` word is shown on LD0 to LD7 (off → bits 7..0, on → bits 15..8) |
| SW7 | OLED mode: off = stretch (2×1), on = 1:1 (see "Display path") |

SW7 used to be the high debug-byte select bit. The user agreed to give it
up for the OLED mode "for now", so bytes 2 and 3 of `coDebug` (bits 31..16)
have no LED view. Put the most useful debug fields in the low 16 bits; the
free half of the OLED in 1:1 mode can show the whole word later.

`ciConfig[5]` carries the speed switch; `ciConfig[7:6]` are reserved (the
debug-byte select and the OLED mode are handled by the platform, not the
core).

## Quirks (`ciConfig` bits, set by SW0 to SW4)

| Bit | Quirk | VIP (default = 0) | SCHIP (= 1) |
|---|---|---|---|
| 0 | `8xy1/2/3` reset VF | VF ← 0 | VF unchanged |
| 1 | `Fx55/Fx65` increment I | I ← I + x + 1 | I unchanged |
| 2 | `8xy6/8xyE` shift source | shift Vy | shift Vx |
| 3 | `Bnnn` jump | V0 + nnn | Vx + nnn (`Bxnn`) |
| 4 | `Dxyn` waits for vblank | wait | no wait |

Sprite clipping (instead of wrapping) at screen edges follows the VIP
behaviour in all modes for now.

## Hardware design

### Clocking

Everything runs in `DomPix` (25.175 MHz). Generated enables:

- **`instrTick`:** one instruction per tick. The rate is configurable
  (default ~700 Hz; SW5 chooses "fast" at ~10 kHz for the test ROMs).
  The core is multi-cycle internally, but waits for `instrTick` before
  fetching.
- **`timerTick`:** 60 Hz, taken from the VGA vertical blank start. This also
  provides the vblank wait for the display-wait quirk.

### Memories

| Memory | Size | Implementation |
|---|---|---|
| Main RAM (font + program) | 4096 × 8 | true dual-port `blockRam`. Port A for the CPU; port B for the loader. Initialised from a `.hex` file in loader stage A |
| Framebuffer | 2048 × 1, organised as 32 rows × 64 bits | dual-port BRAM (or 32 × 64-bit registers if that's easier for clear/draw). CPU read-modify-write on one port; scaler reads on the other |
| Stack | 16 × 12 | registers / LUTRAM |
| Registers | 16 × 8 | registers (a `Vec 16 (BitVector 8)` in the state record) |

### Core state machine (sketch)

```
Fetch1 → Fetch2 → Decode/Execute ─┬→ (single-cycle ops) → WaitTick → Fetch1
                                  ├→ Draw: for row in 0..n-1:
                                  │      ReadSprite → ReadFB → WriteFB (XOR, collision)
                                  ├→ Clear: 32 row writes
                                  ├→ BCD: 3 RAM writes
                                  ├→ Store/Load regs: x+1 RAM accesses
                                  └→ WaitKey: until press-and-release (VIP behaviour)
```

RAM reads take one cycle (BRAM), so states that read memory are split into
"issue address" and "use data" steps. Every instruction finishes within a few
hundred cycles, well inside one `instrTick` period.

### Display path

The scaler maps VGA (x, y) in the 640×320 active window (rows 80 to 399) to
CHIP-8 pixel (x / 10, (y − 80) / 10). Division by 10 is avoided by using
counters that wrap every 10 pixels and lines. The row word is fetched one
pixel early to hide BRAM latency. Foreground and background colours are
platform parameters (default: amber on black).

**HDMI** (PLAN.md M0d step 6) takes the same stream as VGA
(`Chip8.Display.chip8Stream`, before the VGA pin register): converted to
4:2:2 by `Platform.Video.Ycbcr422`, sent to the ADV7511 by
`Platform.Board.Zedboard`, with the chip set up over I2C by
`Platform.Video.Adv7511`. The CHIP-8 LEDs keep showing `coDebug`; to
diagnose HDMI, use the M0d top. Layer 4 compares the HDMI output of every
CI ROM run with `test/golden/hdmi/<rom>.png`, made by `chip8-golden` from
the VGA golden through the ADV7511 model.

The same framebuffer also goes to the onboard 128×32 OLED through
`Platform.Video.Oled` (PLAN.md M0c), the display used on the board until a
VGA cable is available. Two modes, selected by SW7 (user's choice):

- **SW7 off, stretch:** each CHIP-8 pixel is 2×1 OLED pixels, filling the
  panel (image twice as wide as on a monitor).
- **SW7 on, 1:1:** 64×32 in one half of the panel, correct aspect ratio.
  The other half is free for later debug output.

### Input

- **Stage 1:** the 5 buttons map to a subset of keys. No switch is free,
  so BTNC is the shift (confirmed): BTNU/D/L/R alone give four keys, and with BTNC
  held four more. Table (`Chip8.Peripherals.buttonKeys`, step 1.5;
  accepted by the user "for now", 2026-10-02):

  | Button | alone | with BTNC |
  |---|---|---|
  | BTNU | 2 | E |
  | BTND | 8 | F |
  | BTNL | 4 | A |
  | BTNR | 6 | B |

  2/4/6/8 are the usual directions; E/F/A move and select in the
  Timendus menus (5-quirks, 6-keypad) and B is 7-beep's key. BTNC alone is
  no key. Buttons are synchronised and debounced (10 ms on the board).
  Enough for the test ROMs; games that need other keys wait for stage 2.
- **Stage 2:** a Pmod KYPD (4×4 matrix) scanned by a `Platform.Input.Keypad`
  module. Its layout matches the COSMAC hex keypad.
- **Desktop keyboards and gamepads (no extra hardware), confirmed on the
  board:** `make keys` runs the board link, `build/host/link` (Zig,
  `platform/host/`). It reads every keyboard and gamepad on the PC straight
  from the kernel (evdev; no window or focus needed, so keys count
  wherever you type), and sends the key state over the USB-UART (J14) the
  moment it changes, as a 3-byte binary frame, plus a keepalive every
  100 ms. The relay app on the PS (`platform/ps/relay.zig`, bare metal,
  loaded by `make prog`) puts it on EMIO GPIO 0 to 15, which `chip8_top`
  reads through `PS_GPIO` (synchronised, ORed with the buttons). The relay
  releases all keys if updates stop for 0.5 s. Default mapping
  (`platform/ps/keymap.zig`; since side track B a data table that
  `make keymap SYSTEM=chip8` or the key-map file changes):
  - keyboard: `1234/QWER/ASDF/ZXCV` → `123C/456D/789E/A0BF`, plus arrows =
    2/8/4/6 and space = 5;
  - gamepad (e.g. a wired PS4 pad): D-pad or left stick 2/8/4/6, Cross 5,
    Circle B, Square C, Triangle 1, L1 E, R1 F, Options A, Share 0. (The
    user's PS4 pad wasn't recognised on their PC yet; they're looking into
    it. The path is tested with a virtual gamepad.)
- **Loading a ROM without a rebuild, confirmed on the board:**
  `make load ROM=… [RAM_PRESET=0x1FF=1]` holds the design, sends the ROM
  to 0x200 padded with zeros to the end of RAM, writes the presets, checks
  every block's checksum, releases, then carries on as `make keys`. On the
  PL side, EMIO GPIO 16 to 26 carry a data byte, strobe, address flag and
  hold (the shared PS link, `Platform.Ps.Link`; CHIP-8's address map in
  `Chip8.Loader`); hold is the core's soft reset (`ciReset`: power-on
  state, framebuffer cleared, RAM kept). The ROM baked into the bitstream
  only runs until the first load. A load from the desktop also closes the
  menu (below).
- **ROM menu from the SD card (M1b stage C; board-checked 2026-10-03):** after
  `make prog SYSTEM=chip8` the monitor shows a menu of the ROMs in the
  card's `/chip8` folder (any case; subfolders too; files ending in
  `.ch8` or `.c8`; without that folder, the root). BTNU/BTND move (hold to
  repeat), BTNR or BTNC opens a folder or loads the ROM and starts it,
  BTNL goes up a folder, or from the top back to the game. In a game,
  hold BTNC alone for one second to open the menu again (the game is
  paused under it, keys don't reach it; L at the top resumes it). The
  card is read again every time the menu opens. The card must be FAT12,
  FAT16 or FAT32 (cards over 32 GB come as exFAT: reformat as FAT32).
  Everything is in `docs/ARCHITECTURE.md`, "PS link and ROM menu".
- **Filling the card without a card reader:** `make sd [SD_ROMS=roms/chip8]`
  (board running after `make prog`, J14 on USB). **It erases the card.**
  `tools/mksd.sh` makes a 64 MiB image (MBR, FAT32, the ROM folder in
  `/chip8`), `link --sd-image` sends it through the relay, which writes
  every block and reads it back; then the menu reopens with the new list.
  Only the image's start goes over the wire (runs of zeros as one command),
  so it takes seconds to a minute. Any PC reads the card afterwards.
- **Link speed:** the relay supports 115200, 921600, 460800 and 230400 baud
  (`B0` to `B3`, falls back to 115200 by itself after 0.6 s without valid
  input); `link` probes the fast ones and caches the result in
  `build/host/link-baud`. On the user's board the bridge
  ("Cypress-USB2UART-Ver1.0G") does **230400** but not 460800 or 921600.
  Measured round trip PC ↔ PS (`make ping`, 200 pings of 2 + 3 bytes):
  411/429/538 µs min/median/max at 230400 (614/640/763 µs at 115200). A
  key update is 3 bytes one way: about 130 µs on the wire plus USB
  (up to ~1 ms per transfer). On the PC, input event → serial write takes
  50 to 70 µs (`tools/test_link.py`, virtual gamepad through uinput); the
  PS adds microseconds and the PL 80 ns. What's left is mostly the game's
  own key polling (often once per 60 Hz frame).
- **Older tool:** `tools/keys.py` (Python, tkinter window) does the same
  over text lines at 115200. It needs window focus and delays every
  release by 40 ms (X11 autorepeat filter); kept as a fallback for a PC
  without access to `/dev/input`.
- **Simulation:** `make play` drives the board's buttons and switches
  (arrows, space, F1 to F8), not `PS_GPIO`.

### Sound

The sound timer being non-zero drives a ~440 Hz square wave on a Pmod pin. It
moves to the ADAU1761 once the shared audio path exists.

## Test plan

| Test | Layer | Source |
|---|---|---|
| Opcode unit tests (each opcode, each quirk) | 0, 1 | hand-written |
| Decoder round-trip and totality | 1 | hedgehog |
| Random-program lockstep (core vs. spec) | 2 | hedgehog generator |
| `1-chip8-logo`, `2-ibm-logo`, `3-corax+`, `4-flags`, `5-quirks`, `6-keypad` | 0, 2, 3, 4, 6 | Timendus chip8-test-suite (fetched by `tools/fetch-roms.sh`) |
| `7-beep`: holding key B sounds the buzzer | 0, 3 (sound timer / sound-active output while the key is held), 6 (user listens; needs the Pmod beeper) | Timendus chip8-test-suite |
| VGA frame golden images | 4 | generated, checked by eye once |
| Playable game | 2, 6 | CC0 games from the CHIP-8 Archive whose metadata asks for the VIP quirks: Dino Run (lockstep-tested; **plays on the board**, user 2026-10-02), WDL, Knumber Knower (`tools/fetch-roms.sh` → `roms/chip8/games/`). David Winter's Space Invaders (freeware) is fetched too, but the user found it glitchy |

`8-scrolling` is not on the list: it tests SUPER-CHIP/XO-CHIP scrolling,
which is out of scope.

Expected test ROM screens are documented in the Timendus repository's README.
The golden PNGs are checked against those once and then frozen.

## Resource estimate

Rough guess, to be replaced with real numbers after milestone 1.7: well
under 2k LUTs, 2 to 3 RAMB18s, and no DSPs. Under 5% of the XC7Z020.

## Status log

*(fill in per completed step: date, commit, test results, WNS, utilisation)*

### 2026-10-02: step 1.1, reference model (`Chip8.Spec`)

`systems/chip8/src/Chip8/Spec.hs`, tested by `cabal test chip8-spec`
(43 tests: every opcode, every quirk, LFSR period, and the Timendus v4.2
ROMs 1 to 7). The ROM screens are frozen as 64×32 goldens in
`systems/chip8/test/golden/spec/` after checking them by eye against the
suite's README pictures: corax+, flags and quirks (CHIP-8 selected via
0x1FF = 1) are identical to the published screens, all checkmarks.
`6-keypad` passes Ex9E (keys 1 and 6 held) and Fx0A ("ALL GOOD"), and
`7-beep` sounds while B is held. The ROM tests run 200 instructions per
frame with VIP quirks.

Decisions the hardware core must copy (lockstep tests compare state):

- `Cxnn` random numbers: a 16-bit Galois LFSR (taps 0xB400, seed 0xACE1),
  stepped once per `Cxnn`; the byte is its low 8 bits.
- Display wait (quirk bit 4 = 0): `Dxyn` waits for the *next* 60 Hz tick
  (the first execution only starts waiting, the tick releases it, the next
  step draws). With it, every sprite costs a frame, so screens with many
  sprites take a few hundred frames.
- `Fx0A`: waits for a press (lowest key if several), then for that key's
  release, then stores it. Timers keep running.
- Illegal opcodes (`0nnn` other than `00E0`/`00EE`, `5xyN`/`9xyN` with
  N ≠ 0, `8xy8` to `8xyD`, `8xyF`, unknown `Ex`/`Fx` forms), stack
  overflow (more than 16 calls) and underflow halt the machine for good.
  The hardware shows this on the error latch.
- RAM accesses wrap at 4 KiB; I is 16 bits; `Fx1E` never sets VF.
- ALU ops write the result first and VF last, computed from the old
  operands, so with x = F the flag wins.

### 2026-10-02: step 1.3, hardware core (`Chip8.Core`)

Multi-cycle core with all state in `CoreState` and a pure `coreStep`.
Main RAM is one `blockRam` (power-on contents `fontRam`, programs through
the loader port `ciRomWrite`). The framebuffer is 32 × 64-bit `blockRam`
rows, kept twice with identical writes (core read-modify-write, video
read port `ciFbAddr`/`coFbRow`). Instruction and timer ticks are latched
and handled only between instructions (timer first), and every
instruction or tick shows up on `coEvent` for the lockstep test.

`cabal test chip8-lockstep` (layer 2): 300 random programs (hedgehog;
random quirk bits, tick periods and key schedules, 2500 cycles each) and
the Timendus ROMs 1 to 7, every instruction compared with `Chip8.Spec`
and the final framebuffer compared through the video port. The ROMs'
final screens also match the layer 0 goldens. Three mutations of the core
(8xy5 flag, draw row off by one, Fx55 I increment) were all caught by the
random programs and by several ROMs.

This test found that the layer 0 golden for `1-chip8-logo` was taken too
early: 10 frames aren't enough with the display wait, so "CHIP-8" was
missing its last strokes. It was missed in the by-eye check and has been
regenerated from 60 frames ("Timendus' CHIP-8 v4.2 Test suite").

### 2026-10-02: step 1.4, display path (`Chip8.Display`)

VGA: `vgaChip8` scales the framebuffer ×10 into rows 80 to 399 (amber
`F,B,0` on black), dividing by 10 with a multiply and shift (`div10`,
exact over the frame). OLED: `oledAdapter` assembles each SSD1306 byte
from 8 framebuffer rows (9 cycles; the driver gives it 64), for the two
SW7 modes (stretch, 1:1). The core now keeps a third framebuffer copy, so
VGA and OLED each have their own read port.

`cabal test chip8-golden` (layer 3): the scaler and the adapter on a test
framebuffer, then core + display running `2-ibm-logo`. The VGA frame,
decoded from the pins, is exactly the layer 0 golden scaled up and
letterboxed, and is frozen as `test/golden/vga/2-ibm-logo.png` (checked by
eye) for layer 4. The OLED panel model shows the same golden in both
modes. An adapter bit-order mutation was caught.

### 2026-10-02: step 1.5, input, ticks, sound (`Chip8.Peripherals`)

`buttonsToKeys` (sync, debounce, the stage 1 table above), `instrTicker`
(35966 clocks = 700 Hz, or 2518 = 10 kHz with SW5), `beeper` (440 Hz
square while the sound timer runs). The timer tick is the VGA vertical
blank (59.94 Hz). Tests: `chip8-unit` checks the key table exhaustively
and the periods; `chip8-golden` runs the core through the whole button
path: 6-keypad Fx0A with BTNU (key 2) pressed and released reaches the
"ALL GOOD" golden, and in 7-beep the buzzer stays on while BTNC + BTNR
(key B) are held, with gaps after release and without the buttons. Stage
2 (Pmod KYPD) isn't started: it needs the keypad.

### 2026-10-02: step 1.6, top-level and generated-Verilog tests (`Chip8.Top`)

`Chip8.Top.topEntity` (`chip8_top`): core, VGA, OLED, LEDs (debug byte by
SW6), beeper on Pmod JA1 (`boards/zedboard/pmod_ja1.xdc`). Main RAM is a
`blockRamFile` whose image `make hdl SYSTEM=chip8 ROM=… [RAM_PRESET=…]`
writes first (`chip8-ram`, `app/RamImage.hs`: font + ROM + presets).
Clash copies it next to the Verilog (`ram.bin`, loaded by `$readmemb`);
`make sim` runs the harness from that directory and `build.tcl` copies it
into Vivado's working directory. `make hdl` uses `-fclash-no-cache
-fclash-clear`, because Clash's output cache doesn't notice a new RAM
image.

Layer 4: `make sim SYSTEM=chip8 ROM=<rom> FRAMES=<n>` (with SW5 on, 10 kHz)
compares the last frame with `test/golden/vga/<rom>.png`. The goldens are
the layer 0 screens scaled ×10 and letterboxed (layer 3 proved the
hardware does exactly that; 2-ibm-logo's is the Clash simulation's own
frame). From the generated Verilog, `1-chip8-logo` (70 frames),
`2-ibm-logo` (30), `3-corax+` (200), `4-flags` (200) and `5-quirks` with
0x1FF = 1 (400) are all identical to their goldens; `make ci` runs them.
`make lint SYSTEM=chip8` is clean (one more Clash idiom waived:
`$unsigned` widening to Int).

`make play SYSTEM=chip8 ROM=…` opens an SDL3 window on the simulated VGA
output (arrows + space = buttons, F1 to F8 = switches), at about 6
frames/s, not real time. It drives the
board's buttons, not the hex keypad: the keys reach the core through the
same button table as on the board.

### 2026-10-02: step 1.7, Vivado build (board check pending)

`make hdl SYSTEM=chip8 ROM=roms/chip8/timendus/3-corax+.ch8 && make bit
SYSTEM=chip8`: WNS +16.776 ns, WHS +0.122 ns, 0 critical warnings;
2786 LUTs (5.2 %), 741 FFs, 66 F7 muxes, 4 BRAM tiles, 2 DSPs (over the
"under 2k LUTs" guess, still small). Vivado reports `$readmem data file
'ram.bin' is read successfully`. All pins in `io.rpt` as in the XDC files.
Bitstream SHA-256 prefix 4ea443d59aacddfd. **On the board (user check,
2026-10-02): works as intended** (corax+ on the OLED). The other test ROMs
haven't been run on the board yet.

### 2026-10-02: step 1.8, quirk switches (simulation side)

`ciConfig` is the switch byte, so SW0 to SW4 select the quirks directly.
With 5-quirks' CHIP-8 choice (0x1FF = 1) and all five switches on, the
ROM reports VF reset, memory and display wait off and clipping, shifting
and jumping on, with crosses where that differs from CHIP-8 (layer 0
golden `5-quirks-chip8-sw1f`, checked by eye; layer 4 with `--sw 0x3F`,
identical; both in `make ci`). With the switches off it reports the CHIP-8
set with all ticks. The SUPER-CHIP menu choices need hires mode, which is
out of scope. The board check is pending.

### 2026-10-02: milestone 1 done

Exit criteria met (user's board checks): the Timendus ROMs 1 to 7 run on
the board (corax+ baked into the bitstream; the others through
`make load`; 5-quirks shows the CHIP-8 set with all ticks; 6-keypad
reacts to the desktop keyboard), and Dino Run (CC0) is playable. Not
covered on hardware: VGA output (no cable yet; Verilog simulation
reproduces every golden), and 7-beep's buzzer (needs something on JA1;
the sound path is tested in simulation). Final build (`chip8_zynq`, IBM
logo baked): WNS +17.944 ns, WHS +0.122 ns, 2954 LUTs (5.6 %), 4 BRAM
tiles, 2 DSPs, 1 PS7.

### 2026-10-02: on HDMI (M0d step 6)

`chip8_top` drives HDMI from the same pixel stream as VGA. Board check
(user): the IBM logo and Timendus corax+ show correctly on the monitor.
Build (`chip8_zynq`, IBM logo baked): WNS +17.302 ns, WHS +0.085 ns,
3509 LUTs (6.6 %), 4 BRAM tiles. Layer 4 compares every CI ROM's HDMI
frame with `test/golden/hdmi/`.
