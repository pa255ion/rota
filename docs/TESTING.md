# Testing flow

Hardware is slow to build (minutes per bitstream) and hard to observe, so as
much verification as possible happens in Haskell, where a test runs in
milliseconds. Each layer below catches a class of bug the layer above it
can't. The rule: **if a bug is found at layer N, add a test at the lowest
layer that can reproduce it.**

```
             ┌───────────────┐
             │ 6. Hardware   │  minutes/iteration · eyes, LEDs, ILA
            ┌┴───────────────┴┐
            │ 5. Vivado       │  timing, utilisation, DRC
           ┌┴─────────────────┴┐
           │ 4. Verilator      │  generated Verilog, real VGA timing, PNG goldens, SDL play mode
          ┌┴───────────────────┴┐
          │ 3. Core simulation  │  Clash `simulate`, golden framebuffers, test ROMs
         ┌┴─────────────────────┴┐
         │ 2. Differential       │  hardware core vs. reference spec, in lockstep
        ┌┴───────────────────────┴┐
        │ 1. Unit / property      │  decoder, ALU, sprite draw, timing generator
       ┌┴─────────────────────────┴┐
       │ 0. Reference spec          │  pure Haskell interpreter passes the test ROMs
       └────────────────────────────┘
```

## Layer 0: Reference spec

`<Name>.Spec` is a plain Haskell, instruction-level model of the machine:

```haskell
step :: Quirks -> Keys -> Machine -> Machine
```

It is written for clarity, not hardware: `Data.Vector` for RAM, no clock
cycles. Run the system's test ROMs on it first, and check its screen output
against the published expected screenshots. Once it passes, the spec is the
oracle for everything above it. Test ROMs live in the git-ignored `roms/<system>/` (for CHIP-8,
`roms/chip8/timendus/`), fetched and hash-checked by `tools/fetch-roms.sh`,
which `make test` runs first. Game Boy: `roms/gb/` (Blargg, Mooneye,
dmg-acid2); our own Game Boy regression ROMs are assembled from
`systems/gb/test/asm/*.asm` into `build/gb/asm/` by `make gb-asm` (rgbds), which
`make test` also runs first, and read by the tests from there.

**Run:** `cabal test chip8-spec`, `cabal test gb-spec` (Game Boy: the spec
on Blargg and Mooneye ROMs; `cabal run gb-run -- blargg|mooneye <rom.gb>`
runs one ROM and prints its serial output and verdict)

NES and the shared 6502 (M3): `roms/6502/` holds Tom Harte's
SingleStepTests (one JSON file per opcode, 10 000 cases each, with every
cycle's bus access; the whole 2A03 set and the decimal-mode opcodes of
the NMOS set; SHA-256s in `tools/roms-6502.sha256`), `roms/nes/` the NES
suites (nestest and its log, Blargg, AccuracyCoin). Our own NES regression
ROMs are assembled from `systems/nes/test/asm/*.s` into `build/nes/asm/`
by `make nes-asm` (ca65/ld65), part of `make test`. `make test SYSTEM=nes`
also runs the `mos6502` package's tests.

**Run:** `cabal test mos6502-spec` (`Mos6502.Spec` against SingleStepTests,
final state and per-cycle bus trace, plus interrupt, RDY and reset cases;
about 45 s), `cabal test mos6502-lockstep` (the Clash 6502 core on
SingleStepTests and in lockstep with the spec, decoder, broken cores
caught; layers 1 and 2, about 50 s), `cabal test nes-golden` (layer 3: the NES VGA and HDMI goldens from the
spec screens, and the display path in Clash simulation), `cabal test nes-lockstep` (the Clash PPU in lockstep with the
spec's, dot by dot, on PPU ROMs and random programs; the whole core
(`Nes.Core`) against the spec step by step, and test ROMs to the end on
the core; about 1.5 min in the quick tier, 13 min with FULL=1;
`NES_CORE_ROMS=all` runs every test ROM and AccuracyCoin on the core,
about 40 min),
`cabal test nes-spec` (the NES spec on nestest and Blargg's
ROMs; (`cabal run nes-run -- [--trace K] <rom.nes>` runs one ROM and prints
Blargg's result, through $6000 or the older shell's $4016 serial line;
`--screen` for the 2005 sprite tests, `--png N out.png` writes frame N)

**The shared Z80 (M5, `cpus/z80`).** `roms/z80/v1/` holds Tom Harte's
SingleStepTests for the Z80 (1604 JSON files, one per opcode with its
prefixes, 1000 cases each, ~1.3 GB; SHA-256s in `tools/roms-z80.sha256`).
A case lists the pins of every T-state; the harness
(`cpus/z80/test/Z80/SingleStep.hs`) collapses them into memory and I/O
accesses and compares those in order, the total T-states, every register
(MEMPTR, Q, the EI and LD A,I/R flags included) and the RAM.
`make test` with no SYSTEM runs it (`cabal test all`), and so will
`make test SYSTEM=gg` once `systems/gg` exists.

**Run:** `cabal test z80-spec` (`Z80.Spec` against SingleStepTests, the
first 100 cases per file in the quick tier, about 70 s, all 1000 with
FULL=1, about 12 min; plus interrupt cases: IM 0/1/2, NMI and RETN, EI's
delay, DI, HALT); `cabal test z80-zex` (ZEXDOC and ZEXALL, the CP/M
originals in `roms/z80/zex/`, under a BDOS stub: the first test of each
in the quick tier, about 20 s, both to the end with FULL=1, about 4.5
min).

`cabal test z80-lockstep` (the Clash Z80, `Z80.Core`: SingleStepTests on
the core, same tiers as `z80-spec`; lockstep with `Z80.Spec` on random
programs with random /INT and NMI, every bus cycle, the registers and the
T-states after each instruction (60 programs × 3000 instructions, 400
with FULL=1), with a coverage count of interrupts and halts; the WAIT
wrapper; two broken cores must be caught; about 70 s). `make z80-hdl`:
Clash turns the core (its T-state wrapper, `cpus/z80/hdl/Z80Top.hs`) into
Verilog and Verilator lints it.

**The Game Gear (M5, `systems/gg`).** Layer 4 (step 5.8): `make
ci-arty-gg` (lint, `pattern.s` against `systems/gg/test/golden/vga/pattern.png`,
made from the spec with `gg-run ROM --vga-at K FILE`, and `tone.s` at
440.4 Hz in HDMI's audio and on PWM) and `make ci-gg` (the ZedBoard: the
same golden, the ADV7511 init against the Game Boy's I2C golden, the tone
on S/PDIF); both are part of `make ci`. The ROM is in the harness's DDR
(`--ddr-file 0x02000000=ROM`; `gg-image` writes the cartridge word), as
for the Game Boy. `raster.s`'s picture (systems/gg/test/golden/vga/raster.png)
checks the timing end to end with the ROM in DDR, also at
`--axi-latency 60` (step 5.9). `cabal test gg-lockstep` (step
5.6): the Clash VDP (`Gg.Vdp`) against the spec's, replaying the same
port accesses dot by dot (random programs in GG and SMS mode, the
machine's logs of our ROMs and of Waternet's first 120 frames) and
comparing every port read, /INT at every dot and every frame; the Clash
PSG (`Gg.Psg`) on random write/clock programs; a broken VDP and a broken
PSG must be caught; and the whole Clash machine (`Gg.Core`, step 5.7)
against `Gg.Spec`: the registers and T-states after every instruction,
every frame and every PSG write, on our ROMs, Waternet's first 40 frames
and FluBBa's VDP test in SMS mode, with a broken machine caught (about
3 min in all). `make gg-core-hdl`: the whole machine through Clash
(about 10 min) and Verilator. `make gg-vdp-hdl`: the VDP with four
block RAMs (`systems/gg/hdl/VdpTop.hs`) through Clash and Verilator.
 Our own regression ROMs are
assembled from `systems/gg/test/asm/*.s` into `build/gg/asm/*.gg` by
`make gg-asm` (WLA-DX), part of `make test`; each ends with DI / HALT
and a verdict at C000h (01h pass, 80h + n: check n failed; `gg.inc`).
**Run:** `cabal test gg-spec` (`Gg.Spec` on those ROMs, `codemasters.s`
with the Codemasters mapper, the rest with Sega's; mapper detection
tests; unit tests of the VDP's drawing rules and the PSG; the Waternet golden: frame hashes of
frames checked by eye, played with `systems/gg/test/input/waternet-start.txt`
and frozen in `systems/gg/test/golden/waternet.txt`; FluBBa's SMS VDP
Test (`roms/gg/vdptest/`, Master System mode, driven by
`systems/gg/test/input/vdptest.txt`): its three result pages read from
the name table must show all 55 results "Ok." and its X-scroll latch
column must be straight; about 12 s). The
simulator `cabal run gg-run -- ROM ...` (docs/systems/gamegear.md,
"Simulator") writes PNG frames, a WAV file and frame hashes.

## Layer 1: Unit and property tests (`tasty` + `hedgehog`)

Target the pure, combinational pieces of the Clash design:

- decoder: `decode (encode op) == op` for all ops; `decode` is total over
  all 2^16 words
- ALU and flags: each operation compared against the spec's version for
  random inputs
- sprite XOR and collision, and BCD conversion
- platform: the VGA timing generator produces exactly 800×525 clocks per
  frame, with sync pulses at the right positions; the debouncer ignores
  glitches shorter than N cycles

Because Clash functions are ordinary Haskell, these tests call them
directly, with no simulation needed.

**Run:** `cabal test chip8-unit platform-unit gb-unit` (`gb-unit`: the SM83
decoder, exhaustively and against `Gb.Spec`)

## Layer 2: Differential (lockstep) testing

The Clash core is simulated with `simulate`/`sampleN` and exposes a
"retired-instruction" debug strobe that carries the architectural state
(PC, I, V0 to VF, SP, timers). The test harness:

1. loads the same program into the spec and the core
2. each time the core retires an instruction, steps the spec once
3. compares state, and on the first mismatch fails with the instruction,
   both states, and the cycle number

Inputs come from:

- **random programs:** hedgehog generators of valid opcode sequences.
  Shrinking reduces a failure to a minimal program.
- **the test ROMs**
- **regression programs** in `test/regressions/`

This is the most valuable layer. It finds almost all core bugs, cheaply.

**Run:** `cabal test chip8-lockstep`, `cabal test gb-lockstep` (Game Boy: the
Clash CPU `Gb.Cpu` driven on a reference-model `Machine`, compared with the
spec CPU per step: every M-cycle's bus access and the registers; random
programs and test ROMs, then the core alone on Blargg's CPU ROMs; and the
Clash PPU `Gb.Ppu` next to the spec PPU through the spec machine's test
hooks, compared every M-cycle and every pixel). The core's `coEvent` output reports
every retired instruction (with the keys it saw) and every timer tick, so
the harness replays exactly the same sequence on the spec and needs no
cycle-level alignment.

## Layer 3: Core-level golden tests (Clash simulation)

Run a ROM on the core for N frames with a scripted key sequence, dump the
framebuffer to PNG (via `Platform.Sim.Frame`), and compare it against a
committed golden image in `test/golden/`. For VGA output, `Platform.Sim.Frame`
decodes the simulated pins into frames like a monitor (the same algorithm
as the layer 4 harness), so the goldens are shared with layer 4.
Example: `systems/m0b/test/golden/frame_0002.png`, from Clash simulation
of the M0b test pattern.

- Golden images for the test ROMs are **checked by eye once** against the
  test suite's documentation, then frozen.
- To update goldens: `cabal test chip8-golden --test-options=--accept`, then
  review the PNG diff before committing.

## Layer 4: Verilator (generated HDL)

`make sim` runs `make hdl` (`clash --verilog`), then Verilator builds the
whole `topEntity` (clock wizard, reset synchroniser, the platform's VGA
path, everything) together with the shared harness in `platform/sim/`.
The harness is written in Zig (CLAUDE.md rule 9). Verilator only generates
C++, so a thin C++ shim (`vtop.cpp`) exposes the model through a C ABI
(create, set inputs, tick, read outputs, destroy) and Zig does everything
else: VGA decoding (`vga.zig`), PNG (`png.zig`), the command line
(`main.zig`), and SDL3 for `make play` (`play.zig`). All C++ is compiled with `zig c++` so that it
links against the same libc++ as the Zig code. (Clash's own
`<top>_shim.cpp` is just a `main()` loop, not a C ABI, so it isn't used.)

- Verilator runs with `--prefix Vtop`, and every top with VGA has the same
  ports (the board XDC names), so one shim serves every system.
- The clock-wizard IP only exists inside Vivado, so the simulation uses a
  hand-written stand-in, `boards/sim/clk_wiz_sim.v.in` (the Makefile names
  it after the IP in the Clash output). It passes GCLK straight through:
  one harness tick is one pixel clock.

The harness has two modes:

- **headless** (`make sim`): a VGA sink decodes HSYNC/VSYNC/RGB into
  frames the way a monitor would, writes `build/<system>/frames/frame_NNNN.png`,
  and stops after `FRAMES` frames. It also checks the sync period and
  width on both axes and that the colour is black during blanking. If
  `systems/<system>/test/golden/` has PNGs, frame *k* is compared pixel
  for pixel with `frame_NNNN.png` there: the **same golden images** as
  layer 3, because the sink is the same algorithm as the Haskell decoder in
  `Platform.Sim.Frame`. Decoded frame 0 is the first frame after the first
  VSYNC pulse (for a generator that starts at pixel (0, 0) after reset,
  that's its frame 1). There is no cropping or downscaling: goldens are
  full 640×480 frames. For CHIP-8 they are made in Haskell from the
  64×32 layer 0 goldens (`systems/chip8/test/golden/spec/`), scaled ×10
  and letterboxed by the same code as the hardware display path, and
  `chip8-golden` keeps them in `systems/chip8/test/golden/vga/`; so a
  layer 4 match means the generated Verilog shows exactly what the spec
  computed. This catches Clash primitive or black-box problems, HDL-only
  reset behaviour, and VGA timing mistakes. `SIM_ARGS="--sw 0x81 --btn 1"`
  sets switches and buttons.
  `SIM_GOLDEN=<png>` compares the *last* frame instead (screens that only
  settle after a while, such as test ROM results).
- **HDMI** (tops with the HDMI ports, M0d onwards; the shim is built
  with `-DVTOP_HDMI` when the Clash output has `HD_D`, and Verilator with
  `--pins-inout-enables` for the open-drain I2C pins): the harness also
  plays the ADV7511. `i2c.zig` is its I2C slave on HD_SCL/HD_SDA (a port
  of `Platform.Sim.I2c`, power-on registers from `Platform.Sim.Adv7511`),
  and every register write goes to `frames/i2c_writes.txt`. `hdmi.zig`
  frames the 16-bit bus by VSYNC and DE, checks 640-pixel DE runs, 480
  lines per frame, black (0x8010) outside DE and HD_CLK = the inverted
  pixel clock, then turns each frame into RGB like the chip (4:2:2 →
  4:4:4 zero order, then the CSC with the coefficients the design wrote)
  and writes `frames/hdmi_NNNN.png`. `--expect-i2c FILE` and
  `--expect-hdmi-last FILE` compare them with goldens that `m0d-unit`
  makes in Haskell: the write list from the register table, and the M0b
  pattern through `Platform.Video.Ycbcr422` and `Platform.Sim.Adv7511`
  (the same integer CSC model as `hdmi.zig`). A compared frame must start
  after the last register write, or the run fails. `make sim SYSTEM=m0d`
  runs 18 frames (the set-up ends in frame 15). CHIP-8 does the same for
  every CI ROM run, against `systems/chip8/test/golden/hdmi/<rom>.png`
  (made by `chip8-golden` from the VGA golden) and M0d's I2C log; the
  Makefile variables are `SIM_HDMI_GOLDEN` and `SIM_I2C_GOLDEN`. The Game
  Boy has its own I2C golden (`systems/gb/test/golden/i2c_writes.txt`,
  from `gb-golden`: the HDMI-with-audio table).
- **HDMI audio** (tops with `HD_SPDIF`, the Game Boy from step 2.9; the
  shim is also built with `-DVTOP_SPDIF`): the shim samples HD_SPDIF
  after both clock edges (it leaves an ODDR), and `spdif.zig`, an S/PDIF
  receiver written from IEC 60958 independently of
  `Platform.Audio.Spdif`, decodes it: cells from run lengths, preambles,
  biphase-mark slots, parity, B every 192 frames, left before right.
  Any decoding error fails the run, as does a frame count that isn't
  48 kHz of simulated time; the samples go to `frames/audio.wav`.
  `--expect-tone HZ` checks the left channel's frequency (zero crossings,
  ±1 %); CI runs `tone.gb` with 439.8 Hz, and the NES's `tone.s` with
  440.4 Hz (M3 step 3.9). `--expect-quiet N`: in the second half of the run
  consecutive samples differ by at most N (no hum or aliased whine; a DC
  level settling passes); CI runs the NES's `tri_ultra.s` with 4.
- **Game Boy menu** (side track A): `make ci` also plays
  `systems/gb/test/ps/menu.ps` on `gb_top` against
  `systems/gb/test/golden/{vga,hdmi}/menu-overlay.png` (made by
  `gb-golden` through `Platform.Sim.Overlay` and the Game Boy's HDMI
  model) and checks `PS_IN` = 0x00020101 (BTNC, SW0, system ID 2);
  and runs `serial_pass.gb` with `--expect-ps-text Passed` (the harness
  decodes `PS_IN`'s byte channel like the relay, into
  `frames/ps_text.txt`). Battery saves (side track I): `ci-gb` runs our
  `save.gb` with `systems/gb/test/ps/save.ps` (a save written into
  cartridge RAM while held, then read back through the read window while
  the game runs) and compares the byte channel with `test/ps/save.hex`,
  `PS_IN` with 0x0F020040 (dirty flag) and the screen with the `save`
  goldens ("00042"). `make test-zig` builds a card image with save slots
  (`tools/mksd.sh` with `LINK` and a stored save), on which `save.zig`
  loads a save and writes it back in place (`fsck.fat` checks the result)
  and `saves.zig` lists slots and pulls `/saves`; `tools/test_link.py`
  pulls through the fake relay's R command.
- **Game Boy Color** (side track I, phase 3): `make sim SYSTEM=gbc` builds
  `gbc_top` (`Gb.TopCgb`, from `systems/gb/src`; `systems/gbc/` holds only
  its `zedboard.mk`). `ci-gbc` runs cgb-acid2 against
  `systems/gb/test/golden/{vga,hdmi}/cgb-acid2.png` (made by `gb-golden`
  from the spec's colour screen, `test/golden/spec/cgb-acid2.png`), lints
  it, and plays `sound_ds.gb` (sound in double speed) through the S/PDIF
  checks. The Haskell tests stay under `SYSTEM=gb`: `gb-lockstep` runs the
  core's CGB model against the spec's (Mooneye's CGB tests, cgb-acid2 with
  every colour pixel, our `speed.gb`, `hdma.gb`, `sound_ds.gb`, Blargg's
  `cgb_sound` 03, 08, 09, 11, 12).
  `platform/ps/load.zig`'s tests stream files from the FAT images into a
  model of the PL decoder (a 200 KB file at an offset; CHIP-8's clearing;
  refusals write nothing); `gb-lockstep` checks the menu's pause (dot
  enable low: frozen, then identical to an unpaused run).
- **NES menu and loading** (M3 step 3.8): `ci-nes` plays
  `systems/nes/test/ps/menu.ps` against `golden/{vga,hdmi}/menu-overlay.png`
  (from `nes-golden`) and checks `PS_IN` = 0x00030101 (system ID 3); then
  loads nestest over the link into the running build of `ppu_scene.s`
  (`tools/ps-load-script.sh` turns a file into `bytes` lines, as the PS
  app streams it) and compares the last frame with nestest's goldens:
  `Nes.Loader`, the header decoder and the restart in the generated
  Verilog.
- **PS link** (tops with `PS_IN`, M1b stage C; the shim is
  built with `-DVTOP_PS`): `--ps FILE` (`SIM_PS=` in make) plays a PS link
  script on `PS_GPIO` from clock 0 (`ps.zig`). The script is encoded by
  the PS app's own `platform/ps/pl.zig` with its 2 µs timing, so the real
  PS code drives the generated Verilog (ops: `text`, `inv`, `bar`, `ctrl`,
  `hold`, and `bytes ADDR HEX`, raw link writes such as a cartridge).
  `--expect-ps-in X` checks
  `PS_IN` (buttons, switches, system ID) at the end. CI plays
  `systems/chip8/test/ps/menu.ps` (a menu screen, overlay on) and compares
  the last VGA and HDMI frames with `golden/{vga,hdmi}/menu-overlay.png`,
  which `chip8-golden` renders from the same script through
  `Platform.Sim.Overlay`; `platform-unit` checks `Platform.Video.TextOverlay`
  against that renderer pixel for pixel.
  `make ci-demos` (side track J; run by `make ci` after the pool's chains, `CI_AFTER += demos`) plays
  each demo ROM as the boot splash on the Arty, then the menu over it
  (`demos/ps/splash-*.ps`), and compares frames 115 (the splash; checked
  against the spec's frames when made) and 125 (the menu) with
  `demos/golden/splash-*/`.
- **HP port** (tops with `HP0_ARVALID`, side track C; the shim is built
  with `-DVTOP_AXI`): `platform/sim/axi.zig` plays the Zynq's HP0 and its
  DDR, an AXI read slave with random ARREADY delay, first-beat latency
  (`--axi-latency N`, default 20) and gaps (`--axi-seed N`), and checks
  the master (AR held until ARREADY, alignment, 4 KiB, inside DDR); a
  violation fails the run. Its memory is a function of the address, the
  same as `m0e-unit`'s. PS scripts also take `wait US`, and
  `--expect-ps-hex FILE` compares the byte channel's bytes exactly. CI
  (`ci-m0e`) plays `systems/m0e/test/ps/ddr.ps` (two DDR test runs, then
  their result bytes) and compares the bytes with
  `test/golden/ps_bytes.hex`, which `m0e-unit` checks against its own
  reference; again with latency 60 and another seed. `--ddr-file
  ADDR=FILE` (side track I) puts a file into that DDR from ADDR (the
  pattern stays elsewhere): every Game Boy run reads its cartridge ROM
  from simulated DDR at 0x02000000 (`systems/gb/zedboard.mk`'s
  `SIM_SYS_ARGS`), with the random timing above. `gb-lockstep` also runs
  `gbCoreWith` behind a ROM port with random delays (layer 2) against the
  harness.
- **interactive** (`make play`, `platform/sim/play.zig`): an SDL3 window
  shows the VGA output; arrows and space are the board's buttons, F1 to F8
  toggle the switches. It runs as fast as the model allows, capped at 60
  frames/s: for `chip8_top` that's only about 6 frames/s (about 2.6 M pixel
  clocks/s against 25 M), so it's for checking behaviour, not for comfortable
  play.

`make lint` runs `verilator --lint-only -Wall` on the generated Verilog and
fails on any warning. The patterns Clash emits on purpose (register
initialisers, 64-bit index literals, unused slices of lowered tuples) are
waived, with reasons, in `boards/sim/clash.vlt`. Anything else is fixed at
the Clash level.

The harness's own unit tests (on `vga.zig`, `png.zig`, `i2c.zig`,
`hdmi.zig`, `ps.zig`; every Zig test is one `zig build test`, side track
H; a test that prints on stderr is shown with a `w` and a "failed
command" line even when it passes: the build summary's count is what
counts) run as part of `make test`, and so do the
PS app's (desktop builds of `platform/ps/*.zig`): the line protocol, the
link encoder (`pl.zig`, against a model of the PL decoder), the menu
(`menu.zig`), the SD driver (`sd.zig`, against a fake SDHCI controller
with SDHC, SDSC v2 and v1 cards, no card, a card that never gets ready, CRC
errors), and the FAT reader (`fat.zig`, against FAT12/16/32 images that
`tools/mkfat-test.sh` makes with `mkfs.fat`, mtools and `sfdisk` in
`build/ps-test/`: long names, many-cluster directories, fragmented files,
damaged FATs), SD writes on the fake controller, the USB host's fake
controller (`usb_fake.zig`, side track E: the Zynq controller's
registers, PHY and port, the EHCI schedules walked in memory, and a
keyboard emulated from descriptors captured from real receivers in
`usb_fixtures.zig`; it refuses malformed descriptors and counts every
rule a driver breaks), and `make sd`'s
planner (`platform/host/sdimage.zig`: its wire bytes through the relay's
parser rebuild the image; `sdimage_test.zig`: the plan for a
`tools/mksd.sh` image applied to a card full of old data, read with
`fat.zig`, gives the image's files, including an all-zero one). The PS app itself is built for the board (Cortex-A9) by
`make hdl SYSTEM=chip8`, so CI compiles it too; that build also runs
`tools/check-ps-align.sh` on the app's assembly (the PS runs with the MMU
off, where unaligned accesses fault: the first board run died on one).

**Run:** `make sim SYSTEM=m0b FRAMES=3`, `make lint SYSTEM=m0b`; later
`make sim SYSTEM=chip8 ROM=roms/chip8/timendus/3-corax+.ch8 FRAMES=120` or
`make play SYSTEM=chip8 ROM=…`

## Layer 5: Vivado (synthesis and implementation)

Non-project batch mode: `boards/zedboard/build.tcl`, run by `make bit`. It
reads the Clash output through Clash's `clashConnector.tcl` (HDL, the
generated `<top>.sdc` clock constraint, and the clock-wizard IP), then the
board XDC files, and marks the synchroniser flops `ASYNC_REG`. The script
fails the build if:

- worst negative slack (WNS) < 0 or worst hold slack (WHS) < 0
- `check_timing` finds unclocked registers, unconstrained endpoints,
  inputs/outputs without a delay or false path, multiple clocks, or loops
- there is any critical warning (this includes XDC lines that match no
  port, so a port-name mismatch between `<Name>.Top` and the XDC fails)
- DRC errors occur (`write_bitstream` refuses)

Reports kept per build in `build/<system>/vivado/reports/`:
`timing_summary.rpt`, `utilization.rpt`, `clock_interaction.rpt`,
`check_timing.rpt`, `io.rpt`, `drc.rpt`. Watch BRAM and DSP counts so that
later systems still fit.

Before Vivado ever runs, `make test` includes `make check-xdc`
(`tools/check-xdc.py`), which checks every derived pin and IOSTANDARD
against the master XDC.

**Run:** `make bit SYSTEM=chip8`

## Layer 6: On hardware

1. **Smoke bitstream first:** the milestone 0 bitstream (M0a: buttons and
   switches echoed on the LEDs plus a heartbeat; M0b: the VGA test pattern).
   Re-run it whenever pins, clocks, or the board top change.
2. Program via JTAG: `make prog` (runs Vivado's `hw_server`).
3. Built-in observability:
   - LEDs show `coDebug` (PC high bits, a "running" heartbeat, and an error
     latch for illegal opcodes)
   - a switch selects which debug byte is shown
4. **ILA** (Integrated Logic Analyzer) for deeper problems: `build.tcl` can
   insert an ILA on signals marked in the Clash `topEntity` debug port when
   `ILA=1`. Capture, export to CSV/VCD, and replay into a layer 2 or layer 3
   test.
5. Check the test ROMs on the monitor against the golden images.

## Continuous integration

Layers 0 to 4 run on the workstation through `make ci` (`nix develop -c
make ci`; no hosted CI, the user, 2026-10-08).
Layers 5 and 6 need Vivado and a board, so they run locally. Before tagging a
milestone, record the bitstream hash and timing summary in
`docs/systems/<name>.md`.

## Tiers and parallel runs

`make test` runs the Haskell suites as parallel processes
(`tools/run-tests.sh`: TEST_JOBS at a time, default cores / 4, each with
its share of the cores, logs in `build/test-logs/`), longest first: each
passing suite's time is kept per tier in
`~/.cache/emulation_clash/test-times-<tier>` (shared by the worktrees;
`TEST_TIMES_DIR` overrides), and a suite with no time yet starts first.
Suites with long test sets have two tiers:

- **quick** (default; `make test`, `make ci`): SingleStepTests 500 cases
  per opcode instead of 10 000 (`mos6502-spec`, `mos6502-lockstep`), 100
  per file instead of 1000 for the Z80 (`z80-spec`); in
  `nes-lockstep` at most 12 frames per PPU ROM, 15 random programs, the
  core lockstep on 7 ROMs for 100 000 steps; in `gb-lockstep` a quarter
  of every lockstep run and random test count, and three CPU test ROMs on
  the core alone; `chip8-golden` leaves out the two Clash simulations of
  the whole top (layer 4 checks the same golden).
- **`ALL=1`**: also every ZedBoard chain;
  after each fast-forward of `main` and before milestone tags (the user,
  2026-10-08).
- **full** (`FULL=1`, environment `EMU_FULL_TESTS=1`): everything, as each
  step's gate ran it. Run it for the system a step changes
  (`make test SYSTEM=nes FULL=1`).
- `NES_CORE_ROMS=all` on top: every NES test ROM and AccuracyCoin to the
  end on the Clash core (about 40 min).

`make ci` first builds what everything after it reads (`ci-prep`: test
ROMs and programs, `cabal build all`, both boards' PS app) (both, with
their stack checks, in every `make ci` since 2026-10-13; before, the
ZedBoard's only with `ALL=1`), then `make
test`, then layer 4 (`CI_OVERLAP=1` runs them at the same time: faster
on an idle machine, but together they need more than its 30 GB). Layer 4 is one pool of chains, one per
system and board (`ci-<name>`, `ci-arty-<name>`): the Arty's registered
in `CI_LINT` (every `make ci`), the ZedBoard's in `CI_LINT_ALL` (with
`ALL=1` only; side track K's D2, the user 2026-10-08); `make ci-list
[ALL=1]` prints the chains a run would include, in order, CI_JOBS at a time (default 3), the longest (`CI_LONG`, the
800XL's) first. Within a chain, `make hdl` runs once; further ROMs reuse
the Verilator model through `make resim` (only the memory image is
regenerated; Verilator only rebuilds when the Verilog changed). Run `make
sim` (or `make hdl`) again after changing the design, before `make
resim`. Memory images are made per board (`$(BUILD)/images/`; `make hdl`
copies them to the path the design names, `HDL_IMAGE_DIR`), so the two
boards' chains of one system can run at the same time; a new system with
images does the same. `make ci-arty` runs only the Arty's chains. A
chain that must run after the pool (it re-simulates the models the
pool's chains built) registers with `CI_AFTER += <name>` instead: those
run one at a time once the pool is done (`CI_AFTER_ALL += <name>`: only
with `ALL=1`).
Every Clash run takes its turn on the machine (`tools/clash-slot.sh`,
locks in `~/.cache/emulation_clash/`, shared by every worktree, under
any target): at most `CLASH_MAX` (2) at a time, none while less than
`CLASH_MEM_MIN` (6) GB is available, and only one at a time of those
whose `.mk` sets `CLASH_HEAVY = 1` (the 800XL, the Game Gear: about
10 GB). A real system's Clash run needs 4 to 10 GB; three at once ran a
`make ci` out of memory (2026-10-08). Tested by `tools/test_clash_slot.sh`
(part of `make test`).

## The result cache

Side track K's D3 (PLAN.md, "D3"; `tools/ci-cache.py`). Each unit of
work gets a key, a hash of everything it reads; **a unit whose key has a
cached pass is skipped**, and `make ci` reports it as "cached (commit,
date)". `make ci` ends with "N run, M cached": a STATUS gate line says
"`make ci` green (N run, M cached)". The cache is
`~/.cache/emulation_clash/ci-cache/` (`CI_CACHE_DIR`), shared by every
worktree; only passes are kept, entries go after 14 days, and a unit whose
key can't be computed runs ("unknown means run").

| Unit | Key |
|---|---|
| Haskell suite (`make ci` only; `make test` runs them all) | every file of its package and of the local packages it depends on (from the `.cabal` files' `build-depends`; not their Zig, C++ or make files), the build outputs its sources name (`"../../build/gb/asm"`), the ROM manifests (`fetch-roms.sh`, `roms-*.sha256`), the tier and `NES_CORE_ROMS`, `flake.lock`, `cabal.project` |
| Clash run (`make hdl`, so `make sim` and `make bit` too) | the source directories Clash reads (`systems/<n>/src`, `HDL_SRC_EXTRA`, `platform/src`), the system's `.mk`, the Clash command and its flags, `flake.lock`. **The Verilog is kept under this key**: `make hdl` restores it instead of running Clash, and leaves the files alone (no Verilator rebuild) when they are already the same |
| Vivado run (`make bit`; side track K, 2026-10-12) | the Clash output directory whole (Verilog and memory images), the connector, every Tcl of `boards/zedboard/` and the board's directory, the system's wrappers and XDC, the top, the Vivado installation (`VIVADO_SETTINGS`). **A build that passed `build.tcl`'s gates is kept**: `make bit` restores its `.bit`, `reports/` and `build.log` before taking the queue's slot and prints "make bit: cached (commit, date; WNS …, WHS …)". Measured: the Arty Game Boy, 12 min 48 s built, 1 s cached. Tested by `tools/test_bit_cache.sh` (a fake `vivado`). Vivado's incremental implementation was tried the same day: the BASIC licence doesn't cover it |
| layer 4 chain (`ci-<n>`, `ci-arty-<n>`) | the chain's commands as `make -n -B` prints them (comments dropped, the worktree's path replaced, independent of what is built); **the Verilog's hash** of every top it builds, not the Haskell sources (so an upstream change that gives the same Verilog runs no layer 4); every file its commands name (ROMs, goldens, `save.hex`, test programs; in `build/` only files made before the chain); each PS script as **the bytes the harness plays** (`ps-encode`, built from `pl.zig`: a comment in a script or in `pl.zig` costs no run, a change to an encoding reruns every chain with a script); each image generator (`cabal run <exe>`) as its binary; the harness sources (`platform/sim/`, `boards/sim/`), `build.zig`, `flake.lock` |

A chain that re-simulates models it doesn't build (`ci-demos`) is keyed
on their Verilog the same way, and before it runs `make hdl` brings each
of them up to date (from the cache: a chain that was skipped leaves its
build directory as it was). A build's intermediate files (`.o`, `.lbl`,
`.sym`, `.log`, ...) are never part of a key: ca65's objects carry the
time they were made. Modes, `CI_CACHE`: `on` (the default, `ALL=1` included since
2026-10-15, the user: the post-merge `make ci ALL=1` reruns only what the
merge changed, so it runs after every merge without waivers), `write`
(runs everything, reads nothing, records the passes: **the full run, `make
ci ALL=1 CI_CACHE=write`, before a milestone tag and once a week**), `off`
(nothing read or kept; also for `make hdl`). A STATUS gate needing every
run says `CI_CACHE=write` or `CI_CACHE=off`.

Why something ran: `tools/ci-cache.py chain-key <n> --plan --show`
prints a chain key's lines (a pass keeps them too, in the cache's
`pass/<key>.json`). The gate is `tools/test_ci_cache.sh` (in `make
test`): on a scratch copy, a change in `systems/nes/` runs only the NES's
suites and chains; one in `platform/src` runs the chains whose Verilog
changed and no other; `pl.zig`'s encoding reruns every chain with a PS
script, a comment in it none; with the scripts left out of the key that
check fails (the test's own mutation); a failing run isn't cached; a
second worktree sees the first one's passes.

### `make ci` on the worker

`make ci-remote [ALL=1] [FULL=1]` (`tools/ci-remote.sh`; side K, the
user 2026-10-18) runs `make ci` on the remote worker (`wrun`, the
user's `~/.claude/CLAUDE.md`), so a gate doesn't wait for this
machine's queue or take its memory. The worker has no Vivado: the
script copies both boards' `ps7_init` (Vivado's ZedBoard sample, the
Arty's from `make ps7-init BOARD=arty-z7-20`) into the checkout's copy
there, `build/remote-ps7/`, at every run, and `make ci` reads them
through `PS7_DIR`. Then it pulls the worker's cache entries (passes and
Verilog; this machine's own are never overwritten) into this machine's
cache: keys are relative to the checkout, so the worker's passes skip
the same units here, e.g. in the post-merge `make ci ALL=1`. The worker
runs its own queue (one heavy job at a time there); passes recorded
there carry `CI_CACHE_COMMIT`, the commit (`+dirty`: changed tracked
files). A gate line says "`make ci-remote` green (N run, M cached)".
`make bit`, `prog` and `sd` stay on this machine. The run is detached on
the worker (a worker `make ci` takes hours, longer than an ssh session or
an agent's command may last; its log is the checkout copy's
`build/ci-remote.log`, ending "ci-remote: exit N"): `make ci-remote`
starts it and waits; `make ci-remote-start` only starts it and `make
ci-remote-wait [MAX=<s>]` waits (exit 124 after MAX seconds, the run
going on; wait again), then prints the summary and pulls the cache. A
second start in the same checkout while one runs is refused. On the
worker, layer 4 runs 6 chains at a time (`CI_JOBS`; `CI_REMOTE_JOBS=<n>`
for another number), where `make ci` here runs 3: the worker is idle and
has 28 GB, and the heavy Clash runs stay limited by `clash-slot.sh`.
Before a run, this machine's cache entries the worker lacks are pushed
to it, so passes made here (another session's post-merge `make ci`)
count there too; the two caches stay in step both ways.

## The queue

The mechanics behind CLAUDE.md's working rule 10 (moved here from the
rule, unchanged, by the planning review of 2026-10-08):

**`make ci` and `make bit` take their turn by themselves**, one at a
time, in the machine-wide queue (`tools/queue.sh`; `make queue`
shows who runs, who waits, and the waiting list). **The waiting
list** (the user, 2026-10-08) is how a session asks for a turn:
- **Join it, don't start a job that waits:** `make queue-join
  WHO="<your session name>" [PENDING=1] [NOTE=…]` (`PENDING=1`: your
  user hasn't said go; `make queue-ready WHO=…` when they have). An
  agent's background command lives at most 2 hours, waiting in the
  queue included (two gate runs were cut off that way), so from an
  agent's shell (`CLAUDECODE=1`) a `make ci` / `make bit` whose slot
  is busy doesn't wait: it exits (3) at once and says so.
  `QUEUE_WAIT=1` waits anyway, for a wait you know is short (`make
  progress`). A terminal's `make ci` waits as long as it takes.
- **When your heavy job ends** (passed, failed, or stopped): message
  the first ready session on the list, "your turn" (`make queue`
  shows it; the job's last line names it).
- **When it's your turn** (that message, or `make queue` showing the
  slot free with you first: check every half hour or so, as messages
  get lost when a session is killed): start the job; it takes your
  entry off the list. Starting while another session is first gives
  a warning. `make queue-leave WHO=…` if you no longer need it.
**Slots** (side track K's C1 and C4, 2026-10-12): `ci` (`make ci`, with
`bit`; `make test FULL=1`; a heavy system's Clash run, `CLASH_HEAVY`,
outside `make ci` and only when its Verilog isn't cached), `bit` (`make
bit`), `board-arty` and `board-zedboard` (`make prog`, `sd`, `load`,
`saves-pull` on that board; the two boards don't block each other, nor
does a board job wait for `make bit`), `merge`. **Holds:** a board check
(`make prog`, the user looks, `make load`, …) or a merge (merging `main`,
the gates, the fast-forward, `make ci ALL=1` on `main`) spans several
commands and the user's time, so the session holds the slot for all of
it: `make queue-hold WHO=<session> [SLOTS=merge]` (default: `BOARD`'s
board), `make queue-release WHO=<session> [SLOTS=…]` when done, which
names who's next. While a hold stands, other worktrees' runs on that
slot wait (an agent's exit 3) and their holds are refused (exit 3: join
the waiting list with `SLOTS=`); the holder's own runs go ahead. A hold
over 12 hours old is stale: ignored, and marked in `make queue`. A
second merge waits for the first, so no merge invalidates another's gate
halfway through; a merge hold doesn't stop anyone's `make ci`. Tested by
`tools/test_queue.sh`.
**The fast-forward** (the user, 2026-10-28): `make merge-push
WHO=<session>` (`tools/merge-push.sh`) holds the merge slot first and
pushes nothing without it (another holder: exit 3), refuses a dirty tree,
a run from `main` itself and anything but a fast-forward of the remote's
`main` (exit 1: merge it in, re-run the gates), then pushes `HEAD:main`;
the slot stays held in every case, for the post-merge `make ci ALL=1` (or
the merge of `main` after a refusal). Tested by `tools/test_merge_push.sh`
(`make test`), in throwaway repositories and a private queue.
**Submitted jobs** (the user, 2026-10-14: the queue pings the session
when its work is finished). A shell can't message a session, but Claude
Code tells a session when one of its background commands ends. So:
- `make queue-submit WHO=<session> CMD="make ci ALL=1"` starts the
  command detached (`setsid`, `nohup`, `QUEUE_WAIT=1`). Its own `make
  ci`/`make bit` waits for its turn however long; it prints the job's id.
- `make queue-await JOB=<id> [MAX=<s>]`, run as the session's background
  command, ends when the job does, with the job's exit status and its
  log's tail: that is the ping. After MAX seconds (default 6600, under
  the 2-hour limit) it exits 124 ("still not finished": await again); a
  job whose runner died without a result (a restart) is "lost", 125.
- `make queue` (and `make watch`) lists the jobs still waiting or
  running and those that ended in the last 30 minutes (done with the
  exit status, or lost; `QUEUE_SHOW_DONE=<s>` for another window), and
  counts the older ones (the user, 2026-10-18: a day's finished jobs
  buried the running ones); `make queue-jobs` lists every job of the
  last 3 days, after which they are pruned. Their logs are in the
  queue's `jobs/<id>/log`.
A submitted job starts by itself, so nobody has to say "your turn" (the
machine once sat idle for five hours waiting for that message). Tested by
`tools/test_queue.sh`.
Messages are still for what the queue doesn't cover (shared files,
requests) and for agreeing a different order. `NO_QUEUE=1` skips
the queue: only when the user says so. `make progress` shows how far
every running job on the machine is (and `make watch` live, for the
user): check it before asking another session how long it needs.
Work started from a branch without the queue (merge `main`!) or by
hand is listed there as "untracked": the queue doesn't wait for it,
so agree it over messages; `make ci` and `make bit` warn about it.

## Make targets (summary)

| Target | Layers | Needs |
|---|---|---|
| `make test [FULL=1]` | 0 to 3 (+ harness unit tests); quick tier unless FULL=1 | Nix shell |
| `make hdl` | — | Nix shell |
| `make sim` | 4 | Nix shell |
| `make play` | 4, interactive | Nix shell + a display (SDL3 window) |
| `make desktop ROM=…` | — (side track Q) | Nix shell + a display: every system's spec as a desktop emulator, by the file's extension (the 800XL's `.cas` cassettes booted through the OS's SIO call, START held then Space; the NES, the Game Gear and Master System, the 800XL and the SNES on their fast versions, checked by `nes-specfast`, `sms-specfast`, `a800xl-specfast`, `snes-specfast` and `snes-specfast-cpu`); SDL3 window, sound, its own key map `~/.config/emulation_clash/desktop-keymap.conf` if there is one (else the defaults; `make keymap SYSTEM=gb KEYMAP=…` writes it), the board's battery saves. Built with GHC's LLVM backend (`DESKTOP_LLVM=0`: without; its own build directory, `build/desktop-llvm`). `DESKTOP_ARGS`: `--scale N`, `--gbc`, `--no-save`, `--no-fm`, `--bios`, `--os`, `--basic`, `--ipl`, `--fast`, `--quirks N`, and Q.6's switches: `--render-threads N` (the SNES's drawing; 2, at most one fewer than the CPUs) or `--no-render-thread`, `--no-frameskip`, `--no-frame-lag`, `--reference` (the original specs). `make test-desktop` (in `make test`): the libraries against the spec's goldens and the front end headless, on the normal build (dmg-acid2, nestest, the IBM logo; cpu_instrs too with `FULL=1`) |
| `make lint` | 4 | Nix shell |
| `make test-zig` | `zig build test` (every Zig unit test; side track H) after the FAT/SD test images: harness, PS app (protocol, link, menu, SD, FAT), desktop link (key maps: defaults = the old compiled-in maps, parser errors by line, round trip, scripted remap; `tools/test_link.py`: loads, latency, key-map files, remap through a virtual pad) (part of `make test`) | Nix shell |
| `make gb-asm` | Game Boy regression ROMs (rgbds; part of `make test`) | Nix shell |
| `make nes-asm` | NES regression ROMs (ca65; part of `make test`) | Nix shell |
| `make ci [FULL=1]` | 0 to 4, systems in parallel; suites and chains with a cached pass skipped ("The result cache"; `CI_CACHE=off` or `CI_CACHE=write` runs all); waits its turn in the machine-wide queue (both slots: no `make bit` beside it); from an agent's shell (`CLAUDECODE=1`) it doesn't wait: a busy slot exits 3 and points to the waiting list (`QUEUE_WAIT=1` waits) | Nix shell |
| `make watch` / `make progress` | — : progress bars (live / once) of every running `make ci`, `make test`, Clash run, simulation and Vivado run on the machine, from every worktree, and the queue. `tools/progress.sh` logs each step's start and end (`~/.cache/emulation_clash/progress/`, with each step's last 5 real durations for the estimates, kept per test tier: `FULL=1` and `NES_CORE_ROMS=…` runs have their own, `ALL=1` falls back to the plain run's; a pass served from the result cache is marked, counted done and kept out of the history); `tools/progress-watch.py` draws them: an item's estimate is the median of its recorded runs; `make ci` by the steps still to come plus the running step's suites or chains scheduled on their pool (`CI_JOBS` chains, `TEST_JOBS` suites at a time, the `CI_AFTER` chains one at a time after it), a simulation by frames written, Clash by its estimate, Vivado by its phase in `build.log`; an item past its estimate still has a tenth of (estimate + elapsed) to go; a job's bar is elapsed / (elapsed + ETA). The line reads `42%  ETA ~1h05  suites 3/51 · chains 9/17  3:02:41 elapsed`: the ETA always fits the width (the elapsed time and counts give way), and when most of it comes from defaults (no recorded run) it shows as a range (`ETA ~20-50m`) or `ETA ?` (side Q's request, 2026-11-01). Heavy work no job tracks (Vivado, Clash, the simulators, test suites started from a branch without the tracking, or by hand) is listed as "untracked", with its time running from its start (`/proc/<pid>/stat`, measured against the watcher's own start, as the worker's container has its own uptime), and `make ci` / `make bit` warn about it before they queue. **Memory and machines** (the user, 2026-10-18): a heading per machine with its memory in use and available; each job (and untracked process) with its process tree's RAM (PSS where readable, else RSS) and a line of its biggest processes by name; then the remote worker's own section (its jobs from `make ci-remote`, its queue), this script run there over ssh (`WATCH_REMOTE`, default `worker`, empty: none; refreshed every `WATCH_REMOTE_EVERY` seconds, 5). Tested by `tools/test_progress.sh` (part of `make test`; the worker's section through a fake ssh) | — |
| `make queue-join WHO=… [PENDING=1] [SLOTS=bit] [NOTE=…]`, `make queue-ready WHO=…`, `make queue-leave WHO=…` | — : the waiting list, a place in line for `make ci` / `make bit` without a waiting process (CLAUDE.md, working rule 10); tested by `tools/test_queue.sh` | — |
| `make keys`/`load`/`ping`/`sd`/`saves-pull`/`ddr-*` | — : each asks `tools/port-holder.sh` first whether another process holds the board's port (two `link`s on one port steal each other's replies: "NoAnswer"), and stops naming it; tested by `tools/test_port_holder.sh` (part of `make test`) | the board |
| `make version` | — : the PS app's version mark per board (`tools/ps-version.sh`: a hash of `platform/ps/`, `build.zig` and the board's `ps7_init.c`, comments included), which the SD menu shows at the title bar's right end ("PS 3fa9c1"): the board runs this build when they agree; the same sources give the same mark and a byte-identical ELF. Since 2026-11-03 the mark carries the sources' date ("PS 3fa9c1 10-18": the newest commit of the hashed files, "+" with uncommitted changes, none outside a git checkout; `tools/ps-version.sh --mark`), shown where the title leaves room and on the Systems page's status line; the Systems page shows each bitstream's build date (its .bit header), and `make sd`/`sd-sync` warn about a bitstream or BOOT.BIN built more than `SD_OLD_DAYS` (7) days ago | — |
| `make queue` | — : who holds the `make ci` / `make bit` slots, who waits (`tools/queue.sh`; tested by `tools/test_queue.sh`, part of `make test`) | — |
| `make resim ROM=…` | 4: the last `make sim` model with another ROM | Nix shell |
| `make bit` | 5; waits its turn in the machine-wide queue ("bit" slot) | Vivado |
| `make prog` | 6 | Vivado + board on USB (CHIP-8: also the PS relay, through xsdb) |
| `make keys` | 6 | board running CHIP-8, J14 on USB: desktop keyboards/gamepads → keys |
| `make load ROM=…` | 6 | as `keys`: loads a ROM into the running board, then keys |
| `make ping` | 6 | as `keys`: round-trip time PC ↔ PS relay |
| `make keymap SYSTEM=…` | — | desktop only (no board): asks for each button, writes the key-map file (side track B) |
| `make show-keymap SYSTEM=…` | — | desktop only: the key map in force |
| `make sd [SD_ROMS=dir]` | 6 | as `keys`: **erases** the board's SD card and writes a FAT32 image with the ROMs in `/chip8` (and `/gb`, `/nes`); since side track F also `/BOOT.BIN`, `/systems/<name>.bit` with `last.txt`, and the desktop's `keymap.conf` (checked by link first); since side track O the card's `/states` and `/movies`, pulled into `STATES_DIR` and `MOVIES_DIR` first, put back as they are (side track K, F1: the board makes a save, state or movie file when it first needs one, so no empty slots); `SD_MB` the image's size (default 64); test ROMs and our regression programs only with `SD_TESTS=1` (2026-10-16: the 64 MiB image was full) |
| `make sd-sync [SD_MOUNT=dir]` | 6 | (side track K) the same card through the desktop's SD card reader, no board: the card mounted by the desktop (FAT32 in one MBR partition, any size; exFAT won't do), its `/saves`, `/states`, `/movies` copied into `SAVES_DIR`, `STATES_DIR`, `MOVIES_DIR` (clock saves stamped), the image built as for `make sd` (`SD_SYNC_MB`, default 4096, sparse), then the card's files made the image's with rsync: changed files written, gone ones removed, the desktop's own folders kept (`tools/sd-sync.sh`; tested by `tools/test_sd_sync.sh` on a plain folder). `SD_MOUNT` if the card isn't the only FAT mount under `/run/media` or `/media`; eject before removing. `SD_ROMS_DIR=<folder>` (make sd too): every `SD_*_ROMS` folder under `roms/` taken from that folder instead (the main checkout's `roms/` has the user's games; a worktree's only the fetched test ROMs) |
| `make movie-pull`, `make movie-play MOVIE=file [SLOT=/movies/...]` | 6 | side track O: the card's `/movies` into `MOVIES_DIR`; a movie file into a game's slot on the card over the link (header last), then "Play movie" in the board's Options |
| `make boot-image` | 5 | Vivado's `bootgen` (outside nix, after `make ps-app`): `build/boot/BOOT.BIN`, the PS app as the boot ROM's first-stage program, its header checked (`tools/check-boot-image.sh`; side track F) |
| `make ddr-test [DDR_RUN=…]` | 6 | board running M0e (`make prog SYSTEM=m0e`), J14 on USB: the DDR test suite (latency histogram, bandwidth, checksums), or one run (side track C) |
| `make ddr-load FILE=…` | 6 | as `ddr-test`: a file into DDR through the relay, read back by the PL over HP0 |
| `… BOARD=arty-z7-20` | as without | `hdl`, `lint`, `sim`, `bit`, `prog` for the Arty Z7-20 (side track G; `systems/<name>/arty-z7-20.mk`, outputs in `build/arty-z7-20/<name>/`); `sim` receives the HDMI output as TMDS (`arty-sim`, below) |
| `make jtag-cables [BOARD=…]` | 6, dry | the connected JTAG cables and the one `make prog` would program (`tools/jtag-pick.sh`, serials from `local/boards.mk`); programs nothing |
| `make test-jtag` | — | the cable picker's rules with made-up cable lists (part of `make test`) |

## Layer 4 on the Arty Z7-20 (side track G)

Arty tops (`<Name>.ArtyTop`, `make sim SYSTEM=… BOARD=arty-z7-20`) have
the Arty XDC's port names, so they get their own shim,
`platform/sim/vtop_arty.cpp`, and harness, `platform/sim/arty.zig`
(`arty-sim`, `zig build sim-arty`). The hand-written MMCM and TMDS
serialisers have stand-ins in `boards/sim/` (`arty_mmcm.v`, `tmds_out.v`,
found through `-y boards/sim`): one tick is one 5× clock, the pixel clock
is a fifth of it, and each line carries one bit per clock edge. The TMDS
sink (`platform/sim/tmds.zig`) aligns symbols on the clock pair (each
must be 10 bits), checks that every N line is the complement of its P
line, decodes the three lanes (DVI 1.0), **re-encodes every data symbol
with its own encoder and fails on any difference** (a DC-balance mistake
still decodes to the right picture; a seeded one fails the run), checks
that the lanes agree on data versus control, and feeds `vga.zig`'s
decoder in 8-bit mode, which checks the timing as for VGA. TMDS is
lossless, so frames are compared with the system's goldens **exactly**:
M0b's layer 3 golden, CHIP-8's VGA goldens (`SIM_GOLDEN`). Layer 1 for
the encoder: `Test.Platform.Tmds` (platform-unit), exhaustive against a
reference written from the specification.

`make ci` runs the Arty's chains (`ci-arty-<name>`) and, with `ALL=1`,
the ZedBoard's in the same pool (their memory images are per board since 2026-10-08;
before, they ran after them): lint of the M0a, M0b and CHIP-8 Arty tops,
M0b's golden and CHIP-8's IBM logo through TMDS, so other tracks' merges
can't break the Arty unnoticed.

Side track G phase 2 adds to `ci-arty`: the Arty CHIP-8's menu overlay
over the PS link (the ZedBoard's golden), the Game Boy (dmg-acid2 from
simulated DDR over HP0, its menu overlay, "Passed" on the byte channel)
and the NES (its scene ROM, its menu overlay, nestest loaded over the
link; compared on 4 bits a channel, `--golden-4bit`). `arty-sim` takes
`--ps`, `--expect-ps-in`, `--expect-ps-text`, `--expect-ps-hex`,
`--ddr-file`, `--axi-latency`, `--axi-seed` as `vga-sim` does (both
compare `--expect-ps-hex` through `ps.zig`'s `hexReport`; side K added it
to `arty-sim` 2026-10-11, so `ci-arty-gb`, `ci-arty-gbc` and `ci-arty-gg`
check the battery saves' read-back byte for byte, as the ZedBoard's
chains do). Side track O (O.2) added `--ps-bytes-out FILE` (the byte
channel written as `--expect-ps-hex` reads it: a golden's first run) and
`--movie-record FILE` / `--movie-play FILE` (the PS app's frame rule,
`platform/ps/movie.zig`: the input word changes only at PS_IN bit 7's
flips; `ci-arty-gb`'s determinism gate records `joybank.gb` under one
`--axi-seed` and plays it back under another). Layer 6 for the Arty's
PS: `make ps7-init BOARD=arty-z7-20` (Vivado writes its ps7_init) and
`make ps7-check BOARD=arty-z7-20` (xsdb: ps7_init and a DDR test).

`make sd` (side track G phase 2, for both boards): link asks the relay for
a hash of each 16-block chunk on the card (`C`) and skips chunks the card
already holds, writes the rest as one multi-block write each (`M`, `Z`),
and talks at 2 Mbaud where the bridge can (`B4`; the rate is cached per
port). `SD_REWRITE=1` writes everything. With a relay that doesn't know
`C`, link falls back to the block-by-block `D`/`Z`. `tools/test_link.py`
covers all of it against its fake relay (modes `fast2m`, `old_relay`; a
second write of the same image skips every chunk, one changed block
rewrites one chunk).

Arty sound (side track G phase 2, step 6): `arty-sim --expect-tone HZ`
measures the tone on `aud_pwm` (averaged per 524 pixel clocks,
`platform/sim/arty_tone.zig`, with its own unit test); `ci-arty` runs it
for the CHIP-8 beep, `tone.gb` and `tone.nes`. `Test.Platform.Pwm`
(platform-unit) checks the modulator's density and its register.

HDMI audio on the Arty (side track G phase 3): `Test.Platform.HdmiPacket`
(every codeword a multiple of the BCH generator, as polynomials; TERC4;
the layout read back; packet bytes known-answer) and `Test.Platform.Hdmi`
(two frames decoded: pixels, slots, ECC, packet types, ~800.8 samples a
frame, syncs). The TMDS sink (`tmds.zig`) decodes HDMI mode (its own
unit test feeds an island); `arty-sim --expect-hdmi-tone HZ` checks the
samples the islands carry.

**Default board (2026-10-06): the Arty.** `make hdl/sim/bit/prog/sd/…`
without `BOARD` build for the Arty Z7-20; `BOARD=zedboard` for the
ZedBoard. `make ci`'s ZedBoard chains (`ci-m0a` … `ci-m0e`, also when
run on their own) pin `BOARD=zedboard`; `ci-arty` pins the Arty.

