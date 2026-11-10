# Super Nintendo (SNES, NTSC)

Target: a **Super Nintendo** (NTSC, the 5A22 CPU with its 65C816 core,
the two-chip PPU (S-PPU1, S-PPU2), the sound module (SPC700, S-DSP,
64 KiB ARAM), 128 KiB WRAM, LoROM and HiROM cartridges with battery
SRAM, two pads), milestone 7, its own bitstream (`snes.bit`, system ID
9, menu folder `/snes`). This is the first rung of a ladder: first the
base console, then its enhancement chips, one at a time (SA-1, DSP-1,
Super FX; "The ladder" below). Planned 2026-10-15 (Claude Code, branch
`m7-snes`, at the user's request through side track K), before any
code: everything here is a plan, and the parts marked *to verify* are
checked against the references before code depends on them. The
user's decisions (2026-10-15) are in the next section. When the status log at the end disagrees with the plan, the log
wins.

## Why the SNES, and what is new

- **Two new CPUs.** The 65C816 is a 16-bit 6502 (emulation and native
  modes, 8/16-bit A and X/Y set by the M and X flags, 24-bit addresses
  through the bank registers, new modes and instructions). It goes in a
  new shared package, **`cpus/w65c816`**, built like `cpus/mos6502` (a
  cycle-level spec that is given a bus, and a Clash core in lockstep with
  it), because the SA-1 rung reuses it. `cpus/mos6502` is not changed
  (it is shared by the NES and the 800XL). The **SPC700** (the sound
  CPU) gets its own package too, **`cpus/spc700`**: it has its own test
  suite, and keeping it separate keeps `systems/snes` smaller to compile.
- **The PPU is the largest video unit yet.** It has eight background
  modes (up to four layers, 2/4/8 bits per pixel, Mode 7's rotation and
  scaling), 128 sprites (32 per line, 34 tile slivers per line), two
  windows, a main and a sub screen with colour maths, mosaic,
  offset-per-tile, direct colour, and 512-wide and interlaced modes. It
  is LUT-heavy, so it is the first unit built in Clash and measured
  ("Resource budget").
- **Block RAM is the binding limit, not LUTs alone.** The console's own
  RAMs are about 65 of the XC7Z020's 140 tiles, so three frame buffers
  (about 84 tiles at 256 × 224 × 15 bits) don't fit beside them. The
  display is therefore **line-locked**: the core is paced by the
  640 × 480 output and writes into a small ring of line buffers ("Clock
  and display").
- **The ROM is in DDR** (up to 4 MiB HiROM, 6 MiB ExHiROM later), as
  for the Game Boy, NES, Game Gear and Master System. A miss stops the
  whole machine (every clock enable) until the word is there, and the
  machine catches up afterwards (the Game Gear's "freeze and catch up",
  `Gg.RomPort`).
- **A boot ROM in the sound CPU.** The SPC700 starts in a 64-byte IPL
  ROM, Nintendo's code. It is never fetched or committed (rule 7), so
  it needs a decision ("Questions for the user").
- **Sound at 32 kHz.** The S-DSP's sample rate is 32 kHz. The platform's
  audio path runs at 48 kHz (`Platform.Audio.Spdif`, the Arty's HDMI
  packets), so `systems/snes` resamples 2:3 itself and no platform
  interface changes.

## Machine summary (NTSC; from fullsnes and anomie's documents, every row *to verify* at its step)

| Item | Value |
|---|---|
| Master clock | 21.477272 MHz (6 × the colour carrier 3.579545 MHz) |
| CPU | 5A22: a 65C816 core; a memory cycle is 6, 8 or 12 master clocks by address (fast: internal operations, `$2100-$21FF`, `$4200-$5FFF`, FastROM in banks `$80-$FF` with `MEMSEL` (`$420D`) set; slow: WRAM, SlowROM; extra-slow: `$4000-$41FF`, the old joypad ports) |
| Frame | 262 lines (263 in the interlaced field) × 1364 master clocks (340 dots of 4, two of them 6 long), one line 4 clocks short in every other non-interlaced frame; 357,368 clocks, 60.10 Hz |
| Picture | 256 × 224 (or 239 with overscan), 512 wide in modes 5 and 6 and pseudo-hires, 448/478 lines interlaced; 15-bit colour |
| WRAM | 128 KiB at `$7E0000-$7FFFFF`, its first 8 KiB mirrored at `$0000-$1FFF` in banks `$00-$3F` and `$80-$BF`; refreshed 40 master clocks per line (the CPU stalls) |
| Video RAM | VRAM 64 KiB (32 K words, two 8-bit chips with their own address buses: Mode 7 reads both at different addresses); CGRAM 256 × 15 bits; OAM 544 bytes |
| CPU registers | `$4200-$421F`: NMI and H/V IRQ enables and timers, WRIO, multiply (8 × 8) and divide (16 / 8) with their delays, auto-joypad read, RDNMI, TIMEUP, HVBJOY; `$4300-$437F`: DMA and HDMA, 8 channels |
| Sound module | SPC700 at 1.024 MHz (24.576 MHz / 24, its own resonator), 64 KiB ARAM, the 64-byte IPL ROM at `$FFC0`, three timers (8, 8 and 64 kHz), four ports to the CPU (`$2140-$2143`); the S-DSP: eight voices of BRR samples, Gaussian interpolation, ADSR and GAIN envelopes, noise, pitch modulation, echo (an 8-tap FIR, the buffer in ARAM, up to 30 KiB), 32 kHz stereo, 16 bits |
| Cartridge | the header at ROM `$7FC0` (LoROM) or `$FFC0` (HiROM): map mode `$FFD5` (`$20` LoROM, `$21` HiROM, `$30`/`$31` FastROM, `$25` ExHiROM), type `$FFD6` (`$02` ROM + RAM + battery; the chip types above `$02`), ROM size `$FFD7`, SRAM size `$FFD8` (1 KiB << n), region `$FFD9`, checksum and complement `$FFDC-$FFDF`; a 512-byte copier header when the file size is 512 mod 1024 |
| LoROM | 32 KiB banks at `$8000-$FFFF` of banks `$00-$7D`/`$80-$FF`; SRAM at `$70-$7D:$0000-$7FFF` |
| HiROM | 64 KiB banks at `$C0-$FF`, their upper halves also at `$00-$3F:$8000`; SRAM at `$20-$3F:$6000-$7FFF` (8 KiB windows) |
| Input | two pads of 12 buttons (B, Y, Select, Start, up, down, left, right, A, X, L, R), read serially through `$4016/$4017` or by auto-read into `$4218-$421F` |
| Interrupts | NMI at V-blank, IRQ from the H/V timers (and from cartridge chips later) |

## The plan's choices

### Clock and display: line-locked to the output

The system runs in `DomPix` (25.175 MHz on the ZedBoard, 25.174 MHz on
the Arty), as every system does, with a **master-clock enable** in
place of the master clock. The enable is "not stalled and not ahead of
the display", at most one master clock per pixel clock. So the core has
at least 4 pixel clocks per dot (one dot is 4 master clocks), and at
least 6 per CPU cycle. That is the NES's budget ("a dot's work in ≤ 4
clocks", nes.md).

**Every SNES line is shown on two of the output's lines.** One SNES
line is 1364 master clocks, and two 640 × 480 lines are 1600 pixel
clocks. The core writes each finished line into a ring of 8 line
buffers of 512 × 15 bits (2 RAMB36). `Snes.Display` reads each line
twice: low-res pixels ×2 and hi-res pixels ×1 across, so 512 × 448 at
(64, 16), and 239-line overscan as 478 lines at (64, 1). An SNES frame
fills 524 of the output's 525 lines, and in the last one the core waits.
So the machine runs at the display's rate:

- **Speed:** 59.94 Hz in place of 60.10 Hz (0.26 % slow). The master
  clock averages 21.46 MHz in a line (0.07 % slow). This is about the
  speed difference the other systems' frame buffers hide by repeating a
  frame now and then; here there is no repeat and no tearing.
- **DDR misses:** the core gets 236 spare clocks per line. With 8 lines
  in the ring, it can fall up to about 6 lines (9,600 clocks, several
  hundred misses) behind before a line is late. A late line shows the
  ring's old line and is counted (`Snes.Display.pcLate`; LD1 lights
  while the count isn't 0). Layer 4 checks it: none on the exit game
  over 30 frames with a DDR latency of 60 clocks (`--expect-led`), with
  `Platform.Mem.Ddr`'s cache as it is.
- **Interlace:** each field is shown line-doubled (bob), not woven.
- **The cost:** there is no frame buffer, so when the core is held (the
  menu, a load) there is no picture behind the overlay: the menu shows
  over black. The other systems show their frozen frame. The way out,
  frame buffers in DDR through a write channel on HP0, is a
  `Platform.Mem.Ddr` change (side track K's interface), and is not
  planned. It would be a request if the user wants the frozen picture.

The sound module is paced by the master clock too (decided at step 7.4;
first planned on its own enable). On the console it has its own
resonator, so its rate against the CPU's differs from unit to unit. Here
it gets 5632/118125 SPC700 cycles a master clock (1.024 MHz against
21.477 MHz), so when the master clock waits for a DDR miss, the sound
module waits with it. The core and the spec then stay in lockstep, and
the price is a pitch 0.26 % low (4.5 cents), from the display's pacing.

### Audio: 32 kHz to 48 kHz in the system

The S-DSP's 32 kHz stereo goes to the platform's 48 kHz path unchanged
(S/PDIF on the ZedBoard, HDMI audio and PWM on the Arty). **As built
(step 7.10):** no resampler of its own. The plan was a 2:3 polyphase
filter in a `Snes.Audio`. In its place, `Snes.Machine.snesAudio` holds
each sample and low-passes it with two one-pole filters at the pixel
clock (y += (x − y)/512, about 7.8 kHz; the 32 kHz steps' images start
at 16 kHz), and the 48 kHz samplers take the level. No DSP48, no
module. The tone checks at layer 4 hold on both boards.
Sending 32 kHz to HDMI natively would change `Platform.Audio`, which is
K's, so it is not planned.

### The cartridge, DDR and saves

- **DDR layout (the media region):** the ROM at `0x02000000`, up to
  8 MiB reserved (`0x02000000-0x027FFFFF`; 4 MiB HiROM in M7, ExHiROM's
  6 MiB later). Companion files go at `0x02800000`: a chip's firmware
  dump, or the user's own IPL dump if the user chooses that option.
- **The PS's check** (`platform/ps/systems/snes.zig`): it strips a
  copier header, scores the two header positions (checksum and
  complement, map mode, sane sizes) to pick LoROM or HiROM, and refuses
  a cartridge type with a chip the bitstream lacks. It tells the core
  the mapping, the ROM size (for mirroring) and the SRAM size.
- **SRAM in block RAM**, up to 32 KiB (8 tiles; *to verify* against the
  games the user has: few base-console games use more than 8 KiB). It
  is saved to `/saves/snes` as the Game Gear's is (bit 6 of `PS_IN` =
  SRAM written since the PS last cleared it; the read-back over the
  link).

**As built (step 7.11):**
- The PS loads the file whole: the menu's DDR loader has no skip.
- The word is 16 bits, the 2 bytes `ddr_info` carries
  (`Snes.Machine`, `snes_cart.zig`). Its bit 15 marks a copier header,
  and the PL then reads the ROM 512 bytes on.
- The header is scored after the load, on the ROM in DDR (`word`; the
  same scores as `Snes.Cart`, checked against `snes-image` on all 56
  fetched ROMs). `check` sees only the first 512 bytes, so it checks the
  size alone: a chip cartridge isn't refused yet.
- Every ROM gets a 32 KiB save slot, since the header's SRAM size isn't
  in that block either. The slot is written only when the game writes
  SRAM.
- Both wait for a check on the whole ROM: requested from side K.
- SRAM's second port and the save control are in `Snes.Save`: the Game
  Gear's link map and protocol.

### The machine in Clash (step 7.9; planned 2026-10-16)

The spec is a sequential program of two kinds of operation: a **tick**
(a master clock passes; the line's events fire as H reaches them) and an
**access** (a read or write, in no time). The Clash machine runs the same
program: each pixel clock it does one master clock's tick (when the
enable allows), and the accesses due at that time point in the same clock
where it can.

- **Two threads of bus work.** The background is the CPU's cycles (6, 8
  or 12 master clocks; a read 4 before the end, a write at the end) and
  the DMA a write to 420Bh starts (aligned to 8, then 8, then 8 a
  channel and 8 a byte). The foreground is what the line's events start:
  the WRAM refresh (40 clocks at H 538), HDMA's set-up (line 0, H 12)
  and its transfers (H 1116). A foreground activity holds the background
  for its clocks; the background's count goes on after it. In the spec
  this is `tick` recursing into the events' own ticks. Foreground
  activities never overlap: the longest HDMA (about 470 clocks from
  H 1116) ends before the next line's refresh, and the set-up (at most
  about 210 clocks from H 12) before it.
- **The CPU's request** is on the core's bus two pixel clocks after a
  cycle's end ("W65c816.Core"), so a 6-clock cycle's read, 2 master
  clocks in, has it in time. The cycle's speed comes from its address.
- **Accesses with no time between them** (a DMA byte from the A bus to
  the PPU, HDMA's table reads, a read that waits for DDR) hold the
  enable for a pixel clock each: the PPU gets the access with its tick
  as in the PPU's lockstep. A DMA byte costs 9 pixel clocks for 8 master
  clocks, under the 1.17 there are.
- **The sound module runs on its own as far as the spec allows.** The
  spec catches it up, a whole instruction at a time, to the master
  clock's target (5632/118125 cycles a master clock) at each access to
  its ports and at each line's end. The Clash module starts an SPC700
  instruction whenever its first cycle is before the target now (the
  spec's own rule), at a pixel clock a cycle, and the S-DSP's sample
  holds it as in the spec. A CPU access to the ports waits until every
  instruction begun before the target has finished. So the CPU sees the
  ports as in the spec, and the SPC700 sees the CPU's writes as in the
  spec. Comparing the two needs the SPC700's own trace, not its state
  at the CPU's steps (the module runs ahead of the spec's catch-ups).
- **The ROM** comes through the Game Gear's pattern (`Gg.RomPort`): the
  pending read's address goes to the DDR port as soon as it is known,
  and the machine is held until the byte is there; credits make up the
  time later.

**Sub-steps, each with its own gate:**

- **7.9a, the 5A22's side** (`Snes.Bus`: the sequencer, the line's
  events, the registers 4200h-437Fh, multiply and divide, the joypads'
  auto-read, DMA, HDMA, the WRAM port; `Snes.Core` with the CPU and the
  PPU): the core against the spec on the test ROMs, with **the spec's
  sound module serving the core's port accesses** (co-simulation),
  every CPU bus cycle compared (its master clock, kind, address, data),
  the PPU's frames, the memories at the end. ROM from an ideal memory.
- **7.9b, the sound module** (`Snes.Apu`: the SPC700 core, the boot
  ROM, the I/O ports, the timers, the S-DSP): the same tests with the
  Clash module, the SPC700's bus cycles compared too; spctest
  "Success".
- **7.9c, the ROM from DDR** (`Snes.RomPort`): the same with
  `Platform.Mem.Sim`'s random latency; the exit game's first frames.

### Pads

Pad 1's 12 buttons go in the PS link's 16 live user bits (`pgUser`).
Pad 2, if it is in scope, is written by the PS as a register on the
system's write bus (the 800XL sends its key matrix that way), so no
platform change is needed. The key map is a `[snes]` section in
`keymap.zig` (additive, until D4's second part moves key maps out).

## Resource budget (estimates, 2026-10-15; measured at each Clash step)

### Block RAM (RAMB36 tiles; 140 on the XC7Z020)

| What | Tiles | Note |
|---|---|---|
| WRAM 128 KiB | 32 | random access at the CPU's speed: never DDR |
| VRAM 64 KiB, two 32 KiB halves | 16 | each half dual-port: the PPU's fetch, the CPU and DMA writes |
| ARAM 64 KiB | 16 | SPC700, BRR fetches, echo reads and writes |
| CGRAM, OAM | 1 | |
| Cartridge SRAM, up to 32 KiB | 8 | |
| Line ring, 8 × 512 × 15 bits | 2 | in place of frame buffers |
| PPU line buffers (sprites, tile fetch) | 2 | |
| DDR line cache | 1 to 4 | sized by the layer 4 measurement |
| S-DSP tables (Gaussian 512 × 11 bits), echo FIR | 0.5 | |
| Platform (overlay text, font, PS link, audio) | about 4 | CHIP-8's whole build is 4 |
| **Total** | **about 83 to 86** | **59 to 61 %**: about 55 left for the chips |

### LUTs (53,200), from our own builds

Our Clash designs are big: the Arty's latest builds measure the Game
Boy at 25,649 to 41,355 LUTs (with save states), the GBC at 38,689 (and
54,424 with save states, which overflowed: side K's log, 2026-10-12),
the NES with MMC5 at 31,705, the Master System at 18,075 and CHIP-8 at
3,337 (about the platform's floor). Scaled from those:

| Unit | LUTs (estimate) | How it is kept small |
|---|---|---|
| Platform (TMDS, PS link, overlay, DDR port, audio) | 4,000 to 5,000 | as built |
| 65C816 core | 4,000 to 8,000 | per-cycle function, as the 6502 |
| 5A22's own parts (DMA/HDMA ×8, multiply, divide, timers, auto-read, map) | 3,000 to 6,000 | DMA registers in LUT RAM; multiply on a DSP48, divide iterative |
| PPU (both chips) | 15,000 to 25,000 | at least 4 clocks per dot, so the layers are fetched and mixed in turn, not in parallel; Mode 7's products on DSP48s |
| SPC700 | 3,000 to 5,000 | as the 6502 |
| S-DSP | 2,000 to 6,000 | about 786 clocks per 32 kHz sample, so serial: one voice at a time on one DSP48 |
| Resampler, mixer | 500 to 1,000 | one DSP48 |
| **Total** | **31,500 to 56,000** | **59 to 105 %** |

**So the base console itself is at risk, before any chip.** That is
why the plan measures early. Each Clash unit's gate includes an
out-of-context Vivado synthesis of that unit alone (LUTs, block RAM,
DSP48s, its worst path), recorded in "Measured" below; it runs in the
queue's "bit" slot. The PPU is built first. After it and the 65C816,
**a go/no-go check**: if the measured units and the estimates left
project more than 85 % of the LUTs, stop and ask the user what to cut
(hi-res, the rarer PPU features, pad 2), before the whole core is
built.

### Measured

Out-of-context implementation (synthesis, placement, routing; `make
snes-ooc`, `systems/snes/ooc.tcl`) on the Arty's part, the 25.175 MHz
clock the only constraint, inputs and outputs registered:

| Unit (top) | LUTs | FFs | Block RAM tiles | DSP48 | WNS at 39.72 ns | Date |
|---|---|---|---|---|---|---|
| PPU with VRAM, CGRAM, OAM, line buffer (`Snes.PpuOoc`) | 21,467 (40.4 %) | 6,043 | 18.5 (16 RAMB36 VRAM, 5 RAMB18) | 11 | +2.80 ns | 2026-10-15 |
| PPU, the ring in memories and one screen evaluator (`Snes.PpuOoc`) | **12,075 (22.7 %)**, 288 of them LUT RAM | 3,319 | 18.5 | 11 | +2.50 ns | 2026-10-15 |
| 65C816 core (`W65c816.CoreOoc`) | 3,404 (6.4 %) | 313 | 0 | 0 | +9.46 ns | 2026-10-15 |
| SPC700 core (`Spc700.CoreOoc`) | 1,603 (3.0 %) | 168 | 0 | 0 | +19.49 ns | 2026-10-15 |
| S-DSP with ARAM, its samples, the Gaussian table (`Snes.DspOoc`) | 7,284 (13.7 %) | 2,657 | 17 (16 RAMB36 ARAM, 2 RAMB18) | 1 | +16.13 ns | 2026-10-15 |
| 5A22 with the 65C816, WRAM, SRAM, first build (`Snes.BusOoc`) | 30,828 (57.9 %): far too big | 1,906 | 40 (32 WRAM, 8 SRAM) | 0 | **-12.27 ns** (79 levels) | 2026-10-16 |
| 5A22 with the 65C816, WRAM, SRAM, rebuilt (`Snes.BusOoc`) | **10,005 (18.8 %)**, 96 of them LUT RAM | 1,117 | 40 | 0 | +0.05 ns (61 levels) | 2026-10-16 |
| The whole machine, its three state machines as components (`Snes.MachineOoc`, hierarchy kept; synthesis only): the sound module 45,301, the PPU 10,834, the 5A22 7,661, the rest 2,224 | 66,020 (over the part) | 9,087 | | 17 | | 2026-10-18 |
| The same, the sound module's DSP step written once: the sound module **20,236**, the PPU 11,077, the 5A22 7,661, the rest 2,224 | 41,198 | | | 15 | | 2026-10-18 |
| **The Arty's bitstream** after that (`make bit SYSTEM=snes`, flat) | **27,821 (52.3 %)**, was 48,742 (91.6 %) | 9,305 | 85.5 | 15 | **+1.774 ns** | 2026-10-19 |
| **The ZedBoard's bitstream** after that (`make bit SYSTEM=snes BOARD=zedboard`) | **28,199 (53.0 %)**, was 41,129 (77.3 %) | | 85.5 | | **+2.926 ns** | 2026-10-19 |

The PPU is inside its estimate (15,000 to 25,000). Its worst path (36
levels, 13 of them carry chains) runs from the state register through a
tick's dot, the sprite unit and the CPU's register access into CGRAM's
port B address: the access is worked out after the tick in the same
clock. It meets the clock; if a later build needs the margin, the access
can work on the state before the tick (the two don't touch the same
registers but at a line's end).

The 65C816 core is under its estimate (4,000 to 8,000). Its worst path
(42 levels, 16 of them carry chains) is the clock that works out the
next request from the state, through the address generator.

**Go/no-go (2026-10-15).** Measured: 24,871 LUTs (PPU 21,467, 65C816
3,404). Estimates left: platform 4,000 to 5,000, the 5A22's own parts
3,000 to 6,000, SPC700 3,000 to 5,000, S-DSP 2,000 to 6,000, resampler
and mixer 500 to 1,000, so 12,500 to 23,000. Projected: **37,371 to
47,871 LUTs, 70 to 90 %**, the middle 42,600 (80 %), against the line at
85 % (45,220). The upper end crosses the line, so the step stops for the
user (PLAN 7.7); the chip ladder adds to whatever the base console
leaves (SA-1: a second 65C816, 3,404, plus 3,000 to 6,000).

**The user's choice (2026-10-15): optimise the PPU first**, keeping every
feature. The column ring moves out of registers into a memory, and the
main and sub screens share one layer evaluator over the dot's clocks.
Then the PPU is measured again, before the rest of the base console is
built.

**After the optimisation (2026-10-15): go.** The PPU measures 12,075
LUTs (it was 21,467). The column ring was 3,300 register bits, each with
a write mux and 8-way read muxes; it is now 21 small memories (288 LUTs
of LUT RAM). The main and sub screens now share one layer evaluator
over two clocks, not two evaluators. Block RAM, DSP48s and the clock are
unchanged (+2.50 ns, 44 levels). Measured: 15,479 (PPU and 65C816).
With the estimates left (12,500 to 23,000), that projects **27,979 to
38,479 LUTs, 53 to 72 %**, under the 85 % line, with 7,000 to 17,000 to
spare for a chip's bitstream. Every feature stays.

**After step 7.8 (2026-10-15).** The SPC700 measures 1,603 LUTs (its
estimate was 3,000 to 5,000), the S-DSP 7,284 (estimate 2,000 to 6,000:
over). The DSP's 128 registers are flip-flops, with the SPC700's read
port and the voices' read multiplexers, and the voices' rotating state
is about 900 more flip-flops; moving the registers into LUT RAM would
save perhaps 1,500 if a chip's bitstream needs the room. Measured:
24,366 LUTs. With the estimates left (platform 4,000 to 5,000, the
5A22's own parts 3,000 to 6,000, the resampler 500 to 1,000), that projects
**31,866 to 36,366, 60 to 68 %**.

**The 5A22's first build (2026-10-16) is far too big:** 30,828 LUTs with
the 65C816's 3,404 (the estimate for its own parts: 3,000 to 6,000), and
it misses the clock by 12 ns. Two causes in its structure (the
behaviour is right: `snes-core`): the DMA registers are 1,024
flip-flops that six places read and write by a channel number, and each
of the bus's zero-time steps makes its own access (the CPU's read,
DMA's two sides, HDMA's bytes and table reads: each with its own address
decode and register multiplexer, the 128-way one of the DMA registers
among them). The rebuild: the DMA registers in 16 small memories (a
register each, a channel a word, a channel read whole in a clock; the
engines work on the channel loaded and write back what changes), and one
access unit that the steps only ask.

**Rebuilt (2026-10-16): 10,005 LUTs** with the 65C816 (its own parts
about 6,600), and the clock met with almost nothing to spare (+0.05 ns,
61 levels; a register in the access unit's path is the way out if the
whole machine misses). Measured now: 30,967 (PPU 12,075, the 5A22 10,005,
SPC700 1,603, S-DSP 7,284). With the platform (4,000 to 5,000), the
resampler (500 to 1,000) and the sound module's I/O and timers (about
500), that projects **36,000 to 37,500, 68 to 70 %**.

## Save states (step 7.14; planned 2026-10-23)

The direct path, as CHIP-8's (the user's decision, 2026-10-23):
- the PS reads the frozen core's state and later writes it back;
- the state is the records (the 5A22 with the 65C816, the PPU, the
  sound module, the ROM port) and every memory;
- the core then resumes where it stopped;
- there is no restore routine on a CPU.

**The one difficulty: the memories' bytes in flight.** Every block RAM
answers a clock late. A step uses the bytes read for the addresses the
step before it issued, and those addresses come from the state before.
A core frozen, rewritten and resumed would take stale bytes. So the
state also holds each memory's **last read address(es)**. On resume the
core spends one held clock ("the prime") with every block RAM reading
its saved address, and its first real step sees what it would have
seen. That costs an address mux per RAM port and a register per
address.

**The freeze:** the core's clock enable off at a point where
- no ROM read is in flight (the ROM port idle: nothing asked or
  wanted);
- no DDR answer is due;
- the master clock is between ticks.

Then no master clock passes and no step is taken; the menu's pause
already holds the machine there.

**The state space:**
- the records as bytes (`asBytes`, `fromBytes`);
- the memories, through their existing ports while frozen: WRAM, SRAM
  (its PS port), the DMA registers, VRAM (both chips), CGRAM, OAM's
  lanes, the PPU's object banks and column ring, ARAM, the DSP's
  samples;
- the read addresses.

It is larger than 64 KiB, so the SNES gets its own 19-bit windows,
reserved in ARCHITECTURE.md first: the pad-2 address (0x160000) clashes
with the usual windows. A slot is about 260 KiB, a second or so each way
over the link.

**The steps:**
- **7.14a**, in the pure model (the lockstep harness's composition of
  the Clash parts with memory models): freeze at a clock, snapshot,
  build a fresh machine from the snapshot, prime, continue. Gate: the
  copy continues exactly as the original, at random freeze points on
  several ROMs (a new suite).
- **7.14b**, the circuit: `Snes.StateHooks` (the freeze, the windows,
  the read tap, the write path, the prime), in the Arty's top only (the
  ZedBoard has no state hooks, `state_board.zig`). Gate: a layer 4
  chain `ci-savestate-snes`; LUTs and timing measured.
- **7.14c**, the PS side: `platform/ps/systems/snes_state.zig` (the
  sections, `restore = null`), the menu's rows on the Arty. Gate: `make
  test-zig`, the PS app's size and stack checks.
- **7.14d**, the board check: save, play on, load (the user).

**As built (7.14a and 7.14b, 2026-10-23); changes from the plan:**
- **No address prime: a latch of the bytes in flight.** Every register
  of the core is one record (`Snes.Core.CoreRegs`, 1101 bytes), and the
  bytes the memories return for the step after the freeze
  (`Inflight`: WRAM, SRAM, the DMA registers, the PPU's memories, ARAM
  for the SPC700 and the DSP, the IPL ROM, the samples, the Gaussian
  table) are latched into it on the first frozen clock. On the first
  clock after, the step takes them from the latch. So the in-flight bytes
  move with the state, and the RAM ports need no address mux.
- **The record moves as a chain.** While frozen, each access shifts it a
  byte (in at the bottom, the top byte out on `scTop`). The PS reads it
  once round, in order, and writes it the same way. There is no address
  decoder over 1101 bytes. The register holds the record's bits, not
  the record: in Haskell's simulation a record between two shifts drops
  the bits that lie under a `Nothing`, so a round came back with undefined
  bits. The HDL is the same flat register either way. Found by the
  stronger snes-savestate (below), which threw on them; 7.14a's first
  pass did not look there.
- **The freeze** is at rest (`atRest`: the ROM port and its read
  register idle) **in V-blank**. The PPU's line buffer and column ring
  are not in the state: they are out of use there. The state hooks let
  the core run on, despite the menu's pause, until it gets there (within
  a frame).
- **The state space** (ARCHITECTURE.md, reserved 2026-10-23): the
  windows 0x200000 (write) and 0x300000 (read); the layout is in
  `Snes.StateHooks`.
- **Gates.**
  - snes-savestate (FULL tier, about 50 minutes a ROM in Clash's
    simulation): one core's state moved into a fresh one, which then
    agrees with the original on every output for 100,000 clocks, on
    tone.sfc, Mode7HDMA.sfc and shmup_1942.sfc. Then both freeze again
    and their whole states are compared (the chain, every memory byte).
    That comparison was added after two mutants (the latch not taken,
    ARAM not moved) passed the outputs-only version: at tone.sfc's early
    freeze the SPC700 is still in its boot ROM, so 100,000 clocks show
    neither. They passed the state comparison too, because in Haskell's
    simulation the freeze wiped A's ARAM. While the chain shifts the
    record is garbage, its addresses can be undefined, and Clash's model
    of a true dual-port RAM makes the whole memory undefined on such an
    access. `Snes.Core.simOp` (simulation only, as `simDefined`) reads an
    undefined address as 0, as Verilator has it; the HDL is unchanged.
    With that fixed, the ARAM mutant fails (297 clocks of 100,000 differ
    after the resume). The latch mutant still passes, and it is
    equivalent here: when the last frozen clock has no port access, every
    RAM reads at the core's own addresses, so the first clock after has
    the right bytes anyway. The latch stays as the guard for a thaw right
    after a port access (the PS's thaw is a link write of its own); no
    test tells them apart.
    The suite now counts each memory's nonzero bytes and fails if A's
    ARAM or its sound is empty on a ROM with sound, or its VRAM or CGRAM
    on a ROM with a picture. The freezes moved past each ROM's start-up:
    tone.sfc at frame 2 (88 ARAM bytes, 149 of about 150 samples nonzero
    after the resume), Mode7HDMA.sfc and shmup_1942.sfc at frame 5 (VRAM
    12,283 and 4,175 nonzero bytes); about 50 to 75 minutes each.
  - Layer 4, `ci-savestate-snes`: the hooks through the link on the
    Arty's top.

## Save states through the CPU (step 7.15; planned 2026-10-26)

**Why** (the user, 2026-10-26): the direct path costs the Arty about
10,000 LUTs (48.8 % to 67.9 %), and the Super FX 2 and later chips may
need that room. So the SNES gets side N's other pattern as well, and a
build switch picks one.

**The user's decisions (2026-10-26):**
- **A build switch:** `SNES_STATES=direct` (the default, as now),
  `cpu`, or `none`. The direct path stays the default until a chip needs
  the room; that build then changes the switch.
- **The DSP's own record is written directly** (its envelopes, sample
  positions, BRR buffers, echo position and counters): a write path for
  that record only, so sound resumes exactly. Everything else goes
  through the two CPUs' routines.
  - **Changed (the user, 2026-10-27): the whole sound module's record is
    written directly** (3,418 bits: the DSP's 2,514, the SPC700's 119,
    the timers, ports, its clock against the master clock, and the
    sample in work with its pending writes). That is about 900 bits more
    than the DSP's alone. In exchange there is no SPC700 routine and no
    second overlay. A save needs no SPC700 instruction boundary and no
    idle sound module. The sound module is held from the load until the
    65C816's overlay goes off, then runs on from the saved record.
- **Exact, else refuse:** the routine ends on the saved master clock (the
  line and the dot), as the NES's; a state the generator can't place is
  refused before the game is touched.

**The PL (cpu build):**
- **The state table**, read only: the record's bytes through one byte
  mux (a tap, as `Nes.StateTap`), at the chain's place in the state
  space. No shift, so no mux per bit.
- **The DSP's record**, writable: a short chain of its own (only the
  DSP's bits), or byte writes into it.
- **The memories** through the PS's ports, as now.
- **The 65C816's ROM overlay:** 2 KiB of block RAM answering the CPU's
  reads of its reset vector and code bank while armed; it ends after
  the routine's last instruction (the end address and arm bytes, as the
  NES's at 0x1F10 to 0x1F12).
- **The SPC700's overlay:** its 64-byte boot ROM area replaced by a
  stub that jumps to the SPC700's routine, written into ARAM. The bytes
  the routine borrows there are kept in the slot and put back by the
  routine (side N's "fills").
- **The load:** held (the core's reset), the memories and the DSP's
  record written, both overlays written and armed, the hold released.
  Each CPU then runs its routine.

**The routines (generated on the PS, in Zig, from the slot's table):**
- **65C816:**
  - the PPU's registers, including the write-twice latches (scroll,
    mode 7) and the OAM, VRAM and CGRAM addresses, with their internal
    flip-flops;
  - the 5A22's (multiply and divide results, H and V IRQ positions,
    NMI and IRQ flags, the WRAM port address);
  - the CPU ports to the SPC700;
  - then a wait to the saved position;
  - last, its own registers, through an RTI from the overlay's stack
    page.
- **SPC700:** the DSP's registers (where the direct write leaves any),
  the timers' targets, control and stage counters, its side of the CPU
  ports, then its own registers (PSW and PC through RETI).
- **Timing:** the generator models each instruction's master clocks
  (the 6, 8 and 12 clock accesses by region and FastROM). It schedules
  the 65C816's last cycle on the saved H and V, and the SPC700's on its
  saved clock relative to it.
- **States only at rest in V-blank:** no DMA, no HDMA mid-frame (it
  starts again at line 0), the pads' auto-read done.
- **Limits:** written down as the generator finds them, as side N did.

**Budget risks:**
- The PS app's OCM: the NES's generator alone is about 7.6 KiB, and the
  SNES's is larger. If it doesn't fit, the routine is built on the
  desktop (`make` writes it to the card per slot), or a part of it is
  moved to DDR.
- LUTs: the tap, the DSP's write path and the overlays are estimated at
  about 2,000 to 4,000, measured at 7.15c.

**The steps:**
- **7.15a**, the spec: `Snes.StateTable` (the table's layout from the
  record), the 65C816's generator as a host tool (`snes-restore`), and a
  restore test in the spec. Save from the spec, restore through the
  routine run in the spec, then compare the state field by field and
  the next frames. Mutants of the generator must fail.
- **7.15b**, the sound module's direct record in the same test, on ROMs
  with sound (the samples after the restore, the SPC700, ARAM compared).
- **7.15c**, the circuit: the tap, the overlays, the DSP's write path,
  the switch; the table in lockstep with the spec's; a layer 4 chain
  (`ci-savestate-snes-cpu`); LUTs and timing.
- **7.15d**, the PS: `Spec.restore` with the build and the fills, the
  OCM and stack checks.
- **7.15e**, the board check (the user).

**As built (7.15a, 2026-10-27).** Differences from the plan:
- **The table** (`Snes.Spec.SaveState`; 133 fields of 4 bytes, 532 bytes;
  golden `test/golden/state-table.txt`) holds:
  - the 65C816's registers and interrupt lines (`cpuLines`, new in
    `W65c816.Spec`);
  - the 5A22's and the PPU's slots, and the PPU's registers;
  - STAT77;
  - **the master clock mod 8**. A DMA starts on a multiple of 8 clocks,
    and an odd-field frame without interlace is 4 clocks short, so the
    same line and dot come at either phase. Without this field, runs
    parted by 4 clocks about 50,000 instructions after a restore.
- **The overlay's low window** ($0000-$1FFF) maps mod $800 **with bit 10
  inverted**. A stack near $1FFF would otherwise put its bytes on the
  vectors.
- **The routine ends with PLB, PLP, `JML [ptr]`** (not RTI). Only DBR and P
  sit at the saved stack, and the pointer is in the code. D and Y come
  first, by PLD from q.
- **OAM's address** is moved on after V-blank's reload by **a DMA of
  $2138 reads** (B bus to A, A address $2100, where a DMA writes nothing),
  with channel 0 put back from the slot's DMA bytes. Writing OAM's own
  bytes again was 26 lines too slow for games that DMA OAM in V-blank.
- **The CGRAM latch** keeps its byte after a colour's write, so it is set
  whether or not the flip-flop is.
- **Limits** (left out of the comparison):
  - the VRAM read latch, open bus, STAT77's sprite flags;
  - HDMA's per-frame channel flags;
  - the pads' read-back;
  - the data ports' and address registers' bytes *as written* (2104h,
    2116h-2119h, 2121h, 2122h; the addresses themselves are put back).
- **A state the generator can't place is refused:**
  - in STP;
  - a stack byte on a vector the routine needs;
  - the multiplier/divider results from no single operation;
  - no schedule.
- **The sound module** is moved whole (`moveApu`): the board's direct
  write, made after the routine as the hardware holds the module until
  then. Its clock's phase is rebased (`sApuPhase`, `sApuBase`: 0 from
  power-on, no change otherwise). **7.15b** (2026-10-27): the test also
  compares the samples made after the restore, the SPC700's registers
  and ARAM. All 39 cases pass. Mutants: the phase not rebased fails 6
  cases; the module not moved fails 3 (the ROMs with a sound program).
- **Gate:** `snes-spec`, "loading through the CPU" — 17 ROMs × 2 moments
  (OpenSNES and krom, games among them): the table field by field, every
  memory, 100,000 instructions alike, the next frames' pictures. All 35
  pass; the generator's mutants (see the log) must each fail.

**As built (7.15c, the circuit; 2026-10-28):**
- **The switch:** `SNES_STATES=direct` (the default), `cpu` or `none`, in
  the SNES's `.mk` files (`CLASH_EXTRA`; the Verilog's cache key holds
  it). Haskell's side is `cabal -f states-cpu`.
- **The tap** (`Snes.StateTap`): the table's 130 fields from the core's
  registers. Its window is 552 bytes: the table, then OAM's high table
  (registers in the circuit, so the routine writes it back through
  2104h).
  - snes-core checks it against the spec's table at every frame's rest
    moment, at the instruction's end on the same master clock.
  - At an instruction's last clock (`StDone`) the core hasn't applied
    the write-back yet. The first tap applied `finishOp` itself, which
    copied the CPU's ALU (about 4,500 LUTs). Instead, the CPU build's
    freeze comes after **a clock with no master clock**
    (`StCtl.stFinish`, as a pace stall). In it the instruction finishes,
    with time still at its end.
- **The overlay:** a new source (`SrcOvl`) in the 5A22's single address
  decode. It answers bank 00h's 0000h-1FFFh (bit 10 inverted) and
  8000h-FFFFh from a 2 KiB block RAM while armed. Its arm and end
  address live in the hooks, outside the core's reset. It turns off once
  the instruction that read the end address has ended (`scOvlDone`).
- **The sound module** has a register of its own in the CPU build, with
  a clock enable for the hold (from the load until the routine's last
  clock) and for the freeze. Its chain is that record plus 6 pad bits:
  428 bytes. The first version held and shifted it inside the shared
  record's register and cost two LUTs a bit.
- **The freeze** (`atRestCpu`): at rest, the CPU at an instruction's
  end, not stopped, no DMA owed, the multiplier idle, the pads' read
  done, no interrupt seen or owed, in V-blank; at once while held (a
  load).
- **LUTs (Arty, 2026-10-28):**

  | Build | LUTs | Above the base | WNS |
  |---|---|---|---|
  | none | 26,865 (50.5 %) | — | +0.459 ns |
  | cpu | 31,306 (58.9 %) | +4,441 | +1.920 ns |
  | direct | 36,112 (67.9 %) | +9,247 | +0.048 ns |
  | direct, 2026-11-02 (the pace's resync, the table's port) | 37,256 (70.0 %) | +10,391 | +1.287 ns |

  (The cpu row predates the pace's fix. Answering the table's port with
  0 in the direct build, which never asks for it, gave **40,867 LUTs**
  (76.8 %, one DSP more). That was reverted; the same source built
  twice gave 37,256 both times.)

- **A simulation fix:** the overlay's block RAM answers undefined before
  its first read in Clash's simulation, and the 5A22 took that in
  (`simByte`, as `simDefined`).
- **The CPU build's state space (7.15d, 2026-11-02):**
  - 0x00000 holds the record: the table window, 552 bytes (the table,
    then OAM's high table), read only. The status gives it as the
    record's size. savestate.zig's loads through the CPU take the record
    as the table.
  - 0x00800 holds the chain, the sound module's record (428 bytes). It is
    a section of the PS's Spec (`snes_state.spec_cpu`), written back as it
    was read.
  - The overlay is at 0x06000 and its arm at 0x01F10, as before.
- **A load through the CPU at layer 4 (7.15d, 2026-11-06;
  `ci-savestate-snes-cpu`'s second run, `test/ps/load-cpu.ps`):** in the
  PS app's order, the game frozen, then held. The overlay gets a small
  routine (LDA #$42, STA $0310, JMP to a loop written into WRAM), with
  the reset vector and its end armed. Then the core is let go, frozen
  again, and WRAM read back: 42, and the loop.
  - **Found:** `Snes.Machine` never resets a frozen core when it is held
    (the direct build writes its state into the frozen core). So the
    CPU build's routine never ran, and the frozen game simply went on.
    Clash's gate (`LoadCpu`) loads a fresh core, frozen from its
    power-on, so it couldn't see this.
  - **The fix (the CPU build only):** a hold owes the core one reset,
    frozen or not. It lasts a clock or two, and the state the PS writes
    after it stays.
- **The pace after a hold (both builds; found at layer 4, 2026-10-29):**
  - The bug: after the menu's pause or a save, the core was a frame or
    more behind the output, counted mod 4. It ran at a master clock a
    pixel clock (17% fast) for up to three frames to catch up, and the
    tone came out at 1025-1059 Hz instead of 997.4. The direct build's
    chain passed only because its thaw came late in the run.
  - The fix (`Snes.Display`): while held, the output's frame count is
    kept level with the core's, so after a hold the core waits for the
    output instead. The picture is black until the core's first frame
    in step (`pcBlank`), and the run-on to rest is paced too.
  - The check: snes-spec, "after holds", where the old pace takes
    420,000 master clocks in a 357,368 frame. The core alone was never
    wrong: a CPU-build freeze and thaw in Clash's simulation gave the
    same samples as no freeze.

## The DSP-1 (step 7.16; planned 2026-11-03)

The ladder's first rung (below): the NEC µPD77C25 of Super Mario Kart
and Pilotwings, as a bitstream of its own (`snes-dsp1`, the Arty's),
the base console plus the chip.

**The user's decisions (2026-11-03):**
- **The firmware** is the user's own dump: `local/firmware/DSP1 B
  (Enhancement Chip).bin`, 8,192 bytes, git-ignored. It holds the
  program ROM (2,048 × 24 bits) and the data ROM (1,024 × 16).
  - It is never fetched or committed. Tests use our own µPD77C25
    programs and skip the firmware's cases without it.
  - The user allows a clone of the retrobios repository
    (github.com/Abdess/retrobios) into a git-ignored folder for other
    firmware, such as the original DSP-1 for Pilotwings.
- **Save states:** yes, direct, as the base console's 7.14. The
  µPD77C25's registers join the chain, and its RAM (256 × 16) is a
  section.
- **The board check:** Super Mario Kart and Pilotwings. Neither game is
  in `roms/snes` yet: the user's own files, into `roms/snes/local`.

**The chip** (to verify at 7.16a against NEC's µPD77C25 data sheet and
fullsnes; bsnes's uPD96050 core only as a check of values, never code):
- **Speed:** 7.6 MHz, one instruction a cycle: one about every 2.83
  master clocks, as a fractional enable on the master clock.
- **Registers:** the accumulators A and B with their flags, K and L into
  the 16 × 16 multiplier (M and N out), TR and TRB, DP and RP, the
  4-level stack, DR and SR.
- **The instructions:** OP and RT (ALU plus moves), JP (jumps and
  calls), LD (immediate).
- **The SNES's side** is the data register (DR, 8 or 16 bits) and the
  status register (SR; RQM, DRC, DRS) at the cartridge's addresses:
  - LoROM: banks 30h-3Fh, 8000h-BFFFh DR and C000h-FFFFh SR (the
    larger ROMs' map to verify);
  - HiROM: banks 00h-1Fh, 6000h-6FFFh DR and 7000h-7FFFh SR.
- **Budget:** 2,000 to 4,000 LUTs and 2 to 3 block RAMs against the
  Arty's base, 37,256 LUTs (70.0 %) with direct save states. About 75 %
  in all.

**Steps:**
- **7.16a, the chip's spec** (`Snes.Spec.Upd77c25`):
  - every instruction from the data sheet, tested with our own
    programs (a small assembler in the tests);
  - with the dump, the DSP-1's commands checked against fullsnes's
    descriptions (multiply, inverse, the rotations).
- **7.16b, the cartridge in the spec:**
  - the DSP-1's mapping from the header's type byte ($FFD6) and map
    mode, and the firmware file;
  - the two games run on the spec, on the desktop (side Q's emulator):
    Super Mario Kart's first race, Pilotwings' first lesson.
- **7.16c, the circuit** (`Snes.Upd77c25` in Clash):
  - in lockstep with the spec (as snes-core);
  - out-of-context LUTs and timing.
- **7.16d, the bitstream** `snes-dsp1` (the Arty's top, with the hooks):
  - its own layer 4 chain with our own test program;
  - `make bit` meets timing.
- **7.16e, the PS side:**
  - the bitstream chosen by the cartridge's type byte;
  - the firmware from the card into the chip's ROMs (written by the
    PS before the hold falls);
  - the save states' Spec.
  - Choosing a bitstream per cartridge is new for the generic PS app:
    a request to side K.
- **7.16f, the board check** (the user): Super Mario Kart and Pilotwings
  on the Arty.

**As built (7.16a, 2026-11-03):** `Snes.Spec.Upd77c25`, pure (the
registers in a record, RAM a vector), with `firmwareOf` for the three
dump formats.
- **The gate:** snes-spec, "the DSP-1's chip": 23 cases of our own
  programs, and the user's dump's commands through DR and SR. All 26
  pass.
  - 2Fh gives 0101h (DSP-1B), and 00h multiplies.
  - 1Fh sends back the data ROM word for word.
- **Found on the way:**
  - 2Fh and 1Fh take one input word before they answer: fullsnes's
    list doesn't say so.
  - The DSP-1 reads its command with SRC DR, which sets RQM before it
    has switched DR to 16-bit words. Games rely on the 65C816's pace:
    the test's SNES side runs the chip 12 instructions after each
    access, about one cartridge access's 8 master clocks.
- **The firmware cases need the dump on the machine:** the user copies
  it to the worker's `local/firmware` (wrun doesn't sync `local/`). The
  cases are skipped without it.

**As built (7.16b, the spec's part, and 7.16c, 2026-11-03):**
- **The cartridge in the spec:**
  - `Snes.Spec.Map.dspWindow` gives fullsnes's DR and SR windows (by
    map mode and SRAM). It's a function of its own, so side Q's fast
    machine, which uses `decode`, is unchanged.
  - `attachDsp` gives the machine the chip. It is caught up at each
    access to 1672 / 4725 instructions a master clock (7.6 MHz against
    236.25 / 11 MHz).
  - `snes-run --dsp1 FILE` runs a DSP game. snes-spec: 178 of 178.
- **The circuit** (`Snes.Upd77c25`): a pure function a clock. The
  memories are outside, read at the state's addresses. An instruction
  runs once their bytes are in: two clocks at least, against one every
  2.83 master clocks.
  - **Its lockstep test** checks every register and RAM after each
    instruction against the spec: 32 random programs (16 straight, 16
    with jumps) from random starts, and the user's firmware. Five
    mutants (S1, KLM, DPH, the stack's bottom, SBB) each fail it.
  - **Out of context** (`Snes.Upd77c25Ooc`, the Arty's part at
    25.175 MHz): **639 LUTs**, 270 registers, 2.5 block RAMs, 1 DSP,
    WNS +25.758 ns. That is far under the plan's 2,000 to 4,000.

**As built (7.16d, the chip in the core; 2026-11-03), the `SNES_DSP1`
build** (`SNES_CHIP=dsp1`, cabal `-f dsp1`):
- **The parts:**
  - `Snes.Dsp1` (the chip's clock, the firmware's load, the 5A22's
    accesses), its state in the core's record (so in the direct save
    states' chain).
  - `SrcDsp` in `Snes.Bus`: an access waits until the chip has caught
    up.
  - The firmware read through the ROM port from offset 700000h, which
    `Snes.Machine` maps to the companion's DDR address 0x02700000.
- **Our own test cartridge** (`dsp1.s`, type 03h) and firmware
  (`snes-dspfw`, `Snes.Spec.Upd77c25Asm`): 64 rounds of X + Y, X × Y × 2,
  X XOR Y, then a timing probe that reads SR 200 times while the chip
  counts into it.
  - On the spec: RESULT 01h, and the products as Haskell computes them.
  - snes-core (`-f dsp1`): the circuit against the spec for 8 frames,
    every bus event alike. The chip's clock off by 1 in 1,672 fails it.
- **Found:** the access's wait for the chip never triggers as built.
  Instructions fall due at least two clocks apart, and the chip runs one
  in two, with an access after any instruction of its clock. Without the
  wait the lockstep still passes. It is kept as a guard (it costs
  nothing when idle).
- **Layer 4** (`make ci-arty-snes-dsp1`, 2026-11-04): our cartridge with
  our firmware, the backdrop green as on the spec (golden/vga/dsp1.png).
  - The harness's DDR takes one `--ddr-file`, so the ROM and the
    firmware go in as one image: `ddr-dsp1.bin`, made by
    `systems/snes/arty-z7-20.mk`; `SNES_DSP_FW` picks the firmware.
  - The 5A22 is held until the firmware is in. Otherwise its own ROM
    reads would share the ROM port with the load.
  - **Found, three bugs that only the Verilog had** (Clash's Haskell
    simulation was right each time):
    - `dsLoad d == LDone`, a derived Eq on a sum type, compared LDone's
      unused payload bits too. Those bits are undefined in the HDL, so
      the load never ended. The fix: a case on the tag.
    - The block RAMs answer undefined before their first read in
      Haskell's simulation. `SNES_SIM_STRICT`'s guards (`simOp24`,
      `simWord`, `Snes.Upd77c25Ooc`'s `simDef`) make them 0, as
      Verilator has them.
    - **A Clash 1.8.5 bug:** disjoint expression consolidation shared
      `snesAcc`'s `clrB bRqm sr` (the DRC branch) with the
      `clrB bDrs sr` inside the high byte's `clrB bRqm (clrB bDrs sr)`.
      It chose the bit by DRS, so the high byte's access cleared RQM
      twice and DRS never. Every 16-bit transfer after the first was
      then a byte off. The fix: SR's bits by constant masks.
  - **The bitstream** (`make bit SYSTEM=snes SNES_CHIP=dsp1
    BUILD=build/arty-z7-20/snes-dsp1`, 2026-11-06): **45,699 LUTs
    (85.9 %)**, 88 block RAM tiles, 17 DSPs, WNS +0.089 ns. That is
    8,443 LUTs above the direct build's 37,256, against the chip's 639
    out of context and the plan's 2,000 to 4,000.
  - **Measured with the hierarchy kept** (`make snes-ooc
    SNES_OOC=Snes.MachineOoc SNES_OOC_TOP=snes_machine_ooc
    SNES_OOC_MODE=hier`, with and without `SNES_OOC_EXTRA=-DSNES_DSP1`;
    the chip's step is a row of its own, `dspUnit`; 2026-11-06):

    | Row | Base | DSP-1 |
    |---|---|---|
    | the whole machine | 42,684 | 42,110 |
    | sound module (`apuUnit`) | 19,841 | 19,943 |
    | PPU (`ppuUnit`) | 11,443 | 9,839 |
    | 5A22 (`busUnit`) | 9,002 | 8,653 |
    | the DSP-1 (`dspUnit`) | — | 1,110 |
    | the rest (memories, hooks, platform) | 2,395 | 2,567 |
    | block RAM tiles, DSPs | 85.5, 15 | 88, 17 |

    So the chip costs about 1,100 to 1,300 LUTs, 2.5 block RAMs and 2
    DSPs, within the plan. The flat build's +8,443 is Vivado's
    optimisation of the whole moving, not the chip's logic: rows whose
    code didn't change move by up to 1,600 here, and 7.15c saw a 3,600
    swing from an unrelated change. The flat number is what the
    bitstream must fit, so the later chips' budgets should keep
    about 8,000 LUTs of margin.
  - **How the last one was found:** a Verilator harness of
    `Snes.Upd77c25Ooc` (throwaway) and the same inputs in Clash's
    simulation. The test `Snes.Upd77c25Ooc in Clash` keeps that round
    (snes-spec). A per-clock diff of PC, SR, DR and A, through a debug
    port added for the run, showed the first state that differed.

## The ladder after the base console (outline; each rung its own plan)

Every chip is its own bitstream (`snes-dsp1.bit`, `snes-sa1.bit`,
`snes-fx.bit`: the base console plus the chip), so a chip never costs
the base console anything. The PS picks the bitstream from the
cartridge type byte (`$FFD6`). A chip bitstream can drop something
from the base console if it has to (hi-res, say), and the user decides
that. Estimates only. Each rung starts by measuring the base console
as built.

| Rung | What | LUTs | Block RAM | Timing | Firmware |
|---|---|---|---|---|---|
| **DSP-1** (Super Mario Kart, Pilotwings) | NEC µPD77C25 at 7.6 MHz: 16-bit DSP, one instruction per cycle, 16 × 16 multiplier; program ROM 2048 × 24, data ROM 1024 × 16, RAM 256 × 16 | 2,000 to 4,000 | 2 to 3 | about 3.3 pixel clocks per instruction: easy | **copyrighted**: the user's own dump on the card (as the Master System's BIOS), never fetched or committed; tests use our own µPD77C25 programs |
| **SA-1** (Super Mario RPG, Kirby Super Star) | a second 65C816 at 10.74 MHz (the same `W65c816.Core`), 2 KiB I-RAM, BW-RAM, its memory mapper (up to 8 MiB? *to verify*), DMA with character conversion, a 40-bit multiply/divide/sum unit, timers, interrupts between the CPUs | the measured 65C816 plus 3,000 to 6,000 | 1 + BW-RAM: up to 32 KiB in block RAM (8); more would need DDR with writes (K's interface) | one SA-1 cycle per 2 master clocks: 2 pixel clocks when catching up; its ROM reads share the DDR cache, so misses stall both CPUs and come 4 times as often | none |
| **Super FX / GSU-2** (Yoshi's Island, Star Fox 2; the GSU-1 in Star Fox at 10.74 MHz) | 21.477 MHz RISC: 16 registers, a 512-byte instruction cache, two pixel caches with bitplane conversion (PLOT, RPIX), 8 × 8 and 16 × 16 multiplies, ROM and RAM buffers; Game Pak RAM 32 to 64 KiB (*to verify*) | 6,000 to 12,000 | 8 to 16 (its RAM), 1 (cache) | **the likely limit**: one GSU cycle per master clock, so at the catch-up rate it needs a cycle per pixel clock, with 17 % headroom; its ROM reads (textures) are random, so DDR misses of about 15 clocks against the real chip's few cycles. The way out, the whole system in a faster domain (2 × `DomPix`, a dual-clock line ring at the platform boundary), needs every path to meet about 20 ns: our Clash style has 37 ns paths (the Game Boy's). Measure the GSU's out-of-context speed first | none |
| **MSU-1** (the user, 2026-10-16; homebrew and ROM hacks with CD-quality music) | a virtual chip (from the SD2SNES cartridge): a few registers at $2000-$2007 (*to verify*: the data file's seek offset and read port, an audio track number, volume, play and repeat, a status byte with busy flags, an ID string), a data file (`.msu`) read byte by byte, and audio tracks (`-N.pcm`: 44.1 kHz 16-bit stereo with a loop point) mixed with the S-DSP's output | small: hundreds to about 2,000 (the registers, a 44.1 to 48 kHz step, the mix) | 1 or 2 (stream buffers) | easy in the PL; **the work is on the PS**: the files stream from the SD card on demand while the game runs (a seek request from the PL, the PS filling a DDR ring), a new service in the generic PS app (side track K's), and a data file can outgrow DDR's media region | none (the game's own files) |

**Measured 2026-10-16 (the base console's first `make bit`, ZedBoard):
44,599 LUTs, 83.8 %**, against 68 to 70 % projected from the four
blocks alone (31,000; the rest is the platform and the machine's glue).
The platform is the smaller part: CHIP-8's whole bitstream (a tiny core
on the same platform) is 3,604 on the ZedBoard and 3,337 on the Arty,
so about 9,000 is the Super Nintendo's own glue (the display ring, the
ROM port, the sound module's I/O, the blocks' in-context cost), in M7's
files; and the Game Boy Color runs on the board at 48,985 (92 %).
That leaves about 8,600 LUTs beside the base, less in practice
(routing and timing get hard above 85 to 90 %): enough for the DSP-1
and the MSU-1 as things stand, marginal for the SA-1, not for the Super
FX. The plan (the user, 2026-10-16): finish the base console, then
measure where the rest goes (the Arty's build, another system's on the
same board as the platform's baseline), then a go/no-go per rung; the
SA-1 and the Super FX likely need a "diet" step first (the base to about
75 %; the S-DSP, the 5A22 and the PPU the candidates, every feature
kept).

## References

- **fullsnes** (Martin Korth, nocash; https://problemkaputt.de/fullsnes.htm):
  the whole machine, the cartridge chips included. The main reference.
- **anomie's documents** (anomie's register, timing, memory map, SPC700
  and S-DSP notes; mirrored on romhacking.net and the SNESdev wiki):
  cycle timing, the DSP's per-sample schedule.
- **The 65C816:** the Western Design Center's W65C816S data sheet
  (https://www.westerndesigncenter.com/wdc/documentation/w65c816s.pdf):
  the cycle tables (VDA/VPA per cycle), the M/X and E rules; "Programming
  the 65816" (Eyes and Lichty, WDC) for the instruction set.
- **The SNESdev wiki** (https://snes.nesdev.org/wiki/) and the Super
  Famicom development wiki (https://wiki.superfamicom.org/): registers,
  timing, the test-ROM lists (https://snes.nesdev.org/wiki/Emulator_tests).
- **The SPC700 and S-DSP:** fullsnes, anomie's DSP and SPC700 documents;
  blargg's `snes_spc` library (LGPL-2.1,
  https://github.com/yupferris/snes_spc) only as a black-box oracle in
  a test, if the user agrees, as Nuked-OPLL was for the Master System's
  FM unit (fetched, never linked into our code, values only).
- Emulators (bsnes/higan/ares, Mesen 2, snes9x) only to cross-check
  behaviour and values, never code.
- Nintendo's development manual (leaked) is not used.

## Test ROMs (checked 2026-10-15; none committed, all fetched by `tools/fetch-roms.sh` with SHA-256 pins)

| Suite | What it tests | Pass signal | Licence | Use |
|---|---|---|---|---|
| **SingleStepTests 65816** (https://github.com/SingleStepTests/65816, pin `9bf6b708bd82`, 2026-10-15) | every opcode in emulation and native mode: 512 JSON files of 10,000 tests each, initial and final state and every bus cycle (`[addr, value, "dp-remx-"]`), a flat 16 MiB address space | our harness compares state and cycles | MIT ("Copyright (c) Thomas Harte, et al") | `W65c816.Spec` (7.1) and the core's lockstep (7.7). **2.8 GB** (5.4 MB a file), fetched once into the shared `roms/` as the Z80's (step 7.0); the quick tier checks the first 100 tests of each file, FULL all |
| **SingleStepTests SPC700** (https://github.com/SingleStepTests/spc700, pin `67d15f492b27`) | every opcode: 256 JSON files of 1000 tests, state and cycles (`[addr, value or null, kind]`) | as above | MIT (2024 SingleStepTests) | `Spc700.Spec` (7.2), the core (7.8) |
| **gilyon's snes-tests** (https://github.com/gilyon/snes-tests, release v1.4, `snes-tests.zip`) | `cputest-full.sfc` / `cputest-basic.sfc`: every 65C816 opcode but STP and WAI in every mode, the emulation-mode wrapping; `spctest.sfc`: every SPC700 opcode but SLEEP and STOP. Not cycles, interrupts, I/O or the DSP | "Success" on screen ("Failed", the test number and A, X, Y, P otherwise) | MIT (2023 gilyon) | spec 7.3/7.4, core 7.9, **exit** |
| **krom's SNES tests** (PeterLemon, https://github.com/PeterLemon/SNES) | `CPUTest/CPU`: 23 instruction groups (ADC to TRN), `CPUTest/SPC700`; `PPU/`: BG maps, blending, HDMA, interlace, Mode 7, mosaic, windows | "PASS"/"FAIL" per test on screen (CPU); the PPU ones by eye, then frozen goldens | **no licence** (all rights reserved by default): fetched for local tests only, never redistributed (as nestest, nes.md) | spec 7.3 to 7.5, core 7.9 |
| **undisbeliever's snes-test-roms** (https://github.com/undisbeliever/snes-test-roms, pin `ac6ef8006809`) | hardware and glitch tests: PPU, DMA and HDMA timing, VMAIN remapping (sources; we build them with their assembler *to verify*) | per test, mostly on screen (*to verify*) | zlib (2016 Marcus Rowe) | a pick at 7.5, frozen goldens |
| **Our own** (`systems/snes/test/asm/`, WLA-DX's `wla-65816` and `wla-spc700`, already in `flake.nix`) | the mapping, the access speeds, multiply/divide timing, DMA/HDMA, the timers, auto-read, SRAM; a tone ROM; an SRAM counter (`sram.s`) | a result byte in WRAM (`$7E0000` = `$A5` passed, `$5A` failed, as our other ROMs) and on screen | ours | every step |
| blargg's SNES and SPC tests (`blargg_2010-03-14.zip`; spc_dsp6, spc_timer...) | timers, DSP, speed | | **not free** where found (snescentral: "do not use without permission"); the SPC ones not found | not used |
| The 240p test suite (SNES, GPL-2.0) | a display evaluation suite, not accuracy | by eye | GPL-2.0 | optional, by eye on the board |

### The exit game, and the battery save

No freely licensed SNES game we found (2026-10-15) uses battery SRAM.
The candidates, licences read from their repositories:

| Candidate | Licence | Header | Note |
|---|---|---|---|
| **OpenSNES examples** (https://github.com/k0b3n4irb/opensnes, release v0.50.0, `opensnes-examples_v0.50.0.zip`): `tetris`, `shmup_1942`, `breakout`, `likemario`, `mode7_racing` | MIT (OpenSNES contributors, Alekmaul's PVSnesLib); `shmup_1942`'s art Kenney's Pixel Shmup, CC0 (its ATTRIBUTION.md); `tetris`' tiles generated, **its music's origin not stated** (*to verify*) | LoROM, 256 KiB, no SRAM | small but real games; `mode7_racing` exercises Mode 7 |
| **OpenSNES `save_game.sfc`** (same release) | MIT | LoROM, type `$02` (ROM + RAM + battery), 8 KiB SRAM | an API demo: A, B, X, Y write and read two save slots. **The battery-save check** |
| undisbeliever's unnamed-snes-engine, tech-demo-alpha-v3 | code MIT, art and music CC BY-SA 4.0 (KungFuFurby's music), samples mixed free licences (CREDITS.md) | LoROM, 256 KiB, no SRAM | a one-screen game demo with real music: a sound test |
| undisbeliever's horizontal-shooter v1.1 | code MIT, images CC0 | HiROM, 128 KiB | **the HiROM check** (a demo) |
| Super Apocalux V1.0b | GPL-3.0 | LoROM, 256 KiB | a thin platformer |

**Recommended:** `shmup_1942` as the exit game (every part freely
licensed, sprites and scrolling; **found at step 7.0: it has no sound**,
see the status log); `save_game.sfc` for the
battery save across a power cycle; horizontal-shooter for HiROM. Not
free (excluded): Keeping SNES Alive (no licence, NC music), the
retrobrews collection (distribution by permission only), Space Rescue
Squad (assets restricted to the project).

## What is out of scope at first

PAL; ExHiROM (more than 4 MiB) and the special mappers (S-DD1,
SPC7110: later, with the chips); every enhancement chip (the ladder);
the Super Multitap, mouse, Super Scope and the H/V counter latch through
`WRIO` bit 7 (*to decide*: it is cheap); the Satellaview and Super Game
Boy; MSU-1; PPU1/PPU2 version differences and open-bus values beyond
what the chosen tests check; a frozen picture under the menu (see
"Clock and display").

## Questions for the user (asked 2026-10-15; answered: the next section)

1. **The exit game and the required test ROMs:** `shmup_1942`
   (OpenSNES, MIT/CC0) as the exit game, `save_game.sfc` for the battery
   save, horizontal-shooter for HiROM; required: SingleStepTests (both
   CPUs), gilyon's cputest and spctest (the board's verdict = the
   model's), krom's CPU tests, a pick of krom's and undisbeliever's PPU
   tests as frozen goldens?
2. **The base console's scope:** hi-res (modes 5 and 6, pseudo-hires)
   and interlace (bob) **in** (recommended: the display path handles
   them for free; the PPU's cost is measured at the go/no-go); the rarer
   PPU features (mosaic, offset-per-tile, direct colour, Mode 7's EXTBG)
   **in** (recommended: games use them); the S-DSP's echo **in**
   (recommended: most games use it; it costs ARAM bandwidth and one
   multiplier, time-shared); pad 2 in or later?
3. **The SPC700's IPL ROM** (64 bytes of Nintendo's code, run by every
   game to load its sound program): (a) **our own clean-room IPL**
   written from fullsnes's description of the protocol, the same entry
   points and handshake (recommended; the tests use it), with (b) the
   user's own dump on the card (`/snes/ipl.bin`) used in its place when
   present, as the Master System's BIOS; or only one of them.
4. **The ladder:** the order after the exit (recommended: **DSP-1**
   first, the smallest and independent of speed, then **SA-1**, then the
   **Super FX**, where the limit probably is), and whether each rung is
   part of M7 or its own milestone (recommended: each its own milestone,
   M7 the base console).
5. **The Rota boot splash** (`demos/snes`, as M6 made the Master
   System's) and **save states** (side N's pattern: the state table,
   the rest condition, a restore routine on the 65C816 from a ROM
   overlay; the core's state in one record from the start) **as M7's
   last optional steps** (recommended: yes).
6. **The display** (a design choice with a user-visible cost): the
   line-locked display with the menu over black (recommended: it is
   what fits), or ask side K for DDR frame buffers (a write channel on
   HP0) to keep a frozen picture under the menu?

7. **(asked at step 7.0; answered: (a))** `shmup_1942` has no sound. For the exit's
   "with sound": (a) **shmup_1942 for play, and the unnamed tech demo
   (MIT code, CC BY-SA art and music by KungFuFurby) for sound**, both
   from the SD menu (recommended); (b) the unnamed tech demo alone as the
   exit game (a one-screen demo, but with music); (c) OpenSNES `tetris`
   (its music's origin isn't stated, so not cleared).

## The user's decisions (2026-10-15)

The user took every recommendation ("go with your recommendations").

| Question | Decision |
|---|---|
| Exit game | **`shmup_1942`** (OpenSNES v0.50.0, MIT, art CC0) for play; it has no sound, so **the unnamed tech demo v3** (MIT code, CC BY-SA 4.0 art and music) for sound, both from the SD menu (the user, 2026-10-15, question 7) |
| Battery save | **`save_game.sfc`** (OpenSNES, MIT, 8 KiB SRAM): its slots survive a power cycle |
| HiROM check | **horizontal-shooter** v1.1 (MIT, CC0) |
| Required tests | SingleStepTests 65816 and SPC700 (spec and core); gilyon's cputest and spctest (the board = the model); krom's CPU tests; a pick of krom's and undisbeliever's PPU tests as frozen goldens (picked at 7.5) |
| Hi-res, interlace | **in** (modes 5 and 6, pseudo-hires; interlace shown as bob) |
| Rarer PPU features | **in**: mosaic, offset-per-tile, direct colour, Mode 7's EXTBG |
| The S-DSP's echo | **in** |
| Pad 2 | **in**, over the system's write bus (no recommendation was given; Claude Code's choice, since it costs a register: say if it should wait) |
| The SPC700's IPL ROM | **our own clean-room IPL** (from fullsnes's description of the protocol; the tests use it), and the user's own dump, `/snes/ipl.bin`, in its place when the card has one |
| The ladder | **DSP-1, then SA-1, then the Super FX**, each its own bitstream and **its own milestone** after M7 |
| Boot splash, save states | **yes, as M7's last optional steps** (7.13, 7.14) |
| Save states' restore (2026-10-23) | **the direct path, as CHIP-8's** (the user): the PS writes the core's state records (the 5A22 with the 65C816, the PPU, the sound module) and every memory back into the frozen core, which then resumes. Not side N's timed restore routine on the CPU: the SNES's second CPU (the SPC700, the DSP's inner phases reachable only through four ports), its write-only PPU latches, HDMA and its memory speeds made the routine poor value. Its cost is LUTs: a write path for every record bit (estimated 7,000 to 10,000 on the Arty) |
| Display | **line-locked**, the menu over black (no DDR frame buffers) |
| Boot splash's sound (2026-10-18) | **Rota's chime (the same notes and timing as every system's splash) in the Game Boy Advance boot sound's manner** (the user), voiced as the audition page's hypothetical GBA rendition (a flute-like lead, the harmony as two string layers, a plucked bass, a short reverb; changed from the first build's bells on 2026-10-18 to keep the brand's legato rule); Nintendo's sound itself is neither copied nor sampled (step 7.13, docs/brand.md) |
| Boot splash's code (2026-10-18) | **M7 adds the SNES's parts to side track J's generator additively** (the user; J's session has finished): `Rota.Snes`, `Rota.Sound.Snes`, a SNES section in docs/brand.md, as M6 did for the Master System; J's code and the other systems' output stay as they are (their goldens must not change) |
| More rungs (2026-10-16) | **the Super FX 2 (GSU-2) and the MSU-1 wanted down the road**, after the base console; each gets its go/no-go from the LUT measurement above (the user: "go on with the base console") |

## Status log

**2026-10-21, the Rockman & Forte freeze: a CPU read lost to HDMA, with
the ROM from DDR.** The user saw the game freeze now and then, at random
moments of play, on main's build and on the smaller one alike. LD0
kept toggling, the music played on, Esc opened the menu.

**Found with a trace build** (`make hdl SYSTEM=snes SNES_TRACE=1`, then
`make bit`). It prints a line of the core's state a second on the PS
byte channel, which the PS app shows as "serial: ..." lines.
- The 65C816 sat in the game's crash trap at C0:0416 (BRA to itself).
  The COP/BRK/ABORT vector's stub (FF94) jumps there.
- A latch of the first BRK or COP fetched showed the instruction before
  it: STA $0000,X at C0:57D1, with INX next at C0:57D4. So an opcode
  fetch read 00.

**Cause:** in `Snes.Bus`, a read's source is one register (`rSrc`). The
CPU issues its read 4 ticks before its cycle ends, and those ticks
pass while the byte is on its way. From DDR that can take many
clocks. When HDMA started in those ticks, its read took the source
and the ROM port's request, and the CPU got HDMA's byte, or 0.
- The spec reads at once, so it never has this.
- The lockstep test's DDR model answered recent lines at once, so its
  runs missed it.
- Reproduced in `snes-core` with every ROM read slow: Mode7HDMA and
  RedSpaceIndirectHDMA diverged (a CPU read from ROM gave 0).

**Fix:** while a ROM read is in flight and the foreground (HDMA, the
refresh) is active, no master clock passes and its step waits. A hold
passes no master clock, so the spec's timeline is kept. A first
version held for every ROM read in flight; the core then fell behind
the display (ci-arty-snes' frames differed), since every opcode from
ROM held the machine.

**Tests:**
- `snes-core` gains three cases with the ROM from a slow DDR
  (Mode7HDMA, RedSpaceIndirectHDMA, WaveHDMA): all 34 pass.
- `make test SYSTEM=snes`: 9 of 9 suites (with the first version of
  the fix).
- `make ci-arty-snes`: OK.

**Board check passed** (the user, 2026-10-22, on the Arty, the bitstream
with the fix: 25,997 LUTs, WNS +1.670 ns): a long play through four
stages, no freeze. Before the fix it froze within minutes.

**2026-10-19, the size step (after 7.13): the sound module's DSP built
once.** The Arty's build was at 48,742 LUTs (91.6 %), too full for save
states (7.14) or the later chips. Vivado's per-hierarchy report of it
was no use: the core was one Verilog module, and the tool's
cross-boundary optimisation moved logic into the RAM modules. So
`Snes.Core` can now put the 5A22's, the PPU's and the sound module's
state machines in components of their own (CPP flag `SNES_HIER`, set by
`make snes-ooc ... SNES_OOC_MODE=hier`; the normal build inlines them),
and `Snes.MachineOoc` is the whole machine for that run. Measured with
the hierarchy kept: the sound module 45,301 LUTs, against 8,900 for its
DSP and SPC700 measured alone.

**Cause:** `apuClock` wrote the DSP's step (`dspClock`) five times: the
sample in work, a queued register write, a sample's start, and an F3
write at once (in `ioWrite`). Its SPC700 part also called `start` and
`access` from several branches of a case over the phase. Clash builds
each written call as its own logic, so the DSP was built several times
over. Now the DSP steps once a clock, its input chosen first (the
sample in work, then a queued write, then what the SPC700's cycle hands
it), and the SPC700's cycle has one path through `start` and `access`.

**The one timing change:** the SPC700 sees the DSP as it was at the
clock's start. So in the clock a sample ends, a sample due, a read of
F3, or a write the echo or a full log would block waits one clock
more (40 ns), and an F3 write goes through the queue. Results counted
in SPC700 cycles are unchanged.

**Tests:**
- `make test SYSTEM=snes`: all 9 suites pass (on the worker), among
  them snes-core (29), the Clash parts in lockstep with the spec.
- `make ci-arty-snes`: OK (on the worker).
- `make bit SYSTEM=snes` (Arty): **27,821 LUTs (52.3 %)**, WNS
  +1.774 ns.

- `make ci-snes` (the ZedBoard): OK (on the worker).
- `make bit SYSTEM=snes BOARD=zedboard`: 28,199 LUTs (53.0 %), WNS
  +2.926 ns.

**Not yet run:** a board check of the sound in a game.

**2026-10-18, step 7.13, the boot splash (part 2).** The chime now keeps
the brand's legato rule (docs/brand.md, "Sound, per machine"): a voice
keys on only at a phrase's first note, and later notes change its pitch
in place. Part 1 keyed on every note. The voicing now follows the
audition page's GBA rendition: a flute-like lead, the harmony as two
string layers 7 cents apart, a plucked bass, a 64 ms echo, the last
note fading through exponential GAIN from 45 % of it. A bug found while
trying a longer last chord (the user then kept the original length): the
fade wrote ADSR1 = 0 before GAIN, so for one step GAIN's old value 0 set
the envelope to zero and the last chord went silent at once (in
d574efb). GAIN is now written first, and the chord decays smoothly. The SNES rows in
docs/brand.md are written.

**Tests:**
- rota-brand-test: all 56 pass (the SNES include goldens regenerated).
- rota-demos, "Super Nintendo demo": every frame equals the model
  through two idle loops; the worst frame takes 23740 of 50468 master
  clocks; the audio golden `demo-snes-pcm.txt` is new (peak 25013 of
  32767, nothing clipped).
- Layer 4: `make resim` on the Arty model (126 frames, the splash then
  the menu) gives `demos/golden/splash-snes/` (frames 115 and 125);
  `make ci-demos` now compares them.

**Board check passed** (the user, 2026-10-18, on the Arty, through `make
prog SYSTEM=snes` with the card's `/systems/boot/snes.sfc`): the splash
plays and sounds right.

**2026-10-17, step 7.12, the text box: the window bits swapped in the
spec.** The board check of the scroll fix passed: smooth scrolling, LD1
dark into the Cold Man stage, and the intro cutscene no longer locks up.
The dialogue's black box was still missing, while its text showed and
advanced; that pointed at the colour window.

Each window's two bits in W12SEL, W34SEL and WOBJSEL are the enable
(bit 1, and 3 for window 2) and the invert (bit 0, and 2). fullsnes
prints "0..1=Disable, 1=Inside, 2=Outside", a typo for 2 and 3: bsnes
and anomie's document agree. The spec, and the Clash PPU after it, read
the enable from bit 0, so a window written as 2h (on, not inverted) was
off, and a game's colour window never blacked out its box. Both are
fixed.

**Tests:**
- `winblack.s` in snes-spec: a white backdrop, window 1 at x 64-191
  as the colour window, CGWSEL forcing black inside it. The frame is
  checked against that rule, not against a golden.
- `ppufuzz` in snes-lockstep catches the Clash PPU's old order.

No golden changed: the window examples' golden frames use nibbles of 3,
where both readings agree.

**2026-10-17, step 7.12, the stutter found: a spec bug in the BG
scrolls' two writes.** The board check showed LD1 dark (the late rows
are fixed) but the stutter while scrolling stayed. The spec stuttered
too: a camera that the game moves smoothly, by 1 and 2 pixels a frame
(its BG1HOFS writes, `snes-run --ppu-writes`), jumped 4 pixels back and
forth in the spec's pictures (`scroll.py`, an offset per band between
frames).

The cause: HOFS = new << 8 | (old & ~7) | (HOFS >> 8 & 7), fullsnes's
formula, read the last three bits from the register masked to 10 bits,
so a low byte written first lost its bit 2. The hardware keeps a latch
of its own for them, the last byte written to a HOFS (bsnes's
`bgofsPPU2`, apart from the last byte written to any scroll register).
Both the spec and the Clash PPU have it now.

**Tests:**
- `scroll.s` in snes-spec checks the registers after writes as a game
  makes them, the two latches apart. It catches a single latch, and the
  old formula.
- `ppufuzz` in snes-lockstep catches the Clash PPU's old formula.

krom's 8x8BGMap8BPP32x32 frame-60 golden moves by 4 pixels (it was
recorded with the bug). The camera now steps 1, 2, 1, 2 pixels.

**2026-10-17, side K's hooks in (merged origin/main, 929d758):**
- **The chip refusal:** `check_ddr` refuses a cartridge with a chip, by
  name, once the ROM is in DDR. On the user's ROMs it refuses both Star
  Fox games (Super FX) and Mega Man X2 (Cx4), and loads Rockman & Forte.
- **The save size:** `size_of_rom` sizes a save from the header, the
  SRAM of a battery cartridge (at most 32 KiB), so other games get none.
- **Pad 2:** `keys_hi` = 0x160000; `Snes.Machine` takes pad 2 from those
  two link bytes, and layer 4 checks it with `joy2.s` and `pad2.ps`.
- Pad 2's desktop and USB bindings wait for side K's 32-binding key map
  (side-k-bind32, awaiting the user's fast-forward).

**2026-10-16, step 7.12 (the DSP beside the SPC700):** the board check's
second round (the 32-row ring) still lit LD1, as the title music began,
and the stutter stayed. `snes-follow` now counts the clocks the machine
is held, and why: the CPU's port accesses waited for the sound module
about 43,000 pixel clocks a frame even on the tech demo, because the
S-DSP held the SPC700 for up to 240 clocks of each sample. A music
upload, a tight handshake on the ports, made it worse.

Now the DSP works on its own ARAM port beside the SPC700, and the sample
still sees what the spec's sees (`Snes.Apu`'s notes):
- the SPC700's ARAM writes meanwhile are logged with the byte they
  replace;
- the echo bytes wait until the sample is done;
- the SPC700's register writes are queued, and a register read waits;
- the next sample waits for this one.

The tech demo's waits fell to under 2,000 clocks a frame. **New test
ROM `dsprace`:** the SPC700 rewrites BRR data and the echo buffer and
writes and reads DSP registers while samples are in work. It catches
each of four planted bugs (no log, no echo wait, no queue, no
register-read wait), none of which the other ROMs caught.

**2026-10-16, step 7.12 (the board check, first round):** shmup_1942,
the tech demo and horizontal-shooter run on the Arty. A game of the
user's own (Rockman & Forte, 4 MiB HiROM; never committed) showed:
- LD1 (rows late) lit at the end of the title animation;
- the backgrounds stuttering while scrolling;
- the dialogue box wrong;
- a softlock at the intro stage's cutscene.

**`snes-ddrmodel`** (new) replays the spec's ROM reads through models of
the ROM port, the DDR cache and the pace:
- With the board's measured DDR latency, misses barely matter. Each
  read that leaves the ROM port's 8-byte word costs 2 or 3 pixel
  clocks, and **a DMA byte costs the 5A22 9 pixel clocks for 8 master
  clocks**.
- Together those use up the pace's spare clocks at the game's three
  scene changes, frames 396, 836 (where LD1 lit) and 1318: the lag
  reaches 27,000 pixel clocks against the ring's reserve of 12,000.
- During the stage's scrolling the lag stays near 0, so **the stutter
  is not late rows**.

**Fix:** the ring is 32 rows deep, with a lead of 23 lines in both
modes, a reserve of about 48,000 pixel clocks. The model then shows no
late rows, even with 35-clock misses. It costs about 6 block RAM tiles.

**`snes-follow`** (new) runs the Clash parts against the spec a frame
at a time for as long as a game runs (5 s a frame). It is on the user's
game to find the stutter, the text box and the softlock.

**2026-10-16, step 7.11 (the PS side, battery saves):** the menu's row,
the cartridge word (16 bits, a copier's header skipped by the PL), the
key map (pad 1), the card's script, SRAM saves through its second port
(`Snes.Save`); layer 4's save test on both boards; `make bit`: ZedBoard
WNS +1.103 ns at 41,129 LUTs (77.3 %), Arty WNS +0.419 ns at 49,643 (93.3
%). The first ZedBoard build missed by 0.197 ns: the pace's `run` fed the
5A22's step (60 levels); registered now.

**2026-10-16, step 7.10 (display, input, audio, both boards' tops):**
`Snes.Display` (the ring of 8 rows, the stream into 640 × 480, the pace),
`Snes.Machine` (the core, the PS link, the cartridge word, the audio
filter), `Snes.ArtyTop` and `Snes.Top` (the ZedBoard: VGA, HDMI through
the ADV7511, S/PDIF), their wrappers and `.mk` files. **Layer 4**
(`make ci-arty-snes`, `make ci-snes`): the krom picture and the exit game
(shmup_1942, in play at the spec's frame 31, with a DDR latency of 60
clocks) identical to the spec on both boards (the ZedBoard's goldens on
its 4-bit VGA pins, `golden/vga4`, `snes-run --vga4`); no row late on the
exit game (LD1, `arty-sim --expect-led`, added additively with side K's
agreement); the tone at 997.5 Hz on HDMI, PWM and S/PDIF, none at the
menu's volume 0 (`--expect-quiet`, new in arty-sim too); a load through
the PS link; the menu's overlay over black; the ADV7511's init identical
to the Game Boy's. **Found by layer 4** (both now in `snes-spec`'s pace
test against the real 640 × 480 timing): the output's frame count
started a frame early (the core ran a frame ahead and wrote over the rows
shown), and the pace's fraction overflowed 11 bits (the core crawled);
and the menu lit LD1 for good (the rows the held core didn't make
counted late): now nothing counts while the machine is held, nor after
it or reset until the pace first holds the core back. After a hold the
core catches up at about 0.18 of a frame per frame (7 frames for 1.25
lost), and the ring shows old rows meanwhile.
Also: Verilog's `byte` is a keyword (the DSP's BRR byte renamed), and two
lint warnings fixed. **As built:** no `Snes.Audio` resampler; a hold
and two one-pole low-passes (see "Audio"). Pad 2 waits for 7.11 (the PS
link's user bits).

**2026-10-16, step 7.9c (`Snes.RomPort`, the ROM from DDR):** between the
bus's ROM reads and `Platform.Mem.Ddr`'s port: one request outstanding,
the last 8-byte word kept (a read in it answered the next clock). The bus
needed nothing new: a ROM read's value is waited for where it is used (the
CPU's cycle end, DMA's second access, HDMA's table steps), and no master
clock passes meanwhile. **Tests (`snes-core`):** six ROMs (`dma`,
`busfuzz`, `spcfuzz`, krom's Mode7HDMA, `shmup_1942`, the tech demo) also
run with the ROM behind it and a stand-in DDR port (as gg-lockstep's:
the last 16 lines' misses answer the next clock, the rest after 5 to 34),
everything compared as before; `busfuzz` is held 8,258 clocks in 6
frames, and a wrong byte from the word kept is caught at the first read.

**2026-10-16, step 7.9b (`Snes.Apu`, the sound module):** the SPC700
core, the boot ROM, the I/O ports at 00F0h-00FFh, the three timers, the
four ports and the S-DSP, with ARAM, the DSP's samples and its Gaussian
table on ports. An instruction starts when its first cycle is before the
target (5632/118125 cycles a master clock), the spec's own catch-up rule,
so at each port access the module stands where the spec's catch-up
leaves it. A port access waits until it has caught up (two readiness
flags: with and without a tick in that clock, as the tick may raise the
target). A cycle takes two clocks; every 32nd starts with the DSP's
sample, which holds the SPC700. **Tests (`snes-core`, now with the
Clash module):** besides the bus work, the SPC700's writes and the DSP's
samples at their cycle counts, one list a prefix of the other (the
module runs ahead of the spec's catch-ups); 20 ROMs, `spctest`, krom's
SPC700ADC, `tone` and `spcfuzz` among them. `spcfuzz.s` is new: the
SPC700's I/O at random (timer dividers, CONTROL with its port clears and
the boot ROM switch, timer reads, DSP registers, F8h/F9h, the boot
ROM's area) and timer 2 at divider 0 calibrated in a tight loop. Planted
bugs caught: the catch-up one instruction ahead, the readiness without
the tick, the DSP a cycle late, timer 2's 256 steps, CONTROL's port
clear. Writing `spcfuzz` showed four ways a fuzz ROM can test less than
it seems, each found by a planted bug that got through: the
65C816 stuck at the boot ROM's last handshake (the program wrote the
port before the echo was seen), `TAX` with 16-bit X taking the
accumulator's high byte, an LFSR stepped once a call (each byte the last
one shifted), and timers restarted by CONTROL before they could count.

**2026-10-16, step 7.9a (`Snes.Bus`, the 5A22's side):** the 65C816 core
with the memory map and speeds, the registers 4200h-437Fh, multiply and
divide, the joypads and their auto-read, DMA, HDMA, the WRAM refresh and
port, and H and V. Each pixel clock does one tick (when the enable
allows) with the line's events, then one zero-time step of the bus's
owner, as "The machine in Clash" says. **Tests (`snes-core`):** the core
with the Clash PPU against the spec machine, every bus event (each CPU
cycle, DMA and HDMA byte and HDMA table read, at its master clock),
every frame and the memories at the end, with the enable held one clock
in 6 at random. A spec sound module serves the core's port accesses.
16 ROMs: our register tests, `busfuzz.s` (new: every DMA mode and step
both ways, HDMA direct, indirect and repeat on seven channels, the
multiplier and divider read as they work, the V IRQ at random lines), krom's
CPUADC and two HDMA tests, `shmup_1942`, the tech demo. Planted bugs
caught: DMA's alignment, HDMA's position and its indirect 16, the
refresh's length, the multiply's steps, the auto-read's busy time, the
NMI flag's position, DMA's decrement, the read point, the V IRQ's
position by one clock (by `busfuzz` after it moved VTIME at random).
**Found and fixed:** HDMA reading between the CPU's read and its
cycle's end overwrote the CPU's value (the CPU's read has its own latch
now); a DMA chunk at 0 ticks left after an HDMA took one more tick.

**2026-10-15, step 7.8 (first half, `Spc700.Core`):** the SPC700 as
micro-op programs, as the 65C816 is built. Each opcode is its fetch and up to 11
micro-ops, each one bus cycle (`progOf`), with one address adder. The
micro-ops only latch the bytes they read and move PC and SP; the
registers change at the instruction's end (`finish`). DIV works a bit in
each of nine internal cycles (the hardware's shift and subtract, after anomie
and blargg). It equals the spec's formula on all 2^24 inputs (checked
exhaustively, and tested: `divBits` on every X with 2,128 YA each, all
with FULL=1). **Tests (`spc700-core`):** SingleStepTests on the core (100
cases a file, 1000 with FULL=1); the core in lockstep with the spec on
random programs (8 seeds × 50,000 cycles, 64 × 400,000 with FULL=1); 266
pass. Planted bugs caught: the direct page's P, the half carry, ROR's
carry in. DBNZ Y with Y = 0 is caught by the full tier only (Y = 0 or 1 is rare in
random cases), and a wrong compare in DIV only by the DIV test. Measured:
1,603 LUTs, +19.49 ns ("Measured").

**2026-10-15, step 7.8 (second half, `Snes.Dsp`):** the S-DSP serially,
a step a clock: the shared counter and the noise; each voice (KON, KOFF,
FLG's reset, the KON delay or the Gaussian sum, the envelope, the
output, the pitch, a BRR block when the counter passes one, the volumes);
the echo (four bytes in, the FIR, the volumes, four bytes out). The
sound module holds the SPC700 while the DSP makes a sample, so a sample
falls between two SPC700 cycles, as in the spec. The voices rotate
through one head (no multiplexer for the voice in work). One multiplier
serves every step. The rates need no divider: m × 2^k samples is the
counter's low k bits zero and its residue mod m zero (the residues
counted along). ARAM, the samples (each voice two blocks) and the
Gaussian table are outside on ports. **Tests (`snes-dsp`):** the Clash
DSP in lockstep with the spec on random ARAM and registers (12 seeds ×
3,000 samples, 48 × 20,000 with FULL=1: every sample's left and right,
the 128 registers after it, ARAM at the end); the tech demo's sound
trace replayed into it (150 frames, 900 with FULL=1). A sample takes at
most about 240 clocks (the budget is about 700). Planted bugs caught: the
Gaussian index, BRR filter 2, the 15-bit clip, the FIR's last tap, PMON
on voice 0, a rate's period, the KON delay, ENDX, the bent increase and
the echo length (these two by a few seeds only). Measured: 7,284 LUTs.
The full tier found one more bug: 6 of 48 seeds parted from the spec after
6,000 to 19,000 samples, in one channel. One FIR term was exactly +32768
((-16384 × -128) >> 6), and the 16-bit wrap was Clash's `resize`, which
on a `Signed` keeps the sign bit (+32768 became 0, not -32768). It now
takes the low 16 bits, and a test (`snes-dsp`, "the FIR: a term of
+32768") has that history and those coefficients.

**Found in the spec while writing the DSP (2026-10-15):**
`apuSamples` returned only the first half of the samples (n stereo
samples are 2n values), so `snes-run --wav` wrote half the music (the
tone test's frequency didn't notice; it now counts the samples against
the master clocks, which fails on the old code). And, below, the
envelope's counter.

**Found in the DSP spec (2026-10-15): the envelope stepped by the
voice's pitch counter**, not the shared rate counter (a shadowed name),
so every ADSR/GAIN rate slower than one step a sample was off. Fixed;
`snes-spec`'s new "GAIN: linear decrease at rate 29" test fails on the
old code (no step at pitch 0) and passes now. The tech demo's WAV for
the by-ear check was made before both fixes; it is made again (15 s,
`build/snes/listen/unnamed-tech-demo.wav`).

**2026-10-15, step 7.7 (`W65c816.Core`, the Clash 65C816):** a
micro-op sequencer. Each instruction is its opcode fetch and a program
of at most 8 micro-ops, one bus cycle each (`progOf`): the addressing
mode's cycles, then a read's, write's or read-modify-write's five; the
stack instructions, jumps and calls, interrupts and block moves have
programs of their own. The conditional cycles (DL's, the indexed modes'
extra one, a 16-bit operand's second byte, a taken branch's) are
micro-ops with a condition, which the sequencer skips. Every address
comes from one generator (base + offset + k, wrapped at 24, 16 or 8
bits). A bus cycle takes two clocks: at its end NMI and IRQ are sampled,
then the cycle's effect, and the next micro-op is picked; the next clock
works out the request, or the instruction's end and what follows (the
pending interrupt or the opcode fetch). The request is on the bus two
clocks after a cycle's end, which a 6-clock cycle allows (its read is 4
master clocks before its end). **Tests (`w65c816-core`):**
SingleStepTests on the core, the first 100 cases of all 512 files (all
5.12 million with FULL=1); the core in lockstep with the spec on random
programs (memory of pseudo-random bytes, random NMI pulses and IRQ
levels), every bus cycle compared, 8 seeds × 50,000 cycles in each
configuration (64 × 400,000 with FULL=1); 529 tests pass. Planted bugs
caught: DL's cycle, the index page cross, the decimal adjust, IRQ's
mask. The first build put every next request in one clock (17,523 LUTs,
59 logic levels, the clock missed by 0.66 ns): rebuilt as above,
it measures 3,404 LUTs, 313 FFs, +9.46 ns ("Measured"). With the
PPU's 21,467 the go/no-go projects 70 to 90 % of the LUTs: the upper
end crosses 85 %, so the step waits for the user's choice of cuts.

**2026-10-15, step 7.6 (`Snes.Ppu`, the Clash PPU):** the PPU as one
state record and a pure clock function (`ppuClock`), with its memories
outside on ports (VRAM as two byte-wide chips, CGRAM, OAM's low table as
four byte lanes, the sprite line buffer as 8 banks; data one clock
after the address, as block RAM). At a tick (a master clock): H moves
on, a visible line's dot d starts at H = 4 (d + 1) (the registers it
needs copied, its fetch slot's VRAM word or Mode 7's map byte and its
sprite pixel asked for), the sprite unit starts at H = 1120, then the
tick's B-bus access. A dot takes 4 clocks after its tick (the word into
its column, Mode 7's char byte; the screens; the main and the sub
colour's CGRAM reads; colour maths and brightness), so the 25.175 MHz
domain keeps up with one tick a clock. The sprite unit scans the 128
entries one a clock while it clears the line buffer, then fetches up to
34 slivers of two VRAM words, each written 8 pixels at once into the
buffer's 8 banks: about 230 clocks, done in the line's H-blank.
**Lockstep (`snes-lockstep`, layer 1):** the spec records every PPU
access as it runs a ROM (master clock, H, V, the value read: `traceOn`,
`takeTrace`) and each frame at line 0; the trace is replayed into
`ppuClock` over block RAM models (read before write), with idle clocks
at random between ticks; H and V at each access, each value read, each
frame, and VRAM, CGRAM and OAM at the end must match. 20 ROMs: ppuregs,
dma, **`ppufuzz.s`** (ours: random VRAM, CGRAM, OAM and registers, four
random HDMA channels, then random register writes and reads at random
times, forced blank 1 in 8 INIDISP writes), 7 of krom's, 9 OpenSNES
examples, the two games; the quick tier runs each until two frames
after its picture appears (measured; a blank last frame fails the
test), 2.5 minutes; the full tier 60 frames each. **Planted bugs
caught:** the windows' XOR logic, brightness, the CGRAM drawing rule,
the VRAM rule, OAM's reload at V-blank (by `ppufuzz`), the OAM rule in
the sprite unit's window (by ppuregs' new check 4). **Found by the
lockstep:** one Clash bug (the sprite scan's on-line test narrowed
row - y to 7 bits: sprites 128 lines away showed) and three spec rules
a pipelined circuit can't match, now changed in the spec (the goldens
unchanged, 98 tests pass): (1) VRAM and OAM take no writes while the
sprite unit works (H 1120 to the line's end, once it started: the
circuit reads OAM and the tiles over that time, the spec did it at
1120 at once); new ppuregs checks 4-6 (line 0: OAM and VRAM writes
from 1120 dropped, earlier ones land); (2) the BG columns are a ring of
8 a layer, and the offset-per-tile entries have their own ring (they
were in BG3's column): after a mid-line BG mode change both read the
same stale data; (3) a DMA or HDMA byte's access is at the start of its
8 clocks, not the end: an HDMA ending inside a CPU cycle put its last
write and the CPU's access on one master clock, which no bus does.

**2026-10-15, step 7.5 (continued): the BG layers fetched a tile ahead.**
Counting the Clash PPU's VRAM reads showed that per-pixel fetching
doesn't fit: mode 4 with offset-per-tile needs 9 dependent word reads in
one dot where two ports give 8. The spec's BG layers now work as the
chip's do: 33 columns a line, each fetched in 8 slots of one VRAM word a
dot (dots 6-269; a table per mode, `fetchTable`) from the registers at
each slot, the pixels then taken from the fetched columns with the
layer's fine scroll from its first column. Mid-line scroll writes now
take effect at the next column, as on the chip. The core then needs one
VRAM read a dot (Mode 7: two bytes a pixel). Further timing rules for a
pipelined core: pixel x at master clock 4 (x + 23), HDMA at 1116 (after
the last pixel), sprites at 1120 of the line before, CGRAM and OAM
taking no writes while the PPU reads them. **All 29 golden frames are
unchanged**; `snes-spec` 98 tests in 178 s.

**2026-10-15, step 7.5 (the picture, `Snes.Spec.Ppu`):** every pixel
drawn as its dot passes, from the registers and memories as they are
then; a line's sprites worked out as it begins. BG modes 0-7 (2/4/8 bpp,
8×8 and 16×16 tiles, the four map sizes, flips, scrolling with the
write-twice latches), offset-per-tile (modes 2, 4, 6), mosaic, Mode 7
(fullsnes's matrix with its rounding, screen over, flips, EXTBG), direct
colour, sprites (eight sizes, 32 a line and 34 tiles, the flags in
STAT77, priority rotation), the two windows and their logic, main and
sub screens, colour maths (add, subtract, half, the fixed colour, forced
black, its window), brightness, forced blank, hi-res (modes 5 and 6:
half-dots, the sub screen on the left) and pseudo-hires, interlace (a
field at a time; modes 5 and 6 use the field's half of 448 lines); VRAM
takes writes only in V-blank or forced blank; the latched H/V counters
read low then bit 8. `snes-run --png` writes the frame. **Checked by
eye, and against the OpenSNES authors' own screenshots** (fetched with
the examples; `tools/snes-refs.py`): of the 47 examples, the static ones
match their screenshot at 92 to 100 % of pixels (simple_sprite,
two_players, animated_sprite, hdma_indirect_gradient 100 %; extbg,
mode7_flying, mode7_racing, perspective_rotate 99.9 %; mode5_hires and
pseudo_hires once their half-dots are blended as the screenshots are).
The rest are animated (scrolling, fading, the HDMA wave: another frame
than the screenshot's), or their screenshot is stale: mode1 and
gradient_colors show the new OpenSNES logo that v0.50.0 ships, and
mode6's screenshot is both fields woven. **Gates (`snes-spec`, 98
tests):** 29 frames frozen as hashes (`test/golden/frames/`; krom's BG
maps in every depth, flips, HiColor, Mode 7, HDMA, windows, rings,
interlace font; the OpenSNES hi-res, Mode 7, sprite, window, colour
examples; shmup_1942, horizontal-shooter, the tech demo's title), each
checked by eye first; `ppuregs.s` (a VRAM write during the display
dropped, the counter latch's read order, STAT77's range over from 33
sprites; a planted bug (VRAM writes always taken) fails it). Found:
`dma.s` wrote VRAM with the display on, which hardware drops too, so the
start-up code (`snes.inc`) now sets forced blank as programs do. **Not
done:** undisbeliever's test ROMs (sources only; building them means
their toolchain: later, with the timing checks); *to verify*: a BG
tile's fetch a tile ahead of its pixel (here each pixel reads the
registers of its own dot), colour maths in hi-res, CGRAM and OAM
written during the display, the PPU's open bus. The model runs about 10
frames a second; `snes-spec` takes 202 s.

**2026-10-15, step 7.4 (the sound module):** `Snes.Spec.Apu` (the SPC700
with its 64 KiB, the boot ROM at FFC0h under CONTROL bit 7, the I/O
ports, the three timers, the four ports to the CPU), `Snes.Spec.Dsp`
(the S-DSP a sample at a time: BRR with its filters and 15-bit clipping,
the Gaussian interpolation with its overflow rules, ADSR and GAIN with
the shared rate counter, noise, pitch modulation, KON/KOFF every other
sample, FLG, ENDX, the echo ring and its 8-tap FIR, the mix) and
`Snes.Spec.Gauss` (the chip's 512 coefficients from fullsnes; every four
weights sum to 7FFh-801h, as fullsnes says). **Our boot ROM**
(`systems/snes/ipl/ipl.s`, 64 bytes, `make snes-asm` →
`build/snes/ipl.bin`): written from fullsnes's description of the
upload protocol. Its interface is the original's, and has to be: entry
points FFC0h and FFC9h, the zero page cleared, SP = EFh, the handshake,
A = X = Y = 0 at the hand-over. That leaves little room in 64 bytes, so
parts of it are close to the original; fullsnes also prints the
original's disassembly, which I read. The transfer loop and the layout
are our own. The user's own dump replaces it when given (`snes-run
--ipl`). **The sound module is paced by the master clock** (5632/118125
SPC700 cycles a master clock; caught up before each port access and at
each line's end). That changes "Clock and display" above: on the board
it waits for DDR with the rest of the machine, so the core and the spec
stay in lockstep, at the cost of a pitch 0.26 % low (4.5 cents).
`snes-run --wav` writes the DSP's output. **Gates (`snes-spec`, 68
tests, 114 s): gilyon's spctest "Success"** (uploaded through our boot
ROM); **krom's 7 SPC700 tests PASS**; `tone.s` (the 65816 uploads an
SPC700 program and a BRR loop through the boot ROM's protocol) **plays
1000 Hz** measured from the output (within 1 Hz); 5 DSP unit tests
(direct gain, KON's 5-sample delay, the noise LFSR, the fastest attack,
ENDX). Found: a BRR loop of -8 nibbles at shift 12 overflows the
interpolation (fullsnes's "77778888" glitch, as on the chip), so the
tone uses 7 and -7. **Not done: the by-ear check** of music (the
unnamed tech demo's, 15 s in `build/snes/listen/unnamed-tech-demo.wav`,
level RMS 1300-1500) waits for the user. The blargg `snes_spc` oracle
wasn't used, since the user hasn't agreed to it. The sub-sample schedule
(a DSP register written mid-sample) is *to verify*.

**2026-10-15, step 7.3 (`Snes.Spec`, the machine without video and
sound):** `Snes.Spec.Map` (the 5A22's address space: WRAM and its
mirror, the B bus, the CPU registers, LoROM and HiROM ROM and SRAM with
fullsnes's mirroring; the 6/8/12-clock speeds with MEMSEL) and
`Snes.Spec.Machine`: the master clock, the line's events (V-blank and
the NMI flag, HDMA set-up and transfers, the auto-read, the WRAM
refresh, the H/V IRQ), the registers at 4200h-421Fh (multiply and divide
step by step as bsnes, TIMEUP, HVBJOY, the joypads serial and
auto-read), general DMA and HDMA (all eight transfer modes, direct and
indirect), the PPU's memory ports (VRAM with VMAIN's increment and
remapping and the read prefetch, CGRAM, OAM, the WRAM port, M7's
multiplier) with the other PPU registers stored, the APU ports showing
AAh, BBh. `snes-run` runs a ROM headless and prints BG1's tilemap as
text. **Gates (`cabal test snes-spec`, 54 tests): gilyon's cputest-basic
and cputest-full "Success"; krom's 23 CPU tests all PASS, no FAIL; our
ROMs** `map.s`, `muldiv.s`, `dma.s`, `nmi.s`, `virq.s`, `joy.s`, `sram.s`
(with `lorom.s`, `hirom.s`) pass; a frame is 357,368 master clocks, then
357,364. **Found by cputest-full**, and kept as `W65c816.Spec.Config`
(`w65c816` the hardware, `singleStep65816` the tests' model, which still
passes all 5.12 million cases): in emulation mode JSR (a,x) pushes with
the full 16-bit S (test 0277), and (d,x) with DL /= 0 wraps its
pointer's high byte in the page (cputest's README); SingleStepTests'
generator does neither. Two of our own tests were wrong, not the model:
`joy.s` read before the auto-read began (hardware would too), `dma.s`
depended on the VRAM prefetch's reload order. Still *to verify* (at 7.5,
with undisbeliever's timing tests): the events' exact master clocks, a
read's place in its cycle, DMA's start-up and sync, the VRAM prefetch's
order, the H/V latch's high bits, the auto-read's bit-by-bit timing.

**2026-10-15, step 7.2 (`Spc700.Spec`):** the cycle-exact SPC700
(`cpus/spc700/src/Spc700/Spec.hs`): reads, writes and internal cycles on
the caller's bus, every opcode, the direct page in page 0 or 1 with its
wrap, MUL, DIV, DAA, DAS, the bit operations, SLEEP and STOP halting.
**SingleStepTests: all 256 files, all 1000 cases each, final state and
every cycle's address, kind and data, pass** (`EMU_FULL_TESTS=1 cabal
run spc700-spec`, 4 s). From the tests: the stores read their
destination first (not MOV (X)+,A or MOV dp,dp); one-byte instructions
read PC again without advancing (the cycle the files record with no
data, and no RAM listed: reading a timer counter there would clear it,
so the sound module's bus must treat it as a read); DECW writes its low
byte before reading the high one.

**2026-10-15, step 7.1 (`W65c816.Spec`):** the cycle-exact 65C816
(`cpus/w65c816/src/W65c816/Spec.hs`): the caller's bus with the cycle
kinds the 5A22 times by (opcode, operand, data and vector reads, data
writes, internal cycles with their address, stopped cycles), every
opcode in both modes, decimal mode in 8 and 16 bits, MVN/MVP a byte per
step, WAI, STP, NMI, IRQ, BRK, COP, reset. **SingleStepTests: all 512
files, all 10,000 cases each (5.12 million), final state and every
cycle's address, data, direction, VDA, VPA, VPB and MLB, pass**
(`EMU_FULL_TESTS=1 cabal run w65c816-spec`, 103 s; the quick tier reads
100 per file); 10 interrupt, WAI, STP and reset unit tests. What the tests
showed beyond the data sheet: emulation mode's modify cycle is a write
of the old byte with VDA low; WDM's second byte is an internal cycle, not
a fetch; JSR (a,x) keeps its pushes in page 1 in emulation mode (the
other 65816 stack instructions don't); MLB is set from the first data
cycle of a read-modify-write, not on its operand fetches; an internal
cycle's address is PBR:PC for one-byte instructions and the last operand
byte otherwise (the indexed modes': the base's page with the low byte
indexed). Still the 6502's: which cycle an interrupt is decided in, and
the reset's cycles (*to verify* at 7.3 with the 5A22's timers).

**2026-10-15, step 7.0 (ROMs and tooling):** fetched and pinned
(`tools/fetch-roms.sh`, its M7 block): SingleStepTests 65816 (512 files,
`tools/roms-65816.sha256`) and SPC700 (256 files, `tools/roms-spc700.sha256`;
the repository's `simple_py_serve.py` left out), gilyon's v1.4, 53 of
krom's ROMs (every CPUTest, 30 PPU demos), 22 OpenSNES examples, the
unnamed tech demo, horizontal-shooter. undisbeliever's test ROMs are
sources only: picked and built at 7.5. New packages: `cpus/w65c816`
(`W65c816.Types`; `w65c816-spec` decodes every file: the opcode byte at
PBR:PC, E = the file's mode, the first cycle the opcode fetch; WAI and
STP's cycles have no address), `cpus/spc700` (`Spc700.Types`;
`spc700-spec` likewise), `systems/snes` (`Snes.Cart`, the header parser;
`snes-cart`, a dump tool; `snes-spec`, 19 tests). `make snes-asm`
(`snes.inc`: the memory map, the header macro `SNES_HEADER`, vectors,
start-up, the verdict in RESULT = 7E0000h and STP), `lorom.s`, `hirom.s`.
Every fetched cartridge parses as its project says: LoROM but
`hirom_demo` and horizontal-shooter (HiROM), `save_game` battery with 8
KiB. A planted bug (the LoROM and HiROM mode nibbles swapped) passed the
first 18 tests (the checksum and title points decide on real ROMs), so a
19th, two equal headers told apart only by the mode byte, was added; it
catches it. **Found:** `shmup_1942` has no sound (no music or effects in
its sources or README), but the exit needs sound: a question for the
user (snes.md, "Questions", 7).

**2026-10-15:** the user's decisions recorded (above); CLAUDE.md's M7 row
added. Next: 7.0.

**2026-10-15 (M7 planning, branch `m7-snes`):** the plan written (this
file, PLAN.md "Milestone 7"), system ID 9 and `/snes` reserved in
docs/ARCHITECTURE.md, the log `docs/status/m7.md`. Nothing built.
