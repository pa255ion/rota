# Atari 800XL (NTSC)

Target: an **NTSC Atari 800XL** (6502C "SALLY", ANTIC, GTIA, POKEY, a
6520 PIA, 64 KiB RAM), milestone 4. Written in the 2026-10-04 planning
review (claude.ai), before any code: everything here is a plan, and the
parts marked *to verify* must be checked against the references below
before code depends on them. When the status log at the end disagrees
with the plan, the log wins.

## Why the 800XL, and what is new

- **The second machine on the shared 6502** (`cpus/mos6502`, built for
  this in M3): the NMOS variant with decimal mode (`nmos6502` /
  `coreNmos6502`) already exists and passes SingleStepTests.
- **Free system software exists:** AltirraOS and Altirra BASIC (Avery
  Lee) replace Atari's OS and BASIC, so nothing copyrighted is fetched.
  Atari's own ROMs are never fetched or committed.
- **The first system that needs a whole keyboard.** Since side track E
  the board has one (the wireless receiver on J13), so BASIC can be
  typed on the board itself.
- **New compared to the NES:** the video is two chips with cycle
  stealing (ANTIC's DMA halts the CPU), colours change mid-line in most
  games (display list interrupts, WSYNC), POKEY scans a keyboard matrix,
  and programs come as cartridges *and* executable files (XEX) that a
  loader must run.

## Machine summary (NTSC; *to verify* each against the Altirra Hardware Reference Manual)

| Item | Value |
|---|---|
| CPU | 6502C "SALLY": NMOS 6502 with decimal mode, a HALT input driven by ANTIC, 1.7897725 MHz (colour clock 3.579545 MHz / 2) |
| Frame | 262 lines × 114 CPU cycles (228 colour clocks) = 29868 cycles, 59.92 Hz |
| ANTIC | display lists, playfield and player/missile DMA, memory refresh, fine scrolling, WSYNC, VCOUNT, NMI (display list interrupt, vertical blank) |
| GTIA | colour registers (16 hues × 8 luminances; 16 in GTIA mode 9), players and missiles, priority, collisions, GTIA modes 9 to 11, console keys (START, SELECT, OPTION), joystick triggers |
| POKEY | 4 audio channels (polynomial counters, high-pass, 16-bit pairs), timers and IRQs, keyboard scan (KBCODE, SHIFT, CTRL, BREAK), random number generator, serial port (SIO), paddles |
| PIA | PORTA: two joystick ports; PORTB (XL): memory control (OS ROM, BASIC, self-test); control lines for SIO and cassette |
| Memory | 64 KiB RAM; OS ROM 16 KiB (C000-CFFF, D800-FFFF; self-test at 5000-57FF when PORTB bit 7 is 0); BASIC 8 KiB at A000-BFFF (PORTB bit 1); I/O at D000-D7FF (GTIA D000, POKEY D200, PIA D300, ANTIC D400, cartridge control D500) |
| Cartridges | left slot: 8 KiB at A000 or 16 KiB at 8000 (standard types); larger ones bank-switch |
| Display | normal playfield 160 colour clocks = 320 hi-res pixels; up to 240 lines; NTSC artifacting in hi-res |

## References

- Altirra Hardware Reference Manual (Avery Lee): the main reference
  (cycle tables for ANTIC DMA, GTIA and POKEY details, the 6502C).
- Atari Home Computer Technical Reference Notes, the 800XL addendum,
  De Re Atari, Mapping the Atari (the OS's memory locations: SAVMSC,
  INITAD, RUNAD).
- Altirra's source is a cross-check for behaviour (values and rules, not
  code), as the Linux driver was for the ADV7511.

All PDFs go in the git-ignored `local/reference/`, as the board manuals.

## Accuracy target

- **CPU and ANTIC: cycle-exact**, including where ANTIC steals cycles
  within a line, because games and demos time display list interrupts
  and WSYNC to the cycle. The lockstep compares bus traces per CPU cycle,
  as for the NES.
- **GTIA at colour-clock resolution**, register writes visible from the
  colour clock they land on: mid-line colour changes are the normal case
  here, not a stretch goal as on the Game Boy.
- **POKEY** at its 1.79 MHz rate, polynomial counters bit-exact (they
  feed RANDOM, which programs read).
- **The shared 6502 and ANTIC's halt** (settled in 4.0, no change to
  `cpus/mos6502`): ANTIC takes the bus through the 6502C's HALT on any
  cycle, read or write ("the 6502 is blocked from the memory bus while
  ANTIC does a read cycle", Altirra HRM 2.4 "DMA contention"; its DMA
  slots are fixed per line and never wait for the CPU), while WSYNC pulls
  RDY, which the NMOS core only honours on reads (HRM 4.9: "the 6502 does
  not respond to RDY during a write cycle", so `INC WSYNC` writes twice
  before stopping). A halted CPU is frozen whole, including its NMI edge
  detector, so HALT needs nothing from the CPU model: `Mos6502.Core` is a
  function the machine calls once per CPU cycle, and the core simply isn't
  called on a halted cycle; in the spec, the bus callbacks run the halted
  cycles (ANTIC's DMA, GTIA, POKEY) before performing the CPU's access.
  WSYNC uses the existing RDY (`busRdy`, `ciStall`). `mos6502-spec` and
  `mos6502-lockstep` are untouched.

## System software and test ROMs (fetched by `tools/fetch-roms.sh` into `roms/a800xl/`, never committed)

Checked in step 4.0 (2026-10-04); hashes in `tools/fetch-roms.sh`.

| What | Use | Licence | Source (pinned) |
|---|---|---|---|
| AltirraOS 3.11, XL/XE kernel (16 KiB) | the machine's OS | Avery Lee; FSF all-permissive: "Copying and distribution of this file, with or without modification, are permitted in any medium without royalty provided the copyright notice and this notice are preserved." | atari800's built-in copy (`src/roms/altirraos_xl.c`, commit d722329); `fetch-roms.sh` writes the C array's bytes to `altirraos_xl.rom` |
| Altirra BASIC (8 KiB; file header 1.58, banner "Altirra 8K BASIC 1.59") | BASIC | the same notice | `src/roms/altirra_basic.c`, same commit |
| Acid800 1.2, the standalone tests (XEX) | CPU, ANTIC, GTIA, POKEY, PIA, MMU behaviour | MIT (header of every source file) | `Acid800-1.2.7z` from virtualdub.org, `out/Release/Acid800/standalone/` |
| Scorch v1.56 (`scorch.xex`, 56 KiB) | the exit game | the Unlicense, for the whole repository: code, artwork, music (caveat: it bundles Raster Music Tracker's player routine, distributed freely by its author for use in productions) | github.com/pkali/scorch_src, release v1.56 (commit ea08663) |
| Our own ca65 programs (`systems/a800xl/test/asm/`, `make a800-asm`) | rule 5, banking, DMA patterns, a tone | ours | `*.s` → `build/a800xl/asm/*.xex`; `cart_*.s` → 8 KiB `cart_*.rom` |

**Exit game: Scorch** (Pecus and pirx, 2000 to 2026): a turn-based
artillery game (a Scorched Earth remake) for 2 to 6 players, human or
computer, with music and sound effects, playable from the joystick alone
(angle, power, fire; the shop by joystick too). An XEX, so it also
exercises the loader stub. Its cartridge release is a 1 MiB bank-switched
image (out of scope), so M4 uses the XEX.

**Note for the exit:** Altirra BASIC prints **"Ready"** (mixed case,
from its banner bytes), not Atari BASIC's "READY"; the hooks check
"Ready".

### Required tests (frozen 2026-10-04, `A800.Acid800`)

Every Acid800 hardware test that runs on a stock 800XL without a disk
drive. Each prints `Pass`, `FAIL.` and a reason, or `Skipped` and a
reason through the screen editor; `A800.Acid800.verdict` reads it from
the screen text (`A800.Screen`, SAVMSC). Exit: all **51 required** pass
on the spec, the core and the board.

| Group | Required tests |
|---|---|
| CPU (8) | cpu_insn, cpu_flags, cpu_decimal, cpu_timing, cpu_bugs, cpu_clisei, cpu_illegal, cpu_illtiming |
| ANTIC (20) | antic_default, antic_nmist, antic_vcount, antic_wsync, antic_addresswrap, antic_dlistwrap, antic_dlitiming, antic_addrmirror, antic_pmdma, antic_charcontrol, antic_dmapattern, antic_blockednmi, antic_hscrolbug, antic_virtdma, antic_vscroll, antic_vscroldli, antic_pfstarttiming, antic_pfstoptiming, antic_linebuffering, antic_hiresbug |
| GTIA (11) | gtia_default, gtia_phantomdma, gtia_consol, gtia_vdelay, gtia_collision, gtia_collision2, gtia_pmretrigger, gtia_pmresize, gtia_pmoverlap, gtia_psuedomodee, gtia_addrmirror |
| POKEY (9) | pokey_default, pokey_noise, pokey_irqtiming, pokey_timerirq, pokey_timertiming, pokey_timergranularity, pokey_twotone, pokey_inittiming, pokey_addrmirror |
| PIA (2), MMU (1) | pia_basic, pia_irq; mmu_xlbanking |

**Tracked, not required (5):** POKEY's serial port (pokey_seroc,
pokey_serclock, pokey_sertiming, pokey_skstat, pokey_asyncrecv; seroc
and, since cassettes depth 2, asyncrecv pass on the spec); SIO is
out of M4's scope. **Left out:** cpu_65c816 and pokey_serdirect (they
skip themselves without a 65C816 or a disk drive) and the `mod_*` files
(helpers of the full suite, not tests).

Test hooks: screen RAM as text (`A800.Screen`: SAVMSC, internal codes to
ATASCII to ASCII), Acid800's verdicts (`A800.Acid800`), screen goldens at
320 × 240 (checked by eye once, then frozen; layer 4 goldens ×2).

## Reference model (`A800.Spec`)

Package `systems/a800xl`, modules `A800.*`. Built in the order of the
steps: the machine without video (4.1), ANTIC (4.2), GTIA (4.3), POKEY
(4.4). One step per colour clock (two per CPU cycle), state in `ST` with
a pure interface (`runRom`-style: run until a verdict, a frame count or
a cycle limit), events per CPU cycle for the lockstep: bus access, ANTIC
DMA cycle, instruction boundary, NMI/IRQ, line and frame boundaries.

## Hardware design

### Clocking

`DomPix` (25.176 MHz) with a phase-accumulator enable for the colour
clock (≈ 7.03 pixel clocks each) and every second one for the CPU
(≈ 14.07 per CPU cycle), as the NES's dot enable. One clock domain, no
crossing.

### Memories (budget, RAMB36 tiles)

| Memory | Size | Tiles |
|---|---|---|
| RAM | 64 KiB | 16 |
| OS ROM, BASIC | 16 + 8 KiB | 6 |
| Cartridge | up to 16 KiB (standard types; the ZedBoard only since 2026-10-21: the Arty reads every cartridge from DDR, "XEGS cartridges") | 4 |
| Frame buffers | 3 × 320 × 240 × 8 bits (hue and luminance per hi-res pixel) | about 51 |
| Overlay (platform) | | 1 to 2 |

About 80 of 140. Bank-switched cartridges later come from DDR (side
track C's port; the running system's media region, ARCHITECTURE.md).

**OS and BASIC in the bitstream** (proposal; the session may revise it
with reasons): baked in with `blockRamFile` from the fetched files at
`make hdl` time, so the bitstream boots without the PS and layer 4 runs
it as is. Loading another OS from the card is a later option.

### Display

320 × 240 (the normal playfield, lines up to 240) × 2 = 640 × 480,
filling the screen. Wider playfields and the horizontal overscan are
cropped to the normal playfield's width (*to verify* where it sits in
colour clocks), recorded as a known limit. Palette: 256 entries
generated from NTSC signal levels as the NES's (`Rgb888`); hi-res NTSC
artifacting not modelled at first (a later option). The frame rate
(59.92 Hz) differs from the output's (59.94 Hz):
`Platform.Video.FrameBuffers.tripleBuffer`, a frame shown twice about
once a minute.

### Sound

POKEY's four channels mixed (POKEY's output is not a plain sum: the mix
table *to take* from the reference manual or Altirra's measurements),
then S/PDIF over HDMI as the Game Boy and NES (`Platform.Audio.Spdif`).

### Input

As built in step 4.8 (open points marked):

- **Keyboard as a matrix.** The core takes the state of every key (64
  matrix positions, SHIFT, CONTROL, BREAK, the console keys) and POKEY
  scans it as hardware does. The PS turns a USB keyboard's boot reports
  into it (`platform/ps/a800_keys.zig`) and writes eleven bytes over the
  PS link's write bus to `A800.Loader.keysBase` (0x100000; bytes 0-7 the
  matrix, 8 SHIFT/CONTROL/BREAK, 9 START/SELECT/OPTION, 10 joystick 1)
  whenever they change. The layout is positional where the 800XL has the
  key, else by meaning: letters, digits, Space, Return, Backspace, Tab,
  Caps Lock, `- = ; , . /` as themselves; `'` is `+`, `\` is `*`, `[` `]`
  are `<` `>`; Shift SHIFT, Ctrl CONTROL; F1 HELP, F2 START, F3 SELECT,
  F4 OPTION, F6 the inverse key, F8 and Pause BREAK; the arrows are
  CONTROL with `- = + *` (the 800XL's cursor keys), Delete CONTROL+
  Backspace, Insert CONTROL+`>`.
- **The menu key:** Esc stays the menu key everywhere (confirmed with
  the user, 2026-10-04); the 800XL's ESC is `` ` `` (the key left of 1,
  which the 800XL doesn't have), preferred by the user over F12 as the
  menu key (2026-10-05).
- **Joystick 1** from the board buttons (U/D/L/R, C = trigger), the
  `[a800xl]` section of `keymap.conf` (default: the keypad's 8 2 4 6, 5
  down too, 0 or Right Alt the trigger; a gamepad's D-pad, left stick,
  Cross) and the desktop's `K` keys (the same bits), ORed. SW0-2 hold
  START, SELECT, OPTION as well.
- *Not yet:* the desktop `link --system a800xl` sends the joystick only,
  not the keyboard.

### Loading (PS link, SD menu)

- System ID **5** (`systemA800xl`, reserved in ARCHITECTURE.md), menu
  folder `/a800xl`, bitstream `a800xl.bit` in `/systems`.
- **Cartridges** (`.rom` raw 8/16 KiB, `.car` with its header, standard
  types only): written into the cartridge memory over the link while
  the core is held, then a cold start. As built (4.8, `A800.Loader`):
  the file goes from link address 0 as it is; the PL tells XEX (`FF
  FF`), CAR (`CART`, type 1 or 2) and raw images (exactly 8 or 16 KiB)
  apart by their first bytes and length, and refuses anything else (the
  menu's `a800Check` refuses the same first). BASIC is off for loaded
  programs (as Altirra loads them); with nothing loaded the built-in
  BASIC prompt runs.
- **XEX** (binary load files): the PS sends the file and nothing more;
  our own loader runs on the emulated 6502. As built in 4.8, the file
  goes into a 64 KiB block RAM over the link (A800.Loader; 32 KiB at first, doubled for Scorch's 56 KiB), not into DDR
  as first planned; decided with the user (2026-10-05): XEX files move
  into DDR (the media region, through HP0 as the Game Boy's ROM) right
  after M4, with the ATR work that needs DDR anyway. **As built in step 4.1** (moved up from 4.8: the banking tests
  and Acid800 are XEX files the spec has to load; the hardware will use
  the same mechanism): `systems/a800xl/loader/xexload.s`, a 256-byte ROM
  page at **$D500-$D5FF** (the cartridge control area, unused by the
  standard cartridges), assembled by `make a800-asm` into
  `build/a800xl/xexload.bin`. While an executable is attached, a **stub
  cartridge** at $A000-$BFFF (all $FF but its header: start $D500, flags
  4, init $D5EF, an RTS in the page) makes the OS start the loader like a
  cartridge after its own start-up. The loader turns BASIC off (PORTB
  bit 1, BASICF) and the stub away (write $D5FE: RD5 low, TRIG3 = 0),
  sets RAMTOP to $C0 and reopens E:, so the program finds the machine as
  after a DOS boot without BASIC. It reads the file through the page's
  **port** (read $D5FE: next byte, $FF past the end; read $D5FF: bit 7 =
  end) and loads it as DOS 2 does: segments to their addresses, the
  $FFFF markers skipped, INITAD called after a segment that changed it,
  RUNAD at the end (the first segment's start if none set it). Writing
  $D5FF makes the page and port vanish at the CPU's next read outside the
  page (the final `JMP (RUNAD)` reads page 2), so the program sees a stock
  machine. Zero page $43-$48 (DOS's, unused without DOS). Checked by
  `pass.xex`, `banking.xex` and Acid800's XEX tests in `a800xl-spec`.
- **ATR disk images: after M4**, as a PL-side disk drive on POKEY's
  serial port reading the image from DDR (same rule: the PS only loads
  the file).

### The Arty Z7-20 (since 2026-10-07)

The Arty is the main development board from 2026-10-07 (the user; the
ZedBoard is out of reach for now). Both boards run the same machine,
`A800.System` (the core, loading and the keyboard over the PS link, the
frame buffers and the overlay, the sound's filters); the tops only add a
board's ends:

- `A800.Top` (ZedBoard): VGA and HDMI through the ADV7511, S/PDIF, the
  five buttons (stick, BTNC trigger), SW0-2 the console keys, 8 LEDs.
- `A800.ArtyTop` (Arty, wrapper `boards/arty-z7-20/a800xl_zynq.v`,
  `systems/a800xl/arty-z7-20.mk`): HDMI from FPGA pins in HDMI mode
  (`Platform.Video.Hdmi`, side track G phase 3: the sound on both
  channels in its data islands), the same sound as PWM on the jack, BTN0-3 the stick
  (left, down, up, right) and SW1 the trigger as the other systems' Arty
  tops, the console keys from the keyboard (F2-F4) only, LD0-3 (accepted,
  frame, hold, heartbeat).

Layer 4 on the Arty: `ci-arty-a800xl` (scene.s, the tone in the HDMI
audio and on aud_pwm, the keyboard over the link). Board-checked on the
Arty (the user, 2026-10-07): BASIC and typing on the USB keyboard, Scorch
with sound on the monitor and the jack. `make load` over the link needs
`LINK_ARGS=--slow` on the Arty (2 Mbaud outruns the relay's byte-by-byte
forwarding; a platform finding, STATUS).

### XEGS cartridges (side track Q, 2026-10-21)

The user's Lode Runner, a .car of type 14, was refused; the user asked
for XEGS cartridges "in both the spec and the core" and chose to read
every cartridge from DDR on the Arty; side track Q took the 800XL's files
for it (CLAUDE.md).
- **Behaviour** (`A800.Media.cartBanks`; values from the atari800
  emulator's cartridge.c, CARTRIDGE_XEGS_32/_07_64/_128, and the HRM 8.1
  for the CCTL region): CAR types 12, 13, 14, 4/8/16 banks of 8 KiB. The
  window at $8000-$9FFF shows the bank last selected by a write anywhere
  in $D500-$D5FF (the value AND banks - 1), $A000-$BFFF the last bank,
  always; reads of $D5xx change nothing; bank 0 at power-on, kept by the
  Reset key (the cartridge port has no reset line, HRM 8.1).
- **Spec and fast version:** the bank register (`iCartBank`), `cartByte`;
  `xegs_banks.car` (our 32 KiB test cartridge, `make a800-asm`) in
  a800xl-spec, a800xl-specfast, the picture goldens.
- **Core:** `CkXegs n` (the configuration word's bits 1-2 = 3, bits 4-5
  n), the bank register (`ksCartBank`), a slow cartridge: `kiRdOk`, the
  CPU's read and ANTIC's fetches wait for its byte while the credit
  counts real time. The core lockstep gives the cartridge a random
  latency (about one read in eight 1 to 31 clocks late) on our three
  cartridges.
- **The Arty:** `A800.CartDdr` (the NES's `prgDdr` bookkeeping) reads
  the file the PS wrote at the media region (0x02000000, a .car's image
  after its 16-byte header) through HP0 (`a800xl_zynq.v`, `ps7_hp.tcl`).
  The PS writes the file into DDR and its first 64 KiB onto the link
  (the loader reads the header from there; an executable arrives whole).
  `make sim CART=…` puts the image in the harness's DDR;
  ci-arty-a800xl runs `xegs_banks.car`.
- **The ZedBoard:** the cartridge stays in block RAM; its loader and its
  PS app refuse XEGS.
- **Board check** (the user, the Arty, 2026-10-21): Lode Runner (XEGS
  128 KiB) plays, its first stage beaten; Miner 2049er (a raw 16 KiB
  cartridge) works; Scorch (XEX) works. `make bit`: WNS +2.764 ns,
  113.5 BRAM tiles (the 16 KiB cartridge block RAM gone on the Arty).

### Cassettes, depth 1 (side track Q, 2026-10-21)

The user's Extirpator is a `.cas`; PLAN.md's cassette plan, depth 1:
the OS's cassette call answered from the file, not the recorder.
- **`A800.Media`:** `parseCas` (the data chunks of Atari800's and
  Altirra's format, each a record with its sync bytes, control byte, data
  and checksum), `encodeCas` and `casBoot` (our test tapes),
  `sioChecksum`.
- **`A800.Spec.Machine.casTrap`** (and the fast version's copy): with a
  cassette in `Config` and the CPU about to run SIOV ($E459, the OS
  mapped) for DDEVIC $60 (AltirraOS's cassette handler stores it,
  `LDA #$60 / STA $0300` at $EE39; the HRM's $5F is the SIO bus's ID),
  a read (DCOMND $52) gets the next record's first DBYT bytes in DBUF;
  status, registers and CDEVIC-CAUX2 as atari800's SIO patch (its sio.c,
  values only); then the call returns. No write (nothing to record on).
- **Booting:** the OS boots a tape when START is held at power-on, then
  waits for a key; the desktop's library does both for a `.cas`
  (`A800.Spec.runCasBoot` for the tests).
- **Tests:** `cas_boot.s` (`make a800-asm`: `cas.cfg`, `a800-cas`) boots
  in a800xl-spec, a broken checksum refused; a800xl-specfast boots it on
  both machines; the desktop test too. The user's Extirpator reaches its
  title screen on the desktop.
- **The board** (2026-10-26, side Q; PLAN.md "Depth 1 on the board"):
  - **The answer is a program the CPU runs** (spec, fast version, core
    alike, so they match cycle for cycle): SIOV's read answers `JMP
    $D500`, and the empty $D5 page a program of 5-byte groups
    (`LDA abs`/`NOP`/`NOP` reading the DCB, `LDA #byte / STA addr`
    copying the record, the record's bytes as operands), 50 groups a page,
    then `JMP $D500`; every byte follows from its address, so an
    interrupt's dummy reads see the same bytes; it ends at the RTS's
    second cycle (ending at the RTS's opcode let an NMI's dummy reads end
    it early: Extirpator's "LOAD ERROR", now `cas_irq.s`).
    `A800.Spec.Machine.casRead` describes it.
  - **In the PL: `A800.CasUnit`, beside the core.** The core only routes
    the reads (`casRoute`: $E459 with the OS mapped; armed, $E45A, $E45B
    and the $D5 page) to the space `SpCas`; the unit answers, reads the
    tape image through the cartridge's DDR reader, and latches the DCB
    from the core's completed reads. Kept out of the core's state on
    purpose: carried through its sequencer, the unit's 120 bits took
    13,500 LUTs and the answer 12,000 more (60,133 of the XC7Z020's
    53,200); beside it the 800XL takes 38,426.
  - **The tape in DDR:** the PS makes the tape image (`a800xl_cas.zig`,
    `menu.System.ddr_image`; `link --load` too): "FUJI" and 12 zeros, a
    256-byte slot per record (its length, at most 255, then its bytes),
    an empty slot last (`A800.Media.casImageFile`). The loader knows a
    tape by its header (`CkCas`, the Arty only), the reader starts after
    it.
  - **Booting:** for a tape alone the system holds START for 60 frames,
    then presses Space at frames 120 to 125 (`A800.System`), as the
    desktop's library does. The OS then waits for the leader: "Pass" after
    about 1000 frames (`cas_boot`'s layer 4 run, once, 2.6 hours).
  - **RAM cleared on a new file** (found in the board check): the board's
    block RAM kept RAM over a load, and the XL's OS took the kept power-up
    bytes ($033D-$033F) for a Reset key press: a warm start, no tape boot,
    and after loading Scorch the tape program left in RAM ran again
    through CASINI. `A800.System` now clears all 64 KiB before the core
    starts (`ramClearStep`), as switching off does; a save state's load
    writes no file and keeps it.
  - **Board-checked by the user** (2026-10-26, Arty): Extirpator boots from
    its tape by itself (`make load`), and Scorch loads properly after it.
  - **Tests:** a800xl-lockstep's `cas_boot`, `cas_irq` and `cassio`
    (an executable reading `cas_data.cas` through SIOV, the OS's boot
    skipped) with the tape in a slow memory; ci-arty-a800xl's `cassio`
    run from the harness's DDR.
- **Depth 2** is below.

### Cassettes, depth 2: the 410 recorder itself (side track Q, 2026-10-29)

The user asked for it (2026-10-29). The tape plays through the recorder at
its speed, so custom loaders work as well as the OS's.
- **POKEY's serial input** (`A800.Spec.Pokey`, `A800.SpecFast.Pokey`; HRM
  5.6, figure 10). In asynchronous receive mode (SKCTL bit 4), timers 3
  and 4 are held reset while POKEY waits for a start bit. The line's
  falling edge starts them, and each timer 4 fire after that is half a
  bit:
  - the 1st samples the start bit, and SKSTAT bit 1 reads busy;
  - every second one after it samples a data bit, LSB first;
  - the 19th samples the stop bit. SERIN takes the byte, and IRQST bit 5
    is raised, or SKSTAT bit 5 (overrun) if it already is. A stop bit of
    0 sets SKSTAT bit 7 (framing). Then the timers are held again.

  SKSTAT bit 4 reads the line directly; the OS measures the tape's baud
  rate with it. Not modelled: input clocked by timer 4 or the external
  clock without asynchronous mode.
- **The deck** (`setTape`, both machines). The tape is the file's data
  records with their leading gaps (the chunks' aux, in ms) and baud rates
  (`A800.Media.parseTape`, `tapeLevel`). Each record is played as levels
  on SIO's data line: mark through its gap, then for each byte a start
  bit, eight data bits and a stop bit. The tape moves one machine cycle
  per cycle while the motor runs: PIA CA2 driven low, PACTL bits 5-3 =
  110 (the OS writes `$34`, and `$3C` to stop). Its level goes into POKEY
  every cycle, and the line idles at 1 otherwise. The OS waits about 10
  seconds of leader after starting the motor, so a test tape's leader
  can be cut only that far.
- **The core** holds timers 3 and 4 in asynchronous mode too. It has no
  serial input, so its line stays at 1 and the timers are always held;
  that keeps the lockstep with the spec through the OS's SIO calls. The
  board has no depth 2: its tapes are depth 1's.
- **Checked by the user** (2026-11-03, the desktop): `cas_boot.cas` boots
  to Pass, and Extirpator loads and runs after its 10-minute load.
- **The desktop:** `desktop --real-tape FILE.cas` (the library's kind 4).
  Extirpator's 278 records take about 10 minutes; Speed's "Unlimited"
  shortens that.
- **Tests:**
  - a800xl-spec: a byte in at 600 baud (SERIN, IRQ, busy, framing);
    Acid800's `pokey_asyncrecv`, now on the passing list; and, in the
    full tier, `cas_boot.cas` booting through the recorder, plus a
    broken record refused;
  - make test-desktop: the same tape with `--real-tape` ends on the
    instant load's screen.

## Follow-ups after M4 (recorded 2026-10-08)

M4 is done and merged (2026-10-07). What is left, in the order proposed
(the first one decided with the user, 2026-10-05; the rest proposals):

1. **XEX files and ATR disk images in DDR** (decided). Executables go
   into the media region (0x02000000 up, through HP0, as the Game Boy's
   ROM) instead of the 64 KiB block RAM: any size, faster loading, 16
   RAMB36 tiles back. The loader's port reads become DDR reads; the core
   waits for a miss (a machine cycle is about 14 pixel clocks and the core
   keeps 7 cycles of credit, so even a worst-case miss fits). Then ATR
   images: a PL-side disk drive on POKEY's serial port reading the image
   from DDR (the PS only loads files; it never emulates). A disk drive
   on the serial port also gives the four tracked serial tests that
   still fail their chance (pokey_serclock, pokey_sertiming, pokey_skstat,
   pokey_asyncrecv; the fifth, pokey_seroc, passes).
2. **The desktop keyboard.** `link --system a800xl` sends only the
   joystick (the `[a800xl]` key map); the 800XL's keys come only from
   the board's USB keyboard. The desktop could send the same eleven key
   bytes (`A800.Loader.keysBase`) over the link, through a new relay
   command (the PS app's, so side track I or the PS's owner, with M4).
3. **`make load` over the PS link on the Arty at 2 Mbaud** (a platform
   finding, not M4's): `link` sends a block at full speed and the relay
   forwards each byte to the PL more slowly, so its UART FIFO overflows
   (the relay replies `e…`). `LINK_ARGS=--slow` works; the fix belongs in
   `link` (pacing, or acknowledged chunks) and serves CHIP-8 and the NES
   too. Loads into DDR (Game Boy) are not affected.
4. **The ZedBoard bitstream of the final tree** (waived by the user,
   2026-10-07: the ZedBoard was out of reach): `make bit SYSTEM=a800xl
   BOARD=zedboard` once it is back, then a short board check there (the
   last ZedBoard build, before the refactor onto `A800.System`, met timing
   at WNS +0.884 ns; its layer 4 runs on the final tree are green).
5. **The core lockstep's full tier.** `A800.Core` runs about 16k cycles
   per second in plain Haskell; the quick tier covers our programs and
   random programs, and BASIC's boot/typing and the Acid800 core runs are
   in the full tier (`EMU_FULL_TESTS=1`, hours), not yet run in full.
   Making the harness faster (forcing less often, a strict state record)
   would let more of it into the quick tier.
6. **Known limits** kept as they are unless asked: NTSC only (PAL out of
   scope), standard 8/16 KiB cartridges only (no bank-switched types), no
   130XE memory, no paddles (pots read 228), the picture cropped to the
   normal playfield (wide playfields and overscan lost), no NTSC artifact
   colours, the Arty's console keys only from the keyboard (F2-F4).

## Steps

PLAN.md, "Milestone 4", has the table. Notes per step:

- **4.0:** fetch and pin AltirraOS, Altirra BASIC, Acid800 (or what
  exists), the homebrew game; check licences; XEX and CAR parsers with
  tests; `make a800-asm`; freeze the required-test list.
- **4.1:** the spec without video: memory map with PORTB banking,
  the PIA, the CPU through `Mos6502.Spec`'s NMOS variant with a stall
  input; settle the HALT question (above) first. Gate: the OS boots into
  BASIC ("READY" at SAVMSC); banking regression ROMs.
- **4.2 to 4.4:** ANTIC, GTIA, POKEY in the spec, each gated by its
  required tests and unit tests (DMA patterns per mode, polynomial
  counters against a reference sequence).
- **4.5, 4.6:** Clash units then the whole core in lockstep, as
  `nes-lockstep`: deliberate bugs caught; the long runs move to layer 4.
- **4.7:** display, audio and input in the top; layer 4 goldens; every
  other system's goldens unchanged.
- **4.8:** wrapper, PS link, menu row, key path, loader stub. This step
  touches the PS app, which side track I owns: additively (a table row,
  a key module), and if I's PS phases have not merged by then, stop and
  ask before starting it.
- **4.9:** the exit board check (PLAN.md).

## Ownership

M4 owns `systems/a800xl/` and this file, `boards/zedboard/a800xl_zynq.v`
(new); at step 4.8 additively the PS app's system table row and a new key
module, `keymap.zig`'s new section, `tools/mksd.sh` (`/a800xl`). Shared:
`cpus/mos6502` (stop and ask), `Platform.Ps.Link` constants (additive).
Must not touch: `systems/gb/`, `systems/nes/`, `boards/arty-z7-20/`.

## Status log

*(fill in per completed step: date, commit, test results, WNS, utilisation)*

- **4.0 (2026-10-04):** ROMs and tooling. `tools/fetch-roms.sh` fetches
  and hash-checks AltirraOS, Altirra BASIC, 56 Acid800 standalone tests and
  Scorch (licences above); `A800.Media` (XEX segments with INITAD/RUNAD,
  CAR types 1 and 2, raw 8/16 KiB images), `A800.Screen` (screen text),
  `A800.Acid800` (the frozen list, verdicts); `make a800-asm` (XEX and
  8 KiB cartridge layouts, `pass.s`, `cart_pass.s`). The HALT question
  is settled without changing `cpus/mos6502` (above). Gate:
  `make test SYSTEM=a800xl`: a800xl-spec 22 tests, platform-unit 93,
  all passing.
- **4.1 (2026-10-04):** `A800.Spec` without video: `A800.Spec.Machine`
  (memory map, PORTB banking with the pull-ups, self-test, writes to ROM
  lost, cartridge priority and RD5/TRIG3, the PIA's ports and control
  registers, the cycle shared with ANTIC: halted cycles run inside the
  CPU's bus callbacks, WSYNC as RDY), `A800.Spec.Antic` (beam counters,
  VBI/DLI NMI timing with the late-NMIEN case, NMIST/NMIRES, VCOUNT incl.
  the end-of-frame anomaly, WSYNC incl. the INC WSYNC cases, refresh
  cycles at 25 + 4n), `A800.Spec.Pokey` (64/15 kHz clocks, timers with
  the 3-cycle reload pipeline and linked pairs, STIMER, IRQEN/IRQST, the
  serial output shifter and its IRQs, RANDOM's generators),
  `A800.Spec.Gtia` (read registers, TRIG, CONSOL, PAL); the XEX loader
  (above); `a800-run` (trace, screen, RAM). AltirraOS boots into Altirra
  BASIC ("Altirra 8K BASIC 1.59", "Ready") after 4 to 6 million cycles:
  BASIC's header asks for a disk boot, and with no drive the OS retries
  its SIO command until it gives up, which needs POKEY's serial output
  IRQs (the send loop has no timeout) and the VBI's countdown timer.
  Regression programs `banking.s` (9 checks; a seeded bug, the self-test
  visible with the OS off, gives `FAIL 04`) and `cart_banking.s`. 22
  Acid800 tests pass already and are held by a ratchet in `a800xl-spec`
  (CPU 8, `pia_basic`, `mmu_xlbanking`, and those that hold without
  video: antic_default/addrmirror/vcount/wsync, gtia_default/consol,
  pokey_default/addrmirror/noise/timerirq/timergranularity/seroc).
  Gate: `make test SYSTEM=a800xl`: a800xl-spec 51, mos6502-spec 303,
  mos6502-lockstep 300, platform-unit 93, all passing; `cpus/mos6502`
  unchanged.
- **4.2 (2026-10-04):** `A800.Spec.Antic` complete: display list (LMS,
  jumps, JVB, DL DMA off = repeated IR, the 1K wrap), the per-cycle DMA
  clock with start/stop latching at their decision cycles and the
  abnormal (unstopped) patterns rotating 2 per line, playfield name/
  bitmap/character data fetches (virtual past 105, on 0, or with the
  width off: they capture the bus after the CPU's access), refresh (25 +
  4n, deferred to the next free cycle, one at a time; the 9th to the first
  free one), P/M DMA and phantom P/M DMA, the line buffer (48 bytes, 63
  positions), vertical scrolling (both deadlines), horizontal scrolling
  (DMA delay + one-colour-clock delay), the hires bug, and the playfield
  as AN codes per colour clock (HRM table 17), shown 4 cycles after a
  bitmap fetch and 3 after character data. Unit tests per mode/width
  ("ANTIC DMA", from the HRM's charts: e.g. mode 2's one refresh at 98).
  18 of the 20 required ANTIC tests pass; the other two need the shared
  6502 (see "Shared 6502" below; they pass since 4.5).
- **4.3 (2026-10-04):** `A800.Spec.Gtia` on the AN bus: register writes
  queued to their colour clock (colours +1, PRIOR +2, GRAF/SIZE/HITCLR/
  P/M DMA +3, HPOS +5; carried over line ends), the eight P/M shifters
  with Altirra's size state machine (incl. the %10 lockup), collisions,
  the HRM's priority equations, hires luminance, GTIA modes 9-11 and
  pseudo mode E (the hires latch at the end of horizontal blank), trigger
  latch, CONSOL. `A800.Palette` (our own NTSC model) and `A800.Picture`
  (320 × 240 crop). Goldens: BASIC's Ready screen, `scene.s` (all modes,
  players, priority, GTIA mode 9 by DLI), checked by eye. All 11 GTIA
  tests pass.
- **4.4 (2026-10-04):** `A800.Spec.Pokey` completed against Acid800:
  the IRQ line one cycle behind IRQST, counters not counting in their
  reload cycle, two-tone resync, the 15/64 kHz phases after init mode
  (Altirra's: first IRQ 84/25 cycles after the SKCTL write; the HRM says
  83/24), the channels' audio (poly5 gate, square/noise, high-pass,
  volume-only, STIMER), the keyboard scan (Altirra's debounce state
  machine, SHIFT/CONTROL/BREAK latches), `A800.Mixer` (the uneven DAC bits
  and the saturating curve), `A800.Keyboard` (key codes). PIA CA2/CB2
  edges (spurious interrupts) for pia_irq. Tests: POKEY timer timings
  per cycle (figure 8, Acid800's), `tone.s` measured at 437.8 Hz, BASIC
  typed through the key matrix (`PRINT 2+2` → 4). All 9 required POKEY
  tests and both PIA tests pass. Not modelled: pots (a constant 228,
  ALLPOT 0: no paddles), serial input (no SIO devices: the 5 tracked
  serial tests).

- **4.5 (2026-10-05):** the Clash units `A800.Pokey`, `A800.Gtia`,
  `A800.Antic` (pure step functions, state in one record each), each in
  lockstep with its spec through the machine's event hook
  (`a800xl-lockstep`: `LockPokey`, `LockGtia`, `LockAntic`; per tick:
  IRQ, level, serial bytes; register reads and every picture byte; bus
  use, each DMA address, P/M data, WSYNC, NMI, AN codes). ANTIC's cycle
  is split into ticks (begin, fetch, rest of the special cycle, maybe a
  playfield fetch, the CPU's access, end, a second decode), so each
  tick does at most one memory access and one decode position; WSYNC
  and CHBASE's delay count cycles instead of using absolute times.
  Quick tier: BASIC boot/typing/tone.s (POKEY), the Ready screen,
  scene.s, the new `rasterbars.s` (mid-line colour writes; also a spec
  golden) for GTIA, Ready and scene.s for ANTIC; the Acid800 groups in
  the full tier. Seven deliberate bugs, each caught (64 kHz clock period,
  SHIFT dropped, colour writes a clock late, priority mode, a DLI's NMI
  a cycle late, a refresh at 24, CHACTL's inverse bit). With the shared
  6502's 800XL rules (below) all 51 required Acid800 tests pass in the
  spec. Gate: `make test SYSTEM=a800xl` (a800xl-spec 99, a800xl-lockstep
  15, mos6502-spec 303, mos6502-lockstep 304, platform-unit 93) and
  `make test SYSTEM=nes` (all 6 suites, unchanged), all passing.

- **4.6 (2026-10-05):** `A800.Core`, the whole machine in Clash: one pure
  step per clock (`coreStep`), memories outside (`koMem`/`kiRd`: RAM, OS,
  BASIC, cartridge, loader page, executable), a machine cycle in at most 13
  clocks (start, ANTIC's fetch, the rest of its special cycle, maybe a
  playfield fetch, WSYNC's question, the CPU's access and `cpuCycle` with
  `coreSally`, ANTIC's end and second decode, GTIA's two colour clocks),
  plus the PIA, PORTB banking and the loader page/port. DMA cycles halt the
  CPU entirely (it never sees them, as in the spec). `LockCore`
  (a800xl-lockstep) compares each machine cycle (bus access, POKEY's
  level, beam, the four half-clock colours) and the registers after each
  step: scene.s, rasterbars.s, tone.s, banking.s (3M cycles each), five
  random programs (random OS and BASIC images, JAMs as NOP), two
  deliberate core bugs caught (WSYNC ignored, AUDC writes lost); BASIC
  boot/typing and Acid800 in the full tier. The lockstep runs about 16k
  cycles/s; the suite's heap is capped at 4 GB (a GTIA harness leak found
  that way: lazy Vec elements, now forced every 256 cycles). Gate: `make
  test SYSTEM=a800xl` (a800xl-lockstep 26, a800xl-spec 99, a800xl-golden 8,
  mos6502 303 + 304, platform 93), all passing.

- **4.7 (2026-10-05):** `A800.Top` (the ZedBoard top, PS-link ready:
  overlay, pause, system ID 5), `A800.Display` (the machine-cycle enable,
  305328211 per pixel clock; three frame buffers of 160 colour-clock
  words × 240 lines, each word the two half-clock colours; the palette
  ROM; ×2 into 640 × 480), `A800.Audio` (the mixer curve as a ROM, two
  low-passes, the AC coupling, 16-bit PCM to S/PDIF), `a800Core` (the
  core with its memories in block RAM), `a800-image` and
  `systems/a800xl/zedboard.mk` (`XEX=`). Input for now: the buttons are
  joystick 1 (BTNC the trigger), SW0-2 hold START, SELECT, OPTION; the
  keyboard comes with 4.8. `a800xl-golden`: VGA and HDMI goldens from the
  spec goldens, the I2C log, the Clash display path = the Ready golden.
  Layer 4 (`ci-a800xl`): scene.s and rasterbars.s identical on VGA and
  HDMI, tone.s decoded at 437.8 Hz from S/PDIF; with FULL=1 BASIC's Ready
  screen (200 frames, 12 minutes; identical). Found at layer 4: the core
  must start from the configuration word as read from block RAM (held for
  two clocks; regression: the scene.s run). Gate: `make ci` (quick tier):
  OK, every other system's goldens identical; 51 minutes with the 800XL
  (its Clash run and four simulations).

- **4.8 (2026-10-05):** on the PS link. `A800.Loader`: an XEX, a CAR
  or a raw 8/16 KiB cartridge from link address 0 while held (told apart
  by first bytes and length; refused: the core stays held, LD0 off), the
  keyboard's eleven bytes at 0x100000 while running; `A800.Top` on it
  (the core starts two clocks after hold falls, with the loaded
  configuration: found at layer 4); `boards/zedboard/a800xl_zynq.v`
  (`ps7.tcl`, no HP port). PS app (additively): the system table's row
  (ID 5, `/a800xl`, `.xex .car .rom`, `a800Check`), `a800_keys.zig`
  (USB boot reports to the key bytes; the layout under "Input"), the
  `[a800xl]` key map (joystick), the relay writing the bytes when they
  change; `link` loads the 800XL's files (`make load SYSTEM=a800xl`);
  `/a800xl` and `a800xl.bit` on the card (`make sd`). Tests: loader unit
  tests (a800xl-spec), `make test-zig` 361 pass; layer 4: scene.xex
  loaded over the link and `keys.s` with A held over the link, both
  identical on VGA and HDMI (`ci-a800xl`). `make bit SYSTEM=a800xl`: WNS
  +0.884 ns, WHS +0.121 ns, 37739 LUTs (70.9 %), 107.5 BRAM tiles, 12
  DSPs, no critical warnings. Awaiting the board check (4.9).

- **4.9 (2026-10-07):** the exit, board-checked by the user on the Arty
  Z7-20 (the main board since 2026-10-07; JP4 on JTAG for `make prog`,
  on SD for the boot): BASIC's READY prompt, a short program typed on the
  wireless keyboard runs (also on the ZedBoard, 2026-10-05); ten of the
  required Acid800 tests show "Pass" on the hardware (antic_wsync,
  antic_dlitiming, antic_blockednmi, antic_dmapattern, gtia_collision,
  gtia_pmretrigger, pokey_timertiming, pokey_irqtiming, cpu_insn,
  pia_irq; all 51 pass in the spec, and the core runs in lockstep with
  it); Scorch (homebrew, its licence checked in 4.0) from the SD card's
  `/a800xl` menu after SD boot, with sound on the monitor (HDMI) and the
  jack, the joystick from keys. Found on the way: the executable RAM
  doubled to 64 KiB (Scorch is 56 KiB); `make load` over the link needs
  `LINK_ARGS=--slow` on the Arty (STATUS); `make prog` on the ZedBoard
  with the jumpers on SD collides with the card's boot.

### Shared 6502: the 800XL's interrupt rules (decided 2026-10-05)

Two required tests need 6502 behaviour that differs from the NES's, both
rules Altirra models and Acid800 measured on hardware:

- **antic_blockednmi:** an NMI that starts exactly on the cycle the CPU
  pushes P during an IRQ/BRK sequence is lost (Altirra:
  `kStateCheckNMIBlocked`, HRM 4.8 "Missed NMIs", blamed on ANTIC's short
  NMI pulse). On the NES the PPU holds NMI low for a long time; the model
  keeps it pending and takes it after the handler's first instruction
  (AccuracyCoin, Blargg's cpu_interrupts_v2).
- **antic_dlitiming** (the WSYNC case): an NMI that arrives while RDY
  holds the CPU in an instruction's last cycle is taken right after that
  instruction (Altirra: `NegateRDY` makes it eligible). The NES model
  polls interrupts only on unstalled cycles (the 2A03's DMA rule,
  AccuracyCoin "Interrupt flag latency"), so it runs one more instruction
  first.

The user agreed (2026-10-05) to opt-in flags: `cfgStallPolls` and
`cfgPushBlocksNmi` in `Mos6502.Spec.Config`, `ccStallPolls` and
`ccPushBlocksNmi` in `Mos6502.Core.CoreConfig`, on only in the new
presets `sally6502` / `coreSally` (the 800XL's); `nmos6502`,
`ricoh2a03` and their core twins are unchanged, so the NES and
SingleStepTests suites are untouched. `mos6502-lockstep` checks the core
against the spec with the flags on (random programs, and fixed scenarios
with irregular short NMI pulses, a held IRQ and short RDY windows), and
that a core with either flag off is caught.
