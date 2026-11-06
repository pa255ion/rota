# Game Boy (DMG)

Target: the **original Game Boy (DMG, CPU revision as in DMG-ABC)**.
Game Boy Color, Super Game Boy and the link cable are out of scope for
milestone 2. Written during the 2026-10-02 planning session, before any
code: everything here is a plan, and the parts marked *to verify* must be
checked against Pan Docs or the cited source before code depends on them.
Milestone 2 is done (2026-10-03, with sound): where a section says "as
built", or the status log at the end disagrees with the plan, those win.
Side track I (planned 2026-10-04) adds battery saves, cartridges in DDR
and the Game Boy Color: "Side track I" below.

## Machine summary

| Item | Value |
|---|---|
| CPU | Sharp SM83 (8080/Z80-like), 8-bit, 256 base + 256 `CB` opcodes |
| Clocks | 4.194304 MHz master ("dots", T-cycles); CPU works in M-cycles of 4 dots (1.048576 MHz) |
| Frame | 154 lines × 456 dots = 70224 dots → 59.7275 Hz |
| Display | 160×144, 2 bits per pixel through palettes (BGP, OBP0, OBP1) |
| Memory | 8 KiB WRAM, 8 KiB VRAM, 160 B OAM, 127 B HRAM, cartridge ROM (32 KiB to 8 MiB via MBCs) and RAM (0 to 128 KiB) |
| Video | PPU with background, window, 40 sprites (10 per line), modes 0 to 3, STAT/LYC interrupts, OAM DMA |
| Timer | DIV (16-bit internal counter), TIMA/TMA/TAC, with the falling-edge behaviour Mooneye tests |
| Input | 8 buttons in a 2×4 matrix (P1/JOYP) |
| Sound | APU: 2 pulse, 1 wave, 1 noise channel (milestone step 2.9) |

References: Pan Docs (gbdev.io/pandocs), the gbdev opcode tables, Gekkio's
"Game Boy: Complete Technical Reference" (for M-cycle access order and
timer details), and the test suites below.

## Accuracy target

- **CPU:** M-cycle accurate, meaning every memory read and write happens in
  the same M-cycle as on hardware. This is what `instr_timing` and
  `mem_timing` check, and what lets the lockstep test compare bus
  accesses, not just retired instructions.
- **PPU:** dot-level mode timing (OAM scan 80 dots, mode 3 variable,
  HBlank the rest of 456). Start with the pixel-FIFO structure Pan Docs
  describes, so mode 3's length comes out of the model instead of being a
  table. dmg-acid2 doesn't need exact mode 3 length; Mealybug Tearoom
  (mid-line register writes) is a stretch goal, not an M2 requirement.
- **No boot ROM.** The core starts in the documented post-boot state of a
  DMG (CPU registers, I/O registers: Pan Docs, "Power Up Sequence").
  The system counter starts at 0xABC8, so that Mooneye's
  `boot_div-dmgABCmgb` and `boot_sclk_align-dmgABCmgb` pass (Pan Docs
  only gives DIV = 0xAB). The PPU phase at hand-off (LY, STAT; Mooneye
  `boot_hwio-dmgABCmgb`) is step 2.2.
  Mooneye tests that check boot-ROM-specific state are skipped and listed
  as such. Nintendo's boot ROM is never fetched or committed. SameBoy's
  boot ROM (MIT) is an option later; Bootix (CC0) exists only as mirrors
  now, so don't depend on it.

## Test ROMs (fetched by `tools/fetch-roms.sh` into `roms/gb/`, never committed)

| Suite | What | Pass signal | Licence (checked when pinned, 2026-10-02) |
|---|---|---|---|
| Blargg `cpu_instrs` (11 sub-ROMs + combined) | every opcode and flag | text on screen and on the serial port: "Passed" | no formal licence; freely distributed by the author (treat like Space Invaders: fetch, don't commit) |
| Blargg `instr_timing`, `mem_timing`, `mem_timing-2`, `halt_bug` | M-cycle timing | same | same |
| Mooneye test suite, acceptance subset (timer, interrupts, `ei`/`di`/`halt`, OAM DMA, PPU timing later) | precise behaviour | B, C, D, E, H, L = 3, 5, 8, 13, 21, 34 at the `LD B,B` breakpoint; fail = all 0x42 | MIT (LICENSE in the release archive) |
| dmg-acid2 | PPU rendering (priorities, window, sprites, 8×16, flips) | screen equals the published reference image | MIT (GitHub repo licence) |
| Blargg `dmg_sound` | APU (step 2.9) | serial/screen text | as Blargg above |
| Mealybug Tearoom | mid-scanline PPU effects | reference images | MIT (stretch) |

Test hooks the spec and the Clash simulations both need:

- **Serial capture:** a write of 0x81 to SC sends SB; collect those bytes
  as text. Blargg's ROMs print their results this way, so a test can check
  for "Passed" without looking at the screen.
- **`LD B,B` breakpoint** (opcode 0x40): stop and check the Mooneye
  register pattern.
- **Screen goldens:** 160×144 PNGs in `systems/gb/test/golden/spec/`,
  checked by eye once (dmg-acid2 against its own reference image), then
  frozen; the layer 4 goldens are those ×3 and letterboxed, as for CHIP-8.
- Our own regression ROMs, written in assembly with `rgbds` (added to
  `flake.nix` in step 2.0), assembled by the build, never hand-patched
  binaries.

## Reference model (`Gb.Spec`)

Milestone 1's spec was instruction-level and slow-but-clear. This one has
to be cycle-timed and fast: `cpu_instrs` runs for tens of millions of
M-cycles. Keep a pure interface (`stepM :: Inputs -> Machine -> (Machine,
[Event])` or similar, one M-cycle per step) but feel free to implement the
state with unboxed mutable vectors in `ST` behind it. Measure: the full
`cpu_instrs` set should finish in well under a minute on the spec, or the
test layer becomes painful.

Events the spec emits per M-cycle (the hardware core must emit the same on
its debug port, like CHIP-8's `coEvent`): bus access (address, data,
read/write), instruction boundary with register file, interrupt dispatch,
PPU mode change and LY change. Lockstep then compares bus traces, which
catches timing bugs that a per-instruction comparison would miss.

**As built in step 2.1** (`Gb.Spec`, `Gb.Spec.Cpu`, `Gb.Spec.Machine`):

- State in unboxed mutable vectors in `ST`; `step` runs one instruction
  (or one interrupt dispatch, or one halted M-cycle) and `withTrace`
  records its bus trace, one `Access` (read, write, idle) per M-cycle.
  The pure interface is `runRom`: run until a Blargg serial verdict,
  Mooneye's `LD B,B`, or a cycle limit. Instruction boundaries are the
  `step` calls; PPU events come with the PPU (2.2). The full
  `cpu_instrs` runs 56 million M-cycles in about a second.
- Each M-cycle first advances the timer, serial port and LCD timing, then
  does the CPU's access, so a value produced in an M-cycle is visible to
  the access in it (this gives Pan Docs' TIMA reload cycles A and B).
- **The opcode fetch is the first M-cycle of an instruction**, and the
  interrupt check uses IE and IF *after* that fetch's tick: if an
  interrupt is taken, the fetched byte is discarded and the fetch counts
  as the first of the dispatch's 5 M-cycles. While halted, the CPU keeps
  fetching at PC every M-cycle; the fetch in which an interrupt becomes
  pending starts the dispatch (IME = 1) or is the next instruction's
  fetch (IME = 0). Mooneye's `rapid_toggle`, `di_timing-GS` and
  `halt_ime*` tests pin these down; the Clash core must draw the same
  boundaries (step 2.4).
- **PPU (step 2.2, `Gb.Spec.Ppu`)**: dot level, 4 dots per M-cycle; the
  CPU sees the mode of the next dot. Mode 3 is not a pixel FIFO but a
  schedule with Pan Docs' documented mode 3 length: 12 + SCX % 8 dots
  before the first pixel, 6 at the window start, and the "OBJ penalty
  algorithm" per object (X = 0 counts as its own tile). Pixels are coloured
  when output, from the registers at that moment, so mid-line effects are
  only approximate (Mealybug stays a stretch goal). Timing details pinned
  by Mooneye, all in the module comment: LY shows the next line from dot
  452; the LY = LYC flag is latched, clear while LY changes and frozen with
  the LCD off; VRAM/OAM read and write cut-offs differ (OAM reads blocked
  from dot 452 of the previous line, VRAM reads from dot 76, OAM writes
  allowed in the last M-cycle of mode 2); after LCD on the first line has
  no mode 2 and M-cycle boundaries fall on dots 3, 7, ...; line 144 raises
  the mode 2 STAT interrupt with VBlank. Hand-off at line 153, dot 400
  (`boot_hwio` passes for dots 256 to 452).
- **OAM DMA**: starts 2 M-cycles after the FF46 write, 160 M-cycles, OAM
  blocked meanwhile, restart keeps the old copy until the new one starts.
  Other bus conflicts are not modelled.
- Cartridges: no MBC and MBC1 (MBC5 comes with step 2.6); anything else
  is refused by `newMachine`.
- `runRom` also has a `Frames n` mode, and every result carries the last
  complete frame (`resScreen`); `gb-run frames=N <rom> --png out.png`
  writes it.

## Hardware design

### Clocking

Everything stays in **`DomPix`** (25.176 MHz), with a fractional clock
enable for the Game Boy's dot clock: a phase accumulator adds
4194304 / 25176230 of a full turn per pixel clock and fires the dot enable
on overflow (about every 6.0 pixel clocks; the error is a few ppm, not
audible or visible). This keeps rule 6 (one clock domain per system) and
needs no clock-domain crossing between core and video, exactly as for
CHIP-8.

Useful side effect: each dot has about 6 fast clocks, and each M-cycle
about 24. BRAM's one-cycle read latency and time-sharing a memory port
between the CPU and the PPU are therefore easy, without the pipelining a
core running at the native clock would need.

The price (measured 2026-10-04, side track E's bitstream): the core's
worst path fills most of a pixel clock, 37.5 ns of 39.72 ns (WNS +1.527
ns), 40 logic levels from a core register to a block RAM address. Any
logic added to that step (Game Boy Color features, say) needs the path
split over two clocks first; there are about 6 per dot to spend.
docs/ARCHITECTURE.md, "Resource budgets", has the other systems' margins.

### Frame-rate mismatch

The Game Boy draws at 59.7275 Hz, the 640×480 output runs at 59.94 Hz.
Built in 2.7 with **two framebuffers**, swapped at the core's VBlank and
picked up at the display's VBlank. **That tore** (seen on the board,
2026-10-03): after a swap the core drew into the buffer the display was
still showing; two buffers are only enough when the reader follows the
writer, and here the rates drift. Now (2026-10-03, board-checked): **three buffers** (`Gb.Display.Banks`, 3 × 160×144×2 bits in
three block RAMs since M3 step 3.7, `Platform.Video.FrameBuffers`), the core always writing the one that is neither on screen
nor the newest complete one; one frame is shown twice about every 4.6 s
(59.7275 Hz Game Boy vs 59.94 Hz output) and nothing tears. `gb-unit`
checks the invariant on 2000 random sequences of frame ends and VBlanks. Later option: retune the MMCM so the display runs at the
Game Boy's rate (≈ 25.08 MHz pixel clock); check monitor tolerance first.

### Memories (budget)

| Memory | Size | Implementation |
|---|---|---|
| Cartridge ROM | up to 256 KiB (64 RAMB36) at first; 32/64 KiB for most test ROMs | `blockRam`, written by the loader port (`ciRomWrite`, 20-bit address) |
| Cartridge RAM | up to 32 KiB (8 RAMB36) | `blockRam`, no battery saves in M2 |
| WRAM, VRAM | 8 KiB each | `blockRam` |
| OAM, HRAM | 160 B, 127 B | registers or LUTRAM (asyncRam is fine at this size) |
| Framebuffers | 2 × 23040 × 2 bits | `blockRam` |

Roughly 80 of the 140 RAMB36, leaving room. Bigger cartridges need loader
stage C (DDR through an AXI HP port), which is a later milestone.

### Display path

160×144 ×3 = 480×432, letterboxed in 640×480 at (80, 24): integer scale,
counters that wrap every 3, no division. Palette selectable by a switch:
grey (0x000/0x555/0xAAA/0xFFF in 12-bit) or the classic DMG green (values
*to choose*). Output on VGA and HDMI from the same stream (M0d). The OLED
can't show 160×144; it may show debug text later (PC, LY, ROM bank).

### Input

The core's 8 buttons are bits 0 to 7 of `ciKeys`, ORed from:

- **Desktop keyboard (confirmed by the user, 2026-10-02):**

  | Game Boy | Up | Down | Left | Right | B | A | Start | Select |
  |---|---|---|---|---|---|---|---|---|
  | Key | W | S | A | D | J | K | L | Left Shift |

  Exactly these keys; no extra aliases (arrows etc.) unless the user asks.
  These keys mean something else in CHIP-8's map (`1234/QWER/ASDF/ZXCV`),
  so the desktop link needs a per-system key map: `make keys SYSTEM=gb` /
  `make load SYSTEM=gb` select a GB map in `platform/ps/keymap.zig`,
  with CHIP-8's as the default so existing commands keep working. Left
  Shift is read as a plain key through evdev (`KEY_LEFTSHIFT`), not as a
  modifier. Same EMIO GPIO 0 to 7 as CHIP-8's keys. Since side track B
  this is the `[gb]` default of the key-map data (`link --system gb`);
  `make keymap SYSTEM=gb` changes it, `make show-keymap SYSTEM=gb` shows
  the map in force.
- **Board buttons** (proposal, not yet reviewed by the user): BTNU/D/L/R =
  D-pad, BTNC = A. B, Select and Start only come from the desktop.
- **Gamepad** (proposal, not yet reviewed; the user's PS4 pad isn't
  recognised on their PC yet): D-pad or left stick = D-pad, Circle = A,
  Cross = B, Options = Start, Share = Select.

### ROM loading

As built (side track A, 2026-10-03; step 2.8 had `Platform.Loader` and
the relay's `M` command, both gone now): the Game Boy is on the shared PS
link (`Platform.Ps.Link`, docs/ARCHITECTURE.md). `Gb.Loader`: the
cartridge ROM (256 KiB block RAM) at 0x000000 to 0x03FFFF of the link,
written only while held. `Gb.Top`: the menu's text overlay on the pixel
stream; the control register's pause holds the dot enable low (the whole
machine stops with its state; `Gb.Core` is unchanged; `gb-lockstep`
checks frozen-then-identical on Blargg `instr_timing`), and the keys
don't reach the core while paused; `PS_IN` reports `systemGameBoy` (2),
the buttons and switches (`gb_zynq.v` wires it).

From the SD card: the menu (`menu.zig` system table) lists `/gb` (any
case; subfolders too) for `.gb` and `.gbc`, refuses files over 256 KiB
and headers it can't run (unsupported MBC, Game Boy Color only), and
streams the cartridge in (about 1 s for 256 KiB). `make sd` fills `/gb`
with `SD_GB_ROMS` (default: `roms/gb/games`, `dmg-acid2`, `blargg`, each
as a subfolder). In a game, hold **BTNU and BTND together** for a second
to reopen the menu (`System.menu_combo`): BTNC is the Game Boy's A
button and often held, while a real D-pad can't press Up and Down at
once, so no game asks for it (the core does see Up+Down for that second;
confirmed by the user 2026-10-03, may be revisited later). CHIP-8 keeps
BTNC alone. Over J14: `make load
SYSTEM=gb ROM=…` sends 16 KiB blocks with the wide `L` command (24-bit
address). At the link's 230400 baud (about 23 KB/s), a
32 KiB ROM loads in about 1.5 s and 256 KiB in about 11 s. The cartridge
header (type, ROM and RAM size) selects the MBC: no MBC and MBC1 first,
MBC5 next; anything else is reported as unsupported on the error LED.

### Serial port to the desktop (side track A step 4, built 2026-10-03)

Every byte the game sends on its serial port (`koSerial`) goes on the PS
link's byte channel (`Platform.Ps.Link.psByteOut`/`psInWith`: the byte in
`PS_IN` bits 24 to 31, a toggle in bit 5 per byte). The relay polls it
every loop (`pl.zig` `ByteIn`; reads until two agree), makes lines
(`Lines`: at '\n', 64 characters, or after 0.2 s) and sends
`serial: <line>` over J14; `make keys SYSTEM=gb` (and `make load
SYSTEM=gb`) prints them, so Blargg's "Passed" shows on the PC. Layer 4:
`serial_pass.gb`'s "Passed" through the generated Verilog
(`--expect-ps-text`).

### Switches (proposed, per top-level as usual)

| Switch | Function |
|---|---|
| SW0 | palette: off = grey, on = green |
| SW6 | debug-byte select for LD0 to LD7 (as CHIP-8) |
| others | reserved |

### Sound (step 2.9)

Built (2026-10-03): HDMI audio through the ADV7511's S/PDIF input, no
codec (the ADAU1761 stays an option for line out).

- **APU**: `Gb.Spec.Apu` (reference, after Pan Docs; Blargg `dmg_sound`
  01 to 08 and 11 pass) and `Gb.Apu` (Clash, in lockstep with it inside
  `Gb.Core`). Not modelled: wave RAM access while CH3 plays (`dmg_sound`
  09, 10, 12), zombie mode, CH3 trigger corruption, the high-pass filter.
- **Mix**: per side, the sum of the enabled channels' DAC outputs × (NR50
  volume + 1), ±480 (`apuMix`, `koAudio`); ×64 as 16-bit PCM.
- **S/PDIF** (`Platform.Audio.Spdif`): IEC 60958 consumer frames (PCM,
  48 kHz, 24-bit words carrying the 16-bit sample), biphase-mark cells at
  6.144 MHz from a phase accumulator in half pixel clocks, out through an
  ODDR (edges within 19.9 ns, 0.12 cell). The sample is taken at each
  frame start, i.e. the APU output is point-sampled at 48 kHz (aliasing of
  high square waves is audible as in simple emulators; a filter is a
  later option).
- **ADV7511** (`adv7511HdmiAudioTable`, sources per entry): HDMI mode
  instead of DVI, S/PDIF input, N = 6144 with measured CTS, AVI infoframe
  (RGB, 4:3) and audio infoframe (2 channels); the sample rate and word
  length come from the stream's channel status.
- **Tests**: round trip and channel-status tests against an independent
  decoder (`platform-unit`), the register table on the I2C slave model,
  and in Verilator a Zig S/PDIF receiver (`platform/sim/spdif.zig`) on
  `tone.gb` (`make ci`: 439.8 Hz expected, 440.6 Hz measured, 0 errors,
  48 kHz exactly).

## Side track I: saves, larger cartridges, Game Boy Color (planned 2026-10-04)

Planned in the 2026-10-04 planning review (claude.ai) at the user's
request; steps and gates in PLAN.md, "Side track I". The decisions
below were confirmed by the user the same day.

**Battery saves (phase 1).**

- **Where:** `/saves/<system folder>/<ROM path below it>.sav`; for a
  cartridge at `/gb/games/x.gb` that is `/saves/gb/games/x.sav`. The ROM
  folders stay read-only and the menu has nothing to hide.
- **No FAT writes on the board.** `make sd` creates each save file at
  full size for every cartridge whose header says it has a battery
  (types with BATTERY; size from the RAM-size byte; MBC2's 512 × 4 bits
  stored one nibble per byte; MBC3's clock footer added, phase 2). The PS
  rewrites the file's blocks in place, as `last.txt`. So only cartridges
  put on the card by `make sd` get a save slot: without a card reader
  that is the only way onto the card anyway. A blank save holds what the
  core's cartridge RAM holds at power-on (documented when built).
- **`make sd` must not lose saves:** it pulls `/saves` to
  `$XDG_DATA_HOME/emulation_clash/saves/` first (`make saves-pull` does
  only that), refuses to erase the card if the pull fails (`SAVES=skip`
  overrides), and writes the stored saves into the new image.
- **When saves are written:** after about 2 s with no cartridge-RAM
  writes, when the menu opens, before another ROM or system is loaded.
  A power-off within those 2 s loses that save.
- **How the RAM gets out:** the second port of cartridge RAM's block RAM,
  read by an engine while the game runs (no pause), bytes on `PS_IN`'s
  byte channel, which a control-register bit switches between the serial
  port and the read-back (EMIO has no free lines); a dirty flag for the
  PS. The `.sav` goes in over the write bus while the core is held.
- The mechanism is the PS app's, keyed by the system table, so the NES
  (battery PRG RAM) adopts it later with a table entry and its own
  read-back.

**As built (phase 1, 2026-10-04).**

- **PL** (`Gb.Loader`, `Gb.Core.gbCoreWith`, `Gb.Top`; map reserved in
  ARCHITECTURE.md): cartridge RAM is a true dual-port block RAM, the
  core on port A exactly as before, the PS on port B. Link writes at
  0x100000 + n (only while held) load byte n; a data byte written at
  0x120000 + n (any time) reads byte n, which comes out on the byte
  channel the next clock while the save control (0x140000) bit 0 is set;
  serial bytes are dropped meanwhile, and since both feed the same toggle,
  switching the source never fakes a byte. The dirty flag (`PS_IN` bit 6)
  is set by every cartridge-RAM write the core makes and cleared by any
  write to 0x140001 (a game write in the same clock wins).
- **Which cartridges:** `menu.gbSaveSize` (the PS app's system table,
  also asked by `make sd` through `link --save-slots`): types 0x03, 0x09,
  0x1B, 0x1E (the battery types the core runs), RAM-size byte 2 (8 KiB) or
  3 (32 KiB). 64 and 128 KiB, MBC2 and MBC3 wait for phase 2.
- **A blank save is zeros** (`mksd.sh` makes it with `truncate`). The
  block RAM starts at 0 on the board (Vivado's default), and the PS
  writes zeros into cartridge RAM for a battery cartridge without a usable
  save file, so another game's RAM never shows through.
- **Loading** (`relay.zig`, `save.zig`): the cartridge streams in while
  held (`load.streamHeld`), then the save file (found by its path; a file
  of the wrong size is not used), then the hold is released. If the game
  has a battery but no usable save, the menu stays open with a message
  ("No save file on the card", "Save file has the wrong size"); L plays.
- **Writing back:** the PS clears the dirty flag whenever it sees it and
  writes the RAM back after 2 s without a new one; also when the menu
  opens (the game paused first), before `make load` holds the core,
  before `make sd` opens the card and before an R command. Each block of
  the file is read from the card and rewritten only if it differs (an
  unchanged save writes nothing). Reading 8 KiB over the link takes about
  40 ms, 32 KiB about 150 ms (estimated from the link's 4 µs per byte,
  not yet measured on the board: the relay prints each write-back's
  time); the PS's main loop (USB keys, the relay)
  waits meanwhile, the game doesn't. After every mount the file is found
  again by its path and must be the same file (first cluster): on a
  swapped card the game is no longer saved, with a message.
- **Pull** (`platform/host/saves.zig`, `link --saves-pull`, `make
  saves-pull`): the relay's R command reads a block (`r<lba><sum>` and
  512 raw bytes); the desktop mounts the card with the PS's own
  `fat.zig` over it and copies every file under `/saves` into
  `SAVES_DIR` (default `$XDG_DATA_HOME/emulation_clash/saves`), each
  written beside its place and renamed over it. `make sd` pulls first and
  stops without erasing if that fails (a card without a FAT fails too:
  `SAVES=skip` for a new card); `mksd.sh` then fills each new save file
  from `SAVES_DIR` if one of the right size is stored there.

**As built (phase 2, 2026-10-05).**

- **ROM in DDR** (`Gb.Top`): `Platform.Mem.Ddr`'s read port (k = 5: a
  1 KiB line cache of 32 lines, prefetch on; k = 7 took 12k LUTs of tags
  and would not route) through HP0 at `romBase` = 0x02000000;
  the cache is emptied while the PS holds the core. `gb.bit` holds no
  cartridge any more (the 64 RAMB36 of ROM are gone); until the PS loads
  one, the core runs whatever DDR holds, paused behind the menu.
  `gb_zynq.v` wires HP0 as `m0e_zynq.v` and uses `ps7_hp.tcl`.
- **Waiting for ROM** (`Gb.Core.gbCoreWith`): the core asks a ROM port
  (requests out, answers in) and runs under a clock enable that is low
  while its byte is outstanding: its state and memories stand still, so
  it computes exactly what it would with the answer at once. A request
  made while the port is busy (the clock of an answer) waits a clock in a
  queue (the port takes one request at a time); answers to requests from
  before a hold are dropped; dots that pass while it waits are given back
  one per clock afterwards, so short waits don't slow the game. A cache
  hit answers the next clock, as the block RAM did: no wait. The four
  header reads at start wait one clock each.
- **Mappers:** MBC2 (its 512 half-bytes; the upper half reads 1s), MBC3
  (7-bit ROM bank, 4 RAM banks, the clock), MBC5 up to 8 MiB (9-bit
  bank), cartridge RAM up to 128 KiB (true dual-port block RAM).
- **MBC3's clock** as in the spec (`Gb.Spec.Machine.rtcTick`, from Pan
  Docs): a second every 2^20 M-cycles of machine time (real time while
  the board runs) unless halted; registers carry at 60/60/24/512 days
  (day carry stays until written); a value written above a register's
  limit counts to the top of its bits and wraps to 0 without a carry;
  writing the seconds restarts the second; 0 then 1 to 0x6000 latches.
  Reads return the latched values, unused bits 0. The core takes its start
  values from the PS (link 0x140010, while held) and gives them back
  (0x140020, a snapshot at the first byte).
- **Save files:** MBC2 512 bytes (one half-byte per byte), RAM 8/32/64/
  128 KiB, MBC3 with the clock + a 48-byte footer in the VBA-M/BGB
  layout (S M H DL DH and the latched five as 32-bit little-endian
  words, then a 64-bit time stamp). The board writes the time stamp as 0
  (it has no clock of its own; time doesn't advance while it is off), and
  `make saves-pull` stamps the copy on the desktop with the time of the
  pull (decided by the user, 2026-10-05; `saves.stampClock`: a file 48
  bytes past a multiple of 512), so another emulator counts from there;
  the board ignores the stamp when it loads the save. A cartridge
  with a clock is written back at every menu open (its time moved on;
  only the footer's block changes).
- **Loading:** the PS streams the file from the card straight into DDR
  in multi-block reads (`load.streamDdr`) while holding the core, then
  the save, then releases it; `make load SYSTEM=gb` writes DDR through the
  relay's W command (64 KiB each, checked). The menu's limit is 8 MiB.
- **Tests:** `gb-spec`: Mooneye `emulator-only/mbc2` (all 7) and our
  `mbc3.gb` (no MBC3 test in the pinned Mooneye: ROM and RAM banks, the
  clock's latch, roll-over to the day carry, halt, an out-of-range
  value; it fails with a broken carry). `gb-lockstep`: the same ROMs plus
  MBC1 2 MiB / 32 KiB RAM and MBC5 8 MiB on the core, and `gbCoreWith`
  behind a ROM port with random delays (up to 40 clocks) equal to the
  harness. Layer 4: every Game Boy run reads its cartridge from the
  harness's simulated DDR (`--ddr-file`, random latency).

**Phase 3, step 1: test ROMs and the post-boot state (2026-10-05).**

- **Test ROMs** (`tools/fetch-roms.sh`, into `roms/gbc/`, hashes
  pinned): cgb-acid2 v1.1 (MIT) and its `reference.png` (commit
  fa5b7f8); Blargg's `cgb_sound` (all and the 12 singles; results in
  cartridge RAM at $A000) and `interrupt_time` (double speed), same
  mirror and commit as the DMG ones; Blargg's `cpu_instrs`,
  `instr_timing`, `mem_timing` also run in CGB mode (`roms/gb/blargg`);
  Mooneye's CGB tests are in the pinned archive's `misc/`:
  `boot_regs-cgb`, `boot_div-cgbABCDE`, `boot_hwio-C`,
  `bits/unused_hwio-C`, `ppu/vblank_stat_intr-C` (the CGB0-only
  `boot_div-cgb0` is out of scope).
- **The playable homebrew** for the exit: µCity v1.3 by Antonio Niño
  Díaz (code GPLv3+, graphics and music CC BY-SA 4.0,
  codeberg.org/SkyLyrac/ucity), `roms/gbc/games/ucity.gbc`: Color only
  (0x143 = 0xC0), MBC5 with 128 KiB of battery RAM (the largest save),
  128 KiB of ROM.
- **Post-boot state, CGB mode** (Pan Docs, "Power Up Sequence", CGB
  column; there is no boot ROM): A $11, F Z=1 N=0 H=0 C=0, B $00, C $00,
  D $FF, E $56, H $00, L $0D, SP $FFFE, PC $0100. Registers as the DMG
  where the column agrees, and: SC $7F, DMA $00, NR52 $F1, KEY0 = the
  header's 0x143 byte, KEY1 $7E, VBK $FE, HDMA1-5 $FF, RP $3E, SVBK $F8.
  DIV, STAT and LY depend on the boot ROM's run time ("just don't rely on
  these"): the spec takes the values Mooneye's `boot_div-cgbABCDE` and
  `boot_hwio-C` expect (*step 3*).
- **Post-boot state, DMG compatibility mode** (0x143 bit 7 clear): A
  $11, F Z=1 N=0 H=0 C=0, C $00, D $00, E $08, SP $FFFE, PC $0100; B =
  the sum of the 16 title bytes if the licensee is Nintendo (old code
  $01, or $33 with new code "01"), else $00; HL $991A if B is $43 or $58,
  else $007C. KEY0 $04 (the CPU in DMG mode), OPRI $01 (DMG object
  priority). B and HL follow the rule even though we always use the
  default palette (some games may look at them).
- **The fixed palette for DMG games** (decided 2026-10-04: one, since
  the per-title choice is the boot ROM's): the one the CGB boot ROM
  itself uses for a game it doesn't recognise, computed from ISSOtm's
  disassembly (codeberg.org/ISSOtm/gb-bootroms, `src/cgb.asm` at
  443d7f0): game index 0 -> `PaletteIndexesAndFlags[0]` = $7C (triplet
  $1C, shuffle flags 3) -> all of BG, OBJ0 and OBJ1 = the triplet's third
  entry, `Palettes` + $E8: (31,31,31), (15,31,6), (0,12,24), (0,0,0) in
  5-bit RGB, i.e. #FFFFFF #7BFF31 #0063C5 #000000. BGP, OBP0, OBP1 index
  into these (Pan Docs, "Compatibility palettes").

**Phase 3, step 3e: VRAM DMA in the spec (2026-10-06).** HDMA1 to HDMA5
(Pan Docs, "LCD VRAM DMA transfers"), CGB mode only. A block is 16 bytes
from the source (bits 4 to 15) to VRAM (bits 4 to 12, the bank VBK
selects). The CPU waits 8 M-cycles per block, or 16 in double speed, which
takes the same time; a byte pair moves each M-cycle (one byte in double speed).
General purpose DMA (HDMA5 bit 7 clear) runs all its blocks inside the
write. HBlank DMA (bit 7 set) runs one block per HBlank. The PPU
requests the block when it enters mode 0, and the CPU runs it before its
next instruction, but not while halted: the block then waits. A write with
bit 7 clear stops a running HBlank DMA. HDMA5 reads bit 7 clear while
one runs, and otherwise bit 7 set with the blocks left - 1 (so $FF once
done). Our choices, which no test ROM pins down: sources from 0xE000
read 0xA000 and up, VRAM as a source reads $FF, and a transfer started
mid-HBlank waits for the next one. Gate: our `test/asm/hdma.gb` in
gb-spec checks
GDMA into both banks, HBlank DMA's progress, stopping it, and the time per
block in both speeds (TIMA). It fails with each of five mutations: the
bank ignored, half the time, double speed at the normal M-cycles, no
HBlank request, the stop ignored. The HBlank block runs at the end of the
spec's step, after the instruction (the same moment as before the next
one): the lockstep harness then clocks the core through its M-cycles
without the CPU after each step.

**As built (phase 3, steps 4 to 6; 2026-10-06).**

- **The core's model** is a constant: `coreStep` overwrites the config's
  `ccCgbHw`/`ccCgb` with it, so `gb.bit` has no CGB logic. A register
  alone wasn't enough for Vivado (47k LUTs, WNS -4.6 ns); with the
  constant, 34.7k LUTs and WNS +1.575 ns. That clears step 2's gate
  (no worse than +0.726).
- **Colour path (4b).** VRAM is four block RAMs of 4 KiB (each bank's
  even and odd bytes), all read at the PPU's pair address, so a tile and
  its attributes (or a tile row from either bank) come in one read. The
  CGB's object choice (lowest OAM index) is a balanced tree; a chain of
  10 compares was the worst path. The pixel's colour is two palette RAM
  bytes (`koPixelC`, 15-bit BGR).
- **Double speed, STOP, VRAM DMA (4c).** An M-cycle takes 2 dots in
  double speed. The speed switch's 2050 M-cycles and VRAM DMA's blocks
  are M-cycles without the CPU (`ksPause`, `ksHdmaRun`); DIV is held in
  the pause. A DMA M-cycle copies its bytes (2, or 1 in double speed) in
  extra sequencer states before the dots (`QHdmaRd`, `QHdmaWr`), through
  the same read path as OAM DMA (ROM through the DDR port).
- **Sound (4d)** as the spec, with the model given to `apuRead`/`apuWrite`
  and double speed to `apuTick`. One gap closed: with a CH3 period below
  3, the trigger delay wraps the 20-bit counter below 0. `waveClock` now
  counts up through it, as the spec's negative number does. cgb_sound 12
  found this once the quick tier ran far enough.
- **Display (5):** three frame buffers of 23040 × 15 bits, shown through
  `rgb15` (each channel `(c << 3) | (c >> 2)`, no colour correction: off
  by default, and nothing turns it on yet). VGA shows each channel's top 4
  bits; HDMI gets all 8.
- **`gbc.bit` (6):** `Gb.TopCgb` is `Gb.Top`'s design (`gbTop` with a
  `GbVariant`: model, system ID, pixel source, colour), system ID 6,
  wrapper `boards/zedboard/gbc_zynq.v`, `make … SYSTEM=gbc`
  (`systems/gbc/zedboard.mk`, which takes `systems/gb/src` and the Game
  Boy's Clash flags). The PS app's table has a `/gbc` row (`.gbc` and
  `.gb`, Color-only cartridges allowed: `gbcCheck`). It has no battery
  saves yet (left for later, user 2026-10-06: their card slots need
  `link --system gbc`). Keys: system 6 uses the Game Boy's key map
  (`relay.zig`'s `keymapSystem`, one line, agreed with the user).
  `make sd` puts `SD_GBC_ROMS` into `/gbc`, the Game Boy's folders
  (`SD_GB_ROMS`, as `SD_GBC_GB_ROMS`) into `/gbc/gb` (DMG cartridges on the
  GBC: the user, 2026-10-07; copies, the menu lists one folder per system),
  and `gbc.bit` into `/systems`.
- **Palettes for DMG cartridges (2026-10-07, the user's request).** A
  DMG cartridge on the GBC gets its colours from the PS app
  (`platform/ps/gbc_palette.zig`), which writes them while the core is held
  into `Gb.Loader`'s palette window (0x140030, 24 bytes: BG 0, OBJ 0,
  OBJ 1, four 15-bit colours each). `Gb.Machine` keeps them in a register
  (the default until written), and the core takes them at the hand-off
  (`kiPalInit`). The menu's first line in a `/gbc` list, "<Palette: ...>",
  cycles through three kinds of choice:
  - **Auto:** the boot ROM's palette for Nintendo's titles, found by the
    title checksum and, for 29 titles, its 4th letter (Some Game gets
    combination 11). Unknown or non-Nintendo titles get our default.
  - **Default:** the boot demo "rota"'s Game Boy shades (#F4ECD8 #F7A41D
    #22A699 #0E0B14 as 0x6BBE 0x129E 0x4E84 0x0822). This replaces the
    boot ROM's palette for unknown games used before.
  - **The 12 manual palettes,** by their button combination (Right, Left,
    Up, Down, and each with A or with B).

  The tables are SameBoy's boot ROM's (BootROMs/cgb_boot.asm, Expat
  licence; kept in `local/reference/sameboy/`, not committed), copied as
  data. Games in CGB mode ignore the window. Tests:
  - gb-unit: the window, and the core's default against the spec's.
  - gb-lockstep: dmg-acid2 with BG, OBJ 0 and OBJ 1 all different, every
    colour pixel; a mutation swapping OBJ 0 and 1 fails.
  - `make test-zig`: the lookup (Some Game, the 4th-letter rule,
    non-Nintendo games, the manual palettes) and the menu line.
  - Layer 4 (`ci-gbc`, `ci-arty-gbc`): `test/ps/palette.ps` writes the
    palettes, and dmg-acid2 equals the spec's `dmg-acid2-gbc` goldens.
- **Battery saves on the GBC (2026-10-08).** The `/gbc` row of the PS
  app's table has the Game Boy's save hook (the same cartridge RAM map:
  the tops share `Gb.Machine`), so its saves live under `/saves/gbc`,
  following the menu's folders, except the Game Boy's folders copied into
  `/gbc/gb`: those share the Game Boy's saves, `/saves/gb/...` (the user,
  2026-10-08; `save.savePath` maps the path, and `mksd.sh` makes no
  second slot).
  `link --system gbc` (the Game Boy's keys and loading, `/gbc`'s card
  folder) lists the slots for `tools/mksd.sh`, which makes them as for the
  Game Boy and restores stored ones (`make saves-pull` keeps them).
  Tests: `make test-zig` (the row's hook, the shared path), a card image
  (the GBC's slots, a stored 128 KiB save restored byte for byte, no
  `/saves/gbc/gb`), `ci-gbc` (save.ps on `gbc_top`).
- **One machine, four tops (2026-10-07, the Arty as the main board).**
  `Gb.Machine` holds everything from the buttons and the PS link to the
  picture (core, DDR port, saves, frame buffers, overlay) and the two
  variants (`dmg`, `cgb`). `Gb.Top`/`Gb.TopCgb` (ZedBoard) and
  `Gb.ArtyTop`/`Gb.ArtyTopCgb` (Arty, `gbArtyTop`) add only their
  board's ends. `Gb.ArtyTop` no longer keeps its own copy of the machine
  (side track G phase 2 kept it in step by hand). The Arty's
  Game Boy Color: `make … SYSTEM=gbc BOARD=arty-z7-20`
  (`systems/gbc/arty-z7-20.mk`, `boards/arty-z7-20/gbc_zynq.v`); sound
  mono on `aud_pwm` as the Game Boy's; `ci-arty-gbc` compares cgb-acid2
  with a full-colour golden (`test/golden/rgb/`, as TMDS carries 8 bits a
  channel).

**Cartridges in DDR (phase 2, as planned).** ROM moves to DDR (up to 8 MiB, MBC5),
freeing 64 RAMB36; on a cache miss the top holds the dot enable, so the
machine's time stops and core = spec stays exact. MBC2 and MBC3 join;
cartridge RAM up to 128 KiB in BRAM. **MBC3's clock** runs only while
the board is powered (the board has no battery-backed clock); its state
goes into the `.sav` footer in the format other emulators use (*to
verify*); time doesn't advance while the board is off.

**Game Boy Color (phase 3).** A separate bitstream, `gbc.bit` (system ID
6, menu folder `/gbc`), from the same Clash source with the model as a
compile-time parameter: `gb.bit` stays the DMG as tested. DMG cartridges
run on it in compatibility mode with one fixed default palette (the
per-title palettes come from Nintendo's boot ROM, which is out of scope).
**The sequencer must change first:** double speed puts an M-cycle every
2 dots, about 12 pixel clocks, and the core uses 18 to 20 today, with the
worst path at 37.5 of 39.72 ns ("Clocking"). The freed ROM BRAM leaves
room for the CGB's 32 KiB WRAM and 16 KiB VRAM.

## Test plan

| Test | Layer | Source |
|---|---|---|
| Opcode unit tests, flags, timer edge cases | 0, 1 | hand-written + rgbds regression ROMs |
| Decoder round trip and totality | 1 | hedgehog |
| CPU lockstep (bus trace per M-cycle) | 2 | hedgehog random programs, CPU test ROMs |
| PPU lockstep (per line and per frame) | 2 | dmg-acid2, rgbds PPU regressions |
| Full-core ROM runs | 3 (short), 4 (long) | Blargg, Mooneye subset, dmg-acid2 |
| VGA/HDMI frame goldens | 4 | spec goldens ×3, letterboxed |
| Board | 6 | the monitor (and serial text if forwarded) |
| Playable game | 2, 6 | any freely licensed homebrew game (user, 2026-10-02: "any is fine"; your pick). It must fit the supported MBCs and 256 KiB, and its licence (code *and* graphics) must allow redistribution; record it in `tools/fetch-roms.sh` with a pinned hash |

## Status log

*(fill in per completed step: date, commit, test results, WNS, utilisation)*

- **I phase 3 done: the Game Boy Color** (2026-10-08, board-checked by the
  user on the Arty, all areas: cgb-acid2, Blargg `cpu_instrs` in CGB mode,
  µCity with sound, DMG games in compatibility mode with the palettes
  (Some Game on Auto, the Default and manual choices), HDMI sound,
  `make load`). On main up to 6b95647 (the PPU timing fix). Arty
  gbc.bit WNS +0.759 ns, WHS +0.054 ns, 36.6k LUTs; Arty gb.bit +2.159 ns.
  `make ci` green. Not done: battery saves on the GBC (later); the
  ZedBoard's board check (not accessible).

- **I phase 3, steps 2 to 6** (2026-10-06, branch `side-gb-upgrades`,
  up to 3bc5c98; awaiting the board check): see "As built (phase 3)".
  gb-lockstep 82 (CGB: Mooneye 5, cgb-acid2, speed, hdma, sound_ds,
  sound_all, cgb_sound 03/08/09/11/12), gb-spec 209, gb-golden 17,
  gb-unit 18; `make ci-gb`, `ci-gbc` green. `make bit SYSTEM=gbc`: WNS
  +0.165 ns, WHS +0.122 ns, 49019 LUTs (92.1 %), 91.5 BRAM tiles. `make bit
  SYSTEM=gb`: WNS +0.299 ns, WHS +0.119 ns, 36541 LUTs (68.7 %) (+1.575
  ns before steps 4c to 6).

- **I phase 2, ROM in DDR, MBC2/MBC3/MBC5** (2026-10-05, branch
  `side-gb-upgrades`, commits 0330062 to 2d711ff; board-checked by the
  user 2026-10-05: Libbet, dmg-acid2, Blargg, save.gb's count kept
  across `make sd`, Mooneye MBC5 8 MiB, MBC1 2 MiB and MBC2 RAM pass):
  see "As built (phase 2)". `make ci` green (gb-lockstep 64 with random
  ROM delays; every layer 4 Game Boy run from simulated DDR, goldens
  identical; tone.gb 440.6 Hz). `make bit SYSTEM=gb`: WNS +0.726 ns,
  WHS +0.087 ns, 30624 LUTs (57.6 %), 44.5 BRAM tiles (31.8 %), 14 DSPs.
- **I phase 1, battery saves** (2026-10-04, branch `side-gb-upgrades`,
  commits b8681e0, f65723f; board-checked by the user 2026-10-04: the
  boot count survives power cycles, the menu and `make sd`): see "As built (phase
  1)" above. `systems/gb/test/asm/save.asm` counts its boots in MBC1
  RAM: `gb-spec` (from a blank and from a saved count; screen golden
  `save-42`), `gb-lockstep` (save.gb, 20000 steps; `gbCore` with the
  dual-port RAM in Clash simulation still equals the harness), `gb-unit`
  (the map, the save control); layer 4 (`ci-gb`): `test/ps/save.ps`
  loads a save with count 41 while held, and the read-back on the byte
  channel equals `test/ps/save.hex` ("SAVE", 42, and 16 bytes from the
  end of the RAM), `PS_IN` = 0x0F020040 (dirty), screen = the `save`
  VGA/HDMI goldens ("00042"). PS app and desktop: `make test-zig` 306
  tests (`save.zig` on the card image `make sd`'s tools make: load,
  write-back of 2 changed blocks, nothing else changed, `fsck.fat`
  clean; `saves.zig`: slots, pull), `tools/test_link.py` pulls through
  the fake relay (garbled block read again; a card without a FAT fails).
  `make bit SYSTEM=gb`: WNS +1.271 ns, WHS +0.015 ns, 21347 LUTs (40.1 %),
  83.5 BRAM tiles (59.6 %), no critical warnings.

- **2.9c** (2026-10-03, board-checked by the user): HDMI audio, see "Sound".
  HD_SPDIF in `gb_top`, `gb_zynq.v` and `hdmi_audio.xdc` (U15, from the
  master XDC). Layer 4: `make sim SYSTEM=gb ROM=build/gb/asm/tone.gb
  FRAMES=20 SIM_ARGS="--expect-tone 439.8"` decodes 16746 frames without
  errors, 440.6 Hz; the 57 register writes equal the Game Boy's I2C
  golden. `make bit SYSTEM=gb`: WNS +0.909 ns, WHS +0.122 ns, 22024 LUTs
  (41.4 %), 82.5 BRAM tiles (58.9 %), 11 DSPs; `hdmi.xdc` now finds
  HD_CLK's ODDR from the port (a second ODDR had silently removed its
  output delays).
- **2.9a/b** (2026-10-03, commit 4c48f2c): the APU in the spec and in
  Clash, see "Sound"; `gb-spec` runs `dmg_sound` 01-08 and 11 and
  measures `tone.gb` at 439.8 Hz; `gb-lockstep` compares the four channel
  outputs every M-cycle (random ROMs with sound registers, `dmg_sound`
  01/03/05, `tone.gb`, `sound_all.gb`).
- **Tearing fix** (2026-10-03, commit e0ea72a, board-checked):
  three frame buffers, see "Frame-rate mismatch".

- **M2 exit met** (2026-10-03, board-checked by the user): over HDMI,
  dmg-acid2 matches its reference, Blargg `cpu_instrs`, `instr_timing`
  and `mem_timing` pass, Libbet is playable with the desktop keyboard.
  Found on the board: the relay's baud fallback cut long cartridge loads
  (fixed, `protocol.Activity`); the frame buffers tear (open, see
  "Frame-rate mismatch").
- **2.8** (2026-10-03): `boards/zedboard/gb_zynq.v`
  (PS7 + `gb_top`), relay/link protocol `M<5 hex addr><4 hex len>` for
  20-bit loads (the relay always sends three address bytes; CHIP-8's
  12-bit loader keeps the low 12 bits, unit-tested), `make load SYSTEM=gb
  ROM=…` (16 KiB blocks, up to 256 KiB; fake-relay test), playable game
  fetched: *Libbet and the Magic Floor* v0.08 (Damian Yerrick, zlib,
  `roms/gb/games/libbet.gb`, checked on the spec). `make bit SYSTEM=gb`:
  WNS +0.405 ns, WHS +0.122 ns, 22782 LUTs (42.8 %), 3269 FFs, 80.5 BRAM
  tiles (57.5 %), 11 DSPs. Thin margin: the critical path (41 levels)
  runs from the core state to a block RAM address in one clock; if a
  change breaks timing, register the memory requests (one more clock per
  access in the sequencer).
- **2.7** (2026-10-03): `Gb.Display` (dot enable from a 32-bit phase
  accumulator, 715531909 per pixel clock: 4194304.001 Hz; two frame
  buffers in one block RAM, swapped at the core's frame end and taken at
  the display's VBlank; ×3 scaler at (80, 24) with `div3` = ×683 >> 11,
  exhaustively checked; grey/green palette on SW0) and `Gb.Top`
  (`gb_top`: VGA, HDMI through M0d's ADV7511 path, buttons BTNR/L/U/D =
  D-pad and BTNC = A ORed with `PS_GPIO` keys, LEDs, `Platform.Loader`
  with 18-bit addresses; replaced by the PS link in side track A). `tools/gb-rom-image.sh` makes the ROM image for
  `blockRamFile`. `gb-golden` (layer 3): VGA goldens from the spec goldens,
  HDMI goldens through the ADV7511 model, and the display path in Clash
  simulation reproducing dmg-acid2's VGA golden. Layer 4 (`make ci`):
  dmg-acid2, Blargg `instr_timing` and our `buttons.gb` with BTNR + BTNC
  held, each identical to its VGA and HDMI golden with the I2C log equal
  to M0d's. Desktop keys: `make keys SYSTEM=gb` (`link --gb`, W A S D,
  J = B, K = A, L = Start, Left Shift = Select; gamepad map proposed).
- **2.6** (2026-10-03): `Gb.Core`, the whole machine in Clash: one pure
  `coreStep` per clock (M-cycle sequencer: tick, OAM DMA byte, 4 PPU
  dots, STAT, CPU access, PPU command, `cpuCycle`; 18 to 20 clocks, an
  M-cycle every 4 dots of credit), timer, serial, joypad (interrupt
  included), OAM DMA, memory map with the PPU's blocking rules, MBC1 and
  MBC5; the cartridge header (type, sizes, checksum) is read at reset, so
  the loader only writes ROM bytes. `gbCore` wires it to block RAMs: ROM
  256 KiB (caller's, for `blockRamFile` + loader), cartridge RAM 32 KiB,
  WRAM, HRAM, VRAM as even/odd banks. Spec additions: joypad input and
  interrupt (`setJoypad`), MBC5 (all 8 Mooneye MBC5 tests and
  `oam_dma/sources-GS` pass: 86 of 96 Mooneye). New regression ROM
  `joypad.asm`. `gb-lockstep`: the core with vector memories against the
  spec after every step (bus trace, registers, timer, IF/IE, PPU
  position, DMA, every pixel) on 200 I/O-heavy random programs with
  random keys and 17 ROMs (acid2, Mooneye timer/DMA/MBC/PPU/serial,
  Blargg); `gbCore` in Clash simulation equals that harness clock for
  clock; deliberate bugs (joypad groups, timer select) caught. Compiles in
  under a minute and lints clean (`blockRam1`: a 256 Ki-entry initial
  vector had made Clash take over 10 minutes).
- **2.5** (2026-10-02): `Gb.Ppu`, the PPU in Clash: the dot logic is a
  pure function equal to the spec's; a dot takes 3 clocks with two VRAM
  reads (map byte, then tile row from VRAM as 4096 16-bit even/odd
  pairs); line objects' rows are read in mode 3's first stall dots. Spec
  adjusted so hardware can match it exactly: OAM scan is progressive (one
  entry per two dots of mode 2, entries copied) and object tile rows are
  read once at mode 3 (the size latched there). Lockstep (`gb-lockstep`,
  "Clash PPU"): through test hooks in the spec machine (`Hook`, unused in
  normal runs) the Clash PPU runs next to the spec PPU on the same VRAM,
  OAM and registers; every M-cycle the state (line, dot, mode, LYC flag,
  STAT line, mode 3 progress) and interrupt requests match, and every
  pixel, on dmg-acid2 (30 frames, so its frame equals the reference
  image), 7 Mooneye PPU/DMA tests, 60 frames of `cpu_instrs` and 100
  random programs. Deliberate bugs (window stall, object priority) are
  caught. Compiles to Verilog and lints clean.
- **2.4** (2026-10-02): `Gb.Cpu`, the SM83 core in Clash: a pure
  per-M-cycle function (`cpuCycle`: this M-cycle's read data and IE/IF
  in, the next M-cycle's bus request, IF acknowledge and step end out)
  plus `cpuMealy`. It reproduces the spec's step boundaries, the
  interrupt check in the fetch M-cycle, halted fetches, EI delay and both
  HALT bug cases. `gb-lockstep` (layer 2): 300 random 32 KiB ROMs × 3000
  steps and 10 test ROMs (Blargg, Mooneye timer/interrupt/DMA timing,
  dmg-acid2) with identical bus traces and registers per step; the core
  alone passes all 11 `cpu_instrs`, `instr_timing` and `mem_timing`.
  Mutations (an ADC flag, PUSH byte order) are caught within a few
  steps. Spec fix: the dispatch now clears its IF bit in place when it
  picks the vector (it used to overwrite IF from an older copy). The core
  compiles to Verilog (6.5 k lines with a test RAM) and lints clean.
- **2.3** (2026-10-02): `Gb.Types` (Clash ADT: `Instr`, `CbInstr` and
  operand enums with `BitPack`, so fields decode by `unpack` of a bit
  slice) and `Gb.Decode` (`decode`/`encode`, `decodeCb`/`encodeCb`, plus
  `instrLength`, `instrCycles` (condition false/true) and `cbCycles` for
  the core's sequencer). `gb-unit`: round trips over all 256 + 256 bytes
  and every constructor value (exactly 256 + 256); lengths, M-cycles and
  the 11 lock-up opcodes agree with `Gb.Spec`'s bus traces for every
  opcode with conditions false and true (a changed cycle count fails it,
  checked). The decoder goes through Clash to Verilog and lints clean.
- **2.2** (2026-10-02): PPU and OAM DMA in the spec. dmg-acid2 equals its
  reference image pixel for pixel; Blargg `mem_timing` (3 + combined)
  passes over serial, `mem_timing-2` and `halt_bug` show "Passed" on
  screen. Frozen goldens in `systems/gb/test/golden/spec/` (dmg-acid2,
  Blargg cpu_instrs, instr_timing, mem_timing, mem_timing-2, halt_bug),
  checked by eye. Mooneye: 77 pass (was 52), including every
  `acceptance/ppu` test, all OAM DMA tests except `sources-GS` (MBC5) and
  every access-timing test; the 11 left are other models, MBC5 and the
  MBC1 multicart. `gb-spec`: 153 tests, about 5 s (combined `cpu_instrs`
  now 3.5 s with the PPU running).
- **2.1** (2026-10-02): `Gb.Spec` CPU, timer, serial, interrupts, HALT
  bug, MBC1, post-boot state. Blargg `cpu_instrs` (11 + combined) and
  `instr_timing` print "Passed"; 52 Mooneye ROMs pass (all of
  `acceptance/timer`, the interrupt, EI/DI, HALT, DIV and boot-state
  tests for DMG ABC, MBC1); the rest are listed in
  `test/Gb/SpecRoms.hs` with the step that brings them (OAM DMA and the
  PPU in 2.2; other models never). Own regression ROM `halt_bug.asm`.
  `gb-spec`: 117 tests, 1.5 s. Tool: `cabal run gb-run -- blargg|mooneye
  <rom.gb>`.
- **2.0** (2026-10-02): `tools/fetch-roms.sh` fetches into `roms/gb/`:
  Blargg (mirror retrio/gb-test-roms@c240dd7, 20 ROMs, renamed without
  spaces), Mooneye mts-20260714-0944-31510e1 (archive hash-checked, then
  unpacked whole), dmg-acid2 v1.0 + `reference-dmg.png` (@8a98ce7).
  `rgbds` 1.0.1 in the flake; `make gb-asm` assembles
  `systems/gb/test/asm/*.asm` (first: `serial_pass.asm`, prints
  "Passed" over serial, then the Mooneye pass pattern at `LD B,B`).
  Hooks: `Gb.Hooks` (serial capture, Blargg and Mooneye verdicts),
  `Gb.Screen` (160×144 shades ↔ grey PNG, same coding as dmg-acid2's
  reference), `Gb.Cartridge` (header, checksums, MBC type). Every fetched
  ROM (115 Mooneye + 20 Blargg + dmg-acid2) has a valid logo, header
  checksum, known type and matching size. Gate: `nix develop -c make
  test SYSTEM=gb`, 20 tests in `gb-spec`.
