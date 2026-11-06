# NES (NTSC)

Target: the **NTSC NES / Famicom** (2A03 CPU, 2C02 PPU). PAL, the
Famicom Disk System, expansion audio and the light gun are out of scope
for milestone 3. Written during the 2026-10-03 planning session, before
any code: everything here is a plan, and items marked *to verify* must be
checked against the nesdev wiki or the cited source before code depends
on them. Where this page and the code disagree once steps are built, the
"as built" notes and the status log win, as for the Game Boy.

**State (2026-10-04): milestone 3 is done**, its exit board-checked by
the user (see the status log, "M3 exit"). The plan sections below are
kept as written; the status log has what was built.

## Why the NES (planning decision, 2026-10-03)

The user asked for "a 6502 system of any kind" (Atari 2600, Atari 800XL,
C64 or NES). The NES was chosen because it reuses the most of what M0d,
M1b and M2 built, and adds the least new infrastructure:

- **No system ROM.** Unlike the 800XL (OS ROM) and the C64 (KERNAL/BASIC),
  nothing copyrighted has to be fetched or replaced; cartridges boot
  directly. Fits rule 7 cleanly.
- **Best test ROMs of the four:** `nestest` with a published per-
  instruction log, Blargg's CPU/PPU/APU suites (results readable from
  memory, no screen needed), and cycle-by-cycle 6502 tests (below). The
  2600 has far less, and its TIA (no framebuffer, racing the beam) is the
  hardest video model of the four for a first 6502 system.
- **Same shape as the Game Boy:** cartridge in BRAM, framebuffer, ×2
  integer scale (256×240 → 512×480 fills the 480 lines exactly), the
  triple-buffer display, the S/PDIF audio path, the PS link and SD menu,
  the 8-button desktop key map. The 800XL and C64 need a full keyboard
  protocol, disk images and (C64) a second CPU in the drive first.
- **The 6502 core is written once, as a shared package**, with decimal
  mode as a parameter (the 2A03 has none), so the 2600, 800XL or C64 can
  reuse it later.

## Machine summary

| Item | Value |
|---|---|
| CPU | Ricoh 2A03: MOS 6502 without decimal mode, plus APU, OAM DMA, controller ports |
| Clocks | master 21.477272 MHz; CPU = master / 12 = 1.789773 MHz; PPU dot = master / 4 = 5.369318 MHz, so 3 dots per CPU cycle |
| Frame | 262 lines × 341 dots; with rendering on, odd frames skip one dot; 60.0988 Hz |
| Display | 256×240, 6-bit colour index (64-entry palette, no RGB in the chip) + 3 emphasis bits (PPUMASK) |
| Memory | 2 KiB CPU RAM, 2 KiB nametable RAM (VRAM), 256 B OAM, 32 B palette RAM; cartridge PRG ROM, CHR ROM or RAM, optional PRG RAM |
| Video | background + 64 sprites (8 per line), sprite 0 hit, overflow flag (with its hardware bug), VBlank NMI |
| Input | standard controller: 8 buttons, read serially through $4016/$4017 |
| Sound | APU: 2 pulse, triangle, noise, DMC (DMC fetches steal CPU cycles) |
| Cartridges | iNES / NES 2.0 files: 16-byte header, PRG, CHR |

References: the nesdev wiki (CPU, PPU rendering and frame timing, APU,
mappers, "Emulator tests"), the 6502 data sheet and the visual6502
findings summarised on the wiki for per-cycle bus behaviour.

## Accuracy target

- **CPU: cycle accurate**, meaning every bus access of every cycle,
  *including the dummy reads and writes* of the 6502 (indexed reads that
  cross a page, read-modify-write instructions writing the old value
  first). They matter on the NES: reading $2002, $2007 or $4016 has side
  effects. Official opcodes first; the stable undocumented ones next
  (`nestest`'s second half, and the 2600/C64 need them later).
- **PPU: dot accurate** for the register-visible behaviour: VBlank flag
  and NMI timing (Blargg `ppu_vbl_nmi`), sprite 0 hit, sprite overflow,
  the odd-frame skip, the `v`/`t`/`x`/`w` scroll registers with their
  per-dot updates. The fetch pattern follows the wiki's frame-timing
  diagram, because MMC3's scanline counter watches PPU A12 (step 3.10).
- **Interrupt and DMA timing:** NMI/IRQ polling at the documented cycle,
  OAM DMA 513/514 cycles, DMC DMA cycle stealing (step 3.9, with the APU).
- **AccuracyCoin** (below) is the running accuracy score; its CPU,
  interrupt, VBlank/NMI and instruction-timing pages are in the M3 exit,
  the rest is measured, not required.
- Not targeted: mid-scanline palette tricks, PPU revisions other than G.

## Test ROMs (fetched by `tools/fetch-roms.sh` into `roms/nes/`, never committed)

| Suite | What | Pass signal | Licence (*to verify* when pinning) |
|---|---|---|---|
| SingleStepTests 6502 (Tom Harte; JSON, ~10 000 cases per opcode with every cycle's bus access) | the shared 6502 core, opcode by opcode | exact final state and per-cycle bus trace | believed MIT; large (hundreds of MB): pin, fetch once, maybe a subset per opcode |
| `nestest` (kevtris) + `nestest.log` | every official and undocumented opcode, flags | log equality from $C000 (automation mode) | freely distributed, no formal licence |
| Blargg `instr_test-v5`, `instr_timing`, `cpu_interrupts_v2`, `cpu_dummy_reads`, `cpu_dummy_writes` | CPU behaviour, timing, interrupts, dummy accesses | status byte at $6000 (0 = pass), signature $DE $B0 $61 at $6001, text from $6004 | as Blargg's GB tests: fetch, don't commit |
| Blargg `ppu_vbl_nmi`, `sprite_hit_tests`, `sprite_overflow_tests`, `oam_read` | PPU timing | same, or screen | same |
| Blargg `apu_test`, `dmc_dma_during_read4` | APU, DMC DMA (step 3.9) | same | same |
| Holy Mapperel (Damian Yerrick) | mapper behaviour per mapper: PRG and CHR banking, mirroring, work RAM, MMC3 IRQ | screen: "DETAILED TEST RESULT: 0000" (step 3.10) | zlib (checked 2026-10-03); release 0.02, 13 boards of mappers 0-4 and 7 |
| AccuracyCoin (100thCoin) | 141 tests over 20 pages: CPU, open bus, unofficial opcodes, interrupts, DMA, APU, PPU timing and internals (see "AccuracyCoin") | PASS/FAIL and error code per test on screen; Start runs all and draws a results table | MIT (checked 2026-10-03); NROM, so it fits from step 3.2 |

Test hooks the spec and the Clash simulations both need:

- **Blargg memory protocol:** watch writes to $6000 to $60FF, stop when
  the status byte leaves $80 (running) with the signature present; read
  the text. Needs 8 KiB PRG RAM at $6000 for those ROMs (as on hardware).
- **`nestest` log comparison:** start at $C000 with the documented
  initial state; compare registers and cycle count per instruction (the
  log's PPU column once the PPU exists).
- **Screen goldens:** 256×240 PNGs in `systems/nes/test/golden/spec/`,
  checked by eye once, then frozen; layer 4 goldens ×2 and letterboxed.
- Our own regression ROMs in assembly with **ca65** (cc65 in
  `flake.nix`, step 3.0), assembled by the build like the Game Boy's
  rgbds ROMs.

## AccuracyCoin

Reviewed 2026-10-03 (planning). A single NROM cartridge with 141 tests
that print PASS or FAIL with an error code, plus 5 "DRAW" tests that
only print power-on state. MIT; no releases, actively changed (tests get
added and removed), so **pin a commit and the `.nes` file's SHA-256** in
`tools/fetch-roms.sh` and update deliberately, noting score changes. The
repo ships the assembled `.nes`; use it rather than building the
assembly (it targets nesasm, and a Windows binary is in the repo).

Why it matters here: it tests exactly the things this design otherwise
learns slowly: dummy reads and writes, open bus (CPU and PPU), DMC/OAM
DMA timing and conflicts, interrupt polling per instruction, VBlank/NMI
to the PPU cycle, sprite evaluation internals. Because the Clash core is
lockstep-tested against the spec, every test the spec passes the core
gets almost for free; the work happens in Haskell, where it's fast.

**Pages at the pinned commit (673ef55; unchanged at d103cbd, pinned
2026-10-22, D.9)**, number and title as the ROM shows them; when
re-pinning, compare against this list:

| # | Page | # | Page |
|---|---|---|---|
| 1 | CPU Behavior | 12 | CPU Interrupts |
| 2 | Addressing mode wraparound | 13 | APU Registers and DMA tests |
| 3 | Unofficial Instructions: SLO | 14 | APU Tests |
| 4 | Unofficial Instructions: RLA | 15 | CPU Behavior 2 |
| 5 | Unofficial Instructions: SRE | 16 | Power On State (draw-only) |
| 6 | Unofficial Instructions: RRA | 17 | PPU Behavior |
| 7 | Unofficial Instructions: \*AX | 18 | PPU VBlank Timing |
| 8 | Unofficial Instructions: DCP | 19 | Sprite Evaluation |
| 9 | Unofficial Instructions: ISC | 20 | PPU Misc. |
| 10 | Unofficial Instructions: SH\* | 21 | Advanced Background Evaluation |
| 11 | Unofficial Immediates | 22 | Advanced Sprite Evaluation |

**Required for the M3 exit** (PLAN.md; by page name, as of the pinned
commit; checked 2026-10-03: these five pages hold exactly the tests the
planning review intended, none moved or added). If a re-pin moves one of
these tests to another page, the requirement follows the test; if it adds
tests to these pages, they are not required until the user agrees.

| Page | Tests (all must pass) |
|---|---|
| CPU Behavior | ROM is not writable, RAM Mirroring, PC Wraparound, The Decimal Flag, The B Flag, Dummy read cycles, Dummy write cycles, Open Bus, All NOP instructions |
| Addressing mode wraparound | Absolute Indexed, Zero Page Indexed, Indirect, Indirect X, Indirect Y, Relative |
| CPU Interrupts | Interrupt flag latency, NMI Overlap BRK, NMI Overlap IRQ |
| PPU VBlank Timing | VBlank beginning, VBlank end, NMI Control, NMI Timing, NMI Suppression, NMI at VBlank end, NMI disabled at VBlank |
| CPU Behavior 2 | Instruction Timing, Implied Dummy Reads, Branch Dummy Reads, JSR Edge Cases, Internal Data Bus |

State on the spec and the core (step 3.6): all pass except Interrupt flag
latency, NMI Overlap IRQ, Instruction Timing, Implied Dummy Reads and
Internal Data Bus, which all need the DMC (step 3.9).
Step 3.9 (APU, DMC and its DMA): **every test on the five required pages
passes**; 121/144 overall (from 108).

How to use it:

- **Target hardware** (the ROM accepts several behaviours on some tests
  and reports which): US NES-001, RP2A03G CPU (mid-1990 or later DMA
  abort behaviour), RP2C02G composite PPU, NES-style controller reads.
  Record the chosen variants; a test passing with another console's
  behaviour is a failure for us.
- **Automation:** the spec and the Clash simulations script the
  controller (Start on the first menu entry = run all), wait for the table,
  and compare its screen with a golden, and the per-test result bytes in
  RAM where the assembly stores them (*to find* in `AccuracyCoin.asm`),
  so the score is a number in the test output.
- **Dependencies:** many tests check prerequisites first (open bus,
  DMC DMA timing, `SLO abs,X`) and report a prerequisite error code;
  fix in that order rather than by page.
- **Cost check:** some pages model deep PPU internals (OAM corruption,
  misaligned OAM, `$2004`/`$2007` stress, ALE and hybrid addresses, data
  bus decay within a second). Each costs logic in a PPU that has only ≈ 4
  clocks per dot. Pass them in the spec when cheap; in hardware only if
  the timing and LUT budget allow, and say so in the status log.

## Reference model

Two layers, both cycle-timed and fast enough for multi-second test ROMs
(as `Gb.Spec`: unboxed mutable state in `ST` behind a pure interface):

- **`Mos6502.Spec`** (shared package, below): one CPU cycle per step; the
  caller supplies the bus (read/write callbacks or a request/response
  step), so the same model serves the NES and any later 6502 machine.
  Parameter: decimal mode on/off.
- **`Nes.Spec`**: the 2A03 around it (memory map, OAM DMA, controllers,
  APU frame counter early, because its IRQ is in `cpu_interrupts_v2`),
  the PPU at dot level, mappers.

Events per CPU cycle for the lockstep test (like the Game Boy's per
M-cycle): bus access (address, data, read/write), instruction boundary
with registers, interrupt dispatch, DMA cycles, PPU scanline/VBlank.

## Hardware design

### Shared 6502 package

A new package for CPU cores used by more than one system (suggested
`cpus/mos6502`, modules `Mos6502.Spec`, `Mos6502.Types`/`Decode`,
`Mos6502.Core`; naming is the session's call, record it in ARCHITECTURE).
The Clash core is a pure per-cycle function like `Gb.Cpu.cpuCycle`:
this cycle's read data in, next cycle's bus request out, with RDY (for
DMA stalls), IRQ and NMI inputs. It knows nothing about the NES.

### Clocking

Everything in **`DomPix`** (25.176 MHz), with a phase-accumulator **dot
enable** (5.369318 / 25.17623, so one dot every ≈ 4.69 pixel clocks) and
a CPU enable every third dot. **This is tighter than the Game Boy** (≈ 6
clocks per dot there, and `gb` closed timing with only +0.909 ns). Design
the PPU so a dot's work fits in 4 clocks, with VRAM/CHR fetches (one per
2 dots) issued a dot ahead; register memory requests from the start
rather than after a failing build (gameboy.md 2.8 has the lesson). If it
still doesn't fit, record the trade-off before changing the clock plan.

### Frame-rate mismatch

The NES draws at 60.0988 Hz, the output runs at 59.94 Hz: the core is
*faster* here (the Game Boy was slower). The Game Boy's three-buffer
scheme handles both directions; here it drops one frame about every
6.3 s. Generalise `Gb.Display.Banks` into a platform module rather than
copying it, keeping every Game Boy golden identical, and only after side
track A (PLAN.md, "Side tracks") has merged.

### Memories (budget, of 140 RAMB36)

| Memory | Size | RAMB36 (approx.) |
|---|---|---|
| Frame buffers | 3 × 256×240 × 6 bits (colour index), three RAMs; emphasis latched per line, not per pixel | 36 (as built, 3.7) |
| PRG ROM | up to 256 KiB at first | 64 |
| CHR ROM / RAM | up to 128 KiB | 32 |
| PRG RAM | 8 KiB (Blargg's tests need it) | 2 |
| CPU RAM, VRAM | 2 + 2 KiB | 1 to 2 |
| OAM, palette RAM | 256 B, 32 B | registers / LUTRAM |
| Text overlay | as CHIP-8 | 1 |

About 130, so tight. Per-pixel emphasis (9 bits) would cost 15 more and
force a smaller CHR budget; per-line emphasis is a documented
approximation. Larger cartridges (MMC1 up to 512 KiB PRG, MMC3 up to
512 KiB PRG + 256 KiB CHR) need the DDR path (side track C).

**Since side track D (D.7, 2026-10-08, for Kirby's Adventure):** PRG ROM
is read from DDR (up to 512 KiB; the 64 tiles are free), CHR is 256 KiB
of block RAM (64 tiles), OAM 8 small RAMs (LUT RAM). The DDR layout in
the media region: the whole `.nes` file from 0x02000000
(`Nes.Top.romBase`), PRG ROM at +16.

### Display path

256×240 ×2 = 512×480 at (64, 0): integer scale, no division. Palette:
64 entries generated in Haskell from the NTSC signal levels documented
on the wiki (our own table, not an emulator's palette file; record the
method). **Colour depth decision (step 3.7):** the platform's pixel
stream is 12-bit RGB, which shows NES colours only approximately. Prefer
widening the stream to 8 bits per channel (VGA keeps the top 4 bits;
existing 12-bit colours expand ×17, so CHIP-8 and Game Boy HDMI goldens
should come out identical: check that, it's the regression test). It's
a platform change; do it on main, after side track A merges. (Done in 3.7.)

### Input

Bits 0 to 7 of `ciKeys`, ORed from:

- **Desktop keyboard (confirmed by the user, 2026-10-03; the Game Boy's
  map):** W/S/A/D = Up/Down/Left/Right, J = B, K = A, L = Start, Left
  Shift = Select. `make keys SYSTEM=nes` selects it; once side track B
  has merged, it is the NES section of the key-map defaults and the user
  can change it with `make keymap SYSTEM=nes`.
- **Board buttons:** BTNU/D/L/R = D-pad, BTNC = A, as the Game Boy.
- Later: a real NES/SNES pad through a Pmod (`Platform.Input.SnesPad`).

The controller port model (shift register, strobe through $4016) is in
the core; the platform only provides the 8 live bits.

### ROM loading

Through the PS link and the SD menu from the start (`systemNes` = 3,
reserved in ARCHITECTURE.md): folder `/nes`, files `.nes`. The PS reads
the 16-byte iNES / NES 2.0 header, refuses unsupported cartridges with a
message on the menu screen (`nesCheck` in `platform/ps/menu.zig`), and
writes the file. **As built (3.8):** the PS streams the `.nes` file
unchanged from link address 0 with the core held, as for the Game Boy
(the generic `load.zig`); the PL routes it by file offset
(`Nes.Loader.loadRoute`: header into a register, PRG from offset 16, CHR
after PRG), and decodes the header again in Clash when hold falls
(`headerCfg`, tested against `parseHeader` + `cartCfgFromHeader` on every
fetched ROM and 20 000 random headers). Unsupported: the core stays held
and LD0 is off. `make load SYSTEM=nes` goes through the same path
(`link --system nes` since side track B: the whole `.nes` file, up to
16 + 384 KiB as for the menu, iNES magic checked).

**Since side track D (D.7):** the menu and `make load` put the whole
file into DDR at 0x02000000 first (as the Game Boy's, `streamDdr`, the
core held), then send its header and CHR ROM over the link at their file
offsets, read back from DDR (`menu.zig`'s `ddr_link`, `load.sendFromDdr`;
about 1 s for 256 KiB of CHR); `loadRoute` still routes them, and the PRG
bytes the link might carry are ignored. Limits: PRG 512 KiB (MMC1 256
KiB: its 512 KiB board SUROM isn't modelled), CHR ROM 256 KiB.

### Mappers

| Mapper | Step | Why |
|---|---|---|
| 0 NROM | 3.6 | test ROMs, simple homebrew |
| 1 MMC1, 2 UxROM, 3 CNROM, 7 AxROM | 3.6 | most homebrew and many test ROMs |
| 4 MMC3 | 3.10 | scanline IRQ from PPU A12; needs the exact fetch pattern |
| 5 MMC5 | D.8 (side track D) | Castlevania III, the Koei games; its scanline IRQ and extended attributes from the PPU's fetches (their kind, `FetchKind` / `Fetch`) |

### Sound (step 3.9)

APU in the spec, then Clash, in lockstep (channel outputs per CPU
cycle, as the Game Boy's). Mixer: the wiki's non-linear formulas as two
lookup tables (pulse, and triangle/noise/DMC), then the existing
`Platform.Audio.Spdif` path at 48 kHz. DMC DMA steals CPU cycles and has
the controller-read conflict; model both, they're in Blargg's tests.
**As built (3.9):** see the status log; the filters (two 15.6 kHz
low-passes, a 15 Hz high-pass started at the power-on level) are in
`Nes.Audio`, the tables in `Nes.Mixer`.

## Test plan

| Test | Layer | Source |
|---|---|---|
| 6502 per opcode, per cycle | 0 (spec), 1/2 (Clash core) | SingleStepTests |
| Decoder round trip and totality (256 opcodes) | 1 | hedgehog |
| CPU lockstep (bus trace per cycle) | 2 | random programs, `nestest`, Blargg CPU |
| PPU lockstep (per dot state, per frame pixels) | 2 | Blargg PPU, ca65 regressions, homebrew |
| Full-core ROM runs | 3 (short), 4 (long) | Blargg, `nestest`, Holy Mapperel |
| VGA/HDMI goldens, S/PDIF tone | 4 | spec goldens ×2; a ca65 tone ROM |
| Board | 6 | monitor, speakers |
| Playable game | 2, 6 | any freely licensed homebrew (as for the Game Boy: your pick; licence of code *and* graphics checked; supported mapper; pinned hash) |

## Status log

*(fill in per completed step: date, commit, test results, WNS, utilisation)*

### 3.0 and 3.1 (2026-10-03, main track)

- **3.0** `tools/fetch-roms.sh` fetches and hash-checks SingleStepTests
  (commit 2f6980a: the whole `nes6502` set, ~1 GB, and the decimal-mode
  opcodes of the `6502` set; per-file SHA-256s in
  `tools/roms-6502.sha256`), nestest with its log, the Blargg suites
  (CPU, PPU, APU, DMC DMA, MMC3, all from the christopherpow mirror at
  95d8f62) and AccuracyCoin (673ef55, the repo's `.nes`). Holy Mapperel and
  the homebrew game are added with the steps that need them. `cc65` in
  `flake.nix`; `make nes-asm` assembles `systems/nes/test/asm/*.s` with
  `nrom.cfg` (NROM-128, PRG RAM at $6000); `pass.s` speaks Blargg's
  protocol. `Nes.Cartridge` parses iNES and NES 2.0 (exponent sizes,
  DiskDude junk) and says whether M3 supports a cartridge; `Nes.Hooks`
  reads Blargg's $6000 protocol and parses `nestest.log` (all 8991 lines).
- **3.1** The shared package is `cpus/mos6502` (recorded in
  ARCHITECTURE.md). `Mos6502.Spec`: one bus access per cycle through
  caller-supplied callbacks (`Bus`: read, write, RDY, NMI and IRQ lines),
  mutable state in `ST`, decimal mode by `Config`. Interrupt polling as
  the wiki describes (record per cycle, use the second-to-last cycle's;
  branch quirk; NMI hijack of BRK and IRQ; NMI not taken right after BRK);
  JAM as SingleStepTests shows it ($FFFF, $FFFE, $FFFE, then $FFFF).
  Unstable opcodes: ANE/LXA with $EE, SHA/SHX/SHY/TAS with the
  high-byte replacement on page crossing.
- Proof: `nix develop -c make test SYSTEM=nes` green: `mos6502-spec` 302
  tests (all 256 opcodes × 10 000 cases of `nes6502` with every cycle's bus
  access, 32 decimal-mode opcode files of `6502`, 14 interrupt/RDY/reset/JAM
  cases; 46 s on 12 cores), `nes-spec` 17, `platform-unit` 83, Zig tests,
  XDC check, ROM hashes.
- Not covered yet: interrupt timing against hardware (Blargg
  `cpu_interrupts_v2`, step 3.2).

### 3.2 (2026-10-03, main track)

- `Nes.Spec` (runners: `runBlargg`, `runNestest`, `traceTail`),
  `Nes.Spec.Machine` (memory map, open bus, controllers, OAM DMA, the
  clock interleave), `Nes.Spec.Mapper` (NROM, MMC1, UxROM, CNROM, AxROM;
  8 KiB PRG RAM on every board; since 3.10 unless an NES 2.0 header says
  none), `Nes.Spec.Ppu` (the CPU-visible side:
  dot/frame timing, VBlank and NMI, registers, palette, OAM; rendering is
  3.3), `Nes.Spec.Apu` (frame counter with IRQ, length counters, $4015;
  the channels are 3.9). `cabal run nes-run -- [--trace K] <rom.nes>`
  runs a ROM and prints Blargg's result (or the last K instructions).
- **As built, timing:** master clock in 21.48 MHz ticks; reads at tick 5
  of a CPU cycle, writes at 7, PPU 1 tick behind: two dots before each
  access, one after; NMI/IRQ sampled at the end of the cycle. With that,
  the `$2002` suppression window needs no special case (ppu_vbl_nmi
  checks it in 3.3). APU at the start of each cycle; a `$4017` write takes
  effect 3 cycles later on an even cycle, 4 on an odd one, and the
  sequencer counts from the cycle after (apu_test `6-irq_flag_timing`:
  first IRQ flag 29831 cycles after the write). OAM DMA through the
  6502's RDY input, so interrupts are sampled during DMA; gets on odd
  cycle numbers (`cpu_interrupts_v2/4-irq_and_dma` fails by one cycle
  with the other parity).
- **Found by the ROMs:** LXA ($AB) is stable on the 2A03 with constant
  $FF (Blargg `03-immediate`); SingleStepTests' `nes6502` set used $EE,
  so `Mos6502.Spec.Config` has `cfgLxaConst` and the SingleStepTests
  suite runs with `singleStep2a03`. The older Blargg shell
  (`cpu_dummy_reads`, `dmc_dma_during_read4`) reports through a 57600
  baud serial line on $4016 bit 0: `Nes.Hooks.serialText` decodes it.
  Blargg's $6000 text can run past $60FF (the reader takes up to $7FFF).
- Proof: `nix develop -c make test SYSTEM=nes` green. `nes-spec` 67:
  nestest (registers and cycle count on all 8991 log lines), Blargg
  `instr_test-v5` (16 singles, `all_instrs`, `official_only`),
  `instr_timing` (2), `cpu_interrupts_v2` (5), `cpu_dummy_reads`,
  `cpu_dummy_writes` (oam, ppumem), `apu_test` 1 to 6 (frame counter and
  length counters only), mapper unit tests on made-up cartridges,
  controllers through `joypad.s`. `mos6502-spec` 302, `platform-unit` 83.
- Approximations (documented in the modules): MMC1's PRG RAM disable bit
  ignored; no bus conflicts on UxROM/CNROM; no PPU register warm-up after
  power-on; the nestest PPU column isn't compared yet (3.3).

### 3.3 (2026-10-03, main track)

- `Nes.Spec.Ppu` renders at dot level (module header: fetch pattern,
  shifters, scroll increments, per-dot sprite evaluation with the
  overflow bug, sprite fetches, real sprite X counters and shift
  registers, sprite 0 hit, odd-frame skip, rendering-time quirks of
  $2004/$2007, I/O latch with per-bit decay after 30 frames).
  `Nes.Palette`: our 512-entry table from the 2C02G signal levels (nesdev
  "NTSC video": square wave per hue over 12 phases, emphasis attenuation
  0.746, YIQ decode, FCC matrix; hue offset 3.9/12 so 0x16 is red, 0x1A
  green, 0x12 blue; 0x00 comes out (102, 102, 102)). `Nes.Screen` (PNG
  goldens), `Nes.AccuracyCoin` (reads the ROM's own test table at $8100).
- **As built, found by the ROMs:** the odd-frame skip looks at rendering
  as it was when dot 338 of the pre-render line ran (ppu_vbl_nmi
  `10-even_odd_timing`; one dot either way fails). The nestest log's PPU
  column is the dot about to run (ours holds the last dot run; with
  alignment 1 that's a fixed +1). Sprite fetches re-check the range with
  the line's low 8 bits, so the pre-render line acts as line 5 (sprites
  can appear on line 0; AccuracyCoin "Sprites On Scanline 0", and no
  sprite 0 hit from Y = 239). `$2004` writes during rendering: OAMADDR + 4,
  AND $FC.
- Proof: `nix develop -c make test SYSTEM=nes` green, `nes-spec` 102:
  nestest with the PPU column (all 8991 lines), Blargg `ppu_vbl_nmi` (10
  singles and the combined ROM), `oam_read`, `sprite_hit_tests` (11),
  `sprite_overflow_tests` (5), spec goldens (`test/golden/spec/`: nestest
  menu, two Blargg text screens, `ppu_scene.s`, AccuracyCoin's results
  table), checked by eye and frozen.
- **AccuracyCoin (pinned 673ef55): 108 / 144** (`test/golden/
  accuracycoin-spec.txt` lists every test; the suite fails if the score
  drops). Failing: everything that needs the DMC (interrupt flag
  latency, NMI overlap IRQ, the DMA page, instruction timing, implied
  dummy reads, internal data bus, INC $4014, the SH* opcodes' test 7):
  step 3.9; APU frame counter IRQ test 7 and controller strobing test 4
  (get/put alignment details, 3.9); deep PPU internals (OAM corruption,
  $2004/$2007 stress tests, ALE + read, hybrid addresses, stale BG shift
  registers, BG serial in, frozen OAM2 increment, misaligned OAM2
  address, stale sprite shift registers test 3, $2002 flag timing):
  left for now, each would cost logic in the Clash PPU.
- **Note:** the pinned AccuracyCoin has 22 pages, not the 20 the planning
  review saw. Settled with the user (2026-10-03): the M3 exit names the
  pages, not numbers; the required tests are listed under "AccuracyCoin",
  "Pages at the pinned commit".

### 3.4 (2026-10-03, main track)

- `Mos6502.Types` (Op, Mode, Kind), `Mos6502.Decode` (all 256 opcodes
  from the bit groups), `Mos6502.Core`: the Clash 6502 as a pure
  function per CPU cycle (`cpuCycle cfg cpu in -> (cpu', out)`): read
  data, NMI/IRQ lines and a stall flag in, the next bus request and
  "a spec step ended" out. RDY: the machine repeats a read and sets
  `ciStall`; only interrupt sampling moves on. Configuration as a value
  (`coreRicoh2a03`, `coreNmos6502`: decimal mode, LXA constant).
- **As built:** mode sequencing by cycle number, then a shared operand
  stage (access, RMW old write-back, new write); BRK, IRQ/NMI and reset
  share one 7-cycle sequence (vector chosen after PCL is pushed, I set
  after P). A JAMmed CPU ignores RDY, as the spec's JAM cycles do (found
  by the random lockstep).
- Proof: `nix develop -c make test SYSTEM=nes` green; new suite
  `mos6502-lockstep` 300: decoder (total; exactly the 12 JAMs; the 151
  documented opcodes; stores write, RMWs are RMW), the core on all of
  SingleStepTests (nes6502 2.56 M cases and the decimal opcodes of 6502:
  bus trace, registers, RAM), lockstep with `Mos6502.Spec` on random
  64 KiB programs with random NMI/IRQ/RDY windows (400 on the 2A03, 200
  with decimal mode; per step: kind, every bus access, registers), and
  four deliberately broken cores caught (IRQ missed at the end of opcode
  fetches, RDY ignored, ADC #imm decoded as SBC #imm, decimal mode on a
  2A03). Clash compiles the core (a `mealy` around `cpuCycle`) to 3500
  lines of Verilog; Verilator lint shows only the usual Clash-output
  warnings.

### 3.5 (2026-10-03, main track)

- `Nes.Ppu`: the Clash PPU, a pure `ppuTick` per clock for one of a dot,
  a register read or a register write (each one clock, so a dot takes
  one of the ≈ 4.7 clocks available); `ppu` is the Signal form with OAM
  in block RAM. Memory ports: A for rendering fetches (address on the
  first dot of each 2-dot fetch, byte back with the next tick), B for the
  CPU's $2007 accesses (read buffer filled with the next tick). Secondary
  OAM, sprite units and palette are registers.
- Spec change for this: `Nes.Spec.Ppu` now reads memory on the first dot
  of each fetch pair (the address dot; hardware reads on the second, which
  only differs if the CPU writes in between). All spec tests, goldens and
  the AccuracyCoin score unchanged.
- Proof: `nix develop -c make test SYSTEM=nes` green; new suite
  `nes-lockstep` 30: the spec machine runs with a PPU hook (every dot and
  register access), the Clash PPU gets the same ticks with its memory
  answered from the spec's PPU bus; compared: the whole PPU state on
  every register access and every 4th dot, register read values, NMI,
  every pixel, every PPU bus write, OAM per frame. On `ppu_scene.s`,
  nestest, all 11 sprite_hit and 5 sprite_overflow tests, 6 ppu_vbl_nmi
  tests, oam_read, AccuracyCoin's menu, and 60 random NROM programs (3
  frames each); three deliberately broken PPUs caught (fine X ignored,
  OAM bit error, VBlank never suppressed). Clash compiles it (5500 lines
  of Verilog, the usual Clash lint warnings only). The suite takes about
  5 min of wall time on 12 cores.

### 3.6 (2026-10-03, main track)

- `Nes.Core`: the whole NES in Clash, a pure `coreStep` per clock with
  every memory outside it (CPU side: PRG ROM, PRG RAM, CPU RAM; PPU
  ports A and B: CHR and nametable RAM after the cartridge board; OAM).
  A CPU cycle is dot, dot, access (APU first; OAM DMA through RDY), the
  read's data clock, dot, `cpuCycle`; it starts on `kiGo` (three dots of
  time). `Nes.Apu` (frame counter, length counters, $4015) and
  `Nes.Mapper` (NROM, MMC1, UxROM, CNROM, AxROM; `cartCfgFromHeader`)
  are the spec's rules in Clash. `nesCore` is the hardware form: block
  RAMs for PRG (256 KiB) and CHR (128 KiB) from image files, PRG RAM,
  CPU RAM, nametable RAM, OAM; ports A and B share one RAM each (a clock
  carries at most one PPU tick).
- **As built:** the spec's first CPU cycle after power-on has one dot
  before its access, not two (the PPU clock starts one master tick
  behind), so the core starts in its second dot; with three dots per
  cycle from the start the PPU ran one dot ahead and `$2002` reads raced
  differently after a few frames (found by the full-core lockstep).
  The Clash state records use `StrictData` (no thunk build-up in
  simulation; synthesis ignores strictness), and conversions into
  `Index` use `bitCoerce`, not `fromIntegral` (no Integer in the netlist).
- Proof: `nes-lockstep` adds a full-core group: the core against the
  spec step by step (every CPU cycle's bus access, registers after every
  step) for 200 000 steps on 26 test ROMs (Blargg CPU, APU, PPU, sprite
  tests), on nestest's menu, `ppu_scene.s`, `joypad.s` and AccuracyCoin's
  menu; and test ROMs run to the end on the core alone: with
  `NES_CORE_ROMS=all` all 59 Blargg ROMs of steps 3.2 and 3.3 pass, and
  AccuracyCoin on the core equals the spec test by test (108/144) (11
  min wall time on 12 cores), by default a quick subset of 6. Clash
  compiles `nesCore` (18 600 lines of Verilog), Verilator lint clean with
  `boards/sim/clash.vlt`.

### 3.7 (2026-10-03, main track)

- **Colour depth:** the platform's pixel stream is now 24-bit
  (`Rgb888`; VGA shows the top 4 bits of each channel). Older 12-bit
  colours widen ×17 (`fromRgb444`), and the 4:2:2 converter's 8-bit
  coefficients are the old ones ×4096/17, so every CHIP-8, Game Boy, M0b
  and M0d golden is unchanged at layers 3 and 4 (platform-unit checks
  all 4096 old colours against the old converter).
- **Frame buffers:** `Platform.Video.FrameBuffers.tripleBuffer`, the
  Game Boy's bank rotation generalised (size and pixel width as types).
  It uses three block RAMs, not one three times as deep: Vivado rounds a
  memory's depth up to a power of two before splitting it into RAMB36s,
  so one 184320 × 6 memory would need 48 RAMB36s. That doesn't fit next
  to PRG (64) and CHR (32), and synthesis fell back to 23 088 LUTRAMs
  (placement failed).
- `Nes.Display`: a dot enable from the 25.175 MHz clock (phase
  accumulator, 5.369 MHz on average), 6-bit colour index per pixel and
  emphasis per line (`tripleBuffer` @240 × 3 bits), the palette LUT
  (512 entries: 64 colours × 8 emphasis, 24-bit, made by Template Haskell
  from `Nes.Palette`), ×2 at (64, 0).
- `Nes.Top` (`nes_top`): the core with the cartridge built in
  (`nes-image` writes `build/nes/{prg,chr,cart}.bin` from `ROM=`), VGA
  and HDMI (DVI), the board buttons as controller 1 (BTNC = A, BTNU/D/L/R
  = D-pad), the LEDs show controller 1. `test/asm/buttons.s` draws a
  sprite per pressed button.
- **Deviation:** `make keys SYSTEM=nes` moves to step 3.8. The desktop
  keys reach the PL only through the PS link, which `nes_top` gets in
  3.8, and `platform/host/` belongs to side track B until it merges.
- Proof: `nes-golden` (VGA and HDMI goldens from the frozen spec screens
  of `ppu_scene.s`, nestest and `buttons.s` with A and Up held; the
  display path in Clash simulation equals the VGA golden). `make ci`
  runs `ci-nes`: `make sim SYSTEM=nes` and `make resim` of nestest and
  `buttons.s` (`--btn 0x11`, the input path through the debouncer and
  the controller port), every frame identical to its golden.
- Vivado (`make bit SYSTEM=nes`, `ppu_scene.s` built in): WNS +8.611 ns,
  WHS +0.131 ns; 11 820 LUTs (22 %), 1 738 registers, 12 DSPs, 87.5 of
  140 BRAM tiles. The frame buffers take 3 × 12 RAMB36 and CHR 32. The
  PRG ROM uses fewer than its 64 because Vivado trims a constant
  ROM image. Once PRG and CHR are RAMs written through the PS link
  (3.8), expect about 136 of 140.

### 3.8 (2026-10-03, main track): done, board-checked

- `nes_top` on the PS link, as the Game Boy: `boards/zedboard/nes_zynq.v`
  (PS7 + `nes_top`, no logic), `systemNes` = 3 on `PS_IN`, the menu's text
  overlay, the control register's pause holding the dot enable low, the
  desktop's keys ORed with the board buttons (`padFromLink` reorders the
  Game Boy's bit order). LEDs: LD0 cartridge supported, LD1 frame
  toggle, LD2 hold, LD7 heartbeat; SW6 shows controller 1.
- Loading (section "ROM loading", as built): `Nes.Loader` (routing by file
  offset, `headerCfg`), `nesCore` holds at power-on while not running and
  takes PRG and CHR writes; menu entry `/nes` (`nesCheck`), Up+Down for a
  second opens the menu (as the Game Boy; C is A). `make keys` / `make load
  SYSTEM=nes` use `link --gb` (same key map, file to address 0, up to
  256 KiB) until side track B merges (since then `--system nes`, its own
  `[nes]` key map, up to 16 + 384 KiB); `make sd` puts `SD_NES_ROMS` into
  `/nes`.
- Proof: `nes-spec` "cartridge loader" (the Clash header decoder equals
  `parseHeader` + `cartCfgFromHeader` on every fetched ROM and on 20 000
  random headers, 9 % of them supported; every fetched ROM streamed
  through `loadRoute` gives `parseRom`'s PRG and CHR); `make test-zig`
  (`nesCheck`, the harness's `bytes` op); `ci-nes` plays the menu script
  (VGA and HDMI goldens, `PS_IN` = 0x00030101) and loads nestest over the
  link into the running `ppu_scene` build: the last frame equals
  nestest's goldens.
- Vivado (`make bit SYSTEM=nes`, `nes_zynq`): WNS +2.338 ns, WHS +0.077 ns;
  13 722 LUTs (26 %), **136.5 of 140 BRAM tiles** (PRG and CHR are RAMs
  now, as expected in 3.7). Sound (3.9) needs no block RAM; MMC3 (3.10)
  adds none (the same PRG and CHR), so the budget holds for M3.
- Game Boy rebuilt with the three-RAM triple buffer: WNS +1.527 ns (was
  +0.320), 83.5 BRAM tiles.

### 3.9 (2026-10-03, main track): done, board-checked (sound)

- **APU spec** (`Nes.Spec.Apu`, from the nesdev wiki pages it cites):
  pulse with envelope and sweep (ones' complement for pulse 1, muting
  regardless of the enable), triangle with the linear counter, noise, the
  DMC (output unit, memory reader, IRQ, loop), the frame counter as
  before. Timers count on get cycles (odd cycle numbers), the triangle's
  every cycle. A $4015 read clears the frame IRQ flag from the next cycle
  on (AccuracyCoin "Interrupt flag latency" 14).
- **DMA unit** (`Nes.Spec.Machine`, the wiki's "DMA"): OAM and DMC DMA
  share it. A halt cycle, then per cycle the DMC's get (once its halt and
  dummy cycles are done), else OAM's get or put, else the CPU's read
  again; the DMC's halt and dummy cycles overlap OAM accesses. Load DMA
  from the get of the second APU cycle after the $4015 write, reload DMA
  from the put after the buffer empties. While the CPU is halted reading
  $4000-$401F, a DMA read elsewhere also reads register $4000 + (addr &
  $1F) (bus conflicts; $4016/$4017 drive their bits over the byte).
- **CPU, both models:** RDY stall cycles move the NMI edge detector on but
  not the interrupt polls (a DMC IRQ raised by the DMA inside a taken
  branch shows after the next instruction). **Joypads:** a run of reads
  of $4016 on consecutive cycles reads one bit and clocks once (/OE).
  **Internal data bus:** $4015's bit 5 comes from the CPU's own last
  access, not the DMA's.
- **Clash:** `Nes.Apu` (the same rules), the DMA unit in `Nes.Core`, the
  channel outputs compared every CPU cycle in `nes-lockstep` (new
  `apu_mix.s`: every channel, sweeps, both noise modes, a looping DMC
  sample). `Nes.Mixer` (the wiki's lookup tables, 15 bits), `Nes.Audio`
  (filters), S/PDIF on HD_SPDIF, the ADV7511 in HDMI mode with audio (the
  Game Boy's table; NES HDMI goldens unchanged).
- **Found by the tests:** the Clash DMC rate table (428) and noise period
  table (4068) were built in too few bits and wrapped; regression tests in
  `nes-spec` ("Clash APU timers").
- **Not modelled / failing:** `double_2007_read` (no DMA: two $2007 reads
  on consecutive cycles; the PPU fills its read buffer at once, the
  hardware some dots later; same class as the $2007 stress tests left in
  3.3). AccuracyCoin: Explicit/Implicit DMA Abort (the aborted DMAs of
  chip revisions), APU Register Activation 4 (a DMA read of $4000-$401F
  while the CPU is elsewhere should see open bus; with that rule the
  run-all never finishes, cause not found; fixed in side track D.1),
  Delta Modulation Channel 21
  (DMA delayed by a $4015 write 2 cycles before the timer), Frame Counter
  IRQ 7, Controller Strobing 4, DMA + $2002 Read 2.
- Proof: `nes-spec` (apu_test 1-8, dmc_dma_during_read4 by screen: three
  "Passed" and dma_2007_read's CRC 5E3DF9C4, AccuracyCoin 121/144),
  `nes-lockstep` (core = spec per cycle, channels included, on apu_mix.s,
  tone.s, apu_test 7 and 8, the DMA ROMs; with `NES_CORE_ROMS=all`
  AccuracyCoin on the core equals the spec test by test), `ci-nes`
  (`tone.s` decoded from S/PDIF in Verilator: 441.2 Hz for 440.4 Hz).
- Vivado (`make bit SYSTEM=nes`): WNS +8.381 ns, WHS +0.043 ns; 20 218
  LUTs (38 %, from 26 %: the APU, the mixer tables and the filters),
  20 DSPs, BRAM unchanged at 136.5 of 140. `make ci` green (15 min).
- **Board check (user, 2026-10-03):** all tests passed and the game
  sounded right, but with a high-pitched hum in silences after music had
  played. Cause: the game silences the triangle with timer period 0
  (about 56 kHz); the console's analogue filters remove that, our two
  15.6 kHz poles don't remove enough, and the 48 kHz sampling folds it to
  about 7.9 kHz. Fix in the speaker path only: `apuAudioChannels` gives
  the triangle's mean at periods 0 and 1 (the APU and the lockstep are
  unchanged). Regression: `tri_ultra.s` in `ci-nes` with the harness's
  new `--expect-quiet 4` (before: consecutive samples differed by up to
  267, about -42 dBFS; after: 0), and a unit test in `nes-spec`.
- Found on the way (by step 3.8's random-header property, a new seed):
  the Haskell header parser computed NES 2.0 exponent sizes in `Int`, so
  2^61 × 5 bytes overflowed to a negative size that passed the size
  checks; the Clash decoder was right. Now capped in `Integer`;
  regression test in `nes-spec` ("cartridge header").
- After the fix: `make bit SYSTEM=nes` WNS +8.009 ns (17 967 LUTs, BRAM
  136.5/140), `make ci` green (16 min).
- **Re-listen (user, 2026-10-03):** the hum is gone.

### 3.10 (2026-10-03, main track): done, board-checked

- **MMC3** (mapper 4; nesdev wiki "MMC3", fetched 2026-10-03) in the spec
  (`Nes.Spec.Mapper`) and in Clash (`Nes.Mapper`): bank select / data
  (PRG modes, CHR A12 inversion, R0/R1 2 KiB), mirroring, the IRQ latch,
  reload, disable / acknowledge, enable. Power-on banks (unspecified on
  hardware; Blargg's mmc3_test_2 relies on them): R0-R7 = 0, 2, 4, 5, 6,
  7, 0, 1. $A001 (PRG RAM protect) ignored, as most emulators.
- **Scanline counter:** clocked by a rise of PPU A12 after A12 has been low
  for 3 falling edges of M2 (CPU cycles); reload when 0 or asked to, else
  count down; IRQ when 0 and enabled (the "normal"/Sharp revision:
  `5-MMC3` passes, `6-MMC3_alt` is the other revision and fails, as
  expected). Both PPU models report every address on their bus: fetches,
  $2007 accesses, v after $2006 writes and $2007 increments while
  rendering is off (`pbAddr` / `poBus`), and the pattern address on the
  idle dot 0 of lines 0-239 (nesdev wiki "PPU rendering"; without it the
  counter was clocked again at dot 5 with the background at $1000:
  `4-scanline_timing` #12). The unused nametable fetches at 338 and 340
  now keep their byte for that address.
- **PRG RAM** only when the header gives some: iNES 1.0 always (8 KiB),
  NES 2.0 by its size (Holy Mapperel's MMC1 boards checked that work RAM
  is absent and found it, `5000`).
- **Holy Mapperel 0.02** (zlib) fetched: 13 boards of mappers 0, 1, 2, 3,
  4 and 7 within the BRAM budget, each "DETAILED TEST RESULT: 0000" on the
  spec (`nes-spec`), core = spec in lockstep (`nes-lockstep`).
- Found by the tests: the Clash PPU lacked the dot-0 address (only the
  full run of `4-scanline_timing` on the core showed it); the lockstep now
  compares the MMC3 counter after every step, and the new
  `mmc3_bg1000.s` (background at $1000, IRQ every 11 lines) catches that
  at step 17131 with the old PPU.
- Proof: Blargg `mmc3_test_2` 1-5 pass on the spec and, run to the end, on
  the core; Holy Mapperel as above; AccuracyCoin unchanged (121/144).

### Homebrew (the M3 exit's game; picked 2026-10-03)

| Game | Board | Licence (from the repositories) | Fetched |
|---|---|---|---|
| Thwaite 0.04 (Damian Yerrick) | NROM-128, 8 KiB CHR ROM | GPLv3 or later (`LICENSE.txt`, README "Legal"); the README credits "program, graphics, music: Damian Yerrick" (plus Beethoven, public domain) | GitHub release v0.04, SHA-256 pinned |
| Double Action Blaster Guys v2 (NovaSquirrel) | NROM-256, 8 KiB CHR RAM, NES 2.0 header | zlib (`LICENSE.txt`, repeated in the sources) | GitHub release v2, SHA-256 pinned |

Neither repository licenses the graphics and music separately: they are
in the repositories, by the same authors, under the repository licence,
so covered by implication (raised with the user). Ruled out: Nova the
Squirrel (assets CC BY-NC-SA), Super Tilt Bro. (mapper 30), RHDE (two
players), Shiru's and other games (no free licence found). A fully free
MMC3 game with scrolling exists (Annalog, GPLv3) but has no prebuilt ROM.
Both boot to their title screens on the spec; `make sd` puts them in
`/nes/homebrew`.

### M3 exit (board check by the user, 2026-10-04)

All passed on the board over HDMI (bitstream of 3.10, `make prog
SYSTEM=nes`, cartridges from the SD menu): Blargg `instr_test-v5`,
`ppu_vbl_nmi`, `sprite_hit_tests`, `apu_test`, `mmc3_test_2` 1-5;
AccuracyCoin "run all" with the spec's results table (121/144, every test
on CPU Behavior, Addressing Mode Wraparound, CPU Interrupts, PPU VBlank
Timing and CPU Behavior 2 passing): hardware = model; Holy Mapperel's
MMC3 board "0000"; Thwaite playable from the desktop keyboard with music
and sound. Known gaps carried forward (status logs 3.3, 3.9):
`double_2007_read` and the $2004/$2007 stress tests (PPU read-buffer
timing), AccuracyCoin's DMA-abort tests, APU Register Activation 4 and a
few APU and PPU-internals tests; cartridges above the BRAM budget (side
track C). BRAM use is 136.5 of 140 tiles.

## Side track D: accuracy follow-ups (PLAN.md, "Side track D")

Branch `side-nes-accuracy`, alongside side track C. AccuracyCoin (pinned
673ef55) is the score; target hardware as in "AccuracyCoin" above.
Debugging aids: `cabal run nes-run -- --accuracycoin <AccuracyCoin.nes>`
prints the results list (the golden's format) and the score in about 20 s;
`--accuracycoin-trace K <AccuracyCoin.nes> N` runs the run-all for N CPU
cycles and prints the last K instructions and zero page (`$10` is the
error code a test is at, `$35` "running all tests").

### D.1 APU register activation (2026-10-04)

- **Cause of the hang** (step 3.9): not the open-bus rule itself but the
  conflict rule next to it. "APU Register Activation" 5 runs an OAM DMA of
  page $40 with the CPU halted at $4001; the byte the DMA leaves on the bus
  becomes the opcode the CPU runs there. The status reads of the mirrors
  ($4035, $4055, ...) must give the DMA the *status* ($04: the frame IRQ
  flag, cleared by the first read, is gone), which the put writes to the
  bus and the following open-bus reads repeat: $00, BRK. The spec gave
  the DMA the byte on the bus instead ($40), so the CPU ran RTI at $4000
  and never came back.
- **Rules now** (`Nes.Spec.Machine.dmaRead`, `Nes.Core.getAt`; nesdev
  wiki "DMA", "Register conflicts"): the registers are decoded from bits
  15-5 of the CPU's address and 4-0 of the DMA's. CPU halted outside
  $4000-$401F: a DMA read of $4000-$401F sees open bus, no side effects.
  CPU halted inside, DMA elsewhere: register $4000 + (addr & $1F) is read
  too; $4015 gives the DMA the status with bit 5 from the byte on the bus,
  and the bus keeps that byte (as before, the DMC test "DMC DMA Bus
  Conflicts" sees the bus); $4016/$4017 as before (the controller is
  clocked and drives its bits over the byte). Checked and *not* adopted: a
  "CPU RAM wins over the controller" rule read into test 7's table; the
  test accepts both values, so nothing supports it.
- Regression: `test/asm/apu_reg_active.s` (in `nes-spec` and the core
  lockstep). Instead of AccuracyCoin's DMC-timed `STA $4014` from $3FFE,
  it runs `STA ($F0),Y` from $3FFE (opcode from the PPU I/O latch, operand
  from the read buffer), which needs no cycle timing: the next opcode
  fetch, at $4000, is the DMA's halt cycle. Fails with status 2 on the old
  spec (DMA read $4015 with the CPU in ROM) and with 5 (RTI at $4000)
  with only the open-bus rule.
- AccuracyCoin: **122/144** (APU Register Activation passes, behaviour 2:
  "OAM DMA Bus Conflict Clocks"); run-all takes 123.2 M cycles.
- Gate: `make test SYSTEM=nes FULL=1 NES_CORE_ROMS=all` green (6 suites;
  nes-lockstep 157 in 42 min, AccuracyCoin on the core equal to the spec
  test by test).

### D.2 PPU $2007 timing (2026-10-04)

- **$2007 state machines** (`Nes.Spec.Ppu.dataMachine`, `Nes.Ppu.dataMachine`;
  AccuracyCoin's "$2007 Stress Test" comments give the 2C02's latch
  chain): a read returns the buffer at once; the ALE comes on the 3rd dot
  after the CPU's access (the 2nd PPU cycle after the CPU cycle ends) and
  latches v; the 5th dot reads memory into the buffer, then v increments.
  While rendering fetches, the buffer gets the fetch's byte instead (a
  fetch on that dot or the one before: the spec reads on the first dot of
  a fetch, the hardware on the second). Writes likewise: ALE on the 3rd
  dot, the write on the 4th, then the increment; a memory write on a dot
  where rendering fetches is lost (the hardware is unstable there, and the
  core's port A and B then never meet in one tick).
- So Blargg `double_2007_read` shows "22 44 55 66 77" (CRC 85CFD627, one
  of the four its source lists for real consoles) and `read_write_2007`
  still passes (`STA $2007,X`: the write lands after the dummy read's
  increment, which needs the write machine). The increment has to come
  after the fill, not at the ALE: during rendering it moves coarse X and
  fine Y, and the stress test's key shows the fetches in between using the
  old v.
- **Dot 257:** its garbage nametable fetch reads with v from before the
  horizontal copy from t (the test's "common mistakes").
- **$4015 and the frame IRQ flag:** the flag a read clears now drops at
  the start of the next *get* cycle, not simply the next cycle. A read on a
  get is followed by a put that still sees it. AccuracyCoin's sync routines
  (`SLO $4015,X`: two reads on consecutive cycles) tell get from put that
  way; the old rule always gave "put", so its syncs ran one CPU cycle
  early on half the parities (the stress test was 3 dots off). This also
  fixed "DMA + $2002 Read" (step 4's list) and moved "Frame Counter IRQ"
  (7 to 19) and "Misaligned OAM2 Address" (3 to 4) further. Regression:
  `test/asm/irq_clear_get.s` (three measurements whose gaps differ by one
  cycle, so exactly one adjacent pair must differ whatever the parity;
  status 2 on the old rule).
- **OAM attribute bytes** are stored without bits 2-4 (they don't exist on
  the 2C02G), so the evaluation's reads see them 0 too, as the "$2004
  Stress Test" key shows ($61 for $7D). No score change yet: that test
  stops later, where after 8 sprites the key alternates primary-OAM reads
  with a frozen secondary-OAM byte (the "Frozen OAM2 Increment" /
  "Misaligned OAM2 Address" internals): left for step 5.
- Debugging aid: `nes-run --accuracycoin-trace K --stop PC` runs to an
  address and dumps RAM pages 0, 5 and 6 (where the stress tests keep their
  data).

### D.3 DMC DMA aborts (2026-10-04)

Target: the RP2A03G from mid-1990 on (nesdev wiki "DMA", "DMC DMA bugs";
AccuracyCoin "Explicit DMA Abort", "Implicit DMA Abort" behaviour 1).

- **Aborted DMA:** playback stopped in the APU cycle before a reload DMA
  would be scheduled (the 2nd or 3rd CPU cycle before its halt), by $4015
  or implicitly by the last byte of a non-looping sample: the DMA still
  starts and ends after its halt cycle (no dummy, no get). If that halt
  would fall on a write cycle, it doesn't happen at all (a normal DMA
  waits for the next read). `Nes.Spec.Apu` (`dStopAt`, `dAbort`),
  `Nes.Spec.Machine` (`dmaCycle`, `cycleWrite`); Clash: `dmStopAge`,
  `dmAbort` in `Nes.Apu`, `aborting` in `Nes.Core.access`.
- **Unexpected reload DMA** (1990 on): a DMA byte that arrives in the cycle
  in which the output unit finished a byte with the buffer empty still
  gets its reload DMA, looping or not; if that byte stopped playback, from
  the same address (`dEndAt` / `dmEnd`). All three loops of the implicit
  test need it (the third one, looping, too).
- Measured against the tests' own tables of DMA durations (16 offsets
  each; `nes-run --accuracycoin-trace 1 --stop D97A` and `DA90` dump them
  at $50 and $500/$520/$540): all equal.
- Regression: the AccuracyCoin golden in `nes-spec` (both failed before);
  for the core, `test/asm/dmc_abort.s` in the lockstep (450 000 steps): it
  starts and stops 1-byte samples at pseudo-random phases, with the buffer
  empty long enough that a load can land on an output byte's end.
  Checked by mutation: the core without the abort diverges at step 1464,
  without the unexpected reload at step 153 725 (a first version with
  fixed delays never hit that case; the spec's cycle count showed it).
- AccuracyCoin: **126/144**.
- Vivado (`make bit SYSTEM=nes`, D.3 sources): WNS +3.514 ns (from +9.450
  at the M3 exit: the $2007 state machines cost slack, still well clear),
  WHS +0.039 ns, 22 171 LUTs (41.7 %), BRAM 136.5/140 (unchanged).

### D.4 the rest of the 3.9 list (2026-10-04)

- **Frame Counter IRQ** (was error 7, then 19 after D.2): with the inhibit
  bit set, the frame IRQ flag is still set on the first two of the
  sequence's three IRQ cycles (29828, 29829); the third sets it as the
  bit says; the IRQ line follows the flag only without the bit (tests I
  to M). Regression `test/asm/frame_irq_inhibit.s` (polls $4015 every 13
  cycles over 20 frames with I clear: status 2 on the old rule, 3 if the
  IRQ line ignored the bit), in `nes-spec` and the lockstep (mutation
  caught at step 57 404).
- **Delta Modulation Channel** (error 21, tests L to N): a reload DMA that
  comes due before a $4015 enable has taken effect (the load DMA's moment,
  the get of the 2nd APU cycle after the write) waits for it: delayed by
  1, 2 or 3 cycles. Spec `dEnableAt`, Clash `dmEnWait`; `dmc_abort.s`
  catches a core without it.
- **DMA + $2002 Read** passed already with D.2's get-cycle rule.
- **Controller Strobing** (error 4): the controllers see $4016's bit 0 only
  when a get cycle ends (AccuracyCoin: "when the CPU transitions from a get
  cycle to a put cycle"): a write waits for that moment and a later write
  replaces it, so DEC $4016's 1-cycle $41 strobes on one parity only
  (spec `iStrobePend` applied in `endCycle`; core `ksStrobePend` at the
  next put's access). Regression `test/asm/strobe_get.s` (three
  measurements one cycle apart, as `irq_clear_get.s`; status 2 with every
  write seen at once), in `nes-spec` and the lockstep (mutation caught at
  step 93).
- AccuracyCoin: **129/144**; step 4's list is done.
- Gates for D.2 to D.4: `make test SYSTEM=nes FULL=1 NES_CORE_ROMS=all`
  green (nes-lockstep 162 in 39 min, AccuracyCoin on the core equal to the
  spec test by test, 129/144); `make bit SYSTEM=nes`: WNS +6.784 ns, WHS
  +0.121 ns, 20 408 LUTs (38.4 %), BRAM 136.5/140.
- Not done (needs the shared `cpus/mos6502`, which side D doesn't change
  without asking): the SH* opcodes' test 7 (5 tests), "the value written is
  still ANDed with H if the cycle before the write had a DMA".
- **Board check (user, 2026-10-04):** `make prog SYSTEM=nes` from this
  branch (D.4 bitstream), AccuracyCoin run-all from the SD menu: 129/144,
  the spec's table: hardware = model.

### SH* test 7 (2026-10-04; change to the shared 6502 approved by the user)

- SHA, SHX, SHY and TAS store the register *without* the `& (H+1)` when
  RDY held their dummy read (a DMA in the cycle before the write). Same
  rule on the C64 (VICE x64sc, with the VIC-II's stalls), so it belongs in
  `cpus/mos6502`: `Mos6502.Spec` (`rdStalled` in `shWrite`) and
  `Mos6502.Core` (`cHeld`, set in a halted cycle). Regression in
  `mos6502-spec` ("SHA: RDY low on the dummy read ...", fails without);
  the random RDY lockstep in `mos6502-lockstep` catches a core without it.
  SingleStepTests never stall, so they are unchanged.
- AccuracyCoin: **134/144** (all five SH* tests; behaviour 1 for SHA/SHS).
- Gates: `make test SYSTEM=nes FULL=1 NES_CORE_ROMS=all` green (nes-lockstep
  162, core = spec on AccuracyCoin); `make bit SYSTEM=nes` WNS +9.210 ns,
  22 945 LUTs (43 %), BRAM 136.5/140.

### D.5 PPU internals (2026-10-04)

Spec and Clash (`Nes.Ppu`: `pRenHist`, `oam2Inc`, `pEvPost`, `pFlagPend`; the
$2002 patch in `Nes.Core`'s QCpu):

- **Rendering lags PPUMASK by 2 dots** (`renderDelay`; nesdev wiki "PPU
  registers": "toggling rendering takes effect approximately 3-4 dots
  after the write"). Tried 0 to 4 against AccuracyCoin and every Blargg
  PPU test: 2 is the only value that gains and loses nothing. The
  odd-frame skip's special case ("rendering as it was when dot 338 ran")
  was this delay applied locally; it now just uses the delayed flag.
- **Secondary OAM's address register** (`sEvSec`, `oam2Inc`, `oam2Reset`):
  counts 0-31; wrapping raises an "overflowed" flag that freezes it at 0;
  reset (address 0, flag down) after the clear (dot 64), the evaluation
  (256) and the fetch (339), only if rendering runs then. Clear,
  evaluation and sprite fetch all go through it; the fetch latches Y,
  tile and attribute and reads X five times. After all 64 sprites or a
  full secondary OAM, the evaluation's write cycles read it back ($2004
  shows it). After the overflow sprite and its next 3 bytes, the
  evaluation steps through Y bytes only (m back to 0 without carry).
- **$2002's sprite flags** are taken when M2 falls (the end of the CPU
  cycle), the VBlank flag at the access; sprite 0 hit and overflow are
  set one dot after the pixel/evaluation step that finds them.
- **Sprite X counters** (`spriteAt`; Clash `pCounting`): a slot is "counting"
  or "halted"; all start counting at dot 339 if rendering then; a counter
  that reaches 0 halts its slot until the next such dot 339; halted slots
  draw. Counters count on every visible dot even with rendering off, the
  shift registers shift only while rendering (AccuracyCoin "Stale Sprite
  Shift Regs", "Stale BG Shift Registers").
- AccuracyCoin on the spec: Frozen OAM2 Increment, Misaligned OAM2
  Address, $2004 Stress Test, $2002 flag timing, Stale Sprite Shift Regs,
  Stale BG Shift Registers pass: **140/144**; every
  Blargg test and NES golden unchanged. Left (the PPU bus at the level of
  its octal latch and two-dot reads, or analogue OAM corruption; not cheap):
  OAM Corruption, BG Serial In, ALE + Read, Hybrid Addresses.
- Gates for the first part (138/144): `make test SYSTEM=nes FULL=1
  NES_CORE_ROMS=all` green (nes-lockstep 162, core = spec on AccuracyCoin);
  `make bit SYSTEM=nes` WNS +8.327 ns, 23 688 LUTs (44.5 %), BRAM
  136.5/140. Tried and reverted for lack of evidence: the background
  shifters' serial input (1 into the high plane; "BG Serial In" still
  failed with it).
- **The last four** (OAM Corruption, BG Serial In, ALE + Read, Hybrid
  Addresses), studied in TriCNES (MIT; Chris Siebert, 2025; passes every
  AccuracyCoin test; a clone in the git-ignored `local/reference/TriCNES`,
  line numbers in its `Emulator.cs`). Each needs a structural PPU change,
  so they are left for a decision (the PPU has about 4 clocks per dot):
  - two rendering delays, not one: fetches and sprite evaluation see a
    $2001 change 3-4 dots after the write, shifting, pixels and the bus
    2-3 (10002-10012, 2031-2036, 2087-2139);
  - the two-dot fetch with the octal latch: the address on the ALE dot,
    the read on the next with the high byte recomputed from the current v
    for nametable and attribute fetches (4126-4199); $2006 writes land 4-5
    dots later (10157-10163), so a write between the two dots reads a
    "hybrid" address; the $2007 machine's ALE meeting the cadence's read
    feeds the read data back into the latch (4136-4139, 2425-2477);
  - the background shifters' serial inputs (0 low plane, 1 high plane, the
    attribute latch for the attribute shifters; 4334-4340), which only
    show with the separate shift/fetch delays (tried alone: no change);
  - OAM corruption: capture of the secondary OAM address on the dot
    rendering stops (by phase: 3155-3199, 3222-3238, 3399-3444,
    3605-3623), applied as OAM row copies when rendering restarts
    (3069-3131).
- **Board check (user, 2026-10-04):** with side track F merged in, the
  board tests worked as intended; AccuracyCoin run-all on the board:
  140/144, the spec's table (hardware = model).

### D.6 the last four AccuracyCoin tests (2026-10-08, resumed by the user)

- **BG Serial In** passes (141/144): two rendering delays, as TriCNES.
  The background fetches, their reloads into the shifters, the scroll
  updates (incX, incY, the copies) and dot 0's pattern address follow
  PPUMASK 3 dots late (`fetchDelay`); the shifters, sprite evaluation and
  fetches, the pixels and the secondary OAM resets stay at 2
  (`renderDelay`). Sprite evaluation at 3 broke Frozen OAM2 Increment, so
  it stays at 2. The shifters' serial inputs: 0 into the low plane, 1
  into the high plane, the attribute latch into the attribute shifters
  (their low byte holds that latch's copies, so they shift in their own
  bit 0). With rendering off for the reload but on for the shift, the 1s
  draw. Spec and Clash (`pRenHist` 3 bits), nes-lockstep green (quick
  tier: 61 tests, 475 MiB in use), every Blargg test and NES golden
  unchanged.
- **Hybrid Addresses** passes (142/144): the second $2006 write reaches v
  4 dots later (`v2006Delay`; TriCNES 4, 5 in one alignment; the bus
  shows the new v then, outside rendering). Nametable and attribute
  fetches keep their address (`sFetchA`, its low byte the octal latch);
  on the read dot the address's high byte comes from the current v, and
  when that differs (v changed in between) the "hybrid" address is read
  again. Spec and Clash (`pVPend`, `pVPendVal`, `pFetchA`), nes-lockstep
  green, Blargg tests and NES goldens unchanged.
- **ALE + Read** passes (143/144): when the $2007 read machine reads on a
  pattern fetch's address dot (ALE and read together), the octal latch
  keeps the bus's last byte, the last fetch's data, and that becomes the
  pattern address's low byte (boing2k7's effect). Spec (`bgPattern`) and
  Clash (`aleRead`), nes-lockstep green, Blargg tests and NES goldens
  unchanged.
- **OAM Corruption** passes: **144/144** on the spec. Rendering turning
  off (the 2-dot pipeline flag) on a rendering line seeds a corruption
  with secondary OAM's address register (rounded up to a multiple of 4 on
  dots 65 to 256); the pre-render line's clear phase now steps that
  register too (without writing). If rendering comes back on during
  VBlank, the first rendering dot on a rendering line copies OAM row 0
  (bytes 0 to 7) over row seed and secondary OAM byte 0 over byte seed.
  If it comes back on during a rendering line nothing is copied: Blargg's
  sprite_overflow_tests 5.Emulator (3) does that with a $2002 poll's
  jitter and passes on hardware (TriCNES's unfinished
  "RenderingEnabledOutOfVBlank" flag is the same case). Clash: OAM is 8
  block RAMs of 32 bytes, one per byte of a row (`oamRam`), so the row
  copy is one write (`poOamRow`, `koOamRow`); row 0 is shadowed in
  registers (`pRow0`). nes-lockstep green (quick tier), every Blargg test
  and NES golden unchanged.

### D.7 cartridges in DDR: Kirby's Adventure (2026-10-08, approved by the user)

The user's copy is Kirby's Adventure (E), MMC3 with 512 KiB PRG + 256
KiB CHR (PAL; our NES is NTSC, so its music runs faster). It runs on
the spec with the limits raised (intro, attract demo with the status bar
split). What changed:
- **Limits** (`checkSupported`, `headerCfg`, `nesCheck`): PRG 512 KiB
  (MMC1 256 KiB), CHR ROM 256 KiB; PRG addresses 19 bits, CHR 18
  (`Nes.Mapper`, `MemReq`, `VramAddr`, `LoadWrite`).
- **The core waits for PRG ROM** (`Nes.Core`): a read's data clock waits
  for `kiRdOk`, the whole machine with it, while the dots pile up in the
  credit (now up to 63); the emulation is the same cycle for cycle
  whatever the latency. `prgDdr`: the read through `Platform.Mem.Ddr`'s
  `ddrReadPort` (unchanged, k = 5, prefetch, the cache emptied while
  held), request and answer registered, one read out at a time (a core
  reset while one is out drops its answer).
- **Hardware**: CHR block RAM 256 KiB; both tops with HP0
  (`boards/*/nes_zynq.v` as `gb_zynq.v`, `ps7_hp.tcl`); `nes-image` makes
  no PRG image any more; layer 4 puts `ROM=` in the harness's DDR
  (`--ddr-file`), and the link-load chains put nestest there.
- **Loading**: see "ROM loading" (the file in DDR, header and CHR over
  the link; side track I agreed to the NES-only path in its menu).
- **Tests**: the core harness gives about one PRG read in 8 a random wait
  of 1 to 31 clocks (a poison byte meanwhile), so every lockstep run
  checks the wait; Holy Mapperel's M4_P256K_C256K (MMC3, 256 KiB CHR) on
  the spec and in the core lockstep; the user's cartridges in
  `roms/nes/games/local/` in the core lockstep when present (Kirby);
  `nesCheck`, `nesLinkParts` and `sendFromDdr` in the PS app's tests;
  `test_link.py`'s NES load into DDR. nes-spec, nes-lockstep (quick),
  `make test-zig`, `make ci-nes` and `make ci-arty-nes` green.

### D.8 MMC5 (2026-10-08, the user: "try to add MMC5 support")

Branch `side-nes-mmc5` (from `side-nes-accuracy` once its Kirby work was
ready to merge). Sources: the nesdev wiki "MMC5" page and its talk page,
loopy's hardware results for mmc5test_v2, Nestopia's cartridge database
for the boards' sizes.
- **What's modelled** (`Nes.Spec.Mapper`'s header comment): PRG modes 0
  to 3 over ROM and a 64 KiB PRG RAM superset ($5113-$5117, write
  protection), CHR modes with the two register sets (8x16 sprites with
  rendering on: sprites from $5120-$5127, background from $5128-$512B,
  $2007 from the set written last), ExRAM's four modes, nametables
  ($5105: CIRAM, ExRAM, fill), extended attributes, the scanline IRQ (the
  third read in a row of one nametable address; in-frame, $5203/$5204,
  ended by 3 CPU cycles without a fetch, by rendering off and by the NMI
  vector's read), the multiplier. **Not modelled:** the vertical split
  (Uchuu Keibitai SDF's intro, Bandit Kings' ending), expansion audio (no
  NES cartridge carries it), $5130 (CHR is at most 256 KiB: Metal Slader
  Glory's 512 KiB doesn't fit).
- **How:** the PPU says what each rendering fetch is (`FetchKind` in the
  spec, `Fetch`/`poFetch` in Clash, from the dot); the cartridge sees the
  fully decoded $2000/$2001 writes and the vector reads. Clash: `M5` in
  `MapState`; `prgTarget`, `chrAddrK`, `ntTarget` (CIRAM, ExRAM, or a
  constant byte: `VConst`); ExRAM twice in block RAM (one copy for the
  CPU and nametable reads, one for extended attributes' latch, read with
  the nametable fetch, its byte taken the next clock: `koExLatch`,
  `kiExLatch`); PRG RAM 64 KiB for every board.
- **Tests:** the MMC5 test ROMs (`roms/nes/mmc5/`: Quietust's ExRAM test,
  Drag's scanline test, AWJ's CHR bank test) as spec screen goldens,
  checked by eye (mmc5test_v2 also with Select, extended attributes, and
  Up, fill mode, pressed at frame 30); our `mmc5.s` (Blargg's protocol:
  PRG modes, RAM and protection, multiplier, ExRAM, CHR banks,
  nametables, scanline IRQ) on the spec and in the core lockstep; the
  test ROMs in the core lockstep, and their pictures core = spec pixel
  for pixel (full tier: 5 cases of about 6 minutes).
- **Loading:** `nesCheck` (menu and `make load`) accepts mapper 5 with up
  to 64 KiB of PRG RAM; no battery saves for NES cartridges yet.
- **Clash build time:** the 64 KiB PRG RAM as `blockRam (replicate ...)`
  made Clash normalise for 20 minutes (it builds the initial value as a
  literal); `blockRam1 NoClearOnReset` for every zero-filled RAM (as the
  Game Boy's) brought `make hdl` back to 4 minutes. `OPAQUE` (not
  `NOINLINE`, with GHC 9.10) keeps the mapper's functions and
  `coreStep`'s pieces separate; it changed little here.
- **Gates (2026-10-09):** nes-spec 143, nes-lockstep 67 (quick), nes-golden
  11, `make test-zig`, `make lint`, `make ci-arty-nes` and `make ci-nes`
  green; `make bit SYSTEM=nes` (Arty) WNS +8.888 ns, 30 141 LUTs (56.7 %),
  BRAM 120/140.

### D.9 AccuracyCoin d103cbd: 146 tests (2026-10-22, side Q for side D, the user)

- **Re-pinned** to commit d103cbd (`tools/fetch-roms.sh`; sha256
  cafb7cf6…), in its own folder, `roms/nes/accuracycoin-d103cbd/`: the
  worktrees share one `roms/`, so a branch still on 673ef55 keeps its
  `roms/nes/accuracycoin/` copy (the SD card's test ROMs take the new
  folder). The pages
  are the same 22; two tests are new ("DMA Landing on Write", page
  APU Registers and DMA tests; "DMC Reload Timing", page APU Tests), and
  "Misaligned OAM behavior" gained cases (upstream 087050e). Names may
  hold word codes now (bytes $80 to $FE: " indirect", " zeropage",
  " absolute", " immediate", "Unofficial Instructions: "),
  `Nes.AccuracyCoin` expands them. Score at the new pin before the fixes:
  144/146; after: **146/146** (spec, `test/golden/accuracycoin-spec.txt`).
- **DMC Reload Timing:** a $4010 write in the CPU cycle the DMC timer
  reloads sets the reload value (hardware: the same APU cycle uses the new
  rate). The models reload at the cycle's start, before the write, so the
  write reloads the timer again with the new rate when the reload was in
  its cycle (`dReloadAt` in the specs, `dmReload` in `Nes.Apu`).
- **Misaligned OAM behavior:** sprite evaluation realigns OAMADDR (AND
  $FC) after a Y byte out of range (+4, test 2) and after an X byte out of
  range (+1, tests 4 to 6: the X byte goes through the same range check
  as Y); an X byte in range still adds 1. Spec, fast copy and core.
- **Tests:** layer 1 regressions on the core: `Nes.SpecApu` (a $4010 write
  in the reload cycle and one cycle later) and `Nes.SpecEval` (one
  scanline's evaluation with test 2's and test 4's OAM: overflow set);
  both fail on the old core. nes-spec 192, nes-specfast 14, nes-lockstep
  74 (quick) green; the core's 146 results equal the spec's
  (`NES_CORE_ROMS=all`); `make bit SYSTEM=nes` (Arty) WNS +7.995 ns.
- **Board-checked by the user** (2026-10-23, Arty): 146/146.
