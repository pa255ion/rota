# Sega Game Gear (NTSC)

Target: a **Sega Game Gear** (Z80A, the 315-5378 VDP, an SN76489-style
PSG with stereo panning, 8 KiB RAM, Sega-mapper cartridges), milestone 5.
Planned 2026-10-07 (Claude Code, at the user's request: the Game Gear
matters to a concurrent project), before any code: everything here is a
plan, and the parts marked *to verify* must be checked against the
references below before code depends on them. When the status log at the
end disagrees with the plan, the log wins.

## Why the Game Gear, and what is new

- **The first Z80.** It goes in a new shared package, `cpus/z80`, next to
  `cpus/mos6502` and built the same way (a cycle-level `Z80.Spec` the
  caller gives a bus, a Clash `Z80.Core` in lockstep with it), so the
  Master System, ColecoVision, MSX or a ZX Spectrum can reuse it later.
  The Game Boy's SM83 (`systems/gb/`) is a different CPU and stays where
  it is.
- **The display is 160×144, the Game Boy's size**, so the Game Boy's
  display path (three frame buffers, ×3 into 640×480) fits as it is.
- **The VDP is a Master System VDP** (Mode 4: tiles, 64 sprites, line
  interrupts, scrolling) with a 12-bit palette, showing a 160×144 window
  of its 256×192 picture. Games change scroll and palette mid-frame
  through line interrupts, so the VDP works at dot level from the start.
- **The PSG is the first non-Nintendo sound chip** (three square waves
  and a noise channel), stereo through the Game Gear's panning port.
- **Cartridges up to 1 MiB** (*to verify* the largest), so the ROM lives
  in DDR (the media region, as the Game Boy's since side track I phase
  2). The Z80 has a WAIT input, which is how the core waits for a DDR
  miss.
- **No BIOS is needed** (SMS Power, "BIOSes"): only some Game Gears
  have a boot ROM; it shows a logo and checks for "TMR SEGA" at 7FF0h,
  3FF0h or 1FF0h (no checksum, no region check), and VA0 units have
  none. Sega's ROMs are never fetched or committed.

## Machine summary (NTSC; checked 2026-10-07 against the references where marked ✓, the rest *to verify*)

| Item | Value |
|---|---|
| CPU | Zilog Z80A, 3.579545 MHz ✓ (GG Hardware Reference Manual; the NTSC colour carrier, master clock 53.693175 MHz / 15). Inside a Sega ASIC: whether its SCF/CCF X/Y behaviour (Q) is the NMOS Z80's that SingleStepTests models is *to verify* (ZEXALL-SMS's README discusses the variants) |
| Frame | 262 lines ✓ × 228 CPU cycles (342 VDP dots ✓ at 1.5 dots per CPU cycle) = 59736 cycles, 59.92 Hz (MacDonald's VDP notes) |
| Memory map | 0000-BFFF cartridge through the Sega mapper (three 16 KiB slots, the first 1 KiB fixed); C000-DFFF 8 KiB RAM, mirrored at E000-FFFF; mapper registers FFFC-FFFF (written through to RAM) |
| Sega mapper | ✓ (SMS Power, "Mappers") FFFC: bit 3 cartridge RAM at 8000-BFFF, bit 2 its bank; FFFD, FFFE, FFFF: the 16 KiB banks of slots 0, 1, 2, the first 1 KiB of slot 0 fixed. Cartridge RAM (battery-backed in some games) 8 or 32 KiB. Codemasters: registers at 0000h, 4000h, 8000h, no fixed 1 KiB (modelled, "Codemasters mapper" below) |
| I/O ports (partial decode) | ✓ (MacDonald's SMS/GG notes; GG Hardware Reference Manual) 00 (read): D7 Start (0 = pressed), D6 0 = Japan, D5 0 = NTSC; 01-05: EXT port data and direction (with its NMI enable), serial TX, RX and mode; 06: PSG stereo; reset values of 00-06: C0 7F FF 00 FF 00 FF; 07-3F: writes to even ports go to the memory control (3E), odd to I/O control (3F); 40-7F: V counter (even, read) and H counter (odd, read), PSG (write); 80-BF: VDP data (even) and control (odd); C0-FF: pad (C0/DC) and pad B / misc (C1/DD) |
| VDP | Mode 4, VRAM 16 KiB, CRAM 32 × 12-bit ✓ (64 byte addresses; a write to an even address goes to a latch, the odd one commits both bytes; little-endian `----BBBBGGGGRRRR`: "0FH, 00H" is bright red in the manual), 64 sprites (8 per line, 8×8 or 8×16, collision and overflow flags), line counter interrupt (register 10), frame interrupt, V counter with its NTSC jump (DA→D5), H counter latched; LCD window 160×144 at column 48, line 24 of the 256×192 picture ✓ (SMS Power, "GG VDP") |
| PSG | ✓ (SMS Power, "SN76489") SN76489 variant (Sega's: 16-bit noise LFSR, taps on bits 0 and 3, 0009h), clock 3.579545 MHz / 16, three tone channels and noise, 4-bit attenuation in 2 dB steps. Port 06, D7..D0 = noise L, tone 3 L, 2 L, 1 L, noise R, tone 3 R, 2 R, 1 R, **1 = on**, all on at reset (the GG manual; MacDonald's notes say "1 = disabled", which contradicts their own "FF enables all"). On the real unit stereo reaches the headphones only; we send it to HDMI |
| Input | D-pad, 1, 2 (port DC, active low), Start (port 00 bit 7) |
| Interrupts | the VDP's INT on the Z80's /INT, IM 1 in practice (RST 38h); NMI unused on the Game Gear (the Master System's Pause) |

## References (URLs checked 2026-10-07)

- Z80: Zilog's Z80 CPU User Manual, UM0080
  (https://www.zilog.com/docs/z80/um0080.pdf); "The Undocumented Z80
  Documented" (Sean Young, http://www.z80.info/zip/z80-documented.pdf);
  the X/Y flags (http://www.z80.info/z80sflag.htm); the MEMPTR notes
  (boo_boo and Vladimir Kladov; no canonical URL found, mirrors via
  https://msx.org/node/7902); "Z80: the last secrets" (FOSDEM 2022) for
  Q and the block instructions.
- The machine: Sega's Game Gear Hardware Reference Manual
  (https://segaretro.org/images/1/16/Sega_Game_Gear_Hardware_Reference_Manual.pdf;
  wins over the others where they disagree); Charles MacDonald's SMS/GG
  hardware notes
  (https://www.smspower.org/uploads/Development/smstech-20021112.txt)
  and VDP notes
  (https://www.smspower.org/uploads/Development/msvdp-20021112.txt);
  SMS Power's pages "GG VDP", "Palette", "Mappers", "SN76489", "BIOSes"
  (https://www.smspower.org/Development/).
- Emulators as a cross-check for behaviour (values and rules, not code),
  as the Linux driver was for the ADV7511.

All PDFs go in the git-ignored `local/reference/`, as the board manuals.

## Accuracy target

- **Z80: T-state exact.** Every instruction's machine cycles in order
  (opcode fetches with their refresh, reads, writes, I/O and internal
  cycles) with their T-state lengths, interrupt acceptance at the end of
  an instruction, EI's one-instruction delay, HALT, IM 0/1/2 and NMI,
  all undocumented opcodes, the X/Y flags, MEMPTR and Q. Tom Harte's
  SingleStepTests check final state and bus cycles per opcode (5.1).
- **VDP at dot level:** register, CRAM and scroll writes take effect at
  the dot they land on (within the VDP's documented latching: the
  horizontal scroll is latched per line, *to verify*), the line counter
  and the V/H counters exact, so line-interrupt effects (status bars,
  raster scrolling) look right.
- **PSG** at its clock / 16 rate, the LFSR bit-exact.
- **Out of scope:** the full Master System (the model has the Game
  Gear's Master System mode as far as running Master System ROMs needs;
  a Master System as its own bitstream is a natural follow-up, as the
  Game Boy Color is from the DMG's); the EXT link cable, the TV tuner,
  the BIOS, PAL. The Codemasters mapper was added on 2026-10-07 (the
  user).

## Test ROMs and tools (fetched by `tools/fetch-roms.sh` into `roms/z80/` and `roms/gg/`, never committed)

| What | Use | Licence |
|---|---|---|
| SingleStepTests `z80` (Tom Harte, github.com/SingleStepTests/z80) at commit ebe1875: 1604 files (unprefixed, CB, ED 40-7F and the block opcodes, DD, FD, DDCB, FDCB), 1000 cases each, ~1.3 GB; SHA-256s in `tools/roms-z80.sha256`. Generated from a corrected translation of ares' Z80 ("probably not perfect", its README), NMOS | 5.1: final state incl. MEMPTR, Q, EI and the LD A,I/R flag; the pins of every T-state | MIT |
| ZEXDOC / ZEXALL for the Master System (github.com/maxim-zhao/zexall-sms, v0.21) | 5.1/5.5 (FULL tier: the longest test runs over an hour even on hardware); output on the VDP's text screen or the SDSC debug console. Whether it runs in Game Gear mode is *to verify* | GPL-2.0 (Frank Cringle's original, ported) |
| SMS VDP Test 1.31 (FluBBa, 2009; smspower.org/Homebrew/SMSVDPTest-SMS; `roms/gg/vdptest/VDPTEST.sms` from the release zip) | 5.3: the VDP timing gate: data port, counters, interrupts, flags, sprites, the X-scroll latch (55 results + a column, `gg-spec`) | **no formal licence; fetch, don't commit** (the user, 2026-10-07; as Blargg's ROMs and nestest). Its source is in the zip: read for what each test expects, no code copied into our ROMs. A Master System ROM: runs in SMS mode |
| SMS Test Suite v0.38 (sverx, github.com/sverx/SMSTestSuite release; `roms/gg/smstestsuite/SMSTestSuite.sms`) | 5.3/5.4: video patterns, PSG, pad, system info; a visual check (it identifies the model as "Game Gear (Single ASIC)", region USA/BRA, 60 Hz) | **no formal licence; fetch, don't commit** (the user, 2026-10-07) |
| Our own Z80 regression ROMs (`systems/gg/test/asm/`, `make gg-asm`, WLA-DX 10.6 from nixpkgs) | rule 5, mapper, ports, a tone, interrupt timing | ours |
| **Waternet** (joyrider3774, github.com/joyrider3774/waternet; native Game Gear build) | the exit game (**confirmed by the user 2026-10-07**; fetched and pinned, `roms/gg/games/waternet.gg`) | MIT; tiles and music by the author (README); only the cartridge-label art is third party (not used). Alternative: *tunnel* (liamhays, GPL-3.0) |

## Priority (the user, 2026-10-07)

A concurrent project needs **a working, accurate Game Gear simulator to
test a ROM**. So the reference model comes first as a usable tool
(`gg-run`, below), and accuracy is what counts; the hardware steps (5.5
onwards) follow. The exit game is **Waternet** (confirmed by the user).

## Simulator: `gg-run`

The reference model (`Gg.Spec`: `Z80.Spec`, the VDP, the PSG, the
mapper) as a command-line simulator, inside `nix develop`:

```sh
cabal run gg-run -- ROM.gg --frames 600 --input buttons.txt \
    --png-at 300 shot.png --wav sound.wav --console --trace
```

| Option | |
|---|---|
| `--frames N` | frames to run (default 300; 59.92 per second) |
| `--png DIR` / `--png-at K FILE` | every frame as `DIR/frame-NNNNN.png`, or frame K only (160 × 144, the LCD window) |
| `--wav FILE` | the PSG, 48 kHz 16-bit stereo (port 06h's panning applied) |
| `--hashes FILE` | a 64-bit hash per frame (`Gg.Screen.frameHash`), to compare runs |
| `--input FILE` | buttons per frame: lines `FRAME BUTTONS`, BUTTONS `-` or letters `U D L R 1 2 S` (S = Start), held until the next line (`systems/gg/test/input/waternet-start.txt` is an example); since M6 also `u d l r 3 4`, the Master System's pad 2 |
| `--sram FILE` | cartridge RAM: loaded if the file exists, written at the end |
| `--console` | prints what the ROM writes to port FDh (the SDSC debug console, a homebrew convention; the hardware ignores the write) |
| `--peek ADDR` | prints a byte of memory (hex address) at the end |
| `--trace` | one line per frame on stderr: frame, hash, PC |
| `--mapper sega` / `codemasters` | override the detected mapper ("Codemasters mapper" below; `--trace` prints the mode and mapper in use) |
| `--sms` / `--gg` / `--gg-sms` | the Master System II (since M6 the default for a `.sms` file; docs/systems/mastersystem.md), Game Gear mode, or a Game Gear in its Master System mode (SMS CRAM, the whole 256 × 192 picture, Start = Pause on NMI; before M6 this was `--sms`) |
| `--bios FILE` | the Master System II's BIOS image (M6; the user's own dump, never committed) |
| `--text-at K` / `--nametable` | the name table as text at frame K (for test ROMs with an ASCII font at tile 0), or as tile numbers at the end |

It runs at about real time (Waternet: 300 frames in ~5 s). Since M6 its code is the library module `Gg.RunMain` (shared with
the sms package's `sms-run`, which adds the Master System's FM unit). The Master
System mode is there as far as running Master System ROMs needs (the
test ROMs below): SMS CRAM, the whole picture, ports 00h-06h, mono PSG,
Pause; the full Master System (its own BIOS, region variants, the
SMS 1's VDP quirks) stays out of scope. Not modelled yet: the EXT port,
the other mappers (Korean, MSX-style, 4-Pak: none known on Game Gear
cartridges), the items under "VDP timing" marked uncertain.

## VDP timing (calibrated 2026-10-07)

Sources: Sega's GG Hardware Reference Manual [GGM], MacDonald's VDP notes
[CM], cross-checked with MAME's 315_5124 and Genesis Plus GX (values and
rules only) and SMS Power forum threads; then fixed to the dot by
**FluBBa's SMS VDP Test 1.31**, which now passes in full (all 55 results
"Ok.", and the X-scroll latch column straight; `cabal test gg-spec`).
Its source was read for what each test measures (no code copied).

The model (`Gg.Spec.Vdp`, `Gg.Spec`), in dots of a line from the V
counter's change (342 dots, 1.5 per T-state):

| What | Where | Evidence |
|---|---|---|
| The CPU's I/O access reaches the VDP | T3 of its 4 T-state cycle (`ioSampleT`) | Z80 timing; the test's latch-to-read distances |
| /INT sampled | in an instruction's last T-state (`intSampleT` 1 before its end) | Z80 (UM0080); Frame/Line IRQ HCount |
| VDP dot phase | dot = 1.5 × T + 1 (`dotPhase`) | "HCounter correct": the doubled values (02 02, 05 05, ...) |
| H counter | 9-bit dot counter 1E8h at the line start, 000h-127h then 1D2h-1FFh; port 7Fh shows the top 8 bits **as latched** when TH goes low to high | "HCounter correct", "HCounter keeps value", "HC change on TH 0->1" |
| V counter change, line counter, frame flag (line C1h), sprite evaluation and overflow flag | dot 0 (H = F4h) | "VCounter chg time", "VINT flag HCount", "Spr ovr correct HC" |
| Line interrupt raised | dot 1 (`lineIntDot`): a dot after the frame interrupt, as FluBBa measured (HClock −17 against −18) | "Line IRQ HCount" |
| H scroll latched for the next line | 2 dots before it starts (`hScrollEarly`), in the 1 T window the test allows | the X-scroll latch column |
| First active pixel | dot 35 (`pixelDot`): collision at pixel 131 is flagged at H 47h | "Spr col correct HC" |
| Sprites on the top border (lines 235-261) | evaluated, collide, set no overflow, not drawn (`topBorder`) | "Offscreen Y, col", "Offscreen Y, no ovr" |
| Sprite fetches (since 2026-10-07, for the Clash VDP) | the Y of sprite i read at dot 256 + i of the line before (stops at D0h or the ninth); at the line's start the overflow flag and the slots; slot j's X/tile at dot 2j, pattern row at 2j + 1; each pixel from the first opaque slot, two or more = collision | the sources say "evaluation during line N - 1, X and patterns during line N" (MacDonald, quoted in MAME); the dots are ours, *to verify*. Only mid-line SAT/pattern writes can tell; FluBBa's test and the goldens are unchanged |

Also (from the sources, and passing the test's other results): V counter
00h-DAh, D5h-FFh; the line counter decremented on lines 0 to 192 and
reloaded on 193 to 261 (R10 = 0Bh interrupts on line 0Bh); /INT = (frame
flag and R1 bit 5) or (line interrupt and R0 bit 4), level sensitive; a
status read clears bits 7-5, the line interrupt and the control port's
byte flag, bits 4-0 read 1; V scroll latched at the start of line 0, R0
bit 7 stops it for screen columns 192 and up, rows wrap at 224; R0 bit 6
holds the H scroll at 0 on lines 0 to 15; sprites: up to eight per line,
Y = D0h ends the list, shown from line Y + 1, zoom doubles all eight on
the GG, 8×16 uses tiles n AND FEh / n OR 1, R0 bit 3 moves sprites 8 left,
R6 bit 2 adds 256; collision anywhere on the 256-pixel line while the
display is on; the data port's read buffer, pre-read, the first control
byte moving the address at once; CRAM's even-byte latch [CM, GPGX; MAME
uses the read buffer].

**GG mode against SMS mode.** FluBBa measured these on a Master System
(the ReadMe gives Mega Drive values beside them), and the test is a
Master System ROM, so the results above settle **SMS mode**. The model
uses the same timing in **GG mode**: the Game Gear's VDP is the SMS 2's
with the GG's CRAM and window, and nothing measured on a Game Gear says
otherwise. Run in GG mode (`gg-run --gg`), the test gives the same
pass/fail pattern through the cropped window, which only shows that the
model is consistent, not that the hardware is. *To verify on a real Game
Gear:* MAME puts some GG-mode events 1 to 2 H counts later than on the SMS
(the V counter change, the frame interrupt, the scroll latch), unmeasured.
What differs between the modes in the model: CRAM (12-bit with a latch,
or 6-bit direct), the picture (the 160 × 144 window, or 256 × 192), the
ports 00h-06h, the PSG's stereo port, and Start (port 00h, or Pause on
NMI).

**Still uncertain or not modelled:** the first top-border line where
sprites are evaluated (235 here; MAME and Genesis Plus GX use the last 13
to 16 lines); overflow only while the frame flag is clear (GGM and MAME;
the test doesn't cover it); vertical zoom rounding; VRAM access slots
(lost writes on the SMS 2, not seen on the GG); the 224/240-line modes;
legacy TMS modes (backdrop here); the name table's base mid-line (FluBBa
notes it changes during the line; his test for it is disabled).

## Codemasters mapper (2026-10-07)

Sources: SMS Power, "Mappers" and "Codemasters header"; cross-checked
with MAME (`sega8_codemasters_device`), Genesis Plus GX (`sms_cart.c`)
and MEKA (values and rules only). Game Gear titles with it (GPGX's
list): Cosmic Spacehead, Dropzone, Ernie Els Golf, Man Overboard!, Micro
Machines 1 and 2, Pete Sampras Tennis, CJ Elephant Fugitive (the Dizzy
games on GG cartridges are Master System games: SMS mode). `Gg.Spec`:

| Rule | Model | Sources |
|---|---|---|
| Bank registers | writes to exactly 0000h, 4000h, 8000h set slots 0, 1, 2 (8 bits, modulo the ROM's banks); no fixed first 1 KiB | MAME, GPGX, MEKA decode the exact addresses; SMS Power's wording suggests the whole slot (*uncertain*; games only write the exact addresses) |
| Power-on banks | 0, 1, 0 | SMS Power, MAME, GPGX (MEKA: 0, 1, 2; games set their banks at boot) |
| FFFCh-FFFFh | RAM only, no registers | all three |
| Cartridge RAM | a 4000h write with bit 7 set maps 8 KiB at A000h-BFFFh (8000h-9FFFh stays ROM), slot 1's bank unchanged; bit 7 clear maps the ROM back; no battery | SMS Power (8 KiB, Ernie Els Golf only); slot 1 unchanged as MAME and MEKA (GPGX also sets it: *uncertain*; MAME's 64 KiB and bank bits are its own guess) |
| Detection (`detectMapper`, `gg-run --mapper` overrides) | Codemasters if the ROM has a Codemasters header (7FE6h checksum nonzero, 7FE8h its complement, 7FEAh-7FEFh zero) or Codemasters-style bank writes (more `LD (8000h),A` than `LD (FFFFh),A` + 2 in the first 32 KiB, or some and none) | emulators use a CRC list (GPGX) or that count (MAME, MEKA); the header check is ours (SMS Power's header layout; the checksum itself isn't recomputed). *To verify* on real Codemasters ROMs: none is fetched (none is freely licensed) |

Gate: `codemasters.s` (14 checks: power-on banks, each register, FFFFh
inert, wrap, the RAM on and off with slot 1 kept, slot 0 switched from
RAM with no fixed 1 KiB; it carries a Codemasters header and no Sega
one) and the detection unit tests (`cabal test gg-spec`); planted
bugs (GPGX's slot-1 variant, detection off) fail it.

## ROM in DDR (step 5.9, the user's option (a), 2026-10-07)

The cartridge ROM (up to 1 MiB) is in DDR at 0x02000000 (the media
region), read over HP0 through `Platform.Mem.Ddr`'s port (a line cache
with sequential prefetch). A miss takes about 15 pixel clocks and up to
35 (docs/ARCHITECTURE.md, "DDR path"), but a Game Gear memory read leaves
only about 21 from address to data, so no cache can promise every byte
in time, and the spec has no wait states. **The timing stays exact
anyway** (`Gg.RomPort`):

- The port asks for the pending ROM read's address as soon as the core
  shows it (`Gg.Core.ggRomWant`) and keeps the answer with its address.
- When a T-state boundary comes (`ggTBoundary`: where a machine cycle can
  end and use its data) and the byte isn't there, **the whole machine
  waits**: the core gets no active clock. CPU, VDP and PSG stop together,
  so nothing inside the Game Gear can tell (the lockstep tests show an
  inactive clock changes nothing).
- Real time is made up afterwards: credits accrue at the core's rate
  (12 active clocks per two T-states, about 85 % of the 25.175 MHz
  clocks) up to a cap of 4096, and the core runs on every clock while it
  has credit. A stall of a few dozen clocks is caught up within a few
  hundred; only a long run of misses would slow the machine down (the
  cap), never change what it computes. Video (triple buffer) and audio
  (held level) don't notice the jitter.

Checked: `gg-lockstep` runs the machine with its ROM behind `Gg.RomPort`
and a fake DDR of random latency (hits 1 clock, misses 6 to 35) against
the spec, instruction by instruction (Waternet, 10 frames: 37 misses, 508
clocks frozen; identical); with the freeze switched off it fails at the
first instruction. Layer 4: `raster.s` (a line interrupt on every line
rewrites colour 0 mid-line, so the picture shows the exact interrupt
timing; its work reads 48 KiB of pseudo-random ROM data across banks) is
identical to the spec's picture on the Arty at the harness's DDR latency
and at up to 60 clocks (1805 bursts), and on the ZedBoard.

**Bitstreams** (`make bit SYSTEM=gg [BOARD=zedboard]`, the wrappers
`gg_zynq.v` with the PS7 and HP0): the Arty 19,774 LUTs (37 %), 3,497
flip-flops, 51 of 140 block RAM tiles, 8 DSPs, WNS +5.10 ns, WHS +0.11 ns;
the ZedBoard 17,943 LUTs (34 %), 3,430 flip-flops, 51 block RAM tiles,
WNS +5.18 ns; no critical warnings on either. Not yet on a board: the PS
can't load a Game Gear ROM until the PS app knows the system (step 5.9,
after side track I).

## Input and sound on the board (steps 5.9 and 5.10, 2026-10-07)

**Keys** (the user, 2026-10-07: "the USB input like the other cores,
the same layout as the GB"). The PS app knows the Game Gear (ID 7) as a
key-map system, `[gg]` in `platform/ps/keymap.zig`, with the Game Boy's
bits, so the PL decodes it as the Game Boy's: 0 right, 1 left, 2 up,
3 down, 4 button 2 (the Game Boy's A), 5 button 1 (B), 7 Start; no
Select. Default keys as the Game Boy's: W A S D, **J = button 1, K =
button 2, L = Start**; Cross = 1, Circle = 2, Options = Start, the D-pad
and left stick. The USB keyboard (the board's PS) and `make keys
SYSTEM=gg` (the PC) both work; `make keymap SYSTEM=gg` remaps. The tops OR
these with the board's own buttons (`Gg.Machine.psKeys`) and report ID 7
and their buttons on PS_IN. **The SD menu** (`/gg`, the user, 2026-10-07: "merge and proceed",
once side track I's PS work was in `main`). The PS app's row (ID 7,
`.gg` files up to 1 MiB) puts the ROM into DDR at 0x02000000, as the Game
Boy's, and then, still holding the machine, writes the **cartridge word**
(bit 0 the mapper, 1 = Codemasters; bits 15-8 the 16 KiB banks - 1, a
power of two) to address 0 of the Game Gear's link map: the ROM itself
doesn't reliably say its size, and the Codemasters signature sits at
7FE0h, past the first block, so the PS works the word out from the whole
ROM in DDR (`platform/ps/gg_cart.zig`, the spec's `detectMapper` rule;
menu.zig's new `ddr_info`). `make load SYSTEM=gg ROM=…` does the same
from the PC. In the PL (`Gg.Machine`): while held, no credit and no DDR
requests (the port's cache invalidated); once the port is idle the core
and the ROM port are reset, and they start on the new word when the hold
falls; video, audio and the DDR port run on. Before any PS write the word
is `build/gg/cart.bin`'s, so `make gg-prog` still works. The menu's text
overlay is drawn over the picture, and Esc or Up+Down on the board opens
it (the machine paused). `make sd` puts `roms/gg/games` and
`build/gg/asm` into `/gg` and `gg.bit` into `/systems`. Layer 4
(`ci-arty-gg`, `ci-gg`): a load as the menu does it (`test/ps/reload.ps`:
hold, the word, release; raster.s's picture again, PS_IN = system 7) and
the menu's overlay (the Game Boy's script and golden).

**Sound** (`Gg.Audio`, heard on the Arty 2026-10-07: "the sound seems
awful"). The PSG's edges and noise reach about 110 kHz; HDMI's audio
packets and S/PDIF take one point every 48 kHz, so without a filter
everything above 24 kHz folded back as noise. Now, per side, the analogue
part as the NES's (`Nes.Audio`): two one-pole low-passes at 15.6 kHz and
a high-pass at 15 Hz, at the pixel clock. `gg-lockstep` checks a 112 kHz
square comes out under 5 % and a 1 kHz one keeps its level; the layer 4
tone checks still find 440.4 Hz.

## Plan (PLAN.md, "Milestone 5")

The steps are in PLAN.md. Notes per step that don't fit the table:

- **5.1, the Z80 spec.** One `step` is an instruction (its prefixes
  included) or an interrupt acknowledge. The bus record (`Z80.Spec.Bus`)
  has one call per machine cycle kind (opcode fetch, memory read and
  write, I/O read and write, internal T-states, interrupt acknowledge),
  each with its T-state length; the machine counts T-states from them.
  The SingleStepTests harness collapses each test's per-T-state pin list
  into the same machine cycles and compares them, plus the total
  T-states. ZEXDOC/ZEXALL run under a CP/M stub (BDOS at 0005h: print
  functions 2 and 9).
- **5.5, the Clash Z80 (`Z80.Core`).** One transition per machine cycle
  (`cpuStep`), the pending request a register (kind, address, data,
  length) that never depends on its own cycle's data; the requests are
  exactly the spec's bus calls, so lockstep compares them one for one.
  A T-state wrapper (`z80Tick`) counts each request's T-states and
  stretches fetches, reads, writes, I/O and the acknowledge while WAIT is
  held on their last T-state (a machine holds WAIT until its data is
  there, e.g. a DDR miss). Interrupt lines are looked at when an
  instruction ends, as in the spec. Its ALU and flag rules are written
  again, independently of the spec's. ZEXDOC/ZEXALL are not run on it
  (the Clash types make it ~100 times slower than the spec; the
  SingleStepTests and the lockstep cover the same ground).
- **5.6, the Clash VDP and PSG (`Gg.Vdp`, `Gg.Psg`).** The VDP works in
  four clocks per dot on VRAM as four byte-wide block RAMs (address mod
  4; a name entry or a pattern row is one 32-bit read): sub 0 the CPU's
  port access (VRAM write or read-buffer prefetch) and the previous dot's
  sprite read put away, sub 1 the dot's events and the name read, sub 2
  the pattern read, sub 3 the pixel, the collision and the dot's sprite
  read. That keeps the spec's order (an access before its dot's events,
  everything read as of its own dot), so the two match dot for dot. The
  platform supplies the dot strobe (about 4.7 clocks per dot at
  25.175 MHz; extra clocks idle). Window pixels stream out with their
  coordinates; the frame buffers are the platform's. The PSG is a pure
  function per write and per PSG clock.
- **5.7, the whole machine in Clash (`Gg.Core`).** Twelve active clocks
  per two T-states (the platform's clock enable drops the rest to keep
  the real rate): T-states end at clocks 0 and 6, VDP dots start at 0, 4
  and 8; the VDP runs two dots ahead from power-on (the spec's dot phase).
  A VDP or TH access of an I/O cycle starting at T-state t reaches the VDP
  at the start of dot floor(1.5 (t + 3)) + 1; /INT at an instruction's end
  at T-state t is the VDP's /INT at the start of dot floor(1.5 (t - 1)) + 1
  (a four-dot history); the PSG clocks every 16th T-state, a write lands
  at t + 3 after that clock: exactly the spec's sampling points. ROM,
  RAM, cartridge RAM and VRAM are outside, read with one clock's latency
  (the CPU's address is steady for a whole machine cycle). The ROM port
  has no WAIT yet: DDR (5.9) needs it, and a stall there would change the
  timing against the spec (as the Game Boy's misses do).
- **5.8, display, input and audio (`Gg.Machine`, `Gg.Display`,
  `Gg.ArtyTop`, `Gg.Top`).** Both tops share `Gg.Machine`: the core in
  the 25.175 MHz pixel domain with a fractional clock enable (12 active
  clocks per two T-states, about 85 % of clocks; the core holds its
  memory addresses on the others), the window into three frame buffers
  and ×3 into 640 × 480 at (80, 24) (the Game Boy's layout; the triple
  buffer absorbs 59.92 against 59.94 Hz), the PSG held between its
  clocks. The Arty: HDMI with the PSG in its audio, PWM on the jack. The
  ZedBoard: VGA, the ADV7511 (4:2:2, the HDMI-with-audio init) and
  S/PDIF. The cartridge is built in (`gg-image`: up to 128 KiB in block
  RAM, mirrored to fill it, and its mapper) until DDR (5.9). Buttons:
  Arty BTN0-3 left/down/up/right, SW0 button 1, SW1 Start; ZedBoard the
  five buttons (centre = 1), SW0 Start, SW1 button 2.
- **The PSG's output is bipolar** (since 5.8, spec and core): each channel
  plus or minus its volume, as the Game Gear's coupling capacitors leave
  it (the chip itself puts out 0 or the volume). A digital output (HDMI,
  S/PDIF, `gg-run --wav`) then carries no DC; the zero-crossing tone
  check on S/PDIF needed it.
- **5.2, the machine without video.** `Gg.Spec` around `Z80.Spec`: the
  memory map and mapper, the ports, cartridge RAM, the pad and Start,
  the PSG ports; the VDP as a register file only. Test verdicts come
  from our own ROMs (a result byte in RAM, a fixed address).
- **5.8 onwards, the board.** The ROM in DDR at 0x02000000 (the media
  region), cartridge RAM saved on the card as the Game Boy's (side track
  I's `/saves`), the system ID 7, menu folder `/gg`, keymap section
  `gg`. The Arty top first (the main board), the ZedBoard's beside it.

## Status log

| Date | Step | State |
|---|---|---|
| 2026-10-07 | planning | this file, PLAN.md "Milestone 5", system ID 7 reserved (ARCHITECTURE.md); branch `m5-gamegear` |
| 2026-10-07 | 5.0 (part) | SingleStepTests `z80` fetched and pinned (`tools/fetch-roms.sh`, `tools/roms-z80.sha256`); references, licences and the exit game researched (above) |
| 2026-10-07 | 5.1 **done** | `cpus/z80`: `Z80.Spec` passes every SingleStepTests file, all 1000 cases each (`cabal test z80-spec`, FULL=1), the interrupt unit tests, and ZEXDOC and ZEXALL to the end (`cabal test z80-zex`, FULL=1, about 4.5 min). ZEXDOC/ZEXALL are the CP/M originals (Frank Cringle, GPL), pinned from github.com/anotherlin/z80emu at 1c418fa |
| 2026-10-07 | 5.0 (more) | WLA-DX 10.6 in `flake.nix`; `make gg-asm` assembles `systems/gg/test/asm/*.s` (shared `gg.inc`: 64 KiB, Sega header, bank markers, the verdict at C000h) into `build/gg/asm/*.gg` |
| 2026-10-07 | 5.2 (started) | `systems/gg`: `Gg.Spec` without video timing (memory map, Sega mapper, cartridge RAM, ports and their decode, pad and Start, the VDP's port interface, VRAM, CRAM, V counter; PSG writes collected); `mapper.s` passes (`cabal test gg-spec`), and fails on a deliberately broken bank wrap |
| 2026-10-07 | 5.2 to 5.4 (spec) | The user: the concurrent project needs an accurate simulator first; Waternet confirmed. `Gg.Spec.Vdp` (dot level, timing above), `Gg.Spec.Psg`, `gg-run` (PNG, WAV, input scripts, SRAM, SDSC console, peek, trace). Waternet plays: menus, mode and difficulty select, level 1, moves, music. Regression ROMs `ports`, `vdpint` (frame interrupt on C1h, line interrupts on 0Bh and 17h, the last at C0h with R10 = 0), `sprites` (overflow, collision), `console`; VDP and PSG unit tests (white noise period 57337, as SMS Power gives); the Waternet golden (frames checked by eye). `cabal test gg-spec`: 25/25; planted timing bugs caught |
| 2026-10-07 | 5.3 (VDP timing settled) | The user: fetch FluBBa's SMS VDP Test and sverx's SMS Test Suite (no formal licence; fetch, don't commit). The model gained the GG's Master System mode (to run them; `--sms`). Calibrated on FluBBa's test: I/O at T3, /INT in the last T-state, dot phase 1, the line interrupt a dot after the frame interrupt, the H scroll latched 2 dots early, pixel 0 at dot 35, sprites evaluated on the top border; the test went from 8 errors to **55/55 Ok.** and a straight X-scroll column, now a `gg-spec` gate (planted errors caught). sverx's suite found the region check (port DDh's TH bits; fixed, `ports.s` checks 12-13). Waternet's golden unchanged. GG vs SMS mode: above |
| 2026-10-07 | Codemasters mapper | The user: go ahead. Registers 0000h/4000h/8000h, power-on 0, 1, 0, Ernie Els Golf's 8 KiB RAM at A000h, detection by header or bank writes, `gg-run --mapper`; `codemasters.s` (14 checks) and detection unit tests; `cabal test gg-spec` 30/30. Rules and their disagreements: "Codemasters mapper" above |
| 2026-10-07 | 5.5 (gate passed, quick tier) | `Z80.Core`: the core passes all 1604 SingleStepTests files and 60 random programs × 3000 instructions in lockstep with `Z80.Spec` (every bus cycle, registers, T-states; /INT in IM 0, 1, 2, NMI and HALT all exercised), two deliberately broken cores caught, the WAIT wrapper tested (`cabal test z80-lockstep`); `make z80-hdl`: Clash Verilog of the core, Verilator lint clean |
| 2026-10-07 | 5.6 (PSG; VDP spec) | `Gg.Psg` = `Gg.Spec.Psg` in lockstep (200 random programs; `gg-lockstep`). The spec's sprite work spread over dots as a VDP must do it (above), so the Clash VDP can match it dot for dot; FluBBa's test, the goldens and two new unit tests (first sprite wins; X fetched at the line start) pass |
| 2026-10-07 | 5.6 **done** | `Gg.Vdp` = `Gg.Spec.Vdp` in lockstep (`cabal test gg-lockstep`): every port read, /INT at every dot and every frame, on 40 random access programs in GG mode and 10 in SMS mode (random VRAM, registers, mid-line writes, all reads), on the machine logs of our six ROMs (12 frames) and Waternet's first 120 frames; a broken VDP caught. The one difference found: the H counter's 9-bit sum overflowed in the core (fixed). `Gg.Psg` likewise (200 programs). `make gg-vdp-hdl`: the VDP with four block RAMs, Clash Verilog, Verilator lint clean |
| 2026-10-07 | 5.7 **done** | `Gg.Core` = `Gg.Spec` in lockstep (`cabal test gg-lockstep`): registers (WZ, Q included) and T-states after every instruction, every frame, every PSG write with its PSG time, on our six ROMs (8 frames), Waternet's first 40 frames and FluBBa's VDP test in SMS mode (60 frames); a machine with the VDP accesses a dot late is caught. `make gg-core-hdl`: the whole machine with its block RAMs through Clash (~10 min) and Verilator, lint clean |
| 2026-10-07 | 5.8 **done** (layer 4) | `Gg.Machine` shared by `Gg.ArtyTop` and `Gg.Top`; `gg-image`; `pattern.s` and `tone.s`; goldens from the spec (`gg-run --vga-at`). `make ci-arty-gg`: the frame identical to the spec's golden, 440.4 Hz in HDMI's audio and on PWM; `make ci-gg` (ZedBoard): the frame identical, the ADV7511's I2C writes identical to the Game Boy's (same table), 440.4 Hz on S/PDIF; both lint clean; both in `make ci`. The PSG's output made bipolar (above). Also: `gg-lockstep`'s memory fixed (reported by M4: 16 GB → 1.3 GB) |
| 2026-10-07 | 5.9 (PL side) | The user: wait for side track I before the PS app; option (a) for the ROM; carry on. The ROM in DDR behind `Gg.RomPort` (above), the tops with HP0, the wrappers `boards/{arty-z7-20,zedboard}/gg_zynq.v` (PS7 with HP0, the PS link not connected yet); `gg-image` writes the cartridge word (mapper, banks; until the PS link); `raster.s` (a timing picture, every frame the same: its HALT phase tuned with PAD). Layer 4 on both boards with the ROM in the harness's DDR: all goldens identical, also at 60-clock DDR latency |
| 2026-10-07 | 5.9 (bitstreams) | Both boards build and meet timing with the ROM in DDR (numbers in "ROM in DDR"); the ZedBoard wrapper needed usb.xdc (the PHY reset pin). `make test SYSTEM=gg` green. Next: the PS app (ID 7, `/gg`, keys, mksd), once side track I has merged (the user) |
| 2026-10-07 | 5.10 (first board run) | `make gg-prog` (the ROM by JTAG): Waternet plays on the Arty (the user: "It works"), the sound bad: the PSG aliased at 48 kHz; `Gg.Audio` (filters as the NES's) |
| 2026-10-07 | 5.9 (keys) | The user: USB input like the other cores, the Game Boy's layout. `[gg]` key map, ID 7 in the PS app, PS link in both tops and wrappers, the menu's pause; `main` merged (M4) |
| 2026-10-07 | 5.9 (keys, board) | The relay's key map came from the menu row (none for the Game Gear): now from the PL's ID. **Board-checked on the Arty** (the user): Waternet with clean sound and the USB keyboard |
| 2026-10-08 | 5.9 (SD menu, PS loading) | `main` merged (the new queue tooling); layer 4 green on both boards with a load as the menu does it and the menu overlay; Arty bit WNS +9.09 ns. **Board-checked on the Arty** (the user): `make prog SYSTEM=gg`, `make load SYSTEM=gg` (the PS's DDR load, the cartridge word), the menu. The card's `/gg` waits for a merged `main` (`make sd` writes every system's bitstream) |
| 2026-10-08 | 5.10 (part) | Merged into `main` (ba7d58f). **Board-checked from the card** (the user, Arty, SD boot): Waternet from `/gg`. Exit items open: the required tests' results on the board (our ROMs report in RAM only), cartridge-RAM saves |
| 2026-10-08 | 5.10 (tests on screen) | The user: the test results on screen first. `gg.inc`'s `finish` draws the verdict: the window green on a pass, red with n white squares on a fail of check n; `passkeep` for `pattern.s` and `tone.s` (their picture or sound is the test). Golden `test/golden/vga/pass.png` (the spec's picture, the same for all six check ROMs); `ci-arty-gg` compares mapper, ports, vdpint, sprites, console and codemasters with it: all identical. A wrong cartridge word from the PS (Sega for codemasters.gg) gives red with one square at layer 4 (a one-off mutation). `gg-spec` and `gg-lockstep` green |
| 2026-10-08 | fix: the long-run freeze | The user: rare freezes on the board, recovering after minutes (Sonic Chaos's sound test, a note held; LD0 still toggling, so the machine ran). Diagnosed over JTAG (read only: the PS link's GPIO words, the ROM in DDR = the file) and in `Gg.Core`: time was 32-bit counts from power-on; `dotOf`'s 3t overflowed at t = 2^32 / 3 (6 min 40 s), and at 2^32 the T-state count wrapped while the dot count didn't, so VDP port accesses and /INT waited for a dot that never came. A test starting the core just before those points reproduced it (vdpint.gg diverged at all three). **Fix:** the counters are phases with matched periods, T-states mod 512 and dots mod 768 (3 dots every 2 T-states: floor(1.5(t + 512)) = floor(1.5 t) + 768), exact across every wrap and smaller than before; every lockstep test now crosses hundreds of wraps a frame; mismatched periods fail at once (mutation). `gg-lockstep` 34/34 |
| 2026-10-09 | fix: board check | The freeze fix board-checked on the Arty (the user: "Should be fixed"), from the card (side J's `make sd` with this build's gg.bit). Arty bit WNS +8.61 ns |
| 2026-10-09 | 5.10 (exit item 1) | **Board-checked** (the user, Arty): the six check ROMs (mapper, ports, vdpint, sprites, console, codemasters) all green over the link: hardware = model. Left: cartridge-RAM saves, stereo |
| 2026-10-09 | 5.10 (exit item 3) | `stereo.gg`, `gg-spec`'s stereo test; **board-checked** (the user, HDMI): left, right, both, silence as the colours say. Left: cartridge-RAM saves |
| 2026-10-09 | 5.10 (saves) | Battery saves as the Game Boy's (`Gg.Save`): cartridge RAM dual-port, the PS loading the save while held (0x100000) and reading it back while the game runs (0x120000, the byte channel; 0x140000 control, 0x140001 dirty clear), the dirty flag on PS_IN bit 6; the menu row's `.save` (32 KiB for every ROM: a Sega header has no RAM field), `/saves/gg` slots in `mksd.sh`. `save.s` counts boots; `test/ps/save.ps` (load "SAVE" 29h, read back 2Ah) in ci-gg and ci-arty-gg. Waternet saves to cartridge RAM (level unlocks, music and sound: MIT, its source) and is the board test. `gg-spec`, lockstep, `make test-zig` green; layer 4 with the merge gates |
| 2026-10-11 | 5.10 (exit item 2), **M5 done** | **Board-checked** (the user, Arty, SD boot): Waternet's cartridge-RAM save survives a power cycle. Every exit item met; closed out by side K (docs/HISTORY.md, "M5") |
