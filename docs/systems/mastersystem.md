# Sega Master System (NTSC)

Target: a **Sega Master System II** (Z80A, the 315-5246 VDP, the SN76489
PSG, 8 KiB RAM, Sega-mapper cartridges, two pads and the Pause button),
milestone 6, its own bitstream (`sms.bit`, system ID 8, menu folder
`/sms`). Planned 2026-10-11 (Claude Code, branch `m6-sms`, at the user's
request through side track K), before any code: everything here is a
plan, and the parts marked *to verify* are checked against the
references below before code depends on them. The user's decisions
are in the next section. When the status log at the
end disagrees with the plan, the log wins.

## The user's decisions (2026-10-11)

| Question | Decision |
|---|---|
| Who owns `systems/gg/`, `cpus/z80/`, `docs/systems/gamegear.md` (and the `gg_zynq.v` wrappers) | **M6** (its CLAUDE.md row); every M6 gate also runs `gg-spec`, `gg-lockstep` and `ci-arty-gg`, so the Game Gear stays as it is |
| How the code is shared | **`systems/sms` depends on the `gg` package** (option (a) below) |
| Region | **NTSC, export** first, as M5 |
| BIOS | **from the card**: the user's own dump, `/sms/bios.sms`, read into DDR when present; never fetched or committed (rule 7); without it the machine starts as a BIOS leaves it |
| FM unit (YM2413) | **in M6** (steps 6.3, 6.5) |
| Pad 2 | **from the keyboard** (the `[sms]` key map) |
| Exit game | ***Raposa do Sol*** (MIT) |

## Why the Master System, and what is reused

The Game Gear (M5) is a Master System inside: its VDP is the SMS 2's with
a 12-bit CRAM and a 160 × 144 window, its PSG is the SMS's with a stereo
port, its mapper and memory map are the SMS's. M5 already built a
Master System mode into both the spec and the Clash core, to run the
Master System test ROMs (docs/systems/gamegear.md, "GG mode against SMS
mode"): it is a runtime flag (`Gg.Spec.newMachineSms`, `Gg.Core`'s
`gSms`, `Gg.Vdp`'s `vSms`) and it is already in lockstep (FluBBa's VDP
test, 60 frames, spec = core). So M6 is mostly a new **machine and
display** around the existing core, plus the Master System's own ports.

| Part | From | M6's change |
|---|---|---|
| Z80 (`cpus/z80`: `Z80.Spec`, `Z80.Core`, `z80Tick` with WAIT) | M5 | none planned |
| VDP, Mode 4 (`Gg.Spec.Vdp`, `Gg.Vdp`), its dot timing (FluBBa 55/55 in SMS mode) | M5 | SMS mode: the full 256 × 192 picture as a stream (has it), the left-column blank and the border in the backdrop colour; the 224-line mode **if a chosen game needs it** (*to verify*) |
| CRAM, 6-bit `--BBGGRR`, direct writes | M5 (SMS mode) | none |
| PSG (`Gg.Spec.Psg`, `Gg.Psg`), the filter (`Gg.Audio`) | M5 | mono: the stereo port is the GG's only (SMS mode already sends both sides) |
| Sega and Codemasters mappers, cartridge RAM, `detectMapper` | M5 | none (more Codemasters games are SMS ones: still none freely licensed to test with) |
| ROM in DDR (`Gg.RomPort`, the freeze-and-catch-up) | M5 | none: up to 1 MiB, the same port |
| Battery saves (`Gg.Save`, the PS's `.save` handling) | M5 | `/saves/sms` slots |
| The PS's cartridge word (`platform/ps/gg_cart.zig`) | M5 (side K's now) | used as is for `.sms` files (a request to K: one row) |
| Layer 4: `--ddr-file`, `reload.ps`, `save.ps`, the overlay | M5 | SMS copies of the scripts |
| The 640 × 480 display path (`Gg.Display`: ×3 into 640 × 480) | M5 | **new**: 256 × 192 ×2 into 512 × 384 at (64, 48); three frame buffers of 256 × 192 × 6 bits (about 24 RAMB36, the GG's 160 × 144 × 12 bits needs a similar number) |

What is **new**:

- **The ports 00h to 06h are not there**: on the Master System they
  decode as the 3Eh/3Fh mirrors (SMS mode has it).
- **Port 3Eh, memory control** (bit 3 BIOS off, bit 5 card, bit 6
  cartridge, bit 4 work RAM, bit 2 I/O chip; 1 = disabled, *to verify*
  against MacDonald's notes): stored by SMS mode today but not acted on.
  Without a BIOS, the machine starts as a BIOS leaves it: cartridge on,
  BIOS off (*to verify* what else the BIOS leaves: games are said to read
  C000h for the BIOS's last 3Eh value; the stack pointer).
- **Two pads**: port DCh = pad 1 (up, down, left, right, 1, 2) and pad
  2's up and down; DDh = pad 2's left, right, 1, 2, bit 4 the Reset
  button, bit 5 1, and the two TH inputs (the region check, modelled).
  The GG has only pad 1. **The SMS 2 has no Reset button** (MacDonald's
  table: only the SMS 1 and the Japanese SMS's siblings do), so DDh bit 4
  reads 1 and there is no Reset key (corrected 2026-10-11, step 6.1).
- **Pause**: a button on the console wired to the Z80's /NMI
  (edge-triggered, SMS mode has it, from the GG's Start).
- **Region**: export (US/EU), NTSC 60 Hz first, as M5. The
  export/Japanese difference is port 3Fh's TH readback (modelled; the
  Japanese machine inverts it) and the FM unit.
- **The BIOS** (the SMS 2 has one built in: the logo, the header
  checksum and region check, then the cartridge; export BIOSes are 8 to
  256 KiB, *to verify*): not needed to run games (*to verify* for
  Raposa do Sol), never fetched, never committed (rule 7). The user's own
  dump, if the card has `/sms/bios.sms`, goes into DDR beside the
  cartridge, and the machine starts in it with port 3Eh's power-on value
  (BIOS on, cartridge off); without one, it starts in the cartridge as
  the BIOS leaves it. Tests use our own stand-in, `bios.s` (it switches
  3Eh and jumps to the cartridge), never Sega's.
- **The FM unit** (YM2413 "OPLL", ports F0h (address), F1h (data), F2h
  (detection/enable, *to verify*); built into the Japanese SMS, an add-on
  for the Mark III, absent on export machines, where some games still use
  it when found): nine FM channels of two operators, or six plus five
  rhythm sounds; fifteen built-in instruments and one user instrument;
  output at its clock / 72 (49.7 kHz). A second sound chip, with its own
  spec (`Sms.Spec.Fm`) and lockstep (`Sms.Fm`), mixed with the PSG. On by
  default with a menu or key-map switch to an "export, no FM" machine
  (*to decide at 6.3*: which games behave differently with it).
- **Pad 2** on the keyboard, beside pad 1 and Pause: the PS link's 16
  user bits fit them (6 + 6 + 1).

## Machine summary (NTSC; ✓ where M5 already checked it, the rest *to verify*)

| Item | Value |
|---|---|
| CPU | Z80A, 3.579545 MHz ✓ (as the GG) |
| Frame | 262 lines × 228 CPU cycles, 59.92 Hz ✓ |
| Memory map | 0000-BFFF the cartridge through the mapper ✓; C000-DFFF 8 KiB RAM, mirrored at E000-FFFF ✓; mapper registers FFFC-FFFF ✓; the BIOS at 0000 when port 3Eh enables it (not modelled without a BIOS) |
| I/O ports | ✓ (MacDonald, "SMS 2 port map") 00-3F: writes to even addresses memory control (3Eh), odd I/O control (3Fh), reads FFh; 40-7F V/H counters (read), PSG (write); 80-BF VDP data/control; C0-FF reads: even DCh, odd DDh, all FFh while 3Eh bit 2 disables the I/O chip; writes do nothing, but F0h/F1h (the YM2413's address and data) and F2h (its detection bit 0, read back only with the I/O chip off) on a machine with the FM unit |
| DDR layout (the media region) | the cartridge ROM at 0x02000000 (up to 1 MiB, as the GG's), the BIOS at 0x02100000 (up to 256 KiB) |
| VDP | 315-5246 (SMS 2): Mode 4, CRAM 32 × 6 bits ✓ (SMS mode), 256 × 192 ✓, the 224-line mode (*to verify* if needed), the legacy TMS modes (SG-1000 software; out of scope: backdrop) |
| PSG | SN76489 (Sega's), mono ✓ |
| Input | pad 1 (DCh), pad 2 (DCh/DDh) ✓, no Reset button on the SMS 2 (DDh bit 4 = 1) ✓, Pause on /NMI ✓ (MacDonald) |
| Interrupts | the VDP's /INT (IM 1) ✓, Pause on /NMI ✓ |

## References (the M5 ones, plus)

- Charles MacDonald's SMS/GG hardware notes (smstech-20021112.txt) and
  VDP notes (msvdp-20021112.txt): the ports, 3Eh, the pads, the VDP
  differences between the SMS 1 and SMS 2.
- SMS Power, Development: "Memory control", "I/O ports", "Region
  detection", "BIOSes", "YM2413", "VDP registers"
  (https://www.smspower.org/Development/).
- Sega's Software Reference Manual for the Sega Mark III / Master System
  (*to find*: a public scan on segaretro.org), the Master System's
  equivalent of the GG manual M5 used.
- Emulators (MAME's `sms.cpp` / `315_5124`, Genesis Plus GX, MEKA) as a
  cross-check for behaviour, values and rules only, never code.

- The YM2413's behaviour as we model it: **docs/systems/ym2413.md** (written
  2026-10-11 for step 6.3: registers, the instrument ROM, phase, envelope,
  operator, rhythm, output, a cycle schedule; its rules checked by a
  throw-away model against Nuked-OPLL run as a black box; values and rules
  only). Its sources:
- The YM2413: Yamaha's YM2413 application manual (the register map, the
  instrument parameters; *to find* a public scan); SMS Power, "YM2413";
  the built-in instrument table as measured on the chip (published with
  emu2413, MIT, and Nuked-OPLL, GPL-2.0: values only, cross-checked
  between the two, never code); the die-analysis notes behind Nuked-OPLL
  for the envelope and phase details.

## The Master System II in the spec (step 6.1)

`Gg.Spec.newMasterSystem` (BIOS image or none, mapper, ROM) is the
Master System's machine: the VDP and PSG in M5's Master System mode, on
the console's own hardware (`Console`: `MasterSystem2`; a Game Gear is
`GameGear` in either of its modes, and a GG in Master System mode really
does ignore most of port 3Eh and has no pad 2, so M5's model and tests
are untouched). From MacDonald's SMS/GG hardware notes:

| What | The model |
|---|---|
| Port 3Eh (memory control) | bit 6 clear: the cartridge (with its RAM and mapper) answers 0000h-BFFFh; bit 3 clear: the BIOS image; both: ANDed (the SMS 2); neither: FFh (*to verify*); bit 2 set: C0h-FFh read FFh; bits 7, 5 nothing (the SMS 2); bit 4 (work RAM) not modelled, RAM always on (MacDonald hasn't measured it) |
| Power-on without a BIOS | 3Eh = ABh and C000h = ABh, as the SMS 2's BIOS leaves them (MacDonald: it writes ABh, and keeps its last 3Eh value at C000h for the game); SP = DFF0h as M5's |
| Power-on with a BIOS image | 3Eh = E0h (the BIOS, RAM and I/O on, the cartridge off; *to verify*), PC 0 in the BIOS; the BIOS's 16 KiB banks follow FFFDh-FFFFh (a Sega mapper of its own, the first 1 KiB fixed; *to verify* against a 128 KiB BIOS), an image under 16 KiB mirrored |
| Pads | DCh: pad 1 bits 0-5, pad 2's up/down bits 6-7; DDh: pad 2's left, right, 1, 2 bits 0-3, bit 4 1 (no Reset), bit 5 1, TH-A/B bits 6-7 (the export region check) |
| The FM unit | none yet (step 6.3): F2h reads FFh with the I/O chip off, so the detection finds none |
| /INT acknowledge | FFh on the bus (MacDonald: the SMS 2 as the GG) |

`gg-run` runs it (`--sms`, the default for a `.sms` file; `--bios FILE`;
pad 2 in input scripts as `u d l r 3 4`; `--gg-sms` is M5's Game Gear in
Master System mode). Since step 6.3 the simulator is a library module
(`Gg.RunMain`), and `sms-run` (the sms package) is the same program with
the FM unit fitted (`--no-fm` leaves it out); `gg-run` has none.

Checked (`cabal test sms-spec`, 14 tests): `ports.s` (13 checks: the C000h
copy, 00h-3Fh read FFh, the pads' idle values, the even/odd decode, the
export TH read-back, the I/O chip off, the cartridge off and on from code
in RAM; a 14th, "no FM unit", was dropped at 6.6: the board's machine has
the unit, and `fm.s` checks the detection either way); `bios.s`, our own stand-in BIOS (it reads 7FF0h with
the cartridge off, copies its hand-over to RAM, writes 3Eh = ABh and its
C000h copy, jumps to 0000h) with `biosgame.s` as the cartridge (4 checks;
without the BIOS it fails check 2, as it must); the pad bits on both
consoles; FluBBa's VDP test 55/55 on the Master System II; ZEXDOC and
ZEXALL-SMS, every test reported within 30 s of machine time OK (22 or
more; 6 minutes and 30 or more in the FULL tier; the Z80 itself passes the
whole CP/M originals in `z80-zex`). Planted bugs in the model (3Eh's I/O
bit ignored, its cartridge bit ignored, no ABh at C000h) each failed
`ports.s` at their check. By eye: sverx's SMS Test Suite reports "Model:
Master System II, Region: USA/BRA, TV: 60Hz" (on M5's Game Gear in Master
System mode: "Game Gear (Single ASIC)"), and its pad page shows both pads'
presses. ZEXALL-SMS's SCF/CCF pre-test (the undocumented flags after SCF
and CCF, per CPU type) prints the model's pattern (`Z80.Spec`'s, the NMOS
one SingleStepTests checks); a real SMS 2's is *to verify* on the board.
Raposa do Sol and Waternet reach their title screens.

## The picture (step 6.2)

The spec's frame is the whole 256 × 192 picture (M5's Master System
mode; R0 bit 5's blanked left column shows the backdrop, as before). New:
**the border**. A TV shows the backdrop colour (16 + R7's low nibble)
around the picture; the spec latches it as each frame completes
(`Gg.Spec.Vdp.vdpBorder`, `Gg.Spec.frameBorder`): one colour per frame,
which the board draws around the picture. A game changing the backdrop
during the border isn't followed (none known). The board's 640 × 480
picture: ×2 at (64, 48), the border around it (`Gg.Screen.frameSmsVgaPng`,
`gg-run --sms --vga-at`), the layout `Sms.Display` will build (6.6).

The 224-line mode: not needed by Raposa do Sol (192 lines); left out.

Gates (`sms-spec`, 17 tests): `picture.s` (a checkerboard, the left
column blanked, a blue backdrop and so a blue border) against its frozen
640 × 480 golden (`test/golden/vga/picture.png`, checked by eye); Raposa
do Sol played by `test/input/raposa-start.txt`, frame hashes and borders
of the title, the mode menu, the ships screen and the game frozen
(`test/golden/raposadosol.txt`; in the game its backdrop, sprite colour 0,
is white, so the border and the blanked column are white: the game's own
README notes that column for its text screens). A border stuck at black
fails both goldens.

## The FM unit's chip (step 6.3, `Sms.Spec.Fm`)

The YM2413 as docs/systems/ym2413.md describes it, a sample at a time in
the doc's frame order, with register writes landing at their exact cycle
(a cycle is 4 T-states, a sample 72). Output: the MO and RO pins for every
cycle, as the chip puts them out; the audio is a sample's sum of both pins.

**The oracle** (the user's choice, 2026-10-11): Nuked-OPLL at commit
1269cf5 (GPL-2.0-or-later), fetched by `tools/fetch-roms.sh` into
`roms/sms/nuked-opll/` and never committed, built behind a small Zig
driver (`systems/sms/tools/opll_oracle.zig`, `make opll-oracle`). Nothing of
it is in our code. `sms-spec` drives both with the same register programs
(random ones that keep the manual's write waits, the user patch, rhythm
mode on and off, the mirrors) and compares every pin of every cycle: the
quick tier one note and 8 programs (about 4 600 samples each), the FULL
tier 40 programs of about 30 500 samples. All equal, but for one narrow
exception: in the sample where rhythm mode switches on or off a drum's
pins can differ (24 of 22 million cycles, never lasting; unmeasured on
hardware). Planted bugs (a multiplier entry, the attack step, a drum's
second copy) fail it. What the comparison settled beyond the doc (the
vibrato's carry, when a write is seen, where rhythm mode is read, the pins'
gating) is in ym2413.md, 10.5.

## The FM unit in the machine (step 6.3, `Sms.Spec.FmUnit`)

`Gg.Spec.newMasterSystemFm` takes the unit as a record of functions
(`FmUnit`: a port write, an F2h read, the level at a T-state, whether the
PSG plays), so the gg package needn't know the chip. Ports (SMS Power,
"Audio Control Port"): F0h address, F1h data, F2h the audio control as on
the Japanese SMS (bit 0 FM through; the PSG plays while bit 1 = bit 0;
00h at power-on: the PSG only, so every PSG game sounds as without the
unit); F2h reads back bits 1-0 (bits 7-2 0, *to verify*) only with the I/O
chip off, as games' detection expects. A write at T-state t is the
chip's strobe at cycle t div 4. The sound: a sample's pin sum × 24
(`fmGain`: a full FM channel ±6144 against a PSG channel's ±8191; the
balance on real hardware is unmeasured, *to tune* by ear), mono, added to
the PSG on both sides at each PSG tick, clipped to 16 bits.

Gates (`sms-spec`, 29 tests): `fm.s` finds the unit as Phantasy Star's
routine does (I/O chip off, F2h written and read back seven times) and
plays A4 (instrument 1, F-number 120h, block 4): 436.97 Hz, measured
within 0.2 Hz (autocorrelation over 0.5 s); without the unit it fails its
check 1; with the unit fitted but off, Raposa do Sol's 300 frames and their
audio are identical. `fmtune.s` (ours: a melody, bass, chords, the rhythm
section) is for the ear: heard by the user 2026-10-11 ("sounds good",
after a fix to its F-numbers), its 20 s of audio frozen as a hash
(`test/golden/fmtune.txt`, the same as the WAV's). The user also heard
their own Sonic 1 FM hack (a fan ROM, local only, never committed) on
`sms-run`: "sounds good" (the FM:PSG balance included). No freely
licensed homebrew game with FM music was found (MBMlib, devkitSMS's FM
replayer, is a library).

## The Master System II in Clash (step 6.4, `Gg.Core`)

`Gg.Core.smsPowerOn bios fm mapper banks` is the spec's
`newMasterSystemFm` in Clash, the same rules (`Gg.Core`'s header): port
3Eh's cartridge and BIOS enables (both on: two ROM bytes ANDed, the second
through `ggRomWant2`/`giRom2`), the I/O chip off, pad 2 (`giPad2`), the
start state (C000h = ABh written on the first pre-roll clock), the BIOS
slot (`RomAddr`, 21 bits: the BIOS from 100000h, i.e. DDR 0x02100000;
banked by FFFDh-FFFFh), the FM unit's ports as an output (`goFm`, at the
T-state the spec gives the unit its write; `goTState` for the unit's
time; F2h's read-back kept in the core), and the border colour
(`goBorder`, `Gg.Vdp.vdpBorderC`, taken at `goFrameEnd`).

Checked (`cabal test gg-lockstep`, "the Master System II"): core = spec,
registers and T-states after every instruction, frames with their border
colour, PSG writes and FM-unit writes (T-state, port, byte; the spec side a
recording fake unit), on our SMS ROMs (smoke, ports, picture), biosgame
with our stand-in BIOS, fm and fmtune with the unit, `padloop.s` with pad 1
and pad 2 pressed and released, FluBBa's test and Raposa do Sol (60 frames
each); a core with pad 2's up/down swapped is caught, and so are planted
bugs in the I/O chip's disable, C000h's start value, the BIOS enable and
the FM write's T-state. The Game Gear's 38 cases are unchanged (48/48).
`make gg-core-hdl`: the core through Clash and Verilator lint.

## The YM2413 in Clash (step 6.5, `Sms.Fm`)

One function per internal cycle (`fmCycle`: a write strobe in, two pins
out), its state one record. The spec works a slot with the registers its
fetch saw and reads rhythm mode up to three cycles later; hardware can't
look ahead, so every write is held back three cycles before it reaches the
registers (the landing rules shift with it: a channel register's target
cycle number moves by three too) and each slot is worked three cycles after
its fetch, one slot a cycle; rhythm mode's bit is also kept undelayed, as a
four-cycle history, for the reads after the fetch. So `fmCycle` at cycle c
gives the spec's pins of cycle c + 2. The tables are ROMs (`listToVecTH`);
the tremolo has its own exact counter (a frame counter mod 2^20 would drift
after 21 s).

Checked (`cabal test sms-lockstep`): every pin of every cycle equal to
`Sms.Spec.Fm`'s (from the third sample) on the single note, a program that
switches rhythm mode 300 times, and sms-spec's random programs (4 in the
quick tier, 40 of ~30 500 samples in the FULL tier: 42/42, no exceptions:
both follow the same rules at rhythm switches). Found on the way: the
landing shift above, and a precedence slip in the phase increment (`x *
mult `shiftR` 1`) that froze multiplier 0's phase. Planted bugs (the
landing unshifted, the output stage's rhythm read a cycle early, the noise
inverted, a pin changed) are caught. `make sms-fm-hdl`: Clash and Verilator.

## The board's machine (step 6.6, `Sms.Machine`, `Sms.Display`, the tops)

`Sms.Machine` is `Gg.Machine` for a Master System II, with M5's parts
reused (`Gg.RomPort`, `Gg.Save`, `Gg.Audio`, the frame buffers, the text
overlay, the PS link): the core powered on as `smsPowerOn` (the BIOS's
presence and size taken from the cartridge word on the first pre-roll
clock: bits 7-3 its 16 KiB banks, 0 for none; `gg-image ROM DIR --bios
FILE` writes it); the ROM and the BIOS through the one ROM port (the BIOS
at `romBase` + 1 MiB); the FM unit (`Sms.Fm`) stepped every fourth T-state
(`goTState`), the write of the cycle that ended fed to it, its 18 pin pairs
a sample summed ×24 and mixed with the PSG as F2h says, then `Gg.Audio`'s
filters; the picture (`Sms.Display`): 256 × 192 in three frame buffers of
6-bit colours (2 bits a channel, as the CRAM), ×2 into 640 × 480 at
(64, 48), the border from `goBorder` latched at each frame's end (one
register, so it may lead the shown frame by one). Keys from the PS link
(`smsPsKeys`: pad 1 as the Game Gear's bits, Pause bit 7, pad 2 in bits
8-13). `Sms.ArtyTop` (HDMI, PWM; BTN0-3 the pad, SW0 button 1, SW1 Pause)
and `Sms.Top` (the ZedBoard: VGA, HDMI through the ADV7511, S/PDIF), each
in its `sms_zynq.v`.

**Not on the board:** a cycle with the cartridge and the BIOS both enabled
reads the cartridge's byte alone (the spec and the core AND them; the ROM
port serves one byte a cycle; no BIOS or game leaves both on).

Layer 4 (`ci-arty-sms`, all identical to the spec): `picture.s`'s
640 × 480 picture; smoke, ports and padloop give the pass picture; our
stand-in BIOS (`BIOS=build/sms/asm/bios.sms`; the harness takes one DDR
file, so the make files build one image of both) hands over to
biosgame; the FM unit's tone (`fm.s`, now a pure sine from the user
patch) at 436.9 Hz on PWM and in the HDMI audio; a load as the menu does
it (PS_IN = system 8) and the menu's overlay. Verilator lint clean. Clash
warns of a "dubious primitive" (`integerToInt#`) from `Sms.Fm`'s
`fromIntegral`s of a few bits (shift amounts, bit numbers): harmless at
those widths. `ci-sms` (the ZedBoard) waits for its heavy run.

## The PS side (step 6.7)

Done additively, side track K's answer of 2026-10-11 ("do it yourself,
additively"; K reviews at the merge):

| Where | What |
|---|---|
| `platform/ps/menu.zig` | the `/sms` row: ID 8, `.sms` files up to 1 MiB, the ROM into DDR at 0x02000000, the cartridge word from `gg_cart.word` (the Game Gear's: mapper, banks), saves as the Game Gear's (32 KiB, the same link map); a generic **companion file** (`System.companion`: a name in the system's folder, a DDR address, a size limit, a size check, a hook adjusting `ddr_info`'s word), null for every other row |
| `platform/ps/sms_cart.zig` (ours) | the companion: `bios.bin` (not `.sms`: the menu lists those as games), at 0x02100000, 16 KiB × 2^n up to 256 KiB, its banks into the word's bits 7-3 |
| `platform/ps/load.zig` | `loadCompanion` (none, loaded, or refused with a message) and `streamTo`; `streamDdr` is `streamTo` at the row's address |
| `platform/ps/relay.zig` | both DDR loads (the menu's, the splash's): the companion first; a refused one stops before the cartridge (its message on the menu); the word through the row's hook; ID 8 = the `[sms]` key map |
| `platform/ps/keymap.zig`, `usb_keys.zig` | `[sms]`: pad 1 as `[gg]` (W A S D, J = 1, K = 2, L = Pause; Cross, Circle, Options), pad 2 on the arrow keys, `,` = 1, `.` = 2 (the user, 2026-10-11) |
| `platform/host/link.zig` | `--system sms` (keys, load, save slots), `--bios FILE` (`make load SYSTEM=sms BIOS=FILE`) |
| `tools/mksd.sh`, the Makefile | `/sms` from `SD_SMS_ROMS` (`roms/sms/games build/sms/asm`), `/saves/sms`, `sms.bit` into `/systems`; `SMS_BIOS=FILE` (the user's own dump, wherever they keep it) as `/sms/bios.bin` only when given (the user has none: nothing is copied) |

**DDR layout** (the media region, docs/ARCHITECTURE.md): the cartridge
ROM from 0x02000000 (up to 1 MiB), the BIOS from 0x02100000 (up to
256 KiB). The cartridge word (link address 0, low byte first): bit 0 the
mapper (1 = Codemasters), bits 7-3 the BIOS's 16 KiB banks (0: none), bits
15-8 the ROM's 16 KiB banks - 1.

Tests: `make test-zig` (`load.zig`'s companion test on the FAT32 test card,
`tools/mkfat-test.sh`'s `/sms` (a good 32 KiB BIOS: byte for byte in
DDR, 2 banks in the word), `/smsbad` (20000 bytes: refused, nothing
written), `/smsnone` (no BIOS); `sms_cart.zig`'s sizes and word;
`tools/test_link.py`'s remap test knows `[sms]`).

**The PS stack** (`tools/check-ps-stack.py`): the `[sms]` map pushed the
deepest path (a switch on the Systems menu, then the card's listing) from
13632 to 14520 of 16384 bytes (limit 13824): `keymap.parse` returned its
`Config`, a map per system, by value, two copies on the stack. Now
`keymap.parseInto` (side K's choice; `parse` a wrapper, its tests kept, a
new test that parsing into a used `Config` leaves nothing behind) and the
relay parses straight into `key_config` (a bad file then leaves the
defaults, as its message always said): 12496 (ZedBoard) and 12480 (Arty)
bytes.

## The ROM header (`Sms.Header`, step 6.0)

"TMR SEGA" at 7FF0h (or 3FF0h, 1FF0h), the checksum, the region (high
nibble of the last byte: 3 SMS Japan, 4 SMS export, 5 to 7 Game Gear) and
the size code (low nibble: Ah 8 KiB to 2 1 MiB). SMS Power doesn't give
the checksum's range; found from four tools' headers, which all agree:
a 16-bit sum from 0000h up to min(size, 32 KiB) - 16 (that size's own
header left out), then from 8000h up to the size. GBDK's Waternet header
says 8 KiB (Ah) in a 64 KiB file, and its sum covers 0000h-1FEFh; ours
(WLA-DX), Raposa do Sol's (devkitSMS) and ZEXALL's agree as well. Only a
BIOS checks it.

## Accuracy target

As M5: the Z80 T-state exact, the VDP at dot level (FluBBa's test 55/55
in SMS mode stays a gate), the PSG bit-exact. Out of scope: PAL/50 Hz
(the 313-line frame), the SMS 1's VDP quirks (315-5124: the name-table
mask, four zoomed sprites per line), the legacy TMS modes, the 240-line
mode (PAL only), the Japanese region itself, the light phaser, paddle,
sports pad, 3D glasses, the card slot, Korean and other mappers. The
FM unit: sample-exact against its spec; the spec's accuracy against the
real chip is as good as the published measurements (*to verify* how
close emu2413 and Nuked-OPLL agree).

## Test ROMs (fetched by `tools/fetch-roms.sh` into `roms/sms/`, never committed; the VDP test and sverx's suite are the Game Gear's copies in `roms/gg/`)

| What | Use | Licence |
|---|---|---|
| SMS VDP Test 1.31 (FluBBa) | already a `gg-spec` gate in SMS mode; 6.2 on the SMS machine, 6.5 in layer 4, the board at exit | no formal licence; fetch, don't commit (the user, 2026-10-07) |
| SMS Test Suite v0.38 (sverx) | it identifies the model ("Master System II", *to verify* what it prints for our port values), region, 60 Hz; video, PSG and pad pages by eye | no formal licence; fetch, don't commit (the user, 2026-10-07) |
| ZEXDOC / ZEXALL for the Master System (maxim-zhao/zexall-sms) | FULL tier on the SMS spec (it is an SMS ROM; on the GG it was *to verify*) | GPL-2.0 |
| Our own regression ROMs (`systems/sms/test/asm/`, `make sms-asm` into `build/sms/asm/`; `sms.inc` defines `SMS` and includes the Game Gear's `gg.inc`, one copy: the header's region 4 and the verdict in SMS CRAM; the GG's ROMs stay byte-identical) | rule 5: the ports (3Eh, DCh/DDh, Reset, no 00-06h), Pause on NMI, the pad 2 bits, the left-column blank and border, a tone; the verdict on screen as `gg.inc`'s `finish` | ours |
| **Exit game (the user, 2026-10-11):** *Raposa do Sol* (jflores82, github.com/jflores82/raposadosol, release v0.20) | a native Master System game (devkitSMS), cartridge-RAM high-score saves since v0.18: the save exit item | MIT; the README says all assets (code, graphics, music) are the author's (checked 2026-10-11) |
| Second game: *Waternet*, Master System build (joyrider3774, release v1.0, `Waternet.Sega.Master.System.zip`) | the M5 exit game's SMS build: a known-good comparison with `gg-run` | MIT (as M5's) |

Rejected: *Winter Shooter* (MIT code, but its sprites come from the
Spriters Resource and OpenGameArt: mixed licences).

## Plan (PLAN.md, "Milestone 6")

The steps are in PLAN.md. How the code is shared (the user chose (a)):

- **(a) `systems/sms` depends on the `gg` package** (my recommendation):
  `sms.cabal` lists `gg` and `z80`; `HDL_SRC_EXTRA = cpus/z80/src
  systems/gg/src`. Only `Sms.Machine`, `Sms.Display`, `Sms.Top`,
  `Sms.ArtyTop` and the SMS ports are new; the SMS-specific spec and core
  changes go into `Gg.Spec`/`Gg.Core`'s SMS mode, where they already
  live. One copy, no renames, the Game Gear's tests and goldens untouched
  (each gate also runs `gg-spec`, `gg-lockstep` and `ci-arty-gg`, which
  prove nothing changed for the Game Gear).
- **(b) a shared tree** (`systems/sega8/`: `Sega8.Vdp`, `Sega8.Psg`,
  `Sega8.Core`, the mapper, `RomPort`, `Save`, `Audio`), with `systems/gg`
  and `systems/sms` thin on top: cleaner names, but a rename of most of
  M5's code (its goldens must stay identical and `gg.bit` be rebuilt) before
  any Master System work. It can be done later, from (a), if a third Sega
  system comes.

M6 owns `systems/gg/` and `cpus/z80/` for that (the user).

## Status log

| Date | Step | State |
|---|---|---|
| 2026-10-11 | planning | this file, PLAN.md "Milestone 6", system ID 8 and `/sms` reserved (ARCHITECTURE.md); branch `m6-sms` |
| 2026-10-11 | decisions | the user: M6 owns `systems/gg` and `cpus/z80`; `sms` depends on `gg`; NTSC export; the BIOS from the card, the FM unit and pad 2 in; Raposa do Sol. Plan updated (6.0 to 6.8) |
| 2026-10-11 | 6.0 (part) | Raposa do Sol, Waternet's SMS build and ZEXALL-SMS fetched and pinned (`roms/sms/`); `systems/sms` (package on `gg`, `z80`), `Sms.Header`, `sms.inc` (via `gg.inc`, GG ROMs byte-identical), `smoke.s`, `make sms-asm`, `sms-spec` 6/6 (a planted wrong check fails). On today's SMS mode Raposa do Sol and Waternet already reach their title screens in `gg-run` |
| 2026-10-11 | 6.1 (spec) | `newMasterSystem` (`Console`: `MasterSystem2`), port 3Eh, the start state, the BIOS slot, pad 2; `gg-run --sms/--bios`, pad 2 letters; `ports.s`, `bios.s` + `biosgame.s`; `sms-spec` 14/14 with FluBBa 55/55 and ZEXDOC/ZEXALL-SMS on the SMS 2; sverx's suite: "Master System II"; three planted model bugs caught. The SMS 2 has no Reset button (MacDonald): Reset dropped from the plan |
| 2026-10-11 | 6.2 | the border (`vdpBorder`, `frameBorder`), the board's 640 × 480 picture (`frameSmsVgaPng`, `gg-run --vga-at`); `picture.s` with its golden, Raposa do Sol's golden (4 frames, by eye); `sms-spec` 17/17, a black-border mutation fails both; no 224-line mode needed |
| 2026-10-11 | 6.3 (the chip) | docs/systems/ym2413.md (the YM2413's behaviour, researched); `Sms.Spec.Fm` from it; Nuked-OPLL as the oracle (`make opll-oracle`); `sms-spec`'s oracle tests: every pin of every cycle equal on 41 FULL programs (but a drum's pins in a rhythm-switch sample); four rules the doc lacked found and recorded; three planted bugs caught. Next: the ports F0h-F2h in the machine, the mix, an FM tone ROM |
| 2026-10-11 | 6.3 (the unit) | `FmUnit` in `Gg.Spec` (ports F0h-F2h, the Japanese SMS's audio control, the mix), `Sms.Spec.FmUnit`, `sms-run` (`Gg.RunMain` shared with gg-run); `fm.s` (detection, A4 at 436.97 Hz ±0.2), `fmtune.s` (for the ear); `sms-spec` 29/29, `gg-spec` 36/36. Waiting: the user's listen to `build/sms/fmtune.wav` |
| 2026-10-11 | 6.3 **done** (spec) | The user listened: fmtune (after its F-numbers were fixed) and their Sonic 1 FM hack "sound good"; fmtune's audio hash frozen. `sms-spec` 30/30 |
| 2026-10-11 | 6.4 | `Gg.Core`'s Master System II (`smsPowerOn`): 3Eh, BIOS slot (21-bit `RomAddr`), pad 2, start state, FM ports out, border; `Gg.Vdp.vdpBorderC`; `Gg.RomPort` 21-bit; gg-lockstep 48/48 (10 new SMS II cases, planted bugs caught) |
| 2026-10-11 | 6.5 | `Sms.Fm` (the YM2413 per cycle, writes delayed three cycles); `sms-lockstep`: every pin = the spec's, quick 7, FULL 42/42; planted bugs caught; `make sms-fm-hdl` |
| 2026-10-11 | 6.6 (Arty) | `Sms.Machine`, `Sms.Display`, `Sms.ArtyTop`, `Sms.Top`, both `sms_zynq.v`, both `.mk`; `ci-arty-sms`'s every item identical to the spec (picture, check ROMs, BIOS hand-over, FM tone 436.9 Hz, PS load, overlay), lint clean; the ZedBoard's run waits for a heavy slot |
| 2026-10-11 | 6.7 (PS side) | side K: "do it yourself, additively": the `/sms` row, the generic companion file (the BIOS, `sms_cart.zig`), `[sms]` with pad 2, `link --system sms --bios`, `mksd.sh`'s `/sms` and `SMS_BIOS`; `keymap.parseInto` for the stack (12496/12480 of 16384); `make test-zig` 545/546 |
| 2026-10-11 | 6.6 (ZedBoard), bits | `ci-sms` green on the ZedBoard; `make bit SYSTEM=sms` on the Arty (WNS +0.587 ns; 18873 LUTs, 52 BRAM tiles, 8 DSPs) and the ZedBoard (WNS +0.117 ns; 19631 LUTs, 52 BRAM tiles, 20 DSPs): thin slack, a 53-level path in one pixel clock |
| 2026-10-12 | 6.6 merge gate | `main` merged (D3); the FM level registered in `fmStep` (it reached the audio filter in one clock); `make ci` green (40 run); `make bit SYSTEM=sms` WNS +4.771 ns (Arty), +4.679 ns (ZedBoard) |
| 2026-10-12 | 6.8 (JTAG) | **Board-checked by the user on the Arty** over `make prog`/`make load`: Raposa do Sol, picture, border, sound, both pads, the menu; the SD-boot half of the exit next |
| 2026-10-12 | 6.8 **done** | **Board-checked by the user on the Arty**, from SD boot: the exit's checks pass; the user's BIOS waits for a dump |
| 2026-10-12 | boot splash | the user's request: Rota on the Master System (`Rota.Sms`, `demos/sms/rota.s`, on a shared `Rota.Sega` with the Game Gear); layer 3 every frame = the model, layer 4 = the spec, `ci-demos` line; docs/brand.md |
| 2026-10-14 | **M6 done** | Merged (903810b); `main`'s post-merge `make ci ALL=1` green on fd04bc3. Closed out by side K at the user's request (docs/HISTORY.md, "M6"). Left: a board check of the later additions, the user's BIOS dump |
