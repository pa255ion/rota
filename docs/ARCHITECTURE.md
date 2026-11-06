# Architecture

## Layers

```
┌──────────────────────────────────────────────────────────────────┐
│ Board top (Platform.Board.Zedboard + boards/zedboard/*.xdc)      │
│  pins ↔ typed signals, clock wizard, reset sync, PS (optional)   │
│ ┌──────────────────────────────────────────────────────────────┐ │
│ │ Platform services (platform/)                                 │ │
│ │  VGA timing · scaler/letterbox · debouncer · keypad/pad      │ │
│ │  readers · audio out · ROM loader port · debug/LED mux       │ │
│ │ ┌──────────────────────────────────────────────────────────┐ │ │
│ │ │ System core (systems/<name>/src/<Name>/Core.hs)           │ │ │
│ │ │  board-agnostic; talks only through the Core interface   │ │ │
│ │ └──────────────────────────────────────────────────────────┘ │ │
│ └──────────────────────────────────────────────────────────────┘ │
└──────────────────────────────────────────────────────────────────┘
```

A **system core** never touches pins, clocks, or board details. It can be
simulated with nothing but Haskell. The **platform** turns the core's generic
outputs into board signals.

## Clock domains

Defined once in `Platform.Domains`:

```haskell
createDomain vSystem{vName="Dom100", vPeriod=hzToPeriod 100e6}   -- board oscillator (GCLK, pin Y9)
createDomain vXilinxSystem{vName="DomPix", vPeriod=39720}        -- 640×480@60 pixel clock (MMCM output, ≈25.176 MHz)
createDomain vXilinxSystem{vName="DomPixDdr", vPeriod=19860}     -- output side of DDR registers clocked by DomPix (HD_CLK)
createDomain vSystem{vName="Dom125", vPeriod=hzToPeriod 125e6}   -- Arty Z7-20 oscillator (clk, pin H16; side track G)
createDomain vXilinxSystem{vName="DomPix5", vPeriod=7944}        -- Arty: the TMDS serialisers' 5x clock (only their clock input)
```

- **Arty Z7-20 (side track G):** one hand-written MMCM
  (`boards/arty-z7-20/arty_mmcm.v`, `Platform.Board.ArtyZ7.artyClocks`)
  makes `DomPix` at 25.17361 MHz and `DomPix5` at exactly 5×; the domain
  periods are nominal, systems keep `DomPix` unchanged and run 104 ppm
  slower than on the ZedBoard (docs/ARTYZ7.md, "Clocks").

- For simple systems like CHIP-8, the core runs **in `DomPix`** and is gated
  by clock enables. Pixel output and core then share a clock, so no
  clock-domain crossing is needed.
- Later systems that need a specific master clock (for example the Game
  Boy's 4.194304 MHz) run either in a dedicated domain with a dual-clock BRAM
  framebuffer between core and video, or in a faster domain with a
  fractional clock enable. The choice is made per system and documented in
  `docs/systems/<name>.md`. **Game Boy (planned):** `DomPix` with a
  phase-accumulator dot enable (≈ 6 pixel clocks per dot), double-buffered
  framebuffer, no crossing (docs/systems/gameboy.md).

## Video outputs

The scaler produces one pixel stream in `DomPix`: 24-bit RGB (`Rgb888`;
12-bit before M3 step 3.7) plus DE, HSYNC and VSYNC, in 640×480@60
timing. The VGA DAC takes the top 4 bits of each channel, HDMI all 8.
Systems with 12-bit colours widen them at the source (`fromRgb444`, c ×
17), which leaves their VGA and HDMI output exactly as before (the
24-bit YCbCr converter reproduces the 12-bit one for those colours,
checked for all 4096 in `platform-unit`). Systems whose frame rate isn't
the display's use `Platform.Video.FrameBuffers.tripleBuffer` (Game Boy,
NES). Every output is a sink of that
stream: the VGA DAC pins directly, the HDMI path (M0d: 4:2:2 conversion,
then the ADV7511 bus) and, on another board later, a TMDS encoder. The
OLED is separate: it reads the core's framebuffer through its own port.

## Core interface

There is no single Haskell `CoreIn`/`CoreOut` type shared by every
system: each core has its own records (`Chip8In`/`Chip8Out` in
`Chip8.Core`), because address widths, framebuffer shapes and the event
type differ. What is shared is a **set of field names and meanings**, so
that tops, the loader, the LED debug path and the lockstep harness look
the same from system to system. A core is a pure transition function on
one state record plus its memories, wrapped as

```haskell
chip8Core :: HiddenClockResetEnable dom => Signal dom Chip8In -> Signal dom Chip8Out
```

**Fields every core has** (CHIP-8's types shown; widths are per system):

| Field | CHIP-8 | Meaning |
|---|---|---|
| `ciKeys` | `BitVector 16` | buttons held, layout per system (CHIP-8: bit k = key k; Game Boy: bits 0 to 7, docs/systems/gameboy.md) |
| `ciConfig` | `BitVector 8` | options from the switches (CHIP-8: quirk bits) |
| `ciRun` | `Bool` | low: the core makes no progress (held low while loading) |
| `ciReset` | `Bool` | soft reset to the power-on state; memories written by the loader are kept |
| `ciRomWrite` | `Maybe (Unsigned 12, Byte)` | loader port into program memory (Game Boy: 20-bit address) |
| `coDebug` | `BitVector 32` | status for the LEDs (`Platform.Output.Leds.debugByte`) and the ILA |
| `coEvent` | `Maybe Event` | trace for the lockstep tests; `Event` is per system |

**Clock enables come in as inputs.** CHIP-8 has `ciInstrTick` and
`ciTimerTick`; the top derives them (instruction rate, VGA vertical
blank) and the tests drive them directly. A core never counts board
clocks itself, which keeps it board-agnostic and lets tests run it at any
speed. The Game Boy gets its dot enable the same way (the phase
accumulator of docs/systems/gameboy.md lives in the top).

**Video: framebuffer read ports.** The core owns its framebuffer and
exposes synchronous read ports, one per video consumer, so video never
stalls the core: CHIP-8 has `ciFbAddr` → `coFbRow` (VGA) and
`ciFbAddr2` → `coFbRow2` (OLED), each a 64-bit row with one cycle of
latency. The system's display module (`Chip8.Display`) turns those reads
into the platform's pixel stream (`VgaOut` from `Platform.Video.VgaTiming`:
24-bit RGB since M3 step 3.7, DE, syncs, 640×480 timing), which every output sinks (the VGA
pins, and HDMI from M0d).
**Game Boy (decided 2026-10-02):** the same shape. The PPU writes into
the core's double-buffered framebuffer inside the core, and the display
module reads it through a `ciFbAddr`/`coFb…` port pair, ×3 and
letterboxed. A platform scan converter for a native `PixelStream` (pixel
strobe, blanks, RGB) is only built when a later system's video can't be
buffered inside its core.

**Events.** `coEvent` carries at most one event per clock. CHIP-8 emits
`Retired keys arch` per instruction and `Ticked` per timer tick, and the
harness replays the same sequence on the spec. The Game Boy has several
kinds per M-cycle (bus access, instruction boundary, interrupt dispatch,
PPU mode and LY changes); one M-cycle is about 24 `DomPix` clocks, so
they go out in separate clocks within the M-cycle, in a fixed order that
the spec's per-M-cycle event list uses too.

## PS link and ROM menu (M1b stage C)

The PS is a helper only. Everything it does for a system goes through one
narrow link, shared by every system (`Platform.Ps.Link`, the PS side in
`platform/ps/pl.zig`):

- **PS → PL, EMIO GPIO 0 to 31** (`PS_GPIO`): bits 0 to 15 system-specific
  live inputs (CHIP-8 keys), and a byte-wide write bus: data byte (16 to
  23), strobe toggled once per byte (24), address-byte flag (25), hold
  (26). Three address bytes set a 24-bit address, data bytes write and
  advance it. Synchronised bit by bit in the system clock domain; the PS
  waits 2 µs around each strobe edge, so this is the platform-boundary
  clock crossing (one per system).
- **PL → PS, EMIO GPIO 32 to 63** (`PS_IN`, registered): a byte channel
  for a system's text output (byte in bits 24 to 31, a toggle in bit 5
  per byte; the Game Boy's serial port, passed to the desktop as
  `serial: …` lines; side track A), the debounced
  buttons, the switches and a system ID (`systemChip8` = 1,
  `systemGameBoy` = 2, `systemNes` = 3 (M3 step 3.8); 0 = no system
  ID), so one PS app serves every system. Reserve new IDs here first.
  Reserved: 4 = `systemM0e`, side track C's DDR test top (no menu entry;
  its byte channel carries the test's result bytes, "DDR path" below).
  Reserved 2026-10-04 (planning): 5 = `systemA800xl` (M4, Atari 800XL,
  menu folder `/a800xl`), 6 = `systemGbc` (side track I phase 3, Game
  Boy Color, menu folder `/gbc`; `gb.bit` keeps 2).
  Reserved 2026-10-07: 7 = `systemGameGear` (M5, Sega Game Gear, menu
  folder `/gg`; docs/systems/gamegear.md).
  Reserved 2026-10-11: 8 = `systemSms` (M6, Sega Master System, menu
  folder `/sms`; docs/systems/mastersystem.md).
  Reserved 2026-10-15: 9 = `systemSnes` (M7, Super Nintendo, menu
  folder `/snes`; docs/systems/snes.md; later chip bitstreams keep 9).
  Bits 6 and 7 are the system's own flags (0 if unused). Reserved
  2026-10-04 (side track I): bit 6 on the Game Boy = cartridge RAM
  written since the PS last cleared the flag (battery saves).
  Reserved 2026-10-09 (M5): bit 6 on the Game Gear likewise (cartridge
  RAM written; "Gg.Save", the Game Boy's link map and read-back).
  Reserved 2026-10-15 (M7): bit 6 on the Super Nintendo likewise
  (cartridge SRAM written).
  Reserved 2026-10-15 (side track O, input movies): bit 7 on every
  system = the frame toggle, flipping at each of the core's frame starts
  (`Platform.Ps.FrameInput`), 0 on a system without it or built with
  `MOVIES=0`; no system used bit 7 before.
- **Address map:** below 0xF00000 the system's own (CHIP-8: RAM at
  0x000000 to 0x000FFF, `Chip8.Loader`; Game Boy: cartridge ROM at
  0x000000 to 0x03FFFF, `Gb.Loader`, until side track I's phase 2 (since
  then the ROM is in DDR, written there by the PS: the system table's
  `ddr_addr`); NES: the `.nes` file from 0x000000,
  routed by file offset into header, PRG and CHR, `Nes.Loader`; Atari
  800XL (M4 step 4.8, `A800.Loader`): the program file from 0x000000
  (XEX, CAR or raw cartridge, told apart by the PL), and the keyboard's
  eleven bytes at 0x100000 to 0x10000A, written by the PS while the core
  runs; all
  written only while held). Reserved 2026-10-04 (side track I, battery
  saves; `Gb.Loader`): Game Boy cartridge RAM at 0x100000 to 0x11FFFF
  (written only while held; 32 KiB used, 128 KiB from phase 2), its
  read window at 0x120000 to 0x13FFFF (a data byte written at
  0x120000 + n, any value, any time, reads byte n of cartridge RAM onto
  the byte channel), and the Game Boy's save control at 0x140000 (bit 0:
  the byte channel carries those reads instead of the serial port) and
  0x140001 (any write clears the dirty flag). Reserved 2026-10-05 (side
  track I phase 2, MBC3's clock): 0x140010 to 0x140019, the clock's ten
  start bytes (live S M H DL DH, then the latched five; written while
  held), and 0x140020 to 0x140029, reads of them onto the byte channel
  (any time; 0x140020 takes a snapshot the others return). Reserved
  2026-10-11 (side track L, save states, L.6; `Gb.Loader`,
  "Platform.SaveState"): 0x140050, the state mode (bit 0: freeze the core
  for state access once it is paused and at rest; cartridge RAM then
  takes writes as while held), and the state's windows: a data byte
  written at 0x150000 + n writes byte n of the state (only while
  frozen), one written at 0x160000 + n (any value) reads byte n onto the
  byte channel (with the save control's bit 0). The state's bytes
  (L.6c, 2026-10-12: loading through the CPU): the state table
  (`Gb.StateTable`, read only) from 0x0000, the status at 0x1F00 (1
  while frozen; any time), the ROM overlay's end address and arm at
  0x1F10 to 0x1F12 (written), HRAM 0x1000 to 0x107F, VRAM banks 0 and 1
  at 0x2000 and 0x4000, the ROM overlay's 2 KiB at 0x6000 to 0x67FF
  (written), WRAM 0x8000 to 0xFFFF; cartridge RAM through its own
  windows. The core freezes where a state can be taken
  (`Gb.Core.restoreable`), or at once while held; a load holds it,
  writes the memories and the restore routine
  (`platform/ps/gb_restore.zig`) and releases it. Only tops built with save states
  (`WithStateHooks`: the Arty's) decode them. Side track N (written
  2026-10-15): the Game Gear and Master System (`Gg.Hooks`) use the same
  mode address and windows; their state space has the table
  (`Gg.StateTable`, 0x140 bytes) from 0x0000, the status at 0x1F00, the
  overlay's end and arm at 0x1F10 to 0x1F12, VRAM at 0x2000 to 0x5FFF,
  the overlay at 0x6000 to 0x67FF and RAM at 0x8000 to 0x9FFF; cartridge
  RAM through the battery saves' windows. Reserved 2026-10-15 (side
  track N, N.5; `A800.Hooks`): the Atari 800XL's state mode at
  0x140050, the read-back control at 0x140000 (bit 0: the byte channel
  carries the state's reads; the 800XL has no battery saves), and
  128 KiB windows: a data byte written at 0x150000 + n writes byte n of
  the state (frozen only), one written at 0x170000 + n reads byte n. Its
  state space: the table (`A800.StateTable`, 0x80 bytes) from 0x0000,
  the status at 0x1F00, the overlay's end and arm at 0x1F10 to 0x1F12,
  the overlay at 0x6000 to 0x67FF, RAM's 64 KiB at 0x10000 to 0x1FFFF.
  Reserved 2026-10-16 (side track N, N.8; `Nes.StateHooks`, the Arty's
  top only): the NES's state mode at 0x140050, the read-back control at
  0x140000 (bit 0: the byte channel carries the state's reads; no
  battery saves) and the 128 KiB windows at 0x150000 (write, frozen only)
  and 0x170000 (read), as the 800XL's. Its state space: the table
  (`Nes.StateTable`, 0x100 bytes) from 0x0000, the status at 0x1F00, the
  overlay's end and arm at 0x1F10 to 0x1F12, the overlay at 0x6000 to
  0x67FF, RAM at 0x8000 to 0x87FF, nametable RAM 0x8800 to 0x8FFF, OAM
  0x9000 to 0x90FF (read only), MMC5's ExRAM 0x9400 to 0x97FF, CHR RAM
  (8 KiB) 0xA000 to 0xBFFF, PRG RAM's 64 KiB at 0x10000 to 0x1FFFF.
  Reserved 2026-10-16 (side track N, N.10; `Chip8.StateHooks`, the
  Arty's top only): CHIP-8's state mode, read-back control and windows as
  the NES's (0x140050, 0x140000, 0x150000, 0x170000); the direct path, no
  restore routine: the table (`Chip8.StateTable`, 0x40 bytes, read from
  and written into the frozen core's record) from 0x0000, the status at
  0x1F00, RAM's 4 KiB at 0x2000 to 0x2FFF, the framebuffer's 256 bytes
  (32 rows of 8; a row is written with its last byte) at 0x3000 to
  0x30FF.
  Reserved 2026-10-23 (M7, step 7.14b; `Snes.StateHooks`, the Arty's
  top only): the Super Nintendo's state mode at 0x140050 and read-back
  control at 0x140000 (bit 0, shared with its battery saves' reads), and
  512 KiB windows (0x150000 and 0x170000 are too small, and its pad 2
  is at 0x160000): a data byte written at 0x200000 + n writes byte n of
  the state (frozen only), one written at 0x300000 + n reads byte n. The
  direct path: the chain (`Snes.Core.CoreRegs`, 1101 bytes, shifted a
  byte per access, read or written once round in order) from 0x00000,
  CGRAM 0x01000 (512 bytes, a colour with its high byte), OAM's low
  table 0x01200, the DMA registers 0x01400 (128), the DSP's samples
  0x01500 (512), the status 0x01F00 to 0x01F03 (frozen, the chain's
  size, the version), ARAM 0x10000, VRAM 0x20000 (64 KiB, low chip at
  even bytes), WRAM 0x40000 (128 KiB); SRAM through its own windows.
  From 0xF00000 the shared range: the menu's text overlay (0xF00000, 40 × 30
  cells) and the control register (0xF01000: bit 0 shows the overlay,
  bit 1 pauses the core, keeping its state). Reserved 2026-10-11 (side track L,
  agreed with K): the settings registers, `PsSettings`, a decoder of
  their own: 0xF01001 the volume (0 silence to 10, unity, the reset
  value; above 10 counts as 10) and 0xF01002 the emulation speed in
  quarters (2 = 50 % to 8 = 200 %, clamped; reset 4 = 100 %).
  Reserved 2026-10-15 (side track O): 0xF01003, the movie register
  (`PsMovie`, `Platform.Ps.FrameInput`): bit 0 frame-synchronous input
  (the input word latched at each frame start while a movie records or
  plays; reset 0, as today); bits 1 to 7 reserved. Reserved 2026-10-16
  (side track O, O.3): the control register's bit 2, a frame-advance step
  (`frameAdvance`, `Platform.Ps.FrameInput`): written with bit 1 set, it
  runs the paused core to its next frame start and pauses it there again
  (a write, not a level: the bit reads back as nothing); bits 3 to 7 of
  0xF01000 stay reserved.
  Reserved 2026-10-17 (side track O, O.4): 0xF01004, the capture
  register (`Platform.Video.Capture`): a write of a sequence number (1
  to 255) asks for a screenshot; the core's next whole frame goes to DDR
  from 0x10000000 (below) through HP0's write channels, then a trailer
  word with the sequence number, status, size and pixel count.
  Reserved 2026-10-17 (side K, M7's request): the key word is 32 bits
  (`keymap.Word`); bits 0 to 15 stay `PS_GPIO`'s input word, and a system
  with more buttons names a link address (`menu.System.keys_hi`) where the
  PS writes bits 16 to 31 on each change, two bytes, low first. The
  SNES's: 0x160000-0x160001 in its link map, pad 2 (bits 0 to 11 in pad
  1's order: key word bits 16 to 27; bits 12 to 15 zero). The desktop
  sends such a word in the protocol's wide key frame (`protocol.zig`).

No AXI: PLAN.md's original stage C (AXI-Lite → BRAM) would need a second
clock domain or exporting the pixel clock to the PS, and ROMs up to a few
hundred KiB load in well under a second over the GPIO bus (4 µs per byte).
Cartridges larger than BRAM need DDR, which is a separate step (an AXI HP
master in the PL) whichever way the PS writes.

**Text overlay** (`Platform.Video.TextOverlay`): 40 × 30 cells of 8 × 8
glyphs (`Platform.Video.Font8x8`, public-domain font8x8) doubled to
16 × 16, one byte per cell (bit 7 inverse video), on the 640 × 480 pixel
stream before VGA and HDMI. Two clocks of latency (character RAM, then
font ROM), applied to timing and colour alike, so a system's picture is
unchanged while the overlay is hidden. One RAMB18 for the cells, one for
the font.

**PS app** (`platform/ps/relay.zig`, bare metal in OCM): the J14 relay
(`protocol.zig`, for `make keys` / `make load`) and the ROM menu in one
polled loop. The menu (`menu.zig`: pure state, tested on the desktop)
reads the SD card (`sd.zig`: SDHCI driver for SD 0, PIO, 1-bit bus at
25 MHz; `fat.zig`: FAT12/16/32 with long names, MBR or none), lists the
system's folder (`/chip8`, any case; else the root) and loads the chosen
ROM over the link: hold, data, release. Vivado's `ps7_init.c` is linked in
without its silicon revision 1.0 and 2.0 tables (`tools/ps7-trim.sh`, side
K 2026-10-18: both boards are 3.x; 6.6 KiB of the OCM's 192 KiB).

**System table** (`menu.zig`, `systems`; side track A): per system ID its
name, folder on the card, file endings, load address, maximum size,
whether to clear the rest of the area after the file, and an optional
check of the file's first block (the Game Boy's: supported MBC, not
Color-only). `load.zig` does the loading for every entry, streaming the
file a sector at a time (fat.zig's `Reader`; a 256 KiB cartridge is
bigger than OCM). A new system: reserve its ID above, report it on
`PS_IN`, add one table entry, decode its address map in the PL. The same
map serves `make load` (`L<6 hex addr><4 hex len>`, the 24-bit form of
the relay's load command; the 3-hex form stays for CHIP-8). A system
whose ROM lives in DDR gets it with `W<8 hex addr><6 hex len>` (raw bytes
into DDR), and the relay can copy any DDR range onto any link range with
`Y<8 hex DDR addr><6 hex link addr><6 hex len>` (side track D: the NES's
header and CHR after its file is in DDR, as the menu's `ddr_link`; L
blocks overran the relay at 2 Mbaud). Commands: `platform/ps/protocol.zig`.

Pausing: the control register's pause bit is the system's to honour.
CHIP-8 stops starting instructions (`ciRun`); the Game Boy holds its dot
enable low, so no M-cycle starts and all its state stays (checked in
`gb-lockstep`: frozen, then identical to a run without a pause). The NES
does the same with its dot enable (`Nes.Top`).

Without a card reader, `make sd`
writes a card image made on the desktop (`tools/mksd.sh`) through the
relay (`platform/host/sdimage.zig`; the S/D/Z/O commands of
`protocol.zig`; `sd.zig` writes with CMD24 and every block is read back).

**Battery saves** (side track I; `platform/ps/save.zig`, docs/systems/
gameboy.md "Side track I"): the system table's `save` hook gives a
ROM's save size and its RAM's place in the link map. The save file,
`/saves/<folder>/<path>/<name>.sav`, is made at full size by `make sd`
(`link --save-slots` asks the same table), loaded into the system's RAM
while held, and written back in place (only blocks that changed; the FAT
is never written) after 2 s without RAM writes (`PS_IN` bit 6), when the
menu opens and before anything else is loaded. The RAM comes back over
the byte channel while the game runs. `make saves-pull` copies `/saves`
to the desktop through the relay's block read (R), and `make sd` does
that first, so a new image keeps the saves. Buttons: U/D move (held:
repeat), R or C open/load, L up a folder or back to the game; in a game,
holding the system's menu combination for a second opens the menu (the
table's `menu_combo`: CHIP-8, C alone, only a modifier for CHIP-8 keys;
Game Boy, Up and Down together, which a real D-pad can't press, because
C is its A button). The card is read again each time the menu opens.

**Boot splash** (side track J, step 5; `platform/ps/splash.zig`,
docs/brand.md): before the menu first opens, `/systems/boot/<name><ext>`
(`gb.gb`, `nes.nes`, `chip8.ch8`) is loaded like a ROM, with no save,
and runs with the overlay hidden; any input, once released, pauses it
and opens the menu. No file: the menu as before. After a Systems-menu
switch too, unless `[global] splash_on_switch = off` in `keymap.conf`
(on by default since 2026-10-08).

## Package layout

| Package | Contents |
|---|---|
| `platform` | `Platform.Domains`, `Platform.Clocks` (clock wizard, reset synchroniser), `Platform.Video.{VgaTiming,Oled,Scaler,ScanConvert}`, `Platform.Bus.I2c` (I2C master: register write/read transactions, also for the ADAU1761 later), `Platform.Video.Adv7511` (init sequencer, HPD, status; ZedBoard-specific) with its register table in `Platform.Video.Adv7511.Table`, `Platform.Video.Ycbcr422` (24-bit RGB → BT.601 YCbCr 4:2:2, chroma averaged per pixel pair), `Platform.Video.FrameBuffers` (three frames in one block RAM for a core with its own frame rate), `Platform.Input.{Sync,Debounce,Keypad,SnesPad}`, `Platform.Output.Leds` (LED register, debug-byte select, heartbeat), `Platform.Audio.*`, `Platform.Ps.Link` (the PS link: write bus, control register, PL → PS word), `Platform.Video.TextOverlay` with `Platform.Video.Font8x8` (the menu's text screen), `Platform.Board.Zedboard` (HDMI bus wiring, output registers, ODDR clock forwarding, open-drain pins), `Platform.Sim.{Frame,Oled,I2c,Overlay}` (simulation only: VGA decoding to PNG frames; SPI receiver and SSD1306 display-RAM model; I2C register-file slave on the bus wires; the overlay's reference picture and PS link scripts) |
| `platform/ps/` | Bare-metal Zig for the PS (Cortex-A9, OCM, no OS): `relay.zig` (the app: USB-UART relay and ROM menu), `hw.zig` (registers, timer, UART, GPIO), `pl.zig` (PS side of the PS link), `menu.zig`, `sd.zig`, `fat.zig`, `protocol.zig`, `link.ld`; the USB keyboard host (`usb.zig`, `usb_hw.zig`, `usb_keys.zig`; desktop fake `usb_fake.zig`, fixtures `usb_fixtures.zig`; side track E); the key maps shared with the desktop (`keymap.zig`, generated `input_codes.zig`; side track B, moved here by E) |
| `platform/host/` | Desktop side of the board link (Zig): evdev keyboards/gamepads → J14 serial, ROM loading (`make keys`, `make load`); the key-map file and remap utility (`remap.zig`; `make keymap`, side track B), with the maps from `platform/ps/keymap.zig` as the module `keymap` |
| `platform/sim/` | Layer 4 harness (Zig, plus the C++ shim `vtop.cpp`), shared by every system; `ps.zig` plays PS link scripts through `platform/ps/pl.zig` |
| `boards/sim/` | Verilator-only HDL: the clock-wizard stand-in, an `ODDR` model (found through `-y boards/sim`), Clash lint waivers |
| `boards/<board>/` | Constraints and Vivado scripts per board: `zedboard/` (in use); `arty-z7-20/` (only the reference master XDC so far; optional second board, see PLAN.md) |
| `systems/m0a`, `m0b`, `m0c`, `m0d` | Bring-up tops (`m0d`: test pattern on VGA and HDMI, ADV7511 status on the LEDs) |
| `systems/a800xl` (planned, M4) | the Atari 800XL: `A800.Spec`, ANTIC, GTIA, POKEY, core, top (docs/systems/atari800xl.md) |
| `systems/gg` (M5, done 2026-10-11) | the Sega Game Gear: `Gg.Spec`, VDP, PSG, core, tops (docs/systems/gamegear.md) |
| `systems/<name>` | `<Name>.Spec` (reference model), `<Name>.Types`, `<Name>.Core`, `<Name>.Top` (the `topEntity` for this board), tests, sim harness |
| `cpus/mos6502` | The shared MOS 6502 (M3 and later 6502 machines): `Mos6502.Spec` (cycle-level reference model; the caller supplies the bus; decimal mode and the LXA constant are parameters, `ricoh2a03` / `nmos6502`), `Mos6502.Types`/`Decode`/`Core` (the Clash core: a pure function per CPU cycle, RDY as a stall input; `coreRicoh2a03` / `coreNmos6502`). Named `cpus/` so that other shared CPUs (a Z80) can sit next to it; listed in `cabal.project`'s `packages`, compiled into a system's HDL through `HDL_SRC_EXTRA`. Beyond SingleStepTests (which never stall): SHA, SHX, SHY and TAS store the register without the `& (H+1)` when RDY held their dummy read (AccuracyCoin on the NES, VICE on the C64; approved for the shared core by the user, side track D, 2026-10-04) |
| `systems/sms` (M6, planned 2026-10-11) | the Sega Master System on the Game Gear's code (`systems/gg`, `cpus/z80`): its machine, display, tops (docs/systems/mastersystem.md) |
| `systems/snes` (M7, planned 2026-10-15) | the Super Nintendo: `Snes.Spec`, the PPU, the S-DSP, the core, the line-locked display, tops (docs/systems/snes.md) |
| `cpus/w65c816` (M7, planned 2026-10-15) | The shared WDC 65C816 (the SNES's 5A22, later the SA-1): `W65c816.Spec` (cycle-exact, the caller's bus, both modes) and `W65c816.Core` (Clash, a pure function per CPU cycle, a stall input), built like `cpus/mos6502`, which it does not change |
| `cpus/spc700` (M7, planned 2026-10-15) | The Sony SPC700 (the SNES's sound CPU): `Spc700.Spec` and `Spc700.Core`, built the same way |
| `cpus/z80` (M5, from 2026-10-07) | The shared Zilog Z80 (the Game Gear first; later the Master System or other Z80 machines): `Z80.Spec` (T-state exact reference model; the caller supplies the bus, one call per machine cycle with its length; undocumented opcodes and flags, MEMPTR, Q), and `Z80.Core` (Clash: one transition per machine cycle, the requests the spec's bus calls; `z80Tick` per T-state with WAIT; `make z80-hdl` checks it synthesises). Built like `cpus/mos6502`: in `cabal.project`'s `packages`, into a system's HDL through `HDL_SRC_EXTRA`. The Game Boy's SM83 is not this CPU and stays in `systems/gb/` |

## Conventions

- Haskell module names follow the directory structure. Each system's
  `topEntity` lives in `<Name>.Top` and carries a `Synthesize` annotation
  with port names that match the XDC exactly.
- Memories use `blockRam` / `blockRamFile`, which are inferred as Xilinx
  RAMB36/RAMB18. Avoid `asyncRam` for anything over a few hundred bits.
- The core's internal state is a single `mealy`/`moore` state record where
  possible. Pure transition functions are easy to unit-test.
- Every top-level output is registered before it reaches a pin.

## DDR path (side track C)

For anything larger than block RAM (cartridges above the NES's 256 KiB,
Game Boy MBC5, later consoles), cores read the PS's DDR through a Zynq
HP port.

- **`Platform.Mem.Ddr`** (`ddrReadPort`): an AXI3 read master on HP0
  behind a direct-mapped line cache (`2^k` lines of 32 bytes in one block
  RAM; k = 4 is one RAMB18) with sequential prefetch. A core asks for one
  address at a time and gets the 8-byte word holding it later: a hit the
  next clock, a miss at its beat of a 4-beat burst. `invalidate` empties
  the cache after the PS rewrote DDR; a sticky error flag reports AXI
  errors. Types in `Platform.Mem.Axi`; `Platform.Mem.Sim` is a
  simulation-only AXI slave with random timing (unit tests).
- **Clock:** the port runs in the system's own domain and the PL clocks
  the HP port with it (`HP0_ACLK` = the pixel clock): no crossing in the
  PL. DS187 v1.21 table 19: AXI interfaces up to 250 MHz, no minimum.
- **The PS writes DDR directly** (MMU off: strongly ordered, uncached, so
  a write is in DDR before the PS tells the PL to read). Buffers sit at
  0x00100000 and up (OCM is mapped low); the DDR test uses 0x01000000.
  0x00100000 to 0x0010FFFF is the USB host's (side track E, `usb.Dma`).
- **Board wiring:** `boards/zedboard/ps7_hp.tcl` (the shared `ps7.tcl`
  plus HP0, 64 bits) and the wrapper `m0e_zynq.v`, which ties the AR
  channel's fixed fields and the write channels. Until side track C
  merges, only M0e uses them.
- **Measurement** (`systems/m0e`, `make ddr-test`): `M0e.Tester` reads
  DDR through the port, keeps a checksum and a latency histogram; the PS
  (`platform/ps/ddrtest.zig`) writes the data, starts runs, reads the
  results over the PS link and prints them.

**Measured on the board (2026-10-04, `make ddr-test`, pixel clock
25.175 MHz = 39.72 ns per clock; latency is request to answer, as a core
sees it):**

| Access pattern | Latency (clocks) | To the core | CPU busy on DDR |
|---|---|---|---|
| Hit (line in the cache) | 1 | — | unchanged |
| Miss, sequential lines, prefetch one ahead (every word) | ½ of words 1, ¼ 3, rest 6 to 8; worst 27 (the first, cold) | 53 MB/s | a few 9 to 12 |
| Miss, one word per line or every other line (no prefetch benefit) | 14 to 16 (556 to 640 ns); worst 27 to 29 | 13 MB/s | tail to 20 |
| Random words in 1 MiB or 64 MiB | mostly 12 to 18 (peak 15 = 596 ns); worst 32 (1.27 µs) | 12 MB/s | worst 35 (1.39 µs) |

- **A miss costs about 15 clocks (0.6 µs), at worst about 35 (1.4 µs)**,
  with the CPU hammering DDR or not: the CPU's traffic only widens the
  tail by a few clocks. No run ever saw an AXI error, and every checksum
  matched the PS's.
- Sequential reading is bounded by one burst in flight: a line takes about
  15 clocks to arrive, so prefetching one line ahead gives about 53 MB/s;
  two outstanding bursts would roughly double it, if a core ever needs it.
- The PS writes DDR at about 18 MB/s (sequential, uncached, with read
  back) or 6 MB/s (random), against 0.25 MB/s through the GPIO link (4 µs
  per byte): 70 times faster for SD loads. From the desktop the serial
  line is the limit (98 KB in 4.3 s at 230400 baud, `make ddr-load`).
- **For cores:** a NES CPU cycle is about 14 pixel clocks (559 ns), so a
  miss is roughly one CPU cycle, worst two to three. Cartridge ROM in DDR
  works if the core can stall on a miss (the 6502's RDY, or holding the
  system's clock enable) and reads mostly sequentially (code, with
  prefetch: few misses). Data that is read at random and can't stall
  (PPU pattern fetches, two per 8 dots) belongs in BRAM, or needs a
  larger cache filled ahead of time.

## USB host (side track E)

A wireless keyboard's receiver on the board's USB host port drives the
systems and the menu without the desktop. Bare-metal Zig in the PS app;
no OS, no soft core. Plan: PLAN.md, "Side track E".

- **Controller:** PS `usb0` at 0xE000_2000, a ChipIdea core with an EHCI
  host interface and an **embedded transaction translator** (UG585 v1.8.1,
  15.11): a full- or low-speed device on the root port needs no companion
  controller. Its QHs carry Hub Addr = TTCTRL.TTHA (0), EPS = the port
  speed (PORTSC1.PSPD), max packet ≤ 64, the C flag on control endpoints,
  and for interrupt endpoints an S-mask and C-mask as for a hub's TT
  (15.11.6, 15.11.7). The port enables after every reset and PSPD gives
  the speed (15.11.5). **Not as the TRM says:** 15.11.4's self-timed port
  reset didn't happen on the board (PR still 1 after 50 ms), so the
  driver ends it itself as in EHCI (50 ms, then PR = 0). And HCH reads 0
  while the controller is idle (USBMODE.CM = 00, after power-on), so
  "running" means host mode with RS set.
- **PHY and pins:** ULPI on MIO 28 to 39, already muxed to usb0 by the
  ZedBoard `ps7_init` (L1_SEL, LVCMOS18; DIR, NXT and the 60 MHz clock as
  inputs), USB0's AMBA clock on (APER_CLK_CTRL bit 2). The PHY registers
  are reached through the ULPI viewport. PORTSC1.PP only drives an EMIO
  signal: Vbus comes from the PHY, which enables the board's 5 V switch
  when OTG control has DrvVbus and DrvVbusExternal set (as Linux's
  `ulpi-phy` driver does for `drv-vbus`, the Zynq device trees' setting).
  The PHY keeps its registers across `make prog`, so the driver turns
  Vbus off, powers the port, waits 250 ms and turns Vbus on: a device
  left attached by the last run attaches again where the freshly reset
  controller hears it.
- **Memory:** QHs, qTDs and buffers in the app's own OCM: with OCM mapped
  low, other bus masters see it at the same addresses (UG585 table 4-1),
  and with the MMU and caches off nothing needs flushing.
- **Driver** (`platform/ps/usb.zig`, `Host(Bus)`, step 2): a polled state
  machine the relay loop calls between UART bytes, never blocking:
  controller halt (if left running) and reset, host mode, ULPI wake-up if
  needed, the PHY's IDs (TUSB1210 = 0451:1507), Vbus on, run, port power;
  then the port: connect, 100 ms debounce, port reset (the controller's
  timer), speed from PSPD, 10 ms recovery, disconnect. A PHY without its
  60 MHz clock (in reset, unpowered) shows as "no PHY" and is retried
  every second. The state goes to the UART (`usb: ...` lines) and to the
  menu's third row (`menu.Menu.usb`).
- **Enumeration** (step 3): control transfers through one QH, alone in
  the async ring as its head, with setup, data and status qTDs (`usb.Dma`,
  in OCM). The QH's endpoint words change (address, EP0 size) only with
  the async schedule off; the same off/on cycle cleans up after a stall
  or a timeout. Sequence: 8 bytes of the device descriptor, SET_ADDRESS
  1, 2 ms, device descriptor, configuration (head, then whole, through
  the shared parser), boot keyboard (class 3/1/1), SET_CONFIGURATION,
  SET_PROTOCOL(boot), SET_IDLE(0) (a stall tolerated). Failures retry with
  a port reset, three times.
- **Keys** (step 4): the keyboard's interrupt-IN endpoint through the
  periodic schedule: a 32-entry frame list, the QH in every 8th entry
  (the largest power of two up to bInterval), split masks S = µframe 0,
  C = µframes 2 to 4 (EHCI 4.12.2), DTC = 0 so the QH keeps the toggle; a
  fresh qTD after each report. `usb_keys.zig` maps HID usages to Linux
  key codes (our table, HID Usage Tables §10) and a boot report to the
  system's key word through the same `keymap.Lookup` as the desktop; the
  relay ORs it with the desktop link's keys. `/keymap.conf` on the card
  (the desktop file's format) replaces the defaults when the card is
  mounted; a bad line shows on the menu. All DMA memory (frame list,
  QHs, qTDs, buffers) is in DDR at 0x00100000 (64 KiB reserved): OCM is
  nearly full (about 7 KB free below the stacks).
- **Menu from the keyboard** (step 5): `usb_keys.Keyboard.nav` gives the
  held navigation inputs: the arrows, Enter/keypad Enter/Space (open),
  Backspace (back) on any map, plus the system's own D-pad and A/B (Game
  Boy, NES) or keypad 2 4 6 8 5 (CHIP-8). The relay feeds them into a
  second `menu.Buttons`, so edges and repeat match the board buttons.
  The `[global] menu` key (keymap.conf; default `KEY_ESC`) opens the menu
  from a game and closes it. While the menu is open, and after it closes
  until every key is up, the USB keys are held back from the game.
- **Desktop tests** (`make test-zig`): `platform/ps/usb_hw.zig` holds the
  register map, the QH and qTD layout and the descriptor parser shared by
  the driver and its fake; `usb_fake.zig` models the controller (reset,
  host mode, ULPI viewport with a TUSB1210 register file, port power,
  connect, reset, the async ring and the periodic frame list walked in a
  DMA arena, one transaction per QH per micro-frame or frame) and one
  emulated keyboard built from a fixture. Fixtures (`usb_fixtures.zig`)
  are two real receivers' descriptors captured from sysfs: EP0 of 64 and
  8 bytes, boot reports of 8 bytes, report-protocol reports of 8 and 18.
  The fake counts every rule a driver breaks (TRM, EHCI, USB 2.0 timing:
  100 ms connect debounce, 10 ms reset recovery, 2 ms after
  SET_ADDRESS).

## Resource budgets (2026-10-04, after side track E)

Where the design stands against its limits, measured on the bitstreams
and PS app built for side track E's merge (`make bit`, Vivado's
`utilization.rpt` and `timing_summary.rpt`; `readelf -lW
build/ps/relay.elf`). Update the numbers when a change moves them.

| Resource | Use | Limit | Note |
|---|---|---|---|
| OCM (PS app at run time) | about 155 KB: image 89 KB (code 49, read-only data 26, data 12 with `ps7_init`'s tables), .bss about 52 KB, stacks 17 KB | 192 KB | about 37 KB free (side track F: stack 64 → 16 KiB, then PCAP code); the link fails if it doesn't fit (link.ld `ASSERT`) |
| Boot image (side track F) | 89 KB (`BOOT.BIN` 95 KB with its header), `ps7_init` and PCAP included | 192 KB (UG585 6.3) | checked by `tools/check-boot-image.sh` in `make boot-image` |
| Block RAM, NES | 105 tiles (Arty, 2026-10-08) | 140 | 75 %: since side track D PRG ROM is in DDR (up to 512 KiB), CHR 256 KiB in block RAM; was 136.5 |
| Block RAM, Game Boy / CHIP-8 / M0e | 83.5 / 5 / 1 tiles | 140 | |
| LUTs, NES / Game Boy / CHIP-8 / M0e | 41 % / 39 % / 7 % / 4 % | 53 200 | one system in the PL at a time |
| Setup slack, Game Boy | WNS +1.527 ns of 39.72 ns | | worst path 37.5 ns, 40 logic levels into a block RAM address |
| Setup slack, NES / CHIP-8 / M0e | +9.450 / +16.036 / +23.602 ns | | room |
| EMIO GPIO (PS link) | 64 of 64 | 64 as configured | a new PS-to-PL signal needs a wider EMIO in the shared PS7 configuration |
| PS link loading | 0.25 MB/s | | DDR (side track C) about 70× faster |
| J14 UART | 230400 baud (about 23 KB/s) | the user's bridge | keys, logs; ROMs come from the SD card |

- **OCM.** The PS app runs from the 192 KB of OCM mapped low (link.ld):
  image, .bss, then 16 KiB of stack (64 KiB until side track F; frames
  are up to 2 KiB) and 1 KiB for the exception handlers. `_start` paints
  the stack and the relay reports each new KiB of its high-water mark
  over the UART ("relay: stack used N of 16384 bytes"). Big items
  measured with `@sizeOf`: `menu.Menu` 19.3 KB, the directory entries
  behind it (`fents`) 22 KB, the `keymap.conf` buffer 4 KB, the key map
  and its lookup table about 3 KB. Room to win back: the menu's tables,
  which can live in DDR once `ps7_init` has run, as the USB host's DMA
  memory does (0x00100000).
- **Start-up (side track F).** The same app is loaded two ways: by xsdb
  after the Tcl `ps7_init` (`make prog`), or by the boot ROM from
  `BOOT.BIN` on the SD card (`make boot-image`) with nothing set up. It
  runs Vivado's `ps7_init.c` (linked in, `platform/ps/ps7.zig`) when DDR
  is still in reset, and `ps7_post_config` and the level shifters only
  when the PL is configured (devcfg `PCFG_DONE`). `make prog
  PS_INIT=app` tries the boot ROM's case over JTAG.
- **DDR regions in use:** 0x00100000 to 0x0010FFFF the USB host;
  0x00200000 to 0x002FFFFF the PS app's data in DDR (side L, K agreed,
  2026-10-11: `.ddr` in `platform/ps/link.ld`, the menu and the key map's
  buffer; never zeroed at start; cached since M1);
  0x00300000 to 0x003FFFFF the PS app's `.bss` (side K, M1, 2026-11-02:
  `bss_ddr` in `link.ld`, cleared by `main` after ps7_init, cached);
  0x01000000 and up the DDR test (M0e, `make ddr-load`); 0x18000000 to
  0x187FFFFF a bitstream on its way into the PL (side track F,
  `pcap.zig`; swapped words, 4 MB for the 7020); 0x10000000 to
  0x1004FFFF a screenshot (side track O, O.4, `Platform.Video.Capture`:
  up to 320 x 240 pixels, 4 bytes each, R G B 0, then its trailer
  word; written by the PL through HP0, read by the PS).
  **Reserved 2026-10-04 (planning): 0x02000000 to 0x0FFFFFFF, the
  running system's media** (224 MiB): only one system is in the PL at a
  time, so one region serves all; each system documents its own layout
  in its docs/systems file (Game Boy ROM from the start, up to 8 MiB,
  side track I; the 800XL's XEX file and later ATR images, M4). The M0e
  test region (0x01000000) stays below it. In use since 2026-10-05: the
  Game Boy's cartridge ROM at 0x02000000 (`Gb.Top.romBase`, through HP0;
  `gb_zynq.v` with `ps7_hp.tcl`). Since 2026-10-08 the NES's too: the
  whole `.nes` file from 0x02000000, PRG ROM at +16 (`Nes.Top.romBase`,
  side track D; `nes_zynq.v` with `ps7_hp.tcl`).
- **Block RAM.** The NES holds CHR (256 KiB) and three frame buffers in
  BRAM; since side track D (2026-10-08) its PRG ROM is in DDR, the core
  waiting for a miss (`Nes.Core.prgDdr`): fine for data the core can wait
  for (the CPU's reads, with a stall on a miss of about 0.6 µs), not for
  the PPU's pattern fetches (BRAM, or a cache filled ahead). See "DDR
  path".
- **Timing.** The Game Boy core does each step in one pixel clock, so its
  worst path is long (docs/systems/gameboy.md, "Clocking"); logic added
  there needs pipelining first. The other systems have wide margins.
- **The workstation.** 16 cores and 30 GB, shared by every session:
  memory is the limit. Vivado needs up to 14 GB (builds run under a 14 GB
  memory limit), an 800XL Clash run about 10 GB, a lockstep suite up to
  8 GB (capped). One heavy job at a time across all sessions, `make ci`
  or `make bit` (the queue, CLAUDE.md working rule 10), at most 2 Clash
  runs at a time (`tools/clash-slot.sh`); a `make bit` takes 4 to 15
  minutes, `make ci` (quick tier) about an hour (2026-10-08).
