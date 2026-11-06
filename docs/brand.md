# Brand and boot splash

The design for side track J's boot splash (planned as "side track H";
renamed J, H being `zig build`), settled by the user on
2026-10-06 and 2026-10-07 in planning (claude.ai). Two reference pages
hold every number here, and where this file and a page disagree, the page
is what the user heard and saw:

- `demos/rota/reference/audition.html`: the chime on every machine, and
  the 64 × 32 picture as CHIP-8 draws it.
- `demos/rota/reference/tile-lab.html`: the picture for tile machines,
  at each machine's own resolution, with per-frame cost counters.

Beats are counted from 0 in this file (the pages show them from 1).

## Name and idea

**Rota** (confirmed 2026-10-06): an old word for a round, a melody sung
against a delayed copy of itself, and Latin for wheel. In synchronous
hardware, as in functional programming, everything is a pure function plus
a delay; a ring counter, a shift register looped back on itself, is a round
in hardware. The repository keeps its name; the splash carries "ROTA".

The design is built bottom-up: first drawn for CHIP-8 (64 × 32, one bit,
one fixed-pitch buzzer), then redrawn for tile hardware, where everything
is placed once while the screen is dark and animated only with palette
writes, a few tilemap writes and sprites.

## Palette

| Role | Colour | Meaning |
|---|---|---|
| Ink (background) | #14111C | violet-black, a nod to Haskell's purple |
| Shadow | #453A62 | Haskell's logo purple; unlit ring segments |
| Amber | #F7A41D | Zig's orange, and CHIP-8's amber; the lambdas and the lead |
| Teal | #22A699 (bright #3CC7B9) | signal and fabric; the ring and the harmony |
| Violet | #9C8FD0 | the bass |
| Paper | #F4ECD8 | warm off-white; the letters and highlights |

Machines with fixed palettes use their nearest real colours (the tile
lab's `LOOK` table; summary under "Per-machine looks" below).

## The emblem (every machine)

- **Ring:** 8 segments, segment 0 at the top, clockwise, each 45° with a
  gap of about 8°: a pixel at angle `a` from the top is in the ring when
  `(a + 22.5) mod 45` is between 4 and 41, and its radius is between
  12.2/15.6 and 1 of the outer radius.
- **Lambdas:** four capital lambdas (Λ) pointing inward at 0°, 90°, 180°
  and 270°, mirror- and rotation-symmetric (four rotated lowercase λ would
  read as a swastika). With `k = outer radius / 15.6`: apex at 3.6k, legs
  to 10.6k spread ±4.39k (each leg aims at a ring gap), stroke half-width
  0.95k. A square core of half-size k.

## Rendition A: CHIP-8 (64 × 32, the audition page)

- **Layout:** the 32 × 32 emblem centred (x 16 to 47, outer radius 15.6
  about (15.5, 15.5)); the letters split around it as **RO ◯ TA**, bold
  6 × 11, rows 10 to 20, R at x 1, O at 9, T at 49, A at 57 (bitmaps in
  the page's `BIG` table).
- **Threads:** one per voice, 5 pixels thick, `y = cy + flip · A ·
  sin(2π x / L + φ + ω t)`, XOR-drawn, hidden inside the emblem's disc
  (radius 16.4). Lead A 7.5, L 70, φ 0, ω 0.55; harmony 10, 96, 2.2,
  −0.38; bass 5, 52, 4.1, 0.8 (cy 16). Each spins out over 1.5 beats from
  its figure entry (lead 0, harmony 1, bass 2); the lead flips at 3 and
  the harmony at 4 (`cos(π u)` over half a beat); a pulse runs along the
  bass's thread through beat 5.
- **Animation:** as in "The animation" below; dipped and unlit segments
  are dark (one bit), and the idle loop is a single lit segment circling.

## Rendition B: tile machines (the tile lab)

### Layout

- **Ring outer radius 64 pixels**, 48 on the 160 × 144 screens (Game Boy,
  Game Gear); centred on the screen.
- **Letters on tile boundaries:** large letters 2 × 4 tiles (16 × 32),
  small ones 1 × 3 (8 × 24) on the 160 × 144 screens. With the ring's
  left and right edges rounded out to tiles, O sits one tile left of the
  ring, R one tile left of O, T one tile right of the ring, A one tile
  right of T; vertically centred, rounded to a tile.
- **Letter style:** strokes along centre lines (the lab's `LETTERS` table),
  radius 2.1 (large) or 1.1 (small), chamfered at 45° like circuit
  traces; on large letters a groove of radius 0.55 is cut down the middle
  of each stroke. The O is an octagon broken at its corners (each edge
  shortened 1.3 pixels at both ends), eight segments like the ring. The A
  is a capital lambda with a fabric node (4 × 4, or 2 × 2) where its
  crossbar would be, so the wordmark reads "ROTΛ".
- **Background:** plain on every machine (decided 2026-10-07).

### The fabric

- **Grid:** trace rows and columns mirrored about the centre: a tile's
  centre at offset `o` from the screen centre carries a trace when
  `|o| ≥ b` and `(|o| − b) mod 32 = 0`, with `b = 4` when the centre falls
  on a tile edge (every machine here) and 0 otherwise. On a tile edge this
  gives two traces four pixels apart through the middle: **the bus**, the
  fabric's spine on each axis, which the bass rides into the core
  (decided 2026-10-07).
- Traces are drawn only in tiles with no logo pixel.
- **Tile drawing** (rows `ly`, columns `lx` within the tile): a horizontal
  trace on row 3, a vertical one on column 3; dotted (every other pixel,
  dim colour) at rest; a one-pixel solid line in the dim colour as a
  pulse's tail; a two-pixel solid line (rows or columns 3 and 4) in the
  bright colour as the pulse; a 3 × 3 node (2 to 4) where traces cross.
  On the Spectrum and 800XL, dim and bright are one colour and brightness
  is density alone.
- **Pulses:** 16 phase versions of each trace tile. A tile's phase is
  `(⌊(|dx| + |dy|) / 8⌋ + 5 · line) mod 16`, where `dx`, `dy` are its
  centre's offsets from the screen centre and `line` is its trace line's
  index from the centre (`round(|dy| / 32)` for a horizontal trace,
  `round(|dx| / 32) + 3` for a vertical one), so fronts spread outward as
  diagonals, staggered line by line, and the fabric stays mirror-symmetric.
  The step advances every 4 frames, plus a burst of up to 8 quick steps
  (one per 0.06 beat) from the crash; a tile shows level
  `(phase − step) mod 16`: 0 the pulse, 1 its tail, the rest dotted.
- **Moving it:** the tilemap never changes; each step redraws only the
  three phase versions whose look changes (the new pulse, its tail, the one
  returning to dotted), three shapes each (horizontal, vertical, crossing):
  9 tiles. Bitmap machines redraw the cells of those phases instead.

### The sparks

Each voice is one spark, entering a beat apart, each on its own path along
the fabric, moving at an even speed along the path's length. With the
**square** of traces around the ring (the first trace rows and columns at
least R + 2 from the centre: `x0, x1, y0, y1`):

| Voice | Figure (beats) | Path | Inversion (beats) |
|---|---|---|---|
| Lead | 0 to 1.5 | in from the left edge along the top (y0), down the right side (x1) to the centre's height | 3 to 4.5, the same path backwards |
| Harmony | 1 to 2.5 | in from the right edge along the bottom (y1), up the left side (x0) to the centre's height | 4 to 5.5, backwards |
| Bass | 2 to 3.5 | up the bus from the bottom edge to just below the ring | 5 to 6 (figure again): up the bus all the way into the core, on the crash |

- **Idle** (from beat 8.6): the three circle the square clockwise from the
  top centre, a third of the way apart, one loop per 4 beats.
- **Shape:** two 8 × 8 sprites. The head is a diamond of radius 3 with a
  white centre (radius 1); the trail is a diamond of radius 2 at least 5
  pixels behind it and one of radius 1 at least 9 pixels behind. Sprites
  sit behind the logo where they cross it. The Spectrum, with no sprites,
  recolours the cells the spark passes through.
- **Junction lights** (on every machine, decided 2026-10-07): one sprite
  per lit crossing, a 3 × 3 light, mirrored left and right, never on the
  square's top or bottom row. A crossing at column distance `c`
  (`round(|x − cx| / 32)`) on row index `r` is lit when
  `(c + 3r − step) mod 10 = 0`, with `step = ⌊3b⌋ +` roll hits so far
  `+` a crash burst of up to 10 (one per 0.06 beat), and
  `⌊2(b − 8.6)⌋ + 40` while idle.

## The animation (both renditions)

| Beats | Picture |
|---|---|
| lead-in (1.5 s) | black, while the monitor locks on; tile machines place every tile now, with no per-frame limit |
| 0 to 4 | the ring lights top-down from both sides, one stage a beat (stage s holds segments s and 8 − s, which share an odd or even parity, so tile machines need only five colour indices for the ring); the core is lit; letters dim |
| 4 to 5.5 | the roll: on each hit (4, 4.25, 4.5, 4.75, 5, 5.125, 5.25, 5.375) the stages of that hit's parity dip for 60 % of the gap to the next hit, so the flicker speeds up with the roll |
| 5.5 to 6 | the ring holds; the lambdas grow out of the core, ease-out (tile machines reveal them a tile at a time by tile-centre distance) |
| 6 (crash) | every segment flickers twice (bright, dark, bright, dark, 1/8 beat each), then stays lit; the core flashes white for a quarter beat; letters light at 6.4 |
| 8.6 on | idle: a bright wave runs down both sides of the ring, two stages a beat (tile machines; CHIP-8 circles one lit segment); waiting for any input |

## Score

D major, **150 bpm** (a beat is 0.4 s), seven chords: **D, Bm, G, Em, A,
D, D** (I–vi–IV–ii–V–I, the last held to beat 8.6). Notes as (start beat,
end beat, MIDI):

| Voice | Notes |
|---|---|
| Lead | (0, .5, F♯5 78) (.5, 1, A5 81) (1, 3, D6 86) (3, 3.5, B5 83) (3.5, 4, G5 79) (4, 5, E5 76) (5, 5.5, A5 81) (5.5, 6, G5 79) (6, 8.6, F♯5 78) |
| Harmony | (0, 1, D5 74) (1, 1.5, B4 71) (1.5, 2, D5 74) (2, 3, G5 79) (3, 4, E5 76) (4, 4.5, C♯5 73) (4.5, 5, A4 69) (5, 6, F♯4 66) (6, 8.6, A4 69) |
| Bass | (0, 1, D2 38) (1, 2, B1 35) (2, 2.5, G2 43) (2.5, 3, B2 47) (3, 4, E3 52) (4, 5, A2 45) (5, 5.5, F♯2 42) (5.5, 6, A2 45) (6, 8.6, D3 50) |

The figure (minor third up, fourth up) enters in the lead (beat 0), the
harmony (beat 1) and the bass (beat 2, tonally adjusted). The lead inverts
it (beat 3) and the harmony imitates the inversion exactly (beat 4). On
beat 5 the bass plays the figure once more, F♯2 A2 to D3, the lead's
opening pitch classes, landing on the crash.

**Consonance rule:** on every eighth-note position, each pair of voices is
a unison, third, fifth, sixth or octave (compound intervals reduced), or a
fourth between the upper two voices. The one allowed dissonance is the
lead's passing G5 at beat 5.5: a seventh over the bass's A2 and a ninth
over the harmony's F♯4.

**Drums** (one noise channel; a new hit cuts the last): kick 0, hat 0.5,
snare 1, hat 1.5, kick 2, hat 2.5, snare 3, hat 3.5; then one roll through
the V and I chords: sixteenths 4 to 4.75, thirty-seconds 5 to 5.875,
volume 3 rising to 14 of 15, noise pitch up six steps; crash at 6.

## Sound, per machine

Every machine: legato (consecutive notes are one phrase; the pitch
changes in place, never retriggered); the lead and harmony glide into each
note over 40 ms (at most a fifth of the note), scoop up 40 cents into a
phrase's first note, and add vibrato to notes of 0.3 s or more (±15 cents
at 5.8 Hz after 0.11 s; the last note ±20 cents after 0.2 s). Every bend
and vibrato step is one register write per frame, rounded to the chip's
real pitch steps.

| Machine | Lead | Harmony | Bass | Drums and volume |
|---|---|---|---|---|
| Game Boy | pulse 1, 25 % | pulse 2, 50 % | wave channel, 32-sample 4-bit sine | noise; volume constant through a phrase (a change needs a retrigger), the last note retriggered with a decaying envelope; roll noise 16384 to 65536 Hz |
| NES | pulse 1, 25 % | pulse 2, 50 % | triangle | noise, roll up the period table 508 to 128; software envelope in constant-volume mode, accented per note; every lead period below 256, so legato never rewrites $4003; the harmony's F♯4 (beat 5) needs t = 301, so $4007 is rewritten twice, on the first frames of F♯4 and of the A4 after it (step 1, 2026-10-07: the page's "every note has t < 256" is wrong for F♯4), and bends stay inside each note's 256-value range |
| Game Gear, Master System (SN76489) | tone 1 | tone 2 | tone 3; notes under about 109 Hz (B1, D2, G2, F♯2) an octave up | white noise at the chip's three fixed rates; software attenuation per frame; no periodic-noise bass (decided 2026-10-06) |
| Atari 800XL (POKEY) | channel 3, pure tone, 64 kHz clock | channel 4, same | channels 1 and 2 joined, 16-bit at 1.79 MHz | the drums borrow the bass pair a few frames at a time (no fifth channel), so the crash is a short burst |
| Super Nintendo (S-DSP) | voice 0, a flute-like BRR cycle (ADSR: 24 ms to 7/8) | voices 1 and 2, a string cycle 7 cents either side, panned apart | voice 3, a plucked cycle (at once to 1/2) | the audition page's GBA voicing (the user, 2026-10-18): noise voice (rate in FLG) and a swept-sine kick, per-frame GAIN from the Game Gear's curves; echo 64 ms, feedback 0.34 on the tonal voices; the final note fades by exponential GAIN after 45 % of it (GAIN written before ADSR1 hands the envelope to it); key-on only at a phrase's first note |
| CHIP-8 | the lead's rhythm on the buzzer: 3 to 12 frames a note, 30 for the last, starts rounded to a frame | | | no HDMI audio yet (the user will resolve it later, outside J) |

The audition page also has hypothetical versions for the C64 (SID), ZX
Spectrum (one-bit pin-pulse engine), GBA, Atari 2600, ColecoVision and
Intellivision.

## Per-machine looks (tile lab, `LOOK`)

| Machine | Background | Ring (unlit, lit) | Lambdas | Letters (dim, lit) | Fabric (dim, bright) | Notes |
|---|---|---|---|---|---|---|
| NES | #000000 | #3C1E80, #00A0A8 | #FCA044 | #24188C, #FCFCFC | #24188C, #00788A | targets: map to the project's generated 2C02 palette |
| Game Boy | ink | hidden until lit, teal | amber | hidden, paper | teal, amber | the four shades ink, teal, amber, paper |
| Master System | #000000 | #000055, #00AAAA | #FFAA00 | #000055, #FFFFAA | #000055, #005555 | 2 bits a channel |
| Game Gear | brand colours rounded to 4 bits a channel | | | | | |
| Atari 800XL | #0E0B14 | #3B2E5A, #2CB5A8 | #F2A23A | #2A2340, then a colour changed every scanline | #2A2340, one colour | |
| C64 (future) | #352879 | #6C5EB5, #70A4B2 | #B8C76F | #433A8F, #FFFFFF | #000000, #6C5EB5 | the VIC-II's 16 colours |
| ZX Spectrum (future) | #000000 | #0000D7, #00D7D7 | #FFFF00 | #0000D7, #D7D7D7 | #0000D7, one colour | attribute clash drawn in |

Sparks (head, trail): lead warm amber, harmony bright teal, bass violet,
in each machine's nearest colours.

## Measured in the tile lab (2026-10-07)

Junction lights on, plain background. Per-frame write budgets are
estimates, to be measured on each machine.

| Machine | Unique tiles | Most tilemap writes a frame | Fabric per step | Sprites (most, busiest line) | Notes |
|---|---|---|---|---|---|
| NES | 214 (114 / 114 split) of 256 | 16 / 64 | 9 tiles, 144 bytes | 12, 4 / 8 | no 16 × 16 area needs more than 3 colours |
| Game Boy | 150 / 256 | 92 / 150 | 9 tiles, 144 bytes | 8, 4 / 10 | the ring's palette effects done as tile swaps (DMG) |
| Master System | 96 / 448 | 16 / 96 | 9 tiles, 288 bytes | 10, 4 / 8 | |
| Game Gear | 67 / 448 | 8 / 48 | 9 tiles, 288 bytes | 8, 4 / 8 | |
| Atari 800XL | 218 of 128 characters | 16 / 40 | 80 bitmap cells | 10 (players, missiles), 4 / 8 | over in character mode, 5 colours on a row against 4: logo rows need a bitmap mode |
| C64 (future) | 225 / 256 characters | 16; 148 colour writes / 600 | 100 bitmap cells | 12, 4 / 8 | 8 sprites reused down the screen |
| ZX Spectrum (future) | (bitmap) | 152 attribute writes / 768 | 56 bitmap cells | (no sprites) | 8 cells need more than their two colours |

## Budgets

| Item | Limit |
|---|---|
| Game Boy ROM | 32 KiB, no mapper |
| NES cartridge | UxROM (mapper 2): CHR-RAM for the fabric's tile redraws |
| CHIP-8 program | 3584 bytes (0x200 to 0xFFF) |
| Sound stream per machine | 8 KiB target (about 600 frames) |
| Per-frame writes | the tile lab's assumed budgets until measured on each machine |

## The Game Boy as built (step 2, rendition B, 2026-10-07)

`demos/gb/rota.asm` is a stream player; `Rota.Gb` lays out the picture
and `Rota.Gb.Rom` the streams, with `replay`, the exact model of the
screen, which the layer 3 tests compare with the spec frame by frame.

- **Tiles:** 252 background tiles of 256 (blank, 27 fabric slots, 224
  logo tiles: every look of every logo cell over the chime and the idle
  loop, the ring's and letters' looks included), 29 sprite tiles. BG
  colour 0 is the background, so sprites behind the logo use the OAM
  priority bit (set on a sprite touching a logo pixel).
- **Two tilemaps** ($9800, $9C00): each frame shows the map the last
  frame showed, unless the other one last showed a closer picture or more
  than 16 cells change; the changes are then scheduled earliest-deadline
  first (a hidden map's change from the frame after it was last shown,
  due when it is next shown), so the roll's dips and the crash's flicker
  are LCDC writes. The idle loop shows map 0. A frame's tilemap writes run
  as generated code (`ld a, n` per tile value, `ld [nn], a` per cell).
- **The fabric** redraws its slots from a step table (three phases a
  step; a horizontal slot's rows 3 and 4 only), with `sp` popping the
  job list. At most one step a frame: where the crash's burst and the
  regular step fall in one frame, the second waits a frame (it has
  caught up by beat 6.5).
- **Budget (measured on the spec):** vertical-blank work after OAM DMA
  at most 640 M-cycles by the generator's cost model (a tilemap write 4,
  a tile value 2; a step 60, plus 34 a horizontal slot, 94 a whole one);
  the ROM's counters show it ends by line 152 (line 153 is left alone;
  LY reads 0 for most of it, so the overrun check reads STAT's mode).
- **Idle loop:** each part loops on its own period (ring wave 72 frames,
  sparks 96, lights 120, fabric 64); the sparks are drawn a loop (4
  beats) later, so their trails are there from the first idle frame.
- **Sprites:** a spark's trail is one 8 × 8 sprite; where its two
  diamonds don't fit (a corner), at most 1 pixel is left out.
- **On time:** every frame shows the designed picture in the frame it
  belongs to (`rota-brand`, "tile lab": no frame late through two idle
  loops).

## The NES's palettes (decided by the user, 2026-10-07)

Each 16 × 16 area takes one of four 3-colour background palettes, and no
four palettes cover every area of the tile lab's picture (the lab counts
colours per area, not which palettes they need). Of every choice of four
(`rota-stats nes-palettes`), the best is **{ring stages 1 and 2,
lambdas}, {stage 3, lambdas, core}, {stages 4 and 5, lambdas},
{letters, fabric bright, fabric dim}**, and its one loss is the fabric in
the 28 trace tiles (of 316) that share an area with ring, lambda or core
pixels: the sparks' square row loses 64 pixels just above and just below
the ring, the buses their last tile at the ring, a few tiles at the
sides. Everything else is exact. Keeping every trace would lose the core
and most of the lambdas instead.

## The NES as built (step 3, rendition B, 2026-10-07)

`demos/nes/rota.s` is a stream player on UxROM (mapper 2) with CHR RAM;
`Rota.Nes` lays out the picture, `Rota.Nes.Rom` the streams and
`replay`, the exact model the layer 3 tests compare with the spec.

- **Tiles:** with one background pattern table (the default, the user's
  choice: a toggle, 2026-10-07), 249 tiles of 256 (blank, 42 fabric
  slots, 206 logo tiles: the ring's looks are palette writes, so only the
  lambdas' reveal adds tiles); 15 sprite tiles in table 1.
  `make demos NES_SPLIT=1` puts the split build on the card instead
  (`rota-split.nes`, `ca65 -D SPLIT`): top half 149 tiles in table 0,
  bottom half and the sprites 165 in table 1; sprite 0, a dot behind a
  fabric pixel at (8, 115), tells the main loop when to switch the
  background table (in line 119's horizontal blank).
- **Colours:** the lab's NES look; colours it took from the common 2C02
  palette keep their entry (the lambdas $27, white $30, the sparks), the
  in-between ones take the nearest of the project's generated palette.
- **The NMI:** OAM DMA, the palette when it changes (32 bytes), the
  nametable writes (the reveal, at most 16), the fabric's step as
  generated code (`lda #` only where the byte changes), run once per
  pattern table it writes. At most 1146 cycles after DMA with one table,
  1668 with the split, against 1680 (2273 in the vertical blank, less
  the NMI's entry, OAM DMA and its tail).
- **Sprites:** as the Game Boy's, with the NES's rules (the earlier sprite
  wins); a sprite can't start left of the screen, so one that would is
  hidden until it can.

## The Game Gear as built (step 5, rendition B, 2026-10-08)

The tile lab's Game Gear: the 160 × 144 window, ring radius 48, small
letters, the base look rounded to 4 bits a channel (`Rota.Gg`,
`Rota.Gg.Rom`, `Rota.Sound.Gg`; the ROM `demos/gg/rota.s`, WLA-DX as
M5's test ROMs, 32 KiB with the Sega header, no mapper writes).

- **Colour RAM does the looks.** One background palette holds every
  role: 0 the background, 1 to 5 the ring's stages, 6 the lambdas, 7 the
  core, 8 the letters, 9 and 10 the fabric; the sprite palette 1 to 9 the
  sparks (head, trail per voice), a spark's centre and the lights. The
  ring's dips, flicker and idle wave, the core's flash and the letters
  lighting are colour writes, not tiles.
- **Tiles.** 164 background tiles: blank, 27 fabric slots (redrawn as the
  fabric steps, at most 9 a step, as on the Game Boy), and one tile per
  logo cell (136), redrawn only while the emblem grows (6 frames, at most
  8 tiles). Logo cells carry the name table's priority bit, so sparks and
  lights pass behind the logo's pixels and over its background. 42 sprite
  patterns at tiles 256 on; sparks in slots 0 to 5, lights 6 and 7.
- **Each frame**, from the frame interrupt (line 193) to the window's
  first line (24), the ROM plays one record of five streams (sound,
  colours, tiles, sparks, lights), each looping at its idle period; the
  worst frame takes about 15300 of the 21204 T-states (counted
  generously; a test checks every frame).
- **The lead-in** has the display off: the backdrop, the background
  colour.
- **Sound** (`Rota.Sound.Gg`): the page's `smsLevel` and `smsDrum` on the
  SN76489, bass notes under 109 Hz an octave up.
- **Gates.** Layer 3 (`rota-demos`, on M5's `Gg.Spec`): every frame equals
  the model from the lead-in through two idle loops; the audio golden
  from the PSG. Layer 4: `demos/layer4.sh gg`; `make ci-demos` plays it as
  the boot splash (`/systems/boot/gg.gg`; the PS app writes the Game
  Gear's cartridge word after the splash's load, as after a menu load).

## The Master System as built (M6, rendition B, 2026-10-12)

The tile lab's Master System: the whole 256 × 192 screen, ring radius 64,
large letters, the lab's own look (`LOOK.sms`, 2 bits a channel). Built
by M6 at the user's request, on the Game Gear's design: `Rota.Gg` and
`Rota.Gg.Rom` became `Rota.Sega` and `Rota.Sega.Rom`, one target per
machine (the Game Gear's emitted data byte-identical, its golden), and
`Rota.Sms`, `Rota.Sms.Rom` are the Master System's (the ROM
`demos/sms/rota.s`, WLA-DX, 32 KiB, the export header with its checksum,
no mapper writes; the chime is the Game Gear's, `Rota.Sound.Gg`).

- **As the Game Gear's:** colour RAM does the looks; fabric slots, a tile
  per logo cell, the priority bit; five streams a frame from the frame
  interrupt; the lead-in dark.
- **VRAM:** the background needs 266 tiles, so the 35 sprite patterns
  come first (tiles 0 on, register 6 = FBh) and the background after them
  (to tile 300 of 448). The name table is the whole 32 × 24 screen.
- **Colours:** one colour RAM byte an entry (--BBGGRR); the spec shows
  them as 12-bit colours with the levels 0, 5, 10, 15, which the model
  uses (`rgb6`).
- **Sprites:** Y is the top line minus one, X the left column. The VDP
  can't show a sprite partly left of column 0 (short of moving every
  sprite 8 pixels), so the model and the ROM leave such a sprite out: a
  spark coming in at the left edge shows once it is wholly on the screen.
- **Budget:** from the frame interrupt (line 193) to the next frame's line
  0, 15732 T-states; the worst frame takes about 15340 (the generous
  count).
- **Gates.** Layer 3 (`rota-demos`, on `Gg.Spec`'s Master System II):
  every frame equals the model from the lead-in through two idle loops;
  the budget; the audio golden. Layer 4: `demos/layer4.sh sms`; `make
  ci-demos` plays it as the boot splash (`/systems/boot/sms.sms`, and
  `/sms/rota` as a ROM).

## The Super Nintendo as built (M7 step 7.13, rendition B, 2026-10-18)

The lab's Super Nintendo (`snesM`: 256 × 224, ring radius 64, large
letters, the base look rounded to 5 bits a channel, `cgram15`). Built by
M7 at the user's request, additively on the Sega design: `Rota.Snes`
reuses `Rota.Sega`'s target (its pictures, its fabric slots, its records),
`Rota.Snes.Rom` writes the same streams in the SNES's formats, and
`Rota.Sound.Snes` is the chime on the S-DSP (the table above). The ROM is
`demos/snes/rota.s` (WLA-DX, 256 KiB LoROM, no SRAM, no chip), with the
SPC700's driver `demos/snes/rota-spc.s` (uploaded through the boot ROM;
it clears the echo buffer, then applies the DSP writes the 65816 hands it
through the ports).

- **As the Sega machines':** colour RAM does the looks; fabric slots, a
  tile per logo cell, the priority bit (sprites at priority 2 show over a
  background pixel and behind the logo's); five streams a frame from the
  NMI (line 225); the lead-in dark.
- **VRAM:** one 4bpp tile space, the 36 sprite patterns first, then 272
  background tiles (a library of 219 graphics, each copied by DMA as a
  frame needs it); BG1's tilemap at 7000h, unscrolled.
- **Sprites:** the PPU draws a sprite one line below its Y, which the
  model's sprite positions already allow for; a sprite left of column 0
  is left out, as on the Master System.
- **Budget:** V-blank, lines 225 to 261, 50468 master clocks; the worst
  frame takes about 23740 (the generous count; a sound write waits for
  the driver's echo).
- **Gates.** Layer 3 (`rota-demos`, on `Snes.Spec` with the boot ROM):
  every frame equals the model from the lead-in through two idle loops;
  the budget; the audio golden (`demo-snes-pcm.txt`). Layer 4:
  `demos/layer4.sh snes`; `make ci-demos` plays it as the boot splash
  (`demos/golden/splash-snes/`; on the card `/systems/boot/snes.sfc`, and
  `/snes/rota` as a ROM).

## The Atari 800XL's layout (decided by the user, 2026-10-07; step 6)

Chosen from previews (`rota-stats a800-preview-a`; the alternatives were
the whole picture in the bitmap, which needs up to 5 colours on a line,
and the letters in the bitmap sharing the fabric's colour register):

- **ANTIC mode E** for the whole screen (160 × 192, 4 colours a line),
  the lab's 800XL geometry (ring radius 64, large letters, 320 × 192 in
  our 2× HDMI output, so round). Each pixel is two wide: vertical traces
  are 2 px thick.
- **The letters are the four players**, solid (no groove: `geometrySolid`;
  a player is 8 colour clocks, exactly a large letter), each with its own
  colour register: dim, then the per-scanline gradient by DLI.
- **The playfield** (background, PF0 to PF2) holds the ring, the lambdas
  and the fabric, which runs through the letter band unchanged; display-
  list interrupts load each row band's ring colours. At most 3 playfield
  colours a line.
- **PF3, the missiles as a fifth player:** the core (white, on its 8
  lines), the junction lights (the light colour) and, inside the letter
  band (lines 80 to 111), the sparks: four pixels a bit, one colour a
  line (the spark's; white on the core's lines). Outside the band the
  sparks are players, one colour each (the head's).
- Accepted costs: the sparks are blocky for the band's 32 lines (the end
  of the lead's and harmony's figures, two crossings a lap in idle) and
  change shape as they enter it; head, trail and centre are one colour.

## The Atari 800XL as built (step 6, layout A, 2026-10-07)

`demos/a800xl/rota.s` (an XEX the project's loader starts after
AltirraOS) is a stream player; `Rota.A800` lays out the screen and
`renderShot`, the exact model of the picture the layer 3 test compares
with the spec frame by frame; `Rota.A800.Rom` the XEX's data.

- **Screen:** 192 lines of ANTIC mode E after 24 blank lines, in two
  4 KiB blocks ($8000 lines 0 to 95, $9000 from 96: no 8-line cell crosses
  ANTIC's boundary); players and missiles at $A000, one-line resolution.
- **Playfield colours:** the fabric is always PF0; each line's other
  roles (ring stages, lambdas) take PF1 and PF2, kept where they were on
  the line above, so six register loads a frame suffice.
- **Display-list interrupts** on a fixed schedule: each register write has
  a window of lines, placed at the earliest line with room (five stores a
  line); consecutive lines are one handler stepping with WSYNC (the
  letter band, lines 80 to 112, is one kernel: a handler a line would not
  finish within a mode E line). Values come from zero-page slots the main
  loop fills each frame (the stages' colours, the gradient, positions).
- **The gradient** of the lit letters is computed by the ROM: a 256-step
  phase, 13.5 a line and 7 a frame (the lab's 0.33 rad a line and 4 a
  beat), in seven colours between amber and teal.
- **The fabric** steps by writing the cells of three phases in the bitmap,
  only the rows that change: cells above line 128 behind the beam during
  the frame before, the rest after the next VBI, ahead of the beam (the
  beam reaches the bottom too late for them all in one frame).
- **Colours:** the lab's 800XL look in the palette's nearest entry of the
  same hue (within 30° in YIQ; near-whites and greys by distance), so the
  muted violets stay violet.
- **Sound** (`Rota.Sound.A800`): channels 3 and 4 pure tones at 64 kHz
  for the lead and harmony (up to about 20 cents off, POKEY's), channels 1
  and 2 joined (16-bit, 1.79 MHz) for the bass; the drums borrow the pair
  as noise for a few frames a hit.

## CHIP-8 as built (the user's choices, 2026-10-07)

On the Arty the quirks are VIP,
and every DXYN waits for the next 60 Hz tick: at most 60 draws a second.
Everything is XOR-drawn, so each change is drawn as its difference: the
ring, emblem and letters from a timeline of sprite pieces, the threads
column by column in the free time. The three threads are drawn as one
union (the user's choice, 2026-10-08: XORed over each other, two threads
a row apart cancelled to 1-row slivers at the left edge): a changed
column XORs its old union off and its new one on in one draw. They pass
behind the word (the user's choice, 2026-10-08; the audition page XORs
them over the letters, and as they flowed they scrambled R O from the
left): the word's rows, 10 to 20, are left out across the whole width,
as the disc is, so the bass, which lives in those rows, mostly hides.
The beeps keep a timeline of their own (exact at the fast setting, up to
7 frames late at the slow one). Concessions: no roll dips; the crash is one
dark-then-lit blink of the ring; the emblem grows in 3 steps; each flip
happens in one step, at its middle; the threads move as a left-to-right
sweep (about 11 s a sweep at the fast setting, so a flip shows as a seam
for a sweep; at the slow one they barely move). The ring's changes trail
the page by a few frames where several fall together.

## The boot splash on the board (step 7, 2026-10-07)

After a system's bitstream is in the PL, the PS app (`platform/ps/
relay.zig`, `splash.zig`) looks for `/systems/boot/<name><ext>`:
`<name>` as in `/systems/<name>.bit`, `<ext>` the system table's first
extension, so `chip8.ch8`, `gb.gb`, `nes.nes`, `gg.gg`, `a800xl.xex`, and `gbc.gbc` (the Game
Boy demo again: on the Game Boy Color a DMG cartridge without its own
palette gets side track I's default, the demo's own shades; `make sd`
copies the demos there). The file passes the same checks as a ROM from the menu, is
loaded with no save, and plays with the overlay hidden. Any input (a
board button, a key on the USB keyboard, a key from `make keys`) ends
it; once everything is released the core pauses and the menu opens over
it. Input already down when the splash starts doesn't count until it is
let go. Without the file (or with one the system refuses) the menu opens
as before, and the UART says why (`splash: none, ...`). It plays at
power-on, after `make prog` and after a switch on the Systems menu (on
by default since 2026-10-08, the user's choice; `splash_on_switch = off`
in `keymap.conf`'s `[global]` section turns the last off). A
`keymap.conf` from before then has no such line, so it means on: to keep
the old behaviour, add `splash_on_switch = off`.
