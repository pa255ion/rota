# The desktop emulators' speed-ups (side track Q)

Every optimization made for the desktop emulators' fast versions
(`<Sys>.SpecFast.*`; the Game Gear's and Master System's `Gg.SpecFast`), at the user's request (2026-10-21): what it does,
the measured speed-up, the trade-off, and the decision. Newest system
first; within a system, in the order they were made. Side track Q keeps
this file; each new speed-up gets its entry when it is committed.

## The rules every speed-up follows

- **The originals stay as they are.** The reference models
  (`<Sys>.Spec.*`, `cpus/*`) are what the Clash cores are checked
  against; a fast version is a copy beside them, used only by the desktop
  emulator. A bug fix in one goes to both.
- **Same machine, proven per instruction.** Each fast version has an
  equivalence suite (`<sys>-specfast`) that runs it beside the original
  on seeded inputs and compares the CPU's registers and cycle count after
  every instruction, and the picture, RAM and sound every frame. A fast
  CPU also runs its SingleStepTests on the copy, every bus cycle
  compared. Quick tier in `make test`, full tier with `EMU_FULL_TESTS=1`.
- **Each change measured.** With the system's benchmark
  (`<sys>-fast-bench`, GHC's `+RTS -s` for allocation) and through the
  front end (`--unpaced --frames N`). The numbers below come from this
  desktop (Ryzen 7 5700G, idle) unless they say otherwise.
- **`--reference`** runs the originals in the desktop emulator, to
  compare by hand.

## The common findings

- **Unboxing flags alone don't help.** The fast modules turn GHC's
  worker/wrapper back on (`-fworker-wrapper`; `clash-common` turns it off
  for Clash). For the 800XL that came with real rewrites; for the NES,
  copying the PPU, the APU and the mapper with only the flag changed
  nothing (identical allocation, the PPU 7% slower). The speed comes from
  changing the code.
- **Inlining across modules matters more.** Small helpers that return an
  `Int` from `ST` box it on every call when they aren't inlined; INLINE
  pragmas and higher unfolding thresholds remove those allocations.
- **Records of functions block inlining.** The reference models connect
  CPU and machine through a record of closures (`Bus`), so every bus
  access is an unknown call. A class instead lets GHC specialise the CPU
  to the machine (the NES's fast 6502).
- **Measure before choosing.** Twice the obvious target was the wrong
  one: the SNES's single render thread was slower than the CPU side, and
  the NES's desktop sound path cost as much as a chip.

## Master System and Game Gear (Q.7, 2026-10-21)

Benchmark: `sms-fast-bench`, Waternet (Master System), 600 frames (10 s
of game time). The reference model: 3.47 s with the FM unit, 2.53 s
without (about 2.9x and 4x real time here).

**Where the time went** (`perf`): the VDP about 36%, the FM unit
(`Sms.Spec.Fm`) 19%, lazy-evaluation and garbage-collection overhead 5
to 8%, the Z80 only about 2%. So unlike the NES, a fast CPU would barely
help; the VDP and the FM unit come first.

The fast version: `Gg.SpecFast` (the machine, in `systems/gg/`; its
types are the original's) and `Gg.SpecFast.Vdp`. Checked by
`sms-specfast` (both consoles, the Master System with and without FM;
the Z80's registers and T-states every instruction, picture, RAM, VRAM
and sound every frame; seeded pads; a planted difference caught).

1. **The VDP's dot loop, sprite lookup and pixel planes**
   (`Gg.SpecFast.Vdp`).
   - *What:* each dot divided the running dot count three times to find
     its line and dot; now the loop counts them along. Each active pixel
     looked the sprites up twice (for the collision flag and for the
     colour); now once. Tile and sprite pixels were built from their four
     bit planes through Haskell lists (`mapM` over `[0 .. 3]`, `sum`);
     now with plain bit operations, the sprite result packed in an `Int`.
   - *Speed-up:* without FM 2.53 to **1.42 s** (1.8 times faster),
     allocation 11.9 to 3.8 GB; with FM 3.47 to **2.37 s**.
   - *Trade-off:* none: the same reads in the same order.
   - *Decision:* kept. The FM unit is now about 40% of the time with FM.

2. **The FM unit's per-sample work** (`Sms.SpecFast.{Fm,FmUnit}`).
   - *What:* each of the 18 operator slots built an 8-entry vector of the
     user patch every sample (about 900 000 a second): now 8 reads, no
     vector. The envelope clock's locks and the tremolo, the same for
     every slot of a sample, once a sample, not per slot. A rate table
     built from a list per slot: a `case`. The pending-writes list
     scanned per slot: skipped when empty. A mute test searching a list
     (`elem`): a bit mask. Each sample's 18 pins went into a list of
     vectors only for the FM unit to sum each and keep the last: the fast
     FM keeps just the last sample's sum.
   - *Speed-up:* Waternet with FM 2.37 to **2.19 s**.
3. **The FM's lazy pairs made strict, rhythm lookups without lists.**
   - *What:* the envelope's step and next state, each pin's value and
     sign, and the noise generator's state were lazy pairs (a thunk and
     two selector thunks each); rhythm mode was looked up through a list
     comprehension about 72 times a sample.
   - *Speed-up:* `fmtune` (our ROM that plays FM music; the two homebrew
     games leave the FM unit off) 2.27 to **2.14 s**.
4. **Divisions in the FM replaced** (tried, reverted).
   - *What:* `perf` showed integer divisions (`mod 18`, `div 3`) among
     the FM's hottest instructions; replaced with comparisons and bit
     masks.
   - *Speed-up:* none (2.14 to 2.17 s, noise).
   - *Decision:* reverted, to keep the copy closer to the original.

**Where it stands:** with FM music playing, the FM unit is still about
38% of the time and the VDP 19%; without, the VDP leads. The next step
would be caching each slot's patch-derived parameters (they change only
on a register write), not done: see the result below.

**Result** (the desktop emulator, LLVM, 1800 frames):

| ROM | `--reference` | fast |
|---|---|---|
| Waternet (Master System, FM unit fitted, unused) | 2.92x | **4.88x** |
| `fmtune` (FM music) | 3.76x | **4.95x** |
| Mega Man (Game Gear) | 5.35x | **8.43x** |

On a slower 4-core laptop, at about half this desktop's speed when capped
at 2500 MHz, that leaves about 2x real time even with FM music, so the
FM caching waits until a game needs it.

## NES (Q.7, 2026-10-21)

Benchmark: `nes-fast-bench`, Kirby's Adventure (MMC3), 600 frames (9.98 s
of game time). The starting point, the reference model: 5.82 s without
sound, 6.91 s with the desktop's sound trace; the desktop emulator 1.35x
real time (LLVM build).

**Where the time went** (`nes-fast-bench` modes, then GHC's `-fprof-late`
profile): the PPU about half the machine (2.9 of 5.8 s); the rest the
6502, the glue between the CPU and the chips (the bus closures, each
cycle's start and end: about 40% in the profile), the mapper and the APU;
the desktop's sound path 1.1 to 1.5 s on top.

1. **The sound without trace lists** (`Nes.SpecFast.Machine`'s
   `setSound`/`takeSound`; the desktop's filter one strict loop).
   - *What:* the reference model's desktop sound recorded every
     instruction's bus accesses and every cycle's five APU levels in
     Haskell lists ('withTrace', 'apuTrace'), and ran the board's filter
     with a boxed state record per cycle. The fast machine writes each
     cycle's levels, packed in one `Int`, into an unboxed buffer; the
     filter is one strict loop over it.
   - *Speed-up:* the desktop emulator 1.35x to **1.73x** real time.
   - *Trade-off:* none in behaviour (the desktop test compares the fast
     and reference sound sample for sample); the bus trace is still there
     for the tests, off.
   - *Decision:* kept.
2. **Unboxing flags on copies of the PPU, mapper and APU.**
   - *What:* `Nes.SpecFast.{Ppu,Mapper,Apu}` as copies with
     `-fworker-wrapper -fspecialise-aggressively`.
   - *Speed-up:* none (allocation identical; the PPU alone 3.53 to 3.79 s).
   - *Decision:* the copies stay, as the place for the real changes below.
3. **The hook looked up once per run of dots, and no `Maybe` per read.**
   - *What:* the machine looked up its (test-only) PPU hook every dot, and
     asked the APU for its DMC DMA as a `Maybe Int` on every CPU read.
   - *Speed-up:* within noise (6.84 to 6.93 s).
   - *Decision:* kept, harmless; it showed GHC had already removed both.
4. **The APU's levels packed directly** (`apuPacked`, its helpers INLINE).
   - *What:* the sound capture built the APU's five-field `Channels`
     record every cycle, through small helpers each returning a boxed
     `Int`.
   - *Speed-up:* 6.93 to **6.58 s**; allocation 8.5 to 6.2 GB.
   - *Decision:* kept.
5. **Higher inlining thresholds** in the fast modules
   (`-funfolding-use-threshold=200 -funfolding-creation-threshold=1000`).
   - *Speed-up:* 6.58 to **6.37 s**; allocation 6.2 to 5.9 GB.
   - *Trade-off:* longer compiles of these modules.
   - *Decision:* kept.
6. **A fast 6502 specialised to the machine** (`Nes.SpecFast.Cpu`).
   - *What:* a copy of `cpus/mos6502`'s spec whose bus is a class
     (`CpuBus`) instead of a record of functions; the fast machine is the
     instance and holds the CPU (a recursive let; the CPU's bus field is
     lazy). GHC specialises the CPU's code to the machine, so a bus
     access is a direct call.
   - *Speed-up:* 6.37 to **5.84 s**; allocation 5.9 to 5.0 GB. With
     sound, the fast version is now as fast as the reference model
     without it.
   - *Trade-off:* a second copy of the 6502 to keep in step with
     `Mos6502.Spec`. First in `systems/nes/`; moved to
     `cpus/mos6502/src/Mos6502/SpecFast.hs` (2026-10-21, the user: shared,
     beside the original) for the 800XL's fast version too.
   - *Decision:* kept.

7. **The PPU's bus as the cartridge itself** (tried, rejected).
   - *What:* the fast PPU reached the cartridge through a record of
     functions (`PpuBus`), as the CPU did; the copy held the mapper and
     called it directly, with and without INLINE pragmas on the mapper's
     functions.
   - *Speed-up:* none: allocation 5.0 to 3.3 GB (with the pragmas) or 4.2
     GB (without), but the time 5.83 to 6.02 or 6.13 s (best of three
     runs each). Inlining the mapper's larger functions into every fetch
     makes the code bigger, not faster.
   - *Decision:* reverted. Allocation is not what limits the NES any more.
8. **Inlining thresholds on the PPU alone** (measured, kept as is).
   - *What:* the PPU's dot function had become one function of about
     11 650 lines of GHC's Core with the raised thresholds; the PPU alone
     (`fast-ppu`, best of three): 3.40 s with them, 3.73 s with the
     unboxing flags only, 3.75 s with the reference's flags.
   - *Decision:* the thresholds stay. What is left in the PPU is the
     per-dot work itself (about 54 ns a dot here), which only a change
     of structure can cut.

9. **`perf` in the dev shell** (`flake.nix`, for profiling).
   - *Why:* GHC's profiler adds cost centres that distort small hot
     functions, and can't see inside one large function. `perf record -e
     cycles:u` samples the real binary: the PPU's dot function 37%, a
     helper inside it 16% (the sprite units, below), the cycle glue about
     25%, the APU 6%, the mapper's PPU reads 4%.
   - *Trade-off:* one more package in the dev shell.
10. **Each line's sprite pixels precomputed** (`spriteLine` in
    `Nes.SpecFast.Ppu`).
    - *What:* `spriteAt` walked the 8 sprite slots twice for every visible
      pixel, with branches the processor mispredicts. While rendering
      stays on for a whole line, the slots' output follows from their
      state at the line's start (a counting slot with counter c draws from
      pixel c, a slot not counting at once, each shifting once a pixel
      from then on). So the line's first pixel computes all 256 sprite
      pixels, and each pixel is one lookup. If rendering goes off
      mid-line, or when the line ends, the slots' exact state after n
      pixels is computed in closed form and the original per-pixel code
      takes over for the rest of the line.
    - *Speed-up:* the PPU alone 3.40 to **2.89 s**; the machine 5.83 to
      **5.34 s**.
    - *Trade-off:* more code to keep equal to the reference's; checked by
      `nes-specfast`'s quick tier and its full-tier AccuracyCoin, every
      Blargg sprite hit, sprite overflow, OAM and VBlank/NMI test.
    - *Decision:* kept.

11. **Frame skipping for the NES** (as the SNES's and the 800XL's;
    `--no-frameskip`).
    - *What:* when the front end falls behind, the PPU draws nothing for
      the next frame (at most 3 in a row): the sprite units still run and
      sprite 0 hit is still found, the rest of the pixel's work isn't done.
    - *Speed-up:* a skipped frame costs 7% less (the machine 5.34 to 4.98 s
      with every frame skipped), plus the front end's picture conversion
      and upload. Small: on the NES, drawing is a small part of the PPU's
      work (the fetches, the evaluation and the timing are most of it).
    - *Trade-off:* a choppier picture while behind; the game and sound
      keep their speed.
    - *Decision:* on by default, to absorb short dips (a slower 4-core laptop).
      `nes-specfast` checks that skipping changes nothing the game sees
      (every other frame skipped: Thwaite; Blargg's sprite 0 hit basics).

**Result so far** (the desktop emulator, LLVM, 600 frames):
Kirby's Adventure 1.28x (`--reference`) to **1.94x** real time, Thwaite
1.32x to **2.14x**: about 1.5 times faster. On a slower 4-core laptop capped
at 2500 MHz, where the reference model gave 0.78x, that should be about
1.1 to 1.2x (to be measured there).

- **The mapper's reads inlined** (Q.7, 2026-11-02;
  `Nes.SpecFast.Mapper`: `cpuReadCart`, `chrRead`, `ppuRead`).
  - *Found with* `-fprof-late` (as the 800XL's, item 9 there): the PPU's
    dot at 22%; `ppuRead` and `cpuReadCart` at 16% and 14% of the
    allocation for little time: a boxed byte per read across the
    module boundary.
  - *Speed-up:* `nes-fast-bench fast`, Thwaite, 600 frames: allocation
    6.02 to 4.69 GB, MUT time **5.15 to 4.90 s** (5%; built on the
    worker, timed on this machine, alternating, 3 runs each).
  - *Tried, no gain:* the APU's `apuPacked` inlined (allocation 4.69 to
    3.62 GB; time within noise).
  - *Decision:* kept; nes-specfast green in both tiers.

## SNES (Q.5 and Q.6, 2026-10-20)

Benchmark: `snes-fast-bench`, Rockman & Forte, 300 frames. The
reference model through the front end: 0.57x real time.

**Where the time went** (profile): the bus's `tick` 24%, the PPU's
`ppuRenderTo` 19 to 23%, the 65816's `step` 16 to 18%, the bus closures
12 to 14%.

1. **`nextEvent` without lists; the PPU's windows once per pixel and the
   BG layers once for both screens** (Q.5).
   - *Speed-up:* allocation 46 to 28 GB per 300 frames, but the time only
     8.8 to 7.9 s (0.57x to 0.63x).
   - *Decision:* kept; it showed the cost was the per-pixel and
     per-access work itself, which led to Q.6's threads.
2. **The drawing on another core** (Q.6, `Snes.SpecFast.RenderLog`).
   - *What:* the PPU's state changes are logged with their dots and
     replayed by a render thread on its own copy of the PPU. Drawing only
     fills the frame buffer, which nothing the CPU reads depends on, so
     the result is the same.
   - *Speed-up:* 1.6x on the benchmark.
   - *Finding:* the CPU side alone took 2.65 s per 300 frames, the
     drawing 5.9 s: the render thread was now the bottleneck.
3. **Several render threads, each drawing its share of the lines**
   (`--render-threads N`, default 2; `--no-render-thread`).
   - *What:* a line's drawing depends only on the state at its dots, so
     thread k draws the lines with line mod N = k, and the frame is put
     together from their rows.
   - *Speed-up:* about 1.5x more.
   - *Trade-off:* N more cores busy; each thread keeps its own PPU copy.
   - *Decision:* two threads by default (a third gave nothing here, on
     4 cores: 1.45x with two, 1.39x with three).
4. **The picture one frame late** (`--no-frame-lag` turns it off).
   - *What:* the machine runs into the next frame while the threads
     finish this one.
   - *Trade-off:* one frame (16.6 ms) more latency between input and
     picture; frame copies kept only while this is on.
   - *Decision:* on by default.
5. **Frame skipping** (`--no-frameskip`; the 800XL too).
   - *What:* when the front end falls behind (less than a frame of sound
     queued, or the clock late), the next frame's pixels aren't drawn, at
     most 3 frames in a row. The game logic runs every frame.
   - *Trade-off:* a choppier picture while behind, in exchange for steady
     game speed and sound.
   - *Decision:* on by default.
6. **The fast 65816** (`Snes.SpecFast.Cpu`, in `systems/snes/`: M7's
   choice), **the A bus by page** (page entries filled on first use: the
   full table took about 16.7 million decodes at power-on), **the next
   event cached**, the trace closures inlined.
   - *Decision:* kept; `snes-specfast-cpu` runs all 5.12 million
     SingleStepTests cases on the copy.
7. **Sixteen lines per hand-off to the render threads** (2026-10-21).
   - *What:* the log went to the threads after every line, about 16 000
     wake-ups per second per thread.
   - *Speed-up:* none on this desktop; made for a slower 4-core laptop, whose
     idle cores clock down (there the SNES ran at 0.34x). Not measured
     there yet: the laptop turned out to throttle for heat as well.
   - *Trade-off:* none (the threads were never more than a frame behind
     anyway).
   - *Decision:* kept.

8. **The 65816's decode without lists** (Q.7, 2026-10-21;
   `Snes.SpecFast.Cpu`'s `execute` and `aluOp`).
   - *What:* `perf` showed 13% of the SNES's time in `elem`, `eqInt` and
     `!!`: every instruction tested its opcode column against a list of 15
     (`col `elem` aluCols`) and picked its ALU operation by list index,
     through the generic `Eq` class. Now a bit mask (`0xA2AEA2AA`, checked
     against the list) and a `case`. (The 65816's own work, the steps,
     cycles and ALU, is only about 10 to 12% of the time; the machine's
     `tick` 28%.)
   - *Speed-up:* the CPU side (`fast-nodraw`, 300 frames) 3.76 to **2.03 s**,
     1.85 times faster (back to back, both under the same load).
   - *Trade-off:* none; `snes-specfast-cpu`'s SingleStepTests and
     `snes-specfast` green.
   - *Finding:* with drawing on two render threads (3.04 s) the threads
     are the limit again; drawing inline, 7.79 s.

9. **The frame put together by rows, the picture converted by rows**
   (`assemble` in `Snes.SpecFast.RenderLog`, `snes_pixels`).
   - *What:* the frame was assembled from the render threads' frames by
     indexing a boxed vector and dividing for each of 122 000 pixels; the
     conversion to the window's 512 x 478 divided per pixel too. Now one
     slice copy per line, and each line converted once and written twice.
   - *Speed-up:* through the front end 1.60x to about **1.67x**.
10. **Measured: the main thread is the limit.** `perf` by thread, two
    render threads: the main thread (the machine) has 38.6% of all
    samples, about 100% busy; the render threads 17 to 25% each, about
    half busy. So a third render thread changes nothing (1.66x to 1.69x).
    Two thirds of the main thread's hottest code is the machine's `tick`
    (the timing step called before every bus access: advance the clock,
    draw up to the dot, fire events), about 25 ns a call, several million
    calls a second.
11. **The sound CPU's decode without lists** (`Snes.SpecFast.Spc700`, a
    copy of `cpus/spc700`'s spec, as the 65816's) **and unchecked
    register reads in `tick`'s helpers.**
    - *What:* the SPC700's ALU block was found with two `elem` tests on
      lists every instruction; now comparisons (the same result for all
      256 opcodes, checked exhaustively).
    - *Speed-up:* none measurable (1.66x to 1.67x).
    - *Decision:* kept (harmless, verified); the SPC700 is a small part
      of the main thread.

12. **`tick`'s rare events kept out of line** (`{-# NOINLINE event #-}`).
    - *What:* `event` (a new line, HDMA, V-blank, the IRQ timer) had been
      inlined into `tick`, making its common path (a few reads and
      writes, several million calls a second) one function of about
      5700 instructions that saved many registers on every call: `perf`'s
      hottest instructions in `tick` were stack spills at its entry.
    - *Speed-up:* the machine side (`fast-nodraw`, idle machine) 1.99 to
      **1.43 s**, 1.4 times faster.
    - *Lesson:* raised inlining thresholds can hurt a hot function that
      has a rare, large branch; `NOINLINE` on the branch fixes it.
13. **Three render threads by default** (`--render-threads N` still sets
    it; one fewer than the logical CPUs on a smaller machine).
    - *Why:* with the machine faster, two render threads were the limit
      again: 1.73x with two, 2.23x with three, 2.10x with four (all
      cores); on 4 cores 1.82x and 2.19x; even on 2 cores with SMT (4
      logical CPUs) three beat two, 1.54x against 1.33x.
    - *Speed-up:* through the front end 1.67x to **2.26x**.

14. **The pixel's composition without lists** (`screen` and `layers` in
    `Snes.SpecFast.Ppu`, on the render threads).
    - *What:* for the main and sub screen of every pixel, `layers`
      returned a list of `(layer, (pixel, colour))` pairs, and `screen`
      built a list of up to 12 `Maybe` candidates in the mode's priority
      order, looked layers up in it (`lookup`) and took the first hit
      (`find`). Now the layers go into a small unboxed scratch array, and
      each mode's priority order is a fixed table walked to its first hit:
      the same order and rules.
    - *Speed-up:* the drawing (inline minus the machine side) about 5.35
      to **4.40 s** per 300 frames; allocation 26.2 to 18.2 GB. Through
      the front end 2.26x to **2.46x**.
    - *Then:* with the drawing cheaper, two render threads beat three
      again on every layout tried (all cores 2.52x against 2.47x; 4 cores
      2.51x against 2.42x; 4 cores with 8 threads, as that laptop, 2.36x
      against 2.23x). The default went back to **2** (step 13 reversed);
      three only paid while the drawing was the slower side.

15. **The fetch table indexed directly, the fetch's scroll pair strict.**
    - *What:* `fetchTable mode !! s` (a list index on every fetch dot,
      264 a line) became one boxed table indexed by mode * 8 + slot; the
      lazily bound (coarse, vofs) pair is strict.
    - *Speed-up:* the drawing inline 5.83 to 5.67 s per 300 frames; within
      noise through the front end.
16. **Tried for the NES after step 12, rejected:** `NOINLINE` on the NES
    machine's DMA and strobe paths (5.30 to 5.44 s) and on the NES PPU's
    occasional phases (sprite evaluation, the fetch phase, OAM
    corruption, the sprite line: 5.30 to 5.72 s). The NES's large
    function is the PPU's dot function, whose phases run on most dots, so
    calling them out of line costs more than the spills it saves.

Result through the front end, Rockman & Forte: **0.57x to 2.52x** real
time (1.47x at the end of Q.6; a slower 4-core laptop: 0.98x with 9.7% of
frames skipped at Q.6's end, 1.00x with 2% at step 11). The next step would be `tick` itself (the
machine's per-access timing), or the 65816's bus as a class, as the NES's
6502 got.


- **The BG pixels as strict loops** (Q.7, 2026-11-02;
  `Snes.SpecFast.Ppu`: `bgPixel`, `layers`).
  - *Found with* `-fprof-late`: the PPU's `screen`, `ppuRenderTo` and
    `layers` were 58% of the time, and `layers` 36% of the allocation.
    `bgPixel` built two lists, mapped over one and zipped and folded them
    per layer and pixel; `layers` walked a list of its layers per pixel.
  - *Speed-up:* `snes-fast-bench fast-inline`, the tech demo, 300
    frames: allocation 21.6 to 12.9 GB, MUT time **6.83 to 5.45 s**
    (20%; built on the worker, timed on this machine, 3 runs each).
  - *Tried, slower, reverted:* `Shown` packed into an Int and colour
    maths without the Maybe (allocation 12.9 to 12.1 GB, time 5.31 to
    5.45 s).
  - *Decision:* kept; snes-specfast green in both tiers.

## Atari 800XL (Q.5 and Q.6, 2026-10-20)

Benchmark: `a800-fast-bench`, Scorch, 300 frames. The reference model:
6.57 s (0.76x real time).

1. **GTIA and ANTIC rewritten from profiles** (Q.5:
   `A800.SpecFast.{Gtia,Antic,Machine}`).
   - *What:* GTIA's register queue split only when due, strict bindings,
     no tuples, a strict loop over the objects, fast paths for idle
     shifters and for "no player or missile in mode 0"; ANTIC's `edges`
     and AN codes strict; worker/wrapper back on.
   - *Speed-up:* 6.57 to **3.39 s** (1.94x); allocation 32.7 to 10.2 GB.
   - *Decision:* kept. The front end gave 1.21x: the rest was
     per-instruction overhead (POKEY's level built a list).
2. **The fast POKEY** (Q.6, `A800.SpecFast.Pokey`).
   - *What:* the level a strict loop, and the reload pipeline's list
     skipped when nothing is pending.
   - *Speed-up:* through the front end 1.21x to 1.27x.
   - *Decision:* kept.
3. **Frame skipping** (as the SNES's): only the colour computation and
   the picture's writes are skipped; collisions still run, since games
   read them.
4. **GHC's LLVM backend** (both systems, `make desktop`;
   `DESKTOP_LLVM=0` without).
   - *Speed-up:* the 800XL benchmark 3.27 to 2.66 s; the SNES 3.39 to
     3.03 s; the NES 1.57x through the front end.
   - *Trade-off:* LLVM 18 in the dev shell (a one-time download), a second
     build directory (`build/desktop-llvm`), slower library builds;
     `make test` stays on the normal build.
   - *Decision:* on by default.

5. **On the shared fast 6502** (Q.7, 2026-10-21; `Mos6502.SpecFast`, the
   machine its `CpuBus` instance as the NES's).
   - *Where the time went first* (`perf`, the desktop emulator on Scorch,
     normal build): GTIA's per-clock step 26%, ANTIC about 21%, POKEY
     about 9%, lazy-evaluation overhead 5%, the 6502 only about 4%.
   - *Speed-up:* 3.27 to **3.17 s** (`a800-fast-bench`, Scorch, 300
     frames), as small as the profile said.
   - *Decision:* kept (the user's choice of a shared copy; GTIA and ANTIC
     next).

6. **GTIA's objects skipped when idle** (`gtTrig`, `sActive` in
   `A800.SpecFast.Gtia`).
   - *What:* every colour clock (about 3.6 million a second) read all 8
     players' and missiles' positions and shifters to find which start or
     are drawing. Now a table from position to the objects there (kept
     where HPOSP/HPOSM are applied) and a mask of the objects active at
     the last full look (a superset after they shift, which only means
     looking again): a clock where nothing starts and nothing is active
     costs two reads.
   - *Speed-up:* 3.17 to **3.04 s** (Scorch keeps its players busy).
7. **The desktop's sound filter without `pow` per instruction**
   (`A800.Desktop`'s `samples`).
   - *What:* the filter runs once an instruction in closed form; its DC
     step computed `(1 - dcA) ** cycles` with a real `pow` call each time
     (the other pole already used a table). Now a table of the same
     values (bit-identical), and the pair it returns strict.
   - *Speed-up:* through the front end 1.55x to **1.63x**.

Result through the front end: **1.21x to 1.49x** real time.

8. **Tried, no gain, reverted** (Q.7, 2026-10-31). GHC's cost-centre
   profile (`-fprof-auto-top`, `a800-fast-bench`, Scorch) put POKEY's
   tick first: 14.9%, ahead of GTIA's clock at 13.0%. Three changes
   followed from it:
   - POKEY's reload lists as bit masks;
   - its noise generators as tables by the time since they restarted;
   - the desktop's sound filter run only when POKEY's level changes.

   The equivalence suites were green in both tiers, but there was no
   change beyond noise: `a800-fast-bench`, Scorch, 300 frames, 4.5 to
   5.0 s before and 4.8 to 5.0 s after on the worker; through the front
   end 1.07x and 1.06x. The cost centres keep small per-cycle functions
   from being inlined and inflate their share; as in Q.6, only `perf`'s
   profile says where the time goes. `perf` doesn't run in the worker's
   container (`perf_event_paranoid` 4), so this needs a profile on the
   desktop machine.
9. **ANTIC's per-cycle pairs and triples inlined** (Q.7, 2026-11-02;
   `A800.SpecFast.Antic`: `edges`, `anCodes`, `anticEnd`).
   - *Found with* GHC's late cost centres (`-fprof-late`: added after
     optimization, so they inline as the real build does), which `perf`
     stood in for in Q.6. They put GTIA's clock at 19%, the CPU's reads
     at 12%, and ANTIC's cycle end at 9% with a fifth of all
     allocation; POKEY's tick at only 5%.
   - *What:* `edges` returned a triple at its five calls each cycle, and
     `anticEnd` and `anCodes` returned the cycle's AN codes as a pair.
     Inlined, they build neither.
   - *Speed-up:* `a800-fast-bench`, Scorch, 300 frames: allocation 8.82
     to 6.98 GB, MUT time **3.05 to 2.73 s** (10%). The worker built both
     versions and this machine timed them, alternating, 5 runs each
     (spread 0.03 s); the shared worker's own times varied 2.5x.
   - *Tried, no gain:* `event`, `trace` and `anticPosition` inlined (GHC
     already did); strict returns (`pure $!`) in the hot paths (already
     strict); GTIA's `gtiaClocks` inlined; wider late lambda lifting
     (`-fstg-lift-lams-*-args=12`): allocation 6.98 to 5.37 GB but time
     only 0.5%, so allocation no longer costs time.
   - *Decision:* kept; the equivalence suites green in both tiers.
