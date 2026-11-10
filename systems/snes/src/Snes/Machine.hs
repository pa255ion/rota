{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-|
The Super Nintendo machine both boards' tops share (PLAN.md step 7.10):
"Snes.Core" with its ROM in DDR (at 'romBase', through
"Platform.Mem.Ddr"'s port), the picture line-locked into 640 × 480
("Snes.Display": the ring of 32 lines, the master clock's pace), the S-DSP's
32 kHz held and filtered for the platform's 48 kHz samplers, and the PS
link: keys, the menu's pause and overlay, loading.

__Loading__ as the other systems': the PS holds the system, writes the
ROM file into DDR and the cartridge word to 'cartAddr' (two bytes, low
first, as the PS app's @ddr_info@ carries them: bits 1-0 the mapping, 0
LoROM, 1 HiROM, 2 ExHiROM; bits 5-2 the ROM's larger power of two as its
log less 12; bits 10-6 the rest's log, 0 none; bits 14-11 SRAM's size as
the header's code, 1 KiB << n, 0 none; bit 15 a copier's header, the ROM
512 bytes into the file: the PS loads the file whole), then lets go. The
power-on word is @build/snes/cart.bin@ (@snes-image ROM DIR@; its
@rom.bin@ has no copier header).

__Battery saves__ ("Snes.Save"): SRAM loaded while held, read back
through its second port on the byte channel ('snByte'), the dirty flag
for @PS_IN@ bit 6 ('snDirty').

__Keys:__ pad 1 is the PS's user bits 0-11 as the pad's bits (B, Y,
Select, Start, up, down, left, right, A, X, L, R from bit 11); pad 2 the
same bits in two link bytes at 'pad2Addr' (low first), which the PS's
relay writes on each change (the key word's bits 16-27: side track K's
@keys_hi@, docs/ARCHITECTURE.md). Both none while the menu pauses.
-}
module Snes.Machine
  ( SnesMachine (..)
  , snesMachine
  , romBase
  , cartImage
  , cartAddr
  , systemSnes
  , cartConfig
  , snesAudio
  , pad2Addr
    -- * The trace (tests)
  , Trace (..)
  , traceInit
  , traceStep
  , traceLine
  , traceGap
  ) where

import Clash.Prelude

import Data.Maybe (fromMaybe, isJust)

import Platform.Audio.Volume (volumeStereo)
import Platform.Domains
import Platform.Mem.Axi
import Platform.Mem.Ddr (DdrAddr, DdrStatus (..), ddrConfigDefault, ddrReadPort)
import Platform.Ps.Link
import Platform.Video.TextOverlay (textOverlay)
import Platform.Video.VgaTiming
import Snes.Bus (CartCfg (..), CartMap (..))
import Snes.Core
import Snes.Display
#ifdef SNES_DSP1
import Snes.Dsp1 (dspFwOffset)
#endif
import Snes.Save
import Snes.StateHooks
import Platform.SaveState (StateHooks (..))
import Snes.TraceTemplate (traceTemplate)
import qualified Data.List as L

-- | Where the ROM starts in DDR (the media region's start).
romBase :: DdrAddr
romBase = 0x02000000

cartImage :: FilePath
cartImage = "build/snes/cart.bin"

-- | The cartridge word's place in the link map (two bytes, low first).
cartAddr :: LinkAddr
cartAddr = 0x000000

-- | Pad 2's two bytes in the link map (reserved in docs/ARCHITECTURE.md).
pad2Addr :: LinkAddr
pad2Addr = 0x160000

-- | The Super Nintendo's system ID on PS_IN (docs/ARCHITECTURE.md,
-- "Reserved numbers").
systemSnes :: SystemId
systemSnes = 9

-- | The cartridge word as the bus's configuration.
cartConfig :: BitVector 16 -> CartCfg
cartConfig w =
  let m = case slice d1 d0 w of
        1 -> CmHiRom
        2 -> CmExHiRom
        _ -> CmLoRom
      p = 1 `shiftL` (fromEnum (unpack (slice d5 d2 w) :: Unsigned 4) + 12)
      q = let k = unpack (slice d10 d6 w) :: Unsigned 5 in if k == 0 then 0 else 1 `shiftL` fromEnum k
      n = unpack (slice d14 d11 w) :: Unsigned 4
      sram = if n == 0 then 0 else 1024 `shiftL` fromEnum n
  in CartCfg m p q sram

#ifdef SNES_DSP1
-- | (The DSP-1 build) the firmware's place in DDR: the SNES row's
-- companion, @dsp1.rom@ (platform/ps/systems/snes.zig).
dspFwBase :: DdrAddr
dspFwBase = 0x02700000
#endif

-- | Where the ROM starts in the file loaded: after a copier's header (bit
-- 15), or at once.
romSkip :: BitVector 16 -> DdrAddr
romSkip w = if testBit w 15 then 512 else 0

data SnesMachine dom = SnesMachine
  { snTiming :: Signal dom VgaOut
  , snRgb    :: Signal dom Rgb888
  , snAudio  :: Signal dom (Signed 16, Signed 16)
  , snFrame  :: Signal dom Bool             -- ^ toggles with each core frame
  , snAxi    :: Signal dom AxiRdM2S
  , snLate   :: Signal dom (Unsigned 16)    -- ^ rows shown late (the line ring's count)
  , snByte   :: Signal dom (Maybe (BitVector 8)) -- ^ for PS_IN's byte channel: SRAM read back
  , snDirty  :: Signal dom Bool             -- ^ for PS_IN bit 6: the game wrote SRAM since the PS cleared it
  }

-- | The machine from the board's pad (the pad's 12 bits, pressed), the
-- PS link's lines (synchronised) and the HP port's slave side.
snesMachine
  :: HiddenClockResetEnable DomPix
  => StateHooks -> Signal DomPix (Unsigned 12) -> Signal DomPix PsGpio -> Signal DomPix AxiRdS2M
  -> SnesMachine DomPix
snesMachine hooks padB ps s2m = SnesMachine timingO rgbO (volumeStereo vol audio) frameTog m2s (pcLate <$> pace) chanByte (scDirty <$> saveCtl)
 where
  -- the PS link: keys with the board's (none while the menu pauses), the
  -- menu's pause and overlay, the hold, the cartridge word
  link = psLink ps
  ctrl = psControl link
  sets = psSettings link
  vol = psVolume <$> sets
  menuPause = pcPause <$> ctrl
  hold = pgHold <$> ps
  pad1 = (\p b k -> if p then 0 else b .|. resize (unpack k :: Unsigned 16)) <$> menuPause <*> padB <*> (pgUser <$> ps)
  pad2Link = register 0 (pad2Write <$> pad2Link <*> link)
  pad2 = (\p k -> if p then 0 else k) <$> menuPause <*> pad2Link
  pads = (\a b -> a :> b :> Nil) <$> pad1 <*> pad2
  cartFile = blockRamFile (SNat @1) cartImage (pure (0 :: Index 1)) (pure Nothing) :: Signal DomPix (BitVector 16)
  cartPs = register Nothing (cartWord <$> cartPs <*> cartFile <*> link)
  word = fromMaybe <$> cartFile <*> cartPs
  cfg = cartConfig <$> word
  -- held: no DDR requests, the cache invalidated; the core reset once the
  -- port is idle
  busy = stBusy <$> status
  -- (a frozen core is not reset: a save state is written into it while
  -- the PS holds it, step 7.14)
#ifdef SNES_STATES_CPU
  -- (the CPU build: a hold owes the core one reset, frozen or not. A load
  -- comes after the PS's freeze, which reads the layout, and its routine
  -- starts at the reset's vector from the overlay; the reset lasts a clock
  -- or two, so the state the PS then writes stays. Without it the frozen
  -- game ran on, the routine never: layer 4's load-cpu.ps, 7.15d.)
  owed = register True ((\h r o -> not h || (o && not r)) <$> hold <*> inReset <*> owed)
  inReset = register False ((\h b r z o -> h && (not z || o) && (r || not b)) <$> hold <*> busy <*> inReset <*> stFrozen <*> owed)
#else
  inReset = register False ((\h b r z -> h && not z && (r || not b)) <$> hold <*> busy <*> inReset <*> stFrozen)
#endif
  machineRst = unsafeFromActiveHigh ((||) <$> unsafeToActiveHigh hasReset <*> inReset)
  -- the ROM from DDR
#ifdef SNES_DSP1
  -- (the DSP-1 build: the firmware's offsets go to the companion file the
  -- PS put in DDR, 'dspFwBase', without the copier's skip)
  ddrReq = (\h k r -> if h then Nothing else (\a -> if a >= dspFwOffset then dspFwBase + resize (a - dspFwOffset) else romBase + k + resize a) <$> r)
             <$> hold <*> (romSkip <$> word) <*> scDdrReq core
#else
  ddrReq = (\h k r -> if h then Nothing else (\a -> romBase + k + resize a) <$> r) <$> hold <*> (romSkip <$> word) <*> scDdrReq core
#endif
  (resp, m2s, status) = ddrReadPort (SNat @5) ddrConfigDefault hold ddrReq s2m
  answer = (\h r -> if h then Nothing else bytes <$> r) <$> hold <*> resp
  bytes :: AxiData -> Vec 8 (Unsigned 8)
  bytes w = reverse (map unpack (unpack w :: Vec 8 (BitVector 8)))
  -- the core, paced by the output (not while the menu pauses or the PS
  -- holds it)
  vga = vgaTiming vga640x480
  pace = withReset machineRst (register paceInit pace')
  (pace', runPace) = unbundle (paceStep <$> pace <*> (PaceIn <$> vga <*> scFrame core <*> scV core <*> scH core <*> scOverscan core
                                                            <*> ((\p o h -> (p && not o) || h) <$> menuPause <*> stRunOn <*> hold)))
  -- (registered: the pace's sums and compares stay out of the 5A22's
  -- step, the bitstream's worst path at 7.11; a master clock a pixel
  -- clock late is nothing against the line's 236 spare ones)
  -- (and on while a save state's freeze is asked for and not yet reached:
  -- the menu pauses the core, which must still reach rest, within a frame;
  -- paced: the CPU build's rest can be most of a frame away)
  run = register False ((\r p h o -> r && (not p || o) && not h) <$> runPace <*> menuPause <*> hold <*> stRunOn)
  -- (SRAM takes the PS's writes while held, and while frozen: a save
  -- state's SRAM goes in through the battery saves' window)
  core = withReset machineRst (snesCore run cfg pads answer (sramPsOp <$> ((||) <$> hold <*> stFrozen) <*> link) stCtl stOvl)
  -- save states (step 7.14; the Arty's top only): "Snes.StateHooks"
  (stCtl, stOvl, stFrozen, stRunOn, stByte) = case hooks of
    NoStateHooks -> (pure stIdle, pure ovlIdle, pure False, pure False, pure Nothing)
    WithStateHooks ->
#ifdef SNES_STATES_CPU
      let rest = scRestCpu core
#else
      let rest = scRest core
#endif
          ho = snesStateHooks link menuPause hold rest (scV core) (scOverscan core) (scTop core) (scPortRd core) (scOvlDone core)
      in (hoCtl ho, hoOvl ho, hoFrozen ho, hoRun ho, hoByte ho)
  -- battery saves: the save control, the read back on the byte channel
  saveCtl = register saveCtlInit (saveCtlStep <$> saveCtl <*> bundle (link, scSramWrote core))
  sramRd = register False (isJust . sramRead <$> link)
  chanByte = (\sc rd b sb t -> if scReadBack sc && rd then Just (pack b)
                                else if isJust sb then sb
                                else if snesTrace then pack <$> t else Nothing)
               <$> saveCtl <*> sramRd <*> scSramPs core <*> stByte <*> traceOut
  -- the trace (SNES_TRACE builds only): a line of the core's state a
  -- second on the byte channel, which the PS app prints ("serial: ...")
  traceOut = mealy traceStep traceInit (bundle (scReadBack <$> saveCtl, scDebug core))
  -- the picture: the ring, the stream, the overlay
  (wl, wr) = unbundle (ringWrite <$> scPixel core)
  ringL = blockRam1 NoClearOnReset (SNat @8192) 0 rdAddr wl
  ringR = blockRam1 NoClearOnReset (SNat @8192) 0 rdAddr wr
  -- black while held or paused: the ring has no picture to keep (the
  -- menu shows over black); and after, until the core is in step again
  -- ('pcBlank')
  (rdAddr, (timing, rgb)) = snesStream vga (scOverscan core) ((\h p b -> h || p || b) <$> hold <*> menuPause <*> (pcBlank <$> pace)) ringL ringR
  (timingO, rgbO) = textOverlay (overlayWrite <$> link) (pcOverlay <$> ctrl) (timing, rgb)
  -- the sound
  audio = snesAudio (scSample core)
  frameTog = register False ((\t f f0 -> if f /= f0 then not t else t) <$> frameTog <*> scFrame core <*> register 0 (scFrame core))

-- | The DSP's 32 kHz held between its samples and low-passed (two poles
-- at about 7.8 kHz, the pixel clock's: the 32 kHz steps' images start at
-- 16 kHz), for the 48 kHz samplers that take a level every clock.
snesAudio :: HiddenClockResetEnable dom => Signal dom (Maybe (Signed 16, Signed 16)) -> Signal dom (Signed 16, Signed 16)
snesAudio s = bundle (side fst, side snd)
 where
  held = regMaybe (0, 0) s
  side f = let x = f <$> held in pole (pole x)

-- | A one-pole low-pass at the clock (y += (x - y) / 512), 9 bits of
-- fraction kept.
pole :: HiddenClockResetEnable dom => Signal dom (Signed 16) -> Signal dom (Signed 16)
pole x = (\a -> resize (a `shiftR` 9)) <$> y
 where
  y = register (0 :: Signed 26) ((\a v -> a + (((resize v `shiftL` 9) - a) `shiftR` 9)) <$> y <*> x)

-- | Pad 2 from its link bytes (low, then bits 8-11).
pad2Write :: Unsigned 12 -> Maybe (LinkAddr, BitVector 8) -> Unsigned 12
pad2Write p w = case w of
  Just (a, d)
    | a == pad2Addr -> (p .&. 0xF00) .|. resize (unpack d :: Unsigned 8)
    | a == pad2Addr + 1 -> (p .&. 0x0FF) .|. (resize (unpack d :: Unsigned 8) `shiftL` 8)
  _ -> p

-- | A link write into the cartridge word ('cartAddr' and + 1, low byte
-- first), the other byte kept (from the file's word at first).
cartWord :: Maybe (BitVector 16) -> BitVector 16 -> Maybe (LinkAddr, BitVector 8) -> Maybe (BitVector 16)
cartWord cur file w = case w of
  Just (a, d) | a - cartAddr < 2 ->
    let k = fromEnum (a - cartAddr) * 8
        mask = complement (0xFF `shiftL` k) :: BitVector 16
    in Just ((old .&. mask) .|. (zeroExtend d `shiftL` k))
  _ -> cur
 where
  old = fromMaybe file cur

-- * The trace

-- | The trace on (@make hdl SYSTEM=snes SNES_TRACE=1@ defines SNES_TRACE):
-- a debugging build's, never the normal bitstream's.
snesTrace :: Bool
#ifdef SNES_TRACE
snesTrace = True
#else
snesTrace = False
#endif

data Trace = Trace
  { tTimer :: Unsigned 25          -- ^ clocks to the next line
  , tIdx   :: Maybe (Index 72)     -- ^ the line's next character, while one is sent
  , tGap   :: Unsigned 17          -- ^ clocks to that character
  , tLine  :: Vec 72 (Unsigned 8)
  } deriving stock (Generic)
    deriving anyclass NFDataX

traceInit :: Trace
traceInit = Trace traceEvery Nothing 0 (repeat 0)

-- | A line a second, a character every 2 ms (the PS keeps no FIFO: at most
-- one byte a millisecond or so, "Platform.Ps.Link").
traceEvery :: Unsigned 25
traceGap :: Unsigned 17
#ifdef SNES_TRACE_FAST
-- (SNES_TRACE=fast, for layer 4: a line every 1,500 clocks, a character
-- every 16, which the harness's byte channel takes: --ps-bytes-out; the
-- line, 72 characters, must fit between two, or the trace stops)
traceEvery = 1500
traceGap = 16
#else
traceEvery = 25175000
traceGap = 50350
#endif

-- | One clock: paused while the save's read-back has the channel.
traceStep :: Trace -> (Bool, Vec 24 (Unsigned 8)) -> (Trace, Maybe (Unsigned 8))
traceStep t (busy, dbg)
  | busy = (t, Nothing)
  | otherwise = case tIdx t of
      Nothing
        | tTimer t == 0 -> (t { tTimer = traceEvery, tIdx = Just 0, tGap = 0, tLine = traceLine dbg }, Nothing)
        | otherwise -> (t { tTimer = tTimer t - 1 }, Nothing)
      Just k
        | tGap t /= 0 -> (t { tGap = tGap t - 1, tTimer = tTimer t - 1 }, Nothing)
        | otherwise ->
            ( t { tIdx = if k == maxBound then Nothing else Just (k + 1), tGap = traceGap, tTimer = tTimer t - 1 }
            , Just (tLine t !! k) )

-- | The line: "c BBPPPP pPP nNN i IIIIIIII o OOOOOOOO s SSSS fFF b KK
-- PPPPPP AAAAAA VV" and a newline ("Snes.Core"'s 'debugBytes': the
-- 65C816's bank and PC, P, NMITIMEN, the ports the CPU and the SPC700
-- wrote, the SPC700's PC, the flags; then the first BRK or COP fetched:
-- caught (01), the opcode before it, its address, the byte).
traceLine :: Vec 24 (Unsigned 8) -> Vec 72 (Unsigned 8)
traceLine d = map char template
 where
  template :: Vec 72 (Bool, Unsigned 8)
  template = $(listToVecTH (L.map (\(h, v) -> (h, fromIntegral v :: Unsigned 8)) traceTemplate))
  char (isDigit, v)
    | isDigit = let b = d !! (v `shiftR` 1) in digit (if testBit v 0 then b .&. 0xF else b `shiftR` 4)
    | otherwise = v
  digit n = if n < 10 then 0x30 + n else 0x57 + n
