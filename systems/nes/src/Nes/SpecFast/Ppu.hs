{-# OPTIONS_GHC -fworker-wrapper -fspecialise-aggressively -funfolding-use-threshold=200 -funfolding-creation-threshold=1000 #-}
-- (the fast version is never synthesised: GHC's unboxing back on, which
-- clash-common's -fno-worker-wrapper turns off for Clash's sake)
-- Side track Q (PLAN.md Q.7): the desktop emulator's fast version of
-- Nes.Spec.Ppu, kept beside it (the original stays the reference the
-- core's lockstep checks; a fix in either goes to both). nes-specfast
-- runs both machines side by side and compares them.
{-# LANGUAGE MultiWayIf #-}
{-|
The 2C02 PPU of the reference model (PLAN.md steps 3.2 and 3.3): dot and
frame timing, VBlank and NMI, the registers with their latches (@v@,
@t@, @x@, @w@, the $2007 read buffer, the I/O latch that answers reads of
write-only registers), palette RAM, OAM, and rendering at dot level.

Sources: nesdev wiki, "PPU registers", "PPU scrolling", "PPU rendering",
"PPU frame timing" (and its frame timing diagram), "PPU sprite
evaluation", "PPU sprite priority", "PPU OAM", "PPU palettes", "Open bus
behavior".

__Position.__ The state holds the dot just executed: scanline 0 to 261
(241 is the first VBlank line, 261 the pre-render line), dot 0 to 340.
'ppuDot' advances one dot and then does that dot's work. At power-on the
PPU is at (0, 0). With rendering on (as it was when dot 338 ran), the
pre-render line of every other frame ends after dot 339 (the odd-frame
skip).

__VBlank.__ The flag is set at dot 1 of line 241 and cleared at dot 1
of line 261. Reading $2002 when dot 0 of line 241 has just run (one dot
before the flag would be set) returns it clear and stops it from being
set this frame. The NMI output is the flag ANDed with PPUCTRL bit 7; the
CPU samples it at the end of each of its cycles, so a read that clears
the flag in the same CPU cycle in which it was set also hides the NMI
("Nes.Spec.Machine" runs two dots before each CPU access, and one after).

__Background__ (visible and pre-render lines, rendering on): every 8
dots from dot 1 and from dot 321, the nametable, attribute and two
pattern bytes are fetched (read on the first dot of each pair);
the shifters shift on dots 2 to 257 and 322 to 337 and reload on dots 9,
17, ..., 257, 329 and 337; coarse X increments after each tile, Y at dot
256, horizontal bits copied from @t@ at 257, vertical bits on dots 280
to 304 of the pre-render line. Dummy nametable reads at 338 and 340.

__Sprites__ (visible lines): secondary OAM is cleared on dots 1 to 64
(one byte every two dots); dots 65 to 256 evaluate OAM with OAMADDR as
the counter, a read on odd dots and a write on even ones, including the
overflow search with its @m@ increment bug; dots 257 to 320 fetch the
pattern bytes of the 8 slots (low at dot +4, high at +6; OAMADDR held
at 0). No sprites on line 0:
the pre-render line doesn't evaluate. Sprite 0 hit needs sprite 0 to have
been the first sprite evaluated, opaque pixels in both, not at x = 255,
and neither layer clipped at x < 8.

__Pixels__ go to a 256×240 frame of 9-bit values: palette entry (6 bits)
and the emphasis bits of PPUMASK (bits 6 to 8). With rendering off the
backdrop is shown, or the palette entry @v@ points to if it points into
$3F00-$3FFF.

__During rendering__ a $2007 access increments coarse X and Y instead of
adding 1 or 32; a $2004 write doesn't write and adds 4 to OAMADDR; a
$2004 read returns what the evaluation or fetch is reading.
-}
module Nes.SpecFast.Ppu
  ( Ppu
  , ppuFields
  , PpuBus (..)
  , newPpu
  , ppuDot
  , ppuRegRead
  , ppuRegWrite
  , FetchKind (..)
  , ppuNmi
  , ppuSpriteFlags
  , ppuPosition
  , ppuFrames
  , ppuPeekPalette
  , ppuOamWrite
  , ppuFrame
  , ppuFramesDone
  , ppuSetSkip
  , screenWidth
  , screenHeight
    -- * For the lockstep tests
  , PpuSnap (..)
  , ppuSnap
  , ppuSnapVec
  , ppuPixelAt
  , ppuOamAt
    -- * Save states (side track N)
  , ppuLatch
  ) where

import Prelude

import Control.Monad (forM_, when)
import Control.Monad.ST (ST)
import Data.Bits
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MV
import Platform.Snapshot (Field (..))
import Data.Word

import Nes.Spec.Ppu (FetchKind (..))

-- | The PPU's memory bus below $3F00: pattern tables and nametables,
-- answered by the cartridge (and the console's nametable RAM).
data PpuBus s = PpuBus
  { pbRead  :: Word16 -> ST s Word8
  , pbWrite :: Word16 -> Word8 -> ST s ()
  , pbAddr  :: Word16 -> ST s () -- ^ the address bus shows this without an access: v while
                                  -- rendering is off, after a $2006 write or a $2007 increment
                                  -- (MMC3 watches A12, step 3.10)
  , pbFetch :: FetchKind -> Word16 -> ST s Word8
    -- ^ a rendering fetch, and which (MMC5 tells them apart: extended
    -- attributes, the 8x16 sprite banks); 'pbRead' is $2007's
  }

-- | What a rendering fetch is for (side track D, MMC5): the background's
-- nametable byte (the two unused fetches at the line's end included),
-- attribute byte and pattern bytes, the sprite fetch's two garbage
-- nametable reads and its pattern bytes.
-- (the reference model's type: the cartridge's "Nes.Spec.Mapper" takes it)

data Ppu s = Ppu
  { ppBus    :: !(PpuBus s)
  , ppSt     :: !(MV.MVector s Int)
  , ppPal    :: !(MV.MVector s Word8)  -- ^ 32 bytes
  , ppOam    :: !(MV.MVector s Word8)  -- ^ 256 bytes
  , ppSec    :: !(MV.MVector s Word8)  -- ^ secondary OAM, 32 bytes
  , ppUnits  :: !(MV.MVector s Int)    -- ^ 8 sprite slots × (X, attribute, pattern low, pattern high)
  , ppFrameB :: !(MV.MVector s Word16) -- ^ 256 × 240, being drawn
  , ppLatchT :: !(MV.MVector s Int)    -- ^ per bit of the I/O latch: frame it was last driven
  , ppSprLine :: !(MV.MVector s Int)   -- ^ side track Q: the line's sprite output ('spriteLine'), per x
  }

screenWidth, screenHeight :: Int
screenWidth = 256
screenHeight = 240

-- State slots.
sLine, sDot, sFrames, sCtrl, sMask, sVbl, sSpr0, sOvf, sSuppress, sOamAddr, sV, sT, sX, sW, sBuf, sLatch :: Int
sLine = 0
sDot = 1
sFrames = 2     -- VBlank starts so far
sCtrl = 3
sMask = 4
sVbl = 5
sSpr0 = 6
sOvf = 7
sSuppress = 8   -- VBlank flag suppressed this frame by a $2002 read
sOamAddr = 9
sV = 10
sT = 11
sX = 12
sW = 13
sBuf = 14       -- $2007 read buffer
sLatch = 15     -- I/O data latch (open bus of the PPU registers)

sOdd, sNt, sAt, sPatLo, sPatHi, sBgLo, sBgHi, sAtLo, sAtHi :: Int
sOdd = 16       -- the frame being drawn is odd
sNt = 17        -- fetched nametable byte
sAt = 18        -- fetched attribute bits (2)
sPatLo = 19
sPatHi = 20
sBgLo = 21      -- 16-bit background pattern shifters
sBgHi = 22
sAtLo = 23      -- 16-bit attribute shifters
sAtHi = 24

sOamBus, sEvM, sEvFound, sEvSec, sEvDone, sSpr0Next, sSpr0Cur, sFramesDone :: Int
sOamBus = 25    -- what a $2004 read sees during rendering
sEvM = 26       -- evaluation: byte of the sprite being copied (0 = checking Y)
sEvFound = 27   -- sprites found for the next line
sEvSec = 28     -- secondary OAM's address register (0 to 31)
sEvDone = 29    -- all 64 sprites evaluated
sSpr0Next = 30  -- sprite 0 is in the next line's slots
sSpr0Cur = 31   -- sprite 0 is in this line's slots
sFramesDone = 33 -- frames completely drawn (line 239 finished); slot 32 unused

-- The $2007 read state machine (see 'ppuRegRead').
sRdAle, sRdFill, sRdAddr, sFetchB, sFetchAge :: Int
sRdAle = 35     -- dots until its ALE (0: none)
sRdFill = 36    -- dots until it fills the read buffer (0: none)
sRdAddr = 37    -- the address its ALE latched
sFetchB = 38    -- the byte the last rendering fetch read
sFetchAge = 39  -- dots since then (0: this dot)

-- The $2007 write state machine.
sWrAle, sWrDo, sWrAddr, sWrData :: Int
sWrAle = 40     -- dots until its ALE (0: none)
sWrDo = 41      -- 1: it writes on the next dot
sWrAddr = 42    -- the address its ALE latched
sWrData = 43    -- the byte the CPU wrote

-- Secondary OAM's address register (sEvSec) and the sprite fetch.
sOam2Ovf, sFY, sFT, sFA :: Int
sOam2Ovf = 44   -- the address overflowed: increments frozen (at 0) until a reset
sFY = 45        -- the sprite fetch's latched Y, tile and attribute
sFT = 46
sFA = 47


sRenHist :: Int
sRenHist = 48   -- PPUMASK's rendering bits over the last dots (bit 0: the last one)

sEvPost :: Int
sEvPost = 49    -- this line's overflow sprite and its 3 bytes are read: Y bytes only from here

sFlagPend :: Int
sFlagPend = 50  -- sprite 0 hit ($40) and overflow ($20) found this dot, set from the next

sCounting :: Int
sCounting = 51  -- per sprite slot (bit i): its X counter is counting, set for all at dot 339

sVPend :: Int
sVPend = 52     -- dots until the second $2006 write reaches v (0: none pending)

sVPendVal :: Int
sVPendVal = 53  -- the value it writes

sFetchA :: Int
sFetchA = 54    -- the last nametable or attribute fetch's address (its low byte: the octal latch)

sOamCor :: Int
sOamCor = 55    -- OAM corruption pending: 1 + the row it overwrites (0: none)

-- Side track Q: the sprite output of the line precomputed ('spriteLine').
sSprBuf :: Int
sSprBuf = 56    -- 1: ppSprLine holds this line's output, the units as at its start

sSkip :: Int
sSkip = 57      -- 1: the frame's pixels aren't drawn ('ppuSetSkip')

slots :: Int
slots = 58

newPpu :: PpuBus s -> ST s (Ppu s)
newPpu bus = do
  st <- MV.replicate slots 0
  MV.write st sFetchAge 2
  pal <- MV.replicate 32 0
  oam <- MV.replicate 256 0
  sec <- MV.replicate 32 0xFF
  units <- MV.replicate 32 0
  fb <- MV.replicate (screenWidth * screenHeight) 0
  lt <- MV.replicate 8 0
  sl <- MV.replicate screenWidth 0
  pure (Ppu bus st pal oam sec units fb lt sl)

get :: Ppu s -> Int -> ST s Int
get p = MV.unsafeRead (ppSt p)
{-# INLINE get #-}

set :: Ppu s -> Int -> Int -> ST s ()
set p = MV.unsafeWrite (ppSt p)
{-# INLINE set #-}

-- | Scanline and dot just executed.
ppuPosition :: Ppu s -> ST s (Int, Int)
ppuPosition p = (,) <$> get p sLine <*> get p sDot

-- | VBlank starts so far.
ppuFrames :: Ppu s -> ST s Int
ppuFrames p = get p sFrames

-- | Frames completely drawn so far.
ppuFramesDone :: Ppu s -> ST s Int
ppuFramesDone p = get p sFramesDone

-- | The frame buffer (complete after line 239, until line 0 starts
-- again): palette entry in bits 0 to 5, emphasis in bits 6 to 8.
ppuFrame :: Ppu s -> ST s (U.Vector Word16)
ppuFrame p = U.freeze (ppFrameB p)

-- | The NMI output (active).
-- | The sprite 0 hit and overflow flags as $2002 shows them (bits 6, 5).
ppuSpriteFlags :: Ppu s -> ST s Word8
ppuSpriteFlags p = (\s o -> fromIntegral ((s `shiftL` 6) .|. (o `shiftL` 5))) <$> get p sSpr0 <*> get p sOvf

ppuNmi :: Ppu s -> ST s Bool
ppuNmi p = do
  v <- get p sVbl
  c <- get p sCtrl
  pure (v /= 0 && testBit c 7)

-- | Dots by which the rendering pipeline lags PPUMASK (see 'ppuDot').
renderDelay :: Int
renderDelay = 2

-- | Dots from the second $2006 write to v (TriCNES: 4, or 5 in one
-- CPU/PPU alignment; counted as 'renderDelay' is).
v2006Delay :: Int
v2006Delay = 4

-- | Dots by which fetches and sprite evaluation lag PPUMASK.
fetchDelay :: Int
fetchDelay = 3

rendering :: Ppu s -> ST s Bool
rendering p = (\m -> m .&. 0x18 /= 0) <$> get p sMask
{-# INLINE rendering #-}

-- | One dot.
ppuDot :: Ppu s -> ST s ()
ppuDot p = do
  line <- get p sLine
  dot <- get p sDot
  odd' <- get p sOdd
  -- Sprite 0 hit and overflow found during the last dot show from this one
  -- (AccuracyCoin "$2002 flag timing" 2: with $2002's sprite flags taken
  -- at the end of the CPU cycle, one dot later than they were).
  pend <- get p sFlagPend
  when (pend /= 0) $ do
    when (testBit pend 6) (set p sSpr0 1)
    when (testBit pend 5) (set p sOvf 1)
    set p sFlagPend 0
  -- The second $2006 write reaches v.
  vp <- get p sVPend
  when (vp > 0) $ do
    set p sVPend (vp - 1)
    when (vp == 1) $ do
      t' <- get p sVPendVal
      set p sV t'
      busy <- renderingNow p
      when (not busy) (pbAddr (ppBus p) (fromIntegral (t' .&. 0x3FFF)))
  -- The rendering pipeline sees PPUMASK's rendering bits 'renderDelay'
  -- dots late (nesdev wiki "PPU registers": "toggling rendering takes
  -- effect approximately 3-4 dots after the write").
  renNow <- rendering p
  hist <- get p sRenHist
  set p sRenHist (((hist `shiftL` 1) .|. (if renNow then 1 else 0)) .&. 0xFF)
  let ren = if renderDelay == 0 then renNow else testBit hist (renderDelay - 1)
      -- fetches and sprite evaluation one dot later still (TriCNES: the
      -- "_Delayed" mask bits; shifting and pixels use the 2-dot one)
      renF = testBit hist (fetchDelay - 1)
  -- The odd-frame skip with the pipeline's (delayed) rendering flag: that is
  -- PPUMASK as it was 2 dots before (ppu_vbl_nmi 10-even_odd_timing: one dot
  -- either way fails; this was a special case before the delay existed).
  let (line', dot')
        | line == 261 && dot == 339 && odd' /= 0 && ren = (0, 0)
        | dot < 340 = (line, dot + 1)
        | line < 261 = (line + 1, 0)
        | otherwise = (0, 0)
  set p sLine line'
  set p sDot dot'
  when (line' == 0 && dot' == 0) (set p sOdd (1 - odd'))
  when (dot' == 1) $
    if line' == 241
      then do
        sup <- get p sSuppress
        when (sup == 0) (set p sVbl 1)
        set p sSuppress 0
        n <- get p sFrames
        set p sFrames (n + 1)
      else when (line' == 261) $ do
        set p sVbl 0
        set p sSpr0 0
        set p sOvf 0
  when (line' < 240 || line' == 261) $ do
    oamCorruption p dot' (testBit hist renderDelay) ren
    when (ren || renF) (renderDot p line' dot' renF ren)
    -- secondary OAM's address register ('oam2Reset')
    when (ren && (dot' == 64 || dot' == 256 || dot' == 339)) (oam2Reset p)
    -- the sprite units' X counters start counting ('spriteAt')
    when (ren && dot' == 339) (set p sCounting 0xFF)
    when (line' < 240 && dot' >= 1 && dot' <= 256) (pixel p line' (dot' - 1) ren)
  when (line' == 239 && dot' == 340) $ do
    n <- get p sFramesDone
    set p sFramesDone (n + 1)
  dataMachine p

-- | OAM corruption (AccuracyCoin "OAM Corruption"; TriCNES 2123-2138,
-- 3069-3087, 3150-3200). Rendering turning off on a rendering line seeds
-- it with secondary OAM's address register (on dots 65 to 256 rounded up
-- to a multiple of 4). If rendering comes back on during VBlank, the
-- first rendering dot on a rendering line copies OAM row 0 (bytes 0 to 7)
-- over row seed, and secondary OAM byte 0 over byte seed. If it comes
-- back on during a rendering line, nothing is copied: Blargg's
-- sprite_overflow_tests 5.Emulator (3) turns rendering off and on again
-- mid-frame, with a $2002 poll's jitter, and passes on hardware only so
-- (TriCNES keeps a flag for this case, "RenderingEnabledOutOfVBlank").
-- Given the pipeline's rendering flag for the last dot and this one.
oamCorruption :: Ppu s -> Int -> Bool -> Bool -> ST s ()
oamCorruption p dot wasRen ren = do
  pend <- get p sOamCor
  if pend /= 0
    then when ren $ do
      let row = pend - 1
      when (wasRen && row /= 0) $ do
        forM_ [0 .. 7] $ \i -> MV.unsafeRead (ppOam p) i >>= MV.unsafeWrite (ppOam p) (row * 8 + i)
        MV.unsafeRead (ppSec p) 0 >>= MV.unsafeWrite (ppSec p) row
      set p sOamCor 0
    else when (wasRen && not ren) $ do
      a <- get p sEvSec
      let seed = if dot >= 65 && dot <= 256 then ((a + 3) .&. complement 3) .&. 0x1F else a
      set p sOamCor (seed + 1)

-- * Background

renderDot :: Ppu s -> Int -> Int -> Bool -> Bool -> ST s ()
renderDot p line dot renF renS = do
  when (renS && ((dot >= 2 && dot <= 257) || (dot >= 322 && dot <= 337))) (shiftBg p)
  when (renF && ((dot >= 9 && dot <= 257 && (dot - 1) .&. 7 == 0) || dot == 329 || dot == 337)) (reloadBg p)
  renderFetch p line dot renF renS

-- | The rendering pipeline's fetching side (with the fetch delay: see
-- 'ppuDot'): background fetches, scroll updates, sprites.
renderFetch :: Ppu s -> Int -> Int -> Bool -> Bool -> ST s ()
renderFetch p line dot renF renS = do
  let pre = line == 261
      fetching = (dot >= 1 && dot <= 256) || (dot >= 321 && dot <= 336)
  -- Memory is read on the first dot of each 2-dot fetch (the address dot;
  -- the hardware reads on the second, which only differs when the CPU
  -- writes in between). The Clash PPU issues the address there and gets
  -- the data with the next dot.
  when (renF && fetching) $ case (dot - 1) .&. 7 of
    0 -> do
      v <- get p sV
      set p sFetchA (0x2000 .|. (v .&. 0x0FFF))
      b <- fetchRd p FkNt (0x2000 .|. fromIntegral (v .&. 0x0FFF))
      set p sNt (fromIntegral b)
    -- The read dot: the address's high byte comes from the current v, the
    -- low byte from the octal latch (TriCNES 4126-4199). When v changed
    -- in between (a $2006 write landing), that "hybrid" address is read
    -- again (AccuracyCoin "Hybrid Addresses").
    1 -> do
      v <- get p sV
      a <- get p sFetchA
      let h = 0x2000 .|. (v .&. 0x0F00) .|. (a .&. 0xFF)
      when (h /= a) (fetchRd p FkNt (fromIntegral h) >>= set p sNt . fromIntegral)
    2 -> do
      v <- get p sV
      let a = 0x23C0 .|. (v .&. 0x0C00) .|. ((v `shiftR` 4) .&. 0x38) .|. ((v `shiftR` 2) .&. 0x07)
      set p sFetchA a
      b <- fetchRd p FkAt (fromIntegral a)
      let sh = ((v `shiftR` 4) .&. 4) .|. (v .&. 2)
      set p sAt ((fromIntegral b `shiftR` sh) .&. 3)
    3 -> do
      v <- get p sV
      a <- get p sFetchA
      let h = 0x2300 .|. (v .&. 0x0C00) .|. (a .&. 0xFF)
          sh = ((v `shiftR` 4) .&. 4) .|. (v .&. 2)
      when (h /= a) (fetchRd p FkAt (fromIntegral h) >>= \b -> set p sAt ((fromIntegral b `shiftR` sh) .&. 3))
    4 -> bgPattern 0 >>= set p sPatLo
    6 -> bgPattern 8 >>= set p sPatHi
    7 -> incX p
    _ -> pure ()
  when (renF && dot == 256) (incY p)
  when (renF && pre && dot >= 280 && dot <= 304) $ do
    v <- get p sV
    t <- get p sT
    set p sV ((v .&. complement 0x7BE0) .|. (t .&. 0x7BE0))
  when (renF && (dot == 338 || dot == 340)) $ do
    v <- get p sV
    b <- fetchRd p FkNt (0x2000 .|. fromIntegral (v .&. 0x0FFF))
    set p sNt (fromIntegral b)
  -- The idle dot 0 shows the pattern address the dot 5 fetch will use
  -- (nesdev wiki, "PPU rendering"): with the background at $1000 A12 is
  -- high here, so the MMC3 doesn't see a long low stretch from dot 337
  -- to dot 4 (mmc3_test_2 4-scanline_timing). Not on the pre-render line: the
  -- address comes from the previous line's unused fetches, and a VBlank line
  -- has none (the counter is clocked at its dot 5 then).
  when (renF && dot == 0 && not pre) $ do
    c <- get p sCtrl
    nt <- get p sNt
    v <- get p sV
    pbAddr (ppBus p) (fromIntegral (((c .&. 0x10) `shiftL` 8) .|. (nt `shiftL` 4) .|. ((v `shiftR` 12) .&. 7)))
  -- Sprites. The pre-render line doesn't clear or evaluate, but it does
  -- fetch, from what line 239 left in secondary OAM.
  -- (its clear phase steps the address register without writing: the seed
  -- of 'oamCorruption')
  when renS $ if pre
    then do
      when (dot >= 1 && dot <= 64 && even dot) (oam2Inc p)
      when (dot >= 257 && dot <= 320) (fetchPhase p line dot)
    else spriteDot p line dot
  -- The horizontal bits from t, after dot 257's garbage nametable fetch:
  -- that one still reads with the old v (AccuracyCoin, "$2007 Stress
  -- Test", "common mistakes").
  when (renF && dot == 257) $ do
    v <- get p sV
    t <- get p sT
    set p sV ((v .&. complement 0x041F) .|. (t .&. 0x041F))
 where

  bgPattern off = do
    c <- get p sCtrl
    nt <- get p sNt
    v <- get p sV
    let a = ((c .&. 0x10) `shiftL` 8) .|. (nt `shiftL` 4) .|. ((v `shiftR` 12) .&. 7) .|. off
    -- With the $2007 read machine reading on this address dot (ALE and
    -- read together), the octal latch keeps the bus's last byte, the last
    -- read's data, so that becomes the address's low byte (TriCNES
    -- 4136-4139, 2425-2441; AccuracyCoin "ALE + Read").
    fill <- get p sRdFill
    lo <- get p sFetchB
    let a' = if fill == 1 then (a .&. 0xFF00) .|. lo else a
    fromIntegral <$> fetchRd p FkBgPat (fromIntegral a')

-- | One shift of the background shifters. Their serial inputs: 0 into the
-- low pattern plane, 1 into the high one, and the attribute latch into the
-- attribute shifters, whose low byte here holds that latch's 8 copies
-- (TriCNES; AccuracyCoin "BG Serial In": with rendering toggled so the
-- shifters run past a reload, the pixels show these bits).
shiftBg :: Ppu s -> ST s ()
shiftBg p = do
  sh sBgLo (const 0)
  sh sBgHi (const 1)
  sh sAtLo (.&. 1)
  sh sAtHi (.&. 1)
 where
  sh s inp = get p s >>= \x -> set p s (((x `shiftL` 1) .|. inp x) .&. 0xFFFF)

reloadBg :: Ppu s -> ST s ()
reloadBg p = do
  lo <- get p sPatLo
  hi <- get p sPatHi
  at <- get p sAt
  let fill b = if b then 0xFF else 0
  get p sBgLo >>= \x -> set p sBgLo ((x .&. 0xFF00) .|. lo)
  get p sBgHi >>= \x -> set p sBgHi ((x .&. 0xFF00) .|. hi)
  get p sAtLo >>= \x -> set p sAtLo ((x .&. 0xFF00) .|. fill (testBit at 0))
  get p sAtHi >>= \x -> set p sAtHi ((x .&. 0xFF00) .|. fill (testBit at 1))

incX :: Ppu s -> ST s ()
incX p = do
  v <- get p sV
  set p sV (if v .&. 0x1F == 31 then (v .&. complement 0x1F) `xor` 0x400 else v + 1)

incY :: Ppu s -> ST s ()
incY p = do
  v <- get p sV
  if v .&. 0x7000 /= 0x7000
    then set p sV (v + 0x1000)
    else do
      let y = (v .&. 0x3E0) `shiftR` 5
          (y', flipNt) | y == 29 = (0, True)
                       | y == 31 = (0, False)
                       | otherwise = (y + 1, False)
          v1 = (v .&. complement 0x73E0) .|. (y' `shiftL` 5)
      set p sV (if flipNt then v1 `xor` 0x800 else v1)

-- * Sprites

spriteHeight :: Ppu s -> ST s Int
spriteHeight p = (\c -> if testBit c 5 then 16 else 8) <$> get p sCtrl

spriteDot :: Ppu s -> Int -> Int -> ST s ()
spriteDot p line dot
  | dot >= 1 && dot <= 64 = do
      set p sOamBus 0xFF
      when (even dot) $ do
        a <- get p sEvSec
        MV.unsafeWrite (ppSec p) a 0xFF
        oam2Inc p
  | dot >= 65 && dot <= 256 = do
      when (dot == 65) $ do
        set p sEvM 0
        set p sEvFound 0
        set p sEvDone 0
        set p sEvPost 0
        set p sSpr0Next 0
      if odd dot
        then do
          a <- get p sOamAddr
          MV.unsafeRead (ppOam p) a >>= set p sOamBus . fromIntegral
        else evaluate p line dot
  | dot >= 257 && dot <= 320 = fetchPhase p line dot
  | dot >= 321 = oam2Read p >>= set p sOamBus
  | otherwise = pure ()

-- | Secondary OAM at its address register.
oam2Read :: Ppu s -> ST s Int
oam2Read p = get p sEvSec >>= fmap fromIntegral . MV.unsafeRead (ppSec p)

-- | The address register counts 0 to 31; wrapping raises the "overflowed"
-- flag, which freezes it (at 0) until a reset ('oam2Reset'). AccuracyCoin
-- "Frozen OAM2 Increment", "$2004 Stress Test": a full secondary OAM, or
-- the end of the sprite fetch, leaves it frozen if rendering is off at the
-- next reset.
oam2Inc :: Ppu s -> ST s ()
oam2Inc p = do
  o <- get p sOam2Ovf
  when (o == 0) $ do
    a <- get p sEvSec
    if a == 31 then set p sEvSec 0 >> set p sOam2Ovf 1 else set p sEvSec (a + 1)

-- | After the clear (dot 64), the evaluation (256) and the fetch (339; 340
-- is skipped on odd frames), if rendering runs then ('ppuDot', with the
-- pipeline's delay): address 0, flag down. AccuracyCoin "Frozen OAM2
-- Increment" names dots 63, 255 and 339.
oam2Reset :: Ppu s -> ST s ()
oam2Reset p = set p sEvSec 0 >> set p sOam2Ovf 0

-- | An even evaluation dot: act on the byte read on the odd dot before.
evaluate :: Ppu s -> Int -> Int -> ST s ()
evaluate p line dot = do
  done <- get p sEvDone
  b <- get p sOamBus
  a <- get p sOamAddr
  found <- get p sEvFound
  m <- get p sEvM
  h <- spriteHeight p
  let inRange = line - b >= 0 && line - b < h
      -- Advance OAMADDR; an overflow of n ends the evaluation.
      advance a' = do
        set p sOamAddr (a' .&. 0xFF)
        when (a' > 0xFF) (set p sEvDone 1)
      -- Once all 64 are done or secondary OAM is full, the write becomes a
      -- read of secondary OAM at its address ($2004 shows it).
      readBack = oam2Read p >>= set p sOamBus
  if | done /= 0 -> readBack >> advance (a + 4)
     | found < 8 -> do
         sec <- get p sEvSec
         MV.unsafeWrite (ppSec p) sec (fromIntegral b)
         if m == 0
           then if inRange
             then do
               when (dot == 66) (set p sSpr0Next 1)
               oam2Inc p
               set p sEvM 1
               advance (a + 1)
             -- out of range: n moves on and OAMADDR is realigned
             -- (AccuracyCoin "Misaligned OAM behavior" 2: +4, AND $FC)
             else advance ((a + 4) .&. 0x1FC)
           else do
             oam2Inc p
             if m == 3
               then do
                 set p sEvM 0
                 set p sEvFound (found + 1)
                 -- The X byte goes through the same range check: out of
                 -- range, OAMADDR is realigned after its +1 (AccuracyCoin
                 -- d103cbd, "Misaligned OAM behavior" 4 to 6)
                 advance (if inRange then a + 1 else (a + 1) .&. 0x1FC)
               else set p sEvM (m + 1) >> advance (a + 1)
     | m == 0 -> do
         readBack
         post <- get p sEvPost
         if | post /= 0 ->
                -- after the overflow sprite: n only, the Y bytes
                advance ((a .&. 0x1FC) + 4)
            | inRange -> do
                get p sFlagPend >>= set p sFlagPend . (.|. 0x20)
                set p sEvM 1
                advance (a + 1)
            | otherwise ->
                -- The hardware bug: n and m both increment (m without carry).
                let a' = (a + 4) .&. 0x1FC .|. ((a + 1) .&. 3)
                in advance a'
     | otherwise -> do
         -- the overflow sprite's next 3 bytes; then m drops to 0 without
         -- carrying into n (AccuracyCoin "$2004 Stress Test", second key:
         -- A0 A1 A2, then A0 A4 A8 ...)
         readBack
         if m == 3
           then set p sEvM 0 >> set p sEvPost 1 >> advance ((a + 1) .&. 0x1FC)
           else set p sEvM (m + 1) >> advance (a + 1)

-- | Dots 257 to 320: slot @i@ = (dot - 257) / 8 reads its secondary OAM
-- bytes and fetches its pattern (low at dot +4, high at +6).
spriteFetch :: Ppu s -> Int -> Int -> ST s ()
spriteFetch p line dot = do
  let i = (dot - 257) `shiftR` 3
      k = (dot - 257) .&. 7
  -- Secondary OAM through its address register: Y, tile and attribute
  -- (latched, the register moving on after each), then X five times, the
  -- register moving on after the last. Frozen, every read is byte 0
  -- (AccuracyCoin "Frozen OAM2 Increment").
  o2 <- oam2Read p
  set p sOamBus o2
  when (k <= 2) (set p ([sFY, sFT, sFA] !! k) o2)
  when (k <= 2 || k == 7) (oam2Inc p)
  y <- get p sFY
  tile <- get p sFT
  attr <- get p sFA
  let x = o2
  when (k == 0 || k == 2) $ do
    -- garbage nametable fetches
    v <- get p sV
    _ <- fetchRd p FkSprNt (0x2000 .|. fromIntegral (v .&. 0x0FFF))
    pure ()
  when (k == 4 || k == 6) $ do
    c <- get p sCtrl
    h <- spriteHeight p
    -- The fetch checks the range again, with the line number's low 8 bits:
    -- the pre-render line counts as line 5 (AccuracyCoin, "Sprites On
    -- Scanline 0"). Unused slots hold $FF or a Y that was out of range.
    let rowRaw = (line .&. 0xFF) - y
        row0 = rowRaw .&. (h - 1)
        row = if testBit attr 7 then h - 1 - row0 else row0
        addr
          | h == 16 = ((tile .&. 1) `shiftL` 12) .|. (((tile .&. 0xFE) + (row `shiftR` 3)) `shiftL` 4) .|. (row .&. 7)
          | otherwise = ((c .&. 0x08) `shiftL` 9) .|. (tile `shiftL` 4) .|. row
        real = rowRaw >= 0 && rowRaw < h
    b <- fromIntegral <$> fetchRd p FkSprPat (fromIntegral (addr .|. (if k == 6 then 8 else 0)))
    let b' | not real = 0
           | testBit attr 6 = reverseBits8 b
           | otherwise = b
    MV.unsafeWrite (ppUnits p) (i * 4 + (if k == 6 then 3 else 2)) b'
    when (k == 6) $ do
      MV.unsafeWrite (ppUnits p) (i * 4) x
      MV.unsafeWrite (ppUnits p) (i * 4 + 1) attr

-- | Dots 257 to 320: OAMADDR held at 0, the slots for the next line.
fetchPhase :: Ppu s -> Int -> Int -> ST s ()
fetchPhase p line dot = do
  set p sOamAddr 0
  when (dot == 257) (get p sSpr0Next >>= set p sSpr0Cur)
  spriteFetch p line dot

reverseBits8 :: Int -> Int
reverseBits8 b = foldr (\i acc -> if testBit b i then setBit acc (7 - i) else acc) 0 [0 .. 7]

-- * Pixels

-- | Frame skipping (side track Q, the desktop's): while on, 'pixel' draws
-- nothing; the sprite units still run, and sprite 0 hit is still found.
ppuSetSkip :: Ppu s -> Bool -> ST s ()
ppuSetSkip p on = set p sSkip (if on then 1 else 0)

pixel :: Ppu s -> Int -> Int -> Bool -> ST s ()
pixel p line x ren = do
  skip <- get p sSkip
  if skip /= 0 then pixelSkipped p x ren else pixelDrawn p line x ren

-- | A pixel not drawn: only what the CPU can see of it, the sprite units'
-- state and sprite 0 hit (as 'pixelDrawn').
pixelSkipped :: Ppu s -> Int -> Bool -> ST s ()
pixelSkipped p x ren
  | not ren = () <$ spriteLine p x False
  | otherwise = do
      packed <- spriteLine p x True
      s0 <- get p sSpr0Cur
      when (s0 /= 0 && packed /= 0 && packed `shiftR` 10 == 0 && x /= 255) $ do
        mask <- get p sMask
        fx <- get p sX
        lo <- get p sBgLo
        hi <- get p sBgHi
        let bit' = 15 - fx
            bgOn = testBit mask 3 && (x >= 8 || testBit mask 1)
            spOn = testBit mask 4 && (x >= 8 || testBit mask 2)
        when (bgOn && spOn && (testBit lo bit' || testBit hi bit')) (get p sFlagPend >>= set p sFlagPend . (.|. 0x40))

pixelDrawn :: Ppu s -> Int -> Int -> Bool -> ST s ()
pixelDrawn p line x ren = do
  mask <- get p sMask
  out <- if not ren
    then do
      _ <- spriteLine p x False  -- the counters still count
      v <- get p sV
      let i = if v .&. 0x3F00 == 0x3F00 then palIndex (fromIntegral v) else 0
      MV.unsafeRead (ppPal p) i
    else do
      fx <- get p sX
      lo <- get p sBgLo
      hi <- get p sBgHi
      al <- get p sAtLo
      ah <- get p sAtHi
      let bit' = 15 - fx
          bgOn = testBit mask 3 && (x >= 8 || testBit mask 1)
          bgPix = if bgOn then (if testBit lo bit' then 1 else 0) .|. (if testBit hi bit' then 2 else 0) else 0 :: Int
          bgPal = (if testBit al bit' then 1 else 0) .|. (if testBit ah bit' then 2 else 0) :: Int
          spOn = testBit mask 4 && (x >= 8 || testBit mask 2)
      packed <- spriteLine p x True
      let spPix0 = packed .&. 3
          spAttr = (packed `shiftR` 2) .&. 0xFF
          slot = if spPix0 == 0 then -1 else packed `shiftR` 10
      let spPix = if spOn then spPix0 else 0
      s0 <- get p sSpr0Cur
      when (slot == 0 && s0 /= 0 && bgPix /= 0 && spPix /= 0 && x /= 255) (get p sFlagPend >>= set p sFlagPend . (.|. 0x40))
      let idx | bgPix == 0 && spPix == 0 = 0
              | bgPix == 0 = 0x10 .|. ((spAttr .&. 3) `shiftL` 2) .|. spPix
              | spPix == 0 = (bgPal `shiftL` 2) .|. bgPix
              | testBit spAttr 5 = (bgPal `shiftL` 2) .|. bgPix
              | otherwise = 0x10 .|. ((spAttr .&. 3) `shiftL` 2) .|. spPix
      MV.unsafeRead (ppPal p) (palIndex (fromIntegral idx))
  let grey = if testBit mask 0 then 0x30 else 0x3F
      c = fromIntegral (out .&. grey) .|. ((fromIntegral mask .&. 0xE0) `shiftL` 1) :: Word16
  MV.unsafeWrite (ppFrameB p) (line * screenWidth + x) c

-- | Side track Q: 'spriteAt' for pixel @x@ of the line, packed (pixel in
-- bits 0-1, attribute 2-9, slot 10-12; 0: none), and fast. While rendering
-- stays on for the whole line, the units' output over the line follows
-- from their state at its start: a counting slot with X counter c starts
-- drawing at pixel c (a slot not counting at once), and shifts once a
-- pixel from then on. So pixel 0 computes the 256 pixels ('buildLine'),
-- and the units stay as they were until the line ends, or until a pixel
-- with rendering off, where their state is computed for that pixel
-- ('settleLine') and 'spriteAt' takes over for the rest of the line.
spriteLine :: Ppu s -> Int -> Bool -> ST s Int
spriteLine p x ren = do
  buf <- get p sSprBuf
  if | buf /= 0 && ren -> do
         v <- MV.unsafeRead (ppSprLine p) x
         when (x == screenWidth - 1) (settleLine p screenWidth)
         pure v
     | buf /= 0 -> settleLine p x >> slow
     | x == 0 && ren -> buildLine p >> MV.unsafeRead (ppSprLine p) 0
     | otherwise -> slow
 where
  slow = do
    (px, attr, slot) <- spriteAt p ren
    pure (if px == 0 then 0 else px .|. (attr `shiftL` 2) .|. (slot `shiftL` 10))

-- | The line's sprite output from the units' state now (see 'spriteLine').
buildLine :: Ppu s -> ST s ()
buildLine p = do
  let out = ppSprLine p
  MV.set out 0
  counting <- get p sCounting
  -- slot 7 first: a lower slot's opaque pixel wins
  forM_ [7, 6 .. 0] $ \i -> do
    c <- MV.unsafeRead (ppUnits p) (i * 4)
    attr <- MV.unsafeRead (ppUnits p) (i * 4 + 1)
    lo <- MV.unsafeRead (ppUnits p) (i * 4 + 2)
    hi <- MV.unsafeRead (ppUnits p) (i * 4 + 3)
    when (lo .|. hi /= 0) $ do
      let start = if testBit counting i then c else 0
          go k
            | k >= 8 || start + k >= screenWidth = pure ()
            | otherwise = do
                let b = 7 - k
                    px = (if testBit lo b then 1 else 0) .|. (if testBit hi b then 2 else 0)
                when (px /= 0) (MV.unsafeWrite out (start + k) (px .|. (attr `shiftL` 2) .|. (i `shiftL` 10)))
                go (k + 1)
      go 0
  set p sSprBuf 1

-- | The units as 'spriteAt' would have left them after @n@ pixels with
-- rendering on, from their state at the line's start; the precomputed line
-- given up.
settleLine :: Ppu s -> Int -> ST s ()
settleLine p n = do
  counting <- get p sCounting
  let go i cnt
        | i >= 8 = set p sCounting cnt
        | otherwise = do
            c <- MV.unsafeRead (ppUnits p) (i * 4)
            let counts = testBit counting i
                -- a counting slot counts down for c pixels, the next one
                -- clears its flag; then (and a slot not counting at once)
                -- it shifts once a pixel
                shifts | not counts = n
                       | n > c = n - c
                       | otherwise = 0
                cnt' | counts && n > c = clearBit cnt i
                     | otherwise = cnt
                sh v = if shifts >= 8 then 0 else (v `shiftL` shifts) .&. 0xFF
            when counts (MV.unsafeWrite (ppUnits p) (i * 4) (if n > c then 0 else c - n))
            when (shifts > 0) $ do
              MV.unsafeModify (ppUnits p) sh (i * 4 + 2)
              MV.unsafeModify (ppUnits p) sh (i * 4 + 3)
            go (i + 1) cnt'
  go 0 counting
  set p sSprBuf 0

-- | The sprite units at one visible dot, as the hardware: a slot whose X
-- counter is counting counts down to 0 (also while rendering is off); a
-- slot that isn't counting, or has reached 0, outputs bit 7 of its two
-- shift registers, which shift once per dot only while rendering. The
-- counters start counting at dot 339 if rendering then; otherwise they
-- keep their state, so a slot that wasn't counting draws at once
-- (AccuracyCoin "Stale Sprite Shift Regs" 2, 3, 5; "Stale BG Shift
-- Registers" 4). Returns the first opaque output (pixel, attribute,
-- slot), then clocks every slot.
spriteAt :: Ppu s -> Bool -> ST s (Int, Int, Int)
spriteAt p ren = do
  counting <- get p sCounting
  r <- go counting 0
  forM_ [0 .. 7] $ \i -> do
    c <- MV.unsafeRead (ppUnits p) (i * 4)
    if testBit counting i && c > 0
      then MV.unsafeWrite (ppUnits p) (i * 4) (c - 1)
      else do
        -- at 0 the slot switches to halted, and stays so until dot 339
        when (testBit counting i) (get p sCounting >>= set p sCounting . (`clearBit` i))
        when ren $ do
          MV.unsafeModify (ppUnits p) (\v -> (v `shiftL` 1) .&. 0xFF) (i * 4 + 2)
          MV.unsafeModify (ppUnits p) (\v -> (v `shiftL` 1) .&. 0xFF) (i * 4 + 3)
  pure r
 where
  go counting i
    | i >= 8 = pure (0, 0, -1)
    | otherwise = do
        c <- MV.unsafeRead (ppUnits p) (i * 4)
        if testBit counting i && c /= 0
          then go counting (i + 1)
          else do
            lo <- MV.unsafeRead (ppUnits p) (i * 4 + 2)
            hi <- MV.unsafeRead (ppUnits p) (i * 4 + 3)
            let px = (if testBit lo 7 then 1 else 0) .|. (if testBit hi 7 then 2 else 0)
            if px == 0
              then go counting (i + 1)
              else do
                attr <- MV.unsafeRead (ppUnits p) (i * 4 + 1)
                pure (px, attr, i)

-- * Registers

renderingNow :: Ppu s -> ST s Bool
renderingNow p = do
  ren <- rendering p
  line <- get p sLine
  pure (ren && (line < 240 || line == 261))

-- | CPU read of register 0 to 7 ($2000 + n, mirrored).
ppuRegRead :: Ppu s -> Int -> ST s Word8
ppuRegRead p r = case r of
  2 -> do
    line <- get p sLine
    dot <- get p sDot
    vbl <- get p sVbl
    s0 <- get p sSpr0
    ov <- get p sOvf
    latch <- latchValue p
    when (line == 241 && dot == 0) (set p sSuppress 1)
    let v = (vbl `shiftL` 7) .|. (s0 `shiftL` 6) .|. (ov `shiftL` 5) .|. (latch .&. 0x1F)
    set p sVbl 0
    set p sW 0
    latchDrive p 0xE0 v
    pure (fromIntegral v)
  4 -> do
    busy <- renderingNow p
    v <- if busy
      then fromIntegral <$> get p sOamBus
      else do
        a <- get p sOamAddr
        b <- MV.unsafeRead (ppOam p) a
        -- (attribute bytes are stored without bits 2 to 4: 'ppuOamWrite')
        pure b
    latchDrive p 0xFF (fromIntegral v)
    pure v
  7 -> do
    v <- get p sV
    let a = fromIntegral (v .&. 0x3FFF) :: Word16
    out <- if a >= 0x3F00
      then do
        latch <- latchValue p
        c <- MV.unsafeRead (ppPal p) (palIndex a)
        mask <- get p sMask
        -- Greyscale mode applies to reads too.
        let grey = if testBit mask 0 then 0x30 else 0x3F
        latchDrive p 0x3F (fromIntegral c .&. grey)
        pure ((fromIntegral c .&. grey) .|. (latch .&. 0xC0))
      else do
        old <- get p sBuf
        latchDrive p 0xFF old
        pure old
    -- The memory read and the increment come later ('dataMachine'); a
    -- read before the ALE of the last one starts it again (one ALE).
    set p sRdAle 3
    pure (fromIntegral out)
  _ -> fromIntegral <$> latchValue p

-- | The $2007 read and write state machines, once per dot after the dot's work
-- (AccuracyCoin, "$2007 Stress Test", its timeline of an LDA $2007: ALE
-- in the second PPU cycle after the CPU's read cycle ends, the memory
-- read two cycles after that). The CPU cycle ends one dot after the
-- access ("Nes.Spec.Machine"), so the ALE is the 3rd dot after the read:
-- it latches v (palette addresses: the nametable byte under them). The
-- 5th dot reads memory into the buffer, then increments v. While rendering
-- fetches, the PPU's bus carries the fetch's byte then: with a fetch on
-- that dot or the one before (the spec reads on the first dot of a fetch,
-- the hardware on the second), the buffer gets that byte.
--
-- So two reads on consecutive CPU cycles (Blargg double_2007_read) both
-- increment, and the buffer ends up with the byte of the second address:
-- "22 44 55 66 77", one of the results the test lists for real consoles;
-- and a write on the cycle after a read (STA $2007,X) lands at the
-- incremented address (Blargg read_write_2007).
dataMachine :: Ppu s -> ST s ()
dataMachine p = do
  age <- get p sFetchAge
  when (age < 2) (set p sFetchAge (age + 1))
  fill <- get p sRdFill
  when (fill > 0) $ do
    set p sRdFill (fill - 1)
    when (fill == 1) $ do
      a <- get p sRdAddr
      b <- if age == 0 || age == 1 then get p sFetchB else fromIntegral <$> pbRead (ppBus p) (fromIntegral a)
      set p sBuf b
      increment p
  ale <- get p sRdAle
  when (ale > 0) $ do
    set p sRdAle (ale - 1)
    when (ale == 1) $ do
      v <- get p sV
      let a = v .&. 0x3FFF
      set p sRdAddr (if a >= 0x3F00 then a - 0x1000 else a)
      set p sRdFill 2
  -- Writes: ALE on the 3rd dot after the CPU's write too, the write on the
  -- 4th (the timeline's t3.5 to t4.0), then the increment. A memory write
  -- on a dot where rendering fetches is lost (the bus is the fetch's; the
  -- hardware does something unstable there).
  wdo <- get p sWrDo
  when (wdo /= 0) $ do
    set p sWrDo 0
    a <- get p sWrAddr
    d <- fromIntegral <$> get p sWrData
    if a >= 0x3F00
      then MV.unsafeWrite (ppPal p) (palIndex (fromIntegral a)) (d .&. 0x3F)
      else when (age /= 0) (pbWrite (ppBus p) (fromIntegral a) d)
    increment p
  wale <- get p sWrAle
  when (wale > 0) $ do
    set p sWrAle (wale - 1)
    when (wale == 1) $ do
      v <- get p sV
      set p sWrAddr (v .&. 0x3FFF)
      set p sWrDo 1

-- | A rendering fetch: the read, remembered for the $2007 read buffer
-- ('dataMachine').
fetchRd :: Ppu s -> FetchKind -> Word16 -> ST s Word8
fetchRd p k a = do
  b <- pbFetch (ppBus p) k a
  set p sFetchB (fromIntegral b)
  set p sFetchAge 0
  pure b

-- | After a $2007 access: +1 or +32, or during rendering both scroll
-- increments.
increment :: Ppu s -> ST s ()
increment p = do
  busy <- renderingNow p
  if busy
    then incX p >> incY p
    else do
      c <- get p sCtrl
      v <- get p sV
      set p sV ((v + (if testBit c 2 then 32 else 1)) .&. 0x7FFF)
      get p sV >>= pbAddr (ppBus p) . fromIntegral . (.&. 0x3FFF)

-- | CPU write of register 0 to 7.
ppuRegWrite :: Ppu s -> Int -> Word8 -> ST s ()
ppuRegWrite p r w = do
  latchDrive p 0xFF d
  case r of
    0 -> do
      set p sCtrl d
      t <- get p sT
      set p sT ((t .&. 0x73FF) .|. ((d .&. 3) `shiftL` 10))
    1 -> set p sMask d
    3 -> set p sOamAddr d
    4 -> do
      busy <- renderingNow p
      if busy
        then get p sOamAddr >>= \a -> set p sOamAddr ((a + 4) .&. 0xFC)
        else ppuOamWrite p w
    5 -> do
      wl <- get p sW
      t <- get p sT
      if wl == 0
        then do
          set p sT ((t .&. 0x7FE0) .|. (d `shiftR` 3))
          set p sX (d .&. 7)
          set p sW 1
        else do
          set p sT ((t .&. 0x0C1F) .|. ((d .&. 7) `shiftL` 12) .|. ((d .&. 0xF8) `shiftL` 2))
          set p sW 0
    6 -> do
      wl <- get p sW
      t <- get p sT
      if wl == 0
        then do
          set p sT ((t .&. 0x00FF) .|. ((d .&. 0x3F) `shiftL` 8))
          set p sW 1
        else do
          let t' = (t .&. 0x7F00) .|. d
          set p sT t'
          -- v follows 'v2006Delay' dots later ('ppuDot')
          set p sVPend v2006Delay
          set p sVPendVal t'
          set p sW 0
    7 -> do
      -- written later, by the write state machine ('dataMachine')
      set p sWrData (fromIntegral w)
      set p sWrAle 3
    _ -> pure () -- $2002 is read-only
 where
  d = fromIntegral w :: Int

-- | A write to OAMDATA outside rendering (also OAM DMA's put cycles).
ppuOamWrite :: Ppu s -> Word8 -> ST s ()
ppuOamWrite p w = do
  a <- get p sOamAddr
  -- Bits 2 to 4 of attribute bytes don't exist (2C02G): every read, the
  -- evaluation's too, sees them 0 (AccuracyCoin, "$2004 Stress Test").
  MV.unsafeWrite (ppOam p) a (if a .&. 3 == 2 then w .&. 0xE3 else w)
  set p sOamAddr ((a + 1) .&. 0xFF)

-- | Palette RAM index: $3F10, $3F14, $3F18, $3F1C mirror $3F00, $3F04,
-- $3F08, $3F0C.
palIndex :: Word16 -> Int
palIndex a = let i = fromIntegral a .&. 0x1F in if i .&. 0x13 == 0x10 then i .&. 0x0F else i

ppuPeekPalette :: Ppu s -> Int -> ST s Word8
ppuPeekPalette p i = MV.unsafeRead (ppPal p) (i .&. 0x1F)

-- * The I/O latch

-- | Frames an undriven bit of the I/O latch keeps its value (AccuracyCoin
-- recommends 5 to 30; nesdev measured about 600 ms).
latchFrames :: Int
latchFrames = 30

-- | The latch with bits that weren't driven for 'latchFrames' decayed to 0.
latchValue :: Ppu s -> ST s Int
latchValue p = do
  now <- get p sFrames
  v <- get p sLatch
  ages <- mapM (\i -> (\t -> now - t) <$> MV.unsafeRead (ppLatchT p) i) [0 .. 7]
  let v' = foldr (\(i, age) acc -> if age >= latchFrames then clearBit acc i else acc) v (zip [0 ..] ages)
  set p sLatch v'
  pure v'

-- | The I/O latch as a read of a write-only register sees it (side track
-- N's state table).
ppuLatch :: Ppu s -> ST s Int
ppuLatch = latchValue

-- | Drive the bits in the mask with the value (refreshing them).
latchDrive :: Ppu s -> Int -> Int -> ST s ()
latchDrive p mask v = do
  now <- get p sFrames
  old <- latchValue p
  set p sLatch ((old .&. complement mask) .|. (v .&. mask))
  forM_ [0 .. 7] $ \i -> when (testBit mask i) (MV.unsafeWrite (ppLatchT p) i now)

-- * Snapshots for the lockstep tests (step 3.5)

-- | The PPU's state in a form "Nes.Ppu" can be compared with: the named
-- registers, secondary OAM, the sprite units (X, attribute, low, high per
-- slot) and palette RAM.
data PpuSnap = PpuSnap
  { snFields :: [(String, Int)]
  , snSec    :: [Word8]
  , snUnits  :: [Int]
  , snPal    :: [Word8]
  } deriving stock (Show, Eq)

ppuSnap :: Ppu s -> ST s PpuSnap
ppuSnap p = do
  fs <- mapM (\(n, s) -> (,) n <$> get p s)
    [ ("line", sLine), ("dot", sDot), ("odd", sOdd), ("ctrl", sCtrl), ("mask", sMask), ("vbl", sVbl)
    , ("spr0", sSpr0), ("ovf", sOvf), ("suppress", sSuppress), ("oamAddr", sOamAddr), ("v", sV), ("t", sT)
    , ("x", sX), ("w", sW), ("buf", sBuf), ("nt", sNt), ("at", sAt), ("patLo", sPatLo), ("patHi", sPatHi)
    , ("bgLo", sBgLo), ("bgHi", sBgHi), ("atLo", sAtLo), ("atHi", sAtHi), ("oamBus", sOamBus), ("evM", sEvM)
    , ("evFound", sEvFound), ("evSec", sEvSec), ("evDone", sEvDone), ("spr0Next", sSpr0Next)
    , ("spr0Cur", sSpr0Cur), ("rdAle", sRdAle), ("rdFill", sRdFill), ("rdAddr", sRdAddr)
    , ("wrAle", sWrAle), ("wrDo", sWrDo), ("wrAddr", sWrAddr), ("wrData", sWrData)
    , ("oam2Ovf", sOam2Ovf), ("fY", sFY), ("fT", sFT), ("fA", sFA), ("evPost", sEvPost), ("flagPend", sFlagPend), ("counting", sCounting)
    , ("vPend", sVPend), ("vPendVal", sVPendVal), ("fetchA", sFetchA), ("oamCor", sOamCor) ]
  sec <- mapM (MV.unsafeRead (ppSec p)) [0 .. 31]
  units <- mapM (MV.unsafeRead (ppUnits p)) [0 .. 31]
  pal <- mapM (MV.unsafeRead (ppPal p)) [0 .. 31]
  pure (PpuSnap fs sec units pal)

-- | The frame buffer at (x, y).
ppuPixelAt :: Ppu s -> Int -> Int -> ST s Word16
ppuPixelAt p x y = MV.unsafeRead (ppFrameB p) (y * screenWidth + x)

ppuOamAt :: Ppu s -> Int -> ST s Word8
ppuOamAt p i = MV.unsafeRead (ppOam p) (i .&. 0xFF)

-- | 'ppuSnap' as one flat vector (fields, secondary OAM, units, palette),
-- for a fast comparison every dot.
ppuSnapVec :: Ppu s -> ST s (U.Vector Int)
ppuSnapVec p = do
  fs <- mapM (get p) snapSlots
  sec <- mapM (fmap fromIntegral . MV.unsafeRead (ppSec p)) [0 .. 31]
  units <- mapM (MV.unsafeRead (ppUnits p)) [0 .. 31]
  pal <- mapM (fmap fromIntegral . MV.unsafeRead (ppPal p)) [0 .. 31]
  pure (U.fromList (fs <> sec <> units <> pal))

snapSlots :: [Int]
snapSlots =
  [ sLine, sDot, sOdd, sCtrl, sMask, sVbl, sSpr0, sOvf, sSuppress, sOamAddr, sV, sT, sX, sW, sBuf, sNt, sAt
  , sPatLo, sPatHi, sBgLo, sBgHi, sAtLo, sAtHi, sOamBus, sEvM, sEvFound, sEvSec, sEvDone, sSpr0Next
  , sSpr0Cur, sRdAle, sRdFill, sRdAddr, sWrAle, sWrDo, sWrAddr, sWrData
  , sOam2Ovf, sFY, sFT, sFA, sEvPost, sFlagPend, sCounting, sVPend, sVPendVal, sFetchA, sOamCor ]

-- | The PPU's state for a save state (Q.8, "Platform.Snapshot").
ppuFields :: Ppu s -> [Field s]
ppuFields p =
  [ FInt (ppSt p), FW8 (ppPal p), FW8 (ppOam p), FW8 (ppSec p), FInt (ppUnits p)
  , FW16 (ppFrameB p), FInt (ppLatchT p), FInt (ppSprLine p) ]
