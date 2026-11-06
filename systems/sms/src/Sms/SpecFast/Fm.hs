{-# OPTIONS_GHC -fworker-wrapper -fspecialise-aggressively -funfolding-use-threshold=200 -funfolding-creation-threshold=1000 #-}
-- (the fast version is never synthesised: GHC's unboxing back on, which
-- clash-common's -fno-worker-wrapper turns off for Clash's sake)
-- Side track Q (PLAN.md Q.7): the desktop emulator's fast version of
-- Sms.Spec.Fm, kept beside it (the original stays the reference the
-- core's lockstep and the oracle tests check; a fix in either goes to
-- both). The sms-specfast suite runs both machines side by side.
{-# LANGUAGE BangPatterns #-}
{-|
The FM unit's YM2413 (OPLL), the reference model (M6 step 6.3). Written
from docs/systems/ym2413.md (its sections in brackets below), values and
rules only; checked pin for pin against Nuked-OPLL run as a black box
(`cabal test sms-spec`; the user's choice, 2026-10-11). Where the doc
leaves a rule open (when exactly a write is seen, how rhythm mode switches
inside a sample), the oracle settled it; those rules are marked
"(oracle)".

__Time.__ One internal cycle is 4 master clocks, and the SMS feeds the chip
the Z80's 3.579545 MHz, so a cycle is 4 T-states and a sample 18 cycles (72
T-states, 49 715.9 Hz) [2.1]. The model runs a sample at a time in the
doc's frame order [10.2]: frame f covers cycles 17 of sample f - 1 to 16 of
sample f, slot s's parameters are fetched at cycle s - 1 (absolute cycle
18 f + s - 1), and the slots are worked in order 0 to 17. A register write
is applied before every slot whose fetch comes after the write lands, so
writes inside a sample are exact [3.4, 10.3].

__Writes__ ('fmWrite', at an absolute cycle: the cycle before whose clock
the CPU's strobe is there): an address byte with bits 7 and 6 clear
selects a register (00h-3Fh), anything else makes the data that follows
land nowhere. Data for a mode register (00h-07h, 0Eh, 0Fh) lands at the
strobe's cycle, so a fetch at a later cycle sees it (oracle). Data for a
channel register (10h-3Fh) lands at the first cycle after the strobe's
whose number is the address's low nibble (15 at most: the mirrors at
19h-1Fh, 29h-2Fh, 39h-3Fh, channel c mod 9) (oracle) [3.2, 3.4].

__Rhythm mode__ (0Eh bit 5) is read at several points of the pipeline,
each at its own cycle (oracle; the doc's 10.3 says this much and no more):
whether slots 12-17 are drums (their patches, levels, keys and envelope
rules) once per sample, as slot 12 is fetched; HH's, SD's and TCY's special
phases, and the capture of TCY's phase bits, at the slot's own cycle; HH's
and TOM's phase reset rule and feedback, and SD's and TCY's modulation, a
cycle before; HH's and TOM's output stage (feedback kept or not, which
half-sine bit, carrier or drum) two cycles after; the pins two cycles before
they show. The function 'rhythmAt' is that bit at an absolute cycle. One
thing this doesn't follow: in the sample where rhythm mode switches, the
chip can give a drum another value (its stages picking the level and patch
see the switch at other cycles); the oracle tests allow it, a drum's pins
in that sample only.

__Output__ ('fmTakePins'): the MO and RO pins for every cycle, as the chip
puts them out [2.3, 9.1]: per sample each melodic channel once on MO, each
drum twice on RO (its value latched once per sample), the rest of the
cycles idle (+1 or -1, the sign of what is in the pipeline); D = v + 1 for
v >= 0, else v, from the operator's y as v = y >> 3. 'fmMix' is a sample's
sum over its 18 cycles of both pins, as the analogue side adds them (the
drums count twice, melody and drums the same sign; the idle cycles give a
small offset the audio's high-pass removes) [9.2].
-}
module Sms.SpecFast.Fm
  ( Fm
  , fmFields
  , newFm
  , fmWrite
  , fmRunTo
  , fmSamples
  , fmTakePins
  , fmMix
  , fmLastMix
  , fmPatchRom
  , fmMargin
  ) where

import Prelude

import Control.Monad (forM_, when)
import Control.Monad.ST (ST)
import Data.Bits
import Data.STRef
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MV
import Platform.Snapshot (Field (..))
import Data.Word (Word8)

data Fm s = Fm
  { fRegs    :: !(MV.MVector s Int)   -- ^ 64 registers as written (00h-3Fh)
  , fSlot    :: !(MV.MVector s Int)   -- ^ per slot: see the slot fields below
  , fPend    :: !(STRef s [(Int, Int, Int)]) -- ^ writes not yet applied: (landing cycle, register, value), in landing order
  , fRhy     :: !(STRef s [(Int, Bool)]) -- ^ 0Eh bit 5's changes: (landing cycle, value), newest first
  , fAddr    :: !(STRef s (Maybe Int)) -- ^ the selected register, as of the strobes so far
  , fFrame   :: !(STRef s Int)        -- ^ frames run
  , fNoise   :: !(STRef s (Int, Int)) -- ^ the LFSR and the steps taken since reset
  , fTcyBits :: !(STRef s Int)        -- ^ TCY's (slot 17's) 10-bit phase as last captured
  , fHhPhi   :: !(STRef s Int)        -- ^ HH's (slot 13's) 10-bit phase, captured this frame
  , fGroup   :: !(STRef s Bool)       -- ^ slots 12-17 are drums this frame (rhythm mode as slot 12 was fetched)
  , fOuts    :: !(STRef s (U.Vector Int, U.Vector Int)) -- ^ every slot's v (y >> 3): the last frame, the one before
  , fPins    :: !(STRef s [U.Vector (Int, Int)]) -- ^ samples' pins, newest first (unused: 'fmLastMix')
  , fMix     :: !(STRef s Int)        -- ^ side track Q: the last completed sample's 'fmMix'
  }

-- Per-slot fields (fSlot, 8 per slot).
oP, oL, oSt, oY1, oY2, oEvPrev, oYCur, slotWords :: Int
oP = 0       -- phase accumulator, 19 bits
oL = 1       -- EG level, 0..127
oSt = 2      -- EG state
oY1 = 3      -- the operator's last output kept for feedback
oY2 = 4      -- the one before
oEvPrev = 5  -- carriers: an attack event last frame (resets the modulator's phase)
oYCur = 6    -- this frame's output (a carrier reads its modulator's)
slotWords = 8

stAttack, stDecay, stSustain, stRelease :: Int
stAttack = 0
stDecay = 1
stSustain = 2
stRelease = 3

newFm :: ST s (Fm s)
newFm = do
  regs <- MV.replicate 64 0
  slot <- MV.replicate (18 * slotWords) 0
  forM_ [0 .. 17] $ \s -> do
    MV.write slot (s * slotWords + oL) 127
    MV.write slot (s * slotWords + oSt) stRelease
  Fm regs slot <$> newSTRef [] <*> newSTRef [] <*> newSTRef Nothing <*> newSTRef 0
    <*> newSTRef (0, 0) <*> newSTRef 0 <*> newSTRef 0 <*> newSTRef False
    <*> newSTRef (U.replicate 18 0, U.replicate 18 0) <*> newSTRef [] <*> newSTRef 0

-- | A write strobe before the clock of absolute cycle @cyc@: port 0 the
-- address, 1 the data. Writes come in time order, and 'fmRunTo' must not
-- have run past @cyc@ (it keeps 'fmMargin' cycles behind).
fmWrite :: Fm s -> Int -> Int -> Word8 -> ST s ()
fmWrite fm cyc port v
  | even port = do
      let a = fromIntegral v :: Int
      writeSTRef (fAddr fm) (if a .&. 0xC0 == 0 then Just a else Nothing)
  | otherwise = do
      sel <- readSTRef (fAddr fm)
      forM_ sel $ \a -> do
        let val = fromIntegral v
        if a < 0x10
          then when (a <= 7 || a == 0x0E || a == 0x0F) $ do
            queue cyc a val
            when (a == 0x0E) $ modifySTRef' (fRhy fm) ((cyc, testBit val 5) :)
          else do
            -- the first cycle after the strobe's with the address's number
            let c = a .&. 0x0F
                at = cyc + 1
                base = at - (at `mod` 18) + c
                land = if base > at then base else base + 18
                reg = (a .&. 0x30) .|. (c `mod` 9)
            -- a second channel write before the first landed overwrites the
            -- chip's one latch, whatever its register ([3.4]); one that has
            -- landed stays, even if its frame hasn't run yet
            modifySTRef' (fPend fm) (filter (\(t, r, _) -> r < 0x10 || t <= at))
            queue land reg val
  where
    -- kept in landing order; stable for equal cycles
    queue at r val = modifySTRef' (fPend fm) (\ps -> let (x, y) = span (\(t, _, _) -> t <= at) ps in x <> [(at, r, val)] <> y)

-- | How far behind the last write 'fmRunTo' keeps the frames: a frame
-- reads rhythm mode up to two cycles past its last fetch.
fmMargin :: Int
fmMargin = 4

-- | Run every frame that can't be changed by a write at cycle @cyc@ or
-- later.
fmRunTo :: Fm s -> Int -> ST s ()
fmRunTo fm cyc = do
  f <- readSTRef (fFrame fm)
  when (18 * f + 16 + fmMargin < cyc) $ runFrame fm >> fmRunTo fm cyc

-- | Samples whose pins are complete.
fmSamples :: Fm s -> ST s Int
fmSamples fm = readSTRef (fFrame fm)

-- | The pins of the samples completed since the last call, oldest first:
-- per sample, its 18 cycles' (MO, RO).
fmTakePins :: Fm s -> ST s [U.Vector (Int, Int)]
fmTakePins fm = do
  o <- readSTRef (fPins fm)
  writeSTRef (fPins fm) []
  pure (reverse o)

-- | The last completed sample's 'fmMix' (side track Q: what the FM unit
-- reads; the pins themselves aren't kept, 'fmTakePins' gives none).
fmLastMix :: Fm s -> ST s Int
fmLastMix fm = readSTRef (fMix fm)

-- | A sample's sum of both pins over its 18 cycles.
fmMix :: U.Vector (Int, Int) -> Int
fmMix = U.foldl' (\acc (m, r) -> acc + m + r) 0

-- | 0Eh bit 5 as the pipeline sees it at absolute cycle @k@: the last
-- change that landed before @k@.
rhythmAt :: Fm s -> Int -> ST s Bool
rhythmAt fm k = do
  hs <- readSTRef (fRhy fm)
  -- (side track Q: the first change before k, without building a list)
  let go [] = False
      go ((l, b) : rest) = if l < k then b else go rest
  pure (go hs)

-- * Tables [4.2, 5.2, 6.6, 7.1]

-- | The built-in instruments 1-15 and the drums R1-R3 (Nuked-OPLL's set,
-- the doc's 4.2; index 0 unused: the user's patch).
fmPatchRom :: U.Vector Word8
fmPatchRom = U.fromList $ replicate 8 0 <> concat
  [ [0x71, 0x61, 0x1E, 0x17, 0xD0, 0x78, 0x00, 0x17]
  , [0x13, 0x41, 0x1A, 0x0D, 0xD8, 0xF7, 0x23, 0x13]
  , [0x13, 0x01, 0x99, 0x00, 0xF2, 0xC4, 0x11, 0x23]
  , [0x31, 0x61, 0x0E, 0x07, 0xA8, 0x64, 0x70, 0x27]
  , [0x32, 0x21, 0x1E, 0x06, 0xE0, 0x76, 0x00, 0x28]
  , [0x31, 0x22, 0x16, 0x05, 0xE0, 0x71, 0x00, 0x18]
  , [0x21, 0x61, 0x1D, 0x07, 0x82, 0x81, 0x10, 0x07]
  , [0x23, 0x21, 0x2D, 0x14, 0xA2, 0x72, 0x00, 0x07]
  , [0x61, 0x61, 0x1B, 0x06, 0x64, 0x65, 0x10, 0x17]
  , [0x41, 0x61, 0x0B, 0x18, 0x85, 0xF7, 0x71, 0x07]
  , [0x13, 0x01, 0x83, 0x11, 0xFA, 0xE4, 0x10, 0x04]
  , [0x17, 0xC1, 0x24, 0x07, 0xF8, 0xF8, 0x22, 0x12]
  , [0x61, 0x50, 0x0C, 0x05, 0xC2, 0xF5, 0x20, 0x42]
  , [0x01, 0x01, 0x55, 0x03, 0xC9, 0x95, 0x03, 0x02]
  , [0x61, 0x41, 0x89, 0x03, 0xF1, 0xE4, 0x40, 0x13]
  , [0x01, 0x01, 0x18, 0x0F, 0xDF, 0xF8, 0x6A, 0x6D]   -- R1: BD
  , [0x01, 0x01, 0x00, 0x00, 0xC8, 0xD8, 0xA7, 0x48]   -- R2: HH (mod side), SD (car side)
  , [0x05, 0x01, 0x00, 0x00, 0xF8, 0xAA, 0x59, 0x55]   -- R3: TOM (mod side), TCY (car side)
  ]

mult2 :: U.Vector Int
mult2 = U.fromList [1, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 20, 24, 24, 30, 30]

kslRom :: U.Vector Int
kslRom = U.fromList [0, 32, 40, 45, 48, 51, 53, 55, 56, 58, 59, 60, 61, 62, 63, 64]

logsinTab, expTab :: U.Vector Int
logsinTab = U.generate 256 $ \i -> round (negate (logBase 2 (sin ((fromIntegral i + 0.5) * pi / 512 :: Double))) * 256)
expTab = U.generate 256 $ \i -> round ((2 :: Double) ** (fromIntegral (255 - i) / 256) * 1024)

slotChannel :: U.Vector Int
slotChannel = U.fromList [0, 1, 2, 0, 1, 2, 3, 4, 5, 3, 4, 5, 6, 7, 8, 6, 7, 8]

isCarrier :: Int -> Bool
isCarrier s = odd (s `div` 3)

-- * A frame

runFrame :: Fm s -> ST s ()
runFrame fm = do
  f <- readSTRef (fFrame fm)
  outs <- MV.replicate 18 0
  -- (side track Q: the EG clock's locks and the tremolo once a frame)
  let (shiftLock, lowLock) = egLocks f
      amNow = amOutOf f
      go s = when (s < 18) $ do
        applyWrites fm (18 * f + s - 1)
        slotStep fm f shiftLock lowLock amNow s outs
        go (s + 1)
  go 0
  writeSTRef (fFrame fm) (f + 1)
  v <- U.freeze outs
  (prev, _) <- readSTRef (fOuts fm)
  writeSTRef (fOuts fm) (v, prev)
  pins fm f prev v >>= writeSTRef (fMix fm) . fmMix
  -- forget rhythm changes no lookup can reach any more
  modifySTRef' (fRhy fm) (\hs -> let (new, old) = span (\(l, _) -> l >= 18 * f - 18) hs in new <> take 1 old)

-- | Apply the writes that land before absolute cycle @fetch@.
applyWrites :: Fm s -> Int -> ST s ()
applyWrites fm fetch = do
  p <- readSTRef (fPend fm)
  case p of
    [] -> pure ()
    (at0, _, _) : _
      | at0 >= fetch -> pure ()
      | otherwise -> do
          let (now, later) = span (\(at, _, _) -> at < fetch) p
          forM_ now $ \(_, r, v) -> MV.write (fRegs fm) r v
          writeSTRef (fPend fm) later

sget :: Fm s -> Int -> Int -> ST s Int
sget fm s o = MV.unsafeRead (fSlot fm) (s * slotWords + o)

sset :: Fm s -> Int -> Int -> Int -> ST s ()
sset fm s o = MV.unsafeWrite (fSlot fm) (s * slotWords + o)

-- | The EG clock's locks of frame f: (shiftLock, lowLock) [6.4].
egLocks :: Int -> (Int, Int)
egLocks f = (shiftLock, n .&. 3)
 where
  n = (f `div` 4) .&. 0x3FFFF
  shiftLock = if n == 0 then 0 else let z = countTrailingZeros n + 1 in if z <= 13 then z else 0

-- | The tremolo's output in frame f [6.7].
amOutOf :: Int -> Int
amOutOf f = tri (max 0 ((f - 1) `div` 64) `mod` 210) `shiftR` 3
 where
  tri n = if n <= 105 then n else 210 - n

slotStep :: Fm s -> Int -> Int -> Int -> Int -> Int -> MV.MVector s Int -> ST s ()
slotStep fm f shiftLock lowLock amNow s outs = do
  let rg = MV.unsafeRead (fRegs fm)
      ch = slotChannel U.! s
      car = isCarrier s
      cyc k = 18 * f + k   -- an absolute cycle of this frame's sample
  r0e <- rg 0x0E
  test <- rg 0x0F
  -- rhythm mode, as each part of the pipeline sees it (see the header)
  when (s == 12) $ writeSTRef (fGroup fm) (testBit r0e 5)
  grp <- readSTRef (fGroup fm)
  rNow <- rhythmAt fm (cyc s)          -- the slot's own cycle
  rBefore <- rhythmAt fm (cyc (s - 1)) -- a cycle before
  rOut <- rhythmAt fm (cyc (s + 2))    -- its output stage
  let drum = s >= 12 && grp
      hhTom = drum && (s == 13 || s == 14)   -- HH, TOM: single carriers for the EG
      singleOp = (s == 13 || s == 14) && rBefore   -- no feedback, own phase reset
      outCarrier = (s == 13 || s == 14) && rOut     -- output stage as a carrier
      noMod = (s == 16 || s == 17) && rBefore       -- SD, TCY: no modulation
  f1 <- rg (0x10 + ch)
  f2 <- rg (0x20 + ch)
  f3 <- rg (0x30 + ch)
  -- (side track Q: the user patch's 8 registers read once each, no vector)
  u0 <- rg 0
  u1 <- rg 1
  u2 <- rg 2
  u3 <- rg 3
  u4 <- rg 4
  u5 <- rg 5
  u6 <- rg 6
  u7 <- rg 7
  let user i = case i of
        0 -> u0
        1 -> u1
        2 -> u2
        3 -> u3
        4 -> u4
        5 -> u5
        6 -> u6
        _ -> u7
  let fnum = f1 .|. ((f2 .&. 1) `shiftL` 8)
      block = (f2 `shiftR` 1) .&. 7
      sus = testBit f2 5
      inst = f3 `shiftR` 4
      vol = f3 .&. 0x0F
      byte i
        | drum = fromIntegral (fmPatchRom U.! ((10 + ch) * 8 + i))   -- ch 6, 7, 8: R1, R2, R3
        | inst == 0 = user (i :: Int)
        | otherwise = fromIntegral (fmPatchRom U.! (inst * 8 + i))
      side0 = byte (if car then 1 else 0)
      am = testBit side0 7
      vib = testBit side0 6
      egTyp = testBit side0 5
      ksrBit = testBit side0 4
      mult = side0 .&. 0x0F
      ksl = if car then byte 3 `shiftR` 6 else byte 2 `shiftR` 6
      tl = byte 2 .&. 0x3F
      fb = byte 3 .&. 7
      halfSine = if car || outCarrier then testBit (byte 3) 4 else testBit (byte 3) 3
      arDr = byte (if car then 5 else 4)
      slRr = byte (if car then 7 else 6)
      ar = arDr `shiftR` 4
      dr = arDr .&. 0x0F
      sl = slRr `shiftR` 4
      rr = slRr .&. 0x0F
      -- the drums' keys, ORed with the channel's [3.2]
      kon = testBit f2 4 || (drum && testBit r0e (case s of
        12 -> 4
        15 -> 4
        13 -> 0
        16 -> 3
        14 -> 2
        _ -> 1))
      -- the level: TL for modulators (and BD's), the volume for carriers,
      -- the drums' high nibbles for HH and TOM [6.6, 8.1]
      tlAtt
        | hhTom = 8 * inst
        | car = 8 * vol
        | otherwise = 2 * tl
  -- [6.2] the rate
  l <- sget fm s oL
  st <- sget fm s oSt
  let off = (l `shiftR` 2) == 31
      ev = st == stRelease && kon && off
      rBase
        | ev = ar
        | st == stAttack = ar
        | st == stDecay = dr
        | st == stSustain = if egTyp then 0 else rr
        | otherwise = if sus then 5 else rr
      r
        | not kon && not car && not hhTom = 0
        | kon && st == stRelease && not off = 12
        | not kon && not sus && car && not egTyp = 7
        | otherwise = rBase
      zr = r == 0
      ksr4 = block * 2 + (fnum `shiftR` 8)
      ksr = if ksrBit then ksr4 else ksr4 `shiftR` 2
      rr0 = 4 * r + ksr
      rR = if rr0 >= 64 then 60 + (ksr .&. 3) else rr0
      rh = rR `shiftR` 2
      rl = rR .&. 3
      sCount = (if s == 17 then f + 1 else f) .&. 3
      h = testBit (case rl of { 0 -> 0x0; 1 -> 0x1; 2 -> 0x5; _ -> 0x7 :: Int }) lowLock
      lo
        | rh < 12 && not zr = case (shiftLock + rh) `mod` 16 of
            12 -> True
            13 -> testBit rl 1
            14 -> testBit rl 0
            _ -> False
        | otherwise = False
      decayStep
        | rh == 15 = 2
        | rh == 14 = if h then 2 else 1
        | rh == 13 = if h then 1 else (if odd sCount then 1 else 0)
        | rh == 12 = if h then (if odd sCount then 1 else 0) else (if sCount == 3 then 1 else 0)
        | otherwise = if lo && sCount == 3 then 1 else 0
      attackN
        | rh < 12 = if lo then Just 4 else Nothing
        | rh <= 14 = Just (5 - (rh - 11 + fromEnum h))
        | otherwise = Nothing
      attackDelta = maybe 0 (\n -> (negate l - 1) `shiftR` n) attackN
  -- [6.6] the attenuation, from the level as left by the last update
  let kslRaw = max 0 (kslRom U.! (fnum `shiftR` 5) - 8 * (8 - block))
      kslAtt = case ksl of
        0 -> 0
        1 -> (2 * kslRaw) `shiftR` 2
        2 -> (2 * kslRaw) `shiftR` 1
        _ -> 2 * kslRaw
      amAtt = if am then amNow else 0
      att = if testBit test 0 then 0 else min 127 (l + kslAtt + tlAtt + amAtt)
  -- [5] the phase
  p <- sget fm s oP
  let v = (f `shiftR` 10) .&. 7
      f0 = 2 * fnum
      delta
        | not vib = 0
        | otherwise = case v of
            1 -> f0 `shiftR` 8
            2 -> f0 `shiftR` 7
            3 -> f0 `shiftR` 8
            5 -> negate (f0 `shiftR` 8)
            6 -> negate (f0 `shiftR` 7)
            7 -> negate (f0 `shiftR` 8)
            _ -> 0
      fv = f0 + delta   -- 11 bits: no wrap at the top F-number (oracle)
      inc = (((fv `shiftL` block) `shiftR` 1) * (mult2 U.! mult)) `shiftR` 1
      phi = (p `shiftR` 9) .&. 1023
  -- the phase reset: carriers (and HH, TOM as drums) by their own attack
  -- event, modulators by their carrier's of the last frame [5.4]
  evCar <- if car then pure False else (/= 0) <$> sget fm (s + 3) oEvPrev
  let reset = testBit test 2 || (if car || singleOp then ev else evCar)
  -- [7.3, 8.3] the operator's input
  when (s == 13) $ writeSTRef (fHhPhi fm) phi
  hhPhi <- readSTRef (fHhPhi fm)
  tcyBits <- readSTRef (fTcyBits fm)
  noiseBit <- if rNow && (s == 13 || s == 16) then noiseAt fm (cyc s) else pure 0
  let hb i = (hhPhi `shiftR` i) .&. 1
      pBit t = (hb 2 `xor` hb 7) .|. (hb 3 `xor` ((t `shiftR` 5) .&. 1)) .|. (((t `shiftR` 3) .&. 1) `xor` ((t `shiftR` 5) .&. 1))
      tcyNow = if s == 17 && rNow then phi else tcyBits
  when (s == 17 && rNow) $ writeSTRef (fTcyBits fm) phi
  modIn <- if car && not noMod
    then (\y -> (y .&. 0x1FF) `shiftL` 1) <$> sget fm (s - 3) oYCur
    else if not car && fb > 0 && not singleOp
      then do
        y1 <- sget fm s oY1
        y2 <- sget fm s oY2
        pure (((y1 + y2) `shiftR` 1) `shiftR` (7 - fb))
      else pure 0
  let idx0
        | rNow && s == 13 = let pb = pBit tcyBits in (pb `shiftL` 9) .|. (if (pb `xor` noiseBit) /= 0 then 0xD0 else 0x34)
        | rNow && s == 16 = let h8 = hb 8 in (h8 `shiftL` 9) .|. ((h8 `xor` noiseBit) `shiftL` 8)
        | rNow && s == 17 = (pBit tcyNow `shiftL` 9) .|. 0x100
        | otherwise = (phi + modIn) .&. 1023
      idx = if testBit idx0 8 then idx0 `xor` 0xFF else idx0
      neg = testBit idx 9
      a = min 4095 (logsinTab U.! (idx .&. 0xFF) + 16 * att)
      silent = l == 127
      sh = (a `shiftR` 8) .|. (if silent || (neg && halfSine) then 12 else 0)
      y0 = (expTab U.! (a .&. 0xFF)) `shiftR` sh
      y = if neg && not silent then complement y0 else y0
  -- outputs: modulators keep theirs for feedback (not HH and TOM as
  -- carriers), every slot its current one for its carrier [7.3, 8.1]
  sset fm s oYCur y
  when (not car && not outCarrier) $ do
    sget fm s oY1 >>= sset fm s oY2
    sset fm s oY1 y
  MV.write outs s (y `shiftR` 3)
  -- [5.1] the phase write-back
  sset fm s oP (((if reset then 0 else p) + inc) .&. 0x7FFFF)
  -- [6.3] the level and state
  let l1 = if st /= stAttack && off && not ev then 127 else l
      l2 = if ev && rh == 15 then 0 else l1
      -- (side track Q: two strict values, not a lazy pair)
      !step
        | st == stAttack = if rh /= 15 && kon && l /= 0 then attackDelta else 0
        | st == stDecay = if not off && not ev && (l `shiftR` 3) /= sl then decayStep else 0
        | otherwise = if not off && not ev then decayStep else 0
      !next0
        | st == stAttack = if l == 0 then stDecay else stAttack
        | st == stDecay = if (l `shiftR` 3) == sl then stSustain else stDecay
        | otherwise = st
      next1 = if not kon then stRelease else next0
      next = if ev then stAttack else next1
  sset fm s oL (max 0 (min 127 (l2 + step)))
  sset fm s oSt next
  when car $ sset fm s oEvPrev (fromEnum ev)

-- | Sample f's pins [2.3, 9.1] (oracle for the gates): the pin at cycle P
-- shows what the operator's output stage had two cycles before (op cycle k
-- = P - 2: slot k - 2's v, or a drum's latched value on a modulator's
-- cycle), gated by rhythm mode at k. @prev@, @cur@: frames f - 1 and f.
pins :: Fm s -> Int -> U.Vector Int -> U.Vector Int -> ST s (U.Vector (Int, Int))
pins fm f prev cur = U.generateM 18 $ \c -> do
  let pAbs = 18 * f + c
      k = pAbs - 2
      ck = k `mod` 18
      -- slot (ck - 2) mod 18 of the frame its output stage belongs to
      slotOut = (ck - 2) `mod` 18
      vOf = if k - 2 - slotOut >= 18 * f then cur U.! slotOut else prev U.! slotOut
      -- the drums' values, latched in the previous sample's frame (f - 1)
      held = case ck of
        2 -> Just (prev U.! 13)    -- HH
        3 -> Just (prev U.! 14)    -- TOM
        4 -> Just (prev U.! 15)    -- BD
        8 -> Just (prev U.! 16)    -- SD
        9 -> Just (prev U.! 17)    -- TCY
        _ -> Nothing
  rk <- rhythmAt fm k
  let isMod1 = if rk && (ck == 15 || ck == 16) then False else odd ((ck + 1) `div` 3)
      chOut
        | isMod1 = if rk then maybe 0 id held else 0
        | otherwise = vOf
      cj = (k + 1) `mod` 18
      muteM = odd (cj `div` 3) || (rk && (cj + 15) `mod` 18 >= 12)
      muteR = not (rk && testBit (0x3063F :: Int) cj)   -- cycles 0-5, 9, 10, 16, 17
      sign0 = if chOut < 0 then -1 else 0
      !val = if chOut >= 0 then chOut + 1 else chOut
      !sign = if chOut >= 0 then sign0 + 1 else sign0
  pure (if muteM then sign else val, if muteR then sign else val)

-- | The noise LFSR's bit 0 after @n@ steps since reset (one step per cycle,
-- [8.2]); stepped forward from the last call.
noiseAt :: Fm s -> Int -> ST s Int
noiseAt fm n = do
  (r0, k0) <- readSTRef (fNoise fm)
  let go !r !k
        | k >= n = (r, k)
        | otherwise =
            let nb = if r == 0 then 1 else (r .&. 1) `xor` ((r `shiftR` 14) .&. 1)
            in go ((r `shiftR` 1) .|. (nb `shiftL` 22)) (k + 1)
      (!r1, !k1) = go r0 k0
  r1 `seq` k1 `seq` writeSTRef (fNoise fm) (r1, k1)
  pure (r1 .&. 1)

-- | The YM2413's state for a save state (Q.8, "Platform.Snapshot"): its
-- registers and slots, the writes on their way, the rhythm, the noise,
-- the last outputs. Not the pins' log (unused).
fmFields :: Fm s -> [Field s]
fmFields fm =
  [ FInt (fRegs fm), FInt (fSlot fm), FRef (fPend fm), FRef (fRhy fm), FRef (fAddr fm), FRef (fFrame fm)
  , FRef (fNoise fm), FRef (fTcyBits fm), FRef (fHhPhi fm), FRef (fGroup fm), FRef (fOuts fm), FRef (fMix fm) ]
