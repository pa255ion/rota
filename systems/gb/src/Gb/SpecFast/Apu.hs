{-# OPTIONS_GHC -fworker-wrapper -fspecialise-aggressively -funfolding-use-threshold=200 -funfolding-creation-threshold=1000 #-}
-- (the fast version is never synthesised: GHC's unboxing back on, which
-- clash-common's -fno-worker-wrapper turns off for Clash's sake)
-- The Game Boy's fast version (side track Q for side track W, the user
-- 2026-11-07: the browser's Game Boy Color too slow on a laptop): begun as
-- a copy of Gb.Spec.Apu, which stays the reference; changed here for speed.
-- The same machine: gb-specfast runs it beside the reference on every
-- input.
{-|
The sound unit (APU) of the reference model, PLAN.md step 2.9. Rules from
Pan Docs ("Audio Overview", "Audio Registers", "Audio Details", including
its "Obscure Behavior" list), DMG behaviour.

__Timing.__ 'apuTick' runs once per M-cycle in the machine's tick, after
the timer: the DIV-APU frame sequencer steps on a falling edge of system
counter bit 12 (DIV bit 4, 512 Hz; a DIV write that clears a set bit 12
steps it too, 'apuDivWrite'); steps 0, 2, 4, 6 clock the length timers,
2 and 6 the CH1 sweep, 7 the envelopes. The pulse period dividers count up
once per M-cycle (1048576 Hz), the wave divider twice (2097152 Hz); on
overflow past 2047 they reload from the period value and advance the duty
step / wave sample. The noise LFSR is clocked every @divisor × 2^shift@
dots (divisor 8, 16, 32, ..., 112; none for shift 14 and 15).

__Registers__ keep the written values in the machine's I/O backing store;
reads OR in the unused bits ('Gb.SpecFast.Machine.apuReadMask'); NR52 reads
power and the channels' on bits. Power off clears 0xFF10-0xFF25 and
ignores writes to them except the length timers (DMG); wave RAM stays.

__Outputs__ ('channelOutputs'): each channel's digital level 0 to 15 and
whether its DAC is on; 'mix' turns them into a stereo sample (NR51, NR50).

Not modelled: wave RAM access while CH3 plays (Blargg @09@, @10@, @12@),
the "zombie" envelope writes, the CH3 trigger corruption, the high-pass
filter of the analog stage.
-}
module Gb.SpecFast.Apu
  ( apuTick
  , apuDivWrite
  , apuRead
  , apuWrite
  , channelOutputs
  , mix
  , apuStateSize
  , apuChannel
  ) where

import Prelude

import Control.Monad (forM_, when, unless)
import Control.Monad.ST (ST)
import Data.Bits
import qualified Data.Vector.Unboxed.Mutable as MU
import Data.Word

import Gb.SpecFast.State

apuStateSize :: Int
apuStateSize = 64

-- * State (in 'mApu')

aPower, aSeq :: Int
aPower = 0 -- NR52 bit 7
aSeq = 1   -- the next frame sequencer step, 0 to 7

-- per channel k (0 to 3): field f at 8 + 8 k + f
fOn, fLen, fCnt, fStep, fVol, fEnvT :: Int
fOn = 0   -- enabled
fLen = 1  -- length timer: clocks left (64 or 256 when loaded with 0)
fCnt = 2  -- period divider (up counter; noise: dots left)
fStep = 3 -- duty step (0 to 7) / wave sample index (0 to 31)
fVol = 4  -- envelope volume
fEnvT = 5 -- envelope timer

ch :: Int -> Int -> Int
ch k f = 8 + 8 * k + f

aShadow, aSweepT, aSweepOn, aNegUsed, aWaveBuf, aLfsr :: Int
aShadow = 40  -- CH1 sweep shadow period
aSweepT = 41  -- CH1 sweep timer
aSweepOn = 42 -- CH1 sweep enabled flag
aNegUsed = 43 -- a subtraction was calculated since the trigger
aWaveBuf = 44 -- CH3's last sample read (nibble)
aLfsr = 45    -- CH4 LFSR

-- | A channel's state for a save state (side track L, L.6c; k 0 to 3):
-- on, its length timer (clocks left), its envelope volume, and whether
-- the sound unit is on.
apuChannel :: Machine s -> Int -> ST s (Bool, Int, Int, Bool)
apuChannel m k = (\o l v p -> (o /= 0, l, v, p /= 0))
  <$> get m (ch k fOn) <*> get m (ch k fLen) <*> get m (ch k fVol) <*> get m aPower

get :: Machine s -> Int -> ST s Int
get m = MU.unsafeRead (mApu m)
{-# INLINE get #-}

set :: Machine s -> Int -> Int -> ST s ()
set m = MU.unsafeWrite (mApu m)
{-# INLINE set #-}

reg :: Machine s -> Word16 -> ST s Int
reg m a = fromIntegral <$> ramRead m a
{-# INLINE reg #-}

-- | NRx0 .. NRx4 of channel k (CH2 has no NR20: 0xFF15 is unused).
nr :: Int -> Int -> Word16
nr k i = 0xFF10 + fromIntegral (5 * k + i)

-- * DACs, periods

dacOn :: Machine s -> Int -> ST s Bool
dacOn m k
  | k == 2 = (`testBit` 7) <$> reg m 0xFF1A
  | otherwise = (\v -> v .&. 0xF8 /= 0) <$> reg m (nr k 2)

period :: Machine s -> Int -> ST s Int
period m k = do
  lo <- reg m (nr k 3)
  hi <- reg m (nr k 4)
  pure ((hi .&. 7) `shiftL` 8 .|. lo)

noiseDots :: Int -> Int
noiseDots v = divisor `shiftL` (v `shiftR` 4)
 where
  d = v .&. 7
  divisor = if d == 0 then 8 else 16 * d

-- * The M-cycle tick

-- | One M-cycle; @c@ and @c'@ are the system counter before and after it.
apuTick :: Machine s -> Int -> Int -> ST s ()
apuTick m !c !c' = do
  on <- get m aPower
  -- double speed (CGB): an M-cycle is 2 T-cycles of sound time, and DIV
  -- runs twice as fast, so the frame sequencer follows bit 13 (Pan Docs,
  -- "DIV-APU": bit 5 of DIV in double speed)
  ds <- if mCgbMode m then (`testBit` 7) <$> getSt m iKEY1 else pure False
  cyc <- getSt m iCYC
  let fbit = if ds then 13 else 12
      pulseNow = not ds || odd cyc
  when (on /= 0) $ do
    when (testBit c fbit && not (testBit c' fbit)) (frameStep m)
    when pulseNow $ forM_ [0, 1] $ \k -> do
      e <- get m (ch k fOn)
      when (e /= 0) $ do
        n <- (+ 1) <$> get m (ch k fCnt)
        if n > 2047
          then do
            period m k >>= set m (ch k fCnt)
            get m (ch k fStep) >>= set m (ch k fStep) . (\s -> (s + 1) .&. 7)
          else set m (ch k fCnt) n
    e3 <- get m (ch 2 fOn)
    when (e3 /= 0) $ if ds then waveClock m else waveClock m >> waveClock m
    e4 <- get m (ch 3 fOn)
    when (e4 /= 0) $ do
      v <- reg m 0xFF22
      t <- subtract (if ds then 2 else 4) <$> get m (ch 3 fCnt)
      if t <= 0
        then do
          set m (ch 3 fCnt) (t + noiseDots v)
          when (v `shiftR` 4 < 14) (lfsrClock m v)
        else set m (ch 3 fCnt) t

waveClock :: Machine s -> ST s ()
waveClock m = do
  n <- (+ 1) <$> get m (ch 2 fCnt)
  if n > 2047
    then do
      period m 2 >>= set m (ch 2 fCnt)
      i <- (\s -> (s + 1) .&. 31) <$> get m (ch 2 fStep)
      set m (ch 2 fStep) i
      b <- reg m (0xFF30 + fromIntegral (i `shiftR` 1))
      set m aWaveBuf (if even i then b `shiftR` 4 else b .&. 0xF)
    else set m (ch 2 fCnt) n

lfsrClock :: Machine s -> Int -> ST s ()
lfsrClock m v = do
  l <- get m aLfsr
  let b = if testBit l 0 == testBit l 1 then 1 else 0
      l1 = (l .&. 0x7FFF) .|. (b `shiftL` 15)
      l2 = if testBit v 3 then (l1 .&. complement 0x80) .|. (b `shiftL` 7) else l1
  set m aLfsr (l2 `shiftR` 1)

-- | A DIV-APU tick: the next frame sequencer step.
frameStep :: Machine s -> ST s ()
frameStep m = do
  s <- get m aSeq
  set m aSeq ((s + 1) .&. 7)
  when (even s) (forM_ [0 .. 3] (lengthClock m))
  when (s == 2 || s == 6) (sweepClock m)
  when (s == 7) (forM_ [0, 1, 3] (envClock m))

lengthClock :: Machine s -> Int -> ST s ()
lengthClock m k = do
  en <- (`testBit` 6) <$> reg m (nr k 4)
  l <- get m (ch k fLen)
  when (en && l > 0) $ do
    set m (ch k fLen) (l - 1)
    when (l == 1) (set m (ch k fOn) 0)

envClock :: Machine s -> Int -> ST s ()
envClock m k = do
  v <- reg m (nr k 2)
  let pace = v .&. 7
  unless (pace == 0) $ do
    t <- subtract 1 <$> get m (ch k fEnvT)
    if t > 0 then set m (ch k fEnvT) t else do
      set m (ch k fEnvT) pace
      vol <- get m (ch k fVol)
      if testBit v 3
        then when (vol < 15) (set m (ch k fVol) (vol + 1))
        else when (vol > 0) (set m (ch k fVol) (vol - 1))

-- | The new period from the shadow (Pan Docs: frequency calculation),
-- and whether it overflows; a subtraction is remembered ('aNegUsed').
sweepCalc :: Machine s -> ST s (Int, Bool)
sweepCalc m = do
  v <- reg m 0xFF10
  sh <- get m aShadow
  let d = sh `shiftR` (v .&. 7)
      neg = testBit v 3
      n = if neg then sh - d else sh + d
  when neg (set m aNegUsed 1)
  pure (n, n > 2047)

sweepClock :: Machine s -> ST s ()
sweepClock m = do
  v <- reg m 0xFF10
  t <- subtract 1 <$> get m aSweepT
  if t > 0 then set m aSweepT t else do
    let pace = (v `shiftR` 4) .&. 7
    set m aSweepT (if pace == 0 then 8 else pace)
    on <- get m aSweepOn
    when (on /= 0 && pace /= 0) $ do
      (n, over) <- sweepCalc m
      if over then set m (ch 0 fOn) 0 else when (v .&. 7 /= 0) $ do
        set m aShadow n
        ramWrite m 0xFF13 (fromIntegral (n .&. 0xFF))
        hi <- reg m 0xFF14
        ramWrite m 0xFF14 (fromIntegral ((hi .&. 0xF8) .|. (n `shiftR` 8)))
        (_, over2) <- sweepCalc m
        when over2 (set m (ch 0 fOn) 0)

-- | A write to DIV: if it clears a set bit 12, the frame sequencer steps.
apuDivWrite :: Machine s -> Int -> ST s ()
apuDivWrite m c = do
  on <- get m aPower
  when (on /= 0 && testBit c 12) (frameStep m)

-- * Register access (0xFF10-0xFF3F)

apuRead :: Machine s -> Word16 -> ST s Word8
apuRead m a
  | a == 0xFF26 = do
      on <- get m aPower
      bits <- mapM (\k -> get m (ch k fOn)) [0 .. 3]
      let chans = foldr (\(k, b) acc -> if b /= 0 then setBit acc k else acc) 0 (zip [0 ..] bits) :: Int
      pure (fromIntegral (0x70 .|. (if on /= 0 then 0x80 else 0) .|. chans))
  | a >= 0xFF30 = waveAddr m a >>= ramRead m
  | otherwise = ramRead m a -- the caller ORs in the unused bits

-- | Where a CPU access to wave RAM goes: on the Game Boy Color, while CH3
-- plays, to the byte it last read, whatever the address (Blargg's
-- cgb_sound 09 and 12, "Current byte readable at any wave addr"; the
-- DMG's narrower window is not modelled, docs above).
waveAddr :: Machine s -> Word16 -> ST s Word16
waveAddr m a
  | mModel m /= Cgb = pure a
  | otherwise = do
      on <- get m (ch 2 fOn)
      i <- get m (ch 2 fStep)
      pure (if on /= 0 then 0xFF30 + fromIntegral (i `shiftR` 1) else a)

apuWrite :: Machine s -> Word16 -> Word8 -> ST s ()
apuWrite m a v
  | a >= 0xFF30 = waveAddr m a >>= \w -> ramWrite m w v -- wave RAM
  | a == 0xFF26 = do
      on <- get m aPower
      let on' = testBit v 7
      when (on /= 0 && not on') $ do -- power off: registers and channels cleared
        forM_ [0xFF10 .. 0xFF25] $ \r -> ramWrite m r 0
        forM_ [0 .. 3] $ \k -> set m (ch k fOn) 0
        set m aPower 0
      when (on == 0 && on') $ do -- power on: the frame sequencer restarts
        set m aPower 1
        set m aSeq 0
        -- the Game Boy Color resets the length timers (Blargg's cgb_sound
        -- 08 and 11; the DMG keeps them)
        when (mModel m == Cgb) $
          forM_ (zip [0 .. 3] [64, 64, 256, 64]) $ \(k, full) -> set m (ch k fLen) full
        forM_ [0, 1] $ \k -> set m (ch k fStep) 0
        set m aWaveBuf 0
  | otherwise = do
      on <- get m aPower
      if on == 0
        then -- powered off: only the length timers can be written (DMG;
             -- the Game Boy Color ignores them too)
          when (mModel m /= Cgb) $ case a of
            0xFF11 -> lenWrite 0 (fromIntegral v .&. 63) 64
            0xFF16 -> lenWrite 1 (fromIntegral v .&. 63) 64
            0xFF1B -> lenWrite 2 (fromIntegral v) 256
            0xFF20 -> lenWrite 3 (fromIntegral v .&. 63) 64
            _ -> pure ()
        else do
          old <- ramRead m a
          ramWrite m a v
          regWrite m a old v
 where
  lenWrite k x full = set m (ch k fLen) (full - x)

-- | Side effects of a register write (the value is already stored).
regWrite :: Machine s -> Word16 -> Word8 -> Word8 -> ST s ()
regWrite m a old v = case a of
  0xFF10 -> do -- clearing the subtraction bit after a subtraction disables CH1
    used <- get m aNegUsed
    when (testBit old 3 && not (testBit v 3) && used /= 0) (set m (ch 0 fOn) 0)
  0xFF11 -> set m (ch 0 fLen) (64 - fromIntegral v .&. 63)
  0xFF16 -> set m (ch 1 fLen) (64 - fromIntegral v .&. 63)
  0xFF1B -> set m (ch 2 fLen) (256 - fromIntegral v)
  0xFF20 -> set m (ch 3 fLen) (64 - fromIntegral v .&. 63)
  0xFF12 -> dacCheck 0
  0xFF17 -> dacCheck 1
  0xFF1A -> dacCheck 2
  0xFF21 -> dacCheck 3
  0xFF14 -> nrx4 0 64
  0xFF19 -> nrx4 1 64
  0xFF1E -> nrx4 2 256
  0xFF23 -> nrx4 3 64
  _ -> pure ()
 where
  dacCheck k = do
    d <- dacOn m k
    unless d (set m (ch k fOn) 0)
  nrx4 k full = do
    s <- get m aSeq
    let lenNext = even s -- the next frame step clocks the length timers
        enNew = testBit v 6
        enOld = testBit old 6
    -- extra length clock: enabling the length timer when the next step
    -- doesn't clock it (Pan Docs, "Obscure Behavior")
    when (not lenNext && enNew && not enOld) $ do
      l <- get m (ch k fLen)
      when (l > 0) $ do
        set m (ch k fLen) (l - 1)
        when (l == 1 && not (testBit v 7)) (set m (ch k fOn) 0)
    when (testBit v 7) (trigger m k full lenNext enNew)

trigger :: Machine s -> Int -> Int -> Bool -> Bool -> ST s ()
trigger m k full lenNext enNew = do
  d <- dacOn m k
  when d (set m (ch k fOn) 1)
  l <- get m (ch k fLen)
  when (l == 0) (set m (ch k fLen) (if enNew && not lenNext then full - 1 else full))
  case k of
    2 -> do
      -- the Game Boy Color reads the first sample 6 T-cycles (3 wave
      -- clocks) after the period alone would (Blargg's cgb_sound 09: its
      -- reads, 2 T-cycles apart, then equal the hardware's; the DMG's own
      -- trigger delay is not modelled, its tests don't need it)
      p <- period m 2
      set m (ch 2 fCnt) (if mModel m == Cgb then p - 3 else p)
      set m (ch 2 fStep) 0
    3 -> do
      v <- reg m 0xFF22
      set m (ch 3 fCnt) (noiseDots v)
      set m aLfsr 0
    _ -> period m k >>= set m (ch k fCnt)
  unless (k == 2) $ do
    v <- reg m (nr k 2)
    s <- get m aSeq
    let pace = v .&. 7
    -- one more if the next frame step clocks the envelope
    set m (ch k fEnvT) ((if pace == 0 then 8 else pace) + (if s == 7 then 1 else 0))
    set m (ch k fVol) (v `shiftR` 4)
  when (k == 0) $ do
    v <- reg m 0xFF10
    period m 0 >>= set m aShadow
    let pace = (v `shiftR` 4) .&. 7
        stp = v .&. 7
    set m aSweepT (if pace == 0 then 8 else pace)
    set m aSweepOn (if pace /= 0 || stp /= 0 then 1 else 0)
    set m aNegUsed 0
    when (stp /= 0) $ do
      (_, over) <- sweepCalc m
      when over (set m (ch 0 fOn) 0)

-- * Output

-- | Each channel's digital level (0 to 15; 0 while off) and its DAC.
channelOutputs :: Machine s -> ST s [(Int, Bool)]
channelOutputs m = mapM out [0 .. 3]
 where
  out k = do
    d <- dacOn m k
    e <- get m (ch k fOn)
    lvl <- if e == 0 then pure 0 else case k of
      2 -> do
        b <- get m aWaveBuf
        o <- (\v -> (v `shiftR` 5) .&. 3) <$> reg m 0xFF1C
        pure (if o == 0 then 0 else b `shiftR` (o - 1))
      3 -> do
        l <- get m aLfsr
        vol <- get m (ch 3 fVol)
        pure (if testBit l 0 then vol else 0)
      _ -> do
        duty <- (`shiftR` 6) <$> reg m (nr k 1)
        s <- get m (ch k fStep)
        vol <- get m (ch k fVol)
        pure (if testBit (dutyTable duty) s then vol else 0)
    pure (lvl, d)

-- | Pan Docs' waveforms (step 0 first): 12.5 %, 25 %, 50 %, 75 %.
dutyTable :: Int -> Int
dutyTable d = case d of
  0 -> 0x80 -- 00000001
  1 -> 0x81 -- 10000001
  2 -> 0xE1 -- 10000111
  _ -> 0x7E -- 01111110

-- | The stereo sample, signed: each channel whose DAC is on adds
-- @15 - 2 × level@ (digital 0 is analog +1, Pan Docs "DACs") to the sides
-- NR51 selects, each side is scaled by NR50's volume + 1. Range ±480.
mix :: Word8 -> Word8 -> [(Int, Bool)] -> (Int, Int)
mix nr50 nr51 outs = (side 4 * (vl + 1), side 0 * (vr + 1))
 where
  vl = fromIntegral (nr50 `shiftR` 4) .&. 7
  vr = fromIntegral nr50 .&. 7
  side base = sum [ 15 - 2 * l | (k, (l, d)) <- zip [0 ..] outs, d, testBit nr51 (base + k) ]
