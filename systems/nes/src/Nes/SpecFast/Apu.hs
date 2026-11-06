{-# OPTIONS_GHC -fworker-wrapper -fspecialise-aggressively -funfolding-use-threshold=200 -funfolding-creation-threshold=1000 #-}
-- (the fast version is never synthesised: GHC's unboxing back on, which
-- clash-common's -fno-worker-wrapper turns off for Clash's sake)
-- Side track Q (PLAN.md Q.7): the desktop emulator's fast version of
-- Nes.Spec.Apu, kept beside it (the original stays the reference the
-- core's lockstep checks; a fix in either goes to both). nes-specfast
-- runs both machines side by side and compares them.
{-|
The 2A03's APU in the reference model: the frame counter, the two pulse
channels, the triangle, the noise channel and the DMC, with their length
counters, envelopes, sweeps and the linear counter (steps 3.2 and 3.9).
The DMC's sample fetches are DMA ("Nes.Spec.Machine" performs them:
'apuDmcDma' says when one is wanted, 'apuDmcFill' delivers the byte).

Sources: nesdev wiki, "APU", "APU Frame Counter", "APU Length Counter",
"APU Envelope", "APU Sweep", "APU Pulse", "APU Triangle", "APU Noise",
"APU DMC", "APU Mixer", "DMA" (fetched 2026-10-03).

__Clocks.__ CPU cycles alternate get and put; a get and the following put
make an APU cycle. Gets are the odd CPU cycles, counting the first after
power-on as 1 (as OAM DMA, "Nes.Spec.Machine"). The pulse, noise and DMC
timers count on gets (once per APU cycle), the triangle's on every CPU
cycle. A timer counts its period down to 0 and, on the next clock,
reloads and clocks its unit.

__Frame counter__ (NTSC, in CPU cycles since the sequencer was reset):
mode 0 (4-step) clocks the quarter frame at 7457, 14913, 22371, 29829,
the half frame at 14913 and 29829, sets the IRQ flag at 29828, 29829 and
29830 (unless inhibited) and starts again at 29830; mode 1 (5-step)
clocks quarters at 7457, 14913, 22371, 37281, halves at 14913 and 37281,
never sets the flag, and starts again at 37282. A $4017 write takes
effect 3 CPU cycles later if written on an even cycle, 4 if odd (the APU
runs at half the CPU clock); in mode 1 that also clocks a quarter and a
half frame at once. Setting the inhibit bit clears the flag. Reading
$4015 clears it too. Quarter frames clock the envelopes and the
triangle's linear counter, half frames the length counters and sweeps.

__DMC DMA.__ When the output unit takes the sample buffer and bytes
remain, a "reload" DMA is wanted from the next cycle (a put); after a
$4015 write that starts a sample with the buffer empty, a "load" DMA is
wanted from the get of the second APU cycle after the write (3 or 4
cycles later). The machine halts the CPU on its next read from then on.
The aborted and extra DMAs of particular chip revisions are not modelled.
-}
module Nes.SpecFast.Apu
  ( Apu
  , apuFields
  , newApu
  , apuCycle
  , apuWrite
  , apuReadStatus
  , apuIrq
  , apuReset
    -- * DMC DMA
  , apuDmcDma
  , apuDmcDue
  , apuPacked
  , apuDmcAborted
  , apuDmcAbortDone
  , apuDmcAddr
  , apuDmcFill
    -- * Output
  , Channels (..)
  , apuChannels
    -- * Save states (side track N)
  , ApuSnap (..)
  , apuSnapshot
  ) where

import Prelude

import Control.Monad (forM_, when, unless)
import Control.Monad.ST (ST)
import Data.Bits
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MV
import Platform.Snapshot (Field (..))
import Data.Word

newtype Apu s = Apu (MV.MVector s Int)

-- State slots: length counters 0 to 3, their halt flags (envelope loop,
-- the triangle's control flag) 4 to 7, then:
sEnabled, sMode, sInhibit, sIrq, sSeq, sPending, sPendingVal, sCycle :: Int
sEnabled = 8    -- $4015 bits 0 to 3
sMode = 9
sInhibit = 10
sIrq = 11       -- frame IRQ flag
sSeq = 12       -- CPU cycles since the sequencer reset
sPending = 13   -- CPU cycles until a $4017 write takes effect (0 = none)
sPendingVal = 14
sCycle = 15     -- CPU cycles since power-on, this one included

-- Pulse 1 at 16, pulse 2 at 32 (offsets):
pDuty, pConst, pVol, pPeriod, pCount, pStep, pEnvStart, pEnvDiv, pEnvDecay, pSwEn, pSwPeriod, pSwNeg, pSwShift, pSwReload, pSwDiv :: Int
pDuty = 0; pConst = 1; pVol = 2; pPeriod = 3; pCount = 4; pStep = 5
pEnvStart = 6; pEnvDiv = 7; pEnvDecay = 8
pSwEn = 9; pSwPeriod = 10; pSwNeg = 11; pSwShift = 12; pSwReload = 13; pSwDiv = 14

pulseBase :: Int -> Int
pulseBase p = 16 + 16 * p

-- Triangle at 48:
tPeriod, tCount, tStep, tLinear, tLinLoad, tLinReload :: Int
tPeriod = 48; tCount = 49; tStep = 50; tLinear = 51; tLinLoad = 52; tLinReload = 53

-- Noise at 56 (the envelope slots at the pulse's offsets):
nBase, nMode, nPeriodIdx, nCount, nLfsr :: Int
nBase = 56
nMode = 56 + 9; nPeriodIdx = 56 + 10; nCount = 56 + 11; nLfsr = 56 + 12

-- DMC at 72:
dIrqEn, dLoop, dRate, dCount, dLevel, dSampleAddr, dSampleLen, dAddr, dRemain, dBuffer, dFull, dShift, dBits, dSilence, dIrq, dDue :: Int
dIrqEn = 72; dLoop = 73; dRate = 74; dCount = 75; dLevel = 76
dSampleAddr = 77; dSampleLen = 78; dAddr = 79; dRemain = 80
dBuffer = 81; dFull = 82; dShift = 83; dBits = 84; dSilence = 85
dIrq = 86
dDue = 87       -- the first cycle a DMC DMA may halt the CPU (0 = none wanted)

sIrqClear :: Int
sIrqClear = 88  -- a $4015 read cleared the frame IRQ flag: it drops at the next get cycle

-- Explicit DMA abort (side track D step 3).
dStopAt, dAbort, dEndAt, dEnableAt :: Int
dStopAt = 89    -- the cycle a $4015 write stopped playback (0: none)
dAbort = 90     -- the wanted DMA is aborted after its halt cycle
dEndAt = 91     -- the cycle the output unit last finished a byte
dEnableAt = 92  -- the first cycle a DMA may halt after $4015 enabled the DMC

-- DMC reload timing (AccuracyCoin d103cbd, "DMC Reload Timing").
dReloadAt :: Int
dReloadAt = 93  -- the CPU cycle the DMC's timer last reloaded

slots :: Int
slots = 94

newApu :: ST s (Apu s)
newApu = do
  v <- MV.replicate slots 0
  MV.write v nLfsr 1
  MV.write v dBits 8
  MV.write v dSilence 1
  MV.write v dCount (U.unsafeIndex dmcRates 0 `div` 2 - 1)
  MV.write v dSampleAddr 0xC000
  MV.write v dSampleLen 1
  pure (Apu v)

get :: Apu s -> Int -> ST s Int
get (Apu v) = MV.unsafeRead v
{-# INLINE get #-}

set :: Apu s -> Int -> Int -> ST s ()
set (Apu v) = MV.unsafeWrite v
{-# INLINE set #-}

-- | Length counter load values, indexed by bits 7 to 3 of the fourth
-- register of a channel.
lengthTable :: U.Vector Int
lengthTable = U.fromList
  [ 10, 254, 20, 2, 40, 4, 80, 6, 160, 8, 60, 10, 14, 12, 26, 14
  , 12, 16, 24, 18, 48, 20, 96, 22, 192, 24, 72, 26, 16, 28, 32, 30 ]

-- | Pulse duty lookup, read at sequencer steps 0, 7, 6, ..., 1 (the
-- sequencer counts down from 0 after a $4003 write).
dutyTable :: U.Vector Int
dutyTable = U.fromList
  [ 0, 0, 0, 0, 0, 0, 0, 1
  , 0, 0, 0, 0, 0, 0, 1, 1
  , 0, 0, 0, 0, 1, 1, 1, 1
  , 1, 1, 1, 1, 1, 1, 0, 0 ]

triangleTable :: U.Vector Int
triangleTable = U.fromList ([15, 14 .. 0] <> [0 .. 15])

-- | NTSC noise periods, CPU cycles.
noisePeriods :: U.Vector Int
noisePeriods = U.fromList [4, 8, 16, 32, 64, 96, 128, 160, 202, 254, 380, 508, 762, 1016, 2034, 4068]

-- | NTSC DMC rates, CPU cycles per output bit.
dmcRates :: U.Vector Int
dmcRates = U.fromList [428, 380, 340, 320, 286, 254, 226, 214, 190, 160, 142, 128, 106, 84, 72, 54]

-- | One CPU cycle, before that cycle's bus access.
apuCycle :: Apu s -> ST s ()
apuCycle a = do
  cyc <- (+ 1) <$> get a sCycle
  set a sCycle cyc
  clr <- get a sIrqClear
  when (clr /= 0 && odd cyc) (set a sIrq 0 >> set a sIrqClear 0)
  pend <- get a sPending
  if pend == 1
    then do
      -- The reset cycle itself is sequencer cycle 0.
      v <- get a sPendingVal
      set a sPending 0
      set a sSeq 0
      set a sMode (if testBit v 7 then 1 else 0)
      when (testBit v 7) (quarter >> half)
    else do
      when (pend > 1) (set a sPending (pend - 1))
      sequencer
  triangleClock a
  when (odd cyc) $ do
    forM_ [0, 1] (pulseClock a)
    noiseClock a
    dmcClock a cyc
 where
  sequencer = do
    n <- (+ 1) <$> get a sSeq
    mode <- get a sMode
    if mode == 0
      then do
        when (n == 7457 || n == 22371) quarter
        when (n == 14913 || n == 29829) (quarter >> half)
        when (n >= 29828) (irq n)
        set a sSeq (if n >= 29830 then 0 else n)
      else do
        when (n == 7457 || n == 22371) quarter
        when (n == 14913 || n == 37281) (quarter >> half)
        set a sSeq (if n >= 37282 then 0 else n)
  quarter = do
    envelope a (pulseBase 0) 4
    envelope a (pulseBase 1) 5
    envelope a nBase 7
    linear a
  half = do
    forM_ [0 .. 3] $ \ch -> do
      l <- get a ch
      h <- get a (4 + ch)
      when (l > 0 && h == 0) (set a ch (l - 1))
    forM_ [0, 1] (sweep a)
  -- The flag is set on the sequence's last three cycles; even with the
  -- inhibit bit, on the first two of them (the third sets it as the bit
  -- says). The IRQ line follows it only without the bit ('apuIrq').
  -- AccuracyCoin "Frame Counter IRQ" tests I to M.
  irq n = do
    inh <- get a sInhibit
    if n < 29830 then set a sIrq 1 else set a sIrq (if inh == 0 then 1 else 0)

envelope :: Apu s -> Int -> Int -> ST s ()
envelope a b loopSlot = do
  start <- get a (b + pEnvStart)
  v <- get a (b + pVol)
  if start /= 0
    then set a (b + pEnvStart) 0 >> set a (b + pEnvDecay) 15 >> set a (b + pEnvDiv) v
    else do
      d <- get a (b + pEnvDiv)
      if d == 0
        then do
          set a (b + pEnvDiv) v
          decay <- get a (b + pEnvDecay)
          lp <- get a loopSlot
          if decay > 0 then set a (b + pEnvDecay) (decay - 1)
            else when (lp /= 0) (set a (b + pEnvDecay) 15)
        else set a (b + pEnvDiv) (d - 1)

envVolume :: Apu s -> Int -> ST s Int
envVolume a b = do
  c <- get a (b + pConst)
  if c /= 0 then get a (b + pVol) else get a (b + pEnvDecay)
{-# INLINE envVolume #-}

linear :: Apu s -> ST s ()
linear a = do
  r <- get a tLinReload
  if r /= 0 then get a tLinLoad >>= set a tLinear
    else do
      l <- get a tLinear
      when (l > 0) (set a tLinear (l - 1))
  ctl <- get a 6
  when (ctl == 0) (set a tLinReload 0)

-- | The sweep's target period (continuously computed).
sweepTarget :: Apu s -> Int -> ST s Int
sweepTarget a p = do
  let b = pulseBase p
  per <- get a (b + pPeriod)
  sh <- get a (b + pSwShift)
  neg <- get a (b + pSwNeg)
  let c = per `shiftR` sh
      t | neg == 0 = per + c
        | p == 0 = per - c - 1
        | otherwise = per - c
  pure (max 0 t)
{-# INLINE sweepTarget #-}

pulseMuted :: Apu s -> Int -> ST s Bool
pulseMuted a p = do
  per <- get a (pulseBase p + pPeriod)
  t <- sweepTarget a p
  pure (per < 8 || t > 0x7FF)
{-# INLINE pulseMuted #-}

sweep :: Apu s -> Int -> ST s ()
sweep a p = do
  let b = pulseBase p
  d <- get a (b + pSwDiv)
  en <- get a (b + pSwEn)
  sh <- get a (b + pSwShift)
  muted <- pulseMuted a p
  t <- sweepTarget a p
  when (d == 0 && en /= 0 && sh /= 0 && not muted) (set a (b + pPeriod) t)
  rl <- get a (b + pSwReload)
  if d == 0 || rl /= 0
    then get a (b + pSwPeriod) >>= set a (b + pSwDiv) >> set a (b + pSwReload) 0
    else set a (b + pSwDiv) (d - 1)

pulseClock :: Apu s -> Int -> ST s ()
pulseClock a p = do
  let b = pulseBase p
  c <- get a (b + pCount)
  if c == 0
    then do
      get a (b + pPeriod) >>= set a (b + pCount)
      s <- get a (b + pStep)
      set a (b + pStep) ((s - 1) .&. 7)
    else set a (b + pCount) (c - 1)

triangleClock :: Apu s -> ST s ()
triangleClock a = do
  c <- get a tCount
  if c == 0
    then do
      get a tPeriod >>= set a tCount
      l <- get a 2
      lin <- get a tLinear
      when (l > 0 && lin > 0) $ get a tStep >>= set a tStep . (\s -> (s + 1) .&. 31)
    else set a tCount (c - 1)

noiseClock :: Apu s -> ST s ()
noiseClock a = do
  c <- get a nCount
  if c == 0
    then do
      i <- get a nPeriodIdx
      set a nCount (U.unsafeIndex noisePeriods i `div` 2 - 1)
      r <- get a nLfsr
      m <- get a nMode
      let fb = (r .&. 1) `xor` ((r `shiftR` (if m /= 0 then 6 else 1)) .&. 1)
      set a nLfsr ((r `shiftR` 1) .|. (fb `shiftL` 14))
    else set a nCount (c - 1)

dmcClock :: Apu s -> Int -> ST s ()
dmcClock a cyc = do
  c <- get a dCount
  if c /= 0 then set a dCount (c - 1) else do
    r <- get a dRate
    set a dCount (U.unsafeIndex dmcRates r `div` 2 - 1)
    set a dReloadAt cyc
    sil <- get a dSilence
    sh <- get a dShift
    when (sil == 0) $ do
      lv <- get a dLevel
      if testBit sh 0 then when (lv <= 125) (set a dLevel (lv + 2))
                      else when (lv >= 2) (set a dLevel (lv - 2))
    set a dShift (sh `shiftR` 1)
    bits <- subtract 1 <$> get a dBits
    if bits /= 0 then set a dBits bits else do
      set a dBits 8
      set a dEndAt cyc
      full <- get a dFull
      if full == 0 then set a dSilence 1 else do
        set a dSilence 0
        get a dBuffer >>= set a dShift
        set a dFull 0
        -- the buffer is empty: a reload DMA from the next cycle (a put)
        rem' <- get a dRemain
        due <- get a dDue
        -- (not before $4015's enable has taken effect: AccuracyCoin "Delta
        -- Modulation Channel" L to N, the DMA delayed by 1 to 3 cycles)
        en <- get a dEnableAt
        when (rem' > 0 && due == 0) (set a dDue (max (cyc + 1) en))
        -- Playback stopped (by $4015, or by the last byte of a sample: 'apuDmcFill')
        -- in the APU cycle before (the 2nd or 3rd
        -- CPU cycle before the halt attempt): the DMA still starts and is
        -- aborted after its halt cycle (nesdev wiki "DMA", "Explicit
        -- abort"; AccuracyCoin "Explicit DMA Abort"). If the halt would
        -- fall on a write cycle, it doesn't happen at all (Nes.Spec.Machine).
        stop <- get a dStopAt
        when (rem' == 0 && due == 0 && stop > 0 && (cyc - stop == 1 || cyc - stop == 2)) $ do
          set a dDue (cyc + 1)
          set a dAbort 1

-- | CPU write to $4000-$4013, $4015 or $4017.
apuWrite :: Apu s -> Word16 -> Word8 -> ST s ()
apuWrite a addr w = case addr of
  0x4000 -> envReg (pulseBase 0) 0 >> set a (pulseBase 0 + pDuty) (d `shiftR` 6)
  0x4004 -> envReg (pulseBase 1) 1 >> set a (pulseBase 1 + pDuty) (d `shiftR` 6)
  0x4001 -> sweepReg 0
  0x4005 -> sweepReg 1
  0x4002 -> timerLo (pulseBase 0 + pPeriod)
  0x4006 -> timerLo (pulseBase 1 + pPeriod)
  0x4003 -> pulseHi 0
  0x4007 -> pulseHi 1
  0x4008 -> haltBit 2 7 >> set a tLinLoad (d .&. 0x7F)
  0x400A -> timerLo tPeriod
  0x400B -> timerHi tPeriod >> load 2 >> set a tLinReload 1
  0x400C -> envReg nBase 3
  0x400E -> set a nMode (d `shiftR` 7) >> set a nPeriodIdx (d .&. 15)
  0x400F -> load 3 >> set a (nBase + pEnvStart) 1
  0x4010 -> do
    set a dIrqEn (d `shiftR` 7)
    unless (testBit d 7) (set a dIrq 0)
    set a dLoop ((d `shiftR` 6) .&. 1)
    set a dRate (d .&. 15)
    -- A write in the CPU cycle the timer reloads (this model reloads at
    -- the cycle's start, 'apuCycle', before the write) still sets the
    -- reload value: the timer takes the new rate (AccuracyCoin "DMC
    -- Reload Timing": on hardware, the same APU cycle uses the new value).
    cyc <- get a sCycle
    at <- get a dReloadAt
    when (at == cyc) (set a dCount (U.unsafeIndex dmcRates (fromIntegral (d .&. 15)) `div` 2 - 1))
  0x4011 -> set a dLevel (d .&. 0x7F)
  0x4012 -> set a dSampleAddr (0xC000 + d * 64)
  0x4013 -> set a dSampleLen (d * 16 + 1)
  0x4015 -> do
    set a sEnabled (d .&. 0x0F)
    forM_ [0 .. 3] $ \ch -> when (not (testBit d ch)) (set a ch 0)
    set a dIrq 0
    rem' <- get a dRemain
    if not (testBit d 4)
      then do
        set a dRemain 0
        when (rem' > 0) (get a sCycle >>= set a dStopAt)
      else when (rem' == 0) $ do
        restart a
        full <- get a dFull
        due <- get a dDue
        cyc <- get a sCycle
        -- a load DMA: the get of the second APU cycle after this one
        let en = cyc + if odd cyc then 4 else 3
        set a dEnableAt en
        when (full == 0 && due == 0) (set a dDue en)
  0x4017 -> do
    cyc <- get a sCycle
    set a sPendingVal d
    -- sCycle already counts the current cycle; odd: between APU cycles.
    set a sPending (if odd cyc then 4 else 3)
    when (testBit d 6) (set a sIrq 0)
    set a sInhibit (if testBit d 6 then 1 else 0)
  _ -> pure ()
 where
  d = fromIntegral w :: Int
  haltBit ch b = set a (4 + ch) (if testBit d b then 1 else 0)
  envReg b ch = do
    haltBit ch 5
    set a (b + pConst) ((d `shiftR` 4) .&. 1)
    set a (b + pVol) (d .&. 15)
  sweepReg p = do
    let b = pulseBase p
    set a (b + pSwEn) (d `shiftR` 7)
    set a (b + pSwPeriod) ((d `shiftR` 4) .&. 7)
    set a (b + pSwNeg) ((d `shiftR` 3) .&. 1)
    set a (b + pSwShift) (d .&. 7)
    set a (b + pSwReload) 1
  timerLo s = get a s >>= \t -> set a s ((t .&. 0x700) .|. d)
  timerHi s = get a s >>= \t -> set a s ((t .&. 0xFF) .|. ((d .&. 7) `shiftL` 8))
  pulseHi p = do
    let b = pulseBase p
    timerHi (b + pPeriod)
    load p
    set a (b + pStep) 0
    set a (b + pEnvStart) 1
  load ch = do
    en <- get a sEnabled
    when (testBit en ch) (set a ch (U.unsafeIndex lengthTable (d `shiftR` 3)))

restart :: Apu s -> ST s ()
restart a = do
  get a dSampleAddr >>= set a dAddr
  get a dSampleLen >>= set a dRemain

-- | $4015: length counters 0 to 3 non-zero (bits 0 to 3), DMC bytes
-- remaining (bit 4), frame IRQ (bit 6), DMC IRQ (bit 7); bit 5 is open
-- bus (the caller ORs it in). Clears the frame IRQ flag.
apuReadStatus :: Apu s -> ST s Word8
apuReadStatus a = do
  ls <- mapM (get a) [0 .. 3]
  f <- get a sIrq
  rem' <- get a dRemain
  di <- get a dIrq
  -- The flag drops at the start of the next get cycle: the CPU's poll at
  -- the end of this one still sees the IRQ (AccuracyCoin, "Interrupt flag
  -- latency" 14), and after a read on a get cycle so does a read on the
  -- following put. AccuracyCoin's sync routines tell get from put that
  -- way (SLO $4015,X: two reads on consecutive cycles); with the flag
  -- dropping on the next cycle either way, they always took "put" and
  -- ran one CPU cycle early half the time ("$2007 Stress Test").
  when (f /= 0) (set a sIrqClear 1)
  pure (fromIntegral (foldr (.|.) 0 [ bit i | (i, l) <- zip [0 ..] ls, l > 0 ]
                      .|. (if rem' > 0 then 0x10 else 0) .|. (f `shiftL` 6) .|. (di `shiftL` 7)))

apuIrq :: Apu s -> ST s Bool
apuIrq a = (\f inh d -> (f /= 0 && inh == 0) || d /= 0) <$> get a sIrq <*> get a sInhibit <*> get a dIrq

-- | The first cycle a DMC DMA may halt the CPU, if one is wanted.
apuDmcDma :: Apu s -> ST s (Maybe Int)
apuDmcDma a = (\d -> if d == 0 then Nothing else Just d) <$> get a dDue

-- | 'apuDmcDma' without the Maybe (0: none), for the machine's every
-- cycle (side track Q).
apuDmcDue :: Apu s -> ST s Int
apuDmcDue a = get a dDue
{-# INLINE apuDmcDue #-}

-- | The wanted DMC DMA is an aborted one: the halt cycle, then nothing.
apuDmcAborted :: Apu s -> ST s Bool
apuDmcAborted a = (/= 0) <$> get a dAbort

-- | An aborted DMC DMA has had its halt cycle.
apuDmcAbortDone :: Apu s -> ST s ()
apuDmcAbortDone a = set a dDue 0 >> set a dAbort 0

apuDmcAddr :: Apu s -> ST s Word16
apuDmcAddr a = fromIntegral <$> get a dAddr

-- | The DMC DMA's byte: into the sample buffer; the address and bytes
-- remaining advance (the IRQ flag or a loop at the end).
apuDmcFill :: Apu s -> Word8 -> ST s ()
apuDmcFill a v = do
  set a dDue 0
  set a dBuffer (fromIntegral v)
  set a dFull 1
  ad <- get a dAddr
  set a dAddr (if ad == 0xFFFF then 0x8000 else ad + 1)
  old <- get a dRemain
  -- (stopped since the DMA was wanted: the byte still lands, nothing more)
  when (old > 0) $ set a dRemain (old - 1)
  when (old == 1) $ do
    lp <- get a dLoop
    ie <- get a dIrqEn
    if lp /= 0 then restart a else do
      when (ie /= 0) (set a dIrq 1)
      -- Playback stops implicitly (the sample's last byte). In the APU
      -- cycle before a reload DMA would schedule, that aborts it (see the
      -- timer).
      get a sCycle >>= set a dStopAt
  -- A byte that arrives in the cycle in which the output unit finished one
  -- and found the buffer empty: the RP2A03G from 1990 on still does the
  -- reload DMA (looping or not; not stopped: from the same address)
  -- (nesdev wiki "DMA", "DMC DMA bugs": the "unexpected reload DMA";
  -- AccuracyCoin "Implicit DMA Abort", behaviour 1, all three loops).
  cyc <- get a sCycle
  end <- get a dEndAt
  when (end == cyc) $ do
    lp <- get a dLoop
    when (old == 1 && lp == 0) (set a dAddr ad)
    set a dDue (cyc + 1)

-- | The channels' outputs: pulses, triangle and noise 0 to 15, DMC 0 to
-- 127 (what the mixer, "Nes.Mixer", takes).
data Channels = Channels
  { chPulse1, chPulse2, chTriangle, chNoise, chDmc :: !Word8
  } deriving stock (Show, Eq)

apuChannels :: Apu s -> ST s Channels
apuChannels a = do
  p1 <- pulseOut 0
  p2 <- pulseOut 1
  tr <- (triangleTable U.!) <$> get a tStep
  n <- do
    l <- get a 3
    r <- get a nLfsr
    if l == 0 || testBit r 0 then pure 0 else envVolume a nBase
  dl <- get a dLevel
  pure (Channels (fromIntegral p1) (fromIntegral p2) (fromIntegral tr) (fromIntegral n) (fromIntegral dl))
 where
  pulseOut p = do
    let b = pulseBase p
    l <- get a p
    duty <- get a (b + pDuty)
    s <- get a (b + pStep)
    muted <- pulseMuted a p
    if l == 0 || muted || U.unsafeIndex dutyTable (duty * 8 + s) == 0 then pure 0 else envVolume a b

-- | 'apuChannels' packed into one Int, without the record (side track Q,
-- the desktop's sound every cycle): the two pulses' sum in bits 0-4, the
-- triangle 5-8, the noise 9-12, the DMC 13-19.
apuPacked :: Apu s -> ST s Int
apuPacked a = do
  p1 <- pulseOut 0
  p2 <- pulseOut 1
  st <- get a tStep
  l <- get a 3
  r <- get a nLfsr
  n <- if l == 0 || testBit r 0 then pure 0 else envVolume a nBase
  dl <- get a dLevel
  pure ((p1 + p2) .|. (U.unsafeIndex triangleTable st `shiftL` 5) .|. (n `shiftL` 9) .|. (dl `shiftL` 13))
 where
  pulseOut p = do
    let b = pulseBase p
    l <- get a p
    if l == 0 then pure 0 else do
      duty <- get a (b + pDuty)
      s <- get a (b + pStep)
      if U.unsafeIndex dutyTable (duty * 8 + s) == 0 then pure 0 else do
        muted <- pulseMuted a p
        if muted then pure 0 else envVolume a b
  {-# INLINE pulseOut #-}

-- | Reset: $4015 = 0; the frame counter is reset as by rewriting the last
-- mode (the inhibit bit too).
apuReset :: Apu s -> ST s ()
apuReset a = do
  apuWrite a 0x4015 0
  mode <- get a sMode
  inh <- get a sInhibit
  apuWrite a 0x4017 (fromIntegral (mode `shiftL` 7 .|. inh `shiftL` 6))

-- * Save states (side track N, "Nes.Spec.SaveState")

-- | What a save state keeps of the APU: the registers $4000-$4013 as the
-- values that set them now (the fourth registers' period bits only; the
-- unused $4009, $400D and $400F 0), $4015's enables, the frame counter
-- (mode, inhibit, cycles since its reset, its IRQ flag), the DMC's IRQ
-- flag, the length counters, the DMC's timer, output and buffer (what
-- its DMAs' timing follows); and whether nothing is on its way (no $4017
-- write pending, no flag clear pending, the DMC idle: no bytes left, no
-- DMA wanted, its last byte finished, playback stopped or enabled more
-- than a few cycles ago).
data ApuSnap = ApuSnap
  { apRegs    :: [Int] -- ^ 20, $4000-$4013
  , apEnabled :: Int
  , apMode    :: Int
  , apInhibit :: Int
  , apSeq     :: Int
  , apIrq     :: Int
  , apDmcIrq  :: Int
  , apLengths :: [Int]
  , apDmcCount :: Int  -- ^ its timer
  , apDmcBits  :: Int  -- ^ the output unit's bits left
  , apDmcFull  :: Int  -- ^ the sample buffer holds a byte
  , apDmcBuf   :: Int
  , apDmcShift :: Int
  , apDmcSilence :: Int
  , apQuiet   :: Bool
  } deriving stock (Show, Eq)

apuSnapshot :: Apu s -> ST s ApuSnap
apuSnapshot a = do
  let g = get a
      pulse p = do
        let b = pulseBase p
        duty <- g (b + pDuty)
        halt <- g (4 + p)
        c <- g (b + pConst)
        vol <- g (b + pVol)
        en <- g (b + pSwEn)
        per <- g (b + pSwPeriod)
        neg <- g (b + pSwNeg)
        sh <- g (b + pSwShift)
        t <- g (b + pPeriod)
        pure [ duty `shiftL` 6 .|. halt `shiftL` 5 .|. c `shiftL` 4 .|. vol
             , en `shiftL` 7 .|. per `shiftL` 4 .|. neg `shiftL` 3 .|. sh
             , t .&. 0xFF, (t `shiftR` 8) .&. 7 ]
  p1 <- pulse 0
  p2 <- pulse 1
  th <- g 6
  ll <- g tLinLoad
  tp <- g tPeriod
  nh <- g 7
  nc <- g (nBase + pConst)
  nv <- g (nBase + pVol)
  nm <- g nMode
  ni <- g nPeriodIdx
  die <- g dIrqEn
  dl <- g dLoop
  dr <- g dRate
  dv <- g dLevel
  dsa <- g dSampleAddr
  dsl <- g dSampleLen
  let regs = p1 <> p2
        <> [th `shiftL` 7 .|. ll, 0, tp .&. 0xFF, (tp `shiftR` 8) .&. 7]
        <> [nh `shiftL` 5 .|. nc `shiftL` 4 .|. nv, 0, nm `shiftL` 7 .|. ni, 0]
        <> [die `shiftL` 7 .|. dl `shiftL` 6 .|. dr, dv, (dsa - 0xC000) `shiftR` 6, (dsl - 1) `shiftR` 4]
  pend <- g sPending
  clr <- g sIrqClear
  rem' <- g dRemain
  due <- g dDue
  ab <- g dAbort
  cyc <- g sCycle
  stop <- g dStopAt
  ena <- g dEnableAt
  ApuSnap regs <$> g sEnabled <*> g sMode <*> g sInhibit <*> g sSeq <*> g sIrq <*> g dIrq
    <*> mapM g [0 .. 3]
    <*> g dCount <*> g dBits <*> g dFull <*> g dBuffer <*> g dShift <*> g dSilence
    <*> pure (pend == 0 && clr == 0 && rem' == 0 && due == 0 && ab == 0
              && cyc - stop >= 3 && ena <= cyc)

-- | The APU's state for a save state (Q.8, "Platform.Snapshot").
apuFields :: Apu s -> [Field s]
apuFields (Apu v) = [FInt v]
