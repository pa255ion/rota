{-# LANGUAGE StrictData #-}
{-|
The APU in Clash (PLAN.md steps 3.6 and 3.9): the frame counter with its
IRQ, the two pulse channels, the triangle, the noise channel and the DMC,
with $4015. The same rules as "Nes.Spec.Apu" (see there, and the nesdev
wiki pages it cites); the core ("Nes.Core") runs 'apuCycle' at the start
of every CPU cycle, performs the DMC's DMA when 'apuDmcWanted' says so and
hands the byte to 'apuDmcFill'. 'apuChannels' is what the mixer
("Nes.Mixer") takes.

The spec keeps the DMC DMA's first possible cycle as an absolute cycle
number; here it is a countdown ('dmWait'), 0 = wanted now.
-}
module Nes.Apu
  ( Apu (..)
  , Env (..)
  , Pulse (..)
  , Tri (..)
  , Noise (..)
  , Dmc (..)
  , apuInit
  , apuCycle
  , apuWrite
  , apuStatus
  , apuIrq
    -- * DMC DMA
  , apuDmcWanted
  , apuDmcAborted
  , apuDmcAbortDone
  , apuDmcAddr
  , apuDmcFill
    -- * Output
  , Channels (..)
  , apuChannels
  , apuAudioChannels
  ) where

import Clash.Prelude

data Env = Env
  { eStart :: Bool
  , eDiv   :: Unsigned 4
  , eDecay :: Unsigned 4
  , eConst :: Bool
  , eVol   :: Unsigned 4
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFDataX

data Pulse = Pulse
  { puDuty    :: Unsigned 2
  , puEnv     :: Env
  , puPeriod  :: Unsigned 11
  , puCount   :: Unsigned 11
  , puStep    :: Unsigned 3
  , puSwEn    :: Bool
  , puSwPer   :: Unsigned 3
  , puSwNeg   :: Bool
  , puSwShift :: Unsigned 3
  , puSwRel   :: Bool
  , puSwDiv   :: Unsigned 3
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFDataX

data Tri = Tri
  { trPeriod  :: Unsigned 11
  , trCount   :: Unsigned 11
  , trStep    :: Unsigned 5
  , trLinear  :: Unsigned 7
  , trLinLoad :: Unsigned 7
  , trReload  :: Bool
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFDataX

data Noise = Noise
  { nsEnv   :: Env
  , nsMode  :: Bool
  , nsIdx   :: Unsigned 4
  , nsCount :: Unsigned 11
  , nsLfsr  :: BitVector 15
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFDataX

data Dmc = Dmc
  { dmIrqEn   :: Bool
  , dmLoop    :: Bool
  , dmRate    :: Unsigned 4
  , dmCount   :: Unsigned 8
  , dmLevel   :: Unsigned 7
  , dmSampAddr :: Unsigned 16
  , dmSampLen :: Unsigned 12
  , dmAddr    :: Unsigned 16
  , dmRemain  :: Unsigned 12
  , dmBuffer  :: Unsigned 8
  , dmFull    :: Bool
  , dmShift   :: Unsigned 8
  , dmBits    :: Unsigned 4
  , dmSilence :: Bool
  , dmIrq     :: Bool
  , dmWait    :: Maybe (Unsigned 3) -- ^ CPU cycles until a DMA may halt the CPU
  , dmStopAge :: Unsigned 2         -- ^ 1: playback stopped this cycle, 2, 3: 1, 2 cycles ago; 0: not lately
  , dmEnd     :: Bool               -- ^ the output unit finished a byte in this cycle
  , dmAbort   :: Bool               -- ^ the wanted DMA is aborted after its halt cycle
  , dmEnWait  :: Unsigned 3         -- ^ CPU cycles until a $4015 enable has taken effect (DMAs wait for it)
  , dmReload  :: Bool               -- ^ the timer reloaded in this cycle (a $4010 write now still sets its value)
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFDataX

data Apu = Apu
  { aLen     :: Vec 4 (Unsigned 8)
  , aHalt    :: Vec 4 Bool         -- ^ length halt (envelope loop, triangle control)
  , aEnabled :: BitVector 4
  , aMode    :: Bool               -- ^ 5-step
  , aInhibit :: Bool
  , aIrq     :: Bool
  , aIrqClr  :: Bool               -- ^ a $4015 read cleared the flag: it drops at the next get cycle
  , aSeq     :: Unsigned 16        -- ^ CPU cycles since the sequencer reset
  , aPend    :: Unsigned 3         -- ^ cycles until a $4017 write takes effect
  , aPendVal :: Unsigned 8
  , aPulse   :: Vec 2 Pulse
  , aTri     :: Tri
  , aNoise   :: Noise
  , aDmc     :: Dmc
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFDataX

envInit :: Env
envInit = Env False 0 0 False 0

apuInit :: Apu
apuInit = Apu (repeat 0) (repeat False) 0 False False False False 0 0 0
  (repeat (Pulse 0 envInit 0 0 0 False 0 False 0 False 0))
  (Tri 0 0 0 0 0 False)
  (Noise envInit False 0 0 1)
  (Dmc False False 0 (rateReload 0) 0 0xC000 1 0 0 0 False 0 8 True False Nothing 0 False False 0 False)

lengthTable :: Vec 32 (Unsigned 8)
lengthTable =
  10 :> 254 :> 20 :> 2 :> 40 :> 4 :> 80 :> 6 :> 160 :> 8 :> 60 :> 10 :> 14 :> 12 :> 26 :> 14 :>
  12 :> 16 :> 24 :> 18 :> 48 :> 20 :> 96 :> 22 :> 192 :> 24 :> 72 :> 26 :> 16 :> 28 :> 32 :> 30 :> Nil

-- | Duty lookup (bit s = step s), read at sequencer steps 0, 7, 6, ..., 1.
dutyTable :: Vec 4 (BitVector 8)
dutyTable = 0b10000000 :> 0b11000000 :> 0b11110000 :> 0b00111111 :> Nil

-- | NTSC noise periods (CPU cycles) / 2 - 1: the timer's reload value.
noiseReload :: Unsigned 4 -> Unsigned 11
noiseReload i = map (\p -> resize (p `div` 2 - 1)) ((4 :: Unsigned 12) :> 8 :> 16 :> 32 :> 64 :> 96 :> 128 :> 160 :> 202 :> 254 :> 380 :> 508 :> 762 :> 1016 :> 2034 :> 4068 :> Nil) !! i

-- | NTSC DMC rates (CPU cycles) / 2 - 1.
rateReload :: Unsigned 4 -> Unsigned 8
rateReload i = map (\p -> resize (p `div` 2 - 1)) ((428 :: Unsigned 9) :> 380 :> 340 :> 320 :> 286 :> 254 :> 226 :> 214 :> 190 :> 160 :> 142 :> 128 :> 106 :> 84 :> 72 :> 54 :> Nil) !! i

apuIrq :: Apu -> Bool
apuIrq a = (aIrq a && not (aInhibit a)) || dmIrq (aDmc a)

-- | One CPU cycle, before that cycle's bus access. @get@: the cycle count
-- since power-on, this one included, is odd (a get cycle: the APU-rate
-- timers count).
apuCycle :: Bool -> Apu -> Apu
apuCycle get a0 = timers (frame a0 { aIrq = aIrq a0 && not (aIrqClr a0 && get), aIrqClr = aIrqClr a0 && not get
                                     , aDmc = (aDmc a0) { dmWait = countDown <$> dmWait (aDmc a0)
                                                        , dmStopAge = if dmStopAge (aDmc a0) /= 0 then dmStopAge (aDmc a0) + 1 else 0
                                                        , dmEnd = False
                                                        , dmReload = False
                                                        , dmEnWait = countDown (dmEnWait (aDmc a0)) } })
 where
  countDown w = if w > 0 then w - 1 else 0
  frame a
    | aPend a == 1 =
        let five = testBit (aPendVal a) 7
            a' = a { aPend = 0, aSeq = 0, aMode = five }
        in if five then half (quarter a') else a'
    | otherwise = sequencer (if aPend a > 1 then a { aPend = aPend a - 1 } else a)
  sequencer s =
    let n = aSeq s + 1
        q = n == 7457 || n == 22371
    in if not (aMode s)
         then let h = n == 14913 || n == 29829
                  s1 = if h then half (quarter s) else if q then quarter s else s
                  -- the flag even with the inhibit bit on the first two of the three (as the spec)
                  s2 | n == 29828 || n == 29829 = s1 { aIrq = True }
                     | n >= 29830 = s1 { aIrq = not (aInhibit s1) }
                     | otherwise = s1
              in s2 { aSeq = if n >= 29830 then 0 else n }
         else let h = n == 14913 || n == 37281
                  s1 = if h then half (quarter s) else if q then quarter s else s
              in s1 { aSeq = if n >= 37282 then 0 else n }
  timers a =
    let a1 = a { aTri = triClock (aLen a !! (2 :: Index 4)) (aTri a) }
    in if get then a1 { aPulse = map pulseClock (aPulse a1), aNoise = noiseClock (aNoise a1), aDmc = dmcClock (aDmc a1) }
              else a1

quarter :: Apu -> Apu
quarter a = a
  { aPulse = zipWith (\l p -> p { puEnv = envClock l (puEnv p) }) (take d2 (aHalt a)) (aPulse a)
  , aNoise = (aNoise a) { nsEnv = envClock (aHalt a !! (3 :: Index 4)) (nsEnv (aNoise a)) }
  , aTri = linClock (aHalt a !! (2 :: Index 4)) (aTri a) }

half :: Apu -> Apu
half a = a
  { aLen = zipWith (\l h -> if l > 0 && not h then l - 1 else l) (aLen a) (aHalt a)
  , aPulse = imap (\i p -> sweepClock (i == 0) p) (aPulse a) }

envClock :: Bool -> Env -> Env
envClock lp e
  | eStart e = e { eStart = False, eDecay = 15, eDiv = eVol e }
  | eDiv e == 0 = e { eDiv = eVol e, eDecay = if eDecay e > 0 then eDecay e - 1 else if lp then 15 else 0 }
  | otherwise = e { eDiv = eDiv e - 1 }

envVolume :: Env -> Unsigned 4
envVolume e = if eConst e then eVol e else eDecay e

linClock :: Bool -> Tri -> Tri
linClock ctl t = t { trLinear = l, trReload = trReload t && ctl }
 where l | trReload t = trLinLoad t
         | trLinear t > 0 = trLinear t - 1
         | otherwise = 0

-- | The sweep's target period (pulse 1 negates in ones' complement).
sweepTarget :: Bool -> Pulse -> Unsigned 12
sweepTarget one p
  | not (puSwNeg p) = resize per + c
  | otherwise = let sb = c + (if one then 1 else 0)
                in if sb > resize per then 0 else resize per - sb
 where
  per = puPeriod p
  c = resize (per `shiftR` fromEnum (puSwShift p)) :: Unsigned 12

muted :: Bool -> Pulse -> Bool
muted one p = puPeriod p < 8 || sweepTarget one p > 0x7FF

sweepClock :: Bool -> Pulse -> Pulse
sweepClock one p = p2
 where
  apply = puSwDiv p == 0 && puSwEn p && puSwShift p /= 0 && not (muted one p)
  p1 = if apply then p { puPeriod = resize (sweepTarget one p) } else p
  p2 | puSwDiv p == 0 || puSwRel p = p1 { puSwDiv = puSwPer p, puSwRel = False }
     | otherwise = p1 { puSwDiv = puSwDiv p - 1 }

pulseClock :: Pulse -> Pulse
pulseClock p
  | puCount p == 0 = p { puCount = puPeriod p, puStep = puStep p - 1 }
  | otherwise = p { puCount = puCount p - 1 }

triClock :: Unsigned 8 -> Tri -> Tri
triClock len t
  | trCount t == 0 = t { trCount = trPeriod t, trStep = if len > 0 && trLinear t > 0 then trStep t + 1 else trStep t }
  | otherwise = t { trCount = trCount t - 1 }

noiseClock :: Noise -> Noise
noiseClock n
  | nsCount n == 0 =
      let r = nsLfsr n
          fb = testBit r 0 /= testBit r (if nsMode n then 6 else 1)
      in n { nsCount = noiseReload (nsIdx n), nsLfsr = (r `shiftR` 1) .|. (if fb then bit 14 else 0) }
  | otherwise = n { nsCount = nsCount n - 1 }

dmcClock :: Dmc -> Dmc
dmcClock d
  | dmCount d /= 0 = d { dmCount = dmCount d - 1 }
  | otherwise = out
 where
  lv = dmLevel d
  lv' | dmSilence d = lv
      | testBit (dmShift d) 0 = if lv <= 125 then lv + 2 else lv
      | otherwise = if lv >= 2 then lv - 2 else lv
  bits = dmBits d - 1
  dA = d { dmCount = rateReload (dmRate d), dmReload = True, dmLevel = lv', dmShift = dmShift d `shiftR` 1 }
  -- playback stopped in the APU cycle before: an aborted DMA (as the spec)
  stoppedJust = dmStopAge d == 2 || dmStopAge d == 3
  out | bits /= 0 = dA { dmBits = bits }
      | not (dmFull d) = dA { dmBits = 8, dmSilence = True, dmEnd = True }
      | dmRemain d > 0 && dmWait d == Nothing = full { dmWait = Just (max 1 (dmEnWait d)) }
      | dmRemain d == 0 && dmWait d == Nothing && stoppedJust = full { dmWait = Just 1, dmAbort = True }
      | otherwise = full
  full = dA { dmBits = 8, dmSilence = False, dmShift = dmBuffer d, dmFull = False, dmEnd = True }

-- | A CPU write to $4000-$4017 (not $4014, $4016). @oddCycle@: the number
-- of CPU cycles so far, this one included, is odd.
apuWrite :: Bool -> Unsigned 16 -> Unsigned 8 -> Apu -> Apu
apuWrite oddCycle addr v a = case addr of
  0x4000 -> pulseEnv 0
  0x4004 -> pulseEnv 1
  0x4001 -> pulse 0 (\p -> p { puSwEn = testBit v 7, puSwPer = resize (v `shiftR` 4), puSwNeg = testBit v 3
                               , puSwShift = resize v, puSwRel = True })
  0x4005 -> pulse 1 (\p -> p { puSwEn = testBit v 7, puSwPer = resize (v `shiftR` 4), puSwNeg = testBit v 3
                               , puSwShift = resize v, puSwRel = True })
  0x4002 -> pulse 0 (\p -> p { puPeriod = lo (puPeriod p) })
  0x4006 -> pulse 1 (\p -> p { puPeriod = lo (puPeriod p) })
  0x4003 -> load 0 (pulse 0 pulseHi)
  0x4007 -> load 1 (pulse 1 pulseHi)
  0x4008 -> (haltBit 2 7) { aTri = (aTri a) { trLinLoad = resize v } }
  0x400A -> a { aTri = (aTri a) { trPeriod = lo (trPeriod (aTri a)) } }
  0x400B -> load 2 a { aTri = (aTri a) { trPeriod = hi (trPeriod (aTri a)), trReload = True } }
  0x400C -> let a' = haltBit 3 5 in a' { aNoise = (aNoise a') { nsEnv = envReg (nsEnv (aNoise a')) } }
  0x400E -> a { aNoise = (aNoise a) { nsMode = testBit v 7, nsIdx = resize v } }
  0x400F -> load 3 a { aNoise = (aNoise a) { nsEnv = (nsEnv (aNoise a)) { eStart = True } } }
  -- (a write in the cycle the timer reloads still sets the reload value,
  -- as the spec: AccuracyCoin "DMC Reload Timing")
  0x4010 -> dmc (\d -> d { dmIrqEn = testBit v 7, dmIrq = dmIrq d && testBit v 7, dmLoop = testBit v 6, dmRate = resize v
                         , dmCount = if dmReload d then rateReload (resize v) else dmCount d })
  0x4011 -> dmc (\d -> d { dmLevel = resize v })
  0x4012 -> dmc (\d -> d { dmSampAddr = 0xC000 + resize v * 64 })
  0x4013 -> dmc (\d -> d { dmSampLen = resize v * 16 + 1 })
  0x4015 ->
    let a' = a { aEnabled = resize (pack v)
               , aLen = imap (\i l -> if testBit v (fromEnum i) then l else 0) (aLen a) }
        d = (aDmc a) { dmIrq = False }
        d' | not (testBit v 4) = d { dmRemain = 0, dmStopAge = if dmRemain d > 0 then 1 else dmStopAge d }
           | dmRemain d /= 0 = d
           | otherwise =
               let r = d { dmAddr = dmSampAddr d, dmRemain = dmSampLen d }
                   en = if oddCycle then 4 else 3
               in if not (dmFull r) && dmWait r == Nothing then r { dmWait = Just en, dmEnWait = en } else r { dmEnWait = en }
    in a' { aDmc = d' }
  0x4017 -> a { aPendVal = v, aPend = if oddCycle then 4 else 3
              , aIrq = aIrq a && not (testBit v 6), aInhibit = testBit v 6 }
  _ -> a
 where
  lo :: Unsigned 11 -> Unsigned 11
  lo t = (t .&. 0x700) .|. resize v
  hi :: Unsigned 11 -> Unsigned 11
  hi t = (t .&. 0xFF) .|. (resize (v .&. 7) `shiftL` 8)
  haltBit :: Index 4 -> Int -> Apu
  haltBit ch b = a { aHalt = replace ch (testBit v b) (aHalt a) }
  envReg e = e { eConst = testBit v 4, eVol = resize v }
  pulseEnv :: Index 2 -> Apu
  pulseEnv i = let a' = haltBit (resize i) 5
               in a' { aPulse = replace i (let p = aPulse a' !! i in p { puDuty = resize (v `shiftR` 6), puEnv = envReg (puEnv p) }) (aPulse a') }
  pulse :: Index 2 -> (Pulse -> Pulse) -> Apu
  pulse i f = a { aPulse = replace i (f (aPulse a !! i)) (aPulse a) }
  pulseHi p = p { puPeriod = hi (puPeriod p), puStep = 0, puEnv = (puEnv p) { eStart = True } }
  load :: Index 4 -> Apu -> Apu
  load ch x = if testBit (aEnabled x) (fromEnum ch)
                then x { aLen = replace ch (lengthTable !! (v `shiftR` 3)) (aLen x) }
                else x
  dmc f = a { aDmc = f (aDmc a) }

-- | $4015: length counters non-zero (bits 0 to 3), DMC bytes remaining
-- (bit 4), frame IRQ (bit 6), DMC IRQ (bit 7); bit 5 is the caller's.
-- Reading clears the frame IRQ flag, from the next cycle on (as the spec).
apuStatus :: Apu -> (Unsigned 8, Apu)
apuStatus a = (v, a { aIrqClr = aIrq a })
 where
  lens = foldr (.|.) 0 (imap (\i l -> if l > 0 then bit (fromEnum i) else 0) (aLen a))
  d = aDmc a
  v = lens .|. (if dmRemain d > 0 then 0x10 else 0) .|. (if aIrq a then 0x40 else 0) .|. (if dmIrq d then 0x80 else 0)

-- | A DMC DMA may halt the CPU in this cycle (after 'apuCycle').
apuDmcWanted :: Apu -> Bool
apuDmcWanted a = dmWait (aDmc a) == Just 0

apuDmcAddr :: Apu -> Unsigned 16
apuDmcAddr = dmAddr . aDmc

-- | The DMC DMA's byte (see "Nes.Spec.Apu".apuDmcFill: the implicit stop
-- and the RP2A03G's unexpected reload DMA too).
apuDmcFill :: Unsigned 8 -> Apu -> Apu
apuDmcFill v a = a { aDmc = d'' }
 where
  d = aDmc a
  dA = d { dmWait = Nothing, dmBuffer = v, dmFull = True
         , dmAddr = if dmAddr d == 0xFFFF then 0x8000 else dmAddr d + 1
         , dmRemain = if dmRemain d > 0 then dmRemain d - 1 else 0 }
  stopped = dmRemain d == 1 && not (dmLoop d)
  d' | dmRemain d /= 1 = dA
     | dmLoop d = dA { dmAddr = dmSampAddr d, dmRemain = dmSampLen d }
     | otherwise = dA { dmIrq = dmIrq d || dmIrqEn d, dmStopAge = 1 }
  d'' | dmEnd d = d' { dmWait = Just 1, dmAddr = if stopped then dmAddr d else dmAddr d' }
      | otherwise = d'

-- | The wanted DMC DMA is an aborted one: its halt cycle, then nothing.
apuDmcAborted :: Apu -> Bool
apuDmcAborted = dmAbort . aDmc

-- | An aborted DMC DMA has had its halt cycle, or won't happen.
apuDmcAbortDone :: Apu -> Apu
apuDmcAbortDone a = a { aDmc = (aDmc a) { dmWait = Nothing, dmAbort = False } }

-- | The channels' outputs: pulses, triangle, noise 0 to 15, DMC 0 to 127.
data Channels = Channels
  { chPulse1, chPulse2, chTriangle, chNoise :: Unsigned 4
  , chDmc :: Unsigned 7
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFDataX

apuChannels :: Apu -> Channels
apuChannels a = Channels (pulseOut 0) (pulseOut 1) tri noise (dmLevel (aDmc a))
 where
  pulseOut :: Index 2 -> Unsigned 4
  pulseOut i =
    let p = aPulse a !! i
        on = testBit (dutyTable !! puDuty p) (fromEnum (puStep p))
    in if aLen a !! (resize i :: Index 4) == 0 || muted (i == 0) p || not on then 0 else envVolume (puEnv p)
  t = trStep (aTri a)
  -- 15, 14, ..., 0, then 0, 1, ..., 15
  tri = if testBit t 4 then resize (t .&. 15) else 15 - resize (t .&. 15)
  n = aNoise a
  noise = if aLen a !! (3 :: Index 4) == 0 || testBit (nsLfsr n) 0 then 0 else envVolume (nsEnv n)

-- | The channels for the speakers ("Nes.Audio"): as 'apuChannels', but a
-- triangle with timer period 0 or 1 (28 to 56 kHz; games use it to
-- silence the channel) gives its mean. On the console the analogue
-- filters remove it; sampled at 48 kHz it would fold back to an audible
-- whine (7.9 kHz at period 0; heard on the board, step 3.9). The CPU
-- can't see the difference: the triangle itself runs on unchanged.
apuAudioChannels :: Apu -> Channels
apuAudioChannels a
  | trPeriod (aTri a) < 2 = c { chTriangle = 7 }
  | otherwise = c
 where
  c = apuChannels a
