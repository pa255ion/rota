{-|
The sound unit in Clash (PLAN.md step 2.9): the reference model's APU
("Gb.Spec.Apu", which documents the rules) as pure functions on the
registers (0xFF10-0xFF3F, 'Regs') and an 'ApuState', for "Gb.Core". The
lockstep tests compare the register reads and the four channel outputs
with the spec after every step.
-}
module Gb.Apu
  ( Regs
  , regsInit
  , ApuState (..)
  , ChanState (..)
  , apuInit
  , apuTick
  , apuDivWrite
  , apuRead
  , apuWrite
  , apuOutputs
  , apuMix
  ) where

import Clash.Prelude

-- | 0xFF10-0xFF3F as written (wave RAM included).
type Regs = Vec 48 (BitVector 8)

-- | At the hand-off (Pan Docs, "Power Up Sequence"; as Gb.Spec.Machine).
regsInit :: Regs
regsInit = $(listToVecTH
  ([ 0x80, 0xBF, 0xF3, 0xFF, 0xBF, 0x00, 0x3F, 0x00, 0xFF, 0xBF, 0x7F, 0xFF, 0x9F, 0xFF, 0xBF, 0x00
   , 0xFF, 0x00, 0x00, 0xBF, 0x77, 0xF3, 0xF1, 0, 0, 0, 0, 0, 0, 0, 0, 0
   , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ] :: [BitVector 8]))

data ChanState = ChanState
  { csOn   :: Bool
  , csLen  :: Unsigned 9   -- ^ clocks left (64 / 256 when loaded with 0)
  , csCnt  :: Unsigned 20  -- ^ period divider (noise: dots left)
  , csStep :: Unsigned 5   -- ^ duty step / wave sample index
  , csVol  :: Unsigned 4
  , csEnvT :: Unsigned 4
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFDataX

data ApuState = ApuState
  { asPower   :: Bool
  , asSeq     :: Unsigned 3
  , asCh      :: Vec 4 ChanState
  , asShadow  :: Unsigned 11
  , asSweepT  :: Unsigned 4
  , asSweepOn :: Bool
  , asNegUsed :: Bool
  , asWaveBuf :: Unsigned 4
  , asLfsr    :: BitVector 16
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFDataX

chan0 :: ChanState
chan0 = ChanState False 0 0 0 0 0

-- | On, with CH1 still on (NR52 = 0xF1).
apuInit :: ApuState
apuInit = ApuState True 0 (chan0 { csOn = True } :> chan0 :> chan0 :> chan0 :> Nil) 0 0 False False 0 0

-- * Registers

r :: Regs -> Index 48 -> BitVector 8
r = (!!)

-- | Register index of NRx<i> for channel k.
nri :: Index 4 -> Index 5 -> Index 48
nri k i = resize k * 5 + resize i

dacOn :: Regs -> Index 4 -> Bool
dacOn rs k
  | k == 2 = testBit (r rs 0x0A) 7
  | otherwise = r rs (nri k 2) .&. 0xF8 /= 0

period :: Regs -> Index 4 -> Unsigned 11
period rs k = unpack (slice d2 d0 (r rs (nri k 4)) ++# r rs (nri k 3))

noiseDots :: BitVector 8 -> Unsigned 20
noiseDots v = shiftL divisor (fromEnum (unpack (slice d7 d4 v) :: Unsigned 4))
 where
  d = unpack (slice d2 d0 v) :: Unsigned 3
  divisor = if d == 0 then 8 else 16 * zeroExtend (zeroExtend d :: Unsigned 8)

modCh :: Index 4 -> (ChanState -> ChanState) -> ApuState -> ApuState
modCh k f s = s { asCh = replace k (f (asCh s !! k)) (asCh s) }

-- * Tick

-- | One M-cycle; @c@, @c'@: the system counter before and after it.
-- Returns the registers too (the CH1 sweep writes the period back). In
-- double speed (@ds@, the Game Boy Color) an M-cycle is half the sound
-- time: the frame sequencer follows bit 13, the pulse dividers count on
-- odd M-cycles only (@oddMc@: the spec's M-cycle count), the wave divider
-- once, the noise 2 dots (Gb.Spec.Apu.apuTick).
apuTick :: Bool -> Bool -> BitVector 16 -> BitVector 16 -> (Regs, ApuState) -> (Regs, ApuState)
apuTick ds oddMc c c' (rs0, s0)
  | not (asPower s0) = (rs0, s0)
  | otherwise = (rs1, s4)
 where
  fbit = if ds then 13 else 12
  (rs1, s1) = if testBit c fbit && not (testBit c' fbit) then frameStep (rs0, s0) else (rs0, s0)
  pulse k st = let cs = asCh st !! k in
    if not (csOn cs) || (ds && not oddMc) then st
    else if csCnt cs + 1 > 2047
      then modCh k (\x -> x { csCnt = zeroExtend (period rs1 k), csStep = (csStep x + 1) .&. 7 }) st
      else modCh k (\x -> x { csCnt = csCnt x + 1 }) st
  s2 = pulse 1 (pulse 0 s1)
  s3 | not (csOn (asCh s2 !! (2 :: Index 4))) = s2
     | ds = waveClock rs1 s2
     | otherwise = waveClock rs1 (waveClock rs1 s2)
  dots = if ds then 2 else 4
  s4 = let cs = asCh s3 !! (3 :: Index 4)
           v = r rs1 0x12
       in if not (csOn cs) then s3
          else if csCnt cs <= dots
            then let t = csCnt cs + noiseDots v - dots
                     st = modCh 3 (\x -> x { csCnt = t }) s3
                 in if unpack (slice d7 d4 v) < (14 :: Unsigned 4) then st { asLfsr = lfsrClock v (asLfsr st) } else st
            else modCh 3 (\x -> x { csCnt = csCnt x - dots }) s3

waveClock :: Regs -> ApuState -> ApuState
waveClock rs s
  -- (a count above 2^19 is negative: the CGB's trigger delay from a
  -- period below 3, which then counts up through 0 as the spec's does)
  | csCnt cs + 1 > 2047 && csCnt cs + 1 < 0x80000 =
      let i = (csStep cs + 1) .&. 31
          b = r rs (0x20 + resize (bitCoerce (truncateB (shiftR i 1) :: Unsigned 4) :: Index 16))
          nib = if testBit i 0 then slice d3 d0 b else slice d7 d4 b
      in (modCh 2 (\x -> x { csCnt = zeroExtend (period rs 2), csStep = i }) s) { asWaveBuf = unpack nib }
  | otherwise = modCh 2 (\x -> x { csCnt = csCnt x + 1 }) s
 where
  cs = asCh s !! (2 :: Index 4)

lfsrClock :: BitVector 8 -> BitVector 16 -> BitVector 16
lfsrClock v l = shiftR l2 1
 where
  b = if testBit l 0 == testBit l 1 then 1 else 0 :: BitVector 16
  l1 = (l .&. 0x7FFF) .|. shiftL b 15
  l2 = if testBit v 3 then (l1 .&. complement 0x80) .|. shiftL b 7 else l1

frameStep :: (Regs, ApuState) -> (Regs, ApuState)
frameStep (rs, s0) = (rs', s4)
 where
  st = asSeq s0
  s1 = s0 { asSeq = st + 1 }
  s2 = if not (testBit st 0) then foldl (\acc k -> lengthClock rs k acc) s1 (0 :> 1 :> 2 :> 3 :> Nil) else s1
  (rs', s3) = if st == 2 || st == 6 then sweepClock (rs, s2) else (rs, s2)
  s4 = if st == 7 then foldl (\acc k -> envClock rs' k acc) s3 (0 :> 1 :> 3 :> Nil) else s3

lengthClock :: Regs -> Index 4 -> ApuState -> ApuState
lengthClock rs k s
  | testBit (r rs (nri k 4)) 6 && csLen cs > 0 =
      modCh k (\x -> x { csLen = csLen x - 1, csOn = csOn x && csLen x /= 1 }) s
  | otherwise = s
 where
  cs = asCh s !! k

envClock :: Regs -> Index 4 -> ApuState -> ApuState
envClock rs k s
  | pace == 0 = s
  | csEnvT cs > 1 = modCh k (\x -> x { csEnvT = csEnvT x - 1 }) s
  | otherwise = modCh k (\x -> x { csEnvT = pace, csVol = vol' x }) s
 where
  cs = asCh s !! k
  v = r rs (nri k 2)
  pace = unpack (zeroExtend (slice d2 d0 v))
  vol' x | testBit v 3 = if csVol x < 15 then csVol x + 1 else csVol x
         | otherwise = if csVol x > 0 then csVol x - 1 else csVol x

-- | Frequency calculation: the new period and whether it overflows.
sweepCalc :: Regs -> ApuState -> (Unsigned 12, Bool, ApuState)
sweepCalc rs s = (n, n > 2047, if neg then s { asNegUsed = True } else s)
 where
  v = r rs 0x00
  sh = zeroExtend (asShadow s) :: Unsigned 12
  d = shiftR sh (fromEnum (unpack (slice d2 d0 v) :: Unsigned 3))
  neg = testBit v 3
  n = if neg then sh - d else sh + d

sweepClock :: (Regs, ApuState) -> (Regs, ApuState)
sweepClock (rs, s0)
  | asSweepT s0 > 1 = (rs, s0 { asSweepT = asSweepT s0 - 1 })
  | not (asSweepOn s0) || pace == 0 = (rs, s1)
  | over = (rs, modCh 0 (\x -> x { csOn = False }) s2)
  | slice d2 d0 v == 0 = (rs, s2)
  | otherwise =
      let rs' = replace (0x03 :: Index 48) (pack (resize n :: Unsigned 8))
                  (replace (0x04 :: Index 48) ((r rs 0x04 .&. 0xF8) .|. zeroExtend (pack (resize (shiftR n 8) :: Unsigned 3))) rs)
          s3 = s2 { asShadow = resize n }
          (_, over2, s4) = sweepCalc rs' s3
      in (rs', if over2 then modCh 0 (\x -> x { csOn = False }) s4 else s4)
 where
  v = r rs 0x00
  pace = unpack (zeroExtend (slice d6 d4 v)) :: Unsigned 4
  s1 = s0 { asSweepT = if pace == 0 then 8 else pace }
  (n, over, s2) = sweepCalc rs s1

-- | A CPU write to DIV (with the counter before it).
apuDivWrite :: BitVector 16 -> (Regs, ApuState) -> (Regs, ApuState)
apuDivWrite c st@(_, s)
  | asPower s && testBit c 12 = frameStep st
  | otherwise = st

-- * Register access

-- | Where a CPU access to wave RAM goes (offset): on the Game Boy Color
-- (@cgb@), while CH3 plays, to the byte it last read (Gb.Spec.Apu.waveAddr).
waveIx :: Bool -> ApuState -> Index 48 -> Index 48
waveIx cgb s i
  | cgb && csOn cs = 0x20 + resize (bitCoerce (truncateB (shiftR (csStep cs) 1) :: Unsigned 4) :: Index 16)
  | otherwise = i
 where
  cs = asCh s !! (2 :: Index 4)

-- | 0xFF10-0xFF3F (offset), before the caller ORs in the unused bits.
apuRead :: Bool -> Regs -> ApuState -> Index 48 -> BitVector 8
apuRead cgb rs s i
  | i == 0x16 = 0x70 .|. (if asPower s then 0x80 else 0) .|. zeroExtend (pack (map csOn (reverse (asCh s))))
  | i >= 0x20 = r rs (waveIx cgb s i)
  | otherwise = r rs i

-- | A register write; @cgb@: the Game Boy Color's rules (power-on resets
-- the length timers, which can't be written while off; wave RAM as
-- 'waveIx'; CH3's trigger delay).
apuWrite :: Bool -> Index 48 -> BitVector 8 -> (Regs, ApuState) -> (Regs, ApuState)
apuWrite cgb i v (rs, s)
  | i >= 0x20 = (replace (waveIx cgb s i) v rs, s) -- wave RAM
  | i == 0x16 =
      if asPower s && not (testBit v 7)
        then ( imap (\j x -> if j < 0x16 then 0 else x) rs
             , s { asPower = False, asCh = map (\c -> c { csOn = False }) (asCh s) } )
        else if not (asPower s) && testBit v 7
          then (rs, modCh 0 step0 (modCh 1 step0 (lenReset s { asPower = True, asSeq = 0, asWaveBuf = 0 })))
          else (rs, s)
  | not (asPower s) && cgb = (rs, s)
  | not (asPower s) = case i of
      0x01 -> (rs, lenSet 0 (zeroExtend (unpack (slice d5 d0 v))) 64)
      0x06 -> (rs, lenSet 1 (zeroExtend (unpack (slice d5 d0 v))) 64)
      0x0B -> (rs, lenSet 2 (zeroExtend (unpack v)) 256)
      0x10 -> (rs, lenSet 3 (zeroExtend (unpack (slice d5 d0 v))) 64)
      _ -> (rs, s)
  | otherwise = regWrite cgb i (r rs i) v (replace i v rs, s)
 where
  lenSet k x full = modCh k (\c -> c { csLen = full - x }) s
  step0 c = c { csStep = 0 }
  lenReset t
    | cgb = t { asCh = zipWith (\c full -> c { csLen = full }) (asCh t) (64 :> 64 :> 256 :> 64 :> Nil) }
    | otherwise = t

regWrite :: Bool -> Index 48 -> BitVector 8 -> BitVector 8 -> (Regs, ApuState) -> (Regs, ApuState)
regWrite cgb i old v (rs, s) = case i of
  0x00 -> (rs, if testBit old 3 && not (testBit v 3) && asNegUsed s then modCh 0 off s else s)
  0x01 -> (rs, lenLoad 0 64 (zeroExtend (unpack (slice d5 d0 v))))
  0x06 -> (rs, lenLoad 1 64 (zeroExtend (unpack (slice d5 d0 v))))
  0x0B -> (rs, lenLoad 2 256 (zeroExtend (unpack v)))
  0x10 -> (rs, lenLoad 3 64 (zeroExtend (unpack (slice d5 d0 v))))
  0x02 -> (rs, dacCheck 0)
  0x07 -> (rs, dacCheck 1)
  0x0A -> (rs, dacCheck 2)
  0x11 -> (rs, dacCheck 3)
  0x04 -> nrx4 0 64
  0x09 -> nrx4 1 64
  0x0E -> nrx4 2 256
  0x13 -> nrx4 3 64
  _ -> (rs, s)
 where
  off c = c { csOn = False }
  lenLoad k full x = modCh k (\c -> c { csLen = full - x }) s
  dacCheck k = if dacOn rs k then s else modCh k off s
  nrx4 k full = (rs, s2)
   where
    lenNext = not (testBit (asSeq s) 0)
    enNew = testBit v 6
    enOld = testBit old 6
    cs = asCh s !! k
    s1 | not lenNext && enNew && not enOld && csLen cs > 0 =
           modCh k (\c -> c { csLen = csLen c - 1
                            , csOn = csOn c && not (csLen c == 1 && not (testBit v 7)) }) s
       | otherwise = s
    s2 = if testBit v 7 then trigger cgb rs k full lenNext enNew s1 else s1

trigger :: Bool -> Regs -> Index 4 -> Unsigned 9 -> Bool -> Bool -> ApuState -> ApuState
trigger cgb rs k full lenNext enNew s0 = s3
 where
  d = dacOn rs k
  cs0 = asCh s0 !! k
  len' = if csLen cs0 == 0 then (if enNew && not lenNext then full - 1 else full) else csLen cs0
  v2 = r rs (nri k 2)
  pace2 = unpack (zeroExtend (slice d2 d0 v2)) :: Unsigned 4
  envT = (if pace2 == 0 then 8 else pace2) + (if asSeq s0 == 7 then 1 else 0)
  cs1 = cs0 { csOn = csOn cs0 || d, csLen = len' }
  cs2 = case k of
    -- the Game Boy Color: the first sample 3 wave clocks later (as the
    -- spec; a period below 3 wraps below 0, which 'waveClock' counts up
    -- through)
    2 -> cs1 { csCnt = zeroExtend (period rs 2) - (if cgb then 3 else 0), csStep = 0 }
    3 -> cs1 { csCnt = noiseDots (r rs 0x12), csEnvT = envT, csVol = unpack (slice d7 d4 v2) }
    _ -> cs1 { csCnt = zeroExtend (period rs k), csEnvT = envT, csVol = unpack (slice d7 d4 v2) }
  s1 = s0 { asCh = replace k cs2 (asCh s0), asLfsr = if k == 3 then 0 else asLfsr s0 }
  v0 = r rs 0x00
  swPace = unpack (zeroExtend (slice d6 d4 v0)) :: Unsigned 4
  swStep = slice d2 d0 v0
  s2 = if k == 0
         then s1 { asShadow = period rs 0, asSweepT = if swPace == 0 then 8 else swPace
                 , asSweepOn = swPace /= 0 || swStep /= 0, asNegUsed = False }
         else s1
  s3 = if k == 0 && swStep /= 0
         then let (_, over, s') = sweepCalc rs s2 in if over then modCh 0 (\c -> c { csOn = False }) s' else s'
         else s2

-- * Output

-- | Each channel's digital level and DAC state.
apuOutputs :: Regs -> ApuState -> Vec 4 (Unsigned 4, Bool)
apuOutputs rs s = imap out (asCh s)
 where
  out k cs = (if csOn cs then lvl k cs else 0, dacOn rs k)
  lvl k cs = case k of
    2 -> let o = unpack (slice d6 d5 (r rs 0x0C)) :: Unsigned 2
         in if o == 0 then 0 else shiftR (asWaveBuf s) (fromEnum (o - 1))
    3 -> if testBit (asLfsr s) 0 then csVol cs else 0
    _ -> let duty = slice d7 d6 (r rs (nri k 1))
             tbl = case duty of
               0 -> 0x80
               1 -> 0x81
               2 -> 0xE1
               _ -> 0x7E :: BitVector 8
         in if testBit tbl (fromEnum (resize (csStep cs) :: Unsigned 3)) then csVol cs else 0

-- | The stereo sample (as Gb.Spec.Apu.mix): ±480.
apuMix :: Regs -> Vec 4 (Unsigned 4, Bool) -> (Signed 12, Signed 12)
apuMix rs outs = (side 4 * (vl + 1), side 0 * (vr + 1))
 where
  nr50 = r rs 0x14
  nr51 = r rs 0x15
  vl = unpack (zeroExtend (slice d6 d4 nr50)) :: Signed 12
  vr = unpack (zeroExtend (slice d2 d0 nr50)) :: Signed 12
  side base = sum (imap (\k (l, d) -> if d && testBit nr51 (base + fromEnum k)
                                        then 15 - 2 * (bitCoerce (zeroExtend l :: Unsigned 12) :: Signed 12)
                                        else 0) outs)
