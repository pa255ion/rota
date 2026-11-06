{-# LANGUAGE StrictData #-}
{-|
The Game Gear's PSG in Clash (PLAN.md step 5.6): "Gg.Spec.Psg" as a
state record with a pure function per write ('psgWriteC') and per PSG
clock ('psgTickC'), and the output ('psgOutC'). The lockstep tests
(@gg-lockstep@) compare it with the spec write by write and clock by
clock. The rules are the spec's (SMS Power, "SN76489").
-}
module Gg.Psg
  ( PsgC (..)
  , psgPowerOn
  , psgWriteC
  , psgTickC
  , psgOutC
  , volumes
  ) where

import Clash.Prelude

data PsgC = PsgC
  { pPeriod  :: Vec 3 (Unsigned 10) -- ^ tone periods
  , pNoise   :: Unsigned 3          -- ^ noise control: bit 2 white, bits 1-0 rate
  , pVol     :: Vec 4 (Unsigned 4)  -- ^ attenuation, tones then noise
  , pLatch   :: Unsigned 3          -- ^ the latched register: channel × 2 + volume
  , pCount   :: Vec 4 (Unsigned 10)
  , pOut     :: Vec 4 Bool          -- ^ the tones' outputs, the noise counter's flip-flop
  , pLfsr    :: Unsigned 16
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFDataX

psgPowerOn :: PsgC
psgPowerOn = PsgC (repeat 0) 0 (repeat 15) 0 (repeat 0) (repeat False) 0x8000

-- | A byte written to the PSG port.
psgWriteC :: Unsigned 8 -> PsgC -> PsgC
psgWriteC b p
  | testBit b 7 = setLow reg (resize b) p { pLatch = reg }
  | even (pLatch p) && ch < 3 =
      let old = pPeriod p !! ch
      in p { pPeriod = replace ch ((old .&. 0x0F) .|. (resize (b .&. 0x3F) `shiftL` 4)) (pPeriod p) }
  | otherwise = setLow (pLatch p) (resize b) p
 where
  reg = resize (b `shiftR` 4) :: Unsigned 3
  ch = pLatch p `shiftR` 1
  setLow r v q =
    let c = r `shiftR` 1
        v4 = v :: Unsigned 4
    in if odd r
         then q { pVol = replace c v4 (pVol q) }
         else if c < 3
           then let old = pPeriod q !! c
                in q { pPeriod = replace c ((old .&. 0x3F0) .|. resize v4) (pPeriod q) }
           else q { pNoise = resize v4, pLfsr = 0x8000 }

-- | One PSG clock (16 CPU T-states).
psgTickC :: PsgC -> PsgC
psgTickC p =
  let tone i =
        let n = pCount p !! i
        in if n <= 1
             then (max 1 (pPeriod p !! i), not (pOut p !! i))
             else (n - 1, pOut p !! i)
      (c0, o0) = tone (0 :: Index 4)
      (c1, o1) = tone 1
      (c2, o2) = tone 2
      n3 = pCount p !! (3 :: Index 4)
      ctl = pNoise p
      per = case ctl .&. 3 of
        3 -> max 1 (pPeriod p !! (2 :: Index 3))
        r -> 0x10 `shiftL` fromEnum r
      o3 = pOut p !! (3 :: Index 4)
      reload = n3 <= 1
      o3' = if reload then not o3 else o3
      l = pLfsr p
      fb = if testBit ctl 2 then (l `xor` (l `shiftR` 3)) .&. 1 else l .&. 1
      l' = if reload && o3' then (l `shiftR` 1) .|. (fb `shiftL` 15) else l
      c3 = if reload then per else n3 - 1
  in p { pCount = c0 :> c1 :> c2 :> c3 :> Nil, pOut = o0 :> o1 :> o2 :> o3' :> Nil, pLfsr = l' }

-- | Attenuation → amplitude: 8191 × 10^(−a/10), 15 silent (the spec's
-- 'Gg.Spec.Psg.volumeTable').
volumes :: Vec 16 (Unsigned 13)
volumes = 8191 :> 6506 :> 5168 :> 4105 :> 3261 :> 2590 :> 2057 :> 1634 :> 1298 :> 1031
          :> 819 :> 651 :> 517 :> 411 :> 326 :> 0 :> Nil

-- | (left, right) for the stereo routing @st@ (port 06h: bits 4-7 left,
-- 0-3 right, tones then noise): each channel plus or minus its volume
-- (bipolar, as the spec's 'Gg.Spec.Psg.psgSample').
psgOutC :: Unsigned 8 -> PsgC -> (Signed 16, Signed 16)
psgOutC st p =
  let on i = if i < 3 then pOut p !! i else testBit (pLfsr p) 0
      amp i = let a = bitCoerce (resize (volumes !! (pVol p !! i)) :: Unsigned 16) :: Signed 16
              in if on i then a else negate a
      side b0 = sum (map (\i -> if testBit st (b0 + fromEnum i) then amp i else 0) chans) :: Signed 16
  in (side 4, side 0)
 where
  chans = indices d4 :: Vec 4 (Index 4)
