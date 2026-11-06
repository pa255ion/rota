{-|
800XL sound for the S/PDIF output (M4 step 4.7; docs/systems/atari800xl.md,
"Sound"): POKEY's mixed level ('A800.Pokey.poLevel', 0 to 824) through
the output curve of "A800.Mixer" (a ROM of its table), a low-pass (two
one-pole stages, as the NES's, for the 1.79 MHz steps) and the AC
coupling (a slow high-pass), as signed 16-bit PCM for 'spdifDdr'.
-}
module A800.Audio
  ( mixRom
  , pokeySample
  , audioFilter
  ) where

import Clash.Prelude

import A800.Mixer (maxLevel, mixCurve)

-- | The curve, saturated to 16 bits.
mixRom :: Vec 825 (Unsigned 16)
mixRom = $(listToVecTH [ fromIntegral (min 32767 (mixCurve l)) :: Unsigned 16 | l <- [0 .. maxLevel] ])

pokeySample :: Unsigned 10 -> Unsigned 16
pokeySample l = mixRom !! (min 824 l)

-- | The low-passes and the high-pass, one step per clock: signed 16-bit
-- PCM out (silence at power-on: the level starts at 0).
audioFilter :: forall dom. HiddenClockResetEnable dom => Signal dom (Unsigned 16) -> Signal dom (Signed 16)
audioFilter x = out <$> lp2 <*> dc
 where
  -- 16 fraction bits
  x16 = (\v -> resize (bitCoerce v :: Signed 16) `shiftL` 16) <$> x :: Signal dom (Signed 40)
  lp1 = register 0 ((\a b -> a + ((b - a) `shiftR` 8)) <$> lp1 <*> x16)
  lp2 = register 0 ((\a b -> a + ((b - a) `shiftR` 8)) <$> lp2 <*> lp1)
  dc = register 0 ((\a b -> a + ((b - a) `shiftR` 18)) <$> dc <*> lp2)
  out l c = let v = (l - c) `shiftR` 16
            in if v > 32767 then 32767 else if v < -32768 then -32768 else resize v
