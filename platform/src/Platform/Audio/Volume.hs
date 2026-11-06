{-|
The volume (side track L, the in-game menu's Options page): a gain on
the signed 16-bit PCM that every top with sound sends to its outputs
(S/PDIF, the Arty's HDMI data islands, its PWM jack), set by the PS in
the settings register ("Platform.Ps.Link", 'PsSettings').

Eleven levels about 3 dB apart: 10 is unity (the sound bit for bit as
before the volume existed, so a PS that never writes the register
changes nothing), 0 is silence. No level is above unity, so nothing can
clip. Combinational: a top puts it before the register that feeds its
transmitters.
-}
module Platform.Audio.Volume
  ( volumeGain
  , volumeScale
  , volumeMono
  , volumeStereo
  ) where

import Clash.Prelude

import Platform.Ps.Link (Volume)

-- | The gain of each level in 256ths: round (256 × 10^(−3 (10 − v) / 20)),
-- except level 0 (silence). A level above 10 counts as 10.
volumeGain :: Volume -> Unsigned 9
volumeGain v = gains !! min 10 v
 where
  gains :: Vec 11 (Unsigned 9)
  gains = 0 :> 11 :> 16 :> 23 :> 32 :> 45 :> 64 :> 91 :> 128 :> 181 :> 256 :> Nil

-- | One sample at a level (the product shifted down, rounding towards
-- minus infinity; at 10 exactly the sample).
volumeScale :: Volume -> Signed 16 -> Signed 16
volumeScale v x = truncateB ((resize x * g) `shiftR` 8 :: Signed 26)
 where
  g = bitCoerce (zeroExtend (volumeGain v)) :: Signed 26

volumeMono :: Signal dom Volume -> Signal dom (Signed 16) -> Signal dom (Signed 16)
volumeMono = liftA2 volumeScale

volumeStereo
  :: Signal dom Volume -> Signal dom (Signed 16, Signed 16) -> Signal dom (Signed 16, Signed 16)
volumeStereo = liftA2 (\v (l, r) -> (volumeScale v l, volumeScale v r))
