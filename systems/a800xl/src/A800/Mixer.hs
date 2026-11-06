{-|
POKEY's output curve (M4 step 4.4; docs/systems/atari800xl.md, "Sound"):
from the sum of the channels' weighted levels ("A800.Spec.Pokey".
'pokeyLevel', 0 to 824: four channels × the volume bits' weights 12, 26,
56, 112) to a 16-bit sample.

POKEY's mix isn't a plain sum (HRM 5.3, "Volume control"): it is nearly
linear up to one channel's full volume and saturates above, two channels
at full volume giving only about 50% more than one. The curve is
Altirra's fit to scope measurements of POKEY's output pin (a value and a
rule, as the NES's mixer took nesdev's): with /x/ the level over its
maximum, /x/ below 0.14, else 0.14 + (1 − e^(−2.85 (x − 0.14))) / 2.85,
times 2.1; scaled to 32767 (positive only: the AC coupling comes later).
-}
module A800.Mixer
  ( maxLevel
  , mixCurve
  , mixTable
  ) where

import Prelude

import qualified Data.Vector.Unboxed as U

maxLevel :: Int
maxLevel = 4 * (12 + 26 + 56 + 112)

mixTable :: U.Vector Int
mixTable = U.generate (maxLevel + 1) curve
 where
  curve l =
    let x = fromIntegral l / fromIntegral maxLevel :: Double
        y | x < 0.14 = x
          | otherwise = 0.14 + (1 - exp (-2.85 * (x - 0.14))) / 2.85
    in round (y * 2.1 * 32767 / 0.968)

-- | The sample for a level (0 to 'maxLevel').
mixCurve :: Int -> Int
mixCurve l = U.unsafeIndex mixTable (max 0 (min maxLevel l))
