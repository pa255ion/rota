{-|
The APU's mixer as lookup tables (nesdev wiki, "APU Mixer", its lookup
table form, fetched 2026-10-03):

> pulse_table[n] = 95.52 / (8128.0 / n + 100)      n = p1 + p2, 0..30
> tnd_table[n]   = 163.67 / (24329.0 / n + 100)    n = 3t + 2n + d, 0..202

(0 for n = 0), scaled to 15 bits here. Their sum is the NES's output, 0
to 1.0. Shared by the Clash audio path ("Nes.Audio", as ROMs) and tests.
-}
module Nes.Mixer
  ( pulseTable
  , tndTable
  , mixLevel
  ) where

import Prelude

scale :: Double
scale = 32767

pulseTable :: [Int]
pulseTable = [ if n == 0 then 0 else round (scale * 95.52 / (8128 / fromIntegral n + 100)) | n <- [0 .. 30 :: Int] ]

tndTable :: [Int]
tndTable = [ if n == 0 then 0 else round (scale * 163.67 / (24329 / fromIntegral n + 100)) | n <- [0 .. 202 :: Int] ]

-- | The mixed level of pulse 1, pulse 2, triangle, noise (0 to 15) and
-- DMC (0 to 127): 0 to 32767.
mixLevel :: Int -> Int -> Int -> Int -> Int -> Int
mixLevel p1 p2 t n d = pulseTable !! (p1 + p2) + tndTable !! (3 * t + 2 * n + d)
