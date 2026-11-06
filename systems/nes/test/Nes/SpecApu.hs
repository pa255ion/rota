-- | Step 3.9: the Clash APU's timers on their own (layer 1). Regressions:
-- the DMC rate table was built in 8 bits, so rate 0 (428 CPU cycles)
-- wrapped (nes-lockstep found it only once a test played a sample), and
-- the noise table in 11 bits, so period 15 (4068) wrapped (found here).
module Nes.SpecApu
  ( apuTests
  ) where

import Prelude

import Test.Tasty
import Test.Tasty.HUnit

import Nes.Apu
import Nes.Mixer

-- | CPU cycles between two clocks of a timer (a reload: @ old new),
-- running the Clash APU from @a0@ (gets on odd cycles, as the core).
period :: (Apu -> Apu -> Bool) -> Apu -> Int
period clocked a0 = go (1 :: Int) a0 Nothing
 where
  go n a lastClock
    | n > 20000 = -1
    | otherwise =
        let a' = apuCycle (odd n) a
        in if clocked a a'
             then case lastClock of
                    Just l -> n - l
                    Nothing -> go (n + 1) a' (Just n)
             else go (n + 1) a' lastClock

dmcRates, noisePeriods :: [Int]
dmcRates = [428, 380, 340, 320, 286, 254, 226, 214, 190, 160, 142, 128, 106, 84, 72, 54]
noisePeriods = [4, 8, 16, 32, 64, 96, 128, 160, 202, 254, 380, 508, 762, 1016, 2034, 4068]

apuTests :: TestTree
apuTests = testGroup "Clash APU timers (Nes.Apu, step 3.9)"
  [ testCase "DMC: CPU cycles per output bit = the NTSC rate table" $
      [ period (\a b -> dmCount (aDmc a) == 0 && dmCount (aDmc b) == fromIntegral (r `div` 2 - 1))
               (apuWrite True 0x4010 (fromIntegral i) apuInit)
      | (i, r) <- zip [0 :: Int ..] dmcRates ] @?= dmcRates
  , testCase "DMC: a $4010 write in the cycle the timer reloads sets the new rate, a cycle later not (AccuracyCoin d103cbd, DMC Reload Timing)" $ do
      -- from the fastest rate (54 cycles: reload 26), the first reload's
      -- cycle; then $4010 = 0 (428: reload 213) in it and in the next
      let fast = apuWrite True 0x4010 15 apuInit
          run n a = if n > 2000 then (n, a) else
            let a' = apuCycle (odd n) a
            in if dmCount (aDmc a) == 0 && n > 1 && odd n then (n, a') else run (n + 1) a'
          (n0, atReload) = run (1 :: Int) fast
          same = apuWrite (odd n0) 0x4010 0 atReload
          next = apuWrite (odd (n0 + 1)) 0x4010 0 (apuCycle (odd (n0 + 1)) atReload)
      dmCount (aDmc atReload) @?= 26
      dmCount (aDmc same) @?= 213
      dmCount (aDmc next) @?= 26
  , testCase "noise: CPU cycles per LFSR clock = the NTSC period table" $
      [ period (\a b -> nsCount (aNoise a) == 0 && nsCount (aNoise b) == fromIntegral (p `div` 2 - 1))
               (apuWrite True 0x400E (fromIntegral i) apuInit)
      | (i, p) <- zip [0 :: Int ..] noisePeriods ] @?= noisePeriods
  , testCase "speakers: a triangle at timer period 0 or 1 gives its mean (step 3.9 board check)" $ do
      let tri p = apuWrite True 0x400B 0x08 (apuWrite True 0x400A p (apuWrite True 0x4008 0xFF (apuWrite True 0x4015 4 apuInit)))
      map (chTriangle . apuAudioChannels . tri) [0, 1, 2] @?= [7, 7, chTriangle (apuChannels (tri 2))]
      chTriangle (apuChannels (tri 0)) @?= 15
  , testCase "mixer tables: sizes, 0 at 0, rising, full scale under 32768" $ do
      (length pulseTable, length tndTable) @?= (31, 203)
      (take 1 pulseTable, take 1 tndTable) @?= ([0], [0])
      assertBool "pulse rises" (and (zipWith (<) pulseTable (drop 1 pulseTable)))
      assertBool "tnd rises" (and (zipWith (<) tndTable (drop 1 tndTable)))
      -- pulse_table[30] = 95.52 / (8128 / 30 + 100) = 0.2575,
      -- tnd_table[202] = 163.67 / (24329 / 202 + 100) = 0.7425 (× 32767)
      (pulseTable !! 30, tndTable !! 202) @?= (8438, 24328)
      mixLevel 15 15 15 15 127 @?= 32766
  ]
