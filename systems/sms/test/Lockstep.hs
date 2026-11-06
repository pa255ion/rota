-- | Layer 2 (M6 step 6.5): the YM2413 in Clash ("Sms.Fm") in lockstep with
-- its reference ("Sms.Spec.Fm"): the same register programs (sms-spec's
-- random ones, which keep the manual's waits, and a note), every pin of
-- every cycle equal (the Clash unit gives a cycle's pins two cycles ahead),
-- but the first two samples (the pipeline filling). Planted bugs in the
-- Clash unit are caught.
module Main (main) where

import Prelude

import Clash.Prelude (Signed)
import Control.Monad (forM_)
import Data.Bits (xor)
import qualified Data.Vector.Unboxed as U
import System.Environment (lookupEnv)
import Test.Tasty
import Test.Tasty.HUnit
import Text.Printf (printf)

import FmOracle (Program, randomProgram, runModel)
import Sms.Fm

-- | The Clash unit's pins for a program, cycle by cycle (the pins of cycle
-- c + 2 at index c).
runClash :: (FmWrite -> FmC -> (FmC, (Signed 10, Signed 10))) -> Program -> Int -> U.Vector (Int, Int)
runClash step prog cycles = U.fromList (go 0 fmPowerOn prog)
 where
  go c st ws
    | c >= cycles = []
    | otherwise =
        let (now, rest) = span (\(wc, _, _) -> wc == c) ws
            w = case now of
              [] -> Nothing
              ((_, p, v) : _) -> Just (odd p, fromIntegral v)
            (st', (mo, ro)) = step w st
        in st' `seq` ((fromIntegral mo, fromIntegral ro) : go (c + 1) st' rest)

compareClash :: (FmWrite -> FmC -> (FmC, (Signed 10, Signed 10))) -> Program -> Int -> Maybe String
compareClash step prog samples =
  let spec = runModel prog samples
      n = 18 * samples
      hw = runClash step prog n
  in case [ c | c <- [36 .. n - 1], c - 2 >= 0, hw U.! (c - 2) /= spec U.! c ] of
       [] -> Nothing
       (c : _) -> Just (printf "cycle %d (sample %d, cycle %d): spec (MO, RO) %s, Clash %s"
                          c (c `div` 18) (c `mod` 18) (show (spec U.! c)) (show (hw U.! (c - 2))))

-- | Rhythm mode switched on and off 300 times at pseudo-random offsets
-- (40 to 103 cycles apart), the drums keyed and voiced, a melodic note on
-- channels 6-8 as well: the pipeline's rhythm reads at their cycles.
rhythmStorm :: Program
rhythmStorm = setup <> go (1 :: Int) 2000 (0x5DEECE66D :: Int)
 where
  wr c a v = [(c, 0, a), (c + 3, 1, v)]
  setup = concat [ wr (40 + 30 * i) a v | (i, (a, v)) <- zip [0 ..]
    [ (0x16, 0x20), (0x17, 0x50), (0x18, 0xC0), (0x26, 0x15), (0x27, 0x15), (0x28, 0x11)
    , (0x36, 0x52), (0x37, 0x11), (0x38, 0x22) ] ]
  go k c r
    | k > 300 = []
    | otherwise =
        let r' = (r * 1103515245 + 12345) `mod` 2147483648
            v = (if odd k then 0x20 else 0x00) + fromIntegral (r' `div` 65536 `mod` 32)
        in wr c 0x0E v <> go (k + 1) (c + 40 + r' `mod` 64) r'

-- | A Clash unit with a planted bug: its pins' MO negated on cycle 7 (ch 0).
brokenPins :: FmWrite -> FmC -> (FmC, (Signed 10, Signed 10))
brokenPins w st = let (st', (mo, ro)) = fmCycle w st in (st', (if mo == 5 then mo `xor` 1 else mo, ro))

main :: IO ()
main = do
  full <- (== Just "1") <$> lookupEnv "EMU_FULL_TESTS"
  let seeds = if full then [1 .. 40] else [1 .. 4] :: [Int]
      writes = if full then 2000 else 300
      samples = writes * 1100 `div` 72
      note = [ w | (i, (a, v)) <- zip [0 :: Int ..] [(0x30, 0x10), (0x10, 0x20), (0x20, 0x19)]
                 , w <- [(10 + 24 * i, 0, a), (13 + 24 * i, 1, v)] ]
  defaultMain $ testGroup "sms-lockstep"
    [ testGroup "YM2413: Sms.Fm = Sms.Spec.Fm, every pin" $
        testCase "one note (instrument 1, A4)" (forM_ (compareClash fmCycle note 3000) assertFailure)
        : testCase "rhythm mode switched 300 times" (forM_ (compareClash fmCycle rhythmStorm 1500) assertFailure)
        : [ testCase (printf "random program %d" seed) (forM_ (compareClash fmCycle (randomProgram seed writes) samples) assertFailure)
          | seed <- seeds ]
    , testCase "a broken unit is caught (a pin value changed)" $
        assertBool "not caught" (compareClash brokenPins (randomProgram 1 300) 4000 /= Nothing)
    ]
