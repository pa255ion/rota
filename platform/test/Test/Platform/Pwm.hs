module Test.Platform.Pwm (tests) where

import Prelude

import qualified Clash.Prelude as C
import Test.Tasty
import Test.Tasty.HUnit

import Platform.Audio.Pwm

-- | The ones among the first n bits for a constant sample.
ones :: Int -> C.Signed 16 -> Int
ones n s = length (filter (== 1) (take n (go 0)))
 where
  go acc = let (acc', b) = pwmStep acc s in b : go acc'

tests :: TestTree
tests = testGroup "Audio.Pwm"
  [ testCase "density follows the sample within one bit, over any window from the start" $
      sequence_
        [ do let n = 4096
                 want = fromIntegral n * (fromIntegral s + 32768) / 65536 :: Double
                 got = fromIntegral (ones n s)
             assertBool (show (s, got, want)) (abs (got - want) <= 1)
        | s <- [minBound, -20000, -1, 0, 1, 12345, maxBound :: C.Signed 16] ]

  , testCase "silence is a steady 50 % (alternating once settled), no long runs" $ do
      let bits = take 64 (go 0) where go acc = let (acc', b) = pwmStep acc 0 in b : go acc'
      assertBool (show bits) (all (\(a, b) -> a /= b) (zip (drop 2 bits) (drop 3 bits)))

  , testCase "pwmOut: registered (0 in reset), the same bits one clock later" $ do
      let ins = [0, 0, 1000, -1000, 30000, -30000, 0, 0] :: [C.Signed 16]
          got = C.sampleN (length ins + 1) (C.withClockResetEnable (C.clockGen @C.System) C.resetGen C.enableGen
                  (pwmOut (C.fromList (head ins : ins ++ repeat 0))))
          want = snd (foldl (\(acc, out) s -> let (acc', b) = pwmStep acc s in (acc', out ++ [b])) (0, []) ins)
      take 2 got @?= [0, 0]
      drop 2 got @?= take (length ins - 1) want
  ]
