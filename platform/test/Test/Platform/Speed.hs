module Test.Platform.Speed (tests) where

import Prelude

import qualified Clash.Prelude as C
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog

import Platform.Domains (DomPix)
import Platform.Speed

-- | Ticks in the first n clocks.
ticks :: Int -> C.Unsigned 32 -> [C.Unsigned 4] -> Int
ticks n inc qs = length (filter id (take n (C.simulate @DomPix (C.withClockResetEnable C.clockGen C.resetGen C.enableGen (speedEnable inc)) (qs ++ repeat (last qs)))))

-- | The accumulator without a speed, as the systems' Display modules
-- define it (Gb.Display.dotEnable).
plain :: Int -> C.Unsigned 32 -> Int
plain n inc = length (filter id (take n (go 0)))
 where
  go :: Integer -> [Bool]
  go a = let a' = (a `mod` 2 ^ (32 :: Int)) + toInteger inc in (a' >= 2 ^ (32 :: Int)) : go a'

tests :: TestTree
tests = testGroup "speed (side track L)"
  [ testProperty "at 100 % exactly the plain accumulator" $ property $ do
      inc <- forAll (Gen.integral (Range.linear 1 maxBound))
      speedIncrement inc 4 === inc
  , testCase "the Game Boy's dots at 100 %: the same clocks as Gb.Display's" $
      ticks 5000 715531909 [4] @?= plain 5000 715531909
  , testCase "50 %, 75 %, 150 %, 200 %: the rate in proportion (within a tick)" $
      mapM_ (\q -> do
               let n = 200000; inc = 715531909
                   got = ticks n inc [q]
                   want = fromIntegral n * fromIntegral inc * fromIntegral q / 4 / 2 ^ (32 :: Int) :: Double
               assertBool (show (q, got, want)) (abs (fromIntegral got - want) <= 2)) [2, 3, 6, 8]
  , testCase "clamped: below 2 is 50 %, above 8 is 200 %; held below 2^32" $ do
      speedIncrement 1000 0 @?= 500
      speedIncrement 1000 15 @?= 2000
      speedIncrement 3663938527 8 @?= maxBound
      speedIncrement 3663938527 5 @?= maxBound
  ]
