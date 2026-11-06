module Test.Platform.Volume (tests) where

import Prelude

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.HUnit hiding (assert)
import Test.Tasty.Hedgehog

import Platform.Audio.Volume

sample :: Gen Integer
sample = Gen.integral (Range.linearFrom 0 (-32768) 32767)

tests :: TestTree
tests = testGroup "volume (side track L)"
  [ testProperty "level 10 is the identity" $ property $ do
      x <- forAll sample
      volumeScale 10 (fromInteger x) === fromInteger x
  , testProperty "above 10 counts as 10" $ property $ do
      x <- forAll sample
      v <- forAll (Gen.integral (Range.constant 11 15))
      volumeScale (fromInteger v) (fromInteger x) === fromInteger x
  , testProperty "level 0 is silence" $ property $ do
      x <- forAll sample
      volumeScale 0 (fromInteger x) === 0
  , testProperty "never louder than the sample, never louder at a lower level" $ property $ do
      x <- forAll sample
      v <- forAll (Gen.integral (Range.constant 1 10))
      let at l = abs (toInteger (volumeScale (fromInteger l) (fromInteger x)))
      assert (at v <= abs x + 1)
      assert (at (v - 1) <= at v)
  , testCase "no overflow at the extremes" $ do
      mapM_ (\v -> do
               assertBool "max" (volumeScale v 32767 >= 0)
               assertBool "min" (volumeScale v (-32768) <= 0)) [0 .. 15]
      volumeScale 10 (-32768) @?= (-32768)
  , testCase "about 3 dB a level (within 0.3 dB)" $
      mapM_ (\v -> do
               let ratio = fromIntegral (volumeGain v) / fromIntegral (volumeGain (v + 1)) :: Double
                   db = 20 * logBase 10 ratio
               assertBool (show v ++ ": " ++ show db) (abs (db + 3) < 0.3)) [1 .. 9]
  ]
