module Test.Platform.Leds (tests) where

import Prelude
import qualified Data.List as L

import qualified Clash.Prelude as C
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog

import Platform.Output.Leds

tests :: TestTree
tests = testGroup "Leds"
  [ testCase "debugByte picks bytes LSB first" $
      map (`debugByte` 0x11223344) [0 .. 3] @?= [0x44, 0x33, 0x22, 0x11]

  , testProperty "debugByte bytes reassemble the word" $ property $ do
      w <- forAll (Gen.integral (Range.constantBounded :: Range (C.BitVector 32)))
      (debugByte 3 w C.++# debugByte 2 w C.++# debugByte 1 w C.++# debugByte 0 w) === w

  , testProperty "ledDriver: off in reset, then one cycle of latency" $ property $ do
      xs <- forAll (Gen.list (Range.linear 2 50) (Gen.integral (Range.constantBounded :: Range (C.BitVector 8))))
      let out = C.simulateN @C.System (length xs) ledDriver xs
      take 1 out === [0]
      drop 2 out === drop 1 (init xs)

  , testCase "heartbeat toggles every N cycles" $ do
      let out  = drop 1 (C.sampleN @C.System 61 (heartbeat (C.SNat @5)))  -- drop reset cycle
          runs = [ (b, 1 + length bs) | b : bs <- L.group out ]
      take 6 runs @?= [(False, 5), (True, 5), (False, 5), (True, 5), (False, 5), (True, 5)]
  ]
