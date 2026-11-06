module Test.Platform.Clocks (tests) where

import Prelude

import qualified Clash.Explicit.Prelude as C
import Test.Tasty
import Test.Tasty.HUnit

import Platform.Clocks
import Platform.Domains

tests :: TestTree
tests = testGroup "Clocks"
  [ testCase "pixel clock period matches the MMCM output (39720 ps)" $
      C.snatToInteger (C.clockPeriod @DomPix) @?= 39720

  , testCase "pixelClockGen: reset asserted at start, released afterwards" $ do
      let (_clk, rst) = pixelClockGen C.clockGen C.resetGen
          rs          = C.sampleN 20 (C.unsafeToActiveHigh rst)
      take 1 rs @?= [True]
      drop 10 rs @?= replicate 10 False

  , testCase "syncReset: asynchronous assert, synchronous release" $ do
      let rs = C.sampleN 10 (C.unsafeToActiveHigh
                 (syncReset (C.clockGen @DomPix) (C.resetGenN (C.SNat @3))))
      take 3 rs @?= [True, True, True]
      drop 6 rs @?= replicate 4 False
  ]
