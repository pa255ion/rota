module Test.Platform.Board (tests) where

import Prelude

import qualified Clash.Explicit.Prelude as C
import qualified Clash.Prelude as H
import Test.Tasty
import Test.Tasty.HUnit

import Platform.Board.Zedboard
import Platform.Domains
import Platform.Video.Ycbcr422 (Ycc422 (..))

samples :: [Ycc422]
samples = [ Ycc422 (fromIntegral (k `mod` 2)) 1 (k > 2) (fromIntegral (16 + k)) (fromIntegral (128 + 3 * k)) | k <- [0 .. 9 :: Int] ]

tests :: TestTree
tests = testGroup "Board.Zedboard"
  [ testCase "hdmiBus: chroma on HD_D[15:8], Y on HD_D[7:0] (HW UG Table 7, style 1, right-justified)" $ do
      hpData (hdmiBus (Ycc422 0 1 True 0x51 0xF0)) @?= 0xF051
      hdmiBus (Ycc422 0 1 True 0x51 0xF0) @?= HdmiPins 0xF051 1 0 1
      hpDe (hdmiBus (Ycc422 1 1 False 16 128)) @?= 0
      hdmiPinsIdle 1 1 @?= HdmiPins 0x8010 0 1 1

  , testCase "hdmiOut: pins one clock after the stream, idle in reset" $ do
      let clk = C.clockGen @DomPix
          rst = C.resetGen
          (pins, _) = hdmiOut clk rst C.enableGen 1 1 (C.fromList (samples ++ repeat (last samples)))
          got = C.sampleN 11 pins
      take 2 got @?= replicate 2 (hdmiPinsIdle 1 1)
      drop 2 got @?= map hdmiBus (drop 1 (take 10 samples))

  , testCase "hdmiOut: HD_CLK is the pixel clock inverted (low, then high, every cycle), low in reset" $ do
      let clk = C.clockGen @DomPix
          (_, hdClk) = hdmiOut clk C.resetGen C.enableGen 1 1 (pure (Ycc422 1 1 False 16 128))
          got = C.sampleN 12 hdClk
      -- Two samples per pixel clock, (D1, D2) of each clock. Clash's ODDR
      -- model starts with one undefined half-cycle sample; after it: the
      -- reset clock (0, 0), then (0, 1) in every clock.
      drop 1 got @?= [0, 0] ++ take 9 (cycle [0, 1])
  ]
