module Test.Platform.VgaTiming (tests) where

import Prelude
import qualified Data.List as L

import qualified Clash.Prelude as C
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog

import Codec.Picture (pixelAt)
import Platform.Sim.Frame
import Platform.Video.VgaTiming

mode :: VgaMode
mode = vga640x480

start :: VgaCounters
start = VgaCounters 0 0

-- | Every counter state of one frame, in order, starting at (0, 0).
frame :: [VgaCounters]
frame = start : takeWhile (/= start) (drop 1 (iterate (vgaNext mode) start))

frameOut :: [VgaOut]
frameOut = map (vgaDecode mode) frame

count :: (VgaOut -> Bool) -> Int
count p = length (filter p frameOut)

tests :: TestTree
tests = testGroup "VgaTiming"
  [ testCase "mode totals are 800 x 525" $ do
      axisTotal (vmH mode) @?= 800
      axisTotal (vmV mode) @?= 525

  , testCase "one frame is exactly 800*525 clocks" $
      length frame @?= 800 * 525

  , testCase "counters visit every position once per frame" $ do
      all (\(VgaCounters x y) -> x < 800 && y < 525) frame @? "counter out of range"
      map (\(VgaCounters x y) -> fromIntegral y * 800 + fromIntegral x) frame
        @?= [0 .. 800 * 525 - 1 :: Int]

  , testCase "visible area is 640 x 480" $
      count vgaActive @?= 640 * 480

  , testCase "HSYNC low for 96 clocks per line, 525 lines" $
      count ((== 0) . vgaHSync) @?= 96 * 525

  , testCase "VSYNC low for 2 lines" $
      count ((== 0) . vgaVSync) @?= 2 * 800

  , testCase "frame start and vblank start pulse once per frame" $ do
      count vgaFrameStart @?= 1
      count vgaVBlankStart @?= 1
      map (\o -> (vgaX o, vgaY o)) (filter vgaVBlankStart frameOut) @?= [(0, 480)]

  , testCase "exhaustive: sync and active levels at every position of a frame" $
      filter (\c -> vgaDecode mode c /= reference c) frame @?= []

  , testProperty "HSYNC is low exactly for x in [656, 752)" $ property $ do
      c <- forAll genCounters
      vgaHSync (vgaDecode mode c) === boolBit (not (hCount c >= 656 && hCount c < 752))

  , testProperty "VSYNC is low exactly for y in [490, 492)" $ property $ do
      c <- forAll genCounters
      vgaVSync (vgaDecode mode c) === boolBit (not (vCount c >= 490 && vCount c < 492))

  , testProperty "active exactly for x < 640 and y < 480, position = counters" $ property $ do
      c <- forAll genCounters
      let o = vgaDecode mode c
      vgaActive o === (hCount c < 640 && vCount c < 480)
      (vgaX o, vgaY o) === (hCount c, vCount c)

  , testProperty "blanking: syncs never pulse inside the visible area" $ property $ do
      c <- forAll genCounters
      let o = vgaDecode mode c
      if vgaActive o then (vgaHSync o, vgaVSync o) === (1, 1) else success

  , testCase "vgaPins: idle in reset, then one cycle behind its inputs" $ do
      let pins = C.sampleN @C.System 3000 (vgaPins mode (vgaTiming mode) (pure (fromRgb444 (Rgb444 1 2 3))))
          outs = C.sampleN @C.System 3000 (vgaTiming mode)
      -- reset is still asserted at the first clock edge, so two idle samples
      take 2 pins @?= replicate 2 (vgaPinsIdle mode)
      [ k | (k, p, o) <- zip3 [1 :: Int ..] (drop 2 pins) (drop 1 outs)
          , p /= vgaPinLevels o (fromRgb444 (Rgb444 1 2 3)) ] @?= []

  , testCase "decodeFrames: a tiny mode through vgaTiming and vgaPins" $ do
      -- 4x2 visible; 8 clocks per line, 5 lines per frame
      let tiny = VgaMode (Axis 4 1 2 1 SyncNegative) (Axis 2 1 1 1 SyncNegative)
          colour o = Rgb444 (C.resize (C.pack (vgaX o))) (C.resize (C.pack (vgaY o))) 7
          pins = C.sample @C.System (C.withClockResetEnable C.clockGen C.resetGen C.enableGen
                   (let o = vgaTiming tiny in vgaPins tiny o (fromRgb444 . colour <$> o)))
          frames = take 2 (decodeFrames (vgaSink tiny) pins)
          px (x, y) = rgbPixel (Rgb444 (fromIntegral x) (fromIntegral y) 7)
      length frames @?= 2
      sequence_ [ pixelAt f x y @?= px (x, y) | f <- frames, x <- [0 .. 3], y <- [0 .. 1] ]

  , testCase "Signal: starts at (0,0) after reset, then follows vgaNext, frame period 420000" $ do
      let n     = 2 * 800 * 525 + 10
          outs  = drop 1 (C.sampleN @C.System n (vgaTiming mode))   -- drop the reset cycle
          pos   = map (\o -> VgaCounters (vgaX o) (vgaY o)) outs
      take 1 pos @?= [start]
      and (zipWith (\a b -> b == vgaNext mode a) pos (drop 1 pos)) @? "sequence does not follow vgaNext"
      L.findIndices vgaFrameStart outs @?= [0, 420000, 840000]
  ]

-- | Independent restatement of the 640x480 timing table in docs/ZEDBOARD.md.
reference :: VgaCounters -> VgaOut
reference (VgaCounters x y) = VgaOut
  { vgaHSync       = boolBit (not (x >= 640 + 16 && x < 640 + 16 + 96))
  , vgaVSync       = boolBit (not (y >= 480 + 10 && y < 480 + 10 + 2))
  , vgaActive      = x < 640 && y < 480
  , vgaX           = x
  , vgaY           = y
  , vgaFrameStart  = x == 0 && y == 0
  , vgaVBlankStart = x == 0 && y == 480
  }

genCounters :: Gen VgaCounters
genCounters = VgaCounters
  <$> Gen.integral (Range.linear 0 799)
  <*> Gen.integral (Range.linear 0 524)

boolBit :: Bool -> C.Bit
boolBit = C.boolToBit
