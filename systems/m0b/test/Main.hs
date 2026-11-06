module Main (main) where

import Prelude

import Codec.Picture (generateImage)
import Data.Bits (testBit, (.&.))
import qualified Clash.Explicit.Prelude as C
import qualified Clash.Prelude as H
import Hedgehog hiding (assert)
import qualified Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Golden.Advanced (goldenTest)
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog

import M0b.Pattern
import M0b.Top (topEntity)
import Platform.Domains
import Platform.Sim.Frame
import Platform.Video.VgaTiming

main :: IO ()
main = defaultMain $ testGroup "m0b"
  [ testGroup "background"
    [ testCase "exhaustive: every visible pixel against the documented layout" $
        [ (x, y) | x <- [0 .. 639], y <- [0 .. 479], background x y /= reference x y ] @?= []
    , testCase "grey ramp is monotonic, 16 steps of 40 px" $
        map (\x -> rgbR (background x 350)) [20, 60 .. 620] @?= [0 .. 15]
    , testCase "bar colours in SMPTE order" $
        map (\i -> background (40 + 80 * i) 100) [0 .. 7]
          @?= [ c 1 1 1, c 1 1 0, c 0 1 1, c 0 1 0, c 1 0 1, c 1 0 0, c 0 0 1, c 0 0 0 ]
    ]

  , testGroup "square"
    [ testProperty "stays inside the screen and moves 2 px / 1 px per frame" $ property $ do
        n <- forAll (Gen.int (Range.linear 0 5000))
        let Square x y _ _  = squareAt n
            Square x' y' _ _ = squareAt (n + 1)
        Hedgehog.assert (x <= squareMaxX && y <= squareMaxY)
        absDiff x x' === 2
        absDiff y y' === 1
    , testCase "reaches all four edges" $ do
        let sqs = map squareAt [0 .. 2000]
        assertBool "left"   (any ((== 0) . sqX) sqs)
        assertBool "right"  (any ((== squareMaxX) . sqX) sqs)
        assertBool "top"    (any ((== 0) . sqY) sqs)
        assertBool "bottom" (any ((== squareMaxY) . sqY) sqs)
    , testCase "drawn orange over the background, 32 x 32" $ do
        let sq = squareInit
            orange = [ (x, y) | x <- [0 .. 639], y <- [0 .. 479]
                     , testPattern sq x y == Rgb444 15 8 0 ]
        length orange @?= 32 * 32
        minimum orange @?= (sqX sq, sqY sq)
    ]

  , testGroup "vgaPinLevels"
    [ testProperty "black outside the visible area, syncs passed through" $ property $ do
        cs <- forAll (VgaCounters <$> Gen.integral (Range.linear 0 799) <*> Gen.integral (Range.linear 0 524))
        let o = vgaDecode vga640x480 cs
            p = vgaPinLevels o (fromRgb444 (Rgb444 15 15 15))
        (vpHSync p, vpVSync p) === (vgaHSync o, vgaVSync o)
        vpRgb p === (if vgaActive o then Rgb444 15 15 15 else black)
    ]

  , testGroup "m0bVideo (Clash simulation, decoded from the pins)"
    [ testCase "decoded frames 0 to 2 are the pattern of frames 1 to 3" $
        sequence_
          [ maybe (pure ()) (assertFailure . (("frame " <> show k <> ": ") <>))
              (frameDiff f (render (squareAt (k + 1))))
          | (k, f) <- zip [0 :: Int ..] simFrames ]
    , goldenTest "frame 2 matches test/golden/frame_0002.png (--accept to update)"
        (either error id <$> readFrame golden)
        (pure (simFrames !! 2))
        (\g t -> pure (frameDiff g t))
        (writeFrame golden)
    ]

  , testGroup "topEntity (through the clock wizard model)"
    [ testCase "port order: LD, VGA_R/G/B, VGA_HS, VGA_VS" $ do
        let n    = 2 * 800
            outs = C.sampleN n (H.bundle (topEntity C.clockGen
                     (pure 0) (pure 0) (pure 0) (pure 0) (pure 0) (pure 0x81)))
            (lds, rs, gs, bs, hss, vss) = unzip6 outs
        last lds .&. 0x7F @?= 0x01
        -- line 0 is the white border: all three channels full somewhere
        assertBool "R" (15 `elem` rs)
        assertBool "G" (15 `elem` gs)
        assertBool "B" (15 `elem` bs)
        -- one HSYNC pulse of 96 clocks per line, no VSYNC in lines 0 and 1
        length (filter (== 0) hss) @?= 2 * 96
        assertBool "VSYNC" (all (== 1) vss)
        assertBool "HSYNC edge" (any (\(a, b) -> a == 1 && b == 0) (zip hss (drop 1 hss)))
    ]
  ]
 where
  c r g b = Rgb444 (15 * r) (15 * g) (15 * b)
  absDiff a b = if a > b then a - b else b - a
  golden = "test/golden/frame_0002.png"

-- | Independent restatement of the layout in "M0b.Pattern".
reference :: H.Unsigned 10 -> H.Unsigned 10 -> Rgb444
reference x y
  | x == 0 || x == 639 || y == 0 || y == 479 = Rgb444 15 15 15
  | y < 320 = let i = x `div` 80
                  bit n = if testBit (fromIntegral i :: Int) n then 0 else 15
              in Rgb444 (bit 1) (bit 2) (bit 0)
  | y < 400 = Rgb444 l l l
  | y < 427 = Rgb444 l 0 0
  | y < 454 = Rgb444 0 l 0
  | otherwise = Rgb444 0 0 l
 where
  l = fromIntegral (x `div` 40)

render :: Square -> Frame
render sq = generateImage (\x y -> rgbPixel (testPattern sq (fromIntegral x) (fromIntegral y))) 640 480

-- | The first three frames decoded from m0bVideo's pins.
simFrames :: [Frame]
simFrames = take 3 (decodeFrames (vgaSink vga640x480)
              (C.sample (H.withClockResetEnable @DomPix C.clockGen C.resetGen C.enableGen m0bVideo)))

unzip6 :: [(a, b, c, d, e, f)] -> ([a], [b], [c], [d], [e], [f])
unzip6 xs = ( [a | (a, _, _, _, _, _) <- xs], [b | (_, b, _, _, _, _) <- xs]
            , [c | (_, _, c, _, _, _) <- xs], [d | (_, _, _, d, _, _) <- xs]
            , [e | (_, _, _, _, e, _) <- xs], [f | (_, _, _, _, _, f) <- xs] )
