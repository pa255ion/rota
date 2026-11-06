module Test.Platform.Ycbcr422 (tests) where

import Prelude

import qualified Clash.Explicit.Prelude as C
import qualified Clash.Prelude as H
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog

import Platform.Video.VgaTiming (Rgb444 (..), Rgb888 (..), fromRgb444)
import Platform.Video.Ycbcr422

------------------------------------------------------------------------
-- Reference model, in Double

kr, kb, kg :: Double
kr = 0.299
kb = 0.114
kg = 1 - kr - kb

-- | BT.601 limited range of a 24-bit colour.
refYcc :: Rgb888 -> (Double, Double, Double)
refYcc (Rgb888 r g b) = (y, cb, cr)
 where
  f v = fromIntegral v / 255 :: Double
  (r', g', b') = (f r, f g, f b)
  ey = kr * r' + kg * g' + kb * b'
  y  = 16 + 219 * ey
  cb = 128 + 224 * (b' - ey) / (2 * (1 - kb))
  cr = 128 + 224 * (r' - ey) / (2 * (1 - kr))

-- | The expected bus for a stream: data as Doubles, @ycbcrLatency@ clocks
-- late, blank (with the first input's sync levels) before that.
data RefOut = RefOut Bit Bit Bool Double Double
  deriving (Show, Eq)

type Bit = H.Bit

ref422 :: [PixelIn] -> [RefOut]
ref422 pxs = replicate ycbcrLatency (blankOf (firstOf pxs)) ++ go Nothing pxs
 where
  blankOf p = RefOut (piHSync p) (piVSync p) False 16 128
  -- the previous pixel of the line, if it was even (the partner of an odd one)
  go _ [] = []
  go evenPrev (p : rest)
    | not (piDe p) = blankOf p : go Nothing rest
    | otherwise = case evenPrev of
        Just q  ->  -- p is odd
          let (y, _, cr) = refYcc (piRgb p); (_, _, crq) = refYcc (piRgb q)
          in RefOut (piHSync p) (piVSync p) True y ((cr + crq) / 2) : go Nothing rest
        Nothing ->  -- p is even; its partner is the next pixel if that is active
          let (y, cb, _) = refYcc (piRgb p)
              cbn = case rest of
                n : _ | piDe n -> let (_, c, _) = refYcc (piRgb n) in c
                _              -> cb
          in RefOut (piHSync p) (piVSync p) True y ((cb + cbn) / 2) : go (Just p) rest

firstOf :: [PixelIn] -> PixelIn
firstOf pxs = case pxs of
  p : _ -> p
  []    -> blank

-- | Run the pure step function over a stream (reset state with the first
-- input's sync levels).
runYcc :: [PixelIn] -> [Ycc422]
runYcc pxs = map ysOut (scanl yccStep (yccInit (piHSync p0) (piVSync p0)) pxs)
 where p0 = firstOf pxs

-- | Mismatches between the hardware and the reference: syncs and DE exact,
-- data within @tol@.
mismatches :: Double -> [PixelIn] -> [(Int, Ycc422, RefOut)]
mismatches tol pxs =
  [ (i, o, r) | (i, o, r@(RefOut h v de y c)) <- zip3 [0 ..] (runYcc pxs) (ref422 pxs)
  , ycHSync o /= h || ycVSync o /= v || ycDe o /= de
      || abs (fromIntegral (ycY o) - y) > tol || abs (fromIntegral (ycC o) - c) > tol ]

------------------------------------------------------------------------

allColours :: [Rgb444]
allColours = [ Rgb444 r g b | r <- [0 .. 15], g <- [0 .. 15], b <- [0 .. 15] ]

active :: Rgb444 -> PixelIn
active = PixelIn 1 1 True . fromRgb444

blank :: PixelIn
blank = PixelIn 1 1 False (Rgb888 0 0 0)

-- | A line: porch, then the colours with DE.
line :: Int -> [Rgb444] -> [PixelIn]
line porch cs = replicate porch blank ++ map active cs

genStream :: Gen [PixelIn]
genStream = do
  ls <- Gen.list (Range.linear 1 6) $ do
    porch <- Gen.int (Range.linear 0 4)
    n     <- Gen.int (Range.linear 0 9)
    hs    <- Gen.element [0, 1]
    vs    <- Gen.element [0, 1]
    cs    <- Gen.list (Range.singleton n) genColour
    pure (replicate porch (PixelIn hs vs False (Rgb888 0 0 0)) ++ map (PixelIn hs vs True) cs)
  pure (concat ls ++ replicate 4 blank)
 where
  -- any 24-bit colour (the NES palette isn't on the 12-bit grid)
  genColour = Rgb888 <$> byte <*> byte <*> byte
  byte = fromIntegral <$> Gen.int (Range.constant 0 255)

tests :: TestTree
tests = testGroup "Ycbcr422"
  [ testCase "coefficients: within 1 of 4096/15 x BT.601, chroma rows sum to 0" $ do
      let exact = [ [219 * kr, 219 * kg, 219 * kb]
                  , [-224 * kr / (2 * (1 - kb)), -224 * kg / (2 * (1 - kb)), 112]
                  , [112, -224 * kg / (2 * (1 - kr)), -224 * kb / (2 * (1 - kr))] ]
          ours  = map (map fromIntegral . H.toList) [kY, kCb, kCr] :: [[Double]]
      assertBool "within 1" (and (zipWith (\e o -> all (<= 1) (zipWith (\a b -> abs (a * 4096 / 15 - b)) e o)) exact ours))
      sum (H.toList kCb) @?= 0
      sum (H.toList kCr) @?= 0

  , testCase "all 4096 colours (as pixel pairs): Y, Cb, Cr within 0.51 of the Double model" $
      mismatches 0.51 (concatMap (\c -> line 0 [c, c] ++ [blank]) allColours ++ replicate 4 blank) @?= []

  , testCase "24-bit converter = the 12-bit one, exactly, for all 4096 widened colours" $
      [ c | c <- allColours, toFixed (fromRgb444 c) /= toFixed444 c ] @?= []

  , testCase "all 256 24-bit greys: no colour" $
      [ v | v <- [0 .. 255 :: Int]
          , let g = fromIntegral v
                o = [ ycC x | x <- runYcc ([PixelIn 1 1 True (Rgb888 g g g), PixelIn 1 1 True (Rgb888 g g g)] ++ replicate 4 blank), ycDe x ]
          , o /= [128, 128] ] @?= []

  , testCase "black, white, greys: exactly 16/235, no colour" $ do
      let out c = [ (ycY o, ycC o) | o <- runYcc (line 0 [c, c] ++ replicate 4 blank), ycDe o ]
      out (Rgb444 0 0 0) @?= [(16, 128), (16, 128)]
      out (Rgb444 15 15 15) @?= [(235, 128), (235, 128)]
      and [ all ((== 128) . snd) (out (Rgb444 v v v)) | v <- [0 .. 15] ] @?= True

  , testCase "phase restarts each line: odd-length line, then Cb first again" $ do
      let red = Rgb444 15 0 0; blue = Rgb444 0 0 15
          pxs = line 2 [red, red, red] ++ line 3 [blue, blue] ++ replicate 4 blank
          cs = [ ycC o | o <- runYcc pxs, ycDe o ]
          (_, cbR, crR) = refYcc (fromRgb444 red)
          (_, cbB, crB) = refYcc (fromRgb444 blue)
      map fromIntegral cs @?= map (round :: Double -> Integer) [cbR, crR, cbR, cbB, crB]

  , testCase "pairs average: a colour edge inside a pair gives the mean chroma" $ do
      let pxs = line 0 [Rgb444 15 0 0, Rgb444 0 0 15] ++ replicate 4 blank
      mismatches 0.51 pxs @?= []

  , testProperty "random streams (DE patterns, syncs, colours) = the reference, 3 clocks late" $ property $ do
      pxs <- forAll genStream
      mismatches 0.51 pxs === []

  , testCase "circuit (ycbcr422) = the pure step function, idle during reset" $ do
      let pxs = concat [ line 3 (take 7 (drop (37 * k) allColours)) | k <- [0 .. 5] ] ++ replicate 4 blank
          n = length pxs + 1
          dut = H.withClockResetEnable @C.System C.clockGen C.resetGen C.enableGen
                  (ycbcr422 1 1 (C.fromList (blank : pxs ++ repeat blank)))
          -- sample k is the register after k inputs, as in runYcc
      C.sampleN n dut @?= take n (runYcc (blank : pxs))
  ]
