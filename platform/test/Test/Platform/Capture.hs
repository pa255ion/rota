module Test.Platform.Capture (tests) where

import Prelude

import qualified Clash.Prelude as C
import qualified Data.Map.Strict as M
import Data.Bits (shiftR, testBit, (.&.))
import Data.Maybe (isJust)
import Data.Word (Word64, Word8)
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog

import Platform.Domains (DomPix)
import Platform.Mem.Axi
import Platform.Mem.Sim
import Platform.Ps.Link (LinkAddr)
import Platform.Video.Capture

-- | A 6 x 4 picture.
picW, picH :: Int
picW = 6
picH = 4

type In = (Maybe (LinkAddr, C.BitVector 8), Bool, Maybe (C.Unsigned 24, Rgb24))

-- | The colour of pixel i in frame f.
colour :: Int -> Int -> Rgb24
colour f i = fromIntegral ((f * 0x1F3 + i * 0x2A7 + 0x10203) `mod` 0x1000000)

-- | Frames of `n` pixels, `gap` clocks apart (the core's dot rate), each
-- ended by its frame-end pulse and 20 quiet clocks; requests (clock,
-- sequence number).
frames :: Int -> Int -> Int -> [(Int, C.BitVector 8)] -> [In]
frames n nFrames gap reqs =
  [ (lookupReq t, fe, px) | (t, (fe, px)) <- zip [0 ..] (concatMap frame [0 .. nFrames - 1] ++ replicate 6000 (False, Nothing)) ]
 where
  frame f = concat [ (False, Just (fromIntegral i, colour f i)) : replicate (gap - 1) (False, Nothing) | i <- [0 .. n - 1] ]
            ++ [(True, Nothing)] ++ replicate 20 (False, Nothing)
  lookupReq t = case [ sq | (c, sq) <- reqs, c == t ] of
    sq : _ -> Just (captureAddr, sq)
    [] -> Nothing

-- | The capture and the write slave in a loop: per clock, the master's
-- outputs, the slave's, what the slave took, and busy.
run :: Int -> Int -> SlaveConfig -> Word64 -> [In] -> [(AxiWrM2S, AxiWrS2M, Maybe (AxiAddr, AxiData, C.BitVector 8), Bool)]
run w h cfg seed ins = C.simulateN @DomPix (length ins) top ins
 where
  top i = C.withClockResetEnable C.clockGen C.resetGen C.enableGen $
    let (link, fe, px) = C.unbundle i
        (m2s, busy) = C.unbundle (C.mealy (capStep (fromIntegral w) (fromIntegral h)) capInit (C.bundle (link, fe, px, s2m)))
        (s2m, taken) = axiWrSlave cfg seed m2s
    in C.bundle (m2s, s2m, taken, busy)

-- | Memory after the writes taken (byte address -> byte; strobes honoured).
memory :: [Maybe (AxiAddr, AxiData, C.BitVector 8)] -> M.Map Integer Word8
memory = foldl put M.empty
 where
  put m Nothing = m
  put m (Just (a, d, s)) = foldl (\m' k -> if testBit s k then M.insert (toInteger a + toInteger k) (fromIntegral ((C.unpack d :: Word64) `shiftR` (8 * k))) m' else m') m [0 .. 7]

word64At :: M.Map Integer Word8 -> Integer -> Maybe Word64
word64At m a = foldr (\k acc -> (\b w -> fromIntegral b `C.shiftL` (8 * k) + w) <$> M.lookup (a + toInteger k) m <*> acc) (Just 0) [0 .. 7 :: Int]

-- | The AXI rules over a run: AW and W, once valid, stay the same until
-- taken; one-beat bursts, WLAST set.
axiRules :: [(AxiWrM2S, AxiWrS2M, a, b)] -> [String]
axiRules xs = concat (zipWith check xs (drop 1 xs))
 where
  check (m, s, _, _) (m', _, _, _) =
    [ "AW dropped or changed before AWREADY" | Just aw <- [m2sAw m], not (s2mAwReady s), m2sAw m' /= Just aw ]
    ++ [ "W dropped or changed before WREADY" | Just w <- [m2sW m], not (s2mWReady s), m2sW m' /= Just w ]
    ++ [ "a burst longer than one beat" | Just aw <- [m2sAw m], awLen aw /= 0 ]
    ++ [ "W without WLAST" | Just w <- [m2sW m], not (wLast w) ]

-- | The picture frame f should leave in DDR (RGBX).
expectPicture :: M.Map Integer Word8 -> Int -> [String]
expectPicture m f =
  [ "pixel " ++ show i ++ " wrong" | i <- [0 .. picW * picH - 1]
  , let c = colour f i
        want = [fromIntegral (c `shiftR` 16), fromIntegral (c `shiftR` 8 .&. 0xFF), fromIntegral (c .&. 0xFF), 0] :: [Word8]
        got = [ M.lookup (toInteger captureBase + 4 * toInteger i + k) m | k <- [0 .. 3] ]
  , got /= map Just want ]

tests :: TestTree
tests = testGroup "Platform.Video.Capture"
  [ testProperty "the frame after the request, byte for byte, then the trailer; any DDR timing, any dot rate" $ property $ do
      seed <- forAll (Gen.word64 (Range.linear 1 maxBound))
      lat <- forAll (Gen.int (Range.linear 0 40))
      gap <- forAll (Gen.int (Range.linear 2 7))
      -- a request somewhere in frame 0, or after its end pulse before frame
      -- 1's first pixel (a core paused there by frame advance): frame 1
      reqAt <- forAll (Gen.int (Range.linear 0 (picW * picH * gap + 20)))
      let ins = frames (picW * picH) 4 gap [(reqAt, 7)]
          out = run picW picH (slaveRandom lat) seed ins
          m = memory [ t | (_, _, t, _) <- out ]
      axiRules out === []
      -- requested in frame 0: armed at its end, frame 1 captured
      expectPicture m 1 === []
      word64At m (toInteger (trailerAddr 6 4)) === Just (C.unpack (trailerWord 7 False 6 4 24))
      -- the trailer is the last write, after every pixel's
      let writes = [ a | (_, _, Just (a, _, _), _) <- out ]
      last writes === trailerAddr 6 4
      -- done: idle again by the end
      let (_, _, _, busy) = last out
      busy === False
  , testCase "a second request after the first: its own sequence number, a later frame" $ do
      let gap = 3
          frameLen = picW * picH * gap + 21
          ins = frames (picW * picH) 6 gap [(1, 1), (3 * frameLen + 2, 2)]
          out = run picW picH (slaveRandom 10) 99 ins
          m = memory [ t | (_, _, t, _) <- out ]
      expectPicture m 4 @?= []
      word64At m (toInteger (trailerAddr 6 4)) @?= Just (C.unpack (trailerWord 2 False 6 4 24))
  , testCase "a DDR far too slow: pixels lost, said in the trailer, which still comes" $ do
      -- a 16 x 10 picture: 80 beats, the FIFO holds 16
      let slow = SlaveConfig 50 400 50 1 (const False)
          ins = frames 160 2 2 [(1, 9)]
          out = run 16 10 slow 5 ins
          m = memory [ t | (_, _, t, _) <- out ]
          Just tw = word64At m (toInteger (trailerAddr 16 10))
      (tw .&. 0xFF) @?= 9
      testBit tw 8 @?= True
      axiRules out @?= []
  , testCase "the trailer's fields" $ do
      let tw = C.unpack (trailerWord 0xAB True 160 144 23040) :: Word64
      (tw .&. 0xFF) @?= 0xAB
      testBit tw 8 @?= True
      ((tw `shiftR` 16) .&. 0xFFF) @?= 160
      ((tw `shiftR` 28) .&. 0xFFF) @?= 144
      (tw `shiftR` 40) @?= 23040
      trailerAddr 160 144 @?= captureBase + 92160
  , testCase "no request, no writes" $ do
      let out = run picW picH slaveFast 1 (frames (picW * picH) 3 3 [])
      [ () | (m, _, _, _) <- out, isJust (m2sAw m) || isJust (m2sW m) ] @?= []
  ]
