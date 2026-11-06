module Test.Platform.Tmds (tests) where

import Prelude

import Data.Bits (popCount, testBit, (.&.))
import Data.List (nub)
import qualified Data.Set as Set

import qualified Clash.Prelude as C
import Test.Tasty
import Test.Tasty.HUnit

import Platform.Domains
import Platform.Video.Tmds
import Platform.Video.VgaTiming (Rgb888 (..), VgaCounters (..), vga640x480, vgaDecode)

-- * Reference, written from DVI 1.0, section 3.2.2 (figure 3-5)
--
-- Plain lists of Bool, bit 0 first, and Int counters; nothing shared with
-- Platform.Video.Tmds.

-- | One step: cnt(t-1) and either the byte D or the control bits (C1, C0);
-- cnt(t) and q_out[0..9].
refEncode :: Int -> Either (Bool, Bool) [Bool] -> (Int, [Bool])
refEncode _ (Left (c1, c0)) = (0, ctrl c1 c0)
 where
  -- "q_out[9:0] = 0b1101010100" etc.: written MSB first, reversed here.
  ctrl False False = reverse (bits "1101010100")
  ctrl False True  = reverse (bits "0010101011")
  ctrl True  False = reverse (bits "0101010100")
  ctrl True  True  = reverse (bits "1010101011")
  bits = map (== '1')
refEncode cnt (Right d) = (cnt', qOut)
 where
  n1d = length (filter id d)
  xnorMode = n1d > 4 || (n1d == 4 && not (head d))
  op a b = if xnorMode then a == b else a /= b
  qm07 = scanl1 (\prev di -> op prev di) d   -- q_m[0] = D[0]; q_m[i] = q_m[i-1] op D[i]
  qm8 = not xnorMode
  n1 = length (filter id qm07)
  n0 = 8 - n1
  b2i b = if b then 1 else 0
  (qOut, cnt')
    | cnt == 0 || n1 == n0 =
        ( (if qm8 then qm07 else map not qm07) ++ [qm8, not qm8]
        , if not qm8 then cnt + (n0 - n1) else cnt + (n1 - n0) )
    | (cnt > 0 && n1 > n0) || (cnt < 0 && n0 > n1) =
        ( map not qm07 ++ [qm8, True]
        , cnt + 2 * b2i qm8 + (n0 - n1) )
    | otherwise =
        ( qm07 ++ [qm8, False]
        , cnt - 2 * b2i (not qm8) + (n1 - n0) )

byteBits :: Int -> [Bool]
byteBits x = [testBit x i | i <- [0 .. 7]]

symBits :: C.BitVector 10 -> [Bool]
symBits s = [testBit s i | i <- [0 .. 9]]

-- | Every disparity the encoder can reach from 0 (after reset or a
-- control period) with any sequence of bytes.
reachable :: [Int]
reachable = Set.toList (go (Set.singleton 0) [0])
 where
  go seen [] = seen
  go seen (c : rest) =
    let next = [fst (refEncode c (Right (byteBits x))) | x <- [0 .. 255]]
        new = filter (`Set.notMember` seen) (nub next)
    in go (foldr Set.insert seen new) (rest ++ new)

tests :: TestTree
tests = testGroup "Video.Tmds"
  [ testCase "reachable disparities: even, within -8 .. 8, so Signed 6 holds them" $ do
      assertBool (show reachable) (all even reachable)
      assertBool (show reachable) (all (\c -> c >= -8 && c <= 8) reachable)
      minimum reachable @?= -8
      maximum reachable @?= 8

  , testCase "tmdsEncode = the reference, every byte from every reachable disparity" $
      sequence_
        [ let (cntR, qR) = refEncode c (Right (byteBits x))
              (cntC, qC) = tmdsEncode (fromIntegral c) (TmdsData (fromIntegral x))
          in assertEqual ("byte " ++ show x ++ ", cnt " ++ show c) (cntR, qR) (fromIntegral cntC, symBits qC)
        | c <- reachable, x <- [0 .. 255 :: Int] ]

  , testCase "control symbols exactly as DVI 1.0 writes them, disparity back to 0" $ do
      map (uncurry tmdsControlSymbol) [(0, 0), (0, 1), (1, 0), (1, 1)]
        @?= [0b1101010100, 0b0010101011, 0b0101010100, 0b1010101011]
      sequence_
        [ do let (cnt', q) = tmdsEncode c (TmdsControl c1 c0)
             cnt' @?= 0
             symBits q @?= snd (refEncode 0 (Left (c1 == 1, c0 == 1)))
        | c <- [-8, 0, 6], c1 <- [0, 1], c0 <- [0, 1] ]

  , testCase "round trip: tmdsDecode recovers every byte from every disparity, and every control" $ do
      sequence_
        [ tmdsDecode (snd (tmdsEncode (fromIntegral c) (TmdsData (fromIntegral x))))
            @?= TmdsData (fromIntegral x)
        | c <- reachable, x <- [0 .. 255 :: Int] ]
      sequence_ [tmdsDecode (tmdsControlSymbol c1 c0) @?= TmdsControl c1 c0 | c1 <- [0, 1], c0 <- [0, 1]]

  , testCase "data symbols never look like control symbols (460 distinct data symbols)" $ do
      let ds = nub [snd (tmdsEncode (fromIntegral c) (TmdsData (fromIntegral x))) | c <- reachable, x <- [0 .. 255 :: Int]]
          cs = [tmdsControlSymbol c1 c0 | c1 <- [0, 1], c0 <- [0, 1]]
      filter (`elem` cs) ds @?= []
      length ds @?= 460

  , testCase "a long pseudo-random byte stream stays DC-balanced: ones minus zeros sent stays within +-18" $ do
      let xs = take 20000 (iterate (\v -> (v * 1103515245 + 12345) `mod` 2147483648) (7 :: Int))
          bytes = map (\v -> (v `div` 65536) .&. 255) xs
          syms = snd (foldl (\(c, acc) b -> let (c', q) = tmdsEncode c (TmdsData (fromIntegral b)) in (c', q : acc)) (0, []) bytes)
          bal = scanl (\a q -> a + 2 * popCount q - 10) (0 :: Int) (reverse syms)
      assertBool (show (minimum bal, maximum bal)) (all (\b -> abs b <= 18) bal)

  , testCase "tmdsChannels: B, G, R in the visible area; syncs as (C1, C0) = (VSYNC, HSYNC) on channel 0 outside it" $ do
      let at x y = vgaDecode vga640x480 (VgaCounters x y)
          c = Rgb888 0x12 0x34 0x56
      C.toList (tmdsChannels (at 5 7) c) @?= [TmdsData 0x56, TmdsData 0x34, TmdsData 0x12]
      -- (700, 10): blanking, HSYNC pulse (656 .. 751, active low), VSYNC high
      C.toList (tmdsChannels (at 700 10) c) @?= [TmdsControl 1 0, TmdsControl 0 0, TmdsControl 0 0]
      -- (10, 491): VSYNC pulse (490, 491), HSYNC high
      C.toList (tmdsChannels (at 10 491) c) @?= [TmdsControl 0 1, TmdsControl 0 0, TmdsControl 0 0]

  , testCase "tmdsEncoder: registered (control 00 in reset), the reference's disparity carried across symbols" $ do
      let ins = [TmdsControl 0 0, TmdsData 0x00, TmdsData 0xFF, TmdsData 0x10, TmdsData 0xF0, TmdsControl 1 1, TmdsData 0x55, TmdsData 0x0F]
          got = C.sampleN (length ins + 2) (C.withClockResetEnable (C.clockGen @DomPix) C.resetGen C.enableGen
                  (tmdsEncoder (C.fromList (head ins : ins ++ repeat (last ins)))))
          refIn (TmdsControl c1 c0) = Left (c1 == 1, c0 == 1)
          refIn (TmdsData d) = Right (byteBits (fromIntegral d))
          want = snd (foldl (\(c, acc) i -> let (c', q) = refEncode c (refIn i) in (c', acc ++ [q])) (0, []) ins)
      -- The input stream repeats ins!!0 in reset's clock (sample 0). The
      -- output is the register: control 00 in samples 0 and 1 (the reset
      -- value, and the state reset left), then input k's symbol at
      -- sample k + 2, one clock after it was taken.
      map symBits (take 2 got) @?= replicate 2 (symBits (tmdsControlSymbol 0 0))
      map symBits (drop 2 got) @?= want

  , testCase "reference sanity: 0x00 from disparity 0 is 0100000000" $
      -- N1(D) = 0, so the XOR chain: q_m = 0x00, q_m[8] = 1; cnt = 0, so
      -- q_out[9] = ~q_m[8] = 0, q_out[8] = 1, q_out[7:0] = q_m.
      reverse (snd (refEncode 0 (Right (byteBits 0)))) @?= map (== '1') "0100000000"
  ]
