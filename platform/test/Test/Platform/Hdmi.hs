module Test.Platform.Hdmi (tests) where

import Prelude

import Data.Bits (testBit)

import qualified Clash.Prelude as C
import Test.Tasty
import Test.Tasty.HUnit

import Platform.Video.Hdmi
import Platform.Video.HdmiPacket
import Platform.Video.Tmds (TmdsIn (..), tmdsControlSymbol, tmdsDecode)
import Platform.Video.VgaTiming (Rgb888 (..), VgaCounters (..), VgaOut (..), vga640x480, vgaDecode)

cts :: C.Unsigned 20
cts = 25174

-- | Two frames of the step's output with a test pattern and a sample that
-- changes every pixel: (counters, timing, symbols).
run :: [((Int, Int), VgaOut, [C.BitVector 10])]
run = go hdmiInit 0
 where
  total = 2 * 800 * 525
  go s n
    | n >= total = []
    | otherwise =
        let x = n `mod` 800
            y = (n `div` 800) `mod` 525
            o = vgaDecode vga640x480 (VgaCounters (fromIntegral x) (fromIntegral y))
            col = Rgb888 (fromIntegral x) (fromIntegral y) (fromIntegral (x + y))
            smp = (fromIntegral (n `mod` 65536 - 32768), fromIntegral (32767 - n `mod` 65536))
            (s', syms) = hdmiStep s cts o col smp
        in ((x, y), o, C.toList syms) : go s' (n + 1)

-- | A packet read back from its 32 pixels: header and subpackets as sent.
readPacket :: [[C.BitVector 10]] -> Maybe (Integer, [Integer])
readPacket px = do
  ns <- mapM (mapM terc4Decode) px
  let ch k = map (!! k) ns
      hdr = sum [if testBit c 2 then 2 ^ i else 0 | (i, c) <- zip [0 :: Int ..] (ch 0)]
      sub k = sum [ (if testBit c1 k then 2 ^ (2 * i) else 0) + (if testBit c2 k then 2 ^ (2 * i + 1) else 0)
                  | (i, c1, c2) <- zip3 [0 :: Int ..] (ch 1) (ch 2) ]
  pure (hdr, map sub [0 .. 3])

-- | The ECC of a header or subpacket integer (data in the low bits) holds.
eccOk :: Int -> Integer -> Bool
eccOk dataBits v =
  let d = v `mod` (2 ^ dataBits)
      e = v `div` (2 ^ dataBits)
  in case dataBits of
       24 -> fromIntegral e == ecc (fromIntegral d :: C.BitVector 24)
       _ -> fromIntegral e == ecc (fromIntegral d :: C.BitVector 56)

byteAt :: Integer -> Int -> Integer
byteAt v i = (v `div` (256 ^ i)) `mod` 256

tests :: TestTree
tests = testGroup "Video.Hdmi"
  [ testCase "active pixels: every pixel decodes to its colour" $ do
      let bad = [ (x, y) | ((x, y), o, [d0, d1, d2]) <- run, vgaActive o
                , let col = (fromIntegral x, fromIntegral y, fromIntegral (x + y)) :: (C.BitVector 8, C.BitVector 8, C.BitVector 8)
                , (tmdsDecode d2, tmdsDecode d1, tmdsDecode d0) /= (TmdsData (fst3 col), TmdsData (snd3 col), TmdsData (thd3 col)) ]
      take 3 bad @?= []

  , testCase "the slots of a line: preambles, guard bands, packets where the table says; syncs on channel 0" $ do
      let line = [ (x, s) | ((x, y), _, s) <- run, y == 10 ]
          at x = head [ s | (x', s) <- line, x' == x ]
          o x = vgaDecode vga640x480 (VgaCounters (fromIntegral x) 10)
      at 655 @?= [tmdsControlSymbol (vgaVSync (o 655)) (vgaHSync (o 655)), tmdsControlSymbol 0 1, tmdsControlSymbol 0 1]
      at 660 @?= C.toList (islandGuard (vgaHSync (o 660)) (vgaVSync (o 660)))
      at 727 @?= C.toList (islandGuard (vgaHSync (o 727)) (vgaVSync (o 727)))
      at 795 @?= [tmdsControlSymbol (vgaVSync (o 795)) (vgaHSync (o 795)), tmdsControlSymbol 0 1, tmdsControlSymbol 0 0]
      at 799 @?= C.toList videoGuard
      -- no video preamble before a blank line
      let line479 = [ s | ((x, y), _, s) <- run, y == 479, x == 799 ]
      head line479 @?= [tmdsControlSymbol 1 1, tmdsControlSymbol 0 0, tmdsControlSymbol 0 0]

  , testCase "packets: ECC holds; A is Audio Sample, B the AVI InfoFrame (line 0), Audio InfoFrame (1), ACR (else)" $ do
      let frame2 = [ r | r@((_, _), _, _) <- drop (800 * 525) run ]
          pkt y k = readPacket [ s | ((x, y'), _, s) <- frame2, y' == y, x >= 662 + 32 * k, x < 694 + 32 * k ]
          check y = do
            let Just (ha, sa) = pkt y 0
                Just (hb, sb) = pkt y 1
            assertBool ("ECC A " ++ show y) (eccOk 24 ha && all (eccOk 56) sa)
            assertBool ("ECC B " ++ show y) (eccOk 24 hb && all (eccOk 56) sb)
            byteAt ha 0 @?= 0x02
            byteAt hb 0 @?= (if y == 0 then 0x82 else if y == 1 then 0x84 else 0x01)
            if y > 1
              then do
                -- ACR: CTS in SB1..SB3, N in SB4..SB6, big end first
                let s0 = head sb
                (byteAt s0 2 * 256 + byteAt s0 3) @?= 25174
                (byteAt s0 5 * 256 + byteAt s0 6) @?= 6144
              else pure ()
      mapM_ check [0, 1, 2, 100, 479, 500, 524]

  , testCase "samples: about 800.8 a frame (6144 / (128 x 25174) a pixel), B every 192 frames" $ do
      let frame2 = drop (800 * 525) run
          lines' = [0 .. 524 :: Int]
          headerOf y = fmap fst (readPacket [ s | ((x, y'), _, s) <- frame2, y' == y, x >= 662, x < 694 ])
          present = [ length (filter (testBit (byteAt h 1)) [0 .. 3]) | y <- lines', Just h <- [headerOf y] ]
          blocks = length [ () | y <- lines', Just h <- [headerOf y], byteAt h 2 `div` 16 /= 0 ]
          n = sum present
      assertBool (show n) (n >= 799 && n <= 802)
      assertBool (show present) (all (<= 2) present)
      assertBool (show blocks) (blocks >= 4 && blocks <= 5)

  , testCase "syncs: HSYNC and VSYNC readable from channel 0 in every period" $ do
      let readSync s = case tmdsDecode s of
            TmdsControl v h -> Just (h, v)
            TmdsData _ -> fmap (\d -> (if testBit d 0 then 1 else 0, if testBit d 1 then 1 else 0)) (terc4Decode s)
          -- (the video guard band, 798 and 799, carries no syncs)
          bad = [ (x, y) | ((x, y), o, s : _) <- run, not (vgaActive o), x < 798
                , readSync s /= Just (vgaHSync o, vgaVSync o) ]
      take 3 bad @?= []
  ]
 where
  fst3 (a, _, _) = a
  snd3 (_, b, _) = b
  thd3 (_, _, c) = c
