-- | M0e's DDR test ("M0e.Tester" with its read port) against the AXI
-- slave model (Platform.Mem.Sim), driven by PS link writes as the PS app
-- makes them: what it reads, the checksum, the latency histogram, the
-- result bytes read back over the link, and the layer 4 golden (the
-- result bytes of test/ps/ddr.ps, which make sim SYSTEM=m0e checks).
module Main (main) where

import Prelude
import Data.Bits (complement, xor, (.&.))
import Data.Word (Word64)
import Numeric (readHex, showHex)

import qualified Clash.Prelude as C
import Test.Tasty
import Test.Tasty.HUnit

import M0e.Tester
import Platform.Mem.Axi
import Platform.Mem.Sim
import Platform.Ps.Link (LinkAddr, PsByteOut (..))

main :: IO ()
main = defaultMain tests

-- | The memory the slave models stand for, here and in the Verilator
-- harness (platform/sim/axi.zig, memWord): a word made from its address.
memWord :: AxiAddr -> AxiData
memWord a = C.pack (C.resize a * 0x9E3779B97F4A7C15 `xor` 0x0123456789ABCDEF :: C.Unsigned 64)

-- | Link writes as the PS sends them: a few clocks apart.
writes :: [(LinkAddr, C.BitVector 8)] -> [Maybe (LinkAddr, C.BitVector 8)]
writes = concatMap (\w -> [Just w, Nothing, Nothing, Nothing])

le32 :: LinkAddr -> C.Unsigned 32 -> [(LinkAddr, C.BitVector 8)]
le32 a v = [ (a + i, C.resize (C.pack (v `C.shiftR` (8 * fromIntegral i)))) | i <- [0 .. 3] ]

setup :: C.Unsigned 32 -> C.Unsigned 32 -> C.BitVector 8 -> C.Unsigned 32 -> [(LinkAddr, C.BitVector 8)]
setup base n mode mask = le32 0 base ++ le32 4 n ++ [(8, mode)] ++ le32 0xC mask ++ [(0x10, 1)]

-- | Run the tester for @n@ clocks with these link writes (then none);
-- the byte channel, the results and the AXI error flag per clock.
run :: SlaveConfig -> Word64 -> [Maybe (LinkAddr, C.BitVector 8)] -> Int -> [(PsByteOut, Results, Bool)]
run sc seed ws n =
  C.sampleN_lazy n $ C.withClockResetEnable C.systemClockGen C.resetGen C.enableGen $
    let link = C.fromList_lazy (Nothing : Nothing : ws ++ repeat Nothing)
        s2m = axiSlave sc seed (pure memWord) m2s
        (byte, m2s, rs) = tester link s2m
    in (\b (r, e) -> (b, r, e)) <$> byte <*> rs

finalResults :: [(PsByteOut, Results, Bool)] -> (Results, Bool)
finalResults xs = let (_, r, e) = last xs in (r, e)

-- | The bytes of the byte channel, one per toggle (as the PS reads them).
channel :: [(PsByteOut, Results, Bool)] -> [C.BitVector 8]
channel xs = [ pbByte b | ((b, _, _), (p, _, _)) <- zip (drop 1 xs) xs, pbToggle b /= pbToggle p ]

-- | What the PS expects: the checksum of the words at these addresses.
expectSum :: [AxiAddr] -> C.BitVector 32
expectSum = foldl (\c a -> checksumStep c (memWord (a .&. complement 7))) 0

seqAddrs :: AxiAddr -> Int -> Int -> [AxiAddr]
seqAddrs base stride n = [ base + fromIntegral (stride * i) | i <- [0 .. n - 1] ]

randAddrs :: AxiAddr -> C.Unsigned 32 -> Int -> [AxiAddr]
randAddrs base mask n = [ base + (C.unpack x .&. mask .&. complement 7) | x <- take n (iterate xorshift32 rngSeed) ]

-- | The link writes of a PS script's @bytes@ and @wait@ lines
-- (platform/sim/ps.zig), 25 clocks per microsecond.
scriptWrites :: String -> [Maybe (LinkAddr, C.BitVector 8)]
scriptWrites = concatMap line . lines
 where
  line l = case words l of
    ["bytes", a, hs] -> writes (zip [num a ..] (pairs hs))
    ["wait", us] -> replicate (25 * read us) Nothing
    _ -> []
  num s = case s of
    '0' : 'x' : h -> hex h
    _ -> read s
  pairs (x : y : r) = fromInteger (hex [x, y]) : pairs r
  pairs _ = []
  hex :: (Eq a, Num a) => String -> a
  hex h = case readHex h of
    [(v, "")] -> v
    _ -> error ("not hex: " <> h)

hexBytes :: [C.BitVector 8] -> String
hexBytes = unwords . map (\b -> let s = showHex (toInteger b) "" in if length s < 2 then '0' : s else s)

le32Of :: [C.BitVector 8] -> C.BitVector 32
le32Of bs = foldr (\b acc -> acc `C.shiftL` 8 C..|. C.resize b) 0 bs

tests :: TestTree
tests = testGroup "m0e"
  [ testCase "sequential words: the checksum, the count, every latency in the histogram" $ do
      let n = 300
          xs = run (slaveRandom 12) 1 (writes (setup 0x01000000 (fromIntegral n) 0 0)) 6000
          (r, e) = finalResults xs
      rsDone r @? "done"
      rsRunning r @?= False
      e @?= False
      rsCount r @?= fromIntegral n
      rsSum r @?= expectSum (seqAddrs 0x01000000 8 n)
      sum (C.toList (rsHist r)) @?= fromIntegral n
      rsMin r @?= 1
      assertBool "some misses" (rsMax r > 2)
  , testCase "stride 64: no word shares a line, so nothing hits" $ do
      let n = 100
          xs = run (slaveRandom 12) 2 (writes (setup 0x01000000 (fromIntegral n) 3 0)) 6000
          (r, _) = finalResults xs
      rsSum r @?= expectSum (seqAddrs 0x01000000 64 n)
      assertBool ("shortest " <> show (rsMin r)) (rsMin r >= 3)
      C.toList (rsHist r) !! 1 @?= 0
  , testCase "random addresses: the PS's own xorshift32 sequence" $ do
      let n = 200
          xs = run (slaveRandom 12) 3 (writes (setup 0x01000000 (fromIntegral n) 0x80 0xFFFF)) 8000
          (r, _) = finalResults xs
      rsCount r @?= fromIntegral n
      rsSum r @?= expectSum (randAddrs 0x01000000 0xFFFF n)
  , testCase "the result bytes, selected over the link, one per toggle of the byte channel" $ do
      let ws = writes (setup 0x01000000 50 0 0)
          sel = writes [ (resultsBase + i, 0) | i <- [0 .. 31] ]
          xs = run slaveFast 4 (ws ++ replicate 1500 Nothing ++ sel) 2000
          (r, e) = finalResults xs
          got = channel xs
      got @?= [ resultByte e r (fromIntegral i) | i <- [0 .. 31 :: Int] ]
      take 4 got @?= [0x02, 0, 0, 0]
      got !! 16 @?= 50
  , testCase "a run of 0 words is done at once" $ do
      let (r, _) = finalResults (run slaveFast 5 (writes (setup 0x01000000 0 0 0)) 200)
      (rsDone r, rsRunning r, rsCount r) @?= (True, False, 0)
  , testCase "layer 4 golden: test/ps/ddr.ps reads these bytes (test/golden/ps_bytes.hex)" $ do
      script <- readFile "test/ps/ddr.ps"
      golden <- filter (not . null) . lines <$> readFile "test/golden/ps_bytes.hex"
      let xs = run (slaveRandom 20) 6 (scriptWrites script) (length (scriptWrites script) + 100)
          got = channel xs
      [hexBytes got] @?= map (unwords . words) golden
      -- and the golden against the independent reference: per run the
      -- status (done), the checksum, the count
      let run1 = take 9 got
          run2 = take 9 (drop 9 got)
      (run1 !! 0, le32Of (take 4 (drop 1 run1)), le32Of (drop 5 run1))
        @?= (0x02, expectSum (seqAddrs 0x01000000 8 512), 512)
      (run2 !! 0, le32Of (take 4 (drop 1 run2)), le32Of (drop 5 run2))
        @?= (0x02, expectSum (randAddrs 0x01000000 0xFFFFF 256), 256)
  , testCase "latency buckets: exact to 31, then powers of two" $ do
      map bucket [0, 1, 31, 32, 63, 64, 127, 128, 32767, 32768, 65535] @?= [0, 1, 31, 32, 32, 33, 33, 34, 41, 42, 42]
  , testCase "checksumStep: rotate, then both halves" $
      checksumStep 0x80000001 0x0000000F000000F0 @?= (0x00000003 `xor` 0xF0 `xor` 0x0F)
  ]
