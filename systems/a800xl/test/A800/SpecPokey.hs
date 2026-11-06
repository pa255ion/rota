-- | Step 4.4: POKEY alone, cycle by cycle, against the HRM (5.3, figure 8)
-- and Acid800's measured timings.
module A800.SpecPokey (pokeyTests, pokeyRun) where

import Prelude

import Control.Monad (forM, forM_)
import Control.Monad.ST (runST)
import Data.Bits (testBit)
import Data.Word
import Test.Tasty
import Test.Tasty.HUnit

import A800.Spec (ScreenRun (..), runTone, runTyping, stockConfig, xexConfig)
import A800.Spec.Pokey
import A800.TestRoms
import System.FilePath ((</>))

-- | Run POKEY for @n@ cycles from power-on; the writes happen in their
-- cycle (after its tick), as a CPU's do. Returns IRQST after each cycle's
-- tick (what a read in that cycle sees), cycles 1..n.
pokeyRun :: Int -> [(Int, Word16, Word8)] -> [Word8]
pokeyRun n writes = runST $ do
  p <- newPokey
  forM [1 .. n] $ \t -> do
    pokeyTick p
    v <- pokeyRead p 0xD20E
    forM_ [ (r, x) | (wt, r, x) <- writes, wt == t ] $ \(r, x) -> pokeyWrite p r x
    pure v

-- | First cycle from @from@ on where IRQST bit @b@ reads 0.
firstSet :: Int -> Int -> [Word8] -> Maybe Int
firstSet b from vs = case [ t | (t, v) <- zip [1 ..] vs, t >= from, not (testBit v b) ] of
  t : _ -> Just t
  [] -> Nothing

pokeyTests :: TestTree
pokeyTests = testGroup "POKEY (4.4)"
  [   testCase "async receive: $A5 sent at 600 baud ($05CC) comes in SERIN, IRQST bit 5, SKSTAT busy then idle; a bad stop bit marks framing (HRM 5.6, figure 10)" $ do
      let (serin, irq, busy, idle, frame) = asyncByte 0xA5 True
      (serin, irq, busy, idle, frame) @?= (0xA5, True, True, True, False)
      let (serin', _, _, _, frame') = asyncByte 0x3C False
      (serin', frame') @?= (0x3C, True)
  , testCase "timer 1 at 1.79 MHz, AUDF1=0: IRQST at STIMER+8, +12, +16 (HRM figure 8)" $ do
      let base = [(1, 0xD20F, 3), (2, 0xD208, 0x40), (3, 0xD200, 0), (10, 0xD209, 0), (14, 0xD20E, 1)]
          vs = pokeyRun 60 base
      firstSet 0 11 vs @?= Just 18
  , testCase "AUDF1=16: IRQST at STIMER+24 (Acid800 pokey_timertiming)" $ do
      let vs = pokeyRun 80 [(1, 0xD20F, 3), (2, 0xD208, 0x40), (3, 0xD200, 16), (10, 0xD209, 0), (14, 0xD20E, 1)]
      firstSet 0 11 vs @?= Just 34
  , testCase "two-tone, AUDF1=8, mark: IRQST at STIMER+16, then +30 (two cycles longer: the resync)" $ do
      -- a byte through the serial port first leaves the line at 1 (the
      -- stop bit), as Acid800 does
      let prep = [(1, 0xD20F, 0x23), (2, 0xD208, 0x28), (3, 0xD204, 0), (4, 0xD206, 0), (5, 0xD20D, 0)]
          t0 = 1000
          two = [(t0 - 40, 0xD20F, 0), (t0 - 30, 0xD208, 0x40), (t0 - 20, 0xD20F, 0x0B), (t0 - 30, 0xD200, 0xFF), (t0 - 14, 0xD202, 0xFF), (t0 - 2, 0xD200, 8)
                , (t0, 0xD209, 0), (t0 + 4, 0xD20E, 0xFF), (t0 + 19, 0xD20E, 0), (t0 + 23, 0xD20E, 0xFF)]
          vs = pokeyRun (t0 + 40) (prep <> two)
      firstSet 0 (t0 + 1) vs @?= Just (t0 + 16)
      firstSet 0 (t0 + 24) vs @?= Just (t0 + 30)
  , testCase "tone.s: 437.8 Hz on the spec (AUDF1=72, 64 kHz clock)" $ do
      os <- loadFile osRom
      basic <- loadFile basicRom
      l <- loadFile loaderBin
      x <- loadFile (asmRoot </> "tone.xex")
      let f = runTone 9000000 900000 (xexConfig os basic l x)
      assertBool ("tone at " <> show f <> " Hz") (abs (f - 437.8) < 0.2)
  , testCase "the keyboard: PRINT 2+2 typed into BASIC prints 4" $ do
      os <- loadFile osRom
      basic <- loadFile basicRom
      case runTyping "Ready" "PRINT 2+2\n" (stockConfig os basic Nothing) of
        Left e -> assertFailure e
        Right r -> take 4 (drop 2 (srLines r)) @?= ["  PRINT 2+2", "  4", "", "  Ready"]
  ]

-- | One byte sent into POKEY's serial input at 600 baud in asynchronous
-- receive mode (SKCTL $13, timers 3+4 linked at $05CC, IRQEN bit 5), its
-- stop bit 1 or not: SERIN after it, whether IRQST bit 5 went to 0,
-- whether SKSTAT bit 1 read busy (0) mid-byte and idle (1) after it, and
-- SKSTAT bit 7's framing error.
asyncByte :: Word8 -> Bool -> (Word8, Bool, Bool, Bool, Bool)
asyncByte v stop = runST $ do
  p <- newPokey
  let w r x = pokeyWrite p r x
      bitCycles = 1789773 `div` 600
      start = 400
      levels = [False] <> [ testBit v i | i <- [0 .. 7] ] <> [stop] <> repeat True
      lineAt t = if t < start then True else levels !! ((t - start) `div` bitCycles)
  pokeyTick p >> w 0xD20F 0x03
  pokeyTick p >> w 0xD208 0x28
  pokeyTick p >> w 0xD204 0xCC
  pokeyTick p >> w 0xD206 0x05
  pokeyTick p >> w 0xD20E 0x20
  -- (SKRES: SKSTAT's error bits read 0 at power-on, $7F)
  pokeyTick p >> w 0xD20A 0
  pokeyTick p >> w 0xD20F 0x13
  rs <- forM [8 .. start + 12 * bitCycles] $ \t -> do
    pokeySetSerialIn p (lineAt t)
    pokeyTick p
    sk <- pokeyRead p 0xD20F
    pure (t, sk)
  serin <- pokeyRead p 0xD20D
  irqst <- pokeyRead p 0xD20E
  sk <- pokeyRead p 0xD20F
  let mid = [ s' | (t, s') <- rs, t == start + 5 * bitCycles ]
  pure ( serin, not (testBit irqst 5), any (not . (`testBit` 1)) mid, testBit sk 1, not (testBit sk 7) )
