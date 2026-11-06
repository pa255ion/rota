module Test.Platform.Oled (tests) where

import Prelude

import qualified Clash.Explicit.Prelude as C
import qualified Clash.Prelude as H
import qualified Data.Map.Strict as M
import Test.Tasty
import Test.Tasty.HUnit

import Platform.Domains
import Platform.Sim.Oled
import Platform.Video.Oled

-- | Short delays so a simulation covers start-up and two frames quickly.
tiny :: OledTiming
tiny = OledTiming { otShort = 5, otLong = 20, otHalfBit = 2 }

-- | An asymmetric framebuffer, so byte order and bit order both show.
fbByte :: H.Index 512 -> H.BitVector 8
fbByte a = H.resize (H.pack a) * 37 + 11

-- | Pin samples, one per clock (sample 0 is in reset).
run :: OledConfig -> Int -> [OledPins]
run cfg n = C.sampleN n (H.withClockResetEnable @C.System C.clockGen C.resetGen C.enableGen dut)
 where
  dut :: H.HiddenClockResetEnable dom => H.Signal dom OledPins
  dut = pins
   where
    (addr, pins) = oledDriver tiny (pure cfg) fb
    -- one cycle of read latency, like a block RAM
    fb = H.register 0 (fbByte <$> addr)

first :: String -> [a] -> a
first what xs = case xs of
  x : _ -> x
  []    -> error ("no " <> what)

-- | Maximal runs of consecutive elements that satisfy @p@ (separators dropped).
splitRuns :: (a -> Bool) -> [a] -> [[a]]
splitRuns p xs = case break (not . p) (dropWhile (not . p) xs) of
  ([], _)   -> []
  (r, rest) -> r : splitRuns p rest

firstWhen :: (OledPins -> Bool) -> [OledPins] -> Int
firstWhen p = length . takeWhile (not . p)

initCmds :: [Integer]
initCmds = [0xAE, 0x8D, 0x14, 0xD9, 0xF1, 0x81, 0x0F, 0xDA, 0x20, 0x20, 0x00, 0xAF]

frame :: Integer -> Integer -> [(H.Bit, Integer)]
frame seg com =
  map (0,) [seg, com, 0x21, 0x00, 0x7F, 0x22, 0x00, 0x03]
    ++ [ (1, fromIntegral (fbByte a)) | a <- [minBound .. maxBound] ]

tests :: TestTree
tests = testGroup "Oled"
  [ testCase "program: the loop starts at the frame block" $ do
      oledProgram H.!! oledLoopStart @?= OpSegRemap
      H.last oledProgram @?= OpLoop

  , testCase "byte stream: init commands, then frames (command block + 512 bytes)" $ do
      let bytes = map (\b -> (sbDc b, sbValue b)) (spiBytes (run oledConfigDefault 40000))
          want  = map (0,) initCmds ++ frame 0xA1 0xC8 ++ frame 0xA1 0xC8
      take (length want) bytes @?= want

  , testCase "panel model: after two frames the display RAM is the framebuffer" $ do
      let panel = foldl panelFeed panelInit (spiBytes (run oledConfigDefault 40000))
      psUnknown panel @?= []
      (psOn panel, psHorizontal panel, psSegRemap panel, psComRemap panel) @?= (True, True, True, True)
      assertBool "two frames written" (psDataBytes panel >= 2 * 512)
      [ (pg, col) | pg <- [0 .. 3], col <- [0 .. 127]
                  , M.lookup (pg, col) (psRam panel) /= Just (fromIntegral (fbByte (fromIntegral (128 * pg + col)))) ]
        @?= []

  , testCase "OledConfig selects A0/C0" $ do
      let bytes = map sbValue (spiBytes (run (OledConfig False False) 3000))
      take 2 (drop (length initCmds) bytes) @?= [0xA0, 0xC0]

  , testCase "reset: both supplies off, RES# high, SCLK idle high" $ do
      let p = first "sample" (run oledConfigDefault 1)
      (opVdd p, opVbat p, opRes p, opSclk p) @?= (1, 1, 1, 1)

  , testCase "power sequence: VDD, RES# pulse, commands, VBAT, display on" $ do
      let ps      = run oledConfigDefault 3000
          tVdd    = firstWhen ((== 0) . opVdd) ps
          tResLo  = firstWhen ((== 0) . opRes) ps
          tResHi  = tResLo + firstWhen ((== 1) . opRes) (drop tResLo ps)
          tVbat   = firstWhen ((== 0) . opVbat) ps
          bytes   = spiBytes ps
          tFirst  = sbStart (first "byte" bytes)
          tOn     = sbStart (first "0xAF" (filter ((== 0xAF) . sbValue) bytes))
          short   = fromIntegral (otShort tiny)
          long    = fromIntegral (otLong tiny)
      assertBool "VDD on first"           (tVdd < tResLo)
      assertBool "VDD settles"            (tResLo - tVdd >= short)
      assertBool "RES# low long enough"   (tResHi - tResLo >= short)
      assertBool "no SPI before reset ends" (tFirst - tResHi >= short)
      assertBool "VBAT after the charge pump command"
        (tVbat > sbStart (first "0xF1" (filter ((== 0xF1) . sbValue) bytes)))
      assertBool "display on waits for VBAT" (tOn - tVbat >= long)
      -- once on, the supplies stay on and RES# stays high
      assertBool "VDD stays on"  (all ((== 0) . opVdd) (drop tVdd ps))
      assertBool "VBAT stays on" (all ((== 0) . opVbat) (drop tVbat ps))
      assertBool "RES# stays high" (all ((== 1) . opRes) (drop tResHi ps))

  , testCase "SCLK: every high and low phase inside a byte lasts otHalfBit clocks" $ do
      let ps     = run oledConfigDefault 6000
          sclk   = map opSclk ps
          edges  = [ t | (t, a, b) <- zip3 [0 :: Int ..] sclk (drop 1 sclk), a /= b ]
          gaps   = zipWith (-) (drop 1 edges) edges
          h      = fromIntegral (otHalfBit tiny)
      assertBool "SPI is running" (length edges > 100)
      minimum gaps @?= h
      -- A byte is 16 edges (fall, rise, ...) h apart; bytes are further apart.
      -- Drop the last run, which the end of the simulation may cut short.
      let runs = splitRuns (\g -> g == h) gaps
      init (map ((+ 1) . length) runs) @?= replicate (length runs - 1) 16

  , testCase "real timing (DomPix): >= 1 ms, >= 100 ms, SCLK <= 10 MHz" $ do
      let t  = oledTiming @DomPix
          ps = 39720 :: Integer
      assertBool "short" (toInteger (otShort t) * ps >= 1000000000)
      assertBool "long"  (toInteger (otLong t) * ps >= 100000000000)
      assertBool "sclk"  (2 * toInteger (otHalfBit t) * ps >= 100000)
  ]
