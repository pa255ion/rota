module Main (main) where

import Prelude

import Data.Bits (xor, (.&.))
import qualified Clash.Explicit.Prelude as C
import qualified Clash.Prelude as H
import Test.Tasty
import Test.Tasty.Golden.Advanced (goldenTest)
import Test.Tasty.HUnit

import M0c.Pattern
import M0c.Top (m0cOled, topEntity)
import Platform.Sim.Frame (frameDiff, readFrame, writeFrame)
import Platform.Sim.Oled
import Platform.Video.Oled

main :: IO ()
main = defaultMain $ testGroup "m0c"
  [ testGroup "pattern"
    [ testCase "exhaustive: every byte of every bar position against the layout" $
        [ (bar, a) | bar <- [0 .. 63], a <- [0 .. 511]
                   , patternByte bar a /= referenceByte bar a ] @?= []
    ]

  , testGroup "m0cOled (driver + pattern, short timing)"
    [ testCase "panel RAM after the first frame is the pattern with the bar at 0" $ do
        let p = panelAfter 0
        psUnknown p @?= []
        psOn p @?= True
        [ (x, y) | x <- [0 .. 127], y <- [0 .. 31]
                 , panelPixel p x y /= reference 0 x y ] @?= []
    , goldenTest "panel image matches test/golden/oled_bar0.png (--accept to update)"
        (either error id <$> readFrame golden)
        (pure (panelImage 4 (panelAfter 0)))
        (\g t -> pure (frameDiff g t))
        (writeFrame golden)
    , testCase "SW0 / SW1 switch segment remap / COM scan direction" $ do
        let flags sw = let p = panelAfter sw in (psSegRemap p, psComRemap p)
        map flags [0, 1, 2, 3] @?= [(True, True), (False, True), (True, False), (False, False)]
    ]

  , testGroup "topEntity (through the clock wizard model, real timing)"
    [ testCase "port order: LD, OLED_DC/RES/SCLK/SDIN/VBAT/VDD" $ do
        let n    = 80000
            outs = C.sampleN n (H.bundle (topEntity C.clockGen
                     (pure 0) (pure 0) (pure 0) (pure 0) (pure 0) (pure 0x81)))
            pins = [ OledPins dc res sclk sdin vbat vdd | (_, dc, res, sclk, sdin, vbat, vdd) <- outs ]
            lds  = [ ld | (ld, _, _, _, _, _, _) <- outs ]
        last lds .&. 0x7F @?= 0x01
        -- VDD on within a few clocks; RES# low after 1 ms (25177 clocks);
        -- the first byte (display off) at about 3 ms; VBAT on after the
        -- first five command bytes.
        opVdd (pins !! 100) @?= 0
        opRes (pins !! 20000) @?= 1
        opRes (pins !! 30000) @?= 0
        assertBool "VBAT off at first" (all ((== 1) . opVbat) (take 75000 pins))
        opVbat (last pins) @?= 0
        take 1 (map (\b -> (sbDc b, sbValue b)) (spiBytes pins)) @?= [(0, 0xAE)]
    ]
  ]
 where
  golden = "test/golden/oled_bar0.png"

-- | Short start-up delays; the bar never moves within the simulation.
tiny :: OledTiming
tiny = OledTiming { otShort = 5, otLong = 20, otHalfBit = 2 }

-- | The panel model after the first full frame, with switches @sw@.
panelAfter :: H.BitVector 8 -> Panel
panelAfter sw = foldl panelFeed panelInit (takeFrame (spiBytes pins))
 where
  pins = C.sampleN 25000 (H.withClockResetEnable @C.System C.clockGen C.resetGen C.enableGen
           (m0cOled tiny (H.SNat @1000000) (pure sw)))
  -- everything up to and including the first 512 data bytes
  takeFrame bs = let (pre, rest) = break ((== 1) . sbDc) bs
                 in pre ++ take 512 rest

-- | Independent restatement of the layout in "M0c.Pattern".
reference :: Int -> Int -> Int -> Bool
reference bar x y =
     x == 0 || x == 127 || y == 0 || y == 31
  || (x >= 2 && x <= 9 && y >= 2 && y <= 9)
  || (x >= 16 && x <= 47 && y >= 4 && y <= 27 && odd ((x `div` 4) `xor` (y `div` 4)))
  || (x >= 56 + bar && x < 60 + bar && y >= 2 && y <= 29)

referenceByte :: BarPos -> H.Index 512 -> H.BitVector 8
referenceByte bar a =
  sum [ if reference (fromIntegral bar) col (8 * page + b) then 2 ^ b else 0 | b <- [0 .. 7] ]
 where
  (page, col) = fromIntegral a `divMod` 128
