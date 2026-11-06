module Main (main) where

import Prelude
import Codec.Picture (generateImage)
import qualified Data.ByteString.Char8 as BC
import Test.Tasty
import Test.Tasty.Golden.Advanced (goldenTest)
import Test.Tasty.HUnit

import M0b.Pattern (squareAt, testPattern)
import M0d.Top (m0dLeds)
import Platform.Bus.I2c (I2cError (..))
import Platform.Sim.Adv7511 (expectedWrites, hdmiFrame, registerImage, writesLog)
import Platform.Sim.Frame
import Platform.Video.Adv7511 (AdvStatus (..))
import Platform.Video.VgaTiming (Rgb444, fromRgb444)

-- | The frame make sim compares (systems/m0d/zedboard.mk: FRAMES = 18).
-- Decoded frame k is generated frame k + 1 (docs/TESTING.md).
lastFrame :: Int
lastFrame = 17

goldenVga, goldenHdmi, goldenI2c :: FilePath
goldenVga  = "test/golden/frame_0017.png"
goldenHdmi = "test/golden/hdmi_0017.png"
goldenI2c  = "test/golden/i2c_writes.txt"

pattern :: Int -> Int -> Int -> PixelColour
pattern k x y = testPattern (squareAt (k + 1)) (fromIntegral x) (fromIntegral y)

type PixelColour = Rgb444

-- | The I2C log in the harness's format (platform/sim/main.zig).
i2cLog :: BC.ByteString
i2cLog = BC.pack (writesLog expectedWrites)

frameGolden :: String -> FilePath -> Frame -> TestTree
frameGolden name path f = goldenTest name
  (either error id <$> readFrame path) (pure f) (\g t -> pure (frameDiff g t)) (writeFrame path)

main :: IO ()
main = defaultMain $ testGroup "m0d"
  [ testCase "LEDs, SW0 off: heartbeat on LD7, status bits on LD6..LD0" $ do
      m0dLeds False True (AdvStatus True True False Nothing 0x14 1) @?= 0xC3
      m0dLeds False False (AdvStatus False False False (Just I2cTimeout) 0x14 0) @?= 0x38
      m0dLeds False False (AdvStatus False True True (Just NackAddr) 0 2) @?= 0x0E
  , testCase "LEDs, SW0 on: chip revision" $
      m0dLeds True True (AdvStatus True True True Nothing 0x14 3) @?= 0x14
  , testGroup "layer 4 goldens (make sim SYSTEM=m0d; --accept to update)"
    [ frameGolden "VGA frame 17 = the M0b pattern of frame 18" goldenVga
        (generateImage (\x y -> rgbPixel (pattern lastFrame x y)) 640 480)
    , frameGolden "HDMI frame 17 = that pattern through Ycbcr422 and the ADV7511 model" goldenHdmi
        (hdmiFrame (registerImage expectedWrites) 640 480 (\x y -> fromRgb444 (pattern lastFrame x y)))
    , goldenTest "I2C writes = one pass through the register table"
        (BC.readFile goldenI2c) (pure i2cLog)
        (\g t -> pure (if g == t then Nothing else Just "I2C log differs"))
        (BC.writeFile goldenI2c)
    ]
  ]
