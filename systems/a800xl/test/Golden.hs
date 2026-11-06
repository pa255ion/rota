-- | Layer 3 (M4 step 4.7): the VGA and HDMI goldens of layer 4, derived
-- from the frozen spec pictures (320 × 240 ×2, filling 640 × 480; VGA
-- shows the top 4 bits of each channel, HDMI all 8), the I2C log, and the
-- display path in Clash simulation against them.
module Main (main) where

import Prelude

import Codec.Picture (PixelRGB8 (..), generateImage, pixelAt)
import qualified Data.ByteString.Char8 as BC
import qualified Data.Vector.Unboxed as U
import Data.Word
import System.FilePath ((</>), (<.>))
import Test.Tasty
import Test.Tasty.Golden.Advanced (goldenTest)
import Test.Tasty.HUnit

import qualified Clash.Prelude as C
import A800.Display
import A800.Picture (Picture, pictureFrame, picturePixels, pictureWidth, pictureHeight)
import A800.Spec (runPicture, stockConfig)
import A800.TestRoms
import Platform.Sim.Adv7511 (expectedWritesFor, hdmiFrame, registerImage, writesLog)
import Platform.Sim.Frame
import Platform.Video.Adv7511.Table (adv7511HdmiAudioTable)
import Platform.Video.VgaTiming

-- | The layer 4 runs (systems/a800xl/zedboard.mk, Makefile ci-a800xl):
-- golden name; each is the spec golden of the same name.
runs :: [String]
runs = ["ready", "scene", "rasterbars", "keys", "xegs_banks", "cas_boot", "cassio"]

main :: IO ()
main = defaultMain $ testGroup "a800xl-golden"
  [ testGroup "VGA goldens (test/golden/vga/, from the spec goldens)"
      [ frameGolden (vgaPath g) (renderVga <$> specFrame g) | g <- runs ]
  , testGroup "HDMI goldens (test/golden/hdmi/, from the spec goldens)"
      [ frameGolden (hdmiPath g) (renderHdmi <$> specFrame g) | g <- runs ]
  , goldenTest "test/golden/i2c_writes.txt = one pass through the HDMI audio table (--accept to update)"
      (BC.readFile "test/golden/i2c_writes.txt") (pure (BC.pack (writesLog advWrites)))
      (\g t -> pure (if g == t then Nothing else Just "I2C log differs"))
      (BC.writeFile "test/golden/i2c_writes.txt")
  , testCase "display path in Clash simulation: the Ready screen = its VGA golden" $ do
      os <- loadFile osRom
      basic <- loadFile basicRom
      let pic = runPicture 300 (stockConfig os basic Nothing)
      golden <- either error id <$> readFrame (vgaPath "ready")
      frameDiff golden (clashDisplay pic) @?= Nothing
  ]
 where
  vgaPath g = "test/golden/vga" </> g <.> "png"
  hdmiPath g = "test/golden/hdmi" </> g <.> "png"

advWrites :: [(Word8, Word8)]
advWrites = expectedWritesFor adv7511HdmiAudioTable

specFrame :: String -> IO Frame
specFrame g = either error id <$> readFrame ("test/golden" </> g <.> "png")

frameGolden :: FilePath -> IO Frame -> TestTree
frameGolden path make =
  goldenTest (path <> " (--accept to update)")
    (either error id <$> readFrame path)
    make
    (\a b -> pure (frameDiff a b))
    (writeFrame path)

-- | The picture ×2, through @conv@.
place :: (PixelRGB8 -> PixelRGB8) -> Frame -> Frame
place conv f = generateImage (\x y -> conv (pixelAt f (x `div` 2) (y `div` 2))) 640 480

-- | VGA: the top 4 bits of each channel, as the decoder widens them (× 17).
renderVga :: Frame -> Frame
renderVga = place (\(PixelRGB8 r g b) -> PixelRGB8 (q r) (q g) (q b))
 where q v = (v `div` 16) * 17

-- | HDMI: the 24-bit colours through the converter and the chip's CSC.
renderHdmi :: Frame -> Frame
renderHdmi f = hdmiFrame (registerImage advWrites) 640 480 colour
 where
  big = place id f
  colour x y = let PixelRGB8 r g b = pixelAt big x y in Rgb888 (fromIntegral r) (fromIntegral g) (fromIntegral b)

-- | The display path ("A800.Display") in Clash simulation: the picture's
-- colour clocks are written as the core writes them, the frame is
-- completed, and the third VGA frame is decoded from the pins.
clashDisplay :: Picture -> Frame
clashDisplay pic = frames !! 2
 where
  px = picturePixels pic
  w = pictureWidth `div` 2
  n = w * pictureHeight
  writes = [ Just (fromIntegral (i `div` w + 8), fromIntegral (i `mod` w + 0x30), (fromIntegral (px U.! (2 * i)), fromIntegral (px U.! (2 * i + 1))))
           | i <- [0 .. n - 1] ] <> repeat Nothing
  done = replicate n False <> [True] <> repeat False
  pins :: C.Signal C.System VgaPins
  pins = C.withClockResetEnable C.clockGen C.resetGen C.enableGen $
    let vga = vgaTiming vga640x480
        fb = a800FrameBuffers (C.fromList writes) (C.fromList done) (vgaVBlankStart <$> vga) rd
        (rd, (timing, rgb)) = a800Stream vga fb
    in vgaPins vga640x480 timing rgb
  frames = decodeFrames (vgaSink vga640x480) (C.sample pins)
  _unused = pictureFrame
