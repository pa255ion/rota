-- | Layer 3 (PLAN.md step 3.7): the VGA and HDMI goldens of layer 4,
-- derived from the frozen spec screens (the picture ×2 at (64, 0); VGA
-- shows the top 4 bits of each channel, HDMI all 8), and the display path
-- in Clash simulation against them.
module Main (main) where

import Prelude

import Codec.Picture (PixelRGB8 (..), generateImage, pixelAt)
import qualified Data.Vector.Unboxed as U
import Data.Word
import System.FilePath ((</>), (<.>))
import Test.Tasty
import Test.Tasty.Golden.Advanced (goldenTest)
import Test.Tasty.HUnit

import qualified Clash.Prelude as C
import Nes.Display
import Nes.Screen
import Nes.Spec (runFramesWith)
import Nes.TestRoms
import qualified Data.ByteString.Char8 as BC
import Platform.Sim.Adv7511 (expectedWritesFor, hdmiFrame, registerImage, writesLog)
import Platform.Video.Adv7511.Table (adv7511HdmiAudioTable)
import Platform.Sim.Frame
import Platform.Sim.Overlay (overlayImage, parseScript, scriptScreen)
import Platform.Video.VgaTiming

-- | The layer 4 runs (systems/nes/zedboard.mk, Makefile ci-nes): golden
-- name, ROM, frames the spec runs, buttons held.
runs :: [(String, FilePath, Int, Word8)]
runs =
  [ ("ppu_scene", asmRoot </> "ppu_scene.nes", 10, 0)
  , ("nestest", romRoot </> "nestest" </> "nestest.nes", 30, 0)
  , ("buttons", asmRoot </> "buttons.nes", 10, 0x11) -- A and Up held
  ]

main :: IO ()
main = defaultMain $ testGroup "nes-golden"
  [ testGroup "spec screen of buttons.s with A and Up held (test/golden/spec/, checked by eye)"
      [ screenGolden "buttons" ("test" </> "golden" </> "spec" </> "buttons.png") (specRun ("buttons", asmRoot </> "buttons.nes", 10, 0x11)) ]
  , testGroup "VGA goldens (test/golden/vga/, from the spec goldens)"
      [ frameGolden (vgaPath g) (renderVga <$> specFrame g) | (g, _, _, _) <- runs ]
  , testGroup "HDMI goldens (test/golden/hdmi/, from the spec goldens)"
      [ frameGolden (hdmiPath g) (renderHdmi <$> specFrame g) | (g, _, _, _) <- runs ]
  , testGroup "menu overlay goldens for layer 4 (make resim SYSTEM=nes SIM_PS=test/ps/menu.ps; step 3.8)"
      [ frameGolden ("test/golden" </> dir </> "menu-overlay.png") (conv <$> menuOverlay)
      | (dir, conv) <- [("vga", id), ("hdmi", hdmiOf)] ]
  , goldenTest "test/golden/i2c_writes.txt = one pass through the HDMI audio table (--accept to update)"
      (BC.readFile "test/golden/i2c_writes.txt") (pure (BC.pack (writesLog advWrites)))
      (\g t -> pure (if g == t then Nothing else Just "I2C log differs"))
      (BC.writeFile "test/golden/i2c_writes.txt")
  , testCase "display path in Clash simulation: ppu_scene's spec screen = its VGA golden" $ do
      sc <- specRun ("ppu_scene", asmRoot </> "ppu_scene.nes", 10, 0)
      golden <- either error id <$> readFrame (vgaPath "ppu_scene")
      frameDiff golden (clashDisplay sc) @?= Nothing
  ]
 where
  vgaPath g = "test/golden/vga" </> g <.> "png"
  hdmiPath g = "test/golden/hdmi" </> g <.> "png"

-- | What the sequencer writes to the (simulated) chip: HDMI with audio
-- (step 3.9).
advWrites :: [(Word8, Word8)]
advWrites = expectedWritesFor adv7511HdmiAudioTable

specRun :: (String, FilePath, Int, Word8) -> IO Screen
specRun (_, path, n, pad) = do
  rom <- loadRomFile path
  either (ioError . userError) pure (runFramesWith pad n rom)

specFrame :: String -> IO Frame
specFrame g = either error id <$> readFrame ("test/golden/spec" </> g <.> "png")

frameGolden :: FilePath -> IO Frame -> TestTree
frameGolden path make =
  goldenTest (path <> " (--accept to update)")
    (either error id <$> readFrame path)
    make
    (\a b -> pure (frameDiff a b))
    (writeFrame path)

-- | The picture ×2 at (64, 0), black around it, through @conv@.
place :: (PixelRGB8 -> PixelRGB8) -> Frame -> Frame
place conv f = generateImage px 640 480
 where
  px x y
    | x >= 64 && x < 576 = conv (pixelAt f ((x - 64) `div` 2) (y `div` 2))
    | otherwise = PixelRGB8 0 0 0

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

-- | The picture the menu script leaves on screen: the text overlay only
-- (the script turns it on and pauses the core).
menuOverlay :: IO Frame
menuOverlay = do
  ops <- either error id . parseScript <$> readFile "test/ps/menu.ps"
  let (cells, ctl) = scriptScreen ops
  if ctl `mod` 2 == 0 then error "test/ps/menu.ps leaves the overlay off" else pure (overlayImage cells)

-- | A full 640 × 480 picture through the converter and the chip's CSC.
hdmiOf :: Frame -> Frame
hdmiOf f = hdmiFrame (registerImage advWrites) 640 480 colour
 where
  colour x y = let PixelRGB8 r g b = pixelAt f x y in Rgb888 (fromIntegral r) (fromIntegral g) (fromIntegral b)

-- | The display path (Nes.Display) in Clash simulation: the screen's pixels
-- are written as the core writes them, the frame is completed, and the
-- third VGA frame is decoded from the pins.
clashDisplay :: Screen -> Frame
clashDisplay sc = frames !! 2
 where
  px = screenPixels sc
  n = 256 * 240
  writes = [ Just (fromIntegral (i `mod` 256), fromIntegral (i `div` 256), fromIntegral (px U.! i)) | i <- [0 .. n - 1] ]
           <> repeat Nothing
  done = replicate n False <> [True] <> repeat False
  pins :: C.Signal C.System VgaPins
  pins = C.withClockResetEnable C.clockGen C.resetGen C.enableGen $
    let vga = vgaTiming vga640x480
        fb = nesFrameBuffers (C.fromList writes) (C.fromList done) (vgaVBlankStart <$> vga) pixA lineA
        ((pixA, lineA), (timing, rgb)) = nesStream vga fb
    in vgaPins vga640x480 timing rgb
  frames = decodeFrames (vgaSink vga640x480) (C.sample pins)
