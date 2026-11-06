-- | Layer 3: the Game Boy display path, and the goldens for layer 4.
--
-- * VGA goldens (test/golden/vga/<rom>.png, compared by @make sim@ with
--   the last frame of the generated Verilog): the spec goldens ×3 at
--   (80, 24) in 640×480, grey palette, black around.
-- * HDMI goldens (test/golden/hdmi/<rom>.png): the VGA goldens through the
--   4:2:2 converter and the ADV7511 model.
-- * The I2C golden (test/golden/i2c_writes.txt): one pass through the
--   HDMI-with-audio register table from the simulated chip's defaults.
-- * The display path in Clash simulation (frame buffers, scaler, VGA
--   timing) renders a spec screen as exactly that VGA golden.
module Main (main) where

import Prelude

import Codec.Picture (PixelRGB8 (..), generateImage, pixelAt)
import qualified Data.ByteString.Char8 as BC
import Data.Word (Word8)
import qualified Data.Vector.Unboxed as U
import System.FilePath ((<.>), (</>))
import Test.Tasty
import Test.Tasty.Golden.Advanced (goldenTest)
import Test.Tasty.HUnit

import qualified Clash.Prelude as C
import Gb.Display
import Gb.Screen
import Platform.Sim.Adv7511 (expectedWritesFor, hdmiFrame, registerImage, writesLog)
import Platform.Video.Adv7511.Table (adv7511HdmiAudioTable)
import Platform.Sim.Frame
import Platform.Sim.Overlay (overlayImage, parseScript, scriptScreen)
import Platform.Video.VgaTiming

-- | (layer 4 golden name = ROM base name, spec golden name)
goldens :: [(String, String)]
goldens =
  [ ("dmg-acid2", "dmg-acid2")
  , ("instr_timing", "blargg-instr_timing")
  , ("mem_timing", "blargg-mem_timing")
  , ("buttons", "buttons-right-a")
  , ("save", "save-42")
  ]

main :: IO ()
main = defaultMain $ testGroup "gb-golden"
  [ testGroup "VGA goldens for layer 4, from the spec goldens (×3, letterboxed, grey)"
      [ goldenTest ("test/golden/vga/" <> g <> ".png (--accept to update)")
          (either error id <$> readFrame (vgaPath g))
          (renderVga <$> specScreen s)
          (\a b -> pure (frameDiff a b))
          (writeFrame (vgaPath g))
      | (g, s) <- goldens ]
  , testGroup "HDMI goldens for layer 4, from the VGA goldens (4:2:2 + the ADV7511 model)"
      [ goldenTest ("test/golden/hdmi/" <> g <> ".png (--accept to update)")
          (either error id <$> readFrame (hdmiPath g))
          (hdmiOf <$> (either error id <$> readFrame (vgaPath g)))
          (\a b -> pure (frameDiff a b))
          (writeFrame (hdmiPath g))
      | (g, _) <- goldens ]
  , testGroup "menu overlay goldens for layer 4 (make sim SYSTEM=gb SIM_PS=test/ps/menu.ps; side track A)"
      [ goldenTest ("test/golden/" <> d <> "/menu-overlay.png (--accept to update)")
          (either error id <$> readFrame ("test/golden" </> d </> "menu-overlay.png"))
          (conv <$> menuOverlay)
          (\a b -> pure (frameDiff a b))
          (writeFrame ("test/golden" </> d </> "menu-overlay.png"))
      | (d, conv) <- [("vga", id), ("hdmi", hdmiOf)] ]
  , goldenTest "test/golden/i2c_writes.txt = one pass through the HDMI audio table (--accept to update)"
      (BC.readFile "test/golden/i2c_writes.txt") (pure (BC.pack (writesLog advWrites)))
      (\g t -> pure (if g == t then Nothing else Just "I2C log differs"))
      (BC.writeFile "test/golden/i2c_writes.txt")
  , testGroup "Game Boy Color goldens for layer 4 (make sim SYSTEM=gbc; phase 3 step 5)"
      [ goldenTest ("test/golden/vga/" <> g <> ".png (--accept to update)")
          (either error id <$> readFrame (vgaPath g))
          (renderVgaC <$> specColour s)
          (\a b -> pure (frameDiff a b))
          (writeFrame (vgaPath g))
      | (g, s) <- goldensC ]
  , testGroup "Game Boy Color HDMI goldens, from the spec's colours (4:2:2 + the ADV7511 model)"
      [ goldenTest ("test/golden/hdmi/" <> g <> ".png (--accept to update)")
          (either error id <$> readFrame (hdmiPath g))
          (hdmiOfC <$> specColour s)
          (\a b -> pure (frameDiff a b))
          (writeFrame (hdmiPath g))
      | (g, s) <- goldensC ]
  , testGroup "Game Boy Color full-colour goldens (the Arty's TMDS carries all 8 bits a channel)"
      [ goldenTest ("test/golden/rgb/" <> g <> ".png (--accept to update)")
          (either error id <$> readFrame ("test/golden/rgb" </> g <.> "png"))
          (renderRgbC <$> specColour s)
          (\a b -> pure (frameDiff a b))
          (writeFrame ("test/golden/rgb" </> g <.> "png"))
      | (g, s) <- goldensC ]
  , testCase "display path in Clash simulation: cgb-acid2's spec screen = its VGA golden (15-bit frame buffers)" $ do
      f <- specColour "cgb-acid2"
      golden <- either error id <$> readFrame (vgaPath "cgb-acid2")
      frameDiff golden (clashDisplayC f) @?= Nothing
  , testCase "display path in Clash simulation: dmg-acid2's spec screen = its VGA golden" $ do
      sc <- specScreen "dmg-acid2"
      golden <- either error id <$> readFrame (vgaPath "dmg-acid2")
      frameDiff golden (clashDisplay sc) @?= Nothing
  ]
 where
  vgaPath g = "test/golden/vga" </> g <.> "png"
  hdmiPath g = "test/golden/hdmi" </> g <.> "png"

-- | The Game Boy Color's goldens (layer 4 name, spec golden name).
goldensC :: [(String, String)]
goldensC = [ ("cgb-acid2", "cgb-acid2"), ("dmg-acid2-gbc", "dmg-acid2-gbc") ]

-- | A Game Boy Color spec golden: 160 × 144, 24-bit ('rgbFrame').
specColour :: String -> IO Frame
specColour s = either error id <$> readFrame ("test/golden/spec" </> s <.> "png")

-- | A 24-bit colour of a spec golden back as the 15-bit colour
-- ('Gb.Screen.rgb8' keeps the top 5 bits).
colour15 :: Frame -> Int -> Int -> C.BitVector 15
colour15 f x y = let PixelRGB8 r g b = pixelAt f x y
                 in C.pack (C.resize (C.pack (b `div` 8)) :: C.BitVector 5)
                      C.++# (C.resize (C.pack (g `div` 8)) :: C.BitVector 5)
                      C.++# (C.resize (C.pack (r `div` 8)) :: C.BitVector 5)

-- | What the display path should show on VGA for a colour screen: each
-- channel's top 4 bits (the VGA DAC), ×3 at (80, 24).
renderVgaC :: Frame -> Frame
renderVgaC f = generateImage px 640 480
 where
  px x y
    | x >= 80 && x < 560 && y >= 24 && y < 456 =
        let Rgb888 r g b = rgb15 (colour15 f ((x - 80) `div` 3) ((y - 24) `div` 3))
        in PixelRGB8 (n r) (n g) (n b)
    | otherwise = PixelRGB8 0 0 0
  n v = fromIntegral (v `C.shiftR` 4) * 17

-- | ... and the stream itself (all 8 bits; the Arty's TMDS output).
renderRgbC :: Frame -> Frame
renderRgbC f = generateImage px 640 480
 where
  px x y
    | x >= 80 && x < 560 && y >= 24 && y < 456 =
        let Rgb888 r g b = rgb15 (colour15 f ((x - 80) `div` 3) ((y - 24) `div` 3))
        in PixelRGB8 (fromIntegral r) (fromIntegral g) (fromIntegral b)
    | otherwise = PixelRGB8 0 0 0

-- | ... and on HDMI: all 8 bits through 4:2:2 and the ADV7511 model.
hdmiOfC :: Frame -> Frame
hdmiOfC f = hdmiFrame (registerImage advWrites) 640 480 colour
 where
  colour x y
    | x >= 80 && x < 560 && y >= 24 && y < 456 = rgb15 (colour15 f ((x - 80) `div` 3) ((y - 24) `div` 3))
    | otherwise = black8

specScreen :: String -> IO Screen
specScreen s = do
  f <- either error id <$> readFrame ("test/golden/spec" </> s <.> "png")
  either error pure (frameScreen f)

-- | What the display path should show for a screen (grey palette).
renderVga :: Screen -> Frame
renderVga sc = generateImage px 640 480
 where
  px x y
    | x >= 80 && x < 560 && y >= 24 && y < 456 =
        let Rgb444 r g b = palette Grey (fromIntegral (shadeAt sc ((x - 80) `div` 3) ((y - 24) `div` 3)))
        in PixelRGB8 (c r) (c g) (c b)
    | otherwise = PixelRGB8 0 0 0
  c v = fromIntegral v * 17

-- | What the sequencer writes to the (simulated) chip.
advWrites :: [(Word8, Word8)]
advWrites = expectedWritesFor adv7511HdmiAudioTable

-- | The picture the menu script leaves on screen: the text overlay only
-- (the script turns it on and pauses the core).
menuOverlay :: IO Frame
menuOverlay = do
  ops <- either error id . parseScript <$> readFile "test/ps/menu.ps"
  let (cells, ctl) = scriptScreen ops
  if ctl `mod` 2 == 0 then error "test/ps/menu.ps leaves the overlay off" else pure (overlayImage cells)

hdmiOf :: Frame -> Frame
hdmiOf f = hdmiFrame (registerImage advWrites) 640 480 colour
 where
  colour x y = let PixelRGB8 r g b = pixelAt f x y
               in fromRgb444 (Rgb444 (fromIntegral (r `div` 17)) (fromIntegral (g `div` 17)) (fromIntegral (b `div` 17)))

-- | The real display path (Gb.Display) in Clash simulation: the screen's
-- pixels are written, the frame is completed, and the third VGA frame is
-- decoded from the pins.
clashDisplay :: Screen -> Frame
clashDisplay sc = frames !! 2
 where
  shades = screenShades sc
  n = 160 * 144
  writes = [ Just (fromIntegral i, fromIntegral (shades U.! i)) | i <- [0 .. n - 1] ] <> repeat Nothing
  done = replicate n False <> [True] <> repeat False
  pins :: C.Signal C.System VgaPins
  pins = C.withClockResetEnable C.clockGen C.resetGen C.enableGen $
    let vga = vgaTiming vga640x480
        fb = frameBuffers (C.fromList writes) (C.fromList done) (vgaVBlankStart <$> vga) addr
        (addr, (timing, rgb)) = gbStream (pure Grey) vga fb
    in vgaPins vga640x480 timing rgb
  frames = decodeFrames (vgaSink vga640x480) (C.sample pins)

-- | 'clashDisplay' for a Game Boy Color screen: 15-bit frame buffers and
-- 'rgb15'.
clashDisplayC :: Frame -> Frame
clashDisplayC f = frames !! 2
 where
  n = 160 * 144
  writes = [ Just (fromIntegral i, colour15 f (i `mod` 160) (i `div` 160)) | i <- [0 .. n - 1] ] <> repeat Nothing
  done = replicate n False <> [True] <> repeat False
  pins :: C.Signal C.System VgaPins
  pins = C.withClockResetEnable C.clockGen C.resetGen C.enableGen $
    let vga = vgaTiming vga640x480
        fb = frameBuffersC (C.fromList writes) (C.fromList done) (vgaVBlankStart <$> vga) addr
        (addr, (timing, rgb)) = gbStreamWith (\() c -> rgb15 c) (pure ()) vga fb
    in vgaPins vga640x480 timing rgb
  frames = decodeFrames (vgaSink vga640x480) (C.sample pins)
