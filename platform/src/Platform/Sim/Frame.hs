{-|
Simulation-only frame capture (layer 3, docs/TESTING.md): decode a stream of
VGA pin levels, one per pixel clock, into frames, the way a monitor would,
and read, write and compare those frames as PNG.

The decoder only looks at the pins, never at the generator's counters, so
it also catches sync placement mistakes. It is the same algorithm as the
Verilator harness's (@platform/sim/vga.zig@), so frames decoded at layer 3
and layer 4 can be compared with the same golden images:

* a falling HSYNC edge restarts the pixel counter @hc@ (0 on the edge's
  clock) and advances the line counter @n@;
* a falling VSYNC edge sets @n@ to 0 and drops any partial frame;
* the pixel at counters @(hc, n)@ is column @hc - hStart@ of row
  @n - vStart@, where @hStart@ = HSYNC pulse + back porch (clocks) and
  @vStart@ = VSYNC pulse + back porch (lines). This assumes negative
  syncs.

A frame is emitted as soon as its last visible pixel arrives. Frames drawn
before the first VSYNC edge are never seen; for a generator that starts at
pixel (0, 0) after reset, decoded frame 0 is generated frame 1.
-}
module Platform.Sim.Frame
  ( Frame
  , VgaSink (..)
  , vgaSink
  , decodeFrames
  , rgbPixel
  , writeFrame
  , readFrame
  , frameDiff
  ) where

import Prelude

import Codec.Picture
import qualified Data.ByteString as BS
import qualified Data.Vector.Storable as V

import qualified Clash.Prelude as C
import Platform.Video.VgaTiming

type Frame = Image PixelRGB8

-- | Where the visible area sits, relative to the sync edges.
data VgaSink = VgaSink
  { vsHStart :: Int
  , vsVStart :: Int
  , vsWidth  :: Int
  , vsHeight :: Int
  } deriving stock (Show, Eq)

vgaSink :: VgaMode -> VgaSink
vgaSink (VgaMode h v) = VgaSink
  { vsHStart = fromIntegral (axSync h + axBackPorch h)
  , vsVStart = fromIntegral (axSync v + axBackPorch v)
  , vsWidth  = fromIntegral (axVisible h)
  , vsHeight = fromIntegral (axVisible v)
  }

-- | 4-bit channels widened to 8 bits (0xF -> 0xFF).
rgbPixel :: Rgb444 -> PixelRGB8
rgbPixel (Rgb444 r g b) = PixelRGB8 (w r) (w g) (w b)
 where
  w x = fromIntegral x * 17

data St = St
  { stHs   :: C.Bit
  , stVs   :: C.Bit
  , stHc   :: Int
  , stLine :: Maybe Int   -- Nothing until the first VSYNC edge
  , stAcc  :: [PixelRGB8] -- this frame's pixels so far, newest first
  , stLen  :: Int
  }

-- | All complete frames in a pin stream, lazily.
decodeFrames :: VgaSink -> [VgaPins] -> [Frame]
decodeFrames (VgaSink hStart vStart w h) = go (St 0 0 0 Nothing [] 0)
 where
  go _ [] = []
  go st (VgaPins hs vs c : ps) =
    let vsFall = stVs st == 1 && vs == 0
        hsFall = stHs st == 1 && hs == 0
        hc     = if hsFall then 0 else stHc st + 1
        line0  = if vsFall then Just 0 else stLine st
        line   = if hsFall then fmap (+ 1) line0 else line0
        (acc0, len0) = if vsFall then ([], 0) else (stAcc st, stLen st)
        col    = hc - hStart
        visible = case line of
          Just n  -> let row = n - vStart
                     in row >= 0 && row < h && col >= 0 && col < w
          Nothing -> False
        acc    = if visible then rgbPixel c : acc0 else acc0
        len    = if visible then len0 + 1 else len0
        st'    = St hs vs hc line acc len
    in if visible && len == w * h
         then mkFrame acc : go st' { stAcc = [], stLen = 0 } ps
         else go st' ps

  mkFrame acc =
    let v = V.fromListN (w * h * 3) (concatMap (\(PixelRGB8 r g b) -> [r, g, b]) (reverse acc))
    in Image w h v

writeFrame :: FilePath -> Frame -> IO ()
writeFrame = writePng

-- | Throws the usual IO error if the file is missing (tasty-golden relies on
-- that to create missing golden files); 'Left' if it is not an image.
readFrame :: FilePath -> IO (Either String Frame)
readFrame p = fmap convertRGB8 . decodeImage <$> BS.readFile p

-- | 'Nothing' if equal, otherwise a description of the differences.
frameDiff :: Frame -> Frame -> Maybe String
frameDiff a b
  | (imageWidth a, imageHeight a) /= (imageWidth b, imageHeight b) =
      Just ("size " <> sz a <> " vs " <> sz b)
  | otherwise = case bad of
      []      -> Nothing
      p1 : _  -> Just (show (length bad) <> " pixel(s) differ, first at "
                       <> show p1 <> ": " <> show (px a p1) <> " vs " <> show (px b p1))
 where
  sz i = show (imageWidth i) <> "x" <> show (imageHeight i)
  bad = [ (x, y) | y <- [0 .. imageHeight a - 1], x <- [0 .. imageWidth a - 1]
                 , pixelAt a x y /= pixelAt b x y ]
  px i (x, y) = let PixelRGB8 r g bl = pixelAt i x y in (r, g, bl)
