{-|
The picture we show (M4 step 4.3; docs/systems/atari800xl.md,
"Display"): the normal playfield's width, colour clocks $30-$CF, two
pixels each (hires resolution), and the 240 lines the display list can
use, 8-247: 320 × 240 Atari colour bytes, drawn with "A800.Palette" for
PNG goldens. Wider playfields and the overscan are cropped (a known
limit).
-}
module A800.Picture
  ( Picture
  , pictureWidth
  , pictureHeight
  , cropPicture
  , picturePixels
  , pictureFrame
  ) where

import Prelude

import Codec.Picture (PixelRGB8 (..), generateImage)
import qualified Data.Vector.Unboxed as U
import Data.Word

import A800.Palette (paletteRgb)
import Platform.Sim.Frame (Frame)

newtype Picture = Picture (U.Vector Word8)
  deriving stock (Show, Eq)

pictureWidth, pictureHeight :: Int
pictureWidth = 320
pictureHeight = 240

-- | From GTIA's frame: 456 bytes per line (two per colour clock), 262
-- lines.
cropPicture :: U.Vector Word8 -> Picture
cropPicture full = Picture $ U.generate (pictureWidth * pictureHeight) $ \i ->
  let (y, x) = i `divMod` pictureWidth
  in full U.! ((y + 8) * 456 + 2 * 0x30 + x)

picturePixels :: Picture -> U.Vector Word8
picturePixels (Picture v) = v

pictureFrame :: Picture -> Frame
pictureFrame (Picture v) = generateImage px pictureWidth pictureHeight
 where
  px x y = let (r, g, b) = paletteRgb (v U.! (y * pictureWidth + x)) in PixelRGB8 r g b
