{-|
The Game Boy's 160×144 screen as shades, and its PNG form for goldens
(docs/systems/gameboy.md, "Screen goldens").

A shade is the 2-bit colour after the palette registers: 0 is the lightest,
3 the darkest. Goldens are greyscale PNGs with shade @s@ drawn as grey
@255 - 85 s@ (0xFF, 0xAA, 0x55, 0x00), which is also how dmg-acid2's
reference image (@reference-dmg.png@) is coded, so it compares directly.
-}
module Gb.Screen
  ( Shade
  , Screen
  , screenWidth
  , screenHeight
  , mkScreen
  , screenShades
  , shadeAt
  , greyLevel
  , screenFrame
  , frameScreen
  , rgbFrame
  , rgb8
  ) where

import Prelude

import Codec.Picture (PixelRGB8 (..), generateImage, imageHeight, imageWidth, pixelAt)
import qualified Data.Vector.Unboxed as U
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.Word

import Platform.Sim.Frame (Frame)

type Shade = Word8

-- | Row-major, 'screenWidth' × 'screenHeight' shades, each 0 to 3.
newtype Screen = Screen (U.Vector Shade)
  deriving stock (Show, Eq)

screenWidth, screenHeight :: Int
screenWidth = 160
screenHeight = 144

-- | 'Left' unless there are exactly 160 × 144 shades, all 0 to 3.
mkScreen :: U.Vector Shade -> Either String Screen
mkScreen v
  | U.length v /= screenWidth * screenHeight = Left ("screen of " <> show (U.length v) <> " pixels")
  | U.any (> 3) v = Left "shade above 3"
  | otherwise = Right (Screen v)

screenShades :: Screen -> U.Vector Shade
screenShades (Screen v) = v

shadeAt :: Screen -> Int -> Int -> Shade
shadeAt (Screen v) x y = v U.! (y * screenWidth + x)

greyLevel :: Shade -> Word8
greyLevel s = 255 - 85 * s

screenFrame :: Screen -> Frame
screenFrame sc = generateImage px screenWidth screenHeight
 where
  px x y = let g = greyLevel (shadeAt sc x y) in PixelRGB8 g g g

-- | A Game Boy Color frame (15-bit BGR, 'Gb.Spec.screenRgb') as 24-bit
-- RGB, each 5-bit channel as @(c << 3) | (c >> 2)@: the conversion
-- cgb-acid2's reference image uses (its colours 13 -> 107, 23 -> 189).
rgbFrame :: U.Vector Word16 -> Frame
rgbFrame v = generateImage px screenWidth screenHeight
 where
  px x y = let c = v U.! (y * screenWidth + x) in PixelRGB8 (rgb8 c 0) (rgb8 c 5) (rgb8 c 10)

-- | Channel at bit @k@ of a 15-bit colour, as 8 bits.
rgb8 :: Word16 -> Int -> Word8
rgb8 c k = let v = fromIntegral ((c `shiftR` k) .&. 0x1F) :: Word8 in (v `shiftL` 3) .|. (v `shiftR` 2)

-- | The inverse of 'screenFrame': 'Left' for the wrong size or a pixel
-- that is not one of the four greys.
frameScreen :: Frame -> Either String Screen
frameScreen f
  | (imageWidth f, imageHeight f) /= (screenWidth, screenHeight) =
      Left ("image is " <> show (imageWidth f) <> "x" <> show (imageHeight f) <> ", not 160x144")
  | otherwise = traverse shade coords >>= mkScreen . U.fromList
 where
  coords = [ (x, y) | y <- [0 .. screenHeight - 1], x <- [0 .. screenWidth - 1] ]
  shade (x, y) = case pixelAt f x y of
    PixelRGB8 r g b
      | r == g, g == b, Just s <- lookup r [ (greyLevel s', s') | s' <- [0 .. 3] ] -> Right s
      | otherwise -> Left ("pixel " <> show (x, y) <> " is " <> show (r, g, b) <> ", not a DMG grey")
