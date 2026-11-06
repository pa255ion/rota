{-|
The NES's 256×240 picture and its PNG form for goldens (PLAN.md step
3.3; docs/systems/nes.md, "Test ROMs"): each pixel is the PPU's 9-bit
output (palette entry and emphasis bits), drawn with "Nes.Palette".
-}
module Nes.Screen
  ( Screen
  , mkScreen
  , screenPixels
  , screenFrame
  ) where

import Prelude

import Codec.Picture (PixelRGB8 (..), generateImage)
import qualified Data.Vector.Unboxed as U
import Data.Word

import Nes.Palette (paletteRgb)
import Nes.Spec.Ppu (screenHeight, screenWidth)
import Platform.Sim.Frame (Frame)

-- | Row-major 256 × 240 PPU output values.
newtype Screen = Screen (U.Vector Word16)
  deriving stock (Show, Eq)

mkScreen :: U.Vector Word16 -> Either String Screen
mkScreen v
  | U.length v /= screenWidth * screenHeight = Left ("screen of " <> show (U.length v) <> " pixels")
  | otherwise = Right (Screen v)

screenPixels :: Screen -> U.Vector Word16
screenPixels (Screen v) = v

screenFrame :: Screen -> Frame
screenFrame (Screen v) = generateImage px screenWidth screenHeight
 where
  px x y = let (r, g, b) = paletteRgb (v U.! (y * screenWidth + x)) in PixelRGB8 r g b
