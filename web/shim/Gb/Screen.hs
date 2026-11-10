{-|
The browser build's stand-in for "Gb.Screen" (side track W): its pure
part, copied exactly ('Screen', 'mkScreen', 'greyLevel', 'rgb8', ...),
without the PNG frames (JuicyPixels and the simulation's frame type, which
is Clash's), which the WebAssembly build leaves out. web/test/check.mjs
compares the build's frames with the spec's goldens, so a drift shows there.
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
  , rgb8
  ) where

import Prelude

import qualified Data.Vector.Unboxed as U
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.Word

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

-- | Channel at bit @k@ of a 15-bit colour, as 8 bits.
rgb8 :: Word16 -> Int -> Word8
rgb8 c k = let v = fromIntegral ((c `shiftR` k) .&. 0x1F) :: Word8 in (v `shiftL` 3) .|. (v `shiftR` 2)
