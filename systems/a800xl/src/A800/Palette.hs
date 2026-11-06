{-|
The 800XL's NTSC palette (M4 step 4.3; docs/systems/atari800xl.md,
"Display"): our own table, generated from GTIA's colour signal, not an
emulator's palette file.

Method (HRM 6.3, "NTSC color encoding"): a colour byte is a hue (bits
4-7) and a luminance (bits 0-3; the colour registers set only even ones,
GTIA mode 9 all 16). Hue 0 has no colour signal: grey. Hue /n/ is a
subcarrier of fixed strength delayed by /n/ steps of 24° from the colour
burst, whose phase is a light yellow-orange ("gold"): so hue 1 is orange
and the hues go on through red, purple, blue, cyan and green back to
gold-ish at 15. The decoder takes the burst as 180° on the U/V plane
(−U, the yellow-orange side), so hue /n/ sits at 180° − 24°·/n/; the
luminance is linear from 0.04 (0) to 0.96 (15); the chroma's amplitude is
0.22 (on the U/V scale); YUV to RGB with the BT.601 matrix, clamped.
-}
module A800.Palette
  ( ntscPalette
  , paletteRgb
  , paletteWords
  ) where

import Prelude

import Data.Bits (shiftR, (.&.))
import qualified Data.Vector.Unboxed as U
import Data.Word

ntscPalette :: U.Vector (Word8, Word8, Word8)
ntscPalette = U.generate 256 entry

paletteRgb :: Word8 -> (Word8, Word8, Word8)
paletteRgb c = U.unsafeIndex ntscPalette (fromIntegral c)

entry :: Int -> (Word8, Word8, Word8)
entry c =
  let hue = (c `shiftR` 4) .&. 15
      lum = c .&. 15
      y = 0.04 + 0.92 * fromIntegral lum / 15 :: Double
      ang = (180 - 24 * fromIntegral hue) * pi / 180
      a = if hue == 0 then 0 else 0.22
      u = a * cos ang
      v = a * sin ang
      r = y + 1.140 * v
      g = y - 0.395 * u - 0.581 * v
      b = y + 2.032 * u
      to8 x = fromIntegral (round (255 * max 0 (min 1 x)) :: Int)
  in (to8 r, to8 g, to8 b)

-- | The palette as 24-bit words (R in bits 16 to 23), for the hardware's
-- lookup ROM (step 4.7).
paletteWords :: [Word32]
paletteWords = [ fromIntegral r * 65536 + fromIntegral g * 256 + fromIntegral b | (r, g, b) <- U.toList ntscPalette ]
