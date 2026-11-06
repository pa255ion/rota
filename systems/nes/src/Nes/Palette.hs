{-|
The NES palette, generated from the 2C02's composite signal (PLAN.md
step 3.3; docs/systems/nes.md, "Display path"): our own table, not an
emulator's palette file.

Method (nesdev wiki, "NTSC video", the signal levels measured on a 2C02G
and its decoding example): a palette entry @0bLLCCCC@ is a square wave
over the 12 phases of the colour subcarrier, high for the 6 phases
starting at hue @C@ and low for the other 6. The levels come from the
luma @L@ (voltages relative to sync: low 0.350, 0.518, 0.962, 1.550;
high 1.094, 1.506, 1.962, 1.962; black 0.518, white 1.962). Hue 0 is the
high level only, hues 13 to 15 the low level only (and 14, 15 are forced
to luma 1, which is black). Each emphasis bit attenuates the signal to
0.746 during the 6 phases of its colour (red: hue 0, green: hue 4, blue:
hue 8). The decoder averages the 12 samples to Y and correlates them with
the subcarrier for I and Q, then the FCC YIQ matrix gives RGB, clamped
to 0..1. The hue rotation (@hueShift@) puts hue 8 at the burst phase, as
the console's colour burst does, so hue 6 comes out red, 10 green and 2
blue.
-}
module Nes.Palette
  ( ntscPalette
  , paletteRgb
  , paletteWords
  ) where

import Prelude

import Data.Bits (shiftR, testBit, (.&.))
import qualified Data.Vector.Unboxed as U
import Data.Word

-- | 512 entries: palette entry (bits 0 to 5) and emphasis (bits 6 to 8:
-- red, green, blue as in PPUMASK bits 5 to 7), as (R, G, B).
ntscPalette :: U.Vector (Word8, Word8, Word8)
ntscPalette = U.generate 512 entry

paletteRgb :: Word16 -> (Word8, Word8, Word8)
paletteRgb c = U.unsafeIndex ntscPalette (fromIntegral c .&. 511)

levels :: [Double]
levels = [0.350, 0.518, 0.962, 1.550, 1.094, 1.506, 1.962, 1.962]

black, white, attenuation :: Double
black = 0.518
white = 1.962
attenuation = 0.746

-- | Phase offset of the decoder (in 1/12 of a cycle).
hueShift :: Double
hueShift = 3.9

signal :: Int -> Int -> Double
signal c phase =
  let hue = c .&. 0x0F
      luma = if hue > 13 then 1 else (c `shiftR` 4) .&. 3
      emph = c `shiftR` 6
      low0 = levels !! luma
      high0 = levels !! (4 + luma)
      (low, high) | hue == 0 = (high0, high0)
                  | hue > 12 = (low0, low0)
                  | otherwise = (low0, high0)
      inPhase h = (h + phase) `mod` 12 < 6
      s = if inPhase hue then high else low
      att = (testBit emph 0 && inPhase 0) || (testBit emph 1 && inPhase 4) || (testBit emph 2 && inPhase 8)
  in if att then s * attenuation else s

entry :: Int -> (Word8, Word8, Word8)
entry c =
  let samples = [ ((signal c p - black) / (white - black), fromIntegral p) | p <- [0 .. 11 :: Int] ]
      angle p = pi * (p + hueShift) / 6
      y = sum [ s | (s, _) <- samples ] / 12
      i = sum [ s * cos (angle p) | (s, p) <- samples ] / 12 * 2
      q = sum [ s * sin (angle p) | (s, p) <- samples ] / 12 * 2
      r = y + 0.946882 * i + 0.623557 * q
      g = y - 0.274788 * i - 0.635691 * q
      b = y - 1.108545 * i + 1.709007 * q
      to8 v = fromIntegral (round (255 * max 0 (min 1 v)) :: Int)
  in (to8 r, to8 g, to8 b)

-- | The palette as 24-bit words (R in bits 16 to 23), for the hardware's
-- lookup ROM ("Nes.Display").
paletteWords :: [Word32]
paletteWords = [ fromIntegral r * 65536 + fromIntegral g * 256 + fromIntegral b | (r, g, b) <- U.toList ntscPalette ]
