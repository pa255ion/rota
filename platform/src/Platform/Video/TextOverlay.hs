{-|
A text screen over the 640×480 pixel stream, for the PS app's menu
(milestone 1b, shared by every system; written through the PS link,
"Platform.Ps.Link"). 40 × 30 character cells of 16 × 16 pixels: the 8 × 8
glyphs of "Platform.Video.Font8x8", doubled in both directions.

The character RAM holds one byte per cell, row by row: bits 0 to 6 the
character (U+0000 to U+007F), bit 7 inverse video (the menu's cursor bar).
It starts blank (spaces). While the overlay is shown it replaces the whole
picture; while it is hidden the stream passes through unchanged, so a
system's own picture is the same with or without this module, only
'overlayLatency' clocks later (timing and colour are delayed together).
-}
module Platform.Video.TextOverlay
  ( textCols
  , textRows
  , overlayLatency
  , overlayFg
  , overlayBg
  , cellIndex
  , glyphPixel
  , textOverlay
  ) where

import Clash.Prelude

import Platform.Video.Font8x8 (font8x8)
import Platform.Video.VgaTiming

textCols, textRows :: Int
textCols = 40
textRows = 30

-- | Clocks from the input stream to the output stream: the character RAM
-- read, then the font ROM read.
overlayLatency :: Int
overlayLatency = 2

-- | Text and background colours (inverse video swaps them).
overlayFg, overlayBg :: Rgb444
overlayFg = Rgb444 0xE 0xE 0xE
overlayBg = Rgb444 0x1 0x2 0x5

-- | The character cell under a visible pixel: @40 * (y / 16) + x / 16@
-- (0 outside the picture).
cellIndex :: Unsigned 10 -> Unsigned 10 -> Index 1200
cellIndex x y
  | x < 640 && y < 480 = unpack (pack (row * 32 + row * 8 + col))
  | otherwise          = 0
 where
  row = resize (y `shiftR` 4) :: Unsigned 11
  col = resize (x `shiftR` 4) :: Unsigned 11

-- | Whether glyph pixel @(x, y)@ of a cell (0 to 7 each, in glyph pixels)
-- is set, given the glyph's row byte @y@ (bit 0 = leftmost pixel).
glyphPixel :: BitVector 8 -> Unsigned 10 -> Bool
glyphPixel row x = testBit row (fromIntegral ((x `shiftR` 1) .&. 7))

-- | @textOverlay writes shown (timing, colour)@: the stream with the text
-- screen over it while @shown@ is high, 'overlayLatency' clocks later.
-- @writes@ go into the character RAM (cell, byte).
textOverlay
  :: HiddenClockResetEnable dom
  => Signal dom (Maybe (Index 1200, BitVector 8))
  -> Signal dom Bool
  -> (Signal dom VgaOut, Signal dom Rgb888)
  -> (Signal dom VgaOut, Signal dom Rgb888)
textOverlay writes shown (timing, rgb) = (timing2, out)
 where
  idle = vgaDecode vga640x480 (VgaCounters 0 0)
  -- clock 1: the cell's character
  ch      = blockRam (replicate (SNat @1200) 0x20) (cellIndex <$> (vgaX <$> timing) <*> (vgaY <$> timing)) writes
  timing1 = register idle timing
  -- clock 2: the glyph row
  glyph   = rom font8x8 (fontAddr <$> ch <*> (vgaY <$> timing1))
  inv2    = register False (flip testBit 7 <$> ch)
  timing2 = register idle timing1
  rgb2    = register black8 (register black8 rgb)
  shown2  = register False (register False shown)
  out     = colour <$> shown2 <*> timing2 <*> glyph <*> inv2 <*> rgb2
  colour s o g inv c
    | s && vgaActive o = if glyphPixel g (vgaX o) /= inv then fromRgb444 overlayFg else fromRgb444 overlayBg
    | otherwise        = c
  -- glyph c, row (y / 2) mod 8
  fontAddr :: BitVector 8 -> Unsigned 10 -> Unsigned 10
  fontAddr c y = unpack (slice d6 d0 c ++# slice d3 d1 (pack y))
