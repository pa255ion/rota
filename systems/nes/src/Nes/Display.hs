{-|
NES video for the 640×480 output (PLAN.md step 3.7; docs/systems/nes.md
"Clocking", "Frame-rate mismatch", "Display path").

* 'nesDotEnable': the PPU dot clock (5.369318 MHz, master / 4) as an
  enable in 'DomPix' (25.17623 MHz): a 32-bit phase accumulator adding
  915984748 per clock (5369318.18 Hz, 0.0001 ppm). Dots come every 4 or
  5 pixel clocks; "Nes.Core" turns three into a CPU cycle.
* 'nesFrameBuffers': three frames ('Platform.Video.FrameBuffers'), each
  256×240 6-bit palette indices, and per line the three emphasis bits of
  PPUMASK as they were at the line's first pixel (a documented
  approximation: emphasis changed within a line shows from the next one;
  per pixel it would cost 15 more RAMB36, nes.md "Memories").
* 'nesStream': the picture ×2 (512×480) at (64, 0) in 640×480, black
  left and right; 'nesColour' looks the 9 bits up in the palette ROM
  ("Nes.Palette", generated from the 2C02's signal levels).
-}
module Nes.Display
  ( nesDotIncrement
  , nesDotEnable
  , nesFrameBuffers
  , nesStream
  , nesColour
  , paletteRom
  , picX0
  ) where

import Clash.Prelude

import Nes.Palette (paletteWords)
import Platform.Video.FrameBuffers (tripleBuffer)
import Platform.Video.VgaTiming

-- | Added per pixel clock: round (5369318.18 / 25176233.6 × 2^32).
nesDotIncrement :: Unsigned 32
nesDotIncrement = 915984748

-- | One PPU dot elapsed (True for one clock, every 4 or 5 clocks).
nesDotEnable :: HiddenClockResetEnable dom => Signal dom Bool
nesDotEnable = carry
 where
  acc = register (0 :: Unsigned 33) (next <$> acc)
  next a = zeroExtend (truncateB a :: Unsigned 32) + zeroExtend nesDotIncrement
  carry = (`testBit` 32) <$> acc

-- | The 512 colours: palette entry in bits 0 to 5, emphasis in 6 to 8.
paletteRom :: Vec 512 (BitVector 24)
paletteRom = $(listToVecTH (fmap fromIntegral paletteWords :: [BitVector 24]))

nesColour :: Unsigned 9 -> Rgb888
nesColour i = Rgb888 (slice d23 d16 w) (slice d15 d8 w) (slice d7 d0 w)
 where w = paletteRom !! i

-- | Write port from the core (x, y, colour of 'Nes.Core.koPixel'), the
-- core's frame-done pulse, the display's vertical-blank pulse, the read
-- address of a pixel and of its line; the palette index and the line's
-- emphasis (one clock later).
nesFrameBuffers
  :: HiddenClockResetEnable dom
  => Signal dom (Maybe (Unsigned 8, Unsigned 8, Unsigned 9))
  -> Signal dom Bool
  -> Signal dom Bool
  -> Signal dom (Index 61440)
  -> Signal dom (Index 240)
  -> (Signal dom (BitVector 6), Signal dom (BitVector 3))
nesFrameBuffers px frameDone vblank rdPix rdLine = (pix, emph)
 where
  pix = tripleBuffer (SNat @61440) (fmap toPix <$> px) frameDone vblank rdPix
  emph = tripleBuffer (SNat @240) ((toEmph =<<) <$> px) frameDone vblank rdLine
  toPix (x, y, c) = (unpack (pack (resize y * 256 + resize x :: Unsigned 16)), slice d5 d0 (pack c))
  toEmph (x, y, c) = if x == 0 then Just (unpack (pack y), slice d8 d6 (pack c)) else Nothing

-- | Left edge of the picture.
picX0 :: Unsigned 10
picX0 = 64

inPic :: VgaOut -> Bool
inPic o = vgaActive o && vgaX o >= picX0 && vgaX o < picX0 + 512 && vgaY o < 480

-- | @nesStream vga (pix, emph)@: the read addresses for this clock's VGA
-- position (pixel and line), and the timing with its colour one clock
-- later, as the frame buffers answer. VGA and HDMI both take the stream.
nesStream
  :: HiddenClockResetEnable dom
  => Signal dom VgaOut
  -> (Signal dom (BitVector 6), Signal dom (BitVector 3))
  -> ((Signal dom (Index 61440), Signal dom (Index 240)), (Signal dom VgaOut, Signal dom Rgb888))
nesStream vga (pix, emph) = ((pixAddr <$> vga, lineAddr <$> vga), (vgaD, rgb))
 where
  vgaD = register (vgaDecode vga640x480 (VgaCounters 0 0)) vga
  rgb = (\o p e -> if inPic o then nesColour (unpack (e ++# p)) else black8) <$> vgaD <*> pix <*> emph
  pixAddr o
    | inPic o = let x = (vgaX o - picX0) `shiftR` 1
                    y = vgaY o `shiftR` 1
                in unpack (resize (pack (resize y * 256 + resize x :: Unsigned 16)))
    | otherwise = 0
  lineAddr o
    | vgaY o < 480 = unpack (resize (pack (vgaY o `shiftR` 1)))
    | otherwise = 0
