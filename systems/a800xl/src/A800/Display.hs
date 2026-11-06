{-|
800XL video for the 640×480 output (M4 step 4.7; docs/systems/atari800xl.md
"Clocking", "Display").

* 'a800CycleEnable': the machine cycle (1.7897725 MHz, the colour clock
  3.579545 MHz / 2) as an enable in 'DomPix' (25.17623 MHz): a 32-bit
  phase accumulator adding 305328211 per clock, so a cycle every 14 or 15
  clocks ("A800.Core" needs at most 13).
* 'a800FrameBuffers': three frames ('Platform.Video.FrameBuffers'), each
  the normal playfield's 160 colour clocks ($30-$CF) × 240 lines (8-247),
  one 16-bit word per colour clock: its two half-clock colours (hires
  pixels), as "A800.Picture" crops the spec's frame.
* 'a800Stream': the 320 × 240 picture ×2, filling 640 × 480; 'a800Colour'
  looks the colour byte up in the palette ROM ("A800.Palette").
-}
module A800.Display
  ( a800CycleIncrement
  , a800CycleEnable
  , a800FrameBuffers
  , a800Stream
  , a800Colour
  , paletteRom
  ) where

import Clash.Prelude

import A800.Palette (paletteWords)
import Platform.Video.FrameBuffers (tripleBuffer)
import Platform.Video.VgaTiming

-- | Added per pixel clock: round (1789772.5 / 25176233.6 × 2^32).
a800CycleIncrement :: Unsigned 32
a800CycleIncrement = 305328211

-- | One machine cycle elapsed (True for one clock).
a800CycleEnable :: HiddenClockResetEnable dom => Signal dom Bool
a800CycleEnable = carry
 where
  acc = register (0 :: Unsigned 33) (next <$> acc)
  next a = zeroExtend (truncateB a :: Unsigned 32) + zeroExtend a800CycleIncrement
  carry = (`testBit` 32) <$> acc

paletteRom :: Vec 256 (BitVector 24)
paletteRom = $(listToVecTH (fmap fromIntegral paletteWords :: [BitVector 24]))

a800Colour :: Unsigned 8 -> Rgb888
a800Colour i = Rgb888 (slice d23 d16 w) (slice d15 d8 w) (slice d7 d0 w)
 where w = paletteRom !! i

-- | Write port from the core ('A800.Core.koPixel': line, colour clock,
-- its two halves), the core's frame-done pulse, the display's
-- vertical-blank pulse, the read address (colour clock word); the word
-- (one clock later).
a800FrameBuffers
  :: HiddenClockResetEnable dom
  => Signal dom (Maybe (Unsigned 9, Unsigned 8, (Unsigned 8, Unsigned 8)))
  -> Signal dom Bool
  -> Signal dom Bool
  -> Signal dom (Index 38400)
  -> Signal dom (BitVector 16)
a800FrameBuffers px frameDone vblank rd = tripleBuffer (SNat @38400) ((toWord =<<) <$> px) frameDone vblank rd
 where
  toWord (y, h, (a, b))
    | y >= 8 && y < 248 && h >= 0x30 && h < 0xD0 =
        Just (unpack (pack (resize (y - 8) * 160 + resize (h - 0x30) :: Unsigned 16)), pack a ++# pack b)
    | otherwise = Nothing

-- | @a800Stream vga words@: the read address for this clock's VGA
-- position, and the timing with its colour one clock later, as the frame
-- buffers answer.
a800Stream
  :: HiddenClockResetEnable dom
  => Signal dom VgaOut
  -> Signal dom (BitVector 16)
  -> (Signal dom (Index 38400), (Signal dom VgaOut, Signal dom Rgb888))
a800Stream vga word = (addr <$> vga, (vgaD, rgb))
 where
  vgaD = register (vgaDecode vga640x480 (VgaCounters 0 0)) vga
  rgb = (\o w -> if inPic o then a800Colour (unpack (half o w)) else black8) <$> vgaD <*> word
  inPic o = vgaActive o && vgaX o < 640 && vgaY o < 480
  -- pixel x (0-319) is half x mod 2 of colour clock x / 2; each ×2
  half o w = if testBit (vgaX o) 1 then slice d7 d0 w else slice d15 d8 w
  addr o
    | inPic o = let cc = vgaX o `shiftR` 2
                    y = vgaY o `shiftR` 1
                in unpack (resize (pack (resize y * 160 + resize cc :: Unsigned 16)))
    | otherwise = 0
