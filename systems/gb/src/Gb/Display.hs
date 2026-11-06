{-|
Game Boy video and timing for the 640×480 output (PLAN.md step 2.7,
docs/systems/gameboy.md "Clocking", "Frame-rate mismatch", "Display
path").

* 'dotEnable': the Game Boy's 4.194304 MHz dot clock as an enable in
  'DomPix' (25.17623 MHz): a 32-bit phase accumulator adding 715531909
  per clock, which gives 4194304.001 Hz (0.0003 ppm). Dots come every 6 or
  7 pixel clocks.
* 'frameBuffers': three 160×144×2-bit frames in one block RAM ('Banks'; the
  shared 'Platform.Video.FrameBuffers.tripleBuffer' since M3 step 3.7):
  the core draws into one, one holds the newest complete frame, and the
  display shows one, taking the newest complete frame at its own vertical
  blank. The core never draws into the frame on screen, so nothing tears;
  as the 59.73 Hz Game Boy and the 59.94 Hz display drift, a frame is
  shown twice about every 4.6 s. (Two buffers tore on the board,
  2026-10-03: after a swap the core drew into the frame still on screen.)
* 'gbStream': the picture ×3 (480×432) at (80, 24) in 640×480, black
  around it; 'palette' turns shades into colours (grey, or the DMG's
  green).
-}
module Gb.Display
  ( dotEnable
  , dotIncrement
  , frameBuffers
  , frameBuffersC
  , Banks (..)
  , banksInit
  , banksStep
  , gbStream
  , gbStreamWith
  , rgb15
  , Palette (..)
  , palette
  , div3
  , picX0
  , picY0
  ) where

import Clash.Prelude

import Platform.Video.FrameBuffers (Banks (..), banksInit, banksStep, tripleBuffer)
import Platform.Video.VgaTiming

-- | Added per pixel clock: round (4194304 / 25176233.6 × 2^32).
dotIncrement :: Unsigned 32
dotIncrement = 715531909

-- | One Game Boy dot elapsed (True for one clock, every 6 or 7 clocks).
dotEnable :: HiddenClockResetEnable dom => Signal dom Bool
dotEnable = carry
 where
  acc = register (0 :: Unsigned 33) (next <$> acc)
  next a = zeroExtend (truncateB a :: Unsigned 32) + zeroExtend dotIncrement
  carry = (`testBit` 32) <$> acc

-- | Write port from the core (pixel, shade), the core's frame-done pulse,
-- the display's vertical-blank pulse and its read address; the shade read
-- (one clock later).
frameBuffers
  :: HiddenClockResetEnable dom
  => Signal dom (Maybe (Index 23040, BitVector 2))
  -> Signal dom Bool
  -> Signal dom Bool
  -> Signal dom (Index 23040)
  -> Signal dom (BitVector 2)
frameBuffers = tripleBuffer (SNat @23040)

-- | 'frameBuffers' of the Game Boy Color's 15-bit colours.
frameBuffersC
  :: HiddenClockResetEnable dom
  => Signal dom (Maybe (Index 23040, BitVector 15))
  -> Signal dom Bool
  -> Signal dom Bool
  -> Signal dom (Index 23040)
  -> Signal dom (BitVector 15)
frameBuffersC = tripleBuffer (SNat @23040)

data Palette = Grey | Green
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFDataX

-- | Shade 0 (lightest) to 3. Grey: FFF AAA 555 000. Green: the DMG
-- screen's usual rendering, 9BBC0F 8BAC0F 306230 0F380F, in 4 bits.
palette :: Palette -> BitVector 2 -> Rgb444
palette Grey s = case s of
  0 -> Rgb444 0xF 0xF 0xF
  1 -> Rgb444 0xA 0xA 0xA
  2 -> Rgb444 0x5 0x5 0x5
  _ -> Rgb444 0x0 0x0 0x0
palette Green s = case s of
  0 -> Rgb444 0x9 0xB 0x0
  1 -> Rgb444 0x8 0xA 0x0
  2 -> Rgb444 0x3 0x6 0x3
  _ -> Rgb444 0x0 0x3 0x0

-- | @v / 3@ for @v < 1024@ (checked exhaustively by gb-unit).
div3 :: Unsigned 10 -> Unsigned 10
div3 v = resize ((resize v * 683 :: Unsigned 21) `shiftR` 11)

picX0, picY0 :: Unsigned 10
picX0 = 80
picY0 = 24

-- | Is this display pixel inside the 480×432 picture?
inPic :: VgaOut -> Bool
inPic o = vgaActive o && vgaX o >= picX0 && vgaX o < picX0 + 480
                      && vgaY o >= picY0 && vgaY o < picY0 + 432

-- | The frame buffer address for a display pixel (inside the picture).
fbAddr :: VgaOut -> Index 23040
fbAddr o = unpack (resize (pack (resize gy * 160 + resize gx :: Unsigned 15)))
 where
  gx = div3 (vgaX o - picX0)
  gy = div3 (vgaY o - picY0)

-- | The read address for the frame buffer, and the pixel stream (timing
-- delayed by the block RAM's clock, colour).
gbStream
  :: HiddenClockResetEnable dom
  => Signal dom Palette
  -> Signal dom VgaOut
  -> Signal dom (BitVector 2)                      -- ^ frame buffer data
  -> (Signal dom (Index 23040), (Signal dom VgaOut, Signal dom Rgb888))
gbStream = gbStreamWith (\p s -> fromRgb444 (palette p s))

-- | 'gbStream' with the frame buffer's pixels turned into colours by
-- @colour@ (with a setting @a@, e.g. the palette); black around the
-- picture.
gbStreamWith
  :: HiddenClockResetEnable dom
  => (a -> BitVector w -> Rgb888)
  -> Signal dom a
  -> Signal dom VgaOut
  -> Signal dom (BitVector w)                      -- ^ frame buffer data
  -> (Signal dom (Index 23040), (Signal dom VgaOut, Signal dom Rgb888))
gbStreamWith colour set vga fbData = (fbAddr <$> vga, (vgaD, rgb))
 where
  vgaD = register (vgaDecode vga640x480 (VgaCounters 0 0)) vga
  rgb = (\o p s -> if inPic o then colour p s else black8) <$> vgaD <*> set <*> fbData

-- | A Game Boy Color colour (15-bit BGR: red in bits 0 to 4) as 24-bit
-- RGB, each channel @(c << 3) | (c >> 2)@ (Gb.Screen.rgb8, cgb-acid2's
-- reference); no colour correction (phase 3 step 5: off by default).
rgb15 :: BitVector 15 -> Rgb888
rgb15 c = Rgb888 (ch (slice d4 d0 c)) (ch (slice d9 d5 c)) (ch (slice d14 d10 c))
 where
  ch v = v ++# slice d4 d2 v
