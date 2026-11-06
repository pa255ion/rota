{-|
The Game Gear's picture and clock on the platform (PLAN.md step 5.8): the
clock enable that gives "Gg.Core" its twelve active clocks per two
T-states from the 25.175 MHz pixel clock, the three frame buffers
('Platform.Video.FrameBuffers.tripleBuffer'), and the 160 × 144 window
×3 into 640 × 480 at (80, 24) with black around it, as the Game Boy's
(the same LCD size; "Gb.Display").

The Game Gear's frame rate (59.92 Hz) and the display's (59.94 Hz) differ
a little; the triple buffer drops a frame now and then.
-}
module Gg.Display
  ( runIncrement
  , runEnable
  , pixelWrite
  , frameBuffers
  , ggStream
  , div3
  , inPic
  , fbAddr
  ) where

import Clash.Prelude

import Platform.Video.FrameBuffers (tripleBuffer)
import Platform.Video.VgaTiming

-- | Added per pixel clock: round (6 × 3579545 / 25176233.6 × 2^32): the
-- core's active clocks (twelve per two T-states).
runIncrement :: Unsigned 32
runIncrement = 3663938527

-- | An active clock for the core (about 85 % of clocks).
runEnable :: HiddenClockResetEnable dom => Signal dom Bool
runEnable = carry
 where
  acc = register (0 :: Unsigned 33) (next <$> acc)
  next a = zeroExtend (truncateB a :: Unsigned 32) + zeroExtend runIncrement
  carry = (`testBit` 32) <$> acc

-- | The core's window pixel as a frame buffer write.
pixelWrite :: Maybe (Unsigned 8, Unsigned 8, Unsigned 12) -> Maybe (Index 23040, BitVector 12)
pixelWrite = fmap (\(x, y, c) -> (unpack (resize (pack (resize y * 160 + resize x :: Unsigned 15))), pack c))

-- | The core's writes and frame-done pulse, the display's vertical blank
-- and read address; the colour read (one clock later).
frameBuffers
  :: HiddenClockResetEnable dom
  => Signal dom (Maybe (Index 23040, BitVector 12))
  -> Signal dom Bool
  -> Signal dom Bool
  -> Signal dom (Index 23040)
  -> Signal dom (BitVector 12)
frameBuffers = tripleBuffer (SNat @23040)

-- | @v / 3@ for @v < 1024@.
div3 :: Unsigned 10 -> Unsigned 10
div3 v = resize ((resize v * 683 :: Unsigned 21) `shiftR` 11)

picX0, picY0 :: Unsigned 10
picX0 = 80
picY0 = 24

inPic :: VgaOut -> Bool
inPic o = vgaActive o && vgaX o >= picX0 && vgaX o < picX0 + 480
                      && vgaY o >= picY0 && vgaY o < picY0 + 432

fbAddr :: VgaOut -> Index 23040
fbAddr o = unpack (resize (pack (resize gy * 160 + resize gx :: Unsigned 15)))
 where
  gx = div3 (vgaX o - picX0)
  gy = div3 (vgaY o - picY0)

-- | The read address, and the pixel stream (timing delayed by the block
-- RAM's clock; a CRAM colour, ----BBBBGGGGRRRR, widened to 8 bits).
ggStream
  :: HiddenClockResetEnable dom
  => Signal dom VgaOut
  -> Signal dom (BitVector 12)
  -> (Signal dom (Index 23040), (Signal dom VgaOut, Signal dom Rgb888))
ggStream vga fbData = (fbAddr <$> vga, (vgaD, rgb))
 where
  vgaD = register (vgaDecode vga640x480 (VgaCounters 0 0)) vga
  colour c = Rgb444 (slice d3 d0 c) (slice d7 d4 c) (slice d11 d8 c)
  rgb = (\o c -> fromRgb444 (if inPic o then colour c else black)) <$> vgaD <*> fbData
