{-|
The Master System's picture on the platform (M6 step 6.6): the core's
256 × 192 pixels (its colours stored in 6 bits: the Master System's CRAM
has 2 bits a channel, which the core hands out as 12-bit colours with the
levels 0, 5, 10, 15) in three frame buffers
('Platform.Video.FrameBuffers.tripleBuffer'), ×2 into 640 × 480 at
(64, 48), and around it the border in the backdrop's colour, as a TV
shows it (the spec's 'Gg.Spec.frameBorder', 'Gg.Screen.frameSmsVgaPng'):
taken at each frame's end. The clock enable is the Game Gear's
("Gg.Display.runEnable": the same Z80 clock).

The border is one register: it changes with the core's frame end, not with
the frame buffer the display shows (a frame ahead at most); every golden so
far keeps its backdrop.
-}
module Sms.Display
  ( smsPixelWrite
  , smsFrameBuffers
  , smsStream
  , pack6
  , unpack6
  , inSmsPic
  ) where

import Clash.Prelude

import Platform.Video.FrameBuffers (tripleBuffer)
import Platform.Video.VgaTiming

-- | A 12-bit colour (----BBBBGGGGRRRR, each channel 0, 5, 10 or 15) in 6
-- bits (--BBGGRR), and back.
pack6 :: Unsigned 12 -> BitVector 6
pack6 c = ch 8 ++# ch 4 ++# ch 0
 where
  ch :: Int -> BitVector 2
  ch s = resize (pack (((c `shiftR` s) .&. 0xF) `div` 5))

unpack6 :: BitVector 6 -> Unsigned 12
unpack6 b = (ch (slice d5 d4 b) `shiftL` 8) .|. (ch (slice d3 d2 b) `shiftL` 4) .|. ch (slice d1 d0 b)
 where
  ch :: BitVector 2 -> Unsigned 12
  ch x = 5 * resize (unpack x :: Unsigned 2)

-- | The core's pixel as a frame buffer write.
smsPixelWrite :: Maybe (Unsigned 8, Unsigned 8, Unsigned 12) -> Maybe (Index 49152, BitVector 6)
smsPixelWrite = fmap (\(x, y, c) -> (unpack (resize (pack (resize y * 256 + resize x :: Unsigned 16))), pack6 c))

smsFrameBuffers
  :: HiddenClockResetEnable dom
  => Signal dom (Maybe (Index 49152, BitVector 6))
  -> Signal dom Bool
  -> Signal dom Bool
  -> Signal dom (Index 49152)
  -> Signal dom (BitVector 6)
smsFrameBuffers = tripleBuffer (SNat @49152)

picX0, picY0 :: Unsigned 10
picX0 = 64
picY0 = 48

inSmsPic :: VgaOut -> Bool
inSmsPic o = vgaActive o && vgaX o >= picX0 && vgaX o < picX0 + 512
                         && vgaY o >= picY0 && vgaY o < picY0 + 384

fbAddr :: VgaOut -> Index 49152
fbAddr o = unpack (resize (pack (resize gy * 256 + resize gx :: Unsigned 16)))
 where
  gx = (vgaX o - picX0) `shiftR` 1
  gy = (vgaY o - picY0) `shiftR` 1

-- | The read address and the pixel stream (timing delayed by the block
-- RAM's clock): the picture, the border around it, black outside the
-- active area.
smsStream
  :: HiddenClockResetEnable dom
  => Signal dom VgaOut
  -> Signal dom (BitVector 6)
  -> Signal dom (Unsigned 12)
  -> (Signal dom (Index 49152), (Signal dom VgaOut, Signal dom Rgb888))
smsStream vga fbData border = (fbAddr <$> vga, (vgaD, rgb))
 where
  vgaD = register (vgaDecode vga640x480 (VgaCounters 0 0)) vga
  colour c = let u = pack c in Rgb444 (slice d3 d0 u) (slice d7 d4 u) (slice d11 d8 u)
  rgb = (\o c b -> fromRgb444 (if inSmsPic o then colour (unpack6 c) else if vgaActive o then colour b else black))
          <$> vgaD <*> fbData <*> border
