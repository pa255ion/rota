{-|
LED driver: register the LED byte before it reaches the pins, select one byte
of a 32-bit debug word, and a heartbeat that proves the clock is running.
-}
module Platform.Output.Leds
  ( ledDriver
  , debugByte
  , heartbeat
  ) where

import Clash.Prelude

-- | Output register for the eight user LEDs (LD0 = bit 0). All LEDs are off
-- during reset.
ledDriver :: HiddenClockResetEnable dom => Signal dom (BitVector 8) -> Signal dom (BitVector 8)
ledDriver = register 0

-- | Byte @i@ of a debug word; byte 0 is bits 7..0.
debugByte :: Index 4 -> BitVector 32 -> BitVector 8
debugByte i w = reverse (unpack w :: Vec 4 (BitVector 8)) !! i

-- | Square wave that toggles every @n@ cycles (period @2n@ cycles). Starts
-- low after reset. @heartbeat (SNat \@(MsToCycles DomPix 500))@ blinks at
-- 1 Hz.
heartbeat
  :: forall n dom
   . (HiddenClockResetEnable dom, KnownNat n, 1 <= n)
  => SNat n -> Signal dom Bool
heartbeat _ = moore step fst (False, 0 :: Index n) (pure ())
 where
  step (b, c) ()
    | c == maxBound = (not b, 0)
    | otherwise     = (b, c + 1)
