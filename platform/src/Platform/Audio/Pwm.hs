{-|
Mono sound for the Arty Z7-20's audio jack (side track G phase 2, step
6): the board's AUD_PWM pin feeds an analog low-pass filter and a small
amplifier (enabled by AUD_SD high), so a one-bit stream whose density of
ones follows the sample plays as sound. Digilent's own out-of-box design
for the board drives both pins as plain outputs (Digilent/Arty-Z7-20-OOB,
Audio_Stream_PWM: AUD_PWM high and low, AUD_SD = 1).

'pwmStep' is a first-order delta-sigma modulator: the sample, offset to
unsigned, is added to a 16-bit accumulator every clock, and the carry out
is the pin's level. Over any window of clocks the number of ones is the
window times (sample + 32768) / 65536 within one, so silence (0) is a
steady 50 % pattern and nothing pops; at the pixel clock (25.17 MHz) the
quantisation noise sits far above the audible band, where the board's
filter removes it.
-}
module Platform.Audio.Pwm
  ( pwmStep
  , pwmOut
  ) where

import Clash.Prelude

-- | One clock: the accumulator and the sample, the new accumulator and the
-- bit for the pin.
pwmStep :: Unsigned 16 -> Signed 16 -> (Unsigned 16, Bit)
pwmStep acc s = (truncateB total, msb total)
 where
  u = unpack (pack s `xor` 0x8000) :: Unsigned 16   -- -32768 .. 32767 -> 0 .. 65535
  total = zeroExtend acc + zeroExtend u :: Unsigned 17

-- | The modulator on a sample stream; the bit is registered (0 in reset).
pwmOut :: HiddenClockResetEnable dom => Signal dom (Signed 16) -> Signal dom Bit
pwmOut = mealy (\(acc, b) s -> let (acc', b') = pwmStep acc s in ((acc', b'), b)) (0, 0)
