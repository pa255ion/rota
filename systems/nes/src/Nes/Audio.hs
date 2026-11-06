{-|
The NES's sound for the board (PLAN.md step 3.9): the APU's channels
mixed through the lookup tables of "Nes.Mixer" (block-free ROMs), then the
analogue part as filters at the pixel clock: two one-pole low-passes at
about 15.6 kHz (the S/PDIF transmitter samples at 48 kHz, and the
channels have edges far above that) and a one-pole high-pass at about
15 Hz (the NES's output is 0 to 1, all above zero; the console has
high-passes at 90 and 440 Hz, nesdev wiki "APU Mixer"). The result goes
to 'Platform.Audio.Spdif.spdifDdr' as 16-bit PCM, the same on both
channels.

A one-pole filter with shift @k@ at clock @f@ has its corner at
@f / (2π 2^k)@: k = 8 at 25.175 MHz is 15.6 kHz, k = 18 is 15.3 Hz.
-}
module Nes.Audio
  ( mixChannels
  , audioFilter
  ) where

import Clash.Prelude

import Nes.Apu (Channels (..))
import Nes.Mixer (pulseTable, tndTable)

pulseLut :: Vec 31 (Unsigned 15)
pulseLut = $(listToVecTH (fmap fromIntegral pulseTable :: [Unsigned 15]))

tndLut :: Vec 203 (Unsigned 15)
tndLut = $(listToVecTH (fmap fromIntegral tndTable :: [Unsigned 15]))

-- | The mixed level, 0 to 32767.
mixChannels :: Channels -> Unsigned 15
mixChannels (Channels p1 p2 t n d) = pulseLut !! pIdx + tndLut !! tIdx
 where
  pIdx = resize p1 + resize p2 :: Unsigned 5
  tIdx = 3 * resize t + 2 * resize n + resize d :: Unsigned 8

-- | The low-passes and the high-pass, one step per clock: signed 16-bit
-- PCM out.
audioFilter :: forall dom. HiddenClockResetEnable dom => Signal dom (Unsigned 15) -> Signal dom (Signed 16)
audioFilter x = out <$> lp2 <*> dc
 where
  -- 16 fraction bits
  x16 = (\v -> resize (bitCoerce (resize v :: Unsigned 16) :: Signed 16) `shiftL` 16) <$> x :: Signal dom (Signed 40)
  lp1 = register start ((\a b -> a + ((b - a) `shiftR` 8)) <$> lp1 <*> x16)
  lp2 = register start ((\a b -> a + ((b - a) `shiftR` 8)) <$> lp2 <*> lp1)
  dc = register start ((\a b -> a + ((b - a) `shiftR` 18)) <$> dc <*> lp2)
  -- the level at power-on (the triangle holds 15, the rest 0): no thump
  start = resize (bitCoerce (resize (tndLut !! (45 :: Index 203)) :: Unsigned 16) :: Signed 16) `shiftL` 16
  out l c = let v = (l - c) `shiftR` 16
            in if v > 32767 then 32767 else if v < -32768 then -32768 else resize v
