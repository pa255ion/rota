{-|
The Game Gear's sound for the board (M5 step 5.10): the PSG's output (each
channel plus or minus its volume, "Gg.Spec.Psg") through the analogue part
as filters at the pixel clock, per stereo side: two one-pole low-passes at
about 15.6 kHz and a one-pole high-pass at about 15 Hz (the coupling
capacitors). Without the low-passes the 48 kHz samplers (HDMI's audio
packets, S/PDIF) pick the PSG's square edges and noise (up to about
110 kHz) at single points, and everything above 24 kHz folds back as
noise (heard on the Arty, 2026-10-07). The same filters as the NES's
("Nes.Audio", step 3.9), here per side.

A one-pole filter with shift @k@ at clock @f@ has its corner at
@f / (2π 2^k)@: k = 8 at 25.175 MHz is 15.6 kHz, k = 18 is 15.3 Hz.
-}
module Gg.Audio
  ( audioFilter
  , filterStep
  , Filter
  , filterInit
  ) where

import Clash.Prelude

-- | One side's state: the two low-passes and the high-pass's DC estimate,
-- 16 fraction bits.
type Filter = (Signed 40, Signed 40, Signed 40)

filterInit :: Filter
filterInit = (0, 0, 0)

-- | One clock: the next state and the output (signed 16-bit PCM).
filterStep :: Filter -> Signed 16 -> (Filter, Signed 16)
filterStep (lp1, lp2, dc) x = ((lp1', lp2', dc'), out)
 where
  x40 = resize x `shiftL` 16 :: Signed 40
  lp1' = lp1 + ((x40 - lp1) `shiftR` 8)
  lp2' = lp2 + ((lp1 - lp2) `shiftR` 8)
  dc' = dc + ((lp2 - dc) `shiftR` 18)
  v = (lp2 - dc) `shiftR` 16
  out = if v > 32767 then 32767 else if v < -32768 then -32768 else resize v

-- | Both sides, one step per clock (registered output).
audioFilter :: HiddenClockResetEnable dom => Signal dom (Signed 16, Signed 16) -> Signal dom (Signed 16, Signed 16)
audioFilter = mealy step (filterInit, filterInit)
 where
  step (fl, fr) (l, r) =
    let (fl', ol) = filterStep fl l
        (fr', or') = filterStep fr r
    in ((fl', fr'), (ol, or'))
