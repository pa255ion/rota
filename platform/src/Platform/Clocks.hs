{-|
Clock generation for the board top: 100 MHz GCLK in, pixel clock out.

'clockWizard' instantiates a Xilinx MMCM (Clock Wizard IP) and already runs
the MMCM's @locked@ output through a 'resetSynchronizer', so the returned
reset is safe to use in 'DomPix' directly. 'syncReset' is the same
synchroniser for other reset sources (for example a button).
-}
module Platform.Clocks
  ( pixelClockGen
  , pixelClockGenPs
  , syncReset
  ) where

import Clash.Explicit.Prelude
import Clash.Xilinx.ClockGen (clockWizard)

import Platform.Domains

-- | 100 MHz in, 25.175 MHz pixel clock out. The output reset is held while
-- the input reset is asserted or the MMCM has not locked yet, and is
-- deasserted synchronously to the pixel clock.
pixelClockGen :: Clock Dom100 -> Reset Dom100 -> (Clock DomPix, Reset DomPix)
pixelClockGen clkIn rstIn = clockWizard clkIn rstIn

-- | 'pixelClockGen' for a design beside the PS (the ZedBoard's counterpart
-- of "Platform.Board.ArtyZ7".artyClocksPs): the pixel-clock reset is also
-- held while the PS holds its PL reset (FCLK_RESET0_N, active low, the
-- wrapper's PS_RESETN; asynchronous). The PS releases it only after the
-- PS-PL level shifters are on, so a design's first HP0 read can't start
-- while only half the interface is there (the Game Boy's reset race,
-- found on the Arty, side track G phase 3; the ZedBoard's by side track
-- I, 2026-10-10).
pixelClockGenPs :: Clock Dom100 -> Reset Dom100 -> Signal DomPix Bit -> (Clock DomPix, Reset DomPix)
pixelClockGenPs clkIn rstIn psResetN = (clk, resetSynchronizerPs clk running)
 where
  (clk, rst) = clockWizard clkIn rstIn
  running = (&&) <$> (not <$> unsafeToActiveHigh rst) <*> (bitToBool <$> psResetN)

-- | Its own component, so that the flops' names contain
-- "resetSynchronizer" and build.tcl gives them ASYNC_REG.
resetSynchronizerPs :: Clock DomPix -> Signal DomPix Bool -> Reset DomPix
resetSynchronizerPs clk running = resetSynchronizer clk (unsafeFromActiveLow running)
{-# OPAQUE resetSynchronizerPs #-}

-- | Asynchronous assertion, synchronous deassertion (two flip-flops).
syncReset :: KnownDomain dom => Clock dom -> Reset dom -> Reset dom
syncReset = resetSynchronizer
