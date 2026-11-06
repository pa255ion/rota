{-|
Clock domains shared by every system (docs/ARCHITECTURE.md).

* 'Dom100': the ZedBoard's 100 MHz oscillator (GCLK). Only feeds the clock
  wizard, which reacts asynchronously to its reset, hence the asynchronous
  reset here.
* 'DomPix': the 640×480\@60 pixel clock. Simple systems (CHIP-8) run their
  core in this domain too, gated by clock enables. Synchronous, active-high
  reset, as Xilinx recommends.

The MMCM can only approximate 25.175 MHz, and the clock wizard is
configured from the 'DomPix' period ("Clash.Xilinx.ClockGen"), so the
period has to be one the wizard reproduces exactly. Most aren't: asking
for 39721 ps gives 39724 ps, and asking for 39724 ps gives 39721 ps. 39720 ps
is a fixed point: the wizard picks M = 31.25, D = 3, O = 41.375, i.e.
25.17623 MHz = 39720.0 ps (+45 ppm), and Vivado's clocks report shows
39.720 ns. If you change it, re-check that report.

__Arty Z7-20 (side track G).__ 'Dom125' is its 125 MHz oscillator, and
'DomPix5' the TMDS serialisers' fast clock, 5 × 'DomPix'. Both come from
one MMCM programmed directly (boards/arty-z7-20/arty_mmcm.v), not from
the clock wizard, so these periods only type the clocks: the Arty's real
pixel clock is 25.17361 MHz (39724.0 ps, 104 ppm slower than 'DomPix'
says, 55 ppm below VGA's 25.175 MHz), and Vivado times the design with the
real one. Systems keep 'DomPix' on both boards.
-}
module Platform.Domains
  ( Dom100
  , DomPix
  , DomPixDdr
  , Dom125
  , DomPix5
  , MsToCycles
  ) where

import Clash.Prelude

createDomain vSystem{vName="Dom100", vPeriod=hzToPeriod 100e6}
createDomain vXilinxSystem{vName="DomPix", vPeriod=39720}
-- Half the pixel-clock period: only the output side of a DDR output
-- register ("Clash.Xilinx.DDR.oddr") clocked by 'DomPix' lives here, e.g.
-- the HDMI pixel clock forwarded to the ADV7511 ("Platform.Board.Zedboard").
createDomain vXilinxSystem{vName="DomPixDdr", vPeriod=19860}

-- The Arty Z7-20's 125 MHz input (asynchronous reset, as 'Dom100').
createDomain vSystem{vName="Dom125", vPeriod=hzToPeriod 125e6}
-- 5 × the pixel clock: only the TMDS serialisers' fast clock input
-- ("Platform.Board.ArtyZ7.tmdsOut") lives here.
createDomain vXilinxSystem{vName="DomPix5", vPeriod=7944}

-- | Number of clock cycles of domain @dom@ in @ms@ milliseconds (rounded
-- down). For example @MsToCycles DomPix 10@ is a 10 ms debounce window.
type MsToCycles (dom :: Domain) (ms :: Nat) =
  (ms * 1000000000) `Div` DomainPeriod dom
