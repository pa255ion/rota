{-|
Arty Z7-20-specific pin logic (side track G), the counterpart of
"Platform.Board.Zedboard": what sits between the board-agnostic platform
streams and the pins of boards/arty-z7-20/*.xdc.

__Clocks ('artyClocks', step 3).__ One MMCM makes the pixel clock and
5 × the pixel clock from the 125 MHz oscillator: M = 18.125, D = 2,
O0 = 45, O1 = 9, i.e. 25.17361 and 125.86806 MHz from a 1132.8 MHz VCO.
The two must be exactly 1:5 and phase-aligned for the serialisers, which
Clash's clockWizard doesn't guarantee (it asks for each frequency on its
own), so the MMCM is the hand-written boards/arty-z7-20/arty_mmcm.v
behind a blackbox; the reasoning and the wizard's own choice are in that
file. The clock types are 'DomPix' and 'DomPix5'; their periods are
nominal ("Platform.Domains").

__HDMI ('tmdsOut', step 4).__ The connector's TMDS pairs are FPGA pins
(TMDS_33). "Platform.Video.Tmds" makes the three channels' 10-bit
symbols; boards/arty-z7-20/tmds_out.v serialises them, bit 0 first, with
an OSERDESE2 master/slave pair per channel (DDR at 5 × the pixel clock),
adds the clock channel and drives the OBUFDS buffers. The OSERDESE2
outputs are registers in the IOB, so the pins are registered as CLAUDE.md
asks.

Both blackboxes instantiate the hand-written modules by name: build.tcl
reads them through BOARD_WRAPPER (systems/*/arty-z7-20.mk), Verilator
reads the simulation stand-ins of the same names in boards/sim/. Their
Haskell models only exist so that tops type-check and elaborate; nothing
simulates an Arty top at layer 3 (layer 4 runs the Verilog).
-}
module Platform.Board.ArtyZ7
  ( -- * Clocks
    artyMmcm
  , artyClocks
  , artyClocksPs
    -- * HDMI
  , TmdsPins (..)
  , tmdsOut
  , artyCts
  ) where

import Clash.Explicit.Prelude
import Clash.Annotations.Primitive (HDL (..), Primitive (..), hasBlackBox)

import Platform.Domains

-- | The MMCM (boards/arty-z7-20/arty_mmcm.v): pixel clock, 5 × pixel
-- clock and the asynchronous @locked@ output. Use 'artyClocks'.
artyMmcm :: Clock Dom125 -> Reset Dom125 -> (Clock DomPix, Clock DomPix5, Signal DomPix Bool)
artyMmcm !_ !_ = (clockGen, clockGen, pure True)
{-# OPAQUE artyMmcm #-}
{-# ANN artyMmcm hasBlackBox #-}
{-# ANN artyMmcm (InlineYamlPrimitive [Verilog] (unlines
  [ "BlackBox:"
  , "  name: Platform.Board.ArtyZ7.artyMmcm"
  , "  kind: Declaration"
  , "  template: |-"
  , "    // arty_mmcm begin (boards/arty-z7-20/arty_mmcm.v)"
  , "    wire ~GENSYM[clk_pix][0];"
  , "    wire ~GENSYM[clk_5x][1];"
  , "    wire ~GENSYM[locked][2];"
  , "    arty_mmcm ~GENSYM[arty_mmcm_inst][3]"
  , "      ( .clk_in (~ARG[0])"
  , "      , .reset (~ARG[1])"
  , "      , .clk_pix (~SYM[0])"
  , "      , .clk_5x (~SYM[1])"
  , "      , .locked (~SYM[2])"
  , "      );"
  , "    assign ~RESULT = {~SYM[0], ~SYM[1], ~SYM[2]};"
  , "    // arty_mmcm end"
  ])) #-}

-- | 'artyMmcm' with its @locked@ output run through a reset synchroniser:
-- the pixel-clock reset is held until the MMCM has locked and released
-- synchronously, as "Platform.Clocks".pixelClockGen does on the ZedBoard.
artyClocks :: Clock Dom125 -> Reset Dom125 -> (Clock DomPix, Clock DomPix5, Reset DomPix)
artyClocks clkIn rstIn = (pix, x5, resetSynchronizerArty pix locked)
 where
  (pix, x5, locked) = artyMmcm clkIn rstIn

-- | 'artyClocks' for a design beside the PS: the pixel-clock reset is also
-- held while the PS holds its PL reset (FCLK_RESET0_N, active low, the
-- wrappers' PS_RESETN; asynchronous, like @locked@). The PS releases it
-- only after the PS-PL level shifters are on, so nothing the design does
-- at start-up (the Game Boy's first HP0 read) happens while only half the
-- interface is there (side track G phase 3; boards/arty-z7-20/ps7.tcl).
artyClocksPs
  :: Clock Dom125 -> Reset Dom125 -> Signal DomPix Bit
  -> (Clock DomPix, Clock DomPix5, Reset DomPix)
artyClocksPs clkIn rstIn psResetN = (pix, x5, resetSynchronizerArty pix ((&&) <$> locked <*> (bitToBool <$> psResetN)))
 where
  (pix, x5, locked) = artyMmcm clkIn rstIn

-- | Its own component, so that the flops' names contain
-- "resetSynchronizer" and build.tcl gives them ASYNC_REG, as it does to
-- the clock wizard's on the ZedBoard.
resetSynchronizerArty :: Clock DomPix -> Signal DomPix Bool -> Reset DomPix
resetSynchronizerArty pix locked = resetSynchronizer pix (unsafeFromActiveLow locked)
{-# OPAQUE resetSynchronizerArty #-}

-- | CTS for HDMI audio at 48 kHz with N = 6144 (side track G phase 3):
-- f_TMDS x N / (128 x fs) = 25 173 611 x 6144 / 6 144 000 = 25 173.6 for
-- the Arty's real pixel clock (arty_mmcm.v); rounded to 25174.
-- "Platform.Video.Hdmi" takes its 48 kHz strobe from the same N and CTS,
-- so the sink's rebuilt rate (47 999.2 Hz) is the samples' rate.
artyCts :: Unsigned 20
artyCts = 25174

-- | The HDMI TX pins, in the order of the top-level ports.
data TmdsPins = TmdsPins
  { tpClkP :: Bit           -- ^ hdmi_tx_clk_p
  , tpClkN :: Bit           -- ^ hdmi_tx_clk_n
  , tpDP   :: BitVector 3   -- ^ hdmi_tx_d_p[2:0]
  , tpDN   :: BitVector 3   -- ^ hdmi_tx_d_n[2:0]
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFDataX

-- | The serialisers and output buffers (boards/arty-z7-20/tmds_out.v):
-- channel 0, 1, 2's symbols (blue, green, red; "Platform.Video.Tmds"
-- .tmdsVideo) in, the pins out. The reset must be synchronous to the
-- pixel clock and held until both clocks run ('artyClocks' gives one).
tmdsOut
  :: Clock DomPix -> Clock DomPix5 -> Reset DomPix
  -> Signal DomPix (Vec 3 (BitVector 10))
  -> Signal DomPix TmdsPins
tmdsOut !_ !_ !_ syms = idle <$> syms
 where
  -- The Haskell model is only a placeholder (see the module header).
  idle _ = TmdsPins 0 1 0 7
{-# OPAQUE tmdsOut #-}
{-# ANN tmdsOut hasBlackBox #-}
{-# ANN tmdsOut (InlineYamlPrimitive [Verilog] (unlines
  [ "BlackBox:"
  , "  name: Platform.Board.ArtyZ7.tmdsOut"
  , "  kind: Declaration"
  , "  template: |-"
  , "    // tmds_out begin (boards/arty-z7-20/tmds_out.v)"
  , "    wire [29:0] ~GENSYM[syms][0];"
  , "    wire [3:0] ~GENSYM[p][1];"
  , "    wire [3:0] ~GENSYM[n][2];"
  , "    assign ~SYM[0] = ~ARG[3];"
  , "    tmds_out ~GENSYM[tmds_out_inst][3]"
  , "      ( .clk_pix (~ARG[0])"
  , "      , .clk_5x (~ARG[1])"
  , "      , .rst (~ARG[2])"
  , "      , .d0 (~SYM[0][29:20])"
  , "      , .d1 (~SYM[0][19:10])"
  , "      , .d2 (~SYM[0][9:0])"
  , "      , .p (~SYM[1])"
  , "      , .n (~SYM[2])"
  , "      );"
  , "    assign ~RESULT = {~SYM[1][3], ~SYM[2][3], ~SYM[1][2:0], ~SYM[2][2:0]};"
  , "    // tmds_out end"
  ])) #-}
