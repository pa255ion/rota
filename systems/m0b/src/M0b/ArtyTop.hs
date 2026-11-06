{-|
The M0b test pattern on the Arty Z7-20's HDMI output (side track G step
5), with the Arty's M0a LED behaviour ("M0a.ArtyTop") kept.

The pattern is "M0b.Pattern"'s stream, at full 8-bit colour (each 4-bit
channel widened, c × 17, as on the ZedBoard's HDMI), encoded by
"Platform.Video.Tmds" and serialised by "Platform.Board.ArtyZ7".tmdsOut.
TMDS is lossless, so the harness's received frames must equal the M0b
layer 3 goldens exactly (make sim SYSTEM=m0b BOARD=arty-z7-20). Needs
boards/arty-z7-20/arty-z7-20.xdc, rgb_led.xdc and hdmi_tx.xdc.
-}
module M0b.ArtyTop
  ( topEntity
  ) where

import Clash.Prelude

import M0a.ArtyTop (artyM0aInputs)
import M0a.Top (m0aLeds)
import M0b.Pattern (m0bStream)
import Platform.Board.ArtyZ7 (TmdsPins (..), artyClocks, tmdsOut)
import Platform.Domains
import Platform.Video.Tmds (tmdsVideo)

topEntity
  :: Clock Dom125
  -> Signal DomPix (BitVector 4)  -- btn
  -> Signal DomPix (BitVector 2)  -- sw
  -> ( Signal DomPix (BitVector 4)  -- led
     , Signal DomPix Bit            -- led4_g
     , Signal DomPix TmdsPins       -- hdmi_tx_*
     )
topEntity clk125 btn sw = (slice d3 d0 <$> leds, (! (7 :: Int)) <$> leds, pins)
 where
  (clk, clk5x, rst) = artyClocks clk125 (unsafeFromActiveHigh (pure False))
  leds = withClockResetEnable clk rst enableGen $
    let (btns, sw8) = unbundle (artyM0aInputs <$> btn <*> sw)
    in m0aLeds (SNat @(MsToCycles DomPix 10)) (SNat @(MsToCycles DomPix 500)) btns sw8
  syms = withClockResetEnable clk rst enableGen $ uncurry tmdsVideo m0bStream
  pins = tmdsOut clk clk5x rst syms
{-# NOINLINE topEntity #-}
{-# ANN topEntity
  (Synthesize
    { t_name   = "m0b_arty_top"
    , t_inputs = [ PortName "clk", PortName "btn", PortName "sw" ]
    , t_output = PortProduct ""
                 [ PortName "led", PortName "led4_g"
                 , PortProduct ""
                     [ PortName "hdmi_tx_clk_p", PortName "hdmi_tx_clk_n"
                     , PortName "hdmi_tx_d_p", PortName "hdmi_tx_d_n" ] ]
    }) #-}
