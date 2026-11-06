{-|
The M0a test on the Arty Z7-20 (side track G step 5): buttons and
switches echoed on the LEDs, plus a heartbeat. Same logic as "M0a.Top"
('m0aLeds'), with the Arty's 4 buttons, 2 switches and 4 LEDs mapped onto
it:

* LD4 green (RGB LED): heartbeat, ~1 Hz (M0a's LD7).
* SW1 off: LD0 to LD3 show BTN0 to BTN3.
* SW1 on: LD0 shows SW0, LD1 shows SW1 (so it's lit), LD2 and LD3 off.

SW1 plays M0a's SW7 (the view select). Buttons are debounced (10 ms),
switches synchronised. Everything runs in 'DomPix', from the Arty's MMCM
("Platform.Board.ArtyZ7"); the 5 × clock is unused here. Needs
boards/arty-z7-20/arty-z7-20.xdc and rgb_led.xdc.
-}
module M0a.ArtyTop
  ( topEntity
  , artyM0aInputs
  ) where

import Clash.Prelude

import M0a.Top (m0aLeds)
import Platform.Board.ArtyZ7 (artyClocks)
import Platform.Domains

-- | The Arty's buttons and switches as "M0a.Top"'s inputs: BTN0..BTN3 in
-- BTNC, BTND, BTNL, BTNR's places (BTNU never pressed), SW0 as SW0, SW1
-- as SW1 and as SW7.
artyM0aInputs :: BitVector 4 -> BitVector 2 -> (Vec 5 Bit, BitVector 8)
artyM0aInputs btn sw = (btns, sw8)
 where
  btns = map (btn !) (0 :> 1 :> 2 :> 3 :> Nil :: Vec 4 Int) :< 0
  sw8  = pack (sw ! (1 :: Int)) ++# (0 :: BitVector 5) ++# sw

topEntity
  :: Clock Dom125
  -> Signal DomPix (BitVector 4)  -- btn
  -> Signal DomPix (BitVector 2)  -- sw
  -> ( Signal DomPix (BitVector 4)  -- led
     , Signal DomPix Bit            -- led4_g
     )
topEntity clk125 btn sw =
  withClockResetEnable clk rst enableGen $
    let (btns, sw8) = unbundle (artyM0aInputs <$> btn <*> sw)
        leds = m0aLeds (SNat @(MsToCycles DomPix 10)) (SNat @(MsToCycles DomPix 500))
                 btns sw8
    in (slice d3 d0 <$> leds, (! (7 :: Int)) <$> leds)
 where
  -- No reset button: the MMCM's locked output (synchronised inside
  -- artyClocks) is the only reset.
  (clk, _clk5x, rst) = artyClocks clk125 (unsafeFromActiveHigh (pure False))
{-# NOINLINE topEntity #-}
{-# ANN topEntity
  (Synthesize
    { t_name   = "m0a_arty_top"
    , t_inputs = [ PortName "clk", PortName "btn", PortName "sw" ]
    , t_output = PortProduct "" [ PortName "led", PortName "led4_g" ]
    }) #-}
