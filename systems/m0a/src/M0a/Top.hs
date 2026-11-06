{-|
Milestone 0a top-level (PLAN.md step 5a): buttons and switches echoed on the
LEDs, plus a heartbeat proving the clock runs. No VGA ports, so the build
only needs boards/zedboard/zedboard.xdc.

LED mapping (confirmed with the user):

* LD7: heartbeat, ~1 Hz.
* SW7 off: LD0 to LD4 show BTNC, BTND, BTNL, BTNR, BTNU; LD5 and LD6 off.
* SW7 on: LD0 to LD6 show SW0 to SW6.

Buttons are synchronised and debounced (10 ms), switches synchronised.
Everything runs in 'DomPix', straight from the clock wizard.
-}
module M0a.Top
  ( topEntity
  , m0aLeds
  , ledView
  ) where

import Clash.Prelude

import Platform.Clocks (pixelClockGen)
import Platform.Domains
import Platform.Input.Debounce (syncDebounce)
import Platform.Input.Sync (synchronise)
import Platform.Output.Leds (heartbeat, ledDriver)

-- | The LED byte (LD0 = bit 0) for a heartbeat level, the buttons
-- (bit 0 = BTNC, 1 = BTND, 2 = BTNL, 3 = BTNR, 4 = BTNU) and the switches
-- (bit i = SWi).
ledView :: Bool -> BitVector 5 -> BitVector 8 -> BitVector 8
ledView hb btns sw = pack hb ++# body
 where
  body :: BitVector 7
  body | testBit sw 7 = truncateB sw
       | otherwise    = zeroExtend btns

-- | The whole design below the clock wizard, with the debounce window and
-- heartbeat half-period as parameters so tests can use small values.
-- Buttons are given in the order BTNC, BTND, BTNL, BTNR, BTNU.
m0aLeds
  :: forall db hb dom
   . (HiddenClockResetEnable dom, KnownNat db, 1 <= db, KnownNat hb, 1 <= hb)
  => SNat db
  -> SNat hb
  -> Signal dom (Vec 5 Bit)
  -> Signal dom (BitVector 8)
  -> Signal dom (BitVector 8)
m0aLeds db hbN btns sw = ledDriver (ledView <$> heartbeat hbN <*> btnBits <*> swSync)
 where
  debounced = bundle (map (syncDebounce db . fmap bitToBool) (unbundle btns))
  -- 'pack' puts element 0 in the MSB; reverse so BTNC lands in bit 0.
  btnBits   = pack . reverse . map boolToBit <$> debounced
  swSync    = synchronise 0 sw

topEntity
  :: Clock Dom100
  -> Signal DomPix Bit  -- BTNC
  -> Signal DomPix Bit  -- BTND
  -> Signal DomPix Bit  -- BTNL
  -> Signal DomPix Bit  -- BTNR
  -> Signal DomPix Bit  -- BTNU
  -> Signal DomPix (BitVector 8)  -- SW
  -> Signal DomPix (BitVector 8)  -- LD
topEntity gclk c d l r u sw =
  withClockResetEnable clk rst enableGen $
    m0aLeds (SNat @(MsToCycles DomPix 10)) (SNat @(MsToCycles DomPix 500))
      (bundle (c :> d :> l :> r :> u :> Nil)) sw
 where
  -- No reset button: the MMCM's locked output (synchronised inside
  -- pixelClockGen) is the only reset.
  (clk, rst) = pixelClockGen gclk (unsafeFromActiveHigh (pure False))
{-# NOINLINE topEntity #-}
{-# ANN topEntity
  (Synthesize
    { t_name   = "m0a_top"
    , t_inputs = [ PortName "GCLK"
                 , PortName "BTNC", PortName "BTND", PortName "BTNL"
                 , PortName "BTNR", PortName "BTNU"
                 , PortName "SW" ]
    , t_output = PortName "LD"
    }) #-}
