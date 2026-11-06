{-|
Milestone 0c top-level (PLAN.md step 7): the OLED test pattern of
"M0c.Pattern" through "Platform.Video.Oled", with the M0a LED behaviour
kept (heartbeat on LD7, SW7 selects whether LD0 to LD6 show the buttons
or the switches; see "M0a.Top"). Needs boards/zedboard/zedboard.xdc and
oled.xdc.

SW0 and SW1 flip the panel's orientation at run time, to find the right
setting on the board: SW0 on sends A0 instead of A1 (segment remap off),
SW1 on sends C0 instead of C8 (COM scan direction). Both off is Digilent's
setting. They're only synchronised, not debounced: a bounce costs at
most one frame in a mixed orientation.

Everything runs in 'DomPix', straight from the clock wizard. All outputs
are registered: the LEDs in 'ledDriver', the OLED pins in 'oledDriver'.
-}
module M0c.Top
  ( topEntity
  , m0cOled
  ) where

import Clash.Prelude

import M0a.Top (m0aLeds)
import M0c.Pattern (m0cFramebuffer)
import Platform.Clocks (pixelClockGen)
import Platform.Domains
import Platform.Input.Sync (synchronise)
import Platform.Video.Oled

-- | Pattern and driver, with the timing and the bar's step period as
-- parameters so tests can use small values. @sw@ must be synchronised.
m0cOled
  :: forall n dom
   . (HiddenClockResetEnable dom, KnownNat n, 1 <= n)
  => OledTiming -> SNat n -> Signal dom (BitVector 8) -> Signal dom OledPins
m0cOled t n sw = pins
 where
  cfg = (\s -> OledConfig (not (testBit s 0)) (not (testBit s 1))) <$> sw
  (addr, pins) = oledDriver t cfg (m0cFramebuffer n addr)

topEntity
  :: Clock Dom100
  -> Signal DomPix Bit  -- BTNC
  -> Signal DomPix Bit  -- BTND
  -> Signal DomPix Bit  -- BTNL
  -> Signal DomPix Bit  -- BTNR
  -> Signal DomPix Bit  -- BTNU
  -> Signal DomPix (BitVector 8)  -- SW
  -> ( Signal DomPix (BitVector 8)  -- LD
     , Signal DomPix Bit            -- OLED_DC
     , Signal DomPix Bit            -- OLED_RES
     , Signal DomPix Bit            -- OLED_SCLK
     , Signal DomPix Bit            -- OLED_SDIN
     , Signal DomPix Bit            -- OLED_VBAT
     , Signal DomPix Bit            -- OLED_VDD
     )
topEntity gclk c d l r u sw =
  withClockResetEnable clk rst enableGen $
    let leds = m0aLeds (SNat @(MsToCycles DomPix 10)) (SNat @(MsToCycles DomPix 500))
                 (bundle (c :> d :> l :> r :> u :> Nil)) sw
        oled = m0cOled (oledTiming @DomPix) (SNat @(MsToCycles DomPix 30))
                 (synchronise 0 sw)
    in ( leds
       , opDc <$> oled, opRes <$> oled, opSclk <$> oled
       , opSdin <$> oled, opVbat <$> oled, opVdd <$> oled
       )
 where
  -- No reset button: the MMCM's locked output (synchronised inside
  -- pixelClockGen) is the only reset.
  (clk, rst) = pixelClockGen gclk (unsafeFromActiveHigh (pure False))
{-# NOINLINE topEntity #-}
{-# ANN topEntity
  (Synthesize
    { t_name   = "m0c_top"
    , t_inputs = [ PortName "GCLK"
                 , PortName "BTNC", PortName "BTND", PortName "BTNL"
                 , PortName "BTNR", PortName "BTNU"
                 , PortName "SW" ]
    , t_output = PortProduct ""
                 [ PortName "LD"
                 , PortName "OLED_DC", PortName "OLED_RES", PortName "OLED_SCLK"
                 , PortName "OLED_SDIN", PortName "OLED_VBAT", PortName "OLED_VDD" ]
    }) #-}
