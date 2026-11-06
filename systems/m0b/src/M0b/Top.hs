{-|
Milestone 0b top-level (PLAN.md step 5b): the VGA test pattern of
"M0b.Pattern", with the M0a LED behaviour kept (heartbeat on LD7, SW7
selects whether LD0 to LD6 show the buttons or the switches; see
"M0a.Top"). Needs boards/zedboard/zedboard.xdc and vga.xdc.

Everything runs in 'DomPix', straight from the clock wizard. All outputs
are registered: the LEDs in 'ledDriver', the VGA pins in 'vgaPins'.
-}
module M0b.Top
  ( topEntity
  ) where

import Clash.Prelude

import M0a.Top (m0aLeds)
import M0b.Pattern (m0bVideo)
import Platform.Clocks (pixelClockGen)
import Platform.Domains
import Platform.Video.VgaTiming

topEntity
  :: Clock Dom100
  -> Signal DomPix Bit  -- BTNC
  -> Signal DomPix Bit  -- BTND
  -> Signal DomPix Bit  -- BTNL
  -> Signal DomPix Bit  -- BTNR
  -> Signal DomPix Bit  -- BTNU
  -> Signal DomPix (BitVector 8)  -- SW
  -> ( Signal DomPix (BitVector 8)  -- LD
     , Signal DomPix (BitVector 4)  -- VGA_R
     , Signal DomPix (BitVector 4)  -- VGA_G
     , Signal DomPix (BitVector 4)  -- VGA_B
     , Signal DomPix Bit            -- VGA_HS
     , Signal DomPix Bit            -- VGA_VS
     )
topEntity gclk c d l r u sw =
  withClockResetEnable clk rst enableGen $
    let leds = m0aLeds (SNat @(MsToCycles DomPix 10)) (SNat @(MsToCycles DomPix 500))
                 (bundle (c :> d :> l :> r :> u :> Nil)) sw
        vga  = m0bVideo
    in ( leds
       , rgbR . vpRgb <$> vga
       , rgbG . vpRgb <$> vga
       , rgbB . vpRgb <$> vga
       , vpHSync <$> vga
       , vpVSync <$> vga
       )
 where
  -- No reset button: the MMCM's locked output (synchronised inside
  -- pixelClockGen) is the only reset.
  (clk, rst) = pixelClockGen gclk (unsafeFromActiveHigh (pure False))
{-# NOINLINE topEntity #-}
{-# ANN topEntity
  (Synthesize
    { t_name   = "m0b_top"
    , t_inputs = [ PortName "GCLK"
                 , PortName "BTNC", PortName "BTND", PortName "BTNL"
                 , PortName "BTNR", PortName "BTNU"
                 , PortName "SW" ]
    , t_output = PortProduct ""
                 [ PortName "LD"
                 , PortName "VGA_R", PortName "VGA_G", PortName "VGA_B"
                 , PortName "VGA_HS", PortName "VGA_VS" ]
    }) #-}
