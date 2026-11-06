{-|
Milestone 0d top-level (PLAN.md M0d step 5): the M0b test pattern (colour
bars, moving square) on VGA /and/ HDMI, the HDMI side entirely in our own
gateware: "Platform.Video.Ycbcr422" for the 4:2:2 bus, "Platform.Board.Zedboard"
for the pins and the forwarded clock, "Platform.Video.Adv7511" setting up the
ADV7511 over I2C. Needs boards/zedboard/zedboard.xdc, vga.xdc and hdmi.xdc.

__LEDs__ (the ADV7511 status, 'statusWord'): SW0 off: LD0 configured, LD1
HPD, LD2 monitor sense, LD3 I2C error, LD5..LD4 which error (00 address
NACK, 01 register NACK, 10 data NACK, 11 timeout), LD6 toggles with every
completed configuration, LD7 heartbeat (1 Hz). SW0 on: the chip revision
read over I2C. The buttons and SW1 to SW7 are unused (zedboard.xdc
constrains them, so the ports exist).

Everything runs in 'DomPix'; HD_CLK is that clock forwarded through an
ODDR. All outputs are registered: LEDs in 'ledDriver', VGA in 'vgaPins',
HDMI in 'hdmiOut', the I2C lines straight from the master's registers.
-}
module M0d.Top
  ( topEntity
  , m0dLeds
  ) where

import Clash.Prelude

import M0b.Pattern (m0bStream)
import Platform.Board.Zedboard
import Platform.Bus.I2c (I2cLines (..))
import Platform.Clocks (pixelClockGen)
import Platform.Domains
import Platform.Input.Sync (synchronise)
import Platform.Output.Leds (debugByte, heartbeat, ledDriver)
import Platform.Video.Adv7511
import Platform.Video.VgaTiming
import Platform.Video.Ycbcr422

-- | The LED byte: status bits 6..0 and the heartbeat, or the revision.
m0dLeds :: Bool -> Bool -> AdvStatus -> BitVector 8
m0dLeds showRevision hb st
  | showRevision = debugByte 1 w
  | otherwise    = pack hb ++# slice d6 d0 (debugByte 0 w)
 where
  w = zeroExtend (statusWord st) :: BitVector 32

topEntity
  :: Clock Dom100
  -> Signal DomPix Bit  -- BTNC
  -> Signal DomPix Bit  -- BTND
  -> Signal DomPix Bit  -- BTNL
  -> Signal DomPix Bit  -- BTNR
  -> Signal DomPix Bit  -- BTNU
  -> Signal DomPix (BitVector 8)  -- SW
  -> BiSignalIn 'Floating DomPix 1  -- HD_SCL
  -> BiSignalIn 'Floating DomPix 1  -- HD_SDA
  -> ( Signal DomPix (BitVector 8)  -- LD
     , Signal DomPix (BitVector 4)  -- VGA_R
     , Signal DomPix (BitVector 4)  -- VGA_G
     , Signal DomPix (BitVector 4)  -- VGA_B
     , Signal DomPix Bit            -- VGA_HS
     , Signal DomPix Bit            -- VGA_VS
     , Signal DomPixDdr Bit         -- HD_CLK
     , Signal DomPix (BitVector 16) -- HD_D
     , Signal DomPix Bit            -- HD_DE
     , Signal DomPix Bit            -- HD_HSYNC
     , Signal DomPix Bit            -- HD_VSYNC
     , BiSignalOut 'Floating DomPix 1  -- HD_SCL
     , BiSignalOut 'Floating DomPix 1  -- HD_SDA
     )
topEntity gclk _ _ _ _ _ sw sclPin sdaPin =
  ( leds
  , rgbR . vpRgb <$> vga, rgbG . vpRgb <$> vga, rgbB . vpRgb <$> vga
  , vpHSync <$> vga, vpVSync <$> vga
  , hdClk, hpData <$> hdmi, hpDe <$> hdmi, hpHSync <$> hdmi, hpVSync <$> hdmi
  , sclOut, sdaOut
  )
 where
  -- No reset button: the MMCM's locked output (synchronised inside
  -- pixelClockGen) is the only reset.
  (clk, rst) = pixelClockGen gclk (unsafeFromActiveHigh (pure False))

  (timing, rgb) = withClockResetEnable clk rst enableGen m0bStream
  vga = withClockResetEnable clk rst enableGen $ vgaPins vga640x480 timing rgb

  -- HDMI: the same stream, black outside the picture, as 4:2:2
  px = (\o c -> PixelIn (vgaHSync o) (vgaVSync o) (vgaActive o) (if vgaActive o then c else black8))
         <$> timing <*> rgb
  ycc = withClockResetEnable clk rst enableGen $ ycbcr422 1 1 px
  (hdmi, hdClk) = hdmiOut clk rst enableGen 1 1 ycc

  -- ADV7511 set-up over I2C
  (sclOut, sclIn) = openDrain sclPin (fst <$> i2cPins)
  (sdaOut, sdaIn) = openDrain sdaPin (snd <$> i2cPins)
  (i2cPins, status) = withClockResetEnable clk rst enableGen $
    adv7511Init (advTiming @DomPix) (I2cLines <$> sclIn <*> sdaIn)

  leds = withClockResetEnable clk rst enableGen $
    let sw0 = synchronise False (testBit <$> sw <*> pure 0)
        hb  = heartbeat (SNat @(MsToCycles DomPix 500))
    in ledDriver (m0dLeds <$> sw0 <*> hb <*> status)
{-# NOINLINE topEntity #-}
{-# ANN topEntity
  (Synthesize
    { t_name   = "m0d_top"
    , t_inputs = [ PortName "GCLK"
                 , PortName "BTNC", PortName "BTND", PortName "BTNL"
                 , PortName "BTNR", PortName "BTNU"
                 , PortName "SW"
                 , PortName "HD_SCL", PortName "HD_SDA" ]
    , t_output = PortProduct ""
                 [ PortName "LD"
                 , PortName "VGA_R", PortName "VGA_G", PortName "VGA_B"
                 , PortName "VGA_HS", PortName "VGA_VS"
                 , PortName "HD_CLK", PortName "HD_D", PortName "HD_DE"
                 , PortName "HD_HSYNC", PortName "HD_VSYNC"
                 , PortName "HD_SCL", PortName "HD_SDA" ]
    }) #-}
