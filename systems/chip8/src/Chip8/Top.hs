{-|
CHIP-8 top-level for the ZedBoard (PLAN.md step 1.6). Needs
boards/zedboard/zedboard.xdc, vga.xdc, oled.xdc, pmod_ja1.xdc and hdmi.xdc.

* __Program__ (loader stage A): main RAM is a 'blockRamFile' initialised
  from 'ramImage', which @make hdl SYSTEM=chip8 ROM=…@ writes (font +
  ROM, see @app/RamImage.hs@) before running Clash. Clash copies the file
  next to the generated Verilog, which loads it with @$readmemb@.
* __Switches__ (docs/systems/chip8.md): SW0 to SW4 quirks (0 = VIP), SW5
  instruction rate (off 700 Hz, on 10 kHz), SW6 which debug byte the LEDs
  show (off: bits 7..0 of @coDebug@ = PC low byte; on: bits 15..8 = PC
  high nibble and the halted / key-wait / vblank-wait flags), SW7 OLED mode
  (off: stretch, on: 1:1).
* __Buttons__: the keys of 'buttonKeys', debounced 10 ms.
* __From the PS__ (@PS_GPIO@, 32 EMIO GPIO lines, the PS link of
  "Platform.Ps.Link"; CHIP-8 address map in "Chip8.Loader"): keys from a
  desktop keyboard (ORed with the button keys), ROM loading (over J14 or
  from the SD card), the menu's text overlay and the pause/overlay control
  register, all from the PS app (@platform/ps/@). Asynchronous to
  'DomPix', so every bit is synchronised on its own (keys are independent;
  the link's data settles long before its strobe). Only connected in the
  Zynq wrapper (@boards/zedboard/chip8_zynq.v@); simulation leaves it at 0
  (no keys, no hold, overlay hidden) unless the harness plays the PS
  (@--ps@, platform/sim/).
* __To the PS__ (@PS_IN@, EMIO GPIO 32 to 63, 'Platform.Ps.Link.psIn'):
  the debounced buttons, the switches and 'systemChip8', for the menu.
  While the PS pauses the core (menu open) the keys don't reach it.
* __Video__: VGA 640×480 (×10, letterboxed, amber) and the OLED, both from
  the core's framebuffer, and HDMI from the same pixel stream as VGA
  (PLAN.md M0d step 6: "Platform.Video.Ycbcr422", "Platform.Board.Zedboard",
  the ADV7511 set up by "Platform.Video.Adv7511"; its status isn't shown,
  the M0d top is the place to diagnose HDMI). The 60 Hz timer is the VGA
  vertical blank.
* __Sound__: 440 Hz on Pmod JA1 while the sound timer runs.

Everything runs in 'DomPix'; all outputs are registered.
-}
module Chip8.Top
  ( topEntity
  , ramImage
  ) where

import Clash.Prelude

import Chip8.Core
import Chip8.Display
import Chip8.Loader (ramWrite)
import Chip8.Peripherals
import Platform.Board.Zedboard
import Platform.Bus.I2c (I2cLines (..))
import Platform.Clocks (pixelClockGen)
import Platform.Domains
import Platform.Input.Sync (synchronise)
import Platform.Output.Leds (debugByte, ledDriver)
import Platform.Ps.Link
import Platform.Video.Adv7511 (adv7511Init, advTiming)
import Platform.Video.Oled
import Platform.Video.TextOverlay (textOverlay)
import Platform.Video.VgaTiming
import Platform.Video.Ycbcr422

-- | The RAM image, relative to the directory @clash@ runs in (the repository
-- root, through the Makefile).
ramImage :: FilePath
ramImage = "build/chip8/ram.bin"

topEntity
  :: Clock Dom100
  -> Signal DomPix Bit  -- BTNC
  -> Signal DomPix Bit  -- BTND
  -> Signal DomPix Bit  -- BTNL
  -> Signal DomPix Bit  -- BTNR
  -> Signal DomPix Bit  -- BTNU
  -> Signal DomPix (BitVector 8)  -- SW
  -> Signal DomPix (BitVector 32) -- PS_GPIO
  -> BiSignalIn 'Floating DomPix 1  -- HD_SCL
  -> BiSignalIn 'Floating DomPix 1  -- HD_SDA
  -> ( Signal DomPix (BitVector 8)  -- LD
     , Signal DomPix (BitVector 4)  -- VGA_R
     , Signal DomPix (BitVector 4)  -- VGA_G
     , Signal DomPix (BitVector 4)  -- VGA_B
     , Signal DomPix Bit            -- VGA_HS
     , Signal DomPix Bit            -- VGA_VS
     , Signal DomPix Bit            -- OLED_DC
     , Signal DomPix Bit            -- OLED_RES
     , Signal DomPix Bit            -- OLED_SCLK
     , Signal DomPix Bit            -- OLED_SDIN
     , Signal DomPix Bit            -- OLED_VBAT
     , Signal DomPix Bit            -- OLED_VDD
     , Signal DomPix Bit            -- JA1 (beeper)
     , Signal DomPixDdr Bit         -- HD_CLK
     , Signal DomPix (BitVector 16) -- HD_D
     , Signal DomPix Bit            -- HD_DE
     , Signal DomPix Bit            -- HD_HSYNC
     , Signal DomPix Bit            -- HD_VSYNC
     , BiSignalOut 'Floating DomPix 1  -- HD_SCL
     , BiSignalOut 'Floating DomPix 1  -- HD_SDA
     , Signal DomPix (BitVector 32) -- PS_IN
     )
topEntity gclk c d l r u swRaw psRaw sclPin sdaPin =
  withClockResetEnable clk rst enableGen $
    let sw    = synchronise 0 swRaw
        ps    = psGpio <$> synchronise 0 psRaw
        link  = psLink ps
        ctrl  = psControl link
        pause = pcPause <$> ctrl
        load  = ramWrite <$> (pgHold <$> ps) <*> link
        btns  = debouncedButtons (SNat @(MsToCycles DomPix 10)) (bundle (c :> d :> l :> r :> u :> Nil))
        keys  = (\p b k -> if p then 0 else keysOf b .|. k) <$> pause <*> btns <*> (pgUser <$> ps)
        psOut = register 0 (psIn systemChip8 <$> btns <*> sw)
        instr = instrTicker (SNat @SlowPeriod) (SNat @FastPeriod) ((`testBit` 5) <$> sw)
        vga   = vgaTiming vga640x480
        inp   = mkIn <$> keys <*> sw <*> instr <*> (vgaVBlankStart <$> vga) <*> fbVga <*> fbOled
                     <*> (pgHold <$> ps) <*> pause <*> load
        mkIn k s i t a1 a2 hold p w = chip8InIdle
          { ciKeys = k, ciConfig = s, ciInstrTick = i, ciTimerTick = t
          , ciRun = not hold && not p, ciReset = hold, ciRomWrite = w
          , ciFbAddr = a1, ciFbAddr2 = a2 }
        out   = chip8Core (blockRamFile (SNat @4096) ramImage) inp
        (fbVga, stream) = chip8Stream amber vga (coFbRow <$> out)
        -- the PS menu's text screen over the picture, while it's shown
        (timing, rgb) = textOverlay (overlayWrite <$> link) (pcOverlay <$> ctrl) stream
        pins  = vgaPins vga640x480 timing rgb
        -- HDMI from the same stream, black outside the picture
        px    = (\o col -> PixelIn (vgaHSync o) (vgaVSync o) (vgaActive o)
                                  (if vgaActive o then col else black8))
                  <$> timing <*> rgb
        (hdmi, hdClk) = hdmiOut clk rst enableGen 1 1 (ycbcr422 1 1 px)
        (sclOut, sclIn) = openDrain sclPin (fst <$> i2cPins)
        (sdaOut, sdaIn) = openDrain sdaPin (snd <$> i2cPins)
        (i2cPins, _status) = adv7511Init (advTiming @DomPix) (I2cLines <$> sclIn <*> sdaIn)
        mode  = (\s -> if testBit s 7 then OledOneToOne else OledStretch) <$> sw
        (oledAddr, oled) = oledDriver (oledTiming @DomPix) (pure oledConfigDefault) oledByte
        (fbOled, oledByte) = oledAdapter mode oledAddr (coFbRow2 <$> out)
        leds  = ledDriver (debugByte <$> ((\s -> if testBit s 6 then 1 else 0) <$> sw) <*> (coDebug <$> out))
        beep  = register 0 (beeper (SNat @BeepHalfPeriod) (coSound <$> out))
    in ( leds
       , rgbR . vpRgb <$> pins, rgbG . vpRgb <$> pins, rgbB . vpRgb <$> pins
       , vpHSync <$> pins, vpVSync <$> pins
       , opDc <$> oled, opRes <$> oled, opSclk <$> oled
       , opSdin <$> oled, opVbat <$> oled, opVdd <$> oled
       , beep
       , hdClk, hpData <$> hdmi, hpDe <$> hdmi, hpHSync <$> hdmi, hpVSync <$> hdmi
       , sclOut, sdaOut
       , psOut
       )
 where
  -- No reset button: the MMCM's locked output (synchronised inside
  -- pixelClockGen) is the only reset.
  (clk, rst) = pixelClockGen gclk (unsafeFromActiveHigh (pure False))
{-# NOINLINE topEntity #-}
{-# ANN topEntity
  (Synthesize
    { t_name   = "chip8_top"
    , t_inputs = [ PortName "GCLK"
                 , PortName "BTNC", PortName "BTND", PortName "BTNL"
                 , PortName "BTNR", PortName "BTNU"
                 , PortName "SW", PortName "PS_GPIO"
                 , PortName "HD_SCL", PortName "HD_SDA" ]
    , t_output = PortProduct ""
                 [ PortName "LD"
                 , PortName "VGA_R", PortName "VGA_G", PortName "VGA_B"
                 , PortName "VGA_HS", PortName "VGA_VS"
                 , PortName "OLED_DC", PortName "OLED_RES", PortName "OLED_SCLK"
                 , PortName "OLED_SDIN", PortName "OLED_VBAT", PortName "OLED_VDD"
                 , PortName "JA1"
                 , PortName "HD_CLK", PortName "HD_D", PortName "HD_DE"
                 , PortName "HD_HSYNC", PortName "HD_VSYNC"
                 , PortName "HD_SCL", PortName "HD_SDA"
                 , PortName "PS_IN" ]
    }) #-}
