{-|
The Atari 800XL on the ZedBoard (M4 step 4.7): the machine as
"A800.System" puts it together (the core, loading and the keyboard over
the PS link, the picture and the sound), with the ZedBoard's ends: VGA
and HDMI through the ADV7511, sound over S/PDIF, the buttons and
switches, the LEDs. "A800.ArtyTop" is the Arty's.

* __Software__: the OS, BASIC, the loader page and an executable come
  from the block RAM images @a800-image@ makes (@build/a800xl/*.bin@; the
  executable from @XEX=@, else none: BASIC's Ready prompt). The PS
  replaces the program over the PS link while holding the core
  ("A800.Loader": an XEX, a CAR or a raw 8/16 KiB cartridge from link
  address 0), from the SD card menu (@/a800xl@); when hold falls the core
  starts from power-on with it, or stays held if it was refused (LD0
  off).
* __PS link__: the menu's text overlay over the picture; the control
  register's pause stops the cycle enable (the machine stands still).
  @PS_IN@: the debounced buttons, the switches and system ID 5.
* __Timing__: everything in 'DomPix' (25.17623 MHz); the core gets a
  machine-cycle enable ('a800CycleEnable', every 14 or 15 clocks).
* __Input__ (docs/systems/atari800xl.md, "Input"): joystick 1 is the
  board buttons (BTNU, BTND, BTNL, BTNR the stick, BTNC the trigger) ORed
  with the PS's; SW0, SW1, SW2 hold START, SELECT, OPTION, ORed with the
  PS's console keys; the keyboard comes over the PS link as the key
  matrix ("A800.Loader", 'keysBase'). All of them are released while the
  menu pauses the core.
* __Video__: VGA and HDMI from the same 24-bit stream, 320 × 240 × 2.
* __Sound__: POKEY's level through the output curve and the filters, as
  16-bit PCM sampled at 48 kHz by the S/PDIF transmitter on HD_SPDIF (HDMI
  mode with audio, as the NES).
* __LEDs__: LD0 the program was accepted, LD1 toggles every frame, LD2
  the loader's hold, LD7 heartbeat (1 Hz); SW6 on: the joystick bits.
-}
module A800.Top
  ( topEntity
  , imageFiles
  , cfgImage
  , systemA800
  ) where

import Clash.Prelude

import A800.System
import Platform.SaveState (StateHooks (..))
import Platform.Audio.Spdif (spdifDdr)
import Platform.Board.Zedboard
import Platform.Bus.I2c (I2cLines (..))
import Platform.Clocks (pixelClockGen)
import Platform.Domains
import Platform.Input.Debounce (syncDebounce)
import Platform.Input.Sync (synchronise)
import Platform.Output.Leds (heartbeat, ledDriver)
import Platform.Ps.Link
import Platform.Video.Adv7511 (adv7511InitWith, advTiming, tableVecHdmiAudio)
import Platform.Video.VgaTiming
import Platform.Video.Ycbcr422

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
     , Signal DomPixDdr Bit         -- HD_CLK
     , Signal DomPix (BitVector 16) -- HD_D
     , Signal DomPix Bit            -- HD_DE
     , Signal DomPix Bit            -- HD_HSYNC
     , Signal DomPix Bit            -- HD_VSYNC
     , BiSignalOut 'Floating DomPix 1  -- HD_SCL
     , BiSignalOut 'Floating DomPix 1  -- HD_SDA
     , Signal DomPixDdr Bit         -- HD_SPDIF
     , Signal DomPix (BitVector 32) -- PS_IN
     )
topEntity gclk c d l r u swRaw psRaw sclPin sdaPin =
  withClockResetEnable clk rst enableGen $
    let sw    = synchronise 0 swRaw
        ps    = psGpio <$> synchronise 0 psRaw
        db    = syncDebounce (SNat @(MsToCycles DomPix 10)) . fmap bitToBool
        (dbC, dbD, dbL, dbR, dbU) = (db c, db d, db l, db r, db u)
        -- the machine (A800.System): the buttons are the stick and
        -- trigger, SW0-2 hold START, SELECT, OPTION
        board = Board <$> dbU <*> dbD <*> dbL <*> dbR <*> dbC <*> (slice d2 d0 <$> sw)
        -- (the cartridge in block RAM on the ZedBoard: standard cartridges only)
        so    = a800System NoStateHooks CartBram ps board
        psOut = register 0 (psIn systemA800 <$> bundle (dbC :> dbD :> dbL :> dbR :> dbU :> Nil) <*> sw)
        (timing, rgb) = (soTiming so, soRgb so)
        pins  = vgaPins vga640x480 timing rgb
        px    = (\o col -> PixelIn (vgaHSync o) (vgaVSync o) (vgaActive o)
                                  (if vgaActive o then col else black8))
                  <$> timing <*> rgb
        (hdmi, hdClk) = hdmiOut clk rst enableGen 1 1 (ycbcr422 1 1 px)
        (sclOut, sclIn) = openDrain sclPin (fst <$> i2cPins)
        (sdaOut, sdaIn) = openDrain sdaPin (snd <$> i2cPins)
        (i2cPins, _status) = adv7511InitWith tableVecHdmiAudio (advTiming @DomPix) (I2cLines <$> sclIn <*> sdaIn)
        -- sound: both channels the same
        spdifPin = spdifDdr clk rst enableGen (register (0, 0) ((\v -> (v, v)) <$> soPcm so))
        beat  = heartbeat (SNat @(MsToCycles DomPix 500))
        status = (\ok fr h b -> (if ok then 0x01 else 0) .|. (if fr then 0x02 else 0) .|. (if h then 0x04 else 0)
                                .|. (if b then 0x80 else 0) :: BitVector 8)
                   <$> soOk so <*> soFrame so <*> soHold so <*> beat
        leds  = ledDriver ((\s st j -> if testBit s 6 then pack (complement j) else st) <$> sw <*> status <*> soJoy so)
    in ( leds
       , rgbR . vpRgb <$> pins, rgbG . vpRgb <$> pins, rgbB . vpRgb <$> pins
       , vpHSync <$> pins, vpVSync <$> pins
       , hdClk, hpData <$> hdmi, hpDe <$> hdmi, hpHSync <$> hdmi, hpVSync <$> hdmi
       , sclOut, sdaOut
       , spdifPin
       , psOut
       )
 where
  -- No reset button: the MMCM's locked output (synchronised inside
  -- pixelClockGen) is the only reset.
  (clk, rst) = pixelClockGen gclk (unsafeFromActiveHigh (pure False))
{-# NOINLINE topEntity #-}
{-# ANN topEntity
  (Synthesize
    { t_name   = "a800xl_top"
    , t_inputs = [ PortName "GCLK"
                 , PortName "BTNC", PortName "BTND", PortName "BTNL"
                 , PortName "BTNR", PortName "BTNU"
                 , PortName "SW", PortName "PS_GPIO"
                 , PortName "HD_SCL", PortName "HD_SDA" ]
    , t_output = PortProduct ""
                 [ PortName "LD"
                 , PortName "VGA_R", PortName "VGA_G", PortName "VGA_B"
                 , PortName "VGA_HS", PortName "VGA_VS"
                 , PortName "HD_CLK", PortName "HD_D", PortName "HD_DE"
                 , PortName "HD_HSYNC", PortName "HD_VSYNC"
                 , PortName "HD_SCL", PortName "HD_SDA"
                 , PortName "HD_SPDIF"
                 , PortName "PS_IN" ]
    }) #-}
