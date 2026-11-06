{-|
Game Boy top-level for the ZedBoard (PLAN.md steps 2.7 to 2.9). Needs
boards/zedboard/zedboard.xdc, vga.xdc, hdmi.xdc and hdmi_audio.xdc.

* __Cartridge__ (side track I, phase 2): ROM in DDR, up to 8 MiB from
  'romBase' (the running system's media region, docs/ARCHITECTURE.md),
  read through the Zynq's HP0 port by "Platform.Mem.Ddr" (a 1 KiB line
  cache with prefetch). The PS writes it there while holding the core
  (from the SD card menu, or @make load SYSTEM=gb@), and the cache is
  emptied while held. The core waits for each ROM byte ("Gb.Core",
  'gbCoreWith'): a cache hit costs nothing, a miss stops its time. The
  core reads the cartridge header when it starts. The bitstream holds no
  cartridge: until the PS loads one, the core runs whatever DDR holds,
  paused behind the menu.
* __HP0__: the AXI3 read channels as pins of this top and its clock
  (@HP0_ACLK@ = the pixel clock), as "M0e.Top"; the wrapper
  (boards/zedboard/gb_zynq.v, ps7_hp.tcl) ties the fixed AR fields and the
  write channels.
* __PS link__ (side track A, as "Chip8.Top"): the menu's text overlay
  over the picture; the control register's pause stops the dot enable, so
  the whole machine (CPU, PPU, timers, APU) stands still with its state
  kept while the menu is open, and the keys don't reach it. @PS_IN@ (EMIO
  GPIO 32 to 63): the debounced buttons, the switches and 'systemGameBoy',
  so the PS app shows the Game Boy's menu (@/gb@), and on the byte channel
  every byte the game sends on its serial port: the relay passes the text
  on to the desktop (Blargg's "Passed" shows in @make keys SYSTEM=gb@).
* __Battery saves__ (side track I): the PS writes a @.sav@ into
  cartridge RAM while holding the core, and reads it back while the game
  runs through cartridge RAM's second port (map in "Gb.Loader"); the
  bytes come out on the byte channel while the save control's bit 0 is
  set (serial bytes meanwhile are dropped), and @PS_IN@ bit 6 is the
  dirty flag: the game wrote cartridge RAM since the PS last cleared it.
* __Timing__: everything in 'DomPix'; the Game Boy's dot clock is an
  enable ('dotEnable', 4.194304 MHz).
* __Video__: the 160×144 picture ×3 at (80, 24) of 640×480, double
  triple buffered ('frameBuffers'), on VGA and on HDMI (ADV7511, as
  CHIP-8, but in HDMI mode: 'tableVecHdmiAudio').
* __Sound__: the APU's stereo mix (±480) ×64 as 16-bit PCM, sampled at
  48 kHz by the S/PDIF transmitter ('spdifDdr') on HD_SPDIF; the ADV7511
  sends it in the HDMI stream.
* __Switches__: SW0 palette (off grey, on DMG green); SW6 LED view.
* __Buttons__ (debounced 10 ms): BTNR/BTNL/BTNU/BTND = D-pad, BTNC = A;
  everything else, and all keys, from the desktop through the PS
  (@PS_GPIO@ bits 0 to 7: Right Left Up Down A B Select Start), ORed.
* __LEDs__: SW6 off: LD0 cartridge type supported, LD1 toggles every Game
  Boy frame, LD2 loader hold, LD7 heartbeat (1 Hz); SW6 on: the last byte
  sent on the link port (Blargg's test output, one character at a time).
-}
module Gb.Top
  ( topEntity
  , romBase
  , GbVariant (..)
  , gbTop
  ) where

import Clash.Prelude

import Gb.Core
import Gb.Machine
import Platform.Audio.Spdif (spdifDdr)
import Platform.Audio.Volume (volumeStereo)
import Platform.SaveState (StateHooks (..))
import Platform.Board.Zedboard
import Platform.Bus.I2c (I2cLines (..))
import Platform.Clocks (pixelClockGenPs)
import Platform.Domains
import Platform.Input.Sync (synchronise)
import Platform.Mem.Axi
import Platform.Output.Leds (heartbeat, ledDriver)
import Platform.Video.Adv7511 (adv7511InitWith, advTiming, tableVecHdmiAudio)
import Platform.Video.VgaTiming
import Platform.Video.Ycbcr422

topEntity
  :: Clock Dom100
  -> Signal DomPix Bit  -- PS_RESETN (FCLK_RESET0_N)
  -> Signal DomPix Bit  -- BTNC
  -> Signal DomPix Bit  -- BTND
  -> Signal DomPix Bit  -- BTNL
  -> Signal DomPix Bit  -- BTNR
  -> Signal DomPix Bit  -- BTNU
  -> Signal DomPix (BitVector 8)  -- SW
  -> Signal DomPix (BitVector 32) -- PS_GPIO
  -> BiSignalIn 'Floating DomPix 1  -- HD_SCL
  -> BiSignalIn 'Floating DomPix 1  -- HD_SDA
  -> Signal DomPix Bit             -- HP0_ARREADY
  -> Signal DomPix Bit             -- HP0_RVALID
  -> Signal DomPix (BitVector 64)  -- HP0_RDATA
  -> Signal DomPix (BitVector 2)   -- HP0_RRESP
  -> Signal DomPix Bit             -- HP0_RLAST
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
     , Clock DomPix                  -- HP0_ACLK
     , Signal DomPix Bit             -- HP0_ARVALID
     , Signal DomPix (BitVector 32)  -- HP0_ARADDR
     , Signal DomPix (BitVector 4)   -- HP0_ARLEN
     , Signal DomPix Bit             -- HP0_RREADY
     )
topEntity = gbTop dmg
{-# NOINLINE topEntity #-}

-- | The ZedBoard's ends around the machine ("Gb.Machine"; 'topEntity': the DMG).
gbTop
  :: forall w. KnownNat w
  => GbVariant w
  -> Clock Dom100
  -> Signal DomPix Bit
  -> Signal DomPix Bit
  -> Signal DomPix Bit
  -> Signal DomPix Bit
  -> Signal DomPix Bit
  -> Signal DomPix Bit
  -> Signal DomPix (BitVector 8)
  -> Signal DomPix (BitVector 32)
  -> BiSignalIn 'Floating DomPix 1
  -> BiSignalIn 'Floating DomPix 1
  -> Signal DomPix Bit
  -> Signal DomPix Bit
  -> Signal DomPix (BitVector 64)
  -> Signal DomPix (BitVector 2)
  -> Signal DomPix Bit
  -> ( Signal DomPix (BitVector 8)
     , Signal DomPix (BitVector 4)
     , Signal DomPix (BitVector 4)
     , Signal DomPix (BitVector 4)
     , Signal DomPix Bit
     , Signal DomPix Bit
     , Signal DomPixDdr Bit
     , Signal DomPix (BitVector 16)
     , Signal DomPix Bit
     , Signal DomPix Bit
     , Signal DomPix Bit
     , BiSignalOut 'Floating DomPix 1
     , BiSignalOut 'Floating DomPix 1
     , Signal DomPixDdr Bit
     , Signal DomPix (BitVector 32)
     , Clock DomPix
     , Signal DomPix Bit
     , Signal DomPix (BitVector 32)
     , Signal DomPix (BitVector 4)
     , Signal DomPix Bit
     )
gbTop var gclk psResetN c d l r u swRaw psRaw sclPin sdaPin arReady rValid rData' rResp' rLast' =
  withClockResetEnable clk rst enableGen $
    let sw    = synchronise 0 swRaw
        s2m   = (\a v dat rsp lst -> AxiRdS2M (a == 1)
                   (if v == 1 then Just (AxiR dat rsp (lst == 1)) else Nothing))
                  <$> arReady <*> rValid <*> rData' <*> rResp' <*> rLast'
        -- (no screenshots on the ZedBoard yet: its HP0 write channels stay tied off)
        GbMachine core hold timing rgb psOut m2s vol _ = gbMachine NoStateHooks var (c :> d :> l :> r :> u :> Nil) sw psRaw s2m (pure axiWrS2MIdle)
        ar    = m2sAr <$> m2s
        pins  = vgaPins vga640x480 timing rgb
        px    = (\o col -> PixelIn (vgaHSync o) (vgaVSync o) (vgaActive o)
                                  (if vgaActive o then col else black8))
                  <$> timing <*> rgb
        (hdmi, hdClk) = hdmiOut clk rst enableGen 1 1 (ycbcr422 1 1 px)
        (sclOut, sclIn) = openDrain sclPin (fst <$> i2cPins)
        (sdaOut, sdaIn) = openDrain sdaPin (snd <$> i2cPins)
        (i2cPins, _status) = adv7511InitWith tableVecHdmiAudio (advTiming @DomPix) (I2cLines <$> sclIn <*> sdaIn)
        pcm (sl, sr) = (resize sl `shiftL` 6, resize sr `shiftL` 6) :: (Signed 16, Signed 16)
        spdifPin = spdifDdr clk rst enableGen (register (0, 0) (volumeStereo vol (pcm . koAudio <$> core)))
        frameTog = frameToggle core
        serial = register 0 (fromJustX' <$> (koSerial <$> core) <*> serial)
        fromJustX' m prev = maybe prev id m
        beat  = heartbeat (SNat @(MsToCycles DomPix 500))
        status = (\ok fr h b -> (if ok then 0x01 else 0) .|. (if fr then 0x02 else 0) .|. (if h then 0x04 else 0)
                                .|. (if b then 0x80 else 0) :: BitVector 8)
                   <$> (koCartOk <$> core) <*> frameTog <*> hold <*> beat
        leds  = ledDriver ((\s st se -> if testBit s 6 then se else st) <$> sw <*> status <*> serial)
    in ( leds
       , rgbR . vpRgb <$> pins, rgbG . vpRgb <$> pins, rgbB . vpRgb <$> pins
       , vpHSync <$> pins, vpVSync <$> pins
       , hdClk, hpData <$> hdmi, hpDe <$> hdmi, hpHSync <$> hdmi, hpVSync <$> hdmi
       , sclOut, sdaOut
       , spdifPin
       , psOut
       , clk
       , boolToBit . maybe False (const True) <$> ar
       , maybe 0 (pack . arAddr) <$> ar
       , maybe 0 (pack . arLen) <$> ar
       , boolToBit . m2sRReady <$> m2s
       )
 where
  -- the PS's PL reset too (PS_RESETN): see pixelClockGenPs
  (clk, rst) = pixelClockGenPs gclk (unsafeFromActiveHigh (pure False)) psResetN
{-# INLINE gbTop #-}
{-# ANN topEntity
  (Synthesize
    { t_name   = "gb_top"
    , t_inputs = [ PortName "GCLK", PortName "PS_RESETN"
                 , PortName "BTNC", PortName "BTND", PortName "BTNL"
                 , PortName "BTNR", PortName "BTNU"
                 , PortName "SW", PortName "PS_GPIO"
                 , PortName "HD_SCL", PortName "HD_SDA"
                 , PortName "HP0_ARREADY", PortName "HP0_RVALID", PortName "HP0_RDATA"
                 , PortName "HP0_RRESP", PortName "HP0_RLAST" ]
    , t_output = PortProduct ""
                 [ PortName "LD"
                 , PortName "VGA_R", PortName "VGA_G", PortName "VGA_B"
                 , PortName "VGA_HS", PortName "VGA_VS"
                 , PortName "HD_CLK", PortName "HD_D", PortName "HD_DE"
                 , PortName "HD_HSYNC", PortName "HD_VSYNC"
                 , PortName "HD_SCL", PortName "HD_SDA"
                 , PortName "HD_SPDIF"
                 , PortName "PS_IN"
                 , PortName "HP0_ACLK"
                 , PortName "HP0_ARVALID", PortName "HP0_ARADDR", PortName "HP0_ARLEN"
                 , PortName "HP0_RREADY" ]
    }) #-}
