{-|
The Master System on the ZedBoard (M6 step 6.6; the second board):
"Sms.Machine" with the picture on VGA and on HDMI through the ADV7511
(4:2:2, the I2C init with the audio table, as the Game Gear's top), the PSG
and the FM unit (mono) on S/PDIF into the ADV7511. The cartridge (and a
BIOS) in DDR, read over HP0; the PS link as the Game Gear's.

Buttons: BTNU, BTND, BTNL, BTNR the pad, BTNC button 1; SW0 Pause, SW1
button 2. LEDs: LD0 the core's frames (toggling), LD7 the heartbeat.
-}
module Sms.Top
  ( topEntity
  ) where

import Clash.Prelude

import Platform.SaveState (StateHooks (..))
import Sms.Machine
import Platform.Audio.Spdif (spdifDdr)
import Platform.Board.Zedboard
import Platform.Bus.I2c (I2cLines (..))
import Platform.Clocks (pixelClockGen)
import Platform.Domains
import Platform.Input.Debounce (syncDebounce)
import Platform.Input.Sync (synchronise)
import Platform.Mem.Axi
import Platform.Output.Leds (heartbeat, ledDriver)
import Platform.Ps.Link (psByteOut, psGpio, psInWith)
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
     , Clock DomPix                  -- HP0_ACLK
     , Signal DomPix Bit             -- HP0_ARVALID
     , Signal DomPix (BitVector 32)  -- HP0_ARADDR
     , Signal DomPix (BitVector 4)   -- HP0_ARLEN
     , Signal DomPix Bit             -- HP0_RREADY
     , Signal DomPix (BitVector 32)  -- PS_IN
     )
topEntity gclk c d l r u swRaw psRaw sclPin sdaPin arReady rValid rData' rResp' rLast' =
  withClockResetEnable clk rst enableGen $
    let db = syncDebounce (SNat @(MsToCycles DomPix 10)) . fmap bitToBool
        swB b = syncDebounce (SNat @(MsToCycles DomPix 10)) (bitToBool . (! (b :: Int)) <$> swRaw)
        (dbC, dbD, dbL, dbR, dbU) = (db c, db d, db l, db r, db u)
        pad = (\bu bd bl br bc b2 -> (if bu then 0x01 else 0) .|. (if bd then 0x02 else 0) .|. (if bl then 0x04 else 0)
                                    .|. (if br then 0x08 else 0) .|. (if bc then 0x10 else 0) .|. (if b2 then 0x20 else 0) :: Unsigned 8)
                <$> dbU <*> dbD <*> dbL <*> dbR <*> dbC <*> swB 1
        -- the PS link (Sms.Machine: keys, menu, loading, saves)
        ps = psGpio <$> synchronise 0 psRaw
        s2m = (\a v dat rsp lst -> AxiRdS2M (a == 1) (if v == 1 then Just (AxiR dat rsp (lst == 1)) else Nothing))
                <$> arReady <*> rValid <*> rData' <*> rResp' <*> rLast'
        m = smsMachine NoStateHooks pad (swB 0) ps s2m
        timing = smTiming m
        rgb = smRgb m
        pins = vgaPins vga640x480 timing rgb
        px = (\o col -> PixelIn (vgaHSync o) (vgaVSync o) (vgaActive o) (if vgaActive o then col else black8))
               <$> timing <*> rgb
        (hdmi, hdClk) = hdmiOut clk rst enableGen 1 1 (ycbcr422 1 1 px)
        (sclOut, sclIn) = openDrain sclPin (fst <$> i2cPins)
        (sdaOut, sdaIn) = openDrain sdaPin (snd <$> i2cPins)
        (i2cPins, _status) = adv7511InitWith tableVecHdmiAudio (advTiming @DomPix) (I2cLines <$> sclIn <*> sdaIn)
        spdifPin = spdifDdr clk rst enableGen (register (0, 0) (smAudio m))
        beat = heartbeat (SNat @(MsToCycles DomPix 500))
        status = (\f b -> (if f then 0x01 else 0) .|. (if b then 0x80 else 0) :: BitVector 8) <$> smFrame m <*> beat
    in ( ledDriver status
       , rgbR . vpRgb <$> pins, rgbG . vpRgb <$> pins, rgbB . vpRgb <$> pins
       , vpHSync <$> pins, vpVSync <$> pins
       , hdClk, hpData <$> hdmi, hpDe <$> hdmi, hpHSync <$> hdmi, hpVSync <$> hdmi
       , sclOut, sdaOut
       , spdifPin
       , clk
       , boolToBit . maybe False (const True) . m2sAr <$> smAxi m
       , maybe 0 (pack . arAddr) . m2sAr <$> smAxi m
       , maybe 0 (pack . arLen) . m2sAr <$> smAxi m
       , boolToBit . m2sRReady <$> smAxi m
         -- the system ID and the board's buttons and switches for the PS,
         -- the save read back on the byte channel, bit 6 dirty
       , register 0 ((\w dirty -> if dirty then setBit w 6 else w)
                      <$> (psInWith systemSms <$> bundle (dbC :> dbD :> dbL :> dbR :> dbU :> Nil)
                                                   <*> synchronise 0 swRaw
                                                   <*> psByteOut (smByte m))
                      <*> smDirty m)
       )
 where
  (clk, rst) = pixelClockGen gclk (unsafeFromActiveHigh (pure False))
{-# NOINLINE topEntity #-}
{-# ANN topEntity
  (Synthesize
    { t_name   = "sms_top"
    , t_inputs = [ PortName "GCLK"
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
                 , PortName "HP0_ACLK"
                 , PortName "HP0_ARVALID", PortName "HP0_ARADDR", PortName "HP0_ARLEN"
                 , PortName "HP0_RREADY"
                 , PortName "PS_IN" ]
    }) #-}
