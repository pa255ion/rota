{-|
Side track C, step 3: the DDR test top. "M0e.Tester" reads DDR through
the Zynq's HP0 port ("Platform.Mem.Ddr") and measures it; the PS fills
DDR, sets the test up and reads the results over the PS link
(platform/ps/ddrtest.zig, through the relay app). Needs the board wrapper
boards/zedboard/m0e_zynq.v (PS7 with HP0, ps7_hp.tcl).

* __HP0__: the AXI3 read channels as pins of this top, and its clock
  (@HP0_ACLK@ = the pixel clock: the PL clocks the HP port, DS187 allows
  any rate up to 250 MHz). The wrapper ties the fixed AR fields
  ("Platform.Mem.Axi") and the write channels.
* __PS link__ (@PS_GPIO@, @PS_IN@): the test's registers and result
  bytes ("M0e.Tester"); @PS_IN@ carries 'systemM0e', and the selected
  result bytes on its byte channel.
* __LEDs__: LD0 running, LD1 done, LD2 AXI error, LD7 heartbeat.
* __VGA__: the whole screen in the state's colour (blue running, green
  done, red error), mostly for the layer 4 harness, which needs the VGA
  ports.
-}
module M0e.Top
  ( topEntity
  ) where

import Clash.Prelude

import M0e.Tester
import Platform.Clocks (pixelClockGen)
import Platform.Domains
import Platform.Input.Sync (synchronise)
import Platform.Mem.Axi
import Platform.Output.Leds (heartbeat, ledDriver)
import Platform.Ps.Link
import Platform.Video.VgaTiming

topEntity
  :: Clock Dom100
  -> Signal DomPix Bit             -- BTNC
  -> Signal DomPix Bit             -- BTND
  -> Signal DomPix Bit             -- BTNL
  -> Signal DomPix Bit             -- BTNR
  -> Signal DomPix Bit             -- BTNU
  -> Signal DomPix (BitVector 8)   -- SW
  -> Signal DomPix (BitVector 32)  -- PS_GPIO
  -> Signal DomPix Bit             -- HP0_ARREADY
  -> Signal DomPix Bit             -- HP0_RVALID
  -> Signal DomPix (BitVector 64)  -- HP0_RDATA
  -> Signal DomPix (BitVector 2)   -- HP0_RRESP
  -> Signal DomPix Bit             -- HP0_RLAST
  -> ( Signal DomPix (BitVector 8)   -- LD
     , Signal DomPix (BitVector 4)   -- VGA_R
     , Signal DomPix (BitVector 4)   -- VGA_G
     , Signal DomPix (BitVector 4)   -- VGA_B
     , Signal DomPix Bit             -- VGA_HS
     , Signal DomPix Bit             -- VGA_VS
     , Signal DomPix (BitVector 32)  -- PS_IN
     , Clock DomPix                  -- HP0_ACLK
     , Signal DomPix Bit             -- HP0_ARVALID
     , Signal DomPix (BitVector 32)  -- HP0_ARADDR
     , Signal DomPix (BitVector 4)   -- HP0_ARLEN
     , Signal DomPix Bit             -- HP0_RREADY
     )
topEntity gclk c d l r u swRaw psRaw arReady rValid rData' rResp' rLast' =
  withClockResetEnable clk rst enableGen $
    let sw    = synchronise 0 swRaw
        -- C, D, L, R, U
        btns  = reverse . unpack <$> synchronise (0 :: BitVector 5) (pack <$> bundle (u :> r :> l :> d :> c :> Nil))
        ps    = psGpio <$> synchronise 0 psRaw
        link  = psLink ps
        s2m   = (\a v dat rsp lst -> AxiRdS2M (a == 1)
                   (if v == 1 then Just (AxiR dat rsp (lst == 1)) else Nothing))
                  <$> arReady <*> rValid <*> rData' <*> rResp' <*> rLast'
        (byte, m2s, rs) = tester link s2m
        res   = fst <$> rs
        err   = snd <$> rs
        psOut = register 0 (psInWith systemM0e <$> btns <*> sw <*> byte)
        leds  = ledDriver ((\run done e hb -> pack hb ++# (0 :: BitVector 4) ++# pack e ++# pack done ++# pack run)
                  <$> (rsRunning <$> res) <*> (rsDone <$> res) <*> err
                  <*> heartbeat (SNat @(MsToCycles DomPix 500)))
        colour = (\rr e -> if e then Rgb888 0xFF 0 0
                           else if rsDone rr then Rgb888 0 0xFF 0
                           else if rsRunning rr then Rgb888 0 0 0xFF
                           else black8) <$> res <*> err
        pins  = vgaPins vga640x480 (vgaTiming vga640x480) colour
        ar    = m2sAr <$> m2s
    in ( leds
       , rgbR . vpRgb <$> pins, rgbG . vpRgb <$> pins, rgbB . vpRgb <$> pins
       , vpHSync <$> pins, vpVSync <$> pins
       , psOut
       , clk
       , boolToBit . maybe False (const True) <$> ar
       , maybe 0 (pack . arAddr) <$> ar
       , maybe 0 (pack . arLen) <$> ar
       , boolToBit . m2sRReady <$> m2s
       )
 where
  -- No reset button: the MMCM's locked output (synchronised inside
  -- pixelClockGen) is the only reset.
  (clk, rst) = pixelClockGen gclk (unsafeFromActiveHigh (pure False))
{-# NOINLINE topEntity #-}
{-# ANN topEntity
  (Synthesize
    { t_name   = "m0e_top"
    , t_inputs = [ PortName "GCLK"
                 , PortName "BTNC", PortName "BTND", PortName "BTNL"
                 , PortName "BTNR", PortName "BTNU"
                 , PortName "SW", PortName "PS_GPIO"
                 , PortName "HP0_ARREADY", PortName "HP0_RVALID", PortName "HP0_RDATA"
                 , PortName "HP0_RRESP", PortName "HP0_RLAST" ]
    , t_output = PortProduct ""
                 [ PortName "LD"
                 , PortName "VGA_R", PortName "VGA_G", PortName "VGA_B"
                 , PortName "VGA_HS", PortName "VGA_VS"
                 , PortName "PS_IN"
                 , PortName "HP0_ACLK"
                 , PortName "HP0_ARVALID", PortName "HP0_ARADDR", PortName "HP0_ARLEN"
                 , PortName "HP0_RREADY" ]
    }) #-}
