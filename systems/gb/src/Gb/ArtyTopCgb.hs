{-|
The Game Boy Color on the Arty Z7-20 (side track I, phase 3, now that the
Arty is the main board): "Gb.ArtyTop"'s board ends ('gbArtyTop') around
"Gb.Machine"'s Game Boy Color ('cgb': 15-bit colours, system ID 6, menu
folder @/gbc@). The same pins as "Gb.ArtyTop"; the wrapper is
boards/arty-z7-20/gbc_zynq.v; @make … SYSTEM=gbc BOARD=arty-z7-20@
(systems/gbc/arty-z7-20.mk). SW0 does nothing here.
-}
module Gb.ArtyTopCgb
  ( topEntity
  ) where

import Clash.Prelude

import Gb.ArtyTop (gbArtyTop)
import Gb.Machine (cgb)
import Platform.Board.ArtyZ7 (TmdsPins (..))
import Platform.Domains

topEntity
  :: Clock Dom125
  -> Signal DomPix Bit             -- PS_RESETN (FCLK_RESET0_N)
  -> Signal DomPix (BitVector 4)   -- btn
  -> Signal DomPix (BitVector 2)   -- sw
  -> Signal DomPix (BitVector 32)  -- PS_GPIO
  -> Signal DomPix Bit             -- HP0_ARREADY
  -> Signal DomPix Bit             -- HP0_RVALID
  -> Signal DomPix (BitVector 64)  -- HP0_RDATA
  -> Signal DomPix (BitVector 2)   -- HP0_RRESP
  -> Signal DomPix Bit             -- HP0_RLAST
  -> Signal DomPix Bit             -- HP0_AWREADY (side track O, O.4: screenshots)
  -> Signal DomPix Bit             -- HP0_WREADY
  -> Signal DomPix Bit             -- HP0_BVALID
  -> Signal DomPix (BitVector 2)   -- HP0_BRESP
  -> ( Signal DomPix (BitVector 4)   -- led
     , Signal DomPix TmdsPins        -- hdmi_tx_*
     , Signal DomPix (BitVector 32)  -- PS_IN
     , Clock DomPix                  -- HP0_ACLK
     , Signal DomPix Bit             -- HP0_ARVALID
     , Signal DomPix (BitVector 32)  -- HP0_ARADDR
     , Signal DomPix (BitVector 4)   -- HP0_ARLEN
     , Signal DomPix Bit             -- HP0_RREADY
     , Signal DomPix Bit             -- HP0_AWVALID
     , Signal DomPix (BitVector 32)  -- HP0_AWADDR
     , Signal DomPix (BitVector 4)   -- HP0_AWLEN
     , Signal DomPix Bit             -- HP0_WVALID
     , Signal DomPix (BitVector 64)  -- HP0_WDATA
     , Signal DomPix (BitVector 8)   -- HP0_WSTRB
     , Signal DomPix Bit             -- HP0_WLAST
     , Signal DomPix Bit             -- HP0_BREADY
     , Signal DomPix Bit             -- aud_pwm
     , Signal DomPix Bit             -- aud_sd
     )
topEntity = gbArtyTop cgb
{-# NOINLINE topEntity #-}
{-# ANN topEntity
  (Synthesize
    { t_name   = "gbc_arty_top"
    , t_inputs = [ PortName "clk", PortName "PS_RESETN", PortName "btn", PortName "sw", PortName "PS_GPIO"
                 , PortName "HP0_ARREADY", PortName "HP0_RVALID", PortName "HP0_RDATA"
                 , PortName "HP0_RRESP", PortName "HP0_RLAST"
                 , PortName "HP0_AWREADY", PortName "HP0_WREADY", PortName "HP0_BVALID", PortName "HP0_BRESP" ]
    , t_output = PortProduct ""
                 [ PortName "led"
                 , PortProduct ""
                     [ PortName "hdmi_tx_clk_p", PortName "hdmi_tx_clk_n"
                     , PortName "hdmi_tx_d_p", PortName "hdmi_tx_d_n" ]
                 , PortName "PS_IN"
                 , PortName "HP0_ACLK"
                 , PortName "HP0_ARVALID", PortName "HP0_ARADDR", PortName "HP0_ARLEN"
                 , PortName "HP0_RREADY"
                 , PortName "HP0_AWVALID", PortName "HP0_AWADDR", PortName "HP0_AWLEN"
                 , PortName "HP0_WVALID", PortName "HP0_WDATA", PortName "HP0_WSTRB", PortName "HP0_WLAST"
                 , PortName "HP0_BREADY"
                 , PortName "aud_pwm", PortName "aud_sd" ]
    }) #-}
