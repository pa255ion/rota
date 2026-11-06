{-|
The Game Boy on the Arty Z7-20 (side track G phase 2, step 3), the
counterpart of "Gb.Top": the same machine (cartridge ROM in DDR through
HP0, cartridge RAM with battery saves for the PS, MBC3's clock, the PS
link, the menu's text overlay, the pause) with the Arty's I/O. The
synthesis top is the wrapper boards/arty-z7-20/gb_zynq.v (the Arty's PS7
with HP0, boards/arty-z7-20/ps7_hp.tcl). Needs
boards/arty-z7-20/arty-z7-20.xdc and hdmi_tx.xdc.

__The machine is "Gb.Machine"'s__, shared with "Gb.Top" since side track
I phase 3 (before, a copy kept in step by hand); 'gbArtyTop' adds only
the board's ends, for either model ("Gb.ArtyTopCgb": the Game Boy Color).

The Arty's ends:

* __Buttons__, as "Chip8.ArtyTop": BTN0 = Left, BTN1 = Down, BTN2 = Up,
  BTN3 = Right (the ZedBoard's BTNL, BTND, BTNU, BTNR), __SW1__ = A (the
  ZedBoard's BTNC). The menu combination (Up and Down for a second) is
  BTN1 + BTN2. Everything else from the desktop or the USB keyboard.
* __SW0__: palette (off grey, on DMG green), as the ZedBoard's SW0.
* __LEDs__: LD0 cartridge type supported, LD1 toggles every Game Boy
  frame, LD2 loader hold, LD3 heartbeat (1 Hz).
* __Video__: "Gb.Top"'s stream (×3, letterboxed) on the HDMI output in
  HDMI mode with the APU's stereo sound ("Platform.Video.Hdmi"; side track
  G phase 3). The reset includes the PS's PL reset (@PS_RESETN@).
* __Sound__ (phase 2 step 6): the APU's channels mixed to mono, (L + R) ×
  32, through "Platform.Audio.Pwm" on the Arty's jack.
-}
module Gb.ArtyTop
  ( topEntity
  , gbArtyTop
  , artyGbButtons
  ) where

import Clash.Prelude

import Gb.Core
import Gb.Machine
import Platform.Audio.Pwm (pwmOut)
import Platform.Audio.Volume (volumeMono, volumeStereo)
import Platform.SaveState (StateHooks (..))
import Platform.Board.ArtyZ7 (TmdsPins (..), artyClocksPs, artyCts, tmdsOut)
import Platform.Domains
import Platform.Input.Sync (synchronise)
import Platform.Mem.Axi
import Platform.Output.Leds (heartbeat)
import Platform.Video.Hdmi (hdmiVideo)

-- | The Arty's buttons and SW1 as the ZedBoard's five, in "Gb.Top"'s
-- order C, D, L, R, U: SW1 as C (A), BTN1 as D, BTN0 as L, BTN3 as R,
-- BTN2 as U.
artyGbButtons :: BitVector 4 -> BitVector 2 -> Vec 5 Bit
artyGbButtons btn sw = sw ! (1 :: Int) :> btn ! (1 :: Int) :> btn ! (0 :: Int)
                    :> btn ! (3 :: Int) :> btn ! (2 :: Int) :> Nil

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
topEntity = gbArtyTop dmg
{-# NOINLINE topEntity #-}

-- | The Arty's ends around the machine ("Gb.Machine"; 'topEntity': the
-- DMG).
gbArtyTop
  :: forall w. KnownNat w
  => GbVariant w
  -> Clock Dom125
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
gbArtyTop var clk125 psResetN btnRaw swRaw psRaw arReady rValid rData' rResp' rLast' awReady wReady bValid bResp =
  ( leds, tmdsOut clk clk5x rst syms, psOut, clk, arValidO, arAddrO, arLenO, rReadyO
  , awValidO, awAddrO, awLenO, wValidO, wDataO, wStrbO, wLastO, bReadyO, audPwm, audSd )
 where
  -- the PS's PL reset too (PS_RESETN): see artyClocksPs (side track G phase 3)
  (clk, clk5x, rst) = artyClocksPs clk125 (unsafeFromActiveHigh (pure False)) psResetN
  (leds, syms, psOut, arValidO, arAddrO, arLenO, rReadyO, awValidO, awAddrO, awLenO, wValidO, wDataO, wStrbO, wLastO, bReadyO, audPwm, audSd) = withClockResetEnable clk rst enableGen $
    let sw    = synchronise 0 swRaw
        five  = unbundle (artyGbButtons <$> btnRaw <*> swRaw)
        s2m   = (\a v dat rsp lst -> AxiRdS2M (a == 1)
                   (if v == 1 then Just (AxiR dat rsp (lst == 1)) else Nothing))
                  <$> arReady <*> rValid <*> rData' <*> rResp' <*> rLast'
        -- (side track O, O.4) HP0's write channels: screenshots
        s2mW  = (\a w bv br -> AxiWrS2M (a == 1) (w == 1) (if bv == 1 then Just br else Nothing))
                  <$> awReady <*> wReady <*> bValid <*> bResp
        GbMachine core hold timing rgb out m2s vol wrM = gbMachine WithStateHooks var five (zeroExtend <$> sw) psRaw s2m s2mW
        ar    = m2sAr <$> m2s
        aw    = m2sAw <$> wrM
        wch   = m2sW <$> wrM
        -- the picture: the Arty's ends
        frameTog = frameToggle core
        beat  = heartbeat (SNat @(MsToCycles DomPix 500))
        status = (\ok fr h b -> (if ok then 0x1 else 0) .|. (if fr then 0x2 else 0) .|. (if h then 0x4 else 0)
                                .|. (if b then 0x8 else 0) :: BitVector 4)
                   <$> (koCartOk <$> core) <*> frameTog <*> hold <*> beat
    in ( register 0 status
         -- HDMI mode (side track G phase 3): stereo in the data islands, each
         -- channel x 64 as Gb.Top's S/PDIF
       , hdmiVideo artyCts timing rgb (register (0, 0) (volumeStereo vol ((\(sl, sr) -> (resize sl `shiftL` 6, resize sr `shiftL` 6)) . koAudio <$> core)))
       , out
       , boolToBit . maybe False (const True) <$> ar
       , maybe 0 (pack . arAddr) <$> ar
       , maybe 0 (pack . arLen) <$> ar
       , boolToBit . m2sRReady <$> m2s
       , boolToBit . maybe False (const True) <$> aw
       , maybe 0 (pack . awAddr) <$> aw
       , maybe 0 (pack . awLen) <$> aw
       , boolToBit . maybe False (const True) <$> wch
       , maybe 0 wData <$> wch
       , maybe 0 wStrb <$> wch
       , boolToBit . maybe False wLast <$> wch
       , boolToBit . m2sBReady <$> wrM
         -- sound (phase 2 step 6): the APU's two channels mixed to one,
         -- (L + R) x 32 (Gb.Top's S/PDIF has each x 64), through the PWM pin
       , pwmOut (register 0 (volumeMono vol ((\(sl, sr) -> (resize sl + resize sr) `shiftL` 5 :: Signed 16) . koAudio <$> core)))
       , register 0 (pure 1)
       )
{-# INLINE gbArtyTop #-}
{-# ANN topEntity
  (Synthesize
    { t_name   = "gb_arty_top"
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
