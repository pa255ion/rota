{-|
The Atari 800XL on the Arty Z7-20 (M4, the Arty port of steps 4.7 and
4.8; the Arty is the main development board from 2026-10-07): the
machine as "A800.System" puts it together, the same as "A800.Top"'s, with
the Arty's ends. The synthesis top is the wrapper
boards/arty-z7-20/a800xl_zynq.v (the Arty's PS7,
boards/arty-z7-20/ps7.tcl). Needs boards/arty-z7-20/arty-z7-20.xdc,
hdmi_tx.xdc and audio.xdc.

* __Buttons__ as the other systems' Arty tops: BTN0 = left, BTN1 = down,
  BTN2 = up, BTN3 = right, __SW1__ = the trigger (the ZedBoard's BTNC);
  the console keys only from the keyboard (F2-F4) or the desktop.
* __LEDs__: LD0 the program was accepted, LD1 toggles every frame, LD2
  the loader's hold, LD3 heartbeat (1 Hz).
* __Video and sound__: the 640 × 480 stream in HDMI mode
  ("Platform.Video.Hdmi", side track G phase 3), the sound on both
  channels in its data islands; the same sound through
  "Platform.Audio.Pwm" on the Arty's jack too.
* __Save states__ (side track N, "A800.Hooks"): built with them; the
  state's reads come back on PS_IN's byte channel.
* __Reset__: the PS's PL reset (@PS_RESETN@, FCLK_RESET0_N) as well as
  the clock's lock, as the other Arty tops ('artyClocksPs').
-}
module A800.ArtyTop
  ( topEntity
  ) where

import Clash.Prelude

import A800.System
import Platform.SaveState (StateHooks (..))
import Platform.Audio.Pwm (pwmOut)
import Platform.Board.ArtyZ7 (TmdsPins (..), artyClocksPs, artyCts, tmdsOut)
import Platform.Domains
import Platform.Input.Debounce (syncDebounce)
import Platform.Input.Sync (synchronise)
import Platform.Output.Leds (heartbeat)
import Platform.Mem.Axi (AxiAr (..), AxiR (..), AxiRdM2S (..), AxiRdS2M (..))
import Platform.Ps.Link
import Platform.Video.Hdmi (hdmiVideo)

topEntity
  :: Clock Dom125
  -> Signal DomPix Bit             -- PS_RESETN (FCLK_RESET0_N)
  -> Signal DomPix (BitVector 4)   -- btn
  -> Signal DomPix (BitVector 2)   -- sw
  -> Signal DomPix (BitVector 32)  -- PS_GPIO
  -- HP0's read channel (side track Q: the cartridge from DDR, as the NES's
  -- PRG ROM; "A800.CartDdr")
  -> Signal DomPix Bit             -- HP0_ARREADY
  -> Signal DomPix Bit             -- HP0_RVALID
  -> Signal DomPix (BitVector 64)  -- HP0_RDATA
  -> Signal DomPix (BitVector 2)   -- HP0_RRESP
  -> Signal DomPix Bit             -- HP0_RLAST
  -> ( Signal DomPix (BitVector 4)   -- led
     , Signal DomPix TmdsPins        -- hdmi_tx_*
     , Signal DomPix (BitVector 32)  -- PS_IN
     , Signal DomPix Bit             -- aud_pwm
     , Signal DomPix Bit             -- aud_sd
     , Clock DomPix                  -- HP0_ACLK
     , Signal DomPix Bit             -- HP0_ARVALID
     , Signal DomPix (BitVector 32)  -- HP0_ARADDR
     , Signal DomPix (BitVector 4)   -- HP0_ARLEN
     , Signal DomPix Bit             -- HP0_RREADY
     )
topEntity clk125 psResetN btnRaw swRaw psRaw arReady rValid rData' rResp' rLast' =
  ( leds, tmdsOut clk clk5x rst syms, psOut, audPwm, audSd
  , clk, arValid, arAddr', arLen', rReady )
 where
  -- the PS's PL reset too (PS_RESETN): see artyClocksPs
  (clk, clk5x, rst) = artyClocksPs clk125 (unsafeFromActiveHigh (pure False)) psResetN
  (leds, syms, psOut, audPwm, audSd, arValid, arAddr', arLen', rReady) = withClockResetEnable clk rst enableGen $
    let sw    = synchronise 0 swRaw
        ps    = psGpio <$> synchronise 0 psRaw
        db    = syncDebounce (SNat @(MsToCycles DomPix 10)) . fmap bitToBool
        bit' v i = (! (i :: Int)) <$> v
        -- the trigger (C) from SW1, the stick from BTN2/BTN1/BTN0/BTN3
        (dbC, dbD, dbL, dbR, dbU) = (db (bit' swRaw 1), db (bit' btnRaw 1), db (bit' btnRaw 0),
                                     db (bit' btnRaw 3), db (bit' btnRaw 2))
        board = Board <$> dbU <*> dbD <*> dbL <*> dbR <*> dbC <*> pure 0
        s2m   = (\a v dat rsp lst -> AxiRdS2M (a == 1)
                   (if v == 1 then Just (AxiR dat rsp (lst == 1)) else Nothing))
                  <$> arReady <*> rValid <*> rData' <*> rResp' <*> rLast'
        so    = a800System WithStateHooks (CartDdr s2m) ps board
        ar    = m2sAr <$> soDdr so
        -- the system ID, the buttons, the save states' reads on the byte
        -- channel (side track N)
        out   = register 0 (psInWith systemA800 <$> bundle (dbC :> dbD :> dbL :> dbR :> dbU :> Nil) <*> (zeroExtend <$> sw)
                                                <*> psByteOut (soByte so))
        beat  = heartbeat (SNat @(MsToCycles DomPix 500))
        status = (\ok fr h b -> (if ok then 0x1 else 0) .|. (if fr then 0x2 else 0) .|. (if h then 0x4 else 0)
                                .|. (if b then 0x8 else 0) :: BitVector 4)
                   <$> soOk so <*> soFrame so <*> soHold so <*> beat
        pcmR  = register 0 (soPcm so)
    in ( register 0 status
         -- HDMI mode (side track G phase 3): the sound on both channels in
         -- the data islands, and on the jack
       , hdmiVideo artyCts (soTiming so) (soRgb so) ((\v -> (v, v)) <$> pcmR)
       , out, pwmOut pcmR, register 0 (pure 1)
       , boolToBit . maybe False (const True) <$> ar
       , maybe 0 (pack . arAddr) <$> ar
       , maybe 0 (pack . arLen) <$> ar
       , boolToBit . m2sRReady <$> soDdr so )
{-# NOINLINE topEntity #-}
{-# ANN topEntity
  (Synthesize
    { t_name   = "a800xl_arty_top"
    , t_inputs = [ PortName "clk", PortName "PS_RESETN", PortName "btn", PortName "sw", PortName "PS_GPIO"
                 , PortName "HP0_ARREADY", PortName "HP0_RVALID", PortName "HP0_RDATA"
                 , PortName "HP0_RRESP", PortName "HP0_RLAST" ]
    , t_output = PortProduct ""
                 [ PortName "led"
                 , PortProduct ""
                     [ PortName "hdmi_tx_clk_p", PortName "hdmi_tx_clk_n"
                     , PortName "hdmi_tx_d_p", PortName "hdmi_tx_d_n" ]
                 , PortName "PS_IN"
                 , PortName "aud_pwm", PortName "aud_sd"
                 , PortName "HP0_ACLK"
                 , PortName "HP0_ARVALID", PortName "HP0_ARADDR", PortName "HP0_ARLEN"
                 , PortName "HP0_RREADY" ]
    }) #-}
