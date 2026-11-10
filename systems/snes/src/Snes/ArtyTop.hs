{-# LANGUAGE CPP #-}
{-|
The Super Nintendo on the Arty Z7-20 (PLAN.md step 7.10;
docs/systems/snes.md): "Snes.Machine" with the picture on HDMI OUT, the
S-DSP's stereo in HDMI's audio and (mono) on the audio jack (PWM). The ROM
is in DDR, read over HP0; the PS link as the other systems' (keys, the
menu, loading).

Buttons: BTN0 left, BTN1 down, BTN2 up, BTN3 right (as the other
systems' tops); SW0 is B, SW1 Start. LEDs: LD0 the core's frames
(toggling), LD1 a row shown late (since the last reset: the line ring's
count not 0), LD3 the heartbeat.
-}
module Snes.ArtyTop
  ( topEntity
  ) where

import Clash.Prelude

import Platform.Audio.Pwm (pwmOut)
import Platform.Board.ArtyZ7 (TmdsPins (..), artyClocksPs, artyCts, tmdsOut)
import Platform.Domains
import Platform.Input.Debounce (syncDebounce)
import Platform.Input.Sync (synchronise)
import Platform.Mem.Axi
import Platform.Output.Leds (heartbeat)
import Platform.Ps.Link (psByteOut, psGpio, psInWith)
import Platform.Video.Hdmi (hdmiVideo)
import Snes.Machine
import Platform.SaveState (StateHooks (..))

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
  -> ( Signal DomPix (BitVector 4) -- led
     , Signal DomPix TmdsPins      -- hdmi_tx_*
     , Signal DomPix Bit           -- aud_pwm
     , Signal DomPix Bit           -- aud_sd
     , Clock DomPix                  -- HP0_ACLK
     , Signal DomPix Bit             -- HP0_ARVALID
     , Signal DomPix (BitVector 32)  -- HP0_ARADDR
     , Signal DomPix (BitVector 4)   -- HP0_ARLEN
     , Signal DomPix Bit             -- HP0_RREADY
     , Signal DomPix (BitVector 32)  -- PS_IN
     )
topEntity clk125 psResetN btnRaw swRaw psRaw arReady rValid rData' rResp' rLast' =
  (leds, tmdsOut clk clk5x rst syms, audPwm, audSd, clk, arValidO, arAddrO, arLenO, rReadyO, psOut)
 where
  (clk, clk5x, rst) = artyClocksPs clk125 (unsafeFromActiveHigh (pure False)) psResetN
  (leds, syms, audPwm, audSd, arValidO, arAddrO, arLenO, rReadyO, psOut) = withClockResetEnable clk rst enableGen $
    let db b = syncDebounce (SNat @(MsToCycles DomPix 10)) (bitToBool . (! (b :: Int)) <$> btnRaw)
        sw b = syncDebounce (SNat @(MsToCycles DomPix 10)) (bitToBool . (! (b :: Int)) <$> swRaw)
        (bl, bd, bu, br, s0, s1) = (db 0, db 1, db 2, db 3, sw 0, sw 1)
        -- the pad's bits: 11 B, 10 Y, 9 Select, 8 Start, 7 up, 6 down,
        -- 5 left, 4 right, 3 A, 2 X, 1 L, 0 R
        pad = (\l d u r b st -> (if b then 0x800 else 0) .|. (if st then 0x100 else 0) .|. (if u then 0x80 else 0)
                                 .|. (if d then 0x40 else 0) .|. (if l then 0x20 else 0) .|. (if r then 0x10 else 0) :: Unsigned 12)
                <$> bl <*> bd <*> bu <*> br <*> s0 <*> s1
        ps = psGpio <$> synchronise 0 psRaw
        s2m = (\a v dat rsp lst -> AxiRdS2M (a == 1) (if v == 1 then Just (AxiR dat rsp (lst == 1)) else Nothing))
                <$> arReady <*> rValid <*> rData' <*> rResp' <*> rLast'
#ifdef SNES_STATES_NONE
        m = snesMachine NoStateHooks pad ps s2m
#else
        m = snesMachine WithStateHooks pad ps s2m
#endif
        beat = heartbeat (SNat @(MsToCycles DomPix 500))
        status = (\f late b -> (if f then 0x1 else 0) .|. (if late /= 0 then 0x2 else 0) .|. (if b then 0x8 else 0) :: BitVector 4)
                   <$> snFrame m <*> snLate m <*> beat
    in ( register 0 status
       , hdmiVideo artyCts (snTiming m) (snRgb m) (snAudio m)
       , pwmOut (register 0 ((\(l, r) -> (l `shiftR` 1) + (r `shiftR` 1)) <$> snAudio m))
       , register 0 (pure 1)
       , boolToBit . maybe False (const True) . m2sAr <$> snAxi m
       , maybe 0 (pack . arAddr) . m2sAr <$> snAxi m
       , maybe 0 (pack . arLen) . m2sAr <$> snAxi m
       , boolToBit . m2sRReady <$> snAxi m
         -- the system ID and the board's buttons for the PS (no centre
         -- button), SRAM read back on the byte channel, bit 6 dirty
       , register 0 ((\w dirty -> if dirty then setBit w 6 else w) <$> (psInWith systemSnes <$> bundle (pure False :> bd :> bl :> br :> bu :> Nil)
                                        <*> (zeroExtend <$> synchronise 0 swRaw)
                                        <*> psByteOut (snByte m)) <*> snDirty m)
       )
{-# NOINLINE topEntity #-}
{-# ANN topEntity
  (Synthesize
    { t_name   = "snes_arty_top"
    , t_inputs = [ PortName "clk", PortName "PS_RESETN", PortName "btn", PortName "sw", PortName "PS_GPIO"
                 , PortName "HP0_ARREADY", PortName "HP0_RVALID", PortName "HP0_RDATA"
                 , PortName "HP0_RRESP", PortName "HP0_RLAST" ]
    , t_output = PortProduct ""
                 [ PortName "led"
                 , PortProduct ""
                     [ PortName "hdmi_tx_clk_p", PortName "hdmi_tx_clk_n"
                     , PortName "hdmi_tx_d_p", PortName "hdmi_tx_d_n" ]
                 , PortName "aud_pwm"
                 , PortName "aud_sd"
                 , PortName "HP0_ACLK"
                 , PortName "HP0_ARVALID", PortName "HP0_ARADDR", PortName "HP0_ARLEN"
                 , PortName "HP0_RREADY"
                 , PortName "PS_IN"
                 ]
    }) #-}
