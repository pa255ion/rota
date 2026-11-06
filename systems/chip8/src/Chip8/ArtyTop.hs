{-|
CHIP-8 on the Arty Z7-20 (side track G step 5), the first system on the
second board: the picture on the Arty's HDMI output; the program baked
into the bitstream (loader stage A, as "Chip8.Top" with @make hdl
SYSTEM=chip8 BOARD=arty-z7-20 ROM=…@) and, since phase 2 (step 3), the
PS link as on the ZedBoard (@PS_GPIO@/@PS_IN@, "Platform.Ps.Link"): keys
from the desktop or a USB keyboard, ROM loading, the menu's text overlay,
the pause. The synthesis top is the wrapper
@boards/arty-z7-20/chip8_zynq.v@ (the Arty's PS7 + this). Needs
boards/arty-z7-20/arty-z7-20.xdc and hdmi_tx.xdc.

The Arty has 4 buttons and 2 switches instead of 5 and 8:

* __Buttons__: BTN0, BTN1, BTN2, BTN3 are the ZedBoard's BTNL, BTND,
  BTNU, BTNR, i.e. keys 4, 8, 2, 6 ("Chip8.Peripherals".buttonKeys).
* __SW1__ is BTNC, the shift: with it on, the buttons are keys A, F, E,
  B instead.
* __SW0__ is the ZedBoard's SW5: instruction rate (off 700 Hz, on
  10 kHz). The quirks are fixed at 0 (VIP), the ZedBoard's SW0 to SW4
  all off.
* __To the PS__ (the menu): the same mapping (SW1 as C), the switches
  in PS_IN's low switch bits, 'systemChip8'.
* __LEDs__: LD0 halted, LD1 waiting for a key, LD2 waiting for vertical
  blank, LD3 sound timer running ('coDebug' bits 12 to 14, 'coSound').

Video is "Chip8.Top"'s VGA stream (×10, letterboxed, amber) through
"Platform.Video.Tmds"; TMDS is lossless, so frames equal the CHIP-8 VGA
goldens exactly. Sound (phase 2 step 6): the 440 Hz beep as a ±6000 square
wave through "Platform.Audio.Pwm" on the Arty's mono jack (aud_pwm, the
amplifier on with aud_sd).
Save states (side track N, N.10; the Arty's only): "Chip8.StateHooks"
around the core ('chip8CoreWith'): the freeze between instructions, the
table into and out of the core's record, RAM and the framebuffer through
the state's windows; the state's bytes on PS_IN's byte channel.
Everything runs in 'DomPix'; all outputs are registered.
-}
module Chip8.ArtyTop
  ( topEntity
  , artyButtons
  , artyLeds
  ) where

import Clash.Prelude

import Chip8.Core
import Chip8.Display
import Chip8.StateHooks (HookOut (..), chip8StateHooks)
import Chip8.StateTap (tableWrite)
import Chip8.Loader (ramWrite)
import Chip8.Peripherals
import Chip8.Top (ramImage)
import Platform.Audio.Pwm (pwmOut)
import Platform.Board.ArtyZ7 (TmdsPins (..), artyClocksPs, artyCts, tmdsOut)
import Platform.Domains
import Platform.Input.Sync (synchronise)
import Platform.Ps.Link
import Platform.Video.TextOverlay (textOverlay)
import Platform.Video.Hdmi (hdmiVideo)
import Platform.Video.VgaTiming

-- | The Arty's buttons and SW1 as "Chip8.Peripherals"' button pins, in
-- its order (C, D, L, R, U): SW1 as C, BTN1 as D, BTN0 as L, BTN3 as R,
-- BTN2 as U.
artyButtons :: BitVector 4 -> BitVector 2 -> Vec 5 Bit
artyButtons btn sw = sw ! (1 :: Int) :> btn ! (1 :: Int) :> btn ! (0 :: Int)
                  :> btn ! (3 :: Int) :> btn ! (2 :: Int) :> Nil

-- | LD0..LD3 from the core's debug word and sound flag.
artyLeds :: BitVector 32 -> Bool -> BitVector 4
artyLeds dbg snd' = pack snd' ++# slice d14 d12 dbg

topEntity
  :: Clock Dom125
  -> Signal DomPix Bit             -- PS_RESETN (FCLK_RESET0_N)
  -> Signal DomPix (BitVector 4)  -- btn
  -> Signal DomPix (BitVector 2)  -- sw
  -> Signal DomPix (BitVector 32) -- PS_GPIO
  -> ( Signal DomPix (BitVector 4)  -- led
     , Signal DomPix TmdsPins       -- hdmi_tx_*
     , Signal DomPix (BitVector 32) -- PS_IN
     , Signal DomPix Bit            -- aud_pwm
     , Signal DomPix Bit            -- aud_sd
     )
topEntity clk125 psResetN btn swRaw psRaw = (leds, pins, psOut, audPwm, audSd)
 where
  -- the PS's PL reset too (PS_RESETN): see artyClocksPs
  (clk, clk5x, rst) = artyClocksPs clk125 (unsafeFromActiveHigh (pure False)) psResetN
  (leds, syms, psOut, audPwm, audSd) = withClockResetEnable clk rst enableGen $
    let sw    = synchronise 0 swRaw
        -- the PS link, as in Chip8.Top (side track G phase 2): keys from
        -- the desktop or the USB keyboard, ROM loading, the menu's text
        -- overlay and pause; 0 (nothing) in simulation without --ps
        ps    = psGpio <$> synchronise 0 psRaw
        link  = psLink ps
        ctrl  = psControl link
        pause = pcPause <$> ctrl
        load  = ramWrite <$> (pgHold <$> ps) <*> link
        btns  = debouncedButtons (SNat @(MsToCycles DomPix 10)) (artyButtons <$> btn <*> swRaw)
        keys  = (\p b k -> if p then 0 else keysOf b .|. k) <$> pause <*> btns <*> (pgUser <$> ps)
        instr = instrTicker (SNat @SlowPeriod) (SNat @FastPeriod) ((`testBit` 0) <$> sw)
        vga   = vgaTiming vga640x480
        inp   = mkIn <$> keys <*> instr <*> (vgaVBlankStart <$> vga) <*> fbVga
                     <*> (pgHold <$> ps) <*> pause <*> load
        mkIn k i t a hold p w = chip8InIdle
          { ciKeys = k, ciConfig = 0, ciInstrTick = i, ciTimerTick = t
          , ciRun = not hold && not p, ciReset = hold, ciRomWrite = w
          , ciFbAddr = a }
        (out, coreSt, ramD, fbD) = chip8CoreWith tableWrite (blockRamFile (SNat @4096) ramImage) inp
                                     (hoFrozen ho) (hoRecWr ho) (hoPort ho)
        ho    = chip8StateHooks link pause (pgHold <$> ps) coreSt ramD fbD
        (fbVga, stream) = chip8Stream amber vga (coFbRow <$> out)
        (timing, rgb) = textOverlay (overlayWrite <$> link) (pcOverlay <$> ctrl) stream
        smp   = register 0 ((\on b -> if not on then 0 else if b == 1 then 6000 else -6000 :: Signed 16)
                              <$> (coSound <$> out) <*> beeper (SNat @BeepHalfPeriod) (coSound <$> out))
    in ( register 0 (artyLeds <$> (coDebug <$> out) <*> (coSound <$> out))
         -- HDMI mode (phase 3): the beep in the data islands too
       , hdmiVideo artyCts timing rgb ((\v -> (v, v)) <$> smp)
         -- the menu's buttons (artyButtons: SW1 as C) and the switches
       , register 0 (psInWith systemChip8 <$> btns <*> (zeroExtend <$> sw) <*> psByteOut (hoByte ho))
         -- sound (phase 2 step 6): the 440 Hz beep through the PWM pin
       , pwmOut smp
       , register 0 (pure 1) )
  pins = tmdsOut clk clk5x rst syms
{-# NOINLINE topEntity #-}
{-# ANN topEntity
  (Synthesize
    { t_name   = "chip8_arty_top"
    , t_inputs = [ PortName "clk", PortName "PS_RESETN", PortName "btn", PortName "sw", PortName "PS_GPIO" ]
    , t_output = PortProduct ""
                 [ PortName "led"
                 , PortProduct ""
                     [ PortName "hdmi_tx_clk_p", PortName "hdmi_tx_clk_n"
                     , PortName "hdmi_tx_d_p", PortName "hdmi_tx_d_n" ]
                 , PortName "PS_IN"
                 , PortName "aud_pwm", PortName "aud_sd" ]
    }) #-}
