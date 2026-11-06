{-|
The NES on the Arty Z7-20 (side track G phase 2, step 3), the
counterpart of "Nes.Top": the same machine (PRG ROM in DDR through HP0,
CHR in block RAM, loaded by the PS with its header decoded, the menu's
text overlay, the pause; CHR and the configuration built in from @make
hdl ROM=…@) with the Arty's I/O. The synthesis top is the wrapper
boards/arty-z7-20/nes_zynq.v (the Arty's PS7 with HP0,
boards/arty-z7-20/ps7_hp.tcl). Needs
boards/arty-z7-20/arty-z7-20.xdc and hdmi_tx.xdc.

__Kept in step with "Nes.Top" by hand__, as "Gb.ArtyTop" with "Gb.Top":
everything between the inputs and the picture is "Nes.Top"'s (as of main
044c516, side track D at rest); make ci's ci-arty builds this top.

The Arty's ends:

* __Buttons__, as "Chip8.ArtyTop": BTN0 = Left, BTN1 = Down, BTN2 = Up,
  BTN3 = Right, __SW1__ = A (the ZedBoard's BTNC); B, Select and Start
  from the desktop or the USB keyboard.
* __LEDs__: LD0 cartridge supported, LD1 toggles every NES frame, LD2
  loader hold, LD3 heartbeat (1 Hz).
* __Video__: "Nes.Top"'s stream through "Platform.Video.Tmds".
* __Sound__ (phase 2 step 6): "Nes.Top"'s mixer and filters (mono) through
  "Platform.Audio.Pwm" on the Arty's jack.
* __Save states__ (side track N, N.8; the Arty's only, "Nes.Top" has
  none): "Nes.StateHooks" around the core ('nesCoreWith'): the freeze,
  the state's windows on the link, the ROM overlay; the state's bytes on
  the byte channel of @PS_IN@. A hold that writes no file (a state's
  load) keeps the cartridge: the header, PRG in DDR and CHR stay.
-}
module Nes.ArtyTop
  ( topEntity
  ) where

import Clash.Prelude
import Data.Maybe (isJust)

import Nes.Audio (audioFilter, mixChannels)
import Nes.Core
import Nes.Display
import Nes.StateHooks (HookOut (..), nesStateHooks)
import Platform.Speed (speedEnable)
import Nes.Loader
import Nes.Mapper (CartCfg)
import Nes.Top (cartImage, chrImage, romBase)
import Platform.Audio.Pwm (pwmOut)
import Platform.Audio.Volume (volumeMono)
import Platform.Board.ArtyZ7 (TmdsPins (..), artyClocksPs, artyCts, tmdsOut)
import Platform.Domains
import Platform.Input.Debounce (syncDebounce)
import Platform.Input.Sync (synchronise)
import Platform.Mem.Axi
import Platform.Output.Leds (heartbeat)
import Platform.Ps.Link
import Platform.Video.TextOverlay (textOverlay)
import Platform.Video.Hdmi (hdmiVideo)
import Platform.Video.VgaTiming

-- | 'CartCfg' as loaded: nothing yet (the built-in cartridge), or the
-- header's decoding (Nothing: a mapper the core doesn't have), as in
-- "Nes.Top".
type Loaded = Maybe (Maybe CartCfg)

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
        link  = psLink ps
        ctrl  = psControl link
        sets  = psSettings link
        vol   = psVolume <$> sets
        pause = pcPause <$> ctrl
        db    = syncDebounce (SNat @(MsToCycles DomPix 10)) . fmap bitToBool
        -- C (A) from SW1, D/L/R/U from BTN1/BTN0/BTN3/BTN2
        bit' v i = (! (i :: Int)) <$> v
        (dbC, dbD, dbL, dbR, dbU) = (db (bit' swRaw 1), db (bit' btnRaw 1), db (bit' btnRaw 0),
                                     db (bit' btnRaw 3), db (bit' btnRaw 2))
        -- from here to the picture: Nes.Top's machine
        btn   = (\bc bu bd bl br -> (if bc then 0x01 else 0) .|. (if bu then 0x10 else 0) .|. (if bd then 0x20 else 0)
                                   .|. (if bl then 0x40 else 0) .|. (if br then 0x80 else 0) :: Unsigned 8)
                  <$> dbC <*> dbU <*> dbD <*> dbL <*> dbR
        pad1  = (\p b k -> if p then 0 else b .|. padFromLink (truncateB k)) <$> pause <*> btn <*> (pgUser <$> ps)
        hold  = pgHold <$> ps
        load  = loadRoute <$> header <*> hold <*> link
        header = register (repeat 0) (keepHeader <$> header <*> load)
        holdFell = (&&) <$> register False hold <*> (not <$> hold)
        holdRose = (&&) <$> hold <*> (not <$> register False hold)
        -- a hold that wrote no file (a save state's load, side track N)
        -- keeps the cartridge: its header isn't decoded again
        fileIn = register False ((\r l f -> not r && (f || isJust l)) <$> holdRose <*> load <*> fileIn)
        loaded = register (Nothing :: Loaded) (mux ((&&) <$> holdFell <*> fileIn) (Just . headerCfg <$> header) loaded)
        builtIn = unpack <$> blockRamFile (SNat @1) cartImage (pure (0 :: Index 1)) (pure Nothing)
        cfg   = (\ld b -> case ld of Just (Just x) -> x; _ -> b) <$> loaded <*> builtIn
        cartOk = (\ld -> case ld of Just Nothing -> False; _ -> True) <$> loaded
        run   = (&&) <$> (not <$> hold) <*> cartOk
        -- the menu's speed (side track L); no credit while paused, but on
        -- the way to a save state's freeze (side track N)
        dot   = (&&) <$> speedEnable nesDotIncrement (psSpeed <$> sets) <*> (not <$> hoStopped ho)
        s2m   = (\a v dat rsp lst -> AxiRdS2M (a == 1)
                   (if v == 1 then Just (AxiR dat rsp (lst == 1)) else Nothing))
                  <$> arReady <*> rValid <*> rData' <*> rResp' <*> rLast'
        (prgAns, m2s) = prgDdr romBase hold s2m (prgRequest <$> core)
        ar    = m2sAr <$> m2s
        (core, chans, coreSt, portRd) = nesCoreWith cfg prgAns chrImage run (chrWrite <$> load) dot pad1 (pure 0)
                                          (hoFrozen ho) (hoOvlOn ho) (hoOvlWr ho) (hoPort ho)
        ho    = nesStateHooks link pause (not <$> run) coreSt core portRd
        out   = register 0 (psInWith systemNes <$> bundle (dbC :> dbD :> dbL :> dbR :> dbU :> Nil) <*> (zeroExtend <$> sw)
                                                <*> psByteOut (hoByte ho))
        vga   = vgaTiming vga640x480
        fb    = nesFrameBuffers (koPixel <$> core) (koFrame <$> core) (vgaVBlankStart <$> vga) pixA lineA
        ((pixA, lineA), stream) = nesStream vga fb
        (timing, rgb) = textOverlay (overlayWrite <$> link) (pcOverlay <$> ctrl) stream
        -- the Arty's ends
        frameTog = register False (mux (koFrame <$> core) (not <$> frameTog) frameTog)
        beat  = heartbeat (SNat @(MsToCycles DomPix 500))
        status = (\ok fr h b -> (if ok then 0x1 else 0) .|. (if fr then 0x2 else 0) .|. (if h then 0x4 else 0)
                                .|. (if b then 0x8 else 0) :: BitVector 4)
                   <$> cartOk <*> frameTog <*> hold <*> beat
        -- sound (phase 2 step 6): Nes.Top's mixer and filters, through the
        -- PWM pin
        pcm   = audioFilter (mixChannels <$> chans)
        pcmR  = register 0 (volumeMono vol pcm)
    in ( register 0 status
         -- HDMI mode (phase 3): the mix on both channels in the data islands
       , hdmiVideo artyCts timing rgb ((\v -> (v, v)) <$> pcmR)
       , out, pwmOut pcmR, register 0 (pure 1)
       , boolToBit . maybe False (const True) <$> ar
       , maybe 0 (pack . arAddr) <$> ar
       , maybe 0 (pack . arLen) <$> ar
       , boolToBit . m2sRReady <$> m2s )
  keepHeader h w = case w of Just (LwHeader i v) -> replace i v h; _ -> h
  chrWrite w = case w of Just (LwChr a v) -> Just (a, unpack v); _ -> Nothing
{-# NOINLINE topEntity #-}
{-# ANN topEntity
  (Synthesize
    { t_name   = "nes_arty_top"
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
