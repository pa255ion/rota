{-|
The NES on the ZedBoard (PLAN.md steps 3.7 and 3.8): the whole core
("Nes.Core"), the picture on VGA and HDMI ("Nes.Display"), on the PS link
as the Game Boy ("Gb.Top"). Synthesis top with the PS:
boards/zedboard/nes_zynq.v.

* __Cartridge__ (side track D, for Kirby's Adventure): the @.nes@ file in
  DDR at 'romBase' (the media region), PRG ROM read from there through
  HP0 ('Nes.Core.prgDdr', the cache emptied while held); CHR in block RAM,
  from the image @nes-image@ makes from @ROM=@ (@build/nes/chr.bin@, and
  @cart.bin@, the configuration word, 'Nes.Mapper.CartCfg' packed). The PS
  replaces the cartridge while holding the core: the file into DDR, and
  over the PS link ("Nes.Loader": header and CHR; the PRG bytes the link
  may carry are ignored), from the SD card menu (@/nes@) or @make load
  SYSTEM=nes@; when hold falls the header is decoded again, and the core
  starts from power-on if the cartridge is supported, or stays held (LD0
  off). In @make sim@ the harness's DDR holds @ROM=@ (@--ddr-file@).
* __HP0__: the AXI3 read channels as pins of this top and its clock
  (@HP0_ACLK@ = the pixel clock), as "Gb.Top"; the wrapper
  boards/zedboard/nes_zynq.v connects them to the PS7 (ps7_hp.tcl).
* __PS link__: the menu's text overlay over the picture; the control
  register's pause stops the dot enable, so the machine stands still with
  its state kept while the menu is open, and the buttons don't reach it.
  @PS_IN@: the debounced buttons, the switches and 'systemNes'.
* __Timing__: everything in 'DomPix' (25.17623 MHz); the core gets a dot
  enable ('nesDotEnable', every 4 or 5 clocks) and runs a CPU cycle per
  three dots.
* __Input__ (docs/systems/nes.md, "Input"): controller 1 is the board
  buttons (BTNU, BTND, BTNL, BTNR = the D-pad, BTNC = A) ORed with the
  desktop's keys from the PS (@make keys SYSTEM=nes@, the Game Boy's key
  map, 'Nes.Loader.padFromLink').
* __Video__: VGA and HDMI from the same 24-bit stream.
* __Sound__ (step 3.9): the APU's channels through the mixer and the
  filters of "Nes.Audio", as 16-bit PCM sampled at 48 kHz by the S/PDIF
  transmitter on HD_SPDIF; the ADV7511 sends it in the HDMI stream (HDMI
  mode with audio, as the Game Boy).
* __LEDs__: SW6 off: LD0 cartridge supported, LD1 toggles every NES frame,
  LD2 loader hold, LD7 heartbeat (1 Hz); SW6 on: controller 1 (bit 0 A to
  bit 7 Right).
-}
module Nes.Top
  ( topEntity
  , romBase
  , chrImage
  , cartImage
  ) where

import Clash.Prelude

import Nes.Audio (audioFilter, mixChannels)
import Nes.Core
import Nes.Display
import Platform.Speed (speedEnable)
import Nes.Loader
import Nes.Mapper (CartCfg)
import Platform.Audio.Spdif (spdifDdr)
import Platform.Audio.Volume (volumeMono)
import Platform.Board.Zedboard
import Platform.Bus.I2c (I2cLines (..))
import Platform.Clocks (pixelClockGen)
import Platform.Domains
import Platform.Input.Debounce (syncDebounce)
import Platform.Input.Sync (synchronise)
import Platform.Mem.Axi
import Platform.Mem.Ddr (DdrAddr)
import Platform.Output.Leds (heartbeat, ledDriver)
import Platform.Ps.Link
import Platform.Video.Adv7511 (adv7511InitWith, advTiming, tableVecHdmiAudio)
import Platform.Video.TextOverlay (textOverlay)
import Platform.Video.VgaTiming
import Platform.Video.Ycbcr422

chrImage, cartImage :: FilePath
chrImage = "build/nes/chr.bin"
cartImage = "build/nes/cart.bin"

-- | Where the @.nes@ file starts in DDR: the start of the running
-- system's media region (docs/ARCHITECTURE.md, "Resource budgets").
romBase :: DdrAddr
romBase = 0x02000000

-- | The cartridge after the last load: 'Nothing' before the first (the
-- built-in one runs), then the decoded header ('Nothing' inside:
-- unsupported, the core stays held).
type Loaded = Maybe (Maybe CartCfg)

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
     , Signal DomPix (BitVector 32) -- PS_IN
     , Clock DomPix                  -- HP0_ACLK
     , Signal DomPix Bit             -- HP0_ARVALID
     , Signal DomPix (BitVector 32)  -- HP0_ARADDR
     , Signal DomPix (BitVector 4)   -- HP0_ARLEN
     , Signal DomPix Bit             -- HP0_RREADY
     )
topEntity gclk c d l r u swRaw psRaw sclPin sdaPin arReady rValid rData' rResp' rLast' =
  withClockResetEnable clk rst enableGen $
    let sw    = synchronise 0 swRaw
        ps    = psGpio <$> synchronise 0 psRaw
        link  = psLink ps
        ctrl  = psControl link
        sets  = psSettings link
        vol   = psVolume <$> sets
        pause = pcPause <$> ctrl
        db    = syncDebounce (SNat @(MsToCycles DomPix 10)) . fmap bitToBool
        (dbC, dbD, dbL, dbR, dbU) = (db c, db d, db l, db r, db u)
        -- controller bits: 0 A, 1 B, 2 Select, 3 Start, 4 Up, 5 Down, 6 Left, 7 Right
        btn   = (\bc bu bd bl br -> (if bc then 0x01 else 0) .|. (if bu then 0x10 else 0) .|. (if bd then 0x20 else 0)
                                   .|. (if bl then 0x40 else 0) .|. (if br then 0x80 else 0) :: Unsigned 8)
                  <$> dbC <*> dbU <*> dbD <*> dbL <*> dbR
        pad1  = (\p b k -> if p then 0 else b .|. padFromLink (truncateB k)) <$> pause <*> btn <*> (pgUser <$> ps)
        -- loading: the header, PRG and CHR while held; the header decoded
        -- when hold falls
        hold  = pgHold <$> ps
        load  = loadRoute <$> header <*> hold <*> link
        header = register (repeat 0) (keepHeader <$> header <*> load)
        holdFell = (&&) <$> register False hold <*> (not <$> hold)
        loaded = register (Nothing :: Loaded) (mux holdFell (Just . headerCfg <$> header) loaded)
        builtIn = unpack <$> blockRamFile (SNat @1) cartImage (pure (0 :: Index 1)) (pure Nothing)
        cfg   = (\ld b -> case ld of Just (Just x) -> x; _ -> b) <$> loaded <*> builtIn
        cartOk = (\ld -> case ld of Just Nothing -> False; _ -> True) <$> loaded
        run   = (&&) <$> (not <$> hold) <*> cartOk
        dot   = (&&) <$> speedEnable nesDotIncrement (psSpeed <$> sets) <*> (not <$> pause) -- the menu's speed (side track L)
        s2m   = (\a v dat rsp lst -> AxiRdS2M (a == 1)
                   (if v == 1 then Just (AxiR dat rsp (lst == 1)) else Nothing))
                  <$> arReady <*> rValid <*> rData' <*> rResp' <*> rLast'
        (prgAns, m2s) = prgDdr romBase hold s2m (prgRequest <$> core)
        ar    = m2sAr <$> m2s
        (core, chans) = nesCore cfg prgAns chrImage run (chrWrite <$> load) dot pad1 (pure 0)
        psOut = register 0 (psIn systemNes <$> bundle (dbC :> dbD :> dbL :> dbR :> dbU :> Nil) <*> sw)
        vga   = vgaTiming vga640x480
        fb    = nesFrameBuffers (koPixel <$> core) (koFrame <$> core) (vgaVBlankStart <$> vga) pixA lineA
        ((pixA, lineA), stream) = nesStream vga fb
        -- the PS menu's text screen over the picture, while it's shown
        (timing, rgb) = textOverlay (overlayWrite <$> link) (pcOverlay <$> ctrl) stream
        pins  = vgaPins vga640x480 timing rgb
        px    = (\o col -> PixelIn (vgaHSync o) (vgaVSync o) (vgaActive o)
                                  (if vgaActive o then col else black8))
                  <$> timing <*> rgb
        (hdmi, hdClk) = hdmiOut clk rst enableGen 1 1 (ycbcr422 1 1 px)
        (sclOut, sclIn) = openDrain sclPin (fst <$> i2cPins)
        (sdaOut, sdaIn) = openDrain sdaPin (snd <$> i2cPins)
        (i2cPins, _status) = adv7511InitWith tableVecHdmiAudio (advTiming @DomPix) (I2cLines <$> sclIn <*> sdaIn)
        -- sound: the mixer, the filters, both channels the same
        pcm   = audioFilter (mixChannels <$> chans)
        spdifPin = spdifDdr clk rst enableGen (register (0, 0) ((\v -> (v, v)) <$> volumeMono vol pcm))
        frameTog = register False (mux (koFrame <$> core) (not <$> frameTog) frameTog)
        beat  = heartbeat (SNat @(MsToCycles DomPix 500))
        status = (\ok fr h b -> (if ok then 0x01 else 0) .|. (if fr then 0x02 else 0) .|. (if h then 0x04 else 0)
                                .|. (if b then 0x80 else 0) :: BitVector 8)
                   <$> cartOk <*> frameTog <*> hold <*> beat
        leds  = ledDriver ((\s st p -> if testBit s 6 then pack p else st) <$> sw <*> status <*> pad1)
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
  -- No reset button: the MMCM's locked output (synchronised inside
  -- pixelClockGen) is the only reset.
  (clk, rst) = pixelClockGen gclk (unsafeFromActiveHigh (pure False))
  keepHeader h w = case w of Just (LwHeader i v) -> replace i v h; _ -> h
  chrWrite w = case w of Just (LwChr a v) -> Just (a, unpack v); _ -> Nothing
{-# NOINLINE topEntity #-}
{-# ANN topEntity
  (Synthesize
    { t_name   = "nes_top"
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
                 , PortName "PS_IN"
                 , PortName "HP0_ACLK"
                 , PortName "HP0_ARVALID", PortName "HP0_ARADDR", PortName "HP0_ARLEN"
                 , PortName "HP0_RREADY" ]
    }) #-}
