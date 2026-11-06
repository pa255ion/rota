{-# LANGUAGE FlexibleContexts #-}
{-|
The Game Boy's machine without a board (side track I, phase 3): everything
from the buttons and the PS link to the picture, shared by the ZedBoard's
tops ("Gb.Top", "Gb.TopCgb") and the Arty's ("Gb.ArtyTop",
"Gb.ArtyTopCgb"), which add only their board's ends (clocks, video and
sound out, LEDs). Until phase 3, "Gb.ArtyTop" kept a copy in step by hand
(side track G phase 2).

* __Cartridge__ (side track I, phase 2): ROM in DDR, up to 8 MiB from
  'romBase' (the running system's media region, docs/ARCHITECTURE.md),
  read through the Zynq's HP0 port by "Platform.Mem.Ddr" (a 1 KiB line
  cache with prefetch). The PS writes it there while holding the core,
  and the cache is emptied while held. The core waits for each ROM byte
  ("Gb.Core", 'gbCoreWith'): a cache hit costs nothing, a miss stops its
  time. The core reads the cartridge header when it starts.
* __PS link__ (side track A): the menu's text overlay over the picture;
  the control register's pause stops the dot enable, so the whole machine
  stands still with its state kept while the menu is open, and the keys
  don't reach it. @PS_IN@: the debounced buttons, the switches and the
  variant's system ID, and on the byte channel every byte the game sends
  on its serial port.
* __Battery saves__ (side track I): the PS writes a @.sav@ into cartridge
  RAM while holding the core, and reads it back while the game runs
  through cartridge RAM's second port (map in "Gb.Loader"); @PS_IN@ bit 6
  is the dirty flag.
* __Video__: the 160×144 picture ×3 at (80, 24) of 640×480, triple
  buffered, in the variant's colours, under the menu's overlay.
* __Variants__: 'dmg' (the Game Boy: shades, SW0 the palette) and 'cgb'
  (the Game Boy Color: 15-bit colours, pipelined into the frame buffers).
-}
module Gb.Machine
  ( romBase
  , GbVariant (..)
  , dmg
  , cgb
  , GbMachine (..)
  , gbMachine
  , frameToggle
  ) where

import Clash.Prelude
import Data.Maybe (isJust)

import Gb.Core
import Gb.Display
import Platform.Speed (speedEnable)
import Gb.Loader (SaveCtl (..), cramLoad, cramRead, palLoad, rtcLoad, rtcRead, saveCtlInit, saveCtlStep, stateModeAddr, stateRBase, stateWBase)
import Platform.SaveState (StateHooks (..), stateWindow)
import Gb.StateTap (gbStateTap)
import Platform.Domains
import Platform.Input.Debounce (syncDebounce)
import Platform.Input.Sync (synchronise)
import Platform.Mem.Axi
import Platform.Mem.Ddr (DdrAddr, ddrByte, ddrConfigDefault, ddrReadPort)
import Platform.Ps.FrameInput (frameAdvance, frameInput, pmSync, psInFrame, psMovie)
import Platform.Ps.Link
import Platform.Video.Capture (frameCapture)
import Platform.Video.FrameBuffers (tripleBuffer)
import Platform.Video.TextOverlay (textOverlay)
import Platform.Video.VgaTiming

-- | Where the cartridge ROM starts in DDR: the start of the running
-- system's media region (docs/ARCHITECTURE.md, "Resource budgets").
romBase :: DdrAddr
romBase = 0x02000000

-- | What differs between the Game Boy and the Game Boy Color: the core's
-- model, the system ID for the PS app, the core's pixels into the frame
-- buffers (@w@ bits each) with the end of each frame after its last pixel
-- (the CGB pipelines its colour), and their colour on screen (with the
-- switches).
data GbVariant w = GbVariant
  { gvModel  :: CoreModel
  , gvSystem :: SystemId
  , gvVideo  :: forall dom. HiddenClockResetEnable dom
             => Signal dom CoreOut -> (Signal dom (Maybe (Index 23040, BitVector w)), Signal dom Bool)
  , gvColour :: BitVector 8 -> BitVector w -> Rgb888
  }

-- | The DMG: shades, SW0 the palette (grey or green).
dmg :: GbVariant 2
dmg = GbVariant CoreDmg systemGameBoy (\c -> (koPixel <$> c, koFrame <$> c))
  (\s c -> fromRgb444 (palette (if testBit s 0 then Green else Grey) c))

-- | The Game Boy Color. Its colour is pipelined (make bit: the PPU's
-- pixel, the palette RAM lookup and the frame buffer's write in one clock
-- missed by 0.7 ns): the palette byte's index is registered, the colour
-- looked up in the core's palette RAM the next clock (it changes only in
-- the CPU's accesses, never between two dots) and registered again; the
-- frame's end follows its last pixel by the same two clocks. No colour
-- correction ('rgb15'); the switches do nothing.
cgb :: GbVariant 15
cgb = GbVariant CoreCgb systemGameBoyColor video (\_ c -> rgb15 c)
 where
  video core = (pix2, done2)
   where
    pix1 = register Nothing (koPixelP <$> core)
    pix2 = register Nothing ((\p pal -> (\(i, k) -> (i, palColour pal k)) <$> p) <$> pix1 <*> (koPalette <$> core))
    done2 = register False (register False (koFrame <$> core))

-- | What the machine gives its board.
data GbMachine dom = GbMachine
  { gmCore   :: Signal dom CoreOut
  , gmHold   :: Signal dom Bool        -- ^ the PS holds the core (loading)
  , gmTiming :: Signal dom VgaOut      -- ^ the picture's 640×480 timing ...
  , gmRgb    :: Signal dom Rgb888      -- ^ ... and colour, the menu's overlay on top
  , gmPsIn   :: Signal dom (BitVector 32) -- ^ PS_IN (registered)
  , gmM2s    :: Signal dom AxiRdM2S    -- ^ HP0's read channels, to the PS
  , gmVolume :: Signal dom Volume      -- ^ the menu's volume (side track L; "Platform.Audio.Volume")
  , gmWr     :: Signal dom AxiWrM2S    -- ^ HP0's write channels: screenshots (side track O, O.4)
  }

-- | The machine: the five buttons (raw: C = A, D, L, R, U; debounced
-- here, 10 ms), the switches (for @PS_IN@ and the variant's colour), the
-- PS's GPIO word, and HP0's answers. With 'WithStateHooks' (side track
-- L), save states: the state mode and windows of "Gb.Loader" decoded,
-- 'gbCoreHooked''s hooks; with 'NoStateHooks' the machine as before.
gbMachine
  :: forall w. KnownNat w
  => HiddenClockResetEnable DomPix
  => StateHooks
  -> GbVariant w
  -> Vec 5 (Signal DomPix Bit)
  -> Signal DomPix (BitVector 8)
  -> Signal DomPix (BitVector 32)
  -> Signal DomPix AxiRdS2M
  -> Signal DomPix AxiWrS2M
  -> GbMachine DomPix
gbMachine hooks var five sw psRaw s2m s2mW = GbMachine core hold timing rgb psOut m2s vol wr
 where
  ps    = psGpio <$> synchronise 0 psRaw
  link  = psLink ps
  ctrl  = psControl link
  sets  = psSettings link
  vol   = psVolume <$> sets
  -- (side track O, O.3) frame advance: a step runs the paused core to its
  -- next frame start
  pause = frameAdvance link fbDone (pcPause <$> ctrl)
  db    = syncDebounce (SNat @(MsToCycles DomPix 10)) . fmap bitToBool
  dbs   = map db five
  (dbC, dbD, dbL, dbR, dbU) = (dbs !! (0 :: Int), dbs !! (1 :: Int), dbs !! (2 :: Int), dbs !! (3 :: Int), dbs !! (4 :: Int))
  btn   = (\br bl bu bd bc -> (if br then 0x01 else 0) .|. (if bl then 0x02 else 0) .|. (if bu then 0x04 else 0)
                             .|. (if bd then 0x08 else 0) .|. (if bc then 0x10 else 0) :: BitVector 8)
            <$> dbR <*> dbL <*> dbU <*> dbD <*> dbC
  -- (side track O, O.1) the PS's input word latched at each of the core's
  -- frame starts while a movie records or plays (the movie register's bit
  -- 0), else as it comes; the frame toggle goes to PS_IN bit 7. While
  -- held it follows the word (O.2b: a movie's power-on start)
  (psKeys, frameT) = frameInput hold (pmSync <$> psMovie link) fbDone (pgUser <$> ps)
  keys  = (\p b k -> if p then 0 else b .|. k) <$> pause <*> btn <*> (truncateB <$> psKeys)
  hold  = pgHold <$> ps
  -- paused (menu open): no dots, so no M-cycles; the state stays
  -- at the menu's speed (side track L; 100 %: dotEnable's own clocks).
  -- With save states, a core asked to freeze runs on (paused, without
  -- keys) to a moment where a state can be taken (Gb.Core.restoreable:
  -- within about a frame), and freezes there (L.6c)
  dot   = (&&) <$> speedEnable dotIncrement (psSpeed <$> sets) <*> (not <$> stopped)
  stopped = case hooks of
    NoStateHooks -> pause
    WithStateHooks -> (\p m z -> p && not (m && not z)) <$> pause <*> stMode <*> frozen
  -- cartridge RAM for the PS (battery saves): a .sav written while held,
  -- reads at any time ("Gb.Loader")
  psCram = (\h w -> case (cramLoad h w, cramRead w) of
              (Just (a, v), _) -> Just (a, Just v)
              (_, Just a) -> Just (a, Nothing)
              _ -> Nothing) <$> cramHeld <*> link
  -- cartridge RAM takes the PS's writes while held, and while frozen for a
  -- state's load (side track L)
  cramHeld = case hooks of
    NoStateHooks -> hold
    WithStateHooks -> (||) <$> hold <*> frozen
  -- save states (side track L): the state mode (frozen once paused and at
  -- rest) and the accesses through the windows
  stMode = register False ((\w m -> case w of
                              Just (a, d) | a == stateModeAddr -> testBit d 0
                              _ -> m) <$> link <*> stMode)
  freeze = (&&) <$> stMode <*> pause
  -- MBC3's clock at start, from the save file (written while held)
  rtcInit = register (repeat 0) ((\v w -> maybe v (\(i, x) -> replace i x v) w) <$> rtcInit <*> (rtcLoad <$> hold <*> link))
  -- the Game Boy Color's palettes for a DMG cartridge (written while held;
  -- our default until the PS writes others)
  palInit = register gbcDefaultPal ((\v w -> maybe v (\(i, x) -> replace i x v) w) <$> palInit <*> (palLoad <$> hold <*> link))
  -- without save states the core as it was ('gbCoreWith'); with them its
  -- hooks ('gbCoreHooked', the state table from "Gb.StateTap")
  (core, cramB, romReq, stByte, frozen) = case hooks of
    NoStateHooks ->
      let (c, b, r) = gbCoreWith (gvModel var) romAns (not <$> hold) dot keys rtcInit palInit psCram
      in (c, b, r, pure Nothing, pure False)
    WithStateHooks ->
      gbCoreHooked (Just gbStateTap) (gvModel var) romAns (not <$> hold) dot keys rtcInit palInit psCram freeze
                   (stateWindow stateWBase stateRBase <$> link)
  -- the cartridge ROM in DDR: one request at a time ("Gb.Core"), the
  -- answer's byte picked from its word; the cache emptied while held
  ddrReq = fmap (\a -> romBase + resize a) <$> romReq
  (ddrResp, m2s, _ddrStatus) = ddrReadPort (SNat @5) ddrConfigDefault hold ddrReq s2m
  lastReq = regMaybe 0 ddrReq
  romAns = (\w a -> ddrByte a <$> w) <$> ddrResp <*> lastReq
  -- the clock read back: byte 0 takes a snapshot of all ten
  rtcSnap = register (repeat 0) ((\w now v -> if w == Just 0 then now else v) <$> (rtcRead <$> link) <*> (koRtc <$> core) <*> rtcSnap)
  rtcByte = register Nothing ((\w now v -> (\i -> if i == 0 then head now else v !! i) <$> w)
                                <$> (rtcRead <$> link) <*> (koRtc <$> core) <*> rtcSnap)
  gameCramWrite = (\q -> case q of
                     Just m -> mqSpace m == SpCram && isJust (mqWrite m)
                     Nothing -> False) . koMem <$> core
  saveCtl = register saveCtlInit (saveCtlStep <$> saveCtl <*> bundle (link, gameCramWrite))
  -- a read's byte, the clock after it was asked for
  cramRd = register False (isJust . cramRead <$> link)
  cramByte0 = (\asked b rtcB -> if asked then Just b else rtcB) <$> cramRd <*> cramB <*> rtcByte
  -- a state's byte too (side track L), on the same channel
  cramByte = case hooks of
    NoStateHooks -> cramByte0
    WithStateHooks -> (\sb cb -> maybe cb Just sb) <$> stByte <*> cramByte0
  -- PS_IN, with the game's serial bytes on the byte channel, or cartridge
  -- RAM's bytes for the PS (save control bit 0); bit 6 the dirty flag
  chanByte = (\sc ser rb -> if scReadBack sc then rb else ser) <$> saveCtl <*> (koSerial <$> core) <*> cramByte
  psOut = register 0 ((\w sc t -> psInFrame t (if scDirty sc then setBit w 6 else w))
                        <$> (psInWith (gvSystem var) <$> bundle (dbC :> dbD :> dbL :> dbR :> dbU :> Nil) <*> sw
                                                     <*> psByteOut chanByte)
                        <*> saveCtl <*> frameT)
  vga   = vgaTiming vga640x480
  (fbWrite, fbDone) = gvVideo var core
  -- (side track O, O.4) a screenshot: the frame buffer's writes in the
  -- colour the screen shows, into DDR when the PS asks
  (wr, _capturing) = frameCapture 160 144 link fbDone
    ((\s -> fmap (\(i, c) -> (i, rgb24 (gvColour var s c)))) <$> sw <*> fbWrite) s2mW
  rgb24 (Rgb888 r g b) = r ++# g ++# b
  fbData = tripleBuffer (SNat @23040) fbWrite fbDone (vgaVBlankStart <$> vga) fbAddr
  (fbAddr, stream) = gbStreamWith (gvColour var) sw vga fbData
  -- the PS menu's text screen over the picture, while it's shown
  (timing, rgb) = textOverlay (overlayWrite <$> link) (pcOverlay <$> ctrl) stream
{-# INLINE gbMachine #-}

-- | Toggles every Game Boy frame (an LED).
frameToggle :: HiddenClockResetEnable dom => Signal dom CoreOut -> Signal dom Bool
frameToggle core = t
 where
  t = register False (mux (koFrame <$> core) (not <$> t) t)
