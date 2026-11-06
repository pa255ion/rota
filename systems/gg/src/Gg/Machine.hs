{-# LANGUAGE FlexibleContexts #-}
{-|
The Game Gear machine both boards' tops share (PLAN.md steps 5.8, 5.9):
"Gg.Core" with its RAMs and VRAM in block RAM and the cartridge ROM in DDR
(at 'romBase', the running system's media region, docs/ARCHITECTURE.md),
read through "Platform.Mem.Ddr"'s port behind "Gg.RomPort", which keeps
the machine's timing exact (on a late miss the whole machine waits, and
catches up after); the picture ×3 into 640 × 480 ("Gg.Display"); the
PSG's output held between its clocks and filtered ("Gg.Audio"); and the
PS link (step 5.9): the desktop's and the USB keyboard's keys, the menu's
pause and text overlay, and loading. The tops add their own ends.

__Loading.__ The PS holds the system (@pgHold@), writes the ROM into DDR
and the cartridge word (bit 0 the mapper, 1 = Codemasters; bits 15-8 the
16 KiB banks - 1) to 'cartAddr' in the link map (platform/ps/gg_cart.zig),
then lets go. While held the machine gets no credit and the DDR port no
requests (its cache invalidated: the ROM changes under it); once the port
is idle, the core and the ROM port are reset, so the machine starts afresh
on the new cartridge when the hold falls. Video, audio and the DDR port
run on (the picture stays, HDMI keeps its lock, no AXI burst is cut). The
power-on word is @build/gg/cart.bin@ (@gg-image@), for a ROM put in DDR
without the PS (@make gg-prog@; in simulation the harness's
@--ddr-file 0x02000000=ROM@).
-}
module Gg.Machine
  ( GgMachine (..)
  , ggMachine
  , romBase
  , cartImage
  , systemGameGear
  , psKeys
  , cartAddr
  , cartByte
  ) where

import Clash.Prelude

import Gg.Audio (audioFilter)
import Gg.Core
import Gg.Hooks (HookOut (..), ggStateHooks)
import Gg.Display
import Platform.Speed (speedIncrement)
import Gg.RomPort
import Gg.Save
import Platform.Domains
import Platform.Mem.Axi
import Data.Maybe (fromMaybe, isJust)

import Platform.Mem.Ddr (DdrAddr, DdrStatus (..), ddrConfigDefault, ddrReadPort)
import Platform.Audio.Volume (volumeStereo)
import Platform.Ps.Link
import Platform.SaveState (StateHooks (..))
import Platform.Video.TextOverlay (textOverlay)
import Platform.Video.VgaTiming

-- | Where the cartridge ROM starts in DDR: the media region's start.
romBase :: DdrAddr
romBase = 0x02000000

cartImage :: FilePath
cartImage = "build/gg/cart.bin"

-- | Where the cartridge word goes in the Game Gear's link map (two bytes,
-- low first; platform/ps/gg_cart.zig @link_addr@).
cartAddr :: LinkAddr
cartAddr = 0x000000

-- | The Game Gear's system ID on PS_IN (docs/ARCHITECTURE.md, "Reserved
-- numbers").
systemGameGear :: SystemId
systemGameGear = 7

-- | The PS link's user bits (the desktop's and the USB keyboard's keys,
-- platform/ps/keymap.zig's @[gg]@ map: the Game Boy's bits, 0 right,
-- 1 left, 2 up, 3 down, 4 button 2, 5 button 1, 7 Start) as the machine's
-- pad (bits 0-5 up, down, left, right, 1, 2) and Start.
psKeys :: BitVector 16 -> (Unsigned 8, Bool)
psKeys k = (pad, testBit k 7)
 where
  pad = from 2 0 .|. from 3 1 .|. from 1 2 .|. from 0 3 .|. from 5 4 .|. from 4 5
  from i o = if testBit k i then bit o else 0

data GgMachine dom = GgMachine
  { gmTiming :: Signal dom VgaOut           -- ^ 640 × 480 timing
  , gmRgb    :: Signal dom Rgb888           -- ^ its colour
  , gmAudio  :: Signal dom (Signed 16, Signed 16) -- ^ the PSG, left and right, at the menu's volume (side track L)
  , gmFrame  :: Signal dom Bool             -- ^ toggles with each core frame
  , gmAxi    :: Signal dom AxiRdM2S         -- ^ the HP port's read master
  , gmFrozen :: Signal dom Bool             -- ^ no active clock for the core this clock
  , gmByte   :: Signal dom (Maybe (BitVector 8)) -- ^ for PS_IN's byte channel: cartridge RAM read back ("Gg.Save")
  , gmDirty  :: Signal dom Bool             -- ^ for PS_IN bit 6: the game wrote cartridge RAM since the PS cleared it
  }

-- | The machine from the board's pad (pressed: bits 0-5 up, down, left,
-- right, 1, 2) and Start, the PS link's lines (synchronised) and the HP
-- port's slave side; with save states ("Gg.Hooks", side track N) or as
-- before.
ggMachine
  :: HiddenClockResetEnable DomPix
  => StateHooks -> Signal DomPix (Unsigned 8) -> Signal DomPix Bool -> Signal DomPix PsGpio -> Signal DomPix AxiRdS2M
  -> GgMachine DomPix
ggMachine hooks padB startB ps s2m = GgMachine timingO rgbO (volumeStereo vol audio) frameTog m2s frozen chanByte' (scDirty <$> saveCtl)
 where
  -- the PS link: keys with the board's (none while the menu pauses),
  -- the menu's pause and overlay, the hold, the cartridge word
  link = psLink ps
  ctrl = psControl link
  sets = psSettings link
  vol = psVolume <$> sets
  pause = pcPause <$> ctrl
  hold = pgHold <$> ps
  keys = psKeys . pgUser <$> ps
  pad = (\p b (k, _) -> if p then 0 else b .|. k) <$> pause <*> padB <*> keys
  start = (\p s (_, k) -> not p && (s || k)) <$> pause <*> startB <*> keys
  -- bit 0 the mapper (1 = Codemasters), bits 15-8 the 16 KiB banks - 1:
  -- build/gg/cart.bin until the PS writes one
  cartFile = blockRamFile (SNat @1) cartImage (pure (0 :: Index 1)) (pure Nothing) :: Signal DomPix (BitVector 16)
  cartPs = register Nothing (cartByte <$> cartPs <*> cartFile <*> link)
  cart = fromMaybe <$> cartFile <*> cartPs
  mapper = (\b -> if testBit b 0 then MCodemasters else MSega) <$> cart
  banks = (\b -> resize (unpack (slice d15 d8 b) :: Unsigned 8) + 1) <$> cart
  -- held: no new DDR requests, no answers, the cache invalidated; the
  -- core and the ROM port reset once the port is idle (no burst cut)
  busy = stBusy <$> status
  inReset = register False ((\h b r -> h && (r || not b)) <$> hold <*> busy <*> inReset)
  machineRst = unsafeFromActiveHigh ((||) <$> unsafeToActiveHigh hasReset <*> inReset)
  -- the ROM: the DDR port behind the ROM port
  -- at the menu's speed (side track L; 100 %: runIncrement itself)
  -- paused or held: no credit, so the machine stops at its next clock without any
  rpIn = RomIn <$> (ggRomWant <$> core) <*> (ggTBoundary <$> core) <*> answer
               <*> ((\p h s -> if p || h then 0 else speedIncrement runIncrement s) <$> noCredit <*> hold <*> (psSpeed <$> sets))
  ro = withReset machineRst (mealy romPortStep romPortInit rpIn)
  ddrReq = (\h r -> if h then Nothing else (\a -> romBase + resize a) <$> roRequest r) <$> hold <*> ro
  (resp, m2s, status) = ddrReadPort (SNat @5) ddrConfigDefault hold ddrReq s2m
  answer = (\h r -> if h then Nothing else bytes <$> r) <$> hold <*> resp
  bytes :: AxiData -> Vec 8 (Unsigned 8)
  bytes w = reverse (map unpack (unpack w :: Vec 8 (BitVector 8)))
  frozen = not <$> active
  -- the core (its state for the ROM port) and its block RAMs
  gin = GgIn <$> active <*> (roByte <$> ro) <*> ramD <*> cartD <*> vramD <*> pad <*> start <*> mapper <*> banks <*> pure 0xFF <*> pure 0 <*> pure 1 <*> ovl
  -- save states (side track N, "Gg.Hooks"): the core's clock, the credit,
  -- RAM's and VRAM's ports, the overlay and the byte channel; without
  -- them each signal is as it was
  ho = ggStateHooks link pause inReset (pure False) core out active ramD vramD
  (noCredit, active, ovl, ramA, ramW, vramA, vramW, stateByte) = case hooks of
    NoStateHooks -> (pause, roRun <$> ro, pure Nothing, goRamAddr <$> out, goRamWrite <$> out, goVramRead <$> out, goVramWrite <$> out, pure Nothing)
    WithStateHooks -> ( hoStopped ho, (\r g -> r && not g) <$> (roRun <$> ro) <*> hoGate ho, hoOvl ho, hoRamAddr ho, hoRamWrite ho
                      , hoVramRead ho, hoVramWrite ho, hoByte ho )
  (core, out) = unbundle (withReset machineRst
                  (mealy (\s i -> let (s', o) = ggClock i s in (s', (s, o))) (ggPowerOn False MSega 8) gin))
  ramD = blockRam1 NoClearOnReset (SNat @8192) 0 ramA ramW
  -- cartridge RAM: port A the core (a read of its address whenever it
  -- doesn't write), port B the PS (the save loaded while held, read back
  -- while the game runs; "Gg.Save")
  coreCartOp :: GgOut -> RamOp 32768 (Unsigned 8)
  coreCartOp o = case goCartWrite o of
    Just (a, v) -> RamWrite (fromIntegral a) v
    Nothing -> RamRead (fromIntegral (goCartAddr o))
  psCartOp :: Bool -> Maybe (LinkAddr, BitVector 8) -> RamOp 32768 (Unsigned 8)
  psCartOp h w = case (cartLoad h w, cartRead w) of
    (Just (a, v), _) -> RamWrite (fromIntegral a) (unpack v)
    (_, Just a) -> RamRead (fromIntegral a)
    _ -> RamRead 0
  (cartD, cartB) = trueDualPortBlockRam (coreCartOp <$> out) (psCartOp <$> hold <*> link)
    :: (Signal DomPix (Unsigned 8), Signal DomPix (Unsigned 8))
  -- the save control and the dirty flag; a read asked for comes out on the
  -- byte channel a clock later (the block RAM's latency)
  saveCtl = register saveCtlInit (saveCtlStep <$> saveCtl <*> bundle (link, isJust . goCartWrite <$> out))
  cartRd = register False (isJust . cartRead <$> link)
  chanByte = (\sc rd b -> if scReadBack sc && rd then Just (pack b) else Nothing) <$> saveCtl <*> cartRd <*> cartB
  -- (and a state byte read, with the same control's bit)
  chanByte' = case hooks of
    NoStateHooks -> chanByte
    WithStateHooks -> (\c sc st -> case c of
                         Just _ -> c
                         Nothing -> if scReadBack sc then st else Nothing) <$> chanByte <*> saveCtl <*> stateByte
  bank k = blockRam1 NoClearOnReset (SNat @4096) 0 vramA (bankWrite k <$> vramW)
  vramD = bundle (bank 0 :> bank 1 :> bank 2 :> bank 3 :> Nil)
  vga = vgaTiming vga640x480
  fbData = frameBuffers (pixelWrite . goPixel <$> out) (goFrameEnd <$> out) (vgaVBlankStart <$> vga) fbAddrS
  (fbAddrS, (timing, rgb)) = ggStream vga fbData
  -- the menu's text over the picture
  (timingO, rgbO) = textOverlay (overlayWrite <$> link) (pcOverlay <$> ctrl) (timing, rgb)
  -- the PSG's level held between its clocks, then the analogue filters
  -- ("Gg.Audio": without them the 48 kHz samplers alias the PSG's edges)
  audio = audioFilter (regMaybe (0, 0) (goAudio <$> out))
  frameTog = register False (mux (goFrameEnd <$> out) (not <$> frameTog) frameTog)
  bankWrite :: Unsigned 2 -> Maybe (Unsigned 14, Unsigned 8) -> Maybe (Unsigned 12, Unsigned 8)
  bankWrite k w = case w of
    Just (a, b) | resize a == k -> Just (resize (a `shiftR` 2), b)
    _ -> Nothing

-- | A link write into the cartridge word ('cartAddr': low byte, then
-- high), the other byte kept (from the file's word at first).
cartByte :: Maybe (BitVector 16) -> BitVector 16 -> Maybe (LinkAddr, BitVector 8) -> Maybe (BitVector 16)
cartByte cur file w = case w of
  Just (a, d) | a == cartAddr -> Just (slice d15 d8 old ++# d)
              | a == cartAddr + 1 -> Just (d ++# slice d7 d0 old)
  _ -> cur
 where
  old = fromMaybe file cur
