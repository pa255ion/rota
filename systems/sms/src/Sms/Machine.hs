{-# LANGUAGE FlexibleContexts #-}
{-|
The Master System machine both boards' tops share (M6 step 6.6), the
Game Gear's ("Gg.Machine") for a Master System II: "Gg.Core" powered on as
one ('smsPowerOn'), its RAMs and VRAM in block RAM, the cartridge ROM and
the BIOS in DDR (at 'romBase' and 'romBase' + 1 MiB) through
"Platform.Mem.Ddr"'s port behind "Gg.RomPort" (the machine waits on a
late miss, and catches up after); the FM unit ("Sms.Fm", stepped every
fourth T-state, fed the core's writes to F0h-F1h); the picture ×2 into
640 × 480 with its border ("Sms.Display"); the PSG and the FM unit mixed
as F2h says and filtered ("Gg.Audio"); and the PS link: keys, the menu's
pause and overlay, loading, battery saves ("Gg.Save"), as the Game Gear's.

__Loading__ as the Game Gear's: the PS holds the system, writes the ROM
(and a BIOS image, if the card has one) into DDR and the cartridge word
(bit 0 the mapper, 1 = Codemasters; bits 7-3 the BIOS's 16 KiB banks, 0
for none; bits 15-8 the ROM's 16 KiB banks - 1) to 'cartAddr', then lets
go. The power-on word is @build/sms/cart.bin@ (@gg-image ROM DIR [--bios
FILE]@).

__Not on the board:__ with the cartridge and the BIOS both enabled, the
spec and the core AND their bytes; the board's ROM port serves one byte a
cycle and reads the cartridge's alone (no BIOS or game leaves both on).
-}
module Sms.Machine
  ( SmsMachine (..)
  , smsMachine
  , romBase
  , cartImage
  , systemSms
  , smsPsKeys
  , cartAddr
  ) where

import Clash.Prelude

import Gg.Audio (audioFilter)
import Gg.Core
import Gg.Display (runIncrement)
import Gg.Hooks (HookOut (..), ggStateHooks)
import Gg.RomPort
import Platform.Speed (speedIncrement)
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
import Sms.Display
import Sms.Fm

-- | Where the cartridge ROM starts in DDR (the media region's start); the
-- BIOS follows at 1 MiB ('RomAddr' 100000h).
romBase :: DdrAddr
romBase = 0x02000000

cartImage :: FilePath
cartImage = "build/sms/cart.bin"

-- | The cartridge word's place in the Master System's link map (two bytes,
-- low first), as the Game Gear's.
cartAddr :: LinkAddr
cartAddr = 0x000000

-- | The Master System's system ID on PS_IN (docs/ARCHITECTURE.md,
-- "Reserved numbers").
systemSms :: SystemId
systemSms = 8

-- | The PS link's user bits (platform/ps/keymap.zig's @[sms]@, step 6.7):
-- pad 1 as the Game Gear's (the Game Boy's bits: 0 right, 1 left, 2 up, 3
-- down, 4 button 2, 5 button 1), 7 Pause; pad 2 the same in bits 8-13.
-- Out: pad 1, pad 2 (bits 0-5 up, down, left, right, 1, 2) and Pause.
smsPsKeys :: BitVector 16 -> (Unsigned 8, Unsigned 8, Bool)
smsPsKeys k = (pad 0, pad 8, testBit k 7)
 where
  pad o = from (o + 2) 0 .|. from (o + 3) 1 .|. from (o + 1) 2 .|. from o 3 .|. from (o + 5) 4 .|. from (o + 4) 5
  from i b = if testBit k i then bit b else 0

data SmsMachine dom = SmsMachine
  { smTiming :: Signal dom VgaOut           -- ^ 640 × 480 timing
  , smRgb    :: Signal dom Rgb888           -- ^ its colour
  , smAudio  :: Signal dom (Signed 16, Signed 16) -- ^ the PSG and the FM unit (mono: both sides the same), at the menu's volume (side track L)
  , smFrame  :: Signal dom Bool             -- ^ toggles with each core frame
  , smAxi    :: Signal dom AxiRdM2S         -- ^ the HP port's read master
  , smFrozen :: Signal dom Bool             -- ^ no active clock for the core this clock
  , smByte   :: Signal dom (Maybe (BitVector 8)) -- ^ for PS_IN's byte channel: cartridge RAM read back
  , smDirty  :: Signal dom Bool             -- ^ for PS_IN bit 6: the game wrote cartridge RAM since the PS cleared it
  }

-- | The machine from the board's pad 1 (pressed: bits 0-5 up, down, left,
-- right, 1, 2) and Pause, the PS link's lines (synchronised) and the HP
-- port's slave side; with save states ("Gg.Hooks", side track N) or as
-- before.
smsMachine
  :: HiddenClockResetEnable DomPix
  => StateHooks -> Signal DomPix (Unsigned 8) -> Signal DomPix Bool -> Signal DomPix PsGpio -> Signal DomPix AxiRdS2M
  -> SmsMachine DomPix
smsMachine hooks padB pauseB ps s2m = SmsMachine timingO rgbO (volumeStereo vol audio) frameTog m2s frozen chanByte' (scDirty <$> saveCtl)
 where
  -- the PS link: keys with the board's (none while the menu pauses), the
  -- menu's pause and overlay, the hold, the cartridge word
  link = psLink ps
  ctrl = psControl link
  sets = psSettings link
  vol = psVolume <$> sets
  menuPause = pcPause <$> ctrl
  hold = pgHold <$> ps
  keys = smsPsKeys . pgUser <$> ps
  pad1 = (\p b (k, _, _) -> if p then 0 else b .|. k) <$> menuPause <*> padB <*> keys
  pad2 = (\p (_, k, _) -> if p then 0 else k) <$> menuPause <*> keys
  pauseK = (\p s (_, _, k) -> not p && (s || k)) <$> menuPause <*> pauseB <*> keys
  cartFile = blockRamFile (SNat @1) cartImage (pure (0 :: Index 1)) (pure Nothing) :: Signal DomPix (BitVector 16)
  cartPs = register Nothing (cartWord <$> cartPs <*> cartFile <*> link)
  cart = fromMaybe <$> cartFile <*> cartPs
  mapper = (\b -> if testBit b 0 then MCodemasters else MSega) <$> cart
  banks = (\b -> resize (unpack (slice d15 d8 b) :: Unsigned 8) + 1) <$> cart
  biosBanks = (\b -> unpack (slice d7 d3 b) :: Unsigned 5) <$> cart
  -- held: no DDR requests, the cache invalidated; the core, the ROM port and
  -- the FM unit reset once the port is idle
  busy = stBusy <$> status
  inReset = register False ((\h b r -> h && (r || not b)) <$> hold <*> busy <*> inReset)
  machineRst = unsafeFromActiveHigh ((||) <$> unsafeToActiveHigh hasReset <*> inReset)
  -- the ROM (and the BIOS): the DDR port behind the ROM port, at the
  -- menu's speed (side track L; 100 %: runIncrement itself; the FM unit
  -- follows the core's T-states)
  rpIn = RomIn <$> (ggRomWant <$> core) <*> (ggTBoundary <$> core) <*> answer
               <*> ((\p h s -> if p || h then 0 else speedIncrement runIncrement s) <$> noCredit <*> hold <*> (psSpeed <$> sets))
  ro = withReset machineRst (mealy romPortStep romPortInit rpIn)
  ddrReq = (\h r -> if h then Nothing else (\a -> romBase + resize a) <$> roRequest r) <$> hold <*> ro
  (resp, m2s, status) = ddrReadPort (SNat @5) ddrConfigDefault hold ddrReq s2m
  answer = (\h r -> if h then Nothing else bytes <$> r) <$> hold <*> resp
  bytes :: AxiData -> Vec 8 (Unsigned 8)
  bytes w = reverse (map unpack (unpack w :: Vec 8 (BitVector 8)))
  frozen = not <$> active
  -- save states (side track N, "Gg.Hooks"; the pause button is /NMI): the
  -- core's clock, the credit, RAM's and VRAM's ports, the overlay and the
  -- byte channel; without them each signal is as it was
  ho = ggStateHooks link menuPause inReset pauseK core out active ramD vramD
  (noCredit, active, ovl, ramA, ramW, vramA, vramW, stateByte) = case hooks of
    NoStateHooks -> (menuPause, roRun <$> ro, pure Nothing, goRamAddr <$> out, goRamWrite <$> out, goVramRead <$> out, goVramWrite <$> out, pure Nothing)
    WithStateHooks -> ( hoStopped ho, (\r g -> r && not g) <$> (roRun <$> ro) <*> hoGate ho, hoOvl ho, hoRamAddr ho, hoRamWrite ho
                      , hoVramRead ho, hoVramWrite ho, hoByte ho )
  -- the core, a Master System II with the FM unit fitted (the second ROM
  -- byte FFh: see "Not on the board" above)
  gin = GgIn <$> active <*> (roByte <$> ro) <*> ramD <*> cartD <*> vramD <*> pad1 <*> pauseK <*> mapper <*> banks
             <*> pure 0xFF <*> pad2 <*> biosBanks <*> ovl
  (core, out) = unbundle (withReset machineRst
                  (mealy (\s i -> let (s', o) = ggClock i s in (s', (s, o))) (smsPowerOn False True MSega 8) gin))
  ramD = blockRam1 NoClearOnReset (SNat @8192) 0 ramA ramW
  -- cartridge RAM, the save control, the read back: as the Game Gear's
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
  -- the picture and its border (taken at each frame's end)
  vga = vgaTiming vga640x480
  fbData = smsFrameBuffers (smsPixelWrite . goPixel <$> out) (goFrameEnd <$> out) (vgaVBlankStart <$> vga) fbAddrS
  border = regEn 0 (goFrameEnd <$> out) (goBorder <$> out)
  (fbAddrS, (timing, rgb)) = smsStream vga fbData border
  (timingO, rgbO) = textOverlay (overlayWrite <$> link) (pcOverlay <$> ctrl) (timing, rgb)
  -- the FM unit: a cycle every fourth T-state, fed the write of the cycle
  -- that ended; F2h's two bits kept here for the mix
  fm = withReset machineRst (mealy fmStep fmMachInit (bundle (goTState <$> out, goFm <$> out)))
  fmLevel = fst <$> fm
  fmCtl = snd <$> fm
  -- the PSG's level held between its clocks; the mix as F2h says (bit 0
  -- the FM unit, the PSG while bit 1 = bit 0); the analogue filters
  psg = regMaybe (0, 0) (goAudio <$> out)
  mixed = (\(l, r) f c -> let psgOn = testBit c 1 == testBit c 0
                              fmOn = testBit c 0
                              mix a = sat16 ((if psgOn then resize a else 0) + (if fmOn then f else 0))
                          in (mix l, mix r)) <$> psg <*> fmLevel <*> fmCtl
  audio = audioFilter mixed
  frameTog = register False (mux (goFrameEnd <$> out) (not <$> frameTog) frameTog)
  bankWrite :: Unsigned 2 -> Maybe (Unsigned 14, Unsigned 8) -> Maybe (Unsigned 12, Unsigned 8)
  bankWrite k w = case w of
    Just (a, b) | resize a == k -> Just (resize (a `shiftR` 2), b)
    _ -> Nothing

sat16 :: Signed 20 -> Signed 16
sat16 x = if x > 32767 then 32767 else if x < -32768 then -32768 else resize x

-- | The FM unit's gain: a sample's pin sum times this, against the PSG
-- (the spec's "Sms.Spec.FmUnit.fmGain").
fmGainC :: Signed 20
fmGainC = 24

-- | The FM unit on the machine's clock: (the YM2413, its write waiting for
-- its cycle's end, the T-states into the cycle, the cycle's number in its
-- sample, the pins summed so far, the last sample's level, F2h).
data FmMach = FmMach
  { fmChip  :: FmC
  , fmPend  :: FmWrite
  , fmT     :: Unsigned 2
  , fmCyc   :: Index 18
  , fmAcc   :: Signed 20
  , fmLvl   :: Signed 20
  , fmF2    :: Unsigned 2
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFDataX

fmMachInit :: FmMach
fmMachInit = FmMach fmPowerOn Nothing 0 0 0 0 0

fmStep :: FmMach -> (Bool, Maybe (Unsigned 8, Unsigned 8)) -> (FmMach, (Signed 20, Unsigned 2))
fmStep m (tEnd, w) = (m3, (fmLvl m * fmGainC, fmF2 m))
 where
  -- (the level and F2h out of the state register, a clock late: from m3
  -- the YM2413's operator, the pin sum, the mix and the audio filter were
  -- one path, 53 logic levels, WNS +0.117 ns on the ZedBoard)
  -- a T-state ended: the fourth ends the YM2413's cycle (its write in);
  -- its pins are those of the cycle two later: a sample's 18 summed
  t' = if tEnd then fmT m + 1 else fmT m
  cycEnd = tEnd && t' == 0
  (chip', (mo, ro)) = fmCycle (fmPend m) (fmChip m)
  pinCyc = if fmCyc m >= 16 then fmCyc m - 16 else fmCyc m + 2
  acc' = fmAcc m + resize mo + resize ro
  m1 | cycEnd = m { fmChip = chip', fmPend = Nothing, fmT = t'
                  , fmCyc = if fmCyc m == maxBound then 0 else fmCyc m + 1
                  , fmAcc = if pinCyc == 17 then 0 else acc'
                  , fmLvl = if pinCyc == 17 then acc' else fmLvl m }
     | otherwise = m { fmT = t' }
  -- a write (at its T-state: it belongs to the cycle now running)
  m2 = case w of
    Just (0xF0, b) -> m1 { fmPend = Just (False, b) }
    Just (0xF1, b) -> m1 { fmPend = Just (True, b) }
    Just (0xF2, b) -> m1 { fmF2 = resize b }
    _ -> m1
  m3 = m2

-- | A link write into the cartridge word ('cartAddr': low byte, then
-- high), the other byte kept (from the file's word at first).
cartWord :: Maybe (BitVector 16) -> BitVector 16 -> Maybe (LinkAddr, BitVector 8) -> Maybe (BitVector 16)
cartWord cur file w = case w of
  Just (a, d) | a == cartAddr -> Just (slice d15 d8 old ++# d)
              | a == cartAddr + 1 -> Just (d ++# slice d7 d0 old)
  _ -> cur
 where
  old = fromMaybe file cur
