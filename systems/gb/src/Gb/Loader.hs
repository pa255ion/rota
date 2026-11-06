{-|
Cartridge loading from the PS (side track A): the Game Boy's side of the PS
link ("Platform.Ps.Link", which documents the EMIO GPIO lines; the PS side
is @platform/ps/@: the SD card menu, and @make load SYSTEM=gb@ over J14).
Bits 0 to 7 of the PS → PL word are the Game Boy's buttons (Right Left Up
Down A B Select Start).

Address map (below 'Platform.Ps.Link.sharedBase'; docs/ARCHITECTURE.md):

* 0x000000 to 0x0FFFFF: unused since side track I's phase 2 (it was the
  cartridge ROM block RAM): the PS writes the cartridge ROM into DDR
  ("Gb.Top", 'Gb.Top.romBase') while hold is high (the core is then held
  in its hand-off state, "Gb.Core"'s @run@, and doesn't read the ROM).
  When hold falls the core starts the new cartridge and reads its
  header.
* 0x100000 to 0x11FFFF: cartridge RAM (up to 128 KiB; side track I,
  battery saves), written only while hold is high: the PS puts a @.sav@
  file there before the game starts ('cramLoad').
* 0x120000 to 0x13FFFF: cartridge RAM's read window. A data byte written
  at 0x120000 + n (any value, held or not) reads byte n of cartridge RAM
  through the block RAM's second port, so the game runs on while the PS
  reads its save ('cramRead'); the byte goes out on @PS_IN@'s byte
  channel while the save control's bit 0 is set ("Gb.Top").
* 0x140000: the save control ('SaveCtl'), bit 0: the byte channel carries
  cartridge RAM reads instead of the serial port. 0x140001: any write
  clears the dirty flag (@PS_IN@ bit 6, set by every write the game makes
  to cartridge RAM).
* 0x140010 to 0x140019: MBC3's clock at start (phase 2; written while
  held, 'rtcLoad'): S M H DL DH, then the five latched registers; the
  core takes them when it starts.
* 0x140020 to 0x140029: reads of the clock now (any time, 'rtcRead'),
  in the same order, onto the byte channel like cartridge RAM's reads; a
  read of 0x140020 takes a snapshot of all ten that the others return,
  so a second going by meanwhile can't tear them.

* 0x140030 to 0x140047: the Game Boy Color's palettes for a DMG cartridge
  (phase 3; written while held, 'palLoad'): BG palette 0, OBJ 0, OBJ 1,
  four 15-bit colours each, little-endian (platform/ps/gbc_palette.zig);
  the core takes them when it starts. Unwritten, the default
  ('Gb.Core.gbcDefaultPal') stays.

Writes anywhere else below the shared range are ignored.
-}
module Gb.Loader
  ( cramBase
  , cramReadBase
  , saveCtlAddr
  , dirtyClearAddr
  , cramLoad
  , cramRead
  , rtcLoad
  , rtcRead
  , palLoad
  , stateModeAddr
  , stateWBase
  , stateRBase
  , SaveCtl (..)
  , saveCtlInit
  , saveCtlStep
  ) where

import Clash.Prelude

import Platform.Ps.Link (LinkAddr)

cramBase, cramReadBase, saveCtlAddr, dirtyClearAddr :: LinkAddr
cramBase       = 0x100000
cramReadBase   = 0x120000
saveCtlAddr    = 0x140000
dirtyClearAddr = 0x140001

-- | The cartridge RAM write for a link write, if it is one (and hold is
-- high).
cramLoad :: Bool -> Maybe (LinkAddr, BitVector 8) -> Maybe (Unsigned 17, BitVector 8)
cramLoad True (Just (a, d)) | a >= cramBase && a < cramBase + 0x20000 = Just (resize (a - cramBase), d)
cramLoad _ _ = Nothing

-- | The cartridge RAM byte to read for the PS, if the link write asks for
-- one (held or not).
cramRead :: Maybe (LinkAddr, BitVector 8) -> Maybe (Unsigned 17)
cramRead (Just (a, _)) | a >= cramReadBase && a < cramReadBase + 0x20000 = Just (resize (a - cramReadBase))
cramRead _ = Nothing

-- | A write of the clock's start value n (while held).
rtcLoad :: Bool -> Maybe (LinkAddr, BitVector 8) -> Maybe (Index 10, BitVector 8)
rtcLoad True (Just (a, d)) | a >= 0x140010 && a < 0x14001A = Just (fromIntegral (a - 0x140010), d)
rtcLoad _ _ = Nothing

-- | A write of the palettes' byte n (while held).
palLoad :: Bool -> Maybe (LinkAddr, BitVector 8) -> Maybe (Index 24, BitVector 8)
palLoad True (Just (a, d)) | a >= 0x140030 && a < 0x140048 = Just (fromIntegral (a - 0x140030), d)
palLoad _ _ = Nothing

-- | A read of the clock's byte n (any time).
rtcRead :: Maybe (LinkAddr, BitVector 8) -> Maybe (Index 10)
rtcRead (Just (a, _)) | a >= 0x140020 && a < 0x14002A = Just (fromIntegral (a - 0x140020))
rtcRead _ = Nothing

-- | Save states (side track L, L.6; docs/ARCHITECTURE.md): the state
-- mode (bit 0: freeze the core for state access once paused and at
-- rest), and the state's write and read windows ("Platform.SaveState",
-- 'Gb.Core.gbCoreHooked'). Only tops built with save states decode them.
stateModeAddr, stateWBase, stateRBase :: LinkAddr
stateModeAddr = 0x140050
stateWBase = 0x150000
stateRBase = 0x160000

-- | The save control register and the dirty flag.
data SaveCtl = SaveCtl
  { scReadBack :: Bool -- ^ the byte channel carries cartridge RAM reads
  , scDirty    :: Bool -- ^ the game wrote cartridge RAM since the last clear
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFDataX

saveCtlInit :: SaveCtl
saveCtlInit = SaveCtl False False

-- | One clock: a link write, and whether the game writes cartridge RAM
-- now (a write in the same clock as a clear wins: the PS sees it later).
saveCtlStep :: SaveCtl -> (Maybe (LinkAddr, BitVector 8), Bool) -> SaveCtl
saveCtlStep s (w, gameWrite) = SaveCtl rb (gameWrite || (scDirty s && not clear))
 where
  rb = case w of
    Just (a, d) | a == saveCtlAddr -> testBit d 0
    _ -> scReadBack s
  clear = case w of
    Just (a, _) -> a == dirtyClearAddr
    Nothing -> False
