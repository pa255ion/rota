{-|
The Super Nintendo's battery saves on the PS link (PLAN.md step 7.11),
the Game Boy's and the Game Gear's protocol ("Gg.Save", docs/ARCHITECTURE.md
"Battery saves") with the cartridge's SRAM (32 KiB at most here, its
second port: "Snes.Core"). The PS app's menu row
(platform/ps/systems/snes.zig, @.save@) names the same addresses:

* 0x100000 to 0x107FFF: SRAM, written by the PS while it holds the
  machine: the save file before the game starts ('sramLoad').
* 0x120000 to 0x127FFF: the read window. A data byte written at
  0x120000 + n (any value, held or not) reads byte n of SRAM through its
  second port, so the game runs on while the PS reads its save
  ('sramRead'); the byte goes out on @PS_IN@'s byte channel while the
  save control's bit 0 is set.
* 0x140000: the save control ('SaveCtl'), bit 0: the byte channel carries
  SRAM reads. 0x140001: any write clears the dirty flag (@PS_IN@ bit 6,
  set by every write the game makes to SRAM).

The cartridge word ("Snes.Machine.cartAddr") is at 0x000000, below these.
-}
module Snes.Save
  ( sramBase
  , sramReadBase
  , saveCtlAddr
  , dirtyClearAddr
  , sramPsOp
  , sramRead
  , SaveCtl (..)
  , saveCtlInit
  , saveCtlStep
  ) where

import Clash.Prelude

import Platform.Ps.Link (LinkAddr)

sramBase, sramReadBase, saveCtlAddr, dirtyClearAddr :: LinkAddr
sramBase       = 0x100000
sramReadBase   = 0x120000
saveCtlAddr    = 0x140000
dirtyClearAddr = 0x140001

-- | The SRAM byte to read for the PS, if the link write asks for one
-- (held or not).
sramRead :: Maybe (LinkAddr, BitVector 8) -> Maybe (Unsigned 15)
sramRead (Just (a, _)) | a >= sramReadBase && a < sramReadBase + 0x8000 = Just (resize (a - sramReadBase))
sramRead _ = Nothing

-- | The second port's op for a link write: a load while the PS holds the
-- machine, a read for the window, else a read of byte 0.
sramPsOp :: Bool -> Maybe (LinkAddr, BitVector 8) -> RamOp 32768 (Unsigned 8)
sramPsOp held w = case (w, sramRead w) of
  (Just (a, d), _) | held && a >= sramBase && a < sramBase + 0x8000 -> RamWrite (fromIntegral (a - sramBase)) (unpack d)
  (_, Just a) -> RamRead (fromIntegral a)
  _ -> RamRead 0

-- | The save control and the dirty flag.
data SaveCtl = SaveCtl
  { scReadBack :: Bool -- ^ the byte channel carries SRAM reads
  , scDirty    :: Bool -- ^ the game wrote SRAM since the PS cleared this
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFDataX

saveCtlInit :: SaveCtl
saveCtlInit = SaveCtl False False

-- | One clock: a link write (the control, the dirty flag's clear) and
-- whether the game wrote SRAM (a game write in the same clock as a clear
-- wins: it is newer).
saveCtlStep :: SaveCtl -> (Maybe (LinkAddr, BitVector 8), Bool) -> SaveCtl
saveCtlStep s (w, gameWrite) = SaveCtl rb (gameWrite || (scDirty s && not clear))
 where
  rb = case w of
    Just (a, d) | a == saveCtlAddr -> testBit d 0
    _ -> scReadBack s
  clear = case w of
    Just (a, _) -> a == dirtyClearAddr
    Nothing -> False
