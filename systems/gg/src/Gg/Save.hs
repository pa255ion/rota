{-|
The Game Gear's battery saves on the PS link (M5 step 5.10, exit item 2),
the Game Boy's protocol ("Gb.Loader", docs/ARCHITECTURE.md "Battery
saves") with the Game Gear's 32 KiB of cartridge RAM. The PS app's
menu row (platform/ps/menu.zig, @.save@) names the same addresses:

* 0x100000 to 0x107FFF: cartridge RAM, written by the PS while it holds
  the machine: the save file before the game starts ('cartLoad').
* 0x120000 to 0x127FFF: the read window. A data byte written at
  0x120000 + n (any value, held or not) reads byte n of cartridge RAM
  through the block RAM's second port, so the game runs on while the PS
  reads its save ('cartRead'); the byte goes out on @PS_IN@'s byte
  channel while the save control's bit 0 is set.
* 0x140000: the save control ('SaveCtl'), bit 0: the byte channel carries
  cartridge RAM reads. 0x140001: any write clears the dirty flag (@PS_IN@
  bit 6, set by every write the game makes to cartridge RAM).

The cartridge word ("Gg.Machine.cartAddr") is at 0x000000, below these.
-}
module Gg.Save
  ( cartRamBase
  , cartReadBase
  , saveCtlAddr
  , dirtyClearAddr
  , cartLoad
  , cartRead
  , SaveCtl (..)
  , saveCtlInit
  , saveCtlStep
  ) where

import Clash.Prelude

import Platform.Ps.Link (LinkAddr)

cartRamBase, cartReadBase, saveCtlAddr, dirtyClearAddr :: LinkAddr
cartRamBase    = 0x100000
cartReadBase   = 0x120000
saveCtlAddr    = 0x140000
dirtyClearAddr = 0x140001

-- | The cartridge RAM write for a link write, if it is one (and the PS
-- holds the machine).
cartLoad :: Bool -> Maybe (LinkAddr, BitVector 8) -> Maybe (Unsigned 15, BitVector 8)
cartLoad True (Just (a, d)) | a >= cartRamBase && a < cartRamBase + 0x8000 = Just (resize (a - cartRamBase), d)
cartLoad _ _ = Nothing

-- | The cartridge RAM byte to read for the PS, if the link write asks for
-- one (held or not).
cartRead :: Maybe (LinkAddr, BitVector 8) -> Maybe (Unsigned 15)
cartRead (Just (a, _)) | a >= cartReadBase && a < cartReadBase + 0x8000 = Just (resize (a - cartReadBase))
cartRead _ = Nothing

-- | The save control and the dirty flag.
data SaveCtl = SaveCtl
  { scReadBack :: Bool -- ^ the byte channel carries cartridge RAM reads
  , scDirty    :: Bool -- ^ the game wrote cartridge RAM since the PS cleared this
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFDataX

saveCtlInit :: SaveCtl
saveCtlInit = SaveCtl False False

-- | One clock: a link write (the control, the dirty flag's clear) and
-- whether the game wrote cartridge RAM (a game write in the same clock as
-- a clear wins: it is newer).
saveCtlStep :: SaveCtl -> (Maybe (LinkAddr, BitVector 8), Bool) -> SaveCtl
saveCtlStep s (w, gameWrite) = SaveCtl rb (gameWrite || (scDirty s && not clear))
 where
  rb = case w of
    Just (a, d) | a == saveCtlAddr -> testBit d 0
    _ -> scReadBack s
  clear = case w of
    Just (a, _) -> a == dirtyClearAddr
    Nothing -> False
