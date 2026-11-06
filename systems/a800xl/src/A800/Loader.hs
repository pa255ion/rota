{-|
Software from the PS (M4 step 4.8): the 800XL's side of the PS link
("Platform.Ps.Link"; the PS side is @platform/ps/@, the SD card menu's
@/a800xl@ folder).

__Files__ (link address 0 up, the core held): the PS streams the file
unchanged, byte by byte in order. Every byte goes to the executable's
block RAM at its offset (up to 64 KiB) and to the cartridge's (16 KiB):
the first 16 at their offset, the rest at the offset, or 16 less when the
file starts with a CAR header ("CART": its 16 bytes come first). The first
16 bytes are also kept ('LoadState'). When hold falls, 'loadedCfg' says
what was loaded, by its first bytes and its length:

* @FF FF@: an executable (XEX), through the loader page (at most 64 KiB);
* @CART@ with type 1 or 2 (big-endian at bytes 4-7): a standard 8 or 16
  KiB cartridge (the rest of the header, its checksum, is not checked);
  with the cartridge in DDR (the Arty, side track Q, 'loadedCfgFor'), also
  12, 13 or 14: an XEGS of 32, 64 or 128 KiB (its image read from DDR, the
  link carrying the file's first 64 KiB);
* anything else of exactly 8 or 16 KiB: a raw cartridge image.

Other files are refused: the core stays held. BASIC is off for all of them
(as Altirra loads them by default); with no file loaded since power-on
the built-in configuration runs (BASIC's Ready prompt).

__The keyboard__ (link 'keysBase' up, while the core runs): bytes 0-7 the
64 keys held (bit /n/ mod 8 of byte /n/ div 8 = key code /n/,
"A800.Keyboard"), byte 8 SHIFT, CONTROL, BREAK (bits 0-2), byte 9 the
console keys START, SELECT, OPTION (bits 0-2), byte 10 joystick 1 from the
desktop (bits 0-3 up, down, left, right pressed; bit 4 the trigger). The PS
writes all eleven whenever one changes.
-}
module A800.Loader
  ( LoadState (..)
  , loadInit
  , loadStep
  , LoadWrite (..)
  , loadRoute
  , loadedCfg
  , loadedCfgFor
  , isCar
  , isTape
  , keysBase
  , Keys (..)
  , keysInit
  , keysStep
  ) where

import Clash.Prelude

import A800.Core (CartKind (..), CoreCfg (..))
import Platform.Ps.Link (LinkAddr)

-- | The file's first 16 bytes and its length so far.
data LoadState = LoadState
  { lsHead :: Vec 16 (BitVector 8)
  , lsLen  :: Unsigned 24
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFDataX

loadInit :: LoadState
loadInit = LoadState (repeat 0) 0

-- | Block RAM writes for a file byte.
data LoadWrite = LoadWrite
  { lwXex  :: Maybe (Unsigned 16, BitVector 8)
  , lwCart :: Maybe (Unsigned 14, BitVector 8)
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFDataX

isCar :: Vec 16 (BitVector 8) -> Bool
isCar h = pack (take d4 h) == 0x43415254   -- "CART"

-- | A tape image ("A800.Media.casImageFile": "FUJI", side track Q's
-- cassettes on the board): DDR only.
isTape :: Vec 16 (BitVector 8) -> Bool
isTape h = pack (take d4 h) == 0x46554A49   -- "FUJI"

-- | A link write while the core is held: the RAM writes.
loadRoute :: LoadState -> Bool -> Maybe (LinkAddr, BitVector 8) -> LoadWrite
loadRoute s True (Just (a, d)) | a < 0x10000 =
  LoadWrite
    { lwXex = Just (resize a, d)
    , lwCart = let o | a >= 16 && isCar (lsHead s) = a - 16
                     | otherwise = a
               in if o < 0x4000 then Just (resize o, d) else Nothing }
loadRoute _ _ _ = LoadWrite Nothing Nothing

-- | The header and length after a write while held; a new file starts
-- when hold rises.
loadStep :: LoadState -> (Bool, Bool, Maybe (LinkAddr, BitVector 8)) -> LoadState
loadStep s (rose, held, w)
  | rose = loadInit
  -- (the length counts writes past what the RAMs keep too: a file too
  -- long for them must be refused, not cut)
  | held, Just (a, d) <- w, a < keysBase =
      s { lsHead = if a < 16 then replace (fromIntegral a :: Index 16) d (lsHead s) else lsHead s
        , lsLen = max (lsLen s) (a + 1) }
  | otherwise = s

-- | What was loaded, if the 800XL can run it (the cartridge in block RAM:
-- no XEGS).
loadedCfg :: LoadState -> Maybe CoreCfg
loadedCfg = loadedCfgFor False

-- | The same, with the cartridge in DDR (@True@: XEGS cartridges too).
loadedCfgFor :: Bool -> LoadState -> Maybe CoreCfg
loadedCfgFor ddr (LoadState h n)
  | h !! (0 :: Index 16) == 0xFF && h !! (1 :: Index 16) == 0xFF =
      if n <= 0x10000 then Just (CoreCfg False CkNone True n) else Nothing
  | isTape h = if ddr then Just (CoreCfg False CkCas False 0) else Nothing
  | isCar h = case pack (take d4 (drop d4 h)) of
      1 -> Just (CoreCfg False Ck8 False 0)
      2 -> Just (CoreCfg False Ck16 False 0)
      12 | ddr -> Just (CoreCfg False (CkXegs 0) False 0)
      13 | ddr -> Just (CoreCfg False (CkXegs 1) False 0)
      14 | ddr -> Just (CoreCfg False (CkXegs 2) False 0)
      _ -> Nothing
  | n == 0x2000 = Just (CoreCfg False Ck8 False 0)
  | n == 0x4000 = Just (CoreCfg False Ck16 False 0)
  | otherwise = Nothing

-- | The keyboard's link range (the system's own map).
keysBase :: LinkAddr
keysBase = 0x100000

-- | What the PS sends for the keyboard, console keys and joystick.
data Keys = Keys
  { kKeys    :: BitVector 64
  , kMods    :: BitVector 3
  , kConsole :: BitVector 3
  , kJoy     :: BitVector 5    -- ^ up, down, left, right, trigger (pressed)
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFDataX

keysInit :: Keys
keysInit = Keys 0 0 0 0

-- | A link write while the core runs.
keysStep :: Keys -> (Bool, Maybe (LinkAddr, BitVector 8)) -> Keys
keysStep k (False, Just (a, d)) | a >= keysBase && a < keysBase + 11 =
  case a - keysBase of
    8 -> k { kMods = resize d }
    9 -> k { kConsole = resize d }
    10 -> k { kJoy = resize d }
    i -> let bytes = unpack (kKeys k) :: Vec 8 (BitVector 8)
             -- byte i holds codes 8i to 8i+7: in the vector (big-endian
             -- packing) it is element 7 - i
         in k { kKeys = pack (replace (7 - fromIntegral i :: Index 8) d bytes) }
keysStep k _ = k
