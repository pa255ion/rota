{-|
The Super Nintendo's save-state hooks (step 7.14b; docs/systems/snes.md,
"Save states"; the Arty's top only, as the other systems'): the direct
path. The PS freezes the core, reads its state out, and later writes a
state back into a frozen core, which resumes ("Snes.Core", 'StCtl').

* __Freeze.__ The state mode ('stateModeAddr', bit 0) with the menu's
  pause asks for it. The core freezes at rest ('Snes.Core.atRest') in
  V-blank, where the PPU's line buffer and column ring are out of use (not
  in the state: snes-savestate checks it), and stays frozen while asked.
  The PS's hold doesn't reset a frozen core ("Snes.Machine"), so a state
  written while held stays.
* __The state space__, through windows on the link ('stateWBase' + n
  writes byte n, frozen only; 'stateRBase' + n reads it onto the byte
  channel while 'readBackAddr' bit 0 is set, the battery saves' control):

    * 0x00000 to 0x00FFF: the chain ('Snes.Core.chainBytes' long: every
      access shifts it a byte, so it is read and written in order, once
      round; the PS's "table");
    * 0x01000: CGRAM (a colour's low byte, then its high: written with the
      high one), 512 bytes;
    * 0x01200: OAM's low table (byte n: lane n mod 4, entry n / 4), 512;
    * 0x01400: the DMA registers (channel × 16 + register), 128;
    * 0x01500: the DSP's samples (low, high; written with the high), 512;
    * 0x01F00 to 0x01F03: the status (1 while frozen; the chain's size,
      low and high; 'stateVersion'), any time;
    * 0x10000: ARAM, 64 KiB;
    * 0x20000: VRAM (byte 2w the low chip's word w, 2w + 1 the high's),
      64 KiB;
    * 0x40000: WRAM, 128 KiB.

  SRAM (the battery save) is the battery saves' windows' ("Snes.Save").
-}
module Snes.StateHooks
  ( HookOut (..)
  , snesStateHooks
  , stateModeAddr
  , readBackAddr
  , stateWBase
  , stateRBase
  , stateVersion
  , statusAddr
  , cgramBase
  , oamBase
  , dmaBase
  , smpBase
  , aramBase
  , vramBase
  , wramBase
  ) where

import Clash.Prelude


import Platform.Ps.Link (LinkAddr)
import Snes.Core (StCtl (..), StPort (..), chainBytes)

stateModeAddr, readBackAddr, stateWBase, stateRBase :: LinkAddr
stateModeAddr = 0x140050
readBackAddr = 0x140000
stateWBase = 0x200000
stateRBase = 0x300000

-- | The windows' span (the state space's top, 0x60000, rounded up).
windowSize :: LinkAddr
windowSize = 0x80000

statusAddr, cgramBase, oamBase, dmaBase, smpBase, aramBase, vramBase, wramBase :: Unsigned 19
statusAddr = 0x01F00
cgramBase = 0x01000
oamBase = 0x01200
dmaBase = 0x01400
smpBase = 0x01500
aramBase = 0x10000
vramBase = 0x20000
wramBase = 0x40000

-- | The layout's version (the PS's slot header keeps it: a state from
-- another layout is refused).
stateVersion :: BitVector 8
stateVersion = 1

data Op = ORead (Unsigned 19) | OWrite (Unsigned 19) (BitVector 8)
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFDataX

stateOp :: Maybe (LinkAddr, BitVector 8) -> Maybe Op
stateOp (Just (a, d))
  | a >= stateWBase && a < stateWBase + windowSize = Just (OWrite (resize (a - stateWBase)) d)
  | a >= stateRBase && a < stateRBase + windowSize = Just (ORead (resize (a - stateRBase)))
stateOp _ = Nothing

data HookOut dom = HookOut
  { hoCtl    :: Signal dom StCtl                 -- ^ the core's control
  , hoFrozen :: Signal dom Bool                  -- ^ frozen (the hold resets nothing)
  , hoRun    :: Signal dom Bool                  -- ^ a freeze asked for, not yet reached: the core runs on (the menu's pause would stop it short of rest)
  , hoByte   :: Signal dom (Maybe (BitVector 8)) -- ^ a state byte read, for the byte channel
  }

-- | What an access is, by its offset.
data Where = WChain | WCgram (Unsigned 8) Bool | WOam (Index 4) (Unsigned 7) | WDma (Index 16) (Index 8)
           | WSmp (Unsigned 8) Bool | WStatus (Unsigned 2) | WAram (Unsigned 16) | WVram Bool (Unsigned 15)
           | WWram (Unsigned 17) | WNone
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFDataX

whereOf :: Unsigned 19 -> Where
whereOf a
  | a < 0x01000 = WChain
  | a < 0x01200 = let o = a - cgramBase in WCgram (resize (o `shiftR` 1)) (testBit o 0)
  | a < 0x01400 = let o = a - oamBase in WOam (fromIntegral (o .&. 3)) (resize (o `shiftR` 2))
  | a < 0x01480 = let o = a - dmaBase in WDma (fromIntegral (o .&. 15)) (fromIntegral ((o `shiftR` 4) .&. 7))
  | a >= smpBase && a < smpBase + 0x200 = let o = a - smpBase in WSmp (resize (o `shiftR` 1)) (testBit o 0)
  | a >= statusAddr && a < statusAddr + 4 = WStatus (resize (a - statusAddr))
  | a >= aramBase && a < aramBase + 0x10000 = WAram (resize (a - aramBase))
  | a >= vramBase && a < vramBase + 0x10000 = let o = a - vramBase in WVram (testBit o 0) (resize (o `shiftR` 1))
  | a >= wramBase && a < wramBase + 0x20000 = WWram (resize (a - wramBase))
  | otherwise = WNone

-- | The hooks: the link's writes, the menu's pause, the core's rest, its
-- line and 239-line mode, its chain's top byte and its port's read.
snesStateHooks
  :: forall dom. HiddenClockResetEnable dom
  => Signal dom (Maybe (LinkAddr, BitVector 8)) -> Signal dom Bool
  -> Signal dom Bool -> Signal dom (Unsigned 9) -> Signal dom Bool
  -> Signal dom (BitVector 8) -> Signal dom (BitVector 16)
  -> HookOut dom
snesStateHooks link pause rest v overscan top portRd = HookOut ctl frozen runOn byte
 where
  op = stateOp <$> link
  modeStep addr w m = case w of
    Just (a, d) | a == addr -> testBit d 0
    _ -> m
  stMode = register False (modeStep stateModeAddr <$> link <*> stMode)
  readBack = register False (modeStep readBackAddr <$> link <*> readBack)
  want = (&&) <$> stMode <*> pause
  -- at rest in V-blank (lines 226 or 242 to 259: the line buffer and the
  -- ring out of use)
  vblank = (\o l -> l >= (if o then 242 else 226) && l <= 259) <$> overscan <*> v
  freezeNow = (\w r b -> w && r && b) <$> want <*> rest <*> vblank
  frozen = register False ((\w z f -> w && (z || f)) <$> want <*> frozen <*> freezeNow)
  isFrozen = (||) <$> frozen <*> freezeNow
  runOn = (\w z -> w && not z) <$> want <*> isFrozen
  -- an access while frozen (the status any time)
  fop = (\z o -> if z then o else case o of
                   Just (ORead a) | WStatus _ <- whereOf a -> o
                   _ -> Nothing) <$> frozen <*> op
  -- a colour's or a sample's low byte, kept for its high one
  lowB = register 0 ((\o l -> case o of
                        Just (OWrite a d) | WCgram _ False <- whereOf a -> d
                                          | WSmp _ False <- whereOf a -> d
                        _ -> l) <$> fop <*> lowB)
  ctl = (\z o lo t -> StCtl z (shiftOf o t) (portOf o lo)) <$> isFrozen <*> fop <*> lowB <*> top
  -- the chain: a write shifts its byte in, a read the top byte (round)
  shiftOf o t = case o of
    Just (OWrite a d) | WChain <- whereOf a -> Just d
    Just (ORead a) | WChain <- whereOf a -> Just t
    _ -> Nothing
  portOf o lo = case o of
    Just (OWrite a d) -> case whereOf a of
      WCgram i True -> Just (PCgram i (Just (unpack (truncateB (d ++# lo)))))
      WOam l e -> Just (POam l e (Just (unpack d)))
      WDma r ch -> Just (PDma r ch (Just (unpack d)))
      WSmp i True -> Just (PSmp i (Just (unpack (d ++# lo))))
      WAram x -> Just (PAram x (Just (unpack d)))
      WVram h w -> Just (PVram h w (Just (unpack d)))
      WWram x -> Just (PWram x (Just (unpack d)))
      _ -> Nothing
    Just (ORead a) -> case whereOf a of
      WCgram i _ -> Just (PCgram i Nothing)
      WOam l e -> Just (POam l e Nothing)
      WDma r ch -> Just (PDma r ch Nothing)
      WSmp i _ -> Just (PSmp i Nothing)
      WAram x -> Just (PAram x Nothing)
      WVram h w -> Just (PVram h w Nothing)
      WWram x -> Just (PWram x Nothing)
      _ -> Nothing
    Nothing -> Nothing
  -- reads: the byte the clock after its access (the chain's top byte as
  -- it was shifted out; a port's the clock its value comes)
  lastOp = register Nothing fop
  lastTop = register 0 top
  frzNow = register False frozen
  size16 = fromIntegral chainBytes :: Unsigned 16
  rawByte = (\o z t p -> case o of
               Just (ORead a) -> Just $ case whereOf a of
                 WChain -> t
                 WStatus 0 -> if z then 1 else 0
                 WStatus 1 -> pack (truncateB size16 :: Unsigned 8)
                 WStatus 2 -> pack (truncateB (size16 `shiftR` 8) :: Unsigned 8)
                 WStatus _ -> stateVersion
                 WCgram _ hi -> if hi then truncateB (p `shiftR` 8) else truncateB p
                 WSmp _ hi -> if hi then truncateB (p `shiftR` 8) else truncateB p
                 WNone -> 0
                 _ -> truncateB p
               _ -> Nothing) <$> lastOp <*> frzNow <*> lastTop <*> portRd
  byte = (\rb b -> if rb then b else Nothing) <$> readBack <*> rawByte
