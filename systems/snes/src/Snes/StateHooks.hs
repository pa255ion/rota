{-# LANGUAGE CPP #-}
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

__The CPU build__ (@SNES_STATES=cpu@, step 7.15; the switch defines
SNES_STATES_CPU): a load goes through the 65C816's routine in a ROM overlay
(platform/ps/systems/snes_restore.zig). The state space differs there:

    * 0x00000: the record is the state table (520 bytes, read only:
      "Snes.StateTap"), then OAM's high table (32: registers in the
      circuit): 552 bytes, the status's record size (savestate.zig's
      loads through the CPU take the record as the table);
    * 0x00800: the chain, the sound module's record
      ('Snes.Core.apuChainBytes' long), written back directly (a section
      of the PS's Spec);
    * 0x01F10, 0x01F11: the overlay's end address (low, high); 0x01F12 bit
      0 arms it (kept through the hold: it answers the reset's vector;
      off once its last instruction ends);
    * 0x06000 to 0x067FF: the overlay's 2 KiB (written);
    * the memories as above; the status's version 2.

  The freeze waits for 'Snes.Core.atRestCpu' (and V-blank), or comes at
  once while the PS holds the core (a load: the core at power-on).
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
import Snes.Core (OvlIn (..), StCtl (..), StPort (..))
#ifdef SNES_STATES_CPU
import Snes.StateTap (tableWindow)
#else
import Snes.Core (chainBytes)
#endif

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
#ifdef SNES_STATES_CPU
stateVersion = 2
#else
stateVersion = 1
#endif

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
  , hoOvl    :: Signal dom OvlIn                 -- ^ the ROM overlay (the CPU build)
  , hoFrozen :: Signal dom Bool                  -- ^ frozen (the hold resets nothing)
  , hoRun    :: Signal dom Bool                  -- ^ a freeze asked for, not yet reached: the core runs on (the menu's pause would stop it short of rest)
  , hoByte   :: Signal dom (Maybe (BitVector 8)) -- ^ a state byte read, for the byte channel
  }

-- | What an access is, by its offset.
data Where = WChain | WTable (Unsigned 10) | WOvl (Unsigned 11) | WArm (Unsigned 2) | WCgram (Unsigned 8) Bool | WOam (Index 4) (Unsigned 7) | WDma (Index 16) (Index 8)
           | WSmp (Unsigned 8) Bool | WStatus (Unsigned 2) | WAram (Unsigned 16) | WVram Bool (Unsigned 15)
           | WWram (Unsigned 17) | WNone
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFDataX

whereOf :: Unsigned 19 -> Where
whereOf a
#ifdef SNES_STATES_CPU
  | a < 552 = WTable (resize a)
  | a < 0x00800 = WNone
  | a >= 0x01F10 && a < 0x01F13 = WArm (resize (a - 0x01F10))
  | a >= 0x06000 && a < 0x06800 = WOvl (resize (a - 0x06000))
#endif
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

-- | The hooks: the link's writes, the menu's pause, the PS's hold, the
-- core's rest (the CPU build's 'Snes.Core.atRestCpu' there), its line and
-- 239-line mode, its chain's top byte, its port's read, its overlay's end.
snesStateHooks
  :: forall dom. HiddenClockResetEnable dom
  => Signal dom (Maybe (LinkAddr, BitVector 8)) -> Signal dom Bool -> Signal dom Bool
  -> Signal dom Bool -> Signal dom (Unsigned 9) -> Signal dom Bool
  -> Signal dom (BitVector 8) -> Signal dom (BitVector 16) -> Signal dom Bool
  -> HookOut dom
snesStateHooks link pause held rest v overscan top portRd ovlDone = HookOut ctl ovl frozen runOn byte
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
#ifdef SNES_STATES_CPU
  -- (at rest: a finishing clock first, then frozen, 'Snes.Core.stFinish';
  -- a held core, a load's: at once)
  finishNow = (\w r b z -> w && r && b && not z) <$> want <*> rest <*> vblank <*> frozen
  freezeNow = (&&) <$> want <*> held
  -- the overlay: its end address and arm (written by the PS, frozen), off
  -- once its last instruction ends
  ovlEnd = register 0 ((\o e -> case o of
                          Just (OWrite a d) | WArm 0 <- whereOf a -> (e .&. 0xFF00) .|. resize (unpack d :: Unsigned 8)
                                            | WArm 1 <- whereOf a -> (e .&. 0x00FF) .|. (resize (unpack d :: Unsigned 8) `shiftL` 8)
                          _ -> e) <$> fop <*> ovlEnd)
  ovlArm = register False ((\o d r -> case o of
                              Just (OWrite a b) | WArm 2 <- whereOf a -> testBit b 0
                              _ -> r && not d) <$> fop <*> ovlDone <*> ovlArm)
  ovl = OvlIn <$> ovlArm <*> ovlEnd
#else
  freezeNow = (\w r b -> w && r && b) <$> want <*> rest <*> vblank
  ovl = pure (OvlIn False 0) <* held <* ovlDone
#endif
#ifdef SNES_STATES_CPU
  frozen = register False ((\w z f n -> w && (z || f || n)) <$> want <*> frozen <*> freezeNow <*> finishNow)
#else
  frozen = register False ((\w z f -> w && (z || f)) <$> want <*> frozen <*> freezeNow)
  finishNow = pure False
#endif
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
  ctl = (\z o lo t n -> StCtl z (shiftOf o t) (portOf o lo) (n && not z)) <$> isFrozen <*> fop <*> lowB <*> top <*> finishNow
  -- the chain: a write shifts its byte in, a read the top byte (round)
  shiftOf o t = case o of
    Just (OWrite a d) | WChain <- whereOf a -> Just d
    Just (ORead a) | WChain <- whereOf a -> Just t
    _ -> Nothing
  portOf o lo = case o of
    Just (OWrite a d) -> case whereOf a of
      WOvl x -> Just (POvl x (Just (unpack d)))
      WCgram i True -> Just (PCgram i (Just (unpack (truncateB (d ++# lo)))))
      WOam l e -> Just (POam l e (Just (unpack d)))
      WDma r ch -> Just (PDma r ch (Just (unpack d)))
      WSmp i True -> Just (PSmp i (Just (unpack (d ++# lo))))
      WAram x -> Just (PAram x (Just (unpack d)))
      WVram h w -> Just (PVram h w (Just (unpack d)))
      WWram x -> Just (PWram x (Just (unpack d)))
      _ -> Nothing
    Just (ORead a) -> case whereOf a of
      WTable n -> Just (PTable n)
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
#ifdef SNES_STATES_CPU
  size16 = fromIntegral tableWindow :: Unsigned 16
#else
  size16 = fromIntegral chainBytes :: Unsigned 16
#endif
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
