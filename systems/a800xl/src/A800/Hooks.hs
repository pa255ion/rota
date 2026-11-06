{-|
The 800XL's save-state hooks (side track N, PLAN.md N.5), for
"A800.System" built with save states
('Platform.SaveState.WithStateHooks'; the Arty's top). The Game Boy's and
the Game Gear's design ("Gb.Loader", "Gg.Hooks"): saving only reads,
loading runs a restore routine on the machine's own 6502.

* __Freeze.__ The state mode ('stateModeAddr', bit 0) with the menu's
  pause asks for it: the machine runs on (without keys) to a moment a
  state can be taken ("A800.StateTap"'s 'restoreable') and the core takes
  no step from there; while the PS holds it (its reset) it is frozen at
  once. The machine's cycle credit stops only once frozen.
* __The state space__, 128 KiB, through windows on the link
  ('stateWBase' + n writes byte n, frozen only; 'stateRBase' + n reads it
  onto the byte channel): the state table ("A800.StateTable", read only)
  from 0x0000; the status at 'stateStatusAddr' (1 while frozen, the
  table's size, its version; any time); the ROM overlay's end address
  and arm at 'overlayEndAddr'; the overlay's 2 KiB at 'overlayBase';
  RAM's 64 KiB at 'ramBase'. The 800XL has no battery saves: the byte
  channel carries the reads while 'readBackAddr' bit 0 is set (as the
  other systems' save control).
* __The ROM overlay.__ Armed while frozen, it answers every CPU read
  outside the I/O area (the core's 'kiOvl') from its block RAM at the
  CPU's address modulo 2 KiB, until the instruction that read its end
  address has ended; then off until armed again.
-}
module A800.Hooks
  ( HookOut (..)
  , a800StateHooks
  , stateModeAddr
  , readBackAddr
  , stateWBase
  , stateRBase
  , stateStatusAddr
  , overlayBase
  , overlayEndAddr
  , ramBase
  ) where

import Clash.Prelude
import Data.Maybe (isJust)

import A800.Core
import A800.StateTable (tableSize)
import A800.StateTap
import Mos6502.Core (Req (..), cpuRequest)
import Platform.Ps.Link (LinkAddr)

-- | The link map (docs/ARCHITECTURE.md): the state mode, the read-back
-- control, the two 128 KiB windows.
stateModeAddr, readBackAddr, stateWBase, stateRBase :: LinkAddr
stateModeAddr = 0x140050
readBackAddr = 0x140000
stateWBase = 0x150000
stateRBase = 0x170000

-- | In the state space: the status bytes, the overlay's end address (low,
-- high) then its arm, the overlay's bytes, RAM.
stateStatusAddr, overlayEndAddr, overlayBase, ramBase :: Unsigned 17
stateStatusAddr = 0x1F00
overlayEndAddr = 0x1F10
overlayBase = 0x6000
ramBase = 0x10000

-- | One access to the state space: read byte n, or write it.
data Op = ORead (Unsigned 17) | OWrite (Unsigned 17) (Unsigned 8)
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFDataX

stateOp :: Maybe (LinkAddr, BitVector 8) -> Maybe Op
stateOp (Just (a, d))
  | a >= stateWBase && a < stateWBase + 0x20000 = Just (OWrite (resize (a - stateWBase)) (unpack d))
  | a >= stateRBase && a < stateRBase + 0x20000 = Just (ORead (resize (a - stateRBase)))
stateOp _ = Nothing

data HookOut dom = HookOut
  { hoStopped :: Signal dom Bool                                   -- ^ no credit: paused and not on the way to a freeze
  , hoFrozen  :: Signal dom Bool                                   -- ^ the core takes no step (frozen, or freezing now)
  , hoOvl     :: Signal dom (Maybe (Unsigned 8))                   -- ^ the core's 'kiOvl'
  , hoRam     :: Signal dom (Maybe (Unsigned 16, Maybe (Unsigned 8))) -- ^ RAM's port while frozen
  , hoByte    :: Signal dom (Maybe (BitVector 8))                  -- ^ a state byte read, onto the byte channel
  }

-- | The hooks around the core: the link's writes, the menu's pause, the
-- PS's hold (the core at power-on), the core's state (this clock, before
-- its step), its outputs, RAM's output.
a800StateHooks
  :: forall dom. HiddenClockResetEnable dom
  => Signal dom (Maybe (LinkAddr, BitVector 8)) -> Signal dom Bool -> Signal dom Bool
  -> Signal dom CoreState -> Signal dom CoreOut -> Signal dom (Unsigned 8)
  -> HookOut dom
a800StateHooks link pause held core out ramD = HookOut stopped gate ovl ramP byte
 where
  op = stateOp <$> link
  -- (each register a binding of its own: a function calling itself would
  -- build a new one every clock)
  modeStep addr w m = case w of
    Just (a, d) | a == addr -> testBit d 0
    _ -> m
  stMode = register False (modeStep stateModeAddr <$> link <*> stMode)
  readBack = register False (modeStep readBackAddr <$> link <*> readBack)
  want = (&&) <$> stMode <*> pause
  freezeNow = (\w c -> w && restoreable c) <$> want <*> core
  frozen = register False ((\w z f h -> w && (z || f || h)) <$> want <*> frozen <*> freezeNow <*> held)
  gate = (||) <$> frozen <*> freezeNow
  stopped = (\p w z -> p && not (w && not z)) <$> pause <*> want <*> frozen
  -- accesses: the status any time, the rest frozen only
  fop = (\z o -> if z then o else case o of
                   Just (ORead a) | a >= stateStatusAddr && a < stateStatusAddr + 4 -> o
                   _ -> Nothing) <$> frozen <*> op
  inRange lo n a = a >= lo && a < lo + n
  ramP = (\o -> case o of
            Just (ORead a) | a >= ramBase -> Just (resize (a - ramBase), Nothing)
            Just (OWrite a d) | a >= ramBase -> Just (resize (a - ramBase), Just d)
            _ -> Nothing) <$> fop
  -- the ROM overlay
  ovlW = (\o -> case o of
            Just (OWrite a d) | inRange overlayBase 0x800 a -> Just (resize (a - overlayBase) :: Unsigned 11, d)
            _ -> Nothing) <$> fop
  cpuAddr = (\c -> rqAddr (cpuRequest (ksCpu c))) <$> core
  ovlRam = blockRam1 NoClearOnReset (SNat @2048) 0 ((\a -> resize a :: Unsigned 11) <$> cpuAddr) ovlW
  endStep k o v = case o of
    Just (OWrite a d) | a == overlayEndAddr + k -> d
    _ -> v
  endLo = register (0 :: Unsigned 8) (endStep 0 <$> fop <*> endLo)
  endHi = register (0 :: Unsigned 8) (endStep 1 <$> fop <*> endHi)
  endA = (\h l -> (resize h `shiftL` 8) .|. resize l) <$> endHi <*> endLo :: Signal dom (Unsigned 16)
  ev = koEvent <$> out
  readsEnd = (\e a -> case e of Just c -> ceCpu c && not (ceWrite c) && ceAddr c == a; Nothing -> False) <$> ev <*> endA
  stepEnd = (\e -> case e of Just c -> isJust (ceStep c); Nothing -> False) <$> ev
  ovlOn = register False ((\o on s d -> case o of
                             Just (OWrite a v) | a == overlayEndAddr + 2 -> testBit v 0
                             _ -> on && not (s && d)) <$> fop <*> ovlOn <*> seen <*> stepEnd)
  seen = register False ((\o on s r d -> case o of
                            Just (OWrite a _) | a == overlayEndAddr + 2 -> False
                            _ -> on && not (s && d) && (s || r)) <$> fop <*> ovlOn <*> seen <*> readsEnd <*> stepEnd)
  ovl = (\on b -> if on then Just b else Nothing) <$> ovlOn <*> ovlRam
  -- reads: the byte the clock after its access
  lastOp = register Nothing fop
  tabB = register 0 ((\o c -> case o of
                         Just (ORead a) | a < fromIntegral tableSize -> tapTable c (resize a)
                         _ -> 0) <$> fop <*> core)
  frzNow = register False frozen
  size16 = fromIntegral tableSize :: Unsigned 16
  rawByte = (\o z tb r -> case o of
               Just (ORead a)
                 | a == stateStatusAddr -> Just (if z then 1 else 0)
                 | a == stateStatusAddr + 1 -> Just (pack (truncateB size16 :: Unsigned 8))
                 | a == stateStatusAddr + 2 -> Just (pack (truncateB (size16 `shiftR` 8) :: Unsigned 8))
                 | a == stateStatusAddr + 3 -> Just stateTableVersion
                 | a < fromIntegral tableSize -> Just tb
                 | a >= ramBase -> Just (pack r)
                 | otherwise -> Just 0
               _ -> Nothing) <$> lastOp <*> frzNow <*> tabB <*> ramD
  byte = (\rb b -> if rb then b else Nothing) <$> readBack <*> rawByte
