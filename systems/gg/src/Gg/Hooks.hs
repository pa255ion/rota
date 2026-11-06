{-|
The Game Gear's and Master System's save-state hooks (side track N,
PLAN.md N.2), for "Gg.Machine" and "Sms.Machine" built with save states
('Platform.SaveState.WithStateHooks'; the Arty's tops). The Game Boy's
design (side track L, L.6c): saving only reads, loading runs a restore
routine on the machine's own Z80.

* __Freeze.__ The state mode ('stateModeAddr', bit 0) with the menu's
  pause asks for it: the machine runs on (without keys) to an
  instruction's end where a state can be taken ("Gg.StateTap"'s
  'restoreable') and the core gets no active clock from there; while the
  PS holds it (its reset) it is frozen at once. The machine's credit stops
  only once frozen.
* __The state space__, through windows on the link ('stateWBase' + n
  writes byte n, frozen only; 'stateRBase' + n reads it onto the byte
  channel): the state table ("Gg.StateTable", read only) from 0x0000; the
  status at 'stateStatusAddr' (1 while frozen, the table's size, its
  version; any time); the ROM overlay's end address and arm at
  'overlayEndAddr'; VRAM at 0x2000-0x5FFF; the overlay's 2 KiB at
  'overlayBase'; RAM at 0x8000-0x9FFF. Cartridge RAM has its own windows
  ("Gg.Save").
* __The ROM overlay.__ Armed while frozen, it answers every memory read
  below C000h (the core's 'giOvl', whatever the mapper and 3Eh say) from
  its block RAM at the CPU's address modulo 2 KiB, until the instruction
  that read its end address (a memory read: not an I/O cycle, whose
  address carries A) has ended; then off until armed again.
* __Time.__ The T-states since the core's power-on, modulo two frames
  ('tPhaseFrames'), for the table.
-}
module Gg.Hooks
  ( HookOut (..)
  , ggStateHooks
  , stateModeAddr
  , stateWBase
  , stateRBase
  , stateStatusAddr
  , overlayBase
  , overlayEndAddr
  ) where

import Clash.Prelude

import Gg.Core
import Gg.StateTable (tPhaseFrames, tableSize)
import Gg.StateTap
import Platform.Ps.Link (LinkAddr)
import Platform.SaveState (StateOp (..), stateWindow)
import Z80.Core (Cpu (..), Kind (..), Req (..), Z80T (..))

-- | The link map: the state mode and the state space's windows (as the
-- Game Boy's, docs/ARCHITECTURE.md).
stateModeAddr, stateWBase, stateRBase :: LinkAddr
stateModeAddr = 0x140050
stateWBase = 0x150000
stateRBase = 0x160000

-- | In the state space: the status bytes, the overlay's end address (low,
-- high) then its arm, the overlay's bytes.
stateStatusAddr, overlayEndAddr, overlayBase :: Unsigned 16
stateStatusAddr = 0x1F00
overlayEndAddr = 0x1F10
overlayBase = 0x6000

data HookOut dom = HookOut
  { hoStopped   :: Signal dom Bool                       -- ^ no credit: paused and not on the way to a freeze
  , hoGate      :: Signal dom Bool                       -- ^ no active clock for the core (frozen, or freezing now)
  , hoFrozen    :: Signal dom Bool                       -- ^ frozen (the status's flag)
  , hoOvl       :: Signal dom (Maybe (Unsigned 8))       -- ^ the core's 'giOvl'
  , hoRamAddr   :: Signal dom (Unsigned 13)              -- ^ RAM's address and write, the core's or the windows'
  , hoRamWrite  :: Signal dom (Maybe (Unsigned 13, Unsigned 8))
  , hoVramRead  :: Signal dom (Unsigned 12)              -- ^ VRAM's, the same way
  , hoVramWrite :: Signal dom (Maybe (Unsigned 14, Unsigned 8))
  , hoByte      :: Signal dom (Maybe (BitVector 8))      -- ^ a state byte read, the clock after its access
  }

-- | The hooks around a core: the link's writes, the menu's pause, the
-- PS's hold (the core in reset), /NMI's source (the Master System's pause
-- button), the core's state (as its clock starts) and outputs, whether the
-- core has an active clock, RAM's and VRAM's outputs.
ggStateHooks
  :: forall dom. HiddenClockResetEnable dom
  => Signal dom (Maybe (LinkAddr, BitVector 8)) -> Signal dom Bool -> Signal dom Bool -> Signal dom Bool
  -> Signal dom GgC -> Signal dom GgOut -> Signal dom Bool
  -> Signal dom (Unsigned 8) -> Signal dom (Vec 4 (Unsigned 8))
  -> HookOut dom
ggStateHooks link pause held nmi core out active ramD vramD =
  HookOut stopped gate frozen ovl ramA ramW vramA vramW byte
 where
  op = stateWindow stateWBase stateRBase <$> link
  stMode = register False ((\w m -> case w of
                              Just (a, d) | a == stateModeAddr -> testBit d 0
                              _ -> m) <$> link <*> stMode)
  want = (&&) <$> stMode <*> pause
  -- an instruction ended on the core's last active clock (none since)
  atEnd = register False ((\h a e x -> not h && (if a then e else x)) <$> held <*> active <*> (goInstrEnd <$> out) <*> atEnd)
  freezeNow = (\w e n c -> w && e && restoreable n c) <$> want <*> atEnd <*> nmi <*> core
  frozen = register False ((\w z f h -> w && (z || f || h)) <$> want <*> frozen <*> freezeNow <*> held)
  gate = (||) <$> frozen <*> freezeNow
  stopped = (\p w z -> p && not (w && not z)) <$> pause <*> want <*> frozen
  -- accesses: the status any time, the rest frozen only
  fop = (\z o -> if z then o else case o of
                   Just (StRead a) | a >= stateStatusAddr && a < stateStatusAddr + 4 -> o
                   _ -> Nothing) <$> frozen <*> op
  inRange lo n a = a >= lo && a < lo + n
  -- RAM and VRAM: the windows' while frozen
  ramA = (\z o x -> case (z, o) of
            (True, Just (StRead a)) | inRange 0x8000 0x2000 a -> resize (a - 0x8000)
            (True, Just (StWrite a _)) | inRange 0x8000 0x2000 a -> resize (a - 0x8000)
            _ -> x) <$> frozen <*> fop <*> (goRamAddr <$> out)
  ramW = (\z o x -> case (z, o) of
            (True, Just (StWrite a d)) | inRange 0x8000 0x2000 a -> Just (resize (a - 0x8000), unpack d)
            (True, _) -> Nothing
            _ -> x) <$> frozen <*> fop <*> (goRamWrite <$> out)
  vramA = (\z o x -> case (z, o) of
             (True, Just (StRead a)) | inRange 0x2000 0x4000 a -> resize ((a - 0x2000) `shiftR` 2)
             _ -> x) <$> frozen <*> fop <*> (goVramRead <$> out)
  vramW = (\z o x -> case (z, o) of
             (True, Just (StWrite a d)) | inRange 0x2000 0x4000 a -> Just (resize (a - 0x2000), unpack d)
             (True, _) -> Nothing
             _ -> x) <$> frozen <*> fop <*> (goVramWrite <$> out)
  -- the T-states since power-on, modulo two frames
  time = register 0 ((\h a o t -> if h then 0 else if a && goTState o then (if t + 1 == fromIntegral tPhaseFrames then 0 else t + 1) else t)
                       <$> held <*> active <*> out <*> time) :: Signal dom (Unsigned 17)
  -- the ROM overlay
  ovlW = (\o -> case o of
            Just (StWrite a d) | inRange overlayBase 0x800 a -> Just (resize (a - overlayBase) :: Unsigned 11, unpack d)
            _ -> Nothing) <$> fop
  ovlRam = blockRam1 NoClearOnReset (SNat @2048) 0 ((\o -> resize (goCpuAddr o) :: Unsigned 11) <$> out) ovlW
  endLo = register 0 ((\o v -> case o of Just (StWrite a d) | a == overlayEndAddr -> unpack d; _ -> v) <$> fop <*> endLo) :: Signal dom (Unsigned 8)
  endHi = register 0 ((\o v -> case o of Just (StWrite a d) | a == overlayEndAddr + 1 -> unpack d; _ -> v) <$> fop <*> endHi) :: Signal dom (Unsigned 8)
  endA = (\h l -> (resize h `shiftL` 8) .|. resize l) <$> endHi <*> endLo :: Signal dom (Unsigned 16)
  readsEnd = (\c e -> let rq = cReq (zCpu (gZ c)) in rqKind rq == KRead && rqAddr rq == e) <$> core <*> endA
  ovlOn = register False ovlOn'
  seen = register False seen'
  done = (\s a o -> s && a && goInstrEnd o) <$> seen <*> active <*> out
  ovlOn' = (\o on d -> case o of
              Just (StWrite a v) | a == overlayEndAddr + 2 -> testBit v 0
              _ -> on && not d) <$> fop <*> ovlOn <*> done
  seen' = (\on' d s r -> on' && not d && (s || r)) <$> ovlOn' <*> done <*> seen <*> ((&&) <$> ovlOn <*> readsEnd)
  ovl = (\on b -> if on then Just b else Nothing) <$> ovlOn <*> ovlRam
  -- reads: the byte the clock after its access
  lastOp = register Nothing fop
  tabB = register 0 ((\o c t -> case o of
                        Just (StRead a) | a < fromIntegral tableSize -> tapTable c t a
                        _ -> 0) <$> fop <*> core <*> time)
  frzNow = register False frozen
  byte = (\o z tb r v -> case o of
            Just (StRead a)
              | a == stateStatusAddr -> Just (if z then 1 else 0)
              | a == stateStatusAddr + 1 -> Just (resize (pack (fromIntegral tableSize :: Unsigned 16)))
              | a == stateStatusAddr + 2 -> Just (slice d15 d8 (pack (fromIntegral tableSize :: Unsigned 16)))
              | a == stateStatusAddr + 3 -> Just stateTableVersion
              | a < fromIntegral tableSize -> Just tb
              | inRange 0x8000 0x2000 a -> Just (pack r)
              | inRange 0x2000 0x4000 a -> Just (pack (v !! (resize (a - 0x2000) :: Unsigned 2)))
              | otherwise -> Just 0
            _ -> Nothing) <$> lastOp <*> frzNow <*> tabB <*> ramD <*> vramD
