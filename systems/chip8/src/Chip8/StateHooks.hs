{-|
CHIP-8's save-state hooks (side track N, PLAN.md N.10), for the Arty's top:
the direct path (CHIP-8 has no machine code to load a state with). The
other systems' link map ("Nes.StateHooks", "A800.Hooks").

* __Freeze.__ The state mode ('stateModeAddr', bit 0) with the menu's
  pause asks for it: the core freezes between two instructions
  ("Chip8.StateTap"'s 'restoreable') and takes no step from there (the
  timers stop too).
* __The state space__, through windows on the link ('stateWBase' + n
  writes byte n, frozen only; 'stateRBase' + n reads it onto the byte
  channel): the state table ("Chip8.StateTable") from 0x0000, read from
  the core's record and written into it; the status at 'stateStatusAddr'
  (1 while frozen, the table's size, its version; any time); RAM's 4 KiB
  at 'ramBase'; the framebuffer's 256 bytes at 'fbBase' (32 rows of 8,
  bit 7 of a row's first byte column 0; a row is written when its last
  byte is). No battery saves: the byte channel carries the reads while
  'readBackAddr' bit 0 is set.
-}
module Chip8.StateHooks
  ( HookOut (..)
  , chip8StateHooks
  , stateModeAddr
  , readBackAddr
  , stateWBase
  , stateRBase
  , stateStatusAddr
  , ramBase
  , fbBase
  ) where

import Clash.Prelude

import Chip8.Core
import Chip8.StateTable (tableSize)
import Chip8.StateTap
import Chip8.Types (Byte)
import Platform.Ps.Link (LinkAddr)

stateModeAddr, readBackAddr, stateWBase, stateRBase :: LinkAddr
stateModeAddr = 0x140050
readBackAddr = 0x140000
stateWBase = 0x150000
stateRBase = 0x170000

stateStatusAddr, ramBase, fbBase :: Unsigned 16
stateStatusAddr = 0x1F00
ramBase = 0x2000
fbBase = 0x3000

data Op = ORead (Unsigned 16) | OWrite (Unsigned 16) (Unsigned 8)
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFDataX

stateOp :: Maybe (LinkAddr, BitVector 8) -> Maybe Op
stateOp (Just (a, d))
  | a >= stateWBase && a < stateWBase + 0x10000 = Just (OWrite (resize (a - stateWBase)) (unpack d))
  | a >= stateRBase && a < stateRBase + 0x10000 = Just (ORead (resize (a - stateRBase)))
stateOp _ = Nothing

data HookOut dom = HookOut
  { hoFrozen :: Signal dom Bool                              -- ^ the core takes no step
  , hoRecWr  :: Signal dom (Maybe (Unsigned 6, BitVector 8)) -- ^ a table byte into the record
  , hoPort   :: Signal dom (Maybe StPort)                    -- ^ RAM's or the framebuffer's port
  , hoByte   :: Signal dom (Maybe (BitVector 8))             -- ^ a state byte read, onto the byte channel
  }

-- | The hooks: the link's writes, the menu's pause, the PS's hold, the
-- core's state (this clock, before its step), RAM's byte and the
-- framebuffer row the core's port read in the last clock.
chip8StateHooks
  :: forall dom. HiddenClockResetEnable dom
  => Signal dom (Maybe (LinkAddr, BitVector 8)) -> Signal dom Bool -> Signal dom Bool
  -> Signal dom CoreState -> Signal dom Byte -> Signal dom (BitVector 64)
  -> HookOut dom
chip8StateHooks link pause held core ramD fbD = HookOut gate recWr port byte
 where
  op = stateOp <$> link
  modeStep addr w m = case w of
    Just (a, d) | a == addr -> testBit d 0
    _ -> m
  stMode = register False (modeStep stateModeAddr <$> link <*> stMode)
  readBack = register False (modeStep readBackAddr <$> link <*> readBack)
  want = (&&) <$> stMode <*> pause
  freezeNow = (\w c -> w && restoreable c) <$> want <*> core
  frozen = register False ((\w z f h -> w && (z || f || h)) <$> want <*> frozen <*> freezeNow <*> held)
  gate = (||) <$> frozen <*> freezeNow
  fop = (\z o -> if z then o else case o of
                   Just (ORead a) | a >= stateStatusAddr && a < stateStatusAddr + 4 -> o
                   _ -> Nothing) <$> frozen <*> op
  inRange lo n a = a >= lo && a - lo < n
  recWr = (\o -> case o of
             Just (OWrite a d) | a < fromIntegral tableSize -> Just (resize a, pack d)
             _ -> Nothing) <$> fop
  -- the framebuffer's row being written, a byte at a time
  rowBuf = register (repeat 0 :: Vec 8 Byte) (rowStep <$> fop <*> rowBuf)
  rowStep o buf = case o of
    Just (OWrite a d) | inRange fbBase 0x100 a -> replace (fromIntegral ((a - fbBase) .&. 7) :: Index 8) (pack d) buf
    _ -> buf
  port = (\o buf -> case o of
            Just (OWrite a d)
              | inRange ramBase 0x1000 a -> Just (StRam (resize (a - ramBase)) (Just (pack d)))
              | inRange fbBase 0x100 a && (a - fbBase) .&. 7 == 7 ->
                  let buf' = replace (7 :: Index 8) (pack d) buf
                  in Just (StFb (fromIntegral ((a - fbBase) `shiftR` 3)) (Just (pack buf')))
            Just (ORead a)
              | inRange ramBase 0x1000 a -> Just (StRam (resize (a - ramBase)) Nothing)
              | inRange fbBase 0x100 a -> Just (StFb (fromIntegral ((a - fbBase) `shiftR` 3)) Nothing)
            _ -> Nothing) <$> fop <*> rowBuf
  -- reads: the byte the clock after its access
  lastOp = register Nothing fop
  tabB = register 0 ((\o c -> case o of
                         Just (ORead a) | a < fromIntegral tableSize -> tapTable c (resize a)
                         _ -> 0) <$> fop <*> core)
  frzNow = register False frozen
  size16 = fromIntegral tableSize :: Unsigned 16
  rawByte = (\o z tb r fb -> case o of
               Just (ORead a)
                 | a == stateStatusAddr -> Just (if z then 1 else 0)
                 | a == stateStatusAddr + 1 -> Just (pack (truncateB size16 :: Unsigned 8))
                 | a == stateStatusAddr + 2 -> Just (pack (truncateB (size16 `shiftR` 8) :: Unsigned 8))
                 | a == stateStatusAddr + 3 -> Just stateTableVersion
                 | a < fromIntegral tableSize -> Just tb
                 | inRange ramBase 0x1000 a -> Just r
                 -- (the byte by a shift, not a vector index: Vivado removed
                 -- the framebuffer's read RAM as unreachable with the index,
                 -- though Verilator and Yosys kept it: zeros on the board)
                 | inRange fbBase 0x100 a ->
                     let sh = fromIntegral (8 * (7 - ((a - fbBase) .&. 7))) :: Int
                     in Just (truncateB (fb `shiftR` sh) :: BitVector 8)
                 | otherwise -> Just 0
               _ -> Nothing) <$> lastOp <*> frzNow <*> tabB <*> ramD <*> fbD
  byte = (\rb b -> if rb then b else Nothing) <$> readBack <*> rawByte
