{-|
CHIP-8's state table in the core (side track N, PLAN.md N.10): the bytes
of "Chip8.StateTable" read from 'CoreState' ('tapTable', as
'Chip8.SpecState.specTable' has them), one written back into it
('tableWrite': the direct path's load, the core frozen), and where a state
can be taken ('restoreable': between two instructions).
-}
module Chip8.StateTap
  ( tapTable
  , tableWrite
  , restoreable
  , stateTableVersion
  ) where

import Clash.Prelude

import Chip8.Core
import Chip8.StateTable

-- | Bumped whenever the table's meaning changes (a slot of another
-- bitstream is refused): 1.
stateTableVersion :: BitVector 8
stateTableVersion = 1

-- | The table of a core state, all 64 bytes; @live@: the stack's entries at
-- and above SP 0 (they are dead, the spec keeps none), as reads see them;
-- writes build on all of them (a table is written byte by byte, the stack
-- before SP).
tableOf :: Bool -> CoreState -> Vec 64 (BitVector 8)
tableOf live s = map field (iterateI (+ 1) 0)
 where
  a = csArch s
  lo, hi :: Unsigned 16 -> BitVector 8
  lo x = pack (truncateB x :: Unsigned 8)
  hi x = pack (truncateB (x `shiftR` 8) :: Unsigned 8)
  b :: Bool -> BitVector 8
  b x = if x then 1 else 0
  stackAt k = if not live || fromIntegral k < aSp a then zeroExtend (aStack a !! (k :: Index 16)) else 0 :: Unsigned 16
  field :: Unsigned 6 -> BitVector 8
  field i = case fromIntegral i :: Int of
    o | o == tPc -> lo (zeroExtend (aPc a))
      | o == tPc + 1 -> hi (zeroExtend (aPc a))
      | o == tI -> lo (aI a)
      | o == tI + 1 -> hi (aI a)
      | o >= tV && o < tV + 16 -> aV a !! (fromIntegral (o - tV) :: Index 16)
      | o >= tStack && o < tStack + 32 ->
          let k = fromIntegral ((o - tStack) `div` 2) :: Index 16
          in if even (o - tStack) then lo (stackAt k) else hi (stackAt k)
      | o == tSp -> pack (resize (pack (aSp a)) :: BitVector 8)
      | o == tDelay -> aDelay a
      | o == tSound -> aSound a
      | o == tRng -> slice d7 d0 (aRng a)
      | o == tRng + 1 -> slice d15 d8 (aRng a)
      | o == tVblank -> case aVblank a of VIdle -> 0; VWaiting -> 1; VReleased -> 2
      | o == tKeyWait -> maybe 0xFF (\k -> resize (pack k)) (aKeyWait a)
      | o == tHalted -> b (aHalted a)
      | o == tTickPend -> b (csTickPend s)
      | o == tInstrPend -> b (csInstrPend s)
      | otherwise -> 0

-- | Byte @n@ of the table.
tapTable :: CoreState -> Unsigned 6 -> BitVector 8
tapTable s n = tableOf True s !! n

-- | Byte @n@ of the table written into a core between instructions.
tableWrite :: CoreState -> Unsigned 6 -> BitVector 8 -> CoreState
tableWrite s n v = s
  { csArch = Arch
      { aPc = resize (u16 tPc)
      , aI = u16 tI
      , aV = map (\k -> t !! (fromIntegral tV + k :: Index 64)) (iterateI (+ 1) 0)
      , aStack = map (\k -> resize (u16 (tStack + 2 * fromIntegral k))) (indicesI :: Vec 16 (Index 16))
      , aSp = let x = unpack (t !! (fromIntegral tSp :: Index 64)) :: Unsigned 8 in if x > 16 then 16 else resize (bitCoerce x :: Index 256)
      , aDelay = byte tDelay
      , aSound = byte tSound
      , aRng = byte (tRng + 1) ++# byte tRng
      , aVblank = case byte tVblank of
          1 -> VWaiting
          2 -> VReleased
          _ -> VIdle
      , aKeyWait = let k = byte tKeyWait in if k == 0xFF then Nothing else Just (unpack (resize k))
      , aHalted = byte tHalted /= 0 }
  , csPhase = PIdle
  , csTickPend = byte tTickPend /= 0
  , csInstrPend = byte tInstrPend /= 0 }
 where
  t = replace n v (tableOf False s)
  byte o = t !! (fromIntegral o :: Index 64)
  u16 o = unpack (byte (o + 1) ++# byte o) :: Unsigned 16

-- | Where a state can be taken: between two instructions (and not while
-- the framebuffer is cleared after a reset).
restoreable :: CoreState -> Bool
restoreable s = csPhase s == PIdle
