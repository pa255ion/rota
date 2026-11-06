{-|
Save states on CHIP-8's reference model (side track N, PLAN.md N.10): the
state table ("Chip8.StateTable") of a machine, its RAM and framebuffer as
the PS keeps them, and a machine made from all three, the direct path the
core's hooks take (no restore routine: CHIP-8 has no machine code to run
one with). Every moment between two steps is one a state can be taken at.
-}
module Chip8.SpecState
  ( specTable
  , specRam
  , specFb
  , specLoad
  ) where

import Prelude

import Data.Bits
import qualified Data.Vector.Unboxed as U
import Data.Word

import Chip8.Spec
import Chip8.StateTable

-- | The table of a machine (its latched ticks 0: the spec has none).
specTable :: Machine -> U.Vector Word8
specTable m = U.replicate tableSize 0 U.// concat
  [ w16 tPc (mPc m), w16 tI (mI m)
  , zip [tV ..] (U.toList (mV m))
  , concat [ w16 (tStack + 2 * k) a | (k, a) <- zip [0 ..] (reverse (mStack m)) ]
  , [ (tSp, fromIntegral (length (mStack m))), (tDelay, mDelay m), (tSound, mSound m) ]
  , w16 tRng (mRng m)
  , [ (tVblank, case mVblank m of VblankIdle -> 0; VblankWaiting -> 1; VblankReleased -> 2)
    , (tKeyWait, case mKeyWait m of KeyWaitNone -> 0xFF; KeyWaitHeld k -> k)
    , (tHalted, maybe 0 (const 1) (mHalt m)) ]
  ]
 where
  w16 o v = [(o, fromIntegral v), (o + 1, fromIntegral (v `shiftR` 8))]

-- | RAM, 4096 bytes.
specRam :: Machine -> U.Vector Word8
specRam = mRam

-- | The framebuffer as the core keeps it: 32 rows of 8 bytes, bit 7 of a
-- row's first byte column 0.
specFb :: Machine -> U.Vector Word8
specFb m = U.generate 256 byte
 where
  byte i = foldl (\acc b -> (acc `shiftL` 1) .|. (if pixelAt m (8 * (i `mod` 8) + b) (i `div` 8) then 1 else 0)) 0 [0 .. 7]

-- | A machine from a table, RAM and a framebuffer (a halt's reason, which
-- the table doesn't keep: an illegal opcode 0).
specLoad :: U.Vector Word8 -> U.Vector Word8 -> U.Vector Word8 -> Machine
specLoad tb ram fb = Machine
  { mRam = ram
  , mV = U.slice tV 16 tb
  , mI = r16 tI
  , mPc = r16 tPc .&. 0xFFF
  , mStack = reverse [ r16 (tStack + 2 * k) .&. 0xFFF | k <- [0 .. sp - 1] ]
  , mDelay = tb U.! tDelay
  , mSound = tb U.! tSound
  , mDisplay = U.generate (64 * 32) (\p -> let (y, x) = p `divMod` 64 in testBit (fb U.! (y * 8 + x `div` 8)) (7 - x `mod` 8))
  , mRng = r16 tRng
  , mVblank = case tb U.! tVblank of
      1 -> VblankWaiting
      2 -> VblankReleased
      _ -> VblankIdle
  , mKeyWait = case tb U.! tKeyWait of
      0xFF -> KeyWaitNone
      k -> KeyWaitHeld k
  , mHalt = if tb U.! tHalted /= 0 then Just (HaltIllegal 0) else Nothing
  }
 where
  r16 o = fromIntegral (tb U.! o) .|. (fromIntegral (tb U.! (o + 1)) `shiftL` 8) :: Word16
  sp = min 16 (fromIntegral (tb U.! tSp)) :: Int
