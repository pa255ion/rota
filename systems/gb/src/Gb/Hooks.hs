{-|
Test hooks shared by the reference model and the Clash simulations (M2
step 2.0, docs/systems/gameboy.md "Test hooks"): pure functions over what
a running machine emits, so that every layer reads a test ROM's result
the same way.

* __Serial capture__ ('serialWrite'): Blargg's ROMs print their result
  over the link port: the byte goes to SB (0xFF01), then a write to SC
  (0xFF02) with bit 7 (start) and bit 0 (internal clock) set sends it.
  The captured byte is SB's value at that SC write.
* __Blargg verdict__ ('blarggVerdict'): the captured text contains
  "Passed" (single ROMs) or "Passed all tests" (combined ROMs) at the end,
  or "Failed" with details.
* __Mooneye verdict__ ('mooneyeVerdict'): Mooneye's ROMs end by executing
  @LD B,B@ (opcode 0x40, 'mooneyeBreakpoint') with B, C, D, E, H, L =
  3, 5, 8, 13, 21, 34 for a pass, or all 0x42 for a failure.
-}
module Gb.Hooks
  ( -- * Serial capture
    Serial (..)
  , serialIdle
  , serialWrite
  , serialText
  , regSB
  , regSC
    -- * Verdicts
  , Verdict (..)
  , blarggVerdict
  , MooneyeRegs (..)
  , mooneyeBreakpoint
  , mooneyeVerdict
  ) where

import Prelude

import Data.Bits ((.&.))
import Data.List (isInfixOf)
import Data.Word

-- | What the serial port has sent so far.
data Serial = Serial
  { serSb  :: !Word8   -- ^ SB as last written by the CPU
  , serOut :: [Word8]  -- ^ sent bytes, newest first
  } deriving stock (Show, Eq)

regSB, regSC :: Word16
regSB = 0xFF01
regSC = 0xFF02

-- | SB holds 0 at power-up as far as this capture is concerned; Blargg's
-- ROMs always write SB before starting a transfer.
serialIdle :: Serial
serialIdle = Serial 0 []

-- | Feed one CPU write (address, data). Writes elsewhere are ignored, so a
-- whole bus trace can be folded through this.
serialWrite :: Word16 -> Word8 -> Serial -> Serial
serialWrite a d s
  | a == regSB                 = s { serSb = d }
  | a == regSC, d .&. 0x81 == 0x81 = s { serOut = serSb s : serOut s }
  | otherwise                  = s

-- | The sent bytes, oldest first, as Latin-1 text.
serialText :: Serial -> String
serialText = map (toEnum . fromIntegral) . reverse . serOut

data Verdict = Pass | Fail String | Running
  deriving stock (Show, Eq)

-- | Blargg's ROMs print "Failed" (with a test number or a list) or
-- "Passed"; until either appears the ROM is still running.
blarggVerdict :: String -> Verdict
blarggVerdict t
  | "Failed" `isInfixOf` t = Fail t
  | "Passed" `isInfixOf` t = Pass
  | otherwise              = Running

data MooneyeRegs = MooneyeRegs
  { mrB, mrC, mrD, mrE, mrH, mrL :: Word8 }
  deriving stock (Show, Eq)

-- | @LD B,B@.
mooneyeBreakpoint :: Word8
mooneyeBreakpoint = 0x40

-- | The registers at the breakpoint. Anything other than the pass pattern
-- is a failure; the message says whether it was Mooneye's own failure
-- pattern or something unexpected.
mooneyeVerdict :: MooneyeRegs -> Verdict
mooneyeVerdict r
  | regs == [3, 5, 8, 13, 21, 34] = Pass
  | all (== 0x42) regs            = Fail "Mooneye failure pattern (all 0x42)"
  | otherwise                     = Fail ("unexpected registers at LD B,B: " <> show r)
 where
  regs = [mrB r, mrC r, mrD r, mrE r, mrH r, mrL r]
