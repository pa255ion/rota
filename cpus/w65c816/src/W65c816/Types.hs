{-|
The 65C816's programmer-visible registers (WDC's W65C816S data sheet,
"Programming model"), shared by the reference model (step 7.1), the
Clash core (7.7) and the tests. A, X and Y are kept 16 bits wide in
every mode: with M (or X) set only their low byte takes part, and B (A's
high byte) keeps its value; X and Y's high bytes are 0 while X is set.
-}
module W65c816.Types
  ( Regs (..)
  , flagC, flagZ, flagI, flagD, flagX, flagM, flagV, flagN, flagB
  , resetRegs
  ) where

import Prelude

import Data.Bits ((.|.))
import Data.Word

data Regs = Regs
  { rA   :: !Word16 -- ^ the accumulator, C (B:A)
  , rX   :: !Word16
  , rY   :: !Word16
  , rS   :: !Word16 -- ^ the stack pointer (page 1 in emulation mode)
  , rD   :: !Word16 -- ^ the direct page register
  , rDBR :: !Word8  -- ^ the data bank
  , rPBR :: !Word8  -- ^ the program bank
  , rPC  :: !Word16
  , rP   :: !Word8  -- ^ the status register (bit 4 X, bit 5 M in native mode)
  , rE   :: !Bool   -- ^ emulation mode
  } deriving stock (Show, Eq)

flagC, flagZ, flagI, flagD, flagX, flagM, flagV, flagN :: Word8
flagC = 0x01
flagZ = 0x02
flagI = 0x04
flagD = 0x08
flagX = 0x10 -- ^ native mode: 8-bit index registers
flagM = 0x20 -- ^ native mode: 8-bit accumulator and memory
flagV = 0x40
flagN = 0x80

-- | Bit 4 in emulation mode: B, set in the copy BRK pushes.
flagB :: Word8
flagB = 0x10

-- | After /RES (data sheet, "Reset"): emulation mode, M, X and I set, D
-- clear, D = 0, the banks 0, S in page 1, the high bytes of X and Y 0;
-- the PC comes from the vector at 00FFFCh, read by the caller.
resetRegs :: Regs
resetRegs = Regs
  { rA = 0, rX = 0, rY = 0, rS = 0x01FF, rD = 0
  , rDBR = 0, rPBR = 0, rPC = 0
  , rP = flagM .|. flagX .|. flagI
  , rE = True
  }
