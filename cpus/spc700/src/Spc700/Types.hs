{-|
The SPC700's registers (fullsnes, "SNES APU SPC700 CPU"), shared by the
reference model (step 7.2), the Clash core (7.8) and the tests. YA is A
(low) and Y (high) together; the stack is in page 1 (01xxh).
-}
module Spc700.Types
  ( Regs (..)
  , flagC, flagZ, flagI, flagH, flagB, flagP, flagV, flagN
  ) where

import Prelude

import Data.Word

data Regs = Regs
  { rA   :: !Word8
  , rX   :: !Word8
  , rY   :: !Word8
  , rSP  :: !Word8
  , rPC  :: !Word16
  , rPSW :: !Word8
  } deriving stock (Show, Eq)

flagC, flagZ, flagI, flagH, flagB, flagP, flagV, flagN :: Word8
flagC = 0x01
flagZ = 0x02
flagI = 0x04 -- ^ interrupt enable (no interrupt source on the SNES)
flagH = 0x08 -- ^ half carry
flagB = 0x10 -- ^ break
flagP = 0x20 -- ^ the direct page: 0000h-00FFh when clear, 0100h-01FFh when set
flagV = 0x40
flagN = 0x80
