{-|
CHIP-8 instruction set as a Clash data type (PLAN.md step 1.2). The
hardware core decodes every fetched word into an 'Instr' with
"Chip8.Decode"; this is independent of the reference model's own decoding
("Chip8.Spec"), and the tests check the two agree on every 16-bit word.
-}
module Chip8.Types
  ( Reg
  , Addr
  , Byte
  , AluOp (..)
  , Instr (..)
  ) where

import Clash.Prelude

-- | V0 to VF.
type Reg = Index 16

-- | A 12-bit address.
type Addr = Unsigned 12

type Byte = BitVector 8

-- | The @8xyN@ operations, by N.
data AluOp
  = AluMov   -- ^ 0: Vx = Vy
  | AluOr    -- ^ 1
  | AluAnd   -- ^ 2
  | AluXor   -- ^ 3
  | AluAdd   -- ^ 4: VF = carry
  | AluSub   -- ^ 5: Vx = Vx - Vy, VF = no borrow
  | AluShr   -- ^ 6
  | AluSubn  -- ^ 7: Vx = Vy - Vx, VF = no borrow
  | AluShl   -- ^ E
  deriving stock (Show, Eq, Generic, Enum, Bounded)
  deriving anyclass NFDataX

data Instr
  = Cls                    -- ^ 00E0
  | Ret                    -- ^ 00EE
  | Jp Addr                -- ^ 1nnn
  | Call Addr              -- ^ 2nnn
  | SeImm Reg Byte         -- ^ 3xkk
  | SneImm Reg Byte        -- ^ 4xkk
  | SeReg Reg Reg          -- ^ 5xy0
  | LdImm Reg Byte         -- ^ 6xkk
  | AddImm Reg Byte        -- ^ 7xkk
  | Alu AluOp Reg Reg      -- ^ 8xyN
  | SneReg Reg Reg         -- ^ 9xy0
  | LdI Addr               -- ^ Annn
  | JpOff Addr             -- ^ Bnnn (V0 + nnn, or Vx + nnn with the jump quirk off)
  | Rnd Reg Byte           -- ^ Cxkk
  | Drw Reg Reg (Index 16) -- ^ Dxyn
  | Skp Reg                -- ^ Ex9E
  | Sknp Reg               -- ^ ExA1
  | LdVxDt Reg             -- ^ Fx07
  | LdKey Reg              -- ^ Fx0A
  | LdDtVx Reg             -- ^ Fx15
  | LdStVx Reg             -- ^ Fx18
  | AddI Reg               -- ^ Fx1E
  | LdFont Reg             -- ^ Fx29
  | Bcd Reg                -- ^ Fx33
  | Store Reg              -- ^ Fx55
  | Load Reg               -- ^ Fx65
  | Illegal (BitVector 16) -- ^ anything else; halts the machine
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFDataX
