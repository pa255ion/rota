{-|
The 6502's instructions as data (PLAN.md step 3.4): what an opcode does
('Op'), how it reaches its operand ('Mode'), and how it uses the operand
('Kind'). "Mos6502.Decode" maps the 256 opcodes onto these; the Clash
core ("Mos6502.Core") sequences the bus cycles of each 'Mode' and applies
the 'Op'.
-}
module Mos6502.Types
  ( Op (..)
  , Mode (..)
  , Kind (..)
  , Instr (..)
  ) where

import Clash.Prelude

data Op
  -- Loads, stores, transfers
  = LDA | LDX | LDY | STA | STX | STY | TAX | TAY | TXA | TYA | TSX | TXS
  -- ALU
  | ORA | AND | EOR | ADC | SBC | CMP | CPX | CPY | BIT
  -- Shifts and memory increments
  | ASL | LSR | ROL | ROR | INC | DEC | INX | INY | DEX | DEY
  -- Flags
  | CLC | SEC | CLI | SEI | CLV | CLD | SED
  -- Control
  | BPL | BMI | BVC | BVS | BCC | BCS | BNE | BEQ
  | JMP | JSR | RTS | RTI | BRK | PHA | PHP | PLA | PLP | NOP
  -- Undocumented
  | SLO | RLA | SRE | RRA | SAX | LAX | DCP | ISC
  | ANC | ALR | ARR | ANE | LXA | AXS | SHA | SHX | SHY | TAS | LAS
  | JAM
  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)
  deriving anyclass NFDataX

-- | Addressing, i.e. the bus cycles before (and around) the operand.
data Mode
  = Imp     -- ^ implied or accumulator: a dummy read at PC
  | Imm
  | Zp
  | ZpX
  | ZpY
  | Abs
  | AbsX
  | AbsY
  | IndX    -- ^ (zp,X)
  | IndY    -- ^ (zp),Y
  | Rel     -- ^ branches
  | JmpAbs
  | JmpInd
  | Jsr
  | Rts
  | Rti
  | Brk
  | Push    -- ^ PHA, PHP
  | Pull    -- ^ PLA, PLP
  | Jam     -- ^ the CPU stops
  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)
  deriving anyclass NFDataX

-- | How the operand is used: read, written (a store), or read, written
-- back unchanged and written modified.
data Kind = KRead | KWrite | KRmw
  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)
  deriving anyclass NFDataX

data Instr = Instr
  { iOp   :: Op
  , iMode :: Mode
  , iKind :: Kind
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFDataX
