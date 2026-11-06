{-|
The SM83 instruction set as a Clash data type (PLAN.md step 2.3). The
hardware core ("Gb.Cpu", step 2.4) decodes every fetched opcode into an
'Instr' (and every byte after a CB prefix into a 'CbInstr') with
"Gb.Decode". That decoding is independent of the reference model's
("Gb.Spec.Cpu"), and the tests check the two agree on every opcode
(length, M-cycles, the illegal set).

The constructors follow the usual opcode table layout (x/y/z/p/q fields):
register operands are the opcode's 3-bit and 2-bit fields, so decoding is
a few bit slices.
-}
module Gb.Types
  ( R8 (..)
  , R16 (..)
  , R16Stk (..)
  , R16Mem (..)
  , Cond (..)
  , AluOp (..)
  , AccOp (..)
  , RotOp (..)
  , Illegal (..)
  , Instr (..)
  , CbInstr (..)
  ) where

import Clash.Prelude

-- | 8-bit operands, in opcode order; 'HLInd' is the byte at (HL).
data R8 = B | C | D | E | H | L | HLInd | A
  deriving stock (Show, Eq, Ord, Generic, Enum, Bounded)
  deriving anyclass (NFDataX, BitPack)

-- | 16-bit operands of LD rp,nn / INC rp / DEC rp / ADD HL,rp.
data R16 = BC | DE | HL | SP
  deriving stock (Show, Eq, Ord, Generic, Enum, Bounded)
  deriving anyclass (NFDataX, BitPack)

-- | PUSH / POP operands.
data R16Stk = StkBC | StkDE | StkHL | StkAF
  deriving stock (Show, Eq, Ord, Generic, Enum, Bounded)
  deriving anyclass (NFDataX, BitPack)

-- | LD (rr),A / LD A,(rr) operands: (BC), (DE), (HL+), (HL-).
data R16Mem = MemBC | MemDE | MemHLI | MemHLD
  deriving stock (Show, Eq, Ord, Generic, Enum, Bounded)
  deriving anyclass (NFDataX, BitPack)

-- | Branch conditions NZ, Z, NC, C.
data Cond = CondNZ | CondZ | CondNC | CondC
  deriving stock (Show, Eq, Ord, Generic, Enum, Bounded)
  deriving anyclass (NFDataX, BitPack)

-- | The eight ALU operations on A.
data AluOp = AluAdd | AluAdc | AluSub | AluSbc | AluAnd | AluXor | AluOr | AluCp
  deriving stock (Show, Eq, Ord, Generic, Enum, Bounded)
  deriving anyclass (NFDataX, BitPack)

-- | The one-byte operations on A and the flags (x = 0, z = 7).
data AccOp = AccRlca | AccRrca | AccRla | AccRra | AccDaa | AccCpl | AccScf | AccCcf
  deriving stock (Show, Eq, Ord, Generic, Enum, Bounded)
  deriving anyclass (NFDataX, BitPack)

-- | CB-prefixed rotates and shifts.
data RotOp = RotRlc | RotRrc | RotRl | RotRr | RotSla | RotSra | RotSwap | RotSrl
  deriving stock (Show, Eq, Ord, Generic, Enum, Bounded)
  deriving anyclass (NFDataX, BitPack)

-- | The 11 unused opcodes; executing one locks the CPU up.
data Illegal = IllD3 | IllDB | IllDD | IllE3 | IllE4 | IllEB | IllEC | IllED | IllF4 | IllFC | IllFD
  deriving stock (Show, Eq, Ord, Generic, Enum, Bounded)
  deriving anyclass NFDataX

data Instr
  = Nop                   -- ^ 00
  | LdNnSp                -- ^ 08: LD (nn),SP
  | Stop                  -- ^ 10: STOP (a 2-byte NOP in this design)
  | Jr                    -- ^ 18: JR e
  | JrCc Cond             -- ^ 20 28 30 38
  | LdRpNn R16            -- ^ 01 11 21 31
  | AddHlRp R16           -- ^ 09 19 29 39
  | LdMemA R16Mem         -- ^ 02 12 22 32
  | LdAMem R16Mem         -- ^ 0A 1A 2A 3A
  | IncRp R16             -- ^ 03 13 23 33
  | DecRp R16             -- ^ 0B 1B 2B 3B
  | Inc R8                -- ^ 04 0C ... 3C
  | Dec R8                -- ^ 05 0D ... 3D
  | LdRN R8               -- ^ 06 0E ... 3E
  | Acc AccOp             -- ^ 07 0F ... 3F
  | Halt                  -- ^ 76
  | LdRR R8 R8            -- ^ 40-7F except 76: destination, source; never 'HLInd' twice
  | Alu AluOp R8          -- ^ 80-BF
  | RetCc Cond            -- ^ C0 C8 D0 D8
  | LdhNA                 -- ^ E0: LDH (n),A
  | AddSpE                -- ^ E8
  | LdhAN                 -- ^ F0: LDH A,(n)
  | LdHlSpE               -- ^ F8: LD HL,SP+e
  | Pop R16Stk            -- ^ C1 D1 E1 F1
  | Ret                   -- ^ C9
  | Reti                  -- ^ D9
  | JpHl                  -- ^ E9
  | LdSpHl                -- ^ F9
  | JpCc Cond             -- ^ C2 CA D2 DA
  | LdhCA                 -- ^ E2: LD (FF00+C),A
  | LdNnA                 -- ^ EA
  | LdhAC                 -- ^ F2: LD A,(FF00+C)
  | LdANn                 -- ^ FA
  | Jp                    -- ^ C3
  | PrefixCb              -- ^ CB
  | Di                    -- ^ F3
  | Ei                    -- ^ FB
  | CallCc Cond           -- ^ C4 CC D4 DC
  | Push R16Stk           -- ^ C5 D5 E5 F5
  | Call                  -- ^ CD
  | AluN AluOp            -- ^ C6 CE ... FE
  | Rst (Index 8)         -- ^ C7 CF ... FF: call 8 × n
  | Ill Illegal           -- ^ the unused opcodes
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFDataX

data CbInstr
  = Rot RotOp R8          -- ^ CB 00-3F
  | Bit (Index 8) R8      -- ^ CB 40-7F
  | Res (Index 8) R8      -- ^ CB 80-BF
  | Set (Index 8) R8      -- ^ CB C0-FF
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFDataX
