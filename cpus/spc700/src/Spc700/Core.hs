{-# OPTIONS_GHC -fconstraint-solver-iterations=0 #-}
{-# LANGUAGE StrictData #-}
{-|
The SPC700 in Clash (PLAN.md step 7.8): a pure function per bus cycle
('cpuCycle'), following the reference model ("Spc700.Spec") cycle for
cycle; the tests run SingleStepTests on it and compare it with the spec
on random programs, bus cycle by bus cycle.

__The cycle.__ 'cpuCycle' runs at the end of each bus cycle with what its
read returned; the request for the next cycle is a function of the state
('cpuReq'), so the caller can register it. At the end of an instruction
(or of a halted CPU's two cycles) it says so ('coStep'). The sound module
stalls the CPU by not calling it.

__Programs.__ An instruction is its opcode fetch, then a program of at
most 11 micro-ops ('U'), each one bus cycle ('progOf'): operand fetches,
reads and writes at an address made by one adder ('addrOf': a base, X or
Y, plus 1, wrapped within the direct page or not), pushes and pulls,
internal cycles. A taken branch's two internal cycles are micro-ops with
a condition ('UTaken'), the last of a program. The micro-ops only latch
what they read (the operand bytes 'cO0', 'cO1', the data bytes 'cD0',
'cD1') and move PC and SP; the instruction's effect on the registers
comes at the end ('finish'), from the latched bytes, and a write's data
from the same bytes ('wdata'). DIV divides in the internal cycles, a bit
each (nine), into 'cDiv'; the result equals the spec's formula for all
2^24 inputs.
-}
module Spc700.Core
  ( -- * Bus
    Req (..)
  , CpuOut (..)
    -- * State
  , Phase (..)
  , Cpu (..)
  , CpuRegs (..)
  , Base (..)
  , Idx (..)
  , ASrc (..)
  , Dst (..)
  , WSrc (..)
  , U (..)
  , cpuPowerOn
  , cpuInit
  , cpuRegs
  , cpuReq
  , cpuCycle
  , progOf
  , divBits
  ) where

import Clash.Prelude

-- * Bus

-- | A bus cycle: a read, a write, or an internal cycle (no address).
data Req
  = RRead (Unsigned 16)
  | RWrite (Unsigned 16) (Unsigned 8)
  | RIdle
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFDataX, BitPack)

data CpuOut = CpuOut
  { coReq  :: Req    -- ^ the request for the next cycle
  , coStep :: Bool   -- ^ this cycle ended an instruction (or a halted CPU's two cycles)
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (NFDataX, BitPack)

-- * Micro-ops

-- | An address's base: the direct page at 'cO0', at 'cO1', or at 0 (for
-- (X) and (Y)); the operand word; the operand word's 13 bits (mem.bit);
-- the pointer read into 'cD0' and 'cD1'; TCALL's and BRK's vector; the
-- stack (pull: SP + 1).
data Base = BDp | BDp1 | BDp0 | BAbs | BAbs13 | BPtr | BVec | BPush | BPull
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFDataX, BitPack)

data Idx = INone | IX | IY
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFDataX, BitPack)

-- | Base, index, plus one.
data ASrc = ASrc Base Idx Bool
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFDataX, BitPack)

data Dst = DNone | DO0 | DO1 | DD0 | DD1
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFDataX, BitPack)

-- | A write's data: a register, PC's halves, a latched byte, or the
-- instruction's result ('wdata').
data WSrc = WA | WX | WY | WPsw | WPch | WPcl | WD0 | WO0 | WRes | WResHi
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFDataX, BitPack)

data U
  = UNone               -- ^ the program's end
  | UFetch Dst          -- ^ an operand byte at PC (PC + 1)
  | UDummy              -- ^ a read at PC, which stays
  | URead ASrc Dst
  | UWrite ASrc WSrc
  | UAluW ASrc          -- ^ the ALU's result written, or (CMP) an internal cycle
  | UPush WSrc          -- ^ a write at 0100h + SP, SP - 1
  | UPull Dst           -- ^ SP + 1, a read at 0100h + SP
  | UIdle
  | UTaken              -- ^ an internal cycle if the branch is taken
  | UDivInit            -- ^ DIV: a read at PC, YA into 'cDiv'
  | UDiv                -- ^ DIV: an internal cycle, a bit of the quotient
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFDataX, BitPack)

-- * State

data Phase = PFetch | PExec | PHalt0 | PHalt1
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFDataX, BitPack)

data Cpu = Cpu
  { cA, cX, cY, cSP, cPSW :: Unsigned 8
  , cPC    :: Unsigned 16
  , cPhase :: Phase
  , cOp    :: Unsigned 8
  , cStep  :: Index 11
  , cO0, cO1, cD0, cD1 :: Unsigned 8
  , cDiv   :: Unsigned 17
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (NFDataX, BitPack)

data CpuRegs = CpuRegs
  { crA, crX, crY, crSP :: Unsigned 8
  , crPC  :: Unsigned 16
  , crPSW :: Unsigned 8
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (NFDataX, BitPack)

-- | The registers given, the opcode fetch next.
cpuPowerOn :: CpuRegs -> Cpu
cpuPowerOn r = Cpu (crA r) (crX r) (crY r) (crSP r) (crPSW r) (crPC r) PFetch 0 0 0 0 0 0 0

-- | Registers zero, PC at the boot ROM (FFC0h): a literal, for the
-- register's reset value.
cpuInit :: Cpu
cpuInit = Cpu 0 0 0 0 0 0xFFC0 PFetch 0 0 0 0 0 0 0

cpuRegs :: Cpu -> CpuRegs
cpuRegs c = CpuRegs (cA c) (cX c) (cY c) (cSP c) (cPC c) (cPSW c)

-- * Programs

dp, dp1, dpX, dpX1, dpY, dpIx, dpIy, dpO1, abs', absX, absX1, absY, abs13, ptr, ptrY, vec, vec1 :: ASrc
dp = ASrc BDp INone False
dp1 = ASrc BDp INone True
dpX = ASrc BDp IX False
dpX1 = ASrc BDp IX True
dpY = ASrc BDp IY False
dpIx = ASrc BDp0 IX False
dpIy = ASrc BDp0 IY False
dpO1 = ASrc BDp1 INone False
abs' = ASrc BAbs INone False
absX = ASrc BAbs IX False
absX1 = ASrc BAbs IX True
absY = ASrc BAbs IY False
abs13 = ASrc BAbs13 INone False
ptr = ASrc BPtr INone False
ptrY = ASrc BPtr IY False
vec = ASrc BVec INone False
vec1 = ASrc BVec INone True

-- | A program from its micro-ops (at most 11), the rest 'UNone'.
p :: KnownNat n => Vec n U -> Vec 11 U
p v = takeI (v ++ (repeat UNone :: Vec 11 U))

fo0, fo1, fd0, fd1 :: U
fo0 = UFetch DO0
fo1 = UFetch DO1
fd0 = UFetch DD0
fd1 = UFetch DD1

-- | Each opcode's micro-ops after its fetch.
progOf :: Unsigned 8 -> Vec 11 U
progOf op
  | aluRow && lo >= 4 && lo <= 9 = if even hi then aluA lo else aluB lo
  | lo == 1 = p (UDummy :> UIdle :> UPush WPch :> UPush WPcl :> UIdle :> URead vec DD0 :> URead vec1 DD1 :> Nil)
  | lo == 2 = p (fo0 :> URead dp DD0 :> UWrite dp WRes :> Nil)
  | lo == 3 = p (fo0 :> URead dp DD0 :> UIdle :> fo1 :> UTaken :> UTaken :> Nil)
  | otherwise = other
 where
  hi = op `shiftR` 4
  lo = op .&. 0xF
  aluRow = hi <= 0xB
  -- A with dp, abs, (X), [dp+X], #imm; dp with dp
  aluA m = case m of
    4 -> p (fo0 :> URead dp DD0 :> Nil)
    5 -> p (fo0 :> fo1 :> URead abs' DD0 :> Nil)
    6 -> p (UDummy :> URead dpIx DD0 :> Nil)
    7 -> p (fo0 :> UIdle :> URead dpX DD0 :> URead dpX1 DD1 :> URead ptr DD0 :> Nil)
    8 -> p (fd0 :> Nil)
    _ -> p (fo0 :> URead dp DD1 :> fo1 :> URead dpO1 DD0 :> UAluW dpO1 :> Nil)
  -- A with dp+X, abs+X, abs+Y, [dp]+Y; dp with #imm; (X) with (Y)
  aluB m = case m of
    4 -> p (fo0 :> UIdle :> URead dpX DD0 :> Nil)
    5 -> p (fo0 :> fo1 :> UIdle :> URead absX DD0 :> Nil)
    6 -> p (fo0 :> fo1 :> UIdle :> URead absY DD0 :> Nil)
    7 -> p (fo0 :> UIdle :> URead dp DD0 :> URead dp1 DD1 :> URead ptrY DD0 :> Nil)
    8 -> p (fd1 :> fo0 :> URead dp DD0 :> UAluW dp :> Nil)
    _ -> p (UDummy :> URead dpIy DD1 :> URead dpIx DD0 :> UAluW dpIx :> Nil)
  branch = p (fo0 :> UTaken :> UTaken :> Nil)
  other = case op of
    0x00 -> p (UDummy :> Nil)
    0x10 -> branch
    0x30 -> branch
    0x50 -> branch
    0x70 -> branch
    0x90 -> branch
    0xB0 -> branch
    0xD0 -> branch
    0xF0 -> branch
    0x2F -> branch
    0x20 -> p (UDummy :> Nil)
    0x40 -> p (UDummy :> Nil)
    0x60 -> p (UDummy :> Nil)
    0x80 -> p (UDummy :> Nil)
    0xA0 -> p (UDummy :> UIdle :> Nil)
    0xC0 -> p (UDummy :> UIdle :> Nil)
    0xE0 -> p (UDummy :> Nil)
    0xED -> p (UDummy :> UIdle :> Nil)
    -- mem.bit: OR1 and EOR1 have an internal cycle more
    0x0A -> p (fo0 :> fo1 :> URead abs13 DD0 :> UIdle :> Nil)
    0x2A -> p (fo0 :> fo1 :> URead abs13 DD0 :> UIdle :> Nil)
    0x4A -> p (fo0 :> fo1 :> URead abs13 DD0 :> Nil)
    0x6A -> p (fo0 :> fo1 :> URead abs13 DD0 :> Nil)
    0x8A -> p (fo0 :> fo1 :> URead abs13 DD0 :> UIdle :> Nil)
    0xAA -> p (fo0 :> fo1 :> URead abs13 DD0 :> Nil)
    0xCA -> p (fo0 :> fo1 :> URead abs13 DD0 :> UIdle :> UWrite abs13 WRes :> Nil)
    0xEA -> p (fo0 :> fo1 :> URead abs13 DD0 :> UWrite abs13 WRes :> Nil)
    -- 16-bit
    0x1A -> incw
    0x3A -> incw
    0x5A -> p (fo0 :> URead dp DD0 :> URead dp1 DD1 :> Nil)
    0x7A -> p (fo0 :> URead dp DD0 :> UIdle :> URead dp1 DD1 :> Nil)
    0x9A -> p (fo0 :> URead dp DD0 :> UIdle :> URead dp1 DD1 :> Nil)
    0xBA -> p (fo0 :> URead dp DD0 :> UIdle :> URead dp1 DD1 :> Nil)
    0xDA -> p (fo0 :> URead dp DNone :> UWrite dp WA :> UWrite dp1 WY :> Nil)
    -- MUL, DIV, DAA, DAS, XCN
    0xCF -> p (UDummy :> UIdle :> UIdle :> UIdle :> UIdle :> UIdle :> UIdle :> UIdle :> Nil)
    0x9E -> p (UDivInit :> UDiv :> UDiv :> UDiv :> UDiv :> UDiv :> UDiv :> UDiv :> UDiv :> UDiv :> UIdle :> Nil)
    0xDF -> p (UDummy :> UIdle :> Nil)
    0xBE -> p (UDummy :> UIdle :> Nil)
    0x9F -> p (UDummy :> UIdle :> UIdle :> UIdle :> Nil)
    -- TSET1, TCLR1
    0x0E -> tset
    0x4E -> tset
    -- compares with X and Y
    0x1E -> p (fo0 :> fo1 :> URead abs' DD0 :> Nil)
    0x3E -> p (fo0 :> URead dp DD0 :> Nil)
    0xC8 -> p (fd0 :> Nil)
    0x5E -> p (fo0 :> fo1 :> URead abs' DD0 :> Nil)
    0x7E -> p (fo0 :> URead dp DD0 :> Nil)
    0xAD -> p (fd0 :> Nil)
    -- CBNE, DBNZ
    0x2E -> p (fo0 :> URead dp DD0 :> UIdle :> fo1 :> UTaken :> UTaken :> Nil)
    0xDE -> p (fo0 :> UIdle :> URead dpX DD0 :> UIdle :> fo1 :> UTaken :> UTaken :> Nil)
    0x6E -> p (fo0 :> URead dp DD0 :> UWrite dp WRes :> fo1 :> UTaken :> UTaken :> Nil)
    0xFE -> p (UDummy :> UIdle :> fo0 :> UTaken :> UTaken :> Nil)
    -- stack
    0x0D -> p (UDummy :> UPush WPsw :> UIdle :> Nil)
    0x2D -> p (UDummy :> UPush WA :> UIdle :> Nil)
    0x4D -> p (UDummy :> UPush WX :> UIdle :> Nil)
    0x6D -> p (UDummy :> UPush WY :> UIdle :> Nil)
    0x8E -> p (UDummy :> UIdle :> UPull DD0 :> Nil)
    0xAE -> p (UDummy :> UIdle :> UPull DD0 :> Nil)
    0xCE -> p (UDummy :> UIdle :> UPull DD0 :> Nil)
    0xEE -> p (UDummy :> UIdle :> UPull DD0 :> Nil)
    -- jumps, calls, returns
    0x5F -> p (fo0 :> fo1 :> Nil)
    0x1F -> p (fo0 :> fo1 :> UIdle :> URead absX DD0 :> URead absX1 DD1 :> Nil)
    0x3F -> p (fo0 :> fo1 :> UIdle :> UPush WPch :> UPush WPcl :> UIdle :> UIdle :> Nil)
    0x4F -> p (fo0 :> UIdle :> UPush WPch :> UPush WPcl :> UIdle :> Nil)
    0x6F -> p (UDummy :> UIdle :> UPull DD0 :> UPull DD1 :> Nil)
    0x7F -> p (UDummy :> UIdle :> UPull DO0 :> UPull DD0 :> UPull DD1 :> Nil)
    0x0F -> p (UDummy :> UPush WPch :> UPush WPcl :> UPush WPsw :> UIdle :> URead vec DD0 :> URead vec1 DD1 :> Nil)
    -- register moves
    0x5D -> p (UDummy :> Nil)
    0x7D -> p (UDummy :> Nil)
    0x9D -> p (UDummy :> Nil)
    0xBD -> p (UDummy :> Nil)
    0xDD -> p (UDummy :> Nil)
    0xFD -> p (UDummy :> Nil)
    -- loads
    0xE8 -> p (fd0 :> Nil)
    0xCD -> p (fd0 :> Nil)
    0x8D -> p (fd0 :> Nil)
    0xE4 -> p (fo0 :> URead dp DD0 :> Nil)
    0xE5 -> p (fo0 :> fo1 :> URead abs' DD0 :> Nil)
    0xE6 -> p (UDummy :> URead dpIx DD0 :> Nil)
    0xE7 -> p (fo0 :> UIdle :> URead dpX DD0 :> URead dpX1 DD1 :> URead ptr DD0 :> Nil)
    0xF4 -> p (fo0 :> UIdle :> URead dpX DD0 :> Nil)
    0xF5 -> p (fo0 :> fo1 :> UIdle :> URead absX DD0 :> Nil)
    0xF6 -> p (fo0 :> fo1 :> UIdle :> URead absY DD0 :> Nil)
    0xF7 -> p (fo0 :> UIdle :> URead dp DD0 :> URead dp1 DD1 :> URead ptrY DD0 :> Nil)
    0xBF -> p (UDummy :> URead dpIx DD0 :> UIdle :> Nil)
    0xF8 -> p (fo0 :> URead dp DD0 :> Nil)
    0xF9 -> p (fo0 :> UIdle :> URead dpY DD0 :> Nil)
    0xE9 -> p (fo0 :> fo1 :> URead abs' DD0 :> Nil)
    0xEB -> p (fo0 :> URead dp DD0 :> Nil)
    0xFB -> p (fo0 :> UIdle :> URead dpX DD0 :> Nil)
    0xEC -> p (fo0 :> fo1 :> URead abs' DD0 :> Nil)
    -- stores: each reads its destination first (but MOV (X)+,A and MOV dp,dp)
    0xC4 -> p (fo0 :> URead dp DNone :> UWrite dp WA :> Nil)
    0xC5 -> p (fo0 :> fo1 :> URead abs' DNone :> UWrite abs' WA :> Nil)
    0xC6 -> p (UDummy :> URead dpIx DNone :> UWrite dpIx WA :> Nil)
    0xC7 -> p (fo0 :> UIdle :> URead dpX DD0 :> URead dpX1 DD1 :> URead ptr DNone :> UWrite ptr WA :> Nil)
    0xD4 -> p (fo0 :> UIdle :> URead dpX DNone :> UWrite dpX WA :> Nil)
    0xD5 -> p (fo0 :> fo1 :> UIdle :> URead absX DNone :> UWrite absX WA :> Nil)
    0xD6 -> p (fo0 :> fo1 :> UIdle :> URead absY DNone :> UWrite absY WA :> Nil)
    0xD7 -> p (fo0 :> URead dp DD0 :> URead dp1 DD1 :> UIdle :> URead ptrY DNone :> UWrite ptrY WA :> Nil)
    0xAF -> p (UDummy :> UIdle :> UWrite dpIx WA :> Nil)
    0xD8 -> p (fo0 :> URead dp DNone :> UWrite dp WX :> Nil)
    0xD9 -> p (fo0 :> UIdle :> URead dpY DNone :> UWrite dpY WX :> Nil)
    0xC9 -> p (fo0 :> fo1 :> URead abs' DNone :> UWrite abs' WX :> Nil)
    0xCB -> p (fo0 :> URead dp DNone :> UWrite dp WY :> Nil)
    0xDB -> p (fo0 :> UIdle :> URead dpX DNone :> UWrite dpX WY :> Nil)
    0xCC -> p (fo0 :> fo1 :> URead abs' DNone :> UWrite abs' WY :> Nil)
    0xFA -> p (fo0 :> URead dp DD0 :> fo1 :> UWrite dpO1 WD0 :> Nil)
    0x8F -> p (fo0 :> fo1 :> URead dpO1 DNone :> UWrite dpO1 WO0 :> Nil)
    -- SLEEP, STOP
    0xEF -> p (UDummy :> UIdle :> Nil)
    0xFF -> p (UDummy :> UIdle :> Nil)
    -- read-modify-write: dp, dp+X, abs, and the registers
    _ | lo == 0xB && even hi -> p (fo0 :> URead dp DD0 :> UWrite dp WRes :> Nil)
      | lo == 0xB -> p (fo0 :> UIdle :> URead dpX DD0 :> UWrite dpX WRes :> Nil)
      | lo == 0xC && even hi -> p (fo0 :> fo1 :> URead abs' DD0 :> UWrite abs' WRes :> Nil)
      | otherwise -> p (UDummy :> Nil)
  incw = p (fo0 :> URead dp DD0 :> UWrite dp WRes :> URead dp1 DD1 :> UWrite dp1 WResHi :> Nil)
  tset = p (fo0 :> fo1 :> URead abs' DD0 :> URead abs' DNone :> UWrite abs' WRes :> Nil)

-- * The datapath

-- | The direct page's high byte.
page :: Cpu -> Unsigned 16
page c = if testBit (cPSW c) 5 then 0x100 else 0

-- | One adder for every address.
addrOf :: Cpu -> ASrc -> Unsigned 16
addrOf c (ASrc b i k) =
  let ix = case i of
        INone -> 0
        IX -> cX c
        IY -> cY c
      one = if k then 1 else 0 :: Unsigned 8
      inPage o = page c .|. resize (o + ix + one)
      word = (resize (cO1 c) `shiftL` 8) .|. resize (cO0 c) :: Unsigned 16
      wide w = w + resize ix + resize one
  in case b of
       BDp -> inPage (cO0 c)
       BDp1 -> inPage (cO1 c)
       BDp0 -> inPage 0
       BAbs -> wide word
       BAbs13 -> word .&. 0x1FFF
       BPtr -> wide ((resize (cD1 c) `shiftL` 8) .|. resize (cD0 c))
       BVec -> 0xFFDE - (resize (cOp c `shiftR` 4) `shiftL` 1) + resize one
       BPush -> 0x100 .|. resize (cSP c)
       BPull -> 0x100 .|. resize (cSP c + 1)

data AluOp = AOr | AAnd | AEor | ACmp | AAdc | ASbc
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFDataX, BitPack)

-- | The ALU row of an opcode (OR, AND, EOR, CMP, ADC, SBC).
rowOp :: Unsigned 8 -> AluOp
rowOp op = case op `shiftR` 5 of
  0 -> AOr
  1 -> AAnd
  2 -> AEor
  3 -> ACmp
  4 -> AAdc
  _ -> ASbc

flagN, flagV, flagP, flagB, flagH, flagI, flagZ, flagC :: Int
flagN = 7
flagV = 6
flagP = 5
flagB = 4
flagH = 3
flagI = 2
flagZ = 1
flagC = 0

setF :: Int -> Bool -> Unsigned 8 -> Unsigned 8
setF k v ps = if v then setBit ps k else clearBit ps k

nz :: Unsigned 8 -> Unsigned 8 -> Unsigned 8
nz v = setF flagN (testBit v 7) . setF flagZ (v == 0)

-- | ADC of a and v with the carry in: the result and the flags.
adc :: Unsigned 8 -> Unsigned 8 -> Unsigned 8 -> (Unsigned 8, Unsigned 8)
adc ps a v =
  let r = resize a + resize v + (if testBit ps flagC then 1 else 0) :: Unsigned 9
      r8 = resize r :: Unsigned 8
      ps' = setF flagC (testBit r 8)
          . setF flagH (testBit (a `xor` v `xor` r8) 4)
          . setF flagV (testBit (complement (a `xor` v) .&. (a `xor` r8)) 7)
          $ nz r8 ps
  in (r8, ps')

alu :: AluOp -> Unsigned 8 -> Unsigned 8 -> Unsigned 8 -> (Unsigned 8, Unsigned 8)
alu f ps a v = case f of
  AOr -> let r = a .|. v in (r, nz r ps)
  AAnd -> let r = a .&. v in (r, nz r ps)
  AEor -> let r = a `xor` v in (r, nz r ps)
  ACmp -> (a, cmp ps a v)
  AAdc -> adc ps a v
  ASbc -> adc ps a (complement v)

cmp :: Unsigned 8 -> Unsigned 8 -> Unsigned 8 -> Unsigned 8
cmp ps a v = setF flagC (a >= v) (nz (a - v) ps)

-- | The shifts and INC/DEC of the read-modify-write rows (by the
-- opcode's high nibble, bit 0 ignored): the result and the flags.
shiftOp :: Unsigned 8 -> Unsigned 8 -> Unsigned 8 -> (Unsigned 8, Unsigned 8)
shiftOp op ps v =
  let ci = if testBit ps flagC then 1 else 0 :: Unsigned 8
      (r, cf) = case (op `shiftR` 5) .&. 7 of
        0 -> (v `shiftL` 1, Just (testBit v 7))
        1 -> ((v `shiftL` 1) .|. ci, Just (testBit v 7))
        2 -> (v `shiftR` 1, Just (testBit v 0))
        3 -> ((v `shiftR` 1) .|. (ci `shiftL` 7), Just (testBit v 0))
        4 -> (v - 1, Nothing)
        _ -> (v + 1, Nothing)
  in (r, nz r (maybe ps (\b -> setF flagC b ps) cf))

-- | The bit number of SET1/CLR1/BBS/BBC (in the opcode) and of mem.bit
-- (in the operand's top three bits).
opBit, memBit :: Cpu -> Index 8
opBit c = unpack (resize (pack (cOp c `shiftR` 5)))
memBit c = unpack (resize (pack (cO1 c `shiftR` 5)))

-- | The latched data word (dp's low and high byte) and its +1 or -1.
word0 :: Cpu -> Unsigned 16
word0 c = (resize (cD1 c) `shiftL` 8) .|. resize (cD0 c)

incwWord :: Cpu -> Unsigned 16
incwWord c = if cOp c == 0x3A then word0 c + 1 else word0 c - 1

-- | A write's data.
wdata :: Cpu -> WSrc -> Unsigned 8
wdata c w = case w of
  WA -> cA c
  WX -> cX c
  WY -> cY c
  WPsw -> cPSW c
  WPch -> resize (cPC c `shiftR` 8)
  WPcl -> resize (cPC c)
  WD0 -> cD0 c
  WO0 -> cO0 c
  WRes -> result c
  WResHi -> resize (incwWord c `shiftR` 8)

-- | The byte a read-modify-write writes back.
result :: Cpu -> Unsigned 8
result c =
  let op = cOp c
      lo = op .&. 0xF
      d = cD0 c
  in case op of
       0xCA -> if testBit (cPSW c) flagC then setBit d (fromEnum (memBit c)) else clearBit d (fromEnum (memBit c))
       0xEA -> complementBit d (fromEnum (memBit c))
       0x0E -> d .|. cA c
       0x4E -> d .&. complement (cA c)
       0x6E -> d - 1
       0x1A -> d - 1
       0x3A -> d + 1
       _ | lo == 2 -> if even (op `shiftR` 4) then setBit d (fromEnum (opBit c)) else clearBit d (fromEnum (opBit c))
         | otherwise -> fst (shiftOp op (cPSW c) d)

-- | The ALU's operands of the memory forms (dp,dp; dp,#imm; (X),(Y)):
-- the destination and the source.
aluMem :: Cpu -> (Unsigned 8, Unsigned 8)
aluMem c = alu (rowOp (cOp c)) (cPSW c) (cD0 c) (cD1 c)

-- | Whether the branch is taken.
taken :: Cpu -> Bool
taken c =
  let op = cOp c
      ps = cPSW c
      f k = testBit ps k
  in case op of
       0x10 -> not (f flagN)
       0x30 -> f flagN
       0x50 -> not (f flagV)
       0x70 -> f flagV
       0x90 -> not (f flagC)
       0xB0 -> f flagC
       0xD0 -> not (f flagZ)
       0xF0 -> f flagZ
       0x2F -> True
       0x2E -> cA c /= cD0 c
       0xDE -> cA c /= cD0 c
       0x6E -> cD0 c /= 1
       0xFE -> cY c /= 1
       _ -> testBit (cD0 c) (fromEnum (opBit c)) == even (op `shiftR` 4)

-- | The relative operand's byte.
rel :: Cpu -> Unsigned 8
rel c = case cOp c of
  0xFE -> cO0 c
  op | op .&. 0x1F == 0x10 || op == 0x2F -> cO0 c
     | otherwise -> cO1 c

-- | The request of the next cycle.
cpuReq :: Cpu -> Req
cpuReq c = case cPhase c of
  PFetch -> RRead (cPC c)
  PHalt0 -> RRead (cPC c)
  PHalt1 -> RIdle
  PExec -> case progOf (cOp c) !! cStep c of
    UFetch _ -> RRead (cPC c)
    UDummy -> RRead (cPC c)
    UDivInit -> RRead (cPC c)
    URead a _ -> RRead (addrOf c a)
    UWrite a w -> RWrite (addrOf c a) (wdata c w)
    UAluW a -> if rowOp (cOp c) == ACmp then RIdle else RWrite (addrOf c a) (fst (aluMem c))
    UPush w -> RWrite (addrOf c (ASrc BPush INone False)) (wdata c w)
    UPull _ -> RRead (addrOf c (ASrc BPull INone False))
    _ -> RIdle

-- | A cycle's end with what its read returned: the state for the next
-- cycle, and whether an instruction ended.
cpuCycle :: Cpu -> Unsigned 8 -> (Cpu, CpuOut)
cpuCycle c din = (c', CpuOut (cpuReq c') ended)
 where
  (c', ended) = case cPhase c of
    PFetch -> (c { cOp = din, cPC = cPC c + 1, cPhase = PExec, cStep = 0 }, False)
    PHalt0 -> (c { cPhase = PHalt1 }, False)
    PHalt1 -> (c { cPhase = PHalt0 }, True)
    PExec ->
      let prog = progOf (cOp c)
          u = prog !! cStep c
          c1 = latch c u
          more = cStep c < maxBound && active c1 (prog !! (cStep c + 1))
      in if more then (c1 { cStep = cStep c + 1 }, False) else (finish c1, True)
  latch s u = case u of
    UFetch d -> put d s { cPC = cPC s + 1 }
    URead _ d -> put d s
    UPush _ -> s { cSP = cSP s - 1 }
    UPull d -> put d s { cSP = cSP s + 1 }
    UDivInit -> s { cDiv = (resize (cY s) `shiftL` 8) .|. resize (cA s) }
    UDiv -> s { cDiv = divStep (resize (cX s) `shiftL` 9) (cDiv s) }
    _ -> s
   where
    put d s' = case d of
      DNone -> s'
      DO0 -> s' { cO0 = din }
      DO1 -> s' { cO1 = din }
      DD0 -> s' { cD0 = din }
      DD1 -> s' { cD1 = din }
  active s u = case u of
    UNone -> False
    UTaken -> taken s
    _ -> True

-- | DIV's nine bits from YA and X: the quotient in bits 0-7, V in bit 8,
-- the remainder in bits 9-16 (the core does a bit an internal cycle).
divBits :: Unsigned 16 -> Unsigned 8 -> Unsigned 17
divBits ya x = foldl (\w _ -> divStep (resize x `shiftL` 9) w) (resize ya) (repeat () :: Vec 9 ())

-- | DIV's bit: the hardware's shift and subtract (anomie, blargg).
divStep :: Unsigned 17 -> Unsigned 17 -> Unsigned 17
divStep xs w0 =
  let w1 = rotateL w0 1
      w2 = if w1 >= xs then w1 `xor` 1 else w1
  in if testBit w2 0 then w2 - xs else w2

-- | The instruction's effect, at its end; the next phase.
finish :: Cpu -> Cpu
finish c0 = fx c0 { cPhase = PFetch }
 where
  op = cOp c0
  hi = op `shiftR` 4
  lo = op .&. 0xF
  ps = cPSW c0
  dd0 = cD0 c0
  setA v c = c { cA = v, cPSW = nz v (cPSW c) }
  setX v c = c { cX = v, cPSW = nz v (cPSW c) }
  setY v c = c { cY = v, cPSW = nz v (cPSW c) }
  pc16 c = (resize (cD1 c) `shiftL` 8) .|. resize (cD0 c)
  brn c = if taken c then c { cPC = cPC c + bitCoerce (signExtend (bitCoerce (rel c) :: Signed 8) :: Signed 16) } else c
  cmpWith r c = c { cPSW = cmp ps r dd0 }
  word = word0 c0
  ya = (resize (cY c0) `shiftL` 8) .|. resize (cA c0) :: Unsigned 16
  fx c
    | hi <= 0xB && lo >= 4 && lo <= 9 =
        let f = rowOp op
            memForm = (even hi && lo == 9) || (odd hi && lo >= 8)
        in if memForm then c { cPSW = snd (aluMem c) }
           else let (r, ps') = alu f ps (cA c) dd0
                in c { cA = if f == ACmp then cA c else r, cPSW = ps' }
    | lo == 1 || op == 0x1F || op == 0x6F = c { cPC = pc16 c }
    | lo == 2 = c
    | lo == 3 = brn c
    | otherwise = case op of
        0x00 -> c
        _ | op .&. 0x1F == 0x10 || op == 0x2F -> brn c
        0x20 -> c { cPSW = clearBit ps flagP }
        0x40 -> c { cPSW = setBit ps flagP }
        0x60 -> c { cPSW = clearBit ps flagC }
        0x80 -> c { cPSW = setBit ps flagC }
        0xA0 -> c { cPSW = setBit ps flagI }
        0xC0 -> c { cPSW = clearBit ps flagI }
        0xE0 -> c { cPSW = clearBit (clearBit ps flagV) flagH }
        0xED -> c { cPSW = complementBit ps flagC }
        0x0A -> c { cPSW = setF flagC (testBit ps flagC || bitM) ps }
        0x2A -> c { cPSW = setF flagC (testBit ps flagC || not bitM) ps }
        0x4A -> c { cPSW = setF flagC (testBit ps flagC && bitM) ps }
        0x6A -> c { cPSW = setF flagC (testBit ps flagC && not bitM) ps }
        0x8A -> c { cPSW = setF flagC (testBit ps flagC /= bitM) ps }
        0xAA -> c { cPSW = setF flagC bitM ps }
        0xCA -> c
        0xEA -> c
        0x1A -> c { cPSW = word16 (incwWord c) ps }
        0x3A -> c { cPSW = word16 (incwWord c) ps }
        0x5A -> c { cPSW = setF flagC (ya >= word) (word16 (ya - word) ps) }
        0x7A -> addw False c
        0x9A -> addw True c
        0xBA -> c { cA = dd0, cY = cD1 c, cPSW = setF flagN (testBit (cD1 c) 7) (setF flagZ (word == 0) ps) }
        0xDA -> c
        0xCF -> let r = resize (cA c) * resize (cY c) :: Unsigned 16
                    rh = resize (r `shiftR` 8)
                in c { cA = resize r, cY = rh, cPSW = nz rh ps }
        0x9E -> let w = cDiv c
                    q = resize w :: Unsigned 8
                    ps1 = setF flagV (testBit w 8) (setF flagH ((cY c .&. 0xF) >= (cX c .&. 0xF)) ps)
                in c { cA = q, cY = resize (w `shiftR` 9), cPSW = nz q ps1 }
        0xDF -> let a = cA c
                    cf = testBit ps flagC || a > 0x99
                    a1 = if cf then a + 0x60 else a
                    a2 = if testBit ps flagH || (a1 .&. 0xF) > 9 then a1 + 6 else a1
                in c { cA = a2, cPSW = nz a2 (setF flagC cf ps) }
        0xBE -> let a = cA c
                    borrow = not (testBit ps flagC) || a > 0x99
                    a1 = if borrow then a - 0x60 else a
                    a2 = if not (testBit ps flagH) || (a1 .&. 0xF) > 9 then a1 - 6 else a1
                in c { cA = a2, cPSW = nz a2 (setF flagC (not borrow) ps) }
        0x9F -> setA (rotateL (cA c) 4) c
        0x0E -> c { cPSW = setF flagN (testBit (cA c - dd0) 7) (setF flagZ (cA c == dd0) ps) }
        0x4E -> c { cPSW = setF flagN (testBit (cA c - dd0) 7) (setF flagZ (cA c == dd0) ps) }
        0x1E -> cmpWith (cX c) c
        0x3E -> cmpWith (cX c) c
        0xC8 -> cmpWith (cX c) c
        0x5E -> cmpWith (cY c) c
        0x7E -> cmpWith (cY c) c
        0xAD -> cmpWith (cY c) c
        0x2E -> brn c
        0xDE -> brn c
        0x6E -> brn c
        0xFE -> (brn c) { cY = cY c - 1 }
        0x0D -> c
        0x2D -> c
        0x4D -> c
        0x6D -> c
        0x8E -> c { cPSW = dd0 }
        0xAE -> c { cA = dd0 }
        0xCE -> c { cX = dd0 }
        0xEE -> c { cY = dd0 }
        0x5F -> c { cPC = (resize (cO1 c) `shiftL` 8) .|. resize (cO0 c) }
        0x3F -> c { cPC = (resize (cO1 c) `shiftL` 8) .|. resize (cO0 c) }
        0x4F -> c { cPC = 0xFF00 .|. resize (cO0 c) }
        0x7F -> c { cPSW = cO0 c, cPC = pc16 c }
        0x0F -> c { cPC = pc16 c, cPSW = clearBit (setBit ps flagB) flagI }
        0x5D -> setX (cA c) c
        0x7D -> setA (cX c) c
        0x9D -> setX (cSP c) c
        0xBD -> c { cSP = cX c }
        0xDD -> setA (cY c) c
        0xFD -> setY (cA c) c
        0xE8 -> setA dd0 c
        0xCD -> setX dd0 c
        0x8D -> setY dd0 c
        0xE4 -> setA dd0 c
        0xE5 -> setA dd0 c
        0xE6 -> setA dd0 c
        0xE7 -> setA dd0 c
        0xF4 -> setA dd0 c
        0xF5 -> setA dd0 c
        0xF6 -> setA dd0 c
        0xF7 -> setA dd0 c
        0xBF -> setA dd0 c { cX = cX c + 1 }
        0xF8 -> setX dd0 c
        0xF9 -> setX dd0 c
        0xE9 -> setX dd0 c
        0xEB -> setY dd0 c
        0xFB -> setY dd0 c
        0xEC -> setY dd0 c
        0xAF -> c { cX = cX c + 1 }
        0xCB -> c
        0xDB -> c
        0xCC -> c
        0xEF -> c { cPhase = PHalt0 }
        0xFF -> c { cPhase = PHalt0 }
        _ | lo == 0xB || (lo == 0xC && even hi) -> c { cPSW = snd (shiftOp op ps dd0) }
          | lo == 0xC || lo == 0xD ->
              -- A, X and Y: ASL A ... INC A (1C-BC), DEC/INC X (1D, 3D), DEC/INC Y (DC, FC)
              let (src, put) = case op of
                    0x1D -> (cX c, \v s -> s { cX = v })
                    0x3D -> (cX c, \v s -> s { cX = v })
                    0xDC -> (cY c, \v s -> s { cY = v })
                    0xFC -> (cY c, \v s -> s { cY = v })
                    _ -> (cA c, \v s -> s { cA = v })
                  sop = case op of
                    0x1D -> 0x80
                    0x3D -> 0xA0
                    0xDC -> 0x80
                    0xFC -> 0xA0
                    _ -> op
                  (r, ps') = shiftOp sop ps src
              in (put r c) { cPSW = ps' }
          | otherwise -> c
  bitM = testBit dd0 (fromEnum (memBit c0))
  word16 w = setF flagN (testBit w 15) . setF flagZ (w == 0)
  -- ADDW / SUBW YA,dp: two 8-bit additions (C cleared or set first), Z
  -- for the word
  addw isSub c =
    let ps1 = setF flagC isSub ps
        (rl, ps2) = if isSub then adc ps1 (cA c) (complement dd0) else adc ps1 (cA c) dd0
        (rh, ps3) = if isSub then adc ps2 (cY c) (complement (cD1 c)) else adc ps2 (cY c) (cD1 c)
    in c { cA = rl, cY = rh, cPSW = setF flagZ (rl == 0 && rh == 0) ps3 }
