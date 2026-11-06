{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StrictData #-}
{-|
The 68000 core's bus and sequencer enumerations (side track P), shared by
"M68k.Datapath", "M68k.Micro" and "M68k.Core": a transaction's kind and
request, what ends it ('CpuIn'), an operand's place, and the phase and
continuation tags the datapath's state still carries (its faults mark
'PG0' and 'PBound').
-}
module M68k.Bus
  ( Kind (..)
  , Req (..)
  , CpuIn (..)
  , Loc (..)
  , Ph (..)
  , Ret (..)
  ) where

import Clash.Prelude

-- | What a bus transaction does.
data Kind = KRead | KWrite | KTas | KIdle | KIack
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFDataX, BitPack)

-- | One transaction: its kind, function code (FC2-FC0), width (a byte:
-- the data in the low 8 bits), address (the 24-bit bus's), data (writes),
-- and length in clocks (idle; the interrupt acknowledge: the level).
data Req = Req
  { rqKind :: Kind
  , rqFc   :: Unsigned 3
  , rqByte :: Bool
  , rqAddr :: Unsigned 24
  , rqData :: Unsigned 16
  , rqLen  :: Unsigned 8
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (NFDataX, BitPack)

-- | The end of a transaction: the data read (a read; the acknowledge: the
-- vector number in the low byte), BERR (the cycle had no device), the
-- interrupt level on IPL2-IPL0 (looked at when an instruction ends) and
-- VPA on the acknowledge (an autovector).
data CpuIn = CpuIn
  { ciData :: Unsigned 16
  , ciBerr :: Bool
  , ciIpl  :: Unsigned 3
  , ciAuto :: Bool
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (NFDataX, BitPack)

-- | Where a found operand is: a data or address register (its number in
-- 'cEa'), memory at 'cEa', (An)+ as a destination (memory at 'cEa', An
-- stepped after the write), or an immediate.
data Loc = LD | LA | LMem | LPost | LImm
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFDataX, BitPack)

-- | What the pending transaction belongs to.
data Ph
  = PPf          -- ^ the prefetch: IRC into IR, the word read into IRC
  | PSkip        -- ^ an extension word taken and not used
  | PPre         -- ^ -(An)'s idle cycles (a source)
  | PIdx         -- ^ (d8,An,Xn)'s idle cycles
  | PPcIdx       -- ^ (d8,PC,Xn)'s idle cycles
  | PDisp        -- ^ (d16,An)'s extension word
  | PIdxExt      -- ^ (d8,An,Xn)'s extension word
  | PAbsW        -- ^ (xxx).W's word
  | PAbsH        -- ^ (xxx).L's high word
  | PAbsL        -- ^ (xxx).L's low word
  | PPcDisp      -- ^ (d16,PC)'s extension word
  | PPcIdxExt    -- ^ (d8,PC,Xn)'s extension word
  | PImmW        -- ^ a byte or word immediate
  | PImmH        -- ^ a long immediate's high word
  | PImmL        -- ^ its low word
  | PRd          -- ^ an operand read from memory ('cK': which word)
  | PWr          -- ^ a result written to memory ('cK': which word)
  | PMoveAbsH    -- ^ MOVE to (xxx).L from memory: the address's high word
  | PIdleRet     -- ^ idle clocks, then 'cont' 'cRet'
  | PX           -- ^ ADDX/SUBX -(Ay),-(Ax): step 'cK'
  | PBitExt      -- ^ a static bit operation's bit number
  | PSysExt      -- ^ ORI, ANDI, EORI to CCR or SR: the word
  | PTrapv       -- ^ TRAPV taken: the prefetch's read
  | PLink        -- ^ LINK's displacement
  | PUnlk        -- ^ UNLK's reads ('cK')
  | PPush        -- ^ a long pushed ('cK')
  | PJump        -- ^ the queue filled at a jump's target ('cK')
  | PRefill      -- ^ the queue refilled ('cK')
  | PTas         -- ^ TAS's read-modify-write
  | PDbRd        -- ^ DBcc expired: the target's word read and dropped
  | PJt          -- ^ JMP and JSR's target
  | PRet         -- ^ RTS, RTR, RTE's reads ('cK')
  | PMovep       -- ^ MOVEP's displacement
  | PMovepB      -- ^ MOVEP's bytes ('cK')
  | PMovemExt    -- ^ MOVEM's mask
  | PMovem       -- ^ MOVEM's words ('cK', the register in 'cU')
  | PMovemEnd    -- ^ MOVEM from memory: the word past the last
  | PStopExt     -- ^ STOP's immediate
  | PInt         -- ^ an interrupt: step 'cK' (the level in 'cX')
  | PReset       -- ^ the reset: step 'cK'
  | PG0          -- ^ group 0 (address and bus error): step 'cK'
  | PExc         -- ^ groups 1 and 2: step 'cK'
  | PVec         -- ^ a vector taken: step 'cK'
  | PStop        -- ^ a stopped (or halted) CPU's empty transaction
  | PBound       -- ^ an instruction is over, the next not started ('cpuFinish')
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFDataX, BitPack)

-- | Where an instruction goes on when a shared step's transaction ends
-- ('cont').
data Ret
  = RDone        -- ^ the instruction (or exception) is over
  | RMoveSrc     -- ^ MOVE: the source read
  | RMoveDst     -- ^ MOVE: the destination found
  | RMoveW       -- ^ MOVE: written, the prefetch next
  | RMovePre     -- ^ MOVE to -(An): prefetched, the destination next
  | RMovePreL    -- ^ MOVE.L to -(An): prefetched, the low word next
  | RMovePreL2   -- ^ its high word next
  | RMoveAbs2    -- ^ MOVE to (xxx).L from memory: written, its low word next
  | RMoveAbs3    -- ^ the prefetch next
  | RIdle2       -- ^ two idle clocks, then the end
  | RIdle4       -- ^ four
  | RToDn        -- ^ <ea>,Dn: the source read
  | RToDnL       -- ^ prefetched (a long: its idle clocks next)
  | RRmw         -- ^ a read-modify-write: the operand read
  | RRmwW        -- ^ prefetched, the write-back next
  | RAddr        -- ^ ADDA, SUBA, CMPA: the source read
  | RImmS        -- ^ #<data>,<ea>: the immediate taken
  | RImmCmp      -- ^ CMPI to memory: the operand read
  | RCmpmY       -- ^ CMPM: (Ay)+ read
  | RCmpmX       -- ^ (Ax)+ read
  | RTst         -- ^ TST: the operand read
  | RXW          -- ^ ADDX/SUBX -(Ay),-(Ax): prefetched, the write next
  | RXH          -- ^ a long: the low word written and prefetched, the high word next
  | RIdleN       -- ^ 'cU' idle clocks, then the end
  | RBit         -- ^ a bit operation: the byte read
  | RRefill      -- ^ the queue refilled next
  | RPea         -- ^ PEA: the address found
  | RPea2        -- ^ its index's idle clocks over
  | RPeaPush     -- ^ prefetched, the push next
  | RLea         -- ^ LEA: the address found
  | RLea2        -- ^ its index's idle clocks over
  | RChk         -- ^ CHK: the bound read
  | RChk2        -- ^ prefetched, the check next
  | RCcrSr       -- ^ MOVE to CCR or SR: the word read
  | RTas         -- ^ TAS: the byte's address found
  | RLink2       -- ^ LINK: pushed
  | RBsrPush     -- ^ BSR: its idle clocks over, the push next
  | RJump        -- ^ the jump to 'cT' next
  | RSkipPf      -- ^ an extension word skipped, then the prefetch
  | RPf          -- ^ the prefetch, then the end
  | RDbExp       -- ^ DBcc expired: the target's word next
  | RJsrIrc      -- ^ JSR: pushed, IRC's read next
  | RDiv         -- ^ DIVU, DIVS: the divisor read
  | RDivZ        -- ^ a zero divisor: its idle clocks over, the exception next
  | RMul         -- ^ MULU, MULS: the source read
  | RMovem       -- ^ MOVEM: the address found
  | RStop        -- ^ STOP: prefetched, stopped next
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFDataX, BitPack)
