{-# LANGUAGE StrictData #-}
{-|
The 6502 in Clash (PLAN.md step 3.4): a pure function per CPU cycle,
shared by every 6502 machine. It follows the reference model
("Mos6502.Spec") cycle for cycle, and the lockstep tests (@mos6502-lockstep@)
compare the two bus access by bus access.

__The cycle.__ 'cpuCycle' runs at the end of each CPU cycle. It gets what
that cycle's read returned ('ciData'), the NMI and IRQ lines as they are
at the end of the cycle, and whether the cycle was a stalled read
('ciStall', RDY low); it returns the request for the next cycle. On a
stalled cycle only the NMI edge detector moves on (not the polls) and the same request
comes out again, so the machine sees the read repeated, as on the chip.
The machine decides about stalls itself: RDY only stalls read cycles.
A JAMmed CPU ignores RDY (as the spec's JAM cycles do).

__Interrupts__ as in the spec: at the end of every cycle the NMI edge
detector and the IRQ level (masked by I as it was during the cycle) are
recorded; an instruction is followed by the interrupt sequence when its
second-to-last cycle saw one ('cPollPrev' after its last cycle). The
taken-branch rule, the vector choice after pushing PCL (an NMI hijacks
BRK and IRQ) and "no NMI right after BRK" are the spec's.

__Structure.__ An instruction is its opcode fetch, then the cycles of
its 'Mode' ('cCyc' counts them, the fetch being cycle 1). Once the
operand's address is known, 'cAcc' takes over: the access itself, and for
read-modify-write instructions the write-back of the old value and the
write of the new one. BRK, IRQ, NMI and reset share one 7-cycle sequence
('PSeq').

__Configuration__ ('CoreConfig') is a value, so a machine passes a
constant and synthesis keeps only what it uses: decimal mode, LXA's
constant and the 800XL's two interrupt rules, as 'Mos6502.Spec.Config'.
-}
module Mos6502.Core
  ( -- * Configuration
    CoreConfig (..)
  , coreNmos6502
  , coreRicoh2a03
  , coreSally
    -- * Bus
  , Req (..)
  , CpuIn (..)
  , CpuOut (..)
  , StepKind (..)
    -- * State
  , Phase (..)
  , AccStage (..)
  , SeqKind (..)
  , Cpu (..)
  , cpuPowerOn
  , cpuReset
  , cpuCycle
  , cpuRequest
    -- * Registers
  , CpuRegs (..)
  , cpuRegs
  , cpuSetRegs
  ) where

import Clash.Prelude

import Mos6502.Decode
import Mos6502.Types

data CoreConfig = CoreConfig
  { ccDecimal  :: Bool
  , ccLxaConst :: Unsigned 8
  , ccStallPolls :: Bool      -- ^ 'Mos6502.Spec.cfgStallPolls'
  , ccPushBlocksNmi :: Bool   -- ^ 'Mos6502.Spec.cfgPushBlocksNmi'
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFDataX

coreNmos6502, coreRicoh2a03, coreSally :: CoreConfig
coreNmos6502 = CoreConfig True 0xEE False False
coreRicoh2a03 = CoreConfig False 0xFF False False

-- | The 800XL's 6502C ('Mos6502.Spec.sally6502').
coreSally = coreNmos6502 { ccStallPolls = True, ccPushBlocksNmi = True }

-- | One bus cycle.
data Req = Req
  { rqWrite :: Bool
  , rqAddr  :: Unsigned 16
  , rqData  :: Unsigned 8 -- ^ for writes
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFDataX

data CpuIn = CpuIn
  { ciData  :: Unsigned 8 -- ^ what this cycle's read returned
  , ciStall :: Bool       -- ^ this cycle was a read repeated because RDY was low
  , ciNmi   :: Bool       -- ^ the NMI line (asserted) at the end of the cycle
  , ciIrq   :: Bool       -- ^ the IRQ line (asserted) at the end of the cycle
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFDataX

-- | What a finished step was, as 'Mos6502.Spec.Step'.
data StepKind = SkInstr | SkNmi | SkIrq | SkJam | SkReset
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFDataX

data CpuOut = CpuOut
  { coReq  :: Req              -- ^ the request for the next cycle
  , coStep :: Maybe StepKind   -- ^ this cycle ended a step of the spec
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFDataX

data Phase
  = PFetch  -- ^ the opcode fetch is on the bus
  | PExec   -- ^ cycle 'cCyc' of 'cInstr'
  | PSeq    -- ^ cycle 'cCyc' of the BRK / IRQ / NMI / reset sequence
  | PJam    -- ^ stuck; 'cCyc' walks $FFFF, $FFFE, $FFFE, then $FFFF
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFDataX

data AccStage = AccNone | AccOp | AccOld | AccNew
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFDataX

data SeqKind = SeqBrk | SeqIrq | SeqReset
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFDataX

data Cpu = Cpu
  { cA, cX, cY, cS, cP :: Unsigned 8
  , cPc       :: Unsigned 16
  , cPhase    :: Phase
  , cInstr    :: Instr
  , cCyc      :: Index 8
  , cAcc      :: AccStage
  , cSeq      :: SeqKind
  , cAd       :: Unsigned 16  -- ^ operand address; the base before indexing
  , cD        :: Unsigned 8   -- ^ data latch: zero page pointer, branch offset, RMW value
  , cReq      :: Req          -- ^ the request on the bus this cycle
  , cNmiLine  :: Bool         -- ^ NMI line at the end of the last cycle
  , cNmiNeed  :: Bool         -- ^ NMI edge seen, not yet serviced
  , cPollCur  :: BitVector 2  -- ^ bit 0 NMI, bit 1 IRQ: would be taken (last cycle)
  , cPollPrev :: BitVector 2  -- ^ the same, one cycle earlier
  , cNmiVec   :: Bool         -- ^ the sequence chose the NMI vector
  , cHeld     :: Bool         -- ^ the last cycle was a read held by RDY (SH* stores)
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFDataX

data CpuRegs = CpuRegs
  { crA, crX, crY, crS, crP :: Unsigned 8
  , crPc :: Unsigned 16
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFDataX

cpuRegs :: Cpu -> CpuRegs
cpuRegs c = CpuRegs (cA c) (cX c) (cY c) (cS c) (cP c) (cPc c)

-- | Set the registers of a CPU that is about to fetch an opcode.
cpuSetRegs :: CpuRegs -> Cpu -> Cpu
cpuSetRegs (CpuRegs a x y s p pc) c =
  c { cA = a, cX = x, cY = y, cS = s, cP = p, cPc = pc, cReq = rd pc }

-- | The request on the bus this cycle.
cpuRequest :: Cpu -> Req
cpuRequest = cReq

-- | Before the reset sequence: registers as given, the opcode fetch at PC
-- next (for tests that start mid-program), interrupt state clear.
cpuPowerOn :: CpuRegs -> Cpu
cpuPowerOn r = cpuSetRegs r Cpu
  { cA = 0, cX = 0, cY = 0, cS = 0, cP = 0, cPc = 0
  , cPhase = PFetch, cInstr = Instr NOP Imp KRead, cCyc = 1, cAcc = AccNone
  , cSeq = SeqReset, cAd = 0, cD = 0, cReq = rd 0
  , cNmiLine = False, cNmiNeed = False, cPollCur = 0, cPollPrev = 0, cNmiVec = False, cHeld = False }

-- | Start the reset sequence (7 cycles, the first one next): S drops by
-- 3 with reads, I is set, PC comes from $FFFC. A JAM ends.
cpuReset :: Cpu -> Cpu
cpuReset c = c { cPhase = PSeq, cSeq = SeqReset, cCyc = 1, cAcc = AccNone, cReq = rd (cPc c)
               , cNmiNeed = False, cPollCur = 0, cPollPrev = 0 }

rd :: Unsigned 16 -> Req
rd a = Req False a 0

wr :: Unsigned 16 -> Unsigned 8 -> Req
wr = Req True

fC, fZ, fI, fD, fB, fU, fV, fN :: Unsigned 8
fC = 0x01
fZ = 0x02
fI = 0x04
fD = 0x08
fB = 0x10
fU = 0x20
fV = 0x40
fN = 0x80

flag :: Unsigned 8 -> Unsigned 8 -> Bool
flag f p = p .&. f /= 0

setF :: Unsigned 8 -> Bool -> Unsigned 8 -> Unsigned 8
setF f b p = if b then p .|. f else p .&. complement f

nz :: Unsigned 8 -> Unsigned 8 -> Unsigned 8
nz v p = (p .&. 0x7D) .|. (v .&. 0x80) .|. (if v == 0 then fZ else 0)

hi8, lo8 :: Unsigned 16 -> Unsigned 8
hi8 x = resize (x `shiftR` 8)
lo8 = resize

w16 :: Unsigned 8 -> Unsigned 8 -> Unsigned 16
w16 h l = resize h `shiftL` 8 .|. resize l

stackAddr :: Unsigned 8 -> Unsigned 16
stackAddr s = 0x100 .|. resize s

-- * The cycle

cpuCycle :: CoreConfig -> Cpu -> CpuIn -> (Cpu, CpuOut)
cpuCycle cfg c0 inp
  | ciStall inp && cPhase c0 /= PJam = (cHalted, CpuOut (cReq cHalted) Nothing)
  | otherwise = let (c2, st) = advance cfg c1 (ciData inp) in (c2 { cHeld = False }, CpuOut (cReq c2) st)
 where
  c1 = samplePolls c0 inp
  -- halted (RDY low): the NMI edge detector runs, the polls don't (as
  -- Mos6502.Spec's stallCycle), unless 'ccStallPolls'
  cHalted | ccStallPolls cfg = c1 { cHeld = True }
          | otherwise = c0 { cNmiLine = ciNmi inp, cNmiNeed = cNmiNeed c1, cHeld = True }

-- | The end of every cycle: the NMI edge detector and the polls.
samplePolls :: Cpu -> CpuIn -> Cpu
samplePolls c inp = c { cNmiLine = ciNmi inp, cNmiNeed = need, cPollPrev = cPollCur c
                 , cPollCur = (if irq then 2 else 0) .|. (if need then 1 else 0) }
 where
  need = cNmiNeed c || (ciNmi inp && not (cNmiLine c))
  irq = ciIrq inp && not (flag fI (cP c))

-- | End an instruction: the next cycle reads at PC, as an opcode fetch or
-- as the start of the interrupt sequence its second-to-last cycle saw.
done :: Cpu -> (Cpu, Maybe StepKind)
done c
  | cPollPrev c /= 0 = (c { cPhase = PSeq, cSeq = SeqIrq, cCyc = 1, cAcc = AccNone, cReq = rd (cPc c) }, Just SkInstr)
  | otherwise = (c { cPhase = PFetch, cCyc = 1, cAcc = AccNone, cReq = rd (cPc c) }, Just SkInstr)

-- | Next cycle of an instruction: a request, PC unchanged.
next :: Cpu -> Req -> (Cpu, Maybe StepKind)
next c r = (c { cCyc = satSucc SatBound (cCyc c), cReq = r }, Nothing)

-- | Next cycle reads at PC and advances it.
nextPC :: Cpu -> (Cpu, Maybe StepKind)
nextPC c = next c { cPc = cPc c + 1 } (rd (cPc c))

advance :: CoreConfig -> Cpu -> Unsigned 8 -> (Cpu, Maybe StepKind)
advance cfg c d = case cPhase c of
  PFetch ->
    let i = decode d
        c' = c { cPhase = PExec, cInstr = i, cCyc = 1, cAcc = AccNone, cPc = cPc c + 1 }
    in case iMode i of
         Imp -> next c' (rd (cPc c'))
         Push -> next c' (rd (cPc c'))
         Pull -> next c' (rd (cPc c'))
         Rts -> next c' (rd (cPc c'))
         Rti -> next c' (rd (cPc c'))
         Jam -> next c' (rd (cPc c'))
         _ -> nextPC c'
  PJam -> let n = cCyc c
              n' = if n >= 5 then 5 else n + 1
          in (c { cCyc = n', cReq = rd (jamAddr n') }, Just SkJam)
  PSeq -> sequence' cfg c d
  PExec -> case cAcc c of
    AccNone -> exec cfg c d
    AccOp -> case iKind (cInstr c) of
      KRead -> done (applyRead cfg (iOp (cInstr c)) d c)
      KWrite -> done c
      KRmw -> next c { cAcc = AccOld, cD = d } (wr (cAd c) d)
    AccOld ->
      let (v, c') = applyRmw cfg (iOp (cInstr c)) (cD c) c
      in next c' { cAcc = AccNew } (wr (cAd c) v)
    AccNew -> done c

jamAddr :: Index 8 -> Unsigned 16
jamAddr n = if n == 2 || n == 3 then 0xFFFE else 0xFFFF

-- | Put the operand access on the bus.
access :: Cpu -> Unsigned 16 -> (Cpu, Maybe StepKind)
access c a = next c { cAd = a, cAcc = AccOp } req
 where
  req = case iKind (cInstr c) of
    KWrite -> wr a (storeValue c)
    _ -> rd a

storeValue :: Cpu -> Unsigned 8
storeValue c = case iOp (cInstr c) of
  STX -> cX c
  STY -> cY c
  SAX -> cA c .&. cX c
  _ -> cA c

-- | The cycles of each mode, at the end of cycle 'cCyc' (2 and up).
exec :: CoreConfig -> Cpu -> Unsigned 8 -> (Cpu, Maybe StepKind)
exec cfg c d = case iMode i of
  Imp -> done (applyImplied (iOp i) c)
  Imm -> done (applyRead cfg (iOp i) d c)
  Zp -> access c (resize d)
  ZpX -> zpIdx (cX c)
  ZpY -> zpIdx (cY c)
  Abs -> case n of
    2 -> nextPC c { cAd = resize d }
    _ -> access c (w16 d (lo8 (cAd c)))
  AbsX -> absIdx (cX c)
  AbsY -> absIdx (cY c)
  IndX -> case n of
    2 -> next c { cD = d } (rd (resize d))
    3 -> next c (rd (resize (cD c + cX c)))
    4 -> next c { cAd = resize d } (rd (resize (cD c + cX c + 1)))
    _ -> access c (w16 d (lo8 (cAd c)))
  IndY -> case n of
    2 -> next c { cD = d } (rd (resize d))
    3 -> next c { cAd = resize d } (rd (resize (cD c + 1)))
    _ -> indexed (w16 d (lo8 (cAd c))) (cY c) (n == 4)
  Rel -> case n of
    2 | not (branchTaken (iOp i) (cP c)) -> done c
      | otherwise ->
          -- An IRQ first seen in the operand cycle waits one instruction.
          let pc' = cPc c
              cur = cPollCur c
              cur' = if testBit cur 1 && not (testBit (cPollPrev c) 1) then cur .&. 1 else cur
          in next c { cD = d, cPollCur = cur' } (rd pc')
    3 -> let t = cPc c + signExtendOffset (cD c)
             wrong = (cPc c .&. 0xFF00) .|. (t .&. 0x00FF)
         in if wrong /= t then next c { cAd = t } (rd wrong) else done c { cPc = t }
    _ -> done c { cPc = cAd c }
  JmpAbs -> case n of
    2 -> nextPC c { cAd = resize d }
    _ -> done c { cPc = w16 d (lo8 (cAd c)) }
  JmpInd -> case n of
    2 -> nextPC c { cAd = resize d }
    3 -> let a = w16 d (lo8 (cAd c)) in next c { cAd = a } (rd a)
    4 -> next c { cD = d } (rd ((cAd c .&. 0xFF00) .|. resize (lo8 (cAd c) + 1)))
    _ -> done c { cPc = w16 d (cD c) }
  Jsr -> case n of
    2 -> next c { cAd = resize d } (rd (stackAddr (cS c)))
    3 -> next c { cS = cS c - 1 } (wr (stackAddr (cS c)) (hi8 (cPc c)))
    4 -> next c { cS = cS c - 1 } (wr (stackAddr (cS c)) (lo8 (cPc c)))
    5 -> next c (rd (cPc c))
    _ -> done c { cPc = w16 d (lo8 (cAd c)) }
  Rts -> case n of
    2 -> next c (rd (stackAddr (cS c)))
    3 -> next c { cS = cS c + 1 } (rd (stackAddr (cS c + 1)))
    4 -> next c { cS = cS c + 1, cAd = resize d } (rd (stackAddr (cS c + 1)))
    5 -> let pc = w16 d (lo8 (cAd c)) in next c { cPc = pc + 1 } (rd pc)
    _ -> done c
  Rti -> case n of
    2 -> next c (rd (stackAddr (cS c)))
    3 -> next c { cS = cS c + 1 } (rd (stackAddr (cS c + 1)))
    4 -> next c { cS = cS c + 1, cP = (d .&. complement fB) .|. fU } (rd (stackAddr (cS c + 1)))
    5 -> next c { cS = cS c + 1, cAd = resize d } (rd (stackAddr (cS c + 1)))
    _ -> done c { cPc = w16 d (lo8 (cAd c)) }
  Push -> case n of
    2 -> next c { cS = cS c - 1 } (wr (stackAddr (cS c)) pushed)
    _ -> done c
  Pull -> case n of
    2 -> next c (rd (stackAddr (cS c)))
    3 -> next c { cS = cS c + 1 } (rd (stackAddr (cS c + 1)))
    _ -> done (if iOp i == PLP then c { cP = (d .&. complement fB) .|. fU } else c { cA = d, cP = nz d (cP c) })
  Brk -> -- cycle 2 was the padding byte; the sequence goes on from cycle 3
    next c { cPhase = PSeq, cSeq = SeqBrk, cS = cS c - 1 } (wr (stackAddr (cS c)) (hi8 (cPc c)))
  Jam -> (c { cPhase = PJam, cCyc = 1, cReq = rd (jamAddr 1) }, Just SkInstr)
 where
  i = cInstr c
  n = cCyc c
  pushed = if iOp i == PHP then cP c .|. fB .|. fU else cA c
  zpIdx r = case n of
    2 -> next c { cD = d } (rd (resize d))
    _ -> access c (resize (cD c + r))
  absIdx r = case n of
    2 -> nextPC c { cAd = resize d }
    _ -> indexed (w16 d (lo8 (cAd c))) r (n == 3)
  -- The base address is known (first == the cycle that completed it):
  -- the extra cycle at the wrong address when the page is crossed or the
  -- instruction writes, then the access. SHA/SHX/SHY/TAS store their odd
  -- value (and address) instead.
  indexed base r first
    | first =
        let ea = base + resize r
            wrong = (base .&. 0xFF00) .|. (ea .&. 0x00FF)
            sh = iOp i `elem` [SHA, SHX, SHY, TAS]
        in if iKind i /= KRead || wrong /= ea || sh
             then next c { cAd = base } (rd wrong)
             else access c ea
    | otherwise =
        let ea = cAd c + resize r
            wrong = (cAd c .&. 0xFF00) .|. (ea .&. 0x00FF)
        in case iOp i of
             SHA -> shStore (cA c .&. cX c) ea wrong
             SHX -> shStore (cX c) ea wrong
             SHY -> shStore (cY c) ea wrong
             TAS -> shStore (cA c .&. cX c) ea wrong
             _ -> access c ea
  -- RDY held the dummy read (the cycles just before this one): the
  -- & (H+1) drops out (as Mos6502.Spec)
  shStore v ea wrong =
    let dv = if cHeld c then v else v .&. (hi8 (cAd c) + 1)
        target = if wrong /= ea then w16 dv (lo8 ea) else ea
        c' = if iOp i == TAS then c { cS = cA c .&. cX c } else c
    in next c' { cAd = target, cAcc = AccOp } (wr target dv)

signExtendOffset :: Unsigned 8 -> Unsigned 16
signExtendOffset o = if testBit o 7 then 0xFF00 .|. resize o else resize o

branchTaken :: Op -> Unsigned 8 -> Bool
branchTaken op p = case op of
  BPL -> not (flag fN p)
  BMI -> flag fN p
  BVC -> not (flag fV p)
  BVS -> flag fV p
  BCC -> not (flag fC p)
  BCS -> flag fC p
  BNE -> not (flag fZ p)
  _ -> flag fZ p

-- | BRK (from cycle 3), IRQ/NMI and reset: 7 cycles. 'cCyc' is the cycle
-- that just ended. IRQ/NMI and reset read at PC twice; then three pushes
-- (reset: three stack reads), each moving S down; the vector is chosen at
-- the end of cycle 4, I is set at the end of cycle 5, then the vector.
sequence' :: CoreConfig -> Cpu -> Unsigned 8 -> (Cpu, Maybe StepKind)
sequence' cfg c0 d = case cCyc c of
  1 -> next c (rd (cPc c))
  2 | reset -> stackRead
    | otherwise -> push (hi8 (cPc c))
  3 | reset -> stackRead
    | otherwise -> push (lo8 (cPc c))
  4 | reset -> stackRead
    | otherwise ->
        -- The vector is chosen now: an NMI waiting by now takes it.
        let nmi = cNmiNeed c
            pushedP = if cSeq c == SeqBrk then cP c .|. fB .|. fU else (cP c .&. complement fB) .|. fU
        in next c { cS = cS c - 1, cNmiNeed = False, cNmiVec = nmi } (wr (stackAddr (cS c)) pushedP)
  5 -> let v | reset = 0xFFFC
             | cNmiVec c = 0xFFFA
             | otherwise = 0xFFFE
       in next c { cP = cP c .|. fI, cAd = v } (rd v)
  6 -> next c { cD = d } (rd (cAd c + 1))
  _ -> finish c { cPc = w16 d (cD c) }
 where
  reset = cSeq c0 == SeqReset
  -- 'ccPushBlocksNmi': an NMI edge seen on the cycle P was pushed is lost
  c | ccPushBlocksNmi cfg && cCyc c0 == 5 && not reset = c0 { cNmiNeed = False, cPollCur = cPollCur c0 .&. 2 }
    | otherwise = c0
  push v = next c { cS = cS c - 1 } (wr (stackAddr (cS c)) v)
  stackRead = next c { cS = cS c - 1 } (rd (stackAddr (cS c)))
  finish c' = case cSeq c' of
    -- The handler's first instruction runs before a waiting NMI.
    SeqBrk -> done c' { cPollPrev = cPollPrev c' .&. 2 }
    SeqIrq -> (c' { cPhase = PFetch, cCyc = 1, cReq = rd (cPc c') }, Just (if cNmiVec c' then SkNmi else SkIrq))
    SeqReset -> (c' { cPhase = PFetch, cCyc = 1, cReq = rd (cPc c') }, Just SkReset)

-- * Operations

-- | Implied and accumulator instructions, at the end of their dummy read.
applyImplied :: Op -> Cpu -> Cpu
applyImplied op c = case op of
  ASL -> acc (shiftOp ASL)
  LSR -> acc (shiftOp LSR)
  ROL -> acc (shiftOp ROL)
  ROR -> acc (shiftOp ROR)
  TAX -> c { cX = a, cP = nz a p }
  TAY -> c { cY = a, cP = nz a p }
  TXA -> c { cA = cX c, cP = nz (cX c) p }
  TYA -> c { cA = cY c, cP = nz (cY c) p }
  TSX -> c { cX = cS c, cP = nz (cS c) p }
  TXS -> c { cS = cX c }
  INX -> let v = cX c + 1 in c { cX = v, cP = nz v p }
  INY -> let v = cY c + 1 in c { cY = v, cP = nz v p }
  DEX -> let v = cX c - 1 in c { cX = v, cP = nz v p }
  DEY -> let v = cY c - 1 in c { cY = v, cP = nz v p }
  CLC -> c { cP = setF fC False p }
  SEC -> c { cP = setF fC True p }
  CLI -> c { cP = setF fI False p }
  SEI -> c { cP = setF fI True p }
  CLV -> c { cP = setF fV False p }
  CLD -> c { cP = setF fD False p }
  SED -> c { cP = setF fD True p }
  _ -> c
 where
  a = cA c
  p = cP c
  acc f = let (v, p') = f a p in c { cA = v, cP = p' }

-- | ASL, LSR, ROL, ROR: result and P.
shiftOp :: Op -> Unsigned 8 -> Unsigned 8 -> (Unsigned 8, Unsigned 8)
shiftOp op v p = (r, nz r (setF fC co p))
 where
  ci = flag fC p
  (r, co) = case op of
    ASL -> (v `shiftL` 1, testBit v 7)
    LSR -> (v `shiftR` 1, testBit v 0)
    ROL -> ((v `shiftL` 1) .|. (if ci then 1 else 0), testBit v 7)
    _ -> ((v `shiftR` 1) .|. (if ci then 0x80 else 0), testBit v 0)

compareOp :: Unsigned 8 -> Unsigned 8 -> Unsigned 8 -> Unsigned 8
compareOp r v p = setF fC (r >= v) (nz (r - v) p)

-- | ADC with the NMOS decimal mode (Bruce Clark, appendix A): result, P.
adcOp :: CoreConfig -> Unsigned 8 -> Unsigned 8 -> Unsigned 8 -> (Unsigned 8, Unsigned 8)
adcOp cfg a v p
  | ccDecimal cfg && flag fD p =
      let al0 = (ai .&. 0x0F) + (vi .&. 0x0F) + ci
          al = if al0 >= 0x0A then ((al0 + 0x06) .&. 0x0F) + 0x10 else al0
          r1 = (ai .&. 0xF0) + (vi .&. 0xF0) + al
          sa = sx (ai .&. 0xF0) + sx (vi .&. 0xF0) + al
          r2 = if r1 >= 0xA0 then r1 + 0x60 else r1
          p' = (p .&. 0x3C) .|. (if r2 >= 0x100 then fC else 0) .|. (if binR == 0 then fZ else 0)
               .|. (if testBit r1 7 then fN else 0) .|. (if sa < -128 || sa > 127 then fV else 0)
      in (resize (bitCoerce r2 :: Unsigned 12), p')
  | otherwise = binAdc a v (flag fC p) p
 where
  ai = zx a
  vi = zx v
  ci = if flag fC p then 1 else 0
  sx x = if x >= 0x80 then x - 0x100 else x
  binR = a + v + (if flag fC p then 1 else 0)

binAdc :: Unsigned 8 -> Unsigned 8 -> Bool -> Unsigned 8 -> (Unsigned 8, Unsigned 8)
binAdc a v cin p = (r, setF fV ov (setF fC (s > 0xFF) (nz r p)))
 where
  s = resize a + resize v + (if cin then 1 else 0) :: Unsigned 9
  r = resize s
  ov = (a `xor` r) .&. (v `xor` r) .&. 0x80 /= 0

-- | SBC: binary flags always (NMOS); the decimal result (sequence 3).
sbcOp :: CoreConfig -> Unsigned 8 -> Unsigned 8 -> Unsigned 8 -> (Unsigned 8, Unsigned 8)
sbcOp cfg a v p
  | ccDecimal cfg && flag fD p =
      let al0 = (ai .&. 0x0F) - (vi .&. 0x0F) + ci - 1
          al = if al0 < 0 then ((al0 - 0x06) .&. 0x0F) - 0x10 else al0
          r1 = (ai .&. 0xF0) - (vi .&. 0xF0) + al
          r2 = if r1 < 0 then r1 - 0x60 else r1
      in (resize (bitCoerce r2 :: Unsigned 12), p')
  | otherwise = (r, p')
 where
  (r, p') = binAdc a (complement v) (flag fC p) p
  ai = zx a
  vi = zx v
  ci = if flag fC p then 1 else 0

-- | The operand's read: loads, ALU, compares, BIT, and the immediate
-- undocumented ones.
applyRead :: CoreConfig -> Op -> Unsigned 8 -> Cpu -> Cpu
applyRead cfg op v c = case op of
  LDA -> c { cA = v, cP = nz v p }
  LDX -> c { cX = v, cP = nz v p }
  LDY -> c { cY = v, cP = nz v p }
  LAX -> c { cA = v, cX = v, cP = nz v p }
  ORA -> setA (a .|. v)
  AND -> setA (a .&. v)
  EOR -> setA (a `xor` v)
  ADC -> let (r, p') = adcOp cfg a v p in c { cA = r, cP = p' }
  SBC -> let (r, p') = sbcOp cfg a v p in c { cA = r, cP = p' }
  CMP -> c { cP = compareOp a v p }
  CPX -> c { cP = compareOp (cX c) v p }
  CPY -> c { cP = compareOp (cY c) v p }
  BIT -> c { cP = (p .&. 0x3D) .|. (v .&. 0xC0) .|. (if a .&. v == 0 then fZ else 0) }
  ANC -> let r = a .&. v in c { cA = r, cP = setF fC (testBit r 7) (nz r p) }
  ALR -> let (r, p') = shiftOp LSR (a .&. v) p in c { cA = r, cP = p' }
  ARR -> arr
  ANE -> setA ((a .|. 0xEE) .&. cX c .&. v)
  LXA -> let r = (a .|. ccLxaConst cfg) .&. v in c { cA = r, cX = r, cP = nz r p }
  AXS -> let ax = a .&. cX c in c { cX = ax - v, cP = compareOp ax v p }
  LAS -> let r = v .&. cS c in c { cA = r, cX = r, cS = r, cP = nz r p }
  _ -> c -- NOP
 where
  a = cA c
  p = cP c
  setA r = c { cA = r, cP = nz r p }
  arr =
    let t = a .&. v
        ci = flag fC p
        r = (t `shiftR` 1) .|. (if ci then 0x80 else 0)
    in if ccDecimal cfg && flag fD p
         then
           -- "NMOS 6510 Unintended Opcodes", ARR in decimal mode.
           let ah = t `shiftR` 4
               al = t .&. 0x0F
               r1 = if al + (al .&. 1) > 5 then (r .&. 0xF0) .|. ((r + 6) .&. 0x0F) else r
               hc = ah + (ah .&. 1) > 5
               r2 = if hc then r1 + 0x60 else r1
               p' = setF fN ci (setF fZ (r == 0) (setF fV ((t `xor` r) .&. 0x40 /= 0) (setF fC hc p)))
           in c { cA = r2, cP = p' }
         else c { cA = r, cP = setF fV (testBit r 6 /= testBit r 5) (setF fC (testBit r 6) (nz r p)) }

-- | Read-modify-write: the new value and the CPU after it (flags, and A
-- for the combined undocumented ones).
applyRmw :: CoreConfig -> Op -> Unsigned 8 -> Cpu -> (Unsigned 8, Cpu)
applyRmw cfg op v c = case op of
  ASL -> sh ASL
  LSR -> sh LSR
  ROL -> sh ROL
  ROR -> sh ROR
  INC -> let r = v + 1 in (r, c { cP = nz r p })
  DEC -> let r = v - 1 in (r, c { cP = nz r p })
  SLO -> combo ASL (\r c' -> c' { cA = cA c' .|. r, cP = nz (cA c' .|. r) (cP c') })
  RLA -> combo ROL (\r c' -> c' { cA = cA c' .&. r, cP = nz (cA c' .&. r) (cP c') })
  SRE -> combo LSR (\r c' -> c' { cA = cA c' `xor` r, cP = nz (cA c' `xor` r) (cP c') })
  RRA -> combo ROR (\r c' -> let (x, p') = adcOp cfg (cA c') r (cP c') in c' { cA = x, cP = p' })
  DCP -> let r = v - 1 in (r, c { cP = compareOp (cA c) r p })
  ISC -> let r = v + 1
             (x, p') = sbcOp cfg (cA c) r p
         in (r, c { cA = x, cP = p' })
  _ -> (v, c)
 where
  p = cP c
  sh o = let (r, p') = shiftOp o v p in (r, c { cP = p' })
  combo o f = let (r, p') = shiftOp o v p in (r, f r c { cP = p' })

-- | Zero-extend a byte for the decimal arithmetic.
zx :: Unsigned 8 -> Signed 12
zx x = bitCoerce (resize x :: Unsigned 12)
