{-|
The SM83 CPU core in Clash (PLAN.md step 2.4), on a generic memory bus.

The core works in M-cycles. 'cpuCycle' runs at the end of each M-cycle:
it gets the byte the bus returned for this M-cycle's request (if it was a
read) and IE / IF as they are after this M-cycle (the machine's tick and
the access itself), and returns its next state and the request for the
next M-cycle. The machine ("Gb.Core") performs each request after
advancing its peripherals for that M-cycle, exactly like the reference
model's 'Gb.Spec.Machine.busRead' / 'busWrite' / 'busIdle', so the two can
be compared M-cycle by M-cycle (layer 2, @gb-lockstep@).

It follows the reference model's rules ("Gb.Spec.Cpu", and
docs/systems/gameboy.md, "As built"):

* an instruction's first M-cycle is its opcode fetch; 'coStepDone' marks
  the last M-cycle of what the spec calls one 'Gb.Spec.Cpu.step' (an
  instruction, an interrupt dispatch, or one halted M-cycle);
* the interrupt check uses IE and IF after the fetch: if an interrupt is
  taken, the fetched byte is dropped and the dispatch takes 4 more
  M-cycles (idle, push PC high, push PC low, idle); the vector is chosen,
  and its IF bit acknowledged ('coAck'), after the high byte is pushed;
* a halted CPU fetches at PC every M-cycle until IE & IF is non-zero;
* EI takes effect after the next fetch; HALT with IME = 0 and an
  interrupt pending causes the HALT bug; right after EI, it leaves PC on
  the HALT; illegal opcodes lock the CPU (idle M-cycles from then on).

Each instruction is a small sequence over 'cMc' (0 = the fetch); the
cycle counts are those of 'Gb.Decode.instrCycles', which the tests check
against the spec.
-}
module Gb.Cpu
  ( -- * Bus
    BusOp (..)
  , BusReq (..)
  , CpuIn (..)
  , CpuOut (..)
    -- * State
  , Phase (..)
  , Cpu (..)
  , cpuInit
  , cpuInitRegs
  , cpuCycle
  , cpuMealy
    -- * Registers (for tests and debugging)
  , CpuRegs (..)
  , cpuRegs
  ) where

import Clash.Prelude

import Gb.Decode
import Gb.Types

data BusOp = OpIdle | OpRead | OpWrite
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFDataX

-- | One M-cycle on the bus.
data BusReq = BusReq
  { brOp   :: BusOp
  , brAddr :: BitVector 16
  , brData :: BitVector 8 -- ^ for writes
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFDataX

data CpuIn = CpuIn
  { ciData :: BitVector 8 -- ^ what this M-cycle's read returned
  , ciIe   :: BitVector 8 -- ^ IE after this M-cycle
  , ciIf   :: BitVector 5 -- ^ IF after this M-cycle
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFDataX

data CpuOut = CpuOut
  { coReq      :: BusReq          -- ^ the request for the next M-cycle
  , coAck      :: Maybe (Index 5) -- ^ clear this IF bit now (dispatch)
  , coStepDone :: Bool            -- ^ this M-cycle ended a spec step
  , coBreak    :: Bool            -- ^ LD B,B executed (Mooneye's end of test)
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFDataX

data Phase = PhFetch | PhExec | PhDispatch | PhLocked
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFDataX

data Cpu = Cpu
  { cRegs    :: Vec 8 (BitVector 8) -- ^ B C D E H L F A (index = the r field; F at 6)
  , cSp      :: BitVector 16
  , cPc      :: BitVector 16
  , cIme     :: Bool
  , cEi      :: Bool                -- ^ EI executed: IME = 1 after the next fetch
  , cHalt    :: Bool
  , cHaltBug :: Bool                -- ^ the next fetch does not advance PC
  , cPhase   :: Phase
  , cInstr   :: Instr               -- ^ the instruction being executed
  , cCb      :: CbInstr             -- ^ after a CB prefix
  , cEiNow   :: Bool                -- ^ IME became 1 at this instruction's fetch
  , cMc      :: Index 8             -- ^ M-cycle within the instruction (0 = fetch)
  , cZ       :: BitVector 8         -- ^ operand / temporary (low byte)
  , cW       :: BitVector 8         -- ^ operand / temporary (high byte)
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFDataX

-- | The post-boot-ROM state (Pan Docs, DMG): the same as the spec's
-- 'Gb.Spec.Machine.newMachine'. @hc@: the cartridge's header checksum is
-- non-zero (then H and C are set). The first request is the fetch at
-- 0x0100.
cpuInit :: Bool -> (Cpu, BusReq)
cpuInit hc = cpuInitRegs (0x00 :> 0x13 :> 0x00 :> 0xD8 :> 0x01 :> 0x4D :> f :> 0x01 :> Nil)
 where
  f = if hc then 0xB0 else 0x80

-- | The CPU at PC 0x0100 with these registers (B C D E H L F A): the
-- Game Boy Color's hand-off ("Gb.Core").
cpuInitRegs :: Vec 8 (BitVector 8) -> (Cpu, BusReq)
cpuInitRegs regs =
  ( Cpu { cRegs = regs
        , cSp = 0xFFFE, cPc = 0x0100, cIme = False, cEi = False, cHalt = False
        , cHaltBug = False, cPhase = PhFetch, cInstr = Nop, cCb = Rot RotRlc B
        , cEiNow = False, cMc = 0, cZ = 0, cW = 0 }
  , rd 0x0100 )

-- * Small helpers

rd :: BitVector 16 -> BusReq
rd a = BusReq OpRead a 0

wr :: BitVector 16 -> BitVector 8 -> BusReq
wr = BusReq OpWrite

idleReq :: BusReq
idleReq = BusReq OpIdle 0 0

fZ, fN, fH, fC :: BitVector 8
fZ = 0x80
fN = 0x40
fH = 0x20
fC = 0x10

r8ix :: R8 -> Index 8
r8ix = bitCoerce

getR :: Cpu -> R8 -> BitVector 8
getR s r = cRegs s !! r8ix r

setR :: R8 -> BitVector 8 -> Cpu -> Cpu
setR r v s = s { cRegs = replace (r8ix r) v (cRegs s) }

getA, getF :: Cpu -> BitVector 8
getA s = getR s A
getF s = cRegs s !! (6 :: Index 8)

setA :: BitVector 8 -> Cpu -> Cpu
setA = setR A

setF :: BitVector 8 -> Cpu -> Cpu
setF v s = s { cRegs = replace (6 :: Index 8) (v .&. 0xF0) (cRegs s) }

pair :: Cpu -> R8 -> R8 -> BitVector 16
pair s h l = getR s h ++# getR s l

setPair :: R8 -> R8 -> BitVector 16 -> Cpu -> Cpu
setPair h l v = setR h (slice d15 d8 v) . setR l (slice d7 d0 v)

getHL :: Cpu -> BitVector 16
getHL s = pair s H L

setHL :: BitVector 16 -> Cpu -> Cpu
setHL = setPair H L

getRp :: Cpu -> R16 -> BitVector 16
getRp s r = case r of
  BC -> pair s B C
  DE -> pair s D E
  HL -> getHL s
  SP -> cSp s

setRp :: R16 -> BitVector 16 -> Cpu -> Cpu
setRp r v s = case r of
  BC -> setPair B C v s
  DE -> setPair D E v s
  HL -> setHL v s
  SP -> s { cSp = v }

getStk :: Cpu -> R16Stk -> BitVector 16
getStk s r = case r of
  StkBC -> pair s B C
  StkDE -> pair s D E
  StkHL -> getHL s
  StkAF -> getA s ++# getF s

setStk :: R16Stk -> BitVector 16 -> Cpu -> Cpu
setStk r v s = case r of
  StkBC -> setPair B C v s
  StkDE -> setPair D E v s
  StkHL -> setHL v s
  StkAF -> setF (slice d7 d0 v) (setA (slice d15 d8 v) s)

flag :: Bool -> BitVector 8 -> BitVector 8
flag b f = if b then f else 0

condHolds :: Cpu -> Cond -> Bool
condHolds s c = case c of
  CondNZ -> f .&. fZ == 0
  CondZ -> f .&. fZ /= 0
  CondNC -> f .&. fC == 0
  CondC -> f .&. fC /= 0
 where
  f = getF s

sext :: BitVector 8 -> BitVector 16
sext e = pack (signExtend (unpack e :: Signed 8) :: Signed 16)

hi8, lo8 :: BitVector 16 -> BitVector 8
hi8 = slice d15 d8
lo8 = slice d7 d0

-- * ALU

-- | ALU operation on A and an operand: new A (unchanged for CP) and F.
alu :: AluOp -> BitVector 8 -> BitVector 8 -> BitVector 8 -> (BitVector 8, BitVector 8)
alu op a b f = case op of
  AluAdd -> addF 0
  AluAdc -> addF c
  AluSub -> subF 0
  AluSbc -> subF c
  AluAnd -> let r = a .&. b in (r, z r .|. fH)
  AluXor -> let r = a `xor` b in (r, z r)
  AluOr -> let r = a .|. b in (r, z r)
  AluCp -> (a, snd (subF 0))
 where
  c = if f .&. fC /= 0 then 1 else 0 :: Unsigned 9
  z r = flag (r == 0) fZ
  ua = zeroExtend (unpack a) :: Unsigned 9
  ub = zeroExtend (unpack b) :: Unsigned 9
  lo x = x .&. 0xF
  addF cin = let s = ua + ub + cin
                 r = pack (truncateB s :: Unsigned 8)
             in (r, z r .|. flag (lo ua + lo ub + cin > 0xF) fH .|. flag (s > 0xFF) fC)
  subF cin = let r = pack (truncateB (ua - ub - cin) :: Unsigned 8)
             in (r, z r .|. fN .|. flag (lo ua < lo ub + cin) fH .|. flag (ua < ub + cin) fC)

-- | RLCA RRCA RLA RRA DAA CPL SCF CCF.
accOp :: AccOp -> BitVector 8 -> BitVector 8 -> (BitVector 8, BitVector 8)
accOp op a f = case op of
  AccRlca -> rot (rotateL a 1) (testBit a 7)
  AccRrca -> rot (rotateR a 1) (testBit a 0)
  AccRla -> rot (shiftL a 1 .|. flag cy 1) (testBit a 7)
  AccRra -> rot (shiftR a 1 .|. flag cy 0x80) (testBit a 0)
  AccDaa ->
    let (a', c')
          | not n = let (a1, c1) = if cy || a > 0x99 then (a + 0x60, True) else (a, False)
                        a2 = if h || a1 .&. 0x0F > 9 then a1 + 6 else a1
                    in (a2, c1)
          | otherwise = let a1 = if cy then a - 0x60 else a
                            a2 = if h then a1 - 6 else a1
                        in (a2, cy)
    in (a', flag (a' == 0) fZ .|. (f .&. fN) .|. flag c' fC)
  AccCpl -> (complement a, (f .&. (fZ .|. fC)) .|. fN .|. fH)
  AccScf -> (a, (f .&. fZ) .|. fC)
  AccCcf -> (a, (f .&. fZ) .|. flag (not cy) fC)
 where
  cy = f .&. fC /= 0
  n = f .&. fN /= 0
  h = f .&. fH /= 0
  rot r c = (r, flag c fC)

-- | CB rotates and shifts: result and F.
rotOp :: RotOp -> BitVector 8 -> BitVector 8 -> (BitVector 8, BitVector 8)
rotOp op v f = (r, flag (r == 0) fZ .|. flag co fC)
 where
  cy = f .&. fC /= 0
  (r, co) = case op of
    RotRlc -> (rotateL v 1, testBit v 7)
    RotRrc -> (rotateR v 1, testBit v 0)
    RotRl -> (shiftL v 1 .|. flag cy 1, testBit v 7)
    RotRr -> (shiftR v 1 .|. flag cy 0x80, testBit v 0)
    RotSla -> (shiftL v 1, testBit v 7)
    RotSra -> (shiftR v 1 .|. (v .&. 0x80), testBit v 0)
    RotSwap -> (rotateL v 4, False)
    RotSrl -> (shiftR v 1, testBit v 0)

-- | Apply a CB instruction to a value: the new value (for RES/SET/rotates)
-- and the new F.
cbApply :: CbInstr -> BitVector 8 -> BitVector 8 -> (BitVector 8, BitVector 8)
cbApply c v f = case c of
  Rot o _ -> rotOp o v f
  Bit n _ -> (v, flag (v .&. oneHot n == 0) fZ .|. fH .|. (f .&. fC))
  Res n _ -> (v .&. complement (oneHot n), f)
  Set n _ -> (v .|. oneHot n, f)

-- | Bit n set.
oneHot :: Index 8 -> BitVector 8
oneHot n = pack (reverse (map (== n) indicesI))

-- | SP + e and its flags (ADD SP,e; LD HL,SP+e).
spPlusE :: BitVector 16 -> BitVector 8 -> (BitVector 16, BitVector 8)
spPlusE sp e = (sp + sext e, flag (lo4 sp + lo4' e > 0xF) fH .|. flag (lo8u sp + zeroExtend (unpack e) > 0xFF) fC)
 where
  lo4 x = unpack (zeroExtend (slice d3 d0 x)) :: Unsigned 9
  lo4' x = unpack (zeroExtend (slice d3 d0 x)) :: Unsigned 9
  lo8u x = unpack (zeroExtend (slice d7 d0 x)) :: Unsigned 9

-- * The M-cycle step

out :: BusReq -> Bool -> CpuOut
out r d = CpuOut r Nothing d False

-- | End of an M-cycle.
{-# OPAQUE cpuCycle #-}
cpuCycle :: Cpu -> CpuIn -> (Cpu, CpuOut)
cpuCycle s inp = case cPhase s of
  PhLocked -> (s, out idleReq True)
  PhFetch -> fetchDone s inp
  PhExec -> execCycle s inp
  PhDispatch -> dispatchCycle s inp

pendingOf :: CpuIn -> BitVector 5
pendingOf inp = slice d4 d0 (ciIe inp) .&. ciIf inp

-- | The end of a fetch M-cycle (also a halted one).
fetchDone :: Cpu -> CpuIn -> (Cpu, CpuOut)
fetchDone s inp
  | cHalt s && pending == 0 = (s, out (rd (cPc s)) True)
  | cIme s && pending /= 0 =
      (s { cHalt = False, cIme = False, cPhase = PhDispatch, cMc = 1 }, out idleReq False)
  | otherwise =
      let s1 = s { cHalt = False, cIme = cIme s || cEi s, cEi = False, cEiNow = cEi s
                 , cPc = if cHaltBug s then cPc s else cPc s + 1, cHaltBug = False
                 , cInstr = decode (ciData inp), cMc = 0, cPhase = PhExec }
      in execCycle s1 inp
 where
  pending = pendingOf inp

-- | Interrupt dispatch: M-cycles 1 (idle), 2 (push PC high), 3 (push PC
-- low), 4 (idle) after the dropped fetch.
dispatchCycle :: Cpu -> CpuIn -> (Cpu, CpuOut)
dispatchCycle s inp = case cMc s of
  1 -> (s { cMc = 2 }, out (wr (cSp s - 1) (hi8 (cPc s))) False)
  2 ->
    let pend = pendingOf inp
        n = lowestSet pend
        vec = if pend == 0 then 0 else 0x40 .|. shiftL (zeroExtend (pack n)) 3
        s' = s { cMc = 3, cW = hi8 vec, cZ = lo8 vec }
    in (s', (out (wr (cSp s - 2) (lo8 (cPc s))) False)
              { coAck = if pend == 0 then Nothing else Just n })
  3 -> (s { cMc = 4, cSp = cSp s - 2 }, out idleReq False)
  _ -> done s { cPc = cW s ++# cZ s }

-- | The lowest set bit (priority encoder; 4 if none).
lowestSet :: BitVector 5 -> Index 5
lowestSet p
  | testBit p 0 = 0
  | testBit p 1 = 1
  | testBit p 2 = 2
  | testBit p 3 = 3
  | otherwise = 4

-- | Instruction done: the next M-cycle fetches at PC (or locks up).
done :: Cpu -> (Cpu, CpuOut)
done s = (s { cPhase = PhFetch, cMc = 0 }, out (rd (cPc s)) True)

-- | Go on with the next M-cycle of the instruction, which does this.
next :: Cpu -> BusReq -> (Cpu, CpuOut)
next s r = (s { cMc = cMc s + 1 }, out r False)

-- | Read the byte at PC in the next M-cycle (PC advances).
nextPc :: Cpu -> (Cpu, CpuOut)
nextPc s = next s { cPc = cPc s + 1 } (rd (cPc s))

-- | The end of M-cycle 'cMc' of the current instruction.
execCycle :: Cpu -> CpuIn -> (Cpu, CpuOut)
execCycle s inp = case cInstr s of
  Nop -> done s
  Stop -> if mc == 0 then nextPc s else done s
  LdNnSp -> case mc of
    0 -> nextPc s
    1 -> nextPc s { cZ = d }
    2 -> next s { cW = d } (wr (d ++# cZ s) (lo8 (cSp s)))
    3 -> next s (wr (cW s ++# cZ s + 1) (hi8 (cSp s)))
    _ -> done s
  Jr -> jr True
  JrCc c -> jr (condHolds s c)
  LdRpNn r -> case mc of
    0 -> nextPc s
    1 -> nextPc s { cZ = d }
    _ -> done (setRp r (d ++# cZ s) s)
  AddHlRp r -> case mc of
    0 -> next s idleReq
    _ ->
      let hl = getHL s
          v = getRp s r
          sm = (zeroExtend (unpack hl) :: Unsigned 17) + zeroExtend (unpack v)
          h = (zeroExtend (unpack (slice d11 d0 hl)) :: Unsigned 13) + zeroExtend (unpack (slice d11 d0 v)) > 0xFFF
      in done (setF ((getF s .&. fZ) .|. flag h fH .|. flag (sm > 0xFFFF) fC)
                 (setHL (pack (truncateB sm :: Unsigned 16)) s))
  LdMemA r -> case mc of
    0 -> let (a, s') = memAddr r in next s' (wr a (getA s))
    _ -> done s
  LdAMem r -> case mc of
    0 -> let (a, s') = memAddr r in next s' (rd a)
    _ -> done (setA d s)
  IncRp r -> case mc of
    0 -> next s idleReq
    _ -> done (setRp r (getRp s r + 1) s)
  DecRp r -> case mc of
    0 -> next s idleReq
    _ -> done (setRp r (getRp s r - 1) s)
  Inc r -> incDec r True
  Dec r -> incDec r False
  LdRN r -> case mc of
    0 -> nextPc s
    1 | r == HLInd -> next s (wr (getHL s) d)
      | otherwise -> done (setR r d s)
    _ -> done s
  Acc o -> let (a, f) = accOp o (getA s) (getF s) in done (setF f (setA a s))
  Halt
    | cIme s && cEiNow s && pending -> done s { cPc = cPc s - 1 } -- right after EI
    | cIme s -> done s { cHalt = True }
    | pending -> done s { cHaltBug = True }
    | otherwise -> done s { cHalt = True }
  LdRR dst src
    | src == HLInd -> case mc of
        0 -> next s (rd (getHL s))
        _ -> done (setR dst d s)
    | dst == HLInd -> case mc of
        0 -> next s (wr (getHL s) (getR s src))
        _ -> done s
    | otherwise -> done (setR dst (getR s src) s)
  Alu o r
    | r == HLInd -> case mc of
        0 -> next s (rd (getHL s))
        _ -> aluDone o d
    | otherwise -> aluDone o (getR s r)
  RetCc c -> case mc of
    0 -> next s idleReq
    1 | condHolds s c -> next s (rd (cSp s))
      | otherwise -> done s
    2 -> next s { cZ = d } (rd (cSp s + 1))
    3 -> next s { cW = d, cSp = cSp s + 2 } idleReq
    _ -> done s { cPc = cW s ++# cZ s }
  Ret -> ret False
  Reti -> ret True
  LdhNA -> case mc of
    0 -> nextPc s
    1 -> next s (wr (0xFF ++# d) (getA s))
    _ -> done s
  LdhAN -> case mc of
    0 -> nextPc s
    1 -> next s (rd (0xFF ++# d))
    _ -> done (setA d s)
  AddSpE -> case mc of
    0 -> nextPc s
    1 -> next s { cZ = d } idleReq
    2 -> next s idleReq
    _ -> let (r, f) = spPlusE (cSp s) (cZ s) in done (setF f s { cSp = r })
  LdHlSpE -> case mc of
    0 -> nextPc s
    1 -> next s { cZ = d } idleReq
    _ -> let (r, f) = spPlusE (cSp s) (cZ s) in done (setF f (setHL r s))
  Pop r -> case mc of
    0 -> next s (rd (cSp s))
    1 -> next s { cZ = d } (rd (cSp s + 1))
    _ -> done (setStk r (d ++# cZ s) s { cSp = cSp s + 2 })
  JpHl -> done s { cPc = getHL s }
  LdSpHl -> case mc of
    0 -> next s idleReq
    _ -> done s { cSp = getHL s }
  JpCc c -> jp (condHolds s c)
  Jp -> jp True
  LdhCA -> case mc of
    0 -> next s (wr (0xFF ++# getR s C) (getA s))
    _ -> done s
  LdhAC -> case mc of
    0 -> next s (rd (0xFF ++# getR s C))
    _ -> done (setA d s)
  LdNnA -> case mc of
    0 -> nextPc s
    1 -> nextPc s { cZ = d }
    2 -> next s (wr (d ++# cZ s) (getA s))
    _ -> done s
  LdANn -> case mc of
    0 -> nextPc s
    1 -> nextPc s { cZ = d }
    2 -> next s (rd (d ++# cZ s))
    _ -> done (setA d s)
  CallCc c -> call (condHolds s c)
  Call -> call True
  Push r -> case mc of
    0 -> next s { cW = hi8 (getStk s r), cZ = lo8 (getStk s r) } idleReq
    1 -> next s (wr (cSp s - 1) (cW s))
    2 -> next s (wr (cSp s - 2) (cZ s))
    _ -> done s { cSp = cSp s - 2 }
  AluN o -> case mc of
    0 -> nextPc s
    _ -> aluDone o d
  Rst n -> case mc of
    0 -> next s idleReq
    1 -> next s (wr (cSp s - 1) (hi8 (cPc s)))
    2 -> next s (wr (cSp s - 2) (lo8 (cPc s)))
    _ -> done s { cSp = cSp s - 2, cPc = shiftL (zeroExtend (pack n)) 3 }
  PrefixCb -> case mc of
    0 -> nextPc s
    1 -> let c = decodeCb d
             r = cbReg c
         in if r == HLInd then next s { cCb = c } (rd (getHL s))
            else let (v, f) = cbApply c (getR s r) (getF s) in done (setF f (setR r v s))
    2 -> let (v, f) = cbApply (cCb s) d (getF s)
         in case cCb s of
              Bit _ _ -> done (setF f s)
              _ -> next (setF f s) (wr (getHL s) v)
    _ -> done s
  Di -> done s { cIme = False, cEi = False }
  Ei -> done s { cEi = True }
  Ill _ -> (s { cPhase = PhLocked }, out idleReq True)
 where
  mc = cMc s
  d = ciData inp
  pending = pendingOf inp /= 0

  jr taken = case mc of
    0 -> nextPc s
    1 | taken -> next s { cZ = d } idleReq
      | otherwise -> done s
    _ -> done s { cPc = cPc s + sext (cZ s) }

  jp taken = case mc of
    0 -> nextPc s
    1 -> nextPc s { cZ = d }
    2 | taken -> next s { cW = d } idleReq
      | otherwise -> done s
    _ -> done s { cPc = cW s ++# cZ s }

  call taken = case mc of
    0 -> nextPc s
    1 -> nextPc s { cZ = d }
    2 | taken -> next s { cW = d } idleReq
      | otherwise -> done s
    3 -> next s (wr (cSp s - 1) (hi8 (cPc s)))
    4 -> next s (wr (cSp s - 2) (lo8 (cPc s)))
    _ -> done s { cSp = cSp s - 2, cPc = cW s ++# cZ s }

  ret reti = case mc of
    0 -> next s (rd (cSp s))
    1 -> next s { cZ = d } (rd (cSp s + 1))
    2 -> next s { cW = d, cSp = cSp s + 2 } idleReq
    _ -> done s { cPc = cW s ++# cZ s, cIme = cIme s || reti }

  aluDone o v = let (a, f) = alu o (getA s) v (getF s) in done (setF f (setA a s))

  incDec r up = case (r == HLInd, mc) of
    (False, _) -> let (v, f) = incDecV (getR s r) in done (setF f (setR r v s))
    (True, 0) -> next s (rd (getHL s))
    (True, 1) -> let (v, f) = incDecV d in next (setF f s) (wr (getHL s) v)
    _ -> done s
   where
    incDecV v
      | up = let r' = v + 1 in (r', flag (r' == 0) fZ .|. flag (v .&. 0xF == 0xF) fH .|. (getF s .&. fC))
      | otherwise = let r' = v - 1 in (r', flag (r' == 0) fZ .|. fN .|. flag (v .&. 0xF == 0) fH .|. (getF s .&. fC))

  memAddr r = case r of
    MemBC -> (pair s B C, s)
    MemDE -> (pair s D E, s)
    MemHLI -> (getHL s, setHL (getHL s + 1) s)
    MemHLD -> (getHL s, setHL (getHL s - 1) s)

cbReg :: CbInstr -> R8
cbReg c = case c of
  Rot _ r -> r
  Bit _ r -> r
  Res _ r -> r
  Set _ r -> r

-- | The core as a Mealy machine: 'Just' input at the end of each M-cycle.
-- Outputs the request of the current M-cycle (registered), and the
-- 'CpuOut' of an M-cycle's end in the same clock as its input.
cpuMealy
  :: HiddenClockResetEnable dom
  => Bool -- ^ header checksum non-zero
  -> Signal dom (Maybe CpuIn)
  -> Signal dom (BusReq, Maybe CpuOut)
cpuMealy hc = mealy go (cpuInit hc)
 where
  go (s, req) Nothing = ((s, req), (req, Nothing))
  go (s, req) (Just i) = let (s', o) = cpuCycle s i in ((s', coReq o), (req, Just o))

-- | The architectural registers, for comparing with the spec.
data CpuRegs = CpuRegs
  { crA, crF, crB, crC, crD, crE, crH, crL :: BitVector 8
  , crSp, crPc :: BitVector 16
  , crIme :: Bool
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFDataX

cpuRegs :: Cpu -> CpuRegs
cpuRegs s = CpuRegs (getA s) (getF s) (getR s B) (getR s C) (getR s D) (getR s E)
                    (getR s H) (getR s L) (cSp s) (cPc s) (cIme s)
