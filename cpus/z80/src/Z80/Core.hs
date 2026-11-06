{-# LANGUAGE StrictData #-}
{-|
The Z80 in Clash (PLAN.md step 5.5; docs/systems/gamegear.md): the
machine cycles of "Z80.Spec", as a state machine with one transition per
machine cycle, plus a T-state wrapper with WAIT for the machine around it.
The lockstep tests (@z80-lockstep@) compare it with the spec request by
request; it also runs Tom Harte's SingleStepTests on its own.

__The machine cycle.__ The pending request ('cReq', a register) says what
the cycle does: an opcode fetch (4 T), a memory read or write (3 T), an
I/O read or write (4 T), n internal T-states, or the interrupt
acknowledge (6 T). Those are exactly the calls the spec makes on its bus,
in the same order. When the cycle is over, 'cpuStep' gets its data
('ciData', for the reads) and returns the next state with the next
request. Nothing in a request depends on the data of its own cycle.

__Interrupts__ as in the spec: the lines ('ciInt', 'ciNmi') are looked
at only when an instruction (or an interrupt sequence, or a halted fetch)
ends: an /NMI edge since the last instruction end wins, then /INT with
IFF1 set unless the instruction was EI. A prefixed instruction is one
instruction.

__The T-state wrapper__ ('Z80T', 'z80Tick') counts the request's T-states
and completes it after its last one; while WAIT is asserted on that last
T-state of a fetch, a read, a write, an I/O cycle or the acknowledge, the
cycle is stretched by one T-state at a time (the Z80 inserts wait states;
a machine holds WAIT until its data is there).

The ALU and flag rules are written here again, independently of the
spec's (both are checked against SingleStepTests).
-}
module Z80.Core
  ( -- * Requests
    Kind (..)
  , Req (..)
  , CpuIn (..)
    -- * The CPU
  , Phase (..)
  , Table (..)
  , Cpu (..)
  , cpuPowerOn
  , cpuStep
    -- * T-states
  , Z80T (..)
  , z80PowerOn
  , z80Tick
  , z80Done
  ) where

import Clash.Prelude

-- | What a machine cycle does.
data Kind = KFetch | KRead | KWrite | KIn | KOut | KIdle | KIntAck
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFDataX

-- | One machine cycle: its kind, address, data (writes) and length in
-- T-states.
data Req = Req
  { rqKind :: Kind
  , rqAddr :: Unsigned 16
  , rqData :: Unsigned 8
  , rqLen  :: Unsigned 3
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFDataX

-- | The end of a machine cycle: the data read (fetch, read, I/O read,
-- acknowledge), the interrupt lines (True = asserted).
data CpuIn = CpuIn
  { ciData :: Unsigned 8
  , ciInt  :: Bool
  , ciNmi  :: Bool
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFDataX

-- | The opcode tables.
data Table = TMain | TCB | TED | TIxCB
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFDataX

-- | What the pending cycle belongs to.
data Phase
  = PFetch Table -- ^ an opcode fetch: its byte is an opcode of this table
  | PExec Table  -- ^ step 'cStep' of opcode 'cOp'
  | PHalt        -- ^ a halted CPU's fetch
  | PNmi         -- ^ step 'cStep' of the NMI sequence
  | PInt         -- ^ step 'cStep' of the /INT sequence
  | PEnd         -- ^ the instruction is over ('cpuStep' starts the next)
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFDataX

data Cpu = Cpu
  { cA, cF, cB, cC, cD, cE, cH, cL :: Unsigned 8
  , cA', cF', cB', cC', cD', cE', cH', cL' :: Unsigned 8
  , cIX, cIY, cSP, cPC, cWZ :: Unsigned 16
  , cI, cR :: Unsigned 8
  , cIFF1, cIFF2 :: Bool
  , cIM :: Unsigned 2
  , cQ :: Unsigned 8      -- ^ F if the last instruction changed the flags, else 0
  , cQNew :: Bool         -- ^ this instruction changed the flags
  , cEI :: Bool           -- ^ the last instruction was EI
  , cP :: Bool            -- ^ the last instruction was LD A,I or LD A,R
  , cPNew :: Bool
  , cHalted :: Bool
  , cNmiLine :: Bool      -- ^ /NMI at the last instruction end
  , cNew :: Bool          -- ^ the pending cycle starts an instruction
  -- the sequencer
  , cReq :: Req
  , cPhase :: Phase
  , cIx :: Unsigned 2     -- ^ 0 HL, 1 IX, 2 IY
  , cOp :: Unsigned 8
  , cStep :: Unsigned 4
  , cAddr :: Unsigned 16  -- ^ a memory operand's address
  , cTmp :: Unsigned 8    -- ^ data held between cycles
  , cTmp2 :: Unsigned 8
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFDataX

-- | All registers 0, interrupts disabled, IM 0; the first cycle fetches
-- at 0000h.
cpuPowerOn :: Cpu
cpuPowerOn = Cpu
  { cA = 0, cF = 0, cB = 0, cC = 0, cD = 0, cE = 0, cH = 0, cL = 0
  , cA' = 0, cF' = 0, cB' = 0, cC' = 0, cD' = 0, cE' = 0, cH' = 0, cL' = 0
  , cIX = 0, cIY = 0, cSP = 0, cPC = 0, cWZ = 0, cI = 0, cR = 0
  , cIFF1 = False, cIFF2 = False, cIM = 0, cQ = 0, cQNew = False
  , cEI = False, cP = False, cPNew = False, cHalted = False, cNmiLine = False
  , cNew = True
  , cReq = Req KFetch 0 0 4, cPhase = PFetch TMain, cIx = 0, cOp = 0
  , cStep = 0, cAddr = 0, cTmp = 0, cTmp2 = 0
  }

-- * Flags

fC, fN, fP, fX, fH, fY, fZ, fS :: Unsigned 8
fC = 0x01
fN = 0x02
fP = 0x04
fX = 0x08
fH = 0x10
fY = 0x20
fZ = 0x40
fS = 0x80

flag :: Bool -> Unsigned 8 -> Unsigned 8
flag b m = if b then m else 0

par :: Unsigned 8 -> Bool
par v = reduceXor (pack v) == 0

-- | S, Z, X, Y of a result.
sz :: Unsigned 8 -> Unsigned 8
sz v = (v .&. (fS .|. fX .|. fY)) .|. flag (v == 0) fZ

-- | S, Z, X, Y and parity.
szp :: Unsigned 8 -> Unsigned 8
szp v = sz v .|. flag (par v) fP

hi8, lo8 :: Unsigned 16 -> Unsigned 8
hi8 v = resize (v `shiftR` 8)
lo8 = resize

w16 :: Unsigned 8 -> Unsigned 8 -> Unsigned 16
w16 h l = (resize h `shiftL` 8) .|. resize l

sext :: Unsigned 8 -> Unsigned 16
sext d = bitCoerce (resize (bitCoerce d :: Signed 8) :: Signed 16)

-- | ADD ADC SUB SBC AND XOR OR CP: result and flags.
alu :: Unsigned 3 -> Unsigned 8 -> Unsigned 8 -> Unsigned 8 -> (Unsigned 8, Unsigned 8)
alu o a b f = case o of
  4 -> let r = a .&. b in (r, szp r .|. fH)
  5 -> let r = a `xor` b in (r, szp r)
  6 -> let r = a .|. b in (r, szp r)
  _ ->
    let carryIn = (o == 1 || o == 3) && testBit f 0
        isSub = o >= 2
        a9 = resize a :: Unsigned 9
        b9 = resize b :: Unsigned 9
        c9 = if carryIn then 1 else 0
        r9 = if isSub then a9 - b9 - c9 else a9 + b9 + c9
        r = resize r9 :: Unsigned 8
        half = (a `xor` b `xor` r) .&. 0x10 /= 0
        ovf = if isSub
          then (a `xor` b) .&. (a `xor` r) .&. 0x80 /= 0
          else complement (a `xor` b) .&. (a `xor` r) .&. 0x80 /= 0
        xy = (if o == 7 then b else r) .&. (fX .|. fY)
    in ( r
       , (r .&. fS) .|. flag (r == 0) fZ .|. xy .|. flag half fH .|. flag ovf fP
         .|. flag isSub fN .|. flag (testBit r9 8) fC )

incF, decF :: Unsigned 8 -> Unsigned 8 -> (Unsigned 8, Unsigned 8)
incF f v = let r = v + 1 in
  (r, (f .&. fC) .|. sz r .|. flag (r .&. 0x0F == 0) fH .|. flag (r == 0x80) fP)
decF f v = let r = v - 1 in
  (r, (f .&. fC) .|. sz r .|. fN .|. flag (r .&. 0x0F == 0x0F) fH .|. flag (r == 0x7F) fP)

daaF :: Unsigned 8 -> Unsigned 8 -> (Unsigned 8, Unsigned 8)
daaF a f =
  let n = testBit f 1
      h = testBit f 4
      cy = testBit f 0
      lo = a .&. 0x0F
      cy' = cy || a > 0x99
      corr = (if h || lo > 9 then 0x06 else 0) .|. (if cy' then 0x60 else 0)
      r = if n then a - corr else a + corr
      h' = if n then h && lo < 6 else lo > 9
  in (r, szp r .|. (f .&. fN) .|. flag h' fH .|. flag cy' fC)

-- | The CB rotates/shifts (group 0), RES (2), SET (3).
cbOp :: Unsigned 2 -> Unsigned 3 -> Unsigned 8 -> Unsigned 8 -> (Unsigned 8, Unsigned 8)
cbOp g y v f = case g of
  0 ->
    let cy = testBit f 0
        b7 = testBit v 7
        b0 = testBit v 0
        (r, co) = case y of
          0 -> (rotateL v 1, b7)
          1 -> (rotateR v 1, b0)
          2 -> (shiftL v 1 .|. flag cy 1, b7)
          3 -> (shiftR v 1 .|. flag cy 0x80, b0)
          4 -> (shiftL v 1, b7)
          5 -> (shiftR v 1 .|. (v .&. 0x80), b0)
          6 -> (shiftL v 1 .|. 1, b7)
          _ -> (shiftR v 1, b0)
    in (r, szp r .|. flag co fC)
  2 -> (clearBit v (fromEnum y), f)
  _ -> (setBit v (fromEnum y), f)

bitF :: Unsigned 3 -> Unsigned 8 -> Unsigned 8 -> Unsigned 8 -> Unsigned 8
bitF y v xy f =
  let s = testBit v (fromEnum y)
  in (f .&. fC) .|. fH .|. flag (not s) (fZ .|. fP) .|. flag (s && y == 7) fS
     .|. (xy .&. (fX .|. fY))

-- * Registers

hlIx :: Unsigned 2 -> Cpu -> Unsigned 16
hlIx ix c = case ix of
  1 -> cIX c
  2 -> cIY c
  _ -> w16 (cH c) (cL c)

setHlIx :: Unsigned 2 -> Unsigned 16 -> Cpu -> Cpu
setHlIx ix v c = case ix of
  1 -> c { cIX = v }
  2 -> c { cIY = v }
  _ -> c { cH = hi8 v, cL = lo8 v }

-- | Register field r (not 6) under index mode ix.
getR :: Unsigned 2 -> Unsigned 3 -> Cpu -> Unsigned 8
getR ix r c = case r of
  0 -> cB c
  1 -> cC c
  2 -> cD c
  3 -> cE c
  4 -> hi8 (hlIx ix c)
  5 -> lo8 (hlIx ix c)
  6 -> 0
  _ -> cA c

setR :: Unsigned 2 -> Unsigned 3 -> Unsigned 8 -> Cpu -> Cpu
setR ix r v c = case r of
  0 -> c { cB = v }
  1 -> c { cC = v }
  2 -> c { cD = v }
  3 -> c { cE = v }
  4 -> setHlIx ix (w16 v (lo8 (hlIx ix c))) c
  5 -> setHlIx ix (w16 (hi8 (hlIx ix c)) v) c
  6 -> c
  _ -> c { cA = v }

getRP :: Unsigned 2 -> Unsigned 2 -> Cpu -> Unsigned 16
getRP ix p c = case p of
  0 -> w16 (cB c) (cC c)
  1 -> w16 (cD c) (cE c)
  2 -> hlIx ix c
  _ -> cSP c

setRP :: Unsigned 2 -> Unsigned 2 -> Unsigned 16 -> Cpu -> Cpu
setRP ix p v c = case p of
  0 -> c { cB = hi8 v, cC = lo8 v }
  1 -> c { cD = hi8 v, cE = lo8 v }
  2 -> setHlIx ix v c
  _ -> c { cSP = v }

getRP2 :: Unsigned 2 -> Unsigned 2 -> Cpu -> Unsigned 16
getRP2 ix p c = if p == 3 then w16 (cA c) (cF c) else getRP ix p c

setRP2 :: Unsigned 2 -> Unsigned 2 -> Unsigned 16 -> Cpu -> Cpu
setRP2 ix p v c = if p == 3 then c { cA = hi8 v, cF = lo8 v } else setRP ix p v c

cond :: Unsigned 3 -> Unsigned 8 -> Bool
cond y f = case y of
  0 -> not (testBit f 6)
  1 -> testBit f 6
  2 -> not (testBit f 0)
  3 -> testBit f 0
  4 -> not (testBit f 2)
  5 -> testBit f 2
  6 -> not (testBit f 7)
  _ -> testBit f 7

-- | Flags from an instruction that changes them (Q follows).
setF :: Unsigned 8 -> Cpu -> Cpu
setF f c = c { cF = f, cQNew = True }

ir :: Cpu -> Unsigned 16
ir c = w16 (cI c) (cR c)

incR :: Unsigned 8 -> Unsigned 8
incR r = (r .&. 0x80) .|. ((r + 1) .&. 0x7F)

-- * Requests

fetchAt, readAt, inAt :: Unsigned 16 -> Req
fetchAt a = Req KFetch a 0 4
readAt a = Req KRead a 0 3
inAt a = Req KIn a 0 4

writeAt, outAt :: Unsigned 16 -> Unsigned 8 -> Req
writeAt a v = Req KWrite a v 3
outAt a v = Req KOut a v 4

idleAt :: Unsigned 16 -> Unsigned 3 -> Req
idleAt a n = Req KIdle a 0 n

-- | Continue with step k, issuing req.
go :: Unsigned 4 -> Req -> Cpu -> Cpu
go k rq c = c { cStep = k, cReq = rq }

-- | Read the byte at PC (PC + 1), as step k.
goImm :: Unsigned 4 -> Cpu -> Cpu
goImm k c = go k (readAt (cPC c)) c { cPC = cPC c + 1 }

done :: Cpu -> Cpu
done c = c { cPhase = PEnd }

-- * The machine cycle

cpuStep :: CpuIn -> Cpu -> Cpu
cpuStep ci c0 =
  let d = ciData ci
      c1 = case cPhase c0 of
        PFetch t -> fetched t d c0
        PExec TMain -> execMain d c0
        PExec TCB -> execCB d c0
        PExec TED -> execED d c0
        PExec TIxCB -> execIxCB d c0
        PHalt -> done c0 { cR = incR (cR c0), cEI = cEI c0 && not (cNew c0), cNew = False }
        PNmi -> execNmi d c0
        PInt -> execInt d c0
        PEnd -> c0
  in if cPhase c1 == PEnd then endInstr ci c1 else c1

-- | An instruction (or sequence) is over: Q and the LD A,I/R flag, then
-- what comes next.
endInstr :: CpuIn -> Cpu -> Cpu
endInstr ci c =
  let c1 = c { cQ = if cQNew c then cF c else 0, cQNew = False
             , cP = cPNew c, cPNew = False
             , cNmiLine = ciNmi ci, cIx = 0, cNew = True, cStep = 0 }
      pc = cPC c
  in if ciNmi ci && not (cNmiLine c)
       then c1 { cPhase = PNmi, cReq = fetchAt pc }
       else if ciInt ci && cIFF1 c && not (cEI c)
         then c1 { cPhase = PInt, cReq = Req KIntAck 0 0 6 }
         else if cHalted c
           then c1 { cPhase = PHalt, cReq = fetchAt pc }
           else c1 { cPhase = PFetch TMain, cReq = fetchAt pc }

-- | An opcode fetch is over: d is the opcode.
fetched :: Table -> Unsigned 8 -> Cpu -> Cpu
fetched t d c0 =
  let c = c0 { cPC = cPC c0 + 1, cR = incR (cR c0)
             , cEI = cEI c0 && not (cNew c0), cNew = False }
      fetchNext t' c' = c' { cPhase = PFetch t', cReq = fetchAt (cPC c') }
  in case t of
    TMain
      | d == 0xDD || d == 0xFD -> fetchNext TMain c { cIx = if d == 0xDD then 1 else 2, cQ = 0 }
      | d == 0xED -> fetchNext TED c { cIx = 0 }
      | d == 0xCB && cIx c == 0 -> fetchNext TCB c
      | d == 0xCB -> goImm 1 c { cPhase = PExec TIxCB, cOp = d }
      | otherwise -> execMain 0 c { cPhase = PExec TMain, cOp = d, cStep = 0 }
    TCB -> execCB 0 c { cPhase = PExec TCB, cOp = d, cStep = 0 }
    TED -> execED 0 c { cPhase = PExec TED, cOp = d, cStep = 0 }
    TIxCB -> c -- not reached: DDCB's bytes are reads

-- * The main table

execMain :: Unsigned 8 -> Cpu -> Cpu
execMain d c = case x of
  0 -> case z of
    0 -> case y of
      0 -> done c
      1 -> done c { cA = cA' c, cF = cF' c, cA' = cA c, cF' = cF c }
      2 -> case k of        -- DJNZ
        0 -> go 1 (idleAt (ir c) 1) c
        1 -> goImm 2 c
        2 -> let b = cB c - 1 in
             if b /= 0 then go 3 (idleAt (cPC c - 1) 5) c { cB = b, cTmp = d } else done c { cB = b }
        _ -> jump (cTmp c)
      3 -> case k of        -- JR
        0 -> goImm 1 c
        1 -> go 2 (idleAt (cPC c - 1) 5) c { cTmp = d }
        _ -> jump (cTmp c)
      _ -> case k of        -- JR cc
        0 -> goImm 1 c
        1 -> if cond (y - 4) (cF c) then go 2 (idleAt (cPC c - 1) 5) c { cTmp = d } else done c
        _ -> jump (cTmp c)
    1 -> if q == 0
      then case k of        -- LD rp,nn
        0 -> goImm 1 c
        1 -> goImm 2 c { cTmp = d }
        _ -> done (setRP ix p (w16 d (cTmp c)) c)
      else case k of        -- ADD HL,rp
        0 -> go 1 (idleAt (ir c) 7) c
        _ ->
          let hl = hlIx ix c
              v = getRP ix p c
              r17 = resize hl + resize v :: Unsigned 17
              r16 = resize r17 :: Unsigned 16
              half = (hl `xor` v `xor` r16) .&. 0x1000 /= 0
              f = (cF c .&. (fS .|. fZ .|. fP)) .|. (hi8 r16 .&. (fX .|. fY))
                  .|. flag half fH .|. flag (testBit r17 16) fC
          in done (setHlIx ix r16 (setF f c { cWZ = hl + 1 }))
    2 -> case (q, p) of
      (0, 0) -> storeA (w16 (cB c) (cC c))
      (0, 1) -> storeA (w16 (cD c) (cE c))
      (0, 2) -> case k of   -- LD (nn),HL
        0 -> goImm 1 c
        1 -> goImm 2 c { cTmp = d }
        2 -> let a = w16 d (cTmp c) in go 3 (writeAt a (lo8 (hlIx ix c))) c { cAddr = a }
        3 -> go 4 (writeAt (cAddr c + 1) (hi8 (hlIx ix c))) c
        _ -> done c { cWZ = cAddr c + 1 }
      (0, _) -> case k of   -- LD (nn),A
        0 -> goImm 1 c
        1 -> goImm 2 c { cTmp = d }
        2 -> storeA' (w16 d (cTmp c))
        _ -> done c
      (_, 0) -> loadA (w16 (cB c) (cC c))
      (_, 1) -> loadA (w16 (cD c) (cE c))
      (_, 2) -> case k of   -- LD HL,(nn)
        0 -> goImm 1 c
        1 -> goImm 2 c { cTmp = d }
        2 -> let a = w16 d (cTmp c) in go 3 (readAt a) c { cAddr = a }
        3 -> go 4 (readAt (cAddr c + 1)) c { cTmp = d }
        _ -> done (setHlIx ix (w16 d (cTmp c)) c { cWZ = cAddr c + 1 })
      _ -> case k of        -- LD A,(nn)
        0 -> goImm 1 c
        1 -> goImm 2 c { cTmp = d }
        2 -> let a = w16 d (cTmp c) in go 3 (readAt a) c { cAddr = a }
        _ -> done c { cA = d, cWZ = cAddr c + 1 }
    3 -> case k of          -- INC rp / DEC rp
      0 -> go 1 (idleAt (ir c) 2) c
      _ -> let v = getRP ix p c in done (setRP ix p (if q == 0 then v + 1 else v - 1) c)
    4 -> modify (incF (cF c))
    5 -> modify (decF (cF c))
    6 -> if y /= 6
      then case k of        -- LD r,n
        0 -> goImm 1 c
        _ -> done (setR ix y d c)
      else if ix == 0
        then case k of      -- LD (HL),n
          0 -> goImm 1 c
          1 -> go 2 (writeAt (hlIx 0 c) d) c
          _ -> done c
        else case k of      -- LD (IX+d),n
          0 -> goImm 1 c
          1 -> goImm 2 c { cTmp = d }
          2 -> go 3 (idleAt (cPC c - 1) 2) c { cTmp2 = d }
          3 -> let a = hlIx ix c + sext (cTmp c) in go 4 (writeAt a (cTmp2 c)) c { cWZ = a }
          _ -> done c
    _ ->
      let a = cA c
          f = cF c
          keep = f .&. (fS .|. fZ .|. fP)
          xy v = v .&. (fX .|. fY)
          rot a' cy = done (setF (keep .|. xy a' .|. flag cy fC) c { cA = a' })
      in case y of
        0 -> rot (rotateL a 1) (testBit a 7)
        1 -> rot (rotateR a 1) (testBit a 0)
        2 -> rot (shiftL a 1 .|. (f .&. fC)) (testBit a 7)
        3 -> rot (shiftR a 1 .|. flag (testBit f 0) 0x80) (testBit a 0)
        4 -> let (a', f') = daaF a f in done (setF f' c { cA = a' })
        5 -> let a' = complement a in
             done (setF ((f .&. (fS .|. fZ .|. fP .|. fC)) .|. fH .|. fN .|. xy a') c { cA = a' })
        6 -> done (setF (keep .|. fC .|. xy ((cQ c `xor` f) .|. a)) c)
        _ -> done (setF (keep .|. xy ((cQ c `xor` f) .|. a) .|. (if testBit f 0 then fH else fC)) c)
  1 -> if y == 6 && z == 6
    then done c { cHalted = True }
    else if y == 6
      then mem (\a -> go 10 (writeAt a (getR 0 z c)) c) (done c)      -- LD (HL),r
      else if z == 6
        then mem (\a -> go 10 (readAt a) c) (done (setR 0 y d c))       -- LD r,(HL)
        else done (setR ix y (getR ix z c) c)
  2 -> if z == 6
    then mem (\a -> go 10 (readAt a) c) (aluA d)
    else aluA (getR ix z c)
  _ -> case z of
    0 -> case k of          -- RET cc
      0 -> go 1 (idleAt (ir c) 1) c
      1 -> if cond y (cF c) then go 2 (readAt (cSP c)) c else done c
      2 -> go 3 (readAt (cSP c + 1)) c { cTmp = d }
      _ -> ret d
    1 -> if q == 0
      then case k of        -- POP
        0 -> go 1 (readAt (cSP c)) c
        1 -> go 2 (readAt (cSP c + 1)) c { cTmp = d }
        _ -> done (setRP2 ix p (w16 d (cTmp c)) c { cSP = cSP c + 2 })
      else case p of
        0 -> case k of      -- RET
          0 -> go 2 (readAt (cSP c)) c
          2 -> go 3 (readAt (cSP c + 1)) c { cTmp = d }
          _ -> ret d
        1 -> done c { cB = cB' c, cC = cC' c, cD = cD' c, cE = cE' c, cH = cH' c, cL = cL' c
                    , cB' = cB c, cC' = cC c, cD' = cD c, cE' = cE c, cH' = cH c, cL' = cL c }
        2 -> done c { cPC = hlIx ix c }
        _ -> case k of      -- LD SP,HL
          0 -> go 1 (idleAt (ir c) 2) c
          _ -> done c { cSP = hlIx ix c }
    2 -> case k of          -- JP cc,nn
      0 -> goImm 1 c
      1 -> goImm 2 c { cTmp = d }
      _ -> let a = w16 d (cTmp c) in done c { cWZ = a, cPC = if cond y (cF c) then a else cPC c }
    3 -> case y of
      0 -> case k of        -- JP nn
        0 -> goImm 1 c
        1 -> goImm 2 c { cTmp = d }
        _ -> let a = w16 d (cTmp c) in done c { cWZ = a, cPC = a }
      2 -> case k of        -- OUT (n),A
        0 -> goImm 1 c
        1 -> go 2 (outAt (w16 (cA c) d) (cA c)) c { cTmp = d }
        _ -> done c { cWZ = w16 (cA c) (cTmp c + 1) }
      3 -> case k of        -- IN A,(n)
        0 -> goImm 1 c
        1 -> let a = w16 (cA c) d in go 2 (inAt a) c { cAddr = a }
        _ -> done c { cA = d, cWZ = cAddr c + 1 }
      4 -> case k of        -- EX (SP),HL
        0 -> go 1 (readAt (cSP c)) c
        1 -> go 2 (readAt (cSP c + 1)) c { cTmp = d }
        2 -> go 3 (idleAt (cSP c + 1) 1) c { cTmp2 = d }
        3 -> go 4 (writeAt (cSP c + 1) (hi8 (hlIx ix c))) c
        4 -> go 5 (writeAt (cSP c) (lo8 (hlIx ix c))) c
        5 -> go 6 (idleAt (cSP c) 2) c
        _ -> let v = w16 (cTmp2 c) (cTmp c) in done (setHlIx ix v c { cWZ = v })
      5 -> done c { cD = cH c, cE = cL c, cH = cD c, cL = cE c }
      6 -> done c { cIFF1 = False, cIFF2 = False }
      7 -> done c { cIFF1 = True, cIFF2 = True, cEI = True }
      _ -> done c -- CB: handled by the fetch
    4 -> case k of          -- CALL cc,nn
      0 -> goImm 1 c
      1 -> goImm 2 c { cTmp = d }
      2 -> let a = w16 d (cTmp c) in
           if cond y (cF c) then go 3 (idleAt (cPC c - 1) 1) c { cWZ = a } else done c { cWZ = a }
      _ -> call k
    5 -> if q == 0
      then case k of        -- PUSH
        0 -> go 1 (idleAt (ir c) 1) c
        1 -> go 2 (writeAt (cSP c - 1) (hi8 (getRP2 ix p c))) c
        2 -> go 3 (writeAt (cSP c - 2) (lo8 (getRP2 ix p c))) c
        _ -> done c { cSP = cSP c - 2 }
      else case k of        -- CALL nn
        0 -> goImm 1 c
        1 -> goImm 2 c { cTmp = d }
        2 -> go 3 (idleAt (cPC c - 1) 1) c { cWZ = w16 d (cTmp c) }
        _ -> call k
    6 -> case k of          -- ALU A,n
      0 -> goImm 1 c
      _ -> aluA d
    _ -> case k of          -- RST
      0 -> go 1 (idleAt (ir c) 1) c
      1 -> go 2 (writeAt (cSP c - 1) (hi8 (cPC c))) c
      2 -> go 3 (writeAt (cSP c - 2) (lo8 (cPC c))) c
      _ -> let a = resize (cOp c .&. 0x38) in done c { cSP = cSP c - 2, cPC = a, cWZ = a }
 where
  op = cOp c
  k = cStep c
  ix = cIx c
  x = op `shiftR` 6
  y = resize (op `shiftR` 3) :: Unsigned 3
  z = resize op :: Unsigned 3
  p = resize (y `shiftR` 1) :: Unsigned 2
  q = y .&. 1

  jump dd = let a = cPC c + sext dd in done c { cPC = a, cWZ = a }

  storeA a = case k of
    0 -> storeA' a
    _ -> done c
  storeA' a = go 10 (writeAt a (cA c)) c { cWZ = w16 (cA c) (lo8 a + 1) }

  loadA a = case k of
    0 -> go 1 (readAt a) c { cAddr = a }
    _ -> done c { cA = d, cWZ = cAddr c + 1 }

  ret dd = let a = w16 dd (cTmp c) in done c { cSP = cSP c + 2, cPC = a, cWZ = a }

  -- the pushes of CALL, from step 3 on
  call kk = case kk of
    3 -> go 4 (writeAt (cSP c - 1) (hi8 (cPC c))) c
    4 -> go 5 (writeAt (cSP c - 2) (lo8 (cPC c))) c
    _ -> done c { cSP = cSP c - 2, cPC = cWZ c }

  aluA v = let (a', f') = alu y (cA c) v (cF c) in
    done (setF f' (if y == 7 then c else c { cA = a' }))

  -- A memory operand: steps 0-2 find its address (HL, or IX/IY + d with
  -- five internal T-states), then 'start' issues the access as step 10;
  -- step 10 onwards is 'after' (or the instruction's own steps).
  mem start after
    | k == 10 = after
    | ix == 0 && k == 0 = start (hlIx 0 c)
    | k == 0 = goImm 1 c
    | k == 1 = let a = hlIx ix c + sext d in go 2 (idleAt (cPC c - 1) 5) c { cAddr = a, cWZ = a }
    | otherwise = start (cAddr c)

  -- INC/DEC r or (HL)/(IX+d): read, one internal T-state, write.
  modify fl
    | y /= 6 = let (v', f') = fl (getR ix y c) in done (setF f' (setR ix y v' c))
    | k < 10 = mem (\a -> go 10 (readAt a) c { cAddr = a }) c
    | k == 10 = go 11 (idleAt (cAddr c) 1) c { cTmp = d }
    | k == 11 = let (v', f') = fl (cTmp c) in go 12 (writeAt (cAddr c) v') (setF f' c)
    | otherwise = done c

-- * CB

execCB :: Unsigned 8 -> Cpu -> Cpu
execCB d c
  | z /= 6 = let v = getR 0 z c in
      if g == 1 then done (setF (bitF y v v (cF c)) c)
      else let (v', f') = cbOp g y v (cF c) in
           done ((if g == 0 then setF f' else id) (setR 0 z v' c))
  | otherwise = case k of
      0 -> go 1 (readAt hl) c
      1 -> go 2 (idleAt hl 1) c { cTmp = d }
      2 -> if g == 1
             then done (setF (bitF y (cTmp c) (hi8 (cWZ c)) (cF c)) c)
             else let (v', f') = cbOp g y (cTmp c) (cF c) in
                  go 3 (writeAt hl v') ((if g == 0 then setF f' else id) c)
      _ -> done c
 where
  op = cOp c
  k = cStep c
  y = resize (op `shiftR` 3) :: Unsigned 3
  z = resize op :: Unsigned 3
  g = resize (op `shiftR` 6) :: Unsigned 2
  hl = hlIx 0 c

-- | DD CB d op: step 1 has read d, step 2 reads op, then two internal
-- T-states, the read, one internal T-state and (not BIT) the write; the
-- undocumented forms also copy the result into a register.
execIxCB :: Unsigned 8 -> Cpu -> Cpu
execIxCB d c = case k of
  1 -> goImm 2 c { cTmp = d }
  2 -> go 3 (idleAt (cPC c - 1) 2) c { cOp = d }
  3 -> let a = hlIx (cIx c) c + sext (cTmp c) in go 4 (readAt a) c { cAddr = a, cWZ = a }
  4 -> go 5 (idleAt (cAddr c) 1) c { cTmp2 = d }
  5 -> if g == 1
         then done (setF (bitF y (cTmp2 c) (hi8 (cAddr c)) (cF c)) c)
         else let (v', f') = cbOp g y (cTmp2 c) (cF c)
                  c' = (if g == 0 then setF f' else id) c
              in go 6 (writeAt (cAddr c) v') (if z /= 6 then setR 0 z v' c' else c')
  _ -> done c
 where
  op = cOp c
  k = cStep c
  y = resize (op `shiftR` 3) :: Unsigned 3
  z = resize op :: Unsigned 3
  g = resize (op `shiftR` 6) :: Unsigned 2

-- * ED

execED :: Unsigned 8 -> Cpu -> Cpu
execED d c = case x of
  1 -> case z of
    0 -> case k of          -- IN r,(C)
      0 -> go 1 (inAt bc) c
      _ -> let c' = setF ((cF c .&. fC) .|. szp d) c { cWZ = bc + 1 } in
           done (if y /= 6 then setR 0 y d c' else c')
    1 -> case k of          -- OUT (C),r
      0 -> go 1 (outAt bc (if y == 6 then 0 else getR 0 y c)) c
      _ -> done c { cWZ = bc + 1 }
    2 -> case k of          -- SBC HL,rp / ADC HL,rp
      0 -> go 1 (idleAt (ir c) 7) c
      _ ->
        let v = getRP 0 p c
            cy = if testBit (cF c) 0 then 1 else 0 :: Unsigned 17
            h17 = resize hl :: Unsigned 17
            v17 = resize v :: Unsigned 17
            r17 = if q == 0 then h17 - v17 - cy else h17 + v17 + cy
            r16 = resize r17 :: Unsigned 16
            ovf = if q == 0
              then (hl `xor` v) .&. (hl `xor` r16) .&. 0x8000 /= 0
              else complement (hl `xor` v) .&. (hl `xor` r16) .&. 0x8000 /= 0
            f = (hi8 r16 .&. (fS .|. fX .|. fY)) .|. flag (r16 == 0) fZ
                .|. flag ((hl `xor` v `xor` r16) .&. 0x1000 /= 0) fH .|. flag ovf fP
                .|. flag (q == 0) fN .|. flag (testBit r17 16) fC
        in done (setHlIx 0 r16 (setF f c { cWZ = hl + 1 }))
    3 -> case k of          -- LD (nn),rp / LD rp,(nn)
      0 -> goImm 1 c
      1 -> goImm 2 c { cTmp = d }
      2 -> let a = w16 d (cTmp c) in
           if q == 0 then go 3 (writeAt a (lo8 (getRP 0 p c))) c { cAddr = a }
                     else go 3 (readAt a) c { cAddr = a }
      3 -> if q == 0 then go 4 (writeAt (cAddr c + 1) (hi8 (getRP 0 p c))) c
                     else go 4 (readAt (cAddr c + 1)) c { cTmp = d }
      _ -> let c' = c { cWZ = cAddr c + 1 } in
           done (if q == 0 then c' else setRP 0 p (w16 d (cTmp c)) c')
    4 -> let (a', f') = alu 2 0 (cA c) (cF c) in done (setF f' c { cA = a' })   -- NEG
    5 -> case k of          -- RETN / RETI
      0 -> go 1 (readAt (cSP c)) c { cIFF1 = cIFF2 c }
      1 -> go 2 (readAt (cSP c + 1)) c { cTmp = d }
      _ -> let a = w16 d (cTmp c) in done c { cSP = cSP c + 2, cPC = a, cWZ = a }
    6 -> done c { cIM = case y .&. 3 of { 2 -> 1; 3 -> 2; _ -> 0 } }
    _ -> case y of
      0 -> idle1 (\c' -> c' { cI = cA c' })
      1 -> idle1 (\c' -> c' { cR = cA c' })
      2 -> idle1 (\c' -> ldAir (cI c') c')
      3 -> idle1 (\c' -> ldAir (cR c') c')
      4 -> rotDigit True
      5 -> rotDigit False
      _ -> done c
  2 | z <= 3 && y >= 4 -> block
  _ -> done c
 where
  op = cOp c
  k = cStep c
  x = op `shiftR` 6
  y = resize (op `shiftR` 3) :: Unsigned 3
  z = resize op :: Unsigned 3
  p = resize (y `shiftR` 1) :: Unsigned 2
  q = y .&. 1
  bc = w16 (cB c) (cC c)
  hl = hlIx 0 c
  de = w16 (cD c) (cE c)
  inc = y .&. 1 == 0
  rep = y >= 6
  step16 v = if inc then v + 1 else v - 1
  step8 v = if inc then v + 1 else v - 1 :: Unsigned 8

  idle1 f = case k of
    0 -> go 1 (idleAt (ir c) 1) c
    _ -> done (f c)

  ldAir v c' = setF ((cF c' .&. fC) .|. (v .&. (fS .|. fX .|. fY)) .|. flag (v == 0) fZ
                     .|. flag (cIFF2 c') fP) c' { cA = v, cPNew = True }

  rotDigit right = case k of
    0 -> go 1 (readAt hl) c
    1 -> go 2 (idleAt hl 4) c { cTmp = d }
    2 -> let m = cTmp c
             a = cA c
             (a', m') = if right
               then ((a .&. 0xF0) .|. (m .&. 0x0F), (a `shiftL` 4) .|. (m `shiftR` 4))
               else ((a .&. 0xF0) .|. (m `shiftR` 4), (m `shiftL` 4) .|. (a .&. 0x0F))
         in go 3 (writeAt hl m') (setF ((cF c .&. fC) .|. szp a') c { cA = a', cWZ = hl + 1 })
    _ -> done c

  -- going round again: five internal T-states, PC back to the ED byte,
  -- X and Y from PC's high byte; then step 15 ends it
  again a c' = let pc = cPC c' - 2 in
    go 15 (idleAt a 5) c' { cPC = pc, cWZ = pc + 1
                          , cF = (cF c' .&. complement (fX .|. fY)) .|. (hi8 pc .&. (fX .|. fY)) }

  ioFlags v b kk = (sz b) .|. flag (testBit v 7) fN .|. flag (kk > 0xFF) (fH .|. fC)
                   .|. flag (par ((resize kk .&. 7) `xor` b)) fP

  ioRepeat v b c' =
    let f = cF c'
        pf = testBit f 2
        (pf', hf) = if testBit f 0
          then if testBit v 7
            then (pf /= par ((b - 1) .&. 7), b .&. 0x0F == 0x00)
            else (pf /= par ((b + 1) .&. 7), b .&. 0x0F == 0x0F)
          else (pf /= par (b .&. 7), False)
    in c' { cF = (f .&. complement (fP .|. fH)) .|. flag (not pf') fP .|. flag hf fH }

  block = case z of
    0 -> case k of          -- LDI/LDD(R)
      0 -> go 1 (readAt hl) c
      1 -> go 2 (writeAt de d) c { cTmp = d }
      2 -> go 3 (idleAt de 2) c
      15 -> done c
      _ ->
        let bc' = bc - 1
            n = cTmp c + cA c
            f = (cF c .&. (fS .|. fZ .|. fC)) .|. flag (testBit n 1) fY .|. (n .&. fX)
                .|. flag (bc' /= 0) fP
            c' = setF f c { cH = hi8 (step16 hl), cL = lo8 (step16 hl)
                          , cD = hi8 (step16 de), cE = lo8 (step16 de)
                          , cB = hi8 bc', cC = lo8 bc' }
        in if rep && bc' /= 0 then again de c' else done c'
    1 -> case k of          -- CPI/CPD(R)
      0 -> go 1 (readAt hl) c
      1 -> go 2 (idleAt hl 5) c { cTmp = d }
      15 -> done c
      _ ->
        let v = cTmp c
            bc' = bc - 1
            r = cA c - v
            h = (cA c `xor` v `xor` r) .&. fH
            n = r - (if h /= 0 then 1 else 0)
            f = (cF c .&. fC) .|. fN .|. h .|. (r .&. fS) .|. flag (r == 0) fZ
                .|. flag (testBit n 1) fY .|. (n .&. fX) .|. flag (bc' /= 0) fP
            c' = setF f c { cH = hi8 (step16 hl), cL = lo8 (step16 hl)
                          , cB = hi8 bc', cC = lo8 bc', cWZ = step16 (cWZ c) }
        in if rep && bc' /= 0 && r /= 0 then again hl c' else done c'
    2 -> case k of          -- INI/IND(R)
      0 -> go 1 (idleAt (ir c) 1) c
      1 -> go 2 (inAt bc) c
      2 -> go 3 (writeAt hl d) c { cTmp = d }
      3 ->
        let v = cTmp c
            b' = cB c - 1
            kk = resize v + resize (step8 (cC c)) :: Unsigned 9
            c' = setF (ioFlags v b' kk) c { cWZ = step16 bc, cB = b'
                                          , cH = hi8 (step16 hl), cL = lo8 (step16 hl) }
        in if rep && b' /= 0 then again hl (ioRepeat v b' c') else done c'
      _ -> done c
    _ -> case k of          -- OUTI/OUTD, OTIR/OTDR
      0 -> go 1 (idleAt (ir c) 1) c
      1 -> go 2 (readAt hl) c
      2 -> let b' = cB c - 1 in go 3 (outAt (w16 b' (cC c)) d) c { cTmp = d, cB = b' }
      3 ->
        let v = cTmp c
            b' = cB c
            bc' = w16 b' (cC c)
            hl' = step16 hl
            kk = resize v + resize (lo8 hl') :: Unsigned 9
            c' = setF (ioFlags v b' kk) c { cWZ = step16 bc', cH = hi8 hl', cL = lo8 hl' }
        in if rep && b' /= 0 then again bc' (ioRepeat v b' c') else done c'
      _ -> done c

-- * Interrupts

-- | NMI: the thrown-away fetch (step 0), one internal T-state, PC pushed,
-- 0066h.
execNmi :: Unsigned 8 -> Cpu -> Cpu
execNmi _ c = case cStep c of
  0 -> let c' = c { cR = incR (cR c), cHalted = False, cEI = False, cNew = False } in
       go 1 (idleAt (ir c') 1) c'
  1 -> go 2 (writeAt (cSP c - 1) (hi8 (cPC c))) c { cIFF1 = False }
  2 -> go 3 (writeAt (cSP c - 2) (lo8 (cPC c))) c
  _ -> done c { cSP = cSP c - 2, cPC = 0x0066, cWZ = 0x0066 }

-- | /INT: the acknowledge (step 0), one internal T-state, PC pushed; IM 2
-- reads its vector at I:data.
execInt :: Unsigned 8 -> Cpu -> Cpu
execInt d c = case cStep c of
  0 -> let c' = c { cR = incR (cR c), cHalted = False, cEI = False, cNew = False
                  , cIFF1 = False, cIFF2 = False, cTmp = d } in
       go 1 (idleAt (ir c') 1) c'
  1 -> go 2 (writeAt (cSP c - 1) (hi8 (cPC c))) c
  2 -> go 3 (writeAt (cSP c - 2) (lo8 (cPC c))) c
  3 -> let c' = c { cSP = cSP c - 2 } in case cIM c of
    2 -> let vec = w16 (cI c) (cTmp c) in go 4 (readAt vec) c' { cAddr = vec }
    1 -> target 0x0038 c'
    _ -> target (if cTmp c .&. 0xC7 == 0xC7 then resize (cTmp c .&. 0x38) else 0x0038) c'
  4 -> go 5 (readAt (cAddr c + 1)) c { cTmp2 = d }
  _ -> target (w16 d (cTmp2 c)) c
 where
  target a c' = done c' { cPC = a, cWZ = a }

-- * T-states

-- | The CPU with the T-state count of its pending cycle.
data Z80T = Z80T
  { zCpu :: Cpu
  , zT   :: Unsigned 3 -- ^ T-states of the pending cycle already run
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFDataX

z80PowerOn :: Z80T
z80PowerOn = Z80T cpuPowerOn 0

-- | Whether the pending cycle ends with this T-state (WAIT not holding it).
z80Done :: Bool -> Z80T -> Bool
z80Done wait (Z80T c t) =
  let rq = cReq c
      waits = rqKind rq /= KIdle
  in t + 1 >= rqLen rq && not (wait && waits)

-- | One T-state. WAIT is looked at on the cycle's last T-state (not for
-- internal cycles); the inputs are used when the cycle ends.
z80Tick :: Bool -> CpuIn -> Z80T -> Z80T
z80Tick wait ci z@(Z80T c t)
  | z80Done wait z = Z80T (cpuStep ci c) 0
  | t + 1 >= rqLen (cReq c) = z -- stretched by WAIT
  | otherwise = Z80T c (t + 1)
