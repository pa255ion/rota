{-# OPTIONS_GHC -fconstraint-solver-iterations=0 #-}
{-# LANGUAGE StrictData #-}
{-|
The 65C816 in Clash (PLAN.md step 7.7): a pure function per CPU cycle
('cpuCycle'), following the reference model ("W65c816.Spec") cycle for
cycle; the tests run SingleStepTests on it and compare it with the spec on
random programs, bus cycle by bus cycle.

__The cycle.__ 'cpuCycle' runs at the end of each bus cycle with what its
read returned ('ciData') and the NMI and IRQ lines; it returns the request
for the next cycle ('coReq') and, at the end of an instruction, interrupt
sequence, waiting or stopped cycle, what it was ('coStep', as the spec's
'W65c816.Spec.step'). The machine stalls the CPU by not calling it (the
5A22's cycles have their own lengths; a DDR miss freezes the machine).

__Programs.__ An instruction is its opcode fetch, then a program of at
most 8 micro-ops ('U'), each one bus cycle ('progOf'): the addressing
mode's cycles, then the data's (a read, a write, or a read-modify-write's
five), or a program of its own (the stack, jumps and calls, interrupts,
block moves). Some micro-ops only happen when their condition holds
('active': the direct page's extra cycle when DL /= 0, the indexed modes'
extra cycle, a 16-bit operand's second byte, a taken branch's cycles);
after each cycle the sequencer goes on with the next active one. The
order of effects is the spec's: at the end of a cycle NMI and IRQ are
sampled first ('endCycle'), then the cycle's effect ('complete'), and at
the end of a program the instruction's ('finishOp').

__Configuration__ ('CoreConfig') as "W65c816.Spec"'s 'W65c816.Spec.Config':
'coreW65c816' is the SNES's chip, 'coreSingleStep' SingleStepTests' model.
-}
module W65c816.Core
  ( -- * Configuration
    CoreConfig (..)
  , coreW65c816
  , coreSingleStep
    -- * Bus
  , Kind (..)
  , BusOp (..)
  , Req (..)
  , CpuIn (..)
  , CpuOut (..)
  , StepKind (..)
    -- * State
  , Phase (..)
  , Stage (..)
  , Op (..)
  , Mode (..)
  , Acc (..)
  , Info (..)
  , U (..)
  , PushSrc (..)
  , Cpu (..)
  , CpuRegs (..)
  , cpuPowerOn
  , cpuReset
  , cpuInit
  , cpuClock
  , cpuCycle
  , cpuRegs
  , decode
    -- (the SNES's save states, step 7.15c: an instruction's final
    -- registers in its 'StDone' clock)
  , finished
  ) where

import Clash.Prelude

-- * Configuration

data CoreConfig = CoreConfig
  { ccDpXIndWrap :: Bool  -- ^ 'W65c816.Spec.cfgDpXIndWrap'
  , ccJsrIndXNew :: Bool  -- ^ 'W65c816.Spec.cfgJsrIndXNew'
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (NFDataX, BitPack)

coreW65c816, coreSingleStep :: CoreConfig
coreW65c816 = CoreConfig True True
coreSingleStep = CoreConfig False False

-- * Bus

-- | What a read is, as VDA, VPA and VPB show it.
data Kind = KOpcode | KOperand | KData | KVector
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFDataX, BitPack)

data BusOp
  = BRead Kind
  | BWrite Bool   -- ^ VDA (low only for emulation mode's modify cycle)
  | BIdle         -- ^ an internal operation, its address on the bus
  | BStopped      -- ^ WAI waiting or STP: nothing on the bus
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFDataX, BitPack)

data Req = Req
  { rqOp   :: BusOp
  , rqAddr :: Unsigned 24
  , rqData :: Unsigned 8   -- ^ for writes
  , rqLock :: Bool         -- ^ MLB
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (NFDataX, BitPack)

data CpuIn = CpuIn
  { ciData :: Unsigned 8   -- ^ what this cycle's read returned
  , ciNmi  :: Bool         -- ^ the NMI line (asserted) at the end of the cycle
  , ciIrq  :: Bool         -- ^ the IRQ line (asserted) at the end of the cycle
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (NFDataX, BitPack)

-- | What a finished step was, as 'W65c816.Spec.Step'.
data StepKind = SkInstr | SkNmi | SkIrq | SkWait | SkStop | SkReset
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFDataX, BitPack)

data CpuOut = CpuOut
  { coReq  :: Req              -- ^ the request for the next cycle
  , coStep :: Maybe StepKind   -- ^ this cycle ended a step of the spec
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (NFDataX, BitPack)

-- * Instructions

data Op
  = OORA | OAND | OEOR | OADC | OLDA | OCMP | OSBC | OBIT | OLDX | OLDY | OCPX | OCPY
  | OSTA | OSTX | OSTY | OSTZ
  | OASL | OLSR | OROL | OROR | OINC | ODEC | OTSB | OTRB
  | OINX | OINY | ODEX | ODEY
  | OTAX | OTAY | OTXA | OTYA | OTSX | OTXS | OTXY | OTYX | OTCD | OTDC | OTCS | OTSC
  | OXBA | OCLC | OSEC | OCLI | OSEI | OCLV | OCLD | OSED | OXCE | OREP | OSEP | ONOP | OWDM
  | OPHA | OPHX | OPHY | OPHP | OPHB | OPHK | OPHD | OPLA | OPLX | OPLY | OPLP | OPLB | OPLD
  | OPEA | OPEI | OPER
  | OJMP | OJML | OJMPI | OJMPIX | OJMLI | OJSR | OJSRIX | OJSL | ORTS | ORTL | ORTI
  | OBRK | OCOP
  | OBPL | OBMI | OBVC | OBVS | OBCC | OBCS | OBNE | OBEQ | OBRA | OBRL
  | OMVN | OMVP | OWAI | OSTP
  | ONMI | OIRQ | ORESET   -- ^ the hardware sequences
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFDataX, BitPack)

data Mode
  = MNone | MImm | MAbs | MAbsX | MAbsY | MLong | MLongX
  | MDp | MDpX | MDpY | MDpInd | MDpIndLong | MDpXInd | MDpIndY | MDpIndLongY
  | MSr | MSrIndY
  | MAcc   -- ^ the accumulator (ASL A, ...)
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFDataX, BitPack)

data Acc = AccRead | AccWrite | AccRmw | AccOther
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFDataX, BitPack)

data Info = Info
  { iOp   :: Op
  , iMode :: Mode
  , iAcc  :: Acc
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (NFDataX, BitPack)

-- | What a push writes; all but the N ones push 6502 style (in emulation
-- mode within page 1), JSR (a,x)'s by 'ccJsrIndXNew'.
data PushSrc
  = PsPbrInt          -- ^ an interrupt's PBR (native mode only)
  | PsPch | PsPcl     -- ^ an interrupt's PC
  | PsPInt            -- ^ an interrupt's P; then I set, D clear, PBR 0
  | PsP | PsDbr | PsPbr
  | PsRegHi | PsRegLo -- ^ PHA, PHX, PHY (the high byte when 16 bits)
  | PsRetHi | PsRetLo -- ^ JSR: PC - 1
  | PsDHi | PsDLo     -- ^ PHD (N)
  | PsOpndHi | PsOpndLo -- ^ PEA (N)
  | PsPtrHi | PsPtrLo -- ^ PEI (N)
  | PsPerHi | PsPerLo -- ^ PER (N): PC + the operand
  | PsPbrN            -- ^ JSL (N)
  | PsRetHiN | PsRetLoN -- ^ JSL (N): PC - 1
  | PsJxHi | PsJxLo   -- ^ JSR (a,x): PC
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFDataX, BitPack)

-- | A micro-op: one bus cycle.
data U
  = UNone
  | UOpnd (Index 3)   -- ^ operand byte k at PBR:PC, PC + 1
  | UImm (Index 2)    -- ^ an immediate's byte k (1 only when 16 bits)
  | UDl               -- ^ an internal cycle when DL /= 0
  | UIoLast           -- ^ an internal cycle at PBR:PC-1
  | UIoPC             -- ^ an internal cycle at PBR:PC
  | UIdx              -- ^ the indexed modes' extra cycle (when it happens)
  | USrIo             -- ^ (sr,s),y's internal cycle
  | USIo              -- ^ RTS's last cycle, at S
  | UJslIo            -- ^ JSL's internal cycle, at S + 1
  | UPtr (Index 3)    -- ^ pointer byte k
  | URd (Index 2)     -- ^ data byte k (1 only when 16 bits)
  | UWr (Index 2)
  | URmwRd (Index 2)
  | URmwMod           -- ^ the modify cycle
  | URmwWr (Index 2)  -- ^ the result, high byte (when 16 bits) first
  | UPush PushSrc
  | UPull (Index 3)   -- ^ 6502 style, into byte k
  | UPullN (Index 3)
  | UPullP            -- ^ PLP's and RTI's P
  | UVec (Index 2)
  | UBrTaken          -- ^ a taken branch's cycle
  | UBrCross          -- ^ emulation mode: a taken branch to another page
  | UMvRd | UMvWr | UMvIo
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFDataX, BitPack)

-- * State

data Phase = PhFetch | PhRun | PhWait | PhStop
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFDataX, BitPack)

-- | Where the CPU is within a bus cycle: its request is on the bus and
-- it waits for the cycle's end ('StReady'), or the clock after that end
-- works out the next request ('StReq': micro-op 'cT'; 'StDone': the
-- instruction's end; 'StWake': WAI's wait over).
data Stage = StReady | StReq | StDone | StWake
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFDataX, BitPack)

data Cpu = Cpu
  { cA, cX, cY, cS, cD, cPC :: Unsigned 16
  , cDBR, cPBR, cP :: Unsigned 8
  , cE        :: Bool
  , cPhase    :: Phase
  , cStage    :: Stage
  , cInfo     :: Info
  , cT        :: Index 8              -- ^ the micro-op on the bus
  , cOpnd     :: Vec 3 (Unsigned 8)
  , cPtr      :: Vec 3 (Unsigned 8)
  , cVal      :: Vec 3 (Unsigned 8)   -- ^ data read, RMW value, pulls, vector
  , cNmiLine  :: Bool
  , cNmiNeed  :: Bool
  , cPollCur  :: BitVector 2          -- ^ bit 0 NMI, bit 1 IRQ: would be taken (last cycle)
  , cPollPrev :: BitVector 2
  , cReq      :: Req                  -- ^ the request on the bus this cycle
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (NFDataX, BitPack)

data CpuRegs = CpuRegs
  { crA, crX, crY, crS, crD, crPC :: Unsigned 16
  , crDBR, crPBR, crP :: Unsigned 8
  , crE :: Bool
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (NFDataX, BitPack)

cpuRegs :: Cpu -> CpuRegs
cpuRegs c = CpuRegs (cA c) (cX c) (cY c) (cS c) (cD c) (cPC c) (cDBR c) (cPBR c) (cP c) (cE c)

-- | A CPU about to fetch the opcode at PBR:PC, with these registers (as
-- 'W65c816.Spec.setRegs': emulation mode's invariants applied).
cpuPowerOn :: CpuRegs -> Cpu
cpuPowerOn r = fetchNext (normalise Cpu
  { cA = crA r, cX = crX r, cY = crY r, cS = crS r, cD = crD r, cPC = crPC r
  , cDBR = crDBR r, cPBR = crPBR r, cP = crP r, cE = crE r
  , cPhase = PhFetch, cStage = StReady, cInfo = Info ONOP MNone AccOther, cT = 0
  , cOpnd = repeat 0, cPtr = repeat 0, cVal = repeat 0
  , cNmiLine = False, cNmiNeed = False, cPollCur = 0, cPollPrev = 0
  , cReq = Req BStopped 0 0 False })

-- | /RES (as 'W65c816.Spec.reset'): emulation mode, D, DBR and PBR 0, M,
-- X and I set, D clear; two internal cycles, then the vector at 00FFFCh.
cpuReset :: CoreConfig -> Cpu -> Cpu
cpuReset cfg c0 =
  let c = normalise c0 { cE = True, cD = 0, cDBR = 0, cPBR = 0
                       , cP = (cP c0 .|. 0x34) .&. complement 0x08, cNmiNeed = False }
  in startHw c (Info ORESET MNone AccOther) `const` cfg

-- | A literal power-on state, as a register's reset value needs:
-- 'cpuReset' of zeroed registers (S 01FFh), its first cycle (an internal
-- one at 00:0000h) on the bus. A test checks it equals that.
cpuInit :: Cpu
cpuInit = Cpu
  { cA = 0, cX = 0, cY = 0, cS = 0x1FF, cD = 0, cPC = 0
  , cDBR = 0, cPBR = 0, cP = 0x34, cE = True
  , cPhase = PhRun, cStage = StReady, cInfo = Info ORESET MNone AccOther, cT = 0
  , cOpnd = repeat 0, cPtr = repeat 0, cVal = repeat 0
  , cNmiLine = False, cNmiNeed = False, cPollCur = 0, cPollPrev = 0
  , cReq = Req BIdle 0 0 False }

-- * Helpers

fC, fZ, fI, fD, fX, fM, fV, fN :: Unsigned 8
fC = 0x01
fZ = 0x02
fI = 0x04
fD = 0x08
fX = 0x10
fM = 0x20
fV = 0x40
fN = 0x80

flag :: Unsigned 8 -> Cpu -> Bool
flag f c = cP c .&. f /= 0

setF :: Unsigned 8 -> Bool -> Cpu -> Cpu
setF f b c = c { cP = if b then cP c .|. f else cP c .&. complement f }

m8, x8 :: Cpu -> Bool
m8 = flag fM
x8 = flag fX

w16 :: Unsigned 8 -> Unsigned 8 -> Unsigned 16
w16 h l = resize h `shiftL` 8 .|. resize l

w24 :: Unsigned 8 -> Unsigned 16 -> Unsigned 24
w24 b a = resize b `shiftL` 16 .|. resize a

lo8, hi8 :: Unsigned 16 -> Unsigned 8
lo8 = resize
hi8 x = resize (x `shiftR` 8)

v16 :: Vec 3 (Unsigned 8) -> Unsigned 16
v16 v = w16 (v !! (1 :: Int)) (v !! (0 :: Int))

v24 :: Vec 3 (Unsigned 8) -> Unsigned 24
v24 v = w24 (v !! (2 :: Int)) (v16 v)

-- | Emulation mode's invariants, and the 8-bit index registers' high bytes.
-- | The registers as the instruction leaves them: in its 'StDone' clock
-- 'finishOp' (with 'normalise') hasn't been applied yet, the clock after it
-- has; anywhere else the record as it is.
finished :: Cpu -> Cpu
finished c = if cStage c == StDone then normalise (finishOp c) else c

normalise :: Cpu -> Cpu
normalise c0 =
  let c1 = if cE c0 then c0 { cP = cP c0 .|. 0x30, cS = 0x100 .|. (cS c0 .&. 0xFF) } else c0
  in if x8 c1 then c1 { cX = cX c1 .&. 0xFF, cY = cY c1 .&. 0xFF } else c1

nzW :: Bool -> Unsigned 16 -> Cpu -> Cpu
nzW eight v c =
  let (n, z) = if eight then (testBit v 7, v .&. 0xFF == 0) else (testBit v 15, v == 0)
  in setF fN n (setF fZ z c)

setAcc :: Bool -> Unsigned 16 -> Cpu -> Cpu
setAcc eight v c = c { cA = if eight then (cA c .&. 0xFF00) .|. (v .&. 0xFF) else v }

accOf :: Bool -> Cpu -> Unsigned 16
accOf eight c = if eight then cA c .&. 0xFF else cA c

setP :: Unsigned 8 -> Cpu -> Cpu
setP p c = normalise c { cP = p }



pcAddr :: Cpu -> Unsigned 24
pcAddr c = w24 (cPBR c) (cPC c)


opnd0 :: Cpu -> Unsigned 16
opnd0 c = resize (cOpnd c !! (0 :: Int))

-- | 6502-style stack step and the 65816's own.
sDown, sUp :: Cpu -> Unsigned 16
sDown c = if cE c then 0x100 .|. ((cS c - 1) .&. 0xFF) else cS c - 1
sUp c = if cE c then 0x100 .|. ((cS c + 1) .&. 0xFF) else cS c + 1

-- * Decoding

decode :: Unsigned 8 -> Info
decode op
  | aluCol && row /= 4 = Info (aluRow row) (colMode col) AccRead
  | aluCol && op /= 0x89 = Info OSTA (colMode col) AccWrite
  | otherwise = other
 where
  row = op `shiftR` 5
  col = op .&. 0x1F
  aluCol = col `elem` (0x01 :> 0x03 :> 0x05 :> 0x07 :> 0x09 :> 0x0D :> 0x0F :> 0x11 :> 0x12 :> 0x13
                       :> 0x15 :> 0x17 :> 0x19 :> 0x1D :> 0x1F :> Nil)
  aluRow r = case r of
    0 -> OORA
    1 -> OAND
    2 -> OEOR
    3 -> OADC
    6 -> OCMP
    7 -> OSBC
    _ -> OLDA
  colMode m = case m of
    0x01 -> MDpXInd
    0x03 -> MSr
    0x05 -> MDp
    0x07 -> MDpIndLong
    0x09 -> MImm
    0x0D -> MAbs
    0x0F -> MLong
    0x11 -> MDpIndY
    0x12 -> MDpInd
    0x13 -> MSrIndY
    0x15 -> MDpX
    0x17 -> MDpIndLongY
    0x19 -> MAbsY
    0x1D -> MAbsX
    _ -> MLongX
  rdI o m = Info o m AccRead
  wrI o m = Info o m AccWrite
  rmw o m = Info o m AccRmw
  oth o = Info o MNone AccOther
  acc o = Info o MAcc AccOther
  other = case op of
    0x89 -> rdI OBIT MImm
    0x86 -> wrI OSTX MDp
    0x8E -> wrI OSTX MAbs
    0x96 -> wrI OSTX MDpY
    0x84 -> wrI OSTY MDp
    0x8C -> wrI OSTY MAbs
    0x94 -> wrI OSTY MDpX
    0x64 -> wrI OSTZ MDp
    0x74 -> wrI OSTZ MDpX
    0x9C -> wrI OSTZ MAbs
    0x9E -> wrI OSTZ MAbsX
    0xA2 -> rdI OLDX MImm
    0xA6 -> rdI OLDX MDp
    0xAE -> rdI OLDX MAbs
    0xB6 -> rdI OLDX MDpY
    0xBE -> rdI OLDX MAbsY
    0xA0 -> rdI OLDY MImm
    0xA4 -> rdI OLDY MDp
    0xAC -> rdI OLDY MAbs
    0xB4 -> rdI OLDY MDpX
    0xBC -> rdI OLDY MAbsX
    0xE0 -> rdI OCPX MImm
    0xE4 -> rdI OCPX MDp
    0xEC -> rdI OCPX MAbs
    0xC0 -> rdI OCPY MImm
    0xC4 -> rdI OCPY MDp
    0xCC -> rdI OCPY MAbs
    0x24 -> rdI OBIT MDp
    0x2C -> rdI OBIT MAbs
    0x34 -> rdI OBIT MDpX
    0x3C -> rdI OBIT MAbsX
    0x06 -> rmw OASL MDp
    0x0E -> rmw OASL MAbs
    0x16 -> rmw OASL MDpX
    0x1E -> rmw OASL MAbsX
    0x46 -> rmw OLSR MDp
    0x4E -> rmw OLSR MAbs
    0x56 -> rmw OLSR MDpX
    0x5E -> rmw OLSR MAbsX
    0x26 -> rmw OROL MDp
    0x2E -> rmw OROL MAbs
    0x36 -> rmw OROL MDpX
    0x3E -> rmw OROL MAbsX
    0x66 -> rmw OROR MDp
    0x6E -> rmw OROR MAbs
    0x76 -> rmw OROR MDpX
    0x7E -> rmw OROR MAbsX
    0xE6 -> rmw OINC MDp
    0xEE -> rmw OINC MAbs
    0xF6 -> rmw OINC MDpX
    0xFE -> rmw OINC MAbsX
    0xC6 -> rmw ODEC MDp
    0xCE -> rmw ODEC MAbs
    0xD6 -> rmw ODEC MDpX
    0xDE -> rmw ODEC MAbsX
    0x04 -> rmw OTSB MDp
    0x0C -> rmw OTSB MAbs
    0x14 -> rmw OTRB MDp
    0x1C -> rmw OTRB MAbs
    0x0A -> acc OASL
    0x4A -> acc OLSR
    0x2A -> acc OROL
    0x6A -> acc OROR
    0x1A -> acc OINC
    0x3A -> acc ODEC
    0xE8 -> oth OINX
    0xC8 -> oth OINY
    0xCA -> oth ODEX
    0x88 -> oth ODEY
    0xAA -> oth OTAX
    0xA8 -> oth OTAY
    0x8A -> oth OTXA
    0x98 -> oth OTYA
    0xBA -> oth OTSX
    0x9A -> oth OTXS
    0x9B -> oth OTXY
    0xBB -> oth OTYX
    0x5B -> oth OTCD
    0x7B -> oth OTDC
    0x1B -> oth OTCS
    0x3B -> oth OTSC
    0xEB -> oth OXBA
    0x18 -> oth OCLC
    0x38 -> oth OSEC
    0x58 -> oth OCLI
    0x78 -> oth OSEI
    0xB8 -> oth OCLV
    0xD8 -> oth OCLD
    0xF8 -> oth OSED
    0xFB -> oth OXCE
    0xC2 -> oth OREP
    0xE2 -> oth OSEP
    0xEA -> oth ONOP
    0x42 -> oth OWDM
    0x48 -> oth OPHA
    0xDA -> oth OPHX
    0x5A -> oth OPHY
    0x08 -> oth OPHP
    0x8B -> oth OPHB
    0x4B -> oth OPHK
    0x0B -> oth OPHD
    0x68 -> oth OPLA
    0xFA -> oth OPLX
    0x7A -> oth OPLY
    0x28 -> oth OPLP
    0xAB -> oth OPLB
    0x2B -> oth OPLD
    0xF4 -> oth OPEA
    0xD4 -> oth OPEI
    0x62 -> oth OPER
    0x4C -> oth OJMP
    0x5C -> oth OJML
    0x6C -> oth OJMPI
    0x7C -> oth OJMPIX
    0xDC -> oth OJMLI
    0x20 -> oth OJSR
    0xFC -> oth OJSRIX
    0x22 -> oth OJSL
    0x60 -> oth ORTS
    0x6B -> oth ORTL
    0x40 -> oth ORTI
    0x00 -> oth OBRK
    0x02 -> oth OCOP
    0x10 -> oth OBPL
    0x30 -> oth OBMI
    0x50 -> oth OBVC
    0x70 -> oth OBVS
    0x90 -> oth OBCC
    0xB0 -> oth OBCS
    0xD0 -> oth OBNE
    0xF0 -> oth OBEQ
    0x80 -> oth OBRA
    0x82 -> oth OBRL
    0x54 -> oth OMVN
    0x44 -> oth OMVP
    0xCB -> oth OWAI
    _ -> oth OSTP   -- 0xDB

-- * Programs

-- | The micro-ops of an instruction after its opcode fetch.
progOf :: Info -> Vec 8 U
progOf (Info op mode acc) = case acc of
  AccRead -> case mode of
    MImm -> p2 (UImm 0) (UImm 1)
    _ -> modeSteps ++ (URd 0 :> URd 1 :> Nil)
  AccWrite -> modeSteps ++ (UWr 0 :> UWr 1 :> Nil)
  -- read-modify-writes have modes of 3 cycles at most
  AccRmw -> takeI modeSteps ++ (URmwRd 0 :> URmwRd 1 :> URmwMod :> URmwWr 1 :> URmwWr 0 :> Nil)
  AccOther -> other
 where
  -- the mode's cycles, 6 slots (padded at the end with UNone, which the
  -- sequencer skips)
  modeSteps :: Vec 6 U
  modeSteps = case mode of
    MAbs -> q2 (UOpnd 0) (UOpnd 1)
    MAbsX -> q3 (UOpnd 0) (UOpnd 1) (UIdx)
    MAbsY -> q3 (UOpnd 0) (UOpnd 1) (UIdx)
    MLong -> q3 (UOpnd 0) (UOpnd 1) (UOpnd 2)
    MLongX -> q3 (UOpnd 0) (UOpnd 1) (UOpnd 2)
    MDp -> q2 (UOpnd 0) (UDl)
    MDpX -> q3 (UOpnd 0) (UDl) (UIoLast)
    MDpY -> q3 (UOpnd 0) (UDl) (UIoLast)
    MDpInd -> q4 (UOpnd 0) (UDl) (UPtr 0) (UPtr 1)
    MDpIndLong -> q5 (UOpnd 0) (UDl) (UPtr 0) (UPtr 1) (UPtr 2)
    MDpXInd -> q5 (UOpnd 0) (UDl) (UIoLast) (UPtr 0) (UPtr 1)
    MDpIndY -> q5 (UOpnd 0) (UDl) (UPtr 0) (UPtr 1) (UIdx)
    MDpIndLongY -> q5 (UOpnd 0) (UDl) (UPtr 0) (UPtr 1) (UPtr 2)
    MSr -> q2 (UOpnd 0) (UIoLast)
    MSrIndY -> q5 (UOpnd 0) (UIoLast) (UPtr 0) (UPtr 1) (USrIo)
    _ -> q0 
  other = case op of
    OPHA -> p3 (UIoPC) (UPush PsRegHi) (UPush PsRegLo)
    OPHX -> p3 (UIoPC) (UPush PsRegHi) (UPush PsRegLo)
    OPHY -> p3 (UIoPC) (UPush PsRegHi) (UPush PsRegLo)
    OPHP -> p2 (UIoPC) (UPush PsP)
    OPHB -> p2 (UIoPC) (UPush PsDbr)
    OPHK -> p2 (UIoPC) (UPush PsPbr)
    OPHD -> p3 (UIoPC) (UPush PsDHi) (UPush PsDLo)
    OPLA -> p4 (UIoPC) (UIoPC) (UPull 0) (UPull 1)
    OPLX -> p4 (UIoPC) (UIoPC) (UPull 0) (UPull 1)
    OPLY -> p4 (UIoPC) (UIoPC) (UPull 0) (UPull 1)
    OPLP -> p3 (UIoPC) (UIoPC) (UPullP)
    OPLB -> p3 (UIoPC) (UIoPC) (UPullN 0)
    OPLD -> p4 (UIoPC) (UIoPC) (UPullN 0) (UPullN 1)
    OPEA -> p4 (UOpnd 0) (UOpnd 1) (UPush PsOpndHi) (UPush PsOpndLo)
    OPEI -> p6 (UOpnd 0) (UDl) (UPtr 0) (UPtr 1) (UPush PsPtrHi) (UPush PsPtrLo)
    OPER -> p5 (UOpnd 0) (UOpnd 1) (UIoLast) (UPush PsPerHi) (UPush PsPerLo)
    OJMP -> p2 (UOpnd 0) (UOpnd 1)
    OJML -> p3 (UOpnd 0) (UOpnd 1) (UOpnd 2)
    OJMPI -> p4 (UOpnd 0) (UOpnd 1) (UPtr 0) (UPtr 1)
    OJMPIX -> p5 (UOpnd 0) (UOpnd 1) (UIoLast) (UPtr 0) (UPtr 1)
    OJMLI -> p5 (UOpnd 0) (UOpnd 1) (UPtr 0) (UPtr 1) (UPtr 2)
    OJSR -> p5 (UOpnd 0) (UOpnd 1) (UIoLast) (UPush PsRetHi) (UPush PsRetLo)
    OJSRIX -> p7 (UOpnd 0) (UPush PsJxHi) (UPush PsJxLo) (UOpnd 1) (UIoLast) (UPtr 0) (UPtr 1)
    OJSL -> p7 (UOpnd 0) (UOpnd 1) (UPush PsPbrN) (UJslIo) (UOpnd 2) (UPush PsRetHiN) (UPush PsRetLoN)
    ORTS -> p5 (UIoPC) (UIoPC) (UPull 0) (UPull 1) (USIo)
    ORTL -> p5 (UIoPC) (UIoPC) (UPullN 0) (UPullN 1) (UPullN 2)
    ORTI -> p6 (UIoPC) (UIoPC) (UPullP) (UPull 0) (UPull 1) (UPull 2)
    OBRK -> p7 (UOpnd 0) (UPush PsPbrInt) (UPush PsPch) (UPush PsPcl) (UPush PsPInt) (UVec 0) (UVec 1)
    OCOP -> p7 (UOpnd 0) (UPush PsPbrInt) (UPush PsPch) (UPush PsPcl) (UPush PsPInt) (UVec 0) (UVec 1)
    ONMI -> p8 (UIoPC) (UIoPC) (UPush PsPbrInt) (UPush PsPch) (UPush PsPcl) (UPush PsPInt) (UVec 0) (UVec 1)
    OIRQ -> p8 (UIoPC) (UIoPC) (UPush PsPbrInt) (UPush PsPch) (UPush PsPcl) (UPush PsPInt) (UVec 0) (UVec 1)
    ORESET -> p4 (UIoPC) (UIoPC) (UVec 0) (UVec 1)
    OBRL -> p3 (UOpnd 0) (UOpnd 1) (UIoLast)
    OMVN -> p6 (UOpnd 0) (UOpnd 1) (UMvRd) (UMvWr) (UMvIo) (UMvIo)
    OMVP -> p6 (UOpnd 0) (UOpnd 1) (UMvRd) (UMvWr) (UMvIo) (UMvIo)
    OXBA -> p2 (UIoPC) (UIoPC)
    OREP -> p2 (UOpnd 0) (UIoLast)
    OSEP -> p2 (UOpnd 0) (UIoLast)
    OWAI -> p2 (UIoPC) (UIoPC)
    OSTP -> p2 (UIoPC) (UIoPC)
    _ | isBranch op -> p3 (UOpnd 0) (UBrTaken) (UBrCross)
      | otherwise -> p1 (UIoPC)   -- implied, accumulator, WDM

-- | Programs padded to 8 micro-ops, modes' cycles to 6.
p1 :: U -> Vec 8 U
p1 u0 = u0 :> UNone :> UNone :> UNone :> UNone :> UNone :> UNone :> UNone :> Nil
p2 :: U -> U -> Vec 8 U
p2 u0 u1 = u0 :> u1 :> UNone :> UNone :> UNone :> UNone :> UNone :> UNone :> Nil
p3 :: U -> U -> U -> Vec 8 U
p3 u0 u1 u2 = u0 :> u1 :> u2 :> UNone :> UNone :> UNone :> UNone :> UNone :> Nil
p4 :: U -> U -> U -> U -> Vec 8 U
p4 u0 u1 u2 u3 = u0 :> u1 :> u2 :> u3 :> UNone :> UNone :> UNone :> UNone :> Nil
p5 :: U -> U -> U -> U -> U -> Vec 8 U
p5 u0 u1 u2 u3 u4 = u0 :> u1 :> u2 :> u3 :> u4 :> UNone :> UNone :> UNone :> Nil
p6 :: U -> U -> U -> U -> U -> U -> Vec 8 U
p6 u0 u1 u2 u3 u4 u5 = u0 :> u1 :> u2 :> u3 :> u4 :> u5 :> UNone :> UNone :> Nil
p7 :: U -> U -> U -> U -> U -> U -> U -> Vec 8 U
p7 u0 u1 u2 u3 u4 u5 u6 = u0 :> u1 :> u2 :> u3 :> u4 :> u5 :> u6 :> UNone :> Nil
p8 :: U -> U -> U -> U -> U -> U -> U -> U -> Vec 8 U
p8 u0 u1 u2 u3 u4 u5 u6 u7 = u0 :> u1 :> u2 :> u3 :> u4 :> u5 :> u6 :> u7 :> Nil
q0 :: Vec 6 U
q0  = UNone :> UNone :> UNone :> UNone :> UNone :> UNone :> Nil
q2 :: U -> U -> Vec 6 U
q2 u0 u1 = u0 :> u1 :> UNone :> UNone :> UNone :> UNone :> Nil
q3 :: U -> U -> U -> Vec 6 U
q3 u0 u1 u2 = u0 :> u1 :> u2 :> UNone :> UNone :> UNone :> Nil
q4 :: U -> U -> U -> U -> Vec 6 U
q4 u0 u1 u2 u3 = u0 :> u1 :> u2 :> u3 :> UNone :> UNone :> Nil
q5 :: U -> U -> U -> U -> U -> Vec 6 U
q5 u0 u1 u2 u3 u4 = u0 :> u1 :> u2 :> u3 :> u4 :> UNone :> Nil

isBranch :: Op -> Bool
isBranch op = op `elem` (OBPL :> OBMI :> OBVC :> OBVS :> OBCC :> OBCS :> OBNE :> OBEQ :> OBRA :> Nil)

branchTaken :: Op -> Cpu -> Bool
branchTaken op c = case op of
  OBPL -> not (flag fN c)
  OBMI -> flag fN c
  OBVC -> not (flag fV c)
  OBVS -> flag fV c
  OBCC -> not (flag fC c)
  OBCS -> flag fC c
  OBNE -> not (flag fZ c)
  OBEQ -> flag fZ c
  _ -> True

branchTarget :: Cpu -> Unsigned 16
branchTarget c = let o = cOpnd c !! (0 :: Int) in cPC c + (if testBit o 7 then 0xFF00 .|. resize o else resize o)

-- | 16-bit operands: the index registers' instructions by X, the rest by M.
wide :: Cpu -> Bool
wide c = case iOp (cInfo c) of
  op | op `elem` (OLDX :> OLDY :> OCPX :> OCPY :> OSTX :> OSTY :> OPHX :> OPHY :> OPLX :> OPLY :> Nil) -> not (x8 c)
     | otherwise -> not (m8 c)

-- | The index of the indexed modes, and their base address.
indexReg :: Cpu -> Unsigned 16
indexReg c = case iMode (cInfo c) of
  MAbsX -> cX c
  MDpX -> cX c
  MLongX -> cX c
  MDpXInd -> cX c
  _ -> cY c

idxBase :: Cpu -> Unsigned 24
idxBase c = case iMode (cInfo c) of
  MDpIndY -> w24 (cDBR c) (v16 (cPtr c))
  _ -> w24 (cDBR c) (v16 (cOpnd c))

idxCross :: Cpu -> Bool
idxCross c = resize (resize (idxBase c) :: Unsigned 8) + resize (resize (indexReg c) :: Unsigned 8) > (0xFF :: Unsigned 9)

-- | The conditions of the conditional micro-ops, worked out once from the
-- state before the cycle's effect: no condition reads what the cycle
-- that is ending brings (a branch's offset, the indexed base's low byte
-- and the like come a cycle or more before the micro-op that asks).
data Conds = Conds
  { kWide, kM16, kDl, kIdx, kNative, kTaken, kCross, kPullHi :: Bool }

conds :: Cpu -> Conds
conds c =
  let op = iOp (cInfo c)
      taken = branchTaken op c
  in Conds
       { kWide = wide c
       , kM16 = not (m8 c)
       , kDl = cD c .&. 0xFF /= 0
       , kIdx = iAcc (cInfo c) /= AccRead || not (x8 c) || idxCross c
       , kNative = not (cE c)
       , kTaken = taken
       , kCross = taken && cE c && (branchTarget c .&. 0xFF00) /= (cPC c .&. 0xFF00)
       , kPullHi = not (op `elem` (OPLA :> OPLX :> OPLY :> Nil)) || wide c }

-- | Whether a micro-op happens.
active :: Conds -> U -> Bool
active k u = case u of
  UNone -> False
  UImm 1 -> kWide k
  URd 1 -> kWide k
  UWr 1 -> kWide k
  URmwRd 1 -> kM16 k
  URmwWr 1 -> kM16 k
  UDl -> kDl k
  UIdx -> kIdx k
  UPush PsPbrInt -> kNative k
  UPush PsRegHi -> kWide k
  UPull 1 -> kPullHi k
  UPull 2 -> kNative k
  UBrTaken -> kTaken k
  UBrCross -> kCross k
  _ -> True

-- * Addresses

-- | How an address wraps: carries into the bank, within the bank, or
-- within the page.
data Wrap = Wr24 | Wr16 | Wr8

-- | The direct page's 6502 modes: within the page in emulation mode with
-- DL = 0.
dpWrap :: Cpu -> Wrap
dpWrap c = if cE c && cD c .&. 0xFF == 0 then Wr8 else Wr16

-- | One adder for every address: base + offset + k, wrapped.
genAddr :: Unsigned 24 -> Unsigned 16 -> Unsigned 2 -> Wrap -> Unsigned 24
genAddr b o k w =
  let sm = b + resize o + resize k
  in case w of
       Wr24 -> sm
       Wr16 -> (b .&. 0xFF0000) .|. (sm .&. 0xFFFF)
       Wr8 -> (b .&. 0xFFFF00) .|. (sm .&. 0xFF)

-- | A micro-op's address as base, offset, k and wrap.
addrSel :: CoreConfig -> Cpu -> U -> (Unsigned 24, Unsigned 16, Unsigned 2, Wrap)
addrSel cfg c u = case u of
  UOpnd _ -> (pc, 0, 0, Wr16)
  UImm _ -> (pc, 0, 0, Wr16)
  UIoPC -> (pc, 0, 0, Wr16)
  UDl -> pcm1
  UIoLast -> pcm1
  UBrTaken -> pcm1
  UBrCross -> pcm1
  UIdx -> (idxBase c, indexReg c, 0, Wr8)
  USrIo -> (s, opnd0 c, 1, Wr16)
  USIo -> (s, 0, 0, Wr16)
  UJslIo -> (s, 0, 1, Wr16)
  UPtr k -> ptrSel k
  URd b -> eaSel b
  UWr b -> eaSel b
  URmwRd b -> eaSel b
  URmwWr b -> eaSel b
  URmwMod -> eaSel (if cE c || m8 c then 0 else 1)
  UPush _ -> (s, 0, 0, Wr16)
  UPull _ -> (s, 0, 1, if cE c then Wr8 else Wr16)
  UPullP -> (s, 0, 1, if cE c then Wr8 else Wr16)
  UPullN _ -> (s, 0, 1, Wr16)
  UVec k -> (resize (vector c), 0, fromIntegral k, Wr16)
  UMvRd -> (w24 (cOpnd c !! (1 :: Int)) (cX c), 0, 0, Wr24)
  UMvWr -> (w24 (cOpnd c !! (0 :: Int)) (cY c), 0, 0, Wr24)
  UMvIo -> (w24 (cOpnd c !! (0 :: Int)) (cY c), 0, 0, Wr24)
  UNone -> (0, 0, 0, Wr24)
 where
  pc = pcAddr c
  pcm1 = (pc, 0xFFFF, 0, Wr16)
  s = resize (cS c)
  d = resize (cD c)
  dbrOpnd = w24 (cDBR c) (v16 (cOpnd c))
  dbrPtr = w24 (cDBR c) (v16 (cPtr c))
  pre = opnd0 c + indexReg c
  eaSel :: Index 2 -> (Unsigned 24, Unsigned 16, Unsigned 2, Wrap)
  eaSel b0 =
    let b = fromIntegral b0
    in case iMode (cInfo c) of
         MAbs -> (dbrOpnd, 0, b, Wr24)
         MAbsX -> (dbrOpnd, cX c, b, Wr24)
         MAbsY -> (dbrOpnd, cY c, b, Wr24)
         MLong -> (v24 (cOpnd c), 0, b, Wr24)
         MLongX -> (v24 (cOpnd c), cX c, b, Wr24)
         MDp -> (d, opnd0 c, b, dpWrap c)
         MDpX -> (d, pre, b, dpWrap c)
         MDpY -> (d, pre, b, dpWrap c)
         MDpInd -> (dbrPtr, 0, b, Wr24)
         MDpXInd -> (dbrPtr, 0, b, Wr24)
         MDpIndLong -> (v24 (cPtr c), 0, b, Wr24)
         MDpIndY -> (dbrPtr, cY c, b, Wr24)
         MDpIndLongY -> (v24 (cPtr c), cY c, b, Wr24)
         MSr -> (s, opnd0 c, b, Wr16)
         MSrIndY -> (dbrPtr, cY c, b, Wr24)
         _ -> (0, 0, 0, Wr24)
  ptrSel :: Index 3 -> (Unsigned 24, Unsigned 16, Unsigned 2, Wrap)
  ptrSel k0 =
    let k = fromIntegral k0 :: Unsigned 2
        aa = v16 (cOpnd c)
    in case iOp (cInfo c) of
         OJMPI -> (resize aa, 0, k, Wr16)
         OJMLI -> (resize aa, 0, k, Wr16)
         OJMPIX -> (w24 (cPBR c) aa, cX c, k, Wr16)
         OJSRIX -> (w24 (cPBR c) aa, cX c, k, Wr16)
         OPEI -> (d, opnd0 c, k, Wr16)
         _ -> case iMode (cInfo c) of
           MDpIndLong -> (d, opnd0 c, k, Wr16)
           MDpIndLongY -> (d, opnd0 c, k, Wr16)
           MDpXInd
             -- emulation mode, DL /= 0: the high byte within the low
             -- byte's page ('ccDpXIndWrap'); the low byte's address is
             -- still on the bus (the cycle before)
             | k == 1 && ccDpXIndWrap cfg && cE c && cD c .&. 0xFF /= 0 -> (rqAddr (cReq c), 0, 1, Wr8)
             | otherwise -> (d, pre, k, dpWrap c)
           MSrIndY -> (s, opnd0 c, k, Wr16)
           _ -> (d, opnd0 c, k, dpWrap c)

addrOf :: CoreConfig -> Cpu -> U -> Unsigned 24
addrOf cfg c u = let (b, o, k, w) = addrSel cfg c u in genAddr b o k w

-- * The request for a micro-op

rd :: Kind -> Unsigned 24 -> Req
rd k a = Req (BRead k) a 0 False

io :: Unsigned 24 -> Req
io a = Req BIdle a 0 False

-- | The value a store writes (16 bits).
storeValue :: Cpu -> Unsigned 16
storeValue c = case iOp (cInfo c) of
  OSTX -> cX c
  OSTY -> cY c
  OSTZ -> 0
  _ -> cA c

pushReg :: Cpu -> Unsigned 16
pushReg c = case iOp (cInfo c) of
  OPHX -> cX c
  OPHY -> cY c
  _ -> cA c

vector :: Cpu -> Unsigned 16
vector c = case iOp (cInfo c) of
  OBRK -> if cE c then 0xFFFE else 0xFFE6
  OCOP -> if cE c then 0xFFF4 else 0xFFE4
  ONMI -> if cE c then 0xFFFA else 0xFFEA
  OIRQ -> if cE c then 0xFFFE else 0xFFEE
  _ -> 0xFFFC

pushByte :: Cpu -> PushSrc -> Unsigned 8
pushByte c s = case s of
  PsPbrInt -> cPBR c
  PsPch -> hi8 (cPC c)
  PsPcl -> lo8 (cPC c)
  PsPInt -> if cE c && iOp (cInfo c) `elem` (ONMI :> OIRQ :> Nil) then cP c .&. complement 0x10 else cP c
  PsP -> cP c
  PsDbr -> cDBR c
  PsPbr -> cPBR c
  PsRegHi -> hi8 (pushReg c)
  PsRegLo -> lo8 (pushReg c)
  PsRetHi -> hi8 pcm1
  PsRetLo -> lo8 pcm1
  PsDHi -> hi8 (cD c)
  PsDLo -> lo8 (cD c)
  PsOpndHi -> cOpnd c !! (1 :: Int)
  PsOpndLo -> cOpnd c !! (0 :: Int)
  PsPtrHi -> cPtr c !! (1 :: Int)
  PsPtrLo -> cPtr c !! (0 :: Int)
  PsPerHi -> hi8 per
  PsPerLo -> lo8 per
  PsPbrN -> cPBR c
  PsRetHiN -> hi8 pcm1
  PsRetLoN -> lo8 pcm1
  PsJxHi -> hi8 (cPC c)
  PsJxLo -> lo8 (cPC c)
 where
  pcm1 = cPC c - 1
  per = cPC c + v16 (cOpnd c)

-- | Whether a push moves S 6502 style.
pushOld :: CoreConfig -> PushSrc -> Bool
pushOld cfg s = case s of
  PsDHi -> False
  PsDLo -> False
  PsOpndHi -> False
  PsOpndLo -> False
  PsPtrHi -> False
  PsPtrLo -> False
  PsPerHi -> False
  PsPerLo -> False
  PsPbrN -> False
  PsRetHiN -> False
  PsRetLoN -> False
  PsJxHi -> not (ccJsrIndXNew cfg)
  PsJxLo -> not (ccJsrIndXNew cfg)
  _ -> True

reqFor :: CoreConfig -> Cpu -> U -> Req
reqFor cfg c u = Req op (addrOf cfg c u) dat lock
 where
  v0 = cVal c !! (0 :: Int)
  op = case u of
    UOpnd _ -> BRead KOperand
    UImm _ -> BRead KOperand
    UPtr _ -> BRead KData
    URd _ -> BRead KData
    URmwRd _ -> BRead KData
    UPull _ -> BRead KData
    UPullP -> BRead KData
    UPullN _ -> BRead KData
    UMvRd -> BRead KData
    UVec _ -> BRead KVector
    UWr _ -> BWrite True
    URmwWr _ -> BWrite True
    UPush _ -> BWrite True
    UMvWr -> BWrite True
    URmwMod -> if cE c then BWrite False else BIdle
    _ -> BIdle
  dat = case u of
    UWr 0 -> lo8 (storeValue c)
    UWr _ -> hi8 (storeValue c)
    URmwMod -> v0
    URmwWr 0 -> v0
    URmwWr _ -> cVal c !! (1 :: Int)
    UPush ps -> pushByte c ps
    UMvWr -> v0
    _ -> 0
  lock = case u of
    URmwRd _ -> True
    URmwMod -> True
    URmwWr _ -> True
    _ -> False

-- * A cycle's end

endCycle :: Cpu -> CpuIn -> Cpu
endCycle c inp =
  let need = cNmiNeed c || (ciNmi inp && not (cNmiLine c))
      irq = ciIrq inp && not (flag fI c)
  in c { cNmiLine = ciNmi inp, cNmiNeed = need, cPollPrev = cPollCur c
       , cPollCur = (if irq then 2 else 0) .|. (if need then 1 else 0) }

-- | A micro-op's effect, with what its read returned.
complete :: CoreConfig -> Cpu -> U -> Unsigned 8 -> Cpu
complete cfg c u d = case u of
  UOpnd k -> c { cOpnd = replace k d (cOpnd c), cPC = cPC c + 1 }
  UImm k -> c { cVal = replace k d (cVal c), cPC = cPC c + 1 }
  UPtr k -> c { cPtr = replace k d (cPtr c) }
  URd k -> c { cVal = replace k d (cVal c) }
  URmwRd k -> c { cVal = replace k d (cVal c) }
  URmwMod ->
    let eight = m8 c
        (r, c') = rmwOp (iOp (cInfo c)) eight (if eight then resize (cVal c !! (0 :: Int)) else v16 (cVal c)) c
    in c' { cVal = lo8 r :> hi8 r :> (cVal c !! (2 :: Int)) :> Nil }
  UPush s ->
    let c1 = c { cS = if pushOld cfg s then sDown c else cS c - 1 }
    in if s == PsPInt then setF fI True (setF fD False c1) { cPBR = 0 } else c1
  UPull k -> c { cS = sUp c, cVal = replace k d (cVal c) }
  UPullN k -> c { cS = cS c + 1, cVal = replace k d (cVal c) }
  UPullP -> setP d c { cS = sUp c }
  UVec k -> c { cVal = replace k d (cVal c) }
  UMvRd -> c { cVal = replace (0 :: Index 3) d (cVal c) }
  _ -> c

-- * Operations

rmwOp :: Op -> Bool -> Unsigned 16 -> Cpu -> (Unsigned 16, Cpu)
rmwOp op eight v c =
  let top = if eight then 0x80 else 0x8000 :: Unsigned 16
      lim = if eight then 0xFF else 0xFFFF :: Unsigned 16
      cin = flag fC c
      a = accOf eight c
      res r c' = (r, nzW eight r c')
  in case op of
       OASL -> res ((v `shiftL` 1) .&. lim) (setF fC (v .&. top /= 0) c)
       OLSR -> res (v `shiftR` 1) (setF fC (testBit v 0) c)
       OROL -> res (((v `shiftL` 1) .|. (if cin then 1 else 0)) .&. lim) (setF fC (v .&. top /= 0) c)
       OROR -> res ((v `shiftR` 1) .|. (if cin then top else 0)) (setF fC (testBit v 0) c)
       OINC -> res ((v + 1) .&. lim) c
       ODEC -> res ((v - 1) .&. lim) c
       OTSB -> (v .|. a, setF fZ (a .&. v == 0) c)
       _ -> (v .&. complement a .&. lim, setF fZ (a .&. v == 0) c)  -- TRB

-- | ADC and SBC, 8 or 16 bits, binary or decimal, as the spec's digits.
addSub :: Bool -> Bool -> Unsigned 16 -> Cpu -> Cpu
addSub isSub eight dIn c =
  let lim = if eight then 0xFF else 0xFFFF :: Unsigned 16
      a = accOf eight c
      d = if isSub then complement dIn .&. lim else dIn
      ci = if flag fC c then 1 else 0 :: Signed 24
      top = if eight then 0x80 else 0x8000 :: Signed 24
      sa = fromIntegral a :: Signed 24
      sd = fromIntegral d :: Signed 24
      dec = flag fD c
      nib :: Index 4 -> Signed 24
      nib k = 0xF `shiftL` (4 * fromEnum k)
      full :: Index 4 -> Signed 24
      full k = (0x10 `shiftL` (4 * fromEnum k)) - 1
      sixAt :: Index 4 -> Signed 24
      sixAt k = 6 `shiftL` (4 * fromEnum k)
      -- digit k: (the sum before its adjustment, after it)
      digit :: Index 4 -> Signed 24 -> Signed 24 -> (Signed 24, Signed 24)
      digit k carry acc0 =
        let r = (sa .&. nib k) + (sd .&. nib k) + (carry `shiftL` (4 * fromEnum k)) + acc0
            r' | isSub = if r <= full k then r - sixAt k else r
               | otherwise = if r >= (0xA `shiftL` (4 * fromEnum k)) then r + sixAt k else r
        in (r, r')
      step (carry, acc0) k =
        let (_, r') = digit k carry acc0
        in (if r' > full k then 1 else 0, r' .&. full k)
      s0 = step (ci, 0) 0
      s1 = step s0 1
      s2 = step s1 2
      (r0d, r1d) = if eight then digit 1 (fst s0) (snd s0) else digit 3 (fst s2) (snd s2)
      bin = sa + sd + ci
      (r0, r1) = if dec then (r0d, r1d) else (bin, bin)
      v = complement (sa `xor` sd) .&. (sa `xor` r0) .&. top /= 0
      res = resize (bitCoerce r1 :: Unsigned 24) .&. lim :: Unsigned 16
  in nzW eight res (setAcc eight res (setF fC (r1 > fromIntegral lim) (setF fV v c)))

cmpOp :: Bool -> Unsigned 16 -> Unsigned 16 -> Cpu -> Cpu
cmpOp eight r0 v0 c =
  let lim = if eight then 0xFF else 0xFFFF
      r = r0 .&. lim
      v = v0 .&. lim
  in nzW eight ((r - v) .&. lim) (setF fC (r >= v) c)

-- | The end of a program: the instruction's own effect.
finishOp :: Cpu -> Cpu
finishOp c = case iOp (cInfo c) of
  OORA -> setAN (accOf m c .|. val)
  OAND -> setAN (accOf m c .&. val)
  OEOR -> setAN (accOf m c `xor` val)
  OADC -> addSub False m val c
  OSBC -> addSub True m val c
  OLDA -> setAN val
  OCMP -> cmpOp m (accOf m c) val c
  OBIT ->
    let c1 = setF fZ (accOf m c .&. val == 0) c
    in if iMode (cInfo c) == MImm then c1
       else setF fN (if m then testBit val 7 else testBit val 15) (setF fV (if m then testBit val 6 else testBit val 14) c1)
  OLDX -> nzW x val c { cX = val }
  OLDY -> nzW x val c { cY = val }
  OCPX -> cmpOp x (cX c) val c
  OCPY -> cmpOp x (cY c) val c
  OASL | acc -> accRmw
  OLSR | acc -> accRmw
  OROL | acc -> accRmw
  OROR | acc -> accRmw
  OINC | acc -> accRmw
  ODEC | acc -> accRmw
  OINX -> incIdx True (cX c + 1)
  OINY -> incIdx False (cY c + 1)
  ODEX -> incIdx True (cX c - 1)
  ODEY -> incIdx False (cY c - 1)
  OTAX -> xferX (cA c) (\r c' -> c' { cX = r })
  OTAY -> xferX (cA c) (\r c' -> c' { cY = r })
  OTSX -> xferX (cS c) (\r c' -> c' { cX = r })
  OTXY -> xferX (cX c) (\r c' -> c' { cY = r })
  OTYX -> xferX (cY c) (\r c' -> c' { cX = r })
  OTXA -> nzW m (cX c) (setAcc m (cX c) c)
  OTYA -> nzW m (cY c) (setAcc m (cY c) c)
  OTXS -> c { cS = if cE c then 0x100 .|. (cX c .&. 0xFF) else cX c }
  OTCD -> nzW False (cA c) c { cD = cA c }
  OTDC -> nzW False (cD c) c { cA = cD c }
  OTCS -> c { cS = if cE c then 0x100 .|. (cA c .&. 0xFF) else cA c }
  OTSC -> nzW False (cS c) c { cA = cS c }
  OXBA -> let a' = (cA c `shiftR` 8) .|. (cA c `shiftL` 8) in nzW True (a' .&. 0xFF) c { cA = a' }
  OCLC -> setF fC False c
  OSEC -> setF fC True c
  OCLI -> setF fI False c
  OSEI -> setF fI True c
  OCLV -> setF fV False c
  OCLD -> setF fD False c
  OSED -> setF fD True c
  OXCE ->
    let cf = flag fC c
        c1 = setF fC (cE c) c { cE = cf }
    in normalise (if cf then setF fM True (setF fX True c1) else c1)
  OREP -> setP (cP c .&. complement (cOpnd c !! (0 :: Int))) c
  OSEP -> setP (cP c .|. (cOpnd c !! (0 :: Int))) c
  OWDM -> c { cPC = cPC c + 1 }
  OPLA -> nzW m val (setAcc m val c)
  OPLX -> nzW x val c { cX = val }
  OPLY -> nzW x val c { cY = val }
  OPLB -> nzW True (resize (cVal c !! (0 :: Int))) c { cDBR = cVal c !! (0 :: Int) }
  OPLD -> nzW False (v16 (cVal c)) c { cD = v16 (cVal c) }
  OJMP -> c { cPC = v16 (cOpnd c) }
  OJML -> c { cPC = v16 (cOpnd c), cPBR = cOpnd c !! (2 :: Int) }
  OJMPI -> c { cPC = v16 (cPtr c) }
  OJMPIX -> c { cPC = v16 (cPtr c) }
  OJSRIX -> c { cPC = v16 (cPtr c) }
  OJMLI -> c { cPC = v16 (cPtr c), cPBR = cPtr c !! (2 :: Int) }
  OJSR -> c { cPC = v16 (cOpnd c) }
  OJSL -> c { cPC = v16 (cOpnd c), cPBR = cOpnd c !! (2 :: Int) }
  ORTS -> c { cPC = v16 (cVal c) + 1 }
  ORTL -> c { cPC = v16 (cVal c) + 1, cPBR = cVal c !! (2 :: Int) }
  ORTI -> let c1 = c { cPC = v16 (cVal c) } in if cE c then c1 else c1 { cPBR = cVal c !! (2 :: Int) }
  OBRK -> c { cPC = v16 (cVal c) }
  OCOP -> c { cPC = v16 (cVal c) }
  ONMI -> c { cPC = v16 (cVal c) }
  OIRQ -> c { cPC = v16 (cVal c) }
  ORESET -> c { cPC = v16 (cVal c) }
  OBRL -> c { cPC = cPC c + v16 (cOpnd c) }
  OMVN -> move 1
  OMVP -> move (-1)
  op | isBranch op -> if branchTaken op c then c { cPC = branchTarget c } else c
     | otherwise -> c
 where
  m = m8 c
  x = x8 c
  acc = iMode (cInfo c) == MAcc
  wideNow = wide c
  val = if wideNow then v16 (cVal c) else resize (cVal c !! (0 :: Int))
  setAN r = nzW m r (setAcc m r c)
  accRmw = let (r, c') = rmwOp (iOp (cInfo c)) m (accOf m c) c in setAcc m r c'
  incIdx toX r0 =
    let r = if x then r0 .&. 0xFF else r0
    in nzW x r (if toX then c { cX = r } else c { cY = r })
  xferX v f = let r = if x then v .&. 0xFF else v in nzW x r (f r c)
  move :: Unsigned 16 -> Cpu
  move d =
    let lim = if x then 0xFF else 0xFFFF
        a' = cA c - 1
        c1 = c { cDBR = cOpnd c !! (0 :: Int), cX = (cX c + d) .&. lim, cY = (cY c + d) .&. lim, cA = a' }
    in if a' /= 0xFFFF then c1 { cPC = cPC c - 3 } else c1

-- * The sequencer

-- | The first active micro-op of the program at or after @from@.
firstActive :: Conds -> Vec 8 U -> Index 8 -> Maybe (Index 8)
firstActive k p from = findIndex id (imap (\j u -> testBit fromOn (fromEnum j) && active k u) p)
 where
  -- micro-ops from 'from' on, as a mask ('j >= from' would compare a
  -- constant for the last index: lint's CMPCONST)
  fromOn = (maxBound :: BitVector 8) `shiftL` fromEnum from

fetchNext :: Cpu -> Cpu
fetchNext c = c { cPhase = PhFetch, cStage = StReady, cReq = rd KOpcode (pcAddr c) }

-- | A hardware sequence (NMI, IRQ, reset): its first micro-op is an
-- internal cycle at PBR:PC.
startHw :: Cpu -> Info -> Cpu
startHw c info = c { cPhase = PhRun, cStage = StReady, cInfo = info, cT = 0, cReq = io (pcAddr c) }

-- | The next step: the interrupt pending, or the next opcode fetch.
dispatch :: Cpu -> Bool -> BitVector 2 -> Cpu
dispatch c noInt pending
  | not noInt && testBit pending 0 = startHw c { cNmiNeed = False } (Info ONMI MNone AccOther)
  | not noInt && testBit pending 1 = startHw c (Info OIRQ MNone AccOther)
  | otherwise = fetchNext c

-- | One clock. The cycle's end ('Just' its input) comes while the request
-- is on the bus ('StReady'); the next clock works out the next request,
-- which is on the bus ('cReq', 'cStage' 'StReady') from the clock after.
-- A bus cycle is at least two clocks.
cpuClock :: CoreConfig -> Cpu -> Maybe CpuIn -> (Cpu, Maybe StepKind)
cpuClock cfg c mIn = case cStage c of
  StReq -> (c { cStage = StReady, cReq = reqFor cfg c (progOf (cInfo c) !! cT c) }, Nothing)
  StWake -> (dispatch c False (cPollCur c), Nothing)
  StDone ->
    let op = iOp (cInfo c)
        c3 = normalise (finishOp c)
        kind = case op of
          ONMI -> SkNmi
          OIRQ -> SkIrq
          ORESET -> SkReset
          _ -> SkInstr
        stopped ph = c3 { cPhase = ph, cStage = StReady, cReq = Req BStopped 0 0 False }
        c4 = case op of
          OWAI -> stopped PhWait
          OSTP -> stopped PhStop
          _ -> dispatch c3 (op == ORESET) (cPollPrev c3)
    in (c4, Just kind)
  StReady -> case mIn of
    Nothing -> (c, Nothing)
    Just inp ->
      let c1 = endCycle c inp
          d = ciData inp
      in case cPhase c of
           PhStop -> (c1, Just SkStop)
           PhWait
             | ciIrq inp || cNmiNeed c1 -> (c1 { cStage = StWake }, Just SkWait)
             | otherwise -> (c1, Just SkWait)
           PhFetch ->
             let c2 = c1 { cPC = cPC c1 + 1, cInfo = decode d, cPhase = PhRun }
                 j = maybe 0 id (firstActive (conds c2) (progOf (cInfo c2)) 0)
             in (c2 { cT = j, cStage = StReq }, Nothing)
           PhRun ->
             let t = cT c1
                 p = progOf (cInfo c1)
                 nxt = if t == maxBound then Nothing else firstActive (conds c1) p (t + 1)
                 c2 = complete cfg c1 (p !! t) d
             in case nxt of
                  Just j -> (c2 { cT = j, cStage = StReq }, Nothing)
                  Nothing -> (c2 { cStage = StDone }, Nothing)

-- | A whole bus cycle (the tests' view): its end, then the clock that
-- works out the next request.
cpuCycle :: CoreConfig -> Cpu -> CpuIn -> (Cpu, CpuOut)
cpuCycle cfg c0 inp =
  let (c1, s1) = cpuClock cfg c0 (Just inp)
      (c2, s2) = cpuClock cfg c1 Nothing
  in (c2, CpuOut (cReq c2) (case s1 of Just k -> Just k; Nothing -> s2))
