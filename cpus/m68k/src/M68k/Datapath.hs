{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE StrictData #-}
{-|
The 68000 core's datapath (side track P, P.4g; docs/cpus/m68k.md): its
state (the register file, PC, SR, IR, IRC, the temporaries, the request,
and the sequencer's flags 'Ctl'), and 'datapath', which runs one command
('Cmd'): narrow selectors (each value a source, 'Src'), one ALU, an
address unit and a second adder, the PC's adder, a multiplier, a
divider. "M68k.Core"'s microsequencer builds the commands from
"M68k.Micro"'s words.
-}
module M68k.Datapath
  ( Ctl (..)
  , Cpu (..)
  , cpuPowerOn
  , usp
  , ssp
  , regD
  , regA
  , Src (..)
  , AOp (..)
  , AuOff (..)
  , WMode (..)
  , Wr (..)
  , noWr
  , SrOp (..)
  , PcOp (..)
  , IrOp (..)
  , XOp (..)
  , FcSel (..)
  , LenSel (..)
  , RqC (..)
  , Cmd (..)
  , noCmd
  , Ports (..)
  , datapath
  ) where

import Clash.Prelude

import M68k.Bus
import M68k.Ops

-- | The sequencer (and the CPU's flags): what control reads besides IR,
-- IRC, the extension word and SR.
data Ctl = Ctl
  { kPh      :: Ph
  , kRet     :: Ret
  , kK       :: Unsigned 4        -- ^ the step within a phase
  , kOp      :: Unsigned 16       -- ^ the instruction's opcode
  , kTrace   :: Bool              -- ^ SR's T at its start
  , kExc     :: Bool              -- ^ in exception processing: a fault halts
  , kBerr    :: Bool              -- ^ group 0 is a bus error
  , kSz      :: Sz
  , kWho     :: Who
  , kRead    :: Bool
  , kLoc     :: Loc
  , kJ       :: Unsigned 5        -- ^ MOVEM's list position
  , kLvl     :: Unsigned 3        -- ^ an interrupt's level
  , kStopped :: Bool
  , kHalted  :: Bool
  , kIplSeen :: Unsigned 3
  , kNew     :: Bool              -- ^ the pending request starts an instruction
  , kIpl     :: Unsigned 3        -- ^ the level at the end of the last transaction
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (NFDataX, BitPack)

data Cpu = Cpu
  { cK   :: Ctl
  , cRf  :: Vec 16 (Unsigned 32)  -- ^ D0-D7, A0-A7 (A7 the active stack pointer)
  , cOsp :: Unsigned 32           -- ^ the inactive stack pointer
  , cPc  :: Unsigned 32
  , cSr  :: Unsigned 16
  , cIr  :: Unsigned 16
  , cIrc :: Unsigned 16
  , cX   :: Unsigned 16           -- ^ the extension word being taken
  , cEa  :: Unsigned 32           -- ^ the operand's address
  , cV   :: Unsigned 32           -- ^ the operand read; the value written
  , cS   :: Unsigned 32           -- ^ a source kept
  , cT   :: Unsigned 32           -- ^ scratch
  , cU   :: Unsigned 32           -- ^ scratch
  , cBa  :: Unsigned 32           -- ^ the pending bus cycle's whole address
  , cReq :: Req
  , cBad :: Bool                  -- ^ simulation: the last step's command was bad
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (NFDataX, BitPack)

cpuPowerOn :: Cpu
cpuPowerOn = Cpu
  { cK = Ctl { kPh = PStop, kRet = RDone, kK = 0, kOp = 0, kTrace = False, kExc = False, kBerr = False
             , kSz = SW, kWho = WSrc, kRead = False, kLoc = LD, kJ = 0, kLvl = 0, kStopped = False
             , kHalted = False, kIplSeen = 0, kNew = True, kIpl = 0 }
  , cRf = repeat 0, cOsp = 0, cPc = 0, cSr = 0x2700, cIr = 0x4E71, cIrc = 0x4E71, cX = 0
  , cEa = 0, cV = 0, cS = 0, cT = 0, cU = 0, cBa = 0, cReq = Req KIdle 0 False 0 0 0, cBad = False }

supervisor :: Cpu -> Bool
supervisor c = testBit (cSr c) 13

usp, ssp :: Cpu -> Unsigned 32
usp c = if supervisor c then cOsp c else cRf c !! (15 :: Index 16)

ssp c = if supervisor c then cRf c !! (15 :: Index 16) else cOsp c

regD, regA :: Cpu -> Vec 8 (Unsigned 32)
regD c = takeI (cRf c)

regA c = dropI (cRf c)

-- | A value's source this step.
data Src
  = S0
  | SK              -- ^ the command's constant ('mK')
  | SIn             -- ^ the data just read
  | SInB            -- ^ its low byte
  | SInH            -- ^ it, shifted to the high word
  | SInDt           -- ^ it as the high word, the scratch's low word (ADDX's -(An) long)
  | SBa             -- ^ the pending bus cycle's whole address (a bus error's)
  | SDt4            -- ^ the scratch times four (a vector number's address)
  | SDtIn           -- ^ the scratch's high word and the data (a long's two words)
  | SDvIn           -- ^ the value's high word and the data
  | SDvShIn         -- ^ the value shifted by a byte, the data's low byte in (MOVEP)
  | SEa | SDv | SDs | SDt | SDu
  | SX              -- ^ the extension word
  | SXs             -- ^ sign-extended
  | SXb             -- ^ its low byte
  | SXH             -- ^ shifted to the high word
  | SDtX            -- ^ the scratch's high word and the extension word
  | SXIrc           -- ^ the extension word's high word and IRC (as this step leaves it)
  | SXIn            -- ^ the extension word's high word and the data
  | SIrcS           -- ^ IRC (as this step leaves it), sign-extended
  | SDx | SAx       -- ^ D and A of bits 9-11
  | SDy | SAy       -- ^ D and A of bits 0-2
  | SI              -- ^ the index register (the extension word's or IRC's)
  | SJ              -- ^ MOVEM's next register
  | SA7             -- ^ A7 (the active stack pointer)
  | SSpNew          -- ^ the stack pointer active after this step's SR change
  | SOsp
  | SPc | SPc2 | SPc4 -- ^ the PC (advanced if this step advances it), + 2, + 4
  | SSr
  | SAlu | SAu | SAu2 | SMul | SDiv | SLen
  | SDxByte         -- ^ a byte of D (bits 9-11): the one 'mK' names (MOVEP)
  | SOp             -- ^ (the microcode) the operand's value, where the sequencer says it is
  | SWhoA | SWhoD   -- ^ (the microcode) the operand's A or D (bits 0-2 or 9-11 by the sequencer's 'kWho')
  | SMvPre          -- ^ (the microcode) a MOVEM -(An) store's register (An: as it was)
  | SIackVec        -- ^ (the microcode) the acknowledge's vector (VPA: the autovector)
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFDataX, BitPack)

-- | The ALU's operation (its operands A and B, its size).
data AOp
  = OPass                 -- ^ A
  | OLogic                -- ^ A, N and Z from it (V, C cleared)
  | OAlu Alu              -- ^ B op A (Sub, Cmp: B - A)
  | OAluX Bool            -- ^ B + A + X, B - A - X (Z only cleared)
  | OUnary                -- ^ NEGX, CLR, NEG, NOT of B (by the opcode)
  | OShift                -- ^ the opcode's shift or rotate of B by A (a count)
  | OShiftMem             -- ^ a memory shift (a word by one) of B
  | OBcd Bool             -- ^ B + A + X, B - A - X in BCD
  | OBit                  -- ^ the opcode's bit operation on B, bit A; Z from it
  | OSwap | OExtW | OExtL -- ^ of B, N and Z
  | OAdd | OSubBA         -- ^ A + B, B - A (no flags)
  | ODec16                -- ^ B's low word less one (DBcc)
  | OSrOp                 -- ^ SR or, and, eor A by the opcode (to CCR or SR)
  | OSccVal               -- ^ 0xFF or 0 by the opcode's condition
  | OTasD                 -- ^ B with bit 7 set, N and Z from B's byte
  | OChkHi | OChkLo | OChkOk -- ^ CHK's flags (Dn.w in A)
  | ODivZ                 -- ^ a zero divisor's flags
  | ODiv                  -- ^ the divider's flags
  | OMovep                -- ^ B (n = 2: merged into A's high word)
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFDataX, BitPack)

-- | The address unit's offset.
data AuOff
  = AO0
  | AOK             -- ^ the command's offset constant ('mAuK')
  | AOStep Bool     -- ^ the operand size's step for the register ('mStepR'), down (True) or up
  | AOXs            -- ^ the extension word, sign-extended
  | AOIrcS          -- ^ IRC (as this step leaves it), sign-extended
  | AOIdxX          -- ^ the index of the extension word (its register, its byte)
  | AOIdxIrc        -- ^ the index of IRC
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFDataX, BitPack)

-- | A register write's width: all of it, its low byte or word (Dn), or a
-- word sign-extended (An).
data WMode = WFull | WLow Sz | WSext | WDivOk
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFDataX, BitPack)

data Wr = Wr
  { wOn :: Bool, wIdx :: Unsigned 4, wSrc :: Src, wMode :: WMode }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFDataX, BitPack)

noWr :: Wr
noWr = Wr False 0 S0 WFull

data SrOp
  = SrKeep
  | SrAlu           -- ^ the ALU's flags
  | SrSet Src       -- ^ a value (masked; S changing swaps the stack pointers)
  | SrCcr Src       -- ^ the condition codes from a value
  | SrExc           -- ^ an exception's: S set, T cleared
  | SrInt           -- ^ an interrupt's: S set, T cleared, the mask raised to 'mLvl'
  | SrExcDs         -- ^ group 0's: S set, T cleared, from the SR kept at the fault
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFDataX, BitPack)

data PcOp = PcKeep | PcSet Src | PcSetM4 Src
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFDataX, BitPack)

data IrOp = IrKeep | IrIrc | IrX
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFDataX, BitPack)

data XOp = XKeep | XIrc | XIn
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFDataX, BitPack)

-- | The function code: program or data space (by the new SR's S),
-- supervisor program (the reset's), the acknowledge's.
data FcSel = FProg | FData | FSProg | FIack
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFDataX, BitPack)

-- | An idle request's length: a constant, or a unit's.
data LenSel = LConst | LShift | LBit | LMul | LDiv | LDu
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFDataX, BitPack)

data RqC = RqC
  { qKind :: Kind, qFc :: FcSel, qByte :: Bool, qAddr :: Src, qData :: Src, qHi :: Bool
  , qLen :: LenSel, qLenK :: Unsigned 8, qPh :: Ph }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFDataX, BitPack)

data Cmd = Cmd
  { mK      :: Unsigned 32
  , mKUsed  :: Bool
  , mAlu    :: AOp, mAluA :: Src, mAluB :: Src, mAluSz :: Sz, mAluUsed :: Bool, mAluSextA :: Bool
  , mAuB    :: Src, mAuO :: AuOff, mAuK :: Unsigned 32, mStepR :: Unsigned 3, mAuUsed :: Bool
  , mAu2B   :: Src, mAu2K :: Unsigned 32, mAu2Used :: Bool
  , mMdA    :: Src                -- ^ the multiplier's and divider's operand (Dn of bits 9-11 the other)
  , mW1, mW2 :: Wr
  , mOsp    :: Maybe Src
  , mSr     :: SrOp
  , mLvl    :: Unsigned 3
  , mPcAdv  :: Bool               -- ^ the PC on by two (an extension word or the prefetch taken)
  , mPcSet  :: PcOp               -- ^ then set (a jump's)
  , mIr     :: IrOp, mIrcIn :: Bool, mX :: XOp
  , mA7Du   :: Bool               -- ^ A7 from the scratch, before the two writes (a push's)
  , mEa, mV, mS, mT, mU :: Src    -- ^ the temporaries' new sources (themselves: kept)
  , mRq     :: RqC
  , mFault  :: Bool               -- ^ an address error is possible (read or write, a word)
  , mBad    :: Bool
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (NFDataX, BitPack)

noCmd :: Cmd
noCmd = Cmd
  { mK = 0, mKUsed = False, mAlu = OPass, mAluA = S0, mAluB = S0, mAluSz = SL, mAluUsed = False, mAluSextA = False
  , mAuB = S0, mAuO = AO0, mAuK = 0, mStepR = 0, mAuUsed = False
  , mAu2B = S0, mAu2K = 0, mAu2Used = False, mMdA = S0
  , mW1 = noWr, mW2 = noWr, mOsp = Nothing, mSr = SrKeep, mLvl = 0
  , mPcAdv = False, mPcSet = PcKeep, mIr = IrKeep, mIrcIn = False, mX = XKeep, mA7Du = False
  , mEa = SEa, mV = SDv, mS = SDs, mT = SDt, mU = SDu
  , mRq = RqC KIdle FData False S0 S0 False LConst 0 PStop, mFault = False, mBad = False }

data Ports = Ports
  { pDx, pAx, pDy, pAy, pI, pJ :: Unsigned 32 }

lo16s :: Unsigned 32 -> Signed 16
lo16s v = unpack (pack (resize v :: Unsigned 16))

-- | A push's last step writes A7 (from the scratch): an instruction
-- starting in the same step reads the new A7 (registered state decides,
-- not the command).
a7Bypass :: Cpu -> Bool
a7Bypass c = kPh (cK c) == PPush && kK (cK c) == 1

-- | The register file as an instruction starting this step sees it.
rfSeen :: Cpu -> Vec 16 (Unsigned 32)
rfSeen c = if a7Bypass c then replace (15 :: Index 16) (cU c) (cRf c) else cRf c

-- | The divider (32 / 16, the signs outside): DIVU's or DIVS's result
-- word pair (the remainder high), whether it fits, its clocks less four.
{-# OPAQUE divider #-}
divider :: Bool -> Unsigned 32 -> Unsigned 32 -> (Unsigned 32, Bool, Unsigned 8)
divider signed dvd0 s0 =
  let s16 = resize s0 :: Unsigned 16
      dvs = unpack (pack dvd0) :: Signed 32
      sss = unpack (pack s16) :: Signed 16
      negD = signed && dvs < 0
      negS = signed && sss < 0
      n = if negD then negate dvd0 else dvd0
      dsr = resize (if negS then negate s16 else s16) :: Unsigned 32
      big = (n `shiftR` 16) >= dsr
      -- 16 restoring steps on the high word: per step its carry and its
      -- quotient bit
      stepF (rem', qacc, cost, i) _ =
        let t = (resize rem' `shiftL` 1) .|. (if testBit n (15 - fromIntegral i) then 1 else 0) :: Unsigned 17
            cy = testBit t 16
            ge = resize t >= dsr || cy
            rem'' = if ge then resize (resize t - resize dsr :: Unsigned 32) else resize t :: Unsigned 16
            c' = if i < 15 then cost + (if cy then 0 else if ge then 1 else 2) else cost
        in (rem'', (qacc `shiftL` 1) .|. (if ge then 1 else 0), c', i + 1 :: Unsigned 5)
      (rm, q, divuCost, _) = foldl stepF (resize (n `shiftR` 16) :: Unsigned 16, 0 :: Unsigned 16, 38 :: Unsigned 8, 0) (replicate d16 ())
      -- DIVU
      uOk = not big
      uClk = if big then 10 else divuCost * 2
      -- DIVS
      qNeg = negD /= negS
      sOk = not big && (if qNeg then q <= 0x8000 else q <= 0x7FFF)
      qs = if qNeg then negate q else q
      rs = if negD then negate rm else rm
      m0 = 6 + (if negD then 1 else 0) :: Unsigned 8
      m1 = m0 + 55 + (if not negS then (if not negD then maxBound else 1) else 0)
      m2 = m1 + 15 - fromIntegral (popCount ((q `shiftR` 1) .&. 0x7FFF))
      sClk = if not sOk then (m0 + 2) * 2 else m2 * 2
  in if signed
       then ((resize rs `shiftL` 16) .|. resize qs, sOk, sClk - 4)
       else ((resize rm `shiftL` 16) .|. resize q, uOk, uClk - 4)

-- | The ALU: an operation of A and B at a size, the opcode's details, SR;
-- the result and the new SR.
{-# OPAQUE aluUnit #-}
aluUnit :: AOp -> Sz -> Unsigned 16 -> Unsigned 32 -> Unsigned 32 -> Unsigned 16 -> Unsigned 32 -> Bool -> Bool
        -> (Unsigned 32, Unsigned 16)
aluUnit o sz op a b sr dres dok dz = case o of
  OPass -> (a, sr)
  OLogic -> (a, logicSr sz a sr)
  OAlu k -> alu k sz a b sr
  OAluX isAdd -> aluX isAdd sz a b sr
  OUnary -> unaryOp op sz b sr
  OShift -> shiftOp (shiftKind (op `shiftR` 3)) (testBit op 8) sz (resize a) b sr
  OShiftMem -> shiftOp (shiftKind (op `shiftR` 9)) (testBit op 8) SW 1 b sr
  OBcd isAdd -> bcd isAdd a b sr
  OBit ->
    let n = fromIntegral (a .&. (if sz == SB then 7 else 31)) :: Int
        z = not (testBit b n)
    in (bitApply ((op `shiftR` 6) .&. 3) n b, if z then setBit sr 2 else clearBit sr 2)
  OSwap -> let v = (b `shiftL` 16) .|. (b `shiftR` 16) in (v, logicSr SL v sr)
  OExtW -> let v = sext8 b .&. 0xFFFF in (v, logicSr SW v sr)
  OExtL -> let v = sext16 b in (v, logicSr SL v sr)
  OAdd -> (a + b, sr)
  OSubBA -> (b - a, sr)
  ODec16 -> ((b - 1) .&. 0xFFFF, sr)
  OSrOp ->
    let v = resize a :: Unsigned 16
        f = case op .&. 0x0F00 of
          0x0000 -> sr .|. v
          0x0200 -> sr .&. v
          _ -> sr `xor` v
    in (resize f, sr)
  OSccVal -> (if cond (resize (op `shiftR` 8)) sr then 0xFF else 0, sr)
  OTasD -> (b .|. 0x80, logicSr SB b sr)
  OChkHi -> (0, (sr .&. 0xFFF0) .|. (if lo16s a < 0 then 8 else 0) .|. chkZ)
  OChkLo -> (0, (sr .&. 0xFFF0) .|. 8 .|. chkZ)
  OChkOk -> (0, (sr .&. 0xFFF8) .|. chkZ)
  ODivZ -> (0, sr .&. 0xFFF0)
  ODiv
    | dz -> (0, sr .&. 0xFFF0)
    | dok -> (0, logicSr SW dres sr)
    | otherwise -> (0, (sr .&. 0xFFFC) .|. 2)
  OMovep -> (b, sr)
 where
  chkZ = if lo16s a == 0 then 4 else 0

-- | A source among a field's own: a short multiplexer.
pick :: KnownNat n => Vec n Src -> (Src -> Unsigned 32) -> Src -> (Unsigned 32, Bool)
pick allowed env s = case findIndex (== s) allowed of
  Just i -> (map env allowed !! i, True)
  Nothing -> (0, False)

-- Each field's sources (the microcode's: m68k-lockstep fails on one
-- missing).
au2Srcs :: Vec 5 Src
au2Srcs = S0 :> SA7 :> SEa :> SDs :> SPc :> Nil

mdSrcs :: Vec 9 Src
mdSrcs = S0 :> SIn :> SX :> SDy :> SDv :> SDx :> SInB :> SDvIn :> SDtX :> Nil

aluASrcs :: Vec 17 Src
aluASrcs = S0 :> SK :> SIn :> SInB :> SDvIn :> SX :> SXb :> SDtX :> SDv :> SDs :> SDx :> SDy :> SAx :> SAy :> SSr :> SMul :> SInDt :> Nil

aluBSrcs :: Vec 12 Src
aluBSrcs = S0 :> SDx :> SDy :> SAx :> SAy :> SIn :> SInB :> SDvIn :> SX :> SDtX :> SDv :> SInDt :> Nil

srSrcs :: Vec 8 Src
srSrcs = SAlu :> SX :> SIn :> SInB :> SDvIn :> SDv :> SDy :> SDtX :> Nil

auBSrcs :: Vec 10 Src
auBSrcs = S0 :> SAx :> SAy :> SDt :> SSpNew :> SEa :> SDu :> SPc2 :> SAu2 :> SA7 :> Nil

wrSrcs :: Vec 23 Src
wrSrcs = SAlu :> SAu :> SAu2 :> SDu :> SDtIn :> SIn :> SOsp :> SSr :> SDx :> SDy :> SAx :> SAy :> SDiv :> SMul :> SDs :> SEa :> SDv :> SInB :> SDvIn :> SX :> SDtX :> SXs :> SDvShIn :> Nil

pcSrcs :: Vec 6 Src
pcSrcs = SDt :> SAu :> SDtIn :> SIrcS :> SXIn :> SAy :> Nil

eaSrcs :: Vec 12 Src
eaSrcs = SEa :> SAu :> SAu2 :> SAx :> SAy :> SXs :> SDtX :> SXIrc :> SK :> SDt4 :> SA7 :> SBa :> Nil

vSrcs :: Vec 21 Src
vSrcs = SDv :> SIn :> SInB :> SDvIn :> SX :> SDtX :> SDy :> SAy :> SDx :> SAx :> SInH :> SDvShIn :> SAlu :> SDs :> SPc :> SJ :> SDt :> SAu2 :> S0 :> SXs :> SPc2 :> Nil

sSrcs :: Vec 21 Src
sSrcs = SDs :> SDx :> SDy :> SK :> SXb :> SPc2 :> SPc4 :> SPc :> SSr :> SAu :> SX :> SIn :> SInB :> SDvIn :> SDtX :> SAy :> SAx :> SXs :> SAu2 :> SEa :> SInDt :> Nil

tSrcs :: Vec 12 Src
tSrcs = SDt :> SAu :> SPc2 :> SXH :> SInH :> SDtIn :> SIn :> SK :> SAy :> SInB :> SIrcS :> SXIn :> Nil

uSrcs :: Vec 4 Src
uSrcs = SDu :> SAu2 :> SSpNew :> SLen :> Nil

addrSrcs :: Vec 18 Src
addrSrcs = S0 :> SPc4 :> SPc2 :> SAx :> SAy :> SAu :> SAu2 :> SXs :> SDtX :> SEa :> SDt :> SDtIn :> SK :> SDt4 :> SA7 :> SIrcS :> SXIn :> SXIrc :> Nil

dataSrcs :: Vec 22 Src
dataSrcs = S0 :> SAlu :> SDv :> SDs :> SJ :> SDt :> SPc :> SK :> SDxByte :> SEa :> SAu2 :> SAy :> SIn :> SInB :> SDvIn :> SX :> SDtX :> SDy :> SDx :> SAx :> SXs :> SPc2 :> Nil

-- | A command run: its sources evaluated (each field's among its own few,
-- 'pick'; one outside a field's list is 0 and marks the step bad, the
-- tests fail), the units, then SR (an S change swaps the stack pointers,
-- after the register writes), the register file, PC, IR, IRC, the
-- temporaries, the request. An odd word address in the request is the
-- address error: the step's register, SR and PC changes stay, the
-- request becomes group 0's idle clocks and the phase 'PG0' (in exception
-- processing: halted, 'PBound').
datapath :: Cpu -> CpuIn -> Ports -> Ctl -> Cmd -> Cpu
datapath c ci p k m =
  let op = kOp k
      d = ciData ci
      din = w32 d
      rf = cRf c
      pcBase = if mPcAdv m then cPc c + 2 else cPc c
      irc' = if mIrcIn m then d else cIrc c
      x = cX c
      -- the sources before the units
      v0 s = case s of
        S0 -> 0
        SK -> mK m
        SIn -> din
        SInB -> din .&. 0xFF
        SInH -> din `shiftL` 16
        SDtIn -> cT c .|. din
        SDvIn -> cV c .|. din
        SDvShIn -> (cV c `shiftL` 8) .|. (din .&. 0xFF)
        SInDt -> (din `shiftL` 16) .|. (cT c .&. 0xFFFF)
        SEa -> cEa c
        SDv -> cV c
        SDs -> cS c
        SDt -> cT c
        SDt4 -> cT c `shiftL` 2
        SDu -> cU c
        SBa -> cBa c
        SX -> w32 x
        SXs -> sext16 (w32 x)
        SXb -> w32 x .&. 0xFF
        SXH -> w32 x `shiftL` 16
        SDtX -> cT c .|. w32 x
        SXIrc -> (w32 x `shiftL` 16) .|. w32 irc'
        SXIn -> (w32 x `shiftL` 16) .|. din
        SIrcS -> sext16 (w32 irc')
        SDx -> pDx p
        SAx -> pAx p
        SDy -> pDy p
        SAy -> pAy p
        SI -> pI p
        SJ -> pJ p
        SA7 -> rfSeen c !! (15 :: Index 16)
        SOsp -> cOsp c
        SPc -> pcBase
        SPc2 -> pcBase + 2
        SPc4 -> pcBase + 4
        SSr -> w32 (cSr c)
        SDxByte -> (pDx p `shiftR` (8 * fromIntegral (mK m .&. 3))) .&. 0xFF
        _ -> 0
      -- the second adder; the multiplier and divider
      (au2B', ok1) = pick au2Srcs v0 (mAu2B m)
      au2R = au2B' + mAu2K m
      (mdA, ok2) = pick mdSrcs v0 (mMdA m)
      mS16 = mdA .&. 0xFFFF
      mD16 = pDx p .&. 0xFFFF
      signed = testBit op 8
      mulR = if signed then sext16 mS16 * sext16 mD16 else mS16 * mD16
      mulN = if signed then popCount ((mS16 `xor` (mS16 `shiftL` 1)) .&. 0xFFFF) else popCount mS16
      (divR, divOk, divClk) = divider signed (pDx p) mdA
      -- a zero divisor (the microcode runs DIVU/DIVS's step the same for one):
      -- four idle clocks, N Z V C cleared, Dn kept
      divZ = mdA .&. 0xFFFF == 0
      v1 s = case s of
        SAu2 -> au2R
        SMul -> mulR
        SDiv -> divR
        _ -> v0 s
      -- the ALU
      (aluA0, ok3) = pick aluASrcs v1 (mAluA m)
      aluA = if mAluSextA m then sext16 aluA0 else aluA0
      (aluB, ok4) = pick aluBSrcs v1 (mAluB m)
      (aluR, aluSr) = aluUnit (mAlu m) (mAluSz m) op aluA aluB (cSr c) (divR .&. 0xFFFF) divOk divZ
      v2 s = case s of
        SAlu -> aluR
        _ -> v1 s
      -- SR, and the stack pointer it leaves active
      sr0 = cSr c
      srSrc = case mSr m of
        SrSet s -> s
        SrCcr s -> s
        _ -> SAlu
      (srV, ok5) = pick srSrcs v2 srSrc
      sr' = case mSr m of
        SrKeep -> sr0
        SrAlu -> aluSr
        SrSet _ -> resize srV .&. srMask
        SrCcr _ -> (sr0 .&. 0xFF00) .|. (resize srV .&. 0x1F)
        SrExc -> ((sr0 .|. 0x2000) .&. 0x7FFF) .&. srMask
        SrExcDs -> ((resize (cS c) .|. 0x2000) .&. 0x7FFF) .&. srMask
        SrInt -> (((sr0 .|. 0x2000) .&. 0x78FF) .|. (resize (mLvl m) `shiftL` 8)) .&. srMask
      swap = testBit sr' 13 /= testBit sr0 13
      spNew = if swap then cOsp c else rf !! (15 :: Index 16)
      v3 s = case s of
        SSpNew -> spNew
        _ -> v2 s
      -- the address unit
      e = case mAuO m of
        AOIdxIrc -> cIrc c
        _ -> x
      ixv = let xr = pI p in if testBit e 11 then xr else sext16 xr
      auOff = case mAuO m of
        AO0 -> 0
        AOK -> mAuK m
        AOStep down -> let st = stepOf (kSz k) (mStepR m) in if down then negate st else st
        AOXs -> sext16 (w32 x)
        AOIrcS -> sext16 (w32 irc')
        AOIdxX -> ixv + sext8 (w32 e)
        AOIdxIrc -> ixv + sext8 (w32 e)
      (auBv, ok6) = pick auBSrcs v3 (mAuB m)
      auR = auBv + auOff
      -- the length unit
      n6 = aluA .&. 63
      lenR = case qLen (mRq m) of
        LConst -> qLenK (mRq m)
        LShift -> (if mAluSz m == SL then 4 else 2) + 2 * resize n6
        LBit ->
          let bk = (op `shiftR` 6) .&. 3
              n = aluA .&. 31
          in if bk == 0 then 2 else if bk == 2 then (if n < 16 then 4 else 6) else (if n < 16 then 2 else 4)
        LMul -> 34 + 2 * fromIntegral mulN
        LDiv -> if divZ then 4 else divClk
        LDu -> resize (cU c)
      full s = case s of
        SAu -> auR
        SLen -> resize lenR
        _ -> v3 s
      -- the register file: A7 from the scratch, then the two writes, then
      -- the swap
      (w1v, ok7) = pick wrSrcs full (wSrc (mW1 m))
      (w2v, ok8) = pick wrSrcs full (wSrc (mW2 m))
      apply w v r =
        if not (wOn w) then r
        else let old = r !! wIdx w
                 nv = case wMode w of
                   WFull -> v
                   WLow sz -> mergeD sz old v
                   WSext -> sext16 v
                   WDivOk -> if divOk && not divZ then v else old
             in replace (wIdx w) nv r
      rfA7 = if mA7Du m then replace (15 :: Index 16) (cU c) rf else rf
      rfW = apply (mW2 m) w2v (apply (mW1 m) w1v rfA7)
      osp1 = maybe (cOsp c) (const (pAy p)) (mOsp m)
      (rf', osp') = if swap then (replace (15 :: Index 16) osp1 rfW, rfW !! (15 :: Index 16)) else (rfW, osp1)
      pcSrc = case mPcSet m of
        PcSet s -> s
        PcSetM4 s -> s
        PcKeep -> SDt
      (pcV, ok9) = pick pcSrcs full pcSrc
      pc' = case mPcSet m of
        PcKeep -> pcBase
        PcSet _ -> pcV
        PcSetM4 _ -> pcV - 4
      ir' = case mIr m of
        IrKeep -> cIr c
        IrIrc -> cIrc c
        IrX -> x
      x' = case mX m of
        XKeep -> x
        XIrc -> irc'
        XIn -> d
      -- the request
      q = mRq m
      (addr, ok10) = pick addrSrcs full (qAddr q)
      (dv, ok11) = pick dataSrcs full (qData q)
      (ea', ok12) = pick eaSrcs full (mEa m)
      (vv', ok13) = pick vSrcs full (mV m)
      (ss', ok14) = pick sSrcs full (mS m)
      (tt', ok15) = pick tSrcs full (mT m)
      (uu', ok16) = pick uSrcs full (mU m)
      okAll = ok1 && ok2 && ok3 && ok4 && ok5 && ok6 && (ok7 || not (wOn (mW1 m))) && (ok8 || not (wOn (mW2 m)))
              && ok9 && ok10 && ok11 && ok12 && ok13 && ok14 && ok15 && ok16
              && (case mOsp m of { Just s -> s == SAy; Nothing -> True })
      dw = resize (if qHi q then dv `shiftR` 16 else dv) :: Unsigned 16
      sup = testBit sr' 13
      fc = case qFc q of
        FProg -> if sup then 6 else 2
        FData -> if sup then 5 else 1
        FSProg -> 6
        FIack -> 7
      busC = qKind q == KRead || qKind q == KWrite
      req = Req { rqKind = qKind q, rqFc = fc, rqByte = qByte q, rqAddr = resize addr
                , rqData = if qByte q then dw .&. 0xFF else dw
                , rqLen = if qKind q == KIdle then lenR else qLenK q }
      c' = c { cK = k, cRf = rf', cOsp = osp', cPc = pc', cSr = sr', cIr = ir', cIrc = irc', cX = x'
             , cEa = ea', cV = vv', cS = ss', cT = tt', cU = uu'
             , cBa = if busC then addr else cBa c, cReq = req, cBad = mBad m || not okAll }
      -- the address error: an odd word address
      odd' = mFault m && testBit addr 0
      rd = qKind q == KRead
      prog = qFc q == FProg || qFc q == FSProg
      status = (op .&. 0xFFE0) .|. (if rd then 0x10 else 0) .|. (if prog then 0x08 else 0) .|. resize fc
  in if not odd' then c'
     else if kExc k then c' { cK = k { kHalted = True, kPh = PBound }, cReq = cReq c, cBa = cBa c }
     else c' { cK = k { kExc = True, kBerr = False, kK = 0, kPh = PG0 }
             , cReq = Req KIdle 0 False 0 0 4, cBa = cBa c
             , cEa = addr, cT = w32 status, cS = w32 sr', cV = pc' }
