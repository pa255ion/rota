{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE StrictData #-}
{-|
The 68000 in Clash (side track P; docs/cpus/m68k.md, "The core" and
"P.4h"): "M68k.Micro"'s microcode sequenced, each word resolved with the
opcode into a command and run on "M68k.Datapath". One transition a bus
transaction, the same transactions as "M68k.Spec" makes on its bus (the
lockstep tests: @m68k-lockstep@).

The microaddress ('sU') is the word for the pending transaction's end. An
instruction's end ('UEnd') is the boundary: its own actions (the
prefetch's, a jump's, a push's), then (in 'cpuStep') the next: the
trace exception, a halted or stopped CPU's turn, an interrupt, or the
next instruction's entry word ("M68k.Micro"'s 'entry'). 'cpuFinish' stops
at the boundary instead, for the tests.
-}
module M68k.Core
  ( Seq (..)
  , Cpu (..)
  , cpuPowerOn
  , cpuStep
  , cpuFinish
  , begin
  , reset
  , atBound
  , halted
  , badStep
    -- * Clocks
  , M68kT (..)
  , m68kPowerOn
  , m68kTick
  , m68kDone
  ) where

import Clash.Prelude hiding (Word)

import M68k.Bus (CpuIn (..), Kind (..), Loc (..), Ph (..), Req (..))
import M68k.Datapath (AuOff (..), Cmd (..), FcSel (..), PcOp (..), Ports (..), RqC (..), Src (..), SrOp (..), WMode (..), Wr (..))
import qualified M68k.Datapath as C2
import M68k.Micro
import M68k.Ops hiding (alu)

-- | The microsequencer's state.
data Seq = Seq
  { sU     :: U        -- ^ the word for the pending transaction's end
  , sRet   :: Cont     -- ^ an operand routine's continuation
  , sOpv   :: Opv      -- ^ where the operand's value is
  , sBound :: Bool     -- ^ ('cpuFinish') at an instruction boundary
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFDataX

data Cpu = Cpu
  { cC  :: C2.Cpu      -- ^ the datapath's state (and the sequencer's flags, 'C2.Ctl')
  , cSq :: Seq
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFDataX

cpuPowerOn :: Cpu
cpuPowerOn = Cpu C2.cpuPowerOn (Seq (UEnd EIdle) CTst OvDv True)

atBound :: Cpu -> Bool
atBound = sBound . cSq

halted :: Cpu -> Bool
halted = C2.kHalted . C2.cK . cC

badStep :: Cpu -> Bool
badStep = C2.cBad . cC

-- | The reset: supervisor mode, the interrupts masked, the vectors read.
reset :: Cpu -> Cpu
reset (Cpu c _) =
  let k = C2.cK c
  in Cpu c { C2.cK = k { C2.kExc = True, C2.kTrace = False, C2.kK = 0, C2.kNew = True, C2.kStopped = False, C2.kHalted = False }
           , C2.cSr = 0x2700, C2.cRf = replace (15 :: Index 16) 0 (C2.cRf c)
           , C2.cReq = Req KRead 6 False 0 0 0 }
         (Seq (UReset 0) CTst OvDv False)

cpuStep :: CpuIn -> Cpu -> Cpu
cpuStep = stepWith True

cpuFinish :: CpuIn -> Cpu -> Cpu
cpuFinish = stepWith False

-- * The step

-- | What a step runs: a word, the sequencer's flags it starts from (an
-- entry's, or as they are), the continuation and operand place.
data Run = Run
  { rU   :: U
  , rK   :: C2.Ctl
  , rRet :: Cont
  , rOpv :: Opv
  }

stepWith :: Bool -> CpuIn -> Cpu -> Cpu
stepWith chain ci c =
  let c2 = cC c
      s = cSq c
      k0 = (C2.cK c2) { C2.kNew = False, C2.kIpl = ciIpl ci }
      c2k = c2 { C2.cK = k0 }
      rq = C2.cReq c2
      berr = ciBerr ci && (rqKind rq == KRead || rqKind rq == KWrite)
  in if berr
       then if C2.kExc k0
              then Cpu c2k { C2.cK = k0 { C2.kHalted = True } } s { sU = UEnd EIdle, sBound = True }
              else runWord ci (Cpu c2k s) (Run UBerr k0 (sRet s) (sOpv s))
       else case sU s of
         UEnd e ->
           let c2e = applyEnd e ci c2k
           in if chain then runWord ci (Cpu c2e s) (decide ci c2e s)
              -- the tests' boundary: after the trace exception, if T was set
              else if C2.kTrace (C2.cK c2e) then runWord ci (Cpu c2e s) (decide ci c2e s)
              else Cpu c2e s { sU = UEnd EIdle, sBound = True }
         l -> runWord ci (Cpu c2k s) (Run l k0 (sRet s) (sOpv s))

-- | Start from a loaded state (at a boundary), with the level on the bus.
begin :: Unsigned 3 -> Cpu -> Cpu
begin ipl c =
  let ci = CpuIn 0 False ipl False
      c2 = cC c
      k = (C2.cK c2) { C2.kIpl = ipl }
      c2k = c2 { C2.cK = k }
  in runWord ci (Cpu c2k (cSq c)) (decide ci c2k (cSq c))

-- | An instruction's end's own actions, as the boundary step starts.
applyEnd :: End -> CpuIn -> C2.Cpu -> C2.Cpu
applyEnd e ci c = case e of
  EPf -> c { C2.cPc = C2.cPc c + 2, C2.cIr = C2.cIrc c, C2.cIrc = ciData ci }
  EStop -> (applyEnd EPf ci c) { C2.cK = (C2.cK c) { C2.kStopped = True } }
  EJump -> c { C2.cPc = C2.cT c, C2.cIr = C2.cX c, C2.cIrc = ciData ci }
  ERefill -> c { C2.cPc = C2.cPc c + 2, C2.cIr = C2.cX c, C2.cIrc = ciData ci }
  EPush -> c { C2.cRf = replace (15 :: Index 16) (C2.cU c) (C2.cRf c) }
  _ -> c

-- | The boundary: the trace exception, a halted or stopped CPU's turn, an
-- interrupt, or the next instruction's entry.
decide :: CpuIn -> C2.Cpu -> Seq -> Run
decide ci c2 s =
  let k0 = C2.cK c2
      sr = C2.cSr c2
      ir = C2.cIr c2
      lvl = ciIpl ci
      mask = resize (sr `shiftR` 8) :: Unsigned 3
      pending = (lvl > 0 && lvl < 7 && lvl > mask) || (lvl == 7 && C2.kIplSeen k0 < 7)
      kd = k0 { C2.kNew = True, C2.kExc = False, C2.kIplSeen = lvl, C2.kK = 0 }
      dy = C2.cRf c2 !! (resize (field ir 0) :: Unsigned 4)
      (eu, eo) = entry ir sr (resize dy == (0 :: Unsigned 16))
  in if C2.kTrace k0 then Run UTrace k0 { C2.kTrace = False } (sRet s) (sOpv s)
     else if C2.kHalted k0 then Run UStopWait kd (sRet s) (sOpv s)
     else if pending then Run UIntStart kd { C2.kExc = True, C2.kTrace = False, C2.kLvl = lvl } (sRet s) (sOpv s)
     else if C2.kStopped k0 then Run UStopWait kd (sRet s) (sOpv s)
     else
       let kn = kd { C2.kOp = ir, C2.kTrace = testBit sr 15 }
       in Run eu (ctlApply ir kn eo) (maybe (sRet s) id (oRet eo)) (maybe (sOpv s) id (oOpv eo))

-- | A word's (or an entry's) changes to the sequencer's flags.
ctlApply :: Unsigned 16 -> C2.Ctl -> CtlOp -> C2.Ctl
ctlApply op k o =
  let k1 = k { C2.kWho = maybe (C2.kWho k) id (oWho o)
             , C2.kSz = maybe (C2.kSz k) (szOfS op (C2.kSz k)) (oSz o)
             , C2.kLoc = maybe (C2.kLoc k) id (oLoc o)
             , C2.kExc = maybe (C2.kExc k) id (oExc o)
             , C2.kBerr = maybe (C2.kBerr k) id (oBerr o)
             , C2.kStopped = maybe (C2.kStopped k) id (oStopped o)
             , C2.kTrace = if oTraceOff o then False else C2.kTrace k
             , C2.kK = case oK o of
                 KKeep -> C2.kK k
                 KSet n -> n
                 KInc -> C2.kK k + 1 }
  in k1

-- | A size by kind, for an opcode (and the operand routine's).
szOfS :: Unsigned 16 -> Sz -> ZS -> Sz
szOfS op kz z = case z of
  ZOp -> szOf op
  ZMove -> moveSz op
  ZK -> kz
  ZB -> SB
  ZW -> SW
  ZL -> SL
  ZAddr -> if testBit op 8 then SL else SW
  ZMovem -> if testBit op 6 then SL else SW

-- | MOVEM's mask and list position for the step's register: the mask just
-- taken (the extension word) at the start, the mask kept at the start,
-- or the register after the last.
movemFrom :: U -> C2.Cpu -> (Unsigned 16, Unsigned 5)
movemFrom u c = case u of
  UMovemExt {} -> (C2.cX c, 0)
  UMvNext {} -> (resize (C2.cS c), C2.kJ (C2.cK c) + 1)
  _ -> (resize (C2.cS c), 0)

nextSet :: Unsigned 16 -> Unsigned 5 -> Unsigned 5
nextSet mask i =
  let left = if i >= 16 then 0 else mask .&. complement ((1 `shiftL` fromIntegral i) - 1)
  in fromIntegral (countTrailingZeros left)

-- | Run a word: resolved with the opcode, the datapath, an address
-- error, the next word.
runWord :: CpuIn -> Cpu -> Run -> Cpu
runWord ci c r =
  let c2 = cC c
      u = rU r
      w = micro u
      k1a = ctlApply op (rK r) (uCtl w)
      -- MOVEM's list position: to this step's register
      k1 = k1a { C2.kJ = case oJ (uCtl w) of
                   JKeep -> C2.kJ k1a
                   JZero -> 0
                   JNext -> j
                   JInc -> C2.kJ k1a + 1 }
      op = C2.kOp (rK r)
      -- the operand's continuation and place after this word
      ret' = maybe (rRet r) id (oRet (uCtl w))
      opv = maybe (rOpv r) id (oOpv (uCtl w))
      sz' = C2.kSz k1
      who = C2.kWho k1
      onX = who == WDst || who == WPostX
      -- the operand's own register: its routine's (before this word names
      -- another operand)
      who0 = C2.kWho (rK r)
      onX0 = who0 == WDst || who0 == WPostX
      f9 = resize (field op 9) :: Unsigned 4
      f0 = resize (field op 0) :: Unsigned 4
      -- MOVEM's register this step
      (mask, start) = movemFrom u c2
      j = nextSet mask start
      pre = not (testBit op 10) && field op 3 == 4
      jIdx = resize (if pre then 15 - j else j) :: Unsigned 4
      -- the read ports (the index: IRC's for JMP/JSR's target, else the
      -- extension word's)
      rf = C2.cRf c2
      ixE = case u of
        UJt {} -> C2.cIrc c2
        UJtS {} -> C2.cIrc c2
        _ -> C2.cX c2
      ix = (if testBit ixE 15 then 8 else 0) + resize (ixE `shiftR` 12 .&. 7) :: Unsigned 4
      ps = Ports { pDx = rf !! f9, pAx = rf !! (8 + f9), pDy = rf !! f0, pAy = rf !! (8 + f0)
                 , pI = rf !! ix, pJ = rf !! jIdx }
      -- the symbolic sources
      src s = case s of
        SOp -> case opv of
          OvIn -> SIn
          OvInB -> SInB
          OvDvIn -> SDvIn
          OvX -> SX
          OvDtX -> SDtX
          OvRegD -> if onX0 then SDx else SDy
          OvRegA -> if onX0 then SAx else SAy
          OvDv -> SDv
        SWhoA -> if onX then SAx else SAy
        SWhoD -> if onX then SDx else SDy
        SMvPre -> if 15 - j == 8 + resize f0 then (if uT w == SDt then SDt else uT w) else SJ
        SIackVec -> if ciAuto ci then SK else SInB
        _ -> s
      -- registers by role
      rIdx rs = case rs of
        RDx -> f9
        RAx -> 8 + f9
        RDy -> f0
        RAy -> 8 + f0
        RA7 -> 15
        RJ -> resize (C2.kJ (rK r))
        RWho -> 8 + (if onX then f9 else f0)
      rNum rs = resize (rIdx rs) :: Unsigned 3
      zs z = szOfS op sz' z
      wrOf sw = Wr { wOn = swOn sw && (not (swIfPost sw) || C2.kLoc k1 == LPost), wIdx = rIdx (swSel sw)
                   , wSrc = src (swSrc sw)
                   , wMode = case swMode sw of
                       WMFull -> WFull
                       WMLow z -> WLow (zs z)
                       WMSext -> WSext
                       WMDivOk -> WDivOk
                       WMSextIfW z -> if zs z == SW then WSext else WFull }
      -- the ALU's operation, its operands
      line = op `shiftR` 12
      (aop, aA, aB, aSr) = case uAlu w of
        AFix a -> (a, uAluA w, uAluB w, uAluSr w)
        ALine -> (C2.OAlu (lineAlu op), uAluA w, uAluB w, uAluSr w)
        AImm -> (C2.OAlu (immAlu op), uAluA w, uAluB w, uAluSr w)
        AQuick -> (C2.OAlu (if testBit op 8 then ASub else AAdd), uAluA w, uAluB w, uAluSr w)
        AXLine -> (case line of
                     0xC -> C2.OBcd True
                     0x8 -> C2.OBcd False
                     l -> C2.OAluX (l == 0xD), uAluA w, uAluB w, uAluSr w)
        ABcdOp -> (C2.OBcd (line == 0xC), uAluA w, uAluB w, uAluSr w)
        ARmw -> case line of
          0xE -> (C2.OShiftMem, uAluA w, uAluB w, True)
          0x0 -> (C2.OAlu (immAlu op), uAluA w, uAluB w, True)
          0x5 | (op `shiftR` 6) .&. 3 == 3 -> (C2.OSccVal, S0, S0, False)
              | otherwise -> (C2.OAlu (if testBit op 8 then ASub else AAdd), uAluA w, uAluB w, True)
          0x4 | op .&. 0xFFC0 == 0x40C0 -> (C2.OPass, SSr, S0, False)
              | op .&. 0xFFC0 == 0x4800 -> (C2.OBcd False, uAluB w, S0, True)
              | otherwise -> (C2.OUnary, uAluA w, uAluB w, True)
          _ -> (C2.OAlu (lineAlu op), uAluA w, uAluB w, True)
      kk' = C2.kK k1
      n4 = if testBit op 6 then 4 else 2 :: Unsigned 4
      lvl = C2.kLvl k1
      kval = case uK w of
        KLit -> resize (uLit w)
        KMoveq -> sext8 (w32 op)
        KQuick -> quickData op
        KTrap -> 32 + resize (op .&. 15)
        KShift -> let cnt = field op 9 in if cnt == 0 then 8 else resize cnt
        KOp -> resize op
        KByte -> resize (n4 - 1 - kk')
        KIackA -> 0xFFFFF0 .|. resize (resize lvl `shiftL` 1 :: Unsigned 4)
        KLvlVec -> 24 + resize lvl
        KBerr ->
          let rq = C2.cReq c2
              rd = rqKind rq == KRead
              prog = rqFc rq == 2 || rqFc rq == 6
          in resize ((op .&. 0xFFE0) .|. (if rd then 0x10 else 0) .|. (if prog then 0x08 else 0) .|. resize (rqFc rq))
        KVec -> resize (uLit w) * 4
        KLvl -> resize lvl
        KVecG0 -> if C2.kBerr k1 then 8 else 12
      -- an acknowledge with VPA: its autovector through the constant
      kconst = if uT w == SIackVec && ciAuto ci then 24 + resize lvl else kval
      auk = case uAuK w of
        AuLit -> uAuLit w
        Au2K -> 2 * resize kk'
        AuD8 -> sext8 (w32 (op .&. 0xFF))
        AuIfL -> if sz' == SL then 2 else 0
      au2k = case uAu2K w of
        Au2Lit -> uAu2Lit w
        Au2Movem down -> let st = if sz' == SL then 4 else 2 in if down then 0 - st else st
      rqz = zs (uRqZ w)
      isByte = rqz == SB
      pcSet' = case uPcSet w of
        PcKeep -> PcKeep
        PcSet x -> PcSet (src x)
        PcSetM4 x -> PcSetM4 (src x)
      -- SR: the ALU's flags only when the operation sets them (a
      -- read-modify-write's Scc and MOVE from SR don't)
      srOp' = case uSr w of
        SrSet x -> SrSet (src x)
        SrCcr x -> SrCcr (src x)
        C2.SrAlu -> if aSr then C2.SrAlu else C2.SrKeep
        x -> x
      m = C2.noCmd
        { mK = kconst
        , mAlu = aop, mAluA = src aA, mAluB = src aB, mAluSz = zs (uAluZ w), mAluUsed = uAluUsed w, mAluSextA = uSextA w
        , mAuB = src (uAuB w), mAuO = uAuO w, mAuK = auk, mStepR = rNum (uStepR w), mAuUsed = uAuUsed w
        , mAu2B = src (uAu2B w), mAu2K = au2k, mAu2Used = uAu2Used w
        , mMdA = src (uMdA w)
        , mW1 = wrOf (uW1 w), mW2 = wrOf (uW2 w)
        , mOsp = fmap src (uOsp w)
        , mSr = srOp'
        , mLvl = lvl
        , mPcAdv = uPcAdv w, mPcSet = pcSet', mIr = uIr w, mIrcIn = uIrcIn w, mX = uX w, mA7Du = uA7Du w
        , mEa = src (uEa w), mV = src (uV w), mS = src (uS w), mT = src (uT w), mU = src (uU w)
        , mRq = RqC { qKind = uRqKind w, qFc = uRqFc w, qByte = isByte, qAddr = src (uRqAddr w), qData = src (uRqData w)
                    , qHi = uRqHi w || (uRqHiL w && sz' == SL)
                    , qLen = uRqLen w, qLenK = if uRqKind w == KIack then resize lvl else uRqLenK w, qPh = PStop }
        , mFault = (uRqKind w == KRead || uRqKind w == KWrite) && not isByte
        , mBad = uBad w }
      c2' = C2.datapath c2 ci ps (k1 { C2.kPh = PStop }) m
      kr = C2.cK c2'
      -- the next word
      qN = nextOf (uNext w) k1 ret' opv c2' op
  in Cpu c2' (if C2.kPh kr == PG0 then Seq (UG0 0) ret' opv False
              else if C2.kPh kr == PBound then Seq (UEnd EIdle) ret' opv True
              else qN)

-- | The next word from a word's 'Next' and the state the step left.
nextOf :: Next -> C2.Ctl -> Cont -> Opv -> C2.Cpu -> Unsigned 16 -> Seq
nextOf n k ret opv c op =
  let sz = C2.kSz k
      long = sz == SL
      sq l = Seq l ret opv False
      f9 = resize (field op 9) :: Unsigned 4
      dx = C2.cRf c !! f9
      v = C2.cV c
      lo16s x = unpack (pack (resize x :: Unsigned 16)) :: Signed 16
      endMv mask start = nextSet mask start >= 16
      n4 = if testBit op 6 then 4 else 2 :: Unsigned 4
  in case n of
       NGo l -> sq l
       NEnd e -> sq (UEnd e)
       NRet -> sq (UCont ret)
       NRead -> if long then sq URdL2 else Seq (UCont ret) ret (if sz == SB then OvInB else OvIn) False
       NImmW -> if long then sq UEaImmH else sq (UImmW ret)
       NImmL -> sq (UImmL ret)
       NAddr f m -> case ret of
         CMovem d _ -> sq (UAddrDone f m (CMovem d (endMv (resize (C2.cS c)) 0)))
         _ -> sq (UAddrDone f m ret)
       NWr lowFirst wn ->
         if long then sq (if lowFirst then UWr2 wn else UWr0 wn)
         else sq (if wn == XDone then UEnd EWrite else UWrLast wn)
       NMovem d -> sq (UMvNext d long (endMv (resize (C2.cS c)) (C2.kJ k + 1)))
       NMovemMid d -> if long then sq (UMvMid d) else sq (UMvNext d long (endMv (resize (C2.cS c)) (C2.kJ k + 1)))
       NMovemExt d m -> sq (UMovemExt d m (endMv (C2.cX c) 0))
       NMovep toMem -> sq (UMovepB toMem (C2.kK k + 1 == n4))
       NChk -> sq (UChk (if lo16s dx > lo16s v then ChkHi else if lo16s dx < 0 then ChkLo else ChkOk))
       NDiv -> sq (UIdle (if v .&. 0xFFFF == 0 then IDivZ else IPf))

-- * Clocks

data M68kT = M68kT
  { tCpu :: Cpu
  , tClk :: Unsigned 8
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFDataX

m68kPowerOn :: M68kT
m68kPowerOn = M68kT (reset cpuPowerOn) 0

reqClocks :: Req -> Unsigned 8
reqClocks rq = case rqKind rq of
  KRead -> 4
  KWrite -> 4
  KTas -> 10
  KIack -> 4
  KIdle -> max 1 (rqLen rq)

m68kDone :: Bool -> M68kT -> Bool
m68kDone dtack t =
  let rq = C2.cReq (cC (tCpu t))
      last' = tClk t + 1 >= reqClocks rq
  in last' && (rqKind rq == KIdle || dtack)

m68kTick :: Bool -> CpuIn -> M68kT -> M68kT
m68kTick dtack ci t
  | m68kDone dtack t = M68kT (cpuStep ci (tCpu t)) 0
  | tClk t + 1 >= reqClocks (C2.cReq (cC (tCpu t))) = t
  | otherwise = t { tClk = tClk t + 1 }
