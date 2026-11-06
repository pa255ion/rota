{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE StrictData #-}
{-|
The 68000's microcode (side track P, P.4h; docs/cpus/m68k.md, "P.4h"): one
word a transaction, for "M68k.Core"'s microsequencer on "M68k.Datapath". A word is the datapath's command made symbolic where the opcode
matters (registers by role, sizes by kind, the ALU's operation by line, a
constant by kind), what the sequencer's state becomes, and the next word
('Next'): a label, the instruction's end, a return, an operand's dispatch
by addressing mode, a condition.

The words are written as P.4g's builder steps were (the same functions,
with the opcode's decisions as the labels' parameters), so a step that
chains several of them is one word composed at compile time ('micro').
-}
module M68k.Micro where

import Clash.Prelude hiding (Word)

import M68k.Bus (Kind (..), Loc (..))
import M68k.Datapath (AOp (..), AuOff (..), FcSel (..), IrOp (..), LenSel (..), PcOp (..), Src (..), SrOp (..), XOp (..))
import M68k.Ops hiding (alu)

-- * Symbolic fields

-- | A register by its role (resolved with the opcode and the sequencer).
data RS
  = RDx | RAx      -- ^ D, A of bits 9-11
  | RDy | RAy      -- ^ D, A of bits 0-2
  | RA7            -- ^ A7
  | RJ             -- ^ MOVEM's register (its list position)
  | RWho           -- ^ the operand's address register (bits 0-2 or 9-11 by 'kWho')
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFDataX, BitPack)

-- | A size by kind.
data ZS
  = ZOp            -- ^ bits 6-7
  | ZMove          -- ^ MOVE's (bits 12-13)
  | ZK             -- ^ the sequencer's ('kSz', an operand routine's)
  | ZB | ZW | ZL
  | ZAddr          -- ^ ADDA, SUBA, CMPA's (bit 8: long)
  | ZMovem         -- ^ MOVEM's (bit 6: long)
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFDataX, BitPack)

-- | The step's constant by kind.
data KS
  = KLit           -- ^ the word's literal
  | KMoveq         -- ^ MOVEQ's data
  | KQuick         -- ^ ADDQ/SUBQ's data (1-8)
  | KTrap          -- ^ a TRAP's vector (32 + n)
  | KShift         -- ^ a shift's count (1-8)
  | KOp            -- ^ the opcode
  | KByte          -- ^ MOVEP's byte (by the step counter)
  | KIackA         -- ^ the acknowledge's address (by the level)
  | KLvlVec        -- ^ an autovector (24 + the level)
  | KBerr          -- ^ a bus error's status word (by the pending cycle)
  | KVec           -- ^ the word's literal times four (a vector's address)
  | KLvl           -- ^ the level (the acknowledge's length field)
  | KVecG0         -- ^ group 0's vector's address (a bus error's 2, an address error's 3)
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFDataX, BitPack)

-- | The ALU's operation by kind.
data AS
  = AFix AOp
  | ALine          -- ^ the line's (ADD, SUB, AND, OR; line B: CMP or EOR)
  | AImm           -- ^ the immediate's (ORI to CMPI by bits 9-11)
  | AQuick         -- ^ ADDQ or SUBQ
  | ARmw           -- ^ a read-modify-write's (by the line: the immediate's, the quick form's, the unary ones', the shift's, Scc's, MOVE from SR's, NBCD's, the line's)
  | AXLine         -- ^ ADDX, SUBX, ABCD, SBCD by the line
  | ABcdOp         -- ^ ABCD or SBCD by the opcode (registers)
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFDataX, BitPack)

-- | A register write's width.
data WM = WMFull | WMLow ZS | WMSext | WMDivOk | WMSextIfW ZS
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFDataX, BitPack)

data SWr = SWr
  { swOn :: Bool, swSel :: RS, swSrc :: Src, swMode :: WM
  , swIfPost :: Bool    -- ^ only if the operand is (An)+ as a destination ('kLoc')
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (NFDataX, BitPack)

noSWr :: SWr
noSWr = SWr False RDx S0 WMFull False

-- | The address unit's constant.
data AuK = AuLit | Au2K | AuD8 | AuIfL  -- ^ AuIfL: 2 for a long ('kSz'), else 0
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFDataX, BitPack)

-- | The second adder's constant.
data Au2K = Au2Lit | Au2Movem Bool
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFDataX, BitPack)

-- | Where an operand's value is (the sequencer's 'kOpv'): the data just
-- read (a word, a byte, a long's second word), the extension word (an
-- immediate's word, a long's two), a register (the operand's D or A),
-- the value temporary.
data Opv = OvIn | OvInB | OvDvIn | OvX | OvDtX | OvRegD | OvRegA | OvDv
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFDataX, BitPack)

-- * The sequencer's state changes

data KOp = KKeep | KSet (Unsigned 4) | KInc
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFDataX, BitPack)

data JOp = JKeep | JZero | JNext | JInc
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFDataX, BitPack)

data CtlOp = CtlOp
  { oWho :: Maybe Who, oSz :: Maybe ZS, oLoc :: Maybe Loc, oOpv :: Maybe Opv
  , oK :: KOp, oJ :: JOp
  , oExc :: Maybe Bool, oBerr :: Maybe Bool, oStopped :: Maybe Bool, oTraceOff :: Bool
  , oRet :: Maybe Cont, oRet2 :: Maybe Cont
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (NFDataX, BitPack)

noCtl :: CtlOp
noCtl = CtlOp Nothing Nothing Nothing Nothing KKeep JKeep Nothing Nothing Nothing False Nothing Nothing

-- * Labels

-- | An operand routine's kind: a read (a source; a read-modify-write's
-- operand), an address only as a destination (MOVE's, LEA, PEA, MOVEM),
-- an address only as a source (TAS).
data Flav = FRd | FAd | FAs
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFDataX, BitPack)

-- | An addressing mode.
data Mode = MDn | MAn | MInd | MPost | MPre | MDisp | MIdx | MAbsW | MAbsL | MPcDisp | MPcIdx | MImm
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFDataX, BitPack)

modeOf :: Unsigned 3 -> Unsigned 3 -> Mode
modeOf m r = case m of
  0 -> MDn
  1 -> MAn
  2 -> MInd
  3 -> MPost
  4 -> MPre
  5 -> MDisp
  6 -> MIdx
  _ -> case r of
    0 -> MAbsW
    1 -> MAbsL
    2 -> MPcDisp
    3 -> MPcIdx
    _ -> MImm

-- | No transaction before the operand is there (a read flavour: Dn, An;
-- an address flavour: (An), (An)+, and a destination's -(An)).
zeroCycle :: Flav -> Mode -> Bool
zeroCycle f m = case f of
  FRd -> m == MDn || m == MAn
  FAd -> m == MInd || m == MPost || m == MPre
  FAs -> m == MInd || m == MPost

-- | How an instruction ends: the boundary step's own actions.
data End
  = EPf      -- ^ the prefetch: IR from IRC, PC on, IRC from the data
  | EJump    -- ^ a jump's (a vector's, the reset's): PC from the scratch, IR from the extension word, IRC from the data
  | ERefill  -- ^ the queue refilled: PC on, IR from the extension word, IRC from the data
  | EIdle    -- ^ idle clocks (a stopped CPU's turn): nothing
  | EWrite   -- ^ a write: nothing
  | EPush    -- ^ a push: A7 from the scratch
  | EStop    -- ^ STOP's prefetch: as 'EPf', then stopped
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFDataX, BitPack)

data AddrK = KCmpa | KAdda | KSuba
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFDataX, BitPack)

-- | MOVEM's direction: a -(An) store, another store, a load.
data MvDir = MvPre | MvStore | MvLoad
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFDataX, BitPack)

-- | MOVE's destination, as its source's value is there.
data MDst = DDn | DAn | DPreL | DPreW | DAbsLMem | DInd | DPost | DDisp | DIdx | DAbsW | DAbsL
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFDataX, BitPack)

-- | A continuation: where an operand routine returns (the sequencer's
-- 'kRet'), with the opcode's decisions it needs.
data Cont
  = CMoveSrc MDst
  | CToDn Bool (Unsigned 3)       -- ^ <ea>,Dn: Dn written (not CMP); a long's idle clocks (0: not a long)
  | CRmw
  | CAddr AddrK Bool (Unsigned 3) -- ^ CMPA, ADDA, SUBA: a word source; the idle clocks after the prefetch
  | CImmS Mode Bool Bool          -- ^ #<data>,<ea>: the destination's mode, CMPI, a long
  | CImmCmp
  | CCmpmY | CCmpmX
  | CTst
  | CBit Bool                     -- ^ a memory bit operation: BTST
  | CChk
  | CCcrSr Bool                   -- ^ MOVE to CCR (else to SR)
  | CDiv | CMul
  -- the address-only operands'
  | CMoveDst
  | CLea
  | CPea
  | CMovem MvDir Bool             -- ^ MOVEM's address found: the direction; no register (the end; the sequencer's)
  | CTas
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFDataX, BitPack)

-- | What follows a write.
data WNext = XMoveW | XDone | XMoveAbs2
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFDataX, BitPack)

-- | What follows a prefetch that doesn't end the instruction.
data PNext
  = PIdle (Unsigned 3) | PIdleDu | PMovePreL | PMovePre | PRmwW | PXW | PXH | PPeaPush
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFDataX, BitPack)

-- | What follows idle clocks that don't end the instruction.
data INext = IRefill | IPea2 | ILea2 | IBsrPush | IJump | ISkipPf | IPf | IDbExp | IDivZ
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFDataX, BitPack)

-- | What follows a push.
data PushNext = PuJsrIrc | PuLink2 | PuJump | PuPf | PuDone
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFDataX, BitPack)

-- | CHK's three outcomes after its prefetch.
data ChkK = ChkHi | ChkLo | ChkOk
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFDataX, BitPack)

-- | RTS, RTR, RTE.
data RetK = RkRts | RkRtr | RkRte
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFDataX, BitPack)

-- | A word's label: the transaction that just ended, and what follows.
data U
  = UEnd End                   -- ^ (the sequencer's: the instruction is over)
  -- operand routines
  | UEaS Flav Mode             -- ^ an operand's first transaction (at an instruction's start)
  | UEaPre | UEaDisp | UEaIdxI Flav | UEaIdx | UEaAbsW | UEaAbsH Flav | UEaAbsL
  | UEaPcDisp | UEaPcIdxI Flav | UEaPcIdx | UEaImmH
  | URdL2                      -- ^ a long's second word read
  | UCont Cont                 -- ^ a continuation, its operand where 'kOpv' says
  | UImmW Cont | UImmL Cont    -- ^ an immediate's last word taken, then the continuation
  | UAddrDone Flav Mode Cont   -- ^ an address-only operand found, then the continuation
  -- shared sequences
  | UWr0 WNext | UWr2 WNext | UWrLast WNext
  | UPf PNext
  | UIdle INext
  | USkipPf
  | UPush0 PushNext | UPush1 PushNext
  | UJump0 Bool                -- ^ the jump's IR word read (JSR: push next)
  | URefill0
  -- instructions' later steps
  | UMoveAbsH | UMovePreL2
  | UX Bool (Unsigned 3)       -- ^ ADDX/SUBX/ABCD/SBCD -(Ay),-(Ax): a long; step k
  | UBitExt Mode Bool          -- ^ a static bit operation's number taken (the operand's mode; BTST)
  | USysExt Bool               -- ^ ORI/ANDI/EORI to CCR (else SR): the word taken
  | UTrapv
  | ULink Bool                 -- ^ LINK's displacement taken (A7: True)
  | UUnlk0 | UUnlk1
  | UTas
  | UDbRd
  | UJt Mode Bool              -- ^ JMP/JSR's target's transaction over (JSR: True)
  | URet RetK (Unsigned 2)
  | UChk ChkK
  | UMovepExt Bool             -- ^ MOVEP's displacement taken (to memory: True)
  | UMovepB Bool Bool          -- ^ a byte over (to memory); the last
  | UMovemExt MvDir Mode Bool  -- ^ MOVEM's mask taken: the direction, the mode, no register (the end)
  | UMvMid MvDir               -- ^ a long's first word over
  | UMvNext MvDir Bool Bool    -- ^ a register over: a long; the end
  | UMovemEnd Bool             -- ^ the word past the last read ((An)+: True)
  | UStopExt
  -- exceptions
  | UG0 (Unsigned 3)
  | UExc (Unsigned 2)
  | UVec (Unsigned 3)
  | UInt (Unsigned 3)
  | UReset (Unsigned 3)
  -- the boundary's own words (the sequencer picks them)
  | UTrace | UIntStart | UStopWait | UBerr
  -- instructions' first transactions (the entry decoder's)
  | UNop | UMoveq
  | UEorDn Bool                -- ^ a long
  | UAddxR Bool Bool           -- ^ ADDX (else SUBX); a long
  | UQuickA Bool Bool          -- ^ SUBQ (else ADDQ); a word
  | UQuickD Bool               -- ^ a long
  | UQuickM Mode
  | UUnaryR Bool               -- ^ a long
  | UShiftR Bool               -- ^ by a register's count
  | UBitR                      -- ^ a dynamic bit operation on Dn
  | UBitDynM Mode Bool         -- ^ on memory (BTST)
  | UBitS Mode Bool            -- ^ static: the number's extension word (the mode; BTST)
  | UToMemS Mode               -- ^ Dn,<ea>: Dn kept, the operand's first transaction
  | UXS Bool                   -- ^ ADDX/SUBX/ABCD/SBCD -(Ay),-(Ax): a long
  | UCcrImm | USrImm | UTrapvS Bool | UTrap | ULinkS Bool | UUnlkS
  | UToUsp | UFromUsp | USwap | UExtW | UExtL | UFromSrR | USccR Bool | UTasR | UExg (Unsigned 2)
  | UBcc Bool Bool             -- ^ taken; a word displacement
  | UBsr Bool
  | UDbcc Bool Bool            -- ^ the condition true (no loop); expired
  | UJtS Mode Bool             -- ^ JMP/JSR's first transaction (the mode; JSR)
  | URetS RetK
  | UResetS
  | UMovemS MvDir Mode
  | UNbcdR | UBcdR Bool        -- ^ ABCD (else SBCD)
  | UMovepS Bool
  | UStopS
  | UIllegal (Unsigned 4)      -- ^ the vector (4, 10, 11)
  | UPriv
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFDataX, BitPack)

-- | What the sequencer does after a word.
data Next
  = NGo U
  | NEnd End
  | NRet                       -- ^ to 'kRet' (its operand where 'kOpv' says)
  | NRead                      -- ^ an operand's read issued: a long's second word next, or 'NRet'
  | NImmW                      -- ^ an immediate's word issued: 'UImmW' with 'kRet'
  | NImmL                      -- ^ a long immediate's low word issued: 'UImmL' with 'kRet'
  | NAddr Flav Mode            -- ^ an address-only operand's last transaction issued: 'UAddrDone' with 'kRet'
  | NMovemExt MvDir Mode       -- ^ MOVEM's mask issued: 'UMovemExt', its end found
  | NWr Bool WNext             -- ^ a write issued by 'writeOp' (low word first): by the size
  | NMovem MvDir               -- ^ a MOVEM register's last transaction issued: 'UMvNext', its end found
  | NMovemMid MvDir            -- ^ its first (a long's high) issued: 'UMvMid' or 'NMovem'
  | NMovep Bool                -- ^ a MOVEP byte issued: 'UMovepB', the last found
  | NChk                       -- ^ CHK's prefetch issued: 'UChk' by the comparison
  | NDiv                       -- ^ DIVU/DIVS's clocks issued: a zero divisor's exception, or the prefetch
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFDataX, BitPack)

-- * Words

data Word = Word
  { uK      :: KS, uLit :: Unsigned 16, uKUsed :: Bool
  , uAlu    :: AS, uAluA :: Src, uAluB :: Src, uAluZ :: ZS, uAluUsed :: Bool, uAluSr :: Bool, uSextA :: Bool
  , uAuB    :: Src, uAuO :: AuOff, uAuK :: AuK, uAuLit :: Unsigned 32, uStepR :: RS, uAuUsed :: Bool
  , uAu2B   :: Src, uAu2K :: Au2K, uAu2Lit :: Unsigned 32, uAu2Used :: Bool
  , uMdA    :: Src
  , uW1, uW2 :: SWr
  , uOsp    :: Maybe Src
  , uSr     :: SrOp
  , uPcAdv  :: Bool, uPcSet :: PcOp, uIr :: IrOp, uIrcIn :: Bool, uX :: XOp, uA7Du :: Bool
  , uEa, uV, uS, uT, uU :: Src
  , uRqKind :: Kind, uRqFc :: FcSel, uRqZ :: ZS, uRqAddr :: Src, uRqData :: Src, uRqHi :: Bool
  , uRqHiL  :: Bool             -- ^ the data's high word if the request is a long's ('kSz')
  , uRqLen  :: LenSel, uRqLenK :: Unsigned 8
  , uCtl    :: CtlOp
  , uNext   :: Next
  , uBad    :: Bool
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (NFDataX, BitPack)

w0 :: Word
w0 = Word
  { uK = KLit, uLit = 0, uKUsed = False
  , uAlu = AFix OPass, uAluA = S0, uAluB = S0, uAluZ = ZL, uAluUsed = False, uAluSr = False, uSextA = False
  , uAuB = S0, uAuO = AO0, uAuK = AuLit, uAuLit = 0, uStepR = RWho, uAuUsed = False
  , uAu2B = S0, uAu2K = Au2Lit, uAu2Lit = 0, uAu2Used = False
  , uMdA = S0, uW1 = noSWr, uW2 = noSWr, uOsp = Nothing, uSr = SrKeep
  , uPcAdv = False, uPcSet = PcKeep, uIr = IrKeep, uIrcIn = False, uX = XKeep, uA7Du = False
  , uEa = SEa, uV = SDv, uS = SDs, uT = SDt, uU = SDu
  , uRqKind = KIdle, uRqFc = FData, uRqZ = ZW, uRqAddr = S0, uRqData = S0, uRqHi = False, uRqHiL = False
  , uRqLen = LConst, uRqLenK = 0
  , uCtl = noCtl, uNext = NEnd EIdle, uBad = False }

-- | A fragment of a word (composed right to left).
type F = Word -> Word

bad :: F
bad w = w { uBad = True }

-- | The step's constant, once a word.
konst :: KS -> Unsigned 16 -> F
konst k l w = (if uKUsed w then bad w else w) { uK = k, uLit = l, uKUsed = True }

-- | The ALU, once a word: 'SAlu'; @flags@: SR from it.
alu :: AS -> ZS -> Src -> Src -> Bool -> F
alu o z a b flags w = (if uAluUsed w then bad w else w)
  { uAlu = o, uAluZ = z, uAluA = a, uAluB = b, uAluUsed = True, uAluSr = flags
  , uSr = if flags then SrAlu else uSr w }

logicF :: ZS -> Src -> F
logicF z s = alu (AFix OLogic) z s S0 True

sextA :: F
sextA w = w { uSextA = True }

-- | The address unit, once a word: 'SAu'.
au :: Src -> AuOff -> Unsigned 32 -> F
au b o k w = (if uAuUsed w then bad w else w) { uAuB = b, uAuO = o, uAuK = AuLit, uAuLit = k, uAuUsed = True }

auK :: Src -> AuK -> F
auK b k w = (if uAuUsed w then bad w else w) { uAuB = b, uAuO = AOK, uAuK = k, uAuUsed = True }

-- | The address unit by an operand's step for a register.
auStep :: Src -> Bool -> RS -> F
auStep b down r w = (au b (AOStep down) 0 w) { uStepR = r }

au2 :: Src -> Unsigned 32 -> F
au2 b k w = (if uAu2Used w then bad w else w) { uAu2B = b, uAu2K = Au2Lit, uAu2Lit = k, uAu2Used = True }

au2Movem :: Src -> Bool -> F
au2Movem b down w = (if uAu2Used w then bad w else w) { uAu2B = b, uAu2K = Au2Movem down, uAu2Used = True }

-- | A register write: two a word; a later one to the same role replaces
-- the earlier.
wr :: RS -> Src -> WM -> F
wr = wrIf False

wrIf :: Bool -> RS -> Src -> WM -> F
wrIf post r s m w =
  let x = SWr True r s m post
  in if swOn (uW1 w) && swSel (uW1 w) == r then w { uW1 = x }
     else if swOn (uW2 w) && swSel (uW2 w) == r then w { uW2 = x }
     else if not (swOn (uW1 w)) then w { uW1 = x }
     else if not (swOn (uW2 w)) then w { uW2 = x }
     else bad w

setEa, setV, setS, setT, setU :: Src -> F
setEa s w = w { uEa = s }
setV s w = w { uV = s }
setS s w = w { uS = s }
setT s w = w { uT = s }
setU s w = w { uU = s }

ctl :: (CtlOp -> CtlOp) -> F
ctl f w = w { uCtl = f (uCtl w) }

nx :: Next -> F
nx n w = w { uNext = n }

go :: U -> F
go = nx . NGo

-- | Issue a request (its width by a size: a byte when it is one).
issue :: Kind -> FcSel -> ZS -> Src -> Src -> Bool -> F
issue k fc z a d hi w = w { uRqKind = k, uRqFc = fc, uRqZ = z, uRqAddr = a, uRqData = d, uRqHi = hi }

idleN :: LenSel -> Unsigned 8 -> F
idleN l n w = w { uRqKind = KIdle, uRqLen = l, uRqLenK = n }

idle :: Unsigned 8 -> F
idle = idleN LConst

readProg :: Src -> F
readProg a = issue KRead FProg ZW a S0 False

readData :: ZS -> Src -> F
readData z a = issue KRead FData z a S0 False

writeData :: ZS -> Src -> Src -> Bool -> F
writeData z a v hi = issue KWrite FData z a v hi

-- | The prefetch (the next word's label or the end).
prefetch :: F
prefetch = readProg SPc4

-- | An extension word: IRC taken, the word after it read.
ext :: F
ext w = readProg SPc4 w { uX = XIrc }

-- | The PC and IRC moved on (an extension word's or the prefetch's end).
advance :: F
advance w = w { uPcAdv = True, uIrcIn = True }

-- | The prefetch's end (not the instruction's): advance, IR from IRC.
pfDone :: F
pfDone w = advance w { uIr = IrIrc }

-- * Small fragments

lenSel :: LenSel -> F
lenSel l w = w { uRqLen = l }

srOp :: SrOp -> F
srOp s w = w { uSr = s }

xIn :: F
xIn w = w { uX = XIn }

a7du :: F
a7du w = w { uA7Du = True }

pcSetM4 :: Src -> F
pcSetM4 t w = w { uPcSet = PcSetM4 t }

mdA :: Src -> F
mdA s w = w { uMdA = s }

osp :: Src -> F
osp s w = w { uOsp = Just s }

loc :: Loc -> F
loc l = ctl (\c -> c { oLoc = Just l })

exc :: F
exc = ctl (\c -> c { oExc = Just True })

-- | The prefetch, then the instruction's end.
endPf :: F
endPf = nx (NEnd EPf) . prefetch

-- | The prefetch, then a word.
pfThen :: PNext -> F
pfThen p = go (UPf p) . prefetch

idleEnd :: Unsigned 8 -> F
idleEnd n = nx (NEnd EIdle) . idle n

idleThen :: Unsigned 8 -> INext -> F
idleThen n i = go (UIdle i) . idle n

neg :: Unsigned 32 -> Unsigned 32
neg x = 0 - x

-- * Operand routines

-- | An operand's first transaction (a mode that needs one).
eaFirst :: Flav -> Mode -> F
eaFirst f m = case m of
  MInd -> nx NRead . readData ZK SWhoA . setEa SWhoA
  MPost -> nx NRead . readData ZK SWhoA . setEa SWhoA . wr RWho SAu WMFull . auStep SWhoA False RWho
  MPre -> nx (if f == FRd then NGo UEaPre else NAddr f MPre) . idle 2
  MDisp -> nx (if f == FRd then NGo UEaDisp else NAddr f MDisp) . ext
  MIdx -> go (UEaIdxI f) . idle 2
  MAbsW -> nx (if f == FRd then NGo UEaAbsW else NAddr f MAbsW) . ext
  MAbsL -> go (UEaAbsH f) . ext
  MPcDisp -> nx (if f == FRd then NGo UEaPcDisp else NAddr f MPcDisp) . ext . setT SPc2
  MPcIdx -> go (UEaPcIdxI f) . idle 2 . setT SPc2
  MImm -> nx NImmW . ext
  _ -> bad

-- | An address-only operand found (its mode's last transaction over, or
-- at once).
eaAddr :: Flav -> Mode -> F
eaAddr f m = case m of
  MInd -> loc LMem . setEa SWhoA
  MPost | f == FAd -> loc LPost . setEa SWhoA
        | otherwise -> loc LMem . setEa SWhoA . wr RWho SAu WMFull . auStep SWhoA False RWho
  MPre -> loc LMem . setEa SAu . wr RWho SAu WMFull . auStep SWhoA True RWho
  MDisp -> loc LMem . setEa SAu . au SWhoA AOXs 0 . advance
  MIdx -> loc LMem . setEa SAu . au SWhoA AOIdxX 0 . advance
  MAbsW -> loc LMem . setEa SXs . advance
  MAbsL -> loc LMem . setEa SDtX . advance
  MPcDisp -> loc LMem . setEa SAu . au SDt AOXs 0 . advance
  MPcIdx -> loc LMem . setEa SAu . au SDt AOIdxX 0 . advance
  _ -> bad

-- | Write the value to the operand found: a long's high word first, or
-- with @lowFirst@ its low word first (by the size, at run time).
writeOp :: Bool -> WNext -> F
writeOp lowFirst wn w =
  let a = uEa w
      v = uV w
  in if lowFirst
       then nx (NWr True wn) (issue KWrite FData ZK SAu v False (auK a AuIfL w))
       else nx (NWr False wn) ((issue KWrite FData ZK a v False w) { uRqHiL = True })

wnNext :: WNext -> Next
wnNext wn = if wn == XDone then NEnd EWrite else NGo (UWrLast wn)

-- | The flags and the write of MOVE's destination (its address found).
moveDst :: F
moveDst w = writeOp False XMoveW (setV (uS w) (logicF ZMove (uS w) w))

-- * Continuations

-- | A continuation with its operand's value in the value temporary
-- (@imm@: an immediate's).
contV :: Bool -> Cont -> F
contV imm c w =
  let v = uV w
      s = uS w
  in case c of
    CMoveSrc md ->
      let w' = setS v w
      in case md of
        DAn -> endPf (wr RAx v (WMSextIfW ZMove) w')
        DDn -> endPf (wr RDx v (WMLow ZMove) (logicF ZMove v w'))
        DPreL -> pfThen PMovePreL (logicF ZMove v w')
        DPreW -> pfThen PMovePre (logicF ZMove v w')
        DAbsLMem -> go UMoveAbsH (ext w')
        DInd -> moveDst (loc LMem (setEa SAx w'))
        DPost -> moveDst (ctl (\x -> x { oWho = Just WDst }) (loc LPost (setEa SAx w')))
        _ -> eaFirst FAd (dstMode md) (ctl (\x -> x { oWho = Just WDst, oRet = Just CMoveDst }) w')
    CToDn wrt n ->
      let w' = alu ALine ZOp v SDx True w
          w'' = if wrt then wr RDx SAlu (WMLow ZOp) w' else w'
      in if n == 0 then endPf w'' else pfThen (PIdle n) w''
    CRmw -> pfThen PRmwW (setV SAlu (alu ARmw ZK s v True w))
    CAddr k wd n ->
      let w1 = if wd then sextA w else w
      in case k of
           KCmpa -> pfThen (PIdle n) (alu (AFix (OAlu ACmp)) ZL v SAx True w1)
           _ -> pfThen (PIdle n) (wr RAx SAlu WMFull (alu (AFix (if k == KAdda then OAdd else OSubBA)) ZL v SAx False w1))
    CImmS m cmp long ->
      let w' = setS v w
      in if m == MDn
           then let w2 = alu AImm ZOp v SDy True w'
                    w3 = if cmp then w2 else wr RDy SAlu (WMLow ZOp) w2
                in if not long then endPf w3 else pfThen (PIdle (if cmp then 2 else 4)) w3
           else eaFirst FRd m (ctl (\x -> x { oWho = Just WSrc, oRet = Just (if cmp then CImmCmp else CRmw) }) w')
    CImmCmp -> endPf (alu (AFix (OAlu ACmp)) ZOp s v True w)
    CCmpmY -> eaFirst FRd MPost (ctl (\x -> x { oWho = Just WPostX, oRet = Just CCmpmX }) (setS v w))
    CCmpmX -> endPf (alu (AFix (OAlu ACmp)) ZOp s v True w)
    CTst -> endPf (logicF ZOp v w)
    CBit btst ->
      let w' = setV SAlu (alu (AFix OBit) ZB s v True w)
      in if imm then pfThen (PIdle 2) w' else if btst then endPf w' else pfThen PRmwW w'
    CChk -> nx NChk (prefetch (setS SPc2 w))
    CCcrSr ccr -> idleThen 4 IRefill (srOp (if ccr then SrCcr v else SrSet v) w)
    CDiv -> nx NDiv (idleN LDiv 0 (wr RDx SDiv WMDivOk (alu (AFix ODiv) ZW S0 S0 True (mdA v (setS SPc2 w)))))
    CMul -> pfThen PIdleDu (setU SLen (lenSel LMul (wr RDx SMul WMFull (logicF ZL SMul (mdA v w)))))
    _ -> bad w

-- | MOVE's destination kinds that need an operand routine.
dstMode :: MDst -> Mode
dstMode md = case md of
  DDisp -> MDisp
  DIdx -> MIdx
  DAbsW -> MAbsW
  _ -> MAbsL

-- | A continuation of an address-only operand (its address in the
-- operand's temporary; its mode).
contA :: Mode -> Cont -> F
contA m c w =
  let a = uEa w
      idx = m == MIdx || m == MPcIdx
      ab = m == MAbsW || m == MAbsL
  in case c of
    CMoveDst -> moveDst w
    CLea -> let w' = setS a w in if idx then idleThen 2 ILea2 w' else lea2 w'
    CPea -> let w' = setS a w in if idx then idleThen 2 IPea2 w'
                                 else if ab then push (uS w') PuPf w' else pfThen PPeaPush w'
    CMovem dir end -> movemFirst dir end (ctl (\x -> x { oJ = JNext }) w)
    CTas -> go UTas (issue KTas FData ZB a S0 False w)
    _ -> bad w

lea2 :: F
lea2 w = endPf (wr RAx (uS w) WMFull w)

-- * Shared sequences

-- | Push a long: the high word at A7 - 4, then the low; A7 moved after
-- both (its own path).
push :: Src -> PushNext -> F
push v pn = go (UPush0 pn) . writeData ZW SAu2 v True . setU SAu2 . setV v . au2 SA7 (neg 4)

-- | The queue filled at a target (JSR: the push between IR's and IRC's
-- reads).
jumpTo :: Bool -> Src -> F
jumpTo jsr t = go (UJump0 jsr) . readProg t . setT t . pcSetM4 t

-- | Groups 1 and 2: @pre@ idle clocks, then the frame with @pc@, the
-- vector (the constant @k@'s).
excStart :: Unsigned 8 -> KS -> Unsigned 16 -> Src -> F
excStart pre k vec pc =
  (if pre == 0 then excStep0 else go (UExc 0) . idle pre) . setT SK . konst k vec . setV pc . exc

excStep0 :: F
excStep0 w = go (UExc 1) (writeData ZW SAu (uV w) False (setS SSr (setU SSpNew (au SSpNew AOK (neg 2) (srOp SrExc w)))))

vectorT :: F
vectorT = go (UVec 0) . readData ZW SDt4 . setEa SDt4

-- | MOVEM's first register (the conditions' next one), or the end.
movemFirst :: MvDir -> Bool -> F
movemFirst dir end w
  | end = if dir == MvLoad then go (UMovemEnd False) (readData ZW (uEa w) w) else endPf w
  | otherwise = movemIssue dir w

-- | MOVEM's next register's first transaction (at the operand's address).
movemIssue :: MvDir -> F
movemIssue dir w =
  let a = uEa w
  in case dir of
       MvPre -> nx (NMovemMid MvPre) (writeData ZW SAu SMvPre False (setV SMvPre (au a AOK (neg 2) w)))
       MvStore -> nx (NMovemMid MvStore) ((writeData ZW a SJ False (setV SJ w)) { uRqHiL = True })
       MvLoad -> nx (NMovemMid MvLoad) (readData ZW a w)

-- * The words

micro :: U -> Word
micro u = case u of
  UEnd _ -> w0
  -- operand routines
  UEaS f m -> eaFirst f m w0
  UEaPre -> nx NRead (readData ZK SAu (setEa SAu (wr RWho SAu WMFull (auStep SWhoA True RWho w0))))
  UEaDisp -> nx NRead (readData ZK SAu (setEa SAu (au SWhoA AOXs 0 (advance w0))))
  UEaIdxI f -> nx (if f == FRd then NGo UEaIdx else NAddr f MIdx) (ext w0)
  UEaIdx -> nx NRead (readData ZK SAu (setEa SAu (au SWhoA AOIdxX 0 (advance w0))))
  UEaAbsW -> nx NRead (readData ZK SXs (setEa SXs (advance w0)))
  UEaAbsH f -> nx (if f == FRd then NGo UEaAbsL else NAddr f MAbsL) (ext (setT SXH (advance w0)))
  UEaAbsL -> nx NRead (readData ZK SDtX (setEa SDtX (advance w0)))
  UEaPcDisp -> nx NRead (readData ZK SAu (setEa SAu (au SDt AOXs 0 (advance w0))))
  UEaPcIdxI f -> nx (if f == FRd then NGo UEaPcIdx else NAddr f MPcIdx) (ext w0)
  UEaPcIdx -> nx NRead (readData ZK SAu (setEa SAu (au SDt AOIdxX 0 (advance w0))))
  UEaImmH -> nx NImmL (ext (setT SXH (advance w0)))
  URdL2 -> nx NRet (ctl (\x -> x { oOpv = Just OvDvIn }) (readData ZW SAu (au SEa AOK 2 (setV SInH w0))))
  UCont c -> contV False c (setV SOp w0)
  UImmW c -> contV True c (setV SX (advance w0))
  UImmL c -> contV True c (setV SDtX (advance w0))
  UAddrDone f m c -> contA m c (eaAddr f m w0)
  -- writes
  UWr0 wn -> nx (wnNext wn) (writeData ZW SAu SDv False (au SEa AOK 2 w0))
  UWr2 wn -> nx (wnNext wn) (writeData ZW SEa SDv True w0)
  UWrLast wn ->
    let w' = wrIf True RWho SAu WMFull (auStep SEa False RWho w0)
    in case wn of
         XMoveAbs2 -> go USkipPf (ext w')
         _ -> endPf w'
  -- the prefetch's end (not the instruction's)
  UPf p ->
    let w' = pfDone w0
    in case p of
         PIdle n -> idleEnd (resize n) w'
         PIdleDu -> nx (NEnd EIdle) (idleN LDu 0 w')
         PMovePreL -> go UMovePreL2 (writeData ZW SAu SDs False (loc LMem (wr RAx SAu WMFull (au SAx AOK (neg 2) w'))))
         PMovePre -> writeOp False XDone (setV SDs (setEa SAu (loc LMem (wr RAx SAu WMFull (auStep SAx True RAx w')))))
         PRmwW -> writeOp True XDone w'
         PXW -> writeOp False XDone w'
         PXH -> nx (NEnd EWrite) (writeData ZW SEa SDv True w')
         PPeaPush -> push SDs PuDone w'
  UMovePreL2 -> nx (NEnd EWrite) (writeData ZW SAu SDs True (wr RAx SAu WMFull (au SAx AOK (neg 2) w0)))
  -- idle clocks' end
  UIdle i -> case i of
    IRefill -> go URefill0 (readProg SPc2 w0)
    IPea2 -> pfThen PPeaPush w0
    ILea2 -> lea2 w0
    IBsrPush -> push SDs PuJump w0
    IJump -> jumpTo False SDt w0
    ISkipPf -> go USkipPf (ext w0)
    IPf -> endPf w0
    IDbExp -> go UDbRd (readProg SDt w0)
    IDivZ -> excStart 4 KLit 5 SDs w0
  USkipPf -> endPf (advance w0)
  UDbRd -> go USkipPf (ext w0)
  -- pushes, jumps, the queue
  UPush0 pn -> nx (if pn == PuDone then NEnd EPush else NGo (UPush1 pn)) (writeData ZW SAu SDv False (au SDu AOK 2 w0))
  UPush1 pn ->
    let w' = a7du w0
    in case pn of
         PuJsrIrc -> nx (NEnd EJump) (readProg SAu (au SDt AOK 2 w'))
         PuLink2 -> endPf (wr RA7 SAu WMFull (wr RAy SDu WMFull (au SDu AOXs 0 w')))
         PuJump -> jumpTo False SDt w'
         PuPf -> endPf w'
         PuDone -> w'
  UJump0 jsr
    | jsr -> push SDv PuJsrIrc (xIn w0)
    | otherwise -> nx (NEnd EJump) (readProg SAu (au SDt AOK 2 (xIn w0)))
  URefill0 -> nx (NEnd ERefill) (readProg SPc4 (xIn w0))
  -- MOVE's later steps
  UMoveAbsH -> writeOp False XMoveAbs2 (setV SDs (loc LMem (setEa SXIrc (logicF ZMove SDs (advance w0)))))
  -- ADDX, SUBX, ABCD, SBCD -(Ay),-(Ax)
  UX long k -> addx long k w0
  -- bit operations
  UBitExt m btst
    | m == MDn -> bitDn SXb (advance w0)
    | otherwise -> eaFirst FRd m (ctl (\x -> x { oWho = Just WSrc, oSz = Just ZB, oRet = Just (CBit btst) }) (setS SXb (advance w0)))
  -- the system group
  USysExt ccr -> idleThen 8 IRefill (srOp (if ccr then SrCcr SAlu else SrSet SAlu) (alu (AFix OSrOp) ZW SX S0 False (advance w0)))
  UTrapv -> excStart 0 KLit 7 SDs (advance w0)
  ULink a7 -> push (if a7 then SAu2 else SAy) PuLink2 (advance w0)
  UUnlk0 -> go UUnlk1 (readData ZW SAu (au SEa AOK 2 (setT SInH w0)))
  UUnlk1 -> endPf (wr RAy SDtIn WMFull (wr RA7 SAu WMFull (au SEa AOK 4 w0)))
  UTas -> endPf (logicF ZB SInB w0)
  -- branches
  UJt m jsr -> case m of
    MDisp -> jtDone m jsr SAu (au SAy AOIrcS 0 w0)
    MIdx -> jtDone m jsr SAu (au SAy AOIdxIrc 0 w0)
    MAbsW -> jtDone m jsr SIrcS w0
    MAbsL -> jtDone m jsr SXIn (advance w0)
    MPcDisp -> jtDone m jsr SAu (au SPc2 AOIrcS 0 w0)
    _ -> jtDone m jsr SAu (au SPc2 AOIdxIrc 0 w0)
  URet k i -> case (k, i) of
    (RkRts, 0) -> go (URet RkRts 1) (readData ZW SAu (au SEa AOK 2 (setT SInH w0)))
    (RkRts, _) -> jumpTo False SDtIn (wr RA7 SAu WMFull (au SEa AOK 4 w0))
    (_, 0) -> go (URet k 1) (readData ZW SEa (setT SInH w0))
    (_, 1) -> go (URet k 2) (readData ZW SAu (au SEa AOK 4 (xIn w0)))
    _ -> jumpTo False SDtIn (srOp (if k == RkRtr then SrCcr SX else SrSet SX) (wr RA7 SAu WMFull (au SEa AOK 6 w0)))
  UChk k ->
    let w' = pfDone w0
    in case k of
         ChkHi -> excStart 4 KLit 6 SDs (alu (AFix OChkHi) ZW SDx S0 True w')
         ChkLo -> excStart 6 KLit 6 SDs (alu (AFix OChkLo) ZW SDx S0 True w')
         ChkOk -> idleEnd 6 (alu (AFix OChkOk) ZW SDx S0 True w')
  -- MOVEP
  UMovepExt toMem ->
    let w' = setV S0 (ctl (\x -> x { oK = KSet 0 }) (setEa SAu (au SAy AOXs 0 (advance w0))))
    in nx (NMovep toMem) (if toMem then writeData ZB SAu SDxByte False (konst KByte 0 w')
                          else readData ZB SAu w')
  UMovepB toMem lastB ->
    let w1 = ctl (\x -> x { oK = KInc }) w0
        w2 = if toMem then w1 else setV SDvShIn w1
    in if lastB
         then endPf (if toMem then w2 else wr RDx (uV w2) (WMLow ZMovem) w2)
         else nx (NMovep toMem) (if toMem then writeData ZB SAu SDxByte False (konst KByte 0 (auK SEa Au2K w2))
                                 else readData ZB SAu (auK SEa Au2K w2))
  -- MOVEM
  UMovemExt dir m end ->
    let w' = setS SX (advance w0)
    in case dir of
         MvPre -> movemFirst MvPre end (ctl (\x -> x { oJ = JNext }) (loc LMem (setEa SAy (setT SAy w'))))
         MvLoad | m == MPost -> movemFirst MvLoad end (ctl (\x -> x { oJ = JNext }) (loc LPost (setEa SAy (wr RAy SAu WMFull (au SAy AOK 2 w')))))
         _ | m == MInd -> movemFirst dir end (ctl (\x -> x { oJ = JNext }) (loc LMem (setEa SAy w')))
           | otherwise -> eaFirst FAd m (ctl (\x -> x { oWho = Just WSrc, oRet = Just (CMovem dir False) }) w')
  UMvMid dir -> case dir of
    MvPre -> nx (NMovem MvPre) (writeData ZW SAu SDv True (au SEa AOK (neg 4) w0))
    MvStore -> nx (NMovem MvStore) (writeData ZW SAu SDv False (au SEa AOK 2 w0))
    MvLoad -> nx (NMovem MvLoad) (readData ZW SAu (au SEa AOK 2 (setT SInH w0)))
  UMvNext dir long end ->
    let w1 = case dir of
          MvPre -> setEa SAu2 (wr RAy SAu2 WMFull (au2Movem SEa True w0))
          MvLoad -> setEa SAu2 (au2Movem SEa False (wr RJ (if long then SDtIn else SIn) (if long then WMFull else WMSext) w0))
          MvStore -> setEa SAu2 (au2Movem SEa False w0)
        w2 = ctl (\x -> x { oJ = JNext }) w1
    in if end
         then (if dir == MvLoad then go (UMovemEnd False) (readData ZW (uEa w2) w2) else endPf w2)
         else movemIssue dir w2
  UMovemEnd _ -> endPf (wrIf True RAy SEa WMFull w0)
  UStopExt -> nx (NEnd EStop) (prefetch (srOp (SrSet SX) (advance w0)))
  -- exceptions
  UG0 k -> case k of
    0 -> g0 1 SAu SDv False (setU SSpNew (au SSpNew AOK (neg 2) (srOp SrExcDs w0)))
    1 -> g0 2 SAu SDs False (au SDu AOK (neg 6) w0)
    2 -> g0 3 SAu SDv True (au SDu AOK (neg 4) w0)
    3 -> g0 4 SAu SK False (konst KOp 0 (au SDu AOK (neg 8) w0))
    4 -> g0 5 SAu SEa False (au SDu AOK (neg 10) w0)
    5 -> g0 6 SAu SDt False (au SDu AOK (neg 14) w0)
    6 -> g0 7 SAu SEa True (au SDu AOK (neg 12) w0)
    _ -> go (UVec 0) (readData ZW SK (setEa SK (konst KVecG0 0 (wr RA7 SAu WMFull (au SDu AOK (neg 14) w0)))))
  UExc k -> case k of
    0 -> excStep0 w0
    1 -> go (UExc 2) (writeData ZW SAu SDs False (au SDu AOK (neg 6) w0))
    2 -> go (UExc 3) (writeData ZW SAu SDv True (au SDu AOK (neg 4) w0))
    _ -> vectorT (wr RA7 SAu WMFull (au SDu AOK (neg 6) w0))
  UVec k -> case k of
    0 -> go (UVec 1) (readData ZW SAu (au SEa AOK 2 (setT SInH w0)))
    1 -> go (UVec 2) (readProg SDtIn (setT SDtIn w0))
    2 -> go (UVec 3) (idle 2 (xIn w0))
    _ -> nx (NEnd EJump) (readProg SAu (au SDt AOK 2 w0))
  UInt k -> case k of
    0 -> go (UInt 1) (writeData ZW SAu SPc False (setV SPc (setS SSr (setU SSpNew (au SSpNew AOK (neg 2)
           (srOp SrInt (ctl (\x -> x { oStopped = Just False }) w0)))))))
    1 -> go (UInt 2) (issue KIack FIack ZW SK S0 False (konst KIackA 0 w0))
    2 -> go (UInt 3) (idle 4 (setT SIackVec w0))
    3 -> go (UInt 4) (writeData ZW SAu SDs False (au SDu AOK (neg 6) w0))
    4 -> go (UInt 5) (writeData ZW SAu SDv True (au SDu AOK (neg 4) w0))
    _ -> vectorT (wr RA7 SAu WMFull (au SDu AOK (neg 6) w0))
  UReset k -> case k of
    0 -> go (UReset 1) (rdS SAu (au S0 AOK 2 (setT SInH w0)))
    1 -> go (UReset 2) (rdS SAu (au S0 AOK 4 (wr RA7 SDtIn WMFull w0)))
    2 -> go (UReset 3) (rdS SAu (au S0 AOK 6 (setT SInH w0)))
    3 -> go (UReset 4) (rdS SDtIn (setT SDtIn w0))
    _ -> nx (NEnd EJump) (rdS SAu (au SDt AOK 2 (xIn w0)))
  -- the boundary's words
  UTrace -> excStart 4 KLit 9 SPc (ctl (\x -> x { oTraceOff = True }) w0)
  UIntStart -> go (UInt 0) (idle 6 (exc (ctl (\x -> x { oTraceOff = True }) w0)))
  UStopWait -> idleEnd 0 w0
  UBerr -> go (UG0 0) (idle 4 (setEa SBa (setT SK (konst KBerr 0 (setS SSr (setV SPc
             (ctl (\x -> x { oExc = Just True, oBerr = Just True, oK = KSet 0 }) w0)))))))
  UPriv -> excStart 4 KLit 8 SPc w0
  UIllegal v -> excStart 4 KLit (resize v) SPc w0
  -- instructions' first transactions
  UNop -> endPf w0
  UMoveq -> endPf (wr RDx SAlu WMFull (logicF ZL SK (konst KMoveq 0 w0)))
  UEorDn long -> (if long then pfThen (PIdle 4) else endPf) (wr RDy SAlu (WMLow ZOp) (alu (AFix (OAlu AEor)) ZOp SDx SDy True w0))
  UAddxR isAdd long -> (if long then pfThen (PIdle 4) else endPf) (wr RDx SAlu (WMLow ZOp) (alu (AFix (OAluX isAdd)) ZOp SDy SDx True w0))
  UXS long -> go (UX long 0) (idle 2 w0)
  UQuickA isSub word -> pfThen (PIdle (if word then 4 else 2)) (wr RAy SAlu WMFull (alu (AFix (if isSub then OSubBA else OAdd)) ZL SK SAy False (konst KQuick 0 w0)))
  UQuickD long -> (if long then pfThen (PIdle 4) else endPf) (wr RDy SAlu (WMLow ZOp) (alu AQuick ZOp SK SDy True (konst KQuick 0 w0)))
  UQuickM m -> eaFirst FRd m (ctl (\x -> x { oWho = Just WSrc, oSz = Just ZOp, oRet = Just CRmw }) (setS SK (konst KQuick 0 w0)))
  UToMemS m -> eaFirst FRd m (ctl (\x -> x { oWho = Just WSrc, oSz = Just ZOp, oRet = Just CRmw }) (setS SDx w0))
  UUnaryR long -> (if long then pfThen (PIdle 2) else endPf) (wr RDy SAlu (WMLow ZOp) (alu (AFix OUnary) ZOp S0 SDy True w0))
  UShiftR dyn ->
    let w' = if dyn then w0 else konst KShift 0 w0
    in pfThen PIdleDu (setU SLen (lenSel LShift (wr RDy SAlu (WMLow ZOp) (alu (AFix OShift) ZOp (if dyn then SDx else SK) SDy True w'))))
  UBitR -> bitDn SDx w0
  UBitDynM m btst -> eaFirst FRd m (ctl (\x -> x { oWho = Just WSrc, oSz = Just ZB, oRet = Just (CBit btst) }) (setS SDx w0))
  UBitS m btst -> go (UBitExt m btst) (ext w0)
  UCcrImm -> go (USysExt True) (ext w0)
  USrImm -> go (USysExt False) (ext w0)
  UTrapvS v -> if v then go UTrapv (ext (setS SPc2 w0)) else endPf w0
  UTrap -> excStart 4 KTrap 0 SPc2 w0
  ULinkS a7 -> go (ULink a7) (ext w0)
  UUnlkS -> go UUnlk0 (readData ZW SAy (setEa SAy w0))
  UToUsp -> endPf (osp SAy w0)
  UFromUsp -> endPf (wr RAy SOsp WMFull w0)
  USwap -> endPf (wr RDy SAlu WMFull (alu (AFix OSwap) ZL S0 SDy True w0))
  UExtW -> endPf (wr RDy SAlu (WMLow ZW) (alu (AFix OExtW) ZW S0 SDy True w0))
  UExtL -> endPf (wr RDy SAlu WMFull (alu (AFix OExtL) ZL S0 SDy True w0))
  UFromSrR -> pfThen (PIdle 2) (wr RDy SSr (WMLow ZW) w0)
  USccR t -> (if t then pfThen (PIdle 2) else endPf) (wr RDy SAlu (WMLow ZB) (alu (AFix OSccVal) ZB S0 S0 False w0))
  UTasR -> endPf (wr RDy SAlu WMFull (alu (AFix OTasD) ZB S0 SDy True w0))
  UExg k -> pfThen (PIdle 2) $ case k of
    0 -> wr RDy SDx WMFull (wr RDx SDy WMFull w0)
    1 -> wr RAy SAx WMFull (wr RAx SAy WMFull w0)
    _ -> wr RAy SDx WMFull (wr RDx SAy WMFull w0)
  UBcc taken word
    | taken -> idleThen 2 IJump (setT SAu (bt word w0))
    | otherwise -> idleThen 4 (if word then ISkipPf else IPf) w0
  UBsr word -> idleThen 2 IBsrPush (setT SAu (bt word (setS (if word then SPc4 else SPc2) w0)))
  UDbcc true expired
    | true -> idleThen 4 ISkipPf w0
    | otherwise -> idleThen 2 (if expired then IDbExp else IJump)
                     (setT SAu (au SPc2 AOIrcS 0 (wr RDy SAlu (WMLow ZW) (alu (AFix ODec16) ZW S0 SDy False w0))))
  UJtS m jsr ->
    let w' = setS SPc w0
    in case m of
         MInd -> jtDone m jsr SAy w'
         MIdx -> go (UJt m jsr) (idle 6 w')
         MAbsL -> go (UJt m jsr) (ext w')
         MPcIdx -> go (UJt m jsr) (idle 6 w')
         _ -> go (UJt m jsr) (idle 2 w')
  URetS k -> case k of
    RkRts -> go (URet RkRts 0) (readData ZW SA7 (setEa SA7 w0))
    _ -> go (URet k 0) (readData ZW SAu (au SA7 AOK 2 (setEa SA7 w0)))
  UResetS -> idleThen 128 IPf w0
  UMovemS dir m -> nx (NMovemExt dir m) (ext w0)
  UNbcdR -> pfThen (PIdle 2) (wr RDy SAlu (WMLow ZB) (alu (AFix (OBcd False)) ZB SDy S0 True w0))
  UBcdR isAdd -> pfThen (PIdle 2) (wr RDx SAlu (WMLow ZB) (alu (AFix (OBcd isAdd)) ZB SDy SDx True w0))
  UMovepS toMem -> go (UMovepExt toMem) (ext w0)
  UStopS -> go UStopExt (ext w0)
 where
  g0 k a v hi w = go (UG0 k) (writeData ZW a v hi w)
  rdS a = issue KRead FSProg ZW a S0 False
  -- a branch's target: PC + 2 and the displacement (a word's in IRC)
  bt word = if word then au SPc2 AOIrcS 0 else auK SPc2 AuD8
  -- a bit operation on Dn (bit @n@), its idle clocks after the prefetch
  bitDn n = pfThen PIdleDu . setU SLen . lenSel LBit . wr RDy SAlu WMFull . alu (AFix OBit) ZL n SDy True . setS n
  -- JMP, JSR's target found: JSR's return address past its extension words
  jtDone m jsr t w =
    let n = if m == MInd then 0 else if m == MAbsL then 2 else 1
    in jumpTo jsr t (setV SAu2 (au2 (uS w) (2 + 2 * n) w))

-- | ADDX, SUBX, ABCD, SBCD -(Ay),-(Ax) after their two idle clocks (step
-- 0): Ay's operand read (a long: An down by two, the low word, down by two
-- more, the high word), then Ax's; the result to Ax's: a long's low word,
-- the prefetch, its high word; a byte or word after the prefetch.
addx :: Bool -> Unsigned 3 -> F
addx long k w = case k of
  0 -> predec SAy RAy 1 w
  1 -> predec2 SAy RAy 2 (setT SIn w)
  2 -> predec SAx RAx 3 (setS val w)
  3 -> predec2 SAx RAx 4 (setT SIn w)
  4 -> let w' = setEa SAx (setV SAlu (loc LMem (alu AXLine ZK SDs val True w)))
       in if long then go (UX long 5) (writeData ZW SAu SAlu False (au SAx AOK 2 w')) else pfThen PXW w'
  _ -> pfThen PXH w
 where
  val = if long then SInDt else SIn
  predec ap rn k' w'
    | long = go (UX long k') (readData ZW SAu (wr rn SAu WMFull (au ap AOK (neg 2) w')))
    | otherwise = go (UX long (k' + 1)) (readData ZK SAu (wr rn SAu WMFull (auStep ap True rn w')))
  predec2 ap rn k' w' = go (UX long k') (readData ZW SAu (wr rn SAu WMFull (au ap AOK (neg 2) w')))

-- * The entry decoder

-- | An instruction's first word and what the sequencer starts it with
-- (its operand's routine: the operand, its size, the continuation, a
-- register operand's place), from its opcode, SR, and DBcc's counter (its
-- low word 0) as the step starts it.
entry :: Unsigned 16 -> Unsigned 16 -> Bool -> (U, CtlOp)
entry o sr dyLow0
  | o == 0x4E71 = plain UNop
  | o .&. 0xF100 == 0x7000 = plain UMoveq
  | o .&. 0xC000 == 0 && o .&. 0x3000 /= 0 = moveE
  | isMovem o = (UMovemS mvDir srcM, noCtl { oSz = Just ZMovem })
  | mdk /= MNone = mdE
  | br /= BNone = brE
  | sy /= SNone = sysE
  | isBitOp o = bitE
  | isShift o = if (o `shiftR` 6) .&. 3 == 3 then opnd FRd WSrc ZW CRmw srcM else plain (UShiftR (testBit o 5))
  | form /= FNone = arithE
  | o == 0x4E72 = priv (plain UStopS)
  | otherwise = plain (UIllegal (case o `shiftR` 12 of
                                   0xA -> 10
                                   0xF -> 11
                                   _ -> 4))
 where
  sup = testBit sr 13
  plain l = (l, noCtl)
  priv x = if sup then x else plain UPriv
  (sm, sr') = eaOf WSrc o
  srcM = modeOf sm sr'
  form = arithForm o
  br = brForm o
  mdk = mdForm o
  sy = sysForm o
  sz = szOf o
  long = sz == SL
  cc = cond (resize (o `shiftR` 8)) sr
  -- an operand routine at the start: a register's continuation at once
  opnd f who z c m
    | zeroCycle f m =
        let base = noCtl { oWho = Just who, oSz = Just z, oRet = Just c }
        in case f of
             FRd -> (UCont c, base { oOpv = Just (if m == MDn then OvRegD else OvRegA) })
             _ -> (UAddrDone f m c, base)
    | otherwise = (UEaS f m, noCtl { oWho = Just who, oSz = Just z, oRet = Just c })
  mvDir
    | not (testBit o 10) && sm == 4 = MvPre
    | not (testBit o 10) = MvStore
    | otherwise = MvLoad
  moveE =
    let msz = moveSz o
        (dm, dr) = eaOf WDst o
        memSrc = sm >= 2 && not (sm == 7 && sr' == 4)
        valid = (sm /= 7 || sr' <= 4) && (dm /= 7 || dr <= 1) && not (msz == SB && (sm == 1 || dm == 1))
        md = case dm of
          1 -> DAn
          0 -> DDn
          4 -> if msz == SL then DPreL else DPreW
          7 | dr == 1 && memSrc -> DAbsLMem
          2 -> DInd
          3 -> DPost
          5 -> DDisp
          6 -> DIdx
          _ -> if dr == 0 then DAbsW else DAbsL
    in if valid then opnd FRd WSrc ZMove (CMoveSrc md) srcM else plain (UIllegal 4)
  mdE = case mdk of
    MDiv -> opnd FRd WSrc ZW CDiv srcM
    MMul -> opnd FRd WSrc ZW CMul srcM
    MNbcd -> if sm == 0 then plain UNbcdR else (UEaS FRd srcM, noCtl { oWho = Just WSrc, oSz = Just ZB, oRet = Just CRmw })
    MMovep -> plain (UMovepS (testBit o 7))
    _ | not (testBit o 3) -> plain (UBcdR (mdk == MAbcd))
      | otherwise -> (UXS False, noCtl { oSz = Just ZB })
  brE = case br of
    BBcc ->
      let c = resize (o `shiftR` 8) :: Unsigned 4
          word = o .&. 0xFF == 0
      in if c == 1 then plain (UBsr word)
         else plain (UBcc (c == 0 || cc) word)
    BDbcc -> plain (if cc then UDbcc True False else UDbcc False dyLow0)
    BJmp -> plain (UJtS srcM False)
    BJsr -> plain (UJtS srcM True)
    BRts -> plain (URetS RkRts)
    BRtr -> plain (URetS RkRtr)
    BRte -> priv (plain (URetS RkRte))
    BReset -> priv (plain UResetS)
    BNone -> plain (UIllegal 4)
  sysE = case sy of
    SCcrImm -> plain UCcrImm
    SSrImm -> priv (plain USrImm)
    STrapv -> plain (UTrapvS (testBit sr 1))
    STrap -> plain UTrap
    SLink -> plain (ULinkS (field o 0 == 7))
    SUnlk -> plain UUnlkS
    SToUsp -> priv (plain UToUsp)
    SFromUsp -> priv (plain UFromUsp)
    SSwap -> plain USwap
    SExtW -> plain UExtW
    SExtL -> plain UExtL
    SPea -> opnd FAd WSrc ZL CPea srcM
    SLea -> opnd FAd WSrc ZL CLea srcM
    SChk -> opnd FRd WSrc ZW CChk srcM
    SFromSr -> if sm == 0 then plain UFromSrR else (UEaS FRd srcM, noCtl { oWho = Just WSrc, oSz = Just ZW, oRet = Just CRmw })
    SToCcr -> opnd FRd WSrc ZW (CCcrSr True) srcM
    SToSr -> priv (opnd FRd WSrc ZW (CCcrSr False) srcM)
    SScc -> if sm == 0 then plain (USccR cc) else (UEaS FRd srcM, noCtl { oWho = Just WSrc, oSz = Just ZB, oRet = Just CRmw })
    STas -> if sm == 0 then plain UTasR else opnd FAs WSrc ZB CTas srcM
    SExg -> plain (UExg (case (o `shiftR` 3) .&. 0x1F of
                           0x08 -> 0
                           0x09 -> 1
                           _ -> 2))
    SNone -> plain (UIllegal 4)
  bitE =
    let btst = (o `shiftR` 6) .&. 3 == 0
    in if o .&. 0xFF00 == 0x0800 then plain (UBitS srcM btst)
       else if sm == 0 then plain UBitR
       else plain (UBitDynM srcM btst)
  arithE = case form of
    FToDn -> opnd FRd WSrc ZOp (CToDn (lineAlu o /= ACmp) (if long then resize (toDnIdle o) else 0)) srcM
    FToMem -> (UToMemS srcM, noCtl)
    FEorDn -> plain (UEorDn long)
    FAddr ->
      let w = field o 6 == 3
          k = case lineAlu o of
            ACmp -> KCmpa
            AAdd -> KAdda
            _ -> KSuba
          n = if k == KCmpa then 2 else if w || sm <= 1 || (sm == 7 && sr' == 4) then 4 else 2
      in opnd FRd WSrc (if w then ZW else ZL) (CAddr k w n) srcM
    FExt -> if sm == 0 then plain (UAddxR (o `shiftR` 12 == 0xD) long) else (UXS long, noCtl { oSz = Just ZOp })
    FCmpm -> (UEaS FRd MPost, noCtl { oWho = Just WPostY, oSz = Just ZOp, oRet = Just CCmpmY })
    FImm ->
      let (dm, dr) = eaOf WSrc o
      in (UEaS FRd MImm, noCtl { oWho = Just WImm, oSz = Just ZOp
                              , oRet = Just (CImmS (modeOf dm dr) (immAlu o == ACmp) long) })
    FQuick
      | sm == 1 -> plain (UQuickA (testBit o 8) (sz == SW))
      | sm == 0 -> plain (UQuickD long)
      | otherwise -> (UQuickM srcM, noCtl)
    FUnary -> if sm == 0 then plain (UUnaryR long) else (UEaS FRd srcM, noCtl { oWho = Just WSrc, oSz = Just ZOp, oRet = Just CRmw })
    FTst -> opnd FRd WSrc ZOp CTst srcM
    FNone -> plain (UIllegal 4)

-- | A long <ea>,Dn's idle clocks (as "M68k.Core"'s).
toDnIdle :: Unsigned 16 -> Unsigned 8
toDnIdle o =
  let (m, r) = eaOf WSrc o
  in if lineAlu o /= ACmp && (m <= 1 || (m == 7 && r == 4)) then 4 else 2
