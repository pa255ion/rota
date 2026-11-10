{-# LANGUAGE MultiWayIf #-}
{-|
The WDC 65C816 reference model (PLAN.md step 7.1; docs/systems/snes.md):
one bus cycle at a time, in the chip's order, so a machine around it sees
every read, write and internal cycle it makes. The SNES's 5A22 times each
cycle by its kind and address (6, 8 or 12 master clocks), so internal
cycles are cycles of their own here ('busIdle'), with the address the
chip puts on the bus.

Sources: WDC's W65C816S data sheet (table 5-7, the cycle-by-cycle
listing; the addressing modes, 5.1 to 5.24; the E-mode differences),
"Programming the 65816" (Eyes and Lichty) for the instruction set, and
the conventions SingleStepTests (github.com/SingleStepTests/65816) was
generated with, which it is checked against: final state and every
cycle's address, data, read or write, VDA, VPA, VPB and MLB.

__The bus__ ('Bus') belongs to the caller. A read is an opcode fetch
(VDA and VPA), an operand fetch (VPA), a data read (VDA) or a vector read
(VDA and VPB); a write is a data write, or, in emulation mode, the
modify cycle of a read-modify-write, which writes the unchanged value with
VDA low ('busWrite' 'False'). An internal operation ('busIdle') has
neither VDA nor VPA. MLB ("lock") is set on a read-modify-write's data
cycles. Each call is one cycle; after it the model samples NMI and IRQ.

__Wrapping__ (the data sheet, and the tests): absolute and long data
addresses carry into the bank (24 bits); the program counter and the
direct page and stack addresses wrap within their bank (16 bits). In
emulation mode the stack is page 1 and, with the low byte of D zero, the
6502's direct page modes wrap within the page; the 65816's own
instructions (PEA, PEI, PER, PHD, PLD, PLB, JSL, RTL, JSR (a,x), [d] and
[d],y) use the full 16-bit stack and direct page addresses during the
instruction, and S's high byte is 01h again after it.

Where SingleStepTests and the hardware differ ('Config'): JSR (a,x)'s
stack and (d,x)'s pointer in emulation mode; 'w65c816' is the hardware
(gilyon's cputest), 'singleStep65816' the tests' model.

__Interrupts:__ NMI is edge triggered, IRQ level triggered and masked by
I; an instruction is followed by the interrupt its second-to-last cycle
saw, as on the 6502 (*to verify* against the 5A22 at step 7.3). WAI waits
(one stopped cycle per 'step') until NMI or IRQ, then takes it, or with I
set goes on with the next instruction; STP stops until 'reset'.

__MVN and MVP__ move one byte per 'step' (7 cycles); while bytes remain
the PC stays on the instruction, so interrupts come between bytes, as on
the chip.
-}
module W65c816.Spec
  ( -- * Configuration
    Config (..)
  , w65c816
  , singleStep65816
    -- * The bus
  , Bus (..)
  , Kind (..)
    -- * The CPU
  , Cpu
  , newCpu
  , getRegs
  , setRegs
  , cpuCycles
  , Step (..)
  , step
  , reset
  , Run (..)
  , cpuRun
    -- * Save states (the SNES, step 7.15)
  , cpuLines
  ) where

import Prelude

import Control.Monad (replicateM_, unless, when)
import Control.Monad.ST (ST)
import Data.Bits
import qualified Data.Vector.Unboxed.Mutable as MV
import Data.Word

import W65c816.Types

-- | What differs between the chip and the model SingleStepTests was made
-- with.
data Config = Config
  { cfgDpXIndWrap :: Bool
    -- ^ emulation mode, DL /= 0: (d,x) reads its pointer's high byte
    -- wrapped within the low byte's page (gilyon's cputest, measured on a
    -- SNES); SingleStepTests reads it from the next address
  , cfgJsrIndXNew :: Bool
    -- ^ emulation mode: JSR (a,x) pushes with the full 16-bit S, as the
    -- 65816's other new instructions (gilyon's cputest, test 0277: S =
    -- 0100h pushes to 0100h, 00FFh); SingleStepTests keeps it in page 1
  } deriving stock (Show, Eq)

-- | The 65C816 as the SNES's 5A22 has it (gilyon's cputest, which was
-- checked on hardware, where it and SingleStepTests disagree).
w65c816 :: Config
w65c816 = Config { cfgDpXIndWrap = True, cfgJsrIndXNew = True }

-- | The 65C816 as SingleStepTests' generator models it. Only for that set.
singleStep65816 :: Config
singleStep65816 = Config { cfgDpXIndWrap = False, cfgJsrIndXNew = False }

-- | What a read cycle is, as VDA, VPA and VPB show it.
data Kind
  = KOpcode  -- ^ VDA and VPA: the first byte of an instruction
  | KOperand -- ^ VPA: an instruction's operand
  | KData    -- ^ VDA
  | KVector  -- ^ VDA and VPB: an interrupt vector
  deriving stock (Show, Eq)

-- | The machine around the CPU. Addresses are 24 bits.
data Bus s = Bus
  { busRead    :: Kind -> Bool -> Word32 -> ST s Word8
    -- ^ kind, lock (MLB), address
  , busWrite   :: Bool -> Bool -> Word32 -> Word8 -> ST s ()
    -- ^ VDA (low only for emulation mode's modify cycle), lock, address, data
  , busIdle    :: Bool -> Word32 -> ST s ()
    -- ^ an internal operation: lock, the address on the bus
  , busStopped :: ST s ()
    -- ^ a cycle with WAI waiting or STP stopped: nothing on the bus
  , busNmi     :: ST s Bool -- ^ the NMI line, 'True' = asserted
  , busIrq     :: ST s Bool -- ^ the IRQ line, 'True' = asserted
  }

data Cpu s = Cpu
  { cpuConfig :: !Config
  , cpuBus   :: !(Bus s)
  , cpuState :: !(MV.MVector s Int)
  }

-- State slots.
iA, iX, iY, iS, iD, iDBR, iPBR, iPC, iP, iE, iCyc, iNmiLine, iNmiNeed,
  iPollCur, iPollPrev, iPending, iRun, iLock :: Int
iA = 0
iX = 1
iY = 2
iS = 3
iD = 4
iDBR = 5
iPBR = 6
iPC = 7
iP = 8
iE = 9
iCyc = 10
iNmiLine = 11  -- NMI line at the end of the last cycle (edge detector)
iNmiNeed = 12  -- NMI edge seen, not yet serviced
iPollCur = 13  -- at the end of the last cycle: bit 0 NMI, bit 1 IRQ would be taken
iPollPrev = 14 -- the same, one cycle earlier
iPending = 15  -- 'iPollPrev' at the end of the last instruction
iRun = 16      -- 0 running, 1 WAI waiting, 2 STP stopped
iLock = 17     -- MLB for the cycles now

slots :: Int
slots = 18

-- | Whether the CPU runs, waits (WAI) or is stopped (STP).
data Run = Running | Waiting | Stopped
  deriving stock (Show, Eq)

newCpu :: Config -> Bus s -> Regs -> ST s (Cpu s)
newCpu cfg bus r = do
  st <- MV.replicate slots 0
  let c = Cpu cfg bus st
  setRegs c r
  pure c

getRegs :: Cpu s -> ST s Regs
getRegs c = do
  normalise c
  Regs <$> w16 iA <*> w16 iX <*> w16 iY <*> w16 iS <*> w16 iD
       <*> (fromIntegral <$> g c iDBR) <*> (fromIntegral <$> g c iPBR)
       <*> w16 iPC <*> (fromIntegral <$> g c iP) <*> ((/= 0) <$> g c iE)
 where
  w16 i = fromIntegral <$> g c i

-- | Sets the registers; in emulation mode S's high byte is 01h, X's and
-- Y's high bytes 00h and P's bits 4 and 5 set, whatever is given.
setRegs :: Cpu s -> Regs -> ST s ()
setRegs c r = do
  s c iA (fromIntegral (rA r))
  s c iX (fromIntegral (rX r))
  s c iY (fromIntegral (rY r))
  s c iS (fromIntegral (rS r))
  s c iD (fromIntegral (rD r))
  s c iDBR (fromIntegral (rDBR r))
  s c iPBR (fromIntegral (rPBR r))
  s c iPC (fromIntegral (rPC r))
  s c iP (fromIntegral (rP r))
  s c iE (if rE r then 1 else 0)
  normalise c

-- | Cycles run since 'newCpu'.
cpuCycles :: Cpu s -> ST s Int
cpuCycles c = g c iCyc

cpuRun :: Cpu s -> ST s Run
cpuRun c = (\r -> case r of 0 -> Running; 1 -> Waiting; _ -> Stopped) <$> g c iRun

-- | Between instructions, beyond the registers: WAI/STP (0 running, 1
-- waiting, 2 stopped), the NMI line at the last cycle's end, an NMI edge
-- not yet serviced, the interrupt polls at the last cycle's end and the
-- one before it, and the poll the last instruction ended with (bit 0 NMI,
-- bit 1 IRQ). A save state's table keeps them (the SNES, step 7.15).
cpuLines :: Cpu s -> ST s (Int, Int, Int, Int, Int, Int)
cpuLines c = (,,,,,) <$> g c iRun <*> g c iNmiLine <*> g c iNmiNeed <*> g c iPollCur <*> g c iPollPrev <*> g c iPending

-- | What a 'step' did.
data Step
  = Instr !Word8 -- ^ executed this opcode (one byte's move for MVN, MVP)
  | Nmi          -- ^ the interrupt sequence, through the NMI vector
  | Irq          -- ^ the interrupt sequence, through the IRQ vector
  | Wait         -- ^ one cycle of WAI's wait
  | Stop         -- ^ one cycle of STP
  deriving stock (Show, Eq)

-- * State access

g :: Cpu s -> Int -> ST s Int
g c = MV.unsafeRead (cpuState c)
{-# INLINE g #-}

s :: Cpu s -> Int -> Int -> ST s ()
s c = MV.unsafeWrite (cpuState c)
{-# INLINE s #-}

emu :: Cpu s -> ST s Bool
emu c = (/= 0) <$> g c iE

flag :: Cpu s -> Word8 -> ST s Bool
flag c f = (\p -> p .&. fromIntegral f /= 0) <$> g c iP

setFlag :: Cpu s -> Word8 -> Bool -> ST s ()
setFlag c f b = do
  p <- g c iP
  s c iP (if b then p .|. fromIntegral f else p .&. complement (fromIntegral f))

-- | 8-bit accumulator and memory.
m8 :: Cpu s -> ST s Bool
m8 c = flag c flagM

-- | 8-bit index registers.
x8 :: Cpu s -> ST s Bool
x8 c = flag c flagX

-- | Emulation mode's invariants, and the 8-bit index registers' high bytes.
normalise :: Cpu s -> ST s ()
normalise c = do
  e <- emu c
  when e $ do
    p <- g c iP
    s c iP (p .|. 0x30)
    sp <- g c iS
    s c iS (0x100 .|. (sp .&. 0xFF))
  x <- x8 c
  when x $ do
    g c iX >>= s c iX . (.&. 0xFF)
    g c iY >>= s c iY . (.&. 0xFF)

setNZ :: Cpu s -> Bool -> Int -> ST s ()
setNZ c eight v = do
  let (n, z) = if eight then (v .&. 0x80 /= 0, v .&. 0xFF == 0)
                        else (v .&. 0x8000 /= 0, v .&. 0xFFFF == 0)
  setFlag c flagN n
  setFlag c flagZ z

-- * Cycles

-- | The end of every cycle: sample NMI and IRQ.
endCycle :: Cpu s -> ST s ()
endCycle c = do
  let bus = cpuBus c
  nmi <- busNmi bus
  prevLine <- g c iNmiLine
  when (nmi && prevLine == 0) (s c iNmiNeed 1)
  s c iNmiLine (if nmi then 1 else 0)
  irq <- busIrq bus
  i <- flag c flagI
  need <- g c iNmiNeed
  s c iPollPrev =<< g c iPollCur
  s c iPollCur (need .|. (if irq && not i then 2 else 0))
  g c iCyc >>= s c iCyc . (+ 1)

lockNow :: Cpu s -> ST s Bool
lockNow c = (/= 0) <$> g c iLock

rd :: Cpu s -> Kind -> Int -> ST s Int
rd c k a = do
  l <- lockNow c
  v <- busRead (cpuBus c) k l (fromIntegral (a .&. 0xFFFFFF))
  endCycle c
  pure (fromIntegral v)

wr :: Cpu s -> Int -> Int -> ST s ()
wr c a v = wrV c True a v

wrV :: Cpu s -> Bool -> Int -> Int -> ST s ()
wrV c vda a v = do
  l <- lockNow c
  busWrite (cpuBus c) vda l (fromIntegral (a .&. 0xFFFFFF)) (fromIntegral v)
  endCycle c

io :: Cpu s -> Int -> ST s ()
io c a = do
  l <- lockNow c
  busIdle (cpuBus c) l (fromIntegral (a .&. 0xFFFFFF))
  endCycle c

-- | PBR:PC.
pcAddr :: Cpu s -> ST s Int
pcAddr c = do
  b <- g c iPBR
  pc <- g c iPC
  pure (b `shiftL` 16 .|. pc)

-- | An internal cycle at PBR:PC (the byte after the instruction so far).
ioPC :: Cpu s -> ST s ()
ioPC c = pcAddr c >>= io c

-- | An internal cycle at PBR:PC-1 (the last operand byte fetched).
ioLast :: Cpu s -> ST s ()
ioLast c = do
  b <- g c iPBR
  pc <- g c iPC
  io c (b `shiftL` 16 .|. ((pc - 1) .&. 0xFFFF))

-- | An operand byte at PBR:PC; PC advances within the bank.
fetch :: Cpu s -> ST s Int
fetch c = do
  a <- pcAddr c
  g c iPC >>= s c iPC . (\pc -> (pc + 1) .&. 0xFFFF)
  rd c KOperand a

fetch16 :: Cpu s -> ST s Int
fetch16 c = do
  lo <- fetch c
  hi <- fetch c
  pure (lo .|. hi `shiftL` 8)

-- * Stack

-- | Push, 6502 style: in emulation mode within page 1.
push :: Cpu s -> Int -> ST s ()
push c v = do
  sp <- g c iS
  wr c sp v
  e <- emu c
  s c iS (if e then 0x100 .|. ((sp - 1) .&. 0xFF) else (sp - 1) .&. 0xFFFF)

pull :: Cpu s -> ST s Int
pull c = do
  sp <- g c iS
  e <- emu c
  let sp' = if e then 0x100 .|. ((sp + 1) .&. 0xFF) else (sp + 1) .&. 0xFFFF
  s c iS sp'
  rd c KData sp'

-- | Push, the 65816's own instructions: the full 16-bit S.
pushN :: Cpu s -> Int -> ST s ()
pushN c v = do
  sp <- g c iS
  wr c sp v
  s c iS ((sp - 1) .&. 0xFFFF)

pullN :: Cpu s -> ST s Int
pullN c = do
  sp <- g c iS
  let sp' = (sp + 1) .&. 0xFFFF
  s c iS sp'
  rd c KData sp'

-- * Addresses

-- | A direct page address, 6502 style: page-wrapped in emulation mode
-- with DL = 0.
dp :: Cpu s -> Int -> ST s Int
dp c off = do
  d <- g c iD
  e <- emu c
  pure $ if e && d .&. 0xFF == 0 then d .|. (off .&. 0xFF) else (d + off) .&. 0xFFFF

-- | A direct page address, the 65816's own modes: no page wrap.
dpN :: Cpu s -> Int -> ST s Int
dpN c off = (\d -> (d + off) .&. 0xFFFF) <$> g c iD

-- | The extra cycle of a direct page mode when DL isn't zero.
dlPenalty :: Cpu s -> ST s ()
dlPenalty c = do
  d <- g c iD
  when (d .&. 0xFF /= 0) (ioLast c)

-- | Where an operand's next byte is.
data Wrap = W24 | W16
  deriving stock (Eq)

nextAddr :: Wrap -> Int -> Int
nextAddr W24 a = (a + 1) .&. 0xFFFFFF
nextAddr W16 a = (a .&. 0xFF0000) .|. ((a + 1) .&. 0xFFFF)

data Access = ARead | AWrite | ARmw
  deriving stock (Eq)

data Mode
  = Imm | Abs | AbsX | AbsY | Long | LongX
  | Dp | DpX | DpY | DpInd | DpIndLong | DpXInd | DpIndY | DpIndLongY
  | Sr | SrIndY
  deriving stock (Eq)

-- | The cycles of a mode up to its data; the data's address and wrap.
-- (Not 'Imm'.)
address :: Cpu s -> Access -> Mode -> ST s (Int, Wrap)
address c acc mode = case mode of
  Abs -> do
    aa <- fetch16 c
    b <- g c iDBR
    pure (b `shiftL` 16 .|. aa, W24)
  AbsX -> indexed iX
  AbsY -> indexed iY
  Long -> do
    aa <- fetch16 c
    b <- fetch c
    pure (b `shiftL` 16 .|. aa, W24)
  LongX -> do
    aa <- fetch16 c
    b <- fetch c
    x <- g c iX
    pure (((b `shiftL` 16 .|. aa) + x) .&. 0xFFFFFF, W24)
  Dp -> do
    off <- fetch c
    dlPenalty c
    a <- dp c off
    pure (a, W16)
  DpX -> dpIndexed iX
  DpY -> dpIndexed iY
  DpInd -> do
    off <- fetch c
    dlPenalty c
    ptr <- pointer16 =<< mapM (dp c) [off, off + 1]
    b <- g c iDBR
    pure (b `shiftL` 16 .|. ptr, W24)
  DpIndLong -> do
    off <- fetch c
    dlPenalty c
    p <- pointer24 off
    pure (p, W24)
  DpXInd -> do
    off <- fetch c
    dlPenalty c
    ioLast c
    x <- g c iX
    e <- emu c
    d <- g c iD
    -- emulation mode with DL /= 0: the pointer's low byte at D+off+X, its
    -- high byte one further but wrapped within that page ('cfgDpXIndWrap')
    a0 <- dp c (off + x)
    let quirk = cfgDpXIndWrap (cpuConfig c) && e && d .&. 0xFF /= 0
        a1 = (a0 .&. 0xFF00) .|. ((a0 + 1) .&. 0xFF)
    ptr <- pointer16 =<< if quirk then pure [a0, a1] else mapM (dp c) [off + x, off + x + 1]
    b <- g c iDBR
    pure (b `shiftL` 16 .|. ptr, W24)
  DpIndY -> do
    off <- fetch c
    dlPenalty c
    ptr <- pointer16 =<< mapM (dp c) [off, off + 1]
    b <- g c iDBR
    y <- g c iY
    indexIO (b `shiftL` 16 .|. ptr) y
    pure (((b `shiftL` 16 .|. ptr) + y) .&. 0xFFFFFF, W24)
  DpIndLongY -> do
    off <- fetch c
    dlPenalty c
    base <- pointer24 off
    y <- g c iY
    pure ((base + y) .&. 0xFFFFFF, W24)
  Sr -> do
    off <- fetch c
    ioLast c
    sp <- g c iS
    pure ((sp + off) .&. 0xFFFF, W16)
  SrIndY -> do
    off <- fetch c
    ioLast c
    sp <- g c iS
    lo <- rd c KData ((sp + off) .&. 0xFFFF)
    hi <- rd c KData ((sp + off + 1) .&. 0xFFFF)
    io c ((sp + off + 1) .&. 0xFFFF)
    b <- g c iDBR
    y <- g c iY
    pure (((b `shiftL` 16 .|. lo .|. hi `shiftL` 8) + y) .&. 0xFFFFFF, W24)
  Imm -> error "address: Imm"
 where
  indexed ri = do
    aa <- fetch16 c
    b <- g c iDBR
    i <- g c ri
    indexIO (b `shiftL` 16 .|. aa) i
    pure (((b `shiftL` 16 .|. aa) + i) .&. 0xFFFFFF, W24)
  -- the extra cycle of an indexed mode: always for writes and
  -- read-modify-writes and with 16-bit index registers, else on a page
  -- crossing; at the base's page with the low byte indexed
  indexIO base i = do
    x <- x8 c
    let cross = (base .&. 0xFF) + (i .&. 0xFF) > 0xFF
    when (acc /= ARead || not x || cross) $
      io c ((base .&. 0xFFFF00) .|. ((base + i) .&. 0xFF))
  dpIndexed ri = do
    off <- fetch c
    dlPenalty c
    ioLast c
    i <- g c ri
    a <- dp c (off + i)
    pure (a, W16)
  pointer16 [a0, a1] = do
    lo <- rd c KData a0
    hi <- rd c KData a1
    pure (lo .|. hi `shiftL` 8)
  pointer16 _ = error "pointer16"
  pointer24 off = do
    lo <- rd c KData =<< dpN c off
    hi <- rd c KData =<< dpN c (off + 1)
    b <- rd c KData =<< dpN c (off + 2)
    pure (b `shiftL` 16 .|. hi `shiftL` 8 .|. lo)

-- | Reads an operand of 8 or 16 bits.
readOperand :: Cpu s -> Bool -> Mode -> ST s Int
readOperand c eight Imm = if eight then fetch c else fetch16 c
readOperand c eight mode = do
  (a, w) <- address c ARead mode
  lo <- rd c KData a
  if eight then pure lo else do
    hi <- rd c KData (nextAddr w a)
    pure (lo .|. hi `shiftL` 8)

writeOperand :: Cpu s -> Bool -> Mode -> Int -> ST s ()
writeOperand c eight mode v = do
  (a, w) <- address c AWrite mode
  wr c a (v .&. 0xFF)
  unless eight (wr c (nextAddr w a) (v `shiftR` 8 .&. 0xFF))

-- | A read-modify-write: the data, the modify cycle (an internal cycle; in
-- emulation mode a write of the unchanged byte with VDA low), the result,
-- high byte first.
modify :: Cpu s -> Mode -> (Bool -> Int -> ST s Int) -> ST s ()
modify c mode f = do
  eight <- m8 c
  e <- emu c
  (a, w) <- address c ARmw mode
  s c iLock 1
  let a1 = nextAddr w a
  lo <- rd c KData a
  v <- if eight then pure lo else (\hi -> lo .|. hi `shiftL` 8) <$> rd c KData a1
  if e then wrV c False a lo else io c (if eight then a else a1)
  r <- f eight v
  unless eight (wr c a1 (r `shiftR` 8 .&. 0xFF))
  wr c a (r .&. 0xFF)
  s c iLock 0

-- * Registers by width

getAcc :: Cpu s -> Bool -> ST s Int
getAcc c eight = (\a -> if eight then a .&. 0xFF else a) <$> g c iA

-- | Sets A (8 bits: only the low byte, B kept).
setAcc :: Cpu s -> Bool -> Int -> ST s ()
setAcc c eight v = do
  a <- g c iA
  s c iA (if eight then (a .&. 0xFF00) .|. (v .&. 0xFF) else v .&. 0xFFFF)

setIdx :: Cpu s -> Int -> Bool -> Int -> ST s ()
setIdx c i eight v = s c i (if eight then v .&. 0xFF else v .&. 0xFFFF)

-- * Arithmetic

adc :: Cpu s -> Bool -> Int -> ST s ()
adc c eight d = do
  a <- getAcc c eight
  cf <- flag c flagC
  dm <- flag c flagD
  let ci = if cf then 1 else 0
      top = if eight then 0x80 else 0x8000
      lim = if eight then 0xFF else 0xFFFF
      n = if eight then 2 else 4
      (r0, r1) = if dm then digits a ci n else (a + d + ci, a + d + ci)
      v = complement (a `xor` d) .&. (a `xor` r0) .&. top /= 0
  setFlag c flagV v
  setFlag c flagC (r1 > lim)
  setAcc c eight r1
  setNZ c eight r1
 where
  -- BCD addition digit by digit, each adjusted by 6 when it passes 9; V
  -- comes from the sum before the top digit's adjustment, so this returns
  -- that sum and the adjusted one
  digits a ci n = go 0 ci 0
   where
    go k carry low =
      let sh = 4 * k
          r = (a .&. (0xF `shiftL` sh)) + (d .&. (0xF `shiftL` sh)) + (carry `shiftL` sh) + low
          r' = if r >= 0xA `shiftL` sh then r + (0x6 `shiftL` sh) else r
          full = (0x10 `shiftL` sh) - 1
      in if k == n - 1 then (r, r')
         else go (k + 1) (if r' > full then 1 else 0) (r' .&. full)

sbc :: Cpu s -> Bool -> Int -> ST s ()
sbc c eight d0 = do
  a <- getAcc c eight
  cf <- flag c flagC
  dm <- flag c flagD
  let lim = if eight then 0xFF else 0xFFFF
      d = complement d0 .&. lim
      ci = if cf then 1 else 0
      top = if eight then 0x80 else 0x8000
      n = if eight then 2 else 4
      (r0, r1) = if dm then digits a d ci n else (a + d + ci, a + d + ci)
      v = complement (a `xor` d) .&. (a `xor` r0) .&. top /= 0
  setFlag c flagV v
  setFlag c flagC (r1 > lim)
  setAcc c eight r1
  setNZ c eight r1
 where
  digits a d ci n = go 0 ci 0
   where
    go k carry acc =
      let r = (a .&. m k) + (d .&. m k) + (carry `shiftL` (4 * k)) + acc
          top = (0x10 `shiftL` (4 * k)) - 1
          r' = if r <= top then r - (0x6 `shiftL` (4 * k)) else r
          c' = if r' > top then 1 else 0
      in if k == n - 1 then (r, r') else go (k + 1) c' (r' .&. top)
    m k = 0xF `shiftL` (4 * k)

cmpReg :: Cpu s -> Bool -> Int -> Int -> ST s ()
cmpReg c eight reg v = do
  let lim = if eight then 0xFF else 0xFFFF
      r = ((reg .&. lim) - (v .&. lim)) .&. lim
  setFlag c flagC ((reg .&. lim) >= (v .&. lim))
  setNZ c eight r

-- * Instructions

data Alu = ORA | AND | EOR | ADC | LDA | CMP | SBC | BIT | LDX | LDY | CPX | CPY
  deriving stock (Eq)

aluOp :: Cpu s -> Alu -> Mode -> ST s ()
aluOp c op mode = do
  idx <- x8 c
  acc8 <- m8 c
  let eight = if op `elem` [LDX, LDY, CPX, CPY] then idx else acc8
  v <- readOperand c eight mode
  a <- getAcc c eight
  case op of
    ORA -> setAcc c eight (a .|. v) >> setNZ c eight (a .|. v)
    AND -> setAcc c eight (a .&. v) >> setNZ c eight (a .&. v)
    EOR -> setAcc c eight (a `xor` v) >> setNZ c eight (a `xor` v)
    ADC -> adc c eight v
    SBC -> sbc c eight v
    LDA -> setAcc c eight v >> setNZ c eight v
    CMP -> cmpReg c eight a v
    BIT -> do
      setFlag c flagZ (a .&. v == 0)
      unless (mode == Imm) $ do
        setFlag c flagN (v .&. (if eight then 0x80 else 0x8000) /= 0)
        setFlag c flagV (v .&. (if eight then 0x40 else 0x4000) /= 0)
    LDX -> setIdx c iX eight v >> setNZ c eight v
    LDY -> setIdx c iY eight v >> setNZ c eight v
    CPX -> g c iX >>= \x -> cmpReg c eight x v
    CPY -> g c iY >>= \y -> cmpReg c eight y v

data Shift = ASL | LSR | ROL | ROR | INC | DEC | TSB | TRB
  deriving stock (Eq)

shiftOp :: Cpu s -> Shift -> Bool -> Int -> ST s Int
shiftOp c op eight v = do
  cf <- flag c flagC
  let top = if eight then 0x80 else 0x8000
      lim = if eight then 0xFF else 0xFFFF
      ci = if cf then 1 else 0
  case op of
    ASL -> do setFlag c flagC (v .&. top /= 0); let r = (v `shiftL` 1) .&. lim in setNZ c eight r >> pure r
    LSR -> do setFlag c flagC (v .&. 1 /= 0); let r = v `shiftR` 1 in setNZ c eight r >> pure r
    ROL -> do setFlag c flagC (v .&. top /= 0); let r = ((v `shiftL` 1) .|. ci) .&. lim in setNZ c eight r >> pure r
    ROR -> do
      setFlag c flagC (v .&. 1 /= 0)
      let r = (v `shiftR` 1) .|. (if cf then top else 0)
      setNZ c eight r >> pure r
    INC -> let r = (v + 1) .&. lim in setNZ c eight r >> pure r
    DEC -> let r = (v - 1) .&. lim in setNZ c eight r >> pure r
    TSB -> do a <- getAcc c eight; setFlag c flagZ (a .&. v == 0); pure (v .|. a)
    TRB -> do a <- getAcc c eight; setFlag c flagZ (a .&. v == 0); pure (v .&. complement a .&. lim)

-- | A one-byte instruction: the opcode, then an internal cycle at PC.
implied :: Cpu s -> ST s () -> ST s ()
implied c f = ioPC c >> f

store :: Cpu s -> Int -> Mode -> ST s ()
store c ri mode = do
  eight <- if ri == iX || ri == iY then x8 c else m8 c
  v <- if ri == -1 then pure 0 else g c ri
  writeOperand c eight mode v

-- | Sets P: emulation mode keeps M and X; X set clears the index
-- registers' high bytes.
setP :: Cpu s -> Int -> ST s ()
setP c p = s c iP (p .&. 0xFF) >> normalise c

branch :: Cpu s -> Bool -> ST s ()
branch c taken = do
  off <- fetch c
  when taken $ do
    ioLast c
    pc <- g c iPC
    let pc' = (pc + (if off >= 0x80 then off - 0x100 else off)) .&. 0xFFFF
    e <- emu c
    when (e && (pc' .&. 0xFF00) /= (pc .&. 0xFF00)) (ioLast c)
    s c iPC pc'

-- | The interrupt sequence's pushes and vector (BRK and COP after their
-- signature byte, a hardware interrupt after its two internal cycles).
interrupt :: Cpu s -> Int -> Int -> Bool -> ST s ()
interrupt c vecNative vecEmu brk = do
  e <- emu c
  unless e (g c iPBR >>= push c)
  pc <- g c iPC
  push c (pc `shiftR` 8)
  push c (pc .&. 0xFF)
  p <- g c iP
  push c (if e && not brk then p .&. complement 0x10 else p)
  setFlag c flagI True
  setFlag c flagD False
  s c iPBR 0
  let v = if e then vecEmu else vecNative
  lo <- rd c KVector v
  hi <- rd c KVector (v + 1)
  s c iPC (lo .|. hi `shiftL` 8)

-- | One instruction, interrupt sequence, or stopped cycle.
step :: Cpu s -> ST s Step
step c = do
  run <- g c iRun
  case run of
    2 -> busStopped (cpuBus c) >> endCycle c >> pure Stop
    1 -> do
      busStopped (cpuBus c)
      endCycle c
      irq <- busIrq (cpuBus c)
      nmi <- g c iNmiNeed
      when (irq || nmi /= 0) $ do
        s c iRun 0
        -- with I set, an IRQ only ends the wait
        s c iPending =<< g c iPollCur
      pure Wait
    _ -> do
      pend <- g c iPending
      r <- if
        | pend .&. 1 /= 0 -> do
            s c iNmiNeed 0
            hardware 0xFFEA 0xFFFA
            pure Nmi
        | pend .&. 2 /= 0 -> hardware 0xFFEE 0xFFFE >> pure Irq
        | otherwise -> do
            a <- pcAddr c
            g c iPC >>= s c iPC . (\pc -> (pc + 1) .&. 0xFFFF)
            op <- rd c KOpcode a
            execute c op
            pure (Instr (fromIntegral op))
      normalise c
      s c iPending =<< g c iPollPrev
      pure r
 where
  hardware vn ve = do
    ioPC c
    ioPC c
    interrupt c vn ve False

-- | /RES: emulation mode, the registers as 'resetRegs' leaves them (A, X's
-- and Y's low bytes kept), then the vector at 00FFFCh (two internal
-- cycles first, *to verify* the cycle count against the data sheet at 7.3).
reset :: Cpu s -> ST s ()
reset c = do
  s c iE 1
  s c iD 0
  s c iDBR 0
  s c iPBR 0
  p <- g c iP
  s c iP ((p .|. 0x34) .&. complement 0x08)
  s c iRun 0
  s c iNmiNeed 0
  s c iPending 0
  normalise c
  replicateM_ 2 (ioPC c)
  lo <- rd c KVector 0xFFFC
  hi <- rd c KVector 0xFFFD
  s c iPC (lo .|. hi `shiftL` 8)

execute :: Cpu s -> Int -> ST s ()
execute c op
  -- the ALU column (the 6502's cc = 01 and the 65816's modes beside it):
  -- the row is the operation, the low five bits the mode; row 4 is STA
  | col `elem` aluCols && row /= 4 = aluOp c ([ORA, AND, EOR, ADC, LDA, LDA, CMP, SBC] !! row) (colMode col)
  | col `elem` aluCols && op /= 0x89 = store c iA (colMode col)
  | otherwise = execOther c op
 where
  row = op `shiftR` 5
  col = op .&. 0x1F
  aluCols = [0x01, 0x03, 0x05, 0x07, 0x09, 0x0D, 0x0F, 0x11, 0x12, 0x13, 0x15, 0x17, 0x19, 0x1D, 0x1F]
  colMode m = case m of
    0x01 -> DpXInd
    0x03 -> Sr
    0x05 -> Dp
    0x07 -> DpIndLong
    0x09 -> Imm
    0x0D -> Abs
    0x0F -> Long
    0x11 -> DpIndY
    0x12 -> DpInd
    0x13 -> SrIndY
    0x15 -> DpX
    0x17 -> DpIndLongY
    0x19 -> AbsY
    0x1D -> AbsX
    _ -> LongX

execOther :: Cpu s -> Int -> ST s ()
execOther c op = case op of
  0x89 -> aluOp c BIT Imm
  -- stores
  0x86 -> store c iX Dp
  0x8E -> store c iX Abs
  0x96 -> store c iX DpY
  0x84 -> store c iY Dp
  0x8C -> store c iY Abs
  0x94 -> store c iY DpX
  0x64 -> store c (-1) Dp
  0x74 -> store c (-1) DpX
  0x9C -> store c (-1) Abs
  0x9E -> store c (-1) AbsX
  -- index loads and compares
  0xA2 -> aluOp c LDX Imm
  0xA6 -> aluOp c LDX Dp
  0xAE -> aluOp c LDX Abs
  0xB6 -> aluOp c LDX DpY
  0xBE -> aluOp c LDX AbsY
  0xA0 -> aluOp c LDY Imm
  0xA4 -> aluOp c LDY Dp
  0xAC -> aluOp c LDY Abs
  0xB4 -> aluOp c LDY DpX
  0xBC -> aluOp c LDY AbsX
  0xE0 -> aluOp c CPX Imm
  0xE4 -> aluOp c CPX Dp
  0xEC -> aluOp c CPX Abs
  0xC0 -> aluOp c CPY Imm
  0xC4 -> aluOp c CPY Dp
  0xCC -> aluOp c CPY Abs
  0x24 -> aluOp c BIT Dp
  0x2C -> aluOp c BIT Abs
  0x34 -> aluOp c BIT DpX
  0x3C -> aluOp c BIT AbsX
  -- read-modify-write
  0x06 -> modify c Dp (shiftOp c ASL)
  0x0E -> modify c Abs (shiftOp c ASL)
  0x16 -> modify c DpX (shiftOp c ASL)
  0x1E -> modify c AbsX (shiftOp c ASL)
  0x46 -> modify c Dp (shiftOp c LSR)
  0x4E -> modify c Abs (shiftOp c LSR)
  0x56 -> modify c DpX (shiftOp c LSR)
  0x5E -> modify c AbsX (shiftOp c LSR)
  0x26 -> modify c Dp (shiftOp c ROL)
  0x2E -> modify c Abs (shiftOp c ROL)
  0x36 -> modify c DpX (shiftOp c ROL)
  0x3E -> modify c AbsX (shiftOp c ROL)
  0x66 -> modify c Dp (shiftOp c ROR)
  0x6E -> modify c Abs (shiftOp c ROR)
  0x76 -> modify c DpX (shiftOp c ROR)
  0x7E -> modify c AbsX (shiftOp c ROR)
  0xE6 -> modify c Dp (shiftOp c INC)
  0xEE -> modify c Abs (shiftOp c INC)
  0xF6 -> modify c DpX (shiftOp c INC)
  0xFE -> modify c AbsX (shiftOp c INC)
  0xC6 -> modify c Dp (shiftOp c DEC)
  0xCE -> modify c Abs (shiftOp c DEC)
  0xD6 -> modify c DpX (shiftOp c DEC)
  0xDE -> modify c AbsX (shiftOp c DEC)
  0x04 -> modify c Dp (shiftOp c TSB)
  0x0C -> modify c Abs (shiftOp c TSB)
  0x14 -> modify c Dp (shiftOp c TRB)
  0x1C -> modify c Abs (shiftOp c TRB)
  -- accumulator
  0x0A -> accOp ASL
  0x4A -> accOp LSR
  0x2A -> accOp ROL
  0x6A -> accOp ROR
  0x1A -> accOp INC
  0x3A -> accOp DEC
  -- index increments
  0xE8 -> implied c (incIdx iX 1)
  0xC8 -> implied c (incIdx iY 1)
  0xCA -> implied c (incIdx iX (-1))
  0x88 -> implied c (incIdx iY (-1))
  -- transfers
  0xAA -> implied c (xfer iA iX)
  0xA8 -> implied c (xfer iA iY)
  0x8A -> implied c (xferToA iX)
  0x98 -> implied c (xferToA iY)
  0xBA -> implied c (xfer iS iX)
  0x9A -> implied c $ do
    e <- emu c
    x <- g c iX
    s c iS (if e then 0x100 .|. (x .&. 0xFF) else x)
  0x9B -> implied c (xfer iX iY)
  0xBB -> implied c (xfer iY iX)
  0x5B -> implied c (g c iA >>= \a -> s c iD a >> setNZ c False a)
  0x7B -> implied c (g c iD >>= \d -> s c iA d >> setNZ c False d)
  0x1B -> implied c $ do
    e <- emu c
    a <- g c iA
    s c iS (if e then 0x100 .|. (a .&. 0xFF) else a)
  0x3B -> implied c (g c iS >>= \sp -> s c iA sp >> setNZ c False sp)
  0xEB -> do
    ioPC c
    ioPC c
    a <- g c iA
    let a' = (a `shiftR` 8) .|. ((a .&. 0xFF) `shiftL` 8)
    s c iA a'
    setNZ c True (a' .&. 0xFF)
  -- flags
  0x18 -> implied c (setFlag c flagC False)
  0x38 -> implied c (setFlag c flagC True)
  0x58 -> implied c (setFlag c flagI False)
  0x78 -> implied c (setFlag c flagI True)
  0xB8 -> implied c (setFlag c flagV False)
  0xD8 -> implied c (setFlag c flagD False)
  0xF8 -> implied c (setFlag c flagD True)
  0xFB -> implied c $ do
    cf <- flag c flagC
    e <- emu c
    setFlag c flagC e
    s c iE (if cf then 1 else 0)
    when cf $ do
      setFlag c flagM True
      setFlag c flagX True
    normalise c
  0xC2 -> do
    v <- fetch c
    ioLast c
    p <- g c iP
    setP c (p .&. complement v)
  0xE2 -> do
    v <- fetch c
    ioLast c
    p <- g c iP
    setP c (p .|. v)
  0xEA -> implied c (pure ())
  -- WDM: its second byte is skipped with an internal cycle
  0x42 -> ioPC c >> g c iPC >>= s c iPC . (\pc -> (pc + 1) .&. 0xFFFF)
  -- stack
  0x48 -> ioPC c >> (m8 c >>= pushReg iA)
  0xDA -> ioPC c >> (x8 c >>= pushReg iX)
  0x5A -> ioPC c >> (x8 c >>= pushReg iY)
  0x08 -> ioPC c >> (g c iP >>= push c)
  0x8B -> ioPC c >> (g c iDBR >>= push c)
  0x4B -> ioPC c >> (g c iPBR >>= push c)
  0x0B -> do
    ioPC c
    d <- g c iD
    pushN c (d `shiftR` 8)
    pushN c (d .&. 0xFF)
  0x68 -> do
    eight <- m8 c
    v <- pullReg eight
    setAcc c eight v
    setNZ c eight v
  0xFA -> pullIdx iX
  0x7A -> pullIdx iY
  0x28 -> do
    ioPC c
    ioPC c
    pull c >>= setP c
  0xAB -> do
    ioPC c
    ioPC c
    b <- pullN c
    s c iDBR b
    setNZ c True b
  0x2B -> do
    ioPC c
    ioPC c
    lo <- pullN c
    hi <- pullN c
    let d = lo .|. hi `shiftL` 8
    s c iD d
    setNZ c False d
  0xF4 -> do
    v <- fetch16 c
    pushN c (v `shiftR` 8)
    pushN c (v .&. 0xFF)
  0xD4 -> do
    off <- fetch c
    dlPenalty c
    lo <- rd c KData =<< dpN c off
    hi <- rd c KData =<< dpN c (off + 1)
    pushN c hi
    pushN c lo
  0x62 -> do
    off <- fetch16 c
    ioLast c
    pc <- g c iPC
    let v = (pc + off) .&. 0xFFFF
    pushN c (v `shiftR` 8)
    pushN c (v .&. 0xFF)
  -- jumps and calls
  0x4C -> fetch16 c >>= s c iPC
  0x5C -> do
    aa <- fetch16 c
    b <- fetch c
    s c iPC aa
    s c iPBR b
  0x6C -> do
    aa <- fetch16 c
    lo <- rd c KData aa
    hi <- rd c KData ((aa + 1) .&. 0xFFFF)
    s c iPC (lo .|. hi `shiftL` 8)
  0x7C -> do
    aa <- fetch16 c
    ioLast c
    x <- g c iX
    b <- g c iPBR
    lo <- rd c KData (b `shiftL` 16 .|. ((aa + x) .&. 0xFFFF))
    hi <- rd c KData (b `shiftL` 16 .|. ((aa + x + 1) .&. 0xFFFF))
    s c iPC (lo .|. hi `shiftL` 8)
  0xDC -> do
    aa <- fetch16 c
    lo <- rd c KData aa
    hi <- rd c KData ((aa + 1) .&. 0xFFFF)
    b <- rd c KData ((aa + 2) .&. 0xFFFF)
    s c iPC (lo .|. hi `shiftL` 8)
    s c iPBR b
  0x20 -> do
    aa <- fetch16 c
    ioLast c
    pc <- g c iPC
    let ret = (pc - 1) .&. 0xFFFF
    push c (ret `shiftR` 8)
    push c (ret .&. 0xFF)
    s c iPC aa
  0xFC -> do
    lo <- fetch c
    pc <- g c iPC
    let ret = pc -- the address of the operand's high byte
        p = if cfgJsrIndXNew (cpuConfig c) then pushN c else push c
    p (ret `shiftR` 8)
    p (ret .&. 0xFF)
    hi <- fetch c
    ioLast c
    x <- g c iX
    b <- g c iPBR
    let aa = lo .|. hi `shiftL` 8
    plo <- rd c KData (b `shiftL` 16 .|. ((aa + x) .&. 0xFFFF))
    phi <- rd c KData (b `shiftL` 16 .|. ((aa + x + 1) .&. 0xFFFF))
    s c iPC (plo .|. phi `shiftL` 8)
  0x22 -> do
    aa <- fetch16 c
    g c iPBR >>= pushN c
    g c iS >>= io c . (\sp -> (sp + 1) .&. 0xFFFF)
    b <- fetch c
    pc <- g c iPC
    let ret = (pc - 1) .&. 0xFFFF
    pushN c (ret `shiftR` 8)
    pushN c (ret .&. 0xFF)
    s c iPC aa
    s c iPBR b
  0x60 -> do
    ioPC c
    ioPC c
    lo <- pull c
    hi <- pull c
    g c iS >>= io c
    s c iPC (((lo .|. hi `shiftL` 8) + 1) .&. 0xFFFF)
  0x6B -> do
    ioPC c
    ioPC c
    lo <- pullN c
    hi <- pullN c
    b <- pullN c
    s c iPC (((lo .|. hi `shiftL` 8) + 1) .&. 0xFFFF)
    s c iPBR b
  0x40 -> do
    ioPC c
    ioPC c
    e <- emu c
    p <- pull c
    setP c p
    lo <- pull c
    hi <- pull c
    s c iPC (lo .|. hi `shiftL` 8)
    unless e (pull c >>= s c iPBR)
  0x00 -> fetch c >> interrupt c 0xFFE6 0xFFFE True
  0x02 -> fetch c >> interrupt c 0xFFE4 0xFFF4 True
  -- branches
  0x10 -> not <$> flag c flagN >>= branch c
  0x30 -> flag c flagN >>= branch c
  0x50 -> not <$> flag c flagV >>= branch c
  0x70 -> flag c flagV >>= branch c
  0x90 -> not <$> flag c flagC >>= branch c
  0xB0 -> flag c flagC >>= branch c
  0xD0 -> not <$> flag c flagZ >>= branch c
  0xF0 -> flag c flagZ >>= branch c
  0x80 -> branch c True
  0x82 -> do
    off <- fetch16 c
    ioLast c
    g c iPC >>= s c iPC . (\pc -> (pc + off) .&. 0xFFFF)
  -- block moves
  0x54 -> blockMove 1
  0x44 -> blockMove (-1)
  -- WAI, STP
  0xCB -> ioPC c >> ioPC c >> s c iRun 1
  0xDB -> ioPC c >> ioPC c >> s c iRun 2
  _ -> error ("W65c816.Spec: opcode " <> show op <> " not decoded")
 where
  accOp sh = implied c $ do
    eight <- m8 c
    a <- getAcc c eight
    r <- shiftOp c sh eight a
    setAcc c eight r
  incIdx ri d = do
    eight <- x8 c
    v <- g c ri
    let r = (v + d) .&. (if eight then 0xFF else 0xFFFF)
    s c ri r
    setNZ c eight r
  -- A (C), X, Y, S to an index register: the destination's width
  xfer from to = do
    eight <- x8 c
    v <- g c from
    let r = if eight then v .&. 0xFF else v
    s c to r
    setNZ c eight r
  xferToA from = do
    eight <- m8 c
    v <- g c from
    setAcc c eight v
    setNZ c eight v
  pushReg ri eight = do
    v <- g c ri
    unless eight (push c (v `shiftR` 8))
    push c (v .&. 0xFF)
  pullReg eight = do
    ioPC c
    ioPC c
    lo <- pull c
    if eight then pure lo else (\hi -> lo .|. hi `shiftL` 8) <$> pull c
  pullIdx ri = do
    eight <- x8 c
    v <- pullReg eight
    s c ri v
    setNZ c eight v
  blockMove d = do
    dst <- fetch c
    src <- fetch c
    s c iDBR dst
    x <- g c iX
    y <- g c iY
    v <- rd c KData (src `shiftL` 16 .|. x)
    wr c (dst `shiftL` 16 .|. y) v
    io c (dst `shiftL` 16 .|. y)
    io c (dst `shiftL` 16 .|. y)
    eight <- x8 c
    let lim = if eight then 0xFF else 0xFFFF
    s c iX ((x + d) .&. lim)
    s c iY ((y + d) .&. lim)
    a <- g c iA
    let a' = (a - 1) .&. 0xFFFF
    s c iA a'
    when (a' /= 0xFFFF) $ g c iPC >>= s c iPC . (\pc -> (pc - 3) .&. 0xFFFF)
