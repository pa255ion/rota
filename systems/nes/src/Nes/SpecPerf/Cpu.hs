{-# OPTIONS_GHC -fworker-wrapper -fspecialise-aggressively -funfolding-use-threshold=200 -funfolding-creation-threshold=1000 #-}
-- The NES's performance version (side track Q for side track W, the
-- user 2026-11-06: "an alternative, performance version"): begun as a copy
-- of Mos6502.SpecFast (cpus/mos6502, shared with the 800XL: this copy is the NES's own), which stays as it is; changed here for speed, above all in
-- the browser's build (GHC's WebAssembly backend, where every call between
-- Haskell functions goes back through the runtime's loop). The same
-- machine: nes-specperf runs it beside the fast version on every input.
-- (the fast version is never synthesised: GHC's unboxing back on, which
-- clash-common's -fno-worker-wrapper turns off for Clash's sake)
-- Side track Q (PLAN.md Q.7): the desktop emulators' fast copy of
-- Mos6502.Spec, kept beside it (the original stays the reference the
-- cores' lockstep and the NES and 800XL specs use; a fix in either goes to
-- both). Shared by the NES's and the 800XL's fast versions (the user,
-- 2026-10-21: in cpus/mos6502, beside the original). The difference: the
-- bus is a class ('CpuBus'), not a record of functions, so the CPU
-- specialises to the machine's and its accesses inline. nes-specfast and
-- a800xl-specfast run both machines side by side.
{-# LANGUAGE MultiWayIf #-}
{-|
The MOS 6502 reference model (PLAN.md step 3.1; docs/systems/nes.md,
"Shared 6502 package"): one bus access per cycle, in the order of the NMOS
6502, dummy accesses included, so a machine around it sees every read and
write the chip makes (reading $2002 or $4016 on the NES has side effects).

Sources for the per-cycle behaviour: the nesdev wiki ("CPU", "CPU
interrupts", "CPU unofficial opcodes"), "6502_cpu.txt" (John West and
Marko Mäkelä) for the cycle tables, Bruce Clark's decimal mode tutorial
(6502.org, appendix A) for the NMOS decimal flags, and "NMOS 6510
Unintended Opcodes" for ARR in decimal mode. Checked against Tom Harte's
SingleStepTests (all 256 opcodes, final state and per-cycle bus trace).

__The bus__ ('Bus') belongs to the caller: the model knows nothing about
the machine. Each cycle is exactly one 'busRead' or 'busWrite'; after it,
the model samples the NMI and IRQ lines ('busNmi', 'busIrq'), as the
chip does in the second half of every cycle.

__RDY__ ('busRdy') is sampled before each read cycle; while it is low the
CPU repeats that read (one cycle each) and goes on once it is high; those
cycles move the NMI edge detector on but not the interrupt polls
('stallCycle'). Write
cycles ignore it, as on the NMOS 6502. (The NES's DMA unit doesn't use this:
it takes the bus itself, see @Nes.Spec@.)

__Interrupts__ (nesdev wiki, "CPU interrupts"): the NMI input is edge
triggered, the IRQ input level triggered and masked by I. At the end of
every cycle the model records whether an interrupt would be taken; at the
end of an instruction it uses the record of the instruction's
second-to-last cycle, so a CLI, SEI or PLP takes effect one instruction
late and RTI immediately. A taken branch without a page crossing doesn't
see an IRQ that arrived in its second cycle. The interrupt sequence
chooses its vector after pushing PCL: an NMI detected by then hijacks an
IRQ or BRK (the B flag of a hijacked BRK is still pushed set).

__One 'step'__ is an instruction or an interrupt sequence (7 cycles). An
instruction whose second-to-last cycle saw an interrupt is followed by
that sequence in the next step; the first instruction of the handler always
runs before the next one.

__JAM__ (the 12 opcodes @x2@ with x in 0 to 7, 9, B, D, F): after the
opcode and the byte after it, the bus shows $FFFF, $FFFE, $FFFE and then
$FFFF on every cycle; the CPU is stuck until 'reset'. Each later 'step'
is one such cycle.

The unstable opcodes behave as in SingleStepTests: ANE (8B) uses the
constant $EE, LXA (AB) the one in 'Config' ($EE there, $FF on the
2A03); SHA, SHX, SHY and TAS store the register ANDed with the base
address's high byte plus one (not if RDY held the dummy read before the
write: then the register itself), and on a page crossing that value also
replaces the high byte of the address.
-}
module Nes.SpecPerf.Cpu
  ( -- * Configuration
    Config (..)
  , nmos6502
  , sally6502
  , ricoh2a03
  , singleStep2a03
    -- * The bus
  , CpuBus (..)
    -- * Registers
  , Regs (..)
  , flagC, flagZ, flagI, flagD, flagB, flagU, flagV, flagN
    -- * The CPU
  , Cpu
  , cpuStateVector
  , newCpu
  , newCpuState
  , cpuBus
  , getRegs
  , setRegs
  , cpuCycles
  , cpuJammed
  , cpuPending
  , Step (..)
  , step
  , stepCode
  , reset
  ) where

import Prelude

import Control.Monad (unless, when)
import Control.Monad.ST (ST)
import Data.Bits
import Data.Int (Int8)
import qualified Data.Vector.Unboxed.Mutable as MV
import Data.Word

-- | What differs between members of the NMOS family.
data Config = Config
  { cfgDecimal  :: !Bool  -- ^ the D flag selects BCD arithmetic in ADC, SBC (and ARR, RRA, ISC)
  , cfgLxaConst :: !Word8 -- ^ the constant LXA ($AB) ORs into A: $EE in SingleStepTests, $FF on the 2A03
  , cfgStallPolls :: !Bool
    -- ^ RDY-stalled cycles poll interrupts like the CPU's own (the 800XL's
    -- WSYNC, Acid800 antic_dlitiming; off: the 2A03's DMA rule)
  , cfgPushBlocksNmi :: !Bool
    -- ^ an NMI edge seen on the cycle an IRQ/BRK sequence pushes P is lost
    -- (the 800XL, Acid800 antic_blockednmi; off: it waits, as on the NES)
  } deriving stock (Show, Eq)

-- | The MOS 6502 (Atari 2600's 6507, 800XL's SALLY, C64's 6510 core).
nmos6502 :: Config
nmos6502 = Config { cfgDecimal = True, cfgLxaConst = 0xEE, cfgStallPolls = False, cfgPushBlocksNmi = False }

-- | The 800XL's 6502C SALLY: 'nmos6502' with the two interrupt rules
-- measured on the 800XL (Altirra Hardware Reference Manual 4.8 "Missed
-- NMIs", Acid800 antic_blockednmi and antic_dlitiming).
sally6502 :: Config
sally6502 = nmos6502 { cfgStallPolls = True, cfgPushBlocksNmi = True }

-- | The NES CPU: decimal mode removed, the D flag still stored. LXA uses
-- $FF: Blargg's @instr_test-v5@ checks it as a stable opcode on the
-- NES (03-immediate, @AB ATX #n@); see also 'singleStep2a03'.
ricoh2a03 :: Config
ricoh2a03 = Config { cfgDecimal = False, cfgLxaConst = 0xFF, cfgStallPolls = False, cfgPushBlocksNmi = False }

-- | The 2A03 as SingleStepTests' @nes6502@ set generated it: LXA with
-- $EE. Only for that test set.
singleStep2a03 :: Config
singleStep2a03 = ricoh2a03 { cfgLxaConst = 0xEE }

-- | The machine around the CPU. See the module header for when each is
-- called.
class CpuBus b where
  busRead  :: b s -> Word16 -> ST s Word8
  busWrite :: b s -> Word16 -> Word8 -> ST s ()
  busRdy   :: b s -> ST s Bool -- ^ 'False' stalls a read cycle
  busNmi   :: b s -> ST s Bool -- ^ the NMI line, 'True' = asserted (low on the pin)
  busIrq   :: b s -> ST s Bool -- ^ the IRQ line, 'True' = asserted

-- | The programmer-visible registers. B and the unused bit 5 of P are as
-- last set; PHP and BRK push them set, PLP and RTI clear B and set bit 5.
data Regs = Regs
  { rA, rX, rY, rS, rP :: !Word8
  , rPC :: !Word16
  } deriving stock (Show, Eq)

flagC, flagZ, flagI, flagD, flagB, flagU, flagV, flagN :: Word8
flagC = 0x01
flagZ = 0x02
flagI = 0x04
flagD = 0x08
flagB = 0x10
flagU = 0x20
flagV = 0x40
flagN = 0x80

-- | The CPU in machine @b@ (its bus, a class: the CPU's code specialises
-- to the machine's, side track Q). The bus field is lazy, so a machine
-- holding its CPU can be the CPU's bus ('newCpu').
data Cpu b s = Cpu
  { cpuConfig :: !Config
  , cpuBus    :: b s
  , cpuState  :: !(MV.MVector s Int)
  }

-- State slots.
iA, iX, iY, iS, iP, iPC, iCyc, iNmiLine, iNmiNeed, iPollCur, iPollPrev, iPending, iJam :: Int
iA = 0
iX = 1
iY = 2
iS = 3
iP = 4
iPC = 5
iCyc = 6
iNmiLine = 7   -- NMI line at the end of the last cycle (edge detector)
iNmiNeed = 8   -- NMI edge seen, not yet serviced
iPollCur = 9   -- at the end of the last cycle: bit 0 NMI, bit 1 IRQ would be taken
iPollPrev = 10 -- the same, one cycle earlier
iPending = 11  -- 'iPollPrev' at the end of the last instruction
iJam = 12      -- 0 running; 1 to 4 the JAM bus sequence; 5 stuck at $FFFF

slots :: Int
slots = 13

-- | A CPU whose bus is @bus@, from its state ('newCpuState'): pure, so a
-- machine can hold the CPU whose bus it is (a recursive let).
newCpu :: Config -> b s -> MV.MVector s Int -> Cpu b s
newCpu = Cpu

-- | The CPU's state, the registers as given.
newCpuState :: Regs -> ST s (MV.MVector s Int)
newCpuState (Regs a x y s p pc) = do
  st <- MV.replicate slots 0
  mapM_ (\(i, v) -> MV.write st i v) [(iA, fromIntegral a), (iX, fromIntegral x), (iY, fromIntegral y), (iS, fromIntegral s), (iP, fromIntegral p), (iPC, fromIntegral pc)]
  pure st

getRegs :: CpuBus b => Cpu b s -> ST s Regs
getRegs c = do
  a <- r8 c iA
  x <- r8 c iX
  y <- r8 c iY
  s <- r8 c iS
  p <- r8 c iP
  pc <- getPC c
  pure (Regs a x y s p pc)

setRegs :: CpuBus b => Cpu b s -> Regs -> ST s ()
setRegs c (Regs a x y s p pc) = do
  w8 c iA a
  w8 c iX x
  w8 c iY y
  w8 c iS s
  w8 c iP p
  setPC c pc

-- | Cycles run since 'newCpu'.
cpuCycles :: CpuBus b => Cpu b s -> ST s Int
cpuCycles c = MV.unsafeRead (cpuState c) iCyc

cpuJammed :: CpuBus b => Cpu b s -> ST s Bool
cpuJammed c = (/= 0) <$> MV.unsafeRead (cpuState c) iJam

-- | Between two steps: an interrupt about to be taken (the poll of the
-- last instruction's second-to-last cycle) or an NMI edge not yet taken
-- (the performance version's machine asks this instead of keeping a copy
-- of the polls every cycle).
cpuPending :: CpuBus b => Cpu b s -> ST s Bool
cpuPending c = (\p n -> p /= 0 || n /= 0) <$> MV.unsafeRead (cpuState c) iPollPrev <*> MV.unsafeRead (cpuState c) iNmiNeed

-- | The CPU's whole state (registers, the cycle count, the interrupt
-- polls): the desktop's save states (Q.8).
cpuStateVector :: Cpu b s -> MV.MVector s Int
cpuStateVector = cpuState

-- | What a 'step' did.
data Step
  = Instr !Word8 -- ^ executed this opcode
  | Nmi          -- ^ the interrupt sequence, through the NMI vector
  | Irq          -- ^ the interrupt sequence, through the IRQ vector
  | Jammed       -- ^ one cycle of a JAMmed CPU
  deriving stock (Show, Eq)

-- * State access

r8 :: CpuBus b => Cpu b s -> Int -> ST s Word8
r8 c i = fromIntegral <$> MV.unsafeRead (cpuState c) i
{-# INLINE r8 #-}

w8 :: CpuBus b => Cpu b s -> Int -> Word8 -> ST s ()
w8 c i v = MV.unsafeWrite (cpuState c) i (fromIntegral v)
{-# INLINE w8 #-}

getI :: CpuBus b => Cpu b s -> Int -> ST s Int
getI c = MV.unsafeRead (cpuState c)
{-# INLINE getI #-}

setI :: CpuBus b => Cpu b s -> Int -> Int -> ST s ()
setI c = MV.unsafeWrite (cpuState c)
{-# INLINE setI #-}

getPC :: CpuBus b => Cpu b s -> ST s Word16
getPC c = fromIntegral <$> MV.unsafeRead (cpuState c) iPC
{-# INLINE getPC #-}

setPC :: CpuBus b => Cpu b s -> Word16 -> ST s ()
setPC c v = MV.unsafeWrite (cpuState c) iPC (fromIntegral v)
{-# INLINE setPC #-}

getFlag :: CpuBus b => Cpu b s -> Word8 -> ST s Bool
getFlag c f = (\p -> p .&. f /= 0) <$> r8 c iP
{-# INLINE getFlag #-}

setFlag :: CpuBus b => Cpu b s -> Word8 -> Bool -> ST s ()
setFlag c f b = do
  p <- r8 c iP
  w8 c iP (if b then p .|. f else p .&. complement f)
{-# INLINE setFlag #-}

setNZ :: CpuBus b => Cpu b s -> Word8 -> ST s ()
setNZ c v = do
  p <- r8 c iP
  w8 c iP ((p .&. 0x7D) .|. (v .&. 0x80) .|. (if v == 0 then flagZ else 0))
{-# INLINE setNZ #-}

-- * Cycles

-- | The end of every cycle: sample NMI and IRQ (nesdev wiki, "CPU
-- interrupts", "Detailed interrupt behavior").
endCycle :: CpuBus b => Cpu b s -> ST s ()
endCycle c = do
  let bus = cpuBus c
  nmi <- busNmi bus
  prevLine <- getI c iNmiLine
  when (nmi && prevLine == 0) (setI c iNmiNeed 1)
  setI c iNmiLine (if nmi then 1 else 0)
  irq <- busIrq bus
  i <- getFlag c flagI
  need <- getI c iNmiNeed
  setI c iPollPrev =<< getI c iPollCur
  setI c iPollCur (need .|. (if irq && not i then 2 else 0))
  n <- getI c iCyc
  setI c iCyc (n + 1)

-- | The end of a cycle the CPU spent halted (RDY low): the NMI edge
-- detector runs, but the interrupt poll doesn't (unless 'cfgStallPolls'):
-- the instruction's own
-- cycles decide when it sees an IRQ (AccuracyCoin, "Interrupt flag
-- latency" 10: a DMC IRQ raised by the DMA inside a taken branch shows
-- only after the next instruction).
stallCycle :: CpuBus b => Cpu b s -> ST s ()
stallCycle c
  | cfgStallPolls (cpuConfig c) = endCycle c
  | otherwise = do
      nmi <- busNmi (cpuBus c)
      prevLine <- getI c iNmiLine
      when (nmi && prevLine == 0) (setI c iNmiNeed 1)
      setI c iNmiLine (if nmi then 1 else 0)
      n <- getI c iCyc
      setI c iCyc (n + 1)

-- | A read cycle (as 'rdStalled', without the pair: no pair or thunk
-- built on every read, which the browser pays for).
rd :: CpuBus b => Cpu b s -> Word16 -> ST s Word8
rd c !a = do
  stallRd c a
  v <- busRead (cpuBus c) a
  endCycle c
  pure $! v

-- | RDY held: the read repeats, a halted cycle each time, until it lets go.
stallRd :: CpuBus b => Cpu b s -> Word16 -> ST s ()
stallRd c !a = do
  ok <- busRdy (cpuBus c)
  unless ok $ do
    _ <- busRead (cpuBus c) a
    stallCycle c
    stallRd c a

-- | A read cycle; also whether RDY held it (one or more repeats first).
rdStalled :: CpuBus b => Cpu b s -> Word16 -> ST s (Bool, Word8)
rdStalled c !a = do
  s <- stall False
  v <- busRead (cpuBus c) a
  endCycle c
  pure (s, v)
 where
  stall held = do
    ok <- busRdy (cpuBus c)
    if ok then pure held else do
      _ <- busRead (cpuBus c) a
      stallCycle c
      stall True

wr :: CpuBus b => Cpu b s -> Word16 -> Word8 -> ST s ()
wr c a v = do
  busWrite (cpuBus c) a v
  endCycle c

-- | Read at PC and advance it.
fetch :: CpuBus b => Cpu b s -> ST s Word8
fetch c = do
  pc <- getPC c
  setPC c (pc + 1)
  rd c pc

-- | Read at PC without advancing it.
dummyPC :: CpuBus b => Cpu b s -> ST s ()
dummyPC c = getPC c >>= rd c >> pure ()

push :: CpuBus b => Cpu b s -> Word8 -> ST s ()
push c v = do
  s <- r8 c iS
  wr c (0x100 .|. fromIntegral s) v
  w8 c iS (s - 1)

pull :: CpuBus b => Cpu b s -> ST s Word8
pull c = do
  s <- r8 c iS
  w8 c iS (s + 1)
  rd c (0x100 .|. fromIntegral (s + 1))

-- | The read a pull makes before the stack pointer moves.
dummyStack :: CpuBus b => Cpu b s -> ST s ()
dummyStack c = do
  s <- r8 c iS
  _ <- rd c (0x100 .|. fromIntegral s)
  pure ()

w16 :: Word8 -> Word8 -> Word16
w16 hi lo = fromIntegral hi `shiftL` 8 .|. fromIntegral lo
{-# INLINE w16 #-}

hiB, loB :: Word16 -> Word8
hiB x = fromIntegral (x `shiftR` 8)
loB = fromIntegral
{-# INLINE hiB #-}
{-# INLINE loB #-}

-- * Stepping

step :: CpuBus b => Cpu b s -> ST s Step
step c = toStep <$> stepCode c
{-# INLINE step #-}

-- | 'step' as a number (no 'Step' built on every instruction, which the
-- browser pays for: the machine's own loop drops it): the opcode, or
-- 'codeNmi', 'codeIrq', 'codeJammed'.
stepCode :: CpuBus b => Cpu b s -> ST s Int
stepCode c = do
  jam <- getI c iJam
  if jam /= 0
    then jamCycle c jam >> pure codeJammed
    else do
      pending <- getI c iPending
      if pending /= 0
        then do
          setI c iPending 0
          interrupt c
        else do
          op <- fetch c
          execute c op
          jam' <- getI c iJam
          when (jam' == 0) (setI c iPending =<< getI c iPollPrev)
          pure $! fromIntegral op

-- | The JAM bus sequence after opcode and operand byte.
jamCycle :: CpuBus b => Cpu b s -> Int -> ST s ()
jamCycle c n = do
  _ <- busRead (cpuBus c) (if n == 2 || n == 3 then 0xFFFE else 0xFFFF)
  endCycle c
  setI c iJam (min 5 (n + 1))

-- | IRQ or NMI: opcode fetch and operand read without advancing PC, push
-- PC and P (B clear), vector.
codeNmi, codeIrq, codeJammed :: Int
codeNmi = 256
codeIrq = 257
codeJammed = 258

toStep :: Int -> Step
toStep n
  | n < 256 = Instr (fromIntegral n)
  | n == codeNmi = Nmi
  | n == codeIrq = Irq
  | otherwise = Jammed

interrupt :: CpuBus b => Cpu b s -> ST s Int
interrupt c = do
  dummyPC c
  dummyPC c
  pc <- getPC c
  push c (hiB pc)
  push c (loB pc)
  p <- r8 c iP
  vectorTo c ((p .&. complement flagB) .|. flagU)

-- | Push P, pick the vector (NMI if one is waiting), set I, read it.
vectorTo :: CpuBus b => Cpu b s -> Word8 -> ST s Int
vectorTo c pushedP = do
  need <- getI c iNmiNeed
  let nmi = need /= 0
  when nmi (setI c iNmiNeed 0)
  push c pushedP
  -- an NMI edge on this very cycle is lost on the 800XL
  when (cfgPushBlocksNmi (cpuConfig c)) $ do
    setI c iNmiNeed 0
    setI c iPollCur =<< ((.&. 2) <$> getI c iPollCur)
  setFlag c flagI True
  let v = if nmi then 0xFFFA else 0xFFFE
  lo <- rd c v
  hi <- rd c (v + 1)
  setPC c (w16 hi lo)
  pure $! if nmi then codeNmi else codeIrq

-- | The reset sequence: 7 cycles, the three stack cycles are reads, S
-- drops by 3, I set, PC from $FFFC. Interrupt state is cleared, a JAM
-- ends.
reset :: CpuBus b => Cpu b s -> ST s ()
reset c = do
  setI c iJam 0
  setI c iPending 0
  setI c iNmiNeed 0
  setI c iPollCur 0
  setI c iPollPrev 0
  dummyPC c
  dummyPC c
  let fake = do
        s <- r8 c iS
        _ <- rd c (0x100 .|. fromIntegral s)
        w8 c iS (s - 1)
  fake >> fake >> fake
  setFlag c flagI True
  lo <- rd c 0xFFFC
  hi <- rd c 0xFFFD
  setPC c (w16 hi lo)

-- * Addressing

data Kind = KRead | KWrite | KRmw
  deriving stock (Eq)

data Mode
  = Imm | Zp | ZpX | ZpY | Abs | AbsX | AbsY | IndX | IndY
  deriving stock (Eq, Show)

-- | The cycles that compute the effective address. For the indexed modes
-- across a page, a read only takes the extra (wrong address) cycle when
-- the page is crossed; writes and read-modify-writes always take it.
addr :: CpuBus b => Cpu b s -> Kind -> Mode -> ST s Word16
addr c k m = case m of
  Imm -> do
    pc <- getPC c
    setPC c (pc + 1)
    pure pc
  Zp -> fetch c >>= \b -> pure $! fromIntegral b
  ZpX -> zpIdx iX
  ZpY -> zpIdx iY
  Abs -> do
    lo <- fetch c
    hi <- fetch c
    pure $! w16 hi lo
  AbsX -> do
    lo <- fetch c
    hi <- fetch c
    idx (w16 hi lo) =<< r8 c iX
  AbsY -> do
    lo <- fetch c
    hi <- fetch c
    idx (w16 hi lo) =<< r8 c iY
  IndX -> do
    p <- fetch c
    _ <- rd c (fromIntegral p)
    x <- r8 c iX
    lo <- rd c (fromIntegral (p + x))
    hi <- rd c (fromIntegral (p + x + 1))
    pure $! w16 hi lo
  IndY -> do
    p <- fetch c
    lo <- rd c (fromIntegral p)
    hi <- rd c (fromIntegral (p + 1))
    idx (w16 hi lo) =<< r8 c iY
 where
  zpIdx r = do
    b <- fetch c
    _ <- rd c (fromIntegral b)
    i <- r8 c r
    pure $! fromIntegral (b + i)
  idx !base !i = do
    let ea = base + fromIntegral i
        wrong = (base .&. 0xFF00) .|. (ea .&. 0x00FF)
    when (k /= KRead || wrong /= ea) (rd c wrong >> pure ())
    pure $! ea

readOp :: CpuBus b => Cpu b s -> Mode -> ST s Word8
readOp c m = addr c KRead m >>= rd c

writeOp :: CpuBus b => Cpu b s -> Mode -> Word8 -> ST s ()
writeOp c m v = do
  ea <- addr c KWrite m
  wr c ea v

-- | Read, write the old value back, write the new one.
rmw :: CpuBus b => Cpu b s -> Mode -> (Word8 -> ST s Word8) -> ST s ()
rmw c m f = do
  ea <- addr c KRmw m
  v <- rd c ea
  wr c ea v
  v' <- f v
  wr c ea v'

-- * ALU

ora, andA, eor, adc, sbc, cmpA, bitOp :: CpuBus b => Cpu b s -> Word8 -> ST s ()
ora c v = r8 c iA >>= \a -> setA c (a .|. v)
andA c v = r8 c iA >>= \a -> setA c (a .&. v)
eor c v = r8 c iA >>= \a -> setA c (a `xor` v)
cmpA c v = r8 c iA >>= \a -> compareWith c a v
bitOp c v = do
  a <- r8 c iA
  p <- r8 c iP
  w8 c iP ((p .&. 0x3D) .|. (v .&. 0xC0) .|. (if a .&. v == 0 then flagZ else 0))

setA :: CpuBus b => Cpu b s -> Word8 -> ST s ()
setA c v = w8 c iA v >> setNZ c v

compareWith :: CpuBus b => Cpu b s -> Word8 -> Word8 -> ST s ()
compareWith c r v = do
  setNZ c (r - v)
  setFlag c flagC (r >= v)

adc c v = do
  a <- r8 c iA
  cin <- getFlag c flagC
  dec <- getFlag c flagD
  let ci = if cin then 1 else 0 :: Int
      ai = fromIntegral a :: Int
      vi = fromIntegral v :: Int
      bin = ai + vi + ci
      binR = fromIntegral bin :: Word8
  if cfgDecimal (cpuConfig c) && dec
    then do
      -- Bruce Clark, appendix A, sequences 1 and 2 (NMOS: Z from the
      -- binary sum, N and V from the intermediate result).
      let al0 = (ai .&. 0x0F) + (vi .&. 0x0F) + ci
          al = if al0 >= 0x0A then ((al0 + 0x06) .&. 0x0F) + 0x10 else al0
          r1 = (ai .&. 0xF0) + (vi .&. 0xF0) + al
          sa = sx (ai .&. 0xF0) + sx (vi .&. 0xF0) + al
          r2 = if r1 >= 0xA0 then r1 + 0x60 else r1
      w8 c iA (fromIntegral r2)
      p <- r8 c iP
      w8 c iP ((p .&. 0x3C)
               .|. (if r2 >= 0x100 then flagC else 0)
               .|. (if binR == 0 then flagZ else 0)
               .|. (if r1 .&. 0x80 /= 0 then flagN else 0)
               .|. (if sa < -128 || sa > 127 then flagV else 0))
    else binAdc c a v ci
 where
  sx x = if x >= 0x80 then x - 0x100 else x

binAdc :: CpuBus b => Cpu b s -> Word8 -> Word8 -> Int -> ST s ()
binAdc c a v ci = do
  let s = fromIntegral a + fromIntegral v + ci :: Int
      r = fromIntegral s :: Word8
      ov = (a `xor` r) .&. (v `xor` r) .&. 0x80 /= 0
  setA c r
  setFlag c flagC (s > 0xFF)
  setFlag c flagV ov

sbc c v = do
  a <- r8 c iA
  cin <- getFlag c flagC
  dec <- getFlag c flagD
  let ci = if cin then 1 else 0 :: Int
  -- Flags are always the binary ones (NMOS).
  binAdc c a (complement v) ci
  when (cfgDecimal (cpuConfig c) && dec) $ do
    -- Bruce Clark, appendix A, sequence 3.
    let ai = fromIntegral a :: Int
        vi = fromIntegral v :: Int
        al0 = (ai .&. 0x0F) - (vi .&. 0x0F) + ci - 1
        al = if al0 < 0 then ((al0 - 0x06) .&. 0x0F) - 0x10 else al0
        r1 = (ai .&. 0xF0) - (vi .&. 0xF0) + al
        r2 = if r1 < 0 then r1 - 0x60 else r1
    w8 c iA (fromIntegral r2)

asl, lsr, rol, ror :: CpuBus b => Cpu b s -> Word8 -> ST s Word8
asl c v = do
  setFlag c flagC (testBit v 7)
  let r = v `shiftL` 1
  setNZ c r >> pure r
lsr c v = do
  setFlag c flagC (testBit v 0)
  let r = v `shiftR` 1
  setNZ c r >> pure r
rol c v = do
  ci <- getFlag c flagC
  setFlag c flagC (testBit v 7)
  let r = (v `shiftL` 1) .|. (if ci then 1 else 0)
  setNZ c r >> pure r
ror c v = do
  ci <- getFlag c flagC
  setFlag c flagC (testBit v 0)
  let r = (v `shiftR` 1) .|. (if ci then 0x80 else 0)
  setNZ c r >> pure r

incM, decM :: CpuBus b => Cpu b s -> Word8 -> ST s Word8
incM c v = let r = v + 1 in setNZ c r >> pure r
decM c v = let r = v - 1 in setNZ c r >> pure r

onA :: CpuBus b => Cpu b s -> (Cpu b s -> Word8 -> ST s Word8) -> ST s ()
onA c f = do
  dummyPC c
  a <- r8 c iA
  r <- f c a
  w8 c iA r

-- * Instructions

-- | The ALU-group addressing modes (bbb of aaabbbcc, cc = 01 and 11).
mode01 :: Word8 -> Mode
mode01 op = case (op `shiftR` 2) .&. 7 of
  0 -> IndX
  1 -> Zp
  2 -> Imm
  3 -> Abs
  4 -> IndY
  5 -> ZpX
  6 -> AbsY
  _ -> AbsX

-- | cc = 10 modes for bbb = 1, 3, 5, 7 (LDX/STX use Y for indexing).
mode10 :: Word8 -> Mode
mode10 op = case (op `shiftR` 2) .&. 7 of
  1 -> Zp
  3 -> Abs
  5 -> if xy then ZpY else ZpX
  _ -> if xy then AbsY else AbsX
 where
  xy = op .&. 0xE0 == 0x80 || op .&. 0xE0 == 0xA0

implied :: CpuBus b => Cpu b s -> ST s () -> ST s ()
implied c act = dummyPC c >> act

execute :: CpuBus b => Cpu b s -> Word8 -> ST s ()
execute c op = case op .&. 3 of
  1 -> alu
  3 -> undoc
  2 -> group10
  _ -> group00
 where
  aaa = op `shiftR` 5
  alu = case aaa of
    0 -> readOp c m >>= ora c
    1 -> readOp c m >>= andA c
    2 -> readOp c m >>= eor c
    3 -> readOp c m >>= adc c
    4 -> if m == Imm then readOp c m >> pure () else r8 c iA >>= writeOp c m
    5 -> readOp c m >>= setA c
    6 -> readOp c m >>= cmpA c
    _ -> readOp c m >>= sbc c
   where m = mode01 op

  group10 = case (op `shiftR` 2) .&. 7 of
    0 -> if op == 0xA2 then fetch c >>= \v -> w8 c iX v >> setNZ c v
         else if aaa >= 4 then fetch c >> pure ()   -- 82 C2 E2: NOP #imm
         else jam
    4 -> jam
    2 -> case aaa of
      0 -> onA c asl
      1 -> onA c rol
      2 -> onA c lsr
      3 -> onA c ror
      4 -> implied c (r8 c iX >>= setA c)                         -- TXA
      5 -> implied c (r8 c iA >>= \v -> w8 c iX v >> setNZ c v)   -- TAX
      6 -> implied c (r8 c iX >>= \v -> let r = v - 1 in w8 c iX r >> setNZ c r) -- DEX
      _ -> dummyPC c                                              -- NOP
    6 -> case aaa of
      4 -> implied c (r8 c iX >>= w8 c iS)                        -- TXS
      5 -> implied c (r8 c iS >>= \v -> w8 c iX v >> setNZ c v)   -- TSX
      _ -> dummyPC c                                              -- NOPs
    _ -> case aaa of
      0 -> rmw c m (asl c)
      1 -> rmw c m (rol c)
      2 -> rmw c m (lsr c)
      3 -> rmw c m (ror c)
      4 | op == 0x9E -> shStore c iX iY                             -- SHX abs,Y
        | otherwise  -> r8 c iX >>= writeOp c m                   -- STX
      5 -> readOp c m >>= \v -> w8 c iX v >> setNZ c v            -- LDX
      6 -> rmw c m (decM c)
      _ -> rmw c m (incM c)
   where m = mode10 op

  group00 = case op of
    0x00 -> do                                                    -- BRK
      _ <- fetch c
      pc <- getPC c
      push c (hiB pc)
      push c (loB pc)
      p <- r8 c iP
      _ <- vectorTo c (p .|. flagB .|. flagU)
      -- The handler's first instruction runs before an NMI that is still
      -- waiting (nesdev wiki; Blargg cpu_interrupts_v2 2-nmi_and_brk).
      pc' <- getI c iPollPrev
      setI c iPollPrev (pc' .&. complement 1)
    0x20 -> do                                                    -- JSR
      lo <- fetch c
      dummyStack c
      pc <- getPC c
      push c (hiB pc)
      push c (loB pc)
      hi <- fetch c
      setPC c (w16 hi lo)
    0x40 -> do                                                    -- RTI
      dummyPC c
      dummyStack c
      p <- pull c
      w8 c iP ((p .&. complement flagB) .|. flagU)
      lo <- pull c
      hi <- pull c
      setPC c (w16 hi lo)
    0x60 -> do                                                    -- RTS
      dummyPC c
      dummyStack c
      lo <- pull c
      hi <- pull c
      setPC c (w16 hi lo)
      _ <- fetch c
      pure ()
    0x08 -> dummyPC c >> r8 c iP >>= \p -> push c (p .|. flagB .|. flagU)  -- PHP
    0x28 -> do                                                    -- PLP
      dummyPC c
      dummyStack c
      p <- pull c
      w8 c iP ((p .&. complement flagB) .|. flagU)
    0x48 -> dummyPC c >> r8 c iA >>= push c                       -- PHA
    0x68 -> dummyPC c >> dummyStack c >> pull c >>= setA c        -- PLA
    0x4C -> do                                                    -- JMP abs
      lo <- fetch c
      hi <- fetch c
      setPC c (w16 hi lo)
    0x6C -> do                                                    -- JMP (ind), page wrap
      lo <- fetch c
      hi <- fetch c
      tl <- rd c (w16 hi lo)
      th <- rd c (w16 hi (lo + 1))
      setPC c (w16 th tl)
    0x24 -> readOp c Zp >>= bitOp c
    0x2C -> readOp c Abs >>= bitOp c
    0x84 -> r8 c iY >>= writeOp c Zp
    0x8C -> r8 c iY >>= writeOp c Abs
    0x94 -> r8 c iY >>= writeOp c ZpX
    0x9C -> shStore c iY iX                                         -- SHY abs,X
    0xA0 -> fetch c >>= ldy
    0xA4 -> readOp c Zp >>= ldy
    0xAC -> readOp c Abs >>= ldy
    0xB4 -> readOp c ZpX >>= ldy
    0xBC -> readOp c AbsX >>= ldy
    0xC0 -> fetch c >>= \v -> r8 c iY >>= \y -> compareWith c y v
    0xC4 -> readOp c Zp >>= \v -> r8 c iY >>= \y -> compareWith c y v
    0xCC -> readOp c Abs >>= \v -> r8 c iY >>= \y -> compareWith c y v
    0xE0 -> fetch c >>= \v -> r8 c iX >>= \x -> compareWith c x v
    0xE4 -> readOp c Zp >>= \v -> r8 c iX >>= \x -> compareWith c x v
    0xEC -> readOp c Abs >>= \v -> r8 c iX >>= \x -> compareWith c x v
    0x88 -> implied c (r8 c iY >>= \v -> let r = v - 1 in w8 c iY r >> setNZ c r) -- DEY
    0xC8 -> implied c (r8 c iY >>= \v -> let r = v + 1 in w8 c iY r >> setNZ c r) -- INY
    0xE8 -> implied c (r8 c iX >>= \v -> let r = v + 1 in w8 c iX r >> setNZ c r) -- INX
    0x98 -> implied c (r8 c iY >>= setA c)                        -- TYA
    0xA8 -> implied c (r8 c iA >>= \v -> w8 c iY v >> setNZ c v)  -- TAY
    0x18 -> implied c (setFlag c flagC False)
    0x38 -> implied c (setFlag c flagC True)
    0x58 -> implied c (setFlag c flagI False)
    0x78 -> implied c (setFlag c flagI True)
    0xB8 -> implied c (setFlag c flagV False)
    0xD8 -> implied c (setFlag c flagD False)
    0xF8 -> implied c (setFlag c flagD True)
    _ | op .&. 0x1F == 0x10 -> do                                 -- branches
          let cond = case op `shiftR` 6 of
                0 -> flagN
                1 -> flagV
                2 -> flagC
                _ -> flagZ
          f <- getFlag c cond
          branch (f == testBit op 5)
      | otherwise -> nopOp                                        -- unofficial NOPs
   where
    ldy v = w8 c iY v >> setNZ c v
    nopOp = case (op `shiftR` 2) .&. 7 of
      0 -> fetch c >> pure ()        -- 80: NOP #imm
      1 -> readOp c Zp >> pure ()    -- 04 44 64
      3 -> readOp c Abs >> pure ()   -- 0C
      5 -> readOp c ZpX >> pure ()   -- 14 34 54 74 D4 F4
      _ -> readOp c AbsX >> pure ()  -- 1C 3C 5C 7C DC FC

  branch taken = do
    off <- fetch c
    when taken $ do
      -- An IRQ first seen in the operand cycle waits one more instruction.
      cur <- getI c iPollCur
      prev <- getI c iPollPrev
      when (cur .&. 2 /= 0 && prev .&. 2 == 0) (setI c iPollCur (cur .&. complement 2))
      pc <- getPC c
      _ <- rd c pc
      let t = pc + fromIntegral (fromIntegral off :: Int8)
          wrong = (pc .&. 0xFF00) .|. (t .&. 0x00FF)
      when (wrong /= t) (rd c wrong >> pure ())
      setPC c t

  -- Undocumented cc = 11 opcodes.
  undoc = case op of
    0x0B -> imm >>= \v -> andA c v >> getFlag c flagN >>= setFlag c flagC  -- ANC
    0x2B -> imm >>= \v -> andA c v >> getFlag c flagN >>= setFlag c flagC  -- ANC
    0x4B -> imm >>= \v -> do                                               -- ALR
      a <- r8 c iA
      r <- lsr c (a .&. v)
      w8 c iA r
    0x6B -> imm >>= arr                                                    -- ARR
    0x8B -> imm >>= \v -> do                                               -- ANE
      a <- r8 c iA
      x <- r8 c iX
      setA c ((a .|. 0xEE) .&. x .&. v)
    0xAB -> imm >>= \v -> do                                               -- LXA
      a <- r8 c iA
      let r = (a .|. cfgLxaConst (cpuConfig c)) .&. v
      w8 c iX r >> setA c r
    0xCB -> imm >>= \v -> do                                               -- AXS / SBX
      a <- r8 c iA
      x <- r8 c iX
      let ax = a .&. x
      w8 c iX (ax - v)
      compareWith c ax v
    0xEB -> imm >>= sbc c                                                  -- SBC #imm
    0x9B -> do                                                             -- TAS abs,Y
      a <- r8 c iA
      x <- r8 c iX
      w8 c iS (a .&. x)
      shStore c iS iY
    0x93 -> shaInd c                                                         -- SHA (ind),Y
    0x9F -> do                                                             -- SHA abs,Y
      a <- r8 c iA
      x <- r8 c iX
      shStoreV c (a .&. x) iY
    0xBB -> readOp c AbsY >>= \v -> do                                     -- LAS
      s <- r8 c iS
      let r = v .&. s
      w8 c iS r >> w8 c iX r >> setA c r
    _ -> case aaa of
      0 -> rmw c m (\v -> asl c v >>= \r -> ora c r >> pure r)             -- SLO
      1 -> rmw c m (\v -> rol c v >>= \r -> andA c r >> pure r)            -- RLA
      2 -> rmw c m (\v -> lsr c v >>= \r -> eor c r >> pure r)             -- SRE
      3 -> rmw c m (\v -> ror c v >>= \r -> adc c r >> pure r)             -- RRA
      4 -> do                                                              -- SAX
        a <- r8 c iA
        x <- r8 c iX
        writeOp c m (a .&. x)
      5 -> readOp c m >>= \v -> w8 c iX v >> setA c v                      -- LAX
      6 -> rmw c m (\v -> let r = v - 1 in cmpA c r >> pure r)             -- DCP
      _ -> rmw c m (\v -> let r = v + 1 in sbc c r >> pure r)              -- ISC
   where
    -- SAX and LAX index zero page and absolute with Y (97, B7, BF).
    m = case mode01 op of
      ZpX | aaa == 4 || aaa == 5 -> ZpY
      AbsX | aaa == 5 -> AbsY
      mm -> mm
    imm = fetch c

  arr v = do
    a <- r8 c iA
    ci <- getFlag c flagC
    dec <- getFlag c flagD
    let t = a .&. v
        r = (t `shiftR` 1) .|. (if ci then 0x80 else 0)
    if cfgDecimal (cpuConfig c) && dec
      then do
        -- "NMOS 6510 Unintended Opcodes", ARR in decimal mode.
        let ah = t `shiftR` 4
            al = t .&. 0x0F
            r1 = if al + (al .&. 1) > 5 then (r .&. 0xF0) .|. ((r + 6) .&. 0x0F) else r
            hc = ah + (ah .&. 1) > 5
            r2 = if hc then r1 + 0x60 else r1
        w8 c iA r2
        setFlag c flagN ci
        setFlag c flagZ (r == 0)
        setFlag c flagV ((t `xor` r) .&. 0x40 /= 0)
        setFlag c flagC hc
      else do
        setA c r
        setFlag c flagC (testBit r 6)
        setFlag c flagV (testBit r 6 /= testBit r 5)

  jam = do
    _ <- rd c =<< getPC c
    setI c iJam 1

-- (the SH* helpers at the top level, not in 'execute''s where: there they
-- were a closure built on every instruction)

-- | SHX / SHY / TAS: store reg & (base high + 1), base abs; index reg idx.
shStore :: CpuBus b => Cpu b s -> Int -> Int -> ST s ()
shStore c reg ix = do
  v <- r8 c reg
  shStoreV c v ix

shStoreV :: CpuBus b => Cpu b s -> Word8 -> Int -> ST s ()
shStoreV c v ix = do
  lo <- fetch c
  hi <- fetch c
  i <- r8 c ix
  shWrite c v (w16 hi lo) i

shaInd :: CpuBus b => Cpu b s -> ST s ()
shaInd c = do
  p <- fetch c
  lo <- rd c (fromIntegral p)
  hi <- rd c (fromIntegral (p + 1))
  y <- r8 c iY
  a <- r8 c iA
  x <- r8 c iX
  shWrite c (a .&. x) (w16 hi lo) y

-- If RDY held the dummy read (a DMA in the cycle before the write), the
-- & (H+1) drops out: the register value is stored (AccuracyCoin, SH*
-- test 7, on the NES; VICE x64sc the same with the C64's VIC stalls).
shWrite :: CpuBus b => Cpu b s -> Word8 -> Word16 -> Word8 -> ST s ()
shWrite c v base i = do
  let ea = base + fromIntegral i
      wrong = (base .&. 0xFF00) .|. (ea .&. 0x00FF)
  (held, _) <- rdStalled c wrong
  let d = if held then v else v .&. (hiB base + 1)
      target = if wrong /= ea then w16 d (loB ea) else ea
  wr c target d
