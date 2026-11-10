{-|
The Sony SPC700 reference model (PLAN.md step 7.2; docs/systems/snes.md):
one bus cycle at a time, in the chip's order, dummy reads included, so
the sound module around it sees every access (reading a timer's counter
at 00FDh-00FFh clears it, so a dummy read there matters).

Sources: fullsnes ("SNES APU SPC700 CPU": the opcode table, flags and
cycle counts; MUL, DIV, DAA and DAS), anomie's SPC700 document, and the
cycle order SingleStepTests (github.com/SingleStepTests/spc700) records,
which it is checked against: final state and every cycle's address,
direction and data.

__The bus__ ('Bus') belongs to the caller: a read or a write is one
cycle, as is an internal cycle ('busIdle', no address). The direct page
is 0000h-00FFh, or 0100h-01FFh with P set; indexed and 16-bit direct
page addresses wrap within it. The stack is page 1.

__SLEEP and STOP__ halt the CPU (the SNES has nothing to wake it); each
later 'step' is two cycles, a dummy read at PC and an internal cycle.
The SPC700 has no interrupt inputs on the SNES; BRK is its only
interrupt-like instruction.
-}
module Spc700.Spec
  ( Bus (..)
  , Cpu
  , newCpu
  , getRegs
  , setRegs
  , cpuCycles
  , cpuHalted
  , cpuCopy
  , Step (..)
  , step
  ) where

import Prelude

import Control.Monad (replicateM_, unless, when)
import Control.Monad.ST (ST)
import Data.Bits
import qualified Data.Vector.Unboxed.Mutable as MV
import Data.Word

import Spc700.Types

data Bus s = Bus
  { busRead  :: Word16 -> ST s Word8
  , busWrite :: Word16 -> Word8 -> ST s ()
  , busIdle  :: ST s () -- ^ an internal cycle: nothing on the bus
  }

data Cpu s = Cpu
  { cpuBus   :: !(Bus s)
  , cpuState :: !(MV.MVector s Int)
  }

-- | The second CPU's whole state (its registers, cycle count, halt) made
-- the first's; each keeps its own bus (the SNES's save states, step 7.15:
-- the sound module moved whole in the spec's restore test).
cpuCopy :: Cpu s -> Cpu s -> ST s ()
cpuCopy from to = MV.copy (cpuState to) (cpuState from)

iA, iX, iY, iSP, iPC, iPSW, iCyc, iHalt :: Int
iA = 0
iX = 1
iY = 2
iSP = 3
iPC = 4
iPSW = 5
iCyc = 6
iHalt = 7

newCpu :: Bus s -> Regs -> ST s (Cpu s)
newCpu bus r = do
  st <- MV.replicate 8 0
  let c = Cpu bus st
  setRegs c r
  pure c

getRegs :: Cpu s -> ST s Regs
getRegs c = Regs <$> b iA <*> b iX <*> b iY <*> b iSP
                 <*> (fromIntegral <$> g c iPC) <*> b iPSW
 where
  b i = fromIntegral <$> g c i

setRegs :: Cpu s -> Regs -> ST s ()
setRegs c r = do
  s c iA (fromIntegral (rA r))
  s c iX (fromIntegral (rX r))
  s c iY (fromIntegral (rY r))
  s c iSP (fromIntegral (rSP r))
  s c iPC (fromIntegral (rPC r))
  s c iPSW (fromIntegral (rPSW r))

-- | Cycles run since 'newCpu'.
cpuCycles :: Cpu s -> ST s Int
cpuCycles c = g c iCyc

-- | After SLEEP or STOP.
cpuHalted :: Cpu s -> ST s Bool
cpuHalted c = (/= 0) <$> g c iHalt

data Step
  = Instr !Word8 -- ^ executed this opcode
  | Halted       -- ^ two cycles of a halted CPU
  deriving stock (Show, Eq)

g :: Cpu s -> Int -> ST s Int
g c = MV.unsafeRead (cpuState c)
{-# INLINE g #-}

s :: Cpu s -> Int -> Int -> ST s ()
s c = MV.unsafeWrite (cpuState c)
{-# INLINE s #-}

tick :: Cpu s -> ST s ()
tick c = g c iCyc >>= s c iCyc . (+ 1)

rd :: Cpu s -> Int -> ST s Int
rd c a = do
  v <- busRead (cpuBus c) (fromIntegral (a .&. 0xFFFF))
  tick c
  pure (fromIntegral v)

wr :: Cpu s -> Int -> Int -> ST s ()
wr c a v = busWrite (cpuBus c) (fromIntegral (a .&. 0xFFFF)) (fromIntegral (v .&. 0xFF)) >> tick c

idle :: Cpu s -> ST s ()
idle c = busIdle (cpuBus c) >> tick c

-- | A dummy read at PC, which doesn't advance.
dummyPC :: Cpu s -> ST s ()
dummyPC c = () <$ (g c iPC >>= rd c)

fetch :: Cpu s -> ST s Int
fetch c = do
  pc <- g c iPC
  s c iPC ((pc + 1) .&. 0xFFFF)
  rd c pc

fetch16 :: Cpu s -> ST s Int
fetch16 c = do
  lo <- fetch c
  hi <- fetch c
  pure (lo .|. hi `shiftL` 8)

flag :: Cpu s -> Word8 -> ST s Bool
flag c f = (\p -> p .&. fromIntegral f /= 0) <$> g c iPSW

setFlag :: Cpu s -> Word8 -> Bool -> ST s ()
setFlag c f v = do
  p <- g c iPSW
  s c iPSW (if v then p .|. fromIntegral f else p .&. complement (fromIntegral f))

setNZ :: Cpu s -> Int -> ST s ()
setNZ c v = setFlag c flagN (v .&. 0x80 /= 0) >> setFlag c flagZ (v .&. 0xFF == 0)

-- | A direct page address (wrapping within the page).
dp :: Cpu s -> Int -> ST s Int
dp c off = (\p -> (if p .&. 0x20 /= 0 then 0x100 else 0) .|. (off .&. 0xFF)) <$> g c iPSW

push :: Cpu s -> Int -> ST s ()
push c v = do
  sp <- g c iSP
  wr c (0x100 .|. sp) v
  s c iSP ((sp - 1) .&. 0xFF)

pull :: Cpu s -> ST s Int
pull c = do
  sp <- g c iSP
  let sp' = (sp + 1) .&. 0xFF
  s c iSP sp'
  rd c (0x100 .|. sp')

-- * Arithmetic

adc :: Cpu s -> Int -> Int -> ST s Int
adc c a v = do
  ci <- (\f -> if f then 1 else 0) <$> flag c flagC
  let r = a + v + ci
  setFlag c flagC (r > 0xFF)
  setFlag c flagH ((a `xor` v `xor` r) .&. 0x10 /= 0)
  setFlag c flagV (complement (a `xor` v) .&. (a `xor` r) .&. 0x80 /= 0)
  setNZ c r
  pure (r .&. 0xFF)

sbc :: Cpu s -> Int -> Int -> ST s Int
sbc c a v = adc c a (complement v .&. 0xFF)

cmp :: Cpu s -> Int -> Int -> ST s Int
cmp c a v = do
  setFlag c flagC (a >= v)
  setNZ c ((a - v) .&. 0xFF)
  pure a

data Alu = OR | AND | EOR | CMP | ADC | SBC
  deriving stock (Eq)

alu :: Cpu s -> Alu -> Int -> Int -> ST s Int
alu c op a v = case op of
  OR -> let r = a .|. v in setNZ c r >> pure r
  AND -> let r = a .&. v in setNZ c r >> pure r
  EOR -> let r = a `xor` v in setNZ c r >> pure r
  CMP -> cmp c a v
  ADC -> adc c a v
  SBC -> sbc c a v

data Sh = ASL | ROL | LSR | ROR | DEC | INC
  deriving stock (Eq)

shiftOp :: Cpu s -> Sh -> Int -> ST s Int
shiftOp c op v = do
  ci <- (\f -> if f then 1 else 0) <$> flag c flagC
  r <- case op of
    ASL -> setFlag c flagC (v .&. 0x80 /= 0) >> pure ((v `shiftL` 1) .&. 0xFF)
    ROL -> setFlag c flagC (v .&. 0x80 /= 0) >> pure (((v `shiftL` 1) .|. ci) .&. 0xFF)
    LSR -> setFlag c flagC (v .&. 1 /= 0) >> pure (v `shiftR` 1)
    ROR -> setFlag c flagC (v .&. 1 /= 0) >> pure ((v `shiftR` 1) .|. (ci `shiftL` 7))
    DEC -> pure ((v - 1) .&. 0xFF)
    INC -> pure ((v + 1) .&. 0xFF)
  setNZ c r
  pure r

-- * Execution

-- | One instruction, or two cycles of a halted CPU.
step :: Cpu s -> ST s Step
step c = do
  h <- cpuHalted c
  if h then dummyPC c >> idle c >> pure Halted else do
    op <- fetch c
    execute c op
    pure (Instr (fromIntegral op))

execute :: Cpu s -> Int -> ST s ()
execute c op
  -- the ALU block: rows 0, 2, 4, 6, 8, A are OR, AND, EOR, CMP, ADC, SBC;
  -- columns 4 to 9 their modes
  | hi `elem` [0x0, 0x2, 0x4, 0x6, 0x8, 0xA], lo `elem` [0x4 .. 0x9] = aluBlock (rowOp hi) lo
  | hi `elem` [0x1, 0x3, 0x5, 0x7, 0x9, 0xB], lo `elem` [0x4 .. 0x9] = aluBlock1 (rowOp (hi - 1)) lo
  | lo == 0x1 = tcall
  | lo == 0x2 = setClr
  | lo == 0x3 = bbx
  | otherwise = other c op
 where
  hi = op `shiftR` 4
  lo = op .&. 0xF
  bitNo = hi `shiftR` 1
  rowOp k = case k of 0x0 -> OR; 0x2 -> AND; 0x4 -> EOR; 0x6 -> CMP; 0x8 -> ADC; _ -> SBC
  -- A with dp, abs, (X), [dp+X], #imm; dp with dp (09)
  aluBlock f m = case m of
    0x4 -> withA f =<< (rd c =<< dp c =<< fetch c)
    0x5 -> withA f =<< (rd c =<< fetch16 c)
    0x6 -> dummyPC c >> (withA f =<< (rd c =<< dp c =<< g c iX))
    0x7 -> do
      off <- fetch c
      idle c
      x <- g c iX
      lo' <- rd c =<< dp c (off + x)
      hi' <- rd c =<< dp c (off + x + 1)
      withA f =<< rd c (lo' .|. hi' `shiftL` 8)
    0x8 -> withA f =<< fetch c
    _ -> do
      src <- rd c =<< dp c =<< fetch c
      da <- dp c =<< fetch c
      dst <- rd c da
      r <- alu c f dst src
      if f == CMP then idle c else wr c da r
  -- A with dp+X, abs+X, abs+Y, [dp]+Y; dp with #imm (18); (X) with (Y) (19)
  aluBlock1 f m = case m of
    0x4 -> do
      off <- fetch c
      idle c
      x <- g c iX
      withA f =<< (rd c =<< dp c (off + x))
    0x5 -> absIdx iX >>= withA f
    0x6 -> absIdx iY >>= withA f
    0x7 -> do
      off <- fetch c
      idle c
      lo' <- rd c =<< dp c off
      hi' <- rd c =<< dp c (off + 1)
      y <- g c iY
      withA f =<< rd c ((lo' .|. hi' `shiftL` 8) + y)
    0x8 -> do
      imm <- fetch c
      da <- dp c =<< fetch c
      v <- rd c da
      r <- alu c f v imm
      if f == CMP then idle c else wr c da r
    _ -> do
      dummyPC c
      v <- rd c =<< dp c =<< g c iY
      da <- dp c =<< g c iX
      d <- rd c da
      r <- alu c f d v
      if f == CMP then idle c else wr c da r
  absIdx ri = do
    a <- fetch16 c
    idle c
    i <- g c ri
    rd c (a + i)
  withA f v = do
    a <- g c iA
    r <- alu c f a v
    unless (f == CMP) (s c iA r)
  -- TCALL n: PC to the vector at FFDEh - 2n
  tcall = do
    dummyPC c
    idle c
    pc <- g c iPC
    push c (pc `shiftR` 8)
    push c (pc .&. 0xFF)
    idle c
    let v = 0xFFDE - 2 * hi
    l <- rd c v
    h <- rd c (v + 1)
    s c iPC (l .|. h `shiftL` 8)
  -- SET1 / CLR1 dp.bit
  setClr = do
    da <- dp c =<< fetch c
    v <- rd c da
    wr c da (if even hi then setBit v bitNo else clearBit v bitNo)
  -- BBS / BBC dp.bit, rel
  bbx = do
    v <- rd c =<< dp c =<< fetch c
    idle c
    branch c (testBit v bitNo == even hi)

-- | The relative operand, then two internal cycles if taken.
branch :: Cpu s -> Bool -> ST s ()
branch c taken = do
  off <- fetch c
  when taken $ do
    idle c
    idle c
    g c iPC >>= s c iPC . (\pc -> (pc + (if off >= 0x80 then off - 0x100 else off)) .&. 0xFFFF)

other :: Cpu s -> Int -> ST s ()
other c op = case op of
  0x00 -> dummyPC c
  -- branches
  0x10 -> not <$> flag c flagN >>= branch c
  0x30 -> flag c flagN >>= branch c
  0x50 -> not <$> flag c flagV >>= branch c
  0x70 -> flag c flagV >>= branch c
  0x90 -> not <$> flag c flagC >>= branch c
  0xB0 -> flag c flagC >>= branch c
  0xD0 -> not <$> flag c flagZ >>= branch c
  0xF0 -> flag c flagZ >>= branch c
  0x2F -> branch c True
  -- flags
  0x20 -> dummyPC c >> setFlag c flagP False
  0x40 -> dummyPC c >> setFlag c flagP True
  0x60 -> dummyPC c >> setFlag c flagC False
  0x80 -> dummyPC c >> setFlag c flagC True
  0xA0 -> dummyPC c >> idle c >> setFlag c flagI True
  0xC0 -> dummyPC c >> idle c >> setFlag c flagI False
  0xE0 -> dummyPC c >> setFlag c flagV False >> setFlag c flagH False
  0xED -> dummyPC c >> idle c >> (not <$> flag c flagC >>= setFlag c flagC)
  -- the bit operations on mem.bit (abs: 13-bit address, 3-bit bit number)
  0x0A -> bitOp True $ \cf b -> cf || b
  0x2A -> bitOp True $ \cf b -> cf || not b
  0x4A -> bitOp False $ \cf b -> cf && b
  0x6A -> bitOp False $ \cf b -> cf && not b
  0x8A -> bitOp True $ \cf b -> cf /= b
  0xAA -> bitOp False $ \_ b -> b
  0xCA -> do
    (a, b) <- memBit
    v <- rd c a
    idle c
    cf <- flag c flagC
    wr c a (if cf then setBit v b else clearBit v b)
  0xEA -> do
    (a, b) <- memBit
    v <- rd c a
    wr c a (complementBit v b)
  -- read-modify-write on dp, dp+X, abs, A
  0x0B -> rmwDp ASL
  0x2B -> rmwDp ROL
  0x4B -> rmwDp LSR
  0x6B -> rmwDp ROR
  0x8B -> rmwDp DEC
  0xAB -> rmwDp INC
  0x1B -> rmwDpX ASL
  0x3B -> rmwDpX ROL
  0x5B -> rmwDpX LSR
  0x7B -> rmwDpX ROR
  0x9B -> rmwDpX DEC
  0xBB -> rmwDpX INC
  0x0C -> rmwAbs ASL
  0x2C -> rmwAbs ROL
  0x4C -> rmwAbs LSR
  0x6C -> rmwAbs ROR
  0x8C -> rmwAbs DEC
  0xAC -> rmwAbs INC
  0x1C -> regOp iA ASL
  0x3C -> regOp iA ROL
  0x5C -> regOp iA LSR
  0x7C -> regOp iA ROR
  0x9C -> regOp iA DEC
  0xBC -> regOp iA INC
  0x1D -> regOp iX DEC
  0x3D -> regOp iX INC
  0xDC -> regOp iY DEC
  0xFC -> regOp iY INC
  -- 16-bit
  0x1A -> wordStep (-1)
  0x3A -> wordStep 1
  0x5A -> do
    da <- fetch c
    l <- rd c =<< dp c da
    h <- rd c =<< dp c (da + 1)
    ya <- getYA
    let w = l .|. h `shiftL` 8
    setFlag c flagC (ya >= w)
    setFlag c flagN ((ya - w) .&. 0x8000 /= 0)
    setFlag c flagZ ((ya - w) .&. 0xFFFF == 0)
  0x7A -> addw False
  0x9A -> addw True
  0xBA -> do
    da <- fetch c
    l <- rd c =<< dp c da
    idle c
    h <- rd c =<< dp c (da + 1)
    s c iA l
    s c iY h
    setFlag c flagN (h .&. 0x80 /= 0)
    setFlag c flagZ (l .|. h == 0)
  0xDA -> do
    da <- fetch c
    a0 <- dp c da
    _ <- rd c a0
    g c iA >>= wr c a0
    a1 <- dp c (da + 1)
    g c iY >>= wr c a1
  -- MUL, DIV
  0xCF -> do
    dummyPC c
    replicateM_ 7 (idle c)
    a <- g c iA
    y <- g c iY
    let r = a * y
    s c iA (r .&. 0xFF)
    s c iY (r `shiftR` 8)
    setNZ c (r `shiftR` 8)
  0x9E -> do
    dummyPC c
    replicateM_ 10 (idle c)
    ya <- getYA
    x <- g c iX
    y <- g c iY
    setFlag c flagH ((y .&. 0xF) >= (x .&. 0xF))
    setFlag c flagV (y >= x)
    let (q, r) = if y < x `shiftL` 1
          then ya `quotRem` x
          else let t = ya - (x `shiftL` 9)
               in (255 - t `quot` (256 - x), x + t `rem` (256 - x))
    s c iA (q .&. 0xFF)
    s c iY (r .&. 0xFF)
    setNZ c q
  -- decimal adjust, nibble swap
  0xDF -> do
    dummyPC c
    idle c
    a <- g c iA
    cf <- flag c flagC
    hf <- flag c flagH
    a1 <- if cf || a > 0x99 then setFlag c flagC True >> pure ((a + 0x60) .&. 0xFF) else pure a
    let a2 = if hf || (a1 .&. 0xF) > 9 then (a1 + 6) .&. 0xFF else a1
    s c iA a2
    setNZ c a2
  0xBE -> do
    dummyPC c
    idle c
    a <- g c iA
    cf <- flag c flagC
    hf <- flag c flagH
    a1 <- if not cf || a > 0x99 then setFlag c flagC False >> pure ((a - 0x60) .&. 0xFF) else pure a
    let a2 = if not hf || (a1 .&. 0xF) > 9 then (a1 - 6) .&. 0xFF else a1
    s c iA a2
    setNZ c a2
  0x9F -> do
    dummyPC c
    replicateM_ 3 (idle c)
    a <- g c iA
    let r = ((a `shiftL` 4) .|. (a `shiftR` 4)) .&. 0xFF
    s c iA r
    setNZ c r
  -- TSET1, TCLR1 abs
  0x0E -> testSet True
  0x4E -> testSet False
  -- compares with X and Y
  0x1E -> cmpReg iX =<< (rd c =<< fetch16 c)
  0x3E -> cmpReg iX =<< (rd c =<< dp c =<< fetch c)
  0xC8 -> cmpReg iX =<< fetch c
  0x5E -> cmpReg iY =<< (rd c =<< fetch16 c)
  0x7E -> cmpReg iY =<< (rd c =<< dp c =<< fetch c)
  0xAD -> cmpReg iY =<< fetch c
  -- CBNE, DBNZ
  0x2E -> do
    v <- rd c =<< dp c =<< fetch c
    idle c
    a <- g c iA
    branch c (a /= v)
  0xDE -> do
    off <- fetch c
    idle c
    x <- g c iX
    v <- rd c =<< dp c (off + x)
    idle c
    a <- g c iA
    branch c (a /= v)
  0x6E -> do
    da <- dp c =<< fetch c
    v <- rd c da
    let v' = (v - 1) .&. 0xFF
    wr c da v'
    branch c (v' /= 0)
  0xFE -> do
    dummyPC c
    idle c
    y <- g c iY
    let y' = (y - 1) .&. 0xFF
    s c iY y'
    branch c (y' /= 0)
  -- stack
  0x0D -> dummyPC c >> (g c iPSW >>= push c) >> idle c
  0x2D -> dummyPC c >> (g c iA >>= push c) >> idle c
  0x4D -> dummyPC c >> (g c iX >>= push c) >> idle c
  0x6D -> dummyPC c >> (g c iY >>= push c) >> idle c
  0x8E -> dummyPC c >> idle c >> (pull c >>= s c iPSW)
  0xAE -> dummyPC c >> idle c >> (pull c >>= s c iA)
  0xCE -> dummyPC c >> idle c >> (pull c >>= s c iX)
  0xEE -> dummyPC c >> idle c >> (pull c >>= s c iY)
  -- jumps, calls, returns
  0x5F -> fetch16 c >>= s c iPC
  0x1F -> do
    a <- fetch16 c
    idle c
    x <- g c iX
    l <- rd c (a + x)
    h <- rd c (a + x + 1)
    s c iPC (l .|. h `shiftL` 8)
  0x3F -> do
    a <- fetch16 c
    idle c
    pc <- g c iPC
    push c (pc `shiftR` 8)
    push c (pc .&. 0xFF)
    idle c
    idle c
    s c iPC a
  0x4F -> do
    u <- fetch c
    idle c
    pc <- g c iPC
    push c (pc `shiftR` 8)
    push c (pc .&. 0xFF)
    idle c
    s c iPC (0xFF00 .|. u)
  0x6F -> do
    dummyPC c
    idle c
    l <- pull c
    h <- pull c
    s c iPC (l .|. h `shiftL` 8)
  0x7F -> do
    dummyPC c
    idle c
    pull c >>= s c iPSW
    l <- pull c
    h <- pull c
    s c iPC (l .|. h `shiftL` 8)
  0x0F -> do
    dummyPC c
    pc <- g c iPC
    push c (pc `shiftR` 8)
    push c (pc .&. 0xFF)
    g c iPSW >>= push c
    idle c
    l <- rd c 0xFFDE
    h <- rd c 0xFFDF
    s c iPC (l .|. h `shiftL` 8)
    setFlag c flagB True
    setFlag c flagI False
  -- moves: register to register
  0x5D -> transfer iA iX True
  0x7D -> transfer iX iA True
  0x9D -> transfer iSP iX True
  0xBD -> transfer iX iSP False
  0xDD -> transfer iY iA True
  0xFD -> transfer iA iY True
  -- immediate loads
  0xE8 -> load iA =<< fetch c
  0xCD -> load iX =<< fetch c
  0x8D -> load iY =<< fetch c
  -- loads
  0xE4 -> load iA =<< (rd c =<< dp c =<< fetch c)
  0xE5 -> load iA =<< (rd c =<< fetch16 c)
  0xE6 -> dummyPC c >> (load iA =<< (rd c =<< dp c =<< g c iX))
  0xE7 -> do
    off <- fetch c
    idle c
    x <- g c iX
    l <- rd c =<< dp c (off + x)
    h <- rd c =<< dp c (off + x + 1)
    load iA =<< rd c (l .|. h `shiftL` 8)
  0xF4 -> load iA =<< dpIdx iX
  0xF5 -> load iA =<< absIdx' iX
  0xF6 -> load iA =<< absIdx' iY
  0xF7 -> do
    off <- fetch c
    idle c
    l <- rd c =<< dp c off
    h <- rd c =<< dp c (off + 1)
    y <- g c iY
    load iA =<< rd c ((l .|. h `shiftL` 8) + y)
  0xBF -> do
    dummyPC c
    x <- g c iX
    v <- rd c =<< dp c x
    idle c
    s c iX ((x + 1) .&. 0xFF)
    load iA v
  0xF8 -> load iX =<< (rd c =<< dp c =<< fetch c)
  0xF9 -> load iX =<< dpIdx iY
  0xE9 -> load iX =<< (rd c =<< fetch16 c)
  0xEB -> load iY =<< (rd c =<< dp c =<< fetch c)
  0xFB -> load iY =<< dpIdx iX
  0xEC -> load iY =<< (rd c =<< fetch16 c)
  -- stores: each reads its destination first (but MOV (X)+,A and MOV dp,dp)
  0xC4 -> storeAt iA =<< (dp c =<< fetch c)
  0xC5 -> storeAt iA =<< fetch16 c
  0xC6 -> dummyPC c >> (storeAt iA =<< (dp c =<< g c iX))
  0xC7 -> do
    off <- fetch c
    idle c
    x <- g c iX
    l <- rd c =<< dp c (off + x)
    h <- rd c =<< dp c (off + x + 1)
    storeAt iA (l .|. h `shiftL` 8)
  0xD4 -> do
    off <- fetch c
    idle c
    x <- g c iX
    storeAt iA =<< dp c (off + x)
  0xD5 -> storeIdx iX
  0xD6 -> storeIdx iY
  0xD7 -> do
    off <- fetch c
    l <- rd c =<< dp c off
    h <- rd c =<< dp c (off + 1)
    idle c
    y <- g c iY
    storeAt iA ((l .|. h `shiftL` 8) + y)
  0xAF -> do
    dummyPC c
    idle c
    x <- g c iX
    a <- dp c x
    g c iA >>= wr c a
    s c iX ((x + 1) .&. 0xFF)
  0xD8 -> storeAt iX =<< (dp c =<< fetch c)
  0xD9 -> do
    off <- fetch c
    idle c
    y <- g c iY
    storeAt iX =<< dp c (off + y)
  0xC9 -> storeAt iX =<< fetch16 c
  0xCB -> storeAt iY =<< (dp c =<< fetch c)
  0xDB -> do
    off <- fetch c
    idle c
    x <- g c iX
    storeAt iY =<< dp c (off + x)
  0xCC -> storeAt iY =<< fetch16 c
  0xFA -> do
    v <- rd c =<< dp c =<< fetch c
    da <- dp c =<< fetch c
    wr c da v
  0x8F -> do
    v <- fetch c
    da <- dp c =<< fetch c
    _ <- rd c da
    wr c da v
  -- SLEEP, STOP
  0xEF -> halt
  0xFF -> halt
  _ -> error ("Spc700.Spec: opcode " <> show op <> " not decoded")
 where
  halt = do
    s c iHalt 1
    dummyPC c
    idle c
  getYA = (\a y -> a .|. y `shiftL` 8) <$> g c iA <*> g c iY
  load ri v = s c ri v >> setNZ c v
  transfer from to flags = do
    dummyPC c
    v <- g c from
    s c to v
    when flags (setNZ c v)
  storeAt ri a = do
    _ <- rd c a
    g c ri >>= wr c a
  storeIdx ri = do
    a <- fetch16 c
    idle c
    i <- g c ri
    storeAt iA ((a + i) .&. 0xFFFF)
  dpIdx ri = do
    off <- fetch c
    idle c
    i <- g c ri
    rd c =<< dp c (off + i)
  absIdx' ri = do
    a <- fetch16 c
    idle c
    i <- g c ri
    rd c (a + i)
  cmpReg ri v = g c ri >>= \r -> () <$ cmp c r v
  regOp ri f = do
    dummyPC c
    v <- g c ri
    shiftOp c f v >>= s c ri
  rmwDp f = do
    a <- dp c =<< fetch c
    v <- rd c a
    shiftOp c f v >>= wr c a
  rmwDpX f = do
    off <- fetch c
    idle c
    x <- g c iX
    a <- dp c (off + x)
    v <- rd c a
    shiftOp c f v >>= wr c a
  rmwAbs f = do
    a <- fetch16 c
    v <- rd c a
    shiftOp c f v >>= wr c a
  memBit = do
    w <- fetch16 c
    pure (w .&. 0x1FFF, w `shiftR` 13)
  -- OR1, EOR1 have an extra internal cycle; AND1, MOV1 C,m.b don't
  bitOp extra f = do
    (a, b) <- memBit
    v <- rd c a
    when extra (idle c)
    cf <- flag c flagC
    setFlag c flagC (f cf (testBit v b))
  testSet set = do
    a <- fetch16 c
    v <- rd c a
    _ <- rd c a
    acc <- g c iA
    setFlag c flagN ((acc - v) .&. 0x80 /= 0)
    setFlag c flagZ (acc == v)
    wr c a (if set then v .|. acc else v .&. complement acc)
  wordStep d = do
    da <- fetch c
    a0 <- dp c da
    a1 <- dp c (da + 1)
    l <- rd c a0
    let l' = l + d
    wr c a0 (l' .&. 0xFF)
    h <- rd c a1
    let w = ((h `shiftL` 8) + l + d) .&. 0xFFFF
    wr c a1 (w `shiftR` 8)
    setFlag c flagN (w .&. 0x8000 /= 0)
    setFlag c flagZ (w == 0)
  -- ADDW / SUBW YA,dp: two 8-bit additions, Z for the 16-bit result
  addw sub = do
    da <- fetch c
    l <- rd c =<< dp c da
    idle c
    h <- rd c =<< dp c (da + 1)
    a <- g c iA
    y <- g c iY
    setFlag c flagC sub
    rl <- (if sub then sbc else adc) c a l
    rh <- (if sub then sbc else adc) c y h
    s c iA rl
    s c iY rh
    setFlag c flagZ (rl .|. rh == 0)
