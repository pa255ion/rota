{-# LANGUAGE MultiWayIf #-}
{-|
The Zilog Z80 reference model (PLAN.md step 5.1; docs/systems/gamegear.md):
one bus call per machine cycle, in the order of the NMOS Z80, each with its
T-state length, so a machine around it knows when every read, write and
I/O access happens (the Game Gear's VDP and its counters care).

Sources: Zilog's Z80 CPU User Manual (UM0080) for the instruction set and
machine-cycle structure; "The Undocumented Z80 Documented" (Sean Young)
for the undocumented opcodes, the X and Y flags (bits 3 and 5) and the
block instructions' flags; the MEMPTR notes (boo_boo, Vladimir Kladov)
for WZ; Patrik Rak's SCF/CCF findings for Q; David Banks' and Patrik
Rak's 2018 findings for the repeating block instructions' X, Y, H and P/V.
Checked against Tom Harte's SingleStepTests (final state and machine
cycles of every opcode file).

__The bus__ ('Bus') belongs to the caller: the model knows nothing about
the machine. Every machine cycle is exactly one call, whose T-state
length is fixed by its kind (see 'Bus'); the model adds them up
('cpuTStates').

__Interrupts__ are sampled at the start of each 'step', i.e. at the end of
the previous instruction, as the chip samples them in its last T-state.
/NMI is edge triggered (an edge between two instruction ends; a pulse
shorter than an instruction is not modelled), /INT level triggered and
masked by IFF1. No interrupt is taken right after EI, nor between a
prefix and its opcode (one 'step' is a whole prefixed instruction).
IM 0 only executes RST instructions from the bus (anything else is
taken as RST 38h); IM 1 is RST 38h; IM 2 reads its vector at I:data.

__HALT__ leaves PC after the HALT; while halted, every 'step' is one
opcode fetch at PC that is thrown away (R counts on), until an interrupt
pushes PC.

Not modelled: LD A,I / LD A,R reading P/V as 0 when an interrupt is
accepted right after them (an NMOS bug); bus contention.
-}
module Z80.Spec
  ( -- * The bus
    Bus (..)
    -- * Registers
  , Regs (..)
  , zeroRegs
  , flagC, flagN, flagP, flagX, flagH, flagY, flagZ, flagS
    -- * The CPU
  , Cpu
  , cpuStateVector
  , newCpu
  , getRegs
  , setRegs
  , cpuTStates
  , cpuPC
  , Step (..)
  , step
  , reset
  ) where

import Prelude

import Control.Monad (unless, when)
import Control.Monad.ST (ST)
import Data.Bits
import Data.Int (Int8)
import qualified Data.Vector.Unboxed.Mutable as MV
import Data.Word

-- | The machine around the CPU, one call per machine cycle.
data Bus s = Bus
  { busFetch  :: Word16 -> ST s Word8        -- ^ opcode fetch (M1), 4 T (the refresh included)
  , busRead   :: Word16 -> ST s Word8        -- ^ memory read, 3 T
  , busWrite  :: Word16 -> Word8 -> ST s ()  -- ^ memory write, 3 T
  , busIn     :: Word16 -> ST s Word8        -- ^ I/O read, 4 T (one wait state included)
  , busOut    :: Word16 -> Word8 -> ST s ()  -- ^ I/O write, 4 T
  , busIdle   :: Word16 -> Int -> ST s ()    -- ^ n internal T-states, this address on the bus
  , busIntAck :: ST s Word8                  -- ^ interrupt acknowledge (M1 with IORQ), 6 T (two wait states included); the data bus
  , busInt    :: ST s Bool                   -- ^ /INT asserted
  , busNmi    :: ST s Bool                   -- ^ /NMI asserted
  }

-- | The programmer-visible state, plus the hidden registers the
-- undocumented behaviour depends on.
data Regs = Regs
  { rA, rF, rB, rC, rD, rE, rH, rL :: !Word8
  , rAF', rBC', rDE', rHL'          :: !Word16
  , rIX, rIY, rSP, rPC              :: !Word16
  , rI, rR                          :: !Word8
  , rIFF1, rIFF2                    :: !Bool
  , rIM                             :: !Word8
  , rWZ                             :: !Word16 -- ^ MEMPTR
  , rQ                              :: !Word8  -- ^ F if the last instruction changed the flags, else 0 (SCF, CCF read it)
  , rEI                             :: !Bool   -- ^ the last instruction was EI
  , rP                              :: !Bool   -- ^ the last instruction was LD A,I or LD A,R
  , rHalted                         :: !Bool
  } deriving stock (Show, Eq)

-- | All registers 0, interrupts disabled, IM 0, not halted.
zeroRegs :: Regs
zeroRegs = Regs 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 False False 0 0 0 False False False

flagC, flagN, flagP, flagX, flagH, flagY, flagZ, flagS :: Word8
flagC = 0x01
flagN = 0x02
flagP = 0x04 -- ^ parity / overflow
flagX = 0x08 -- ^ undocumented, bit 3
flagH = 0x10
flagY = 0x20 -- ^ undocumented, bit 5
flagZ = 0x40
flagS = 0x80

data Cpu s = Cpu
  { cpuBus   :: !(Bus s)
  , cpuState :: !(MV.MVector s Int)
  }

-- State slots. B C D E H L F A sit at 0 to 7 so that an instruction's
-- 3-bit register field is its slot (6, (HL), never reaches a slot).
iB, iC, iD, iE, iH, iL, iF, iA :: Int
iB = 0
iC = 1
iD = 2
iE = 3
iH = 4
iL = 5
iF = 6
iA = 7
iIXH, iIXL, iIYH, iIYL, iSP, iPC, iWZ, iI, iR, iIFF1, iIFF2, iIM :: Int
iIXH = 8
iIXL = 9
iIYH = 10
iIYL = 11
iSP = 12
iPC = 13
iWZ = 14
iI = 15
iR = 16
iIFF1 = 17
iIFF2 = 18
iIM = 19
iQ, iQNew, iEI, iHalt, iNmiLine, iT, iShadow :: Int
iQ = 20       -- Q after the last instruction
iQNew = 21    -- 1 once the current instruction has changed the flags
iEI = 22      -- the last instruction was EI
iHalt = 23
iNmiLine = 24 -- /NMI at the last instruction end (edge detector)
iT = 25       -- T-states since 'newCpu'
iShadow = 26  -- B' to A' at 26 to 33, in the order of B to A
iP, iPNew :: Int
iP = 34       -- the last instruction was LD A,I or LD A,R
iPNew = 35    -- the current one is

slots :: Int
slots = 36

newCpu :: Bus s -> Regs -> ST s (Cpu s)
newCpu bus r = do
  st <- MV.replicate slots 0
  let c = Cpu bus st
  setRegs c r
  pure c

-- | The CPU's whole state (registers, T-states, the NMI edge, the
-- in-instruction flags): the desktop's save states (side track Q, Q.8;
-- the user, 2026-10-26). Read and written only between steps.
cpuStateVector :: Cpu s -> MV.MVector s Int
cpuStateVector = cpuState

getRegs :: Cpu s -> ST s Regs
getRegs c = do
  let sh i = r8 c (iShadow + i)
  a <- r8 c iA
  f <- r8 c iF
  b <- r8 c iB
  cc <- r8 c iC
  d <- r8 c iD
  e <- r8 c iE
  h <- r8 c iH
  l <- r8 c iL
  a' <- sh iA
  f' <- sh iF
  b' <- sh iB
  c' <- sh iC
  d' <- sh iD
  e' <- sh iE
  h' <- sh iH
  l' <- sh iL
  ix <- pair c iIXH
  iy <- pair c iIYH
  sp <- r16 c iSP
  pc <- r16 c iPC
  i <- r8 c iI
  r <- r8 c iR
  iff1 <- getB c iIFF1
  iff2 <- getB c iIFF2
  im <- r8 c iIM
  wz <- r16 c iWZ
  q <- r8 c iQ
  ei <- getB c iEI
  hl <- getB c iHalt
  pp <- getB c iP
  pure Regs { rA = a, rF = f, rB = b, rC = cc, rD = d, rE = e, rH = h, rL = l
            , rAF' = mk16 a' f', rBC' = mk16 b' c', rDE' = mk16 d' e', rHL' = mk16 h' l'
            , rIX = ix, rIY = iy, rSP = sp, rPC = pc, rI = i, rR = r
            , rIFF1 = iff1, rIFF2 = iff2, rIM = im, rWZ = wz, rQ = q
            , rEI = ei, rP = pp, rHalted = hl }

setRegs :: Cpu s -> Regs -> ST s ()
setRegs c rs = do
  mapM_ (uncurry (w8 c))
    [(iA, rA rs), (iF, rF rs), (iB, rB rs), (iC, rC rs), (iD, rD rs), (iE, rE rs), (iH, rH rs), (iL, rL rs)]
  setShadowPair iA iF (rAF' rs)
  setShadowPair iB iC (rBC' rs)
  setShadowPair iD iE (rDE' rs)
  setShadowPair iH iL (rHL' rs)
  setPair c iIXH (rIX rs)
  setPair c iIYH (rIY rs)
  w16 c iSP (rSP rs)
  w16 c iPC (rPC rs)
  w8 c iI (rI rs)
  w8 c iR (rR rs)
  setB c iIFF1 (rIFF1 rs)
  setB c iIFF2 (rIFF2 rs)
  w8 c iIM (rIM rs)
  w16 c iWZ (rWZ rs)
  w8 c iQ (rQ rs)
  setB c iEI (rEI rs)
  setB c iP (rP rs)
  setB c iHalt (rHalted rs)
 where
  setShadowPair hi lo v = do
    w8 c (iShadow + hi) (hiB v)
    w8 c (iShadow + lo) (loB v)

-- | T-states run since 'newCpu'.
cpuTStates :: Cpu s -> ST s Int
cpuTStates c = getI c iT

-- | PC alone (cheaper than 'getRegs').
cpuPC :: Cpu s -> ST s Word16
cpuPC = getPC

-- | The /RESET line: PC, I, R, IFF1, IFF2 and IM cleared; the other
-- registers keep their values (on the chip they are undefined, mostly
-- FF). Takes no bus cycles.
reset :: Cpu s -> ST s ()
reset c = do
  mapM_ (\i -> setI c i 0) [iPC, iI, iR, iIFF1, iIFF2, iIM, iEI, iHalt, iQ]
  w16 c iSP 0xFFFF
  w8 c iA 0xFF
  w8 c iF 0xFF

-- | What a 'step' did.
data Step
  = Instr !Word8 -- ^ executed an instruction; its first byte (a prefix, for prefixed ones)
  | Halted       -- ^ one opcode fetch of a halted CPU
  | Int          -- ^ accepted an interrupt (/INT)
  | Nmi          -- ^ accepted a non-maskable interrupt
  deriving stock (Show, Eq)

-- * State access

getI :: Cpu s -> Int -> ST s Int
getI c = MV.unsafeRead (cpuState c)
{-# INLINE getI #-}

setI :: Cpu s -> Int -> Int -> ST s ()
setI c = MV.unsafeWrite (cpuState c)
{-# INLINE setI #-}

r8 :: Cpu s -> Int -> ST s Word8
r8 c i = fromIntegral <$> getI c i
{-# INLINE r8 #-}

w8 :: Cpu s -> Int -> Word8 -> ST s ()
w8 c i v = setI c i (fromIntegral v)
{-# INLINE w8 #-}

r16 :: Cpu s -> Int -> ST s Word16
r16 c i = fromIntegral <$> getI c i
{-# INLINE r16 #-}

w16 :: Cpu s -> Int -> Word16 -> ST s ()
w16 c i v = setI c i (fromIntegral v)
{-# INLINE w16 #-}

getB :: Cpu s -> Int -> ST s Bool
getB c i = (/= 0) <$> getI c i

setB :: Cpu s -> Int -> Bool -> ST s ()
setB c i b = setI c i (if b then 1 else 0)

-- | A register pair whose high half is at slot @hi@ and low half at @hi+1@.
pair :: Cpu s -> Int -> ST s Word16
pair c hi = mk16 <$> r8 c hi <*> r8 c (hi + 1)
{-# INLINE pair #-}

setPair :: Cpu s -> Int -> Word16 -> ST s ()
setPair c hi v = w8 c hi (hiB v) >> w8 c (hi + 1) (loB v)
{-# INLINE setPair #-}

mk16 :: Word8 -> Word8 -> Word16
mk16 h l = fromIntegral h `shiftL` 8 .|. fromIntegral l
{-# INLINE mk16 #-}

hiB, loB :: Word16 -> Word8
hiB v = fromIntegral (v `shiftR` 8)
loB = fromIntegral
{-# INLINE hiB #-}
{-# INLINE loB #-}

getPC :: Cpu s -> ST s Word16
getPC c = r16 c iPC

setPC :: Cpu s -> Word16 -> ST s ()
setPC c = w16 c iPC

setWZ :: Cpu s -> Word16 -> ST s ()
setWZ c = w16 c iWZ

getF :: Cpu s -> ST s Word8
getF c = r8 c iF

-- | Flags from an instruction that changes them (Q follows).
setF :: Cpu s -> Word8 -> ST s ()
setF c f = w8 c iF f >> setI c iQNew 1

-- * Machine cycles

tick :: Cpu s -> Int -> ST s ()
tick c n = getI c iT >>= setI c iT . (+ n)
{-# INLINE tick #-}

incR :: Cpu s -> ST s ()
incR c = do
  r <- r8 c iR
  w8 c iR ((r .&. 0x80) .|. ((r + 1) .&. 0x7F))

fetchOp :: Cpu s -> ST s Word8
fetchOp c = do
  pc <- getPC c
  v <- busFetch (cpuBus c) pc
  setPC c (pc + 1)
  incR c
  tick c 4
  pure v

rd :: Cpu s -> Word16 -> ST s Word8
rd c a = do
  v <- busRead (cpuBus c) a
  tick c 3
  pure v

wr :: Cpu s -> Word16 -> Word8 -> ST s ()
wr c a v = busWrite (cpuBus c) a v >> tick c 3

ioIn :: Cpu s -> Word16 -> ST s Word8
ioIn c a = do
  v <- busIn (cpuBus c) a
  tick c 4
  pure v

ioOut :: Cpu s -> Word16 -> Word8 -> ST s ()
ioOut c a v = busOut (cpuBus c) a v >> tick c 4

idle :: Cpu s -> Word16 -> Int -> ST s ()
idle c a n = busIdle (cpuBus c) a n >> tick c n

-- | The byte at PC (a memory read, not a fetch), PC incremented.
imm8 :: Cpu s -> ST s Word8
imm8 c = do
  pc <- getPC c
  v <- rd c pc
  setPC c (pc + 1)
  pure v

imm16 :: Cpu s -> ST s Word16
imm16 c = do
  l <- imm8 c
  h <- imm8 c
  pure (mk16 h l)

-- | Idle T-states with the address of the last byte read (PC - 1) on the bus.
idlePC :: Cpu s -> Int -> ST s ()
idlePC c n = getPC c >>= \pc -> idle c (pc - 1) n

-- | Idle T-states with IR on the bus (after an opcode fetch).
idleIR :: Cpu s -> Int -> ST s ()
idleIR c n = do
  i <- r8 c iI
  r <- r8 c iR
  idle c (mk16 i r) n

push16 :: Cpu s -> Word16 -> ST s ()
push16 c v = do
  sp <- r16 c iSP
  wr c (sp - 1) (hiB v)
  wr c (sp - 2) (loB v)
  w16 c iSP (sp - 2)

pop16 :: Cpu s -> ST s Word16
pop16 c = do
  sp <- r16 c iSP
  l <- rd c sp
  h <- rd c (sp + 1)
  w16 c iSP (sp + 2)
  pure (mk16 h l)

-- * Steps

step :: Cpu s -> ST s Step
step c = do
  let bus = cpuBus c
  nmi <- busNmi bus
  nmiPrev <- getB c iNmiLine
  setB c iNmiLine nmi
  int <- busInt bus
  iff1 <- getB c iIFF1
  ei <- getB c iEI
  setB c iEI False
  setI c iQNew 0
  setI c iPNew 0
  r <- if
    | nmi && not nmiPrev -> takeNmi c >> pure Nmi
    | int && iff1 && not ei -> takeInt c >> pure Int
    | otherwise -> do
        halted <- getB c iHalt
        if halted
          then do
            pc <- getPC c
            _ <- busFetch bus pc
            incR c
            tick c 4
            pure Halted
          else do
            op <- fetchOp c
            Instr op <$ exec c 0 op
  qn <- getI c iQNew
  f <- getF c
  w8 c iQ (if qn /= 0 then f else 0)
  setI c iP =<< getI c iPNew
  pure r

leaveHalt :: Cpu s -> ST s ()
leaveHalt c = setB c iHalt False

-- | NMI: an opcode fetch that is thrown away, one internal T-state, PC
-- pushed, jump to 0066h; 11 T. IFF2 keeps IFF1's old value.
takeNmi :: Cpu s -> ST s ()
takeNmi c = do
  leaveHalt c
  pc <- getPC c
  _ <- busFetch (cpuBus c) pc
  incR c
  tick c 4
  idleIR c 1
  setB c iIFF1 False
  push16 c pc
  setPC c 0x0066
  setWZ c 0x0066

-- | /INT: the acknowledge cycle (6 T), one internal T-state, then by mode:
-- IM 0 and 1 push PC and jump (13 T), IM 2 also reads the vector (19 T).
takeInt :: Cpu s -> ST s ()
takeInt c = do
  leaveHalt c
  v <- busIntAck (cpuBus c)
  tick c 6
  incR c
  setB c iIFF1 False
  setB c iIFF2 False
  idleIR c 1
  pc <- getPC c
  push16 c pc
  im <- r8 c iIM
  target <- case im of
    2 -> do
      i <- r8 c iI
      let vec = mk16 i v
      l <- rd c vec
      h <- rd c (vec + 1)
      pure (mk16 h l)
    1 -> pure 0x0038
    _ -> pure (if v .&. 0xC7 == 0xC7 then fromIntegral (v .&. 0x38) else 0x0038)
  setPC c target
  setWZ c target

-- * Decoding

-- | Index mode: 0 HL, 1 IX, 2 IY.
type Ix = Int

hSlot, lSlot :: Ix -> Int
hSlot ix = case ix of { 1 -> iIXH; 2 -> iIYH; _ -> iH }
lSlot ix = case ix of { 1 -> iIXL; 2 -> iIYL; _ -> iL }

-- | Register field @r@ (not 6) under index mode @ix@ (H and L become the
-- index register's halves).
regSlot :: Ix -> Int -> Int
regSlot ix r = case r of
  4 -> hSlot ix
  5 -> lSlot ix
  _ -> r

getR :: Cpu s -> Ix -> Int -> ST s Word8
getR c ix r = r8 c (regSlot ix r)

setR :: Cpu s -> Ix -> Int -> Word8 -> ST s ()
setR c ix r = w8 c (regSlot ix r)

getHL :: Cpu s -> Ix -> ST s Word16
getHL c ix = pair c (hSlot ix)

setHL :: Cpu s -> Ix -> Word16 -> ST s ()
setHL c ix = setPair c (hSlot ix)

-- | Register pair field @p@: BC, DE, HL (or IX, IY), SP.
getRP :: Cpu s -> Ix -> Int -> ST s Word16
getRP c ix p = case p of
  0 -> pair c iB
  1 -> pair c iD
  2 -> getHL c ix
  _ -> r16 c iSP

setRP :: Cpu s -> Ix -> Int -> Word16 -> ST s ()
setRP c ix p v = case p of
  0 -> setPair c iB v
  1 -> setPair c iD v
  2 -> setHL c ix v
  _ -> w16 c iSP v

-- | PUSH/POP's pair field: AF instead of SP.
getRP2 :: Cpu s -> Ix -> Int -> ST s Word16
getRP2 c ix p
  | p == 3 = mk16 <$> r8 c iA <*> r8 c iF
  | otherwise = getRP c ix p

setRP2 :: Cpu s -> Ix -> Int -> Word16 -> ST s ()
setRP2 c ix p v
  | p == 3 = w8 c iA (hiB v) >> w8 c iF (loB v)
  | otherwise = setRP c ix p v

cond :: Cpu s -> Int -> ST s Bool
cond c y = do
  f <- getF c
  let t m = f .&. m /= 0
  pure $ case y of
    0 -> not (t flagZ)
    1 -> t flagZ
    2 -> not (t flagC)
    3 -> t flagC
    4 -> not (t flagP)
    5 -> t flagP
    6 -> not (t flagS)
    _ -> t flagS

-- | The address of a memory operand: HL, or IX/IY plus a displacement
-- read now, followed by five internal T-states (as in LD r,(IX+d)).
memAddr :: Cpu s -> Ix -> ST s Word16
memAddr c 0 = getHL c 0
memAddr c ix = do
  d <- imm8 c
  base <- getHL c ix
  idlePC c 5
  let a = base + fromIntegral (fromIntegral d :: Int8)
  setWZ c a
  pure a

-- | Execute the instruction whose first byte @op@ has been fetched.
exec :: Cpu s -> Ix -> Word8 -> ST s ()
exec c ix op = case x of
  0 -> case z of
    0 -> case y of
      0 -> pure () -- NOP
      1 -> do      -- EX AF,AF'
        mapM_ (\i -> do
          a <- getI c i
          b <- getI c (iShadow + i)
          setI c i b
          setI c (iShadow + i) a) [iA, iF]
      2 -> do      -- DJNZ d
        idleIR c 1
        d <- imm8 c
        b <- subtract 1 <$> r8 c iB
        w8 c iB b
        when (b /= 0) (jumpRel d)
      3 -> imm8 c >>= jumpRel  -- JR d
      _ -> do                  -- JR cc,d
        d <- imm8 c
        t <- cond c (y - 4)
        when t (jumpRel d)
    1 -> if q == 0
      then imm16 c >>= setRP c ix p                        -- LD rp,nn
      else do                                              -- ADD HL,rp
        hl <- getHL c ix
        v <- getRP c ix p
        idleIR c 7
        f <- getF c
        let res = fromIntegral hl + fromIntegral v :: Int
            r16' = fromIntegral res :: Word16
        setF c ((f .&. (flagS .|. flagZ .|. flagP))
                .|. (hiB r16' .&. (flagX .|. flagY))
                .|. (if (fromIntegral hl `xor` fromIntegral v `xor` res) .&. 0x1000 /= 0 then flagH else 0)
                .|. (if res > 0xFFFF then flagC else 0))
        setWZ c (hl + 1)
        setHL c ix r16'
    2 -> case (q, p) of
      (0, 0) -> storeA =<< pair c iB
      (0, 1) -> storeA =<< pair c iD
      (0, 2) -> do         -- LD (nn),HL
        a <- imm16 c
        hl <- getHL c ix
        wr c a (loB hl)
        wr c (a + 1) (hiB hl)
        setWZ c (a + 1)
      (0, _) -> do         -- LD (nn),A
        a <- imm16 c
        storeA a
      (_, 0) -> loadA =<< pair c iB
      (_, 1) -> loadA =<< pair c iD
      (_, 2) -> do         -- LD HL,(nn)
        a <- imm16 c
        l <- rd c a
        h <- rd c (a + 1)
        setWZ c (a + 1)
        setHL c ix (mk16 h l)
      _ -> imm16 c >>= loadA   -- LD A,(nn)
    3 -> do                    -- INC rp / DEC rp
      v <- getRP c ix p
      idleIR c 2
      setRP c ix p (if q == 0 then v + 1 else v - 1)
    4 -> modifyR (\f v -> incFlags f v)
    5 -> modifyR (\f v -> decFlags f v)
    6 -> if y == 6
      then if ix == 0
        then do
          n <- imm8 c
          hl <- getHL c 0
          wr c hl n
        else do              -- LD (IX+d),n: d, n, 2 internal, write
          d <- imm8 c
          n <- imm8 c
          idlePC c 2
          base <- getHL c ix
          let a = base + fromIntegral (fromIntegral d :: Int8)
          setWZ c a
          wr c a n
      else imm8 c >>= setR c ix y
    _ -> do
      a <- r8 c iA
      f <- getF c
      let keep = f .&. (flagS .|. flagZ .|. flagP)
          xy v = v .&. (flagX .|. flagY)
          rot a' cy = do
            w8 c iA a'
            setF c (keep .|. xy a' .|. (if cy then flagC else 0))
      case y of
        0 -> rot (rotateL a 1) (testBit a 7)                                 -- RLCA
        1 -> rot (rotateR a 1) (testBit a 0)                                 -- RRCA
        2 -> rot (shiftL a 1 .|. (f .&. flagC)) (testBit a 7)                -- RLA
        3 -> rot (shiftR a 1 .|. (if f .&. flagC /= 0 then 0x80 else 0)) (testBit a 0) -- RRA
        4 -> let (a', f') = daa a f in w8 c iA a' >> setF c f'               -- DAA
        5 -> do                                                              -- CPL
          let a' = complement a
          w8 c iA a'
          setF c ((f .&. (flagS .|. flagZ .|. flagP .|. flagC)) .|. flagH .|. flagN .|. xy a')
        6 -> do                                                              -- SCF
          qv <- r8 c iQ
          setF c (keep .|. flagC .|. xy ((qv `xor` f) .|. a))
        _ -> do                                                              -- CCF
          qv <- r8 c iQ
          setF c (keep .|. xy ((qv `xor` f) .|. a)
                  .|. (if f .&. flagC /= 0 then flagH else flagC))
  1 -> if y == 6 && z == 6
    then setB c iHalt True -- HALT
    else if y == 6
      then do              -- LD (HL),r
        a <- memAddr c ix
        v <- getR c 0 z
        wr c a v
      else if z == 6
        then do            -- LD r,(HL)
          a <- memAddr c ix
          rd c a >>= setR c 0 y
        else getR c ix z >>= setR c ix y
  2 -> do                  -- ALU A,r
    v <- if z == 6 then memAddr c ix >>= rd c else getR c ix z
    alu y v
  _ -> case z of
    0 -> do                -- RET cc
      idleIR c 1
      t <- cond c y
      when t ret
    1 -> if q == 0
      then pop16 c >>= setRP2 c ix p   -- POP
      else case p of
        0 -> ret                       -- RET
        1 -> mapM_ (\i -> do           -- EXX
               a <- getI c i
               b <- getI c (iShadow + i)
               setI c i b
               setI c (iShadow + i) a) [iB, iC, iD, iE, iH, iL]
        2 -> getHL c ix >>= setPC c    -- JP (HL)
        _ -> do                        -- LD SP,HL
          hl <- getHL c ix
          idleIR c 2
          w16 c iSP hl
    2 -> do                -- JP cc,nn
      a <- imm16 c
      setWZ c a
      t <- cond c y
      when t (setPC c a)
    3 -> case y of
      0 -> do              -- JP nn
        a <- imm16 c
        setWZ c a
        setPC c a
      1 -> execCB c ix
      2 -> do              -- OUT (n),A
        n <- imm8 c
        a <- r8 c iA
        ioOut c (mk16 a n) a
        setWZ c (mk16 a (n + 1))
      3 -> do              -- IN A,(n)
        n <- imm8 c
        a <- r8 c iA
        let port = mk16 a n
        v <- ioIn c port
        w8 c iA v
        setWZ c (port + 1)
      4 -> do              -- EX (SP),HL
        sp <- r16 c iSP
        l <- rd c sp
        h <- rd c (sp + 1)
        idle c (sp + 1) 1
        hl <- getHL c ix
        wr c (sp + 1) (hiB hl)
        wr c sp (loB hl)
        idle c sp 2
        let v = mk16 h l
        setHL c ix v
        setWZ c v
      5 -> do              -- EX DE,HL (never IX/IY)
        de <- pair c iD
        hl <- pair c iH
        setPair c iD hl
        setPair c iH de
      6 -> setB c iIFF1 False >> setB c iIFF2 False   -- DI
      _ -> do                                         -- EI
        setB c iIFF1 True
        setB c iIFF2 True
        setB c iEI True
    4 -> do                -- CALL cc,nn
      a <- imm16 c
      setWZ c a
      t <- cond c y
      when t (callTo a)
    5 -> if q == 0
      then do              -- PUSH
        v <- getRP2 c ix p
        idleIR c 1
        push16 c v
      else case p of
        0 -> do            -- CALL nn
          a <- imm16 c
          setWZ c a
          callTo a
        1 -> prefix 1
        2 -> fetchOp c >>= execED c
        _ -> prefix 2
    6 -> imm8 c >>= alu y  -- ALU A,n
    _ -> do                -- RST
      idleIR c 1
      pc <- getPC c
      push16 c pc
      let a = fromIntegral (op .&. 0x38)
      setPC c a
      setWZ c a
 where
  x = op `shiftR` 6
  y = fromIntegral ((op `shiftR` 3) .&. 7) :: Int
  z = fromIntegral (op .&. 7) :: Int
  p = y `shiftR` 1
  q = y .&. 1

  -- A DD/FD prefix: the next opcode under the index mode; a further
  -- prefix replaces this one (an ED one cancels it). The prefix changes
  -- no flags, so Q is 0 for what follows (SCF, CCF).
  prefix m = w8 c iQ 0 >> fetchOp c >>= exec c m

  jumpRel d = do
    pc <- getPC c
    idlePC c 5
    let a = pc + fromIntegral (fromIntegral d :: Int8)
    setPC c a
    setWZ c a

  callTo a = do
    idlePC c 1
    pc <- getPC c
    push16 c pc
    setPC c a

  ret = do
    a <- pop16 c
    setPC c a
    setWZ c a

  storeA a = do
    v <- r8 c iA
    wr c a v
    setWZ c (mk16 v (loB a + 1))

  loadA a = do
    rd c a >>= w8 c iA
    setWZ c (a + 1)

  -- INC r / DEC r, on a register or (HL)/(IX+d): read, one internal
  -- T-state, write.
  modifyR fl
    | y == 6 = do
        a <- memAddr c ix
        v <- rd c a
        idle c a 1
        f <- getF c
        let (v', f') = fl f v
        setF c f'
        wr c a v'
    | otherwise = do
        v <- getR c ix y
        f <- getF c
        let (v', f') = fl f v
        setF c f'
        setR c ix y v'

  alu o v = do
    a <- r8 c iA
    f <- getF c
    let (a', f') = alu8 o a v f
    setF c f'
    unless (o == 7) (w8 c iA a')

-- | CB-prefixed instructions; DDCB/FDCB under an index mode.
execCB :: Cpu s -> Ix -> ST s ()
execCB c 0 = do
  op <- fetchOp c
  let y = fromIntegral ((op `shiftR` 3) .&. 7) :: Int
      z = fromIntegral (op .&. 7) :: Int
      g = op `shiftR` 6
  if z == 6
    then do
      hl <- getHL c 0
      v <- rd c hl
      idle c hl 1
      if g == 1
        then do
          wz <- r16 c iWZ
          bitOp y v (hiB wz)
        else do
          f <- getF c
          let (v', f') = cbOp g y v f
          when (g == 0) (setF c f')
          wr c hl v'
    else do
      v <- r8 c z
      if g == 1
        then bitOp y v v
        else do
          f <- getF c
          let (v', f') = cbOp g y v f
          when (g == 0) (setF c f')
          w8 c z v'
 where
  bitOp y v xy = do
    f <- getF c
    setF c (bitFlags y v xy f)
execCB c ix = do
  -- DD CB d op: d and op are memory reads, not fetches (R counts two).
  d <- imm8 c
  op <- imm8 c
  idlePC c 2
  base <- getHL c ix
  let a = base + fromIntegral (fromIntegral d :: Int8)
      y = fromIntegral ((op `shiftR` 3) .&. 7) :: Int
      z = fromIntegral (op .&. 7) :: Int
      g = op `shiftR` 6
  setWZ c a
  v <- rd c a
  idle c a 1
  f <- getF c
  if g == 1
    then setF c (bitFlags y v (hiB a) f)
    else do
      let (v', f') = cbOp g y v f
      when (g == 0) (setF c f')
      wr c a v'
      -- The undocumented forms also copy the result into a register.
      when (z /= 6) (w8 c z v')

-- | Rotates/shifts (group 0), RES (2) and SET (3): result and flags.
cbOp :: Word8 -> Int -> Word8 -> Word8 -> (Word8, Word8)
cbOp g y v f = case g of
  0 ->
    let cy = f .&. flagC /= 0
        (r, co) = case y of
          0 -> (rotateL v 1, testBit v 7)                               -- RLC
          1 -> (rotateR v 1, testBit v 0)                               -- RRC
          2 -> (shiftL v 1 .|. (if cy then 1 else 0), testBit v 7)      -- RL
          3 -> (shiftR v 1 .|. (if cy then 0x80 else 0), testBit v 0)   -- RR
          4 -> (shiftL v 1, testBit v 7)                                -- SLA
          5 -> (shiftR v 1 .|. (v .&. 0x80), testBit v 0)               -- SRA
          6 -> (shiftL v 1 .|. 1, testBit v 7)                          -- SLL (undocumented)
          _ -> (shiftR v 1, testBit v 0)                                -- SRL
    in (r, szp r .|. (if co then flagC else 0))
  2 -> (clearBit v y, f)
  _ -> (setBit v y, f)

-- | BIT y,v: X and Y come from @xy@ (the register, or for memory operands
-- MEMPTR's high byte).
bitFlags :: Int -> Word8 -> Word8 -> Word8 -> Word8
bitFlags y v xy f =
  let set = testBit v y
  in (f .&. flagC) .|. flagH
     .|. (if set then 0 else flagZ .|. flagP)
     .|. (if set && y == 7 then flagS else 0)
     .|. (xy .&. (flagX .|. flagY))

-- | ED-prefixed instructions (opcode @op@ already fetched).
execED :: Cpu s -> Word8 -> ST s ()
execED c op = case x of
  1 -> case z of
    0 -> do                -- IN r,(C) (r = 6: flags only)
      bc <- pair c iB
      v <- ioIn c bc
      setWZ c (bc + 1)
      f <- getF c
      setF c ((f .&. flagC) .|. szp v)
      when (y /= 6) (w8 c y v)
    1 -> do                -- OUT (C),r (r = 6: 0 on the NMOS Z80)
      bc <- pair c iB
      v <- if y == 6 then pure 0 else r8 c y
      ioOut c bc v
      setWZ c (bc + 1)
    2 -> do                -- SBC HL,rp / ADC HL,rp
      hl <- pair c iH
      v <- getRP c 0 p
      idleIR c 7
      f <- getF c
      let cy = if f .&. flagC /= 0 then 1 else 0 :: Int
          hi = fromIntegral hl :: Int
          vi = fromIntegral v :: Int
          res = if q == 0 then hi - vi - cy else hi + vi + cy
          r16' = fromIntegral res :: Word16
          ovf = if q == 0
            then (hi `xor` vi) .&. (hi `xor` res) .&. 0x8000 /= 0
            else (hi `xor` complement vi) .&. (hi `xor` res) .&. 0x8000 /= 0
      setF c ((hiB r16' .&. (flagS .|. flagX .|. flagY))
              .|. (if r16' == 0 then flagZ else 0)
              .|. (if (hi `xor` vi `xor` res) .&. 0x1000 /= 0 then flagH else 0)
              .|. (if ovf then flagP else 0)
              .|. (if q == 0 then flagN else 0)
              .|. (if res < 0 || res > 0xFFFF then flagC else 0))
      setWZ c (hl + 1)
      setPair c iH r16'
    3 -> do                -- LD (nn),rp / LD rp,(nn)
      a <- imm16 c
      if q == 0
        then do
          v <- getRP c 0 p
          wr c a (loB v)
          wr c (a + 1) (hiB v)
        else do
          l <- rd c a
          h <- rd c (a + 1)
          setRP c 0 p (mk16 h l)
      setWZ c (a + 1)
    4 -> do                -- NEG
      a <- r8 c iA
      f <- getF c
      let (a', f') = alu8 2 0 a f
      w8 c iA a'
      setF c f'
    5 -> do                -- RETN / RETI: IFF1 = IFF2
      setB c iIFF1 =<< getB c iIFF2
      a <- pop16 c
      setPC c a
      setWZ c a
    6 -> w8 c iIM (case y .&. 3 of { 2 -> 1; 3 -> 2; _ -> 0 })  -- IM 0/1/2 (0/1 for y = 1, 5: IM 0)
    _ -> case y of
      0 -> idleIR c 1 >> r8 c iA >>= w8 c iI     -- LD I,A
      1 -> idleIR c 1 >> r8 c iA >>= w8 c iR     -- LD R,A
      2 -> idleIR c 1 >> r8 c iI >>= ldAir       -- LD A,I
      3 -> idleIR c 1 >> r8 c iR >>= ldAir       -- LD A,R
      4 -> rotDigit True                         -- RRD
      5 -> rotDigit False                        -- RLD
      _ -> pure ()                               -- NOP (ED 77, ED 7F)
  2 | z <= 3 && y >= 4 -> block (y .&. 1 == 0) (y >= 6) z
  _ -> pure ()             -- the other ED opcodes are 8 T NOPs
 where
  x = op `shiftR` 6
  y = fromIntegral ((op `shiftR` 3) .&. 7) :: Int
  z = fromIntegral (op .&. 7) :: Int
  p = y `shiftR` 1
  q = y .&. 1

  ldAir v = do
    setI c iPNew 1
    w8 c iA v
    f <- getF c
    iff2 <- getB c iIFF2
    setF c ((f .&. flagC) .|. (v .&. (flagS .|. flagX .|. flagY))
            .|. (if v == 0 then flagZ else 0) .|. (if iff2 then flagP else 0))

  rotDigit right = do
    hl <- pair c iH
    m <- rd c hl
    idle c hl 4
    a <- r8 c iA
    let (a', m') = if right
          then ((a .&. 0xF0) .|. (m .&. 0x0F), (a `shiftL` 4) .|. (m `shiftR` 4))
          else ((a .&. 0xF0) .|. (m `shiftR` 4), (m `shiftL` 4) .|. (a .&. 0x0F))
    wr c hl m'
    w8 c iA a'
    f <- getF c
    setF c ((f .&. flagC) .|. szp a')
    setWZ c (hl + 1)

  -- LDI/LDD/CPI/CPD/INI/IND/OUTI/OUTD and their repeating forms.
  block inc rep kind = do
    hl <- pair c iH
    bc <- pair c iB
    let step16 v = if inc then v + 1 else v - 1
    case kind of
      0 -> do              -- LDI/LDD(R)
        v <- rd c hl
        de <- pair c iD
        wr c de v
        idle c de 2
        setPair c iH (step16 hl)
        setPair c iD (step16 de)
        let bc' = bc - 1
        setPair c iB bc'
        a <- r8 c iA
        f <- getF c
        let n = v + a
        setF c ((f .&. (flagS .|. flagZ .|. flagC))
                .|. (if testBit n 1 then flagY else 0) .|. (n .&. flagX)
                .|. (if bc' /= 0 then flagP else 0))
        when (rep && bc' /= 0) (repeatBlock de)
      1 -> do              -- CPI/CPD(R)
        v <- rd c hl
        idle c hl 5
        setPair c iH (step16 hl)
        let bc' = bc - 1
        setPair c iB bc'
        a <- r8 c iA
        f <- getF c
        let r = a - v
            h = (a `xor` v `xor` r) .&. flagH
            n = r - (if h /= 0 then 1 else 0)
        setF c ((f .&. flagC) .|. flagN .|. h
                .|. (r .&. flagS) .|. (if r == 0 then flagZ else 0)
                .|. (if testBit n 1 then flagY else 0) .|. (n .&. flagX)
                .|. (if bc' /= 0 then flagP else 0))
        wz <- r16 c iWZ
        setWZ c (step16 wz)
        when (rep && bc' /= 0 && r /= 0) (repeatBlock hl)
      2 -> do              -- INI/IND(R)
        idleIR c 1
        v <- ioIn c bc
        wr c hl v
        setWZ c (step16 bc)
        let b' = hiB bc - 1
        w8 c iB b'
        setPair c iH (step16 hl)
        let k = fromIntegral v + fromIntegral (step16' (loB bc)) :: Int
        ioFlags v b' k
        when (rep && b' /= 0) (repeatBlock hl >> ioRepeatFlags v b')
      _ -> do              -- OUTI/OUTD, OTIR/OTDR
        idleIR c 1
        v <- rd c hl
        let b' = hiB bc - 1
        w8 c iB b'
        let bc' = mk16 b' (loB bc)
        ioOut c bc' v
        setWZ c (step16 bc')
        let hl' = step16 hl
        setPair c iH hl'
        let k = fromIntegral v + fromIntegral (loB hl') :: Int
        ioFlags v b' k
        when (rep && b' /= 0) (repeatBlock bc' >> ioRepeatFlags v b')
   where
    step16' v = if inc then v + 1 else v - 1

  -- A repeating block instruction goes round again: five internal
  -- T-states, PC back to the ED byte, X and Y from PC's high byte.
  repeatBlock a = do
    idle c a 5
    pc <- subtract 2 <$> getPC c
    setPC c pc
    setWZ c (pc + 1)
    f <- getF c
    setF c ((f .&. complement (flagX .|. flagY)) .|. (hiB pc .&. (flagX .|. flagY)))

  ioFlags v b k = setF c (szp' b
    .|. (if testBit v 7 then flagN else 0)
    .|. (if k > 0xFF then flagH .|. flagC else 0)
    .|. (if parity ((fromIntegral k .&. 7) `xor` b) then flagP else 0))

  -- INIR/INDR/OTIR/OTDR going round again: H and P/V as the ALU leaves
  -- them for the decrement of B that comes with the next round.
  ioRepeatFlags v b = do
    f <- getF c
    let pf = f .&. flagP /= 0
        cy = f .&. flagC /= 0
        (pf', hf) = if cy
          then if testBit v 7
            then (pf /= parity ((b - 1) .&. 7), b .&. 0x0F == 0x00)
            else (pf /= parity ((b + 1) .&. 7), b .&. 0x0F == 0x0F)
          else (pf /= parity (b .&. 7), False)
    setF c ((f .&. complement (flagP .|. flagH))
            .|. (if pf' then 0 else flagP) .|. (if hf then flagH else 0))

-- * Flags

parity :: Word8 -> Bool
parity v = even (popCount v)

-- | S, Z, X, Y from a result and P/V as its parity; H, N, C clear.
szp :: Word8 -> Word8
szp v = szp' v .|. (if parity v then flagP else 0)

-- | S, Z, X, Y from a result.
szp' :: Word8 -> Word8
szp' v = (v .&. (flagS .|. flagX .|. flagY)) .|. (if v == 0 then flagZ else 0)

-- | The 8-bit ALU: ADD ADC SUB SBC AND XOR OR CP (op 0 to 7). CP takes
-- X and Y from the operand.
alu8 :: Int -> Word8 -> Word8 -> Word8 -> (Word8, Word8)
alu8 o a b f = case o of
  4 -> let r = a .&. b in (r, szp r .|. flagH)
  5 -> let r = a `xor` b in (r, szp r)
  6 -> let r = a .|. b in (r, szp r)
  _ ->
    let cy = if o `elem` [1, 3] && f .&. flagC /= 0 then 1 else 0 :: Int
        sub = o >= 2
        ai = fromIntegral a :: Int
        bi = fromIntegral b :: Int
        res = if sub then ai - bi - cy else ai + bi + cy
        r = fromIntegral res :: Word8
        ovf = if sub
          then (ai `xor` bi) .&. (ai `xor` res) .&. 0x80 /= 0
          else (ai `xor` complement bi) .&. (ai `xor` res) .&. 0x80 /= 0
        xySrc = if o == 7 then b else r
    in ( r
       , (r .&. flagS) .|. (if r == 0 then flagZ else 0)
         .|. (xySrc .&. (flagX .|. flagY))
         .|. (if (ai `xor` bi `xor` res) .&. 0x10 /= 0 then flagH else 0)
         .|. (if ovf then flagP else 0)
         .|. (if sub then flagN else 0)
         .|. (if res < 0 || res > 0xFF then flagC else 0) )

incFlags, decFlags :: Word8 -> Word8 -> (Word8, Word8)
incFlags f v =
  let r = v + 1
  in (r, (f .&. flagC) .|. szp' r
         .|. (if r .&. 0x0F == 0 then flagH else 0)
         .|. (if r == 0x80 then flagP else 0))
decFlags f v =
  let r = v - 1
  in (r, (f .&. flagC) .|. szp' r .|. flagN
         .|. (if r .&. 0x0F == 0x0F then flagH else 0)
         .|. (if r == 0x7F then flagP else 0))

daa :: Word8 -> Word8 -> (Word8, Word8)
daa a f =
  let n = f .&. flagN /= 0
      h = f .&. flagH /= 0
      cy = f .&. flagC /= 0
      lo = a .&. 0x0F
      corrLo = if h || lo > 9 then 0x06 else 0
      cy' = cy || a > 0x99
      corr = corrLo .|. (if cy' then 0x60 else 0)
      r = if n then a - corr else a + corr
      h' = if n then h && lo < 6 else lo > 9
  in (r, szp r .|. (f .&. flagN) .|. (if h' then flagH else 0) .|. (if cy' then flagC else 0))
