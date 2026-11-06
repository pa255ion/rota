{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-|
The Motorola 68000, bus-cycle exact: the reference model (side track P,
CLAUDE.md rule 1; docs/cpus/m68k.md). Pure Haskell; the caller supplies
the bus ('BusOps') in any monad, and 'step' runs one instruction, making
the chip's bus accesses in the chip's order.

The prefetch queue is the chip's: 'cIr' is the opcode being run, at
'cPc'; 'cIrc' the word after it. Taking an extension word ('extWord')
uses IRC, moves 'cPc' on and reads the next word into IRC; each
instruction ends by moving IRC into IR and reading the word after it
('prefetch'), as Tom Harte's SingleStepTests record (NOP: one read, at
pc + 4).

Per-instruction bus orders follow Jorge Cwik's "Yet Another Cycle Hunting
Table" (YACHT), checked against the tests.
-}
module M68k.Spec
  ( -- * State
    Cpu (..)
  , dReg
  , aReg
  , setD
  , setA
  , supervisor
    -- * The bus
  , Size (..)
  , Fc
  , BusOps (..)
  , Fault (..)
    -- * Running
  , step
  , pending
    -- * Condition codes
  , flagC, flagV, flagZ, flagN, flagX
  ) where

import Prelude

import Data.Array.Unboxed (UArray, (!), (//))
import Data.Bits hiding (And, Ior, Xor, Iff)
import Data.Int
import Data.Word

-- | The registers, SR and the prefetch queue. A7 is 'cUsp' or 'cSsp' by
-- SR's S bit ('aReg' 7 reads the active one).
data Cpu = Cpu
  { cD   :: !(UArray Int Word32) -- ^ D0-D7
  , cA   :: !(UArray Int Word32) -- ^ A0-A6
  , cUsp :: !Word32
  , cSsp :: !Word32
  , cSr  :: !Word16
  , cPc  :: !Word32 -- ^ the address of the opcode in 'cIr'
  , cIr  :: !Word16
  , cIrc :: !Word16
  , cStopped :: !Bool -- ^ STOP ran: no instructions until an interrupt or a reset
  , cIplSeen :: !Int -- ^ the interrupt level last sampled (level 7 is taken on its rising edge)
  } deriving stock (Show, Eq)

dReg :: Cpu -> Int -> Word32
dReg c n = cD c ! n

aReg :: Cpu -> Int -> Word32
aReg c 7 = if supervisor c then cSsp c else cUsp c
aReg c n = cA c ! n

setD :: Int -> Word32 -> Cpu -> Cpu
setD n v c = c { cD = cD c // [(n, v)] }

setA :: Int -> Word32 -> Cpu -> Cpu
setA 7 v c
  | supervisor c = c { cSsp = v }
  | otherwise = c { cUsp = v }
setA n v c = c { cA = cA c // [(n, v)] }

supervisor :: Cpu -> Bool
supervisor c = testBit (cSr c) 13

flagC, flagV, flagZ, flagN, flagX :: Int
flagC = 0
flagV = 1
flagZ = 2
flagN = 3
flagX = 4

-- | A bus cycle's width: a byte (UDS or LDS) or a word.
data Size = Byte | Word
  deriving stock (Show, Eq)

-- | The function code, FC2-FC0: 1 user data, 2 user program, 5
-- supervisor data, 6 supervisor program, 7 interrupt acknowledge.
type Fc = Word8

-- | The bus, in the caller's monad. Each read and write is a bus cycle of
-- four clocks, TAS's indivisible read-modify-write ten (no wait states in
-- the model's count: DTACK is the bus's business); 'busIdle' is internal
-- cycles. Addresses are the
-- 24-bit bus's; a byte read gives the byte in the low 8 bits.
data BusOps m = BusOps
  { busRead  :: Fc -> Size -> Word32 -> m Word16
  , busWrite :: Fc -> Size -> Word32 -> Word16 -> m ()
  , busTas   :: Fc -> Word32 -> (Word8 -> Word8) -> m Word8
  , busIdle  :: Int -> m ()
  , busIpl   :: m Int
    -- ^ the interrupt level on IPL2-IPL0 (0: none), sampled before each
    -- instruction
  , busIack  :: Int -> m (Maybe Word8)
    -- ^ the interrupt acknowledge cycle for a level (a four-clock bus
    -- cycle, function code 7): the vector number the device puts on the
    -- bus, or Nothing for an autovector (VPA: vector 24 + level)
  , busBerr  :: m Bool
    -- ^ whether BERR ended the bus cycle just made (no device there): a
    -- bus error (vector 2)
  }


-- | Why an instruction stopped short. An address error becomes exception
-- processing ('step' runs it); 'Unimplemented' is the model's own gap (an
-- opcode it doesn't run yet).
data Fault
  = AddressError !Word32 !Bool !Bool !Fc
    -- ^ the address, a read, a program (instruction stream) access, its
    -- function code
  | BusError !Word32 !Bool !Bool !Fc
    -- ^ BERR ended a bus cycle (the bus says so, 'busBerr'): as the
    -- address error, vector 2
  | Unimplemented !Word16
  deriving stock (Show, Eq)

-- | The model's monad: the CPU's state, the bus, and an early exit that
-- keeps the state as it was at the fault (what was done before it stays
-- done: flags, a post-increment).
newtype X m a = X { runX :: BusOps m -> Cpu -> m (Either (Fault, Cpu) (a, Cpu)) }

instance Monad m => Functor (X m) where
  fmap f (X g) = X $ \b c -> fmap (fmap (\(a, c') -> (f a, c'))) (g b c)

instance Monad m => Applicative (X m) where
  pure a = X $ \_ c -> pure (Right (a, c))
  X f <*> X g = X $ \b c -> f b c >>= \case
    Left e -> pure (Left e)
    Right (h, c') -> fmap (fmap (\(a, c'') -> (h a, c''))) (g b c')

instance Monad m => Monad (X m) where
  X g >>= k = X $ \b c -> g b c >>= \case
    Left e -> pure (Left e)
    Right (a, c') -> runX (k a) b c'

get :: Monad m => X m Cpu
get = X $ \_ c -> pure (Right (c, c))

gets :: Monad m => (Cpu -> a) -> X m a
gets f = f <$> get

modify :: Monad m => (Cpu -> Cpu) -> X m ()
modify f = X $ \_ c -> pure (Right ((), f c))

throw :: Monad m => Fault -> X m a
throw e = X $ \_ c -> pure (Left (e, c))

bus :: Monad m => (BusOps m -> m a) -> X m a
bus f = X $ \b c -> (\a -> Right (a, c)) <$> f b

idle :: Monad m => Int -> X m ()
idle n = bus (\b -> busIdle b n)

-- | The 24-bit address bus.
mask24 :: Word32 -> Word32
mask24 = (.&. 0x00FF_FFFF)

progFc, dataFc :: Cpu -> Fc
progFc c = if supervisor c then 6 else 2
dataFc c = if supervisor c then 5 else 1

-- * Bus accesses

-- | A word from program space; odd: an address error.
readProg :: Monad m => Word32 -> X m Word16
readProg a = do
  fc <- gets progFc
  if testBit a 0 then throw (AddressError a True True fc) else bus (\b -> busRead b fc Word (mask24 a)) <* berr a True True fc

readData :: Monad m => Size -> Word32 -> X m Word16
readData sz a = do
  fc <- gets dataFc
  if sz == Word && testBit a 0 then throw (AddressError a True False fc) else bus (\b -> busRead b fc sz (mask24 a)) <* berr a True False fc

writeData :: Monad m => Size -> Word32 -> Word16 -> X m ()
writeData sz a v = do
  fc <- gets dataFc
  if sz == Word && testBit a 0 then throw (AddressError a False False fc) else bus (\b -> busWrite b fc sz (mask24 a) v) <* berr a False False fc

-- | A bus error if BERR ended the cycle just made.
berr :: Monad m => Word32 -> Bool -> Bool -> Fc -> X m ()
berr a rd prog fc = do
  e <- bus busBerr
  if e then throw (BusError a rd prog fc) else pure ()

-- | The end of every instruction: IRC into IR, the next word into IRC.
prefetch :: Monad m => X m ()
prefetch = do
  c <- get
  let pc' = cPc c + 2
  w <- readProg (pc' + 2)
  modify (\x -> x { cPc = pc', cIr = cIrc x, cIrc = w })

-- | An extension word: IRC, then the next word into IRC.
extWord :: Monad m => X m Word16
extWord = do
  c <- get
  let pc' = cPc c + 2
  w <- readProg (pc' + 2)
  modify (\x -> x { cPc = pc', cIrc = w })
  pure (cIrc c)

-- * Operand sizes and values

data Sz = SB | SW | SL
  deriving stock (Show, Eq)

bytes :: Sz -> Word32
bytes SB = 1
bytes SW = 2
bytes SL = 4

-- | The operand's bits of a 32-bit value (the rest zero).
trunc :: Sz -> Word32 -> Word32
trunc SB v = v .&. 0xFF
trunc SW v = v .&. 0xFFFF
trunc SL v = v

msb :: Sz -> Word32 -> Bool
msb SB v = testBit v 7
msb SW v = testBit v 15
msb SL v = testBit v 31

sext8, sext16 :: Word32 -> Word32
sext8 v = fromIntegral (fromIntegral (fromIntegral v :: Word8) :: Int8)
sext16 v = fromIntegral (fromIntegral (fromIntegral v :: Word16) :: Int16)

-- | A data register with its low byte or word replaced (the rest kept).
merge :: Sz -> Word32 -> Word32 -> Word32
merge SB old v = (old .&. 0xFFFF_FF00) .|. (v .&. 0xFF)
merge SW old v = (old .&. 0xFFFF_0000) .|. (v .&. 0xFFFF)
merge SL _ v = v

-- * Condition codes

setFlags :: [(Int, Bool)] -> Cpu -> Cpu
setFlags fs c = c { cSr = foldr (\(i, v) s -> if v then setBit s i else clearBit s i) (cSr c) fs }

-- | N and Z from a result, V and C cleared (moves, logic).
logicFlags :: Sz -> Word32 -> Cpu -> Cpu
logicFlags sz v = setFlags [(flagN, msb sz v), (flagZ, trunc sz v == 0), (flagV, False), (flagC, False)]

-- * Effective addresses

data EA
  = EDn !Int | EAn !Int | EInd !Int | EPost !Int | EPre !Int | EDisp !Int | EIdx !Int
  | EAbsW | EAbsL | EPcDisp | EPcIdx | EImm
  deriving stock (Show, Eq)

-- | The mode and register fields (3 bits each).
decodeEA :: Int -> Int -> Maybe EA
decodeEA mode r = case mode of
  0 -> Just (EDn r)
  1 -> Just (EAn r)
  2 -> Just (EInd r)
  3 -> Just (EPost r)
  4 -> Just (EPre r)
  5 -> Just (EDisp r)
  6 -> Just (EIdx r)
  7 -> case r of
    0 -> Just EAbsW
    1 -> Just EAbsL
    2 -> Just EPcDisp
    3 -> Just EPcIdx
    4 -> Just EImm
    _ -> Nothing
  _ -> Nothing

-- | Where an operand is: a register, memory, or an immediate value.
data Loc = LD !Int | LA !Int | LMem !Word32 | LPost !Int !Word32 | LImm !Word32
  deriving stock (Show, Eq)

-- | An address register's step for (An)+ and -(An): a byte on A7 moves it
-- by two (the stack stays even).
stepOf :: Sz -> Int -> Word32
stepOf SB 7 = 2
stepOf sz _ = bytes sz

-- | The index of a brief extension word: its register, word (sign
-- extended) or long, plus the 8-bit displacement.
indexed :: Monad m => Word32 -> Word16 -> X m Word32
indexed base ext = do
  c <- get
  let r = fromIntegral ((ext `shiftR` 12) .&. 7)
      x = if testBit ext 15 then aReg c r else dReg c r
      xi = if testBit ext 11 then x else sext16 x
  pure (base + xi + sext8 (fromIntegral ext))

-- | An operand's location, with the bus cycles that find it (extension
-- words, the index's idle cycles) and the register change of (An)+ and
-- -(An). @src@: a source operand: -(An) takes two idle cycles first, and
-- (An)+ steps now (a read that faults leaves it stepped); a destination's
-- (An)+ steps after its write ('writeLoc'), and its -(An) has no idle.
locate :: Monad m => Bool -> Sz -> EA -> X m Loc
locate src sz = \case
  EDn r -> pure (LD r)
  EAn r -> pure (LA r)
  EInd r -> LMem <$> gets (`aReg` r)
  EPost r
    | src -> do
        a <- gets (`aReg` r)
        modify (setA r (a + stepOf sz r))
        pure (LMem a)
    | otherwise -> LPost r <$> gets (`aReg` r)
  EPre r -> do
    if src then idle 2 else pure ()
    a <- subtract (stepOf sz r) <$> gets (`aReg` r)
    modify (setA r a)
    pure (LMem a)
  EDisp r -> do
    d <- extWord
    a <- gets (`aReg` r)
    pure (LMem (a + sext16 (fromIntegral d)))
  EIdx r -> do
    idle 2
    e <- extWord
    a <- gets (`aReg` r)
    LMem <$> indexed a e
  EAbsW -> LMem . sext16 . fromIntegral <$> extWord
  EAbsL -> do
    h <- extWord
    l <- extWord
    pure (LMem (fromIntegral h `shiftL` 16 .|. fromIntegral l))
  EPcDisp -> do
    base <- gets ((+ 2) . cPc)
    d <- extWord
    pure (LMem (base + sext16 (fromIntegral d)))
  EPcIdx -> do
    base <- gets ((+ 2) . cPc)
    idle 2
    e <- extWord
    LMem <$> indexed base e
  EImm -> case sz of
    SL -> do
      h <- extWord
      l <- extWord
      pure (LImm (fromIntegral h `shiftL` 16 .|. fromIntegral l))
    _ -> LImm . trunc sz . fromIntegral <$> extWord

-- | A memory operand: a byte, a word, or a long as two words (high
-- first).
readMem :: Monad m => Sz -> Word32 -> X m Word32
readMem SB a = fromIntegral . (.&. 0xFF) <$> readData Byte a
readMem SW a = fromIntegral <$> readData Word a
readMem SL a = do
  h <- readData Word a
  l <- readData Word (a + 2)
  pure (fromIntegral h `shiftL` 16 .|. fromIntegral l)

readLoc :: Monad m => Sz -> Loc -> X m Word32
readLoc sz = \case
  LD r -> trunc sz <$> gets (`dReg` r)
  LA r -> trunc sz <$> gets (`aReg` r)
  LMem a -> readMem sz a
  LPost _ a -> readMem sz a
  LImm v -> pure v

-- | An operand written to memory (@lowFirst@: see 'writeMem'); (An)+
-- stepped after it.
writeLoc :: Monad m => Bool -> Sz -> Loc -> Word32 -> X m ()
writeLoc lowFirst sz l v = case l of
  LMem a -> writeMem lowFirst sz a v
  LPost r a -> do
    writeMem lowFirst sz a v
    modify (setA r (a + stepOf sz r))
  _ -> pure ()

-- | A long to memory as two words: the high word first (MOVE), or the low
-- word first (@lowFirst@: a read-modify-write's write-back).
writeMem :: Monad m => Bool -> Sz -> Word32 -> Word32 -> X m ()
writeMem _ SB a v = writeData Byte a (fromIntegral v .&. 0xFF)
writeMem _ SW a v = writeData Word a (fromIntegral v)
writeMem lowFirst SL a v
  | lowFirst = do
      writeData Word (a + 2) (fromIntegral v)
      writeData Word a (fromIntegral (v `shiftR` 16))
  | otherwise = do
      writeData Word a (fromIntegral (v `shiftR` 16))
      writeData Word (a + 2) (fromIntegral v)

-- * Running

-- | One instruction: the one in IR, with its bus accesses through @b@.
-- An address error runs the exception (group 0); Left: an opcode the
-- model doesn't run yet.
--
-- Before the instruction the interrupt level is sampled: one above SR's
-- mask (level 7: on its rising edge, whatever the mask) is taken instead
-- of the instruction ('interrupt'); a stopped CPU (STOP) does nothing else
-- until one comes. With SR's T bit set at its start, the instruction is
-- followed by the trace exception (vector 9). Neither is in the
-- SingleStepTests (no case starts with T set; none has an interrupt):
-- their timing is YACHT's (docs/cpus/m68k.md).
step :: Monad m => BusOps m -> Cpu -> m (Either Fault Cpu)
step b c0 = do
  ipl <- busIpl b
  let c = c0 { cIplSeen = ipl }
  if pending ipl c0
    then finish (runX (interrupt ipl) b c)
    else if cStopped c
      then pure (Right c)
      else do
        let tracing = testBit (cSr c) 15
        r <- runX (exec (cIr c)) b c
        r' <- case r of
          Left (f@AddressError {}, c') -> runX (addressError (cIr c) f) b c'
          Left (f@BusError {}, c') -> runX (addressError (cIr c) f) b c'
          _ -> pure r
        case r' of
          Right ((), c') | tracing -> finish (runX (gets cPc >>= exception 4 9) b c')
          _ -> finish (pure r')
 where
  -- a fault in an exception (double fault: halt) is the model's gap
  finish x = x >>= \case
    Right ((), c') -> pure (Right c')
    Left (f, _) -> pure (Left f)

-- | Whether 'step' takes an interrupt at this level instead of an
-- instruction: above SR's mask, or level 7 newly asserted.
pending :: Int -> Cpu -> Bool
pending ipl c =
  let mask = fromIntegral ((cSr c `shiftR` 8) .&. 7) :: Int
  in (ipl > 0 && ipl < 7 && ipl > mask) || (ipl == 7 && cIplSeen c < 7)

-- | An interrupt at a level: six idle cycles, the PC's low word pushed,
-- the acknowledge cycle (the vector, or an autovector), four idle cycles,
-- SR and the PC's high word pushed, the vector read and the queue filled
-- there: 44 clocks (YACHT: n nn ns ni n- n nS ns nV nv np n np). SR: S
-- set, T cleared, the mask raised to the level; a STOP ends.
interrupt :: Monad m => Int -> X m ()
interrupt lvl = do
  idle 6
  c <- get
  let sr0 = cSr c
  modify (\x -> x { cSr = ((sr0 .|. 0x2000) .&. 0x78FF) .|. (fromIntegral lvl `shiftL` 8), cStopped = False })
  sp <- gets cSsp
  let pc = cPc c
  writeData Word (sp - 2) (fromIntegral pc)
  v <- bus (\b -> busIack b lvl)
  idle 4
  writeData Word (sp - 6) sr0
  writeData Word (sp - 4) (fromIntegral (pc `shiftR` 16))
  modify (\x -> x { cSsp = sp - 6 })
  vector (maybe (24 + fromIntegral lvl) fromIntegral v)

-- | Group 0: the address error (vector 3) and the bus error (vector 2,
-- the same frame and timing: the latter's not in the SingleStepTests,
-- untested against a chip). Four idle cycles, then the
-- 14-byte frame (the PC, SR, IR, the access's address, and a status word:
-- IR's upper 11 bits, R/W, I/N and the function code), written in the
-- chip's order; the vector; the queue refilled from the handler.
addressError :: Monad m => Word16 -> Fault -> X m ()
addressError op f = do
  let (addr, rd, prog, fc, vec) = case f of
        AddressError a r p x -> (a, r, p, x, 3)
        BusError a r p x -> (a, r, p, x, 2)
        _ -> (0, False, False, 0, 3)
  c <- get
  idle 4
  let sr0 = cSr c
      sp = cSsp c
      pc = cPc c
      status = (op .&. 0xFFE0) .|. (if rd then 0x10 else 0) .|. (if prog then 0x08 else 0) .|. fromIntegral fc
  modify (\x -> x { cSr = (sr0 .|. 0x2000) .&. 0x7FFF })
  writeData Word (sp - 2) (fromIntegral pc)
  writeData Word (sp - 6) sr0
  writeData Word (sp - 4) (fromIntegral (pc `shiftR` 16))
  writeData Word (sp - 8) op
  writeData Word (sp - 10) (fromIntegral addr)
  writeData Word (sp - 14) status
  writeData Word (sp - 12) (fromIntegral (addr `shiftR` 16))
  modify (\x -> x { cSsp = sp - 14 })
  vector vec

-- | Jump through vector @n@: its long, then the queue filled there.
vector :: Monad m => Word32 -> X m ()
vector n = do
  h <- readData Word (n * 4)
  l <- readData Word (n * 4 + 2)
  let pc = fromIntegral h `shiftL` 16 .|. fromIntegral l
  ir <- readProg pc
  idle 2
  irc <- readProg (pc + 2)
  modify (\x -> x { cPc = pc, cIr = ir, cIrc = irc })

exec :: Monad m => Word16 -> X m ()
exec op
  | op == 0x4E71 = prefetch -- NOP
  | op .&. 0xF100 == 0x7000 = moveq op
  | op .&. 0xC000 == 0 && op .&. 0x3000 /= 0 = move op
  | Just x <- movem op = x
  | Just x <- divide op = x
  | Just x <- mulBcd op = x
  | Just x <- branches op = x
  | Just x <- system op = x
  | Just x <- bitOps op = x
  | Just x <- shifts op = x
  | Just x <- arith op = x
  | op == 0x4E72 = stop -- STOP #imm
  | otherwise = illegal op

field :: Word16 -> Int -> Int
field op at = fromIntegral ((op `shiftR` at) .&. 7)

-- | MOVEQ #d8,Dn: the byte sign-extended; N and Z set, V and C cleared.
moveq :: Monad m => Word16 -> X m ()
moveq op = do
  let v = sext8 (fromIntegral op)
  modify (logicFlags SL v . setD (field op 9) v)
  prefetch

-- | MOVE and MOVEA (00ss ddd DDD MMM rrr; size 1 byte, 3 word, 2 long).
move :: Monad m => Word16 -> X m ()
move op = do
  let sz = case op `shiftR` 12 of
        1 -> SB
        3 -> SW
        _ -> SL
      src = decodeEA (field op 3) (field op 0)
      dst = decodeEA (field op 6) (field op 9)
  case (src, dst) of
    (Just s, Just d) | valid s d sz -> do
      v <- locate True sz s >>= readLoc sz
      case d of
        EAn r -> do -- MOVEA: no flags; a word sign-extended
          modify (setA r (if sz == SW then sext16 v else v))
          prefetch
        EDn r -> do
          modify (logicFlags sz v . (\c -> setD r (merge sz (dReg c r) v) c))
          prefetch
        EPre r | sz == SL -> do
          -- a long to -(An): An down by two, the low word, down by two
          -- more, the high word (a fault: at the first, An two down)
          modify (logicFlags sz v)
          prefetch
          a0 <- gets (`aReg` r)
          modify (setA r (a0 - 2))
          writeData Word (a0 - 2) (fromIntegral v)
          modify (setA r (a0 - 4))
          writeData Word (a0 - 4) (fromIntegral (v `shiftR` 16))
        EPre _ -> do
          modify (logicFlags sz v)
          prefetch
          l <- locate False sz d
          writeLoc True sz l v
        EAbsL | memSrc s -> do
          -- the write once the first word is taken and the second is in
          -- IRC; then past the second word, then the prefetch
          h <- extWord
          l <- gets cIrc
          modify (logicFlags sz v)
          writeMem False sz (fromIntegral h `shiftL` 16 .|. fromIntegral l) v
          _ <- extWord
          prefetch
        _ -> do
          l <- locate False sz d
          modify (logicFlags sz v)
          writeLoc False sz l v
          prefetch
    _ -> throw (Unimplemented op)
 where
  valid s d sz = (sz /= SB || not (isAn s || isAn d)) && destOk d
  isAn (EAn _) = True
  isAn _ = False
  memSrc = \case
    EDn _ -> False
    EAn _ -> False
    EImm -> False
    _ -> True
  destOk = \case
    EPcDisp -> False
    EPcIdx -> False
    EImm -> False
    _ -> True

-- * Integer arithmetic and logic

data Alu = Add | Sub | And | Or | Eor | Cmp
  deriving stock (Show, Eq)

-- | A result and its flags: @d op s@ (Sub and Cmp: @d - s@). Logic: N
-- and Z, V and C cleared; ADD and SUB set X as C; Cmp leaves X.
alu :: Alu -> Sz -> Word32 -> Word32 -> Cpu -> (Word32, Cpu)
alu op sz s d c = case op of
  Add -> arithOp (d + s) True
  Sub -> arithOp (d - s) False
  Cmp -> (d, setFlags (nzvc (d - s) False) c)
  And -> logic (d .&. s)
  Or -> logic (d .|. s)
  Eor -> logic (d `xor` s)
 where
  logic r = (trunc sz r, logicFlags sz r c)
  arithOp r add = (trunc sz r, setFlags ((flagX, carry r add) : nzvc r add) c)
  nzvc r add = [(flagN, msb sz r), (flagZ, trunc sz r == 0), (flagV, overflow r add), (flagC, carry r add)]
  carry r add = let sm = msb sz s; dm = msb sz d; rm = msb sz r
                in if add then (sm && dm) || (not rm && dm) || (sm && not rm)
                          else (sm && not dm) || (rm && not dm) || (sm && rm)
  overflow r add = let sm = msb sz s; dm = msb sz d; rm = msb sz r
                   in if add then (sm && dm && not rm) || (not sm && not dm && rm)
                             else (not sm && dm && not rm) || (sm && not dm && rm)

-- | ADDX, SUBX, NEGX: with X in the sum, and Z only ever cleared.
aluX :: Alu -> Sz -> Word32 -> Word32 -> Cpu -> (Word32, Cpu)
aluX op sz s d c =
  let x = if testBit (cSr c) flagX then 1 else 0
      z0 = testBit (cSr c) flagZ
      r = trunc sz (if op == Add then d + s + x else d - s - x)
      sm = msb sz s
      dm = msb sz d
      rm = msb sz r
      (cy, ov) = if op == Add
        then ((sm && dm) || (not rm && dm) || (sm && not rm), (sm && dm && not rm) || (not sm && not dm && rm))
        else ((sm && not dm) || (rm && not dm) || (sm && rm), (not sm && dm && not rm) || (sm && not dm && rm))
  in (r, setFlags [(flagX, cy), (flagN, rm), (flagZ, z0 && r == 0), (flagV, ov), (flagC, cy)] c)

sizeBits :: Word16 -> Maybe Sz
sizeBits op = case (op `shiftR` 6) .&. 3 of
  0 -> Just SB
  1 -> Just SW
  2 -> Just SL
  _ -> Nothing

isReg :: EA -> Bool
isReg = \case
  EDn _ -> True
  EAn _ -> True
  _ -> False

isAnEA :: EA -> Bool
isAnEA = \case
  EAn _ -> True
  _ -> False

-- | Memory alterable: memory, not PC-relative or an immediate.
memAlterable :: EA -> Bool
memAlterable = \case
  EDn _ -> False
  EAn _ -> False
  EPcDisp -> False
  EPcIdx -> False
  EImm -> False
  _ -> True

-- | Data alterable: Dn or memory alterable.
dataAlt :: EA -> Bool
dataAlt = \case
  EDn _ -> True
  e -> memAlterable e

-- | Apply an ALU step to the state, keeping its result.
runAlu :: Monad m => (Cpu -> (Word32, Cpu)) -> X m Word32
runAlu f = do
  c <- get
  let (r, c') = f c
  modify (const c')
  pure r

-- | A read-modify-write of a memory operand: read, the operation, the
-- prefetch, the write-back (a long low word first).
rmw :: Monad m => Sz -> EA -> (Word32 -> Cpu -> (Word32, Cpu)) -> X m ()
rmw sz e f = do
  l <- locate True sz e
  d <- readLoc sz l
  r <- runAlu (f d)
  prefetch
  writeLoc True sz l r

-- | The ALU group: ADD, SUB, CMP, AND, OR, EOR, their A, X, I and Q forms,
-- CMPM, NEG, NEGX, NOT, CLR, TST. Nothing for other opcodes.
arith :: Monad m => Word16 -> Maybe (X m ())
arith op = case op `shiftR` 12 of
  0xD -> twoOp Add
  0x9 -> twoOp Sub
  0xB -> cmpEor
  0xC -> twoOp And
  0x8 -> twoOp Or
  0x0 -> immediate
  0x5 -> quick
  0x4 -> single
  _ -> Nothing
 where
  reg = field op 9
  eaMode = field op 3
  ea = decodeEA eaMode (field op 0)
  opmode = field op 6
  okSrc k sz e = case e of
    EAn _ -> k `elem` [Add, Sub, Cmp] && sz /= SB
    _ -> True
  -- ADD, SUB, AND, OR: <ea>,Dn (opmodes 0-2), Dn,<ea> (4-6), ADDA/SUBA
  -- (3, 7), ADDX/SUBX (4-6 on registers); AND and OR's 3 and 7 are
  -- MUL/DIV, their 4-6 on registers ABCD/SBCD/EXG (later)
  twoOp k = case opmode of
    3 | k `elem` [Add, Sub] -> addrOp k SW
    7 | k `elem` [Add, Sub] -> addrOp k SL
    o | o <= 2, Just sz <- sizeBits op, Just e <- ea, okSrc k sz e -> Just (toDn k sz e)
      | o >= 4 && o <= 6, Just sz <- sizeBits op, eaMode <= 1 ->
          if k `elem` [Add, Sub] then Just (extended k sz) else Nothing
      | o >= 4 && o <= 6, Just sz <- sizeBits op, Just e <- ea, memAlterable e -> Just (toMem k sz e)
    _ -> Nothing
  cmpEor = case opmode of
    3 -> addrOp Cmp SW
    7 -> addrOp Cmp SL
    o | o <= 2, Just sz <- sizeBits op, Just e <- ea, okSrc Cmp sz e -> Just (toDn Cmp sz e)
      | Just sz <- sizeBits op, eaMode == 1 -> Just (cmpm sz)
      | Just sz <- sizeBits op, eaMode == 0 -> Just (eorDn sz (field op 0))
      | Just sz <- sizeBits op, Just e <- ea, memAlterable e -> Just (toMem Eor sz e)
    _ -> Nothing
  -- <ea> op Dn -> Dn (Cmp: the flags only)
  toDn k sz e = do
    s <- locate True sz e >>= readLoc sz
    d <- gets (`dReg` reg)
    r <- runAlu (alu k sz s (trunc sz d))
    if k == Cmp then pure () else modify (setD reg (merge sz d r))
    prefetch
    if sz == SL then idle (if k /= Cmp && (isReg e || e == EImm) then 4 else 2) else pure ()
  -- Dn op <ea> -> <ea> (memory)
  toMem k sz e = do
    s <- gets (trunc sz . (`dReg` reg))
    rmw sz e (alu k sz s)
  eorDn sz r = do
    s <- gets (trunc sz . (`dReg` reg))
    d <- gets (`dReg` r)
    v <- runAlu (alu Eor sz s (trunc sz d))
    modify (setD r (merge sz d v))
    prefetch
    if sz == SL then idle 4 else pure ()
  -- ADDA, SUBA, CMPA: a word sign-extended; no flags but CMPA's (a long
  -- compare)
  addrOp k sz = flip fmap ea $ \e -> do
    v0 <- locate True sz e >>= readLoc sz
    let v = if sz == SW then sext16 v0 else v0
    a <- gets (`aReg` reg)
    case k of
      Cmp -> do
        _ <- runAlu (alu Cmp SL v a)
        prefetch
        idle 2
      _ -> do
        modify (setA reg (if k == Add then a + v else a - v))
        prefetch
        idle (if sz == SW || isReg e || e == EImm then 4 else 2)
  -- ADDX, SUBX: Dy,Dx or -(Ay),-(Ax)
  extended k sz
    | eaMode == 0 = do
        s <- gets (trunc sz . (`dReg` field op 0))
        d <- gets (`dReg` reg)
        r <- runAlu (aluX k sz s (trunc sz d))
        modify (setD reg (merge sz d r))
        prefetch
        if sz == SL then idle 4 else pure ()
    | otherwise = do
        idle 2
        s <- predecRead sz (field op 0)
        d <- predecRead sz reg
        a <- gets (`aReg` reg)
        r <- runAlu (aluX k sz s d)
        case sz of
          SL -> do
            writeData Word (a + 2) (fromIntegral r)
            prefetch
            writeData Word a (fromIntegral (r `shiftR` 16))
          _ -> do
            prefetch
            writeMem False sz a r
  -- CMPM (Ay)+,(Ax)+
  cmpm sz = do
    s <- locate True sz (EPost (field op 0)) >>= readLoc sz
    d <- locate True sz (EPost reg) >>= readLoc sz
    _ <- runAlu (alu Cmp sz s d)
    prefetch
  -- ORI, ANDI, SUBI, ADDI, EORI, CMPI #,<ea>
  immediate = if testBit op 8 then Nothing else case (op `shiftR` 9) .&. 7 of
    0 -> imm Or
    1 -> imm And
    2 -> imm Sub
    3 -> imm Add
    5 -> imm Eor
    6 -> imm Cmp
    _ -> Nothing
  imm k = case (sizeBits op, ea) of
    (Just sz, Just e) | dataAlt e -> Just $ do
      s <- locate True sz EImm >>= readLoc sz
      case e of
        EDn r -> do
          d <- gets (`dReg` r)
          v <- runAlu (alu k sz s (trunc sz d))
          if k == Cmp then pure () else modify (setD r (merge sz d v))
          prefetch
          if sz == SL then idle (if k == Cmp then 2 else 4) else pure ()
        _ | k == Cmp -> do
              d <- locate True sz e >>= readLoc sz
              _ <- runAlu (alu Cmp sz s d)
              prefetch
          | otherwise -> rmw sz e (alu k sz s)
    _ -> Nothing
  -- ADDQ, SUBQ #1-8,<ea> (size 3 is Scc/DBcc)
  quick = case (sizeBits op, ea) of
    (Just sz, Just e) | dataAlt e || (isAnEA e && sz /= SB) -> Just $ do
      let k = if testBit op 8 then Sub else Add
          q = let n = fromIntegral reg in if n == 0 then 8 else n
      case e of
        EAn r -> do
          a <- gets (`aReg` r)
          modify (setA r (if k == Add then a + q else a - q))
          prefetch
          idle (if sz == SW then 4 else 2) -- the tests: .l 6 cycles, .w 8
        EDn r -> do
          d <- gets (`dReg` r)
          v <- runAlu (alu k sz q (trunc sz d))
          modify (setD r (merge sz d v))
          prefetch
          if sz == SL then idle 4 else pure ()
        _ -> rmw sz e (alu k sz q)
    _ -> Nothing
  -- NEGX, CLR, NEG, NOT, TST <ea>
  single = case (op `shiftR` 8) .&. 0xF of
    0x0 -> unary (\sz v -> aluX Sub sz v 0)
    0x2 -> unary (\sz _ -> \c -> (0, logicFlags sz 0 c))
    0x4 -> unary (\sz v -> alu Sub sz v 0)
    0x6 -> unary (\sz v -> \c -> let r = trunc sz (complement v) in (r, logicFlags sz r c))
    0xA -> tst
    _ -> Nothing
  unary f = case (sizeBits op, ea) of
    (Just sz, Just e) | dataAlt e -> Just $ case e of
      EDn r -> do
        d <- gets (`dReg` r)
        v <- runAlu (f sz (trunc sz d))
        modify (setD r (merge sz d v))
        prefetch
        if sz == SL then idle 2 else pure ()
      _ -> rmw sz e (f sz)
    _ -> Nothing
  tst = case (sizeBits op, ea) of
    (Just sz, Just e) | dataAlt e -> Just $ do
      v <- locate True sz e >>= readLoc sz
      modify (logicFlags sz v)
      prefetch
    _ -> Nothing

-- | A -(An) operand read for ADDX/SUBX: a long as two words, the low word
-- first, An down two at a time.
predecRead :: Monad m => Sz -> Int -> X m Word32
predecRead sz r = case sz of
  SL -> do
    a0 <- gets (`aReg` r)
    modify (setA r (a0 - 2))
    l <- readData Word (a0 - 2)
    modify (setA r (a0 - 4))
    h <- readData Word (a0 - 4)
    pure (fromIntegral h `shiftL` 16 .|. fromIntegral l)
  _ -> do
    a <- subtract (stepOf sz r) <$> gets (`aReg` r)
    modify (setA r a)
    readMem sz a

-- * Shifts and rotates

data Shift = As | Ls | Rox | Ro
  deriving stock (Show, Eq)

-- | One shift or rotate, bit by bit (@n@ steps, up to 63): the result and
-- the flags. ASL's V: the sign changed at any step. A count of 0: C
-- cleared (ROXL/ROXR: C = X), X and V as they were (V cleared).
shiftOp :: Shift -> Bool -> Sz -> Int -> Word32 -> Cpu -> (Word32, Cpu)
shiftOp k left sz n v0 c =
  let top = case sz of
        SB -> 7
        SW -> 15
        SL -> 31
      x0 = testBit (cSr c) flagX
      go 0 v cy x ov = (v, cy, x, ov)
      go i v _ x ov =
        let out = if left then testBit v top else testBit v 0
            inBit = case k of
              As -> not left && testBit v top
              Ls -> False
              Rox -> x
              Ro -> out
            v' = trunc sz (if left then (v `shiftL` 1) .|. (if inBit then 1 else 0)
                                   else (v `shiftR` 1) .|. (if inBit then bit top else 0))
            ov' = ov || (k == As && left && testBit v' top /= testBit v top)
        in go (i - 1 :: Int) v' out (if k == Ro then x else out) ov'
      (r, cy0, x1, ov) = go n (trunc sz v0) False x0 False
      -- ASR past the operand's width: C and X cleared, as the tests
      -- record (bit by bit they'd be the sign; an open question for a
      -- hardware check, docs/cpus/m68k.md)
      past = k == As && not left && n > top + 1
      cy = cy0 && not past
      x = x1 && not past
      cFlag = if n == 0 then (k == Rox && x0) else cy
      xs = [(flagX, x) | n > 0, k /= Ro]
  in (r, setFlags (xs <> [(flagN, msb sz r), (flagZ, r == 0), (flagV, ov), (flagC, cFlag)]) c)

-- | ASL, ASR, LSL, LSR, ROXL, ROXR, ROL, ROR: on a data register (by an
-- immediate 1-8 or a register mod 64; 6 + 2n cycles, a long 8 + 2n) or a
-- word in memory (by one).
shifts :: Monad m => Word16 -> Maybe (X m ())
shifts op
  | op .&. 0xF000 /= 0xE000 = Nothing
  | (op `shiftR` 6) .&. 3 == 3 = case (decodeEA (field op 3) (field op 0), (op `shiftR` 11) .&. 1) of
      (Just e, 0) | memAlterable e -> Just $
        rmw SW e (\v -> shiftOp (kind ((op `shiftR` 9) .&. 3)) left SW 1 v)
      _ -> Nothing
  | otherwise = sizeBits op >>= \sz -> Just $ do
      let r = field op 0
          cnt = field op 9
      n <- if testBit op 5 then gets ((.&. 63) . fromIntegral . (`dReg` cnt))
                            else pure (if cnt == 0 then 8 else cnt)
      d <- gets (`dReg` r)
      v <- runAlu (shiftOp (kind ((op `shiftR` 3) .&. 3)) left sz n (trunc sz d))
      modify (setD r (merge sz d v))
      prefetch
      idle ((if sz == SL then 4 else 2) + 2 * n)
 where
  left = testBit op 8
  kind :: Word16 -> Shift
  kind = \case
    0 -> As
    1 -> Ls
    2 -> Rox
    _ -> Ro

-- * Bit operations

data BitOp = BTst | BChg | BClr | BSet
  deriving stock (Show, Eq)

-- | BTST, BCHG, BCLR, BSET: the bit number from a register (0000 rrr
-- 1oo; mode 1 there is MOVEP) or an extension word (0000 1000 oo). On a
-- data register the bit is mod 32, in memory a byte's mod 8. Z: the bit
-- was clear.
bitOps :: Monad m => Word16 -> Maybe (X m ())
bitOps op
  | op .&. 0xF000 /= 0 = Nothing
  | testBit op 8 && field op 3 /= 1 = run False
  | op .&. 0xFF00 == 0x0800 = run True
  | otherwise = Nothing
 where
  k = case (op `shiftR` 6) .&. 3 of
    0 -> BTst
    1 -> BChg
    2 -> BClr
    _ -> BSet
  run static = case decodeEA (field op 3) (field op 0) of
    Just e | ok static e -> Just $ do
      b <- if static then fromIntegral . (.&. 0xFF) <$> extWord else gets (`dReg` field op 9)
      case e of
        EDn r -> do
          let n = fromIntegral (b .&. 31) :: Int
          d <- gets (`dReg` r)
          modify (setFlags [(flagZ, not (testBit d n))] . setD r (apply n d))
          prefetch
          idle (regIdle n)
        _ -> do
          let n = fromIntegral (b .&. 7) :: Int
          l <- locate True SB e
          d <- readLoc SB l
          modify (setFlags [(flagZ, not (testBit d n))])
          prefetch
          case l of
            LImm _ -> idle 2 -- BTST Dn,#imm
            _ -> pure ()
          if k == BTst then pure () else writeLoc True SB l (apply n d)
    _ -> Nothing
  ok static = \case
    EAn _ -> False
    EImm -> k == BTst && not static
    EPcDisp -> k == BTst
    EPcIdx -> k == BTst
    _ -> True
  apply n d = case k of
    BTst -> d
    BChg -> complementBit d n
    BClr -> clearBit d n
    BSet -> setBit d n
  -- on a register: BTST 2, BCHG/BSET 2 (4 for bits 16-31), BCLR 4 (6)
  regIdle n = case k of
    BTst -> 2
    BClr -> if n < 16 then 4 else 6
    _ -> if n < 16 then 2 else 4

-- * Exceptions (groups 1 and 2)

-- | Exception processing for vector @vec@ with @pc@ in the frame: the
-- idle cycles before it (@pre@), supervisor mode (T cleared), the 6-byte
-- frame (PC low, SR, PC high: the chip's order), the vector, the queue
-- filled at the handler.
exception :: Monad m => Int -> Word32 -> Word32 -> X m ()
exception pre vec pc = do
  idle pre
  c <- get
  let sr0 = cSr c
  modify (\x -> x { cSr = (sr0 .|. 0x2000) .&. 0x7FFF })
  sp <- gets cSsp
  writeData Word (sp - 2) (fromIntegral pc)
  writeData Word (sp - 6) sr0
  writeData Word (sp - 4) (fromIntegral (pc `shiftR` 16))
  modify (\x -> x { cSsp = sp - 6 })
  vector vec

-- | A privileged instruction in user mode: vector 8, its own address in
-- the frame.
privileged :: Monad m => X m () -> X m ()
privileged x = do
  s <- gets supervisor
  if s then x else gets cPc >>= exception 4 8

-- | The queue refilled from the next instruction (after the SR changed:
-- its function code may have): IR and IRC read again from pc + 2.
refill :: Monad m => X m ()
refill = do
  pc <- gets ((+ 2) . cPc)
  ir <- readProg pc
  irc <- readProg (pc + 2)
  modify (\x -> x { cPc = pc, cIr = ir, cIrc = irc })

-- | The status register as written: T, S, the interrupt mask, the CCR.
srMask :: Word16
srMask = 0xA71F

setSr :: Monad m => Word16 -> X m ()
setSr v = modify (\x -> x { cSr = v .&. srMask })

setCcr :: Monad m => Word16 -> X m ()
setCcr v = modify (\x -> x { cSr = (cSr x .&. 0xFF00) .|. (v .&. 0x1F) })

-- | The condition codes' 16 conditions (Bcc, DBcc, Scc).
cond :: Int -> Word16 -> Bool
cond n sr =
  let c = testBit sr flagC
      v = testBit sr flagV
      z = testBit sr flagZ
      nn = testBit sr flagN
  in case n of
       0 -> True
       1 -> False
       2 -> not c && not z -- HI
       3 -> c || z -- LS
       4 -> not c -- CC
       5 -> c -- CS
       6 -> not z -- NE
       7 -> z -- EQ
       8 -> not v -- VC
       9 -> v -- VS
       10 -> not nn -- PL
       11 -> nn -- MI
       12 -> nn == v -- GE
       13 -> nn /= v -- LT
       14 -> not z && nn == v -- GT
       _ -> z || nn /= v -- LE

-- | Push a long on the active stack: the high word at sp - 4 first.
pushLong :: Monad m => Word32 -> X m ()
pushLong v = do
  sp <- subtract 4 <$> gets (`aReg` 7)
  writeData Word sp (fromIntegral (v `shiftR` 16))
  writeData Word (sp + 2) (fromIntegral v)
  modify (setA 7 sp)

-- | The address an EA names (LEA, PEA, JMP, JSR): its extension words and
-- index cycles, no operand access.
addressOf :: Monad m => EA -> X m (Maybe Word32)
addressOf e
  | controlEA e = locate False SL e >>= \case
      LMem a -> pure (Just a)
      LPost _ a -> pure (Just a)
      _ -> pure Nothing
  | otherwise = pure Nothing

-- | The control addressing modes.
controlEA :: EA -> Bool
controlEA = \case
  EInd _ -> True
  EDisp _ -> True
  EIdx _ -> True
  EAbsW -> True
  EAbsL -> True
  EPcDisp -> True
  EPcIdx -> True
  _ -> False

isIdx :: EA -> Bool
isIdx = \case
  EIdx _ -> True
  EPcIdx -> True
  _ -> False

-- * The system and register instructions

-- | MOVE to/from SR, CCR and USP; ANDI, ORI, EORI to CCR and SR; LEA, PEA,
-- LINK, UNLK, EXG, SWAP, EXT, Scc, TAS, TRAP, TRAPV, CHK.
system :: Monad m => Word16 -> Maybe (X m ())
system op = case op of
  0x003C -> Just (ccrImm (.|.))
  0x023C -> Just (ccrImm (.&.))
  0x0A3C -> Just (ccrImm xor)
  0x007C -> Just (srImm (.|.))
  0x027C -> Just (srImm (.&.))
  0x0A7C -> Just (srImm xor)
  0x4E76 -> Just $ do -- TRAPV
    v <- gets (\c -> testBit (cSr c) flagV)
    if v
      then do
        pc <- gets ((+ 2) . cPc)
        _ <- extWord' -- the prefetch's read, then the exception
        exception 0 7 pc
      else prefetch
  _ | op .&. 0xFFF0 == 0x4E40 -> Just $ do -- TRAP #n
        pc <- gets ((+ 2) . cPc)
        exception 4 (32 + fromIntegral (op .&. 15)) pc
    | op .&. 0xFFF8 == 0x4E50 -> Just (link (field op 0))
    | op .&. 0xFFF8 == 0x4E58 -> Just (unlk (field op 0))
    | op .&. 0xFFF8 == 0x4E60 -> Just $ privileged $ do -- MOVE An,USP
        a <- gets (`aReg` field op 0)
        modify (\x -> x { cUsp = a })
        prefetch
    | op .&. 0xFFF8 == 0x4E68 -> Just $ privileged $ do -- MOVE USP,An
        u <- gets cUsp
        modify (setA (field op 0) u)
        prefetch
    | op .&. 0xFFF8 == 0x4840 -> Just $ do -- SWAP
        let r = field op 0
        d <- gets (`dReg` r)
        let v = (d `shiftL` 16) .|. (d `shiftR` 16)
        modify (logicFlags SL v . setD r v)
        prefetch
    | op .&. 0xFFF8 == 0x4880 -> Just $ do -- EXT.w
        let r = field op 0
        d <- gets (`dReg` r)
        let v = sext8 d .&. 0xFFFF
        modify (logicFlags SW v . setD r (merge SW d v))
        prefetch
    | op .&. 0xFFF8 == 0x48C0 -> Just $ do -- EXT.l
        let r = field op 0
        d <- gets (`dReg` r)
        let v = sext16 d
        modify (logicFlags SL v . setD r v)
        prefetch
    | op .&. 0xFFC0 == 0x4840, Just e <- ea, controlEA e -> Just $ do -- PEA
        a <- maybe 0 id <$> addressOf e
        if isIdx e then idle 2 else pure ()
        case e of
          EAbsW -> pushLong a >> prefetch
          EAbsL -> pushLong a >> prefetch
          _ -> prefetch >> pushLong a
    | op .&. 0xF1C0 == 0x41C0, Just e <- ea, controlEA e -> Just $ do -- LEA
        a <- maybe 0 id <$> addressOf e
        if isIdx e then idle 2 else pure ()
        modify (setA (field op 9) a)
        prefetch
    | op .&. 0xF1C0 == 0x4180, Just e <- ea, dataEA e -> Just (chk e)
    | op .&. 0xFFC0 == 0x40C0, Just e <- ea, dataAlt e -> Just $ do -- MOVE from SR
        sr <- gets cSr
        case e of
          EDn r -> do
            modify (\c -> setD r (merge SW (dReg c r) (fromIntegral sr)) c)
            prefetch
            idle 2
          _ -> rmw SW e (\_ c -> (fromIntegral sr, c))
    | op .&. 0xFFC0 == 0x44C0, Just e <- ea, dataEA e -> Just $ do -- MOVE to CCR
        v <- locate True SW e >>= readLoc SW
        setCcr (fromIntegral v)
        idle 4
        refill
    | op .&. 0xFFC0 == 0x46C0, Just e <- ea, dataEA e -> Just $ privileged $ do -- MOVE to SR
        v <- locate True SW e >>= readLoc SW
        setSr (fromIntegral v)
        idle 4
        refill
    | op .&. 0xF0C0 == 0x50C0, Just e <- ea, dataAlt e -> Just $ do -- Scc
        t <- gets (cond (fromIntegral ((op `shiftR` 8) .&. 15)) . cSr)
        let v = if t then 0xFF else 0
        case e of
          EDn r -> do
            modify (\c -> setD r (merge SB (dReg c r) v) c)
            prefetch
            if t then idle 2 else pure ()
          _ -> rmw SB e (\_ c -> (v, c))
    | op .&. 0xFFC0 == 0x4AC0, op /= 0x4AFC, Just e <- ea, dataAlt e -> Just $ case e of -- TAS
        EDn r -> do
          d <- gets (`dReg` r)
          modify (logicFlags SB d . setD r (d .|. 0x80))
          prefetch
        _ -> do
          l <- locate True SB e
          a <- case l of
            LMem a -> pure a
            LPost r a -> modify (setA r (a + stepOf SB r)) >> pure a
            _ -> pure 0
          fc <- gets dataFc
          v <- bus (\b -> busTas b fc (mask24 a) (.|. 0x80))
          modify (logicFlags SB (fromIntegral v))
          prefetch
    | op .&. 0xF130 == 0xC100, (op `shiftR` 3) .&. 0x1F `elem` [0x08, 0x09, 0x11] -> Just $ do -- EXG
        let x = field op 9
            y = field op 0
        c <- get
        let (gx, sx, gy, sy) = case (op `shiftR` 3) .&. 0x1F of
              0x08 -> (dReg c x, setD x, dReg c y, setD y)
              0x09 -> (aReg c x, setA x, aReg c y, setA y)
              _ -> (dReg c x, setD x, aReg c y, setA y)
        modify (sy gx . sx gy)
        prefetch
        idle 2
    | otherwise -> Nothing
 where
  ea = decodeEA (field op 3) (field op 0)
  dataEA = \case
    EAn _ -> False
    _ -> True
  extWord' = extWord
  ccrImm f = do
    v <- extWord
    sr <- gets cSr
    setCcr (f sr v)
    idle 8
    refill
  srImm f = privileged $ do
    v <- extWord
    sr <- gets cSr
    setSr (f sr v)
    idle 8
    refill
  link r = do
    d <- extWord
    a <- gets (`aReg` r)
    sp <- subtract 4 <$> gets (`aReg` 7)
    -- LINK A7 pushes the decremented A7
    let v = if r == 7 then sp else a
    writeData Word sp (fromIntegral (v `shiftR` 16))
    writeData Word (sp + 2) (fromIntegral v)
    modify (setA 7 (sp + sext16 (fromIntegral d)) . setA r sp)
    prefetch
  unlk r = do
    a <- gets (`aReg` r)
    h <- readData Word a
    l <- readData Word (a + 2)
    modify (setA r (fromIntegral h `shiftL` 16 .|. fromIntegral l) . setA 7 (a + 4))
    prefetch
  -- CHK <ea>,Dn: Dn.w below 0 or above the bound: vector 6
  chk e = do
    b <- locate True SW e >>= readLoc SW
    d <- gets ((.&. 0xFFFF) . (`dReg` field op 9))
    let ds = fromIntegral (fromIntegral d :: Word16) :: Int16
        bs = fromIntegral (fromIntegral b :: Word16) :: Int16
    pc <- gets ((+ 2) . cPc)
    prefetch
    -- above the bound first (N cleared, 4 idle cycles), then below zero
    -- (N set, 6); V and C cleared, Z too when it traps (as the tests)
    if ds > bs
      then modify (setFlags [(flagN, ds < 0), (flagZ, ds == 0), (flagV, False), (flagC, False)]) >> exception 4 6 pc
      else if ds < 0
        then modify (setFlags [(flagN, True), (flagZ, ds == 0), (flagV, False), (flagC, False)]) >> exception 6 6 pc
        else modify (setFlags [(flagZ, ds == 0), (flagV, False), (flagC, False)]) >> idle 6

-- * Branches, jumps and returns

-- | The queue filled at a new PC: IR, then IRC. An odd target faults on
-- the first fetch with the target less 4 as the frame's PC (the tests).
jumpTo :: Monad m => Word32 -> X m ()
jumpTo t = do
  modify (\x -> x { cPc = t - 4 })
  ir <- readProg t
  irc <- readProg (t + 2)
  modify (\x -> x { cPc = t, cIr = ir, cIrc = irc })

-- | Past an extension word without using it (an untaken branch's).
skipExt :: Monad m => X m ()
skipExt = () <$ extWord

-- | JMP and JSR's target: the extension words but the last taken, the
-- last read from IRC where it is (no fetch past it); the idle cycles of
-- the address calculation.
jumpTarget :: Monad m => EA -> X m Word32
jumpTarget = \case
  EInd r -> gets (`aReg` r)
  EDisp r -> do
    idle 2
    d <- gets cIrc
    (+ sext16 (fromIntegral d)) <$> gets (`aReg` r)
  EIdx r -> do
    idle 6
    e <- gets cIrc
    a <- gets (`aReg` r)
    indexed a e
  EAbsW -> do
    idle 2
    sext16 . fromIntegral <$> gets cIrc
  EAbsL -> do
    h <- extWord
    l <- gets cIrc
    pure (fromIntegral h `shiftL` 16 .|. fromIntegral l)
  EPcDisp -> do
    idle 2
    c <- get
    pure (cPc c + 2 + sext16 (fromIntegral (cIrc c)))
  EPcIdx -> do
    idle 6
    c <- get
    indexed (cPc c + 2) (cIrc c)
  _ -> pure 0

-- | The extension words a control mode has (JSR's return address).
extWords :: EA -> Word32
extWords = \case
  EInd _ -> 0
  EAbsL -> 2
  _ -> 1

-- | Bcc, BRA, BSR, DBcc, JMP, JSR, RTS, RTR, RTE, RESET.
branches :: Monad m => Word16 -> Maybe (X m ())
branches op
  | op .&. 0xF000 == 0x6000 = Just $ do
      c <- get
      let cc = fromIntegral ((op `shiftR` 8) .&. 15) :: Int
          d8 = op .&. 0xFF
          base = cPc c + 2
          disp = if d8 == 0 then sext16 (fromIntegral (cIrc c)) else sext8 (fromIntegral d8)
          next = base + (if d8 == 0 then 2 else 0)
      case cc of
        1 -> do -- BSR
          idle 2
          pushLong next
          jumpTo (base + disp)
        _ | cc == 0 || cond cc (cSr c) -> do
              idle 2
              jumpTo (base + disp)
          | otherwise -> do
              idle 4
              if d8 == 0 then skipExt else pure ()
              prefetch
  | op .&. 0xF0F8 == 0x50C8 = Just $ do -- DBcc Dn,d16
      c <- get
      let r = field op 0
          base = cPc c + 2
          disp = sext16 (fromIntegral (cIrc c))
      if cond (fromIntegral ((op `shiftR` 8) .&. 15)) (cSr c)
        then do
          idle 4
          skipExt
          prefetch
        else do
          let d = dReg c r
              w = (d - 1) .&. 0xFFFF
          modify (setD r (merge SW d w))
          if w /= 0xFFFF
            then idle 2 >> jumpTo (base + disp)
            else do
              -- the counter expired: 14 cycles (YACHT: n np np np), the
              -- branch target's word fetched and thrown away first. Not in
              -- the SingleStepTests (no case expires): the target read's
              -- address is unverified (docs/cpus/m68k.md)
              idle 2
              _ <- readProg (base + disp)
              skipExt
              prefetch
  | op .&. 0xFFC0 == 0x4EC0, Just e <- ea, controlEA e = Just (jumpTarget e >>= jumpTo) -- JMP
  | op .&. 0xFFC0 == 0x4E80, Just e <- ea, controlEA e = Just $ do -- JSR
      pc <- gets cPc
      t <- jumpTarget e
      let ret = pc + 2 + 2 * extWords e
      modify (\x -> x { cPc = t - 4 })
      ir <- readProg t
      pushLong ret
      irc <- readProg (t + 2)
      modify (\x -> x { cPc = t, cIr = ir, cIrc = irc })
  | op == 0x4E75 = Just $ do -- RTS
      sp <- gets (`aReg` 7)
      h <- readData Word sp
      l <- readData Word (sp + 2)
      modify (setA 7 (sp + 4))
      jumpTo (fromIntegral h `shiftL` 16 .|. fromIntegral l)
  | op == 0x4E77 = Just $ do -- RTR: the PC's high word, the CCR, the PC's low word
      sp <- gets (`aReg` 7)
      h <- readData Word (sp + 2)
      ccr <- readData Word sp
      l <- readData Word (sp + 4)
      modify (setA 7 (sp + 6))
      setCcr ccr
      jumpTo (fromIntegral h `shiftL` 16 .|. fromIntegral l)
  | op == 0x4E73 = Just $ privileged $ do -- RTE: as RTR, the whole SR
      sp <- gets cSsp
      h <- readData Word (sp + 2)
      sr <- readData Word sp
      l <- readData Word (sp + 4)
      modify (\x -> x { cSsp = sp + 6 })
      setSr sr
      jumpTo (fromIntegral h `shiftL` 16 .|. fromIntegral l)
  | op == 0x4E70 = Just $ privileged $ do -- RESET: the RESET line for 124 clocks
      idle 128
      prefetch
  | otherwise = Nothing
 where
  ea = decodeEA (field op 3) (field op 0)

-- * Multiply, BCD, MOVEP

-- | MULU/MULS <ea>,Dn (16 x 16 -> 32): 38 + 2n cycles, n the source's 1
-- bits (MULU) or its 01 and 10 pairs, a 0 below bit 0 (MULS).
mulOp :: Monad m => Bool -> Int -> EA -> X m ()
mulOp signed r e = do
  s <- locate True SW e >>= readLoc SW
  d <- gets ((.&. 0xFFFF) . (`dReg` r))
  let v = if signed then sext16 s * sext16 d else s * d
      n = if signed then popCount ((s `xor` (s `shiftL` 1)) .&. 0xFFFF) else popCount s
  modify (logicFlags SL v . setD r v)
  prefetch
  idle (34 + 2 * n)

-- | ABCD and SBCD on a byte with X (NBCD: SBCD from 0), after flamewing's
-- analysis of the chip's BCD logic: the binary result, its per-nibble
-- carries (and ABCD's decimal carries) giving the correction; X and C
-- the decimal carry; Z only cleared; N bit 7; V the correction's effect
-- on bit 7.
bcd :: Bool -> Word32 -> Word32 -> Cpu -> (Word32, Cpu)
bcd add s0 d0 c =
  let x = if testBit (cSr c) flagX then 1 else 0 :: Int
      s = fromIntegral (s0 .&. 0xFF) :: Int
      d = fromIntegral (d0 .&. 0xFF) :: Int
      (rr, cy, v)
        | add =
            let ss = s + d + x
                bc = ((s .&. d) .|. (complement ss .&. d) .|. (s .&. complement ss)) .&. 0x88
                dc = (((ss + 0x66) `xor` ss) .&. 0x110) `shiftR` 1
                corf = (bc .|. dc) - ((bc .|. dc) `shiftR` 2)
                r = ss + corf
            in (r, testBit (bc .|. (ss .&. complement r)) 7, testBit (complement ss .&. r) 7)
        | otherwise =
            let dd = d - s - x
                bc = ((complement d .&. s) .|. (dd .&. complement d) .|. (dd .&. s)) .&. 0x88
                corf = bc - (bc `shiftR` 2)
                r = dd - corf
            in (r, testBit (bc .|. (complement dd .&. r)) 7, testBit (dd .&. complement r) 7)
      res = fromIntegral (rr .&. 0xFF) :: Word32
      z0 = testBit (cSr c) flagZ
  in ( res
     , setFlags [ (flagX, cy), (flagC, cy), (flagZ, z0 && res == 0), (flagN, testBit res 7)
                , (flagV, v) ] c )

-- | MOVEP: Dn to or from alternate bytes at d16(An) (a word: 2, a long:
-- 4, high byte first).
movep :: Monad m => Word16 -> X m ()
movep op = do
  let dr = field op 9
      ar = field op 0
      toMem = testBit op 7
      n = if testBit op 6 then 4 else 2 :: Int
  d16 <- extWord
  a <- (+ sext16 (fromIntegral d16)) <$> gets (`aReg` ar)
  if toMem
    then do
      v <- gets (`dReg` dr)
      mapM_ (\i -> writeData Byte (a + 2 * fromIntegral i)
                     (fromIntegral (v `shiftR` (8 * (n - 1 - i))) .&. 0xFF)) [0 .. n - 1]
    else do
      bs <- mapM (\i -> fromIntegral . (.&. 0xFF) <$> readData Byte (a + 2 * fromIntegral i)) [0 .. n - 1]
      let v = foldl (\acc b -> (acc `shiftL` 8) .|. b) 0 bs :: Word32
      old <- gets (`dReg` dr)
      modify (setD dr (if n == 2 then merge SW old v else v))
  prefetch

-- | MULU, MULS, ABCD, SBCD, NBCD, MOVEP.
mulBcd :: Monad m => Word16 -> Maybe (X m ())
mulBcd op
  | op .&. 0xF0C0 == 0xC0C0, Just e <- ea, notAn e = Just (mulOp (testBit op 8) (field op 9) e)
  | op .&. 0xF1F0 == 0xC100 = Just (bcdOp True)
  | op .&. 0xF1F0 == 0x8100 = Just (bcdOp False)
  | op .&. 0xFFC0 == 0x4800, Just e <- ea, dataAlt e = Just $ case e of -- NBCD
      EDn r -> do
        d <- gets (`dReg` r)
        v <- runAlu (bcd False d 0)
        modify (setD r (merge SB d v))
        prefetch
        idle 2
      _ -> rmw SB e (\d -> bcd False d 0)
  | op .&. 0xF138 == 0x0108 = Just (movep op)
  | otherwise = Nothing
 where
  ea = decodeEA (field op 3) (field op 0)
  notAn = \case
    EAn _ -> False
    _ -> True
  -- ABCD/SBCD Dy,Dx or -(Ay),-(Ax)
  bcdOp add
    | not (testBit op 3) = do
        let rx = field op 9
        s <- gets (`dReg` field op 0)
        d <- gets (`dReg` rx)
        v <- runAlu (bcd add s d)
        modify (setD rx (merge SB d v))
        prefetch
        idle 2
    | otherwise = do
        idle 2
        s <- predecRead SB (field op 0)
        d <- predecRead SB (field op 9)
        a <- gets (`aReg` field op 9)
        v <- runAlu (bcd add s d)
        prefetch
        writeMem False SB a v

-- * Divide

-- | DIVU's cycles (Jorge Cwik's analysis of the chip's division): 10 on
-- overflow, else the 15 steps' cost.
divuCycles :: Word32 -> Word32 -> Int
divuCycles dividend divisor
  | (dividend `shiftR` 16) >= divisor = 10
  | otherwise = go (15 :: Int) dividend 38 * 2
 where
  hdivisor = divisor `shiftL` 16
  go 0 _ m = m
  go i dv m =
    let dv' = dv `shiftL` 1
    in if testBit dv 31
         then go (i - 1) (dv' - hdivisor) m
         else if dv' >= hdivisor
           then go (i - 1) (dv' - hdivisor) (m + 1)
           else go (i - 1) dv' (m + 2)

-- | DIVS's cycles (the same analysis).
divsCycles :: Int32 -> Int16 -> Int
divsCycles dividend divisor =
  let m0 = 6 + (if dividend < 0 then 1 else 0) :: Int
      adiv = fromIntegral (abs (fromIntegral dividend :: Integer)) :: Word32
      adsr = fromIntegral (abs (fromIntegral divisor :: Integer)) :: Word32
      q = fromIntegral dividend `quot` fromIntegral divisor :: Integer
  -- any overflow takes the short path, a signed one too (the tests)
  in if (adiv `shiftR` 16) >= adsr || q > 32767 || q < -32768
       then (m0 + 2) * 2
       else
         let aq = adiv `div` adsr
             m1 = m0 + 55 + (if divisor >= 0 then (if dividend >= 0 then -1 else 1) else 0)
             m2 = m1 + length [ () | i <- [0 .. 14 :: Int], not (testBit (aq `shiftL` i) 15) ]
         in m2 * 2

-- | DIVU/DIVS <ea>,Dn: 32 / 16 -> the quotient in the low word, the
-- remainder in the high; a zero divisor: vector 5; overflow: V set, Dn
-- unchanged.
divOp :: Monad m => Bool -> Int -> EA -> X m ()
divOp signed r e = do
  s <- locate True SW e >>= readLoc SW
  d <- gets (`dReg` r)
  pc <- gets ((+ 2) . cPc)
  if s == 0
    then do
      modify (setFlags [(flagN, False), (flagZ, False), (flagV, False), (flagC, False)])
      idle 4
      exception 4 5 pc
    else if not signed
      then do
        let q = d `div` s
            rm = d `mod` s
        idle (divuCycles d s - 4)
        if q > 0xFFFF
          then modify (setFlags [(flagV, True), (flagC, False)])
          else modify (setFlags [(flagN, testBit q 15), (flagZ, q == 0), (flagV, False), (flagC, False)]
                       . setD r ((rm `shiftL` 16) .|. q))
        prefetch
      else do
        let dv = fromIntegral d :: Int32
            ds = fromIntegral (fromIntegral s :: Word16) :: Int16
            q = fromIntegral dv `quot` fromIntegral ds :: Integer
            rm = fromIntegral dv `rem` fromIntegral ds :: Integer
        idle (divsCycles dv ds - 4)
        if q > 32767 || q < -32768
          then modify (setFlags [(flagV, True), (flagC, False)])
          else do
            let qw = fromIntegral q .&. 0xFFFF :: Word32
                rw = fromIntegral rm .&. 0xFFFF :: Word32
            modify (setFlags [(flagN, testBit qw 15), (flagZ, qw == 0), (flagV, False), (flagC, False)]
                    . setD r ((rw `shiftL` 16) .|. qw))
        prefetch

-- | DIVU, DIVS.
divide :: Monad m => Word16 -> Maybe (X m ())
divide op
  | op .&. 0xF0C0 == 0x80C0, Just e <- decodeEA (field op 3) (field op 0), notAn e =
      Just (divOp (testBit op 8) (field op 9) e)
  | otherwise = Nothing
 where
  notAn = \case
    EAn _ -> False
    _ -> True

-- * MOVEM

-- | Register i of MOVEM's list order: D0-D7, then A0-A7.
listReg :: Cpu -> Int -> Word32
listReg c i = if i < 8 then dReg c i else aReg c (i - 8)

setListReg :: Int -> Word32 -> Cpu -> Cpu
setListReg i v = if i < 8 then setD i v else setA (i - 8) v

-- | MOVEM registers <-> memory: the mask first, then the address's
-- extension words. To memory: D0 up to A7 (a long's high word first), or
-- with -(An) A7 down to D0 at falling addresses (mask bit 0 is A7; a
-- long's low word first; An as it was before the instruction if it is in
-- the list). From memory: words sign-extended into the whole register,
-- one more word read past the last, (An)+ left past the last register.
movem :: Monad m => Word16 -> Maybe (X m ())
movem op
  | op .&. 0xFB80 /= 0x4880 = Nothing
  | Just e <- decodeEA (field op 3) (field op 0), ok e = Just $ do
      mask <- extWord
      if toMem then store mask e else load mask e
      prefetch
  | otherwise = Nothing
 where
  toMem = not (testBit op 10)
  sz = if testBit op 6 then SL else SW
  ok e
    | toMem = case e of
        EPre _ -> True
        _ -> controlEA e && not (pcRel e)
    | otherwise = case e of
        EPost _ -> True
        _ -> controlEA e
  pcRel = \case
    EPcDisp -> True
    EPcIdx -> True
    _ -> False
  store mask = \case
    EPre r -> do
      c0 <- get
      a0 <- gets (`aReg` r)
      let go a i
            | i > 15 = pure a
            | not (testBit mask i) = go a (i + 1)
            | otherwise = do
                let v = listReg c0 (15 - i)
                a' <- case sz of
                  SL -> do
                    writeData Word (a - 2) (fromIntegral v)
                    writeData Word (a - 4) (fromIntegral (v `shiftR` 16))
                    pure (a - 4)
                  _ -> do
                    writeData Word (a - 2) (fromIntegral v)
                    pure (a - 2)
                modify (setA r a')
                go a' (i + 1)
      _ <- go a0 0
      pure ()
    e -> do
      a0 <- addrOf e
      c0 <- get
      let go _ i | i > 15 = pure ()
          go a i
            | not (testBit mask i) = go a (i + 1)
            | otherwise = do
                writeMem False sz a (listReg c0 i)
                go (a + bytes sz) (i + 1)
      go a0 0
  load mask e = do
    a0 <- case e of
      EPost r -> do
        a <- gets (`aReg` r)
        -- a fault on the first read leaves An two on (the tests)
        modify (setA r (a + 2))
        pure a
      _ -> addrOf e
    let go a i
          | i > 15 = pure a
          | not (testBit mask i) = go a (i + 1)
          | otherwise = do
              v <- readMem sz a
              modify (setListReg i (if sz == SW then sext16 v else v))
              go (a + bytes sz) (i + 1)
    end <- go a0 0
    _ <- readData Word end
    case e of
      EPost r -> modify (setA r end)
      _ -> pure ()
  addrOf e = locate False SW e >>= \case
    LMem a -> pure a
    LPost _ a -> pure a
    _ -> pure 0

-- * STOP and the illegal instructions (not in the SingleStepTests: their
-- timing follows the privilege violation's and the manual, untested)

-- | STOP #imm (privileged): the immediate into SR, the queue moved on to
-- the next instruction, then nothing until an interrupt or a reset
-- ('cStopped').
stop :: Monad m => X m ()
stop = privileged $ do
  v <- extWord
  setSr v
  prefetch
  modify (\x -> x { cStopped = True })

-- | An opcode the 68000 doesn't have: line 1010 (vector 10), line 1111
-- (11), anything else (ILLEGAL, 0x4AFC, included) vector 4; the
-- instruction's own address in the frame.
illegal :: Monad m => Word16 -> X m ()
illegal op = do
  pc <- gets cPc
  exception 4 (case op `shiftR` 12 of
                 0xA -> 10
                 0xF -> 11
                 _ -> 4) pc
