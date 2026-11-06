{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-|
The 68000 reference model as a library (side track P, P.3; for the user's
benchmark project, docs/cpus/m68k.md): a machine with 16 MiB of memory
(the 24-bit bus, no devices), an image loaded into it, the reset, and runs
counted in instructions and bus cycles, with an optional trace.

A run's cycles are the model's: four per read or write, ten per TAS, the
internal cycles as the chip spends them, no wait states. The reset
sequence itself isn't counted (its timing isn't modelled yet).

> m <- newMachine
> loadImage m 0 image                -- vectors at 0: SSP, then PC
> resetCpu m
> s <- run m (stopAt 1000000)        -- until STOP, an error, or 10^6 cycles
> print (statCycles s, statInstructions s)
-}
module M68k.Run
  ( -- * The machine
    Machine
  , newMachine
  , loadImage
  , peekByte
  , pokeByte
  , readCpu
  , writeCpu
  , resetCpu
  , startAt
    -- * Running
  , Limit (..)
  , stopAt
  , Stop (..)
  , Stats (..)
  , stepOne
  , run
    -- * Tracing
  , Event (..)
  , setTrace
    -- * Interrupts
  , setIpl
  , setIack
  , setBerr
  ) where

import Prelude

import Control.Monad (forM_)
import Data.Array.IO (IOUArray, newArray, readArray, writeArray)
import Data.Array.Unboxed (listArray)
import Data.Bits
import qualified Data.ByteString as B
import Data.IORef
import Data.Word

import M68k.Spec

-- | One bus event of a trace (the model's, in order), or the start of an
-- instruction (its address and opcode).
data Event
  = Instruction !Word32 !Word16
  | Read !Fc !Size !Word32 !Word16
  | Write !Fc !Size !Word32 !Word16
  | Tas !Fc !Word32 !Word8
  | Idle !Int
  | Iack !Int !(Maybe Word8) -- ^ the acknowledge cycle: the level, the vector (Nothing: autovector)
  deriving stock (Show, Eq)

data Machine = Machine
  { mMem    :: IOUArray Int Word8 -- ^ 16 MiB, the 24-bit bus
  , mCpu    :: IORef Cpu
  , mCycles :: IORef Int -- ^ since the machine was made
  , mTrace  :: IORef (Maybe (Event -> IO ()))
  , mIpl    :: IORef Int -- ^ the interrupt level the CPU sees ('setIpl')
  , mIack   :: IORef (Int -> IO (Maybe Word8)) -- ^ the acknowledge's answer ('setIack')
  , mBerr   :: IORef (Word32 -> Bool) -- ^ addresses with no device: a bus error ('setBerr')
  , mLastBerr :: IORef Bool -- ^ the last bus cycle's BERR
  }

-- | The addresses (24-bit) where no device answers: an access there ends
-- with BERR, a bus error (vector 2). None by default.
setBerr :: Machine -> (Word32 -> Bool) -> IO ()
setBerr m = writeIORef (mBerr m)

-- | The interrupt level on the CPU's IPL lines (0: none), as a device
-- would drive it; sampled before each instruction.
setIpl :: Machine -> Int -> IO ()
setIpl m = writeIORef (mIpl m)

-- | What the acknowledge cycle answers for a level: a vector number, or
-- Nothing for an autovector (the default for every level).
setIack :: Machine -> (Int -> IO (Maybe Word8)) -> IO ()
setIack m = writeIORef (mIack m)

memSize :: Int
memSize = 0x100_0000

-- | A machine with zeroed memory and a CPU that hasn't been reset.
newMachine :: IO Machine
newMachine = do
  mem <- newArray (0, memSize - 1) 0
  cpu <- newIORef Cpu
    { cD = listArray (0, 7) (replicate 8 0), cA = listArray (0, 6) (replicate 7 0)
    , cUsp = 0, cSsp = 0, cSr = 0x2700, cPc = 0, cIr = 0, cIrc = 0, cStopped = False, cIplSeen = 0 }
  Machine mem cpu <$> newIORef 0 <*> newIORef Nothing <*> newIORef 0 <*> newIORef (\_ -> pure Nothing)
    <*> newIORef (const False) <*> newIORef False

-- | An image into memory at an address (bytes past the top wrap).
loadImage :: Machine -> Word32 -> B.ByteString -> IO ()
loadImage m at bs = forM_ (zip [0 ..] (B.unpack bs)) $ \(i, b) -> pokeByte m (at + i) b

peekByte :: Machine -> Word32 -> IO Word8
peekByte m a = readArray (mMem m) (fromIntegral (a .&. 0xFF_FFFF))

pokeByte :: Machine -> Word32 -> Word8 -> IO ()
pokeByte m a = writeArray (mMem m) (fromIntegral (a .&. 0xFF_FFFF))

readCpu :: Machine -> IO Cpu
readCpu = readIORef . mCpu

writeCpu :: Machine -> Cpu -> IO ()
writeCpu m = writeIORef (mCpu m)

-- | The 68000's reset: supervisor mode, interrupts masked (SR 0x2700),
-- SSP from address 0, PC from address 4, the queue filled there.
resetCpu :: Machine -> IO ()
resetCpu m = do
  let long a = do
        bs <- mapM (peekByte m) [a .. a + 3]
        pure (foldl (\acc b -> acc `shiftL` 8 .|. fromIntegral b) 0 bs :: Word32)
      word a = (\h l -> fromIntegral h `shiftL` 8 .|. fromIntegral l) <$> peekByte m a <*> peekByte m (a + 1)
  ssp <- long 0
  pc <- long 4
  ir <- word pc
  irc <- word (pc + 2)
  modifyIORef' (mCpu m) $ \c -> c { cSsp = ssp, cPc = pc, cSr = 0x2700, cIr = ir, cIrc = irc, cStopped = False, cIplSeen = 0 }

-- | Start at @pc@ with the supervisor stack at @ssp@ (an image without
-- vectors): as the reset, without reading them.
startAt :: Machine -> Word32 -> Word32 -> IO ()
startAt m pc ssp = do
  let word a = (\h l -> fromIntegral h `shiftL` 8 .|. fromIntegral l) <$> peekByte m a <*> peekByte m (a + 1)
  ir <- word pc
  irc <- word (pc + 2)
  modifyIORef' (mCpu m) $ \c -> c { cSsp = ssp, cPc = pc, cSr = 0x2700, cIr = ir, cIrc = irc, cStopped = False, cIplSeen = 0 }

setTrace :: Machine -> Maybe (Event -> IO ()) -> IO ()
setTrace m = writeIORef (mTrace m)

-- | The bus over the machine's memory, counting cycles and tracing.
busOf :: Machine -> BusOps IO
busOf m = BusOps
  { busRead = \fc sz a -> do
      v <- case sz of
        Byte -> fromIntegral <$> peekByte m a
        Word -> (\h l -> fromIntegral h `shiftL` 8 .|. fromIntegral l) <$> peekByte m a <*> peekByte m (a + 1)
      tick 4 (Read fc sz a v)
      noDevice a
      pure v
  , busWrite = \fc sz a v -> do
      nd <- ($ a) <$> readIORef (mBerr m)
      -- no device: nothing written
      if nd then pure () else case sz of
        Byte -> pokeByte m a (fromIntegral v)
        Word -> pokeByte m a (fromIntegral (v `shiftR` 8)) >> pokeByte m (a + 1) (fromIntegral v)
      tick 4 (Write fc sz a v)
      noDevice a
  , busTas = \fc a f -> do
      v <- peekByte m a
      pokeByte m a (f v)
      tick 10 (Tas fc a v)
      pure v
  , busIdle = \n -> tick n (Idle n)
  , busIpl = readIORef (mIpl m)
  , busIack = \lvl -> do
      v <- readIORef (mIack m) >>= ($ lvl)
      tick 4 (Iack lvl v)
      pure v
  , busBerr = readIORef (mLastBerr m)
  }
 where
  noDevice a = do
    nd <- ($ a) <$> readIORef (mBerr m)
    writeIORef (mLastBerr m) nd
  tick n e = do
    modifyIORef' (mCycles m) (+ n)
    readIORef (mTrace m) >>= maybe (pure ()) ($ e)

-- | Why a run ended.
data Stop
  = Stopped -- ^ STOP ran
  | LimitReached
  | Fault Fault -- ^ the model can't go on (e.g. a double fault)
  deriving stock (Show, Eq)

data Stats = Stats
  { statCycles       :: !Int
  , statInstructions :: !Int
  , statStop         :: !Stop
  } deriving stock (Show, Eq)

-- | When a run stops besides STOP and errors: a number of cycles or of
-- instructions (whichever the limit names; the instruction that crosses a
-- cycle limit runs whole).
data Limit = Cycles !Int | Instructions !Int
  deriving stock (Show, Eq)

stopAt :: Int -> Limit
stopAt = Cycles

-- | One instruction; Left: a fault the model can't go on from.
-- Right True: an instruction ran; Right False: an interrupt was taken, or
-- the CPU stays stopped.
stepOne :: Machine -> IO (Either Fault Bool)
stepOne m = do
  c <- readCpu m
  ipl <- readIORef (mIpl m)
  let instr = not (pending ipl c) && not (cStopped c)
  if instr then readIORef (mTrace m) >>= maybe (pure ()) ($ Instruction (cPc c) (cIr c)) else pure ()
  r <- step (busOf m) c
  case r of
    Right c' -> writeCpu m c' >> pure (Right instr)
    Left f -> pure (Left f)

-- | Run from the CPU's state until STOP, a fault, or the limit.
run :: Machine -> Limit -> IO Stats
run m limit = do
  c0 <- readIORef (mCycles m)
  let go !n = do
        c <- readCpu m
        cy <- subtract c0 <$> readIORef (mCycles m)
        let reached = case limit of
              Cycles k -> cy >= k
              Instructions k -> n >= k
        ipl <- readIORef (mIpl m)
        if cStopped c && not (pending ipl c)
          then pure (Stats cy n Stopped) -- until an interrupt ('setIpl', then run again)
          else if reached
            then pure (Stats cy n LimitReached)
            else stepOne m >>= \case
              Left f -> pure (Stats cy n (Fault f))
              -- STOP counts as an instruction, an interrupt doesn't
              Right ran -> go (if ran then n + 1 else n)
  go 0
