{-|
The sound module (PLAN.md step 7.4; docs/systems/snes.md): the SPC700
("Spc700.Spec") with its 64 KiB of RAM, the boot ROM (ours, or the user's
dump: 64 bytes at FFC0h while CONTROL bit 7 is set), the I/O ports at
00F0h-00FFh (fullsnes, "SNES APU SPC700 I/O Ports"), the three timers,
the four ports to the main CPU, and the S-DSP ("Snes.Spec.Dsp"), one
stereo sample every 32 SPC700 cycles.

__Time:__ the module runs at 1.024 MHz (24.576 MHz / 24). It is caught up
to the main machine's master clock ('runApuTo', called by
"Snes.Spec.Machine" before every port access and at each line's end), at
1.024 MHz against 21.47727 MHz, so on the board, where the master clock
waits for DDR, the sound module waits with it (snes.md, "Clock and
display").

__Timers:__ T0 and T1 count at 8 kHz (every 128 cycles), T2 at 64 kHz
(every 16); each counts its divider (TnDIV, 0 = 256) and then its 4-bit
output, cleared by reading it.
-}
module Snes.Spec.Apu
  ( Apu
  , newApu
  , apuCycles
  , runApuTo
  , apuPortRead
  , apuPortWrite
  , apuSamples
  , apuRam
  , apuCpuRegs
  , ApuEvent (..)
  , apuTraceOn
  , apuTakeTrace
  ) where

import Prelude

import Control.Monad (forM_, when)
import Control.Monad.ST (ST, fixST)
import Data.Bits
import Data.STRef
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MV
import Data.Word

import Snes.Spec.Dsp
import Spc700.Spec (Bus (..), Cpu)
import qualified Spc700.Spec as Spc
import Spc700.Types (Regs (..))

data Apu s = Apu
  { aRam     :: !(MV.MVector s Word8)
  , aIpl     :: !(U.Vector Word8)
  , aDsp     :: !(Dsp s)
  , aSt      :: !(MV.MVector s Int)
  , aSamples :: !(STRef s (MV.MVector s Int, Int)) -- ^ L, R pairs and their count
  , aCpu     :: !(Cpu s)
  , aTrace   :: !(STRef s (Maybe [ApuEvent]))
  }

-- | What the S-DSP sees from outside, for its lockstep test (step 7.8):
-- the SPC700's writes to ARAM (at the module's cycle count) and to the
-- DSP's registers, and each sample (at its cycle count).
data ApuEvent
  = EvAram !Int !Int !Word8
  | EvDsp !Int !Word8
  | EvSample !Int !Int !Int
  deriving stock (Show, Eq)

sCycles, sControl, sDspAddr, sIn, sOut, sTimerDiv, sTimerStage, sTimerOut :: Int
sCycles = 0
sControl = 1
sDspAddr = 2
sIn = 3          -- 4: what the main CPU wrote
sOut = 7         -- 4: what the SPC700 wrote
sTimerDiv = 11   -- 3
sTimerStage = 14 -- 3
sTimerOut = 17   -- 3

-- | The sound module after reset: CONTROL B0h (the boot ROM on), the
-- SPC700 at the reset vector (FFFEh: the boot ROM's FFC0h).
newApu :: U.Vector Word8 -> ST s (Apu s)
newApu ipl = do
  ram <- MV.replicate 0x10000 0
  dsp <- newDsp
  st <- MV.replicate 20 0
  MV.write st sControl 0xB0
  buf <- MV.replicate 65536 0
  samples <- newSTRef (buf, 0)
  trace <- newSTRef Nothing
  fixST $ \a -> do
    let bus = Bus
          { busRead = \addr -> apuCycle a >> memRead a (fromIntegral addr)
          , busWrite = \addr v -> apuCycle a >> memWrite a (fromIntegral addr) v
          , busIdle = apuCycle a
          }
        lo = ipl U.! 0x3E
        hi = ipl U.! 0x3F
    cpu <- Spc.newCpu bus (Regs 0 0 0 0 (fromIntegral lo .|. fromIntegral hi `shiftL` 8) 0)
    pure (Apu ram ipl dsp st samples cpu trace)

get :: Apu s -> Int -> ST s Int
get a = MV.unsafeRead (aSt a)

set :: Apu s -> Int -> Int -> ST s ()
set a = MV.unsafeWrite (aSt a)

apuCycles :: Apu s -> ST s Int
apuCycles a = get a sCycles

apuRam :: Apu s -> Int -> ST s Word8
apuRam a i = MV.read (aRam a) (i .&. 0xFFFF)

apuCpuRegs :: Apu s -> ST s Regs
apuCpuRegs a = Spc.getRegs (aCpu a)

-- | Records 'ApuEvent's from now on.
apuTraceOn :: Apu s -> ST s ()
apuTraceOn a = writeSTRef (aTrace a) (Just [])

-- | The events recorded so far, oldest first.
apuTakeTrace :: Apu s -> ST s [ApuEvent]
apuTakeTrace a = maybe [] reverse <$> readSTRef (aTrace a)

record :: Apu s -> ApuEvent -> ST s ()
record a e = modifySTRef' (aTrace a) (fmap (e :))

-- | Runs the SPC700 until the module's cycle count reaches @target@.
runApuTo :: Apu s -> Int -> ST s ()
runApuTo a target = do
  n <- get a sCycles
  when (n < target) (Spc.step (aCpu a) >> runApuTo a target)

-- | What the main CPU reads at 2140h + i.
apuPortRead :: Apu s -> Int -> ST s Word8
apuPortRead a i = fromIntegral <$> get a (sOut + (i .&. 3))

-- | The main CPU's write to 2140h + i.
apuPortWrite :: Apu s -> Int -> Word8 -> ST s ()
apuPortWrite a i v = set a (sIn + (i .&. 3)) (fromIntegral v)

-- | Every stereo sample made so far (left, right, ...): n samples are 2n
-- values.
apuSamples :: Apu s -> ST s (U.Vector Int)
apuSamples a = do
  (buf, n) <- readSTRef (aSamples a)
  U.freeze (MV.slice 0 (2 * n) buf)

-- | One SPC700 cycle's worth of the rest: the timers, and every 32 cycles
-- the DSP's sample.
apuCycle :: Apu s -> ST s ()
apuCycle a = do
  n <- (+ 1) <$> get a sCycles
  set a sCycles n
  when (n .&. 15 == 0) (timer 2)
  when (n .&. 127 == 0) (timer 0 >> timer 1)
  when (n .&. 31 == 0) $ do
    (l, r) <- dspSample (aDsp a) (\i -> MV.read (aRam a) i) (\i v -> MV.write (aRam a) i v)
    record a (EvSample n l r)
    (buf, k) <- readSTRef (aSamples a)
    buf' <- if 2 * k + 2 > MV.length buf then MV.grow buf (MV.length buf) else pure buf
    MV.write buf' (2 * k) l
    MV.write buf' (2 * k + 1) r
    writeSTRef (aSamples a) (buf', k + 1)
 where
  timer t = do
    ctl <- get a sControl
    when (testBit ctl t) $ do
      st <- (+ 1) <$> get a (sTimerStage + t)
      dv <- get a (sTimerDiv + t)
      if st >= (if dv == 0 then 256 else dv)
        then do
          set a (sTimerStage + t) 0
          get a (sTimerOut + t) >>= set a (sTimerOut + t) . (\o -> (o + 1) .&. 0xF)
        else set a (sTimerStage + t) st

memRead :: Apu s -> Int -> ST s Word8
memRead a addr
  | addr >= 0xF0 && addr <= 0xFF = case addr of
      0xF2 -> fromIntegral <$> get a sDspAddr
      0xF3 -> get a sDspAddr >>= dspRead (aDsp a)
      _ | addr >= 0xF4 && addr <= 0xF7 -> fromIntegral <$> get a (sIn + addr - 0xF4)
      _ | addr == 0xF8 || addr == 0xF9 -> MV.read (aRam a) addr
      _ | addr >= 0xFD -> do
            let t = addr - 0xFD
            o <- get a (sTimerOut + t)
            set a (sTimerOut + t) 0
            pure (fromIntegral o)
      _ -> pure 0
  | addr >= 0xFFC0 = do
      ctl <- get a sControl
      if testBit ctl 7 then pure (aIpl a U.! (addr - 0xFFC0)) else MV.read (aRam a) addr
  | otherwise = MV.read (aRam a) addr

-- | Writes to 00F0h-00FFh reach the registers and RAM both (fullsnes).
memWrite :: Apu s -> Int -> Word8 -> ST s ()
memWrite a addr v = do
  MV.write (aRam a) addr v
  get a sCycles >>= \n -> record a (EvAram n addr v)
  let i = fromIntegral v :: Int
  when (addr >= 0xF0 && addr <= 0xFF) $ case addr of
    0xF1 -> do
      old <- get a sControl
      set a sControl i
      forM_ [0 .. 2] $ \t -> when (testBit i t && not (testBit old t)) $
        set a (sTimerStage + t) 0 >> set a (sTimerOut + t) 0
      when (testBit i 4) $ set a sIn 0 >> set a (sIn + 1) 0
      when (testBit i 5) $ set a (sIn + 2) 0 >> set a (sIn + 3) 0
    0xF2 -> set a sDspAddr i
    0xF3 -> do
      da <- get a sDspAddr
      record a (EvDsp da v)
      dspWrite (aDsp a) da v
    _ | addr >= 0xF4 && addr <= 0xF7 -> set a (sOut + addr - 0xF4) i
    _ | addr >= 0xFA && addr <= 0xFC -> set a (sTimerDiv + addr - 0xFA) i
    _ -> pure ()
