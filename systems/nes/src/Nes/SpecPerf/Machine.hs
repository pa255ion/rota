{-# OPTIONS_GHC -fworker-wrapper -fspecialise-aggressively -funfolding-use-threshold=200 -funfolding-creation-threshold=1000 #-}
-- The NES's performance version (side track Q for side track W, the
-- user 2026-11-06: "an alternative, performance version"): begun as a copy
-- of Nes.SpecFast.Machine, which stays as it is; changed here for speed, above all in
-- the browser's build (GHC's WebAssembly backend, where every call between
-- Haskell functions goes back through the runtime's loop). The same
-- machine: nes-specperf runs it beside the fast version on every input.
-- (the fast version is never synthesised: GHC's unboxing back on, which
-- clash-common's -fno-worker-wrapper turns off for Clash's sake)
-- Side track Q (PLAN.md Q.7): the desktop emulator's fast version of
-- Nes.Spec.Machine, kept beside it (the original stays the reference the
-- core's lockstep checks; a fix in either goes to both). Same machine:
-- the nes-specfast suite runs both side by side and compares them.
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-|
The NES around the 6502 (PLAN.md step 3.2): the 2A03's memory map, open
bus, controllers, OAM DMA and the APU, the PPU, the cartridge, and how
their clocks interleave. Everything runs in 'ST' behind "Nes.Spec".

Sources: nesdev wiki, "CPU memory map", "Open bus behavior", "Standard
controller", "PPU OAM" (DMA), "DMA", "Cycle reference chart".

__Clocks.__ A master clock counts 21.477272 MHz ticks: a CPU cycle is 12,
a PPU dot 4. A read cycle's bus access is at tick 5 of the cycle, a
write's at tick 7 (the 2A03's φ2 timing as Mesen models it). The PPU runs
up to the access, then to the end of the cycle; with the PPU clock 1 tick
behind (the "alignment"; real consoles power up in one of several) that
is two dots before every access and one after. The APU (and the mapper's
cycle count) advances at the start of each CPU cycle. After the cycle the
CPU samples the NMI line (the PPU's output) and the IRQ line (APU frame
counter).

__Open bus.__ The CPU data bus keeps the last value driven on it: every
read result and every written value. Unmapped reads ($4018-$401F,
nothing answering at $4020-$5FFF) return it; $4016/$4017 drive only bits
0 to 4; $4015 drives everything but bit 5 and, being inside the 2A03,
doesn't change the latch.

__DMA__ (OAM and DMC; nesdev wiki "DMA"). A write to $4014 asks the CPU to halt: RDY drops, and the
CPU's next read cycle becomes the halt cycle (a read at the CPU's
address, side effects included). Then, if the next cycle is a "put"
cycle, one alignment cycle (the same read again); then 256 pairs of a
get (read $xx00 + i) and a put (write $2004). So 513 or 514 cycles. Gets
fall on odd cycle numbers (counting the first cycle after power-on as
1); the other parity fails @cpu_interrupts_v2/4-irq_and_dma@ by one
cycle.
The CPU repeats its read during all of them, as on hardware, and samples
interrupts every cycle; the machine performs the DMA's accesses instead.

__Controllers.__ Writing bit 0 of $4016 = 1 reloads both shift registers
from the buttons continuously; = 0 lets reads shift them out, A first
(A, B, Select, Start, Up, Down, Left, Right), then 1s.
-}
module Nes.SpecPerf.Machine
  ( Machine (..)
  , machineFields
  , Access (..)
  , newMachine
  , stepCpu
  , runFrame
  , setRebase
  , resetMachine
  , machineCycles
  , setButtons
  , peekCpu
  , withTrace
  , apuTrace
  , alignment
  , serialBits
  , nametableText
  , PpuEvent (..)
  , setPpuHook
  , ppuBusPeek
    -- * Save states (side track N, "Nes.Spec.SaveState")
  , newMachineOverlay
  , setOverlay
  , overlaySeen
  , machineQuiet
  , machineStrobe
  , ramImage
  , ciramImage
  , loadRam
  , loadCiram
    -- * The desktop's sound (side track Q)
  , setSound
  , takeSound
  , takeSoundView
  , soundCap
  ) where

import Prelude

import Control.Monad (when)
import Data.Maybe (isJust, isNothing)
import Control.Monad.ST (ST)
import GHC.Exts (State#, Word8#)
import GHC.ST (ST (..))
import GHC.Word (Word8 (..))
import Data.Bits
import qualified Data.ByteString as BS
import Data.STRef
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MV
import Platform.Snapshot (Field (..))
import Data.Word

import Nes.SpecPerf.Cpu
import Nes.Cartridge
import Nes.SpecPerf.Apu
import Nes.SpecPerf.Mapper
import Nes.SpecPerf.Ppu

-- | One CPU cycle's bus access: address, data, write. DMA cycles show the
-- DMA's access.
data Access = Access !Word16 !Word8 !Bool
  deriving stock (Show, Eq)

data Machine s = Machine
  { mCpu   :: !(Cpu Machine s)
  , mPpu   :: !(Ppu s)
  , mApu   :: !(Apu s)
  , mCart  :: !(Mapper s)
  , mRam   :: !(MV.MVector s Word8)  -- ^ 2 KiB
  , mCiram :: !(MV.MVector s Word8)  -- ^ 2 KiB nametable RAM
  , mSt    :: !(MV.MVector s Int)
  , mTrace :: !(STRef s [Access])
  , mApuTr :: !(STRef s [Channels])   -- ^ while tracing: the APU's outputs after each cycle, newest first
  , mSerial :: !(STRef s [Bool])     -- ^ bit 0 of every $4016 write, newest first
  , mHook   :: !(STRef s (Maybe (PpuEvent -> ST s ())))
  , mOvl    :: !(STRef s (Maybe (U.Vector Word8, Word16, Bool))) -- ^ the ROM overlay: its bytes, end address, end read
  , mSnd    :: !(MV.MVector s Int)   -- ^ while 'setSound': the APU's outputs after each cycle, packed ('takeSound')
  }

-- | What the PPU just did, for an observer (the lockstep tests of step
-- 3.5): a dot, or a CPU (or DMA) read or write of a register and its value.
data PpuEvent = PeDot | PeRead !Int !Word8 | PeWrite !Int !Word8
  | PeBusWrite !Word16 !Word8 -- ^ the PPU wrote its memory bus ($2007), inside a PeWrite
  deriving stock (Show, Eq)

-- | PPU clock ticks behind the CPU's master clock (see the module header).
alignment :: Int
alignment = 1

-- State slots.
iMaster, iPpuClk, iCycles, iBus, iHalted, iDmaPage, iOamCount, iDmaVal, iStrobe, iShift1, iShift2, iBtn1, iBtn2, iTraceOn, iOamOn, iDmcState, iLastRead, iCurRead, iJoyBit1, iJoyBit2, iIntBus :: Int
iMaster = 0
iPpuClk = 1
iCycles = 2   -- CPU cycles since power-on
iBus = 3      -- open bus latch
iHalted = 4   -- the DMA unit holds the CPU (its halt cycle is done)
iDmaPage = 5
iOamCount = 6 -- OAM DMA accesses done (even: a get next, odd: a put)
iDmaVal = 7
iStrobe = 8
iShift1 = 9
iShift2 = 10
iBtn1 = 11
iBtn2 = 12
iTraceOn = 13
iOamOn = 14    -- OAM DMA wanted or running
iDmcState = 15 -- DMC DMA: 0 none, 1 needs its halt, 2 its dummy, 3 ready to get
iLastRead = 16 -- address the previous cycle read (-1: it didn't)
iCurRead = 17
iJoyBit1 = 18  -- the bit a run of consecutive reads of  returns
iJoyBit2 = 19
iIntBus = 20   -- the CPU's internal data bus: its own reads and writes, not the DMA's

iStrobePend :: Int
iStrobePend = 21 -- the last $4016 write, for the end of the next get cycle: bit 0 + 2 (0: none)

-- (Slots 22 to 25 were the fast version's copy of the 6502's interrupt
-- polls, for side track N's rest; the performance version asks the CPU,
-- 'cpuPending', so they are unused.)

-- The desktop's sound ('setSound').
iSndOn, iSndLen :: Int
iSndOn = 26
iSndLen = 27

-- The lazy PPU (the performance version's): the PPU runs only when
-- something looks at it ('syncPpu'), not at every CPU cycle's two halves.
iLazy, iQuiet, iNmiLevel :: Int
iLazy = 28     -- 1: lazy (no hook, a mapper whose IRQ doesn't count PPU fetches)
iQuiet = 29    -- the PPU's clock before which its NMI and frame count can't change
iNmiLevel = 30 -- its NMI output as of the last sync

-- The lazy APU (the performance version's): the APU runs only when
-- something looks at it ('syncApu').
iApuQuiet, iApuSmp :: Int
iApuQuiet = 31 -- the CPU cycle from which the APU must be run again
iApuSmp = 32   -- the last CPU cycle whose sound level is in mSnd

iNoRebase :: Int
iNoRebase = 33 -- 1: 'rebase' off (nes-specperf's twin, on 64 bits only)

-- The event horizon (the performance version's): before CPU cycle
-- iFastTill nothing but the access itself can matter in a cycle that reads
-- RAM or PRG ROM or writes RAM ('fastHorizon').
iFastTill, iIrqLevel :: Int
iFastTill = 34 -- the first CPU cycle that must take the full path (0: the next)
iIrqLevel = 35 -- the IRQ line as of the horizon's start (it can't change before it)

slots :: Int
slots = 36

get :: Machine s -> Int -> ST s Int
get m = MV.unsafeRead (mSt m)
{-# INLINE get #-}

set :: Machine s -> Int -> Int -> ST s ()
set m = MV.unsafeWrite (mSt m)
{-# INLINE set #-}

-- | Power on and run the reset sequence (7 CPU cycles): CPU registers as
-- after power-on (A, X, Y = 0, P = $34 after reset, S = $FD), PPU at
-- (0, 0) with its registers clear, RAM clear.
newMachine :: BS.ByteString -> ST s (Either String (Machine s))
newMachine file = newMachineOverlay file Nothing

-- | The same with the ROM overlay armed from the start (its bytes, its
-- end: 'setOverlay'), so the reset sequence reads its vector from it, as
-- the core does when the PS releases it (side track N).
newMachineOverlay :: BS.ByteString -> Maybe (U.Vector Word8, Word16) -> ST s (Either String (Machine s))
newMachineOverlay file ovl0 = case parseRom file of
  Left e -> pure (Left e)
  Right rom -> do
    ecart <- newMapper rom
    case ecart of
      Left e -> pure (Left e)
      Right cart -> do
        ram <- MV.replicate 0x800 0
        ciram <- MV.replicate 0x800 0
        st <- MV.replicate slots 0
        MV.write st iLastRead (-1)
        MV.write st iCurRead (-1)
        tr <- newSTRef []
        atr <- newSTRef []
        ser <- newSTRef []
        hook <- newSTRef Nothing
        ovlRef <- newSTRef ((\(img, end) -> (img, end, False)) <$> ovl0)
        let pbus = PpuBus
              { pbRead = ppuRead cart ciram Nothing
              , pbWrite = \a v -> do
                  readSTRef hook >>= maybe (pure ()) (\k -> k (PeBusWrite a v))
                  ppuWrite cart ciram a v
              , pbAddr = mapperA12 cart
              , pbFetch = ppuRead cart ciram . Just
              }
        ppu <- newPpu cart ciram pbus
        apu <- newApu
        -- The machine is the CPU's bus ('CpuBus', below) and holds the CPU:
        -- a recursive let (the CPU's bus field is lazy).
        cst <- newCpuState (Regs 0 0 0 0 0x34 0)
        snd' <- MV.new soundCap
        let m = Machine cpu ppu apu cart ram ciram st tr atr ser hook ovlRef snd'
            cpu = newCpu ricoh2a03 m cst
        -- MMC5's IRQ counts the PPU's fetches, which the CPU polls every
        -- cycle: it runs the PPU at every cycle as ever. The MMC3 is lazy
        -- too: its A12 filter's cycles are recorded and counted when the PPU
        -- gets there ('mapperRecordCycle'), its IRQ bounds the PPU's quiet
        -- dots ('mapperIrqQuietDots').
        -- (the MMC5 too: its in-frame logic's cycles replayed before each
        -- rendering fetch, its IRQ bounding the quiet dots likewise)
        set m iLazy 1
        mapperSetRecord cart (mapperKind cart == 4 || mapperKind cart == 5)
        reset cpu
        pure (Right m)

-- | The 2A03's side of the CPU's bus (side track Q: a class, so the CPU's
-- code specialises to the machine's; the reference model's is a record).
instance CpuBus Machine where
  busRead = machineRead
  busWrite = machineWrite
  busRdy m = do
    c <- get m iCycles
    ft <- get m iFastTill
    if c + 1 < ft then pure True else not <$> dmaWanted m
  busNmi m = do
    c <- get m iCycles
    ft <- get m iFastTill
    -- (the CPU keeps the edge and the polls itself: 'machineQuiet' asks it)
    if c < ft then (/= 0) <$> get m iNmiLevel else nmiNow m
  busIrq m = do
    c <- get m iCycles
    ft <- get m iFastTill
    -- (the horizon worked out here, at a full cycle's very end: after the
    -- NMI's sample has brought the PPU's quiet dots up to date)
    if c < ft then (/= 0) <$> get m iIrqLevel else do
      i <- (||) <$> apuIrq (mApu m) <*> mapperIrq (mCart m)
      fastHorizon m i
      pure i
  {-# INLINE busRead #-}
  {-# INLINE busWrite #-}
  {-# INLINE busRdy #-}
  {-# INLINE busNmi #-}
  {-# INLINE busIrq #-}

-- | The reset button: CPU reset sequence, PPUCTRL/PPUMASK cleared, APU
-- silenced and its frame counter restarted.
resetMachine :: Machine s -> ST s ()
resetMachine m = do
  set m iFastTill 0
  syncLazy m
  ppuRegWrite (mPpu m) 0 0
  ppuRegWrite (mPpu m) 1 0
  syncApu m
  apuReset (mApu m)
  set m iHalted 0
  set m iOamOn 0
  reset (mCpu m)

stepCpu :: Machine s -> ST s Step
stepCpu m = step (mCpu m)

machineCycles :: Machine s -> ST s Int
machineCycles m = get m iCycles

-- | Instructions up to the PPU's next frame, or @cap@ CPU cycles (the
-- desktop's frame; the performance version's addition, so the loop runs
-- inside the model).
runFrame :: Machine s -> Int -> ST s ()
runFrame m cap = do
  f0 <- ppuFramesDone (mPpu m)
  c0 <- get m iCycles
  let go = do
        _ <- stepCode (mCpu m)
        f <- ppuFramesDone (mPpu m)
        c <- get m iCycles
        when (f == f0 && c - c0 < cap) go
  go
  -- (the PPU up to now at each frame's end: the lazy MMC3's recorded cycles
  -- drained, for save states and the rebase)
  syncLazy m
  rebase m

-- | The clocks counted from power-on kept small (side track W: the
-- browser's WebAssembly has a 32-bit Int; the master clock would pass 2^31
-- after 100 s, the CPU's cycles after 20 minutes): after a frame, past
-- 2^28 master ticks, the master clock and the PPU's clock go back by 12 *
-- 2^24 ticks and every CPU cycle count by 2^24 (even: get and put cycles
-- stay as they were) -- the machine's, the APU's and their stamps, the
-- mapper's. Only differences and parities are used, so nothing changes.
rebase :: Machine s -> ST s ()
rebase m = do
  t <- get m iMaster
  off <- get m iNoRebase
  when (t >= 1 `shiftL` 28 && off == 0) $ do
    let dc = 1 `shiftL` 24
        dm = 12 * dc
        sub i d = get m i >>= set m i . subtract d
    sub iMaster dm
    sub iPpuClk dm
    q <- get m iQuiet
    when (q /= 0) (set m iQuiet (q - dm))
    mapM_ (`sub` dc) [iCycles, iApuQuiet, iApuSmp]
    set m iFastTill 0
    apuRebase (mApu m) dc
    mapperRebase (mCart m) dc

-- | 'rebase' on (the default) or off: nes-specperf runs a twin without it
-- (64 bits only).
setRebase :: Machine s -> Bool -> ST s ()
setRebase m on = set m iNoRebase (if on then 0 else 1)

-- | Buttons of controller 1 or 2, bit 0 = A to bit 7 = Right.
setButtons :: Machine s -> Int -> Word8 -> ST s ()
setButtons m port b = set m (if port == 1 then iBtn1 else iBtn2) (fromIntegral b)

-- | CPU address space without side effects: RAM, PRG RAM and ROM; 0
-- elsewhere.
peekCpu :: Machine s -> Word16 -> ST s Word8
peekCpu m a
  | a < 0x2000 = MV.unsafeRead (mRam m) (fromIntegral a .&. 0x7FF)
  | a >= 0x6000 = fromIntegral . max 0 <$> cpuReadCart (mCart m) a
  | otherwise = pure 0

-- | Run an action with the bus trace on; its result and the accesses,
-- oldest first.
withTrace :: Machine s -> ST s a -> ST s (a, [Access])
withTrace m act = do
  writeSTRef (mTrace m) []
  writeSTRef (mApuTr m) []
  set m iTraceOn 1
  set m iFastTill 0
  r <- act
  set m iTraceOn 0
  t <- readSTRef (mTrace m)
  pure (r, reverse t)

-- * Cycles

-- | The PPU caught up with the master clock (in lazy mode; at every cycle's
-- halves otherwise): before every access that reads or changes it, or
-- that changes what it fetches (a cartridge write: banks, mirroring), and
-- whenever its NMI output or frame count could have changed ('nmiNow').
-- It runs the same dots at the same master clock either way: nothing it
-- does in between is seen. 'iQuiet' then needs working out again.
syncPpu :: Machine s -> ST s ()
syncPpu m = do
  t <- get m iMaster
  runPpu m (t - alignment)
  set m iQuiet 0
{-# INLINE syncPpu #-}

-- | 'syncPpu' in lazy mode (eager mode is always in step).
syncLazy :: Machine s -> ST s ()
syncLazy m = do
  l <- get m iLazy
  when (l /= 0) (syncPpu m)
{-# INLINE syncLazy #-}

-- | The PPU's NMI output now (the CPU samples it every cycle): in lazy
-- mode the last sync's, until the PPU's quiet dots run out.
nmiNow :: Machine s -> ST s Bool
nmiNow m = do
  l <- get m iLazy
  if l == 0 then ppuNmi (mPpu m) else do
    t <- get m iMaster
    q <- get m iQuiet
    if t - alignment < q then (/= 0) <$> get m iNmiLevel else do
      syncPpu m
      v <- ppuNmi (mPpu m)
      d0 <- ppuQuietDots (mPpu m)
      d1 <- mapperIrqQuietDots (mCart m)
      let d = min d0 d1
      clk <- get m iPpuClk
      set m iQuiet (clk + 4 * d)
      set m iNmiLevel (if v then 1 else 0)
      pure v

-- | The APU run up to CPU cycle @n@, the current one (its 'apuCycle',
-- which comes before the cycle's access), the sound levels of the cycles
-- before it written: each cycle's level is the APU's after that cycle's
-- access, and nothing touched the APU in the cycles it skipped. Then the
-- cycle it must run again from ('apuQuietCycles').
syncApuTo :: Machine s -> Int -> ST s ()
syncApuTo m n = do
  let a = mApu m
  s <- apuCycles a
  smp <- get m iApuSmp
  -- the APU's last cycle's level, if not written (its access is over)
  when (smp < s) (packSample m)
  let go c = when (c < n) (apuCycle a >> packSample m >> go (c + 1))
  go (s + 1)
  when (s < n) (apuCycle a)
  set m iApuSmp (n - 1)
  q <- apuQuietCycles a
  set m iApuQuiet (n + q)

-- | Before an access that reads or changes the APU (its registers, the
-- DMA unit's calls): the APU up to now, and run again at the next cycle
-- (the access may change what 'apuQuietCycles' said).
syncApu :: Machine s -> ST s ()
syncApu m = do
  n <- get m iCycles
  s <- apuCycles (mApu m)
  when (s < n) (syncApuTo m n)
  set m iApuQuiet 0

-- | The sound level now, into mSnd (while 'setSound').
packSample :: Machine s -> ST s ()
packSample m = do
  sOn <- get m iSndOn
  when (sOn /= 0) $ do
    k <- get m iSndLen
    when (k < soundCap) $ do
      apuPackedCached (mApu m) >>= MV.unsafeWrite (mSnd m) k
      set m iSndLen (k + 1)

{-# INLINE startCycle #-}
startCycle :: Machine s -> Int -> ST s ()
startCycle m ticks = do
  t <- (+ ticks) <$> get m iMaster
  set m iMaster t
  l <- get m iLazy
  when (l == 0) (runPpu m (t - alignment))
  c <- get m iCycles
  set m iCycles (c + 1)
  -- the APU: in step while a trace is on, else when its quiet cycles end
  q <- get m iApuQuiet
  tr <- get m iTraceOn
  when (c + 1 >= q || tr /= 0) (syncApuTo m (c + 1))
  -- the cartridge's M2 edge: counted now in eager mode; the lazy MMC3's
  -- recorded with its time, for when the PPU gets there
  if l == 0 then mapperCycle (mCart m)
    else when (recordsCycles m) (mapperRecordCycle (mCart m) t)
  get m iCurRead >>= set m iLastRead
  set m iCurRead (-1)

{-# INLINE endCycle #-}
endCycle :: Machine s -> Int -> ST s ()
endCycle m ticks = do
  t <- (+ ticks) <$> get m iMaster
  set m iMaster t
  l <- get m iLazy
  when (l == 0) (runPpu m (t - alignment))
  -- the end of a get cycle (odd numbers; the change to a put): the
  -- controllers see the last $4016 write
  n <- get m iCycles
  sp <- get m iStrobePend
  when (odd n && sp /= 0) (set m iStrobePend 0 >> strobe m (sp - 2))
  on <- get m iTraceOn
  when (on /= 0) (apuChannels (mApu m) >>= \c -> modifySTRef' (mApuTr m) (c :))
  -- this cycle's level now if the APU is up to it (else a later sync
  -- writes it, from the same state)
  s <- apuCycles (mApu m)
  when (s == n) (packSample m >> set m iApuSmp n)

runPpu :: Machine s -> Int -> ST s ()
runPpu m target = do
  clk0 <- get m iPpuClk
  hook <- readSTRef (mHook m)
  -- (the hook looked up once, not every dot: side track Q)
  let go clk
        | clk + 4 <= target = case hook of
            Just k -> ppuDot (mPpu m) >> k PeDot >> go (clk + 4)
            Nothing -> do
              -- (whole idle stretches at once: 'ppuIdleRun')
              let todo = (target - clk) `div` 4
              -- (a stretch: in eager mode the dots come one or two at a time)
              r <- if todo >= 8 then ppuVisibleRun (mPpu m) clk todo else pure 0
              -- (and a line's end: 'ppuHblankRun')
              h <- if r == 0 && todo >= 8 then ppuHblankRun (mPpu m) clk todo else pure 0
              if r > 0 || h > 0 then go (clk + 4 * (r + h)) else do
                i <- if todo >= 8 then ppuIdleRun (mPpu m) todo else pure 0
                if i > 0 then go (clk + 4 * i) else do
                  mapperSetDotClock (mCart m) (clk + 4)
                  ppuDot (mPpu m)
                  go (clk + 4)
        -- (and the lazy MMC3's recorded cycles all counted: the PPU is
        -- up to the present)
        | otherwise = set m iPpuClk clk >> mapperReplayAll (mCart m)
  go clk0

trace :: Machine s -> Word16 -> Word8 -> Bool -> ST s ()
trace m a v w = do
  on <- get m iTraceOn
  when (on /= 0) (modifySTRef' (mTrace m) (Access a v w :))

-- | The CPU's read and write cycles: inside the event horizon the fast
-- path, else the full one. Out of line (one call from each of the CPU's
-- many access sites: inlined there, the code grew too big to run fast in
-- the browser).
machineRead :: Machine s -> Word16 -> ST s Word8
machineRead m a = ST (\s -> case machineReadU m a s of (# s', w #) -> (# s', W8# w #))
{-# INLINE machineRead #-}

-- | 'machineRead' out of line with its byte unboxed by hand: GHC returns
-- a NOINLINE function's result boxed, 16 bytes a CPU cycle, which the
-- browser pays for in garbage collection.
machineReadU :: Machine s -> Word16 -> State# s -> (# State# s, Word8# #)
machineReadU m a s = case machineReadBody m a of
  ST f -> case f s of (# s', W8# w #) -> (# s', w #)
{-# NOINLINE machineReadU #-}

machineReadBody :: Machine s -> Word16 -> ST s Word8
machineReadBody m !a = do
  c <- get m iCycles
  ft <- get m iFastTill
  -- (the MMC5 sees the NMI vector's read: the full path)
  let vec5 = mapperKind (mCart m) == 5 && (a == 0xFFFA || a == 0xFFFB)
  -- (strict in the address, and the byte returned evaluated: no boxes on
  -- every cycle, which the browser pays for)
  v <- if c + 1 < ft && (a < 0x2000 || a >= 0x8000) && not vec5 then fastRead m a else cycleRead m a
  pure $! v

-- | The lazy MMC3 and MMC5 record each CPU cycle ('mapperRecordCycle').
recordsCycles :: Machine s -> Bool
recordsCycles m = let k = mapperKind (mCart m) in k == 4 || k == 5
{-# INLINE recordsCycles #-}
{-# INLINE machineReadBody #-}

machineWrite :: Machine s -> Word16 -> Word8 -> ST s ()
machineWrite m a (W8# v) = ST (\s -> (# machineWriteU m a v s, () #))
{-# INLINE machineWrite #-}

-- | 'machineWrite' out of line with its byte unboxed by hand (see
-- 'machineReadU').
machineWriteU :: Machine s -> Word16 -> Word8# -> State# s -> State# s
machineWriteU m a v s = case machineWriteBody m a (W8# v) of ST f -> case f s of (# s', () #) -> s'
{-# NOINLINE machineWriteU #-}

machineWriteBody :: Machine s -> Word16 -> Word8 -> ST s ()
machineWriteBody m !a !v = do
  c <- get m iCycles
  ft <- get m iFastTill
  if c + 1 < ft && a < 0x2000 then fastWrite m a v else cycleWrite m a v
{-# INLINE machineWriteBody #-}

-- | A read cycle inside the event horizon, of RAM or PRG ROM: the clocks,
-- the bus latches and the last-read bookkeeping (the joypads' runs);
-- nothing else can happen (no DMA, no trace or hook or
-- overlay, the PPU and APU quiet, a mapper without per-cycle work).
fastRead :: Machine s -> Word16 -> ST s Word8
fastRead m a = do
  t0 <- get m iMaster
  set m iMaster (t0 + 12)
  when (recordsCycles m) (mapperRecordCycle (mCart m) (t0 + 5))
  get m iCycles >>= set m iCycles . (+ 1)
  get m iCurRead >>= set m iLastRead
  set m iCurRead (fromIntegral a)
  v <- if a < 0x2000 then MV.unsafeRead (mRam m) (fromIntegral a .&. 0x7FF) else do
         x <- cpuReadCart (mCart m) a
         if x < 0 then fromIntegral <$> get m iBus else pure (fromIntegral x)
  set m iBus (fromIntegral v)
  set m iIntBus (fromIntegral v)
  pure v
{-# INLINE fastRead #-}

-- | A RAM write cycle inside the event horizon.
fastWrite :: Machine s -> Word16 -> Word8 -> ST s ()
fastWrite m a v = do
  t0 <- get m iMaster
  set m iMaster (t0 + 12)
  when (recordsCycles m) (mapperRecordCycle (mCart m) (t0 + 7))
  get m iCycles >>= set m iCycles . (+ 1)
  get m iCurRead >>= set m iLastRead
  set m iCurRead (-1)
  set m iBus (fromIntegral v)
  MV.unsafeWrite (mRam m) (fromIntegral a .&. 0x7FF) v
  set m iIntBus (fromIntegral v)
{-# INLINE fastWrite #-}

-- | At the end of a cycle on the full path: the event horizon from here,
-- and the IRQ line (@irq@, now) it keeps. None while anything can happen in any cycle: the
-- eager PPU (a hook, MMC3, MMC5), a trace, the ROM overlay, a DMA wanted or
-- running, a $4016 write waiting for a get cycle. Else the earliest of the
-- APU's quiet cycles' end ('iApuQuiet') and the PPU's (its clock
-- 'iQuiet', in whole CPU cycles of 12 ticks).
fastHorizon :: Machine s -> Bool -> ST s ()
fastHorizon m irq = do
  lz <- get m iLazy
  tr <- get m iTraceOn
  h <- get m iHalted
  o <- get m iOamOn
  st <- get m iDmcState
  sp <- get m iStrobePend
  due <- apuDmcDue (mApu m)
  q <- get m iQuiet
  ovl <- readSTRef (mOvl m)
  if lz == 0 || tr /= 0 || h /= 0 || o /= 0 || st /= 0 || sp /= 0 || due /= 0 || q == 0 || isJust ovl
    then set m iFastTill 0
    else do
      c <- get m iCycles
      t <- get m iMaster
      aq <- get m iApuQuiet
      -- cycles k with t + 12 k - alignment < q
      let pq = c + (q + alignment - t - 1) `div` 12
      set m iFastTill (min aq pq)
      set m iIrqLevel (if irq then 1 else 0)

cycleRead :: Machine s -> Word16 -> ST s Word8
cycleRead m a = do
  -- (a cycle on the full path: its NMI and IRQ samples too; the IRQ's
  -- sets the next horizon)
  set m iFastTill 0
  startCycle m 5
  dma <- dmaNow m
  v <- if dma then dmaCycle m a else do
         x <- readMem m a
         set m iIntBus (fromIntegral x)
         pure x
  endCycle m 7
  -- $2002: the VBlank flag is latched when M2 rises (the access), the
  -- sprite 0 and overflow flags only when it falls, at the end of the
  -- cycle (AccuracyCoin "$2002 flag timing": about 1.9 dots later).
  if dma || a >= 0x4000 || a < 0x2000 || a .&. 7 /= 2 then pure v else do
    syncLazy m
    f <- ppuSpriteFlags (mPpu m)
    let v' = (v .&. 0x9F) .|. f
    set m iIntBus (fromIntegral v')
    set m iBus (fromIntegral v')
    on <- get m iTraceOn
    when (on /= 0) (modifySTRef' (mTrace m) (\t -> case t of
      Access ta _ w : rest -> Access ta v' w : rest
      [] -> []))
    pure v'

cycleWrite :: Machine s -> Word16 -> Word8 -> ST s ()
cycleWrite m a v = do
  set m iFastTill 0
  startCycle m 7
  -- An aborted DMC DMA whose halt falls on a write cycle doesn't happen at
  -- all (a normal one waits for the next read; nesdev wiki "DMA").
  n <- get m iCycles
  due <- apuDmcDma (mApu m)
  ab <- apuDmcAborted (mApu m)
  st <- get m iDmcState
  when (ab && st == 0 && maybe False (<= n) due) (syncApu m >> apuDmcAbortDone (mApu m))
  writeMem m a v
  set m iIntBus (fromIntegral v)
  endCycle m 5

-- | Whether the DMA unit wants the coming cycle (the CPU checks RDY before
-- each read; a write cycle can't be halted and goes ahead).
dmaWanted :: Machine s -> ST s Bool
dmaWanted m = do
  n <- (+ 1) <$> get m iCycles
  dmaAt m n

-- | The same, inside the cycle (the cycle count already includes it).
dmaNow :: Machine s -> ST s Bool
dmaNow m = get m iCycles >>= dmaAt m

dmaAt :: Machine s -> Int -> ST s Bool
dmaAt m n = do
  h <- get m iHalted
  o <- get m iOamOn
  st <- get m iDmcState
  due <- apuDmcDue (mApu m)
  pure (h /= 0 || o /= 0 || st /= 0 || (due /= 0 && due <= n))

-- | One cycle of the DMA unit while the CPU repeats its read at @a@
-- (nesdev wiki, "DMA"). The first is the halt cycle. Then, on a get: the
-- DMC's fetch if its halt and dummy cycles are done, else OAM DMA's read,
-- else nothing; on a put: OAM DMA's write if it has read a byte, else
-- nothing. Each cycle also counts as the DMC's halt, then its dummy, once
-- it is wanted, whatever else the cycle does; a cycle that does nothing
-- else repeats the CPU's read (alignment, halt and dummy cycles).
dmaCycle :: Machine s -> Word16 -> ST s Word8
dmaCycle m a = do
  -- (the DMA unit reads and changes the APU: up to now first)
  syncApu m
  n <- get m iCycles
  due <- apuDmcDma (mApu m)
  st0 <- get m iDmcState
  when (st0 == 0 && maybe False (<= n) due) (set m iDmcState 1)
  st <- get m iDmcState
  halted <- get m iHalted
  oam <- get m iOamOn
  cnt <- get m iOamCount
  -- this cycle counts as the DMC's halt or dummy; an aborted DMA ends
  -- after its halt
  ab <- apuDmcAborted (mApu m)
  if st == 1 && ab
    then set m iDmcState 0 >> apuDmcAbortDone (mApu m)
    else when (st == 1 || st == 2) (set m iDmcState (st + 1))
  v <- if
    | halted == 0 -> set m iHalted 1 >> readMem m a >>= \x -> x <$ set m iIntBus (fromIntegral x)
    | odd n && st == 3 -> do
        da <- apuDmcAddr (mApu m)
        d <- dmaRead m a da
        apuDmcFill (mApu m) d
        set m iDmcState 0
        pure d
    | odd n && oam /= 0 && even cnt -> do
        page <- get m iDmaPage
        d <- dmaRead m a (fromIntegral (page `shiftL` 8 .|. cnt `shiftR` 1))
        set m iDmaVal (fromIntegral d)
        set m iOamCount (cnt + 1)
        pure d
    | even n && oam /= 0 && odd cnt -> do
        d <- fromIntegral <$> get m iDmaVal
        set m iBus (fromIntegral d)
        syncLazy m
        ppuRegWrite (mPpu m) 4 d
        hookEv m (PeWrite 4 d)
        trace m 0x2004 d True
        if cnt == 511 then set m iOamOn 0 >> set m iOamCount 0 else set m iOamCount (cnt + 1)
        pure d
    | otherwise -> readMem m a >>= \x -> x <$ set m iIntBus (fromIntegral x)
  o' <- get m iOamOn
  st' <- get m iDmcState
  when (o' == 0 && st' == 0) (set m iHalted 0)
  pure v

-- | A DMA read at @d@ while the CPU is halted reading @c@. The 2A03 decodes
-- its registers ($4000-$401F) from bits 15-5 of the CPU's address and
-- bits 4-0 of the DMA's (nesdev wiki, "DMA", "Register conflicts";
-- AccuracyCoin, "DMC DMA Bus Conflicts", "Controller Clocking" 7, "APU
-- Register Activation" 4-7). So the registers are active only while the
-- CPU is halted reading $4000-$401F:
--
-- * CPU outside, DMA at $4000-$401F: open bus, no side effects.
-- * CPU inside, DMA elsewhere: the read at @d@ also reads register
--   $4000 + (d & $1F). $4015: the DMA gets the status (bit 5 from the
--   byte on the bus), the bus keeps the byte ($4015 never drives it).
--   $4016/$4017: the controller is clocked and drives its bits over the
--   byte (see 'readBus').
--
-- The $4015 rule is what AccuracyCoin's "APU Register Activation" 5 needs
-- to come back at all: it runs an OAM DMA of page $40 with the CPU halted
-- at $4001, and the last byte the DMA puts on the bus is the opcode the
-- CPU runs there. Status reads of the mirrors ($4035, ...) clear the frame
-- IRQ flag and with it bit 6 of that byte: $00, BRK. With the bus byte
-- instead of the status it stays $40, RTI, and the run-all never finished
-- (step 3.9).
dmaRead :: Machine s -> Word16 -> Word16 -> ST s Word8
dmaRead m c d
  | apuReg c && not (apuReg d) = do
      v <- readMem m d
      let r = 0x4000 .|. (d .&. 0x1F)
      if | r == 0x4015 -> syncApu m >> (\s -> s .|. (v .&. 0x20)) <$> apuReadStatus (mApu m)
         | r == 0x4016 || r == 0x4017 -> do
             -- one access in the trace: the DMA's, with the byte as driven
             on <- get m iTraceOn
             when (on /= 0) (modifySTRef' (mTrace m) (drop 1))
             set m iTraceOn 0
             v' <- readMem m r
             set m iTraceOn on
             trace m d v' False
             pure v'
         | otherwise -> pure v
  | apuReg d && not (apuReg c) = do
      set m iCurRead (fromIntegral d)
      b <- fromIntegral <$> get m iBus
      trace m d b False
      pure b
  | otherwise = readMem m d
 where
  apuReg x = x >= 0x4000 && x < 0x4020

readMem :: Machine s -> Word16 -> ST s Word8
readMem m a = set m iCurRead (fromIntegral a) >> readBus m a

readBus :: Machine s -> Word16 -> ST s Word8
readBus m a = do
  ovl <- readSTRef (mOvl m)
  case ovl of
    -- the ROM overlay answers every read of RAM ($0000-$1FFF) and of
    -- $6000-$FFFF, the cartridge seeing none of them; the registers between
    -- (the PPU's, the APU's, a board's at $4020-$5FFF) answer as ever
    -- (side track N)
    Just (img, end, _) | a < 0x2000 || a >= 0x6000 -> do
      when (a == end) (writeSTRef (mOvl m) (Just (img, end, True)))
      let v = img U.! (fromIntegral a .&. (U.length img - 1))
      set m iBus (fromIntegral v)
      trace m a v False
      pure v
    _ -> readBus' m a

readBus' :: Machine s -> Word16 -> ST s Word8
readBus' m a
  | a < 0x2000 = latch =<< MV.unsafeRead (mRam m) (fromIntegral a .&. 0x7FF)
  | a < 0x4000 = do
      syncLazy m
      v <- ppuRegRead (mPpu m) (fromIntegral a .&. 7)
      hookEv m (PeRead (fromIntegral a .&. 7) v)
      latch v
  | a == 0x4015 = do
      -- bit 5 floats: the CPU's internal bus (AccuracyCoin, "Internal Data
      -- Bus" 2: a DMA just before doesn't change it)
      b <- get m iIntBus
      syncApu m
      s <- apuReadStatus (mApu m)
      let v = s .|. (fromIntegral b .&. 0x20)
      trace m a v False
      pure v
  | a == 0x4016 = joy iShift1 iBtn1 iJoyBit1
  | a == 0x4017 = joy iShift2 iBtn2 iJoyBit2
  | a < 0x4020 = openBus
  | otherwise = do
      -- (the MMC5's status, and its NMI vector read, touch what the PPU's
      -- fetches do: the PPU up to now first)
      when (mapperKind (mCart m) == 5 && (a < 0x6000 || a == 0xFFFA || a == 0xFFFB)) (syncLazy m)
      mapperCpuRead (mCart m) a
      v <- cpuReadCart (mCart m) a
      if v < 0 then openBus else latch (fromIntegral v)
 where
  latch v = do
    set m iBus (fromIntegral v)
    trace m a v False
    pure v
  openBus = get m iBus >>= latch . fromIntegral
  joy sh btn held = do
    strobeOn <- get m iStrobe
    b <- get m iBus
    s <- if strobeOn /= 0 then get m btn else get m sh
    -- The joypads clock on /OE, which stays low over reads of the same
    -- register on consecutive cycles (the DMA's repeated reads): the run
    -- reads one bit, and the register moves on once (AccuracyCoin,
    -- "Controller Clocking" 4 and 5).
    lastA <- get m iLastRead
    bit0 <- if strobeOn /= 0 then pure (s .&. 1)
            else if lastA == fromIntegral a then get m held
            else do
              set m sh ((s `shiftR` 1) .|. 0x80)
              set m held (s .&. 1)
              pure (s .&. 1)
    latch (fromIntegral (bit0 .|. (b .&. 0xE0)))

writeMem :: Machine s -> Word16 -> Word8 -> ST s ()
writeMem m a v = do
  set m iBus (fromIntegral v)
  trace m a v True
  if | a < 0x2000 -> MV.unsafeWrite (mRam m) (fromIntegral a .&. 0x7FF) v
     | a < 0x4000 -> do
         syncLazy m
         -- (MMC5 snoops $2000 and $2001, fully decoded)
         mapperPpuReg (mCart m) a v
         ppuRegWrite (mPpu m) (fromIntegral a .&. 7) v
         hookEv m (PeWrite (fromIntegral a .&. 7) v)
     | a == 0x4014 -> do
         set m iDmaPage (fromIntegral v)
         set m iOamCount 0
         set m iOamOn 1
     | a == 0x4016 -> do
         modifySTRef' (mSerial m) (testBit v 0 :)
         -- The 2A03 passes bit 0 to the controllers' strobe line only when a
         -- get cycle ends ('endCycle'): a write waits for the end of the next
         -- get, and a later write before then replaces it, so a 1-cycle
         -- strobe from DEC $4016 counts only if its 1 is there when a get ends
         -- (AccuracyCoin "Controller Strobing" 3 and 4).
         set m iStrobePend (2 + (fromIntegral v .&. 1))
     | a < 0x4018 -> syncApu m >> apuWrite (mApu m) a v
     | a >= 0x4020 -> do
         -- (banks and mirroring: what the PPU fetches from here on)
         syncLazy m
         c <- get m iCycles
         cpuWriteCart (mCart m) c a v
     | otherwise -> pure ()

-- | The controllers' strobe line: the shift registers follow the buttons
-- while it is high, so they hold the buttons of the moment it falls.
strobe :: Machine s -> Int -> ST s ()
strobe m b = do
  old <- get m iStrobe
  set m iStrobe b
  when (old /= 0 || b /= 0) $ do
    get m iBtn1 >>= set m iShift1
    get m iBtn2 >>= set m iShift2

-- | Bit 0 of every $4016 write so far, oldest first (Blargg's serial
-- output, "Nes.Hooks").
serialBits :: Machine s -> ST s [Bool]
serialBits m = reverse <$> readSTRef (mSerial m)

-- | The first nametable ($2000) as 30 lines of 32 characters, taking each
-- tile number as an ASCII code (Blargg's text consoles use an ASCII
-- font); other bytes become spaces.
nametableText :: Machine s -> ST s [String]
nametableText m = syncLazy m >> mapM row [0 .. 29]
 where
  row r = mapM (\c -> do
                  i <- ciramIndex (mCart m) (0x2000 + fromIntegral (r * 32 + c))
                  b <- MV.unsafeRead (mCiram m) i
                  pure (if b >= 0x20 && b < 0x7F then toEnum (fromIntegral b) else ' '))
              [0 .. 31 :: Int]

-- | (A hook sees every dot as it happens: the PPU runs eagerly while one
-- is set.)
setPpuHook :: Machine s -> Maybe (PpuEvent -> ST s ()) -> ST s ()
setPpuHook m h = do
  syncLazy m
  writeSTRef (mHook m) h
  let k = mapperKind (mCart m)
  set m iLazy (if isJust h then 0 else 1)
  mapperSetRecord (mCart m) (isNothing h && (k == 4 || k == 5))
  set m iFastTill 0

hookEv :: Machine s -> PpuEvent -> ST s ()
hookEv m e = readSTRef (mHook m) >>= maybe (pure ()) ($ e)
{-# INLINE hookEv #-}

-- | The PPU's memory bus below $3F00 without side effects (pattern tables
-- through the mapper, nametables in CIRAM).
ppuBusPeek :: Machine s -> Word16 -> ST s Word8
ppuBusPeek m a = syncLazy m >> peek
 where
  peek
    | a .&. 0x3FFF < 0x2000 = chrRead (mCart m) (a .&. 0x1FFF)
    | otherwise = ciramIndex (mCart m) a >>= MV.unsafeRead (mCiram m)

-- | The APU's outputs after each cycle of the last 'withTrace', oldest
-- first.
apuTrace :: Machine s -> ST s [Channels]
apuTrace m = reverse <$> readSTRef (mApuTr m)

-- * Save states (side track N, "Nes.Spec.SaveState")

-- | The ROM overlay (the PS's restore routine): while set, every CPU read
-- of $0000-$1FFF and $6000-$FFFF answers its byte at the address modulo
-- its size (a power of two); a read of the end address is noted
-- ('overlaySeen').
setOverlay :: Machine s -> Maybe (U.Vector Word8, Word16) -> ST s ()
setOverlay m o = set m iFastTill 0 >> writeSTRef (mOvl m) ((\(img, end) -> (img, end, False)) <$> o)

overlaySeen :: Machine s -> ST s Bool
overlaySeen m = maybe False (\(_, _, s) -> s) <$> readSTRef (mOvl m)

-- | Between two steps: no interrupt about to be taken or owed (the
-- 6502's poll of the last instruction's second-to-last cycle, an NMI edge
-- not taken), the CPU not jammed, no DMA wanted or running, no $4016
-- write on its way to the controllers.
machineQuiet :: Machine s -> ST s Bool
machineQuiet m = do
  pend <- cpuPending (mCpu m)
  jam <- cpuJammed (mCpu m)
  sp <- get m iStrobePend
  h <- get m iHalted
  o <- get m iOamOn
  d <- get m iDmcState
  due <- apuDmcDma (mApu m)
  pure (not pend && not jam && sp == 0 && h == 0 && o == 0 && d == 0 && due == Nothing)

-- | The controllers' strobe line (bit 0 of the last $4016 write that
-- reached them).
machineStrobe :: Machine s -> ST s Int
machineStrobe m = get m iStrobe

-- | The console's 2 KiB of RAM and its 2 KiB of nametable RAM.
ramImage, ciramImage :: Machine s -> ST s (U.Vector Word8)
ramImage m = U.freeze (mRam m)
ciramImage m = syncLazy m >> U.freeze (mCiram m)

-- | The same written (as the PS does through its windows while the core is
-- held).
loadRam, loadCiram :: Machine s -> U.Vector Word8 -> ST s ()
loadRam m = U.imapM_ (MV.write (mRam m))
loadCiram m v = syncLazy m >> U.imapM_ (MV.write (mCiram m)) v

-- * The desktop's sound (side track Q, PLAN.md Q.7)

-- | The APU's outputs after every CPU cycle, kept (on) or not (off): the
-- desktop's sound, without 'withTrace's lists.
setSound :: Machine s -> Bool -> ST s ()
setSound m on = do
  -- (the lazy APU first brought up to now, under the old setting: the
  -- cycles so far keep it, as they did when they ran)
  n <- get m iCycles
  s <- apuCycles (mApu m)
  when (s < n) (syncApuTo m n)
  smp <- get m iApuSmp
  when (smp < n) (packSample m >> set m iApuSmp n)
  set m iSndOn (if on then 1 else 0)

-- | The cycles' outputs since the last call, oldest first: each the two
-- pulses' sum (bits 0-4), the triangle (5-8), the noise (9-12) and the DMC
-- (13-19); at most 'soundCap'.
takeSound :: Machine s -> ST s (U.Vector Int)
takeSound m = do
  -- (the APU up to now, and this cycle's level: its access is over)
  n <- get m iCycles
  s0 <- apuCycles (mApu m)
  when (s0 < n) (syncApuTo m n)
  smp <- get m iApuSmp
  when (smp < n) (packSample m >> set m iApuSmp n)
  k <- get m iSndLen
  set m iSndLen 0
  U.freeze (MV.slice 0 k (mSnd m))

-- | 'takeSound' without the copy (side track W, the browser's): a view of
-- the machine's own buffer, valid until the machine runs again (the
-- desktop library filters it at once). No 240 KB copied every frame.
takeSoundView :: Machine s -> ST s (U.Vector Int)
takeSoundView m = do
  n <- get m iCycles
  s0 <- apuCycles (mApu m)
  when (s0 < n) (syncApuTo m n)
  smp <- get m iApuSmp
  when (smp < n) (packSample m >> set m iApuSmp n)
  k <- get m iSndLen
  set m iSndLen 0
  U.unsafeFreeze (MV.slice 0 k (mSnd m))

-- | CPU cycles of sound kept between two 'takeSound's (a frame is 29781).
soundCap :: Int
soundCap = 65536

-- | Everything that changes while the machine runs, for a save state
-- (Q.8, "Platform.Snapshot"): the CPU, the chips, the cartridge, RAM.
-- Not the traces, the $4016 log or the overlay (tests and the board's
-- restore only).
machineFields :: Machine s -> [Field s]
machineFields m =
  FInt (cpuStateVector (mCpu m)) : ppuFields (mPpu m) <> apuFields (mApu m) <> mapperFields (mCart m)
    <> [ FW8 (mRam m), FW8 (mCiram m), FInt (mSt m), FInt (mSnd m) ]
