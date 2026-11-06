{-# OPTIONS_GHC -fworker-wrapper -fspecialise-aggressively -funfolding-use-threshold=200 -funfolding-creation-threshold=1000 #-}
-- (the fast version is never synthesised: GHC's unboxing back on, which
-- clash-common's -fno-worker-wrapper turns off for Clash's sake)
-- Side track Q (PLAN.md Q.7): the desktop emulator's fast version of
-- Nes.Spec.Machine, kept beside it (the original stays the reference the
-- core's lockstep checks; a fix in either goes to both). Same machine:
-- the nes-specfast suite runs both side by side and compares them.
{-# LANGUAGE MultiWayIf #-}
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
module Nes.SpecFast.Machine
  ( Machine (..)
  , machineFields
  , Access (..)
  , newMachine
  , stepCpu
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
  , soundCap
  ) where

import Prelude

import Control.Monad (when)
import Control.Monad.ST (ST)
import Data.Bits
import qualified Data.ByteString as BS
import Data.STRef
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MV
import Platform.Snapshot (Field (..))
import Data.Word

import Mos6502.SpecFast
import Nes.Cartridge
import Nes.SpecFast.Apu
import Nes.SpecFast.Mapper
import Nes.SpecFast.Ppu

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

-- The interrupt poll as the 6502 records it (side track N's rest).
iPoll1, iPoll2, iNmiPrev, iNmiOwed :: Int
iPoll1 = 22   -- the last cycle's poll: 1 an interrupt would be taken
iPoll2 = 23   -- the cycle before's (the 6502 acts on it at an instruction's end)
iNmiPrev = 24 -- the NMI line at the last cycle's end
iNmiOwed = 25 -- an NMI edge seen and not yet taken (a vector read takes it)

-- The desktop's sound ('setSound').
iSndOn, iSndLen :: Int
iSndOn = 26
iSndLen = 27

slots :: Int
slots = 28

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
        ppu <- newPpu pbus
        apu <- newApu
        -- The machine is the CPU's bus ('CpuBus', below) and holds the CPU:
        -- a recursive let (the CPU's bus field is lazy).
        cst <- newCpuState (Regs 0 0 0 0 0x34 0)
        snd' <- MV.new soundCap
        let m = Machine cpu ppu apu cart ram ciram st tr atr ser hook ovlRef snd'
            cpu = newCpu ricoh2a03 m cst
        reset cpu
        pure (Right m)

-- | The 2A03's side of the CPU's bus (side track Q: a class, so the CPU's
-- code specialises to the machine's; the reference model's is a record).
instance CpuBus Machine where
  busRead = cycleRead
  busWrite = cycleWrite
  busRdy m = not <$> dmaWanted m
  busNmi m = do
    v <- ppuNmi (mPpu m)
    p <- get m iNmiPrev
    when (v && p == 0) (set m iNmiOwed 1)
    set m iNmiPrev (if v then 1 else 0)
    pure v
  busIrq m = do
    irq <- (||) <$> apuIrq (mApu m) <*> mapperIrq (mCart m)
    -- the poll of this cycle (busIrq comes after busNmi), as the 6502
    -- records it
    masked <- if irq then (\r -> rP r .&. flagI /= 0) <$> getRegs (mCpu m) else pure True
    owed <- get m iNmiOwed
    get m iPoll1 >>= set m iPoll2
    set m iPoll1 (if owed /= 0 || not masked then 1 else 0)
    pure irq
  {-# INLINE busRead #-}
  {-# INLINE busWrite #-}
  {-# INLINE busRdy #-}
  {-# INLINE busNmi #-}
  {-# INLINE busIrq #-}

-- | The reset button: CPU reset sequence, PPUCTRL/PPUMASK cleared, APU
-- silenced and its frame counter restarted.
resetMachine :: Machine s -> ST s ()
resetMachine m = do
  ppuRegWrite (mPpu m) 0 0
  ppuRegWrite (mPpu m) 1 0
  apuReset (mApu m)
  set m iHalted 0
  set m iOamOn 0
  reset (mCpu m)

stepCpu :: Machine s -> ST s Step
stepCpu m = step (mCpu m)

machineCycles :: Machine s -> ST s Int
machineCycles m = get m iCycles

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
  r <- act
  set m iTraceOn 0
  t <- readSTRef (mTrace m)
  pure (r, reverse t)

-- * Cycles

startCycle :: Machine s -> Int -> ST s ()
startCycle m ticks = do
  t <- (+ ticks) <$> get m iMaster
  set m iMaster t
  runPpu m (t - alignment)
  c <- get m iCycles
  set m iCycles (c + 1)
  apuCycle (mApu m)
  mapperCycle (mCart m)
  get m iCurRead >>= set m iLastRead
  set m iCurRead (-1)

endCycle :: Machine s -> Int -> ST s ()
endCycle m ticks = do
  t <- (+ ticks) <$> get m iMaster
  set m iMaster t
  runPpu m (t - alignment)
  -- the end of a get cycle (odd numbers; the change to a put): the
  -- controllers see the last $4016 write
  n <- get m iCycles
  sp <- get m iStrobePend
  when (odd n && sp /= 0) (set m iStrobePend 0 >> strobe m (sp - 2))
  on <- get m iTraceOn
  when (on /= 0) (apuChannels (mApu m) >>= \c -> modifySTRef' (mApuTr m) (c :))
  sOn <- get m iSndOn
  when (sOn /= 0) $ do
    k <- get m iSndLen
    when (k < soundCap) $ do
      apuPacked (mApu m) >>= MV.unsafeWrite (mSnd m) k
      set m iSndLen (k + 1)

runPpu :: Machine s -> Int -> ST s ()
runPpu m target = do
  clk0 <- get m iPpuClk
  hook <- readSTRef (mHook m)
  -- (the hook looked up once, not every dot: side track Q)
  let go clk
        | clk + 4 <= target = ppuDot (mPpu m) >> maybe (pure ()) ($ PeDot) hook >> go (clk + 4)
        | otherwise = set m iPpuClk clk
  go clk0

trace :: Machine s -> Word16 -> Word8 -> Bool -> ST s ()
trace m a v w = do
  on <- get m iTraceOn
  when (on /= 0) (modifySTRef' (mTrace m) (Access a v w :))

cycleRead :: Machine s -> Word16 -> ST s Word8
cycleRead m a = do
  startCycle m 5
  dma <- dmaNow m
  v <- if dma then dmaCycle m a else do
         -- a vector read: the NMI owed is taken
         when (a == 0xFFFA || a == 0xFFFC || a == 0xFFFE) (set m iNmiOwed 0)
         x <- readMem m a
         set m iIntBus (fromIntegral x)
         pure x
  endCycle m 7
  -- $2002: the VBlank flag is latched when M2 rises (the access), the
  -- sprite 0 and overflow flags only when it falls, at the end of the
  -- cycle (AccuracyCoin "$2002 flag timing": about 1.9 dots later).
  if dma || a >= 0x4000 || a < 0x2000 || a .&. 7 /= 2 then pure v else do
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
  startCycle m 7
  -- An aborted DMC DMA whose halt falls on a write cycle doesn't happen at
  -- all (a normal one waits for the next read; nesdev wiki "DMA").
  n <- get m iCycles
  due <- apuDmcDma (mApu m)
  ab <- apuDmcAborted (mApu m)
  st <- get m iDmcState
  when (ab && st == 0 && maybe False (<= n) due) (apuDmcAbortDone (mApu m))
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
      if | r == 0x4015 -> (\s -> s .|. (v .&. 0x20)) <$> apuReadStatus (mApu m)
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
      v <- ppuRegRead (mPpu m) (fromIntegral a .&. 7)
      hookEv m (PeRead (fromIntegral a .&. 7) v)
      latch v
  | a == 0x4015 = do
      -- bit 5 floats: the CPU's internal bus (AccuracyCoin, "Internal Data
      -- Bus" 2: a DMA just before doesn't change it)
      b <- get m iIntBus
      s <- apuReadStatus (mApu m)
      let v = s .|. (fromIntegral b .&. 0x20)
      trace m a v False
      pure v
  | a == 0x4016 = joy iShift1 iBtn1 iJoyBit1
  | a == 0x4017 = joy iShift2 iBtn2 iJoyBit2
  | a < 0x4020 = openBus
  | otherwise = do
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
     | a < 0x4018 -> apuWrite (mApu m) a v
     | a >= 0x4020 -> do
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
nametableText m = mapM row [0 .. 29]
 where
  row r = mapM (\c -> do
                  i <- ciramIndex (mCart m) (0x2000 + fromIntegral (r * 32 + c))
                  b <- MV.unsafeRead (mCiram m) i
                  pure (if b >= 0x20 && b < 0x7F then toEnum (fromIntegral b) else ' '))
              [0 .. 31 :: Int]

setPpuHook :: Machine s -> Maybe (PpuEvent -> ST s ()) -> ST s ()
setPpuHook m = writeSTRef (mHook m)

hookEv :: Machine s -> PpuEvent -> ST s ()
hookEv m e = readSTRef (mHook m) >>= maybe (pure ()) ($ e)
{-# INLINE hookEv #-}

-- | The PPU's memory bus below $3F00 without side effects (pattern tables
-- through the mapper, nametables in CIRAM).
ppuBusPeek :: Machine s -> Word16 -> ST s Word8
ppuBusPeek m a
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
setOverlay m o = writeSTRef (mOvl m) ((\(img, end) -> (img, end, False)) <$> o)

overlaySeen :: Machine s -> ST s Bool
overlaySeen m = maybe False (\(_, _, s) -> s) <$> readSTRef (mOvl m)

-- | Between two steps: no interrupt about to be taken or owed (the
-- 6502's poll of the last instruction's second-to-last cycle, an NMI edge
-- not taken), the CPU not jammed, no DMA wanted or running, no $4016
-- write on its way to the controllers.
machineQuiet :: Machine s -> ST s Bool
machineQuiet m = do
  p2 <- get m iPoll2
  owed <- get m iNmiOwed
  jam <- cpuJammed (mCpu m)
  sp <- get m iStrobePend
  h <- get m iHalted
  o <- get m iOamOn
  d <- get m iDmcState
  due <- apuDmcDma (mApu m)
  pure (p2 == 0 && owed == 0 && not jam && sp == 0 && h == 0 && o == 0 && d == 0 && due == Nothing)

-- | The controllers' strobe line (bit 0 of the last $4016 write that
-- reached them).
machineStrobe :: Machine s -> ST s Int
machineStrobe m = get m iStrobe

-- | The console's 2 KiB of RAM and its 2 KiB of nametable RAM.
ramImage, ciramImage :: Machine s -> ST s (U.Vector Word8)
ramImage m = U.freeze (mRam m)
ciramImage m = U.freeze (mCiram m)

-- | The same written (as the PS does through its windows while the core is
-- held).
loadRam, loadCiram :: Machine s -> U.Vector Word8 -> ST s ()
loadRam m = U.imapM_ (MV.write (mRam m))
loadCiram m = U.imapM_ (MV.write (mCiram m))

-- * The desktop's sound (side track Q, PLAN.md Q.7)

-- | The APU's outputs after every CPU cycle, kept (on) or not (off): the
-- desktop's sound, without 'withTrace's lists.
setSound :: Machine s -> Bool -> ST s ()
setSound m on = set m iSndOn (if on then 1 else 0)

-- | The cycles' outputs since the last call, oldest first: each the two
-- pulses' sum (bits 0-4), the triangle (5-8), the noise (9-12) and the DMC
-- (13-19); at most 'soundCap'.
takeSound :: Machine s -> ST s (U.Vector Int)
takeSound m = do
  k <- get m iSndLen
  set m iSndLen 0
  U.freeze (MV.slice 0 k (mSnd m))

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
