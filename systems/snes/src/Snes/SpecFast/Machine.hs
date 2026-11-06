{-# OPTIONS_GHC -fworker-wrapper -fspecialise-aggressively #-}
-- (the fast version is never synthesised: GHC's unboxing back on, which
-- clash-common's -fno-worker-wrapper turns off for Clash's sake)
-- Side track Q (PLAN.md Q.5): the desktop emulator's fast version of
-- Snes.Spec.Machine, kept beside it (the original stays the reference the
-- core's lockstep checks; a fix in either goes to both). Same machine:
-- the snes-specfast suite runs both side by side and compares them.
{-# LANGUAGE MultiWayIf #-}
{-|
The Super Nintendo without its video and sound (PLAN.md step 7.3;
docs/systems/snes.md): the 5A22 (the 65C816 of "W65c816.Spec", its memory
map and access speeds, WRAM and its refresh, the registers at 4200h-421Fh:
NMI, the H/V IRQ timer, multiply and divide, the joypad auto-read; DMA and
HDMA), the cartridge (ROM and SRAM), and the PPU as far as the CPU side
needs it now: the frame's timing (the H and V counters, V-blank) and the
memory ports (VRAM, CGRAM, OAM, the WRAM port), with every other PPU
register only stored, and the sound module ("Snes.Spec.Apu", step 7.4),
caught up to the master clock before each access to its ports and at
each line's end. The picture comes at step 7.5.

__Time__ is master clocks (21.477 MHz). The CPU's cycles take 6, 8 or 12
by address ("Snes.Spec.Map"); a read happens 4 clocks before the end of
its cycle, a write at its end (bsnes's model, *to verify* where a test
shows it). A line is 1364 clocks (340 dots), 1360 for line 240 of every
other non-interlaced frame; 262 lines. The events of a line, at their
master clock (fullsnes and anomie's timing notes; *to verify* each against
undisbeliever's timing tests at 7.5):

> 2      V-blank begins (line 225, or 240 with overscan): the NMI flag; ends at line 0
> 12     line 0: HDMA set-up
> 130    the first V-blank line: the joypad auto-read starts (4224 clocks)
> 538    WRAM refresh: the CPU stops for 40 clocks
> 1116   lines before V-blank: HDMA (after the line's last pixel; anomie: 1104)
> 1120   lines before the last visible one: the next line's sprites
> HTIME*4 + 14 (H or H+V IRQ) / 10 (V IRQ): the IRQ flag

__DMA__ starts at the CPU cycle after the write to 420Bh: the clock is
brought to a multiple of 8, then 8 clocks, then per channel 8 plus 8 per
byte (*to verify*). HDMA's own cost: 18 clocks, 8 per channel, 8 per
byte, 16 per indirect address.
-}
module Snes.SpecFast.Machine
  ( Machine
  , machineFields
  , machineStateSync
  , machineReloaded
  , newMachine
  , newMachineWith
  , setSkipDraw
  , reset
  , stepCpu
  , runFrames
  , runUntilStopped
  , setPads
  , frames
  , masterClocks
  , frameStart
  , cpuStopped
  , readWram
  , readVram
  , readOam
  , screenText
  , cpuRegs
  , apuInputs
  , audioSamples
  , audioTake
  , apuOf
  , frameColors
  , lastFrameColors
  , setFrameLag
  , visibleLines
  , ppuRegisters
  , cgramColors
  , PpuAccess (..)
  , PpuEvent (..)
  , traceOn
  , takeTrace
  , BusEvent (..)
  , busTraceOn
  , takeBusTrace
  ) where

import Prelude

import Control.Monad (forM_, unless, when)
import Control.Monad.ST (ST, fixST)
import Data.Bits
import qualified Data.ByteString as B
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MV
import Platform.Snapshot (Field (..))
import Data.Char (chr)
import Data.STRef
import Data.Word

import Snes.Cart (Cart (..), Mapping (..), stripCopier)
import Snes.SpecFast.Apu
import Snes.Spec.Map
import Snes.SpecFast.Ppu
import Snes.SpecFast.RenderLog
import Snes.SpecFast.Cpu (Bus (..), Kind (..), Cpu, Run (..))
import qualified Snes.SpecFast.Cpu as Cpu
import W65c816.Types (Regs (..), resetRegs)

data Machine s = Machine
  { mRom     :: !(U.Vector Word8)
  , mMapping :: !Mapping
  , mSramSize :: !Int
  , mPages   :: !(MV.MVector s Int)      -- ^ the fast version: the decoded A bus per 256-byte page ('pageEntry'; -1 not yet)
  , mWram    :: !(MV.MVector s Word8)
  , mSram    :: !(MV.MVector s Word8)
  , mVram    :: !(MV.MVector s Word8)   -- ^ 64 KiB; word w = bytes 2w, 2w+1
  , mCgram   :: !(MV.MVector s Word8)   -- ^ 256 words, little-endian
  , mOam     :: !(MV.MVector s Word8)   -- ^ 544 bytes
  , mPpu     :: !(MV.MVector s Int)     -- ^ 2100h-213Fh as last written
  , mDma     :: !(MV.MVector s Word8)   -- ^ 4300h-437Fh
  , mSt      :: !(MV.MVector s Int)
  , mApu     :: !(Apu s)
  , mPicture :: !(Ppu s)
  , mRender  :: !(RenderLog s)            -- ^ the fast version: the drawing on a second core, or inline
  , mSkip    :: !(STRef s Bool)           -- ^ the fast version: the pixels not drawn (frame skipping)
  , mCpu     :: !(Cpu s)
  , mTrace   :: !(STRef s (Maybe [PpuEvent]))   -- ^ newest first, while tracing
  , mBusTrace :: !(STRef s (Maybe [BusEvent]))  -- ^ newest first, while tracing
  }

-- | The 5A22's bus work at its master clock, for the machine's lockstep
-- test ("snes-core", step 7.9): the CPU's cycles (a read and its value, a
-- write, an internal or stopped cycle), DMA's and HDMA's bytes (the A-bus
-- address, the B-bus register, the value, to the A bus?), HDMA's table
-- reads.
data BusEvent
  = BeRead !Int !Int !Word8
  | BeWrite !Int !Int !Word8
  | BeIdle !Int
  | BeDma !Int !Int !Int !Word8 !Bool
  | BeTable !Int !Int !Word8
  deriving stock (Show, Eq)

-- | An access to the PPU's registers (2100h-213Fh) as the lockstep tests
-- ("snes-lockstep", step 7.6) replay it into the Clash PPU: a write; a
-- read (the CPU's open bus then, the value read); the counters latched
-- through WRIO.
data PpuAccess = PpuWrite !Int !Word8 | PpuRead !Int !Word8 !Word8 | PpuLatch
  deriving stock (Show, Eq)

-- | An access at its master clock (and H, V there, as a check); or a
-- frame finished (line 0 begun) and its picture.
data PpuEvent
  = PpuAt !Int !Int !Int !PpuAccess
  | PpuFrameDone !Int !(U.Vector Word16)
  deriving stock (Show)

-- Scalar state.
sMc, sHc, sVc, sField, sFrames, sMdr, sNmitimen, sRdnmi, sTimeup, sWrio,
  sMpya, sMpyb, sDiv, sDivb, sHtime, sVtime, sDmaPending, sHdmaen, sMemsel,
  sRddiv, sRdmpy, sMpyCtr, sDivCtr, sShift, sJoyBase, sPad1, sPad2,
  sJoyBusyUntil, sStrobe, sSer1, sSer2, sVaddr, sVmain, sVlatch, sCgadd,
  sCgLatch, sCgFlip, sOamadd, sOamLatch, sOamRel, sWmadd,
  sHdmaDo, sHdmaDone, sHlatch, sVlatch2, sLatched, sM7Prev, sFrameStart, sHflip, sVflip, sBgPrev,
  sSprBusy, sBgPrev2, sNextEv,
  sCount :: Int
sMc = 0
sHc = 1
sVc = 2
sField = 3
sFrames = 4
sMdr = 5
sNmitimen = 6
sRdnmi = 7
sTimeup = 8
sWrio = 9
sMpya = 10
sMpyb = 11
sDiv = 12
sDivb = 13
sHtime = 14
sVtime = 15
sDmaPending = 16
sHdmaen = 17
sMemsel = 18
sRddiv = 19
sRdmpy = 20
sMpyCtr = 21
sDivCtr = 22
sShift = 23
sJoyBase = 24    -- 4 slots: JOY1-JOY4 (16 bits each)
sPad1 = 28
sPad2 = 29
sJoyBusyUntil = 30
sStrobe = 31
sSer1 = 32
sSer2 = 33
sVaddr = 34
sVmain = 35
sVlatch = 36
sCgadd = 37
sCgLatch = 38
sCgFlip = 39
sOamadd = 40
sOamLatch = 41
sOamRel = 42
sWmadd = 43
sHdmaDo = 52     -- bit per channel
sHdmaDone = 53   -- bit per channel
sHlatch = 54
sVlatch2 = 55
sLatched = 56
sM7Prev = 57
sFrameStart = 58
sHflip = 44
sVflip = 45
sBgPrev = 46
sSprBusy = 59    -- 1 from master clock 1120 to the line's end while the sprite unit runs
sBgPrev2 = 60    -- the HOFS writes' own byte latch (bsnes's bgofsPPU2)
sNextEv = 61     -- the fast version: the next event's master clock, cached (-1: to work out)
sCount = 62

-- | A machine for a boot ROM for the SPC700 (64 bytes), a cartridge (its
-- parsed header) and the ROM file.
newMachine :: B.ByteString -> Cart -> B.ByteString -> ST s (Machine s)
newMachine = newMachineWith 2

-- | The fast version's choice: the pixels drawn by @threads@ render
-- threads ("Snes.SpecFast.RenderLog"; 2 by default), or inline (0), as the
-- reference model draws them.
newMachineWith :: Int -> B.ByteString -> Cart -> B.ByteString -> ST s (Machine s)
newMachineWith threaded ipl cart file = do
  apu <- newApu (U.fromList (B.unpack ipl))
  let rom = U.fromList (B.unpack (fst (stripCopier file)))
  wram <- MV.replicate 0x20000 0x55
  sram <- MV.replicate (max 1 (cartSramSize cart)) 0xFF
  vram <- MV.replicate 0x10000 0
  cg <- MV.replicate 512 0
  oam <- MV.replicate 544 0
  -- 2100h-213Fh as written; the scroll registers 0Dh-14h as their 10-bit
  -- values; 40h, 41h M7HOFS, M7VOFS; 42h the fixed colour (COLDATA)
  ppu <- MV.replicate 0x48 0
  picture <- newPpu vram cg oam ppu
  rlog <- newRenderLog threaded vram cg oam ppu
  dma <- MV.replicate 128 0xFF
  st <- MV.replicate sCount 0
  -- the CPU's bus closes over the machine, which holds the CPU: tied
  -- through fixST (the bus's closures don't look at the machine until
  -- the CPU runs)
  fixST $ \m -> do
    cpu <- Cpu.newCpu Cpu.w65c816 (busFor m) resetRegs
    tr <- newSTRef Nothing
    btr <- newSTRef Nothing
    skipRef <- newSTRef False
    pages <- MV.replicate 65536 (-1)
    pure (Machine rom (cartMapping cart) (cartSramSize cart) pages wram sram vram cg oam ppu dma st apu picture rlog skipRef cpu tr btr)
 where
  busFor m = Bus
    { busRead = \k _ a -> cpuRead m k (fromIntegral a)
    , busWrite = \_ _ a v -> cpuWrite m (fromIntegral a) v
    , busIdle = \_ _ -> cpuIdle m
    , busStopped = cpuIdle m
    , busNmi = nmiLine m
    , busIrq = (/= 0) <$> get m sTimeup
    }

get :: Machine s -> Int -> ST s Int
get m = MV.unsafeRead (mSt m)
{-# INLINE get #-}

set :: Machine s -> Int -> Int -> ST s ()
set m = MV.unsafeWrite (mSt m)
{-# INLINE set #-}

-- | Skip drawing the pixels (the fast version's addition, for frame
-- skipping): the drawing reads the PPU and writes only the frame buffer,
-- so nothing the CPU sees changes; the frame keeps the last picture drawn.
setSkipDraw :: Machine s -> Bool -> ST s ()
setSkipDraw m = writeSTRef (mSkip m)

-- | Power on: the registers their reset values, the CPU through its reset
-- vector (the sound module starts in its boot ROM).
reset :: Machine s -> ST s ()
reset m = do
  set m sWrio 0xFF
  set m sRddiv 0
  set m sRdmpy 0
  Cpu.reset (mCpu m)

-- | Start recording the PPU's accesses (before the first step).
traceOn :: Machine s -> ST s ()
traceOn m = writeSTRef (mTrace m) (Just [])

-- | The accesses recorded so far, oldest first; recording goes on.
takeTrace :: Machine s -> ST s [PpuEvent]
takeTrace m = do
  t <- readSTRef (mTrace m)
  writeSTRef (mTrace m) (Just [])
  pure (maybe [] reverse t)

-- | Start recording the bus work ('BusEvent').
busTraceOn :: Machine s -> ST s ()
busTraceOn m = writeSTRef (mBusTrace m) (Just [])

-- | The bus work recorded so far, oldest first; recording goes on.
takeBusTrace :: Machine s -> ST s [BusEvent]
takeBusTrace m = do
  t <- readSTRef (mBusTrace m)
  writeSTRef (mBusTrace m) (Just [])
  pure (maybe [] reverse t)

{-# INLINE busRecord #-}
busRecord :: Machine s -> (Int -> BusEvent) -> ST s ()
busRecord m f = do
  t <- readSTRef (mBusTrace m)
  case t of
    Nothing -> pure ()
    Just es -> get m sMc >>= \mc -> writeSTRef (mBusTrace m) (Just (f mc : es))

{-# INLINE record #-}
record :: Machine s -> PpuAccess -> ST s ()
record m a = do
  t <- readSTRef (mTrace m)
  case t of
    Nothing -> pure ()
    Just es -> do
      mc <- get m sMc
      hc <- get m sHc
      v <- get m sVc
      writeSTRef (mTrace m) (Just (PpuAt mc hc v a : es))

cpuRegs :: Machine s -> ST s Regs
cpuRegs m = Cpu.getRegs (mCpu m)

-- | The CPU's next instruction (or interrupt, or stopped cycle).
stepCpu :: Machine s -> ST s ()
stepCpu m = () <$ Cpu.step (mCpu m)

cpuStopped :: Machine s -> ST s Bool
cpuStopped m = (== Stopped) <$> Cpu.cpuRun (mCpu m)

frames :: Machine s -> ST s Int
frames m = get m sFrames

masterClocks :: Machine s -> ST s Int
masterClocks m = get m sMc

-- | The master clock the current frame began at.
frameStart :: Machine s -> ST s Int
frameStart m = get m sFrameStart

-- | Runs until @n@ more frames have begun, or the CPU stops.
runFrames :: Machine s -> Int -> ST s ()
runFrames m n = do
  f0 <- frames m
  let go = do
        f <- frames m
        stopped <- cpuStopped m
        unless (f >= f0 + n || stopped) (stepCpu m >> go)
  go

-- | Runs until STP or @n@ frames; whether it stopped.
runUntilStopped :: Machine s -> Int -> ST s Bool
runUntilStopped m n = runFrames m n >> cpuStopped m

-- | Pad 1 and 2, 12 bits each, bit 11 to 0: B, Y, Select, Start, up,
-- down, left, right, A, X, L, R (1 = pressed).
setPads :: Machine s -> Int -> Int -> ST s ()
setPads m p1 p2 = set m sPad1 p1 >> set m sPad2 p2

readWram :: Machine s -> Int -> ST s Word8
readWram m a = MV.read (mWram m) (a .&. 0x1FFFF)

readVram :: Machine s -> Int -> ST s Word8
readVram m a = MV.read (mVram m) (a .&. 0xFFFF)

-- | OAM's 544 bytes (the low table, then the high).
readOam :: Machine s -> Int -> ST s Word8
readOam m a = MV.read (mOam m) a

-- | What the SPC700 shows the CPU in the four ports.
apuInputs :: Machine s -> ST s [Word8]
apuInputs m = syncApu m >> mapM (apuPortRead (mApu m)) [0 .. 3]

-- | Every stereo sample the DSP has made (left, right, ...), up to now.
audioSamples :: Machine s -> ST s (U.Vector Int)
audioSamples m = syncApu m >> apuSamples (mApu m)

-- | The samples since the last call, the buffer emptied (the fast
-- version's addition, for the desktop: "Snes.SpecFast.Apu"'s 'apuTake').
audioTake :: Machine s -> ST s (U.Vector Int)
audioTake m = syncApu m >> apuTake (mApu m)

apuOf :: Machine s -> Apu s
apuOf = mApu

-- | The sound module caught up to the master clock: 1.024 MHz against
-- 21.47727 MHz (236.25 MHz / 11), 5632 / 118125.
syncApu :: Machine s -> ST s ()
syncApu m = do
  mc <- get m sMc
  runApuTo (mApu m) ((mc * 5632) `div` 118125)

-- | BG1's first 32 × 32 tilemap as text: each entry's tile number's low
-- byte as a character (the test ROMs' fonts are ASCII), others as spaces.
screenText :: Machine s -> ST s [String]
screenText m = do
  sc <- MV.read (mPpu m) 0x07
  let base = ((sc `shiftR` 2) .&. 0x3F) * 0x800
  mapM (\row -> mapM (\col -> do
          v <- MV.read (mVram m) ((base + 2 * (row * 32 + col)) .&. 0xFFFF)
          let ch = fromIntegral v :: Int
          pure (if ch >= 0x20 && ch < 0x7F then chr ch else ' ')) [0 .. 31]) [0 .. 31]

-- * Time

vblankStart :: Machine s -> ST s Int
vblankStart m = (\v -> if v .&. 0x04 /= 0 then 240 else 225) <$> MV.unsafeRead (mPpu m) 0x33

lineLength :: Machine s -> ST s Int
lineLength m = do
  v <- get m sVc
  f <- get m sField
  setini <- MV.unsafeRead (mPpu m) 0x33
  pure (if v == 240 && f == 1 && setini .&. 1 == 0 then 1360 else 1364)

linesPerFrame :: Machine s -> ST s Int
linesPerFrame m = do
  f <- get m sField
  setini <- MV.unsafeRead (mPpu m) 0x33
  pure (if setini .&. 1 /= 0 && f == 0 then 263 else 262)

-- | The next master clock in this line where something happens.
nextEvent :: Machine s -> Int -> ST s Int
nextEvent m !hc = do
  -- (the fast version: the same minimum by comparisons, no lists per tick)
  len <- lineLength m
  en <- get m sNmitimen
  ht <- get m sHtime
  let !fixed | hc < 2 = 2 | hc < 12 = 12 | hc < 130 = 130 | hc < 538 = 538
             | hc < 1116 = 1116 | hc < 1120 = 1120 | otherwise = maxBound
      !irq = case en .&. 0x30 of
        0x00 -> maxBound
        0x20 -> 10
        _ -> ht * 4 + 14
      after e = if e > hc then e else maxBound
  pure (min fixed (min (after irq) (after len)))

irqPosition :: Machine s -> ST s (Maybe Int)
irqPosition m = do
  en <- get m sNmitimen
  ht <- get m sHtime
  pure $ case en .&. 0x30 of
    0x00 -> Nothing
    0x20 -> Just 10
    _ -> Just (ht * 4 + 14)

-- | Advance time by @n@ master clocks, with every event on the way.
tick :: Machine s -> Int -> ST s ()
tick m n
  | n <= 0 = pure ()
  | otherwise = do
      hc <- get m sHc
      -- (the fast version: the next event cached, 'sNextEv'; it changes
      -- only when an event fires or NMITIMEN, HTIME or SETINI is written)
      cached <- get m sNextEv
      next <- if cached > hc then pure cached else do
        e <- nextEvent m hc
        set m sNextEv e
        pure e
      let d = min n (next - hc)
      set m sHc (hc + d)
      get m sMc >>= set m sMc . (+ d)
      render m (hc + d)
      when (hc + d == next) (event m next >> set m sNextEv (-1))
      tick m (n - d)

-- | The visible lines (1 to 224, or 239 with overscan) draw their pixels
-- as their dots pass: dot 22 + x is pixel x.
render :: Machine s -> Int -> ST s ()
render m hc = do
  v <- get m sVc
  vbs <- vblankStart m
  when (v >= 1 && v < vbs) $ do
    f <- get m sField
    skip <- readSTRef (mSkip m)
    if skip then pure () else if rlThreaded (mRender m)
      then rlDraw (mRender m) v f (hc `div` 4)
      else ppuRenderTo (mPicture m) v f (hc `div` 4)

-- (side track Q: not inlined into 'tick', whose common path is a few
-- reads and writes; inlined, the rare events made it one large function
-- that saves many registers on every call)
{-# NOINLINE event #-}
event :: Machine s -> Int -> ST s ()
event m hc = do
  v <- get m sVc
  vbs <- vblankStart m
  len <- lineLength m
  if
    | hc == len -> newLine
    | otherwise -> do
        when (hc == 2 && v == vbs) $ do
          set m sRdnmi 1
          -- OAM's address reloads at V-blank (fullsnes)
          get m sOamRel >>= set m sOamadd
        when (hc == 2 && v == 0) $ set m sRdnmi 0
        when (hc == 12 && v == 0) $ hdmaInit m
        when (hc == 130 && v == vbs) $ autoJoypad m
        when (hc == 538) $ tick m 40
        when (hc == 1116 && v < vbs) $ hdmaRun m
        -- the next visible line's sprites, in this line's H-blank (the
        -- chip's time; OAM takes no writes on visible lines)
        -- (in forced blank none: the line buffer is only cleared)
        when (hc == 1120 && v + 1 >= 1 && v + 1 < vbs) $ do
          inidisp <- MV.read (mPpu m) 0x00
          if inidisp .&. 0x80 /= 0 then ppuClearSprites (mPicture m) >> rlClearObj (mRender m)
            else set m sSprBusy 1 >> get m sOamRel >>= ppuSprites (mPicture m) v >> rlObj (mRender m) (mPicture m)
        irqPos <- irqPosition m
        en <- get m sNmitimen
        vt <- get m sVtime
        when (irqPos == Just hc && (en .&. 0x20 == 0 || v == vt)) $ set m sTimeup 1
 where
  newLine = do
    syncApu m
    render m 2000
    set m sHc 0
    set m sSprBusy 0
    v <- get m sVc
    lpf <- linesPerFrame m
    if v + 1 >= lpf
      then do
        set m sVc 0
        get m sField >>= set m sField . xor 1
        get m sFrames >>= set m sFrames . (+ 1)
        -- (the fast version: the render threads keep the frame's rows)
        rlFrameEnd (mRender m)
        get m sMc >>= set m sFrameStart
        t <- readSTRef (mTrace m)
        case t of
          Just es -> do
            mc <- get m sMc
            fr <- rlFrame (mRender m) (mPicture m)
            writeSTRef (mTrace m) (Just (PpuFrameDone mc fr : es))
          Nothing -> pure ()
      else set m sVc (v + 1)
    -- a visible line's sprites, from OAM as it is now
    v' <- get m sVc
    vbs' <- vblankStart m
    when (v' >= 1 && v' < vbs') $ ppuStartLine (mPicture m) >> rlStartLine (mRender m)
    -- the end of V-blank clears STAT77's overflow flags (not in forced blank)
    inidisp <- MV.read (mPpu m) 0x00
    when (v' == 0 && inidisp .&. 0x80 == 0) $ ppuClearFlags (mPicture m)
    -- the line's log to the render thread
    rlLineEnd (mRender m)

-- | The last frame's picture, 512 × 239 15-bit colours (see "Snes.Spec.Ppu").
frameColors :: Machine s -> ST s (U.Vector Word16)
frameColors m = rlFrame (mRender m) (mPicture m)

-- | The fast version: the frame before the latest, as the render threads
-- finished it, without waiting for this one (the desktop's picture one
-- frame late); 'frameColors' when the drawing is inline, or before two
-- frames.
-- | Lets 'lastFrameColors' work (the render threads keep each frame).
setFrameLag :: Machine s -> Bool -> ST s ()
setFrameLag m = rlSetLag (mRender m)

lastFrameColors :: Machine s -> ST s (U.Vector Word16)
lastFrameColors m = rlLastFrame (mRender m) >>= maybe (frameColors m) pure

-- | The PPU's register file (2100h-213Fh as written, then 40h-47h).
ppuRegisters :: Machine s -> ST s [Int]
ppuRegisters m = mapM (MV.read (mPpu m)) [0 .. 0x47]

-- | CGRAM as 256 15-bit colours.
cgramColors :: Machine s -> ST s [Int]
cgramColors m = mapM (\i -> (\lo hi -> fromIntegral lo + 256 * fromIntegral hi) <$> MV.read (mCgram m) (2 * i) <*> MV.read (mCgram m) (2 * i + 1)) [0 .. 255]

-- | 224, or 239 with overscan.
visibleLines :: Machine s -> ST s Int
visibleLines m = subtract 1 <$> vblankStart m

nmiLine :: Machine s -> ST s Bool
nmiLine m = do
  f <- get m sRdnmi
  en <- get m sNmitimen
  pure (f /= 0 && en .&. 0x80 /= 0)

-- | The multiply and divide units, one step per CPU cycle (bsnes's
-- algorithm: the partial results are readable while they run).
aluStep :: Machine s -> ST s ()
aluStep m = do
  mc <- get m sMpyCtr
  when (mc > 0) $ do
    set m sMpyCtr (mc - 1)
    rd <- get m sRddiv
    sh <- get m sShift
    when (rd .&. 1 /= 0) $ get m sRdmpy >>= set m sRdmpy . (\v -> (v + sh) .&. 0xFFFF)
    set m sRddiv (rd `shiftR` 1)
    set m sShift (sh `shiftL` 1)
  dc <- get m sDivCtr
  when (dc > 0) $ do
    set m sDivCtr (dc - 1)
    rd <- get m sRddiv
    sh <- get m sShift
    let rd' = (rd `shiftL` 1) .&. 0xFFFF
        sh' = sh `shiftR` 1
    rm <- get m sRdmpy
    if rm >= sh'
      then set m sRdmpy (rm - sh') >> set m sRddiv (rd' .|. 1)
      else set m sRddiv rd'
    set m sShift sh'

-- * The CPU's bus

-- | Before a CPU cycle: a DMA the last write started.
dmaEdge :: Machine s -> ST s ()
dmaEdge m = do
  p <- get m sDmaPending
  when (p /= 0) $ do
    set m sDmaPending 0
    mc <- get m sMc
    tick m ((8 - mc `mod` 8) `mod` 8)
    tick m 8
    forM_ [0 .. 7] $ \ch -> when (testBit p ch) (dmaChannel m ch)

cpuRead :: Machine s -> Kind -> Int -> ST s Word8
cpuRead m _ a = do
  dmaEdge m
  ms <- get m sMemsel
  let sp = speed (ms /= 0) a
  tick m (sp - 4)
  v <- busReadA m a
  busRecord m (\mc -> BeRead mc a v)
  tick m 4
  aluStep m
  set m sMdr (fromIntegral v)
  pure v

-- | A write cycle. Emulation mode's modify cycle (VDA low) is written too:
-- the 5A22 performs it (fullsnes: the double writes to I/O registers).
cpuWrite :: Machine s -> Int -> Word8 -> ST s ()
cpuWrite m a v = do
  dmaEdge m
  aluStep m
  ms <- get m sMemsel
  tick m (speed (ms /= 0) a)
  set m sMdr (fromIntegral v)
  busRecord m (\mc -> BeWrite mc a v)
  busWriteA m a v

cpuIdle :: Machine s -> ST s ()
cpuIdle m = do
  dmaEdge m
  tick m 6
  busRecord m BeIdle
  aluStep m

-- | The A bus as the CPU (and DMA's A side) sees it.
-- | The fast version: 'decode' once per 256-byte page of the A bus, at
-- power-on. Within a page every target is linear in the address (WRAM,
-- ROM and SRAM are mirrored in pieces of at least 256 bytes when their
-- sizes are multiples of 256), so a page's entry is its kind (bits 0-2)
-- and the offset of its first byte (above): 0 WRAM, 1 ROM, 2 SRAM, 3 the
-- B bus, 4 the CPU's registers, 5 open bus, 7 a page the table doesn't
-- cover (mixed, as 4000h-40FFh with the joypads; or every page for a ROM
-- or SRAM size not a multiple of 256), decoded as the reference model does.
-- Each page's entry is worked out the first time the page is used.
pageEntry :: Mapping -> Int -> Int -> Int -> Int
pageEntry mapping romSize sramSize = entry
 where
  ok = romSize `mod` 256 == 0 && (sramSize == 0 || sramSize `mod` 256 == 0)
  entry pg
    | not ok = 7
    | otherwise =
        let a0 = pg `shiftL` 8
            t0 = decode mapping romSize sramSize a0
            linear k t = case (t0, t) of
              (TWram o0, TWram o) -> o == o0 + k
              (TRom o0, TRom o) -> o == o0 + k
              (TSram o0, TSram o) -> o == o0 + k
              (TBbus o0, TBbus o) -> o == o0 + k
              (TCpuReg o0, TCpuReg o) -> o == o0 + k
              (TOpen, TOpen) -> True
              _ -> False
            uniform = and [ linear k (decode mapping romSize sramSize (a0 + k)) | k <- [1 .. 255] ]
        in if not uniform then 7 else case t0 of
             TWram o -> o `shiftL` 3
             TRom o -> (o `shiftL` 3) .|. 1
             TSram o -> (o `shiftL` 3) .|. 2
             TBbus o -> (o `shiftL` 3) .|. 3
             TCpuReg o -> (o `shiftL` 3) .|. 4
             TOpen -> 5
             TJoy _ -> 7

-- | Where an A-bus address goes: the page table, else 'decode'.
target :: Machine s -> Int -> ST s Target
target m a = do
  let pg = (a `shiftR` 8) .&. 0xFFFF
  e0 <- MV.unsafeRead (mPages m) pg
  e <- if e0 >= 0 then pure e0 else do
    let e1 = pageEntry (mMapping m) (U.length (mRom m)) (mSramSize m) pg
    MV.unsafeWrite (mPages m) pg e1
    pure e1
  let o = (e `shiftR` 3) + (a .&. 0xFF)
  pure $ case e .&. 7 of
       0 -> TWram o
       1 -> TRom o
       2 -> TSram o
       3 -> TBbus o
       4 -> TCpuReg o
       5 -> TOpen
       _ -> decode (mMapping m) (U.length (mRom m)) (mSramSize m) a
{-# INLINE target #-}

busReadA :: Machine s -> Int -> ST s Word8
busReadA m a = target m a >>= \t -> case t of
  TWram o -> MV.read (mWram m) o
  TRom o -> pure (mRom m U.! o)
  TSram o -> MV.read (mSram m) o
  TBbus r -> busReadB m r
  TJoy r -> joyRead m r
  TCpuReg r -> regRead m r
  TOpen -> mdr m

busWriteA :: Machine s -> Int -> Word8 -> ST s ()
busWriteA m a v = target m a >>= \t -> case t of
  TWram o -> MV.write (mWram m) o v
  TRom _ -> pure ()
  TSram o -> MV.write (mSram m) o v
  TBbus r -> busWriteB m r v
  TJoy r -> joyWrite m r v
  TCpuReg r -> regWrite m r v
  TOpen -> pure ()

mdr :: Machine s -> ST s Word8
mdr m = fromIntegral <$> get m sMdr

-- * The B bus: 2100h-21FFh

busReadB :: Machine s -> Int -> ST s Word8
busReadB m r
  | r < 0x40 = do
      o <- mdr m
      v <- busReadB' m r
      record m (PpuRead r o v)
      pure v
  | otherwise = busReadB' m r

busReadB' :: Machine s -> Int -> ST s Word8
busReadB' m r
  | r >= 0x40 && r < 0x80 = syncApu m >> apuPortRead (mApu m) (r .&. 3)
  | r == 0x80 = do
      a <- get m sWmadd
      set m sWmadd ((a + 1) .&. 0x1FFFF)
      MV.read (mWram m) a
  | otherwise = case r of
      0x34 -> mpy 0
      0x35 -> mpy 8
      0x36 -> mpy 16
      0x37 -> latchCounters m >> mdr m
      0x38 -> do
        a <- get m sOamadd
        set m sOamadd ((a + 1) .&. 0x3FF)
        MV.read (mOam m) (oamIndex a)
      0x39 -> vramRead False
      0x3A -> vramRead True
      0x3B -> do
        a <- get m sCgadd
        fl <- get m sCgFlip
        set m sCgFlip (1 - fl)
        when (fl == 1) $ set m sCgadd ((a + 1) .&. 0xFF)
        MV.read (mCgram m) (a * 2 + fl)
      0x3C -> hvRead sHlatch sHflip
      0x3D -> hvRead sVlatch2 sVflip
      0x3E -> (\f -> fromIntegral (f .|. 0x01)) <$> ppuStat77 (mPicture m) -- PPU1 version 1
      0x3F -> do
        l <- get m sLatched
        f <- get m sField
        set m sLatched 0
        set m sHflip 0
        set m sVflip 0
        pure (fromIntegral ((f `shiftL` 7) .|. (l `shiftL` 6) .|. 0x03))
      _ -> mdr m
 where
  mpy sh = do
    a <- MV.read (mPpu m) 0x1B
    b <- MV.read (mPpu m) 0x1C
    let sa = if a >= 0x8000 then a - 0x10000 else a
        hb = b `shiftR` 8
        sb = if hb >= 0x80 then hb - 0x100 else hb
    pure (fromIntegral (((sa * sb) `shiftR` sh) .&. 0xFF))
  vramRead high = do
    l <- get m sVlatch
    vm <- get m sVmain
    let v = if high then l `shiftR` 8 else l .&. 0xFF
    when ((vm .&. 0x80 /= 0) == high) $ do
      a <- get m sVaddr
      lo <- MV.read (mVram m) (vramByte vm a)
      hi <- MV.read (mVram m) (vramByte vm a + 1)
      set m sVlatch (fromIntegral lo .|. fromIntegral hi `shiftL` 8)
      set m sVaddr ((a + vramStep vm) .&. 0x7FFF)
    pure (fromIntegral v)
  -- the latched counters: low byte, then bit 8 (the rest PPU2's open
  -- bus, here the CPU's), each with its own flip-flop (fullsnes)
  hvRead slot ff = do
    v <- get m slot
    fl <- get m ff
    set m ff (1 - fl)
    o <- get m sMdr
    pure (fromIntegral (if fl == 0 then v .&. 0xFF else ((v `shiftR` 8) .&. 1) .|. (o .&. 0xFE)))

busWriteB :: Machine s -> Int -> Word8 -> ST s ()
busWriteB m r v0
  | r >= 0x40 && r < 0x80 = syncApu m >> apuPortWrite (mApu m) (r .&. 3) v0
  | r == 0x80 = do
      a <- get m sWmadd
      set m sWmadd ((a + 1) .&. 0x1FFFF)
      MV.write (mWram m) a v0
  | r == 0x81 = get m sWmadd >>= set m sWmadd . (\a -> (a .&. 0x1FF00) .|. v)
  | r == 0x82 = get m sWmadd >>= set m sWmadd . (\a -> (a .&. 0x100FF) .|. (v `shiftL` 8))
  | r == 0x83 = get m sWmadd >>= set m sWmadd . (\a -> (a .&. 0x0FFFF) .|. ((v .&. 1) `shiftL` 16))
  | r < 0x40 = do
      record m (PpuWrite r v0)
      case r of
        0x02 -> do
          a <- get m sOamRel
          let a' = (a .&. 0x200) .|. (v `shiftL` 1)
          set m sOamRel a' >> set m sOamadd a'
        0x03 -> do
          a <- get m sOamRel
          let a' = ((v .&. 1) `shiftL` 9) .|. (a .&. 0x1FE)
          set m sOamRel a' >> set m sOamadd a'
        0x04 -> oamBusy >>= \busy -> if busy then get m sOamadd >>= set m sOamadd . (\a -> (a + 1) .&. 0x3FF) else do
          a <- get m sOamadd
          if a >= 0x200
            then MV.write (mOam m) (oamIndex a) v0
            else if even a
              then set m sOamLatch v
              else do
                l <- get m sOamLatch
                MV.write (mOam m) (a - 1) (fromIntegral l)
                MV.write (mOam m) a v0
          set m sOamadd ((a + 1) .&. 0x3FF)
        0x15 -> set m sVmain v
        0x16 -> get m sVaddr >>= set m sVaddr . (\a -> (a .&. 0x7F00) .|. v) >> prefetch
        0x17 -> get m sVaddr >>= set m sVaddr . (\a -> (a .&. 0x00FF) .|. ((v .&. 0x7F) `shiftL` 8)) >> prefetch
        0x18 -> vramWrite False
        0x19 -> vramWrite True
        0x21 -> set m sCgadd v >> set m sCgFlip 0
        0x22 -> drawing >>= \busy -> unless busy $ do
          fl <- get m sCgFlip
          if fl == 0 then set m sCgLatch v >> set m sCgFlip 1 else do
            a <- get m sCgadd
            l <- get m sCgLatch
            MV.write (mCgram m) (a * 2) (fromIntegral l)
            MV.write (mCgram m) (a * 2 + 1) (v0 .&. 0x7F)
            rlCg (mRender m) (a * 2) (fromIntegral l)
            rlCg (mRender m) (a * 2 + 1) (v0 .&. 0x7F)
            set m sCgadd ((a + 1) .&. 0xFF)
            set m sCgFlip 0
        _ | r >= 0x1B && r <= 0x20 -> m7Write r
        -- the BG scrolls, written twice (fullsnes, bsnes): HOFS = new << 8
        -- | (old & ~7) | (old2 & 7), VOFS = new << 8 | old; old is the last
        -- byte written to any of them, old2 the last written to a HOFS (a
        -- latch of its own: fullsnes's "HOFS >> 8 & 7" read from the
        -- register masked to 10 bits lost bit 2 of a low byte written
        -- first, and a game's scroll jumped back and forth by 4 pixels,
        -- step 7.12); 210Dh/210Eh are also M7HOFS/M7VOFS through the Mode 7
        -- latch
        _ | r >= 0x0D && r <= 0x14 -> do
              old <- get m sBgPrev
              old2 <- get m sBgPrev2
              let val = if odd r
                    then ((v `shiftL` 8) .|. (old .&. complement 7) .|. (old2 .&. 7)) .&. 0x3FF
                    else ((v `shiftL` 8) .|. old) .&. 0x3FF
              MV.write (mPpu m) r val
              rlReg (mRender m) r val
              set m sBgPrev v
              when (odd r) $ set m sBgPrev2 v
              when (r == 0x0D) $ m7Write 0x40
              when (r == 0x0E) $ m7Write 0x41
        0x32 -> do
          c <- MV.read (mPpu m) 0x42
          let k = v .&. 0x1F
              c1 = if testBit v 5 then (c .&. complement 0x1F) .|. k else c
              c2 = if testBit v 6 then (c1 .&. complement (0x1F `shiftL` 5)) .|. (k `shiftL` 5) else c1
              c3 = if testBit v 7 then (c2 .&. complement (0x1F `shiftL` 10)) .|. (k `shiftL` 10) else c2
          MV.write (mPpu m) 0x42 c3
          rlReg (mRender m) 0x42 c3
        _ -> pure ()
      unless ((r >= 0x1B && r <= 0x20) || (r >= 0x0D && r <= 0x14) || r == 0x32) $ MV.write (mPpu m) r v >> rlReg (mRender m) r v
      -- (SETINI: the line length, so the cached next event)
      when (r == 0x33) $ set m sNextEv (-1)
  | otherwise = pure ()
 where
  v = fromIntegral v0 :: Int
  -- CGRAM takes no writes while a visible line's pixels are drawn (master
  -- clocks 88-1115), OAM none on visible lines, both but in forced blank
  -- (hardware garbles them: the PPU is reading the memory)
  visibleLine = do
    line <- get m sVc
    vbs <- vblankStart m
    inidisp <- MV.read (mPpu m) 0x00
    pure (inidisp .&. 0x80 == 0 && line >= 1 && line < vbs)
  drawing = do
    vis <- visibleLine
    hc <- get m sHc
    pure (vis && hc >= 88 && hc < 1116)
  -- OAM: no writes on visible lines, nor while the sprite unit works out
  -- the next line's sprites (from master clock 1120 to the line's end,
  -- once started: not in forced blank, where it isn't; the circuit reads
  -- OAM and the tiles over that time)
  oamBusy = do
    vis <- visibleLine
    busy <- get m sSprBusy
    pure (vis || busy /= 0)
  m7Write slot = do
    pv <- get m sM7Prev
    MV.write (mPpu m) slot ((v `shiftL` 8) .|. pv)
    rlReg (mRender m) slot ((v `shiftL` 8) .|. pv)
    set m sM7Prev v
  prefetch = do
    a <- get m sVaddr
    vm <- get m sVmain
    lo <- MV.read (mVram m) (vramByte vm a)
    hi <- MV.read (mVram m) (vramByte vm a + 1)
    set m sVlatch (fromIntegral lo .|. fromIntegral hi `shiftL` 8)
  -- VRAM takes writes only in V-blank or forced blank (fullsnes), and
  -- none while the sprite unit reads it (as OAM); the address counts on
  -- either way
  vramWrite high = do
    a <- get m sVaddr
    vm <- get m sVmain
    line <- get m sVc
    vbs <- vblankStart m
    inidisp <- MV.read (mPpu m) 0x00
    busy <- get m sSprBusy
    when (busy == 0 && (inidisp .&. 0x80 /= 0 || line == 0 || line >= vbs)) $
      MV.write (mVram m) (vramByte vm a + (if high then 1 else 0)) v0 >> rlVram (mRender m) (vramByte vm a + (if high then 1 else 0)) v0
    when ((vm .&. 0x80 /= 0) == high) $ set m sVaddr ((a + vramStep vm) .&. 0x7FFF)

-- | VMAIN's increment: 1, 32, 128 words.
vramStep :: Int -> Int
vramStep vm = case vm .&. 3 of 0 -> 1; 1 -> 32; _ -> 128

-- | The byte address of a VRAM word address, after VMAIN's remapping
-- (bits 2-3: rotate the low 8, 9 or 10 bits; fullsnes).
vramByte :: Int -> Int -> Int
vramByte vm a = 2 * (remap .&. 0x7FFF)
 where
  remap = case (vm `shiftR` 2) .&. 3 of
    0 -> a
    1 -> (a .&. 0x7F00) .|. ((a .&. 0x1F) `shiftL` 3) .|. ((a `shiftR` 5) .&. 7)
    2 -> (a .&. 0x7E00) .|. ((a .&. 0x3F) `shiftL` 3) .|. ((a `shiftR` 6) .&. 7)
    _ -> (a .&. 0x7C00) .|. ((a .&. 0x7F) `shiftL` 3) .|. ((a `shiftR` 7) .&. 7)

-- | OAM's 10-bit address to its byte: the low table 0-511, the high table
-- (32 bytes) mirrored over 512-1023.
oamIndex :: Int -> Int
oamIndex a = if a >= 0x200 then 0x200 + (a .&. 0x1F) else a

-- | 2137h's read (and WRIO bit 7 falling): the H and V counters latched.
latchCounters :: Machine s -> ST s ()
latchCounters m = do
  hc <- get m sHc
  v <- get m sVc
  set m sHlatch (hdot hc)
  set m sVlatch2 v
  set m sLatched 1

-- | A master clock in the line as the H counter's dot: 4 clocks a dot,
-- dots 323 and 327 6 (fullsnes).
hdot :: Int -> Int
hdot hc
  | hc < 1292 = hc `div` 4
  | hc < 1298 = 323
  | hc < 1310 = 324 + (hc - 1298) `div` 4
  | hc < 1316 = 327
  | otherwise = 328 + (hc - 1316) `div` 4

-- * The CPU's registers: 4200h-43FFh

regRead :: Machine s -> Int -> ST s Word8
regRead m r = do
  open <- get m sMdr
  if
    | r == 0x4210 -> do
        f <- get m sRdnmi
        set m sRdnmi 0
        pure (fromIntegral ((f `shiftL` 7) .|. (open .&. 0x70) .|. 0x02))
    | r == 0x4211 -> do
        f <- get m sTimeup
        set m sTimeup 0
        pure (fromIntegral ((f `shiftL` 7) .|. (open .&. 0x7F)))
    | r == 0x4212 -> do
        v <- get m sVc
        hc <- get m sHc
        vbs <- vblankStart m
        mc <- get m sMc
        busy <- get m sJoyBusyUntil
        let vb = v >= vbs
            hb = hc < 4 || hc >= 1096
        pure (fromIntegral ((if vb then 0x80 else 0) .|. (if hb then 0x40 else 0)
                            .|. (open .&. 0x3E) .|. (if mc < busy then 1 else 0)))
    | r == 0x4213 -> fromIntegral <$> get m sWrio
    | r == 0x4214 -> fromIntegral . (.&. 0xFF) <$> get m sRddiv
    | r == 0x4215 -> fromIntegral . (`shiftR` 8) <$> get m sRddiv
    | r == 0x4216 -> fromIntegral . (.&. 0xFF) <$> get m sRdmpy
    | r == 0x4217 -> fromIntegral . (`shiftR` 8) <$> get m sRdmpy
    | r >= 0x4218 && r <= 0x421F -> do
        j <- get m (sJoyBase + (r - 0x4218) `div` 2)
        pure (fromIntegral (if even r then j .&. 0xFF else j `shiftR` 8))
    | r >= 0x4300 && r < 0x4380 -> MV.read (mDma m) (r - 0x4300)
    | otherwise -> pure (fromIntegral open)

regWrite :: Machine s -> Int -> Word8 -> ST s ()
regWrite m r v0 = case r of
  0x4200 -> do
    set m sNmitimen v
    set m sNextEv (-1)
    when (v .&. 0x30 == 0) $ set m sTimeup 0
  0x4201 -> do
    old <- get m sWrio
    when (old .&. 0x80 /= 0 && v .&. 0x80 == 0) (record m PpuLatch >> latchCounters m)
    set m sWrio v
  0x4202 -> set m sMpya v
  0x4203 -> do
    set m sRdmpy 0
    busy <- (\a b -> a + b > 0) <$> get m sMpyCtr <*> get m sDivCtr
    unless busy $ do
      a <- get m sMpya
      set m sMpyb v
      set m sRddiv ((v `shiftL` 8) .|. a)
      set m sMpyCtr 8
      set m sShift v
  0x4204 -> get m sDiv >>= set m sDiv . (\d -> (d .&. 0xFF00) .|. v)
  0x4205 -> get m sDiv >>= set m sDiv . (\d -> (d .&. 0x00FF) .|. (v `shiftL` 8))
  0x4206 -> do
    d <- get m sDiv
    set m sRdmpy d
    busy <- (\a b -> a + b > 0) <$> get m sMpyCtr <*> get m sDivCtr
    unless busy $ do
      set m sDivb v
      set m sDivCtr 16
      set m sShift (v `shiftL` 16)
  0x4207 -> get m sHtime >>= set m sHtime . (\h -> (h .&. 0x100) .|. v) >> set m sNextEv (-1)
  0x4208 -> get m sHtime >>= set m sHtime . (\h -> (h .&. 0xFF) .|. ((v .&. 1) `shiftL` 8)) >> set m sNextEv (-1)
  0x4209 -> get m sVtime >>= set m sVtime . (\h -> (h .&. 0x100) .|. v)
  0x420A -> get m sVtime >>= set m sVtime . (\h -> (h .&. 0xFF) .|. ((v .&. 1) `shiftL` 8))
  0x420B -> set m sDmaPending v
  0x420C -> set m sHdmaen v
  0x420D -> set m sMemsel (v .&. 1)
  _ | r >= 0x4300 && r < 0x4380 -> MV.write (mDma m) (r - 0x4300) v0
  _ -> pure ()
 where
  v = fromIntegral v0 :: Int

-- * Joypads

joyRead :: Machine s -> Int -> ST s Word8
joyRead m r = do
  open <- get m sMdr
  let slot = if r == 0x4016 then sSer1 else sSer2
  sr <- get m slot
  st <- get m sStrobe
  pad <- get m (if r == 0x4016 then sPad1 else sPad2)
  -- the pad's serial bit: B first; past 16 bits, 1s
  let b = if st /= 0 then (pad `shiftR` 11) .&. 1 else (sr `shiftR` 15) .&. 1
  unless (st /= 0) $ set m slot (((sr `shiftL` 1) .|. 1) .&. 0xFFFF)
  pure (fromIntegral ((open .&. (if r == 0x4016 then 0xFC else 0xE0)) .|. (if r == 0x4017 then 0x1C else 0) .|. b))

joyWrite :: Machine s -> Int -> Word8 -> ST s ()
joyWrite m r v = when (r == 0x4016) $ do
  st <- get m sStrobe
  let st' = fromIntegral v .&. 1
  set m sStrobe st'
  when (st == 1 && st' == 0) $ do
    get m sPad1 >>= set m sSer1 . padWord
    get m sPad2 >>= set m sSer2 . padWord

-- | A pad's 12 buttons as its 16-bit serial word (B first, then 4 zeros).
padWord :: Int -> Int
padWord p = (p .&. 0xFFF) `shiftL` 4

autoJoypad :: Machine s -> ST s ()
autoJoypad m = do
  en <- get m sNmitimen
  when (en .&. 1 /= 0) $ do
    mc <- get m sMc
    set m sJoyBusyUntil (mc + 4224)
    p1 <- get m sPad1
    p2 <- get m sPad2
    set m sJoyBase (padWord p1)
    set m (sJoyBase + 1) (padWord p2)
    set m (sJoyBase + 2) 0
    set m (sJoyBase + 3) 0
    -- the auto-read leaves the shift registers empty (1s)
    set m sSer1 0xFFFF
    set m sSer2 0xFFFF

-- * DMA

dmaReg :: Machine s -> Int -> Int -> ST s Int
dmaReg m ch i = fromIntegral <$> MV.read (mDma m) (ch * 16 + i)

setDmaReg :: Machine s -> Int -> Int -> Int -> ST s ()
setDmaReg m ch i v = MV.write (mDma m) (ch * 16 + i) (fromIntegral (v .&. 0xFF))

-- | The B-bus offsets of a transfer mode's bytes.
pattern :: Int -> [Int]
pattern mode = case mode .&. 7 of
  0 -> [0]
  1 -> [0, 1]
  2 -> [0, 0]
  3 -> [0, 0, 1, 1]
  4 -> [0, 1, 2, 3]
  5 -> [0, 1, 0, 1]
  6 -> [0, 0]
  _ -> [0, 0, 1, 1]

-- | One byte between the A bus and the B bus (8 clocks). DMA can't reach
-- the B bus or the DMA registers through the A bus: those read open bus
-- (fullsnes).
-- | One byte of DMA or HDMA: the access at the start of its 8 clocks, so
-- the bus never carries two at one master clock (an HDMA ending inside a
-- CPU cycle is followed by the CPU's access 8 clocks after its last).
transfer :: Machine s -> Bool -> Int -> Int -> ST s ()
transfer m toA aAddr bReg = do
  let off = aAddr .&. 0xFFFF
      system = ((aAddr `shiftR` 16) .&. 0x7F) < 0x40
      forbidden = system && ((off >= 0x2100 && off < 0x2200) || (off >= 0x4300 && off < 0x4380))
  if toA
    then do
      v <- busReadB m bReg
      busRecord m (\mc -> BeDma mc aAddr bReg v True)
      unless forbidden (busWriteA m aAddr v)
    else do
      v <- if forbidden then mdr m else busReadA m aAddr
      busRecord m (\mc -> BeDma mc aAddr bReg v False)
      busWriteB m bReg v
  tick m 8

dmaChannel :: Machine s -> Int -> ST s ()
dmaChannel m ch = do
  tick m 8
  dmap <- dmaReg m ch 0
  bbad <- dmaReg m ch 1
  let toA = testBit dmap 7
      step' = if testBit dmap 3 then 0 else if testBit dmap 4 then -1 else 1
      pat = pattern dmap
      go k = do
        lo <- dmaReg m ch 5
        hi <- dmaReg m ch 6
        let count = lo .|. hi `shiftL` 8
        a <- (\l h -> l .|. h `shiftL` 8) <$> dmaReg m ch 2 <*> dmaReg m ch 3
        bank <- dmaReg m ch 4
        transfer m toA (bank `shiftL` 16 .|. a) ((bbad + pat !! (k `mod` length pat)) .&. 0xFF)
        let a' = (a + step') .&. 0xFFFF
        setDmaReg m ch 2 a'
        setDmaReg m ch 3 (a' `shiftR` 8)
        let c' = (count - 1) .&. 0xFFFF
        setDmaReg m ch 5 c'
        setDmaReg m ch 6 (c' `shiftR` 8)
        unless (c' == 0) (go (k + 1))
  go 0

-- * HDMA

hdmaInit :: Machine s -> ST s ()
hdmaInit m = do
  en <- get m sHdmaen
  set m sHdmaDo 0
  set m sHdmaDone 0
  when (en /= 0) $ do
    tick m 18
    forM_ [0 .. 7] $ \ch -> when (testBit en ch) $ do
      tick m 8
      -- the table address from its start
      a1l <- dmaReg m ch 2
      a1h <- dmaReg m ch 3
      setDmaReg m ch 8 a1l
      setDmaReg m ch 9 a1h
      hdmaReload m ch

-- | A new entry from the table: its line count (and for indirect mode
-- its data address); a 0 count ends the channel for this frame.
hdmaReload :: Machine s -> Int -> ST s ()
hdmaReload m ch = do
  n <- tableByte
  setDmaReg m ch 0xA n
  dmap <- dmaReg m ch 0
  if n == 0
    then get m sHdmaDone >>= set m sHdmaDone . (`setBit` ch)
    else do
      when (testBit dmap 6) $ do
        lo <- tableByte
        hi <- tableByte
        setDmaReg m ch 5 lo
        setDmaReg m ch 6 hi
        tick m 16
      get m sHdmaDo >>= set m sHdmaDo . (`setBit` ch)
 where
  tableByte = do
    a <- (\l h -> l .|. h `shiftL` 8) <$> dmaReg m ch 8 <*> dmaReg m ch 9
    bank <- dmaReg m ch 4
    v <- busReadA m (bank `shiftL` 16 .|. a)
    busRecord m (\mc -> BeTable mc (bank `shiftL` 16 .|. a) v)
    let a' = (a + 1) .&. 0xFFFF
    setDmaReg m ch 8 a'
    setDmaReg m ch 9 (a' `shiftR` 8)
    pure (fromIntegral v)

hdmaRun :: Machine s -> ST s ()
hdmaRun m = do
  en <- get m sHdmaen
  done <- get m sHdmaDone
  let active = [ ch | ch <- [0 .. 7], testBit en ch, not (testBit done ch) ]
  unless (null active) $ do
    tick m 18
    forM_ active $ \ch -> do
      tick m 8
      dmap <- dmaReg m ch 0
      bbad <- dmaReg m ch 1
      dos <- get m sHdmaDo
      when (testBit dos ch) $ forM_ (pattern dmap) $ \off -> do
        let indirect = testBit dmap 6
        (aAddr, bump) <- if indirect
          then do
            a <- (\l h -> l .|. h `shiftL` 8) <$> dmaReg m ch 5 <*> dmaReg m ch 6
            bank <- dmaReg m ch 7
            pure (bank `shiftL` 16 .|. a, \a' -> setDmaReg m ch 5 a' >> setDmaReg m ch 6 (a' `shiftR` 8))
          else do
            a <- (\l h -> l .|. h `shiftL` 8) <$> dmaReg m ch 8 <*> dmaReg m ch 9
            bank <- dmaReg m ch 4
            pure (bank `shiftL` 16 .|. a, \a' -> setDmaReg m ch 8 a' >> setDmaReg m ch 9 (a' `shiftR` 8))
        transfer m (testBit dmap 7) aAddr ((bbad + off) .&. 0xFF)
        bump ((aAddr + 1) .&. 0xFFFF)
      -- the line counter: count down; at zero the next entry; the repeat
      -- bit (7) transfers on every line of the entry
      ntrl <- dmaReg m ch 0xA
      let n' = (ntrl - 1) .&. 0xFF
      setDmaReg m ch 0xA n'
      get m sHdmaDo >>= set m sHdmaDo . (\d -> if testBit n' 7 then setBit d ch else clearBit d ch)
      when (n' .&. 0x7F == 0) (hdmaReload m ch)

-- | Everything that changes while the machine runs, for a save state
-- (Q.8, "Platform.Snapshot"): the CPUs, the sound module, the PPU's own
-- buffers, the memories, the registers, DMA. Not the traces, nor the
-- render threads' copies ('machineReloaded' makes them again). Take it
-- after 'machineStateSync'.
machineFields :: Machine s -> [Field s]
machineFields m =
  Cpu.cpuFields (mCpu m) <> apuFields (mApu m) <> ppuOwnFields (mPicture m)
    <> [ FInt (mPages m), FW8 (mWram m), FW8 (mSram m), FW8 (mVram m), FW8 (mCgram m), FW8 (mOam m)
       , FInt (mPpu m), FW8 (mDma m), FInt (mSt m) ]

-- | Before a save state is taken or loaded: the render threads idle.
machineStateSync :: Machine s -> ST s ()
machineStateSync m = rlSync (mRender m)

-- | After a save state is loaded: the render threads' copies of the PPU
-- made the machine's again.
machineReloaded :: Machine s -> ST s ()
machineReloaded m = rlReload (mRender m) (mPicture m)
