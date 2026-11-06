{-|
The Game Gear reference model (PLAN.md steps 5.2 to 5.4;
docs/systems/gamegear.md): 'Z80.Spec' with the memory map, the Sega
mapper, cartridge RAM, the ports, the VDP ('Gg.Spec.Vdp') and the PSG
('Gg.Spec.Psg').

Sources: Sega's Game Gear Hardware Reference Manual (ports 00h to 06h,
CRAM), Charles MacDonald's SMS/GG hardware and VDP notes (port decode,
the VDP's ports), SMS Power's "Mappers" page.

__Time.__ The machine counts the CPU's T-states (one call per machine
cycle, 'Z80.Spec.Bus'). Before every VDP or PSG port access and before
/INT is sampled, the VDP is run up to the same moment (1.5 dots per
T-state) and the PSG ticks once per 16 T-states, so both see every
access at its own T-state. An I/O access is placed at its third T-state
(where the Z80 samples or drives the data, *to verify* against the VDP
test ROMs).

__Memory:__ 0000h-03FFh is always the first 1 KiB of the ROM; 0400h-3FFFh,
4000h-7FFFh and 8000h-BFFFh show the banks in FFFDh, FFFEh and FFFFh
(modulo the ROM's bank count); with FFFCh bit 3 set, 8000h-BFFFh is
cartridge RAM instead (bit 2: which 16 KiB of it). C000h-DFFFh is the
8 KiB RAM, mirrored at E000h-FFFFh; writes to FFFCh-FFFFh go to the
mapper and to the RAM.

__The Codemasters mapper__ (SMS Power "Mappers" and "Codemasters
header", cross-checked with MAME, Genesis Plus GX and MEKA;
gamegear.md, "Codemasters mapper"): writes to exactly 0000h, 4000h and
8000h select the 16 KiB banks of slots 0, 1 and 2 (modulo the ROM's bank
count; no fixed first 1 KiB); power-on banks 0, 1, 0; FFFCh-FFFFh are
only RAM. A write to 4000h with bit 7 set maps 8 KiB of cartridge RAM at
A000h-BFFFh instead (Ernie Els Golf; slot 1's bank is left as it was, as
MAME and MEKA do); bit 7 clear maps the ROM back. 'detectMapper' picks
it for a ROM with a Codemasters header or Codemasters-style bank writes.

__Ports__ (A7, A6 and A0 decoded, plus 00h-06h on the Game Gear): 00h-06h
the Game Gear registers (00h: Start, region; 06h: PSG stereo); 07h-3Fh
writes go to the memory control (even) or I/O control (odd) register,
reads give FFh; 40h-7Fh read the V (even) and H (odd) counters and write
the PSG; 80h-BFh the VDP data (even) and control (odd) ports; C0h-FFh
read the pad (even: DCh) and the TH lines (odd: DDh, bits 6-7); a write to FDh goes to the
SDSC debug console ('takeConsole').

__Audio:__ the PSG's stereo output averaged down to 48 kHz ('takeAudio').

__Master System mode__ ('newMachineSms'; a Game Gear running a Master
System cartridge through an adapter, which ties the cartridge's mode
pin): the VDP in its SMS mode ('Gg.Spec.Vdp'), no Game Gear registers
(ports 00h-06h decode as 00h-3Fh: memory and I/O control), the PSG in
mono (both sides), and Start is the Master System's Pause button, on
/NMI.

__The Master System II__ ('newMasterSystem', M6; docs/systems/
mastersystem.md): the same Master System mode, on the console's own
hardware ('MasterSystem2'; the Game Gear's is 'GameGear', whichever its
mode). What differs, from MacDonald's SMS/GG hardware notes: port 3Eh
(memory control) acts: bit 6 clear enables the cartridge, bit 3 clear the
BIOS (both at once: their bytes ANDed), bit 2 set disables the I/O chip
(C0h-FFh then read FFh); bits 7 and 5 (expansion, card) do nothing on the
SMS 2, and bit 4 (work RAM) is not modelled (RAM always on; unmeasured).
Pad 2 on DCh bits 6-7 (up, down) and DDh bits 0-3 (left, right, 1, 2);
DDh bit 4 reads 1 (the SMS 2 has no Reset button), bit 5 1. Without a BIOS
image the machine starts as the BIOS leaves it: 3Eh = ABh (cartridge,
RAM and I/O on, the rest off), also written to C000h, where the BIOS
keeps its last 3Eh value for the game. With one (the user's own dump,
never committed), it starts in the BIOS: 3Eh = E0h (BIOS, RAM and I/O on;
*to verify*), and the BIOS's 16 KiB banks follow FFFDh-FFFFh as a Sega
mapper's (FFFCh's RAM bits and the fixed first 1 KiB as the cartridge's;
*to verify*), an image under 16 KiB mirrored.
-}
module Gg.Spec
  ( -- * Input
    Input (..)
  , noInput
  , padUp, padDown, padLeft, padRight, pad1, pad2
  , memCtlNoBios
  , memCtlBios
    -- * The machine
  , Machine
  , mCpu
  , machineFields
  , newMachine
  , newMachineSms
  , newMachineWith
  , newMasterSystem
  , newMasterSystemFm
  , FmUnit (..)
  , Console (..)
  , machineConsole
  , Mapper (..)
  , detectMapper
  , machineMapper
  , machineSms
  , frameWidth
  , setInput
  , stepMachine
  , runFrame
  , machineTStates
  , peek
  , vdpRegister
  , vramByte
  , psgRegisters
  , stereo
  , frameBuffer
  , frameBorder
  , takeAudio
  , takeConsole
  , machineFrames
  , psgSnapshot
  , takePsgLog
  , setPsgLog
  , PortAccess (..)
  , setVdpLog
  , takeVdpLog
  , cartRam
  , loadCartRam
    -- * Save states (side track N)
  , specTable
  , specAtRest
  , runOverlay
  , ramImage
  , loadRam
  , vramImage
  , loadVram
    -- * Timing
  , tPerLine
  , linesPerFrame
  , tPerFrame
  , vCounter
  , audioRate
  ) where

import Prelude

import Control.Monad (forM_, when)
import Control.Monad.ST (ST)
import Data.Bits
import qualified Data.ByteString as BS
import Data.Int (Int16)
import Data.Maybe (isJust)
import Data.STRef
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MV
import Platform.Snapshot (Field (..))
import Data.Word

import Gg.StateTable
import Gg.Spec.Psg hiding (psgRegisters)
import qualified Gg.Spec.Psg as Psg
import Gg.Spec.Vdp hiding (vdpRegister)
import qualified Gg.Spec.Vdp as Vdp
import Z80.Spec

-- | The buttons, 'True' = pressed.
data Input = Input
  { inPad   :: !Word8 -- ^ pressed buttons, bits as 'padUp' .. 'pad2'
  , inStart :: !Bool
  , inPad2  :: !Word8 -- ^ the Master System's pad 2, bits as 'inPad' (ignored on a Game Gear)
  } deriving stock (Show, Eq)

noInput :: Input
noInput = Input 0 False 0

-- | Port 3Eh at power-on on the Master System II: as the BIOS leaves it
-- for a cartridge (MacDonald: the SMS 2's Alex Kidd BIOS writes ABh), or,
-- with a BIOS image, the BIOS enabled (*to verify*).
memCtlNoBios, memCtlBios :: Word8
memCtlNoBios = 0xAB
memCtlBios = 0xE0

padUp, padDown, padLeft, padRight, pad1, pad2 :: Word8
padUp = 0x01
padDown = 0x02
padLeft = 0x04
padRight = 0x08
pad1 = 0x10
pad2 = 0x20

-- | CPU T-states per line (342 VDP dots at 1.5 per T-state) and per frame.
tPerLine, tPerFrame :: Int
tPerLine = 228
tPerFrame = tPerLine * linesPerFrame

-- | Where the CPU meets the VDP (calibrated on FluBBa's SMS VDP Test,
-- docs/systems/gamegear.md, "VDP timing"): an I/O access reaches the VDP
-- this many T-states into its 4 T-state machine cycle (the Z80 drives or
-- samples the data at T3); /INT is sampled this many T-states before the
-- end of an instruction (in its last T-state); and the VDP's dot count is
-- 1.5 × T-states plus this phase (0-2 dots).
ioSampleT, intSampleT, dotPhase :: Int
ioSampleT = 3
intSampleT = 1
dotPhase = 1

-- | The audio sample rate of 'takeAudio'.
audioRate :: Int
audioRate = 48000

data Machine s = Machine
  { mCpu     :: Cpu s                  -- ^ lazy: the bus is built before the CPU
  , mRom     :: !(U.Vector Word8)
  , mBanks   :: !Int                   -- ^ 16 KiB banks in the ROM (a power of two)
  , mRam     :: !(MV.MVector s Word8)  -- ^ 8 KiB
  , mCartRam :: !(MV.MVector s Word8)  -- ^ 32 KiB
  , mSms     :: !Bool                  -- ^ Master System mode
  , mHw      :: !Console               -- ^ whose hardware ("Console")
  , mBios    :: !(Maybe (U.Vector Word8)) -- ^ the Master System's BIOS, a multiple of 16 KiB
  , mFm      :: !(Maybe (FmUnit s))    -- ^ the Master System's FM unit, if fitted
  , mMapper  :: !Mapper
  , mVdp     :: !(Vdp s)
  , mPsg     :: !(Psg s)
  , mState   :: !(MV.MVector s Int)    -- ^ see the slots below
  , mInput   :: !(STRef s Input)
  , mAudio   :: !(STRef s [(Int16, Int16)]) -- ^ newest first
  , mConsole :: !(STRef s [Word8])     -- ^ bytes written to port FDh, newest first
  , mVdpLog  :: !(STRef s (Maybe [(Int, PortAccess)])) -- ^ VDP port accesses with their dot, newest first (tests)
  , mPsgLog  :: !(STRef s [(Int, Word8)]) -- ^ PSG writes with the PSG clocks run before them, newest first (while logging)
  , mPsgLogOn :: !(STRef s Bool)
  , mOvl     :: !(STRef s (Maybe Overlay)) -- ^ the ROM overlay while it runs (side track N, 'runOverlay')
  }

-- | The ROM overlay (side track N): its bytes (a power of two), the
-- address whose read ends it, and whether that read has come.
data Overlay = Overlay !(U.Vector Word8) !Word16 !Bool

-- | A CPU access to the VDP's ports, for the lockstep tests' replays.
data PortAccess = AccCtrlW Word8 | AccDataW Word8 | AccDataR | AccStatusR | AccVCountR | AccHCountR | AccLatchH
  deriving stock (Show, Eq)

-- | Start (True) or stop logging the VDP port accesses and PSG writes.
setVdpLog :: Machine s -> Bool -> ST s ()
setVdpLog m on = writeSTRef (mVdpLog m) (if on then Just [] else Nothing) >> setPsgLog m on

-- | Start (True) or stop logging the PSG writes alone.
setPsgLog :: Machine s -> Bool -> ST s ()
setPsgLog m on = writeSTRef (mPsgLogOn m) on >> writeSTRef (mPsgLog m) []

-- | The PSG writes logged so far, oldest first: the PSG clocks run before
-- each, and the byte.
takePsgLog :: Machine s -> ST s [(Int, Word8)]
takePsgLog m = do
  l <- readSTRef (mPsgLog m)
  writeSTRef (mPsgLog m) []
  pure (reverse l)

-- | The accesses logged since the last call, oldest first: the VDP's dot
-- (since power-on) and the access, which the VDP sees before that dot.
takeVdpLog :: Machine s -> ST s [(Int, PortAccess)]
takeVdpLog m = do
  l <- readSTRef (mVdpLog m)
  case l of
    Nothing -> pure []
    Just xs -> writeSTRef (mVdpLog m) (Just []) >> pure (reverse xs)

logVdp :: Machine s -> PortAccess -> ST s ()
logVdp m a = do
  l <- readSTRef (mVdpLog m)
  case l of
    Nothing -> pure ()
    Just xs -> do
      d <- vdpDot (mVdp m)
      writeSTRef (mVdpLog m) (Just ((d, a) : xs))

-- State slots: the mapper (Sega: FFFCh-FFFFh in 0-3; Codemasters: the
-- banks of slots 0-2 in 1-3), the Game Gear registers
-- (ports 00h-06h), memory and I/O control, T-states, PSG clocks run,
-- and the audio resampler (phase, sums, count).
sMap, sGg, sMemCtl, sIoCtl, sT, sPsgTicks, sAPhase, sASumL, sASumR, sACount, sCmRam, sBiosMap, slots :: Int
sMap = 0          -- 4 slots
sGg = 4           -- 7 slots
sMemCtl = 11
sIoCtl = 12
sT = 13
sPsgTicks = 14
sAPhase = 15
sASumL = 16
sASumR = 17
sACount = 18
sCmRam = 19       -- Codemasters: cartridge RAM at A000h-BFFFh
sBiosMap = 20     -- 3 slots: the Master System BIOS's banks (FFFDh-FFFFh)
slots = 23

-- | Game Gear mode, the mapper from 'detectMapper'.
newMachine :: BS.ByteString -> ST s (Machine s)
newMachine rom = newMachineWith False (detectMapper rom) rom

-- | Master System mode, the mapper from 'detectMapper'.
newMachineSms :: BS.ByteString -> ST s (Machine s)
newMachineSms rom = newMachineWith True (detectMapper rom) rom

-- | Whose hardware runs the cartridge.
data Console
  = GameGear       -- ^ in either mode: port 3Eh has no effect, no pad 2
  | MasterSystem2  -- ^ the SMS 2 (M6): port 3Eh, pad 2, a BIOS slot
  deriving stock (Show, Eq)

-- | The Master System's FM unit (M6; the model is "Sms.Spec.FmUnit" in the
-- sms package): its ports F0h-F2h and its sound, at the machine's
-- T-states (the YM2413 runs from the same 3.579545 MHz).
data FmUnit s = FmUnit
  { fuWrite :: Int -> Word8 -> Word8 -> ST s () -- ^ an OUT to F0h, F1h or F2h at a T-state
  , fuRead  :: Int -> ST s Word8                 -- ^ an IN from F2h (with the I/O chip off)
  , fuLevel :: Int -> ST s Int                   -- ^ its output at a T-state, scaled to the PSG's (mono)
  , fuPsgOn :: ST s Bool                         -- ^ F2h lets the PSG through
  }

-- | The Master System II (M6) with its BIOS image, if any, and the
-- cartridge's mapper; no FM unit.
newMasterSystem :: Maybe BS.ByteString -> Mapper -> BS.ByteString -> ST s (Machine s)
newMasterSystem bios = newMasterSystemFm bios Nothing

-- | The Master System II with an FM unit, or none.
newMasterSystemFm :: Maybe BS.ByteString -> Maybe (FmUnit s) -> Mapper -> BS.ByteString -> ST s (Machine s)
newMasterSystemFm bios fm mapper rom = do
  let img b = let n = max 1 ((BS.length b + 0x3FFF) `div` 0x4000) * 0x4000
              in U.fromList (take n (cycle (BS.unpack b)))
  m <- newMachineFull True MasterSystem2 (img <$> bios) fm mapper rom
  case bios of
    Nothing -> setS m sMemCtl (fromIntegral memCtlNoBios) >> MV.write (mRam m) 0 memCtlNoBios
    Just _ -> setS m sMemCtl (fromIntegral memCtlBios)
  pure m

machineConsole :: Machine s -> Console
machineConsole = mHw

-- | The cartridge's mapper.
data Mapper
  = SegaMapper   -- ^ FFFCh-FFFFh (every Sega and most homebrew cartridges)
  | Codemasters  -- ^ 0000h, 4000h, 8000h
  deriving stock (Show, Eq)

machineMapper :: Machine s -> Mapper
machineMapper = mMapper

-- | Which mapper a ROM needs. Emulators recognise Codemasters cartridges
-- by a CRC list (Genesis Plus GX) or by counting bank writes in the first
-- 32 KiB (MAME, MEKA); this takes either sign:
--
-- * a Codemasters header at 7FE0h: the checksum at 7FE6h nonzero, its
--   complement at 7FE8h (the two words sum to 0 modulo 10000h), and
--   7FEAh-7FEFh zero (SMS Power, "Codemasters header");
-- * MAME's and MEKA's count: more @LD (8000h),A@ (32 00 80) than
--   @LD (FFFFh),A@ (32 FF FF) plus 2, or any of the first and none of the
--   second.
detectMapper :: BS.ByteString -> Mapper
detectMapper rom
  | header || writes = Codemasters
  | otherwise = SegaMapper
 where
  byte i = if i < BS.length rom then BS.index rom i else 0
  word i = fromIntegral (byte i) + 256 * fromIntegral (byte (i + 1)) :: Int
  sumW = word 0x7FE6
  header = BS.length rom >= 0x8000 && sumW /= 0
           && (sumW + word 0x7FE8) `mod` 0x10000 == 0
           && all (\i -> byte i == 0) [0x7FEA .. 0x7FEF]
  first = BS.take 0x8000 rom
  n8000 = length (findAll (BS.pack [0x32, 0x00, 0x80]) first)
  nFFFF = length (findAll (BS.pack [0x32, 0xFF, 0xFF]) first)
  writes = n8000 > nFFFF + 2 || (n8000 > 0 && nFFFF == 0)

-- | Every position of @pat@ in @b@.
findAll :: BS.ByteString -> BS.ByteString -> [Int]
findAll pat = go 0
 where
  go off x = case BS.breakSubstring pat x of
    (pre, r) | BS.null r -> []
             | otherwise -> let at = off + BS.length pre in at : go (at + 1) (BS.drop 1 r)

machineSms :: Machine s -> Bool
machineSms = mSms

-- | The frame buffer's width: 160 (the GG window) or 256 (SMS mode).
frameWidth :: Machine s -> Int
frameWidth m = vdpWidth (mVdp m)

-- | A machine with the mode (True: Master System) and mapper given.
newMachineWith :: Bool -> Mapper -> BS.ByteString -> ST s (Machine s)
newMachineWith sms = newMachineFull sms GameGear Nothing Nothing

newMachineFull :: Bool -> Console -> Maybe (U.Vector Word8) -> Maybe (FmUnit s) -> Mapper -> BS.ByteString -> ST s (Machine s)
newMachineFull sms console bios fm mapper romBytes = do
  let banks = max 1 ((BS.length romBytes + 0x3FFF) `div` 0x4000)
      pow2 = until (>= banks) (* 2) 1
      rom = U.fromList (BS.unpack romBytes <> replicate (pow2 * 0x4000 - BS.length romBytes) 0xFF)
  ram <- MV.replicate 0x2000 0
  cart <- MV.replicate 0x8000 0
  vdp <- if sms then newVdpSms else newVdp
  psg <- newPsg
  st <- MV.replicate slots 0
  input <- newSTRef noInput
  audio <- newSTRef []
  cons <- newSTRef []
  vlog <- newSTRef Nothing
  plog <- newSTRef []
  plogOn <- newSTRef False
  ovl <- newSTRef Nothing
  -- The mapper's usual power-on banks and the Game Gear registers' reset
  -- values (MacDonald: C0 7F FF 00 FF 00 FF).
  mapM_ (\(i, v) -> MV.write st (sMap + i) v) $ case mapper of
    SegaMapper -> [(0, 0), (1, 0), (2, 1), (3, 2)]
    Codemasters -> [(0, 0), (1, 0), (2, 1), (3, 0)]
  mapM_ (\(i, v) -> MV.write st (sGg + i) v) (zip [0 ..] [0xC0, 0x7F, 0xFF, 0x00, 0xFF, 0x00, 0xFF])
  MV.write st sIoCtl 0xFF -- every TH/TR an input
  -- the BIOS's banks: FFFDh-FFFFh's power-on values, as the Sega mapper's
  mapM_ (\(i, v) -> MV.write st (sBiosMap + i) v) [(0, 0), (1, 1), (2, 2)]
  let m0 = Machine undefined rom pow2 ram cart sms console bios fm mapper vdp psg st input audio cons vlog plog plogOn ovl
  cpu <- newCpu (bus m0) zeroRegs { rSP = 0xDFF0 }
  pure m0 { mCpu = cpu }

-- | One CPU step ('Z80.Spec.step').
stepMachine :: Machine s -> ST s Step
stepMachine m = step (mCpu m)

-- | Steps until the VDP has completed another frame (the window's last
-- line drawn); the frame buffer then holds it.
runFrame :: Machine s -> ST s ()
runFrame m = do
  f0 <- vdpFrames (mVdp m)
  let go = do
        _ <- stepMachine m
        f <- vdpFrames (mVdp m)
        when (f == f0) go
  go

setInput :: Machine s -> Input -> ST s ()
setInput m = writeSTRef (mInput m)

machineTStates :: Machine s -> ST s Int
machineTStates m = getS m sT

-- | Memory as the CPU sees it, without side effects.
peek :: Machine s -> Word16 -> ST s Word8
peek = readMem

vdpRegister :: Machine s -> Int -> ST s Word8
vdpRegister m = Vdp.vdpRegister (mVdp m)

-- | A VRAM byte (no side effects).
vramByte :: Machine s -> Int -> ST s Word8
vramByte m a = vdpVram (mVdp m) (a .&. 0x3FFF)

-- | Port 06h: the PSG's stereo routing.
stereo :: Machine s -> ST s Word8
stereo m = if mSms m then pure 0xFF else fromIntegral <$> getS m (sGg + 6)

-- | The frame: the 160 × 144 window (256 × 192 in SMS mode),
-- ----BBBBGGGGRRRR per pixel.
frameBuffer :: Machine s -> ST s (U.Vector Word16)
frameBuffer m = vdpFrame (mVdp m)

-- | The border's colour for the last completed frame ('Gg.Spec.Vdp.vdpBorder';
-- the Master System's board picture draws it around the frame).
frameBorder :: Machine s -> ST s Word16
frameBorder m = vdpBorder (mVdp m)

-- | The audio produced since the last call, (left, right) at 'audioRate'.
takeAudio :: Machine s -> ST s [(Int16, Int16)]
takeAudio m = do
  a <- readSTRef (mAudio m)
  writeSTRef (mAudio m) []
  pure (reverse a)

-- | The bytes written to port FDh since the last call: the SDSC debug
-- console (SMS Power, "SDSC Debug Console Specification"), a homebrew
-- convention for printing from a ROM. Writes to C0h-FFh do nothing on the
-- hardware.
takeConsole :: Machine s -> ST s [Word8]
takeConsole m = do
  c <- readSTRef (mConsole m)
  writeSTRef (mConsole m) []
  pure (reverse c)

-- | Frames the VDP has completed (as far as it has run).
machineFrames :: Machine s -> ST s Int
machineFrames m = vdpFrames (mVdp m)

-- | The PSG's registers and LFSR (tests).
psgSnapshot :: Machine s -> ST s ([Int], Int)
psgSnapshot m = (,) <$> Psg.psgRegisters (mPsg m) <*> psgLfsr (mPsg m)

-- | The 32 KiB cartridge RAM (battery-backed in some games).
cartRam :: Machine s -> ST s (U.Vector Word8)
cartRam m = U.freeze (mCartRam m)

loadCartRam :: Machine s -> BS.ByteString -> ST s ()
loadCartRam m b = forM_ (zip [0 .. 0x7FFF] (BS.unpack b)) $ \(i, v) -> MV.write (mCartRam m) i v

psgRegisters :: Machine s -> ST s [Int]
psgRegisters m = Psg.psgRegisters (mPsg m)

-- * State

getS :: Machine s -> Int -> ST s Int
getS m = MV.unsafeRead (mState m)
{-# INLINE getS #-}

setS :: Machine s -> Int -> Int -> ST s ()
setS m = MV.unsafeWrite (mState m)
{-# INLINE setS #-}

tick :: Machine s -> Int -> ST s ()
tick m n = getS m sT >>= setS m sT . (+ n)
{-# INLINE tick #-}

-- | Run the VDP and the PSG up to T-state @t@.
sync :: Machine s -> Int -> ST s ()
sync m t = do
  vdpSync (mVdp m) ((t * 3) `div` 2 + dotPhase)
  done <- getS m sPsgTicks
  let want = t `div` 16
  when (want > done) $ do
    st <- stereo m
    forM_ [done .. want - 1] $ \k -> do
      psgTick (mPsg m)
      (l0, r0) <- psgSample (mPsg m) st
      -- the FM unit, if fitted: its level at this PSG tick, on both sides,
      -- and F2h's say on the PSG (M6)
      (l, r) <- case mFm m of
        Nothing -> pure (l0, r0)
        Just fu -> do
          f <- fuLevel fu (16 * k)
          on <- fuPsgOn fu
          let mix a = fromIntegral (max (-32768) (min 32767 ((if on then fromIntegral a else 0) + f))) :: Int16
          pure (mix l0, mix r0)
      sl <- getS m sASumL
      sr <- getS m sASumR
      n <- getS m sACount
      setS m sASumL (sl + fromIntegral l)
      setS m sASumR (sr + fromIntegral r)
      setS m sACount (n + 1)
      -- 48 kHz from the PSG's 3579545 / 16 Hz
      ph <- (+ (audioRate * 16)) <$> getS m sAPhase
      if ph >= 3579545
        then do
          setS m sAPhase (ph - 3579545)
          sl' <- getS m sASumL
          sr' <- getS m sASumR
          n' <- getS m sACount
          modifySTRef' (mAudio m) ((fromIntegral (sl' `div` n'), fromIntegral (sr' `div` n')) :)
          mapM_ (\i -> setS m i 0) [sASumL, sASumR, sACount]
        else setS m sAPhase ph
    setS m sPsgTicks want

-- * The bus

bus :: Machine s -> Bus s
bus m = Bus
  { busFetch = \a -> ovlFetch m >> busMem m a <* tick m 4
  , busRead = \a -> busMem m a <* tick m 3
  , busWrite = \a v -> writeMem m a v >> tick m 3
  , busIn = \a -> do
      t <- getS m sT
      sync m (t + ioSampleT)
      ioRead m (loB a) <* tick m 4
  , busOut = \a v -> do
      t <- getS m sT
      sync m (t + ioSampleT)
      ioWrite m (loB a) v
      tick m 4
  , busIdle = \_ n -> tick m n
  , busIntAck = pure 0xFF <* tick m 6
  , busInt = do
      t <- getS m sT
      sync m (t - intSampleT)
      vdpInt (mVdp m)
  , busNmi = if mSms m then inStart <$> readSTRef (mInput m) else pure False
  }
 where
  loB = fromIntegral :: Word16 -> Word8

-- | A memory read on the bus: the ROM overlay's byte below C000h while it
-- runs (whatever the mapper and port 3Eh say), else 'readMem'.
busMem :: Machine s -> Word16 -> ST s Word8
busMem m a = do
  o <- readSTRef (mOvl m)
  case o of
    Just (Overlay bytes end _) | a < 0xC000 -> do
      when (a == end) (writeSTRef (mOvl m) (Just (Overlay bytes end True)))
      pure (bytes `U.unsafeIndex` (fromIntegral a .&. (U.length bytes - 1)))
    _ -> readMem m a

-- | An opcode fetch: the overlay ends at the first one after its end
-- address was read (the routine's JP has jumped).
ovlFetch :: Machine s -> ST s ()
ovlFetch m = do
  o <- readSTRef (mOvl m)
  case o of
    Just (Overlay _ _ True) -> writeSTRef (mOvl m) Nothing
    _ -> pure ()

romByte :: Machine s -> Int -> Int -> Word8
romByte m bank off = mRom m `U.unsafeIndex` ((bank `mod` mBanks m) * 0x4000 + off)

readMem :: Machine s -> Word16 -> ST s Word8
readMem m a
  | mHw m == MasterSystem2 && a < 0xC000 = do
      -- port 3Eh: bit 6 clear the cartridge, bit 3 clear the BIOS; both:
      -- ANDed (MacDonald, the SMS 2); neither: FFh (*to verify*)
      ctl <- getS m sMemCtl
      let cartOn = not (testBit ctl 6)
      bios <- case mBios m of
        Just img | not (testBit ctl 3) -> Just <$> biosByte m img a
        _ -> pure Nothing
      cart <- if cartOn then Just <$> cartByte m a else pure Nothing
      pure $ case (cart, bios) of
        (Just c, Just b) -> c .&. b
        (Just c, Nothing) -> c
        (Nothing, Just b) -> b
        (Nothing, Nothing) -> 0xFF
  | otherwise = cartByte m a

-- | The Master System BIOS's byte at an address below C000h: its first 1 KiB
-- fixed, then its banks in 16 KiB slots ('sBiosMap').
biosByte :: Machine s -> U.Vector Word8 -> Word16 -> ST s Word8
biosByte m img a = do
  b <- if a < 0x0400 then pure 0 else getS m (sBiosMap + ai `shiftR` 14)
  let banks = U.length img `div` 0x4000
  pure (img `U.unsafeIndex` ((b `mod` banks) * 0x4000 + (ai .&. 0x3FFF)))
 where
  ai = fromIntegral a :: Int

-- | Memory as the cartridge slot and work RAM answer it.
cartByte :: Machine s -> Word16 -> ST s Word8
cartByte m a = case mMapper m of
  SegaMapper
    | a < 0x0400 -> pure (romByte m 0 ai)
    | a < 0x4000 -> slot 1 >>= \b -> pure (romByte m b ai)
    | a < 0x8000 -> slot 2 >>= \b -> pure (romByte m b (ai - 0x4000))
    | a < 0xC000 -> do
        ctl <- getS m sMap
        if testBit ctl 3
          then MV.unsafeRead (mCartRam m) ((if testBit ctl 2 then 0x4000 else 0) + (ai - 0x8000))
          else slot 3 >>= \b -> pure (romByte m b (ai - 0x8000))
    | otherwise -> ram
  Codemasters
    | a < 0x4000 -> slot 1 >>= \b -> pure (romByte m b ai)
    | a < 0x8000 -> slot 2 >>= \b -> pure (romByte m b (ai - 0x4000))
    | a < 0xC000 -> do
        cr <- getS m sCmRam
        if cr /= 0 && a >= 0xA000
          then MV.unsafeRead (mCartRam m) (ai - 0xA000)
          else slot 3 >>= \b -> pure (romByte m b (ai - 0x8000))
    | otherwise -> ram
 where
  ai = fromIntegral a :: Int
  slot i = getS m (sMap + i)
  ram = MV.unsafeRead (mRam m) (ai .&. 0x1FFF)

writeMem :: Machine s -> Word16 -> Word8 -> ST s ()
writeMem m a v
  | a >= 0xC000 = do
      MV.unsafeWrite (mRam m) (ai .&. 0x1FFF) v
      when (a >= 0xFFFC && mMapper m == SegaMapper) (setS m (sMap + ai - 0xFFFC) (fromIntegral v))
      when (a >= 0xFFFD && mHw m == MasterSystem2) (setS m (sBiosMap + ai - 0xFFFD) (fromIntegral v))
  | mMapper m == Codemasters = case a of
      0x0000 -> setS m (sMap + 1) (fromIntegral v)
      0x4000 -> if testBit v 7
        then setS m sCmRam 1
        else setS m sCmRam 0 >> setS m (sMap + 2) (fromIntegral v)
      0x8000 -> setS m (sMap + 3) (fromIntegral v)
      _ -> do
        cr <- getS m sCmRam
        when (cr /= 0 && a >= 0xA000 && a < 0xC000) $ MV.unsafeWrite (mCartRam m) (ai - 0xA000) v
  | a >= 0x8000 = do
      ctl <- getS m sMap
      when (testBit ctl 3) $
        MV.unsafeWrite (mCartRam m) ((if testBit ctl 2 then 0x4000 else 0) + (ai - 0x8000)) v
  | otherwise = pure ()
 where
  ai = fromIntegral a :: Int

-- * Ports

ioRead :: Machine s -> Word8 -> ST s Word8
ioRead m p
  | p <= 0x06 && not (mSms m) = case p of
      0 -> do
        i <- readSTRef (mInput m)
        r <- getS m sGg
        pure ((fromIntegral r .&. 0x7F) .|. (if inStart i then 0 else 0x80))
      6 -> pure 0xFF
      _ -> fromIntegral <$> getS m (sGg + fromIntegral p)
  | p < 0x40 = pure 0xFF
  | p < 0x80 = if even p
      then logVdp m AccVCountR >> vdpVCounter (mVdp m)
      else logVdp m AccHCountR >> vdpHCounter (mVdp m)
  | p < 0xC0 = if even p
      then logVdp m AccDataR >> vdpDataRead (mVdp m)
      else logVdp m AccStatusR >> vdpStatusRead (mVdp m)
  | mHw m == MasterSystem2 = do
      -- the SMS 2: port 3Eh bit 2 disables the I/O chip (C0h-FFh read FFh);
      -- else even ports read DCh (pad 1, pad 2's up and down), odd DDh (pad
      -- 2's left, right, 1, 2; bit 4 no Reset button, bit 5 1; the TH lines
      -- as on the Game Gear)
      ctl <- getS m sMemCtl
      if testBit ctl 2 then case mFm m of
        -- with the I/O chip off, F2h is the FM unit's (its detection, M6)
        Just fu | p == 0xF2 -> getS m sT >>= \t -> fuRead fu (t + ioSampleT)
        _ -> pure 0xFF
      else do
        i <- readSTRef (mInput m)
        c <- getS m sIoCtl
        let p2 = inPad2 i
            th dir lvl = testBit c dir || testBit c lvl
        pure $ if even p
          then complement ((inPad i .&. 0x3F) .|. ((p2 .&. 0x03) `shiftL` 6))
          else complement ((p2 `shiftR` 2) .&. 0x0F) .&. (0x3F .|. (if th 1 5 then 0x40 else 0) .|. (if th 3 7 then 0x80 else 0))
  | even p = do
      i <- readSTRef (mInput m)
      pure (complement (inPad i) .|. 0xC0)
  | otherwise = do
      -- port DDh: bits 6 and 7 are the TH-A and TH-B lines, which follow
      -- port 3Fh when it drives them (an export unit, as port 00h says;
      -- the region check of Master System software, SMS Power "Region
      -- detection"); pad B and the rest read 1.
      c <- getS m sIoCtl
      let th dir lvl = testBit c dir || testBit c lvl
      pure (0x3F .|. (if th 1 5 then 0x40 else 0) .|. (if th 3 7 then 0x80 else 0))

ioWrite :: Machine s -> Word8 -> Word8 -> ST s ()
ioWrite m p v
  | p <= 0x06 && not (mSms m) = when (p /= 0) (setS m (sGg + fromIntegral p) (fromIntegral v))
  | p < 0x40 = if even p
      then setS m sMemCtl (fromIntegral v)
      else do
        -- I/O control: a TH line going from low to high latches the H
        -- counter (bits 1/3: TH-A/B is an input, pulled high; bits 5/7:
        -- its output level).
        old <- getS m sIoCtl
        setS m sIoCtl (fromIntegral v)
        let th :: Int -> Int -> Int -> Bool
            th c dir lvl = testBit c dir || testBit c lvl
            rises dir lvl = not (th old dir lvl) && th (fromIntegral v) dir lvl
        when (rises 1 5 || rises 3 7) (logVdp m AccLatchH >> vdpLatchHCounter (mVdp m))
  | p < 0x80 = do
      on <- readSTRef (mPsgLogOn m)
      when on $ getS m sPsgTicks >>= \k -> modifySTRef' (mPsgLog m) ((k, v) :)
      psgWrite (mPsg m) v
  | p < 0xC0 = if even p
      then logVdp m (AccDataW v) >> vdpDataWrite (mVdp m) v
      else logVdp m (AccCtrlW v) >> vdpControlWrite (mVdp m) v
  | p >= 0xF0 && p <= 0xF2, mHw m == MasterSystem2, Just fu <- mFm m = do
      -- the FM unit (M6): F0h address, F1h data, F2h audio control
      t <- getS m sT
      fuWrite fu (t + ioSampleT) p v
  | p == 0xFD = modifySTRef' (mConsole m) (v :) -- the SDSC debug console (no effect on hardware)
  | otherwise = pure ()

-- * Save states (side track N, PLAN.md N.1)

-- | The 8 KiB RAM, and all of it at once (the PS's windows).
ramImage :: Machine s -> ST s (U.Vector Word8)
ramImage m = U.freeze (mRam m)

loadRam :: Machine s -> U.Vector Word8 -> ST s ()
loadRam m img = U.copy (mRam m) (U.take 0x2000 img)

vramImage :: Machine s -> ST s (U.Vector Word8)
vramImage m = U.generateM 0x4000 (vdpVram (mVdp m))

loadVram :: Machine s -> U.Vector Word8 -> ST s ()
loadVram m = vdpLoadVram (mVdp m)

-- | The state table of a machine between two steps ("Gg.StateTable"),
-- the VDP and the PSG brought up to now first.
specTable :: forall s. Machine s -> ST s (U.Vector Word8)
specTable m = do
  t <- getS m sT
  sync m t
  r <- getRegs (mCpu m)
  tb <- MV.replicate tableSize 0
  let put :: Int -> Int -> ST s ()
      put o v = MV.write tb o (fromIntegral v)
      put16 o v = put o (v .&. 0xFF) >> put (o + 1) ((v `shiftR` 8) .&. 0xFF)
      b x = if x then 1 else 0
  mapM_ (\(o, v) -> put o (fromIntegral v))
    [ (tA, rA r), (tF, rF r), (tB, rB r), (tC, rC r), (tD, rD r), (tE, rE r), (tH, rH r), (tL, rL r)
    , (tI, rI r), (tR, rR r), (tIM, rIM r) ]
  mapM_ (\(o, v) -> put16 o (fromIntegral v))
    [ (tAF', rAF' r), (tBC', rBC' r), (tDE', rDE' r), (tHL', rHL' r), (tIX, rIX r), (tIY, rIY r)
    , (tSP, rSP r), (tPC, rPC r), (tWZ, rWZ r) ]
  put tHalted (b (rHalted r))
  put tIFF1 (b (rIFF1 r))
  put tIFF2 (b (rIFF2 r))
  put tFlags (b (mSms m) .|. (b (mHw m == MasterSystem2) `shiftL` 1) .|. (b (mMapper m == Codemasters) `shiftL` 2)
              .|. (b (isJust (mBios m)) `shiftL` 3) .|. (b (isJust (mFm m)) `shiftL` 4))
  let ph = t `mod` tPhaseFrames
  put tTime (ph .&. 0xFF) >> put (tTime + 1) ((ph `shiftR` 8) .&. 0xFF) >> put (tTime + 2) (ph `shiftR` 16)
  mapM_ (\i -> getS m (sMap + i) >>= put (tMap + i)) [0 .. 3]
  getS m sCmRam >>= put tCmRam
  getS m sMemCtl >>= put tMemCtl
  getS m sIoCtl >>= put tIoCtl
  mapM_ (\i -> MV.read (mRam m) (0x1FFC + i) >>= put (tRamTail + i) . fromIntegral) [0 .. 3]
  mapM_ (\i -> getS m (sGg + i) >>= put (tGgRegs + i)) [0 .. 6]
  mapM_ (\i -> getS m (sBiosMap + i) >>= put (tBiosMap + i)) [0 .. 2]
  mapM_ (\i -> Vdp.vdpRegister (mVdp m) i >>= put (tVdpRegs + i) . fromIntegral) [0 .. 10]
  snap <- vdpSnapshot (mVdp m)
  case snap of
    [addr, code, latched, latch, buf, cramL, status, linePend, lineCnt, hLatch, _dot] -> do
      put16 tVdpAddr addr
      mapM_ (uncurry put)
        [ (tVdpCode, code), (tVdpLatched, latched), (tVdpLatch, latch), (tVdpBuf, buf), (tVdpCramLatch, cramL)
        , (tVdpStatus, status), (tVdpLinePend, linePend), (tVdpLineCnt, lineCnt), (tVdpHLatch, hLatch) ]
    _ -> pure ()
  pr <- Psg.psgRegisters (mPsg m)
  mapM_ (\(c, v) -> put16 (tPsgPeriod + 2 * c) v) (zip [0 .. 2] (take 3 pr))
  put tPsgNoise (pr !! 3)
  mapM_ (\(c, v) -> put (tPsgVol + c) v) (zip [0 .. 3] (drop 4 pr))
  psgLatched (mPsg m) >>= put tPsgLatch
  psgLfsr (mPsg m) >>= put16 tPsgLfsr
  mapM_ (\i -> vdpCram (mVdp m) i >>= put (tCram + i) . fromIntegral) [0 .. 63]
  U.freeze tb

-- | Whether a state can be taken now (between two steps): the CPU at an
-- instruction's start or halted (on a HALT at PC - 1, which the routine
-- jumps back onto: a game waiting for its interrupt is halted through
-- most of vertical blank), no EI pending, IFF1 = IFF2 (not inside
-- the Master System's pause NMI), no interrupt about to be taken, no NMI
-- edge (the pause button up), and the VDP in vertical blank on lines 196
-- to 259 (two lines after the frame interrupt's, before the last: so
-- nothing sets a VDP flag during the routine's tail, about a line long).
specAtRest :: Machine s -> ST s Bool
specAtRest m = do
  t <- getS m sT
  sync m t
  r <- getRegs (mCpu m)
  int <- vdpInt (mVdp m)
  nmi <- busNmi (bus m)
  d <- vdpDot (mVdp m)
  onHalt <- (== 0x76) <$> readMem m (rPC r - 1)
  let line = (d `mod` dotsPerFrame) `div` dotsPerLine
  pure $ (not (rHalted r) || onHalt) && not (rEI r) && rIFF1 r == rIFF2 r && not (rIFF1 r && int) && not nmi
    && line >= 196 && line <= 259

-- | Run a machine through a ROM overlay (the PS's restore routine): every
-- memory read below C000h answers the overlay's byte (at the address
-- modulo its size) until the read of @end@ and the instruction it ends;
-- the machine then stands before its next fetch, from its own memory.
-- 'Left' if the routine didn't end within @limit@ instructions; else the
-- T-states it ran.
runOverlay :: Machine s -> U.Vector Word8 -> Word16 -> Int -> ST s (Either String Int)
runOverlay m ovl end limit = do
  t0 <- getS m sT
  writeSTRef (mOvl m) (Just (Overlay ovl end False))
  let go k
        | k >= limit = writeSTRef (mOvl m) Nothing >> pure (Left ("the restore routine didn't end within " <> show limit <> " instructions"))
        | otherwise = do
            _ <- stepMachine m
            o <- readSTRef (mOvl m)
            case o of
              Just (Overlay _ _ True) -> do
                writeSTRef (mOvl m) Nothing
                Right . subtract t0 <$> getS m sT
              _ -> go (k + 1)
  go (0 :: Int)

-- | Everything that changes while the machine runs, for a save state
-- (side track Q, Q.8: gg-run's command mode, the user 2026-10-26;
-- "Platform.Snapshot"): the Z80, RAM, cartridge RAM, the VDP, the PSG,
-- the machine's slots (the mapper's banks with them). Not the FM unit
-- (behind its calls; gg-run refuses states with it), the input (set every
-- frame), the samples and the console (taken every frame), the logs or
-- the overlay.
machineFields :: Machine s -> [Field s]
machineFields m =
  FInt (cpuStateVector (mCpu m)) : FW8 (mRam m) : FW8 (mCartRam m) : vdpFields (mVdp m)
    <> [psg (mPsg m), FInt (mState m)]
 where
  psg (Psg v) = FInt v
