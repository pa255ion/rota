{-# LANGUAGE BangPatterns #-}
{-|
The NES behind a C ABI, for the desktop emulator (side track Q, PLAN.md
Q.4), as "Gb.Desktop" is for the Game Boy: the Zig front end
(platform/desktop/) calls these after @hs_init@, from one thread. It runs
the fast version ("Nes.SpecFast.Machine", Q.7), which the nes-specfast
suite checks against the reference model ("Nes.Spec.Machine")
instruction by instruction; the desktop's --reference runs the latter.

* @nes_new@ makes a machine from an iNES file.
* @nes_frame@ runs it to the PPU's next frame with controller 1's buttons
  (bit 0 A, 1 B, 2 Select, 3 Start, 4 Up, 5 Down, 6 Left, 7 Right, as
  'setButtons'), sampling the sound at 48 kHz; @nes_audio@ hands the
  samples over (mono, the same on both channels).
* @nes_pixels@ is the last frame, 256 × 240 pixels of 0xFFRRGGBB through
  "Nes.Palette" (the goldens' colours).
* @nes_state_save@, @nes_state_get@, @nes_state_load@: save states (Q.8,
  "Platform.Snapshot"; the fast version's: the reference model has none,
  save gives 0 and load -1), as "Gb.Desktop"'s.

The sound: the five channels' levels at every CPU cycle (the fast
version's 'takeSound'; the reference model's APU trace, 'withTrace' /
'apuTrace'), through the hardware's mixer table
("Nes.Mixer"'s 'mixLevel', 0 to 32767), then the board's filter
("Nes.Audio"'s 'audioFilter': two low-passes and a DC high-pass, there
one step per pixel clock, here the same per CPU cycle), sampled at 48
kHz. And as the board's speaker path ("Nes.Apu"'s 'apuAudioChannels',
M3 step 3.9): a triangle at timer period 0 or 1 (28 to 56 kHz, how games
silence it) gives its mean, 7. The filter alone leaves it at about -42
dBFS, which the 48 kHz sampling folds to a 7.9 kHz whine (the user heard
it, 2026-10-20). The period is read off the trace: at period 0 or 1 the
triangle's level changes every one or two CPU cycles (four at its ends,
where a step repeats), at period 2 or more every three or more (six at
the ends); so a change at most two cycles after the last one sets
"ultrasonic", a gap of five or more clears it.
-}
module Nes.Desktop () where

import Prelude

import Control.Monad (when)
import Control.Monad.ST (RealWorld, ST, stToIO)
import qualified Data.ByteString as BS
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.Int (Int16)
import Data.STRef
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import Data.Word (Word16, Word32, Word64, Word8)
import Foreign.C.Types (CInt (..), CSize (..))
import Foreign.Ptr (Ptr)
import Foreign.StablePtr
import Foreign.Storable (peekElemOff, poke, pokeElemOff)

import Nes.Mixer (mixLevel)
import Nes.Palette (paletteRgb)
import Nes.Spec.Apu (Channels (..))
import qualified Nes.Spec.Machine as S
import Nes.Spec.Ppu (ppuFrame, ppuFramesDone)
import qualified Nes.SpecFast.Machine as F
import qualified Nes.SpecFast.Ppu as FP
import Platform.Snapshot (Field (..), fnv1a, restore, snapshot)

-- | What the library needs of a machine: the fast version's or the
-- reference model's ('fastOps', 'refOps').
data Ops = Ops
  { oFrame :: Word8 -> ST RealWorld (U.Vector Int)  -- ^ the buttons, one frame; its cycles' APU outputs, packed as 'F.takeSound'
  , oPicture :: ST RealWorld (U.Vector Word16)
  , oSkip :: Bool -> ST RealWorld ()  -- ^ frame skipping (the fast version only)
  , oFields :: [Field RealWorld]      -- ^ for save states (the fast version only; none: no states)
  }

fastOps :: F.Machine RealWorld -> Ops
fastOps m = Ops
  { oFrame = \pad -> do
      F.setButtons m 1 pad
      f0 <- FP.ppuFramesDone (F.mPpu m)
      c0 <- F.machineCycles m
      let go = do
            _ <- F.stepCpu m
            f <- FP.ppuFramesDone (F.mPpu m)
            c <- F.machineCycles m
            when (f == f0 && c - c0 < frameCap) go
      go
      F.takeSound m
  , oPicture = FP.ppuFrame (F.mPpu m)
  , oSkip = FP.ppuSetSkip (F.mPpu m)
  , oFields = F.machineFields m
  }

refOps :: S.Machine RealWorld -> Ops
refOps m = Ops
  { oFrame = \pad -> do
      S.setButtons m 1 pad
      f0 <- ppuFramesDone (S.mPpu m)
      c0 <- S.machineCycles m
      out <- newSTRef []
      let go = do
            _ <- S.withTrace m (S.stepCpu m)
            S.apuTrace m >>= \t -> modifySTRef' out (reverse t ++)
            f <- ppuFramesDone (S.mPpu m)
            c <- S.machineCycles m
            when (f == f0 && c - c0 < frameCap) go
      go
      U.fromList . map pack . reverse <$> readSTRef out
  , oPicture = ppuFrame (S.mPpu m)
  , oSkip = const (pure ())
  , oFields = []
  }
 where
  pack (Channels p1 p2 t n dm) =
    (fromIntegral p1 + fromIntegral p2) .|. (fromIntegral t `shiftL` 5) .|. (fromIntegral n `shiftL` 9) .|. (fromIntegral dm `shiftL` 13)

data Desk = Desk
  { dOps     :: !Ops
  , dAcc     :: !(STRef RealWorld Int)            -- ^ 48 kHz phase, in units of 1/48000 CPU cycle
  , dFilt    :: !(STRef RealWorld Filt)             -- ^ the board's filter
  , dAudio   :: !(MU.MVector RealWorld Int16)     -- ^ this frame's samples, left and right
  , dCount   :: !(STRef RealWorld Int)
  , dBlob    :: !(STRef RealWorld BS.ByteString) -- ^ the last save state taken
  , dId      :: !Word64                          -- ^ the game: 'fnv1a' of the file
  }

-- | Stereo samples kept per frame (a frame is about 799).
audioCap :: Int
audioCap = 2048

-- | CPU cycles a frame may take before 'nes_frame' returns anyway (one
-- frame is about 29781).
frameCap :: Int
frameCap = 2 * 29781

-- | The NTSC CPU's clock: 21477272 Hz / 12.
cpuHz :: Int
cpuHz = 1789773

foreign export ccall nes_new :: Ptr Word8 -> CSize -> CInt -> Ptr (StablePtr ()) -> IO CInt
foreign export ccall nes_free :: StablePtr () -> IO ()
foreign export ccall nes_frame :: StablePtr () -> Word8 -> IO CInt
foreign export ccall nes_audio :: StablePtr () -> Ptr Int16 -> CInt -> IO CInt
foreign export ccall nes_pixels :: StablePtr () -> Ptr Word32 -> IO ()
foreign export ccall nes_skip :: StablePtr () -> CInt -> IO ()
foreign export ccall nes_state_save :: StablePtr () -> IO CSize
foreign export ccall nes_state_get :: StablePtr () -> Ptr Word8 -> CSize -> IO ()
foreign export ccall nes_state_load :: StablePtr () -> Ptr Word8 -> CSize -> IO CInt

desk :: StablePtr () -> IO Desk
desk = deRefStablePtr . castPtrToStablePtr . castStablePtrToPtr

-- | 0 and the handle in @out@, or -1 (the file refused: its header, an
-- unsupported mapper); @reference@ 1 runs the reference model (the
-- desktop's --reference).
nes_new :: Ptr Word8 -> CSize -> CInt -> Ptr (StablePtr ()) -> IO CInt
nes_new p n reference out = do
  rom <- BS.pack <$> mapM (peekElemOff p) [0 .. fromIntegral n - 1]
  eops <- stToIO $ if reference /= 0
    then fmap refOps <$> S.newMachine rom
    else do
      em <- F.newMachine rom
      case em of
        Left e -> pure (Left e)
        Right m -> Right (fastOps m) <$ F.setSound m True
  case eops of
    Left _ -> pure (-1)
    Right ops -> do
      d <- stToIO (Desk ops <$> newSTRef 0 <*> newSTRef (Filt start start start 15 0 False) <*> MU.new (2 * audioCap) <*> newSTRef 0
                     <*> newSTRef BS.empty <*> pure (fnv1a rom))
      sp <- newStablePtr d
      poke out (castPtrToStablePtr (castStablePtrToPtr sp))
      pure 0

nes_free :: StablePtr () -> IO ()
nes_free = freeStablePtr

-- | Runs to the PPU's next frame (or 'frameCap' CPU cycles) with the
-- buttons @pad@; the number of stereo samples it made.
nes_frame :: StablePtr () -> Word8 -> IO CInt
nes_frame sp pad = do
  d <- desk sp
  stToIO $ do
    writeSTRef (dCount d) 0
    levels <- oFrame (dOps d) pad
    cyclesOut d levels
    fromIntegral <$> readSTRef (dCount d)

-- | "Nes.Audio"'s filter state: the two low-passes and the DC level; the
-- triangle's last level, the cycles since it changed, and whether it is
-- ultrasonic.
data Filt = Filt !Double !Double !Double !Int !Int !Bool
  deriving (Show, Read)

-- | The filter's level at power-on, as the board's: the triangle holds
-- 15, the rest 0 (no thump).
start :: Double
start = fromIntegral (tndLevels U.! (15 * 2048))

-- | The board's filter steps per CPU cycle: its pixel clock (about 25.2
-- MHz) over the CPU's.
perCycle :: Double
perCycle = 25.2e6 / fromIntegral cpuHz

-- | One step of 'audioFilter' (alpha 2^-8, 2^-8, 2^-18 per pixel clock),
-- as per CPU cycle: 1 - (1 - alpha)^perCycle.
lpA, dcA :: Double
lpA = 1 - (1 - 1 / 256) ** perCycle
dcA = 1 - (1 - 1 / 262144) ** perCycle

-- | The frame's CPU cycles (their levels packed as 'F.takeSound'), one
-- strict loop: each through the filter, and a sample at each 48 kHz
-- instant it passes.
cyclesOut :: Desk -> U.Vector Int -> ST RealWorld ()
cyclesOut d lv = do
  Filt a0 b0 c0 lt0 gap0 ultra0 <- readSTRef (dFilt d)
  acc0 <- readSTRef (dAcc d)
  k0 <- readSTRef (dCount d)
  let n = U.length lv
      go !i !a !b !c !lt !gap !ultra !acc !k
        | i >= n = do
            writeSTRef (dFilt d) (Filt a b c lt gap ultra)
            writeSTRef (dAcc d) acc
            writeSTRef (dCount d) k
        | otherwise = do
            let w = U.unsafeIndex lv i
                p = w .&. 31
                t = (w `shiftR` 5) .&. 15
                ns = (w `shiftR` 9) .&. 15
                dm = (w `shiftR` 13) .&. 127
                changed = t /= lt
                gap' = if changed then 0 else gap + 1
                ultra'
                  | changed && gap + 1 <= 2 = True
                  | gap' >= 5 = False
                  | otherwise = ultra
                tOut = if ultra' then 7 else t
                x = fromIntegral (U.unsafeIndex pulseLevels p + U.unsafeIndex tndLevels (tOut * 2048 + ns * 128 + dm)) :: Double
                a' = a + (x - a) * lpA
                b' = b + (a' - b) * lpA
                c' = c + (b' - c) * dcA
                acc1 = acc + 48000
            if acc1 < cpuHz
              then go (i + 1) a' b' c' t gap' ultra' acc1 k
              else do
                when (k < audioCap) $ do
                  let s = fromIntegral (max (-32767) (min 32767 (round (b' - c') :: Int))) :: Int16
                  MU.unsafeWrite (dAudio d) (2 * k) s
                  MU.unsafeWrite (dAudio d) (2 * k + 1) s
                go (i + 1) a' b' c' t gap' ultra' (acc1 - cpuHz) (if k < audioCap then k + 1 else k)
  go 0 a0 b0 c0 lt0 gap0 ultra0 acc0 k0

-- | 'mixLevel' as two unboxed tables, its own values ('mixLevel' walks
-- lists; measured, the difference is small): the pulse sum 0 to 30, and
-- triangle × 2048 + noise × 128 + DMC.
pulseLevels, tndLevels :: U.Vector Int
pulseLevels = U.generate 31 (\k -> mixLevel (min 15 k) (k - min 15 k) 0 0 0)
tndLevels = U.generate (16 * 2048) (\i -> mixLevel 0 0 (i `div` 2048) ((i `div` 128) `mod` 16) (i `mod` 128))
{-# NOINLINE pulseLevels #-}
{-# NOINLINE tndLevels #-}

nes_audio :: StablePtr () -> Ptr Int16 -> CInt -> IO CInt
nes_audio sp out cap = do
  d <- desk sp
  n <- min (fromIntegral cap) <$> stToIO (readSTRef (dCount d))
  mapM_ (\i -> stToIO (MU.read (dAudio d) i) >>= pokeElemOff out i) [0 .. 2 * n - 1]
  pure (fromIntegral n)

nes_pixels :: StablePtr () -> Ptr Word32 -> IO ()
nes_pixels sp out = do
  d <- desk sp
  v <- stToIO (oPicture (dOps d))
  U.imapM_ (\i c -> pokeElemOff out i (U.unsafeIndex palette32 (fromIntegral c .&. 511))) v

-- | "Nes.Palette"'s 512 colours (entry and emphasis) as 0xFFRRGGBB.
palette32 :: U.Vector Word32
palette32 = U.generate 512 (\c -> let (r, g, b) = paletteRgb (fromIntegral c) in rgb r g b)
 where
  rgb :: Word8 -> Word8 -> Word8 -> Word32
  rgb r g b = 0xFF000000 .|. (fromIntegral r `shiftL` 16) .|. (fromIntegral g `shiftL` 8) .|. fromIntegral b
{-# NOINLINE palette32 #-}

-- | The next frames' pixels not drawn (1) or drawn (0): frame skipping
-- (Q.7), when the desktop runs behind; nothing the game sees changes.
nes_skip :: StablePtr () -> CInt -> IO ()
nes_skip sp on = do
  d <- desk sp
  stToIO (oSkip (dOps d) (on /= 0))

-- * Save states (Q.8)

stateFields :: Desk -> [Field RealWorld]
stateFields d = FCheck (dId d) : oFields (dOps d) <> [FRef (dAcc d), FRef (dFilt d)]

nes_state_save :: StablePtr () -> IO CSize
nes_state_save sp = do
  d <- desk sp
  if null (oFields (dOps d)) then pure 0 else do
    b <- stToIO (snapshot (stateFields d))
    stToIO (writeSTRef (dBlob d) b)
    pure (fromIntegral (BS.length b))

nes_state_get :: StablePtr () -> Ptr Word8 -> CSize -> IO ()
nes_state_get sp out n = do
  d <- desk sp
  b <- stToIO (readSTRef (dBlob d))
  mapM_ (\i -> pokeElemOff out i (BS.index b i)) [0 .. min (fromIntegral n) (BS.length b) - 1]

nes_state_load :: StablePtr () -> Ptr Word8 -> CSize -> IO CInt
nes_state_load sp p n = do
  d <- desk sp
  if null (oFields (dOps d)) then pure (-1) else do
    b <- BS.pack <$> mapM (peekElemOff p) [0 .. fromIntegral n - 1]
    r <- stToIO (restore (stateFields d) b)
    pure (either (const (-1)) (const 0) r)
