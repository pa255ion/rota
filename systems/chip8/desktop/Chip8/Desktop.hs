{-|
CHIP-8's reference model ("Chip8.Spec") behind a C ABI, for the desktop
emulator (side track Q, PLAN.md Q.5), as "Gb.Desktop" is for the Game
Boy. The spec is pure; the machine lives in an 'IORef'.

* @chip8_new@ loads a program at 0x200 with the quirk bits (the board's
  SW0 to SW4, 'quirksFromConfig': 0 is the COSMAC VIP's) and the
  instructions per 60 Hz frame (the board's slow speed is about 700 Hz,
  12 a frame; its fast one about 10 kHz, 167).
* @chip8_frame@ runs one frame with the keys held (bit k = key k), then
  the 60 Hz tick ('runFrames' for one frame); the beeper as a 440 Hz
  square wave at 48 kHz while the sound timer is non-zero, as the board's
  speaker does. @chip8_audio@ hands the samples over.
* @chip8_pixels@ is the display, 64 × 32 pixels of 0xFFRRGGBB, lit white
  (the spec goldens' colours). @chip8_halted@ is 1 after an illegal
  opcode.
* @chip8_state_save@, @_get@, @_load@: save states (Q.8,
  "Platform.Snapshot", as "Gb.Desktop"'s): the machine through 'St' (the
  spec's value has no 'Read'), the beeper's phase.
-}
module Chip8.Desktop () where

import Prelude

import Control.Monad (forM_)
import Data.Int (Int16)
import Data.IORef
import qualified Data.Vector.Unboxed as U
import Data.Word (Word16, Word32, Word8)
import Foreign.C.Types (CInt (..), CSize (..))
import Foreign.Ptr (Ptr)
import Foreign.StablePtr
import Foreign.Storable (peekElemOff, poke, pokeElemOff)
import Control.Monad.ST (stToIO)
import qualified Data.ByteString as BS
import Data.STRef
import Data.Word (Word64)

import Platform.Snapshot (Field (..), fnv1a, restore, snapshot)

import Chip8.Spec

data Desk = Desk
  { dQuirks :: !Quirks
  , dIpf    :: !Int
  , dMach   :: !(IORef Machine)
  , dPhase  :: !(IORef Int)   -- ^ the beeper's phase, in units of 1/48000 of its period
  , dSound  :: !(IORef Bool)  -- ^ the last frame beeped
  , dBlob   :: !(IORef BS.ByteString) -- ^ the last save state taken
  , dId     :: !Word64        -- ^ the program, its quirks and speed ('fnv1a')
  }

-- | 48 kHz over 60 Hz.
samplesPerFrame :: Int
samplesPerFrame = 800

foreign export ccall chip8_new :: Ptr Word8 -> CSize -> Word8 -> CInt -> Ptr (StablePtr ()) -> IO CInt
foreign export ccall chip8_free :: StablePtr () -> IO ()
foreign export ccall chip8_frame :: StablePtr () -> Word16 -> IO CInt
foreign export ccall chip8_audio :: StablePtr () -> Ptr Int16 -> CInt -> IO CInt
foreign export ccall chip8_pixels :: StablePtr () -> Ptr Word32 -> IO ()
foreign export ccall chip8_halted :: StablePtr () -> IO CInt
foreign export ccall chip8_state_save :: StablePtr () -> IO CSize
foreign export ccall chip8_state_get :: StablePtr () -> Ptr Word8 -> CSize -> IO ()
foreign export ccall chip8_state_load :: StablePtr () -> Ptr Word8 -> CSize -> IO CInt

desk :: StablePtr () -> IO Desk
desk = deRefStablePtr . castPtrToStablePtr . castStablePtrToPtr

-- | 0 and the handle in @out@, or -1 (a program over 3584 bytes, or none).
chip8_new :: Ptr Word8 -> CSize -> Word8 -> CInt -> Ptr (StablePtr ()) -> IO CInt
chip8_new p n cfg ipf out
  | n == 0 || n > 0xE00 = pure (-1)
  | otherwise = do
      bytes <- mapM (peekElemOff p) [0 .. fromIntegral n - 1]
      d <- Desk (quirksFromConfig cfg) (max 1 (fromIntegral ipf))
             <$> newIORef (loadProgram bytes newMachine) <*> newIORef 0 <*> newIORef False
             <*> newIORef BS.empty <*> pure (fnv1a (BS.pack (cfg : fromIntegral ipf : bytes)))
      sp <- newStablePtr d
      poke out (castPtrToStablePtr (castStablePtrToPtr sp))
      pure 0

chip8_free :: StablePtr () -> IO ()
chip8_free = freeStablePtr

-- | One 60 Hz frame with @keys@ held; the stereo samples (always 800).
chip8_frame :: StablePtr () -> Word16 -> IO CInt
chip8_frame sp keys = do
  d <- desk sp
  m <- readIORef (dMach d)
  let m' = runFrames (dQuirks d) (dIpf d) (const keys) 1 m
  writeIORef (dMach d) $! m'
  writeIORef (dSound d) (soundOn m)
  pure (fromIntegral samplesPerFrame)

-- | The frame's beeper (on while the sound timer was non-zero at its
-- start), up to @cap@ stereo samples.
chip8_audio :: StablePtr () -> Ptr Int16 -> CInt -> IO CInt
chip8_audio sp out cap = do
  d <- desk sp
  on <- readIORef (dSound d)
  ph0 <- readIORef (dPhase d)
  let n = min (fromIntegral cap) samplesPerFrame
  forM_ [0 .. n - 1] $ \i -> do
    let ph = (ph0 + 440 * i) `mod` 48000
        s = if not on then 0 else if ph < 24000 then 6000 else -6000 :: Int16
    pokeElemOff out (2 * i) s
    pokeElemOff out (2 * i + 1) s
  writeIORef (dPhase d) ((ph0 + 440 * n) `mod` 48000)
  pure (fromIntegral n)

chip8_pixels :: StablePtr () -> Ptr Word32 -> IO ()
chip8_pixels sp out = do
  m <- readIORef . dMach =<< desk sp
  U.imapM_ (\i lit -> pokeElemOff out i (if lit then 0xFFFFFFFF else 0xFF000000)) (mDisplay m)

chip8_halted :: StablePtr () -> IO CInt
chip8_halted sp = do
  m <- readIORef . dMach =<< desk sp
  pure (maybe 0 (const 1) (mHalt m))

-- * Save states (Q.8)

-- | The spec's 'Machine' as plain values ('Read' for "Platform.Snapshot"),
-- with the beeper's phase and last state.
data St = St [Word8] [Word8] Word16 Word16 [Word16] Word8 Word8 [Bool] Word16 Int (Maybe Word8) (Maybe (Int, Word16)) Int Bool
  deriving (Show, Read)

toSt :: Machine -> Int -> Bool -> St
toSt m = St (U.toList (mRam m)) (U.toList (mV m)) (mI m) (mPc m) (mStack m) (mDelay m) (mSound m) (U.toList (mDisplay m)) (mRng m)
  (case mVblank m of VblankIdle -> 0; VblankWaiting -> 1; VblankReleased -> 2)
  (case mKeyWait m of KeyWaitNone -> Nothing; KeyWaitHeld k -> Just k)
  (case mHalt m of Nothing -> Nothing; Just (HaltIllegal o) -> Just (0, o); Just HaltStackUnderflow -> Just (1, 0); Just HaltStackOverflow -> Just (2, 0))

fromSt :: St -> (Machine, Int, Bool)
fromSt (St ram v i pc stack dl snd' disp rng vb kw h ph on) =
  ( Machine (U.fromList ram) (U.fromList v) i pc stack dl snd' (U.fromList disp) rng
      (case vb of 1 -> VblankWaiting; 2 -> VblankReleased; _ -> VblankIdle)
      (maybe KeyWaitNone KeyWaitHeld kw)
      (case h of Just (0, o) -> Just (HaltIllegal o); Just (1, _) -> Just HaltStackUnderflow; Just (2, _) -> Just HaltStackOverflow; _ -> Nothing)
  , ph, on )

chip8_state_save :: StablePtr () -> IO CSize
chip8_state_save sp = do
  d <- desk sp
  st <- toSt <$> readIORef (dMach d) <*> readIORef (dPhase d) <*> readIORef (dSound d)
  b <- stToIO (newSTRef st >>= \r -> snapshot [FCheck (dId d), FRef r])
  writeIORef (dBlob d) b
  pure (fromIntegral (BS.length b))

chip8_state_get :: StablePtr () -> Ptr Word8 -> CSize -> IO ()
chip8_state_get sp out n = do
  d <- desk sp
  b <- readIORef (dBlob d)
  forM_ [0 .. min (fromIntegral n) (BS.length b) - 1] $ \i -> pokeElemOff out i (BS.index b i)

chip8_state_load :: StablePtr () -> Ptr Word8 -> CSize -> IO CInt
chip8_state_load sp p n = do
  d <- desk sp
  b <- BS.pack <$> mapM (peekElemOff p) [0 .. fromIntegral n - 1]
  cur <- toSt <$> readIORef (dMach d) <*> readIORef (dPhase d) <*> readIORef (dSound d)
  (res, st) <- stToIO (newSTRef cur >>= \r -> (,) <$> restore [FCheck (dId d), FRef r] b <*> readSTRef r)
  case res of
    Left _ -> pure (-1)
    Right () -> do
      let (m, ph, on) = fromSt st
      writeIORef (dMach d) $! m
      writeIORef (dPhase d) ph
      writeIORef (dSound d) on
      pure 0
