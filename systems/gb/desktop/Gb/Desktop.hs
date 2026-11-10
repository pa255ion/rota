{-|
The Game Boy's reference model ("Gb.Spec"), or its fast version
("Gb.SpecFast": models 2 and 3, the browser's), behind a C ABI, for the
desktop emulator (side track Q, PLAN.md Q.1): the Zig front end
(platform/desktop/) calls these after @hs_init@. One machine per handle;
every call comes from the front end's one thread.

* @gb_new@ makes a machine from a ROM image (model 0 the DMG, 1 the GBC).
* @gb_frame@ runs it to its next frame with the buttons held (bit 0
  Right, 1 Left, 2 Up, 3 Down, 4 A, 5 B, 6 Select, 7 Start), sampling the
  sound at 48 kHz; @gb_audio@ hands those samples over.
* @gb_pixels@ is the last complete frame, 160 × 144 pixels of
  0xFFRRGGBB: the DMG's shades as the goldens draw them ("Gb.Screen"'s
  'greyLevel'), the GBC's 15-bit colours scaled to 8 bits.
* @gb_cram_*@ read and write cartridge RAM (the battery save's bytes).
* @gb_state_save@ takes a save state (Q.8, "Platform.Snapshot": the
  machine's every vector, the 48 kHz phase) and says its size,
  @gb_state_get@ copies it out, @gb_state_load@ puts one back (0, or -1
  when it doesn't fit this machine: another game or build).
-}
module Gb.Desktop () where

import Prelude

import Control.Monad (when)
import Control.Monad.ST (RealWorld, ST, stToIO)
import qualified Data.ByteString as BS
import Data.Bits (shiftL, shiftR, testBit, (.&.), (.|.))
import Data.STRef
import Data.Int (Int16)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import Data.Word (Word16, Word32, Word64, Word8)
import Foreign.C.Types (CInt (..), CSize (..))
import Foreign.Ptr (Ptr)
import Foreign.StablePtr
import Foreign.Storable (poke, pokeElemOff, peekElemOff)

import Gb.Screen (greyLevel)
import qualified Gb.Spec as S
import qualified Gb.Spec.Machine as SM
import qualified Gb.Spec.State as SS
import qualified Gb.SpecFast as F
import qualified Gb.SpecFast.State as FS
import Platform.Snapshot (Field (..), fnv1a, restore, snapshot)

-- | What the library needs of a machine: the reference model's
-- ("Gb.Spec") or the fast version's ("Gb.SpecFast", the browser's).
data Ops = Ops
  { oStep    :: ST RealWorld ()
  , oFrames  :: ST RealWorld Int
  , oCycles  :: ST RealWorld Int
  , oJoypad  :: Word8 -> ST RealWorld ()
  , oDouble  :: ST RealWorld Bool               -- ^ the GBC's CPU in double speed
  , oSample  :: ST RealWorld (Int, Int)         -- ^ the sound's output now
  , oColour  :: Bool                            -- ^ the GBC: 'oLastC' is the picture, else 'oLast'
  , oLast    :: MU.MVector RealWorld Word8      -- ^ the last complete frame, shades
  , oLastC   :: MU.MVector RealWorld Word16     -- ^ the last complete frame, 15-bit BGR
  , oCramOn  :: Bool
  , oCram    :: MU.MVector RealWorld Word8
  , oFields  :: [Field RealWorld]               -- ^ for save states
  }

-- | The reference model's (its fields as Q.8 took them).
refOps :: SS.Machine RealWorld -> Ops
refOps m = Ops
  { oStep = S.step m, oFrames = S.frames m, oCycles = S.cycles m, oJoypad = S.setJoypad m
  , oDouble = if SS.mCgbMode m then (`testBit` 7) <$> SS.getSt m SS.iKEY1 else pure False
  , oSample = S.audioSample m
  , oColour = SS.mModel m == SS.Cgb, oLast = SS.mLast m, oLastC = SS.mLastC m
  , oCramOn = SS.mCramOn m, oCram = SS.mCram m
  , oFields =
      [ FW8 (SS.mRegs m), FInt (SS.mSt m), FW8 (SS.mRam m), FW8 (SS.mCram m), FW8 (SS.mFb m), FW8 (SS.mLast m)
      , FInt (SS.mApu m), FInt (SS.mSpr m), FW8 (SS.mVram1 m), FW8 (SS.mWramX m), FW8 (SS.mPal m)
      , FW16 (SS.mFbC m), FW16 (SS.mLastC m) ]
  }

-- | The fast version's.
fastOps :: FS.Machine RealWorld -> Ops
fastOps m = Ops
  { oStep = F.step m, oFrames = F.frames m, oCycles = F.cycles m, oJoypad = F.setJoypad m
  , oDouble = if FS.mCgbMode m then (`testBit` 7) <$> FS.getSt m FS.iKEY1 else pure False
  , oSample = F.audioSample m
  , oColour = FS.mModel m == FS.Cgb, oLast = FS.mLast m, oLastC = FS.mLastC m
  , oCramOn = FS.mCramOn m, oCram = FS.mCram m
  , oFields =
      [ FW8 (FS.mRegs m), FInt (FS.mSt m), FW8 (FS.mRam m), FW8 (FS.mCram m), FW8 (FS.mFb m), FW8 (FS.mLast m)
      , FInt (FS.mApu m), FInt (FS.mSpr m), FW8 (FS.mVram1 m), FW8 (FS.mWramX m), FW8 (FS.mPal m)
      , FW16 (FS.mFbC m), FW16 (FS.mLastC m), FInt (FS.mObjLine m) ]
  }

data Desk = Desk
  { dOps     :: !Ops
  , dAcc     :: !(STRef RealWorld Int)          -- ^ 48 kHz phase, in units of 1/48000 M-cycle
  , dAudio   :: !(MU.MVector RealWorld Int16)   -- ^ this frame's samples, left and right
  , dCount   :: !(STRef RealWorld Int)          -- ^ stereo samples in 'dAudio'
  , dBlob    :: !(STRef RealWorld BS.ByteString) -- ^ the last save state taken
  , dId      :: !Word64                          -- ^ the game ('fnv1a' of the ROM and the model): a state is this game's
  }

-- | Samples kept per frame: a frame is about 804 at 48 kHz (more while
-- the LCD is off and a frame is 'frameCap' long).
audioCap :: Int
audioCap = 2048

-- | M-cycles a frame may take: one LCD frame (70224 dots), so a game
-- with the LCD off still returns; twice that in double speed.
frameCap :: Int
frameCap = 17556

-- | The GBC's CPU in double speed (KEY1 bit 7, CGB mode): two M-cycles
-- per dot pair, so a frame has twice the M-cycles (Gb.Spec.Machine's
-- doubleSpeed).
double :: Desk -> ST RealWorld Bool
double = oDouble . dOps

-- | M-cycles per second (the DMG's, and the GBC's at normal speed).
mHz :: Int
mHz = 1048576

foreign export ccall gb_new :: Ptr Word8 -> CSize -> CInt -> Ptr (StablePtr ()) -> IO CInt
foreign export ccall gb_free :: StablePtr () -> IO ()
foreign export ccall gb_frame :: StablePtr () -> Word8 -> IO CInt
foreign export ccall gb_audio :: StablePtr () -> Ptr Int16 -> CInt -> IO CInt
foreign export ccall gb_pixels :: StablePtr () -> Ptr Word32 -> IO ()
foreign export ccall gb_cram_size :: StablePtr () -> IO CSize
foreign export ccall gb_cram_get :: StablePtr () -> Ptr Word8 -> CSize -> IO ()
foreign export ccall gb_cram_set :: StablePtr () -> Ptr Word8 -> CSize -> IO ()
foreign export ccall gb_state_save :: StablePtr () -> IO CSize
foreign export ccall gb_state_get :: StablePtr () -> Ptr Word8 -> CSize -> IO ()
foreign export ccall gb_state_load :: StablePtr () -> Ptr Word8 -> CSize -> IO CInt

desk :: StablePtr () -> IO Desk
desk = deRefStablePtr . castPtrToStablePtr . castStablePtrToPtr

-- | 0 and the handle in @out@, or -1 (the ROM's header refused) / -2
-- (an unknown model). @model@: 0 the DMG, 1 the GBC, on the reference
-- model; 2 and 3 the same on the fast version ("Gb.SpecFast", the
-- browser's).
gb_new :: Ptr Word8 -> CSize -> CInt -> Ptr (StablePtr ()) -> IO CInt
gb_new p n model out = do
  rom <- BS.pack <$> mapM (peekElemOff p) [0 .. fromIntegral n - 1]
  case model of
    0 -> ref SS.Dmg rom
    1 -> ref SS.Cgb rom
    2 -> fast F.Dmg rom
    3 -> fast F.Cgb rom
    _ -> pure (-2)
 where
  -- (a state keeps to its model and version: the fast version's fields
  -- are its own)
  ref md rom = do
    em <- stToIO (SM.newMachineModel md rom)
    case em of
      Left _ -> pure (-1)
      Right m -> do
        d <- new (refOps m) (BS.cons (if md == SS.Cgb then 1 else 0) rom)
        -- the sound, sampled at the 48 kHz instants (as Gb.Spec.audioRun)
        stToIO (writeSTRef (SS.mHook m) (Just (SS.Hook (pure ()) (sample d) (\_ _ -> pure ()))))
        handle d
  fast md rom = do
    em <- stToIO (F.newMachineModel md rom)
    case em of
      Left _ -> pure (-1)
      Right m -> do
        d <- new (fastOps m) (BS.cons (if md == F.Cgb then 3 else 2) rom)
        stToIO (writeSTRef (FS.mHook m) (Just (FS.Hook (pure ()) (sample d) (\_ _ -> pure ()))))
        handle d
  new ops idBytes = stToIO (Desk ops <$> newSTRef 0 <*> MU.new (2 * audioCap) <*> newSTRef 0 <*> newSTRef BS.empty
                                     <*> pure (fnv1a idBytes))
  handle d = do
    sp <- newStablePtr d
    poke out (castPtrToStablePtr (castStablePtrToPtr sp))
    pure 0

gb_free :: StablePtr () -> IO ()
gb_free = freeStablePtr

-- | Runs to the next frame (or 'frameCap' M-cycles) with the buttons
-- @keys@; the number of stereo samples it made.
gb_frame :: StablePtr () -> Word8 -> IO CInt
gb_frame sp keys = do
  d <- desk sp
  let o = dOps d
  stToIO (writeSTRef (dCount d) 0)
  stToIO (oJoypad o keys)
  f0 <- stToIO (oFrames o)
  c0 <- stToIO (oCycles o)
  cap <- (\d2 -> if d2 then 2 * frameCap else frameCap) <$> stToIO (double d)
  let go = do
        stToIO (oStep o)
        f <- stToIO (oFrames o)
        c <- stToIO (oCycles o)
        when (f == f0 && c - c0 < cap) go
  go
  fromIntegral <$> stToIO (readSTRef (dCount d))

-- | Copies up to @cap@ stereo samples of the last frame (left, right) to
-- @out@; how many.
gb_audio :: StablePtr () -> Ptr Int16 -> CInt -> IO CInt
gb_audio sp out cap = do
  d <- desk sp
  n <- min (fromIntegral cap) <$> stToIO (readSTRef (dCount d))
  mapM_ (\i -> stToIO (MU.read (dAudio d) i) >>= pokeElemOff out i) [0 .. 2 * n - 1]
  pure (fromIntegral n)

gb_pixels :: StablePtr () -> Ptr Word32 -> IO ()
gb_pixels sp out = do
  o <- dOps <$> desk sp
  if oColour o
    then do
      v <- stToIO (U.freeze (oLastC o)) :: IO (U.Vector Word16)
      U.imapM_ (\i c -> pokeElemOff out i (bgr15 c)) v
    else do
      v <- stToIO (U.freeze (oLast o))
      U.imapM_ (\i s -> let g = fromIntegral (greyLevel s) in pokeElemOff out i (rgb g g g)) v
 where
  rgb :: Word32 -> Word32 -> Word32 -> Word32
  rgb r g b = 0xFF000000 .|. (r `shiftL` 16) .|. (g `shiftL` 8) .|. b
  five x = let v = fromIntegral (x .&. 0x1F) :: Word32 in (v `shiftL` 3) .|. (v `shiftR` 2)
  bgr15 c = rgb (five c) (five (c `shiftR` 5)) (five (c `shiftR` 10))

-- | Cartridge RAM's size in bytes (0 without RAM).
gb_cram_size :: StablePtr () -> IO CSize
gb_cram_size sp = do
  o <- dOps <$> desk sp
  pure (if oCramOn o then fromIntegral (MU.length (oCram o)) else 0)

gb_cram_get :: StablePtr () -> Ptr Word8 -> CSize -> IO ()
gb_cram_get sp out n = do
  o <- dOps <$> desk sp
  let k = if oCramOn o then min (fromIntegral n) (MU.length (oCram o)) else 0
  mapM_ (\i -> stToIO (MU.read (oCram o) i) >>= pokeElemOff out i) [0 .. k - 1]

gb_cram_set :: StablePtr () -> Ptr Word8 -> CSize -> IO ()
gb_cram_set sp p n = do
  o <- dOps <$> desk sp
  let k = if oCramOn o then min (fromIntegral n) (MU.length (oCram o)) else 0
  mapM_ (\i -> peekElemOff p i >>= stToIO . MU.write (oCram o) i) [0 .. k - 1]

-- | The hook's post-PPU part, once per M-cycle: a sample at each 48 kHz
-- instant (an M-cycle is half as long in double speed).
sample :: Desk -> ST RealWorld ()
sample d = do
  d2 <- double d
  a <- (+ (if d2 then 24000 else 48000)) <$> readSTRef (dAcc d)
  if a >= mHz
    then do
      writeSTRef (dAcc d) (a - mHz)
      (l, r) <- oSample (dOps d)
      n <- readSTRef (dCount d)
      when (n < audioCap) $ do
        MU.write (dAudio d) (2 * n) (pcm l)
        MU.write (dAudio d) (2 * n + 1) (pcm r)
        writeSTRef (dCount d) (n + 1)
    else writeSTRef (dAcc d) a
 where
  -- Gb.Spec.Apu.mix is ±480: × 64 to 16 bits
  pcm x = fromIntegral (max (-511) (min 511 x) * 64)

-- * Save states (Q.8)

-- | Everything that changes while the machine runs (the trace, the
-- serial log and the test hook aside: no game sees them).
stateFields :: Desk -> [Field RealWorld]
stateFields d = [FCheck (dId d)] <> oFields (dOps d) <> [FRef (dAcc d)]

gb_state_save :: StablePtr () -> IO CSize
gb_state_save sp = do
  d <- desk sp
  b <- stToIO (snapshot (stateFields d))
  stToIO (writeSTRef (dBlob d) b)
  pure (fromIntegral (BS.length b))

gb_state_get :: StablePtr () -> Ptr Word8 -> CSize -> IO ()
gb_state_get sp out n = do
  d <- desk sp
  b <- stToIO (readSTRef (dBlob d))
  mapM_ (\i -> pokeElemOff out i (BS.index b i)) [0 .. min (fromIntegral n) (BS.length b) - 1]

gb_state_load :: StablePtr () -> Ptr Word8 -> CSize -> IO CInt
gb_state_load sp p n = do
  d <- desk sp
  b <- BS.pack <$> mapM (peekElemOff p) [0 .. fromIntegral n - 1]
  r <- stToIO (restore (stateFields d) b)
  pure (either (const (-1)) (const 0) r)
