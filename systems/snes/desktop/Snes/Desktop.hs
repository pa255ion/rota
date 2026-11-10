{-# LANGUAGE BangPatterns #-}
{-|
The Super Nintendo's model behind a C ABI, for the desktop emulator (side
track Q, PLAN.md Q.5), as "Gb.Desktop" is for the Game Boy. It runs the
fast version ("Snes.SpecFast.Machine"), which the snes-specfast suite
checks against the reference model instruction by instruction.

* @snes_new@ makes a machine from the sound module's boot ROM (@make
  snes-asm@'s ipl.bin, or the user's dump) and a cartridge; @threaded@ the
  number of render threads drawing the pixels on other cores
  ("Snes.SpecFast.RenderLog", side track Q's Q.6; the library is linked
  with GHC's threaded runtime and asks for a capability each), 0 inline
  (the desktop's --no-render-thread).
* @snes_frame@ runs one frame with the key word held (the board's key
  maps, snes_keys.zig: pad 1 in bits 0 to 11 in the pad's own order, 11
  B, 10 Y, 9 Select, 8 Start, 7 up, 6 down, 5 left, 4 right, 3 A, 2 X, 1
  L, 0 R; pad 2 the same in bits 16 to 27, as 'setPads' takes them); the
  DSP's 32 kHz output resampled to 48 kHz (linear); @snes_audio@ hands it
  over.
* @snes_pixels@ is the picture, 512 × 478: the 512 × 239 frame
  ("Snes.Spec.Ppu"), each line twice, lines past the visible ones (224
  without overscan) black; 0xFFRRGGBB as snes-run's PNGs draw it.
  @snes_hash@ is the spec tests' frame hash.
-}
module Snes.Desktop () where

import Prelude

import Control.Concurrent (getNumCapabilities, setNumCapabilities)
import Control.Monad (forM_, when)
import Control.Monad.ST (RealWorld, ST, stToIO)
import Data.STRef
import qualified Data.ByteString as B
import Data.Bits (shiftL, shiftR, xor, (.&.), (.|.))
import Data.Int (Int16, Int32)
import Data.IORef
import qualified Data.Vector.Unboxed as U
import Data.Word (Word16, Word32, Word64, Word8)
import qualified Data.Vector.Unboxed.Mutable as MU
import Foreign.C.Types (CInt (..), CSize (..))
import Foreign.Ptr (Ptr)
import Foreign.StablePtr
import Foreign.Storable (peekElemOff, poke, pokeElemOff)

import Snes.Cart (cartBattery, cartSramSize, parseCart)
import Snes.SpecFast.Machine
import Snes.SpecFast.RenderLog (Replayer, newReplayer, replayerFeed, replayerFrame, replayerLines)
import qualified Snes.Spec.Machine as S
import Platform.Snapshot (Field (..), fnv1a, restore, snapshot)

-- | What the library needs of a machine: the fast version's or the
-- reference model's ('fastOps', 'refOps').
data Ops = Ops
  { oFrame  :: Int -> Int -> ST RealWorld ()      -- ^ the pads, then one frame
  , oAudio  :: ST RealWorld (U.Vector Int)        -- ^ the samples since the last call
  , oColors :: Bool -> ST RealWorld (U.Vector Word16)  -- ^ the frame (one late: True)
  , oLines  :: ST RealWorld Int
  , oSkip   :: Bool -> ST RealWorld ()            -- ^ frame skipping (the fast version only)
  , oLag    :: Bool -> ST RealWorld ()            -- ^ the frame one late (the fast version only)
  , oFields :: [Field RealWorld]                  -- ^ for save states (the fast version only; none: no states)
  , oSync   :: ST RealWorld ()                    -- ^ before a state is taken or loaded: the render threads idle
  , oLoaded :: ST RealWorld ()                    -- ^ after one is loaded: the threads' copies made again
  , oTakeLog :: ST RealWorld (U.Vector Int)        -- ^ an exported render log's events (made with -1 threads; else none)
  , oSram   :: Maybe (MU.MVector RealWorld Word8)  -- ^ the SRAM where the cartridge has a battery ('snes_new' sets it; the reference model's: none)
  }

fastOps :: Machine RealWorld -> Ops
fastOps m = Ops
  { oFrame = \p1 p2 -> setPads m p1 p2 >> runFrames m 1
  , oAudio = audioTake m
  , oColors = \lag -> (if lag then lastFrameColors else frameColors) m
  , oLines = visibleLines m
  , oSkip = setSkipDraw m
  , oLag = setFrameLag m
  , oFields = machineFields m
  , oSync = machineStateSync m
  , oLoaded = machineReloaded m
  , oSram = Just (machineSram m)
  , oTakeLog = machineTakeLog m
  }

-- | The reference model: its samples come from a buffer that only grows,
-- so the new ones are those past @taken@.
refOps :: S.Machine RealWorld -> STRef RealWorld Int -> Ops
refOps m taken = Ops
  { oFrame = \p1 p2 -> S.setPads m p1 p2 >> S.runFrames m 1
  , oAudio = do
      all' <- S.audioSamples m
      k <- readSTRef taken
      writeSTRef taken (U.length all')
      pure (U.drop k all')
  , oColors = const (S.frameColors m)
  , oLines = S.visibleLines m
  , oSkip = const (pure ())
  , oLag = const (pure ())
  , oFields = []
  , oSync = pure ()
  , oLoaded = pure ()
  , oSram = Nothing
  , oTakeLog = pure U.empty
  }

data Desk = Desk
  { dOps     :: !Ops
  , dPhase   :: !(IORef Double)            -- ^ the resampler's position in the 32 kHz stream
  , dPrev    :: !(IORef (Int, Int))         -- ^ the last 32 kHz sample of the previous frame
  , dOut     :: !(IORef (U.Vector Int16))   -- ^ this frame's 48 kHz samples, L, R
  , dLag     :: !(IORef Bool)                -- ^ the picture one frame late ('snes_lag')
  , dBlob    :: !(IORef B.ByteString)        -- ^ the last save state taken
  , dId      :: !Word64                      -- ^ the game ('fnv1a' of the file and the sound ROM)
  , dLog     :: !(IORef (U.Vector Int))      -- ^ the render log taken ('snes_log_size'), until 'snes_log_get'
  }

foreign export ccall snes_new :: Ptr Word8 -> CSize -> Ptr Word8 -> CSize -> CInt -> CInt -> Ptr (StablePtr ()) -> IO CInt
foreign export ccall snes_free :: StablePtr () -> IO ()
foreign export ccall snes_frame :: StablePtr () -> Word32 -> IO CInt
foreign export ccall snes_audio :: StablePtr () -> Ptr Int16 -> CInt -> IO CInt
foreign export ccall snes_pixels :: StablePtr () -> Ptr Word32 -> IO ()
foreign export ccall snes_pixels_rgba :: StablePtr () -> Ptr Word32 -> IO ()
foreign export ccall snes_cram_size :: StablePtr () -> IO CSize
foreign export ccall snes_log_size :: StablePtr () -> IO CInt
foreign export ccall snes_log_get :: StablePtr () -> Ptr Int32 -> IO ()
foreign export ccall snes_render_new :: Ptr (StablePtr ()) -> IO CInt
foreign export ccall snes_render_free :: StablePtr () -> IO ()
foreign export ccall snes_render_feed :: StablePtr () -> Ptr Int32 -> CInt -> IO CInt
foreign export ccall snes_render_pixels_rgba :: StablePtr () -> Ptr Word32 -> IO ()
foreign export ccall snes_render_hash :: StablePtr () -> IO Word64
foreign export ccall snes_cram_get :: StablePtr () -> Ptr Word8 -> CSize -> IO ()
foreign export ccall snes_cram_set :: StablePtr () -> Ptr Word8 -> CSize -> IO ()
foreign export ccall snes_hash :: StablePtr () -> IO Word64
foreign export ccall snes_skip :: StablePtr () -> CInt -> IO ()
foreign export ccall snes_lag :: StablePtr () -> CInt -> IO ()
foreign export ccall snes_state_save :: StablePtr () -> IO CSize
foreign export ccall snes_state_get :: StablePtr () -> Ptr Word8 -> CSize -> IO ()
foreign export ccall snes_state_load :: StablePtr () -> Ptr Word8 -> CSize -> IO CInt

desk :: StablePtr () -> IO Desk
desk = deRefStablePtr . castPtrToStablePtr . castStablePtrToPtr

bytesAt :: Ptr Word8 -> CSize -> IO B.ByteString
bytesAt p n = B.pack <$> mapM (peekElemOff p) [0 .. fromIntegral n - 1]

-- | 0 and the handle in @out@, or -1 (no cartridge header); @reference@ 1
-- runs the reference model "Snes.Spec.Machine" (the desktop's
-- --reference), its drawing inline.
snes_new :: Ptr Word8 -> CSize -> Ptr Word8 -> CSize -> CInt -> CInt -> Ptr (StablePtr ()) -> IO CInt
snes_new ip ipn rp rn threaded reference out = do
  ipl <- bytesAt ip ipn
  file <- bytesAt rp rn
  case parseCart file of
    Nothing -> pure (-1)
    Just cart -> do
      -- capabilities: the machine's and one per render thread
      -- (-1: the log exported, drawn by the caller's replayer: 'snes_log_size')
      let n = if reference /= 0 then 0 else if threaded < 0 then -1 else fromIntegral threaded :: Int
      when (n > 0) $ getNumCapabilities >>= \c -> when (c < n + 1) (setNumCapabilities (n + 1))
      ops <- stToIO $ if reference /= 0
        then do
          m <- S.newMachine ipl cart file
          S.reset m
          refOps m <$> newSTRef 0
        else do
          m <- newMachineWith n ipl cart file
          reset m
          pure (fastOps m)
      let ops' = if cartBattery cart && cartSramSize cart > 0 then ops else ops { oSram = Nothing }
      d <- Desk ops' <$> newIORef 0 <*> newIORef (0, 0) <*> newIORef U.empty <*> newIORef False
             <*> newIORef B.empty <*> pure (fnv1a (ipl <> file)) <*> newIORef U.empty
      sp <- newStablePtr d
      poke out (castPtrToStablePtr (castStablePtrToPtr sp))
      pure 0

snes_free :: StablePtr () -> IO ()
snes_free = freeStablePtr

-- | One frame with the key word @w@; its stereo samples at 48 kHz.
snes_frame :: StablePtr () -> Word32 -> IO CInt
snes_frame sp w = do
  d <- desk sp
  let o = dOps d
  stToIO (oFrame o (fromIntegral (w .&. 0xFFF)) (fromIntegral ((w `shiftR` 16) .&. 0xFFF)))
  raw <- stToIO (oAudio o)
  -- linear resampling, 32 kHz to 48 kHz: output k at input position
  -- phase + k * 2/3, between the previous frame's last sample (index -1)
  -- and this frame's
  prev <- readIORef (dPrev d)
  ph <- readIORef (dPhase d)
  let n = U.length raw `div` 2
      at i | i < 0 = prev
           | otherwise = (raw U.! (2 * i), raw U.! (2 * i + 1))
      step = 32000 / 48000 :: Double
      outs = takeWhile (\pos -> pos <= fromIntegral n - 1) [ ph + step * fromIntegral k | k <- [0 :: Int ..] ]
      sample pos =
        let i = floor pos :: Int
            t = pos - fromIntegral i
            (l0, r0) = at (i - 1)
            (l1, r1) = at i
            mix a b = fromIntegral a * (1 - t) + fromIntegral b * t :: Double
            clip x = fromIntegral (max (-32768) (min 32767 (round x :: Int))) :: Int16
        in [clip (mix l0 l1), clip (mix r0 r1)]
      out = U.fromList (concatMap sample outs)
      next = if null outs then ph - fromIntegral n else last outs + step - fromIntegral n
  writeIORef (dOut d) out
  writeIORef (dPhase d) next
  writeIORef (dPrev d) (if n > 0 then at (n - 1) else prev)
  pure (fromIntegral (U.length out `div` 2))

snes_audio :: StablePtr () -> Ptr Int16 -> CInt -> IO CInt
snes_audio sp out cap = do
  v <- readIORef . dOut =<< desk sp
  let n = min (fromIntegral cap) (U.length v `div` 2)
  forM_ [0 .. 2 * n - 1] $ \i -> pokeElemOff out i (v U.! i)
  pure (fromIntegral n)

snes_pixels :: StablePtr () -> Ptr Word32 -> IO ()
snes_pixels sp out = do
  d <- desk sp
  lag <- readIORef (dLag d)
  fr <- stToIO (oColors (dOps d) lag)
  lns <- stToIO (oLines (dOps d))
  -- (a 32768-entry colour table; each frame line converted once and
  -- written twice, the lines past the visible ones black: no division
  -- per pixel, Q.7)
  let row !l
        | l >= 239 = pure ()
        | otherwise = do
            let base = l * 512
                o1 = 2 * l * 512
                o2 = o1 + 512
                px !x
                  | x >= 512 = pure ()
                  | otherwise = do
                      let c = U.unsafeIndex rgbTable (if l < lns then fromIntegral (U.unsafeIndex fr (base + x)) .&. 0x7FFF else 0)
                      pokeElemOff out (o1 + x) c
                      pokeElemOff out (o2 + x) c
                      px (x + 1)
            px 0
            row (l + 1)
  row 0

-- | The render log exported (a machine made with -1 threads; side track
-- W: the page's second worker draws it): its events since the last call,
-- taken, and their number; 'snes_log_get' copies them (as Int32s).
snes_log_size :: StablePtr () -> IO CInt
snes_log_size sp = do
  d <- desk sp
  v <- stToIO (oTakeLog (dOps d))
  writeIORef (dLog d) v
  pure (fromIntegral (U.length v))

snes_log_get :: StablePtr () -> Ptr Int32 -> IO ()
snes_log_get sp out = do
  d <- desk sp
  v <- readIORef (dLog d)
  U.imapM_ (\i x -> pokeElemOff out i (fromIntegral x)) v
  writeIORef (dLog d) U.empty

-- | A replayer: the PPU drawn from an exported log (in the page's second
-- worker, its own instance of this library).
snes_render_new :: Ptr (StablePtr ()) -> IO CInt
snes_render_new out = do
  r <- stToIO newReplayer
  sp <- newStablePtr r
  poke out (castPtrToStablePtr (castStablePtrToPtr sp))
  pure 0

snes_render_free :: StablePtr () -> IO ()
snes_render_free = freeStablePtr

replayer :: StablePtr () -> IO (Replayer RealWorld)
replayer = deRefStablePtr . castPtrToStablePtr . castStablePtrToPtr

-- | @n@ of a log's events (Int32s at @p@) replayed; the frames that ended.
snes_render_feed :: StablePtr () -> Ptr Int32 -> CInt -> IO CInt
snes_render_feed sp p n = do
  r <- replayer sp
  v <- U.generateM (fromIntegral n) (\i -> fromIntegral <$> peekElemOff p i)
  fromIntegral <$> stToIO (replayerFeed r v)

-- | The last frame the replayer finished, as 'snes_pixels_rgba'.
snes_render_pixels_rgba :: StablePtr () -> Ptr Word32 -> IO ()
snes_render_pixels_rgba sp out = do
  r <- replayer sp
  fr <- stToIO (replayerFrame r)
  lns <- stToIO (replayerLines r)
  rgbaInto out fr lns

-- | As 'snes_hash', the replayer's last frame.
snes_render_hash :: StablePtr () -> IO Word64
snes_render_hash sp = do
  r <- replayer sp
  fr <- stToIO (replayerFrame r)
  lns <- stToIO (replayerLines r)
  pure (U.foldl' (\acc c -> (acc `xor` fromIntegral c) * 0x100000001b3) 0xcbf29ce484222325 (U.take (512 * lns) fr))

-- | The battery-backed RAM (side track W: the page's battery saves, as
-- gb_cram_*): the SRAM when the header's cartridge type has a battery,
-- else none (size 0).
snes_cram_size :: StablePtr () -> IO CSize
snes_cram_size sp = maybe 0 (fromIntegral . MU.length) . oSram . dOps <$> desk sp

snes_cram_get :: StablePtr () -> Ptr Word8 -> CSize -> IO ()
snes_cram_get sp out n = do
  o <- dOps <$> desk sp
  forM_ (oSram o) $ \ram ->
    mapM_ (\i -> stToIO (MU.read ram i) >>= pokeElemOff out i) [0 .. min (fromIntegral n) (MU.length ram) - 1]

snes_cram_set :: StablePtr () -> Ptr Word8 -> CSize -> IO ()
snes_cram_set sp p n = do
  o <- dOps <$> desk sp
  forM_ (oSram o) $ \ram ->
    mapM_ (\i -> peekElemOff p i >>= stToIO . MU.write ram i) [0 .. min (fromIntegral n) (MU.length ram) - 1]

-- | The picture for a canvas (side track W, the browser's): 512 × 239,
-- each line once (the page stretches it), as RGBA bytes (0xFFBBGGRR
-- words): the page copies it as it is, no conversion per pixel.
snes_pixels_rgba :: StablePtr () -> Ptr Word32 -> IO ()
snes_pixels_rgba sp out = do
  d <- desk sp
  lag <- readIORef (dLag d)
  fr <- stToIO (oColors (dOps d) lag)
  lns <- stToIO (oLines (dOps d))
  rgbaInto out fr lns

-- | A frame's colours as RGBA words (0xFFBBGGRR), 512 × 239, the lines past
-- @lns@ black.
rgbaInto :: Ptr Word32 -> U.Vector Word16 -> Int -> IO ()
rgbaInto out fr lns = do
  let n = 239 * 512
      visible = lns * 512
      px !i
        | i >= n = pure ()
        | otherwise = do
            pokeElemOff out i (U.unsafeIndex rgbaTable (if i < visible then fromIntegral (U.unsafeIndex fr i) .&. 0x7FFF else 0))
            px (i + 1)
  px 0

-- | 'rgbTable' as RGBA bytes in a little-endian word: 0xFFBBGGRR.
rgbaTable :: U.Vector Word32
rgbaTable = U.map (\c -> (c .&. 0xFF00FF00) .|. ((c `shiftR` 16) .&. 0xFF) .|. ((c .&. 0xFF) `shiftL` 16)) rgbTable
{-# NOINLINE rgbaTable #-}

-- | Each 15-bit colour as 0xFFRRGGBB, as snes-run's PNGs draw it.
rgbTable :: U.Vector Word32
rgbTable = U.generate 32768 rgb
 where
  rgb c = let ch k = fromIntegral (((c `shiftR` k) .&. 31) * 255 `div` 31) :: Word32
          in 0xFF000000 .|. (ch 0 `shiftL` 16) .|. (ch 5 `shiftL` 8) .|. ch 10
{-# NOINLINE rgbTable #-}

-- | The spec tests' frame hash (FNV-1a over the visible lines' colours).
snes_hash :: StablePtr () -> IO Word64
snes_hash sp = do
  d <- desk sp
  fr <- stToIO (oColors (dOps d) False)
  lns <- stToIO (oLines (dOps d))
  pure (U.foldl' (\acc c -> (acc `xor` fromIntegral c) * 0x100000001b3) 0xcbf29ce484222325 (U.take (512 * lns) fr))

-- | The next frames' pixels not drawn (1) or drawn (0): frame skipping
-- (Q.6), when the desktop runs behind; nothing the game sees changes.
snes_skip :: StablePtr () -> CInt -> IO ()
snes_skip sp on = do
  d <- desk sp
  stToIO (oSkip (dOps d) (on /= 0))

-- | The picture one frame late (1): the machine runs on into the next frame
-- while the render threads finish this one (Q.6; the desktop's default,
-- --no-frame-lag turns it off); 0: each frame as it ends.
snes_lag :: StablePtr () -> CInt -> IO ()
snes_lag sp on = do
  d <- desk sp
  writeIORef (dLag d) (on /= 0)
  stToIO (oLag (dOps d) (on /= 0))

-- * Save states (Q.8, "Platform.Snapshot"; as "Gb.Desktop"'s, the fast
-- version's only)

-- | The machine's fields and the resampler's (its two references here
-- are IORefs: through STRefs made for the occasion).
withFields :: Desk -> ([Field RealWorld] -> ST RealWorld a) -> IO a
withFields d act = do
  ph <- readIORef (dPhase d)
  pv <- readIORef (dPrev d)
  (r, ph', pv') <- stToIO $ do
    rp <- newSTRef ph
    rv <- newSTRef pv
    a <- act (FCheck (dId d) : oFields (dOps d) <> [FRef rp, FRef rv])
    (,,) a <$> readSTRef rp <*> readSTRef rv
  writeIORef (dPhase d) ph'
  writeIORef (dPrev d) pv'
  pure r

snes_state_save :: StablePtr () -> IO CSize
snes_state_save sp = do
  d <- desk sp
  if null (oFields (dOps d)) then pure 0 else do
    stToIO (oSync (dOps d))
    b <- withFields d snapshot
    writeIORef (dBlob d) b
    pure (fromIntegral (B.length b))

snes_state_get :: StablePtr () -> Ptr Word8 -> CSize -> IO ()
snes_state_get sp out n = do
  d <- desk sp
  b <- readIORef (dBlob d)
  forM_ [0 .. min (fromIntegral n) (B.length b) - 1] $ \i -> pokeElemOff out i (B.index b i)

snes_state_load :: StablePtr () -> Ptr Word8 -> CSize -> IO CInt
snes_state_load sp p n = do
  d <- desk sp
  if null (oFields (dOps d)) then pure (-1) else do
    b <- bytesAt p n
    stToIO (oSync (dOps d))
    r <- withFields d (`restore` b)
    case r of
      Left _ -> pure (-1)
      Right () -> do
        stToIO (oLoaded (dOps d))
        writeIORef (dOut d) U.empty
        pure 0
