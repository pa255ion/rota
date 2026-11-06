{-# LANGUAGE TupleSections #-}
{-|
The Game Gear and Master System (with the Master System's FM unit
"Sms.Spec.FmUnit") behind a C ABI, for the desktop emulator (side track
Q, PLAN.md Q.5), as "Gb.Desktop" is for the Game Boy; one library for
both, as @sms-run@ is. It runs the fast version ("Gg.SpecFast", Q.7),
which the sms-specfast suite checks against the reference model
("Gg.Spec") instruction by instruction; the desktop's --reference runs
the latter.

* @sms_new@ makes a machine: mode 0 the Game Gear, 1 the Master System
  II (with the FM unit when @fm@ is 1, and the BIOS image if one is
  given), 2 a Game Gear in its Master System mode; the mapper from the
  ROM ('detectMapper').
* @sms_frame@ runs one frame with the key word held (the board's key
  maps' bits, gg_keys.zig and sms_keys.zig: bit 0 Right, 1 Left, 2 Up, 3
  Down, 4 button 2, 5 button 1, 7 Start or Pause; bits 8 to 13 the
  Master System's pad 2 the same way); @sms_audio@ hands its 48 kHz
  stereo samples over.
* @sms_pixels@ is the frame, @sms_width@ × @sms_height@ (160 × 144 on the
  Game Gear, 256 × 192 in Master System mode), 0xFFRRGGBB as the goldens
  draw it ("Gg.Screen"'s 'frameRgb'); @sms_hash@ its 'frameHash' (the
  goldens' frame hashes).
* @sms_cram_*@: cartridge RAM, 32 KiB (the battery save's bytes).
-}
module Sms.Desktop () where

import Prelude

import Control.Monad.ST (RealWorld, stToIO)
import qualified Data.ByteString as BS
import Data.Bits (shiftL, shiftR, testBit, (.|.))
import Data.Int (Int16)
import Data.IORef
import qualified Data.Vector.Unboxed as U
import Data.Word (Word16, Word32, Word64, Word8)
import Foreign.C.Types (CInt (..), CSize (..))
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.StablePtr
import Foreign.Storable (peekElemOff, poke, pokeElemOff)

import Gg.Screen (frameHash, frameRgb)
import Gg.Spec
import qualified Gg.SpecFast as F
import Sms.Spec.FmUnit (newFmUnit)
import qualified Sms.SpecFast.FmUnit as FF
import Platform.Snapshot (Field (..), fnv1a, restore, snapshot)
import Z80.Spec (Regs (..), cpuPC, getRegs)

-- | The fast version or the reference model (the desktop's --reference).
data M = Fast !(F.Machine RealWorld) | Ref !(Machine RealWorld)

data Desk = Desk
  { dMachine :: !M
  , dAudio   :: !(IORef [(Int16, Int16)]) -- ^ the last frame's samples
  , dFields  :: ![Field RealWorld]         -- ^ for save states (Q.8: the fast version's, its FM unit's; none: no states)
  , dBlob    :: !(IORef BS.ByteString)     -- ^ the last save state taken
  }

-- The machine operations the library needs, on either.
mInput :: M -> Input -> IO ()
mInput (Fast m) = stToIO . F.setInput m
mInput (Ref m) = stToIO . setInput m

mFrame :: M -> IO [(Int16, Int16)]
mFrame (Fast m) = stToIO (F.runFrame m >> F.takeAudio m)
mFrame (Ref m) = stToIO (runFrame m >> takeAudio m)

mWidth :: M -> Int
mWidth (Fast m) = F.frameWidth m
mWidth (Ref m) = frameWidth m

mFrameBuffer :: M -> IO (U.Vector Word16)
mFrameBuffer (Fast m) = stToIO (F.frameBuffer m)
mFrameBuffer (Ref m) = stToIO (frameBuffer m)

mCartRam :: M -> IO (U.Vector Word8)
mCartRam (Fast m) = stToIO (F.cartRam m)
mCartRam (Ref m) = stToIO (cartRam m)

mLoadCartRam :: M -> BS.ByteString -> IO ()
mLoadCartRam (Fast m) = stToIO . F.loadCartRam m
mLoadCartRam (Ref m) = stToIO . loadCartRam m

foreign export ccall sms_new :: Ptr Word8 -> CSize -> CInt -> CInt -> Ptr Word8 -> CSize -> CInt -> Ptr (StablePtr ()) -> IO CInt
foreign export ccall sms_free :: StablePtr () -> IO ()
foreign export ccall sms_frame :: StablePtr () -> Word32 -> IO CInt
foreign export ccall sms_audio :: StablePtr () -> Ptr Int16 -> CInt -> IO CInt
foreign export ccall sms_width :: StablePtr () -> IO CInt
foreign export ccall sms_height :: StablePtr () -> IO CInt
foreign export ccall sms_pixels :: StablePtr () -> Ptr Word32 -> IO ()
foreign export ccall sms_hash :: StablePtr () -> IO Word64
foreign export ccall sms_cram_size :: StablePtr () -> IO CSize
foreign export ccall sms_cram_get :: StablePtr () -> Ptr Word8 -> CSize -> IO ()
foreign export ccall sms_cram_set :: StablePtr () -> Ptr Word8 -> CSize -> IO ()
foreign export ccall sms_state_save :: StablePtr () -> IO CSize
foreign export ccall sms_state_get :: StablePtr () -> Ptr Word8 -> CSize -> IO ()
foreign export ccall sms_state_load :: StablePtr () -> Ptr Word8 -> CSize -> IO CInt
foreign export ccall sms_dbg_regs :: StablePtr () -> Ptr Word16 -> IO CInt
foreign export ccall sms_dbg_peek :: StablePtr () -> Word16 -> IO CInt
foreign export ccall sms_dbg_poke :: StablePtr () -> Word16 -> Word8 -> IO CInt
foreign export ccall sms_dbg_vram :: StablePtr () -> Word16 -> IO CInt
foreign export ccall sms_dbg_vdp :: StablePtr () -> CInt -> IO CInt
foreign export ccall sms_dbg_step :: StablePtr () -> IO CInt
foreign export ccall sms_dbg_run :: StablePtr () -> Word32 -> Ptr Word16 -> CInt -> IO CInt

desk :: StablePtr () -> IO Desk
desk = deRefStablePtr . castPtrToStablePtr . castStablePtrToPtr

bytesAt :: Ptr Word8 -> CSize -> IO BS.ByteString
bytesAt p n = BS.pack <$> mapM (peekElemOff p) [0 .. fromIntegral n - 1]

-- | 0 and the handle in @out@, -2 for an unknown mode; @reference@ 1: the
-- reference model.
sms_new :: Ptr Word8 -> CSize -> CInt -> CInt -> Ptr Word8 -> CSize -> CInt -> Ptr (StablePtr ()) -> IO CInt
sms_new p n mode fm bp bn reference out = do
  rom <- bytesAt p n
  bios <- if bp == nullPtr || bn == 0 then pure Nothing else Just <$> bytesAt bp bn
  let mapper = detectMapper rom
      ref = reference /= 0
  -- (the FM unit's state comes from its maker: the machine only calls it)
  mm <- case mode of
    0 -> Just . (, []) <$> if ref then Ref <$> stToIO (newMachineWith False mapper rom) else Fast <$> stToIO (F.newMachineWith False mapper rom)
    1 -> do
      unit <- if fm == 0 then pure Nothing
        else if ref then (\u -> Just (u, [])) <$> stToIO newFmUnit
        else Just <$> stToIO FF.newFmUnitFields
      let u = fst <$> unit
          fmFs = maybe [] snd unit
      Just <$> if ref then (, []) . Ref <$> stToIO (newMasterSystemFm bios u mapper rom)
        else (, fmFs) . Fast <$> stToIO (F.newMasterSystemFm bios u mapper rom)
    2 -> Just . (, []) <$> if ref then Ref <$> stToIO (newMachineWith True mapper rom) else Fast <$> stToIO (F.newMachineWith True mapper rom)
    _ -> pure Nothing
  case mm of
    Nothing -> pure (-2)
    Just (m, fmFs) -> do
      -- (the ROM alone names the game: a state's layout is the reference
      -- model's too, so gg-run's command mode and its --movie take the
      -- states of the desktop's movies; with the FM unit the state is
      -- longer, and refused there)
      let game = fnv1a rom
          fs = case m of
            Fast f -> FCheck game : F.machineFields f <> fmFs
            Ref _ -> []
      d <- Desk m <$> newIORef [] <*> pure fs <*> newIORef BS.empty
      sp <- newStablePtr d
      poke out (castPtrToStablePtr (castStablePtrToPtr sp))
      pure 0

sms_free :: StablePtr () -> IO ()
sms_free = freeStablePtr

-- | One frame with the key word @w@; its stereo samples.
sms_frame :: StablePtr () -> Word32 -> IO CInt
sms_frame sp w = do
  d <- desk sp
  let m = dMachine d
  mInput m (keyInput w)
  a <- mFrame m
  writeIORef (dAudio d) a
  pure (fromIntegral (length a))

-- | The key word as the machine's input: the key maps' bits (0 Right, 1
-- Left, 2 Up, 3 Down, 4 button 2, 5 button 1; 7 Start; 8-13 pad 2) as
-- Gg.Spec's pad bits.
keyInput :: Word32 -> Input
keyInput w = Input (pad w) (testBit w 7) (pad (w `shiftR` 8))
 where
  pad x = (if testBit x 0 then padRight else 0) .|. (if testBit x 1 then padLeft else 0)
      .|. (if testBit x 2 then padUp else 0) .|. (if testBit x 3 then padDown else 0)
      .|. (if testBit x 4 then pad2 else 0) .|. (if testBit x 5 then pad1 else 0)

sms_audio :: StablePtr () -> Ptr Int16 -> CInt -> IO CInt
sms_audio sp out cap = do
  a <- readIORef . dAudio =<< desk sp
  let xs = take (fromIntegral cap) a
  mapM_ (\(i, (l, r)) -> pokeElemOff out (2 * i) l >> pokeElemOff out (2 * i + 1) r) (zip [0 ..] xs)
  pure (fromIntegral (length xs))

sms_width :: StablePtr () -> IO CInt
sms_width sp = fromIntegral . mWidth . dMachine <$> desk sp

sms_height :: StablePtr () -> IO CInt
sms_height sp = do
  m <- dMachine <$> desk sp
  fb <- mFrameBuffer m
  pure (fromIntegral (U.length fb `div` mWidth m))

sms_pixels :: StablePtr () -> Ptr Word32 -> IO ()
sms_pixels sp out = do
  fb <- mFrameBuffer . dMachine =<< desk sp
  U.imapM_ (\i c -> let (r, g, b) = frameRgb c in pokeElemOff out i (rgb r g b)) fb
 where
  rgb :: Word8 -> Word8 -> Word8 -> Word32
  rgb r g b = 0xFF000000 .|. (fromIntegral r `shiftL` 16) .|. (fromIntegral g `shiftL` 8) .|. fromIntegral b

sms_hash :: StablePtr () -> IO Word64
sms_hash sp = frameHash <$> (mFrameBuffer . dMachine =<< desk sp)

sms_cram_size :: StablePtr () -> IO CSize
sms_cram_size sp = fromIntegral . U.length <$> (mCartRam . dMachine =<< desk sp)

sms_cram_get :: StablePtr () -> Ptr Word8 -> CSize -> IO ()
sms_cram_get sp out n = do
  ram <- mCartRam . dMachine =<< desk sp
  U.imapM_ (pokeElemOff out) (U.take (fromIntegral n) ram)

sms_cram_set :: StablePtr () -> Ptr Word8 -> CSize -> IO ()
sms_cram_set sp p n = do
  m <- dMachine <$> desk sp
  bytesAt p n >>= mLoadCartRam m

-- * Save states (Q.8, "Platform.Snapshot"; as "Gb.Desktop"'s, the fast
-- version's only)

sms_state_save :: StablePtr () -> IO CSize
sms_state_save sp = do
  d <- desk sp
  if null (dFields d) then pure 0 else do
    b <- stToIO (snapshot (dFields d))
    writeIORef (dBlob d) b
    pure (fromIntegral (BS.length b))

sms_state_get :: StablePtr () -> Ptr Word8 -> CSize -> IO ()
sms_state_get sp out n = do
  d <- desk sp
  b <- readIORef (dBlob d)
  mapM_ (\i -> pokeElemOff out i (BS.index b i)) [0 .. min (fromIntegral n) (BS.length b) - 1]

sms_state_load :: StablePtr () -> Ptr Word8 -> CSize -> IO CInt
sms_state_load sp p n = do
  d <- desk sp
  if null (dFields d) then pure (-1) else do
    b <- bytesAt p n
    r <- stToIO (restore (dFields d) b)
    pure (either (const (-1)) (const 0) r)

-- * The debugger (side track R, R.1): the fast version only (-1 on the
-- reference model, as save states).

-- | The Z80's registers into @out@, 22 words: A F B C D E H L, AF' BC'
-- DE' HL', IX IY SP PC, I R, IFF1 IFF2 IM, halted; 22, or -1.
sms_dbg_regs :: StablePtr () -> Ptr Word16 -> IO CInt
sms_dbg_regs sp out = withFast sp $ \m -> do
  r <- stToIO (getRegs (F.mCpu m))
  let b8 = fromIntegral :: Word8 -> Word16
      bool b = if b then 1 else 0
      ws = [ b8 (rA r), b8 (rF r), b8 (rB r), b8 (rC r), b8 (rD r), b8 (rE r), b8 (rH r), b8 (rL r)
           , rAF' r, rBC' r, rDE' r, rHL' r, rIX r, rIY r, rSP r, rPC r
           , b8 (rI r), b8 (rR r), bool (rIFF1 r), bool (rIFF2 r), b8 (rIM r), bool (rHalted r) ]
  mapM_ (uncurry (pokeElemOff out)) (zip [0 ..] ws)
  pure 22

-- | A byte of the CPU's map (no side effects), or -1.
sms_dbg_peek :: StablePtr () -> Word16 -> IO CInt
sms_dbg_peek sp a = withFast sp $ \m -> fromIntegral <$> stToIO (F.peek m a)

-- | A write as the CPU's (RAM, cartridge RAM, the mapper's registers): 0, or -1.
sms_dbg_poke :: StablePtr () -> Word16 -> Word8 -> IO CInt
sms_dbg_poke sp a v = withFast sp $ \m -> 0 <$ stToIO (F.poke m a v)

-- | A VRAM byte (0-3FFFh), or -1.
sms_dbg_vram :: StablePtr () -> Word16 -> IO CInt
sms_dbg_vram sp a = withFast sp $ \m -> fromIntegral <$> stToIO (F.vramByte m (fromIntegral a))

-- | VDP register R0-R10, or -1.
sms_dbg_vdp :: StablePtr () -> CInt -> IO CInt
sms_dbg_vdp sp i = withFast sp $ \m -> fromIntegral <$> stToIO (F.vdpRegister m (fromIntegral i))

-- | One instruction (or an interrupt's response); 1 if a frame ended in
-- it (its sound then taken, as 'sms_frame''s), 0 if not, -1.
sms_dbg_step :: StablePtr () -> IO CInt
sms_dbg_step sp = do
  d <- desk sp
  case dMachine d of
    Ref _ -> pure (-1)
    Fast m -> do
      f0 <- stToIO (F.machineFrames m)
      _ <- stToIO (F.stepMachine m)
      f <- stToIO (F.machineFrames m)
      if f == f0 then pure 0 else do
        stToIO (F.takeAudio m) >>= writeIORef (dAudio d)
        pure 1

-- | The rest of a frame with the key word @w@, stopped before an
-- instruction at one of the @n@ breakpoints in @bps@ (not before the
-- first: a run goes on from one): 0 the frame ended, 1 a breakpoint, -1.
-- The sound so far is taken either way ('sms_audio').
sms_dbg_run :: StablePtr () -> Word32 -> Ptr Word16 -> CInt -> IO CInt
sms_dbg_run sp w bpsP n = do
  d <- desk sp
  case dMachine d of
    Ref _ -> pure (-1)
    Fast m -> do
      bps <- mapM (peekElemOff bpsP) [0 .. fromIntegral n - 1]
      mInput (dMachine d) (keyInput w)
      f0 <- stToIO (F.machineFrames m)
      let go first = do
            pc <- stToIO (cpuPC (F.mCpu m))
            if not first && pc `elem` bps then pure 1 else do
              _ <- stToIO (F.stepMachine m)
              f <- stToIO (F.machineFrames m)
              if f /= f0 then pure 0 else go False
      r <- go True
      stToIO (F.takeAudio m) >>= writeIORef (dAudio d)
      pure r

withFast :: StablePtr () -> (F.Machine RealWorld -> IO CInt) -> IO CInt
withFast sp k = do
  d <- desk sp
  case dMachine d of
    Fast m -> k m
    Ref _ -> pure (-1)
