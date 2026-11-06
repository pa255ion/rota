{-# LANGUAGE BangPatterns #-}
{-|
The Atari 800XL's model behind a C ABI, for the desktop emulator (side
track Q, PLAN.md Q.5), as "Gb.Desktop" is for the Game Boy. It runs the
fast version ("A800.SpecFast.Machine"), which the a800xl-specfast suite
checks against the reference model cycle by cycle.

* @a800_new@ makes a machine (the fast version, or with @reference@ 1 the
  reference model "A800.Spec.Machine": the desktop's --reference) from
  the OS ROM, BASIC, and a program: none
  (BASIC's READY), an executable through the loader page (@make
  a800-asm@'s xexload.bin), or a cartridge ('parseCart': .car or raw).
* @a800_frame@ runs one frame with the PS link's 11 key bytes (the board's
  a800_keys.zig: the key matrix as bits 0 to 63, SHIFT CONTROL BREAK,
  START SELECT OPTION, the joystick's up down left right trigger), and
  samples the sound at 48 kHz; @a800_audio@ hands it over.
* @a800_pixels@ is the picture as the board shows it ("A800.Picture"'s
  320 × 240), 0xFFRRGGBB through "A800.Palette".

The sound: POKEY's level after each instruction (held over its cycles),
through "A800.Mixer"'s curve, then the board's filters ("A800.Audio": two
low-passes and a DC high-pass, there per pixel clock, here the same per
CPU cycle), sampled at 48 kHz.
-}
module A800.Desktop () where

import Prelude

import Control.Monad (when)
import Control.Monad.ST (RealWorld, ST, stToIO)
import Control.Monad.ST.Unsafe (unsafeIOToST)
import qualified Data.ByteString as BS
import Data.Bits (bit, complement, shiftL, testBit, (.&.), (.|.))
import Data.Int (Int16)
import Data.STRef
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import Data.Word (Word32, Word64, Word8)
import Foreign.C.Types (CInt (..), CSize (..))
import Foreign.Ptr (Ptr)
import Foreign.StablePtr
import Foreign.Storable (peekElemOff, poke, pokeElemOff)

import A800.Media (parseCart, parseCas, parseTape)
import A800.Mixer (mixCurve)
import A800.Palette (ntscPalette)
import A800.SpecFast.Antic (anticFrames)
import A800.SpecFast.Pokey (pokeyLevel)
import A800.SpecFast.Gtia (frameWidth, gtiaPicByte, setGtiaSkip, setTriggers)
import A800.SpecFast.Machine
import Platform.Snapshot (Field (..), fnv1a, restore, snapshot)
import qualified A800.Spec.Antic as SA
import qualified A800.Spec.Gtia as SG
import qualified A800.Spec.Machine as S
import qualified A800.Spec.Pokey as SP

data Filt = Filt !Double !Double !Double
  deriving (Show, Read)

-- | What the library needs of a machine: the fast version's or the
-- reference model's ('fastOps', 'refOps').
data Ops = Ops
  { oInputs :: Word64 -> Word8 -> Word8 -> Word8 -> Word8 -> ST RealWorld ()  -- ^ keys, modifiers, console, port A, triggers
  , oStep   :: ST RealWorld ()
  , oFrames :: ST RealWorld Int
  , oCycles :: ST RealWorld Int
  , oLevel  :: ST RealWorld Int    -- ^ POKEY's mixed level
  , oPic    :: Int -> ST RealWorld Word8  -- ^ byte i of GTIA's picture, 'frameWidth' per line
  , oSkip   :: Bool -> ST RealWorld ()    -- ^ frame skipping (the fast version only)
  , oFields :: [Field RealWorld]          -- ^ for save states (the fast version only; none: no states)
  }

fastOps :: Machine RealWorld -> Ops
fastOps m = Ops
  { oInputs = \k md con pa tr -> setKeys m k md >> setConsole m con >> setJoystick m pa >> setTriggers (mGtia m) tr
  , oStep = () <$ stepCpu m
  , oFrames = anticFrames (mAntic m)
  , oCycles = machineCycles m
  , oLevel = pokeyLevel (mPokey m)
  , oPic = gtiaPicByte (mGtia m)
  , oSkip = setGtiaSkip (mGtia m)
  , oFields = machineFields m
  }

refOps :: S.Machine RealWorld -> Ops
refOps m = Ops
  { oInputs = \k md con pa tr -> S.setKeys m k md >> S.setConsole m con >> S.setJoystick m pa >> SG.setTriggers (S.mGtia m) tr
  , oStep = () <$ S.stepCpu m
  , oFrames = SA.anticFrames (S.mAntic m)
  , oCycles = S.machineCycles m
  , oLevel = SP.pokeyLevel (S.mPokey m)
  , oPic = \i -> let (y, x) = i `quotRem` frameWidth in (\(a, b) -> if even x then a else b) <$> SG.gtiaPixels (S.mGtia m) y (x `quot` 2)
  , oSkip = const (pure ())
  , oFields = []
  }

data Desk = Desk
  { dOps     :: !Ops
  , dAcc     :: !(STRef RealWorld Int)
  , dLast    :: !(STRef RealWorld Int)          -- ^ the machine cycle at the last sample check
  , dFilt    :: !(STRef RealWorld Filt)
  , dAudio   :: !(MU.MVector RealWorld Int16)
  , dCount   :: !(STRef RealWorld Int)
  , dCas     :: !Bool                           -- ^ a cassette: booted by holding START, then Space ('a800_frame')
  , dBlob    :: !(STRef RealWorld BS.ByteString) -- ^ the last save state taken
  , dId      :: !Word64                          -- ^ the program ('fnv1a' of its file): a state is its
  }

audioCap :: Int
audioCap = 2048

-- | Machine cycles a frame may take before 'a800_frame' returns anyway
-- (one is 114 × 262 = 29868).
frameCap :: Int
frameCap = 2 * 29868

cpuHz :: Int
cpuHz = 1789773

foreign export ccall a800_new :: Ptr Word8 -> CSize -> Ptr Word8 -> CSize -> Ptr Word8 -> CSize -> Ptr Word8 -> CSize -> CInt -> CInt -> Ptr (StablePtr ()) -> IO CInt
foreign export ccall a800_free :: StablePtr () -> IO ()
foreign export ccall a800_frame :: StablePtr () -> Ptr Word8 -> IO CInt
foreign export ccall a800_audio :: StablePtr () -> Ptr Int16 -> CInt -> IO CInt
foreign export ccall a800_pixels :: StablePtr () -> Ptr Word32 -> IO ()
foreign export ccall a800_skip :: StablePtr () -> CInt -> IO ()
foreign export ccall a800_state_save :: StablePtr () -> IO CSize
foreign export ccall a800_state_get :: StablePtr () -> Ptr Word8 -> CSize -> IO ()
foreign export ccall a800_state_load :: StablePtr () -> Ptr Word8 -> CSize -> IO CInt

desk :: StablePtr () -> IO Desk
desk = deRefStablePtr . castPtrToStablePtr . castStablePtrToPtr

bytesAt :: Ptr Word8 -> CSize -> IO BS.ByteString
bytesAt p n = BS.pack <$> mapM (peekElemOff p) [0 .. fromIntegral n - 1]

-- | @kind@: 0 none (BASIC), 1 an executable (with the loader), 2 a
-- cartridge, 3 a cassette (.cas; side track Q, booted through the OS's
-- SIO call, "A800.Spec.Machine"'s casTrap), 4 a cassette played by the
-- 410 recorder itself (depth 2, 'setTape': every byte through POKEY's
-- serial input at the tape's speed; custom loaders too). 0 and the handle in @out@; -1
-- a cartridge or cassette refused, -2 an unknown kind.
a800_new :: Ptr Word8 -> CSize -> Ptr Word8 -> CSize -> Ptr Word8 -> CSize -> Ptr Word8 -> CSize -> CInt -> CInt -> Ptr (StablePtr ()) -> IO CInt
a800_new op on bp bn lp ln fp fn kind reference out = do
  os <- bytesAt op on
  basic <- bytesAt bp bn
  file <- bytesAt fp fn
  cfg <- case kind of
    0 -> pure (Right (Config os (Just basic) Nothing Nothing Nothing))
    1 -> do
      loader <- bytesAt lp ln
      pure (Right (Config os (Just basic) Nothing (Just (XexBoot loader file)) Nothing))
    -- (a .car file by its header, else a raw image: the desktop's .rom)
    2 -> pure (either Left (\c -> Right (Config os (Just basic) (Just c) Nothing Nothing))
                (parseCart (if BS.take 4 file == BS.pack [0x43, 0x41, 0x52, 0x54] then "cart.car" else "cart.rom") file))
    3 -> pure (either Left (\t -> Right (Config os (Just basic) Nothing Nothing (Just t))) (parseCas file))
    4 -> pure (either Left (const (Right (Config os (Just basic) Nothing Nothing Nothing))) (parseTape file))
    _ -> pure (Left "kind")
  case cfg of
    Left "kind" -> pure (-2)
    Left _ -> pure (-1)
    Right c -> do
      let tape = if kind == 4 then either (const Nothing) Just (parseTape file) else Nothing
      ops <- stToIO $ if reference /= 0
        then S.newMachine c >>= \mm -> S.setTape mm tape >> pure (refOps mm)
        else newMachine c >>= \mm -> setTape mm tape >> pure (fastOps mm)
      d <- stToIO (Desk ops <$> newSTRef 0 <*> newSTRef 0 <*> newSTRef (Filt 0 0 0) <*> MU.new (2 * audioCap) <*> newSTRef 0 <*> pure (kind == 3 || kind == 4)
                     <*> newSTRef BS.empty <*> pure (fnv1a (BS.cons (fromIntegral kind) (os <> basic <> file))))
      sp <- newStablePtr d
      poke out (castPtrToStablePtr (castStablePtrToPtr sp))
      pure 0

a800_free :: StablePtr () -> IO ()
a800_free = freeStablePtr

-- | One frame with the 11 key bytes at @kp@; its stereo samples.
a800_frame :: StablePtr () -> Ptr Word8 -> IO CInt
a800_frame sp kp = do
  d <- desk sp
  ks <- mapM (peekElemOff kp) [0 .. 10]
  let o = dOps d
      keys = foldr (\b acc -> (acc `shiftL` 8) .|. fromIntegral b) 0 (take 8 ks) :: Word64
      mods = ks !! 8 .&. 7
      console = ks !! 9 .&. 7
      joy = ks !! 10
      -- port A: bits 0-3 stick 1 up down left right, 0 = pushed; stick 2 released
      porta = 0xF0 .|. (complement joy .&. 0x0F)
  stToIO $ do
    writeSTRef (dCount d) 0
    f0 <- oFrames o
    -- a cassette boots as a user does it (A800.Spec.runCasBoot): START
    -- held for the first second, then Space when the OS asks for a key
    let bootCon = if dCas d && f0 < 60 then 1 else 0
        bootKey = if dCas d && f0 >= 120 && f0 < 126 then bit 0x21 else 0
    oInputs o (keys .|. bootKey) mods (console .|. bootCon) porta (if testBit joy 4 then 1 else 0)
    c0 <- oCycles o
    let go = do
          oStep o
          samples d
          f <- oFrames o
          c <- oCycles o
          when (f == f0 && c - c0 < frameCap) go
    go
    fromIntegral <$> readSTRef (dCount d)

-- | The board's filter per CPU cycle (its pixel clock, about 25.2 MHz,
-- over the CPU's): 1 - (1 - alpha)^perCycle.
lpA, dcA :: Double
lpA = 1 - (1 - 1 / 256) ** (25.2e6 / fromIntegral cpuHz)
dcA = 1 - (1 - 1 / 262144) ** (25.2e6 / fromIntegral cpuHz)

-- | The cycles since the last check at POKEY's level now (constant over
-- them: POKEY's output changes only with its own steps, which the CPU's
-- instruction granularity is finer than for sound), through the filter in
-- closed form rather than cycle by cycle: with r = 1 - lpA and the two
-- low-passes' errors from the input e_a, e_b, after k cycles e_a r^k and
-- r^k (e_b + k (1 - r) e_a) (the second pole takes the first's new
-- value each cycle, as "A800.Audio" does); the DC level, far slower, as
-- one step of k. A sample at each 48 kHz instant the cycles pass.
samples :: Desk -> ST RealWorld ()
samples d = do
  let o = dOps d
  c <- oCycles o
  l <- readSTRef (dLast d)
  writeSTRef (dLast d) c
  lvl <- oLevel o
  -- A800.Audio reads the curve's 16 bits as signed
  let v = mixCurve lvl
      x = fromIntegral (if v > 32767 then v - 65536 else v) :: Double
      cycles = c - l
  Filt a b dc <- readSTRef (dFilt d)
  let ea = a - x
      eb = b - x
      at k = let !rk = rPow k; !ak = x + ea * rk; !bk = x + rk * (eb + fromIntegral k * lpA * ea) in (ak, bk)
      -- the 48 kHz instants among the next cycles: the phase accumulates
      -- 48000 a cycle and a sample is due when it reaches cpuHz, so the
      -- next is ceil ((cpuHz - acc) / 48000) cycles on (about 37)
      emit acc k0
        | k > cycles = pure (acc + 48000 * (cycles - k0 + 1))
        | otherwise = do
            let (_, bk) = at k
            n <- readSTRef (dCount d)
            when (n < audioCap) $ do
              let x0 = fromIntegral (max (-32767) (min 32767 (round (bk - dc) :: Int))) :: Int16
              MU.write (dAudio d) (2 * n) x0
              MU.write (dAudio d) (2 * n + 1) x0
              writeSTRef (dCount d) (n + 1)
            emit (acc + 48000 * (k - k0 + 1) - cpuHz) (k + 1)
       where
        k = k0 + (cpuHz - acc + 47999) `div` 48000 - 1
  acc0 <- readSTRef (dAcc d)
  -- next 48 kHz instants: jump by whole sample periods (one is 37.3 cycles)
  accN <- if cycles <= 0 then pure acc0 else emit acc0 1
  writeSTRef (dAcc d) accN
  let (aN, bN) = at cycles
      -- (Q.7: the DC step's power from a table too, not pow every instruction)
      !dcN = dc + (bN - dc) * (1 - dcPow cycles)
  writeSTRef (dFilt d) (Filt aN bN dcN)

-- | (1 - lpA)^k, from a table for the usual instruction lengths.
rPow :: Int -> Double
rPow k
  | k < 512 = U.unsafeIndex rTable k
  | otherwise = (1 - lpA) ** fromIntegral k

rTable :: U.Vector Double
rTable = U.generate 512 (\k -> (1 - lpA) ** fromIntegral k)
{-# NOINLINE rTable #-}

-- | (1 - dcA)^k, the same way (Q.7: was a pow call every instruction).
dcPow :: Int -> Double
dcPow k
  | k < 512 = U.unsafeIndex dcTable k
  | otherwise = (1 - dcA) ** fromIntegral k

dcTable :: U.Vector Double
dcTable = U.generate 512 (\k -> (1 - dcA) ** fromIntegral k)
{-# NOINLINE dcTable #-}

a800_audio :: StablePtr () -> Ptr Int16 -> CInt -> IO CInt
a800_audio sp out cap = do
  d <- desk sp
  n <- min (fromIntegral cap) <$> stToIO (readSTRef (dCount d))
  mapM_ (\i -> stToIO (MU.read (dAudio d) i) >>= pokeElemOff out i) [0 .. 2 * n - 1]
  pure (fromIntegral n)

a800_pixels :: StablePtr () -> Ptr Word32 -> IO ()
a800_pixels sp out = do
  d <- desk sp
  -- "A800.Picture"'s crop: 320 × 240 from line 8, colour clock 0x30
  -- (one ST action for the whole picture, not one per pixel)
  let pic = oPic (dOps d)
      px !i
        | i >= 320 * 240 = pure ()
        | otherwise = do
            let (y, x) = i `quotRem` 320
            c <- pic ((y + 8) * frameWidth + 2 * 0x30 + x)
            unsafeIOToST (pokeElemOff out i (U.unsafeIndex palette32 (fromIntegral c)))
            px (i + 1)
  stToIO (px 0)

palette32 :: U.Vector Word32
palette32 = U.map (\(r, g, b) -> 0xFF000000 .|. (fromIntegral r `shiftL` 16) .|. (fromIntegral g `shiftL` 8) .|. fromIntegral b) ntscPalette
{-# NOINLINE palette32 #-}

-- | The next frames' picture not drawn (1) or drawn (0): frame skipping
-- (Q.6), when the desktop runs behind; GTIA's collisions and everything
-- else the CPU reads go on as ever.
a800_skip :: StablePtr () -> CInt -> IO ()
a800_skip sp on = do
  d <- desk sp
  stToIO (oSkip (dOps d) (on /= 0))

-- * Save states (Q.8, "Platform.Snapshot"; as "Gb.Desktop"'s, the fast
-- version's only)

stateFields :: Desk -> [Field RealWorld]
stateFields d = FCheck (dId d) : oFields (dOps d) <> [FRef (dAcc d), FRef (dLast d), FRef (dFilt d)]

a800_state_save :: StablePtr () -> IO CSize
a800_state_save sp = do
  d <- desk sp
  if null (oFields (dOps d)) then pure 0 else do
    b <- stToIO (snapshot (stateFields d))
    stToIO (writeSTRef (dBlob d) b)
    pure (fromIntegral (BS.length b))

a800_state_get :: StablePtr () -> Ptr Word8 -> CSize -> IO ()
a800_state_get sp out n = do
  d <- desk sp
  b <- stToIO (readSTRef (dBlob d))
  mapM_ (\i -> pokeElemOff out i (BS.index b i)) [0 .. min (fromIntegral n) (BS.length b) - 1]

a800_state_load :: StablePtr () -> Ptr Word8 -> CSize -> IO CInt
a800_state_load sp p n = do
  d <- desk sp
  if null (oFields (dOps d)) then pure (-1) else do
    b <- bytesAt p n
    r <- stToIO (restore (stateFields d) b)
    pure (either (const (-1)) (const 0) r)
