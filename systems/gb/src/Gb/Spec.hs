{-# LANGUAGE MultiWayIf #-}
{-|
Reference model of the Game Boy (DMG), PLAN.md step 2.1: the SM83 CPU
("Gb.Spec.Cpu") on the machine of "Gb.Spec.Machine" (timer, serial port,
LCD timing stand-in, cartridge). It starts in the post-boot-ROM state; no
boot ROM is needed or used.

Two interfaces:

* __'ST'__ ('newMachine', 'step', 'regs', 'withTrace', ...) for tests that
  look inside, and for the lockstep tests of step 2.4, which compare the
  bus trace ('Access', one per M-cycle) and the registers after each
  'step'.
* __Pure__ ('runRom'): run a ROM image until the Mooneye breakpoint
  (@LD B,B@), a Blargg verdict on the serial port, or a cycle limit.
-}
module Gb.Spec
  ( -- * Running
    Suite (..)
  , Stop (..)
  , Result (..)
  , runRom
  , runRomModel
  , Model (..)
    -- * In ST
  , Machine
  , Access (..)
  , newMachine
  , newMachineModel
  , newMachineModelPal
  , step
  , Regs (..)
  , regs
  , cycles
  , serialOut
  , withTrace
  , screen
  , screenRgb
  , frames
  , setJoypad
  , audioSample
  , audioRun
  , peek
  , poke
  ) where

import Prelude

import Control.Monad.ST (ST, runST)
import qualified Data.ByteString as BS
import Data.STRef
import qualified Data.Vector.Unboxed as U
import Data.Bits ((.&.))
import Data.Word

import Gb.Hooks
import Gb.Screen (Screen, mkScreen)
import Gb.Spec.Cpu (step)
import Gb.Spec.Apu (channelOutputs, mix)
import Gb.Spec.Machine

data Regs = Regs
  { rgA, rgF, rgB, rgC, rgD, rgE, rgH, rgL :: !Word8
  , rgSP, rgPC :: !Word16
  , rgIME :: !Bool
  } deriving stock (Show, Eq)

regs :: Machine s -> ST s Regs
regs m = do
  let r = getR m
  b <- r rB; c <- r rC; d <- r rD; e <- r rE
  h <- r rH; l <- r rL; f <- r rF; a <- r rA
  sp <- getSt m iSP
  pc <- getSt m iPC
  ime <- getSt m iIME
  pure (Regs a f b c d e h l (fromIntegral sp) (fromIntegral pc) (ime /= 0))

-- | M-cycles since the hand-off.
cycles :: Machine s -> ST s Int
cycles m = getSt m iCYC

-- | Bytes sent over the serial port so far (Blargg's output).
serialOut :: Machine s -> ST s String
serialOut m = serialText <$> readSTRef (mSerial m)

-- | Run an action with the bus trace on; returns its result and the
-- M-cycles it took, oldest first.
withTrace :: Machine s -> ST s a -> ST s (a, [Access])
withTrace m act = do
  writeSTRef (mTrace m) []
  setSt m iTRACE 1
  r <- act
  setSt m iTRACE 0
  t <- readSTRef (mTrace m)
  pure (r, reverse t)

-- | How a test ROM reports its result. Blargg's ROMs execute @LD B,B@ as
-- an ordinary instruction (@06-ld r,r@, @instr_timing@), so the breakpoint
-- only ends a Mooneye run.
--
-- 'Frames' runs a ROM for that many frames (VBlank entries), for screen
-- tests such as dmg-acid2.
--
-- 'BlarggMem': Blargg's newer ROMs (dmg_sound) also report in cartridge
-- RAM: signature DE B0 61 at 0xA001, status at 0xA000 (0x80 while
-- running, 0 passed), text from 0xA004.
data Suite = Blargg | BlarggMem | Mooneye | Frames !Int
  deriving stock (Show, Eq)

data Stop
  = StopBreakpoint    -- ^ @LD B,B@ executed (Mooneye's end of test)
  | StopSerial Verdict -- ^ Blargg's "Passed" / "Failed" on the serial port
  | StopFrames        -- ^ 'Frames': the frames are done
  | StopCycles        -- ^ the limit was reached first
  deriving stock (Show, Eq)

data Result = Result
  { resStop   :: Stop
  , resSerial :: String
  , resRegs   :: Regs
  , resCycles :: Int
  , resScreen :: Screen -- ^ the last complete frame
  } deriving stock (Show, Eq)

-- | Run a ROM image for at most this many M-cycles, until the suite's end
-- signal. After a Blargg
-- failure it keeps going for up to 2^21 M-cycles (2 s), so that the
-- details after "Failed" are captured too.
runRom :: Suite -> Int -> BS.ByteString -> Either String Result
runRom = runRomModel Dmg

-- | 'runRom' on a model (side track I, phase 3: the Game Boy Color).
runRomModel :: Model -> Suite -> Int -> BS.ByteString -> Either String Result
runRomModel model suite limit rom = runST $ do
  em <- newMachineModel model rom
  case em of
    Left e -> pure (Left e)
    Right m -> Right <$> loop m limit
 where
  loop :: Machine s -> Int -> ST s Result
  loop m lim = do
    step m
    brk <- getSt m iBREAK
    new <- getSt m iSERNEW
    n <- getSt m iCYC
    fr <- getSt m iFRAMES
    n0 <- getSt m iCYC
    memV <- if suite == BlarggMem && n0 .&. 0xFFF < 6 then memVerdict m else pure Running
    if | brk /= 0, suite == Mooneye -> done m StopBreakpoint
       | memV /= Running -> done m (StopSerial memV)
       | Frames k <- suite, fr >= k -> done m StopFrames
       | new /= 0, suite == Blargg -> do
           setSt m iSERNEW 0
           v <- blarggVerdict <$> serialOut m
           case v of
             Pass -> done m (StopSerial Pass)
             Fail _ | lim == limit -> loop m (min limit (n + 2 ^ (21 :: Int)))
             _ -> loop m lim
       | n >= lim -> do
           v <- blarggVerdict <$> serialOut m
           done m (if lim < limit then StopSerial v else StopCycles)
       | otherwise -> loop m lim
  done m s = Result s <$> serialOut m <*> regs m <*> cycles m <*> screen m

-- | Blargg's result in cartridge RAM, if the signature is there.
memVerdict :: Machine s -> ST s Verdict
memVerdict m = do
  sig <- mapM (peek m) [0xA001, 0xA002, 0xA003]
  st <- peek m 0xA000
  if sig /= [0xDE, 0xB0, 0x61] || st == 0x80 then pure Running else do
    txt <- memText m
    pure (if st == 0 then Pass else Fail txt)

-- | The zero-terminated text at 0xA004.
memText :: Machine s -> ST s String
memText m = go 0xA004 []
 where
  go a acc
    | a >= 0xC000 = pure (reverse acc)
    | otherwise = do
        b <- peek m a
        if b == 0 then pure (reverse acc) else go (a + 1) (toEnum (fromIntegral b) : acc)

-- | The last complete frame.
screen :: Machine s -> ST s Screen
screen m = either error id . mkScreen <$> U.freeze (mLast m)

-- | The last complete frame in colour (the Game Boy Color: 15-bit BGR,
-- red in bits 0 to 4), 160 × 144, row by row.
screenRgb :: Machine s -> ST s (U.Vector Word16)
screenRgb m = U.freeze (mLastC m)

-- | The mixed stereo sample now (Gb.Spec.Apu.mix: signed, ±480).
audioSample :: Machine s -> ST s (Int, Int)
audioSample m = do
  nr50 <- ramRead m 0xFF24
  nr51 <- ramRead m 0xFF25
  mix nr50 nr51 <$> channelOutputs m

-- | Run a ROM for @n@ M-cycles and sample its audio at 48 kHz (taking the
-- sample current at each 48 kHz instant, measured in M-cycles of
-- 1048576 Hz); the samples, oldest first.
audioRun :: Int -> BS.ByteString -> Either String [(Int, Int)]
audioRun n rom = runST $ do
  em <- newMachine rom
  case em of
    Left e -> pure (Left e)
    Right m -> do
      acc <- newSTRef (0 :: Int)
      out <- newSTRef []
      let hook = Hook (pure ()) (do
            a <- (+ 48000) <$> readSTRef acc
            if a >= 1048576
              then do
                writeSTRef acc (a - 1048576)
                smp <- audioSample m
                modifySTRef' out (smp :)
              else writeSTRef acc a) (\_ _ -> pure ())
      writeSTRef (mHook m) (Just hook)
      let go = do
            c <- getSt m iCYC
            if c >= n then pure () else step m >> go
      go
      Right . reverse <$> readSTRef out

-- | The buttons held (bit 0 Right, 1 Left, 2 Up, 3 Down, 4 A, 5 B,
-- 6 Select, 7 Start).
setJoypad :: Machine s -> Word8 -> ST s ()
setJoypad m = setSt m iJOY . fromIntegral

-- | Frames completed so far.
frames :: Machine s -> ST s Int
frames m = getSt m iFRAMES
