-- | Side track Q (PLAN.md Q.7): where the NES reference model's time goes,
-- to choose what a fast version speeds up (+RTS -s for the allocation):
--
-- > nes-fast-bench spec|trace|ppu|apu|fast|fast-skip|fast-ppu ROM FRAMES
--
-- * spec: the machine for FRAMES frames (no sound out);
-- * fast, fast-ppu: the fast version's ("Nes.SpecFast"), the machine with
--   its sound kept and its PPU alone, as spec and ppu (fast-skip: every
--   frame's picture skipped, the cost of a skipped frame);
-- * trace: the same with the bus and APU traces on, as the desktop runs it
--   for its sound (every instruction in 'withTrace', the APU's outputs read);
-- * ppu: 60 frames of the machine, then its PPU alone for FRAMES frames'
--   dots (the registers as the game left them);
-- * apu: the APU alone for FRAMES frames' CPU cycles.
module Main (main) where

import Prelude

import Control.Monad (replicateM_, when)
import Control.Monad.ST (ST, stToIO)
import qualified Data.ByteString as B
import qualified Data.Vector.Unboxed as U
import System.Environment (getArgs)

import Nes.Spec.Apu (apuCycle)
import Nes.Spec.Machine
import Nes.Spec.Ppu (ppuDot, ppuFramesDone)
import qualified Nes.SpecFast.Machine as F
import qualified Nes.SpecFast.Ppu as FP

-- | Dots and CPU cycles a frame (NTSC).
dotsPerFrame, cyclesPerFrame :: Int
dotsPerFrame = 341 * 262
cyclesPerFrame = 29781

main :: IO ()
main = do
  [which, romF, n] <- getArgs
  file <- B.readFile romF
  let frames = read n :: Int
  r <- stToIO $ if which `elem` ["fast", "fast-skip", "fast-ppu"] then do
    em <- F.newMachine file
    case em of
      Left e -> pure e
      Right m -> do
        F.setSound m True
        when (which == "fast-skip") (FP.ppuSetSkip (F.mPpu m) True)
        -- a frame at a time, its sound taken (as the desktop does)
        let runF k = replicateM_ k $ do
              f0 <- FP.ppuFramesDone (F.mPpu m)
              let go = do
                    _ <- F.stepCpu m
                    f <- FP.ppuFramesDone (F.mPpu m)
                    when (f == f0) go
              go
              s <- F.takeSound m
              when (U.length s < 0) (pure ())
        if which /= "fast-ppu" then runF frames else runF 60 >> replicateM_ (frames * dotsPerFrame) (FP.ppuDot (F.mPpu m))
        c <- F.machineCycles m
        pure (which <> ": " <> show c <> " CPU cycles")
   else do
    em <- newMachine file
    case em of
      Left e -> pure e
      Right m -> do
        case which of
          "spec" -> run m frames False
          "trace" -> run m frames True
          "ppu" -> run m 60 False >> replicateM_ (frames * dotsPerFrame) (ppuDot (mPpu m))
          "apu" -> replicateM_ (frames * cyclesPerFrame) (apuCycle (mApu m))
          _ -> pure ()
        c <- machineCycles m
        pure (which <> ": " <> show c <> " CPU cycles")
  putStrLn r

-- | @k@ frames of the machine, each instruction in 'withTrace' if @tr@.
run :: Machine s -> Int -> Bool -> ST s ()
run m k tr = do
  f0 <- ppuFramesDone (mPpu m)
  let go = do
        if tr
          then do
            (_, acc) <- withTrace m (stepCpu m)
            ch <- apuTrace m
            when (length acc + length ch < 0) (pure ())
          else () <$ stepCpu m
        f <- ppuFramesDone (mPpu m)
        when (f - f0 < k) go
  go
