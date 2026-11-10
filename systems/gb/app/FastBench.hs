-- | Side track Q for side track W: where the Game Boy's time goes, the
-- reference model's or the fast version's (+RTS -s for the allocation; a
-- profiling build with -fprof-late for the functions):
--
-- > gb-fast-bench spec|fast dmg|cgb ROM FRAMES
module Main (main) where

import Prelude

import Control.Monad (replicateM_, when)
import Control.Monad.ST (stToIO)
import qualified Data.ByteString as B
import System.Environment (getArgs)

import qualified Gb.Spec as S
import qualified Gb.Spec.Machine as SM
import qualified Gb.Spec.State as SS
import qualified Gb.SpecFast as F

main :: IO ()
main = do
  [which, model, romF, n] <- getArgs
  rom <- B.readFile romF
  let frames = read n :: Int
      cgb = model == "cgb"
      cap = 2 * 17556
  if which == "fast"
    then do
      em <- stToIO (F.newMachineModel (if cgb then F.Cgb else F.Dmg) rom)
      case em of
        Left e -> putStrLn e
        Right m -> do
          replicateM_ frames $ do
            f0 <- stToIO (F.frames m)
            c0 <- stToIO (F.cycles m)
            let go = do
                  stToIO (F.step m)
                  f <- stToIO (F.frames m)
                  c <- stToIO (F.cycles m)
                  when (f == f0 && c - c0 < cap) go
            go
          c <- stToIO (F.cycles m)
          putStrLn ("fast: " <> show c <> " M-cycles")
    else do
      em <- stToIO (SM.newMachineModel (if cgb then SS.Cgb else SS.Dmg) rom)
      case em of
        Left e -> putStrLn e
        Right m -> do
          replicateM_ frames $ do
            f0 <- stToIO (S.frames m)
            c0 <- stToIO (S.cycles m)
            let go = do
                  stToIO (S.step m)
                  f <- stToIO (S.frames m)
                  c <- stToIO (S.cycles m)
                  when (f == f0 && c - c0 < cap) go
            go
          c <- stToIO (S.cycles m)
          putStrLn ("spec: " <> show c <> " M-cycles")
