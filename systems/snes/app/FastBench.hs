-- | Side track Q (PLAN.md Q.5): the SNES's reference model or its fast
-- version ("Snes.SpecFast") on a ROM for N frames, to compare their
-- speed and allocation (+RTS -s):
--
-- > snes-fast-bench spec|fast|fast-inline|fast-nodraw IPL ROM FRAMES +RTS -s -N2
--
-- (fast: the drawing on a second thread; fast-inline: in the machine's;
-- fast-nodraw: none, the CPU side alone)
module Main (main) where

import Prelude

import Control.Monad.ST (stToIO)
import qualified Data.ByteString as B
import System.Environment (getArgs)

import Snes.Cart (parseCart)
import qualified Snes.Spec.Machine as S
import qualified Snes.SpecFast.Machine as F

main :: IO ()
main = do
  [which, iplF, romF, n] <- getArgs
  ipl <- B.readFile iplF
  file <- B.readFile romF
  let Just cart = parseCart file
      frames = read n :: Int
  case which of
    "spec" -> stToIO (S.newMachine ipl cart file >>= \m -> S.reset m >> S.runFrames m frames)
    "fast-nodraw" -> stToIO (F.newMachineWith 0 ipl cart file >>= \m -> F.setSkipDraw m True >> F.reset m >> F.runFrames m frames)
    "fast-inline" -> stToIO (F.newMachineWith 0 ipl cart file >>= \m -> F.reset m >> F.runFrames m frames >> () <$ F.frameColors m)
    _ -> stToIO (F.newMachineWith 2 ipl cart file >>= \m -> F.reset m >> F.runFrames m frames >> () <$ F.frameColors m)
  putStrLn ("ran " <> n <> " frames on the " <> which)
