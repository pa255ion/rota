-- | Side track Q (PLAN.md Q.5): the 800XL's reference model or its fast
-- version ("A800.SpecFast") on a program for N frames, to compare their
-- speed and allocation (+RTS -s):
--
-- > a800-fast-bench spec|fast OS BASIC LOADER PROGRAM.xex FRAMES +RTS -s
module Main (main) where

import Prelude

import Control.Monad.ST (stToIO)
import qualified Data.ByteString as BS
import System.Environment (getArgs)

import A800.Spec (xexConfig)
import A800.Spec.Antic (anticFrames)
import qualified A800.Spec.Machine as S
import qualified A800.SpecFast.Antic as FA
import qualified A800.SpecFast.Machine as F

main :: IO ()
main = do
  [which, os, basic, loader, prog, n] <- getArgs
  cfg <- xexConfig <$> BS.readFile os <*> BS.readFile basic <*> BS.readFile loader <*> BS.readFile prog
  let frames = read n :: Int
  case which of
    "spec" -> do
      m <- stToIO (S.newMachine cfg)
      let go = do
            _ <- S.stepCpu m
            f <- anticFrames (S.mAntic m)
            if f < frames then go else pure ()
      stToIO go
    _ -> do
      m <- stToIO (F.newMachine cfg)
      let go = do
            _ <- F.stepCpu m
            f <- FA.anticFrames (F.mAntic m)
            if f < frames then go else pure ()
      stToIO go
  putStrLn ("ran " <> n <> " frames on the " <> which)
