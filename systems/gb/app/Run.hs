-- | Run a ROM on the reference model (Gb.Spec.runRom) and report.
--
-- > gb-run blargg|mooneye|frames=N <rom.gb> [M-cycles] [--png out.png]
--
-- prints the serial output, how the run stopped, the registers and the
-- suite's verdict; --png writes the last complete frame (grey, as the
-- goldens).
module Main (main) where

import Prelude

import qualified Data.ByteString as BS
import Data.List (stripPrefix)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import Gb.Hooks (MooneyeRegs (..), mooneyeVerdict)
import Gb.Screen (screenFrame)
import Gb.Spec
import Platform.Sim.Frame (writeFrame)

main :: IO ()
main = do
  args0 <- getArgs
  let (png, args) = case break (== "--png") args0 of
        (a, "--png" : f : b) -> (Just f, a <> b)
        _ -> (Nothing, args0)
  (suite, path, limit) <- case args of
    [s, p] -> (, p, 200000000) <$> suiteArg s
    [s, p, n] -> (, p, read n) <$> suiteArg s
    _ -> usage
  rom <- BS.readFile path
  case runRom suite limit rom of
    Left e -> hPutStrLn stderr ("gb-run: " <> e) >> exitFailure
    Right r -> do
      putStr (resSerial r)
      putStrLn ("\n-- " <> show (resStop r) <> " after " <> show (resCycles r) <> " M-cycles")
      print (resRegs r)
      let g = resRegs r
      putStrLn ("-- verdict: " <> case (suite, resStop r) of
        (Mooneye, StopBreakpoint) -> show (mooneyeVerdict (MooneyeRegs (rgB g) (rgC g) (rgD g) (rgE g) (rgH g) (rgL g)))
        (Blargg, StopSerial v) -> show v
        (BlarggMem, StopSerial v) -> show v
        _ -> "no result")
      mapM_ (\f -> writeFrame f (screenFrame (resScreen r))) png
 where
  usage = hPutStrLn stderr "usage: gb-run blargg|blargg-mem|mooneye|frames=N <rom.gb> [M-cycles] [--png out.png]" >> exitFailure
  suiteArg "blargg" = pure Blargg
  suiteArg "blargg-mem" = pure BlarggMem
  suiteArg "mooneye" = pure Mooneye
  suiteArg s | Just n <- stripPrefix "frames=" s = pure (Frames (read n))
  suiteArg _ = usage
