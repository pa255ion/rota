-- | Run a ROM on the reference model:
--
-- > cabal run nes-run -- <rom.nes> [CPU cycles]                 Blargg's result
-- > cabal run nes-run -- --trace K <rom.nes> [CPU cycles]       last K instructions
-- > cabal run nes-run -- --screen <rom.nes> [CPU cycles]        2005 tests: $F8 and screen text
-- > cabal run nes-run -- --png N out.png <rom.nes>              frame N as a PNG
-- > cabal run nes-run -- --accuracycoin <AccuracyCoin.nes>      run all: every result, the score
-- > cabal run nes-run -- --accuracycoin-trace K [--stop PC [--after CYCLES]] <AccuracyCoin.nes> N
-- >                    run all for N cycles (or to PC, hex): last K instructions, RAM pages 0, 5, 6
module Main (main) where

import Prelude

import qualified Data.ByteString as BS
import System.Environment (getArgs)
import System.Exit (exitFailure)

import Numeric (showHex)

import Nes.AccuracyCoin (resultLines)
import Nes.Hooks
import Nes.Screen
import Nes.Spec
import Platform.Sim.Frame (writeFrame)

main :: IO ()
main = do
  args <- getArgs
  case args of
    "--trace" : k : rest -> withRom rest $ \rom lim ->
      either (\e -> putStrLn e >> exitFailure) (mapM_ putStrLn) (traceTail (read k) lim rom)
    "--screen" : rest -> withRom rest $ \rom lim ->
      case runOldScreenTest lim rom of
        Left e -> putStrLn e >> exitFailure
        Right r -> do
          mapM_ putStrLn (orText r)
          putStrLn ("$F8 = " <> show (orCode r) <> " after " <> show (orCycles r) <> " cycles")
    "--accuracycoin" : rest -> withRom rest $ \rom lim -> case runAccuracyCoin (max lim (600 * 1789773)) rom of
      Left e -> putStrLn e >> exitFailure
      Right (pages, n, _) -> mapM_ putStrLn (resultLines pages) >> putStrLn (show n <> " cycles")
    "--accuracycoin-trace" : k : rest -> do
      let (stop, rest') = case rest of
            "--stop" : pc : "--after" : n : r -> (Just (read ("0x" <> pc), read n), r)
            "--stop" : pc : r -> (Just (read ("0x" <> pc), 0), r)
            r -> (Nothing, r)
      withRom rest' $ \rom lim -> case accuracyCoinTrace (read k) lim stop rom of
        Left e -> putStrLn e >> exitFailure
        Right (t, ram) -> do
          mapM_ putStrLn t
          -- zero page, pages 5 and 6 (where many tests keep results), $7F0-$7FF
          let rows = zip [0 :: Int ..] (chunks ram)
          mapM_ (\(i, row) -> putStrLn (hex4 (i * 16) <> ": " <> unwords (map hex2 row)))
                [ r | r@(i, _) <- rows, i < 8 || (i >= 0x50 && i < 0x70) || i == 0x7F ]
    ["--png", n, out, path] -> do
      rom <- BS.readFile path
      either (\e -> putStrLn e >> exitFailure) (writeFrame out . screenFrame) (runFrames (read n) rom)
    _ -> withRom args $ \rom lim -> case runBlargg lim rom of
      Left e -> putStrLn e >> exitFailure
      Right r -> do
        case brResult r of
          Done s t -> putStr t >> putStrLn ("status " <> show s)
          other -> print other
        putStrLn (show (brCycles r) <> " cycles, " <> show (brResets r) <> " resets")
 where
  hex2 :: (Integral a, Show a) => a -> String
  hex2 v = let x = showHex v "" in replicate (2 - length x) '0' <> x
  hex4 :: (Integral a, Show a) => a -> String
  hex4 v = let x = showHex v "" in replicate (4 - length x) '0' <> x
  chunks xs = if null xs then [] else take 16 xs : chunks (drop 16 xs)
  withRom as k = case as of
    [p] -> BS.readFile p >>= \r -> k r (30 * 1789773)
    [p, n] -> BS.readFile p >>= \r -> k r (read n)
    _ -> putStrLn "usage: nes-run [--trace K | --screen] <rom.nes> [CPU cycles] | --png N out.png <rom.nes>" >> exitFailure
