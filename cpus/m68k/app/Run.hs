{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
-- | m68k-run: a 68000 program on the reference model (side track P, P.3;
-- for the user's benchmark project). The image is loaded at --at (default
-- 0); without --pc the CPU starts from the reset vectors in it (SSP at 0,
-- PC at 4), with --pc ADDR [--sp ADDR] there. It runs until STOP, a fault,
-- or the limit (--cycles N, default 10^9; or --instructions N), then
-- prints the counts and the registers; --trace prints every instruction
-- and bus cycle on the way.
--
-- > m68k-run prog.bin --at 0x1000 --pc 0x1000 --sp 0x8000
module Main (main) where

import Prelude

import qualified Data.ByteString as B
import Data.List (intercalate)
import Numeric (readHex, showHex)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import Data.Word
import M68k.Run
import M68k.Spec

data Opts = Opts
  { oImage :: Maybe FilePath
  , oAt    :: Word32
  , oPc    :: Maybe Word32
  , oSp    :: Word32
  , oLimit :: Limit
  , oTrace :: Bool
  }

usage :: IO a
usage = do
  hPutStrLn stderr "usage: m68k-run IMAGE [--at ADDR] [--pc ADDR [--sp ADDR]] [--cycles N | --instructions N] [--trace]"
  exitFailure

num :: String -> Maybe Integer
num ('0' : x : h) | x `elem` "xX", [(v, "")] <- readHex h = Just v
num s | [(v, "")] <- reads s = Just v
num _ = Nothing

parse :: [String] -> Opts -> Maybe Opts
parse [] o = Just o
parse ("--at" : v : r) o = num v >>= \n -> parse r o { oAt = fromIntegral n }
parse ("--pc" : v : r) o = num v >>= \n -> parse r o { oPc = Just (fromIntegral n) }
parse ("--sp" : v : r) o = num v >>= \n -> parse r o { oSp = fromIntegral n }
parse ("--cycles" : v : r) o = num v >>= \n -> parse r o { oLimit = Cycles (fromIntegral n) }
parse ("--instructions" : v : r) o = num v >>= \n -> parse r o { oLimit = Instructions (fromIntegral n) }
parse ("--trace" : r) o = parse r o { oTrace = True }
parse (f : r) o | take 2 f /= "--", Nothing <- oImage o = parse r o { oImage = Just f }
parse _ _ = Nothing

hex :: (Integral a, Show a) => a -> String
hex v = "0x" <> showHex v ""

main :: IO ()
main = do
  args <- getArgs
  o <- maybe usage pure (parse args (Opts Nothing 0 Nothing 0x8000 (Cycles 1_000_000_000) False))
  file <- maybe usage pure (oImage o)
  img <- B.readFile file
  m <- newMachine
  loadImage m (oAt o) img
  case oPc o of
    Just pc -> startAt m pc (oSp o)
    Nothing -> resetCpu m
  if oTrace o then setTrace m (Just (putStrLn . showEvent)) else pure ()
  s <- run m (oLimit o)
  c <- readCpu m
  putStrLn ("cycles " <> show (statCycles s))
  putStrLn ("instructions " <> show (statInstructions s))
  putStrLn ("stop " <> show (statStop s))
  putStrLn (intercalate " " [ "d" <> show i <> "=" <> hex (dReg c i) | i <- [0 .. 7] ])
  putStrLn (intercalate " " [ "a" <> show i <> "=" <> hex (aReg c i) | i <- [0 .. 7] ])
  putStrLn ("pc=" <> hex (cPc c) <> " sr=" <> hex (cSr c) <> " usp=" <> hex (cUsp c) <> " ssp=" <> hex (cSsp c))

showEvent :: Event -> String
showEvent = \case
  Instruction pc op -> hex pc <> ": " <> hex op
  Read fc sz a v -> "  r " <> size sz <> " fc" <> show fc <> " " <> hex a <> " " <> hex v
  Write fc sz a v -> "  w " <> size sz <> " fc" <> show fc <> " " <> hex a <> " " <> hex v
  Tas fc a v -> "  t .b fc" <> show fc <> " " <> hex a <> " " <> hex v
  Idle n -> "  n " <> show n
  Iack l v -> "  iack level " <> show l <> maybe " autovector" ((" vector " <>) . show) v
 where
  size Byte = ".b"
  size Word = ".w"
