-- | m68k-diag FILE [N]: one SingleStepTests file's failures, grouped by the
-- case's addressing modes (registers named as Dn/An), with the first
-- failure's differences of each group (side track P's working tool).
module Main (main) where

import Prelude

import Data.Char (isDigit)
import Data.List (sortOn)
import qualified Data.Map.Strict as M
import System.Environment (getArgs)

import M68k.SingleStep

main :: IO ()
main = do
  args <- getArgs
  let (file, n, names) = case args of
        [f] -> (f, Nothing, False)
        [f, k] -> (f, Just (read k), False)
        [f, k, "names"] -> (f, Just (read k), True)
        _ -> error "usage: m68k-diag FILE [N [names]]"
  cases <- loadCases n ("../../roms/m68k/v1/" <> file <> ".json.gz")
  let fails = [ (shape (caseName tc), (caseName tc, es)) | tc <- cases, let es = runCase tc, not (null es) ]
      groups = M.fromListWith (\_ old -> old) [ (k, v) | (k, v) <- fails ]
      counts = M.fromListWith (+) [ (k, 1 :: Int) | (k, _) <- fails ]
  putStrLn (file <> ": " <> show (length fails) <> " of " <> show (length cases) <> " fail")
  if names then mapM_ (putStrLn . ("  case: " <>) . fst . snd) fails else pure ()
  mapM_ (\(k, c) -> do
          let (nm, es) = groups M.! k
          putStrLn ("  " <> show c <> "  " <> k <> "    e.g. " <> nm)
          mapM_ (putStrLn . ("      " <>)) es)
        (sortOn (negate . snd) (M.toList counts))
 where
  -- "3a8e [MOVE.w A6, (A5)] 1" -> "A, (A)"-like: the bracket, registers anonymous
  shape nm = norm (takeWhile (/= ']') (drop 1 (dropWhile (/= '[') nm)))
  norm (c : d : r) | c `elem` "AD" && isDigit d = c : 'n' : norm r
  norm (c : r) = c : norm r
  norm [] = []
