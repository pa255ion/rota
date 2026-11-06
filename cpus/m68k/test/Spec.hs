-- | Layer 0: "M68k.Spec" against Tom Harte's 680x0 SingleStepTests (PLAN.md,
-- "Side track P"): the first cases of every file in the quick tier, all
-- 8065 with FULL=1. Files of instructions the model doesn't run yet are
-- listed as pending (their count says how far it has come); the ones in
-- 'done' must pass whole. M68K_LOAD_ALL=1 also reads every pending file
-- whole (the loader's gate in P.1; about three seconds a file).
module Main (main) where

import Prelude

import Control.Exception (evaluate)
import Control.Monad (unless)
import Data.List (isSuffixOf, sort)
import System.Directory (doesDirectoryExist, listDirectory)
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import Test.Tasty
import Test.Tasty.HUnit

import M68k.SingleStep

-- | Relative to the package directory, where cabal runs the tests.
romRoot :: FilePath
romRoot = "../../roms/m68k/v1"

-- | The files the model runs (P.1: NOP and MOVEQ); each step adds its
-- group's.
done :: [String]
done =
  [ "NOP", "MOVE.q", "MOVE.b", "MOVE.w", "MOVE.l", "MOVEA.w", "MOVEA.l"
  ] <> [ i <> s | i <- ["ADD", "SUB", "CMP", "AND", "OR", "EOR", "NEG", "NEGX", "NOT", "CLR", "TST", "ADDX", "SUBX"], s <- [".b", ".w", ".l"] ]
    <> [ i <> s | i <- ["ADDA", "SUBA", "CMPA"], s <- [".w", ".l"] ]
    <> [ i <> s | i <- ["ASL", "ASR", "LSL", "LSR", "ROL", "ROR", "ROXL", "ROXR"], s <- [".b", ".w", ".l"] ]
    <> ["BTST", "BCHG", "BCLR", "BSET"]
    <> [ "MOVEtoSR", "MOVEtoCCR", "MOVEfromSR", "MOVEtoUSP", "MOVEfromUSP", "ANDItoCCR", "ANDItoSR", "ORItoCCR"
       , "ORItoSR", "EORItoCCR", "EORItoSR", "LEA", "PEA", "LINK", "UNLINK", "EXG", "SWAP", "EXT.w", "EXT.l", "Scc"
       , "TAS", "CHK", "TRAPV" ]
    <> ["Bcc", "BSR", "DBcc", "JMP", "JSR", "RTS", "RTR", "RTE", "RESET", "TRAP"]
    <> ["MULU", "MULS", "DIVU", "DIVS", "ABCD", "SBCD", "NBCD", "MOVEP.w", "MOVEP.l", "MOVEM.w", "MOVEM.l"]

main :: IO ()
main = do
  ok <- doesDirectoryExist romRoot
  unless ok (fail (romRoot <> " missing: run tools/fetch-roms.sh"))
  files <- sort . filter (".json.gz" `isSuffixOf`) <$> listDirectory romRoot
  full <- fullTests
  loadAll <- (== Just "1") <$> lookupEnv "M68K_LOAD_ALL"
  let n = if full then Nothing else Just quickCases
      tier = maybe "all 8065" (\k -> "first " <> show k) n <> " cases per file"
      (now, later) = foldr (\f (a, b) -> if name f `elem` done then (f : a, b) else (a, f : b)) ([], []) files
  defaultMain $ testGroup "m68k-spec" $
    [ testGroup ("SingleStepTests, " <> tier) (map (fileTest n) now)
    , testCase ("pending: " <> show (length later) <> " of " <> show (length files) <> " files") (pure ())
    , testCase "every file is known" (length now + length later @?= 124)
    ]
    <> [ testGroup "pending files read whole (M68K_LOAD_ALL=1)" (map loads later) | loadAll ]

name :: FilePath -> String
name f = take (length f - length (".json.gz" :: String)) f

-- | Cases the test set has wrong, skipped (and counted in the test's name):
-- each checked by hand, with the reason.
knownBad :: [(String, String)]
knownBad =
  -- ASL.b #2,D2 can't change D2's upper 24 bits; the expected final D2
  -- differs in all of them (0xcdfb7fbe -> 0x2e5e4304; 0x417c7e7d ->
  -- 0x6461d390): corrupt in the set (checked 2026-10-18)
  [ ("ASL.b", "e502 [ASL.b Q, D2] 1583")
  , ("ASL.b", "e502 [ASL.b Q, D2] 1761")
  -- the set's only divide by zero (of 16,130 DIVU/DIVS cases) puts the
  -- DIVU's own address in the frame, where Motorola's manual (and MAME)
  -- push the next instruction's; the model keeps the manual's and takes
  -- this case's flags (N, Z, V, C cleared). Divide by zero gets its own
  -- tests (PLAN.md, P.2's last part)
  , ("DIVU", "80ef [DIVU (d16, A7), D0] 5745")
  ]

fileTest :: Maybe Int -> FilePath -> TestTree
fileTest n f = testCase (name f <> skipped) $ do
  cases <- filter (\tc -> (name f, caseName tc) `notElem` knownBad) <$> loadCases n (romRoot </> f)
  let failures = [ (caseName tc, es) | tc <- cases, let es = runCase tc, not (null es) ]
  nFail <- evaluate (length failures)
  unless (nFail == 0) $
    assertFailure (show nFail <> " of " <> show (length cases) <> " cases fail; first:\n"
                   <> unlines [ nm <> ": " <> unlines es | (nm, es) <- take 3 failures ])
 where
  bad = length [ () | (b, _) <- knownBad, b == name f ]
  skipped = if bad == 0 then "" else " (" <> show bad <> " known-bad cases skipped)"

loads :: FilePath -> TestTree
loads f = testCase (name f) $ do
  cases <- loadCases Nothing (romRoot </> f)
  length cases @?= 8065
