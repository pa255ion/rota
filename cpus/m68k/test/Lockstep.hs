-- | Layer 0 for the Clash core (PLAN.md, "Side track P", P.4): "M68k.Core"
-- over Tom Harte's 680x0 SingleStepTests, its requests made through the
-- model's bus ("M68k.CoreRun"), compared exactly as the model is (the
-- transactions in order, the cycles, the registers, the queue, the RAM).
-- The first cases of every file in the quick tier, all 8065 with FULL=1.
-- The files of instructions the core doesn't run yet are counted as
-- pending; the ones in 'done' must pass whole.
module Main (main) where

import Prelude

import Control.Exception (evaluate)
import Control.Monad (unless)
import Data.List (isSuffixOf, sort)
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath ((</>))
import Test.Tasty
import Test.Tasty.HUnit

import M68k.CoreRun
import M68k.Fuzz
import M68k.SingleStep

-- | Relative to the package directory, where cabal runs the tests.
romRoot :: FilePath
romRoot = "../../roms/m68k/v1"

-- | The files the core runs; each stage of P.4 adds its group's.
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

-- | The model's known-bad cases (test/Spec.hs: each checked by hand).
knownBad :: [(String, String)]
knownBad =
  [ ("ASL.b", "e502 [ASL.b Q, D2] 1583")
  , ("ASL.b", "e502 [ASL.b Q, D2] 1761")
  , ("DIVU", "80ef [DIVU (d16, A7), D0] 5745")
  ]

main :: IO ()
main = do
  ok <- doesDirectoryExist romRoot
  unless ok (fail (romRoot <> " missing: run tools/fetch-roms.sh"))
  files <- sort . filter (".json.gz" `isSuffixOf`) <$> listDirectory romRoot
  full <- fullTests
  let n = if full then Nothing else Just quickCases
      tier = maybe "all 8065" (\k -> "first " <> show k) n <> " cases per file"
      (now, later) = foldr (\f (a, b) -> if name f `elem` done then (f : a, b) else (a, f : b)) ([], []) files
  defaultMain $ testGroup "m68k-lockstep"
    [ testGroup ("the core on SingleStepTests, " <> tier) (map (fileTest n) now)
    , testCase ("pending: " <> show (length later) <> " of " <> show (length files) <> " files") (pure ())
    , testCase "every file in done is there" (length now @?= length done)
    , fuzzTests full
    ]

name :: FilePath -> String
name f = take (length f - length (".json.gz" :: String)) f

fileTest :: Maybe Int -> FilePath -> TestTree
fileTest n f = testCase (name f) $ do
  cases <- filter (\tc -> (name f, caseName tc) `notElem` knownBad) <$> loadCases n (romRoot </> f)
  let failures = [ (caseName tc, es) | tc <- cases, let es = runCaseWith coreRunner tc, not (null es) ]
  nFail <- evaluate (length failures)
  unless (nFail == 0) $
    assertFailure (show nFail <> " of " <> show (length cases) <> " cases fail; first:\n"
                   <> unlines [ nm <> ": " <> unlines es | (nm, es) <- take 3 failures ])
