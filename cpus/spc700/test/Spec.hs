-- | Layer 0: the SPC700 against SingleStepTests (PLAN.md steps 7.0, 7.2).
-- All 256 opcode files are there and decode, each case's opcode byte at PC
-- is its file's and its first cycle reads it (step 7.0), and
-- "Spc700.Spec" runs it: final state and every cycle (step 7.2). The
-- first 100 cases per file in the quick tier, all with FULL=1.
module Main (main) where

import Prelude

import Control.Exception (evaluate)
import Control.Monad (unless)
import Data.List (sort)
import Numeric (showHex)
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath ((</>), takeFileName, takeExtension)
import Test.Tasty
import Test.Tasty.HUnit

import Spc700.SingleStep
import Spc700.Types

-- | Relative to the package directory, where cabal runs the tests.
romDir :: FilePath
romDir = "../../roms/spc700/v1"

main :: IO ()
main = do
  files <- jsonFiles romDir
  full <- fullTests
  let n = if full then Nothing else Just quickCases
      tier = maybe "all 1000" (\k -> "first " <> show k) n <> " cases per file"
  defaultMain $ testGroup "spc700-spec"
    [ testCase "all 256 opcode files" $
        map takeFileName files @?= [ hex2 op <> ".json" | op <- [0 .. 255 :: Int] ]
    , testGroup ("SingleStepTests SPC700, " <> tier) (map (decodeTest n) files)
    ]

hex2 :: Int -> String
hex2 v = let s = showHex v "" in replicate (2 - length s) '0' <> s

jsonFiles :: FilePath -> IO [FilePath]
jsonFiles d = do
  ok <- doesDirectoryExist d
  unless ok (fail (d <> " missing: run tools/fetch-roms.sh"))
  map (d </>) . sort . filter ((== ".json") . takeExtension) <$> listDirectory d

decodeTest :: Maybe Int -> FilePath -> TestTree
decodeTest n f = testCase (takeFileName f) $ do
  let op = read ("0x" <> take 2 (takeFileName f)) :: Int
  cases <- loadCasesUpTo n f
  let bad =
        [ caseName tc <> ": " <> why
        | tc <- cases
        , let pc = rPC (csRegs (caseInitial tc))
        , why <- [ "opcode byte not " <> hex2 op | lookup pc (csRam (caseInitial tc)) /= Just (fromIntegral op) ]
              <> [ "the first cycle doesn't read the opcode"
                 | take 1 (caseCycles tc) /= [Cycle (Just pc) (Just (fromIntegral op)) Read] ]
              <> take 1 [ unlines es | let es = runCase tc, not (null es) ]
        ]
  nBad <- evaluate (length bad)
  unless (not (null cases) && nBad == 0) $
    assertFailure (show nBad <> " of " <> show (length cases) <> " cases fail; first:\n" <> unlines (take 2 bad))
