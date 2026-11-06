-- | Layer 1 (PLAN.md step 7.7): the Clash 65C816 ("W65c816.Core").
-- SingleStepTests on the core, as w65c816-spec runs them on the spec
-- (the first 100 cases per file in the quick tier, all with FULL=1;
-- W65C816_ONLY="a9.n 00.e" runs only those files), and the core in
-- lockstep with the spec on random programs ("W65c816.Lockstep").
module Main (main) where

import Prelude

import Control.Exception (evaluate)
import Control.Monad (unless)
import Data.List (sort)
import System.Directory (doesDirectoryExist, listDirectory)
import System.Environment (lookupEnv)
import System.FilePath ((</>), takeFileName, takeExtension)
import Test.Tasty
import Test.Tasty.HUnit

import W65c816.Core
import W65c816.CoreStep
import W65c816.Lockstep
import W65c816.SingleStep

romDir :: FilePath
romDir = "../../roms/65816/v1"

main :: IO ()
main = do
  allFiles <- jsonFiles romDir
  only <- maybe [] words <$> lookupEnv "W65C816_ONLY"
  let files = if null only then allFiles else filter ((`elem` only) . take 4 . takeFileName) allFiles
  full <- fullTests
  let n = if full then Nothing else Just quickCases
      tier = maybe "all 10 000" (\k -> "first " <> show k) n <> " cases per file"
  defaultMain $ testGroup "w65c816-core"
    [ testCase "cpuInit is cpuReset of zeroed registers" $
        cpuInit @?= cpuReset coreW65c816 (cpuPowerOn (CpuRegs 0 0 0 0x1FF 0 0 0 0 0 False))
    , lockstepTests full
    , testGroup ("SingleStepTests 65816 on the core, " <> tier) (map (opcodeTest n) files)
    ]

jsonFiles :: FilePath -> IO [FilePath]
jsonFiles d = do
  ok <- doesDirectoryExist d
  unless ok (fail (d <> " missing: run tools/fetch-roms.sh"))
  map (d </>) . sort . filter ((== ".json") . takeExtension) <$> listDirectory d

opcodeTest :: Maybe Int -> FilePath -> TestTree
opcodeTest n f = testCase (takeFileName f) $ do
  cases <- loadCasesUpTo n f
  let bad = [ caseName tc <> ": " <> unlines es | tc <- cases, let es = runCaseCore tc, not (null es) ]
  nBad <- evaluate (length bad)
  unless (not (null cases) && nBad == 0) $
    assertFailure (show nBad <> " of " <> show (length cases) <> " cases fail; first:\n" <> unlines (take 2 bad))
