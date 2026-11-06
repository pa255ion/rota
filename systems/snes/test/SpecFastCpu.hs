-- | Side track Q (PLAN.md Q.6): the SNES's fast 65816 ("Snes.SpecFast.Cpu",
-- a copy of cpus/w65c816's W65c816.Spec) on Tom Harte's SingleStepTests,
-- as w65c816-spec runs them on the reference: the first cases of every
-- file in the quick tier, all 5.12 million in the full tier
-- (EMU_FULL_TESTS=1).
module Main (main) where

import Prelude

import Control.Exception (evaluate)
import Control.Monad (unless)
import Data.List (sort)
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (takeExtension, takeFileName, (</>))
import Test.Tasty
import Test.Tasty.HUnit

import SpecFastCpu.SingleStep

romDir :: FilePath
romDir = "../../roms/65816/v1"

main :: IO ()
main = do
  ok <- doesDirectoryExist romDir
  unless ok (fail (romDir <> " missing: run tools/fetch-roms.sh"))
  files <- map (romDir </>) . sort . filter ((== ".json") . takeExtension) <$> listDirectory romDir
  full <- fullTests
  let n = if full then Nothing else Just quickCases
      tier = maybe "all 10 000" (\k -> "first " <> show k) n <> " cases per file"
  defaultMain $ testGroup ("snes-specfast-cpu: SingleStepTests 65816 on the fast copy, " <> tier)
    [ testCase (takeFileName f) $ do
        cases <- loadCasesUpTo n f
        let bad = [ es | tc <- cases, let es = runCase tc, not (null es) ]
        nBad <- evaluate (length bad)
        unless (not (null cases) && nBad == 0) $
          assertFailure (show nBad <> " of " <> show (length cases) <> " cases fail; first:\n" <> unlines (concat (take 1 bad)))
    | f <- files ]
