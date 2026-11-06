-- | Layer 0: 'Z80.Spec' against SingleStepTests (PLAN.md step 5.1), the
-- first 100 cases per opcode file in the quick tier, all 1000 with FULL=1;
-- every one of the 1604 files, grouped by prefix; and the interrupt cases
-- SingleStepTests can't show.
module Main (main) where

import Prelude

import Control.Exception (evaluate)
import Control.Monad (unless)
import Data.List (isPrefixOf, sort)
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath ((</>), takeBaseName, takeExtension)
import Test.Tasty
import Test.Tasty.HUnit

import Z80.Interrupts
import Z80.SingleStep

-- | Relative to the package directory, where cabal runs the tests.
romRoot :: FilePath
romRoot = "../../roms/z80/v1"

main :: IO ()
main = do
  files <- jsonFiles romRoot
  full <- fullTests
  let n = if full then Nothing else Just quickCases
      tier = maybe "all 1000" (\k -> "first " <> show k) n <> " cases per file"
      group name p = testGroup name (map (opcodeTest n) (filter (p . takeBaseName) files))
      isPre s f = s `isPrefixOf` f
  defaultMain $ testGroup "z80-spec"
    [ interruptTests
    , testGroup ("SingleStepTests, " <> tier) (singleStep group isPre)
    ]
 where
  singleStep group isPre =
    [ group "unprefixed" (\f -> length f == 2)
    , group "CB" (isPre "cb ")
    , group "ED" (isPre "ed ")
    , group "DD" (\f -> isPre "dd " f && not (isPre "dd cb" f))
    , group "FD" (\f -> isPre "fd " f && not (isPre "fd cb" f))
    , group "DDCB" (isPre "dd cb")
    , group "FDCB" (isPre "fd cb")
    ]

jsonFiles :: FilePath -> IO [FilePath]
jsonFiles d = do
  ok <- doesDirectoryExist d
  unless ok (fail (d <> " missing: run tools/fetch-roms.sh"))
  map (d </>) . sort . filter ((== ".json") . takeExtension) <$> listDirectory d

opcodeTest :: Maybe Int -> FilePath -> TestTree
opcodeTest n f = testCase (takeBaseName f) $ do
  cases <- loadCasesUpTo n f
  let failures = [ (caseName tc, es) | tc <- cases, let es = runCase tc, not (null es) ]
  nFail <- evaluate (length failures)
  unless (nFail == 0) $
    assertFailure (show nFail <> " of " <> show (length cases) <> " cases fail; first:\n"
                   <> unlines [ name <> ": " <> unlines es | (name, es) <- take 2 failures ])
