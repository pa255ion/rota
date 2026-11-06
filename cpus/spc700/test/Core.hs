-- | Layer 1 (PLAN.md step 7.8): the Clash SPC700 ("Spc700.Core").
-- SingleStepTests on the core, as spc700-spec runs them on the spec (the
-- first 100 cases per file in the quick tier, all with FULL=1;
-- SPC700_ONLY="9e 0f" runs only those files), and the core in lockstep
-- with the spec on random programs ("Spc700.Lockstep").
module Main (main) where

import Prelude

import Control.Exception (evaluate)
import Control.Monad (unless)
import Data.Bits
import Data.List (sort)
import System.Directory (doesDirectoryExist, listDirectory)
import System.Environment (lookupEnv)
import System.FilePath ((</>), takeFileName, takeExtension)
import Test.Tasty
import Test.Tasty.HUnit

import Spc700.Core
import Spc700.CoreStep
import Spc700.Lockstep
import Spc700.SingleStep

romDir :: FilePath
romDir = "../../roms/spc700/v1"

main :: IO ()
main = do
  allFiles <- jsonFiles romDir
  only <- maybe [] words <$> lookupEnv "SPC700_ONLY"
  let files = if null only then allFiles else filter ((`elem` only) . take 2 . takeFileName) allFiles
  full <- fullTests
  let n = if full then Nothing else Just quickCases
      tier = maybe "all 1000" (\k -> "first " <> show k) n <> " cases per file"
  defaultMain $ testGroup "spc700-core"
    [ testCase "cpuInit is the power-on state at FFC0h" $
        cpuInit @?= cpuPowerOn (CpuRegs 0 0 0 0 0xFFC0 0)
    , divTest full
    , lockstepTests full
    , testGroup ("SingleStepTests SPC700 on the core, " <> tier) (map (opcodeTest n) files)
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

-- | DIV's datapath ('divBits', a bit an internal cycle) against the spec's
-- formula (as in "Spc700.Spec"): every YA and X with FULL=1; otherwise
-- every X with small, large and spread YA (a wrong compare in a bit
-- shows only on a few thousand inputs, too few for the random cases).
divTest :: Bool -> TestTree
divTest full = testCase ("DIV = the spec's formula, " <> (if full then "all 2^24 inputs" else "every X, 2,128 YA each")) $ do
  let yas = if full then [0 .. 0xFFFF] else [0 .. 1023] <> [1024, 1085 .. 0xFBFF] <> [0xFC00 .. 0xFFFF]
      bad = [ (ya, x, got, want) | x <- [0 .. 255 :: Int], ya <- yas :: [Int]
            , let w = divBits (fromIntegral ya) (fromIntegral x)
                  got = (fromIntegral w .&. 0xFF, fromIntegral (w `shiftR` 9) .&. 0xFF, testBit w 8)
                  want = specDiv ya x
            , got /= want ]
  nBad <- evaluate (length bad)
  unless (nBad == 0) $ assertFailure (show nBad <> " inputs differ (YA, X, core, spec): " <> show (take 4 bad))
 where
  specDiv :: Int -> Int -> (Int, Int, Bool)
  specDiv ya x =
    let y = ya `shiftR` 8
        (q, r) = if y < x `shiftL` 1 then ya `quotRem` x
                 else let t = ya - (x `shiftL` 9) in (255 - t `quot` (256 - x), x + t `rem` (256 - x))
    in (q .&. 0xFF, r .&. 0xFF, y >= x)
