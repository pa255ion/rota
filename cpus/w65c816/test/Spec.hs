-- | Layer 0: the 65C816 against SingleStepTests (PLAN.md steps 7.0, 7.1).
-- All 512 files are there (every opcode in emulation and native mode);
-- each case's opcode byte at PBR:PC is its file's and its E flag its
-- file's mode (step 7.0), and "W65c816.Spec" runs it: final state and
-- every cycle (step 7.1). The first 100 cases per file in the quick tier,
-- all with FULL=1; W65C816_ONLY="a9.n 00.e" runs only those files.
module Main (main) where

import Prelude

import Control.Exception (evaluate)
import Control.Monad (unless)
import Data.Bits (shiftL, (.|.))
import Data.List (sort)
import Numeric (showHex)
import System.Directory (doesDirectoryExist, listDirectory)
import System.Environment (lookupEnv)
import System.FilePath ((</>), takeFileName, takeExtension)
import Test.Tasty
import Test.Tasty.HUnit

import W65c816.Interrupts
import W65c816.SingleStep
import W65c816.Types

-- | Relative to the package directory, where cabal runs the tests.
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
      expected = sort [ hex2 op <> "." <> m <> ".json" | op <- [0 .. 255 :: Int], m <- ["e", "n"] ]
  defaultMain $ testGroup "w65c816-spec"
    [ testCase "all 512 files (256 opcodes, e and n)" $
        map takeFileName allFiles @?= expected
    , interruptTests
    , testGroup ("SingleStepTests 65816, " <> tier) (map (opcodeTest n) files)
    ]

hex2 :: Int -> String
hex2 v = let s = showHex v "" in replicate (2 - length s) '0' <> s

jsonFiles :: FilePath -> IO [FilePath]
jsonFiles d = do
  ok <- doesDirectoryExist d
  unless ok (fail (d <> " missing: run tools/fetch-roms.sh"))
  map (d </>) . sort . filter ((== ".json") . takeExtension) <$> listDirectory d

-- | "a9.n.json": opcode A9h, native mode.
opcodeTest :: Maybe Int -> FilePath -> TestTree
opcodeTest n f = testCase (takeFileName f) $ do
  let name = takeFileName f
      op = read ("0x" <> take 2 name) :: Int
      emu = take 1 (drop 3 name) == "e"
  cases <- loadCasesUpTo n f
  let bad =
        [ caseName tc <> ": " <> why
        | tc <- cases
        , let r = csRegs (caseInitial tc)
              pc = (fromIntegral (rPBR r) `shiftL` 16) .|. fromIntegral (rPC r)
        , why <- [ "opcode byte not " <> hex2 op | lookup pc (csRam (caseInitial tc)) /= Just (fromIntegral op) ]
              <> [ "E is not the file's mode" | rE r /= emu ]
              <> [ "the first cycle doesn't fetch the opcode" | not (fetchesOpcode pc (caseCycles tc)) ]
              <> take 1 [ unlines es | let es = runCase tc, not (null es) ]
        ]
  nBad <- evaluate (length bad)
  unless (not (null cases) && nBad == 0) $
    assertFailure (show nBad <> " of " <> show (length cases) <> " cases fail; first:\n" <> unlines (take 2 bad))
 where
  fetchesOpcode pc (c : _) = cyAddr c == Just pc && cyVda c && cyVpa c && not (cyWrite c)
  fetchesOpcode _ [] = False
