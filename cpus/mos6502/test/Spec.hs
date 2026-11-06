-- | Layer 0: 'Mos6502.Spec' against SingleStepTests (PLAN.md step 3.1),
-- the first 500 cases per opcode in the quick tier, all with FULL=1;
-- every opcode of the 2A03 set (decimal mode off) and the decimal-mode
-- opcodes of the NMOS 6502 set.
module Main (main) where

import Prelude

import Control.Exception (evaluate)
import Control.Monad (unless)
import Data.List (sort)
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath ((</>), takeBaseName, takeExtension)
import Test.Tasty
import Test.Tasty.HUnit

import Mos6502.Interrupts
import Mos6502.SingleStep
import Mos6502.Spec

-- | Relative to the package directory, where cabal runs the tests.
romRoot :: FilePath
romRoot = "../../roms/6502"

main :: IO ()
main = do
  nes <- jsonFiles (romRoot </> "nes6502")
  dec <- jsonFiles (romRoot </> "6502")
  full <- fullTests
  let n = if full then Nothing else Just quickCases
      tier = maybe "all 10 000" (\k -> "first " <> show k) n <> " cases per opcode"
  defaultMain $ testGroup "mos6502-spec"
    [ interruptTests
    , testGroup ("SingleStepTests nes6502 (2A03, no decimal), " <> tier) (map (opcodeTest n singleStep2a03) nes)
    , testGroup ("SingleStepTests 6502 (decimal-mode opcodes), " <> tier) (map (opcodeTest n nmos6502) dec)
    ]

jsonFiles :: FilePath -> IO [FilePath]
jsonFiles d = do
  ok <- doesDirectoryExist d
  unless ok (fail (d <> " missing: run tools/fetch-roms.sh"))
  map (d </>) . sort . filter ((== ".json") . takeExtension) <$> listDirectory d

opcodeTest :: Maybe Int -> Config -> FilePath -> TestTree
opcodeTest n cfg f = testCase (takeBaseName f) $ do
  cases <- loadCasesUpTo n f
  let failures = [ (caseName tc, es) | tc <- cases, let es = runCase cfg tc, not (null es) ]
  nFail <- evaluate (length failures)
  unless (nFail == 0) $
    assertFailure (show nFail <> " of " <> show (length cases) <> " cases fail; first:\n"
                   <> unlines [ name <> ": " <> unlines es | (name, es) <- take 3 failures ])
