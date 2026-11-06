-- | Layer 0: the media parsers and the test hooks (step 4.0), the
-- reference model (steps 4.1 and on), on synthetic inputs, the fetched
-- files and our own regression programs.
module Main (main) where

import Prelude

import System.Environment (lookupEnv)
import Test.Tasty

import A800.SpecAntic (anticTests)
import A800.SpecDisplay (displayTests)
import A800.SpecLoader (loaderTests)
import A800.SpecPokey (pokeyTests)
import A800.SpecMachine (machineTests)
import A800.SpecMedia (mediaTests)
import A800.SpecRestore (restoreTests)

main :: IO ()
main = do
 fullTier <- maybe False (not . null) <$> lookupEnv "EMU_FULL_TESTS"
 defaultMain $ testGroup "a800xl-spec"
  [ mediaTests
  , machineTests fullTier
  , anticTests
  , displayTests
  , pokeyTests
  , loaderTests
  , restoreTests
  ]
