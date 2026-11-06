-- | Step 3.10: Holy Mapperel (Damian Yerrick) on the reference model: each
-- board's screen says "DETAILED TEST RESULT: wxyz" (digits: work RAM,
-- PRG banking, IRQ, CHR banking; 0 = pass). Our boards always have 8 KiB
-- of PRG RAM only when the header says so (these ROMs' NES 2.0 headers
-- say none), and MMC3's $A001 protection isn't modelled (as most
-- emulators; it would only show with work RAM).
module Nes.SpecMapperel
  ( mapperelTests
  , mapperelRoms
  ) where

import Prelude

import Data.Char (isHexDigit)
import Data.List (find)
import System.FilePath ((</>))
import Test.Tasty
import Test.Tasty.HUnit

import Nes.Spec (OldResult (..), runOldScreenTest)
import Nes.TestRoms

-- | The ROMs tools/fetch-roms.sh extracts, with the result each must show.
mapperelRoms :: [(FilePath, String)]
mapperelRoms =
  [ ("M0_P32K_C8K_V.nes", "0000"), ("M0_P32K_CR8K_V.nes", "0000")
  , ("M1_P128K.nes", "0000"), ("M1_P128K_CR8K.nes", "0000"), ("M1_P128K_C32K.nes", "0000"), ("M1_P128K_C128K.nes", "0000")
  , ("M2_P128K_V.nes", "0000"), ("M2_P128K_CR8K_V.nes", "0000")
  , ("M3_P32K_C32K_H.nes", "0000")
  , ("M4_P128K.nes", "0000"), ("M4_P128K_CR8K.nes", "0000")
    -- MMC3 with 256 KiB of CHR ROM: the CHR block RAM's full size (side track D, Kirby)
  , ("M4_P256K_C256K.nes", "0000")
  , ("M7_P128K.nes", "0000"), ("M7_P128K_CR8K.nes", "0000")
  ]

-- | The result after about 6 s (the screen is drawn after its beeps).
mapperelResult :: FilePath -> IO String
mapperelResult name = do
  rom <- loadRomFile (romRoot </> "holy-mapperel" </> name)
  r <- either assertFailure pure (runOldScreenTest (6 * 1789773) rom)
  -- (the screen reader knows digits and punctuation, not this font's
  -- letters: the result line is the one ending in ": " and four hex digits)
  let result l = case words l of
        [":", d] | length d == 4 && all isHexDigit d -> Just d
        _ -> Nothing
  pure (maybe ("no result line: " <> show (orText r)) id (find (/= Nothing) (map result (orText r)) >>= id))

mapperelTests :: TestTree
mapperelTests = testGroup "Holy Mapperel (step 3.10)"
  [ testCase name (mapperelResult name >>= (@?= want)) | (name, want) <- mapperelRoms ]
