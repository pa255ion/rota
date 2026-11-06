-- | Step 4.3: the picture. Goldens at 320 × 240 (the normal playfield,
-- lines 8-247; "A800.Picture"), checked by eye once, then frozen.
module A800.SpecDisplay (displayTests) where

import Prelude

import Data.Bits (bit)

import System.FilePath ((</>))
import Test.Tasty
import Test.Tasty.Golden.Advanced (goldenTest)

import A800.Picture
import A800.Spec
import A800.Media (parseCart, parseCas)
import A800.TestRoms
import Platform.Sim.Frame (frameDiff, readFrame, writeFrame)

displayTests :: TestTree
displayTests = testGroup "display (4.3)"
  [ pictureGolden "BASIC's Ready screen" "test/golden/ready.png" $ do
      os <- loadFile osRom
      basic <- loadFile basicRom
      pure (runPicture 300 (stockConfig os basic Nothing))
  , pictureGolden "scene.s: every mode, players, priority, GTIA mode 9 by DLI" "test/golden/scene.png" $ do
      os <- loadFile osRom
      basic <- loadFile basicRom
      l <- loadFile loaderBin
      x <- loadFile (asmRoot </> "scene.xex")
      pure (runPicture 300 (xexConfig os basic l x))
  , pictureGolden "rasterbars.s: colour writes mid-line, on exact colour clocks" "test/golden/rasterbars.png" $ do
      os <- loadFile osRom
      basic <- loadFile basicRom
      l <- loadFile loaderBin
      x <- loadFile (asmRoot </> "rasterbars.xex")
      pure (runPicture 100 (xexConfig os basic l x))
  , pictureGolden "keys.s with A held from frame 90: the background is A's code times two" "test/golden/keys.png" $ do
      os <- loadFile osRom
      basic <- loadFile basicRom
      l <- loadFile loaderBin
      x <- loadFile (asmRoot </> "keys.xex")
      pure (runPictureKeys 90 (bit 0x3F, 0) 100 (xexConfig os basic l x))
  , pictureGolden "xegs_banks.s: an XEGS cartridge's banks (Pass)" "test/golden/xegs_banks.png" $ do
      os <- loadFile osRom
      basic <- loadFile basicRom
      c <- loadFile (asmRoot </> "xegs_banks.car")
      cart <- either fail pure (parseCart "xegs_banks.car" c)
      pure (runPicture 100 (stockConfig os basic (Just cart)))
  , pictureGolden "cassio.s: 20 records, the end-of-file record and the end through SIOV, with cas_data.cas in the recorder (Pass)" "test/golden/cassio.png" $ do
      os <- loadFile osRom
      basic <- loadFile basicRom
      l <- loadFile loaderBin
      x <- loadFile (asmRoot </> "cassio.xex")
      c <- loadFile (asmRoot </> "cas_data.cas")
      cas <- either fail pure (parseCas c)
      pure (runPicture 150 ((xexConfig os basic l x) { cfgCas = Just cas }))
  , pictureGolden "cas_boot.s from tape after 1100 frames (START held, Space; the OS waits for the leader): Pass" "test/golden/cas_boot.png" $ do
      os <- loadFile osRom
      basic <- loadFile basicRom
      c <- loadFile (asmRoot </> "cas_boot.cas")
      cas <- either fail pure (parseCas c)
      pure (runCasPicture 1100 (casConfig os basic cas))
  ]

pictureGolden :: String -> FilePath -> IO Picture -> TestTree
pictureGolden name path run =
  goldenTest name
    (either error id <$> readFrame path)
    (pictureFrame <$> run)
    (\g t -> pure (frameDiff g t))
    (writeFrame path)
