-- | Step 3.3: the reference model's PPU on Blargg's PPU ROMs, screen
-- goldens, and AccuracyCoin's results.
module Nes.SpecPpu
  ( ppuRomTests
  , accuracyCoinTests
  ) where

import Prelude

import qualified Data.ByteString.Char8 as BC
import System.FilePath ((</>), (<.>))
import Test.Tasty
import Test.Tasty.Golden (goldenVsString)
import Test.Tasty.HUnit

import qualified Data.ByteString.Lazy.Char8 as BL
import Nes.AccuracyCoin
import Nes.Hooks

import Nes.Spec
import Nes.TestRoms

limit :: Int
limit = 30 * 1789773

blarggTest :: FilePath -> TestTree
blarggTest p = testCase p $ do
  rom <- loadRomFile (romRoot </> "blargg" </> p)
  r <- either assertFailure pure (runBlargg (if p == "ppu_vbl_nmi/ppu_vbl_nmi.nes" then 2 * limit else limit) rom)
  case brResult r of
    Done 0 _ -> pure ()
    other -> assertFailure (show other <> " after " <> show (brCycles r) <> " cycles")

-- | The 2005 tests: "PASSED" on screen and 1 at $F8.
oldTest :: FilePath -> TestTree
oldTest p = testCase p $ do
  rom <- loadRomFile (romRoot </> "blargg" </> p)
  r <- either assertFailure pure (runOldScreenTest limit rom)
  (orCode r, any (BC.isInfixOf (BC.pack "PASSED") . BC.pack) (orText r)) @?= (1, True)

ppuRomTests :: TestTree
ppuRomTests = testGroup "PPU ROMs (step 3.3)"
  [ testGroup "ppu_vbl_nmi" $ map (blarggTest . ("ppu_vbl_nmi" </>))
      [ "01-vbl_basics.nes", "02-vbl_set_time.nes", "03-vbl_clear_time.nes", "04-nmi_control.nes"
      , "05-nmi_timing.nes", "06-suppression.nes", "07-nmi_on_timing.nes", "08-nmi_off_timing.nes"
      , "09-even_odd_frames.nes", "10-even_odd_timing.nes", "ppu_vbl_nmi.nes" ]
  , blarggTest "oam_read/oam_read.nes"
  , testGroup "sprite_hit_tests" $ map (oldTest . ("sprite_hit_tests_2005.10.05" </>))
      [ "01.basics.nes", "02.alignment.nes", "03.corners.nes", "04.flip.nes", "05.left_clip.nes"
      , "06.right_edge.nes", "07.screen_bottom.nes", "08.double_height.nes", "09.timing_basics.nes"
      , "10.timing_order.nes", "11.edge_timing.nes" ]
  , testGroup "sprite_overflow_tests" $ map (oldTest . ("sprite_overflow_tests" </>))
      [ "1.Basics.nes", "2.Details.nes", "3.Timing.nes", "4.Obscure.nes", "5.Emulator.nes" ]
  , testGroup "screen goldens (test/golden/spec/, checked by eye, then frozen)"
      [ golden "nestest" 30 (romRoot </> "nestest" </> "nestest.nes")
      , golden "sprite_hit-01.basics" 60 (romRoot </> "blargg" </> "sprite_hit_tests_2005.10.05" </> "01.basics.nes")
      , golden "ppu_vbl_nmi-01" 300 (romRoot </> "blargg" </> "ppu_vbl_nmi" </> "01-vbl_basics.nes")
      , golden "ppu_scene" 10 (asmRoot </> "ppu_scene.nes")
        -- MMC5 (side track D): code run from ExRAM with its colour bars;
        -- the scanline counter's ruler; the CHR bank test with 8x16
        -- sprites (its start: OBJ 0-7, BG 8-B, $2007 from the set written
        -- last, as loopy measured on hardware)
      , golden "mmc5exram" 120 (romRoot </> "mmc5" </> "mmc5exram.nes")
      , golden "mmc5test" 120 (romRoot </> "mmc5" </> "mmc5test.nes")
      , golden "mmc5test_v2" 120 (romRoot </> "mmc5" </> "mmc5test_v2.nes")
        -- ... with Select pressed at frame 30: extended attributes on (the
        -- background's banks and palettes from ExRAM's diagonal pattern);
        -- Up: fill mode
      , goldenPad "mmc5test_v2-exattr" 0x04 120 (romRoot </> "mmc5" </> "mmc5test_v2.nes")
      , goldenPad "mmc5test_v2-fill" 0x10 120 (romRoot </> "mmc5" </> "mmc5test_v2.nes")
      ]
  ]
 where
  golden name n path = goldenPad name 0 n path
  goldenPad name pad n path = screenGolden name ("test" </> "golden" </> "spec" </> name <.> "png") $ do
    rom <- loadRomFile path
    either (ioError . userError) pure (runFramesPress 30 pad n rom)

-- | The spec's AccuracyCoin results as a golden list (any change shows in
-- the diff), and a floor: the score never drops below the recorded one.
accuracyCoinTests :: TestTree
accuracyCoinTests = withResource run (const (pure ())) $ \res -> testGroup "AccuracyCoin (a measure, not a gate)"
  [ goldenVsString "results (test/golden/accuracycoin-spec.txt)" ("test" </> "golden" </> "accuracycoin-spec.txt") $ do
      (pages, _, _) <- res
      pure (BL.pack (unlines (resultLines pages)))
  , testCase ("score at least " <> show recordedScore) $ do
      (pages, _, _) <- res
      let score = length [ () | (_, es) <- pages, (e, b) <- es, counted e, isPass (outcome b) ]
      (score >= recordedScore) @? ("score " <> show score)
  , screenGolden "accuracycoin-results" ("test" </> "golden" </> "spec" </> "accuracycoin-results.png")
      ((\(_, _, s) -> s) <$> res)
  ]
 where
  run = do
    rom <- loadRomFile (romRoot </> "accuracycoin-d103cbd" </> "AccuracyCoin.nes")
    either (ioError . userError) pure (runAccuracyCoin (600 * 1789773) rom)
  isPass (Passed _) = True
  isPass _ = False

-- | The score as last recorded (docs/systems/nes.md, status log; side
-- track D step 5).
recordedScore :: Int
recordedScore = 144
