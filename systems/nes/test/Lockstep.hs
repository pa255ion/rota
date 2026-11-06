-- | Layer 2 (PLAN.md step 3.5): the Clash PPU in lockstep with the
-- reference model's PPU, dot by dot, on PPU test ROMs, our scene ROM and
-- random programs.
module Main (main) where

import Prelude

import Control.Monad (forM_, when)
import Data.Bits (shiftR, xor, (.&.))
import Data.List (sort)
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (takeExtension)
import Nes.Cartridge (parseRom, romHeader)
import Nes.Mapper (cartCfgFromHeader)
import qualified Data.ByteString as BS
import Data.Word
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import System.FilePath ((</>))
import Test.Tasty
import Test.Tasty.Hedgehog (testProperty)
import Test.Tasty.HUnit

import Nes.Ppu
import Nes.AccuracyCoin (Entry (..))
import Nes.CoreHarness
import Nes.LockState (stateTests)
import Nes.Spec (runAccuracyCoin, runFramesPress)
import Nes.Screen (screenPixels)
import qualified Data.Vector.Unboxed as U
import System.Environment (lookupEnv)
import Nes.Hooks (Blargg (..))
import Nes.PpuHarness
import Nes.SpecMapperel (mapperelRoms)
import Nes.TestRoms

main :: IO ()
main = do
  allRoms <- (== Just "all") <$> lookupEnv "NES_CORE_ROMS"
  fullT <- (== Just "1") <$> lookupEnv "EMU_FULL_TESTS"
  defaultMain (tests (if allRoms then TAll else if fullT then TFull else TQuick))

-- | Test tiers (docs/TESTING.md, "Tiers"). Quick (the default, make
-- test and make ci): fewer frames and steps, a subset of ROMs. Full
-- (EMU_FULL_TESTS=1, make test FULL=1): the sets of steps 3.5 and 3.6.
-- All (NES_CORE_ROMS=all): also every test ROM to the end on the core,
-- and AccuracyCoin (about 20 min of CPU each for the long ones).
data Tier = TQuick | TFull | TAll
  deriving stock (Eq)

tests :: Tier -> TestTree
tests tier = testGroup "nes-lockstep"
  [ testGroup "PPU, dot by dot (step 3.5)"
      [ romTest "ppu_scene.s" 12 (asmRoot </> "ppu_scene.nes")
      , romTest "nestest (menu)" 30 (romRoot </> "nestest" </> "nestest.nes")
      , testGroup "sprite_hit_tests" [ romTest t 40 (romRoot </> "blargg" </> "sprite_hit_tests_2005.10.05" </> t)
                                     | t <- [ "01.basics.nes", "02.alignment.nes", "03.corners.nes", "04.flip.nes"
                                            , "05.left_clip.nes", "06.right_edge.nes", "07.screen_bottom.nes"
                                            , "08.double_height.nes", "09.timing_basics.nes", "10.timing_order.nes"
                                            , "11.edge_timing.nes" ] ]
      , testGroup "sprite_overflow_tests" [ romTest t 40 (romRoot </> "blargg" </> "sprite_overflow_tests" </> t)
                                          | t <- ["1.Basics.nes", "2.Details.nes", "3.Timing.nes", "4.Obscure.nes", "5.Emulator.nes"] ]
      , testGroup "ppu_vbl_nmi" [ romTest t 60 (romRoot </> "blargg" </> "ppu_vbl_nmi" </> t)
                                | t <- [ "01-vbl_basics.nes", "02-vbl_set_time.nes", "04-nmi_control.nes"
                                       , "06-suppression.nes", "09-even_odd_frames.nes", "10-even_odd_timing.nes" ] ]
      , romTest "oam_read" 30 (romRoot </> "blargg" </> "oam_read" </> "oam_read.nes")
      , romTest "AccuracyCoin (menu)" 40 (romRoot </> "accuracycoin-d103cbd" </> "AccuracyCoin.nes")
      , randomPrograms (if tier == TQuick then 15 else 60)
      ]
  , mutants
  , coreTests tier
  , stateTests (tier /= TQuick)
  ]
 where
  -- quick tier: at most 12 frames per ROM
  romTest name frames = romTest0 name (if tier == TQuick then min 12 frames else frames)

-- | Step 3.6: the whole core against the spec, step by step.
coreTests :: Tier -> TestTree
coreTests tier = testGroup "full core (step 3.6)" $
  [ coreLockstep "nestest (menu)" 20000 (romRoot </> "nestest" </> "nestest.nes")
  , coreLockstep "ppu_scene.s" 50000 (asmRoot </> "ppu_scene.nes")
  , coreLockstep "joypad.s" 2000 (asmRoot </> "joypad.nes")
  , coreLockstep "apu_mix.s (every channel, DMC DMA)" 300000 (asmRoot </> "apu_mix.nes")
  , coreLockstep "tone.s" 20000 (asmRoot </> "tone.nes")
  , coreLockstep "mmc3_bg1000.s (MMC3 counter, background at $1000)" 60000 (asmRoot </> "mmc3_bg1000.nes")
  , coreLockstep "apu_reg_active.s (APU registers and DMA, side track D 1)" 80000 (asmRoot </> "apu_reg_active.nes")
  , coreLockstep "irq_clear_get.s ($4015 clears the frame IRQ flag on a get, side track D 2)" 40000 (asmRoot </> "irq_clear_get.nes")
  , coreLockstep "dmc_abort.s (DMC DMA aborts, side track D 3)" 450000 (asmRoot </> "dmc_abort.nes")
  , coreLockstep "frame_irq_inhibit.s (frame IRQ flag with the inhibit bit, side track D 4)" 220000 (asmRoot </> "frame_irq_inhibit.nes")
  , coreLockstep "strobe_get.s (the strobe seen when a get cycle ends, side track D 4)" 3000 (asmRoot </> "strobe_get.nes")
  , coreLockstep "mmc5.s (MMC5: PRG, RAM, ExRAM, CHR, nametables, scanline IRQ; side track D)" 60000 (asmRoot </> "mmc5.nes")
  , testGroup ("MMC5 test ROMs (side track D), " <> show steps <> " steps each")
      [ coreLockstep t steps (romRoot </> "mmc5" </> t) | t <- ["mmc5exram.nes", "mmc5test.nes", "mmc5test_v2.nes"] ]
  , testGroup "MMC5's pictures: the core's frame = the spec's (side track D; fill, ExRAM nametables, extended attributes)"
      $ if tier == TQuick then [] else [ coreFrameTest "mmc5exram.nes" 0 60, coreFrameTest "mmc5test.nes" 0 60
      , coreFrameTest "mmc5test_v2.nes" 0 60
        -- Select pressed at frame 30: extended attributes on; Up: fill mode
      , coreFrameTest "mmc5test_v2.nes" 0x04 60, coreFrameTest "mmc5test_v2.nes" 0x10 60 ]
  , testGroup ("lockstep on test ROMs, " <> show steps <> " steps each")
      [ coreLockstep t steps (romRoot </> "blargg" </> t) | t <- if tier == TQuick then quickLockstep else lockstepRoms ]
  , coreLockstep "AccuracyCoin (menu)" steps (romRoot </> "accuracycoin-d103cbd" </> "AccuracyCoin.nes")
  , localGames steps
  , testGroup ("Holy Mapperel (step 3.10), " <> show steps <> " steps each")
      [ coreLockstep t steps (romRoot </> "holy-mapperel" </> t)
      | t <- if tier == TQuick then ["M4_P128K_CR8K.nes", "M1_P128K_C128K.nes", "M4_P256K_C256K.nes"] else map fst mapperelRoms ]
  , testGroup "test ROMs to the end on the core alone (they pass on the spec)"
      ( [ coreRom t | t <- if full then completionRoms else quickRoms ]
        <> [ coreOldRom t | t <- if full then oldRoms else take 2 oldRoms ] )
  ]
  <> [ accuracyCoinCore | full ]
 where
  full = tier == TAll
  steps = if tier == TQuick then 100000 else 200000 :: Int

-- | Quick tier: one ROM per area (CPU timing, interrupts with DMA, dummy
-- writes, APU IRQ, VBlank races, odd frames, sprites).
quickLockstep :: [FilePath]
quickLockstep =
  [ "instr_timing/1-instr_timing.nes", "cpu_interrupts_v2/4-irq_and_dma.nes"
  , "cpu_dummy_writes/cpu_dummy_writes_oam.nes", "apu_test/6-irq_flag_timing.nes"
  , "ppu_vbl_nmi/02-vbl_set_time.nes", "ppu_vbl_nmi/10-even_odd_timing.nes"
  , "sprite_hit_tests_2005.10.05/09.timing_basics.nes", "apu_test/7-dmc_basics.nes"
  , "dmc_dma_during_read4/dma_4016_read.nes", "mmc3_test_2/4-scanline_timing.nes"
  , "dmc_dma_during_read4/double_2007_read.nes" ]

quickRoms :: [FilePath]
quickRoms =
  [ "instr_test-v5/14-rti.nes", "cpu_interrupts_v2/1-cli_latency.nes", "apu_test/2-len_table.nes"
  , "ppu_vbl_nmi/04-nmi_control.nes" ]

-- | The core's AccuracyCoin results equal the spec's, test by test.
accuracyCoinCore :: TestTree
accuracyCoinCore = testCase "AccuracyCoin: the core's results equal the spec's" $ do
  rom <- loadRomFile (romRoot </> "accuracycoin-d103cbd" </> "AccuracyCoin.nes")
  (spec, _, _) <- either assertFailure pure (runAccuracyCoin (600 * 1789773) rom)
  (core, _) <- either assertFailure pure (coreAccuracyCoin (600 * 1789773) rom)
  let flat r = [ (p, enName e, b) | (p, es) <- r, (e, b) <- es ]
      diffs = [ (s, c) | (s, c) <- zip (flat spec) (flat core), s /= c ]
  diffs @?= []

-- | Every Blargg ROM of steps 3.2 and 3.3.
completionRoms :: [FilePath]
completionRoms =
  [ "instr_test-v5" </> t | t <- [ "01-basics.nes", "02-implied.nes", "03-immediate.nes", "04-zero_page.nes"
                                 , "05-zp_xy.nes", "06-absolute.nes", "07-abs_xy.nes", "08-ind_x.nes", "09-ind_y.nes"
                                 , "10-branches.nes", "11-stack.nes", "12-jmp_jsr.nes", "13-rts.nes", "14-rti.nes"
                                 , "15-brk.nes", "16-special.nes" ] ]
  <> [ "instr_timing" </> t | t <- ["1-instr_timing.nes", "2-branch_timing.nes"] ]
  <> [ "cpu_interrupts_v2" </> t | t <- ["1-cli_latency.nes", "2-nmi_and_brk.nes", "3-nmi_and_irq.nes", "4-irq_and_dma.nes", "5-branch_delays_irq.nes"] ]
  <> [ "cpu_dummy_reads/cpu_dummy_reads.nes", "cpu_dummy_writes/cpu_dummy_writes_oam.nes", "cpu_dummy_writes/cpu_dummy_writes_ppumem.nes" ]
  <> [ "apu_test" </> t | t <- ["1-len_ctr.nes", "2-len_table.nes", "3-irq_flag.nes", "4-jitter.nes", "5-len_timing.nes", "6-irq_flag_timing.nes"
                              , "7-dmc_basics.nes", "8-dmc_rates.nes"] ]
  <> [ "dmc_dma_during_read4" </> t | t <- ["dma_2007_write.nes", "read_write_2007.nes"] ]
  <> [ "mmc3_test_2" </> t | t <- ["1-clocking.nes", "2-details.nes", "3-A12_clocking.nes", "4-scanline_timing.nes", "5-MMC3.nes"] ]
  <> [ "ppu_vbl_nmi" </> t | t <- [ "01-vbl_basics.nes", "02-vbl_set_time.nes", "03-vbl_clear_time.nes", "04-nmi_control.nes"
                               , "05-nmi_timing.nes", "06-suppression.nes", "07-nmi_on_timing.nes", "08-nmi_off_timing.nes"
                               , "09-even_odd_frames.nes", "10-even_odd_timing.nes" ] ]
  <> [ "oam_read/oam_read.nes" ]

oldRoms :: [FilePath]
oldRoms =
  [ "sprite_hit_tests_2005.10.05" </> t | t <- [ "01.basics.nes", "02.alignment.nes", "03.corners.nes", "04.flip.nes"
                                              , "05.left_clip.nes", "06.right_edge.nes", "07.screen_bottom.nes"
                                              , "08.double_height.nes", "09.timing_basics.nes", "10.timing_order.nes"
                                              , "11.edge_timing.nes" ] ]
  <> [ "sprite_overflow_tests" </> t | t <- ["1.Basics.nes", "2.Details.nes", "3.Timing.nes", "4.Obscure.nes", "5.Emulator.nes"] ]

coreRom :: FilePath -> TestTree
coreRom t = testCase t $ do
  rom <- loadRomFile (romRoot </> "blargg" </> t)
  case coreBlargg (30 * 1789773) rom of
    Left e -> assertFailure e
    Right (Done 0 _, _) -> pure ()
    Right (other, n) -> assertFailure (show other <> " after " <> show n <> " cycles")

coreOldRom :: FilePath -> TestTree
coreOldRom t = testCase t $ do
  rom <- loadRomFile (romRoot </> "blargg" </> t)
  coreOld (30 * 1789773) rom @?= Right 1

lockstepRoms :: [FilePath]
lockstepRoms =
  [ "instr_test-v5" </> t | t <- ["01-basics.nes", "03-immediate.nes", "07-abs_xy.nes", "11-stack.nes", "15-brk.nes", "16-special.nes"] ]
  <> [ "instr_timing" </> t | t <- ["1-instr_timing.nes", "2-branch_timing.nes"] ]
  <> [ "cpu_interrupts_v2" </> t | t <- ["1-cli_latency.nes", "2-nmi_and_brk.nes", "3-nmi_and_irq.nes", "4-irq_and_dma.nes", "5-branch_delays_irq.nes"] ]
  <> [ "cpu_dummy_reads/cpu_dummy_reads.nes", "cpu_dummy_writes/cpu_dummy_writes_oam.nes", "cpu_dummy_writes/cpu_dummy_writes_ppumem.nes" ]
  <> [ "apu_test" </> t | t <- ["1-len_ctr.nes", "3-irq_flag.nes", "6-irq_flag_timing.nes", "7-dmc_basics.nes", "8-dmc_rates.nes"] ]
  <> [ "dmc_dma_during_read4" </> t | t <- ["dma_2007_read.nes", "dma_2007_write.nes", "dma_4016_read.nes", "read_write_2007.nes", "double_2007_read.nes"] ]
  <> [ "mmc3_test_2" </> t | t <- ["1-clocking.nes", "2-details.nes", "3-A12_clocking.nes", "4-scanline_timing.nes", "5-MMC3.nes"] ]
  <> [ "ppu_vbl_nmi" </> t | t <- ["02-vbl_set_time.nes", "05-nmi_timing.nes", "08-nmi_off_timing.nes", "10-even_odd_timing.nes"] ]
  <> [ "sprite_hit_tests_2005.10.05/09.timing_basics.nes", "sprite_overflow_tests/3.Timing.nes", "oam_read/oam_read.nes" ]

-- | The user's own cartridges in roms/nes/games/local/ (never committed;
-- none there: nothing to do), each the core against the spec (side track
-- D: Kirby's Adventure, MMC3 with 512 KiB PRG + 256 KiB CHR). Cartridges
-- the core doesn't support ('cartCfgFromHeader') are skipped.
localGames :: Int -> TestTree
localGames steps = testCase ("the user's cartridges (roms/nes/games/local), " <> show steps <> " steps each") $ do
  let dir = romRoot </> "games" </> "local"
  here <- doesDirectoryExist dir
  files <- if here then sort . filter ((== ".nes") . takeExtension) <$> listDirectory dir else pure []
  forM_ files $ \f -> do
    rom <- BS.readFile (dir </> f)
    let supported = either (const False) (maybe False (const True) . cartCfgFromHeader . romHeader) (parseRom rom)
    when supported $ case lockstepCore steps rom of
      Left e -> assertFailure (f <> ": " <> e)
      Right Nothing -> pure ()
      Right (Just (i, spec, core)) -> assertFailure (f <> ", step " <> show i <> ":\nspec " <> show spec <> "\ncore " <> show core)

-- | The core's frame after @n@ frames with these buttons held equals the
-- spec's, pixel for pixel (the step lockstep doesn't compare pictures).
coreFrameTest :: FilePath -> Word8 -> Int -> TestTree
coreFrameTest name pad n = testCase (name <> ", buttons " <> show pad <> " from frame 30, frame " <> show n) $ do
  rom <- loadRomFile (romRoot </> "mmc5" </> name)
  spec <- either assertFailure (pure . screenPixels) (runFramesPress 30 pad n rom)
  core <- either assertFailure pure (coreFramesPress 30 pad n rom)
  let diffs = [ i | i <- [0 .. U.length spec - 1], spec U.! i /= core U.! i ]
  case diffs of
    [] -> pure ()
    i : _ -> assertFailure (show (length diffs) <> " pixels differ, first at " <> show (i `mod` 256, i `div` 256)
                            <> ": spec " <> show (spec U.! i) <> ", core " <> show (core U.! i))

coreLockstep :: String -> Int -> FilePath -> TestTree
coreLockstep name steps path = testCase (name <> ", " <> show steps <> " steps") $ do
  rom <- loadRomFile path
  case lockstepCore steps rom of
    Left e -> assertFailure e
    Right Nothing -> pure ()
    Right (Just (i, spec, core)) -> assertFailure ("step " <> show i <> ":\nspec " <> show spec <> "\ncore " <> show core)

-- | Deliberately broken PPUs must be caught: fine X scroll ignored,
-- a wrong OAM byte read, the VBlank flag never suppressed.
mutants :: TestTree
mutants = testGroup "deliberately broken PPUs are caught"
  [ testCase "fine X ignored (ppu_scene.s scrolls by 3)" $ broken 12 (asmRoot </> "ppu_scene.nes")
      (\p t i -> ppuTick p { pX = 0 } t i)
  , testCase "OAM reads off by one bit (sprite_hit 01)" $
      broken 20 (romRoot </> "blargg" </> "sprite_hit_tests_2005.10.05" </> "01.basics.nes")
      (\p t i -> ppuTick p t i { piOam = piOam i `xor` 1 })
  , testCase "VBlank never suppressed (ppu_vbl_nmi 06)" $
      broken 60 (romRoot </> "blargg" </> "ppu_vbl_nmi" </> "06-suppression.nes")
      (\p t i -> ppuTick p { pSuppress = False } t i)
  ]
 where
  broken frames path fn = do
    rom <- loadRomFile path
    case lockstepRomWith fn frames rom of
      Left _ -> pure ()
      Right _ -> assertFailure "not caught"

romTest0 :: String -> Int -> FilePath -> TestTree
romTest0 name frames path = testCase (name <> ", " <> show frames <> " frames") $ do
  rom <- loadRomFile path
  case lockstepRom frames rom of
    Left e -> assertFailure e
    Right _ -> pure ()

-- | A random NROM cartridge: random PRG (JAMs made NOPs, vectors into PRG)
-- and random CHR; random code pokes the PPU registers often (an eighth of
-- the address space is $2000-$3FFF).
randomCart :: Word64 -> BS.ByteString
randomCart seed = BS.concat [header, BS.pack prg', BS.pack chr]
 where
  header = BS.pack ([0x4E, 0x45, 0x53, 0x1A, 2, 1, fromIntegral (seed .&. 1), 0] <> replicate 8 0)
  bytes = randomBytes seed (0x8000 + 0x2000)
  prg = take 0x8000 bytes
  chr = drop 0x8000 bytes
  jam b = b `elem` [0x02, 0x12, 0x22, 0x32, 0x42, 0x52, 0x62, 0x72, 0x92, 0xB2, 0xD2, 0xF2]
  prg' = [ if i >= 0x7FFA then (if even i then 0x00 else 0x80) else if jam b then 0xEA else b | (i, b) <- zip [0 :: Int ..] prg ]

randomBytes :: Word64 -> Int -> [Word8]
randomBytes seed n = take n (go seed)
 where
  go s = let s' = s + 0x9E3779B97F4A7C15
             z1 = (s' `xor` (s' `shiftR` 30)) * 0xBF58476D1CE4E5B9
             z2 = (z1 `xor` (z1 `shiftR` 27)) * 0x94D049BB133111EB
             r = z2 `xor` (z2 `shiftR` 31)
         in [fromIntegral (r `shiftR` (8 * i)) | i <- [0 .. 7]] <> go s'

randomPrograms :: H.TestLimit -> TestTree
randomPrograms k = testProperty "random programs: 3 frames each" $
  H.withTests k $ H.property $ do
    seed <- H.forAll (Gen.word64 Range.constantBounded)
    either Just (const Nothing) (lockstepRom 3 (randomCart seed)) H.=== Nothing
