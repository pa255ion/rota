-- | Step 3.2: the reference model on nestest and Blargg's CPU ROMs.
module Nes.SpecRoms
  ( cpuRomTests
  ) where

import Prelude

import qualified Data.ByteString.Char8 as BC
import System.FilePath ((</>))
import Test.Tasty
import Test.Tasty.HUnit

import Nes.Hooks
import Nes.Spec
import Nes.TestRoms

-- | CPU cycles: 30 s of NES time, far more than the single ROMs need
-- (the multi-ROMs take about 32 s, so they get 60 s).
limit :: Int
limit = 30 * 1789773

blarggTest :: FilePath -> TestTree
blarggTest p = testCase p $ do
  rom <- loadRomFile (romRoot </> "blargg" </> p)
  let lim = if p `elem` multi then 2 * limit else limit
      multi = ["instr_test-v5/all_instrs.nes", "instr_test-v5/official_only.nes"]
  r <- either assertFailure pure (runBlargg lim rom)
  case brResult r of
    Done 0 _ -> pure ()
    other -> assertFailure (show other <> " after " <> show (brCycles r) <> " cycles")

cpuRomTests :: TestTree
cpuRomTests = testGroup "CPU ROMs (step 3.2)"
  [ testCase "nestest: registers, cycles and PPU position of all 8991 log lines" $ do
      rom <- loadRomFile (romRoot </> "nestest" </> "nestest.nes")
      lg <- either assertFailure pure . parseNestestLog . BC.unpack
              =<< loadRomFile (romRoot </> "nestest" </> "nestest.log")
      runNestest rom lg @?= Right 8991
  , testCase "our pass.s through the protocol" $ do
      rom <- loadRomFile (asmRoot </> "pass.nes")
      brResult <$> either assertFailure pure (runBlargg limit rom) >>= (@?= Done 0 "pass\n\nPassed\n")
  , testCase "our apu_reg_active.s: APU registers only while the CPU is at $4000-$401F (side track D 1)" $ do
      rom <- loadRomFile (asmRoot </> "apu_reg_active.nes")
      r <- either assertFailure pure (runBlargg limit rom)
      case brResult r of
        Done 0 _ -> pure ()
        other -> assertFailure (show other)
  , testCase "our mmc5.s: MMC5's PRG modes and RAM, multiplier, ExRAM, CHR banks, nametables, scanline IRQ (side track D)" $ do
      rom <- loadRomFile (asmRoot </> "mmc5.nes")
      r <- either assertFailure pure (runBlargg limit rom)
      case brResult r of
        Done 0 _ -> pure ()
        other -> assertFailure (show other)
  , testCase "our irq_clear_get.s: a $4015 read clears the frame IRQ flag on the next get (side track D 2)" $ do
      rom <- loadRomFile (asmRoot </> "irq_clear_get.nes")
      r <- either assertFailure pure (runBlargg limit rom)
      case brResult r of
        Done 0 _ -> pure ()
        other -> assertFailure (show other)
  , testCase "our frame_irq_inhibit.s: the frame IRQ flag pulses with the inhibit bit, no IRQ (side track D 4)" $ do
      rom <- loadRomFile (asmRoot </> "frame_irq_inhibit.nes")
      r <- either assertFailure pure (runBlargg limit rom)
      case brResult r of
        Done 0 _ -> pure ()
        other -> assertFailure (show other)
  , testCase "our strobe_get.s: the controllers see $4016 when a get cycle ends (side track D 4)" $ do
      rom <- loadRomFile (asmRoot </> "strobe_get.nes")
      r <- either assertFailure pure (runBlargg limit rom)
      case brResult r of
        Done 0 _ -> pure ()
        other -> assertFailure (show other)
  , testGroup "instr_test-v5" $ map (blarggTest . ("instr_test-v5" </>))
      [ "01-basics.nes", "02-implied.nes", "03-immediate.nes", "04-zero_page.nes"
      , "05-zp_xy.nes", "06-absolute.nes", "07-abs_xy.nes", "08-ind_x.nes", "09-ind_y.nes"
      , "10-branches.nes", "11-stack.nes", "12-jmp_jsr.nes", "13-rts.nes", "14-rti.nes"
      , "15-brk.nes", "16-special.nes", "all_instrs.nes", "official_only.nes" ]
  , testGroup "instr_timing" $ map (blarggTest . ("instr_timing" </>))
      [ "1-instr_timing.nes", "2-branch_timing.nes" ]
  , testGroup "cpu_interrupts_v2" $ map (blarggTest . ("cpu_interrupts_v2" </>))
      [ "1-cli_latency.nes", "2-nmi_and_brk.nes", "3-nmi_and_irq.nes"
      , "4-irq_and_dma.nes", "5-branch_delays_irq.nes" ]
  , testGroup "apu_test" $ map (blarggTest . ("apu_test" </>))
      [ "1-len_ctr.nes", "2-len_table.nes", "3-irq_flag.nes", "4-jitter.nes"
      , "5-len_timing.nes", "6-irq_flag_timing.nes", "7-dmc_basics.nes", "8-dmc_rates.nes" ]
  , dmcDmaTests
  , testGroup "dummy accesses" $ map blarggTest
      [ "cpu_dummy_reads/cpu_dummy_reads.nes", "cpu_dummy_writes/cpu_dummy_writes_oam.nes"
      , "cpu_dummy_writes/cpu_dummy_writes_ppumem.nes" ]
  ]

-- | Blargg's dmc_dma_during_read4 (step 3.9): the DMC's DMA halting the CPU
-- in a read. The results are on the screen: "Passed", or for
-- dma_2007_read only a CRC, one of two (the number of extra $2007 reads
-- depends on the CPU-PPU alignment). double_2007_read (no DMA: two $2007
-- reads on consecutive cycles; side track D 2, the PPU's read state
-- machine): one of the four CRCs its source lists for real consoles.
dmcDmaTests :: TestTree
dmcDmaTests = testGroup "dmc_dma_during_read4 (screen)"
  [ screen "dma_2007_write.nes" (any (== "Passed"))
  , screen "dma_4016_read.nes" (any (== "Passed"))
  , screen "read_write_2007.nes" (any (== "Passed"))
  , screen "dma_2007_read.nes" (any (`elem` ["159A7A8F", "5E3DF9C4"]))
  , screen "double_2007_read.nes" (any (`elem` ["85CFD627", "F018C287", "440EF923", "E52F41A5"]))
  ]
 where
  screen p ok = testCase p $ do
    rom <- loadRomFile (romRoot </> "blargg" </> "dmc_dma_during_read4" </> p)
    r <- either assertFailure pure (runOldScreenTest (3 * 1789773) rom)
    let t = map trim (orText r)
    assertBool (unlines t) (ok t)
  trim = reverse . dropWhile (== ' ') . reverse . dropWhile (== ' ')
