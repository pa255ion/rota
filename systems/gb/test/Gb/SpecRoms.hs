-- | Layer 0: the spec on test ROMs. Step 2.1: Blargg's cpu_instrs and
-- instr_timing over serial, the Mooneye acceptance tests, our own
-- regression ROMs (build/gb/asm). Step 2.2: dmg-acid2 against its
-- reference image, Blargg's mem_timing, and frozen screen goldens
-- (test/golden/spec/, checked by eye once).
module Gb.SpecRoms (romTests) where

import Prelude

import qualified Data.ByteString as BS
import qualified Data.Vector.Unboxed.Mutable as MU

import System.FilePath ((</>))
import Test.Tasty
import Test.Tasty.HUnit

import Gb.Hooks
import Gb.Screen (Screen, frameScreen, rgbFrame, screenFrame)
import Gb.Spec
import Gb.Spec.Machine (Machine (..), getSt, iPC)
import Control.Monad.ST (runST)
import Data.Word (Word8)
import Gb.TestRoms
import Platform.Sim.Frame (frameDiff, readFrame, writeFrame)
import Test.Tasty.Golden.Advanced (goldenTest)

-- | Run a ROM and check the suite's verdict.
romPasses :: Suite -> Int -> FilePath -> Assertion
romPasses = romPassesOn Dmg

romPassesOn :: Model -> Suite -> Int -> FilePath -> Assertion
romPassesOn model suite limit path = do
  rom <- loadRomFile path
  r <- either assertFailure pure (runRomModel model suite limit rom)
  let g = resRegs r
      verdict = case (suite, resStop r) of
        (Mooneye, StopBreakpoint) -> mooneyeVerdict (MooneyeRegs (rgB g) (rgC g) (rgD g) (rgE g) (rgH g) (rgL g))
        (Blargg, StopSerial v) -> v
        (BlarggMem, StopSerial v) -> v
        (_, s) -> Fail (show s)
  case verdict of
    Pass -> pure ()
    v -> assertFailure (show v <> " after " <> show (resCycles r) <> " M-cycles; serial: " <> show (resSerial r))

blargg :: FilePath -> TestTree
blargg p = testCase p (romPasses Blargg 100000000 (romRoot </> "blargg" </> p))

mooneye :: FilePath -> TestTree
mooneye p = testCase p (romPasses Mooneye 20000000 (romRoot </> "mooneye" </> p))

-- | The last complete frame after @n@ frames, with buttons held.
screenWithKeys :: FilePath -> Word8 -> Int -> IO Screen
screenWithKeys path keys n = do
  rom <- loadRomFile path
  pure $ runST $ do
    m <- either error id <$> newMachine rom
    setJoypad m keys
    let go = do
          f <- frames m
          if f >= n then screen m else step m >> go
    go

-- | save.gb (our battery-save ROM) from cartridge RAM starting with
-- @ram@ (zeros after it), for 5 frames: cartridge RAM's first 7 bytes and
-- the last frame.
saveRun :: BS.ByteString -> [Word8] -> ([Word8], Screen)
saveRun rom ram = runST $ do
  m <- either error id <$> newMachine rom
  mapM_ (\(i, b) -> MU.write (mCram m) i b) (zip [0 ..] ram)
  let go = do
        f <- frames m
        if f >= 5 then pure () else step m >> go
  go
  bytes <- mapM (MU.read (mCram m)) [0 .. 6]
  sc <- screen m
  pure (bytes, sc)

-- | The last complete frame after @n@ frames.
screenAfter :: Int -> FilePath -> IO Screen
screenAfter n path = do
  rom <- loadRomFile path
  r <- either (ioError . userError) pure (runRom (Frames n) (n * 17556 + 100000) rom)
  pure (resScreen r)

romTests :: TestTree
romTests = testGroup "spec on test ROMs"
  [ soundTests
  , testGroup "screens"
      [ testCase "dmg-acid2 equals its reference image (30 frames)" $ do
          ref <- readFrame (romRoot </> "dmg-acid2/reference-dmg.png") >>= either assertFailure pure
          sc <- screenAfter 30 (romRoot </> "dmg-acid2/dmg-acid2.gb")
          maybe (pure ()) assertFailure (frameDiff ref (screenFrame sc))
          frameScreen ref @?= Right sc
      , screenGolden "dmg-acid2" "dmg-acid2" (screenAfter 30 (romRoot </> "dmg-acid2/dmg-acid2.gb"))
      , screenGolden "Blargg cpu_instrs, final screen" "blargg-cpu_instrs"
          (screenAfter 3300 (romRoot </> "blargg/cpu_instrs/cpu_instrs.gb")) -- "Passed" at frame ~3195
      , screenGolden "Blargg instr_timing, final screen" "blargg-instr_timing"
          (screenAfter 60 (romRoot </> "blargg/instr_timing.gb")) -- ~38
      , screenGolden "Blargg mem_timing, final screen" "blargg-mem_timing"
          (screenAfter 120 (romRoot </> "blargg/mem_timing/mem_timing.gb")) -- ~89
      , screenGolden "Blargg mem_timing-2 (screen only), final screen" "blargg-mem_timing-2"
          (screenAfter 600 (romRoot </> "blargg/mem_timing-2.gb"))
      , screenGolden "our buttons.gb with Right and A held (keys 0x11)" "buttons-right-a"
          (screenWithKeys (asmRoot </> "buttons.gb") 0x11 30)
      , screenGolden "Blargg halt_bug (screen only), final screen" "blargg-halt_bug"
          (screenAfter 600 (romRoot </> "blargg/halt_bug.gb"))
      , screenGolden "our save.gb booted from a save with count 41: 00042" "save-42"
          (snd . flip saveRun (map (fromIntegral . fromEnum) "SAVE" <> [41, 0]) <$> loadRomFile (asmRoot </> "save.gb"))
      ]
  , testGroup "our regression ROMs"
      [ testCase "serial_pass.gb: Passed over serial" $
          romPasses Blargg 100000 (asmRoot </> "serial_pass.gb")
      , testCase "serial_pass.gb: Mooneye pass pattern" $
          romPasses Mooneye 100000 (asmRoot </> "serial_pass.gb")
      , testCase "halt_bug.gb: both HALT bug cases (Pan Docs)" $
          romPasses Mooneye 100000 (asmRoot </> "halt_bug.gb")
      , testCase "mbc3.gb: MBC3 banks and the real-time clock (side track I)" $
          romPasses Mooneye 6000000 (asmRoot </> "mbc3.gb")
      , testCase "save.gb: counts its boots in cartridge RAM (side track I)" $ do
          rom <- loadRomFile (asmRoot </> "save.gb")
          let (blank, scBlank) = saveRun rom []
              (saved, scSaved) = saveRun rom (map (fromIntegral . fromEnum) "SAVE" <> [41, 0, 0x77])
          blank @?= map (fromIntegral . fromEnum) "SAVE" <> [1, 0, 0]
          saved @?= map (fromIntegral . fromEnum) "SAVE" <> [42, 0, 0x77]
          assertBool "the count shows on the screen" (scBlank /= scSaved)
      ]
  , testGroup "Blargg (serial)" $
      map (blargg . ("cpu_instrs" </>))
        [ "01-special.gb", "02-interrupts.gb", "03-op_sp_hl.gb", "04-op_r_imm.gb"
        , "05-op_rp.gb", "06-ld_r_r.gb", "07-jr_jp_call_ret_rst.gb", "08-misc_instrs.gb"
        , "09-op_r_r.gb", "10-bit_ops.gb", "11-op_a_hl.gb", "cpu_instrs.gb" ]
      <> [ blargg "instr_timing.gb" ]
      <> map (blargg . ("mem_timing" </>))
        [ "01-read_timing.gb", "02-write_timing.gb", "03-modify_timing.gb", "mem_timing.gb" ]
  , testGroup "Mooneye (LD B,B)" (map mooneye mooneyePassing)
  , testGroup "Game Boy Color: screens (side track I phase 3)"
      [ testCase "cgb-acid2 equals its reference image (CGB mode)" $ do
          ref <- readFrame (romRoot </> "../gbc/cgb-acid2/reference.png") >>= either assertFailure pure
          rom <- loadRomFile (romRoot </> "../gbc/cgb-acid2/cgb-acid2.gbc")
          let sc = runST $ do
                m <- either error id <$> newMachineModel Cgb rom
                let go = do
                      f <- frames m
                      if f >= 30 then screenRgb m else step m >> go
                go
          maybe (pure ()) assertFailure (frameDiff ref (rgbFrame sc))
      , goldenTest "dmg-acid2 on the GBC with three palettes as test/golden/spec/dmg-acid2-gbc.png (for gb-golden; --accept to update)"
          (either error id <$> readFrame "test/golden/spec/dmg-acid2-gbc.png")
          (do rom <- loadRomFile (romRoot </> "dmg-acid2/dmg-acid2.gb")
              pure (rgbFrame (runST $ do
                -- test/ps/palette.ps's palettes: BG 28, OBJ 0 4, OBJ 1 5
                m <- either error id <$> newMachineModelPal Cgb
                       [ [0x7FFF, 0x7E8C, 0x7C00, 0x0000], [0x7FFF, 0x421F, 0x1CF2, 0x0000], [0x7FFF, 0x5294, 0x294A, 0x0000] ] rom
                let go = do
                      f <- frames m
                      if f >= 30 then screenRgb m else step m >> go
                go)))
          (\g t -> pure (frameDiff g t))
          (writeFrame "test/golden/spec/dmg-acid2-gbc.png")
      , goldenTest "cgb-acid2's screen as test/golden/spec/cgb-acid2.png (for gb-golden; --accept to update)"
          (either error id <$> readFrame "test/golden/spec/cgb-acid2.png")
          (do rom <- loadRomFile (romRoot </> "../gbc/cgb-acid2/cgb-acid2.gbc")
              pure (rgbFrame (runST $ do
                m <- either error id <$> newMachineModel Cgb rom
                let go = do
                      f <- frames m
                      if f >= 30 then screenRgb m else step m >> go
                go)))
          (\g t -> pure (frameDiff g t))
          (writeFrame "test/golden/spec/cgb-acid2.png")
      ]
  , testGroup "Game Boy Color: Blargg in CGB mode"
      ([ testCase p (romPassesOn Cgb Blargg 100000000 (romRoot </> p))
       | p <- [ "blargg/cpu_instrs/cpu_instrs.gb", "blargg/instr_timing.gb", "blargg/mem_timing/mem_timing.gb" ] ]
      <> [ testCase "our speed.gb: KEY1 and the speed switch, the LCD at its own speed" $
             romPassesOn Cgb Mooneye 2000000 (asmRoot </> "speed.gb") ]
      <> [ testCase "our hdma.gb: general purpose and HBlank DMA into both VRAM banks, stopping, timing" $
             romPassesOn Cgb Mooneye 2000000 (asmRoot </> "hdma.gb") ]
      <> [ testCase "../gbc/blargg/interrupt_time.gb (double speed; reaches its exit loop: a smoke test only)" $ do
             -- built for Blargg's devcart: its print routines are stubs and
             -- it has no cartridge RAM; it ends in its exit loop (0xC9C9 in
             -- WRAM, copied from ROM 0x49C9) with A = 0 even without the
             -- speed switch (a "Done" path skips the CRC check), so this
             -- only shows it runs to its end in both speeds; speed.gb is
             -- the gate
             rom <- loadRomFile (romRoot </> "../gbc/blargg/interrupt_time.gb")
             let (pc, a) = runST $ do
                   m <- either error id <$> newMachineModel Cgb rom
                   let go k = do
                         p <- getSt m iPC
                         if p == 0xC9C9 || k == (0 :: Int) then (,) p . rgA <$> regs m else step m >> go (k - 1)
                   go 50000000
             pc @?= 0xC9C9
             a @?= 0 ])
  , testGroup "Game Boy Color: Blargg cgb_sound (results in cartridge RAM)"
      [ testCase p (romPassesOn Cgb BlarggMem 100000000 (romRoot </> "../gbc/blargg/cgb_sound" </> p))
      | p <- [ "01-registers.gb", "02-len_ctr.gb", "03-trigger.gb", "04-sweep.gb", "05-sweep_details.gb"
             , "06-overflow_on_trigger.gb", "07-len_sweep_period_sync.gb", "08-len_ctr_during_power.gb"
             , "09-wave_read_while_on.gb", "10-wave_trigger_while_on.gb", "11-regs_after_power.gb", "12-wave.gb" ] ]
  , testGroup "Game Boy Color: Mooneye's CGB tests (DMG cartridges in compatibility mode; side track I phase 3)"
      [ testCase p (romPassesOn Cgb Mooneye 20000000 (romRoot </> "mooneye/misc" </> p)) | p <- mooneyeCgb ]
  ]

-- | Mooneye's tests for the CGB (in the pinned archive's misc/; the
-- CGB0-only boot_div-cgb0 is out of scope).
mooneyeCgb :: [FilePath]
mooneyeCgb =
  [ "boot_regs-cgb.gb", "boot_hwio-C.gb", "bits/unused_hwio-C.gb"
  , "boot_div-cgbABCDE.gb", "ppu/vblank_stat_intr-C.gb" ]

-- | The Mooneye ROMs the spec passes (86 of the 96 in acceptance/ and
-- emulator-only/mbc1, mbc5 after step 2.6; mbc2 from side track I phase 2,
-- 93 of 103). The others:
--
-- * Never on a DMG: the @boot_*@ tests for DMG0, MGB, SGB, SGB2.
-- * Out of scope (MBC1 multicart wiring): @emulator-only/mbc1/multicart_rom_8Mb@.
mooneyePassing :: [FilePath]
mooneyePassing =
  [ "acceptance/add_sp_e_timing.gb"
  , "acceptance/bits/mem_oam.gb"
  , "acceptance/bits/reg_f.gb"
  , "acceptance/bits/unused_hwio-GS.gb"
  , "acceptance/boot_div-dmgABCmgb.gb"
  , "acceptance/boot_hwio-dmgABCmgb.gb"
  , "acceptance/boot_regs-dmgABC.gb"
  , "acceptance/call_cc_timing2.gb"
  , "acceptance/call_cc_timing.gb"
  , "acceptance/call_timing2.gb"
  , "acceptance/call_timing.gb"
  , "acceptance/di_timing-GS.gb"
  , "acceptance/div_timing.gb"
  , "acceptance/ei_sequence.gb"
  , "acceptance/ei_timing.gb"
  , "acceptance/halt_ime0_ei.gb"
  , "acceptance/halt_ime0_nointr_timing.gb"
  , "acceptance/halt_ime1_timing2-GS.gb"
  , "acceptance/halt_ime1_timing.gb"
  , "acceptance/if_ie_registers.gb"
  , "acceptance/instr/daa.gb"
  , "acceptance/interrupts/ie_push.gb"
  , "acceptance/intr_timing.gb"
  , "acceptance/jp_cc_timing.gb"
  , "acceptance/jp_timing.gb"
  , "acceptance/ld_hl_sp_e_timing.gb"
  , "acceptance/oam_dma/basic.gb"
  , "acceptance/oam_dma/reg_read.gb"
  , "acceptance/oam_dma_restart.gb"
  , "acceptance/oam_dma/sources-GS.gb"
  , "acceptance/oam_dma_start.gb"
  , "acceptance/oam_dma_timing.gb"
  , "acceptance/pop_timing.gb"
  , "acceptance/ppu/hblank_ly_scx_timing-GS.gb"
  , "acceptance/ppu/intr_1_2_timing-GS.gb"
  , "acceptance/ppu/intr_2_0_timing.gb"
  , "acceptance/ppu/intr_2_mode0_timing.gb"
  , "acceptance/ppu/intr_2_mode0_timing_sprites.gb"
  , "acceptance/ppu/intr_2_mode3_timing.gb"
  , "acceptance/ppu/intr_2_oam_ok_timing.gb"
  , "acceptance/ppu/lcdon_timing-GS.gb"
  , "acceptance/ppu/lcdon_write_timing-GS.gb"
  , "acceptance/ppu/stat_irq_blocking.gb"
  , "acceptance/ppu/stat_lyc_onoff.gb"
  , "acceptance/ppu/vblank_stat_intr-GS.gb"
  , "acceptance/push_timing.gb"
  , "acceptance/rapid_di_ei.gb"
  , "acceptance/ret_cc_timing.gb"
  , "acceptance/reti_intr_timing.gb"
  , "acceptance/reti_timing.gb"
  , "acceptance/ret_timing.gb"
  , "acceptance/rst_timing.gb"
  , "acceptance/serial/boot_sclk_align-dmgABCmgb.gb"
  , "acceptance/timer/div_write.gb"
  , "acceptance/timer/rapid_toggle.gb"
  , "acceptance/timer/tim00_div_trigger.gb"
  , "acceptance/timer/tim00.gb"
  , "acceptance/timer/tim01_div_trigger.gb"
  , "acceptance/timer/tim01.gb"
  , "acceptance/timer/tim10_div_trigger.gb"
  , "acceptance/timer/tim10.gb"
  , "acceptance/timer/tim11_div_trigger.gb"
  , "acceptance/timer/tim11.gb"
  , "acceptance/timer/tima_reload.gb"
  , "acceptance/timer/tima_write_reloading.gb"
  , "acceptance/timer/tma_write_reloading.gb"
  , "emulator-only/mbc1/bits_bank1.gb"
  , "emulator-only/mbc1/bits_bank2.gb"
  , "emulator-only/mbc1/bits_mode.gb"
  , "emulator-only/mbc1/bits_ramg.gb"
  , "emulator-only/mbc1/ram_256kb.gb"
  , "emulator-only/mbc1/ram_64kb.gb"
  , "emulator-only/mbc1/rom_16Mb.gb"
  , "emulator-only/mbc1/rom_1Mb.gb"
  , "emulator-only/mbc1/rom_2Mb.gb"
  , "emulator-only/mbc1/rom_4Mb.gb"
  , "emulator-only/mbc1/rom_512kb.gb"
  , "emulator-only/mbc1/rom_8Mb.gb"
  , "emulator-only/mbc2/bits_ramg.gb"
  , "emulator-only/mbc2/bits_romb.gb"
  , "emulator-only/mbc2/bits_unused.gb"
  , "emulator-only/mbc2/ram.gb"
  , "emulator-only/mbc2/rom_1Mb.gb"
  , "emulator-only/mbc2/rom_2Mb.gb"
  , "emulator-only/mbc2/rom_512kb.gb"
  , "emulator-only/mbc5/rom_16Mb.gb"
  , "emulator-only/mbc5/rom_1Mb.gb"
  , "emulator-only/mbc5/rom_2Mb.gb"
  , "emulator-only/mbc5/rom_32Mb.gb"
  , "emulator-only/mbc5/rom_4Mb.gb"
  , "emulator-only/mbc5/rom_512kb.gb"
  , "emulator-only/mbc5/rom_64Mb.gb"
  , "emulator-only/mbc5/rom_8Mb.gb"
  ]

-- | Rising zero crossings per second of a 48 kHz signal (DC removed).
toneHz :: [Int] -> Double
toneHz xs = fromIntegral crossings / (fromIntegral (length xs) / 48000)
 where
  mean = sum xs `div` max 1 (length xs)
  ys = map (subtract mean) xs
  crossings = length [ () | (a, b) <- zip ys (drop 1 ys), a < 0, b >= 0 ]

soundTests :: TestTree
soundTests = testGroup "sound (step 2.9)"
  [ testGroup "Blargg dmg_sound (results in cartridge RAM)"
      [ testCase f (romPasses BlarggMem 60000000 (romRoot </> "blargg/dmg_sound" </> f))
      | f <- [ "01-registers.gb", "02-len_ctr.gb", "03-trigger.gb", "04-sweep.gb", "05-sweep_details.gb"
             , "06-overflow_on_trigger.gb", "07-len_sweep_period_sync.gb", "08-len_ctr_during_power.gb"
             , "11-regs_after_power.gb" ] ]
  , testCase "tone.gb: CH1 square at 439.8 Hz on both sides (1 s at 48 kHz)" $ do
      rom <- loadRomFile (asmRoot </> "tone.gb")
      smps <- either assertFailure pure (audioRun 1048576 rom)
      let l = map fst (drop 4800 smps)
          r = map snd (drop 4800 smps)
          hz = toneHz l
      assertBool ("left at " <> show hz <> " Hz") (abs (hz - 439.8) < 2)
      l @?= r
      assertBool "full scale: 15 × 8 = 120" (maximum l - minimum l == 2 * 120)
  ]
