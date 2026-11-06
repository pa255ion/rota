-- | Layer 2: the Clash CPU core ("Gb.Cpu") against the reference model's
-- CPU, in lockstep: per spec step, the bus accesses M-cycle by M-cycle and
-- the registers after it. Random programs (hedgehog) and test ROMs; then
-- the core alone on the long CPU test ROMs.
module Main (main) where

import Prelude

import Control.Monad.ST (runST)
import Data.Bits (shiftR, xor)
import qualified Data.ByteString as BS
import Data.Word
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import System.FilePath ((</>))
import Test.Tasty
import Test.Tasty.Hedgehog (testProperty)
import Test.Tasty.HUnit

import Gb.CoreHarness
import Gb.PpuHarness
import Gb.FullHarness
import Gb.Core (CoreModel (..), CoreOut (..), CoreState (..), McEvent (..), RomPort, cartFromHeader, gbCore, gbCoreWith, gbcDefaultPal)
import qualified Clash.Prelude as C
import Data.Maybe (catMaybes)
import Gb.Hooks (Verdict (..))
import Gb.Spec (Access, Model (..), Regs, newMachine, step, withTrace)
import Gb.TestRoms
import System.Environment (lookupEnv)
import System.IO.Unsafe (unsafePerformIO)

-- | Test tiers (docs/TESTING.md, "Tiers and parallel runs"): the quick tier
-- (default) runs a quarter of every lockstep run and random test count and
-- three of the CPU test ROMs on the core alone; EMU_FULL_TESTS=1 (make
-- test FULL=1) runs everything.
fullTier :: Bool
fullTier = unsafePerformIO ((== Just "1") <$> lookupEnv "EMU_FULL_TESTS")
{-# NOINLINE fullTier #-}

scaled :: Int -> Int
scaled n = if fullTier then n else max 5000 (n `div` 4)

tierTests :: H.TestLimit -> H.TestLimit
tierTests k = if fullTier then k else max 25 (k `div` 4)

main :: IO ()
main = defaultMain $ testGroup "gb-lockstep"
  [ randomPrograms
  , romLockstep
  , coreRoms
  , ppuTests
  , fullTests
  , clashTests
  , stallTests
  , costReport
  ]

-- | Run both for up to @n@ steps; the first difference, if any.
lockstep :: Int -> BS.ByteString -> Maybe String
lockstep n rom = runST $ do
  ma <- either error id <$> newMachine rom
  cr <- either error id <$> newCoreRun rom
  let go k
        | k >= n = pure Nothing
        | otherwise = do
            ra0 <- specRegs ma
            (_, ta) <- withTrace ma (step ma)
            tb <- coreStep cr
            ra <- specRegs ma
            rb <- coreRegs cr
            if ta /= tb || ra /= rb
              then pure (Just (report k ra0 ta tb ra rb))
              else go (k + 1)
  go 0
 where
  report :: Int -> Regs -> [Access] -> [Access] -> Regs -> Regs -> String
  report k r0 ta tb ra rb = unlines
    [ "step " <> show k <> ", before it: " <> show r0
    , " spec bus: " <> show ta
    , " core bus: " <> show tb
    , " spec after: " <> show ra
    , " core after: " <> show rb ]

-- | A whole 32 KiB ROM of pseudo-random bytes from a seed (no MBC).
randomRom :: Word64 -> BS.ByteString
randomRom seed = BS.pack [ hdr i (byte i) | i <- [0 .. 0x7FFF] ]
 where
  byte :: Int -> Word8
  byte i = let x = splitmix (seed `xor` fromIntegral i) in fromIntegral (x `shiftR` 32)
  hdr i b | i >= 0x147 && i <= 0x149 = 0 -- ROM only, 32 KiB, no RAM
          | otherwise = b

-- | Like 'randomRom', but a quarter of the code is @LD A,n; LDH (r),A@ to
-- an I/O register the core models (timer, serial, joypad, IF, IE, PPU,
-- OAM DMA), so random programs exercise them.
randomRomIo :: Word64 -> BS.ByteString
randomRomIo seed = BS.pack (take 0x8000 (fixHdr (go 0)))
 where
  rnd :: Int -> Word64
  rnd i = splitmix (seed `xor` (fromIntegral i * 0x9E37))
  ioRegs = [0x00, 0x01, 0x02, 0x04, 0x05, 0x06, 0x07, 0x0F, 0x40, 0x41, 0x42, 0x43, 0x45, 0x46, 0x47, 0x4A, 0x4B, 0xFF
           -- sound (step 2.9): every register, wave RAM twice
           , 0x10, 0x11, 0x12, 0x13, 0x14, 0x16, 0x17, 0x18, 0x19, 0x1A, 0x1B, 0x1C, 0x1D, 0x1E
           , 0x20, 0x21, 0x22, 0x23, 0x24, 0x25, 0x26, 0x26, 0x30, 0x37, 0x3F]
  go :: Int -> [Word8]
  go i
    | i >= 0x8000 = []
    | rnd i `mod` 4 == 0 =
        let r = ioRegs !! fromIntegral ((rnd (i + 1) `shiftR` 8) `mod` fromIntegral (length ioRegs))
            v = fromIntegral (rnd (i + 2) `shiftR` 16)
        in 0x3E : v : 0xE0 : r : go (i + 4)
    | otherwise = fromIntegral (rnd i `shiftR` 32) : go (i + 1)
  fixHdr bs = [ if j >= 0x147 && j <= 0x149 then 0 else b | (j, b) <- zip [0 :: Int ..] bs ]

splitmix :: Word64 -> Word64
splitmix x0 = let x1 = (x0 + 0x9E3779B97F4A7C15)
                  x2 = (x1 `xor` (x1 `shiftR` 30)) * 0xBF58476D1CE4E5B9
                  x3 = (x2 `xor` (x2 `shiftR` 27)) * 0x94D049BB133111EB
              in x3 `xor` (x3 `shiftR` 31)

randomPrograms :: TestTree
randomPrograms = testProperty "random programs: 3000 steps, identical bus traces and registers" $
  H.withTests (tierTests 300) $ H.property $ do
    seed <- H.forAll (Gen.word64 Range.constantBounded)
    case lockstep 3000 (randomRom seed) of
      Nothing -> pure ()
      Just e -> H.annotate e >> H.failure

romLockstep :: TestTree
romLockstep = testGroup "test ROMs in lockstep (first steps)"
  [ t "our serial_pass.gb" (asmRoot </> "serial_pass.gb") 5000
  , t "our halt_bug.gb" (asmRoot </> "halt_bug.gb") 5000
  , t "Blargg 02-interrupts" (romRoot </> "blargg/cpu_instrs/02-interrupts.gb") 200000
  , t "Blargg instr_timing" (romRoot </> "blargg/instr_timing.gb") 300000
  , t "Mooneye ei_sequence" (romRoot </> "mooneye/acceptance/ei_sequence.gb") 100000
  , t "Mooneye halt_ime1_timing2-GS" (romRoot </> "mooneye/acceptance/halt_ime1_timing2-GS.gb") 100000
  , t "Mooneye ie_push" (romRoot </> "mooneye/acceptance/interrupts/ie_push.gb") 100000
  , t "Mooneye rapid_toggle" (romRoot </> "mooneye/acceptance/timer/rapid_toggle.gb") 100000
  , t "Mooneye call_timing (OAM DMA)" (romRoot </> "mooneye/acceptance/call_timing.gb") 100000
  , t "dmg-acid2" (romRoot </> "dmg-acid2/dmg-acid2.gb") 200000
  ]
 where
  t name path n0 = testCase name $ do
    let n = scaled n0
    rom <- loadRomFile path
    maybe (pure ()) assertFailure (lockstep n rom)

cpuRoms :: [FilePath]
cpuRoms =
  [ "01-special.gb", "02-interrupts.gb", "03-op_sp_hl.gb", "04-op_r_imm.gb", "05-op_rp.gb"
  , "06-ld_r_r.gb", "07-jr_jp_call_ret_rst.gb", "08-misc_instrs.gb", "09-op_r_r.gb"
  , "10-bit_ops.gb", "11-op_a_hl.gb" ]

coreRoms :: TestTree
coreRoms = testGroup "the core alone on the CPU test ROMs (Blargg over serial)" $
  [ t ("cpu_instrs/" <> f) (romRoot </> "blargg/cpu_instrs" </> f)
  | f <- if fullTier then cpuRoms else ["01-special.gb", "02-interrupts.gb"] ]
  <> [ t "instr_timing" (romRoot </> "blargg/instr_timing.gb") ]
  <> [ t "mem_timing" (romRoot </> "blargg/mem_timing/mem_timing.gb") | fullTier ]
 where
  t name path = testCase name $ do
    rom <- loadRomFile path
    case runCoreRom False 60000000 rom of
      Left e -> assertFailure e
      Right r | corVerdict r == Pass -> pure ()
              | otherwise -> assertFailure (show (corVerdict r) <> " after " <> show (corCycles r)
                                            <> " M-cycles: " <> show (corSerial r))

-- | Gb.Top pauses the core for the PS menu by holding its dot enable low
-- ('Gb.Core' itself unchanged). Within a few M-cycles (the stored dot
-- credit, at most 15 dots) the state must stop changing, with no pixels
-- or events; after resuming, the machine must be the same as without a
-- pause, step for step (only the dot credit, which paces it, may differ).
pauseTest :: Assertion
pauseTest = do
  rom <- loadRomFile (romRoot </> "blargg/instr_timing.gb")
  let total = 300000 :: Int        -- spec steps in all
      at = 120000 :: Int           -- pause after this many
      pauseClocks = 30000 :: Int
      settle = 500 :: Int          -- clocks for the credit to run out
      noCredit s = s { ksCredit = 0 }
      result = runST $ do
        a <- either error id <$> newCoreSim rom
        b <- either error id <$> newCoreSim rom
        let steps cs n dot = go n 0
             where
              go 0 _ = pure ()
              go k c = do
                (e, _) <- clockDot cs dot 0
                case e of
                  Just ev | meStepDone ev -> go (k - 1) (c + 1)
                  _ -> go k (c + 1 :: Int)
            idle cs n = do
              outs <- mapM (const (clockDot cs False 0)) [1 .. n]
              pure [ o | o@(e, p) <- outs, isJust' e || isJust' p ]
            isJust' = maybe False (const True)
        steps a total True
        steps b at True
        -- the stored credit may still finish a step or two here
        early <- idle b settle
        let doneEarly = length [ () | (Just ev, _) <- early, meStepDone ev ]
        frozenAt <- coreState b
        fbAt <- coreFb b
        busy <- idle b pauseClocks
        frozenEnd <- coreState b
        fbEnd <- coreFb b
        steps b (total - at - doneEarly) True
        sa <- coreState a
        sb <- coreState b
        fa <- coreFb a
        fb <- coreFb b
        sera <- coreSerial a
        serb <- coreSerial b
        pure (frozenAt == frozenEnd, fbAt == fbEnd, length busy, firstDiff (show (noCredit sa)) (show (noCredit sb)), fa == fb, sera == serb, length sera)
      (frozen, fbSame, busyN, diff, fbEq, serEq, serLen) = result
      -- where two Show strings first differ, with some context
      firstDiff x y = case [ i | (i, c, d) <- zip3 [0 :: Int ..] x y, c /= d ] of
        [] | length x == length y -> Nothing
        [] -> Just ("lengths differ: " <> show (length x, length y))
        (i : _) -> Just (take 300 (drop (i - 150) x) <> "\n  vs\n" <> take 300 (drop (i - 150) y))
  assertBool "state changed while paused" frozen
  assertBool "frame buffer changed while paused" fbSame
  busyN @?= 0
  maybe (pure ()) (assertFailure . ("the run with a pause ended in a different state:\n" <>)) diff
  assertBool "frame buffers differ" fbEq
  assertBool "serial output differs" serEq
  assertBool "no serial output: the ROM did nothing to compare" (serLen > 0)

-- * PPU (step 2.5)

ppuTests :: TestTree
ppuTests = testGroup "Clash PPU against the spec PPU (every M-cycle, every pixel)"
  [ t "dmg-acid2, 30 frames" (romRoot </> "dmg-acid2/dmg-acid2.gb") (30 * 17556)
  , t "Mooneye intr_2_mode0_timing_sprites" (romRoot </> "mooneye/acceptance/ppu/intr_2_mode0_timing_sprites.gb") 3000000
  , t "Mooneye lcdon_timing-GS" (romRoot </> "mooneye/acceptance/ppu/lcdon_timing-GS.gb") 1000000
  , t "Mooneye lcdon_write_timing-GS" (romRoot </> "mooneye/acceptance/ppu/lcdon_write_timing-GS.gb") 1000000
  , t "Mooneye stat_lyc_onoff" (romRoot </> "mooneye/acceptance/ppu/stat_lyc_onoff.gb") 1000000
  , t "Mooneye vblank_stat_intr-GS" (romRoot </> "mooneye/acceptance/ppu/vblank_stat_intr-GS.gb") 1000000
  , t "Mooneye hblank_ly_scx_timing-GS" (romRoot </> "mooneye/acceptance/ppu/hblank_ly_scx_timing-GS.gb") 1000000
  , t "Mooneye oam_dma_start" (romRoot </> "mooneye/acceptance/oam_dma_start.gb") 1000000
  , t "Blargg cpu_instrs, first 60 frames" (romRoot </> "blargg/cpu_instrs/cpu_instrs.gb") (60 * 17556)
  , testProperty "random programs, 20000 M-cycles" $ H.withTests (tierTests 100) $ H.property $ do
      seed <- H.forAll (Gen.word64 Range.constantBounded)
      case ppuLockstep 20000 (randomRom seed) of
        Nothing -> pure ()
        Just e -> H.annotate e >> H.failure
  ]
 where
  t name path n0 = testCase name $ do
    let n = scaled n0
    rom <- loadRomFile path
    maybe (pure ()) assertFailure (ppuLockstep n rom)

-- * The whole core (step 2.6)

fullTests :: TestTree
fullTests = testGroup "the whole Clash core (Gb.Core) against the spec, per step"
  [ testCase "paused (dot enable held low, the PS menu; side track A): frozen, then the same machine" pauseTest
  , testCase "cartFromHeader (hardware) = Gb.Cartridge on every fetched ROM (up to 8 MiB)" $ do
      ps <- fetchedRoms
      bad <- fmap catMaybes . mapM (\p -> do
        rom <- loadRomFile p
        let h i = fromIntegral (BS.index rom i)
            (hw, ok) = cartFromHeader (h 0x147) (h 0x148) (h 0x149) (h 0x14D)
        pure (case cartCfg rom of
          Right c | c /= hw || not ok -> Just (p, c, hw)
          _ -> Nothing)) $ ps
      bad @?= []
  , testProperty "random programs and random keys, 3000 steps" $ H.withTests (tierTests 200) $ H.property $ do
      seed <- H.forAll (Gen.word64 Range.constantBounded)
      let keys k = fromIntegral (splitmix (seed + fromIntegral (k `div` 200)) `shiftR` 56)
      case fullLockstep 3000 keys (randomRomIo seed) of
        Nothing -> pure ()
        Just e -> H.annotate e >> H.failure
  , t "our serial_pass.gb" (asmRoot </> "serial_pass.gb") 5000
  , t "our halt_bug.gb" (asmRoot </> "halt_bug.gb") 5000
  , t "our save.gb (cartridge RAM, MBC1 enable)" (asmRoot </> "save.gb") 20000
  -- side track I, phase 3: the Game Boy Color, core and spec on the CGB
  -- model (Mooneye's CGB tests run in DMG compatibility mode)
  , tc "Mooneye boot_regs-cgb (CGB)" (romRoot </> "mooneye/misc/boot_regs-cgb.gb") 200000
  , tc "Mooneye boot_hwio-C (CGB)" (romRoot </> "mooneye/misc/boot_hwio-C.gb") 200000
  , tc "Mooneye unused_hwio-C (CGB)" (romRoot </> "mooneye/misc/bits/unused_hwio-C.gb") 200000
  , tc "Mooneye boot_div-cgbABCDE (CGB)" (romRoot </> "mooneye/misc/boot_div-cgbABCDE.gb") 200000
  , tc "Mooneye vblank_stat_intr-C (CGB)" (romRoot </> "mooneye/misc/ppu/vblank_stat_intr-C.gb") 1000000
  , tc "cgb-acid2 (CGB mode: attributes, palettes, priority; every colour pixel)" (romRoot </> "../gbc/cgb-acid2/cgb-acid2.gbc") 1000000
  , testCase "dmg-acid2 on the Game Boy Color with three palettes the PS gave (BG, OBJ 0, OBJ 1 apart; every colour pixel)" $ do
      rom <- loadRomFile (romRoot </> "dmg-acid2/dmg-acid2.gb")
      maybe (pure ()) assertFailure (fullLockstepPal Cgb (Just threePalettes) (scaled 1000000) (const 0) rom)
  , tc "our speed.gb (CGB: KEY1, the speed switch and its pause, double speed)" (asmRoot </> "speed.gb") 2000000
  , tc "our hdma.gb (CGB: general purpose and HBlank VRAM DMA, both speeds)" (asmRoot </> "hdma.gb") 2000000
  , testCase "our joypad.gb, keys changing" $ do
      rom <- loadRomFile (asmRoot </> "joypad.gb")
      let keysAt k | k < 300 = 0x21 | k < 600 = 0x00 | otherwise = 0x20
      maybe (pure ()) assertFailure (fullLockstep 2000 keysAt rom)
  , t "Mooneye timer/tim00" (romRoot </> "mooneye/acceptance/timer/tim00.gb") 1000000
  , t "Mooneye timer/tim01" (romRoot </> "mooneye/acceptance/timer/tim01.gb") 1000000
  , t "Mooneye timer/tim10" (romRoot </> "mooneye/acceptance/timer/tim10.gb") 1000000
  , t "Mooneye timer/tim11" (romRoot </> "mooneye/acceptance/timer/tim11.gb") 1000000
  , t "dmg-acid2 (to its LD B,B)" (romRoot </> "dmg-acid2/dmg-acid2.gb") 1000000
  , t "Mooneye oam_dma/sources-GS (MBC5)" (romRoot </> "mooneye/acceptance/oam_dma/sources-GS.gb") 1000000
  , t "Mooneye emulator-only/mbc1/bits_ramg" (romRoot </> "mooneye/emulator-only/mbc1/bits_ramg.gb") 1000000
  , t "Mooneye emulator-only/mbc5/rom_512kb" (romRoot </> "mooneye/emulator-only/mbc5/rom_512kb.gb") 1000000
  -- side track I, phase 2: MBC2, MBC3 and its clock, 8 MiB, 32 KiB RAM
  , t "Mooneye emulator-only/mbc2/bits_ramg" (romRoot </> "mooneye/emulator-only/mbc2/bits_ramg.gb") 1000000
  , t "Mooneye emulator-only/mbc2/bits_romb" (romRoot </> "mooneye/emulator-only/mbc2/bits_romb.gb") 1000000
  , t "Mooneye emulator-only/mbc2/bits_unused" (romRoot </> "mooneye/emulator-only/mbc2/bits_unused.gb") 1000000
  , t "Mooneye emulator-only/mbc2/ram" (romRoot </> "mooneye/emulator-only/mbc2/ram.gb") 1000000
  , t "Mooneye emulator-only/mbc2/rom_2Mb" (romRoot </> "mooneye/emulator-only/mbc2/rom_2Mb.gb") 1000000
  , t "Mooneye emulator-only/mbc1/ram_256kb" (romRoot </> "mooneye/emulator-only/mbc1/ram_256kb.gb") 1000000
  , t "Mooneye emulator-only/mbc1/rom_16Mb" (romRoot </> "mooneye/emulator-only/mbc1/rom_16Mb.gb") 1000000
  , t "Mooneye emulator-only/mbc5/rom_64Mb (8 MiB)" (romRoot </> "mooneye/emulator-only/mbc5/rom_64Mb.gb") 1000000
  , t "our mbc3.gb (banks, the clock: 3 waits of 1.3 s)" (asmRoot </> "mbc3.gb") 2000000
  , t "Mooneye lcdon_timing-GS" (romRoot </> "mooneye/acceptance/ppu/lcdon_timing-GS.gb") 1000000
  , t "Mooneye intr_2_mode0_timing_sprites" (romRoot </> "mooneye/acceptance/ppu/intr_2_mode0_timing_sprites.gb") 1000000
  , t "Mooneye boot_sclk_align (serial)" (romRoot </> "mooneye/acceptance/serial/boot_sclk_align-dmgABCmgb.gb") 1000000
  , t "Mooneye rapid_toggle" (romRoot </> "mooneye/acceptance/timer/rapid_toggle.gb") 1000000
  , t "Blargg 02-interrupts" (romRoot </> "blargg/cpu_instrs/02-interrupts.gb") 300000
  , t "Blargg instr_timing" (romRoot </> "blargg/instr_timing.gb") 400000
  , t "Blargg dmg_sound 01-registers" (romRoot </> "blargg/dmg_sound/01-registers.gb") 400000
  , t "Blargg dmg_sound 03-trigger" (romRoot </> "blargg/dmg_sound/03-trigger.gb") 400000
  , t "Blargg dmg_sound 05-sweep_details" (romRoot </> "blargg/dmg_sound/05-sweep_details.gb") 400000
  , t "our tone.gb" (asmRoot </> "tone.gb") 40000
  , t "our sound_all.gb (every channel feature)" (asmRoot </> "sound_all.gb") 250000
  , tc "our sound_ds.gb (CGB: every channel feature in double speed)" (asmRoot </> "sound_ds.gb") 500000
  , tc "our sound_all.gb (CGB: every channel feature, compatibility mode)" (asmRoot </> "sound_all.gb") 250000
  , tc "Blargg cgb_sound 03-trigger (CGB)" (romRoot </> "../gbc/blargg/cgb_sound/03-trigger.gb") 400000
  , tc "Blargg cgb_sound 08-len_ctr_during_power (CGB)" (romRoot </> "../gbc/blargg/cgb_sound/08-len_ctr_during_power.gb") 400000
  , tc "Blargg cgb_sound 09-wave_read_while_on (CGB)" (romRoot </> "../gbc/blargg/cgb_sound/09-wave_read_while_on.gb") 1000000
  , tc "Blargg cgb_sound 11-regs_after_power (CGB)" (romRoot </> "../gbc/blargg/cgb_sound/11-regs_after_power.gb") 1600000
  , tc "Blargg cgb_sound 12-wave (CGB)" (romRoot </> "../gbc/blargg/cgb_sound/12-wave.gb") 1000000
  ]
 where
  t name path n0 = testCase name $ do
    let n = scaled n0
    rom <- loadRomFile path
    maybe (pure ()) assertFailure (fullLockstep n (const 0) rom)
  tc name path n0 = testCase name $ do
    let n = scaled n0
    rom <- loadRomFile path
    maybe (pure ()) assertFailure (fullLockstepOn Cgb n (const 0) rom)

-- | The Clash design 'gbCore' (block RAMs, loader port) in Clash
-- simulation: load the ROM with 'run' low, then run; its M-cycle events
-- for @n@ clocks.
clashEvents :: Int -> BS.ByteString -> [McEvent]
clashEvents n rom = catMaybes (map koEvent (drop hold (C.sampleN (hold + n) out)))
 where
  len = BS.length rom
  hold = len + 2
  load = [ Just (fromIntegral i, fromIntegral (BS.index rom i)) | i <- [0 .. len - 1] ] <> repeat Nothing
  run = replicate hold False <> repeat True
  out :: C.Signal C.System CoreOut
  out = C.withClockResetEnable C.clockGen C.resetGen C.enableGen $
          gbCore (C.blockRam1 C.NoClearOnReset (C.SNat @262144) 0) (C.fromList run) (pure True) (pure 0) (C.fromList load)

-- | A ROM port in Clash simulation (side track I, phase 2: the cartridge
-- in DDR) that answers each request after 0 to 'maxDelay' extra clocks,
-- pseudo-randomly from @seed@ (0 extra: the next clock, a cache hit), and
-- stops the simulation if the core breaks the port's rule (a request
-- while one is unanswered, the answer's own clock included).
delayPort :: C.HiddenClockResetEnable dom => BS.ByteString -> Word64 -> Int -> RomPort dom
delayPort rom seed maxDelay = C.mealy go (Nothing, seed)
 where
  len = BS.length rom
  go (pend, rng) req =
    let (ans, pend1) = case pend of
          Just (a, 0) -> (Just (fromIntegral (BS.index rom (fromIntegral a `mod` len))), Nothing)
          Just (a, k) -> (Nothing, Just (a, k - 1 :: Int))
          Nothing -> (Nothing, Nothing)
        rng' = splitmix rng
        delay = fromIntegral (rng' `shiftR` 40) `mod` (maxDelay + 1)
        pend2 = case req of
          Just a | Just _ <- pend -> error ("delayPort: request for " <> show a <> " while one is unanswered")
                 | otherwise -> Just (a, delay)
          Nothing -> pend1
    in ((pend2, rng'), ans)

-- | 'gbCoreWith' behind 'delayPort' (the ROM preloaded, so it starts at
-- once): its M-cycle events for @n@ clocks.
stalledEvents :: Int -> Word64 -> Int -> BS.ByteString -> [McEvent]
stalledEvents n seed maxDelay rom = catMaybes (map koEvent (C.sampleN n out))
 where
  out :: C.Signal C.System CoreOut
  out = C.withClockResetEnable C.clockGen C.resetGen C.enableGen $
          let (o, _, reqs) = gbCoreWith CoreDmg (delayPort rom seed maxDelay reqs) (pure True) (pure True) (pure 0) (pure (C.repeat 0)) (pure gbcDefaultPal) (pure Nothing)
          in o

-- | Clocks per M-cycle, flat out (a dot every clock, ROM answers with no
-- extra delay: DDR cache hits), over @n@ clocks: a histogram of the
-- clocks between consecutive M-cycle ends, minus the 12 of the four
-- dots (3 clocks each). Phase 3's budget in double speed: 2 dots (6
-- clocks) plus this, at most 12 (side track I, step 2).
sequencerCost :: Int -> BS.ByteString -> [(Int, Int)]
sequencerCost n rom = histogram (zipWith (-) (drop 1 ends) ends)
 where
  outs = C.sampleN n out
  ends = [ i | (i, o) <- zip [0 :: Int ..] outs, Just _ <- [koEvent o] ]
  histogram ds = [ (d - 12, length (filter (== d) ds)) | d <- [minimum ds .. maximum ds], d `elem` ds ]
  out :: C.Signal C.System CoreOut
  out = C.withClockResetEnable C.clockGen C.resetGen C.enableGen $
          let (o, _, reqs) = gbCoreWith CoreDmg (delayPort rom 1 0 reqs) (pure True) (pure True) (pure 0) (pure (C.repeat 0)) (pure gbcDefaultPal) (pure Nothing)
          in o

-- | The sequencer's cost per M-cycle beyond the dots (phase 3, step 2):
-- in double speed an M-cycle has 2 dots (6 clocks) and 12 clocks in all,
-- so the mean must stay at most 5 (a clock to spare). Before the ROM
-- hints it was 6.1 to 6.3 (a ROM read waited 2 clocks for the DDR port).
costReport :: TestTree
costReport = testGroup "sequencer cost per M-cycle beyond the 4 dots: mean at most 5 (double speed: 12 clocks)"
  [ t "Blargg cpu_instrs" (romRoot </> "blargg/cpu_instrs/cpu_instrs.gb")
  , t "dmg-acid2" (romRoot </> "dmg-acid2/dmg-acid2.gb")
  , t "Mooneye oam_dma/sources-GS (OAM DMA from everywhere)" (romRoot </> "mooneye/acceptance/oam_dma/sources-GS.gb")
  ]
 where
  t name path = testCaseInfo name $ do
    rom <- loadRomFile path
    let h = sequencerCost 400000 rom
        total = sum (map snd h)
        mean = fromIntegral (sum [ c * k | (c, k) <- h ]) / fromIntegral total :: Double
    assertBool ("mean " <> show mean <> " above 5: " <> show h) (mean <= 5)
    pure ("clocks beyond the dots: " <> show h <> ", mean " <> show mean)

-- | With ROM answers delayed at random (DDR misses), the core makes the
-- same M-cycles as the harness without any: waiting only stops time.
stallTests :: TestTree
stallTests = testGroup "gbCoreWith with random ROM delays (DDR, side track I) = the harness"
  [ t "serial_pass.gb, delays 0 to 3" (asmRoot </> "serial_pass.gb") 200000 1 3
  , t "dmg-acid2, delays 0 to 40" (romRoot </> "dmg-acid2/dmg-acid2.gb") 400000 2 40
  , t "Mooneye mbc5/rom_512kb, delays 0 to 9" (romRoot </> "mooneye/emulator-only/mbc5/rom_512kb.gb") 400000 3 9
  ]
 where
  t name path n0 seed maxD = testCase name $ do
    let n = scaled n0
    rom <- loadRomFile path
    let a = stalledEvents n seed maxD rom
        b = harnessEvents n rom
    assertBool "some M-cycles ran" (length a > n `div` 60)
    case [ (i, x, y) | (i, x, y) <- zip3 [0 :: Int ..] a b, x /= y ] of
      [] -> pure ()
      (i, x, y) : _ -> assertFailure ("M-cycle " <> show i <> ": stalled " <> show x <> ", harness " <> show y)

clashTests :: TestTree
clashTests = testGroup "gbCore in Clash simulation (block RAMs) = the harness, M-cycle for M-cycle"
  [ t "serial_pass.gb, 120000 clocks" (asmRoot </> "serial_pass.gb") 120000
  , t "dmg-acid2, 200000 clocks" (romRoot </> "dmg-acid2/dmg-acid2.gb") 200000
  ]
 where
  t name path n0 = testCase name $ do
    let n = scaled n0
    rom <- loadRomFile path
    let a = clashEvents n rom
        b = harnessEvents n rom
    assertBool "some M-cycles ran" (length a > n `div` 30)
    case [ (i, x, y) | (i, x, y) <- zip3 [0 :: Int ..] a b, x /= y ] of
      -- the same M-cycles. Every ROM read waits a clock (the port's answer
      -- is registered, side track I), and with a dot every clock the core
      -- runs flat out here, so it gets through fewer of them in n clocks;
      -- at the real dot rate the M-cycle's spare clocks absorb the wait
      -- (layer 4's tone.gb checks the pitch)
      [] -> assertBool ("M-cycles: Clash " <> show (length a) <> ", harness " <> show (length b))
                       (length a <= length b && 10 * length a >= 9 * length b)
      (i, x, y) : _ -> assertFailure ("M-cycle " <> show i <> ": Clash " <> show x <> ", harness " <> show y)

-- | Three of the boot ROM's palettes (platform/ps/gbc_palette.zig): BG 28,
-- OBJ 0 4, OBJ 1 5, so a mix-up of the three shows.
threePalettes :: [[Int]]
threePalettes = [ [0x7FFF, 0x7E8C, 0x7C00, 0x0000], [0x7FFF, 0x421F, 0x1CF2, 0x0000], [0x7FFF, 0x5294, 0x294A, 0x0000] ]
