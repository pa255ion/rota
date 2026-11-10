-- | Layer 0. Step 7.0: the cartridge header: our ROMs (build/snes/asm,
-- make snes-asm) and every fetched cartridge parse to their known mapping,
-- sizes and battery; a copier header is skipped; the checksum follows
-- fullsnes's rule for sizes that aren't a power of two. Step 7.3: the
-- machine without video and sound ("Snes.Spec"): our ROMs pass (RESULT 01h
-- at STP), gilyon's cputest shows "Success", krom's CPU tests show PASS
-- and no FAIL, a frame is 357,368 master clocks (357,364 every other).
module Main (main) where

import SpecRestore (restoreTests)
import Prelude

import Control.Monad (forM, unless)
import Data.Bits (xor)
import Numeric (showHex)
import System.Environment (lookupEnv)
import Control.Monad.ST (stToIO)
import qualified Data.ByteString as B
import Data.List (isInfixOf)
import Data.Word (Word64, Word8)
import System.Directory (doesFileExist)
import System.FilePath ((</>), takeBaseName)
import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Vector.Unboxed as U

import DisplayTest
import DspTest
import Upd77c25Test
import Snes.Cart
import Snes.Spec

-- | Relative to the package directory, where cabal runs the tests.
asmDir, romDir, iplFile :: FilePath
asmDir = "../../build/snes/asm"
romDir = "../../roms/snes"
-- | Our SPC700 boot ROM (make snes-asm).
iplFile = "../../build/snes/ipl.bin"

main :: IO ()
main = do
 rt <- restoreTests
 dsp1 <- upd77c25Tests
 defaultMain $ testGroup "snes-spec" $ (<> [rt, dsp1])
  [ testGroup "cartridge header"
      [ testCase "lorom.sfc: LoRom, 32 KiB, checksum good" $ do
          c <- cart (asmDir </> "lorom.sfc")
          (cartMapping c, cartRomSize c, cartChecksumOk c, cartTitle c, cartBattery c)
            @?= (LoRom, 32768, True, "M7 LOROM TEST        ", False)
      , testCase "hirom.sfc: HiRom, 64 KiB, checksum good" $ do
          c <- cart (asmDir </> "hirom.sfc")
          (cartMapping c, cartRomSize c, cartChecksumOk c, cartHeaderAt c)
            @?= (HiRom, 65536, True, 0xFFC0)
      , testCase "a copier header is skipped" $ do
          b <- readRom (asmDir </> "hirom.sfc")
          parseCart (B.replicate 512 0 <> b) @?= parseCart b
      , testCase "checksum of 96 KiB: 64 KiB + the last 32 KiB twice" $ do
          let rom = B.replicate 65536 1 <> B.replicate 32768 2
          checksum rom @?= fromIntegral ((65536 * 1 + 2 * 32768 * 2) `mod` 65536 :: Int)
      , testCase "the map mode decides between two otherwise equal headers" $ do
          let rom mode = twoHeaders mode
          fmap cartMapping (parseCart (rom 0x21)) @?= Just HiRom
          fmap cartMapping (parseCart (rom 0x20)) @?= Just LoRom
          fmap cartFastRom (parseCart (rom 0x31)) @?= Just True
      , testCase "too small for a header" $
          parseCart (B.replicate 1024 0) @?= Nothing
      ]
  , testGroup "the machine: our ROMs (RESULT 01h at STP)"
      [ testCase r $ do
          (stopped, res, _) <- runRom (asmDir </> (r <> ".sfc")) 60 pad
          (stopped, res) @?= (True, 1)
      | (r, pad) <- [ ("lorom", 0), ("hirom", 0), ("map", 0), ("muldiv", 0), ("dma", 0)
                    , ("nmi", 0), ("virq", 0), ("sram", 0), ("save", 0), ("joy", 0x0A5), ("ppuregs", 0) ] ]
  , testCase "the BG scrolls' two writes (scroll.s; step 7.12: Rockman & Forte's scroll jumped by 4)" $ do
      file <- readRom (asmDir </> "scroll.sfc")
      ipl <- readRom iplFile
      c <- maybe (assertFailure "scroll.sfc: no header" >> fail "") pure (parseCart file)
      regs <- stToIO $ do
        m <- newMachine ipl c file
        reset m
        _ <- runUntilStopped m 3
        ppuRegisters m
      map (regs !!) [0x0D, 0x0E, 0x0F, 0x11] @?= [0x106, 0x134, 0x20D, 0x255]
  , testCase "the colour window forcing black (winblack.s; step 7.12: a game's text box was missing)" $ do
      file <- readRom (asmDir </> "winblack.sfc")
      ipl <- readRom iplFile
      c <- maybe (assertFailure "winblack.sfc: no header" >> fail "") pure (parseCart file)
      pic <- stToIO $ do
        m <- newMachine ipl c file
        reset m
        runFrames m 4
        frameColors m
      -- row 100, each x's left half-dot
      let at x = pic U.! (100 * 512 + 2 * x)
      map at [32, 63, 64, 128, 191, 192, 250] @?= [0x7FFF, 0x7FFF, 0, 0, 0, 0x7FFF, 0x7FFF]
  , testGroup "the machine: gilyon's cputest"
      [ testCase f $ do
          (_, _, text) <- runRom (romDir </> "gilyon" </> f) 900 0
          unless (any ("Success" `isInfixOf`) text) $ assertFailure (unlines text)
      | f <- ["cputest-basic.sfc", "cputest-full.sfc"] ]
  , testGroup "the machine: krom's CPU tests"
      [ testCase f $ do
          (_, _, text) <- runRom (romDir </> "krom" </> f) 120 0
          unless (any ("PASS" `isInfixOf`) text && not (any ("FAIL" `isInfixOf`) text)) $
            assertFailure (unlines text)
      | f <- kromCpu ]
  , testGroup "the sound module (step 7.4)"
      [ dspTests
      , displayTests
      , traceTests
      , testCase "gilyon's spctest: Success (through our boot ROM)" $ do
          (_, _, text) <- runRom (romDir </> "gilyon/spctest.sfc") 1200 0
          unless (any ("Success" `isInfixOf`) text) $ assertFailure (unlines text)
      , testGroup "krom's SPC700 tests"
          [ testCase f $ do
              (_, _, text) <- runRom (romDir </> "krom" </> f) 200 0
              unless (any ("PASS" `isInfixOf`) text && not (any ("FAIL" `isInfixOf`) text)) $
                assertFailure (unlines text)
          | f <- [ "SPC700" <> t <> ".sfc" | t <- words "ADC AND DEC EOR INC ORA SBC" ] ]
      , testCase "tone.s: 1000 Hz from the DSP (the 65816 uploads it through the boot ROM)" $ do
          (res, audio, mc) <- runAudio (asmDir </> "tone.sfc") 40
          res @?= 1
          -- a sample every 32 SPC700 cycles, 5632 / 118125 a master clock
          -- (all of them: the buffer once gave only its first half)
          let want = (mc * 5632 `div` 118125) `div` 32
          (abs (U.length audio `div` 2 - want) <= 1) @? (show (U.length audio `div` 2) <> " samples, " <> show want <> " expected")
          let left = [ audio U.! i | i <- [0, 2 .. U.length audio - 2] ]
              rising = [ i | (i, (a, b)) <- zip [0 :: Int ..] (zip left (drop 1 left)), a < 0, b >= 0 ]
          case rising of
            r0 : rest@(_ : _) -> do
              let r1 = last rest
                  hz = fromIntegral (length rest) * 32000 / fromIntegral (r1 - r0) :: Double
              (abs (hz - 1000) < 1) @? ("tone at " <> show hz <> " Hz")
            _ -> assertFailure "no tone"
      ]
  , pictureTests
  , testCase "a frame: 357368 master clocks, then 357364 (the short line)" $ do
      file <- readRom (romDir </> "gilyon/cputest-basic.sfc")
      ipl <- readRom iplFile
      Just c <- pure (parseCart file)
      ds <- stToIO $ do
        m <- newMachine ipl c file
        reset m
        runFrames m 2
        forM [1 .. 4 :: Int] $ \_ -> do
          a <- frameStart m
          runFrames m 1
          b <- frameStart m
          pure (b - a)
      unless (ds == take 4 (cycle [357368, 357364]) || ds == take 4 (cycle [357364, 357368])) $
        assertFailure (show ds)
  , testGroup "fetched cartridges (tools/fetch-roms.sh)"
      [ testCase f $ do
          c <- cart (romDir </> f)
          (cartMapping c, cartRomSize c `div` 1024, cartBattery c, cartSramSize c `div` 1024, cartChip c)
            @?= (m, kib, bat, sram, Nothing)
      | (f, m, kib, bat, sram) <- known ]
  ]

kromCpu :: [FilePath]
kromCpu = [ "CPU" <> t <> ".sfc" | t <- words "ADC AND ASL BIT BRA CMP DEC EOR INC JMP LDR LSR MOV MSC ORA PHL PSR RET ROL ROR SBC STR TRN" ]

-- | Runs a cartridge on the model up to STP or @n@ frames, pad 1 held as
-- given: whether it stopped, RESULT (7E0000h), BG1's text.
runRom :: FilePath -> Int -> Int -> IO (Bool, Word8, [String])
runRom p n pad = do
  file <- readRom p
  ipl <- readRom iplFile
  c <- maybe (assertFailure (p <> ": no header") >> fail "") pure (parseCart file)
  stToIO $ do
    m <- newMachine ipl c file
    reset m
    setPads m pad 0
    s <- runUntilStopped m n
    r <- readWram m 0
    t <- screenText m
    pure (s, r, t)

-- | Step 7.5: the picture. Each ROM run so many frames, its frame's hash
-- (FNV-1a over the visible lines' 512 15-bit colours) against its file in
-- test/golden/frames/, each frozen after a check by eye (and, for the
-- OpenSNES examples, against the authors' screenshots: snes.md, "Status
-- log"). SNES_GOLDEN_WRITE=1 writes the files instead.
pictureTests :: TestTree
pictureTests = testGroup "the picture (frame hashes, test/golden/frames/)"
  [ testCase (f <> " at frame " <> show n) $ do
      write <- (== Just "1") <$> lookupEnv "SNES_GOLDEN_WRITE"
      h <- frameHashOf (romDir </> f) n
      let g = "test/golden/frames" </> (takeBaseName f <> "-" <> show n <> ".txt")
      if write then writeFile g (h <> "\n") else do
        want <- takeWhile (/= '\n') <$> readFile g
        h @?= want
  | (f, n) <- goldenRoms ]

-- | The ROMs and frame counts whose pictures are frozen.
goldenRoms :: [(FilePath, Int)]
goldenRoms =
  [ ("krom/8x8BG1Map2BPP32x328PAL.sfc", 60), ("krom/8x8BG4Map2BPP32x328PAL.sfc", 60)
  , ("krom/8x8BGMap4BPP32x328PAL.sfc", 60), ("krom/8x8BGMap8BPP32x32.sfc", 60)
  , ("krom/8x8BGMapTileFlip.sfc", 60), ("krom/HiColor64PerTileRow.sfc", 60)
  , ("krom/HiColor64PerTileRowPseudoHiRes.sfc", 60), ("krom/Mode7HDMA.sfc", 60)
  , ("krom/RedSpaceIndirectHDMA.sfc", 60), ("krom/Rings.sfc", 60), ("krom/RotZoom.sfc", 60)
  , ("krom/WindowMultiHDMA.sfc", 60), ("krom/InterlaceFont.sfc", 60)
  , ("opensnes/extbg.sfc", 60), ("opensnes/hdma_indirect_gradient.sfc", 30)
  , ("opensnes/mode5_hires.sfc", 120), ("opensnes/mode6.sfc", 120), ("opensnes/pseudo_hires.sfc", 30)
  , ("opensnes/mode7_flying.sfc", 30), ("opensnes/perspective_rotate.sfc", 30)
  , ("opensnes/simple_sprite.sfc", 30), ("opensnes/sprite_sizes.sfc", 120)
  , ("opensnes/direct_color.sfc", 60), ("opensnes/window.sfc", 120), ("opensnes/shadow_tint.sfc", 120)
  , ("opensnes/text_glyphs.sfc", 30), ("opensnes/shmup_1942.sfc", 60)
  , ("games/unnamed-tech-demo-v3.sfc", 120), ("games/horizontal-shooter.sfc", 60)
  ]

frameHashOf :: FilePath -> Int -> IO String
frameHashOf p n = do
  file <- readRom p
  ipl <- readRom iplFile
  c <- maybe (assertFailure (p <> ": no header") >> fail "") pure (parseCart file)
  (fr, lns) <- stToIO $ do
    m <- newMachine ipl c file
    reset m
    runFrames m n
    (,) <$> frameColors m <*> visibleLines m
  let h = U.foldl' (\acc w -> (acc `xor` fromIntegral w) * 0x100000001b3) (0xcbf29ce484222325 :: Word64) (U.take (512 * lns) fr)
  pure (let s = showHex h "" in replicate (16 - length s) '0' <> s)

-- | Runs a cartridge @n@ frames: RESULT, the DSP's samples (L, R, ...)
-- and the master clocks run.
runAudio :: FilePath -> Int -> IO (Word8, U.Vector Int, Int)
runAudio p n = do
  file <- readRom p
  ipl <- readRom iplFile
  c <- maybe (assertFailure (p <> ": no header") >> fail "") pure (parseCart file)
  stToIO $ do
    m <- newMachine ipl c file
    reset m
    runFrames m n
    (,,) <$> readWram m 0 <*> audioSamples m <*> masterClocks m

-- | The fetched cartridges and what their headers say (read 2026-10-15
-- with snes-cart, checked against each project's README where it says).
known :: [(FilePath, Mapping, Int, Bool, Int)]
known =
  [ ("gilyon/cputest-full.sfc", LoRom, 256, False, 0)
  , ("gilyon/cputest-basic.sfc", LoRom, 256, False, 0)
  , ("gilyon/spctest.sfc", LoRom, 128, False, 0)
  , ("opensnes/shmup_1942.sfc", LoRom, 256, False, 0)
  , ("opensnes/save_game.sfc", LoRom, 256, True, 8)
  , ("opensnes/hirom_demo.sfc", HiRom, 512, False, 0)
  , ("opensnes/mode7_racing.sfc", LoRom, 256, False, 0)
  , ("games/unnamed-tech-demo-v3.sfc", LoRom, 256, False, 0)
  , ("games/horizontal-shooter.sfc", HiRom, 128, False, 0)
  , ("krom/CPUADC.sfc", LoRom, 32, False, 0)
  , ("krom/SPC700ADC.sfc", LoRom, 32, False, 0)
  , ("krom/8x8BGMap8BPP32x32.sfc", LoRom, 96, False, 0)
  , ("krom/RotZoom.sfc", LoRom, 32, False, 0)
  ]

-- | 64 KiB with the same header (bad checksum, map mode @mode@) at 7FC0h and
-- FFC0h, so only the mode byte can tell them apart.
twoHeaders :: Word8 -> B.ByteString
twoHeaders mode = B.concat [B.replicate 0x7FC0 0, hdr, B.replicate (0xFFC0 - 0x8000) 0, hdr]
 where
  hdr = B.pack (map (fromIntegral . fromEnum) "TWO HEADERS          "
                <> [mode, 0, 0x06, 0, 1, 0, 0, 0, 0, 0, 0]
                <> replicate 28 0 <> [0x00, 0x80, 0, 0])

readRom :: FilePath -> IO B.ByteString
readRom p = do
  ok <- doesFileExist p
  unless ok (assertFailure (p <> " missing: run make snes-asm / tools/fetch-roms.sh"))
  B.readFile p

cart :: FilePath -> IO Cart
cart p = do
  b <- readRom p
  maybe (assertFailure (p <> ": no header") >> fail "") pure (parseCart b)
