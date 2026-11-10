-- | Layer 0, M7 step 7.15a: loading a Super Nintendo state through the
-- 65C816, on the reference model. A machine runs a cartridge to a moment
-- it can be saved at ('specAtRest'); its state table ("Snes.Spec.SaveState")
-- goes through the PS's routine builder (build/host/snes-restore, from
-- platform/ps/systems/snes_restore.zig); a fresh machine with the same
-- cartridge gets its memories (as the PS writes them while it holds the
-- core) and runs the routine from the ROM overlay ('runOverlay'; the sound
-- module moved whole until 7.15b). Then:
--
-- * its table is the saved one, field by field, but what the routine
--   can't put back ('loose');
-- * every memory is the saved one;
-- * both machines go on alike: the registers and the beam after every
--   instruction, then the next frames' pictures.
module SpecRestore (restoreTests) where

import Prelude

import Control.Monad (filterM, replicateM, unless, when)
import Control.Monad.ST (stToIO)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.List (intercalate)
import qualified Data.Vector.Unboxed as U
import Data.Word (Word8)
import Numeric (showHex)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.Exit (ExitCode (..))
import System.FilePath ((</>), takeBaseName)
import System.Process (readProcessWithExitCode)
import Test.Tasty
import Test.Tasty.Golden (goldenVsString)
import Test.Tasty.HUnit

import Snes.Cart (parseCart)
import Snes.Spec.Apu (apuCpuRegs, apuRam)
import Snes.Spec.Machine
import Snes.Spec.SaveState
import qualified W65c816.Spec as Cpu
import W65c816.Types (Regs (..))

tool, scratch, romDir, iplFile :: FilePath
tool = "../../build/host/snes-restore"
scratch = "../../build/snes/restore-test"
romDir = "../../roms/snes"
iplFile = "../../build/snes/ipl.bin"

-- | Fields the routine can't put back (snes_restore.zig's header).
loose :: [String]
loose = ["mdr", "vlatch", "stat77", "hdmaDo", "hdmaDone", "joy1", "joy2", "joy3", "joy4", "ser1", "ser2"]
        <> ["ppu04", "ppu18", "ppu19", "ppu22"]
        -- (the address registers as last written: VRAM's and CGRAM's move on
        -- with their data ports; their addresses are the slots)
        <> ["ppu16", "ppu17", "ppu21"]

fieldAt :: Int -> String
fieldAt o = case [ nm | (nm, fo, sz) <- tableFields, o >= fo, o < fo + sz ] of
  nm : _ -> nm
  [] -> "?"

-- | The registers and the beam a run is compared by.
traceOf :: Machine s -> Regs -> Int -> Int -> [Int]
traceOf _ r hc vc = map fromIntegral [rA r, rX r, rY r, rS r, rD r] <> map fromIntegral [rDBR r, rPBR r]
                    <> [fromIntegral (rPC r), fromIntegral (rP r), hc, vc]

buildOverlay :: String -> U.Vector Word8 -> Images -> IO (U.Vector Word8, Int, [(Int, (Int, Int, Int))])
buildOverlay name tbl im = do
  have <- doesFileExist tool
  unless have (assertFailure (tool <> " is missing: make snes-restore"))
  createDirectoryIfMissing True scratch
  let f ext = scratch </> (name <> "." <> ext)
  B.writeFile (f "table") (B.pack (U.toList tbl))
  -- (the slot's bytes the routine writes: the DMA registers, OAM's high table)
  B.writeFile (f "fill") (B.pack (U.toList (imDma im) <> U.toList (U.drop 512 (imOam im))))
  (code, out, err) <- readProcessWithExitCode tool [f "table", f "fill", f "overlay"] ""
  when (code /= ExitSuccess) (assertFailure ("snes-restore: " <> err))
  ovl <- U.fromList . B.unpack <$> B.readFile (f "overlay")
  -- the generator's checkpoints: where it expects the machine to be
  let marks = [ (read pc, (read fr, read v, read h)) | ["mark", pc, fr, v, h] <- map words (lines out) ]
  case words err of
    ["end", e, "cycles", _] -> pure (ovl, read e, marks)
    _ -> assertFailure ("snes-restore said: " <> err) >> pure (ovl, 0, marks)

-- | Run n instructions, on to a moment at rest; restore a fresh machine;
-- compare; go on k instructions on both.
restoreCheck :: FilePath -> String -> Moment -> Int -> Assertion
restoreCheck path name at k = do
  file <- B.readFile path
  ipl <- B.readFile iplFile
  cart <- maybe (assertFailure (path <> ": no header") >> fail "") pure (parseCart file)
  m1 <- stToIO (newMachine ipl cart file)
  stToIO (reset m1)
  stToIO $ case at of
    Steps n -> sequence_ (replicate n (stepCpu m1))
    Frames f -> runFrames m1 f
  let toRest i = do
        r <- stToIO (specAtRest m1)
        if r || i > 400000 then pure r else stToIO (stepCpu m1) >> toRest (i + 1 :: Int)
  rest <- toRest 0
  unless rest (assertFailure "no moment at rest")
  tbl <- stToIO (specTable m1)
  im <- stToIO (images m1)
  (ovl, end, marks) <- buildOverlay name tbl im
  (r, hits) <- stToIO (runOverlay ipl cart file m1 im ovl end 2000000 (map fst marks))
  -- (on a failure: the checkpoints, expected and met)
  let timeline = "\n  checkpoints (PC: expected frame, line, clock / met): " <> intercalate "; "
        [ showHex pc "" <> ": " <> show ex <> " / " <> maybe "never" show (lookup pc hits) | (pc, ex) <- marks ]
  m2 <- either (\e -> assertFailure (e <> timeline) >> fail e) pure r
  -- a state in WAI: the RTI returned to its WAI, which runs once more
  waiting <- stToIO ((== Cpu.Waiting) <$> Cpu.cpuRun (mCpu m1))
  when waiting $ stToIO (stepCpu m2)
  -- the samples made so far (the sound module caught up): what each makes
  -- from here is compared
  n1 <- U.length <$> stToIO (audioSamples m1)
  n2 <- U.length <$> stToIO (audioSamples m2)
  tbl2 <- stToIO (specTable m2)
  let diffs = [ (o, tbl U.! o, tbl2 U.! o) | o <- [0 .. tableSize - 1], fieldAt o `notElem` loose, tbl U.! o /= tbl2 U.! o ]
  unless (null diffs) $ assertFailure $ "restored table differs: " <> intercalate ", "
    [ fieldAt o <> "@" <> showHex o "" <> " saved " <> showHex a "" <> " restored " <> showHex b "" | (o, a, b) <- take 12 diffs ]
    <> timeline
  im2 <- stToIO (images m2)
  let memDiff nm a b = [ nm <> " differs at " <> showHex i "" | Just i <- [U.findIndex id (U.zipWith (/=) a b)] ]
      ds = concat [ memDiff "WRAM" (imWram im) (imWram im2), memDiff "VRAM" (imVram im) (imVram im2)
                  , memDiff "CGRAM" (imCgram im) (imCgram im2), memDiff "OAM" (imOam im) (imOam im2)
                  , memDiff "DMA" (imDma im) (imDma im2), memDiff "SRAM" (imSram im) (imSram im2) ]
  unless (null ds) (assertFailure (intercalate "; " ds))
  let run m = stToIO $ replicateM k $ do
        stepCpu m
        rg <- Cpu.getRegs (mCpu m)
        traceOf m rg <$> slotGet m sHc <*> slotGet m sVc
  t1 <- run m1
  t2 <- run m2
  case [ (i, x, y) | (i, x, y) <- zip3 [0 :: Int ..] t1 t2, x /= y ] of
    (i, x, y) : _ -> assertFailure ("the runs part at instruction " <> show i <> ":\n  " <> show x <> "\n  " <> show y)
    [] -> pure ()
  stToIO (runFrames m1 2 >> runFrames m2 2)
  f1 <- stToIO (frameColors m1)
  f2 <- stToIO (frameColors m2)
  assertBool "the next frames' pictures differ" (f1 == f2)
  -- the sound: the samples made after the restore, the SPC700, ARAM
  s1 <- U.drop n1 <$> stToIO (audioSamples m1)
  s2 <- U.drop n2 <$> stToIO (audioSamples m2)
  unless (s1 == s2) $ assertFailure ("the sound after the restore differs: " <> show (U.length s1) <> " and " <> show (U.length s2)
                                     <> " sample values, first apart at " <> show (U.findIndex id (U.zipWith (/=) s1 s2)))
  r1 <- stToIO (apuCpuRegs (apuOf m1))
  r2 <- stToIO (apuCpuRegs (apuOf m2))
  r1 @?= r2
  a1 <- stToIO (mapM (apuRam (apuOf m1)) [0 .. 0xFFFF])
  a2 <- stToIO (mapM (apuRam (apuOf m2)) [0 .. 0xFFFF])
  unless (a1 == a2) $ assertFailure ("ARAM differs at " <> show (length (takeWhile id (zipWith (==) a1 a2))))

-- | Where a state is taken: the first moment at rest after so many
-- instructions, or frames (our test ROMs stop soon: STP).
data Moment = Steps Int | Frames Int

-- | A cartridge saved at two moments.
moments :: FilePath -> String -> [Moment] -> TestTree
moments path what ms = testGroup (takeBaseName path <> " (" <> what <> ")")
  [ testCase (label at) $ restoreCheck path (takeBaseName path <> "-" <> tag at) at 100000 | at <- ms ]
 where
  label (Steps n) = "after " <> show n <> " instructions"
  label (Frames f) = "in frame " <> show f <> "'s V-blank"
  tag (Steps n) = show n
  tag (Frames f) = "f" <> show f

-- | The cases, those with a missing file left out.
restoreTests :: IO TestTree
restoreTests = do
  let os = romDir </> "opensnes"
      kr = romDir </> "krom"
      asm = "../../build/snes/asm"
      cases =
        [ (asm </> "stateheld.sfc", "WRIO, the strobe, CGRAM's flip-flop, TIMEUP held")
        , (asm </> "statecleared.sfc", "the same, TIMEUP read clear")
        , (os </> "continuous_scroll.sfc", "scrolling")
        , (os </> "parallax_scroll.sfc", "BG scrolls, written twice")
        , (kr </> "InterlaceScroll.sfc", "interlace")
        , (os </> "mode7_flying.sfc", "Mode 7")
        , (kr </> "Mode7HDMA.sfc", "Mode 7, HDMA")
        , (os </> "hdma_wave.sfc", "HDMA")
        , (os </> "window_multi_hdma.sfc", "windows, HDMA")
        , (os </> "sprite_swarm.sfc", "sprites, OAM")
        , (os </> "palette_cycle.sfc", "CGRAM")
        , (os </> "fading.sfc", "INIDISP")
        , (os </> "mode5_hires.sfc", "hi-res")
        , (os </> "controller.sfc", "the pads")
        , (os </> "save_game.sfc", "SRAM")
        , (os </> "echo.sfc", "sound")
        , (os </> "breakout.sfc", "a game")
        , (os </> "tetris.sfc", "a game")
        , (os </> "shmup_1942.sfc", "a game")
        ]
  have <- filterM (doesFileExist . fst) cases
  pure $ testGroup "loading through the CPU (step 7.15a)" $
    goldenVsString "the state table's layout" "test/golden/state-table.txt"
      (pure $ BL.pack $ unlines $ ("size 0x" <> showHex tableSize "") : [ nm <> " 0x" <> showHex o "" <> " " <> show sz | (nm, o, sz) <- tableFields ])
    : [ moments f w [Steps 150000, Steps 400000] | (f, w) <- have ]
