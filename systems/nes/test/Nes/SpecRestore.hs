-- | Layer 0, side track N (PLAN.md N.7): loading a NES state through the
-- 2A03, on the reference model. A machine runs a cartridge to a moment it
-- can be saved at ('specAtRest'); its state table ("Nes.StateTable") goes
-- through the PS's routine builder (build/host/nes-restore, from
-- platform/ps/systems/nes_restore.zig); a fresh machine with the same
-- cartridge gets its memories (as the PS writes them through its windows)
-- and runs the routine through the ROM overlay ('runOverlay'). Then:
--
-- * its table is the saved one, field by field, except what the routine
--   can't put back (the PPU's sprite flags, the APU's length
--   counters, the DMC's output shift register, silence flag and IRQ
--   flag, MMC5's IRQ pending, in-frame flag and line
--   counter, the 6502's B flag; PPUCTRL's nametable bits where t's
--   differ; a cycle of the frame counter's phase if it was never reset);
-- * every memory is the saved one (RAM, nametable RAM, OAM, PRG RAM, CHR
--   RAM, ExRAM);
-- * both machines go on alike: the registers after every instruction,
--   then the next frames' picture.
module Nes.SpecRestore (restoreTests) where

import Prelude

import Control.Monad (filterM, replicateM, unless, when)
import Control.Monad.ST (stToIO)
import Data.Bits (shiftR, (.&.))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.List (intercalate)
import qualified Data.Vector.Unboxed as U
import Data.Word (Word16, Word8)
import Numeric (showHex)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.Exit (ExitCode (..))
import System.FilePath ((</>), takeBaseName)
import System.Process (readProcessWithExitCode)
import Test.Tasty
import Test.Tasty.Golden (goldenVsString)
import Test.Tasty.HUnit

import Mos6502.Spec (Regs (..), getRegs)
import Nes.Spec.Machine
import Nes.Spec.Ppu (ppuFrame)
import Nes.Spec.SaveState
import Nes.StateTable
import Nes.TestRoms

tool, scratch :: FilePath
tool = "../../build/host/nes-restore"
scratch = "../../build/nes/restore-test"

-- | Fields compared loosely, whatever the table says.
loose :: [Int]
loose = [tDmcIrq, tDmcShift, tDmcSilence] <> [tLengths .. tLengths + 3] <> [tMmc5 + 28, tMmc5 + 29, tMmc5Line]

-- | The registers a run is compared by (P without B and bit 5).
regsOf :: Regs -> [Int]
regsOf r = map fromIntegral [rA r, rX r, rY r, rS r, rP r .&. 0xCF] <> [fromIntegral (rPC r)]

buildOverlay :: String -> U.Vector Word8 -> Images -> IO (U.Vector Word8, Word16)
buildOverlay name tbl im = do
  have <- doesFileExist tool
  unless have (assertFailure (tool <> " is missing: make nes-restore"))
  createDirectoryIfMissing True scratch
  let f ext = scratch </> (name <> "." <> ext)
      save ext v = BS.writeFile (f ext) (BS.pack (U.toList v))
  save "table" tbl
  save "ram" (imRam im)
  save "ciram" (imCiram im)
  save "oam" (imOam im)
  (code, _, err) <- readProcessWithExitCode tool [f "table", f "ram", f "ciram", f "oam", f "overlay"] ""
  when (code /= ExitSuccess) (assertFailure ("nes-restore: " <> err))
  ovl <- U.fromList . BS.unpack <$> BS.readFile (f "overlay")
  case words err of
    ["end", e, "cycles", _] -> pure (ovl, read e)
    _ -> assertFailure ("nes-restore said: " <> err) >> pure (ovl, 0)

-- | Run n steps, on to a moment at rest; restore a fresh machine;
-- compare; go on k steps on both.
restoreCheck :: FilePath -> String -> Int -> Int -> Assertion
restoreCheck file name n k = do
  rom <- loadRomFile file
  m1 <- stToIO (newMachine rom) >>= either (\e -> assertFailure e >> fail e) pure
  stToIO (sequence_ (replicate n (stepCpu m1)))
  let toRest i = do
        r <- stToIO (specAtRest m1)
        if r || i > 300000 then pure r else stToIO (stepCpu m1) >> toRest (i + 1 :: Int)
  rest <- toRest 0
  unless rest $ do
    rs <- stToIO (mapM (const (stepCpu m1 >> restReasons m1)) [1 .. 20000 :: Int])
    assertFailure ("no moment at rest; per condition (vblank, PPU, APU, machine), steps of 20000 it held: "
                   <> show (map (length . filter id) (foldr (zipWith (:)) (repeat []) rs)))
  tbl <- stToIO (specTable m1)
  im <- stToIO (images m1)
  (ovl, end) <- buildOverlay name tbl im
  r <- stToIO (runOverlay rom im ovl end 400000)
  m2 <- either (\e -> assertFailure e >> fail e) (pure . fst) r
  tbl2 <- stToIO (specTable m2)
  let at o = tbl U.! o
      tNt = (fromIntegral (at (tT + 1)) `shiftR` 2) .&. 3 :: Int
      ctrlMask = if fromIntegral (at tCtrl) .&. 3 /= tNt then 0xFC else 0xFF
      masked o v
        | o == tP = v .&. 0xCF
        | o == tStatus = v .&. 0x80
        | o == tCtrl = v .&. ctrlMask
        | otherwise = v
      -- a frame counter never reset since power-on: its phase no write
      -- reaches, one cycle off (cycles to the reset even: odd after a write)
      seqOf tb' = fromIntegral (tb' U.! tSeq) + 256 * fromIntegral (tb' U.! (tSeq + 1)) :: Int
      powerOnPhase = even (fromIntegral (at tParity) - seqOf tbl)
      seqOk = seqOf tbl2 == seqOf tbl || (powerOnPhase && abs (seqOf tbl2 - seqOf tbl) == 1)
      loose' = loose <> [tSeq, tSeq + 1]
      diffs = [ (o, at o, tbl2 U.! o) | o <- [0 .. tableSize - 1], o `notElem` loose', masked o (at o) /= masked o (tbl2 U.! o) ]
               <> [ (tSeq, at tSeq, tbl2 U.! tSeq) | not seqOk ]
      fieldOf o = case [nm | (nm, fo, sz) <- tableFields, o >= fo, o < fo + sz] of
        nm : _ -> nm
        [] -> "?"
  unless (null diffs) $ assertFailure $ "restored table differs: " <> intercalate ", "
    [ fieldOf o <> "@" <> showHex o "" <> " saved " <> showHex a "" <> " restored " <> showHex b "" | (o, a, b) <- take 12 diffs ]
  im2 <- stToIO (images m2)
  let memDiff nm a b = [ nm <> " differs at " <> showHex i "" | Just i <- [U.findIndex id (U.zipWith (/=) a b)] ]
                       <> [ nm <> "'s size differs" | U.length a /= U.length b ]
      ds = concat [ memDiff "RAM" (imRam im) (imRam im2), memDiff "CIRAM" (imCiram im) (imCiram im2)
                  , memDiff "OAM" (imOam im) (imOam im2), memDiff "PRG RAM" (imPrgRam im) (imPrgRam im2)
                  , memDiff "CHR RAM" (imChrRam im) (imChrRam im2), memDiff "ExRAM" (imExram im) (imExram im2) ]
  unless (null ds) (assertFailure (intercalate "; " ds))
  t1 <- stToIO (replicateM k (stepCpu m1 >> (regsOf <$> getRegs (mCpu m1))))
  t2 <- stToIO (replicateM k (stepCpu m2 >> (regsOf <$> getRegs (mCpu m2))))
  case [ (i, x, y) | (i, x, y) <- zip3 [0 :: Int ..] t1 t2, x /= y ] of
    (i, x, y) : _ -> do
      ta <- stToIO (specTable m1)
      tb' <- stToIO (specTable m2)
      let d = [ fieldOf o' <> "@" <> showHex o' "" <> " " <> showHex a "" <> "/" <> showHex b "" | (o', a, b) <- zip3 [0 :: Int ..] (U.toList ta) (U.toList tb'), a /= b ]
      assertFailure ("the runs part at step " <> show i <> ":\n  " <> show x <> "\n  " <> show y <> "\n  tables after: " <> unwords d)
    [] -> pure ()
  stToIO (runToFrame m1 >> runToFrame m1 >> runToFrame m2 >> runToFrame m2)
  f1 <- stToIO (ppuFrame (mPpu m1))
  f2 <- stToIO (ppuFrame (mPpu m2))
  assertBool "the frame differs" (f1 == f2)

-- | A cartridge saved at three moments (savestate.nes: one in each of
-- its frame counter setups, 16 frames apart).
moments :: FilePath -> String -> TestTree
moments file what = testGroup (takeBaseName file <> " (" <> what <> ")")
  [ testCase ("after " <> show n <> " steps") $ restoreCheck file (takeBaseName file <> "-" <> show n) n 60000
  | n <- steps ]
 where
  steps
    -- (savestate.nes: its first $4017 write at frame 16, after which the
    -- frame counter's phase is one a write reaches; then 16 frames each of
    -- 5-step, 4-step unacknowledged, acknowledged, inhibited)
    | takeBaseName file == "savestate" = [170000, 220000, 330000, 380000, 490000, 540000, 650000, 700000 :: Int]
    | otherwise = [200000, 300000, 400000]

-- | The cases, those with a missing file (the user's own games) left out.
restoreTests :: IO TestTree
restoreTests = do
  let cases =
        [ (asmRoot </> "savestate.nes", "w, the strobe, a full DMC buffer, the frame counter's setups")
        , (asmRoot </> "ppu_scene.nes", "the PPU's registers, scrolling, sprites")
        , (asmRoot </> "tone.nes", "the APU")
        , (asmRoot </> "mmc3_bg1000.nes", "MMC3's IRQ")
        , (asmRoot </> "mmc5.nes", "MMC5")
        , (romRoot </> "homebrew" </> "thwaite.nes", "NROM, a game")
        , (romRoot </> "homebrew" </> "dabg.nes", "NROM, CHR RAM")
        , (romRoot </> "holy-mapperel" </> "M1_P128K_C128K.nes", "MMC1")
        , (romRoot </> "holy-mapperel" </> "M1_P128K_CR8K.nes", "MMC1, CHR RAM")
        , (romRoot </> "holy-mapperel" </> "M2_P128K_V.nes", "UxROM")
        , (romRoot </> "holy-mapperel" </> "M3_P32K_C32K_H.nes", "CNROM")
        , (romRoot </> "holy-mapperel" </> "M4_P256K_C256K.nes", "MMC3")
        , (romRoot </> "holy-mapperel" </> "M7_P128K.nes", "AxROM")
        , (romRoot </> "games" </> "local" </> "mmc3-game.nes", "MMC3, the user's")
        , (romRoot </> "games" </> "local" </> "mmc1-chr-ram-game.nes", "MMC1, CHR RAM, the user's")
        ]
  have <- filterM (doesFileExist . fst) cases
  pure $ testGroup "loading through the CPU (side track N, N.7)" $
    goldenVsString "the state table's layout" "test/golden/state-table.txt"
      (pure $ BL.pack $ unlines $ ("size 0x" <> showHex tableSize "") : [ nm <> " 0x" <> showHex o "" <> " " <> show sz | (nm, o, sz) <- tableFields ])
    : [ moments f w | (f, w) <- have ]
