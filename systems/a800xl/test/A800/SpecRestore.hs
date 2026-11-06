-- | Layer 0, side track N (PLAN.md N.4): loading an 800XL state through
-- the 6502, on the reference model. A machine runs a program to a moment
-- it can be saved at ('specAtRest'); its state table ("A800.StateTable")
-- goes through the PS's routine builder (build/host/a800-restore, from
-- platform/ps/systems/a800_restore.zig); a fresh machine with the same
-- media gets RAM (as the PS writes it through its window) and runs the
-- routine through the ROM overlay ('runOverlay'). Then:
--
-- * its table is the saved one, field by field, except what the routine
--   can't put back (POKEY's IRQST, SKSTAT, KBCODE; GTIA's collisions,
--   trigger latch; ANTIC's light pen, previous
--   instruction, memory scan counter, hires flag; the 6502's B flag),
--   NMIST if the routine can't reach it;
-- * RAM is the saved one;
-- * both machines go on alike: the registers after every instruction,
--   then the next frame's picture.
module A800.SpecRestore (restoreTests) where

import Prelude

import Control.Monad (replicateM, unless, when)
import Control.Monad.ST (stToIO)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Bits ((.&.))
import Data.List (intercalate)
import qualified Data.Vector.Unboxed as U
import Data.Word (Word16, Word8)
import Numeric (showHex)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.Process (readProcessWithExitCode)
import Test.Tasty
import Test.Tasty.Golden (goldenVsString)
import Test.Tasty.HUnit

import A800.Media (Cart, parseRawCart)
import A800.Spec (stockConfig, xexConfig)
import A800.Spec.Gtia (gtiaFrame)
import A800.Spec.Machine
import A800.Spec.SaveState
import A800.StateTable
import A800.TestRoms
import Mos6502.Spec (Regs (..), getRegs)

tool, scratch :: FilePath
tool = "../../build/host/a800-restore"
scratch = "../../build/a800xl/restore-test"

-- | Fields compared loosely.
loose :: [Int]
loose = [tIrqst, tSkstat, tKbcode, tTrigLatch, tPenh, tPenv, tIrPrev, tMsc, tMsc + 1, tHires]
  <> [tColl .. tColl + 7]

-- | The registers a run is compared by (P without B and bit 5).
regsOf :: Regs -> [Int]
regsOf r = map fromIntegral [rA r, rX r, rY r, rS r, rP r .&. 0xCF] <> [fromIntegral (rPC r)]

buildOverlay :: String -> U.Vector Word8 -> IO (U.Vector Word8, Word16)
buildOverlay name tbl = do
  have <- doesFileExist tool
  unless have (assertFailure (tool <> " is missing: make a800-restore"))
  createDirectoryIfMissing True scratch
  let tf = scratch </> (name <> ".table")
      of' = scratch </> (name <> ".overlay")
  BS.writeFile tf (BS.pack (U.toList tbl))
  (code, _, err) <- readProcessWithExitCode tool [tf, of'] ""
  when (code /= ExitSuccess) (assertFailure ("a800-restore: " <> err))
  ovl <- U.fromList . BS.unpack <$> BS.readFile of'
  case words err of
    ["end", e, "cycles", _] -> pure (ovl, read e)
    _ -> assertFailure ("a800-restore said: " <> err) >> pure (ovl, 0)

-- | The machine a test runs: an executable, or a cartridge (or neither).
data Media = Xex FilePath | Cartridge (Maybe Cart) | RawCart FilePath

configOf :: Media -> IO Config
configOf media = do
  os <- loadFile osRom
  basic <- loadFile basicRom
  case media of
    Xex f -> xexConfig os basic <$> loadFile loaderBin <*> loadFile f
    Cartridge c -> pure (stockConfig os basic c)
    RawCart f -> loadFile f >>= either fail (pure . stockConfig os basic . Just) . parseRawCart

-- | Run n steps, on to a moment at rest; restore a fresh machine;
-- compare; go on k steps on both.
restoreCheck :: Media -> String -> Int -> Int -> Assertion
restoreCheck media name n k = do
  cfg <- configOf media
  m1 <- stToIO (newMachine cfg)
  stToIO (sequence_ (replicate n (stepCpu m1)))
  let toRest i = do
        r <- stToIO (specAtRest m1)
        if r || i > 200000 then pure r else stToIO (stepCpu m1) >> toRest (i + 1 :: Int)
  rest <- toRest 0
  unless rest $ do
    -- how often each condition held over a frame, for the message
    rs <- stToIO (mapM (const (stepCpu m1 >> restReasons m1)) [1 .. 10000 :: Int])
    assertFailure ("no moment at rest; per condition (vblank, ANTIC, GTIA, POKEY, machine), steps of 10000 it held: "
                   <> show (map (length . filter id) (foldr (zipWith (:)) (repeat []) rs)))
  tbl <- stToIO (specTable m1)
  (ovl, end) <- buildOverlay name tbl
  ram <- stToIO (ramImage m1)
  r <- stToIO (runOverlay cfg ram ovl end 100000)
  m2 <- either (\e -> assertFailure e >> fail e) (pure . fst) r
  tbl2 <- stToIO (specTable m2)
  let -- NMIST in vertical blank: $1F, $5F or $7F (bit 5 set without bit
      -- 6 no routine reaches)
      loose' = loose <> [tNmist | tbl U.! tNmist .&. 0x60 == 0x20] <> [tSerout | tbl U.! tSerial .&. 1 == 0]
      masked o v = if o == tP then v .&. 0xCF else v
      diffs = [ (o, tbl U.! o, tbl2 U.! o) | o <- [0 .. tableSize - 1], o `notElem` loose', masked o (tbl U.! o) /= masked o (tbl2 U.! o) ]
      fieldOf o = case [nm | (nm, fo, sz) <- tableFields, o >= fo, o < fo + sz] of
        nm : _ -> nm
        [] -> "?"
  unless (null diffs) $ assertFailure $ "restored table differs: " <> intercalate ", "
    [ fieldOf o <> "@" <> showHex o "" <> " saved " <> showHex a "" <> " restored " <> showHex b "" | (o, a, b) <- take 12 diffs ]
  a1 <- stToIO (ramImage m1)
  a2 <- stToIO (ramImage m2)
  assertBool "RAM differs" (a1 == a2)
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
  f1 <- stToIO (gtiaFrame (mGtia m1))
  f2 <- stToIO (gtiaFrame (mGtia m2))
  assertBool "the frame differs" (f1 == f2)

-- | A program saved at three moments.
moments :: String -> Media -> TestTree
moments name media = testGroup name
  [ testCase ("after " <> show n <> " steps") $ restoreCheck media (takeWhile (/= '.') name <> "-" <> show n) n 60000
  | n <- [800000, 900000, 1000000 :: Int] ]

restoreTests :: TestTree
restoreTests = testGroup "loading through the CPU (side track N, N.4)"
  [ goldenVsString "the state table's layout" "test/golden/state-table.txt" $ pure $ BL.pack $ unlines $
      ("size 0x" <> showHex tableSize "") : [ nm <> " 0x" <> showHex o "" <> " " <> show sz | (nm, o, sz) <- tableFields ]
  , testCase "BASIC at READY (the OS's disk boot tried first)" $ restoreCheck (Cartridge Nothing) "basic" 2000000 100000
  , moments "regs.xex (every register the routine writes)" (Xex (asmRoot </> "regs.xex"))
  , moments "rasterbars.xex (DLIs, WSYNC)" (Xex (asmRoot </> "rasterbars.xex"))
  , moments "scene.xex (players, missiles, scrolling)" (Xex (asmRoot </> "scene.xex"))
  , moments "tone.xex (POKEY)" (Xex (asmRoot </> "tone.xex"))
  , moments "banking.xex (PORTB)" (Xex (asmRoot </> "banking.xex"))
  , moments "cart_banking.rom (a cartridge, PORTB)" (RawCart (asmRoot </> "cart_banking.rom"))
  , testCase "Scorch (a game started from an INIT segment: the loader's page stays)" $ restoreCheck (Xex scorchXex) "scorch" 3000000 200000
  ]
