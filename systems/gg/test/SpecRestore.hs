-- | Layer 0, side track N (PLAN.md N.1): loading a Game Gear or Master
-- System state through the Z80, on the reference model. A machine runs a
-- ROM to a moment it can be saved at ('specAtRest'); its state table
-- ("Gg.StateTable") goes through the PS's routine builder
-- (build/host/gg-restore, from platform/ps/systems/gg_restore.zig); a fresh
-- machine gets the memories (as the PS writes them through its windows)
-- and runs the routine through the ROM overlay ('runOverlay'). Then:
--
-- * its table is the saved one, field by field, except what the routine
--   can't put back (the PSG's LFSR, the VDP's read buffer in a write
--   mode, its H latch, a pending line interrupt and the status's sticky
--   sprite overflow and collision flags, MEMPTR, the FM unit);
-- * the memories are the saved ones;
-- * both machines go on alike: the registers after every instruction,
--   then the frame.
module SpecRestore (restoreTests, localGames) where

import Prelude

import Control.Monad (replicateM, unless, when)
import Control.Monad.ST (RealWorld, stToIO)
import qualified Data.ByteString as BS
import Data.List (intercalate, isSuffixOf)
import qualified Data.Vector.Unboxed as U
import Data.Bits ((.&.))
import Data.Word (Word16, Word8)
import Numeric (showHex)
import System.Directory (createDirectoryIfMissing, doesFileExist, listDirectory)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.Process (readProcessWithExitCode)
import Test.Tasty
import Test.Tasty.Golden (goldenVsString)
import Test.Tasty.HUnit
import qualified Data.ByteString.Lazy.Char8 as BL

import Gg.Spec
import Gg.StateTable
import Z80.Spec (Regs (..), getRegs)

tool, scratch, asmDir :: FilePath
tool = "../../build/host/gg-restore"
scratch = "../../build/gg/restore-test"
asmDir = "../../build/gg/asm"

-- | Fields compared loosely.
loose :: [Int]
loose = [tWZ, tWZ + 1, tPsgLfsr, tPsgLfsr + 1, tVdpHLatch, tVdpLinePend, tFmCtl] <> [tFmRegs .. tFmRegs + 63]

-- | The registers a run is compared by (not MEMPTR, Q and the flags of
-- the last instruction, which the routine leaves as its own).
regsOf :: Regs -> [Int]
regsOf r = map fromIntegral [rA r, rF r, rB r, rC r, rD r, rE r, rH r, rL r, rI r, rR r, rIM r]
  <> map fromIntegral [rAF' r, rBC' r, rDE' r, rHL' r, rIX r, rIY r, rSP r, rPC r]
  <> map fromEnum [rIFF1 r, rIFF2 r, rHalted r]

buildOverlay :: String -> U.Vector Word8 -> IO (U.Vector Word8, Word16)
buildOverlay name tbl = do
  have <- doesFileExist tool
  unless have (assertFailure (tool <> " is missing: make gg-restore"))
  createDirectoryIfMissing True scratch
  let tf = scratch </> (name <> ".table")
      of' = scratch </> (name <> ".overlay")
  BS.writeFile tf (BS.pack (U.toList tbl))
  (code, _, err) <- readProcessWithExitCode tool [tf, of'] ""
  when (code /= ExitSuccess) (assertFailure ("gg-restore: " <> err))
  ovl <- U.fromList . BS.unpack <$> BS.readFile of'
  case words err of
    ["end", e, "tstates", _] -> pure (ovl, read e)
    _ -> assertFailure ("gg-restore said: " <> err) >> pure (ovl, 0)

type Make = BS.ByteString -> IO (Machine RealWorld)

gameGear, masterSystem :: Make
gameGear rom = stToIO (newMachine rom)
masterSystem rom = stToIO (newMasterSystem Nothing (detectMapper rom) rom)

-- | Run n instructions, on to a moment at rest; restore a fresh machine;
-- compare; go on k instructions on both.
restoreCheck :: Make -> String -> FilePath -> Int -> Int -> Assertion
restoreCheck = restoreCheckBy regsOf

-- | Compared by @by@: every register ('regsOf'), or only the control flow
-- (PC, SP: a game reading an approximate flag, e.g. the VDP's sticky
-- sprite overflow, gets another A but may go on the same way).
restoreCheckBy :: (Regs -> [Int]) -> Make -> String -> FilePath -> Int -> Int -> Assertion
restoreCheckBy by make name path n k = do
  rom <- BS.readFile path
  m1 <- make rom
  stToIO (sequence_ (replicate n (stepMachine m1)))
  let toRest i = do
        r <- stToIO (specAtRest m1)
        if r || i > 400000 then pure r else stToIO (stepMachine m1) >> toRest (i + 1 :: Int)
  rest <- toRest 0
  unless rest (assertFailure "no moment at rest")
  tbl <- stToIO (specTable m1)
  (ovl, end) <- buildOverlay name tbl
  m2 <- make rom
  stToIO $ do
    ramImage m1 >>= loadRam m2
    vramImage m1 >>= loadVram m2
    cartRam m1 >>= loadCartRam m2 . BS.pack . U.toList
  r <- stToIO (runOverlay m2 ovl end 400000)
  either assertFailure (const (pure ())) r
  -- (halted: the routine jumped back onto the game's HALT, one step on)
  when (tbl U.! tHalted /= 0) (stToIO (stepMachine m2) >> pure ())
  tbl2 <- stToIO (specTable m2)
  let -- the control port's byte counts only while latched, the read
      -- buffer only in read mode (code 0)
      loose' = loose <> [tVdpLatch | tbl U.! tVdpLatched == 0] <> [tVdpBuf | tbl U.! tVdpCode /= 0]
      -- the status's sprite overflow and collision (bits 6, 5) stay set
      -- until read, from whatever frame set them: only the frame flag
      -- (bit 7) is put back
      masked o v = if o == tVdpStatus then v .&. 0x80 else v
      diffs = [ (o, tbl U.! o, tbl2 U.! o) | o <- [0 .. tableSize - 1], o `notElem` loose', masked o (tbl U.! o) /= masked o (tbl2 U.! o) ]
      fieldOf o = case [nm | (nm, fo, sz) <- tableFields, o >= fo, o < fo + sz] of
        nm : _ -> nm
        [] -> "?"
  unless (null diffs) $ assertFailure $ "restored table differs: " <> intercalate ", "
    [ fieldOf o <> "@" <> showHex o "" <> " saved " <> showHex a "" <> " restored " <> showHex b "" | (o, a, b) <- take 12 diffs ]
  let mems m = stToIO ((,,) <$> ramImage m <*> vramImage m <*> cartRam m)
  (a1, b1, c1) <- mems m1
  (a2, b2, c2) <- mems m2
  assertBool "RAM differs" (a1 == a2)
  assertBool "VRAM differs" (b1 == b2)
  assertBool "cartridge RAM differs" (c1 == c2)
  t1 <- stToIO (replicateM k (stepMachine m1 >> (by <$> getRegs (mCpu m1))))
  t2 <- stToIO (replicateM k (stepMachine m2 >> (by <$> getRegs (mCpu m2))))
  case [ (i, x, y) | (i, x, y) <- zip3 [0 :: Int ..] t1 t2, x /= y ] of
    (i, x, y) : _ -> assertFailure ("the runs part at instruction " <> show i <> ":\n  " <> show x <> "\n  " <> show y)
    [] -> pure ()
  stToIO (runFrame m1 >> runFrame m2)
  f1 <- stToIO (frameBuffer m1)
  f2 <- stToIO (frameBuffer m2)
  assertBool "the frame differs" (f1 == f2)

-- | Optional: every game in a folder (EMU_RESTORE_DIR; never in the
-- repo), saved at a few moments.
localGames :: Maybe FilePath -> IO [TestTree]
localGames Nothing = pure []
localGames (Just dir) = do
  fs <- filter (\f -> ".gg" `isSuffixOf` f || ".sms" `isSuffixOf` f) <$> listDirectory dir
  pure [ testCase ("local: " <> f <> " after " <> show n <> (if flow then " (control flow)" else "")) $
           (if flow then restoreCheckBy (\r -> map fromIntegral [rPC r, rSP r]) else restoreCheck)
             (if ".sms" `isSuffixOf` f then masterSystem else gameGear) ("local-" <> show n) (dir </> f) n 100000
       | f <- fs, n <- [200000, 1000000, 3000000 :: Int], flow <- [False, True] ]

restoreTests :: TestTree
restoreTests = testGroup "loading through the CPU (side track N, N.1)"
  [ goldenVsString "the state table's layout" "test/golden/state-table.txt" $ pure $ BL.pack $ unlines $
      ("size 0x" <> showHex tableSize "") : [ nm <> " 0x" <> showHex o "" <> " " <> show sz | (nm, o, sz) <- tableFields ]
  , testCase "tone.gg" $ restoreCheck gameGear "tone" (asmDir </> "tone.gg") 20000 30000
  , testCase "sprites.gg" $ restoreCheck gameGear "sprites" (asmDir </> "sprites.gg") 20000 30000
  , testCase "vdpint.gg" $ restoreCheck gameGear "vdpint" (asmDir </> "vdpint.gg") 20000 30000
  , testCase "mapper.gg" $ restoreCheck gameGear "mapper" (asmDir </> "mapper.gg") 20000 30000
  , testCase "codemasters.gg" $ restoreCheck gameGear "codemasters" (asmDir </> "codemasters.gg") 20000 30000
  , testCase "save.gg (cartridge RAM)" $ restoreCheck gameGear "save" (asmDir </> "save.gg") 20000 30000
  , testCase "Waternet (a Game Gear game)" $ restoreCheck gameGear "waternet" "../../roms/gg/games/waternet.gg" 300000 100000
  , testCase "Master System: Waternet" $ restoreCheck masterSystem "sms-waternet" "../../roms/sms/games/waternet.sms" 300000 100000
  , testCase "Master System: Raposa do Sol" $ restoreCheck masterSystem "sms-raposa" "../../roms/sms/games/raposadosol_v20.sms" 300000 100000
  ]
