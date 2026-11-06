-- | Layer 0, side track L (PLAN.md L.6c.1): loading a state through the
-- CPU, on the reference model. A machine runs a ROM to a moment it can be
-- saved at ('specAtRest'); its state table ("Gb.StateTable") goes through
-- the PS's routine builder (build/host/gb-restore, from
-- platform/ps/gb_restore.zig); a machine fresh from its reset gets the
-- memories (as the PS writes them through the block RAMs' ports) and runs
-- the routine through the ROM overlay ('runOverlay'). Then:
--
-- * its table is the saved one, field by field, except what the routine
--   can't put back (the sound unit's registers as written, its length
--   timers and envelope volumes: compared loosely; 0xFF46's value);
-- * the memories are the saved ones;
-- * both machines go on alike: the registers after every instruction,
--   then the screen and the serial output.
module Gb.SpecRestore (restoreTests, slotReplay) where

import Prelude

import Control.Monad (forM_, replicateM, replicateM_, unless, when)
import Data.Bits (shiftR, testBit, (.&.))
import Codec.Picture (PixelRGB8 (..), generateImage, writePng)
import Control.Monad.ST (RealWorld, stToIO)
import qualified Data.ByteString as BS
import Data.List (intercalate, isSuffixOf)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import Data.Word (Word16, Word8)
import Numeric (showHex)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.Process (readProcessWithExitCode)
import Test.Tasty
import Test.Tasty.Golden (goldenVsString)
import Test.Tasty.HUnit
import qualified Data.ByteString.Lazy.Char8 as BL

import Gb.Spec
import Gb.Spec.SaveState (runOverlay, specAtRest, specTable)
import qualified Gb.Spec.State as S
import Gb.StateTable
import Gb.TestRoms

tool, scratch :: FilePath
tool = "../../build/host/gb-restore"
scratch = "../../build/gb/restore-test"

-- | The memories the PS writes through the ports: VRAM, WRAM, HRAM (in
-- the spec's flat 0x8000-0xFFFF store), cartridge RAM, and the Game Boy
-- Color's second VRAM bank and WRAM banks 2 to 7.
memRanges :: [(Int, Int)]
memRanges = [(0x8000, 0x2000), (0xC000, 0x2000), (0xFF80, 0x7F)]

copyMems :: S.Machine RealWorld -> S.Machine RealWorld -> IO ()
copyMems from to = stToIO $ do
  forM_ memRanges $ \(a, n) -> forM_ [a .. a + n - 1] $ \i ->
    MU.read (S.mRam from) (i - 0x8000) >>= MU.write (S.mRam to) (i - 0x8000)
  U.freeze (S.mCram from) >>= U.copy (S.mCram to)
  U.freeze (S.mVram1 from) >>= U.copy (S.mVram1 to)
  U.freeze (S.mWramX from) >>= U.copy (S.mWramX to)

mems :: S.Machine RealWorld -> IO (U.Vector Word8, U.Vector Word8)
mems m = stToIO $ do
  r <- U.freeze (S.mRam m)
  c <- U.freeze (S.mCram m)
  v1 <- U.freeze (S.mVram1 m)
  wx <- U.freeze (S.mWramX m)
  pure (U.concat ([U.slice (a - 0x8000) n r | (a, n) <- memRanges] <> [v1, wx]), c)

-- | Fields compared loosely (what the routine can't put back exactly).
loose :: [Int]
loose = [tNr .. tNr + 21] <> [tChLen .. tChLen + 7] <> [tChVol .. tChVol + 3] <> [tDma]

-- | The routine for a table, from the PS's builder.
buildOverlay :: String -> U.Vector Word8 -> IO (U.Vector Word8, Word16)
buildOverlay name tbl = do
  have <- doesFileExist tool
  unless have (assertFailure (tool <> " is missing: make gb-restore"))
  createDirectoryIfMissing True scratch
  let tf = scratch </> (name <> ".table")
      of' = scratch </> (name <> ".overlay")
  BS.writeFile tf (BS.pack (U.toList tbl))
  (code, _, err) <- readProcessWithExitCode tool [tf, of'] ""
  when (code /= ExitSuccess) (assertFailure ("gb-restore: " <> err))
  ovl <- U.fromList . BS.unpack <$> BS.readFile of'
  case words err of
    ["end", e, "cycles", _] -> pure (ovl, read e)
    _ -> assertFailure ("gb-restore said: " <> err) >> pure (ovl, 0)

-- | Run to step n, then on to a moment at rest; restore a fresh machine
-- from there; compare; go on k instructions on both. @lcd@: the path the
-- routine takes, which the check pins (the LCD off; on with the dot
-- phase 0, from the reset; on with phase 3, switched off and on).
data Lcd = LcdOff | Phase0 | Phase3
  deriving stock (Show, Eq)

restoreCheck :: String -> FilePath -> Lcd -> Int -> Int -> Assertion
restoreCheck = restoreCheckOn Dmg

restoreCheckOn :: Model -> String -> FilePath -> Lcd -> Int -> Int -> Assertion
restoreCheckOn model name path lcd = restoreCheckOn' (Just lcd) model name path

restoreCheckOn' :: Maybe Lcd -> Model -> String -> FilePath -> Int -> Int -> Assertion
restoreCheckOn' lcd model name path n k = do
  rom <- loadRomFile path
  Right m1 <- stToIO (newMachineModel model rom)
  stToIO (replicateM_ n (step m1))
  let toRest i = do
        r <- stToIO (specAtRest m1)
        if r then pure i else if i > 200000 then pure i else stToIO (step m1) >> toRest (i + 1 :: Int)
  waited <- toRest 0
  rest <- stToIO (specAtRest m1)
  unless rest (assertFailure ("no moment at rest within " <> show waited <> " instructions"))
  tbl <- stToIO (specTable m1)
  framesAtSave <- stToIO (S.getSt m1 S.iFRAMES)
  let pos = fromIntegral (tbl U.! tLy) * 456 + fromIntegral (tbl U.! tDot) + 256 * fromIntegral (tbl U.! (tDot + 1)) :: Int
      path' | not (testBit (tbl U.! tLcdc) 7) = LcdOff
            | pos `mod` 4 == 3 = Phase3
            | otherwise = Phase0
  mapM_ (path' @?=) lcd
  (ovl, end) <- buildOverlay name tbl
  Right m2 <- stToIO (newMachineModel model rom)
  copyMems m1 m2
  r <- stToIO (runOverlay m2 ovl end 100000)
  case r of
    Left e -> assertFailure e
    Right _ -> pure ()
  tbl2 <- stToIO (specTable m2)
  let diffs = [ (o, tbl U.! o, tbl2 U.! o) | o <- [0 .. tableSize - 1], o `notElem` loose, tbl U.! o /= tbl2 U.! o ]
      fieldOf o = case [nm | (nm, fo, sz) <- tableFields, o >= fo, o < fo + sz] of
        nm : _ -> nm
        [] -> "?"
  unless (null diffs) $ assertFailure $ "restored table differs: " <> intercalate ", "
    [ fieldOf o <> "@" <> showHex o "" <> " saved " <> showHex a "" <> " restored " <> showHex b "" | (o, a, b) <- take 12 diffs ]
  (v1, c1) <- mems m1
  (v2, c2) <- mems m2
  assertBool "the memories differ" (v1 == v2 && c1 == c2)
  -- (the serial capture is instrumentation since power-on: compared
  -- from here)
  before1 <- stToIO (serialOut m1)
  before2 <- stToIO (serialOut m2)
  t1 <- stToIO (replicateM k (step m1 >> regs m1))
  t2 <- stToIO (replicateM k (step m2 >> regs m2))
  case [ (i, a, b) | (i, a, b) <- zip3 [0 :: Int ..] t1 t2, a /= b ] of
    (i, a, b) : _ -> assertFailure ("the runs part at instruction " <> show i <> ": " <> show a <> " vs " <> show b)
    [] -> pure ()
  -- (the last complete frame isn't state: compared once both have
  -- finished one after the save)
  f1 <- stToIO (S.getSt m1 S.iFRAMES)
  s1 <- stToIO (screen m1)
  s2 <- stToIO (screen m2)
  when (f1 > framesAtSave + 1) $ assertBool "the screen differs" (s1 == s2)
  o1 <- stToIO (serialOut m1)
  o2 <- stToIO (serialOut m2)
  drop (length before2) o2 @?= drop (length before1) o1
  assertBool "nothing to compare: no serial output after the restore" (not (null (drop (length before1) o1)) || not (wantsSerial name))

-- | The ROMs whose check must see serial output after the restore.
wantsSerial :: String -> Bool
wantsSerial = (== "blargg02")

-- | A local game (never in the repo): EMU_RESTORE_ROM=path (a .gbc runs
-- on the Game Boy Color), saved at several moments of its first minute.
localGame :: Maybe FilePath -> [TestTree]
localGame Nothing = []
localGame (Just path) =
  [ testCase ("local: " <> path <> " saved after " <> show n <> " instructions") $
      restoreAny (if ".gbc" `isSuffixOf` path then Cgb else Dmg) ("local-" <> show n) path n 200000
  | n <- [300000, 1000000, 3000000, 6000000 :: Int] ]

-- | 'restoreCheckOn' without pinning the LCD's path.
restoreAny :: Model -> String -> FilePath -> Int -> Int -> Assertion
restoreAny model name path n k = restoreCheckOn' Nothing model name path n k

-- | A slot file from the board (EMU_RESTORE_SLOT, with its ROM in
-- EMU_RESTORE_ROM) into a fresh spec machine through the routine, then run
-- on: frames written to build/gb/restore-test/slot-*.png, the PC sampled;
-- fails if the CPU locks up.
slotReplay :: FilePath -> FilePath -> Assertion
slotReplay rom slot = do
  romB <- loadRomFile rom
  d <- BS.readFile slot
  let model = if ".gbc" `isSuffixOf` rom then Cgb else Dmg
      at o n = U.fromList (BS.unpack (BS.take n (BS.drop o d)))
      tbl = at 512 tableSize
      sec0 = 512 + 4096
      hram = at sec0 0x80
      vram = at (sec0 + 0x80) 0x4000
      wram = at (sec0 + 0x80 + 0x4000) 0x8000
      cram = at (sec0 + 0x80 + 0x4000 + 0x8000) (BS.length d - (sec0 + 0x80 + 0x4000 + 0x8000))
  Right m <- stToIO (newMachineModel model romB)
  stToIO $ do
    forM_ [0 .. 0x7E] $ \i -> MU.write (S.mRam m) (0xFF80 - 0x8000 + i) (hram U.! i)
    forM_ [0 .. 0x1FFF] $ \i -> MU.write (S.mRam m) i (vram U.! i) >> MU.write (S.mVram1 m) i (vram U.! (0x2000 + i))
    forM_ [0 .. 0x1FFF] $ \i -> MU.write (S.mRam m) (0x4000 + i) (wram U.! i)
    forM_ [0 .. 0x5FFF] $ \i -> MU.write (S.mWramX m) i (wram U.! (0x2000 + i))
    forM_ [0 .. min (U.length cram) (MU.length (S.mCram m)) - 1] $ \i -> MU.write (S.mCram m) i (cram U.! i)
  (ovl, end) <- buildOverlay "slot" tbl
  r <- stToIO (runOverlay m ovl end 200000)
  either assertFailure (const (pure ())) r
  tbl2 <- stToIO (specTable m)
  let diffs = [ o | o <- [0 .. tableSize - 1], o `notElem` loose, tbl U.! o /= tbl2 U.! o ]
  unless (null diffs) $ assertFailure ("restored table differs at " <> show (take 12 diffs))
  createDirectoryIfMissing True scratch
  forM_ [1 .. 8 :: Int] $ \k -> do
    stToIO (replicateM_ 250000 (step m))
    pc <- stToIO (S.getSt m S.iPC)
    lock <- stToIO (S.getSt m S.iLOCK)
    putStrLn ("slot replay: " <> show (k * 250000) <> " instructions, PC " <> showHex pc "" <> (if lock /= 0 then " LOCKED" else ""))
    rgb <- stToIO (screenRgb m)
    let px x y = let c = fromIntegral (rgb U.! (y * 160 + x)) :: Int
                     ch v = fromIntegral (((v .&. 31) * 255) `div` 31) :: Word8
                 in PixelRGB8 (ch c) (ch (c `shiftR` 5)) (ch (c `shiftR` 10))
    writePng (scratch </> ("slot-" <> show k <> ".png")) (generateImage px 160 144)
    when (lock /= 0) (assertFailure "the CPU locked up")

restoreTests :: Maybe FilePath -> TestTree
restoreTests local = testGroup "loading through the CPU (side track L, L.6c)" $ localGame local <>
  [ goldenVsString "the state table's layout" "test/golden/state-table.txt" $ pure $ BL.pack $ unlines $
      ("size 0x" <> showHex tableSize "") : [ nm <> " 0x" <> showHex o "" <> " " <> show sz | (nm, o, sz) <- tableFields ]
  , testCase "restore.gb (timer, OAM, LCD toggled, sound)" $
      restoreCheck "restore" (asmRoot </> "restore.gb") Phase3 20000 60000
  , testCase "state_fill.gb" $ restoreCheck "state_fill" (asmRoot </> "state_fill.gb") LcdOff 30000 30000
  , testCase "tone.gb" $ restoreCheck "tone" (asmRoot </> "tone.gb") Phase0 30000 30000
  , testCase "dmg-acid2" $ restoreCheck "acid2" (romRoot </> "dmg-acid2" </> "dmg-acid2.gb") LcdOff 40000 60000
  , testCase "save.gb (cartridge RAM)" $ restoreCheck "save" (asmRoot </> "save.gb") Phase3 20000 20000
  , testCase "mbc3.gb (the clock)" $ restoreCheck "mbc3" (asmRoot </> "mbc3.gb") Phase0 20000 20000
  , testCase "Blargg cpu_instrs 02 (serial)" $
      restoreCheck "blargg02" (romRoot </> "blargg" </> "cpu_instrs" </> "02-interrupts.gb") LcdOff 20000 400000
  , testCase "libbet (a game)" $ restoreCheck "libbet" (romRoot </> "games" </> "libbet.gb") Phase0 150000 50000
    -- the Game Boy Color (L.6c.4)
  , testCase "GBC: hdma.gb (CGB mode: VRAM DMA, both banks, palettes)" $
      restoreCheckOn Cgb "gbc-hdma" (asmRoot </> "hdma.gb") LcdOff 20000 30000
  , testCase "GBC: sound_ds.gb (double speed)" $
      restoreCheckOn Cgb "gbc-sound_ds" (asmRoot </> "sound_ds.gb") Phase0 20000 30000
  , testCase "GBC: speed.gb (speed switches)" $
      restoreCheckOn Cgb "gbc-speed" (asmRoot </> "speed.gb") Phase0 20000 30000
  , testCase "GBC: tone.gb (a DMG cartridge: compatibility mode)" $
      restoreCheckOn Cgb "gbc-tone" (asmRoot </> "tone.gb") Phase0 30000 30000
  , testCase "GBC: restore.gb (compatibility mode, the LCD toggled)" $
      restoreCheckOn Cgb "gbc-restore" (asmRoot </> "restore.gb") Phase3 20000 60000
  ]
