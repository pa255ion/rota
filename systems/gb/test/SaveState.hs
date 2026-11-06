{-# LANGUAGE FlexibleContexts #-}
-- | Layer 2, side track L (PLAN.md L.6c.2): the Game Boy core's save
-- states in Clash simulation of the circuit ('gbCoreHooked'), the DMG.
--
-- * Save: a core (B) runs a ROM and is asked to freeze; it goes on to a
--   moment a state can be taken ('restoreable') and freezes; its state
--   table, HRAM, VRAM, WRAM and cartridge RAM are read through the hooks
--   and the PS's port; it goes on. Its M-cycle events (every bus access
--   with the registers) are those of a core never frozen (A).
-- * Load: a fresh core (C) is held (its reset) and frozen, gets the
--   memories and the restore routine (build/host/gb-restore, the PS's
--   builder, from B's table) in its ROM overlay, and is released. After
--   the routine's M-cycles its events are B's after the save, M-cycle for
--   M-cycle.
-- * The table read back from the reset core, the status bytes, the hooks
--   idle when not asked.
module Main (main) where

import Prelude

import qualified Clash.Prelude as C
import Clash.Prelude (BitVector, Signal, Unsigned)
import Control.Monad (unless, when)
import qualified Data.ByteString as BS
import Data.List (findIndex)
import Data.Maybe (catMaybes, isJust)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.Process (readProcessWithExitCode)
import Test.Tasty
import Test.Tasty.HUnit

import Gb.Core
import Gb.Cpu (BusOp (..))
import Control.Monad.ST (stToIO)
import qualified Data.Vector.Unboxed as U
import Numeric (showHex)
import Data.Word (Word8)

import qualified Gb.Spec as Spec
import Gb.Spec.SaveState (specTable)
import Gb.StateTable (tableFields, tableSize)
import Gb.StateTap (stateTableVersion, gbStateTap)
import Gb.TestRoms (asmRoot, loadRomFile, romRoot)
import Platform.SaveState

data TbIn = TbIn
  { tiDot    :: Bool
  , tiRun    :: Bool
  , tiFreeze :: Bool
  , tiOp     :: Maybe StateOp
  , tiCram   :: CramPort
  } deriving stock (C.Generic)
    deriving anyclass C.NFDataX

data TbOut = TbOut
  { toEvent :: Maybe McEvent
  , toState :: Maybe (BitVector 8)
  , toFroze :: Bool
  , toCram  :: BitVector 8
  } deriving stock (C.Generic)
    deriving anyclass C.NFDataX

-- | The core with its ROM answered a clock after each request.
tb :: C.HiddenClockResetEnable C.System => CoreModel -> BS.ByteString -> Signal C.System TbIn -> Signal C.System TbOut
tb model rom inp = TbOut <$> (koEvent <$> o) <*> stO <*> frz <*> cramB
 where
  (o, cramB, preq, stO, frz) =
    gbCoreHooked (Just gbStateTap) model ans (tiRun <$> inp) (tiDot <$> inp) (pure 0) (pure (C.repeat 0))
                 (pure gbcDefaultPal) (tiCram <$> inp) (tiFreeze <$> inp) (tiOp <$> inp)
  ans = C.register Nothing (fmap romAt <$> preq)
  romAt :: Unsigned 23 -> BitVector 8
  romAt a = let i = fromIntegral a in if i < BS.length rom then fromIntegral (BS.index rom i) else 0xFF

cramBytes :: Int
cramBytes = 0x2000

-- | A run, after cartridge RAM is zeroed through the PS's port (a block
-- RAM starts undefined in simulation); the outputs of `ins` only.
runs :: BS.ByteString -> [TbIn] -> [TbOut]
runs = runsOn CoreDmg

runsOn :: CoreModel -> BS.ByteString -> [TbIn] -> [TbOut]
runsOn model rom ins = take (length ins) (drop cramBytes (C.simulate (C.withClockResetEnable C.clockGen C.resetGen C.enableGen (tb model rom)) (zeroCram ++ ins)))
 where
  zeroCram = [TbIn False True False Nothing (Just (fromIntegral i, Just 0)) | i <- [0 .. cramBytes - 1]]

-- | Running: a dot every 6 clocks (as Gb.Display's 4.19 MHz in 25.2),
-- asked to freeze or not.
going :: Bool -> Int -> [TbIn]
going f n = [TbIn (k `mod` 6 == 0) True f Nothing Nothing | k <- [0 .. n - 1]]

-- | Frozen (held or not): 4 quiet clocks (the freeze takes one to latch,
-- the status one more to see it), these accesses one per clock, then 2
-- quiet clocks.
frozenOps :: Bool -> [(Maybe StateOp, CramPort)] -> [TbIn]
frozenOps run ops = replicate 4 quiet ++ [TbIn False run True o c | (o, c) <- ops] ++ replicate 2 quiet
 where
  quiet = TbIn False run True Nothing Nothing

-- | The memories in the state window: HRAM, VRAM bank 0, WRAM banks 0
-- and 1; on the Game Boy Color VRAM bank 1 and WRAM banks 2 to 7 too.
memAddrs :: CoreModel -> [Unsigned 16]
memAddrs CoreDmg = [0x1000 .. 0x107F] ++ [0x2000 .. 0x3FFF] ++ [0x8000 .. 0x9FFF]
memAddrs CoreCgb = [0x1000 .. 0x107F] ++ [0x2000 .. 0x5FFF] ++ [0x8000 .. 0xFFFF]

-- | The status, the table, the memories, cartridge RAM.
readOps :: CoreModel -> [(Maybe StateOp, CramPort)]
readOps model = (Just (StRead stateStatusAddr), Nothing)
        : [(Just (StRead a), Nothing) | a <- [0 .. fromIntegral tableSize - 1] ++ memAddrs model]
       ++ [(Nothing, Just (fromIntegral i, Nothing)) | i <- [0 .. cramBytes - 1]]

-- | From the outputs of 'frozenOps' readOps: whether the status read 1,
-- the table, the memories, cartridge RAM (each the clock after its access).
readOut :: CoreModel -> [TbOut] -> (Bool, [BitVector 8], [BitVector 8], [BitVector 8])
readOut model outs = (fmap toState (take 1 answers) == [Just 1], take tableSize vals, drop tableSize vals, cramVals)
 where
  answers = take (length (readOps model)) (drop 1 outs)
  nState = tableSize + length (memAddrs model)
  vals = [maybe 0 id (toState a) | a <- take nState (drop 1 answers)]
  cramVals = [toCram a | a <- drop (1 + nState) answers]

tool, scratch :: FilePath
tool = "../../build/host/gb-restore"
scratch = "../../build/gb/restore-test"

-- | The PS's routine for a table: the overlay, its end address, its
-- M-cycles.
buildOverlay :: String -> [BitVector 8] -> IO ([BitVector 8], Int, Int)
buildOverlay name tbl = do
  have <- doesFileExist tool
  unless have (assertFailure (tool <> " is missing: make gb-restore"))
  createDirectoryIfMissing True scratch
  let tf = scratch </> ("clash-" <> name <> ".table")
      of' = scratch </> ("clash-" <> name <> ".overlay")
  BS.writeFile tf (BS.pack (map fromIntegral tbl))
  (code, _, err) <- readProcessWithExitCode tool [tf, of'] ""
  when (code /= ExitSuccess) (assertFailure ("gb-restore: " <> err))
  ovl <- map fromIntegral . BS.unpack <$> BS.readFile of'
  case words err of
    ["end", e, "cycles", c] -> pure (ovl, read e, read c)
    _ -> assertFailure ("gb-restore said: " <> err) >> pure (ovl, 0, 0)

-- | Save after n clocks (and the wait for a moment at rest), load into a
-- fresh core, compare m clocks of each after.
check :: String -> FilePath -> Int -> Int -> Assertion
check = checkOn CoreDmg

checkOn :: CoreModel -> String -> FilePath -> Int -> Int -> Assertion
checkOn model name path n m = do
  rom <- loadRomFile path
  let run = runsOn model
      readOps' = readOps model
      memAddrs' = memAddrs model
  -- where B freezes, once asked at n (a first run, to place the reads)
  let probe = run rom (going False n ++ going True 500000)
  z <- case findIndex toFroze (drop n probe) of
    Just i -> pure (n + i)
    Nothing -> assertFailure "B never froze" >> pure 0
  let wait = z - n + 2
      outB = run rom (going False n ++ going True wait ++ frozenOps True readOps' ++ going False m)
      (ok, tbl, mem, cram) = readOut model (drop (n + wait + 4) outB)
      evA = catMaybes (map toEvent (run rom (going False (n + wait + m))))
      evB = catMaybes (map toEvent outB)
      readLen = 4 + length readOps' + 2
      afterB = catMaybes (map toEvent (drop (n + wait + readLen) outB))
  assertBool "the status did not read 1 (not frozen)" ok
  -- the core's table is the spec's after as many instructions (the
  -- events that end a spec step): every field, the sound unit's too
  specTbl <- specTableAt model rom (length (filter meStepDone (catMaybes (map toEvent (take (n + wait) outB)))))
  let diffs = [ (o, a, x) | (o, a, x) <- zip3 [0 :: Int ..] (map fromIntegral tbl) (U.toList specTbl), a /= x ]
      fieldOf o = case [nm | (nm, fo, sz) <- tableFields, o >= fo, o < fo + sz] of
        nm : _ -> nm
        [] -> "?"
  unless (null diffs) $ assertFailure $ "the core's table differs from the spec's: " <> unwords
    [ fieldOf o <> "@" <> showHex o "" <> " core " <> showHex a "" <> " spec " <> showHex x "" | (o, a, x) <- take 12 diffs ]
  (ovl, end, cycles) <- buildOverlay name tbl
  let writes = [(Just (StWrite a v), Nothing) | (a, v) <- zip memAddrs' mem]
            ++ [(Nothing, Just (fromIntegral i, Just v)) | (i, v) <- zip [0 :: Int ..] cram]
            ++ [(Just (StWrite (overlayBase + fromIntegral i) v), Nothing) | (i, v) <- zip [0 :: Int ..] ovl]
            ++ [ (Just (StWrite overlayEndAddr (fromIntegral (end `mod` 256))), Nothing)
               , (Just (StWrite (overlayEndAddr + 1) (fromIntegral (end `div` 256))), Nothing)
               , (Just (StWrite (overlayEndAddr + 2) 1), Nothing) ]
      -- as the PS does: the game running (k clocks), frozen at a moment at
      -- rest, then held (its reset), the writes, released
      k = 30000
      probeC = run rom (going False k ++ going True 500000)
      zC = maybe 0 (k +) (findIndex toFroze (drop k probeC))
      waitC = zC - k + 2
      outC = drop (k + waitC) (run rom (going False k ++ going True waitC ++ frozenOps False writes ++ going False (m + 24 * cycles + 2000)))
      evC = catMaybes (map toEvent outC)
      -- the routine ends with the CPU's read of its JP's last byte (the
      -- end address), then the JP's internal M-cycle: the game from there
      -- (the speed switch's pause has no CPU events: not by its M-cycles)
      endAt = findIndex (\ev -> meOp ev == OpRead && meAddr ev == fromIntegral end) evC
      afterC = maybe [] (\i -> drop (i + 2) evC) endAt
      len = min (length afterB) (length afterC)
      lenAB = min (length evA) (length evB)
  assertBool "the loaded core never read the routine's last byte" (isJust endAt)
  assertBool "a frozen and read core differs from one never frozen" (take lenAB evA == take lenAB evB)
  assertBool ("too few events after the load: " <> show len) (len > 1000)
  case [ (i, a, b) | (i, a, b) <- zip3 [0 :: Int ..] (take len afterB) (take len afterC), a /= b ] of
    (i, a, b) : _ -> assertFailure ("the loaded core parts from the saved one at event " <> show i
                                    <> " of " <> show len <> ":\n  saved  " <> show a <> "\n  loaded " <> show b)
    [] -> pure ()

-- | The spec's table after @k@ instructions (the core freezes at an
-- instruction's start).
specTableAt :: CoreModel -> BS.ByteString -> Int -> IO (U.Vector Word8)
specTableAt model rom k = do
  em <- stToIO (Spec.newMachineModel (if model == CoreCgb then Spec.Cgb else Spec.Dmg) rom)
  m <- either (\e -> assertFailure e >> fail e) pure em
  stToIO (sequence_ (replicate k (Spec.step m)))
  stToIO (specTable m)

main :: IO ()
main = do
  full <- (== Just "1") <$> lookupEnv "EMU_FULL_TESTS"
  defaultMain $ testGroup "gb-savestate (side track L, L.6c)" $
    [ testCase "restore.gb (timer, OAM, LCD toggled, sound)" $ check "restore" (asmRoot </> "restore.gb") 120000 120000
    , testCase "tone.gb" $ check "tone" (asmRoot </> "tone.gb") 60000 60000
    , testCase "GBC: sound_ds.gb (double speed)" $ checkOn CoreCgb "gbc-sound_ds" (asmRoot </> "sound_ds.gb") 150000 80000
    , testCase "the status bytes: the table's size and version, frozen or not" $ do
        rom <- loadRomFile (asmRoot </> "tone.gb")
        let st = [Just (StRead (stateStatusAddr + i)) | i <- [1, 2, 3]]
            ins = [TbIn False True False q Nothing | q <- st] ++ [TbIn False True False Nothing Nothing]
            o = runs rom (going False 2000 ++ ins)
            got = map toState (take 3 (drop 2001 o))
        got @?= map Just [fromIntegral (tableSize `mod` 256), fromIntegral (tableSize `div` 256), stateTableVersion]
    , testCase "the hooks idle: no state output, never frozen" $ do
        rom <- loadRomFile (asmRoot </> "tone.gb")
        let o = runs rom (going False 20000)
        assertBool "state output" (not (any (isJust . toState) o))
        assertBool "frozen" (not (any toFroze o))
    , testCase "frozen, then held: the core is reset (the PS's order for a load)" $ do
        rom <- loadRomFile (asmRoot </> "tone.gb")
        let k = 30000
            probe = runs rom (going False k ++ going True 500000)
            z = maybe 0 (k +) (findIndex toFroze (drop k probe))
            wait = z - k + 2
            pcOps = [(Just (StRead a), Nothing) | a <- [0x0A, 0x0B]]
            o = runs rom (going False k ++ going True wait ++ frozenOps False pcOps)
            got = map toState (take 2 (drop (k + wait + 5) o))
        got @?= [Just 0x00, Just 0x01]
    , testCase "held, the core freezes at once" $ do
        rom <- loadRomFile (asmRoot </> "tone.gb")
        let o = runs rom (frozenOps False [(Just (StRead stateStatusAddr), Nothing)])
        map toState (take 1 (drop 5 o)) @?= [Just 1]
    ] <>
    (if full
       then [ testCase "save.gb: cartridge RAM" $ check "save" (asmRoot </> "save.gb") 120000 80000
            , testCase "dmg-acid2" $ check "acid2" (romRoot </> "dmg-acid2" </> "dmg-acid2.gb") 250000 150000
            , testCase "state_fill.gb (LCD off)" $ check "state_fill" (asmRoot </> "state_fill.gb") 500000 60000
            , testCase "GBC: hdma.gb (VRAM DMA, both VRAM banks)" $ checkOn CoreCgb "gbc-hdma" (asmRoot </> "hdma.gb") 200000 80000
            , testCase "GBC: restore.gb (compatibility mode)" $ checkOn CoreCgb "gbc-restore" (asmRoot </> "restore.gb") 120000 120000 ]
       else [])
