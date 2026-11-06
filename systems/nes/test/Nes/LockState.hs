-- | Layer 2, side track N (PLAN.md N.8): the core's save-state tap against
-- the reference model's.
--
-- * __Rest and table:__ the spec and the core side by side ("Nes.CoreHarness"),
--   step by step: 'restoreable' says what 'specAtRest' says after every
--   step, and wherever both are at rest the core's table ('tapTable') is
--   the spec's ('specTable'), byte for byte.
-- * __A load in the core:__ a state the spec saves at rest, its routine
--   (build/host/nes-restore) run by a fresh core with the memories written
--   and the ROM overlay on ('kiOvlOn'), as the PS does: the core's table
--   then is the saved one but for the approximate fields, and the loaded
--   core goes on as the saved spec machine, step by step.
module Nes.LockState (stateTests) where

import Prelude

import Control.Monad (forM_, unless, when)
import Control.Monad.ST (RealWorld, stToIO)
import Data.Bits (shiftR, (.&.))
import qualified Data.ByteString as BS
import Data.List (intercalate)
import Data.STRef
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MV
import Data.Word (Word16, Word8)
import Numeric (showHex)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.Exit (ExitCode (..))
import System.FilePath ((</>), takeBaseName)
import System.Process (readProcessWithExitCode)
import Test.Tasty
import Test.Tasty.HUnit

import Mos6502.Core (CpuRegs (..))
import Mos6502.Spec (Regs (..), getRegs)
import Nes.Core
import Nes.CoreHarness
import qualified Nes.Spec.Machine as S
import Nes.Spec.SaveState
import Nes.StateTable
import Nes.StateTap
import Nes.TestRoms

tool, scratch :: FilePath
tool = "../../build/host/nes-restore"
scratch = "../../build/nes/lockstate-test"

-- | The core's table.
tableOf :: CoreRun RealWorld -> IO (U.Vector Word8)
tableOf cr = do
  s <- stToIO (readSTRef (crState cr))
  pure (U.generate tableSize (\i -> fromIntegral (tapTable s (fromIntegral i))))

coreAtRest :: CoreRun RealWorld -> IO Bool
coreAtRest cr = restoreable <$> stToIO (readSTRef (crState cr))

fieldOf :: Int -> String
fieldOf o = case [nm | (nm, fo, sz) <- tableFields, o >= fo, o < fo + sz] of
  nm : _ -> nm
  [] -> "?"

diffText :: U.Vector Word8 -> U.Vector Word8 -> [Int] -> String
diffText a b skip = unwords
  [ fieldOf o <> "@" <> showHex o "" <> " " <> showHex x "" <> "/" <> showHex y ""
  | o <- [0 .. tableSize - 1], o `notElem` skip, let x = a U.! o, let y = b U.! o, x /= y ]

-- | Spec and core side by side for @n@ steps: rest and table agree.
followsSpec :: FilePath -> Int -> Assertion
followsSpec file n = do
  rom <- loadRomFile file
  m <- stToIO (S.newMachine rom) >>= either (\e -> assertFailure e >> fail e) pure
  cr <- stToIO (newCoreRun rom) >>= either (\e -> assertFailure e >> fail e) pure
  _ <- stToIO (coreRunStep cr) -- the reset sequence (the spec ran it in newMachine)
  rests <- newSTRefIO (0 :: Int)
  let go i
        | i >= n = pure ()
        | otherwise = do
            _ <- stToIO (S.stepCpu m)
            _ <- stToIO (coreRunStep cr)
            rs <- stToIO (specAtRest m)
            rc <- coreAtRest cr
            when (rs /= rc) $ do
              why <- stToIO (restReasons m)
              t1 <- stToIO (specTable m)
              t2 <- tableOf cr
              assertFailure ("step " <> show i <> ": spec at rest " <> show rs <> " (" <> show why <> "), core " <> show rc
                             <> "; tables: " <> diffText t1 t2 [])
            when rs $ do
              stToIO (modifySTRef' rests (+ 1))
              t1 <- stToIO (specTable m)
              t2 <- tableOf cr
              unless (t1 == t2) (assertFailure ("step " <> show i <> ", at rest: tables differ: " <> diffText t1 t2 []))
            go (i + 1)
  go (0 :: Int)
  k <- stToIO (readSTRef rests)
  assertBool "no moment at rest in the run" (k > 0)
 where
  newSTRefIO = stToIO . newSTRef

-- | Fields compared loosely after a load (as Nes.SpecRestore's).
loose :: [Int]
loose = [tDmcIrq, tDmcShift, tDmcSilence, tSeq, tSeq + 1] <> [tLengths .. tLengths + 3] <> [tMmc5 + 28, tMmc5 + 29, tMmc5Line]

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

-- | A state saved by the spec after @n@ steps (and on to rest), loaded by
-- a fresh core through the routine; then @k@ steps alike.
loadCheck :: FilePath -> Int -> Int -> Assertion
loadCheck file n k = do
  rom <- loadRomFile file
  m <- stToIO (S.newMachine rom) >>= either (\e -> assertFailure e >> fail e) pure
  stToIO (sequence_ (replicate n (S.stepCpu m)))
  let toRest i = do
        r <- stToIO (specAtRest m)
        if r || i > 300000 then pure r else stToIO (S.stepCpu m) >> toRest (i + 1 :: Int)
  rest <- toRest 0
  unless rest (assertFailure "no moment at rest")
  tbl <- stToIO (specTable m)
  im <- stToIO (images m)
  (ovl, end) <- buildOverlay (takeBaseName file <> "-" <> show n) tbl im
  cr <- stToIO (newCoreRun rom) >>= either (\e -> assertFailure e >> fail e) pure
  -- the PS's windows, while the core is held at power-on
  let load v img = forM_ [0 .. min (U.length img) (MV.length v) - 1] (\i -> MV.write v i (img U.! i))
  stToIO $ do
    load (crRam cr) (imRam im)
    load (crCiram cr) (imCiram im)
    load (crPrgRam cr) (imPrgRam im)
    load (crChr cr) (imChrRam im)
    load (crExram cr) (imExram im)
    load (crOvl cr) ovl
    modifySTRef' (crIn cr) (\i -> i { kiOvlOn = True })
  -- run until the instruction that read the end address has ended
  let run seen i
        | i > 3000000 = assertFailure "the routine didn't end"
        | otherwise = do
            st <- stToIO (coreRunStep cr)
            let readsEnd = any (\(a, _, w) -> a == end && not w) (srBus st)
            if seen || readsEnd
              then stToIO (modifySTRef' (crIn cr) (\x -> x { kiOvlOn = False }))
              else run False (i + 1 :: Int)
  run False 0
  t2 <- tableOf cr
  let at o = tbl U.! o
      tNt = (fromIntegral (at (tT + 1)) `shiftR` 2) .&. 3 :: Int
      ctrlMask = if fromIntegral (at tCtrl) .&. 3 /= tNt then 0xFC else 0xFF
      masked o v
        | o == tP = v .&. 0xCF
        | o == tStatus = v .&. 0x80
        | o == tCtrl = v .&. ctrlMask
        | otherwise = v
      diffs = [ o | o <- [0 .. tableSize - 1], o `notElem` loose, masked o (at o) /= masked o (t2 U.! o) ]
  unless (null diffs) $ assertFailure ("the core's table after the load differs: " <> intercalate ", "
    [ fieldOf o <> "@" <> showHex o "" <> " saved " <> showHex (at o) "" <> " core " <> showHex (t2 U.! o) "" | o <- take 12 diffs ])
  -- on, side by side: the registers after each step
  let regsS r = [fromIntegral (rA r), fromIntegral (rX r), fromIntegral (rY r), fromIntegral (rS r), fromIntegral (rP r) .&. 0xCF, fromIntegral (rPC r)] :: [Int]
      regsC r = [fromIntegral (crA r), fromIntegral (crX r), fromIntegral (crY r), fromIntegral (crS r), fromIntegral (crP r) .&. 0xCF, fromIntegral (crPc r)] :: [Int]
      go i
        | i >= k = pure ()
        | otherwise = do
            _ <- stToIO (S.stepCpu m)
            st <- stToIO (coreRunStep cr)
            r <- stToIO (getRegs (S.mCpu m))
            s <- stToIO (readSTRef (crState cr))
            let cs = regsC (CpuRegs (fromIntegral (rA (srRegs st))) (fromIntegral (rX (srRegs st))) (fromIntegral (rY (srRegs st)))
                                    (fromIntegral (rS (srRegs st))) (fromIntegral (rP (srRegs st))) (fromIntegral (rPC (srRegs st))))
            _ <- pure s
            when (regsS r /= cs) $ do
              ta <- stToIO (specTable m)
              tb' <- tableOf cr
              assertFailure ("the runs part at step " <> show i <> ": spec " <> show (regsS r) <> ", core " <> show cs
                             <> "; tables: " <> diffText ta tb' [])
            go (i + 1)
  go (0 :: Int)

-- | The tests (the full tier adds more programs and longer runs).
stateTests :: Bool -> TestTree
stateTests full = testGroup "save states in the core (side track N, N.8)"
  [ testGroup "rest and table as the spec's, after every step"
      [ testCase (takeBaseName f) (followsSpec f n) | (f, n) <- follows ]
  , testGroup "a load through the routine, in the core"
      [ testCase (takeBaseName f <> " after " <> show n <> " steps") (loadCheck f n 30000) | (f, n) <- loads ]
  ]
 where
  follows =
    [ (asmRoot </> "savestate.nes", 400000)
    , (asmRoot </> "ppu_scene.nes", 60000)
    , (asmRoot </> "mmc5.nes", 60000) ]
    <> [ (romRoot </> "holy-mapperel" </> "M4_P256K_C256K.nes", 60000) | full ]
  loads =
    [ (asmRoot </> "savestate.nes", 220000)
    , (asmRoot </> "savestate.nes", 380000)
    , (romRoot </> "holy-mapperel" </> "M1_P128K_CR8K.nes", 300000)
    , (romRoot </> "holy-mapperel" </> "M4_P256K_C256K.nes", 300000) ]
    <> [ (romRoot </> "homebrew" </> "thwaite.nes", 400000) | full ]
