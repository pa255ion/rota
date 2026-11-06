{-|
Save states in the Clash core (side track N, PLAN.md N.5), on the core as
plain Haskell (its memories in vectors, as "A800.LockCore"):

* __Rest and the table follow the spec's.__ Core and reference model run
  side by side, step for step; after every step the core's 'restoreable'
  must be the spec's 'specAtRest', and where both are at rest the core's
  'tapTable' must be the spec's 'specTable', byte for byte.
* __A load.__ The core runs to a moment at rest; its table goes through
  the PS's routine builder (build/host/a800-restore); a fresh core with
  the same media and RAM's image runs the routine through 'kiOvl' (the
  overlay's byte at the CPU's address, until the end of the instruction
  that read the routine's end). Its table must then be the saved one but
  for the loose fields (as the spec's restore tests), RAM the same, and
  the two cores go on alike: the registers after every step.
-}
module A800.LockState
  ( stateTests
    -- * The core in IO (for the Clash save-state test, a800xl-savestate)
  , Run (..)
  , newRun
  , stepOf
  , warm
  , tableOf
  , atRest
  , regsOf
  , xexCfg
  , loose
  , diffText
  , buildOverlay
  ) where

import Prelude

import Control.Monad (forM_, replicateM, unless, when)
import Control.Monad.ST (RealWorld, stToIO)
import qualified Data.ByteString as BS
import Data.Bits ((.&.))
import Data.IORef
import Data.List (intercalate)
import Data.Maybe (isJust)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MV
import Data.Word
import Numeric (showHex)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.Process (readProcessWithExitCode)
import Test.Tasty
import Test.Tasty.HUnit

import qualified Clash.Prelude as C

import A800.Core
import A800.LockCore (coreCfgOf)
import A800.Media (Cart (..))
import A800.Spec (xexConfig)
import A800.Spec.Machine
import A800.Spec.SaveState
import A800.StateTable
import A800.StateTap
import A800.TestRoms
import Mos6502.Core (Cpu (..), Req (..), cpuRequest)
import Mos6502.Spec (step)

tool, scratch :: FilePath
tool = "../../build/host/a800-restore"
scratch = "../../build/a800xl/restore-test"

-- | A core running in IO: its state, RAM, the byte of its last read.
data Run = Run
  { rState :: IORef CoreState
  , rRam   :: MV.IOVector Word8
  , rRd    :: IORef Word8
  , rOvl   :: IORef (Maybe (U.Vector Word8, Word16, Bool)) -- ^ the overlay, its end, the end read
  , rCfg   :: Config
  , rCount :: IORef Int -- ^ machine cycles run (the state is forced every 256)
  }

newRun :: Config -> IO Run
newRun cfg = Run <$> newIORef (coreInit (coreCfgOf cfg)) <*> MV.replicate 65536 0 <*> newIORef 0 <*> newIORef Nothing <*> pure cfg <*> newIORef 0

-- | One machine cycle of the core (the clocks to its event).
cycleOf :: Run -> IO CycleEvent
cycleOf r = go
 where
  cfg = rCfg r
  rom sp a = case sp of
    SpOs -> idx (Just (cfgOs cfg)) a
    SpBasic -> idx (cfgBasic cfg) a
    SpCart -> idx (cartImage <$> cfgCart cfg) a
    SpLoader -> idx (xbLoader <$> cfgXex cfg) a
    SpXex -> idx (xbFile <$> cfgXex cfg) a
    SpRam -> 0
  idx mb a = case mb of
    Just b | fromIntegral a < BS.length b -> BS.index b (fromIntegral a)
    _ -> 0xFF
  go = do
    s <- readIORef (rState r)
    rdv <- readIORef (rRd r)
    ovl <- readIORef (rOvl r)
    let addr = rqAddr (cpuRequest (ksCpu s))
        ob = (\(img, _, _) -> img U.! (fromIntegral addr .&. (U.length img - 1))) <$> ovl
        (s', o) = coreStep s (CoreIn True (fromIntegral rdv) True 0xFF 0 0 0 0 (fromIntegral <$> ob) False)
    writeIORef (rState r) s'
    case koMem o of
      Just (MemReq SpRam a (Just v)) -> MV.write (rRam r) (fromIntegral a) (fromIntegral v)
      Just (MemReq SpRam a Nothing) -> MV.read (rRam r) (fromIntegral a) >>= writeIORef (rRd r)
      Just (MemReq sp a _) -> writeIORef (rRd r) (rom sp a)
      Nothing -> pure ()
    case koEvent o of
      Just e -> do
        -- force the whole state every 256 machine cycles: lazy Vec
        -- elements would otherwise keep old states alive
        k <- readIORef (rCount r)
        writeIORef (rCount r) (k + 1)
        when (k `mod` 256 == 0) (C.rnfX s' `seq` pure ())
        -- the overlay: its end read by the CPU, then off at that
        -- instruction's end
        case ovl of
          Just (img, end, seen)
            | seen && isJust (ceStep e) -> writeIORef (rOvl r) Nothing
            | ceCpu e && not (ceWrite e) && ceAddr e == fromIntegral end -> writeIORef (rOvl r) (Just (img, end, True))
          _ -> pure ()
        pure e
      Nothing -> go

-- | Cycles to the end of the next step; its kind.
stepOf :: Run -> IO ()
stepOf r = do
  e <- cycleOf r
  unless (isJust (ceStep e)) (stepOf r)

-- | The core's reset sequence (the spec runs its own in newMachine).
warm :: Run -> IO ()
warm = stepOf

tableOf :: Run -> IO (U.Vector Word8)
tableOf r = do
  s <- readIORef (rState r)
  pure (U.generate tableSize (\i -> fromIntegral (tapTable s (fromIntegral i))))

atRest :: Run -> IO Bool
atRest r = restoreable <$> readIORef (rState r)

regsOf :: Run -> IO [Int]
regsOf r = do
  c <- ksCpu <$> readIORef (rState r)
  pure (map fromIntegral [cA c, cX c, cY c, cS c, cP c .&. 0xCF] <> [fromIntegral (cPc c)])

xexCfg :: FilePath -> IO Config
xexCfg f = xexConfig <$> loadFile osRom <*> loadFile basicRom <*> loadFile loaderBin <*> loadFile f

-- | Core and spec side by side for @n@ steps after @skip@: rest and the
-- table must agree after every step; how many steps were at rest.
followsSpec :: Config -> Int -> Int -> IO Int
followsSpec cfg skip n = do
  m <- stToIO (newMachine cfg) :: IO (Machine RealWorld)
  r <- newRun cfg
  warm r
  let go i rests
        | i >= skip + n = pure rests
        | otherwise = do
            _ <- stToIO (step (mCpu m))
            stepOf r
            if i < skip then go (i + 1) rests else do
              sr <- stToIO (specAtRest m)
              cr <- atRest r
              when (sr /= cr) $ do
                rs <- stToIO (restReasons m)
                assertFailure ("step " <> show i <> ": the spec at rest " <> show sr <> " (" <> show rs <> "), the core " <> show cr)
              when sr $ do
                ts <- stToIO (specTable m)
                tc <- tableOf r
                unless (ts == tc) $ assertFailure ("step " <> show i <> ": the tables differ: " <> diffText ts tc)
              go (i + 1) (if sr then rests + 1 else rests)
  go (0 :: Int) (0 :: Int)

diffText :: U.Vector Word8 -> U.Vector Word8 -> String
diffText a b = intercalate ", "
  [ fieldOf o <> "@" <> showHex o "" <> " " <> showHex x "" <> "/" <> showHex y "" | (o, x, y) <- U.toList (U.zip3 (U.enumFromN 0 tableSize) a b), x /= y ]
 where
  fieldOf o = case [nm | (nm, fo, sz) <- tableFields, o >= fo, o < fo + sz] of
    nm : _ -> nm
    [] -> "?"

-- | Fields compared loosely (the spec's restore tests').
loose :: U.Vector Word8 -> [Int]
loose tbl = [tIrqst, tSkstat, tKbcode, tTrigLatch, tPenh, tPenv, tIrPrev, tMsc, tMsc + 1, tHires] <> [tColl .. tColl + 7]
  <> [tNmist | tbl U.! tNmist .&. 0x60 == 0x20] <> [tSerout | tbl U.! tSerial .&. 1 == 0]

-- | The PS's restore routine for a table (build/host/a800-restore): the
-- overlay and its end address.
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
  end <- case words err of
    ["end", e, "cycles", _] -> pure (read e :: Word16)
    _ -> assertFailure ("a800-restore said: " <> err) >> pure 0
  ovl <- U.fromList . BS.unpack <$> BS.readFile of'
  pure (ovl, end)

-- | Save the core after @n@ steps (on to rest), load it into a fresh
-- core, compare, go on @k@ steps.
loadCheck :: Config -> String -> Int -> Int -> Assertion
loadCheck cfg name n k = do
  r1 <- newRun cfg
  warm r1
  forM_ [1 .. n] (const (stepOf r1))
  let toRest i = do
        a <- atRest r1
        if a || i > 200000 then pure a else stepOf r1 >> toRest (i + 1 :: Int)
  ok <- toRest 0
  unless ok (assertFailure "no moment at rest")
  tbl <- tableOf r1
  (ovl, end) <- buildOverlay ("core-" <> name) tbl
  -- the PS holds the core (power-on), writes RAM, arms the overlay, lets go
  r2 <- newRun cfg
  forM_ [0 .. 65535] $ \i -> MV.read (rRam r1) i >>= MV.write (rRam r2) i
  writeIORef (rOvl r2) (Just (ovl, end, False))
  let run i = do
        o <- readIORef (rOvl r2)
        case o of
          Nothing -> pure True
          Just _ | i > 100000 -> pure False
                 | otherwise -> stepOf r2 >> run (i + 1 :: Int)
  done <- run 0
  unless done (assertFailure "the restore routine didn't end")
  tbl2 <- tableOf r2
  let diffs = [ o | o <- [0 .. tableSize - 1], o `notElem` loose tbl
                  , (if o == tP then (.&. 0xCF) else id) (tbl U.! o) /= (if o == tP then (.&. 0xCF) else id) (tbl2 U.! o) ]
  unless (null diffs) (assertFailure ("restored table differs: " <> diffText tbl tbl2))
  ram1 <- U.freeze (rRam r1)
  ram2 <- U.freeze (rRam r2)
  assertBool "RAM differs" (ram1 == ram2)
  t1 <- replicateM k (stepOf r1 >> regsOf r1)
  t2 <- replicateM k (stepOf r2 >> regsOf r2)
  case [ (i, a, b) | (i, a, b) <- zip3 [0 :: Int ..] t1 t2, a /= b ] of
    (i, a, b) : _ -> assertFailure ("the cores part at step " <> show i <> ": " <> show a <> " / " <> show b)
    [] -> pure ()

-- | The tests; Scorch's (some 20 minutes each) in the full tier only.
stateTests :: Bool -> TestTree
stateTests fullTier = testGroup "save states in the core (side track N, N.5)" $
  [ testCase "rasterbars.xex: rest and the table as the spec's, 30000 steps" $ do
      cfg <- xexCfg (asmRoot </> "rasterbars.xex")
      rests <- followsSpec cfg 800000 30000
      assertBool ("only " <> show rests <> " steps at rest") (rests > 100)
  , testCase "regs.xex: rest and the table as the spec's, 30000 steps" $ do
      cfg <- xexCfg (asmRoot </> "regs.xex")
      rests <- followsSpec cfg 800000 30000
      assertBool ("only " <> show rests <> " steps at rest") (rests > 100)
  , testCase "regs.xex: a load through the core" $ do
      cfg <- xexCfg (asmRoot </> "regs.xex")
      loadCheck cfg "regs" 800000 60000
  ] <> if not fullTier then [testGroup "(Scorch: full tier, EMU_FULL_TESTS=1)" []] else
  [ testCase "Scorch: rest and the table as the spec's, 30000 steps" $ do
      cfg <- xexCfg scorchXex
      rests <- followsSpec cfg 3000000 30000
      assertBool ("only " <> show rests <> " steps at rest") (rests > 100)
  , testCase "Scorch: a load through the core" $ do
      cfg <- xexCfg scorchXex
      loadCheck cfg "scorch" 3000000 60000
  ]
