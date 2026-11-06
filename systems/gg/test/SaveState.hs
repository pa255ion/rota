{-# LANGUAGE FlexibleContexts #-}
-- | Layer 2, side track N (PLAN.md N.2): the Game Gear's and Master
-- System's save states in Clash simulation: the core ("Gg.Core") with its
-- memories and the hooks ("Gg.Hooks"), as "Gg.Machine" and "Sms.Machine"
-- wire them.
--
-- * Save: a core (B) runs a ROM, is asked to freeze, goes on to a moment a
--   state can be taken and freezes; its state table, RAM and VRAM are read
--   through the windows; it goes on. It runs as a core never frozen does,
--   and its table is the spec's after as many instructions (every field,
--   but RAM's DFFCh-DFFFh, which the PS fills, and the FM unit's).
-- * Load: a fresh core (C), held and frozen, gets the memories, cartridge
--   RAM and the restore routine (build/host/gg-restore, from B's table) in
--   its ROM overlay, and is let go. After the routine its instructions are
--   B's after the save, register for register.
module Main (main) where

import Prelude

import qualified Clash.Prelude as C
import Clash.Prelude (BitVector, Signal, Unsigned)
import Control.Monad (unless, when)
import Control.Monad.ST (stToIO)
import qualified Data.ByteString as BS
import Data.Bits (shiftR, (.&.))
import Data.List (findIndex)
import Data.Maybe (isJust, mapMaybe)
import qualified Data.Vector.Unboxed as U
import Data.Word (Word8)
import Numeric (showHex)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.Process (readProcessWithExitCode)
import Test.Tasty
import Test.Tasty.HUnit

import Gg.Core
import Gg.Hooks
import qualified Gg.Spec as Spec
import Gg.StateTable (tFmCtl, tFmRegs, tRamTail, tableFields, tableSize)
import Platform.Ps.Link (LinkAddr)
import Z80.Core (Cpu (..), Kind (..), Req (..), Z80T (..))

data TbIn = TbIn
  { tiLink  :: Maybe (LinkAddr, BitVector 8)
  , tiPause :: Bool
  , tiHeld  :: Bool
  , tiCart  :: Maybe (Unsigned 15, Unsigned 8) -- ^ the PS's cartridge RAM write (held)
  } deriving stock (C.Generic)
    deriving anyclass C.NFDataX

data TbOut = TbOut
  { toCore   :: GgC
  , toOut    :: GgOut
  , toActive :: Bool
  , toByte   :: Maybe (BitVector 8)
  , toFrozen :: Bool
  } deriving stock (C.Generic)
    deriving anyclass C.NFDataX

-- | The core and its memories, the hooks around them; the ROM answers a
-- clock after each request; a clock is active unless the hooks gate it.
tb :: C.HiddenClockResetEnable C.System => Bool -> Mapper -> Int -> BS.ByteString -> Signal C.System TbIn -> Signal C.System TbOut
tb sms mapper pow2 rom inp = TbOut <$> core <*> out <*> active <*> hoByte ho <*> hoFrozen ho
 where
  held = tiHeld <$> inp
  link = tiLink <$> inp
  pause = tiPause <$> inp
  ho = ggStateHooks link pause held (pure False) core out active ramD vramD
  active = not <$> hoGate ho
  -- (a block RAM's output is undefined in a simulation's first clock, and
  -- the core's inputs are strict: 0 there, in this testbench only)
  defined :: C.NFDataX a => a -> Signal C.System a -> Signal C.System a
  defined z = fmap (\x -> maybe z id (C.maybeIsX x))
  gin = GgIn <$> active <*> romD <*> defined 0 ramD <*> defined 0 cartD <*> defined (C.repeat 0) vramD <*> pure 0 <*> pure False <*> pure mapper <*> pure (fromIntegral pow2)
             <*> pure 0xFF <*> pure 0 <*> pure (if sms then 0 else 1) <*> hoOvl ho
  -- (no FM unit: the spec it is compared with has none, and N.2 restores none)
  power = if sms then smsPowerOn False False mapper (fromIntegral pow2) else ggPowerOn False mapper (fromIntegral pow2)
  (core, out) = C.unbundle (C.withReset (C.unsafeFromActiveHigh held)
                  (C.mealy (\s i -> let (s', o) = ggClock i s in (s', (s, o))) power gin))
  romD = C.register 0 (romAt . goRomAddr <$> out)
  romAt a = let i = fromIntegral a `mod` BS.length rom in fromIntegral (BS.index rom i)
  ramD = C.blockRam1 C.NoClearOnReset (C.SNat @8192) 0 (hoRamAddr ho) (hoRamWrite ho)
  cartOp :: GgOut -> C.RamOp 32768 (Unsigned 8)
  cartOp o = case goCartWrite o of
    Just (a, v) -> C.RamWrite (fromIntegral a) v
    Nothing -> C.RamRead (fromIntegral (goCartAddr o))
  psOp :: Maybe (Unsigned 15, Unsigned 8) -> C.RamOp 32768 (Unsigned 8)
  psOp w = maybe (C.RamRead 0) (\(a, v) -> C.RamWrite (fromIntegral a) v) w
  (cartD, _) = C.trueDualPortBlockRam (cartOp <$> out) (psOp . tiCart <$> inp)
    :: (Signal C.System (Unsigned 8), Signal C.System (Unsigned 8))
  bank k = C.blockRam1 C.NoClearOnReset (C.SNat @4096) 0 (hoVramRead ho) (bankWrite k <$> hoVramWrite ho)
  vramD = C.bundle (bank 0 C.:> bank 1 C.:> bank 2 C.:> bank 3 C.:> C.Nil)
  bankWrite :: Unsigned 2 -> Maybe (Unsigned 14, Unsigned 8) -> Maybe (Unsigned 12, Unsigned 8)
  bankWrite k w = case w of
    Just (a, b) | C.resize a == k -> Just (C.resize (a `shiftR` 2), b)
    _ -> Nothing

data Rom = Rom { rSms :: Bool, rMapper :: Mapper, rPow2 :: Int, rBytes :: BS.ByteString }

loadRom :: Bool -> FilePath -> IO Rom
loadRom sms path = do
  b <- BS.readFile path
  let banks = max 1 ((BS.length b + 0x3FFF) `div` 0x4000)
      pow2 = until (>= banks) (* 2) 1
      full = b <> BS.replicate (pow2 * 0x4000 - BS.length b) 0xFF
      mapper = case Spec.detectMapper b of
        Spec.Codemasters -> MCodemasters
        Spec.SegaMapper -> MSega
  pure (Rom sms mapper pow2 full)

runs :: Rom -> [TbIn] -> [TbOut]
runs r ins = take (length ins) (C.simulate (C.withClockResetEnable C.clockGen C.resetGen C.enableGen
  (tb (rSms r) (rMapper r) (rPow2 r) (rBytes r))) ins)

quiet :: Bool -> Bool -> TbIn
quiet p h = TbIn Nothing p h Nothing

going :: Int -> [TbIn]
going n = replicate n (quiet False False)

-- | A link write: the address bytes are the link's, here one write per clock.
wr :: Bool -> Bool -> LinkAddr -> BitVector 8 -> TbIn
wr p h a d = TbIn (Just (a, d)) p h Nothing

memAddrs :: [Unsigned 16]
memAddrs = [0x2000 .. 0x5FFF] ++ [0x8000 .. 0x9FFF]

-- | The registers after each instruction's end (the state a clock after).
ends :: [TbOut] -> [[Int]]
ends outs = [ regs (toCore b) | (a, b) <- zip outs (drop 1 outs), toActive a && goInstrEnd (toOut a) ]
 where
  regs g = let c = zCpu (gZ g) in map fromIntegral [cA c, cF c, cB c, cC c, cD c, cE c, cH c, cL c, cI c, cR c]
                                  <> map fromIntegral [cIX c, cIY c, cSP c, cPC c] <> map fromEnum [cIFF1 c, cIFF2 c, cHalted c]

tool, scratch :: FilePath
tool = "../../build/host/gg-restore"
scratch = "../../build/gg/restore-test"

buildOverlay :: String -> [Word8] -> IO ([Word8], Int)
buildOverlay name tbl = do
  have <- doesFileExist tool
  unless have (assertFailure (tool <> " is missing: make gg-restore"))
  createDirectoryIfMissing True scratch
  let tf = scratch </> ("clash-" <> name <> ".table")
      of' = scratch </> ("clash-" <> name <> ".overlay")
  BS.writeFile tf (BS.pack tbl)
  (code, _, err) <- readProcessWithExitCode tool [tf, of'] ""
  when (code /= ExitSuccess) (assertFailure ("gg-restore: " <> err))
  ovl <- BS.unpack <$> BS.readFile of'
  case words err of
    ["end", e, "tstates", _] -> pure (ovl, read e)
    _ -> assertFailure ("gg-restore said: " <> err) >> pure (ovl, 0)

-- | The spec's table after @k@ instructions.
specTableAt :: Rom -> Int -> IO (U.Vector Word8)
specTableAt r k = do
  m <- stToIO (if rSms r then Spec.newMasterSystem Nothing (Spec.detectMapper (rBytes r)) (rBytes r) else Spec.newMachine (rBytes r))
  stToIO (sequence_ (replicate k (Spec.stepMachine m)))
  stToIO (Spec.specTable m)

check :: Bool -> String -> FilePath -> Int -> Int -> Assertion
check sms name path n m = do
  r <- loadRom sms path
  -- where B freezes, asked at n (a first run, to place the reads)
  let ask = [wr True False stateModeAddr 1]
      probe = runs r (going n ++ ask ++ replicate 800000 (quiet True False))
  z <- case findIndex toFrozen (drop n probe) of
    Just i -> pure (n + i)
    Nothing -> assertFailure "B never froze" >> pure 0
  let wait = z - n + 2
      rdOps = [wr True False (stateRBase + fromIntegral a) 0 | a <- [0x1F00] ++ [0 .. fromIntegral tableSize - 1] ++ memAddrs]
      thaw = [wr True False stateModeAddr 0, quiet False False]
      outB = runs r (going n ++ ask ++ replicate (wait - 1) (quiet True False) ++ rdOps ++ replicate 2 (quiet True False) ++ thaw ++ going m)
      got = mapMaybe toByte (drop (n + wait) outB)
      (status, rest) = splitAt 1 got
      tbl = map (fromIntegral . C.pack) (take tableSize rest) :: [Word8]
      mem = drop tableSize rest
      nB = n + wait + length rdOps + 2 + length thaw
      afterB = ends (drop nB outB)
  status @?= [1]
  length mem @?= length memAddrs
  -- the core's table is the spec's after as many instructions
  spec <- specTableAt r (length (ends (take z outB)))
  let skip = [tRamTail .. tRamTail + 3] <> [tFmCtl] <> [tFmRegs .. tFmRegs + 63]
      diffs = [ (o, a, b) | (o, a, b) <- zip3 [0 :: Int ..] tbl (U.toList spec), o `notElem` skip, a /= b ]
      fieldOf o = case [nm | (nm, fo, sz) <- tableFields, o >= fo, o < fo + sz] of
        nm : _ -> nm
        [] -> "?"
  unless (null diffs) $ assertFailure $ "the core's table differs from the spec's: " <> unwords
    [ fieldOf o <> "@" <> showHex o "" <> " core " <> showHex a "" <> " spec " <> showHex x "" | (o, a, x) <- take 12 diffs ]
  -- the routine (with RAM's DFFCh-DFFFh, as the PS fills them)
  let ramB = drop (length memAddrs - 0x2000) mem
      tail4 = map (fromIntegral . C.pack) (take 4 (drop 0x1FFC ramB)) :: [Word8]
      tbl' = take tRamTail tbl <> tail4 <> drop (tRamTail + 4) tbl
  (ovl, end) <- buildOverlay name tbl'
  let writes = [wr True True (stateWBase + fromIntegral a) v | (a, v) <- zip memAddrs mem]
            ++ [wr True True (stateWBase + fromIntegral overlayBase + fromIntegral i) (fromIntegral v) | (i, v) <- zip [0 :: Int ..] ovl]
            ++ [ wr True True (stateWBase + fromIntegral overlayEndAddr) (fromIntegral (end .&. 0xFF))
               , wr True True (stateWBase + fromIntegral overlayEndAddr + 1) (fromIntegral (end `shiftR` 8))
               , wr True True (stateWBase + fromIntegral overlayEndAddr + 2) 1 ]
      start = [wr True True stateModeAddr 1] ++ replicate 4 (quiet True True)
      outC = runs r (start ++ writes ++ [quiet True False, wr True False stateModeAddr 0] ++ going (m + 4000000))
      afterGo = drop (length start + length writes + 2) outC
      endRead = findIndex (\o -> let rq = cReq (zCpu (gZ (toCore o))) in toActive o && rqKind rq == KRead && rqAddr rq == fromIntegral end) afterGo
      halted = tbl !! 0x23 /= 0
  case endRead of
    Nothing -> assertFailure "the loaded core never read the routine's last byte"
    Just i -> do
      let afterC = drop (if halted then 2 else 1) (ends (drop i afterGo))
          len = min (length afterB) (length afterC)
      assertBool ("too few instructions after the load: " <> show len) (len > 500)
      case [ (k, x, y) | (k, x, y) <- zip3 [0 :: Int ..] (take len afterB) (take len afterC), x /= y ] of
        (k, x, y) : _ -> assertFailure ("the loaded core parts from the saved one at instruction " <> show k
                                        <> " of " <> show len <> ":\n  saved  " <> show x <> "\n  loaded " <> show y)
        [] -> pure ()

asmDir :: FilePath
asmDir = "../../build/gg/asm"

main :: IO ()
main = do
 full <- (== Just "1") <$> lookupEnv "EMU_FULL_TESTS"
 defaultMain $ testGroup "gg-savestate (side track N, N.2)" $
  [ testCase "tone.gg" $ check False "tone" (asmDir </> "tone.gg") 200000 400000
  , testCase "sprites.gg" $ check False "sprites" (asmDir </> "sprites.gg") 200000 400000
  , testCase "the status bytes: the table's size and version, frozen or not" $ do
      r <- loadRom False (asmDir </> "tone.gg")
      let o = runs r (going 2000 ++ [wr False False (stateRBase + 0x1F01) 0, wr False False (stateRBase + 0x1F02) 0, wr False False (stateRBase + 0x1F03) 0] ++ going 3)
      map (fmap (fromIntegral . C.pack :: BitVector 8 -> Int)) (take 3 (drop 2001 (map toByte o))) @?= map Just [0x40, 0x01, 1]
  , testCase "the hooks idle: no state output, never frozen" $ do
      r <- loadRom False (asmDir </> "tone.gg")
      let o = runs r (going 20000)
      assertBool "state output" (not (any (isJust . toByte) o))
      assertBool "frozen" (not (any toFrozen o))
  ] <>
  (if full
     then [ testCase "vdpint.gg (halted, waiting for its interrupts)" $ check False "vdpint" (asmDir </> "vdpint.gg") 200000 400000
          , testCase "Master System: Raposa do Sol" $ check True "sms-raposa" "../../roms/sms/games/raposadosol_v20.sms" 2000000 400000 ]
     else [])
