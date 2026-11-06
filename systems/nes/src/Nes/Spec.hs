{-# LANGUAGE MultiWayIf #-}
{-|
Reference model of the NTSC NES (PLAN.md steps 3.2 and on): the shared
6502 ("Mos6502.Spec") in the machine of "Nes.Spec.Machine".

Two interfaces, as "Gb.Spec":

* __'ST'__: "Nes.Spec.Machine" ('newMachine', 'stepCpu', 'withTrace',
  ...) for tests that look inside, and for the lockstep tests of steps
  3.4 to 3.6.
* __Pure__: 'runBlargg' runs a ROM until Blargg's protocol reports a
  result; 'runNestest' runs nestest in its automation mode and compares
  every instruction with the log.
-}
module Nes.Spec
  ( -- * Blargg
    BlarggRun (..)
  , runBlargg
    -- * nestest
  , runNestest
    -- * Screens
  , runFrames
  , runFramesWith
  , runFramesPress
  , OldResult (..)
  , runOldScreenTest
    -- * AccuracyCoin
  , runAccuracyCoin
    -- * Debugging
  , traceTail
  , accuracyCoinTrace
  ) where

import Prelude

import Control.Monad.ST (ST, runST)
import qualified Data.ByteString as BS
import Data.Foldable (toList)
import Data.List (isInfixOf)
import qualified Data.Sequence as Seq
import Data.Word
import Numeric (showHex)

import Mos6502.Spec (Regs (..), getRegs, setRegs, Step (..))
import Nes.AccuracyCoin
import Nes.Cartridge (Rom (..), parseRom)
import Nes.Hooks
import Nes.Screen
import Nes.Spec.Machine
import Nes.Spec.Ppu (ppuFrame, ppuFrames, ppuFramesDone, ppuPosition)

data BlarggRun = BlarggRun
  { brResult :: Blargg -- ^ the last protocol state seen
  , brCycles :: Int    -- ^ CPU cycles run
  , brResets :: Int    -- ^ resets the ROM asked for
  } deriving stock (Show, Eq)

-- | Run until the protocol (or, for the older shell, the $4016 serial
-- output) reports a final status, or for at most this many CPU cycles. A reset request ($81) is answered after 1/5 s
-- (Blargg asks for at least 100 ms).
runBlargg :: Int -> BS.ByteString -> Either String BlarggRun
runBlargg limit rom = runST $ do
  em <- newMachine rom
  case em of
    Left e -> pure (Left e)
    Right m -> Right <$> loop m 0 Nothing
 where
  resetDelay = 357954 :: Int
  loop :: Machine s -> Int -> Maybe Int -> ST s BlarggRun
  loop m resets resetAt = do
    runFor m 4096
    n <- machineCycles m
    mem <- blargg <$> peekRange m
    -- The older shell reports through $4016 instead (Nes.Hooks).
    res <- case mem of
      NoSignature -> serialVerdict . serialText <$> serialBits m
      _ -> pure mem
    case res of
      Done _ _ -> pure (BlarggRun res n resets)
      _ | n >= limit -> pure (BlarggRun res n resets)
      NeedsReset -> case resetAt of
        Nothing -> loop m resets (Just (n + resetDelay))
        Just t | n >= t -> do
                   resetMachine m
                   loop m (resets + 1) Nothing
               | otherwise -> loop m resets resetAt
      _ -> loop m resets Nothing

runFor :: Machine s -> Int -> ST s ()
runFor m k = do
  start <- machineCycles m
  let go = do
        _ <- stepCpu m
        n <- machineCycles m
        if n - start < k then go else pure ()
  go

-- | The protocol's $6000-$7FFF as a pure reader. Only the status and
-- the signature are read until they say the test is over (the text is
-- read then).
peekRange :: Machine s -> ST s (Word16 -> Word8)
peekRange m = do
  hd <- mapM (peekCpu m) [0x6000 .. 0x6003]
  let final = drop 1 hd == blarggSignature && take 1 hd `notElem` [[0x80], [0x81]]
  bs <- if final then mapM (peekCpu m) [0x6000 .. 0x7FFF] else pure hd
  let v = BS.pack bs
  pure (\a -> let i = fromIntegral a - 0x6000 in if i < BS.length v then BS.index v i else 0)

-- | Run nestest from $C000 (P = $24, as the log starts) and compare each
-- instruction's starting registers, cycle count and PPU position with the
-- log. 'Left'
-- names the first line that differs; 'Right' the number of lines checked.
runNestest :: BS.ByteString -> [NestestLine] -> Either String Int
runNestest rom expected = runST $ do
  em <- newMachine rom
  case em of
    Left e -> pure (Left e)
    Right m -> do
      r <- getRegs (mCpu m)
      setRegs (mCpu m) r { rPC = 0xC000, rP = 0x24 }
      go m (zip [1 :: Int ..] expected)
 where
  go :: Machine s -> [(Int, NestestLine)] -> ST s (Either String Int)
  go _ [] = pure (Right (length expected))
  go m ((i, want) : rest) = do
    r <- getRegs (mCpu m)
    n <- machineCycles m
    (sl0, dot0) <- ppuPosition (mPpu m)
    -- The log shows the dot about to run; the model holds the last one run.
    let (sl, dot) | dot0 < 340 = (sl0, dot0 + 1)
                  | otherwise = (if sl0 == 261 then 0 else sl0 + 1, 0)
        got = want { nlPC = rPC r, nlA = rA r, nlX = rX r, nlY = rY r, nlP = rP r, nlSP = rS r, nlCycle = n
                   , nlScanline = sl, nlDot = dot }
    if got /= want
      then pure (Left ("line " <> show i <> ": expected " <> show want <> ", got " <> show got))
      else do
        s <- stepCpu m
        case s of
          Instr _ -> go m rest
          other -> pure (Left ("line " <> show i <> ": " <> show other))

-- | Debugging aid: run like 'runBlargg' and return the last @k@
-- instructions before the stop (PC, opcode, registers before it, CPU
-- cycle), oldest first.
traceTail :: Int -> Int -> BS.ByteString -> Either String [String]
traceTail k limit rom = runST $ do
  em <- newMachine rom
  case em of
    Left e -> pure (Left e)
    Right m -> Right <$> go m Seq.empty
 where
  go :: Machine s -> Seq.Seq String -> ST s [String]
  go m acc = do
    (line, n) <- traceStep m
    let acc' = Seq.drop (Seq.length acc + 1 - k) (acc Seq.|> line)
    st <- blargg <$> peekRange m
    case st of
      Done _ _ -> pure (toList acc')
      _ | n >= limit -> pure (toList acc')
        | otherwise -> go m $! acc' -- forced: a lazy chain of Seqs grows by one per instruction

-- | One instruction, and its line for a trace: PC, opcode, registers
-- before it, CPU cycle, what the step was.
traceStep :: Machine s -> ST s (String, Int)
traceStep m = do
  r <- getRegs (mCpu m)
  n <- machineCycles m
  op <- peekCpu m (rPC r)
  s <- stepCpu m
  pure ( hex4 (rPC r) <> " " <> hex2 op <> "  A:" <> hex2 (rA r) <> " X:" <> hex2 (rX r)
         <> " Y:" <> hex2 (rY r) <> " P:" <> hex2 (rP r) <> " S:" <> hex2 (rS r)
         <> " CYC:" <> show n <> " " <> show s
       , n )
 where
  hex2 v = let x = showHex v "" in replicate (2 - length x) '0' <> x
  hex4 v = let x = showHex v "" in replicate (4 - length x) '0' <> x

-- | Debugging aid: start AccuracyCoin's "run all" as 'runAccuracyCoin'
-- does, run it for @limit@ CPU cycles or until the CPU is about to run
-- the instruction at @stop@ (once at least the given number of cycles
-- have run), and return the last @k@ instructions (as
-- 'traceTail') and CPU RAM ($0000-$07FF).
accuracyCoinTrace :: Int -> Int -> Maybe (Word16, Int) -> BS.ByteString -> Either String ([String], [Word8])
accuracyCoinTrace k limit stop file = runST $ do
  em <- newMachine file
  case em of
    Left e -> pure (Left e)
    Right m -> do
      startRunAll m
      let go acc = do
            (line, n) <- traceStep m
            pc <- rPC <$> getRegs (mCpu m)
            let acc' = Seq.drop (Seq.length acc + 1 - k) (acc Seq.|> line)
            if n >= limit || maybe False (\(s, after) -> pc == s && n >= after) stop then pure (toList acc') else go $! acc'
      t <- go Seq.empty
      ram <- mapM (peekCpu m) [0 .. 0x7FF]
      pure (Right (t, ram))

-- | Run until @n@ frames have been drawn and return the last one.
runFrames :: Int -> BS.ByteString -> Either String Screen
runFrames = runFramesWith 0

-- | The same with these buttons held on controller 1 (bit 0 A to bit 7
-- Right).
runFramesWith :: Word8 -> Int -> BS.ByteString -> Either String Screen
runFramesWith = runFramesPress 0

-- | The same with the buttons pressed from frame @from@ on (a press that
-- a game sees as one: they were up before).
runFramesPress :: Int -> Word8 -> Int -> BS.ByteString -> Either String Screen
runFramesPress from pad n rom = runST $ do
  em <- newMachine rom
  case em of
    Left e -> pure (Left e)
    Right m -> do
      let go = do
            d <- ppuFramesDone (mPpu m)
            setButtons m 1 (if d >= from then pad else 0)
            _ <- stepCpu m
            if d < n then go else pure ()
      go
      mkScreen <$> ppuFrame (mPpu m)

-- | What the 2005 Blargg ROMs (sprite hit and overflow tests) show: the
-- result code at $F8 (1 = passed, otherwise the failed test's number) and
-- the screen text with "PASSED" or "FAILED #n".
data OldResult = OldResult
  { orCode   :: Word8
  , orText   :: [String]
  , orCycles :: Int
  } deriving stock (Show, Eq)

-- | Run until the screen says PASSED or FAILED, or for at most this many
-- CPU cycles.
runOldScreenTest :: Int -> BS.ByteString -> Either String OldResult
runOldScreenTest limit rom = runST $ do
  em <- newMachine rom
  case em of
    Left e -> pure (Left e)
    Right m -> do
      let go = do
            runFor m 30000
            t <- nametableText m
            n <- machineCycles m
            if any (\l -> "PASSED" `isInfixOf` l || "FAILED" `isInfixOf` l) t || n >= limit
              then do
                -- Let the line finish printing (the failure number).
                runFor m 60000
                t' <- nametableText m
                code <- peekCpu m 0xF8
                pure (OldResult code (filter (any (/= ' ')) t') n)
              else go
      Right <$> go

-- | Run AccuracyCoin's "run all" (Start on the first menu entry, after 1 s)
-- and return every page's entries with their result bytes, the CPU cycles
-- it took, and the results table it draws. 'Left' if the run doesn't finish within the limit.
runAccuracyCoin :: Int -> BS.ByteString -> Either String ([(String, [(Entry, Word8)])], Int, Screen)
runAccuracyCoin limit file = do
  rom <- parseRom file
  pages <- parsePages (romPrg rom)
  runST $ do
    em <- newMachine file
    case em of
      Left e -> pure (Left e)
      Right m -> do
        startRunAll m
        -- Done once the flag has been set and then stayed clear for 3 s
        -- (some tests clear zero page for a moment).
        let quiet = 3 * 1789773
            wait started clearSince = do
              runFor m 10000
              r <- peekCpu m runningAllTests
              n <- machineCycles m
              if | n >= limit -> pure False
                 | r /= 0 -> wait True n
                 | started && n - clearSince >= quiet -> pure True
                 | otherwise -> wait started clearSince
        ok <- wait False 0
        if not ok
          then pure (Left "AccuracyCoin: run-all didn't finish")
          else do
            runFrames' m 30
            n <- machineCycles m
            res <- mapM (\pg -> (,) (pgName pg) <$> mapM (\e -> (,) e <$> peekCpu m (enResult e)) (pgEntries pg)) pages
            sc <- mkScreen <$> ppuFrame (mPpu m)
            pure ((\s -> (res, n, s)) <$> sc)

-- | Wait 1 s on AccuracyCoin's menu, then press Start on its first entry
-- (run all).
startRunAll :: Machine s -> ST s ()
startRunAll m = do
  runFrames' m 60
  setButtons m 1 0x08
  runFrames' m 5
  setButtons m 1 0

-- | Run until @k@ more frames have been drawn.
runFrames' :: Machine s -> Int -> ST s ()
runFrames' m k = do
  start <- ppuFrames (mPpu m)
  let go = do
        _ <- stepCpu m
        f <- ppuFrames (mPpu m)
        if f - start < k then go else pure ()
  go
