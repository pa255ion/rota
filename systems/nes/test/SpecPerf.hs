{-# LANGUAGE MultiWayIf #-}
-- | The NES's performance version ("Nes.SpecPerf", the browser's; side
-- track W) is the same machine as the fast version ("Nes.SpecFast", which
-- nes-specfast checks against the reference model). Both run side by side
-- on the same inputs: after every instruction the CPU cycle count, the
-- CPU's registers and the APU's outputs of every cycle; at every frame the
-- picture, the 2 KiB of RAM and the 2 KiB of nametable RAM; and once more
-- a frame at a time through 'P.runFrame' (the browser's loop), the
-- frame's sound, picture and RAM. The buttons are a seeded sequence, so
-- menus and games are driven. Quick tier: a few test ROMs, Thwaite and a
-- mapper of each kind briefly; full tier (EMU_FULL_TESTS=1): longer, and
-- every Blargg test, Holy Mapperel ROM and AccuracyCoin, and the user's
-- games in roms/nes/games/local if there are any.
module Main (main) where

import Prelude

import Control.Monad (when)
import Control.Monad.ST (ST, runST)
import Data.Bits (shiftL, shiftR, xor, (.&.), (.|.))
import qualified Data.ByteString as BS
import Data.List (isSuffixOf, sort)
import qualified Data.Vector.Unboxed as U
import Data.Word (Word32, Word8)
import System.Directory (doesDirectoryExist, listDirectory)
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import Test.Tasty
import Test.Tasty.HUnit

import qualified Mos6502.SpecFast as FC
import qualified Nes.SpecFast.Machine as F
import qualified Nes.SpecFast.Ppu as FP
import qualified Nes.SpecPerf.Cpu as PC
import qualified Nes.SpecPerf.Machine as P
import qualified Nes.SpecPerf.Ppu as PP
import Nes.TestRoms

main :: IO ()
main = do
  full <- maybe False (not . null) <$> lookupEnv "EMU_FULL_TESTS"
  let n q f = if full then f else q
      under d = do
        ok <- doesDirectoryExist d
        if not ok then pure [] else do
          es <- sort <$> listDirectory d
          concat <$> mapM (\e -> if ".nes" `isSuffixOf` e then pure [d </> e] else under (d </> e)) es
  blargg <- under (romRoot </> "blargg")
  mapperel <- under (romRoot </> "holy-mapperel")
  local <- under (romRoot </> "games" </> "local")
  let quickMapperel = filter (\f -> any (`isSuffixOf` f) ["M1_P128K_C32K.nes", "M4_P256K_C256K.nes", "M7_P128K.nes"]) mapperel
      rom f k = testCase f (same k =<< loadRomFile f)
  defaultMain $ testGroup "nes-specperf: the performance version equals the fast version"
    [ rom (romRoot </> "nestest" </> "nestest.nes") (n 60 300)
    , rom (romRoot </> "homebrew" </> "thwaite.nes") (n 400 3000)
    , rom (romRoot </> "homebrew" </> "dabg.nes") (n 200 2000)
    , testCase "Thwaite with every other frame's picture skipped: the same CPU, RAM and sound"
        (sameSkip (n 400 3000) =<< loadRomFile (romRoot </> "homebrew" </> "thwaite.nes"))
    , testCase "sprite 0 hit with every other frame skipped (Blargg's 01.basics)"
        (sameSkip 300 =<< loadRomFile (romRoot </> "blargg" </> "sprite_hit_tests_2005.10.05" </> "01.basics.nes"))
    , testGroup "the reset button pressed now and then (the browser's RESET): the same machine"
        [ testCase f (sameReset k =<< loadRomFile f)
        | (f, k) <- [ (romRoot </> "homebrew" </> "thwaite.nes", n 400 2000), (asmRoot </> "mmc3_bg1000.nes", n 120 400)
                    , (romRoot </> "mmc5" </> "mmc5test_v2.nes", n 200 600) ] ]
    , testGroup "the clocks rebased (the browser's 32-bit Int) against a twin without: the same machine"
        [ testCase f (sameTwin k =<< loadRomFile f)
        | (f, k) <- [ (romRoot </> "homebrew" </> "thwaite.nes", n 1500 6000), (romRoot </> "holy-mapperel" </> "M1_P128K_C32K.nes", n 1500 6000) ] ]
    , testGroup "a frame at a time (runFrame, the browser's loop)"
        [ testCase f (byFrame k =<< loadRomFile f)
        | (f, k) <- [ (romRoot </> "homebrew" </> "thwaite.nes", n 400 3000), (asmRoot </> "mmc3_bg1000.nes", n 30 120)
                    , (romRoot </> "mmc5" </> "mmc5test_v2.nes", n 120 600) ] ]
    , rom (asmRoot </> "ppu_scene.nes") (n 30 120)
    , rom (asmRoot </> "apu_mix.nes") (n 60 300)
    , rom (asmRoot </> "tone.nes") (n 60 300)
    , rom (asmRoot </> "mmc3_bg1000.nes") (n 30 120)
    , rom (asmRoot </> "mmc5.nes") (n 60 300)
    , rom (romRoot </> "mmc5" </> "mmc5test_v2.nes") (n 120 600)
    , testGroup "Holy Mapperel" [ rom f (n 40 200) | f <- if full then mapperel else quickMapperel ]
    , testGroup "Blargg" [ rom f 600 | full, f <- blargg ]
    , testGroup "AccuracyCoin" [ rom (romRoot </> "accuracycoin-d103cbd" </> "AccuracyCoin.nes") 3000 | full ]
    , testGroup "the user's games" [ rom f 3000 | full, f <- local ]
    ]
 where
  same frames file = either assertFailure (const (pure ())) (lockstep False [] frames file)
  -- (pressed at frames 37, 100, 163, ...: at once after a frame's end)
  sameReset frames file = either assertFailure (const (pure ())) (lockstep False [37, 100 .. frames] frames file)
  byFrame frames file = either assertFailure (const (pure ())) (frameStep frames file)
  sameTwin frames file = either assertFailure (const (pure ())) (rebaseTwin frames file)
  sameSkip frames file = either assertFailure (const (pure ())) (lockstep True [] frames file)

-- | The buttons at frame @k@: a new set every 8 frames from a xorshift
-- seed, Start and Select rarer (a game paused all the time tests little).
buttons :: Int -> Word8
buttons k =
  let r = rand (fromIntegral (k `div` 8) + 1)
      dirs = fromIntegral (r .&. 0xF3) :: Word8
      startSel = if (r `shiftR` 8) .&. 7 == 0 then fromIntegral ((r `shiftR` 11) .&. 0x0C) else 0
  in dirs .|. startSel
 where
  rand :: Word32 -> Word32
  rand s0 = let a = s0 `xor` (s0 `shiftL` 13); b = a `xor` (a `shiftR` 17) in b `xor` (b `shiftL` 5)

-- | Both machines for @frames@ frames; Left with the first difference.
-- @skips@: the fast version skips every other frame's picture (and those
-- pictures aren't compared). @resets@: the frames before which both
-- machines' reset button is pressed.
lockstep :: Bool -> [Int] -> Int -> BS.ByteString -> Either String Int
lockstep skips resets frames file = runST $ do
  ea <- F.newMachine file
  eb <- P.newMachine file
  case (ea, eb) of
    (Left e, _) -> pure (Left e)
    (_, Left e) -> pure (Left e)
    (Right a, Right b) -> do
      F.setSound a True
      P.setSound b True
      let frame k
            | k >= frames = pure (Right k)
            | otherwise = do
                PP.ppuSetSkip (P.mPpu b) (skips && odd k)
                when (k `elem` resets) (F.resetMachine a >> P.resetMachine b)
                F.setButtons a 1 (buttons k) >> P.setButtons b 1 (buttons k)
                F.setButtons a 2 (buttons (k + 1000)) >> P.setButtons b 2 (buttons (k + 1000))
                f0 <- FP.ppuFramesDone (F.mPpu a)
                r <- steps a b k f0
                case r of
                  Just e -> pure (Left e)
                  Nothing -> do
                    pa <- FP.ppuFrame (F.mPpu a)
                    pb <- PP.ppuFrame (P.mPpu b)
                    ra <- F.ramImage a
                    rb <- P.ramImage b
                    ca <- F.ciramImage a
                    cb <- P.ciramImage b
                    if | pa /= pb && not (skips && odd k) -> pure (Left ("frame " <> show k <> ": the pictures differ at pixel " <> show (firstDiff (U.toList pa) (U.toList pb))))
                       | ra /= rb -> pure (Left ("frame " <> show k <> ": RAM differs at " <> show (firstDiff (U.toList ra) (U.toList rb))))
                       | ca /= cb -> pure (Left ("frame " <> show k <> ": nametable RAM differs at " <> show (firstDiff (U.toList ca) (U.toList cb))))
                       | otherwise -> frame (k + 1)
      frame 0

steps :: F.Machine s -> P.Machine s -> Int -> Int -> ST s (Maybe String)
steps a b k f0 = go
 where
  go = do
    _ <- F.stepCpu a
    _ <- P.stepCpu b
    sa <- U.toList <$> F.takeSound a
    sb <- U.toList <$> P.takeSound b
    ca <- F.machineCycles a
    cb <- P.machineCycles b
    ga <- (\(FC.Regs ra rx ry rs rp pc) -> (ra, rx, ry, rs, rp, pc)) <$> FC.getRegs (F.mCpu a)
    gb <- (\(PC.Regs ra rx ry rs rp pc) -> (ra, rx, ry, rs, rp, pc)) <$> PC.getRegs (P.mCpu b)
    if | ca /= cb || ga /= gb -> pure (Just ("frame " <> show k <> ", cycle " <> show ca <> ": " <> show (ca, ga) <> " vs " <> show (cb, gb)))
       | sa /= sb -> pure (Just ("frame " <> show k <> ", cycle " <> show ca <> ": the APU's outputs differ: " <> show sa <> " vs " <> show sb))
       | otherwise -> do
           f <- FP.ppuFramesDone (F.mPpu a)
           fb <- PP.ppuFramesDone (P.mPpu b)
           if | f /= fb -> pure (Just ("frame " <> show k <> ": the PPUs' frame counts differ"))
              | f == f0 -> go
              | otherwise -> pure Nothing

-- | Both a frame at a time, as the desktop library runs them (the fast
-- version by instructions to the frame's end, the performance version by
-- 'P.runFrame'); each frame's sound, then its picture and RAM.
frameStep :: Int -> BS.ByteString -> Either String Int
frameStep frames file = runST $ do
  ea <- F.newMachine file
  eb <- P.newMachine file
  case (ea, eb) of
    (Left e, _) -> pure (Left e)
    (_, Left e) -> pure (Left e)
    (Right a, Right b) -> do
      F.setSound a True
      P.setSound b True
      let cap = 2 * 29781
          fastFrame = do
            f0 <- FP.ppuFramesDone (F.mPpu a)
            c0 <- F.machineCycles a
            let go = do
                  _ <- F.stepCpu a
                  f <- FP.ppuFramesDone (F.mPpu a)
                  c <- F.machineCycles a
                  if f == f0 && c - c0 < cap then go else pure ()
            go
          frame k
            | k >= frames = pure (Right k)
            | otherwise = do
                F.setButtons a 1 (buttons k) >> P.setButtons b 1 (buttons k)
                fastFrame
                P.runFrame b cap
                sa <- F.takeSound a
                sb <- P.takeSound b
                pa <- FP.ppuFrame (F.mPpu a)
                pb <- PP.ppuFrame (P.mPpu b)
                ra <- F.ramImage a
                rb <- P.ramImage b
                ca <- F.machineCycles a
                cb <- P.machineCycles b
                -- (the performance version's count is rebased by 2^24 now and then:
                -- the same modulo that)
                if | (ca - cb) `mod` (1 `shiftL` 24) /= 0 -> pure (Left ("frame " <> show k <> ": cycles " <> show ca <> " vs " <> show cb))
                   | sa /= sb -> pure (Left ("frame " <> show k <> ": the sound differs at cycle " <> show (firstDiff (U.toList sa) (U.toList sb))))
                   | pa /= pb -> pure (Left ("frame " <> show k <> ": the pictures differ at pixel " <> show (firstDiff (U.toList pa) (U.toList pb))))
                   | ra /= rb -> pure (Left ("frame " <> show k <> ": RAM differs at " <> show (firstDiff (U.toList ra) (U.toList rb))))
                   | otherwise -> frame (k + 1)
      frame 0

-- | The performance version with its clocks' rebase ('P.rebase', after
-- every 2^28 master ticks: about 12.5 s) and a twin without it, on 64
-- bits: each frame's sound, picture and RAM; and the rebase did happen.
rebaseTwin :: Int -> BS.ByteString -> Either String Int
rebaseTwin frames file = runST $ do
  ea <- P.newMachine file
  eb <- P.newMachine file
  case (ea, eb) of
    (Left e, _) -> pure (Left e)
    (_, Left e) -> pure (Left e)
    (Right a, Right b) -> do
      P.setSound a True
      P.setSound b True
      P.setRebase b False
      let cap = 2 * 29781
          frame k
            | k >= frames = do
                ca <- P.machineCycles a
                cb <- P.machineCycles b
                pure (if cb - ca >= 1 `shiftL` 24 then Right k else Left ("the clocks weren't rebased: " <> show (ca, cb)))
            | otherwise = do
                P.setButtons a 1 (buttons k) >> P.setButtons b 1 (buttons k)
                P.runFrame a cap
                P.runFrame b cap
                sa <- P.takeSound a
                sb <- P.takeSound b
                pa <- PP.ppuFrame (P.mPpu a)
                pb <- PP.ppuFrame (P.mPpu b)
                ra <- P.ramImage a
                rb <- P.ramImage b
                if | sa /= sb -> pure (Left ("frame " <> show k <> ": the sound differs at cycle " <> show (firstDiff (U.toList sa) (U.toList sb))))
                   | pa /= pb -> pure (Left ("frame " <> show k <> ": the pictures differ at pixel " <> show (firstDiff (U.toList pa) (U.toList pb))))
                   | ra /= rb -> pure (Left ("frame " <> show k <> ": RAM differs at " <> show (firstDiff (U.toList ra) (U.toList rb))))
                   | otherwise -> frame (k + 1)
      frame 0

firstDiff :: Eq a => [a] -> [a] -> Int
firstDiff xs ys = length (takeWhile id (zipWith (==) xs ys))
