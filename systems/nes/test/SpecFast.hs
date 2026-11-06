{-# LANGUAGE MultiWayIf #-}
-- | Side track Q (PLAN.md Q.7): the NES's fast version ("Nes.SpecFast",
-- the desktop emulator's) is the same machine as the reference model
-- ("Nes.Spec"). Both run side by side on the same inputs: after every
-- instruction the CPU cycle count, the CPU's registers and the APU's
-- outputs of every cycle (the fast version's 'F.takeSound', the
-- reference's APU trace); at every frame the picture, the 2 KiB of RAM
-- and the 2 KiB of nametable RAM. The buttons are a seeded sequence, so
-- menus and games are driven. Quick tier: a few test ROMs, Thwaite and a
-- mapper of each kind briefly; full tier (EMU_FULL_TESTS=1): longer, and
-- every Blargg test, Holy Mapperel ROM and AccuracyCoin, and the user's
-- games in roms/nes/games/local if there are any.
module Main (main) where

import Prelude

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

import qualified Mos6502.Spec as SC
import qualified Mos6502.SpecFast as FC
import Nes.Spec.Apu (Channels (..))
import qualified Nes.Spec.Machine as S
import Nes.Spec.Ppu (ppuFrame, ppuFramesDone)
import qualified Nes.SpecFast.Machine as F
import qualified Nes.SpecFast.Ppu as FP
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
  defaultMain $ testGroup "nes-specfast: the fast version equals the reference model"
    [ rom (romRoot </> "nestest" </> "nestest.nes") (n 60 300)
    , rom (romRoot </> "homebrew" </> "thwaite.nes") (n 400 3000)
    , rom (romRoot </> "homebrew" </> "dabg.nes") (n 200 2000)
    , testCase "Thwaite with every other frame's picture skipped: the same CPU, RAM and sound"
        (sameSkip (n 400 3000) =<< loadRomFile (romRoot </> "homebrew" </> "thwaite.nes"))
    , testCase "sprite 0 hit with every other frame skipped (Blargg's 01.basics)"
        (sameSkip 300 =<< loadRomFile (romRoot </> "blargg" </> "sprite_hit_tests_2005.10.05" </> "01.basics.nes"))
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
  same frames file = either assertFailure (const (pure ())) (lockstep False frames file)
  sameSkip frames file = either assertFailure (const (pure ())) (lockstep True frames file)

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
-- pictures aren't compared).
lockstep :: Bool -> Int -> BS.ByteString -> Either String Int
lockstep skips frames file = runST $ do
  ea <- S.newMachine file
  eb <- F.newMachine file
  case (ea, eb) of
    (Left e, _) -> pure (Left e)
    (_, Left e) -> pure (Left e)
    (Right a, Right b) -> do
      F.setSound b True
      let frame k
            | k >= frames = pure (Right k)
            | otherwise = do
                FP.ppuSetSkip (F.mPpu b) (skips && odd k)
                S.setButtons a 1 (buttons k) >> F.setButtons b 1 (buttons k)
                S.setButtons a 2 (buttons (k + 1000)) >> F.setButtons b 2 (buttons (k + 1000))
                f0 <- ppuFramesDone (S.mPpu a)
                r <- steps a b k f0
                case r of
                  Just e -> pure (Left e)
                  Nothing -> do
                    pa <- ppuFrame (S.mPpu a)
                    pb <- FP.ppuFrame (F.mPpu b)
                    ra <- S.ramImage a
                    rb <- F.ramImage b
                    ca <- S.ciramImage a
                    cb <- F.ciramImage b
                    if | pa /= pb && not (skips && odd k) -> pure (Left ("frame " <> show k <> ": the pictures differ at pixel " <> show (firstDiff (U.toList pa) (U.toList pb))))
                       | ra /= rb -> pure (Left ("frame " <> show k <> ": RAM differs at " <> show (firstDiff (U.toList ra) (U.toList rb))))
                       | ca /= cb -> pure (Left ("frame " <> show k <> ": nametable RAM differs at " <> show (firstDiff (U.toList ca) (U.toList cb))))
                       | otherwise -> frame (k + 1)
      frame 0

steps :: S.Machine s -> F.Machine s -> Int -> Int -> ST s (Maybe String)
steps a b k f0 = go
 where
  go = do
    _ <- S.withTrace a (S.stepCpu a)
    _ <- F.stepCpu b
    sa <- map pack <$> S.apuTrace a
    sb <- U.toList <$> F.takeSound b
    ca <- S.machineCycles a
    cb <- F.machineCycles b
    ga <- (\(SC.Regs ra rx ry rs rp pc) -> (ra, rx, ry, rs, rp, pc)) <$> SC.getRegs (S.mCpu a)
    gb <- (\(FC.Regs ra rx ry rs rp pc) -> (ra, rx, ry, rs, rp, pc)) <$> FC.getRegs (F.mCpu b)
    if | ca /= cb || ga /= gb -> pure (Just ("frame " <> show k <> ", cycle " <> show ca <> ": " <> show (ca, ga) <> " vs " <> show (cb, gb)))
       | sa /= sb -> pure (Just ("frame " <> show k <> ", cycle " <> show ca <> ": the APU's outputs differ: " <> show sa <> " vs " <> show sb))
       | otherwise -> do
           f <- ppuFramesDone (S.mPpu a)
           fb <- FP.ppuFramesDone (F.mPpu b)
           if | f /= fb -> pure (Just ("frame " <> show k <> ": the PPUs' frame counts differ"))
              | f == f0 -> go
              | otherwise -> pure Nothing
  pack (Channels p1 p2 t n dm) =
    (fromIntegral p1 + fromIntegral p2) .|. (fromIntegral t `shiftL` 5) .|. (fromIntegral n `shiftL` 9) .|. (fromIntegral dm `shiftL` 13)

firstDiff :: Eq a => [a] -> [a] -> Int
firstDiff xs ys = length (takeWhile id (zipWith (==) xs ys))
