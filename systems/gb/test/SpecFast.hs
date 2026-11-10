{-# LANGUAGE MultiWayIf #-}
-- | Side track Q for side track W (the user, 2026-11-07): the Game Boy's
-- fast version ("Gb.SpecFast", the browser's) is the same machine as the
-- reference model ("Gb.Spec"). Both run side by side on the same inputs:
-- after every instruction the M-cycle count, the registers and the sound's
-- output; at every frame the finished picture (grey and colour), the
-- 0x8000-0xFFFF memory and the Color's extra VRAM and WRAM banks. The
-- buttons are a seeded sequence. Quick tier: a few test ROMs and games
-- briefly, on both models; full tier (EMU_FULL_TESTS=1): longer, every
-- Blargg ROM, the Mooneye acceptance tests, our own ROMs and the user's
-- games in roms/gb/games/local and roms/gbc/games/local if there are any.
module Main (main) where

import Prelude

import Control.Monad.ST (ST, runST)
import Data.Bits (shiftL, shiftR, xor, (.&.))
import qualified Data.ByteString as BS
import Data.List (isSuffixOf, sort)
import qualified Data.Vector.Unboxed as U
import Data.Word (Word32, Word8)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import Test.Tasty
import Test.Tasty.HUnit

import qualified Gb.Spec as S
import qualified Gb.Spec.Machine as SM
import qualified Gb.Spec.State as SS
import qualified Gb.SpecFast as F
import qualified Gb.SpecFast.State as FS

main :: IO ()
main = do
  full <- maybe False (not . null) <$> lookupEnv "EMU_FULL_TESTS"
  let n q f = if full then f else q
      under d = do
        ok <- doesDirectoryExist d
        if not ok then pure [] else do
          es <- sort <$> listDirectory d
          concat <$> mapM (\e -> if any (`isSuffixOf` e) [".gb", ".gbc"] then pure [d </> e] else under (d </> e)) es
      roms = "../../roms"
      asm = "../../build/gb/asm"
  blargg <- under (roms </> "gb" </> "blargg")
  mooneye <- under (roms </> "gb" </> "mooneye" </> "acceptance")
  ours <- under asm
  local <- (<>) <$> under (roms </> "gb" </> "games" </> "local") <*> under (roms </> "gbc" </> "games" </> "local")
  let rom md f k = testCase (show md <> " " <> f) $ do
        ok <- doesFileExist f
        if not ok then assertFailure (f <> " missing: tools/fetch-roms.sh, make test (build/gb/asm)") else
          either assertFailure (const (pure ())) . lockstep md k =<< BS.readFile f
  defaultMain $ testGroup "gb-specfast: the fast version equals the reference model"
    [ rom SS.Dmg (roms </> "gb" </> "dmg-acid2" </> "dmg-acid2.gb") (n 60 120)
    , rom SS.Cgb (roms </> "gbc" </> "cgb-acid2" </> "cgb-acid2.gbc") (n 60 120)
    , rom SS.Cgb (roms </> "gbc" </> "games" </> "ucity.gbc") (n 300 3000)
    , rom SS.Dmg (roms </> "gbc" </> "games" </> "ucity.gbc") (n 120 1200)
    , rom SS.Dmg (roms </> "gb" </> "blargg" </> "cpu_instrs" </> "02-interrupts.gb") (n 120 600)
    , rom SS.Cgb (asm </> "hdma.gb") (n 60 300)
    , rom SS.Dmg (asm </> "buttons.gb") (n 60 300)
    , testGroup "Blargg" [ rom SS.Dmg f 3000 | full, f <- blargg ]
    , testGroup "Mooneye acceptance" [ rom SS.Dmg f 600 | full, f <- mooneye ]
    , testGroup "our ROMs" [ rom md f 600 | full, f <- ours, md <- [SS.Dmg, SS.Cgb] ]
    , testGroup "the user's games" [ rom SS.Cgb f 6000 | full, f <- local ]
    ]

-- | The buttons at frame @k@: a new set every 8 frames from a xorshift
-- seed, Start and Select rarer.
buttons :: Int -> Word8
buttons k =
  let r = rand (fromIntegral (k `div` 8) + 1)
      dirs = fromIntegral (r .&. 0x3F) :: Word8
      startSel = if (r `shiftR` 8) .&. 7 == 0 then fromIntegral ((r `shiftR` 11) .&. 0xC0) else 0
  in dirs `xor` startSel
 where
  rand :: Word32 -> Word32
  rand s0 = let a = s0 `xor` (s0 `shiftL` 13); b = a `xor` (a `shiftR` 17) in b `xor` (b `shiftL` 5)

-- | M-cycles a frame may take (the LCD off: the frame never ends).
frameCap :: Int
frameCap = 2 * 17556

-- | Both machines for @frames@ frames; Left with the first difference.
lockstep :: SS.Model -> Int -> BS.ByteString -> Either String Int
lockstep md frames file = runST $ do
  ea <- SM.newMachineModel md file
  eb <- F.newMachineModel (if md == SS.Cgb then FS.Cgb else FS.Dmg) file
  case (ea, eb) of
    (Left e, _) -> pure (Left e)
    (_, Left e) -> pure (Left e)
    (Right a, Right b) -> do
      let frame k
            | k >= frames = pure (Right k)
            | otherwise = do
                S.setJoypad a (buttons k)
                F.setJoypad b (buttons k)
                f0 <- S.frames a
                c0 <- S.cycles a
                r <- steps a b k f0 c0
                case r of
                  Just e -> pure (Left e)
                  Nothing -> do
                    d <- frameDiff a b
                    maybe (frame (k + 1)) (\e -> pure (Left ("frame " <> show k <> ": " <> e))) d
      frame 0

steps :: SS.Machine s -> FS.Machine s -> Int -> Int -> Int -> ST s (Maybe String)
steps a b k f0 c0 = go
 where
  go = do
    S.step a
    F.step b
    ca <- S.cycles a
    cb <- F.cycles b
    ra <- U.freeze (SS.mRegs a)
    rb <- U.freeze (FS.mRegs b)
    pa <- SS.getSt a SS.iPC
    pb <- FS.getSt b FS.iPC
    sa <- S.audioSample a
    sb <- F.audioSample b
    if | ca /= cb || pa /= pb || ra /= rb -> pure (Just ("frame " <> show k <> ", M-cycle " <> show ca <> ": " <> show (ca, pa, U.toList ra) <> " vs " <> show (cb, pb, U.toList rb)))
       | sa /= sb -> pure (Just ("frame " <> show k <> ", M-cycle " <> show ca <> ": the sound " <> show sa <> " vs " <> show sb))
       | otherwise -> do
           fa <- S.frames a
           fb <- F.frames b
           if | fa /= fb -> pure (Just ("frame " <> show k <> ", M-cycle " <> show ca <> ": the frame counts differ"))
              | fa == f0 && ca - c0 < frameCap -> go
              | otherwise -> pure Nothing

-- | The finished pictures and the memory, at a frame's end.
frameDiff :: SS.Machine s -> FS.Machine s -> ST s (Maybe String)
frameDiff a b = do
  la <- U.freeze (SS.mLast a)
  lb <- U.freeze (FS.mLast b)
  lca <- U.freeze (SS.mLastC a)
  lcb <- U.freeze (FS.mLastC b)
  ma <- U.freeze (SS.mRam a)
  mb <- U.freeze (FS.mRam b)
  va <- U.freeze (SS.mVram1 a)
  vb <- U.freeze (FS.mVram1 b)
  wa <- U.freeze (SS.mWramX a)
  wb <- U.freeze (FS.mWramX b)
  pure $ if
    | la /= lb -> Just ("the pictures differ at pixel " <> show (firstDiff la lb))
    | lca /= lcb -> Just ("the colour pictures differ at pixel " <> show (firstDiff lca lcb))
    | ma /= mb -> Just ("memory differs at " <> show (0x8000 + firstDiff ma mb))
    | va /= vb -> Just "VRAM bank 1 differs"
    | wa /= wb -> Just "WRAM banks 2-7 differ"
    | otherwise -> Nothing

firstDiff :: (U.Unbox a, Eq a) => U.Vector a -> U.Vector a -> Int
firstDiff xs ys = U.length (U.takeWhile id (U.zipWith (==) xs ys))
