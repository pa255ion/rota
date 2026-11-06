{-# LANGUAGE MultiWayIf #-}
-- | Side track Q (PLAN.md Q.7): the Game Gear's and Master System's fast
-- version ("Gg.SpecFast", the desktop emulator's) is the same machine as
-- the reference model ("Gg.Spec"). Both run side by side on the same
-- inputs: after every instruction the Z80's registers and the T-state
-- count; at every frame the picture, the 8 KiB of RAM, the 16 KiB of
-- VRAM and the frame's sound samples. The pads are a seeded sequence, so
-- menus and games are driven. Quick tier: our regression ROMs (fmtune
-- longer: the games leave the FM unit off), Waternet on both consoles
-- (the Master System with and without its FM unit) and
-- the VDP test briefly; full tier (EMU_FULL_TESTS=1): longer, ZEXDOC's
-- start, and the user's games in roms/gg/local and roms/sms/local if
-- there are any.
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

import qualified Gg.Spec as S
import qualified Gg.SpecFast as F
import Sms.Spec.FmUnit (newFmUnit)
import qualified Sms.SpecFast.FmUnit as FF
import Z80.Spec (getRegs)

-- | Which console: the Game Gear, the Master System II with or without
-- its FM unit.
data Kind = GG | SMS | SMSFM
  deriving stock (Show, Eq)

main :: IO ()
main = do
  full <- maybe False (not . null) <$> lookupEnv "EMU_FULL_TESTS"
  let n q f = if full then f else q
      under d ext = do
        ok <- doesDirectoryExist d
        if not ok then pure [] else map (d </>) . sort . filter (ext `isSuffixOf`) <$> listDirectory d
  ggAsm <- under "../../build/gg/asm" ".gg"
  smsAsm <- under "../../build/sms/asm" ".sms"
  ggLocal <- under "../../roms/gg/local" ".gg"
  smsLocal <- under "../../roms/sms/local" ".sms"
  let rom k f frames = testCase (show k <> " " <> f) (same k frames =<< BS.readFile f)
  defaultMain $ testGroup "sms-specfast: the fast version equals the reference model"
    [ rom GG "../../roms/gg/games/waternet.gg" (n 300 3000)
    , rom SMSFM "../../roms/sms/games/waternet.sms" (n 300 3000)
    , rom SMS "../../roms/sms/games/waternet.sms" (n 120 1200)
    , rom SMSFM "../../roms/sms/games/raposadosol_v20.sms" (n 300 3000)
    , rom SMS "../../roms/gg/vdptest/VDPTEST.sms" (n 120 1200)
    , rom SMS "../../roms/gg/smstestsuite/SMSTestSuite.sms" (n 120 1200)
    -- (the two games leave the FM unit off: our FM ROMs are what plays it)
    , rom SMSFM "../../build/sms/asm/fmtune.sms" (n 300 1500)
    , testGroup "our Game Gear ROMs" [ rom GG f (n 30 200) | f <- ggAsm ]
    , testGroup "our Master System ROMs" [ rom SMSFM f (n 30 200) | f <- smsAsm ]
    , testGroup "ZEXDOC's start" [ rom SMS "../../roms/sms/zexall/zexdoc.sms" 600 | full ]
    , testGroup "the user's games" ([ rom GG f 3000 | full, f <- ggLocal ] <> [ rom SMSFM f 3000 | full, f <- smsLocal ])
    ]
 where
  same k frames file = either assertFailure (const (pure ())) (lockstep k frames file)

-- | The pads at frame @k@: a new set every 8 frames from a xorshift seed,
-- Start (Pause) rarer.
inputs :: Int -> S.Input
inputs k =
  let r = rand (fromIntegral (k `div` 8) + 1)
      p1 = fromIntegral (r .&. 0x3F) :: Word8
      p2 = fromIntegral ((r `shiftR` 6) .&. 0x3F) :: Word8
      start = (r `shiftR` 12) .&. 15 == 0
  in S.Input p1 start p2
 where
  rand :: Word32 -> Word32
  rand s0 = let a = s0 `xor` (s0 `shiftL` 13); b = a `xor` (a `shiftR` 17) in b `xor` (b `shiftL` 5)

-- | Both machines for @frames@ frames; Left with the first difference.
lockstep :: Kind -> Int -> BS.ByteString -> Either String Int
lockstep kind frames rom = runST $ do
  let mapper = S.detectMapper rom
  (a, b) <- case kind of
    GG -> (,) <$> S.newMachineWith False mapper rom <*> F.newMachineWith False mapper rom
    SMS -> (,) <$> S.newMasterSystemFm Nothing Nothing mapper rom <*> F.newMasterSystemFm Nothing Nothing mapper rom
    SMSFM -> do
      ua <- newFmUnit
      ub <- FF.newFmUnit
      (,) <$> S.newMasterSystemFm Nothing (Just ua) mapper rom <*> F.newMasterSystemFm Nothing (Just ub) mapper rom
  let frame k
        | k >= frames = pure (Right k)
        | otherwise = do
            S.setInput a (inputs k) >> F.setInput b (inputs k)
            f0 <- S.machineFrames a
            r <- steps a b k f0
            case r of
              Just e -> pure (Left e)
              Nothing -> do
                pa <- S.frameBuffer a
                pb <- F.frameBuffer b
                ra <- S.ramImage a
                rb <- F.ramImage b
                va <- S.vramImage a
                vb <- F.vramImage b
                sa <- S.takeAudio a
                sb <- F.takeAudio b
                if | pa /= pb -> pure (Left ("frame " <> show k <> ": the pictures differ at pixel " <> show (firstDiff (U.toList pa) (U.toList pb))))
                   | ra /= rb -> pure (Left ("frame " <> show k <> ": RAM differs at " <> show (firstDiff (U.toList ra) (U.toList rb))))
                   | va /= vb -> pure (Left ("frame " <> show k <> ": VRAM differs at " <> show (firstDiff (U.toList va) (U.toList vb))))
                   | sa /= sb -> pure (Left ("frame " <> show k <> ": the sound differs at sample " <> show (firstDiff sa sb) <> " of " <> show (length sa, length sb)))
                   | otherwise -> frame (k + 1)
  frame 0

steps :: S.Machine s -> F.Machine s -> Int -> Int -> ST s (Maybe String)
steps a b k f0 = go
 where
  go = do
    _ <- S.stepMachine a
    _ <- F.stepMachine b
    ta <- S.machineTStates a
    tb <- F.machineTStates b
    ga <- getRegs (S.mCpu a)
    gb <- getRegs (F.mCpu b)
    if | ta /= tb || ga /= gb -> pure (Just ("frame " <> show k <> ", T-state " <> show ta <> ": " <> show (ta, ga) <> " vs " <> show (tb, gb)))
       | otherwise -> do
           f <- S.machineFrames a
           fb <- F.machineFrames b
           if | f /= fb -> pure (Just ("frame " <> show k <> ": the frame counts differ"))
              | f == f0 -> go
              | otherwise -> pure Nothing

firstDiff :: Eq x => [x] -> [x] -> Int
firstDiff xs ys = length (takeWhile id (zipWith (==) xs ys))
