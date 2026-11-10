{-# LANGUAGE MultiWayIf #-}
-- | Side track W: the fast version's clocks rebased ("Gg.SpecFast"'s
-- 'F.rebase' past 2^28 T-states; the FM unit's own, 'fmRebase', past
-- 2^20 * 105 samples) are the same machine. In the browser, Int is 32 bits
-- and the clocks counted from power-on overflowed (the Game Gear froze
-- after 3.3 minutes); here, on 64 bits, a twin runs without the rebase,
-- and both get the same buttons: at every frame the same picture and the
-- same sound. Quick tier: the Game Gear and the Master System with its FM
-- unit for 5 minutes (the machine's rebase, four times); full tier
-- (EMU_FULL_TESTS=1): our FM tune for 45 minutes, past the FM's own (at
-- about 37).
module Main (main) where

import Prelude

import Control.Monad.ST (runST)
import Data.Bits (shiftL, shiftR, testBit, xor, (.&.), (.|.))
import qualified Data.ByteString as BS
import Data.Word (Word32)
import System.Environment (lookupEnv)
import Test.Tasty
import Test.Tasty.HUnit

import qualified Gg.SpecFast as F
import qualified Sms.SpecFast.FmUnit as FF

data Kind = GG | SMSFM
  deriving stock (Show, Eq)

main :: IO ()
main = do
  full <- maybe False (not . null) <$> lookupEnv "EMU_FULL_TESTS"
  let minutes k = k * 60 * 60 -- frames (about 59.92 a second)
      rom k f frames = testCase (show k <> " " <> f <> ", " <> show (frames `div` 3600) <> " minutes") (same k frames =<< BS.readFile f)
  defaultMain $ testGroup "sms-rebase: the rebased clocks equal a twin's without"
    ( [ rom GG "../../roms/gg/games/waternet.gg" (minutes 5)
      , rom SMSFM "../../roms/sms/games/raposadosol_v20.sms" (minutes 5)
      ]
      <> [ rom SMSFM "../../build/sms/asm/fmtune.sms" (minutes 45) | full ] )
 where
  same k frames file = either assertFailure (const (pure ())) (twins k frames file)

-- | The buttons at frame @k@: a new set every 16 frames from a xorshift
-- seed (Start rarer), so menus and games move.
pads :: Int -> F.Input
pads k =
  let r = rand (fromIntegral (k `div` 16) + 1)
      w = fromIntegral (r .&. 0x3F) :: Int
      pad = (if testBit w 0 then F.padRight else 0) .|. (if testBit w 1 then F.padLeft else 0)
        .|. (if testBit w 2 then F.padUp else 0) .|. (if testBit w 3 then F.padDown else 0)
        .|. (if testBit w 4 then F.pad1 else 0) .|. (if testBit w 5 then F.pad2 else 0)
  in F.Input pad ((r `shiftR` 8) .&. 31 == 0) 0
 where
  rand :: Word32 -> Word32
  rand s0 = let a = s0 `xor` (s0 `shiftL` 13); b = a `xor` (a `shiftR` 17) in b `xor` (b `shiftL` 5)

-- | The machine and its twin (no rebase) for @frames@ frames; Left with
-- the first difference.
twins :: Kind -> Int -> BS.ByteString -> Either String ()
twins k frames rom = runST $ do
  (a, b) <- case k of
    GG -> (,) <$> F.newMachine rom <*> F.newMachine rom
    SMSFM -> do
      (ua, _) <- FF.newFmUnitRebase True
      (ub, _) <- FF.newFmUnitRebase False
      let mapper = F.detectMapper rom
      (,) <$> F.newMasterSystemFm Nothing (Just ua) mapper rom <*> F.newMasterSystemFm Nothing (Just ub) mapper rom
  F.setRebase b False
  let go n
        | n >= frames = pure (Right ())
        | otherwise = do
            F.setInput a (pads n) >> F.setInput b (pads n)
            F.runFrame a >> F.runFrame b
            pa <- F.frameBuffer a
            pb <- F.frameBuffer b
            sa <- F.takeAudio a
            sb <- F.takeAudio b
            ta <- F.machineTStates a
            tb <- F.machineTStates b
            if | pa /= pb -> pure (Left ("frame " <> show n <> ": the pictures differ"))
               | sa /= sb -> pure (Left ("frame " <> show n <> ": the sound differs"))
               -- (and the rebase did happen: the twin's clock is past it)
               | n == frames - 1 && (ta >= 1 `shiftL` 28 || tb < 1 `shiftL` 28) -> pure (Left ("the clock wasn't rebased: " <> show (ta, tb)))
               | otherwise -> go (n + 1)
  go 0
