-- | Side track Q (PLAN.md Q.7): where the Master System's and Game Gear's
-- reference model's time goes, to choose what a fast version speeds up
-- (+RTS -s for the allocation):
--
-- > sms-fast-bench sms|sms-nofm|gg|fast|fast-nofm|fast-gg ROM FRAMES
--
-- each a frame at a time with its sound taken, as the desktop runs it:
-- the Master System II with the FM unit (sms), without (sms-nofm), the
-- Game Gear (gg); fast*: the same on the fast version ("Gg.SpecFast").
module Main (main) where

import Prelude

import Control.Monad (replicateM_, when)
import Control.Monad.ST (stToIO)
import qualified Data.ByteString as B
import System.Environment (getArgs)

import Gg.Spec
import qualified Gg.SpecFast as F
import Sms.Spec.FmUnit (newFmUnit)
import qualified Sms.SpecFast.FmUnit as FF

main :: IO ()
main = do
  [which, romF, n] <- getArgs
  rom <- B.readFile romF
  let mapper = detectMapper rom
      frames = read n :: Int
  t <- if take 4 which == "fast" then do
      m <- stToIO $ case which of
        "fast" -> FF.newFmUnit >>= \u -> F.newMasterSystemFm Nothing (Just u) mapper rom
        "fast-nofm" -> F.newMasterSystemFm Nothing Nothing mapper rom
        _ -> F.newMachineWith False mapper rom
      replicateM_ frames $ stToIO $ do
        F.runFrame m
        a <- F.takeAudio m
        when (length a < 0) (pure ())
      stToIO (F.machineTStates m)
    else do
      m <- stToIO $ case which of
        "sms" -> newFmUnit >>= \u -> newMasterSystemFm Nothing (Just u) mapper rom
        "sms-nofm" -> newMasterSystemFm Nothing Nothing mapper rom
        _ -> newMachineWith False mapper rom
      replicateM_ frames $ stToIO $ do
        runFrame m
        a <- takeAudio m
        when (length a < 0) (pure ())
      stToIO (machineTStates m)
  putStrLn (which <> ": " <> show t <> " T-states")
