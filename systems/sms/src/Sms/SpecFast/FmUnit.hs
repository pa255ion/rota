{-# OPTIONS_GHC -fworker-wrapper -fspecialise-aggressively -funfolding-use-threshold=200 -funfolding-creation-threshold=1000 #-}
-- (the fast version is never synthesised: GHC's unboxing back on, which
-- clash-common's -fno-worker-wrapper turns off for Clash's sake)
-- Side track Q (PLAN.md Q.7): the desktop emulator's fast version of
-- Sms.Spec.FmUnit, kept beside it (the original stays the reference the
-- core's lockstep and the oracle tests check; a fix in either goes to
-- both). The sms-specfast suite runs both machines side by side.
{-|
The Master System's FM unit in the reference model (M6 step 6.3): the
YM2413 ("Sms.Spec.Fm") behind ports F0h-F2h, as "Gg.Spec"'s 'FmUnit'
('newMasterSystemFm').

__Ports__ (SMS Power, "Audio Control Port"; docs/systems/ym2413.md 9.3):
F0h the YM2413's address, F1h its data; F2h the audio control, as on the
Japanese SMS: bit 0 lets the FM unit through, and the PSG plays while bit 1
equals bit 0 (00h, the power-on value: PSG only; 01h FM only; 02h neither;
03h both). Read back (only with the I/O chip off, port 3Eh bit 2: games'
detection, "FM Chip Detection"), bits 1-0 are the last value written and
bits 7-2 0 (the hardware's bits 7-5, a C-Sync counter, *to verify*).

__Time.__ The chip runs from the Z80's clock: a write at T-state t is the
YM2413's strobe at cycle t div 4 (its cycle 0 at T-state 0: the real phase
between the two after a power-on is unknown and inaudible).

__Sound.__ A sample's sum of the chip's two pins ("Sms.Spec.Fm.fmMix"),
times 'fmGain', mono, held until the next sample; the machine adds it to
the PSG's on both sides. 'fmGain' sets the balance: a full-scale FM channel
(DAC ±256) comes out at ±6144 against a PSG channel's ±8191 (MAME's
testers put the PSG at about 0.25 to 0.35 of the FM on a Japanese SMS; that
balance is unmeasured, *to tune* by ear on the board).
-}
module Sms.SpecFast.FmUnit
  ( newFmUnit
  , newFmUnitFields
  ) where

import Prelude

import Data.Bits (testBit, (.&.))
import Data.STRef
import Control.Monad.ST (ST)
import Data.Word (Word8)

import Gg.Spec (FmUnit (..))
import Sms.Spec.FmUnit (fmGain)
import Sms.SpecFast.Fm
import Platform.Snapshot (Field (..))

newFmUnit :: ST s (FmUnit s)
newFmUnit = fst <$> newFmUnitFields

-- | The same, with its state for a save state (Q.8, "Platform.Snapshot":
-- the machine sees the unit only through its calls).
newFmUnitFields :: ST s (FmUnit s, [Field s])
newFmUnitFields = do
  fm <- newFm
  ctl <- newSTRef (0 :: Word8)
  level <- newSTRef 0
  -- (side track Q: the level is the last completed sample's mix, kept by
  -- the fast FM itself, as the reference's last pins' 'fmMix'; no list)
  let catchUp t = do
        fmRunTo fm (t `div` 4)
        fmLastMix fm >>= writeSTRef level
  pure . (\u -> (u, fmFields fm <> [FRef ctl, FRef level])) $ FmUnit
    { fuWrite = \t port v -> do
        catchUp t
        if port == 0xF2
          then writeSTRef ctl (v .&. 3)
          else fmWrite fm (t `div` 4) (fromIntegral port .&. 1) v
    , fuRead = \_ -> (.&. 3) <$> readSTRef ctl
    , fuLevel = \t -> do
        catchUp t
        c <- readSTRef ctl
        if testBit c 0 then (* fmGain) <$> readSTRef level else pure 0
    , fuPsgOn = (\c -> testBit c 1 == testBit c 0) <$> readSTRef ctl
    }
