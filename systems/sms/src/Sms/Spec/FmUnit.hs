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
module Sms.Spec.FmUnit
  ( newFmUnit
  , fmGain
  ) where

import Prelude

import Data.Bits (testBit, (.&.))
import Data.STRef
import Control.Monad (forM_)
import Control.Monad.ST (ST)
import Data.Word (Word8)

import Gg.Spec (FmUnit (..))
import Sms.Spec.Fm

fmGain :: Int
fmGain = 24

newFmUnit :: ST s (FmUnit s)
newFmUnit = do
  fm <- newFm
  ctl <- newSTRef (0 :: Word8)
  level <- newSTRef 0
  let catchUp t = do
        fmRunTo fm (t `div` 4)
        ps <- fmTakePins fm
        forM_ ps $ \p -> writeSTRef level (fmMix p)
  pure FmUnit
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
