{-# OPTIONS_GHC -fworker-wrapper -fspecialise-aggressively -funfolding-use-threshold=200 -funfolding-creation-threshold=1000 #-}
{-|
The Game Boy's fast version (side track Q for side track W, the user
2026-11-07: the browser's Game Boy Color too slow on a laptop): the calls
the desktop library ("Gb.Desktop") makes, as "Gb.Spec"'s, on the machine
of "Gb.SpecFast.*" (begun as copies of "Gb.Spec.*", which stay the
reference). gb-specfast runs it beside the reference on every input.
-}
module Gb.SpecFast
  ( Machine
  , Model (..)
  , newMachineModel
  , step
  , frames
  , cycles
  , setJoypad
  , audioSample
  ) where

import Prelude

import Control.Monad.ST (ST)
import Data.Word (Word8)

import Gb.SpecFast.Apu (channelOutputs, mix)
import Gb.SpecFast.Cpu (step)
import Gb.SpecFast.Machine (newMachineModel)
import Gb.SpecFast.State

-- | The output now, as 'Gb.Spec.audioSample': the channels through NR50
-- and NR51.
audioSample :: Machine s -> ST s (Int, Int)
audioSample m = do
  nr50 <- ramRead m 0xFF24
  nr51 <- ramRead m 0xFF25
  mix nr50 nr51 <$> channelOutputs m

-- | The buttons held (bit 0 Right, 1 Left, 2 Up, 3 Down, 4 A, 5 B,
-- 6 Select, 7 Start).
setJoypad :: Machine s -> Word8 -> ST s ()
setJoypad m = setSt m iJOY . fromIntegral

-- | Frames completed so far.
frames :: Machine s -> ST s Int
frames m = getSt m iFRAMES

-- | M-cycles since power-up.
cycles :: Machine s -> ST s Int
cycles m = getSt m iCYC
