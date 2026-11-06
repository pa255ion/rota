{-|
Synchroniser for asynchronous board inputs (switches, buttons). Every pin
that is not already in the sampling clock domain goes through 'synchronise'
before any logic sees it.
-}
module Platform.Input.Sync
  ( synchronise
  ) where

import Clash.Prelude

-- | Two-flip-flop synchroniser: two cycles of latency, @dflt@ during reset.
-- The flops are named @sync_ff1@ / @sync_ff2@ in the generated HDL so that
-- boards/zedboard/build.tcl can find them and mark them @ASYNC_REG@.
synchronise :: (HiddenClockResetEnable dom, NFDataX a) => a -> Signal dom a -> Signal dom a
synchronise dflt x = ff2
 where
  ff1 = setName @"sync_ff1" (register dflt x)
  ff2 = setName @"sync_ff2" (register dflt ff1)
