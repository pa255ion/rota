{-|
Button debouncer. The output only changes after the (synchronised) input has
differed from it for @n@ consecutive cycles, so any glitch or bounce shorter
than @n@ cycles is ignored. Typical use: @debounce (SNat \@(MsToCycles DomPix 10))@.
-}
module Platform.Input.Debounce
  ( debounce
  , debounceStep
  , syncDebounce
  ) where

import Clash.Prelude

import Platform.Input.Sync (synchronise)

-- | Pure transition function. State: the current output, and how many
-- consecutive samples so far have disagreed with it. The output is the
-- current (registered) value.
debounceStep :: KnownNat n => SNat n -> (Bool, Index n) -> Bool -> ((Bool, Index n), Bool)
debounceStep _ (out, cnt) inp
  | inp == out      = ((out, 0), out)
  | cnt == maxBound = ((inp, 0), out)
  | otherwise       = ((out, cnt + 1), out)

-- | Debounce a synchronised input. A change that holds for @n@ cycles
-- appears on the output exactly @n@ cycles after it first appeared on the
-- input; shorter pulses never appear. @initial@ is the output after reset.
debounce
  :: forall n dom
   . (HiddenClockResetEnable dom, KnownNat n, 1 <= n)
  => SNat n -> Bool -> Signal dom Bool -> Signal dom Bool
debounce n initial = mealy (debounceStep n) (initial, 0)

-- | 'synchronise' followed by 'debounce', for raw button pins.
syncDebounce
  :: forall n dom
   . (HiddenClockResetEnable dom, KnownNat n, 1 <= n)
  => SNat n -> Signal dom Bool -> Signal dom Bool
syncDebounce n = debounce n False . synchronise False
