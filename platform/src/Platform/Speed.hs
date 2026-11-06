{-|
The emulation speed (side track L, the in-game menu's Options page).
Every core with a frame buffer is ticked by a phase accumulator's carry
(the Game Boy's 'dotEnable', the NES's 'nesDotEnable', ...): an increment
added every pixel clock, 2^32 per tick. 'speedEnable' is the same
accumulator with the increment scaled by the menu's speed, in quarters
("Platform.Ps.Link", 'SpeedQuarters', 2 = 50 % to 8 = 200 %).

At 4 (100 %, the settings register's reset value) the increment is the
system's own, so the core runs bit for bit as before. Each scaled
increment is a constant, picked by a mux; one that would pass 2^32 − 1
is held there (a tick every clock: the Game Gear's increment is already
85 % of that). A core whose work for a tick takes longer than the tick
falls behind, and its own credit counter drops ticks: that, not this
module, is a core's speed limit (measured per system, PLAN.md L.4).
-}
module Platform.Speed
  ( speedIncrement
  , speedEnable
  ) where

import Clash.Prelude

import Platform.Ps.Link (SpeedQuarters)

-- | The increment at a speed (clamped to 2 to 8 quarters).
speedIncrement :: Unsigned 32 -> SpeedQuarters -> Unsigned 32
speedIncrement inc q = table !! (max 2 (min 8 q) - 2)
 where
  table :: Vec 7 (Unsigned 32)
  table = map scale (2 :> 3 :> 4 :> 5 :> 6 :> 7 :> 8 :> Nil)
  scale :: Integer -> Unsigned 32
  scale k = fromInteger (min (toInteger (maxBound :: Unsigned 32)) (toInteger inc * k `div` 4))

-- | The carry of a 32-bit phase accumulator adding the scaled increment
-- every clock (True for one clock per tick).
speedEnable
  :: HiddenClockResetEnable dom
  => Unsigned 32 -> Signal dom SpeedQuarters -> Signal dom Bool
speedEnable inc q = carry
 where
  acc = register (0 :: Unsigned 33) (next <$> acc <*> q)
  next a s = zeroExtend (truncateB a :: Unsigned 32) + zeroExtend (speedIncrement inc s)
  carry = (`testBit` 32) <$> acc
