{-|
The M0c OLED test pattern (PLAN.md step 7), 128×32, in framebuffer
coordinates (x = column, y = row, (0, 0) top left as the driver numbers
them; see "Platform.Video.Oled"):

* a one-pixel border around the whole panel;
* a solid 8×8 block at x 2 to 9, y 2 to 9: the orientation marker. On the
  board it should be in the top-left corner; anywhere else means the
  segment remap or COM scan direction is the wrong way round;
* a checkerboard of 4×4 cells at x 16 to 47, y 4 to 27;
* a 4-pixel-wide vertical bar (y 2 to 29) that sweeps from x = 56 to
  x = 119 and starts again, one pixel per step.
-}
module M0c.Pattern
  ( BarPos
  , pixel
  , patternByte
  , m0cFramebuffer
  ) where

import Clash.Prelude

-- | Bar offset from x = 56, 0 to 63.
type BarPos = Unsigned 6

pixel :: BarPos -> Unsigned 7 -> Unsigned 5 -> Bool
pixel bar x y = border || marker || checker || barOn
 where
  border  = x == 0 || x == 127 || y == 0 || y == 31
  marker  = x >= 2 && x <= 9 && y >= 2 && y <= 9
  checker = x >= 16 && x <= 47 && y >= 4 && y <= 27
            && odd (resize (x `shiftR` 2) `xor` (resize (y `shiftR` 2) :: Unsigned 7))
  bx      = 56 + resize bar :: Unsigned 7
  barOn   = x >= bx && x < bx + 4 && y >= 2 && y <= 29

-- | Framebuffer byte at address @128 * page + column@: bit @b@ is the pixel
-- at row @8 * page + b@.
patternByte :: BarPos -> Index 512 -> BitVector 8
patternByte bar a = pack (reverse (map px indicesI))
 where
  (page, col) = (unpack (slice d8 d7 (pack a)) :: Unsigned 2, unpack (slice d6 d0 (pack a)) :: Unsigned 7)
  px :: Index 8 -> Bit
  px b = boolToBit (pixel bar col (resize page * 8 + fromIntegral b))

-- | The pattern as a framebuffer read port with one cycle of latency (like
-- a block RAM). The bar moves one step every @n@ clocks.
m0cFramebuffer
  :: forall n dom
   . (HiddenClockResetEnable dom, KnownNat n, 1 <= n)
  => SNat n -> Signal dom (Index 512) -> Signal dom (BitVector 8)
m0cFramebuffer _ addr = register 0 (patternByte <$> bar <*> addr)
 where
  tick = moore (\c () -> if c == maxBound then 0 else c + 1) (== maxBound) (0 :: Index n) (pure ())
  bar  = regEn 0 tick (bar + 1)
