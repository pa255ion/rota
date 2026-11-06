{-|
The M0b VGA test pattern (PLAN.md step 5b), 640×480:

* rows 0 to 319: eight colour bars, 80 px each: white, yellow, cyan,
  green, magenta, red, blue, black;
* rows 320 to 399: a 16-step grey ramp, 40 px per step, 0 on the left;
* rows 400 to 479: the same ramp for red (400 to 426), green (427 to 453)
  and blue (454 to 479) alone. A wrong DAC bit order (docs/ZEDBOARD.md,
  boards/zedboard/vga.xdc) shows up as a ramp that isn't monotonic;
* a one-pixel white border on the edges of the visible area, so a monitor
  that crops or shifts the picture is easy to spot;
* on top, a 32×32 orange square that bounces off the edges, moving 2 px
  right/left and 1 px down/up per frame.

Everything is a pure function of the square's state and the pixel
position; 'm0bVideo' only adds the timing generator and registers.
-}
module M0b.Pattern
  ( Square (..)
  , squareSize
  , squareMaxX
  , squareMaxY
  , squareInit
  , squareNext
  , squareAt
  , background
  , testPattern
  , m0bStream
  , m0bVideo
  ) where

import Clash.Prelude

import Platform.Video.VgaTiming

data Square = Square
  { sqX     :: Unsigned 10  -- ^ left column
  , sqY     :: Unsigned 10  -- ^ top row
  , sqRight :: Bool         -- ^ moving right
  , sqDown  :: Bool         -- ^ moving down
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFDataX

squareSize, squareMaxX, squareMaxY :: Unsigned 10
squareSize = 32
squareMaxX = 640 - squareSize
squareMaxY = 480 - squareSize

-- | The square's state during the first frame after reset.
squareInit :: Square
squareInit = Square 100 60 True True

-- | Move by one frame, bouncing off the edges.
squareNext :: Square -> Square
squareNext (Square x y r d) = Square x' y' r' d'
 where
  (x', r') = axisStep 2 squareMaxX x r
  (y', d') = axisStep 1 squareMaxY y d

axisStep :: Unsigned 10 -> Unsigned 10 -> Unsigned 10 -> Bool -> (Unsigned 10, Bool)
axisStep s maxP p fwd
  | fwd       = if p >= maxP - s then (maxP, False) else (p + s, True)
  | otherwise = if p <= s then (0, True) else (p - s, False)

-- | The square during frame @n@ after reset.
squareAt :: Int -> Square
squareAt n = iterate' n squareNext squareInit
 where
  iterate' k f a = if k <= 0 then a else iterate' (k - 1) f (f a)

-- | @x \`div\` 40@ for @x < 640@, without a divider: 205 / 2^13 is just above
-- 1/40, and the error stays below 1/64 for @x < 640@, so the floor is
-- exact (checked exhaustively in the tests).
div40 :: Unsigned 10 -> BitVector 4
div40 x = slice d16 d13 (resize x * 205 :: Unsigned 17)

full :: BitVector 4
full = maxBound

-- | The pattern without the square.
background :: Unsigned 10 -> Unsigned 10 -> Rgb444
background x y
  | x == 0 || x == 639 || y == 0 || y == 479 = Rgb444 full full full
  | y < 320   = bar
  | y < 400   = Rgb444 ramp ramp ramp
  | y < 427   = Rgb444 ramp 0 0
  | y < 454   = Rgb444 0 ramp 0
  | otherwise = Rgb444 0 0 ramp
 where
  ramp = div40 x
  -- 80 px per bar, so bar index = ramp / 2. In SMPTE order, red is off
  -- for odd pairs, green for the right half, blue for odd bars.
  i    = slice d3 d1 ramp :: BitVector 3
  on b = if b == 0 then full else 0
  bar  = Rgb444 (on (i ! (1 :: Int))) (on (i ! (2 :: Int))) (on (i ! (0 :: Int)))

-- | The full pattern at a visible pixel.
testPattern :: Square -> Unsigned 10 -> Unsigned 10 -> Rgb444
testPattern sq x y
  | inside (sqX sq) x && inside (sqY sq) y = Rgb444 full 8 0
  | otherwise                              = background x y
 where
  inside p c = c >= p && c < p + squareSize

-- | Timing generator, square and pattern: the timing and the colour of
-- the same clock, before the pin register (M0d feeds this to HDMI too).
-- The square advances at the start of every vertical blank.
m0bStream :: HiddenClockResetEnable dom => (Signal dom VgaOut, Signal dom Rgb888)
m0bStream = (vga, fromRgb444 <$> rgb)
 where
  vga = vgaTiming vga640x480
  sq  = regEn squareInit (vgaVBlankStart <$> vga) (squareNext <$> sq)
  rgb = testPattern <$> sq <*> (vgaX <$> vga) <*> (vgaY <$> vga)

-- | 'm0bStream', registered for the VGA pins.
m0bVideo :: HiddenClockResetEnable dom => Signal dom VgaPins
m0bVideo = uncurry (vgaPins vga640x480) m0bStream
