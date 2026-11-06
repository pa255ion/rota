{-|
VGA timing generator. One 'moore' machine with a pair of counters; all
outputs are decoded combinationally from the registered counters by the pure
function 'vgaDecode', so the logic is unit-testable without simulation.

Outputs are valid in the same cycle as the counters. A pixel generator that
needs @k@ cycles of latency (for example a BRAM read) must delay the syncs by
@k@ cycles too; the board top then registers everything before the pins.
-}
module Platform.Video.VgaTiming
  ( Axis (..)
  , SyncPolarity (..)
  , VgaMode (..)
  , VgaCounters (..)
  , VgaOut (..)
  , vga640x480
  , axisTotal
  , vgaNext
  , vgaDecode
  , vgaTiming
  , Rgb444 (..)
  , Rgb888 (..)
  , black8
  , fromRgb444
  , toRgb444
  , black
  , VgaPins (..)
  , vgaPinsIdle
  , vgaPinLevels
  , vgaPins
  ) where

import Clash.Prelude

-- | One axis of a video mode, in pixels (horizontal) or lines (vertical).
-- Order along the axis: visible, front porch, sync pulse, back porch.
-- Counters are 10 bits wide, so 'axisTotal' must not exceed 1024.
data Axis = Axis
  { axVisible    :: Unsigned 10
  , axFrontPorch :: Unsigned 10
  , axSync       :: Unsigned 10
  , axBackPorch  :: Unsigned 10
  , axPolarity   :: SyncPolarity
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFDataX

-- | Level of the sync pin while the sync pulse is active.
data SyncPolarity = SyncNegative | SyncPositive
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFDataX

data VgaMode = VgaMode
  { vmH :: Axis
  , vmV :: Axis
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFDataX

-- | 640×480\@60 (pixel clock 25.175 MHz): 800×525 clocks per frame,
-- negative sync on both axes.
vga640x480 :: VgaMode
vga640x480 = VgaMode
  { vmH = Axis 640 16 96 48 SyncNegative
  , vmV = Axis 480 10  2 33 SyncNegative
  }

axisTotal :: Axis -> Unsigned 10
axisTotal (Axis vis fp sy bp _) = vis + fp + sy + bp

data VgaCounters = VgaCounters
  { hCount :: Unsigned 10
  , vCount :: Unsigned 10
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFDataX

data VgaOut = VgaOut
  { vgaHSync       :: Bit
    -- ^ HSYNC pin level
  , vgaVSync       :: Bit
    -- ^ VSYNC pin level
  , vgaActive      :: Bool
    -- ^ inside the visible area; RGB must be black otherwise
  , vgaX           :: Unsigned 10
    -- ^ horizontal counter (equals the pixel column while 'vgaActive')
  , vgaY           :: Unsigned 10
    -- ^ vertical counter (equals the pixel row while 'vgaActive')
  , vgaFrameStart  :: Bool
    -- ^ first pixel of a frame, @(0, 0)@
  , vgaVBlankStart :: Bool
    -- ^ first clock after the last visible line: a once-per-frame tick
    -- (60 Hz for 640×480) for timers and vblank waits
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFDataX

-- | Advance the counters by one pixel clock.
vgaNext :: VgaMode -> VgaCounters -> VgaCounters
vgaNext (VgaMode h v) (VgaCounters x y)
  | x /= axisTotal h - 1 = VgaCounters (x + 1) y
  | y /= axisTotal v - 1 = VgaCounters 0 (y + 1)
  | otherwise            = VgaCounters 0 0

-- | Decode the counters into sync, blanking and position signals.
vgaDecode :: VgaMode -> VgaCounters -> VgaOut
vgaDecode (VgaMode h v) (VgaCounters x y) = VgaOut
  { vgaHSync       = syncLevel h x
  , vgaVSync       = syncLevel v y
  , vgaActive      = x < axVisible h && y < axVisible v
  , vgaX           = x
  , vgaY           = y
  , vgaFrameStart  = x == 0 && y == 0
  , vgaVBlankStart = x == 0 && y == axVisible v
  }

syncLevel :: Axis -> Unsigned 10 -> Bit
syncLevel ax c = case axPolarity ax of
  SyncNegative -> boolToBit (not inPulse)
  SyncPositive -> boolToBit inPulse
 where
  start   = axVisible ax + axFrontPorch ax
  inPulse = c >= start && c < start + axSync ax

-- | Free-running timing generator, starting at @(0, 0)@ after reset.
vgaTiming :: HiddenClockResetEnable dom => VgaMode -> Signal dom VgaOut
vgaTiming mode = moore (\s () -> vgaNext mode s) (vgaDecode mode) (VgaCounters 0 0) (pure ())

-- | 12-bit colour, 4 bits per channel, as the ZedBoard's VGA DAC takes it.
data Rgb444 = Rgb444
  { rgbR :: BitVector 4
  , rgbG :: BitVector 4
  , rgbB :: BitVector 4
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFDataX

black :: Rgb444
black = Rgb444 0 0 0

-- | 24-bit colour, the platform's pixel stream (M3 step 3.7): the NES
-- palette needs more than 4 bits a channel. The VGA DAC takes the top 4
-- bits ('vgaPinLevels'); HDMI takes all 8 ('Platform.Video.Ycbcr422').
-- A 12-bit colour widens by repeating each nibble ('fromRgb444': c × 17),
-- so systems with 12-bit colours look exactly as before on both outputs.
data Rgb888 = Rgb888
  { rgb8R :: BitVector 8
  , rgb8G :: BitVector 8
  , rgb8B :: BitVector 8
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFDataX

black8 :: Rgb888
black8 = Rgb888 0 0 0

fromRgb444 :: Rgb444 -> Rgb888
fromRgb444 (Rgb444 r g b) = Rgb888 (w r) (w g) (w b)
 where w x = x ++# x

-- | The top 4 bits of each channel (what the VGA DAC shows).
toRgb444 :: Rgb888 -> Rgb444
toRgb444 (Rgb888 r g b) = Rgb444 (slice d7 d4 r) (slice d7 d4 g) (slice d7 d4 b)

-- | Everything that goes to the VGA connector in one clock.
data VgaPins = VgaPins
  { vpHSync :: Bit
  , vpVSync :: Bit
  , vpRgb   :: Rgb444
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFDataX

-- | Pin levels for one clock: the syncs, and the colour (its top 4 bits)
-- forced to black outside the visible area.
vgaPinLevels :: VgaOut -> Rgb888 -> VgaPins
vgaPinLevels o c = VgaPins (vgaHSync o) (vgaVSync o) (if vgaActive o then toRgb444 c else black)

-- | Pins while nothing is shown: syncs inactive, black.
vgaPinsIdle :: VgaMode -> VgaPins
vgaPinsIdle (VgaMode h v) = VgaPins (idle h) (idle v) black
 where
  idle ax = case axPolarity ax of
    SyncNegative -> 1
    SyncPositive -> 0

-- | The output register in front of the VGA pins: one cycle of latency for
-- syncs and colour alike, idle during reset. The colour must be valid in
-- the same cycle as the 'VgaOut' it belongs to.
vgaPins
  :: HiddenClockResetEnable dom
  => VgaMode -> Signal dom VgaOut -> Signal dom Rgb888 -> Signal dom VgaPins
vgaPins mode o c = register (vgaPinsIdle mode) (vgaPinLevels <$> o <*> c)
