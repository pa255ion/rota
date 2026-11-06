{-|
The CHIP-8 display path (PLAN.md step 1.4): the 64×32 framebuffer on VGA
and on the onboard OLED. Both read the core's framebuffer through its
video ports ("Chip8.Core": one 64-bit row per read, one cycle of latency,
bit 63 = column 0).

__VGA.__ Each CHIP-8 pixel is 10×10 VGA pixels; the 640×320 picture sits in
rows 80 to 399 of the 640×480 frame, with black above and below. Row and
column are @(y - 80) \/ 10@ and @x \/ 10@, computed by a multiply and shift
('div10', exact for the whole frame). The framebuffer row is read in the
cycle of the counters, so the counters are delayed one cycle to meet the
data, and 'vgaPins' registers everything once more.

__OLED.__ An SSD1306 byte is 8 pixels of one column, from 8 different rows,
so 'oledAdapter' reads the 8 rows one after the other (9 cycles) and then
holds the assembled byte. "Platform.Video.Oled" asks for a new address only
after loading the previous byte and loads the next one a whole SPI byte
(64 clocks) later, so the byte is always ready in time. Two modes (SW7 in
the CHIP-8 top, docs/systems/chip8.md):

* 'OledStretch': each CHIP-8 pixel is 2×1 OLED pixels, filling the panel;
* 'OledOneToOne': 64×32 in the left half, the right half dark.
-}
module Chip8.Display
  ( -- * VGA
    amber
  , div10
  , vgaChip8
  , chip8Stream
    -- * OLED
  , OledMode (..)
  , oledPixel
  , AdapterState (..)
  , adapterInit
  , adapterStep
  , oledAdapter
  ) where

import Clash.Prelude

import Platform.Video.VgaTiming

-- | Foreground colour on VGA (amber on black, as in docs/systems/chip8.md).
amber :: Rgb444
amber = Rgb444 0xF 0xB 0x0

-- | @v \`div\` 10@ for @v < 640@: 205 / 2048 is just above 1/10, and the
-- error stays below 1/15 for @v < 640@, so the floor is exact (checked
-- exhaustively in the tests).
div10 :: Unsigned 10 -> Unsigned 10
div10 v = resize ((resize v * 205 :: Unsigned 18) `shiftR` 11)

-- | Framebuffer row of a VGA line inside the picture (rows 80 to 399).
lineRow :: Unsigned 10 -> Index 32
lineRow y = unpack (resize (pack (div10 (y - 80))))

inPicture :: VgaOut -> Bool
inPicture o = vgaActive o && vgaY o >= 80 && vgaY o < 400

-- | The VGA picture. Returns the framebuffer row to read and the pins.
-- @fbRow@ must be the row asked for in the previous cycle.
vgaChip8
  :: HiddenClockResetEnable dom
  => Rgb444                     -- ^ foreground
  -> Signal dom VgaOut          -- ^ from 'vgaTiming' 'vga640x480'
  -> Signal dom (BitVector 64)  -- ^ framebuffer row data
  -> (Signal dom (Index 32), Signal dom VgaPins)
vgaChip8 fg vga fbRow = (addr, uncurry (vgaPins vga640x480) stream)
 where
  (addr, stream) = chip8Stream fg vga fbRow

-- | The picture as a pixel stream, before the VGA pin register: the
-- framebuffer row to read, and the timing with the colour of the same
-- clock (one clock after @vga@). VGA and HDMI both take this stream.
chip8Stream
  :: HiddenClockResetEnable dom
  => Rgb444
  -> Signal dom VgaOut
  -> Signal dom (BitVector 64)
  -> (Signal dom (Index 32), (Signal dom VgaOut, Signal dom Rgb888))
chip8Stream fg vga fbRow = (lineRow . vgaY <$> vga, (vgaD, rgb))
 where
  vgaD = register (vgaDecode vga640x480 (VgaCounters 0 0)) vga
  rgb  = fromRgb444 <$> (colour <$> vgaD <*> fbRow)
  colour o row
    | inPicture o && testBit row (63 - fromIntegral (div10 (vgaX o))) = fg
    | otherwise = black

data OledMode = OledStretch | OledOneToOne
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFDataX

-- | The CHIP-8 pixel shown at OLED column @c@ (0 to 127), if any.
oledPixel :: OledMode -> Unsigned 7 -> Maybe (Unsigned 6)
oledPixel OledStretch c = Just (truncateB (c `shiftR` 1))
oledPixel OledOneToOne c
  | c < 64    = Just (truncateB c)
  | otherwise = Nothing

data AdapterState = AdapterState
  { adKey  :: (Index 512, OledMode)  -- ^ byte being assembled
  , adStep :: Index 10               -- ^ rows asked for so far (0 = not started, 9 = done)
  , adAcc  :: BitVector 8
  , adOut  :: BitVector 8            -- ^ the last finished byte
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFDataX

adapterInit :: AdapterState
adapterInit = AdapterState (0, OledStretch) 0 0 0

-- | One cycle. Inputs: mode, the driver's byte address, the row asked for
-- in the previous cycle. Outputs: the row to read, the byte for the driver.
-- Invariant: while assembling, the row arriving now is row @adStep - 1@ of
-- the page.
adapterStep
  :: AdapterState -> (OledMode, Index 512, BitVector 64)
  -> (AdapterState, (Index 32, BitVector 8))
adapterStep s (mode, addr, row) = (s', (rowAddr, adOut s))
 where
  key     = (addr, mode)
  restart = key /= adKey s || adStep s == 0
  page    = slice d8 d7 (pack addr) :: BitVector 2
  col     = unpack (slice d6 d0 (pack addr)) :: Unsigned 7
  next :: Index 8
  next | restart       = 0
       | adStep s <= 7 = fromIntegral (adStep s)
       | otherwise     = 0
  rowAddr = unpack (page ++# pack next) :: Index 32
  pixel = case oledPixel mode col of
    Just cx -> testBit row (63 - fromIntegral cx)
    Nothing -> False
  acc' = replaceBit (fromIntegral (adStep s) - 1 :: Int) (boolToBit pixel) (adAcc s)
  s'
    | restart       = AdapterState key 1 0 (adOut s)
    | adStep s <= 7 = s { adStep = adStep s + 1, adAcc = acc' }
    | adStep s == 8 = s { adStep = 9, adAcc = acc', adOut = acc' }
    | otherwise     = s

-- | The adapter between a framebuffer video port and "Platform.Video.Oled".
-- Returns the framebuffer row to read and the byte for the driver.
oledAdapter
  :: HiddenClockResetEnable dom
  => Signal dom OledMode
  -> Signal dom (Index 512)       -- ^ driver's read address
  -> Signal dom (BitVector 64)    -- ^ framebuffer row data
  -> (Signal dom (Index 32), Signal dom (BitVector 8))
oledAdapter mode addr row = unbundle (mealy adapterStep adapterInit (bundle (mode, addr, row)))
