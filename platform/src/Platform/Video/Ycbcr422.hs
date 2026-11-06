{-|
RGB → YCbCr 4:2:2 for the ADV7511's 16-bit input (PLAN.md M0d step 3).

__Colour.__ BT.601, limited range ("studio swing": Y 16 to 235, Cb/Cr 16
to 240, 128 = no colour), from the platform's 24-bit RGB (12-bit before M3 step 3.7). With the
8-bit components R, G, B,

@
Y  = 16  + ( 65.481 R + 128.553 G +  24.966 B) / 255
Cb = 128 + (-37.797 R -  74.203 G + 112.000 B) / 255
Cr = 128 + (112.000 R -  93.786 G -  18.214 B) / 255
@

(the coefficients are 219 or 224 times those of Kr = 0.299, Kb = 0.114).
The 12-bit design multiplied 4-bit components (R = 17 r) by the
coefficients × 4096 / 15, rounded ('kY', 'kCb', 'kCr'; each chroma row
adjusted by at most 1 so that it sums to 0, so greys have exactly no
colour), and rounded once at the end. The 24-bit design uses those
coefficients × 4096 / 17 for 8-bit components ('toFixed'), which gives
exactly the 12-bit results for 12-bit colours, and rounds the same way.

__4:2:2.__ Every pixel carries its Y; the chroma byte alternates Cb (even
pixels) and Cr (odd pixels) of each line. Both come from the /average/ of
the pixel pair (2k, 2k+1), so a colour edge between two pixels of a pair
is filtered rather than aliased. Pixel 0 of a line is the first pixel
with DE high, so the phase restarts on every line whatever the porches.
A line with an odd number of pixels gives its last pixel's Cb from that
pixel alone. Outside DE the data is black (Y 16, C 128).

__Timing.__ Three clocks from input to output ('ycbcrLatency'), for data,
syncs and DE alike: one for the multiplications, one to see the next
pixel (the pair partner), one output register.

How the bytes reach the chip's data pins (which of D[23:8] carry Y) is
board wiring, decided in the ZedBoard top (M0d step 4).
-}
module Platform.Video.Ycbcr422
  ( PixelIn (..)
  , Ycc422 (..)
  , ycc422Blank
  , ycbcrLatency
  , kY, kCb, kCr
  , kY8, kCb8, kCr8
  , YccFixed (..)
  , toFixed
  , toFixed444
  , YccState (..)
  , yccInit
  , yccStep
  , ycbcr422
  ) where

import Clash.Prelude

import Platform.Video.VgaTiming (Rgb444 (..), Rgb888 (..))

-- | The platform's pixel stream for one clock.
data PixelIn = PixelIn
  { piHSync :: Bit
  , piVSync :: Bit
  , piDe    :: Bool
  , piRgb   :: Rgb888
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFDataX

-- | One clock of the 4:2:2 bus.
data Ycc422 = Ycc422
  { ycHSync :: Bit
  , ycVSync :: Bit
  , ycDe    :: Bool
  , ycY     :: BitVector 8
  , ycC     :: BitVector 8   -- ^ Cb on even pixels, Cr on odd ones
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFDataX

-- | Idle bus with these sync levels: black, DE low.
ycc422Blank :: Bit -> Bit -> Ycc422
ycc422Blank h v = Ycc422 h v False 16 128

ycbcrLatency :: Int
ycbcrLatency = 3

-- | Coefficients × 4096 / 15 for (r, g, b) in 0..15.
kY, kCb, kCr :: Vec 3 (Signed 22)
kY  = 17881 :>  35104 :>  6817 :> Nil   -- 17880.68, 35103.54, 6817.38
kCb = -10321 :> -20262 :> 30583 :> Nil  -- -10321.06, -20262.41, 30583.47
kCr = 30583 :> -25609 :> -4974 :> Nil   -- 30583.47, -25609.84 (rounded up to sum 0), -4973.63

-- | Y, Cb, Cr of one pixel × 4096, without the offsets 16 / 128.
data YccFixed = YccFixed
  { yfY  :: Signed 22
  , yfCb :: Signed 22
  , yfCr :: Signed 22
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFDataX

-- | The 12-bit conversion: the reference that 'toFixed' reproduces exactly
-- for every 12-bit colour widened by 'fromRgb444'.
toFixed444 :: Rgb444 -> YccFixed
toFixed444 (Rgb444 r g b) = YccFixed (dot kY) (dot kCb) (dot kCr)
 where
  c   = map (unpack . zeroExtend) (r :> g :> b :> Nil) :: Vec 3 (Signed 22)
  dot k = sum (zipWith (*) k c)

-- | Coefficients for 8-bit components: the 4-bit ones × 4096 / 17, rounded
-- (so the sum below is in units of 1 / 4096 of 'YccFixed').
kY8, kCb8, kCr8 :: Vec 3 (Signed 36)
kY8  = map scale8 kY
kCb8 = map scale8 kCb
kCr8 = map scale8 kCr

scale8 :: Signed 22 -> Signed 36
scale8 k = let x = resize k * 4096 :: Signed 36 in (x + (if x < 0 then -8 else 8)) `quot` 17

-- | One pixel of the 24-bit stream. The products are 4096 times finer than
-- 'YccFixed'; rounding them back leaves an error below 1 / 2 unit (each
-- coefficient is off by at most 1 / 2 in 4096 / 17 units, times at most 3 ×
-- 255 is 383 of the 2048 that would matter), so for a widened 12-bit colour
-- (components 17 c) the result equals 'toFixed444' exactly, and the
-- CHIP-8 and Game Boy HDMI output didn't change with the wider stream.
toFixed :: Rgb888 -> YccFixed
toFixed (Rgb888 r g b) = YccFixed (dot kY8) (dot kCb8) (dot kCr8)
 where
  c   = map (unpack . zeroExtend) (r :> g :> b :> Nil) :: Vec 3 (Signed 36)
  dot k = resize ((sum (zipWith (*) k c) + 2048) `shiftR` 12)

-- | A pixel in the pipeline.
data Stage = Stage
  { stIn  :: PixelIn
  , stYcc :: YccFixed
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFDataX

data YccState = YccState
  { ysS1   :: Stage   -- ^ converted
  , ysS2   :: Stage   -- ^ the pixel being output
  , ysS3   :: Stage   -- ^ the one before it
  , ysOdd  :: Bool    -- ^ 'ysS2' is an odd pixel of its line
  , ysOut  :: Ycc422
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFDataX

-- | Reset: blank, with these idle sync levels.
yccInit :: Bit -> Bit -> YccState
yccInit h v = YccState st st st False (ycc422Blank h v)
 where st = Stage (PixelIn h v False (Rgb888 0 0 0)) (YccFixed 0 0 0)

-- | Round @s / 4096@ (or the pair average @(a + b) / 8192@) and add the offset.
-- The results stay inside 16..240, so the low 8 bits are the value.
round1, round2 :: Signed 22 -> Signed 22 -> Signed 22 -> BitVector 8
round1 off s _ = slice d7 d0 (pack (off + ((s + 2048) `shiftR` 12)))
round2 off a b = slice d7 d0 (pack (off + ((a + b + 4096) `shiftR` 13)))

yccStep :: YccState -> PixelIn -> YccState
yccStep s px = YccState
  { ysS1  = Stage px (toFixed (piRgb px))
  , ysS2  = s1
  , ysS3  = s2
    -- the pixel entering S2 is odd if the one before it (now in S2) was an
    -- even pixel of the same line
  , ysOdd = piDe (stIn s1) && piDe (stIn s2) && not (ysOdd s)
  , ysOut = out
  }
 where
  s1 = ysS1 s
  s2 = ysS2 s
  s3 = ysS3 s
  i2 = stIn s2
  y2 = stYcc s2
  out
    | not (piDe i2) = ycc422Blank (piHSync i2) (piVSync i2)
    | otherwise     = Ycc422 (piHSync i2) (piVSync i2) True (round1 16 (yfY y2) 0) chroma
  chroma
    | ysOdd s             = round2 128 (yfCr (stYcc s3)) (yfCr y2)
    | piDe (stIn s1)      = round2 128 (yfCb y2) (yfCb (stYcc s1))
    | otherwise           = round2 128 (yfCb y2) (yfCb y2)

-- | The converter as a circuit; idle sync levels for reset.
ycbcr422 :: HiddenClockResetEnable dom => Bit -> Bit -> Signal dom PixelIn -> Signal dom Ycc422
ycbcr422 h v px = ysOut <$> st
 where st = register (yccInit h v) (yccStep <$> st <*> px)
