{-|
A model of the ADV7511 for tests (simulation only, not hardware): what the
chip does with the registers "Platform.Video.Adv7511" writes and the bus
"Platform.Video.Ycbcr422" drives. Shared by layer 1 tests and the layer 4
golden images (PLAN.md M0d step 5); the Zig HDMI sink in the Verilator
harness (@platform/sim/hdmi.zig@, @i2c.zig@) implements the same
definitions, so the two must change together.

* 'chipDefaults': the power-on values the simulated chip starts with. All
  other registers read 0. (Real defaults are in the Programming Guide;
  only these two matter to the table's masked updates and the sequence.)
* 'expectedWrites': the register writes of one configuration, in order.
* 'cscRgb': the colour space converter, as the Programming Guide's
  equations 1 to 3 (4.3.7.1) with integer arithmetic, defined here
  exactly: 8-bit inputs as 12-bit codes (× 16), each channel
  @((c1 x1 + c2 x2 + c3 x3) >> 12) + c4@, times @2^mode@, then @>> 4@
  and clamped to 0..255 (@>>@ rounds towards minus infinity). The chip's
  own rounding isn't documented; this is our reference.
* 'upsample': 4:2:2 to 4:4:4 by zero-order interpolation (the default,
  0x17[2] = 0): both pixels of a pair use the pair's Cb (sent with the
  even pixel) and Cr (sent with the odd one).
-}
module Platform.Sim.Adv7511
  ( chipDefaults
  , chipRevision
  , registerImage
  , expectedWrites
  , expectedWritesFor
  , writesLog
  , cscRgb
  , upsample
  , hdmiLine
  , hdmiFrame
  ) where

import Prelude

import Codec.Picture (PixelRGB8 (..), generateImage)
import Data.Bits (complement, shiftL, shiftR, (.&.), (.|.))
import qualified Data.Map.Strict as M
import qualified Data.Vector as V
import Data.Word (Word8)
import Numeric (showHex)

import qualified Clash.Prelude as C

import Platform.Sim.Frame (Frame)
import Platform.Video.Adv7511.Table
import Platform.Video.VgaTiming (Rgb888 (..))
import Platform.Video.Ycbcr422

chipRevision :: Word8
chipRevision = 0x14

-- | Revision (0x00, the value the ZedBoard's chip reported, 2026-10-02)
-- and power (0x41, powered down: Linux's register defaults).
chipDefaults :: M.Map Word8 Word8
chipDefaults = M.fromList [(0x00, chipRevision), (0x41, 0x50)]

w8 :: C.BitVector 8 -> Word8
w8 = fromIntegral

-- | The writes of one pass through the table, starting from
-- 'chipDefaults': a masked entry writes the old value updated.
expectedWrites :: [(Word8, Word8)]
expectedWrites = expectedWritesFor adv7511Table

-- | The writes of a given table (e.g. 'adv7511HdmiAudioTable').
expectedWritesFor :: [TableEntry] -> [(Word8, Word8)]
expectedWritesFor tbl = reverse (snd (foldl step (chipDefaults, []) tbl))
 where
  step (m, acc) e =
    let r = w8 (teReg e); k = w8 (teMask e); v = w8 (teValue e)
        new = (M.findWithDefault 0 r m .&. complement k) .|. (v .&. k)
    in (M.insert r new m, (r, new) : acc)

-- | The registers after a list of writes on top of 'chipDefaults'.
registerImage :: [(Word8, Word8)] -> M.Map Word8 Word8
registerImage = foldl (\m (r, v) -> M.insert r v m) chipDefaults

-- | Y, Cb, Cr (8 bit) to RGB (8 bit) with the CSC registers of an image.
cscRgb :: M.Map Word8 Word8 -> (Int, Int, Int) -> (Word8, Word8, Word8)
cscRgb regs (y, cb, cr) = (chan 0, chan 1, chan 2)
 where
  reg :: Int -> Int
  reg r = fromIntegral (M.findWithDefault 0 (fromIntegral r) regs)
  coef :: Int -> Int
  coef i = let v = (reg (0x18 + 2 * i) .&. 0x1F) `shiftL` 8 .|. reg (0x19 + 2 * i)
           in if v >= 4096 then v - 8192 else v
  mode = min 2 ((reg 0x18 `shiftR` 5) .&. 3)
  -- channel order of PG Table 31: A = R from (Cr, Y, Cb), B = G, C = B
  ins = [cr `shiftL` 4, y `shiftL` 4, cb `shiftL` 4]
  chan k =
    let c j = coef (4 * k + j)
        out12 = ((sum (zipWith (*) (map c [0, 1, 2]) ins) `shiftR` 12) + c 3) `shiftL` mode
    in fromIntegral (max 0 (min 255 (out12 `shiftR` 4)))

-- | One line of the 4:2:2 bus, (Y, C) per pixel, to (Y, Cb, Cr) per pixel.
upsample :: [(Int, Int)] -> [(Int, Int, Int)]
upsample ((y0, cb) : (y1, cr) : rest) = (y0, cb, cr) : (y1, cb, cr) : upsample rest
upsample [(y0, cb)] = [(y0, cb, 128)]
upsample [] = []

-- | What the monitor gets for one line of colours: the line through the
-- converter (as a DE run with blanking around it), upsampled, converted.
hdmiLine :: M.Map Word8 Word8 -> [Rgb888] -> [(Word8, Word8, Word8)]
hdmiLine regs cs = map (cscRgb regs) (upsample [ (fromIntegral (ycY o), fromIntegral (ycC o)) | o <- outs, ycDe o ])
 where
  idle = PixelIn 1 1 False (Rgb888 0 0 0)
  pxs = map (PixelIn 1 1 True) cs ++ replicate (ycbcrLatency + 1) idle
  outs = map ysOut (scanl yccStep (yccInit 1 1) (idle : pxs))

-- | A whole frame: @pixel x y@ is the colour the design sends (black
-- outside the picture is not needed: only DE pixels are shown).
hdmiFrame :: M.Map Word8 Word8 -> Int -> Int -> (Int -> Int -> Rgb888) -> Frame
hdmiFrame regs w h pixel = generateImage at w h
 where
  rows = V.fromList [ V.fromList (hdmiLine regs [ pixel x y | x <- [0 .. w - 1] ]) | y <- [0 .. h - 1] ]
  at x y = let (r, g, b) = (rows V.! y) V.! x in PixelRGB8 r g b

-- | Register writes in the format of the layer 4 harness's I2C log
-- (platform/sim/main.zig): one @0xRR 0xVV@ line per write.
writesLog :: [(Word8, Word8)] -> String
writesLog ws = concat [ hex r <> " " <> hex v <> "\n" | (r, v) <- ws ]
 where
  hex b = "0x" <> pad (map upper (showHex b ""))
  pad x = replicate (2 - length x) '0' <> x
  upper c = if c >= 'a' && c <= 'f' then toEnum (fromEnum c - 32) else c
