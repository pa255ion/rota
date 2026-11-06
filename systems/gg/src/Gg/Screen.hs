{-|
Frames and audio out of 'Gg.Spec', for @gg-run@ and the goldens: the
160 × 144 window as a PNG (each 4-bit channel scaled to 8 bits, × 17),
a frame hash, and a 16-bit stereo WAV file.
-}
module Gg.Screen
  ( frameRgb
  , framePng
  , frameVgaPng
  , frameSmsVgaPng
  , frameHash
  , wavFile
  ) where

import Prelude

import Codec.Picture (Image, PixelRGB8 (..), encodePng, generateImage)
import Data.Bits
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BL
import Data.Int (Int16)
import qualified Data.Vector.Unboxed as U
import Data.Word


-- | A CRAM colour (----BBBBGGGGRRRR) as 8-bit R, G, B.
frameRgb :: Word16 -> (Word8, Word8, Word8)
frameRgb c =
  let ch s = fromIntegral ((c `shiftR` s) .&. 0x0F) * 17
  in (ch 0, ch 4, ch 8)

-- | A frame @w@ pixels wide (160 for the Game Gear window, 256 in Master
-- System mode) as a PNG.
framePng :: Int -> U.Vector Word16 -> BL.ByteString
framePng w fb = encodePng img
 where
  img :: Image PixelRGB8
  img = generateImage px w (U.length fb `div` w)
  px x y = let (r, g, b) = frameRgb (fb U.! (y * w + x)) in PixelRGB8 r g b

-- | A Game Gear frame (160 × 144) as the board shows it on 640 × 480:
-- ×3 at (80, 24), black around ("Gg.Display"): the layer 4 goldens.
frameVgaPng :: U.Vector Word16 -> BL.ByteString
frameVgaPng fb = encodePng img
 where
  img :: Image PixelRGB8
  img = generateImage px 640 480
  px x y
    | x >= 80 && x < 560 && y >= 24 && y < 456 =
        let (r, g, b) = frameRgb (fb U.! (((y - 24) `div` 3) * 160 + (x - 80) `div` 3)) in PixelRGB8 r g b
    | otherwise = PixelRGB8 0 0 0

-- | A Master System frame (256 × 192) as the board shows it on 640 × 480
-- (M6, "Sms.Display"): ×2 at (64, 48), the border colour around it, as a
-- TV shows the backdrop: the layer 4 goldens.
frameSmsVgaPng :: Word16 -> U.Vector Word16 -> BL.ByteString
frameSmsVgaPng border fb = encodePng img
 where
  img :: Image PixelRGB8
  img = generateImage px 640 480
  px x y = let (r, g, b) = frameRgb (colour x y) in PixelRGB8 r g b
  colour x y
    | x >= 64 && x < 576 && y >= 48 && y < 432 = fb U.! (((y - 48) `div` 2) * 256 + (x - 64) `div` 2)
    | otherwise = border

-- | FNV-1a over the frame's colours (a short, stable fingerprint for
-- goldens and for comparing runs).
frameHash :: U.Vector Word16 -> Word64
frameHash = U.foldl' step 0xcbf29ce484222325
 where
  step h c = ((h `xor` fromIntegral (c .&. 0xFF)) * 0x100000001b3 `xor` fromIntegral (c `shiftR` 8)) * 0x100000001b3

-- | A 16-bit stereo PCM WAV file at @rate@ Hz.
wavFile :: Int -> [(Int16, Int16)] -> BL.ByteString
wavFile rate samples = B.toLazyByteString $
  B.string7 "RIFF" <> w32 (36 + dataLen) <> B.string7 "WAVE"
  <> B.string7 "fmt " <> w32 16 <> w16 1 <> w16 2 <> w32 rate <> w32 (rate * 4) <> w16 4 <> w16 16
  <> B.string7 "data" <> w32 dataLen
  <> foldMap (\(l, r) -> B.int16LE l <> B.int16LE r) samples
 where
  dataLen = length samples * 4
  w32 = B.word32LE . fromIntegral :: Int -> B.Builder
  w16 = B.word16LE . fromIntegral :: Int -> B.Builder
