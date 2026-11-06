{-|
The Game Gear's PSG (step 5.4; docs/systems/gamegear.md): Sega's SN76489
variant, from SMS Power's "SN76489" page.

__Writes:__ a byte with bit 7 set latches a register (bits 6-5 the
channel, bit 4 volume/tone) and writes its low 4 bits; a byte with bit 7
clear writes the latched register: bits 5-0 become a tone period's high
six bits, or the low 4 bits of a volume or the noise control. Any write
to the noise control resets the LFSR to 8000h.

__Clock:__ the CPU clock / 16 ('psgTick' once per 16 T-states). Each tone
counter counts down; at zero it reloads the period and flips its output.
A period of 0 counts as 1 (*to verify*: SMS Power says Sega's chip treats
0 as 1). The noise counter reloads with 10h, 20h, 40h or tone 2's period
(noise control bits 1-0) and shifts the LFSR on every second flip (when
its output goes high): white noise feeds back bits 0 XOR 3, periodic
noise bit 0; the channel's output is the LFSR's bit 0.

__Output:__ each channel is plus or minus its volume (attenuation 0 =
full, 2 dB per step, 15 = off) as its output bit is high or low: the chip
puts out 0 or the volume, and the Game Gear's coupling capacitors take
the DC away, so a digital output (HDMI, S/PDIF, a WAV file) carries the
bipolar signal a speaker would. Port 06h routes each channel to the left
and/or right side (1 = on). Samples are signed 16-bit, the sum of the
four channels within ±4 × 8191.
-}
module Gg.Spec.Psg
  ( Psg (..)
  , newPsg
  , psgWrite
  , psgTick
  , psgSample
  , psgRegisters
  , psgLfsr
  , psgLatched
  , volumeTable
  ) where

import Prelude

import Control.Monad (forM_, when)
import Control.Monad.ST (ST)
import Data.Bits
import Data.Int (Int16)
import qualified Data.Vector.Unboxed.Mutable as MV
import Data.Word

newtype Psg s = Psg (MV.MVector s Int)

-- Slots: tone periods 0-2 and the noise control (3), volumes (4-7), the
-- latched register (8), counters (9-12), outputs (13-16), the LFSR (17).
period, vol, latch, counter, output, lfsr, slots :: Int
period = 0
vol = 4
latch = 8
counter = 9
output = 13
lfsr = 17
slots = 18

newPsg :: ST s (Psg s)
newPsg = do
  v <- MV.replicate slots 0
  forM_ [0 .. 3] $ \c -> MV.write v (vol + c) 15
  MV.write v lfsr 0x8000
  pure (Psg v)

rd :: Psg s -> Int -> ST s Int
rd (Psg v) = MV.unsafeRead v

wr :: Psg s -> Int -> Int -> ST s ()
wr (Psg v) = MV.unsafeWrite v

-- | A byte written to the PSG port.
psgWrite :: Psg s -> Word8 -> ST s ()
psgWrite p b = do
  let bi = fromIntegral b :: Int
  if testBit b 7
    then do
      let reg = (bi `shiftR` 4) .&. 7 -- channel * 2 + (1 = volume)
      wr p latch reg
      setLow reg (bi .&. 0x0F)
    else do
      reg <- rd p latch
      let ch = reg `shiftR` 1
      if even reg && ch < 3
        then do
          old <- rd p (period + ch)
          wr p (period + ch) ((old .&. 0x0F) .|. ((bi .&. 0x3F) `shiftL` 4))
        else setLow reg (bi .&. 0x0F)
 where
  setLow reg v = do
    let ch = reg `shiftR` 1
    if odd reg
      then wr p (vol + ch) v
      else if ch < 3
        then do
          old <- rd p (period + ch)
          wr p (period + ch) ((old .&. 0x3F0) .|. v)
        else do
          wr p (period + 3) (v .&. 7)
          wr p lfsr 0x8000

-- | One PSG clock (16 CPU T-states).
psgTick :: Psg s -> ST s ()
psgTick p = do
  forM_ [0 .. 2] $ \ch -> do
    n <- rd p (counter + ch)
    if n <= 1
      then do
        per <- rd p (period + ch)
        wr p (counter + ch) (max 1 per)
        o <- rd p (output + ch)
        wr p (output + ch) (1 - o)
      else wr p (counter + ch) (n - 1)
  n <- rd p (counter + 3)
  when (n <= 1) $ do
    ctl <- rd p (period + 3)
    per <- case ctl .&. 3 of
      3 -> max 1 <$> rd p (period + 2)
      r -> pure (0x10 `shiftL` r)
    wr p (counter + 3) per
    o <- rd p (output + 3)
    let o' = 1 - o
    wr p (output + 3) o'
    when (o' == 1) $ do
      l <- rd p lfsr
      let fb = if testBit ctl 2
            then (l .&. 1) `xor` ((l `shiftR` 3) .&. 1)
            else l .&. 1
      wr p lfsr ((l `shiftR` 1) .|. (fb `shiftL` 15))
  when (n > 1) (wr p (counter + 3) (n - 1))

-- | Attenuation → amplitude, 2 dB per step, 15 = silent.
volumeTable :: Int -> Int
volumeTable a
  | a >= 15 = 0
  | otherwise = round (8191 * (10 :: Double) ** (negate (fromIntegral a) * 2 / 20))

-- | The current output (left, right) for the stereo routing @st@ (port 06h).
psgSample :: Psg s -> Word8 -> ST s (Int16, Int16)
psgSample p st = do
  chans <- mapM chan [0 .. 3]
  let side bit0 = sum [ a | (ch, a) <- zip [0 ..] chans, testBit st (bit0 + ch) ]
  pure (fromIntegral (side 4 :: Int), fromIntegral (side 0))
 where
  chan ch = do
    on <- if ch < 3 then (== 1) <$> rd p (output + ch) else (\l -> l .&. 1 == 1) <$> rd p lfsr
    a <- rd p (vol + ch)
    pure (if on then volumeTable a else negate (volumeTable a))

-- | Tone periods 0-2, the noise control, the four volumes.
psgRegisters :: Psg s -> ST s [Int]
psgRegisters p = mapM (rd p) [0 .. 7]

-- | The noise LFSR.
psgLfsr :: Psg s -> ST s Int
psgLfsr p = rd p lfsr

-- | The latched register, 0 to 7 (side track N's save states).
psgLatched :: Psg s -> ST s Int
psgLatched p = rd p latch
