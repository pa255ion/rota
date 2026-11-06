{-|
The chime on the Game Boy's APU (docs/brand.md, "Sound, per machine"; the
page's @SYS.gb@, @gbLevel@, @drumSpec@): the lead on pulse 1 (25 %), the
harmony on pulse 2 (50 %), the bass on the wave channel (a 32-sample 4-bit
sine), the drums on the noise channel.

Legato as a sound driver plays it: a phrase starts with a trigger, then
only the frequency registers change (NRx3, and NRx4 without the trigger
bit when the high bits change), so the volume holds through the phrase.
The final note retriggers with a decaying envelope (the lead from 12,
a step every 6/64 s; the harmony from 11, every 7/64 s). A phrase ends
by turning its DAC off. Each drum hit retriggers the noise channel with
its own envelope and frequency.

Pulse frequency 131072 / (2048 - x) Hz, wave 65536 / (2048 - x) Hz, x
rounded as the page rounds it.
-}
module Rota.Sound.Gb
  ( gbFps
  , gbStream
  , gbPeriod
  , gbPeriodHz
  , gbNoise
  , gbNoiseHz
  , gbWaveRam
  , gbPulseVolume
  , gbFinalEnvelope
  ) where

import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.Maybe (fromMaybe)
import Data.Word (Word8)

import Rota.Score
import Rota.Sound

-- | 4194304 Hz / 70224 dots a frame.
gbFps :: Double
gbFps = 4194304 / 70224

-- | The 11-bit period value for a frequency (pulse, or wave for the bass).
gbPeriod :: Voice -> Double -> Int
gbPeriod v hz = jsRound (2048 - base v / hz)

gbPeriodHz :: Voice -> Int -> Double
gbPeriodHz v x = base v / fromIntegral (2048 - x)

base :: Voice -> Double
base Bass = 65536
base _ = 131072

-- | Each pulse voice's phrase volume (the page's @gbLevel(11, 6)@ and
-- @gbLevel(10, 7)@).
gbPulseVolume :: Voice -> Int
gbPulseVolume Lead = 11
gbPulseVolume _ = 10

-- | The final note's retrigger: start volume and envelope pace.
gbFinalEnvelope :: Voice -> (Int, Int)
gbFinalEnvelope v = (gbPulseVolume v + 1, if v == Lead then 6 else 7)

-- | The wave RAM: a 4-bit sine, sample i = round (7.5 + 7.5 sin (2 pi
-- (i + 0.5) / 32)), two samples a byte, the first in the high nibble.
gbWaveRam :: [Word8]
gbWaveRam = [ fromIntegral (s (2 * i) `shiftL` 4 .|. s (2 * i + 1)) | i <- [0 .. 15] ]
 where
  s :: Int -> Int
  s i = jsRound (7.5 + 7.5 * sin (2 * pi * (fromIntegral i + 0.5) / 32))

-- | A drum's noise setting: (NR43, envelope pace). NR43 = shift << 4 |
-- divisor code, 15-bit LFSR; the LFSR clock is 262144 / (d 2^shift) Hz
-- with d = 0.5 for code 0. The page's rates: kick 8192 Hz, snare 32768,
-- hat 262144, the roll 16384 to 65536 in six steps, the crash 16384.
gbNoise :: Drum -> (Word8, Int)
gbNoise d = (nr43 rate, pace)
 where
  (rate, pace) = case d of
    Kick -> (8192, 1)
    Snare -> (32768, 2)
    Hat -> (262144, 1)
    Roll r -> ([16384, 21845, 26214, 32768, 43690, 65536] !! r, 1)
    Crash -> (16384, 6)
  nr43 :: Double -> Word8
  nr43 hz = case [ fromIntegral (s `shiftL` 4 .|. c) | s <- [0 .. 13], c <- [0 .. 7], abs (noiseHz c s - hz) < 1 ] of
    (x : _) -> x
    [] -> error ("gbNoise: no setting for " <> show hz <> " Hz")

noiseHz :: Int -> Int -> Double
noiseHz c s = 262144 / ((if c == 0 then 0.5 else fromIntegral c) * 2 ^ s)

-- | The LFSR clock an NR43 value gives.
gbNoiseHz :: Word8 -> Double
gbNoiseHz v = noiseHz (fromIntegral v .&. 7) (fromIntegral v `shiftR` 4)

-- | The whole chime, frame by frame, from frame 0 (set-up) to the frame
-- after the last write.
gbStream :: Stream
gbStream = [ frameWrites k | k <- [0 .. lastFrame] ]
 where
  lastFrame = maximum [ spTo s | v <- [minBound .. maxBound], s <- noteSpans gbFps v ]
  frameWrites k = (if k == 0 then setUp else [])
    <> pulse Lead 0xFF10 k <> pulse Harm 0xFF15 k <> bass k <> noise k

setUp :: [Write]
setUp =
  [ Write 0xFF26 0x80 -- power on
  , Write 0xFF24 0x77 -- full volume both sides
  , Write 0xFF25 0xFF -- every channel to both sides
  , Write 0xFF10 0x00 -- no sweep
  , Write 0xFF1A 0x00 -- wave DAC off while its RAM is written
  ] <> zipWith Write [0xFF30 ..] gbWaveRam

-- | The period a voice plays in a frame (Nothing: silent).
periodAt :: Voice -> Int -> Maybe Int
periodAt v k = do
  s <- spanAt (noteSpans gbFps v) k
  pure (gbPeriod v (bentHz v (spIndex s) (fromIntegral (k - spFrom s) / gbFps)))

-- | A pulse voice; @nr0@ is the address before its NRx1.
pulse :: Voice -> Int -> Int -> [Write]
pulse v nr0 k = case spanAt sps k of
  Nothing
    | any (\s -> spPhraseEnd s && spTo s == k) sps -> [Write (nr0 + 2) 0x00] -- DAC off
    | otherwise -> []
  Just s
    | spPhraseStart s && k == spFrom s ->
        trig (fromIntegral (gbPulseVolume v `shiftL` 4))
    | final s && k == spFrom s ->
        let (vol, pace) = gbFinalEnvelope v in trig (fromIntegral (vol `shiftL` 4 .|. pace))
    | otherwise -> legato (nr0 + 3) x (periodAt v (k - 1))
   where
    x = fromMaybe 0 (periodAt v k)
    trig nr2 =
      [ Write (nr0 + 1) (if v == Lead then 0x40 else 0x80) -- duty
      , Write (nr0 + 2) nr2
      , Write (nr0 + 3) (lo x)
      , Write (nr0 + 4) (0x80 .|. hi x) ]
 where
  sps = noteSpans gbFps v
  final s = abs (nEnd (spNote s) - endBeat) < 1e-6

-- | The bass on the wave channel: on for the phrase, no bends.
bass :: Int -> [Write]
bass k = case spanAt sps k of
  Nothing
    | any (\s -> spPhraseEnd s && spTo s == k) sps -> [Write 0xFF1A 0x00]
    | otherwise -> []
  Just s
    | spPhraseStart s && k == spFrom s ->
        [ Write 0xFF1A 0x80, Write 0xFF1C 0x20, Write 0xFF1D (lo x), Write 0xFF1E (0x80 .|. hi x) ]
    | otherwise -> legato 0xFF1D x (periodAt Bass (k - 1))
   where
    x = fromMaybe 0 (periodAt Bass k)
 where
  sps = noteSpans gbFps Bass

-- | A frequency change in place: the low byte, and the high bits without
-- the trigger, each only when it changed.
legato :: Int -> Int -> Maybe Int -> [Write]
legato nr3 x old = case old of
  Nothing -> [Write nr3 (lo x), Write (nr3 + 1) (hi x)]
  Just o ->
    [ Write nr3 (lo x) | lo x /= lo o ] <> [ Write (nr3 + 1) (hi x) | hi x /= hi o ]

lo, hi :: Int -> Word8
lo x = fromIntegral (x .&. 0xFF)
hi x = fromIntegral (x `shiftR` 8)

noise :: Int -> [Write]
noise k = concat
  [ [ Write 0xFF21 (fromIntegral (hVol h `shiftL` 4 .|. pace)), Write 0xFF22 nr43, Write 0xFF23 0x80 ]
  | h <- drums, frameOfBeat gbFps (hBeat h) == k, let (nr43, pace) = gbNoise (hDrum h) ]
