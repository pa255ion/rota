{-|
The chime on the NES's APU (docs/brand.md, "Sound, per machine"; the
page's @SYS.nes@, @nesLevel@, @drumSpec@): the lead on pulse 1 (25 %),
the harmony on pulse 2 (50 %), the bass on the triangle, the drums on the
noise channel.

The pulses run in constant-volume mode with a software envelope, accented
on each note (the lead from 12 down to 9, the harmony from 11 to 8, a step
a frame; the final note fades after 45 % of its length, a step every 4
frames). Pulse frequency 1789773 / (16 (t + 1)) Hz, the triangle's
1789773 / (32 (t + 1)) Hz.

__The high period byte.__ Writing $4003/$4007 restarts a pulse's
waveform, so legato changes only the low byte. docs/brand.md asks for
every lead and harmony period below 256, but the harmony's F♯4 (beat 5)
needs t = 301: the score crosses the boundary twice (A4 to F♯4 at beat
5, back to A4 at beat 6, where the crash covers it). So each note keeps
its own high byte: bends and vibrato are clamped to the note's 256-value
range, and the high byte is rewritten only on the first frame of a note
whose nominal period lies in the other range. The lead never crosses.

The drums use the noise channel in constant-volume mode with a software
decay (the page's 1/240 s units: a step every @n@ quarter frames), the
roll climbing the period table from 508 to 128 CPU cycles.
-}
module Rota.Sound.Nes
  ( nesFps
  , nesCpuHz
  , nesStream
  , nesPeriod
  , nesPeriodHz
  , nesNoisePeriods
  , nesNoise
  , nesEnvelope
  ) where

import Data.Bits (shiftR, (.&.), (.|.))
import Data.Word (Word8)

import Rota.Score
import Rota.Sound

nesCpuHz :: Double
nesCpuHz = 1789773

-- | 3 dots a CPU cycle, 89341.5 dots a frame (rendering on).
nesFps :: Double
nesFps = 3 * nesCpuHz / 89341.5

-- | The period value for a frequency, before the high-byte clamp.
nesPeriod :: Voice -> Double -> Int
nesPeriod v hz = jsRound (nesCpuHz / (div' v * hz) - 1)

nesPeriodHz :: Voice -> Int -> Double
nesPeriodHz v t = nesCpuHz / (div' v * fromIntegral (t + 1))

div' :: Voice -> Double
div' Bass = 32
div' _ = 16

-- | The noise period table (NTSC, CPU cycles); index = $400E's low bits.
nesNoisePeriods :: [Int]
nesNoisePeriods = [4, 8, 16, 32, 64, 96, 128, 160, 202, 254, 380, 508, 762, 1016, 2034, 4068]

-- | A drum's ($400E period index, decay step in quarter frames).
nesNoise :: Drum -> (Int, Int)
nesNoise d = (idx p, step)
 where
  (p, step) = case d of
    Kick -> (1016, 3)
    Snare -> (160, 6)
    Hat -> (16, 2)
    Roll r -> ([508, 380, 254, 202, 160, 128] !! r, 3)
    Crash -> (202, 14)
  idx x = case lookup x (zip nesNoisePeriods [0 ..]) of
    Just i -> i
    Nothing -> error ("nesNoise: " <> show x <> " is not in the table")

-- | A pulse voice's (accent, sustain) volumes.
levels :: Voice -> (Int, Int)
levels Lead = (12, 9)
levels _ = (11, 8)

-- | The software envelope of a note, frame by frame from its first frame
-- (the page's @nesLevel@, in frames).
nesEnvelope :: Voice -> NoteSpan -> [Int]
nesEnvelope v s = take len (expand (go 0 acc))
 where
  (acc, sus) = levels v
  len = spTo s - spFrom s
  final = abs (nEnd (spNote s) - endBeat) < 1e-6
  durSec = (nEnd (spNote s) - nStart (spNote s)) * beatSec
  go j x
    | j >= len = []
    | otherwise = (j, x) : go (j + step x') x'
   where
    x' | x > sus = x - 1
       | final && fromIntegral j / nesFps > 0.45 * durSec = max 0 (x - 1)
       | otherwise = x
  step x = if final && x <= sus then 4 else 1
  -- hold each value until the next change
  expand ((j, x) : rest@((j', _) : _)) = replicate (j' - j) x <> expand rest
  expand [(_, x)] = repeat x
  expand [] = []

nesStream :: Stream
nesStream = [ frameWrites k | k <- [0 .. lastFrame] ]
 where
  lastFrame = maximum [ spTo s | v <- [minBound .. maxBound], s <- noteSpans nesFps v ]
  frameWrites k = (if k == 0 then setUp else [])
    <> pulse Lead 0x4000 k <> pulse Harm 0x4004 k <> triangle k <> noise k

setUp :: [Write]
setUp =
  [ Write 0x4015 0x0F -- pulses, triangle, noise
  , Write 0x4017 0x40 -- 4-step sequence, no frame IRQ
  , Write 0x4001 0x08, Write 0x4005 0x08 -- sweeps off (negate set: no muting)
  , Write 0x4000 (duty Lead .|. 0x30), Write 0x4004 (duty Harm .|. 0x30)
  , Write 0x4008 0x80, Write 0x400C 0x30 ]

duty :: Voice -> Word8
duty Lead = 0x40 -- 25 %
duty _ = 0x80             -- 50 %

-- | The period a voice plays in a frame, clamped to its note's high byte.
periodAt :: Voice -> Int -> Maybe Int
periodAt v k = do
  s <- spanAt (noteSpans nesFps v) k
  let n = spNote s
      t = nesPeriod v (bentHz v (spIndex s) (fromIntegral (k - spFrom s) / nesFps))
      h = nesPeriod v (midiHz (nMidi n)) `shiftR` 8
  pure (if v == Bass then t else max (h * 256) (min (h * 256 + 255) t))

volAt :: Voice -> Int -> Maybe Int
volAt v k = do
  s <- spanAt (noteSpans nesFps v) k
  pure (nesEnvelope v s !! (k - spFrom s))

pulse :: Voice -> Int -> Int -> [Write]
pulse v r0 k = case (spanAt sps k, periodAt v k, volAt v k) of
  (Just s, Just t, Just vol)
    | spPhraseStart s && k == spFrom s ->
        [ Write r0 (ctrl vol), Write (r0 + 2) (lo t), Write (r0 + 3) (0x08 .|. hi t) ]
    | otherwise ->
        [ Write r0 (ctrl vol) | Just vol /= volAt v (k - 1) ]
          <> [ Write (r0 + 2) (lo t) | Just (lo t) /= (lo <$> periodAt v (k - 1)) ]
          <> [ Write (r0 + 3) (0x08 .|. hi t) | Just (hi t) /= (hi <$> periodAt v (k - 1)) ]
  _ | any (\s -> spPhraseEnd s && spTo s == k) sps -> [Write r0 (ctrl 0)]
    | otherwise -> []
 where
  sps = noteSpans nesFps v
  ctrl :: Int -> Word8
  ctrl vol = duty v .|. 0x30 .|. fromIntegral vol

triangle :: Int -> [Write]
triangle k = case (spanAt sps k, periodAt Bass k) of
  (Just s, Just t)
    | spPhraseStart s && k == spFrom s ->
        [ Write 0x4008 0xFF, Write 0x400A (lo t), Write 0x400B (0x08 .|. hi t) ]
    | otherwise ->
        [ Write 0x400A (lo t) | Just (lo t) /= (lo <$> periodAt Bass (k - 1)) ]
          <> [ Write 0x400B (0x08 .|. hi t) | Just (hi t) /= (hi <$> periodAt Bass (k - 1)) ]
  _ | any (\s -> spPhraseEnd s && spTo s == k) sps -> [Write 0x4008 0x80]
    | otherwise -> []
 where
  sps = noteSpans nesFps Bass

lo, hi :: Int -> Word8
lo x = fromIntegral (x .&. 0xFF)
hi x = fromIntegral (x `shiftR` 8)

-- | The drum sounding in a frame: the latest hit at or before it, and
-- its volume then.
noiseVol :: Int -> Maybe (Hit, Int, Int)
noiseVol k = case [ (h, f) | h <- drums, let f = frameOfBeat nesFps (hBeat h), f <= k ] of
  [] -> Nothing
  hs -> let (h, f) = last hs
            (_, step) = nesNoise (hDrum h)
            tau = fromIntegral (k - f) / nesFps
        in Just (h, f, max 0 (hVol h - floor (tau / (fromIntegral step / 240))))

noise :: Int -> [Write]
noise k = case noiseVol k of
  Just (h, f, vol)
    | f == k -> [ Write 0x400E (fromIntegral (fst (nesNoise (hDrum h)))), Write 0x400C (0x30 .|. fromIntegral vol), Write 0x400F 0x08 ]
    | Just vol /= (thd <$> noiseVol (k - 1)) -> [ Write 0x400C (0x30 .|. fromIntegral vol) ]
  _ -> []
 where
  thd (_, _, x) = x
