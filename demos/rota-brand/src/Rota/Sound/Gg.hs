{-|
The chime on the Game Gear's SN76489 (docs/brand.md, "Sound, per
machine"; the audition page's @SYS.sms@, @smsLevel@, @smsDrum@): the lead
on tone 1, the harmony on tone 2, the bass on tone 3, the drums on the
noise channel as white noise. A tone sounds 3579545 / (32 N) Hz with a
10-bit N, so tone 3 can't go below about 109 Hz: bass notes under that
play an octave up (no periodic-noise bass, decided 2026-10-06).

__Volume__ is 4-bit attenuation, 2 dB a step, with no envelopes in the
chip: every envelope is a write a frame, as the page's @smsLevel@. A note
starts at its voice's accent (lead 1, harmony 2, bass 0) and falls a step
every 2 frames to its sustain (3, 4, 2); the final note, after 40 % of its
length, falls a step every 4 frames to silence. A drum (the page's
@smsDrum@) starts at attenuation 15 - its volume and falls a step every
1 to 5 frames until 15 or the next hit; kick at the chip's slowest noise
rate (3579545 / 2048), snare 1024, hat 512, the roll climbing from 2048 to
512, the crash 512, 5 frames a step.

Every write is a byte to port 7Fh: latch bytes (1 cc t dddd) and data
bytes (0 x dddddd, the tone's high 6 bits). The stream holds them per
frame ("Rota.Sound"'s 'Write', register 7Fh).
-}
module Rota.Sound.Gg
  ( ggFps
  , psgClock
  , ggStream
  , ggSoundBytes
  , toneN
  , toneHz
  ) where

import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.Word (Word8)

import Rota.Score
import Rota.Sound

psgClock :: Double
psgClock = 3579545

-- | 262 lines of 228 T-states a frame.
ggFps :: Double
ggFps = psgClock / 59736

-- | A tone's N for a frequency, and what it sounds.
toneN :: Double -> Int
toneN f = max 1 (min 1023 (jsRound (psgClock / (32 * f))))

toneHz :: Int -> Double
toneHz n = psgClock / (32 * fromIntegral n)

-- | The lowest pitch tone 3 reaches.
minHz :: Double
minHz = psgClock / (32 * 1023)

-- | (accent, sustain) attenuation.
levels :: Voice -> (Int, Int)
levels Lead = (1, 3)
levels Harm = (2, 4)
levels Bass = (0, 2)

channel :: Voice -> Int
channel Lead = 0
channel Harm = 1
channel Bass = 2

-- | A note's attenuation, frame by frame from its first frame.
envelope :: Voice -> NoteSpan -> [Int]
envelope v s = take len (go 0 acc 0)
 where
  (acc, sus) = levels v
  len = spTo s - spFrom s
  final = abs (nEnd (spNote s) - endBeat) < 1e-6
  -- the value, held for the frames until the next change
  go j a hold
    | j >= len = []
    | hold > 0 = a : go (j + 1) a (hold - 1)
    | otherwise =
        let fading = final && fromIntegral j > 0.4 * fromIntegral len
            a' | j == 0 = a
               | a < sus = a + 1
               | fading = min 15 (a + 1)
               | otherwise = a
            step = if final && a' >= sus then 4 else 2
        in a' : go (j + 1) a' (step - 1)

-- | A voice in a frame: its N and attenuation.
voiceAt :: Voice -> Int -> Maybe (Int, Int)
voiceAt v k = do
  s <- spanAt (noteSpans ggFps v) k
  let up = v == Bass && midiHz (nMidi (spNote s)) < minHz
      hz = bentHz v (spIndex s) (fromIntegral (k - spFrom s) / ggFps) * (if up then 2 else 1)
  pure (toneN hz, envelope v s !! (k - spFrom s))

-- | The drum in a frame: its noise rate (0 to 2: clock / 512, 1024,
-- 2048) and attenuation, and whether it starts there.
drumAt :: Int -> Maybe (Int, Int, Bool)
drumAt k = case [ (h, f, f') | (h, f, f') <- hitFrames, f <= k, k < f' ] of
  ((h, f, _) : _) ->
    let (rate, step) = spec (hDrum h)
        a = 15 - hVol h + (k - f) `div` step
    in if a >= 15 then Nothing else Just (rate, a, k == f)
  [] -> Nothing
 where
  hitFrames =
    [ (h, frameOfBeat ggFps (hBeat h), maybe maxBound (frameOfBeat ggFps . hBeat) next)
    | (h, next) <- zip drums (map Just (drop 1 drums) <> [Nothing]) ]
  spec d = case d of
    Kick -> (2, 1)
    Snare -> (1, 2)
    Hat -> (0, 1)
    Roll r -> ([2, 2, 1, 1, 0, 0] !! r, 1)
    Crash -> (0, 5)

latchTone, latchVol :: Int -> Int -> Word8
latchTone ch n = fromIntegral (0x80 .|. (ch `shiftL` 5) .|. (n .&. 0x0F))
latchVol ch a = fromIntegral (0x90 .|. (ch `shiftL` 5) .|. a)

-- | The bytes a frame writes.
frameBytes :: Int -> [Word8]
frameBytes k = setUp <> concatMap tone [Lead, Harm, Bass] <> noise
 where
  setUp = if k == 0 then [ latchVol ch 15 | ch <- [0 .. 3] ] else []
  tone v =
    let ch = channel v
        now = voiceAt v k
        before = if k == 0 then Nothing else voiceAt v (k - 1)
    in case now of
         Just (n, a) ->
           (case before of
              Just (n0, _)
                | n0 == n -> []
                | n0 `shiftR` 4 == n `shiftR` 4 -> [latchTone ch n]
              _ -> [latchTone ch n, fromIntegral (n `shiftR` 4)])
             <> [ latchVol ch a | Just a /= (snd <$> before) ]
         Nothing -> [ latchVol ch 15 | before /= Nothing ]
  noise = case (drumAt k, if k == 0 then Nothing else drumAt (k - 1)) of
    (Just (rate, a, start), prev) ->
      [ fromIntegral (0xE4 .|. rate) | start ]
        <> [ latchVol 3 a | Just a /= ((\(_, x, _) -> x) <$> prev) || start ]
    (Nothing, Just _) -> [latchVol 3 15]
    (Nothing, Nothing) -> []

-- | The chime's writes, frame by frame.
ggStream :: Stream
ggStream = [ map (Write 0x7F) (frameBytes k) | k <- [0 .. lastFrame] ]
 where
  lastFrame = maximum [ spTo s | v <- [minBound .. maxBound], s <- noteSpans ggFps v ] + 1

-- | The stream for the ROM: per frame a count, then the bytes; 0xFF ends.
ggSoundBytes :: [Word8]
ggSoundBytes = concatMap frame ggStream <> [0xFF]
 where
  frame ws
    | length ws >= 0xFF = error "ggSoundBytes: too many writes in a frame"
    | otherwise = fromIntegral (length ws) : map wVal ws
