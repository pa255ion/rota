{-|
The chime on the Atari 800XL's POKEY (docs/brand.md, "Sound, per
machine"; the audition page's @SYS.a800@, @nesLevel@, @stealDrums@): the
lead on channel 3 and the harmony on channel 4, pure tones from the 64 kHz
clock (63920 / (2 (AUDF + 1)) Hz, so high notes land up to about 20 cents
off); the bass on channels 1 and 2 joined into one 16-bit channel clocked
at 1.79 MHz (1789773 / (2 (N + 7)) Hz, AUDCTL $50), sounding on channel 2.

Volumes are software envelopes, a write a frame, as the NES's: the lead
from 12 down to 9, the harmony from 11 to 8, the bass from 13 to 11; the
final note fades after 45 % of its length, a step every 4 frames.

__The drums borrow the bass pair__ (POKEY has no fifth channel): each hit
turns it to noise (the 17-bit and 5-bit polynomials, AUDC distortion 0)
at the hit's pitch for a few frames (kick 3, snare 4, hat 1, crash 6, a
roll's hit 1 or 2), its volume falling 3 a frame, then the bass comes
back; the crash is a short burst.

The stream holds, per frame, the registers that change, as offsets from
$D200 ("Rota.Sound"'s encoding).
-}
module Rota.Sound.A800
  ( a800SoundFps
  , a800Stream
  , audf64
  , audf64Hz
  , bassN
  , bassNHz
  , pokeyClock
  ) where

import Data.Bits (shiftR, (.&.), (.|.))
import qualified Data.Map.Strict as M
import Data.Word (Word8)

import Rota.Score
import Rota.Sound

pokeyClock :: Double
pokeyClock = 1789773

-- | The NTSC 800XL's frame rate (as "Rota.A800").
a800SoundFps :: Double
a800SoundFps = 1789772.5 / 29868

-- | A pure tone's AUDF at the 64 kHz clock, and what it sounds.
audf64 :: Double -> Int
audf64 f = max 0 (min 255 (jsRound (pokeyClock / 28 / (2 * f) - 1)))

audf64Hz :: Int -> Double
audf64Hz n = pokeyClock / 28 / (2 * fromIntegral (n + 1))

-- | The joined pair's 16-bit N at 1.79 MHz, and what it sounds.
bassN :: Double -> Int
bassN f = jsRound (pokeyClock / (2 * f) - 7)

bassNHz :: Int -> Double
bassNHz n = pokeyClock / (2 * fromIntegral (n + 7))

-- | A voice's (accent, sustain) volumes.
levels :: Voice -> (Int, Int)
levels Lead = (12, 9)
levels Harm = (11, 8)
levels Bass = (13, 11)

-- | A note's software envelope, frame by frame from its first frame.
envelope :: Voice -> NoteSpan -> [Int]
envelope v s = take len (expand (go 0 acc))
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
       | final && fromIntegral j / a800SoundFps > 0.45 * durSec = max 0 (x - 1)
       | otherwise = x
  step x = if final && x <= sus then 4 else 1
  expand ((j, x) : rest@((j', _) : _)) = replicate (j' - j) x <> expand rest
  expand [(_, x)] = repeat x
  expand [] = []

-- | A voice in a frame: its pitch register value and volume, or Nothing.
voiceAt :: Voice -> Int -> Maybe (Int, Int)
voiceAt v k = do
  s <- spanAt (noteSpans a800SoundFps v) k
  let hz = bentHz v (spIndex s) (fromIntegral (k - spFrom s) / a800SoundFps)
      reg = if v == Bass then bassN hz else audf64 hz
  pure (reg, envelope v s !! (k - spFrom s))

-- | The drum on the bass pair in a frame: its noise N and volume.
drumAt :: Int -> Maybe (Int, Int)
drumAt k = case [ (h, f, n) | (h, f, n) <- hitFrames, f <= k, k < f + n ] of
  ((h, f, _) : _) -> Just (bassN (noiseHz (hDrum h)), max 0 (hVol h - 3 * (k - f)))
  [] -> Nothing
 where
  hitFrames =
    [ (h, f, min frames (f' - f))
    | (h, h') <- zip drums (drop 1 drums <> [Hit 1e9 Kick 0])
    , let f = frameOfBeat a800SoundFps (hBeat h)
          f' = if hBeat h' > 1e8 then maxBound else frameOfBeat a800SoundFps (hBeat h')
          gapSec = (hBeat h' - hBeat h) * beatSec
          frames = case hDrum h of
            Kick -> 3
            Snare -> 4
            Hat -> 1
            Crash -> 6
            Roll _ -> if gapSec < 0.06 then 1 else 2 ]

-- | The page's noise pitches (Hz).
noiseHz :: Drum -> Double
noiseHz d = case d of
  Kick -> 1800
  Snare -> 7000
  Hat -> 30000
  Roll r -> 6000 * 1.3 ^ r
  Crash -> 16000

-- | The registers (offsets from $D200) a frame wants.
regsAt :: Int -> M.Map Int Word8
regsAt k = M.fromList (lead <> harm <> pair)
 where
  tone ch v = case voiceAt v k of
    Just (n, vol) -> [ (2 * ch - 2, fromIntegral n), (2 * ch - 1, 0xA0 .|. fromIntegral vol) ]
    Nothing -> [ (2 * ch - 1, 0xA0) ]
  lead = tone 3 Lead
  harm = tone 4 Harm
  pair = case drumAt k of
    Just (n, vol) -> [ (0, lo n), (2, hi n), (3, fromIntegral vol) ]
    Nothing -> case voiceAt Bass k of
      Just (n, vol) -> [ (0, lo n), (2, hi n), (3, 0xA0 .|. fromIntegral vol) ]
      Nothing -> [ (3, 0xA0) ]
  lo x = fromIntegral (x .&. 0xFF)
  hi x = fromIntegral (x `shiftR` 8)

-- | The chime's register writes, frame by frame (frame 0: AUDCTL $50,
-- channel 1 silent).
a800Stream :: Stream
a800Stream = [ frameWrites k | k <- [0 .. lastFrame] ]
 where
  lastFrame = maximum [ spTo s | v <- [minBound .. maxBound], s <- noteSpans a800SoundFps v ] + 1
  state = scanl (\acc k -> M.union (regsAt k) acc) M.empty [0 .. lastFrame]
  frameWrites k =
    let before = state !! k
        now = M.union (regsAt k) before
    in (if k == 0 then [Write 0xD208 0x50, Write 0xD201 0x00] else [])
       <> [ Write (0xD200 + r) v | (r, v) <- M.toList now, M.lookup r before /= Just v ]
