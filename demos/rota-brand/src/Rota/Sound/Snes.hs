{-|
The chime on the Super Nintendo's S-DSP (M7 step 7.13; docs/brand.md,
"Sound, per machine" and "The Super Nintendo as built"): Rota's chime, the
same notes, timing and bends as every machine's ("Rota.Score",
"Rota.Sound"), voiced in the Game Boy Advance boot sound's manner (the
user, 2026-10-18), after the audition page's hypothetical Game Boy
Advance rendition (demos/rota/reference/audition.html, @renderGBA@): a
flute-like lead, the harmony as two string layers 7 cents either side, a
plucked bass, the engine's short reverb. Our own voicing: nothing of
Nintendo's sound is copied or sampled. Legato as on every machine
(docs/brand.md): a voice keys on at a phrase's first note only, later
notes change its pitch in place.

__Voices.__ 0 the lead, 1 and 2 the harmony's layers (7 cents flat and
sharp, panned apart), 3 the bass, 4 the drums' noise, 5 the kick (a sine
swept down).
__Samples__ ('brrSamples', at ARAM 0310h on, the directory at 0300h):
single cycles of 32 samples, looped, so a pitch of 1000h plays 1000 Hz
('pitchReg'): 0 the flute (harmonics 1, 2, 3 at 0.8, 0.22, 0.08), 1 the
string (15 harmonics at 1/k), 2 the bass (10 at 1/k^1.4), 3 a sine. BRR
filter 0, the shift fitted per block.
__Envelopes:__ the tonal voices' in hardware (ADSR, the page's attack and
sustain: the lead 24 ms to 7/8, the strings 40 ms to 7/8, the bass at once
to 1/2), the final note's fading after 45 % of it (exponential GAIN,
written before ADSR1 hands the envelope to it: the other way round, GAIN's
old value 0 would zero the envelope at once); the
drums' per frame (direct GAIN),
from the Game Gear's drum curves ("Rota.Sound.Gg"'s spec: a level 15 - the
hit's volume, rising a step every few frames), the noise's rate in FLG.
__Echo:__ the page's reverb on the tonal voices: 64 ms (EDL 4, the buffer
at 8000h), feedback 2Bh (0.34), a plain FIR (7Fh, then 0s).

The stream ('snesStream'): the DSP's register writes, per ROM frame, each
a (register, value) pair the 65816 hands to the SPC700's driver through
the ports (demos/snes/rota-spc.s); frame 0 sets everything up (the lead-in
is silent). KOFF is a level (a voice stays off while its bit is set), so
the stream writes it whenever its mask changes.
-}
module Rota.Sound.Snes
  ( snesFps
  , pitchReg
  , snesStream
  , snesSoundBytes
    -- * Samples
  , brrSamples
  , brrBlocks
  , dirBase
  , sampleBase
  ) where

import Data.Bits (complement, shiftL, shiftR, (.&.), (.|.))
import Data.Word (Word8)

import Rota.Score
import Rota.Sound

-- | 262 lines of 1364 master clocks (21.477 MHz) a frame.
snesFps :: Double
snesFps = 21477272 / 357368

-- | A frequency as the DSP's 14-bit pitch, for a 32-sample cycle (1000h
-- plays it at 32 kHz / 32 = 1000 Hz).
pitchReg :: Double -> Int
pitchReg hz = max 1 (min 0x3FFF (jsRound (hz * 4096 / 1000)))

-- * Samples

-- | The directory's ARAM address (DIR = its page) and the samples'.
dirBase, sampleBase :: Int
dirBase = 0x0300
sampleBase = 0x0310

-- | The samples' 32 values each (16-bit), one cycle.
cycles :: [[Double]]
cycles =
  [ wave [(1, 0.8), (2, 0.22), (3, 0.08)]                              -- the flute
  , wave [ (fromIntegral k, 0.55 / fromIntegral k) | k <- [1 .. 15 :: Int] ]   -- the string
  , wave [ (fromIntegral k, 0.7 / fromIntegral k ** 1.4) | k <- [1 .. 10 :: Int] ] -- the bass
  , wave [(1, 1)] ]                                                    -- the kick's sine
 where
  wave hs =
    let raw = [ sum [ a * sin (2 * pi * h * fromIntegral i / 32) | (h, a) <- hs ] | i <- [0 .. 31 :: Int] ]
        peak = maximum (map abs raw)
    in map (\x -> x / peak * 0.9) raw

-- | A cycle as BRR: two blocks of 16 samples, filter 0, the second ending
-- and looping (to the sample's start).
brrBlocks :: [Double] -> [Word8]
brrBlocks xs = block False (take 16 xs) <> block True (drop 16 xs)
 where
  block final ys =
    let -- 15-bit values (the decoder's (n << shift) >> 1)
        vs = map (\y -> jsRound (y * 16383)) ys
        need s = all (\v -> let n = jsRound (fromIntegral v / fromIntegral ((2 :: Int) ^ s) * 2) in n >= -8 && n <= 7) vs
        shift = case [ s | s <- [0 .. 12 :: Int], need s ] of
          s : _ -> s
          [] -> 12
        nib v = let n = max (-8) (min 7 (jsRound (fromIntegral v * 2 / fromIntegral ((2 :: Int) ^ shift)))) in fromIntegral (n .&. 0xF) :: Word8
        header = fromIntegral (shift `shiftL` 4) .|. (if final then 0x03 else 0x00) :: Word8
        ns = map nib vs
        pairs = [ (a `shiftL` 4) .|. b | (a, b) <- pairsOf ns ]
    in header : pairs
  pairsOf (a : b : r) = (a, b) : pairsOf r
  pairsOf _ = []

-- | Every sample's BRR bytes, in order from 'sampleBase', and the
-- directory's entries (start, loop: each sample loops to its start).
brrSamples :: ([Word8], [(Int, Int)])
brrSamples = (concat blocks, [ (a, a) | a <- starts ])
 where
  blocks = map brrBlocks cycles
  starts = scanl (+) sampleBase (map length (init blocks))

-- * Voices

leadV, harmV, harm2V, bassV, noiseV, kickV :: Int
leadV = 0
harmV = 1
harm2V = 2
bassV = 3
noiseV = 4
kickV = 5

voiceOf :: Voice -> Int
voiceOf Lead = leadV
voiceOf Harm = harmV
voiceOf Bass = bassV

-- | A voice's register.
vreg :: Int -> Int -> Int
vreg v r = v * 0x10 + r

-- | The writes that set everything up (ROM frame 0).
setUp :: [Write]
setUp =
  [ w 0x5C 0x00, w 0x4C 0x00                       -- KOFF, KON clear
  , w 0x5D (dirBase `shiftR` 8)                     -- DIR
  , w 0x0C 0x40, w 0x1C 0x40                        -- master volume (60h clipped)
  , w 0x6D 0x80, w 0x7D 0x04                        -- echo at 8000h, 64 ms
  , w 0x0D 0x2B, w 0x2C 0x24, w 0x3C 0x24           -- feedback, echo volume
  , w 0x4D (mask [leadV, harmV, harm2V, bassV])
  , w 0x2D 0x00, w 0x3D (1 `shiftL` noiseV)         -- no PMON, the noise voice
  ]
  <> [ w (0x0F + 0x10 * i) (if i == 0 then 0x7F else 0) | i <- [0 .. 7] ]
  <> concat
       [ voice leadV 0 leadAdsr (0x48, 0x48)
       , voice harmV 1 harmAdsr (0x1C, 0x10)
       , voice harm2V 1 harmAdsr (0x10, 0x1C)
       , voice bassV 2 bassAdsr (0x50, 0x50)
       , gainVoice noiseV 3 (0x40, 0x40)
       , gainVoice kickV 3 (0x60, 0x60) ]
  <> [ w 0x6C 0x00 ]                                -- FLG: on, echo writes on
 where
  w :: Int -> Int -> Write
  w r v = Write r (fromIntegral v)
  voice v src (a1, a2) (l, r) =
    [ w (vreg v 4) src, w (vreg v 5) a1, w (vreg v 6) a2, w (vreg v 0) l, w (vreg v 1) r ]
  gainVoice v src (l, r) =
    [ w (vreg v 4) src, w (vreg v 5) 0x00, w (vreg v 7) 0x00, w (vreg v 0) l, w (vreg v 1) r ]

-- | ADSR1 and ADSR2 (1, the decay rate, the attack rate; the sustain
-- level, the sustain rate: 0, held): the page's attack and sustain.
leadAdsr, harmAdsr, bassAdsr :: (Int, Int)
leadAdsr = (0x80 .|. 0x40 .|. 0x0B, 0xC0)   -- 24 ms, decay 180 ms to 7/8
harmAdsr = (0x80 .|. 0x40 .|. 0x0A, 0xC0)   -- 40 ms, to 7/8
bassAdsr = (0x80 .|. 0x40 .|. 0x0F, 0x60)   -- at once, to 1/2

-- | A voice set as a register mask.
mask :: [Int] -> Int
mask = foldl' (\m v -> m .|. (1 `shiftL` v)) 0

-- | GAIN's exponential decrease (101xxxxx, rate 10h: about 1.2 s), the
-- final note's fade.
fadeGain :: Int
fadeGain = 0xA0 .|. 0x10

-- | A tonal voice in a frame: its pitch, whether it keys on there (a
-- phrase's first note, legato after that), and whether its fade starts
-- there (45 % into the final note).
toneAt :: Voice -> Int -> Maybe (Double, Bool, Bool)
toneAt v k = do
  s <- spanAt sps k
  let hz = bentHz v (spIndex s) (fromIntegral (k - spFrom s) / snesFps)
      sounding = maybe False (\p -> spTo p == spFrom s) (spanAt sps (k - 1))
      keyOn = k == spFrom s && (spPhraseStart s || not sounding)
      fade = abs (nEnd (spNote s) - endBeat) < 1e-6 && k == spFrom s + (45 * (spTo s - spFrom s)) `div` 100
  pure (hz, keyOn, fade)
 where
  sps = noteSpans snesFps v

-- | The drum in a frame: its noise rate (FLG's low 5 bits, or Nothing for
-- the kick), its level 0-15, and whether it starts there.
drumAt :: Int -> Maybe (Maybe Int, Int, Bool)
drumAt k = case [ (h, f) | (h, f, f') <- hitFrames, f <= k, k < f' ] of
  ((h, f) : _) ->
    let (rate, step) = spec (hDrum h)
        level = hVol h - (k - f) `div` step
    in if level <= 0 then Nothing else Just (rate, level, k == f)
  [] -> Nothing
 where
  hitFrames =
    [ (h, frameOfBeat snesFps (hBeat h), maybe maxBound (frameOfBeat snesFps . hBeat) next)
    | (h, next) <- zip drums (map Just (drop 1 drums) <> [Nothing]) ]
  spec = drumSpec

-- | A drum's noise rate (Nothing: the kick's voice) and its frames a step.
drumSpec :: Drum -> (Maybe Int, Int)
drumSpec d = case d of
  Kick -> (Nothing, 1)
  Snare -> (Just 0x1A, 2)
  Hat -> (Just 0x1E, 1)
  Roll r -> (Just ([0x16, 0x18, 0x1A, 0x1B, 0x1C, 0x1D] !! r), 1)
  Crash -> (Just 0x1D, 5)

-- | The kick's sweep: frames since its hit, its pitch (150 Hz down to 45).
kickHz :: Int -> Double
kickHz j = 45 + 105 * (0.6 ** fromIntegral j)

-- | A frame's writes (k > 0) before its keys, and the voices whose notes
-- start and end there.
frameParts :: Int -> ([Write], Int, Int)
frameParts k = (pitches <> drumWrites, mask (starts <> drumStart), mask (ends <> drumEnd))
 where
  w :: Int -> Int -> Write
  w r v = Write r (fromIntegral v)
  pw v hz = let p = pitchReg hz in [w (vreg v 2) (p .&. 0xFF), w (vreg v 3) (p `shiftR` 8)]
  tones = [ (v, toneAt v k, toneAt v (k - 1)) | v <- [Lead, Harm, Bass] ]
  -- the harmony's layers, 7 cents either side
  layers v hz = if v == Harm then [(harmV, hz * 0.99596), (harm2V, hz * 1.00405)] else [(voiceOf v, hz)]
  layerVs v = map fst (layers v (1 :: Double))
  -- a pitch written when it changes or the voice keys on; the fade: the
  -- envelope handed to GAIN, an exponential fall
  pitches = concat
    [ (if changed then concat [ pw x f | (x, f) <- layers v hz ] else [])
      <> (if fade then concat [ [w (vreg x 7) fadeGain, w (vreg x 5) 0x00] | x <- layerVs v ] else [])
    | (v, Just (hz, start, fade), before) <- tones
    , let regs x = [ pitchReg f | (_, f) <- layers v x ]
          changed = start || fmap (\(b, _, _) -> regs b) before /= Just (regs hz) ]
  starts = concat [ layerVs v | (v, Just (_, True, _), _) <- tones ]
  ends = concat [ layerVs v | (v, Nothing, Just _) <- tones ]
  -- the drums: the noise's rate and level, the kick's sweep and level
  (drumWrites, drumStart, drumEnd) = case (drumAt k, drumAt (k - 1)) of
    (Just (Just rate, level, start), _) ->
      ( [ w 0x6C rate | start ] <> [w (vreg noiseV 7) (level * 8)]
      , [ noiseV | start ], [] )
    (Just (Nothing, level, start), _) ->
      ( pw kickV (kickHz (kickAge k)) <> [w (vreg kickV 7) (level * 8)]
      , [ kickV | start ], [] )
    (Nothing, Just (r, _, _)) -> ([], [], [maybe kickV (const noiseV) r])
    (Nothing, Nothing) -> ([], [], [])
  kickAge j = length (takeWhile (\i -> case drumAt i of Just (Nothing, _, False) -> True; _ -> False) [j - 1, j - 2 .. 0])

-- | The chime's writes, frame by frame. The KOFF mask is state: voices
-- whose notes ended join it, voices whose notes start leave it (a voice
-- with its KOFF bit set won't sound), written before the frame's KON
-- whenever it changes. It runs until the notes and the drums are done
-- (the final crash outlasts the notes: cut there, its noise held), then
-- keys every voice off.
snesStream :: Stream
snesStream = setUp : go 0 [1 .. lastFrame] <> [[Write 0x5C 0x3F]]
 where
  lastFrame = maximum
    ( [ spTo s | v <- [minBound .. maxBound], s <- noteSpans snesFps v ]
      <> [ frameOfBeat snesFps (hBeat h) + hVol h * snd (drumSpec (hDrum h)) | h <- drums ] ) + 1
  go _ [] = []
  go koff (k : ks) =
    let (ws, kon, ended) = frameParts k
        koff' = (koff .|. ended) .&. complement kon
        keys = [ Write 0x5C (fromIntegral koff') | koff' /= koff ] <> [ Write 0x4C (fromIntegral kon) | kon /= 0 ]
    in (ws <> keys) : go koff' ks

-- | The stream for the ROM: per frame a count of writes, then each
-- (register, value); 0xFF ends it.
snesSoundBytes :: [Word8]
snesSoundBytes = concatMap frame snesStream <> [0xFF]
 where
  frame ws
    | length ws >= 0xFF = error "snesSoundBytes: too many writes in a frame"
    | otherwise = fromIntegral (length ws) : concat [ [fromIntegral (wReg x), wVal x] | x <- ws ]
