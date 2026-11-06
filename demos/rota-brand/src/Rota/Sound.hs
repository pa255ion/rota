{-|
What every machine's sound shares (docs/brand.md, "Sound, per machine"):
the frame grid, the melodic voices' bends (the audition page's
@pitchTrack@: a glide into each legato note, a scoop into a phrase's
first note, vibrato on held notes), and the register-write streams.

__Frames.__ Frame 0 is the first vertical blank after the demo ROM's
start; the chime's first beat is 'leadInSec' later. Every event (a note,
a hit) starts at the frame nearest its time ('frameOfBeat'). Within a
note, its bends are sampled on the frame grid counted from the note's
first frame, as the page's sound driver does.

__Streams.__ One list of register writes per frame, oldest frame first;
frames without changes are empty. 'encodeStream' packs a stream for a
ROM: per frame a count byte, then that many (register, value) pairs; a
count of 0xFF ends the stream.
-}
module Rota.Sound
  ( -- * Frames
    frameOfBeat
  , NoteSpan (..)
  , noteSpans
  , spanAt
    -- * Bends
  , bentHz
    -- * Streams
  , Write (..)
  , Stream
  , encodeStream
  , streamEnd
  ) where

import Data.List (find)
import Data.Word (Word8)

import Rota.Score

-- | The frame nearest a beat, at a machine's frame rate (Hz).
frameOfBeat :: Double -> Double -> Int
frameOfBeat fps b = jsRound ((leadInSec + b * beatSec) * fps)

-- | A note on the frame grid: frames 'spFrom' to 'spTo' - 1.
data NoteSpan = NoteSpan
  { spIndex :: Int        -- ^ the note's index in its voice
  , spNote :: Note
  , spFrom :: Int
  , spTo :: Int
  , spPhraseStart :: Bool -- ^ the first note of a phrase
  , spPhraseEnd :: Bool   -- ^ the last note of a phrase
  } deriving stock (Eq, Show)

noteSpans :: Double -> Voice -> [NoteSpan]
noteSpans fps v = concat
  [ [ NoteSpan i n (frameOfBeat fps (nStart n)) (frameOfBeat fps (nEnd n)) (j == 0) (j == length p - 1)
    | (j, (i, n)) <- zip [0 :: Int ..] p ]
  | p <- groupIdx ]
 where
  ns = notes v
  -- phrases, keeping each note's index
  groupIdx = go (zip [0 ..] ns) (map length (phrases ns))
  go xs (l : ls) = take l xs : go (drop l xs) ls
  go _ [] = []

-- | The note sounding in a frame.
spanAt :: [NoteSpan] -> Int -> Maybe NoteSpan
spanAt sps k = find (\s -> spFrom s <= k && k < spTo s) sps

-- | A voice's frequency @tau@ seconds into its note @i@, before the
-- machine rounds it to a register value (the page's @pitchTrack@ with
-- bends). The bass has no bends.
bentHz :: Voice -> Int -> Double -> Double
bentHz v i tau
  | v == Bass = f
  | otherwise = vibrato (glide f)
 where
  ns = notes v
  n = ns !! i
  f = midiHz (nMidi n)
  dur = (nEnd n - nStart n) * beatSec
  legato = i > 0 && abs (nEnd (ns !! (i - 1)) - nStart n) < 1e-6
  prev = if legato then midiHz (nMidi (ns !! (i - 1))) else f * 2 ** (-40 / 1200)
  g = if legato then min 0.04 (dur * 0.2) else 0.035
  glide hz
    | tau < g = let u = tau / g in prev * (f / prev) ** (1 - (1 - u) * (1 - u))
    | otherwise = hz
  final = abs (nEnd n - endBeat) < 1e-6
  (depth, dl) = if final then (20, 0.2) else (15, 0.11)
  vibrato hz
    | dur >= 0.3 && tau > dl =
        let w = min 1 ((tau - dl) / 0.18)
        in hz * 2 ** (depth * w * sin (2 * pi * 5.8 * (tau - dl)) / 1200)
    | otherwise = hz

-- | A write of 'wVal' to the register at address 'wReg'.
data Write = Write
  { wReg :: Int
  , wVal :: Word8
  } deriving stock (Eq, Show)

type Stream = [[Write]]

-- | Pack a stream; @reg@ turns an address into the byte the ROM's driver
-- uses (the Game Boy's low address byte, the NES's offset from $4000).
encodeStream :: (Int -> Word8) -> Stream -> [Word8]
encodeStream reg s = concatMap frame s <> [0xFF]
 where
  frame ws
    | length ws >= 0xFF = error "encodeStream: too many writes in a frame"
    | otherwise = fromIntegral (length ws) : concat [ [reg (wReg w), wVal w] | w <- ws ]

-- | The frame after a stream's last write.
streamEnd :: Stream -> Int
streamEnd s = case [ k | (k, ws) <- zip [0 ..] s, not (null ws) ] of
  [] -> 0
  ks -> last ks + 1
