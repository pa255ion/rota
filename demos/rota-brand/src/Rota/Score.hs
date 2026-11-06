{-|
The chime's score (docs/brand.md, "Score"; the audition page's @SCORE@,
@BASS_TAILS.figure@, @DRUMS@ and @MOTIFS@). Beats are counted from 0 here,
as in the page's code; docs/brand.md's tables count from 1.

D major, 150 bpm, seven chords (D, Bm, G, Em, A, D, D). Three voices in
legato phrases, one drum part on a noise channel.
-}
module Rota.Score
  ( -- * Tempo and landmarks
    bpm
  , beatSec
  , leadInSec
  , lastBeat
  , endBeat
  , rollBeat
  , growBeat
    -- * Voices
  , Voice (..)
  , Note (..)
  , notes
  , phrases
  , chords
  , Motif (..)
  , motifs
    -- * Drums
  , Drum (..)
  , Hit (..)
  , drums
  , rollHits
    -- * Pitch
  , midiHz
  , noteName
  , jsRound
  ) where

-- | Tempo: one beat is 0.4 s.
bpm :: Double
bpm = 150

beatSec :: Double
beatSec = 60 / bpm

-- | Black before the first beat, so the monitor locks on.
leadInSec :: Double
leadInSec = 1.5

-- | The crash (the page's @LAST@), the end of the held chord (@END@), the
-- roll's start (@ROLL@) and the lambdas' burst (@GROW@).
lastBeat, endBeat, rollBeat, growBeat :: Double
lastBeat = 6
endBeat = 8.6
rollBeat = 4
growBeat = 5.5

data Voice = Lead | Harm | Bass
  deriving stock (Eq, Ord, Show, Enum, Bounded)

-- | A note from beat 'nStart' to beat 'nEnd' (MIDI note number).
data Note = Note
  { nStart :: Double
  , nEnd :: Double
  , nMidi :: Int
  } deriving stock (Eq, Show)

notes :: Voice -> [Note]
notes v = [ Note a e m | (a, e, m) <- case v of
  Lead -> [(0, 0.5, 78), (0.5, 1, 81), (1, 3, 86), (3, 3.5, 83), (3.5, 4, 79), (4, 5, 76), (5, 5.5, 81), (5.5, 6, 79), (6, endBeat, 78)]
  Harm -> [(0, 1, 74), (1, 1.5, 71), (1.5, 2, 74), (2, 3, 79), (3, 4, 76), (4, 4.5, 73), (4.5, 5, 69), (5, 6, 66), (6, endBeat, 69)]
  -- the head, then the "figure again" ending (chosen 2026-10-06)
  Bass -> [(0, 1, 38), (1, 2, 35), (2, 2.5, 43), (2.5, 3, 47), (3, 4, 52), (4, 5, 45), (5, 5.5, 42), (5.5, 6, 45), (6, endBeat, 50)] ]

-- | Consecutive notes (one ends where the next starts) form one phrase:
-- one tone, its frequency changed in place, never restarted.
phrases :: [Note] -> [[Note]]
phrases = foldr step []
 where
  step n (p@(n' : _) : ps) | abs (nEnd n - nStart n') < 1e-6 = (n : p) : ps
  step n ps = [n] : ps

chords :: [(String, String)]
chords = [("D", "I"), ("Bm", "vi"), ("G", "IV"), ("Em", "ii"), ("A", "V"), ("D", "I"), ("D", "I")]

-- | Where the figure (minor third up, fourth up) and its inversion are
-- stated, and the roll.
data Motif = Motif
  { moVoice :: Maybe Voice -- ^ Nothing: the drums
  , moFrom :: Double
  , moTo :: Double
  , moLabel :: String
  } deriving stock (Eq, Show)

motifs :: [Motif]
motifs =
  [ Motif (Just Lead) 0 1.5 "figure", Motif (Just Harm) 1 2.5 "figure"
  , Motif (Just Bass) 2 3.5 "figure", Motif (Just Lead) 3 4.5 "inverted"
  , Motif (Just Harm) 4 5.5 "inverted", Motif (Just Bass) 5 6.5 "figure, again"
  , Motif Nothing 4 6.6 "roll, crescendo through V and I" ]

-- | Drum kinds; a roll hit carries its step (0 to 5), which raises the
-- noise pitch.
data Drum = Kick | Hat | Snare | Roll Int | Crash
  deriving stock (Eq, Show)

-- | A hit at a beat with a volume (0 to 15). A new hit cuts the last.
data Hit = Hit
  { hBeat :: Double
  , hDrum :: Drum
  , hVol :: Int
  } deriving stock (Eq, Show)

drums :: [Hit]
drums =
  [ Hit 0 Kick 12, Hit 0.5 Hat 5, Hit 1 Snare 11, Hit 1.5 Hat 5
  , Hit 2 Kick 12, Hit 2.5 Hat 5, Hit 3 Snare 11, Hit 3.5 Hat 5
  , Hit 4 (Roll 0) 3, Hit 4.25 (Roll 0) 4, Hit 4.5 (Roll 1) 5, Hit 4.75 (Roll 1) 6
  , Hit 5 (Roll 2) 7, Hit 5.125 (Roll 2) 8, Hit 5.25 (Roll 3) 9, Hit 5.375 (Roll 3) 10
  , Hit 5.5 (Roll 4) 11, Hit 5.625 (Roll 4) 12, Hit 5.75 (Roll 5) 13, Hit 5.875 (Roll 5) 14
  , Hit lastBeat Crash 15 ]

-- | The roll hits' beats (the ring's flicker follows them).
rollHits :: [Double]
rollHits = [ hBeat h | h <- drums, isRoll (hDrum h) ]
 where
  isRoll (Roll _) = True
  isRoll _ = False

-- | Equal temperament, A4 = 440 Hz.
midiHz :: Int -> Double
midiHz m = 440 * 2 ** (fromIntegral (m - 69) / 12)

noteName :: Int -> String
noteName m = names !! (m `mod` 12) <> show (m `div` 12 - 1)
 where
  names = ["C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B"]

-- | JavaScript's @Math.round@ (halves up), which the page uses for every
-- register value; Haskell's 'round' rounds halves to even.
jsRound :: Double -> Int
jsRound x = floor (x + 0.5)
