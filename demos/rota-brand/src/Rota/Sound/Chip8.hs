{-|
The chime on CHIP-8's buzzer (docs/brand.md, "Sound, per machine"; the
page's @CHIP8@): one fixed pitch, so legato can't exist; the lead's
rhythm survives as beeps. Each lead note sets the sound timer at the
60 Hz frame nearest its start: 3 to 12 frames (its length in seconds
times 24, rounded and clamped), 30 for the last note.
-}
module Rota.Sound.Chip8
  ( chip8Fps
  , chip8Beeps
  ) where

import Rota.Score

-- | CHIP-8's timers count at 60 Hz.
chip8Fps :: Double
chip8Fps = 60

-- | (frame, sound timer value): frame 0 is the program's first timer
-- tick, the first beat 'leadInSec' later.
chip8Beeps :: [(Int, Int)]
chip8Beeps =
  [ (lead + jsRound (nStart n * beatSec * 60), st)
  | (i, n) <- zip [0 :: Int ..] ns
  , let st | i == length ns - 1 = 30
           | otherwise = max 3 (min 12 (jsRound ((nEnd n - nStart n) * beatSec * 24)))
  ]
 where
  ns = notes Lead
  lead = jsRound (leadInSec * chip8Fps)
