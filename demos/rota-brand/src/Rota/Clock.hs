{-|
A tile machine's frames on the tile lab's timeline (PLAN.md, side track
J; "Rota.Lab"). ROM frame 0 is the first vertical blank after the ROM
starts, as for the sound streams ("Rota.Sound"); the first beat is at
'frameOfBeat' 0 (the lead-in before it is dark). From there a picture
frame @p@ shows beat @p / (fps × beat length)@, until the idle loop
starts at the first frame at or after beat 'endBeat'.

__Idle__ runs on whole frames, so it can loop: the lab's idle at 60
frames a second exactly (a beat is 24 frames), each part with its own
period ('idlePeriods'): the ring's wave 72 frames, the sparks 96, the
junction lights 120, the fabric 64 (a step every 4 frames, 16 steps).
-}
module Rota.Clock
  ( Clock (..)
  , clock
  , PicTime (..)
  , picTime
  , romFrames
  , idlePeriods
  , weaveCapped
  ) where

import qualified Data.Vector.Unboxed as U

import Rota.Lab (labFps)
import Rota.Score
import Rota.Sound (frameOfBeat)

data Clock = Clock
  { ckFps :: Double
  , ckLead :: Int  -- ^ the ROM frame of the first beat
  , ckIdle :: Int  -- ^ the first idle picture frame
  }

clock :: Double -> Clock
clock fps = Clock fps (frameOfBeat fps 0) (ceiling (endBeat * fps * beatSec - 1e-9))

-- | What a picture frame shows: its beat and the fabric's step; idle
-- frames also carry their number since the idle start.
data PicTime = PicTime
  { ptBeat :: Double
  , ptWeave :: Int
  , ptIdle :: Maybe Int
  } deriving stock (Eq, Show)

picTime :: Clock -> Int -> PicTime
picTime ck p
  | p < ckIdle ck =
      let b = fromIntegral p / (ckFps ck * beatSec)
      in PicTime b (p `div` 4 + burst b) Nothing
  | otherwise =
      let q = p - ckIdle ck
      in PicTime (endBeat + fromIntegral q / (labFps * beatSec)) (p `div` 4 + 8) (Just q)
 where
  burst b = if b >= lastBeat then min 8 (floor ((b - lastBeat) / 0.06)) else 0

-- | ROM frames from the start to the idle start.
romFrames :: Clock -> Int
romFrames ck = ckLead ck + ckIdle ck

-- | The idle loop's parts and their periods in frames: (ring wave,
-- sparks, junction lights, fabric).
idlePeriods :: (Int, Int, Int, Int)
idlePeriods = (72, 96, 120, 64)

-- | The fabric's step with at most one step a frame (the crash's burst
-- and the regular step can fall in one frame; the second waits a frame).
-- It catches up long before the idle loop (checked by the tests).
weaveCapped :: Clock -> Int -> Int
weaveCapped ck p
  | p < 0 = 0
  | p < U.length capped = capped U.! p
  | otherwise = ptWeave (picTime ck p)
 where
  capped = U.fromList (scanl1 (\a b -> min b (a + 1)) [ ptWeave (picTime ck q) | q <- [0 .. ckIdle ck] ])
