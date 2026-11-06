{-|
The logo, 64 × 32 (docs/brand.md, "Logo" and "Animation"; the audition
page's @draw@ in the spanning layout): the ring of eight segments, the
emblem of four capital lambdas, the letters RO and TA either side, and
the three threads, one per voice.

'picture' is the page's drawing as a pure function of a 'Moment' (the
beat, and each thread's flow phase) and a machine's 'Style'. Drawing
order as on the page: the letters, then the threads (behind the letters
and earlier threads, or XOR-drawn on one-bit machines), then the ring and
the emblem on top.
-}
module Rota.Logo
  ( -- * Time
    Moment (..)
  , Phase (..)
  , phaseOf
    -- * Styles
  , Style (..)
    -- * The picture
  , width
  , height
  , picture
    -- * Pieces (for tests and the ROM generators)
  , segmentOf
  , emblemAt
  , letterPixels
  , Thread (..)
  , threads
  , threadWidth
  , discRadius
  , ringOrigin
  , centre
    -- * Scene rules
  , segColour
  , wordColour
  , growthAt
  , revealAt
  , flipAt
  , pulseAt
  , inDisc
  , band
  ) where

import Data.Fixed (mod')
import qualified Data.Map.Strict as M
import qualified Data.Vector as V

import Rota.Score

width, height :: Int
width = 64
height = 32

-- | A moment of the animation: 'moBeat' is Nothing in the lead-in, else
-- the beat (from 0, after the lead-in); 'moFlow' each thread's flow phase
-- in radians (the page's @w × t@, t in seconds).
data Moment = Moment
  { moBeat :: Maybe Double
  , moFlow :: Voice -> Double
  }

data Phase = LeadIn | Build | Full | Idle
  deriving stock (Eq, Show)

phaseOf :: Maybe Double -> Phase
phaseOf Nothing = LeadIn
phaseOf (Just b)
  | b < lastBeat = Build
  | b < endBeat = Full
  | otherwise = Idle

-- | How a machine colours the logo (the page's @STYLE@ entries). Nothing
-- leaves a pixel unlit (the background shows).
data Style c = Style
  { stOff :: Maybe c           -- ^ a segment not lit yet
  , stBuilt :: c               -- ^ a lit segment
  , stHead :: c                -- ^ the crash's flicker, the idle token, a thread's head
  , stT1 :: Maybe c            -- ^ the idle token's first trail
  , stT2 :: Maybe c            -- ^ its second trail
  , stIdle :: Maybe c          -- ^ the other segments while idle
  , stDip :: Maybe (Maybe c)   -- ^ a dipped segment; Nothing: the style has no
                               --   dip colour, the page then takes t2, else off
  , stLam :: c                 -- ^ the lambdas
  , stCore :: c                -- ^ the 2 × 2 core
  , stWord :: c                -- ^ the letters, lit
  , stWordOff :: Maybe c       -- ^ the letters before they light
  , stThread :: Voice -> c
  , stOneBit :: Bool           -- ^ CHIP-8: one bit, threads XOR-drawn, the
                               --   idle token a single lit segment
  }

-- | The emblem's square sits at x 16 to 47.
ringOrigin :: Int
ringOrigin = 16

centre :: Double
centre = 15.5

-- | The ring segment (0 at the top, clockwise) of a pixel in the 32 × 32
-- square, if any: radius 12.2 to 15.6, 45° segments with gaps of about 8°.
segmentOf :: Int -> Int -> Maybe Int
segmentOf x y
  | r >= 12.2 && r <= 15.6 && a2 > 4 && a2 < 41 = Just (floor (((a + 22.5) `mod'` 360) / 45))
  | otherwise = Nothing
 where
  dx = fromIntegral x - centre
  dy = fromIntegral y - centre
  r = sqrt (dx * dx + dy * dy)
  a = (atan2 dx (-dy) * 180 / pi + 360) `mod'` 360
  a2 = (a + 22.5) `mod'` 45

-- | The emblem at growth p (0 to 1, rounded to fortieths as on the page):
-- 1 = a lambda, 2 = the core, 0 = nothing; indexed y * 32 + x.
emblemAt :: Double -> V.Vector Int
emblemAt p = V.generate 1024 px
 where
  q = fromIntegral (jsRound (p * 40)) / 40 :: Double
  apex = 3.6
  reach = 10.6
  spread = 4.39
  px i
    | max (abs dx) (abs dy) < 1 = 2
    | q <= 0 = 0
    | any leg [0 .. 3 :: Int] = 1
    | otherwise = 0
   where
    (y, x) = i `divMod` 32
    dx = fromIntegral x - centre
    dy = fromIntegral y - centre
    leg k =
      let ang = fromIntegral k * pi / 2
          ca = cos ang
          sa = sin ang
          rx = dx * ca + dy * sa
          ry = -dx * sa + dy * ca
          ey = -(apex + (reach - apex) * q)
          ex = spread * q
      in dseg rx ry 0 (-apex) (-ex) ey < 0.95 || dseg rx ry 0 (-apex) ex ey < 0.95

dseg :: Double -> Double -> Double -> Double -> Double -> Double -> Double
dseg px py ax ay bx by = sqrt (ex * ex + ey * ey)
 where
  vx = bx - ax
  vy = by - ay
  t = max 0 (min 1 (((px - ax) * vx + (py - ay) * vy) / (vx * vx + vy * vy)))
  ex = px - ax - t * vx
  ey = py - ay - t * vy

-- | The letters' pixels, bold 6 × 11 at rows 10 to 20: R at x 1, O at 9,
-- T at 49, A at 57.
letterPixels :: [(Int, Int)]
letterPixels =
  [ (x0 + x, 10 + y)
  | (glyph, x0) <- [(r, 1), (o, 9), (t, 49), (a, 57)]
  , (y, row) <- zip [0 ..] glyph
  , (x, c) <- zip [0 ..] row
  , c == '1' ]
 where
  r = ["11111.", "11..11", "11..11", "11..11", "11111.", "1111..", "11.11.", "11..11", "11..11", "11..11", "11..11"]
  o = [".1111.", "11..11", "11..11", "11..11", "11..11", "11..11", "11..11", "11..11", "11..11", "11..11", ".1111."]
  t = ["111111", "111111", "..11..", "..11..", "..11..", "..11..", "..11..", "..11..", "..11..", "..11..", "..11.."]
  a = [".1111.", "11..11", "11..11", "11..11", "11..11", "111111", "11..11", "11..11", "11..11", "11..11", "11..11"]

-- | A thread: y = cy + flip A sin (2 pi x / L + phi + flow).
data Thread = Thread
  { thVoice :: Voice
  , thA :: Double
  , thL :: Double
  , thPhi :: Double
  , thOmega :: Double         -- ^ rad/s (the page's @w@)
  , thCy :: Double
  , thStart :: Double         -- ^ the beat it starts spinning out
  , thInv :: Maybe Double     -- ^ the beat it flips over (its inversion)
  }

threads :: [Thread]
threads =
  [ Thread Lead 7.5 70 0.0 0.55 15.5 0 (Just 3)
  , Thread Harm 10 96 2.2 (-0.38) 15.5 1 (Just 4)
  , Thread Bass 5 52 4.1 0.8 16 2 Nothing ]

threadWidth :: Int
threadWidth = 5

-- | Threads are hidden inside this radius around the emblem's centre.
discRadius :: Double
discRadius = 16.4

-- | Beats a thread takes to spin out across the screen.
travel :: Double
travel = 1.5

-- * The scene rules (shared by 'picture' and the ROMs' model, "Rota.Runtime")

-- | A ring segment's colour (Nothing: unlit, the background shows).
segColour :: Style c -> Maybe Double -> Int -> Maybe c
segColour sty mb k = case phaseOf mb of
  LeadIn -> stOff sty
  Build
    | stage > floor b -> stOff sty
    | b >= rollBeat && b < growBeat, Just h <- lastHit, k `mod` 2 == h `mod` 2, b - hitAt h < 0.6 * hitLen h -> dipB
    | otherwise -> Just (stBuilt sty)
  Full
    | f < 0.5 -> if even (floor (f / 0.125) :: Int) then Just (stHead sty) else dipF
    | otherwise -> Just (stBuilt sty)
  Idle
    | stOneBit sty -> if k == tok then Just (stBuilt sty) else Nothing
    | otherwise -> case (tok - k + 8) `mod` 8 of
        0 -> Just (stHead sty)
        1 -> stT1 sty
        2 -> stT2 sty
        _ -> stIdle sty
 where
  b = maybe 0 id mb
  f = b - lastBeat
  tok = (floor ((b - endBeat) * 2) :: Int) `mod` 8
  stage = min k (8 - k)
  dipB = case stDip sty of
    Just d -> d
    Nothing -> maybe (stOff sty) Just (stT2 sty)
  dipF = case stDip sty of
    Just d -> d
    Nothing -> stOff sty
  lastHit = case [ j | (j, t) <- zip [0 :: Int ..] rollHits, t <= b ] of
    [] -> Nothing
    js -> Just (last js)
  hitAt j = rollHits !! j
  hitLen j = (if j + 1 < length rollHits then rollHits !! (j + 1) else growBeat) - hitAt j

-- | The letters' colour.
wordColour :: Style c -> Maybe Double -> Maybe c
wordColour sty mb
  | ph == Idle || (ph == Full && b >= lastBeat + 0.4) = Just (stWord sty)
  | otherwise = stWordOff sty
 where
  ph = phaseOf mb
  b = maybe 0 id mb

-- | The emblem's growth (Nothing in the lead-in: no emblem, no core).
growthAt :: Maybe Double -> Maybe Double
growthAt mb = case phaseOf mb of
  LeadIn -> Nothing
  Build -> let u = max 0 (min 1 ((maybe 0 id mb - growBeat) / (lastBeat - growBeat))) in Just (1 - (1 - u) * (1 - u))
  _ -> Just 1

-- | How many columns of a thread are out (none in the lead-in).
revealAt :: Maybe Double -> Thread -> Int
revealAt mb th = case phaseOf mb of
  LeadIn -> 0
  Build -> jsRound (fromIntegral width * max 0 (min 1 ((maybe 0 id mb - thStart th) / travel)))
  _ -> width

-- | A thread's flip, 1 to -1 (its inversion).
flipAt :: Maybe Double -> Thread -> Double
flipAt mb th = case thInv th of
  Nothing -> 1
  Just inv -> cos (pi * max 0 (min 1 ((maybe 0 id mb - inv) / 0.5)))

-- | The bass's pulse (the figure again): its leading column.
pulseAt :: Maybe Double -> Maybe Int
pulseAt mb = case (phaseOf mb, mb) of
  (Build, Just b) | b >= 5 -> Just (floor (64 * (b - 5) / (lastBeat - 5)))
  _ -> Nothing

-- | Inside the emblem's disc (threads hidden).
inDisc :: Int -> Int -> Bool
inDisc x y =
  let dx = fromIntegral x - (fromIntegral ringOrigin + centre)
      dy = fromIntegral y - centre
  in sqrt (dx * dx + dy * dy) < discRadius

-- | A thread column's rows: (lo, hi) from the rounded centre rows of the
-- column and the next (the page's drawing: 5 pixels thick, joined).
band :: Int -> Int -> (Int, Int)
band y0 y1 = (lo, max lo (max y0 y1 - 1 - half) + threadWidth - 1)
 where
  half = (threadWidth - 1) `div` 2
  lo = min y0 y1 - half

-- | The picture at a moment, row-major, 64 × 32.
picture :: Style c -> Moment -> V.Vector (Maybe c)
picture sty mo = V.generate (width * height) pixel
 where
  mb = moBeat mo
  ph = phaseOf mb
  one = stOneBit sty
  -- letters, then threads: a map of the lower layers (one-bit: parity)
  letters = M.fromList [ ((x, y), (c, 1 :: Int)) | Just c <- [wordColour sty mb], (x, y) <- letterPixels ]
  lower = foldl' drawThread letters (if ph == LeadIn then [] else threads)
  drawThread acc th
    | reveal <= 0 = acc
    | otherwise = foldl' put acc
        [ ((x, y), c)
        | x <- [0 .. reveal - 1]
        , let (lo, hi) = band (jsRound (yAt (fromIntegral x))) (jsRound (yAt (fromIntegral x + 1)))
              c | reveal < width && x >= reveal - 2 = stHead sty
                | thVoice th == Bass, Just hx <- pulseAt mb, x >= hx - 3 && x <= hx = stHead sty
                | otherwise = stThread sty (thVoice th)
        , y <- [lo .. hi]
        , y >= 0, y < height
        , not (inDisc x y) ]
   where
    reveal = revealAt mb th
    yAt x = thCy th + flipAt mb th * thA th * sin (2 * pi * x / thL th + thPhi th + moFlow mo (thVoice th))
    put m (k, c)
      | one = M.insertWith (\_ (c0, n) -> (c0, n + 1)) k (c, 1) m
      | M.member k m = m
      | otherwise = M.insert k (c, 1) m
  -- ring and emblem on top
  em = emblemAt <$> growthAt mb
  top x y
    | x < ringOrigin || x >= ringOrigin + 32 = Nothing
    | otherwise =
        let lx = x - ringOrigin
        in case segmentOf lx y of
             Just k -> Just (segColour sty mb k)
             Nothing -> case em of
               Just e | e V.! (y * 32 + lx) == 1 -> Just (Just (stLam sty))
                      | e V.! (y * 32 + lx) == 2 -> Just (Just (stCore sty))
               _ -> Nothing
  pixel i =
    let (y, x) = i `divMod` width
        below = M.lookup (x, y) lower
    in case top x y of
         Just (Just c) -> Just (if one then stLam sty else c)
         _ | one -> case below of
               Just (_, n) | odd n -> Just (stLam sty)
               _ -> Nothing
           | otherwise -> fst <$> below
