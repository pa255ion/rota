{-|
Rendition B, the picture for tile machines (docs/brand.md, "Rendition B";
PLAN.md, side track J): a port of the tile lab's script
(@demos/rota/reference/tile-lab.html@), function by function, so its
figures can be checked here ('labStats') and every machine's data is
generated from the same picture.

__Pixels__ hold colour roles, as the lab's @IDX@: 0 the background, 1 to 5
the ring's stages (stage @s@ holds segments @s - 1@ and @9 - s@), 6 the
lambdas, 7 the core, 8 the letters, 10 and 11 the fabric's bright and
dim. The sprite layer adds 27 and 28 (a junction light's centre and
corners), 30 to 35 the sparks (head, trail for lead, harmony, bass) and 36
a spark's white centre. What each role looks like at a frame is
'roleState'; each machine's colours are its own (Rota.Palette).

__Time__ is the lab's: a beat @b@ from the first note (the lead-in comes
before it), and the fabric's step from frames at 60 a second. A machine's
own clock maps onto it in its tiles module.
-}
module Rota.Lab
  ( -- * Machines
    Machine (..)
  , nesM
  , gbM
  , smsM
  , snesM
  , ggM
  , a800M
    -- * Geometry
  , Geo (..)
  , geometry
  , geometrySolid
  , traceH
  , traceV
  , isTraceCell
  , Square (..)
    -- * Roles
  , roleBg, roleEmb, roleCore, roleLetters, roleTeal, roleViolet
  , roleLightB, roleLightD, roleSparkCore
  , sparkRoles
    -- * The background
  , Shape (..)
  , weaveL
  , weavePhase
  , weaveStep
  , weaveLevel
  , embRadius
  , compose
  , Weave (..)
    -- * Sprites
  , SparkVoice (..)
  , sparksAt
  , SprPixel (..)
  , Part (..)
  , sparkPixels
  , lightStep
  , lightPixels
    -- * Colour roles over time
  , RingLook (..)
  , RoleState (..)
  , roleState
    -- * Time
  , labFps
  , labFrames
  , flickHits
  , rollAll
    -- * The lab's figures
  , LabStats (..)
  , labStats
  , tileKeys
  ) where

import Data.Bits (shiftR)
import Data.Fixed (mod')
import Data.List (nub)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Data.Word (Word8)

import Rota.Score

-- * Machines

-- | A screen and its logo: the lab's @PRESETS@ (only the geometry; the
-- budgets are the tests').
data Machine = Machine
  { mName :: String
  , mW, mH :: Int
  , mCx, mCy :: Int
  , mR :: Int       -- ^ the ring's outer radius
  , mBig :: Bool    -- ^ large letters (2 × 4 tiles), else small (1 × 3)
  } deriving stock (Eq, Show)

nesM, gbM, smsM, ggM, a800M, snesM :: Machine
nesM = Machine "NES" 256 240 128 120 64 True
gbM = Machine "Game Boy" 160 144 80 72 48 False
smsM = Machine "Master System" 256 192 128 96 64 True
ggM = Machine "Game Gear" 160 144 80 72 48 False
a800M = Machine "Atari 800XL" 320 192 160 96 64 True
-- | (M7) the Super Nintendo's 256 x 224, the Master System's ring and letters
snesM = Machine "Super Nintendo" 256 224 128 112 64 True

-- * Roles

roleBg, roleEmb, roleCore, roleLetters, roleTeal, roleViolet, roleLightB, roleLightD, roleSparkCore :: Word8
roleBg = 0
roleEmb = 6
roleCore = 7
roleLetters = 8
roleTeal = 10
roleViolet = 11
roleLightB = 27
roleLightD = 28
roleSparkCore = 36

data SparkVoice = SLead | SHarm | SBass
  deriving stock (Eq, Ord, Show, Enum, Bounded)

-- | (head, trail) roles of a voice's spark.
sparkRoles :: SparkVoice -> (Word8, Word8)
sparkRoles SLead = (30, 31)
sparkRoles SHarm = (32, 33)
sparkRoles SBass = (34, 35)

-- * Time

labFps :: Double
labFps = 60

-- | The lab's run: the chime and four beats of idle.
labFrames :: Int
labFrames = jsRound ((endBeat + 4) * beatSec * labFps)

-- | The roll hits the ring flickers on (the lab's @FLICK@), and all twelve
-- (its @ROLL@, which the junction lights count).
flickHits, rollAll :: [Double]
flickHits = [4, 4.25, 4.5, 4.75, 5, 5.125, 5.25, 5.375]
rollAll = flickHits <> [5.5, 5.625, 5.75, 5.875]

-- * Geometry

-- | The sparks' square: the first trace rows and columns clear of the ring.
data Square = Square { sqX0, sqX1, sqY0, sqY1 :: Int }
  deriving stock (Eq, Show)

data Geo = Geo
  { gM :: Machine
  , gRing :: U.Vector Word8    -- ^ the ring's stage (1 to 5), or 0
  , gEmb :: U.Vector Bool
  , gCore :: U.Vector Bool
  , gLetters :: U.Vector Bool
  , gLogo :: U.Vector Bool
  , gEmbCell :: U.Vector Double  -- ^ the distance of a pixel's tile centre from the logo's centre
  , gEmbMax :: Double
  , gColsX :: [Int]
  , gRowsY :: [Int]
  , gSquare :: Square
  , gBaseX, gBaseY :: Int
  }

-- | The distance from a point to a segment.
dseg :: Double -> Double -> Double -> Double -> Double -> Double -> Double
dseg px py ax ay bx by =
  let vx = bx - ax; vy = by - ay
      den = vx * vx + vy * vy
      t = max 0 (min 1 (((px - ax) * vx + (py - ay) * vy) / (if den == 0 then 1 else den)))
  in sqrt ((px - ax - t * vx) ^ (2 :: Int) + (py - ay - t * vy) ^ (2 :: Int))

-- | The lab's @LETTERS@.
data Glyph = Glyph { glLines :: [[(Double, Double)]], glOctagon :: [(Double, Double)], glNode :: Maybe (Int, Int, Int, Int) }

data LetterSpec = LetterSpec { lsW, lsH :: Int, lsR, lsGroove, lsGap :: Double, lsGlyph :: Char -> Glyph }

bigLetters, smallLetters :: LetterSpec
bigLetters = LetterSpec 16 32 2.1 0.55 1.3 g
 where
  g 'R' = Glyph [[(2.5, 29.5), (2.5, 2.5), (10.5, 2.5), (13.5, 5.5), (13.5, 12.5), (10.5, 15.5), (2.5, 15.5)], [(7.5, 15.5), (13.5, 21.5), (13.5, 29.5)]] [] Nothing
  g 'O' = Glyph [] [(5.5, 2.5), (10.5, 2.5), (13.5, 5.5), (13.5, 26.5), (10.5, 29.5), (5.5, 29.5), (2.5, 26.5), (2.5, 5.5)] Nothing
  g 'T' = Glyph [[(2.5, 2.5), (13.5, 2.5)], [(7.5, 2.5), (7.5, 29.5)]] [] Nothing
  g _ = Glyph [[(2.5, 29.5), (2.5, 8.5), (8, 3)], [(13.5, 29.5), (13.5, 8.5), (8, 3)]] [] (Just (6, 17, 4, 4))
smallLetters = LetterSpec 8 24 1.1 0 0 g
 where
  g 'R' = Glyph [[(1.5, 22.5), (1.5, 1.5), (4.5, 1.5), (6.5, 3.5), (6.5, 8.5), (4.5, 10.5), (1.5, 10.5)], [(3.5, 10.5), (6.5, 13.5), (6.5, 22.5)]] [] Nothing
  g 'O' = Glyph [[(3.5, 1.5), (4.5, 1.5), (6.5, 3.5), (6.5, 20.5), (4.5, 22.5), (3.5, 22.5), (1.5, 20.5), (1.5, 3.5), (3.5, 1.5)]] [] Nothing
  g 'T' = Glyph [[(1.5, 1.5), (6.5, 1.5)], [(3.5, 1.5), (3.5, 22.5)]] [] Nothing
  g _ = Glyph [[(1.5, 22.5), (1.5, 4.5), (4, 2)], [(6.5, 22.5), (6.5, 4.5), (4, 2)]] [] (Just (3, 13, 2, 2))

glyphMask :: LetterSpec -> Glyph -> U.Vector Bool
glyphMask sp g = U.generate (w * h) px
 where
  w = lsW sp
  h = lsH sp
  segs = concat [ zipWith (\(ax, ay) (bx, by) -> (ax, ay, bx, by)) pl (drop 1 pl) | pl <- glLines g ]
      <> octSegs (glOctagon g)
  octSegs [] = []
  octSegs v =
    [ (ax + ux * gp, ay + uy * gp, bx - ux * gp, by - uy * gp)
    | i <- [0 .. 7]
    , let (ax, ay) = v !! i
          (bx, by) = v !! ((i + 1) `mod` 8)
          l = sqrt ((bx - ax) ^ (2 :: Int) + (by - ay) ^ (2 :: Int))
          ux = (bx - ax) / l
          uy = (by - ay) / l
          gp = lsGap sp ]
  inNode x y = case glNode g of
    Just (nx, ny, nw, nh) -> x >= nx && x < nx + nw && y >= ny && y < ny + nh
    Nothing -> False
  px i =
    let (y, x) = i `divMod` w
        d = minimum (1e9 : [ dseg (fromIntegral x + 0.5) (fromIntegral y + 0.5) ax ay bx by | (ax, ay, bx, by) <- segs ])
        groove = lsGroove sp
    in (d <= lsR sp && not (groove > 0 && d < groove)) || inNode x y

traceOff :: Int -> Int -> Bool
traceOff o base = abs o >= base && (abs o - base) `mod` 32 == 0

geometry :: Machine -> Geo
geometry = geometryWith True

-- | The same, with the large letters' groove left out (solid strokes: the
-- 800XL's mode E, whose pixels are two wide, decided with the user).
geometrySolid :: Machine -> Geo
geometrySolid = geometryWith False

geometryWith :: Bool -> Machine -> Geo
geometryWith grooved m = Geo
  { gM = m, gRing = ring, gEmb = emb, gCore = core, gLetters = lets
  , gLogo = U.generate (w * h) (\i -> ring U.! i /= 0 || emb U.! i || core U.! i || lets U.! i)
  , gEmbCell = embCell, gEmbMax = embMax
  , gColsX = colsX, gRowsY = rowsY, gSquare = sq, gBaseX = baseX, gBaseY = baseY }
 where
  w = mW m
  h = mH m
  cx = fromIntegral (mCx m) :: Double
  cy = fromIntegral (mCy m) :: Double
  k = fromIntegral (mR m) / 15.6
  apex = 3.6 * k
  reach = 10.6 * k
  spread = 4.39 * k
  half = 0.95 * k
  coords i = let (y, x) = i `divMod` w in (x, y, fromIntegral x + 0.5 - cx, fromIntegral y + 0.5 - cy)
  ring = U.generate (w * h) $ \i ->
    let (_, _, dx, dy) = coords i
        r = sqrt (dx * dx + dy * dy)
        a = (atan2 dx (-dy) * 180 / pi + 360) `mod'` 360
        a2 = (a + 22.5) `mod'` 45
        sg = floor (((a + 22.5) `mod'` 360) / 45) :: Int
    in if r >= 12.2 * k && r <= 15.6 * k && a2 > 4 && a2 < 41 then fromIntegral (1 + min sg (8 - sg)) else 0
  core = U.generate (w * h) $ \i -> let (_, _, dx, dy) = coords i in max (abs dx) (abs dy) < k
  emb = U.generate (w * h) $ \i ->
    let (_, _, dx, dy) = coords i
        arm q = let ang = fromIntegral q * pi / 2
                    ca = cos ang
                    sa = sin ang
                    rx = dx * ca + dy * sa
                    ry = -dx * sa + dy * ca
                in dseg rx ry 0 (-apex) (-spread) (-reach) < half || dseg rx ry 0 (-apex) spread (-reach) < half
    in not (core U.! i) && any arm [0 :: Int .. 3]
  embCell = U.generate (w * h) $ \i ->
    let (x, y, _, _) = coords i
        ex = fromIntegral ((x `shiftR` 3) * 8 + 4) - cx
        ey = fromIntegral ((y `shiftR` 3) * 8 + 4) - cy
    in sqrt (ex * ex + ey * ey)
  embMax = maximum (0 : [ embCell U.! i | i <- [0 .. w * h - 1], emb U.! i ])
  spec = (if mBig m then bigLetters else smallLetters) { lsGroove = if grooved then lsGroove spec0 else 0 }
  spec0 = if mBig m then bigLetters else smallLetters
  lw = lsW spec
  lh = lsH spec
  ringL = floor ((cx - fromIntegral (mR m)) / 8) * 8 :: Int
  ringR = ceiling ((cx + fromIntegral (mR m)) / 8) * 8 :: Int
  ly = jsRound ((cy - fromIntegral lh / 2) / 8) * 8
  place = [('R', ringL - 8 - lw - 8 - lw), ('O', ringL - 8 - lw), ('T', ringR + 8), ('A', ringR + 8 + lw + 8)]
  letterSet = S.fromList
    [ (ly + y) * w + x0 + x
    | (ch, x0) <- place
    , let msk = glyphMask spec (lsGlyph spec ch)
    , y <- [0 .. lh - 1], x <- [0 .. lw - 1]
    , msk U.! (y * lw + x), x0 + x >= 0, x0 + x < w ]
  lets = U.generate (w * h) (`S.member` letterSet)
  baseX = if mCx m `mod` 8 == 0 then 4 else 0
  baseY = if mCy m `mod` 8 == 0 then 4 else 0
  rowsY = [ ty + 4 | ty <- [0, 8 .. h - 1], traceOff (ty + 4 - mCy m) baseY ]
  colsX = [ tx + 4 | tx <- [0, 8 .. w - 1], traceOff (tx + 4 - mCx m) baseX ]
  first base = head [ o | o <- [base, base + 32 ..], fromIntegral o >= fromIntegral (mR m) + (2 :: Double) ]
  ox = first baseX
  oy = first baseY
  sq = Square (mCx m - ox) (mCx m + ox) (max 4 (mCy m - oy)) (min (h - 4) (mCy m + oy))

traceH, traceV :: Geo -> Int -> Bool
traceH g ty = traceOff (ty + 4 - mCy (gM g)) (gBaseY g)
traceV g tx = traceOff (tx + 4 - mCx (gM g)) (gBaseX g)

isTraceCell :: Geo -> Int -> Int -> Bool
isTraceCell g tx ty = traceH g ty || traceV g tx

-- * The fabric

-- | A trace tile's shape: horizontal, vertical, a crossing.
data Shape = SH | SV | SX
  deriving stock (Eq, Ord, Show, Enum, Bounded)

weaveL :: Int
weaveL = 16

-- | A trace tile's phase (its top-left pixel @tx, ty@).
weavePhase :: Geo -> Int -> Int -> Int
weavePhase g tx ty = (floor (fromIntegral (abs dx + abs dy) / (8 :: Double)) + line * 5) `mod` weaveL
 where
  dx = tx + 4 - mCx (gM g)
  dy = ty + 4 - mCy (gM g)
  line
    | traceH g ty = jsRound (fromIntegral (abs dy) / 32)
    | otherwise = jsRound (fromIntegral (abs dx) / 32) + 3

-- | The fabric's step at lab frame @f@ (every 4 frames, plus the crash's
-- burst of up to 8).
weaveStep :: Int -> Int
weaveStep f = f `div` 4 + burst
 where
  b = fromIntegral f / labFps / beatSec
  burst = if b >= lastBeat then min 8 (floor ((b - lastBeat) / 0.06)) else 0

-- | A phase's level at a step: 0 the pulse, 1 its tail, 2 and on dotted.
weaveLevel :: Int -> Int -> Int
weaveLevel ph step = (ph - step) `mod` weaveL

-- | Draw a trace tile at level @lv@ into a pixel map.
weaveTile :: Geo -> Int -> Int -> Int -> [(Int, Word8)]
weaveTile g tx ty lv =
  [ ((ty + ly) * w + tx + lx, if lv == 0 then bright else dim)
  | ly <- [0 .. 7], lx <- [0 .. 7]
  , let onH = hz && (if lv <= 1 then ly == 3 || (lv == 0 && ly == 4) else ly == 3 && even lx)
        onV = vt && (if lv <= 1 then lx == 3 || (lv == 0 && lx == 4) else lx == 3 && even ly)
        node = hz && vt && lx >= 2 && lx <= 4 && ly >= 2 && ly <= 4
  , onH || onV || node ]
 where
  w = mW (gM g)
  hz = traceH g ty
  vt = traceV g tx
  bright = roleTeal
  dim = roleViolet

-- | How far the lambdas are revealed at a beat (tile centres within it).
embRadius :: Geo -> Double -> Double
embRadius g b
  | b < growBeat = -1
  | b >= lastBeat = 1e9
  | otherwise = (1 - (1 - (b - growBeat) / (lastBeat - growBeat)) ^ (2 :: Int)) * gEmbMax g

-- | What the fabric's trace tiles show: each at its level ('WeaveAt'), or
-- at its own phase number as a level, as the lab counts tiles ('WeaveCost').
data Weave = WeaveAt Int | WeaveCost

-- | One frame of background at a beat (the lab's @compose@).
compose :: Geo -> Double -> Weave -> U.Vector Word8
compose g b wv = base U.// fabric
 where
  m = gM g
  w = mW m
  h = mH m
  er = embRadius g b
  base = U.generate (w * h) $ \i ->
    if gCore g U.! i then roleCore
    else if gEmb g U.! i && gEmbCell g U.! i <= er then roleEmb
    else if gRing g U.! i /= 0 then gRing g U.! i
    else if gLetters g U.! i then roleLetters
    else roleBg
  fabric = concat
    [ weaveTile g tx ty lv
    | ty <- [0, 8 .. h - 1], tx <- [0, 8 .. w - 1]
    , isTraceCell g tx ty
    , not (or [ gLogo g U.! (y * w + x) | y <- [ty .. ty + 7], x <- [tx .. tx + 7] ])
    , let ph = weavePhase g tx ty
          lv = case wv of
            WeaveCost -> ph
            WeaveAt s -> weaveLevel ph s ]

-- * Sparks

polyAt :: [(Double, Double)] -> Double -> (Double, Double)
polyAt pts u = go pts (max 0 (min 1 u) * total)
 where
  lens = zipWith dist pts (drop 1 pts)
  total = sum lens
  dist (ax, ay) (bx, by) = sqrt ((bx - ax) ^ (2 :: Int) + (by - ay) ^ (2 :: Int))
  go ((ax, ay) : rest@((bx, by) : more)) d =
    let l = dist (ax, ay) (bx, by)
    in if d <= l || null more
         then let t = if l /= 0 then min 1 (d / l) else 0 in (ax + (bx - ax) * t, ay + (by - ay) * t)
         else go rest (d - l)
  go _ _ = error "polyAt: a path needs two points"

data PathKind = PLead | PLeadBack | PHarm | PHarmBack | PBass | PBassCore

statements :: [(SparkVoice, [(Double, Double, PathKind)])]
statements =
  [ (SLead, [(0, 1.5, PLead), (3, 4.5, PLeadBack)])
  , (SHarm, [(1, 2.5, PHarm), (4, 5.5, PHarmBack)])
  , (SBass, [(2, 3.5, PBass), (5, 6, PBassCore)]) ]

path :: Geo -> PathKind -> [(Double, Double)]
path g kind = case kind of
  PLead -> lead
  PLeadBack -> reverse lead
  PHarm -> harm
  PHarmBack -> reverse harm
  PBass -> [(cx, h + 10), (cx, cy + r + 6)]
  PBassCore -> [(cx, h + 10), (cx, cy)]
 where
  m = gM g
  q = gSquare g
  f = fromIntegral :: Int -> Double
  cx = f (mCx m)
  cy = f (mCy m)
  h = f (mH m)
  r = f (mR m)
  lead = [(-10, f (sqY0 q)), (f (sqX1 q), f (sqY0 q)), (f (sqX1 q), cy)]
  harm = [(f (mW m) + 10, f (sqY1 q)), (f (sqX0 q), f (sqY1 q)), (f (sqX0 q), cy)]

-- | Every spark's head at a beat, in the lab's order (statements, then idle).
sparksAt :: Geo -> Double -> [(Double, Double, SparkVoice)]
sparksAt g b = moving <> idle
 where
  m = gM g
  q = gSquare g
  f = fromIntegral :: Int -> Double
  moving =
    [ (x, y, v)
    | (v, list) <- statements, (b0, b1, kind) <- list
    , b >= b0, b < b1
    , let (x, y) = polyAt (path g kind) ((b - b0) / (b1 - b0)) ]
  loop = [(f (mCx m), f (sqY0 q)), (f (sqX1 q), f (sqY0 q)), (f (sqX1 q), f (sqY1 q)), (f (sqX0 q), f (sqY1 q)), (f (sqX0 q), f (sqY0 q)), (f (mCx m), f (sqY0 q))]
  idle
    | b >= endBeat = [ (x, y, v) | (i, v) <- zip [0 :: Int ..] [SLead, SHarm, SBass]
                                 , let (x, y) = polyAt loop (((b - endBeat) / 4 + fromIntegral i / 3) `mod'` 1) ]
    | otherwise = []

data Part = PartHead | PartTrail
  deriving stock (Eq, Ord, Show)

-- | A sprite-layer pixel: where, its role, and which sprite it belongs to
-- (the lab's grouping: the voice and head index with the part, or a
-- light's crossing).
data SprPixel = SprPixel
  { spX, spY :: Int
  , spRole :: Word8
  , spGroup :: (Int, Int, Int, Part)  -- ^ (0 spark / 1 light, voice or x, head index or y, part)
  } deriving stock (Eq, Show)

-- | The sparks' pixels at a beat: each head a diamond of radius 3 with a
-- centre of radius 1, its trail two diamonds behind it.
sparkPixels :: Geo -> Double -> [SprPixel]
sparkPixels g b = concat (zipWith one [0 ..] (sparksAt g b))
 where
  one n (hx, hy, v) =
    let (hi, ti) = sparkRoles v
        xr = jsRound hx
        yr = jsRound hy
        grp p = (0, fromEnum v, n, p)
        trail = findTrail [(5, 2), (9, 1)] 1
        findTrail [] _ = []
        findTrail want@((dmin, r) : rest) j
          | j > (30 :: Int) = []
          | otherwise = case [ (x, y) | (x, y, v') <- sparksAt g (b - fromIntegral j * 0.008), v' == v ] of
              [] -> []
              ((px, py) : _) ->
                let d = abs (px - hx) + abs (py - hy)
                in if d < dmin then findTrail want (j + 1)
                   else diamond (jsRound px) (jsRound py) r (const ti) (grp PartTrail) <> findTrail rest (j + 1)
        headPx = diamond xr yr 3 (\md -> if md <= 1 then roleSparkCore else hi) (grp PartHead)
    in trail <> headPx
  diamond x0 y0 r col grp =
    [ SprPixel (x0 + dx) (y0 + dy) (col (abs dx + abs dy)) grp
    | dy <- [-r .. r], dx <- [-r .. r], abs dx + abs dy <= r ]

-- | The junction lights' step at a beat.
lightStep :: Double -> Int
lightStep b
  | b >= endBeat = floor ((b - endBeat) * 2) + 40
  | otherwise = floor (b * 3) + hits + crash
 where
  hits = length (filter (<= b) rollAll)
  crash = if b >= lastBeat then min 10 (floor ((b - lastBeat) / 0.06)) else 0

-- | The junction lights at a beat, over that frame's background (a light
-- only shows where a trace crossing is drawn).
lightPixels :: Geo -> Double -> U.Vector Word8 -> [SprPixel]
lightPixels g b buf
  | b < 0 = []
  | otherwise =
      [ SprPixel (x + dx) (y + dy) (if abs dx + abs dy == 2 then roleLightD else roleLightB) (1, x, y, PartHead)
      | (r, y) <- zip [0 :: Int ..] (gRowsY g)
      , y /= sqY0 q, y /= sqY1 q
      , x <- gColsX g
      , buf U.! (y * w + x) /= 0, not (gLogo g U.! (y * w + x))
      , let c = jsRound (fromIntegral (abs (x - mCx m)) / (32 :: Double))
      , (c + r * 3 - step) `mod` 10 == 0
      , dy <- [-1 .. 1], dx <- [-1 .. 1] ]
 where
  m = gM g
  w = mW m
  q = gSquare g
  step = lightStep b

-- * Colour roles over time

-- | A ring stage's look: unlit, lit, the crash's and idle wave's head, the
-- wave's trail, or the background (the crash's dark flicker).
data RingLook = RingOff | RingOn | RingHead | RingMid | RingBg
  deriving stock (Eq, Ord, Show, Enum, Bounded)

data RoleState = RoleState
  { rsRing :: [RingLook]  -- ^ stages 1 to 5
  , rsCoreFlash :: Bool   -- ^ the core flashes white
  , rsLettersLit :: Bool
  } deriving stock (Eq, Ord, Show)

-- | The lab's @palette@, as looks rather than colours.
roleState :: Double -> RoleState
roleState b = RoleState (map stage [0 .. 4]) (b >= lastBeat && b < lastBeat + 0.25) (b >= lastBeat + 0.4)
 where
  stage :: Int -> RingLook
  stage s
    | b >= endBeat = let w = floor ((b - endBeat) * 2) `mod` 6 in if s == w then RingHead else if s == w - 1 then RingMid else RingOn
    | b >= lastBeat && b < lastBeat + 0.5 = if even (floor ((b - lastBeat) / 0.125) :: Int) then RingHead else RingBg
    | b >= rollBeat && b < growBeat, Just (h, hb, len) <- hit, even s == even h, b - hb < len * 0.6 = RingOff
    | b >= fromIntegral s = RingOn
    | otherwise = RingOff
  hit = case [ (j, hb) | (j, hb) <- zip [0 :: Int ..] flickHits, hb <= b ] of
    [] -> Nothing
    hs -> let (j, hb) = last hs
              nxt = case drop (j + 1) flickHits of
                (n : _) -> n
                [] -> growBeat
          in Just (j, hb, nxt - hb)

-- * The lab's figures

-- | Each 8 × 8 tile's pixels, row by row of tiles.
tileKeys :: Machine -> U.Vector Word8 -> [U.Vector Word8]
tileKeys m buf =
  [ U.fromList [ buf U.! (y * w + x) | y <- [ty .. ty + 7], x <- [tx .. tx + 7] ]
  | ty <- [0, 8 .. mH m - 1], tx <- [0, 8 .. w - 1] ]
 where
  w = mW m

data LabStats = LabStats
  { lsUnique :: Int
  , lsSplit :: (Int, Int)   -- ^ unique tiles in the top and bottom halves
  , lsMaxWrites :: Int      -- ^ tilemap writes in one frame (with the Game Boy's swaps if asked)
  , lsSprMax :: Int
  , lsSprLine :: Int
  , lsAttrMax :: Int        -- ^ 16 × 16 areas needing more than 3 colours, at worst
  , lsWeaveTiles :: Int
  } deriving stock (Eq, Show)

-- | Run the lab's @simulate@ for a machine: @twoBit@ relabels tiles (the
-- NES), @flips@ folds mirror images, @swap@ counts the colour changes of
-- a machine with one palette (the Game Boy) as tile writes, with its
-- colours per role and state ('RoleState' to a colour per role).
labStats :: Geo -> Bool -> Bool -> Maybe (RoleState -> Word8 -> Int) -> LabStats
labStats g twoBit flips swap = LabStats
  { lsUnique = S.size (S.unions uniques)
  , lsSplit = (S.size (S.unions (map fst halves)), S.size (S.unions (map snd halves)))
  , lsMaxWrites = maximum writes
  , lsSprMax = maximum (map fst sprs)
  , lsSprLine = maximum (map snd sprs)
  , lsAttrMax = maximum attrs
  , lsWeaveTiles = 9 }
 where
  m = gM g
  w = mW m
  h = mH m
  cols = w `div` 8
  splitRow = jsRound (fromIntegral (mCy m) / (8 :: Double))
  frames = [0 .. labFrames - 1]
  beatOf f = fromIntegral f / labFps / beatSec
  bufs = V.fromList [ compose g (beatOf f) WeaveCost | f <- frames ]
  keysV = V.map (V.fromList . map canon . tileKeys m) bufs
  canon k = let x = if twoBit then relabel k else k
            in if flips then minimum [x, flipH x, flipV x, flipH (flipV x)] else x
  uniques = [ S.fromList (V.toList ks) | ks <- V.toList keysV ]
  halves = [ ( S.fromList [ k | (i, k) <- zip [0 :: Int ..] (V.toList ks), i `div` cols < splitRow ]
             , S.fromList [ k | (i, k) <- zip [0 :: Int ..] (V.toList ks), i `div` cols >= splitRow ] )
           | ks <- V.toList keysV ]
  rawKeys = V.map (V.fromList . tileKeys m) bufs
  writes = 0 : [ tw + cw
               | f <- drop 1 frames
               , let a = rawKeys V.! (f - 1)
                     c = rawKeys V.! f
                     tw = length [ () | (x, y) <- zip (V.toList a) (V.toList c), x /= y ]
                     cw = case swap of
                       Nothing -> 0
                       Just col ->
                         let cellCol st ks = V.map (\k -> [ col st r | r <- nub (U.toList k), r /= 0 ]) ks
                             pa = cellCol (roleState (beatOf (f - 1))) a
                             pc = cellCol (roleState (beatOf f)) c
                         in length [ () | i <- [0 .. V.length c - 1], pa V.! i /= pc V.! i, a V.! i == c V.! i ] ]
  sprs = [ sprCount (sparkPixels g b <> lightPixels g b (compose g b (WeaveAt (weaveStep f))))
         | f <- frames, let b = beatOf f ]
  sprCount px =
    let inside = [ p | p <- px, spX p >= 0, spX p < w, spY p >= 0, spY p < h ]
        groups = M.fromListWith (\(a0, a1) (b0, b1) -> (min a0 b0, max a1 b1)) [ (spGroup p, (spY p, spY p)) | p <- inside ]
        perLine = M.fromListWith (+) [ (y, 1 :: Int) | (y0, _) <- M.elems groups, y <- [y0 .. min (h - 1) (y0 + 7)] ]
    in (M.size groups, maximum (0 : M.elems perLine))
  attrs = [ length [ () | ay <- [0, 16 .. h - 1], ax <- [0, 16 .. w - 1]
                        , S.size (S.fromList [ c | y <- [ay .. min (h - 1) (ay + 15)], x <- [ax .. ax + 15], let c = buf U.! (y * w + x), c /= 0 ]) > 3 ]
          | twoBit, buf <- V.toList bufs ] <> [0]

relabel :: U.Vector Word8 -> U.Vector Word8
relabel k = U.fromList (go M.empty 1 (U.toList k))
 where
  go _ _ [] = []
  go mp nx (c : cs)
    | c == 0 = 0 : go mp nx cs
    | Just l <- M.lookup c mp = l : go mp nx cs
    | otherwise = nx : go (M.insert c nx mp) (nx + 1) cs

flipH, flipV :: U.Vector Word8 -> U.Vector Word8
flipH k = U.concat [ U.reverse (U.slice (r * 8) 8 k) | r <- [0 .. 7] ]
flipV k = U.concat [ U.slice (r * 8) 8 k | r <- [7, 6 .. 0] ]
