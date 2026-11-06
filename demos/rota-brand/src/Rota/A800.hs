{-|
Rendition B on the Atari 800XL (PLAN.md, side track J, step 6;
docs/brand.md, "The Atari 800XL's layout", layout A, the user's choice
2026-10-07). This module holds the screen's static layout: the mode E
bitmap's roles, the playfield registers each line uses for them, and the
lines where the display-list interrupts reload registers.

__Screen.__ 192 lines of ANTIC mode E (160 pixels, two of our 320 a
pixel), after 24 blank lines: picture rows 24 to 215 of "A800.Picture"'s
320 × 240. The lab's 800XL geometry with solid letters
('geometrySolid').

__Playfield.__ Background, PF0 to PF2: the ring's stages, the lambdas and
the fabric (one colour). The letters are the four players on the letter
band (lines 80 to 111); the core is PF3 (the missiles as the fifth
player), so a line holds at most three playfield roles. Each line's roles
take PF0 to PF2 ('pfMap'), kept where they were on the line before.
-}
module Rota.A800
  ( a800Geo
    -- * Screen
  , screenLines
  , bandTop
  , bandBottom
  , sparkTop
  , sparkBottom
  , coreTop
  , coreBottom
    -- * Playfield roles
  , PfRole (..)
  , lineRoles
  , pfMap
  , pfWrites
    -- * Time
  , a800Fps
  , a800Clock
    -- * Colours
  , atari
  , colBk
  , colFabric
  , colLambda
  , colCore
  , colLight
  , ringColour
  , letterColour
  , sparkColour
  , gradLevel
  , gradPhaseLevel
    -- * A frame
  , Obj (..)
  , Shot (..)
  , shotAt
  , sparkRow
  , lightRows
  , letterObjs
  , coreObjs
  , Regs (..)
  , lineRegs
    -- * Picture
  , renderShot
  , a800Expected
  ) where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Vector.Unboxed as U
import Data.Word (Word8)

import Data.Bits (setBit, shiftL, testBit, (.&.), (.|.))
import Data.List (minimumBy)
import Data.Maybe (isJust, listToMaybe)
import Data.Ord (comparing)

import A800.Palette (paletteRgb)
import Rota.Clock
import Rota.Lab
import Rota.Score (jsRound)

a800Geo :: Geo
a800Geo = geometrySolid a800M

screenLines :: Int
screenLines = 192

-- | The letters' rows (players), and the rows where the sparks are PF3
-- missiles instead of players (the band and four lines either side, so
-- the players' positions and colours can change there unseen).
bandTop, bandBottom, sparkTop, sparkBottom :: Int
bandTop = 80
bandBottom = 111
sparkTop = 76
sparkBottom = 115

-- | The core's rows (PF3, white).
coreTop, coreBottom :: Int
coreTop = 92
coreBottom = 99

-- | A playfield role: a ring stage (1 to 5), the lambdas, the fabric.
data PfRole = Stage Int | Lambdas | Fabric
  deriving stock (Eq, Ord, Show)

w :: Int
w = 320

-- | The playfield roles of a mode E pixel pair on line y, from the whole
-- demo (the lambdas whole, the fabric at any level): the left pixel's,
-- or the right one's where the left is background. Letters and the core
-- are not playfield.
pairRole :: Int -> Int -> Maybe PfRole
pairRole xe y = case (role (2 * xe), role (2 * xe + 1)) of
  (Just a, _) -> Just a
  (Nothing, b) -> b
 where
  buf = composed
  role x = case buf U.! (y * w + x) of
    r | r >= 1 && r <= 5 -> Just (Stage (fromIntegral r))
      | r == roleEmb -> Just Lambdas
      | r == roleTeal || r == roleViolet -> Just Fabric
      | otherwise -> Nothing

-- | The whole picture once: the lambdas revealed, every trace at a
-- drawn level.
composed :: U.Vector Word8
composed = compose a800Geo 7 WeaveCost

-- | Each line's playfield roles.
lineRoles :: [[PfRole]]
lineRoles = [ S.toList (S.fromList [ r | xe <- [0 .. 159], Just r <- [pairRole xe y] ]) | y <- [0 .. screenLines - 1] ]

-- | Each line's roles on PF0 to PF2 (index = register): the fabric is
-- always PF0; the others keep the register they had on the line before,
-- new ones take PF1 or PF2 (the one free). An error if a line needs more
-- than three.
pfMap :: [M.Map PfRole Int]
pfMap = go M.empty (zip [0 :: Int ..] lineRoles)
 where
  go _ [] = []
  go prev ((y, rs) : rest)
    | length rs > 3 || length [ r | r <- rs, r /= Fabric ] > 2 = error ("Rota.A800: line " <> show y <> " needs " <> show rs)
    | otherwise =
        let others = [ r | r <- rs, r /= Fabric ]
            kept = M.filterWithKey (\r _ -> r `elem` others) prev
            free = [ i | i <- [1, 2], i `notElem` M.elems kept ]
            new = M.fromList (zip [ r | r <- others, not (M.member r kept) ] free)
            m = M.insert Fabric 0 (M.union kept new)
        in m : go (M.delete Fabric m) rest

-- | The playfield register loads: (first line it must hold, register,
-- role), with the earliest line it may be written on (the line after the
-- register last showed another role).
pfWrites :: [(Int, Int, Int, PfRole)]
pfWrites =
  [ (earliest, y, reg, role)
  | reg <- [0 .. 2]
  , let segs = segments [ (y, M.foldrWithKey (\r i acc -> if i == reg then Just r else acc) Nothing m) | (y, m) <- zip [0 ..] pfMap ]
  , (y, role, earliest) <- segs ]
 where
  -- each change of role on the register, with the line after its last use
  segments ys = go (-1) Nothing ys
   where
    go _ _ [] = []
    go lastUse cur ((y, Just r) : rest)
      | Just r == cur = go y cur rest
      | otherwise = (y, r, lastUse + 1) : go y (Just r) rest
    go lastUse cur ((_, Nothing) : rest) = go lastUse cur rest

-- * Time

-- | The NTSC 800XL's frame rate: 29868 machine cycles a frame.
a800Fps :: Double
a800Fps = 1789772.5 / 29868

a800Clock :: Clock
a800Clock = clock a800Fps

-- * Colours

-- | The nearest colour of the 800XL's palette (even values: GTIA ignores
-- bit 0). A colour with a hue keeps it: only entries within 30° of its
-- hue (in YIQ) are candidates, so the lab's muted dark violets stay
-- violet rather than turn grey.
atari :: Int -> Word8
atari rgb = minimumBy (comparing dist) (if chroma t > 12 then hued else [0, 2 .. 254])
 where
  t = (rgb `div` 65536, (rgb `div` 256) `mod` 256, rgb `mod` 256)
  rgbOf c = let (r', g', b') = paletteRgb c in (fromIntegral r', fromIntegral g', fromIntegral b')
  dist c = let (r, g, b) = t; (r', g', b') = rgbOf c in (r - r') ^ (2 :: Int) + (g - g') ^ (2 :: Int) + (b - b') ^ (2 :: Int)
  iq (r, g, b) = let f = fromIntegral :: Int -> Double
                 in (0.596 * f r - 0.274 * f g - 0.322 * f b, 0.211 * f r - 0.523 * f g + 0.312 * f b)
  chroma c = let (i, q) = iq c in sqrt (i * i + q * q)
  hue c = let (i, q) = iq c in atan2 q i
  hued = [ c | c <- [0, 2 .. 254], let c' = rgbOf c, chroma c' > 8
             , let d = abs (hue c' - hue t), min d (2 * pi - d) < pi / 6 ]

-- | The lab's 800XL look (@LOOK.a800@).
colBk, colFabric, colLambda, colCore, colLight :: Word8
colBk = atari 0x0E0B14
colFabric = atari 0x1E6E68
colLambda = atari 0xF2A23A
colCore = atari 0xE8F8F0
colLight = atari 0xC8F8F0

ringColour :: RingLook -> Word8
ringColour l = atari $ case l of
  RingOff -> 0x3B2E5A
  RingOn -> 0x2CB5A8
  RingHead -> 0xE8F8F0
  RingMid -> 0x7FD8CC
  RingBg -> 0x0E0B14

-- | The letters: dim, then lit as one of seven steps from amber to teal.
letterColour :: Maybe Int -> Word8
letterColour Nothing = atari 0x2A2340
letterColour (Just lv) = atari (mixRgb 0xF2A23A 0x2CB5A8 (fromIntegral lv / 6))

mixRgb :: Int -> Int -> Double -> Int
mixRgb a c t = ch 65536 * 65536 + ch 256 * 256 + ch 1
 where
  ch s' = jsRound (fromIntegral ((a `div` s') `mod` 256) * (1 - t) + fromIntegral ((c `div` s') `mod` 256) * t)

-- | A spark's one colour (its head's): lead, harmony, bass.
sparkColour :: SparkVoice -> Word8
sparkColour v = atari $ case v of
  SLead -> 0xFFE0A0
  SHarm -> 0xB8F8F0
  SBass -> 0xD8C8FF

-- | The lit letters' gradient on line y in picture frame p: the lab's
-- @0.5 + 0.5 sin(0.33 y - 4 b)@ in seven steps, as the ROM computes it:
-- a 256-step phase, 13.5 a line and 7 a frame.
gradLevel :: Int -> Int -> Int
gradLevel p y = gradPhaseLevel (((27 * y - 14 * p) `div` 2) `mod` 256)

-- | The gradient's step (0 to 6) at a phase (0 to 255).
gradPhaseLevel :: Int -> Int
gradPhaseLevel ph = jsRound (6 * (0.5 + 0.5 * sin (2 * pi * fromIntegral ph / 256)))

-- * A frame

-- | A player's or missile's graphics: the picture x of its leftmost bit
-- (even), the first screen line, a value a line (a player's 8 bits, bit
-- 7 left, or a missile's 2, bit 1 left), how many of our pixels a bit
-- covers (2, or 4 at double width), and the pixels outside its bits.
data Obj = Obj
  { obX0 :: Int
  , obY0 :: Int
  , obRows :: [Word8]
  , obBitW :: Int
  , obLost :: Int
  } deriving stock (Eq, Show)

-- | One picture frame of the 800XL demo.
data Shot = Shot
  { shBitmap :: U.Vector Word8          -- ^ 160 × 192 mode E pixels, 0 to 3
  , shStages :: [Word8]                 -- ^ ring stages 1 to 5's colours
  , shLetters :: Maybe Int              -- ^ Nothing: dim; Just p: lit, the gradient's frame
  , shPlayers :: [Maybe Obj]            -- ^ the sparks outside the spark rows (lead, harmony, bass)
  , shBand :: (Maybe Obj, Maybe Obj)    -- ^ the sparks in the spark rows: left (M2), right (M3)
  , shBandColour :: Word8               -- ^ PF3 in the spark rows (but the core's)
  , shLights :: [(Int, Maybe Obj, Maybe Obj)] -- ^ per light row: left (M2), right (M3)
  }

-- | Is screen line y one of the sparks' missile rows?
sparkRow :: Int -> Bool
sparkRow y = y >= sparkTop && y <= sparkBottom

-- | The rows the lights can be on (none in the spark rows).
lightRows :: [Int]
lightRows = [ y | y <- gRowsY a800Geo, y /= sqY0 sq, y /= sqY1 sq, not (sparkRow y) ]
 where
  sq = gSquare a800Geo

isLogo :: Int -> Int -> Bool
isLogo x y = x >= 0 && x < w && y >= 0 && y < screenLines && gLogo a800Geo U.! (y * w + x)

-- | An object from pixels: 8 bits of 2 pixels (players) or 2 bits of
-- @bitW@ (missiles); bits whose pixels touch the logo stay clear (behind
-- it, as the lab draws sparks).
mkObj :: Bool -> Int -> [(Int, Int)] -> Maybe Obj
mkObj _ _ [] = Nothing
mkObj player bitW pxs = Just (Obj x0 y0 [ row y | y <- [y0 .. y1] ] bitW (length pxs - length kept))
 where
  bitsN = if player then 8 else 2
  x0 = 2 * (minimum (map fst pxs) `div` 2)
  y0 = minimum (map snd pxs)
  y1 = maximum (map snd pxs)
  bitOf x = (x - x0) `div` bitW
  kept = [ q | q@(x, _) <- pxs, bitOf x < bitsN ]
  behind x y = let xa = x0 + bitW * bitOf x in or [ isLogo xx y | xx <- [xa .. xa + bitW - 1] ]
  row y = foldl (\acc (x, _) -> setBit acc (bitsN - 1 - bitOf x)) 0 [ q | q@(x, yy) <- kept, yy == y, not (behind x y) ]

shotAt :: Int -> Shot
shotAt p = Shot bitmap stages letters players band bandColour lights
 where
  t = picTime a800Clock p
  b = ptBeat t
  bs = if isJust (ptIdle t) then b + 4 else b
  st = roleState b
  stages = map ringColour (rsRing st)
  letters = if rsLettersLit st then Just p else Nothing
  buf = compose a800Geo b (WeaveAt (weaveCapped a800Clock p))
  bitmap = U.generate (160 * screenLines) $ \i ->
    let (y, xe) = i `divMod` 160
        m = pfMap !! y
        reg ro = maybe 0 (fromIntegral . (+ 1)) (M.lookup ro m)
        val x = case buf U.! (y * w + x) of
          r | r >= 1 && r <= 5 -> reg (Stage (fromIntegral r))
            | r == roleEmb -> reg Lambdas
            | r == roleTeal || r == roleViolet -> reg Fabric
            | otherwise -> 0
        a = val (2 * xe)
    in if a /= 0 then a else val (2 * xe + 1)
  sparkPx = [ (spX q, spY q, toEnum vi :: SparkVoice) | q <- sparkPixels a800Geo bs, let (_, vi, _, _) = spGroup q, spY q >= 0, spY q < screenLines ]
  players = [ mkObj True 2 [ (x, y) | (x, y, v') <- sparkPx, v' == v, not (sparkRow y) ] | v <- [SLead, SHarm, SBass] ]
  inBand = [ q | q@(_, y, _) <- sparkPx, sparkRow y ]
  side left = [ (x, y) | (x, y, _) <- inBand, (x < 160) == left ]
  band = (mkObj False 4 (side True), mkObj False 4 (side False))
  bandColour = case [ v | v <- [SLead, SHarm, SBass], any (\(_, _, v') -> v' == v) inBand ] of
    (v : _) -> sparkColour v
    [] -> colLight
  lightPx = [ (spX q, spY q) | q <- lightPixels a800Geo b crossings ]
  lights = [ (ry, mkObj False 2 [ q | q@(x, _) <- near, x < 160 ], mkObj False 2 [ q | q@(x, _) <- near, x >= 160 ])
           | ry <- lightRows, let near = [ q | q@(_, y) <- lightPx, abs (y - ry) <= 1 ] ]

crossings :: U.Vector Word8
crossings = compose a800Geo 0 WeaveCost

-- * Registers

-- | GTIA's registers on a screen line as the ROM sets them (colours,
-- positions as colour clocks, missile sizes), and the graphics there.
data Regs = Regs
  { rgPf :: [Word8]        -- ^ COLPF0 to COLPF3
  , rgPm :: [Word8]        -- ^ COLPM0 to COLPM3
  , rgHposP :: [Int]
  , rgHposM :: [Int]
  , rgSizeM :: Word8
  , rgGrafP :: [Word8]
  , rgGrafM :: Word8
  } deriving stock (Eq, Show)

-- | The letters as players: R, O, T, A, each 16 of our pixels wide at the
-- lab's places (one tile from the ring, one tile apart).
letterObjs :: [Obj]
letterObjs =
  [ Obj x0 bandTop [ foldl (\acc i -> if pair x0 i y then setBit acc (7 - i) else acc) 0 [0 .. 7] | y <- [bandTop .. bandBottom] ] 2 0
  | x0 <- [ringL - 48, ringL - 24, ringR + 8, ringR + 32] ]
 where
  ringL = ((mCx a800M - mR a800M) `div` 8) * 8
  ringR = ((mCx a800M + mR a800M + 7) `div` 8) * 8
  pair x0 i y = gLetters a800Geo U.! (y * w + x0 + 2 * i) || gLetters a800Geo U.! (y * w + x0 + 2 * i + 1)

-- | The core's two missiles (M0, M1): picture x 156 and 160.
coreObjs :: (Obj, Obj)
coreObjs = (Obj 156 coreTop (replicate 8 3) 2 0, Obj 160 coreTop (replicate 8 3) 2 0)

-- | The registers on each screen line of a shot.
lineRegs :: Shot -> [Regs]
lineRegs sh = [ regs y | y <- [0 .. screenLines - 1] ]
 where
  cc x = 48 + x `div` 2
  rowOf o y = let i = y - obY0 o in if i >= 0 && i < length (obRows o) then obRows o !! i else 0
  inBand y = y >= bandTop && y <= bandBottom
  regs y =
    let m = pfMap !! y
        roleColour ro = case ro of
          Stage s -> shStages sh !! (s - 1)
          Lambdas -> colLambda
          Fabric -> colFabric
        pfReg i = maybe 0 roleColour (listToMaybe [ ro | (ro, j) <- M.toList m, j == i ])
        pf3 | y >= coreTop && y <= coreBottom = colCore
            | sparkRow y = shBandColour sh
            | otherwise = colLight
        letterCol = letterColour (fmap (`gradLevel` y) (shLetters sh))
        -- a spark player above the spark rows shows above them, one below below
        playerHere v = case shPlayers sh !! v of
          Just o | (obY0 o < sparkTop) == (y < sparkTop) -> Just o
          _ -> Nothing
        colpm v | inBand y = letterCol
                | v < 3 = sparkColour (toEnum v)
                | otherwise = colBk
        hposp v | inBand y = cc (obX0 (letterObjs !! v))
                | v < 3, Just o <- playerHere v = cc (obX0 o)
                | otherwise = 0
        grafp v | inBand y = rowOf (letterObjs !! v) y
                | v < 3, Just o <- playerHere v = rowOf o y
                | otherwise = 0
        (core0, core1) = coreObjs
        (m2, m3) | sparkRow y = shBand sh
                 | otherwise = case [ (l, r) | (ry, l, r) <- shLights sh, (ry < sparkTop) == (y < sparkTop) ] of
                     (lr : _) -> lr
                     [] -> (Nothing, Nothing)
        hm = maybe 0 (cc . obX0)
        gm = maybe 0 (`rowOf` y)
        grafm = (rowOf core0 y .&. 3) .|. ((rowOf core1 y .&. 3) `shiftL` 2) .|. ((gm m2 .&. 3) `shiftL` 4) .|. ((gm m3 .&. 3) `shiftL` 6)
        sizem = if sparkRow y then 0x50 else 0  -- M2, M3 at double width
    in Regs [pfReg 0, pfReg 1, pfReg 2, pf3] (map colpm [0 .. 3]) (map hposp [0 .. 3]) [cc 156, cc 160, hm m2, hm m3] sizem (map grafp [0 .. 3]) grafm

-- * Picture

-- | The picture (320 × 240 colour bytes, "A800.Picture"'s crop): the
-- background outside the 192 lines; players over the missiles (PF3, the
-- fifth player) over the playfield (PRIOR $11).
renderShot :: Maybe Shot -> U.Vector Word8
renderShot Nothing = U.replicate (320 * 240) 0
renderShot (Just sh) = U.concat [ line py | py <- [0 .. 239] ]
 where
  regsL = lineRegs sh
  line py
    | y < 0 || y >= screenLines = U.replicate 320 colBk
    | otherwise = U.generate 320 px
   where
    y = py - 24
    r = regsL !! y
    px x =
      let c = 48 + x `div` 2
          pl = [ n | n <- [0 .. 3], let h = rgHposP r !! n, c >= h, c < h + 8, testBit (rgGrafP r !! n) (7 - (c - h)) ]
          mWidth m = if testBit (rgSizeM r) (2 * m) then 2 else 1
          mi = [ m | m <- [0 .. 3], let h = rgHposM r !! m; wd = mWidth m, c >= h, c < h + 2 * wd
                   , testBit (rgGrafM r) (2 * m + 1 - (c - h) `div` wd) ]
          pfv = shBitmap sh U.! (y * 160 + x `div` 2)
      in case pl of
           (n : _) -> rgPm r !! n
           [] | not (null mi) -> rgPf r !! 3
              | pfv == 0 -> colBk
              | otherwise -> rgPf r !! (fromIntegral pfv - 1)

-- | The picture of each ROM frame: black in the lead-in.
a800Expected :: Int -> [U.Vector Word8]
a800Expected n = [ renderShot (if k < lead then Nothing else Just (shotAt (k - lead))) | k <- [0 .. n - 1] ]
 where
  lead = ckLead a800Clock
