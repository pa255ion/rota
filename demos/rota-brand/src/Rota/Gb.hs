{-|
Rendition B on the Game Boy (PLAN.md, side track J, step 2; docs/brand.md,
"Rendition B"): what the ROM shows, frame by frame, as tiles, tilemaps,
sprites and palettes. "Rota.Gb.Rom" turns it into the ROM's streams.

__Colours.__ BG colour 0 is the background (the DMG's sprite priority
counts it as transparent), 1 teal, 2 amber, 3 paper; BGP $1B shows them as
ink, teal, amber, paper. The DMG has one background palette, so the
ring's and letters' looks are tiles of their own, swapped in the tilemap.
Sprites use OBP0 (1 paper, 2 amber, 3 teal).

__The background.__ A tile is either a fabric tile, (phase, shape), drawn
in one of 48 slots whose graphics the ROM redraws as the fabric steps, or a
logo tile, whose tile index changes as its look does. During the lead-in
the screen is dark (BGP $FF, sprites off).

__Sprites.__ Each spark is a head and a trail sprite, each light one
sprite, as the lab groups them; a group that doesn't fit an 8 × 8 box
loses the pixels outside it (counted by the tests). A sprite touching a
logo pixel goes behind the background (OAM priority), as the lab draws
sparks behind the logo; elsewhere it is in front.
-}
module Rota.Gb
  ( gbGeo
  , gbClock
    -- * Cells
  , Shape (..)
  , Cell (..)
  , cells
  , cellAt
  , fabricSlots
  , fabricTile
    -- * Pictures
  , Tile
  , logoTile
  , rawRole
  , GbShot (..)
  , shotAt
    -- * Sprites
  , Sprite (..)
  , spritesAt
  , spriteTile
  , objIndex
    -- * Screen (the model the layer 3 tests compare with)
  , gbBgp
  , gbObp0
  , renderShot
  ) where

import Data.Bits (shiftR, (.&.))
import Data.List (sortOn)
import Data.Maybe (isJust)
import qualified Data.Map.Strict as M
import qualified Data.Vector.Unboxed as U
import Data.Word (Word8)

import Rota.Clock
import Rota.Lab
import Rota.Sound.Gb (gbFps)

gbGeo :: Geo
gbGeo = geometry gbM

gbClock :: Clock
gbClock = clock gbFps

w, h :: Int
w = 160
h = 144

-- * Cells

data Cell = Fabric Int Shape | Logo
  deriving stock (Eq, Ord, Show)

-- | The 20 × 18 cells, row by row.
cells :: [Cell]
cells = [ cellAt tx ty | ty <- [0, 8 .. h - 1], tx <- [0, 8 .. w - 1] ]

cellAt :: Int -> Int -> Cell
cellAt tx ty
  | isTraceCell gbGeo tx ty
  , not (or [ gLogo gbGeo U.! (y * w + x) | y <- [ty .. ty + 7], x <- [tx .. tx + 7] ]) =
      Fabric (weavePhase gbGeo tx ty) (case (traceH gbGeo ty, traceV gbGeo tx) of
        (True, True) -> SX
        (True, False) -> SH
        _ -> SV)
  | otherwise = Logo

-- | The fabric slots in use, in order.
fabricSlots :: [(Int, Shape)]
fabricSlots = M.keys (M.fromList [ ((ph, sh), ()) | Fabric ph sh <- cells ])

-- | 64 colour indices, rows top first.
type Tile = U.Vector Word8

-- | A fabric shape at a level (0 the pulse, 1 the tail, 2 dotted): the
-- lab's @weaveTile@ (bright amber, dim teal).
fabricTile :: Shape -> Int -> Tile
fabricTile sh lv = U.generate 64 $ \i ->
  let (ly, lx) = i `divMod` 8
      hz = sh /= SV
      vt = sh /= SH
      onH = hz && (if lv <= 1 then ly == 3 || (lv == 0 && ly == 4) else ly == 3 && even lx)
      onV = vt && (if lv <= 1 then lx == 3 || (lv == 0 && lx == 4) else lx == 3 && even ly)
      node = hz && vt && lx >= 2 && lx <= 4 && ly >= 2 && ly <= 4
  in if onH || onV || node then (if lv == 0 then 2 else 1) else 0

-- | A background role's colour index at a moment.
rawRole :: RoleState -> Word8 -> Word8
rawRole st r
  | r >= 1 && r <= 5 = case rsRing st !! fromIntegral (r - 1) of
      RingOff -> 0
      RingOn -> 1
      RingHead -> 3
      RingMid -> 1
      RingBg -> 0
  | r == roleEmb = 2
  | r == roleCore = 3
  | r == roleLetters = if rsLettersLit st then 3 else 0
  | otherwise = 0

-- | A logo cell's tile at a beat.
logoTile :: Double -> Int -> Int -> Tile
logoTile b tx ty = U.generate 64 $ \i ->
  let (ly, lx) = i `divMod` 8 in rawRole st (bgRole (tx + lx) (ty + ly))
 where
  st = roleState b
  er = embRadius gbGeo b
  bgRole x y =
    let j = y * w + x
    in if gCore gbGeo U.! j then roleCore
       else if gEmb gbGeo U.! j && gEmbCell gbGeo U.! j <= er then roleEmb
       else if gRing gbGeo U.! j /= 0 then gRing gbGeo U.! j
       else if gLetters gbGeo U.! j then roleLetters
       else roleBg

-- | One picture frame: each logo cell's tile (cells in order, Nothing for
-- fabric cells), the fabric's step, the sprites.
data GbShot = GbShot
  { shLogo :: [Maybe Tile]
  , shWeave :: Int
  , shSprites :: [Sprite]
  }

shotAt :: Int -> GbShot
shotAt p = GbShot
  [ case c of
      Logo -> Just (logoTile b tx ty)
      Fabric _ _ -> Nothing
  | (c, (tx, ty)) <- zip cells [ (tx, ty) | ty <- [0, 8 .. h - 1], tx <- [0, 8 .. w - 1] ] ]
  (weaveCapped gbClock p) (spritesAt (if isJust (ptIdle t) then b + 4 else b) b)
 where
  t = picTime gbClock p
  b = ptBeat t

-- * Sprites

-- | A sprite: its group (for slots), its top-left pixel, its 8 × 8
-- pattern (OBP0 indices), behind the background or not, and the pixels
-- its box had to leave out.
data Sprite = Sprite
  { spKey :: (Int, Int, Int, Part)
  , spX0, spY0 :: Int
  , spPat :: Tile
  , spBehind :: Bool
  , spLost :: Int
  } deriving stock (Eq, Show)

objIndex :: Word8 -> Word8
objIndex r
  | r `elem` [30, 32, 34, roleSparkCore, roleLightB] = 1
  | r `elem` [31, 35] = 2
  | otherwise = 3

-- | The sparks and the lights at a beat, as sprites (sparks first). The
-- idle loop draws the sparks a loop (4 beats) later, so their trails are
-- there from its first frame and it repeats exactly.
spritesAt :: Double -> Double -> [Sprite]
spritesAt bs b = map toSprite (M.toList groups)
 where
  px = sparkPixels gbGeo bs <> lightPixels gbGeo b crossings
  groups = M.fromListWith (flip (<>)) [ (spGroup p, [p]) | p <- px ]
  toSprite (k, ps) =
    let x0 = minimum (map spX ps)
        y0 = minimum (map spY ps)
        inBox p = spX p - x0 < 8 && spY p - y0 < 8
        kept = filter inBox ps
        pat = U.replicate 64 0 U.// [ ((spY p - y0) * 8 + spX p - x0, objIndex (spRole p)) | p <- kept ]
        behind = or [ gLogo gbGeo U.! (spY p * w + spX p) | p <- kept, onScreen p ]
    in Sprite k x0 y0 pat behind (length ps - length kept)
  onScreen p = spX p >= 0 && spX p < w && spY p >= 0 && spY p < h

-- | Every trace crossing is drawn at every level, so the lights need the
-- background only once.
crossings :: U.Vector Word8
crossings = compose gbGeo 0 WeaveCost

spriteTile :: Sprite -> Tile
spriteTile = spPat

-- * Screen

gbBgp, gbObp0 :: Word8
gbBgp = 0x1B
gbObp0 = 0x90

-- | The DMG shades (0 white to 3 black) of a screen: the background from
-- each cell's tile (fabric cells at the step's levels), sprites by the
-- DMG's rules (the smaller x wins, then the earlier sprite; behind the
-- background's colours 1 to 3 if so marked). In the lead-in, all black.
renderShot :: Maybe GbShot -> U.Vector Word8
renderShot Nothing = U.replicate (w * h) 3
renderShot (Just sh) = U.generate (w * h) px
 where
  bgTiles = zipWith bgTile cells (shLogo sh)
  bgTile (Fabric ph s) _ = fabricTile s (min 2 (weaveLevel ph (shWeave sh)))
  bgTile Logo (Just t) = t
  bgTile Logo Nothing = U.replicate 64 0
  bgv = U.concat bgTiles
  bgAt x y = bgv U.! (((y `div` 8) * 20 + x `div` 8) * 64 + (y `mod` 8) * 8 + x `mod` 8)
  sprs = sortOn (\(i, s) -> (spX0 s, i :: Int)) (zip [0 ..] (shSprites sh))
  shade pal c = (pal `shiftR` (2 * fromIntegral c)) .&. 3
  px j =
    let (y, x) = j `divMod` w
        bg = bgAt x y
        obj = [ (c, spBehind s) | (_, s) <- sprs
                                , let dx = x - spX0 s
                                      dy = y - spY0 s
                                , dx >= 0, dx < 8, dy >= 0, dy < 8
                                , let c = spPat s U.! (dy * 8 + dx), c /= 0 ]
    in case obj of
         ((c, behind) : _) | not (behind && bg /= 0) -> shade gbObp0 c
         _ -> shade gbBgp bg
