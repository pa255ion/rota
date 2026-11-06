{-|
Rendition B on the NES (PLAN.md, side track J, step 3; docs/brand.md,
"Rendition B" and "The NES's palettes"): what the ROM shows, frame by
frame, as tiles, a nametable, attributes, palettes and sprites.
"Rota.Nes.Rom" turns it into the ROM's streams.

__Palettes__ (the user's choice, 2026-10-07): four background palettes,
{ring stages 1 and 2, lambdas}, {stage 3, lambdas, core}, {stages 4 and 5,
lambdas}, {letters, fabric bright, fabric dim}; each 16 × 16 area takes
the one that holds its roles. The fabric is left out of areas that hold
ring, lambda or core pixels. Every look of the ring, the core's flash and
the letters' lighting is a palette write; only the lambdas' reveal
changes tiles (a tile at a time).

__Sprites__ have their own four palettes: the three sparks (head, trail,
the white centre) and the junction lights. As on the Game Boy, a sprite
touching a logo pixel goes behind the background; a sprite can't start
left of the screen's edge, so one that would is hidden.
-}
module Rota.Nes
  ( nesGeo
  , nesClock
    -- * Cells and areas
  , Cell (..)
  , cells
  , areaPalette
  , paletteRoles
    -- * Pictures
  , Tile
  , logoTile
  , fabricTile
  , NesShot (..)
  , shotAt
  , Sprite (..)
    -- * Colours
  , nesColour
  , bgColours
  , objColours
    -- * Screen
  , renderShot
  ) where

import Data.List (elemIndex, minimumBy)
import qualified Data.Map.Strict as M
import Data.Maybe (isJust)
import Data.Ord (comparing)
import qualified Data.Vector.Unboxed as U
import Data.Word (Word16, Word8)

import Nes.Palette (ntscPalette)
import Rota.Clock
import Rota.Lab
import Rota.Sound.Nes (nesFps)

nesGeo :: Geo
nesGeo = geometry nesM

nesClock :: Clock
nesClock = clock nesFps

w, h :: Int
w = 256
h = 240

-- * Cells and areas

-- | The roles of each background palette (slots 1 to 3).
paletteRoles :: [[Word8]]
paletteRoles = [[1, 2, roleEmb], [3, roleEmb, roleCore], [4, 5, roleEmb], [roleLetters, roleTeal, roleViolet]]

-- | Does a 16 × 16 area hold ring, lambda or core pixels?
logoArea :: Int -> Int -> Bool
logoArea ax ay = or [ gRing nesGeo U.! j /= 0 || gEmb nesGeo U.! j || gCore nesGeo U.! j
                    | y <- [ay .. min (h - 1) (ay + 15)], x <- [ax .. ax + 15], let j = y * w + x ]

logoAreas :: U.Vector Bool
logoAreas = U.generate (16 * 15) (\i -> let (ay, ax) = i `divMod` 16 in logoArea (16 * ax) (16 * ay))

data Cell = Fabric Int Shape | Logo
  deriving stock (Eq, Ord, Show)

-- | The 32 × 30 cells, row by row.
cells :: [Cell]
cells = [ cellAt tx ty | ty <- [0, 8 .. h - 1], tx <- [0, 8 .. w - 1] ]

cellAt :: Int -> Int -> Cell
cellAt tx ty
  | isTraceCell nesGeo tx ty
  , not (or [ gLogo nesGeo U.! (y * w + x) | y <- [ty .. ty + 7], x <- [tx .. tx + 7] ])
  , not (logoAreas U.! ((ty `div` 16) * 16 + tx `div` 16)) =
      Fabric (weavePhase nesGeo tx ty) (case (traceH nesGeo ty, traceV nesGeo tx) of
        (True, True) -> SX
        (True, False) -> SH
        _ -> SV)
  | otherwise = Logo

-- | The roles an area's pixels take over the whole demo (fabric where
-- its cells are fabric).
areaRoles :: Int -> [Word8]
areaRoles i =
  let (ay, ax) = i `divMod` 16
      logo = [ r | y <- [16 * ay .. min (h - 1) (16 * ay + 15)], x <- [16 * ax .. 16 * ax + 15], let r = roleAt (y * w + x), r /= 0 ]
      fab = [ roleTeal | ty <- [16 * ay, 16 * ay + 8], ty < h, tx <- [16 * ax, 16 * ax + 8], Fabric _ _ <- [cellAt tx ty] ]
  in M.keys (M.fromList [ (r, ()) | r <- logo <> fab ])
 where
  roleAt j
    | gCore nesGeo U.! j = roleCore
    | gEmb nesGeo U.! j = roleEmb
    | gRing nesGeo U.! j /= 0 = gRing nesGeo U.! j
    | gLetters nesGeo U.! j = roleLetters
    | otherwise = 0

-- | Each area's palette: the first that holds its roles.
areaPalette :: U.Vector Int
areaPalette = U.generate (16 * 15) $ \i ->
  case [ p | (p, rs) <- zip [0 ..] paletteRoles, all (`elem` (rs <> [roleViolet])) (areaRoles i) ] of
    (p : _) -> p
    [] -> error ("Rota.Nes: no palette for area " <> show i <> ": " <> show (areaRoles i))

-- | 64 two-bit values, rows top first.
type Tile = U.Vector Word8

slot :: Int -> Word8 -> Word8
slot p r = maybe 0 (fromIntegral . (+ 1)) (elemIndex r (paletteRoles !! p))

-- | A fabric shape at a level: bright (slot 2) for the pulse, dim (slot 3).
fabricTile :: Shape -> Int -> Tile
fabricTile sh lv = U.generate 64 $ \i ->
  let (ly, lx) = i `divMod` 8
      hz = sh /= SV
      vt = sh /= SH
      onH = hz && (if lv <= 1 then ly == 3 || (lv == 0 && ly == 4) else ly == 3 && even lx)
      onV = vt && (if lv <= 1 then lx == 3 || (lv == 0 && lx == 4) else lx == 3 && even ly)
      node = hz && vt && lx >= 2 && lx <= 4 && ly >= 2 && ly <= 4
  in if onH || onV || node then (if lv == 0 then 2 else 3) else 0

-- | A logo cell's tile at a beat (the lambdas revealed a tile at a time).
logoTile :: Double -> Int -> Int -> Tile
logoTile b tx ty = U.generate 64 $ \i ->
  let (ly, lx) = i `divMod` 8 in slot p (bgRole (tx + lx) (ty + ly))
 where
  p = areaPalette U.! ((ty `div` 16) * 16 + tx `div` 16)
  er = embRadius nesGeo b
  bgRole x y =
    let j = y * w + x
    in if gCore nesGeo U.! j then roleCore
       else if gEmb nesGeo U.! j && gEmbCell nesGeo U.! j <= er then roleEmb
       else if gRing nesGeo U.! j /= 0 then gRing nesGeo U.! j
       else if gLetters nesGeo U.! j then roleLetters
       else 0

-- * Colours

-- | A colour of the tile lab's NES look as an NES colour. The lab took
-- most of them from the common 2C02 palette (FCEUX's): those keep their
-- entry, the hue the designer picked; the in-between ones take the nearest
-- of the project's generated palette ("Nes.Palette"). Black is $0F.
nesColour :: Int -> Word8
nesColour 0 = 0x0F
nesColour rgb
  | Just c <- lookup rgb fceux = c
  | otherwise = nearest rgb
 where
  fceux = [ (0xFCA044, 0x27), (0xFCFCFC, 0x30), (0xE45C10, 0x17), (0x6844FC, 0x13), (0xFCE0A8, 0x37), (0xD8B8F8, 0x33), (0xA4E4FC, 0x31) ]

nearest :: Int -> Word8
nearest rgb = minimumBy (comparing dist) [ c | c <- [0 .. 0x3F], c `notElem` [0x0D, 0x0E, 0x0F, 0x1D, 0x1E, 0x1F, 0x2E, 0x2F, 0x3E, 0x3F] ]
 where
  (r, g, b) = (rgb `div` 65536, (rgb `div` 256) `mod` 256, rgb `mod` 256)
  dist c = let (r', g', b') = ntscPalette U.! fromIntegral c
               sq a a' = (a - fromIntegral a') ^ (2 :: Int)
           in sq r r' + sq g g' + sq b b'

-- | The four background palettes' colours at a moment ('docs/brand.md',
-- the NES's look).
bgColours :: RoleState -> [[Word8]]
bgColours st = [ map role rs | rs <- paletteRoles ]
 where
  role r
    | r >= 1 && r <= 5 = nesColour (case rsRing st !! fromIntegral (r - 1) of
        RingOff -> 0x3C1E80
        RingOn -> 0x00A0A8
        RingHead -> 0xFCFCFC
        RingMid -> 0x78DCE8
        RingBg -> 0)
    | r == roleEmb = nesColour 0xFCA044
    | r == roleCore = nesColour 0xFCFCFC
    | r == roleLetters = nesColour (if rsLettersLit st then 0xFCFCFC else 0x24188C)
    | r == roleTeal = nesColour 0x00788A
    | otherwise = nesColour 0x24188C

-- | The sprite palettes: lead, harmony, bass (head, trail, centre), the
-- lights (centre, corners).
objColours :: [[Word8]]
objColours = map (map nesColour)
  [ [0xFCE0A8, 0xE45C10, 0xFCFCFC], [0xB8F8F8, 0x00A8A8, 0xFCFCFC], [0xD8B8F8, 0x6844FC, 0xFCFCFC], [0xA4E4FC, 0x0088B8, 0xFCFCFC] ]

-- * Shots

data Sprite = Sprite
  { spKey :: (Int, Int, Int, Part)
  , spX0, spY0 :: Int
  , spPat :: Tile         -- ^ two-bit values
  , spPal :: Int
  , spBehind :: Bool
  , spLost :: Int
  } deriving stock (Eq, Show)

data NesShot = NesShot
  { shLogo :: [Maybe Tile]
  , shWeave :: Int
  , shSprites :: [Sprite]
  , shBg :: [[Word8]]     -- ^ the four background palettes' colours
  }

shotAt :: Int -> NesShot
shotAt p = NesShot
  [ case c of
      Logo -> Just (logoTile b tx ty)
      Fabric _ _ -> Nothing
  | (c, (tx, ty)) <- zip cells [ (tx, ty) | ty <- [0, 8 .. h - 1], tx <- [0, 8 .. w - 1] ] ]
  (weaveCapped nesClock p) (spritesAt (if isJust (ptIdle t) then b + 4 else b) b) (bgColours (roleState b))
 where
  t = picTime nesClock p
  b = ptBeat t

spritesAt :: Double -> Double -> [Sprite]
spritesAt bs b = filter visible (map toSprite (M.toList groups))
 where
  px = sparkPixels nesGeo bs <> lightPixels nesGeo b crossings
  groups = M.fromListWith (flip (<>)) [ (spGroup p, [p]) | p <- px ]
  toSprite (k@(kind, v, _, _), ps) =
    let x0 = minimum (map spX ps)
        y0 = minimum (map spY ps)
        inBox p = spX p - x0 < 8 && spY p - y0 < 8
        kept = filter inBox ps
        val r | r `elem` [30, 32, 34, roleLightB] = 1
              | r `elem` [31, 33, 35, roleLightD] = 2
              | otherwise = 3
        pat = U.replicate 64 0 U.// [ ((spY p - y0) * 8 + spX p - x0, val (spRole p)) | p <- kept ]
        behind = or [ gLogo nesGeo U.! (spY p * w + spX p) | p <- kept, spX p >= 0, spX p < w, spY p >= 0, spY p < h ]
    in Sprite k x0 y0 pat (if kind == 1 then 3 else v) behind (length ps - length kept)
  visible s = spX0 s >= 0 && spX0 s < w && spY0 s >= 1 && spY0 s < h

crossings :: U.Vector Word8
crossings = compose nesGeo 0 WeaveCost

-- * Screen

-- | The NES colour of each pixel (as the PPU's output: no emphasis).
-- Sprites: the earlier sprite wins; behind the background's non-zero
-- pixels if so marked. In the lead-in, all black.
renderShot :: Maybe NesShot -> U.Vector Word16
renderShot Nothing = U.replicate (w * h) 0x0F
renderShot (Just sh) = U.generate (w * h) px
 where
  tiles = U.concat (zipWith bgTile cells (shLogo sh))
  bgTile (Fabric ph s) _ = fabricTile s (min 2 (weaveLevel ph (shWeave sh)))
  bgTile Logo (Just t) = t
  bgTile Logo Nothing = U.replicate 64 0
  bgAt x y = tiles U.! (((y `div` 8) * 32 + x `div` 8) * 64 + (y `mod` 8) * 8 + x `mod` 8)
  px j =
    let (y, x) = j `divMod` w
        bg = bgAt x y
        pal = areaPalette U.! ((y `div` 16) * 16 + x `div` 16)
        bgCol = if bg == 0 then 0x0F else (shBg sh !! pal) !! (fromIntegral bg - 1)
        obj = [ (c, s) | s <- shSprites sh
                       , let dx = x - spX0 s
                             dy = y - spY0 s
                       , dx >= 0, dx < 8, dy >= 0, dy < 8
                       , let c = spPat s U.! (dy * 8 + dx), c /= 0 ]
    in fromIntegral $ case obj of
         ((c, s) : _) | not (spBehind s && bg /= 0) -> (objColours !! spPal s) !! (fromIntegral c - 1)
         _ -> bgCol
