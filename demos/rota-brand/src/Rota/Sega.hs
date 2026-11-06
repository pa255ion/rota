{-|
Rendition B on Sega's mode 4 VDP (docs/brand.md, "Rendition B"; the Game
Gear's step 5, the Master System's M6): what the ROM shows, frame by
frame, as tiles, colours and sprites, for either screen ('Target': the
tile lab's machine, its look, its colour format). "Rota.Gg" and "Rota.Sms"
are the two machines; "Rota.Sega.Rom" turns a target's pictures into the
ROM's streams. (Rota.Gg's code, made a target's in M6; the Game Gear's
emitted data is unchanged, its golden says so.)

__Colours.__ The VDP's colour RAM holds every role at once: background
palette indices 0 (the background), 1 to 5 (the ring's stages), 6 (the
lambdas), 7 (the core), 8 (the letters), 9 and 10 (the fabric, bright and
dim); the sprite palette 1 to 9 (the sparks' heads and trails, the spark's
centre, the lights). Each is the tile lab's colour for its role at that
moment in the target's look (the Game Gear: @LOOK.base@ rounded to 4 bits
a channel, as the lab's @qbits@; the Master System: @LOOK.sms@, 2 bits a
channel), so the ring's and letters' looks are colour writes, not tiles.

__The background.__ A cell is a fabric cell, (phase, shape), drawn in one
of the slots whose graphics the ROM redraws as the fabric steps (as on the
Game Boy), a logo cell with a tile of its own (redrawn only while the
emblem grows), or blank. Logo cells have the name table's priority bit,
so a sprite shows over their background pixels and behind the logo's, as
the lab draws sparks behind the logo.

__Sprites.__ Each spark is a head and a trail sprite, each light one
sprite (the lab's groups, as on the Game Boy); sparks first in the table,
so they win where they meet a light.

__The lead-in__ is dark: the display is off, the backdrop the background.
-}
module Rota.Sega
  ( -- * Targets
    Target (tMachine, tClock, tLook, tSprites, tColour, tShown, tGeo)
  , target
  , tW, tH
  , tCols
    -- * Cells
  , Cell (..)
  , cells
  , cellPositions
  , fabricSlots
  , fabricTile
    -- * Colours
  , Rgb12
  , rgb12
  , bgColours
  , spriteColours
  , spriteIndex
    -- * Pictures
  , Tile
  , logoTile
  , Shot (..)
  , shotAt
  , Sprite (..)
  , spritesAt
    -- * Screen (the model the layer 3 tests compare with)
  , renderShot
  ) where

import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.Maybe (isJust)
import qualified Data.Map.Strict as M
import qualified Data.Vector.Unboxed as U
import Data.Word (Word16, Word8)
import Numeric (readHex)

import Rota.Clock
import Rota.Lab
import Rota.Score (jsRound)

-- * Targets

-- | A machine with Sega's VDP: the tile lab's machine (its screen, ring
-- and letters), its clock, its look's colour per role, its sprite
-- palette's nine colours (as the lab's @LOOK@), its colours as the
-- screen shows them, and which sprites (by their top-left pixel) the VDP
-- can show where the lab puts them (the others are left out, in the ROM
-- and its model alike); with what follows from them, made once ('target':
-- a target is a top-level value, so its geometry and cells are shared as
-- Rota.Gg's were).
data Target = Target
  { tMachine :: Machine
  , tClock   :: Clock
  , tLook    :: String -> String
  , tSprites :: [String]
  , tColour  :: String -> Rgb12
  , tShown   :: Int -> Int -> Bool
  , tGeo     :: Geo
  , tCells   :: [Cell]
  , tSlots   :: [(Int, Shape)]
  , tCellV   :: U.Vector Bool
  , tLights  :: U.Vector Word8   -- ^ the picture the lights are placed on
  }

target :: Machine -> Clock -> (String -> String) -> [String] -> (String -> Rgb12) -> (Int -> Int -> Bool) -> Target
target m ck lk sps col shown = t
 where
  g = geometry m
  cs = [ cellAt g (mW m) tx ty | (tx, ty) <- cellPositions t ]
  t = Target
    { tMachine = m, tClock = ck, tLook = lk, tSprites = sps, tColour = col, tShown = shown
    , tGeo = g
    , tCells = cs
    , tSlots = M.keys (M.fromList [ ((ph, sh), ()) | Fabric ph sh <- cs ])
    , tCellV = U.fromList [ c == Logo | c <- cs ]
    , tLights = compose g 0 WeaveCost }

tW, tH, tCols :: Target -> Int
tW = mW . tMachine
tH = mH . tMachine
tCols t = tW t `div` 8

-- * Cells

data Cell = Fabric Int Shape | Logo | Blank
  deriving stock (Eq, Ord, Show)

-- | The cells, row by row.
cells :: Target -> [Cell]
cells = tCells

-- | Each cell's top-left pixel, row by row.
cellPositions :: Target -> [(Int, Int)]
cellPositions t = [ (tx, ty) | ty <- [0, 8 .. tH t - 1], tx <- [0, 8 .. tW t - 1] ]

cellAt :: Geo -> Int -> Int -> Int -> Cell
cellAt g w tx ty
  | logo = Logo
  | isTraceCell g tx ty =
      Fabric (weavePhase g tx ty) (case (traceH g ty, traceV g tx) of
        (True, True) -> SX
        (True, False) -> SH
        _ -> SV)
  | otherwise = Blank
 where
  logo = or [ gLogo g U.! (y * w + x) | y <- [ty .. ty + 7], x <- [tx .. tx + 7] ]

-- | The fabric slots in use, in order.
fabricSlots :: Target -> [(Int, Shape)]
fabricSlots = tSlots

-- | 64 colour indices, rows top first.
type Tile = U.Vector Word8

-- | A fabric shape at a level (0 the pulse, 1 the tail, 2 dotted): the
-- lab's @weaveTile@, bright (index 9) at level 0, dim (10) after.
fabricTile :: Shape -> Int -> Tile
fabricTile sh lv = U.generate 64 $ \i ->
  let (ly, lx) = i `divMod` 8
      hz = sh /= SV
      vt = sh /= SH
      onH = hz && (if lv <= 1 then ly == 3 || (lv == 0 && ly == 4) else ly == 3 && even lx)
      onV = vt && (if lv <= 1 then lx == 3 || (lv == 0 && lx == 4) else lx == 3 && even ly)
      node = hz && vt && lx >= 2 && lx <= 4 && ly >= 2 && ly <= 4
  in if onH || onV || node then (if lv == 0 then 9 else 10) else 0

-- | A logo cell's tile at a beat: the roles as background indices (the
-- ring's stages 1 to 5, the lambdas 6 once the emblem has grown over
-- them, the core 7, the letters 8).
logoTile :: Target -> Double -> Int -> Int -> Tile
logoTile t b tx ty = U.generate 64 $ \i ->
  let (ly, lx) = i `divMod` 8 in role (tx + lx) (ty + ly)
 where
  g = tGeo t
  er = embRadius g b
  role x y =
    let j = y * tW t + x
    in if gCore g U.! j then 7
       else if gEmb g U.! j && gEmbCell g U.! j <= er then 6
       else if gRing g U.! j /= 0 then gRing g U.! j
       else if gLetters g U.! j then 8
       else 0

-- * Colours

-- | A colour as the screen shows it: 0000BBBBGGGGRRRR (the Game Gear's
-- colour RAM format; the Master System's 2-bit channels as 0, 5, 10, 15).
type Rgb12 = Word16

-- | "#RRGGBB" rounded to 4 bits a channel.
rgb12 :: String -> Rgb12
rgb12 ('#' : hex) = case readHex hex of
  [(n, "")] ->
    let c sh = jsRound (fromIntegral ((n `shiftR` sh) .&. 0xFF :: Int) / 255 * 15 :: Double)
    in fromIntegral (c 16 .|. (c 8 `shiftL` 4) .|. (c 0 `shiftL` 8))
  _ -> error ("rgb12: " <> hex)
rgb12 s = error ("rgb12: " <> s)

-- | Background indices 0 to 10 at a beat.
bgColours :: Target -> Double -> [Rgb12]
bgColours t b = map (tColour t . tLook t) $
  ["bg"] <> map ring (rsRing st)
  <> ["emb", if rsCoreFlash st then "sparkCore" else "core", if rsLettersLit st then "let" else "letDim", "fabOn", "fabDim"]
 where
  st = roleState b
  ring l = case l of
    RingOff -> "off"
    RingOn -> "on"
    RingHead -> "head"
    RingMid -> "mid"
    RingBg -> "bg"

-- | The sprite palette, indices 1 to 9 (index 0 is transparent).
spriteColours :: Target -> [Rgb12]
spriteColours t = map (tColour t) (tSprites t)

-- | A sprite-layer role's index in the sprite palette.
spriteIndex :: Word8 -> Word8
spriteIndex r
  | r >= 30 && r <= 35 = r - 29
  | r == roleSparkCore = 7
  | r == roleLightB = 8
  | r == roleLightD = 9
  | otherwise = error ("spriteIndex: " <> show r)

-- * Pictures

-- | One picture frame: each logo cell's tile (cells in order, Nothing for
-- the others), the fabric's step, the background colours, the sprites.
data Shot = Shot
  { shLogo :: [Maybe Tile]
  , shWeave :: Int
  , shBg :: [Rgb12]
  , shSprites :: [Sprite]
  }

shotAt :: Target -> Int -> Shot
shotAt tg p = Shot
  [ case c of
      Logo -> Just (logoTile tg b tx ty)
      _ -> Nothing
  | (c, (tx, ty)) <- zip (cells tg) (cellPositions tg) ]
  (weaveCapped (tClock tg) p) (bgColours tg b) (spritesAt tg (if isJust (ptIdle t) then b + 4 else b) b)
 where
  t = picTime (tClock tg) p
  b = ptBeat t

-- | A sprite: its group, its top-left pixel (on the screen), its 8 × 8
-- pattern (sprite palette indices), and the pixels its box had to leave
-- out.
data Sprite = Sprite
  { spKey :: (Int, Int, Int, Part)
  , spX0, spY0 :: Int
  , spPat :: Tile
  , spLost :: Int
  } deriving stock (Eq, Show)

-- | The sparks (drawn as at beat @bs@: the idle loop draws them a loop
-- later, as the Game Boy) and the lights at a beat, sparks first.
spritesAt :: Target -> Double -> Double -> [Sprite]
spritesAt t bs b = filter (\s -> tShown t (spX0 s) (spY0 s)) (map toSprite (M.toList groups))
 where
  g = tGeo t
  px = sparkPixels g bs <> lightPixels g b (tLights t)
  groups = M.fromListWith (flip (<>)) [ (spGroup p, [p]) | p <- px ]
  toSprite (k, ps) =
    let x0 = minimum (map spX ps)
        y0 = minimum (map spY ps)
        inBox p = spX p - x0 < 8 && spY p - y0 < 8
        kept = filter inBox ps
        pat = U.replicate 64 0 U.// [ ((spY p - y0) * 8 + spX p - x0, spriteIndex (spRole p)) | p <- kept ]
    in Sprite k x0 y0 pat (length ps - length kept)

-- * Screen

-- | The screen's colours. The background from each cell's tile (fabric
-- cells at the step's levels); sprites in order, the first opaque one
-- winning, behind a logo cell's non-zero pixels; Nothing (the lead-in):
-- the backdrop, the background colour.
renderShot :: Target -> Maybe Shot -> U.Vector Rgb12
renderShot t Nothing = U.replicate (tW t * tH t) (tColour t (tLook t "bg"))
renderShot t (Just sh) = U.generate (w * tH t) px
 where
  w = tW t
  cs = cells t
  bgv = U.concat (zipWith tileOf cs (shLogo sh))
  tileOf (Fabric ph s) _ = fabricTile s (min 2 (weaveLevel ph (shWeave sh)))
  tileOf Logo (Just tl) = tl
  tileOf _ _ = U.replicate 64 0
  cellV = tCellV t
  bgCol = U.fromList (shBg sh)
  spCol = U.fromList (spriteColours t)
  px j =
    let (y, x) = j `divMod` w
        cell = (y `div` 8) * tCols t + x `div` 8
        bg = bgv U.! (cell * 64 + (y `mod` 8) * 8 + x `mod` 8)
        obj = [ c | s <- shSprites sh
                  , let dx = x - spX0 s
                        dy = y - spY0 s
                  , dx >= 0, dx < 8, dy >= 0, dy < 8
                  , let c = spPat s U.! (dy * 8 + dx), c /= 0 ]
    in case obj of
         (c : _) | not (cellV U.! cell && bg /= 0) -> spCol U.! fromIntegral (c - 1)
         _ -> bgCol U.! fromIntegral bg
