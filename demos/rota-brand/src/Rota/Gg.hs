{-|
Rendition B on the Game Gear (PLAN.md, side track J, step 5; docs/brand.md,
"Rendition B"): "Rota.Sega"'s pictures on the Game Gear's 160 × 144
window, the tile lab's base look rounded to 4 bits a channel (@LOOK.base@,
the lab's @qbits@). "Rota.Gg.Rom" turns it into the ROM's streams.
-}
module Rota.Gg
  ( ggTarget
  , ggGeo
  , ggClock
  , ggW, ggH
    -- * Cells
  , Cell (..)
  , cells
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
  , GgShot
  , Shot (..)
  , shotAt
  , Sprite (..)
  , spritesAt
    -- * Screen (the model the layer 3 tests compare with)
  , renderShot
  ) where

import qualified Data.Vector.Unboxed as U

import Rota.Clock
import Rota.Lab (Geo, Shape, ggM)
import Rota.Sega (Cell (..), Rgb12, Shot (..), Sprite (..), Target (..), Tile, fabricTile, rgb12, spriteIndex)
import qualified Rota.Sega as S
import Rota.Sound.Gg (ggFps)

ggTarget :: Target
ggTarget = S.target ggM (clock ggFps) look
  [ "#FFE29A", "#C98A1E", "#B4F7EF", "#1FA597", "#DCD0FF", "#7A68C8"   -- sparks: lead, harmony, bass (head, trail)
  , "#FFFFFF", "#BFF5EE", "#3C8F87" ]                                  -- a spark's centre, a light's centre and corners
  rgb12 (\_ _ -> True)

ggGeo :: Geo
ggGeo = S.tGeo ggTarget

ggClock :: Clock
ggClock = tClock ggTarget

ggW, ggH :: Int
ggW = S.tW ggTarget
ggH = S.tH ggTarget

type GgShot = Shot

-- | The tile lab's base look (LOOK.base).
look :: String -> String
look k = case k of
  "bg" -> "#14111C"
  "off" -> "#453A62"
  "on" -> "#22A699"
  "head" -> "#F4ECD8"
  "mid" -> "#7FD3C9"
  "emb" -> "#F7A41D"
  "core" -> "#F4ECD8"
  "letDim" -> "#2A2340"
  "let" -> "#F4ECD8"
  "fabOn" -> "#1E6E68"
  "fabDim" -> "#2C2546"
  "lightB" -> "#BFF5EE"
  "lightD" -> "#3C8F87"
  "sparkCore" -> "#FFFFFF"
  _ -> error ("look: " <> k)

-- | The 20 × 18 cells, row by row.
cells :: [Cell]
cells = S.cells ggTarget

fabricSlots :: [(Int, Shape)]
fabricSlots = S.fabricSlots ggTarget

bgColours :: Double -> [Rgb12]
bgColours = S.bgColours ggTarget

spriteColours :: [Rgb12]
spriteColours = S.spriteColours ggTarget

logoTile :: Double -> Int -> Int -> Tile
logoTile = S.logoTile ggTarget

shotAt :: Int -> Shot
shotAt = S.shotAt ggTarget

spritesAt :: Double -> Double -> [Sprite]
spritesAt = S.spritesAt ggTarget

renderShot :: Maybe Shot -> U.Vector Rgb12
renderShot = S.renderShot ggTarget
