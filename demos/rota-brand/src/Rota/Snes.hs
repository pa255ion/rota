{-|
Rendition B on the Super Nintendo (M7 step 7.13; docs/brand.md, "Rendition
B" and "The Super Nintendo as built"): "Rota.Sega"'s pictures (its model is
the tile machine's, not the VDP's own: tiles, colours, sprites) on the tile
lab's Super Nintendo, the whole 256 × 224 screen, ring radius 64 and the
large letters as the Master System's, in the lab's base look rounded to 4
bits a channel (as the Game Gear's: the SNES shows 5, and 4 widen to them
exactly). As on the Master System, no sprite is shown partly left of column
0 (it shows once wholly on): the ROM keeps a sprite's X in one byte.
"Rota.Snes.Rom" turns it into the ROM's streams; the clock is the SNES's
NTSC frame ("Rota.Sound.Snes").
-}
module Rota.Snes
  ( snesTarget
  , snesClock
  , snesW, snesH
  , cgram15
  ) where

import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.Word (Word16)

import Rota.Clock
import Rota.Lab (snesM)
import Rota.Sega (Rgb12, Target (..), rgb12)
import qualified Rota.Sega as S
import Rota.Sound.Snes (snesFps)

snesTarget :: Target
snesTarget = S.target snesM (clock snesFps) look
  [ "#FFE29A", "#C98A1E", "#B4F7EF", "#1FA597", "#DCD0FF", "#7A68C8"   -- sparks: lead, harmony, bass (head, trail)
  , "#FFFFFF", "#BFF5EE", "#3C8F87" ]                                  -- a spark's centre, a light's centre and corners
  rgb12 (\x _ -> x >= 0)   -- no sprite partly left of column 0: it shows once wholly on

snesClock :: Clock
snesClock = tClock snesTarget

snesW, snesH :: Int
snesW = S.tW snesTarget
snesH = S.tH snesTarget

-- | A screen colour (4 bits a channel) as CGRAM's 15-bit word (0BBBBBGG
-- GGGRRRRR), each channel's 4 bits widened to 5 (c × 2 + c >> 3: 0 to 0,
-- 15 to 31, as the screen's 5-bit channel shows a 4-bit one).
cgram15 :: Rgb12 -> Word16
cgram15 c = ch 0 .|. (ch 4 `shiftL` 5) .|. (ch 8 `shiftL` 10)
 where
  ch sh = let v = (fromIntegral c `shiftR` sh) .&. 0xF :: Word16 in (v `shiftL` 1) .|. (v `shiftR` 3)

-- | The tile lab's base look (LOOK.base), as the Game Gear's.
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
