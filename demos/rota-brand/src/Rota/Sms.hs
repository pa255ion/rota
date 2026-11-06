{-|
Rendition B on the Master System (M6; docs/brand.md, "Rendition B" and
"The Master System as built"): "Rota.Sega"'s pictures on the tile lab's
Master System, the whole 256 × 192 screen, ring radius 64, the large
letters, the lab's own look (@LOOK.sms@: 2 bits a channel, colours the
screen shows as 0, 5, 10, 15 a channel). The VDP can't show a sprite
partly left of column 0 (without moving every sprite 8 pixels): a spark
coming in there shows once it is wholly on the screen. "Rota.Sms.Rom" turns it into the
ROM's streams. The clock and the chime are the Game Gear's (the same
NTSC VDP and SN76489).
-}
module Rota.Sms
  ( smsTarget
  , smsClock
  , smsW, smsH
  , rgb6
  , cram6
  ) where

import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.Word (Word8)
import Numeric (readHex)

import Rota.Clock
import Rota.Lab (smsM)
import Rota.Score (jsRound)
import Rota.Sega (Rgb12, Target (..))
import qualified Rota.Sega as S
import Rota.Sound.Gg (ggFps)

smsTarget :: Target
smsTarget = S.target smsM (clock ggFps) look
  [ "#FFFF55", "#AA5500", "#AAFFFF", "#00AAAA", "#FFAAFF", "#5555AA"   -- sparks: lead, harmony, bass (head, trail)
  , "#FFFFFF", "#AAFFFF", "#00AAAA" ]                                  -- a spark's centre, a light's centre and corners
  rgb6 (\x _ -> x >= 0)   -- no sprite partly left of column 0: it shows once wholly on

smsClock :: Clock
smsClock = tClock smsTarget

smsW, smsH :: Int
smsW = S.tW smsTarget
smsH = S.tH smsTarget

-- | "#RRGGBB" rounded to 2 bits a channel, as the screen shows it (each
-- channel 0, 5, 10 or 15: "Gg.Spec"'s Master System colours).
rgb6 :: String -> Rgb12
rgb6 ('#' : hex) = case readHex hex of
  [(n, "")] ->
    let c sh = 5 * jsRound (fromIntegral ((n `shiftR` sh) .&. 0xFF :: Int) / 255 * 3 :: Double)
    in fromIntegral (c 16 .|. (c 8 `shiftL` 4) .|. (c 0 `shiftL` 8))
  _ -> error ("rgb6: " <> hex)
rgb6 s = error ("rgb6: " <> s)

-- | A screen colour as the Master System's colour RAM byte (--BBGGRR).
cram6 :: Rgb12 -> Word8
cram6 c = fromIntegral (ch 0 .|. (ch 4 `shiftL` 2) .|. (ch 8 `shiftL` 4))
 where
  ch sh = ((c `shiftR` sh) .&. 0xF) `div` 5

-- | The tile lab's Master System look (LOOK.sms).
look :: String -> String
look k = case k of
  "bg" -> "#000000"
  "off" -> "#000055"
  "on" -> "#00AAAA"
  "head" -> "#FFFFAA"
  "mid" -> "#55AAAA"
  "emb" -> "#FFAA00"
  "core" -> "#FFFFAA"
  "letDim" -> "#000055"
  "let" -> "#FFFFAA"
  "fabOn" -> "#005555"
  "fabDim" -> "#000055"
  "lightB" -> "#AAFFFF"
  "lightD" -> "#00AAAA"
  "sparkCore" -> "#FFFFFF"
  _ -> error ("look: " <> k)
