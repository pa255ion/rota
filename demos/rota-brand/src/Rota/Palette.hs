{-|
The brand's colours and each machine's logo style (docs/brand.md,
"Palette"; the audition page's @STYLE@ table).
-}
module Rota.Palette
  ( -- * Colours
    Rgb (..)
  , rgbHex
  , ink
  , inkScreen
  , shadow
  , amber
  , teal
  , tealBright
  , violet
  , paper
    -- * Machines
  , chip8Style
  ) where

import Data.Word (Word8)
import Numeric (readHex)

import Rota.Logo (Style (..))
import Rota.Score (Voice (..))

data Rgb = Rgb !Word8 !Word8 !Word8
  deriving stock (Eq, Ord, Show)

-- | "#RRGGBB".
rgbHex :: String -> Rgb
rgbHex ['#', r1, r2, g1, g2, b1, b2] = Rgb (h [r1, r2]) (h [g1, g2]) (h [b1, b2])
 where
  h s = case readHex s of
    [(v, "")] -> v
    _ -> error ("rgbHex: " <> s)
rgbHex s = error ("rgbHex: " <> s)

ink, inkScreen, shadow, amber, teal, tealBright, violet, paper :: Rgb
ink = rgbHex "#14111C"
inkScreen = rgbHex "#0E0B14"
shadow = rgbHex "#453A62"
amber = rgbHex "#F7A41D"
teal = rgbHex "#22A699"
tealBright = rgbHex "#3CC7B9"
violet = rgbHex "#9C8FD0"
paper = rgbHex "#F4ECD8"

-- | One bit, amber.
chip8Style :: Style Rgb
chip8Style = Style
  { stOff = Nothing, stBuilt = amber, stHead = amber
  , stT1 = Nothing, stT2 = Nothing, stIdle = Nothing
  , stDip = Just Nothing
  , stLam = amber, stCore = amber, stWord = amber, stWordOff = Nothing
  , stThread = const amber
  , stOneBit = True }
