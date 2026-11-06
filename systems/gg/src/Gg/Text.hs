{-|
The screen as text, for test ROMs that print their results with an ASCII
font loaded at tile 0 (tile number + 20h = the character), as FluBBa's
SMS VDP Test does: the name table's 24 rows of 32 cells.
-}
module Gg.Text
  ( nameTable
  , screenText
  ) where

import Prelude

import Control.Monad (forM)
import Control.Monad.ST (ST)
import Data.Bits ((.&.))

import Gg.Spec (Machine, vdpRegister, vramByte)

-- | The tile numbers of the name table (R2's base), 24 rows of 32.
nameTable :: Machine s -> ST s [[Int]]
nameTable m = do
  r2 <- vdpRegister m 2
  let base = (fromIntegral r2 .&. 0x0E) * 0x400 :: Int
  forM [0 .. 23] $ \row -> forM [0 .. 31] $ \col -> do
    lo <- vramByte m (base + (row * 32 + col) * 2)
    hi <- vramByte m (base + (row * 32 + col) * 2 + 1)
    pure (fromIntegral lo + 256 * (fromIntegral hi .&. 1))

-- | The name table as text ('?' for tiles outside the font).
screenText :: Machine s -> ST s [String]
screenText m = map (map ch) <$> nameTable m
 where
  ch t = if t < 0x5F then toEnum (t + 0x20) else '?'
