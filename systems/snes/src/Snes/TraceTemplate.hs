{-|
The trace line's template ("Snes.Machine", "The trace"; the freeze hunt,
2026-10-20), in a module of its own so that "Snes.Machine" can make it a
'Vec' at compile time (Template Haskell's stage restriction): each of the
line's 72 characters is a literal, or a hex digit of one of the 24 debug
bytes ("Snes.Core"'s 'debugBytes'). A line built by '++' of many small
vectors went past Clash's specialisation limit.
-}
module Snes.TraceTemplate
  ( traceTemplate
  ) where

import Prelude

import Data.Word (Word8)

-- | (a digit, its value): a literal character, or the nibble number k
-- (byte k / 2, the high digit for an even k).
traceTemplate :: [(Bool, Word8)]
traceTemplate = concatMap seg
  [ L "c ", H 0, H 1, H 2, L " p", H 3, L " n", H 4
  , L " i ", H 5, H 6, H 7, H 8
  , L " o ", H 9, H 10, H 11, H 12
  , L " s ", H 13, H 14, L " f", H 15
  , L " b ", H 16, L " ", H 17, H 18, H 19, L " ", H 20, H 21, H 22, L " ", H 23
  , L "\n" ]
 where
  seg (L s) = [ (False, fromIntegral (fromEnum c)) | c <- s ]
  seg (H b) = [(True, 2 * b), (True, 2 * b + 1)]

data Seg = L String | H Word8
