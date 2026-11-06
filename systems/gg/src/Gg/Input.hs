{-|
Input scripts for @gg-run@ and the goldens: lines "FRAME BUTTONS"; the
buttons hold from that frame until the next line. BUTTONS is "-" (none)
or letters: U D L R (pad), 1 2 (buttons), S (Start; Pause on a Master
System), u d l r 3 4 (the Master System's pad 2: directions, buttons 1
and 2). '#' starts a comment.
-}
module Gg.Input
  ( parseInput
  , inputAt
  ) where

import Prelude

import Data.Bits ((.|.))
import Data.List (sortOn)
import Text.Read (readMaybe)

import Gg.Spec (Input (..), noInput, pad1, pad2, padDown, padLeft, padRight, padUp)

-- | The script as (first frame, buttons), sorted; or the first error
-- with its line number.
parseInput :: String -> Either String [(Int, Input)]
parseInput src = sortOn fst <$> mapM line numbered
 where
  numbered = [ (n, ws) | (n, l) <- zip [1 :: Int ..] (lines src), let ws = words (takeWhile (/= '#') l), not (null ws) ]
  line (n, [fr, bs]) = case readMaybe fr of
    Just k -> (\i -> (k, i)) <$> buttons n bs
    Nothing -> Left (show n <> ": not a frame number: " <> fr)
  line (n, _) = Left (show n <> ": expected FRAME BUTTONS")
  buttons _ "-" = Right noInput
  buttons n bs = foldl (\acc ch -> acc >>= add n ch) (Right noInput) bs
  add n ch i = case ch of
    'U' -> Right i { inPad = inPad i .|. padUp }
    'D' -> Right i { inPad = inPad i .|. padDown }
    'L' -> Right i { inPad = inPad i .|. padLeft }
    'R' -> Right i { inPad = inPad i .|. padRight }
    '1' -> Right i { inPad = inPad i .|. pad1 }
    '2' -> Right i { inPad = inPad i .|. pad2 }
    'S' -> Right i { inStart = True }
    'u' -> Right i { inPad2 = inPad2 i .|. padUp }
    'd' -> Right i { inPad2 = inPad2 i .|. padDown }
    'l' -> Right i { inPad2 = inPad2 i .|. padLeft }
    'r' -> Right i { inPad2 = inPad2 i .|. padRight }
    '3' -> Right i { inPad2 = inPad2 i .|. pad1 }
    '4' -> Right i { inPad2 = inPad2 i .|. pad2 }
    _ -> Left (show n <> ": unknown button " <> [ch])

-- | The buttons in force on frame @k@.
inputAt :: [(Int, Input)] -> Int -> Input
inputAt script k = case [ i | (fr, i) <- script, fr <= k ] of
  [] -> noInput
  is -> last is
