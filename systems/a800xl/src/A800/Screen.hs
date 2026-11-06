{-|
Test hook: the OS's text screen as text (M4 step 4.0; docs/systems/
atari800xl.md, "Test hooks"). A pure function over a memory reader, so the
reference model, the Clash core's simulations and a RAM dump all read a
program's output the same way, without video.

The OS keeps the screen editor's display (GRAPHICS 0: 24 lines of 40
characters, one byte each) at the address in SAVMSC ($58, $59), in
ANTIC's internal character codes ("Mapping the Atari", $58; De Re Atari,
appendix). Internal codes map to ATASCII by moving three blocks of 32:
$00-$3F are ATASCII $20-$5F, $40-$5F are $00-$1F (graphics characters),
$60-$7F stay. Bit 7 is inverse video (the cursor is an inverse space);
'screenLines' drops it.

ATASCII $20-$5F and $61-$7A are ASCII; everything else (graphics
characters, $60, $7B-$7F) shows as @'.'@ except $7C (@'|'@).

The screen editor wraps a logical line over physical rows between the
margins LMARGN and RMARGN ($52, $53; 2 and 39 after start-up), so text
can break anywhere ("Pa" at the end of a row, "ss" at the start of the
next). 'screenFlat' joins the rows' text between the margins into one
string, which is what to search for words.
-}
module A800.Screen
  ( savmsc
  , screenBase
  , internalToAtascii
  , atasciiToAscii
  , screenLines
  , screenText
  , screenFlat
  , screenHas
  ) where

import Prelude

import Data.Bits (shiftL, (.&.), (.|.))
import Data.List (dropWhileEnd, isInfixOf)
import Data.Word

-- | Zero-page pointer to the screen editor's display memory.
savmsc :: Word16
savmsc = 0x58

screenBase :: (Word16 -> Word8) -> Word16
screenBase rd = fromIntegral (rd savmsc) .|. (fromIntegral (rd (savmsc + 1)) `shiftL` 8)

internalToAtascii :: Word8 -> Word8
internalToAtascii c = case c .&. 0x60 of
  0x00 -> c + 0x20
  0x20 -> c + 0x20
  0x40 -> c - 0x40
  _    -> c

atasciiToAscii :: Word8 -> Char
atasciiToAscii a
  | a >= 0x20 && a <= 0x5F = toEnum (fromIntegral a)
  | a >= 0x61 && a <= 0x7A = toEnum (fromIntegral a)
  | a == 0x7C = '|'
  | otherwise = '.'

-- | The 24 lines, trailing blanks dropped.
screenLines :: (Word16 -> Word8) -> [String]
screenLines rd =
  [ dropWhileEnd (== ' ')
      [ atasciiToAscii (internalToAtascii (rd (base + 40 * r + c) .&. 0x7F)) | c <- [0 .. 39] ]
  | r <- [0 .. 23] ]
 where
  base = screenBase rd

-- | The lines joined with newlines, trailing empty lines dropped.
screenText :: (Word16 -> Word8) -> String
screenText = unlines . dropWhileEnd null . screenLines

-- | Every row's text between the margins, joined (blank runs kept, so a
-- line ending early leaves spaces before the next).
screenFlat :: (Word16 -> Word8) -> String
screenFlat rd = concat
  [ [ atasciiToAscii (internalToAtascii (rd (base + 40 * r + c) .&. 0x7F)) | c <- [lm .. rm] ]
  | r <- [0 .. 23] ]
 where
  base = screenBase rd
  lm = fromIntegral (min 39 (rd 0x52))
  rm = fromIntegral (min 39 (max (rd 0x52) (rd 0x53)))

-- | Whether the screen's text contains the string (across row breaks).
screenHas :: String -> (Word16 -> Word8) -> Bool
screenHas s rd = s `isInfixOf` screenFlat rd
