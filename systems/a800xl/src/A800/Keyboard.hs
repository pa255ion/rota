{-|
The 800XL keyboard as POKEY scans it (M4 step 4.4): each key is a
position 0-63 in the matrix, its key code (KBCODE bits 0-5); SHIFT,
CONTROL and BREAK are separate lines (HRM 5.8, "Keyboard scan"; the
codes as in the Atari Home Computer Technical Reference Notes and
"Mapping the Atari", $D209).

'charKeys' gives the keys that type an ASCII character on a stock OS
(unshifted or with SHIFT), for tests that type into BASIC.
-}
module A800.Keyboard
  ( keyCode
  , charKeys
  , keyNames
  ) where

import Prelude

import Data.Char (toUpper)
import Data.Word

-- | (name, key code): the 57 keys of the matrix (codes not listed are not
-- on the 800XL's keyboard).
keyNames :: [(String, Word8)]
keyNames =
  [ ("L", 0x00), ("J", 0x01), (";", 0x02), ("K", 0x05), ("+", 0x06), ("*", 0x07)
  , ("O", 0x08), ("P", 0x0A), ("U", 0x0B), ("RETURN", 0x0C), ("I", 0x0D), ("-", 0x0E), ("=", 0x0F)
  , ("V", 0x10), ("HELP", 0x11), ("C", 0x12), ("B", 0x15), ("X", 0x16), ("Z", 0x17)
  , ("4", 0x18), ("3", 0x1A), ("6", 0x1B), ("ESC", 0x1C), ("5", 0x1D), ("2", 0x1E), ("1", 0x1F)
  , (",", 0x20), ("SPACE", 0x21), (".", 0x22), ("N", 0x23), ("M", 0x25), ("/", 0x26), ("INVERSE", 0x27)
  , ("R", 0x28), ("E", 0x2A), ("Y", 0x2B), ("TAB", 0x2C), ("T", 0x2D), ("W", 0x2E), ("Q", 0x2F)
  , ("9", 0x30), ("0", 0x32), ("7", 0x33), ("BACKSPACE", 0x34), ("8", 0x35), ("<", 0x36), (">", 0x37)
  , ("F", 0x38), ("H", 0x39), ("D", 0x3A), ("CAPS", 0x3C), ("G", 0x3D), ("S", 0x3E), ("A", 0x3F)
  ]

keyCode :: String -> Maybe Word8
keyCode n = lookup n keyNames

-- | The key and whether SHIFT is held, for a character (letters type as
-- capitals: the OS starts in caps mode).
charKeys :: Char -> Maybe (Word8, Bool)
charKeys c = case c of
  ' ' -> plain "SPACE"
  '\n' -> plain "RETURN"
  '"' -> shifted "2"
  '!' -> shifted "1"
  '#' -> shifted "3"
  '$' -> shifted "4"
  '%' -> shifted "5"
  '&' -> shifted "6"
  '\'' -> shifted "7"
  '@' -> shifted "8"
  '(' -> shifted "9"
  ')' -> shifted "0"
  '?' -> shifted "/"
  ':' -> shifted ";"
  '_' -> shifted "-"
  '|' -> shifted "="
  '\\' -> shifted "+"
  '^' -> shifted "*"
  '[' -> shifted ","
  ']' -> shifted "."
  _ -> plain [toUpper c]
 where
  plain n = (\k -> (k, False)) <$> keyCode n
  shifted n = (\k -> (k, True)) <$> keyCode n
