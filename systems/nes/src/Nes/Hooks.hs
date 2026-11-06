{-|
Test hooks shared by the reference model and the Clash simulations (M3
step 3.0, docs/systems/nes.md "Test ROMs"): pure functions over what a
running machine shows, so every layer reads a test ROM's result the same
way.

* __Blargg's memory protocol__ ('blargg'): the newer Blargg ROMs keep
  their result in PRG RAM. $6001 to $6003 hold the signature DE B0 61
  once the protocol is live; $6000 is the status: $80 running, $81 "press
  reset now" (after at least 100 ms), anything else is the final result,
  0 = passed. Text output, zero-terminated, starts at $6004.
* __nestest log__ ('parseNestestLine'): kevtris's @nestest.log@, one line
  per instruction: PC, opcode bytes, disassembly, then
  @A:xx X:xx Y:xx P:xx SP:xx PPU:sss,ddd CYC:n@ (registers before the
  instruction, the PPU's scanline and dot, CPU cycles since power-on).
  Started at $C000 (the "automation" entry), the ROM runs every official
  and undocumented opcode without a screen.
* __Blargg's serial output__ ('serialText'): the older shell
  (@cpu_dummy_reads@, @dmc_dma_during_read4@) has no $6000 protocol. It
  prints to the screen and, at 57600 baud, through bit 0 of $4016: one
  write per bit, 8N1 (start bit 0, data LSB first, stop bit 1; idle 1).
  It prints "Passed" when all tests pass, "Failed" or "Error n" otherwise
  ('serialVerdict').
-}
module Nes.Hooks
  ( -- * Blargg
    Blargg (..)
  , blargg
  , blarggSignature
  , serialText
  , serialVerdict
    -- * nestest
  , NestestLine (..)
  , parseNestestLine
  , parseNestestLog
  ) where

import Prelude

import Data.Char (isDigit, isHexDigit, isSpace)
import Data.List (isPrefixOf, tails)
import Data.Word
import Numeric (readHex)

-- | What a Blargg ROM says through PRG RAM.
data Blargg
  = NoSignature          -- ^ the protocol isn't live (yet)
  | Running              -- ^ status $80
  | NeedsReset           -- ^ status $81: the test asks for a reset
  | Done !Word8 String   -- ^ final status (0 = passed) and the text
  deriving stock (Show, Eq)

blarggSignature :: [Word8]
blarggSignature = [0xDE, 0xB0, 0x61]

-- | Read the protocol through a memory reader for $6000 to $7FFF (the
-- text can run past $60FF).
blargg :: (Word16 -> Word8) -> Blargg
blargg rd
  | map rd [0x6001, 0x6002, 0x6003] /= blarggSignature = NoSignature
  | otherwise = case rd 0x6000 of
      0x80 -> Running
      0x81 -> NeedsReset
      s    -> Done s text
 where
  text = map (toEnum . fromIntegral) (takeWhile (/= 0) (map rd [0x6004 .. 0x7FFF]))

data NestestLine = NestestLine
  { nlPC       :: !Word16
  , nlA, nlX, nlY, nlP, nlSP :: !Word8
  , nlScanline :: !Int
  , nlDot      :: !Int
  , nlCycle    :: !Int
  } deriving stock (Show, Eq)

parseNestestLine :: String -> Either String NestestLine
parseNestestLine l = do
  pc <- hex (take 4 l)
  a <- field "A:"
  x <- field "X:"
  y <- field "Y:"
  p <- field "P:"
  sp <- field "SP:"
  (sl, dot) <- case after "PPU:" of
    Just r -> let (s1, r1) = span (\ch -> isDigit ch || isSpace ch) r
              in case r1 of
                   ',' : r2 -> Right (read (filter isDigit s1), read (takeWhile isDigit (dropWhile isSpace r2)))
                   _ -> Left ("bad PPU field: " <> l)
    Nothing -> Left ("no PPU field: " <> l)
  cyc <- maybe (Left ("no CYC field: " <> l)) (Right . read . takeWhile isDigit) (after "CYC:")
  pure (NestestLine pc a x y p sp sl dot cyc)
 where
  after k = case [drop (length k) t | t <- tails l, k `isPrefixOf` t, preceded t] of
    r : _ -> Just r
    [] -> Nothing
  -- "A:" must not match inside "SP:" etc.: the key starts a word.
  preceded t = let i = length l - length t in i == 0 || isSpace (l !! (i - 1))
  field :: (Integral b, Eq b, Num b) => String -> Either String b
  field k = maybe (Left ("no " <> k <> " field: " <> l)) (hex . takeWhile isHexDigit) (after k)
  hex :: (Eq b, Num b) => String -> Either String b
  hex s = case readHex s of
    [(v, "")] -> Right v
    _ -> Left ("bad hex " <> show s <> ": " <> l)

parseNestestLog :: String -> Either String [NestestLine]
parseNestestLog = traverse parseNestestLine . filter (not . all isSpace) . lines

-- | Decode the bits written to $4016 bit 0, oldest first.
serialText :: [Bool] -> String
serialText = go
 where
  go (False : rest) = case splitAt 8 rest of
    (bits, True : more) | length bits == 8 ->
      toEnum (sum [ 2 ^ i | (i, True) <- zip [0 :: Int ..] bits ]) : go more
    (_, more) -> go more -- framing error: resynchronise
  go (True : rest) = go rest
  go [] = []

-- | The older shell's result: 'Done' once "Passed" or "Failed" has been
-- printed, or "Error n" with its newline.
serialVerdict :: String -> Blargg
serialVerdict t
  | Just r <- after "Error " = case reads r of
      [(n, _)] | '\n' `elem` r -> Done (fromIntegral (n :: Int)) t
      _ -> Running
  | Just _ <- after "Failed" = Done 1 t
  | Just _ <- after "Passed" = Done 0 t
  | otherwise = NoSignature
 where
  after k = case [drop (length k) s | s <- tails t, k `isPrefixOf` s] of
    r : _ -> Just r
    [] -> Nothing
