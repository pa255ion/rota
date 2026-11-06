{-|
The cartridge header (PLAN.md step 7.0; docs/systems/snes.md, "Machine
summary"; fullsnes, "SNES Cartridge ROM Header"). A ROM file is the
cartridge's ROM in order, maybe after a 512-byte copier header (a file
size of 512 modulo 1024). The 32 bytes at ROM 7FC0h (LoROM), FFC0h
(HiROM) or 40FFC0h (ExHiROM) hold:

> FFC0 21  title (ASCII)
> FFD5     map mode: 001s mmmm, s = FastROM, m = 0 LoROM, 1 HiROM, 5 ExHiROM
> FFD6     cartridge type: low nibble 0 ROM, 1 + RAM, 2 + RAM + battery,
>          3 + chip, 4 + chip + RAM, 5 + chip + RAM + battery, 6 + chip +
>          battery; high nibble the chip (0 DSP, 1 GSU, 2 OBC1, 3 SA-1,
>          4 S-DD1, 5 S-RTC, E other, F custom: FFBFh says which)
> FFD7     ROM size, 1 KiB << n
> FFD8     SRAM size, 1 KiB << n (0 = none)
> FFD9     region
> FFDC 2   checksum complement, FFDE 2 checksum (little-endian)

and the emulation-mode vectors end the bank (reset at FFFCh). Real
headers are often sloppy (homebrew leaves checksums at 0), so the header
is found by scoring each place that fits the file ('candidates'); the PS
app (platform/ps/systems/snes.zig, step 7.11) is to use the same rules.
-}
module Snes.Cart
  ( Cart (..)
  , Mapping (..)
  , Chip (..)
  , stripCopier
  , parseCart
  , candidates
  , checksum
  ) where

import Prelude

import Data.Bits (shiftL, testBit, (.&.), (.|.))
import qualified Data.ByteString as B
import Data.Char (chr)
import Data.List (sortOn)
import Data.Ord (Down (..))
import Data.Word

data Mapping = LoRom | HiRom | ExHiRom
  deriving stock (Show, Eq, Ord, Enum, Bounded)

-- | The chip named by the type byte's high nibble (and FFBFh for F).
data Chip
  = ChipDsp | ChipGsu | ChipObc1 | ChipSa1 | ChipSdd1 | ChipSrtc
  | ChipOther !Word8   -- ^ high nibble E (Super Game Boy, Satellaview)
  | ChipCustom !Word8  -- ^ high nibble F: FFBFh's value (SPC7110, ST01x, Cx4)
  | ChipUnknown !Word8
  deriving stock (Show, Eq)

data Cart = Cart
  { cartMapping    :: !Mapping
  , cartFastRom    :: !Bool
  , cartTitle      :: !String
  , cartType       :: !Word8
  , cartChip       :: !(Maybe Chip)
  , cartBattery    :: !Bool
  , cartRomSize    :: !Int  -- ^ the ROM's bytes in the file (the header's size byte is only a hint)
  , cartSramSize   :: !Int  -- ^ bytes, 0 = none
  , cartRegion     :: !Word8
  , cartChecksumOk :: !Bool -- ^ the header's checksum is the ROM's, and its complement matches
  , cartHeaderAt   :: !Int  -- ^ the header's offset in the ROM
  } deriving stock (Show, Eq)

-- | The ROM without a copier header, and whether there was one.
stripCopier :: B.ByteString -> (B.ByteString, Bool)
stripCopier f
  | B.length f `mod` 1024 == 512 = (B.drop 512 f, True)
  | otherwise = (f, False)

-- | Header offsets that fit the ROM, with their mapping.
places :: B.ByteString -> [(Int, Mapping)]
places rom =
  [ (o, m) | (o, m) <- [(0x7FC0, LoRom), (0xFFC0, HiRom), (0x40FFC0, ExHiRom)]
  , o + 0x40 <= B.length rom ]

-- | Every place with its score, best first (ties keep LoRom, HiRom, ExHiRom).
candidates :: B.ByteString -> [((Int, Mapping), Int)]
candidates rom = sortOn (Down . snd) [ (p, score rom p) | p <- places rom ]

-- | How much a place looks like a header: its map mode names its own
-- mapping (4), the mode byte's fixed bits (2), a checksum that is the
-- ROM's (4) or at least matches its complement (2), a reset vector into
-- ROM (2), plausible ROM and SRAM sizes (1 each), a printable title (1).
score :: B.ByteString -> (Int, Mapping) -> Int
score rom (o, m) = sum (concat
  [ [ 4 | mode .&. 0x0F == wantNibble ]
  , [ 2 | mode .&. 0xE0 == 0x20 ]
  , [ 4 | csum + cpl == 0xFFFF, csum == fromIntegral (checksum rom) ]
  , [ 2 | csum + cpl == 0xFFFF ]
  , [ 2 | reset >= 0x8000 ]
  , [ 1 | B.index rom (o + 0x17) `elem` [0x05 .. 0x0D] ]
  , [ 1 | B.index rom (o + 0x18) <= 0x08 ]
  , [ 1 | all printable (B.unpack (B.take 21 (B.drop o rom))) ]
  ])
 where
  mode = B.index rom (o + 0x15)
  wantNibble = case m of LoRom -> 0; HiRom -> 1; ExHiRom -> 5
  csum = fromIntegral (word16 rom (o + 0x1E)) :: Int
  cpl = fromIntegral (word16 rom (o + 0x1C)) :: Int
  reset = word16 rom (o + 0x3C)
  printable c = c >= 0x20 && c < 0x7F

word16 :: B.ByteString -> Int -> Word16
word16 b o = fromIntegral (B.index b o) .|. (fromIntegral (B.index b (o + 1)) `shiftL` 8)

-- | The cartridge checksum: the sum of the ROM's bytes, modulo 65536, with
-- a size that isn't a power of two counted as the largest power of two
-- below it plus the rest repeated to fill the same again (fullsnes: a
-- 3 MiB ROM is 2 MiB + 1 MiB twice).
checksum :: B.ByteString -> Word16
checksum rom
  | n == 0 = 0
  | n == p = total rom
  | otherwise = total (B.take p rom) + fromIntegral (p `div` B.length rest) * total rest
 where
  n = B.length rom
  p = until (\k -> k * 2 > n) (* 2) 1
  rest = B.drop p rom
  total = B.foldl' (\s w -> s + fromIntegral w) 0

-- | The cartridge in a ROM file, from its best-scoring header; Nothing if
-- the file is too small for any header.
parseCart :: B.ByteString -> Maybe Cart
parseCart file = case candidates rom of
  [] -> Nothing
  ((o, m), _) : _ ->
    let byte k = B.index rom (o + k)
        ty = byte 0x16
        lo = ty .&. 0x0F
        hi = ty `div` 16
        chip
          | lo < 3 = Nothing
          | otherwise = Just $ case hi of
              0 -> ChipDsp
              1 -> ChipGsu
              2 -> ChipObc1
              3 -> ChipSa1
              4 -> ChipSdd1
              5 -> ChipSrtc
              0xE -> ChipOther ty
              0xF -> ChipCustom (byte (-1))
              _ -> ChipUnknown ty
        sramN = byte 0x18
        csum = word16 rom (o + 0x1E)
        cpl = word16 rom (o + 0x1C)
    in Just Cart
         { cartMapping = m
         , cartFastRom = testBit (byte 0x15) 4
         , cartTitle = map (chr . fromIntegral) (B.unpack (B.take 21 (B.drop o rom)))
         , cartType = ty
         , cartChip = chip
         , cartBattery = lo `elem` [2, 5, 6]
         , cartRomSize = B.length rom
         , cartSramSize = if sramN == 0 || sramN > 0x0D then 0 else 1024 `shiftL` fromIntegral sramN
         , cartRegion = byte 0x19
         , cartChecksumOk = csum == checksum rom && csum + cpl == 0xFFFF
         , cartHeaderAt = o
         }
 where
  rom = fst (stripCopier file)
