{-|
The Sega ROM header (M6 step 6.0; SMS Power, "ROM header"): 16 bytes at
7FF0h (also looked for at 3FF0h and 1FF0h, as the export BIOS does):
"TMR SEGA", two reserved bytes, the checksum (little-endian), the product
code and version, and a last byte with the region code (high nibble) and
the ROM size code (low nibble).

The checksum, as the export Master System BIOS checks it: a 16-bit sum of
the ROM's bytes over the range the size code gives, without the last 16
bytes of its first 32 KiB, where the header of a ROM that size sits
(1FF0h, 3FF0h or 7FF0h). SMS Power doesn't spell the rule out; it was
found from the headers four tools wrote, which all agree with it
(`cabal test sms-spec`): WLA-DX's (ours, 64 KiB), devkitSMS's (Raposa do
Sol, 32 KiB code), ZEXALL's (32 KiB code) and GBDK's (Waternet: size code
Ah, 8 KiB, so 0000h-1FEFh, in a 64 KiB file). Nothing in the machine depends on
it: without a BIOS no one checks it, and with the user's BIOS the BIOS does.
-}
module Sms.Header
  ( Header (..)
  , Region (..)
  , findHeader
  , regionOf
  , sizeOfCode
  , checksum
  , checksumOk
  ) where

import Prelude

import Data.Bits (shiftR, (.&.))
import qualified Data.ByteString as BS
import Data.List (find)
import Data.Word (Word16, Word8)

data Region = SmsJapan | SmsExport | GgJapan | GgExport | GgInternational | OtherRegion Word8
  deriving (Eq, Show)

data Header = Header
  { hAt       :: Int      -- ^ where it was found (7FF0h, 3FF0h or 1FF0h)
  , hChecksum :: Word16   -- ^ as stored
  , hRegion   :: Region
  , hSizeCode :: Word8    -- ^ the low nibble of the last byte
  } deriving (Eq, Show)

regionOf :: Word8 -> Region
regionOf c = case c of
  3 -> SmsJapan
  4 -> SmsExport
  5 -> GgJapan
  6 -> GgExport
  7 -> GgInternational
  _ -> OtherRegion c

-- | The bytes a size code covers (SMS Power: 8 KiB to 1 MiB; 0Dh, 48 KiB,
-- and 02h, 1 MiB, "buggy" in the BIOS, and never used).
sizeOfCode :: Word8 -> Maybe Int
sizeOfCode c = lookup c
  [ (0xA, 0x2000), (0xB, 0x4000), (0xC, 0x8000), (0xD, 0xC000), (0xE, 0x10000)
  , (0xF, 0x20000), (0x0, 0x40000), (0x1, 0x80000), (0x2, 0x100000) ]

findHeader :: BS.ByteString -> Maybe Header
findHeader rom = header <$> find tmr [0x7FF0, 0x3FF0, 0x1FF0]
 where
  tmr at = BS.length rom >= at + 16 && BS.take 8 (BS.drop at rom) == BS.pack (map (fromIntegral . fromEnum) "TMR SEGA")
  byte i = BS.index rom i
  header at = Header
    { hAt = at
    , hChecksum = fromIntegral (byte (at + 10)) + 256 * fromIntegral (byte (at + 11))
    , hRegion = regionOf (byte (at + 15) `shiftR` 4)
    , hSizeCode = byte (at + 15) .&. 0x0F
    }

-- | The checksum over a size's bytes: from 0 up to the size, without the
-- 16 bytes before min size 8000h (that size's header); the ROM's length
-- limits the sum (a short file sums what it has).
checksum :: Int -> BS.ByteString -> Word16
checksum size rom = sumOf 0 (min size 0x8000 - 16) + sumOf 0x8000 size
 where
  sumOf a b = BS.foldl' (\s w -> s + fromIntegral w) 0 (BS.take (b - a) (BS.drop a rom))

-- | Whether the stored checksum is the one its size code gives.
checksumOk :: BS.ByteString -> Header -> Bool
checksumOk rom h = maybe False (\n -> checksum n rom == hChecksum h) (sizeOfCode (hSizeCode h))
