{-|
The cartridge header (Pan Docs, "The Cartridge Header"): what a ROM image
says about itself at 0x0100 to 0x014F. The test hooks use it to check the
fetched and the assembled ROMs (M2 step 2.0); the core's loader will use
the same fields to pick the MBC and the RAM size (steps 2.6 and 2.8).

Only the DMG reading of the header: 0x0143 (the CGB flag on later
cartridges) is treated as the last title byte, as on the DMG.
-}
module Gb.Cartridge
  ( -- * Header
    Header (..)
  , parseHeader
  , logoHash
    -- * Fields
  , Mbc (..)
  , CartType (..)
  , cartType
  , hasTimer
  , romSizeBytes
  , ramSizeBytes
    -- * Checksums
  , headerChecksum
  , globalChecksum
  ) where

import Prelude

import Data.Bits (shiftL, xor, (.|.))
import Data.Word (Word64)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.Word

data Header = Header
  { hdLogoOk        :: Bool   -- ^ 0x0104-0x0133 hold the boot ROM's logo ('logoHash'; the boot ROM checks this)
  , hdTitle         :: String -- ^ 0x0134-0x0143, up to the first 0
  , hdCartType      :: Word8  -- ^ 0x0147, see 'cartType'
  , hdRomSize       :: Word8  -- ^ 0x0148, see 'romSizeBytes'
  , hdRamSize       :: Word8  -- ^ 0x0149, see 'ramSizeBytes'
  , hdHeaderSum     :: Word8  -- ^ 0x014D as stored
  , hdHeaderSumOk   :: Bool   -- ^ equals 'headerChecksum' (the boot ROM checks this)
  , hdGlobalSum     :: Word16 -- ^ 0x014E-0x014F as stored, big-endian
  , hdGlobalSumOk   :: Bool   -- ^ equals 'globalChecksum' (nothing on the DMG checks it)
  } deriving stock (Show, Eq)

-- | 'Left' if the image is too short to hold a header.
parseHeader :: BS.ByteString -> Either String Header
parseHeader rom
  | BS.length rom < 0x150 = Left ("image of " <> show (BS.length rom) <> " bytes has no header")
  | otherwise = Right Header
      { hdLogoOk      = logoHash (slice 0x104 0x30) == logoHashWanted
      , hdTitle       = BC.unpack (BS.takeWhile (/= 0) (slice 0x134 0x10))
      , hdCartType    = at 0x147
      , hdRomSize     = at 0x148
      , hdRamSize     = at 0x149
      , hdHeaderSum   = at 0x14D
      , hdHeaderSumOk = at 0x14D == headerChecksum rom
      , hdGlobalSum   = gsum
      , hdGlobalSumOk = gsum == globalChecksum rom
      }
 where
  at = BS.index rom
  slice o n = BS.take n (BS.drop o rom)
  gsum = fromIntegral (at 0x14E) `shiftL` 8 .|. fromIntegral (at 0x14F)

-- | FNV-1a (64-bit) of the bytes: the header's logo (0x0104-0x0133, Pan
-- Docs' "Nintendo Logo") checked by its hash, so this source doesn't
-- carry the logo's bitmap itself.
logoHash :: BS.ByteString -> Word64
logoHash = BS.foldl' (\h b -> (h `xor` fromIntegral b) * 0x100000001b3) 0xcbf29ce484222325

-- | 'logoHash' of the 48 bytes the boot ROM compares.
logoHashWanted :: Word64
logoHashWanted = 0x0e13f8585a99f41f

-- | Memory bank controllers; M2 implements 'NoMbc' and 'Mbc1', then
-- 'Mbc5' (PLAN.md step 2.6).
data Mbc = NoMbc | Mbc1 | Mbc2 | Mbc3 | Mbc5 | Mmm01
  deriving stock (Show, Eq, Ord, Enum, Bounded)

data CartType = CartType
  { ctMbc     :: Mbc
  , ctRam     :: Bool -- ^ external RAM (MBC2's built-in RAM counts)
  , ctBattery :: Bool
  } deriving stock (Show, Eq)

-- | The 0x0147 byte (Pan Docs table). 'Nothing' for types that are not
-- plain cartridges with one of the MBCs above (camera, TAMA5, HuC1/3,
-- MBC6/7) or are unassigned. Timer and rumble variants of MBC3/MBC5 map to
-- the plain MBC.
cartType :: Word8 -> Maybe CartType
cartType t = case t of
  0x00 -> c NoMbc False False
  0x01 -> c Mbc1 False False
  0x02 -> c Mbc1 True False
  0x03 -> c Mbc1 True True
  0x05 -> c Mbc2 True False
  0x06 -> c Mbc2 True True
  0x08 -> c NoMbc True False
  0x09 -> c NoMbc True True
  0x0B -> c Mmm01 False False
  0x0C -> c Mmm01 True False
  0x0D -> c Mmm01 True True
  0x0F -> c Mbc3 False True
  0x10 -> c Mbc3 True True
  0x11 -> c Mbc3 False False
  0x12 -> c Mbc3 True False
  0x13 -> c Mbc3 True True
  0x19 -> c Mbc5 False False
  0x1A -> c Mbc5 True False
  0x1B -> c Mbc5 True True
  0x1C -> c Mbc5 False False
  0x1D -> c Mbc5 True False
  0x1E -> c Mbc5 True True
  _    -> Nothing
 where
  c m r b = Just (CartType m r b)

-- | MBC3 with the real-time clock (types 0x0F, 0x10: TIMER+BATTERY).
hasTimer :: Word8 -> Bool
hasTimer t = t == 0x0F || t == 0x10

-- | The 0x0148 byte: 32 KiB shifted left by it, for 0 to 8 (8 MiB).
romSizeBytes :: Word8 -> Maybe Int
romSizeBytes n
  | n <= 8    = Just (0x8000 `shiftL` fromIntegral n)
  | otherwise = Nothing

-- | The 0x0149 byte. Code 1 is listed as unused by Pan Docs (some
-- homebrew writes it for 2 KiB); it is rejected here.
ramSizeBytes :: Word8 -> Maybe Int
ramSizeBytes n = case n of
  0 -> Just 0
  2 -> Just 0x2000
  3 -> Just 0x8000
  4 -> Just 0x20000
  5 -> Just 0x10000
  _ -> Nothing

-- | Pan Docs: @x = 0; for 0x0134..0x014C: x = x - byte - 1@. The image
-- must hold a header.
headerChecksum :: BS.ByteString -> Word8
headerChecksum rom = BS.foldl' (\x b -> x - b - 1) 0 (BS.take 0x19 (BS.drop 0x134 rom))

-- | Sum of every byte of the image except the two checksum bytes, mod 2^16.
globalChecksum :: BS.ByteString -> Word16
globalChecksum rom = sumOf (BS.take 0x14E rom) + sumOf (BS.drop 0x150 rom)
 where
  sumOf = BS.foldl' (\s b -> s + fromIntegral b) 0
