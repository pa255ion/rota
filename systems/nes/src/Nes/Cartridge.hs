{-|
NES cartridge files (M3 step 3.0): the 16-byte iNES / NES 2.0 header and
the PRG and CHR images behind it (nesdev wiki, "INES" and "NES 2.0").
The spec, the Clash core's loader and the PS menu (step 3.8, which checks
the header again in Zig) read the same fields.

Header layout:

* 0 to 3: @"NES" 0x1A@.
* 4, 5: PRG ROM in 16 KiB units, CHR ROM in 8 KiB units (0 = CHR RAM).
  NES 2.0 adds a high nibble each in byte 9; a high nibble of 0xF means
  the low byte is an exponent-multiplier pair: @2^E * (2 * MM + 1)@ bytes.
* 6: bit 0 mirroring (0 horizontal, 1 vertical), bit 1 battery, bit 2
  512-byte trainer before PRG, bit 3 four-screen VRAM, bits 4 to 7 mapper
  bits 0 to 3.
* 7: bits 2 and 3 = 2 marks NES 2.0; bits 4 to 7 mapper bits 4 to 7.
* NES 2.0 only: 8 = mapper bits 8 to 11 (low nibble) and submapper; 10,
  11 = PRG and CHR (NV)RAM sizes as shift counts (@64 << n@, 0 = none).
* iNES only: 8 = PRG RAM in 8 KiB units (0 means 8 KiB). Old dumps have
  junk ("DiskDude!") in bytes 7 to 15; if any of bytes 12 to 15 is
  non-zero, byte 7 is ignored (the wiki's recommendation).
-}
module Nes.Cartridge
  ( -- * Header
    Format (..)
  , Mirroring (..)
  , Header (..)
  , headerSize
  , parseHeader
    -- * Whole files
  , Rom (..)
  , parseRom
    -- * What M3 supports
  , supportedMappers
  , checkSupported
  ) where

import Prelude

import Data.Bits (shiftL, shiftR, testBit, (.&.), (.|.))
import qualified Data.ByteString as BS
import Data.Word

data Format = Ines | Nes20
  deriving stock (Show, Eq)

data Mirroring = Horizontal | Vertical | FourScreen
  deriving stock (Show, Eq)

data Header = Header
  { hdFormat    :: Format
  , hdMapper    :: Int
  , hdSubmapper :: Int        -- ^ 0 for iNES
  , hdPrgRom    :: Int        -- ^ bytes
  , hdChrRom    :: Int        -- ^ bytes; 0 = the cartridge has CHR RAM
  , hdChrRam    :: Int        -- ^ bytes (iNES: 8 KiB when there is no CHR ROM)
  , hdPrgRam    :: Int        -- ^ bytes of PRG RAM and NVRAM together (iNES: at least 8 KiB)
  , hdMirroring :: Mirroring
  , hdBattery   :: Bool
  , hdTrainer   :: Bool
  } deriving stock (Show, Eq)

headerSize :: Int
headerSize = 16

parseHeader :: BS.ByteString -> Either String Header
parseHeader b
  | BS.length b < headerSize = Left "shorter than an iNES header"
  | BS.take 4 b /= BS.pack [0x4E, 0x45, 0x53, 0x1A] = Left "no iNES signature"
  | nes20 = Right Header
      { hdFormat    = Nes20
      , hdMapper    = mapperLo .|. mapperMid .|. (fromIntegral (at 8 .&. 0x0F) `shiftL` 8)
      , hdSubmapper = fromIntegral (at 8 `shiftR` 4)
      , hdPrgRom    = romSize (at 4) (at 9 .&. 0x0F) 0x4000
      , hdChrRom    = romSize (at 5) (at 9 `shiftR` 4) 0x2000
      , hdChrRam    = shiftSize (at 11 .&. 0x0F) + shiftSize (at 11 `shiftR` 4)
      , hdPrgRam    = shiftSize (at 10 .&. 0x0F) + shiftSize (at 10 `shiftR` 4)
      , hdMirroring = mirroring
      , hdBattery   = testBit f6 1
      , hdTrainer   = testBit f6 2
      }
  | otherwise = Right Header
      { hdFormat    = Ines
      , hdMapper    = mapperLo .|. (if junk then 0 else mapperMid)
      , hdSubmapper = 0
      , hdPrgRom    = fromIntegral (at 4) * 0x4000
      , hdChrRom    = fromIntegral (at 5) * 0x2000
      , hdChrRam    = if at 5 == 0 then 0x2000 else 0
      , hdPrgRam    = 0x2000 * max 1 (fromIntegral (if junk then 0 else at 8))
      , hdMirroring = mirroring
      , hdBattery   = testBit f6 1
      , hdTrainer   = testBit f6 2
      }
 where
  at = BS.index b
  f6 = at 6
  f7 = at 7
  nes20 = f7 .&. 0x0C == 0x08
  junk = any ((/= 0) . at) [12 .. 15]
  mapperLo = fromIntegral (f6 `shiftR` 4)
  mapperMid = fromIntegral (f7 .&. 0xF0)
  mirroring
    | testBit f6 3 = FourScreen
    | testBit f6 0 = Vertical
    | otherwise    = Horizontal
  romSize :: Word8 -> Word8 -> Int -> Int
  romSize lsb msb unit
    -- (computed in Integer and capped: 2^63 * 7 would overflow an Int to
    -- a negative size that passes the size checks; found by nes-spec's
    -- random headers, 2026-10-03)
    | msb == 0x0F = fromInteger (min (toInteger (maxBound :: Int))
                      (2 ^ (fromIntegral (lsb `shiftR` 2) :: Int) * (toInteger (lsb .&. 3) * 2 + 1)))
    | otherwise = (fromIntegral msb `shiftL` 8 .|. fromIntegral lsb) * unit
  shiftSize :: Word8 -> Int
  shiftSize 0 = 0
  shiftSize n = 64 `shiftL` fromIntegral n

-- | A cartridge file split into its parts.
data Rom = Rom
  { romHeader :: Header
  , romPrg    :: BS.ByteString
  , romChr    :: BS.ByteString -- ^ empty for CHR RAM
  } deriving stock (Show, Eq)

-- | 'Left' if the file is shorter than its header says; bytes after CHR
-- (PlayChoice data, padding) are ignored.
parseRom :: BS.ByteString -> Either String Rom
parseRom b = do
  h <- parseHeader b
  let prgAt = headerSize + (if hdTrainer h then 512 else 0)
      need = prgAt + hdPrgRom h + hdChrRom h
  if BS.length b < need
    then Left ("file of " <> show (BS.length b) <> " bytes, header says " <> show need)
    else Right Rom
      { romHeader = h
      , romPrg = BS.take (hdPrgRom h) (BS.drop prgAt b)
      , romChr = BS.take (hdChrRom h) (BS.drop (prgAt + hdPrgRom h) b)
      }

-- | Mappers of M3 (docs/systems/nes.md, "Mappers"): NROM, MMC1, UxROM,
-- CNROM, AxROM; MMC3 (4) comes with step 3.10.
supportedMappers :: [Int]
supportedMappers = [0, 1, 2, 3, 4, 5, 7]

-- | Why M3 can't run this cartridge, if it can't. Sizes: PRG ROM in DDR
-- up to 512 KiB, CHR ROM in block RAM up to 256 KiB (nes.md, "Memories";
-- MMC3's largest board). MMC1's 512 KiB board (SUROM, its CHR register
-- choosing the PRG half) isn't modelled: MMC1 up to 256 KiB.
checkSupported :: Header -> Either String ()
checkSupported h
  | hdMapper h `notElem` supportedMappers = Left ("mapper " <> show (hdMapper h) <> " not supported")
  | hdPrgRom h > 512 * 1024 = Left "PRG ROM above 512 KiB"
  | hdMapper h == 1 && hdPrgRom h > 256 * 1024 = Left "MMC1 PRG ROM above 256 KiB (SUROM) not supported"
  | hdChrRom h > 256 * 1024 = Left "CHR ROM above 256 KiB"
  | hdChrRam h > 8 * 1024 = Left "CHR RAM above 8 KiB"
  | hdPrgRam h > (if hdMapper h == 5 then 64 else 8) * 1024 = Left "PRG RAM above 8 KiB (MMC5: 64 KiB)"
  | hdMirroring h == FourScreen = Left "four-screen VRAM not supported"
  | hdTrainer h = Left "trainer not supported"
  | otherwise = Right ()
