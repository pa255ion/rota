-- | Layer 0: the test hooks and the cartridge header (M3 step 3.0), then
-- the reference model (steps 3.2 and 3.3) on the fetched test ROMs and our
-- own regression ROMs.
module Main (main) where

import Prelude

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Data.Map.Strict as M
import Data.Word
import System.FilePath ((</>), takeFileName)
import Test.Tasty
import Test.Tasty.HUnit

import Nes.Cartridge
import Nes.Hooks
import Nes.SpecApu (apuTests)
import Nes.SpecEval (evalTests)
import Nes.SpecMapperel (mapperelTests)
import Nes.SpecLoader (loaderTests)
import Nes.SpecMappers (controllerTests, mapperTests)
import Nes.SpecPpu (accuracyCoinTests, ppuRomTests)
import Nes.SpecRestore (restoreTests)
import Nes.SpecRoms (cpuRomTests)
import Nes.TestRoms

main :: IO ()
main = do
  roms <- fetchedRoms
  restore <- restoreTests
  defaultMain $ testGroup "nes-spec"
    [ headerTests roms
    , blarggTests
    , nestestLogTests
    , cpuRomTests
    , mapperTests
    , controllerTests
    , ppuRomTests
    , accuracyCoinTests
    , loaderTests roms
    , apuTests
    , evalTests
    , mapperelTests
    , restore
    ]

-- * Cartridge header

-- | A header with the given bytes 4 to 15.
hdr :: [Word8] -> BS.ByteString
hdr rest = BS.pack ([0x4E, 0x45, 0x53, 0x1A] <> take 12 (rest <> repeat 0))

headerTests :: [FilePath] -> TestTree
headerTests roms = testGroup "cartridge header"
  [ testCase "iNES: sizes, mapper nibbles, mirroring, battery" $
      parseHeader (hdr [2, 1, 0x13, 0x40]) @?= Right Header
        { hdFormat = Ines, hdMapper = 0x41, hdSubmapper = 0, hdPrgRom = 0x8000
        , hdChrRom = 0x2000, hdChrRam = 0, hdPrgRam = 0x2000, hdMirroring = Vertical
        , hdBattery = True, hdTrainer = False }
  , testCase "iNES: no CHR ROM means 8 KiB CHR RAM; four-screen wins over mirroring" $ do
      h <- parsed (hdr [1, 0, 0x09])
      (hdChrRom h, hdChrRam h, hdMirroring h) @?= (0, 0x2000, FourScreen)
  , testCase "iNES: junk in bytes 12 to 15 drops byte 7 (DiskDude!)" $
      hdMapper <$> parseHeader (hdr ([1, 1, 0x10] <> map (fromIntegral . fromEnum) "DiskDude!"))
        @?= Right 1
  , testCase "NES 2.0 exponent sizes don't overflow (2^61 * 5 bytes of CHR: refused)" $ do
      h <- parsed (hdr [0x00, 0xF6, 0x00, 0x08, 0x00, 0xF0])
      assertBool "positive" (hdChrRom h > 0)
      assertBool "refused" (either (const True) (const False) (checkSupported h))
  , testCase "NES 2.0: 12-bit mapper, submapper, size MSBs, RAM shifts" $ do
      h <- parsed (hdr [0x02, 0x01, 0x40, 0x18, 0x21, 0x10, 0x07, 0x70])
      (hdFormat h, hdMapper h, hdSubmapper h) @?= (Nes20, 0x114, 2)
      (hdPrgRom h, hdChrRom h) @?= (2 * 0x4000, 0x101 * 0x2000)
      (hdPrgRam h, hdChrRam h) @?= (64 * 2 ^ (7 :: Int), 64 * 2 ^ (7 :: Int))
  , testCase "NES 2.0: exponent-multiplier PRG size" $
      hdPrgRom <$> parseHeader (hdr [0x07, 0, 0, 0x08, 0, 0x0F]) @?= Right (2 * (3 * 2 + 1))
  , testCase "no signature" $
      parseHeader (BS.replicate 16 0) @?= Left "no iNES signature"
  , testCase "short file" $
      parseRom (BS.take 1000 (hdr [1, 1])) @?= Left "file of 16 bytes, header says 24592"
  , testCase "every fetched ROM parses and is the size its header says" $
      mapM_ checkRom roms
  , testCase "the mappers of the fetched suites" $ do
      ms <- mapM (\p -> (,) (takeFileName p) . hdMapper . romHeader <$> rom p) roms
      let m = M.fromList ms
      M.lookup "nestest.nes" m @?= Just 0
      M.lookup "AccuracyCoin.nes" m @?= Just 0
      M.lookup "all_instrs.nes" m @?= Just 1
      M.lookup "1-clocking.nes" m @?= Just 4
  , testCase "our ca65 ROM (make nes-asm): NROM, 16 KiB PRG, 8 KiB CHR, vertical" $ do
      r <- rom (asmRoot </> "pass.nes")
      let h = romHeader r
      (hdMapper h, hdPrgRom h, hdChrRom h, hdMirroring h) @?= (0, 0x4000, 0x2000, Vertical)
      checkSupported h @?= Right ()
      -- The reset vector points into PRG ($C000 onwards).
      let vec = BS.drop 0x3FFC (romPrg r)
      (BS.index vec 1 >= 0xC0) @? "reset vector below $C000"
  ]
 where
  parsed = either assertFailure pure . parseHeader
  rom p = either (assertFailure . ((p <> ": ") <>)) pure . parseRom =<< loadRomFile p
  checkRom p = do
    b <- loadRomFile p
    r <- either (assertFailure . ((p <> ": ") <>)) pure (parseRom b)
    let h = romHeader r
    BS.length b @?= headerSize + hdPrgRom h + hdChrRom h

-- * Blargg

-- | Memory with these bytes from $6000 and zeros elsewhere.
mem6000 :: [Word8] -> Word16 -> Word8
mem6000 bs a = let i = fromIntegral a - 0x6000 in if i >= 0 && i < length bs then bs !! i else 0

blarggTests :: TestTree
blarggTests = testGroup "Blargg protocol"
  [ testCase "no signature" $ blargg (mem6000 [0x80]) @?= NoSignature
  , testCase "running" $ blargg (mem6000 ([0x80] <> blarggSignature)) @?= Running
  , testCase "reset request" $ blargg (mem6000 ([0x81] <> blarggSignature)) @?= NeedsReset
  , testCase "passed, with text" $
      blargg (mem6000 ([0x00] <> blarggSignature <> txt "ok\nPassed\n" <> [0]))
        @?= Done 0 "ok\nPassed\n"
  , testCase "failed with code 3" $
      blargg (mem6000 ([0x03] <> blarggSignature <> txt "Failed #3" <> [0])) @?= Done 3 "Failed #3"
  ]
 where
  txt = map (fromIntegral . fromEnum)

-- * nestest log

nestestLogTests :: TestTree
nestestLogTests = testGroup "nestest log"
  [ testCase "first line" $
      parseNestestLine "C000  4C F5 C5  JMP $C5F5                       A:00 X:00 Y:00 P:24 SP:FD PPU:  0, 21 CYC:7"
        @?= Right (NestestLine 0xC000 0 0 0 0x24 0xFD 0 21 7)
  , testCase "the whole fetched log parses; cycles increase" $ do
      t <- BC.unpack <$> loadRomFile (romRoot </> "nestest" </> "nestest.log")
      ls <- either assertFailure pure (parseNestestLog t)
      length ls @?= 8991
      and (zipWith (<) (map nlCycle ls) (drop 1 (map nlCycle ls))) @? "cycles not increasing"
      nlPC (last ls) @?= 0xC66E
  ]
