-- | Layer 0: the test hooks (M2 step 2.0) and the reference model (step
-- 2.1) on hand-written programs, the fetched test ROMs and our own
-- regression ROMs.
module Main (main) where

import Prelude

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.Foldable (for_)
import qualified Data.Vector.Unboxed as U
import Data.Word
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import System.FilePath ((</>))
import Test.Tasty
import Test.Tasty.Hedgehog (testProperty)
import Test.Tasty.HUnit

import Gb.Cartridge
import Gb.Hooks
import Gb.Screen
import Gb.SpecCpu (cpuTests, joypadTests)
import Gb.SpecRoms (romTests)
import Gb.SpecSaveState (saveStateTests)
import Gb.SpecRestore (restoreTests, slotReplay)
import Gb.TestRoms
import System.Environment (lookupEnv)
import Platform.Sim.Frame (readFrame)

main :: IO ()
main = do
 local <- lookupEnv "EMU_RESTORE_ROM"
 slot <- lookupEnv "EMU_RESTORE_SLOT"
 defaultMain $ testGroup "gb-spec" $ maybe [] (\sl -> [testCase "slot replay (EMU_RESTORE_SLOT)" (maybe (assertFailure "EMU_RESTORE_ROM too") (\r -> slotReplay r sl) local)]) slot <>
  [ serialTests
  , verdictTests
  , cartridgeTests
  , screenTests
  , cpuTests
  , joypadTests
  , romTests
  , saveStateTests
  , restoreTests local
  ]

-- * Serial capture

-- | The writes a Blargg-style print routine makes for one byte.
printWrites :: Word8 -> [(Word16, Word8)]
printWrites c = [(regSB, c), (regSC, 0x81)]

capture :: [(Word16, Word8)] -> String
capture = serialText . foldl' (\s (a, d) -> serialWrite a d s) serialIdle

serialTests :: TestTree
serialTests = testGroup "serial capture"
  [ testCase "SB then SC=0x81 sends SB" $
      capture (concatMap printWrites (map (fromIntegral . fromEnum) "Passed\n")) @?= "Passed\n"
  , testCase "SC without start or without internal clock sends nothing" $
      capture [(regSB, 0x41), (regSC, 0x80), (regSC, 0x01), (regSC, 0x00)] @?= ""
  , testCase "SC=0x83 (bit 1 set, CGB speed) still counts as start + internal clock" $
      capture [(regSB, 0x41), (regSC, 0x83)] @?= "A"
  , testCase "a second start without a new SB sends SB again" $
      capture [(regSB, 0x41), (regSC, 0x81), (regSC, 0x81)] @?= "AA"
  , testProperty "text survives other writes in between" $ H.property $ do
      txt <- H.forAll (Gen.list (Range.linear 0 64) (Gen.word8 Range.constantBounded))
      noise <- H.forAll (Gen.list (Range.linear 0 64)
                 ((,) <$> Gen.filter (`notElem` [regSB, regSC]) (Gen.word16 Range.constantBounded)
                      <*> Gen.word8 Range.constantBounded))
      let ws = interleave (map printWrites txt) noise
      capture ws H.=== map (toEnum . fromIntegral) txt
  ]
 where
  interleave (p : ps) (n : ns) = p <> [n] <> interleave ps ns
  interleave ps [] = concat ps
  interleave [] ns = ns

-- * Verdicts

verdictTests :: TestTree
verdictTests = testGroup "verdicts"
  [ testCase "Blargg single ROM passes" $ blarggVerdict "01-special\n\n\nPassed\n" @?= Pass
  , testCase "Blargg combined ROM passes" $
      blarggVerdict "cpu_instrs\n\n01:ok  02:ok  \n\nPassed all tests\n" @?= Pass
  , testCase "Blargg failure" $
      blarggVerdict "02-interrupts\n\n\nFailed #3\n" @?= Fail "02-interrupts\n\n\nFailed #3\n"
  , testCase "Blargg still running" $ blarggVerdict "03-op sp,hl\n\n" @?= Running
  , testCase "Mooneye pass" $ mooneyeVerdict (MooneyeRegs 3 5 8 13 21 34) @?= Pass
  , testCase "Mooneye failure pattern" $
      mooneyeVerdict (MooneyeRegs 0x42 0x42 0x42 0x42 0x42 0x42)
        @?= Fail "Mooneye failure pattern (all 0x42)"
  , testCase "anything else at the breakpoint fails" $
      case mooneyeVerdict (MooneyeRegs 3 5 8 13 21 0) of
        Fail _ -> pure ()
        v      -> assertFailure (show v)
  , testCase "the breakpoint is LD B,B" $ mooneyeBreakpoint @?= 0x40
  ]

-- * Cartridge headers

-- | Logo, header checksum, a supported cartridge type, and a ROM size
-- field that matches the file: what every ROM we load must have.
checkRom :: FilePath -> BS.ByteString -> Assertion
checkRom p rom = case parseHeader rom of
  Left e -> assertFailure (p <> ": " <> e)
  Right h -> do
    assertBool (p <> ": logo") (hdLogoOk h)
    assertBool (p <> ": header checksum") (hdHeaderSumOk h)
    assertBool (p <> ": cartridge type " <> show (hdCartType h)) (cartType (hdCartType h) /= Nothing)
    romSizeBytes (hdRomSize h) @?= Just (BS.length rom)

cartridgeTests :: TestTree
cartridgeTests = testGroup "cartridge headers"
  [ testCase "our serial_pass.gb (rgbds) has a complete header" $ do
      rom <- loadRomFile (asmRoot </> "serial_pass.gb")
      checkRom "serial_pass.gb" rom
      h <- either assertFailure pure (parseHeader rom)
      hdTitle h @?= "SERIAL_PASS"
      cartType (hdCartType h) @?= Just (CartType NoMbc False False)
      ramSizeBytes (hdRamSize h) @?= Just 0
      assertBool "global checksum (rgbfix -v)" (hdGlobalSumOk h)
      assertBool "the message is in the ROM" (BC.pack "Passed\n\0" `BS.isInfixOf` rom)
  , testCase "every fetched ROM (Blargg, Mooneye, dmg-acid2) has a valid header" $ do
      ps <- fetchedRoms
      assertBool ("only " <> show (length ps) <> " ROMs found") (length ps >= 100)
      for_ ps $ \p -> loadRomFile p >>= checkRom p
  , testCase "dmg-acid2 and cpu_instrs are what M2 starts with: no MBC / MBC1, 32/64 KiB" $ do
      acid <- loadRomFile (romRoot </> "dmg-acid2/dmg-acid2.gb")
      cpu <- loadRomFile (romRoot </> "blargg/cpu_instrs/cpu_instrs.gb")
      let ty r = either (const Nothing) (cartType . hdCartType) (parseHeader r)
      fmap ctMbc (ty acid) @?= Just NoMbc
      fmap ctMbc (ty cpu) @?= Just Mbc1
      (BS.length acid, BS.length cpu) @?= (0x8000, 0x10000)
  , testCase "header checksum of a known header (Pan Docs formula)" $ do
      let rom = BS.replicate 0x134 0 <> BS.replicate 0x19 0 <> BS.replicate 0x100 0
      headerChecksum rom @?= 0xE7 -- 0 - 25 * 1 mod 256
  ]

-- * Screens and goldens

screenTests :: TestTree
screenTests = testGroup "screens"
  [ testCase "dmg-acid2's reference image is a DMG screen" $ do
      f <- readFrame (romRoot </> "dmg-acid2/reference-dmg.png") >>= either assertFailure pure
      sc <- either assertFailure pure (frameScreen f)
      frameScreen (screenFrame sc) @?= Right sc
      -- all four shades occur, and the border is white
      filter (`U.elem` screenShades sc) [0 .. 3] @?= [0 .. 3]
      shadeAt sc 0 0 @?= 0
  , testCase "mkScreen rejects wrong sizes and shades above 3" $ do
      either (const ()) (const (error "size")) (mkScreen (U.replicate 10 0)) @?= ()
      either (const ()) (const (error "shade")) (mkScreen (U.replicate (160 * 144) 4)) @?= ()
  , screenGolden "four shades in vertical bands (golden path check)" "shade-bands" $
      either fail pure (mkScreen (U.generate (160 * 144) (\i -> fromIntegral ((i `mod` 160) `div` 40))))
  ]
