-- | Step 4.0: XEX and cartridge parsers, the screen-text hook, Acid800's
-- verdicts and the frozen test list, the fetched files' shapes.
module A800.SpecMedia (mediaTests) where

import Prelude

import Control.Monad (forM_, unless)
import Data.Bits (shiftR, (.&.))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.List (isInfixOf, sort)
import qualified Data.Map.Strict as M
import Data.Word
import Hedgehog ((===))
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import System.Directory (listDirectory)
import System.FilePath ((</>), dropExtension, takeExtension)
import Test.Tasty
import Test.Tasty.Hedgehog (testProperty)
import Test.Tasty.HUnit

import A800.Acid800
import A800.Media
import A800.Screen
import A800.TestRoms

mediaTests :: TestTree
mediaTests = testGroup "media and hooks (4.0)"
  [ xexTests
  , cartTests
  , screenTests
  , acidTests
  , fileTests
  ]

-- * XEX

seg :: Word16 -> [Word8] -> Segment
seg s = Segment s . BS.pack

xexTests :: TestTree
xexTests = testGroup "XEX"
  [ testProperty "encode then parse gives the segments back" $ H.property $ do
      segs <- H.forAll (Gen.list (Range.linear 1 6) genSeg)
      parseXex (encodeXex segs) === Right (Xex segs 0)
  , testCase "a $FFFF marker before a later segment is skipped" $
      parseXex (BS.pack [0xFF, 0xFF, 0x00, 0x20, 0x00, 0x20, 0xA9, 0xFF, 0xFF, 0x00, 0x30, 0x01, 0x30, 1, 2])
        @?= Right (Xex [seg 0x2000 [0xA9], seg 0x3000 [1, 2]] 0)
  , testCase "no marker" $ parseXex (BS.pack [0x00, 0x20, 0x00, 0x20, 0]) @?= Left XexNoMarker
  , testCase "marker only" $ parseXex (BS.pack [0xFF, 0xFF]) @?= Left XexEmpty
  , testCase "end before start" $
      parseXex (BS.pack [0xFF, 0xFF, 0x00, 0x20, 0xFF, 0x1F]) @?= Left (XexBadRange 2 0x2000 0x1FFF)
  , testCase "data cut short" $
      parseXex (BS.pack [0xFF, 0xFF, 0x00, 0x20, 0x03, 0x20, 1, 2]) @?= Left (XexTruncated 2 0x2000 2)
  , testCase "a stray byte after the last segment is tolerated" $
      xexTrailing <$> parseXex (encodeXex [seg 0x2000 [1]] <> BS.pack [0]) @?= Right 1
  , testCase "INITAD: only a segment writing both bytes" $ do
      segInit (seg 0x02E2 [0x00, 0x30]) @?= Just 0x3000
      segInit (seg 0x02E0 [0x00, 0x20, 0x34, 0x12]) @?= Just 0x1234
      segInit (seg 0x02E2 [0x00]) @?= Nothing
      segInit (seg 0x02E3 [0x30]) @?= Nothing
  , testCase "run address: the last RUNAD written, else the first segment" $ do
      xexRunAddress (Xex [seg 0x2000 [0], seg 0x02E0 [0x00, 0x21], seg 0x02E0 [0x00, 0x22]] 0) @?= 0x2200
      xexRunAddress (Xex [seg 0x2400 [0], seg 0x3000 [0]] 0) @?= 0x2400
  ]
 where
  genSeg = do
    s <- Gen.word16 (Range.linear 0 0xFF00)
    n <- Gen.int (Range.linear 1 (min 300 (0x10000 - fromIntegral s)))
    -- a segment starting at $FFFF would read as a marker; start below
    Segment s <$> Gen.bytes (Range.singleton n)

-- * Cartridges

carFile :: Word32 -> BS.ByteString -> BS.ByteString
carFile t img = BC.pack "CART" <> be t <> be (carChecksum img) <> BS.replicate 4 0 <> img
 where
  be w = BS.pack [fromIntegral (w `shiftR` s) | s <- [24, 16, 8, 0]]

cartTests :: TestTree
cartTests = testGroup "cartridges"
  [ testCase "CAR type 1 (8 KiB) and 2 (16 KiB)" $ do
      parseCar (carFile 1 img8) @?= Right (Cart Std8k img8)
      parseCar (carFile 2 img16) @?= Right (Cart Std16k img16)
      (cartBase Std8k, cartBase Std16k) @?= (0xA000, 0x8000)
  , testCase "CAR errors" $ do
      bad (parseCar (BC.pack "CARX" <> BS.drop 4 (carFile 1 img8))) "no CART header"
      bad (parseCar (carFile 15 img8)) "not supported"
      bad (parseCar (carFile 2 img8)) "needs 16384"
      let f = carFile 1 img8
      bad (parseCar (BS.take 20 f <> BS.singleton (BS.index f 20 + 1) <> BS.drop 21 f)) "checksum"
  , testCase "raw images and file names" $ do
      parseCart "x.ROM" img8 @?= Right (Cart Std8k img8)
      parseCart "y.bin" img16 @?= Right (Cart Std16k img16)
      parseCart "z.car" (carFile 1 img8) @?= Right (Cart Std8k img8)
      bad (parseCart "w.rom" (BS.take 100 img8)) "8192 or 16384"
      bad (parseCart "v.xex" img8) "not a cartridge"
  ]
 where
  img8 = BS.pack (take 0x2000 (cycle [0 .. 255]))
  img16 = BS.pack (take 0x4000 (cycle [7, 1, 255]))
  bad r s = case r of
    Left e -> unless (s `isInfixOf` e) (assertFailure ("error " <> show e <> " lacks " <> show s))
    Right c -> assertFailure ("parsed: " <> show (cartType c))

-- * Screen

-- | A memory with SAVMSC = $9C40 and the given lines as ATASCII text.
screenMem :: [String] -> Word16 -> Word8
screenMem ls a
  | a == 0x58 = 0x40
  | a == 0x59 = 0x9C
  | otherwise = M.findWithDefault 0 a m
 where
  m = M.fromList
    [ (0x9C40 + 40 * r + c, toInternal (fromIntegral (fromEnum ch)))
    | (r, l) <- zip [0 ..] ls, (c, ch) <- zip [0 ..] l ]

-- | LMARGN 2, RMARGN 39 (as after start-up) on top of a memory.
withMargins :: (Word16 -> Word8) -> Word16 -> Word8
withMargins rd a
  | a == 0x52 = 2
  | a == 0x53 = 39
  | otherwise = rd a

-- | ATASCII to internal codes (the inverse of 'internalToAtascii').
toInternal :: Word8 -> Word8
toInternal a = case a .&. 0x60 of
  0x00 -> a + 0x40
  0x20 -> a - 0x20
  0x40 -> a - 0x20
  _ -> a

screenTests :: TestTree
screenTests = testGroup "screen text"
  [ testCase "internal codes: the three moved blocks" $ do
      map internalToAtascii [0x00, 0x21, 0x3F, 0x40, 0x5F, 0x61, 0x7A, 0x80 .&. 0x7F]
        @?= [0x20, 0x41, 0x5F, 0x00, 0x1F, 0x61, 0x7A, 0x20]
      forM_ [0 .. 0x7F] $ \c -> toInternal (internalToAtascii c) @?= c
  , testCase "lines, inverse dropped, blanks trimmed" $ do
      let rd a | a == 0x9C40 + 40 * 3 + 2 = 0x80 -- the cursor: an inverse space
               | otherwise = screenMem ["  Altirra 8K BASIC 1.59", "  Ready"] a
      take 3 (screenLines rd) @?= ["  Altirra 8K BASIC 1.59", "  Ready", ""]
      length (screenLines rd) @?= 24
      screenText rd @?= "  Altirra 8K BASIC 1.59\n  Ready\n"
      screenHas "Ready" (withMargins rd) @?= True
      screenHas "READY" (withMargins rd) @?= False
  , testCase "the text between the margins, joined" $ do
      let rd = withMargins (screenMem ["  ab", "  cd"])
      take 6 (screenFlat rd) @?= "ab    "
      drop 38 (take 40 (screenFlat rd)) @?= "cd"
  , testCase "graphics characters show as dots" $
      map atasciiToAscii [0x00, 0x60, 0x7B, 0x7C, 0x7D, 0x9B] @?= "...|.."
  ]

-- * Acid800

acidTests :: TestTree
acidTests = testGroup "Acid800"
  [ testCase "verdicts from the screen's text" $ do
      verdict "ANTIC: WSYNC timing...      " @?= Running
      verdict "ANTIC: WSYNC timing...Pass      " @?= Passed
      verdict "CPU: Timing...FAIL.        LDA abs,X: 5 cycles   " @?= Failed "LDA abs,X: 5 cycles"
      verdict "CPU: 65C816 tests...Skipped   No 65C816" @?= Skipped "No 65C816"
  , testCase "a verdict broken across rows (the editor wraps at RMARGN)" $ do
      let rd a | a == 0x52 = 2
               | a == 0x53 = 39
               | otherwise = screenMem
                   [ "  POKEY: Serial output complete IRQ...Pa"
                   , "  ss" ] a
      verdict (screenFlat rd) @?= Passed
  , testCase "the frozen list: 51 required, 5 tracked, no repeats" $ do
      (length required, length tracked) @?= (51, 5)
      let names = map tName tests
      sort names @?= sort (M.keys (M.fromList [(n, ()) | n <- names]))
  ]

-- * Fetched files and our programs

fileTests :: TestTree
fileTests = testGroup "files"
  [ testCase "AltirraOS: 16 KiB, vectors into the ROM" $ do
      os <- loadFile osRom
      BS.length os @?= 0x4000
      forM_ [0xFFFA, 0xFFFC, 0xFFFE] $ \v -> do
        let w = romWord os 0xC000 v
        assertBool ("vector " <> show v <> " = " <> show w) (w >= 0xC000)
  , testCase "Altirra BASIC: 8 KiB, its banner" $ do
      b <- loadFile basicRom
      BS.length b @?= 0x2000
      assertBool "banner" (BC.pack "Altirra 8K BASIC" `BS.isInfixOf` b)
  , testCase "every fetched Acid800 test is on the list and parses" $ do
      fs <- sort . map dropExtension . filter ((== ".xex") . takeExtension)
              <$> listDirectory (romRoot </> "acid800")
      fs @?= sort (map tName tests)
      forM_ fs $ \n -> do
        x <- loadFile (acidFile n)
        case parseXex x of
          Left e -> assertFailure (n <> ": " <> show e)
          Right p -> assertBool (n <> ": runs from page 0") (xexRunAddress p >= 0x0200)
  , testCase "Scorch parses; it runs from RAM" $ do
      x <- loadFile scorchXex
      case parseXex x of
        Left e -> assertFailure (show e)
        Right p -> do
          assertBool "segments" (length (xexSegments p) > 1)
          assertBool "run address" (xexRunAddress p >= 0x0200)
  , testCase "our XEX (pass.s) and cartridge (cart_pass.s)" $ do
      x <- loadFile (asmRoot </> "pass.xex")
      fmap xexRunAddress (parseXex x) @?= Right 0x2000
      c <- loadFile (asmRoot </> "cart_pass.rom")
      case parseCart "cart_pass.rom" c of
        Left e -> assertFailure e
        Right (Cart t img) -> do
          t @?= Std8k
          romWord img 0xA000 0xBFFA @?= 0xA000 -- start address
          BS.index img 0x1FFC @?= 0              -- cartridge present
          BS.index img 0x1FFD @?= 4              -- start it, no disk boot
  , testCase "our XEGS cartridge (xegs_banks.s): type 12, four banks, the last fixed at $A000" $ do
      c <- loadFile (asmRoot </> "xegs_banks.car")
      case parseCart "xegs_banks.car" c of
        Left e -> assertFailure e
        Right cart@(Cart t _) -> do
          t @?= Xegs32k
          cartBanks t @?= 4
          [cartByte cart b 0x9FF0 | b <- [0 .. 3]] @?= [0, 1, 2, 3]
          [cartByte cart b 0xBFF0 | b <- [0 .. 3]] @?= [3, 3, 3, 3]
  , testCase ".car types 12, 13, 14: XEGS of 32, 64, 128 KiB; a wrong size refused" $ do
      let car ty n = BS.concat [BC.pack "CART", be ty, be (fromIntegral n * 7), BS.replicate 4 0, BS.replicate n 7]
          be :: Word32 -> BS.ByteString
          be v = BS.pack [fromIntegral (v `shiftR` k) | k <- [24, 16, 8, 0]]
      fmap cartType (parseCar (car 12 0x8000)) @?= Right Xegs32k
      fmap cartType (parseCar (car 13 0x10000)) @?= Right Xegs64k
      fmap cartType (parseCar (car 14 0x20000)) @?= Right Xegs128k
      either (const True) (const False) (parseCar (car 14 0x10000)) @? "a 64 KiB image as type 14"
  ]
 where
  romWord img base a =
    let i = fromIntegral (a - base)
    in fromIntegral (BS.index img i) + 256 * fromIntegral (BS.index img (i + 1)) :: Word16
  romWord :: BS.ByteString -> Word16 -> Word16 -> Word16
