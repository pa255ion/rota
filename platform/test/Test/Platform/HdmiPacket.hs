module Test.Platform.HdmiPacket (tests) where

import Prelude

import Data.Bits (popCount, shiftL, testBit, xor, (.&.))
import Data.List (nub)

import qualified Clash.Prelude as C
import qualified Clash.Sized.Vector as V
import Hedgehog hiding (assert)
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog

import Platform.Video.HdmiPacket
import Platform.Video.Tmds (tmdsControlSymbol)

-- * Reference: the BCH code as polynomials over GF(2)

-- | Bits in send order as a polynomial, the first bit the highest power.
poly :: [Bool] -> Integer
poly = foldl (\acc b -> acc * 2 + (if b then 1 else 0)) 0

-- | The remainder of a polynomial divided by g(x) = x^8 + x^7 + x^6 + 1.
polyMod :: Integer -> Integer
polyMod = go
 where
  g = 0x1C1 :: Integer
  deg x = if x == 0 then -1 else length (takeWhile (> 0) (iterate (`div` 2) x)) - 1
  go x
    | deg x < 8 = x
    | otherwise = go (x `xor` (g `shiftL` (deg x - 8)))

bitsOf :: C.KnownNat n => C.BitVector n -> [Bool]
bitsOf v = [testBit v i | i <- [0 .. fromIntegral (C.natVal v) - 1]]

genBytes :: Int -> Gen [C.BitVector 8]
genBytes n = Gen.list (Range.singleton n) (fromIntegral <$> Gen.word8 Range.constantBounded)

vec3 :: [C.BitVector 8] -> C.Vec 3 (C.BitVector 8)
vec3 [a, b, c] = a C.:> b C.:> c C.:> C.Nil
vec3 _ = error "three bytes"

vec7 :: [C.BitVector 8] -> C.Vec 7 (C.BitVector 8)
vec7 xs = V.unsafeFromList xs

-- | A packet's header and subpackets read back from the 32 pixels' nibbles,
-- by the layout rules alone.
fromNibbles :: [C.Vec 3 (C.BitVector 4)] -> (Integer, [Integer], Bool)
fromNibbles ns = (hdr, subs, firstOk)
 where
  ch k = map (C.!! (k :: Int)) ns
  hdr = sum [if testBit c 2 then 2 ^ i else 0 | (i, c) <- zip [0 :: Int ..] (ch 0)]
  subs = [ sum [ (if testBit c1 k then 2 ^ (2 * i) else 0) + (if testBit c2 k then 2 ^ (2 * i + 1) else 0)
               | (i, c1, c2) <- zip3 [0 :: Int ..] (ch 1) (ch 2) ]
         | k <- [0 .. 3] ]
  firstOk = not (testBit (head (ch 0)) 3) && all (`testBit` 3) (tail (ch 0))

tests :: TestTree
tests = testGroup "Video.HdmiPacket"
  [ testProperty "the header's ECC makes a BCH codeword: data then ECC, in send order, is a multiple of g(x)" $ property $ do
      hb <- forAll (genBytes 3)
      let p = Packet (vec3 hb) (C.repeat (C.repeat 0))
      polyMod (poly (bitsOf (headerBits p))) === 0

  , testProperty "the subpackets' ECC too (64,56)" $ property $ do
      sb <- forAll (genBytes 7)
      polyMod (poly (bitsOf (subpacketBits (vec7 sb)))) === 0

  , testCase "ECC: zero data, zero ECC; a single bit gives a nonzero ECC" $ do
      ecc (0 :: C.BitVector 24) @?= 0
      assertBool "one bit" (ecc (1 :: C.BitVector 24) /= 0)

  , testCase "TERC4: 16 distinct symbols, each with five ones, none a control symbol; decoding is the inverse" $ do
      let syms = [terc4 (fromIntegral d) | d <- [0 .. 15 :: Int]]
      length (nub syms) @?= 16
      assertBool "balanced" (all (\s -> popCount s == 5) syms)
      let ctl = [tmdsControlSymbol a b | a <- [0, 1], b <- [0, 1]]
      assertBool "not control" (all (`notElem` ctl) syms)
      sequence_ [terc4Decode (terc4 (fromIntegral d)) @?= Just (fromIntegral d) | d <- [0 .. 15 :: Int]]
      terc4Decode (tmdsControlSymbol 0 0) @?= Nothing

  , testProperty "islandNibbles: the header and subpackets come back from the 32 pixels; bit 3 is 0 only first" $ property $ do
      hb <- forAll (genBytes 3)
      sbs <- forAll (Gen.list (Range.singleton 4) (genBytes 7))
      hs <- forAll (Gen.element [0, 1])
      vs <- forAll (Gen.element [0, 1])
      let p = Packet (vec3 hb) (V.unsafeFromList (map vec7 sbs))
          ns = [islandNibbles hs vs p (fromIntegral i) | i <- [0 .. 31 :: Int]]
          (h, ss, firstOk) = fromNibbles ns
      h === fromIntegral (headerBits p)
      ss === map (fromIntegral . subpacketBits . vec7) sbs
      H.assert firstOk
      H.assert (all (\n -> C.slice C.d1 C.d0 (n C.!! (0 :: Int)) == C.pack vs C.++# C.pack hs) ns)

  , testCase "ACR: N 6144, CTS 25174 in SB1 to SB6 of every subpacket" $ do
      let p = acrPacket 6144 25174
      C.toList (pHeader p) @?= [0x01, 0, 0]
      mapM_ (\sb -> C.toList sb @?= [0x00, 0x00, 0x62, 0x56, 0x00, 0x18, 0x00]) (C.toList (pSub p))

  , testCase "Audio Sample: present and B bits, the samples' bytes, even parity with the channel status bit" $ do
      let f0 = AudioFrame 0x1234 (-2) 0
          f1 = AudioFrame (-32768) 7 1
          p = audioSamplePacket (Just f0 C.:> Just f1 C.:> Nothing C.:> Nothing C.:> C.Nil)
      C.toList (pHeader p) @?= [0x02, 0x03, 0x10]
      let [s0, s1, s2, _] = map C.toList (C.toList (pSub p))
      take 6 s0 @?= [0x00, 0x34, 0x12, 0x00, 0xFE, 0xFF]
      take 6 s1 @?= [0x00, 0x00, 0x80, 0x00, 0x07, 0x00]
      s2 @?= replicate 7 0
      let flagsOk sub = and [ even (popCount (sub !! 1) + popCount (sub !! 2) + n (testBit (sub !! 6) 2) + n (testBit (sub !! 6) 3))
                            , even (popCount (sub !! 4) + popCount (sub !! 5) + n (testBit (sub !! 6) 6) + n (testBit (sub !! 6) 7)) ]
          n b = if b then 1 else 0 :: Int
      assertBool "parity" (flagsOk s0 && flagsOk s1)
      -- V = U = 0
      ((s0 !! 6) .&. 0x33) @?= 0

  , testCase "InfoFrames: the checksum makes the header and payload sum to 0 (AVI PB0 0x56, Audio 0x70)" $ do
      let total p = sum (map fromIntegral (C.toList (pHeader p)) ++ concatMap (map fromIntegral . C.toList) (C.toList (pSub p))) `mod` 256 :: Int
      total aviInfoFrame @?= 0
      total audioInfoFrame @?= 0
      C.toList (pHeader aviInfoFrame) @?= [0x82, 0x02, 0x0D]
      take 6 (C.toList (C.head (pSub aviInfoFrame))) @?= [0x56, 0x00, 0x18, 0x00, 0x01, 0x00]
      C.toList (pHeader audioInfoFrame) @?= [0x84, 0x01, 0x0A]
      take 2 (C.toList (C.head (pSub audioInfoFrame))) @?= [0x70, 0x01]

  , testCase "guard bands: the island's channel 0 is TERC4 of 11, VSYNC, HSYNC" $ do
      C.head (islandGuard 1 0) @?= terc4 0b1101
      C.toList (C.tail (islandGuard 0 0)) @?= [0b0100110011, 0b0100110011]
      C.toList videoGuard @?= [0b1011001100, 0b0100110011, 0b1011001100]
  ]
