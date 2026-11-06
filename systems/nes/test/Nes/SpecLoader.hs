-- | Step 3.8: the PL's side of cartridge loading ("Nes.Loader") against the
-- reference: the header decoded in Clash = 'parseHeader',
-- 'checkSupported' and 'cartCfgFromHeader'; a file streamed through
-- 'loadRoute' = 'parseRom''s PRG and CHR; the desktop's buttons in the
-- controller's order.
module Nes.SpecLoader
  ( loaderTests
  ) where

import Prelude

import Control.Monad (forM_)
import qualified Data.ByteString as BS
import Data.Bits (testBit)
import qualified Data.Map.Strict as M
import Data.String (fromString)
import Data.Word
import System.FilePath (takeFileName)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog (testProperty)
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import qualified Clash.Prelude as C
import Nes.Cartridge
import Nes.Loader
import Nes.Mapper (CartCfg, cartCfgFromHeader)
import Nes.TestRoms

-- | What the Haskell side says about a header.
reference :: BS.ByteString -> Maybe (Maybe CfgBits)
reference b = Just (either (const Nothing) (fmap C.pack . cartCfgFromHeader) (parseHeader b))

type CfgBits = C.BitVector (C.BitSize CartCfg)

clashHeader :: BS.ByteString -> HeaderBytes
clashHeader b = C.map (\i -> fromIntegral (BS.index b (fromEnum i))) C.indicesI

decoded :: BS.ByteString -> Maybe (Maybe CfgBits)
decoded b = Just (C.pack <$> headerCfg (clashHeader b))

-- | Headers near the supported ones: the signature mostly right, small
-- sizes, the NES 2.0 identifier often set, sometimes junk at the end.
genHeader :: H.Gen BS.ByteString
genHeader = do
  sig <- Gen.frequency [(9, pure [0x4E, 0x45, 0x53, 0x1A]), (1, Gen.list (Range.singleton 4) (Gen.word8 Range.constantBounded))]
  prg <- Gen.frequency [(3, Gen.element [0, 1, 2, 4, 8, 16, 17, 32]), (1, Gen.word8 Range.constantBounded)]
  chr <- Gen.frequency [(3, Gen.element [0, 1, 2, 4, 8, 16, 17]), (1, Gen.word8 Range.constantBounded)]
  -- mapper nibbles mostly supported ones, flags mostly plain
  mapLo <- Gen.frequency [(4, Gen.element [0, 1, 2, 3, 7]), (1, Gen.word8 (Range.constant 0 15))]
  flags <- Gen.frequency [(4, Gen.element [0, 1, 2, 3]), (1, Gen.word8 (Range.constant 0 15))]
  mapHi <- Gen.frequency [(4, pure 0), (1, Gen.word8 (Range.constant 0 15))]
  ident <- Gen.frequency [(2, pure 0x08), (1, pure 0x00), (1, Gen.word8 (Range.constant 0 15))]
  let f6 = mapLo * 16 + flags
      f7 = mapHi * 16 + ident
  -- NES 2.0 bytes 8 to 11: mapper MSBs, size MSBs, RAM shift counts
  let small = Gen.frequency [(3, pure 0), (3, Gen.word8 (Range.constant 0 7)), (1, Gen.word8 Range.constantBounded)]
  rest <- sequence [small, Gen.frequency [(3, pure 0), (1, Gen.word8 Range.constantBounded)], small, small]
  tailB <- Gen.frequency [(4, pure [0, 0, 0, 0]), (1, Gen.list (Range.singleton 4) (Gen.word8 Range.constantBounded))]
  pure (BS.pack (sig <> [prg, chr, f6, f7] <> rest <> tailB))

-- | Streams a file through 'loadRoute' as the PS does (header first, the
-- core held), into maps of the PRG and CHR writes.
stream :: BS.ByteString -> (M.Map Int Word8, M.Map Int Word8, HeaderBytes)
stream b = go 0 (C.repeat 0) M.empty M.empty
 where
  go i h p c
    | i >= BS.length b = (p, c, h)
    | otherwise = case loadRoute h True (Just (fromIntegral i, fromIntegral (BS.index b i))) of
        Just (LwHeader k d) -> go (i + 1) (C.replace k d h) p c
        Just (LwPrg a d) -> go (i + 1) h (M.insert (fromIntegral a) (fromIntegral d) p) c
        Just (LwChr a d) -> go (i + 1) h p (M.insert (fromIntegral a) (fromIntegral d) c)
        Nothing -> go (i + 1) h p c

loaderTests :: [FilePath] -> TestTree
loaderTests roms = testGroup "cartridge loader (Nes.Loader, step 3.8)"
  [ testCase "every fetched ROM's header: Clash decoder = parseHeader + cartCfgFromHeader" $
      forM_ roms $ \path -> do
        b <- loadRomFile path
        assertEqual (takeFileName path) (reference b) (decoded b)
  , testProperty "random headers: Clash decoder = parseHeader + cartCfgFromHeader" $ H.withTests 20000 $ H.property $ do
      b <- H.forAll genHeader
      H.cover 5 (fromString "supported") (reference b /= Just Nothing)
      H.cover 2 (fromString "NES 2.0 and supported") (reference b /= Just Nothing && BS.index b 7 `mod` 16 `div` 4 == 2)
      decoded b H.=== reference b
  , testCase "every fetched ROM streamed through loadRoute: PRG and CHR as parseRom, the header kept" $
      forM_ roms $ \path -> do
        b <- loadRomFile path
        case parseRom b of
          Left e -> assertFailure (path <> ": " <> e)
          Right r | Just _ <- cartCfgFromHeader (romHeader r) -> do
            let (p, c, h) = stream b
            assertEqual (takeFileName path <> " PRG") (BS.unpack (romPrg r)) (M.elems p)
            assertEqual (takeFileName path <> " PRG addresses") [0 .. BS.length (romPrg r) - 1] (M.keys p)
            assertEqual (takeFileName path <> " CHR") (BS.unpack (romChr r)) (M.elems c)
            assertEqual (takeFileName path <> " CHR addresses") [0 .. BS.length (romChr r) - 1] (M.keys c)
            assertEqual (takeFileName path <> " header") (clashHeader b) h
          Right _ -> pure ()
  , testCase "nothing is written while the core runs" $
      loadRoute (C.repeat 0) False (Just (20, 1)) @?= Nothing
  , testCase "desktop buttons (Game Boy order) to the controller's order" $
      forM_ [0 .. 7 :: Int] $ \k ->
        -- link bit k: Right Left Up Down A B Select Start
        let pad = padFromLink (C.bit k)
            want = [7, 6, 4, 5, 0, 1, 2, 3] !! k :: Int
        in assertEqual ("link bit " <> show k) [ i == want | i <- [0 .. 7] ] [ testBit pad i | i <- [0 .. 7] ]
  ]
