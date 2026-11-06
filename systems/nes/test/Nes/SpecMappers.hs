{-# LANGUAGE RankNTypes #-}
-- | Step 3.2: the mappers on made-up cartridges (each PRG bank filled with
-- its own number), and the controllers through a ca65 ROM.
module Nes.SpecMappers
  ( mapperTests
  , controllerTests
  ) where

import Prelude

import Control.Monad (forM, forM_, replicateM_)
import Control.Monad.ST (ST, runST)
import Data.Bits ((.&.), shiftL, testBit)
import qualified Data.ByteString as BS
import Data.Word
import System.FilePath ((</>))
import Test.Tasty
import Test.Tasty.HUnit

import Nes.Cartridge
import Nes.Hooks
import Nes.Spec.Machine
import Nes.Spec.Mapper
import Nes.TestRoms

-- | An iNES image: mapper, flags 6 low bits, PRG in 16 KiB units, CHR in
-- 8 KiB units; PRG filled per 16 KiB bank and CHR per 4 KiB bank with the
-- bank number.
image :: Int -> Word8 -> Int -> Int -> BS.ByteString
image mapper f6 prg chr = BS.concat
  [ BS.pack ([0x4E, 0x45, 0x53, 0x1A, fromIntegral prg, fromIntegral chr
             , fromIntegral (mapper `shiftL` 4) .|. f6, fromIntegral (mapper .&. 0xF0)] <> replicate 8 0)
  , BS.concat [BS.replicate 0x4000 (fromIntegral b) | b <- [0 .. prg - 1]]
  , BS.concat [BS.replicate 0x1000 (fromIntegral b) | b <- [0 .. 2 * chr - 1]]
  ]
 where
  a .|. b = a + b

withMapper :: BS.ByteString -> (forall s. Mapper s -> ST s a) -> a
withMapper file act = runST $ do
  let rom = either error id (parseRom file)
  m <- either error id <$> newMapper rom
  act m

rd :: Mapper s -> Word16 -> ST s Int
rd = cpuReadCart

-- | MMC1 serial write of a 5-bit value, one write every other cycle.
mmc1 :: Mapper s -> Int -> Word16 -> Int -> ST s Int
mmc1 m cyc a v = do
  forM_ [0 .. 4] $ \i -> cpuWriteCart m (cyc + 2 * i) a (if testBit v i then 1 else 0)
  pure (cyc + 10)

mapperTests :: TestTree
mapperTests = testGroup "mappers"
  [ testCase "NROM-128: 16 KiB mirrored at $C000; PRG RAM at $6000" $
      withMapper (image 0 0 1 1) (\m -> do
        cpuWriteCart m 0 0x6123 0x5A
        (,,) <$> rd m 0x8000 <*> rd m 0xC000 <*> rd m 0x6123) @?= (0, 0, 0x5A)
  , testCase "nothing answers at $4020-$5FFF (open bus)" $
      withMapper (image 0 0 1 1) (\m -> rd m 0x5000) @?= (-1)
  , testCase "UxROM: switchable $8000, last bank fixed at $C000" $
      withMapper (image 2 0 8 0) (\m -> do
        cpuWriteCart m 0 0x8000 3
        (,) <$> rd m 0x8000 <*> rd m 0xFFFF) @?= (3, 7)
  , testCase "UxROM: CHR RAM is writable" $
      withMapper (image 2 0 2 0) (\m -> do
        chrWrite m 0x1234 0x99
        chrRead m 0x1234) @?= 0x99
  , testCase "CNROM: 8 KiB CHR banks, CHR ROM not writable" $
      withMapper (image 3 0 2 4) (\m -> do
        cpuWriteCart m 0 0x8000 2
        chrWrite m 0x0000 0x99
        (,) <$> chrRead m 0x0000 <*> chrRead m 0x1000) @?= (4, 5)
  , testCase "AxROM: 32 KiB banks, one-screen nametable from bit 4" $
      withMapper (image 7 0 16 0) (\m -> do
        cpuWriteCart m 0 0x8000 0x15
        a <- (,) <$> rd m 0x8000 <*> rd m 0xC000
        n1 <- mapM (ciramIndex m) [0x2000, 0x2400, 0x2C05]
        cpuWriteCart m 0 0x8000 0x05
        n0 <- ciramIndex m 0x2400
        pure (a, n1, n0)) @?= ((10, 11), [0x400, 0x400, 0x405], 0)
  , testCase "fixed mirroring from the header" $ do
      withMapper (image 0 1 1 1) (\m -> mapM (ciramIndex m) [0x2000, 0x2400, 0x2800, 0x2C00])
        @?= [0, 0x400, 0, 0x400]
      withMapper (image 0 0 1 1) (\m -> mapM (ciramIndex m) [0x2000, 0x2400, 0x2800, 0x2C00])
        @?= [0, 0, 0x400, 0x400]
  , testCase "MMC1: powers up in PRG mode 3 (last bank at $C000)" $
      withMapper (image 1 0 16 0) (\m -> (,) <$> rd m 0x8000 <*> rd m 0xC000) @?= (0, 15)
  , testCase "MMC1: PRG modes" $
      withMapper (image 1 0 16 0) (\m -> do
        c <- mmc1 m 100 0xE000 5
        mode3 <- (,) <$> rd m 0x8000 <*> rd m 0xC000
        c' <- mmc1 m c 0x8000 0x08            -- PRG mode 2: fix first, switch $C000
        mode2 <- (,) <$> rd m 0x8000 <*> rd m 0xC000
        _ <- mmc1 m c' 0x8000 0x00            -- 32 KiB: bank & ~1 = 4
        mode0 <- (,) <$> rd m 0x8000 <*> rd m 0xC000
        pure (mode3, mode2, mode0)) @?= ((5, 15), (0, 5), (4, 5))
  , testCase "MMC1: a write right after a write is ignored (RMW)" $
      withMapper (image 1 0 16 0) (\m -> do
        -- Five writes of 1, but the second of each back-to-back pair is lost.
        forM_ [0 .. 4] $ \i -> do
          cpuWriteCart m (100 + 3 * i) 0xE000 1
          cpuWriteCart m (101 + 3 * i) 0xE000 0
        rd m 0x8000) @?= 31 .&. 15
  , testCase "MMC1: bit 7 resets the shift register" $
      withMapper (image 1 0 16 0) (\m -> do
        cpuWriteCart m 10 0xE000 1
        cpuWriteCart m 12 0xE000 1
        cpuWriteCart m 14 0xE000 0x80
        _ <- mmc1 m 20 0xE000 2
        rd m 0x8000) @?= 2
  , testCase "MMC1: mirroring and 4 KiB CHR banks" $
      withMapper (image 1 0 2 4) (\m -> do
        c <- mmc1 m 10 0x8000 0x12            -- vertical, CHR 4 KiB mode
        c' <- mmc1 m c 0xA000 3
        _ <- mmc1 m c' 0xC000 6
        v <- mapM (ciramIndex m) [0x2000, 0x2400, 0x2800]
        chr <- (,) <$> chrRead m 0x0000 <*> chrRead m 0x1000
        pure (v, chr)) @?= ([0, 0x400, 0], (3, 6))
  , testCase "unsupported mapper refused" $
      (checkSupported . romHeader <$> parseRom (image 9 0 2 2)) @?= Right (Left "mapper 9 not supported")
  ]

-- | joypad.s reads both ports ten times after a strobe and stores what it
-- read at $6010 and $6020.
controllerTests :: TestTree
controllerTests = testCase "controllers: A first, 1s after 8 reads, bits 5-7 open bus" $ do
  file <- loadRomFile (asmRoot </> "joypad.nes")
  let (st, p1, p2) = runST $ do
        m <- either error id <$> newMachine file
        setButtons m 1 0x5A
        setButtons m 2 0x81
        replicateM_ 20000 (stepCpu m)
        s <- peekCpu m 0x6000
        a <- forM [0x6010 .. 0x6019] (peekCpu m)
        b <- forM [0x6020 .. 0x6029] (peekCpu m)
        pure (s, a, b)
      expect btn = [0x40 + (if testBit (btn :: Int) i then 1 else 0) | i <- [0 .. 7]] <> [0x41, 0x41]
  st @?= 0
  p1 @?= expect 0x5A
  p2 @?= expect 0x81
  -- (The protocol hook sees the same result.)
  blargg (\a -> if a == 0x6000 then st else if a == 0x6001 then 0xDE else if a == 0x6002 then 0xB0 else if a == 0x6003 then 0x61 else 0)
    @?= Done 0 ""
