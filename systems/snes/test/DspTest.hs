-- | The S-DSP's units on their own (PLAN.md step 7.4): a voice in direct
-- gain on a constant BRR block, the 5 silent samples after KON, the noise
-- generator's sequence, the attack at its fastest rate, a GAIN rate on the
-- shared counter, ENDX.
module DspTest (dspTests) where

import Prelude

import Control.Monad (forM, forM_, replicateM)
import Control.Monad.ST (ST, runST)
import Data.Bits ((.&.), shiftR, xor, (.|.), shiftL)
import qualified Data.Vector.Unboxed.Mutable as MV
import Data.Word (Word8)
import Test.Tasty
import Test.Tasty.HUnit

import Snes.Spec.Dsp

-- | ARAM: the DIR table at 0300h (entry 0: 0310h, looping to itself), a
-- BRR block at 0310h; the registers given, then KON for voice 0.
voice0 :: [Word8] -> [(Int, Word8)] -> (forall s. Dsp s -> (Int -> ST s Word8) -> (Int -> Word8 -> ST s ()) -> ST s a) -> a
voice0 block regs act = runST $ do
  ram <- MV.replicate 0x10000 0
  mapM_ (uncurry (MV.write ram)) (zip [0x300 ..] [0x10, 0x03, 0x10, 0x03])
  mapM_ (uncurry (MV.write ram)) (zip [0x310 ..] block)
  d <- newDsp
  forM_ ([(0x6C, 0x20), (0x5D, 0x03), (0x04, 0), (0x0C, 0x7F), (0x1C, 0x7F)] <> regs) $ \(a, v) -> dspWrite d a v
  dspWrite d 0x4C 0x01
  act d (MV.read ram) (MV.write ram)

samples :: Dsp s -> (Int -> ST s Word8) -> (Int -> Word8 -> ST s ()) -> Int -> ST s [(Int, Int)]
samples d r w n = replicateM n (dspSample d r w)

-- | A loop of nibbles 1, shift 12: every sample (1 << 12) >> 1 = 0800h.
flat :: [Word8]
flat = 0xC3 : replicate 8 0x11

dspTests :: TestTree
dspTests = testGroup "S-DSP"
  [ testCase "direct gain 7Fh on a constant block: ENVX 7Fh, OUTX 0Fh, a steady output" $ do
      let (envx, outx, out) = voice0 flat [(0x05, 0), (0x07, 0x7F), (0x03, 0x10), (0x00, 0x40)] $ \d r w -> do
            ss <- samples d r w 20
            e <- dspRead d 0x08
            o <- dspRead d 0x09
            pure (e, o, last ss)
      -- 0800h * 7F0h >> 11 = 07F0h (OUTX its high 8 of 15 bits: 0Fh); * 40h >> 6;
      -- * MVOL 7Fh >> 7
      (envx, outx) @?= (0x7F, 0x0F)
      -- within the Gaussian table's 7FFh-801h sums
      (abs (fst out - (0x7F0 * 0x7F) `shiftR` 7) <= 2) @? ("steady output " <> show out)
  , testCase "KON: 5 silent samples (after the poll), then sound" $ do
      let outs = voice0 flat [(0x05, 0), (0x07, 0x7F), (0x03, 0x10), (0x00, 0x40)] $ \d r w ->
            map fst <$> samples d r w 10
      -- the 5 of the delay, then a 6th: the interpolation starts at index 0,
      -- where the newest sample's weight is 0 and the older ones are still 0
      takeWhile (== 0) outs @?= replicate 6 0
      (outs !! 6 /= 0) @? "sound after the delay"
  , testCase "noise: the LFSR from 4000h, a step a sample at rate 1Fh" $ do
      let outx = voice0 flat [(0x05, 0), (0x07, 0x7F), (0x3D, 0x01), (0x6C, 0x3F)] $ \d r w -> do
            _ <- samples d r w 6
            forM [1 .. 8 :: Int] $ \_ -> dspSample d r w >> dspRead d 0x09
          lfsr n = ((n `shiftR` 1) .&. 0x3FFF) .|. (((n `xor` (n `shiftR` 1)) .&. 1) `shiftL` 14)
          seqN = drop 7 (iterate lfsr (0x4000 :: Int))
          -- OUTX: the noise as 15 bits signed, times the envelope 7F0h >> 11, high 8 bits
          expect = [ fromIntegral ((((if n >= 0x4000 then n - 0x8000 else n) * 0x7F0) `shiftR` 11) `shiftR` 7 .&. 0xFF) | n <- take 8 seqN ]
      outx @?= expect
  , testCase "ADSR: attack rate 15 (+400h a sample) reaches 7FFh and decays from there" $ do
      -- the poll and 4 delayed samples, the block's load in the 5th
      let envs = voice0 flat [(0x05, 0x8F), (0x06, 0xE0), (0x03, 0x10)] $ \d r w -> do
            _ <- samples d r w 5
            forM [1 .. 6 :: Int] $ \_ -> dspSample d r w >> dspRead d 0x08
      take 3 envs @?= [0x40, 0x7F, 0x7F]
  , testCase "GAIN: linear decrease at rate 29 steps every 3rd sample of the shared counter, whatever the pitch" $ do
      -- direct gain 7Fh to set the envelope (7F0h), then decrease -20h at
      -- rate 29 (every 3 samples); pitch 0 keeps the voice's own counter
      -- at 0, which once (wrongly) clocked the envelope: it never steps
      let envs = voice0 flat [(0x05, 0), (0x07, 0x7F), (0x02, 0), (0x03, 0)] $ \d r w -> do
            _ <- samples d r w 8
            dspWrite d 0x07 (0x80 + 29)
            forM [1 .. 30 :: Int] $ \_ -> dspSample d r w >> dspRead d 0x08
      -- ENVX is env >> 4: 7F0h - 20h k, so it falls by 2 every 3 samples
      (head envs - last envs) @?= 20
  , testCase "ENDX: set by a block with the end flag, cleared by any write" $ do
      let (a, b) = voice0 flat [(0x05, 0), (0x07, 0x7F), (0x03, 0x10)] $ \d r w -> do
            _ <- samples d r w 8
            x <- dspRead d 0x7C
            dspWrite d 0x7C 0x55
            y <- dspRead d 0x7C
            pure (x, y)
      (a .&. 1, b) @?= (1, 0)
  ]
