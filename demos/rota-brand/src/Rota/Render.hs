{-|
The sound streams played through the reference models' APUs
(@Gb.Spec.Apu@ on a machine with a blank cartridge, @Nes.Spec.Apu@ on its
own): the mixed signal at 48 kHz, and each channel's rising edges (to
measure its pitch).

A stream's frame k is written at the start of the frame: on the Game Boy
at M-cycle 17556 k (70224 dots a frame), on the NES at CPU cycle
floor (89341.5 k / 3). The Game Boy's frame sequencer follows the system
counter, which counts dots from 0 here.

__Edges.__ A channel's rising edge is where its digital level goes from 7
or less to 8 or more (pulses: from 0 to their volume; the wave channel's
sine and the triangle cross the middle once a period), as a time in
seconds.
-}
module Rota.Render
  ( Render (..)
  , renderGb
  , renderNes
  , pcmSummary
  , wavFile
  ) where

import Control.Monad (forM_, when)
import Control.Monad.ST (ST, runST)
import Data.Bits (xor, (.&.))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BL
import Data.Int (Int16)
import Data.STRef
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import Data.Word (Word16, Word64)
import Numeric (showHex)
import Text.Printf (printf)

import qualified Gb.Spec as G
import qualified Gb.Spec.Apu as GA
import qualified Nes.Mixer as NM
import qualified Nes.Spec.Apu as NA

import Rota.Sound

data Render = Render
  { rPcm :: U.Vector Int16    -- ^ mono, 48 kHz
  , rEdges :: [U.Vector Double] -- ^ per channel (pulse 1, pulse 2, wave/triangle, noise): rising edges, s
  }

sampleRate :: Int
sampleRate = 48000

-- | Rising edges, one growable list per channel, newest first.
type EdgeRefs s = [STRef s ([Double], Int)]

edge :: STRef s ([Double], Int) -> Double -> Int -> ST s ()
edge ref t lvl = do
  (es, old) <- readSTRef ref
  when (old <= 7 && lvl >= 8) (writeSTRef ref (t : es, lvl))
  when (not (old <= 7 && lvl >= 8)) (writeSTRef ref (es, lvl))

finish :: EdgeRefs s -> ST s [U.Vector Double]
finish = mapM (fmap (U.fromList . reverse . fst) . readSTRef)

-- | Play a Game Boy stream, then @tailFrames@ more frames.
renderGb :: Stream -> Int -> Render
renderGb s tailFrames = runST $ do
  em <- G.newMachine blankRom
  m <- either (error . ("renderGb: " <>)) pure em
  out <- MU.new (n * perFrame `div` 4 + 1)
  refs <- mapM (const (newSTRef ([], 0))) [0 .. 3 :: Int]
  acc <- newSTRef (0 :: Int)
  cnt <- newSTRef (0 :: Int)
  forM_ [0 .. n - 1] $ \k -> do
    forM_ (if k < length s then s !! k else []) $ \w ->
      GA.apuWrite m (fromIntegral (wReg w)) (wVal w)
    forM_ [0 .. perFrame - 1] $ \j -> do
      let c = (k * perFrame + j) * 4
      GA.apuTick m (c .&. 0xFFFF) ((c + 4) .&. 0xFFFF)
      outs <- GA.channelOutputs m
      let t = fromIntegral (k * perFrame + j) / 1048576 :: Double
      sequence_ [ edge r t (if d then l else 0) | (r, (l, d)) <- zip refs outs ]
      a <- (+ sampleRate) <$> readSTRef acc
      if a >= 1048576
        then do
          writeSTRef acc (a - 1048576)
          (l, r) <- G.audioSample m
          i <- readSTRef cnt
          MU.write out i (fromIntegral ((l + r) * 16))
          writeSTRef cnt (i + 1)
        else writeSTRef acc a
  i <- readSTRef cnt
  pcm <- U.freeze (MU.take i out)
  Render pcm <$> finish refs
 where
  n = length s + tailFrames
  perFrame = 17556

-- | 32 KiB, no mapper, otherwise zero: enough for 'G.newMachine'.
blankRom :: BS.ByteString
blankRom = BS.replicate 0x8000 0

-- | Play an NES stream, then @tailFrames@ more frames.
renderNes :: Stream -> Int -> Render
renderNes s tailFrames = runST $ do
  a <- NA.newApu
  out <- MU.new (total `div` 37 + 1)
  refs <- mapM (const (newSTRef ([], 0))) [0 .. 3 :: Int]
  acc <- newSTRef (0 :: Int)
  cnt <- newSTRef (0 :: Int)
  let frameStart k = (k * 893415) `div` 30
  forM_ [0 .. n - 1] $ \k -> do
    forM_ (if k < length s then s !! k else []) $ \w ->
      NA.apuWrite a (fromIntegral (wReg w) :: Word16) (wVal w)
    forM_ [frameStart k .. frameStart (k + 1) - 1] $ \c -> do
      NA.apuCycle a
      ch <- NA.apuChannels a
      let t = fromIntegral c / 1789773 :: Double
          lv = [NA.chPulse1 ch, NA.chPulse2 ch, NA.chTriangle ch, NA.chNoise ch]
      sequence_ [ edge r t (fromIntegral l) | (r, l) <- zip refs lv ]
      x <- (+ sampleRate) <$> readSTRef acc
      if x >= 1789773
        then do
          writeSTRef acc (x - 1789773)
          let v = NM.mixLevel (fromIntegral (NA.chPulse1 ch)) (fromIntegral (NA.chPulse2 ch))
                    (fromIntegral (NA.chTriangle ch)) (fromIntegral (NA.chNoise ch)) (fromIntegral (NA.chDmc ch))
          i <- readSTRef cnt
          MU.write out i (fromIntegral (v - 16384))
          writeSTRef cnt (i + 1)
        else writeSTRef acc x
  i <- readSTRef cnt
  pcm <- U.freeze (MU.take i out)
  Render pcm <$> finish refs
 where
  n = length s + tailFrames
  total = (n * 893415) `div` 30

-- | A text summary for a golden: the length, an FNV-1a hash of the
-- samples, and the RMS level of each 100 ms.
pcmSummary :: U.Vector Int16 -> String
pcmSummary v = unlines $
  [ "samples " <> show (U.length v) <> " at 48 kHz"
  , "fnv1a64 " <> showHex hash "" ]
  <> [ printf "%5.1f s  rms %6.0f" (fromIntegral i * 0.1 :: Double) (rms i) | i <- [0 .. U.length v `div` 4800 - 1] ]
 where
  hash = U.foldl' (\h x -> (h `xor` fromIntegral (fromIntegral x :: Word16)) * 0x100000001b3) (0xcbf29ce484222325 :: Word64) v
  rms :: Int -> Double
  rms i = let w = U.slice (i * 4800) 4800 v
              m = U.sum (U.map fromIntegral w) / 4800 :: Double
          in sqrt (U.sum (U.map (\x -> (fromIntegral x - m) ^ (2 :: Int)) w) / 4800)

-- | A 16-bit mono WAV file at 48 kHz.
wavFile :: U.Vector Int16 -> BL.ByteString
wavFile v = B.toLazyByteString $ mconcat
  [ B.string7 "RIFF", B.word32LE (36 + len), B.string7 "WAVE"
  , B.string7 "fmt ", B.word32LE 16, B.word16LE 1, B.word16LE 1
  , B.word32LE 48000, B.word32LE 96000, B.word16LE 2, B.word16LE 16
  , B.string7 "data", B.word32LE len
  , U.foldr (\x b -> B.int16LE x <> b) mempty v ]
 where
  len = fromIntegral (2 * U.length v)
