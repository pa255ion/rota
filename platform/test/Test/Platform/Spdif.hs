-- | S/PDIF transmitter (Platform.Audio.Spdif) against an independent
-- decoder written from IEC 60958: preambles, biphase-mark, parity,
-- samples, channel status.
module Test.Platform.Spdif (tests, decodeSpdif, Subframe (..)) where

import Prelude

import Data.Bits (xor)
import Data.List (group, isPrefixOf)
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog (testProperty)
import Test.Tasty.HUnit

import Platform.Audio.Spdif

data Subframe = Subframe
  { sfPre    :: Char      -- ^ 'B', 'M' or 'W'
  , sfBits   :: [Bool]    -- ^ slots 4 to 31
  } deriving stock (Show, Eq)

-- | Decode a cell stream (one level per cell) into subframes; 'Left' at
-- the first violation (an unknown preamble, a data slot without a
-- transition at its start, odd parity).
decodeSpdif :: [Bool] -> Either String [Subframe]
decodeSpdif = go False []
 where
  go _ acc cs | length (take 64 cs) < 64 = Right (reverse acc)
  go prev acc cs = do
    let (sub, rest) = splitAt 64 cs
        pre = take 8 sub
        pat = map (/= prev) pre -- relative to the level before
    p <- case map (\b -> if b then '1' else '0') pat of
      "11101000" -> Right 'B'
      "11100010" -> Right 'M'
      "11100100" -> Right 'W'
      other -> Left ("bad preamble " <> other)
    let lvl0 = last pre
        slots = pairs (drop 8 sub)
        step (l, bits) (a, b)
          | a == l = Left "no transition at a slot start"
          | otherwise = Right (b, bits <> [a /= b])
    (lvlEnd, bits) <- foldl' (\e x -> e >>= \st -> step st x) (Right (lvl0, [])) slots
    if foldl' xor False bits then Left "odd parity" else go lvlEnd (Subframe p bits : acc) rest
  pairs (a : b : r) = (a, b) : pairs r
  pairs _ = []

sampleOf :: Subframe -> Int
sampleOf sf = let v = sum [ 2 ^ i | (i, True) <- zip [0 :: Int ..] (take 16 (drop 8 (sfBits sf))) ]
              in if v >= 32768 then v - 65536 else v

-- | Run the transmitter with the enable always on, holding each sample
-- pair for one frame (128 cells); the cells.
cellsFor :: [(Int, Int)] -> [Bool]
cellsFor smps = map (== 1) (go spdifInit inputs)
 where
  inputs = concatMap (\(l, r) -> replicate 128 (True, (fromIntegral l, fromIntegral r))) smps
  go _ [] = []
  go s (i : is) = let (s', o) = spdifStep s i in o : go s' is

tests :: TestTree
tests = testGroup "S/PDIF transmitter"
  [ testProperty "samples survive the round trip; every subframe well formed" $ H.withTests 50 $ H.property $ do
      smps <- H.forAll (Gen.list (Range.linear 1 40) ((,) <$> Gen.int (Range.linearFrom 0 (-32768) 32767)
                                                          <*> Gen.int (Range.linearFrom 0 (-32768) 32767)))
      sfs <- either (\e -> H.annotate e >> H.failure) pure (decodeSpdif (cellsFor smps))
      map sfPre sfs H.=== concat [ [if f `mod` 192 == 0 then 'B' else 'M', 'W'] | f <- [0 .. length smps - 1] ]
      [ (sampleOf a, sampleOf b) | (a, b) <- pairsOf sfs ] H.=== smps
  , testCase "a 192-frame block: B once, channel status says PCM, 48 kHz, 24 bits" $ do
      let sfs = either error id (decodeSpdif (cellsFor (replicate 192 (0, 0))))
          lefts = [ s | (s, i) <- zip sfs [0 :: Int ..], even i ]
          cs = map (\s -> sfBits s !! 26) lefts -- slot 30
      map sfPre lefts @?= 'B' : replicate 191 'M'
      [ n | (n, True) <- zip [0 :: Int ..] cs ] @?= [2, 25, 32, 33, 35]
      let rights = [ s | (s, i) <- zip sfs [0 :: Int ..], odd i ]
      map (\s -> sfBits s !! 26) rights @?= cs -- the same C bit in both subframes
  , testCase "on the pin (two halves per pixel clock): decodes, 48 kHz, transitions within half a clock" $ do
      -- 4000 pixel clocks per sample pair change; ~0.25 s of line
      let clocks = 25176 * 10 :: Int
          smpAt n = let k = n `div` 4000 in (fromIntegral (k * 37 - 900), fromIntegral (500 - k * 11))
          go _ n | n >= clocks = []
          go st n = let (st', (a, b)) = spdifDdrStep st (smpAt n) in a : b : go st' (n + 1)
          halves = dropWhile not (map (== 1) (go (0, spdifInit) 0)) -- idle low before the first cell
          runs = map length (group halves)
          -- 8.195 halves per cell: a run of k cells is 8k or 8k + 1 halves
          cellsOf len = round (fromIntegral len * (6144000 * 19.86e-9) :: Double) :: Int
          cells = concat [ replicate (cellsOf len) v | (len, v) <- zip runs [ v | v : _ <- group halves ] ]
      filter (\len -> let k = cellsOf len in len /= 8 * k && len /= 8 * k + 1 && len /= 8 * k + 2) (init runs) @?= []
      sfs <- either assertFailure pure (decodeSpdif cells)
      -- 10 ms of 48 kHz = 480 frames (± 1)
      let frames = length sfs `div` 2
      assertBool ("frames: " <> show frames) (abs (frames - 480) <= 1)
      map sfPre (take 4 sfs) @?= "BWMW"
      -- each sample pair appears, in order, from the first frame on
      let got = [ (sampleOf a, sampleOf b) | (a, b) <- pairsOf sfs ]
          want = [ (k * 37 - 900, 500 - k * 11) | k <- [0 .. clocks `div` 4000] ]
      assertBool "samples in order" (all (`elem` want) got && map fst (dedup got) `isPrefixOf'` map fst want)

  , testCase "the line holds its level while the enable is low" $ do
      let (s1, o1) = spdifStep spdifInit (True, (5, 6))
          (s2, o2) = spdifStep s1 (False, (5, 6))
      (o2, s2) @?= (o1, s1)
  ]
 where
  pairsOf (a : b : r) = (a, b) : pairsOf r
  pairsOf _ = []
  dedup (a : b : r) | a == b = dedup (b : r)
  dedup (a : r) = a : dedup r
  dedup [] = []
  isPrefixOf' = isPrefixOf
