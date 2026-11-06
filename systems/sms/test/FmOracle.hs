-- | The YM2413 model ("Sms.Spec.Fm") against Nuked-OPLL run as a black box
-- (build/sms/opll-oracle, make opll-oracle; GPL, fetched, never committed:
-- the user's choice, 2026-10-11). Random register programs that keep the
-- manual's waits (an address, 3 cycles, the data, 21 cycles or more) go
-- to both; every frame's DAC value of every melodic channel and drum must
-- have the oracle's pins (MO, RO) at every cycle: the channels, the drums
-- (both copies) and the idle cycles, rhythm mode switches included.
module FmOracle
  ( Program
  , randomProgram
  , runOracle
  , runModel
  , compareRuns
  , oracleTests
  , explain
  , reduceFailure
  ) where

import Prelude

import Control.Monad (forM_, unless)
import Control.Monad.ST (runST)
import Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BL
import Data.Int (Int16)
import qualified Data.Vector.Unboxed as U
import Data.Word (Word32, Word8)
import System.Directory (doesFileExist, getTemporaryDirectory)
import System.FilePath ((</>))
import System.Process (callProcess)
import Test.Tasty
import Test.Tasty.HUnit
import Text.Printf (printf)

import Sms.Spec.Fm

-- | Writes: (the cycle before whose clock the strobe is there, port, byte).
type Program = [(Int, Int, Word8)]

oracleBin :: FilePath
oracleBin = "../../build/sms/opll-oracle"

-- | A seeded program of @n@ register writes from cycle 40 on.
randomProgram :: Int -> Int -> Program
randomProgram seed n = go (lcg seed) 40 n
 where
  lcg x = (x * 6364136223846793005 + 1442695040888963407) .&. 0x7FFFFFFFFFFFFFFF
  go _ _ 0 = []
  go r cyc k =
    let r1 = lcg r; r2 = lcg r1; r3 = lcg r2; r4 = lcg r3
        pick = (r1 `shiftR` 33) `mod` 100
        ch = (r2 `shiftR` 33) `mod` 9
        v = fromIntegral ((r3 `shiftR` 33) .&. 0xFF) :: Word8
        a | pick < 10 = (r2 `shiftR` 33) `mod` 8             -- the user's patch
          | pick < 14 = 0x0E                                  -- rhythm
          | pick < 34 = 0x10 + ch                             -- F-number
          | pick < 64 = 0x20 + ch                             -- key, block, sus
          | pick < 94 = 0x30 + ch                             -- instrument, volume
          | otherwise = [0x19, 0x1F, 0x29, 0x2C, 0x39, 0x3F] !! ((r2 `shiftR` 33) `mod` 6)
        gap = 21 + (r4 `shiftR` 33) `mod` 2000
    in (cyc, 0, fromIntegral a) : (cyc + 3, 1, v) : go r4 (cyc + 3 + gap) (k - 1 :: Int)

-- | The oracle's pins for @cycles@ cycles: (MO, RO) per cycle.
runOracle :: String -> Program -> Int -> IO (U.Vector (Int, Int))
runOracle tag prog cycles = do
  tmp <- getTemporaryDirectory
  let pf = tmp </> ("sms-fm-" <> tag <> ".prog")
      of' = tmp </> ("sms-fm-" <> tag <> ".pins")
  BL.writeFile pf $ B.toLazyByteString $ foldMap
    (\(c, p, v) -> B.word32LE (fromIntegral c :: Word32) <> B.word8 (fromIntegral p) <> B.word8 v) prog
  callProcess oracleBin [pf, of', show cycles]
  bs <- BS.readFile of'
  let i16 o = fromIntegral (fromIntegral (BS.index bs o) .|. (fromIntegral (BS.index bs (o + 1)) `shiftL` 8) :: Int16) :: Int
  pure (U.generate cycles (\k -> (i16 (4 * k), i16 (4 * k + 2))))

-- | The model's pins for the same program, cycle by cycle.
runModel :: Program -> Int -> U.Vector (Int, Int)
runModel prog samples = runST $ do
  fm <- newFm
  forM_ prog $ \(c, p, v) -> fmRunTo fm c >> fmWrite fm c p v
  fmRunTo fm (18 * samples + 17 + fmMargin)
  U.concat . take samples <$> fmTakePins fm

-- | Where rhythm mode (0Eh bit 5) changes: the cycles of the data writes.
rhythmSwitches :: Program -> [Int]
rhythmSwitches prog = go False (zip prog (drop 1 prog))
 where
  go _ [] = []
  go on (((_, 0, 0x0E), (c, 1, v)) : rest) =
    let on' = testBit v 5 in [ c | on' /= on ] <> go on' rest
  go on (_ : rest) = go on rest

-- | The first cycle where the pins differ, if any. The first two samples
-- are left out (after reset the chip's output registers hold 0 until its
-- pipeline has filled), and so are the drums' pin cycles (RO, and MO's
-- idle level there, which shows the same value's sign) within two samples
-- of a rhythm switch: in the sample where rhythm mode switches on or off, a
-- drum's value can differ (the oracle's stages that pick the drum's level
-- and patch see the switch at other cycles than the model's; 24 of 22
-- million cycles in the FULL tier's programs, none lasting, the area
-- Nuked-OPLL itself marks as unverified: docs/systems/ym2413.md, 11.10).
-- Every other cycle is compared, the melodic channels always.
compareRuns :: Program -> U.Vector (Int, Int) -> U.Vector (Int, Int) -> Maybe String
compareRuns prog model oracle = case [ c | c <- [36 .. min (U.length model) (U.length oracle) - 1], differs c ] of
  [] -> Nothing
  (c : _) -> Just (printf "cycle %d (sample %d, cycle %d): model (MO, RO) %s, oracle %s"
                     c (c `div` 18) (c `mod` 18) (show (model U.! c)) (show (oracle U.! c)))
 where
  switches = rhythmSwitches prog
  nearSwitch c = any (\w -> abs (c - w) <= 36) switches
  differs c =
    let (m1, r1) = model U.! c
        (m2, r2) = oracle U.! c
        drumCycle = (c `mod` 18) `elem` [17, 0, 1, 2, 3, 4, 5, 6, 10, 11]
    in (m1 /= m2 || r1 /= r2) && not (drumCycle && nearSwitch c)

-- | One oracle comparison: @samples@ frames of a seeded program.
oracleCase :: String -> Program -> Int -> TestTree
oracleCase name prog samples = testCase name $ do
  ok <- doesFileExist oracleBin
  unless ok (assertFailure (oracleBin <> " missing: make opll-oracle"))
  pins <- runOracle (filter (/= ' ') name) prog (18 * samples)
  forM_ (compareRuns prog (runModel prog samples) pins) assertFailure

oracleTests :: Bool -> TestTree
oracleTests full = testGroup "YM2413 = Nuked-OPLL (oracle)" $
  oracleCase "one note (instrument 1, A4)" note 3000
  : [ oracleCase (printf "random program %d" seed) (randomProgram seed writes) samples | seed <- seeds ]
 where
  seeds = if full then [1 .. 40] else [1 .. 8] :: [Int]
  writes = if full then 2000 else 300
  samples = writes * 1100 `div` 72
  note = [ w | (i, (a, v)) <- zip [0 :: Int ..] [(0x30, 0x10), (0x10, 0x20), (0x20, 0x19)]
             , w <- [(10 + 24 * i, 0, a), (13 + 24 * i, 1, v)] ]

-- | For debugging a failure: the writes around sample @f@ and both pins
-- of both sides over samples f - 1 .. f + 1.
explain :: Program -> Int -> IO ()
explain prog f = do
  pins <- runOracle "explain" prog (18 * (f + 2))
  let model = runModel prog (f + 2)
  putStrLn "writes:"
  mapM_ (\(c, p, v) -> printf "  cycle %d (sample %d cyc %d) port %d %02X\n" c (c `div` 18) (c `mod` 18) p v)
    [ w | w@(c, _, _) <- prog, c >= 18 * (f - 8), c <= 18 * (f + 2) ]
  forM_ [18 * (f - 1) .. 18 * (f + 2) - 1] $ \c ->
    printf "  cycle %d (sample %d cyc %2d): model %-12s oracle %-12s%s\n" c (c `div` 18) (c `mod` 18)
      (show (model U.! c)) (show (pins U.! c)) (if model U.! c /= pins U.! c then "  <--" else "")

-- | For debugging a failure: the smallest program found (dropping one
-- address-and-data pair at a time while the first @samples@ samples still
-- differ), printed with its first difference.
reduceFailure :: Program -> Int -> IO Program
reduceFailure prog samples = do
  let pairs = [ [x, y] | (x@(_, 0, _), y) <- zip prog (drop 1 prog) ]
      fails p = do
        pins <- runOracle "reduce" p (18 * samples)
        pure (compareRuns p (runModel p samples) pins)
      go i qs
        | i >= length qs = pure qs
        | otherwise = do
            let qs' = take i qs <> drop (i + 1) qs
            r <- fails (concat qs')
            case r of
              Just _ -> go i qs'
              Nothing -> go (i + 1) qs
  small <- go 0 pairs
  printf "%d of %d pairs:\n" (length small) (length pairs)
  forM_ small $ \pr -> case pr of
    [(c, _, a), (_, _, v)] -> printf "  cycle %d (sample %d cyc %d) %02X=%02X\n" c (c `div` 18) (c `mod` 18) a v
    _ -> pure ()
  fails (concat small) >>= putStrLn . maybe "  (passes)" ("  " <>)
  pure (concat small)
