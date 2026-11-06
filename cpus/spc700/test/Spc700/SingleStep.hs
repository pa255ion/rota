{-# LANGUAGE OverloadedStrings #-}
{-|
SingleStepTests for the SPC700 (github.com/SingleStepTests/spc700, MIT;
fetched by tools/fetch-roms.sh into roms/spc700/v1): one JSON file per
opcode, 1000 cases each. A case gives the registers and the RAM it
touches before and after one instruction (a flat 64 KiB address space),
and every cycle as address, data and kind: a read, a write, or a "wait"
(an internal cycle; its address and data are 'Nothing' when the file has
null).

'runCase' runs a case on "Spc700.Spec" and returns the differences
(empty = pass): the registers, the RAM, and every cycle's address, kind
and data (where the file records it). SLEEP and STOP run on, halted,
for the cycles the file records.
-}
module Spc700.SingleStep
  ( Case (..)
  , CpuState (..)
  , Cycle (..)
  , Kind (..)
  , loadCases
  , loadCasesUpTo
  , fullTests
  , quickCases
  , runCase
  , judge
  ) where

import Prelude

import Control.Monad (forM_, when)
import Control.Monad.ST (runST)
import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.IntMap.Strict as IM
import Data.STRef
import Data.Word
import Numeric (showHex)
import System.Environment (lookupEnv)

import Spc700.Spec
import Spc700.Types

data CpuState = CpuState
  { csRegs :: Regs
  , csRam  :: [(Word16, Word8)]
  } deriving stock (Show)

data Kind = Read | Write | Wait
  deriving stock (Show, Eq)

-- | Address, data, kind.
data Cycle = Cycle !(Maybe Word16) !(Maybe Word8) !Kind
  deriving stock (Show, Eq)

data Case = Case
  { caseName    :: String
  , caseInitial :: CpuState
  , caseFinal   :: CpuState
  , caseCycles  :: [Cycle]
  } deriving stock (Show)

instance FromJSON CpuState where
  parseJSON = withObject "state" $ \o -> do
    let w8 k = (fromIntegral :: Int -> Word8) <$> o .: k
    rs <- Regs <$> w8 "a" <*> w8 "x" <*> w8 "y" <*> w8 "sp"
               <*> ((fromIntegral :: Int -> Word16) <$> o .: "pc") <*> w8 "psw"
    ram <- map pair <$> o .: "ram"
    pure (CpuState rs ram)
   where
    pair :: (Int, Int) -> (Word16, Word8)
    pair (a, v) = (fromIntegral a, fromIntegral v)

instance FromJSON Cycle where
  parseJSON v = do
    (a, d, k) <- parseJSON v
    kind <- case (k :: String) of
      "read" -> pure Read
      "write" -> pure Write
      "wait" -> pure Wait
      _ -> fail ("bad cycle kind " <> show k)
    pure (Cycle (fromIntegral <$> (a :: Maybe Int)) (fromIntegral <$> (d :: Maybe Int)) kind)

instance FromJSON Case where
  parseJSON = withObject "case" $ \o ->
    Case <$> o .: "name" <*> o .: "initial" <*> o .: "final" <*> o .: "cycles"

loadCases :: FilePath -> IO [Case]
loadCases p = do
  r <- eitherDecode' <$> BL.readFile p
  either (\e -> fail (p <> ": " <> e)) pure r

-- | The first @n@ cases only ('Nothing': all).
loadCasesUpTo :: Maybe Int -> FilePath -> IO [Case]
loadCasesUpTo n p = maybe id take n <$> loadCases p

-- | The test tier (docs/TESTING.md, "Tiers"): EMU_FULL_TESTS=1 (make
-- test FULL=1) runs the full sets; otherwise the quick tier.
fullTests :: IO Bool
fullTests = (== Just "1") <$> lookupEnv "EMU_FULL_TESTS"

-- | SingleStepTests cases per opcode file in the quick tier.
quickCases :: Int
quickCases = 100

-- | Differences between "Spc700.Spec" and the case; empty when it passes.
runCase :: Case -> [String]
runCase tc = runST $ do
  let ini = caseInitial tc
  ram <- newSTRef (IM.fromList [(fromIntegral a, v) | (a, v) <- csRam ini])
  trace <- newSTRef []
  let note cy = modifySTRef' trace (cy :)
      bus = Bus
        { busRead = \a -> do
            m <- readSTRef ram
            -- the file lists no RAM for a dummy read (its data is null);
            -- a real read of an unlisted address fails the cycle check
            let v = IM.findWithDefault 0 (fromIntegral a) m
            note (Cycle (Just a) (Just v) Read)
            pure v
        , busWrite = \a v -> modifySTRef' ram (IM.insert (fromIntegral a) v) >> note (Cycle (Just a) (Just v) Write)
        , busIdle = note (Cycle Nothing Nothing Wait)
        }
  cpu <- newCpu bus (csRegs ini)
  _ <- step cpu
  let want = caseCycles tc
      more = do
        h <- cpuHalted cpu
        n <- cpuCycles cpu
        when (h && n < length want) (step cpu >> more)
  more
  r <- getRegs cpu
  m <- readSTRef ram
  got <- reverse <$> readSTRef trace
  pure (judge tc r m got)

-- | The differences between a run's outcome (registers, RAM, cycles) and
-- the case's.
judge :: Case -> Regs -> IM.IntMap Word8 -> [Cycle] -> [String]
judge tc r m got = runST $ do
  errs <- newSTRef []
  let bad e = modifySTRef' errs (e :)
      fin = caseFinal tc
      want = caseCycles tc
      w = csRegs fin
      regs = [ (nm, gv, ev) | (nm, gv, ev) <-
                 [ ("A", f rA r, f rA w), ("X", f rX r, f rX w), ("Y", f rY r, f rY w)
                 , ("SP", f rSP r, f rSP w), ("PSW", f rPSW r, f rPSW w)
                 , ("PC", fromIntegral (rPC r), fromIntegral (rPC w)) ]
             , gv /= ev ]
      f :: (Regs -> Word8) -> Regs -> Int
      f sel = fromIntegral . sel
  forM_ regs $ \(nm, gv, ev) -> bad (nm <> " = " <> showHex gv "" <> ", expected " <> showHex ev "")
  forM_ (csRam fin) $ \(a, v) -> do
    let gv = IM.findWithDefault 0 (fromIntegral a) m
    when (gv /= v) (bad ("RAM " <> showHex a "" <> " = " <> showHex gv "" <> ", expected " <> showHex v ""))
  let same (Cycle a d k) (Cycle a' d' k') = a == a' && k == k' && (d' == Nothing || d == d')
  when (length got /= length want || or (zipWith (\x y -> not (same x y)) got want)) $
    bad ("cycles: got " <> show got <> "\n  expected " <> show want)
  reverse <$> readSTRef errs
