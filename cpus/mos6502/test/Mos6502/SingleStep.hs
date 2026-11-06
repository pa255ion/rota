{-# LANGUAGE OverloadedStrings #-}
{-|
Tom Harte's SingleStepTests for the 6502 family
(github.com/SingleStepTests/65x02, MIT; fetched by tools/fetch-roms.sh):
one JSON file per opcode, 10 000 cases each. A case gives the registers
and the RAM it touches before and after one instruction, and the bus
access of every cycle. Memory outside the listed addresses must not be
accessed (writes may go to addresses only listed afterwards).

'runCase' runs one case on 'Mos6502.Spec' and returns the differences
(empty = pass): registers, RAM, and the cycle list, compared cycle by
cycle (address, data, read/write).
-}
module Mos6502.SingleStep
  ( Case (..)
  , CpuState (..)
  , Access (..)
  , loadCases
  , loadCasesUpTo
  , fullTests
  , quickCases
  , runCase
  ) where

import Prelude

import Control.Monad (forM_, when)
import Control.Monad.ST (runST)
import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BC
import System.Environment (lookupEnv)
import qualified Data.IntMap.Strict as IM
import Data.STRef
import Data.Word

import Mos6502.Spec

data CpuState = CpuState
  { csPC :: !Word16
  , csS, csA, csX, csY, csP :: !Word8
  , csRam :: [(Word16, Word8)]
  } deriving stock (Show)

data Access = Access !Word16 !Word8 !Bool -- ^ address, data, write
  deriving stock (Show, Eq)

data Case = Case
  { caseName    :: String
  , caseInitial :: CpuState
  , caseFinal   :: CpuState
  , caseCycles  :: [Access]
  } deriving stock (Show)

instance FromJSON CpuState where
  parseJSON = withObject "state" $ \o ->
    CpuState <$> o .: "pc" <*> o .: "s" <*> o .: "a" <*> o .: "x" <*> o .: "y" <*> o .: "p"
             <*> (map pair <$> o .: "ram")
   where
    pair :: (Int, Int) -> (Word16, Word8)
    pair (a, v) = (fromIntegral a, fromIntegral v)

instance FromJSON Access where
  parseJSON v = do
    (a, d, t) <- parseJSON v
    pure (Access (fromIntegral (a :: Int)) (fromIntegral (d :: Int)) ((t :: String) == "write"))

instance FromJSON Case where
  parseJSON = withObject "case" $ \o ->
    Case <$> o .: "name" <*> o .: "initial" <*> o .: "final" <*> o .: "cycles"

loadCases :: FilePath -> IO [Case]
loadCases p = do
  r <- eitherDecode' <$> BL.readFile p
  either (\e -> fail (p <> ": " <> e)) pure r

-- | The first @n@ cases only ('Nothing': all). The files hold one case per
-- line, so the rest isn't parsed.
loadCasesUpTo :: Maybe Int -> FilePath -> IO [Case]
loadCasesUpTo Nothing p = loadCases p
loadCasesUpTo (Just n) p = do
  ls <- BC.lines <$> BL.readFile p
  let cases = take n [ stripComma l | l <- ls, BC.take 1 (BC.dropWhile (== ' ') l) == BC.pack "{" ]
      stripComma l = let l' = BC.dropWhileEnd (\c -> c == ' ' || c == '\r') l
                     in if BC.takeEnd 1 l' == BC.pack "," then BC.dropEnd 1 l' else l'
      doc = BC.concat ["[", BC.intercalate "," cases, "]"]
  either (\e -> fail (p <> ": " <> e)) pure (eitherDecode' doc)

-- | The test tier (docs/TESTING.md, "Tiers"): EMU_FULL_TESTS=1 (make
-- test FULL=1) runs the full sets; otherwise the quick tier.
fullTests :: IO Bool
fullTests = (== Just "1") <$> lookupEnv "EMU_FULL_TESTS"

-- | SingleStepTests cases per opcode in the quick tier.
quickCases :: Int
quickCases = 500

-- | Differences between the model and the case; empty when it passes.
runCase :: Config -> Case -> [String]
runCase cfg tc = runST $ do
  let ini = caseInitial tc
      fin = caseFinal tc
  ram <- newSTRef (IM.fromList [(fromIntegral a, v) | (a, v) <- csRam ini])
  trace <- newSTRef []
  errs <- newSTRef []
  let listed = IM.fromList [(fromIntegral a, ()) | (a, _) <- csRam ini <> csRam fin]
      bad e = modifySTRef' errs (e :)
      bus = Bus
        { busRead = \a -> do
            m <- readSTRef ram
            v <- case IM.lookup (fromIntegral a) m of
              Just v -> pure v
              Nothing -> bad ("read of unlisted address " <> show a) >> pure 0
            modifySTRef' trace (Access a v False :)
            pure v
        , busWrite = \a v -> do
            m <- readSTRef ram
            when (IM.notMember (fromIntegral a) listed) (bad ("write to unlisted address " <> show a))
            writeSTRef ram (IM.insert (fromIntegral a) v m)
            modifySTRef' trace (Access a v True :)
        , busRdy = pure True
        , busNmi = pure False
        , busIrq = pure False
        }
  cpu <- newCpu cfg bus (Regs (csA ini) (csX ini) (csY ini) (csS ini) (csP ini) (csPC ini))
  let want = length (caseCycles tc)
      go = do
        _ <- step cpu
        n <- cpuCycles cpu
        when (n < want) go
  go
  r <- getRegs cpu
  m <- readSTRef ram
  got <- reverse <$> readSTRef trace
  let regs = [ (n, g, w) | (n, g, w) <-
                 [ ("PC", fromIntegral (rPC r), fromIntegral (csPC fin) :: Int)
                 , ("S", fromIntegral (rS r), fromIntegral (csS fin))
                 , ("A", fromIntegral (rA r), fromIntegral (csA fin))
                 , ("X", fromIntegral (rX r), fromIntegral (csX fin))
                 , ("Y", fromIntegral (rY r), fromIntegral (csY fin))
                 , ("P", fromIntegral (rP r), fromIntegral (csP fin)) ]
             , g /= w ]
  forM_ regs $ \(n, g, w) -> bad (n <> " = " <> show g <> ", expected " <> show w)
  forM_ (csRam fin) $ \(a, v) ->
    let g = IM.findWithDefault 0 (fromIntegral a) m
    in when (g /= v) (bad ("RAM " <> show a <> " = " <> show g <> ", expected " <> show v))
  when (got /= caseCycles tc) $
    bad ("cycles: got " <> show got <> ", expected " <> show (caseCycles tc))
  reverse <$> readSTRef errs
