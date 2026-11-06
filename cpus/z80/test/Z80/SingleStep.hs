{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-|
Tom Harte's SingleStepTests for the Z80 (github.com/SingleStepTests/z80,
MIT; fetched by tools/fetch-roms.sh): one JSON file per opcode, 1000 cases
each. A case gives the registers (MEMPTR, Q, the EI and LD A,I/R flags
included) and the RAM it touches before and after one instruction, the
I/O port values, and the pins of every T-state.

The model works in machine cycles, so 'accesses' collapses a case's
T-states into its memory and I/O accesses: a T-state whose pins show RD
or WR starts one (MREQ: memory, IORQ: I/O); a write carries its data on
that T-state, a read on the next one. An opcode fetch is a memory read
here. 'runCase' compares those accesses in order, the total T-states,
the registers and the RAM, and returns the differences (empty = pass).
-}
module Z80.SingleStep
  ( Case (..)
  , CpuState (..)
  , Access (..)
  , loadCases
  , loadCasesUpTo
  , fullTests
  , quickCases
  , runCase
  , runCaseWith
  , Runner
  , specRunner
  ) where

import Prelude

import Control.Monad (forM_, when)
import Control.Monad.ST (ST, runST)
import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.IntMap.Strict as IM
import Data.STRef
import Data.Word
import System.Environment (lookupEnv)

import Z80.Spec

data CpuState = CpuState
  { csRegs :: Regs
  , csRam  :: [(Word16, Word8)]
  } deriving stock (Show)

-- | A memory or I/O access: address, data, write, I/O.
data Access = Access !Word16 !Word8 !Bool !Bool
  deriving stock (Show, Eq)

-- | One T-state's pins: address, data, "rwmi".
data TState = TState !(Maybe Int) !(Maybe Int) !String

data Case = Case
  { caseName    :: String
  , caseInitial :: CpuState
  , caseFinal   :: CpuState
  , caseTStates :: Int
  , caseAccess  :: [Access]
  , casePorts   :: [(Word16, Word8, Bool)] -- ^ address, value, write
  } deriving stock (Show)

instance FromJSON CpuState where
  parseJSON = withObject "state" $ \o -> do
    let w8 k = (fromIntegral :: Int -> Word8) <$> o .: k
        w16 k = (fromIntegral :: Int -> Word16) <$> o .: k
        b k = (/= (0 :: Int)) <$> o .: k
    rs <- Regs <$> w8 "a" <*> w8 "f" <*> w8 "b" <*> w8 "c" <*> w8 "d" <*> w8 "e" <*> w8 "h" <*> w8 "l"
               <*> w16 "af_" <*> w16 "bc_" <*> w16 "de_" <*> w16 "hl_"
               <*> w16 "ix" <*> w16 "iy" <*> w16 "sp" <*> w16 "pc"
               <*> w8 "i" <*> w8 "r" <*> b "iff1" <*> b "iff2" <*> w8 "im"
               <*> w16 "wz" <*> w8 "q" <*> b "ei" <*> b "p" <*> pure False
    ram <- map pair <$> o .: "ram"
    pure (CpuState rs ram)
   where
    pair :: (Int, Int) -> (Word16, Word8)
    pair (a, v) = (fromIntegral a, fromIntegral v)

instance FromJSON TState where
  parseJSON v = do
    (a, d, p) <- parseJSON v
    pure (TState a d p)

instance FromJSON Case where
  parseJSON = withObject "case" $ \o -> do
    ts <- o .: "cycles"
    ports <- o .:? "ports" .!= []
    Case <$> o .: "name" <*> o .: "initial" <*> o .: "final"
         <*> pure (length ts) <*> pure (accesses ts)
         <*> pure [ (fromIntegral (a :: Int), fromIntegral (d :: Int), (t :: String) == "w")
                  | (a, d, t) <- ports ]

-- | The accesses in a T-state list (see the module header).
accesses :: [TState] -> [Access]
accesses ts = go ts
 where
  go (TState a d p : rest) = case p of
    (r : w : m : _)
      | w == 'w' -> Access (addr a) (dat d) True (m /= 'm') : go rest
      | r == 'r' -> case rest of
          TState _ d' _ : _ -> Access (addr a) (dat d') False (m /= 'm') : go rest
          [] -> Access (addr a) 0 False (m /= 'm') : []
    _ -> go rest
  go [] = []
  addr = maybe 0 fromIntegral
  dat = maybe 0 fromIntegral

loadCases :: FilePath -> IO [Case]
loadCases p = do
  r <- eitherDecode' <$> BL.readFile p
  either (\e -> fail (p <> ": " <> e)) pure r

-- | The first @n@ cases only ('Nothing': all). Each file is one line of
-- JSON, so the whole file is parsed either way.
loadCasesUpTo :: Maybe Int -> FilePath -> IO [Case]
loadCasesUpTo n p = maybe id take n <$> loadCases p

-- | The test tier (docs/TESTING.md, "Tiers"): EMU_FULL_TESTS=1 (make
-- test FULL=1) runs the full sets; otherwise the quick tier.
fullTests :: IO Bool
fullTests = (== Just "1") <$> lookupEnv "EMU_FULL_TESTS"

-- | SingleStepTests cases per opcode file in the quick tier.
quickCases :: Int
quickCases = 100

-- | Runs one instruction from the given registers on the given bus;
-- returns the registers after it and the T-states it took.
type Runner = forall s. Bus s -> Regs -> ST s (Regs, Int)

-- | "Z80.Spec" as a 'Runner'.
specRunner :: Runner
specRunner bus rs = do
  cpu <- newCpu bus rs
  _ <- step cpu
  (,) <$> getRegs cpu <*> cpuTStates cpu

-- | Differences between "Z80.Spec" and the case; empty when it passes.
runCase :: Case -> [String]
runCase = runCaseWith specRunner

-- | Differences between a CPU model and the case; empty when it passes.
runCaseWith :: Runner -> Case -> [String]
runCaseWith run tc = runST $ do
  let ini = caseInitial tc
      fin = caseFinal tc
  ram <- newSTRef (IM.fromList [(fromIntegral a, v) | (a, v) <- csRam ini])
  ports <- newSTRef [ (a, v) | (a, v, False) <- casePorts tc ]
  trace <- newSTRef []
  errs <- newSTRef []
  let listed = IM.fromList [(fromIntegral a, ()) | (a, _) <- csRam ini <> csRam fin]
      bad e = modifySTRef' errs (e :)
      memRead a = do
        m <- readSTRef ram
        v <- case IM.lookup (fromIntegral a) m of
          Just v -> pure v
          Nothing -> bad ("read of unlisted address " <> show a) >> pure 0
        modifySTRef' trace (Access a v False False :)
        pure v
      bus = Bus
        { busFetch = memRead
        , busRead = memRead
        , busWrite = \a v -> do
            when (IM.notMember (fromIntegral a) listed) (bad ("write to unlisted address " <> show a))
            modifySTRef' ram (IM.insert (fromIntegral a) v)
            modifySTRef' trace (Access a v True False :)
        , busIn = \a -> do
            ps <- readSTRef ports
            v <- case ps of
              (_, v) : rest -> writeSTRef ports rest >> pure v
              [] -> bad ("unlisted port read " <> show a) >> pure 0xFF
            modifySTRef' trace (Access a v False True :)
            pure v
        , busOut = \a v -> modifySTRef' trace (Access a v True True :)
        , busIdle = \_ _ -> pure ()
        , busIntAck = pure 0xFF
        , busInt = pure False
        , busNmi = pure False
        }
  (r, t) <- run bus (csRegs ini)
  m <- readSTRef ram
  got <- reverse <$> readSTRef trace
  let w = csRegs fin
      regs = [ (n, g, e) | (n, g, e) <-
                 [ ("A", f8 rA r, f8 rA w), ("F", f8 rF r, f8 rF w)
                 , ("B", f8 rB r, f8 rB w), ("C", f8 rC r, f8 rC w)
                 , ("D", f8 rD r, f8 rD w), ("E", f8 rE r, f8 rE w)
                 , ("H", f8 rH r, f8 rH w), ("L", f8 rL r, f8 rL w)
                 , ("AF'", f16 rAF' r, f16 rAF' w), ("BC'", f16 rBC' r, f16 rBC' w)
                 , ("DE'", f16 rDE' r, f16 rDE' w), ("HL'", f16 rHL' r, f16 rHL' w)
                 , ("IX", f16 rIX r, f16 rIX w), ("IY", f16 rIY r, f16 rIY w)
                 , ("SP", f16 rSP r, f16 rSP w), ("PC", f16 rPC r, f16 rPC w)
                 , ("I", f8 rI r, f8 rI w), ("R", f8 rR r, f8 rR w)
                 , ("IFF1", fb rIFF1 r, fb rIFF1 w), ("IFF2", fb rIFF2 r, fb rIFF2 w)
                 , ("IM", f8 rIM r, f8 rIM w), ("WZ", f16 rWZ r, f16 rWZ w)
                 , ("Q", f8 rQ r, f8 rQ w), ("EI", fb rEI r, fb rEI w), ("P", fb rP r, fb rP w) ]
             , g /= e ]
  forM_ regs $ \(n, g, e) -> bad (n <> " = " <> show g <> ", expected " <> show e)
  forM_ (csRam fin) $ \(a, v) ->
    let g = IM.findWithDefault 0 (fromIntegral a) m
    in when (g /= v) (bad ("RAM " <> show a <> " = " <> show g <> ", expected " <> show v))
  when (t /= caseTStates tc) $
    bad ("T-states: " <> show t <> ", expected " <> show (caseTStates tc))
  when (got /= caseAccess tc) $
    bad ("accesses: got " <> show got <> ", expected " <> show (caseAccess tc))
  reverse <$> readSTRef errs
 where
  f8 g = fromIntegral . g :: Regs -> Int
  f16 g = fromIntegral . g :: Regs -> Int
  fb g = fromEnum . g :: Regs -> Int
