-- Side track Q (PLAN.md Q.6): cpus/w65c816's test/W65c816/SingleStep.hs
-- with "W65c816.Spec" replaced by the SNES's fast copy
-- ("Snes.SpecFast.Cpu"), so the snes-specfast-cpu suite runs the same
-- SingleStepTests on it. Keep it in step with the original.
{-# LANGUAGE OverloadedStrings #-}
{-|
Tom Harte's SingleStepTests for the 65816 (github.com/SingleStepTests/65816,
MIT; fetched by tools/fetch-roms.sh into roms/65816/v1): one JSON file per
opcode and mode ("a9.e.json" emulation, "a9.n.json" native), 10 000 cases
each. A case gives the registers and the RAM it touches before and after
one instruction in a flat 24-bit address space, and the bus of every
cycle: address ('Nothing' while WAI or STP has stopped the CPU), data
('Nothing' on a read with neither VDA nor VPA nor VPB, which the tests'
machine doesn't answer) and the pins, in the order
VDA, VPA, VPB, RWB, E, M, X, MLB (the repository's README).

'runCase' runs a case on "W65c816.Spec" and returns the differences
(empty = pass): the registers, the RAM, and every cycle's address, data,
read or write, VDA, VPA, VPB and MLB (not E, M and X: the 5A22 doesn't
bring them out). WAI and STP are followed by one stopped cycle, and MVN
and MVP run to the end of the move, as the tests record them.
-}
module SpecFastCpu.SingleStep
  ( Case (..)
  , CpuState (..)
  , Cycle (..)
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

import Snes.SpecFast.Cpu
import W65c816.Types

data CpuState = CpuState
  { csRegs :: Regs
  , csRam  :: [(Word32, Word8)]
  } deriving stock (Show)

-- | One bus cycle (see the module header).
data Cycle = Cycle
  { cyAddr  :: !(Maybe Word32)
  , cyData  :: !(Maybe Word8)
  , cyVda   :: !Bool
  , cyVpa   :: !Bool
  , cyVpb   :: !Bool
  , cyWrite :: !Bool
  , cyE     :: !Bool
  , cyM     :: !Bool
  , cyX     :: !Bool
  , cyMlb   :: !Bool
  } deriving stock (Show, Eq)

data Case = Case
  { caseName    :: String
  , caseInitial :: CpuState
  , caseFinal   :: CpuState
  , caseCycles  :: [Cycle]
  } deriving stock (Show)

instance FromJSON CpuState where
  parseJSON = withObject "state" $ \o -> do
    let w8 k = (fromIntegral :: Int -> Word8) <$> o .: k
        w16 k = (fromIntegral :: Int -> Word16) <$> o .: k
    rs <- Regs <$> w16 "a" <*> w16 "x" <*> w16 "y" <*> w16 "s" <*> w16 "d"
               <*> w8 "dbr" <*> w8 "pbr" <*> w16 "pc" <*> w8 "p"
               <*> ((/= (0 :: Int)) <$> o .: "e")
    ram <- map pair <$> o .: "ram"
    pure (CpuState rs ram)
   where
    pair :: (Int, Int) -> (Word32, Word8)
    pair (a, v) = (fromIntegral a, fromIntegral v)

instance FromJSON Cycle where
  parseJSON v = do
    (a, d, pins) <- parseJSON v
    case (pins :: String) of
      [vda, vpa, vpb, rw, e, m, x, mlb] ->
        pure Cycle
          { cyAddr = fromIntegral <$> (a :: Maybe Int)
          , cyData = fromIntegral <$> (d :: Maybe Int)
          , cyVda = vda == 'd', cyVpa = vpa == 'p', cyVpb = vpb == 'v'
          , cyWrite = rw == 'w', cyE = e == 'e', cyM = m == 'm', cyX = x == 'x'
          , cyMlb = mlb == 'l'
          }
      _ -> fail ("bad pins " <> show pins)

instance FromJSON Case where
  parseJSON = withObject "case" $ \o ->
    Case <$> o .: "name" <*> o .: "initial" <*> o .: "final" <*> o .: "cycles"

loadCases :: FilePath -> IO [Case]
loadCases p = do
  r <- eitherDecode' <$> BL.readFile p
  either (\e -> fail (p <> ": " <> e)) pure r

-- | The first @n@ cases only ('Nothing': all). Each file is one JSON
-- array, so the whole file is parsed either way.
loadCasesUpTo :: Maybe Int -> FilePath -> IO [Case]
loadCasesUpTo n p = maybe id take n <$> loadCases p

-- | The test tier (docs/TESTING.md, "Tiers"): EMU_FULL_TESTS=1 (make
-- test FULL=1) runs the full sets; otherwise the quick tier.
fullTests :: IO Bool
fullTests = (== Just "1") <$> lookupEnv "EMU_FULL_TESTS"

-- | SingleStepTests cases per opcode file in the quick tier.
quickCases :: Int
quickCases = 100

-- | Differences between "W65c816.Spec" and the case; empty when it passes.
runCase :: Case -> [String]
runCase tc = runST $ do
  let ini = caseInitial tc
      fin = caseFinal tc
  ram <- newSTRef (IM.fromList [(fromIntegral a, v) | (a, v) <- csRam ini])
  trace <- newSTRef []
  errs <- newSTRef []
  let bad e = modifySTRef' errs (e :)
      note cy = modifySTRef' trace (cy :)
      blank = Cycle Nothing Nothing False False False False False False False False
      bus = Bus
        { busRead = \k l a -> do
            m <- readSTRef ram
            v <- case IM.lookup (fromIntegral a) m of
              Just v -> pure v
              Nothing -> bad ("read of unlisted address " <> hex a) >> pure 0
            note blank { cyAddr = Just a, cyData = Just v
                       , cyVda = k /= KOperand, cyVpa = k `elem` [KOpcode, KOperand]
                       , cyVpb = k == KVector, cyMlb = l }
            pure v
        , busWrite = \vda l a v -> do
            modifySTRef' ram (IM.insert (fromIntegral a) v)
            note blank { cyAddr = Just a, cyData = Just v, cyVda = vda, cyWrite = True, cyMlb = l }
        , busIdle = \l a -> note blank { cyAddr = Just a, cyMlb = l }
        , busStopped = note blank
        , busNmi = pure False
        , busIrq = pure False
        }
  cpu <- newCpu singleStep65816 bus (csRegs ini)
  let pc0 = rPC (csRegs ini)
      go n = do
        st <- step cpu
        r <- getRegs cpu
        run <- cpuRun cpu
        case st of
          Instr op
            | op `elem` [0x44, 0x54], rPC r == pc0, n < (70000 :: Int) -> go (n + 1)
          _ | run /= Running -> () <$ step cpu
          _ -> pure ()
  go 0
  r <- getRegs cpu
  m <- readSTRef ram
  got <- reverse <$> readSTRef trace
  errs0 <- reverse <$> readSTRef errs
  pure (errs0 <> judge tc r m got)
 where
  hex a = showHex a ""

-- | The differences between a run's outcome (its registers, RAM and
-- cycles) and the case's.
judge :: Case -> Regs -> IM.IntMap Word8 -> [Cycle] -> [String]
judge tc r m got = runST $ do
  errs <- newSTRef []
  let bad e = modifySTRef' errs (e :)
      fin = caseFinal tc
      hex a = showHex a ""
  let w = csRegs fin
      regs = [ (nm, gv, ev) | (nm, gv, ev) <-
                 [ ("A", f rA r, f rA w), ("X", f rX r, f rX w), ("Y", f rY r, f rY w)
                 , ("S", f rS r, f rS w), ("D", f rD r, f rD w)
                 , ("DBR", fromIntegral (rDBR r), fromIntegral (rDBR w))
                 , ("PBR", fromIntegral (rPBR r), fromIntegral (rPBR w))
                 , ("PC", f rPC r, f rPC w), ("P", fromIntegral (rP r), fromIntegral (rP w))
                 , ("E", fromEnum (rE r), fromEnum (rE w)) ]
             , gv /= ev ]
      f :: (Regs -> Word16) -> Regs -> Int
      f sel = fromIntegral . sel
  forM_ regs $ \(nm, gv, ev) -> bad (nm <> " = " <> showHex gv "" <> ", expected " <> showHex ev "")
  forM_ (csRam fin) $ \(a, v) -> do
    let gv = IM.findWithDefault 0 (fromIntegral a) m
    when (gv /= v) (bad ("RAM " <> hex a <> " = " <> showHex gv "" <> ", expected " <> showHex v ""))
  let want = caseCycles tc
      same x y = cyAddr x == cyAddr y && cyWrite x == cyWrite y && cyVda x == cyVda y
                 && cyVpa x == cyVpa y && cyVpb x == cyVpb y && cyMlb x == cyMlb y
                 && (cyData y == Nothing || cyData x == cyData y)
  when (length got /= length want || or (zipWith (\x y -> not (same x y)) got want)) $
    bad ("cycles:\n" <> unlines (zipWithLong showC got want))
  reverse <$> readSTRef errs

-- | The two traces side by side (got, expected).
zipWithLong :: (Maybe Cycle -> Maybe Cycle -> String) -> [Cycle] -> [Cycle] -> [String]
zipWithLong f (x : xs) (y : ys) = f (Just x) (Just y) : zipWithLong f xs ys
zipWithLong f (x : xs) [] = f (Just x) Nothing : zipWithLong f xs []
zipWithLong f [] (y : ys) = f Nothing (Just y) : zipWithLong f [] ys
zipWithLong _ [] [] = []

showC :: Maybe Cycle -> Maybe Cycle -> String
showC a b = "  " <> pad (maybe "-" one a) <> " | " <> maybe "-" one b
 where
  pad t = t <> replicate (30 - length t) ' '
  one cy = maybe "------" (\x -> showHex x "") (cyAddr cy) <> " "
           <> maybe "--" (\x -> showHex x "") (cyData cy) <> " "
           <> [ if cyVda cy then 'd' else '-', if cyVpa cy then 'p' else '-'
              , if cyVpb cy then 'v' else '-', if cyWrite cy then 'w' else 'r'
              , if cyMlb cy then 'l' else '-' ]
