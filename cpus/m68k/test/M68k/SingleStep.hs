{-# LANGUAGE OverloadedStrings #-}
{-|
Tom Harte's 680x0 SingleStepTests for the 68000 (github.com/
SingleStepTests/680x0, MIT; tools/fetch-roms.sh; docs/cpus/m68k.md): one
gzipped JSON file per instruction and size, 8065 cases each. A case gives
the registers, SR, the prefetch queue and the RAM it touches before and
after one instruction, its cycles, and every bus transaction in order.

'runCase' runs "M68k.Spec" on the case's RAM, recording each access, and
compares: the transactions (kind, cycles, function code, address, size,
data; idle stretches merged), the total cycles, the registers, the queue
and the RAM. The differences, empty when it passes.
-}
module M68k.SingleStep
  ( Case (..)
  , Txn (..)
  , loadCases
  , fullTests
  , quickCases
  , runCase
  , Runner
  , runCaseWith
  ) where

import Prelude

import Codec.Compression.GZip (decompress)
import Control.Monad.Trans.State.Strict (State, gets, modify', runState)
import Data.Aeson
import qualified Data.Aeson.Key as K
import Data.Aeson.Types (Parser)
import Data.Array.Unboxed (listArray)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import Data.Bits
import qualified Data.Map.Strict as M
import Data.Word
import System.Environment (lookupEnv)

import M68k.Spec

data Regs = Regs
  { rCpu :: Cpu
  , rRam :: [(Word32, Word8)]
  }

-- | A bus transaction as the tests write it: read, write, TAS (with
-- cycles, function code, address, byte or word, data) or idle cycles.
data Txn
  = Bus !Char !Int !Fc !Word32 !Size !Word16
  | Idle !Int
  deriving stock (Show, Eq)

data Case = Case
  { caseName    :: String
  , caseInitial :: Regs
  , caseFinal   :: Regs
  , caseLength  :: Int
  , caseTxns    :: [Txn]
  }

instance FromJSON Regs where
  parseJSON = withObject "state" $ \o -> do
    let w32 k = (fromIntegral :: Integer -> Word32) <$> o .: k
    ds <- mapM (\i -> w32 (key 'd' i)) [0 .. 7 :: Int]
    as <- mapM (\i -> w32 (key 'a' i)) [0 .. 6 :: Int]
    usp <- w32 "usp"
    ssp <- w32 "ssp"
    sr <- (fromIntegral :: Int -> Word16) <$> o .: "sr"
    pc <- w32 "pc"
    pf <- o .: "prefetch"
    (ir, irc) <- case pf of
      [a, b] -> pure (fromIntegral (a :: Int), fromIntegral (b :: Int))
      _ -> fail "prefetch: not two words"
    ram <- map (\(a, v) -> (fromIntegral (a :: Integer), fromIntegral (v :: Int))) <$> o .: "ram"
    pure Regs
      { rCpu = Cpu { cD = listArray (0, 7) ds, cA = listArray (0, 6) as, cUsp = usp, cSsp = ssp
                   , cSr = sr, cPc = pc, cIr = ir, cIrc = irc, cStopped = False, cIplSeen = 0 }
      , rRam = ram }
   where
    key c i = K.fromString (c : show i)

instance FromJSON Txn where
  parseJSON v = do
    xs <- parseJSON v :: Parser [Value]
    case xs of
      [String "n", n] -> Idle <$> parseJSON n
      [String k, n, fc, a, String sz, d] | k `elem` ["r", "w", "t"] ->
        Bus (T.head k) <$> parseJSON n <*> (fromIntegral <$> (parseJSON fc :: Parser Int))
            <*> (fromIntegral <$> (parseJSON a :: Parser Integer))
            <*> pure (if sz == ".b" then Byte else Word)
            <*> (fromIntegral <$> (parseJSON d :: Parser Int))
      _ -> fail ("transaction: " <> show xs)

instance FromJSON Case where
  parseJSON = withObject "case" $ \o ->
    Case <$> o .: "name" <*> o .: "initial" <*> o .: "final" <*> o .: "length" <*> o .: "transactions"

-- | The first @n@ cases of a file (Nothing: all). The files hold one case
-- per line ("[{...},", "{...},", ..., "]"), so the quick tier decompresses
-- and decodes only the lines it takes (was the whole file: 89 s and 3.9
-- GB for the suite).
loadCases :: Maybe Int -> FilePath -> IO [Case]
loadCases n p = do
  raw <- decompress <$> BL.readFile p
  let ls = maybe id take n (filter (not . BL.null) (map clean (BLC.lines raw)))
  either (\e -> fail (p <> ": " <> e)) pure (mapM eitherDecode' ls)
 where
  clean = BLC.dropWhileEnd (`elem` (",] \r" :: String)) . BLC.dropWhile (`elem` ("[ " :: String))

-- | The test tier (docs/TESTING.md): EMU_FULL_TESTS=1 runs every case.
fullTests :: IO Bool
fullTests = (== Just "1") <$> lookupEnv "EMU_FULL_TESTS"

-- | Cases per file in the quick tier.
quickCases :: Int
quickCases = 200

-- | The bus over the case's RAM, recording each access.
type Sim = State (M.Map Word32 Word8, [Txn])

simBus :: BusOps Sim
simBus = BusOps
  { busRead = \fc sz a -> do
      v <- case sz of
        Byte -> fromIntegral <$> byte a
        Word -> (\h l -> fromIntegral h `shiftL` 8 .|. fromIntegral l) <$> byte a <*> byte (a + 1)
      record (Bus 'r' 4 fc a sz v)
      pure v
  , busWrite = \fc sz a v -> do
      case sz of
        Byte -> poke a (fromIntegral v)
        Word -> poke a (fromIntegral (v `shiftR` 8)) >> poke (a + 1) (fromIntegral v)
      record (Bus 'w' 4 fc a sz v)
  , busTas = \fc a f -> do
      v <- byte a
      poke a (f v)
      record (Bus 't' 10 fc a Byte (fromIntegral (f v))) -- the byte written, as the tests record
      pure v
  , busIdle = \n -> record (Idle n)
  , busIpl = pure 0 -- the tests have no interrupts
  , busIack = \_ -> pure Nothing
  , busBerr = pure False
  }
 where
  byte a = gets (M.findWithDefault 0 a . fst)
  poke a v = modify' (\(m, t) -> (M.insert a v m, t))
  record x = modify' (\(m, t) -> (m, x : t))

-- | Adjacent idle stretches as one (the tests' "n" entries are whole).
merge :: [Txn] -> [Txn]
merge (Idle a : Idle b : r) = merge (Idle (a + b) : r)
merge (Idle 0 : r) = merge r
merge (x : r) = x : merge r
merge [] = []

cycles :: Txn -> Int
cycles (Bus _ n _ _ _ _) = n
cycles (Idle n) = n

-- | Something that runs one instruction on a bus: the model's 'step', or
-- the Clash core through it ("M68k.CoreRun").
type Runner = forall m. Monad m => BusOps m -> Cpu -> m (Either Fault Cpu)

-- | The differences between the model and the case; empty: it passes.
runCase :: Case -> [String]
runCase = runCaseWith step

-- | The same for any runner.
runCaseWith :: Runner -> Case -> [String]
runCaseWith run tc =
  let ini = caseInitial tc
      fin = caseFinal tc
      (r, (ram, txns)) = runState (run simBus (rCpu ini)) (M.fromList (rRam ini), [])
      got = merge (reverse txns)
      want = merge (caseTxns tc)
  in case r of
       Left f -> ["stopped: " <> show f]
       Right c ->
         [ "transactions: got " <> show got <> ", want " <> show want | got /= want ]
         <> [ "cycles: got " <> show (sum (map cycles got)) <> ", want " <> show (caseLength tc)
            | sum (map cycles got) /= caseLength tc ]
         <> regDiffs c (rCpu fin)
         <> [ "ram " <> show a <> ": got " <> show g <> ", want " <> show v
            | (a, v) <- rRam fin, let g = M.findWithDefault 0 a ram, g /= v ]

regDiffs :: Cpu -> Cpu -> [String]
regDiffs g w =
  [ name <> ": got " <> hex (f g) <> ", want " <> hex (f w)
  | (name, f) <- [ ("d" <> show i, \c -> dReg c i) | i <- [0 .. 7] ]
                 <> [ ("a" <> show i, \c -> cA' c i) | i <- [0 .. 6] ]
                 <> [ ("usp", cUsp), ("ssp", cSsp), ("sr", fromIntegral . cSr), ("pc", cPc)
                    , ("ir", fromIntegral . cIr), ("irc", fromIntegral . cIrc) ]
  , f g /= f w ]
 where
  cA' c i = aReg c i
  hex v = "0x" <> showHex' v
  showHex' v = let s = go v in if null s then "0" else s
   where go 0 = ""
         go x = go (x `div` 16) <> [("0123456789abcdef" !! fromIntegral (x `mod` 16))]
