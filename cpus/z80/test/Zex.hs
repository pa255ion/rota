{-|
Layer 0, FULL tier: ZEXDOC and ZEXALL (Frank D. Cringle, GPL; the CP/M
originals, fetched by tools/fetch-roms.sh into roms/z80/zex/) on
'Z80.Spec' under a minimal CP/M: the program at 0100h, @JP F000h@ at
0005h (so the program's stack goes below F000h, which it reads from
0006h) and a RET at F000h. When PC reaches 0005h the BDOS call is done
here: C = 2 prints E, C = 9 the string at DE up to @$@. A jump to 0000h
ends the run.

Each of the 67 tests prints its name and "OK" or "ERROR" with the CRCs;
ZEXDOC masks the undocumented flags, ZEXALL checks them. The quick tier
runs the first test of each (seconds); @EMU_FULL_TESTS=1@ runs both to
the end (minutes: ZEXALL is about 46 billion T-states). @ZEX_TESTS=n@
overrides the number of tests run.
-}
module Main (main) where

import Prelude

import Control.Monad (unless)
import Control.Monad.ST (RealWorld, stToIO)
import qualified Data.ByteString as BS
import Data.Char (chr)
import Data.IORef
import Data.List (isInfixOf)
import qualified Data.Vector.Unboxed.Mutable as MV
import Data.Word
import System.Directory (doesFileExist)
import System.Environment (lookupEnv)
import Test.Tasty
import Test.Tasty.HUnit

import Z80.Spec

zexDir :: FilePath
zexDir = "../../roms/z80/zex"

main :: IO ()
main = do
  full <- (== Just "1") <$> lookupEnv "EMU_FULL_TESTS"
  limit <- fmap read <$> lookupEnv "ZEX_TESTS"
  let n = case limit of
        Just k -> Just k
        Nothing -> if full then Nothing else Just 1
      tier = maybe "all tests" (\k -> "first " <> show k <> " test(s)") n
  defaultMain $ testGroup ("z80-zex (" <> tier <> ")")
    [ zexTest n "zexdoc.com"
    , zexTest n "zexall.com"
    ]

zexTest :: Maybe Int -> FilePath -> TestTree
zexTest n file = testCase file $ do
  let path = zexDir <> "/" <> file
  ok <- doesFileExist path
  unless ok (assertFailure (path <> " missing: run tools/fetch-roms.sh"))
  prog <- BS.readFile path
  (out, results) <- runZex n prog
  let errors = filter ("ERROR" `isInfixOf`) (lines out)
  assertBool ("no test results:\n" <> out) (results > 0)
  assertBool ("failed:\n" <> unlines errors) (null errors)
  -- a whole run must reach the end (the 67th result, then this line)
  unless (n /= Nothing) $
    assertBool ("did not finish:\n" <> out) ("Tests complete" `isInfixOf` out)

-- | Run a CP/M program until it exits or @n@ test results ("OK" or
-- "ERROR" lines) have been printed; returns the output and that count.
runZex :: Maybe Int -> BS.ByteString -> IO (String, Int)
runZex n prog = do
  mem <- MV.replicate 0x10000 0 :: IO (MV.MVector RealWorld Word8)
  mapM_ (\(i, b) -> MV.write mem (0x100 + i) b) (zip [0 ..] (BS.unpack prog))
  mapM_ (uncurry (MV.write mem)) [(5, 0xC3), (6, 0x00), (7, 0xF0), (0xF000, 0xC9)]
  out <- newIORef ""
  results <- newIORef (0 :: Int)
  line <- newIORef ""
  let rdM a = MV.read mem (fromIntegral a)
      bus = Bus
        { busFetch = rdM
        , busRead = rdM
        , busWrite = \a v -> MV.write mem (fromIntegral a) v
        , busIn = \_ -> pure 0xFF
        , busOut = \_ _ -> pure ()
        , busIdle = \_ _ -> pure ()
        , busIntAck = pure 0xFF
        , busInt = pure False
        , busNmi = pure False
        }
      emit ch = do
        modifyIORef' out (ch :)
        if ch == '\n'
          then do
            l <- readIORef line
            writeIORef line ""
            unless (not ("OK" `isInfixOf` l || "ERROR" `isInfixOf` l)) (modifyIORef' results (+ 1))
          else modifyIORef' line (++ [ch])
  cpu <- stToIO (newCpu bus zeroRegs { rPC = 0x100, rSP = 0xF000 })
  let done = do
        k <- readIORef results
        pure (maybe False (k >=) n)
      bdos = do
        r <- stToIO (getRegs cpu)
        case rC r of
          2 -> emit (chr (fromIntegral (rE r)))
          9 -> let go a = do
                     b <- MV.read mem (fromIntegral a)
                     unless (b == 0x24) (emit (chr (fromIntegral b)) >> go (a + 1 :: Word16))
               in go (fromIntegral (rD r) * 256 + fromIntegral (rE r))
          _ -> pure ()
      loop = do
        _ <- stToIO (step cpu)
        pc <- stToIO (cpuPC cpu)
        if pc == 0
          then pure ()
          else do
            if pc == 5 then bdos else pure ()
            stop <- if pc == 5 then done else pure False
            unless stop loop
  loop
  (,) <$> (reverse <$> readIORef out) <*> readIORef results
