{-|
AccuracyCoin (100thCoin, MIT; docs/systems/nes.md, "AccuracyCoin") as a
running score (PLAN.md, from step 3.3).

The ROM lists its tests in PRG at $8100 ("TableTable"): one word per
page pointing at the page, then the pages themselves. A page is its name
(ended by $FF), then entries of a name (ended by $FF), the address of the
result byte and the address of the test, and a closing $FF. The number of
pages follows from where the first page starts.

Pressing Start on the first menu entry runs every test ("RunningAllTests"
at $35 is 1 meanwhile) and then draws a results table. Result bytes live
at $0400-$04FF: $FF = skipped, bit 0 set = passed (bits 2 and up: which
of several accepted behaviours), bit 0 clear = failed (bits 2 and up: the
error code). Entries whose result lives at $03xx only draw power-on state
and aren't counted. Names may hold the ROM's word codes (bytes $80 to
$FE, since commit d103cbd), expanded here.
-}
module Nes.AccuracyCoin
  ( Page (..)
  , Entry (..)
  , parsePages
  , Outcome (..)
  , outcome
  , counted
  , runningAllTests
  , resultLines
  ) where

import Prelude

import Data.Bits (shiftL, shiftR, testBit, (.&.), (.|.))
import qualified Data.ByteString as BS
import Data.Word

data Page = Page
  { pgName    :: String
  , pgEntries :: [Entry]
  } deriving stock (Show, Eq)

data Entry = Entry
  { enName   :: String
  , enResult :: Word16 -- ^ where the result byte is stored
  } deriving stock (Show, Eq)

-- | Zero-page flag: 1 while the ROM runs every test.
runningAllTests :: Word16
runningAllTests = 0x35

-- | The test pages from the PRG image (32 KiB at $8000).
parsePages :: BS.ByteString -> Either String [Page]
parsePages prg = do
  first <- word 0x8100
  let n = (fromIntegral first - 0x8100) `div` 2
  ptrs <- mapM (\i -> word (0x8100 + 2 * i)) [0 .. n - 1]
  mapM page ptrs
 where
  at :: Int -> Either String Word8
  at a | a - 0x8000 < BS.length prg && a >= 0x8000 = Right (BS.index prg (a - 0x8000))
       | otherwise = Left ("AccuracyCoin table runs out of PRG at " <> show a)
  word a = (\lo hi -> fromIntegral hi `shiftL` 8 .|. fromIntegral lo :: Word16) <$> at a <*> at (a + 1)
  str a = do
    b <- at a
    if b == 0xFF then pure ("", a + 1) else (\(s, e) -> (expand b <> s, e)) <$> str (a + 1)
  -- Since d103cbd, $80 to $FE stand for common words ("PrintTextSpecialStrings")
  expand b
    | b >= 0x80 = ([" indirect", " zeropage", " absolute", " immediate", "Unofficial Instructions: "] <> repeat "?") !! fromIntegral (b .&. 7)
    | otherwise = [toEnum (fromIntegral b)]
  page p = do
    (name, a) <- str (fromIntegral p)
    es <- entries a
    pure (Page name es)
  entries a = do
    b <- at a
    if b == 0xFF
      then pure []
      else do
        (name, a') <- str a
        res <- word a'
        (Entry name res :) <$> entries (a' + 4)

data Outcome = Passed !Int | Failed !Int | Skipped
  deriving stock (Show, Eq)

-- | A result byte: the number is the behaviour (passed) or the error code
-- (failed).
outcome :: Word8 -> Outcome
outcome b
  | b == 0xFF = Skipped
  | testBit b 0 = Passed (fromIntegral (b `shiftR` 2))
  | otherwise = Failed (fromIntegral (b `shiftR` 2))

-- | Entries that count towards the score (not the $03xx draw-only ones).
counted :: Entry -> Bool
counted e = enResult e .&. 0xFF00 /= 0x0300

-- | The results, one line per test, and the score line (the format of
-- @test/golden/accuracycoin-spec.txt@).
resultLines :: [(String, [(Entry, Word8)])] -> [String]
resultLines pages =
  concat [ ("== " <> show i <> " " <> name) : [ "   " <> showOutcome e b <> "  " <> enName e | (e, b) <- es ]
         | (i, (name, es)) <- zip [1 :: Int ..] pages ]
  <> ["score " <> show (length [ () | b <- cs, isPass (outcome b) ]) <> " / " <> show (length cs)]
 where
  cs = [ b | (_, es) <- pages, (e, b) <- es, counted e ]
  isPass (Passed _) = True
  isPass _ = False
  showOutcome e b
    | not (counted e) = "draw     "
    | otherwise = case outcome b of
        Passed k -> "pass" <> (if k == 0 then "     " else " (" <> show k <> ")")
        Failed k -> "FAIL " <> show k
        Skipped -> "skipped  "
