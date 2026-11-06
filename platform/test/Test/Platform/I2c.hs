module Test.Platform.I2c (tests) where

import Prelude

import qualified Clash.Explicit.Prelude as C
import qualified Clash.Prelude as H
import qualified Data.Map.Strict as M
import Data.Word (Word8)
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog

import Platform.Bus.I2c
import Platform.Domains
import Platform.Sim.I2c

-- | Short phases so a simulation stays small (3 is the minimum).
fast :: I2cTiming
fast = I2cTiming { itQuarter = 3, itStretchMax = 200 }

dev :: Word8
dev = 0x39   -- the ADV7511's main map (0x72 as an 8-bit write address)

w :: Word8 -> Word8 -> Word8 -> I2cTxn
w a r v = TxnWrite (fromIntegral a) (fromIntegral r) (fromIntegral v)

r' :: Word8 -> Word8 -> I2cTxn
r' a r = TxnRead (fromIntegral a) (fromIntegral r)

run :: SlaveConfig -> [I2cTxn] -> Run
run cfg = runTxns fast cfg 100000

-- | Lengths of the maximal runs of equal values.
runLengths :: Eq a => [a] -> [(a, Int)]
runLengths [] = []
runLengths (x : xs) = let (same, rest) = span (== x) xs in (x, 1 + length same) : runLengths rest

tests :: TestTree
tests = testGroup "I2c"
  [ testCase "standard-mode timing: 63 clocks per quarter at the pixel clock, 250 at 100 MHz" $ do
      itQuarter (i2cTiming @DomPix) @?= 63
      itQuarter (i2cTiming @Dom100) @?= 250

  , testCase "write three registers, read them back" $ do
      let txns = [w dev 0x41 0x10, w dev 0x98 0x03, w dev 0x16 0x30, r' dev 0x41, r' dev 0x98, r' dev 0x16]
          o = run (slaveConfig dev) txns
      runResults o @?= map Right [0, 0, 0, 0x10, 0x03, 0x30]
      M.toList (slRegs (runSlave o)) @?= [(0x16, 0x30), (0x41, 0x10), (0x98, 0x03)]
      reverse (slLog (runSlave o)) @?=
        concat [ [EvStart, EvWrite 0x41 0x10, EvStop], [EvStart, EvWrite 0x98 0x03, EvStop]
               , [EvStart, EvWrite 0x16 0x30, EvStop]
               , [EvStart, EvStart, EvRead 0x41 0x10, EvStop], [EvStart, EvStart, EvRead 0x98 0x03, EvStop]
               , [EvStart, EvStart, EvRead 0x16 0x30, EvStop] ]
      slErrors (runSlave o) @?= []

  , testCase "reads see the slave's initial registers (chip revision)" $ do
      let cfg = (slaveConfig dev) { scRegs = M.fromList [(0x00, 0x14), (0xF5, 0x75)] }
      runResults (run cfg [r' dev 0x00, r' dev 0xF5, r' dev 0x01]) @?= map Right [0x14, 0x75, 0]

  , testCase "wrong address: NackAddr, then the next transaction works" $ do
      let o = run (slaveConfig dev) [w 0x38 0x41 0x10, r' 0x3A 0x00, w dev 0x41 0x10, r' dev 0x41]
      runResults o @?= [Left NackAddr, Left NackAddr, Right 0, Right 0x10]
      M.toList (slRegs (runSlave o)) @?= [(0x41, 0x10)]

  , testCase "refused register byte: NackReg" $ do
      let cfg = (slaveConfig dev) { scBadPtr = [0x77] }
      runResults (run cfg [w dev 0x77 1, r' dev 0x77, w dev 0x41 2])
        @?= [Left NackReg, Left NackReg, Right 0]

  , testCase "refused value byte: NackData, register unchanged, STOP sent" $ do
      let cfg = (slaveConfig dev) { scNackReg = [0x41], scRegs = M.fromList [(0x41, 0x55)] }
          o = run cfg [w dev 0x41 0x10, r' dev 0x41]
      runResults o @?= [Left NackData, Right 0x55]
      take 3 (reverse (slLog (runSlave o))) @?= [EvStart, EvNack 0x41, EvStop]

  , testCase "clock stretching: same results, more clocks" $ do
      let txns = [w dev 0x41 0x10, r' dev 0x41]
          plain = run (slaveConfig dev) txns
          slow  = run ((slaveConfig dev) { scStretch = 40 }) txns
      runResults slow @?= [Right 0, Right 0x10]
      assertBool "stretching costs clocks" (runClocks slow > runClocks plain + 40 * 9)

  , testCase "SCL held low: I2cTimeout, lines released, ready again" $ do
      let o = run ((slaveConfig dev) { scStuck = True }) [w dev 0x41 0x10]
      runResults o @?= [Left I2cTimeout]
      assertBool "finished well before the clock cap" (runClocks o < 2000)
      -- the slave still holds SCL; the master has let go of SDA
      ilSda (last (runBus o)) @?= 1

  , testCase "real timing (DomPix): SCL low and high at least 5 us each, at most 100 kHz" $ do
      let o = runTxns (i2cTiming @DomPix) (slaveConfig dev) 200000 [w dev 0x41 0x10, r' dev 0x41]
          sclRuns = map snd (drop 1 (init (runLengths (map ilScl (runBus o)))))
          -- 5 us at the pixel clock (39720 ps)
          minRun = ceiling (5e6 / 39720 :: Double)
      runResults o @?= [Right 0, Right 0x10]
      assertBool ("shortest SCL run " <> show (minimum sclRuns)) (minimum sclRuns >= minRun)

  , testCase "bus waveform: SDA moves only while SCL is low (START/STOP aside), 2 quarters low and high" $ do
      let o = run (slaveConfig dev) [w dev 0x41 0x10, r' dev 0x41]
          bus = runBus o
          pairs = zip bus (drop 1 bus)
          sdaWhileHigh = [ () | (a, b) <- pairs, ilScl a == 1, ilScl b == 1, ilSda a /= ilSda b ]
          -- every SDA change with SCL high is a START or STOP in the log
          condCount = length [ e | e <- slLog (runSlave o), e `elem` [EvStart, EvStop] ]
          sclRuns = map snd (drop 1 (init (runLengths (map ilScl bus))))
      length sdaWhileHigh @?= condCount
      assertBool ("SCL runs: " <> show sclRuns) (all (>= 2 * 3) sclRuns)
      -- SCL never changes in the same clock as SDA
      [ () | (a, b) <- pairs, ilScl a /= ilScl b, ilSda a /= ilSda b ] @?= []

  , testProperty "random writes then reading every register back = a Map model" $ property $ do
      ws <- forAll (Gen.list (Range.linear 1 12)
              ((,) <$> Gen.word8 Range.constantBounded <*> Gen.word8 Range.constantBounded))
      let model = M.fromList ws
          txns = [ w dev a v | (a, v) <- ws ] ++ [ r' dev a | a <- M.keys model ]
          o = run (slaveConfig dev) txns
      runResults o === map (const (Right 0)) ws ++ [ Right (fromIntegral v) | v <- M.elems model ]
      slRegs (runSlave o) === model
      slErrors (runSlave o) === []

  , testCase "circuit (i2cMaster, with the input synchroniser) against the slave model" $ do
      let txns = [w dev 0x41 0x10, r' dev 0x41, w 0x10 0 0]
          outs = C.sampleN 3000 (H.withClockResetEnable @C.System C.clockGen C.resetGen C.enableGen (circuit txns))
      [ x | Just x <- map ioDone outs ] @?= [Right 0, Right 0x10, Left NackAddr]
  ]

-- | Master, slave (as a mealy machine) and a driver that offers the
-- transactions in turn, all in Clash signals.
circuit :: H.HiddenClockResetEnable dom => [I2cTxn] -> H.Signal dom I2cOut
circuit txns = out
 where
  out = i2cMaster fast bus offer
  slave = H.register (slaveInit (slaveConfig dev)) (slaveStep <$> slave <*> bus)
  bus = busLines <$> (ioSclLow <$> out) <*> (ioSdaLow <$> out) <*> slave
  -- driver: the list of transactions still to offer
  queue = H.register txns (next <$> queue <*> out)
  offer = (\q o -> if ioReady o then safeHead q else Nothing) <$> queue <*> out
  next q o = if ioReady o then drop 1 q else q
  safeHead q = case q of
    x : _ -> Just x
    []    -> Nothing
