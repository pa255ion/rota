{-# LANGUAGE BangPatterns #-}
module Test.Platform.Adv7511 (tests) where

import Prelude

import qualified Clash.Prelude as H
import Data.Bits (complement, (.&.), (.|.))
import qualified Data.Map.Strict as M
import Data.Word (Word8)
import Test.Tasty
import Test.Tasty.HUnit

import Platform.Bus.I2c
import Platform.Video.VgaTiming (Rgb444 (..), Rgb888 (..), fromRgb444)
import Platform.Video.Ycbcr422
import Platform.Domains
import Platform.Sim.I2c
import qualified Platform.Sim.Adv7511 as Chip
import Platform.Sim.Adv7511 (cscRgb)
import Platform.Video.Adv7511
import Platform.Video.Adv7511.Table

-- | Short waits and fast I2C, so a whole configuration is ~30k clocks.
fast :: AdvTiming
fast = AdvTiming { atBoot = 10, atPoll = 500, atRetry = 800
                 , atI2c = I2cTiming { itQuarter = 3, itStretchMax = 200 } }

addr :: Word8
addr = fromIntegral adv7511Addr

-- | Power-on values of the registers the table updates with a partial
-- mask, plus the revision and the power register (from the Linux driver's
-- adv7511_register_defaults, except the revision, an arbitrary test value).
defaults :: M.Map Word8 Word8
defaults = M.fromList $
  [ (0x00, 0x14), (0x41, 0x50), (0x15, 0x00), (0x16, 0x00), (0x17, 0x00), (0xAF, 0x14), (0xFB, 0x00)
  , (0x0A, 0x01), (0x0B, 0x0E), (0x44, 0x79), (0x4A, 0x80) ]
  ++ zip [0x18 ..] [ 0x46, 0x62, 0x04, 0xa8, 0x00, 0x00, 0x1c, 0x84
                   , 0x1c, 0xbf, 0x04, 0xa8, 0x1e, 0x70, 0x02, 0x1e
                   , 0x00, 0x00, 0x04, 0xa8, 0x08, 0x12, 0x1b, 0xac ]

-- | The register image the table should leave behind.
expected :: M.Map Word8 Word8
expected = expectedFor adv7511Table

expectedFor :: [TableEntry] -> M.Map Word8 Word8
expectedFor = foldl apply defaults
 where
  apply m e =
    let r = w8 (teReg e); k = w8 (teMask e); v = w8 (teValue e)
        old = M.findWithDefault 0 r m
    in M.insert r ((old .&. complement k) .|. (v .&. k)) m
  w8 :: H.BitVector 8 -> Word8
  w8 = fromIntegral

-- | The chip as seen by the sequencer: while HPD is low every register is
-- back at its default, and the status register shows HPD and sense.
chip :: Bool -> Slave -> Slave
chip hpd s = s { slRegs = M.insert 0x42 (if hpd then 0x60 else 0x00) regs }
 where regs = if hpd then slRegs s else defaults

slave0 :: Slave
slave0 = slaveInit ((slaveConfig addr) { scRegs = defaults })

-- | Run the sequencer and the slave; @env n@ changes the slave before
-- clock @n@ (HPD, a device appearing). Returns every status change with
-- its clock, and the slave at the end.
runAdv :: (Int -> Slave -> Slave) -> Int -> ([(Int, AdvStatus)], Slave)
runAdv = runAdvFrom slave0

runAdvFrom :: Slave -> (Int -> Slave -> Slave) -> Int -> ([(Int, AdvStatus)], Slave)
runAdvFrom = runAdvWith advStep

runAdvWith :: (AdvTiming -> AdvState -> I2cLines -> AdvState)
           -> Slave -> (Int -> Slave -> Slave) -> Int -> ([(Int, AdvStatus)], Slave)
runAdvWith stepF start env clocks = go 0 (advInit fast) start (idle, idle) []
 where
  idle = I2cLines 1 1
  go !n !a !sl (d1, d2) acc
    | n >= clocks = (reverse acc, sl)
    | otherwise =
        let sl0 = env n sl
            (scl, sda) = advPins a
            bus = busLines scl sda sl0
            a'  = stepF fast a d2
            sl' = slaveStep sl0 bus
            acc' = if advStatus a' /= advStatus a then (n, advStatus a') : acc else acc
        in H.rnfX a' `seq` go (n + 1) a' sl' (bus, d1) acc'

allColours :: [Rgb444]
allColours = [ Rgb444 r g b | r <- [0 .. 15], g <- [0 .. 15], b <- [0 .. 15] ]

-- | Y, Cb, Cr of a colour as our converter sends it (a pixel pair of it).
yccOf :: Rgb444 -> (Int, Int, Int)
yccOf c = case [ o | o <- outs, ycDe o ] of
  o0 : o1 : _ -> (fromIntegral (ycY o0), fromIntegral (ycC o0), fromIntegral (ycC o1))
  _           -> error "no output"
 where
  px = PixelIn 1 1 True (fromRgb444 c)
  idle = PixelIn 1 1 False (Rgb888 0 0 0)
  outs = map ysOut (scanl yccStep (yccInit 1 1) ([px, px] ++ replicate 4 idle))

finalStatus :: [(Int, AdvStatus)] -> AdvStatus
finalStatus xs = case xs of
  [] -> advStatus (advInit fast)
  _  -> snd (last xs)

writes :: Slave -> [(Word8, Word8)]
writes s = [ (r, v) | EvWrite r v <- reverse (slLog s) ]

tests :: TestTree
tests = testGroup "Adv7511"
  [ testCase "tableVec is the table (40 entries, same order), all checked against the PG" $ do
      H.toList tableVec @?= [ (teReg e, teMask e, teValue e) | e <- adv7511Table ]
      length adv7511Table @?= 40
      filter (not . teChecked) adv7511Table `seq` all teChecked adv7511Table @?= True

  , testCase "chip CSC (decoded from the register image) undoes Ycbcr422 for all 4096 colours" $ do
      let bad = [ (c, rgb, back) | c@(Rgb444 r g b) <- allColours
                , let (y, cb, cr) = yccOf c
                      (br, bg, bb) = cscRgb expected (y, cb, cr)
                      back = (fromIntegral br, fromIntegral bg, fromIntegral bb) :: (Int, Int, Int)
                      rgb = (17 * fromIntegral r, 17 * fromIntegral g, 17 * fromIntegral b)
                , not (close back rgb) ]
          close (a, b, c) (x, y, z) = all (<= 2) [abs (a - x), abs (b - y), abs (c - z)]
      take 5 bad @?= []

  , testCase "one configuration from the simulated chip's defaults writes Platform.Sim.Adv7511.expectedWrites" $ do
      let sl0 = slaveInit ((slaveConfig addr) { scRegs = Chip.chipDefaults })
          (_, sl) = runAdvFrom sl0 (\_ -> chip True) 60000
      writes sl @?= Chip.expectedWrites
      length Chip.expectedWrites @?= length adv7511Table

  , testCase "HDMI audio table: tableVecHdmiAudio is it; DVI table less 0xAF, then audio, HDMI mode last" $ do
      H.toList tableVecHdmiAudio @?= [ (teReg e, teMask e, teValue e) | e <- adv7511HdmiAudioTable ]
      all teChecked adv7511HdmiAudioTable @?= True
      take 39 adv7511HdmiAudioTable `sameEntries` init adv7511Table
      map (\e -> (teReg e, teMask e, teValue e)) (drop 39 adv7511HdmiAudioTable) @?=
        [ (0x0A, 0xFF, 0x11), (0x01, 0xFF, 0x00), (0x02, 0xFF, 0x18), (0x03, 0xFF, 0x00)
        , (0x15, 0xF0, 0x20), (0x0B, 0x80, 0x80), (0x4A, 0x60, 0x60)
        , (0x55, 0xFF, 0x00), (0x56, 0xFF, 0x18), (0x57, 0xFF, 0x00)
        , (0x73, 0xFF, 0x01), (0x74, 0xFF, 0x00), (0x75, 0xFF, 0x00), (0x76, 0xFF, 0x00), (0x77, 0xFF, 0x00)
        , (0x4A, 0x60, 0x00), (0x44, 0x78, 0x78), (0xAF, 0x02, 0x02) ]

  , testCase "HDMI audio table on the chip: written in order, HDMI mode, S/PDIF on, N = 6144" $ do
      let (ss, sl) = runAdvWith (advStepWith tableVecHdmiAudio) slave0 (\_ -> chip True) 90000
      finalStatus ss @?= AdvStatus True True True Nothing 0x14 1
      map fst (writes sl) @?= [ fromIntegral (teReg e) | e <- adv7511HdmiAudioTable ]
      M.delete 0x42 (slRegs sl) @?= expectedFor adv7511HdmiAudioTable
      [ M.lookup r (slRegs sl) | r <- [0xAF, 0x0B, 0x0A, 0x15, 0x44, 0x4A, 0x01, 0x02, 0x03] ]
        @?= map Just [0x16, 0x8E, 0x11, 0x21, 0x79, 0x80, 0x00, 0x18, 0x00]
      slErrors sl @?= []
      let sl0 = slaveInit ((slaveConfig addr) { scRegs = Chip.chipDefaults })
          (_, slSim) = runAdvWith (advStepWith tableVecHdmiAudio) sl0 (\_ -> chip True) 90000
      writes slSim @?= Chip.expectedWritesFor adv7511HdmiAudioTable

  , testCase "real timing (DomPix): 200 ms boot, 50 ms poll, 100 kHz I2C" $ do
      atBoot (advTiming @DomPix) @?= 5035246
      atPoll (advTiming @DomPix) @?= 1258811
      itQuarter (atI2c (advTiming @DomPix)) @?= 63

  , testCase "HPD high: revision read, the table applied once, register image = table" $ do
      let (ss, sl) = runAdv (\_ -> chip True) 60000
          st = finalStatus ss
      st @?= AdvStatus True True True Nothing 0x14 1
      M.delete 0x42 (slRegs sl) @?= expected
      -- the writes in table order (each entry once), no protocol errors
      length (writes sl) @?= length adv7511Table
      map fst (writes sl) @?= [ fromIntegral (teReg e) | e <- adv7511Table ]
      slErrors sl @?= []

  , testCase "HPD low: nothing written; HPD rising later configures" $ do
      let hpdAt n = n >= 20000
          (ss, sl) = runAdv (\n -> chip (hpdAt n)) 80000
          configuredAt = [ n | (n, s) <- ss, asConfigured s ]
      assertBool ("configured at " <> show (take 1 configuredAt)) (take 1 configuredAt > [20000])
      asInits (finalStatus ss) @?= 1
      length (writes sl) @?= length adv7511Table
      M.delete 0x42 (slRegs sl) @?= expected

  , testCase "HPD drop and return: the table is applied again" $ do
      let hpdAt n = n < 60000 || n >= 70000
          (ss, sl) = runAdv (\n -> chip (hpdAt n)) 140000
      -- configured, then not (HPD seen low), then configured again
      map (asConfigured . snd) ss `seqOf` [True, False, True]
      finalStatus ss @?= AdvStatus True True True Nothing 0x14 2
      M.delete 0x42 (slRegs sl) @?= expected

  , testCase "HPD glitch between two polls: the power bit gives it away, table again" $ do
      -- 100 clocks low is far shorter than a poll period (500 + a read)
      let hpdAt n = n < 60000 || n >= 60100
          (ss, sl) = runAdv (\n -> chip (hpdAt n)) 120000
      asInits (finalStatus ss) @?= 2
      asConfigured (finalStatus ss) @?= True
      -- no poll saw HPD low, so it was the power register
      [ n | (n, st) <- ss, n > 1000, not (asHpd st) ] @?= []
      M.delete 0x42 (slRegs sl) @?= expected

  , testCase "no chip at the address: NackAddr reported, retried, configured once it answers" $ do
      let present n = n >= 30000
          env n s = chip True (if present n then s { slCfg = (slCfg s) { scAddr = addr } }
                                            else s { slCfg = (slCfg s) { scAddr = 0x10 } })
          (ss, sl) = runAdv env 100000
          before = [ s | (n, s) <- ss, n < 30000 ]
      assertBool "error while absent" (any ((== Just NackAddr) . asError) before)
      assertBool "never configured while absent" (not (any asConfigured before))
      finalStatus ss @?= AdvStatus True True True Nothing 0x14 1
      M.delete 0x42 (slRegs sl) @?= expected

  , testCase "statusWord layout" $ do
      statusWord (AdvStatus True True False Nothing 0x14 5) @?= 0x1443
      statusWord (AdvStatus False False False (Just I2cTimeout) 0 0) @?= 0x0038
      statusWord (AdvStatus False True True (Just NackAddr) 0 0) @?= 0x000E
  ]
 where
  -- the list, with repeats collapsed, starts with these values
  seqOf xs want = take (length want) (collapse (dropWhile not xs)) @?= want
  collapse (a : b : r) | a == b = collapse (b : r)
  collapse (a : r) = a : collapse r
  collapse [] = []

sameEntries :: [TableEntry] -> [TableEntry] -> Assertion
sameEntries a b = map key a @?= map key b
 where key e = (teReg e, teMask e, teValue e, teSource e)
