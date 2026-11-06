-- | Layer 1: pure pieces of the Game Boy hardware design. Step 2.3: the
-- decoder ("Gb.Decode"), exhaustively, and against the reference model.
module Main (main) where

import Prelude

import Control.Monad (replicateM_)
import Control.Monad.ST (runST)
import qualified Data.ByteString as BS
import Data.List (nub, sort)
import Data.Word
import Test.Tasty
import Test.Tasty.HUnit

import Clash.Prelude (BitVector)
import Gb.Decode
import Gb.Loader (SaveCtl (..), cramLoad, cramRead, palLoad, rtcLoad, rtcRead, saveCtlInit, saveCtlStep)
import Platform.Ps.Link (PsGpio (..), linkInit, linkStep)
import Gb.Display (Banks (..), Palette (..), banksInit, banksStep, div3, dotIncrement, palette)
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty.Hedgehog (testProperty)
import Platform.Video.VgaTiming (Rgb444 (..))
import Gb.Spec
import Gb.Types
import qualified Gb.Core as K
import qualified Clash.Prelude as C
import Gb.Spec.Machine (cgbDefaultPalette)

main :: IO ()
main = defaultMain $ testGroup "gb-unit"
  [ decoderTests
  , specAgreement
  , displayTests
  , loaderTests
  , bankTests
  , cgbTests
  ]

bytes :: [BitVector 8]
bytes = [minBound .. maxBound]

enumAll :: (Enum a, Bounded a) => [a]
enumAll = [minBound .. maxBound]

-- | Every valid 'Instr' (LD (HL),(HL) is HALT, so it isn't one).
allInstrs :: [Instr]
allInstrs = concat
  [ [Nop, LdNnSp, Stop, Jr, Halt, LdhNA, AddSpE, LdhAN, LdHlSpE, Ret, Reti, JpHl, LdSpHl]
  , [LdhCA, LdNnA, LdhAC, LdANn, Jp, PrefixCb, Di, Ei, Call]
  , map JrCc enumAll, map RetCc enumAll, map JpCc enumAll, map CallCc enumAll
  , map LdRpNn enumAll, map AddHlRp enumAll, map IncRp enumAll, map DecRp enumAll
  , map LdMemA enumAll, map LdAMem enumAll
  , map Inc enumAll, map Dec enumAll, map LdRN enumAll, map Acc enumAll
  , [ LdRR d s | d <- enumAll, s <- enumAll, (d, s) /= (HLInd, HLInd) ]
  , [ Alu o r | o <- enumAll, r <- enumAll ]
  , map Pop enumAll, map Push enumAll, map AluN enumAll, map Rst [minBound .. maxBound]
  , map Ill enumAll
  ]

allCb :: [CbInstr]
allCb = [ Rot o r | o <- enumAll, r <- enumAll ]
     <> concat [ [Bit n r, Res n r, Set n r] | n <- [minBound .. maxBound], r <- enumAll ]

decoderTests :: TestTree
decoderTests = testGroup "decoder"
  [ testCase "total, and encode . decode == id for all 256 opcodes" $
      [ b | b <- bytes, encode (decode b) /= b ] @?= []
  , testCase "decode . encode == id for every Instr, and there are exactly 256" $ do
      [ i | i <- allInstrs, decode (encode i) /= i ] @?= []
      length allInstrs @?= 256
      length (nub (map encode allInstrs)) @?= 256
  , testCase "CB: encodeCb . decodeCb == id for all 256" $
      [ b | b <- bytes, encodeCb (decodeCb b) /= b ] @?= []
  , testCase "CB: decodeCb . encodeCb == id for every CbInstr, and there are 256" $ do
      [ c | c <- allCb, decodeCb (encodeCb c) /= c ] @?= []
      length (nub (map encodeCb allCb)) @?= 256
  , testCase "spot checks" $ do
      decode 0x00 @?= Nop
      decode 0x20 @?= JrCc CondNZ
      decode 0x38 @?= JrCc CondC
      decode 0x36 @?= LdRN HLInd
      decode 0x41 @?= LdRR B C
      decode 0x76 @?= Halt
      decode 0x9E @?= Alu AluSbc HLInd
      decode 0xC8 @?= RetCc CondZ
      decode 0xF1 @?= Pop StkAF
      decode 0xFF @?= Rst 7
      decode 0xDD @?= Ill IllDD
      decodeCb 0x37 @?= Rot RotSwap A
      decodeCb 0x7E @?= Bit 7 HLInd
      decodeCb 0xC0 @?= Set 0 B
  , testCase "the illegal opcodes are D3 DB DD E3 E4 EB EC ED F4 FC FD" $
      sort [ b | b <- bytes, isIll (decode b) ]
        @?= [0xD3, 0xDB, 0xDD, 0xE3, 0xE4, 0xEB, 0xEC, 0xED, 0xF4, 0xFC, 0xFD]
  ]

isIll :: Instr -> Bool
isIll (Ill _) = True
isIll _ = False

-- * Against the reference model

-- | Run one instruction on the spec after a preamble that sets F, SP and
-- HL: LD SP,$D000; POP AF; LD HL,$C000; then the bytes under test at
-- 0x0107. Returns the M-cycles of that step, PC after it, and whether
-- the CPU locked up (the next step is a single idle M-cycle).
runOne :: Word8 -> [Word8] -> (Int, Word16, Bool)
runOne f code = runST $ do
  m <- either error id <$> newMachine rom
  poke m 0xD000 f
  poke m 0xD001 0x12
  replicateM_ 3 (step m)
  (_, t) <- withTrace m (step m)
  pc <- rgPC <$> regs m
  (_, t2) <- withTrace m (step m)
  pure (length t, pc, t2 == [AIdle])
 where
  pre = [0x31, 0x00, 0xD0, 0xF1, 0x21, 0x00, 0xC0]
  prog = pre <> code
  rom = BS.concat [BS.replicate 0x100 0, BS.pack prog, BS.replicate (0x8000 - 0x100 - length prog) 0]

-- | Does the condition hold with these flags?
holds :: Word8 -> Cond -> Bool
holds f c = case c of
  CondNZ -> f < 0x80
  CondZ -> f >= 0x80
  CondNC -> f `mod` 0x20 < 0x10
  CondC -> f `mod` 0x20 >= 0x10

-- | The condition of a conditional instruction.
condOf :: Instr -> Maybe Cond
condOf i = case i of
  JrCc c -> Just c
  RetCc c -> Just c
  JpCc c -> Just c
  CallCc c -> Just c
  _ -> Nothing

-- | Instructions that may leave PC somewhere other than after themselves.
jumps :: Instr -> Bool
jumps i = case i of
  Jr -> True
  Ret -> True
  Reti -> True
  JpHl -> True
  Jp -> True
  Call -> True
  Rst _ -> True
  _ -> False

specAgreement :: TestTree
specAgreement = testGroup "decoder agrees with the reference model (Gb.Spec)"
  [ testCase "M-cycles, length and lock-up of every opcode, conditions false and true" $
      [ (b, f, got, want)
      | b <- bytes, decode b /= PrefixCb
      , f <- [0x00, 0xF0]
      , let i = decode b
            (cyc, pc, locked) = runOne f [fromIntegral b, 0x00, 0x00]
            taken = maybe True (holds f) (condOf i)
            (nt, tk) = instrCycles i
            wantCyc = fromIntegral (if taken then tk else nt) :: Int
            wantPc = if jumps i || (taken && condOf i /= Nothing) then Nothing
                     else Just (0x107 + fromIntegral (instrLength i))
            got = (cyc, if wantPc == Nothing then Nothing else Just pc, locked)
            want = (wantCyc, wantPc, isIll i)
      , got /= want
      ] @?= []
  , testCase "M-cycles and length of every CB instruction" $
      [ (b, got, want)
      | b <- bytes
      , let c = decodeCb b
            (cyc, pc, _) = runOne 0x00 [0xCB, fromIntegral b]
            got = (cyc, pc)
            want = (1 + fromIntegral (cbCycles c), 0x107 + fromIntegral (instrLength PrefixCb))
      , got /= want
      ] @?= []
  ]

-- * Display (step 2.7)

displayTests :: TestTree
displayTests = testGroup "display arithmetic"
  [ testCase "div3 v == v `div` 3 for every v < 1024" $
      [ v | v <- [0 .. 1023 :: Int], fromIntegral (div3 (fromIntegral v)) /= v `div` 3 ] @?= []
  , testCase "dotEnable: 4194304 Hz from 25.17623 MHz within 1 ppm over a second of clocks" $ do
      let clocks = 25176233 :: Integer
          dots = (fromIntegral dotIncrement * clocks) `div` (2 ^ (32 :: Int))
      assertBool (show dots) (abs (dots - 4194304) <= 4)
  , testCase "palette: grey shade 0 is white, 3 black" $
      (palette Grey 0, palette Grey 3) @?= (Rgb444 15 15 15, Rgb444 0 0 0)
  ]

-- * Loader (step 2.8)

loaderTests :: TestTree
loaderTests = testGroup "cartridge loading over the PS link (Gb.Loader)"
  [ testCase "three address bytes, bytes written in order, nothing once released" $ do
      let hold d a st = PsGpio 0 d st a True
          steps = [ hold 0 False False
                  , hold 0x10 True True, hold 0x00 True False, hold 0x00 True True -- 0x100000
                  , hold 0x11 False False, hold 0x22 False True
                  , PsGpio 0 0x33 False False False ]                             -- released
          go (st, acc) g = let (st', w) = linkStep st g in (st', acc ++ [cramLoad (pgHold g) w])
          outs = snd (foldl go (linkInit, []) steps)
      outs @?= [Nothing, Nothing, Nothing, Nothing, Just (0, 0x11), Just (1, 0x22), Nothing]
  , testCase "cartridge RAM (side track I): written only while held, read at any time, 128 KiB" $ do
      cramLoad True (Just (0x100000, 0x53)) @?= Just (0, 0x53)
      cramLoad True (Just (0x11FFFF, 0x01)) @?= Just (0x1FFFF, 0x01)
      cramLoad True (Just (0x120000, 0x01)) @?= Nothing
      cramLoad True (Just (0x0FFFFF, 0x01)) @?= Nothing
      cramLoad False (Just (0x100000, 0x53)) @?= Nothing
      cramRead (Just (0x120000, 0x00)) @?= Just 0
      cramRead (Just (0x13FFFF, 0xFF)) @?= Just 0x1FFFF
      cramRead (Just (0x140000, 0x00)) @?= Nothing
      cramRead (Just (0x100000, 0x00)) @?= Nothing
      cramRead Nothing @?= Nothing
  , testCase "MBC3's clock (phase 2): its start values while held, read back at any time" $ do
      rtcLoad True (Just (0x140010, 0x3B)) @?= Just (0, 0x3B)
      rtcLoad True (Just (0x140019, 0x01)) @?= Just (9, 0x01)
      rtcLoad True (Just (0x14001A, 0x01)) @?= Nothing
      rtcLoad False (Just (0x140010, 0x3B)) @?= Nothing
      rtcRead (Just (0x140020, 0)) @?= Just 0
      rtcRead (Just (0x140029, 0)) @?= Just 9
      rtcRead (Just (0x14002A, 0)) @?= Nothing
      rtcRead (Just (0x140010, 0)) @?= Nothing
  , testCase "save control: read-back bit, dirty set by the game, cleared by the PS (a write in the same clock wins)" $ do
      let steps = [ (Nothing, False), (Nothing, True), (Nothing, False)
                  , (Just (0x140000, 0x01), False), (Just (0x140001, 0x00), False)
                  , (Just (0x140001, 0x00), True), (Just (0x140001, 0x00), False)
                  , (Just (0x140000, 0xFE), False), (Just (0x140002, 0x00), True) ]
          outs = drop 1 (scanl saveCtlStep saveCtlInit steps)
      map scDirty outs @?= [False, True, True, True, False, True, False, False, True]
      map scReadBack outs @?= [False, False, False, True, True, True, True, False, False]
  ]

-- * Frame buffer banks (tearing fix)

bankTests :: TestTree
bankTests = testGroup "frame buffer banks (three; the core never draws into the frame on screen)"
  [ testProperty "any sequence of frame ends and VBlanks: invariant, and the display shows the newest complete frame" $
      H.withTests 2000 $ H.property $ do
        evs <- H.forAll (Gen.list (Range.linear 0 300) ((,) <$> Gen.bool <*> Gen.bool))
        let go _ _ [] = pure ()
            go b newest (e@(fd, vb) : rest) = do
              let b' = banksStep b e
                  newest' = if fd then Just (bkW b) else newest
              H.assert (bkW b' /= bkR b')
              H.assert (bkW b' /= bkReady b')
              -- at a VBlank the display takes the newest frame completed before this clock
              case (vb, newest) of
                (True, Just n) -> bkR b' H.=== n
                _ -> pure ()
              go b' newest' rest
        go banksInit Nothing evs
  , testCase "a core faster than the display alternates between the two banks not on screen" $ do
      let steps = scanl banksStep banksInit (replicate 6 (True, False))
      [ bkW b | b <- steps ] @?= [0, 2, 0, 2, 0, 2, 0]
  ]

-- * The Game Boy Color in the core (side track I, phase 3)

cgbTests :: TestTree
cgbTests = testGroup "the Game Boy Color in Gb.Core"
  [ testCase "the palette window (Gb.Loader): 24 bytes from 0x140030, only while held" $
      ( palLoad True (Just (0x140030, 0x12)), palLoad True (Just (0x140047, 0x34))
      , palLoad True (Just (0x140048, 0x56)), palLoad False (Just (0x140030, 0x12)) )
        @?= (Just (0, 0x12), Just (23, 0x34), Nothing, Nothing)
  , testCase "the core's default palettes = the spec's (BG 0, OBJ 0, OBJ 1)" $
      C.toList K.gbcDefaultPal
        @?= [ fromIntegral (c `div` d) | _ <- [1 .. 3 :: Int], c <- cgbDefaultPalette, d <- [1, 256 :: Int] ]
  , testCase "a CPU write to VRAM goes to bank 1 while VBK is 1 (CGB mode), else to bank 0" $ do
      -- regression: the bank bit was ORed into a 13-bit slice and lost
      -- (found by cgb-acid2 in gb-lockstep)
      let s0 = K.coreInit
          cfg c = (K.ksCfg s0) { K.ccCgbHw = True, K.ccCgb = c }
          s c v = s0 { K.ksCfg = cfg c, K.ksVbk = v }
          addr c v = (\(_, q, _, _) -> K.mqAddr <$> q) (K.cpuWrite (cfg c) (s c v) 0x9880 0x12)
      (addr True False, addr True True, addr False True) @?= (Just 0x1880, Just 0x3880, Just 0x1880)
  ]
