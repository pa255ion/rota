-- | Layer 1: pure pieces of the hardware design.
module Main (main) where

import Prelude

import qualified Clash.Explicit.Prelude as CE
import qualified Clash.Prelude as C
import Data.Bits ((.|.))
import qualified Data.Vector.Unboxed as U
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog

import qualified Chip8.ArtyTop as Arty
import Chip8.Decode
import Chip8.Peripherals
import qualified Chip8.Spec as Spec
import Chip8.Types

main :: IO ()
main = defaultMain $ testGroup "chip8-unit"
  [ testGroup "decoder"
    [ testCase "total, and encode . decode == id for all 65536 words" $
        [ w | w <- [minBound .. maxBound], encode (decode w) /= w ] @?= []
    , testProperty "decode . encode == id" $ withTests 2000 $ property $ do
        i <- forAll genInstr
        decode (encode i) === i
    , testCase "illegal exactly where the reference model halts" $
        [ w | w <- [minBound .. maxBound], isIllegal (decode w) /= specIllegal w ] @?= []
    , testCase "spot checks" $ do
        decode 0x00E0 @?= Cls
        decode 0x8AB4 @?= Alu AluAdd 0xA 0xB
        decode 0xD12F @?= Drw 1 2 15
        decode 0xF365 @?= Load 3
        decode 0x0123 @?= Illegal 0x0123
    ]
  , testGroup "Arty Z7-20 (side track G)"
    [ testCase "buttons: BTN0..3 are keys 4, 8, 2, 6; with SW1 (shift) A, F, E, B" $ do
        let keys btn sw = keysOfList (map (== 1) (C.toList (Arty.artyButtons btn sw)))
        map (\i -> keys (2 ^ i) 0) [0 .. 3 :: Int] @?= [0x0010, 0x0100, 0x0004, 0x0040]
        map (\i -> keys (2 ^ i) 2) [0 .. 3 :: Int] @?= [0x0400, 0x8000, 0x4000, 0x0800]
        keys 0 0b11 @?= 0
    , testCase "LEDs: LD0 halted, LD1 key wait, LD2 vblank wait, LD3 sound" $ do
        Arty.artyLeds 0x00001000 False @?= 0b0001
        Arty.artyLeds 0x00002000 False @?= 0b0010
        Arty.artyLeds 0x00004000 False @?= 0b0100
        Arty.artyLeds 0xFFFF8FFF True @?= 0b1000
    ]

  , testGroup "peripherals"
    [ testCase "button key table (exhaustive over the 32 button states)" $
        sequence_
          [ assertEqual (show bs) (expectedKeys bs) (keysOfList bs)
          | bs <- sequence (replicate 5 [False, True]) ]
    , testCase "instrTicker: a pulse every 7 clocks, every 3 when fast" $ do
        let run fs = CE.sampleN 40 (C.withClockResetEnable @C.System CE.clockGen CE.resetGen CE.enableGen
                       (instrTicker (C.SNat @7) (C.SNat @3) (pure fs)))
        gaps (run False) @?= replicate (length (gaps (run False))) 7
        gaps (run True) @?= replicate (length (gaps (run True))) 3
        assertBool "pulses" (length (gaps (run True)) > 5)
    , testCase "beeper: half period n while on, low when off" $ do
        let on  = CE.sampleN 60 (C.withClockResetEnable @C.System CE.clockGen CE.resetGen CE.enableGen
                    (beeper (C.SNat @5) (pure True)))
            off = CE.sampleN 60 (C.withClockResetEnable @C.System CE.clockGen CE.resetGen CE.enableGen
                    (beeper (C.SNat @5) (pure False)))
        take 3 (drop 1 (map length (runs (drop 10 on)))) @?= [5, 5, 5]
        all (== 0) off @? "silent"
    , testCase "real rates at the 25.176 MHz pixel clock: 700 Hz, 10 kHz, 440 Hz, 59.94 Hz" $ do
        let fpix = 1e12 / 39720 :: Double
            near want got = abs (got - want) / want < 0.001
        assertBool "slow" (near 700 (fpix / fromIntegral (C.natToNum @SlowPeriod :: Int)))
        assertBool "fast" (near 10000 (fpix / fromIntegral (C.natToNum @FastPeriod :: Int)))
        assertBool "beep" (near 440 (fpix / (2 * fromIntegral (C.natToNum @BeepHalfPeriod :: Int))))
        assertBool "60 Hz" (near 59.94 (fpix / (800 * 525)))
    ]
  ]

isIllegal :: Instr -> Bool
isIllegal (Illegal _) = True
isIllegal _           = False

-- | Does the reference model halt on this opcode as illegal? (Run it at
-- 0x300 so no stack is involved and the opcode can't be overwritten.)
specIllegal :: C.BitVector 16 -> Bool
specIllegal w = case Spec.mHalt m' of
  Just (Spec.HaltIllegal _) -> True
  _                         -> False
 where
  m0 = Spec.newMachine
  m  = m0 { Spec.mPc = 0x300
          , Spec.mRam = Spec.mRam m0 U.// [(0x300, fromIntegral (w `C.shiftR` 8)), (0x301, fromIntegral w)] }
  m' = Spec.step Spec.vipQuirks 0 m

genInstr :: Gen Instr
genInstr = Gen.choice
  [ pure Cls, pure Ret
  , Jp <$> addr, Call <$> addr
  , SeImm <$> reg <*> byte, SneImm <$> reg <*> byte, SeReg <$> reg <*> reg
  , LdImm <$> reg <*> byte, AddImm <$> reg <*> byte
  , Alu <$> Gen.enumBounded <*> reg <*> reg
  , SneReg <$> reg <*> reg, LdI <$> addr, JpOff <$> addr, Rnd <$> reg <*> byte
  , Drw <$> reg <*> reg <*> Gen.enumBounded
  , Skp <$> reg, Sknp <$> reg
  , LdVxDt <$> reg, LdKey <$> reg, LdDtVx <$> reg, LdStVx <$> reg, AddI <$> reg
  , LdFont <$> reg, Bcd <$> reg, Store <$> reg, Load <$> reg
  , Illegal <$> Gen.filter (isIllegal . decode) (Gen.enumBounded)
  ]
 where
  reg  = Gen.enumBounded
  addr = Gen.enumBounded
  byte = Gen.enumBounded

-- | Buttons in the order C, D, L, R, U.
keysOfList :: [Bool] -> C.BitVector 16
keysOfList [c, d, l, r, u] = keysOf (c C.:> d C.:> l C.:> r C.:> u C.:> C.Nil)
keysOfList _ = error "five buttons"

-- | The table in docs/systems/chip8.md, restated.
expectedKeys :: [Bool] -> C.BitVector 16
expectedKeys [c, d, l, r, u] = foldr (.|.) 0
  [ if b then C.bit k else 0
  | (b, plain, shifted) <- [(u, 0x2, 0xE), (d, 0x8, 0xF), (l, 0x4, 0xA), (r, 0x6, 0xB)]
  , let k = if c then shifted else plain ]
expectedKeys _ = error "five buttons"

gaps :: [Bool] -> [Int]
gaps xs = let is = [ i | (i, True) <- zip [0 :: Int ..] xs ] in zipWith (-) (drop 1 is) is

runs :: Eq a => [a] -> [[a]]
runs [] = []
runs (x : xs) = let (a, b) = span (== x) xs in (x : a) : runs b
