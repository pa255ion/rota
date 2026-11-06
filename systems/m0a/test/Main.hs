module Main (main) where

import Prelude

import Data.Bits (testBit, (.&.))
import qualified Clash.Explicit.Prelude as C
import qualified Clash.Prelude as H
import Test.Tasty
import Test.Tasty.HUnit

import M0a.Top
import qualified M0a.ArtyTop as Arty
import Platform.Domains

main :: IO ()
main = defaultMain $ testGroup "m0a"
  [ testGroup "ledView"
    [ testCase "exhaustive: every LED against the PLAN.md mapping" $
        sequence_
          [ assertEqual (show (hb, b, s, i)) (expected hb b s i) (testBit (ledView hb b s) i)
          | hb <- [False, True], b <- [0 .. 31], s <- [0 .. 255], i <- [0 .. 7] ]
    ]

  , testGroup "m0aLeds (debounce 3, heartbeat 8)"
    [ testCase "BTNL alone lights LD2 once debounced" $
        steady (oneHot 2) 0 @?= 0x04
    , testCase "each button lights only its own LED" $
        sequence_
          [ assertEqual ("button " <> show i) (2 ^ i) (steady (oneHot i) 0)
          | i <- [0 .. 4] ]
    , testCase "SW7 on: LD0-6 show SW0-6, buttons ignored" $
        steady (H.repeat 1) 0xA5 @?= 0x25
    , testCase "SW7 off: switches ignored" $
        steady (H.repeat 0) 0x7F @?= 0
    , testCase "a 2-cycle button press never reaches the LEDs" $ do
        let btn = [if t `elem` [20, 21] then oneHot 0 else H.repeat 0 | t <- [0 :: Int ..]]
            out = C.sampleN 60 (small (C.fromList btn) (pure 0))
        filter (\v -> v .&. 0x7F /= 0) out @?= []
    , testCase "heartbeat on LD7 toggles every 8 cycles" $ do
        let hb = map (`testBit` 7) (drop 20 (C.sampleN 52 (small (pure (H.repeat 0)) (pure 0))))
        -- runs of equal values, ignoring the partial first and last runs
        map length (take 3 (drop 1 (runs hb))) @?= [8, 8, 8]
    ]

  , testGroup "topEntity (through the clock wizard model, real 10 ms debounce)"
    [ testCase "port order: BTNU lights LD4, SW7+SW0 light LD0" $ do
        let n = debounceCycles + 50
            run u sw = last (C.sampleN n (topEntity C.clockGen
                                (pure 0) (pure 0) (pure 0) (pure 0) (pure u) (pure sw)))
                         .&. 0x7F
        run 1 0 @?= 0x10
        run 0 0x81 @?= 0x01
    ]

  , testGroup "Arty Z7-20 (side track G; through the MMCM blackbox's model)"
    [ testCase "artyM0aInputs: BTN0..3 as BTNC, BTND, BTNL, BTNR; SW1 as SW1 and SW7" $ do
        Arty.artyM0aInputs 0b1001 0b00 @?= (1 H.:> 0 H.:> 0 H.:> 1 H.:> 0 H.:> H.Nil, 0)
        Arty.artyM0aInputs 0 0b10 @?= (H.repeat 0, 0x82)
        Arty.artyM0aInputs 0 0b01 @?= (H.repeat 0, 0x01)
    , testCase "port order: BTN3 lights LD3; SW1 + SW0 light LD0 and LD1" $ do
        let n = debounceCycles + 50
            run btn sw = last (C.sampleN n (fst (Arty.topEntity C.clockGen (pure btn) (pure sw))))
        run 0b1000 0 @?= 0b1000
        run 0b0001 0 @?= 0b0001
        run 0 0b11 @?= 0b0011
        run 0b1111 0b10 @?= 0b0010
    ]
  ]
 where
  expected hb b s i
    | i == 7          = hb
    | testBit s 7     = testBit s i
    | i < 5           = testBit b i
    | otherwise       = False

  oneHot :: Int -> H.Vec 5 H.Bit
  oneHot i = H.imap (\j _ -> if fromEnum j == i then 1 else 0) (H.repeat (0 :: H.Bit))

  small :: C.Signal C.System (H.Vec 5 H.Bit) -> C.Signal C.System (H.BitVector 8) -> C.Signal C.System (H.BitVector 8)
  small = H.withClockResetEnable C.clockGen C.resetGen C.enableGen
            (m0aLeds (H.SNat @3) (H.SNat @8))

  -- LEDs (heartbeat masked off) once constant inputs have settled.
  steady btns sw = last (C.sampleN 30 (small (pure btns) (pure sw))) .&. 0x7F

  runs [] = []
  runs (x : xs) = let (a, b) = span (== x) xs in (x : a) : runs b

  debounceCycles = fromInteger (C.snatToInteger (H.SNat @(MsToCycles DomPix 10))) :: Int
