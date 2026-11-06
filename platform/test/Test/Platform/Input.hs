module Test.Platform.Input (tests) where

import Prelude
import qualified Data.List as L

import qualified Clash.Prelude as C
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.HUnit hiding (assert)
import Test.Tasty.Hedgehog

import Platform.Domains
import Platform.Input.Debounce
import Platform.Input.Sync

-- | Debounce window used by the tests.
type N = 8

n :: Int
n = 8

-- | Run the pure transition function from the reset state.
runPure :: [Bool] -> [Bool]
runPure = snd . L.mapAccumL (debounceStep (C.SNat @N)) (False, 0)

-- | Run the Signal version. The first cycle is spent in reset, so the
-- stimulus is prefixed with the reset value to line up with 'runPure'.
runSignal :: [Bool] -> [Bool]
runSignal xs =
  drop 1 (C.simulateN @C.System (length xs + 1) (debounce (C.SNat @N) False) (False : xs))

-- | Alternating runs starting with True, ending with False, every run shorter
-- than the debounce window.
genBounce :: Gen [Bool]
genBounce = do
  runs <- Gen.list (Range.linear 0 20) (Gen.int (Range.linear 1 (n - 1)))
  let lens = if even (length runs) then runs else runs ++ [1]
  pure (concat (zipWith replicate lens (cycle [True, False])))

tests :: TestTree
tests = testGroup "Input"
  [ testGroup "debounce"
    [ testProperty "glitches shorter than N cycles are ignored" $ property $ do
        k   <- forAll (Gen.int (Range.linear 1 (n - 1)))
        pre <- forAll (Gen.int (Range.linear 0 20))
        let input = replicate pre False ++ replicate k True ++ replicate (3 * n) False
        assert (not (or (runPure input)))

    , testProperty "a change held for >= N cycles appears exactly N cycles later" $ property $ do
        pre  <- forAll (Gen.int (Range.linear 0 20))
        hold <- forAll (Gen.int (Range.linear n (3 * n)))
        let input = replicate pre False ++ replicate hold True ++ replicate (2 * n) False
            out   = runPure input
        L.findIndex id out === Just (pre + n)
        -- and the release is debounced the same way
        length (filter id out) === hold

    , testProperty "bouncing, then stable: output switches exactly once, N cycles after it settles" $ property $ do
        bounce <- forAll genBounce
        let settle = length bounce
            out    = runPure (bounce ++ replicate (2 * n) True)
        out === replicate (settle + n) False ++ replicate n True

    , testProperty "Signal version matches the pure transition function" $ property $ do
        xs <- forAll (Gen.list (Range.linear 1 200) Gen.bool)
        runSignal xs === runPure xs

    , testCase "MsToCycles: 10 ms" $ do
        C.natToNum @(MsToCycles DomPix 10) @?= (251762 :: Integer)  -- 10e9 ps / 39720 ps, rounded down
        C.natToNum @(MsToCycles Dom100 10) @?= (1000000 :: Integer)
    ]

  , testGroup "synchronise"
    [ testProperty "two cycles of latency" $ property $ do
        xs <- forAll (Gen.list (Range.linear 4 100) (Gen.integral (Range.constantBounded :: Range C.Bit)))
        let out = C.simulateN @C.System (length xs) (synchronise 0) xs
        -- cycle 0 is reset; after that the output is the input two cycles earlier
        drop 3 out === drop 1 (take (length xs - 2) xs)
    ]
  ]
