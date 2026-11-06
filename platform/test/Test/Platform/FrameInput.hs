module Test.Platform.FrameInput (tests) where

import Prelude

import qualified Clash.Prelude as C
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog

import Platform.Domains (DomPix)
import Platform.Ps.FrameInput

-- | The latch run over (sync, frame start, live word) inputs, the core
-- not held: the core's words and the toggles.
run :: [(Bool, Bool, C.BitVector 16)] -> [(C.BitVector 16, Bool)]
run ins = runHeld [(False, s, st, l) | (s, st, l) <- ins]

-- | The same with the hold: (hold, sync, frame start, live word).
runHeld :: [(Bool, Bool, Bool, C.BitVector 16)] -> [(C.BitVector 16, Bool)]
runHeld ins =
  let f h s st l = C.bundle (frameInput h s st l)
  in take (length ins) (C.simulateN @DomPix (length ins)
       (\i -> C.withClockResetEnable C.clockGen C.resetGen C.enableGen
                (let (h, s, st, l) = C.unbundle i in f h s st l))
       ins)

genInputs :: Gen [(Bool, Bool, C.BitVector 16)]
genInputs = do
  n <- Gen.int (Range.linear 2 200)
  sync <- Gen.bool
  Gen.list (Range.singleton n)
    ((,,) sync <$> Gen.frequency [(1, pure True), (9, pure False)] <*> (fromIntegral <$> Gen.int (Range.linear 0 65535)))

tests :: TestTree
tests = testGroup "Platform.Ps.FrameInput"
  [ testCase "the movie register: 0xF01003's bit 0; other writes ignored; off at reset" $ do
      psMovieInit @?= PsMovie False
      psMovieStep psMovieInit (Just (0xF01003, 0x01)) @?= PsMovie True
      psMovieStep (PsMovie True) (Just (0xF01003, 0x00)) @?= PsMovie False
      psMovieStep (PsMovie True) (Just (0xF01000, 0x00)) @?= PsMovie True
      psMovieStep (PsMovie True) (Just (0xF01004, 0x00)) @?= PsMovie True
      psMovieStep (PsMovie True) Nothing @?= PsMovie True
  , testProperty "not synchronised: the core sees the live word, as before" $ property $ do
      ins <- forAll genInputs
      let ins' = [(False, st, l) | (_, st, l) <- ins]
      map fst (run ins') === [l | (_, _, l) <- ins']
  , testProperty "synchronised: the word the live one had at the last frame start (0 before any), constant in between" $ property $ do
      ins <- forAll genInputs
      let ins' = [(True, st, l) | (_, st, l) <- ins]
          -- the latch takes the word at a start; the core sees it from the next clock
          latched = scanl (\w (_, st, l) -> if st then l else w) 0 ins'
      map fst (run ins') === take (length ins') latched
  , testProperty "the toggle flips once per frame start, whatever sync says" $ property $ do
      ins <- forAll genInputs
      let flips = scanl (\t (_, st, _) -> if st then not t else t) False ins
      map snd (run ins) === take (length ins) flips
  , testProperty "held: the latch follows the live word without a flip; after the hold, the word it had at the end" $ property $ do
      ins <- forAll genInputs
      -- a hold over the first part (frame starts there too: the core makes
      -- none while held, but the latch must not count them anyway)
      k <- forAll (Gen.int (Range.linear 1 (length ins - 1)))
      let ins' = [(i < k, True, st, l) | (i, (_, st, l)) <- zip [0 :: Int ..] ins]
          latched = scanl (\w (h, _, st, l) -> if h || st then l else w) 0 ins'
          flips = scanl (\t (h, _, st, _) -> if st && not h then not t else t) False ins'
      runHeld ins' === take (length ins') (zip latched flips)
  , testCase "frame advance: the step bit of a control write, other writes not" $ do
      stepRequest (Just (0xF01000, 0x06)) @?= True
      stepRequest (Just (0xF01000, 0x02)) @?= False
      stepRequest (Just (0xF01001, 0x04)) @?= False
      stepRequest Nothing @?= False
  , testProperty "frame advance: paused, each step runs to the next frame start and pauses the clock after; unpaused, as the PS says" $ property $ do
      n <- forAll (Gen.int (Range.linear 4 300))
      ins <- forAll (Gen.list (Range.singleton n)
               ((,,) <$> Gen.frequency [(1, pure True), (20, pure False)]   -- a step write
                     <*> Gen.frequency [(1, pure True), (12, pure False)]   -- a frame start
                     <*> Gen.frequency [(9, pure True), (1, pure False)]))  -- paused
      let link = [if r then Just (0xF01000, 0x06) else Nothing | (r, _, _) <- ins]
          starts = [st | (_, st, _) <- ins]
          pauses = [p | (_, _, p) <- ins]
          -- the model: a step starts the clock after its write, and is
          -- paused again the clock after the frame start
          model = go False ins
          go _ [] = []
          go s ((r, st, p) : rest) = (p && not s) : go (frameAdvanceStep s (r, st, p)) rest
          got = C.simulateN @DomPix n
                  (\i -> C.withClockResetEnable C.clockGen C.resetGen C.enableGen
                           (let (l, st, p) = C.unbundle i in frameAdvance l st p))
                  (zip3 link starts pauses)
      got === model
  , testCase "frame advance: three steps from a pause are three frames" $ do
      -- frame starts every 10 clocks, steps at 3, 25, 47: the core runs
      -- from each to the next start (that clock included) and only then
      let n = 70
          starts = [i `mod` 10 == 9 | i <- [0 .. n - 1]]
          link = [if i `elem` [3, 25, 47] then Just (0xF01000, 0x06) else Nothing | i <- [0 .. n - 1 :: Int]]
          got = C.simulateN @DomPix n
                  (\i -> C.withClockResetEnable C.clockGen C.resetGen C.enableGen
                           (let (l, st) = C.unbundle i in frameAdvance l st (pure True)))
                  (zip link starts)
          running = [i | (i, p) <- zip [0 :: Int ..] got, not p]
          framesRun = length [() | (i, st) <- zip [0 :: Int ..] starts, st, i - 1 `elem` running]
      running @?= [4 .. 9] ++ [26 .. 29] ++ [48, 49]
      framesRun @?= 3
  , testCase "PS_IN bit 7 carries the toggle, the rest unchanged" $ do
      psInFrame True 0x12345600 @?= 0x12345680
      psInFrame False 0x12345600 @?= 0x12345600
      psInFrame True 0xFFFFFFFF @?= 0xFFFFFFFF
  ]
