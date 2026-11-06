-- | The DDR read port (Platform.Mem.Ddr) against the AXI slave model
-- (Platform.Mem.Sim): random latency and back-pressure, the AXI rules a
-- master keeps, hits, prefetch, invalidation, errors.
module Test.Platform.Ddr (tests) where

import Prelude
import Data.Bits (complement, shiftL, xor, (.&.))
import Data.Maybe (isJust, mapMaybe)
import Data.Word (Word64)

import qualified Clash.Prelude as C
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog

import Platform.Mem.Axi
import Platform.Mem.Ddr
import Platform.Mem.Sim

-- | The memory's contents: a word made from its address (and a version,
-- for the invalidation test).
memWord :: Int -> AxiAddr -> AxiData
memWord v a = C.pack (C.resize a * 0x9E3779B97F4A7C15 `xor` (fromIntegral v `shiftL` 56) :: C.Unsigned 64)

-- | One clock of a run.
data Clk = Clk
  { ckReq  :: Maybe DdrAddr
  , ckResp :: Maybe AxiData
  , ckM2S  :: AxiRdM2S
  , ckS2M  :: AxiRdS2M
  , ckStat :: DdrStatus
  } deriving Show

-- | The core side: each address of the list after the given number of
-- idle clocks, from the clock after the answer to the last.
data Driver = Driver
  { drTodo :: [(Int, DdrAddr)]
  , drWait :: Bool
  } deriving stock (Show, C.Generic)
    deriving anyclass C.NFDataX

driverStep :: Driver -> Maybe AxiData -> (Driver, Maybe DdrAddr)
driverStep d (Just _) = (d { drWait = False }, Nothing)
driverStep d Nothing = case drTodo d of
  (0, a) : rest | not (drWait d) -> (Driver rest True, Just a)
  (n, a) : rest | not (drWait d) -> (Driver ((n - 1, a) : rest) False, Nothing)
  _ -> (d, Nothing)

-- | Run the port (2^4 lines) against the slave for @n@ clocks. @mem@ and
-- @inval@ per clock (lists, extended with their last element).
run
  :: DdrConfig -> SlaveConfig -> Word64 -> [(Int, DdrAddr)]
  -> [AxiAddr -> AxiData] -> [Bool] -> Int -> [Clk]
run cfg sc seed todo mems invals n =
  C.sampleN_lazy n $ C.withClockResetEnable C.systemClockGen C.resetGen C.enableGen $
    let mem = C.fromList_lazy (mems ++ repeat (last mems))
        inval = C.fromList (invals ++ repeat False)
        -- the first request two clocks in, after the reset
        req = C.mealy driverStep (Driver (wait2 todo) False) resp
        wait2 ((d, a) : rest) = (d + 2, a) : rest
        wait2 [] = []
        s2m = axiSlave sc seed mem m2s
        (resp, m2s, stat) = ddrReadPort (C.SNat @4) cfg inval req s2m
    in Clk <$> req <*> resp <*> m2s <*> s2m <*> stat

-- | The requests and their answers in order, each with its latency
-- (clocks from the request to the answer).
answers :: [Clk] -> [(DdrAddr, Maybe AxiData, Int)]
answers = go 0
 where
  go _ [] = []
  go i (c : cs) = case ckReq c of
    Just a -> case break (isJust . ckResp . snd) (zip [i + 1 ..] cs) of
      (_, (j, r) : _) -> (a, ckResp r, j - i) : go (j + 1) (drop (j - i) cs)
      (_, []) -> [(a, Nothing, -1)]
    Nothing -> go (i + 1) cs

-- | The AXI rules of a master (A3.2, A3.3) over a run: an AR stays valid
-- with the same payload until ARREADY; every AR is a 4-beat burst of an
-- aligned 32-byte line (so it can't cross 4 KiB); at most one burst
-- outstanding; RREADY only while one is.
axiRules :: [Clk] -> Either String ()
axiRules = go Nothing (0 :: Int) . zip [0 :: Int ..]
 where
  go _ _ [] = Right ()
  go held out ((i, c) : cs) = do
    let ar = m2sAr (ckM2S c)
        hs = isJust ar && s2mArReady (ckS2M c)
        rDone = case s2mR (ckS2M c) of
          Just r | m2sRReady (ckM2S c) && rLast r -> 1
          _ -> 0
    case held of
      Just p | ar /= Just p -> Left ("clock " <> show i <> ": AR changed or dropped before ARREADY")
      _ -> Right ()
    case ar of
      Just (AxiAr a l) | a .&. 31 /= 0 || l /= 3 -> Left ("clock " <> show i <> ": AR " <> show (a, l))
      _ -> Right ()
    let out' = out + (if hs then 1 else 0) - rDone
    if out' > 1 then Left ("clock " <> show i <> ": two bursts outstanding") else Right ()
    if m2sRReady (ckM2S c) && out == 0 then Left ("clock " <> show i <> ": RREADY with no burst") else Right ()
    go (if isJust ar && not hs then ar else Nothing) out' cs

aligned :: DdrAddr -> DdrAddr
aligned a = a .&. complement 7

tests :: TestTree
tests = testGroup "DDR read port"
  [ testProperty "random requests, random slave timing: every answer is the memory's word, once, in order" $
      property $ do
        seed <- forAll (Gen.word64 Range.constantBounded)
        lat <- forAll (Gen.int (Range.linear 0 40))
        pre <- forAll Gen.bool
        -- a small range, so lines are hit, missed and evicted (16 lines of 32 bytes)
        todo <- forAll $ Gen.list (Range.linear 1 150) $
          (,) <$> Gen.frequency [(3, pure 0), (1, Gen.int (Range.linear 0 20))]
              <*> Gen.frequency [ (3, (0x100000 +) <$> Gen.integral (Range.linear 0 2047))
                                , (1, (0x1FF000 +) <$> Gen.integral (Range.linear 0 4095)) ]
        let n = 200 + length todo * (lat + 60)
            clks = run (DdrConfig pre) (slaveRandom lat) seed todo [memWord 0] [] n
            as = answers clks
        annotateShow (take 5 as)
        map (\(a, _, _) -> a) as === map snd todo
        [ (a, d) | (a, d, _) <- as, d /= Just (memWord 0 (aligned a)) ] === []
        -- never more answers than requests
        length (mapMaybe ckResp clks) === length todo
        axiRules clks === Right ()
        any (stError . ckStat) clks === False
  , testCase "a hit answers the next clock; a miss after its beat (fast slave)" $ do
      let clks = run (DdrConfig False) slaveFast 1 [(0, 0x100008), (10, 0x100010), (0, 0x100008)] [memWord 0] [] 40
      [ l | (_, _, l) <- answers clks ] @?= [3, 1, 1]
  , testCase "prefetch: reading forwards, after the first line every request hits" $ do
      let todo = [ (8, 0x200000 + 8 * i) | i <- [0 .. 63] ]
          clks cfg = run cfg (slaveRandom 6) 7 todo [memWord 0] [] 2000
          lat cfg = [ l | (_, _, l) <- answers (clks cfg) ]
          bursts cfg = length [ () | c <- clks cfg, isJust (m2sAr (ckM2S c)), s2mArReady (ckS2M c) ]
      assertBool "first one misses" (all (> 1) (take 1 (lat (DdrConfig True))))
      -- (the first line's other three come while it is being read)
      drop 4 (lat (DdrConfig True)) @?= replicate 60 1
      -- 16 lines, each read once; with prefetch also the one after them
      bursts (DdrConfig False) @?= 16
      bursts (DdrConfig True) @?= 17
      assertBool "without prefetch, misses on every line" (length (filter (> 1) (lat (DdrConfig False))) >= 16)
  , testCase "a request during a prefetch of another line still gets its hit at once" $ do
      -- line 0x100000 cached, then a request to the next line's neighbour
      -- starts a prefetch; a hit on the first line meanwhile answers in 1
      let todo = [(0, 0x100000), (20, 0x100020), (0, 0x100000), (0, 0x100008)]
          clks = run (DdrConfig True) (slaveRandom 20) 3 todo [memWord 0] [] 400
      [ (a, d == Just (memWord 0 a)) | (a, d, _) <- answers clks ] @?= map (\(_, a) -> (a, True)) todo
      axiRules clks @?= Right ()
  , testCase "invalidate: the PS rewrote the memory, the next read sees it" $ do
      let todo = [(0, 0x100000), (60, 0x100000), (0, 0x100008)]
          -- memory version 1 from clock 30, invalidate at clock 40
          clks = run (DdrConfig True) (slaveRandom 3) 5 todo
                   (replicate 30 (memWord 0) ++ [memWord 1]) (replicate 40 False ++ [True]) 300
      [ d | (_, d, _) <- answers clks ]
        @?= [Just (memWord 0 0x100000), Just (memWord 1 0x100000), Just (memWord 1 0x100008)]
  , testCase "invalidate during a fill: the old line isn't kept, the request after it refetches" $ do
      let todo = [(0, 0x100000), (0, 0x100008)]
          -- the first miss's burst is slow; the memory changes and the
          -- port is invalidated while it is under way
          clks = run (DdrConfig False) (slaveRandom 30) 9 todo
                   (replicate 6 (memWord 0) ++ [memWord 1]) (replicate 6 False ++ [True]) 200
      stBusy (ckStat (clks !! 6)) @? "a fill under way at the invalidation"
      -- the first answer may be old (requested before the invalidation)
      [ d | (_, d, _) <- drop 1 (answers clks) ] @?= [Just (memWord 1 0x100008)]
  , testCase "an SLVERR beat sets the sticky error flag" $ do
      let sc = slaveFast { scError = (== 0x300010) }
          clks = run (DdrConfig False) sc 1 [(0, 0x300000), (0, 0x100000)] [memWord 0] [] 60
      any (stError . ckStat) clks @? "error seen"
      stError (ckStat (last clks)) @? "and kept"
  ]
