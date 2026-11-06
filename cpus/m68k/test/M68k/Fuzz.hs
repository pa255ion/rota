{-|
P.4f: what the SingleStepTests don't cover (interrupts, trace, STOP, the
bus error), by random programs run on the model and on the core side by
side, each on its own copy of one world: memory a pseudo-random function
of the address (vectors even, below the bus-error region) plus what was
written, an interrupt level changing in windows of steps, an acknowledge
that answers an autovector or a vector by the level, BERR on the top 64
KiB. After every step (an instruction, an exception, a stopped CPU's
turn) the two bus logs and the states must agree; a run ends at its
length or when the model stops (a double fault, or an encoding it calls
unimplemented). The runs must between them take each of the paths they
are for.

The clock wrapper ('m68kTick') too: the same transactions as 'cpuStep'
when DTACK comes late, each taking its clocks plus its wait states.
-}
module M68k.Fuzz
  ( fuzzTests
  ) where

import Prelude

import Control.Monad.Trans.State.Strict (State, evalState, gets, modify', runState)
import Data.Array.Unboxed (listArray)
import Data.Bits
import qualified Data.Map.Strict as M
import Data.Word
import Test.Tasty
import Test.Tasty.HUnit

import M68k.Bus (CpuIn (..), Kind (..), Req (..))
import M68k.Core (M68kT (..), begin, cpuStep, m68kDone, m68kTick)
import qualified M68k.Core as C3
import qualified M68k.Datapath as C2
import M68k.CoreRun
import M68k.Spec

-- | One bus event, as both runs log it.
data Ev
  = ERead Fc Size Word32 Word16
  | EWrite Fc Size Word32 Word16
  | ETas Fc Word32 Word8
  | EIdle Int
  | EIack Int
  deriving stock (Show, Eq)

data World = World
  { wMem  :: !(M.Map Word32 Word8)
  , wLog  :: [Ev]
  , wIpl  :: !Int
  , wLast :: !Word32
  , wSeed :: !Word64
  }

-- | SplitMix64's finaliser.
mix :: Word64 -> Word64
mix z0 =
  let z1 = (z0 `xor` (z0 `shiftR` 30)) * 0xbf58476d1ce4e5b9
      z2 = (z1 `xor` (z1 `shiftR` 27)) * 0x94d049bb133111eb
  in z2 `xor` (z2 `shiftR` 31)

rnd :: Word64 -> Word64 -> Word64
rnd seed k = mix (seed * 0x9E3779B97F4A7C15 + k)

-- | A byte not yet written: the vectors (below 0x400) even and below the
-- bus-error region.
bg :: Word64 -> Word32 -> Word8
bg seed a
  | a < 0x400 && a .&. 3 == 3 = v .&. 0xFE
  | a < 0x400 && a .&. 3 == 1 = v .&. 0x7F
  | otherwise = v
 where
  v = fromIntegral (rnd seed (fromIntegral a))

byte :: Word32 -> State World Word8
byte a = gets (\w -> M.findWithDefault (bg (wSeed w) a) a (wMem w))

poke :: Word32 -> Word8 -> State World ()
poke a v = modify' (\w -> w { wMem = M.insert a v (wMem w) })

record :: Ev -> State World ()
record e = modify' (\w -> w { wLog = e : wLog w })

touch :: Word32 -> State World ()
touch a = modify' (\w -> w { wLast = a })

busOps :: BusOps (State World)
busOps = BusOps
  { busRead = \fc sz a -> do
      v <- case sz of
        Byte -> fromIntegral <$> byte a
        Word -> (\h l -> fromIntegral h `shiftL` 8 .|. fromIntegral l) <$> byte a <*> byte (a + 1)
      record (ERead fc sz a v)
      touch a
      pure v
  , busWrite = \fc sz a v -> do
      case sz of
        Byte -> poke a (fromIntegral v)
        Word -> poke a (fromIntegral (v `shiftR` 8)) >> poke (a + 1) (fromIntegral v)
      record (EWrite fc sz a v)
      touch a
  , busTas = \fc a f -> do
      v <- byte a
      poke a (f v)
      record (ETas fc a (f v))
      pure v
  , busIdle = \n -> record (EIdle n)
  , busIpl = gets wIpl
  , busIack = \lvl -> do
      record (EIack lvl)
      s <- gets wSeed
      let h = rnd s (0x1AC + fromIntegral lvl)
      pure (if odd h then Nothing else Just (64 + fromIntegral (h `mod` 192)))
  , busBerr = gets (\w -> wLast w .&. 0xFFFFFF >= 0xFF0000)
  }

-- | Adjacent idle stretches as one.
merged :: [Ev] -> [Ev]
merged (EIdle a : EIdle b : r) = merged (EIdle (a + b) : r)
merged (EIdle 0 : r) = merged r
merged (x : r) = x : merged r
merged [] = []

-- | The interrupt level at a step: constant over windows of 37 steps,
-- level 0 four windows in five.
iplAt :: Word64 -> Int -> Int
iplAt seed i =
  let h = rnd seed (0x1F1 + fromIntegral (i `div` 37))
  in if h `mod` 5 == 0 then 1 + fromIntegral ((h `shiftR` 8) `mod` 7) else 0

-- | A program's start: random registers, supervisor mode, a random mask
-- and condition codes, T set one time in four; every eighth a STOP #$2000
-- at the PC.
startOf :: Word64 -> (Cpu, M.Map Word32 Word8)
startOf seed =
  let r k = fromIntegral (rnd seed (0x5000 + k)) :: Word32
      pc = r 20 .&. 0x7FFFFE
      stopProg = seed `mod` 8 == 0
      mem = if stopProg then M.fromList [(pc, 0x4E), (pc + 1, 0x72), (pc + 2, 0x20), (pc + 3, 0x00)] else M.empty
      at a = M.findWithDefault (bg seed a) a mem
      wordAt a = fromIntegral (at a) `shiftL` 8 .|. fromIntegral (at (a + 1)) :: Word16
      sr = 0x2000 .|. (fromIntegral (r 21) .&. 0x071F) .|. (if r 22 .&. 3 == 0 then 0x8000 else 0)
  in ( Cpu { cD = listArray (0, 7) [ r k | k <- [0 .. 7] ]
           , cA = listArray (0, 6) [ r k | k <- [8 .. 14] ]
           , cUsp = r 15 .&. 0x7FFFFE, cSsp = r 16 .&. 0x7FFFFE
           , cSr = sr, cPc = pc, cIr = wordAt pc, cIrc = wordAt (pc + 2)
           , cStopped = False, cIplSeen = 0 }
     , mem )

-- | What a run took: interrupts, traces, bus and address errors, stops,
-- steps.
data Stats = Stats { sInt, sTrace, sBerr, sAddr, sStop, sSteps :: !Int }
  deriving stock Show

instance Semigroup Stats where
  Stats a b c d e f <> Stats a' b' c' d' e' f' = Stats (a + a') (b + b') (c + c') (d + d') (e + e') (f + f')

instance Monoid Stats where
  mempty = Stats 0 0 0 0 0 0

-- | One program, @n@ steps at most: the first difference, or what it took.
runProg :: Int -> Word64 -> Either String Stats
runProg n seed = go 0 cpu0 w0 w0 mempty
 where
  (cpu0, mem0) = startOf seed
  w0 = World mem0 [] 0 0 seed
  go i cpu wa wb st
    | i >= n = Right st
    | otherwise =
        let ipl = iplAt seed i
            (rs, wa') = runState (step busOps cpu) wa { wIpl = ipl, wLog = [] }
            (rc, wb') = runState (coreRunner busOps cpu) wb { wIpl = ipl, wLog = [] }
            ls = merged (reverse (wLog wa'))
            lc = merged (reverse (wLog wb'))
            -- the exception's vector read (supervisor data, at 4n)
            vecRead v = any (\e -> case e of
                                     ERead 5 Word a _ -> a == v * 4
                                     _ -> False) ls
            st' = st <> Stats (length [ () | EIack _ <- ls ]) (fromEnum (vecRead 9)) (fromEnum (vecRead 2))
                              (fromEnum (vecRead 3)) 0 1
        in case rs of
             Left _ -> Right st'
             Right s'
               | ls /= lc -> Left (at i <> "IR " <> show (cIr cpu) <> ", SR " <> show (cSr cpu) <> "; bus: model " <> show ls <> "\ncore  " <> show lc)
               | otherwise -> case rc of
                   Left f -> Left (at i <> "the core halted (" <> show f <> "), the model didn't")
                   Right c'
                     | c' /= s' -> Left (at i <> "state: model " <> show s' <> "\ncore  " <> show c')
                     | otherwise -> go (i + 1) s' wa' wb' st' { sStop = sStop st' + fromEnum (cStopped s' && not (cStopped cpu)) }
  at i = "seed " <> show seed <> ", step " <> show i <> ": "

-- | A request made on the world: the core's input when it's over.
perform :: Int -> Req -> State World CpuIn
perform lvl rq = do
  let fc = fromIntegral (rqFc rq)
      sz = if rqByte rq then Byte else Word
      a = fromIntegral (rqAddr rq)
      ci d be au = CpuIn { ciData = d, ciBerr = be, ciIpl = fromIntegral lvl, ciAuto = au }
  case rqKind rq of
    KRead -> (\v be -> ci (fromIntegral v) be False) <$> busRead busOps fc sz a <*> busBerr busOps
    KWrite -> busWrite busOps fc sz a (fromIntegral (rqData rq)) >> (\be -> ci 0 be False) <$> busBerr busOps
    KTas -> (\v -> ci (fromIntegral v) False False) <$> busTas busOps fc a (.|. 0x80)
    KIdle -> ci 0 False False <$ busIdle busOps (fromIntegral (rqLen rq))
    KIack -> maybe (ci 0 False True) (\v -> ci (fromIntegral v) False False) <$> busIack busOps (fromIntegral (rqLen rq))

-- | @n@ transactions by 'cpuStep', and by the clock wrapper with DTACK
-- late by 0-2 clocks on each bus cycle: the requests, and each one's
-- clocks.
clockRun :: Int -> Word64 -> ([Req], [(Req, Int, Int)])
clockRun n seed = (ref, clk)
 where
  (cpu0, mem0) = startOf (seed * 8 + 1)
  w0 = World mem0 [] 0 0 seed
  c0 = begin 0 (toCore cpu0)
  ref = evalState (refLoop n c0) w0
  refLoop 0 _ = pure []
  refLoop k c = do
    ci <- perform 0 (rqOf c)
    (rqOf c :) <$> refLoop (k - 1 :: Int) (cpuStep ci c)
  clk = evalState (clkLoop n (M68kT c0 0) 0 0) w0
  clkLoop 0 _ _ _ = pure []
  clkLoop k t clocks idx = do
    let rq = rqOf (tCpu t)
        bus = rqKind rq /= KIdle
        waits = if bus then fromIntegral (rnd seed (0xC10C + fromIntegral idx) `mod` 3) else 0
        base = case rqKind rq of
          KTas -> 10
          KIdle -> max 1 (fromIntegral (rqLen rq))
          _ -> 4
        -- DTACK once the transaction's base clocks and its waits are past
        dtack = clocks + 1 >= base + waits
    if m68kDone dtack t
      then do
        ci <- perform 0 rq
        rest <- clkLoop (k - 1 :: Int) (m68kTick dtack ci t) 0 (idx + 1 :: Int)
        pure ((rq, clocks + 1, base + waits) : rest)
      else clkLoop k (m68kTick dtack (CpuIn 0 False 0 False) t) (clocks + 1) idx

fuzzTests :: Bool -> TestTree
fuzzTests full = testGroup "random programs (interrupts, trace, STOP, bus errors)"
  [ testCaseInfo (show progs <> " programs of up to " <> show steps <> " steps: the model and the core agree") $ do
      let results = map (runProg steps) [1 .. fromIntegral progs]
      case [ e | Left e <- results ] of
        e : _ -> assertFailure e >> pure ""
        [] -> do
          let st = mconcat [ s | Right s <- results ]
          assertBool ("not every path taken: " <> show st)
            (sInt st > 0 && sTrace st > 0 && sBerr st > 0 && sAddr st > 0 && sStop st > 0)
          pure (show (sSteps st) <> " steps: " <> show (sInt st) <> " interrupts, " <> show (sTrace st)
                <> " traces, " <> show (sBerr st) <> " bus errors, " <> show (sAddr st) <> " address errors, "
                <> show (sStop st) <> " STOPs")
  , testCaseInfo (show progs <> " programs chained (cpuStep: the hardware's boundary), interrupt level 0: the model's transactions") $ do
      let results = map (chainRun steps) [1 .. fromIntegral progs]
      case [ e | Left e <- results ] of
        e : _ -> assertFailure e >> pure ""
        [] -> pure (show (sum [ n | Right n <- results ]) <> " transactions")
  , testCase "the clock wrapper: cpuStep's transactions, each its clocks and wait states" $
      mapM_ (\s -> do
               let (ref, clk) = clockRun 3000 s
               map (\(rq, _, _) -> rq) clk @?= ref
               mapM_ (\(rq, got, want) -> assertEqual (show rq) want got) clk)
            [1 .. 4]
  ]
 where
  progs = if full then 400 else 40 :: Int
  steps = if full then 4000 else 1500

rqOf :: C3.Cpu -> Req
rqOf = C2.cReq . C3.cC

-- | A program run on the model step by step (interrupt level 0), its
-- transactions in one list; and on the core through 'cpuStep' alone (the
-- hardware's way: the boundary chained into the next instruction), its
-- transactions as many: the two lists must agree. A run ends where the
-- model stops (a double fault, an unimplemented encoding) or STOPs.
chainRun :: Int -> Word64 -> Either String Int
chainRun n seed =
  let (cpu0, mem0) = startOf seed
      w0 = World mem0 [] 0 0 seed
      specLog = evalState (specLoop n cpu0) w0
      specLoop 0 _ = pure []
      specLoop i cpu = do
        modify' (\w -> w { wLog = [] })
        r <- step busOps cpu
        l <- gets (merged . reverse . wLog)
        case r of
          Right c' | not (cStopped c') -> (l <>) <$> specLoop (i - 1 :: Int) c'
          Right _ -> pure l
          Left _ -> pure [] -- (a double fault: the model's last step left out)
      want = merged specLog
      cnt = length want
      coreLog = merged (evalState (coreLoop (cnt * 3 + 16) (begin 0 (toCore cpu0))) w0)
      coreLoop 0 _ = pure []
      coreLoop i c = do
        modify' (\w -> w { wLog = [] })
        ci <- perform 0 (rqOf c)
        l <- gets (reverse . wLog)
        (l <>) <$> coreLoop (i - 1 :: Int) (cpuStep ci c)
      got = take cnt (filter (/= EIdle 0) coreLog)
      got' = merged got
      -- (the model's last transaction may be idle clocks the core's next
      -- instruction runs on into: not compared)
      want' = if null want then want else init want
  in if take (length want') (merged coreLog) == want' then Right cnt
     else Left ("seed " <> show seed <> ": chained: first difference at " <> show (firstDiff want' (merged coreLog)) <> " of " <> show cnt
                <> "\nmodel " <> show (take 6 (drop (firstDiff want (merged coreLog)) want))
                <> "\ncore  " <> show (take 6 (drop (firstDiff want (merged coreLog)) (merged coreLog)))
                <> (if got' == got' then "" else ""))
 where
  firstDiff a b = length (takeWhile id (zipWith (==) a b))
