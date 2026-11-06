-- | Layers 1 and 2 for the Clash Z80 ("Z80.Core", PLAN.md step 5.5): the
-- core on SingleStepTests (the first 100 cases per file in the quick
-- tier, all with FULL=1), and in lockstep with "Z80.Spec" on random
-- programs with random interrupts: every bus cycle (kind, address, data,
-- length), the registers and the T-states after every instruction. Two
-- deliberately broken cores must be caught.
module Main (main) where

import Prelude

import Clash.Prelude (Unsigned)
import Control.Exception (evaluate)
import Control.Monad (forM_, unless)
import Control.Monad.ST (ST, runST)
import Data.Bits
import Data.List (isPrefixOf, sort)
import Data.STRef
import qualified Data.Vector.Unboxed.Mutable as MV
import Data.Word
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath ((</>), takeBaseName, takeExtension)
import Test.Tasty
import Test.Tasty.HUnit

import Z80.Core
import Z80.CoreRun
import Z80.SingleStep
import qualified Z80.Spec as S

romRoot :: FilePath
romRoot = "../../roms/z80/v1"

main :: IO ()
main = do
  ok <- doesDirectoryExist romRoot
  unless ok (fail (romRoot <> " missing: run tools/fetch-roms.sh"))
  files <- map (romRoot </>) . sort . filter ((== ".json") . takeExtension) <$> listDirectory romRoot
  full <- fullTests
  let n = if full then Nothing else Just quickCases
      tier = maybe "all 1000" (\k -> "first " <> show k) n <> " cases per file"
      programs = if full then 400 else 60
      group name p = testGroup name (map (opcodeTest n) (filter (p . takeBaseName) files))
      isPre s f = s `isPrefixOf` f
  defaultMain $ testGroup "z80-lockstep"
    [ testGroup ("the core on SingleStepTests, " <> tier)
        [ group "unprefixed" (\f -> length f == 2)
        , group "CB" (isPre "cb ")
        , group "ED" (isPre "ed ")
        , group "DD" (\f -> isPre "dd " f && not (isPre "dd cb" f))
        , group "FD" (\f -> isPre "fd " f && not (isPre "fd cb" f))
        , group "DDCB" (isPre "dd cb")
        , group "FDCB" (isPre "fd cb")
        ]
    , testCaseInfo (show programs <> " random programs, 3000 instructions each, with interrupts: core = spec") $ do
        covs <- mapM (\seed -> case lockstepCov cpuStep seed 3000 of
                         Left e -> assertFailure ("seed " <> show seed <> ": " <> e) >> pure (Cov 0 0 0 0 0)
                         Right c -> pure c) [1 .. programs]
        let total = foldr addCov (Cov 0 0 0 0 0) covs
        -- the random programs must reach every kind of step
        assertBool ("coverage too thin: " <> show total) $
          covIm0 total > 0 && covIm1 total > 0 && covIm2 total > 0 && covNmi total > 0 && covHalt total > 0
        pure ("steps taken: " <> show total)
    , waitTest
    , testCase "a broken core is caught: X flag wrong after ADD A,r" $
        assertBool "not caught" (any (\s -> lockstep brokenFlag s 3000 /= Nothing) [1 .. 20 :: Int])
    , testCase "a broken core is caught: ADD HL,rr one T-state short" $
        assertBool "not caught" (any (\s -> lockstep brokenIdle s 3000 /= Nothing) [1 .. 20 :: Int])
    ]

opcodeTest :: Maybe Int -> FilePath -> TestTree
opcodeTest n f = testCase (takeBaseName f) $ do
  cases <- loadCasesUpTo n f
  let failures = [ (caseName tc, es) | tc <- cases, let es = runCaseWith coreRunner tc, not (null es) ]
  nFail <- evaluate (length failures)
  unless (nFail == 0) $
    assertFailure (show nFail <> " of " <> show (length cases) <> " cases fail; first:\n"
                   <> unlines [ name <> ": " <> unlines es | (name, es) <- take 2 failures ])

-- * Broken cores (the harness must see them)

brokenFlag :: CpuIn -> Cpu -> Cpu
brokenFlag ci c =
  let c' = cpuStep ci c
      isAdd = cPhase c == PExec TMain && cOp c .&. 0xF8 == 0x80
  in if isAdd && cPhase c' /= cPhase c then c' { cF = cF c' `xor` 0x08 } else c'

brokenIdle :: CpuIn -> Cpu -> Cpu
brokenIdle ci c =
  let c' = cpuStep ci c
      rq = cReq c'
  in if rqKind rq == KIdle && rqLen rq == 7 then c' { cReq = rq { rqLen = 6 } } else c'

-- * Lockstep

-- | xorshift64*, for reproducible random programs.
rand :: Word64 -> Word64
rand x0 = let x1 = x0 `xor` (x0 `shiftR` 12)
              x2 = x1 `xor` (x1 `shiftL` 25)
              x3 = x2 `xor` (x2 `shiftR` 27)
          in x3 * 2685821657736338717

randoms :: Int -> [Word64]
randoms seed = drop 1 (iterate rand (fromIntegral seed * 0x9E3779B97F4A7C15 + 1))

-- | How many of each step the spec took: /INT in IM 0, 1, 2, NMI, halted
-- fetches.
data Cov = Cov { covIm0, covIm1, covIm2, covNmi, covHalt :: Int }
  deriving stock (Show)

addCov :: Cov -> Cov -> Cov
addCov (Cov a b c d e) (Cov a' b' c' d' e') = Cov (a + a') (b + b') (c + c') (d + d') (e + e')

lockstep :: (CpuIn -> Cpu -> Cpu) -> Int -> Int -> Maybe String
lockstep stepFn seed steps = either Just (const Nothing) (lockstepCov stepFn seed steps)

-- | Run @steps@ instructions of a random program on the spec and on a
-- core (its step function given), compare after each; the first
-- difference, or what kinds of steps were run.
lockstepCov :: (CpuIn -> Cpu -> Cpu) -> Int -> Int -> Either String Cov
lockstepCov stepFn seed steps = runST $ do
  let rs = randoms seed
      memBytes = map (fromIntegral . (`shiftR` 56)) (take 65536 rs) :: [Word8]
      extra = drop 65536 rs
      regWords = take 16 extra
      lineWords = drop 16 extra
      word i = fromIntegral (regWords !! i) :: Word16
      byte i = fromIntegral (regWords !! i) :: Word8
      regs0 = S.zeroRegs
        { S.rA = byte 0, S.rF = byte 1, S.rB = byte 2, S.rC = byte 3, S.rD = byte 4
        , S.rE = byte 5, S.rH = byte 6, S.rL = byte 7
        , S.rAF' = word 8, S.rBC' = word 9, S.rDE' = word 10, S.rHL' = word 11
        , S.rIX = word 12, S.rIY = word 13, S.rSP = word 14, S.rPC = word 15 .&. 0x0FFF
        , S.rI = byte 8, S.rR = byte 9
        , S.rIFF1 = odd (byte 10), S.rIFF2 = odd (byte 10)
        , S.rIM = byte 11 `mod` 3 }
      -- the lines and the acknowledge's data for instruction n (none at 0)
      lw n = if n == 0 then 0 else lineWords !! (n `mod` 4096)
      intAt n = n > 0 && lw n .&. 0x7 == 0
      nmiAt n = n > 0 && lw n .&. 0x3F0 == 0x10
      vecAt n = fromIntegral (lw n `shiftR` 20) :: Word8
  memS <- MV.new 65536
  memC <- MV.new 65536
  forM_ (zip [0 ..] memBytes) $ \(i, b) -> MV.write memS i b >> MV.write memC i b
  stepRef <- newSTRef 0
  traceS <- newSTRef []
  traceC <- newSTRef []
  ioS <- newSTRef (0 :: Int)
  ioC <- newSTRef (0 :: Int)
  let mkBus mem tr io = S.Bus
        { S.busFetch = rdM 0
        , S.busRead = rdM 1
        , S.busWrite = \a v -> MV.write mem (fromIntegral a) v >> ev (2, a, v, 3)
        , S.busIn = \a -> do
            k <- readSTRef io
            writeSTRef io (k + 1)
            let v = fromIntegral ((fromIntegral a * 31 + k * 7) `mod` 256) :: Word8
            v <$ ev (3, a, v, 4)
        , S.busOut = \a v -> ev (4, a, v, 4)
        , S.busIdle = \a n -> ev (5, a, 0, n)
        , S.busIntAck = do
            n <- readSTRef stepRef
            let v = vecAt n
            v <$ ev (6, 0, v, 6)
        , S.busInt = intAt <$> readSTRef stepRef
        , S.busNmi = nmiAt <$> readSTRef stepRef
        }
       where
        ev e = modifySTRef' tr (e :)
        -- bus events: kind (0 fetch, 1 read, 2 write, 3 in, 4 out, 5 idle,
        -- 6 acknowledge), address, data, length
        rdM k a = do
          v <- MV.read mem (fromIntegral a)
          v <$ ev (k :: Int, a, v, if k == 0 then 4 else 3)
      busS = mkBus memS traceS ioS
      busC = mkBus memC traceC ioC
  cpu <- S.newCpu busS regs0
  cov <- newSTRef (Cov 0 0 0 0 0)
  let loop n core
        | n >= steps = Right <$> readSTRef cov
        | otherwise = do
            writeSTRef stepRef n
            t0 <- S.cpuTStates cpu
            imBefore <- S.rIM <$> S.getRegs cpu
            kind <- S.step cpu
            modifySTRef' cov $ \c -> case kind of
              S.Int -> case imBefore of
                0 -> c { covIm0 = covIm0 c + 1 }
                1 -> c { covIm1 = covIm1 c + 1 }
                _ -> c { covIm2 = covIm2 c + 1 }
              S.Nmi -> c { covNmi = covNmi c + 1 }
              S.Halted -> c { covHalt = covHalt c + 1 }
              _ -> c
            t1 <- S.cpuTStates cpu
            rS <- S.getRegs cpu
            evS <- reverse <$> readSTRef traceS
            writeSTRef traceS []
            -- the core decides what follows at the end, from the next lines
            (core', tC) <- coreInstrWith stepFn busC (intAt (n + 1)) (nmiAt (n + 1)) core
            evC <- reverse <$> readSTRef traceC
            writeSTRef traceC []
            let rC = fromCore core'
                diff
                  | evS /= evC = Just ("instruction " <> show n <> ": bus cycles\n  spec " <> show evS <> "\n  core " <> show evC)
                  | rS /= rC = Just ("instruction " <> show n <> ": registers\n  spec " <> show rS <> "\n  core " <> show rC)
                  | t1 - t0 /= tC = Just ("instruction " <> show n <> ": T-states spec " <> show (t1 - t0) <> ", core " <> show tC)
                  | otherwise = Nothing
            case diff of
              Just e -> pure (Left e)
              Nothing -> loop (n + 1) core'
  loop 0 (toCore regs0)

-- | 'coreInstr' with another step function.
coreInstrWith :: (CpuIn -> Cpu -> Cpu) -> S.Bus s -> Bool -> Bool -> Cpu -> ST s (Cpu, Int)
coreInstrWith stepFn bus int nmi = go 0 True
 where
  go t first c
    | not first && cNew c = pure (c, t)
    | otherwise = do
        let rq = cReq c
            a = fromIntegral (rqAddr rq) :: Word16
        v <- case rqKind rq of
          KFetch -> S.busFetch bus a
          KRead -> S.busRead bus a
          KWrite -> 0 <$ S.busWrite bus a (fromIntegral (rqData rq))
          KIn -> S.busIn bus a
          KOut -> 0 <$ S.busOut bus a (fromIntegral (rqData rq))
          KIdle -> 0 <$ S.busIdle bus a (fromIntegral (rqLen rq))
          KIntAck -> S.busIntAck bus
        let c' = stepFn (CpuIn (fromIntegral v :: Unsigned 8) int nmi) c
        go (t + fromIntegral (rqLen rq)) False c'

-- * WAIT

-- | The T-state wrapper: a short program run T-state by T-state, once
-- without WAIT and once with WAIT held for two T-states on every bus
-- cycle (not the internal ones). Same result; the second run longer by
-- exactly two T-states per bus cycle.
waitTest :: TestTree
waitTest = testCase "z80Tick: WAIT stretches bus cycles, nothing else changes" $ do
  let (r0, t0, n0) = runT 0
      (r2, t2, _) = runT 2
  r2 @?= r0
  t2 @?= t0 + 2 * n0
 where
  -- LD A,5; LD (8000h),A; LD B,A; INC (HL) with HL = 8000h; ADD HL,BC; IN A,(10h); HALT
  prog = [0x3E, 0x05, 0x32, 0x00, 0x80, 0x47, 0x21, 0x00, 0x80, 0x34, 0x09, 0xDB, 0x10, 0x76] :: [Word8]
  -- (registers at the HALT, T-states, bus cycles)
  runT :: Int -> (S.Regs, Int, Int)
  runT waits = runST $ do
    mem <- MV.replicate 65536 (0 :: Word8)
    forM_ (zip [0 ..] prog) $ \(i, b) -> MV.write mem i b
    let go z t held cycles
          | cHalted (zCpu z) = pure (fromCore (zCpu z), t, cycles)
          | otherwise = do
              let rq = cReq (zCpu z)
                  busCycle = rqKind rq /= KIdle
                  lastT = zT z + 1 >= rqLen rq
                  wait = busCycle && lastT && held < waits
              v <- if z80Done wait z
                then case rqKind rq of
                  KWrite -> 0 <$ MV.write mem (fromIntegral (rqAddr rq)) (fromIntegral (rqData rq))
                  KIn -> pure 0x42
                  KFetch -> MV.read mem (fromIntegral (rqAddr rq))
                  KRead -> MV.read mem (fromIntegral (rqAddr rq))
                  _ -> pure 0
                else pure 0
              let z' = z80Tick wait (CpuIn (fromIntegral v) False False) z
                  ended = z80Done wait z
              go z' (t + 1) (if ended then 0 else if wait then held + 1 else held)
                 (if ended && busCycle then cycles + 1 else cycles)
    go z80PowerOn (0 :: Int) 0 (0 :: Int)