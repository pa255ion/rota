{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=0 #-}
{-# LANGUAGE FlexibleContexts #-}
-- | snes-savestate (PLAN.md step 7.14a; docs/systems/snes.md, "Save
-- states"): the direct path on the circuit itself ("Snes.Core" in Clash's
-- simulation), two cores side by side. A runs a ROM to a point at rest in
-- V-blank and freezes; B, frozen since reset, takes A's state: the chain a
-- byte a clock (A's reads go round its chain once, B shifts in A's top
-- byte in the same clock), then every memory in the state space (A's port
-- reads, B's writes the clock after). Both resume on the same clock, and
-- their outputs (pixels, samples, DDR requests, the beam) must agree on
-- every clock after: whatever the state misses would show there. The
-- PPU's line buffer and column ring are not moved (out of use in V-blank:
-- this checks it). Then both freeze again (at rest) and their whole states
-- are compared, the chain and every memory byte: a part the move missed
-- that the outputs haven't shown yet (ARAM, early in a game: the SPC700
-- still in its boot ROM) differs there.
module Main (main) where

import Prelude

import Clash.Sized.Internal.BitVector (unsafeToNatural)
import Clash.Prelude (BitVector, Signal, Unsigned, Vec, bundle, mealy, register, sampleN,
                      systemClockGen, unbundle, withClockResetEnable)
import qualified Clash.Prelude as C
import Clash.Explicit.Reset (resetGen)
import Control.Monad (unless, when)
import Debug.Trace (trace)
import qualified Data.List as L
import GHC.Generics (Generic)
import qualified Data.ByteString as B
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import System.Directory (setCurrentDirectory)
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import Test.Tasty
import Test.Tasty.HUnit

import Snes.Bus (CartCfg (..), CartMap (..))
import Snes.Cart
import Snes.Core
#ifdef SNES_STATES_CPU
import LoadCpu (loadTest)
#endif
#ifdef SNES_DSP1
import DspCircuit (dspCircuitTest)
#endif

romDir :: FilePath
romDir = "roms/snes"

main :: IO ()
main = do
  -- (the boot ROM's image, build/snes/ipl.mem, is read from the repository's root)
  setCurrentDirectory "../.."
  -- (the moves take about 50 minutes a ROM in Clash's simulation: the FULL
  -- tier's; the quick tier's check of the circuit is ci-savestate-snes)
  full <- (== Just "1") <$> lookupEnv "EMU_FULL_TESTS"
  defaultMain $ testGroup "snes-savestate" $
    [ testCase "the chain is whole bytes" $ chainBytes * 8 @?= fromIntegral (C.natToInteger @(C.BitSize CoreRegs)) ]
    <> if not full then [] else
#ifdef SNES_DSP1
    -- (the DSP-1 build, step 7.16d: our DSP-1 test cartridge on the circuit)
    [ testCase "dsp1.sfc on the circuit (the DSP-1 build): the backdrop green" dspCircuitTest ]
#elif defined(SNES_STATES_CPU)
    -- (the CPU build, step 7.15c: a load through the 65C816's routine)
    [ testCase "Mode7HDMA.sfc: a load through the CPU resumes as the original" (loadTest (romDir </> "krom" </> "Mode7HDMA.sfc") 5 100000)
    , testCase "tone.sfc: the same (its sound)" (loadTest ("build/snes/asm" </> "tone.sfc") 2 100000)
    , testCase "shmup_1942.sfc: the same" (loadTest (romDir </> "opensnes" </> "shmup_1942.sfc") 5 100000)
    ]
#else
    -- (tone.sfc frozen after 2 frames: its sound program in ARAM, the SPC700
    -- running it; at frame 0 it is still in its boot ROM, ARAM all zeros,
    -- and a move that left ARAM out passed)
    [ testCase "tone.sfc: a state moved to a fresh core resumes as the original" (moveTest ("build/snes/asm" </> "tone.sfc") 2 True False)
    -- (Mode7HDMA.sfc frozen after 5 frames: its picture on from frame 5,
    -- VRAM and CGRAM loaded; at frame 0 both were still empty)
    , testCase "Mode7HDMA.sfc: the same" (moveTest (romDir </> "krom" </> "Mode7HDMA.sfc") 5 False True)
    -- (shmup_1942.sfc frozen after 5 frames: its picture on from frame 5;
    -- no sound by then, tone.sfc's case has it)
    , testCase "shmup_1942.sfc: the same" (moveTest (romDir </> "opensnes" </> "shmup_1942.sfc") 5 False True)
    ]
#endif

-- | What a core shows each clock (compared between the two).
type Seen = (Maybe (Unsigned 8, Unsigned 8, Unsigned 15, Unsigned 15), Maybe (C.Signed 16, C.Signed 16), Maybe (Unsigned 23), Unsigned 9, Unsigned 11)

bitsOf :: Seen -> Integer
bitsOf = toInteger . unsafeToNatural . C.pack

nat :: C.KnownNat n => BitVector n -> Integer
nat = toInteger . unsafeToNatural

-- | The memories in the state space, one port op each (read a, write b).
memOps :: [Bool -> Maybe (BitVector 16) -> StPort]
memOps =
  [ \_ w -> PWram a (C.unpack . C.truncateB <$> w) | a <- [0 .. 0x1FFFF] ]
  <> [ \_ w -> PVram h a (C.unpack . C.truncateB <$> w) | h <- [False, True], a <- [0 .. 0x7FFF] ]
  <> [ \_ w -> PCgram a (C.unpack . C.truncateB <$> w) | a <- [0 .. 255] ]
  <> [ \_ w -> POam l a (C.unpack . C.truncateB <$> w) | l <- [0 .. 3], a <- [0 .. 127] ]
  <> [ \_ w -> PDma r ch (C.unpack . C.truncateB <$> w) | r <- [0 .. 15], ch <- [0 .. 7] ]
  <> [ \_ w -> PAram a (C.unpack . C.truncateB <$> w) | a <- [0 .. 0xFFFF] ]
  <> [ \_ w -> PSmp a (C.unpack <$> w) | a <- [0 .. 255] ]

-- | The controller's phases (counts in clocks).
data Phase = PRun | PChain Int | PMem Int | PResumed Int | PCChain Int | PCMem Int | PDone
  deriving stock (Show, Eq, Generic)
  deriving anyclass C.NFDataX

-- | Where the second freeze's states first differ.
data Where = MChain Int | MMem Int
  deriving stock (Show, Eq, Generic)
  deriving anyclass C.NFDataX

type Mismatch = (Where, BitVector 16, BitVector 16)

-- | The memories in 'memOps' order (for the counts and the messages).
regions :: [(String, Int)]
regions = [("WRAM", 0x20000), ("VRAM", 0x10000), ("CGRAM", 256), ("OAM", 512), ("DMA", 128), ("ARAM", 0x10000), ("samples", 256)]

-- | An op's memory (its index in 'regions') and its place in it.
regionOf :: Int -> (Int, Int)
regionOf = go 0 regions
 where
  go r ((_, n) : rest) i | i >= n, not (null rest) = go (r + 1) rest (i - n)
  go r _ i = (r, i)

-- | A state move on a ROM, frozen after the frames given; with sound (its
-- program in ARAM by then, playing), A's ARAM and samples must not be all
-- zero, and with a picture its VRAM and CGRAM (else the comparison would
-- see nothing: an ARAM left out once passed, the SPC700 still in its boot
-- ROM).
moveTest :: FilePath -> Int -> Bool -> Bool -> Assertion
moveTest path frames sound picture = do
  file <- B.readFile path
  cart <- maybe (assertFailure (path <> ": no header") >> fail "") pure (parseCart file)
  let rom = U.fromList (B.unpack (fst (stripCopier file)))
      romSize = U.length rom
      p = if romSize == 0 then 0 else C.bit (C.finiteBitSize romSize - 1 - C.countLeadingZeros romSize)
      cfg = CartCfg
        { ccMap = case cartMapping cart of LoRom -> CmLoRom; HiRom -> CmHiRom; ExHiRom -> CmExHiRom
        , ccRomP = fromIntegral p, ccRomQ = fromIntegral (romSize - p), ccSram = fromIntegral (cartSramSize cart) }
      opsV = V.fromList memOps
      nOps = V.length opsV
      -- the freeze: after the frames asked for, at rest, in V-blank
      minClocks = frames * 357368
      -- (Clash's simulation of the cores grows its heap by some KB a clock:
      -- the run is kept short, the first V-blank and 100,000 clocks after)
      runAfter = 100000 :: Int
      total = minClocks + 360000 + 2 * (chainBytes + nOps + 2) + runAfter + 20000
      -- the DDR's stand-in: a request answered the next clock
      ddr :: C.HiddenClockResetEnable C.System => Signal C.System (Maybe (Unsigned 23)) -> Signal C.System (Maybe (Vec 8 (Unsigned 8)))
      ddr req = register Nothing (fmap (\a -> C.unpack (C.pack (C.map (\k -> fromIntegral (rom U.! ((fromIntegral a `div` 8 * 8 + k) `mod` romSize))) (C.iterateI (+ 1) (0 :: Int)) :: Vec 8 (Unsigned 8)))) <$> req)
      circuit :: Signal C.System (Seen, Seen, (Phase, (Maybe Mismatch, Maybe (Int, Bool, Bool))))
      circuit = withClockResetEnable systemClockGen (resetGen @C.System) C.enableGen $
        let coreA = snesCore (pure True) (pure cfg) (pure (C.repeat 0)) (ddr (scDdrReq coreA)) (pure (C.RamRead 0)) stA (pure ovlIdle)
            coreB = snesCore (pure True) (pure cfg) (pure (C.repeat 0)) (ddr (scDdrReq coreB)) (pure (C.RamRead 0)) stB (pure ovlIdle)
            (stA, stB, ph) = unbundle (mealy ctl (PRun, 0 :: Int)
                                         (bundle (scRest coreA, scV coreA, scTop coreA, scPortRd coreA, scTop coreB, scPortRd coreB)))
            seen c = bundle (scPixel c, scSample c, scDdrReq c, scV c, scH c)
        in bundle (seen coreA, seen coreB, ph)
      opAt i = opsV V.! i
      ctl st inp = let (st', (a, b, ph)) = ctl' st inp in (st', (a, b, (ph, check st inp)))
      -- the second freeze's comparison: the chains' bytes as they come
      -- round, the memories' bytes the clock after each read
      check st inp = (checkM st inp, count st inp)
      -- each memory byte compared: its memory, nonzero in A, in B
      count (phase, _) (_, _, _, prdA, _, prdB) = case phase of
        PCMem i | i >= 1 && i <= nOps -> Just (fst (regionOf (i - 1)), nat prdA /= 0, nat prdB /= 0)
        _ -> Nothing
      checkM (phase, _) (_, _, topA, prdA, topB, prdB) = case phase of
        -- (by their bits, undefined ones 0: as the outputs')
        PCChain k | k >= 1 && k <= chainBytes && nat topA /= nat topB -> Just (MChain (k - 1), C.resize topA, C.resize topB)
        PCMem i | i >= 1 && i <= nOps && nat prdA /= nat prdB -> Just (MMem (i - 1), prdA, prdB)
        _ -> Nothing
      ctl' (phase, n) (rest, v, top, prd, topB, _) = case phase of
        PRun
          | n >= minClocks && rest && v >= 226 && v <= 250 ->
              trace ("froze at clock " <> show n) ((PChain 0, n + 1), (frozen Nothing Nothing, frozen Nothing Nothing, phase))
          | otherwise -> ((PRun, n + 1), (stIdle, frozen Nothing Nothing, phase))
        PChain k
          -- (the first frozen clock latches the bytes in flight: no shift
          -- in it, PChain 0 is that clock's)
          | k == 0 -> ((PChain 1, n + 1), (frozen Nothing Nothing, frozen Nothing Nothing, phase))
          | k <= chainBytes -> (if k == chainBytes then trace ("chain moved at clock " <> show n) else id) ((if k == chainBytes then PMem 0 else PChain (k + 1), n + 1), (frozen (Just top) Nothing, frozen (Just top) Nothing, phase))
          | otherwise -> ((PMem 0, n + 1), (frozen Nothing Nothing, frozen Nothing Nothing, phase))
        PMem i
          -- A reads op i; B writes op i - 1 with what A read last clock
          | i <= nOps ->
              let rd = if i < nOps then Just (opAt i False Nothing) else Nothing
                  wr = if i > 0 then Just (opAt (i - 1) True (Just prd)) else Nothing
              in (if i == nOps then trace ("memories moved at clock " <> show n) else id) ((if i == nOps then PResumed 0 else PMem (i + 1), n + 1), (frozen Nothing rd, frozen Nothing wr, phase))
          | otherwise -> ((PResumed 0, n + 1), (stIdle, stIdle, phase))
        PResumed k
          | k >= runAfter && rest -> trace ("froze again at clock " <> show n) ((PCChain 0, n + 1), (frozen Nothing Nothing, frozen Nothing Nothing, phase))
          | otherwise -> ((PResumed (k + 1), n + 1), (stIdle, stIdle, phase))
        -- (the first frozen clock the latch, as above; then both chains
        -- read round, each byte shifted back in)
        PCChain k
          | k == 0 -> ((PCChain 1, n + 1), (frozen Nothing Nothing, frozen Nothing Nothing, phase))
          | k < chainBytes -> ((PCChain (k + 1), n + 1), (frozen (Just top) Nothing, frozen (Just topB) Nothing, phase))
          | otherwise -> ((PCMem 0, n + 1), (frozen (Just top) Nothing, frozen (Just topB) Nothing, phase))
        PCMem i
          | i < nOps -> ((PCMem (i + 1), n + 1), (frozen Nothing (Just (opAt i False Nothing)), frozen Nothing (Just (opAt i False Nothing)), phase))
          | otherwise -> trace ("states compared at clock " <> show n) ((PDone, n + 1), (frozen Nothing Nothing, frozen Nothing Nothing, phase))
        PDone -> ((PDone, n + 1), (frozen Nothing Nothing, frozen Nothing Nothing, phase))
      frozen sh pt = StCtl True sh pt False
      -- one pass, nothing kept but counts and the first difference (the
      -- samples are a million clocks)
      -- (every sample's outputs forced, whatever the phase: an output not
      -- looked at holds the simulation's streams from its clock on, and
      -- the heap grew by some KB a clock until it ran out)
      step (acc, done, mis, cnt, nSnd) (a, b, (ph, (m, c))) =
        let done' = done || ph == PDone
            mis' = case mis of Nothing -> m; _ -> mis
            cnt' = case c of
              Just (r, za, zb) -> [ if j == r then (na + fromEnum za, nb + fromEnum zb) else (na, nb) | (j, (na, nb)) <- zip [0 ..] cnt ]
              Nothing -> cnt
            nSnd' = case (ph, a) of
              (PResumed _, (_, Just smp, _, _, _)) | nat (C.pack smp) /= 0 -> nSnd + 1
              _ -> nSnd
        in C.deepseqX a (C.deepseqX b (ph `seq` m `seq` done' `seq` mis' `seq` sum (map fst cnt') `seq` sum (map snd cnt') `seq` nSnd' `seq`
             (step' acc (a, b, ph), done', mis', cnt', nSnd')))
      step' (fr, nRes, nBad, first) (a, b, ph) = case ph of
        PRun -> (fr, nRes, nBad, first)
        PResumed k | k < runAfter ->
          -- (by their bits, undefined ones 0: a memory cell never written is
          -- undefined in Clash's simulation, 0 on the FPGA and in Verilator)
          let d = bitsOf a /= bitsOf b
          in fr `seq` nRes `seq` nBad `seq` (True, nRes + 1, if d then nBad + 1 else nBad, if d then maybe (Just (k, a, b)) Just first else first)
        _ -> (True, nRes, nBad, first)
      ((froze, resumed, nBadAll, firstBad), compared, mismatch, counts, sounding) =
        L.foldl' step ((False, 0 :: Int, 0 :: Int, Nothing), False, Nothing, map (const (0 :: Int, 0 :: Int)) regions, 0 :: Int) (sampleN total circuit)
  unless froze $ assertFailure "never at rest in V-blank: no freeze"
  unless (resumed > 1000) $ assertFailure ("resumed for only " <> show resumed <> " clocks")
  case firstBad of
    Nothing -> pure ()
    Just (k, a, b) -> assertFailure ("clock " <> show k <> " after the resume: A " <> show (bitsOf a) <> ", B " <> show (bitsOf b)
                                     <> " (" <> show nBadAll <> " clocks differ of " <> show resumed <> ")")
  unless compared $ assertFailure "the second freeze's comparison never ended"
  putStrLn ("  nonzero bytes (A, B): " <> L.intercalate ", " [ n <> " " <> show c | ((n, _), c) <- zip regions counts ]
            <> "; A's nonzero samples after the resume: " <> show sounding)
  let aramA = fst (counts !! 5)
  when sound $ do
    unless (aramA > 0) $ assertFailure "A's ARAM all zero at the second freeze: the comparison saw no sound program"
    unless (sounding > 0) $ assertFailure "A made no sound after the resume"
  when picture $ do
    unless (fst (counts !! 1) > 0) $ assertFailure "A's VRAM all zero at the second freeze: the comparison saw no picture"
    unless (fst (counts !! 2) > 0) $ assertFailure "A's CGRAM all zero at the second freeze: the comparison saw no colours"
  case mismatch of
    Nothing -> pure ()
    Just (w, a, b) -> assertFailure ("the states differ after the run on: " <> describe w <> ": A " <> show a <> ", B " <> show b)
  where
    describe (MChain k) = "the chain's byte " <> show k
    describe (MMem i) = "memory op " <> show i <> " (" <> opName i <> ")"
    opName i = let (r, k) = regionOf i in fst (regions !! r) <> " " <> show k

