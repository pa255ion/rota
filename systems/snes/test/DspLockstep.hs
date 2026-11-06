{-|
The Clash S-DSP ("Snes.Dsp", step 7.8) in lockstep with the spec's
("Snes.Spec.Dsp"), a sample at a time: both start from the same ARAM of
pseudo-random bytes (so the BRR blocks have every header: shifts, filters,
loops and ends), take the same register writes between samples (random,
with KON now and then, the echo on and off, FLG's bits) and the same
writes to ARAM, and must give the same left and right of every sample,
the same 128 registers after it, and the same ARAM at the end (the echo
writes there). The Clash DSP runs over models of its memories, one clock
at a time; no sample may take more than 700 clocks (the sound module's
budget, snes.md).

And a real program: the unnamed tech demo's music in the spec machine,
its sound module's trace (the SPC700's writes to ARAM and the DSP's
registers, each sample) replayed into the Clash DSP, every sample and
ARAM at the end compared.
-}
module Main (main) where

import Prelude

import Control.Monad (forM, forM_, unless, when)
import Control.Monad.ST (stToIO)
import Data.Bits
import qualified Data.ByteString as B
import Data.IORef
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MV
import Data.Word
import System.Directory (doesFileExist)
import System.Environment (lookupEnv)
import Test.Tasty
import Test.Tasty.HUnit

import qualified Clash.Prelude as C
import Snes.Cart (parseCart)
import Snes.Dsp
import Snes.Spec (newMachine, reset, runFrames, apuOf, apuTraceOn, apuTakeTrace, apuRam, ApuEvent (..))
import Snes.Spec.Dsp (newDsp, dspWrite, dspRead, dspSample)
import Snes.Spec.Gauss (gauss)

main :: IO ()
main = do
  full <- (== Just "1") <$> lookupEnv "EMU_FULL_TESTS"
  -- DSP_ONLY="16 18": only those seeds (at the full tier's length)
  only <- maybe [] (map read . words) <$> lookupEnv "DSP_ONLY"
  let seeds = if null only then [1 .. if full then 48 else 12] else only
      n = if full || not (null only) then 20000 else 3000
      frames = if full then 900 else 150
  defaultMain $ testGroup "snes-dsp"
    [ testGroup ("the Clash S-DSP in lockstep with the spec, " <> show n <> " samples each")
        [ testCase ("seed " <> show s) (lockstep s n) | s <- seeds ]
    , testCase "the FIR: a term of +32768 wraps in the first seven taps (as the spec's sum)" firWrap
    , testCase ("the tech demo's music, " <> show frames <> " frames, replayed into the Clash S-DSP") (music frames)
    ]

-- | The FIR's right channel from a history and coefficients where one of
-- the first seven terms is (-16384 * -128) >> 6 = +32768 (full tier, seed
-- 16, sample 6514: a 'resize' made it 0), against the spec's sum: the
-- first seven wrapped to 16 bits, then the eighth added and clamped.
firWrap :: Assertion
firWrap = do
  let hist = [15137, 7961, 2450, 6056, -16384, -8832, -401, 446] :: [Int]
      coefs = [91, 27, 249, 110, 128, 64, 113, 16] :: [Int]
      s8 v = if v >= 0x80 then v - 0x100 else v
      ts = [ (h * s8 k) `shiftR` 6 | (h, k) <- zip hist coefs ]
      w16 v = let x = v .&. 0xFFFF in if x >= 0x8000 then x - 0x10000 else x
      want = max (-0x8000) (min 0x7FFF (w16 (sum (take 7 ts)) + ts !! 7))
      regs0 = foldl (\r (j, v) -> C.replace (j * 16 + 15 :: Int) (fromIntegral v) r) (dRegs dspInit) (zip [0 ..] coefs)
      c0 = dspInit { dStep = EFir, dK = 8, dFirR = C.map fromIntegral (vecOf hist), dRegs = regs0 }
      run c n = if n == (0 :: Int) then c else run (fst (dspClock c dspIdleIn)) (n - 1)
  fromIntegral (dFirOutR (run c0 8)) @?= want
 where
  vecOf :: [Int] -> C.Vec 8 Int
  vecOf xs = C.map (\i -> xs !! fromEnum i) C.indicesI

-- | The Clash DSP over models of its memories (ARAM, the samples, the
-- Gaussian table): a clock with the start and write given.
data Runner = Runner
  { rClock :: DspIn -> IO DspOut
  , rState :: IO Dsp
  , rAram  :: MV.IOVector Word8
  }

newRunner :: U.Vector Word8 -> IO Runner
newRunner aram0 = do
  ramK <- U.thaw aram0
  smp <- MV.replicate 256 (0 :: Int)
  st <- newIORef dspInit
  inp <- newIORef dspIdleIn
  let clock i0 = do
        c <- readIORef st
        i <- readIORef inp
        let (c', o) = dspClock c (i { diStart = diStart i0, diWrite = diWrite i0 })
        writeIORef st c'
        -- the memories: this clock's reads (seen next clock), then the writes
        av <- MV.read ramK (fromIntegral (doAram o))
        sv <- MV.read smp (fromIntegral (doSmpRd o))
        forM_ (doAramW o) $ \v -> MV.write ramK (fromIntegral (doAram o)) (fromIntegral v)
        forM_ (doSmpW o) $ \(a, v) -> MV.write smp (fromIntegral a) (fromIntegral v)
        writeIORef inp DspIn { diStart = False, diWrite = Nothing, diAram = fromIntegral av
                             , diSmp = fromIntegral sv, diGauss = fromIntegral (gauss U.! fromIntegral (doGauss o)) }
        pure o
  pure (Runner clock (readIORef st) ramK)

-- | A register write (a clock while idle).
regWriteK :: Runner -> Int -> Word8 -> IO ()
regWriteK r a v = () <$ rClock r dspIdleIn { diWrite = Just (fromIntegral a, fromIntegral v) }

-- | A sample: its left and right, and the clocks it took.
sampleK :: Runner -> IO ((Int, Int), Int)
sampleK r = do
  _ <- rClock r dspIdleIn { diStart = True }
  let loop k = do
        o <- rClock r dspIdleIn
        case doSample o of
          Just (l, rr) -> pure ((fromIntegral l, fromIntegral rr), k)
          Nothing | k > 5000 -> assertFailure "the Clash DSP never finished its sample" >> fail ""
                  | otherwise -> loop (k + 1)
  loop (1 :: Int)

-- | The tech demo in the spec machine with its sound module traced, the
-- trace replayed into the Clash DSP.
music :: Int -> Assertion
music frames = do
  let path = "../../roms/snes/games/unnamed-tech-demo-v3.sfc"
      iplPath = "../../build/snes/ipl.bin"
  ok <- (&&) <$> doesFileExist path <*> doesFileExist iplPath
  unless ok $ assertFailure (path <> " or " <> iplPath <> " missing: run tools/fetch-roms.sh and make snes-asm")
  file <- B.readFile path
  ipl <- B.readFile iplPath
  c <- maybe (assertFailure "no header" >> fail "") pure (parseCart file)
  (events, ramEnd) <- stToIO $ do
    m <- newMachine ipl c file
    apuTraceOn (apuOf m)
    reset m
    runFrames m frames
    es <- apuTakeTrace (apuOf m)
    ram <- U.generateM 0x10000 (apuRam (apuOf m))
    pure (es, ram)
  r <- newRunner (U.replicate 0x10000 0)
  loud <- newIORef (0 :: Int)
  count <- newIORef (0 :: Int)
  forM_ events $ \e -> case e of
    EvAram _ a v -> MV.write (rAram r) a v
    EvDsp a v -> regWriteK r a v
    EvSample _ l rr -> do
      k <- readIORef count
      writeIORef count (k + 1)
      ((kl, kr), clocks) <- sampleK r
      when (clocks > 700) $ assertFailure ("sample " <> show k <> " took " <> show clocks <> " clocks")
      when ((l, rr) /= (0, 0)) $ modifyIORef' loud (+ 1)
      unless ((kl, kr) == (l, rr)) $
        assertFailure ("sample " <> show k <> ": " <> show (kl, kr) <> ", the spec " <> show (l, rr))
  ramK <- U.freeze (rAram r)
  let bad = [ a | a <- [0 .. 0xFFFF], ramEnd U.! a /= ramK U.! a ]
  unless (null bad) $ assertFailure (show (length bad) <> " ARAM bytes differ, the first at " <> show (take 4 bad))
  nLoud <- readIORef loud
  n <- readIORef count
  -- the music, not the silence before it
  (nLoud * 4 > n) @? ("only " <> show nLoud <> " of " <> show n <> " samples make a sound")

-- | A hash of the seed and an index.
hash :: Int -> Int -> Int
hash seed a =
  let h0 = (a * 0x9E3779B1 + seed * 0x85EBCA77 + 0x165667B1) .&. 0xFFFFFFFF
      h1 = ((h0 `xor` (h0 `shiftR` 15)) * 0x2C1B3C6D) .&. 0xFFFFFFFF
  in (h1 `xor` (h1 `shiftR` 12)) .&. 0xFFFFFFFF

-- | The register writes before sample k: a few, of the kinds that make
-- sound (the voices' registers, KON and KOFF, the echo's, FLG).
writesAt :: Int -> Int -> [(Int, Word8)]
writesAt seed k =
  let h = hash seed (k * 7919)
      count = if k == 0 then 120 else [0, 0, 0, 1, 1, 2, 3, 6] !! (h .&. 7)
  in [ w | j <- [1 .. count], let w = oneWrite (hash seed (k * 7919 + j)) ]
 where
  oneWrite r =
    let v = fromIntegral (r `shiftR` 8) :: Word8
        voice = (r `shiftR` 4) .&. 7
    in case r .&. 15 of
         0 -> (0x4C, v .&. fromIntegral (r `shiftR` 16))   -- KON, a few voices
         1 -> (0x5C, v .&. fromIntegral (r `shiftR` 16) .&. fromIntegral (r `shiftR` 24))
         2 -> (voice * 16 + 2, v)                          -- pitch
         3 -> (voice * 16 + 3, v .&. 0x3F)
         4 -> (voice * 16 + 4, v)                          -- SRCN
         5 -> (voice * 16 + 5, v)                          -- ADSR1
         6 -> (voice * 16 + 6, v)
         7 -> (voice * 16 + 7, v)                          -- GAIN
         8 -> (voice * 16 + [0, 1] !! ((r `shiftR` 20) .&. 1), v)
         9 -> ([0x0C, 0x1C, 0x2C, 0x3C, 0x0D, 0x7D] !! ((r `shiftR` 20) `mod` 6), v)
         10 -> ([0x2D, 0x3D, 0x4D, 0x5D, 0x6D] !! ((r `shiftR` 20) `mod` 5), v)
         11 -> (voice * 16 + 0xF, v)                       -- FIR
         12 -> (0x6C, v .&. (if (r `shiftR` 20) .&. 15 == 0 then 0xFF else 0x3F))  -- FLG, its mute and reset rarely
         13 -> (0x7C, v)                                   -- ENDX: cleared
         14 -> (voice * 16 + 8, v)                         -- ENVX, OUTX: the DSP writes them over
         _ -> (voice * 16 + 0xA + ((r `shiftR` 20) .&. 3), v)  -- the unused ones

lockstep :: Int -> Int -> Assertion
lockstep seed n = do
  let aram0 = U.generate 0x10000 (\a -> fromIntegral (hash seed (a + 0x5A5A0000)) :: Word8)
  -- the spec
  ramS <- U.thaw aram0
  dS <- stToIO newDsp
  -- the Clash DSP and its memories
  r <- newRunner aram0
  let ramK = rAram r
  maxClocks <- newIORef (0 :: Int)
  loud <- newIORef (0 :: Int)
  forM_ [0 .. n - 1] $ \k -> do
    let ws = writesAt seed k
    forM_ ws $ \(a, v) -> do
      stToIO (dspWrite dS a v)
      regWriteK r a v
    -- now and then the SPC700 writes ARAM (a BRR byte, an echo byte)
    let h = hash seed (k * 31 + 7)
    when (h .&. 3 == 0) $ do
      let a = (h `shiftR` 8) .&. 0xFFFF
          v = fromIntegral (h `shiftR` 24) :: Word8
      MV.write ramS a v
      MV.write ramK a v
    (l, rr) <- stToIO (dspSample dS (\a -> MV.read ramS a) (\a v -> MV.write ramS a v))
    ((kl, kr), clocks) <- sampleK r
    modifyIORef' maxClocks (max clocks)
    when ((l, rr) /= (0, 0)) $ modifyIORef' loud (+ 1)
    c <- rState r
    unless ((kl, kr) == (l, rr)) $
      assertFailure ("sample " <> show k <> ": " <> show (kl, kr) <> ", the spec " <> show (l, rr)
                     <> "\n  core: mix " <> show (dMixL c, dMixR c) <> " echo " <> show (dEchoL c, dEchoR c)
                     <> " fir " <> show (dFirOutL c, dFirOutR c) <> " histR " <> show (dFirR c)
                     <> "\n  regs MVOL " <> show (dspRegRead c 0x0C, dspRegRead c 0x1C) <> " EVOL " <> show (dspRegRead c 0x2C, dspRegRead c 0x3C)
                     <> " EFB " <> show (dspRegRead c 0x0D) <> " FLG " <> show (dspRegRead c 0x6C) <> " ESA " <> show (dspRegRead c 0x6D)
                     <> " EDL " <> show (dspRegRead c 0x7D) <> " EON " <> show (dspRegRead c 0x4D)
                     <> " FIR " <> show [ dspRegRead c (fromIntegral (j * 16 + 15)) | j <- [0 .. 7 :: Int] ]
                     <> " idx " <> show (dEchoIdx c, dEchoLen c))
    regsS <- forM [0 .. 127] $ \a -> stToIO (dspRead dS a)
    let regsK = [ fromIntegral (dspRegRead c (fromIntegral a)) | a <- [0 .. 127 :: Int] ]
    unless (regsS == regsK) $
      assertFailure ("sample " <> show k <> ": registers " <> show [ (a, x, y) | (a, x, y) <- zip3 [0 :: Int ..] regsK regsS, x /= y ] <> " (address, core, spec)")
  ramSf <- U.freeze ramS
  ramKf <- U.freeze ramK
  let bad = [ a | a <- [0 .. 0xFFFF], ramSf U.! a /= ramKf U.! a ]
  unless (null bad) $ assertFailure (show (length bad) <> " ARAM bytes differ, the first at " <> show (take 4 bad))
  mc <- readIORef maxClocks
  when (mc > 700) $ assertFailure ("a sample took " <> show mc <> " clocks, over the budget of 700")
  -- a test of sound, not of silence
  nLoud <- readIORef loud
  when (nLoud * 2 < n) $ assertFailure ("only " <> show nLoud <> " of " <> show n <> " samples make a sound")
