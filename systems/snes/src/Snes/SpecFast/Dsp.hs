-- Side track W: the fast version's copy of "Snes.Spec.Dsp" (which stays
-- the reference, untouched): the same S-DSP and the same state record
-- (save states unchanged), 'dspSample' without lists (the voices mixed in
-- a loop, the FIR's taps in one) and over the sound RAM itself, not
-- through read and write functions. snes-specfast checks the fast machine,
-- this inside, against the reference model's sound.
{-|
The S-DSP (PLAN.md step 7.4; docs/systems/snes.md): eight voices of BRR
samples, the Gaussian interpolation, the ADSR and GAIN envelopes, noise,
pitch modulation, the echo with its 8-tap FIR, one stereo sample per call
of 'dspSample' (32 kHz, every 32 SPC700 cycles).

Sources: fullsnes ("SNES APU DSP BRR Samples", "BRR Pitch", "ADSR/Gain
Envelope", "Volume Registers", "Control Registers", "Echo Registers") for
the formulas, and the envelope rate counter's phase offsets as blargg
measured them (the 1040 and 536 of his snes_spc, values only).

This model works a sample at a time: each call does a whole sample's
work in this order: the rate counter, noise, KON/KOFF (every other
sample), the voices 0 to 7 (sample, envelope, output, pitch), the echo,
the mix. The chip spreads that over the sample's 32 clocks (fullsnes,
"SNES APU Low Level Timings"); where that matters (a register written in
the middle of a sample) this model is *to verify*.

Samples are kept as 15-bit values (fullsnes's form); a voice's output is
15 bits, the mix 16.
-}
module Snes.SpecFast.Dsp
  ( Dsp (..)
  , newDsp
  , dspRead
  , dspWrite
  , dspSample
  ) where

import Prelude

import Control.Monad (forM_, when)
import Control.Monad.ST (ST)
import Data.Bits
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MV
import Data.Word

import Snes.Spec.Gauss (gauss)

data Dsp s = Dsp
  { dRegs  :: !(MV.MVector s Int)  -- ^ the 128 registers
  , dVoice :: !(MV.MVector s Int)  -- ^ 8 voices × 'vSize' slots
  , dSt    :: !(MV.MVector s Int)
  , dFir   :: !(MV.MVector s Int)  -- ^ 8 × 2 echo samples, newest at the index
  }

-- Voice slots.
vBlock, vHeader, vCounter, vEnv, vHidden, vMode, vKonDelay, vOut, vSamples, vSize :: Int
vBlock = 0     -- the current BRR block's address
vHeader = 1    -- its header
vCounter = 2   -- the pitch counter (16 bits)
vEnv = 3       -- the envelope (11 bits)
vHidden = 4    -- the envelope before the rate counter allowed it
vMode = 5      -- 0 attack, 1 decay, 2 sustain, 3 release
vKonDelay = 6  -- samples left of the 5 after KON
vOut = 7       -- the last output (15 bits, signed)
vSamples = 8   -- 3 samples of the previous block, then the 16 of this one
vSize = 27

-- Global slots.
gCounter, gNoise, gKonPending, gEven, gEndx, gEchoIdx, gEchoLen, gFirIdx, gSize :: Int
gCounter = 0
gNoise = 1
gKonPending = 2
gEven = 3
gEndx = 4
gEchoIdx = 5
gEchoLen = 6
gFirIdx = 7
gSize = 8

-- Registers.
rVolL, rVolR, rPitchL, rPitchH, rSrcn, rAdsr1, rAdsr2, rGain, rEnvx, rOutx :: Int
rVolL = 0
rVolR = 1
rPitchL = 2
rPitchH = 3
rSrcn = 4
rAdsr1 = 5
rAdsr2 = 6
rGain = 7
rEnvx = 8
rOutx = 9

rMvolL, rMvolR, rEvolL, rEvolR, rKon, rKoff, rFlg, rEndx, rEfb, rPmon, rNon, rEon, rDir, rEsa, rEdl :: Int
rMvolL = 0x0C
rMvolR = 0x1C
rEvolL = 0x2C
rEvolR = 0x3C
rKon = 0x4C
rKoff = 0x5C
rFlg = 0x6C
rEndx = 0x7C
rEfb = 0x0D
rPmon = 0x2D
rNon = 0x3D
rEon = 0x4D
rDir = 0x5D
rEsa = 0x6D
rEdl = 0x7D

newDsp :: ST s (Dsp s)
newDsp = do
  regs <- MV.replicate 128 0
  MV.write regs rFlg 0xE0
  voice <- MV.replicate (8 * vSize) 0
  forM_ [0 .. 7] $ \v -> MV.write voice (v * vSize + vMode) 3
  st <- MV.replicate gSize 0
  MV.write st gNoise 0x4000
  fir <- MV.replicate 16 0
  pure (Dsp regs voice st fir)

reg :: Dsp s -> Int -> ST s Int
reg d = MV.unsafeRead (dRegs d)

vget :: Dsp s -> Int -> Int -> ST s Int
vget d v i = MV.unsafeRead (dVoice d) (v * vSize + i)

vset :: Dsp s -> Int -> Int -> Int -> ST s ()
vset d v i = MV.unsafeWrite (dVoice d) (v * vSize + i)

gget :: Dsp s -> Int -> ST s Int
gget d = MV.unsafeRead (dSt d)

gset :: Dsp s -> Int -> Int -> ST s ()
gset d = MV.unsafeWrite (dSt d)

-- | A register read (addresses 80h-FFh mirror 00h-7Fh).
dspRead :: Dsp s -> Int -> ST s Word8
dspRead d a0 = fromIntegral <$> case a .&. 0xF of
  0xC | a == rEndx -> gget d gEndx
  _ -> reg d a
 where
  a = a0 .&. 0x7F

-- | A register write (80h-FFh: read-only). KON is latched for the next
-- KON/KOFF poll; any write to ENDX clears it.
dspWrite :: Dsp s -> Int -> Word8 -> ST s ()
dspWrite d a v
  | a >= 0x80 = pure ()
  | a == rEndx = gset d gEndx 0
  | otherwise = do
      MV.write (dRegs d) a (fromIntegral v)
      when (a == rKon) $ gset d gKonPending (fromIntegral v)

signed8 :: Int -> Int
signed8 v = if v >= 0x80 then v - 0x100 else v

clamp16 :: Int -> Int
clamp16 = max (-0x8000) . min 0x7FFF

-- | 16-bit wrap-around.
wrap16 :: Int -> Int
wrap16 v = let w = v .&. 0xFFFF in if w >= 0x8000 then w - 0x10000 else w

-- | The envelope and noise rates: samples between steps (fullsnes), and
-- the phase of each in the shared counter (blargg).
rates, offsets :: U.Vector Int
rates = U.fromList [0, 2048, 1536, 1280, 1024, 768, 640, 512, 384, 320, 256, 192, 160, 128, 96, 80, 64, 48, 40, 32, 24, 20, 16, 12, 10, 8, 6, 5, 4, 3, 2, 1]
offsets = U.fromList [0, 0, 1040, 536, 0, 1040, 536, 0, 1040, 536, 0, 1040, 536, 0, 1040, 536, 0, 1040, 536, 0, 1040, 536, 0, 1040, 536, 0, 1040, 536, 0, 1040, 0, 0]

-- | Whether a rate's step falls on this sample.
rateHit :: Int -> Int -> Bool
rateHit counter r
  | r == 0 = False
  | otherwise = (counter + offsets U.! r) `mod` (rates U.! r) == 0

-- | The voices' mix: main left and right, echo left and right.
data Mix = Mix !Int !Int !Int !Int

-- | One output sample; reads and writes the sound RAM (ARAM) given.
-- Returns (left, right), 16-bit signed.
dspSample :: Dsp s -> MV.MVector s Word8 -> ST s (Int, Int)
dspSample d ram = do
  -- the shared rate counter
  c0 <- gget d gCounter
  let counter = if c0 == 0 then 0x77FF else c0 - 1
  gset d gCounter counter
  flg <- reg d rFlg
  -- noise
  when (rateHit counter (flg .&. 0x1F)) $ do
    n <- gget d gNoise
    gset d gNoise (((n `shiftR` 1) .&. 0x3FFF) .|. (((n `xor` (n `shiftR` 1)) .&. 1) `shiftL` 14))
  -- KON / KOFF, every other sample
  ev <- gget d gEven
  gset d gEven (1 - ev)
  when (ev == 0) $ do
    kon <- gget d gKonPending
    gset d gKonPending 0
    koff <- reg d rKoff
    forM_ [0 .. 7] $ \v -> do
      when (testBit koff v) $ vset d v vMode 3
      when (testBit kon v) $ keyOn v
  -- FLG bit 7: every voice released, its envelope 0
  when (testBit flg 7) $ forM_ [0 .. 7] $ \v -> vset d v vMode 3 >> vset d v vEnv 0 >> vset d v vHidden 0
  -- the voices
  pmon <- reg d rPmon
  non <- reg d rNon
  eon <- reg d rEon
  noise <- gget d gNoise
  -- (the fast version: the voices in order, their outputs mixed as they
  -- come, each sum clamped as the list's folds did; no list of them)
  let mixVoices !v !mL !mR !eL !eR
        | v > 7 = pure (Mix mL mR eL eR)
        | otherwise = do
            out <- voice v counter pmon non noise
            volL <- signed8 <$> reg d (v * 16 + rVolL)
            volR <- signed8 <$> reg d (v * 16 + rVolR)
            let !l = (out * volL) `shiftR` 6
                !r = (out * volR) `shiftR` 6
                !e = testBit eon v
            mixVoices (v + 1) (clamp16 (mL + l)) (clamp16 (mR + r))
              (if e then clamp16 (eL + l) else eL) (if e then clamp16 (eR + r) else eR)
  Mix mixL mixR echoL echoR <- mixVoices (0 :: Int) 0 0 0 0
  -- the echo: the oldest entry out of the ring, through the FIR
  esa <- reg d rEsa
  idx <- gget d gEchoIdx
  let addr = (esa * 0x100 + idx) .&. 0xFFFF
      rd16 a = (\lo hi -> wrap16 (fromIntegral lo .|. fromIntegral hi `shiftL` 8)) <$> ramR' (a .&. 0xFFFF) <*> ramR' ((a + 1) .&. 0xFFFF)
  inL <- (`shiftR` 1) <$> rd16 addr
  inR <- (`shiftR` 1) <$> rd16 (addr + 2)
  fi <- gget d gFirIdx
  MV.write (dFir d) (fi * 2) inL
  MV.write (dFir d) (fi * 2 + 1) inR
  gset d gFirIdx ((fi + 1) .&. 7)
  let tap ch k = do
        s <- MV.read (dFir d) (((fi - 7 + k) .&. 7) * 2 + ch)
        cf <- signed8 <$> reg d (k * 16 + 0xF)
        pure $! (s * cf) `shiftR` 6
      firSum ch = do
        -- FIR0 is the oldest; the first seven sums wrap, the last saturates
        -- (a loop, not lists of the coefficients and the terms)
        let go !k !acc
              | k > 6 = pure acc
              | otherwise = tap ch k >>= \t -> go (k + 1) (acc + t)
        partial <- wrap16 <$> go (0 :: Int) 0
        t7 <- tap ch 7
        pure $! clamp16 (partial + t7)
  firL <- firSum 0
  firR <- firSum 1
  mvolL <- signed8 <$> reg d rMvolL
  mvolR <- signed8 <$> reg d rMvolR
  evolL <- signed8 <$> reg d rEvolL
  evolR <- signed8 <$> reg d rEvolR
  efb <- signed8 <$> reg d rEfb
  let outL = clamp16 ((mixL * mvolL) `shiftR` 7 + (firL * evolL) `shiftR` 7)
      outR = clamp16 ((mixR * mvolR) `shiftR` 7 + (firR * evolR) `shiftR` 7)
      eL = clamp16 (echoL + (firL * efb) `shiftR` 7) .&. 0xFFFE
      eR = clamp16 (echoR + (firR * efb) `shiftR` 7) .&. 0xFFFE
  unless' (testBit flg 5) $ do
    let wr16 a v = ramW' (a .&. 0xFFFF) (fromIntegral (v .&. 0xFF)) >> ramW' ((a + 1) .&. 0xFFFF) (fromIntegral ((v `shiftR` 8) .&. 0xFF))
    wr16 addr eL
    wr16 (addr + 2) eR
  -- the ring: its length is read when the index is back at 0
  edl <- reg d rEdl
  len0 <- gget d gEchoLen
  let len = if idx == 0 then max 4 ((edl .&. 0xF) * 2048) else len0
      idx' = if idx + 4 >= len then 0 else idx + 4
  gset d gEchoLen len
  gset d gEchoIdx idx'
  pure (if testBit flg 6 then (0, 0) else (outL, outR))
 where
  ramR' i = MV.read ram i
  ramW' i v = MV.write ram i v
  unless' b act = if b then pure () else act
  keyOn v = do
    vset d v vKonDelay 5
    vset d v vEnv 0
    vset d v vHidden 0
    vset d v vMode 0
    vset d v vCounter 0
    gget d gEndx >>= gset d gEndx . (`clearBit` v)
  -- the BRR block at an address into the voice's samples (keeping the
  -- previous block's last three for the interpolation); its header's end
  -- flag sets ENDX, and code 1 (end without loop) releases the voice
  loadBlock v addr = do
    vset d v vBlock addr
    hdr <- fromIntegral <$> ramR' addr
    vset d v vHeader hdr
    forM_ [0 .. 2] $ \k -> vget d v (vSamples + 16 + k) >>= vset d v (vSamples + k)
    let shiftAmt = hdr `shiftR` 4
        filt = (hdr `shiftR` 2) .&. 3
    forM_ [0 .. 15] $ \k -> do
      b <- fromIntegral <$> ramR' ((addr + 1 + k `div` 2) .&. 0xFFFF)
      let nib0 = if even k then b `shiftR` 4 else b .&. 0xF
          nib = if nib0 >= 8 then nib0 - 16 else nib0
          s0 = if shiftAmt <= 12 then (nib `shiftL` shiftAmt) `shiftR` 1 else (nib `shiftR` 3) `shiftL` 11
      old <- vget d v (vSamples + 2 + k)
      older <- vget d v (vSamples + 1 + k)
      let new = case filt of
            0 -> s0
            1 -> s0 + old + ((-old) `shiftR` 4)
            2 -> s0 + old * 2 + ((-old * 3) `shiftR` 5) - older + (older `shiftR` 4)
            _ -> s0 + old * 2 + ((-old * 13) `shiftR` 6) - older + ((older * 3) `shiftR` 4)
          -- clamp to 16 bits, then keep 15 (the sign lost above 3FFFh)
          c = clamp16 new
          s15 = let w = (c `shiftL` 1) .&. 0xFFFF in (if w >= 0x8000 then w - 0x10000 else w) `shiftR` 1
      vset d v (vSamples + 3 + k) s15
    when (hdr .&. 1 /= 0) $ do
      gget d gEndx >>= gset d gEndx . (`setBit` v)
      when (hdr .&. 3 == 1) $ vset d v vMode 3 >> vset d v vEnv 0 >> vset d v vHidden 0
  dirEntry v k = do
    dir <- reg d rDir
    srcn <- reg d (v * 16 + rSrcn)
    let a = (dir * 0x100 + srcn * 4 + k) .&. 0xFFFF
    (\lo hi -> fromIntegral lo .|. fromIntegral hi `shiftL` 8) <$> ramR' a <*> ramR' ((a + 1) .&. 0xFFFF)
  voice v rc pmon non noise = do
    delay <- vget d v vKonDelay
    if delay > 0
      then do
        vset d v vKonDelay (delay - 1)
        when (delay == 1) $ do
          forM_ [0 .. 18] $ \k -> vset d v (vSamples + k) 0
          start <- dirEntry v 0
          loadBlock v start
          vset d v vCounter 0
        vset d v vOut 0
        MV.write (dRegs d) (v * 16 + rOutx) 0
        MV.write (dRegs d) (v * 16 + rEnvx) 0
        pure 0
      else do
        counter <- vget d v vCounter
        sample <- if testBit non v
          then pure (let n = noise .&. 0x7FFF in if n >= 0x4000 then n - 0x8000 else n)
          else do
            let i = (counter `shiftR` 4) .&. 0xFF
                p = counter `shiftR` 12
                s k = vget d v (vSamples + 3 + p - k)
            new <- s 0
            old <- s 1
            older <- s 2
            oldest <- s 3
            let g k = gauss U.! k
                a = (g (0xFF - i) * oldest) `shiftR` 10
                b = (g (0x1FF - i) * older) `shiftR` 10
                c = (g (0x100 + i) * old) `shiftR` 10
                out0 = wrap16 (a + b + c)
                out1 = clamp16 (out0 + (g i * new) `shiftR` 10)
            pure (out1 `shiftR` 1)
        -- the envelope steps by the shared rate counter (not the pitch's)
        envelope v rc
        env <- vget d v vEnv
        let out = (sample * env) `shiftR` 11
        vset d v vOut out
        MV.write (dRegs d) (v * 16 + rOutx) ((out `shiftR` 7) .&. 0xFF)
        MV.write (dRegs d) (v * 16 + rEnvx) (env `shiftR` 4)
        -- the pitch: PMON scales it by the previous voice's output
        pl <- reg d (v * 16 + rPitchL)
        ph <- reg d (v * 16 + rPitchH)
        let pitch0 = ((ph .&. 0x3F) `shiftL` 8) .|. pl
        pitch <- if v > 0 && testBit pmon v
          then (\prev -> pitch0 + ((pitch0 * (prev `shiftR` 4)) `shiftR` 10)) <$> vget d (v - 1) vOut
          else pure pitch0
        let counter' = counter + pitch
        if counter' >= 0x10000
          then do
            vset d v vCounter (counter' - 0x10000)
            hdr <- vget d v vHeader
            blk <- vget d v vBlock
            next <- if hdr .&. 1 /= 0 then dirEntry v 2 else pure ((blk + 9) .&. 0xFFFF)
            loadBlock v next
          else vset d v vCounter counter'
        pure out
  -- the envelope: worked out every sample, kept when its rate's step
  -- falls on the shared rate counter (blargg's arrangement)
  envelope v rc = do
    mode <- vget d v vMode
    env <- vget d v vEnv
    hidden <- vget d v vHidden
    adsr1 <- reg d (v * 16 + rAdsr1)
    adsr2 <- reg d (v * 16 + rAdsr2)
    gain <- reg d (v * 16 + rGain)
    let expDec e = e - (((e - 1) `shiftR` 8) + 1)
        envData = if testBit adsr1 7 then adsr2 else gain
        (env1, rate)
          | mode == 3 = (env - 8, 31)
          | testBit adsr1 7 = case mode of
              0 -> let r = (adsr1 .&. 0xF) * 2 + 1 in (env + (if r < 31 then 0x20 else 0x400), r)
              1 -> (expDec env, ((adsr1 `shiftR` 4) .&. 7) * 2 + 16)
              _ -> (expDec env, adsr2 .&. 0x1F)
          | not (testBit gain 7) = (gain * 0x10, 31)
          | otherwise = case (gain `shiftR` 5) .&. 3 of
              0 -> (env - 0x20, gain .&. 0x1F)
              1 -> (expDec env, gain .&. 0x1F)
              2 -> (env + 0x20, gain .&. 0x1F)
              _ -> (env + (if hidden < 0x600 then 0x20 else 0x08), gain .&. 0x1F)
        mode1 = if mode == 1 && (env1 `shiftR` 8) == (envData `shiftR` 5) then 2 else mode
        (env2, mode2) = if env1 < 0 || env1 > 0x7FF
          then (if env1 < 0 then 0 else 0x7FF, if mode1 == 0 then 1 else mode1)
          else (env1, mode1)
    vset d v vMode mode2
    -- the bent increase compares the value before clipping, kept in 11 bits
    vset d v vHidden (env1 .&. 0x7FF)
    when (mode == 3 || rateHit rc rate) $ vset d v vEnv env2
