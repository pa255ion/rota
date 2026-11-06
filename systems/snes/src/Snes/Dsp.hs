{-# OPTIONS_GHC -fconstraint-solver-iterations=0 #-}
{-# LANGUAGE StrictData #-}
{-|
The S-DSP in Clash (PLAN.md step 7.8): "Snes.Spec.Dsp" serially, a step
a clock, one voice at a time, as a state record and a pure clock function
('dspClock') with its memories outside on ports.

__A sample__ starts with 'diStart' (the sound module holds the SPC700
meanwhile, so the sample happens between two of its cycles, as in the
spec) and ends with 'doSample', about 100 to 300 clocks later: the shared
rate counter and the noise; then each voice (KON and KOFF on the polled
samples, FLG's reset, the KON delay or the Gaussian sum of four samples,
the envelope, the output, the pitch and a BRR block when the counter
passes a block, the volumes into the mix); then the echo (its four bytes
from ARAM, the FIR, the volumes, its four bytes back). Between samples
the SPC700 reads and writes the registers ('dspRegRead', 'diWrite').

__The voices__ rotate: the one in work is always the head of 'dVoices',
the one before it the last (PMON's), so a voice's state needs no
multiplexer.

__Memories:__ ARAM (one access a clock), the samples (each voice two
blocks of 16, 15 bits: the block being played and the one before, whose
last three the Gaussian sum may need), the Gaussian table (512 × 11
bits). Their data comes back on the next clock.

__The rates__ ('rateHit'): a step every m × 2^k samples (m 1, 3 or 5) of
the shared counter with its offset. By the Chinese remainder theorem that
is the counter's low k bits (plus the offset) all zero and its residue
mod m zero; the residues are counted along ('dR3', 'dR5'), so there is no
divider.
-}
module Snes.Dsp
  ( DspIn (..)
  , DspOut (..)
  , Voice (..)
  , Step (..)
  , Dsp (..)
  , dspInit
  , dspIdleIn
  , dspClock
  , dspRegRead
  ) where

import Clash.Prelude

-- * Interface

data DspIn = DspIn
  { diStart :: Bool                           -- ^ a sample (when idle)
  , diWrite :: Maybe (Unsigned 8, Unsigned 8)  -- ^ the SPC700's register write (when idle)
  , diAram  :: Unsigned 8                     -- ^ ARAM at last clock's address
  , diSmp   :: Signed 16                      -- ^ the samples at last clock's read address
  , diGauss :: Unsigned 11                    -- ^ the Gaussian table at last clock's address
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (NFDataX, BitPack)

dspIdleIn :: DspIn
dspIdleIn = DspIn False Nothing 0 0 0

data DspOut = DspOut
  { doAram   :: Unsigned 16
  , doAramW  :: Maybe (Unsigned 8)             -- ^ a write at 'doAram'
  , doSmpRd  :: Unsigned 8                     -- ^ voice × 32 + block × 16 + sample
  , doSmpW   :: Maybe (Unsigned 8, Signed 16)
  , doGauss  :: Unsigned 9
  , doSample :: Maybe (Signed 16, Signed 16)    -- ^ the sample's left and right
  , doBusy   :: Bool
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (NFDataX, BitPack)

-- * State

data Voice = Voice
  { vBlock    :: Unsigned 16
  , vHeader   :: Unsigned 8
  , vCounter  :: Unsigned 16   -- ^ the pitch counter
  , vEnv      :: Unsigned 11
  , vHidden   :: Unsigned 11   -- ^ the envelope before the rate allowed it
  , vMode     :: Unsigned 2    -- ^ attack, decay, sustain, release
  , vKonDelay :: Unsigned 3
  , vOut      :: Signed 16     -- ^ the last output (15 bits)
  , vHalf     :: Bool          -- ^ which of the voice's two blocks is the one played
  , vOld      :: Signed 16     -- ^ the last two decoded samples (the BRR filters')
  , vOlder    :: Signed 16
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (NFDataX, BitPack)

data Step
  = SIdle
  | SStart
  | VStart
  | VKonZero    -- ^ the block before cleared (3 samples)
  | VDir        -- ^ the directory entry (start or loop)
  | VBlock      -- ^ a BRR block: header, 8 bytes, 16 samples
  | VKonDone
  | VGauss
  | VEnv
  | VOut
  | VPitch
  | VVolL
  | VVolR
  | VNext
  | EIn
  | EFir
  | EOut
  | EWrite
  | EDone
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFDataX, BitPack)

data Dsp = Dsp
  { dRegs       :: Vec 128 (Unsigned 8)
  , dVoices     :: Vec 8 Voice
  , dCounter    :: Unsigned 15
  , dR3         :: Index 3      -- ^ (counter + 1040) mod 3
  , dR5         :: Index 5      -- ^ (counter + 536) mod 5
  , dNoise      :: Unsigned 15
  , dKonPending :: Unsigned 8
  , dEven       :: Bool
  , dEndx       :: Unsigned 8
  , dKon        :: Unsigned 8   -- ^ this sample's KON and KOFF (0 when not polled)
  , dKoff       :: Unsigned 8
  , dEchoIdx    :: Unsigned 16
  , dEchoLen    :: Unsigned 16
  , dFirL       :: Vec 8 (Signed 16)   -- ^ the echo's history, oldest first
  , dFirR       :: Vec 8 (Signed 16)
  , dStep       :: Step
  , dV          :: Index 8
  , dK          :: Unsigned 5
  , dKonPath    :: Bool         -- ^ the block load is KON's (else the loop's)
  , dSample     :: Signed 16    -- ^ the voice's sample
  , dAcc        :: Signed 16    -- ^ the Gaussian and FIR sums (wrapping)
  , dByte       :: Unsigned 8
  , dAddr       :: Unsigned 16
  , dMixL, dMixR, dEchoL, dEchoR :: Signed 16
  , dFirOutL, dFirOutR :: Signed 16
  , dOutL, dOutR :: Signed 20
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (NFDataX, BitPack)

voice0 :: Voice
voice0 = Voice 0 0 0 0 0 3 0 0 False 0 0

dspInit :: Dsp
dspInit = Dsp
  { dRegs = replace (0x6C :: Int) 0xE0 (repeat 0)
  , dVoices = repeat voice0
  , dCounter = 0, dR3 = 2, dR5 = 1, dNoise = 0x4000, dKonPending = 0, dEven = False, dEndx = 0
  , dKon = 0, dKoff = 0, dEchoIdx = 0, dEchoLen = 0
  , dFirL = repeat 0, dFirR = repeat 0
  , dStep = SIdle, dV = 0, dK = 0, dKonPath = False
  , dSample = 0, dAcc = 0, dByte = 0, dAddr = 0
  , dMixL = 0, dMixR = 0, dEchoL = 0, dEchoR = 0, dFirOutL = 0, dFirOutR = 0, dOutL = 0, dOutR = 0
  }

-- * Registers

reg :: Dsp -> Int -> Unsigned 8
reg c a = dRegs c !! a

-- | The voice's register r.
vreg :: Dsp -> Int -> Unsigned 8
vreg c r = dRegs c !! (fromEnum (dV c) * 16 + r)

-- | The SPC700's read (80h-FFh mirror 00h-7Fh; ENDX its own).
dspRegRead :: Dsp -> Unsigned 8 -> Unsigned 8
dspRegRead c a0 =
  let a = a0 .&. 0x7F
  in if a == 0x7C then dEndx c else dRegs c !! a

-- | The SPC700's write: 80h-FFh read-only; any write to ENDX clears it;
-- KON is latched for the next poll.
regWrite :: Dsp -> Unsigned 8 -> Unsigned 8 -> Dsp
regWrite c a v
  | a >= 0x80 = c
  | a == 0x7C = c { dEndx = 0 }
  | otherwise = c { dRegs = replace a v (dRegs c), dKonPending = if a == 0x4C then v else dKonPending c }

-- * Arithmetic

clamp16 :: KnownNat n => Signed n -> Signed 16
clamp16 x
  | x > 0x7FFF = 0x7FFF
  | x < -0x8000 = -0x8000
  | otherwise = resize x

-- | 16-bit wrap-around: the low 16 bits ('resize' of a 'Signed' would
-- keep the sign bit instead: +32768 became 0).
wrap16 :: KnownNat n => Signed n -> Signed 16
wrap16 x = unpack (resize (pack x))

-- | Whether rate r's step falls on this sample of the shared counter:
-- every m × 2^k samples, at the offset 0, 1040 or 536 (m 1, 3, 5).
rateHit :: Unsigned 15 -> Index 3 -> Index 5 -> Unsigned 5 -> Bool
rateHit counter r3 r5 r =
  let (m, k) = rateMK r
      zeroBelow :: Unsigned 15 -> Bool
      zeroBelow x = (x .&. ((1 `shiftL` fromEnum k) - 1)) == 0
  in case m of
       0 -> False
       1 -> zeroBelow counter
       3 -> zeroBelow (counter + 1040) && r3 == 0
       _ -> zeroBelow (counter + 536) && r5 == 0

-- | Rate r's period as m × 2^k (m 0: never).
rateMK :: Unsigned 5 -> (Unsigned 3, Unsigned 4)
rateMK r = case r of
  0 -> (0, 0)
  30 -> (1, 1)
  31 -> (1, 0)
  _ -> let j = (r - 1) `div` 3
           t = (r - 1) `mod` 3
       in case t of
            0 -> (1, resize (11 - j))
            1 -> (3, resize (9 - j))
            _ -> (5, resize (8 - j))

lfsr :: Unsigned 15 -> Unsigned 15
lfsr n = (n `shiftR` 1) .|. ((if testBit n 0 /= testBit n 1 then 1 else 0) `shiftL` 14)

-- | The envelope worked out (every sample), kept when its rate's step
-- falls ('Snes.Spec.Dsp''s, blargg's arrangement).
envelope :: Unsigned 8 -> Unsigned 8 -> Unsigned 8 -> (Unsigned 5 -> Bool) -> Voice -> Voice
envelope adsr1 adsr2 gain hit vc =
  let mode = vMode vc
      env = fromIntegral (vEnv vc) :: Signed 13
      expDec e = e - (((e - 1) `shiftR` 8) + 1)
      envData = if testBit adsr1 7 then adsr2 else gain
      g5 = resize (gain .&. 0x1F) :: Unsigned 5
      (env1, rate)
        | mode == 3 = (env - 8, 31)
        | testBit adsr1 7 = case mode of
            0 -> let r = resize (adsr1 .&. 0xF) * 2 + 1 :: Unsigned 5
                 in (env + (if r < 31 then 0x20 else 0x400), r)
            1 -> (expDec env, resize ((adsr1 `shiftR` 4) .&. 7) * 2 + 16)
            _ -> (expDec env, resize (adsr2 .&. 0x1F))
        | not (testBit gain 7) = (fromIntegral gain * 0x10, 31)
        | otherwise = case (gain `shiftR` 5) .&. 3 of
            0 -> (env - 0x20, g5)
            1 -> (expDec env, g5)
            2 -> (env + 0x20, g5)
            _ -> (env + (if vHidden vc < 0x600 then 0x20 else 0x08), g5)
      mode1 = if mode == 1 && (env1 `shiftR` 8) == fromIntegral (envData `shiftR` 5) then 2 else mode
      (env2, mode2)
        | env1 < 0 = (0, if mode1 == 0 then 1 else mode1)
        | env1 > 0x7FF = (0x7FF, if mode1 == 0 then 1 else mode1)
        | otherwise = (env1, mode1)
  in vc { vMode = mode2
        , vHidden = fromIntegral (env1 .&. 0x7FF)
        , vEnv = if mode == 3 || hit rate then fromIntegral env2 else vEnv vc }

-- | A BRR sample: the nibble, shift and filter, the last two samples; 15
-- bits (clamped to 16, the sign lost above 3FFFh).
brr :: Unsigned 8 -> Unsigned 4 -> Signed 16 -> Signed 16 -> Signed 16
brr hdr nib0 old0 older0 =
  let shiftAmt = hdr `shiftR` 4
      filt = (hdr `shiftR` 2) .&. 3
      nib = resize (bitCoerce nib0 :: Signed 4) :: Signed 22
      s0 = if shiftAmt <= 12 then (nib `shiftL` fromEnum shiftAmt) `shiftR` 1 else (nib `shiftR` 3) `shiftL` 11
      old = resize old0 :: Signed 22
      older = resize older0 :: Signed 22
      new = case filt of
        0 -> s0
        1 -> s0 + old + ((negate old) `shiftR` 4)
        2 -> s0 + old * 2 + ((negate old * 3) `shiftR` 5) - older + (older `shiftR` 4)
        _ -> s0 + old * 2 + ((negate old * 13) `shiftR` 6) - older + ((older * 3) `shiftR` 4)
      w = resize (clamp16 new `shiftL` 1) :: Signed 16
  in w `shiftR` 1

-- * The clock

-- | The samples' address of voice v's sample j (-3 to 15) of the block
-- played (the one before for j < 0).
smpAddr :: Index 8 -> Bool -> Signed 6 -> Unsigned 8
smpAddr v half j =
  let (h, pos) = if j >= 0 then (half, j) else (not half, j + 16)
  in (fromIntegral v `shiftL` 5) .|. (if h then 16 else 0) .|. resize (bitCoerce pos :: Unsigned 6)

noOut :: DspOut
noOut = DspOut 0 Nothing 0 Nothing 0 Nothing True

dspClock :: Dsp -> DspIn -> (Dsp, DspOut)
dspClock c i = case dStep c of
  SIdle ->
    let c1 = maybe c (uncurry (regWrite c)) (diWrite i)
    in (if diStart i then c1 { dStep = SStart } else c1, noOut { doBusy = diStart i })
  SStart ->
    let counter' = if dCounter c == 0 then 0x77FF else dCounter c - 1
        r3' = if dR3 c == 0 then 2 else dR3 c - 1
        r5' = if dR5 c == 0 then 4 else dR5 c - 1
        flg = reg c 0x6C
        noise' = if rateHit counter' r3' r5' (resize flg) then lfsr (dNoise c) else dNoise c
        poll = not (dEven c)
    in go c { dCounter = counter', dR3 = r3', dR5 = r5', dNoise = noise', dEven = poll
            , dKon = if poll then dKonPending c else 0
            , dKonPending = if poll then 0 else dKonPending c
            , dKoff = if poll then reg c 0x5C else 0
            , dMixL = 0, dMixR = 0, dEchoL = 0, dEchoR = 0, dV = 0, dStep = VStart } noOut
  VStart ->
    let v = dV c
        vc0 = head (dVoices c)
        flg = reg c 0x6C
        vc1 = if testBit (dKoff c) (fromEnum v) then vc0 { vMode = 3 } else vc0
        kon = testBit (dKon c) (fromEnum v)
        vc2 = if kon then vc1 { vKonDelay = 5, vEnv = 0, vHidden = 0, vMode = 0, vCounter = 0 } else vc1
        vc3 = if testBit flg 7 then vc2 { vMode = 3, vEnv = 0, vHidden = 0 } else vc2
        endx = if kon then clearBit (dEndx c) (fromEnum v) else dEndx c
        c1 = c { dEndx = endx }
        dly = vKonDelay vc3
        noiseVoice = testBit (reg c 0x3D) (fromEnum v)
        noiseSample = resize (bitCoerce (dNoise c) :: Signed 15)
    in if dly > 0
         then let vc4 = vc3 { vKonDelay = dly - 1 }
              in go (setHead c1 vc4) { dStep = if dly == 1 then VKonZero else VKonDone, dK = 0, dKonPath = True } noOut
         else if noiseVoice
           then go (setHead c1 vc3) { dStep = VEnv, dSample = noiseSample } noOut
           else go (setHead c1 vc3) { dStep = VGauss, dK = 0 } noOut
  VKonZero ->
    let vc = head (dVoices c)
        w = Just (smpAddr (dV c) (vHalf vc) (13 + bitCoerce (resize (dK c) :: Unsigned 6)), 0)
        c1 = setHead c vc { vOld = 0, vOlder = 0 }
    in if dK c == 2 then (c1 { dStep = VDir, dK = 0 }, noOut { doSmpW = w })
       else (c1 { dK = dK c + 1 }, noOut { doSmpW = w })
  VDir ->
    let base = (resize (reg c 0x5D) `shiftL` 8) + resize (vreg c 4) * 4 + (if dKonPath c then 0 else 2) :: Unsigned 16
    in case dK c of
         0 -> (c { dK = 1 }, noOut { doAram = base })
         1 -> (c { dK = 2, dByte = diAram i }, noOut { doAram = base + 1 })
         _ -> go c { dStep = VBlock, dK = 0, dAddr = (resize (diAram i) `shiftL` 8) .|. resize (dByte c) } noOut
  VBlock ->
    let vc = head (dVoices c)
        a = dAddr c
        k = dK c
        smp = resize ((k - 2) `shiftR` 1) :: Unsigned 3   -- the byte
        hiNib = not (testBit k 0)
        brrByte = if hiNib then diAram i else dByte c
        nib = if hiNib then resize (brrByte `shiftR` 4) else resize brrByte :: Unsigned 4
        s = brr (vHeader vc) nib (vOld vc) (vOlder vc)
        pos = resize (k - 2) :: Unsigned 4
        wr = Just ((fromIntegral (dV c) `shiftL` 5) .|. (if vHalf vc then 0 else 16) .|. resize pos, s)
        vcD = vc { vOld = s, vOlder = vOld vc }
        hdr = vHeader vc
        endFlags v' =
          let released = if hdr .&. 3 == 1 then v' { vMode = 3, vEnv = 0, vHidden = 0 } else v'
          in if testBit hdr 0 then released else v'
        after = if dKonPath c then VKonDone else VVolL
    in case k of
         0 -> (setHead c vc { vBlock = a }, noOut { doAram = a }) `withK` 1
         1 -> (setHead c vc { vHeader = diAram i }, noOut { doAram = a + 1 }) `withK` 2
         17 ->
           let vcE = endFlags vcD { vHalf = not (vHalf vc) }
               endx = if testBit hdr 0 then setBit (dEndx c) (fromEnum (dV c)) else dEndx c
           in (setHead c vcE) { dStep = after, dK = 0, dEndx = endx } `outWith` noOut { doSmpW = wr }
         _ ->
           ( (setHead c vcD) { dK = k + 1, dByte = brrByte }
           , noOut { doSmpW = wr, doAram = a + 2 + resize smp } )
  VKonDone ->
    let vc = head (dVoices c)
        vcK = vc { vOut = 0 }
        c1 = setVReg (setVReg (setHead c vcK) 8 0) 9 0
    in go c1 { dStep = VNext } noOut
  VGauss ->
    let vc = head (dVoices c)
        cnt = vCounter vc
        ix = resize (cnt `shiftR` 4) :: Unsigned 8
        p = bitCoerce (resize (cnt `shiftR` 12) :: Unsigned 6) :: Signed 6
        k = dK c
        -- issue: oldest first (p - 3 with FFh - i), newest last (p with i)
        req :: Unsigned 5 -> (Unsigned 8, Unsigned 9)
        req n = case n of
          0 -> (smpAddr (dV c) (vHalf vc) (p - 3), 0xFF - resize ix)
          1 -> (smpAddr (dV c) (vHalf vc) (p - 2), 0x1FF - resize ix)
          2 -> (smpAddr (dV c) (vHalf vc) (p - 1), 0x100 + resize ix)
          _ -> (smpAddr (dV c) (vHalf vc) p, resize ix)
        term = resize (prod `shiftR` 10) :: Signed 20
        acc' = dAcc c + wrap16 term
        (sa, ga) = req k
    in case k of
         0 -> (c { dK = 1, dAcc = 0 }, noOut { doSmpRd = sa, doGauss = ga })
         4 -> let out1 = clamp16 (resize (dAcc c) + term)
              in go c { dStep = VEnv, dSample = out1 `shiftR` 1 } noOut
         _ -> (c { dK = k + 1, dAcc = acc' }, noOut { doSmpRd = sa, doGauss = ga })
  VEnv ->
    let vc = head (dVoices c)
        hit = rateHit (dCounter c) (dR3 c) (dR5 c)
    in go (setHead c (envelope (vreg c 5) (vreg c 6) (vreg c 7) hit vc)) { dStep = VOut } noOut
  VOut ->
    let vc = head (dVoices c)
        out = resize (prod `shiftR` 11) :: Signed 16
        c1 = setVReg (setVReg (setHead c vc { vOut = out }) 9 (resize (bitCoerce (out `shiftR` 7) :: Unsigned 16)))
                     8 (resize (vEnv vc `shiftR` 4))
    in go c1 { dStep = VPitch } noOut
  VPitch ->
    let vc = head (dVoices c)
        pitch0 = (resize (vreg c 3 .&. 0x3F) `shiftL` 8) .|. resize (vreg c 2) :: Unsigned 17
        pmon = dV c > 0 && testBit (reg c 0x2D) (fromEnum (dV c))
        pitch = if pmon then pitch0 + bitCoerce (resize (prod `shiftR` 10) :: Signed 17) else pitch0
        cnt' = resize (vCounter vc) + pitch :: Unsigned 17
        hdr = vHeader vc
    in if testBit cnt' 16
         then let c1 = setHead c vc { vCounter = resize cnt' }
              in if testBit hdr 0
                   then go c1 { dStep = VDir, dK = 0, dKonPath = False } noOut
                   else go c1 { dStep = VBlock, dK = 0, dKonPath = False, dAddr = vBlock vc + 9 } noOut
         else go (setHead c vc { vCounter = resize cnt' }) { dStep = VVolL } noOut
  VVolL ->
    let t = resize (prod `shiftR` 6) :: Signed 20
        eon = testBit (reg c 0x4D) (fromEnum (dV c))
    in go c { dStep = VVolR, dMixL = clamp16 (resize (dMixL c) + t)
            , dEchoL = if eon then clamp16 (resize (dEchoL c) + t) else dEchoL c } noOut
  VVolR ->
    let t = resize (prod `shiftR` 6) :: Signed 20
        eon = testBit (reg c 0x4D) (fromEnum (dV c))
    in go c { dStep = VNext, dMixR = clamp16 (resize (dMixR c) + t)
            , dEchoR = if eon then clamp16 (resize (dEchoR c) + t) else dEchoR c } noOut
  VNext ->
    let c1 = c { dVoices = rotateLeft (dVoices c) (1 :: Int) }
    in if dV c == maxBound then go c1 { dStep = EIn, dK = 0 } noOut
       else go c1 { dStep = VStart, dV = dV c + 1 } noOut
  EIn ->
    let a = echoAddr c
        sample16 = bitCoerce ((resize (diAram i) `shiftL` 8) .|. resize (dByte c) :: Unsigned 16) :: Signed 16
        inS = sample16 `shiftR` 1
    in case dK c of
         0 -> (c { dK = 1 }, noOut { doAram = a })
         1 -> (c { dK = 2, dByte = diAram i }, noOut { doAram = a + 1 })
         2 -> (c { dK = 3, dFirL = dFirL c <<+ inS }, noOut { doAram = a + 2 })
         3 -> (c { dK = 4, dByte = diAram i }, noOut { doAram = a + 3 })
         _ -> go c { dStep = EFir, dK = 0, dAcc = 0, dFirR = dFirR c <<+ inS } noOut
  EFir ->
    let k = dK c
        t = resize (prod `shiftR` 6) :: Signed 20
        lastTap = (k .&. 7) == 7
        sum' = clamp16 (resize (dAcc c) + t)
        c1 | not lastTap = c { dAcc = dAcc c + wrap16 t }
           | k < 8 = c { dAcc = 0, dFirOutL = sum' }
           | otherwise = c { dAcc = 0, dFirOutR = sum' }
    in if k == 15 then go c1 { dStep = EOut, dK = 0 } noOut else go c1 { dK = k + 1 } noOut
  EOut ->
    let t = resize (prod `shiftR` 7) :: Signed 20
        k = dK c
        c1 = case k of
          0 -> c { dOutL = t }
          1 -> c { dOutL = resize (clamp16 (dOutL c + t)) }
          2 -> c { dOutR = t }
          3 -> c { dOutR = resize (clamp16 (dOutR c + t)) }
          4 -> c { dEchoL = clamp16 (resize (dEchoL c) + t) .&. complement 1 }
          _ -> c { dEchoR = clamp16 (resize (dEchoR c) + t) .&. complement 1 }
    in if k == 5 then go c1 { dStep = EWrite, dK = 0 } noOut else go c1 { dK = k + 1 } noOut
  EWrite ->
    let a = echoAddr c
        k = dK c
        on = not (testBit (reg c 0x6C) 5)
        el = bitCoerce (dEchoL c) :: Unsigned 16
        er = bitCoerce (dEchoR c) :: Unsigned 16
        (wa, wv) = case k of
          0 -> (a, resize el)
          1 -> (a + 1, resize (el `shiftR` 8))
          2 -> (a + 2, resize er)
          _ -> (a + 3, resize (er `shiftR` 8))
        o = noOut { doAram = wa, doAramW = if on then Just wv else Nothing }
    in if k == 3 then (c { dStep = EDone, dK = 0 }, o) else (c { dK = k + 1 }, o)
  EDone ->
    let idx = dEchoIdx c
        len = if idx == 0 then max 4 (resize (reg c 0x7D .&. 0xF) * 2048) else dEchoLen c
        idx' = if idx + 4 >= len then 0 else idx + 4
        flg = reg c 0x6C
        out = if testBit flg 6 then (0, 0) else (resize (dOutL c), resize (dOutR c))
    in (c { dStep = SIdle, dEchoLen = len, dEchoIdx = idx' }, noOut { doSample = Just out, doBusy = False })
 where
  go c' o = (c', o)
  withK (c', o) k = (c' { dK = k }, o)
  outWith c' o = (c', o)
  setHead s vc = s { dVoices = replace (0 :: Int) vc (dVoices s) }
  setVReg s r v = s { dRegs = replace (fromEnum (dV s) * 16 + r) v (dRegs s) }
  echoAddr s = (resize (reg s 0x6D) `shiftL` 8) + dEchoIdx s
  -- the one multiplier, its operands by step
  (ma, mb) = mulOps c i
  prod = resize ma * resize mb :: Signed 36

-- | The multiplier's operands of the step.
mulOps :: Dsp -> DspIn -> (Signed 18, Signed 18)
mulOps c i =
  let vc = head (dVoices c)
      s8 :: Unsigned 8 -> Signed 18
      s8 v = resize (bitCoerce v :: Signed 8)
      k = dK c
      firTap = (if k < 8 then dFirL c else dFirR c) !! (k .&. 7)
      coef = reg c (fromEnum (k .&. 7) * 16 + 0xF)
  in case dStep c of
       VGauss -> (resize (diSmp i), fromIntegral (diGauss i))
       VOut -> (resize (dSample c), fromIntegral (vEnv vc))
       VPitch -> ( fromIntegral ((resize (vreg c 3 .&. 0x3F) `shiftL` 8) .|. resize (vreg c 2) :: Unsigned 14)
                 , resize (vOut (last (dVoices c)) `shiftR` 4) )
       VVolL -> (resize (vOut vc), s8 (vreg c 0))
       VVolR -> (resize (vOut vc), s8 (vreg c 1))
       EFir -> (resize firTap, s8 coef)
       EOut -> case k of
         0 -> (resize (dMixL c), s8 (reg c 0x0C))
         1 -> (resize (dFirOutL c), s8 (reg c 0x2C))
         2 -> (resize (dMixR c), s8 (reg c 0x1C))
         3 -> (resize (dFirOutR c), s8 (reg c 0x3C))
         4 -> (resize (dFirOutL c), s8 (reg c 0x0D))
         _ -> (resize (dFirOutR c), s8 (reg c 0x0D))
       _ -> (0, 0)
