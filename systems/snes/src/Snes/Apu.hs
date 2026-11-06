{-# OPTIONS_GHC -fconstraint-solver-iterations=0 #-}
{-# LANGUAGE StrictData #-}
{-|
The sound module in Clash (PLAN.md step 7.9b; docs/systems/snes.md, "The
machine in Clash"): the SPC700 core ("Spc700.Core"), the boot ROM's port,
the I/O ports at 00F0h-00FFh, the three timers, the four ports to the CPU
and the S-DSP ("Snes.Dsp"); "Snes.Spec.Apu" transcribed, with ARAM (two
ports: the SPC700's and the DSP's), the boot ROM (64 bytes: ours, or the
user's dump), the DSP's samples and its Gaussian table outside on ports.
The SPC700 starts at FFC0h, every boot ROM's reset vector.

__Time.__ The spec catches the module up to the master clock's target
(5632/118125 SPC700 cycles a master clock, counted here along 'aiTick'),
a whole instruction at a time, at each access to its ports and each
line's end. This module starts an instruction (or a halted CPU's two
cycles) whenever its first cycle is before the target, so at every port
access it stands at the instruction boundary the spec's catch-up reaches
(the first at or after the target; the target only grows, so the
spec's line-end catch-ups change nothing). A port access needs it there
('aoReady0' without a tick in that clock, 'aoReady1' with one).

__A cycle__ takes two clocks: its start (the count, the timers; every
32nd the DSP's sample begins) with its access, then its value into the
core (a write lands then: its first clock read the byte it replaces).

__The DSP runs beside the SPC700__ (step 7.12; it used to hold the
SPC700 for the up to 240 clocks of a sample, and the CPU's port accesses
waited meanwhile: about 10 % of a frame, more while a game uploads its
music, which made rows late). The spec's sample happens at the start of
its cycle, before that cycle's access and every later one; here the
SPC700 runs on while the DSP works on its own ARAM port, and the sample
still sees what the spec's sees:

* the SPC700's ARAM writes during a sample are logged with the byte they
  replace ('aLog', 16 at most), and the DSP's reads take the oldest
  logged byte for their address;
* the sample's echo bytes (four from ESA × 256 plus the echo index, when
  FLG lets it write them) can't be read or written by the SPC700 until
  the sample is done (the DSP reads them, then writes them);
* the SPC700's DSP register writes wait in a queue ('aRegQ', 4) and go
  in, in order, once the sample is done; a register read waits for that;
* the next sample waits for this one (and the queue).

A full log or queue holds the SPC700 too. None of this waits for the
CPU's port accesses: those need only the SPC700's instruction boundary.
-}
module Snes.Apu
  ( ApuIn (..)
  , ApuOut (..)
  , ApuPhase (..)
  , ApuC (..)
  , apuInit
  , apuClock
  , apuIdleIn
  , apuStatus
  ) where

import Clash.Prelude

import Data.Maybe (isJust)

import Snes.Dsp
import qualified Spc700.Core as S

data ApuIn = ApuIn
  { aiTick :: Bool                                -- ^ a master clock passed
  , aiPort :: Maybe (Unsigned 2, Maybe (Unsigned 8)) -- ^ the CPU's port access (when ready): read, or write
  , aiAram :: Unsigned 8                          -- ^ ARAM's SPC700 port at last clock's address
  , aiDAram :: Unsigned 8                         -- ^ ARAM's DSP port at last clock's address
  , aiIpl  :: Unsigned 8                          -- ^ the boot ROM at last clock's address
  , aiSmp  :: Signed 16                           -- ^ the DSP's samples
  , aiGauss :: Unsigned 11                        -- ^ its Gaussian table
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (NFDataX, BitPack)

apuIdleIn :: ApuIn
apuIdleIn = ApuIn False Nothing 0 0 0 0 0

data ApuOut = ApuOut
  { aoReady0 :: Bool                -- ^ a port access may happen now (no tick this clock)
  , aoReady1 :: Bool                -- ^ the same, with a tick this clock
  , aoRead   :: Unsigned 8          -- ^ the last port read's value
  , aoAram   :: Unsigned 16         -- ^ ARAM's SPC700 port
  , aoAramW  :: Maybe (Unsigned 8)
  , aoDAram  :: Unsigned 16         -- ^ ARAM's DSP port
  , aoDAramW :: Maybe (Unsigned 8)
  , aoIpl    :: Unsigned 6
  , aoSmpRd  :: Unsigned 8
  , aoSmpW   :: Maybe (Unsigned 8, Signed 16)
  , aoGauss  :: Unsigned 9
  , aoSample :: Maybe (Signed 16, Signed 16)  -- ^ the DSP's sample (32 kHz)
  , aoSampleN :: Unsigned 32                  -- ^ for the tests: the cycle the sample belongs to
  , aoWrite  :: Maybe (Unsigned 32, Unsigned 16, Unsigned 8) -- ^ for the tests: the SPC700's writes, at the cycle count
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (NFDataX, BitPack)

data ApuPhase
  = ABoundary     -- ^ between instructions: waits for the target
  | AStart        -- ^ a cycle's start and access
  | AAccess       -- ^ the cycle's access, held (a wait for the DSP)
  | AComplete     -- ^ its value into the core (a write lands)
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFDataX, BitPack)

data ApuC = ApuC
  { aCpu      :: S.Cpu
  , aDsp      :: Dsp
  , aPhase    :: ApuPhase
  , aN        :: Unsigned 32       -- ^ cycles run
  , aTgt      :: Unsigned 32       -- ^ the target
  , aRem      :: Unsigned 17       -- ^ its fraction (of 118125)
  , aData     :: Unsigned 8        -- ^ a read's value known at once
  , aFromAram :: Bool              -- ^ the read's value comes from ARAM
  , aFromIpl  :: Bool              -- ^ or from the boot ROM
  , aControl  :: Unsigned 8
  , aDspAddr  :: Unsigned 8
  , aIn       :: Vec 4 (Unsigned 8)  -- ^ what the CPU wrote
  , aOut      :: Vec 4 (Unsigned 8)  -- ^ what the SPC700 wrote
  , aDiv      :: Vec 3 (Unsigned 8)
  , aStage    :: Vec 3 (Unsigned 9)
  , aTOut     :: Vec 3 (Unsigned 4)
  , aLastPort :: Unsigned 2
    -- the DSP beside the SPC700
  , aBusy     :: Bool                          -- ^ a sample in work
  , aSampleN  :: Unsigned 32                   -- ^ its cycle
  , aEcho     :: Maybe (Unsigned 16)           -- ^ its echo bytes (four from here), while it may write them
  , aLog      :: Vec 16 (Unsigned 16, Unsigned 8)  -- ^ the SPC700's writes during it, the byte each replaced, oldest first
  , aLogN     :: Index 17
  , aRegQ     :: Vec 4 (Unsigned 8, Unsigned 8)    -- ^ register writes waiting, oldest first
  , aRegN     :: Index 5
  , aDRd      :: Unsigned 16                   -- ^ the DSP's ARAM read last clock
  , aWPend    :: Maybe (Unsigned 16, Unsigned 8)   -- ^ the SPC700's write landing next clock
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (NFDataX, BitPack)

-- | After reset: CONTROL B0h (the boot ROM on), the SPC700 at FFC0h.
apuInit :: ApuC
apuInit = ApuC
  { aCpu = S.cpuPowerOn (S.CpuRegs 0 0 0 0 0xFFC0 0)
  , aDsp = dspInit, aPhase = ABoundary, aN = 0, aTgt = 0, aRem = 0
  , aData = 0, aFromAram = False, aFromIpl = False, aControl = 0xB0, aDspAddr = 0
  , aIn = repeat 0, aOut = repeat 0, aDiv = repeat 0, aStage = repeat 0, aTOut = repeat 0, aLastPort = 0
  , aBusy = False, aSampleN = 0, aEcho = Nothing, aLog = repeat (0, 0), aLogN = 0
  , aRegQ = repeat (0, 0), aRegN = 0, aDRd = 0, aWPend = Nothing
  }

-- | 'aoReady0', 'aoReady1' and 'aoRead', from the state only (a machine
-- can take them in the clock it drives the port access, "Snes.Core").
apuStatus :: ApuC -> (Bool, Bool, Unsigned 8)
apuStatus c =
  let inc = resize (aRem c) + 5632 >= (118125 :: Unsigned 18)
      atBoundary = aPhase c == ABoundary
  in ( atBoundary && aN c >= aTgt c
     , atBoundary && aN c >= aTgt c + (if inc then 1 else 0)
     , aOut c !! aLastPort c )

-- | The DSP's state with a sample begun (FLG and the echo pointer as the
-- sample will use them: the registers can't change while it works).
echoOf :: Dsp -> Maybe (Unsigned 16)
echoOf d =
  if testBit (dRegs d !! (0x6C :: Int)) 5 then Nothing
    else Just ((resize (dRegs d !! (0x6D :: Int)) `shiftL` 8) + dEchoIdx d)

-- | An address among the four echo bytes from e.
inEcho :: Maybe (Unsigned 16) -> Unsigned 16 -> Bool
inEcho e a = case e of
  Just b -> a - b < 4
  Nothing -> False

-- | What the SPC700's cycle hands the DSP this clock, when the DSP was idle
-- at the clock's start: a sample to start, or a register write.
data DspReq = RNone | RSample | RWrite (Unsigned 8, Unsigned 8)

-- | One clock. The DSP steps once a clock at most: 'dspClock' has one
-- instance, its input chosen first (a sample in work, then a queued
-- register write, then the SPC700's request). The SPC700's cycle has one
-- path through its start and access: a case over the phase that called
-- them from several branches made a copy of each per branch, and of the
-- DSP step inside them (the sound module measured 45,301 LUTs with the
-- hierarchy kept, 2026-10-18). The SPC700 sees the DSP as it was at the
-- clock's start, so in the clock a sample ends, a sample due, a read of
-- F3, or a write the echo or a full log would block waits one clock more,
-- and an F3 write is queued.
apuClock :: ApuC -> ApuIn -> (ApuC, ApuOut)
apuClock c0 i = (cZ, out)
 where
  -- the target, a master clock at a time
  (ready0, ready1, readV) = apuStatus c0
  cT | aiTick i = let r = resize (aRem c0) + 5632 :: Unsigned 18
                  in if r >= 118125 then c0 { aRem = resize (r - 118125), aTgt = aTgt c0 + 1 } else c0 { aRem = resize r }
     | otherwise = c0
  -- the CPU's port access
  cP = case aiPort i of
    Just (p, Just v) -> cT { aIn = replace p v (aIn cT) }
    Just (p, Nothing) -> cT { aLastPort = p }
    Nothing -> cT
  -- the DSP's ARAM reads as of its sample's start
  dspData = maybe (aiDAram i) id (foldr pick Nothing (zip indicesI (aLog cP)))
  pick (k, (a, old)) rest = if fromEnum k < fromEnum (aLogN cP) && a == aDRd cP then Just old else rest
  -- the DSP idle: no sample in work, no register write waiting
  idleOf c = not (aBusy c) && aRegN c == 0
  -- the SPC700's cycle, on the state at the clock's start
  (cS, aram, aramW, req, wr) = spc cP
  -- the DSP's one step this clock: a sample in work, else a queued
  -- register write, else what the SPC700's cycle hands it
  dIn | aBusy cP = Just (DspIn False Nothing dspData (aiSmp i) (aiGauss i))
      | aRegN cP > 0 = Just (DspIn False (Just (head (aRegQ cP))) 0 0 0)
      | otherwise = case req of
          RSample -> Just (DspIn True Nothing 0 0 0)
          RWrite w -> Just (DspIn False (Just w) 0 0 0)
          RNone -> Nothing
  (dNext, dO) = dspClock (aDsp cP) (maybe dspIdleIn id dIn)
  dsp' = if isJust dIn then dNext else aDsp cP
  sampleStarts = idleOf cP && isSample req
  isSample r = case r of
    RSample -> True
    _ -> False
  done = aBusy cP && isJust (doSample dO)
  cZ | aBusy cP =
         -- (a write landing this clock went into the log; if the sample
         -- ends, the log is emptied)
         cS { aDsp = dsp', aBusy = not done, aEcho = if done then Nothing else aEcho cS
            , aLogN = if done then 0 else aLogN cS, aDRd = doAram dO }
     | aRegN cP > 0 =
         -- the queue's head applied (the SPC700 may have queued one more)
         cS { aDsp = dsp', aRegQ = aRegQ cS <<+ (0, 0), aRegN = aRegN cS - 1 }
     | sampleStarts = cS { aDsp = dsp', aDRd = doAram dO }
     | otherwise = cS { aDsp = dsp' }
  -- the DSP's output: its sample's first clock, or a clock in work
  dOutF = if aBusy cP || sampleStarts then Just dO else Nothing
  -- the SPC700: a cycle's completion, or its start (when due) and access,
  -- or its held access
  spc c = case aPhase c of
    AComplete ->
      let d | aFromAram c = aiAram i
            | aFromIpl c = aiIpl i
            | otherwise = aData c
          (cpu', o) = S.cpuCycle (aCpu c) d
          next = if S.coStep o then ABoundary else AStart
          c1 = c { aCpu = cpu', aPhase = next, aFromAram = False, aFromIpl = False, aWPend = Nothing }
      in case aWPend c of
           -- the write lands; during a sample, its old byte into the log
           Just (a, v) ->
             let c2 = if aBusy c
                        then c1 { aLog = replace (resize (aLogN c) :: Index 16) (a, aiAram i) (aLog c), aLogN = aLogN c + 1 }
                        else c1
             in (c2, a, Just v, RNone, Nothing)
           Nothing -> (c1, 0, Nothing, RNone, Nothing)
    ph ->
      let starting = ph == AStart || (ph == ABoundary && aN c < aTgt c)
          ready | starting = start c
                | ph == AAccess = Just (c, False)
                | otherwise = Nothing
      in case ready of
           Nothing -> (c, 0, Nothing, RNone, Nothing)
           Just (c', smp) ->
             let (c'', a, w, rq, wr') = access c'
             in (c'', a, w, if smp then RSample else rq, wr')
  -- a cycle's start: the count, the timers; the DSP's sample every 32nd
  -- (once the last one and its queued writes are done): the state for the
  -- cycle's access and whether a sample starts, or Nothing while it waits
  start c =
    let n' = aN c + 1
        sampleDue = resize n' == (0 :: Unsigned 5)
    in if sampleDue && not (idleOf c) then Nothing
       else
         let c1 = timers n' c { aN = n' }
         in if sampleDue
              then Just (c1 { aBusy = True, aSampleN = n', aEcho = echoOf (aDsp c1), aLogN = 0 }, True)
              else Just (c1, False)
  timers n c =
    let t2 = resize n == (0 :: Unsigned 4)
        t01 = resize n == (0 :: Unsigned 7)
        go :: Index 3 -> ApuC -> ApuC
        go k cc = if testBit (aControl cc) (fromEnum k) then timer k cc else cc
        c1 = if t2 then go (2 :: Index 3) c else c
    in if t01 then go 1 (go 0 c1) else c1
  timer k c =
    let st = aStage c !! k + 1
        dv = aDiv c !! k
        lim = if dv == 0 then 256 else resize dv :: Unsigned 9
    in if st >= lim
         then c { aStage = replace k 0 (aStage c), aTOut = replace k (aTOut c !! k + 1) (aTOut c) }
         else c { aStage = replace k st (aStage c) }
  -- the cycle's access: a read (from the registers and the boot ROM at
  -- once, from ARAM the next clock), a write (ARAM always, landing next
  -- clock, and the registers), or nothing; held (AAccess) while it must
  -- wait for the DSP
  hold c = (c { aPhase = AAccess }, 0, Nothing, RNone, Nothing)
  access c = case S.cpuReq (aCpu c) of
    S.RRead a
      | a == 0xF3 && not (idleOf c) -> hold c
      | a >= 0xF0 && a <= 0xFF && a /= 0xF8 && a /= 0xF9 ->
          let (c1, v) = ioRead c (resize a)
          in (c1 { aPhase = AComplete, aData = v, aFromAram = False }, a, Nothing, RNone, Nothing)
      | a >= 0xFFC0 && testBit (aControl c) 7 ->
          (c { aPhase = AComplete, aFromIpl = True, aFromAram = False }, a, Nothing, RNone, Nothing)
      | aBusy c && inEcho (aEcho c) a -> hold c
      | otherwise -> (c { aPhase = AComplete, aFromAram = True }, a, Nothing, RNone, Nothing)
    S.RWrite a v
      | aBusy c && (inEcho (aEcho c) a || aLogN c == maxBound) -> hold c
      | a == 0xF3 && aRegN c == maxBound -> hold c
      | otherwise ->
          let (c1, rq) = if a >= 0xF0 && a <= 0xFF then ioWrite c (resize a) v else (c, RNone)
          -- read the byte it replaces now; it lands next clock
          in (c1 { aPhase = AComplete, aFromAram = False, aData = 0, aWPend = Just (a, v) }, a, Nothing, rq, Just (aN c, a, v))
    S.RIdle -> (c { aPhase = AComplete, aFromAram = False, aData = 0 }, 0, Nothing, RNone, Nothing)
  ioRead :: ApuC -> Unsigned 8 -> (ApuC, Unsigned 8)
  ioRead c r = case r of
    0xF2 -> (c, aDspAddr c)
    0xF3 -> (c, dspRegRead (aDsp c) (aDspAddr c))
    _ | r >= 0xF4 && r <= 0xF7 -> (c, aIn c !! (fromIntegral (r - 0xF4) :: Index 4))
      | r >= 0xFD ->
          let k = fromIntegral (r - 0xFD) :: Index 3
          in (c { aTOut = replace k 0 (aTOut c) }, resize (aTOut c !! k))
      | otherwise -> (c, 0)
  ioWrite :: ApuC -> Unsigned 8 -> Unsigned 8 -> (ApuC, DspReq)
  ioWrite c r v = case r of
    -- a register write: at once when the DSP is idle (this clock's DSP
    -- step), else queued
    0xF3 | idleOf c -> (c, RWrite (aDspAddr c, v))
         | otherwise -> (c { aRegQ = replace (resize (aRegN c) :: Index 4) (aDspAddr c, v) (aRegQ c), aRegN = aRegN c + 1 }, RNone)
    _ -> (ioWrite' c r v, RNone)
  ioWrite' :: ApuC -> Unsigned 8 -> Unsigned 8 -> ApuC
  ioWrite' c r v = case r of
    0xF1 ->
      let old = aControl c
          rise k = testBit v k && not (testBit old k)
          clr k cc = if rise (fromEnum k) then cc { aStage = replace k 0 (aStage cc), aTOut = replace k 0 (aTOut cc) } else cc
          c1 = clr (2 :: Index 3) (clr 1 (clr 0 c { aControl = v }))
          in01 = if testBit v 4 then replace (0 :: Int) 0 (replace (1 :: Int) 0 (aIn c1)) else aIn c1
          in23 = if testBit v 5 then replace (2 :: Int) 0 (replace (3 :: Int) 0 in01) else in01
      in c1 { aIn = in23 }
    0xF2 -> c { aDspAddr = v }
    _ | r >= 0xF4 && r <= 0xF7 -> c { aOut = replace (fromIntegral (r - 0xF4) :: Index 4) v (aOut c) }
      | r >= 0xFA && r <= 0xFC -> c { aDiv = replace (fromIntegral (r - 0xFA) :: Index 3) v (aDiv c) }
      | otherwise -> c
  out = ApuOut
    { aoReady0 = ready0
    , aoReady1 = ready1
    , aoRead = readV
    , aoAram = aram
    , aoAramW = aramW
    , aoDAram = maybe 0 doAram dOutF
    , aoDAramW = maybe Nothing doAramW dOutF
    , aoIpl = resize aram
    , aoSmpRd = maybe 0 doSmpRd dOutF
    , aoSmpW = maybe Nothing doSmpW dOutF
    , aoGauss = maybe 0 doGauss dOutF
    , aoSample = maybe Nothing doSample dOutF
    , aoSampleN = aSampleN cP
    , aoWrite = wr
    }
