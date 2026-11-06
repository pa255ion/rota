{-|
The NES core's state table (side track N, PLAN.md N.8): the bytes of
"Nes.StateTable" read from 'CoreState', as the spec's
('Nes.Spec.SaveState.specTable') has them, and where a state can be
taken ('restoreable', the spec's 'Nes.Spec.SaveState.specAtRest').

Wiring only: each byte is a field (or a piece of one) of 'CoreState', 0
where the table has nothing (synthesis drops those), one byte mux over
the table. Nothing in @cpus/mos6502@ changed: the 6502's record is read
as it is.
-}
module Nes.StateTap
  ( tapTable
  , restoreable
  , stateTableVersion
  ) where

import Clash.Prelude
import Data.Maybe (isNothing)

import Mos6502.Core (Cpu (..), Phase (..))
import Nes.Apu
import Nes.Core
import Nes.Mapper
import Nes.Ppu (Ppu (..))
import Nes.StateTable

-- | Bumped whenever the table's meaning changes (a slot of another
-- bitstream is refused): 1.
stateTableVersion :: BitVector 8
stateTableVersion = 1

-- | Byte @a@ of the state table.
tapTable :: CoreState -> Unsigned 8 -> BitVector 8
tapTable s a = bytes !! a
 where
  bytes = map field (iterateI (+ 1) 0) :: Vec 256 (BitVector 8)
  c = ksCpu s
  p = ksPpu s
  ap = ksApu s
  dm = aDmc ap
  m = ksMap s
  m5 = msM5 m
  cfg = ksCfg s
  u8 :: Unsigned 8 -> BitVector 8
  u8 = pack
  lo, hi :: Unsigned 16 -> BitVector 8
  lo x = pack (truncateB x :: Unsigned 8)
  hi x = pack (truncateB (x `shiftR` 8) :: Unsigned 8)
  b :: Bool -> BitVector 8
  b x = if x then 1 else 0
  kind = case ccMapper cfg of
    MNrom -> 0
    MMmc1 -> 1
    MUxrom -> 2
    MCnrom -> 3
    MMmc3 -> 4
    MMmc5 -> 5
    MAxrom -> 7 :: BitVector 8
  -- the APU's registers as the values that set the channels now
  halt k = b (aHalt ap !! (k :: Index 4))
  pulse i = r0 :> r1 :> r2 :> r3 :> Nil
   where
    q = aPulse ap !! (i :: Index 2)
    e = puEnv q
    r0 = (zeroExtend (pack (puDuty q)) `shiftL` 6) .|. (halt (fromIntegral i) `shiftL` 5)
         .|. (b (eConst e) `shiftL` 4) .|. zeroExtend (pack (eVol e))
    r1 = (b (puSwEn q) `shiftL` 7) .|. (zeroExtend (pack (puSwPer q)) `shiftL` 4) .|. (b (puSwNeg q) `shiftL` 3)
         .|. zeroExtend (pack (puSwShift q))
    r2 = pack (truncateB (puPeriod q) :: Unsigned 8)
    r3 = pack (truncateB (puPeriod q `shiftR` 8) :: Unsigned 8)
  tri = aTri ap
  ns = aNoise ap
  triRegs = t0 :> 0 :> t2 :> t3 :> Nil
   where
    t0 = (halt 2 `shiftL` 7) .|. zeroExtend (pack (trLinLoad tri))
    t2 = pack (truncateB (trPeriod tri) :: Unsigned 8)
    t3 = pack (truncateB (trPeriod tri `shiftR` 8) :: Unsigned 8)
  noiseRegs = n0 :> 0 :> n2 :> 0 :> Nil
   where
    n0 = (halt 3 `shiftL` 5) .|. (b (eConst (nsEnv ns)) `shiftL` 4) .|. zeroExtend (pack (eVol (nsEnv ns)))
    n2 = (b (nsMode ns) `shiftL` 7) .|. zeroExtend (pack (nsIdx ns))
  dmcRegs = m0 :> m1 :> m2 :> m3 :> Nil
   where
    m0 = (b (dmIrqEn dm) `shiftL` 7) .|. (b (dmLoop dm) `shiftL` 6) .|. zeroExtend (pack (dmRate dm))
    m1 = zeroExtend (pack (dmLevel dm))
    m2 = pack (truncateB ((dmSampAddr dm - 0xC000) `shiftR` 6) :: Unsigned 8)
    m3 = pack (truncateB ((dmSampLen dm - 1) `shiftR` 4) :: Unsigned 8)
  apuRegs = pulse 0 ++ pulse 1 ++ triRegs ++ noiseRegs ++ dmcRegs :: Vec 20 (BitVector 8)
  mmc1 = u8 (zeroExtend (msShift m)) :> u8 (zeroExtend (msCount m)) :> u8 (zeroExtend (msControl m))
         :> u8 (zeroExtend (msChr0 m)) :> u8 (zeroExtend (msChr1 m)) :> u8 (zeroExtend (msPrg m)) :> Nil
  mmc3 = u8 (msSel m) :> map u8 (msR m) ++ (b (msMirror m) :> u8 (msIrqLatch m) :> u8 (msIrqCount m)
         :> b (msIrqReload m) :> b (msIrqEn m) :> b (msIrqFlag m) :> b (msA12 m) :> Nil)
  u2 :: Unsigned 2 -> BitVector 8
  u2 = zeroExtend . pack
  mmc5 = u2 (m5PrgMode m5) :> u2 (m5ChrMode m5) :> u2 (m5Prot1 m5) :> u2 (m5Prot2 m5) :> u2 (m5ExMode m5)
         :> u8 (m5Nt m5) :> u8 (m5FillT m5) :> u2 (m5FillA m5) :> Nil
         ++ map u8 (m5Prg m5) ++ map u8 (m5Chr m5)
         ++ (b (m5LastB m5) :> u8 (m5IrqCmp m5) :> b (m5IrqEn m5) :> b (m5IrqPend m5) :> b (m5InFrame m5)
             :> u8 (m5Mul1 m5) :> u8 (m5Mul2 m5) :> Nil)
  pick8 :: forall n. KnownNat n => Vec n (BitVector 8) -> Int -> Int -> BitVector 8
  pick8 v base o = v !! (fromIntegral (o - base) :: Index n)
  isK k = ccMapper cfg == k
  -- a $4016 write waiting for the end of a get cycle: the last cycle was
  -- one, the core lets it through at the next cycle's start (the spec at
  -- this one's end)
  strobeNow = case ksStrobePend s of
    Just v | ksOdd s -> v
    _ -> ksStrobe s
  field :: Unsigned 8 -> BitVector 8
  field i = case fromIntegral i :: Int of
    o | o == tA -> u8 (cA c)
      | o == tX -> u8 (cX c)
      | o == tY -> u8 (cY c)
      | o == tS -> u8 (cS c)
      | o == tP -> u8 (cP c)
      | o == tPC -> lo (cPc c)
      | o == tPC + 1 -> hi (cPc c)
      | o == tLine -> lo (zeroExtend (pLine p))
      | o == tLine + 1 -> hi (zeroExtend (pLine p))
      | o == tDot -> lo (zeroExtend (pDot p))
      | o == tDot + 1 -> hi (zeroExtend (pDot p))
      | o == tOdd -> b (pOdd p)
      | o == tParity -> b (ksOdd s)
      | o == tStrobe -> b strobeNow
      | o == tCtrl -> u8 (pCtrl p)
      | o == tMask -> u8 (pMask p)
      | o == tStatus -> (b (pVbl p) `shiftL` 7) .|. (b (pSpr0 p) `shiftL` 6) .|. (b (pOvf p) `shiftL` 5)
      | o == tOamAddr -> u8 (pOamAddr p)
      | o == tV -> lo (zeroExtend (pV p))
      | o == tV + 1 -> hi (zeroExtend (pV p))
      | o == tT -> lo (zeroExtend (pT p))
      | o == tT + 1 -> hi (zeroExtend (pT p))
      | o == tFineX -> zeroExtend (pack (pX p))
      | o == tW -> b (pW p)
      | o == tBuf -> u8 (pBuf p)
      | o == tLatch -> u8 (pLatch p)
      | o >= tPal && o < tPal + 32 -> zeroExtend (pack (pPal p !! (fromIntegral (o - tPal) :: Index 32)))
      | o >= tApu && o < tApu + 20 -> pick8 apuRegs tApu o
      | o == tEnabled -> zeroExtend (aEnabled ap)
      | o == tFrame -> (b (aMode ap) `shiftL` 7) .|. (b (aInhibit ap) `shiftL` 6)
      | o == tSeq -> lo (aSeq ap)
      | o == tSeq + 1 -> hi (aSeq ap)
      | o == tFrameIrq -> b (aIrq ap)
      | o == tDmcIrq -> b (dmIrq dm)
      | o >= tLengths && o < tLengths + 4 -> u8 (aLen ap !! (fromIntegral (o - tLengths) :: Index 4))
      | o == tMapper -> kind
      | o == tMirror -> b (ccVertical cfg)
      | o == tBoard -> b (ccChrRam cfg) .|. (b (ccPrgRam cfg || isK MMmc5) `shiftL` 1)
      | o >= tMmc1 && o < tMmc1 + 6 -> if isK MMmc1 then pick8 mmc1 tMmc1 o else 0
      | o == tBank -> if isK MUxrom || isK MCnrom || isK MAxrom then u8 (msBank m) else 0
      | o >= tMmc3 && o < tMmc3 + 16 -> if isK MMmc3 then pick8 mmc3 tMmc3 o else 0
      | o >= tMmc5 && o < tMmc5 + 32 -> if isK MMmc5 then pick8 mmc5 tMmc5 o else 0
      | o == tMmc5Line -> if isK MMmc5 then u8 (m5Line m5) else 0
      | o == tDmcCount -> u8 (dmCount dm)
      | o == tDmcCount + 1 -> 0
      | o == tDmcBits -> zeroExtend (pack (dmBits dm))
      | o == tDmcFull -> b (dmFull dm)
      | o == tDmcBuf -> if dmFull dm then u8 (dmBuffer dm) else 0
      | o == tDmcShift -> u8 (dmShift dm)
      | o == tDmcSilence -> b (dmSilence dm)
      | otherwise -> 0

-- | Where a state can be taken (the spec's 'Nes.Spec.SaveState.specAtRest'):
-- right after a CPU cycle that ended a step (the machine between two
-- cycles); the 6502 about to fetch an opcode (no interrupt sequence next),
-- no NMI edge owed; lines 243 to 260; no $2006 or $2007 access on its way
-- in the PPU, no sprite flag about to be set, no OAM corruption of a row
-- but 0; the APU with nothing on its way (no $4017 write, no flag clear,
-- the DMC idle and its IRQ flag clear, its full buffer two bits old) and
-- the frame counter's flag where the routine can leave it; no DMA, no
-- $4016 write on its way (but one the last cycle, a get, let through: the
-- core passes it on at the next cycle's start).
restoreable :: CoreState -> Bool
restoreable s =
  ksSeq s == QIdle && ksStepped s && cPhase c == PFetch && not (cNmiNeed c)
    && pLine p >= 243 && pLine p <= 260
    && pRdAle p == 0 && pRdFill p == 0 && pWrAle p == 0 && not (pWrDo p) && pVPend p == 0 && pFlagPend p == 0
    && pOamCor p <= 1
    && aPend ap == 0 && not (aIrqClr ap)
    && dmRemain dm == 0 && isNothing (dmWait dm) && not (dmAbort dm) && dmStopAge dm == 0 && dmEnWait dm == 0
    && not (dmIrq dm) && not frameRisk && (not (dmFull dm) || dmBits dm <= 6)
    && not (ksHalted s) && not (ksOamOn s) && ksDmcSt s == 0 && (isNothing (ksStrobePend s) || ksOdd s)
 where
  c = ksCpu s
  p = ksPpu s
  ap = ksApu s
  dm = aDmc ap
  sq = aSeq ap
  frameRisk = not (aMode ap) && (sq >= 29824 || (not (aInhibit ap) && not (aIrq ap) && sq < 24))
