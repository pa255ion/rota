{-|
The Game Boy core's state table (side track L, PLAN.md L.6c.2): the
record's fields a save state keeps, read-only, in "Gb.StateTable"'s
layout, the one the spec fills ("Gb.Spec.SaveState".'specTable') and the
PS's restore routine reads (@platform/ps/gb_restore.zig@). The state
window's reads below 0x1000 return its bytes ('gbStateTap', for
"Gb.Core".'gbCoreHooked').

Wiring only: each byte is a field (or a piece of one) of 'CoreState', a
constant 0 where the table has nothing (synthesis drops those), and one
byte mux over the table's 0x220 places. Nothing is written back: a state
is loaded through the CPU (the overlay in 'gbCoreHooked').
-}
module Gb.StateTap
  ( gbStateTap
  , tapTable
  , stateTableVersion
  ) where

import Clash.Prelude

import Gb.Apu (ApuState (..), ChanState (..))
import Gb.Core
import Gb.Cpu (Cpu (..))
import Gb.Ppu (PpuRegs (..), PpuState (..))
import Gb.StateTable

-- | Bumped whenever the table's meaning changes, so a slot written by
-- another bitstream is refused: 3 (L.6c; 1 and 2 were L.6's and L.6b's
-- records).
stateTableVersion :: BitVector 8
stateTableVersion = 3

gbStateTap :: StateTap
gbStateTap = StateTap
  { tapRead = tapTable
  , tapSize = fromIntegral tableSize
  , tapVersion = stateTableVersion
  }

-- | Byte @a@ of the state table (0 past its end).
tapTable :: CoreState -> Unsigned 16 -> BitVector 8
tapTable s a
  | a < 0x100 = bytes !! (truncateB a :: Unsigned 8)
  | a < 0x100 + 160 = ksOam s !! (truncateB (a - 0x100) :: Unsigned 8)
  | a >= fromIntegral tPal && a < fromIntegral tPal + 128 =
      if gbc then ksPal s !! (truncateB (a - fromIntegral tPal) :: Unsigned 7) else 0
  | otherwise = 0
 where
  bytes = map field (iterateI (+ 1) 0) :: Vec 256 (BitVector 8)
  cpu = ksCpu s
  regs = cRegs cpu -- B C D E H L F A
  ppu = fst (ksPpu s)
  pr = ksRegs s
  apu = ksApuSt s
  cfg = ksCfg s
  gbc = ccCgbHw cfg
  ch k = asCh apu !! (k :: Index 4)
  lo, hi :: BitVector 16 -> BitVector 8
  lo = slice d7 d0
  hi = slice d15 d8
  b :: Bool -> BitVector 8
  b x = if x then 1 else 0
  mbc = case ccMbc cfg of
    MbcNone -> 0
    MbcOne -> 1
    MbcTwo -> 2
    MbcThree -> if ccTimer cfg then 4 else 3
    MbcFive -> 5
  field :: Unsigned 8 -> BitVector 8
  field i = case fromIntegral i :: Int of
    o | o == tA -> regs !! (7 :: Index 8)
      | o == tF -> regs !! (6 :: Index 8)
      | o == tB -> regs !! (0 :: Index 8)
      | o == tC -> regs !! (1 :: Index 8)
      | o == tD -> regs !! (2 :: Index 8)
      | o == tE -> regs !! (3 :: Index 8)
      | o == tH -> regs !! (4 :: Index 8)
      | o == tL -> regs !! (5 :: Index 8)
      | o == tSp -> lo (cSp cpu)
      | o == tSp + 1 -> hi (cSp cpu)
      | o == tPc -> lo (cPc cpu)
      | o == tPc + 1 -> hi (cPc cpu)
      | o == tIme -> b (cIme cpu)
      | o == tModel -> b (ccCgbHw cfg) .|. (b (ccCgb cfg) `shiftL` 1)
      | o == tMbc -> mbc
      | o == tDiv -> lo (ksDiv s)
      | o == tDiv + 1 -> hi (ksDiv s)
      | o == tTima -> ksTima s
      | o == tTma -> ksTma s
      | o == tTac -> zeroExtend (ksTac s)
      | o == tIf -> zeroExtend (ksIf s)
      | o == tIe -> ksIe s
      | o == tSb -> ksSb s
      | o == tSc -> ksSc s
      | o == tP1 -> ksP1 s
      | o == tLy -> pack (psLy ppu)
      | o == tDot -> lo (zeroExtend (pack (psDot ppu)))
      | o == tDot + 1 -> hi (zeroExtend (pack (psDot ppu)))
      | o == tLcdc -> prLcdc pr
      | o == tStat -> prStat pr
      | o == tScy -> prScy pr
      | o == tScx -> prScx pr
      | o == tLyc -> prLyc pr
      | o == tBgp -> prBgp pr
      | o == tObp0 -> prObp0 pr
      | o == tObp1 -> prObp1 pr
      | o == tWy -> prWy pr
      | o == tWx -> prWx pr
      | o == tDma -> ksDmaReg s
      | o == tRomb -> lo (zeroExtend (ksRomb s))
      | o == tRomb + 1 -> hi (zeroExtend (ksRomb s))
      | o == tBank2 -> zeroExtend (ksBank2 s)
      | o == tRamen -> b (ksRamEn s)
      | o == tMode -> b (ksMode s)
      | o == tRtcLatch -> ksRtcPrev s
      | o >= tRtc && o < tRtc + 5 -> ksRtc s !! (fromIntegral (o - tRtc) :: Index 5)
      | o >= tRtcL && o < tRtcL + 5 -> ksRtcL s !! (fromIntegral (o - tRtcL) :: Index 5)
      | o >= tNr && o < tNr + 22 -> ksApu s !! (fromIntegral (o - tNr) :: Index 48)
      | o == tNr52 -> b (asPower apu) `shiftL` 7
      | o == tChOn -> b (csOn (ch 0)) .|. (b (csOn (ch 1)) `shiftL` 1) .|. (b (csOn (ch 2)) `shiftL` 2)
                      .|. (b (csOn (ch 3)) `shiftL` 3)
      | o >= tChLen && o < tChLen + 8 ->
          let k = fromIntegral ((o - tChLen) `div` 2) :: Index 4
              l = zeroExtend (pack (csLen (ch k))) :: BitVector 16
          in if even (o - tChLen) then lo l else hi l
      | o >= tChVol && o < tChVol + 4 -> zeroExtend (pack (csVol (ch (fromIntegral (o - tChVol)))))
      | o >= tWave && o < tWave + 16 -> ksApu s !! (fromIntegral (0x20 + o - tWave) :: Index 48)
      -- the Game Boy Color's (0 on the DMG)
      | not gbc -> 0
      | o == tKey1 -> ksKey1 s
      | o == tVbk -> b (ksVbk s)
      | o == tSvbk -> zeroExtend (ksSvbk s)
      | o == tBcps -> ksBcps s
      | o == tOcps -> ksOcps s
      | o == tOpri -> b (ksOpri s)
      | o == tFf72 -> ksFf72 s
      | o == tFf73 -> ksFf73 s
      | o == tFf74 -> ksFf74 s
      | o == tFf75 -> ksFf75 s
      | o == tRp -> ksRp s
      | o == tHdmaSrc -> lo (ksHdmaSrc s)
      | o == tHdmaSrc + 1 -> hi (ksHdmaSrc s)
      | o == tHdmaDst -> lo (zeroExtend (ksHdmaDst s))
      | o == tHdmaDst + 1 -> hi (zeroExtend (ksHdmaDst s))
      | otherwise -> 0
