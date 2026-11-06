{-|
The Game Gear's and Master System's state table in the core (side track
N, PLAN.md N.2): the record's fields a save state keeps, read-only, in
"Gg.StateTable"'s layout, the one the spec fills
("Gg.Spec".'specTable') and the PS's restore routine reads
(@platform/ps/systems/gg_restore.zig@); and where a state can be taken
('restoreable', the spec's 'Gg.Spec.specAtRest').

Wiring only: each byte is a field (or a piece of one) of 'GgC' or the
machine's T-state count ('tTime': the core counts its T-states modulo 512
only, so "Gg.Hooks" keeps the count modulo 'tPhaseFrames'), 0 where the
table has nothing (synthesis drops those), one byte mux over the table.
RAM's DFFCh-DFFFh ('tRamTail') and the FM unit's registers are 0 here:
the PS fills the first from the slot's RAM, and the FM unit isn't
restored yet.
-}
module Gg.StateTap
  ( tapTable
  , restoreable
  , stateTableVersion
  ) where

import Clash.Prelude

import Gg.Core
import Gg.Psg (PsgC (..))
import Gg.StateTable
import Gg.Vdp (VdpC (..), vdpIntC)
import Z80.Core (Cpu (..), Z80T (..))

-- | Bumped whenever the table's meaning changes (a slot of another
-- bitstream is refused): 1.
stateTableVersion :: BitVector 8
stateTableVersion = 1

-- | Byte @a@ of the state table, from the core and the T-state count
-- (0 past its end).
tapTable :: GgC -> Unsigned 17 -> Unsigned 16 -> BitVector 8
tapTable g time a
  | a < 0x100 = bytes !! (truncateB a :: Unsigned 8)
  | a < 0x140 =
      let e = vCram v !! (truncateB ((a - 0x100) `shiftR` 1) :: Unsigned 5)
      in if even a then pack (truncateB e :: Unsigned 8) else zeroExtend (pack (truncateB (e `shiftR` 8) :: Unsigned 4))
  | otherwise = 0
 where
  bytes = map field (iterateI (+ 1) 0) :: Vec 256 (BitVector 8)
  c = zCpu (gZ g)
  v = gVdp g
  p = gPsg g
  u8 :: Unsigned 8 -> BitVector 8
  u8 = pack
  lo, hi :: Unsigned 16 -> BitVector 8
  lo x = pack (truncateB x :: Unsigned 8)
  hi x = pack (truncateB (x `shiftR` 8) :: Unsigned 8)
  b :: Bool -> BitVector 8
  b x = if x then 1 else 0
  field :: Unsigned 8 -> BitVector 8
  field i = case fromIntegral i :: Int of
    o | o == tA -> u8 (cA c)
      | o == tF -> u8 (cF c)
      | o == tB -> u8 (cB c)
      | o == tC -> u8 (cC c)
      | o == tD -> u8 (cD c)
      | o == tE -> u8 (cE c)
      | o == tH -> u8 (cH c)
      | o == tL -> u8 (cL c)
      | o == tAF' -> u8 (cF' c)
      | o == tAF' + 1 -> u8 (cA' c)
      | o == tBC' -> u8 (cC' c)
      | o == tBC' + 1 -> u8 (cB' c)
      | o == tDE' -> u8 (cE' c)
      | o == tDE' + 1 -> u8 (cD' c)
      | o == tHL' -> u8 (cL' c)
      | o == tHL' + 1 -> u8 (cH' c)
      | o == tIX -> lo (cIX c)
      | o == tIX + 1 -> hi (cIX c)
      | o == tIY -> lo (cIY c)
      | o == tIY + 1 -> hi (cIY c)
      | o == tSP -> lo (cSP c)
      | o == tSP + 1 -> hi (cSP c)
      | o == tPC -> lo (cPC c)
      | o == tPC + 1 -> hi (cPC c)
      | o == tI -> u8 (cI c)
      | o == tR -> u8 (cR c)
      | o == tIFF1 -> b (cIFF1 c)
      | o == tIFF2 -> b (cIFF2 c)
      | o == tIM -> zeroExtend (pack (cIM c))
      | o == tFlags -> b (gSms g) .|. (b (gSms2 g) `shiftL` 1) .|. (b (gMapper g == MCodemasters) `shiftL` 2)
                         .|. (b (gHasBios g) `shiftL` 3) .|. (b (gHasFm g) `shiftL` 4)
      | o == tWZ -> lo (cWZ c)
      | o == tWZ + 1 -> hi (cWZ c)
      | o == tTime -> pack (truncateB time :: Unsigned 8)
      | o == tTime + 1 -> pack (truncateB (time `shiftR` 8) :: Unsigned 8)
      | o == tTime + 2 -> zeroExtend (pack (truncateB (time `shiftR` 16) :: Unsigned 1))
      | o == tHalted -> b (cHalted c)
      | o >= tMap && o < tMap + 4 -> u8 (gMap g !! (fromIntegral (o - tMap) :: Index 4))
      | o == tCmRam -> b (gCmRam g)
      | o == tMemCtl -> u8 (gMemCtl g)
      | o == tIoCtl -> u8 (gIoCtl g)
      | o >= tGgRegs && o < tGgRegs + 7 -> u8 (gGgRegs g !! (fromIntegral (o - tGgRegs) :: Index 7))
      | o >= tBiosMap && o < tBiosMap + 3 -> u8 (gBiosMap g !! (fromIntegral (o - tBiosMap) :: Index 3))
      | o >= tVdpRegs && o < tVdpRegs + 11 -> u8 (vRegs v !! (fromIntegral (o - tVdpRegs) :: Index 11))
      | o == tVdpAddr -> lo (zeroExtend (vAddr v))
      | o == tVdpAddr + 1 -> hi (zeroExtend (vAddr v))
      | o == tVdpCode -> zeroExtend (pack (vCode v))
      | o == tVdpLatched -> b (vLatched v)
      | o == tVdpLatch -> u8 (vLatchB v)
      | o == tVdpBuf -> u8 (vBuf v)
      | o == tVdpCramLatch -> u8 (vCramL v)
      | o == tVdpStatus -> u8 (vStatus v)
      | o == tVdpLinePend -> b (vLinePend v)
      | o == tVdpLineCnt -> u8 (vLineCnt v)
      | o == tVdpHLatch -> u8 (vHLatch v)
      | o >= tPsgPeriod && o < tPsgPeriod + 6 ->
          let x = zeroExtend (pPeriod p !! (fromIntegral ((o - tPsgPeriod) `div` 2) :: Index 3)) :: Unsigned 16
          in if even (o - tPsgPeriod) then lo x else hi x
      | o == tPsgNoise -> zeroExtend (pack (pNoise p))
      | o >= tPsgVol && o < tPsgVol + 4 -> zeroExtend (pack (pVol p !! (fromIntegral (o - tPsgVol) :: Index 4)))
      | o == tPsgLatch -> zeroExtend (pack (pLatch p))
      | o == tPsgLfsr -> lo (pLfsr p)
      | o == tPsgLfsr + 1 -> hi (pLfsr p)
      | otherwise -> 0

-- | Where a state can be taken (the spec's 'Gg.Spec.specAtRest'): the CPU
-- at an instruction's start (running or halted), no EI pending, IFF1 =
-- IFF2, no interrupt about to be taken, no NMI edge (@nmi@: the Master
-- System's pause button), no port access or PSG/FM write under way, the
-- VDP on lines 196 to 259.
restoreable :: Bool -> GgC -> Bool
restoreable nmi g =
  cNew c && not (cEI c) && cIFF1 c == cIFF2 c && not (cIFF1 c && vdpIntC v) && not nmi
    && gPreroll g == 0 && gVdpOp g == Nothing && gPsgW g == Nothing && gFmW g == Nothing
    && vLine v >= 196 && vLine v <= 259
 where
  c = zCpu (gZ g)
  v = gVdp g
