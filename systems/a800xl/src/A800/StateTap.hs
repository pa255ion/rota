{-|
The 800XL's state table in the core (side track N, PLAN.md N.5): the
record's fields a save state keeps, read-only, in "A800.StateTable"'s
layout, the one the spec fills ("A800.Spec.SaveState".'specTable') and
the PS's restore routine reads (@platform/ps/systems/a800_restore.zig@);
and where a state can be taken ('restoreable', the spec's
'A800.Spec.SaveState.specAtRest').

Wiring only: each byte is a field (or a piece of one) of 'CoreState', 0
where the table has nothing (synthesis drops those), one byte mux over
the table. Nothing in @cpus/mos6502@ changed: the 6502's record is read
as it is.
-}
module A800.StateTap
  ( tapTable
  , restoreable
  , stateTableVersion
  ) where

import Clash.Prelude
import Data.Maybe (isJust, isNothing)

import A800.Antic (Antic (..))
import A800.Core
import A800.Gtia (Gtia (..))
import A800.Pokey (Pokey (..))
import A800.StateTable
import Mos6502.Core (Cpu (..), Phase (..))

-- | Bumped whenever the table's meaning changes (a slot of another
-- bitstream is refused): 1.
stateTableVersion :: BitVector 8
stateTableVersion = 1

-- | Byte @a@ of the state table (0 past its end).
tapTable :: CoreState -> Unsigned 16 -> BitVector 8
tapTable s a
  | a < fromIntegral tableSize = bytes !! (truncateB a :: Unsigned 7)
  | otherwise = 0
 where
  bytes = map field (iterateI (+ 1) 0) :: Vec 128 (BitVector 8)
  c = ksCpu s
  an = ksAntic s
  g = ksGtia s
  p = ksPokey s
  u8 :: Unsigned 8 -> BitVector 8
  u8 = pack
  lo, hi :: Unsigned 16 -> BitVector 8
  lo x = pack (truncateB x :: Unsigned 8)
  hi x = pack (truncateB (x `shiftR` 8) :: Unsigned 8)
  b :: Bool -> BitVector 8
  b x = if x then 1 else 0
  field :: Unsigned 7 -> BitVector 8
  field i = case fromIntegral i :: Int of
    o | o == tA -> u8 (cA c)
      | o == tX -> u8 (cX c)
      | o == tY -> u8 (cY c)
      | o == tS -> u8 (cS c)
      | o == tP -> u8 (cP c)
      | o == tPC -> lo (cPc c)
      | o == tPC + 1 -> hi (cPc c)
      | o == tLine -> lo (zeroExtend (sY an))
      | o == tLine + 1 -> hi (zeroExtend (sY an))
      | o == tCycle -> zeroExtend (pack (sX an))
      | o == tNmist -> u8 (sNmist an)
      | o == tDmactl -> u8 (sDmactl an)
      | o == tChactl -> u8 (sChactl an)
      | o == tDlist -> lo (sDlist an)
      | o == tDlist + 1 -> hi (sDlist an)
      | o == tHscrol -> u8 (sHscrol an)
      | o == tVscrol -> u8 (sVscrol an)
      | o == tPmbase -> u8 (sPmbase an)
      | o == tChbase -> u8 (sChbase an)
      | o == tNmien -> u8 (sNmien an)
      | o == tPenh -> 0    -- no light pen (A800.Antic's reads)
      | o == tPenv -> 0xFF
      | o == tIrPrev -> u8 (sIrPrev an)
      | o == tMsc -> lo (sMsc an)
      | o == tMsc + 1 -> hi (sMsc an)
      | o == tHires -> b (sHires an)
      | o == tLoader -> b (ksStub s) .|. (if ksPage s /= 0 then 2 else 0)
      | o == tXexPos -> pack (truncateB (ksXexPos s) :: Unsigned 8)
      | o == tXexPos + 1 -> pack (truncateB (ksXexPos s `shiftR` 8) :: Unsigned 8)
      | o == tXexPos + 2 -> pack (truncateB (ksXexPos s `shiftR` 16) :: Unsigned 8)
      | o == tPactl -> u8 (ksPactl s)
      | o == tPbctl -> u8 (ksPbctl s)
      | o == tOra -> u8 (ksOra s)
      | o == tDdra -> u8 (ksDdra s)
      | o == tOrb -> u8 (ksOrb s)
      | o == tDdrb -> u8 (ksDdrb s)
      | o == tEdges -> b (ksEdgeA s) .|. (b (ksEdgeB s) `shiftL` 1)
      | o == tCb2 -> zeroExtend (pack (ksCb2 s))
      | o >= tAudf && o < tAudf + 4 -> u8 (pAudf p !! (fromIntegral (o - tAudf) :: Index 4))
      | o >= tAudc && o < tAudc + 4 -> u8 (pAudc p !! (fromIntegral (o - tAudc) :: Index 4))
      | o == tAudctl -> u8 (pAudctl p)
      | o == tSkctl -> u8 (pSkctl p)
      | o == tIrqen -> u8 (pIrqen p)
      | o == tIrqst -> u8 (pIrqst p)
      | o == tSkstat -> u8 (pSkstat p)
      | o == tKbcode -> u8 (pKbcode p)
      | o == tSerout -> u8 (pSerout p)
      | o == tSerial -> b (pSerValid p)
      | o >= tGtia && o < tGtia + 0x20 -> gtiaReg (o - tGtia)
      | o >= tLatch && o < tLatch + 8 -> u8 (gLatch g !! (fromIntegral (o - tLatch) :: Index 8))
      | o >= tColl && o < tColl + 8 -> u8 (gColl g !! (fromIntegral (o - tColl) :: Index 8))
      | o == tTrigLatch -> zeroExtend (gTrigLat g)
      | otherwise -> 0
  -- GTIA's registers in effect, as the spec's table has them
  gtiaReg :: Int -> BitVector 8
  gtiaReg r
    | r < 0x08 = u8 (gPos g !! (fromIntegral r :: Index 8))
    | r < 0x0C = zeroExtend (pack (gSizeP g !! (fromIntegral (r - 0x08) :: Index 4)))
    | r == 0x0C = u8 (gSizeM g)
    | r < 0x12 = 0
    | r < 0x16 = u8 (gColPm g !! (fromIntegral (r - 0x12) :: Index 4))
    | r < 0x1A = u8 (gColPf g !! (fromIntegral (r - 0x16) :: Index 4))
    | r == 0x1A = u8 (gColBk g)
    | r == 0x1B = u8 (gPrior g)
    | r == 0x1C = u8 (gVdelay g)
    | r == 0x1D = u8 (gGractl g)
    | r == 0x1F = u8 (gConsol g)
    | otherwise = 0

-- | Where a state can be taken (the spec's 'A800.Spec.SaveState.specAtRest'):
-- right after a CPU cycle that ended a step (the machine between two
-- cycles); the 6502 about to fetch an opcode (no interrupt sequence
-- next), no NMI edge owed, the IRQ line low; vertical blank (line 248
-- from cycle 64); no WSYNC armed or held, no CHBASE write on its way, no
-- NMI line or late NMI; GTIA's queue empty; POKEY's serial port idle (or
-- a byte waiting with no clock), no STIMER pending; the loader's page not
-- on its way out.
restoreable :: CoreState -> Bool
restoreable s =
  ksSeq s == QIdle && isJust (ksStep s) && cPhase c == PFetch && not (cNmiNeed c)
    && not (pIrqLine p || coreIrqPia s)
    && vblank
    && isNothing (sWsAge an) && not (sWsHold an) && sChbaseCnt an == 0 && not (sNmiLine an) && not (sLateNmi an)
    && all isNothing (gQueue (ksGtia s))
    && pStimerIn p == 0 && pSerTickIn p == 0 && serialOk
    && ksPage s /= 2
 where
  c = ksCpu s
  an = ksAntic s
  p = ksPokey s
  y = sY an
  x = sX an
  vblank = y >= 249 || y < 8 || (y == 248 && x >= 64)
  serialOk = (not (pSerValid p) && pSerCount p == 0)
          || (pSerValid p && pSerCount p == 1 && pSkctl p .&. 0x60 == 0)
