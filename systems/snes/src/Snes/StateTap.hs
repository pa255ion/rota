{-|
The Super Nintendo's state table in the circuit (step 7.15c;
docs/systems/snes.md, "Save states through the CPU"): the fields of
"Snes.Spec.SaveState"'s table, from the core's registers, for the CPU
build's restore ('SNES_STATES=cpu'): the PS reads them (a byte at a time,
through one mux) and builds the restore routine
(platform/ps/systems/snes_restore.zig). Between two instructions
('Snes.Core.atRest' and the CPU's instruction end) they are the spec's
there, field by field: snes-core checks it.
-}
module Snes.StateTap
  ( tableFields
  , tableOf
  , tableByte
  , tableWindow
  ) where

import Clash.Prelude

import Snes.Bus (BusC (..))
import Snes.Ppu (PpuC (..))
import W65c816.Core (Cpu (..), Phase (..))

-- | The table's fields (Snes.Spec.SaveState's 130).
type Fields = 130

tableFields :: SNat Fields
tableFields = SNat

-- | The fields, as the spec's table has them (4 bytes each, little-endian).
tableOf :: BusC -> PpuC -> Vec Fields (Unsigned 32)
tableOf b p = cpu ++ slots ++ ppu ++ (resize (pStat77 p) :> resize (bMc b .&. 7) :> Nil)
 where
  -- (the CPU's registers as they are: the CPU build's freeze comes after a
  -- clock with no master clock in which the instruction finished,
  -- 'Snes.Core.stFinish'; the lockstep test applies 'W65c816.Core.finished'
  -- at an instruction's last clock)
  c = bCpu b
  w :: (Integral a) => a -> Unsigned 32
  w = fromIntegral
  bool x = if x then 1 else 0 :: Unsigned 32
  run = case cPhase c of
    PhWait -> 1
    PhStop -> 2
    _ -> 0
  cpu = w (cA c) :> w (cX c) :> w (cY c) :> w (cS c) :> w (cD c) :> w (cDBR c) :> w (cPBR c) :> w (cPC c)
        :> w (cP c) :> bool (cE c) :> run :> bool (cNmiLine c) :> bool (cNmiNeed c)
        :> w (unpack (cPollCur c) :: Unsigned 2) :> w (unpack (cPollPrev c) :: Unsigned 2)
        :> w (unpack (cPollPrev c) :: Unsigned 2) :> Nil
  slots = w (bHc b) :> w (bVc b) :> bool (bField b) :> w (bMdr b) :> w (bNmitimen b) :> bool (bRdnmi b)
          :> bool (bTimeup b) :> w (bWrio b) :> w (bMpya b) :> w (bShift b) :> w (bDiv b)
          :> w (bHtime b) :> w (bVtime b) :> w (bHdmaen b) :> bool (bMemsel b) :> w (bRddiv b) :> w (bRdmpy b)
          :> w (bJoy b !! (0 :: Int)) :> w (bJoy b !! (1 :: Int)) :> w (bJoy b !! (2 :: Int)) :> w (bJoy b !! (3 :: Int))
          :> w (bJoyBusy b) :> bool (bStrobe b) :> w (bSer b !! (0 :: Int)) :> w (bSer b !! (1 :: Int))
          :> w (pVaddr p) :> w (pRegs p !! (0x15 :: Int)) :> w (pVlatch p) :> w (pCgadd p) :> w (pCglatch p)
          :> bool (pCgflip p) :> w (pOamadd p) :> w (pOamlatch p) :> w (pOamrel p) :> w (bWmadd b)
          :> w (bHdmaDo b) :> w (bHdmaDone b) :> w (pHlatch p) :> w (pVlatch2 p) :> bool (pLatched p)
          :> w (pM7Prev p) :> bool (pHflip p) :> bool (pVflip p) :> w (pBgPrev p) :> w (pBgPrev2 p) :> Nil
  -- 2100h-2142h as the spec keeps them: the scrolls as their 10-bit
  -- values, Mode 7's matrix and offsets as 16-bit, COLDATA at 42h, the
  -- rest as written
  ppu = imap reg (repeat () :: Vec 67 ())
  reg (i :: Index 67) _ = case i of
    _ | i >= 0x0D && i <= 0x14 ->
          let n = fromIntegral ((i - 0x0D) `shiftR` 1) :: Index 4
          in w (if odd i then pHofs p !! n else pVofs p !! n)
      | i >= 0x1B && i <= 0x20 -> w (pM7 p !! (fromIntegral (i - 0x1B) :: Index 6))
      | i == 0x40 -> w (pM7Ofs p !! (0 :: Int))
      | i == 0x41 -> w (pM7Ofs p !! (1 :: Int))
      | i == 0x42 -> w (pFixed p)
      | i == 0x32 -> 0
      | otherwise -> w (pRegs p !! (fromIntegral i :: Index 64))

-- | Byte n of the table (n below 4 × 130), then OAM's high table (32
-- bytes: registers in the circuit; the restore routine writes them back
-- through 2104h, platform/ps/systems/snes_restore.zig's fill).
tableByte :: BusC -> PpuC -> Unsigned 10 -> BitVector 8
tableByte b p n
  | n < 520 =
      let f = tableOf b p !! (fromIntegral (n `shiftR` 2) :: Index Fields)
      in pack (resize (f `shiftR` (8 * fromIntegral (n .&. 3))) :: Unsigned 8)
  | otherwise = pack (pOamHigh p !! (resize (n - 520) :: Unsigned 5))

-- | The window's bytes: the table, then OAM's high table.
tableWindow :: Int
tableWindow = 520 + 32
