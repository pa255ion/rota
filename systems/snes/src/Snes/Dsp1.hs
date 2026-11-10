{-# LANGUAGE StrictData #-}
{-|
The DSP-1 cartridge's chip in the core (M7 step 7.16d; the @SNES_DSP1@
build, docs/systems/snes.md, "The DSP-1"): "Snes.Upd77c25" with its
clock, the firmware's load and the 5A22's accesses, one clock at a time
('dspClock').

__The clock.__ The chip runs on its own 7.6 MHz: 1672 / 4725 of the
master clock ("Snes.Spec.Machine"'s 'dspTarget'). Each master clock adds
1672; at 4725 an instruction falls due ('uiGo'). The 5A22's access to DR
or SR waits until every instruction due by its master clock has run
('dspReady0', 'dspReady1' for a clock with a tick: "Snes.Bus"'s
'waitsDsp'), so it sees the chip as the spec's catch-up does.

__The firmware__ (the user's dump, a companion file the PS puts in DDR:
'dspFwOffset' in the ROM port's offsets, which "Snes.Machine" maps there):
after reset, its 8 KiB are read a byte at a time through the ROM port
('doRomReq') into the program ROM (2,048 words of 3 bytes, little-endian)
and the data ROM (1,024 of 2), the chip held in reset and the machine
without master clocks meanwhile ('dspLoaded').
-}
module Snes.Dsp1
  ( DspRegs (..)
  , Load (..)
  , dspInit
  , DspIn (..)
  , DspOut (..)
  , dspClock
  , dspReady0
  , dspReady1
  , dspLoaded
  , dspReq
  , dspFwOffset
  ) where

import Clash.Prelude

import Snes.Upd77c25

-- | The firmware's place among the ROM port's offsets (past any DSP-1
-- game's ROM: 2 MiB at most).
dspFwOffset :: Unsigned 23
dspFwOffset = 0x700000

-- | The load: the next byte of the program (word, byte) or the data ROM,
-- or done.
data Load = LProg (Unsigned 11) (Index 3) | LData (Unsigned 10) (Index 2) | LDone
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFDataX, BitPack)

data DspRegs = DspRegs
  { dsCore :: UpdCore
  , dsAcc  :: Unsigned 13        -- ^ the clock's fraction (of 4725)
  , dsLoad :: Load
  , dsPend :: Bool               -- ^ the load's byte asked for, not yet in
  , dsPart :: BitVector 16       -- ^ the word's bytes so far
  , dsByte :: Unsigned 8         -- ^ the 5A22's read's byte (its next clock)
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (NFDataX, BitPack)

dspInit :: DspRegs
dspInit = DspRegs updCorePowerOn 0 (LProg 0 0) False 0 0

data DspIn = DspIn
  { diTick :: Bool                     -- ^ a master clock passed
  , diAcc  :: Maybe SnesAcc            -- ^ the 5A22's access ('Snes.Bus.boDsp')
  , diRom  :: Maybe (Unsigned 8)       -- ^ the ROM port's byte ('Snes.RomPort.rpOut')
  , diOp   :: BitVector 24
  , diRo   :: Unsigned 16
  , diMem  :: Unsigned 16
  , diMemK :: Unsigned 16
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (NFDataX, BitPack)

data DspOut = DspOut
  { doRomReq :: Maybe (Unsigned 23)                -- ^ the load's read (for one clock)
  , doProgW  :: Maybe (Unsigned 11, BitVector 24)
  , doDatW   :: Maybe (Unsigned 10, Unsigned 16)
  , doRam    :: Maybe (Unsigned 8, Unsigned 16)
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (NFDataX, BitPack)

dspLoaded :: DspRegs -> Bool
-- (by its tag: an Eq on the packed value would compare LDone's unused
-- payload bits too, undefined in the HDL; Verilator found it never done)
dspLoaded d = case dsLoad d of
  LDone -> True
  _ -> False

-- | The chip has run every instruction due (no tick this clock).
dspReady0 :: DspRegs -> Bool
dspReady0 d = ucOwed (dsCore d) == 0

-- | The same if this clock's tick comes.
dspReady1 :: DspRegs -> Bool
dspReady1 d = dspReady0 d && dsAcc d + 1672 < 4725

dspReq :: DspRegs -> ReqAddrs
dspReq = updReq . dsCore

-- | The load's byte offset in the firmware.
loadOffset :: Load -> Unsigned 23
loadOffset l = case l of
  LProg w k -> resize w * 3 + fromIntegral k
  LData w k -> 6144 + resize w * 2 + fromIntegral k
  LDone -> 0

-- | A clock.
dspClock :: DspRegs -> DspIn -> (DspRegs, DspOut)
dspClock d i = case dsLoad d of
  LDone ->
    let acc1 = dsAcc d + (if diTick i then 1672 else 0)
        due = acc1 >= 4725
        acc2 = if due then acc1 - 4725 else acc1
        (c', o) = updCycle (dsCore d) (UpdIn due (diOp i) (diRo i) (diMem i) (diMemK i) (diAcc i) False)
        byte = case diAcc i of
          Just SReadDr -> uoByte o
          Just SReadSr -> uoByte o
          _ -> dsByte d
    in (d { dsCore = c', dsAcc = acc2, dsByte = byte }, DspOut Nothing Nothing Nothing (uoRam o))
  l -> case diRom i of
    -- a byte in: the word's, written once whole
    Just b | dsPend d ->
      let part = dsPart d
          (l', pw, dw, part') = case l of
            LProg w 2 -> (if w == maxBound then LData 0 0 else LProg (w + 1) 0, Just (w, pack b ++# part), Nothing, 0)
            LProg w k -> (LProg w (k + 1), Nothing, Nothing, if k == 0 then resize (pack b) else (pack b ++# slice d7 d0 part))
            LData w 1 -> (if w == maxBound then LDone else LData (w + 1) 0, Nothing, Just (w, unpack (pack b ++# slice d7 d0 part)), 0)
            LData w k -> (LData w (k + 1), Nothing, Nothing, resize (pack b))
      in (d { dsLoad = l', dsPend = False, dsPart = part', dsCore = reset' }, DspOut Nothing pw dw Nothing)
    _ | dsPend d -> (d { dsCore = reset' }, DspOut Nothing Nothing Nothing Nothing)
      | otherwise -> (d { dsPend = True, dsCore = reset' }, DspOut (Just (dspFwOffset + loadOffset l)) Nothing Nothing Nothing)
 where
  -- (held in reset while loading)
  reset' = fst (updCycle (dsCore d) (UpdIn False 0 0 0 0 Nothing True))
