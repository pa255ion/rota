{-|
A synthesis check for the Clash YM2413 (M6 step 6.5; @make sms-fm-hdl@):
'Sms.Fm.fmCycle' stepped on a cycle strobe, a write strobe in, the pins
out (registered), so Clash turns it into Verilog and Verilator lints it.
Not the Master System's top (step 6.6).
-}
module FmTop (topEntity) where

import Clash.Prelude

import Sms.Fm

-- | Per clock: the internal cycle's strobe and a write (valid, data port,
-- byte); out: the pins (MO, RO).
topEntity
  :: Clock System -> Reset System -> Enable System
  -> Signal System Bool -> Signal System Bool -> Signal System Bool -> Signal System (Unsigned 8)
  -> Signal System (Signed 10, Signed 10)
topEntity clk rst en cycS wrS dataPortS byteS = exposeClockResetEnable go clk rst en
 where
  go :: HiddenClockResetEnable System => Signal System (Signed 10, Signed 10)
  go = mealy step (fmPowerOn, (0, 0)) (bundle (cycS, wrS, dataPortS, byteS))
  step (st, out) (cyc, wr, dp, b)
    | cyc = let (st', p) = fmCycle (if wr then Just (dp, b) else Nothing) st in ((st', p), out)
    | otherwise = ((st, out), out)
{-# ANN topEntity (Synthesize
  { t_name = "sms_fm_top"
  , t_inputs = [PortName "clk", PortName "rst", PortName "en", PortName "cyc", PortName "wr", PortName "data_port", PortName "wr_byte"]
  , t_output = PortProduct "" [PortName "mo", PortName "ro"]
  }) #-}
