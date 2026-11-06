{-|
A synthesis check for the Clash Z80 (PLAN.md step 5.5; @make z80-hdl@):
"Z80.Core"'s T-state wrapper as a Mealy machine with its bus as ports, so
Clash turns the core into Verilog and Verilator lints it. Not a system
top: the Game Gear's ('Gg.Top', step 5.9) embeds the core itself.
-}
module Z80Top (topEntity) where

import Clash.Prelude

import Z80.Core

-- | Per T-state: WAIT, the data bus, /INT, /NMI in; the pending cycle's
-- kind (0 fetch, 1 read, 2 write, 3 in, 4 out, 5 idle, 6 acknowledge),
-- address and write data, and whether this T-state ends it.
topEntity
  :: Clock System -> Reset System -> Enable System
  -> Signal System Bool -> Signal System (Unsigned 8) -> Signal System Bool -> Signal System Bool
  -> Signal System (BitVector 3, Unsigned 16, Unsigned 8, Bool)
topEntity clk rst en wait din int nmi =
  exposeClockResetEnable (mealy step z80PowerOn (bundle (wait, din, int, nmi))) clk rst en
 where
  step z (w, d, i, n) =
    let rq = cReq (zCpu z)
        out = (kindBits (rqKind rq), rqAddr rq, rqData rq, z80Done w z)
    in (z80Tick w (CpuIn d i n) z, out)
  kindBits k = case k of
    KFetch -> 0
    KRead -> 1
    KWrite -> 2
    KIn -> 3
    KOut -> 4
    KIdle -> 5
    KIntAck -> 6
{-# OPAQUE topEntity #-}

{-# ANN topEntity
  (Synthesize
    { t_name = "z80_top"
    , t_inputs = [ PortName "clk", PortName "rst", PortName "en"
                 , PortName "wait_n", PortName "din", PortName "int_n", PortName "nmi_n" ]
    , t_output = PortProduct "" [PortName "kind", PortName "addr", PortName "dout", PortName "done"]
    }) #-}
