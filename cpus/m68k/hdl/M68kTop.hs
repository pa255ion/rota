{-# LANGUAGE NoImplicitPrelude #-}
{-|
The Clash 68000 with its bus as ports (side track P): @make m68k-hdl@
turns it into Verilog and Verilator lints it, @make m68k-ooc@ gives its
size and Fmax, and P.5's Verilator model (cpus/m68k/sim/: the harness and
the C library for the user's benchmark project) runs it. Not a system top:
a machine embeds the core itself. Since P.4h the core is microcoded
("M68k.Core" on "M68k.Datapath"); the ports stayed.
-}
module M68kTop (topEntity) where

import Clash.Prelude

import M68k.Bus (CpuIn (..), Kind (..), Req (..))
import M68k.Core
import qualified M68k.Datapath as C2

-- | Per clock: DTACK, BERR, VPA (active low), IPL2-IPL0 (active low), the
-- data bus, a register's number for the debug port (0-7 D0-D7, 8-15
-- A0-A7, A7 the active stack pointer; 16 USP, 17 SSP, 18 SR, 19 PC, 20
-- IR) in; out: the pending transaction's kind (0 read, 1 write, 2 TAS, 3
-- idle, 4 acknowledge), function code, width (a byte), address, write data,
-- whether this clock ends it; whether this clock starts an instruction
-- (its first transaction's first clock: not an interrupt's, the reset's,
-- or a stopped CPU's turn), whether the CPU is stopped (STOP) or halted (a
-- double fault), and the register asked for.
topEntity
  :: Clock System -> Reset System -> Enable System
  -> Signal System Bool -> Signal System Bool -> Signal System Bool
  -> Signal System (BitVector 3) -> Signal System (Unsigned 16) -> Signal System (Unsigned 5)
  -> Signal System ( BitVector 3, Unsigned 3, Bool, Unsigned 24, Unsigned 16, Bool
                   , Bool, Bool, Bool, Unsigned 32 )
topEntity clk rst en dtackN berrN vpaN iplN din dbgSel =
  exposeClockResetEnable (mealy step m68kPowerOn (bundle (dtackN, berrN, vpaN, iplN, din, dbgSel))) clk rst en
 where
  step t (dn, bn, vn, ipl, d, sel) =
    let c = cC (tCpu t)
        rq = C2.cReq c
        ack = not dn || not bn || not vn
        ci = CpuIn { ciData = d, ciBerr = not bn, ciIpl = unpack (complement ipl), ciAuto = not vn }
        k = C2.cK c
        -- an instruction's first transaction (not an exception's, a
        -- stopped or halted CPU's turn)
        istart = C2.kNew k && tClk t == 0 && not (C2.kExc k) && not (C2.kStopped k) && not (C2.kHalted k)
        out = ( kindBits (rqKind rq), rqFc rq, rqByte rq, rqAddr rq, rqData rq, m68kDone ack t
              , istart, C2.kStopped k, C2.kHalted k, dbg sel c )
    in (m68kTick ack ci t, out)
  kindBits k = case k of
    KRead -> 0
    KWrite -> 1
    KTas -> 2
    KIdle -> 3
    KIack -> 4
  dbg sel c
    | sel < 16 = C2.cRf c !! sel
    | otherwise = case sel of
        16 -> C2.usp c
        17 -> C2.ssp c
        18 -> resize (C2.cSr c)
        19 -> C2.cPc c
        20 -> resize (C2.cIr c)
        _ -> 0
{-# OPAQUE topEntity #-}

{-# ANN topEntity
  (Synthesize
    { t_name = "m68k_top"
    , t_inputs = [ PortName "clk", PortName "rst", PortName "en"
                 , PortName "dtack_n", PortName "berr_n", PortName "vpa_n", PortName "ipl_n", PortName "din"
                 , PortName "dbg_sel" ]
    , t_output = PortProduct "" [ PortName "kind", PortName "fc", PortName "is_byte", PortName "addr"
                                , PortName "dout", PortName "done", PortName "istart", PortName "stopped"
                                , PortName "halted", PortName "dbg" ]
    }) #-}
