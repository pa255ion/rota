{-|
The 65C816 core alone, for its out-of-context synthesis (PLAN.md step
7.7; @make snes-ooc SNES_OOC=W65c816.CoreOoc SNES_OOC_TOP=w65c816_ooc@):
"W65c816.Core" with the SNES's configuration, one CPU cycle each clock
that has EN (the machine's cycle end), its inputs and outputs registered.
Its numbers go in docs/systems/snes.md, for the go/no-go of step 7.7.
-}
module W65c816.CoreOoc
  ( topEntity
  ) where

import Clash.Prelude

import W65c816.Core

-- | The pixel clock's period (25.175 MHz, as Platform.Domains's DomPix;
-- a domain of its own, so the cpus package needs no platform).
createDomain vXilinxSystem{vName="DomCpu", vPeriod=39720}

-- | The request out: its kind (0 opcode, 1 operand, 2 data and 3 vector
-- reads, 4 write, 5 write with VDA low, 6 internal, 7 stopped), address,
-- data, MLB; whether it is ready (two clocks after a cycle's end); and
-- whether the cycle ended a step.
topEntity
  :: Clock DomCpu -> Reset DomCpu
  -> Signal DomCpu Bool -> Signal DomCpu (Unsigned 8)
  -> Signal DomCpu Bool -> Signal DomCpu Bool
  -> Signal DomCpu (Unsigned 3, Unsigned 24, Unsigned 8, Bool, Bool, Bool)
topEntity clk rst en d nmi irq = withClockResetEnable clk rst enableGen $
  let inp = CpuIn <$> register 0 d <*> register False nmi <*> register False irq
      en' = register False en
      next = (\c e i -> cpuClock coreW65c816 c (if e then Just i else Nothing)) <$> st <*> en' <*> inp
      st = register cpuInit (fst <$> next)
  in register (7, 0, 0, False, False, False) (flat <$> st <*> (snd <$> next))
 where
  flat c s =
    let r = cReq c
        k = case rqOp r of
          BRead KOpcode -> 0
          BRead KOperand -> 1
          BRead KData -> 2
          BRead KVector -> 3
          BWrite True -> 4
          BWrite False -> 5
          BIdle -> 6
          BStopped -> 7
    in (k, rqAddr r, rqData r, rqLock r, cStage c == StReady, s /= Nothing)
{-# NOINLINE topEntity #-}
{-# ANN topEntity
  (Synthesize
    { t_name   = "w65c816_ooc"
    , t_inputs = [ PortName "CLK", PortName "RST", PortName "EN", PortName "DATA"
                 , PortName "NMI", PortName "IRQ" ]
    , t_output = PortProduct ""
                 [ PortName "KIND", PortName "ADDR", PortName "WDATA", PortName "MLB", PortName "READY", PortName "STEP" ]
    }) #-}
