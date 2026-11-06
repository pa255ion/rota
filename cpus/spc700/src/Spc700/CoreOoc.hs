{-|
The SPC700 core alone, for its out-of-context synthesis (PLAN.md step
7.8; @make snes-ooc SNES_OOC=Spc700.CoreOoc SNES_OOC_TOP=spc700_ooc@):
"Spc700.Core", one bus cycle each clock that has EN (the sound module's
cycle end), its inputs and outputs registered. Its numbers go in
docs/systems/snes.md ("Measured").
-}
module Spc700.CoreOoc
  ( topEntity
  ) where

import Clash.Prelude

import Spc700.Core

-- | The pixel clock's period (25.175 MHz, as Platform.Domains's DomPix;
-- a domain of its own, so the cpus package needs no platform).
createDomain vXilinxSystem{vName="DomSpc", vPeriod=39720}

-- | The request out: its kind (0 read, 1 write, 2 internal), address and
-- data; whether the last cycle ended an instruction.
topEntity
  :: Clock DomSpc -> Reset DomSpc
  -> Signal DomSpc Bool -> Signal DomSpc (Unsigned 8)
  -> Signal DomSpc (Unsigned 2, Unsigned 16, Unsigned 8, Bool)
topEntity clk rst en d = withClockResetEnable clk rst enableGen $
  let d' = register 0 d
      en' = register False en
      next = (\c e v -> if e then cpuCycle c v else (c, CpuOut (cpuReq c) False)) <$> st <*> en' <*> d'
      st = register cpuInit (fst <$> next)
  in register (2, 0, 0, False) (flat <$> st <*> (snd <$> next))
 where
  flat c o = case cpuReq c of
    RRead a -> (0, a, 0, coStep o)
    RWrite a v -> (1, a, v, coStep o)
    RIdle -> (2, 0, 0, coStep o)
{-# NOINLINE topEntity #-}
{-# ANN topEntity
  (Synthesize
    { t_name   = "spc700_ooc"
    , t_inputs = [ PortName "CLK", PortName "RST", PortName "EN", PortName "DATA" ]
    , t_output = PortProduct "" [ PortName "KIND", PortName "ADDR", PortName "WDATA", PortName "STEP" ]
    }) #-}
