{-|
The S-DSP alone with its memories, for its out-of-context synthesis
(PLAN.md step 7.8; @make snes-ooc SNES_OOC=Snes.DspOoc
SNES_OOC_TOP=snes_dsp_ooc@): "Snes.Dsp" with ARAM (64 KiB), the voices'
samples (256 × 16 bits) and the Gaussian table (512 × 11 bits) as block
RAMs, its ports registered. ARAM has a second port for the SPC700's side
(here the top's inputs), as in the sound module; the SPC700 reads the
registers through a read port. Its numbers go in docs/systems/snes.md.
-}
module Snes.DspOoc
  ( topEntity
  ) where

import Clash.Prelude

import qualified Data.List as L
import qualified Data.Vector.Unboxed as U

import Platform.Domains (DomPix)
import Snes.Dsp
import Snes.Spec.Gauss (gauss)

gaussTable :: Vec 512 (Unsigned 11)
gaussTable = $(listToVecTH (L.map fromIntegral (U.toList gauss) :: [Unsigned 11]))

-- | Start a sample; a register write (enable, address, value); an ARAM
-- write from the SPC700's side (enable, address, value); a register read
-- address. Out: the sample (valid, left, right), busy, the register read.
topEntity
  :: Clock DomPix -> Reset DomPix
  -> Signal DomPix Bool
  -> Signal DomPix Bool -> Signal DomPix (Unsigned 8) -> Signal DomPix (Unsigned 8)
  -> Signal DomPix Bool -> Signal DomPix (Unsigned 16) -> Signal DomPix (Unsigned 8)
  -> Signal DomPix (Unsigned 8)
  -> Signal DomPix (Bool, Signed 16, Signed 16, Bool, Unsigned 8)
topEntity clk rst start we wa wv ae aa av ra = withClockResetEnable clk rst enableGen $
  let start' = register False start
      wr = register Nothing (mk <$> we <*> wa <*> wv)
      aw = register Nothing (mk <$> ae <*> aa <*> av)
      ra' = register 0 ra
      st = register dspInit st'
      (st', out) = unbundle (step <$> st <*> start' <*> wr <*> aramA <*> smp <*> gs)
      step c s w a sm g = dspClock c (DspIn s w a sm g)
      -- ARAM: port A the DSP's, port B the SPC700's
      (aramA, _) = trueDualPortBlockRam (aramOp <$> out) (cpuOp <$> aw)
      aramOp o = case doAramW o of
        Just v -> RamWrite (bitCoerce (doAram o)) v
        Nothing -> RamRead (bitCoerce (doAram o))
      cpuOp :: Maybe (Unsigned 16, Unsigned 8) -> RamOp 65536 (Unsigned 8)
      cpuOp m = case m of
        Just (a, v) -> RamWrite (bitCoerce a) v
        Nothing -> RamRead 0
      smp = blockRam (replicate d256 0) (doSmpRd <$> out) (doSmpW <$> out)
      gs = blockRam gaussTable (doGauss <$> out) (pure Nothing)
      flat o c r =
        let (v, (l, rr)) = maybe (False, (0, 0)) (True,) (doSample o)
        in (v, l, rr, doBusy o, dspRegRead c r)
  in register (False, 0, 0, False, 0) (flat <$> out <*> st <*> ra')
 where
  mk :: Bool -> a -> b -> Maybe (a, b)
  mk e a v = if e then Just (a, v) else Nothing
{-# NOINLINE topEntity #-}
{-# ANN topEntity
  (Synthesize
    { t_name   = "snes_dsp_ooc"
    , t_inputs = [ PortName "CLK", PortName "RST", PortName "START"
                 , PortName "WE", PortName "WA", PortName "WV"
                 , PortName "AE", PortName "AA", PortName "AV", PortName "RA" ]
    , t_output = PortProduct ""
                 [ PortName "VALID", PortName "LEFT", PortName "RIGHT", PortName "BUSY", PortName "RD" ]
    }) #-}
