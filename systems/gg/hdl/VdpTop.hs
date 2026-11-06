{-|
A synthesis check for the Clash VDP (PLAN.md step 5.6; @make gg-vdp-hdl@):
'Gg.Vdp.vdpClock' with its VRAM, four byte-wide block RAMs (4096 × 8,
interleaved by address mod 4), so Clash turns it into Verilog and
Verilator lints it. Not the Game Gear's top (step 5.9).
-}
module VdpTop (topEntity) where

import Clash.Prelude

import Gg.Vdp

-- | Per clock: the dot strobe and a port access (as tag, data); out: the
-- port read result, a window pixel (valid, x, y, colour), /INT.
topEntity
  :: Clock System -> Reset System -> Enable System
  -> Signal System Bool -> Signal System (Unsigned 3) -> Signal System (Unsigned 8)
  -> Signal System (Maybe (Unsigned 8), Bool, Unsigned 8, Unsigned 8, Unsigned 12, Bool)
topEntity clk rst en dotS tagS dataS = exposeClockResetEnable go clk rst en
 where
  go :: HiddenClockResetEnable System => Signal System (Maybe (Unsigned 8), Bool, Unsigned 8, Unsigned 8, Unsigned 12, Bool)
  go =
    let port = portOf <$> tagS <*> dataS
        vin = VdpIn <$> dotS <*> port <*> readData
        (state, out) = unbundle (mealy' vin)
        mealy' i = mealy (\s x -> let (s', o) = vdpClock x s in (s', (s', o))) (vdpPowerOn False) i
        rdAddr = voReadAddr <$> out
        wr = voWrite <$> out
        bank k = blockRam1 NoClearOnReset (SNat @4096) 0 rdAddr (bankWrite k <$> wr)
        readData = bundle (bank 0 :> bank 1 :> bank 2 :> bank 3 :> Nil)
        result o s = case voPixel o of
          Just (x, y, c) -> (voPortData o, True, x, y, c, vdpIntC s)
          Nothing -> (voPortData o, False, 0, 0, 0, vdpIntC s)
    in result <$> out <*> state
  bankWrite :: Unsigned 2 -> Maybe (Unsigned 14, Unsigned 8) -> Maybe (Unsigned 12, Unsigned 8)
  bankWrite k w = case w of
    Just (a, b) | resize a == k -> Just (resize (a `shiftR` 2), b)
    _ -> Nothing
  portOf t d = case t of
    1 -> PCtrlW d
    2 -> PDataW d
    3 -> PDataR
    4 -> PStatusR
    5 -> PVCountR
    6 -> PHCountR
    7 -> PLatchH
    _ -> PNone
{-# OPAQUE topEntity #-}

{-# ANN topEntity
  (Synthesize
    { t_name = "gg_vdp_top"
    , t_inputs = [PortName "clk", PortName "rst", PortName "en", PortName "dot", PortName "port_tag", PortName "port_data"]
    , t_output = PortProduct "" [PortName "port_out", PortName "pix_valid", PortName "pix_x", PortName "pix_y", PortName "pix_colour", PortName "int_n"]
    }) #-}
