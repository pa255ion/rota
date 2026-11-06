{-|
The whole machine, for an out-of-context synthesis that keeps the
hierarchy (the size step, after 7.13; @make snes-ooc
SNES_OOC=Snes.MachineOoc SNES_OOC_TOP=snes_machine_ooc
SNES_OOC_MODE=hier@): "Snes.Machine" as the boards' tops use it (the PS
link, the DDR port, the line ring and its stream, the overlay, the
sound's filter), without the board's clocks, TMDS encoders and pins; its
inputs and outputs registered. The core's parts ("Snes.Core": the 5A22,
the PPU, the sound module) are components of their own, so each gets a
row in utilization-hier.txt. Its numbers go in docs/systems/snes.md.
-}
module Snes.MachineOoc
  ( topEntity
  ) where

import Clash.Prelude

import Platform.Domains (DomPix)
import Platform.Mem.Axi
import Platform.Ps.Link (psGpio)
import Platform.Video.VgaTiming (Rgb888 (..), VgaOut (..))
import Snes.Machine
import Platform.SaveState (StateHooks (..))

-- | In: the pad (12 bits, pressed), PS_GPIO, the HP port's read side.
-- Out: the syncs and active flag, the colour, the sound, the HP port's
-- read address side, the frame toggle, a late row, SRAM's byte channel
-- (valid, byte), the dirty flag.
topEntity
  :: Clock DomPix -> Reset DomPix
  -> Signal DomPix (Unsigned 12) -> Signal DomPix (BitVector 32)
  -> Signal DomPix Bit -> Signal DomPix Bit -> Signal DomPix (BitVector 64) -> Signal DomPix (BitVector 2) -> Signal DomPix Bit
  -> Signal DomPix ( (Bit, Bit, Bool, BitVector 24, Signed 16, Signed 16)
                   , (Bool, BitVector 32, BitVector 4, Bool, Bool, Bool, Bool, BitVector 8, Bool) )
topEntity clk rst pad psRaw arReady rValid rDat rRsp rLst =
  withClockResetEnable clk rst enableGen $
  let r :: NFDataX a => a -> Signal DomPix a -> Signal DomPix a
      r = register
      s2m = (\a v d rs l -> AxiRdS2M (a == 1) (if v == 1 then Just (AxiR d rs (l == 1)) else Nothing))
              <$> r 0 arReady <*> r 0 rValid <*> r 0 rDat <*> r 0 rRsp <*> r 0 rLst
      m = snesMachine NoStateHooks (r 0 pad) (psGpio <$> r 0 psRaw) s2m
      flat t c (al, ar) ax f late b d =
        ( (vgaHSync t, vgaVSync t, vgaActive t, rgb8R c ++# rgb8G c ++# rgb8B c, al, ar)
        , ( maybe False (const True) (m2sAr ax), maybe 0 (pack . arAddr) (m2sAr ax), maybe 0 (pack . arLen) (m2sAr ax)
          , m2sRReady ax, f, late /= 0, maybe False (const True) b, maybe 0 id b, d ) )
  in register ((0, 0, False, 0, 0, 0), (False, 0, 0, False, False, False, False, 0, False))
       (flat <$> snTiming m <*> snRgb m <*> snAudio m <*> snAxi m <*> snFrame m <*> snLate m <*> snByte m <*> snDirty m)
{-# NOINLINE topEntity #-}
{-# ANN topEntity
  (Synthesize
    { t_name   = "snes_machine_ooc"
    , t_inputs = [ PortName "CLK", PortName "RST", PortName "PAD", PortName "PS_GPIO"
                 , PortName "AR_READY", PortName "R_VALID", PortName "R_DATA", PortName "R_RESP", PortName "R_LAST" ]
    , t_output = PortProduct ""
                 [ PortProduct "" [ PortName "HSYNC", PortName "VSYNC", PortName "ACTIVE", PortName "RGB", PortName "AUD_L", PortName "AUD_R" ]
                 , PortProduct "" [ PortName "AR_VALID", PortName "AR_ADDR", PortName "AR_LEN", PortName "R_READY"
                                  , PortName "FRAME", PortName "LATE", PortName "BYTE_V", PortName "BYTE", PortName "DIRTY" ] ]
    }) #-}
