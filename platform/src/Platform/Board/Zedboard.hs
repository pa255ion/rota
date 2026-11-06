{-|
ZedBoard-specific pin logic (docs/ARCHITECTURE.md): what sits between the
board-agnostic platform streams and the pins of boards/zedboard/*.xdc.

__HDMI (PLAN.md M0d step 4).__ The ADV7511 takes 16 data lines, HD_D[15:0],
wired to its D[23:8] (ZedBoard HW user's guide 2.4.1). With input ID 1,
style 1, right-justified, 8 bits per component (the register table in
"Platform.Video.Adv7511.Table"), ADV7511 Hardware User's Guide Table 7
puts Cb/Cr[7:0] on D[23:16] and Y[7:0] on D[15:8], so chroma goes to
HD_D[15:8] and luma to HD_D[7:0] ('hdmiBus'). ADI's ZedBoard reference HDL
does the same (docs/ZEDBOARD.md).

Every output is registered once more in 'hdmiOut' and placed in the IOB
(hdmi.xdc, @IOB TRUE@), so all lines leave the FPGA within a few hundred
picoseconds of each other. The pixel clock leaves through an ODDR with
D1 = 0, D2 = 1: HD_CLK is the internal clock inverted, so data changes on
HD_CLK's falling edge and the chip samples it on the rising edge half a
period (19.9 ns) later. The chip needs 1 ns set-up and 0.7 ns hold
(Hardware User's Guide, AC specifications: tVSU, tVHLD); hdmi.xdc states
those as output delays so that Vivado checks them.

__I2C.__ HD_SCL and HD_SDA are open drain: 'openDrain' drives a pin low or
releases it (high impedance) and reads it back. The Verilog has a
tri-state assignment that Vivado maps to the pin's IOBUF.
-}
module Platform.Board.Zedboard
  ( -- * HDMI
    HdmiPins (..)
  , hdmiPinsIdle
  , hdmiBus
  , hdmiOut
    -- * Open-drain pins
  , openDrain
  ) where

import Clash.Prelude
import Clash.Xilinx.DDR (oddr)

import Platform.Domains
import Platform.Video.Ycbcr422 (Ycc422 (..))

-- | Everything that goes to the ADV7511's video input in one clock
-- (HD_CLK aside).
data HdmiPins = HdmiPins
  { hpData  :: BitVector 16   -- ^ HD_D[15:0]: chroma in 15..8, Y in 7..0
  , hpDe    :: Bit            -- ^ HD_DE
  , hpHSync :: Bit            -- ^ HD_HSYNC
  , hpVSync :: Bit            -- ^ HD_VSYNC
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFDataX

-- | During reset: black, DE low, syncs at these idle levels.
hdmiPinsIdle :: Bit -> Bit -> HdmiPins
hdmiPinsIdle h v = HdmiPins 0x8010 0 h v   -- C = 128, Y = 16

-- | The bus wiring: chroma on HD_D[15:8], Y on HD_D[7:0].
hdmiBus :: Ycc422 -> HdmiPins
hdmiBus y = HdmiPins (ycC y ++# ycY y) (boolToBit (ycDe y)) (ycHSync y) (ycVSync y)

-- | The output registers and the forwarded pixel clock (HD_CLK). The
-- idle sync levels are for reset; during reset HD_CLK is held low.
hdmiOut
  :: Clock DomPix -> Reset DomPix -> Enable DomPix
  -> Bit -> Bit
  -> Signal DomPix Ycc422
  -> (Signal DomPix HdmiPins, Signal DomPixDdr Bit)
hdmiOut clk rst en h v ycc = (pins, hdClk)
 where
  pins  = withClockResetEnable clk rst en $ register (hdmiPinsIdle h v) (hdmiBus <$> ycc)
  hdClk = unpack <$> oddr clk rst en (pure (0, 1))

-- | An open-drain pin: True pulls it low, False releases it. Returns the
-- write side for the top-level port and the level read back (not
-- synchronised; the I2C master does that).
openDrain
  :: BiSignalIn 'Floating dom 1
  -> Signal dom Bool
  -> (BiSignalOut 'Floating dom 1, Signal dom Bit)
openDrain pin pullLow = (writeToBiSignal pin (drive <$> pullLow), readFromBiSignal pin)
 where
  drive :: Bool -> Maybe Bit
  drive l = if l then Just 0 else Nothing
