{-# LANGUAGE AllowAmbiguousTypes #-}
{-|
Driver for the ZedBoard's onboard 128×32 OLED (UG-2832HSWEG04, SSD1306
controller, 4-wire SPI, write only; docs/ZEDBOARD.md, PLAN.md M0 step 7).

After reset the driver powers the panel up:

1. VDD on, wait 'otShort';
2. RES# low for 'otShort', then high, wait 'otShort';
3. display off, charge pump on, pre-charge period;
4. VBAT on, wait 'otLong' (the SSD1306 wants 100 ms before display on);
5. contrast, COM pin configuration, horizontal addressing mode, display on.

It then streams frames forever: each frame is a short command block
(segment remap and COM scan direction from 'OledConfig', column range
0 to 127, page range 0 to 3) followed by the 512 bytes of the framebuffer.
This is the sequence of Digilent's Pmod OLED reference driver (same panel),
with the datasheet's order of reset and first command.

__Framebuffer read port.__ The driver outputs a byte address @a@ and
expects the byte at @a@ on its data input one cycle later or any time after
(a block RAM read port fits). Byte @a = 128 * page + column@ holds the
pixels @x = column@, @y = 8 * page + b@ for bits @b = 0..7@ (bit 0 on top),
1 = lit. The address changes only right after a data byte has been loaded,
and the next load is a whole SPI byte (64 clocks) later.

__SPI.__ Mode 3: SCLK idles high; SDIN (MSB first) and D/C# change with the
falling edge, the panel samples on the rising edge. Each half of an SCLK
period is 'otHalfBit' clocks. CS# is tied low on the board, so the RES#
pulse is what aligns the panel's bit counter.

__Pins.__ Every pin comes straight from a register in 'OledState'.
@OLED_VDD@ and @OLED_VBAT@ are active low (P-FETs with gate pull-ups), so
during reset (and before configuration) both supplies are off.
-}
module Platform.Video.Oled
  ( OledTiming (..)
  , oledTiming
  , OledConfig (..)
  , oledConfigDefault
  , OledPins (..)
  , OledState (..)
  , Op (..)
  , oledProgram
  , oledLoopStart
  , oledInit
  , oledPins
  , oledStep
  , oledDriver
  , oledFrameBytes
  ) where

import Clash.Prelude

-- | Delays and SPI speed, in clocks of the driver's domain.
data OledTiming = OledTiming
  { otShort   :: Unsigned 24  -- ^ power/reset settling, at least 1 ms
  , otLong    :: Unsigned 24  -- ^ VBAT on to display on, at least 100 ms
  , otHalfBit :: Unsigned 8   -- ^ clocks per SCLK half period, at least 1
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFDataX

-- | The real timing for domain @dom@: 1 ms, 100 ms and 4 clocks per half
-- bit (SCLK = clock / 8, 3.1 MHz at the 25.18 MHz pixel clock; the SSD1306
-- allows 10 MHz).
oledTiming :: forall dom. KnownDomain dom => OledTiming
oledTiming = OledTiming
  { otShort   = 1 + ms 1
  , otLong    = 1 + ms 100
  , otHalfBit = 4
  }
 where
  period = snatToNum (clockPeriod @dom) :: Integer
  ms n   = fromInteger ((n * 1000000000) `div` period)

-- | Orientation, re-sent with every frame so it can change at run time.
data OledConfig = OledConfig
  { ocSegRemap :: Bool  -- ^ column 127 drives SEG0 (command A1 instead of A0)
  , ocComRemap :: Bool  -- ^ scan COM from the top down (C8 instead of C0)
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFDataX

-- | Digilent's setting for this panel (A1, C8).
oledConfigDefault :: OledConfig
oledConfigDefault = OledConfig True True

data OledPins = OledPins
  { opDc   :: Bit  -- ^ OLED_DC: 0 = command, 1 = data
  , opRes  :: Bit  -- ^ OLED_RES: active low
  , opSclk :: Bit  -- ^ OLED_SCLK
  , opSdin :: Bit  -- ^ OLED_SDIN
  , opVbat :: Bit  -- ^ OLED_VBAT: 0 = on
  , opVdd  :: Bit  -- ^ OLED_VDD: 0 = on
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFDataX

-- | One step of the start-up and frame program.
data Op
  = OpVddOn
  | OpVbatOn
  | OpResLow
  | OpResHigh
  | OpWaitShort
  | OpWaitLong
  | OpCmd (BitVector 8)  -- ^ send a command byte
  | OpSegRemap           -- ^ send A0 or A1 from 'OledConfig'
  | OpComScan            -- ^ send C0 or C8 from 'OledConfig'
  | OpFrame              -- ^ send the 512 framebuffer bytes
  | OpLoop               -- ^ continue at 'oledLoopStart'
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFDataX

type ProgLen = 30

oledProgram :: Vec ProgLen Op
oledProgram =
     OpVddOn :> OpWaitShort
  :> OpResLow :> OpWaitShort :> OpResHigh :> OpWaitShort
  :> OpCmd 0xAE                      -- display off
  :> OpCmd 0x8D :> OpCmd 0x14        -- charge pump on
  :> OpCmd 0xD9 :> OpCmd 0xF1        -- pre-charge period
  :> OpVbatOn :> OpWaitLong
  :> OpCmd 0x81 :> OpCmd 0x0F        -- contrast
  :> OpCmd 0xDA :> OpCmd 0x20        -- COM pins: sequential, left/right remap
  :> OpCmd 0x20 :> OpCmd 0x00        -- horizontal addressing mode
  :> OpCmd 0xAF                      -- display on
  -- frame loop (oledLoopStart)
  :> OpSegRemap :> OpComScan
  :> OpCmd 0x21 :> OpCmd 0x00 :> OpCmd 0x7F  -- columns 0 to 127
  :> OpCmd 0x22 :> OpCmd 0x00 :> OpCmd 0x03  -- pages 0 to 3
  :> OpFrame :> OpLoop
  :> Nil

oledLoopStart :: Index ProgLen
oledLoopStart = 20

-- | Bytes per frame: 4 pages of 128 columns.
oledFrameBytes :: Int
oledFrameBytes = 512

data OledState = OledState
  { osPc    :: Index ProgLen
  , osWait  :: Unsigned 24   -- ^ clocks left in a wait
  , osBits  :: Index 9       -- ^ bits of the current byte left to send
  , osPhase :: Unsigned 8    -- ^ clock within the current bit
  , osShift :: BitVector 8   -- ^ the bit being sent is the MSB
  , osAddr  :: Index 512     -- ^ next framebuffer byte (the read address)
  , osPins  :: OledPins
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFDataX

-- | Reset state: both supplies off, RES# and SCLK high.
oledInit :: OledState
oledInit = OledState
  { osPc = 0, osWait = 0, osBits = 0, osPhase = 0, osShift = 0, osAddr = 0
  , osPins = OledPins { opDc = 0, opRes = 1, opSclk = 1, opSdin = 0, opVbat = 1, opVdd = 1 }
  }

oledPins :: OledState -> OledPins
oledPins = osPins

-- | One clock. Inputs: the orientation and the framebuffer byte at the
-- address of the previous cycle.
oledStep :: OledTiming -> OledState -> (OledConfig, BitVector 8) -> OledState
oledStep t s (cfg, fbByte)
  | osBits s /= 0  = shiftBit
  | osWait s /= 0  = s { osWait = osWait s - 1 }
  | otherwise      = case oledProgram !! osPc s of
      OpVddOn     -> next (pins { opVdd = 0 })
      OpVbatOn    -> next (pins { opVbat = 0 })
      OpResLow    -> next (pins { opRes = 0 })
      OpResHigh   -> next (pins { opRes = 1 })
      OpWaitShort -> (next pins) { osWait = otShort t }
      OpWaitLong  -> (next pins) { osWait = otLong t }
      OpCmd b     -> send 0 b (osPc s + 1) (osAddr s)
      OpSegRemap  -> send 0 (0xA0 .|. boolBit (ocSegRemap cfg)) (osPc s + 1) (osAddr s)
      OpComScan   -> send 0 (0xC0 .|. (if ocComRemap cfg then 0x08 else 0)) (osPc s + 1) (osAddr s)
      OpFrame
        | osAddr s == maxBound -> send 1 fbByte (osPc s + 1) 0
        | otherwise            -> send 1 fbByte (osPc s) (osAddr s + 1)
      OpLoop      -> s { osPc = oledLoopStart }
 where
  pins = osPins s
  next p = s { osPc = osPc s + 1, osPins = p }
  boolBit b = if b then 1 else 0

  -- Start a byte: SCLK falls and the MSB appears on SDIN together.
  send dc b pc addr = s
    { osPc = pc, osAddr = addr, osBits = 8, osPhase = 0, osShift = b
    , osPins = pins { opDc = dc, opSclk = 0, opSdin = msbit b } }

  -- Within a bit: SCLK rises after otHalfBit clocks; after two halves the
  -- next bit starts (SCLK falls, SDIN changes) or, after the last one,
  -- SCLK stays high.
  shiftBit
    | osPhase s + 1 == 2 * otHalfBit t =
        let sh   = osShift s `shiftL` 1
            more = osBits s /= 1
        in s { osBits = osBits s - 1, osPhase = 0, osShift = sh
             , osPins = if more then pins { opSclk = 0, opSdin = msbit sh } else pins }
    | osPhase s + 1 == otHalfBit t =
        s { osPhase = osPhase s + 1, osPins = pins { opSclk = 1 } }
    | otherwise = s { osPhase = osPhase s + 1 }

  msbit :: BitVector 8 -> Bit
  msbit = (! (7 :: Int))

-- | The driver. Returns the framebuffer read address and the pins (all
-- registered).
oledDriver
  :: HiddenClockResetEnable dom
  => OledTiming
  -> Signal dom OledConfig
  -> Signal dom (BitVector 8)   -- ^ framebuffer byte at last cycle's address
  -> (Signal dom (Index 512), Signal dom OledPins)
oledDriver t cfg fb = (osAddr <$> st, osPins <$> st)
 where
  st = register oledInit (oledStep t <$> st <*> bundle (cfg, fb))
