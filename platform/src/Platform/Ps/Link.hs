{-|
The PS ↔ PL link over EMIO GPIO (milestone 1b, shared by every system that
uses the PS; the PS side is @platform/ps/@). No AXI: the PS drives 32 EMIO
GPIO lines into the PL (bank 2, 'PsGpio') and reads 32 back (bank 3,
'PsIn').

__PS → PL__ (EMIO GPIO 0 to 31):

* bits 0 to 15: system-specific live inputs (CHIP-8: the 16 keys);
* bits 16 to 23: a data byte;
* bit 24: strobe, toggled once per byte;
* bit 25: the byte is an address byte;
* bit 26: hold (the system keeps its core in reset while it is high).

Each strobe toggle takes the data byte: an address byte shifts into the
24-bit write address (the PS always sends three, high byte first), any
other byte is a write at the address, which then advances ('linkStep').
Writes come out whether or not hold is high; each system decides which of
its address ranges it accepts while running ("Chip8.Loader": main RAM only
while held). The address map is per system, except the shared top
range, 'sharedBase' and up: the menu text overlay ('overlayWrite') and the
link control register ('controlAddr', 'PsControl') and the settings
registers ('volumeAddr', 'speedAddr', 'PsSettings').

__PL → PS__ (EMIO GPIO 32 to 63, 'psIn'): the debounced board buttons, the
switches and a system ID, so the PS app can drive a menu with the buttons
and knows which system's bitstream is loaded; and a byte channel
('psByteOut', 'psInWith'): the last byte a system sent (bits 24 to 31) and
a toggle (bit 5) that flips once per byte, for text a system writes, such
as the Game Boy's serial port (Blargg's results). At most one byte per
millisecond or so: the PS polls in microseconds and keeps no FIFO.

All PS → PL lines are asynchronous to the system clock and synchronised bit
by bit before they reach 'linkStep'. The PS changes the data bits about
2 µs before toggling the strobe, so they have long settled when the
synchronised strobe edge arrives.
-}
module Platform.Ps.Link
  ( -- * PS → PL
    PsGpio (..)
  , psGpio
  , LinkState (..)
  , linkInit
  , linkStep
  , psLink
    -- * Shared address range
  , LinkAddr
  , sharedBase
  , overlayBase
  , overlayWrite
  , controlAddr
  , PsControl (..)
  , psControlInit
  , psControlStep
  , psControl
  , volumeAddr
  , speedAddr
  , Volume
  , SpeedQuarters
  , PsSettings (..)
  , psSettingsInit
  , psSettingsStep
  , psSettings
    -- * PL → PS
  , SystemId
  , systemChip8
  , systemGameBoy
  , systemNes
  , systemM0e
  , systemGameBoyColor
  , psIn
  , PsByteOut (..)
  , psByteOutInit
  , psByteOutStep
  , psByteOut
  , psInWith
  ) where

import Clash.Prelude

-- | The PS → PL lines, decoded.
data PsGpio = PsGpio
  { pgUser    :: BitVector 16   -- ^ system-specific (CHIP-8: keys)
  , pgData    :: BitVector 8
  , pgStrobe  :: Bool
  , pgAddrSel :: Bool
  , pgHold    :: Bool
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFDataX

psGpio :: BitVector 32 -> PsGpio
psGpio w = PsGpio
  { pgUser    = slice d15 d0 w
  , pgData    = slice d23 d16 w
  , pgStrobe  = testBit w 24
  , pgAddrSel = testBit w 25
  , pgHold    = testBit w 26
  }

-- | A write address on the link.
type LinkAddr = Unsigned 24

data LinkState = LinkState
  { lsStrobe :: Bool       -- ^ strobe level last cycle
  , lsAddr   :: LinkAddr   -- ^ next write address
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFDataX

linkInit :: LinkState
linkInit = LinkState False 0

-- | One cycle: the write to make, if any.
linkStep :: LinkState -> PsGpio -> (LinkState, Maybe (LinkAddr, BitVector 8))
linkStep s g
  | not edge    = (s, Nothing)
  | pgAddrSel g = (s' { lsAddr = (lsAddr s `shiftL` 8) .|. resize (unpack (pgData g)) }, Nothing)
  | otherwise   = (s' { lsAddr = lsAddr s + 1 }, Just (lsAddr s, pgData g))
 where
  edge = pgStrobe g /= lsStrobe s
  s' = s { lsStrobe = pgStrobe g }

-- | The writes from synchronised link lines.
psLink
  :: HiddenClockResetEnable dom
  => Signal dom PsGpio -> Signal dom (Maybe (LinkAddr, BitVector 8))
psLink = mealy linkStep linkInit

-- | Addresses from here up are the same on every system; below it is the
-- system's own map (memories to load).
sharedBase :: LinkAddr
sharedBase = 0xF00000

-- | The menu text overlay ("Platform.Video.TextOverlay"): one byte per
-- character cell, row by row, 40 × 30 cells.
overlayBase :: LinkAddr
overlayBase = 0xF00000

-- | A write into the overlay's character RAM, if the address is in it.
overlayWrite :: Maybe (LinkAddr, BitVector 8) -> Maybe (Index 1200, BitVector 8)
overlayWrite (Just (a, d))
  | a >= overlayBase && a < overlayBase + 1200 = Just (unpack (resize (pack (a - overlayBase))), d)
overlayWrite _ = Nothing

-- | The control register, 'PsControl': bit 0 shows the overlay, bit 1
-- pauses the core (no instructions, inputs ignored, state kept).
controlAddr :: LinkAddr
controlAddr = 0xF01000

data PsControl = PsControl
  { pcOverlay :: Bool
  , pcPause   :: Bool
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFDataX

psControlInit :: PsControl
psControlInit = PsControl False False

psControlStep :: PsControl -> Maybe (LinkAddr, BitVector 8) -> PsControl
psControlStep _ (Just (a, d)) | a == controlAddr = PsControl (testBit d 0) (testBit d 1)
psControlStep c _ = c

-- | The control register, written over the link (registered: the new value
-- is out one clock after the write).
psControl
  :: HiddenClockResetEnable dom
  => Signal dom (Maybe (LinkAddr, BitVector 8)) -> Signal dom PsControl
psControl = moore psControlStep id psControlInit

-- | The settings registers (side track L, the in-game menu's options;
-- reserved in docs/ARCHITECTURE.md): the volume at 'volumeAddr', the
-- emulation speed at 'speedAddr'. A separate register from 'PsControl',
-- so a top that doesn't instantiate 'psSettings' is unchanged.
volumeAddr, speedAddr :: LinkAddr
volumeAddr = 0xF01001
speedAddr  = 0xF01002

-- | The volume level: 0 is silence, 10 the level every system had before
-- (unity gain; "Platform.Audio.Volume" maps the levels to gains).
type Volume = Unsigned 4

-- | The emulation speed in quarters of the real machine's: 2 is 50 %,
-- 4 is 100 %, 8 is 200 % (the highest; each system's PS table entry says
-- how fast its core can really go).
type SpeedQuarters = Unsigned 4

data PsSettings = PsSettings
  { psVolume :: Volume
  , psSpeed  :: SpeedQuarters
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFDataX

-- | Neutral: unity volume, 100 % speed, so a PS app that never writes the
-- registers (older cards, the layer 4 goldens) changes nothing.
psSettingsInit :: PsSettings
psSettingsInit = PsSettings 10 4

-- | Out-of-range bytes are clamped: a volume above 10 is 10, a speed
-- below 2 is 2 and above 8 is 8.
psSettingsStep :: PsSettings -> Maybe (LinkAddr, BitVector 8) -> PsSettings
psSettingsStep s (Just (a, d))
  | a == volumeAddr = s { psVolume = resize (min 10 v) }
  | a == speedAddr  = s { psSpeed = resize (max 2 (min 8 v)) }
  where v = unpack d :: Unsigned 8
psSettingsStep s _ = s

-- | The settings registers, written over the link (registered: the new
-- value is out one clock after the write).
psSettings
  :: HiddenClockResetEnable dom
  => Signal dom (Maybe (LinkAddr, BitVector 8)) -> Signal dom PsSettings
psSettings = moore psSettingsStep id psSettingsInit

-- | Which system a bitstream holds, for the PS app.
type SystemId = BitVector 8

systemChip8, systemGameBoy :: SystemId
systemChip8   = 1
systemGameBoy = 2

-- | The NES (M3 step 3.8; reserved in docs/ARCHITECTURE.md).
systemNes :: SystemId
systemNes = 3

-- | The DDR test top, M0e (side track C; reserved in docs/ARCHITECTURE.md).
systemM0e :: SystemId
systemM0e = 4

-- | The Game Boy Color (side track I, phase 3; reserved in
-- docs/ARCHITECTURE.md).
systemGameBoyColor :: SystemId
systemGameBoyColor = 6

-- | The PL → PS word (EMIO GPIO 32 to 63): bits 0 to 4 the debounced
-- buttons BTNC, BTND, BTNL, BTNR, BTNU (1 = pressed), bits 8 to 15 the
-- switches, bits 16 to 23 the system ID, the rest 0 (no byte channel).
psIn :: SystemId -> Vec 5 Bool -> BitVector 8 -> BitVector 32
psIn sys btns sw = psInWith sys btns sw psByteOutInit

-- | The byte channel's state: the last byte, and a toggle that flips with
-- every byte.
data PsByteOut = PsByteOut
  { pbByte   :: BitVector 8
  , pbToggle :: Bool
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFDataX

psByteOutInit :: PsByteOut
psByteOutInit = PsByteOut 0 False

psByteOutStep :: PsByteOut -> Maybe (BitVector 8) -> PsByteOut
psByteOutStep s Nothing  = s
psByteOutStep s (Just b) = PsByteOut b (not (pbToggle s))

-- | Bytes for the PS (registered: out one clock after they come in).
psByteOut
  :: HiddenClockResetEnable dom
  => Signal dom (Maybe (BitVector 8)) -> Signal dom PsByteOut
psByteOut = moore psByteOutStep id psByteOutInit

-- | 'psIn' with the byte channel: its byte in bits 24 to 31, its toggle in
-- bit 5.
psInWith :: SystemId -> Vec 5 Bool -> BitVector 8 -> PsByteOut -> BitVector 32
psInWith sys btns sw (PsByteOut b t) =
  b ++# sys ++# sw ++# (0 :: BitVector 2) ++# boolToBV t ++# pack (reverse btns)
