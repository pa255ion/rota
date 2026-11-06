{-|
S/PDIF (IEC 60958, consumer) transmitter for the ADV7511's audio input
(PLAN.md step 2.9; the ZedBoard wires HD_SPDIF to it).

The line carries one biphase-mark __cell__ per 'spdifStep' with the cell
enable high; 128 cells make a frame (two 32-slot subframes, left then
right), so at 48 kHz cells come at 6.144 MHz. Per subframe:

* slots 0-3: preamble, 8 cells: B "11101000" (first subframe of a
  192-frame block), M "11100010" (other left), W "11100100" (right);
* slots 4-27: the 24-bit sample, LSB first (a 16-bit sample sits in
  slots 12-27, with zeros below);
* slot 28 V = 0 (valid), 29 U = 0, 30 C = bit /n/ of the channel status
  in frame /n/ of the block ('channelStatus'), 31 P (even parity over
  slots 4-31).

Biphase-mark: every data slot starts with a transition, a 1 adds one in
its middle. Each subframe has an even number of transitions (the parity
bit sees to it, and the preambles have four), so the line is low before
every preamble and the patterns above are absolute levels.

The sample for a frame is taken at its first cell, so the frame rate is
the sample rate.

__On the pin__ ('spdifDdr'): cells come at 6.144 MHz, a little under a
quarter of the 25.18 MHz pixel clock, so a cell boundary falls between
clock edges. The line is driven through an ODDR, which gives two levels
per pixel clock: a phase accumulator advances in half-clock steps
('cellIncrement') and 'spdifStep' runs once per half (two copies of the
logic). Transitions land within half a pixel clock (19.9 ns, 0.12 cells)
of their ideal time; the receiver recovers the clock from the stream.
-}
module Platform.Audio.Spdif
  ( SpdifState (..)
  , spdifInit
  , spdifStep
  , spdif
  , spdifDdr
  , spdifDdrStep
  , cellIncrement
  , channelStatus
  , subframeWord
  ) where

import Clash.Prelude
import Clash.Xilinx.DDR (oddr)

import Platform.Domains (DomPix, DomPixDdr)

data SpdifState = SpdifState
  { ssFrame :: Index 192
  , ssRight :: Bool          -- ^ second subframe of the frame
  , ssCell  :: Index 64      -- ^ cell within the subframe (2 per slot)
  , ssLevel :: Bit           -- ^ the line now
  , ssWord  :: BitVector 32  -- ^ the subframe being sent (slots 0-31)
  , ssR     :: Signed 16     -- ^ the right sample of this frame
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFDataX

spdifInit :: SpdifState
spdifInit = SpdifState 0 False 0 0 0 0

-- | Consumer channel status (IEC 60958-3) as sent, bit /n/ in frame /n/:
-- PCM, no copyright asserted (bit 2), category general, 48 kHz (bits
-- 24-27 = 0100 in order, i.e. bit 25 set), word length 24 bits (bit 32 =
-- 1, bits 33-35 = 101).
channelStatus :: Index 192 -> Bit
channelStatus n = boolToBit (n == 2 || n == 25 || n == 32 || n == 33 || n == 35)

-- | Slots 0-31 of a subframe (slot i at bit i; slots 0-3 unused here).
subframeWord :: Signed 16 -> Bit -> BitVector 32
subframeWord smp c = pack parity ++# body
 where
  -- slots 4-30: 8 zero bits, the sample, V, U, C
  body = (pack c ++# (0 :: BitVector 2) ++# pack smp ++# (0 :: BitVector 8) ++# (0 :: BitVector 4)) :: BitVector 31
  parity = reduceXor (slice d30 d4 body)

preamble :: Index 192 -> Bool -> BitVector 8
preamble f right
  | right = 0b11100100
  | f == 0 = 0b11101000
  | otherwise = 0b11100010

-- | One clock: the cell enable and the current stereo sample in; the next
-- state and the line level out.
spdifStep :: SpdifState -> (Bool, (Signed 16, Signed 16)) -> (SpdifState, Bit)
spdifStep s (en, (l, r))
  | not en = (s, ssLevel s)
  | otherwise = (s', out)
 where
  cell = ssCell s
  -- at a frame's first cell, take the new samples
  newFrame = not (ssRight s) && cell == 0
  w | cell == 0 = subframeWord (if ssRight s then ssR s else l) (channelStatus (ssFrame s))
    | otherwise = ssWord s
  slot = cell `shiftR` 1
  second = testBit cell 0
  out | cell < 8 = boolToBit (testBit (preamble (ssFrame s) (ssRight s)) (fromEnum (7 - cell)))
      | not second = complement (ssLevel s)
      | otherwise = if testBit w (fromEnum slot) then complement (ssLevel s) else ssLevel s
  endSub = cell == maxBound
  s' = s { ssCell = if endSub then 0 else cell + 1
         , ssRight = if endSub then not (ssRight s) else ssRight s
         , ssFrame = if endSub && ssRight s then (if ssFrame s == maxBound then 0 else ssFrame s + 1) else ssFrame s
         , ssLevel = out
         , ssWord = w
         , ssR = if newFrame then r else ssR s }

-- | The transmitter: cell enable and samples in, the S/PDIF line out
-- (registered).
spdif
  :: HiddenClockResetEnable dom
  => Signal dom Bool -> Signal dom (Signed 16, Signed 16) -> Signal dom Bit
spdif en smp = register 0 (mealy spdifStep spdifInit (bundle (en, smp)))

-- | Added per half pixel clock: round (6144000 × 19.86 ns × 2^32), cells
-- at 6.144 MHz (48 kHz × 128) with 'DomPix' (39720 ps).
cellIncrement :: Unsigned 32
cellIncrement = 524071222

-- | One pixel clock: the phase and the transmitter after both halves, and
-- the line in the first and the second half.
spdifDdrStep
  :: (Unsigned 32, SpdifState) -> (Signed 16, Signed 16)
  -> ((Unsigned 32, SpdifState), (Bit, Bit))
spdifDdrStep (acc, s) smp = ((acc2, s2), (o1, o2))
 where
  half :: Unsigned 32 -> (Unsigned 32, Bool)
  half a = let n = zeroExtend a + zeroExtend cellIncrement :: Unsigned 33
           in (truncateB n, testBit n 32)
  (acc1, c1) = half acc
  (acc2, c2) = half acc1
  (s1, o1) = spdifStep s (c1, smp)
  (s2, o2) = spdifStep s1 (c2, smp)

-- | The transmitter at half-clock resolution, for an output pin: samples
-- in, the line out of an ODDR (in the double-rate domain, like
-- HD_CLK in "Platform.Board.Zedboard").
spdifDdr
  :: Clock DomPix -> Reset DomPix -> Enable DomPix
  -> Signal DomPix (Signed 16, Signed 16) -> Signal DomPixDdr Bit
spdifDdr clk rst en smp = unpack <$> oddr clk rst en (fmap (\(a, b) -> (pack a, pack b)) halves)
 where
  halves = withClockResetEnable clk rst en $
    register (0, 0) (mealy spdifDdrStep (0, spdifInit) smp)
