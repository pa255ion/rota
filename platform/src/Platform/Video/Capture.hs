{-|
Screenshots (side track O, O.4; PLAN.md "Side track O"): one whole frame,
at the system's own resolution, written into DDR through the HP port's
write channels, where the PS reads it and makes a PNG.

The PS asks for one by writing a sequence number (1 to 255) to the capture
register ('captureAddr'). From the core's next frame start (its first
pixel, index 0: so a core paused at a frame's end by frame advance gives
its next frame in one step), every pixel it writes into its frame buffer
(the same writes, in the same order, with the colour the screen shows,
'Rgb24') goes to DDR as well, at
@'captureBase' + 4 * index@: bytes R, G, B, 0. At that frame's end
("frame done") the capture stops, and a trailer word follows the picture (at
@'captureBase' + 4 * width * height@, rounded up to 8): the sequence
number, a status byte (bit 0: some pixels were lost, the writes fell
behind), the width, the height and how many pixels were written
('trailerWord'). It is written last, after every pixel's write was
answered, so the PS knows the picture is complete once the trailer carries
its sequence number.

Pixels are packed two to a 64-bit beat; each beat is one AXI burst of one
beat (byte strobes for a half-filled one), up to 15 in flight. A 16-beat
FIFO between the packer and the writer takes up the DDR's latency;
pixels come at most one every few clocks (a core's dot rate against the
pixel clock), beats half as often. Built with @SCREENSHOTS=0@
('screenshotsEnabled' false) there is no capture: the write channels idle.
-}
module Platform.Video.Capture
  ( captureAddr
  , captureBase
  , Rgb24
  , Mode (..)
  , Cap (..)
  , capInit
  , capStep
  , trailerAddr
  , trailerWord
  , frameCapture
  ) where

import Clash.Prelude

import Platform.Features (screenshotsEnabled)
import Platform.Mem.Axi
import Platform.Ps.Link (LinkAddr)

-- | The capture register: a write of a sequence number (not 0) asks for a
-- screenshot (reserved in docs/ARCHITECTURE.md beside the movie register).
captureAddr :: LinkAddr
captureAddr = 0xF01004

-- | Where the picture goes in DDR (docs/ARCHITECTURE.md, "Resource
-- budgets": above the running system's media).
captureBase :: AxiAddr
captureBase = 0x10000000

-- | A pixel's colour: R in bits 23 to 16, G 15 to 8, B 7 to 0.
type Rgb24 = BitVector 24

type Beat = (AxiAddr, AxiData, BitVector 8)

data Mode = Idle | Armed | Capturing | Ending | Flushing
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFDataX

data Cap = Cap
  { cMode     :: Mode
  , cSeq      :: BitVector 8
  , cOverflow :: Bool
  , cCount    :: Unsigned 24
  , cBeat     :: Maybe Beat           -- ^ the beat being packed
  , cFifo     :: Vec 16 Beat
  , cHead     :: Index 16
  , cLen      :: Index 17
  , cAwDone   :: Bool                 -- ^ the head beat's AW taken
  , cWDone    :: Bool                 -- ^ its W taken
  , cOut      :: Unsigned 5           -- ^ bursts waiting for their B
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFDataX

capInit :: Cap
capInit = Cap Idle 0 False 0 Nothing (repeat (0, 0, 0)) 0 0 False False 0

-- | The trailer's address for a picture of @w * h@ pixels.
trailerAddr :: Unsigned 12 -> Unsigned 12 -> AxiAddr
trailerAddr w h = captureBase + ((4 * resize w * resize h + 7) .&. complement 7)

-- | The trailer: sequence number (bits 7 to 0), status (15 to 8), width
-- (27 to 16), height (39 to 28), pixels written (63 to 40).
trailerWord :: BitVector 8 -> Bool -> Unsigned 12 -> Unsigned 12 -> Unsigned 24 -> AxiData
trailerWord sq ovf w h n =
  pack n ++# pack h ++# pack w ++# (0 :: BitVector 7) ++# pack ovf ++# sq

-- | A pixel's beat: its address, and its four bytes in the half the index
-- picks.
pixelBeat :: Unsigned 24 -> Rgb24 -> Beat
pixelBeat i c = (addr, d, strb)
 where
  addr = captureBase + 8 * resize (i `shiftR` 1)
  (r, g, b) = (slice d23 d16 c, slice d15 d8 c, slice d7 d0 c) :: (BitVector 8, BitVector 8, BitVector 8)
  word = (0 :: BitVector 8) ++# b ++# g ++# r
  hi = testBit i 0
  d = if hi then word ++# (0 :: BitVector 32) else (0 :: BitVector 32) ++# word
  strb = if hi then 0xF0 else 0x0F

-- | A beat merged into the one being packed (same address).
mergeBeat :: Beat -> Beat -> Beat
mergeBeat (a, d0, s0) (_, d1, s1) = (a, d0 .|. d1, s0 .|. s1)

-- | One clock: (the link's write, the frame end, the pixel written, the
-- slave's outputs) to the writer's AXI outputs and whether a capture is
-- under way. @w@ and @h@: the picture's size (the trailer's).
capStep
  :: Unsigned 12 -> Unsigned 12
  -> Cap
  -> (Maybe (LinkAddr, BitVector 8), Bool, Maybe (Unsigned 24, Rgb24), AxiWrS2M)
  -> (Cap, (AxiWrM2S, Bool))
capStep w h s (link, frameEnd, pixel, s2m) = (s', (m2s, cMode s /= Idle))
 where
  -- the writer: the FIFO's head as a one-beat burst
  (ha, hd, hs) = cFifo s !! cHead s
  present = cLen s > 0 && (cAwDone s || cWDone s || cOut s < 15)
  aw = if present && not (cAwDone s) then Just (AxiAw ha 0) else Nothing
  wr = if present && not (cWDone s) then Just (AxiW hd hs True) else Nothing
  m2s = AxiWrM2S aw wr True
  awDone = cAwDone s || (aw /= Nothing && s2mAwReady s2m)
  wDone = cWDone s || (wr /= Nothing && s2mWReady s2m)
  popped = present && awDone && wDone
  out1 = cOut s + (if popped then 1 else 0) - (if s2mB s2m /= Nothing then 1 else 0)
  head1 = if popped then satSucc SatWrap (cHead s) else cHead s
  len1 = if popped then cLen s - 1 else cLen s
  -- room for a push at the FIFO's tail
  room = len1 < 16
  tailIx = fromIntegral ((fromIntegral head1 + fromIntegral len1 :: Unsigned 5) .&. 15) :: Index 16

  -- the request, and the capture's course
  req = case link of
    Just (a, d) | a == captureAddr && d /= 0 && cMode s == Idle -> Just d
    _ -> Nothing

  -- the frame's first pixel starts a capture (and is taken)
  first = case pixel of
    Just (0, _) -> True
    _ -> False
  capturing = cMode s == Capturing || (cMode s == Armed && first)
  -- the pixel, while capturing: merged into the beat being packed, or that
  -- beat pushed (lost if no room) and a new one begun
  (pixBeat, pixPush, pixCount, pixLost) = case pixel of
    Just (i, c) | capturing ->
      let pb@(pa, _, _) = pixelBeat i c
          cnt = cCount s + 1
      in case cBeat s of
           Just ob@(oa, _, _)
             | oa == pa -> (Just (mergeBeat ob pb), Nothing, cnt, False)
             | room -> (Just pb, Just ob, cnt, False)
             | otherwise -> (Just pb, Nothing, cnt, True)
           Nothing -> (Just pb, Nothing, cnt, False)
    _ -> (cBeat s, Nothing, cCount s, False)
  trailer = (trailerAddr w h, trailerWord (cSeq s) (cOverflow s) w h (cCount s), 0xFF)
  awDone' = awDone && not popped
  wDone' = wDone && not popped

  -- each mode decides the next one, the beat being packed and at most one
  -- beat to push; the FIFO takes it once, below (one write port: a copy of
  -- the whole FIFO a branch cost the GBC about 14,500 LUTs, make bit)
  same = (cMode s, cSeq s, cBeat s, Nothing, cCount s, cOverflow s)
  (mode', seq', beat', pushB, count', ovf') = case cMode s of
    Idle -> case req of
      Just sq -> (Armed, sq, Nothing, Nothing, 0, False)
      Nothing -> same
    _ | capturing ->
      (if frameEnd then Ending else Capturing, cSeq s, pixBeat, pixPush, pixCount, cOverflow s || pixLost)
    Armed -> same
    Capturing -> same -- (not reached: capturing)
    -- the last beat, then the trailer: they wait for room, never lost
    Ending -> case cBeat s of
      Just b | room -> (Ending, cSeq s, Nothing, Just b, cCount s, cOverflow s)
      Nothing | room -> (Flushing, cSeq s, Nothing, Just trailer, cCount s, cOverflow s)
      _ -> same
    Flushing ->
      if len1 == 0 && out1 == 0 && not awDone' && not wDone'
        then (Idle, cSeq s, cBeat s, Nothing, cCount s, cOverflow s)
        else same

  fifo' = case pushB of
    Just b -> replace tailIx b (cFifo s)
    Nothing -> cFifo s
  len' = if pushB /= Nothing then len1 + 1 else len1
  s' = Cap mode' seq' ovf' count' beat' fifo' head1 len' awDone' wDone' out1

-- | The capture for a core: the link's writes, the core's frame end, its
-- pixel writes (index in raster order, the colour shown), the picture's
-- size, the HP port's write slave; the write channels' outputs and
-- whether a capture is under way.
frameCapture
  :: (HiddenClockResetEnable dom, KnownNat n)
  => Unsigned 12 -> Unsigned 12
  -> Signal dom (Maybe (LinkAddr, BitVector 8))
  -> Signal dom Bool
  -> Signal dom (Maybe (Index n, Rgb24))
  -> Signal dom AxiWrS2M
  -> (Signal dom AxiWrM2S, Signal dom Bool)
frameCapture w h link frameEnd pixel s2m
  | screenshotsEnabled = unbundle (mealy (capStep w h) capInit (bundle (link, frameEnd, fmap (fmap (\(i, c) -> (fromIntegral i, c))) pixel, s2m)))
  | otherwise = (pure axiWrM2SIdle, pure False)
