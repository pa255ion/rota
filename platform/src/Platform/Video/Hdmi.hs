{-|
HDMI mode on the Arty's TMDS output (side track G phase 3, step 2): the
video of "Platform.Video.Tmds" with data islands in every line's
horizontal blanking, carrying sound ("Platform.Video.HdmiPacket").

A line of 640x480 (800 pixels; active 0 to 639, blanking 640 to 799, HSYNC
656 to 751):

* 640 to 651: control period (syncs on channel 0);
* 652 to 659: the data-island preamble (CTL0 = 1, CTL2 = 1: channels 1
  and 2 send control symbol 01);
* 660, 661: the island's leading guard band;
* 662 to 693: packet A, an Audio Sample packet with the samples taken
  since the last one (up to four; 48 kHz is 1.53 a line);
* 694 to 725: packet B: the AVI InfoFrame on line 0, the Audio InfoFrame
  on line 1, Audio Clock Regeneration on every other line;
* 726, 727: the trailing guard band;
* 728 to 789: control period;
* 790 to 797, 798 and 799, only before an active line (lines 524 and 0 to
  478): the video preamble (CTL0 = 1: channel 1 sends 01) and the video
  guard band.

The 48 kHz sample strobe and the ACR packet come from the same numbers:
a phase accumulator adds N (6144) every pixel clock and takes a sample
when it reaches 128 x CTS, so the rate the sink rebuilds from N / CTS
(128 x fs = f_TMDS x N / CTS) is exactly the rate the samples arrive at,
whatever the real pixel clock is. CTS is a parameter: 25174 for the
Arty's 25.17361 MHz gives fs = 47 999.2 Hz.

'hdmiStep' is the pure transition function (unit-tested); 'hdmiVideo' its
registered form, a drop-in for "Platform.Video.Tmds".tmdsVideo.
-}
module Platform.Video.Hdmi
  ( HdmiState (..)
  , hdmiInit
  , hdmiStep
  , hdmiVideo
  , HdmiSlot (..)
  , slotAt
  , acrN
  ) where

import Clash.Prelude

import Platform.Video.HdmiPacket
import Platform.Video.Tmds (Disparity, TmdsIn (..), tmdsControlSymbol, tmdsEncode)
import Platform.Video.VgaTiming (Rgb888 (..), VgaOut (..))

-- | N for 48 kHz (HDMI 1.4b table 7-1).
acrN :: Unsigned 20
acrN = 6144

-- | What a pixel of the line carries.
data HdmiSlot
  = SlotVideo
  | SlotControl
  | SlotIslandPreamble
  | SlotIslandGuard
  | SlotPacket Bit (Index 32)   -- ^ packet A (0) or B (1), its pixel
  | SlotVideoPreamble
  | SlotVideoGuard
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFDataX

-- | The slot of pixel (x, y) (the timing generator's counters).
slotAt :: Unsigned 10 -> Unsigned 10 -> HdmiSlot
slotAt x y
  | x < 640 && y < 480 = SlotVideo
  | x >= 652 && x < 660 = SlotIslandPreamble
  | x >= 660 && x < 662 = SlotIslandGuard
  | x >= 662 && x < 694 = SlotPacket 0 (fromIntegral (x - 662))
  | x >= 694 && x < 726 = SlotPacket 1 (fromIntegral (x - 694))
  | x >= 726 && x < 728 = SlotIslandGuard
  | nextActive && x >= 790 && x < 798 = SlotVideoPreamble
  | nextActive && x >= 798 = SlotVideoGuard
  | otherwise = SlotControl
 where
  nextActive = y < 479 || y == 524

data HdmiState = HdmiState
  { hsCnt     :: Disparity                -- ^ the video encoders' disparities
  , hsCnt1    :: Disparity
  , hsCnt2    :: Disparity
  , hsAcc     :: Unsigned 23              -- ^ the 48 kHz phase accumulator
  , hsFifo    :: Vec 4 (Maybe AudioFrame) -- ^ samples taken, for the next packet A
  , hsFrame   :: Index 192                -- ^ the next sample's frame in its block
  , hsPacketA :: Packet
  , hsPacketB :: Packet
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFDataX

hdmiInit :: HdmiState
hdmiInit = HdmiState 0 0 0 0 (repeat Nothing) 0 nullPacket nullPacket

-- | One pixel clock: the state, CTS, the timing and colour of this pixel,
-- the system's current sample (left, right); the next state and the three
-- channels' symbols.
hdmiStep
  :: HdmiState -> Unsigned 20 -> VgaOut -> Rgb888 -> (Signed 16, Signed 16)
  -> (HdmiState, Vec 3 (BitVector 10))
hdmiStep s cts o (Rgb888 r g b) (sl, sr) = (s', syms)
 where
  x = vgaX o
  y = vgaY o
  hs = vgaHSync o
  vs = vgaVSync o
  slot = slotAt x y
  ctl c1 c0 = tmdsControlSymbol c1 c0

  -- video: the three encoders; their disparity restarts in every other period
  (c0', d0) = tmdsEncode (hsCnt s) (TmdsData b)
  (c1', d1) = tmdsEncode (hsCnt1 s) (TmdsData g)
  (c2', d2) = tmdsEncode (hsCnt2 s) (TmdsData r)
  video = slot == SlotVideo

  syms = case slot of
    SlotVideo -> d0 :> d1 :> d2 :> Nil
    SlotControl -> ctl vs hs :> ctl 0 0 :> ctl 0 0 :> Nil
    SlotIslandPreamble -> ctl vs hs :> ctl 0 1 :> ctl 0 1 :> Nil
    SlotIslandGuard -> islandGuard hs vs
    SlotPacket k i -> map terc4 (islandNibbles hs vs (if k == 0 then hsPacketA s else hsPacketB s) i)
    SlotVideoPreamble -> ctl vs hs :> ctl 0 1 :> ctl 0 0 :> Nil
    SlotVideoGuard -> videoGuard

  -- the 48 kHz strobe
  limit = 128 * resize cts :: Unsigned 23
  accSum = hsAcc s + resize acrN
  strobe = accSum >= limit
  acc' = if strobe then accSum - limit else accSum
  frame = AudioFrame sl sr (hsFrame s)
  frame' = if strobe then satSucc SatWrap (hsFrame s) else hsFrame s

  -- packets latched the pixel before the island (x = 661): A takes the
  -- samples so far, the FIFO starts again
  latch = x == 661
  fifoBase = if latch then repeat Nothing else hsFifo s
  push v = case findIndex (== Nothing) v of
    Just i -> replace i (Just frame) v
    Nothing -> v   -- at most 2 a line arrive; 4 always leave room
  fifo' = if strobe then push fifoBase else fifoBase
  packetA' = if latch then audioSamplePacket (hsFifo s) else hsPacketA s
  packetB' | not latch = hsPacketB s
           | y == 0 = aviInfoFrame
           | y == 1 = audioInfoFrame
           | otherwise = acrPacket acrN cts

  s' = HdmiState
    { hsCnt = if video then c0' else 0
    , hsCnt1 = if video then c1' else 0
    , hsCnt2 = if video then c2' else 0
    , hsAcc = acc'
    , hsFifo = fifo'
    , hsFrame = frame'
    , hsPacketA = packetA'
    , hsPacketB = packetB'
    }

-- | The video stream and the system's sample stream to the three channels'
-- symbols in HDMI mode, one clock later (control 00 in reset): the drop-in
-- for "Platform.Video.Tmds".tmdsVideo.
hdmiVideo
  :: HiddenClockResetEnable dom
  => Unsigned 20 -> Signal dom VgaOut -> Signal dom Rgb888 -> Signal dom (Signed 16, Signed 16)
  -> Signal dom (Vec 3 (BitVector 10))
hdmiVideo cts o c a = mealy step (hdmiInit, repeat (tmdsControlSymbol 0 0)) (bundle (o, c, a))
 where
  step (st, out) (oi, ci, ai) = let (st', syms) = hdmiStep st cts oi ci ai in ((st', syms), out)
