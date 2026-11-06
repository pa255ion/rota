{-|
Frame-synchronous input for input movies (side track O, O.1; PLAN.md "Side
track O"; docs/ARCHITECTURE.md, the movie register and @PS_IN@ bit 7).

The PS changes the input word (@PS_GPIO@ bits 0 to 15) whenever it likes,
microseconds apart from one run to the next, so a recording replayed that
way desyncs. While a movie records or plays, the PS sets the movie
register's bit 0, and the input word a core sees is the one latched at the
start of its frame: constant for the whole frame, the same frame after
frame on every run. A frame toggle on @PS_IN@ bit 7 tells the PS that a
frame started, so it has the rest of it (16.7 ms; it polls about once a
millisecond) to set the next word. With the bit clear (the reset value) the
word passes through as before: no delay in normal play.

While the PS holds the core (a game loading, a movie's power-on start,
O.2b) the core makes no frames, so nothing would be latched: the latch
follows the live word instead (without a flip), and the core's first frame
after the hold sees the word the PS set, not one left from before.

Frame advance (O.3, 'frameAdvance'): a write to the control register
('controlAddr') with bit 2 set while the core is paused lets it run to its
next frame start, and is paused again on the clock after it (the pause
comes from a register only: the frame start itself depends on the core's
dot enable, which the pause gates, so a pause taken from it the same clock
was a combinational loop, Verilator's UNOPTFLAT). Each step after the
first is exactly one frame (the first ends the frame the pause cut), and
stops at the same point of it every time.
The pause bit stays set throughout; keys reach the core during a step (a
movie recorded frame by frame).

Built with @MOVIES=0@ ('moviesEnabled' false) there is no latch: the word
passes through and the toggle stays 0; and no frame advance.
-}
module Platform.Ps.FrameInput
  ( -- * The movie register
    movieAddr
  , PsMovie (..)
  , psMovieInit
  , psMovieStep
  , psMovie
    -- * The latch
  , FrameInput (..)
  , frameInputInit
  , frameInputStep
  , frameInputOut
  , frameInput
  , psInFrame
    -- * Frame advance
  , frameAdvanceStep
  , stepRequest
  , frameAdvance
  ) where

import Clash.Prelude

import Platform.Features (moviesEnabled)
import Platform.Ps.Link (LinkAddr, controlAddr)

-- | The movie register, in the link's shared range (reserved in
-- docs/ARCHITECTURE.md): bit 0 frame-synchronous input.
movieAddr :: LinkAddr
movieAddr = 0xF01003

newtype PsMovie = PsMovie
  { pmSync :: Bool  -- ^ the input word latched at each frame start
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFDataX

-- | Off, as before the register existed.
psMovieInit :: PsMovie
psMovieInit = PsMovie False

psMovieStep :: PsMovie -> Maybe (LinkAddr, BitVector 8) -> PsMovie
psMovieStep _ (Just (a, d)) | a == movieAddr = PsMovie (testBit d 0)
psMovieStep s _ = s

-- | The register, decoded from the link's writes.
psMovie
  :: HiddenClockResetEnable dom
  => Signal dom (Maybe (LinkAddr, BitVector 8)) -> Signal dom PsMovie
psMovie = moore psMovieStep id psMovieInit

-- | The latched word and the frame toggle.
data FrameInput = FrameInput
  { fiWord   :: BitVector 16
  , fiToggle :: Bool
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFDataX

frameInputInit :: FrameInput
frameInputInit = FrameInput 0 False

-- | At a frame start: the live word latched, the toggle flipped.
frameInputStep :: FrameInput -> (Bool, Bool, BitVector 16) -> FrameInput
frameInputStep s (hold, start, live)
  | hold = FrameInput live (fiToggle s)
  | start = FrameInput live (not (fiToggle s))
  | otherwise = s

-- | The word the core sees: the latched one while synchronised, else the
-- live one.
frameInputOut :: Bool -> BitVector 16 -> FrameInput -> BitVector 16
frameInputOut sync live s = if sync then fiWord s else live

-- | The core's input word and the frame toggle, from the movie register's
-- sync bit, the core's frame start (a pulse) and the live word.
frameInput
  :: HiddenClockResetEnable dom
  => Signal dom Bool          -- ^ the core held by the PS
  -> Signal dom Bool          -- ^ synchronised ('pmSync')
  -> Signal dom Bool          -- ^ the core's frame start
  -> Signal dom (BitVector 16) -- ^ the live word ('pgUser')
  -> (Signal dom (BitVector 16), Signal dom Bool)
frameInput hold sync start live
  | moviesEnabled = (frameInputOut <$> sync <*> live <*> st, fiToggle <$> st)
  | otherwise = (live, pure False)
 where
  st = register frameInputInit (frameInputStep <$> st <*> bundle (hold, start, live))

-- | A write to the control register with bit 2, the step bit.
stepRequest :: Maybe (LinkAddr, BitVector 8) -> Bool
stepRequest (Just (a, d)) = a == controlAddr && testBit d 2
stepRequest Nothing = False

-- | Whether a step is under way: (step requested, frame start, paused).
-- A request while paused starts one; the frame start ends it; unpaused,
-- none.
frameAdvanceStep :: Bool -> (Bool, Bool, Bool) -> Bool
frameAdvanceStep stepping (req, start, pause)
  | not pause = False
  | req = True
  | start = False
  | otherwise = stepping

-- | The pause the core sees: the PS's, except during a step; paused again
-- the clock after the frame start (from the register alone: no
-- combinational path from the core's frame start back to its enable).
frameAdvance
  :: HiddenClockResetEnable dom
  => Signal dom (Maybe (LinkAddr, BitVector 8)) -- ^ the link's writes
  -> Signal dom Bool                            -- ^ the core's frame start
  -> Signal dom Bool                            -- ^ the PS's pause ('pcPause')
  -> Signal dom Bool
frameAdvance link start pause
  | moviesEnabled = (\p s -> p && not s) <$> pause <*> stepping
  | otherwise = pause
 where
  stepping = register False (frameAdvanceStep <$> stepping <*> bundle (stepRequest <$> link, start, pause))

-- | @PS_IN@ with the frame toggle in bit 7.
psInFrame :: Bool -> BitVector 32 -> BitVector 32
psInFrame t w = if t then setBit w 7 else w
