{-|
Three frame buffers in block RAM, for a core whose frame rate isn't
the display's (PLAN.md step 3.7; generalised from the Game Boy's,
docs/systems/gameboy.md "Frame-rate mismatch").

The core draws into one buffer ('bkW'), one holds the newest complete
frame ('bkReady'), and the display shows one ('bkR'), taking the newest
complete frame at its own vertical blank. The core never draws into the
frame on screen, so nothing tears; a core slower than the display shows a
frame twice now and then, a faster one (the NES, 60.10 Hz against 59.94)
drops one.

'tripleBuffer' takes the frame size (pixels) and the pixel width as types:
the Game Boy has 160 × 144 × 2 bits, the NES 256 × 240 × 6.
-}
module Platform.Video.FrameBuffers
  ( Banks (..)
  , banksInit
  , banksStep
  , tripleBuffer
  ) where

import Clash.Prelude

-- | Which frame buffer is which: the core draws into 'bkW', 'bkReady' is
-- the newest complete frame, the display shows 'bkR'. Invariant: 'bkW'
-- differs from both others.
data Banks = Banks { bkW, bkReady, bkR :: Index 3 }
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFDataX

banksInit :: Banks
banksInit = Banks 0 1 1

-- | One clock: the core completed a frame, the display's vertical blank.
banksStep :: Banks -> (Bool, Bool) -> Banks
banksStep (Banks w ready r) (frameDone, vblank) = Banks w' ready' r'
 where
  r' = if vblank then ready else r
  ready' = if frameDone then w else ready
  -- the bank that is neither the frame just completed nor the one on screen
  w' | frameDone = case (w, r') of
         (0, 1) -> 2
         (1, 0) -> 2
         (0, 2) -> 1
         (2, 0) -> 1
         _ -> 0
     | otherwise = w

-- | @tripleBuffer n wr frameDone vblank rdAddr@: write port from the core
-- (pixel, value), the core's frame-done pulse, the display's
-- vertical-blank pulse and its read address; the value read (one clock
-- later). The buffers are three block RAMs of @n@ words of @w@ bits, not
-- one of @3 × n@: Vivado rounds a memory's depth up to a power of two
-- before splitting it into RAMB36s, so one NES buffer of 184320 × 6 bits
-- would take 48 of them (256K deep) against 36 for three of 61440.
tripleBuffer
  :: forall n w dom
   . (HiddenClockResetEnable dom, KnownNat n, KnownNat w, 1 <= n)
  => SNat n
  -> Signal dom (Maybe (Index n, BitVector w))
  -> Signal dom Bool
  -> Signal dom Bool
  -> Signal dom (Index n)
  -> Signal dom (BitVector w)
tripleBuffer n wr frameDone vblank rdAddr = (!!) <$> bundle outs <*> register 0 (bkR <$> banks)
 where
  banks = register banksInit (banksStep <$> banks <*> bundle (frameDone, vblank))
  outs :: Vec 3 (Signal dom (BitVector w))
  outs = map (\b -> blockRam1 NoClearOnReset n 0 rdAddr (wrFor b <$> banks <*> wr)) indicesI
  wrFor b bk w = if bkW bk == b then w else Nothing
