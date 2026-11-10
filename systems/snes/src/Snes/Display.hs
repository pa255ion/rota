{-# LANGUAGE StrictData #-}
{-|
The Super Nintendo's picture, line-locked to the output (PLAN.md step
7.10; docs/systems/snes.md, "Clock and display"): no frame buffer, a ring
of 32 lines between the PPU and 640 × 480.

__The ring__ ('ringWrite'): the PPU's rows go into two memories of 32 × 256
15-bit colours, the left half-dots and the right (the same colour twice on
a low-res line), at row mod 32 ('ringRows'). The stream ('snesStream') reads a row's
512 half-dots across 512 output columns from x 64, each row on two output
lines: 224 rows from y 16 (448 lines), 239 with overscan from y 1 (478).
Black around it, and while the machine is held (the menu shows over
black: there is no picture to keep).

__The pace__ ('paceStep'): one SNES line (1364 master clocks) for two
output lines (1600 pixel clocks), so the master clock may run as long as
the core's place in its frame is behind the output's (1364/1600 master
clocks a pixel clock, over 524 of the output's 525 lines: in the last one
the core waits for the next frame), plus a lead that keeps the row shown
within the ring: without one, the row on output line y is shown 7.5 SNES
lines after the core makes it (from y 16; from y 1, the 239-line mode, 7.5
lines sooner, so that mode's lead has 7.5 more); the lead is 23 lines on
top ('leadMc'), so a row is shown 30.5 lines after it is made, a line and
a half before the ring brings its slot round again. A core behind (a DDR
miss, a long DMA from ROM, a wait for the sound module) runs at a master
clock a pixel clock until it is back: with 30 lines in hand (about 48,000
pixel clocks) before a row is shown late. (Step 7.12: with 8 rows and no
lead, a game's DMA from ROM at its scene changes showed rows late; the
DMA byte's 9 pixel clocks, `snes-ddrmodel`.) A late row
shows the ring's old one and is counted ('pcLate'), except while the
machine is held (the menu, a load) and after it, or after reset, until
the pace first holds the core back ('pcSettle': the core has caught up;
a menu once lit the late LED for good).

After a hold the core is not run on to catch up (at a master clock a
pixel clock: a pause's or a save's frames came out 17% fast, the tone's
pitch with them, step 7.15c): while held the output's frame count keeps
level with the core's (the core in the output's frame, or a frame on when
the output has passed it), so the core waits for the output to come round
to it, and the picture stays black ('pcBlank') until the output's first
frame start after the core has run again (the rows before it are the
ring's old ones).
-}
module Snes.Display
  ( ringWrite
  , snesStream
  , PaceIn (..)
  , Pace (..)
  , paceInit
  , paceStep
  , lineMc
  , frameMc
  , ringRows
  , leadMc
  ) where

import Clash.Prelude

import Platform.Video.VgaTiming

-- | A pixel ('Snes.Ppu.poPixel': x, row, left and right half-dots) into
-- the ring's two memories: the address (row mod 8, x) and the colour.
ringWrite :: Maybe (Unsigned 8, Unsigned 8, Unsigned 15, Unsigned 15)
          -> (Maybe (Unsigned 13, Unsigned 15), Maybe (Unsigned 13, Unsigned 15))
ringWrite px = case px of
  Just (x, row, l, r) ->
    let a = (resize (row .&. 31) `shiftL` 8) .|. resize x
    in (Just (a, l), Just (a, r))
  Nothing -> (Nothing, Nothing)

-- | The picture's first output line (16; 1 with overscan) and its rows.
top :: Bool -> Unsigned 10
top ov = if ov then 1 else 16

rows :: Bool -> Unsigned 10
rows ov = if ov then 239 else 224

-- | Where an output pixel falls in the picture: the row, the x, which half.
inPicture :: Bool -> VgaOut -> Maybe (Unsigned 8, Unsigned 8, Bool)
inPicture ov o =
  let x = vgaX o
      y = vgaY o
      t = top ov
      dy = y - t
  in if vgaActive o && x >= 64 && x < 576 && y >= t && dy < 2 * rows ov
       then let c = x - 64 in Just (resize (dy `shiftR` 1), resize (c `shiftR` 1), testBit c 0)
       else Nothing

-- | The ring's read address for an output pixel (read the clock before it
-- shows).
readAddr :: Bool -> VgaOut -> Unsigned 13
readAddr ov o = case inPicture ov o of
  Just (row, x, _) -> (resize (row .&. 31) `shiftL` 8) .|. resize x
  Nothing -> 0

bgr555 :: Unsigned 15 -> Rgb888
bgr555 c =
  let ch k = let v = resize ((c `shiftR` k) .&. 31) :: Unsigned 8 in pack ((v `shiftL` 3) .|. (v `shiftR` 2))
  in Rgb888 (ch 0) (ch 5) (ch 10)

-- | The stream: from the output's timing, the 239-line mode and the hold,
-- the ring's read address now, and the timing a clock later with its
-- colour (the ring's two memories' data then).
snesStream
  :: HiddenClockResetEnable dom
  => Signal dom VgaOut -> Signal dom Bool -> Signal dom Bool
  -> Signal dom (Unsigned 15) -> Signal dom (Unsigned 15)
  -> (Signal dom (Unsigned 13), (Signal dom VgaOut, Signal dom Rgb888))
snesStream vga ov held left right = (readAddr <$> ov <*> vga, (vgaD, rgb))
 where
  vgaD = register (vgaDecode vga640x480 (VgaCounters 0 0)) vga
  ovD = register False ov
  rgb = (\o v h l r -> case inPicture v o of
            Just (_, _, half) | not h -> bgr555 (if half then r else l)
            _ -> Rgb888 0 0 0)
          <$> vgaD <*> ovD <*> held <*> left <*> right

-- | A line's and a frame's master clocks (the frame as the output's 524
-- lines give it).
lineMc :: Unsigned 20
lineMc = 1364

frameMc :: Unsigned 20
frameMc = 262 * 1364

-- | The ring's rows, and the lead it allows (23 SNES lines of master
-- clocks; the 239-line mode 7.5 more).
ringRows :: Int
ringRows = 32

leadMc :: Unsigned 20
leadMc = 23 * 1364

data PaceIn = PaceIn
  { paVga      :: VgaOut
  , paFrame    :: Unsigned 2      -- ^ the core's frame count (mod 4)
  , paV        :: Unsigned 9      -- ^ its line
  , paH        :: Unsigned 11     -- ^ its master clock in the line
  , paOverscan :: Bool
  , paHeld     :: Bool            -- ^ the machine held (the menu, a load): its rows aren't late
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFDataX

data Pace = Pace
  { pcFrame   :: Unsigned 2       -- ^ the output's frame count (mod 4)
  , pcAllowed :: Unsigned 20      -- ^ master clocks the output has given in its frame
  , pcFrac    :: Unsigned 12      -- ^ the fraction (of 1600; + 1364 reaches 2963)
  , pcLate    :: Unsigned 16      -- ^ rows shown late (saturating)
  , pcSettle  :: Bool             -- ^ not counting: held, or not caught up since
  , pcBlank   :: Bool             -- ^ held, or since: black until a frame start after the core ran
  , pcRan     :: Bool             -- ^ the core allowed to run since the hold ended
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFDataX

-- | The output's frame count starts at -1: its first pixel after reset is
-- a frame's start, which makes it 0 with the core's first frame (from 0,
-- the core counted itself a frame behind and ran a frame ahead). Not
-- counting late rows until the core has caught up (in the 239-line mode
-- the first frame's top rows come before the core can make them).
paceInit :: Pace
paceInit = Pace maxBound 0 0 0 True False False

-- | A pixel clock: whether the master clock may run.
paceStep :: Pace -> PaceIn -> (Pace, Bool)
paceStep s i = (s', run)
 where
  o = paVga i
  ov = paOverscan i
  -- the output's frame: a new one at its first pixel; 1364/1600 master
  -- clocks a pixel clock over its first 524 lines
  newFrame = vgaFrameStart o
  frame1 = if newFrame then pcFrame s + 1 else pcFrame s
  (allowed0, frac0) = if newFrame then (0, 0) else (pcAllowed s, pcFrac s)
  giving = vgaY o < 524
  f1 = frac0 + (if giving then 1364 else 0)
  (allowed', frac') = if f1 >= 1600 then (allowed0 + 1, f1 - 1600) else (allowed0, f1)
  -- the core's place against it: behind (a frame back), the same frame,
  -- or ahead (a frame on: only with the lead)
  corePos = resize (paV i) * lineMc + resize (paH i) :: Unsigned 20
  lead = leadMc + (if ov then 10230 else 0)
  -- (held: the output's count level with the core's, the core in its
  -- frame or, passed, a frame on: no catching up after)
  frame' = if not (paHeld i) then frame1
           else if corePos >= allowed' then paFrame i else paFrame i - 1
  rel = paFrame i - frame'
  run = case rel of
    0 -> corePos < allowed' + lead
    1 -> corePos + frameMc < allowed' + lead
    _ -> True
  -- a row starting on screen that the core hasn't finished: late
  dy = vgaY o - top ov
  rowStart = vgaX o == 0 && vgaY o >= top ov && dy < 2 * rows ov && not (testBit dy 0)
  row = resize (dy `shiftR` 1) :: Unsigned 9
  done = case rel of
    0 -> paV i >= row + 2
    1 -> True
    _ -> False
  late' = if rowStart && not done && not (pcSettle s) && pcLate s /= maxBound then pcLate s + 1 else pcLate s
  -- counting again once the pace holds the core back (it has caught up)
  settle' = paHeld i || (pcSettle s && run)
  ran' = not (paHeld i) && (pcRan s || run)
  blank' = paHeld i || (pcBlank s && not (newFrame && pcRan s))
  s' = Pace frame' allowed' frac' late' settle' blank' ran'
