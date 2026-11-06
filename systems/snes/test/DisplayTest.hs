-- | The line ring's pace ("Snes.Display.paceStep", step 7.10) against the
-- output's real timing (640 × 480, 800 × 525 clocks) and a model core that
-- takes every master clock the pace allows: over three frames, no row is
-- shown before the core has made it (none late) or after it has started
-- writing the row 8 later into the same slot (none overwritten), in both
-- the 224-line and the 239-line mode; and the core is never a frame ahead.
-- (Layer 4's first run found the output's frame count starting a frame
-- early: the core ran a frame ahead and wrote over the rows shown.) And
-- the late count ('pcLate', LD1): a stall of 25 lines fits in the ring's
-- reserve (step 7.12: with 8 rows, a game's DMA from ROM showed rows
-- late), one of 40 counts, a hold (the menu) doesn't, nor does the 239-line mode's first frame, and after a
-- hold the count is back on once the core has caught up (layer 4 found
-- the menu lighting LD1 for good).
module DisplayTest (displayTests, traceTests) where

import Prelude

import qualified Clash.Prelude as C
import Test.Tasty
import Test.Tasty.HUnit

import Platform.Video.VgaTiming
import Snes.Display
import Snes.Machine (Trace (..), traceGap, traceInit, traceLine, traceStep)

displayTests :: TestTree
displayTests = testGroup "the line ring's pace"
  [ testCase "224 lines: no row late or overwritten, the core never ahead" (paceCheck False)
  , testCase "239 lines: the same, with the lead" (paceCheck True)
  , testCase "a stall of 25 lines: within the ring's reserve, nothing late" $ do
      let s = lateRun False (\n -> (False, n >= frame + 100 * 800 && n < frame + 150 * 800)) 3
      pcLate s @?= 0
  , testCase "a stall of 40 lines: rows counted late" $ do
      let s = lateRun False (\n -> (False, n >= frame + 100 * 800 && n < frame + 180 * 800)) 3
      assertBool ("late " <> show (pcLate s)) (pcLate s > 0)
  , testCase "the menu held a frame and a half: nothing counted, counting again after" $ do
      let s = lateRun False (\n -> (n >= frame + 50 * 800 && n < 2 * frame + 300 * 800, False)) 12
      pcLate s @?= 0
      pcSettle s @?= False
  , testCase "239 lines, from reset: nothing counted" $ do
      let s = lateRun True (const (False, False)) 3
      pcLate s @?= 0
      pcSettle s @?= False
  ]
 where
  frame = 525 * 800

-- | The pace over N output frames with a model core that takes every
-- master clock allowed unless it's held or stalled at that pixel clock
-- (held: the machine's hold, given to the pace; stalled: a DDR miss).
lateRun :: Bool -> (Int -> (Bool, Bool)) -> Int -> Pace
lateRun ov stop nFrames = go (zip [0 ..] (drop 1 (C.sampleN @C.System (nFrames * 525 * 800 + 1) (vgaTiming vga640x480)))) paceInit (0 :: Int) (0 :: Int) (0 :: Int)
 where
  go [] s _ _ _ = s
  go ((n, o) : rest) s f v h =
    let (held, stalled) = stop n
        (s', run) = paceStep s (PaceIn o (fromIntegral (f `mod` 4)) (fromIntegral v) (fromIntegral h) ov held)
        (f', v', h') | not run || held || stalled = (f, v, h)
                     | h + 1 < 1364 = (f, v, h + 1)
                     | v + 1 < 262 = (f, v + 1, 0)
                     | otherwise = (f + 1, 0, 0)
    in s' `seq` go rest s' f' v' h'

paceCheck :: Bool -> Assertion
-- (the timing's first sample is its reset cycle, at 0, 0 as the next: in
-- the machine the pace's register is in reset with it, so it isn't given)
paceCheck ov = go (drop 1 (C.sampleN @C.System (3 * 525 * 800 + 10) (vgaTiming vga640x480))) paceInit (0 :: Int) 0 0 (-1 :: Int)
 where
  top = if ov then 1 else 16
  nRows = if ov then 239 else 224
  -- the model core: its frame, line and master clock
  go [] _ _ _ _ _ = pure ()
  go (o : rest) s f v h k0 = do
    let k = if vgaFrameStart o then k0 + 1 else k0   -- the output's frame (this pixel's)
        pin = PaceIn o (fromIntegral (f `mod` 4)) (fromIntegral v) (fromIntegral h) ov False
        (s', run) = paceStep s pin
        (f', v', h') | not run = (f, v, h)
                     | h + 1 < 1364 = (f, v, h + 1)
                     | v + 1 < 262 = (f, v + 1, 0)
                     | otherwise = (f + 1, 0, 0)
        y = fromIntegral (vgaY o) :: Int
        x = fromIntegral (vgaX o) :: Int
        -- the row on screen (in the picture), and the core's row being made
        -- (line v makes row v - 1), both counted across frames
        shown = (y - top) `div` 2
        inPic = vgaActive o && x >= 64 && x < 576 && y >= top && y < top + 2 * nRows
        absShown = k * 262 + shown
        absMaking = f * 262 + v - 1
    -- (from y 1 the first frame's top rows come before the core, starting
    -- with the output, can make them: the lead holds from the next frame)
    if inPic && absMaking <= absShown && not (ov && k == 0)
      then assertFailure ("frame " <> show k <> " line " <> show y <> ": row " <> show shown <> " shown late (the core at line " <> show v <> " of frame " <> show f <> ")")
      else if inPic && absMaking >= absShown + ringRows
        then assertFailure ("frame " <> show k <> " line " <> show y <> ": row " <> show shown <> " written over (the core at line " <> show v <> " of frame " <> show f <> ")")
        else if f > k + 1
          then assertFailure ("the core a frame ahead at output frame " <> show k)
          else go rest s' f' v' h' k

-- | The freeze hunt's trace (2026-10-19, "Snes.Machine"): the line's text,
-- its characters one a gap apart, paused while the save's read-back has the
-- byte channel.
traceTests :: TestTree
traceTests = testGroup "the trace"
  [ testCase "the line's text" $
      map (toEnum . fromIntegral) (C.toList (traceLine bytes)) @?= "c 7e8123 p34 n81 i 01020304 o aabbccdd s 0fff f61 b 01 c01cdf c00416 02\n"
  , testCase "a line: 72 characters, a gap apart" $ do
      let out = run (replicate n False)
      map fst out @?= C.toList (traceLine bytes)
      -- (the snapshot the first clock, the first character the next)
      map snd out @?= [ 1 + k * (fromIntegral traceGap + 1) | k <- [0 .. 71] ]
  , testCase "paused while the read-back has the channel" $ do
      let busyAt k = k >= 100000 && k < 300000
          out = run (map busyAt [0 .. n + 200000 - 1])
      map fst out @?= C.toList (traceLine bytes)
      assertBool "nothing sent while paused" (not (any (\(_, k) -> busyAt k) out))
  ]
 where
  bytes = 0x7E C.:> 0x81 C.:> 0x23 C.:> 0x34 C.:> 0x81 C.:> 1 C.:> 2 C.:> 3 C.:> 4
          C.:> 0xAA C.:> 0xBB C.:> 0xCC C.:> 0xDD C.:> 0x0F C.:> 0xFF C.:> 0x61
          C.:> 1 C.:> 0xC0 C.:> 0x1C C.:> 0xDF C.:> 0xC0 C.:> 0x04 C.:> 0x16 C.:> 0x02 C.:> C.Nil
  n = 72 * (fromIntegral traceGap + 1) + 10
  -- from a line due now: the characters sent and the clock each went out at
  run busy = go (traceInit { tTimer = 0 }) (zip [0 :: Int ..] busy)
  go _ [] = []
  go t ((k, b) : rest) =
    let (t', o) = traceStep t (b, bytes)
    in maybe id (\c -> ((c, k) :)) o (go t' rest)
