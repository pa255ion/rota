{-# OPTIONS_GHC -fworker-wrapper -fspecialise-aggressively #-}
{-|
Side track Q (PLAN.md Q.6): the SNES's pixel drawing on a second CPU core,
for the fast version ("Snes.SpecFast.Machine"). Never synthesised.

The drawing ('ppuRenderTo') reads the PPU's registers, VRAM, CGRAM and the
line's sprite pixels, and writes only the frame buffer and its own fetch
state; nothing the CPU reads depends on it. So the machine keeps its own
PPU state as before, logs every change to what the drawing reads, in
order, with a "draw up to here" before each one, and a thread replays the
log on a copy of that state ('rlShadow'). Drawing a stretch of dots later,
in one piece, gives the pixels it gave dot by dot then: they depend only
on the state, and the state only changes where the log says.

* 'rlDraw' notes how far the line may be drawn (the machine calls it as
  its dots pass); nothing is logged until something changes ('rlFlush').
* 'rlReg', 'rlVram', 'rlCg', 'rlObj', 'rlStartLine', 'rlClearObj': the
  changes, each after the drawing so far.
* 'rlLineEnd' hands the lines' log to the threads, 'batchLines' at a time; 'rlSync' waits until it
  has drawn everything, before the frame buffer is read ('rlFrame').
* 'rlFrameEnd' marks a frame's end; there each thread keeps a copy of its
  rows, and 'rlLastFrame' puts together the frame before the latest (the
  desktop's default: the machine runs on into the next frame while the
  threads finish this one, the picture one frame late).

Several threads share the work: each applies every change to its own
copy, but draws only its share of the lines (line mod the number of
threads), and the frame is put together from their rows ('rlFrame'). A
line's drawing depends only on the state at its dots and on what the line
itself sets up (its fetches, its fine scroll, its sprites, all started
afresh at the line's start), so the lines can be drawn apart.

Off (0 threads) the machine draws inline as the reference model does, and
none of this runs.
-}
module Snes.SpecFast.RenderLog
  ( RenderLog
  , newRenderLog
  , rlThreaded
  , rlDraw
  , rlFlush
  , rlReg
  , rlVram
  , rlCg
  , rlObj
  , rlStartLine
  , rlClearObj
  , rlLineEnd
  , rlSync
  , rlFrame
  , rlFrameEnd
  , rlLastFrame
  , rlSetLag
  , rlReload
  ) where

import Prelude

import Control.Concurrent (forkIO)
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Exception (SomeException, try, throwIO)
import Control.Monad (when)
import Control.Monad.ST (ST)
import Control.Monad.ST.Unsafe (unsafeIOToST, unsafeSTToIO)
import Data.STRef
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MV
import Data.Word (Word16, Word8)

import Snes.SpecFast.Ppu

data Msg = Batch (U.Vector Int) | Sync (MVar (Maybe SomeException)) | FrameEnd

data RenderLog s = RenderLog
  { rlThreaded :: !Bool
  , rlCount    :: !Int                             -- ^ render threads (0: inline)
  , rlBuf      :: !(STRef s (MV.MVector s Int))  -- ^ this line's events
  , rlLen      :: !(STRef s Int)
  , rlLines    :: !(STRef s Int)                   -- ^ lines ended since the last batch
  , rlPend     :: !(MV.MVector s Int)             -- ^ 0: a draw pending, 1 line, 2 field, 3 up to
  , rlChans    :: ![Chan Msg]                      -- ^ one per thread
  , rlShadows  :: ![Ppu s]                         -- ^ each thread's copy of the PPU
  , rlDone     :: ![Chan (U.Vector Word16)]        -- ^ each thread's frames at their ends
  , rlEnded    :: !(STRef s Int)                   -- ^ frame ends marked, not yet taken
  , rlLag      :: !(STRef s Bool)                  -- ^ the frames kept at their ends (only then: 'rlSetLag')
  }

-- Event codes, then their arguments.
evReg, evVram, evCg, evObj, evStart, evClear, evDraw :: Int
evReg = 0     -- register, value
evVram = 1    -- byte, value
evCg = 2      -- byte, value
evObj = 3     -- the line's 256 sprite pixels
evStart = 4
evClear = 5
evDraw = 6    -- line, field, up to

-- | The log, its @n@ threads started (0: none, inline): each copy of the
-- PPU starts as the machine's (@vram@, @cg@, @oam@, @regs@, as they are now).
newRenderLog :: Int -> MV.MVector s Word8 -> MV.MVector s Word8 -> MV.MVector s Word8 -> MV.MVector s Int -> ST s (RenderLog s)
newRenderLog n vram cg oam regs = do
  shadows <- mapM (const $ do
      v <- MV.clone vram
      c <- MV.clone cg
      o <- MV.clone oam
      r <- MV.clone regs
      newPpu v c o r) [1 .. n]
  buf <- MV.new 4096 >>= newSTRef
  len <- newSTRef 0
  lns <- newSTRef 0
  pend <- MV.replicate 4 0
  chans <- mapM (const (unsafeIOToST newChan)) [1 .. n]
  dones <- mapM (const (unsafeIOToST newChan)) [1 .. n]
  ended <- newSTRef 0
  lag <- newSTRef False
  let rl = RenderLog (n > 0) n buf len lns pend chans shadows dones ended lag
  mapM_ (\(k, (ch, dn), sh) -> unsafeIOToST (() <$ forkIO (unsafeSTToIO (worker n k ch dn sh)))) (zip3 [0 ..] (zip chans dones) shadows)
  pure rl

-- | Thread @k@ of @n@: every event, in order, on its copy; the drawing of
-- its lines only.
worker :: Int -> Int -> Chan Msg -> Chan (U.Vector Word16) -> Ppu s -> ST s ()
worker n k chan done shadow = go Nothing
 where
  go err = do
    msg <- unsafeIOToST (readChan chan)
    case msg of
      Sync mv -> unsafeIOToST (putMVar mv err) >> go err
      FrameEnd -> do
        f <- ppuFrame shadow
        unsafeIOToST (writeChan done f)
        go err
      Batch v -> do
        r <- unsafeIOToST (try (unsafeSTToIO (replay n k shadow v)))
        go (either Just (const err) r)

replay :: Int -> Int -> Ppu s -> U.Vector Int -> ST s ()
replay nThreads k shadow v = loop 0
 where
  p = shadow
  n = U.length v
  at i = U.unsafeIndex v i
  loop !i
    | i >= n = pure ()
    | otherwise = case at i of
        0 -> ppuRegW p (at (i + 1)) (at (i + 2)) >> loop (i + 3)
        1 -> ppuVramW p (at (i + 1)) (fromIntegral (at (i + 2))) >> loop (i + 3)
        2 -> ppuCgW p (at (i + 1)) (fromIntegral (at (i + 2))) >> loop (i + 3)
        3 -> ppuSetObjLine p (U.slice (i + 1) 256 v) >> loop (i + 257)
        4 -> ppuStartLine p >> loop (i + 1)
        5 -> ppuClearSprites p >> loop (i + 1)
        _ -> do
          let line = at (i + 1)
          when (line `mod` nThreads == k) $ ppuRenderTo p line (at (i + 2)) (at (i + 3))
          loop (i + 4)

push :: RenderLog s -> [Int] -> ST s ()
push rl xs = do
  n <- readSTRef (rlLen rl)
  b0 <- readSTRef (rlBuf rl)
  let k = length xs
  b <- if n + k > MV.length b0 then do { b' <- MV.grow b0 (max (MV.length b0) k); writeSTRef (rlBuf rl) b'; pure b' } else pure b0
  mapM_ (\(j, x) -> MV.unsafeWrite b (n + j) x) (zip [0 ..] xs)
  writeSTRef (rlLen rl) (n + k)

-- | The line may be drawn up to dot @upTo@ (as 'ppuRenderTo' takes it).
rlDraw :: RenderLog s -> Int -> Int -> Int -> ST s ()
rlDraw rl !v !f !upTo = do
  let pd = rlPend rl
  MV.unsafeWrite pd 0 1
  MV.unsafeWrite pd 1 v
  MV.unsafeWrite pd 2 f
  MV.unsafeWrite pd 3 upTo

-- | The pending drawing into the log (before a change).
rlFlush :: RenderLog s -> ST s ()
rlFlush rl = do
  let pd = rlPend rl
  has <- MV.unsafeRead pd 0
  when (has /= 0) $ do
    v <- MV.unsafeRead pd 1
    f <- MV.unsafeRead pd 2
    u <- MV.unsafeRead pd 3
    MV.unsafeWrite pd 0 0
    push rl [evDraw, v, f, u]

change :: RenderLog s -> [Int] -> ST s ()
change rl xs = when (rlThreaded rl) (rlFlush rl >> push rl xs)

rlReg :: RenderLog s -> Int -> Int -> ST s ()
rlReg rl r v = change rl [evReg, r, v]

rlVram :: RenderLog s -> Int -> Word8 -> ST s ()
rlVram rl a v = change rl [evVram, a, fromIntegral v]

rlCg :: RenderLog s -> Int -> Word8 -> ST s ()
rlCg rl a v = change rl [evCg, a, fromIntegral v]

-- | The line's sprite pixels as the machine's PPU (@cpuPpu@) has them now.
rlObj :: RenderLog s -> Ppu s -> ST s ()
rlObj rl cpuPpu = when (rlThreaded rl) $ do
  rlFlush rl
  o <- ppuObjLine cpuPpu
  push rl (evObj : U.toList o)

rlStartLine, rlClearObj :: RenderLog s -> ST s ()
rlStartLine rl = change rl [evStart]
rlClearObj rl = change rl [evClear]

-- | Lines handed to the threads at once: each hand-off wakes them, and on
-- a laptop whose idle cores clock down a wake-up per line cost more than
-- the drawing (a slower 4-core laptop, 2026-10-21).
batchLines :: Int
batchLines = 16

-- | A line's end: its log to the threads with the next ones', every
-- 'batchLines' lines.
rlLineEnd :: RenderLog s -> ST s ()
rlLineEnd rl = when (rlThreaded rl) $ do
  k <- (+ 1) <$> readSTRef (rlLines rl)
  if k >= batchLines then send rl else writeSTRef (rlLines rl) k

-- | The log so far to the threads.
send :: RenderLog s -> ST s ()
send rl = do
  writeSTRef (rlLines rl) 0
  rlFlush rl
  n <- readSTRef (rlLen rl)
  when (n > 0) $ do
    b <- readSTRef (rlBuf rl)
    v <- U.freeze (MV.slice 0 n b)
    writeSTRef (rlLen rl) 0
    unsafeIOToST (mapM_ (\ch -> writeChan ch (Batch v)) (rlChans rl))

-- | Waits until the thread has drawn everything logged (a failure in it
-- is raised here).
rlSync :: RenderLog s -> ST s ()
rlSync rl = when (rlThreaded rl) $ do
  send rl
  unsafeIOToST $ do
    mvs <- mapM (\ch -> do { mv <- newEmptyMVar; writeChan ch (Sync mv); pure mv }) (rlChans rl)
    mapM_ (\mv -> takeMVar mv >>= maybe (pure ()) throwIO) mvs

-- | A frame's end (after its last line's log): each thread keeps its rows.
rlFrameEnd :: RenderLog s -> ST s ()
rlFrameEnd rl = do
 lag <- readSTRef (rlLag rl)
 when (rlThreaded rl && lag) $ do
  send rl
  unsafeIOToST (mapM_ (\ch -> writeChan ch FrameEnd) (rlChans rl))
  modifySTRef' (rlEnded rl) (+ 1)

-- | Keep each frame at its end, for 'rlLastFrame' (off by default: kept
-- frames nobody takes would pile up).
rlSetLag :: RenderLog s -> Bool -> ST s ()
rlSetLag rl = writeSTRef (rlLag rl)

-- | The frame before the latest one ended (waiting for it if the threads
-- are behind), put together from the threads' rows; Nothing before two
-- frames have ended (and when off). The frames in between are dropped.
rlLastFrame :: RenderLog s -> ST s (Maybe (U.Vector Word16))
rlLastFrame rl
  | not (rlThreaded rl) = pure Nothing
  | otherwise = do
      e <- readSTRef (rlEnded rl)
      if e < 2 then pure Nothing else do
        -- take all but the latest: the last of them is the frame wanted
        framesPer <- mapM (\dn -> unsafeIOToST (last <$> mapM (const (readChan dn)) [1 .. e - 1])) (rlDone rl)
        writeSTRef (rlEnded rl) 1
        pure (Just (assemble (rlCount rl) framesPer))

assemble :: Int -> [U.Vector Word16] -> U.Vector Word16
assemble n frames =
  let fv = V.fromList frames
  -- (row by row: a slice of the thread that drew it, one copy each; not
  -- a boxed index and a division per pixel, Q.7)
  in U.concat [ U.slice (y * frameWidth) frameWidth (fv V.! ((y + 1) `mod` n)) | y <- [0 .. frameHeight - 1] ]

-- | The frame buffer: the thread's, after 'rlSync'; @inline@'s when off.
rlFrame :: RenderLog s -> Ppu s -> ST s (U.Vector Word16)
rlFrame rl inline
  | rlThreaded rl = do
      rlSync rl
      frames <- mapM ppuFrame (rlShadows rl)
      -- row r is line r + 1, drawn by thread (r + 1) mod n
      pure (assemble (rlCount rl) frames)
  | otherwise = ppuFrame inline

-- | After a save state is loaded into the machine: every thread idle, its
-- copy of the PPU made the machine's again (@p@, whose VRAM, CGRAM, OAM
-- and registers are the machine's), the frames kept at their ends dropped.
rlReload :: RenderLog s -> Ppu s -> ST s ()
rlReload rl p = when (rlThreaded rl) $ do
  rlSync rl
  mapM_ (`ppuReload` p) (rlShadows rl)
  e <- readSTRef (rlEnded rl)
  mapM_ (\dn -> unsafeIOToST (mapM_ (const (readChan dn)) [1 .. e])) (rlDone rl)
  writeSTRef (rlEnded rl) 0
