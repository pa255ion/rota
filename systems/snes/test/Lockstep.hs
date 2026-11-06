-- | Layer 1, step 7.6: the Clash PPU ("Snes.Ppu") against the spec's.
-- Each ROM runs in the spec ("Snes.Spec") with its PPU accesses recorded
-- (their master clocks, H and V, the values read: 'traceOn'); the trace
-- is replayed into 'ppuClock' a clock at a time over models of its block
-- RAMs (one clock's latency, read before write), with idle clocks (no
-- master clock) between ticks at random, as the 25.175 MHz domain has.
-- Checked: H and V at each access, each value read, each frame's picture
-- when line 0 begins, and VRAM, CGRAM and OAM at the end.
module Main (main) where

import Prelude

import Control.Monad (forM_, unless, when)
import Control.Monad.ST (stToIO)
import Data.Bits
import Data.IORef
import System.Environment (lookupEnv)
import System.FilePath ((</>), takeFileName)
import Test.Tasty
import Test.Tasty.HUnit

import qualified Clash.Prelude as C
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MV

import PpuModel
import Snes.Cart
import Snes.Ppu
import Snes.Spec

asmDir, romDir, iplFile :: FilePath
asmDir = "../../build/snes/asm"
romDir = "../../roms/snes"
iplFile = "../../build/snes/ipl.bin"

-- | The quick tier runs each ROM a few frames; the full tier
-- (EMU_FULL_TESTS=1, make test FULL=1) 60.
main :: IO ()
main = do
  full <- (== Just "1") <$> lookupEnv "EMU_FULL_TESTS"
  defaultMain $ testGroup "snes-lockstep"
    [ testCase (f <> ", " <> show k <> " frames" <> (if idle then ", idle clocks" else "")) (lockstep (dir </> f) k idle)
    | (dir, f, n, idle) <- roms, let k = if full then max n 60 else n ]

-- | The ROMs: our register tests (no picture: they stop); krom's picture
-- tests (BG modes, Mode 7, HDMA, windows, hi-res, interlace); the
-- OpenSNES examples (sprites, colour maths, mosaic, EXTBG); the two
-- games. The quick tier's frames: two past the first with the picture
-- (measured 2026-10-15), so each run compares pictures, not black.
roms :: [(FilePath, FilePath, Int, Bool)]
roms =
  [ (asmDir, "ppuregs.sfc", 8, True)
  , (asmDir, "dma.sfc", 4, True)
  , (asmDir, "ppufuzz.sfc", 12, True)
  , (romDir </> "krom", "8x8BG1Map2BPP32x328PAL.sfc", 8, True)
  , (romDir </> "krom", "8x8BGMap8BPP32x32.sfc", 10, False)
  , (romDir </> "krom", "8x8BGMapTileFlip.sfc", 10, True)
  , (romDir </> "krom", "HiColor64PerTileRowPseudoHiRes.sfc", 10, True)
  , (romDir </> "krom", "Mode7HDMA.sfc", 10, True)
  , (romDir </> "krom", "WindowMultiHDMA.sfc", 10, True)
  , (romDir </> "krom", "InterlaceFont.sfc", 10, True)
  , (romDir </> "opensnes", "extbg.sfc", 40, True)
  , (romDir </> "opensnes", "mode5_hires.sfc", 10, True)
  , (romDir </> "opensnes", "mode6.sfc", 10, True)
  , (romDir </> "opensnes", "sprite_sizes.sfc", 8, True)
  , (romDir </> "opensnes", "simple_sprite.sfc", 8, True)
  , (romDir </> "opensnes", "window.sfc", 6, True)
  , (romDir </> "opensnes", "shadow_tint.sfc", 8, True)
  , (romDir </> "opensnes", "direct_color.sfc", 42, True)
  , (romDir </> "opensnes", "shmup_1942.sfc", 6, True)
  , (romDir </> "games", "horizontal-shooter.sfc", 18, True)
  ]

lockstep :: FilePath -> Int -> Bool -> Assertion
lockstep path n idle = do
  file <- readRom path
  ipl <- readRom iplFile
  c <- maybe (assertFailure (path <> ": no header") >> fail "") pure (parseCart file)
  (trace, vram, cg, oam) <- stToIO $ do
    m <- newMachine ipl c file
    traceOn m
    reset m
    runFrames m n
    t <- takeTrace m
    v <- U.fromList <$> mapM (readVram m) [0 .. 0xFFFF]
    g <- cgramColors m
    o <- mapM (readOam m) [0 .. 543]
    pure (t, v, g, o)
  -- LOCKSTEP_LINE="frame line": that line's accesses, for debugging
  dbg <- lookupEnv "LOCKSTEP_LINE"
  forM_ (fmap (map read . words) dbg) $ \fl -> case fl of
    [fk, ln] ->
      let inFrame = takeWhile (not . isDone fk) (dropWhile (not . isDone (fk - 1)) trace)
          isDone k e = case e of PpuFrameDone _ _ -> k < 0; _ -> False
          frameOf = scanl (\k e -> case e of PpuFrameDone _ _ -> k + 1; _ -> k) (0 :: Int) trace
      in do
        _ <- pure inFrame
        forM_ (zip frameOf trace) $ \(k, e) -> case e of
          PpuAt t hc v a | k == fk && (v == ln || v == ln - 1) -> putStrLn (show t <> " V " <> show v <> " H " <> show hc <> ": " <> show a)
          _ -> pure ()
    _ -> pure ()
  mems <- newMems
  seed <- newIORef (12345 :: Int)
  (final, framesSeen) <- replay mems seed idle trace
  -- the memories at the end
  forM_ [0 .. 0x7FFF] $ \w -> do
    lo <- MV.read (mVlo mems) w
    hi <- MV.read (mVhi mems) w
    when ((lo, hi) /= (vram U.! (2 * w), vram U.! (2 * w + 1))) $
      assertFailure ("VRAM word " <> show w <> ": " <> show (lo, hi) <> ", the spec " <> show (vram U.! (2 * w), vram U.! (2 * w + 1)))
  cgs <- mapM (MV.read (mCg mems)) [0 .. 255]
  unless (map fromIntegral cgs == map (.&. 0x7FFF) cg) $ assertFailure "CGRAM differs"
  oams <- mapM (MV.read (mOamL mems)) [0 .. 511]
  let hiT = map fromIntegral (C.toList (pOamHigh final))
  unless (oams <> hiT == oam) $ assertFailure "OAM differs"
  let specFrames = length [ () | PpuFrameDone _ _ <- trace ]
      lastFrame = [ fr | PpuFrameDone _ fr <- trace ]
      stops = takeWhile (/= '.') (takeFileName path) `elem` ["ppuregs", "dma"]
  -- a picture compared, not a blank screen
  unless stops $ case reverse lastFrame of
    fr : _ | U.any (/= U.head fr) fr -> pure ()
    _ -> assertFailure "no picture in the last frame: run more frames"
  unless (framesSeen == specFrames) $
    assertFailure (show framesSeen <> " frames compared of the spec's " <> show specFrames)

-- | Replays the trace; the PPU at the end, the frames compared.
replay :: Mems -> IORef Int -> Bool -> [PpuEvent] -> IO (PpuC, Int)
replay mems seed idle = go ppuInit ppuIdleIn 0 Nothing Nothing 0 0
 where
  -- with idle clocks, a third of the accesses come in an extra clock
  -- after their tick ('defer'), as the machine gives a DMA byte
  go st inp mc pend defer nFrames clocks evs = case (evs, defer) of
    ([], Nothing) -> pure (st, nFrames)
    _ -> do
      r <- if idle then rand seed else pure 1
      let tickNow = r `mod` 7 /= 0 && defer == Nothing
          mc' = if tickNow then mc + 1 else mc
          (now, rest) = if tickNow then span (atClock mc') evs else ([], evs)
          accs0 = [ a | PpuAt _ _ _ a <- now ]
          later = idle && (r `div` 7) `mod` 3 == 0 && not (null accs0)
          accs = case defer of
            Just a -> [a]
            Nothing -> if later then [] else accs0
          defer' = case accs0 of
            a : _ | later -> Just a
            _ -> Nothing
      when (length accs0 > 1) $ assertFailure ("two accesses at master clock " <> show mc' <> ": " <> show now)
      let acc = case accs of
            [a] -> Just (toAccess a)
            _ -> Nothing
          (st', out) = ppuClock st inp { piTick = tickNow, piAccess = acc }
      -- the last clock's read
      case pend of
        Just (pc, mdr, want) ->
          let got = case poRead out of
                Just v -> maybe mdr fromIntegral v
                Nothing -> 0xEEEE
          in unless (got == fromIntegral want) $
               assertFailure ("master clock " <> show pc <> ": read " <> hex got <> ", the spec " <> hex (fromIntegral want :: Int))
        Nothing -> pure ()
      forM_ now $ \e -> case e of
        PpuAt _ hc v a ->
          unless (fromIntegral (poHc out) == hc && fromIntegral (poVc out) == v) $
            assertFailure ("master clock " <> show mc' <> " " <> show a <> ": H, V " <> show (poHc out, poVc out) <> ", the spec " <> show (hc, v))
        PpuFrameDone _ _ -> pure ()
      -- the sprite unit is done by the line's end (VRAM and OAM take
      -- writes again then)
      when (tickNow && poHc out == 0 && pSpr st' /= SpIdle) $
        assertFailure ("master clock " <> show mc' <> ": the sprite unit still running at the line's end")
      -- the pixel
      case poPixel out of
        Just (x, row, l, rr) -> do
          let i = fromIntegral row * 512 + 2 * fromIntegral x
          MV.write (mFrame mems) i (fromIntegral l)
          MV.write (mFrame mems) (i + 1) (fromIntegral rr)
        Nothing -> pure ()
      nF <- foldr (\e k -> case e of
                      PpuFrameDone _ fr -> do
                        compareFrame mems nFrames fr
                        (+ 1) <$> k
                      _ -> k) (pure nFrames) now
      inp' <- memories mems out
      let pend' = case accs of
            [PpuRead _ mdr v] -> Just (mc', fromIntegral mdr :: Int, v)
            _ -> Nothing
      go st' inp' mc' pend' defer' nF (clocks + 1 :: Int) rest
  atClock k e = case e of
    PpuAt t _ _ _ -> t == k
    PpuFrameDone t _ -> t == k

toAccess :: PpuAccess -> Access
toAccess a = case a of
  PpuWrite r v -> AWrite (fromIntegral r) (fromIntegral v)
  PpuRead r mdr _ -> ARead (fromIntegral r) (fromIntegral mdr)
  PpuLatch -> ALatch
