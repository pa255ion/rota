{-# OPTIONS_GHC -fworker-wrapper -fspecialise-aggressively -funfolding-use-threshold=200 -funfolding-creation-threshold=1000 #-}
-- (the fast version is never synthesised: GHC's unboxing back on, which
-- clash-common's -fno-worker-wrapper turns off for Clash's sake)
-- The Game Boy's fast version (side track Q for side track W, the user
-- 2026-11-07: the browser's Game Boy Color too slow on a laptop): begun as
-- a copy of Gb.Spec.Ppu, which stays the reference; changed here for speed.
-- The same machine: gb-specfast runs it beside the reference on every
-- input.
{-|
The PPU of the Game Boy reference model (PLAN.md step 2.2), at dot level.

__Line timing__ (lines 0 to 143): mode 2 (OAM scan) for dots 0 to 79,
mode 3 from dot 80 until the 160th pixel is out, then mode 0 to dot 455.
Lines 144 to 153 are mode 1; entering line 144 requests the VBlank
interrupt and completes the frame. The mode a CPU access sees is the mode
of the next dot to be processed ('iDOT').

__Mode 3__ outputs one pixel per dot, after stalls that reproduce Pan
Docs' mode 3 length ("Rendering", "Mode 3 length"):

* 12 dots plus @SCX & 7@ before the first pixel (SCX & 7 is latched here);
* 6 dots before the first window pixel;
* before the first pixel of each object (by drawing priority: smaller X,
  then lower OAM index; X = 0 counts at pixel 0), Pan Docs' "OBJ penalty
  algorithm": if the BG or window tile holding the object's leftmost pixel
  wasn't already considered by the previous object, @max 0 (pixels of
  that tile right of it - 2)@, plus 6. X = 0 counts as its own tile with
  5 extra dots whatever SCX is (so 11, then 6 for each further object at
  X = 0: Mooneye's @intr_2_mode0_timing_sprites@). Only while LCDC bit 1
  is set.

So the minimum is 172 dots (pixels at dots 92 to 251). A pixel's colour
is computed when it is output, from the registers at that moment (SCX's
coarse part, SCY, LCDC, palettes, VRAM), so mid-line register writes take
effect at pixel granularity, though not with the fetcher's real 8-pixel
lag (Mealybug Tearoom is a stretch goal, docs/systems/gameboy.md).

__Window__ (Pan Docs, "Window"): the Y condition becomes true at the start
of a line with LY = WY and is cleared at VBlank; the window starts at pixel
WX - 7 (pixel 0, skipping 7 - WX columns, for WX < 7) when LCDC bits 5
and 0 are set, and its line counter advances once per line on which it
started. Not modelled: the WX = 0 fine-scroll shift, the WX = 166 bug, the
extra pixel when the window is disabled at a tile boundary.

__Objects__: up to 10 per line, chosen at the end of OAM scan in OAM order
by Y only (nothing if OAM DMA is running then). Per pixel, the first
opaque pixel among the line's objects in drawing priority order is the
object pixel; it is hidden by BG colours 1 to 3 if its priority bit is set
(Pan Docs, "Drawing priority": a hidden object still masks lower ones).

__LY__ (the register) shows the next line from dot 452, one M-cycle before
the line ends (Mooneye @hblank_ly_scx_timing-GS@), and 0 throughout line
153, so 153 is visible only in the last M-cycle of line 152 (Pan Docs'
LY = 153 quirk). The LY = LYC flag is clear in that M-cycle, is latched,
and freezes while the LCD is off, as does the STAT interrupt line
(@stat_lyc_onoff@). With STAT bit 5 set, line 144 raises the STAT
interrupt together with VBlank (@vblank_stat_intr-GS@). The CPU's VRAM
and OAM cut-offs are listed at 'vramReadBlocked'. __LCD on__: the first line has no
OAM scan (mode 0 for its first 80 dots), and at the end of the M-cycle
that wrote LCDC the PPU is at dot 3, so M-cycle boundaries fall on dots
3, 7, 11, ... of each line (Mooneye @lcdon_timing-GS@,
@intr_2_mode0_timing_sprites@). __LCD off__:
LY = 0, mode 0.
-}
module Gb.SpecFast.Ppu
  ( ppuTick
  , ppuQuietTicks
  , ppuRun
  , lcdcWrite
  , statRead
  , lyRead
  , updateStat
  , vramReadBlocked
  , vramWriteBlocked
  , oamReadBlocked
  , oamWriteBlocked
  ) where

import Prelude

import Control.Monad (forM_, when)
import Control.Monad.ST (ST)
import Data.Bits
import Data.Int (Int8)
import qualified Data.Vector.Unboxed.Mutable as MU
import Data.Word

import Gb.SpecFast.State

-- * Registers as the CPU sees them

lcdOn :: Machine s -> ST s Bool
lcdOn m = (`testBit` 7) <$> getSt m iLCDC
{-# INLINE lcdOn #-}

-- | LY as the CPU sees it: the next line's number from dot 452 (one
-- M-cycle before the line ends), and 0 for all of line 153 (so 153 shows
-- only in the last M-cycle of line 152).
lyReg :: Machine s -> ST s Int
lyReg m = do
  ly <- getSt m iLY
  dot <- getSt m iDOT
  pure (if ly == 153 then 0 else if dot >= 452 then ly + 1 else ly)
{-# INLINE lyReg #-}

lyRead :: Machine s -> ST s Word8
lyRead m = fromIntegral <$> lyReg m

-- | The LY = LYC flag, as the PPU's comparison computes it now: clear in
-- the last M-cycle of a line, while LY changes (Mooneye @lcdon_timing-GS@).
-- STAT shows the latched copy ('iLYCFLAG'), which the comparison updates
-- only while the LCD is on: switching the LCD off keeps it (Mooneye
-- @stat_lyc_onoff@).
compareLyc :: Machine s -> ST s ()
compareLyc m = do
  ly <- lyReg m
  lyc <- getSt m iLYC
  dot <- getSt m iDOT
  setSt m iLYCFLAG (if dot < 452 && ly == lyc then 1 else 0)

coincidence :: Machine s -> ST s Bool
coincidence m = (/= 0) <$> getSt m iLYCFLAG
{-# INLINE coincidence #-}

statRead :: Machine s -> ST s Word8
statRead m = do
  stat <- getSt m iSTAT
  md <- getSt m iPPUMODE
  on <- lcdOn m
  coinc <- coincidence m
  pure (fromIntegral (0x80 .|. stat .|. (if coinc then 4 else 0) .|. (if on then md else 0)))

-- | CPU access to VRAM and OAM (Mooneye @lcdon_timing-GS@ for reads,
-- @lcdon_write_timing-GS@ for writes). Reads and writes are cut off at
-- different dots:
--
-- * VRAM reads: blocked in mode 3 and from dot 76 (one M-cycle before
--   STAT shows mode 3); writes: only in mode 3.
-- * OAM reads: blocked in modes 2 and 3 and from dot 452 of the line before
--   a mode 2 line (when LY changes); writes: in mode 3 and in mode 2 except
--   its last M-cycle (dots 76 to 79).
-- * Both OAM reads and writes while OAM DMA copies.
--
-- None of the early cut-offs happen on the first line after LCD on, which
-- has no mode 2.
vramReadBlocked, vramWriteBlocked, oamReadBlocked, oamWriteBlocked :: Machine s -> ST s Bool
vramReadBlocked m = ppuIs m (\md _ dot -> md == 3 || (md == 2 && dot >= 76))
vramWriteBlocked m = ppuIs m (\md _ _ -> md == 3)
oamReadBlocked m = dmaOr m (ppuIs m (\md ly dot -> md >= 2 || (dot >= 452 && (ly < 143 || ly == 153))))
oamWriteBlocked m = dmaOr m (ppuIs m (\md _ dot -> md == 3 || (md == 2 && dot < 76)))

ppuIs :: Machine s -> (Int -> Int -> Int -> Bool) -> ST s Bool
ppuIs m f = do
  on <- lcdOn m
  if not on then pure False else f <$> getSt m iPPUMODE <*> getSt m iLY <*> getSt m iDOT
{-# INLINE ppuIs #-}

dmaOr :: Machine s -> ST s Bool -> ST s Bool
dmaOr m k = do
  d <- getSt m iDMAIDX
  if d >= 0 then pure True else k
{-# INLINE dmaOr #-}

-- | Re-evaluate the LY = LYC flag and the STAT interrupt line; a rising
-- edge requests the interrupt. While the LCD is off both are frozen. The
-- mode 2 source also fires in the first M-cycle of line 144, together with
-- VBlank (Mooneye @vblank_stat_intr-GS@); on the CGB from the M-cycle
-- before.
updateStat :: Machine s -> ST s ()
updateStat m = do
  on <- lcdOn m
  when on $ do
    compareLyc m
    stat <- getSt m iSTAT
    md <- getSt m iPPUMODE
    coinc <- coincidence m
    ly <- getSt m iLY
    dot <- getSt m iDOT
    -- on the Game Boy Color it starts one M-cycle earlier, in the last
    -- M-cycle of line 143 (Mooneye @vblank_stat_intr-C@: rounds 3 and 4
    -- one NOP shorter than on the DMG)
    let cgbEarly = mModel m == Cgb && ly == 143 && dot >= 452
        m2 = md == 2 || (ly == 144 && dot < 4) || cgbEarly
        line = (testBit stat 3 && md == 0) || (testBit stat 4 && md == 1)
               || (testBit stat 5 && m2) || (testBit stat 6 && coinc)
    old <- getSt m iSTATLINE
    when (line && old == 0) (modSt m iIF (.|. 0x02))
    setSt m iSTATLINE (if line then 1 else 0)

-- | A CPU write to LCDC.
lcdcWrite :: Machine s -> Word8 -> ST s ()
lcdcWrite m v = do
  old <- getSt m iLCDC
  setSt m iLCDC (fromIntegral v)
  when (testBit old 7 && not (testBit v 7)) $ do -- off
    setSt m iLY 0
    setSt m iDOT 0
    setSt m iPPUMODE 0
  when (not (testBit old 7) && testBit v 7) $ do -- on
    setSt m iLY 0
    setSt m iDOT 3 -- see "LCD on" above

    setSt m iPPUMODE 0
    setSt m iLCDON1 1
    setSt m iWYTRIG 0
    setSt m iWINLINE 0
    startLine m
  updateStat m

-- * Timing

-- | One M-cycle: four dots, then the STAT line.
ppuTick :: Machine s -> ST s ()
ppuTick m = do
  on <- lcdOn m
  -- the LCD keeps its speed: 2 dots per M-cycle in double speed (CGB)
  ds <- doubleSpeedP m
  when on $ do
    dotStep m
    dotStep m
    when (not ds) $ do
      dotStep m
      dotStep m
  updateStat m

-- | M-cycles the PPU may fall behind (the fast version's lazy PPU): those
-- before the next dot where something the CPU sees could change. Mode 2:
-- dot 80 (mode 3). Mode 3: its earliest end (a pixel a dot at most: 160
-- less the pixels out). Modes 0 and 1: dot 452 (LY as read, the LY = LYC
-- flag, the Color's early STAT), dot 456 (the next line: mode 2, VBlank),
-- and line 144's dot 4 (its mode 2 STAT source). None while OAM DMA copies
-- (it writes OAM as the scan reads it). The LCD off: no dots, nothing
-- changes (a write to LCDC catches the PPU up first).
ppuQuietTicks :: Machine s -> ST s Int
ppuQuietTicks m = do
  on <- lcdOn m
  dma <- (\i p -> i >= 0 || p > 0) <$> getSt m iDMAIDX <*> getSt m iDMAPEND
  if dma then pure 0 else if not on then pure 70224 else do
    md <- getSt m iPPUMODE
    d <- getSt m iDOT
    ly <- getSt m iLY
    px <- getSt m iPX
    ds <- doubleSpeedP m
    -- (mode 0 before dot 80: the first line after LCD on, mode 3 at dot 80)
    let e | md == 2 || (md == 0 && d < 80) = 80 - d
          | md == 3 = max 1 (160 - px)
          | ly == 144 && d < 4 = 4 - d
          | d < 452 = 452 - d
          | otherwise = 456 - d
        dpm = if ds then 2 else 4
    pure (max 0 ((e - 1) `div` dpm))

-- | @k@ M-cycles of the PPU (the fast version's catch-up), each as
-- 'ppuTick', but stretches where a dot does nothing at once: in modes 0
-- and 1 a dot only counts, and the STAT line can't change before dot 452
-- (dot 80 on the first line after LCD on, dot 4 of line 144), so the dots
-- jump there and STAT is evaluated once, as the last of those M-cycles
-- would; in mode 2 a dot only runs the OAM scan's entry (on even dots)
-- until dot 80, so the entries run in a loop. With the LCD off an M-cycle
-- does nothing at all.
ppuRun :: Machine s -> Int -> ST s ()
ppuRun m k0 = do
  on <- lcdOn m
  when on $ do
    ds <- doubleSpeedP m
    let dpm = if ds then 2 else 4
        go k = when (k > 0) $ do
          md <- getSt m iPPUMODE
          d <- getSt m iDOT
          ly <- getSt m iLY
          let limit | md == 2 || (md == 0 && d < 80) = 80
                    | ly == 144 && d < 4 = 4
                    | otherwise = 452
              -- M-cycles whose dots all end before the limit
              j = if md == 3 then 0 else (limit - 1 - d) `div` dpm
          if md == 3 then do
              -- mode 3: no STAT source, LY steady: its dots in one run
              -- ('mode3Run'), STAT evaluated only at the M-cycle that ends
              -- it (as each M-cycle's would be), that M-cycle's last dots
              -- (mode 0's, which only count) counted
              u <- mode3Run m (k * dpm)
              md' <- getSt m iPPUMODE
              -- (still in mode 3: all k M-cycles run. Ended, on any of
              -- their dots, the last one's included: STAT at that M-cycle)
              if md' == 3 then go 0 else do
                let used = (u + dpm - 1) `div` dpm
                    rest = used * dpm - u
                getSt m iDOT >>= setSt m iDOT . (+ rest)
                updateStat m
                go (k - used)
          else if j < 1 then ppuTick m >> go (k - 1) else do
            let t = min k j
                d' = d + t * dpm
            when (md == 2) $
              forM_ [d .. d' - 1] $ \x -> when (even x) (scanEntry m (x `shiftR` 1))
            setSt m iDOT d'
            updateStat m
            go (k - t)
    go k0

-- | Up to @n@ dots of mode 3 at once (the fast version's catch-up: no
-- register, VRAM or OAM can change inside it), as 'dotStep' and
-- 'mode3Dot' run them, in their order: a stall, or the window's start, or
-- an object's penalty, or a pixel; the registers read once. Stops at the
-- dot where mode 3 ends (HBlank entered as 'mode3Dot' does); the dots run.
mode3Run :: forall s. Machine s -> Int -> ST s Int
mode3Run m n = do
  lcdc <- getSt m iLCDC
  ly <- getSt m iLY
  wx <- fromIntegral <$> ramRead m 0xFF4B
  wyt <- getSt m iWYTRIG
  scx <- fromIntegral <$> ramRead m 0xFF43
  scy <- fromIntegral <$> ramRead m 0xFF42
  scxf <- getSt m iSCXF
  bgp <- ramRead m 0xFF47
  obp0 <- ramRead m 0xFF48
  obp1 <- ramRead m 0xFF49
  wl <- getSt m iWINLINE
  d0 <- getSt m iDOT
  st0 <- getSt m iSTALL
  x0 <- getSt m iPX
  let cgb = mCgbMode m
      colour = mModel m == Cgb
      bgMap = if testBit lcdc 3 then 0x9C00 else 0x9800
      winMap = if testBit lcdc 6 then 0x9C00 else 0x9800
      row = (ly + scy) .&. 0xFF
      base = ly * 160
      done i stall x = do
        setSt m iDOT (d0 + i)
        setSt m iSTALL stall
        setSt m iPX x
        pure i
      -- a background or window tile row, cached in the loop (key: as
      -- 'tileColour''s)
      -- (continuation-passing: the results go straight on, nothing boxed)
      tileAt :: Word16 -> Int -> Int -> Int -> Int -> Int -> Int
             -> (Int -> Int -> Int -> Int -> Int -> Int -> ST s Int) -> ST s Int
      tileAt mapBase col r tk tlo thi tat k = do
        let !mapA = mapBase + fromIntegral ((r `shiftR` 3) * 32 + (col `shiftR` 3) .&. 31) :: Word16
            !key = fromIntegral mapA .|. ((r .&. 7) `shiftL` 16) .|. (if testBit lcdc 4 then 0x80000 else 0)
            out !lo !hi !at =
              let !c = if testBit at 5 then 7 - col .&. 7 else col .&. 7
                  !b = 7 - c
                  !v = (if testBit lo b then 1 else 0) .|. (if testBit hi b then 2 else 0)
              in k v at key lo hi at
        if key == tk then out tlo thi tat else do
          t <- ramRead m mapA
          a0 <- if cgb then vramByte m True mapA else pure 0
          let tb | testBit lcdc 4 = 0x8000 + 16 * fromIntegral t
                 | otherwise = 0x9000 + 16 * fromIntegral (fromIntegral t :: Int8)
              rr = if testBit a0 6 then 7 - r .&. 7 else r .&. 7
              ad = tb + 2 * fromIntegral rr
          l <- vramByte m (testBit a0 3) ad
          h <- vramByte m (testBit a0 3) (ad + 1)
          out (fromIntegral l) (fromIntegral h) (fromIntegral a0)
      {-# INLINE tileAt #-}
      go !i !stall !x !tk !tlo !thi !tat
        | i >= n = done i stall x
        | stall > 0 = go (i + 1) (stall - 1) x tk tlo thi tat
        | otherwise = do
            winx <- getSt m iWINX
            let winStart = winx < 0 && wyt /= 0 && testBit lcdc 5 && testBit lcdc 0
                           && wx < 167 && x == max 0 (wx - 7)
            if winStart
              then do
                setSt m iWINX x
                setSt m iWINSHIFT (max 0 (7 - wx))
                go (i + 1) 5 x tk tlo thi tat -- 6 dots, this one included
              else do
                -- (objPenalty's own first test inline: no object left, or
                -- objects off)
                sk <- getSt m iSPRNEXT
                sn <- getSt m iNSPR
                pen <- if not (testBit lcdc 1) || sk >= sn then pure 0 else objPenalty m x
                if pen > 0 then go (i + 1) (pen - 1) x tk tlo thi tat else do
                  -- the pixel, as 'pixel'
                  let bgThen k
                        | not cgb && not (testBit lcdc 0) = k 0 0 tk tlo thi tat
                        | winx >= 0 && x >= winx && testBit lcdc 5 = do
                            wsh <- getSt m iWINSHIFT
                            tileAt winMap (x - winx + wsh) wl tk tlo thi tat k
                        | otherwise = tileAt bgMap (((scx .&. 0xF8) + scxf + x) .&. 0xFF) row tk tlo thi tat k
                  bgThen $ \ !bg !battr0 !tk' !tlo' !thi' !tat' -> do
                    let battr = fromIntegral battr0 :: Word8
                    (oc, attr) <- if testBit lcdc 1 then objPixel m x else pure (0, 0)
                    let objWins
                          | cgb = oc /= 0 && (not (testBit lcdc 0) || bg == 0 || not (testBit battr 7 || testBit attr 7))
                          | otherwise = oc /= 0 && not (testBit attr 7 && bg /= 0)
                        shade = if objWins then palette (if testBit attr 4 then obp1 else obp0) oc else palette bgp bg
                    MU.unsafeWrite (mFb m) (base + x) shade
                    when colour $ do
                      let off | cgb && objWins = 64 + 8 * fromIntegral (attr .&. 7) + 2 * oc
                              | cgb = 8 * fromIntegral (battr .&. 7) + 2 * bg
                              | objWins = 64 + (if testBit attr 4 then 8 else 0) + 2 * fromIntegral shade
                              | otherwise = 2 * fromIntegral shade
                      lo <- MU.unsafeRead (mPal m) off
                      hi <- MU.unsafeRead (mPal m) (off + 1)
                      MU.unsafeWrite (mFbC m) (base + x) ((fromIntegral hi `shiftL` 8 .|. fromIntegral lo) .&. 0x7FFF)
                    let x' = x + 1
                    if x' < 160 then go (i + 1) 0 x' tk' tlo' thi' tat' else do
                      -- HBlank, as 'mode3Dot'
                      setSt m iPPUMODE 0
                      act <- getSt m iHDMAACT
                      when (act /= 0) (setSt m iHDMAREQ 1)
                      wx0 <- getSt m iWINX
                      when (wx0 >= 0) (modSt m iWINLINE (+ 1))
                      done (i + 1) 0 x'
  go 0 st0 x0 (-1) 0 0 0

-- | Double speed (CGB mode, KEY1 bit 7); here as Gb.SpecFast.Machine imports
-- this module.
doubleSpeedP :: Machine s -> ST s Bool
doubleSpeedP m
  | mCgbMode m = (`testBit` 7) <$> getSt m iKEY1
  | otherwise = pure False

dotStep :: Machine s -> ST s ()
dotStep m = do
  md <- getSt m iPPUMODE
  d0 <- getSt m iDOT
  when (md == 2 && even d0) (scanEntry m (d0 `shiftR` 1))
  when (md == 3) (mode3Dot m)
  let d = d0 + 1
  if d == 456
    then do
      setSt m iDOT 0
      ly <- (\l -> if l == 153 then 0 else l + 1) <$> getSt m iLY
      setSt m iLY ly
      setSt m iLCDON1 0
      when (ly == 144) $ do
        setSt m iPPUMODE 1
        modSt m iIF (.|. 0x01)
        modSt m iFRAMES (+ 1)
        setSt m iWYTRIG 0
        setSt m iWINLINE 0
        MU.copy (mLast m) (mFb m)
        MU.copy (mLastC m) (mFbC m)
      when (ly < 144) (startLine m)
    else do
      setSt m iDOT d
      ly <- getSt m iLY
      when (d == 80 && ly < 144) (startMode3 m)

-- | Dot 0 of a visible line: mode 2 (or 0 on the first line after LCD
-- on), the window's Y condition.
startLine :: Machine s -> ST s ()
startLine m = do
  first <- getSt m iLCDON1
  setSt m iPPUMODE (if first /= 0 then 0 else 2)
  setSt m iNSPR 0
  ly <- getSt m iLY
  wy <- fromIntegral <$> ramRead m 0xFF4A
  when (ly == wy) (setSt m iWYTRIG 1)

-- | Dot 80: mode 3; the selected objects' tile rows are fetched now, with
-- the object size LCDC bit 2 has now.
startMode3 :: Machine s -> ST s ()
startMode3 m = do
  fetchObjRows m
  objLine m
  scx <- fromIntegral <$> ramRead m 0xFF43
  setSt m iPPUMODE 3
  setSt m iPX 0
  setSt m iSCXF (scx .&. 7)
  setSt m iSTALL (12 + scx .&. 7)
  setSt m iWINX (-1)
  setSt m iSPRNEXT 0
  setSt m iLASTTILE minBound

-- | OAM scan, one entry every second dot of mode 2 (entry @i@ at dot
-- @2 i@), as the hardware does: an entry whose rows cover LY (object size
-- from LCDC bit 2 now) is copied into the line's list ('mSpr', 'sprSlot')
-- while it has fewer than 10, kept in drawing priority order (X, then OAM
-- order). Nothing is selected while OAM DMA runs (Pan Docs, "OAM DMA
-- Transfer").
scanEntry :: Machine s -> Int -> ST s ()
scanEntry m i = do
  dma <- getSt m iDMAIDX
  n <- getSt m iNSPR
  when (dma < 0 && n < 10) $ do
    lcdc <- getSt m iLCDC
    ly <- getSt m iLY
    let a = 0xFE00 + fromIntegral (4 * i)
        h = if testBit lcdc 2 then 16 else 8
    y <- fromIntegral <$> ramRead m a
    when (ly + 16 >= y && ly + 16 < y + h) $ do
      x <- fromIntegral <$> ramRead m (a + 1)
      t <- fromIntegral <$> ramRead m (a + 2)
      at <- fromIntegral <$> ramRead m (a + 3)
      -- insert after every entry with X <= x
      let findPos k
            | k >= n = pure k
            | otherwise = do
                xk <- sprGet m k 1
                if xk > x then pure k else findPos (k + 1)
      pos <- findPos 0
      forM_ [n - 1, n - 2 .. pos] $ \k -> forM_ [0 .. 6] $ \f -> sprGet m k f >>= sprSet m (k + 1) f
      sprSet m pos 6 i
      sprSet m pos 0 y
      sprSet m pos 1 x
      sprSet m pos 2 t
      sprSet m pos 3 at
      setSt m iNSPR (n + 1)

-- | Field @f@ of line object @k@: 0 Y, 1 X, 2 tile, 3 attributes, 4 and 5
-- the tile row's low and high bytes ('fetchObjRows'), 6 its OAM index.
sprGet :: Machine s -> Int -> Int -> ST s Int
sprGet m k f = MU.unsafeRead (mSpr m) (7 * k + f)
{-# INLINE sprGet #-}

sprSet :: Machine s -> Int -> Int -> Int -> ST s ()
sprSet m k f = MU.unsafeWrite (mSpr m) (7 * k + f)
{-# INLINE sprSet #-}

-- | At the start of mode 3: each line object's tile row (VRAM can't change
-- during mode 3, so this equals fetching it when the object is drawn).
fetchObjRows :: Machine s -> ST s ()
fetchObjRows m = do
  n <- getSt m iNSPR
  lcdc <- getSt m iLCDC
  ly <- getSt m iLY
  let h = if testBit lcdc 2 then 16 else 8
  forM_ [0 .. n - 1] $ \k -> do
    oy <- sprGet m k 0
    t <- sprGet m k 2
    attr <- sprGet m k 3
    let r0 = (ly + 16 - oy) .&. (h - 1)
        r = if testBit attr 6 then h - 1 - r0 else r0
        tile = if h == 16 then (t .&. 0xFE) + (r `shiftR` 3) else t
        a = 0x8000 + fromIntegral (16 * tile + 2 * (r .&. 7))
        -- CGB mode: the tile in VRAM bank 1 if attribute bit 3 is set
        bank1 = mCgbMode m && testBit attr 3
    vramByte m bank1 a >>= sprSet m k 4 . fromIntegral
    vramByte m bank1 (a + 1) >>= sprSet m k 5 . fromIntegral

-- | One dot of mode 3: a stall, or the window start, or an object's
-- penalty, or a pixel.
mode3Dot :: Machine s -> ST s ()
mode3Dot m = do
  st <- getSt m iSTALL
  if st > 0 then setSt m iSTALL (st - 1) else do
    x <- getSt m iPX
    lcdc <- getSt m iLCDC
    wx <- fromIntegral <$> ramRead m 0xFF4B
    wyt <- getSt m iWYTRIG
    winx <- getSt m iWINX
    let winStart = winx < 0 && wyt /= 0 && testBit lcdc 5 && testBit lcdc 0
                   && wx < 167 && x == max 0 (wx - 7)
    if winStart
      then do
        setSt m iWINX x
        setSt m iWINSHIFT (max 0 (7 - wx))
        setSt m iSTALL 5 -- 6 dots, this one included
      else do
        pen <- objPenalty m x
        if pen > 0 then setSt m iSTALL (pen - 1) else do
          pixel m x
          let x' = x + 1
          setSt m iPX x'
          when (x' == 160) $ do
            setSt m iPPUMODE 0
            -- HBlank: an HBlank DMA's block is due (Gb.SpecFast.Machine.hdmaPending)
            act <- getSt m iHDMAACT
            when (act /= 0) (setSt m iHDMAREQ 1)
            wx0 <- getSt m iWINX
            when (wx0 >= 0) (modSt m iWINLINE (+ 1))

-- | The penalty of the next object due at pixel @x@ (0 if none), which is
-- then marked as done.
objPenalty :: Machine s -> Int -> ST s Int
objPenalty m x = do
  lcdc <- getSt m iLCDC
  k <- getSt m iSPRNEXT
  n <- getSt m iNSPR
  if not (testBit lcdc 1) || k >= n then pure 0 else do
    ox <- sprGet m k 1
    if ox >= 168 then setSt m iSPRNEXT n >> pure 0 -- the rest are off screen too
    else if max 0 (ox - 8) < x then setSt m iSPRNEXT (k + 1) >> objPenalty m x -- passed while objects were off
    else if max 0 (ox - 8) > x then pure 0
    else do
      setSt m iSPRNEXT (k + 1)
      let sx = ox - 8
      winx <- getSt m iWINX
      wsh <- getSt m iWINSHIFT
      scx <- fromIntegral <$> ramRead m 0xFF43
      let (key, col) | ox == 0 = (minBound + 1, 0) -- X = 0: 11 dots whatever SCX
                     | winx >= 0 && sx >= winx = let c = sx - winx + wsh in (0x10000 + c `shiftR` 3, c .&. 7)
                     | otherwise = let c = sx + scx in (c `shiftR` 3, c .&. 7)
      lastKey <- getSt m iLASTTILE
      setSt m iLASTTILE key
      pure (6 + (if key /= lastKey then max 0 (7 - col - 2) else 0))

-- | Output pixel @x@ of the current line into the frame (shades, and on
-- the Game Boy Color the colour frame too, from palette RAM).
--
-- __CGB mode__ (Pan Docs, "BG Map Attributes", "Drawing priority"): the BG
-- and window are always drawn, with their tile's attributes from VRAM
-- bank 1 (palette, bank, flips, priority); LCDC bit 0 is the master
-- priority: when clear, objects are always on top; when set, an object
-- pixel loses to BG colours 1 to 3 if the BG attribute's bit 7 or the
-- object's bit 7 is set. Objects use palettes 0 to 7 (attribute bits 0 to
-- 2) and their tile's bank (bit 3).
--
-- __DMG compatibility mode__: the DMG's rules; BGP, OBP0 and OBP1 pick a
-- colour of BG palette 0 and OBJ palettes 0 and 1.
pixel :: Machine s -> Int -> ST s ()
pixel m x = do
  lcdc <- getSt m iLCDC
  ly <- getSt m iLY
  let cgb = mCgbMode m
  (bg, battr) <- if not cgb && not (testBit lcdc 0) then pure (0, 0) else do
    winx <- getSt m iWINX
    if winx >= 0 && x >= winx && testBit lcdc 5
      then do
        wsh <- getSt m iWINSHIFT
        wl <- getSt m iWINLINE
        tileColour m lcdc (if testBit lcdc 6 then 0x9C00 else 0x9800) (x - winx + wsh) wl
      else do
        scx <- fromIntegral <$> ramRead m 0xFF43
        scy <- fromIntegral <$> ramRead m 0xFF42
        f <- getSt m iSCXF
        tileColour m lcdc (if testBit lcdc 3 then 0x9C00 else 0x9800)
          (((scx .&. 0xF8) + f + x) .&. 0xFF) ((ly + scy) .&. 0xFF)
  (oc, attr) <- if testBit lcdc 1 then objPixel m x else pure (0, 0)
  let objWins
        | cgb = oc /= 0 && (not (testBit lcdc 0) || bg == 0 || not (testBit battr 7 || testBit attr 7))
        | otherwise = oc /= 0 && not (testBit attr 7 && bg /= 0)
  shade <- if objWins
    then palette <$> ramRead m (if testBit attr 4 then 0xFF49 else 0xFF48) <*> pure oc
    else palette <$> ramRead m 0xFF47 <*> pure bg
  MU.unsafeWrite (mFb m) (ly * 160 + x) shade
  when (mModel m == Cgb) $ do
    -- the palette RAM byte offset of the colour
    let off | cgb && objWins = 64 + 8 * fromIntegral (attr .&. 7) + 2 * oc
            | cgb = 8 * fromIntegral (battr .&. 7) + 2 * bg
            | objWins = 64 + (if testBit attr 4 then 8 else 0) + 2 * fromIntegral shade
            | otherwise = 2 * fromIntegral shade
    lo <- MU.unsafeRead (mPal m) off
    hi <- MU.unsafeRead (mPal m) (off + 1)
    MU.unsafeWrite (mFbC m) (ly * 160 + x) ((fromIntegral hi `shiftL` 8 .|. fromIntegral lo) .&. 0x7FFF)

-- | A VRAM byte for the PPU: bank 1 ('mVram1') or bank 0.
vramByte :: Machine s -> Bool -> Word16 -> ST s Word8
vramByte m bank1 a
  | bank1 = MU.unsafeRead (mVram1 m) (fromIntegral a - 0x8000)
  | otherwise = ramRead m a
{-# INLINE vramByte #-}

palette :: Word8 -> Int -> Word8
palette p c = (p `shiftR` (2 * c)) .&. 3
{-# INLINE palette #-}

-- | Colour index of BG/window map pixel (column, row) of the 256 × 256
-- map, and the tile's attributes (CGB mode: the byte at the same place in
-- VRAM bank 1, with the tile data's bank and the flips applied here; 0
-- otherwise).
tileColour :: Machine s -> Int -> Word16 -> Int -> Int -> ST s (Int, Word8)
tileColour m lcdc mapBase col row = do
  let mapA = mapBase + fromIntegral ((row `shiftR` 3) * 32 + (col `shiftR` 3) .&. 31)
      -- (the fast version: the tile row cached for its 8 pixels; the key
      -- holds all the row depends on, its attribute byte being at mapA too)
      key = fromIntegral mapA .|. ((row .&. 7) `shiftL` 16) .|. (if testBit lcdc 4 then 0x80000 else 0)
  k <- getSt m iTCKEY
  (lo, hi, attr) <- if k == key
    then (\l h a -> (l, h, a)) <$> getSt m iTCLO <*> getSt m iTCHI <*> getSt m iTCATTR
    else do
      t <- ramRead m mapA
      at <- if mCgbMode m then vramByte m True mapA else pure 0
      let base | testBit lcdc 4 = 0x8000 + 16 * fromIntegral t
               | otherwise = 0x9000 + 16 * fromIntegral (fromIntegral t :: Int8)
          r = if testBit at 6 then 7 - row .&. 7 else row .&. 7
          a = base + 2 * fromIntegral r
      l <- fromIntegral <$> vramByte m (testBit at 3) a
      h <- fromIntegral <$> vramByte m (testBit at 3) (a + 1)
      setSt m iTCKEY key
      setSt m iTCLO l
      setSt m iTCHI h
      setSt m iTCATTR (fromIntegral at)
      pure (l, h, fromIntegral at)
  let c = if testBit attr 5 then 7 - col .&. 7 else col .&. 7
      b = 7 - c
      v = (if testBit lo b then 1 else 0) .|. (if testBit hi b then 2 else 0)
  pure (v, fromIntegral attr)

-- | Colour index of pixel @c@ (0 = leftmost) of the tile row at this
-- address (in VRAM bank 1 if @bank1@).
tileRowColour :: Machine s -> Bool -> Word16 -> Int -> ST s Int
tileRowColour m bank1 a c = do
  lo <- vramByte m bank1 a
  hi <- vramByte m bank1 (a + 1)
  let b = 7 - c
  pure ((if testBit lo b then 1 else 0) .|. (if testBit hi b then 2 else 0))

-- | The line's object pixels, once at mode 3's start (the fast version's:
-- the line's objects and their tile rows don't change during mode 3), by
-- 'objPixelAt''s rules: on the DMG (and with the Color's DMG priority) the
-- first object in drawing order with an opaque pixel there, on the Color
-- the opaque one first in OAM. Packed: colour (bits 0-1), attributes
-- (2-9), the object's OAM index + 1 (10-; 0: no pixel).
objLine :: Machine s -> ST s ()
objLine m = do
  let buf = mObjLine m
  MU.set buf 0
  n <- getSt m iNSPR
  opri <- getSt m iOPRI
  let byOam = mCgbMode m && opri == 0
  forM_ [0 .. n - 1] $ \k -> do
    ox <- sprGet m k 1
    attr <- sprGet m k 3
    lo <- sprGet m k 4
    hi <- sprGet m k 5
    i <- sprGet m k 6
    forM_ [max 0 (ox - 8) .. min 159 (ox - 1)] $ \x -> do
      let c0 = x - (ox - 8)
          b = if testBit attr 5 then c0 else 7 - c0
          col = (if testBit lo b then 1 else 0) .|. (if testBit hi b then 2 else 0)
      when (col /= 0) $ do
        cur <- MU.unsafeRead buf x
        let take' = cur == 0 || (byOam && i + 1 < cur `shiftR` 10)
        when take' (MU.unsafeWrite buf x (col .|. ((attr .&. 0xFF) `shiftL` 2) .|. ((i + 1) `shiftL` 10)))

-- | The object pixel at @x@ (the fast version: from 'objLine').
objPixel :: Machine s -> Int -> ST s (Int, Word8)
objPixel m x = do
  v <- MU.unsafeRead (mObjLine m) x
  pure (if v == 0 then (0, 0) else (v .&. 3, fromIntegral ((v `shiftR` 2) .&. 0xFF)))
{-# INLINE objPixel #-}

-- | The object pixel at @x@, as the reference works it out per pixel:
-- colour index (0 = none) and attributes. ('objLine' paints the same.)
_objPixelAt :: Machine s -> Int -> ST s (Int, Word8)
_objPixelAt m x = do
  n <- getSt m iNSPR
  let go k
        | k >= n = pure (0, 0)
        | otherwise = do
            ox <- sprGet m k 1
            if x < ox - 8 || x >= ox then go (k + 1) else do
              attr <- sprGet m k 3
              lo <- sprGet m k 4
              hi <- sprGet m k 5
              let c0 = x - (ox - 8)
                  b = if testBit attr 5 then c0 else 7 - c0
                  col = (if testBit lo b then 1 else 0) .|. (if testBit hi b then 2 else 0)
              if col == 0 then go (k + 1) else pure (col, fromIntegral attr)
      -- CGB priority (OPRI = 0): of the opaque objects here, the one first
      -- in OAM (Pan Docs, "Drawing priority")
      best k cur
        | k >= n = pure (maybe (0, 0) (\(_, c, a) -> (c, a)) cur)
        | otherwise = do
            ox <- sprGet m k 1
            if x < ox - 8 || x >= ox then best (k + 1) cur else do
              attr <- sprGet m k 3
              lo <- sprGet m k 4
              hi <- sprGet m k 5
              i <- sprGet m k 6
              let c0 = x - (ox - 8)
                  b = if testBit attr 5 then c0 else 7 - c0
                  col = (if testBit lo b then 1 else 0) .|. (if testBit hi b then 2 else 0)
                  better = case cur of
                    Just (j, _, _) -> i < j
                    Nothing -> True
              best (k + 1) (if col /= 0 && better then Just (i, col, fromIntegral attr) else cur)
  opri <- getSt m iOPRI
  if mCgbMode m && opri == 0 then best 0 Nothing else go 0
