{-# OPTIONS_GHC -fworker-wrapper -fspecialise-aggressively #-}
{-# LANGUAGE MultiWayIf #-}
-- (the fast version is never synthesised: GHC's unboxing back on, which
-- clash-common's -fno-worker-wrapper turns off for Clash's sake)
-- Side track Q (PLAN.md Q.5): the desktop emulator's fast version of
-- Snes.Spec.Ppu, kept beside it (the original stays the reference the
-- core's lockstep checks; a fix in either goes to both). Same machine:
-- the snes-specfast suite runs both side by side and compares them.
{-|
The picture (PLAN.md step 7.5; docs/systems/snes.md): the two-chip PPU's
output, a pixel at a time as its dot passes ('ppuRenderTo', called by
"Snes.Spec.Machine" as time advances), from the registers and memories as
they are at that moment: pixel x at master clock 4 (x + 23), before any
write at that clock; the sprites of a line are worked out in the H-blank
of the line before ('ppuSprites', master clock 1120), as the chip does.
These rules (with CGRAM and OAM taking no writes while the PPU reads
them: "Snes.Spec.Machine") are what a pipelined circuit can match
exactly (step 7.6).

Sources: fullsnes ("SNES PPU ..."), anomie's register documents (the
priority orders, offset-per-tile), the SNESdev wiki.

* BG modes 0 to 6 are fetched a tile ahead, as the chip does: a line's
  33 columns, each in 8 slots of one VRAM word a dot ('fetchTable', dots
  6-269), from the registers at each slot, kept in a ring of 8 columns
  a layer ('colIx'); a layer's fine scroll is
  taken at its first column, so a scroll written mid-line takes effect
  at the next column. Mode 7 reads its two bytes per pixel.
* BG modes 0 to 7: 2/4/8 bits per pixel, 8×8 and 16×16 tiles, the four
  map sizes, scrolling, flips; offset-per-tile (modes 2, 4, 6); mosaic;
  Mode 7 (the matrix with fullsnes's rounding, screen over, flips,
  EXTBG); direct colour.
* Sprites: 128 in OAM, the eight sizes, 32 a line (range over) and 34
  tiles a line (time over: the tiles are fetched from the last sprite
  found to the first, so the first lose theirs), the lowest index in
  front of the others, priority rotation.
* The windows (two, per layer and for colour maths, with their logic),
  the main and sub screens, colour maths (add or subtract, half, the
  fixed colour, forced black, its windows), brightness, forced blank.
* Hi-res: modes 5 and 6 (BG pixels at half dots: the sub screen's on the
  left half, the main screen's on the right) and pseudo-hires (SETINI
  bit 3); in both, colour maths uses the fixed colour, and each half is
  its own screen's pixel (*to verify*). Interlace: each field is drawn on
  its own; modes 5 and 6 then use the field's half of a 448-line picture.

The frame is 512 × 239 15-bit colours ('ppuFrame'): a low-res pixel is
written twice. Output row r is the line V = r + 1.
-}
module Snes.SpecFast.Ppu
  ( Ppu
  , newPpu
  , ppuOwnFields
  , ppuReload
  , ppuStartLine
  , ppuSprites
  , ppuClearSprites
  , ppuRenderTo
  , ppuFrame
  , ppuStat77
  , ppuClearFlags
  , ppuObjLine
  , ppuRegW
  , ppuRegR
  , ppuVramW
  , ppuCgW
  , ppuSetObjLine
  , Slot (..)
  , fetchTable
  , fetchSlotOf
  , frameWidth
  , frameHeight
  ) where

import Prelude

import Control.Monad (forM_, when)
import Control.Monad.ST (ST)
import Data.Bits
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MV
import Platform.Snapshot (Field (..))
import Data.Word

data Ppu s = Ppu
  { pVram   :: !(MV.MVector s Word8)
  , pCgram  :: !(MV.MVector s Word8)
  , pOam    :: !(MV.MVector s Word8)
  , pRegs   :: !(MV.MVector s Int)
  , pFrame  :: !(MV.MVector s Word16)
  , pObj    :: !(MV.MVector s Int)   -- ^ the line's sprite pixels: 0 none, else idx | pal << 4 | pri << 7
  , pCol    :: !(MV.MVector s Int)   -- ^ the fetched columns: 'colIx'
  , pFine   :: !(MV.MVector s Int)   -- ^ each layer's fine scroll for the line
  , pSt     :: !(MV.MVector s Int)
  , pLay    :: !(MV.MVector s Int)   -- ^ side track Q: the BG layers at the pixel ('layers'): 4 x (index, palette, priority, colour code: 'colourOf'), the count at 16
  , pSpr    :: !(MV.MVector s Int)   -- ^ side track W: 'ppuSprites'' scratch, the line's first 32 sprites found (OAM index, x, y, w, h); not state
  }

-- A column's slots: 0 the map entry, 1 and 2 the left and right char
-- tiles, 7 the row within them, 3-6 the char words in fetch order.
colSize :: Int
colSize = 8

frameWidth, frameHeight :: Int
frameWidth = 512
frameHeight = 239

-- State: the next dot to do, the overflow flags (STAT77 bits 6, 7).
stX, stFlags :: Int
stX = 0
stFlags = 1

-- | The PPU over the machine's memories and register file (2100h-213Fh as
-- written, scroll and Mode 7 values at the slots "Snes.Spec.Machine"
-- documents).
newPpu :: MV.MVector s Word8 -> MV.MVector s Word8 -> MV.MVector s Word8 -> MV.MVector s Int -> ST s (Ppu s)
newPpu vram cg oam regs = do
  fr <- MV.replicate (frameWidth * frameHeight) 0
  obj <- MV.replicate 256 0
  col <- MV.replicate (5 * 8 * colSize) 0
  fine <- MV.replicate 4 0
  st <- MV.replicate 4 0
  lay <- MV.replicate 17 0
  spr <- MV.replicate (32 * 5) 0
  pure (Ppu vram cg oam regs fr obj col fine st lay spr)

reg :: Ppu s -> Int -> ST s Int
reg p = MV.unsafeRead (pRegs p)

ppuFrame :: Ppu s -> ST s (U.Vector Word16)
ppuFrame p = U.freeze (pFrame p)

-- | STAT77's flags: bit 7 time over, bit 6 range over.
ppuStat77 :: Ppu s -> ST s Int
ppuStat77 p = MV.read (pSt p) stFlags

-- | The line's sprite pixels, and setting them (the fast version's
-- addition: "Snes.SpecFast.RenderLog" hands them to its render thread).
ppuObjLine :: Ppu s -> ST s (U.Vector Int)
ppuObjLine p = U.freeze (pObj p)

ppuSetObjLine :: Ppu s -> U.Vector Int -> ST s ()
ppuSetObjLine p = U.imapM_ (MV.write (pObj p))

-- | Writes into the PPU's own registers, VRAM and CGRAM (the render
-- thread's copy; the machine writes the vectors it shares).
-- | A register as the PPU holds it (side track W: the replayer's view).
ppuRegR :: Ppu s -> Int -> ST s Int
ppuRegR = reg

ppuRegW :: Ppu s -> Int -> Int -> ST s ()
ppuRegW p = MV.write (pRegs p)

ppuVramW, ppuCgW :: Ppu s -> Int -> Word8 -> ST s ()
ppuVramW p = MV.write (pVram p)
ppuCgW p = MV.write (pCgram p)

-- | At the end of V-blank (not in forced blank; fullsnes).
ppuClearFlags :: Ppu s -> ST s ()
ppuClearFlags p = MV.write (pSt p) stFlags 0

vramWord :: Ppu s -> Int -> ST s Int
vramWord p a = do
  let b = (a .&. 0x7FFF) * 2
  lo <- MV.unsafeRead (pVram p) b
  hi <- MV.unsafeRead (pVram p) (b + 1)
  pure $! fromIntegral lo .|. fromIntegral hi `shiftL` 8

cgColor :: Ppu s -> Int -> ST s Int
cgColor p i = do
  lo <- MV.unsafeRead (pCgram p) (i * 2)
  hi <- MV.unsafeRead (pCgram p) (i * 2 + 1)
  pure $! (fromIntegral lo .|. fromIntegral hi `shiftL` 8) .&. 0x7FFF

-- * Sprites

-- | Small and large sizes (width, height) by OBSEL bits 5-7.
objSizes :: Int -> ((Int, Int), (Int, Int))
objSizes k = case k .&. 7 of
  0 -> ((8, 8), (16, 16))
  1 -> ((8, 8), (32, 32))
  2 -> ((8, 8), (64, 64))
  3 -> ((16, 16), (32, 32))
  4 -> ((16, 16), (64, 64))
  5 -> ((32, 32), (64, 64))
  6 -> ((16, 32), (32, 64))
  _ -> ((16, 32), (32, 32))

-- | A visible line begins: its pixels from x = 0.
ppuStartLine :: Ppu s -> ST s ()
ppuStartLine p = MV.write (pSt p) stX 0

-- | In forced blank at master clock 1120: no sprites on the next line.
ppuClearSprites :: Ppu s -> ST s ()
ppuClearSprites p = MV.set (pObj p) 0

-- | In line V's H-blank (master clock 1120), the sprites of line V + 1
-- (output row V; the machine's OAM address given for the priority
-- rotation) into the line buffer, which line V has finished reading.
ppuSprites :: Ppu s -> Int -> Int -> ST s ()
ppuSprites p row oamRel = do
  MV.set (pObj p) 0
  obsel <- reg p 0x01
  rot <- reg p 0x03
  let ((sw, sh), (lw, lh)) = objSizes (obsel `shiftR` 5)
      first = if testBit rot 7 then (oamRel `shiftR` 2) .&. 0x7F else 0
      spr = pSpr p
      -- (side track W: strict loops over a scratch array, not lists of
      -- the 128 entries and their slivers built on every line, which the
      -- browser paid for in garbage; the same order and limits)
      -- the entries on the line, in OAM order from the first: the first 32
      -- kept, all counted
      scan !k !nf
        | k >= 128 = pure nf
        | otherwise = do
            let i = (first + k) .&. 0x7F
            x0 <- fromIntegral <$> MV.read (pOam p) (i * 4)
            y <- fromIntegral <$> MV.read (pOam p) (i * 4 + 1)
            hi <- fromIntegral <$> MV.read (pOam p) (0x200 + i `shiftR` 2)
            let bits = (hi :: Int) `shiftR` ((i .&. 3) * 2)
                x9 = x0 .|. ((bits .&. 1) `shiftL` 8)
                x = if x9 >= 256 then x9 - 512 else x9
                big = testBit bits 1
                w = if big then lw else sw
                h = if big then lh else sh
            if (row - y) .&. 0xFF < h && (x > negate w || x == -256) && x < 256
              then do
                when (nf < 32) $ do
                  MV.unsafeWrite spr (nf * 5) i
                  MV.unsafeWrite spr (nf * 5 + 1) x
                  MV.unsafeWrite spr (nf * 5 + 2) y
                  MV.unsafeWrite spr (nf * 5 + 3) w
                  MV.unsafeWrite spr (nf * 5 + 4) h
                scan (k + 1) (nf + 1)
              else scan (k + 1) nf
  nf <- scan (0 :: Int) (0 :: Int)
  when (nf > 32) $ MV.modify (pSt p) (.|. 0x40) stFlags
  let base = (obsel .&. 7) `shiftL` 13
      gap = (((obsel `shiftR` 3) .&. 3) + 1) `shiftL` 12
      -- the tiles (8-pixel slivers on the screen), from the last sprite
      -- kept to the first, left to right; the first 34 drawn, all counted
      walk !e !c !cnt
        | e < 0 = pure cnt
        | otherwise = do
            i <- MV.unsafeRead spr (e * 5)
            x <- MV.unsafeRead spr (e * 5 + 1)
            y <- MV.unsafeRead spr (e * 5 + 2)
            w <- MV.unsafeRead spr (e * 5 + 3)
            h <- MV.unsafeRead spr (e * 5 + 4)
            if c >= w `div` 8 then walk (e - 1) 0 cnt else do
              let sx0 = x + c * 8
              if sx0 > -8 && sx0 < 256
                then do
                  when (cnt < 34) (sliver i x y w h c)
                  walk e (c + 1) (cnt + 1)
                else walk e (c + 1) cnt
      sliver i x y w h c = do
        tileNo <- fromIntegral <$> MV.read (pOam p) (i * 4 + 2)
        attr <- fromIntegral <$> MV.read (pOam p) (i * 4 + 3)
        let n = tileNo .|. ((attr .&. 1) `shiftL` 8) :: Int
            vflip = testBit attr 7
            hflip = testBit attr 6
            pri = (attr `shiftR` 4) .&. 3
            pal = (attr `shiftR` 1) .&. 7
            yr0 = (row - y) .&. 0xFF
            yr = if vflip then h - 1 - yr0 else yr0
            col = if hflip then w `div` 8 - 1 - c else c
            tx = ((n .&. 0xF) + col) .&. 0xF
            ty = (((n `shiftR` 4) .&. 0xF) + yr `shiftR` 3) .&. 0xF
            addr = base + (if testBit n 8 then gap else 0) + (ty * 16 + tx) * 16 + (yr .&. 7)
        w01 <- vramWord p addr
        w23 <- vramWord p (addr + 8)
        let px !k
              | k > 7 = pure ()
              | otherwise = do
                  let sx = x + c * 8 + k
                      b = if hflip then k else 7 - k
                      idx = ((w01 `shiftR` b) .&. 1) .|. (((w01 `shiftR` (b + 8)) .&. 1) `shiftL` 1)
                            .|. (((w23 `shiftR` b) .&. 1) `shiftL` 2) .|. (((w23 `shiftR` (b + 8)) .&. 1) `shiftL` 3)
                  when (sx >= 0 && sx < 256 && idx /= 0) $
                    MV.write (pObj p) sx (idx .|. (pal `shiftL` 4) .|. (pri `shiftL` 7))
                  px (k + 1)
        px (0 :: Int)
  cnt <- walk (min 32 nf - 1) (0 :: Int) (0 :: Int)
  when (cnt > 34) $ MV.modify (pSt p) (.|. 0x80) stFlags

-- * Background layers

-- | A BG pixel: colour index (0 = transparent), palette, priority bit.
data Px = Px !Int !Int !Int

-- | What a fetch slot does: an offset-per-tile entry (H or V), a layer's
-- map entry, or a layer's k-th char word.
data Slot = SOptH | SOptV | SMap !Int | SChar !Int !Int | SNone
  deriving stock (Eq, Show)

-- | The eight fetch slots of a column's 8 dots, by BG mode (the
-- bandwidth the chip has: one VRAM word a dot). Modes 5 and 6 fetch both
-- 8-pixel halves of a 16-pixel hi-res tile: words in the order left
-- planes, then right planes.
fetchTable :: Int -> [Slot]
fetchTable mode = case mode of
  0 -> [SMap 0, SMap 1, SMap 2, SMap 3, SChar 0 0, SChar 1 0, SChar 2 0, SChar 3 0]
  1 -> [SMap 0, SMap 1, SMap 2, SChar 0 0, SChar 0 1, SChar 1 0, SChar 1 1, SChar 2 0]
  2 -> [SOptH, SOptV, SMap 0, SMap 1, SChar 0 0, SChar 0 1, SChar 1 0, SChar 1 1]
  3 -> [SMap 0, SMap 1, SChar 0 0, SChar 0 1, SChar 0 2, SChar 0 3, SChar 1 0, SChar 1 1]
  4 -> [SOptH, SMap 0, SMap 1, SChar 0 0, SChar 0 1, SChar 0 2, SChar 0 3, SChar 1 0]
  5 -> [SMap 0, SMap 1, SChar 0 0, SChar 0 1, SChar 0 2, SChar 0 3, SChar 1 0, SChar 1 1]
  6 -> [SOptH, SOptV, SMap 0, SChar 0 0, SChar 0 1, SChar 0 2, SChar 0 3, SNone]
  _ -> replicate 8 SNone

-- | 'fetchTable''s slot @s@ of mode @mode@ as nested cases (side track W:
-- not a boxed table of 'Slot's, a top-level value entered on every fetch
-- dot; inlined, each case's constructor is known where it is matched).
-- The same table: snes-specfast's test checks them equal.
fetchSlotOf :: Int -> Int -> Slot
fetchSlotOf mode s = case mode of
  0 -> case s of
    0 -> SMap 0
    1 -> SMap 1
    2 -> SMap 2
    3 -> SMap 3
    4 -> SChar 0 0
    5 -> SChar 1 0
    6 -> SChar 2 0
    _ -> SChar 3 0
  1 -> case s of
    0 -> SMap 0
    1 -> SMap 1
    2 -> SMap 2
    3 -> SChar 0 0
    4 -> SChar 0 1
    5 -> SChar 1 0
    6 -> SChar 1 1
    _ -> SChar 2 0
  2 -> case s of
    0 -> SOptH
    1 -> SOptV
    2 -> SMap 0
    3 -> SMap 1
    4 -> SChar 0 0
    5 -> SChar 0 1
    6 -> SChar 1 0
    _ -> SChar 1 1
  3 -> case s of
    0 -> SMap 0
    1 -> SMap 1
    2 -> SChar 0 0
    3 -> SChar 0 1
    4 -> SChar 0 2
    5 -> SChar 0 3
    6 -> SChar 1 0
    _ -> SChar 1 1
  4 -> case s of
    0 -> SOptH
    1 -> SMap 0
    2 -> SMap 1
    3 -> SChar 0 0
    4 -> SChar 0 1
    5 -> SChar 0 2
    6 -> SChar 0 3
    _ -> SChar 1 0
  5 -> case s of
    0 -> SMap 0
    1 -> SMap 1
    2 -> SChar 0 0
    3 -> SChar 0 1
    4 -> SChar 0 2
    5 -> SChar 0 3
    6 -> SChar 1 0
    _ -> SChar 1 1
  6 -> case s of
    0 -> SOptH
    1 -> SOptV
    2 -> SMap 0
    3 -> SChar 0 0
    4 -> SChar 0 1
    5 -> SChar 0 2
    6 -> SChar 0 3
    _ -> SNone
  _ -> SNone
{-# INLINE fetchSlotOf #-}

-- | 'fetchTable' as one table, mode * 8 + slot (side track Q: not `!!` on
-- a list every fetch dot).
fetchSlots :: V.Vector Slot
fetchSlots = V.fromList (concatMap fetchTable [0 .. 7])
{-# NOINLINE fetchSlots #-}

-- | Bits per pixel of each layer by mode.
bppOf :: Int -> Int -> Int
bppOf mode n = case (mode, n) of
  (0, _) -> 2
  (1, 2) -> 2
  (1, _) -> 4
  (2, _) -> 4
  (3, 0) -> 8
  (3, _) -> 4
  (4, 0) -> 8
  (4, _) -> 2
  (5, 0) -> 4
  (5, _) -> 2
  _ -> 4

-- | Slot k of column j of layer n (4: the offset-per-tile entries, slots
-- 0 and 1, H and V). A ring of 8 columns a layer, as the circuit keeps
-- them ("Snes.Ppu"): the pixels read at most 3 columns behind the fetch,
-- and what a column holds that this line has not fetched (after a
-- mid-line mode change) is then the same in both: the column 8 before,
-- or an earlier line's.
colIx :: Int -> Int -> Int -> Int
colIx n j k = (n * 8 + (j .&. 7)) * colSize + k

-- | A BG map's entry address for tile (tx, ty) under its SC register.
{-# INLINE mapAddr #-}
mapAddr :: Int -> Int -> Int -> Int
mapAddr sc tx0 ty0 =
  let wide = testBit sc 0
      tall = testBit sc 1
      tx = tx0 .&. (if wide then 63 else 31)
      ty = ty0 .&. (if tall then 63 else 31)
  in ((sc `shiftR` 2) `shiftL` 10) + (ty .&. 31) * 32 + (tx .&. 31)
     + (if tx .&. 32 /= 0 then 0x400 else 0)
     + (if ty .&. 32 /= 0 then (if wide then 0x800 else 0x400) else 0)

-- | Fetch slot @s@ of column @j@ on line V, from the registers as they
-- are now (dot 6 + 8j + s of the line).
fetchSlot :: Ppu s -> Ctx -> Int -> Int -> Int -> Int -> ST s ()
fetchSlot p c v field j s = do
  -- (side track W: BGMODE, MOSAIC and SETINI from the stretch's 'Ctx')
  let bgmode = cBgmode c
      mode = cMode c
      hiresMode = mode == 5 || mode == 6
  case fetchSlotOf mode s of
    SNone -> pure ()
    SOptH -> optEntry 0
    SOptV -> optEntry 1
    SMap n -> do
      let mosaic = cMosaic c
          setini = cSetini c
      sc <- reg p (0x07 + n)
      hofs <- reg p (0x0D + 2 * n)
      vofs <- reg p (0x0E + 2 * n)
      eh <- MV.unsafeRead (pCol p) (colIx 4 j 0)
      ev <- MV.unsafeRead (pCol p) (colIx 4 j 1)
      when (j == 0) $ MV.write (pFine p) n (hofs .&. 7)
      let row = v - 1
          msize = (mosaic `shiftR` 4) + 1
          srow0 = if testBit mosaic n then row - row `mod` msize else row
          srow = if testBit setini 0 && hiresMode then srow0 * 2 + field else srow0
          opt = (mode == 2 || mode == 4 || mode == 6) && n < 2 && j >= 1
          apply e = testBit e (13 + n)
          !(!coarse, !vofs')
            | opt && mode == 4 && apply eh = if testBit eh 15 then (hofs `shiftR` 3, eh .&. 0x3FF) else ((eh .&. 0x3F8) `shiftR` 3, vofs)
            | opt && mode /= 4 = ( if apply eh then (eh .&. 0x3F8) `shiftR` 3 else hofs `shiftR` 3
                                 , if apply ev then ev .&. 0x3FF else vofs )
            | otherwise = (hofs `shiftR` 3, vofs)
          big = testBit bgmode (4 + n)
          tw = if hiresMode || big then 16 else 8 :: Int
          th = if big then 16 else 8 :: Int
          tcol = coarse + j
          tx = if tw == 16 && not hiresMode then tcol `shiftR` 1 else tcol
          y = srow + vofs'
      -- (th is 8 or 16: the shift and mask are div and mod)
      e <- vramWord p (mapAddr sc tx (y `shiftR` (if th == 16 then 4 else 3)))
      let hflip = testBit e 14
          py0 = y .&. (th - 1)
          py = if testBit e 15 then th - 1 - py0 else py0
          ch0 = (e .&. 0x3FF) + (if py >= 8 then 16 else 0)
          (chL, chR)
            | hiresMode = (ch0 + (if hflip then 1 else 0), ch0 + (if hflip then 0 else 1))
            | tw == 16 = let sub = (if odd tcol then 1 else 0) `xor` (if hflip then 1 else 0) in (ch0 + sub, ch0 + sub)
            | otherwise = (ch0, ch0)
      MV.write (pCol p) (colIx n j 0) e
      MV.write (pCol p) (colIx n j 1) (chL .&. 0x3FF)
      MV.write (pCol p) (colIx n j 2) (chR .&. 0x3FF)
      MV.write (pCol p) (colIx n j 7) (py .&. 7)
    SChar n k -> do
      nba <- reg p (if n < 2 then 0x0B else 0x0C)
      chL <- MV.unsafeRead (pCol p) (colIx n j 1)
      chR <- MV.unsafeRead (pCol p) (colIx n j 2)
      r <- MV.unsafeRead (pCol p) (colIx n j 7)
      let bpp = bppOf mode n
          half = bpp `div` 2
          (ch, plane) = if hiresMode then (if k < half then (chL, k) else (chR, k - half)) else (chL, k)
          base = ((nba `shiftR` (4 * (n .&. 1))) .&. 0xF) `shiftL` 12
      w <- vramWord p (base + ch * (bpp * 4) + r + plane * 8)
      MV.write (pCol p) (colIx n j (3 + k)) w
 where
  -- BG3's map at its scroll: column j's entry from the column before
  -- (j - 1 + BG3HOFS / 8), row BG3VOFS / 8 (H) and the next (V)
  optEntry k = do
    h3 <- reg p 0x11
    v3 <- reg p 0x12
    sc <- reg p 0x09
    e <- vramWord p (mapAddr sc (j - 1 + (h3 `shiftR` 3)) ((v3 `shiftR` 3) + k))
    MV.write (pCol p) (colIx 4 j k) (if j == 0 then 0 else e)

-- | Layer n's pixel at screen x (after mosaic) from its fetched columns;
-- in hi-res, @half@ is the half-dot.
{-# INLINE bgPixel #-}
bgPixel :: Ppu s -> Int -> Int -> Int -> Int -> ST s Px
bgPixel p mode n x half = do
  fine <- MV.unsafeRead (pFine p) n
  let q = x + fine
      j = q `shiftR` 3
      px0 = q .&. 7
      hiresMode = mode == 5 || mode == 6
      bpp = bppOf mode n
      -- (Q.7: the bit planes' words in a strict loop, not lists of their
      -- indices and words zipped and folded per layer and pixel)
      planes = bpp `div` 2
      (base, px) = if hiresMode
        then let hp = 2 * px0 + max 0 half
                 sub = hp `shiftR` 3
             in (3 + sub * planes, hp .&. 7)
        else (3, px0)
  e <- MV.unsafeRead (pCol p) (colIx n j 0)
  let b = if testBit e 14 then px else 7 - px
      go !k !acc
        | k >= planes = pure acc
        | otherwise = do
            w <- MV.unsafeRead (pCol p) (colIx n j (base + k))
            go (k + 1) (acc .|. (((w `shiftR` b) .&. 1) `shiftL` (2 * k))
                            .|. (((w `shiftR` (b + 8)) .&. 1) `shiftL` (2 * k + 1)))
  idx <- go (0 :: Int) (0 :: Int)
  pure (Px idx ((e `shiftR` 10) .&. 7) ((e `shiftR` 13) .&. 1))

-- | Mode 7's pixel (colour 0-255) at screen (x, line V).
mode7Pixel :: Ppu s -> Int -> Int -> ST s Int
mode7Pixel p x v = do
  sel <- reg p 0x1A
  a <- s16 <$> reg p 0x1B
  b <- s16 <$> reg p 0x1C
  c <- s16 <$> reg p 0x1D
  d <- s16 <$> reg p 0x1E
  cx <- s13 <$> reg p 0x1F
  cy <- s13 <$> reg p 0x20
  hofs <- s13 <$> reg p 0x40
  vofs <- s13 <$> reg p 0x41
  let sx = if testBit sel 0 then x `xor` 0xFF else x
      sy = if testBit sel 1 then v `xor` 0xFF else v
      clip o = let w = o .&. complement 0x1C00 in if o < 0 then w .|. 0x1C00 else w
      ox = s13 (clip (hofs - cx) .&. 0x1FFF)
      oy = s13 (clip (vofs - cy) .&. 0x1FFF)
      m k = k .&. complement 0x3F
      vx = m (a * ox) + m (b * oy) + cx * 256 + m (b * sy) + a * sx
      vy = m (c * ox) + m (d * oy) + cy * 256 + m (d * sy) + c * sx
      px = vx `shiftR` 8
      py = vy `shiftR` 8
      outside = px < 0 || px > 1023 || py < 0 || py > 1023
      over = sel `shiftR` 6
  if outside && over == 2 then pure 0 else do
    tile <- if outside && over == 3 then pure 0
            else (.&. 0xFF) <$> vramWord p (((py .&. 1023) `shiftR` 3) * 128 + ((px .&. 1023) `shiftR` 3))
    w <- vramWord p (tile * 64 + (py .&. 7) * 8 + (px .&. 7))
    pure (w `shiftR` 8)
 where
  s16 k = if k >= 0x8000 then k - 0x10000 else k
  s13 k = let w = k .&. 0x1FFF in if w >= 0x1000 then w - 0x2000 else w

-- * The pixel

-- | What a screen shows at a pixel: its colour, the layer (0-3 BG1-4, 4
-- OBJ, 5 backdrop), and whether an OBJ's palette allows colour maths.
data Shown = Shown { shColor :: !Int, shLayer :: !Int, shObjMath :: !Bool }

-- | Line V's dots up to (not including) @upTo@: dot d's fetch slot (dots
-- 6-269: column (d - 6) / 8, slot (d - 6) mod 8) and pixel (dots 22-277:
-- x = d - 22), each done as of master clock 4 (d + 1).
ppuRenderTo :: Ppu s -> Int -> Int -> Int -> ST s ()
ppuRenderTo p v field upTo = do
  d0 <- MV.read (pSt p) stX
  when (d0 < upTo && d0 < 278) $ do
    -- (side track W: the registers and CGRAM's backdrop read once for the
    -- stretch, which no write interrupts: 'Ctx')
    c <- mkCtx p
    let end = min 277 (upTo - 1)
        go !d
          | d > end = pure ()
          | otherwise = do
              when (d >= 6 && d < 270) $ fetchSlot p c v field ((d - 6) `shiftR` 3) ((d - 6) .&. 7)
              when (d >= 22) $ pixel p c v field (d - 22)
              go (d + 1)
    go d0
    MV.write (pSt p) stX (min 278 upTo)

-- | What the pixels of a stretch read of the registers (and CGRAM's
-- colour 0), once for the stretch: within one 'ppuRenderTo' nothing is
-- written (the machine draws before each write; the render threads
-- between their log's changes). Side track W: they were read again for
-- every pixel, the windows' alone 36 times.
data Ctx = Ctx
  { cInidisp, cSetini, cBgmode, cMode, cTm, cTs, cTmw, cTsw, cCgwsel, cAdsub, cFixed, cMosaic
  , cWsel, cWlog, cL1, cR1, cL2, cR2, cBack :: !Int
  , cOrder :: !(U.Vector Int)
  , cRankBg :: !Int   -- ^ 'ranks': BG layer n at priority hi's place in the order, 4 bits at (n * 2 + hi) * 4 (15: not in it)
  , cRankObj :: !Int  -- ^ the sprites at priority k's, 4 bits at k * 4
  , cMasks :: !Int    -- ^ 'windowMask' for the four cases (inside window 1, inside window 2): 6 bits each at (in1 + 2 in2) * 8
  }

mkCtx :: Ppu s -> ST s Ctx
mkCtx p = do
  inidisp <- reg p 0x00
  setini <- reg p 0x33
  bgmode <- reg p 0x05
  tm <- reg p 0x2C
  ts <- reg p 0x2D
  tmw <- reg p 0x2E
  tsw <- reg p 0x2F
  cgwsel <- reg p 0x30
  adsub <- reg p 0x31
  fixed <- reg p 0x42
  mosaic <- reg p 0x06
  w12 <- reg p 0x23
  w34 <- reg p 0x24
  wobj <- reg p 0x25
  lbg <- reg p 0x2A
  lobj <- reg p 0x2B
  l1 <- reg p 0x26
  r1 <- reg p 0x27
  l2 <- reg p 0x28
  r2 <- reg p 0x29
  back <- cgColor p 0
  let mode = bgmode .&. 7
      order = orderTable mode (testBit bgmode 3) (testBit setini 6)
      (rbg, robj) = ranks order
  let c0 = Ctx inidisp setini bgmode mode tm ts tmw tsw cgwsel adsub fixed mosaic
        (w12 .|. (w34 `shiftL` 8) .|. (wobj `shiftL` 16)) (lbg .|. (lobj `shiftL` 8)) l1 r1 l2 r2 back
        order rbg robj 0
      -- (the windows' masks: a pixel's depends only on whether it is
      -- inside each window, so the four are worked out once a stretch)
      masks = foldr (\k acc -> acc .|. (masksFor c0 (odd k) (k >= 2) `shiftL` (k * 8))) 0 [0 .. 3 :: Int]
  pure $! c0 { cMasks = masks }

-- | Each entry's place in a priority order ('orderTable'), front first, as
-- 'Ctx''s two packed fields: the front-most pixel is the lowest place
-- among those present ('pixelFast').
ranks :: U.Vector Int -> (Int, Int)
ranks order = U.ifoldl' put (complement 0 .&. 0xFFFFFFFF, 0xFFFF) order
 where
  put (bg, obj) k code
    | code >= 8 = (bg, setPlace obj (code - 8) k)
    | otherwise = (setPlace bg code k, obj)
  setPlace w slot k = (w .&. complement (0xF `shiftL` (slot * 4))) .|. (k `shiftL` (slot * 4))

pixel :: Ppu s -> Ctx -> Int -> Int -> Int -> ST s ()
pixel p c v field x = do
  let !row = v - 1
      inidisp = cInidisp c
      setini = cSetini c
      mode = cMode c
      hires = mode == 5 || mode == 6
      pseudo = testBit setini 3
      -- (row and x checked: within the frame)
      out k col = MV.unsafeWrite (pFrame p) (row * frameWidth + 2 * x + k) (fromIntegral col)
  if testBit inidisp 7 || row < 0 || row >= frameHeight
    then when (row >= 0 && row < frameHeight) (out 0 0 >> out 1 0)
    else do
      let bright = inidisp .&. 0xF
          scale c | bright == 15 = c   -- (full brightness: (c * 16) / 16)
                  | bright == 0 = 0
                  | otherwise =
            let f k = (((c `shiftR` k) .&. 31) * (bright + 1)) `div` 16 in f 0 .|. (f 5 `shiftL` 5) .|. (f 10 `shiftL` 10)
      -- (the fast version: the windows once per pixel, as a bit mask; they
      -- don't depend on the screen)
      let !wins = windowMask c x
      if hires || pseudo
        then do
          -- the sub screen's half-dot on the left, the main screen's on the right
          subPx <- screen p c v field x False 0 wins False
          mainPx <- screen p c v field x True 1 wins False
          let back = cBack c
              col sh = if shLayer sh == 5 then back else shColor sh
              !mathed = colorMath c wins mainPx Nothing
          out 0 (scale (col subPx))
          out 1 (scale mathed)
        else do
          col <- pixelFast p c v x wins
          out 0 (scale col)
          out 1 (scale col)

-- | Outside hi-res: both screens' front-most pixels and their colour maths
-- at x, as 'layers' and 'screen' twice would find them (side track W):
-- each BG layer's pixel once, kept in hand, both screens' winners found
-- in the same pass as the lowest place in the mode's order ('ranks')
-- among the pixels present and on, not by walking the order for each
-- screen over a scratch array.
{-# INLINE pixelFast #-}
pixelFast :: Ppu s -> Ctx -> Int -> Int -> Int -> ST s Int
pixelFast p c v x wins = do
  let mode = cMode c
      mosaic = cMosaic c
      msize = (mosaic `shiftR` 4) + 1
      row = v - 1
      direct = testBit (cCgwsel c) 0
      onOf tm tw l = testBit tm l && not (testBit tw l && testBit wins l)
      onM = onOf (cTm c) (cTmw c)
      onS = onOf (cTs c) (cTsw c)
      -- the sub screen without BG/OBJ (CGWSEL bit 1): only its backdrop
      !subOff = not (testBit (cCgwsel c) 1)
      rankBg n pri = (cRankBg c `unsafeShiftR` ((n * 2 + pri) * 4)) .&. 0xF
      nl = case mode of 0 -> 4; 1 -> 3; 6 -> 1; 7 -> 2; _ -> 2 :: Int
  objW <- MV.unsafeRead (pObj p) x
  let -- whether layer n's pixel (place r) beats the main or sub screen's so far
      winsM !n !idx !r !rm = idx /= 0 && onM n && r < rm
      winsS !n !idx !r !rs = idx /= 0 && not subOff && onS n && r < rs
      done = pixelDone p c wins objW
  if mode == 7
    then do
      let mx = if testBit mosaic 0 then x - x `mod` msize else x
          mv = if testBit mosaic 0 then v - (row `mod` msize) else v
      m7 <- mode7Pixel p mx mv
      let !code0 = if m7 == 0 then 0 else if direct then colorDirect (directColor m7 0) else m7
          !r0 = rankBg 0 0
          !m0 = winsM 0 m7 r0 15
          !s0 = winsS 0 m7 r0 15
          !rm0 = if m0 then r0 else 15
          !rs0 = if s0 then r0 else 15
          !i1 = m7 .&. 0x7F
          !r1 = rankBg 1 (m7 `shiftR` 7)
          !m1 = winsM 1 i1 r1 rm0
          !s1 = winsS 1 i1 r1 rs0
      done (if m1 then r1 else rm0) (if m1 then 1 else if m0 then 0 else -1) (if m1 then i1 else code0)
           (if s1 then r1 else rs0) (if s1 then 1 else if s0 then 0 else -1) (if s1 then i1 else code0)
    else do
      let each !n !rm !lm !cm !rs !ls !cs
            | n >= nl = done rm lm cm rs ls cs
            | otherwise = do
                let bpp = bppOf mode n
                    !sx = if testBit mosaic n then x - x `mod` msize else x
                Px idx pal pri <- bgPixel p mode n sx (-1)
                let !code | idx == 0 = 0
                          | bpp == 8 && direct = colorDirect (directColor idx pal)
                          | otherwise = case bpp of
                              2 -> (if mode == 0 then n * 32 else 0) + pal * 4 + idx
                              4 -> pal * 16 + idx
                              _ -> idx
                let !r = rankBg n pri
                    !m = winsM n idx r rm
                    !sb = winsS n idx r rs
                each (n + 1) (if m then r else rm) (if m then n else lm) (if m then code else cm)
                             (if sb then r else rs) (if sb then n else ls) (if sb then code else cs)
      each 0 15 (-1) 0 15 (-1) 0

-- | 'pixelFast''s end: the screens' colours from the layers' winners
-- (place, layer, colour code; layer -1 none), the sprite and the backdrop
-- considered last, and their colour maths. (Top level: as a local
-- function, called from the layers' loop and from mode 7's, it was a
-- closure built on every pixel.)
{-# INLINE pixelDone #-}
pixelDone :: Ppu s -> Ctx -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> ST s Int
pixelDone p c !wins !objW !rm !lm !cm !rs !ls !cs = do
  let onOf tm tw l = testBit tm l && not (testBit tw l && testBit wins l)
      !subOff = not (testBit (cCgwsel c) 1)
      !objPri = objW `unsafeShiftR` 7
      !ro = if objW == 0 then 15 else (cRankObj c `unsafeShiftR` (objPri * 4)) .&. 0xF
      !objM = objW /= 0 && onOf (cTm c) (cTmw c) 4 && ro < rm
      !objS = objW /= 0 && not subOff && onOf (cTs c) (cTsw c) 4 && ro < rs
      !opal = (objW `unsafeShiftR` 4) .&. 7
      !omath = opal >= 4
  !oc <- if objM || objS then cgColor p (128 + opal * 16 + (objW .&. 0xF)) else pure 0
  !mc <- if objM || lm < 0 then pure 0 else colourOf p cm
  !sc <- if objS || ls < 0 then pure 0 else colourOf p cs
  let !mainPx | objM = Shown oc 4 omath
              | lm < 0 = Shown (cBack c) 5 False
              | otherwise = Shown mc lm False
      !subPx | objS = Shown oc 4 omath
             | ls < 0 = Shown (cFixed c) 5 False
             | otherwise = Shown sc ls False
  pure $! colorMath c wins mainPx (Just subPx)

-- | The front-most pixel of the main or sub screen at x; @half@ is the
-- hi-res half-dot (0 left, 1 right) or -1; @given@: the BG layers are in
-- 'pLay' already ('layers', for both screens outside hi-res). (Side track
-- Q: the layers in a scratch array and the mode's priority order a table
-- walked to its first hit, not lists of candidates searched with lookup
-- and find; the same order and rules.)
{-# INLINE screen #-}
screen :: Ppu s -> Ctx -> Int -> Int -> Int -> Bool -> Int -> Int -> Bool -> ST s Shown
screen p c v _field x main half wins given = do
  let tm = if main then cTm c else cTs c
      tw = if main then cTmw c else cTsw c
      cgwsel = cCgwsel c
      on l = testBit tm l && not (testBit tw l && testBit wins l)
      -- as colour maths' operand, the sub screen without BG/OBJ shows only
      -- its backdrop (CGWSEL bit 1); in hi-res (a half-dot given) it is
      -- drawn either way
      !subOff = not main && half < 0 && not (testBit cgwsel 1)
  objW <- MV.unsafeRead (pObj p) x
  let !objPri = if objW == 0 then -1 else objW `shiftR` 7
      !objOn = on 4 && objW /= 0 && not subOff
      lay = pLay p
  nl <- if subOff then pure 0 else do
    when (not given) (layers p c v x half)
    MV.unsafeRead lay 16
  let -- the priority order of each mode, front first ('orderTable')
      order = cOrder c
      len = U.length order
      -- the hit: -1 none (the backdrop), 4 the sprite, else BG layer n and
      -- its colour (the search tail-calls 'finish': a jump, no closure)
      finish !layer !col = case layer of
        4 -> do
          let idx = objW .&. 0xF
              pal = (objW `shiftR` 4) .&. 7
          oc <- cgColor p (128 + pal * 16 + idx)
          let !om = pal >= 4
          pure $! Shown oc 4 om
        -1 -> do
          pure $! Shown (if main then cBack c else cFixed c) 5 False
        _ -> pure $! Shown col layer False
      go !k
        | k >= len = finish (-1) 0
        | otherwise = do
            let code = U.unsafeIndex order k
            if code >= 8
              then if objOn && objPri == code - 8 then finish 4 0 else go (k + 1)
              else do
                let n = code `shiftR` 1
                    hi = code .&. 1
                if n >= nl || not (on n) then go (k + 1) else do
                  idx <- MV.unsafeRead lay (n * 4)
                  pri <- MV.unsafeRead lay (n * 4 + 2)
                  if idx /= 0 && pri == hi
                    then MV.unsafeRead lay (n * 4 + 3) >>= colourOf p >>= finish n
                    else go (k + 1)
  go 0

-- | Each mode's priority order, front first: BG layer n at priority hi as
-- n * 2 + hi, the sprites at priority k as 8 + k.
orderTable :: Int -> Bool -> Bool -> U.Vector Int
orderTable mode m1bg3 m7ext = case mode of
  0 -> o0
  1 | m1bg3 -> o1a
    | otherwise -> o1b
  7 | m7ext -> o7a
    | otherwise -> o7b
  _ -> oN
 where
  obj k = 8 + k
  bg n hi = n * 2 + hi
  o0 = U.fromList [obj 3, bg 0 1, bg 1 1, obj 2, bg 0 0, bg 1 0, obj 1, bg 2 1, bg 3 1, obj 0, bg 2 0, bg 3 0]
  o1a = U.fromList [bg 2 1, obj 3, bg 0 1, bg 1 1, obj 2, bg 0 0, bg 1 0, obj 1, obj 0, bg 2 0]
  o1b = U.fromList [obj 3, bg 0 1, bg 1 1, obj 2, bg 0 0, bg 1 0, obj 1, bg 2 1, obj 0, bg 2 0]
  o7a = U.fromList [obj 3, obj 2, bg 1 1, obj 1, bg 0 0, obj 0, bg 1 0]
  o7b = U.fromList [obj 3, obj 2, obj 1, bg 0 0, obj 0]
  oN = U.fromList [obj 3, bg 0 1, obj 2, bg 1 1, obj 1, bg 0 0, obj 0, bg 1 0]
{-# NOINLINE orderTable #-}

-- | Each BG layer of the mode at x, its pixel and colour, into 'pLay'
-- (the count at 16).
layers :: Ppu s -> Ctx -> Int -> Int -> Int -> ST s ()
layers p ctx v x half = do
  let mosaic = cMosaic ctx
      cgwsel = cCgwsel ctx
      mode = cMode ctx
      row = v - 1
      msize = (mosaic `shiftR` 4) + 1
      nLayers = case mode of 0 -> 4; 1 -> 3; 6 -> 1; _ -> 2 :: Int
      direct = testBit cgwsel 0
  if mode == 7
    then do
      let mx = if testBit mosaic 0 then x - x `mod` msize else x
          mv = if testBit mosaic 0 then v - (row `mod` msize) else v
      c <- mode7Pixel p mx mv
      put 0 (Px c 0 0) (if c == 0 then 0 else if direct then colorDirect (directColor c 0) else c)
      put 1 (Px (c .&. 0x7F) 0 (c `shiftR` 7)) (c .&. 0x7F)
      MV.unsafeWrite (pLay p) 16 2
    else do
      MV.unsafeWrite (pLay p) 16 nLayers
      -- (Q.7: a strict loop over the layers, not a list of them per pixel)
      let each !n
            | n >= nLayers = pure ()
            | otherwise = do
                let bpp = bppOf mode n
                    mos = testBit mosaic n
                    sx = if mos then x - x `mod` msize else x
                px@(Px idx pal _) <- bgPixel p mode n sx half
                -- (side track W: the colour's CGRAM address, or a direct
                -- colour, kept; the colour looked up only for the pixel a
                -- screen shows, 'colourOf', not for every layer's)
                let !code | idx == 0 = 0
                          | bpp == 8 && direct = colorDirect (directColor idx pal)
                          | otherwise = case bpp of
                              2 -> (if mode == 0 then n * 32 else 0) + pal * 4 + idx
                              4 -> pal * 16 + idx
                              _ -> idx
                put n px code
                each (n + 1)
      each (0 :: Int)
 where
  put n (Px idx pal pri) col = do
    MV.unsafeWrite (pLay p) (n * 4) idx
    MV.unsafeWrite (pLay p) (n * 4 + 1) pal
    MV.unsafeWrite (pLay p) (n * 4 + 2) pri
    MV.unsafeWrite (pLay p) (n * 4 + 3) col

-- | A layer's colour code ('layers'): a CGRAM address (0-255), or a
-- direct colour with bit 16 set.
colorDirect :: Int -> Int
colorDirect c = c .|. 0x10000
{-# INLINE colorDirect #-}

colourOf :: Ppu s -> Int -> ST s Int
colourOf p code
  | code >= 0x10000 = pure $! code .&. 0x7FFF
  | otherwise = cgColor p code
{-# INLINE colourOf #-}

-- | Direct colour: BBGGGRRR and the palette's bgr into 15 bits.
directColor :: Int -> Int -> Int
directColor c pal =
  let r = ((c .&. 7) `shiftL` 2) .|. ((pal .&. 1) `shiftL` 1)
      g = (((c `shiftR` 3) .&. 7) `shiftL` 2) .|. (((pal `shiftR` 1) .&. 1) `shiftL` 1)
      b = (((c `shiftR` 6) .&. 3) `shiftL` 3) .|. (((pal `shiftR` 2) .&. 1) `shiftL` 2)
  in r .|. (g `shiftL` 5) .|. (b `shiftL` 10)

-- | The windows of layers 0 to 5 at x as bits ('windowAt' each).
windowMask :: Ctx -> Int -> Int
windowMask c x =
  let !k = (if x >= cL1 c && x <= cR1 c then 1 else 0) .|. (if x >= cL2 c && x <= cR2 c then 2 else 0)
  in (cMasks c `unsafeShiftR` (k * 8)) .&. 0x3F
{-# INLINE windowMask #-}

-- | The six layers' window bits for a pixel inside window 1 or not and
-- inside window 2 or not ('windowAt' each).
masksFor :: Ctx -> Bool -> Bool -> Int
masksFor c in1 in2 = go 0 0
 where
  go !l !acc
    | l > 5 = acc
    | otherwise = go (l + 1) (if windowAt c l in1 in2 then setBit acc l else acc)

-- | Whether layer l (0-3 BG1-4, 4 OBJ, 5 the colour maths window) is in
-- its window at x.
windowAt :: Ctx -> Int -> Bool -> Bool -> Bool
windowAt c l in1 in2 =
  let sel = (cWsel c `shiftR` (8 * (l `div` 2))) .&. 0xFF
      logic = if l < 4 then cWlog c .&. 0xFF else cWlog c `shiftR` 8
      bits = (sel `shiftR` (4 * (l .&. 1))) .&. 0xF
      -- each window's two bits: enable (the higher), invert (the lower);
      -- fullsnes's "0..1=Disable, 1=Inside, 2=Outside" is a typo for 2 and
      -- 3 (bsnes, anomie); read as bit 0 enable, a game's color window (a
      -- text box's black, step 7.12) was off
      -- (side track W: each window's state as a plain value, no Maybe
      -- built on every pixel)
      !e1 = testBit bits 1
      !e2 = testBit bits 3
      !a = in1 /= testBit bits 0
      !b = in2 /= testBit bits 2
      lg = (logic `shiftR` (2 * (l .&. 3))) .&. 3
  in if
    | e1 && e2 -> case lg of
        0 -> a || b
        1 -> a && b
        2 -> a /= b
        _ -> a == b
    | e1 -> a
    | e2 -> b
    | otherwise -> False

-- | Colour maths on the main screen's pixel with the sub screen's (or,
-- given Nothing, the fixed colour).
{-# INLINE colorMath #-}
colorMath :: Ctx -> Int -> Shown -> Maybe Shown -> Int
colorMath c wins mainPx subPx =
  let cgwsel = cCgwsel c
      adsub = cAdsub c
      fixed = cFixed c
      inWin = testBit wins 5
      -- CGWSEL bits 7-6 force the main screen black and 5-4 allow the maths:
      -- 0 never / always, 1 and 2 outside or inside the maths window, 3
      -- always / never
      !black = case (cgwsel `shiftR` 6) .&. 3 of 0 -> False; 1 -> not inWin; 2 -> inWin; _ -> True
      !allowed = case (cgwsel `shiftR` 4) .&. 3 of 0 -> True; 1 -> inWin; 2 -> not inWin; _ -> False
      !layerOk = case shLayer mainPx of
        4 -> shObjMath mainPx && testBit adsub 4
        5 -> testBit adsub 5
        n -> testBit adsub n
      !mainC = if black then 0 else shColor mainPx
      !subC = case subPx of
        Just sp | shLayer sp /= 5 -> shColor sp
        _ -> fixed
      !subIsBack = case subPx of
        Just sp | shLayer sp /= 5 -> False
        _ -> True
      -- the half is not applied to the fixed colour as the sub screen's
      -- backdrop, nor on a forced-black main screen (fullsnes)
      !half = testBit adsub 6 && not black && not subIsBack
      !minus = testBit adsub 7
      op k = mathChannel minus half ((mainC `unsafeShiftR` k) .&. 31) ((subC `unsafeShiftR` k) .&. 31)
  in if not (layerOk && allowed) then mainC else op 0 .|. (op 5 `unsafeShiftL` 5) .|. (op 10 `unsafeShiftL` 10)

-- | One colour channel's maths: the main screen's plus or minus the sub
-- screen's, halved or not, clamped to 0-31. (Side track W: top level and
-- inlined; as colorMath's local function it was a call per channel.)
mathChannel :: Bool -> Bool -> Int -> Int -> Int
mathChannel minus half ch cs =
  let r0 = if minus then ch - cs else ch + cs
      r1 = if half then r0 `unsafeShiftR` 1 else r0
  in max 0 (min 31 r1)
{-# INLINE mathChannel #-}

-- | The PPU's own state for a save state (Q.8, "Platform.Snapshot"): its
-- frame, the line's sprites, fetches, fine scroll and state; not VRAM,
-- CGRAM, OAM and the registers, which it shares with the machine.
ppuOwnFields :: Ppu s -> [Field s]
ppuOwnFields p = [FW16 (pFrame p), FInt (pObj p), FInt (pCol p), FInt (pFine p), FInt (pSt p), FInt (pLay p)]

-- | A render thread's copy made like @p@ again (after a save state is
-- loaded): its VRAM, CGRAM, OAM and registers copied from @p@'s.
ppuReload :: Ppu s -> Ppu s -> ST s ()
ppuReload copy p = do
  MV.copy (pVram copy) (pVram p)
  MV.copy (pCgram copy) (pCgram p)
  MV.copy (pOam copy) (pOam p)
  MV.copy (pRegs copy) (pRegs p)
