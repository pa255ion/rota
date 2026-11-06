{-# OPTIONS_GHC -fworker-wrapper -fspecialise-aggressively #-}
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
  , ppuVramW
  , ppuCgW
  , ppuSetObjLine
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
  , pLay    :: !(MV.MVector s Int)   -- ^ side track Q: the BG layers at the pixel ('layers'): 4 x (index, palette, priority, colour), the count at 16
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
  pure (Ppu vram cg oam regs fr obj col fine st lay)

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
  pure (fromIntegral lo .|. fromIntegral hi `shiftL` 8)

cgColor :: Ppu s -> Int -> ST s Int
cgColor p i = do
  lo <- MV.unsafeRead (pCgram p) (i * 2)
  hi <- MV.unsafeRead (pCgram p) (i * 2 + 1)
  pure ((fromIntegral lo .|. fromIntegral hi `shiftL` 8) .&. 0x7FFF)

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
  let (small, large) = objSizes (obsel `shiftR` 5)
      first = if testBit rot 7 then (oamRel `shiftR` 2) .&. 0x7F else 0
  entries <- mapM (\k -> do
    let i = (first + k) .&. 0x7F
    x0 <- fromIntegral <$> MV.read (pOam p) (i * 4)
    y <- fromIntegral <$> MV.read (pOam p) (i * 4 + 1)
    hi <- fromIntegral <$> MV.read (pOam p) (0x200 + i `shiftR` 2)
    let bits = (hi :: Int) `shiftR` ((i .&. 3) * 2)
        x9 = x0 .|. ((bits .&. 1) `shiftL` 8)
        x = if x9 >= 256 then x9 - 512 else x9
        (w, h) = if testBit bits 1 then large else small
    pure (i, x, y :: Int, w, h)) [0 .. 127]
  let onLine (_, x, y, w, h) = (row - y) .&. 0xFF < h && (x > negate w || x == -256) && x < 256
      found = filter onLine entries
      inRange = take 32 found
  when (length found > 32) $ MV.modify (pSt p) (.|. 0x40) stFlags
  -- the tiles, from the last sprite found to the first; 34 at most
  let slivers = concat
        [ [ (e, c) | c <- [0 .. w `div` 8 - 1], let sx = x + c * 8, sx > -8, sx < 256 ]
        | e@(_, x, _, w, _) <- reverse inRange ]
  when (length slivers > 34) $ MV.modify (pSt p) (.|. 0x80) stFlags
  let base = (obsel .&. 7) `shiftL` 13
      gap = (((obsel `shiftR` 3) .&. 3) + 1) `shiftL` 12
  forM_ (take 34 slivers) $ \((i, x, y, w, h), c) -> do
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
    forM_ [0 .. 7] $ \k -> do
      let sx = x + c * 8 + k
          b = if hflip then k else 7 - k
          idx = ((w01 `shiftR` b) .&. 1) .|. (((w01 `shiftR` (b + 8)) .&. 1) `shiftL` 1)
                .|. (((w23 `shiftR` b) .&. 1) `shiftL` 2) .|. (((w23 `shiftR` (b + 8)) .&. 1) `shiftL` 3)
      when (sx >= 0 && sx < 256 && idx /= 0) $
        MV.write (pObj p) sx (idx .|. (pal `shiftL` 4) .|. (pri `shiftL` 7))

-- * Background layers

-- | A BG pixel: colour index (0 = transparent), palette, priority bit.
data Px = Px !Int !Int !Int

-- | What a fetch slot does: an offset-per-tile entry (H or V), a layer's
-- map entry, or a layer's k-th char word.
data Slot = SOptH | SOptV | SMap !Int | SChar !Int !Int | SNone
  deriving stock (Eq)

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
fetchSlot :: Ppu s -> Int -> Int -> Int -> Int -> ST s ()
fetchSlot p v field j s = do
  bgmode <- reg p 0x05
  let mode = bgmode .&. 7
      hiresMode = mode == 5 || mode == 6
  case V.unsafeIndex fetchSlots (mode * 8 + s) of
    SNone -> pure ()
    SOptH -> optEntry 0
    SOptV -> optEntry 1
    SMap n -> do
      mosaic <- reg p 0x06
      setini <- reg p 0x33
      sc <- reg p (0x07 + n)
      hofs <- reg p (0x0D + 2 * n)
      vofs <- reg p (0x0E + 2 * n)
      eh <- MV.read (pCol p) (colIx 4 j 0)
      ev <- MV.read (pCol p) (colIx 4 j 1)
      when (j == 0) $ MV.write (pFine p) n (hofs .&. 7)
      let row = v - 1
          msize = (mosaic `shiftR` 4) + 1
          srow0 = if testBit mosaic n then row - row `mod` msize else row
          srow = if testBit setini 0 && hiresMode then srow0 * 2 + field else srow0
          opt = mode `elem` [2, 4, 6] && n < 2 && j >= 1
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
      e <- vramWord p (mapAddr sc tx (y `div` th))
      let hflip = testBit e 14
          py0 = y `mod` th
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
      chL <- MV.read (pCol p) (colIx n j 1)
      chR <- MV.read (pCol p) (colIx n j 2)
      r <- MV.read (pCol p) (colIx n j 7)
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
bgPixel :: Ppu s -> Int -> Int -> Int -> Int -> ST s Px
bgPixel p mode n x half = do
  fine <- MV.read (pFine p) n
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
  e <- MV.read (pCol p) (colIx n j 0)
  let b = if testBit e 14 then px else 7 - px
      go !k !acc
        | k >= planes = pure acc
        | otherwise = do
            w <- MV.read (pCol p) (colIx n j (base + k))
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
    forM_ [d0 .. min 277 (upTo - 1)] $ \d -> do
      when (d >= 6 && d < 270) $ fetchSlot p v field ((d - 6) `shiftR` 3) ((d - 6) .&. 7)
      when (d >= 22) $ pixel p v field (d - 22)
    MV.write (pSt p) stX (min 278 upTo)

pixel :: Ppu s -> Int -> Int -> Int -> ST s ()
pixel p v field x = do
  let row = v - 1
  inidisp <- reg p 0x00
  setini <- reg p 0x33
  bgmode <- reg p 0x05
  let mode = bgmode .&. 7
      hires = mode == 5 || mode == 6
      pseudo = testBit setini 3
      out k c = MV.write (pFrame p) (row * frameWidth + 2 * x + k) (fromIntegral c)
  if testBit inidisp 7 || row < 0 || row >= frameHeight
    then when (row >= 0 && row < frameHeight) (out 0 0 >> out 1 0)
    else do
      let bright = inidisp .&. 0xF
          scale c = if bright == 0 then 0 else
            let f k = (((c `shiftR` k) .&. 31) * (bright + 1)) `div` 16 in f 0 .|. (f 5 `shiftL` 5) .|. (f 10 `shiftL` 10)
      -- (the fast version: the windows once per pixel, as a bit mask; they
      -- don't depend on the screen)
      wins <- windowMask p x
      if hires || pseudo
        then do
          -- the sub screen's half-dot on the left, the main screen's on the right
          subPx <- screen p v field x False 0 wins False
          mainPx <- screen p v field x True 1 wins False
          back <- cgColor p 0
          let col sh = if shLayer sh == 5 then back else shColor sh
          mathed <- colorMath p wins mainPx Nothing
          out 0 (scale (col subPx))
          out 1 (scale mathed)
        else do
          -- (the fast version: the BG layers once for both screens, which
          -- see the same ones outside hi-res)
          layers p v x (-1) mode
          mainPx <- screen p v field x True (-1) wins True
          subPx <- screen p v field x False (-1) wins True
          c <- colorMath p wins mainPx (Just subPx)
          out 0 (scale c)
          out 1 (scale c)

-- | The front-most pixel of the main or sub screen at x; @half@ is the
-- hi-res half-dot (0 left, 1 right) or -1; @given@: the BG layers are in
-- 'pLay' already ('layers', for both screens outside hi-res). (Side track
-- Q: the layers in a scratch array and the mode's priority order a table
-- walked to its first hit, not lists of candidates searched with lookup
-- and find; the same order and rules.)
screen :: Ppu s -> Int -> Int -> Int -> Bool -> Int -> Int -> Bool -> ST s Shown
screen p v _field x main half wins given = do
  bgmode <- reg p 0x05
  tm <- reg p (if main then 0x2C else 0x2D)
  tw <- reg p (if main then 0x2E else 0x2F)
  setini <- reg p 0x33
  cgwsel <- reg p 0x30
  let mode = bgmode .&. 7
      on l = testBit tm l && not (testBit tw l && testBit wins l)
      -- as colour maths' operand, the sub screen without BG/OBJ shows only
      -- its backdrop (CGWSEL bit 1); in hi-res (a half-dot given) it is
      -- drawn either way
      subOff = not main && half < 0 && not (testBit cgwsel 1)
  objW <- MV.read (pObj p) x
  let objPri = if objW == 0 then -1 else objW `shiftR` 7
      objOn = on 4 && objW /= 0 && not subOff
      lay = pLay p
  nl <- if subOff then pure 0 else do
    when (not given) (layers p v x half mode)
    MV.unsafeRead lay 16
  let -- the priority order of each mode, front first ('orderTable')
      order = orderTable mode (testBit bgmode 3) (testBit setini 6)
      len = U.length order
      -- a code's hit: 0 none, 1 the sprite, 2 a BG layer
      go !k
        | k >= len = pure (Shown 0 (-1) False)
        | otherwise = do
            let code = U.unsafeIndex order k
            if code >= 8
              then if objOn && objPri == code - 8 then pure (Shown 0 4 False) else go (k + 1)
              else do
                let n = code `shiftR` 1
                    hi = code .&. 1
                if n >= nl || not (on n) then go (k + 1) else do
                  idx <- MV.unsafeRead lay (n * 4)
                  pri <- MV.unsafeRead lay (n * 4 + 2)
                  if idx /= 0 && pri == hi
                    then (\c -> Shown c n False) <$> MV.unsafeRead lay (n * 4 + 3)
                    else go (k + 1)
  hit <- go 0
  case shLayer hit of
    4 -> do
      let idx = objW .&. 0xF
          pal = (objW `shiftR` 4) .&. 7
      c <- cgColor p (128 + pal * 16 + idx)
      pure (Shown c 4 (pal >= 4))
    -1 -> do
      c <- if main then cgColor p 0 else reg p 0x42
      pure (Shown c 5 False)
    _ -> pure hit

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
layers :: Ppu s -> Int -> Int -> Int -> Int -> ST s ()
layers p v x half mode = do
  mosaic <- reg p 0x06
  cgwsel <- reg p 0x30
  let row = v - 1
      msize = (mosaic `shiftR` 4) + 1
      nLayers = case mode of 0 -> 4; 1 -> 3; 6 -> 1; _ -> 2 :: Int
      direct = testBit cgwsel 0
  if mode == 7
    then do
      let mx = if testBit mosaic 0 then x - x `mod` msize else x
          mv = if testBit mosaic 0 then v - (row `mod` msize) else v
      c <- mode7Pixel p mx mv
      col <- if c == 0 then pure 0 else if direct then pure (directColor c 0) else cgColor p c
      col2 <- if c .&. 0x7F == 0 then pure 0 else cgColor p (c .&. 0x7F)
      put 0 (Px c 0 0) col
      put 1 (Px (c .&. 0x7F) 0 (c `shiftR` 7)) col2
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
                col <- if idx == 0 then pure 0
                       else if bpp == 8 && direct then pure (directColor idx pal)
                       else cgColor p (case bpp of
                              2 -> (if mode == 0 then n * 32 else 0) + pal * 4 + idx
                              4 -> pal * 16 + idx
                              _ -> idx)
                put n px col
                each (n + 1)
      each (0 :: Int)
 where
  put n (Px idx pal pri) col = do
    MV.unsafeWrite (pLay p) (n * 4) idx
    MV.unsafeWrite (pLay p) (n * 4 + 1) pal
    MV.unsafeWrite (pLay p) (n * 4 + 2) pri
    MV.unsafeWrite (pLay p) (n * 4 + 3) col

-- | Direct colour: BBGGGRRR and the palette's bgr into 15 bits.
directColor :: Int -> Int -> Int
directColor c pal =
  let r = ((c .&. 7) `shiftL` 2) .|. ((pal .&. 1) `shiftL` 1)
      g = (((c `shiftR` 3) .&. 7) `shiftL` 2) .|. (((pal `shiftR` 1) .&. 1) `shiftL` 1)
      b = (((c `shiftR` 6) .&. 3) `shiftL` 3) .|. (((pal `shiftR` 2) .&. 1) `shiftL` 2)
  in r .|. (g `shiftL` 5) .|. (b `shiftL` 10)

-- | The windows of layers 0 to 5 at x as bits ('windowAt' each).
windowMask :: Ppu s -> Int -> ST s Int
windowMask p x = go 0 0
 where
  go !l !acc
    | l > 5 = pure acc
    | otherwise = do
        w <- windowAt p l x
        go (l + 1) (if w then setBit acc l else acc)

-- | Whether layer l (0-3 BG1-4, 4 OBJ, 5 the colour maths window) is in
-- its window at x.
windowAt :: Ppu s -> Int -> Int -> ST s Bool
windowAt p l x = do
  sel <- reg p (0x23 + l `div` 2)
  logic <- reg p (if l < 4 then 0x2A else 0x2B)
  l1 <- reg p 0x26
  r1 <- reg p 0x27
  l2 <- reg p 0x28
  r2 <- reg p 0x29
  let bits = (sel `shiftR` (4 * (l .&. 1))) .&. 0xF
      in1 = x >= l1 && x <= r1
      in2 = x >= l2 && x <= r2
      -- each window's two bits: enable (the higher), invert (the lower);
      -- fullsnes's "0..1=Disable, 1=Inside, 2=Outside" is a typo for 2 and
      -- 3 (bsnes, anomie); read as bit 0 enable, a game's color window (a
      -- text box's black, step 7.12) was off
      w1 = if testBit bits 1 then Just (in1 /= testBit bits 0) else Nothing
      w2 = if testBit bits 3 then Just (in2 /= testBit bits 2) else Nothing
      lg = (logic `shiftR` (2 * (l .&. 3))) .&. 3
  pure $ case (w1, w2) of
    (Just a, Just b) -> case lg of
      0 -> a || b
      1 -> a && b
      2 -> a /= b
      _ -> a == b
    (Just a, Nothing) -> a
    (Nothing, Just b) -> b
    _ -> False

-- | Colour maths on the main screen's pixel with the sub screen's (or,
-- given Nothing, the fixed colour).
colorMath :: Ppu s -> Int -> Shown -> Maybe Shown -> ST s Int
colorMath p wins mainPx subPx = do
  cgwsel <- reg p 0x30
  adsub <- reg p 0x31
  fixed <- reg p 0x42
  let inWin = testBit wins 5
  -- CGWSEL bits 7-6 force the main screen black and 5-4 allow the maths:
  -- 0 never / always, 1 and 2 outside or inside the maths window, 3
  -- always / never
  let black = case (cgwsel `shiftR` 6) .&. 3 of 0 -> False; 1 -> not inWin; 2 -> inWin; _ -> True
      allowed = case (cgwsel `shiftR` 4) .&. 3 of 0 -> True; 1 -> inWin; 2 -> not inWin; _ -> False
      layerOk = case shLayer mainPx of
        4 -> shObjMath mainPx && testBit adsub 4
        5 -> testBit adsub 5
        n -> testBit adsub n
      mainC = if black then 0 else shColor mainPx
      (subC, subIsBack) = case subPx of
        Just s | shLayer s /= 5 -> (shColor s, False)
        _ -> (fixed, True)
      -- the half is not applied to the fixed colour as the sub screen's
      -- backdrop, nor on a forced-black main screen (fullsnes)
      half = testBit adsub 6 && not black && not subIsBack
  if not (layerOk && allowed) then pure mainC else do
    let ch k = (mainC `shiftR` k) .&. 31
        cs k = (subC `shiftR` k) .&. 31
        op k = let r0 = if testBit adsub 7 then ch k - cs k else ch k + cs k
                   r1 = if half then r0 `shiftR` 1 else r0
               in max 0 (min 31 r1)
    pure (op 0 .|. (op 5 `shiftL` 5) .|. (op 10 `shiftL` 10))

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
