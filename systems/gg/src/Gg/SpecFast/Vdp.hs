{-# OPTIONS_GHC -fworker-wrapper -fspecialise-aggressively -funfolding-use-threshold=200 -funfolding-creation-threshold=1000 #-}
-- (the fast version is never synthesised: GHC's unboxing back on, which
-- clash-common's -fno-worker-wrapper turns off for Clash's sake)
-- Side track Q (PLAN.md Q.7): the desktop emulator's fast version of
-- Gg.Spec.Vdp, kept beside it (the original stays the reference the
-- core's lockstep checks; a fix in either goes to both). The
-- sms-specfast suite runs both machines side by side.
{-# LANGUAGE BangPatterns #-}
{-|
The Game Gear's VDP (step 5.3; docs/systems/gamegear.md, "Machine summary"
and "VDP timing"): the Master System's Mode 4 with the Game Gear's 12-bit
CRAM, NTSC, 192 lines, at dot level.

__Time__ is counted in dots (342 per line, 262 lines; 1.5 dots per CPU
T-state). A line here starts where the V counter increments, at H
counter F4h (the 9-bit dot counter at 1E8h); its first active pixel is
'pixelDot' (35) dots later, so pixel x is dot x + 35 of its line. The
machine calls 'vdpSync' with the current dot before every VDP port access
and before sampling /INT, so the VDP has drawn every pixel and passed
every event up to that point: a register, CRAM or scroll write takes
effect at the dot it lands on, within the latches below.

__Calibration.__ The positions are from Sega's GG Hardware Reference
Manual and MacDonald's VDP notes (cross-checked with MAME and Genesis
Plus GX), then fixed to the dot by FluBBa's SMS VDP Test 1.31, which
passes in full (gamegear.md, "VDP timing"): with the machine's sampling
points ('Gg.Spec': I/O at T3, /INT in the last T-state, the dot phase),
the H counter sequence, the V counter change, both interrupts, the flags
and the scroll latch all land where that test measured them on a
Master System.

__Per line:__ 'hScrollEarly' (2) dots before a line starts its horizontal
scroll is latched (0 on lines 0 to 15 with R0 bit 6). At its start (dot
0): on line 0 the vertical scroll is latched; the V counter changes; the
line counter is decremented on lines 0 to 192 or reloaded from R10 on the
others; line 'frameIntLine' (C1h) sets the frame interrupt flag; the
line's sprites are evaluated (up to eight; a ninth sets the overflow flag,
lines 0 to 191 only). An underflow of the line counter raises the line
interrupt 'lineIntDot' (1) dot later. Sprite collision is flagged at the
pixel where two opaque sprite pixels meet, anywhere on the 256-pixel
line while the display is on, on the active lines and on the top border
('topBorder'), where sprites are evaluated but not drawn.

__Pixels__ are drawn only inside the Game Gear's 160×144 window (columns
48 to 207, lines 24 to 167); the frame buffer holds their CRAM colours
(----BBBBGGGGRRRR).

__Master System mode__ ('newVdpSms'; the Game Gear running a Master
System cartridge): CRAM holds 32 one-byte colours (--BBGGRR, written
directly, no latch), each 2-bit channel shown as 4 bits (× 5), and the
whole 256×192 picture is drawn. Timing is the same.

__Not modelled__ (uncertain or unused on the Game Gear): the overflow
flag's dependence on the frame flag (the GG manual; MAME follows it,
Genesis Plus GX doesn't), VRAM access slots (writes too close together
during the active display are lost on the Master System 2; not seen on
the GG), the 224- and 240-line modes, any GG-mode timing offset (MAME
puts some GG events 1 or 2 H counts later; unmeasured, so GG mode uses
the Master System timing).

Only Mode 4 is modelled (the Game Gear's own games use nothing else); in
another mode the screen shows the backdrop.
-}
module Gg.SpecFast.Vdp
  ( Vdp
  , vdpFields
  , newVdp
  , vdpRebase
  , newVdpSms
  , vdpSms
  , vdpWidth
  , vdpHeight
    -- * Timing
  , dotsPerLine
  , linesPerFrame
  , dotsPerFrame
  , pixelDot
  , frameIntLine
  , vCounter
  , hCounter
    -- * Running
  , vdpSync
  , vdpDot
  , vdpInt
  , vdpFrames
  , vdpBorder
    -- * Ports
  , vdpControlWrite
  , vdpDataWrite
  , vdpDataRead
  , vdpStatusRead
  , vdpVCounter
  , vdpHCounter
  , vdpLatchHCounter
    -- * Inspection
  , vdpRegister
  , vdpVram
  , vdpCram
  , vdpFrame
  , vdpSnapshot
  , vdpLoadVram
  , ggWidth
  , ggHeight
  ) where

import Prelude

import Control.Monad (forM_, when)
import Control.Monad.ST (ST)
import Data.Bits
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MV
import Platform.Snapshot (Field (..))
import Data.Word

-- * Timing

dotsPerLine, linesPerFrame, dotsPerFrame :: Int
dotsPerLine = 342
linesPerFrame = 262
dotsPerFrame = dotsPerLine * linesPerFrame

-- | The dot of a line (from H = F4h) where its first active pixel is.
pixelDot :: Int
pixelDot = 35

-- | The dot (after the line's start) where an underflow of the line
-- counter raises the line interrupt: a dot after the frame interrupt
-- (FluBBa: HClock -17 against -18).
lineIntDot :: Int
lineIntDot = 1

-- | How many dots before a line starts its horizontal scroll is latched
-- (FluBBa: between HClock -18 and -17, the V counter changing at -16).
hScrollEarly :: Int
hScrollEarly = 2

-- | The first line of the top border (NTSC: lines 235 to 261). Sprites
-- are evaluated there too, so they collide (FluBBa's "Offscreen Y, col":
-- sprites on lines whose V counter reads F1h-F8h), but they set no
-- overflow ("Offscreen Y, no ovr") and nothing is drawn. *To verify*: the
-- exact first line (Genesis Plus GX and MAME use the last 13 to 16).
topBorder :: Int
topBorder = 235

-- | The line at whose start the frame interrupt flag is set.
frameIntLine :: Int
frameIntLine = 0xC1

-- | The V counter on line @l@ in the 192-line mode: 00h-DAh, then D5h-FFh.
vCounter :: Int -> Word8
vCounter l
  | l <= 0xDA = fromIntegral l
  | otherwise = fromIntegral (l - 6)

-- | The H counter at dot @d@ of a line (0 = the start, H = F4h): the
-- 9-bit dot counter counts 000h-127h, then 1D2h-1FFh; the port shows its
-- upper 8 bits (00h-93h, E9h-FFh).
hCounter :: Int -> Word8
hCounter d =
  let hd = (d + 318) `mod` dotsPerLine
      h9 = if hd < 296 then hd else 0x1D2 + hd - 296
  in fromIntegral (h9 `shiftR` 1)

ggWidth, ggHeight, ggLeft, ggTop :: Int
ggWidth = 160
ggHeight = 144
ggLeft = 48
ggTop = 24

-- * State

data Vdp s = Vdp
  { vSms   :: !Bool                  -- ^ Master System mode
  , vVram  :: !(MV.MVector s Word8)  -- ^ 16 KiB
  , vCram  :: !(MV.MVector s Word8)  -- ^ 64 bytes: 32 entries ----BBBBGGGGRRRR (SMS mode: converted)
  , vRegs  :: !(MV.MVector s Int)    -- ^ R0-R10
  , vState :: !(MV.MVector s Int)
  , vSpr   :: !(MV.MVector s Int)    -- ^ the sprite slots (layout at 'spScan')
  , vFb    :: !(MV.MVector s Word16) -- ^ 'vdpWidth' × 'vdpHeight'
  }

sDot, sAddr, sCode, sLatched, sLatch, sBuf, sCramLatch, sStatus, sLinePend, sLineCnt, sHScroll, sVScroll, sFrames, sHLatch, sLineUnder, sBorder, slots :: Int
sDot = 0        -- dots run since power-on
sAddr = 1
sCode = 2
sLatched = 3
sLatch = 4
sBuf = 5
sCramLatch = 6
sStatus = 7     -- bits 7 (frame), 6 (overflow), 5 (collision)
sLinePend = 8
sLineCnt = 9
sHScroll = 10   -- latched for this line
sVScroll = 11   -- latched for this frame
sFrames = 12    -- frames completed (the window's last line drawn)
sHLatch = 13    -- the H counter as last latched (port 7Fh)
sLineUnder = 14 -- the line counter underflowed at this line's start; the interrupt follows at 'lineIntDot'
sBorder = 15    -- the backdrop's colour as the last frame completed (M6: the Master System's border)
slots = 16

newVdp :: ST s (Vdp s)
newVdp = newVdpMode False

newVdpSms :: ST s (Vdp s)
newVdpSms = newVdpMode True

vdpSms :: Vdp s -> Bool
vdpSms = vSms

-- | The frame buffer's size: the GG window, or the whole SMS picture.
vdpWidth, vdpHeight :: Vdp s -> Int
vdpWidth v = if vSms v then 256 else ggWidth
vdpHeight v = if vSms v then 192 else ggHeight

winLeft, winTop :: Vdp s -> Int
winLeft v = if vSms v then 0 else ggLeft
winTop v = if vSms v then 0 else ggTop

newVdpMode :: Bool -> ST s (Vdp s)
newVdpMode sms = do
  vram <- MV.replicate 0x4000 0
  cram <- MV.replicate 64 0
  regs <- MV.replicate 11 0
  st <- MV.replicate slots 0
  spr <- MV.replicate spSize 0
  fb <- MV.replicate (if sms then 256 * 192 else ggWidth * ggHeight) 0
  -- R2-R5 as the BIOS-less games find them: all bits set (*to verify*)
  mapM_ (\r -> MV.write regs r 0xFF) [2, 3, 4, 5]
  MV.write st sLineCnt 0xFF
  pure (Vdp sms vram cram regs st spr fb)

gs :: Vdp s -> Int -> ST s Int
gs v = MV.unsafeRead (vState v)
{-# INLINE gs #-}

ss :: Vdp s -> Int -> Int -> ST s ()
ss v = MV.unsafeWrite (vState v)
{-# INLINE ss #-}

reg :: Vdp s -> Int -> ST s Int
reg v = MV.unsafeRead (vRegs v)
{-# INLINE reg #-}

regBit :: Vdp s -> Int -> Int -> ST s Bool
regBit v r b = (`testBit` b) <$> reg v r
{-# INLINE regBit #-}

-- * Running

-- | Dots run since power-on.
vdpDot :: Vdp s -> ST s Int
vdpDot v = gs v sDot

-- | Frames completed.
vdpFrames :: Vdp s -> ST s Int
vdpFrames v = gs v sFrames

-- | The border's colour for the last completed frame (M6): the backdrop
-- (colour 16 + R7's low nibble) as the frame completed. A TV shows the
-- backdrop around the picture; the board shows the border with this one
-- colour per frame (a game changing the backdrop mid-border is not
-- followed; none known).
vdpBorder :: Vdp s -> ST s Word16
vdpBorder v = fromIntegral <$> gs v sBorder

-- | Run the VDP up to (not including) dot @target@.
-- | The dot count moved back by @n@ dots, a whole number of frames
-- ("Gg.SpecFast"'s 'rebase': WebAssembly's Int is 32 bits); the VDP only
-- uses it modulo a line or a frame.
vdpRebase :: Vdp s -> Int -> ST s ()
vdpRebase v n = gs v sDot >>= ss v sDot . subtract n

vdpSync :: Vdp s -> Int -> ST s ()
vdpSync v target = do
  cur <- gs v sDot
  -- (side track Q: the line and dot counted along, not divided out of the
  -- dot count every dot)
  let fd0 = cur `mod` dotsPerFrame
      go !n !l !d
        | n >= target = ss v sDot n
        | otherwise = do
            dotStep v l d
            if d + 1 < dotsPerLine then go (n + 1) l (d + 1)
              else go (n + 1) (if l + 1 < linesPerFrame then l + 1 else 0) 0
  when (cur < target) (go cur (fd0 `div` dotsPerLine) (fd0 `mod` dotsPerLine))

dotStep :: Vdp s -> Int -> Int -> ST s ()
dotStep v l d = do
  when (d == 0) $ do
    when (l == 0) $ reg v 9 >>= ss v sVScroll
    if l <= 192
      then do
        c <- gs v sLineCnt
        if c == 0
          then do
            ss v sLineUnder 1
            reg v 10 >>= ss v sLineCnt
          else ss v sLineCnt (c - 1)
      else reg v 10 >>= ss v sLineCnt
    when (l == frameIntLine) $ gs v sStatus >>= ss v sStatus . (.|. 0x80)
    startSprites v l
  when (d < 16) $ fetchSprite v l d
  when (d >= spriteScanDot && d < spriteScanDot + 64) $
    scanSprite v ((l + 1) `mod` linesPerFrame) (d - spriteScanDot)
  -- the next line's horizontal scroll, latched just before it starts
  when (d == dotsPerLine - hScrollEarly) $ do
    lock <- regBit v 0 6
    h <- reg v 8
    let next = (l + 1) `mod` linesPerFrame
    ss v sHScroll (if lock && next < 16 then 0 else h)
  when (d == lineIntDot) $ do
    u <- gs v sLineUnder
    when (u /= 0) $ ss v sLinePend 1 >> ss v sLineUnder 0
  when ((l < 192 || l >= topBorder) && d >= pixelDot && d < pixelDot + 256) $ do
    let x = d - pixelDot
    -- (side track Q: the sprites looked up once, for the collision and
    -- the pixel)
    sprs <- spriteAt v x
    let opaque = sprs `shiftR` 4
    when (opaque >= 2) $ do
      on <- regBit v 1 6
      when on $ gs v sStatus >>= ss v sStatus . (.|. 0x20)
    let wl = winLeft v
        wt = winTop v
        ww = vdpWidth v
    when (l < 192 && l >= wt && l < wt + vdpHeight v && x >= wl && x < wl + ww) $ do
      c <- pixel v l x (sprs .&. 15)
      col <- cramColour v c
      MV.unsafeWrite (vFb v) ((l - wt) * ww + (x - wl)) col
  when (l == winTop v + vdpHeight v - 1 && d == pixelDot + winLeft v + vdpWidth v) $ do
    gs v sFrames >>= ss v sFrames . (+ 1)
    r7 <- reg v 7
    cramColour v (16 + (r7 .&. 0x0F)) >>= ss v sBorder . fromIntegral

-- | /INT: (frame flag and R1 bit 5) or (line interrupt pending and R0 bit 4).
vdpInt :: Vdp s -> ST s Bool
vdpInt v = do
  s <- gs v sStatus
  ie0 <- regBit v 1 5
  lp <- gs v sLinePend
  ie1 <- regBit v 0 4
  pure ((testBit s 7 && ie0) || (lp /= 0 && ie1))

-- * Drawing

cramColour :: Vdp s -> Int -> ST s Word16
cramColour v c = do
  lo <- MV.unsafeRead (vCram v) (c * 2)
  hi <- MV.unsafeRead (vCram v) (c * 2 + 1)
  pure (fromIntegral lo .|. (fromIntegral (hi .&. 0x0F) `shiftL` 8))

-- | The colour index (0-31) of dot @d@ on line @l@ (both in the active
-- area), the sprites' colour there given ('spriteAt').
pixel :: Vdp s -> Int -> Int -> Int -> ST s Int
pixel v l x spr = do
  r0 <- reg v 0
  r1 <- reg v 1
  backdrop <- (\r7 -> 16 + (r7 .&. 0x0F)) <$> reg v 7
  if not (testBit r1 6) || not (testBit r0 2) || (testBit r0 5 && x < 8)
    then pure backdrop
    else do
      hs <- gs v sHScroll
      vsLatched <- gs v sVScroll
      r2 <- reg v 2
      let vs = if testBit r0 7 && x >= 192 then 0 else vsLatched
          bx = (x - hs) .&. 0xFF
          by = (l + vs) `mod` 224
          nameBase = (r2 .&. 0x0E) `shiftL` 10
          ea = nameBase + ((by `shiftR` 3) * 32 + (bx `shiftR` 3)) * 2
      lo <- fromIntegral <$> MV.unsafeRead (vVram v) ea
      hi <- fromIntegral <$> MV.unsafeRead (vVram v) (ea + 1)
      let entry = lo .|. (hi `shiftL` 8) :: Int
          tile = entry .&. 0x1FF
          row = if testBit entry 10 then 7 - (by .&. 7) else by .&. 7
          col = if testBit entry 9 then 7 - (bx .&. 7) else bx .&. 7
          pal = if testBit entry 11 then 16 else 0
          prio = testBit entry 12
      bg <- tilePixel v (tile * 32 + row * 4) col
      pure $ if spr /= 0 && not (prio && bg /= 0) then 16 + spr else pal + bg

-- | Pixel @col@ (0 = left) of the tile row at VRAM address @a@.
tilePixel :: Vdp s -> Int -> Int -> ST s Int
tilePixel v a col = do
  let b7 = 7 - col
      plane i = (\b -> if testBit (b :: Word8) b7 then 1 `shiftL` i else 0) <$> MV.unsafeRead (vVram v) ((a + i) .&. 0x3FFF)
  -- (side track Q: the four planes without a list)
  p0 <- plane 0
  p1 <- plane 1
  p2 <- plane 2
  p3 <- plane 3
  pure (p0 .|. p1 .|. p2 .|. p3)
{-# INLINE tilePixel #-}

-- * Sprites
--
-- Spread over dots as a VDP must do it (the sources: evaluation during
-- line N - 1, fetching during line N; the dots are ours, *to verify*):
-- the Y of sprite i is read at dot 'spriteScanDot' + i of the line
-- before (the scan stops at D0h or at a ninth sprite on the line: the
-- overflow); at the line's start the overflow flag is set (lines 0 to
-- 191) and the up to eight found become the line's slots; slot j reads
-- its X and tile at dot 2j and its pattern row at dot 2j + 1 (all done
-- before the first pixel, dot 'pixelDot'). Each pixel then takes the
-- first slot with an opaque pixel there; two or more opaque ones are a
-- collision (flagged while the display is on).

-- | The dot (of the line before) where the scan reads sprite 0's Y.
spriteScanDot :: Int
spriteScanDot = 256

-- Layout of 'vSpr': the scan (8 × sprite number and its row, count,
-- done, overflow), then the line's slots (count; per slot: row, X,
-- zoom, tile, pattern bytes 0-3).
spScan, spScanCount, spScanDone, spScanOvr, spCount, spSlots, spSlot, spSize :: Int
spScan = 0
spScanCount = 16
spScanDone = 17
spScanOvr = 18
spCount = 19
spSlots = 20
spSlot = 8
spSize = spSlots + 8 * spSlot

sp :: Vdp s -> Int -> ST s Int
sp v = MV.unsafeRead (vSpr v)

spw :: Vdp s -> Int -> Int -> ST s ()
spw v = MV.unsafeWrite (vSpr v)

-- | Sprites are evaluated for the active lines and the top border.
spriteLine :: Int -> Bool
spriteLine l = l < 192 || l >= topBorder

satBase :: Vdp s -> ST s Int
satBase v = (\r5 -> (r5 .&. 0x7E) `shiftL` 7) <$> reg v 5

-- | Sprite @i@'s Y, for line @next@.
scanSprite :: Vdp s -> Int -> Int -> ST s ()
scanSprite v next i = do
  when (i == 0) $ do
    spw v spScanCount 0
    spw v spScanOvr 0
    spw v spScanDone (if spriteLine next then 0 else 1)
  doneScan <- sp v spScanDone
  when (doneScan == 0) $ do
    sat <- satBase v
    r1 <- reg v 1
    y <- fromIntegral <$> MV.unsafeRead (vVram v) (sat + i)
    if y == 0xD0
      then spw v spScanDone 1
      else do
        let h = (if testBit r1 1 then 16 else 8) * (if testBit r1 0 then 2 else 1)
            dy = (next - (y + 1)) .&. 0xFF
        when (dy < h) $ do
          n <- sp v spScanCount
          if n == 8
            then spw v spScanOvr 1 >> spw v spScanDone 1
            else do
              spw v (spScan + 2 * n) i
              spw v (spScan + 2 * n + 1) dy
              spw v spScanCount (n + 1)

-- | The line starts: the overflow flag, the scan's sprites become the slots.
startSprites :: Vdp s -> Int -> ST s ()
startSprites v l = do
  ovr <- sp v spScanOvr
  when (ovr /= 0 && l < 192) $ gs v sStatus >>= ss v sStatus . (.|. 0x40)
  n <- if spriteLine l then sp v spScanCount else pure 0
  spw v spCount n
  forM_ [0 .. 7] $ \j -> do
    sp v (spScan + 2 * j) >>= spw v (spSlots + j * spSlot + 7) -- the sprite number, for the X/tile read
    sp v (spScan + 2 * j + 1) >>= spw v (spSlots + j * spSlot)

-- | Dot @d@ (0-15) of line @l@: slot d / 2's X and tile, or its pattern row.
fetchSprite :: Vdp s -> Int -> Int -> ST s ()
fetchSprite v _ d = do
  let j = d `div` 2
      base = spSlots + j * spSlot
  n <- sp v spCount
  when (j < n) $ do
    sat <- satBase v
    if even d
      then do
        num <- sp v (base + 7)
        r0 <- reg v 0
        x <- fromIntegral <$> MV.unsafeRead (vVram v) (sat + 0x80 + num * 2)
        t <- fromIntegral <$> MV.unsafeRead (vVram v) (sat + 0x81 + num * 2)
        spw v (base + 1) (x - (if testBit r0 3 then 8 else 0))
        spw v (base + 3) t
      else do
        r1 <- reg v 1
        r6 <- reg v 6
        dy <- sp v base
        t <- sp v (base + 3)
        let zoom = if testBit r1 0 then 2 else 1
            row = dy `div` zoom
            tile = (if testBit r6 2 then 256 else 0) + (if testBit r1 1 then (t .&. 0xFE) + row `div` 8 else t)
            a = tile * 32 + (row .&. 7) * 4
        spw v (base + 2) zoom
        forM_ [0 .. 3] $ \k -> do
          b <- fromIntegral <$> MV.unsafeRead (vVram v) ((a + k) .&. 0x3FFF)
          spw v (base + 4 + k) b

-- | The sprites at pixel x of this line: the first opaque one's colour
-- (0: none) in bits 0-3 and how many are opaque there from bit 4 (side
-- track Q: packed, no tuple or list).
spriteAt :: Vdp s -> Int -> ST s Int
spriteAt v x = do
  n <- sp v spCount
  let go !j !col !cnt
        | j >= n = pure (col .|. (cnt `shiftL` 4))
        | otherwise = do
            let base = spSlots + j * spSlot
            sx <- sp v (base + 1)
            zoom <- sp v (base + 2)
            let off = x - sx
            if off < 0 || off >= 8 * zoom
              then go (j + 1) col cnt
              else do
                let b7 = 7 - off `div` zoom
                    bitOf k b = if testBit b b7 then 1 `shiftL` k else 0
                p0 <- bitOf 0 <$> sp v (base + 4)
                p1 <- bitOf 1 <$> sp v (base + 5)
                p2 <- bitOf 2 <$> sp v (base + 6)
                p3 <- bitOf 3 <$> sp v (base + 7)
                let c = p0 .|. p1 .|. p2 .|. p3 :: Int
                if c == 0 then go (j + 1) col cnt
                  else go (j + 1) (if cnt == 0 then c else col) (cnt + 1)
  go 0 0 (0 :: Int)

-- * Ports

vdpControlWrite :: Vdp s -> Word8 -> ST s ()
vdpControlWrite v b = do
  latched <- gs v sLatched
  if latched == 0
    then do
      ss v sLatch (fromIntegral b)
      ss v sLatched 1
      a <- gs v sAddr
      ss v sAddr ((a .&. 0x3F00) .|. fromIntegral b)
    else do
      ss v sLatched 0
      lo <- gs v sLatch
      let code = fromIntegral b `shiftR` 6
          addr = ((fromIntegral b .&. 0x3F) `shiftL` 8) .|. lo
      ss v sCode code
      ss v sAddr addr
      case code of
        0 -> do
          x <- MV.unsafeRead (vVram v) addr
          ss v sBuf (fromIntegral x)
          ss v sAddr ((addr + 1) .&. 0x3FFF)
        2 -> do
          let r = fromIntegral b .&. 0x0F
          when (r <= 10) (MV.unsafeWrite (vRegs v) r lo)
        _ -> pure ()

vdpDataWrite :: Vdp s -> Word8 -> ST s ()
vdpDataWrite v b = do
  ss v sLatched 0
  code <- gs v sCode
  addr <- gs v sAddr
  if code == 3 && vSms v
    then do
      -- --BBGGRR, each channel 2 bits -> 4
      let ca = (addr .&. 0x1F) * 2
          ch s = fromIntegral ((b `shiftR` s) .&. 3) * 5 :: Word8
      MV.unsafeWrite (vCram v) ca (ch 0 .|. (ch 2 `shiftL` 4))
      MV.unsafeWrite (vCram v) (ca + 1) (ch 4)
  else if code == 3
    then do
      let ca = addr .&. 0x3F
      if even ca
        then ss v sCramLatch (fromIntegral b)
        else do
          lo <- gs v sCramLatch
          MV.unsafeWrite (vCram v) (ca - 1) (fromIntegral lo)
          MV.unsafeWrite (vCram v) ca (b .&. 0x0F)
    else MV.unsafeWrite (vVram v) addr b
  ss v sBuf (fromIntegral b)
  ss v sAddr ((addr + 1) .&. 0x3FFF)

vdpDataRead :: Vdp s -> ST s Word8
vdpDataRead v = do
  ss v sLatched 0
  r <- gs v sBuf
  addr <- gs v sAddr
  x <- MV.unsafeRead (vVram v) addr
  ss v sBuf (fromIntegral x)
  ss v sAddr ((addr + 1) .&. 0x3FFF)
  pure (fromIntegral r)

-- | The status register; reading clears its flags, the line interrupt
-- and the control port's latch.
vdpStatusRead :: Vdp s -> ST s Word8
vdpStatusRead v = do
  ss v sLatched 0
  s <- gs v sStatus
  ss v sStatus 0
  ss v sLinePend 0
  pure (fromIntegral s .|. 0x1F)

vdpVCounter :: Vdp s -> ST s Word8
vdpVCounter v = do
  d <- gs v sDot
  pure (vCounter ((d `mod` dotsPerFrame) `div` dotsPerLine))

-- | Port 7Fh: the H counter as latched by 'vdpLatchHCounter' (a TH
-- input going high; MacDonald's notes, MAME, Genesis Plus GX). Nothing
-- drives TH on a Game Gear, so games see a stale value (0 from power-on).
vdpHCounter :: Vdp s -> ST s Word8
vdpHCounter v = fromIntegral <$> gs v sHLatch

vdpLatchHCounter :: Vdp s -> ST s ()
vdpLatchHCounter v = do
  d <- gs v sDot
  ss v sHLatch (fromIntegral (hCounter (d `mod` dotsPerLine)))

-- * Inspection

vdpRegister :: Vdp s -> Int -> ST s Word8
vdpRegister v r = fromIntegral <$> reg v r

vdpVram :: Vdp s -> Int -> ST s Word8
vdpVram v = MV.unsafeRead (vVram v)

vdpCram :: Vdp s -> Int -> ST s Word8
vdpCram v = MV.unsafeRead (vCram v)

-- | The frame buffer as it is now ('vdpWidth' × 'vdpHeight' colours,
-- ----BBBBGGGGRRRR).
vdpFrame :: Vdp s -> ST s (U.Vector Word16)
vdpFrame v = U.freeze (vFb v)

-- | The VDP's state for a save state (side track N): address, code, the
-- control port's latch flag and byte, the read buffer, the CRAM latch,
-- the status flags, the line interrupt's pending flag and counter, the H
-- latch, and the dots run since power-on (in this order).
vdpSnapshot :: Vdp s -> ST s [Int]
vdpSnapshot v = mapM (gs v) [sAddr, sCode, sLatched, sLatch, sBuf, sCramLatch, sStatus, sLinePend, sLineCnt, sHLatch, sDot]

-- | All of VRAM at once (side track N: the PS's window, for the tests).
vdpLoadVram :: Vdp s -> U.Vector Word8 -> ST s ()
vdpLoadVram v img = U.copy (vVram v) (U.take 0x4000 img)

-- | The VDP's state for a save state (Q.8, "Platform.Snapshot").
vdpFields :: Vdp s -> [Field s]
vdpFields v = [FW8 (vVram v), FW8 (vCram v), FInt (vRegs v), FInt (vState v), FInt (vSpr v), FW16 (vFb v)]
