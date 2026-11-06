{-# LANGUAGE StrictData #-}
{-|
The Game Gear's VDP in Clash (PLAN.md step 5.6): "Gg.Spec.Vdp" dot for
dot, built for block RAM. The lockstep tests (@gg-lockstep@) replay the
port accesses of real programs and random ones into both and compare
what they show, report and interrupt.

__VRAM__ is four byte-wide RAMs outside this module, interleaved by
address mod 4, read as one 32-bit word ('voReadAddr', a word address; the
data comes back on the next clock, 'viRead') and written a byte at a
time ('voWrite'). A name-table entry or a pattern row is one read.

__A dot is four clocks__ ('viDot' starts it; more clocks per dot just
idle), each with one read:

* sub 0: the result of the previous dot's sprite read is put away (it
  belongs to that dot); then the CPU's port access of this dot
  ('viPort'), with its VRAM write or its read-buffer prefetch;
* sub 1: the prefetch's data into the buffer; this dot's events (as the
  spec's 'Gg.Spec.Vdp.dotStep': the line start, the scroll latch, the line
  interrupt); the name-table read for this dot's pixel;
* sub 2: the pattern read;
* sub 3: the pixel ('voPixel') and the collision; this dot's sprite read
  (scan, X/tile or pattern row), with the register bits it needs.

So, as in the spec, a port access lands before its dot's events, and
everything is read as it is at its own dot. Only Mode 4.
-}
module Gg.Vdp
  ( PortOp (..)
  , VdpIn (..)
  , VdpOut (..)
  , vdpBorderC
  , VdpC (..)
  , Slot (..)
  , SprRead (..)
  , vdpPowerOn
  , vdpClock
  , vdpIntC
  ) where

import Clash.Prelude

-- | The CPU's access to the VDP's ports in a dot.
data PortOp
  = PNone
  | PCtrlW (Unsigned 8)
  | PDataW (Unsigned 8)
  | PDataR
  | PStatusR
  | PVCountR
  | PHCountR
  | PLatchH       -- ^ a TH line went high (port 3Fh): latch the H counter
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFDataX

data VdpIn = VdpIn
  { viDot  :: Bool                 -- ^ this clock starts a dot (sub 0)
  , viPort :: PortOp               -- ^ with 'viDot'
  , viRead :: Vec 4 (Unsigned 8)   -- ^ VRAM bytes 0-3 of the word read on the clock before
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFDataX

data VdpOut = VdpOut
  { voReadAddr :: Unsigned 12                    -- ^ word address read this clock
  , voWrite    :: Maybe (Unsigned 14, Unsigned 8) -- ^ a VRAM byte written this clock
  , voPortData :: Maybe (Unsigned 8)             -- ^ the result of a port read (sub 0)
  , voPixel    :: Maybe (Unsigned 8, Unsigned 8, Unsigned 12) -- ^ x, y in the window, colour (sub 3)
  , voFrameEnd :: Bool                           -- ^ the window's last pixel is out
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFDataX

-- | A sprite slot of the current line.
data Slot = Slot
  { slNum  :: Unsigned 6
  , slDy   :: Unsigned 8
  , slX    :: Signed 10
  , slZoom :: Bool
  , slTile :: Unsigned 8
  , slPat  :: Vec 4 (Unsigned 8)
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFDataX

-- | The sprite read issued at sub 3, put away at the next sub 0.
data SprRead
  = SrNone
  | SrScan (Unsigned 6) (Unsigned 9) Bool Bool  -- ^ sprite i, the line it is for, tall, zoom
  | SrXT (Index 8) Bool                         -- ^ slot j's X and tile; R0 bit 3
  | SrPat (Index 8) Bool                        -- ^ slot j's pattern row; zoom
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFDataX

data VdpC = VdpC
  { vSms      :: Bool
  , vRegs     :: Vec 11 (Unsigned 8)
  , vCram     :: Vec 32 (Unsigned 12)
  , vAddr     :: Unsigned 14
  , vCode     :: Unsigned 2
  , vLatched  :: Bool
  , vLatchB   :: Unsigned 8
  , vBuf      :: Unsigned 8
  , vCramL    :: Unsigned 8
  , vStatus   :: Unsigned 8   -- ^ bits 7-5
  , vLinePend :: Bool
  , vLineUnd  :: Bool
  , vLineCnt  :: Unsigned 8
  , vHScroll  :: Unsigned 8
  , vVScroll  :: Unsigned 8
  , vHLatch   :: Unsigned 8
  , vLine     :: Unsigned 9   -- ^ the current dot
  , vDotN     :: Unsigned 9
  , vSub      :: Unsigned 3
  , vPrefetch :: Maybe (Unsigned 2) -- ^ a prefetch issued at sub 0: the byte's lane
  , vNameE     :: Unsigned 16  -- ^ the name entry of this dot's pixel (sub 2)
  , vNameLane :: Unsigned 2
  , vPatShift :: Unsigned 3   -- ^ the pixel's column in its tile row
  , vSprRead  :: SprRead
  , vSprLane  :: Unsigned 2
  , vScan     :: Vec 8 (Unsigned 6, Unsigned 8) -- ^ sprite number and row, for the next line
  , vScanN    :: Unsigned 4
  , vScanDone :: Bool
  , vScanOvr  :: Bool
  , vSlots    :: Vec 8 Slot
  , vSlotN    :: Unsigned 4
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFDataX

-- | The backdrop's colour (16 + R7's low nibble): the border the spec
-- latches at the frame's end (M6, 'Gg.Spec.Vdp.vdpBorder').
vdpBorderC :: VdpC -> Unsigned 12
vdpBorderC v = vCram v !! (16 + resize (vRegs v !! (7 :: Index 11) .&. 0x0F) :: Unsigned 5)

-- | Power-on: as the spec (R2-R5 FFh, the line counter FFh), at line 0,
-- dot 0. @sms@: Master System mode.
vdpPowerOn :: Bool -> VdpC
vdpPowerOn sms = VdpC
  { vSms = sms
  , vRegs = 0 :> 0 :> 0xFF :> 0xFF :> 0xFF :> 0xFF :> 0 :> 0 :> 0 :> 0 :> 0 :> Nil
  , vCram = repeat 0, vAddr = 0, vCode = 0, vLatched = False, vLatchB = 0, vBuf = 0
  , vCramL = 0, vStatus = 0, vLinePend = False, vLineUnd = False, vLineCnt = 0xFF
  , vHScroll = 0, vVScroll = 0, vHLatch = 0, vLine = 0, vDotN = 0, vSub = 4
  , vPrefetch = Nothing, vNameE = 0, vNameLane = 0, vPatShift = 0
  , vSprRead = SrNone, vSprLane = 0
  , vScan = repeat (0, 0), vScanN = 0, vScanDone = True, vScanOvr = False
  , vSlots = repeat (Slot 0 0 0 False 0 (repeat 0)), vSlotN = 0
  }

-- * Timing (the spec's)

dotsPerLine, linesPerFrame, pixelDot, lineIntDot, hScrollEarly, topBorder, frameIntLine, scanDot :: Unsigned 9
dotsPerLine = 342
linesPerFrame = 262
pixelDot = 35
lineIntDot = 1
hScrollEarly = 2
topBorder = 235
frameIntLine = 0xC1
scanDot = 256

ggLeft, ggTop, ggWidth, ggHeight :: Unsigned 9
ggLeft = 48
ggTop = 24
ggWidth = 160
ggHeight = 144

winLeft, winTop, winWidth, winHeight :: VdpC -> Unsigned 9
winLeft v = if vSms v then 0 else ggLeft
winTop v = if vSms v then 0 else ggTop
winWidth v = if vSms v then 256 else ggWidth
winHeight v = if vSms v then 192 else ggHeight

vCounterOf :: Unsigned 9 -> Unsigned 8
vCounterOf l = resize (if l <= 0xDA then l else l - 6)

hCounterOf :: Unsigned 9 -> Unsigned 8
hCounterOf d =
  let d10 = resize d + 318 :: Unsigned 10  -- (d + 318 needs 10 bits)
      hd = resize (if d10 >= resize dotsPerLine then d10 - resize dotsPerLine else d10) :: Unsigned 9
      h9 = if hd < 296 then hd else 0x1D2 + hd - 296
  in resize (h9 `shiftR` 1)

spriteLine :: Unsigned 9 -> Bool
spriteLine l = l < 192 || l >= topBorder

reg :: VdpC -> Index 11 -> Unsigned 8
reg v r = vRegs v !! r

regBit :: VdpC -> Index 11 -> Int -> Bool
regBit v r b = testBit (reg v r) b

satBase :: VdpC -> Unsigned 14
satBase v = resize (reg v 5 .&. 0x7E) `shiftL` 7

-- | /INT: (frame flag and R1 bit 5) or (line interrupt and R0 bit 4).
vdpIntC :: VdpC -> Bool
vdpIntC v = (testBit (vStatus v) 7 && regBit v 1 5) || (vLinePend v && regBit v 0 4)

lane :: Unsigned 14 -> Unsigned 2
lane = resize

word :: Unsigned 14 -> Unsigned 12
word a = resize (a `shiftR` 2)

idleOut :: VdpOut
idleOut = VdpOut 0 Nothing Nothing Nothing False

-- * The clock

vdpClock :: VdpIn -> VdpC -> (VdpC, VdpOut)
vdpClock vin v0
  | viDot vin = sub0 vin (putAway (viRead vin) v0 { vSub = 0 })
  | otherwise = case vSub v0 of
      0 -> sub1 vin v0 { vSub = 1 }
      1 -> sub2 vin v0 { vSub = 2 }
      2 -> sub3 vin v0 { vSub = 3 }
      _ -> (v0 { vSub = 4 }, idleOut)

-- | The previous dot's sprite read comes back.
putAway :: Vec 4 (Unsigned 8) -> VdpC -> VdpC
putAway bytes v = case vSprRead v of
  SrNone -> v
  SrScan i next tall zoom ->
    let y = bytes !! vSprLane v
        h = (if tall then 16 else 8) * (if zoom then 2 else 1) :: Unsigned 8
        dy = resize next - (y + 1)
    in if y == 0xD0 then v { vScanDone = True, vSprRead = SrNone }
       else if dy < h
         then if vScanN v == 8
           then v { vScanOvr = True, vScanDone = True, vSprRead = SrNone }
           else v { vScan = replace (vScanN v) (i, dy) (vScan v), vScanN = vScanN v + 1, vSprRead = SrNone }
         else v { vSprRead = SrNone }
  SrXT j shift8 ->
    let ln = vSprLane v
        x = bytes !! ln
        t = bytes !! (ln + 1)
        s = vSlots v !! j
        sx = bitCoerce (resize x :: Unsigned 10) - (if shift8 then 8 else 0) :: Signed 10
    in v { vSlots = replace j s { slX = sx, slTile = t } (vSlots v), vSprRead = SrNone }
  SrPat j zoom ->
    let s = vSlots v !! j
    in v { vSlots = replace j s { slPat = bytes, slZoom = zoom } (vSlots v), vSprRead = SrNone }

-- | Sub 0: the port access.
sub0 :: VdpIn -> VdpC -> (VdpC, VdpOut)
sub0 vin v = case viPort vin of
  PNone -> (v { vPrefetch = Nothing }, idleOut)
  PCtrlW b ->
    if not (vLatched v)
      then (v { vLatchB = b, vLatched = True, vAddr = (vAddr v .&. 0x3F00) .|. resize b, vPrefetch = Nothing }, idleOut)
      else
        let code = resize (b `shiftR` 6)
            addr = (resize (b .&. 0x3F) `shiftL` 8) .|. resize (vLatchB v)
            v1 = v { vLatched = False, vCode = code, vAddr = addr, vPrefetch = Nothing }
        in case code of
          0 -> ( v1 { vAddr = addr + 1, vPrefetch = Just (lane addr) }
               , idleOut { voReadAddr = word addr } )
          2 -> let r = resize b .&. 0x0F :: Unsigned 4
                   v2 = if r <= 10 then v1 { vRegs = replace r (vLatchB v) (vRegs v1) } else v1
               in (v2, idleOut)
          _ -> (v1, idleOut)
  PDataW b ->
    let a = vAddr v
        v1 = v { vLatched = False, vBuf = b, vAddr = a + 1, vPrefetch = Nothing }
    in if vCode v == 3
         then if vSms v
           then let ch s = (resize (b `shiftR` s) .&. 3) * 5 :: Unsigned 12
                    c = ch 0 .|. (ch 2 `shiftL` 4) .|. (ch 4 `shiftL` 8)
                in (v1 { vCram = replace (resize a .&. 0x1F :: Unsigned 5) c (vCram v1) }, idleOut)
           else let ca = resize a .&. 0x3F :: Unsigned 6
                in if even ca
                     then (v1 { vCramL = b }, idleOut)
                     else let c = resize (vCramL v) .|. (resize (b .&. 0x0F) `shiftL` 8)
                          in (v1 { vCram = replace (ca `shiftR` 1) c (vCram v1) }, idleOut)
         else (v1, idleOut { voWrite = Just (a, b) })
  PDataR ->
    let a = vAddr v in
    ( v { vLatched = False, vAddr = a + 1, vPrefetch = Just (lane a) }
    , idleOut { voReadAddr = word a, voPortData = Just (vBuf v) } )
  PStatusR ->
    ( v { vLatched = False, vStatus = 0, vLinePend = False, vPrefetch = Nothing }
    , idleOut { voPortData = Just (vStatus v .|. 0x1F) } )
  PVCountR -> (v { vPrefetch = Nothing }, idleOut { voPortData = Just (vCounterOf (vLine v)) })
  PHCountR -> (v { vPrefetch = Nothing }, idleOut { voPortData = Just (vHLatch v) })
  PLatchH -> (v { vHLatch = hCounterOf (vDotN v), vPrefetch = Nothing }, idleOut)

-- | Sub 1: the prefetch's byte, the dot's events, the name-table read.
sub1 :: VdpIn -> VdpC -> (VdpC, VdpOut)
sub1 vin v0 =
  let v1 = case vPrefetch v0 of
        Just ln -> v0 { vBuf = viRead vin !! ln, vPrefetch = Nothing }
        Nothing -> v0
      l = vLine v1
      d = vDotN v1
      v2 = if d == 0 then lineStart v1 else v1
      v3 = if d == dotsPerLine - hScrollEarly
             then let next = if l + 1 == linesPerFrame then 0 else l + 1
                  in v2 { vHScroll = if regBit v2 0 6 && next < 16 then 0 else reg v2 8 }
             else v2
      v4 = if d == lineIntDot && vLineUnd v3 then v3 { vLinePend = True, vLineUnd = False } else v3
      -- the name-table entry of this dot's pixel
      x = d - pixelDot
      vs = if regBit v4 0 7 && x >= 192 then 0 else vVScroll v4
      bx = resize x - vHScroll v4 :: Unsigned 8
      by9 = resize l + resize vs :: Unsigned 10
      by = resize (if by9 >= 224 then (if by9 >= 448 then by9 - 448 else by9 - 224) else by9) :: Unsigned 8
      nameBase = resize (reg v4 2 .&. 0x0E) `shiftL` 10 :: Unsigned 14
      ea = nameBase + (resize (by `shiftR` 3) * 32 + resize (bx `shiftR` 3)) * 2
  in ( v4 { vNameLane = lane ea, vPatShift = resize bx }
     , idleOut { voReadAddr = word ea } )

-- | The line's start (dot 0): as the spec's.
lineStart :: VdpC -> VdpC
lineStart v =
  let l = vLine v
      v1 = if l == 0 then v { vVScroll = reg v 9 } else v
      v2 = if l <= 192
             then if vLineCnt v1 == 0
               then v1 { vLineUnd = True, vLineCnt = reg v1 10 }
               else v1 { vLineCnt = vLineCnt v1 - 1 }
             else v1 { vLineCnt = reg v1 10 }
      v3 = if l == frameIntLine then v2 { vStatus = vStatus v2 .|. 0x80 } else v2
      v4 = if vScanOvr v3 && l < 192 then v3 { vStatus = vStatus v3 .|. 0x40 } else v3
      n = if spriteLine l then vScanN v4 else 0
      slots = zipWith (\(num, dy) s -> s { slNum = num, slDy = dy }) (vScan v4) (vSlots v4)
  in v4 { vSlotN = n, vSlots = slots }

-- | Sub 2: the name entry is back; the pattern read.
sub2 :: VdpIn -> VdpC -> (VdpC, VdpOut)
sub2 vin v =
  let bytes = viRead vin
      ln = vNameLane v
      entry = (resize (bytes !! (ln + 1)) `shiftL` 8) .|. resize (bytes !! ln) :: Unsigned 16
      l = vLine v
      vs = if regBit v 0 7 && vDotN v - pixelDot >= 192 then 0 else vVScroll v
      by9 = resize l + resize vs :: Unsigned 10
      by = resize (if by9 >= 224 then (if by9 >= 448 then by9 - 448 else by9 - 224) else by9) :: Unsigned 8
      row = if testBit entry 10 then 7 - (by .&. 7) else by .&. 7
      tile = entry .&. 0x1FF
      a = resize tile * 32 + resize row * 4 :: Unsigned 14
  in (v { vNameE = entry }, idleOut { voReadAddr = word a })

-- | Sub 3: the pattern is back: the pixel and the collision; the sprite read.
sub3 :: VdpIn -> VdpC -> (VdpC, VdpOut)
sub3 vin v0 =
  let l = vLine v0
      d = vDotN v0
      active = d >= pixelDot && d < pixelDot + 256
      x = d - pixelDot
      (sprCol, opaque) = spriteAt v0 x
      v1 = if active && spriteLine l && opaque >= 2 && regBit v0 1 6
             then v0 { vStatus = vStatus v0 .|. 0x20 } else v0
      -- the pixel
      entry = vNameE v1
      pat = viRead vin
      col = if testBit entry 9 then 7 - vPatShift v1 else vPatShift v1
      bitIx = 7 - col
      bg' = (if testBit (pat !! (3 :: Index 4)) (fromEnum bitIx) then 8 else 0)
            .|. (if testBit (pat !! (2 :: Index 4)) (fromEnum bitIx) then 4 else 0)
            .|. (if testBit (pat !! (1 :: Index 4)) (fromEnum bitIx) then 2 else 0)
            .|. (if testBit (pat !! (0 :: Index 4)) (fromEnum bitIx) then 1 else 0) :: Unsigned 5
      pal = if testBit entry 11 then 16 else 0 :: Unsigned 5
      prio = testBit entry 12
      backdrop = 16 + resize (reg v1 7 .&. 0x0F) :: Unsigned 5
      ci = if not (regBit v1 1 6) || not (regBit v1 0 2) || (regBit v1 0 5 && x < 8)
             then backdrop
             else if sprCol /= 0 && not (prio && bg' /= 0) then 16 + sprCol else pal + bg'
      wl = winLeft v1
      wt = winTop v1
      inWin = active && l < 192 && l >= wt && l < wt + winHeight v1 && x >= wl && x < wl + winWidth v1
      pixel = if inWin then Just (resize (x - wl), resize (l - wt), vCram v1 !! ci) else Nothing
      frameEnd = l == wt + winHeight v1 - 1 && d == pixelDot + wl + winWidth v1
      -- this dot's sprite read
      sat = satBase v1
      next = if l + 1 == linesPerFrame then 0 else l + 1
      (sr, ln, ra, v2) =
        if d < 16
          then let j = resize (d `shiftR` 1) :: Unsigned 3
                   ji = unpack (pack j) :: Index 8
                   s = vSlots v1 !! ji
               in if resize j >= vSlotN v1 then (SrNone, 0, 0, v1)
                  else if even d
                    then let a = sat + 0x80 + resize (slNum s) * 2
                         in (SrXT ji (regBit v1 0 3), lane a, word a, v1)
                    else let zoom = regBit v1 1 0
                             row = if zoom then slDy s `shiftR` 1 else slDy s
                             t = slTile s
                             tile = (if regBit v1 6 2 then 256 else 0)
                                    + (if regBit v1 1 1 then resize (t .&. 0xFE) + resize (row `shiftR` 3) else resize t) :: Unsigned 14
                             a = tile * 32 + resize (row .&. 7) * 4
                         in (SrPat ji zoom, 0, word a, v1)
          else if d >= scanDot && d < scanDot + 64
            then let i = resize (d - scanDot) :: Unsigned 6
                     v' = if i == 0 then v1 { vScanN = 0, vScanOvr = False, vScanDone = not (spriteLine next) } else v1
                 in if vScanDone v' then (SrNone, 0, 0, v')
                    else let a = sat + resize i
                         in (SrScan i next (regBit v' 1 1) (regBit v' 1 0), lane a, word a, v')
            else (SrNone, 0, 0, v1)
      -- the next dot
      dn = if d + 1 == dotsPerLine then 0 else d + 1
      ln' = if d + 1 == dotsPerLine then (if l + 1 == linesPerFrame then 0 else l + 1) else l
  in ( v2 { vSprRead = sr, vSprLane = ln, vDotN = dn, vLine = ln' }
     , idleOut { voReadAddr = ra, voPixel = pixel, voFrameEnd = frameEnd } )

-- | The sprites at pixel x: the first opaque one's colour (0: none) and
-- how many are opaque there.
spriteAt :: VdpC -> Unsigned 9 -> (Unsigned 5, Unsigned 4)
spriteAt v x =
  let px = bitCoerce (resize x :: Unsigned 10) :: Signed 10
      one j s =
        let off = px - slX s
            w = if slZoom s then 16 else 8
            inRange = fromEnum j < fromEnum (vSlotN v) && off >= 0 && off < w
            o = bitCoerce off :: Unsigned 10
            colIx = resize (if slZoom s then o `shiftR` 1 else o) :: Unsigned 3
            b = 7 - colIx
            p k = if testBit (slPat s !! (k :: Index 4)) (fromEnum b) then 1 else 0 :: Unsigned 5
            c = p 0 .|. (p 1 `shiftL` 1) .|. (p 2 `shiftL` 2) .|. (p 3 `shiftL` 3)
        in if inRange then c else 0
      cols = imap one (vSlots v)
      first = foldr (\c acc -> if c /= 0 then c else acc) 0 cols
      count = sum (map (\c -> if c /= 0 then 1 else 0) cols) :: Unsigned 4
  in (first, count)
