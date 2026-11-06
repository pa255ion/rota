{-|
The PPU in Clash (PLAN.md step 2.5). It reproduces the reference model's
PPU ("Gb.Spec.Ppu") dot for dot; see that module for the rules (mode
timing, mode 3 stalls, window, objects, LY/LYC/STAT details).

__Interface.__ The machine owns the registers, VRAM and OAM; the PPU gets
them as inputs ('PpuIn') and receives commands:

* 'CmdDot': process one dot. Takes 3 clocks ('poBusy' meanwhile), with
  two VRAM reads: the BG/window map byte, then the tile row, from a VRAM
  organised as 4096 16-bit pairs (even byte low, odd byte high, so a tile
  row is one read). The machine gives four per M-cycle, then 'CmdStat'.
* 'CmdStat': re-evaluate the LY = LYC flag and the STAT interrupt line
  (after the M-cycle's dots, and after CPU writes to STAT or LYC).
* 'CmdLcdc': LCDC was just written (switches the LCD on or off), then the
  same as 'CmdStat'.

Objects: OAM scan copies the entries (as the spec does), and their tile
rows are read during the first stall dots of mode 3, two per dot; VRAM
can't change in mode 3 and the object size is latched at its start, so
this matches the spec, which reads them all at dot 80.

The dot logic itself ('dotLogic') is pure; the Mealy wrapper ('ppu') only
sequences the VRAM reads.
-}
module Gb.Ppu
  ( PpuRegs (..)
  , Oam (..)
  , PpuCmd (..)
  , PpuIn (..)
  , PpuOut (..)
  , Obj (..)
  , TileKey (..)
  , PpuState (..)
  , Micro (..)
  , ppuInit
  , ppuStep
  , ppu
    -- * Views for the machine
  , lyView
  , statView
  , Blocked (..)
  , blocked
  ) where

import Clash.Prelude

-- | The PPU's registers, as the machine holds them.
data PpuRegs = PpuRegs
  { prLcdc, prStat, prScy, prScx, prLyc, prBgp, prObp0, prObp1, prWy, prWx :: BitVector 8
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFDataX

-- | One OAM entry.
data Oam = Oam { oaY, oaX, oaTile, oaAttr :: BitVector 8 }
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFDataX

data PpuCmd = CmdDot | CmdStat | CmdLcdc
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFDataX

data PpuIn = PpuIn
  { piCmd  :: Maybe PpuCmd
  , piRegs :: PpuRegs
  , piOam  :: Vec 40 Oam
  , piDma  :: Bool          -- ^ OAM DMA is copying
  , piVram :: BitVector 16  -- ^ the pair at the address of the previous clock
  , piCgbHw :: Bool         -- ^ the console is a Game Boy Color (side track I, phase 3)
  , piVram1 :: BitVector 16 -- ^ VRAM bank 1's pair at the same address (CGB)
  , piCgb   :: Bool         -- ^ CGB mode: attributes, palettes, CGB priority
  , piOpri  :: Bool         -- ^ OPRI bit 0: objects by X (DMG) rather than OAM order
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFDataX

data PpuOut = PpuOut
  { poVramAddr :: Unsigned 12       -- ^ pair to read (data next clock)
  , poBusy     :: Bool
  , poState    :: PpuState          -- ^ after the last command
  , poVblank   :: Bool              -- ^ request the VBlank interrupt
  , poStatIrq  :: Bool              -- ^ request the STAT interrupt
  , poPixel    :: Maybe (Index 23040, BitVector 2)
  , poFrame    :: Bool              -- ^ the frame is complete (line 144 starts)
  , poPalByte  :: Unsigned 7        -- ^ with 'poPixel' on the Game Boy Color: the colour's first byte in palette RAM
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFDataX

-- | A line object: OAM entry copied at OAM scan, tile row read at mode 3.
data Obj = Obj { obY, obX, obTile, obAttr, obLo, obHi, obIdx :: BitVector 8 }
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFDataX

-- | The tile an object's penalty considered (Pan Docs' OBJ penalty
-- algorithm): none yet, the X = 0 position, a window tile, a BG tile.
data TileKey = TkNone | TkX0 | TkWin (Unsigned 5) | TkBg (Unsigned 7)
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFDataX

data PpuState = PpuState
  { psOn       :: Bool
  , psLy       :: Unsigned 8
  , psDot      :: Unsigned 9   -- ^ the next dot of the line
  , psMode     :: Unsigned 2
  , psFirst    :: Bool         -- ^ first line after LCD on
  , psStatLine :: Bool
  , psLycFlag  :: Bool
  , psPx       :: Unsigned 8   -- ^ mode 3: next pixel, 0 to 160
  , psStall    :: Unsigned 5
  , psScxf     :: Unsigned 3
  , psWinX     :: Maybe (Unsigned 8)
  , psWinShift :: Unsigned 3
  , psWinLine  :: Unsigned 8
  , psWyTrig   :: Bool
  , psN        :: Index 11     -- ^ line objects
  , psNext     :: Index 11     -- ^ next object whose penalty is due
  , psLast     :: TileKey
  , psObjs     :: Vec 10 Obj
  , psRows     :: Index 11     -- ^ object rows fetched so far this line
  , psTall     :: Bool         -- ^ 8×16 objects, latched at mode 3
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFDataX

-- | At the hand-off: line 153, dot 400, mode 1 (as 'Gb.Spec.Machine').
ppuInit :: PpuState
ppuInit = PpuState
  { psOn = True, psLy = 153, psDot = 400, psMode = 1, psFirst = False
  , psStatLine = False, psLycFlag = False, psPx = 0, psStall = 0, psScxf = 0
  , psWinX = Nothing, psWinShift = 0, psWinLine = 0, psWyTrig = False
  , psN = 0, psNext = 0, psLast = TkNone, psObjs = repeat (Obj 0 0 0 0 0 0 0)
  , psRows = 0, psTall = False }

-- * Views

-- | LY as the CPU reads it.
lyView :: PpuState -> Unsigned 8
lyView s
  | psLy s == 153 = 0
  | psDot s >= 452 = psLy s + 1
  | otherwise = psLy s

-- | STAT as the CPU reads it (bits 3 to 6 from the register).
statView :: PpuRegs -> PpuState -> BitVector 8
statView r s = 0x80 .|. (prStat r .&. 0x78) .|. (if psLycFlag s then 4 else 0)
               .|. (if psOn s then zeroExtend (pack (psMode s)) else 0)

-- | Which CPU accesses are blocked (Gb.Spec.Ppu: vramReadBlocked and co.).
data Blocked = Blocked { bkVramRd, bkVramWr, bkOamRd, bkOamWr :: Bool }
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFDataX

blocked :: Bool -> PpuState -> Blocked
blocked dma s = Blocked
  { bkVramRd = on && (md == 3 || (md == 2 && dot >= 76))
  , bkVramWr = on && md == 3
  , bkOamRd = dma || (on && (md >= 2 || (dot >= 452 && (ly < 143 || ly == 153))))
  , bkOamWr = dma || (on && (md == 3 || (md == 2 && dot < 76)))
  }
 where
  on = psOn s
  md = psMode s
  dot = psDot s
  ly = psLy s

-- * Commands

-- | The LY = LYC flag and the STAT line (only while the LCD is on).
statUpdate :: Bool -> PpuRegs -> PpuState -> (PpuState, Bool)
statUpdate cgbHw r s
  | not (psOn s) = (s, False)
  | otherwise = (s { psLycFlag = coinc, psStatLine = line }, line && not (psStatLine s))
 where
  coinc = psDot s < 452 && lyView s == unpack (prLyc r)
  st = prStat r
  md = psMode s
  -- on the Game Boy Color the line-144 mode 2 source starts one M-cycle
  -- earlier (Gb.Spec.Ppu.updateStat; Mooneye vblank_stat_intr-C)
  m2 = md == 2 || (psLy s == 144 && psDot s < 4) || (cgbHw && psLy s == 143 && psDot s >= 452)
  line = (testBit st 3 && md == 0) || (testBit st 4 && md == 1)
         || (testBit st 5 && m2) || (testBit st 6 && coinc)

-- | LCDC was written: on/off transitions (then 'statUpdate').
lcdcUpdate :: PpuRegs -> PpuState -> PpuState
lcdcUpdate r s
  | psOn s && not on = s { psOn = False, psLy = 0, psDot = 0, psMode = 0 }
  | not (psOn s) && on =
      startLine r s { psOn = True, psLy = 0, psDot = 3, psMode = 0, psFirst = True
                    , psWyTrig = False, psWinLine = 0 }
  | otherwise = s
 where
  on = testBit (prLcdc r) 7

-- | Dot 0 of a visible line.
startLine :: PpuRegs -> PpuState -> PpuState
startLine r s = s { psMode = if psFirst s then 0 else 2, psN = 0
                  , psWyTrig = psWyTrig s || psLy s == unpack (prWy r) }

-- * One dot

-- | Inputs a dot gets from VRAM: the map byte and the tile row (if it
-- outputs a pixel).
data Fetched = Fetched { fMap :: BitVector 8, fLo :: BitVector 8, fHi :: BitVector 8
                       , fAttr :: BitVector 8 -- ^ the tile's attributes (CGB mode; 0 otherwise)
                       , fObj :: (BitVector 2, BitVector 8) -- ^ the object pixel here: colour, attributes ('objHere')
                       }

-- | Window start at pixel @x@ now?
winStarts :: PpuRegs -> PpuState -> Bool
winStarts r s = psWinX s == Nothing && psWyTrig s && testBit lcdc 5 && testBit lcdc 0
                && wx < 167 && psPx s == (if wx < 7 then 0 else wx - 7)
 where
  lcdc = prLcdc r
  wx = unpack (prWx r) :: Unsigned 8

-- | Where the BG/window pixel @x@ comes from: map byte address, then (given
-- the map byte) the tile row address and the column in it.
bgSource :: PpuRegs -> PpuState -> (BitVector 16, BitVector 8 -> BitVector 8 -> (BitVector 16, Unsigned 3))
bgSource r s = (mapAddr, tileRow)
 where
  lcdc = prLcdc r
  x = psPx s
  win = case psWinX s of
    Just wx0 -> x >= wx0 && testBit lcdc 5
    Nothing -> False
  wx0' = maybe 0 id (psWinX s)
  (base, col, row)
    | win = (if testBit lcdc 6 then 0x9C00 else 0x9800, x - wx0' + zeroExtend (psWinShift s), psWinLine s)
    | otherwise = ( if testBit lcdc 3 then 0x9C00 else 0x9800
                  , (unpack (prScx r) .&. 0xF8) + zeroExtend (psScxf s) + x
                  , psLy s + unpack (prScy r) )
  mapAddr = base + pack (zeroExtend (shiftR row 3) * 32 + zeroExtend (shiftR col 3 .&. 31) :: Unsigned 16)
  -- the tile's row, flipped if its attribute's bit 6 says so (CGB mode;
  -- the attribute is 0 otherwise)
  tileRow t attr =
    let tb | testBit lcdc 4 = 0x8000 + shiftL (zeroExtend t) 4
           | otherwise = 0x9000 + pack (shiftL (signExtend (unpack t :: Signed 8)) 4 :: Signed 16)
        r0 = truncateB row :: Unsigned 3
        r' = if testBit attr 6 then 7 - r0 else r0
    in (tb + shiftL (zeroExtend (pack r')) 1, truncateB col)

-- | Colour index of column @c@ (0 = left) of a tile row.
rowColour :: BitVector 8 -> BitVector 8 -> Unsigned 3 -> BitVector 2
rowColour lo hi c = pack (testBit hi b) ++# pack (testBit lo b)
 where
  b = fromEnum (7 - c)

-- | The object pixel at @x@: colour index and attributes. The first
-- opaque one in the line's order (X, then OAM: the DMG), or with the
-- Game Boy Color's priority (@byOam@: CGB mode, OPRI 0) the opaque one
-- first in OAM.
objPixel :: Bool -> PpuState -> Unsigned 8 -> (BitVector 2, BitVector 8)
objPixel byOam s x
  | byOam = let (c, a, _) = fold best (map opaqueKey cands) in if c == 0 then (0, 0) else (c, a)
  | otherwise = foldr pick (0, 0) cands
 where
  cands = map colourOf (zip (indicesI :: Vec 10 (Index 10)) (psObjs s))
  colourOf (k, o)
    | resize k >= psN s = (0, 0, maxBound)
    | ox == 0 || x + 8 < ox || x >= ox = (0, 0, maxBound)
    | otherwise = (col, obAttr o, obIdx o)
   where
    ox = unpack (obX o) :: Unsigned 8
    c0 = truncateB (x + 8 - ox) :: Unsigned 3
    col = rowColour (obLo o) (obHi o) (if testBit (obAttr o) 5 then 7 - c0 else c0)
  pick (c, a, _) rest = if c /= 0 then (c, a) else rest
  -- the CGB's choice (lowest OAM index of the opaque ones) as a balanced
  -- tree: a transparent pixel counts as index 255, so the choice is a
  -- minimum and associative (a chain of 10 compares was too slow)
  opaqueKey (c, a, i) = (c, a, if c == 0 then maxBound else i)
  best (c, a, i) (bc, ba, bi) = if i < bi then (c, a, i) else (bc, ba, bi)

palette :: BitVector 8 -> BitVector 2 -> BitVector 2
palette p c = slice d1 d0 (shiftR p (2 * fromEnum (unpack c :: Unsigned 2)))

-- | The penalty due at the current pixel (0 if none), and the state
-- with that object marked done. Objects whose first pixel has already
-- passed (while LCDC bit 1 was clear) are skipped; the list is sorted by
-- X, so the next one that matters is the first that is off screen or not
-- passed yet.
objPenalty :: PpuRegs -> PpuState -> (Unsigned 5, PpuState)
objPenalty r s
  | not (testBit (prLcdc r) 1) || k0 >= psN s = (0, s)
  | k >= psN s = (0, s { psNext = k })
  | ox >= 168 = (0, s { psNext = psN s })
  | start > x = (0, s { psNext = k })
  | otherwise = (6 + extra, s { psNext = k + 1, psLast = key })
 where
  k0 = psNext s
  x = psPx s
  startOf ob = let v = unpack (obX ob) :: Unsigned 8 in if v < 8 then 0 else v - 8
  matters j ob = j >= k0 && j < psN s && ((unpack (obX ob) :: Unsigned 8) >= 168 || startOf ob >= x)
  k = foldr (\(j, ob) acc -> if matters j ob then j else acc) (psN s)
            (zip (map resize indicesI :: Vec 10 (Index 11)) (psObjs s))
  o = psObjs s !! k
  ox = unpack (obX o) :: Unsigned 8
  start = startOf o
  scx = unpack (prScx r) :: Unsigned 8
  (key, col)
    | ox == 0 = (TkX0, 0)
    | Just wx0 <- psWinX s, ox >= 8, ox - 8 >= wx0 =
        let c = ox - 8 - wx0 + zeroExtend (psWinShift s) in (TkWin (truncateB (shiftR c 3)), truncateB c)
    | otherwise = let c = (zeroExtend ox + zeroExtend scx) :: Unsigned 9
                  in (TkBg (truncateB (shiftR c 3)), truncateB c :: Unsigned 3)
  extra = if key /= psLast s then (if col < 5 then 5 - zeroExtend col else 0) else 0

-- | The object pixel at the current x (mode 3), worked out in the dot's
-- second clock ('ppuStep', 'MDot1') so that the third only adds the BG and
-- priority (make bit on the Arty: the whole pixel in one clock missed by
-- 0.7 ns). The state is the same in the third clock: 'storeRow' runs only
-- in mode 3's first stall, which puts out no pixel.
objHere :: PpuIn -> PpuRegs -> PpuState -> (BitVector 2, BitVector 8)
objHere inp r s
  | testBit (prLcdc r) 1 = objPixel (piCgb inp && not (piOpri inp)) s (psPx s)
  | otherwise = (0, 0)

-- | One dot (spec: dotStep), given what VRAM returned for it.
dotLogic :: PpuIn -> PpuRegs -> Vec 40 Oam -> Bool -> Fetched -> PpuState
         -> (PpuState, Bool, Maybe (Index 23040, BitVector 2), Bool, Unsigned 7)
dotLogic inp r oam dma f s0 = (s4, vbl, pix, frame, palByte)
 where
  dot0 = psDot s0
  s1 = if psMode s0 == 2 && not (testBit dot0 0) then scanEntry r oam dma (unpack (truncateB (pack (shiftR dot0 1)) :: BitVector 6)) s0 else s0
  (s2, pix, palByte) = if psMode s1 == 3 then mode3 inp r f s1 else (s1, Nothing, 0)
  d = dot0 + 1
  (s4, vbl, frame)
    | d == 456 =
        let ly' = if psLy s2 == 153 then 0 else psLy s2 + 1
            s3 = s2 { psDot = 0, psLy = ly', psFirst = False }
        in if ly' == 144
             then (s3 { psMode = 1, psWyTrig = False, psWinLine = 0 }, True, True)
             else (if ly' < 144 then startLine r s3 else s3, False, False)
    | d == 80 && psLy s2 < 144 =
        (startMode3 r s2 { psDot = d }, False, False)
    | otherwise = (s2 { psDot = d }, False, False)

scanEntry :: PpuRegs -> Vec 40 Oam -> Bool -> Index 40 -> PpuState -> PpuState
scanEntry r oam dma i s
  | dma || psN s >= 10 || not visible = s
  | otherwise = s { psObjs = objs', psN = psN s + 1 }
 where
  e = oam !! i
  h = if testBit (prLcdc r) 2 then 16 else 8 :: Unsigned 9
  y = zeroExtend (unpack (oaY e)) :: Unsigned 9
  ly16 = zeroExtend (psLy s) + 16 :: Unsigned 9
  visible = ly16 >= y && ly16 < y + h
  x = unpack (oaX e) :: Unsigned 8
  newObj = Obj (oaY e) (oaX e) (oaTile e) (oaAttr e) 0 0 (resize (pack i))
  -- insert after every entry with X <= x: entry k moves to k+1 if it is at
  -- or after the insertion point
  pos = foldr (\(k, o) p -> if resize k < psN s && (unpack (obX o) :: Unsigned 8) > x then k else p)
              (resize (psN s)) (zip (indicesI :: Vec 10 (Index 10)) (psObjs s)) -- first k with X > x
  objs' = imap (\k o -> if k < pos then o
                        else if k == pos then newObj
                        else psObjs s !! (k - 1)) (psObjs s :: Vec 10 Obj)

startMode3 :: PpuRegs -> PpuState -> PpuState
startMode3 r s = s { psMode = 3, psPx = 0, psScxf = truncateB scx, psStall = 12 + zeroExtend (truncateB scx :: Unsigned 3)
                   , psWinX = Nothing, psNext = 0, psLast = TkNone, psRows = 0
                   , psTall = testBit (prLcdc r) 2 }
 where
  scx = unpack (prScx r) :: Unsigned 8

-- | One dot of mode 3.
-- One dot of mode 3. The rules of 'Gb.Spec.Ppu.pixel': on the Game Boy
-- Color in CGB mode the BG is always drawn, with its tile's attributes
-- (palette, X flip; the Y flip and the bank were applied to the fetch),
-- and LCDC bit 0 is the master priority; the colour is the palette RAM
-- byte 'poPalByte' (CGB mode by the palettes, compatibility mode by the
-- DMG shade through BG palette 0 and OBJ palettes 0 and 1).
mode3 :: PpuIn -> PpuRegs -> Fetched -> PpuState -> (PpuState, Maybe (Index 23040, BitVector 2), Unsigned 7)
mode3 inp r f s
  | psStall s > 0 = (s { psStall = psStall s - 1 }, Nothing, 0)
  | winStarts r s =
      let wx = unpack (prWx r) :: Unsigned 8
      in (s { psWinX = Just (psPx s), psWinShift = if wx < 7 then truncateB (7 - wx) else 0, psStall = 5 }, Nothing, 0)
  | pen > 0 = (s' { psStall = pen - 1 }, Nothing, 0)
  | otherwise =
      let x' = psPx s' + 1
          s'' = s' { psPx = x' }
          done = if x' == 160 then s'' { psMode = 0, psWinLine = psWinLine s'' + (if psWinX s'' /= Nothing then 1 else 0) } else s''
      in (done, Just (idx, shade), palByte)
 where
  cgb = piCgb inp
  (pen, s') = objPenalty r s
  lcdc = prLcdc r
  x = psPx s'
  (_, tileRow) = bgSource r s'
  battr = fAttr f
  (_, col0) = tileRow (fMap f) battr
  col = if testBit battr 5 then 7 - col0 else col0
  bg = if cgb || testBit lcdc 0 then rowColour (fLo f) (fHi f) col else 0
  -- the object pixel, found a clock earlier ('objHere': objPenalty
  -- changes neither the objects nor x)
  (oc, attr) = fObj f
  objWins
    | cgb = oc /= 0 && (not (testBit lcdc 0) || bg == 0 || not (testBit battr 7 || testBit attr 7))
    | otherwise = oc /= 0 && not (testBit attr 7 && bg /= 0)
  shade | objWins = palette (if testBit attr 4 then prObp1 r else prObp0 r) oc
        | otherwise = palette (prBgp r) bg
  palByte :: Unsigned 7
  palByte
    | cgb && objWins = 64 + 8 * unpack (resize (slice d2 d0 attr)) + 2 * unpack (zeroExtend oc)
    | cgb = 8 * unpack (resize (slice d2 d0 battr)) + 2 * unpack (zeroExtend bg)
    | objWins = 64 + (if testBit attr 4 then 8 else 0) + 2 * unpack (zeroExtend shade)
    | otherwise = 2 * unpack (zeroExtend shade)
  idx = unpack (truncateB (pack (zeroExtend (psLy s') * 160 + zeroExtend x :: Unsigned 16)) :: BitVector 15)

-- * The clocked PPU

data Micro = MIdle | MDot1 | MDot2 (BitVector 8) (BitVector 8) (BitVector 2, BitVector 8)
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFDataX

pairOf :: BitVector 16 -> Unsigned 12
pairOf a = unpack (slice d12 d1 a)

-- | The object row address of line object @k@ (with the size latched at
-- mode 3).
objRowAddr :: PpuState -> Index 10 -> BitVector 16
objRowAddr s k = 0x8000 + shiftL (zeroExtend tile) 4 + shiftL (zeroExtend (pack (truncateB r :: Unsigned 3))) 1
 where
  o = psObjs s !! k
  h = if psTall s then 16 else 8 :: Unsigned 8
  r0 = (psLy s + 16 - unpack (obY o)) .&. (h - 1)
  r = if testBit (obAttr o) 6 then h - 1 - r0 else r0
  t = obTile o
  tile = if psTall s then (t .&. 0xFE) + pack (shiftR r 3) else t

-- | One clock: (state, micro step), input, output. A dot takes three
-- clocks: 'MIdle' puts out the first VRAM address, 'MDot1' gets its data
-- and puts out the second, 'MDot2' gets that and runs 'dotLogic'. In the
-- first dots of mode 3 the two reads fetch line objects' tile rows
-- instead (those dots are stalls, so they need no BG data).
{-# OPAQUE ppuStep #-}
ppuStep :: (PpuState, Micro) -> PpuIn -> ((PpuState, Micro), PpuOut)
ppuStep (s, mi) inp = case (mi, piCmd inp) of
  (MIdle, Just CmdStat) ->
    let (s', irq) = statUpdate (piCgbHw inp) r s in ((s', MIdle), o0 s' False irq Nothing False)
  (MIdle, Just CmdLcdc) ->
    let (s', irq) = statUpdate (piCgbHw inp) r (lcdcUpdate r s) in ((s', MIdle), o0 s' False irq Nothing False)
  (MIdle, Just CmdDot)
    | not (psOn s) -> ((s, MIdle), o0 s False False Nothing False)
    | rowsDue s -> ((s, MDot1), busy s (pairOf (objRowAddr s (rowK s))))
    | otherwise -> ((s, MDot1), busy s (pairOf mapA))
  (MDot1, _)
    | rowsDue s ->
        let s' = storeRow s
        -- (no object pixel: these dots are mode 3's first stall)
        in ((s', MDot2 0 0 (0, 0)), busy s' (if rowsDue s' then pairOf (objRowAddr s' (rowK s')) else 0))
    | otherwise ->
        let byteOf v = if testBit mapA 0 then slice d15 d8 v else slice d7 d0 v
            mb = byteOf (piVram inp)
            -- the tile's attributes: the same place in bank 1 (CGB mode)
            ab = if piCgb inp then byteOf (piVram1 inp) else 0
            (ta, _) = tileRowOf mb ab
        in ((s, MDot2 mb ab (objHere inp r s)), busy s (pairOf ta))
  (MDot2 mb ab op, _) ->
    let s1 = if rowsDue s then storeRow s else s
        -- the tile row from bank 1 if the attribute's bit 3 says so
        v = if testBit ab 3 then piVram1 inp else piVram inp
        fetched = Fetched mb (slice d7 d0 v) (slice d15 d8 v) ab op
        (s', vbl, pix, frame, pb) = dotLogic inp r (piOam inp) (piDma inp) fetched s1
    in ((s', MIdle), (o0 s' vbl False pix frame) { poPalByte = pb })
  (MIdle, Nothing) -> ((s, MIdle), o0 s False False Nothing False)
 where
  r = piRegs inp
  (mapA, tileRowOf) = bgSource r s
  rowsDue st = psMode st == 3 && psRows st < psN st
  rowK st = resize (psRows st) :: Index 10
  storeRow st =
    let k = rowK st
        o = psObjs st !! k
        -- the object's tile from bank 1 if its attribute's bit 3 says so (CGB mode)
        v = if piCgb inp && testBit (obAttr o) 3 then piVram1 inp else piVram inp
    in st { psObjs = replace k o { obLo = slice d7 d0 v, obHi = slice d15 d8 v } (psObjs st)
          , psRows = psRows st + 1 }
  o0 st vbl irq pix frame = PpuOut 0 False st vbl irq pix frame 0
  busy st a = (o0 st False False Nothing False) { poVramAddr = a, poBusy = True }

-- | The PPU as a Mealy machine.
ppu :: HiddenClockResetEnable dom => Signal dom PpuIn -> Signal dom PpuOut
ppu = mealy ppuStep (ppuInit, MIdle)
