{-# OPTIONS_GHC -fconstraint-solver-iterations=0 #-}
{-# LANGUAGE StrictData #-}
{-|
The PPU in Clash (PLAN.md step 7.6): "Snes.Spec.Ppu" and the PPU's half of
"Snes.Spec.Machine" (the registers at 2100h-213Fh, the H/V counters, the
memory ports), with the spec's rules: the lockstep tests (@snes-lockstep@)
replay a program's PPU accesses into both and compare the pictures and
the values read back.

__Time:__ 'piTick' is a master clock (at most one a clock). At a tick, in
the spec's order: H moves on; a visible line's dot d starts when H
reaches 4 (d + 1) (its fetch slot and its pixel, from the registers as
they are then); at H = 1120 the next line's sprites start; at the line's
end, the next line; then the tick's B-bus access ('piAccess'). An access
may also come in a clock without a tick, after its master clock's tick
and before the next: it happens at that master clock all the same (the
machine gives a DMA byte from the A bus a clock to read it, step 7.9).

__A dot is four clocks__ (more just idle). Sub 0, the tick: the registers
the pixel needs are copied ('Snap'), the fetch slot's VRAM word (or Mode
7's map byte), the pixel's sprite entry and its columns in the ring are
asked for. Sub 1: the word into its column (the ring: 8 columns a layer,
the spec's 33 but only those still in use); Mode 7's char byte asked
for. Sub 2: the BG pixels, priorities and windows give the main screen's
source; its CGRAM read. Sub 3: the same for the sub screen (one
'screen' for both). Sub 4: colour maths and brightness; the pixel comes
out on the next clock ('poPixel').

__Sprites__ ('SprPhase'): from H = 1120 of the line before (not in forced
blank: then only the clear), the 128 entries scanned one a clock while
the line buffer is cleared, then up to 34 slivers of two VRAM words, each
written 8 pixels at once into the line buffer's 8 banks (x mod 8). About
200 clocks, done before the next line's first fetch (H = 28).

__Memories__ are outside, block RAM ('PpuIn', 'PpuOut'): VRAM as two
byte-wide chips (port A the PPU's, port B the CPU's), CGRAM (256 15-bit
words; A the pixels', B the CPU's), OAM's low table as four byte lanes (A
the sprite unit's, B the CPU's; the high table is registers), the line
buffer's 8 banks, and the ring: each layer's map entries (8 × 16 bits),
its char words (4 lanes of 8 × 16 bits), and the char headers (32 ×
23 bits; small, so LUT RAM). Data comes back on the next clock, and so does a
register read's value ('poRead').
-}
module Snes.Ppu
  ( Access (..)
  , PpuIn (..)
  , PpuOut (..)
  , PpuC (..)
  , SprPhase (..)
  , SprEntry (..)
  , RingH (..)
  , Snap (..)
  , Pipe (..)
  , Src (..)
  , Fetch (..)
  , Kind (..)
  , ReadPend (..)
  , ppuInit
  , ppuClock
  , ppuIdleIn
  , ppuRead
  ) where

import Clash.Prelude

-- * Interface

-- | A B-bus access to 2100h-213Fh (the offset), with the CPU's open bus.
data Access
  = AWrite (Unsigned 6) (Unsigned 8)
  | ARead (Unsigned 6) (Unsigned 8)
  | ALatch                          -- ^ WRIO bit 7 falling: latch the counters
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFDataX, BitPack)

data PpuIn = PpuIn
  { piTick   :: Bool
  , piAccess :: Maybe Access          -- ^ at the last tick's master clock
  , piVramA  :: Vec 2 (Unsigned 8)    -- ^ port A: low and high chip, at last clock's addresses
  , piVramB  :: Vec 2 (Unsigned 8)    -- ^ port B: the word at last clock's address
  , piCgramA :: Unsigned 15
  , piCgramB :: Unsigned 15
  , piOamA   :: Vec 4 (Unsigned 8)    -- ^ OAM low table, port A: an entry's 4 bytes
  , piOamB   :: Vec 4 (Unsigned 8)
  , piObj    :: Vec 8 (Unsigned 9)    -- ^ the line buffer's 8 banks
  , piRingE  :: Vec 4 (Unsigned 16)   -- ^ the ring: each layer's map entry at 'poRingA'
  , piRingW  :: Vec 4 (Vec 4 (Unsigned 16)) -- ^ each layer's char words at 'poRingA'
  , piRingH  :: RingH                 -- ^ the char header at 'poRingH'
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (NFDataX, BitPack)

ppuIdleIn :: PpuIn
ppuIdleIn = PpuIn False Nothing (repeat 0) (repeat 0) 0 0 (repeat 0) (repeat 0) (repeat 0)
                  (repeat 0) (repeat (repeat 0)) (RingH 0 0 0)

data PpuOut = PpuOut
  { poVramA    :: Vec 2 (Unsigned 15)                         -- ^ port A addresses (low chip, high chip)
  , poVramB    :: Unsigned 15                                 -- ^ port B address (read or write)
  , poVramW    :: Maybe (Bool, Unsigned 8)                    -- ^ port B write: high chip?, byte
  , poCgramA   :: Unsigned 8
  , poCgramB   :: Unsigned 8
  , poCgramW   :: Maybe (Unsigned 15)                         -- ^ port B write at 'poCgramB'
  , poOamA     :: Unsigned 7
  , poOamB     :: Unsigned 7
  , poOamW     :: Vec 4 (Maybe (Unsigned 8))                  -- ^ port B lane writes at 'poOamB'
  , poObjA     :: Unsigned 5
  , poObjW     :: Vec 8 (Maybe (Unsigned 5, Unsigned 9))
  , poRingA    :: Vec 4 (Unsigned 3)                          -- ^ the ring: each layer's column to read
  , poRingH    :: Unsigned 5                                  -- ^ the char header to read (layer * 8 + column)
  , poRingWE   :: Maybe (Index 4, Unsigned 3, Unsigned 16)     -- ^ a map entry written: layer, column
  , poRingWH   :: Maybe (Unsigned 5, RingH)                   -- ^ a char header written
  , poRingWW   :: Maybe (Index 4, Index 4, Unsigned 3, Unsigned 16) -- ^ a char word written: layer, word, column
  , poPixel    :: Maybe (Unsigned 8, Unsigned 8, Unsigned 15, Unsigned 15) -- ^ x, row, left, right
  , poRead     :: Maybe (Maybe (Unsigned 8))                  -- ^ a read's value (Nothing inside: open bus)
  , poHc       :: Unsigned 11
  , poVc       :: Unsigned 9
  , poField    :: Bool
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (NFDataX, BitPack)

-- * State

-- | A column's char header, from its map entry: the left and right
-- half's chars (hi-res; the same otherwise), the row in the tile.
data RingH = RingH
  { rhChL :: Unsigned 10
  , rhChR :: Unsigned 10
  , rhRow :: Unsigned 3
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (NFDataX, BitPack)

data Snap = Snap
  { sInidisp, sBgmode, sMosaic, sSetini :: Unsigned 8
  , sTm, sTs, sTmw, sTsw :: Unsigned 8
  , sWsel    :: Vec 3 (Unsigned 8)
  , sWpos    :: Vec 4 (Unsigned 8)
  , sWlog    :: Vec 2 (Unsigned 8)
  , sCgwsel, sCgadsub :: Unsigned 8
  , sFixed   :: Unsigned 15
  , sX       :: Unsigned 8
  , sRow     :: Unsigned 8
  , sFine    :: Vec 4 (Unsigned 3)
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (NFDataX, BitPack)

data Kind = KNone | KOpt (Index 2) | KMap (Index 4) | KChar (Index 4) (Index 4)
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFDataX, BitPack)

-- | What the fetch slot's word needs at sub 1 (worked out at sub 0).
data Fetch = Fetch
  { fKind  :: Kind
  , fCol   :: Unsigned 6
  , fY     :: Unsigned 10   -- ^ map: the BG line
  , fBig   :: Bool          -- ^ map: 16-line tiles
  , fOdd   :: Bool          -- ^ map: the 8-pixel column's half of a 16-wide tile
  , fWide  :: Bool          -- ^ map: 16-wide tiles (not hi-res)
  , fHires :: Bool
  , fFine  :: Unsigned 3    -- ^ map, column 0: the layer's fine scroll
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (NFDataX, BitPack)

-- | A screen's colour source: a CGRAM index or a colour; its layer (0-3
-- BG, 4 OBJ, 5 backdrop); an OBJ palette's colour maths bit.
data Src = Src
  { srCg      :: Maybe (Unsigned 8)
  , srColor   :: Unsigned 15
  , srLayer   :: Unsigned 3
  , srObjMath :: Bool
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (NFDataX, BitPack)

src0 :: Src
src0 = Src Nothing 0 5 False

data Pipe = Pipe
  { ppSub    :: Index 5
  , ppSnap   :: Snap
  , ppPixel  :: Bool
  , ppFetch  :: Maybe Fetch
  , ppObj    :: Unsigned 9
  , ppM7     :: Maybe (Bool, Unsigned 3, Unsigned 3, Bool) -- ^ tile 0?, row, column, transparent
  , ppM7Byte :: Unsigned 8
  , ppMain   :: Src
  , ppSubS   :: Src
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (NFDataX, BitPack)

data SprEntry = SprEntry
  { seX    :: Signed 10
  , seY    :: Unsigned 8
  , seTile :: Unsigned 9
  , seAttr :: Unsigned 8
  , seW    :: Unsigned 7
  , seH    :: Unsigned 7
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (NFDataX, BitPack)

data SprPhase
  = SpIdle
  | SpScan (Unsigned 8)                          -- ^ entries asked for so far
  | SpFind (Index 32) (Unsigned 6)
    -- ^ the found entry (counting down) to look at, slivers so far
  | SpSliv (Index 32) (Unsigned 4) (Unsigned 6) (Index 3)
    -- ^ the found entry, its column, slivers so far, the clock
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFDataX, BitPack)

data ReadPend
  = RNone
  | RNow (Maybe (Unsigned 8))   -- ^ the value is known (Nothing: open bus)
  | RLane (Index 4)             -- ^ OAM port B's lane
  | RCg Bool                    -- ^ CGRAM port B: the high byte?
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFDataX, BitPack)

data PpuC = PpuC
  { pRegs     :: Vec 64 (Unsigned 8)
  , pHofs     :: Vec 4 (Unsigned 10)
  , pVofs     :: Vec 4 (Unsigned 10)
  , pM7       :: Vec 6 (Unsigned 16)
  , pM7Ofs    :: Vec 2 (Unsigned 16)
  , pFixed    :: Unsigned 15
  , pBgPrev   :: Unsigned 8
  , pBgPrev2  :: Unsigned 8          -- ^ the HOFS writes' own byte latch (the spec's)
  , pM7Prev   :: Unsigned 8
  , pHc       :: Unsigned 11
  , pVc       :: Unsigned 9
  , pField    :: Bool
  , pVaddr    :: Unsigned 15
  , pVlatch   :: Unsigned 16
  , pVfill    :: Bool
  , pCgadd    :: Unsigned 8
  , pCgflip   :: Bool
  , pCglatch  :: Unsigned 8
  , pOamadd   :: Unsigned 10
  , pOamrel   :: Unsigned 10
  , pOamlatch :: Unsigned 8
  , pOamHigh  :: Vec 32 (Unsigned 8)
  , pHlatch   :: Unsigned 9
  , pVlatch2  :: Unsigned 9
  , pHflip    :: Bool
  , pVflip    :: Bool
  , pLatched  :: Bool
  , pStat77   :: Unsigned 8
  , pOpt      :: Vec 8 (Vec 2 (Unsigned 16))
  , pFine     :: Vec 4 (Unsigned 3)
  , pPipe     :: Pipe
  , pRead     :: ReadPend
  , pFinish   :: Maybe (Unsigned 8, Unsigned 8, Unsigned 15, Unsigned 15)
  , pSpr      :: SprPhase
  , pSprObsel :: Unsigned 8
  , pSprRow   :: Unsigned 8
  , pSprFirst :: Unsigned 7
  , pFound    :: Vec 32 SprEntry
  , pNFound   :: Unsigned 6
  , pSprWord  :: Unsigned 16
  , pClear    :: Unsigned 6
  , pSprBusy  :: Bool          -- ^ from H = 1120 to the line's end once the sprite unit started: VRAM and OAM take no writes
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (NFDataX, BitPack)

ppuInit :: PpuC
ppuInit = PpuC
  { pRegs = repeat 0, pHofs = repeat 0, pVofs = repeat 0, pM7 = repeat 0, pM7Ofs = repeat 0
  , pFixed = 0, pBgPrev = 0, pBgPrev2 = 0, pM7Prev = 0, pHc = 0, pVc = 0, pField = False
  , pVaddr = 0, pVlatch = 0, pVfill = False, pCgadd = 0, pCgflip = False, pCglatch = 0
  , pOamadd = 0, pOamrel = 0, pOamlatch = 0, pOamHigh = repeat 0
  , pHlatch = 0, pVlatch2 = 0, pHflip = False, pVflip = False, pLatched = False, pStat77 = 0
  , pOpt = repeat (repeat 0), pFine = repeat 0
  , pPipe = Pipe 0 snap0 False Nothing 0 Nothing 0 src0 src0
  , pRead = RNone, pFinish = Nothing
  , pSpr = SpIdle, pSprObsel = 0, pSprRow = 0, pSprFirst = 0
  , pFound = repeat (SprEntry 0 0 0 0 8 8), pNFound = 0, pSprWord = 0, pClear = 32, pSprBusy = False
  }
 where
  snap0 = Snap 0 0 0 0 0 0 0 0 (repeat 0) (repeat 0) (repeat 0) 0 0 0 0 0 (repeat 0)

-- * The registers' rules

reg :: PpuC -> Int -> Unsigned 8
reg c i = pRegs c !! i

vblankStart :: PpuC -> Unsigned 9
vblankStart c = if testBit (reg c 0x33) 2 then 240 else 225

lineLength :: PpuC -> Unsigned 11
lineLength c = if pVc c == 240 && pField c && not (testBit (reg c 0x33) 0) then 1360 else 1364

linesPerFrame :: PpuC -> Unsigned 9
linesPerFrame c = if testBit (reg c 0x33) 0 && not (pField c) then 263 else 262

forcedBlank :: PpuC -> Bool
forcedBlank c = testBit (reg c 0x00) 7

visibleLine :: PpuC -> Bool
visibleLine c = pVc c >= 1 && pVc c < vblankStart c

vramStep :: Unsigned 8 -> Unsigned 15
vramStep vm = case vm .&. 3 of 0 -> 1; 1 -> 32; _ -> 128

vramRemap :: Unsigned 8 -> Unsigned 15 -> Unsigned 15
vramRemap vm a = case (vm `shiftR` 2) .&. 3 of
  0 -> a
  1 -> (a .&. 0x7F00) .|. ((a .&. 0x1F) `shiftL` 3) .|. ((a `shiftR` 5) .&. 7)
  2 -> (a .&. 0x7E00) .|. ((a .&. 0x3F) `shiftL` 3) .|. ((a `shiftR` 6) .&. 7)
  _ -> (a .&. 0x7C00) .|. ((a .&. 0x7F) `shiftL` 3) .|. ((a `shiftR` 7) .&. 7)

hdot :: Unsigned 11 -> Unsigned 9
hdot hc
  | hc < 1292 = resize (hc `shiftR` 2)
  | hc < 1298 = 323
  | hc < 1310 = 324 + resize ((hc - 1298) `shiftR` 2)
  | hc < 1316 = 327
  | otherwise = 328 + resize ((hc - 1316) `shiftR` 2)

mapAddr :: Unsigned 8 -> Unsigned 10 -> Unsigned 10 -> Unsigned 15
mapAddr sc tx0 ty0 =
  let wide = testBit sc 0
      tall = testBit sc 1
      tx = tx0 .&. (if wide then 63 else 31)
      ty = ty0 .&. (if tall then 63 else 31)
      base = resize (sc `shiftR` 2) `shiftL` 10 :: Unsigned 15
  in base + resize (ty .&. 31) * 32 + resize (tx .&. 31)
     + (if testBit tx 5 then 0x400 else 0)
     + (if testBit ty 5 then (if wide then 0x800 else 0x400) else 0)

slotKind :: Unsigned 3 -> Unsigned 3 -> Kind
slotKind mode s = case mode of
  0 -> tbl (KMap 0) (KMap 1) (KMap 2) (KMap 3) (KChar 0 0) (KChar 1 0) (KChar 2 0) (KChar 3 0)
  1 -> tbl (KMap 0) (KMap 1) (KMap 2) (KChar 0 0) (KChar 0 1) (KChar 1 0) (KChar 1 1) (KChar 2 0)
  2 -> tbl (KOpt 0) (KOpt 1) (KMap 0) (KMap 1) (KChar 0 0) (KChar 0 1) (KChar 1 0) (KChar 1 1)
  3 -> tbl (KMap 0) (KMap 1) (KChar 0 0) (KChar 0 1) (KChar 0 2) (KChar 0 3) (KChar 1 0) (KChar 1 1)
  4 -> tbl (KOpt 0) (KMap 0) (KMap 1) (KChar 0 0) (KChar 0 1) (KChar 0 2) (KChar 0 3) (KChar 1 0)
  5 -> tbl (KMap 0) (KMap 1) (KChar 0 0) (KChar 0 1) (KChar 0 2) (KChar 0 3) (KChar 1 0) (KChar 1 1)
  6 -> tbl (KOpt 0) (KOpt 1) (KMap 0) (KChar 0 0) (KChar 0 1) (KChar 0 2) (KChar 0 3) KNone
  _ -> KNone
 where
  tbl a b c d e f g h = (a :> b :> c :> d :> e :> f :> g :> h :> Nil) !! s

bppOf :: Unsigned 3 -> Index 4 -> Unsigned 4
bppOf mode n = case mode of
  0 -> 2
  1 -> if n == 2 then 2 else 4
  2 -> 4
  3 -> if n == 0 then 8 else 4
  4 -> if n == 0 then 8 else 2
  5 -> if n == 0 then 4 else 2
  _ -> 4

-- | The fetch slot of column j, slot s, line V: its word address and what
-- sub 1 needs. A char slot's header is the ring's, read the clock before
-- ('ringHAddr').
fetchAddr :: PpuC -> RingH -> Unsigned 9 -> Unsigned 6 -> Unsigned 3 -> (Unsigned 15, Fetch)
fetchAddr c hd v j s =
  let bgmode = reg c 0x05
      mode = resize bgmode :: Unsigned 3
      hiresMode = mode == 5 || mode == 6
      k = slotKind mode s
      jr = resize j :: Unsigned 3
      none = Fetch k j 0 False False False hiresMode 0
  in case k of
       KNone -> (0, none)
       KOpt e ->
         let h3 = pHofs c !! (2 :: Int)
             v3 = pVofs c !! (2 :: Int)
         in (mapAddr (reg c 0x09) (resize j - 1 + (h3 `shiftR` 3)) ((v3 `shiftR` 3) + (if e == 1 then 1 else 0)), none)
       KMap n ->
         let mosaic = reg c 0x06
             setini = reg c 0x33
             hofs = pHofs c !! n
             vofs = pVofs c !! n
             eh = (pOpt c !! jr) !! (0 :: Int)
             ev = (pOpt c !! jr) !! (1 :: Int)
             row = resize (v - 1) :: Unsigned 10
             msize = resize (mosaic `shiftR` 4) + 1 :: Unsigned 10
             srow0 = if testBit mosaic (fromEnum n) then row - row `mod` msize else row
             srow = if testBit setini 0 && hiresMode then srow0 * 2 + (if pField c then 1 else 0) else srow0
             opt = (mode == 2 || mode == 4 || mode == 6) && n < 2 && j >= 1
             apply e = testBit e (13 + fromEnum n)
             optH e = resize ((e .&. 0x3F8) `shiftR` 3) :: Unsigned 10
             (coarse, vofs')
               | opt && mode == 4 = if apply eh
                   then (if testBit eh 15 then (hofs `shiftR` 3, resize eh .&. 0x3FF) else (optH eh, vofs))
                   else (hofs `shiftR` 3, vofs)
               | opt = (if apply eh then optH eh else hofs `shiftR` 3, if apply ev then resize ev .&. 0x3FF else vofs)
               | otherwise = (hofs `shiftR` 3, vofs)
             big = testBit bgmode (4 + fromEnum n)
             wide = big && not hiresMode
             tcol = coarse + resize j
             tx = if wide then tcol `shiftR` 1 else tcol
             y = srow + vofs'
             ty = if big then y `shiftR` 4 else y `shiftR` 3
         in (mapAddr (reg c (0x07 + fromEnum n)) tx ty, Fetch k j y big (testBit tcol 0) wide hiresMode (resize hofs))
       KChar n w ->
         let nba = reg c (if n < 2 then 0x0B else 0x0C)
             bpp = bppOf mode n
             half = bpp `shiftR` 1
             wi = fromIntegral w :: Unsigned 4
             (ch, plane) = if hiresMode then (if wi < half then (rhChL hd, wi) else (rhChR hd, wi - half)) else (rhChL hd, wi)
             base = resize ((nba `shiftR` (4 * (fromEnum n .&. 1))) .&. 0xF) `shiftL` 12 :: Unsigned 15
         in (base + resize ch * resize bpp * 4 + resize (rhRow hd) + resize plane * 8, none)

-- | The ring's writes of a fetch slot's word (sub 1): a map entry, its
-- char header; a char word.
data RingWrite = RingWrite
  { rwE :: Maybe (Index 4, Unsigned 3, Unsigned 16)
  , rwH :: Maybe (Unsigned 5, RingH)
  , rwW :: Maybe (Index 4, Index 4, Unsigned 3, Unsigned 16)
  }

noRingWrite :: RingWrite
noRingWrite = RingWrite Nothing Nothing Nothing

-- | The fetch slot's word into its column (sub 1): the offset-per-tile
-- entries and the fine scroll are registers, the rest goes to the ring.
fetchStore :: PpuC -> Fetch -> Unsigned 16 -> (PpuC, RingWrite)
fetchStore c f w =
  let j = fCol f
      jr = resize j :: Unsigned 3
  in case fKind f of
       KNone -> (c, noRingWrite)
       KOpt e -> (c { pOpt = replace jr (replace e (if j == 0 then 0 else w) (pOpt c !! jr)) (pOpt c) }, noRingWrite)
       KMap n ->
         let th = if fBig f then 16 else 8 :: Unsigned 10
             py0 = fY f `mod` th
             py = if testBit w 15 then th - 1 - py0 else py0
             hflip = testBit w 14
             ch0 = resize (w .&. 0x3FF) + (if py >= 8 then 16 else 0) :: Unsigned 10
             (chL, chR)
               | fHires f = (ch0 + (if hflip then 1 else 0), ch0 + (if hflip then 0 else 1))
               | fWide f = let half = (if fOdd f then 1 else 0) `xor` (if hflip then 1 else 0) in (ch0 + half, ch0 + half)
               | otherwise = (ch0, ch0)
             fine' = if j == 0 then replace n (fFine f) (pFine c) else pFine c
         in ( c { pFine = fine' }
            , RingWrite (Just (n, jr, w)) (Just (ringIx n jr, RingH chL chR (resize py))) Nothing )
       KChar n wi -> (c, RingWrite Nothing Nothing (Just (n, wi, jr, w)))

ringIx :: Index 4 -> Unsigned 3 -> Unsigned 5
ringIx n j = (fromIntegral n `shiftL` 3) .|. resize j

-- | Layer n's position in its line (x after mosaic, plus the fine
-- scroll): its column in the ring is bits 3-5.
bgPos :: Snap -> Index 4 -> Unsigned 9
bgPos sn n =
  let x = sX sn
      mosaic = sMosaic sn
      msize = resize (mosaic `shiftR` 4) + 1 :: Unsigned 9
      sx = if testBit mosaic (fromEnum n) then resize x - (resize x `mod` msize) else resize x
  in sx + resize (sFine sn !! n)

-- | The ring's columns the pixel reads, one a layer.
ringAddrs :: Snap -> Vec 4 (Unsigned 3)
ringAddrs sn = map (\n -> resize (bgPos sn n `shiftR` 3)) indicesI

-- | The char header the next dot's fetch slot reads, from H the clock
-- before its tick (the registers then are the tick's).
ringHAddr :: PpuC -> Unsigned 5
ringHAddr c =
  let d = resize ((pHc c + 1) `shiftR` 2) - 7 :: Unsigned 9
      mode = resize (reg c 0x05) :: Unsigned 3
      n = case slotKind mode (resize d) of
        KChar k _ -> k
        _ -> 0
  in ringIx n (resize (d `shiftR` 3))

-- | The ring's column a layer's pixel reads ('ringAddrs'): its map entry
-- and char words.
type RingCol = (Unsigned 16, Vec 4 (Unsigned 16))

-- | A BG pixel (index, palette, priority) of the snapshot's x, from its
-- column.
bgPixel :: Vec 4 RingCol -> Unsigned 3 -> Snap -> Index 4 -> Bool -> (Unsigned 8, Unsigned 3, Bool)
bgPixel ring mode sn n right =
  let px0 = resize (bgPos sn n .&. 7) :: Unsigned 4
      hiresMode = mode == 5 || mode == 6
      bpp = bppOf mode n
      (e, ws) = ring !! n
      hp = 2 * px0 + (if right then 1 else 0)
      base = if hiresMode && testBit hp 3 then bpp `shiftR` 1 else 0
      px = if hiresMode then hp .&. 7 else px0
      b = if testBit e 14 then px else 7 - px
      bitAt w k = if testBit w (fromEnum k) then 1 else 0 :: Unsigned 8
      plane (i :: Unsigned 4) = let w = ws !! (fromIntegral (base + i) :: Index 4)
                in bitAt w b .|. (bitAt w (b + 8) `shiftL` 1)
      idx = case bpp of
        2 -> plane 0
        4 -> plane 0 .|. (plane 1 `shiftL` 2)
        _ -> plane 0 .|. (plane 1 `shiftL` 2) .|. (plane 2 `shiftL` 4) .|. (plane 3 `shiftL` 6)
  in (idx, resize (e `shiftR` 10), testBit e 13)

directColor :: Unsigned 8 -> Unsigned 3 -> Unsigned 15
directColor col pal =
  let r = (resize (col .&. 7) `shiftL` 2) .|. (resize (pal .&. 1) `shiftL` 1)
      g = (resize ((col `shiftR` 3) .&. 7) `shiftL` 2) .|. (resize ((pal `shiftR` 1) .&. 1) `shiftL` 1)
      b = (resize ((col `shiftR` 6) .&. 3) `shiftL` 3) .|. (resize ((pal `shiftR` 2) .&. 1) `shiftL` 2)
  in r .|. (g `shiftL` 5) .|. (b `shiftL` 10)

windowAt :: Snap -> Index 6 -> Bool
windowAt sn l =
  let x = sX sn
      sel = sWsel sn !! (fromEnum l `div` 2)
      logic = sWlog sn !! (if l < 4 then 0 else 1 :: Int)
      l1 = sWpos sn !! (0 :: Int)
      r1 = sWpos sn !! (1 :: Int)
      l2 = sWpos sn !! (2 :: Int)
      r2 = sWpos sn !! (3 :: Int)
      bits = (sel `shiftR` (4 * (fromEnum l .&. 1))) .&. 0xF
      in1 = x >= l1 && x <= r1
      in2 = x >= l2 && x <= r2
      -- each window's two bits: enable (the higher), invert (the lower);
      -- fullsnes's "0..1=Disable, 1=Inside, 2=Outside" is a typo for 2 and
      -- 3 (bsnes, anomie); read as bit 0 enable, a game's color window (a
      -- text box's black, step 7.12) was off
      w1 = if testBit bits 1 then Just (in1 /= testBit bits 0) else Nothing
      w2 = if testBit bits 3 then Just (in2 /= testBit bits 2) else Nothing
      lg = (logic `shiftR` (2 * (fromEnum l .&. 3))) .&. 3
  in case (w1, w2) of
       (Just a, Just b) -> case lg of
         0 -> a || b
         1 -> a && b
         2 -> a /= b
         _ -> a == b
       (Just a, Nothing) -> a
       (Nothing, Just b) -> b
       _ -> False

-- | One screen's front pixel; @half@ is the hi-res half-dot (False: the
-- sub screen's left, True: the main screen's right).
screen :: Vec 4 RingCol -> Snap -> Unsigned 9 -> Unsigned 8 -> Bool -> Maybe Bool -> Src
screen ring sn objW m7 isMain half =
  let bgmode = sBgmode sn
      mode = resize bgmode :: Unsigned 3
      tm = if isMain then sTm sn else sTs sn
      tw = if isMain then sTmw sn else sTsw sn
      on :: Int -> Bool
      on l = testBit tm l && not (testBit tw l && windowAt sn (fromIntegral l))
      subOff = not isMain && half == Nothing && not (testBit (sCgwsel sn) 1)
      objPri = resize (objW `shiftR` 7) :: Unsigned 2
      objOn = on 4 && objW /= 0 && not subOff
      direct = testBit (sCgwsel sn) 0
      bg :: Index 4 -> (Bool, Bool, Src)
      bg n =
        let (idx, pal, pri) = bgPixel ring mode sn n (half == Just True)
            bpp = bppOf mode n
            cg :: Unsigned 8
            cg = case bpp of
              2 -> (if mode == 0 then fromIntegral n * 32 else 0) + resize pal * 4 + idx
              4 -> resize pal * 16 + idx
              _ -> idx
            s = if bpp == 8 && direct then Src Nothing (directColor idx pal) (fromIntegral n) False
                else Src (Just cg) 0 (fromIntegral n) False
        in (idx /= 0 && on (fromEnum n) && not subOff, pri, s)
      m7s = (m7 /= 0 && on 0 && not subOff, False, if direct then Src Nothing (directColor m7 0) 0 False else Src (Just m7) 0 0 False)
      ext = (m7 .&. 0x7F /= 0 && on 1 && not subOff, testBit m7 7, Src (Just (m7 .&. 0x7F)) 0 1 False)
      bgAt :: Index 4 -> Bool -> Maybe Src
      bgAt n hi = let (vis, pri, s) = if mode == 7 then (if n == 0 then m7s else ext) else bg n
                  in if vis && pri == hi then Just s else Nothing
      obj :: Unsigned 2 -> Maybe Src
      obj k = if objOn && objPri == k
        then let idx = objW .&. 0xF
                 pal = (objW `shiftR` 4) .&. 7
             in Just (Src (Just (resize (128 + pal * 16 + idx))) 0 4 (pal >= 4))
        else Nothing
      first = foldr (\a b -> case a of Just _ -> a; Nothing -> b) Nothing
      found = case mode of
        0 -> first (obj 3 :> bgAt 0 True :> bgAt 1 True :> obj 2 :> bgAt 0 False :> bgAt 1 False :> obj 1 :> bgAt 2 True :> bgAt 3 True :> obj 0 :> bgAt 2 False :> bgAt 3 False :> Nil)
        1 | testBit bgmode 3 -> first (bgAt 2 True :> obj 3 :> bgAt 0 True :> bgAt 1 True :> obj 2 :> bgAt 0 False :> bgAt 1 False :> obj 1 :> obj 0 :> bgAt 2 False :> Nil)
          | otherwise -> first (obj 3 :> bgAt 0 True :> bgAt 1 True :> obj 2 :> bgAt 0 False :> bgAt 1 False :> obj 1 :> bgAt 2 True :> obj 0 :> bgAt 2 False :> Nil)
        6 -> first (obj 3 :> bgAt 0 True :> obj 2 :> obj 1 :> bgAt 0 False :> obj 0 :> Nil)
        7 | testBit (sSetini sn) 6 -> first (obj 3 :> obj 2 :> bgAt 1 True :> obj 1 :> bgAt 0 False :> obj 0 :> bgAt 1 False :> Nil)
          | otherwise -> first (obj 3 :> obj 2 :> obj 1 :> bgAt 0 False :> obj 0 :> Nil)
        _ -> first (obj 3 :> bgAt 0 True :> obj 2 :> bgAt 1 True :> obj 1 :> bgAt 0 False :> obj 0 :> bgAt 1 False :> Nil)
      backdrop = if isMain || half /= Nothing then Src (Just 0) 0 5 False else Src Nothing (sFixed sn) 5 False
  in case found of
       Just s -> s
       Nothing -> backdrop

colorMath :: Snap -> Src -> Maybe Src -> Unsigned 15
colorMath sn mainS subS =
  let cgwsel = sCgwsel sn
      adsub = sCgadsub sn
      inWin = windowAt sn 5
      black = case (cgwsel `shiftR` 6) .&. 3 of 0 -> False; 1 -> not inWin; 2 -> inWin; _ -> True
      allowed = case (cgwsel `shiftR` 4) .&. 3 of 0 -> True; 1 -> inWin; 2 -> not inWin; _ -> False
      layerOk = case srLayer mainS of
        4 -> srObjMath mainS && testBit adsub 4
        5 -> testBit adsub 5
        n -> testBit adsub (fromEnum n)
      mainC = if black then 0 else srColor mainS
      (subC, subIsBack) = case subS of
        Just s | srLayer s /= 5 -> (srColor s, False)
        _ -> (sFixed sn, True)
      half = testBit adsub 6 && not black && not subIsBack
      ch k = bitCoerce (resize ((mainC `shiftR` k) .&. 31) :: Unsigned 8) :: Signed 8
      cs k = bitCoerce (resize ((subC `shiftR` k) .&. 31) :: Unsigned 8) :: Signed 8
      op k = let r0 = if testBit adsub 7 then ch k - cs k else ch k + cs k
                 r1 = if half then r0 `shiftR` 1 else r0
             in resize (bitCoerce (max 0 (min 31 r1)) :: Unsigned 8) :: Unsigned 15
  in if not (layerOk && allowed) then mainC else op 0 .|. (op 5 `shiftL` 5) .|. (op 10 `shiftL` 10)

scale :: Unsigned 8 -> Unsigned 15 -> Unsigned 15
scale inidisp col =
  let bright = resize (inidisp .&. 0xF) :: Unsigned 10
      f k = resize ((resize ((col `shiftR` k) .&. 31) * (bright + 1)) `shiftR` 4 :: Unsigned 10) :: Unsigned 15
  in if bright == 0 then 0 else f 0 .|. (f 5 `shiftL` 5) .|. (f 10 `shiftL` 10)

-- | Mode 7's map read for pixel x of line V: the address; tile 0
-- (outside, screen over 3); the pixel's row and column in its tile;
-- transparent (outside, screen over 2).
mode7Map :: PpuC -> Unsigned 8 -> Unsigned 9 -> (Unsigned 15, (Bool, Unsigned 3, Unsigned 3, Bool))
mode7Map c x0 v0 =
  let sel = reg c 0x1A
      mosaic = reg c 0x06
      msize = resize (mosaic `shiftR` 4) + 1 :: Unsigned 9
      row = v0 - 1
      x = if testBit mosaic 0 then resize (resize x0 - (resize x0 `mod` msize) :: Unsigned 9) else x0
      v = if testBit mosaic 0 then v0 - (row `mod` msize) else v0
      s16 :: Unsigned 16 -> Signed 32
      s16 w = resize (bitCoerce w :: Signed 16)
      s13 :: Unsigned 16 -> Signed 32
      s13 w = resize (bitCoerce (resize w :: Unsigned 13) :: Signed 13)
      a = s16 (pM7 c !! (0 :: Int))
      b = s16 (pM7 c !! (1 :: Int))
      cc = s16 (pM7 c !! (2 :: Int))
      d = s16 (pM7 c !! (3 :: Int))
      cx = s13 (pM7 c !! (4 :: Int))
      cy = s13 (pM7 c !! (5 :: Int))
      hofs = s13 (pM7Ofs c !! (0 :: Int))
      vofs = s13 (pM7Ofs c !! (1 :: Int))
      sx = fromIntegral (if testBit sel 0 then x `xor` 0xFF else x) :: Signed 32
      vv = v :: Unsigned 9
      sy = fromIntegral (if testBit sel 1 then vv `xor` 0xFF else vv) :: Signed 32
      clip o = let w = o .&. complement 0x1C00 in if o < 0 then w .|. 0x1C00 else w
      ox = s13 (resize (bitCoerce (clip (hofs - cx)) :: Unsigned 32))
      oy = s13 (resize (bitCoerce (clip (vofs - cy)) :: Unsigned 32))
      m k = k .&. complement 0x3F
      vx = m (a * ox) + m (b * oy) + cx * 256 + m (b * sy) + a * sx
      vy = m (cc * ox) + m (d * oy) + cy * 256 + m (d * sy) + cc * sx
      px = vx `shiftR` 8
      py = vy `shiftR` 8
      outside = px < 0 || px > 1023 || py < 0 || py > 1023
      over = sel `shiftR` 6
      pxu = resize (bitCoerce px :: Unsigned 32) :: Unsigned 10
      pyu = resize (bitCoerce py :: Unsigned 32) :: Unsigned 10
      addr = resize (pyu `shiftR` 3) * 128 + resize (pxu `shiftR` 3) :: Unsigned 15
  in (addr, (outside && over == 3, resize pyu, resize pxu, outside && over == 2))

-- * The clock

ppuClock :: PpuC -> PpuIn -> (PpuC, PpuOut)
ppuClock c0 i = (cE, out)
 where
  -- last clock's reads land
  readOut = ppuRead c0 i
  pixOut = pFinish c0
  cA = (if pVfill c0
          then c0 { pVlatch = resize (piVramB i !! (0 :: Int)) .|. (resize (piVramB i !! (1 :: Int)) `shiftL` 8), pVfill = False }
          else c0) { pRead = RNone, pFinish = Nothing }
  (cP, pipeA, cgA, rw) = pipeline cA i
  (cS, sprA, oamA, objW) = sprites cP i
  (cT, dotA, objA) = if piTick i then tick cS (piRingH i) else (cS, Nothing, 0)
  (cE, acc) = case piAccess i of
    Just a -> access cT a
    _ -> (cT, noAccess)
  vramA = case dotA of
    Just a -> a
    Nothing -> case pipeA of
      Just a -> a
      Nothing -> sprA
  out = PpuOut
    { poVramA = vramA
    , poVramB = aVramB acc
    , poVramW = aVramW acc
    , poCgramA = cgA
    , poCgramB = aCgB acc
    , poCgramW = aCgW acc
    , poOamA = oamA
    , poOamB = aOamB acc
    , poOamW = aOamW acc
    , poObjA = objA
    , poObjW = objW
    , poRingA = ringAddrs (ppSnap (pPipe cE))
    , poRingH = ringHAddr cE
    , poRingWE = rwE rw
    , poRingWH = rwH rw
    , poRingWW = rwW rw
    , poPixel = pixOut
    , poRead = readOut
    , poHc = pHc cE
    , poVc = pVc cE
    , poField = pField cE
    }

-- | A register read's value of last clock ('poRead'), from the state and
-- the memories' outputs only, not this clock's access: a machine can take
-- it in the clock it drives the next access ("Snes.Core").
ppuRead :: PpuC -> PpuIn -> Maybe (Maybe (Unsigned 8))
ppuRead c i = case pRead c of
  RNone -> Nothing
  RNow v -> Just v
  RLane l -> Just (Just (piOamB i !! l))
  RCg hiB -> Just (Just (if hiB then resize (piCgramB i `shiftR` 8) else resize (piCgramB i)))

-- | The pipeline's clock (subs 1-4): the VRAM addresses it reads (Mode 7's
-- char byte), the CGRAM address, the ring's writes. One 'screen' serves
-- both screens: the main at sub 2, the sub screen at sub 3 (the ring's
-- columns are the snapshot's until the next dot).
pipeline :: PpuC -> PpuIn -> (PpuC, Maybe (Vec 2 (Unsigned 15)), Unsigned 8, RingWrite)
pipeline c i =
  let pp = pPipe c
      sn = ppSnap pp
      subN = ppSub pp
      word = resize (piVramA i !! (0 :: Int)) .|. (resize (piVramA i !! (1 :: Int)) `shiftL` 8) :: Unsigned 16
      mode = resize (sBgmode sn) :: Unsigned 3
      hires = mode == 5 || mode == 6 || testBit (sSetini sn) 3
      m7fresh = case ppM7 pp of
        Just (_, _, _, True) -> 0
        Just _ -> piVramA i !! (1 :: Int)
        Nothing -> 0
      m7b = if subN == 2 then m7fresh else ppM7Byte pp
      isMain = subN == 2
      ring = zip (piRingE i) (piRingW i)
      scr = screen ring sn (ppObj pp) m7b isMain (if hires then Just isMain else Nothing)
  in case subN of
       1 ->
         let (c2, rw) = maybe (c, noRingWrite) (\f -> fetchStore c f word) (ppFetch pp)
             obj = piObj i !! (sX sn .&. 7)
             m7Req = case ppM7 pp of
               Just (tile0, py, px, _) ->
                 let tile = if tile0 then 0 else piVramA i !! (0 :: Int)
                 in Just (0 :> (resize tile * 64 + resize py * 8 + resize px) :> Nil)
               Nothing -> Nothing
         in (c2 { pPipe = pp { ppSub = 2, ppObj = obj } }, m7Req, 0, rw)
       2 -> (c { pPipe = pp { ppSub = 3, ppMain = scr, ppM7Byte = m7b } }, Nothing, maybe 0 id (srCg scr), noRingWrite)
       3 ->
         let mainS = (ppMain pp) { srColor = maybe (srColor (ppMain pp)) (const (piCgramA i)) (srCg (ppMain pp)) }
         in (c { pPipe = pp { ppSub = 4, ppMain = mainS, ppSubS = scr } }, Nothing, maybe 0 id (srCg scr), noRingWrite)
       4 ->
         let subS = (ppSubS pp) { srColor = maybe (srColor (ppSubS pp)) (const (piCgramA i)) (srCg (ppSubS pp)) }
             mainS = ppMain pp
             (l, r) | testBit (sInidisp sn) 7 = (0, 0)
                    | hires = (scale (sInidisp sn) (srColor subS), scale (sInidisp sn) (colorMath sn mainS Nothing))
                    | otherwise = let m = scale (sInidisp sn) (colorMath sn mainS (Just subS)) in (m, m)
             fin = if ppPixel pp then Just (sX sn, sRow sn, l, r) else Nothing
         in (c { pPipe = pp { ppSub = 0 }, pFinish = fin }, Nothing, 0, noRingWrite)
       _ -> (c, Nothing, 0, noRingWrite)

-- | A tick: H moves on; the dot it ends; the sprites at 1120; the line's
-- end. The VRAM port A addresses of the dot, the line buffer's address.
tick :: PpuC -> RingH -> (PpuC, Maybe (Vec 2 (Unsigned 15)), Unsigned 5)
tick c0 hd =
  let hc' = pHc c0 + 1
      c1 = c0 { pHc = hc' }
      dot = resize (hc' `shiftR` 2) - 1 :: Unsigned 9
      isDot = visibleLine c1 && hc' .&. 3 == 0 && dot >= 6 && dot <= 277
      (c2, req, objA) = if isDot then startDot c1 hd dot else (c1, Nothing, 0)
      next = pVc c2 + 1
      c3 = if hc' == 1120 && next >= 1 && next < vblankStart c2
             then (if forcedBlank c2 then c2 { pClear = 0 } else startSprites c2)
             else c2
      -- OAM's address reloads at V-blank (fullsnes)
      c3' = if hc' == 2 && pVc c3 == vblankStart c3 then c3 { pOamadd = pOamrel c3 } else c3
      c4 = if hc' == lineLength c0 then newLine c3' else c3'
  in (c4, req, objA)

newLine :: PpuC -> PpuC
newLine c =
  let (v', f') = if pVc c + 1 >= linesPerFrame c then (0, not (pField c)) else (pVc c + 1, pField c)
      c' = c { pHc = 0, pVc = v', pField = f', pSprBusy = False }
  in if v' == 0 && not (forcedBlank c') then c' { pStat77 = 0 } else c'

startDot :: PpuC -> RingH -> Unsigned 9 -> (PpuC, Maybe (Vec 2 (Unsigned 15)), Unsigned 5)
startDot c hd d =
  let v = pVc c
      fetching = d >= 6 && d < 270
      (fAddr, f) = fetchAddr c hd v (resize ((d - 6) `shiftR` 3)) (resize ((d - 6) .&. 7))
      pixel = d >= 22
      x = resize (d - 22) :: Unsigned 8
      mode = resize (reg c 0x05) :: Unsigned 3
      (m7Addr, m7) = mode7Map c x v
      sn = Snap
        { sInidisp = reg c 0x00, sBgmode = reg c 0x05, sMosaic = reg c 0x06, sSetini = reg c 0x33
        , sTm = reg c 0x2C, sTs = reg c 0x2D, sTmw = reg c 0x2E, sTsw = reg c 0x2F
        , sWsel = map (reg c) (0x23 :> 0x24 :> 0x25 :> Nil)
        , sWpos = map (reg c) (0x26 :> 0x27 :> 0x28 :> 0x29 :> Nil)
        , sWlog = map (reg c) (0x2A :> 0x2B :> Nil)
        , sCgwsel = reg c 0x30, sCgadsub = reg c 0x31, sFixed = pFixed c
        , sX = x, sRow = resize (v - 1), sFine = pFine c }
      m7now = pixel && mode == 7
      req | m7now = Just (m7Addr :> 0 :> Nil)
          | fetching = Just (fAddr :> fAddr :> Nil)
          | otherwise = Nothing
      pp = (pPipe c)
        { ppSub = 1, ppSnap = sn, ppPixel = pixel
        , ppFetch = if fetching then Just f else Nothing
        , ppM7 = if m7now then Just m7 else Nothing }
  in (c { pPipe = pp }, req, resize (x `shiftR` 3))

-- * The sprites

startSprites :: PpuC -> PpuC
startSprites c =
  let first = if testBit (reg c 0x03) 7 then resize (pOamrel c `shiftR` 2) else 0
  in c { pSpr = SpScan 0, pSprBusy = True, pSprObsel = reg c 0x01, pSprRow = resize (pVc c), pSprFirst = first
       , pNFound = 0, pClear = 0 }

objSizes :: Unsigned 8 -> ((Unsigned 7, Unsigned 7), (Unsigned 7, Unsigned 7))
objSizes k = case k .&. 7 of
  0 -> ((8, 8), (16, 16))
  1 -> ((8, 8), (32, 32))
  2 -> ((8, 8), (64, 64))
  3 -> ((16, 16), (32, 32))
  4 -> ((16, 16), (64, 64))
  5 -> ((32, 32), (64, 64))
  6 -> ((16, 32), (32, 64))
  _ -> ((16, 32), (32, 32))

-- | The sprite unit's clock: VRAM port A, OAM port A, line buffer writes.
sprites :: PpuC -> PpuIn -> (PpuC, Vec 2 (Unsigned 15), Unsigned 7, Vec 8 (Maybe (Unsigned 5, Unsigned 9)))
sprites c0 i =
  let clearing = pClear c0 < 32
      clearW = if clearing then repeat (Just (resize (pClear c0), 0)) else repeat Nothing
      c = if clearing then c0 { pClear = pClear c0 + 1 } else c0
      (small, large) = objSizes (pSprObsel c `shiftR` 5)
  in case pSpr c of
       SpIdle -> (c, repeat 0, 0, clearW)
       SpScan k ->
         let c2 = if k == 0 then c else
               let idx = (pSprFirst c + resize (k - 1)) .&. 0x7F
                   x0 = piOamA i !! (0 :: Int)
                   y = piOamA i !! (1 :: Int)
                   t = piOamA i !! (2 :: Int)
                   attr = piOamA i !! (3 :: Int)
                   hi = pOamHigh c !! (idx `shiftR` 2)
                   bits = hi `shiftR` (fromEnum (idx .&. 3) * 2)
                   x9 = resize x0 .|. (if testBit bits 0 then 0x100 else 0) :: Unsigned 9
                   xs = resize (bitCoerce x9 :: Signed 9) :: Signed 10
                   (w, h) = if testBit bits 1 then large else small
                   onLine = pSprRow c - y < resize h && (xs > negate (bitCoerce (resize w :: Unsigned 10)) || xs == -256) && xs < 256
                   e = SprEntry xs y (resize t .|. (if testBit attr 0 then 0x100 else 0)) attr w h
               in if onLine
                    then if pNFound c < 32
                           then c { pFound = replace (pNFound c) e (pFound c), pNFound = pNFound c + 1 }
                           else c { pStat77 = pStat77 c .|. 0x40 }
                    else c
             c3 = if k == 128 then startSliv c2 else c2 { pSpr = SpScan (k + 1) }
         in (c3, repeat 0, resize ((pSprFirst c + resize k) .&. 0x7F), clearW)
       SpFind e n -> (findSliv c e n, repeat 0, 0, clearW)
       SpSliv e col n ph ->
         let ent = pFound c !! e
             obsel = pSprObsel c
             base = resize (obsel .&. 7) `shiftL` 13 :: Unsigned 15
             gap = (resize ((obsel `shiftR` 3) .&. 3) + 1) `shiftL` 12 :: Unsigned 15
             attr = seAttr ent
             vflip = testBit attr 7
             hflip = testBit attr 6
             yr0 = resize (pSprRow c - seY ent) :: Unsigned 7
             yr = if vflip then seH ent - 1 - yr0 else yr0
             cols = resize (seW ent `shiftR` 3) :: Unsigned 4
             colI = if hflip then cols - 1 - col else col
             tile = seTile ent
             tx = (resize (tile .&. 0xF) + colI) .&. 0xF :: Unsigned 4
             ty = (resize ((tile `shiftR` 4) .&. 0xF) + resize (yr `shiftR` 3)) .&. 0xF :: Unsigned 4
             addr = base + (if testBit tile 8 then gap else 0) + (resize ty * 16 + resize tx) * 16 + resize (yr .&. 7)
             wordIn = resize (piVramA i !! (0 :: Int)) .|. (resize (piVramA i !! (1 :: Int)) `shiftL` 8) :: Unsigned 16
         in case ph of
              0 -> (c { pSpr = SpSliv e col n 1 }, addr :> addr :> Nil, 0, clearW)
              1 -> (c { pSpr = SpSliv e col n 2, pSprWord = wordIn }, (addr + 8) :> (addr + 8) :> Nil, 0, clearW)
              _ ->
                let w01 = pSprWord c
                    w23 = wordIn
                    pal = resize ((attr `shiftR` 1) .&. 7) :: Unsigned 9
                    pri = resize ((attr `shiftR` 4) .&. 3) :: Unsigned 9
                    sx0 = seX ent + bitCoerce (resize col `shiftL` 3 :: Unsigned 10)
                    bitAt ww j = if testBit ww j then 1 else 0 :: Unsigned 9
                    -- the pixel landing in bank b: the one of the 8 with (sx0 + k) mod 8 = b
                    pixFor :: Index 8 -> Maybe (Unsigned 5, Unsigned 9)
                    pixFor bank =
                      let k = resize ((fromIntegral (fromEnum bank) - resize (bitCoerce sx0 :: Unsigned 10)) .&. 7 :: Unsigned 10) :: Unsigned 4
                          sx = sx0 + resize (bitCoerce (resize k :: Unsigned 10) :: Signed 10)
                          b = fromEnum (if hflip then k else 7 - k)
                          idx = bitAt w01 b .|. (bitAt w01 (b + 8) `shiftL` 1) .|. (bitAt w23 b `shiftL` 2) .|. (bitAt w23 (b + 8) `shiftL` 3)
                      in if sx >= 0 && sx < 256 && idx /= 0
                           then Just (resize ((bitCoerce sx :: Unsigned 10) `shiftR` 3), idx .|. (pal `shiftL` 4) .|. (pri `shiftL` 7))
                           else Nothing
                    writes = map pixFor indicesI
                in (afterSliv c e col (n + 1), repeat 0, 0, zipWith orW clearW writes)
 where
  orW a b = case b of Just _ -> b; Nothing -> a

startSliv :: PpuC -> PpuC
startSliv c = if pNFound c == 0 then c { pSpr = SpIdle }
              else c { pSpr = SpFind (fromIntegral (pNFound c - 1)) 0 }

-- | A found sprite's columns on screen (x + 8c > -8 and < 256): a range,
-- empty only at x = -256.
colRange :: SprEntry -> (Signed 12, Signed 12)
colRange ent =
  let x = resize (seX ent) :: Signed 12
      cols = fromIntegral (seW ent `shiftR` 3) :: Signed 12
  in (max 0 (((-8 - x) `shiftR` 3) + 1), min (cols - 1) ((255 - x) `shiftR` 3))

-- | The tiles go from the last sprite found to the first, 34 at most (one
-- more on screen: time over). A clock a sprite without any on screen.
findSliv :: PpuC -> Index 32 -> Unsigned 6 -> PpuC
findSliv c e n =
  let (f, l) = colRange (pFound c !! e)
  in if f <= l
       then (if n >= 34 then c { pSpr = SpIdle, pStat77 = pStat77 c .|. 0x80 }
             else c { pSpr = SpSliv e (resize (bitCoerce f :: Unsigned 12)) n 0 })
       else (if e == 0 then c { pSpr = SpIdle } else c { pSpr = SpFind (e - 1) n })

-- | After a sliver (n so far): the sprite's next column, or the sprite
-- found before.
afterSliv :: PpuC -> Index 32 -> Unsigned 4 -> Unsigned 6 -> PpuC
afterSliv c e col n =
  let (_, l) = colRange (pFound c !! e)
  in if resize (bitCoerce (resize col :: Unsigned 12) :: Signed 12) < l
       then (if n >= 34 then c { pSpr = SpIdle, pStat77 = pStat77 c .|. 0x80 }
             else c { pSpr = SpSliv e (col + 1) n 0 })
       else (if e == 0 then c { pSpr = SpIdle } else c { pSpr = SpFind (e - 1) n })

-- * The CPU's accesses

data Acc = Acc
  { aVramB :: Unsigned 15
  , aVramW :: Maybe (Bool, Unsigned 8)
  , aCgB   :: Unsigned 8
  , aCgW   :: Maybe (Unsigned 15)
  , aOamB  :: Unsigned 7
  , aOamW  :: Vec 4 (Maybe (Unsigned 8))
  }

noAccess :: Acc
noAccess = Acc 0 Nothing 0 Nothing 0 (repeat Nothing)

access :: PpuC -> Access -> (PpuC, Acc)
access c a = case a of
  ALatch -> (latch c, noAccess)
  ARead r mdr -> readReg c r mdr
  AWrite r v -> writeReg c r v

latch :: PpuC -> PpuC
latch c = c { pHlatch = hdot (pHc c), pVlatch2 = pVc c, pLatched = True }

readReg :: PpuC -> Unsigned 6 -> Unsigned 8 -> (PpuC, Acc)
readReg c r mdr = case r of
  0x34 -> now (Just (mpy 0))
  0x35 -> now (Just (mpy 8))
  0x36 -> now (Just (mpy 16))
  0x37 -> ((latch c) { pRead = RNow Nothing }, noAccess)
  0x38 ->
    let ad = pOamadd c
        c' = c { pOamadd = ad + 1 }
    in if ad >= 0x200
         then (c' { pRead = RNow (Just (pOamHigh c !! (ad .&. 0x1F))) }, noAccess)
         else (c' { pRead = RLane (fromIntegral (ad .&. 3)) }, noAccess { aOamB = resize (ad `shiftR` 2) })
  0x39 -> vramRead False
  0x3A -> vramRead True
  0x3B ->
    let ad = pCgadd c
        fl = pCgflip c
    in (c { pCgflip = not fl, pCgadd = if fl then ad + 1 else ad, pRead = RCg fl }, noAccess { aCgB = ad })
  0x3C -> hv (pHlatch c) (pHflip c) (\c' f -> c' { pHflip = f })
  0x3D -> hv (pVlatch2 c) (pVflip c) (\c' f -> c' { pVflip = f })
  0x3E -> now (Just (pStat77 c .|. 0x01))
  0x3F -> ( c { pLatched = False, pHflip = False, pVflip = False
              , pRead = RNow (Just ((if pField c then 0x80 else 0) .|. (if pLatched c then 0x40 else 0) .|. 0x03)) }
          , noAccess )
  _ -> now Nothing
 where
  now v = (c { pRead = RNow v }, noAccess)
  mpy :: Int -> Unsigned 8
  mpy sh =
    let sa = resize (bitCoerce (pM7 c !! (0 :: Int)) :: Signed 16) :: Signed 24
        sb = resize (bitCoerce (resize ((pM7 c !! (1 :: Int)) `shiftR` 8) :: Unsigned 8) :: Signed 8) :: Signed 24
    in resize (bitCoerce ((sa * sb) `shiftR` sh) :: Unsigned 24)
  vramRead hiB =
    let l = pVlatch c
        vm = reg c 0x15
        val = if hiB then resize (l `shiftR` 8) else resize l
        inc = testBit vm 7 == hiB
        ad = pVaddr c
    in if inc
         then (c { pRead = RNow (Just val), pVfill = True, pVaddr = (ad + vramStep vm) .&. 0x7FFF }, noAccess { aVramB = vramRemap vm ad })
         else (c { pRead = RNow (Just val) }, noAccess)
  hv val fl set' =
    let out = if fl then resize ((val `shiftR` 8) .&. 1) .|. (mdr .&. 0xFE) else resize val
    in ((set' c (not fl)) { pRead = RNow (Just out) }, noAccess)

writeReg :: PpuC -> Unsigned 6 -> Unsigned 8 -> (PpuC, Acc)
writeReg c r v =
  let stored = c { pRegs = replace r v (pRegs c) }
      visL = not (forcedBlank c) && visibleLine c
      drawing = visL && pHc c >= 88 && pHc c < 1116
      oamBusy = visL || pSprBusy c
      vm = reg c 0x15
      m7w s c' = c' { pM7 = replace s ((resize v `shiftL` 8) .|. resize (pM7Prev c)) (pM7 c'), pM7Prev = v }
      m7o (s :: Index 2) c' = c' { pM7Ofs = replace s ((resize v `shiftL` 8) .|. resize (pM7Prev c)) (pM7Ofs c'), pM7Prev = v }
  in case r of
       0x02 -> let a' = (pOamrel c .&. 0x200) .|. (resize v `shiftL` 1) in (stored { pOamrel = a', pOamadd = a' }, noAccess)
       0x03 -> let a' = (if testBit v 0 then 0x200 else 0) .|. (pOamrel c .&. 0x1FE) in (stored { pOamrel = a', pOamadd = a' }, noAccess)
       0x04 ->
         let ad = pOamadd c
             c' = stored { pOamadd = ad + 1 }
         in if oamBusy then (c', noAccess)
            else if ad >= 0x200 then (c' { pOamHigh = replace (ad .&. 0x1F) v (pOamHigh c) }, noAccess)
            else if not (testBit ad 0) then (c' { pOamlatch = v }, noAccess)
            else let lane = fromIntegral (ad .&. 3) :: Index 4
                     ws = imap (\k _ -> if k == lane then Just v else if k == lane - 1 then Just (pOamlatch c) else Nothing) (repeat () :: Vec 4 ())
                 in (c', noAccess { aOamB = resize (ad `shiftR` 2), aOamW = ws })
       0x16 -> let a' = (pVaddr c .&. 0x7F00) .|. resize v in (stored { pVaddr = a', pVfill = True }, noAccess { aVramB = vramRemap vm a' })
       0x17 -> let a' = (pVaddr c .&. 0x00FF) .|. (resize (v .&. 0x7F) `shiftL` 8) in (stored { pVaddr = a', pVfill = True }, noAccess { aVramB = vramRemap vm a' })
       0x18 -> vramWrite False
       0x19 -> vramWrite True
       0x21 -> (stored { pCgadd = v, pCgflip = False }, noAccess)
       0x22 | drawing -> (stored, noAccess)
            | not (pCgflip c) -> (stored { pCglatch = v, pCgflip = True }, noAccess)
            | otherwise -> ( stored { pCgadd = pCgadd c + 1, pCgflip = False }
                           , noAccess { aCgB = pCgadd c, aCgW = Just (resize (pCglatch c) .|. (resize (v .&. 0x7F) `shiftL` 8)) } )
       _ | r >= 0x1B && r <= 0x20 -> (m7w (fromIntegral (r - 0x1B) :: Index 6) c, noAccess)
       _ | r >= 0x0D && r <= 0x14 ->
             let n = fromIntegral ((r - 0x0D) `shiftR` 1) :: Index 4
                 old = pBgPrev c
                 c1 = if testBit r 0
                        then c { pHofs = replace n (((resize v `shiftL` 8) .|. resize (old .&. complement 7) .|. resize (pBgPrev2 c .&. 7)) .&. 0x3FF) (pHofs c)
                               , pBgPrev2 = v }
                        else c { pVofs = replace n (((resize v `shiftL` 8) .|. resize old) .&. 0x3FF) (pVofs c) }
                 c2 = c1 { pBgPrev = v }
                 c3 = if r == 0x0D then m7o 0 c2 else if r == 0x0E then m7o 1 c2 else c2
             in (c3, noAccess)
       0x32 ->
         let k = resize (v .&. 0x1F) :: Unsigned 15
             f0 = pFixed c
             f1 = if testBit v 5 then (f0 .&. complement 0x1F) .|. k else f0
             f2 = if testBit v 6 then (f1 .&. complement (0x1F `shiftL` 5)) .|. (k `shiftL` 5) else f1
             f3 = if testBit v 7 then (f2 .&. complement (0x1F `shiftL` 10)) .|. (k `shiftL` 10) else f2
         in (c { pFixed = f3 }, noAccess)
       _ -> (stored, noAccess)
 where
  vramWrite hiB =
    let vm = reg c 0x15
        ad = pVaddr c
        ok = not (pSprBusy c) && (forcedBlank c || pVc c == 0 || pVc c >= vblankStart c)
        c' = c { pRegs = replace r v (pRegs c)
               , pVaddr = if testBit vm 7 == hiB then (ad + vramStep vm) .&. 0x7FFF else ad }
    in (c', noAccess { aVramB = vramRemap vm ad, aVramW = if ok then Just (hiB, v) else Nothing })
