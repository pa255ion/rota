{-# LANGUAGE MultiWayIf #-}
{-|
ANTIC in the reference model (M4 steps 4.1 and 4.2): the beam, NMIs,
VCOUNT, WSYNC, the display list, playfield, player/missile and refresh
DMA with cycle stealing, the line buffer, scrolling, and the playfield
sent to GTIA as codes on the AN bus.

Sources: Altirra Hardware Reference Manual (HRM) chapter 4 and 6.2
("ANx bus encodings"); Altirra's ANTIC as the cross-check for the rules
and cycle numbers (values and rules, not code: this is a per-cycle model
of the mechanisms the HRM describes, not Altirra's per-line patterns).

__The cycle.__ 'anticBegin' starts a machine cycle: it moves the beam,
does the work of the special cycles 0 to 10 (missile, display list,
player, address fetches; NMIs), 105 (WSYNC) and 112, and decides the
cycle's DMA: special DMA, playfield fetches, refresh. Real fetches read
memory through the connection's 'acRead' and halt the CPU ('True');
/virtual/ fetches (playfield DMA past cycle 105, on cycle 0, or with the
playfield width off: HRM 4.14, "Virtual DMA cycles") take no bus cycle
and capture whatever is on the bus at the end of the cycle, which
'anticEnd' supplies, after the CPU's access. 'anticEnd' also produces
the cycle's two colour clocks for GTIA.

__The DMA clock__ (HRM 4.12). Playfield fetches come from a clock that
ticks on a set of cycle positions modulo 8: every 2, 4 or 8 cycles for
the fast, medium and slow fetch rates (modes 2-5/D-F, 6-7/A-C, 8-9). It
is started at the playfield's start and stopped at its stop, both
computed from the fetch width and HSCROL ("the start" below is the
name-fetch cycle 10/18/26 for wide/normal/narrow, +HSCROL/2 when
scrolled; bitmap fetches come two cycles after a tick, character data
three); the start and stop positions freeze once their decision cycle
(one cycle before a name fetch, three before a bitmap fetch) has
passed, so later writes to DMACTL or HSCROL don't move them. A stop that
doesn't line up with the start (HSCROL or DMACTL changed between) leaves
bits in the clock: they go on ticking into the next lines, rotating by
two positions each line (114 mod 8), the /abnormal DMA/ of HRM 4.12,
until a stop clears them, a blank or jump line or vertical blank.

__The line buffer__ (HRM 4.11): 48 bytes, written by the name/bitmap
fetches of a mode line's first scan line, replayed on its other lines;
the display reads it from the playfield start on. Addresses past 47 are
discarded on write and read as $FF (an abnormal clock can get there).

__Display__ (HRM 6.2, table 17). Per colour clock ANTIC sends GTIA a
3-bit code: 000 background, 001 vertical sync, 010/011 horizontal blank
(and whether the line is hires), 100-111 PF0-PF3 in lores modes or a
pixel pair in hires modes (2, 3, F). Bitmap data shows four cycles after
its fetch, character data three (six after the name fetch); an odd
HSCROL delays the playfield by one more colour clock.
-}
module A800.Spec.Antic
  ( Antic
  , AnticConn (..)
  , newAntic
  , anticReset
  , anticBegin
  , anticEnd
  , anticRead
  , anticWrite
  , anticNmi
  , wsyncHold
  , anticPosition
  , anticFrames
  , anticIr
  , anticDebug
  , AnticSnap (..)
  , anticSnapshot
  ) where

import Prelude

import Control.Monad (forM_, unless, when)
import Control.Monad.ST (ST)
import Data.Bits
import qualified Data.Vector.Unboxed.Mutable as MV
import Data.Word

-- | What ANTIC is connected to.
data AnticConn s = AnticConn
  { acRead    :: Word16 -> ST s Word8          -- ^ a DMA read (no side effects)
  , acMissile :: Bool -> Word8 -> ST s ()      -- ^ missile data for GTIA (odd line?)
  , acPlayer  :: Bool -> Int -> Word8 -> ST s () -- ^ player n data for GTIA (odd line?)
  }

data Antic s = Antic
  { anSt    :: !(MV.MVector s Int)
  , anLine  :: !(MV.MVector s Word8) -- ^ the line buffer (48 bytes; 63 positions, the last 15 not there)
  , anChars :: !(MV.MVector s Word8) -- ^ character data fetched this scan line (in order)
  , anDisp  :: !(MV.MVector s Int)   -- ^ per cycle: the decoded playfield to show (see 'pfCell')
  , anConn  :: !(AnticConn s)
  }

-- * State

sX, sY, sFrames, sDmactl, sChactl, sDlist, sHscrol, sVscrol, sPmbase, sChbase,
  sChbaseNew, sChbaseAt, sNmien, sNmist, sPending, sEarlyNmien, sLateNmi,
  sNmiLine, sWsArm, sWsHold, sPenh, sPenv :: Int
sX = 0
sY = 1
sFrames = 2
sDmactl = 3
sChactl = 4
sDlist = 5
sHscrol = 6
sVscrol = 7
sPmbase = 8
sChbase = 9
sChbaseNew = 10   -- CHBASE takes effect two cycles after the write
sChbaseAt = 11    -- the cycle it does (-1: none waiting)
sNmien = 12
sNmist = 13
sPending = 14     -- the NMI noted at cycle 7 of this line ($40 VBI, $80 DLI, 0)
sEarlyNmien = 15  -- NMIEN as it was at the start of cycle 7
sLateNmi = 16     -- NMIEN enabled the pending NMI only by cycle 8: assert at 9
sNmiLine = 17
sWsArm = 18       -- WSYNC: absolute cycle from which the halt may start (-1: none)
sWsHold = 19      -- WSYNC: absolute cycle the halt ends (-1: not started)
sPenh = 20
sPenv = 21

-- Display list.
sIr, sIrPrev, sRow, sRowCount, sRowStopVs, sVsLatch, sVsLatch2, sDlActive,
  sExtra, sDlNext, sDlInTime, sDlistLatch :: Int
sIr = 22          -- the instruction register
sIrPrev = 23      -- the previous mode line's instruction (its vertical scroll bit)
sRow = 24         -- the row counter (4 bits)
sRowCount = 25    -- scan lines in this mode line
sRowStopVs = 26   -- this mode line ends a vertically scrolled region
sVsLatch = 27     -- VSCROL as of cycle 109 / the line start
sVsLatch2 = 28    -- VSCROL as of cycle 6 (for the DLI decision)
sDlActive = 29    -- the display list runs (lines 8-247, until a JVB)
sExtra = 30       -- address bytes to fetch at cycles 6 and 7 (LMS, jump)
sDlNext = 31      -- the address's low byte
sDlInTime = 32    -- DL DMA was on at cycle 0
sDlistLatch = 33  -- DLIST as of cycle 0

-- Playfield.
sPfFirst, sPfActive, sHsOn, sHsOff, sHsDelay, sLStart, sLEnd, sLVEnd, sLastCheck,
  sAbn, sEnding, sMsc, sWr, sNameRd, sCharWr, sDecRd, sDecChar, sShift, sAttr,
  sDecPos, sPreStart, sDispStart, sDispEnd, sHires, sRefresh, sTicks,
  sMissLate, sPhantomPm, sPhantomPmOn, sPhantomPl, sVblankBug,
  sCharBase :: Int
sPfFirst = 34     -- first scan line of the mode line: name/bitmap fetches
sPfActive = 35    -- the playfield runs on this scan line
sHsOn = 36        -- horizontal scrolling on this mode line
sHsOff = 37       -- HSCROL / 2 (DMA delay, cycles)
sHsDelay = 38     -- HSCROL bit 0 (one colour clock)
sLStart = 39      -- latched start (name-fetch cycle; 0: not latched)
sLEnd = 40        -- latched stop (capped at 106)
sLVEnd = 41       -- latched stop (not capped)
sLastCheck = 42   -- cycle of the last latch check
sAbn = 43         -- the DMA clock's stray bits (positions mod 8, as at the line start)
sEnding = 44      -- latent clock bits a slow mode can recapture
sMsc = 45         -- memory scan counter
sWr = 46          -- line buffer write position
sNameRd = 47      -- line buffer position the character data fetch reads names from
sCharWr = 48      -- character data written this line
sDecRd = 49       -- line buffer position the display reads
sDecChar = 50     -- character data read by the display
sShift = 51       -- the display's shift register
sAttr = 52        -- the last character name's attribute (invert mask / colour)
sDecPos = 53      -- next cycle position to decode
sPreStart = 54    -- the display decoded positions before the start (abnormal)
sDispStart = 55   -- playfield display region (cycles)
sDispEnd = 56
sHires = 57       -- the current mode is hires (2, 3, F)
sRefresh = 58     -- a refresh request waits
sTicks = 59       -- this cycle's playfield requests (bits: 0 graphic, 1 char data, 2 virtual graphic, 3 virtual char)
sMissLate = 60    -- missile DMA turned off too late (cycle 113)
sPhantomPm = 61   -- P/M DMA off in ANTIC, on in GTIA: GTIA reads the bus
sPhantomPmOn = 62
sPhantomPl = 63
sVblankBug = 64   -- hires in IR across vertical blank: the playfield isn't blanked
sCharBase = 66    -- character data base for this row

slots :: Int
slots = 67

get :: Antic s -> Int -> ST s Int
get a = MV.unsafeRead (anSt a)
{-# INLINE get #-}

set :: Antic s -> Int -> Int -> ST s ()
set a = MV.unsafeWrite (anSt a)
{-# INLINE set #-}

modify' :: Antic s -> Int -> (Int -> Int) -> ST s ()
modify' a s f = get a s >>= set a s . f
{-# INLINE modify' #-}

lineCycles, frameLines, vbiLine :: Int
lineCycles = 114
frameLines = 262
vbiLine = 248

-- * Mode tables (HRM 4.4, 4.5, table 7)

-- | Fetch rate: 0 none, 1 slow (8), 2 medium (4), 3 fast (2).
fetchRate :: Int -> Int
fetchRate m
  | m < 2 = 0
  | m <= 5 = 3
  | m <= 7 = 2
  | m <= 9 = 1
  | m <= 12 = 2
  | otherwise = 3

-- | The clock positions (mod 8) a clock started at offset @o@ ticks on.
clockPat :: Int -> Int -> Int
clockPat rate o = case rate of
  1 -> bit (o .&. 7)
  2 -> 0x11 `shiftL` (o .&. 3)
  3 -> 0x55 `shiftL` (o .&. 1)
  _ -> 0

rowsOf :: Int -> Int -> Int
rowsOf ir m = case m of
  0 -> ((ir `shiftR` 4) .&. 7) + 1
  1 -> 1
  2 -> 8
  3 -> 10
  4 -> 8
  5 -> 16
  6 -> 8
  7 -> 16
  8 -> 8
  9 -> 4
  10 -> 4
  11 -> 2
  12 -> 1
  13 -> 2
  _ -> 1

isChar :: Int -> Bool
isChar m = m >= 2 && m <= 7

rotl8, rotr8 :: Int -> Int -> Int
rotl8 v n = ((v `shiftL` n) .|. (v `shiftR` (8 - n))) .&. 0xFF
rotr8 v n = ((v `shiftR` n) .|. (v `shiftL` (8 - n))) .&. 0xFF

-- * Construction

-- | Power on: NMIST $7F (HRM table 5), PENV $FF, everything else 0.
newAntic :: AnticConn s -> ST s (Antic s)
newAntic conn = do
  a <- Antic <$> MV.replicate slots 0 <*> MV.replicate 63 0 <*> MV.replicate 64 0
             <*> MV.replicate 128 0 <*> pure conn
  set a sNmist 0x7F
  set a sPenv 0xFF
  set a sWsArm (-1)
  set a sWsHold (-1)
  set a sChbaseAt (-1)
  set a sDispStart 110
  set a sDispEnd 110
  pure a

-- | The reset line (XL/XE): NMIEN, DMACTL, the beam, the playfield DMA
-- clock (HRM 4.1, "Reset behavior").
anticReset :: Antic s -> ST s ()
anticReset a = do
  mapM_ (\s -> set a s 0) [sX, sY, sDmactl, sNmien, sNmiLine, sPending, sLateNmi, sAbn, sEnding, sExtra]
  set a sWsArm (-1)
  set a sWsHold (-1)

-- * The cycle

-- | Start the next cycle; @t@ is its absolute number, @bus@ the value on
-- the data bus from the last cycle. 0: the cycle is the CPU's; 1: ANTIC
-- fetched (through 'acRead'); 2: a refresh cycle (nothing drives the bus).
anticBegin :: Antic s -> Int -> Word8 -> ST s Int
anticBegin a t bus = do
  x0 <- get a sX
  x <- if x0 + 1 >= lineCycles then endLine a >> pure 0 else set a sX (x0 + 1) >> pure (x0 + 1)
  y <- get a sY
  -- CHBASE's delayed write
  cbAt <- get a sChbaseAt
  when (cbAt >= 0 && t >= cbAt) $ do
    get a sChbaseNew >>= set a sChbase
    set a sChbaseAt (-1)
    updateCharRow a
  -- latch the playfield edges whose decision point is this cycle
  latchEdges a x
  -- the playfield's requests for this cycle (they may merge with special DMA)
  pf <- playfieldTicks a x
  set a sTicks pf
  -- refresh requests: nine, from 25, every four
  when (x >= 25 && x <= 57 && (x - 25) .&. 3 == 0) (set a sRefresh 1)
  special <- specialCycle a x y bus
  let realPf = pf .&. 3 /= 0
  if | special -> pure 1
     | realPf -> do
         doPlayfieldFetches a False 0
         pure 1
     | otherwise -> do
         r <- get a sRefresh
         if r /= 0 then set a sRefresh 0 >> pure 2 else pure 0

-- | The playfield's requests for cycle @x@: bit 0 graphic (name/bitmap)
-- fetch, bit 1 character data fetch, bits 2/3 the same as virtual fetches.
playfieldTicks :: Antic s -> Int -> ST s Int
playfieldTicks a x = do
  active <- get a sPfActive
  if active == 0 then pure 0 else do
    ir <- get a sIr
    let m = ir .&. 15
        rate = fetchRate m
    if rate == 0 then pure 0 else do
      first <- get a sPfFirst
      (start, _, vend) <- edges a
      abn <- get a sAbn
      dmactl <- get a sDmactl
      let gStart = start + (if m >= 8 then 2 else 0)
          gVEnd = vend + (if m >= 8 then 2 else 0)
          gAbn = if m >= 8 then rotl8 abn 2 else abn
          tickAt s e ab p
            | s >= e = False
            | p < s = testBit ab (p .&. 7)
            | p < e = testBit (ab .|. clockPat rate s) (p .&. 7)
            | otherwise = testBit ((ab .|. clockPat rate s) .&. complement (clockPat rate e)) (p .&. 7)
          g = first /= 0 && tickAt gStart gVEnd gAbn x
          c = isChar m && tickAt (start + 3) (vend + 3) (rotl8 abn 3) x
          virtualAt = x >= 106 || x == 0 || dmactl .&. 3 == 0
          bits b v = if b then (if virtualAt then shiftL v 2 else v) else 0
      pure (bits g 1 .|. bits c 2)

-- | The playfield's start (name-fetch cycle), stop (capped at 106) and
-- uncapped stop, from the registers unless latched.
edges :: Antic s -> ST s (Int, Int, Int)
edges a = do
  ir <- get a sIr
  dmactl <- get a sDmactl
  hsOn <- get a sHsOn
  hsOff <- get a sHsOff
  ls <- get a sLStart
  le <- get a sLEnd
  lv <- get a sLVEnd
  let m = ir .&. 15
      width = dmactl .&. 3
      fw | hsOn /= 0 && width /= 0 && width /= 3 = width + 1
         | otherwise = width
      (s0, w) = case fw of
        1 -> (26, 64)
        2 -> (18, 80)
        3 -> (10, 96)
        _ -> (114, 0)
      s = if m >= 2 then s0 + hsOff else 114
      e = if m >= 2 then s + w else 114
      start = if ls /= 0 then ls else s
      end = min 106 (if le /= 0 then le else e)
      vend = if lv /= 0 then lv else e
  pure (start, end, vend)

-- | Freeze the start and stops whose decision points (a cycle before a
-- name fetch, three before a bitmap fetch: Altirra's offsets) are at or
-- before @x@ and after the last check.
latchEdges :: Antic s -> Int -> ST s ()
latchEdges a x = do
  dmactl <- get a sDmactl
  when (dmactl .&. 3 /= 0) $ do
    ir <- get a sIr
    lastC <- get a sLastCheck
    (start, _, vend) <- edges a
    let m = ir .&. 15
        off = if m < 8 then 1 else 3
        g = if m >= 8 then 2 else 0 -- the edges as Altirra counts them (bitmap: fetch cycles)
        passed e = (e + g - off) >= lastC && (e + g - off) <= x
        cap = min 106
    ls <- get a sLStart
    le <- get a sLEnd
    lv <- get a sLVEnd
    when (ls == 0 && start < 114 && passed start) (set a sLStart start)
    when (le == 0 && vend < 114 + 8 && passed (cap vend)) (set a sLEnd (cap vend))
    when (lv == 0 && vend < 114 + 8 && passed vend) (set a sLVEnd vend)
  set a sLastCheck x

-- | The special cycles 0-10, 105 and 112. 'True' if special DMA took the
-- bus (merged with any playfield fetch of the same cycle).
specialCycle :: Antic s -> Int -> Int -> Word8 -> ST s Bool
specialCycle a x y bus
  | x == 0 = do
      dmactl <- get a sDmactl
      late <- get a sMissLate
      took <- if (late /= 0 || dmactl .&. 0x0C /= 0) && visible
        then do
          set a sMissLate 0
          pmb <- get a sPmbase
          let addr | testBit dmactl 4 = ((pmb .&. 0xF8) `shiftL` 8) + 0x300 + y
                   | otherwise = ((pmb .&. 0xFC) `shiftL` 8) + 0x180 + (y `shiftR` 1)
          v <- fetchMerged addr
          acMissile (anConn a) (odd y) v
          unless (testBit dmactl 3) (set a sPhantomPl 1)
          pure True
        else pure False
      set a sDlInTime (if testBit dmactl 5 then 1 else 0)
      get a sDlist >>= set a sDlistLatch
      pure took
  | x == 1 = line1
  | x >= 2 && x <= 5 = do
      dmactl <- get a sDmactl
      let n = x - 2
      if testBit dmactl 3 && visible
        then do
          pmb <- get a sPmbase
          let addr | testBit dmactl 4 = ((pmb .&. 0xF8) `shiftL` 8) + 0x400 + 0x100 * n + y
                   | otherwise = ((pmb .&. 0xFC) `shiftL` 8) + 0x200 + 0x80 * n + (y `shiftR` 1)
          v <- fetchMerged addr
          acPlayer (anConn a) (odd y) n v
          pure True
        else do
          pmOn <- get a sPhantomPmOn
          pl <- get a sPhantomPl
          if | pmOn /= 0 && x > 3 -> acPlayer (anConn a) (odd y) (x - 4) bus
             | pl /= 0 && x > 2 -> acPlayer (anConn a) (odd y) (x - 3) bus
             | otherwise -> pure ()
          pure False
  | x == 6 = do
      pmOn <- get a sPhantomPmOn
      pl <- get a sPhantomPl
      if | pmOn /= 0 -> acPlayer (anConn a) (odd y) 2 bus
         | pl /= 0 -> acPlayer (anConn a) (odd y) 3 bus
         | otherwise -> pure ()
      took <- addrFetch $ \v -> set a sDlNext (fromIntegral v)
      get a sVscrol >>= set a sVsLatch2
      pure took
  | x == 7 = do
      pmOn <- get a sPhantomPmOn
      when (pmOn /= 0) (acPlayer (anConn a) (odd y) 3 bus)
      took <- addrFetch $ \v -> do
        lo <- get a sDlNext
        let addr = lo .|. (fromIntegral v `shiftL` 8)
        ir <- get a sIr
        if ir .&. 15 == 1
          then do
            set a sDlist addr
            when (testBit ir 6) $ do
              set a sDlActive 0
              set a sExtra 0
              set a sIr (ir .&. complement 0x4F)
              set a sRowCount 1
          else do
            set a sMsc addr
            set a sExtra 0
      -- NMIs (HRM 4.8)
      get a sNmien >>= set a sEarlyNmien
      if y == vbiLine
        then do
          set a sPending 0x40
          modify' a sNmist (\s -> (s .|. 0x40) .&. complement 0x80)
          ir <- get a sIr
          set a sIrPrev ir
          set a sIr (ir .&. 0x20)
        else do
          set a sPending 0
          ir <- get a sIr
          row <- get a sRow
          stop <- rowStop a sVsLatch2
          when (testBit ir 7 && row == stop) $ do
            set a sPending 0x80
            modify' a sNmist (\s -> (s .&. complement 0x40) .|. 0x80)
      -- the line buffer pointers restart
      mapM_ (\s -> set a s 0) [sWr, sNameRd, sCharWr, sDecRd, sDecChar]
      pure took
  | x == 8 = do
      p <- get a sPending
      early <- get a sEarlyNmien
      en <- get a sNmien
      set a sLateNmi 0
      if | p .&. early /= 0 -> set a sNmiLine 1
         | p .&. en .&. complement early /= 0 -> set a sLateNmi 1
         | otherwise -> pure ()
      pure False
  | x == 9 = do
      late <- get a sLateNmi
      when (late /= 0) (set a sNmiLine 1)
      pure False
  | x == 10 = do
      -- the display restarts; vertical blank, or the hires bug (HRM 6.2)
      forM_ [0 .. 127] $ \i -> MV.unsafeWrite (anDisp a) i 0
      mapM_ (\s -> set a s 0) [sShift, sAttr, sPreStart]
      set a sDecPos 1
      hires <- get a sHires
      dmactl <- get a sDmactl
      set a sVblankBug (if not visible && hires /= 0 && dmactl .&. 3 /= 0 then 1 else 0)
      pure False
  | x == 112 = do
      -- a mode line ending here: the next line's first fetches may come in
      -- cycles 0-7 with an abnormal clock (Altirra's rule)
      row <- get a sRow
      stop <- rowStop a sVsLatch
      first <- get a sPfFirst
      when (row == stop && first == 0) $ do
        set a sPfFirst 1
        set a sWr 48
      pure False
  | otherwise = pure False
 where
  visible = y >= 8 && y < 248
  -- a special fetch: merged with a playfield fetch of the same cycle
  -- (the address is the AND of both, the data goes to both)
  fetchMerged addr = do
    pf <- get a sTicks
    if pf .&. 3 /= 0
      then do
        pa <- pfAddress a pf
        v <- acRead (anConn a) (fromIntegral (addr .&. pa))
        doPlayfieldFetches a True v
        pure v
      else acRead (anConn a) (fromIntegral addr)
  addrFetch k = do
    extra <- get a sExtra
    dmactl <- get a sDmactl
    if extra /= 0 && testBit dmactl 5
      then do
        dl <- get a sDlist
        v <- fetchMerged dl
        set a sDlist ((dl .&. 0xFC00) .|. ((dl + 1) .&. 0x03FF))
        k v
        pure True
      else pure False
  line1 = do
    mapM_ (\s -> set a s 0) [sPfFirst, sPfActive, sLStart, sLEnd, sLVEnd, sLastCheck]
    when (y == 8) $ do
      set a sDlActive 1
      set a sRow 0
      set a sRowCount 1
      set a sRowStopVs 0
      get a sIrPrev >>= set a sIr
    stop <- rowStop a sVsLatch
    get a sVscrol >>= set a sVsLatch
    row <- get a sRow
    took <- if row /= stop
      then do
        set a sRow ((row + 1) .&. 15)
        set a sPfActive 1
        ir <- get a sIr
        when (ir .&. 15 /= 1) (set a sExtra 0)
        pure False
      else do
        set a sRow 0
        set a sRowStopVs 0
        dlActive <- get a sDlActive
        if dlActive == 0 then pure False else do
          set a sExtra 0
          get a sIr >>= set a sIrPrev
          inTime <- get a sDlInTime
          took <- if inTime /= 0
            then do
              dl <- get a sDlistLatch
              v <- fetchMerged dl
              set a sIr (fromIntegral v)
              modify' a sDlist (\d -> (d .&. 0xFC00) .|. ((d + 1) .&. 0x03FF))
              pm <- get a sPhantomPm
              when (pm /= 0) $ do
                set a sPhantomPmOn 1
                acMissile (anConn a) (odd y) v
              pure True
            else pure False
          newModeLine
          pure took
    hsOn <- get a sHsOn
    hs <- get a sHscrol
    set a sHsOff (if hsOn /= 0 then (hs .&. 14) `shiftR` 1 else 0)
    set a sHsDelay (if hsOn /= 0 then hs .&. 1 else 0)
    updateCharRow a
    updateDisplayRegion a
    set a sShift 0
    -- the playfield runs from here on this line (whether it fetches depends
    -- on the mode; recomputed on DMACTL/HSCROL writes)
    set a sPfActive 1
    pure took
  newModeLine = do
    ir <- get a sIr
    let m = ir .&. 15
    when (m == 1 || (m >= 2 && testBit ir 6)) (set a sExtra 1)
    set a sRow 0
    set a sRowCount (rowsOf ir m)
    set a sHires (if m == 2 || m == 3 || m == 15 then 1 else 0)
    -- the DMA clock on a mode change (HRM 4.12, "Disrupting the DMA clock
    -- with mode switching")
    abn <- get a sAbn
    ending <- get a sEnding
    set a sAbn $ if m < 2 then 0 else case fetchRate m of
      1 -> abn .|. ending
      2 -> (abn .&. 15) * 0x11
      _ -> (abn .&. 3) * 0x55
    -- vertical scrolling (HRM 4.7)
    prev <- get a sIrPrev
    let vsOf v = if v .&. 15 < 2 then 0 else v
        cur = vsOf ir
        prv = vsOf prev
    when ((cur `xor` prv) .&. 0x20 /= 0) $
      if testBit cur 5 then get a sVscrol >>= set a sRow else set a sRowStopVs 1
    set a sHsOn (if m /= 1 && testBit cur 4 then 1 else 0)
    set a sPfFirst 1
    set a sPfActive 1

-- | The row the mode line ends on.
rowStop :: Antic s -> Int -> ST s Int
rowStop a latch = do
  vs <- get a sRowStopVs
  if vs /= 0 then get a latch else (\n -> (n - 1) .&. 15) <$> get a sRowCount

-- | The address of this cycle's playfield fetch (the AND of a name/bitmap
-- and a character data fetch if both).
pfAddress :: Antic s -> Int -> ST s Int
pfAddress a pf = do
  msc <- get a sMsc
  base <- get a sCharBase
  ir <- get a sIr
  nr <- get a sNameRd
  name <- lineAt a nr
  let gAddr = (msc .&. 0xF000) .|. (msc .&. 0x0FFF)
      mask = if (ir .&. 15) >= 6 then 0x3F else 0x7F
      cAddr = base + ((fromIntegral name .&. mask) `shiftL` 3)
  pure $ case (testBit pf 0 || testBit pf 2, testBit pf 1 || testBit pf 3) of
    (True, True) -> gAddr .&. cAddr
    (True, False) -> gAddr
    _ -> cAddr

lineAt :: Antic s -> Int -> ST s Word8
lineAt a i
  | i >= 48 = pure 0xFF
  | otherwise = MV.unsafeRead (anLine a) i

-- | Do this cycle's playfield fetches. With @merged@, the data is @v@ (a
-- special fetch of the same cycle read it); otherwise real fetches read
-- memory. Virtual fetches wait for 'anticEnd'.
doPlayfieldFetches :: Antic s -> Bool -> Word8 -> ST s ()
doPlayfieldFetches a merged v0 = do
  pf <- get a sTicks
  when (pf .&. 3 /= 0) $ do
    v <- if merged then pure v0 else pfAddress a pf >>= acRead (anConn a) . fromIntegral
    completeFetches a (pf .&. 3) v
    set a sTicks (pf .&. 0x0C)

-- | Graphic (bit 0) and character (bit 1) fetches with this data.
completeFetches :: Antic s -> Int -> Word8 -> ST s ()
completeFetches a kinds v = do
  when (testBit kinds 0) $ do
    w <- get a sWr
    when (w < 48) (MV.unsafeWrite (anLine a) w v)
    set a sWr (if w + 1 >= 63 then 0 else w + 1)
    modify' a sMsc (\msc -> (msc .&. 0xF000) .|. ((msc + 1) .&. 0x0FFF))
  when (testBit kinds 1) $ do
    cw <- get a sCharWr
    when (cw < 64) (MV.unsafeWrite (anChars a) cw v)
    set a sCharWr (cw + 1)
    modify' a sNameRd (\r -> if r + 1 >= 63 then 0 else r + 1)

updateCharRow :: Antic s -> ST s ()
updateCharRow a = do
  ir <- get a sIr
  cb <- get a sChbase
  chactl <- get a sChactl
  row <- get a sRow
  let m = ir .&. 15
      flipR = if testBit chactl 2 then 7 else 0
      r = case m of
        5 -> (row `shiftR` 1) .&. 7
        7 -> (row `shiftR` 1) .&. 7
        _ -> row .&. 7
      base | m == 6 || m == 7 = (cb .&. 0xFE) `shiftL` 8
           | otherwise = (cb .&. 0xFC) `shiftL` 8
  set a sCharBase (base + (r `xor` flipR))

updateDisplayRegion :: Antic s -> ST s ()
updateDisplayRegion a = do
  dmactl <- get a sDmactl
  let (s, e) = case dmactl .&. 3 of
        1 -> (32, 96)
        2 -> (24, 104)
        3 -> (22, 112)
        _ -> (110, 110)
  set a sDispStart s
  set a sDispEnd e

-- | The end of a line (Altirra's AdvanceScanline).
endLine :: Antic s -> ST s ()
endLine a = do
  set a sX 0
  set a sNmiLine 0
  y <- (+ 1) <$> get a sY
  if y >= frameLines
    then do
      set a sY 0
      set a sDlActive 0
      modify' a sFrames (+ 1)
    else set a sY y
  y' <- get a sY
  when (y >= vbiLine && y < frameLines) $ do
    set a sDlActive 0
    set a sExtra 0
    set a sAbn 0
  when (y < vbiLine) $ do
    ir <- get a sIr
    let m = ir .&. 15
    set a sEnding 0
    when (m >= 2) $ do
      (start, _, vend) <- edges a
      dmactl <- get a sDmactl
      lv <- get a sLVEnd
      abn <- get a sAbn
      let rate = fetchRate m
          danger = vend - (16 `shiftR` rate) - 109
      when (vend > start && rate > 0 && danger >= 0) $
        set a sEnding ((clockPat rate 7 `shiftR` (4 - danger)) .&. 0x38)
      let abn1 = if start < 114 then abn .|. clockPat rate start else abn
          abn2 = if lv /= 0 || dmactl .&. 3 /= 0 then abn1 .&. complement (clockPat rate vend) else abn1
      set a sAbn (rotr8 abn2 2)
  dmactl <- get a sDmactl
  set a sPhantomPm (if dmactl .&. 0x2C == 0x20 && y' >= 8 && y' < 248 then 1 else 0)
  set a sPhantomPmOn 0
  set a sPhantomPl 0
  mapM_ (\s -> set a s 0) [sWr, sNameRd, sCharWr, sDecRd, sDecChar]

-- | The end of the cycle: virtual fetches capture @bus@ (what the CPU or
-- ANTIC put on it), the display decodes, and the two colour clocks of
-- this cycle go to GTIA as AN codes (first, second).
anticEnd :: Antic s -> Word8 -> ST s (Int, Int)
anticEnd a bus = do
  pf <- get a sTicks
  when (pf .&. 0x0C /= 0) $ do
    completeFetches a (pf `shiftR` 2) bus
    set a sTicks 0
  x <- get a sX
  -- the display restarts at cycle 10 (from position 1); nothing is decoded
  -- before (the line buffer's pointers restart at 7)
  when (x >= 10) (decode a x)
  anCodes a x

-- | Decode the playfield up to this cycle (bitmap data is decoded the
-- cycle it arrives, character data the cycle its data fetch did, three
-- after the name), into the display buffer at +4 / +6.
decode :: Antic s -> Int -> ST s ()
decode a x = do
  active <- get a sPfActive
  ir <- get a sIr
  let m = ir .&. 15
      rate = fetchRate m
      char = isChar m
      target = if char then x - 3 else x
  when (active /= 0 && rate > 0) $ do
    (start, _, vend) <- edges a
    abn <- get a sAbn
    let s = start + (if m >= 8 then 2 else 0)
        e = vend + (if m >= 8 then 2 else 0)
        ab = if m >= 8 then rotl8 abn 2 else abn
        pat = ab .|. clockPat rate s
        -- bit 0: the clock has even positions, bit 1: odd ones (mode 8's shift)
        pat' = (if pat .&. 0x55 /= 0 then 1 else 0) .|. (if pat .&. 0xAA /= 0 then 2 else 0) :: Int
        tick p
          | s >= e = False
          | p < s = testBit ab (p .&. 7)
          | p < e = testBit pat (p .&. 7)
          | otherwise = False
        -- the display's read position moves two cycles before each load;
        -- at the stop only stray (abnormal) bits count
        -- (before the start, only from position 5: the line buffer's
        -- pointers restart at cycle 7; Altirra's rule)
        incAt p
          | p < s && p < 5 = False
          | p + 2 >= e = testBit (pat .&. complement (clockPat rate s)) ((p + 2) .&. 7)
          | otherwise = tick (p + 2)
        -- at most two positions a cycle (the hardware's budget): the
        -- positions before an abnormal start, which begin at cycle 10, are
        -- caught up by cycle 19, before anything of them is shown
        go budget p
          | budget == 0 || p > target || p >= e = pure ()
          | otherwise = do
              pre <- get a sPreStart
              when (p == s && pre /= 0) $ do
                modify' a sDecRd (\r -> (r + 62) `mod` 63)
                set a sPreStart 0
              when (p < s) (set a sPreStart 1)
              step p (tick p) (testBit pat' (p .&. 1))
              when (incAt p) (modify' a sDecRd (\r -> if r + 1 >= 63 then 0 else r + 1))
              set a sDecPos (p + 1)
              go (budget - 1) (p + 1)
    pos <- get a sDecPos
    let from = if ab == 0 then max pos s else pos
    go (2 :: Int) from
 where
  step p load parity8 = do
    ir <- get a sIr
    chactl <- get a sChactl
    row <- get a sRow
    let m = ir .&. 15
        shiftBy = case m of
          8 -> 0
          9 -> 1
          6 -> 2
          7 -> 2
          10 -> 2
          11 -> 2
          12 -> 2
          _ -> 4
    sh0 <- get a sShift
    -- mode 8 shifts two bits every other cycle: on the cycles of the
    -- clock's parity (both, with an abnormal clock: HRM 4.12)
    let _ = p
        sh1 | m == 8 = if parity8 then (sh0 `shiftL` 2) .&. 0xFF else sh0
            | otherwise = (sh0 `shiftL` shiftBy) .&. 0xFF
    sh2 <- if not load then pure sh1 else do
      rd <- get a sDecRd
      c <- lineAt a rd
      if isChar m
        then do
          ci <- get a sDecChar
          set a sDecChar (ci + 1)
          d0 <- if ci < 64 then MV.unsafeRead (anChars a) ci else pure 0
          let cI = fromIntegral c :: Int
              hi = testBit cI 7
              -- blink (CHACTL bit 0) only in the hires modes 2 and 3
              blinkOk = m > 3 || not (testBit chactl 0) || not hi
              d1 = if blinkOk then fromIntegral d0 else 0
              d = case m of
                2 | row .&. 14 == 8 && cI .&. 0x60 /= 0x60 -> 0
                3 -> let base = if row >= 2 then 0xFF else 0
                         msk = if row .&. 6 == 0 && cI .&. 0x60 /= 0x60 then base `xor` 0xFF else base
                     in d1 .&. msk
                _ -> d1
          set a sAttr $ case m of
            _ | m <= 3 -> if hi && testBit chactl 1 then 0xFF else 0
              | m <= 5 -> if hi then 1 else 0
              | otherwise -> cI `shiftR` 6
          pure (sh1 .|. d)
        else pure (sh1 .|. fromIntegral c)
    set a sShift sh2
    attr <- get a sAttr
    let out = case m of
          _ | m <= 3 -> ((sh2 `xor` attr) `shiftR` 4) .&. 15              -- 2 cc x 2 hires bits
            | m <= 5 -> lores2 (sh2 `shiftR` 6) (attr /= 0) * 16 + lores2 ((sh2 `shiftR` 4) .&. 3) (attr /= 0)
            | m <= 7 -> let b1 = testBit sh2 7; b2 = testBit sh2 6
                            col = bit attr :: Int
                        in (if b1 then col else 0) * 16 + (if b2 then col else 0)
            | m == 8 || m == 10 -> let v = lores2 (sh2 `shiftR` 6) False in v * 16 + v
            | m == 9 -> let v = if testBit sh2 7 then 1 else 0 in v * 16 + v
            | m <= 12 -> (if testBit sh2 7 then 16 else 0) + (if testBit sh2 6 then 1 else 0)
            | m <= 14 -> lores2 (sh2 `shiftR` 6) False * 16 + lores2 ((sh2 `shiftR` 4) .&. 3) False
            | otherwise -> (sh2 `shiftR` 4) .&. 15
        dst = p + (if isChar m then 6 else 4)
    when (dst < 128) (MV.unsafeWrite (anDisp a) dst out)
  -- a lores 2-bit pixel as one-hot playfield bits (BAK 0, PF0 1, PF1 2, PF2 4, PF3 8)
  lores2 v alt = case v .&. 3 of
    0 -> 0
    1 -> 1
    2 -> 2
    _ -> if alt then 8 else 4

-- | The AN codes of this cycle's two colour clocks (HRM table 17).
anCodes :: Antic s -> Int -> ST s (Int, Int)
anCodes a x = do
  y <- get a sY
  hires <- get a sHires
  bug <- get a sVblankBug
  let hblank = if hires /= 0 then 3 else 2
      vblank = y < 8 || y >= 248
  if | vblank && bug == 0 -> pure (if y >= 251 && y <= 253 && x >= 17 && x < 111 then (1, 1) else (hblank, hblank))
     | x < 17 || x >= 111 -> pure (hblank, hblank)
     | otherwise -> do
         ds <- get a sDispStart
         de <- get a sDispEnd
         dmactl <- get a sDmactl
         if x < ds || x >= de || dmactl .&. 3 == 0 then pure (0, 0) else do
           active <- get a sPfActive
           delay <- get a sHsDelay
           cur <- if active /= 0 && not vblank then MV.unsafeRead (anDisp a) x else pure 0
           prev <- if active /= 0 && not vblank && x > 0 then MV.unsafeRead (anDisp a) (x - 1) else pure 0
           let v | delay /= 0 && hires /= 0 = ((prev `shiftL` 2) .|. (cur `shiftR` 2)) .&. 15
                 | delay /= 0 = ((prev .&. 15) `shiftL` 4) .|. (cur `shiftR` 4)
                 | otherwise = cur
           pure $ if hires /= 0
             then (4 .|. ((v `shiftR` 2) .&. 3), 4 .|. (v .&. 3))
             else (pfCode (v `shiftR` 4), pfCode (v .&. 15))
 where
  pfCode n
    | testBit n 3 = 7
    | testBit n 2 = 6
    | testBit n 1 = 5
    | testBit n 0 = 4
    | otherwise = 0

-- | The NMI output.
anticNmi :: Antic s -> ST s Bool
anticNmi a = (/= 0) <$> get a sNmiLine

-- | Whether WSYNC holds RDY low on cycle @t@, where the CPU wants to read
-- and its last own cycle was @lastCpu@. Starts and ends the halt.
wsyncHold :: Antic s -> Int -> Int -> ST s Bool
wsyncHold a t lastCpu = do
  hold <- get a sWsHold
  if hold >= 0
    then if t < hold then pure True else do
      set a sWsHold (-1)
      pure False
    else do
      arm <- get a sWsArm
      if arm < 0 || t < arm then pure False else do
        -- RDY drops at the first cycle from the arm on where the CPU's next
        -- access is a read: right after its last own cycle.
        let s = max arm (lastCpu + 1)
        x <- get a sX
        let xs = x - (t - s)
            toRelease = (105 - xs) `mod` lineCycles
            release = s + toRelease
        set a sWsArm (-1)
        if t < release
          then set a sWsHold release >> pure True
          else pure False

-- | Read register @r@ (low four bits of the address).
anticRead :: Antic s -> Word16 -> ST s Word8
anticRead a r = case r .&. 0x0F of
  0x0B -> do
    x <- get a sX
    y <- get a sY
    let y' | x >= 111 = if x >= 112 && y + 1 >= frameLines then 0 else y + 1
           | otherwise = y
    pure (fromIntegral (y' `shiftR` 1))
  0x0C -> fromIntegral <$> get a sPenh
  0x0D -> fromIntegral <$> get a sPenv
  0x0F -> fromIntegral <$> get a sNmist
  _ -> pure 0xFF

-- | Write register @r@ on cycle @t@.
anticWrite :: Antic s -> Int -> Word16 -> Word8 -> ST s ()
anticWrite a t r v0 = case r .&. 0x0F of
  0x00 -> do
    let new = v .&. 0x3F
    old <- get a sDmactl
    when (new /= old) $ do
      x <- get a sX
      y <- get a sY
      latchEdges a x
      -- missile DMA turned off on cycle 113 is too late for cycle 0
      when (x == 113 && new .&. 0x0C == 0 && old .&. 0x0C /= 0 && y >= 7 && y < 247) (set a sMissLate 1)
      set a sDmactl new
      updateDisplayRegion a
      recheckActive a
  0x01 -> set a sChactl (v .&. 0x07) >> updateCharRow a
  0x02 -> modify' a sDlist (\d -> (d .&. 0xFF00) .|. v)
  0x03 -> modify' a sDlist (\d -> (d .&. 0x00FF) .|. (v `shiftL` 8))
  0x04 -> do
    let new = v .&. 0x0F
    old <- get a sHscrol
    hsOn <- get a sHsOn
    when (new /= old && hsOn /= 0) $ do
      when ((new `xor` old) .&. 1 /= 0) (set a sHsDelay (new .&. 1))
      when ((new `xor` old) .&. 14 /= 0) $ do
        get a sX >>= latchEdges a
        set a sHsOff ((new .&. 14) `shiftR` 1)
        recheckActive a
    set a sHscrol new
  0x05 -> do
    set a sVscrol (v .&. 0x0F)
    x <- get a sX
    when (x >= 1 && x < 109) (set a sVsLatch (v .&. 0x0F))
  0x07 -> set a sPmbase (v .&. 0xFC)
  0x09 -> do
    set a sChbaseNew v
    set a sChbaseAt (t + 2)
  0x0A -> do
    arm <- get a sWsArm
    hold <- get a sWsHold
    x <- get a sX
    -- A write while one is armed rearms only for the second write of an
    -- INC WSYNC on cycle 104 (HRM 4.9, "Read-modify-write instructions").
    when (hold < 0 && (arm < 0 || (t >= arm - 1 && x == 104))) (set a sWsArm (t + 2))
  0x0E -> set a sNmien (v .&. 0xC0)
  0x0F -> do
    x <- get a sX
    p <- get a sPending
    set a sNmist (0x1F .|. (if x == 7 then p else 0))
  _ -> pure ()
 where
  v = fromIntegral v0 :: Int

-- | After a DMACTL or HSCROL change: the playfield runs on if its start
-- is latched or still ahead (Altirra's rule).
recheckActive :: Antic s -> ST s ()
recheckActive a = do
  x <- get a sX
  when (x >= 1) $ do
    ir <- get a sIr
    ls <- get a sLStart
    (start, _, _) <- edges a
    let m = ir .&. 15
        ok = ls /= 0 || x <= max 10 (start + (if m >= 8 then 2 else 0) - (if m < 8 then 2 else 4))
    set a sPfActive (if ok then 1 else 0)

-- | (line, cycle) of the cycle being run.
anticPosition :: Antic s -> ST s (Int, Int)
anticPosition a = (,) <$> get a sY <*> get a sX

-- | Frames completed (line 261 to 0 transitions).
anticFrames :: Antic s -> ST s Int
anticFrames a = get a sFrames

-- | The instruction register (for tests).
anticIr :: Antic s -> ST s Int
anticIr a = get a sIr

-- | What a save state takes from ANTIC (side track N,
-- "A800.Spec.SaveState"): the position, the registers, the hidden state
-- the tests compare, and whether nothing is on its way.
data AnticSnap = AnticSnap
  { asLine, asCycle, asNmist, asDmactl, asChactl, asDlist, asHscrol, asVscrol, asPmbase, asChbase
  , asNmien, asPenh, asPenv, asIrPrev, asMsc, asHires :: !Int
  , asQuiet :: !Bool -- ^ no WSYNC armed or held, no CHBASE write on its way, the NMI line low, no late NMI
  }

anticSnapshot :: Antic s -> ST s AnticSnap
anticSnapshot a = do
  let g = get a
  quiet <- and <$> sequence
    [ (< 0) <$> g sWsArm, (< 0) <$> g sWsHold, (< 0) <$> g sChbaseAt, (== 0) <$> g sNmiLine, (== 0) <$> g sLateNmi ]
  AnticSnap <$> g sY <*> g sX <*> g sNmist <*> g sDmactl <*> g sChactl <*> g sDlist <*> g sHscrol
    <*> g sVscrol <*> g sPmbase <*> g sChbase <*> g sNmien <*> g sPenh <*> g sPenv <*> g sIrPrev
    <*> g sMsc <*> g sHires <*> pure quiet

-- | Internal state, for debugging logs.
anticDebug :: Antic s -> ST s String
anticDebug a = do
  vs <- mapM (get a) [sIr, sRow, sPfFirst, sPfActive, sDecPos, sDecRd, sAbn, sLStart, sLVEnd, sHsOff, sWr]
  x <- get a sX
  d <- if x < 128 then MV.unsafeRead (anDisp a) x else pure 0
  pure (unwords (zipWith (\n v -> n <> "=" <> show v) ["ir", "row", "first", "act", "dpos", "drd", "abn", "ls", "lv", "hs", "wr"] vs) <> " disp=" <> show d)
