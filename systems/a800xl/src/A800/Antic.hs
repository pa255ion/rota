{-# LANGUAGE StrictData #-}
{-|
ANTIC in Clash (M4 step 4.5). It reproduces the reference model
("A800.Spec.Antic") cycle for cycle; that module has the rules (the DMA
clock and its abnormal bits, the edge latches, the line buffer, the
display's decode, NMIs, WSYNC).

__Ticks__ ('AnticIn'), in a machine cycle's order:

1. 'ABegin' (the last cycle's bus value): the beam moves, the special
   cycle's work up to its fetch, the playfield's requests; 'aoBus' says
   whose cycle it is. If a special fetch (missile, player, display list)
   takes the bus, 'aoReq' asks for it now (merged with a playfield fetch
   of the same cycle: the AND of both addresses).
2. 'AFetch' (the data), only when 'aoReq' asked: the playfield's part of
   the fetch is done here.
3. 'APost': the rest of the special cycle (with the special fetch's
   data). If only the playfield fetches, 'aoReq' asks for it now (the
   spec's order: its address depends on the special cycle's work), and
   'AFetch' follows.
4. The CPU's access, if the cycle is its: 'ARead', 'AWrite', and before a
   read 'ARdy' (WSYNC: 'aoRdy' 'False' holds the read).
5. 'AEnd' (the cycle's bus value): virtual fetches capture it, and the
   display decodes one position; 'ADecode' a second (the hardware's
   budget, two a cycle) and gives the cycle's AN codes ('aoCodes').

Player/missile data for GTIA comes out as 'aoPm' (phantom data on
'ABegin', fetched data on 'APost'); the NMI line is 'aoNmi' throughout.

__Absolute cycles__, which the spec uses for WSYNC and CHBASE's delay,
are counters here: CHBASE counts the cycle starts after its write; WSYNC
counts the cycles since its arm ('sWsAge', from -2 at the write) and the
ANTIC cycles in a row before this one ('sDmaRun': the spec's "last CPU
cycle"), and holds until a cycle start at cycle 105.
-}
module A800.Antic
  ( AnticIn (..)
  , AnticOut (..)
  , BusUse (..)
  , PmOut (..)
  , Antic (..)
  , anticInit
  , anticStep
  ) where

import Clash.Prelude

data AnticIn
  = AReset
  | ABegin (Unsigned 8)              -- ^ the last cycle's bus value
  | AFetch (Unsigned 8)              -- ^ the data read for 'aoReq'
  | APost
  | ARead (Unsigned 4)
  | AWrite (Unsigned 4) (Unsigned 8)
  | ARdy
  | AEnd (Unsigned 8)                -- ^ the cycle's bus value
  | ADecode
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFDataX

data BusUse = BusCpu | BusFetch | BusRefresh
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFDataX

-- | Player/missile data for GTIA: odd line, (player), data.
data PmOut = PmNone | PmMissile Bool (Unsigned 8) | PmPlayer Bool (Index 4) (Unsigned 8)
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFDataX

data AnticOut = AnticOut
  { aoBus   :: BusUse                    -- ^ after 'ABegin'
  , aoReq   :: Maybe (Unsigned 16)       -- ^ a fetch to do now
  , aoPm    :: PmOut
  , aoRead  :: Unsigned 8
  , aoRdy   :: Bool
  , aoCodes :: (Unsigned 3, Unsigned 3)  -- ^ after 'ADecode'
  , aoNmi   :: Bool
  , aoY     :: Unsigned 9
  , aoX     :: Unsigned 7
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFDataX

data Antic = Antic
  { sX, sDispStart, sDispEnd, sLastCheck :: Unsigned 7
  , sY :: Unsigned 9
  , sDmactl, sChactl, sHscrol, sVscrol, sPmbase, sChbase, sChbaseNew :: Unsigned 8
  , sChbaseCnt :: Unsigned 2          -- ^ cycle starts until CHBASE's write lands (0: none)
  , sDlist, sDlistLatch, sMsc, sCharBase :: Unsigned 16
  , sNmien, sNmist, sPending, sEarlyNmien :: Unsigned 8
  , sLateNmi, sNmiLine :: Bool
  , sWsAge :: Maybe (Signed 8)        -- ^ cycles since WSYNC's arm (negative: not yet)
  , sWsHold, sWsPassed :: Bool        -- ^ holding; cycle 105 has begun since
  , sDmaRun :: Unsigned 7             -- ^ ANTIC cycles in a row before this one
  , sLastBus :: BusUse
  , sIr, sIrPrev :: Unsigned 8
  , sRow, sRowLast, sVsLatch, sVsLatch2 :: Unsigned 4
  , sRowStopVs, sDlActive, sExtra, sDlInTime :: Bool
  , sDlNext :: Unsigned 8
  , sPfFirst, sPfActive, sHsOn, sHsDelay :: Bool
  , sHsOff :: Unsigned 3
  , sLStart, sLEnd, sLVEnd :: Unsigned 8 -- ^ latched edges (0: not latched)
  , sAbn, sEnding :: Unsigned 8
  , sWr, sNameRd, sDecRd :: Unsigned 6
  , sCharWr, sDecChar :: Unsigned 7
  , sShift, sAttr :: Unsigned 8
  , sDecPos :: Unsigned 8
  , sPreStart, sHires, sRefresh :: Bool
  , sTicks :: BitVector 4             -- ^ 0 graphic, 1 char data, 2/3 the same virtual
  , sMissLate, sPhantomPm, sPhantomPmOn, sPhantomPl, sVblankBug :: Bool
  , sL1 :: Unsigned 2                 -- ^ cycle 1's branch: 0 the mode line goes on, 1 a new one, 2 no display list
  , sSpAddr :: Maybe (Unsigned 16)    -- ^ the special fetch of this cycle
  , sReq :: Bool                      -- ^ a fetch is asked for
  , sData :: Unsigned 8               -- ^ the last fetch's data
  , sLine :: Vec 48 (Unsigned 8)
  , sChars :: Vec 64 (Unsigned 8)
  , sDisp :: Vec 128 (Unsigned 8)
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFDataX

-- | Power on (the spec's 'newAntic').
anticInit :: Antic
anticInit = Antic
  { sX = 0, sDispStart = 110, sDispEnd = 110, sLastCheck = 0, sY = 0
  , sDmactl = 0, sChactl = 0, sHscrol = 0, sVscrol = 0, sPmbase = 0, sChbase = 0, sChbaseNew = 0
  , sChbaseCnt = 0, sDlist = 0, sDlistLatch = 0, sMsc = 0, sCharBase = 0
  , sNmien = 0, sNmist = 0x7F, sPending = 0, sEarlyNmien = 0, sLateNmi = False, sNmiLine = False
  , sWsAge = Nothing, sWsHold = False, sWsPassed = False, sDmaRun = 1, sLastBus = BusCpu
  , sIr = 0, sIrPrev = 0, sRow = 0, sRowLast = 0, sVsLatch = 0, sVsLatch2 = 0
  , sRowStopVs = False, sDlActive = False, sExtra = False, sDlInTime = False, sDlNext = 0
  , sPfFirst = False, sPfActive = False, sHsOn = False, sHsDelay = False, sHsOff = 0
  , sLStart = 0, sLEnd = 0, sLVEnd = 0, sAbn = 0, sEnding = 0
  , sWr = 0, sNameRd = 0, sDecRd = 0, sCharWr = 0, sDecChar = 0, sShift = 0, sAttr = 0, sDecPos = 0
  , sPreStart = False, sHires = False, sRefresh = False, sTicks = 0
  , sMissLate = False, sPhantomPm = False, sPhantomPmOn = False, sPhantomPl = False, sVblankBug = False
  , sL1 = 0, sSpAddr = Nothing, sReq = False, sData = 0
  , sLine = repeat 0, sChars = repeat 0, sDisp = repeat 0 }

-- * Tables

-- | Fetch rate: 0 none, 1 slow (8), 2 medium (4), 3 fast (2).
fetchRate :: Unsigned 4 -> Unsigned 2
fetchRate m
  | m < 2 = 0
  | m <= 5 = 3
  | m <= 7 = 2
  | m <= 9 = 1
  | m <= 12 = 2
  | otherwise = 3

clockPat :: Unsigned 2 -> Unsigned 8 -> Unsigned 8
clockPat rate o = case rate of
  1 -> bit (fromIntegral (o .&. 7))
  2 -> 0x11 `shiftL` fromIntegral (o .&. 3)
  3 -> 0x55 `shiftL` fromIntegral (o .&. 1)
  _ -> 0

-- | The mode line's last row (rows - 1).
rowLastOf :: Unsigned 8 -> Unsigned 4 -> Unsigned 4
rowLastOf ir m = case m of
  0 -> resize ((ir `shiftR` 4) .&. 7)
  2 -> 7
  3 -> 9
  4 -> 7
  5 -> 15
  6 -> 7
  7 -> 15
  8 -> 7
  9 -> 3
  10 -> 3
  11 -> 1
  13 -> 1
  _ -> 0

isChar :: Unsigned 4 -> Bool
isChar m = m >= 2 && m <= 7

modeOf :: Antic -> Unsigned 4
modeOf s = resize (sIr s)

rotl8, rotr8 :: Unsigned 8 -> Int -> Unsigned 8
rotl8 = rotateL
rotr8 = rotateR

visible :: Antic -> Bool
visible s = sY s >= 8 && sY s < 248

oddLine :: Antic -> Bool
oddLine s = testBit (sY s) 0

wrap63 :: Unsigned 6 -> Unsigned 6
wrap63 r = if r >= 62 then 0 else r + 1

dlInc :: Unsigned 16 -> Unsigned 16
dlInc d = (d .&. 0xFC00) .|. ((d + 1) .&. 0x03FF)

lineAt :: Antic -> Unsigned 6 -> Unsigned 8
lineAt s i = if i >= 48 then 0xFF else sLine s !! i

-- * Edges

-- | The playfield's start (name-fetch cycle), stop (capped at 106) and
-- uncapped stop.
edges :: Antic -> (Unsigned 8, Unsigned 8, Unsigned 8)
edges s =
  let m = modeOf s
      width = sDmactl s .&. 3
      fw | sHsOn s && width /= 0 && width /= 3 = width + 1
         | otherwise = width
      (s0, w) = case fw of
        1 -> (26, 64)
        2 -> (18, 80)
        3 -> (10, 96)
        _ -> (114, 0)
      st = if m >= 2 then s0 + resize (sHsOff s) else 114
      e = if m >= 2 then st + w else 114
      start = if sLStart s /= 0 then sLStart s else st
      end = min 106 (if sLEnd s /= 0 then sLEnd s else e)
      vend = if sLVEnd s /= 0 then sLVEnd s else e
  in (start, end, vend)

latchEdges :: Unsigned 7 -> Antic -> Antic
latchEdges x s
  | sDmactl s .&. 3 == 0 = s { sLastCheck = x }
  | otherwise =
      let m = modeOf s
          (start, _, vend) = edges s
          off = if m < 8 then 1 else 3
          g = if m >= 8 then 2 else 0
          passed e = (e + g - off) >= resize (sLastCheck s) && (e + g - off) <= resize x
          cap = min 106
          s1 = if sLStart s == 0 && start < 114 && passed start then s { sLStart = start } else s
          s2 = if sLEnd s == 0 && vend < 122 && passed (cap vend) then s1 { sLEnd = cap vend } else s1
          s3 = if sLVEnd s == 0 && vend < 122 && passed vend then s2 { sLVEnd = vend } else s2
      in s3 { sLastCheck = x }

-- | This cycle's playfield requests (see 'sTicks').
playfieldTicks :: Antic -> BitVector 4
playfieldTicks s
  | not (sPfActive s) || rate == 0 = 0
  | otherwise = bits g 1 .|. bits c 2
 where
  m = modeOf s
  rate = fetchRate m
  x = resize (sX s) :: Unsigned 8
  (start, _, vend) = edges s
  abn = sAbn s
  gStart = start + (if m >= 8 then 2 else 0)
  gVEnd = vend + (if m >= 8 then 2 else 0)
  gAbn = if m >= 8 then rotl8 abn 2 else abn
  tickAt st e ab p
    | st >= e = False
    | p < st = testBit ab (fromIntegral (p .&. 7))
    | p < e = testBit (ab .|. clockPat rate st) (fromIntegral (p .&. 7))
    | otherwise = testBit ((ab .|. clockPat rate st) .&. complement (clockPat rate e)) (fromIntegral (p .&. 7))
  g = sPfFirst s && tickAt gStart gVEnd gAbn x
  c = isChar m && tickAt (start + 3) (vend + 3) (rotl8 abn 3) x
  virtualAt = x >= 106 || x == 0 || sDmactl s .&. 3 == 0
  bits b v = if b then (if virtualAt then shiftL v 2 else v) else 0

-- | The playfield fetch's address (the AND of a name/bitmap and a
-- character data fetch if both).
pfAddress :: Antic -> Unsigned 16
pfAddress s =
  let pf = sTicks s
      name = lineAt s (sNameRd s)
      mask = if modeOf s >= 6 then 0x3F else 0x7F
      gAddr = sMsc s
      cAddr = sCharBase s + (resize (name .&. mask) `shiftL` 3)
  in case (testBit pf 0 || testBit pf 2, testBit pf 1 || testBit pf 3) of
       (True, True) -> gAddr .&. cAddr
       (True, False) -> gAddr
       _ -> cAddr

-- | Graphic (bit 0) and character (bit 1) fetches with this data.
completeFetches :: BitVector 2 -> Unsigned 8 -> Antic -> Antic
completeFetches kinds v s =
  let w = sWr s
      s1 = if testBit kinds 0
        then s { sLine = if w < 48 then replace w v (sLine s) else sLine s
               , sWr = wrap63 w
               , sMsc = (sMsc s .&. 0xF000) .|. ((sMsc s + 1) .&. 0x0FFF) }
        else s
      cw = sCharWr s1
  in if testBit kinds 1
       then s1 { sChars = if cw < 64 then replace cw v (sChars s1) else sChars s1
               , sCharWr = satSucc SatBound cw
               , sNameRd = wrap63 (sNameRd s1) }
       else s1

updateCharRow :: Antic -> Antic
updateCharRow s =
  let m = modeOf s
      flipR = if testBit (sChactl s) 2 then 7 else 0
      r = case m of
        5 -> (sRow s `shiftR` 1) .&. 7
        7 -> (sRow s `shiftR` 1) .&. 7
        _ -> sRow s .&. 7
      base | m == 6 || m == 7 = resize (sChbase s .&. 0xFE) `shiftL` 8
           | otherwise = resize (sChbase s .&. 0xFC) `shiftL` 8
  in s { sCharBase = base + resize (r `xor` flipR) }

updateDisplayRegion :: Antic -> Antic
updateDisplayRegion s =
  let (a, b) = case sDmactl s .&. 3 of
        1 -> (32, 96)
        2 -> (24, 104)
        3 -> (22, 112)
        _ -> (110, 110)
  in s { sDispStart = a, sDispEnd = b }

rowStop :: Antic -> Unsigned 4 -> Unsigned 4
rowStop s latch = if sRowStopVs s then latch else sRowLast s

-- | After a DMACTL or HSCROL change.
recheckActive :: Antic -> Antic
recheckActive s
  | sX s < 1 = s
  | otherwise =
      let m = modeOf s
          (start, _, _) = edges s
          lim = (start + (if m >= 8 then 2 else 0)) - (if m < 8 then 2 else 4)
          ok = sLStart s /= 0 || resize (sX s) <= max 10 lim
      in s { sPfActive = ok }

-- * The cycle

endLine :: Antic -> Antic
endLine s0 =
  let y = resize (sY s0) + 1 :: Unsigned 10
      wrapped = y >= 262
      s1 = s0 { sX = 0, sNmiLine = False
              , sY = if wrapped then 0 else resize y
              , sDlActive = not wrapped && sDlActive s0 }
      s2 = if y >= 248 && y < 262 then s1 { sDlActive = False, sExtra = False, sAbn = 0 } else s1
      m = modeOf s2
      rate = fetchRate m
      (start, _, vend) = edges s2
      danger = fromIntegral vend - (16 `shiftR` fromIntegral rate) - 109 :: Signed 10
      ending = if vend > start && rate > 0 && danger >= 0
        then (clockPat rate 7 `shiftR` fromIntegral (4 - danger)) .&. 0x38 else 0
      abn1 = if start < 114 then sAbn s2 .|. clockPat rate start else sAbn s2
      abn2 = if sLVEnd s2 /= 0 || sDmactl s2 .&. 3 /= 0 then abn1 .&. complement (clockPat rate vend) else abn1
      s3 | y < 248 = s2 { sEnding = if m >= 2 then ending else 0
                        , sAbn = if m >= 2 then rotr8 abn2 2 else sAbn s2 }
         | otherwise = s2
      y' = sY s3
  in s3 { sPhantomPm = sDmactl s3 .&. 0x2C == 0x20 && y' >= 8 && y' < 248
        , sPhantomPmOn = False, sPhantomPl = False
        , sWr = 0, sNameRd = 0, sCharWr = 0, sDecRd = 0, sDecChar = 0 }

-- | A cycle start: the beam, CHBASE, edges, requests, the special cycle's
-- work up to its fetch.
begin :: Unsigned 8 -> Antic -> (Antic, BusUse, PmOut)
begin bus s0 =
  let s1 = if sX s0 + 1 >= 114 then endLine s0 else s0 { sX = sX s0 + 1 }
      x = sX s1
      -- WSYNC's counters
      s2 = s1 { sWsAge = fmap (satSucc SatBound) (sWsAge s1)
              , sWsPassed = sWsPassed s1 || (sWsHold s1 && x == 105)
              , sDmaRun = if sLastBus s1 == BusCpu then 0 else satSucc SatBound (sDmaRun s1)
              , sChbaseCnt = if sChbaseCnt s1 /= 0 then sChbaseCnt s1 - 1 else 0 }
      s3 = if sChbaseCnt s1 == 1 then updateCharRow s2 { sChbase = sChbaseNew s2 } else s2
      s4 = latchEdges x s3
      s5 = s4 { sTicks = playfieldTicks s4, sSpAddr = Nothing
              , sRefresh = sRefresh s4 || (x >= 25 && x <= 57 && (x - 25) .&. 3 == 0) }
      (s6, pm) = specialPre bus s5
      took = isJust (sSpAddr s6)
      realPf = sTicks s6 .&. 3 /= 0
      use | took || realPf = BusFetch
          | sRefresh s6 = BusRefresh
          | otherwise = BusCpu
      s7 = s6 { sRefresh = sRefresh s6 && use /= BusRefresh, sReq = took, sLastBus = use }
  in (s7, use, pm)
 where
  isJust = maybe False (const True)

-- | The special cycles' work before their fetch (and all of it for the
-- cycles that never fetch).
specialPre :: Unsigned 8 -> Antic -> (Antic, PmOut)
specialPre bus s = case sX s of
  0 ->
    let dmactl = sDmactl s
        took = (sMissLate s || dmactl .&. 0x0C /= 0) && visible s
        pmb = resize (sPmbase s) :: Unsigned 16
        y = resize (sY s) :: Unsigned 16
        addr | testBit dmactl 4 = ((pmb .&. 0xF8) `shiftL` 8) + 0x300 + y
             | otherwise = ((pmb .&. 0xFC) `shiftL` 8) + 0x180 + (y `shiftR` 1)
        s1 = if took then s { sMissLate = False, sSpAddr = Just addr } else s
    in (s1 { sDlInTime = testBit dmactl 5, sDlistLatch = sDlist s }, PmNone)
  1 -> (line1Pre s, PmNone)
  x | x >= 2 && x <= 5 ->
    let n = resize (x - 2) :: Unsigned 16
        pmb = resize (sPmbase s) :: Unsigned 16
        y = resize (sY s) :: Unsigned 16
        addr | testBit (sDmactl s) 4 = ((pmb .&. 0xF8) `shiftL` 8) + 0x400 + 0x100 * n + y
             | otherwise = ((pmb .&. 0xFC) `shiftL` 8) + 0x200 + 0x80 * n + (y `shiftR` 1)
        pm | sPhantomPmOn s && x > 3 = PmPlayer (oddLine s) (fromIntegral (x - 4)) bus
           | sPhantomPl s && x > 2 = PmPlayer (oddLine s) (fromIntegral (x - 3)) bus
           | otherwise = PmNone
    in if testBit (sDmactl s) 3 && visible s then (s { sSpAddr = Just addr }, PmNone) else (s, pm)
  6 ->
    let pm | sPhantomPmOn s = PmPlayer (oddLine s) 2 bus
           | sPhantomPl s = PmPlayer (oddLine s) 3 bus
           | otherwise = PmNone
    in (addrFetch s { sVsLatch2 = sVscrol' s }, pm)
  7 ->
    let pm = if sPhantomPmOn s then PmPlayer (oddLine s) 3 bus else PmNone
        s1 = addrFetch s
        -- without a fetch, the rest of the cycle (pointer restart) comes
        -- before the playfield's fetch of this cycle
    in (s1, pm)
  8 ->
    let p = sPending s
        early = sEarlyNmien s
        en = sNmien s
    in ( s { sLateNmi = p .&. early == 0 && p .&. en .&. complement early /= 0
           , sNmiLine = sNmiLine s || p .&. early /= 0 }
       , PmNone )
  9 -> (s { sNmiLine = sNmiLine s || sLateNmi s }, PmNone)
  10 ->
    ( s { sDisp = repeat 0, sShift = 0, sAttr = 0, sPreStart = False, sDecPos = 1
        , sVblankBug = not (visible s) && sHires s && sDmactl s .&. 3 /= 0 }
    , PmNone )
  112 ->
    let stop = rowStop s (sVsLatch s)
    in if sRow s == stop && not (sPfFirst s) then (s { sPfFirst = True, sWr = 48 }, PmNone) else (s, PmNone)
  _ -> (s, PmNone)
 where
  addrFetch q = if sExtra q && testBit (sDmactl q) 5 then q { sSpAddr = Just (sDlist q) } else q
  sVscrol' = resize . sVscrol

line1Pre :: Antic -> Antic
line1Pre s0 =
  let s1 = s0 { sPfFirst = False, sPfActive = False, sLStart = 0, sLEnd = 0, sLVEnd = 0, sLastCheck = 0 }
      s2 = if sY s1 == 8
        then s1 { sDlActive = True, sRow = 0, sRowLast = 0, sRowStopVs = False, sIr = sIrPrev s1 }
        else s1
      stop = rowStop s2 (sVsLatch s2)
      s3 = s2 { sVsLatch = resize (sVscrol s2) }
      row = sRow s3
  in if row /= stop
       then s3 { sRow = row + 1, sPfActive = True, sExtra = sExtra s3 && modeOf s3 == 1, sL1 = 0 }
       else
         let s4 = s3 { sRow = 0, sRowStopVs = False }
         in if not (sDlActive s4) then s4 { sL1 = 2 } else
              let s5 = s4 { sExtra = False, sIrPrev = sIr s4, sL1 = 1 }
              in if sDlInTime s5 then s5 { sSpAddr = Just (sDlistLatch s5) } else s5

-- | The rest of the special cycle: @v@ is the special fetch's data.
specialPost :: Unsigned 8 -> Antic -> (Antic, PmOut)
specialPost v s = case sX s of
  0 | took -> (s { sPhantomPl = sPhantomPl s || not (testBit (sDmactl s) 3) }, PmMissile (oddLine s) v)
  1 ->
    let (s1, pm) = if took
          then ( s { sIr = v, sDlist = dlInc (sDlist s), sPhantomPmOn = sPhantomPmOn s || sPhantomPm s }
               , if sPhantomPm s then PmMissile (oddLine s) v else PmNone )
          else (s, PmNone)
        s2 = if sL1 s1 == 1 then newModeLine s1 else s1
        hsOn = sHsOn s2
        s3 = s2 { sHsOff = if hsOn then resize ((sHscrol s2 .&. 14) `shiftR` 1) else 0
                , sHsDelay = hsOn && testBit (sHscrol s2) 0 }
    in (updateDisplayRegion (updateCharRow s3) { sShift = 0, sPfActive = True }, pm)
  x | x >= 2 && x <= 5 && took -> (s, PmPlayer (oddLine s) (fromIntegral (x - 2)) v)
  6 | took -> (s { sDlNext = v, sDlist = dlInc (sDlist s) }, PmNone)
  7 ->
    let s1 = if took then addr7 s { sDlist = dlInc (sDlist s) } else s
        s2 = nmi7 s1
    in (s2 { sWr = 0, sNameRd = 0, sCharWr = 0, sDecRd = 0, sDecChar = 0 }, PmNone)
  _ -> (s, PmNone)
 where
  took = maybe False (const True) (sSpAddr s)
  addr7 q =
    let addr = resize (sDlNext q) .|. (resize v `shiftL` 8) :: Unsigned 16
        ir = sIr q
    in if ir .&. 15 == 1
         then if testBit ir 6
           then q { sDlist = addr, sDlActive = False, sExtra = False, sIr = ir .&. complement 0x4F, sRowLast = 0 }
           else q { sDlist = addr }
         else q { sMsc = addr, sExtra = False }
  nmi7 q =
    let q1 = q { sEarlyNmien = sNmien q }
    in if sY q1 == 248
         then q1 { sPending = 0x40, sNmist = (sNmist q1 .|. 0x40) .&. complement 0x80
                 , sIrPrev = sIr q1, sIr = sIr q1 .&. 0x20 }
         else
           let stop = rowStop q1 (sVsLatch2 q1)
           in if testBit (sIr q1) 7 && sRow q1 == stop
                then q1 { sPending = 0x80, sNmist = (sNmist q1 .&. complement 0x40) .|. 0x80 }
                else q1 { sPending = 0 }

newModeLine :: Antic -> Antic
newModeLine s =
  let ir = sIr s
      m = modeOf s
      abn = sAbn s
      abn' = if m < 2 then 0 else case fetchRate m of
        1 -> abn .|. sEnding s
        2 -> (abn .&. 15) * 0x11
        _ -> (abn .&. 3) * 0x55
      vsOf w = if w .&. 15 < 2 then 0 else w
      cur = vsOf ir
      prv = vsOf (sIrPrev s)
      vsChange = (cur `xor` prv) .&. 0x20 /= 0
      s1 = s { sExtra = sExtra s || m == 1 || (m >= 2 && testBit ir 6)
             , sRow = 0, sRowLast = rowLastOf ir m
             , sHires = m == 2 || m == 3 || m == 15
             , sAbn = abn' }
      s2 | vsChange && testBit cur 5 = s1 { sRow = resize (sVscrol s1) }
         | vsChange = s1 { sRowStopVs = True }
         | otherwise = s1
  in s2 { sHsOn = m /= 1 && testBit cur 4, sPfFirst = True, sPfActive = True }

-- * Display

-- | Decode one playfield position (the spec's 'decode', one step of its
-- budget).
decodeOne :: Antic -> Antic
decodeOne s
  | sX s < 10 || not (sPfActive s) || rate == 0 = s
  | p > target || p >= e = s
  | otherwise =
      let s1 = if p == st && sPreStart s then s { sDecRd = if sDecRd s == 0 then 62 else sDecRd s - 1, sPreStart = False } else s
          s2 = if p < st then s1 { sPreStart = True } else s1
          s3 = decodeStep p (tick p) (testBit pat' (fromIntegral (p .&. 1))) s2
          s4 = if incAt then s3 { sDecRd = wrap63 (sDecRd s3) } else s3
      in s4 { sDecPos = p + 1 }
 where
  m = modeOf s
  rate = fetchRate m
  x = resize (sX s) :: Unsigned 8
  target = if isChar m then x - 3 else x
  (start, _, vend) = edges s
  st = start + (if m >= 8 then 2 else 0)
  e = vend + (if m >= 8 then 2 else 0)
  ab = if m >= 8 then rotl8 (sAbn s) 2 else sAbn s
  pat = ab .|. clockPat rate st
  pat' = (if pat .&. 0x55 /= 0 then 1 else 0) .|. (if pat .&. 0xAA /= 0 then 2 else 0) :: Unsigned 2
  tick q
    | st >= e = False
    | q < st = testBit ab (fromIntegral (q .&. 7))
    | q < e = testBit pat (fromIntegral (q .&. 7))
    | otherwise = False
  p = if ab == 0 then max (sDecPos s) st else sDecPos s
  incAt
    | p < st && p < 5 = False
    | p + 2 >= e = testBit (pat .&. complement (clockPat rate st)) (fromIntegral ((p + 2) .&. 7))
    | otherwise = tick (p + 2)

decodeStep :: Unsigned 8 -> Bool -> Bool -> Antic -> Antic
decodeStep p load parity8 s =
  let m = modeOf s
      chactl = sChactl s
      row = sRow s
      sh0 = sShift s
      sh1 | m == 8 = if parity8 then sh0 `shiftL` 2 else sh0
          | otherwise = sh0 `shiftL` shiftBy
      shiftBy = case m of
        8 -> 0
        9 -> 1
        6 -> 2
        7 -> 2
        10 -> 2
        11 -> 2
        12 -> 2
        _ -> 4
      c = lineAt s (sDecRd s)
      ci = sDecChar s
      d0 = if ci < 64 then sChars s !! ci else 0
      hi = testBit c 7
      blinkOk = m > 3 || not (testBit chactl 0) || not hi
      d1 = if blinkOk then d0 else 0
      d = case m of
        2 | row .&. 14 == 8 && c .&. 0x60 /= 0x60 -> 0
        3 -> let base = if row >= 2 then 0xFF else 0
                 msk = if row .&. 6 == 0 && c .&. 0x60 /= 0x60 then base `xor` 0xFF else base
             in d1 .&. msk
        _ -> d1
      attrNew
        | m <= 3 = if hi && testBit chactl 1 then 0xFF else 0
        | m <= 5 = if hi then 1 else 0
        | otherwise = c `shiftR` 6
      (sh2, attr, s1)
        | not load = (sh1, sAttr s, s)
        | isChar m = (sh1 .|. d, attrNew, s { sDecChar = satSucc SatBound ci, sAttr = attrNew })
        | otherwise = (sh1 .|. c, sAttr s, s)
      lores2 v alt = case v .&. 3 of
        0 -> 0
        1 -> 1
        2 -> 2
        _ -> if alt then 8 else 4 :: Unsigned 8
      out | m <= 3 = ((sh2 `xor` attr) `shiftR` 4) .&. 15
          | m <= 5 = lores2 (sh2 `shiftR` 6) (attr /= 0) * 16 + lores2 ((sh2 `shiftR` 4) .&. 3) (attr /= 0)
          | m <= 7 = let col = bit (fromIntegral (attr .&. 3)) :: Unsigned 8
                     in (if testBit sh2 7 then col else 0) * 16 + (if testBit sh2 6 then col else 0)
          | m == 8 || m == 10 = let v = lores2 (sh2 `shiftR` 6) False in v * 16 + v
          | m == 9 = if testBit sh2 7 then 0x11 else 0
          | m <= 12 = (if testBit sh2 7 then 16 else 0) + (if testBit sh2 6 then 1 else 0)
          | m <= 14 = lores2 (sh2 `shiftR` 6) False * 16 + lores2 ((sh2 `shiftR` 4) .&. 3) False
          | otherwise = (sh2 `shiftR` 4) .&. 15
      dst = p + (if isChar m then 6 else 4)
  in s1 { sShift = sh2, sDisp = if dst < 128 then replace dst out (sDisp s1) else sDisp s1 }

-- | The cycle's two AN codes (HRM table 17).
anCodes :: Antic -> (Unsigned 3, Unsigned 3)
anCodes s
  | vblank && not (sVblankBug s) = if y >= 251 && y <= 253 && x >= 17 && x < 111 then (1, 1) else (hblank, hblank)
  | x < 17 || x >= 111 = (hblank, hblank)
  | x < sDispStart s || x >= sDispEnd s || sDmactl s .&. 3 == 0 = (0, 0)
  | otherwise =
      let on = sPfActive s && not vblank
          cur = if on then sDisp s !! x else 0
          prev = if on && x > 0 then sDisp s !! (x - 1) else 0
          v | sHsDelay s && sHires s = ((prev `shiftL` 2) .|. (cur `shiftR` 2)) .&. 15
            | sHsDelay s = ((prev .&. 15) `shiftL` 4) .|. (cur `shiftR` 4)
            | otherwise = cur
      in if sHires s
           then (4 .|. resize ((v `shiftR` 2) .&. 3), 4 .|. resize (v .&. 3))
           else (pfCode (v `shiftR` 4), pfCode (v .&. 15))
 where
  x = sX s
  y = sY s
  hblank = if sHires s then 3 else 2
  vblank = y < 8 || y >= 248
  pfCode n
    | testBit n 3 = 7
    | testBit n 2 = 6
    | testBit n 1 = 5
    | testBit n 0 = 4
    | otherwise = 0

-- * Registers

readReg :: Antic -> Unsigned 4 -> Unsigned 8
readReg s r = case r of
  0x0B ->
    let x = sX s
        y = sY s
        y' | x >= 111 = if x >= 112 && y + 1 >= 262 then 0 else y + 1
           | otherwise = y
    in resize (y' `shiftR` 1)
  0x0C -> 0     -- PENH, PENV: no light pen
  0x0D -> 0xFF
  0x0F -> sNmist s
  _ -> 0xFF

writeReg :: Unsigned 4 -> Unsigned 8 -> Antic -> Antic
writeReg r v s = case r of
  0x00 ->
    let nv = v .&. 0x3F
        old = sDmactl s
        x = sX s
        y = sY s
        s1 = latchEdges x s
        late = x == 113 && nv .&. 0x0C == 0 && old .&. 0x0C /= 0 && y >= 7 && y < 247
    in if nv == old then s else
         recheckActive (updateDisplayRegion s1 { sDmactl = nv, sMissLate = sMissLate s1 || late })
  0x01 -> updateCharRow s { sChactl = v .&. 7 }
  0x02 -> s { sDlist = (sDlist s .&. 0xFF00) .|. resize v }
  0x03 -> s { sDlist = (sDlist s .&. 0x00FF) .|. (resize v `shiftL` 8) }
  0x04 ->
    let nv = v .&. 0x0F
        old = sHscrol s
        ch = nv `xor` old
        s1 = if ch .&. 1 /= 0 then s { sHsDelay = testBit nv 0 } else s
        s2 = if ch .&. 14 /= 0 then recheckActive (latchEdges (sX s1) s1) { sHsOff = resize (nv `shiftR` 1) } else s1
        s3 = if nv /= old && sHsOn s then s2 else s
    in s3 { sHscrol = nv }
  0x05 ->
    let x = sX s
    in s { sVscrol = v .&. 0x0F, sVsLatch = if x >= 1 && x < 109 then resize v else sVsLatch s }
  0x07 -> s { sPmbase = v .&. 0xFC }
  0x09 -> s { sChbaseNew = v, sChbaseCnt = 2 }
  0x0A ->
    let rearm = case sWsAge s of
          Nothing -> True
          Just a -> a >= -1 && sX s == 104
    in if not (sWsHold s) && rearm then s { sWsAge = Just (-2) } else s
  0x0E -> s { sNmien = v .&. 0xC0 }
  0x0F -> s { sNmist = 0x1F .|. (if sX s == 7 then sPending s else 0) }
  _ -> s

-- | WSYNC before a CPU read: 'True' holds it.
rdy :: Antic -> (Antic, Bool)
rdy s
  | sWsHold s = if sWsPassed s then (s { sWsHold = False }, False) else (s, True)
  | otherwise = case sWsAge s of
      Just age | age >= 0 ->
        let d = min (resize age) (fromIntegral (sDmaRun s)) :: Signed 10
            xs = fromIntegral (sX s) - d
            r0 = 105 - xs
            toRel | r0 < 0 = r0 + 114
                  | r0 >= 114 = r0 - 114
                  | otherwise = r0
            hold = d < toRel
        in (s { sWsAge = Nothing, sWsHold = hold, sWsPassed = False }, hold)
      _ -> (s, False)

-- * The step

anticStep :: Antic -> AnticIn -> (Antic, AnticOut)
anticStep s0 i =
  let -- one fetch completion for every tick that has one
      (kinds, fdata) = case i of
        AFetch v -> (slice d1 d0 (sTicks s0), v)
        AEnd b -> (slice d3 d2 (sTicks s0), b)
        _ -> (0, 0)
      sF = completeFetches kinds fdata s0
      sF' = case i of
        AFetch v -> sF { sTicks = sTicks sF .&. 0xC, sReq = False, sData = v }
        AEnd _ -> sF { sTicks = 0 }
        _ -> sF
      (s1, use, pm, rd, ok) = case i of
        AReset -> (resetState sF', BusCpu, PmNone, 0, True)
        ABegin b -> let (q, u, p) = begin b sF' in (q, u, p, 0, True)
        APost ->
          let (q, p) = specialPost (sData sF') sF'
              took = maybe False (const True) (sSpAddr sF')
          in (q { sSpAddr = Nothing, sReq = not took && sTicks q .&. 3 /= 0 }, sLastBus sF', p, 0, True)
        ARead r -> (sF', sLastBus sF', PmNone, readReg sF' r, True)
        AWrite r v -> (writeReg r v sF', sLastBus sF', PmNone, 0, True)
        ARdy -> let (q, h) = rdy sF' in (q, sLastBus sF', PmNone, 0, not h)
        _ -> (sF', sLastBus sF', PmNone, 0, True)
      s2 = case i of
        AEnd _ -> decodeOne s1
        ADecode -> decodeOne s1
        _ -> s1
      req = if sReq s2 then Just (reqAddr s2) else Nothing
  in ( s2
     , AnticOut { aoBus = use, aoReq = req, aoPm = pm, aoRead = rd, aoRdy = ok
                , aoCodes = anCodes s2, aoNmi = sNmiLine s2, aoY = sY s2, aoX = sX s2 } )
 where
  reqAddr q = case sSpAddr q of
    Just a | sTicks q .&. 3 /= 0 -> a .&. pfAddress q
           | otherwise -> a
    Nothing -> pfAddress q
  resetState q = q { sX = 0, sY = 0, sDmactl = 0, sNmien = 0, sNmiLine = False, sPending = 0
                   , sLateNmi = False, sAbn = 0, sEnding = 0, sExtra = False
                   , sWsAge = Nothing, sWsHold = False }
