{-# LANGUAGE StrictData #-}
{-|
The 2C02 PPU in Clash (PLAN.md step 3.5). It reproduces the reference
model ("Nes.Spec.Ppu") dot for dot; that module has the rules (fetch
pattern, scroll increments, sprite evaluation, sprite units, VBlank and
NMI, register quirks).

__Ticks.__ 'ppuTick' does one of three things ('Tick'): a dot, a CPU read
of a register, or a CPU write. Each is one clock of combinational logic,
so a dot takes one clock (the platform has about 4.7 clocks per dot); the
machine ("Nes.Core") gives the CPU's register access its own clock
between dots, where the spec has it.

__Memories__ ('PpuIn' / 'PpuOut'):

* Port A, rendering fetches (pattern tables and nametables, through the
  cartridge): the address goes out on the first dot of each 2-dot fetch
  and the byte comes back with the next tick ('piVramA'), where it is put
  away ('pPend'). The spec reads on that same first dot.
* Port B, the CPU's $2007 accesses: writes go out at once; a read's byte
  (for the read buffer) comes back with the next tick ('piVramB').
* OAM, 256 bytes in block RAM ('oamRam') whose read address is OAMADDR
  ('poOamAddr'): 'piOam' is always the byte at OAMADDR as it was before
  this tick. Writes come out as 'poOamWrite', and OAM corruption's row
  copy as 'poOamRow' (all 8 bytes of a row in one write: 'oamRam' keeps
  one RAM per byte of a row; row 0's bytes are shadowed in 'pRow0').

Secondary OAM, the sprite units and palette RAM are registers.

__The I/O latch__ decays per bit as in the spec: a bit driven by an
access lives for 30 VBlank starts ('pDecay').
-}
module Nes.Ppu
  ( Tick (..)
  , PpuIn (..)
  , PpuOut (..)
  , Fetch (..)
  , Pend (..)
  , Unit (..)
  , Ppu (..)
  , ppuInit
  , ppuTick
  , ppuSettle
  , ppuNmiOut
  , ppu
  , oamRam
  ) where

import Clash.Prelude

data Tick
  = TDot
  | TRead (Index 8)
  | TWrite (Index 8) (Unsigned 8)
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFDataX

data PpuIn = PpuIn
  { piVramA :: Unsigned 8 -- ^ port A: the byte at the address of the last port A read
  , piVramB :: Unsigned 8 -- ^ port B: the byte at the address of the last port B read
  , piOam   :: Unsigned 8 -- ^ OAM at OAMADDR (as before this tick)
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFDataX

data PpuOut = PpuOut
  { poRead     :: Unsigned 8                                -- ^ a register read's value
  , poVramA    :: Maybe (Unsigned 14)                       -- ^ port A read
  , poVramB    :: Maybe (Bool, Unsigned 14, Unsigned 8)     -- ^ port B (write?, address, data)
  , poOamWrite :: Maybe (Unsigned 8, Unsigned 8)
  , poOamAddr  :: Unsigned 8                                -- ^ OAMADDR after this tick
  , poPixel    :: Maybe (Unsigned 8, Unsigned 8, Unsigned 9) -- ^ x, y, colour (palette entry, emphasis)
  , poNmi      :: Bool                                      -- ^ the NMI output after this tick
  , poFrame    :: Bool                                      -- ^ line 239 finished
  , poBus      :: Maybe (Unsigned 14)                       -- ^ v on the address bus without an access
                                                            --   (rendering off: $2006, $2007 increments; MMC3, step 3.10)
  , poOamRow   :: Maybe (Unsigned 5, Vec 8 (Unsigned 8))    -- ^ OAM corruption: this row gets these bytes
  , poFetch    :: Fetch                                     -- ^ what 'poVramA' is for (MMC5)
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFDataX

-- | Where the port A byte of the last fetch goes.
data Pend
  = PNone
  | PNt
  | PAt (Unsigned 3)                 -- ^ attribute byte, shift for the quadrant
  | PPatLo
  | PPatHi
  | PSpr Bool (Index 8) Bool Bool    -- ^ high byte?, slot, in range, flip horizontally
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFDataX

data Unit = Unit
  { uX, uAttr, uLo, uHi :: Unsigned 8
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFDataX

data Ppu = Ppu
  { pLine, pDot   :: Unsigned 9
  , pOdd          :: Bool
  , pCtrl, pMask  :: Unsigned 8
  , pVbl, pSpr0, pOvf, pSuppress :: Bool
  , pOamAddr      :: Unsigned 8
  , pV, pT        :: Unsigned 15
  , pX            :: Unsigned 3
  , pW            :: Bool
  , pBuf          :: Unsigned 8
  , pBufPend      :: Bool          -- ^ port B read pending for the buffer
  , pLatch        :: Unsigned 8
  , pDecay        :: Vec 8 (Unsigned 5)
  , pNt           :: Unsigned 8
  , pAt           :: Unsigned 2
  , pPatLo, pPatHi :: Unsigned 8
  , pBgLo, pBgHi, pAtLo, pAtHi :: Unsigned 16
  , pPend         :: Pend
  , pOamBus       :: Unsigned 8
  , pEvM          :: Unsigned 2
  , pEvFound      :: Unsigned 4
  , pEvSec        :: Unsigned 8    -- ^ secondary OAM's address register (0 to 31)
  , pEvDone       :: Bool
  , pSpr0Next, pSpr0Cur :: Bool
  , pRenHist      :: BitVector 3   -- ^ PPUMASK's rendering bits over the last 3 dots (bit 0: the last)
  , pOam2Ovf      :: Bool          -- ^ the address register overflowed: frozen until a reset
  , pFY, pFT, pFA :: Unsigned 8    -- ^ the sprite fetch's latched Y, tile, attribute
  , pEvPost       :: Bool          -- ^ past this line's overflow sprite: Y bytes only
  , pFlagPend     :: BitVector 2   -- ^ sprite 0 hit (bit 1), overflow (bit 0) found, set next dot
  , pCounting     :: BitVector 8   -- ^ per sprite slot: its X counter is counting (set at dot 339)
  , pVPend        :: Unsigned 3    -- ^ dots until the second $2006 write reaches v (0: none)
  , pVPendVal     :: Unsigned 15   -- ^ the value it writes
  , pFetchA       :: Unsigned 14   -- ^ the last nametable or attribute fetch's address (low byte: the octal latch)
  , pOamCor       :: Unsigned 6    -- ^ OAM corruption pending: 1 + the row it overwrites (0: none)
  , pRow0         :: Vec 8 (Unsigned 8) -- ^ OAM bytes 0 to 7 (what the corruption copies)
  , pSec          :: Vec 32 (Unsigned 8)
  , pUnits        :: Vec 8 Unit
  , pPal          :: Vec 32 (Unsigned 6)
  , pRdAle, pRdFill :: Unsigned 2  -- ^ the $2007 read state machine: dots to its ALE, to its fill
  , pRdAddr       :: Unsigned 14   -- ^ the address its ALE latched
  , pFetched      :: Bool          -- ^ port A was read by the last tick (its byte: 'pFetchB')
  , pFetchB       :: Unsigned 8    -- ^ the byte of the last port A read
  , pFetchAge     :: Unsigned 2    -- ^ 1: the last dot read port A, 2: earlier
  , pBufFromA     :: Bool          -- ^ the buffer takes the next port A byte
  , pWrAle        :: Unsigned 2    -- ^ the $2007 write state machine: dots to its ALE
  , pWrDo         :: Bool          -- ^ it writes on the next dot
  , pWrAddr       :: Unsigned 14   -- ^ the address its ALE latched
  , pWrData       :: Unsigned 8    -- ^ the byte the CPU wrote
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFDataX

-- | At power-on (as 'Nes.Spec.Ppu.newPpu'): everything 0, secondary OAM
-- $FF, at (0, 0).
ppuInit :: Ppu
ppuInit = Ppu
  { pLine = 0, pDot = 0, pOdd = False, pCtrl = 0, pMask = 0, pVbl = False, pSpr0 = False
  , pOvf = False, pSuppress = False, pOamAddr = 0, pV = 0, pT = 0, pX = 0, pW = False
  , pBuf = 0, pBufPend = False, pLatch = 0, pDecay = repeat 0, pNt = 0, pAt = 0
  , pPatLo = 0, pPatHi = 0, pBgLo = 0, pBgHi = 0, pAtLo = 0, pAtHi = 0, pPend = PNone
  , pOamBus = 0, pEvM = 0, pEvFound = 0, pEvSec = 0, pEvDone = False, pSpr0Next = False
  , pSpr0Cur = False, pRenHist = 0, pOam2Ovf = False, pFY = 0, pFT = 0, pFA = 0, pEvPost = False
  , pFlagPend = 0, pCounting = 0, pVPend = 0, pVPendVal = 0, pFetchA = 0, pOamCor = 0, pRow0 = repeat 0, pSec = repeat 0xFF, pUnits = repeat (Unit 0 0 0 0)
  , pPal = repeat 0, pRdAle = 0, pRdFill = 0, pRdAddr = 0, pFetched = False, pFetchB = 0
  , pFetchAge = 2, pBufFromA = False, pWrAle = 0, pWrDo = False, pWrAddr = 0, pWrData = 0 }

ppuNmiOut :: Ppu -> Bool
ppuNmiOut p = pVbl p && testBit (pCtrl p) 7

-- | Put away the port A byte of the last fetch (done at the start of every
-- tick; the tests also use it to compare with the spec, which stores the
-- byte at once).
ppuSettle :: Unsigned 8 -> Ppu -> Ppu
ppuSettle d p0 = case pPend p of
  PNone -> p
  PNt -> p { pNt = d, pPend = PNone }
  PAt sh -> p { pAt = resize (d `shiftR` fromEnum sh), pPend = PNone }
  PPatLo -> p { pPatLo = d, pPend = PNone }
  PPatHi -> p { pPatHi = d, pPend = PNone }
  PSpr hi i real flipH ->
    let b | not real = 0
          | flipH = reverse8 d
          | otherwise = d
        u = pUnits p !! i
        u' = if hi then u { uHi = b } else u { uLo = b }
    in p { pUnits = replace i u' (pUnits p), pPend = PNone }
 where
  -- the raw byte too, for the $2007 read buffer
  p = if pFetched p0 then p0 { pFetchB = d, pFetched = False } else p0

reverse8 :: Unsigned 8 -> Unsigned 8
reverse8 = bitCoerce . reverse . (bitCoerce :: Unsigned 8 -> Vec 8 Bit)

noOut :: Ppu -> PpuOut
noOut p = PpuOut 0 Nothing Nothing Nothing (pOamAddr p) Nothing (ppuNmiOut p) False Nothing Nothing FNt

-- | What a rendering fetch is for (as "Nes.Spec.Ppu"'s 'FetchKind'; side
-- track D, MMC5): the background's nametable byte (the line's two unused
-- fetches too), attribute byte, pattern bytes; the sprite fetch's garbage
-- nametable reads and pattern bytes.
data Fetch = FNt | FAt | FBgPat | FSprNt | FSprPat
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFDataX, BitPack)

-- | The kind of the dot's port A fetch, from the dot (sprite fetches on
-- 257 to 320, the background's 8-dot cadence elsewhere) and the address.
fetchKind :: Unsigned 9 -> Unsigned 14 -> Fetch
fetchKind dot a
  | dot >= 257 && dot <= 320 = if a >= 0x2000 then FSprNt else FSprPat
  | dot == 338 || dot == 340 = FNt
  | otherwise = case (dot - 1) .&. 7 of
      0 -> FNt
      1 -> FNt
      2 -> FAt
      3 -> FAt
      _ -> FBgPat

ppuTick :: Ppu -> Tick -> PpuIn -> (Ppu, PpuOut)
ppuTick p0 t inp = case t of
  TDot -> dotTick p1 inp
  TRead r -> regRead p1 r inp
  TWrite r v -> regWrite p1 r v inp
 where
  p1a = ppuSettle (piVramA inp) p0
  p1b = if pBufPend p1a then p1a { pBuf = piVramB inp, pBufPend = False } else p1a
  p1 = if pBufFromA p1b then p1b { pBuf = piVramA inp, pBufFromA = False } else p1b

rendering :: Ppu -> Bool
rendering p = pMask p .&. 0x18 /= 0

renderingNow :: Ppu -> Bool
renderingNow p = rendering p && (pLine p < 240 || pLine p == 261)

-- * Dots

dotTick :: Ppu -> PpuIn -> (Ppu, PpuOut)
dotTick p0 inp = (p6, out)
 where
  -- sprite 0 hit and overflow found during the last dot (as the spec)
  pf = p0 { pSpr0 = pSpr0 p0 || testBit (pFlagPend p0) 1, pOvf = pOvf p0 || testBit (pFlagPend p0) 0
          , pFlagPend = 0, pVPend = if pVPend p0 > 0 then pVPend p0 - 1 else 0 }
  -- the second $2006 write reaches v (as the spec)
  vLands = pVPend p0 == 1
  p = if vLands then pf { pV = pVPendVal pf } else pf
  vBus = if vLands && not (renderingNow pf) then Just (resize (pVPendVal pf)) else Nothing
  line = pLine p
  dot = pDot p
  -- the rendering pipeline lags PPUMASK by 2 dots (Nes.Spec.Ppu.renderDelay),
  -- its background fetches and reloads by 3 (Nes.Spec.Ppu.fetchDelay)
  ren = testBit (pRenHist p) 1
  renF = testBit (pRenHist p) 2
  hist' = (pRenHist p `shiftL` 1) .|. (if rendering p then 1 else 0)
  (line', dot')
    | line == 261 && dot == 339 && pOdd p && ren = (0, 0)
    | dot < 340 = (line, dot + 1)
    | line < 261 = (line + 1, 0)
    | otherwise = (0, 0)
  odd' = if line' == 0 && dot' == 0 then not (pOdd p) else pOdd p
  p2 = p { pLine = line', pDot = dot', pOdd = odd', pRenHist = hist' }
  -- VBlank and the flags
  p3 | dot' == 1 && line' == 241 =
         p2 { pVbl = pVbl p2 || not (pSuppress p2), pSuppress = False
            , pDecay = map (\c -> if c > 0 then c - 1 else 0) (pDecay p2)
            , pLatch = decayed (pLatch p2) (pDecay p2) }
     | dot' == 1 && line' == 261 = p2 { pVbl = False, pSpr0 = False, pOvf = False }
     | otherwise = p2
  visibleOrPre = line' < 240 || line' == 261
  -- OAM corruption (as 'Nes.Spec.Ppu.oamCorruption'), before the dot's work
  wasRen = testBit (pRenHist p) 2
  corRow = resize (pOamCor p3 - 1) :: Unsigned 5
  (p3c, oamRow)
    | not visibleOrPre = (p3, Nothing)
    | pOamCor p3 /= 0 = if not ren then (p3, Nothing)
        else ( p3 { pOamCor = 0, pSec = if wasRen && corRow /= 0 then replace corRow (head (pSec p3)) (pSec p3) else pSec p3 }
             , if wasRen && corRow /= 0 then Just (corRow, pRow0 p3) else Nothing )
    | wasRen && not ren =
        let a = pEvSec p3
            seed = if dot' >= 65 && dot' <= 256 then ((a + 3) .&. complement 3) .&. 0x1F else a
        in (p3 { pOamCor = resize seed + 1 }, Nothing)
    | otherwise = (p3, Nothing)
  (p4a, reqA) = if visibleOrPre && (ren || renF) then renderDot p3c inp line' dot' renF ren else (p3c, Nothing)
  -- secondary OAM's address register resets (Nes.Spec.Ppu.oam2Reset)
  p4 = if visibleOrPre && ren && (dot' == 64 || dot' == 256 || dot' == 339)
         then p4b { pEvSec = 0, pOam2Ovf = False } else p4b
  -- the sprite units' X counters start counting (Nes.Spec.Ppu.spriteAt)
  p4b = if visibleOrPre && ren && dot' == 339 then p4a { pCounting = 0xFF } else p4a
  (p5, pix) = if line' < 240 && dot' >= 1 && dot' <= 256
                then pixel p4 (resize line') (resize (dot' - 1)) ren
                else (p4, Nothing)
  (p6, reqB, incBus) = dataMachine p5 (maybe False (const True) reqA)
  -- the idle dot 0 shows the pattern address dot 5 will fetch (as the spec)
  dot0Bus = if renF && line' < 240 && dot' == 0
              then Just (resize ((resize (pCtrl p4 .&. 0x10) `shiftL` 8) .|. (resize (pNt p4) `shiftL` 4)
                                 .|. ((pV p4 `shiftR` 12) .&. 7) :: Unsigned 15))
              else Nothing
  out = (noOut p6) { poVramA = reqA, poVramB = reqB, poPixel = pix, poFrame = line' == 239 && dot' == 340
                   , poBus = maybe (maybe vBus Just incBus) Just dot0Bus, poOamRow = oamRow
                   , poFetch = fetchKind dot' (maybe 0 id reqA) }

-- | The $2007 read state machine after a dot's work (as
-- 'Nes.Spec.Ppu.dataMachine'), given whether this dot read port A. The
-- fill takes a fetch's byte if this dot or the last one read port A (this
-- dot's arrives with the next tick: 'pBufFromA'), else reads port B
-- itself, so the two ports never meet in one tick. Then v increments
-- (outside rendering the bus shows the new v). The write machine writes
-- the dot after its ALE, through port B unless this dot read port A (then
-- the write is lost, as in the spec). The two machines never act on the
-- same dot (CPU accesses are 3 dots apart).
dataMachine :: Ppu -> Bool -> (Ppu, Maybe (Bool, Unsigned 14, Unsigned 8), Maybe (Unsigned 14))
dataMachine p fetchedNow = (pWr2, maybe reqW Just reqB, maybe busW Just bus)
 where
  -- writes
  wa = pWrAddr pAle
  pWr0 = pAle { pWrDo = False, pWrAle = if pWrAle pAle > 0 then pWrAle pAle - 1 else 0 }
  (pWr1, reqW, busW)
    | not (pWrDo pAle) = (pWr0, Nothing, Nothing)
    | otherwise =
        let pw | wa >= 0x3F00 = pWr0 { pPal = replace (palIndex (resize wa)) (resize (pWrData pAle)) (pPal pWr0) }
               | otherwise = pWr0
            pi' = increment pw
        in (pi', if wa < 0x3F00 && not fetchedNow then Just (True, wa, pWrData pAle) else Nothing, idleBus pw pi')
  wv = resize (pV pWr1) :: Unsigned 14
  pWr2 | pWrAle pAle == 1 = pWr1 { pWrAddr = wv, pWrDo = True }
       | otherwise = pWr1
  age = if fetchedNow then 0 else pFetchAge p
  p0 = p { pFetched = fetchedNow, pFetchAge = if fetchedNow then 1 else 2
         , pRdFill = if pRdFill p > 0 then pRdFill p - 1 else 0
         , pRdAle = if pRdAle p > 0 then pRdAle p - 1 else 0 }
  fill = pRdFill p == 1
  (pFill, reqB, bus)
    | not fill = (p0, Nothing, Nothing)
    | otherwise =
        let pb | age == 0 = p0 { pBufFromA = True }
               | age == 1 = p0 { pBuf = pFetchB p }
               | otherwise = p0 { pBufPend = True }
            pi' = increment pb
        in ( pi', if age >= 2 then Just (False, pRdAddr p, 0) else Nothing, idleBus pb pi' )
  a = resize (pV pFill) :: Unsigned 14
  pAle | pRdAle p == 1 = pFill { pRdAddr = if a >= 0x3F00 then a - 0x1000 else a, pRdFill = 2 }
       | otherwise = pFill

-- | Bits whose counter runs out at this VBlank start (it was 1) decay to
-- 0. Element i of the counters belongs to bit i.
decayed :: Unsigned 8 -> Vec 8 (Unsigned 5) -> Unsigned 8
decayed v cs = v .&. complement (fold (.|.) (imap (\i c -> if c == 1 then bit (fromEnum i) else 0) cs))

ntAddr :: Unsigned 15 -> Unsigned 14
ntAddr v = 0x2000 .|. resize (v .&. 0x0FFF)

-- | One rendering dot (as 'Nes.Spec.Ppu.renderDot'): the shifters and the
-- sprites run with the 2-dot delay ('renS'), the background fetches,
-- reloads and scroll updates with the 3-dot one ('renF').
renderDot :: Ppu -> PpuIn -> Unsigned 9 -> Unsigned 9 -> Bool -> Bool -> (Ppu, Maybe (Unsigned 14))
renderDot p0 inp line dot renF renS = (pS, req)
 where
  pre = line == 261
  fetching = renF && ((dot >= 1 && dot <= 256) || (dot >= 321 && dot <= 336))
  shiftNow = renS && ((dot >= 2 && dot <= 257) || (dot >= 322 && dot <= 337))
  reloadNow = renF && ((dot >= 9 && dot <= 257 && (dot - 1) .&. 7 == 0) || dot == 329 || dot == 337)
  -- the serial inputs: 0, 1 and the attribute latch (as 'Nes.Spec.Ppu.shiftBg')
  pA = if shiftNow then p0 { pBgLo = pBgLo p0 `shiftL` 1, pBgHi = (pBgHi p0 `shiftL` 1) .|. 1
                           , pAtLo = (pAtLo p0 `shiftL` 1) .|. (pAtLo p0 .&. 1)
                           , pAtHi = (pAtHi p0 `shiftL` 1) .|. (pAtHi p0 .&. 1) } else p0
  pB = if reloadNow then reloadBg pA else pA
  v = pV pB
  phase = resize ((dot - 1) .&. 7) :: Unsigned 3
  atSh = resize (((v `shiftR` 4) .&. 4) .|. (v .&. 2))
  atA = resize (0x23C0 .|. (v .&. 0x0C00) .|. ((v `shiftR` 4) .&. 0x38) .|. ((v `shiftR` 2) .&. 0x07)) :: Unsigned 14
  hybridNt = 0x2000 .|. resize (v .&. 0x0F00) .|. (pFetchA pB .&. 0xFF)
  hybridAt = 0x2300 .|. resize (v .&. 0x0C00) .|. (pFetchA pB .&. 0xFF)
  patAddr off = aleRead (resize ((resize (pCtrl pB .&. 0x10) `shiftL` 8) .|. (resize (pNt pB) `shiftL` 4)
                        .|. ((v `shiftR` 12) .&. 7) .|. off :: Unsigned 15) :: Unsigned 14)
  -- the $2007 read machine reading on this address dot: the octal latch
  -- keeps the last read's byte as the low byte (as the spec's 'bgPattern')
  aleRead a = if pRdFill pB == 1 then (a .&. 0x3F00) .|. resize (pFetchB pB) else a
  (pC, bgReq)
    | not fetching = (pB, Nothing)
    | otherwise = case phase of
        0 -> (pB { pPend = PNt, pFetchA = ntAddr v }, Just (ntAddr v))
        -- the read dot: the high byte from the current v, the low byte from
        -- the octal latch; read again when v changed ('Nes.Spec.Ppu.renderFetch')
        1 | hybridNt /= pFetchA pB -> (pB { pPend = PNt }, Just hybridNt)
        2 -> (pB { pPend = PAt atSh, pFetchA = atA }, Just atA)
        3 | hybridAt /= pFetchA pB -> (pB { pPend = PAt atSh }, Just hybridAt)
        4 -> (pB { pPend = PPatLo }, Just (patAddr 0))
        6 -> (pB { pPend = PPatHi }, Just (patAddr 8))
        7 -> (incX pB, Nothing)
        _ -> (pB, Nothing)
  pD = if renF && dot == 256 then incY pC else pC
  -- the horizontal copy at 257 comes after that dot's garbage nametable
  -- fetch, which uses the old v (as the spec)
  pE = pD
  pF = if renF && pre && dot >= 280 && dot <= 304 then pE { pV = (pV pE .&. complement 0x7BE0) .|. (pT pE .&. 0x7BE0) } else pE
  dummyNt = renF && (dot == 338 || dot == 340)
  dummyReq = if dummyNt then Just (ntAddr (pV pF)) else Nothing
  -- the unused fetches' byte is kept: dot 0 shows its pattern address
  pG = if dummyNt then pF { pPend = PNt } else pF
  (pS0, sprReq)
    | not renS = (pG, Nothing)
    | pre = if dot >= 257 && dot <= 320 then fetchPhase pG line dot
            else (if dot >= 1 && dot <= 64 && even dot then oam2Inc pG else pG, Nothing)
    | otherwise = spriteDot pG inp line dot
  pS = if renF && dot == 257 then pS0 { pV = (pV pS0 .&. complement 0x041F) .|. (pT pS0 .&. 0x041F) } else pS0
  req = case bgReq of
    Just a -> Just a
    Nothing -> case dummyReq of
      Just a -> Just a
      Nothing -> sprReq

reloadBg :: Ppu -> Ppu
reloadBg p = p { pBgLo = (pBgLo p .&. 0xFF00) .|. resize (pPatLo p)
               , pBgHi = (pBgHi p .&. 0xFF00) .|. resize (pPatHi p)
               , pAtLo = (pAtLo p .&. 0xFF00) .|. fill (testBit (pAt p) 0)
               , pAtHi = (pAtHi p .&. 0xFF00) .|. fill (testBit (pAt p) 1) }
 where
  fill b = if b then 0xFF else 0

incX :: Ppu -> Ppu
incX p = p { pV = if v .&. 0x1F == 31 then (v .&. complement 0x1F) `xor` 0x400 else v + 1 }
 where v = pV p

incY :: Ppu -> Ppu
incY p
  | v .&. 0x7000 /= 0x7000 = p { pV = v + 0x1000 }
  | otherwise =
      let y = (v .&. 0x3E0) `shiftR` 5
          (y', flipNt) | y == 29 = (0, True)
                       | y == 31 = (0, False)
                       | otherwise = (y + 1, False)
          v1 = (v .&. complement 0x73E0) .|. (y' `shiftL` 5)
      in p { pV = if flipNt then v1 `xor` 0x800 else v1 }
 where v = pV p

spriteHeight :: Ppu -> Unsigned 9
spriteHeight p = if testBit (pCtrl p) 5 then 16 else 8

spriteDot :: Ppu -> PpuIn -> Unsigned 9 -> Unsigned 9 -> (Ppu, Maybe (Unsigned 14))
spriteDot p inp line dot
  | dot >= 1 && dot <= 64 =
      let p' = p { pOamBus = 0xFF }
      in (if even dot then oam2Inc p' { pSec = replace (secIx p') 0xFF (pSec p') } else p', Nothing)
  | dot >= 65 && dot <= 256 =
      let p' = if dot == 65 then p { pEvM = 0, pEvFound = 0, pEvDone = False, pSpr0Next = False, pEvPost = False } else p
      in if odd dot then (p' { pOamBus = piOam inp }, Nothing) else (evaluate p' line dot, Nothing)
  | dot >= 257 && dot <= 320 = fetchPhase p line dot
  | dot >= 321 = (p { pOamBus = oam2Read p }, Nothing)
  | otherwise = (p, Nothing)

-- | Secondary OAM's address register as an index, and the byte there.
secIx :: Ppu -> Index 32
secIx p = bitCoerce (resize (pEvSec p) :: Unsigned 5)

oam2Read :: Ppu -> Unsigned 8
oam2Read p = pSec p !! secIx p

-- | The register counts 0 to 31; wrapping freezes it at 0 until a reset
-- (as Nes.Spec.Ppu.oam2Inc).
oam2Inc :: Ppu -> Ppu
oam2Inc p
  | pOam2Ovf p = p
  | pEvSec p == 31 = p { pEvSec = 0, pOam2Ovf = True }
  | otherwise = p { pEvSec = pEvSec p + 1 }

-- | An even evaluation dot: act on the byte read on the odd dot before
-- (as Nes.Spec.Ppu.evaluate).
evaluate :: Ppu -> Unsigned 9 -> Unsigned 9 -> Ppu
evaluate p line dot
  | pEvDone p = advance rb (a + 4)
  | pEvFound p < 8 =
      let pw = p { pSec = replace (secIx p) b (pSec p) }
      in if pEvM p == 0
           then if inRange
                  then advance (oam2Inc pw { pSpr0Next = pSpr0Next pw || dot == 66, pEvM = 1 }) (a + 1)
                  -- out of range: realigned (AccuracyCoin "Misaligned OAM behavior" 2)
                  else advance pw ((a + 4) .&. 0x1FC)
           else if pEvM p == 3
             -- the X byte: the same range check, realigned when out of range
             -- (AccuracyCoin d103cbd, "Misaligned OAM behavior" 4 to 6)
             then advance (oam2Inc pw { pEvM = 0, pEvFound = pEvFound p + 1 }) (if inRange then a + 1 else (a + 1) .&. 0x1FC)
             else advance (oam2Inc pw { pEvM = pEvM p + 1 }) (a + 1)
  | pEvM p == 0 =
      if pEvPost p then advance rb ((a .&. 0x1FC) + 4)
      else if inRange then advance rb { pFlagPend = pFlagPend rb .|. 1, pEvM = 1 } (a + 1)
      -- The hardware bug: n and m both increment (m without carry).
      else advance rb (((a + 4) .&. 0x1FC) .|. ((a + 1) .&. 3))
  | pEvM p == 3 = advance rb { pEvM = 0, pEvPost = True } ((a + 1) .&. 0x1FC)
  | otherwise = advance rb { pEvM = pEvM p + 1 } (a + 1)
 where
  -- done or full: the write cycle reads secondary OAM back
  rb = p { pOamBus = oam2Read p }
  b = pOamBus p
  a = resize (pOamAddr p) :: Unsigned 9
  h = spriteHeight p
  diff = line - resize b  -- line - Y, modulo 512
  inRange = line >= resize b && diff < h
  -- OAMADDR moves on; an overflow of n ends the evaluation.
  advance q a' = q { pOamAddr = resize a', pEvDone = pEvDone q || a' > 0xFF }

-- | Dots 257 to 320: OAMADDR held at 0, the slots for the next line.
fetchPhase :: Ppu -> Unsigned 9 -> Unsigned 9 -> (Ppu, Maybe (Unsigned 14))
fetchPhase p0 line dot = spriteFetch p1 line dot
 where
  p1 = (if dot == 257 then p0 { pSpr0Cur = pSpr0Next p0 } else p0) { pOamAddr = 0 }

spriteFetch :: Ppu -> Unsigned 9 -> Unsigned 9 -> (Ppu, Maybe (Unsigned 14))
spriteFetch p line dot = (p2, req)
 where
  i = bitCoerce (resize ((dot - 257) `shiftR` 3) :: Unsigned 3) :: Index 8
  k = resize ((dot - 257) .&. 7) :: Unsigned 3
  -- secondary OAM through its address register: Y, tile, attribute
  -- latched, then X five times (as Nes.Spec.Ppu.spriteFetch)
  o2 = oam2Read p
  pL = case k of
    0 -> p { pFY = o2 }
    1 -> p { pFT = o2 }
    2 -> p { pFA = o2 }
    _ -> p
  y = pFY pL
  tile = pFT pL
  attr = pFA pL
  x = o2
  p1 = (if k <= 2 || k == 7 then oam2Inc pL else pL) { pOamBus = o2 }
  h = spriteHeight p
  -- The fetch checks the range again with the line's low 8 bits (the
  -- pre-render line counts as line 5).
  s10 :: Unsigned 9 -> Signed 10
  s10 u = bitCoerce (resize u :: Unsigned 10)
  rowRaw = s10 (line .&. 0xFF) - s10 (resize y)
  real = rowRaw >= 0 && rowRaw < s10 h
  row0 = (bitCoerce (resize rowRaw :: Signed 9) :: Unsigned 9) .&. (h - 1)
  row = if testBit attr 7 then h - 1 - row0 else row0
  addr :: Unsigned 14
  addr | h == 16 = resize ((resize (tile .&. 1) `shiftL` 12) .|. ((resize (tile .&. 0xFE) + resize (row `shiftR` 3)) `shiftL` 4) .|. resize (row .&. 7) :: Unsigned 15)
       | otherwise = resize ((resize (pCtrl p .&. 0x08) `shiftL` 9) .|. (resize tile `shiftL` 4) .|. resize row :: Unsigned 15)
  (p2, req) = case k of
    0 -> (p1, Just (ntAddr (pV p)))
    2 -> (p1, Just (ntAddr (pV p)))
    4 -> (p1 { pPend = PSpr False i real (testBit attr 6) }, Just addr)
    6 -> let u = pUnits p1 !! i
             p' = p1 { pUnits = replace i u { uX = x, uAttr = attr } (pUnits p1) }
         in (p' { pPend = PSpr True i real (testBit attr 6) }, Just (addr .|. 8))
    _ -> (p1, Nothing)

-- * Pixels

pixel :: Ppu -> Unsigned 8 -> Unsigned 8 -> Bool -> (Ppu, Maybe (Unsigned 8, Unsigned 8, Unsigned 9))
pixel p y x ren = (p', Just (x, y, colour))
 where
  mask = pMask p
  fx = 15 - bitCoerce (resize (pX p) :: Unsigned 4) :: Index 16
  bitAt r = if testBit r (fromEnum fx) then 1 else 0 :: Unsigned 5
  bgOn = testBit mask 3 && (x >= 8 || testBit mask 1)
  bgPix = if bgOn then bitAt (pBgLo p) .|. (bitAt (pBgHi p) `shiftL` 1) else 0
  bgPal = bitAt (pAtLo p) .|. (bitAt (pAtHi p) `shiftL` 1)
  spOn = testBit mask 4 && (x >= 8 || testBit mask 2)
  -- Sprite units (as Nes.Spec.Ppu.spriteAt): a slot draws when it isn't
  -- counting or its counter has reached 0; counters count even while
  -- rendering is off, shifters shift only while rendering; at 0 a slot
  -- stops counting until dot 339.
  counting i = testBit (pCounting p) (fromEnum i)
  active = imap (\i u -> not (counting i) || uX u == 0) (pUnits p)
  outs = zipWith (\a u -> if a
                            then (if testBit (uLo u) 7 then 1 else 0) .|. (if testBit (uHi u) 7 then 2 else 0)
                            else 0 :: Unsigned 5) active (pUnits p)
  firstOpaque = findIndex (/= 0) outs
  (spPix0, spAttr, slot0) = case firstOpaque of
    Just i -> (outs !! i, uAttr (pUnits p !! i), i == 0)
    Nothing -> (0, 0, False)
  spPix = if spOn then spPix0 else 0
  hit = slot0 && pSpr0Cur p && bgPix /= 0 && spPix /= 0 && x /= 255
  clocked = zipWith (\a u -> if not a then u { uX = uX u - 1 }
                             else if ren then u { uLo = uLo u `shiftL` 1, uHi = uHi u `shiftL` 1 }
                             else u) active (pUnits p)
  -- slots counting with the counter at 0 switch to halted
  counting' = pCounting p .&. complement (pack (reverse (imap (\i a -> a && counting i) active)))
  sp5 = resize spAttr .&. 3 :: Unsigned 5
  idx :: Unsigned 5
  idx | bgPix == 0 && spPix == 0 = 0
      | bgPix == 0 = 0x10 .|. (sp5 `shiftL` 2) .|. spPix
      | spPix == 0 = (bgPal `shiftL` 2) .|. bgPix
      | testBit spAttr 5 = (bgPal `shiftL` 2) .|. bgPix
      | otherwise = 0x10 .|. (sp5 `shiftL` 2) .|. spPix
  entry
    | ren = pPal p !! palIndex idx
    | pV p .&. 0x3F00 == 0x3F00 = pPal p !! palIndex (resize (pV p))
    | otherwise = pPal p !! (0 :: Index 32)
  grey = if testBit mask 0 then 0x30 else 0x3F
  colour = resize (entry .&. grey) .|. (resize (mask .&. 0xE0) `shiftL` 1)
  -- (forced every dot: with rendering off nothing else reads the units,
  -- and a simulation would pile up thunks; no logic)
  p' = foldr seq () clocked `seq`
       p { pUnits = clocked, pCounting = counting'
         , pFlagPend = pFlagPend p .|. (if ren && hit then 2 else 0) }

-- | Palette RAM index: $3F10, $3F14, $3F18, $3F1C mirror $3F00 to $3F0C.
palIndex :: Unsigned 5 -> Index 32
palIndex i = bitCoerce (if i .&. 0x13 == 0x10 then i .&. 0x0F else i)

-- * Registers

-- | The I/O latch now (decayed bits already cleared at VBlank starts).
latchDrive :: Unsigned 8 -> Unsigned 8 -> Ppu -> Ppu
latchDrive mask v p = p { pLatch = (pLatch p .&. complement mask) .|. (v .&. mask)
                        , pDecay = imap (\i c -> if testBit mask (fromEnum i) then 30 else c) (pDecay p) }

regRead :: Ppu -> Index 8 -> PpuIn -> (Ppu, PpuOut)
regRead p r inp = case r of
  2 ->
    let v = (if pVbl p then 0x80 else 0) .|. (if pSpr0 p then 0x40 else 0) .|. (if pOvf p then 0x20 else 0)
            .|. (pLatch p .&. 0x1F)
        p' = latchDrive 0xE0 v p { pVbl = False, pW = False
                                 , pSuppress = pSuppress p || (pLine p == 241 && pDot p == 0) }
    in (p', (noOut p') { poRead = v })
  4 ->
    let v | renderingNow p = pOamBus p
          | otherwise = piOam inp
        p' = latchDrive 0xFF v p
    in (p', (noOut p') { poRead = v })
  7 ->
    let a = resize (pV p) :: Unsigned 14
        pal = a >= 0x3F00
        grey = if testBit (pMask p) 0 then 0x30 else 0x3F
        c = resize (pPal p !! palIndex (resize a)) .&. grey :: Unsigned 8
        out | pal = c .|. (pLatch p .&. 0xC0)
            | otherwise = pBuf p
        p1 = if pal then latchDrive 0x3F c p else latchDrive 0xFF (pBuf p) p
        -- the memory read and the increment come later ('dataMachine')
        p2 = p1 { pRdAle = 3 }
    in (p2, (noOut p2) { poRead = out })
  _ -> (p, (noOut p) { poRead = pLatch p })

-- | After a $2007 access outside rendering the bus shows the new v.
idleBus :: Ppu -> Ppu -> Maybe (Unsigned 14)
idleBus before after = if renderingNow before then Nothing else Just (resize (pV after))

increment :: Ppu -> Ppu
increment p
  | renderingNow p = incY (incX p)
  | otherwise = p { pV = pV p + (if testBit (pCtrl p) 2 then 32 else 1) }

regWrite :: Ppu -> Index 8 -> Unsigned 8 -> PpuIn -> (Ppu, PpuOut)
regWrite p0 r d _ = case r of
  0 -> done p { pCtrl = d, pT = (pT p .&. 0x73FF) .|. (resize (d .&. 3) `shiftL` 10) }
  1 -> done p { pMask = d }
  3 -> done p { pOamAddr = d }
  4 | renderingNow p -> done p { pOamAddr = (pOamAddr p + 4) .&. 0xFC }
    | otherwise -> let p' = p { pOamAddr = pOamAddr p + 1 }
                       -- attribute bytes without bits 2 to 4 (as the spec)
                       d' = if pOamAddr p .&. 3 == 2 then d .&. 0xE3 else d
                       p'' = if pOamAddr p < 8 then p' { pRow0 = replace (resize (pOamAddr p) :: Unsigned 3) d' (pRow0 p') } else p'
                   in (p'', (noOut p'') { poOamWrite = Just (pOamAddr p, d') })
  5 | not (pW p) -> done p { pT = (pT p .&. 0x7FE0) .|. resize (d `shiftR` 3), pX = resize d, pW = True }
    | otherwise -> done p { pT = (pT p .&. 0x0C1F) .|. (resize (d .&. 7) `shiftL` 12) .|. (resize (d .&. 0xF8) `shiftL` 2), pW = False }
  6 | not (pW p) -> done p { pT = (pT p .&. 0x00FF) .|. (resize (d .&. 0x3F) `shiftL` 8), pW = True }
    | otherwise -> let t' = (pT p .&. 0x7F00) .|. resize d
                   -- v follows 'Nes.Spec.Ppu.v2006Delay' dots later ('dotTick')
                   in done p { pT = t', pVPend = 4, pVPendVal = t', pW = False }
  -- written later, by the write state machine ('dataMachine')
  7 -> done p { pWrData = d, pWrAle = 3 }
  _ -> done p
 where
  p = latchDrive 0xFF d p0
  done q = (q, noOut q)

-- * In hardware

-- | The PPU with its OAM in block RAM. The machine gives at most one tick
-- per clock, and the bytes its memories return for port A and port B (one
-- clock after the request; they must hold until the next request). On a
-- clock without a tick the state holds and OAM keeps being read at
-- OAMADDR, so 'piOam' is right whenever the next tick comes.
ppu
  :: HiddenClockResetEnable dom
  => Signal dom (Maybe Tick)
  -> Signal dom (Unsigned 8) -- ^ port A data
  -> Signal dom (Unsigned 8) -- ^ port B data
  -> Signal dom PpuOut
ppu tick dataA dataB = out
 where
  oam = oamRam (poOamAddr <$> out) (poOamWrite <$> out) (poOamRow <$> out)
  out = mealy step ppuInit (bundle (tick, dataA, dataB, oam))
  step s (t, a, b, o) = case t of
    Nothing -> (s, noOut s)
    Just tk -> ppuTick s tk (PpuIn a b o)

-- | OAM: 256 bytes as 8 block RAMs of 32, one per byte of a row (byte i
-- of row r at address 8r + i), so a row is written in one clock (OAM
-- corruption). The byte at the read address, one clock later.
oamRam
  :: HiddenClockResetEnable dom
  => Signal dom (Unsigned 8)                           -- ^ read address
  -> Signal dom (Maybe (Unsigned 8, Unsigned 8))       -- ^ a byte write
  -> Signal dom (Maybe (Unsigned 5, Vec 8 (Unsigned 8))) -- ^ a row write
  -> Signal dom (Unsigned 8)
oamRam addr wr rowWr = (!!) <$> bundle cols <*> register 0 (col <$> addr)
 where
  row a = resize (a `shiftR` 3) :: Unsigned 5
  col a = resize a :: Unsigned 3
  cols = map column indicesI
  column i = blockRam (replicate d32 0) (row <$> addr) (colWr i <$> wr <*> rowWr)
  colWr :: Index 8 -> Maybe (Unsigned 8, Unsigned 8) -> Maybe (Unsigned 5, Vec 8 (Unsigned 8)) -> Maybe (Unsigned 5, Unsigned 8)
  colWr i w rw = case rw of
    Just (r', bs) -> Just (r', bs !! i)
    Nothing -> case w of
      Just (a, d) | col a == bitCoerce i -> Just (row a, d)
      _ -> Nothing
