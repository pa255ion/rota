{-# LANGUAGE StrictData #-}
{-|
Side track Q, cassettes on the board: the tape's answer to the OS's
cassette call, beside the core ("A800.Core" only routes the reads to it:
'SpCas'), as the spec's "A800.Spec.Machine.casRead" describes it. Kept out
of the core's state: carried through the core's sequencer, its 120 bits
of state took 13,500 LUTs and the answer 12,000 more (measured,
2026-10-26: the Arty's 800XL at 60,133 of 53,200); here it is a few
hundred.

'casUnit' answers the core's 'SpCas' reads (a byte the clock after, or
the tape image's byte from the cartridge's reader once it arrives),
latches the DCB's bytes from the core's completed reads ('casEvent'), and
says when it is armed (the core then routes $E45A, $E45B and the $D5 page
here too). Its pure parts ('casProg', 'casTape', 'casEvent') are the
lockstep test's model too ("A800.LockCore").
-}
module A800.CasUnit
  ( CasState (..)
  , CasWant (..)
  , CasAns (..)
  , casInit
  , casProg
  , casTape
  , casEvent
  , casUnit
  ) where

import Clash.Prelude


-- | The answer's state, as the spec's slots ("A800.Spec.Machine.casRead"):
-- armed, its page count, the record to read next and the one being read,
-- DDEVIC and DUNIT, DCOMND, DBUF, DBYT, DAUX1, DAUX2 as the program read
-- them, the record's length, the data's running checksum, its verdict,
-- and what the next memory read's byte is for.
data CasState = CasState
  { csArm  :: Bool
  , csLap  :: Unsigned 3
  , csPos  :: Unsigned 12
  , csRec  :: Unsigned 12
  , csDev  :: Unsigned 8
  , csDcb  :: Vec 8 (Unsigned 8)
  , csLen  :: Unsigned 8
  , csSum  :: Unsigned 8
  , csOk   :: Bool
  , csWant :: CasWant
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFDataX

-- | The program's reads whose byte the answer keeps: the DCB's byte k (0
-- DDEVIC, 1 to 8 'casDcbLo') from the CPU's next read, the record's
-- length, a data byte, the checksum.
data CasWant = CwNone | CwRam (Index 9) | CwLen | CwData | CwCk
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFDataX

casInit :: CasState
casInit = CasState False 0 0 0 0 (repeat 0) 0 0 False CwNone

-- | The DCB's bytes the program reads: DDEVIC, DUNIT, DCOMND, DBUF,
-- DBYT, DAUX1, DAUX2 (their addresses' low bytes, page 3).
casDcbLo :: Index 9 -> Unsigned 8
casDcbLo k = (0x00 :> 0x01 :> 0x02 :> 0x04 :> 0x05 :> 0x08 :> 0x09 :> 0x0A :> 0x0B :> Nil) !! k

-- | A completed CPU read (its address and byte, from the core's cycle
-- events): the DCB's byte the program asked for ('CwRam').
casEvent :: Unsigned 16 -> Unsigned 8 -> CasState -> CasState
casEvent a v c = case csWant c of
  CwRam k | a == 0x0300 + resize (casDcbLo k) ->
    if k == 0 then c { csDev = v, csWant = CwNone }
      else c { csDcb = replace (k - 1) v (csDcb c), csWant = CwNone }
  _ -> c

-- | The tape image's byte the program read ('CaTape') arrived: what the
-- answer keeps of it.
casTape :: Unsigned 8 -> CasState -> CasState
casTape v c = case csWant c of
  CwLen ->
    let more = csDcb c !! (1 :: Index 8) == 0x52 && v > 0
    in c { csLen = v, csSum = 0, csPos = if more then csPos c + 1 else csPos c, csWant = CwNone }
  CwData ->
    let t = resize (csSum c) + resize v :: Unsigned 9
    in c { csSum = resize (if t > 0xFF then t - 0xFF else t), csWant = CwNone }
  CwCk -> c { csOk = v == csSum c, csWant = CwNone }
  _ -> c

-- | The tape's answer to the CPU's read at @a@, one the core routed here
-- ('A800.Core.casRoute': $E459 with the OS mapped, and while armed $E45A,
-- $E45B and the $D5 page; as the spec's "A800.Spec.Machine.casRead",
-- which describes the program): a byte now, or the tape image's byte at
-- an offset (the media, through the cartridge's reader: record k's slot
-- at k * 256, its length first, then its bytes), with the new state.
data CasAns = CaNow (Unsigned 8) | CaTape (Unsigned 24)
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFDataX

casProg :: CasState -> Unsigned 16 -> (CasState, CasAns)
casProg c a
  | not (csArm c) = (c { csArm = True, csLap = 0 }, CaNow 0x4C)
  | a == 0xE459 = (c, CaNow 0x4C)
  | a == 0xE45A = (c, CaNow 0x00)
  | a == 0xE45B = (c, CaNow 0xD5)
  | otherwise = program
 where
  o = resize a :: Unsigned 8
  g = resize (csLap c) * 50 + resize (o `div` 5) :: Unsigned 9
  j = o `mod` 5
  pick v0 v1 v2 v3 v4 = case j of
    0 -> v0
    1 -> v1
    2 -> v2
    3 -> v3
    _ -> v4
  dcb = csDcb c
  unit = dcb !! (0 :: Index 8)
  cmd = dcb !! (1 :: Index 8)
  buf = (resize (dcb !! (3 :: Index 8)) `shiftL` 8) .|. resize (dcb !! (2 :: Index 8)) :: Unsigned 16
  n = (resize (dcb !! (5 :: Index 8)) `shiftL` 8) .|. resize (dcb !! (4 :: Index 8)) :: Unsigned 16
  aux1 = dcb !! (6 :: Index 8)
  aux2 = dcb !! (7 :: Index 8)
  len = csLen c
  isRead = cmd == 0x52
  cnt = if not isRead then 0 else if n < resize len then resize n else resize len :: Unsigned 9
  hasCk = isRead && n < resize len
  status = if not isRead then 139 else if len == 0 then 144 else if hasCk && csOk c then 1 else 144 :: Unsigned 8
  i = g - 10
  e = i - cnt - (if hasCk then 1 else 0)
  tape q off = (q, CaTape ((resize (csRec q) `shiftL` 8) + off))
  store q v ad = (q, CaNow (pick 0xA9 v 0x8D (resize ad) (resize (ad `shiftR` 8 :: Unsigned 16))))
  program
    | o == 250 = (c, CaNow 0x4C)
    | o == 251 = (c, CaNow 0x00)
    | o == 252 = (c { csLap = csLap c + 1 }, CaNow 0xD5)
    | o > 252 = (c, CaNow 0xFF)
    | g == 0 = (if j == 2 then c { csWant = CwRam 0 } else c, CaNow (pick 0xAD 0x00 0x03 0xEA 0xEA))
    | csDev c /= 0x60 = (if j == 2 then c { csArm = False } else c, CaNow (pick 0x6C 0x5A 0xE4 0xEA 0xEA))
    | g <= 8 = (if j == 2 then c { csWant = CwRam (fromIntegral g) } else c, CaNow (pick 0xAD (casDcbLo (fromIntegral g)) 0x03 0xEA 0xEA))
    | g == 9 = if j == 1 then tape c { csWant = CwLen, csRec = csPos c } 0 else (c, CaNow (pick 0xA9 0 0x8D 0x42 0x00))
    | i < cnt = if j == 1 then tape c { csWant = CwData } (1 + resize i) else store c 0 (buf + resize i)
    | hasCk && i == cnt = if j == 1 then tape c { csWant = CwCk } (1 + resize n) else store c 0 (0x0042 :: Unsigned 16)
    | e == 0 = store c (0x5F + unit) (0x023A :: Unsigned 16)
    | e == 1 = store c cmd (0x023B :: Unsigned 16)
    | e == 2 = store c aux1 (0x023C :: Unsigned 16)
    | e == 3 = store c aux2 (0x023D :: Unsigned 16)
    | e == 4 = store c status (0x0303 :: Unsigned 16)
    | e == 5 = store c 0 (0x0042 :: Unsigned 16)
    -- gone at the RTS's second cycle (read once; an interrupt's dummy
    -- reads read the RTS itself)
    | otherwise = (if j == 4 then c { csArm = False } else c, CaNow (pick 0xA0 status 0x38 0x60 0xEA))

-- | The unit: @run@ the core running (low: back to power-on), @req@ the
-- core's 'SpCas' read (its CPU address, for one clock), @ev@ the core's
-- completed CPU reads (address, byte), @tape@ the cartridge reader's byte
-- and its readiness. Out, all from its state (no path through it): armed,
-- the answer's byte and readiness (low from the clock after a request
-- until it is there), the tape image's read for the cartridge reader (a
-- clock long, the clock after the core's request; the reader is ready
-- again only after it saw it, so the unit waits a clock before looking).
casUnit
  :: HiddenClockResetEnable dom
  => Signal dom Bool
  -> Signal dom (Maybe (Unsigned 16))
  -> Signal dom (Maybe (Unsigned 16, Unsigned 8))
  -> Signal dom (Unsigned 8, Bool)
  -> (Signal dom Bool, Signal dom (Unsigned 8, Bool), Signal dom (Maybe (Unsigned 24)))
casUnit run req ev tape = unbundle (mealy step unitInit (bundle (run, req, ev, tape)))
 where
  step u (r, rq, e, (tb, tok)) = (u', (csArm (uCas u), uOut u, uReq u))
   where
    u' | not r = unitInit
       | Just a <- rq = case casProg c a of
           (c', CaNow v) -> u { uCas = c', uWait = 0, uOut = (v, True), uReq = Nothing }
           (c', CaTape o) -> u { uCas = c', uWait = 2, uOut = (0, False), uReq = Just o }
       | uWait u == 2 = u { uCas = c, uWait = 1, uReq = Nothing }
       | uWait u == 1 && tok = u { uCas = casTape tb c, uWait = 0, uOut = (tb, True) }
       | otherwise = u { uCas = c }
    -- (a completed read's DCB byte, each clock)
    c = case e of
      Just (ea, ed) -> casEvent ea ed (uCas u)
      Nothing -> uCas u

data Unit = Unit
  { uCas  :: CasState
  , uWait :: Unsigned 2                 -- ^ 2: the tape's read going out, 1: waiting for it
  , uOut  :: (Unsigned 8, Bool)
  , uReq  :: Maybe (Unsigned 24)
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFDataX

unitInit :: Unit
unitInit = Unit casInit 0 (0, True) Nothing
