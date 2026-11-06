{-|
A read port into the PS's DDR for cores (side track C, step 1): an AXI3
master for a Zynq HP port ("Platform.Mem.Axi") behind a small line cache
with sequential prefetch.

The port runs in the system's own clock domain (normally 'DomPix'); the PL
supplies the HP port's clock, so there is no crossing in the PL. Any
frequency up to 250 MHz is allowed, with no minimum (DS187 v1.21,
table 19, F_AXI_MAX); the HP port's own FIFOs cross into the DDR
controller's clock.

__For the core:__ a request is an address for one clock ('Just'); the
answer comes later, once, as the 8-byte word holding that address
('Just'; pick a byte with 'ddrByte'). A core has at most one request
outstanding: it asks again from the clock after the answer at the
earliest (a request while one is outstanding, the answer's own clock
included, is a mistake and may be dropped). A hit answers the next clock;
a miss after the burst's beat with that word arrives (its latency depends
on the DDR controller and on what else uses DDR; step 3 measures it).

__The cache:__ @2^k@ lines of 32 bytes (4 beats of 8), direct-mapped, in
one block RAM; tags in registers. A miss reads its whole line in one burst
of 4. With 'dcPrefetch', every request also queues the next line, read
while the port is otherwise idle, so a core reading forwards (a CPU
fetching code) mostly hits. Hits are answered while a fill runs (the block
RAM has separate read and write ports); a miss waits for the fill in
progress, or is answered by it when it is for that line.

'diInval' empties the cache (for a core whose data the PS has just
rewritten, as after loading a cartridge); a fill already under way
completes on the bus but isn't kept. Requests from the clock after it see
only new data.

'stError' is sticky: a beat answered with an AXI error (SLVERR, DECERR),
or a burst whose RLAST disagrees with its length.
-}
module Platform.Mem.Ddr
  ( DdrAddr
  , Line
  , Beat
  , lineOf
  , beatOf
  , lineAddr
  , ddrByte
    -- * Configuration
  , DdrConfig (..)
  , ddrConfigDefault
    -- * The pure transition
  , Engine (..)
  , DdrState (..)
  , ddrInit
  , DdrIn (..)
  , DdrOut (..)
  , DdrStatus (..)
  , ddrStep
    -- * As a circuit
  , ddrReadPort
  ) where

import Clash.Prelude

import Platform.Mem.Axi

-- | A byte address in DDR (as the PS sees it).
type DdrAddr = AxiAddr

-- | A 32-byte line: the address without its low 5 bits.
type Line = Unsigned 27

-- | One of a line's four 8-byte beats.
type Beat = Unsigned 2

lineOf :: DdrAddr -> Line
lineOf a = resize (a `shiftR` 5)

beatOf :: DdrAddr -> Beat
beatOf a = resize (a `shiftR` 3)

lineAddr :: Line -> DdrAddr
lineAddr l = resize l `shiftL` 5

-- | The byte at an address, from the word the port answered with.
ddrByte :: DdrAddr -> AxiData -> BitVector 8
ddrByte a w = resize (w `shiftR` (8 * fromIntegral (a .&. 7)))

newtype DdrConfig = DdrConfig
  { dcPrefetch :: Bool   -- ^ read the next line ahead
  } deriving stock (Show, Eq)

ddrConfigDefault :: DdrConfig
ddrConfigDefault = DdrConfig { dcPrefetch = True }

-- | The bus side: nothing to read, an AR waiting for ARREADY, or a burst
-- coming in (the next beat expected).
data Engine
  = EIdle
  | ERequest Line
  | EFill Line Beat
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFDataX

data DdrState k = DdrState
  { dsTags     :: Vec (2 ^ k) (Maybe Line)  -- ^ which line each slot holds
  , dsEngine   :: Engine
  , dsLook     :: Bool          -- ^ a hit was read last clock: answer from the RAM now
  , dsMiss     :: Maybe DdrAddr -- ^ the outstanding request, waiting for a fill
  , dsPrefetch :: Maybe Line    -- ^ the line to read ahead, when idle
  , dsStale    :: Bool          -- ^ the fill under way began before an invalidation
  , dsError    :: Bool
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFDataX

ddrInit :: KnownNat k => DdrState k
ddrInit = DdrState (repeat Nothing) EIdle False Nothing Nothing False False

data DdrIn = DdrIn
  { diReq   :: Maybe DdrAddr
  , diInval :: Bool
  , diAxi   :: AxiRdS2M
  , diRam   :: AxiData     -- ^ the block RAM's output (the address of last clock)
  } deriving stock (Show, Eq, Generic)

data DdrStatus = DdrStatus
  { stBusy  :: Bool   -- ^ a burst under way, or a request waiting for one
  , stError :: Bool
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFDataX

data DdrOut k = DdrOut
  { doResp   :: Maybe AxiData
  , doAxi    :: AxiRdM2S
  , doRamRd  :: Unsigned (k + 2)
  , doRamWr  :: Maybe (Unsigned (k + 2), AxiData)
  , doStatus :: DdrStatus
  } deriving stock (Show, Eq, Generic)

-- | One clock. The AXI outputs depend only on the state (no path from
-- ARREADY or RVALID to ARVALID or RREADY).
ddrStep
  :: forall k
   . (KnownNat k, 1 <= k)
  => DdrConfig -> DdrState k -> DdrIn -> (DdrState k, DdrOut k)
ddrStep cfg s ~(DdrIn req inval (AxiRdS2M arReady rIn) ramQ) = (sInv, out)
 where
  slotOf :: Line -> Unsigned k
  slotOf = resize
  ramAddr :: Line -> Beat -> Unsigned (k + 2)
  ramAddr l b = unpack (pack (slotOf l) ++# pack b)
  cached l = dsTags s !! slotOf l == Just l

  -- A new request: a hit is read from the RAM now (answered next clock),
  -- a miss waits for a fill. Either way, queue the line after it.
  hit = maybe False (cached . lineOf) req
  miss0 = case req of
    Just a | not hit, Nothing <- dsMiss s -> Just a
    _ -> dsMiss s
  reqRd = maybe 0 (\a -> ramAddr (lineOf a) (beatOf a)) req
  prefetch0
    | dcPrefetch cfg, Just a <- req = Just (lineOf a + 1)
    | otherwise = dsPrefetch s
  s1 = s { dsMiss = miss0, dsPrefetch = prefetch0, dsLook = hit }

  start l = s1 { dsEngine = ERequest l
               , dsTags = replace (slotOf l) Nothing (dsTags s)
               , dsStale = False
               , dsPrefetch = if prefetch0 == Just l then Nothing else prefetch0 }

  -- The bus side. What it answers and writes depends only on the state
  -- and the bus: a request is answered from an earlier clock's state, so
  -- there is no path from a request to an answer in the same clock.
  stale = dsStale s
  waiting = dsMiss s
  beat = case (dsEngine s, rIn) of
    (EFill l b, Just r) -> Just (l, b, r)
    _ -> Nothing
  final = maybe False (\(_, b, _) -> b == maxBound) beat
  here = case (waiting, beat) of
    (Just a, Just (l, _, _)) -> not stale && lineOf a == l
    _ -> False
  -- the waiting request's word is this beat: answer it from the bus ...
  now = here && fmap beatOf waiting == fmap (\(_, b, _) -> b) beat
  -- ... or it came after its beat went by: read it from the RAM once the
  -- line is complete
  relook = here && not now && final
  -- a request that came with the last beat of its own line waited for
  -- nothing: the line is in now
  idleHit = case (dsEngine s, waiting) of
    (EIdle, Just a) -> cached (lineOf a)
    _ -> False
  beatResp = case beat of
    Just (_, _, r) | now -> Just (rData r)
    _ -> Nothing
  wr = fmap (\(l, b, r) -> (ramAddr l b, rData r)) beat
  rd = case waiting of
    Just a | relook || idleHit -> ramAddr (lineOf a) (beatOf a)
    _ -> reqRd

  s2 = case dsEngine s of
    EIdle
      | idleHit -> s1 { dsMiss = Nothing, dsLook = True }
      | Just a <- miss0 -> start (lineOf a)
      | Just l <- prefetch0, not (cached l) -> start l
      | otherwise -> s1 { dsPrefetch = Nothing }
    ERequest l
      | arReady -> s1 { dsEngine = EFill l 0 }
      | otherwise -> s1
    EFill l b -> case rIn of
      Nothing -> s1
      Just r -> s1
        { dsEngine = if final then EIdle else EFill l (b + 1)
        , dsTags = if final && not stale then replace (slotOf l) (Just l) (dsTags s) else dsTags s
        , dsMiss = if now || relook then Nothing else miss0
        , dsLook = dsLook s1 || relook
        , dsError = dsError s || rResp r /= respOkay || rLast r /= final }

  sInv
    | inval = s2 { dsTags = repeat Nothing
                 , dsPrefetch = Nothing
                 , dsStale = dsEngine s2 /= EIdle }
    | otherwise = s2

  out = DdrOut
    { doResp = if dsLook s then Just ramQ else beatResp
    , doAxi = AxiRdM2S
        { m2sAr = case dsEngine s of
            ERequest l -> Just (AxiAr (lineAddr l) 3)
            _ -> Nothing
        , m2sRReady = case dsEngine s of
            EFill _ _ -> True
            _ -> False
        }
    , doRamRd = rd
    , doRamWr = wr
    , doStatus = DdrStatus
        { stBusy = dsEngine s /= EIdle || dsMiss s /= Nothing
        , stError = dsError s }
    }

-- | The read port: @ddrReadPort k cfg inval req s2m@ gives the answers,
-- the AXI master's outputs and the status. @2^k@ lines of 32 bytes in
-- one block RAM (k = 4: 512 bytes, one RAMB18).
ddrReadPort
  :: forall k dom
   . (HiddenClockResetEnable dom, KnownNat k, 1 <= k)
  => SNat k
  -> DdrConfig
  -> Signal dom Bool             -- ^ invalidate
  -> Signal dom (Maybe DdrAddr)  -- ^ requests
  -> Signal dom AxiRdS2M
  -> (Signal dom (Maybe AxiData), Signal dom AxiRdM2S, Signal dom DdrStatus)
ddrReadPort SNat cfg inval req s2m = (doResp <$> o, doAxi <$> o, doStatus <$> o)
 where
  o = mealy (ddrStep @k cfg) ddrInit (DdrIn <$> req <*> inval <*> s2m <*> ramQ)
  ramQ = blockRamPow2 (repeat 0) (doRamRd <$> o) (doRamWr <$> o)
