{-|
The DDR test of "M0e.Top" (side track C, step 3): reads a buffer in DDR
through "Platform.Mem.Ddr", one 8-byte word per request, keeps a checksum
of what it read, and measures each request's latency (clocks from the
request to the answer, as a core sees it) into a histogram. The PS sets it
up and reads the results over the PS link (platform/ps/ddrtest.zig).

__Registers__ (PS link writes, little-endian):

* 0x000000: base address (4 bytes);
* 0x000004: number of words to read (4 bytes);
* 0x000008: mode: bits 0 to 2 the stride, @8 << s@ bytes; bit 7 random
  addresses instead: @base + (x .&. mask .&. complement 7)@ with @x@ from
  'xorshift32' seeded with 'rngSeed', the next number per request;
* 0x00000C: the mask for random addresses (4 bytes);
* 0x000010: command: bit 0 starts a run (results cleared, the cache
  emptied), bit 1 only empties the cache.

__Results__: a write to 0x010000 + i (any data byte) selects result byte
i, which the PL then sends on @PS_IN@'s byte channel (bits 24 to 31, the
toggle in bit 5 flips once per selection; "M0e.Top"). Bytes, little-endian
('resultByte'):

* 0: status: bit 0 running, bit 1 done, bit 2 AXI error (sticky);
* 4: checksum (4 bytes; 'checksumStep' over the words in request order);
* 8: clocks from the start to the last answer (4 bytes);
* 12: shortest latency, 14: longest (2 bytes each);
* 16: words read (4 bytes);
* 32 + 4 b: histogram bucket b (4 bytes each, 'buckets' of them; 'bucket').
-}
module M0e.Tester
  ( Config (..)
  , Results (..)
  , TState (..)
  , TIn (..)
  , TOut (..)
  , Buckets
  , buckets
  , bucket
  , rngSeed
  , xorshift32
  , checksumStep
  , testerInit
  , testerStep
  , resultByte
  , resultsBase
  , tester
  ) where

import Clash.Prelude

import Platform.Mem.Axi
import Platform.Mem.Ddr
import Platform.Ps.Link (LinkAddr, PsByteOut (..))

data Config = Config
  { cfBase  :: Unsigned 32
  , cfCount :: Unsigned 32
  , cfMode  :: BitVector 8
  , cfMask  :: Unsigned 32
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFDataX

-- | Latency buckets: 0 to 31 one per clock count, then one per power of
-- two (32 to 63, 64 to 127, ..., 32768 to 65535; latencies saturate at
-- 65535).
type Buckets = 43

buckets :: SNat Buckets
buckets = SNat

bucket :: Unsigned 16 -> Index Buckets
bucket l
  | l < 32 = fromIntegral l
  | otherwise = fromIntegral (32 + top - 5)
 where
  top = 15 - countLeadingZeros l :: Int

data Results = Results
  { rsRunning :: Bool
  , rsDone    :: Bool
  , rsSum     :: BitVector 32
  , rsCycles  :: Unsigned 32
  , rsMin     :: Unsigned 16
  , rsMax     :: Unsigned 16
  , rsCount   :: Unsigned 32
  , rsHist    :: Vec Buckets (Unsigned 32)
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFDataX

resultsInit :: Results
resultsInit = Results False False 0 0 maxBound 0 0 (repeat 0)

data TState = TState
  { tsCfg     :: Config
  , tsRes     :: Results
  , tsIssued  :: Unsigned 32   -- ^ requests made in this run
  , tsWaiting :: Bool          -- ^ a request is outstanding
  , tsLat     :: Unsigned 16   -- ^ its age in clocks
  , tsOffset  :: Unsigned 32   -- ^ the next sequential offset
  , tsRng     :: BitVector 32
  , tsSel     :: Unsigned 8    -- ^ the result byte the PS selected
  , tsTog     :: Bool          -- ^ flips with every selection
  , tsInval   :: Bool          -- ^ empty the cache (one clock)
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFDataX

testerInit :: TState
testerInit = TState (Config 0 0 0 0) resultsInit 0 False 0 0 rngSeed 0 False False

data TIn = TIn
  { tiLink :: Maybe (LinkAddr, BitVector 8)
  , tiResp :: Maybe AxiData
  } deriving stock (Show, Eq, Generic)

data TOut = TOut
  { toReq   :: Maybe DdrAddr
  , toInval :: Bool
  , toSel   :: Unsigned 8
  , toTog   :: Bool
  , toRes   :: Results
  } deriving stock (Show, Eq, Generic)

rngSeed :: BitVector 32
rngSeed = 0x2545F491

-- | xorshift32 (Marsaglia 2003, shifts 13, 17, 5).
xorshift32 :: BitVector 32 -> BitVector 32
xorshift32 x0 = x3
 where
  x1 = x0 `xor` (x0 `shiftL` 13)
  x2 = x1 `xor` (x1 `shiftR` 17)
  x3 = x2 `xor` (x2 `shiftL` 5)

-- | The checksum after one more word: rotate left by one, then xor both
-- halves in.
checksumStep :: BitVector 32 -> AxiData -> BitVector 32
checksumStep c w = (c `rotateL` 1) `xor` slice d31 d0 w `xor` slice d63 d32 w

-- | The result bytes start here on the link.
resultsBase :: LinkAddr
resultsBase = 0x010000

-- | Replace byte @i@ (0 to 3) of a little-endian word.
setByte :: Unsigned 32 -> LinkAddr -> BitVector 8 -> Unsigned 32
setByte w i d = unpack (pack w .&. complement mask .|. (resize d `shiftL` sh))
 where
  sh = 8 * fromIntegral (i .&. 3)
  mask = 0xFF `shiftL` sh :: BitVector 32

-- | The request the state asks for (from the state alone).
request :: TState -> Maybe DdrAddr
request s
  | rsRunning (tsRes s) && not (tsWaiting s) && tsIssued s < cfCount (tsCfg s) =
      Just (cfBase (tsCfg s) + off)
  | otherwise = Nothing
 where
  off | testBit (cfMode (tsCfg s)) 7 = unpack (tsRng s) .&. cfMask (tsCfg s) .&. complement 7
      | otherwise = tsOffset s

testerStep :: TState -> TIn -> (TState, TOut)
testerStep s ~(TIn link resp) = (s', TOut req (tsInval s) (tsSel s) (tsTog s) (tsRes s))
 where
  req = request s
  cfg = tsCfg s
  res = tsRes s
  random = testBit (cfMode cfg) 7
  stride = 8 `shiftL` fromIntegral (slice d2 d0 (cfMode cfg)) :: Unsigned 32

  -- the run: a request goes out, its answer comes back
  issued = case req of
    Just _ -> s { tsIssued = tsIssued s + 1, tsWaiting = True, tsLat = 1
                , tsOffset = tsOffset s + stride
                , tsRng = if random then xorshift32 (tsRng s) else tsRng s }
    Nothing
      | tsWaiting s -> s { tsLat = if tsLat s == maxBound then maxBound else tsLat s + 1 }
      | otherwise -> s
  answered = case resp of
    Just w | tsWaiting s ->
      let lat = tsLat s
          n = rsCount res + 1
          r = res { rsSum = checksumStep (rsSum res) w
                  , rsMin = min lat (rsMin res)
                  , rsMax = max lat (rsMax res)
                  , rsCount = n
                  , rsHist = replace (bucket lat) (rsHist res !! bucket lat + 1) (rsHist res)
                  , rsRunning = n < cfCount cfg
                  , rsDone = n >= cfCount cfg }
      in issued { tsWaiting = False, tsRes = r }
    _ -> issued
  ticking = if rsRunning (tsRes s)
              then answered { tsRes = (tsRes answered) { rsCycles = rsCycles (tsRes s) + 1 } }
              else answered
  -- (the cycle count includes the last answer's clock)

  -- the PS writes a register
  s' = case link of
    Just (a, d)
      | a < 4 -> run0 { tsCfg = cfg { cfBase = setByte (cfBase cfg) a d } }
      | a < 8 -> run0 { tsCfg = cfg { cfCount = setByte (cfCount cfg) a d } }
      | a == 8 -> run0 { tsCfg = cfg { cfMode = d } }
      | a >= 0xC && a < 0x10 -> run0 { tsCfg = cfg { cfMask = setByte (cfMask cfg) a d } }
      | a == 0x10, testBit d 0 ->
          run0 { tsRes = resultsInit { rsRunning = cfCount cfg /= 0, rsDone = cfCount cfg == 0 }
               , tsIssued = 0, tsWaiting = False, tsOffset = 0, tsRng = rngSeed, tsInval = True }
      | a == 0x10, testBit d 1 -> run0 { tsInval = True }
      | a >= resultsBase && a < resultsBase + 256 -> run0 { tsSel = resize a, tsTog = not (tsTog s) }
      | otherwise -> run0
    _ -> run0
  run0 = ticking { tsInval = False }

-- | Result byte @i@ (layout above); @err@ is the port's AXI error flag.
resultByte :: Bool -> Results -> Unsigned 8 -> BitVector 8
resultByte err r i = bytes !! i
 where
  status = (0 :: BitVector 5) ++# pack err ++# pack (rsDone r) ++# pack (rsRunning r)
  le32 :: Unsigned 32 -> Vec 4 (BitVector 8)
  le32 = reverse . unpack . pack
  le16 :: Unsigned 16 -> Vec 2 (BitVector 8)
  le16 = reverse . unpack . pack
  header = (status :> 0 :> 0 :> 0 :> Nil)
        ++ le32 (unpack (rsSum r)) ++ le32 (rsCycles r)
        ++ le16 (rsMin r) ++ le16 (rsMax r) ++ le32 (rsCount r)
        ++ (repeat 0 :: Vec 12 (BitVector 8))
  hist = concatMap le32 (rsHist r)
  bytes = header ++ hist ++ (repeat 0 :: Vec (256 - 32 - 4 * Buckets) (BitVector 8))

-- | The test with its read port (@2^4@ lines): link writes and the HP
-- port in; the selected result byte, the AXI master's outputs and the
-- results out.
tester
  :: HiddenClockResetEnable dom
  => Signal dom (Maybe (LinkAddr, BitVector 8))
  -> Signal dom AxiRdS2M
  -> (Signal dom PsByteOut, Signal dom AxiRdM2S, Signal dom (Results, Bool))
tester link s2m = (byte, m2s, bundle (toRes <$> o, stError <$> stat))
 where
  o = mealy testerStep testerInit (TIn <$> link <*> resp)
  (resp, m2s, stat) = ddrReadPort d4 ddrConfigDefault (toInval <$> o) (toReq <$> o) s2m
  byte = register (PsByteOut 0 False)
    (PsByteOut <$> (resultByte <$> (stError <$> stat) <*> (toRes <$> o) <*> (toSel <$> o)) <*> (toTog <$> o))
