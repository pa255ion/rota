{-|
Simulation only: an AXI3 read slave standing in for a Zynq HP port and
its DDR, with random timing (side track C, step 1). Not synthesisable,
and not meant to be.

Per clock it decides, from a seeded random stream, whether ARREADY is
high, how long an accepted burst waits before its first beat, and the
gaps between beats; it accepts up to 'scDepth' bursts before the first
has finished. Data is read from the memory function when the burst is
accepted, so a memory that changes afterwards doesn't change a burst in
flight. Addresses for which 'scError' holds are answered with SLVERR.

Outputs depend only on the slave's state, never on the master's outputs
in the same clock, so the master and the slave can be wired in a loop.
-}
module Platform.Mem.Sim
  ( SlaveConfig (..)
  , slaveFast
  , slaveRandom
  , axiSlave
  , axiWrSlave
  , xorshift
  ) where

import Clash.Prelude
import qualified Prelude as P
import Data.Word (Word64)

import Platform.Mem.Axi

data SlaveConfig = SlaveConfig
  { scArWait  :: Int              -- ^ most clocks ARREADY stays low after a handshake
  , scLatency :: Int              -- ^ most clocks from AR to the first beat (beyond 1)
  , scGap     :: Int              -- ^ most idle clocks between beats
  , scDepth   :: Int              -- ^ bursts accepted ahead
  , scError   :: AxiAddr -> Bool  -- ^ beats from these addresses answer SLVERR
  }

-- | ARREADY always high, the first beat the clock after the AR, no gaps.
slaveFast :: SlaveConfig
slaveFast = SlaveConfig 0 0 0 4 (const False)

-- | Random waits up to the given latency (ARREADY up to 5 clocks late,
-- gaps up to 3 clocks, 2 bursts ahead).
slaveRandom :: Int -> SlaveConfig
slaveRandom lat = SlaveConfig 5 lat 3 2 (const False)

-- | @axiWrSlave cfg seed m2s@: an AXI3 write slave (side track O, O.4),
-- with the read slave's random timing ('scArWait' for AWREADY, 'scGap'
-- for WREADY, 'scLatency' before a burst's response, 'scDepth' bursts
-- accepted ahead): its outputs, and each write it takes (address, data,
-- strobes), in the clock it takes it. W beats are taken once their burst's
-- AW was. Outputs depend only on the slave's state.
axiWrSlave
  :: HiddenClockResetEnable dom
  => SlaveConfig
  -> Word64
  -> Signal dom AxiWrM2S
  -> (Signal dom AxiWrS2M, Signal dom (Maybe (AxiAddr, AxiData, BitVector 8)))
axiWrSlave cfg seed m2s = unbundle (mealy step s0 m2s)
 where
  s0 = WrState (xorshift (seed .|. 2)) 0 0 [] []
  rand n x = if n <= 0 then 0 else P.fromIntegral (x `mod` P.fromIntegral (n + 1))
  awReady s = wsAwWait s == 0 && P.length (wsBursts s) < scDepth cfg
  wReady s = wsWGap s == 0 && not (P.null (wsBursts s))
  bOut s = case wsResp s of
    0 : _ -> Just respOkay
    _ -> Nothing
  step s (AxiWrM2S aw w bReady) = (s', (AxiWrS2M (awReady s) (wReady s) (bOut s), taken))
   where
    r1 = xorshift (wsRng s)
    r2 = xorshift r1
    r3 = xorshift r2
    -- a W beat into the oldest burst
    (taken, bursts1, done)
      | Just (AxiW d strb _) <- w, wReady s, (a, n) : rest <- wsBursts s =
          let addr = a + 8 * P.fromIntegral n
          in ( Just (addr, d, strb)
             , if n == 0 then rest else (a, n - 1 :: Int) : rest
             , n == 0 )
      | otherwise = (Nothing, wsBursts s, False)
    -- countdowns, the B taken, a finished burst's B queued
    resp1 = case P.map (\x -> max 0 (x - 1)) (wsResp s) of
      r | bReady, 0 : _ <- wsResp s -> P.drop 1 r
        | otherwise -> r
    resp2 = if done then resp1 P.++ [rand (scLatency cfg) r1] else resp1
    -- a new burst: (address, beats still to come - 1)
    (bursts2, awWait')
      | Just (AxiAw a len) <- aw, awReady s = (bursts1 P.++ [(a, P.fromIntegral len)], rand (scArWait cfg) r2)
      | otherwise = (bursts1, max 0 (wsAwWait s - 1))
    gap'
      | P.not (P.null (wsBursts s)) && taken /= Nothing = rand (scGap cfg) r3
      | otherwise = max 0 (wsWGap s - 1)
    s' = WrState r3 awWait' gap' bursts2 resp2

data WrState = WrState
  { wsRng    :: Word64
  , wsAwWait :: Int
  , wsWGap   :: Int
  , wsBursts :: [(AxiAddr, Int)]  -- ^ accepted bursts: address, beats left - 1
  , wsResp   :: [Int]             -- ^ clocks before each pending response
  } deriving stock (Show, Generic)
    deriving anyclass NFDataX

-- | xorshift64 (Marsaglia 2003): the next state, which is also the number.
xorshift :: Word64 -> Word64
xorshift x0 = x3
 where
  x1 = x0 `xor` (x0 `shiftL` 13)
  x2 = x1 `xor` (x1 `shiftR` 7)
  x3 = x2 `xor` (x2 `shiftL` 17)

data Burst = Burst
  { buWait  :: Int                      -- ^ clocks before the first beat
  , buBeats :: [(AxiData, BitVector 2)] -- ^ beats still to send
  } deriving stock (Show, Generic)
    deriving anyclass NFDataX

data SlaveState = SlaveState
  { ssRng    :: Word64
  , ssArWait :: Int      -- ^ ARREADY is low while this is above 0
  , ssGap    :: Int      -- ^ clocks before the next beat may go out
  , ssQueue  :: [Burst]
  } deriving stock (Show, Generic)
    deriving anyclass NFDataX

-- | @axiSlave cfg seed mem m2s@: the slave's outputs. @mem@ is the memory
-- in each clock (8 bytes per aligned address).
axiSlave
  :: HiddenClockResetEnable dom
  => SlaveConfig
  -> Word64
  -> Signal dom (AxiAddr -> AxiData)
  -> Signal dom AxiRdM2S
  -> Signal dom AxiRdS2M
axiSlave cfg seed mem m2s = mealy step s0 ((,) <$> mem <*> m2s)
 where
  s0 = SlaveState (xorshift (seed .|. 1)) 0 0 []
  rand n x = if n <= 0 then 0 else P.fromIntegral (x `mod` P.fromIntegral (n + 1))

  arReady s = ssArWait s == 0 && P.length (ssQueue s) < scDepth cfg
  beat s = case ssQueue s of
    Burst 0 ((d, resp) : rest) : _ | ssGap s == 0 -> Just (AxiR d resp (P.null rest))
    _ -> Nothing

  step s ~(m, AxiRdM2S ar rReady) = (s', AxiRdS2M (arReady s) (beat s))
   where
    r1 = xorshift (ssRng s)
    r2 = xorshift r1
    r3 = xorshift r2
    -- the burst at the head: count down, send, drop when done
    sent = case beat s of
      Just _ -> rReady
      Nothing -> False
    q1 = case ssQueue s of
      Burst w bs : rest
        | sent -> case P.drop 1 bs of
            [] -> rest
            bs' -> Burst 0 bs' : rest
        | otherwise -> Burst (max 0 (w - 1)) bs : rest
      [] -> []
    q2 = case q1 of
      b : rest -> b : P.map (\x -> x { buWait = max 0 (buWait x - 1) }) rest
      [] -> []
    gap'
      | sent = rand (scGap cfg) r1
      | otherwise = max 0 (ssGap s - 1)
    -- a new burst
    (q3, arWait')
      | Just (AxiAr a len) <- ar, arReady s =
          let addrs = [ a + 8 * P.fromIntegral i | i <- [0 .. P.fromIntegral len :: Int] ]
              bs = [ (m (x .&. complement 7), if scError cfg x then 2 else respOkay) | x <- addrs ]
          in (q2 P.++ [Burst (rand (scLatency cfg) r2) bs], rand (scArWait cfg) r3)
      | otherwise = (q2, max 0 (ssArWait s - 1))
    s' = SlaveState r3 arWait' gap' q3
