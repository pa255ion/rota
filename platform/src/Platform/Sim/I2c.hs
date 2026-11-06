{-|
A pure I2C slave model for tests (simulation only, not hardware): a
register-file device that sees the bus one clock at a time, like a real
chip on the wire. Used against "Platform.Bus.I2c" (PLAN.md M0d step 1)
and, later, the ADV7511 init sequencer.

The device has 256 byte registers. A write transaction sets the register
pointer with its first data byte and writes the following bytes from
there (pointer incrementing); a read returns bytes from the pointer
onwards until the master NACKs. Options ('SlaveConfig') make it refuse
writes to some registers, stretch the clock, or hold SCL low for good,
so the master's error paths can be tested. Every completed access is
logged ('SlaveEvent').

Like a real device, it takes any SDA change while SCL is high as a START
or STOP, so a master that moves SDA at the wrong time shows up as extra
'EvStart' / 'EvStop' entries in the log; 'slErrors' records clock pulses
that don't fit the byte framing.
-}
module Platform.Sim.I2c
  ( SlaveConfig (..)
  , slaveConfig
  , SlaveEvent (..)
  , Slave (..)
  , slaveInit
  , slaveStep
  , busLines
  , Run (..)
  , runTxns
  ) where

import Prelude

import qualified Data.Map.Strict as M
import Data.Bits (shiftL, testBit, (.|.))
import Data.Word (Word8)

import qualified Clash.Prelude as C

import Platform.Bus.I2c

data SlaveConfig = SlaveConfig
  { scAddr    :: Word8     -- ^ 7-bit device address
  , scNackReg :: [Word8]   -- ^ writes to these registers are NACKed
  , scBadPtr  :: [Word8]   -- ^ these register numbers are NACKed as the pointer byte
  , scStretch :: Int       -- ^ clocks to hold SCL low after each falling edge
  , scStuck   :: Bool      -- ^ hold SCL low for good after the first START
  , scRegs    :: M.Map Word8 Word8  -- ^ initial register contents (others 0)
  } deriving (Show, Eq)

slaveConfig :: Word8 -> SlaveConfig
slaveConfig a = SlaveConfig a [] [] 0 False M.empty

data SlaveEvent
  = EvStart
  | EvStop
  | EvWrite Word8 Word8   -- ^ register, value
  | EvRead Word8 Word8    -- ^ register, value sent
  | EvNack Word8          -- ^ refused a write to this register
  deriving (Show, Eq)

-- | What the byte being received is for.
data Target = TAddr | TPtr | TData
  deriving (Show, Eq)

data Mode
  = MIdle                      -- ^ not addressed: wait for START
  | MRecv Target Int Word8     -- ^ bits received so far, value
  | MAck Bool (Maybe Target)   -- ^ ninth slot: ACK driven?, then receive this (Nothing: send)
  | MSend Int Word8            -- ^ sending bit n (0 = MSB)
  | MMasterAck (Maybe Bool)    -- ^ ninth slot of a read: the master's answer once sampled
  deriving (Show, Eq)

data Slave = Slave
  { slCfg     :: SlaveConfig
  , slRegs    :: M.Map Word8 Word8
  , slPtr     :: Word8
  , slMode    :: Mode
  , slPrev    :: (C.Bit, C.Bit)   -- ^ bus (SCL, SDA) in the previous clock
  , slSdaLow  :: Bool
  , slHold    :: Int              -- ^ clocks of SCL stretching left
  , slStuck   :: Bool
  , slLog     :: [SlaveEvent]     -- ^ newest first
  , slErrors  :: [String]         -- ^ newest first
  } deriving (Show, Eq)

-- | So that the model can sit in a Clash 'register' in circuit tests. The
-- slave is never undefined, so the methods are trivial.
instance C.NFDataX Slave where
  deepErrorX    = C.errorX
  hasUndefined  = const False
  ensureSpine   = id
  rnfX          = (`seq` ())

slaveInit :: SlaveConfig -> Slave
slaveInit cfg = Slave cfg (scRegs cfg) 0 MIdle (1, 1) False 0 False [] []

-- | The bus levels: open drain with pull-ups, so a line is low if anyone
-- pulls it low. Arguments: master SCL low, master SDA low, the slave.
busLines :: Bool -> Bool -> Slave -> I2cLines
busLines mScl mSda s = I2cLines (lvl (mScl || slHold s > 0 || slStuck s)) (lvl (mSda || slSdaLow s))
 where lvl low = if low then 0 else 1

reg :: Slave -> Word8 -> Word8
reg s r = M.findWithDefault 0 r (slRegs s)

-- | One clock, seeing the bus as it is in this clock. The slave's own
-- outputs ('slSdaLow', SCL hold) change for the next clock.
slaveStep :: Slave -> I2cLines -> Slave
slaveStep s0 (I2cLines scl sda) = s1 { slPrev = (scl, sda) }
 where
  (pScl, pSda) = slPrev s0
  rising  = pScl == 0 && scl == 1
  falling = pScl == 1 && scl == 0
  s = s0 { slHold = max 0 (slHold s0 - 1) }

  s1
    | scl == 1 && pScl == 1 && pSda == 1 && sda == 0 =     -- START
        s { slMode = MRecv TAddr 0 0, slSdaLow = False, slStuck = scStuck (slCfg s)
          , slLog = EvStart : slLog s }
    | scl == 1 && pScl == 1 && pSda == 0 && sda == 1 =     -- STOP
        s { slMode = MIdle, slSdaLow = False, slLog = EvStop : slLog s }
    | rising    = onRising
    | falling   = stretch onFalling
    | otherwise = s

  stretch t = t { slHold = scStretch (slCfg s) }

  onRising = case slMode s of
    MRecv t n v | n < 8 -> s { slMode = MRecv t (n + 1) ((v `shiftL` 1) .|. bit sda) }
    MMasterAck Nothing  -> s { slMode = MMasterAck (Just (sda == 0)) }
    MRecv _ _ _         -> err "SCL pulse after 8 bits before the ACK slot"
    _                   -> s

  onFalling = case slMode s of
    MRecv t 8 v -> byteDone t v
    MAck acked next
      | not acked -> s { slMode = MIdle, slSdaLow = False }
      | otherwise -> case next of
          Just t  -> s { slMode = MRecv t 0 0, slSdaLow = False }
          Nothing -> send s
    MSend 7 _ -> s { slMode = MMasterAck Nothing, slSdaLow = False }
    MSend n b -> s { slMode = MSend (n + 1) b, slSdaLow = not (testBit b (6 - n)) }
    MMasterAck (Just True) -> send s
    MMasterAck _ -> s { slMode = MIdle, slSdaLow = False }
    _ -> s

  -- A byte has been received: drive (or not) the ACK in the ninth slot.
  byteDone t v = case t of
    TAddr
      | v `div` 2 /= scAddr (slCfg s) -> s { slMode = MAck False Nothing }
      | odd v     -> ack Nothing
      | otherwise -> ack (Just TPtr)
    TPtr
      | v `elem` scBadPtr (slCfg s) -> s { slMode = MAck False Nothing }
      | otherwise -> (ack (Just TData)) { slPtr = v }
    TData
      | slPtr s `elem` scNackReg (slCfg s) ->
          s { slMode = MAck False Nothing, slLog = EvNack (slPtr s) : slLog s }
      | otherwise ->
          (ack (Just TData))
            { slRegs = M.insert (slPtr s) v (slRegs s), slPtr = slPtr s + 1
            , slLog = EvWrite (slPtr s) v : slLog s }
   where ack next = s { slMode = MAck True next, slSdaLow = True }

  -- Start sending the byte at the pointer (MSB now).
  send t =
    let b = reg t (slPtr t)
    in t { slMode = MSend 0 b, slSdaLow = not (testBit b 7), slPtr = slPtr t + 1
         , slLog = EvRead (slPtr t) b : slLog t }

  bit b = if b == 1 then 1 else 0
  err m = s { slErrors = m : slErrors s }

-- | The outcome of 'runTxns'.
data Run = Run
  { runResults :: [I2cResult]   -- ^ in the order of the transactions
  , runSlave   :: Slave         -- ^ at the end
  , runClocks  :: Int
  , runBus     :: [I2cLines]    -- ^ the bus in every clock
  }

-- | Run transactions one after the other on a master and a slave, until all
-- have finished or @maxClocks@ have passed. The master reads the bus
-- through a two-clock delay, as through 'Platform.Input.Sync.synchronise'.
runTxns :: I2cTiming -> SlaveConfig -> Int -> [I2cTxn] -> Run
runTxns t cfg maxClocks txns0 = go 0 i2cInit (slaveInit cfg) (idleL, idleL) txns0 [] []
 where
  idleL = I2cLines 1 1
  go n m sl (d1, d2) txns acc trace
    | n >= maxClocks || (null txns && ioReady o && length acc == length txns0) =
        Run (reverse acc) sl n (reverse trace)
    | otherwise =
        let bus  = busLines (ioSclLow o) (ioSdaLow o) sl
            (offer, rest) = case txns of
              x : xs | ioReady o -> (Just x, xs)
              _                  -> (Nothing, txns)
            m'   = i2cStep t m (d2, offer)
            sl'  = slaveStep sl bus
            acc' = maybe acc (: acc) (ioDone o)
        in go (n + 1) m' sl' (bus, d1) rest acc' (bus : trace)
   where
    o = i2cOut m
