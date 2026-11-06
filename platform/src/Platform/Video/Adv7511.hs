{-# LANGUAGE AllowAmbiguousTypes #-}
{-|
ADV7511 HDMI transmitter set-up (PLAN.md M0d step 2), entirely in the PL:
an init sequencer on top of the I2C master ("Platform.Bus.I2c") that
writes the register table of "Platform.Video.Adv7511.Table".

__Sequence.__ After reset, wait 'atBoot'; read the chip revision; then
poll the status register (HPD, monitor sense) every 'atPoll'. When HPD is
high and the chip isn't configured, apply the table, entry by entry
(entries with a partial mask are read first and updated). Once configured,
every poll also reads the power register: the ADV7511 resets most of its
registers while HPD is low (Linux driver: "Most of the registers are reset
during power down or when HPD is low"), so if the power-down bit is set
again, or a poll sees HPD low, the table is applied again as soon as HPD
is back. An I2C error (no ACK, SCL stuck) abandons the table, waits
'atRetry' and starts again from the revision read: a missing chip is
reported, never a hang.

__Status__ ('AdvStatus', for the LEDs): configured, HPD, monitor sense,
the last I2C error (sticky until the next complete configuration), the
chip revision read back, and a count of completed configurations.
-}
module Platform.Video.Adv7511
  ( AdvTiming (..)
  , advTiming
  , AdvStatus (..)
  , statusWord
  , AdvPhase (..)
  , AdvState (..)
  , advInit
  , advStep
  , advPins
  , advStatus
  , adv7511Init
  , adv7511InitWith
  , advStepWith
  , tableVec
  , tableVecHdmiAudio
  , tableLast
  ) where

import Clash.Prelude

import Platform.Bus.I2c
import Platform.Input.Sync (synchronise)
import Platform.Video.Adv7511.Table

-- | Waits, in clocks of the sequencer's domain.
data AdvTiming = AdvTiming
  { atBoot  :: Unsigned 24   -- ^ after reset, before the first access
  , atPoll  :: Unsigned 24   -- ^ between status polls
  , atRetry :: Unsigned 24   -- ^ after an I2C error
  , atI2c   :: I2cTiming
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFDataX

-- | Domain @dom@: 200 ms boot wait, polls at 20 Hz, 100 ms retry, I2C at
-- 100 kHz. (The Programming Guide, 4.1: wait 200 ms after power-up before
-- the chip's I2C address is decided.)
advTiming :: forall dom. KnownDomain dom => AdvTiming
advTiming = AdvTiming
  { atBoot = ms 200, atPoll = ms 50, atRetry = ms 100, atI2c = i2cTiming @dom }
 where
  period = snatToNum (clockPeriod @dom) :: Integer
  ms n   = fromInteger ((n * 1000000000) `div` period)

data AdvStatus = AdvStatus
  { asConfigured :: Bool
  , asHpd        :: Bool
  , asSense      :: Bool
  , asError      :: Maybe I2cError
  , asRevision   :: BitVector 8
  , asInits      :: Unsigned 4      -- ^ completed configurations (wraps)
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFDataX

-- | 16 bits for 'Platform.Output.Leds.debugByte': bit 0 configured, 1 HPD,
-- 2 monitor sense, 3 any error, 5..4 the error (0 address NACK, 1
-- register NACK, 2 data NACK, 3 timeout), 7..6 configurations (low two
-- bits), 15..8 revision.
statusWord :: AdvStatus -> BitVector 16
statusWord s =
  asRevision s ++# slice d1 d0 (pack (asInits s)) ++# err
    ++# pack (asSense s) ++# pack (asHpd s) ++# pack (asConfigured s)
 where
  err :: BitVector 3
  err = case asError s of
    Nothing         -> 0b000
    Just NackAddr   -> 0b001
    Just NackReg    -> 0b011
    Just NackData   -> 0b101
    Just I2cTimeout -> 0b111

-- | The table as hardware: (register, mask, value).
tableVec :: Vec 40 (BitVector 8, BitVector 8, BitVector 8)
tableVec = $(listToVecTH (fmap (\e -> (teReg e, teMask e, teValue e)) adv7511Table))

tableLast :: Unsigned 8
tableLast = fromIntegral (length tableVec - 1)

-- | HDMI mode with S/PDIF audio ('adv7511HdmiAudioTable').
tableVecHdmiAudio :: Vec 57 (BitVector 8, BitVector 8, BitVector 8)
tableVecHdmiAudio = $(listToVecTH (fmap (\e -> (teReg e, teMask e, teValue e)) adv7511HdmiAudioTable))

data AdvPhase
  = AWait (Unsigned 24) AdvPhase'  -- ^ clocks left, then the access
  | AAccess AdvPhase'              -- ^ transaction offered or running
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFDataX

-- | The accesses of the sequence.
data AdvPhase'
  = ARevision
  | AStatus
  | APower
  | AEntryRead (Unsigned 8)
  | AEntryWrite (Unsigned 8) (BitVector 8)   -- ^ entry, old value (if masked)
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFDataX

data AdvState = AdvState
  { avI2c    :: I2cState
  , avPhase  :: AdvPhase
  , avSent   :: Bool         -- ^ the master has taken the current access
  , avStatus :: AdvStatus
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFDataX

advInit :: AdvTiming -> AdvState
advInit t = AdvState
  { avI2c = i2cInit, avPhase = AWait (atBoot t) ARevision, avSent = False
  , avStatus = AdvStatus False False False Nothing 0 0 }

-- | The I2C transaction of an access.
accessTxn :: KnownNat n => Vec n (BitVector 8, BitVector 8, BitVector 8) -> AdvPhase' -> I2cTxn
accessTxn tbl a = case a of
  ARevision       -> TxnRead adv7511Addr regRevision
  AStatus         -> TxnRead adv7511Addr regStatus
  APower          -> TxnRead adv7511Addr regPower
  AEntryRead k    -> let (r, _, _) = tbl !! k in TxnRead adv7511Addr r
  AEntryWrite k o -> let (r, m, v) = tbl !! k
                     in TxnWrite adv7511Addr r ((o .&. complement m) .|. (v .&. m))

-- | The first access for entry @k@: a read if its mask is partial.
entryStart :: KnownNat n => Vec n (BitVector 8, BitVector 8, BitVector 8) -> Unsigned 8 -> AdvPhase'
entryStart tbl k = let (_, m, _) = tbl !! k in if m == 0xFF then AEntryWrite k 0 else AEntryRead k

-- | One clock, with the I2C line levels (synchronised).
advStep :: AdvTiming -> AdvState -> I2cLines -> AdvState
advStep = advStepWith tableVec

-- | 'advStep' with a given register table.
advStepWith :: KnownNat n => Vec n (BitVector 8, BitVector 8, BitVector 8) -> AdvTiming -> AdvState -> I2cLines -> AdvState
advStepWith tbl t s lines' = case avPhase s of
  AWait n a
    | n == 0    -> s' { avPhase = AAccess a, avSent = False }
    | otherwise -> s' { avPhase = AWait (n - 1) a }
  AAccess a -> case ioDone o of
    Nothing          -> s' { avSent = avSent s || offer }
    Just (Left e)    -> (go (AWait (atRetry t) ARevision)) { avStatus = st { asConfigured = False, asError = Just e } }
    Just (Right v)   -> done a v
 where
  o      = i2cOut (avI2c s)
  st     = avStatus s
  offer  = case avPhase s of
    AAccess _ -> not (avSent s) && ioReady o
    _         -> False
  txn    = case avPhase s of
    AAccess a | offer -> Just (accessTxn tbl a)
    _                 -> Nothing
  s'     = s { avI2c = i2cStep (atI2c t) (avI2c s) (lines', txn) }
  go p   = s' { avPhase = p, avSent = False }
  poll   = AWait (atPoll t) AStatus

  done a v = case a of
    ARevision -> (go poll) { avStatus = st { asRevision = v } }
    AStatus
      | not hpd             -> (go poll) { avStatus = st' { asConfigured = False } }
      | asConfigured st     -> (go (AAccess APower)) { avStatus = st' }
      | otherwise           -> (go (AAccess (entryStart tbl 0))) { avStatus = st' }
     where
      hpd = testBit v 6
      st' = st { asHpd = hpd, asSense = testBit v 5 }
    APower
      | testBit v 6 -> (go (AAccess (entryStart tbl 0))) { avStatus = st { asConfigured = False } }
      | otherwise   -> go poll
    AEntryRead k -> go (AAccess (AEntryWrite k v))
    AEntryWrite k _
      | k == fromIntegral (length tbl - 1) ->
          (go poll) { avStatus = st { asConfigured = True, asError = Nothing, asInits = asInits st + 1 } }
      | otherwise -> go (AAccess (entryStart tbl (k + 1)))

-- | (SCL low, SDA low): pull the line low when True.
advPins :: AdvState -> (Bool, Bool)
advPins s = let o = i2cOut (avI2c s) in (ioSclLow o, ioSdaLow o)

advStatus :: AdvState -> AdvStatus
advStatus = avStatus

-- | The sequencer as a circuit: takes the I2C line levels from the pins
-- (synchronised here) and returns the open-drain controls and the status.
adv7511Init
  :: HiddenClockResetEnable dom
  => AdvTiming
  -> Signal dom I2cLines
  -> (Signal dom (Bool, Bool), Signal dom AdvStatus)
adv7511Init = adv7511InitWith tableVec

-- | The sequencer with a given register table (e.g. 'tableVecHdmiAudio').
adv7511InitWith
  :: (HiddenClockResetEnable dom, KnownNat n)
  => Vec n (BitVector 8, BitVector 8, BitVector 8)
  -> AdvTiming
  -> Signal dom I2cLines
  -> (Signal dom (Bool, Bool), Signal dom AdvStatus)
adv7511InitWith tbl t pins = (advPins <$> st, advStatus <$> st)
 where
  st = register (advInit t) (advStepWith tbl t <$> st <*> synchronise (I2cLines 1 1) pins)
