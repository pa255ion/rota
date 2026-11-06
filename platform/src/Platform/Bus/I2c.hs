{-# LANGUAGE AllowAmbiguousTypes #-}
{-|
I2C master (PLAN.md M0d step 1): register writes and reads on a single
master bus, for the ADV7511 HDMI transmitter now and the ADAU1761 codec
later.

__Transactions.__ The user hands in one 'I2cTxn' at a time (when
'ioReady' is high) and gets one 'I2cResult' back on 'ioDone' (a one-cycle
pulse):

* @'TxnWrite' a r v@: START, address @a@ + W, register @r@, value @v@, STOP;
* @'TxnRead' a r@: START, @a@ + W, @r@, repeated START, @a@ + R, one byte
  read with NACK, STOP.

A NACK at any byte ends the transaction with a STOP and reports which byte
was refused ('NackAddr', 'NackReg', 'NackData'); the master is then ready
again, so a missing or unpowered device is an error, never a hang.

__Bus levels.__ Each bit is four phases of 'itQuarter' clocks: SCL falls;
SDA changes (so SDA never moves in the same clock as SCL); SCL is
released; SCL high. SDA is sampled in the last clock of the high time.
START and STOP take six phases, so that their set-up and hold times are
two quarters as well. With 'i2cTiming' a quarter is at least 2.5 µs: SCL
at most 100 kHz, low and high times 5 µs, START/STOP set-up and hold 5 µs
(standard mode asks for 4.7 µs and 4.0 µs).

__Clock stretching.__ After releasing SCL the master waits until it reads
SCL high before it counts the high time. If SCL stays low for more than
'itStretchMax' clocks, both lines are released and the transaction ends
with 'I2cTimeout'. There is no multi-master arbitration (the master never
checks that SDA follows what it drives).

__Pins.__ Open drain: 'ioSclLow' / 'ioSdaLow' True means pull the line
low, False means release it (the board's pull-ups make it high). They come
straight from registers in 'I2cState'. The line levels read back
('I2cLines') must be synchronised to the master's domain; 'i2cMaster' does
that with "Platform.Input.Sync".
-}
module Platform.Bus.I2c
  ( -- * Interface
    I2cTxn (..)
  , I2cError (..)
  , I2cResult
  , I2cLines (..)
  , I2cOut (..)
  , I2cTiming (..)
  , i2cTiming
    -- * Implementation
  , I2cOp (..)
  , I2cResp (..)
  , Slot (..)
  , Engine (..)
  , engineInit
  , engineStep
  , I2cState (..)
  , i2cInit
  , i2cStep
  , i2cOut
  , i2cMaster
  ) where

import Clash.Prelude

import Platform.Input.Sync (synchronise)

-- | One register access. Addresses are 7-bit device addresses.
data I2cTxn
  = TxnWrite (BitVector 7) (BitVector 8) (BitVector 8)  -- ^ device, register, value
  | TxnRead  (BitVector 7) (BitVector 8)                -- ^ device, register
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFDataX

data I2cError
  = NackAddr     -- ^ no device answered the address (write or read phase)
  | NackReg      -- ^ the register byte was refused
  | NackData     -- ^ the value byte was refused (writes only)
  | I2cTimeout   -- ^ SCL held low longer than 'itStretchMax'
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFDataX

-- | The byte read (0 for writes), or what went wrong.
type I2cResult = Either I2cError (BitVector 8)

-- | Line levels read back from the bus (after synchronisation).
data I2cLines = I2cLines
  { ilScl :: Bit
  , ilSda :: Bit
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFDataX

data I2cOut = I2cOut
  { ioSclLow :: Bool               -- ^ pull SCL low
  , ioSdaLow :: Bool               -- ^ pull SDA low
  , ioReady  :: Bool               -- ^ a transaction offered now is taken
  , ioDone   :: Maybe I2cResult    -- ^ the transaction just finished
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFDataX

data I2cTiming = I2cTiming
  { itQuarter    :: Unsigned 16   -- ^ clocks per phase, at least 3 (the input synchroniser lags 2)
  , itStretchMax :: Unsigned 24   -- ^ longest SCL low a slave may hold, in clocks
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFDataX

-- | Standard mode for domain @dom@: a quarter of at least 2.5 µs (SCL at
-- most 100 kHz; 63 clocks at the 25.18 MHz pixel clock) and a 10 ms
-- stretch limit.
i2cTiming :: forall dom. KnownDomain dom => I2cTiming
i2cTiming = I2cTiming
  { itQuarter    = fromInteger (ceilDiv 2500000 period)
  , itStretchMax = fromInteger (ceilDiv 10000000000 period)
  }
 where
  period = snatToNum (clockPeriod @dom) :: Integer
  ceilDiv a b = (a + b - 1) `div` b

------------------------------------------------------------------------
-- Byte engine

-- | Byte-level operations of the engine.
data I2cOp
  = OpStart              -- ^ START, or repeated START while the bus is held
  | OpStop
  | OpWrite (BitVector 8)
  | OpRead Bool          -- ^ read a byte; True = answer ACK (more to come)
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFDataX

data I2cResp
  = RespOk               -- ^ START or STOP done
  | RespAck Bool         -- ^ byte written; True = the slave ACKed
  | RespByte (BitVector 8)
  | RespTimeout
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFDataX

-- | What the engine is doing on the bus.
data Slot
  = SIdle
  | SStart
  | SStop
  | SBit Bit Bool        -- ^ SDA level to drive (1 = release), sample it?
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFDataX

data Engine = Engine
  { eSlot    :: Slot
  , ePhase   :: Index 6
  , eCount   :: Unsigned 16      -- ^ clocks left in this phase
  , eStretch :: Unsigned 24      -- ^ clocks spent waiting for SCL high
  , eBits    :: Index 10         -- ^ bit slots left in the current byte op, this one included
  , eWrite   :: Bool             -- ^ the current byte op is a write
  , eShift   :: BitVector 9      -- ^ bits to send (MSB first) / bits received
  , eHeld    :: Bool             -- ^ the bus is ours (after START, before STOP)
  , eSclLow  :: Bool
  , eSdaLow  :: Bool
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFDataX

-- | Bus released and free.
engineInit :: Engine
engineInit = Engine
  { eSlot = SIdle, ePhase = 0, eCount = 0, eStretch = 0, eBits = 0
  , eWrite = False, eShift = 0, eHeld = False, eSclLow = False, eSdaLow = False }

-- | The line levels of each phase, as (SCL, SDA): 'Nothing' keeps the
-- level. SDA keeps its level in phase 0, while SCL falls.
phaseLevels :: Slot -> Index 6 -> (Maybe Bit, Maybe Bit)
phaseLevels slot p = case slot of
  SIdle    -> (Just 1, Just 1)
  SStart   -> (scl, Just (if p < 4 then 1 else 0))
  SStop    -> (scl, Just (if p < 4 then 0 else 1))
  SBit d _ -> (scl, Just d)
 where
  scl | p == 0    = Just 0
      | p == 1    = Just 0
      | otherwise = Just 1

phaseLast :: Slot -> Index 6
phaseLast slot = case slot of
  SBit _ _ -> 3
  _        -> 5

-- | Set the pins for phase @p@ of @slot@ and start counting it.
enterPhase :: I2cTiming -> Slot -> Index 6 -> Engine -> Engine
enterPhase t slot p e = e
  { eSlot = slot, ePhase = p, eCount = itQuarter t, eStretch = 0
  , eSclLow = maybe (eSclLow e) (== 0) scl
  , eSdaLow = if p == 0 then eSdaLow e else maybe (eSdaLow e) (== 0) sda }
 where
  (scl, sda) = phaseLevels slot p

-- | The next bit slot of the current byte op, from the top of 'eShift'.
nextBit :: Engine -> Slot
nextBit e
  | eWrite e  = SBit (msb (eShift e)) (eBits e == 1)
  | otherwise = if eBits e == 1 then SBit (msb (eShift e)) False
                                else SBit 1 True

-- | Start an operation (the engine is idle between operations).
startOp :: I2cTiming -> I2cOp -> Engine -> Engine
startOp t op e = case op of
  -- From a free bus both lines are already high: skip to the high phase.
  OpStart  -> enterPhase t SStart (if eHeld e then 0 else 2) e
  OpStop   -> enterPhase t SStop 0 e
  OpWrite b ->
    let e' = e { eBits = 9, eWrite = True, eShift = b ++# (1 :: BitVector 1) }
    in enterPhase t (nextBit e') 0 e'
  OpRead ack ->
    -- eShift starts with the ACK level in bit 0; after the 8 data bits are
    -- shifted in below it, it is the MSB, driven in the ninth slot
    let e' = e { eBits = 9, eWrite = False, eShift = (0 :: BitVector 8) ++# boolToBV (not ack) }
    in enterPhase t (nextBit e') 0 e'

-- | One clock. Takes the line levels and an operation to start (only
-- looked at while idle); returns the response of an operation that
-- finished in this clock.
engineStep :: I2cTiming -> Engine -> (I2cLines, Maybe I2cOp) -> (Engine, Maybe I2cResp)
engineStep t e (lines', op) = case eSlot e of
  SIdle -> (maybe e (\o -> startOp t o e) op, Nothing)
  slot
    -- SCL released: wait for it to read high (clock stretching)
    | ePhase e == 2 && ilScl lines' == 0 ->
        if eStretch e == itStretchMax t
          then (engineInit, Just RespTimeout)
          else (e { eStretch = eStretch e + 1 }, Nothing)
    | eCount e > 1 -> (e { eCount = eCount e - 1 }, Nothing)
    | ePhase e /= lastPhase -> (enterPhase t slot (ePhase e + 1) e, Nothing)
    | otherwise -> endSlot slot
 where
  lastPhase = phaseLast (eSlot e)
  idle e' = e' { eSlot = SIdle }

  endSlot slot = case slot of
    SStart -> (idle e { eHeld = True }, Just RespOk)
    SStop  -> (idle e { eHeld = False }, Just RespOk)
    SBit _ sampleIt ->
      let sh  | eWrite e  = eShift e `shiftL` 1
              | sampleIt  = (eShift e `shiftL` 1) .|. resize (pack (ilSda lines'))
              | otherwise = eShift e
          e'  = e { eBits = eBits e - 1, eShift = sh }
          rsp | eWrite e  = RespAck (ilSda lines' == 0)
              | otherwise = RespByte (slice d7 d0 sh)
      in if eBits e == 1
           then (idle e', Just rsp)
           else (enterPhase t (nextBit e') 0 e', Nothing)
    SIdle -> (e, Nothing)

------------------------------------------------------------------------
-- Transactions

-- | Where a transaction is. A failed one goes to 'TStop' with its error.
data Step
  = TIdle
  | TStart
  | TAddrW
  | TReg
  | TData
  | TRestart
  | TAddrR
  | TRead
  | TStop
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFDataX

data I2cState = I2cState
  { isEngine :: Engine
  , isTxn    :: I2cTxn
  , isStep   :: Step
  , isIssue  :: Bool             -- ^ the engine still has to be given 'isStep''s op
  , isResult :: I2cResult        -- ^ reported when the STOP is done
  , isDone   :: Maybe I2cResult  -- ^ registered 'ioDone'
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFDataX

i2cInit :: I2cState
i2cInit = I2cState
  { isEngine = engineInit, isTxn = TxnRead 0 0, isStep = TIdle
  , isIssue = False, isResult = Right 0, isDone = Nothing }

-- | The engine operation of a step.
stepOp :: I2cTxn -> Step -> I2cOp
stepOp txn step = case step of
  TStart   -> OpStart
  TAddrW   -> OpWrite (dev ++# (0 :: BitVector 1))
  TReg     -> OpWrite reg
  TData    -> OpWrite val
  TRestart -> OpStart
  TAddrR   -> OpWrite (dev ++# (1 :: BitVector 1))
  TRead    -> OpRead False
  _        -> OpStop
 where
  (dev, reg, val) = case txn of
    TxnWrite d r v -> (d, r, v)
    TxnRead d r    -> (d, r, 0)

-- | The step after @step@ succeeded.
stepNext :: I2cTxn -> Step -> Step
stepNext txn step = case (step, txn) of
  (TStart, _)            -> TAddrW
  (TAddrW, _)            -> TReg
  (TReg, TxnWrite{})     -> TData
  (TReg, TxnRead{})      -> TRestart
  (TData, _)             -> TStop
  (TRestart, _)          -> TAddrR
  (TAddrR, _)            -> TRead
  (TRead, _)             -> TStop
  _                      -> TIdle

-- | The error for a NACK in @step@.
stepNack :: Step -> I2cError
stepNack step = case step of
  TReg  -> NackReg
  TData -> NackData
  _     -> NackAddr

-- | One clock: the line levels and a transaction to start (taken only when
-- 'ioReady' of the current output is high).
i2cStep :: I2cTiming -> I2cState -> (I2cLines, Maybe I2cTxn) -> I2cState
i2cStep t s (lines', txnIn) = case isStep s of
  TIdle -> case txnIn of
    Just txn -> s' { isTxn = txn, isStep = TStart, isIssue = True, isResult = Right 0 }
    Nothing  -> s'
  step -> case rsp of
    Nothing -> s'
    Just RespTimeout -> s' { isStep = TIdle, isDone = Just (Left I2cTimeout) }
    Just r
      | step == TStop -> s' { isStep = TIdle, isDone = Just (isResult s) }
      | r == RespAck False -> s' { isStep = TStop, isIssue = True, isResult = Left (stepNack step) }
      | otherwise ->
          let res = case r of
                RespByte b -> Right b
                _          -> isResult s
          in s' { isStep = stepNext (isTxn s) step, isIssue = True, isResult = res }
 where
  op | isIssue s && isStep s /= TIdle = Just (stepOp (isTxn s) (isStep s))
     | otherwise                      = Nothing
  (eng, rsp) = engineStep t (isEngine s) (lines', op)
  -- The engine takes the op in this clock (it is idle whenever isIssue is set).
  s' = s { isEngine = eng, isIssue = False, isDone = Nothing }

i2cOut :: I2cState -> I2cOut
i2cOut s = I2cOut
  { ioSclLow = eSclLow (isEngine s)
  , ioSdaLow = eSdaLow (isEngine s)
  , ioReady  = isStep s == TIdle
  , ioDone   = isDone s
  }

-- | The master as a circuit. The line levels are synchronised here (they
-- come from the pins, asynchronous to @dom@; idle level high).
i2cMaster
  :: HiddenClockResetEnable dom
  => I2cTiming
  -> Signal dom I2cLines        -- ^ SCL and SDA as read from the pins
  -> Signal dom (Maybe I2cTxn)
  -> Signal dom I2cOut
i2cMaster t pins txn = i2cOut <$> st
 where
  st = register i2cInit (i2cStep t <$> st <*> bundle (synchronise (I2cLines 1 1) pins, txn))
