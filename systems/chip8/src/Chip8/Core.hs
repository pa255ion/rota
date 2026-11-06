{-|
The CHIP-8 hardware core (PLAN.md step 1.3): multi-cycle, one block RAM
for the 4 KiB main memory and the framebuffer as block RAM too, with all
state in one record ('CoreState') and a pure transition function
('coreStep').

__Timing.__ Instructions start on 'ciInstrTick' and the timers count down
on 'ciTimerTick'. Both are single-cycle pulses that are latched, and both
are only acted on between instructions, in 'PIdle': a pending timer tick
first (one cycle), then a pending instruction. So no tick is ever lost or
applied in the middle of an instruction, and the order of the two is
reported on 'coEvent' for the lockstep tests. Every instruction takes a
handful of cycles (at most about 40, for a 15-row sprite), far less than
the instruction period on the board.

__Memories.__ RAM reads take one cycle, so the fetch is two cycles and
every RAM read is issued one state before its data is used. RAM writes
(Fx33, Fx55, and the loader) go through the same block RAM's write port.
The framebuffer is 32 rows of 64 bits (bit 63 = column 0), kept three
times with identical writes: one copy for the core's read-modify-write
when drawing, and one for each video read port (VGA: 'ciFbAddr' /
'coFbRow', OLED: 'ciFbAddr2' / 'coFbRow2'), so video never stalls the
core. Each copy is 2 Kbit.

__Semantics__ are those of "Chip8.Spec", including its documented choices
(LFSR random numbers, display wait for the /next/ tick, Fx0A
press-and-release, halting on illegal opcodes and stack errors). Layer 2
compares the two after every instruction.
-}
module Chip8.Core
  ( -- * Interface
    Chip8In (..)
  , chip8InIdle
  , Chip8Out (..)
  , Event (..)
    -- * Architectural state
  , Arch (..)
  , VWait (..)
  , archInit
    -- * Implementation
  , CoreState (..)
  , Phase (..)
  , MemReq (..)
  , coreInit
  , coreStep
  , rngNext
  , fontRam
  , Ram
  , chip8Core
    -- * With save states (side track N)
  , StPort (..)
  , chip8CoreWith
  ) where

import Clash.Prelude

import Chip8.Decode (decode)
import Chip8.Types

data Chip8In = Chip8In
  { ciKeys      :: BitVector 16   -- ^ bit k = key k held
  , ciConfig    :: BitVector 8    -- ^ quirk bits 0 to 4 (0 = VIP), docs/systems/chip8.md
  , ciInstrTick :: Bool           -- ^ start the next instruction
  , ciTimerTick :: Bool           -- ^ 60 Hz
  , ciRun       :: Bool           -- ^ low: start no instructions (loading)
  , ciReset     :: Bool           -- ^ soft reset: back to the power-on state (PC 0x200,
                                  --   registers, timers, RNG), framebuffer cleared; RAM kept
  , ciRomWrite  :: Maybe (Unsigned 12, Byte)  -- ^ loader port into RAM
  , ciFbAddr    :: Index 32       -- ^ video read port A (VGA): framebuffer row
  , ciFbAddr2   :: Index 32       -- ^ video read port B (OLED): framebuffer row
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFDataX

chip8InIdle :: Chip8In
chip8InIdle = Chip8In 0 0 False False False False Nothing 0 0

data Chip8Out = Chip8Out
  { coFbRow  :: BitVector 64      -- ^ row 'ciFbAddr' of the previous cycle; bit 63 = x 0
  , coFbRow2 :: BitVector 64      -- ^ row 'ciFbAddr2' of the previous cycle
  , coSound :: Bool               -- ^ sound timer non-zero
  , coDebug :: BitVector 32       -- ^ PC (11..0), halted (12), key wait (13), vblank wait (14), I (31..16)
  , coEvent :: Maybe Event        -- ^ trace for the lockstep tests
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFDataX

-- | Display-wait state (as 'Chip8.Spec.VblankWait').
data VWait = VIdle | VWaiting | VReleased
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFDataX

-- | The architectural state, everything the reference model has.
data Arch = Arch
  { aPc      :: Unsigned 12
  , aI       :: Unsigned 16
  , aV       :: Vec 16 Byte
  , aStack   :: Vec 16 (Unsigned 12)  -- ^ entry 0 is the oldest
  , aSp      :: Index 17
  , aDelay   :: Byte
  , aSound   :: Byte
  , aRng     :: BitVector 16
  , aVblank  :: VWait
  , aKeyWait :: Maybe (Index 16)      -- ^ Fx0A: this key is held
  , aHalted  :: Bool
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFDataX

data Event
  = Retired (BitVector 16) Arch  -- ^ an instruction finished: the keys it saw, the state after
  | Ticked                       -- ^ the timers were decremented
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFDataX

archInit :: Arch
archInit = Arch
  { aPc = 0x200, aI = 0, aV = repeat 0, aStack = repeat 0, aSp = 0
  , aDelay = 0, aSound = 0, aRng = 0xACE1, aVblank = VIdle
  , aKeyWait = Nothing, aHalted = False }

data Phase
  = PInit (Index 32)  -- ^ after (soft) reset: clear framebuffer row r, no event
  | PIdle
  | PFetch            -- ^ RAM data = high byte; read low byte
  | PDecode           -- ^ RAM data = low byte; execute
  | PClear (Index 32)
  | PBcd (Index 3)
  | PStore Reg
  | PLoadA Reg        -- ^ read I + r
  | PLoadB Reg        -- ^ RAM data = byte for V[r]
  | PDrawA (Index 16) -- ^ read sprite row r and its framebuffer row
  | PDrawB (Index 16) -- ^ both in: XOR, collision, write back
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFDataX

data CoreState = CoreState
  { csArch      :: Arch
  , csPhase     :: Phase
  , csHi        :: Byte
  , csOp        :: Instr
  , csKeys      :: BitVector 16   -- ^ keys latched at the start of the instruction
  , csCfg       :: BitVector 8    -- ^ quirks latched at the start of the instruction
  , csX0        :: Unsigned 6     -- ^ sprite column (Vx mod 64)
  , csY0        :: Unsigned 5     -- ^ sprite row (Vy mod 32)
  , csHit       :: Bool           -- ^ collision so far
  , csTickPend  :: Bool
  , csInstrPend :: Bool
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFDataX

coreInit :: CoreState
coreInit = CoreState archInit (PInit 0) 0 Cls 0 0 0 0 False False False

-- | Memory requests for one cycle.
data MemReq = MemReq
  { mrRamAddr  :: Unsigned 12
  , mrRamWrite :: Maybe (Unsigned 12, Byte)
  , mrFbAddr   :: Index 32
  , mrFbWrite  :: Maybe (Index 32, BitVector 64)
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFDataX

-- | The random number generator of "Chip8.Spec".
rngNext :: BitVector 16 -> BitVector 16
rngNext r = (r `shiftR` 1) `xor` (if testBit r 0 then 0xB400 else 0)

-- | Power-on RAM: the font of "Chip8.Spec" at 0x050, zeros elsewhere.
fontRam :: Vec 4096 Byte
fontRam = replicate d80 0 ++ fontData ++ replicate (SNat @3936) 0
 where
  fontData :: Vec 80 Byte
  fontData =
       0xF0 :> 0x90 :> 0x90 :> 0x90 :> 0xF0 :> 0x20 :> 0x60 :> 0x20 :> 0x20 :> 0x70
    :> 0xF0 :> 0x10 :> 0xF0 :> 0x80 :> 0xF0 :> 0xF0 :> 0x10 :> 0xF0 :> 0x10 :> 0xF0
    :> 0x90 :> 0x90 :> 0xF0 :> 0x10 :> 0x10 :> 0xF0 :> 0x80 :> 0xF0 :> 0x10 :> 0xF0
    :> 0xF0 :> 0x80 :> 0xF0 :> 0x90 :> 0xF0 :> 0xF0 :> 0x10 :> 0x20 :> 0x40 :> 0x40
    :> 0xF0 :> 0x90 :> 0xF0 :> 0x90 :> 0xF0 :> 0xF0 :> 0x90 :> 0xF0 :> 0x10 :> 0xF0
    :> 0xF0 :> 0x90 :> 0xF0 :> 0x90 :> 0x90 :> 0xE0 :> 0x90 :> 0xE0 :> 0x90 :> 0xE0
    :> 0xF0 :> 0x80 :> 0x80 :> 0x80 :> 0xF0 :> 0xE0 :> 0x90 :> 0x90 :> 0x90 :> 0xE0
    :> 0xF0 :> 0x80 :> 0xF0 :> 0x80 :> 0xF0 :> 0xF0 :> 0x80 :> 0xF0 :> 0x80 :> 0x80
    :> Nil

-- | One clock. Inputs: the core inputs, the RAM byte and the framebuffer
-- row read in the previous cycle.
coreStep :: CoreState -> (Chip8In, Byte, BitVector 64) -> (CoreState, (MemReq, Chip8Out))
coreStep s0 (inp, ramData, fbData) = (s', (req', out))
 where
  -- tick latches, updated every cycle
  s = s0 { csTickPend  = csTickPend s0 || ciTimerTick inp
         , csInstrPend = csInstrPend s0 || ciInstrTick inp }
  a = csArch s
  cfg = csCfg s

  -- quirks: bit = 0 selects the VIP behaviour
  vfReset    = not (testBit cfg 0)
  memIncI    = not (testBit cfg 1)
  shiftVy    = not (testBit cfg 2)
  jumpV0     = not (testBit cfg 3)
  dispWait   = not (testBit cfg 4)
  clipping   = True

  idleReq = MemReq (aPc a) Nothing 0 Nothing
  -- the loader may write whenever the core doesn't
  withLoader r = case mrRamWrite r of
    Nothing -> r { mrRamWrite = ciRomWrite inp }
    Just _  -> r

  (s', req, ev)
    | ciReset inp = (coreInit, idleReq, Nothing)
    | otherwise = case csPhase s of
    PInit r
      | r == maxBound -> (s { csPhase = PIdle }, fbW r 0, Nothing)
      | otherwise     -> (s { csPhase = PInit (r + 1) }, fbW r 0, Nothing)
    PIdle
      | csTickPend s ->
          (s { csArch = tick a, csTickPend = False }, idleReq, Just Ticked)
      | csInstrPend s && ciRun inp && not (aHalted a) ->
          ( s { csPhase = PFetch, csInstrPend = False, csKeys = ciKeys inp, csCfg = ciConfig inp }
          , idleReq { mrRamAddr = aPc a }, Nothing )
      | otherwise -> (s, idleReq, Nothing)

    PFetch -> (s { csPhase = PDecode, csHi = ramData }, idleReq { mrRamAddr = aPc a + 1 }, Nothing)

    PDecode -> execute (decode (csHi s ++# ramData))

    PClear r
      | r == maxBound -> retire a { aPc = pc2 } (fbW r 0)
      | otherwise     -> (s { csPhase = PClear (r + 1) }, fbW r 0, Nothing)

    PBcd k ->
      let vx = aV a !! opX
          d = case k of
                0 -> vx `div` 100
                1 -> (vx `div` 10) `mod` 10
                _ -> vx `mod` 10
          w = ramW (resize (aI a) + fromIntegral k) d
      in if k == 2 then retire a w else (s { csPhase = PBcd (k + 1) }, w, Nothing)

    PStore r ->
      let w = ramW (resize (aI a) + fromIntegral r) (aV a !! r)
      in if r == opX then retire (incI a) w else (s { csPhase = PStore (r + 1) }, w, Nothing)

    PLoadA r -> (s { csPhase = PLoadB r }, idleReq { mrRamAddr = resize (aI a) + fromIntegral r }, Nothing)

    PLoadB r ->
      let a' = a { aV = replace r ramData (aV a) }
      in if r == opX then retire (incI a') idleReq
                     else (s { csArch = a', csPhase = PLoadA (r + 1) }, idleReq, Nothing)

    PDrawA r ->
      ( s { csPhase = PDrawB r }
      , idleReq { mrRamAddr = resize (aI a) + fromIntegral r, mrFbAddr = drawRow r }
      , Nothing )

    PDrawB r ->
      let sprite  = (resize ramData :: BitVector 64) `shiftL` 56
          placed  = if clipping then sprite `shiftR` fromIntegral (csX0 s)
                                else sprite `rotateR` fromIntegral (csX0 s)
          visible = not clipping || resize (csY0 s) + fromIntegral r < (32 :: Unsigned 6)
          hit     = csHit s || (visible && (fbData .&. placed) /= 0)
          w       = if visible then fbW (drawRow r) (fbData `xor` placed) else idleReq
          lastRow = fromIntegral r + 1 == (fromIntegral opN :: Unsigned 5)
      in if lastRow
           then retire a { aV = replace (15 :: Index 16) (flagByte hit) (aV a) } w
           else (s { csPhase = PDrawA (r + 1), csHit = hit }, w, Nothing)

  -- the operand fields of the latched instruction
  opX :: Reg
  opX = case csOp s of
    Bcd x   -> x
    Store x -> x
    Load x  -> x
    _       -> 0
  opN :: Index 16
  opN = case csOp s of
    Drw _ _ n -> n
    _         -> 0

  drawRow :: Index 16 -> Index 32
  drawRow r = unpack (resize (pack (csY0 s)) + resize (pack r))  -- mod 32

  pc2 = aPc a + 2

  ramW addr d = idleReq { mrRamWrite = Just (addr, d) }
  fbW row w = idleReq { mrFbWrite = Just (row, w) }

  retire a' r = (s { csArch = a', csPhase = PIdle, csHit = False }, r, Just (Retired (csKeys s) a'))

  incI x = if memIncI then x { aI = aI x + fromIntegral opX + 1 } else x

  tick x = x
    { aDelay  = dec (aDelay x)
    , aSound  = dec (aSound x)
    , aVblank = if aVblank x == VWaiting then VReleased else aVblank x }
  dec t = if t == 0 then 0 else t - 1

  flagByte :: Bool -> Byte
  flagByte b = if b then 1 else 0

  keyDown :: Byte -> Bool
  keyDown k = testBit (csKeys s) (fromIntegral (k .&. 0xF))

  setV :: Reg -> Byte -> Arch -> Arch
  setV r v x = x { aV = replace r v (aV x) }

  -- Execute a decoded instruction (state PDecode).
  execute i =
    let v = aV a
        vr r = v !! r
        next = a { aPc = pc2 }
        skip c = retire a { aPc = if c then aPc a + 4 else pc2 } idleReq
        flag f r x vxNew = retire (setV 15 (flagByte f) (setV x vxNew next)) r
        multi ph a' = (s { csOp = i, csPhase = ph, csArch = a' }, idleReq, Nothing)
        halt = retire a { aHalted = True } idleReq
    in case i of
      Cls -> multi (PClear 0) a
      Ret
        | aSp a == 0 -> halt
        | otherwise  -> retire a { aPc = aStack a !! (aSp a - 1), aSp = aSp a - 1 } idleReq
      Jp n -> retire a { aPc = n } idleReq
      Call n
        | aSp a == 16 -> halt
        | otherwise   -> retire a { aPc = n, aStack = replace (aSp a) pc2 (aStack a), aSp = aSp a + 1 } idleReq
      SeImm x k  -> skip (vr x == k)
      SneImm x k -> skip (vr x /= k)
      SeReg x y  -> skip (vr x == vr y)
      SneReg x y -> skip (vr x /= vr y)
      LdImm x k  -> retire (setV x k next) idleReq
      AddImm x k -> retire (setV x (vr x + k) next) idleReq
      Alu f x y ->
        let vx = vr x
            vy = vr y
            src = if shiftVy then vy else vx
            logic r = retire ((if vfReset then setV 15 0 else id) (setV x r next)) idleReq
        in case f of
          AluMov  -> retire (setV x vy next) idleReq
          AluOr   -> logic (vx .|. vy)
          AluAnd  -> logic (vx .&. vy)
          AluXor  -> logic (vx `xor` vy)
          AluAdd  -> let sm = (zeroExtend vx + zeroExtend vy :: BitVector 9)
                     in flag (testBit sm 8) idleReq x (truncateB sm)
          AluSub  -> flag (vx >= vy) idleReq x (vx - vy)
          AluShr  -> flag (testBit src 0) idleReq x (src `shiftR` 1)
          AluSubn -> flag (vy >= vx) idleReq x (vy - vx)
          AluShl  -> flag (testBit src 7) idleReq x (src `shiftL` 1)
      LdI n -> retire next { aI = resize n } idleReq
      JpOff n ->
        let r = if jumpV0 then 0 else unpack (slice d11 d8 (pack n)) :: Reg
        in retire a { aPc = n + resize (unpack (vr r) :: Unsigned 8) } idleReq
      Rnd x k ->
        let r = rngNext (aRng a)
        in retire (setV x (truncateB r .&. k) next { aRng = r }) idleReq
      Drw x y _
        | dispWait && aVblank a /= VReleased -> retire a { aVblank = VWaiting } idleReq
        | otherwise ->
            let a' = next { aVblank = VIdle }
                s1 = s { csOp = i, csArch = a'
                       , csX0 = truncateB (unpack (vr x) :: Unsigned 8)
                       , csY0 = truncateB (unpack (vr y) :: Unsigned 8) }
            in case i of
                 Drw _ _ 0 -> (s1 { csArch = setV 15 0 a', csPhase = PIdle }, idleReq
                              , Just (Retired (csKeys s) (setV 15 0 a')))
                 _         -> (s1 { csPhase = PDrawA 0, csHit = False }, idleReq, Nothing)
      Skp x  -> skip (keyDown (vr x))
      Sknp x -> skip (not (keyDown (vr x)))
      LdVxDt x -> retire (setV x (aDelay a) next) idleReq
      LdKey x -> case aKeyWait a of
        Nothing
          | csKeys s /= 0 -> retire a { aKeyWait = Just (fromIntegral (countTrailingZeros (csKeys s))) } idleReq
          | otherwise     -> retire a idleReq
        Just k
          | testBit (csKeys s) (fromEnum k) -> retire a idleReq
          | otherwise -> retire (setV x (fromIntegral k) next { aKeyWait = Nothing }) idleReq
      LdDtVx x -> retire next { aDelay = vr x } idleReq
      LdStVx x -> retire next { aSound = vr x } idleReq
      AddI x   -> retire next { aI = aI a + resize (unpack (vr x) :: Unsigned 8) } idleReq
      LdFont x -> retire next { aI = 0x50 + 5 * resize (unpack (vr x .&. 0xF) :: Unsigned 8) } idleReq
      Bcd _    -> multi (PBcd 0) next
      Store _  -> multi (PStore 0) next
      Load _   -> multi (PLoadA 0) next
      Illegal _ -> halt

  req' = withLoader req

  aOut = csArch s'
  out = Chip8Out
    { coFbRow = 0, coFbRow2 = 0  -- filled in by chip8Core from the video copies
    , coSound = aSound aOut /= 0
    , coDebug = pack (aI aOut) ++# (0 :: BitVector 1) ++# boolBit (aVblank aOut == VWaiting)
                ++# boolBit (aKeyWait aOut /= Nothing) ++# boolBit (aHalted aOut) ++# pack (aPc aOut)
    , coEvent = ev }
  boolBit :: Bool -> BitVector 1
  boolBit b = if b then 1 else 0

-- | The main RAM: read address, optional write, data one cycle later. For
-- example @blockRam fontRam@ (programs then come through 'ciRomWrite'), or
-- @blockRamFile d4096 file@ with a RAM image that already holds a program.
type Ram dom = Signal dom (Unsigned 12) -> Signal dom (Maybe (Unsigned 12, Byte)) -> Signal dom Byte

-- | The core with its memories.
chip8Core
  :: HiddenClockResetEnable dom
  => Ram dom -> Signal dom Chip8In -> Signal dom Chip8Out
chip8Core mainRam inp = (\o r1 r2 -> o { coFbRow = r1, coFbRow2 = r2 }) <$> out <*> fbVideo <*> fbVideo2
 where
  (req, out) = unbundle (mealy coreStep coreInit (bundle (inp, ramData, fbCpu)))
  ramData = mainRam (mrRamAddr <$> req) (mrRamWrite <$> req)
  fbCpu   = blockRam (repeat 0 :: Vec 32 (BitVector 64)) (mrFbAddr <$> req) (mrFbWrite <$> req)
  fbVideo = blockRam (repeat 0 :: Vec 32 (BitVector 64)) (ciFbAddr <$> inp) (mrFbWrite <$> req)
  fbVideo2 = blockRam (repeat 0 :: Vec 32 (BitVector 64)) (ciFbAddr2 <$> inp) (mrFbWrite <$> req)

-- * With save states (side track N, N.10; "Chip8.StateHooks")

-- | A state port access while frozen: a RAM byte (read, or written), or a
-- framebuffer row (read, or written whole).
data StPort
  = StRam (Unsigned 12) (Maybe Byte)
  | StFb (Index 32) (Maybe (BitVector 64))
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFDataX

-- | 'chip8Core' with save states: while @frozen@ the core takes no step
-- (no tick is latched); a table write (byte, value) goes into its record
-- through @write@ ("Chip8.StateTap".tableWrite); a state port access takes RAM's or the framebuffer's
-- port (the framebuffer's writes all its copies). Also the core's state
-- before each clock's step, RAM's byte and the port's framebuffer row read
-- in the previous clock: from a fourth copy of the framebuffer, the
-- port's own (the core's copy keeps its read address to itself, as in
-- 'chip8Core': read through it, the port got zeros on the board, though
-- not in simulation).
chip8CoreWith
  :: HiddenClockResetEnable dom
  => (CoreState -> Unsigned 6 -> BitVector 8 -> CoreState)
  -> Ram dom -> Signal dom Chip8In
  -> Signal dom Bool                                   -- ^ frozen
  -> Signal dom (Maybe (Unsigned 6, BitVector 8))      -- ^ a write into the record
  -> Signal dom (Maybe StPort)                         -- ^ the state port
  -> (Signal dom Chip8Out, Signal dom CoreState, Signal dom Byte, Signal dom (BitVector 64))
chip8CoreWith write mainRam inp frozen recWr port = (out', coreSt, ramData, fbState)
 where
  out' = (\o r1 r2 -> o { coFbRow = r1, coFbRow2 = r2 }) <$> out <*> fbVideo <*> fbVideo2
  (req, out, coreSt) = unbundle (mealy step coreInit (bundle (inp, ramData, fbCpu, frozen, recWr)))
  -- (the state out before the guards: the hooks' freeze reads it)
  step s (i, rd, fb, f, w) = (next, (rq, o, s))
   where
    (s', (rq0, o0)) = coreStep s (i, rd, fb)
    idle = MemReq (aPc (csArch s)) Nothing 0 Nothing
    (next, rq, o)
      | Just (n, v) <- w = (write s n v, idle, o0 { coEvent = Nothing })
      | f = (s, idle, o0 { coEvent = Nothing })
      | otherwise = (s', rq0, o0)
  portRam = (\p -> case p of Just (StRam a d) -> Just (a, d); _ -> Nothing) <$> port
  portFb = (\p -> case p of Just (StFb r d) -> Just (r, d); _ -> Nothing) <$> port
  ramAddr = (\p r -> maybe (mrRamAddr r) fst p) <$> portRam <*> req
  ramWr = (\p r -> case p of
              Just (a, Just d) -> Just (a, d)
              Just (_, Nothing) -> Nothing
              Nothing -> mrRamWrite r) <$> portRam <*> req
  ramData = mainRam ramAddr ramWr
  fbWr = (\p r -> case p of
             Just (row, Just d) -> Just (row, d)
             Just (_, Nothing) -> Nothing
             Nothing -> mrFbWrite r) <$> portFb <*> req
  fbCpu   = blockRam (repeat 0 :: Vec 32 (BitVector 64)) (mrFbAddr <$> req) fbWr
  fbState = blockRam (repeat 0 :: Vec 32 (BitVector 64)) (maybe 0 fst <$> portFb) fbWr
  fbVideo = blockRam (repeat 0 :: Vec 32 (BitVector 64)) (ciFbAddr <$> inp) fbWr
  fbVideo2 = blockRam (repeat 0 :: Vec 32 (BitVector 64)) (ciFbAddr2 <$> inp) fbWr
