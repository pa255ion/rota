{-# LANGUAGE MultiWayIf #-}
{-|
POKEY in the reference model (M4 step 4.1: what the OS needs to boot;
the keyboard scan, pots, serial input and audio output follow in step
4.4). Sources: Altirra Hardware Reference Manual (HRM) chapter 5 and the
register reference (14.4); Altirra's POKEY (values and rules) as the
cross-check.

One 'pokeyTick' per machine cycle, at its start, before the CPU's access
in that cycle; register writes land after the tick of their cycle.

__Clocks__ (HRM 5.4). The 64 kHz and 15 kHz clocks are pulses every 28
and 114 cycles, their phase set when initialization mode (SKCTL bits 0-1
both 0) ends: the first pulses come 20 and 79 cycles after that write,
so that a period-0 timer on them shows its IRQ 25 and 84 cycles after it
(Altirra's phases, which pass Acid800's pokey_inittiming; HRM 5.2,
"Clock reset timing", gives 24 and 83). They don't run in initialization
mode.
Channels 1 and 3 run on the machine clock with AUDCTL bits 6 and 5.

__Timers__ (HRM 5.3, figure 8). A counter counts its clock down; on the
clock after it reaches 0 it underflows, and three cycles later it is
reloaded from AUDF and the timer /fires/: its IRQST bit and the serial
clock see it the cycle after. So an unlinked machine-clock timer has the
period AUDF + 4 and fires 8 cycles after STIMER for AUDF = 0. STIMER
reloads every counter three cycles after the write without firing, and
cancels an underflow of that same cycle (the last preempting write is 4
cycles before the IRQ shows). Linked pairs (AUDCTL bits 4 and 3): the
low counter isn't reloaded on underflow but wraps to $FF and goes on; it
still fires, three cycles later, and that clocks the high counter, whose
underflow reloads both three cycles later: period AUDF + 256 × AUDF' + 7
on the machine clock. A counter reloaded in a cycle (STIMER, its own
reload) only counts from the next. Two-tone mode (SKCTL bit 3): a timer 1
fire on a mark bit (unless force break) or any timer 2 fire reloads both
timers two cycles later, without firing; a fire due then is lost.

__Serial output__ (HRM 5.6). The output clock toggles each time timer 4
(SKCTL bits 6-5 = 01 or 10) or timer 2 (11) fires; two cycles after each
rising edge, while a byte is being sent or one waits in SEROUT, the
output shifter moves one bit cell on. When it has sent its ten bits (or
was idle) it loads a waiting SEROUT byte: IRQST bit 3 (sending) goes to
1 and the "output ready" IRQ (bit 4) is raised; with nothing waiting, bit
3 goes to 0 (complete). Bit 3 isn't latched: it is the shifter's state,
whatever IRQEN says.

__Serial input__ (HRM 5.6, "Input shift register", "Asynchronous
receive mode", figure 10; side track Q, cassettes depth 2). The input
line comes from outside ('pokeySetSerialIn': the cassette deck; 1 when
idle); SKSTAT bit 4 reads it directly. In asynchronous receive mode
(SKCTL bit 4) timers 3 and 4 are held reset (counters at AUDF, no
reload under way) while POKEY waits for a start bit; the line's falling
edge starts them, and every timer 4 fire after that is half a bit: the
first samples the middle of the start bit (SKSTAT bit 1 goes to 0, busy),
every second one after it a data bit (LSB first), the 19th the stop bit:
SERIN takes the byte, IRQST bit 5 (serial input ready) is raised if
enabled, or SKSTAT bit 5 (overrun, sticky until SKRES) if it already is;
SKSTAT bit 7 marks a stop bit of 0 (framing); bit 1 goes back to 1 and
the timers are held again. Initialization mode stops a byte being
received. Not modelled: input clocked by timer 4 or the external clock
without asynchronous mode (nothing on the 800XL's tape path uses it).

__IRQs__ (HRM 5.7). IRQST bits are active low; IRQEN clears (to 1) the
bits it disables, except bit 3. The IRQ line is asserted when an enabled
bit is 0, one cycle after IRQST shows it (the HRM's common 3-cycle delay
to the CPU's response).

__RANDOM__ (HRM 5.5): the 17-bit (x^17 + x^12 + 1) or 9-bit (x^9 + x^4
+ 1) XNOR generator, stepped every cycle from all zeros when
initialization mode ends; RANDOM shows eight of its bits inverted. In
initialization mode ones shift in from the left: $FF after ten cycles.
-}
module A800.Spec.Pokey
  ( Pokey
  , newPokey
  , pokeyTick
  , pokeyRead
  , pokeyWrite
  , pokeyIrq
  , pokeySerialOut
  , pokeyLevel
  , pokeySetKeys
  , pokeySetSerialIn
  , pokeySnapshot
  , pokeyQuiet
  ) where

import Prelude

import Control.Monad (forM, forM_, when)
import Control.Monad.ST (ST)
import Data.Bits
import Data.STRef
import qualified Data.Vector.Unboxed.Mutable as MV
import Data.Word

data Pokey s = Pokey
  { pkSt  :: !(MV.MVector s Int)
  , pkOut :: !(STRef s [Word8]) -- ^ bytes the serial port sent, newest first
  }

-- Slots. Per channel i (0-3): counter, reload countdown (underflow path),
-- whether that reload fires.
sAudf, sAudc, sCnt, sRel, sRelFire :: Int -> Int
sAudf i = i
sAudc i = 4 + i
sCnt i = 8 + i
sRel i = 12 + i
sRelFire i = 16 + i

sAudctl, sSkctl, sIrqen, sIrqst, sSkstat, sKbcode, sSerin, sSerout, sSerValid,
  sSerCount, sSerShift, sSerPhase, sSerTickIn, sStimerIn, sPhase64, sPhase15,
  sPoly9, sPoly17, sInitAt, sTime, sIrqLine, sFire, sAllpot :: Int
sAudctl = 20
sSkctl = 21
sIrqen = 22
sIrqst = 23
sSkstat = 24
sKbcode = 25
sSerin = 26
sSerout = 27
sSerValid = 28  -- a byte waits in SEROUT
sSerCount = 29  -- bit cells left in the output shifter (0: idle)
sSerShift = 30
sSerPhase = 31  -- output clock flip-flop
sSerTickIn = 32 -- cycles to the next shifter step (0: none)
sStimerIn = 33  -- cycles to STIMER's reload (0: none)
sPhase64 = 34   -- cycles to the next 64 kHz pulse
sPhase15 = 35   -- cycles to the next 15 kHz pulse
sPoly9 = 36
sPoly17 = 37
sInitAt = 38    -- cycle initialization mode began (for RANDOM)
sTime = 39      -- cycles since power-on
sIrqLine = 40
sFire = 41      -- channels that fire this cycle (bits 0-3), from last cycle's reloads
sAllpot = 42
sTwoTone, sSerBit :: Int
sTwoTone = 43   -- 1: timers 1 and 2 resync this cycle (two-tone mode)
sSerBit = 44    -- the bit on the serial output line

-- Audio: channel output flip-flops, high-pass flip-flops, the 4- and 5-bit
-- generators, the noise bits' last four values (per generator: bit 0 the
-- newest) for the channels' delays.
sOut :: Int -> Int
sOut i = 45 + i
sHp1, sHp2, sPoly4, sPoly5, sHist :: Int
sHp1 = 49
sHp2 = 50
sPoly4 = 51
sPoly5 = 52
sHist = 53      -- bits 0-3 poly4, 4-7 poly5, 8-11 poly9/17

-- Keyboard.
sKeyState, sKeyLatch, sKeyScan, sKeysLo, sKeysHi, sModKeys, sModLatched :: Int
sKeyState = 54   -- the scan's state machine (0-3)
sKeyLatch = 55   -- the key code it compares against
sKeyScan = 56    -- the scan counter
sKeysLo = 57     -- keys held, codes 0-31 (bit n = code n)
sKeysHi = 58     -- codes 32-63
sModKeys = 59    -- bit 0 SHIFT, 1 CONTROL, 2 BREAK held
sModLatched = 60 -- the same as the scan last latched them

-- Serial input.
sRxLine, sRxPrev, sRxOn, sRxCount, sRxShift :: Int
sRxLine = 61     -- the input line (1 idle)
sRxPrev = 62     -- the line at the last tick (for the start bit's edge)
sRxOn = 63       -- 1: a byte is coming in (timers 3 and 4 run)
sRxCount = 64    -- timer 4 fires since its start bit's edge
sRxShift = 65    -- the bits sampled so far (the newest at bit 7)

slots :: Int
slots = 66

get :: Pokey s -> Int -> ST s Int
get p = MV.unsafeRead (pkSt p)
{-# INLINE get #-}

set :: Pokey s -> Int -> Int -> ST s ()
set p = MV.unsafeWrite (pkSt p)
{-# INLINE set #-}

-- | Power on (POKEY has no reset line): as Altirra's cold start, IRQEN 0,
-- IRQST $F7 (the idle output shifter shows "complete"), SKSTAT $7F, SKCTL
-- 0 (initialization mode).
newPokey :: ST s (Pokey s)
newPokey = do
  p <- Pokey <$> MV.replicate slots 0 <*> newSTRef []
  set p sIrqst 0xF7
  set p sSkstat 0x7F
  set p sPhase64 28
  set p sPhase15 114
  set p sInitAt (-100)
  set p sRxLine 1
  set p sRxPrev 1
  forM_ [0 .. 3] $ \i -> set p (sCnt i) 1
  pure p

initMode :: Int -> Bool
initMode skctl = skctl .&. 3 == 0

-- | One machine cycle.
pokeyTick :: Pokey s -> ST s ()
pokeyTick p = do
  t <- (+ 1) <$> get p sTime
  set p sTime t
  -- The IRQ line, one cycle behind IRQST: from the state the last cycle
  -- left (including its IRQEN write).
  en0 <- get p sIrqen
  st0 <- get p sIrqst
  set p sIrqLine (if en0 .&. complement st0 .&. 0xFF /= 0 then 1 else 0)
  -- a resync asked for by the last cycle's fire (read before this cycle's)
  tt <- get p sTwoTone
  set p sTwoTone 0
  skctl <- get p sSkctl
  audctl <- get p sAudctl
  -- What fires this cycle (reloads of the last one).
  fire <- get p sFire
  set p sFire 0
  when (fire /= 0) (fired fire skctl)
  -- Asynchronous receive (SKCTL bit 4): timers 3 and 4 held reset while
  -- waiting for a start bit; its falling edge starts them (they count
  -- from the next cycle).
  line <- get p sRxLine
  prev <- get p sRxPrev
  set p sRxPrev line
  rxOn0 <- get p sRxOn
  let async = testBit skctl 4
  when (async && rxOn0 == 0 && prev == 1 && line == 0 && not (initMode skctl)) $ do
    set p sRxOn 1
    set p sRxCount 0
    set p sRxShift 0
  let hold34 = async && rxOn0 == 0
  when hold34 $ forM_ [2, 3] $ \i -> do
    get p (sAudf i) >>= set p (sCnt i)
    set p (sRel i) 0
  -- The serial output shifter.
  st <- get p sSerTickIn
  when (st > 0) $ do
    set p sSerTickIn (st - 1)
    when (st == 1) serialStep
  -- Clock pulses.
  let running = not (initMode skctl)
  p64 <- pulse sPhase64 28 running
  p15 <- pulse sPhase15 114 running
  let slow = if audctl .&. 1 /= 0 then p15 else p64
      clk i | i == 0 && audctl .&. 0x40 /= 0 = True
            | i == 2 && audctl .&. 0x20 /= 0 = True
            | otherwise = slow
      link12 = audctl .&. 0x10 /= 0
      link34 = audctl .&. 0x08 /= 0
  -- STIMER's reload.
  -- Two-tone resync: timers 1 and 2 reload (no fire; a fire due now is
  -- lost), two cycles after the fire that caused it (HRM 5.6, "Two-tone
  -- resync timing")
  when (tt /= 0) $ forM_ [0, 1] $ \i -> do
    get p (sAudf i) >>= set p (sCnt i)
    set p (sRel i) 0
  si <- get p sStimerIn
  stimerNow <- if si <= 0 then pure False else do
    set p sStimerIn (si - 1)
    when (si == 1) $ forM_ [0 .. 3] $ \i -> do
      get p (sAudf i) >>= set p (sCnt i)
      set p (sRel i) 0
    pure (si == 1)
  -- The reload pipelines (a linked low timer's pulse clocks its high
  -- timer after the loop, so the high pipeline starts on the next tick),
  -- then the counters.
  highs <- fmap concat $ forM [0 .. 3] $ \i -> do
    r <- get p (sRel i)
    if r <= 0 then pure [] else do
      set p (sRel i) (r - 1)
      if r /= 1 then pure [] else do
        let linkedLow = (i == 0 && link12) || (i == 2 && link34)
            linkedHigh = (i == 1 && link12) || (i == 3 && link34)
        f <- get p (sRelFire i)
        if linkedLow
          then if f == 0 then pure [] else do
            -- The low timer's pulse: it fires and clocks the high one.
            markFire i
            pure [i + 1]
          else do
            get p (sAudf i) >>= set p (sCnt i)
            when linkedHigh (get p (sAudf (i - 1)) >>= set p (sCnt (i - 1)))
            when (f /= 0) (markFire i)
            -- (as -1: reloaded this cycle, so it doesn't count in it)
            pure ([-1 - i] <> [ -1 - (i - 1) | linkedHigh ])
  let reloaded = [ -1 - j | j <- highs, j < 0 ] <> (if tt /= 0 then [0, 1] else []) <> (if hold34 then [2, 3] else [])
  mapM_ countHigh [ j | j <- highs, j >= 0 ]
  forM_ [0 .. 3] $ \i -> do
    let linkedHigh = (i == 1 && link12) || (i == 3 && link34)
        linkedLow = (i == 0 && link12) || (i == 2 && link34)
    r <- get p (sRel i)
    -- a counter loaded this cycle (STIMER, its own reload) counts from the next
    when (not linkedHigh && clk i && (r == 0 || linkedLow) && not stimerNow && i `notElem` reloaded) $ do
      c <- get p (sCnt i)
      if c == 0
        then do
          when linkedLow (set p (sCnt i) 0xFF)
          set p (sRel i) 3
          set p (sRelFire i) 1
        else set p (sCnt i) (c - 1)
  -- RANDOM's generators (they keep running in initialization mode, where
  -- only the read is forced, and restart from zero when it ends).
  get p sPoly9 >>= \q -> set p sPoly9 (step9 q)
  get p sPoly17 >>= \q -> set p sPoly17 (step17 q)
  -- the 4- and 5-bit generators (constant 0 and 1 in initialization mode)
  -- and the noise bits' history for the channels' delays (HRM 5.5)
  if running
    then do
      get p sPoly4 >>= \q -> set p sPoly4 (step4 q)
      get p sPoly5 >>= \q -> set p sPoly5 (step5 q)
    else set p sPoly4 0 >> set p sPoly5 0
  q4 <- get p sPoly4
  q5 <- get p sPoly5
  q9 <- get p sPoly9
  q17 <- get p sPoly17
  let n917 = if audctl .&. 0x80 /= 0 then q9 .&. 1 else (q17 `shiftR` 8) .&. 1
      b4 = if running then q4 .&. 1 else 0
      b5 = if running then q5 .&. 1 else 1
      b917 = if running then n917 else 0
  h <- get p sHist
  let push sh b = ((((h `shiftR` sh) .&. 15) `shiftL` 1) .|. b) .&. 15
  set p sHist (push 0 b4 .|. (push 4 b5 `shiftL` 4) .|. (push 8 b917 `shiftL` 8))
  -- the keyboard scan, one key per 15 kHz clock
  when (p15 && testBit skctl 1) (keyScan skctl)
 where
  pulse s period running = do
    n <- get p s
    if not running then pure False
      else if n <= 1 then set p s period >> pure True
      else set p s (n - 1) >> pure False
  markFire i = get p sFire >>= \f -> set p sFire (f .|. bit i)
  countHigh j = do
    r <- get p (sRel j)
    when (r == 0) $ do
      c <- get p (sCnt j)
      if c == 0
        then set p (sRel j) 3 >> set p (sRelFire j) 1
        else set p (sCnt j) (c - 1)
  fired fire skctl = do
    -- the channels' audio (HRM 5.3, "Distortion", "High-pass filter")
    h <- get p sHist
    audctl' <- get p sAudctl
    forM_ [0 .. 3] $ \i -> when (testBit fire i) $ do
      c <- get p (sAudc i)
      let poly5 = testBit h (4 + i)
          noise = if testBit c 6 then testBit h i else testBit h (8 + i)
      when (testBit c 7 || poly5) $
        if testBit c 5
          then get p (sOut i) >>= set p (sOut i) . xor 1
          else set p (sOut i) (if noise then 1 else 0)
      -- channels 3 and 4 clock the high-pass flip-flops of 1 and 2
      when (i == 2 && testBit audctl' 2) (get p (sOut 0) >>= set p sHp1)
      when (i == 3 && testBit audctl' 1) (get p (sOut 1) >>= set p sHp2)
    en <- get p sIrqen
    forM_ [(0, 0x01), (1, 0x02), (3, 0x04)] $ \(i, b) ->
      when (testBit fire i && en .&. b /= 0) $
        get p sIrqst >>= \s -> set p sIrqst (s .&. complement b)
    -- two-tone mode: timer 1 resyncs on a mark (unless force break),
    -- timer 2 always
    serBit <- get p sSerBit
    when (testBit skctl 3 && ((testBit fire 0 && serBit /= 0 && not (testBit skctl 7)) || testBit fire 1)) $
      set p sTwoTone 1
    -- the input shifter, on timer 4's fires while a byte comes in
    rxOn <- get p sRxOn
    when (testBit fire 3 && rxOn /= 0 && testBit skctl 4) $ do
      n <- (+ 1) <$> get p sRxCount
      set p sRxCount n
      when (odd n) $ do
        b <- get p sRxLine
        if | n == 1 -> modifyS sSkstat (.&. complement 0x02)
           | n < 19 -> modifyS sRxShift (\x -> (x `shiftR` 1) .|. (b `shiftL` 7))
           | otherwise -> do
               get p sRxShift >>= set p sSerin
               when (b == 0) (modifyS sSkstat (.&. complement 0x80))
               en <- get p sIrqen
               irqst <- get p sIrqst
               when (testBit en 5) $
                 if not (testBit irqst 5)
                   then modifyS sSkstat (.&. complement 0x20)
                   else set p sIrqst (irqst .&. complement 0x20)
               modifyS sSkstat (.|. 0x02)
               set p sRxOn 0
    let mode = skctl .&. 0x60
        outClock = (testBit fire 3 && (mode == 0x20 || mode == 0x40))
                || (testBit fire 1 && mode == 0x60)
    when outClock $ do
      ph <- (`xor` 1) <$> get p sSerPhase
      set p sSerPhase ph
      n <- get p sSerCount
      when (ph == 1 && n > 0) (set p sSerTickIn 2)
  serialStep = do
    n <- (subtract 1) <$> get p sSerCount
    set p sSerCount (max 0 n)
    when (n <= 0) $ do
      valid <- get p sSerValid
      st <- get p sIrqst
      if valid /= 0
        then do
          b <- get p sSerout
          set p sSerShift b
          set p sSerValid 0
          set p sSerCount 10
          modifySTRef' (pkOut p) (fromIntegral b :)
          en <- get p sIrqen
          set p sIrqst ((st .|. 0x08) .&. (if en .&. 0x10 /= 0 then complement 0x10 else 0xFF))
        else set p sIrqst (st .&. complement 0x08)
    -- the output line: start bit, data LSB first, stop bit; 1 when idle
    c <- get p sSerCount
    sh <- get p sSerShift
    set p sSerBit $ if | c > 9 -> 0
                       | c > 1 -> (sh `shiftR` (9 - c)) .&. 1
                       | otherwise -> 1

  -- Altirra's description of POKEY's keyboard state machine: 0 waiting
  -- for a key, 1 waiting for its bounce (the same code again: KBCODE, the
  -- IRQ), 2 waiting for its release, 3 waiting for the release to settle;
  -- without debounce (SKCTL bit 0) the compare always matches. CONTROL is
  -- latched at code 0, SHIFT at 16, BREAK at 48 (its own IRQ, bit 7).
  keyScan skctl = do
    kc <- (.&. 0x3F) <$> get p sKeyScan
    modifyS sKeyScan (\k -> (k + 1) .&. 0x3F)
    mods <- get p sModKeys
    lat <- get p sModLatched
    when (kc == 0) $ set p sModLatched ((lat .&. complement 2) .|. (mods .&. 2))
    when (kc == 0x10) $ do
      set p sModLatched ((lat .&. complement 1) .|. (mods .&. 1))
      modifyS sSkstat (\v -> if testBit mods 0 then v .&. complement 0x08 else v .|. 0x08)
    when (kc == 0x30) $ do
      let was = testBit lat 2
          now = testBit mods 2
      set p sModLatched ((lat .&. complement 4) .|. (mods .&. 4))
      when (now && not was) $ do
        en <- get p sIrqen
        when (testBit en 7) (modifyS sIrqst (.&. complement 0x80))
    lo <- get p sKeysLo
    hi <- get p sKeysHi
    let held = testBit (if kc < 32 then lo else hi) (kc .&. 31)
    st <- get p sKeyState
    latch <- get p sKeyLatch
    lat' <- get p sModLatched
    let same = kc == latch || not (testBit skctl 0)
    case st of
      0 -> when held $ set p sKeyLatch kc >> set p sKeyState 1
      1 | held && same -> do
            set p sKeyState 2
            set p sKbcode (kc .|. (if testBit lat' 0 then 0x40 else 0) .|. (if testBit lat' 1 then 0x80 else 0))
            modifyS sSkstat (.&. complement 0x04)
            en <- get p sIrqen
            irqst <- get p sIrqst
            when (testBit en 6) $
              if not (testBit irqst 6)
                then modifyS sSkstat (.&. complement 0x40) -- overrun
                else set p sIrqst (irqst .&. complement 0x40)
        | held -> set p sKeyState 0
        | same -> set p sKeyState 0
        | otherwise -> pure ()
      2 -> when (same && not held) (set p sKeyState 3)
      _ -> when same $
             if held then set p sKeyState 2
             else modifyS sSkstat (.|. 0x04) >> set p sKeyState 0
  modifyS sl f = get p sl >>= set p sl . f

step4, step5 :: Int -> Int
step4 q = (q `shiftR` 1) .|. (complement ((q `shiftL` 2) `xor` (q `shiftL` 3)) .&. 8)
step5 q = (q `shiftR` 1) .|. (complement ((q `shiftL` 2) `xor` (q `shiftL` 4)) .&. 16)

-- | The four channels' levels mixed, before the output curve
-- ("A800.Mixer"): each channel whose output is on adds its volume bits'
-- weights (HRM 5.3, "Volume control": uneven DAC steps).
pokeyLevel :: Pokey s -> ST s Int
pokeyLevel p = do
  audctl <- get p sAudctl
  hp1 <- get p sHp1
  hp2 <- get p sHp2
  ls <- forM [0 .. 3] $ \i -> do
    c <- get p (sAudc i)
    o <- get p (sOut i)
    -- channels 1 and 2 go through the high-pass XOR; with the filter off
    -- its flip-flop is 1 (HRM 5.3)
    let hp | i == 0 = if testBit audctl 2 then hp1 else 1
           | i == 1 = if testBit audctl 1 then hp2 else 1
           | otherwise = 0
        on | testBit c 4 = True
           | otherwise = (o `xor` hp) /= 0
    pure (if on then volumeWeight (c .&. 15) else 0)
  pure (sum ls)

-- | A channel's volume as the sum of its bits' weights (Altirra's measured
-- 0.12, 0.26, 0.56, 1.12 steps, in hundredths).
volumeWeight :: Int -> Int
volumeWeight v = sum [ w | (b, w) <- zip [0 ..] [12, 26, 56, 112], testBit v b ]

-- | The serial input line (the cassette deck's data; 'True' = 1, idle),
-- for the next tick on; SKSTAT bit 4 shows it at once.
pokeySetSerialIn :: Pokey s -> Bool -> ST s ()
pokeySetSerialIn p b = do
  let v = if b then 1 else 0
  set p sRxLine v
  get p sSkstat >>= \s -> set p sSkstat (if b then s .|. 0x10 else s .&. complement 0x10)

-- | Keys held: bit n of @keys@ = key code n (the matrix position POKEY
-- scans), @mods@ bit 0 SHIFT, 1 CONTROL, 2 BREAK.
pokeySetKeys :: Pokey s -> Word64 -> Word8 -> ST s ()
pokeySetKeys p keys mods = do
  set p sKeysLo (fromIntegral (keys .&. 0xFFFFFFFF))
  set p sKeysHi (fromIntegral (keys `shiftR` 32))
  set p sModKeys (fromIntegral mods)

step9, step17 :: Int -> Int
step9 q = (q `shiftR` 1) .|. ((complement ((q `shiftL` 8) `xor` (q `shiftL` 3))) .&. 0x100)
step17 q = (q `shiftR` 1) .|. ((complement ((q `shiftL` 16) `xor` (q `shiftL` 11))) .&. 0x10000)

-- | The IRQ output.
pokeyIrq :: Pokey s -> ST s Bool
pokeyIrq p = (/= 0) <$> get p sIrqLine

-- | Bytes the serial port has sent, oldest first.
pokeySerialOut :: Pokey s -> ST s [Word8]
pokeySerialOut p = reverse <$> readSTRef (pkOut p)

-- | Read register @r@ (the low four bits count).
pokeyRead :: Pokey s -> Word16 -> ST s Word8
pokeyRead p r = case r .&. 0x0F of
  n | n < 8 -> pure 228 -- no paddles: the pots count to 228 (step 4.4)
  0x08 -> fromIntegral <$> get p sAllpot
  0x09 -> fromIntegral <$> get p sKbcode
  0x0A -> do
    skctl <- get p sSkctl
    audctl <- get p sAudctl
    v <- if audctl .&. 0x80 /= 0
           then (.&. 0xFF) <$> get p sPoly9
           else (\q -> (q `shiftR` 8) .&. 0xFF) <$> get p sPoly17
    let rnd = complement v .&. 0xFF
    if initMode skctl
      then do
        t <- get p sTime
        i <- get p sInitAt
        let off = t - i
        pure (if off > 10 then 0xFF else fromIntegral (rnd .|. ((0xFFE00 `shiftR` off) .&. 0xFF)))
      else pure (fromIntegral rnd)
  0x0D -> fromIntegral <$> get p sSerin
  0x0E -> fromIntegral <$> get p sIrqst
  0x0F -> fromIntegral <$> get p sSkstat
  _ -> pure 0xFF

-- | What a save state takes from POKEY (side track N,
-- "A800.Spec.SaveState"): AUDF1-4, AUDC1-4, AUDCTL, SKCTL, IRQEN as
-- written; IRQST, SKSTAT, KBCODE; SEROUT and whether its byte waits.
pokeySnapshot :: Pokey s -> ST s [Int]
pokeySnapshot p = mapM (get p) (map sAudf [0 .. 3] <> map sAudc [0 .. 3] <> [sAudctl, sSkctl, sIrqen, sIrqst, sSkstat, sKbcode, sSerout, sSerValid])

-- | Nothing on its way a save state can't put back: no STIMER reload
-- pending, the serial port idle, or a byte waiting in SEROUT with no
-- clock to send it (SKCTL bits 6-5 clear: what writing SEROUT under
-- that SKCTL leaves).
pokeyQuiet :: Pokey s -> ST s Bool
pokeyQuiet p = do
  valid <- get p sSerValid
  count <- get p sSerCount
  tick <- get p sSerTickIn
  stimer <- get p sStimerIn
  skctl <- get p sSkctl
  let idle = valid == 0 && count == 0
      stalled = valid == 1 && count == 1 && skctl .&. 0x60 == 0
  rx <- get p sRxOn
  pure (stimer == 0 && tick == 0 && rx == 0 && (idle || stalled))

-- | Write register @r@.
pokeyWrite :: Pokey s -> Word16 -> Word8 -> ST s ()
pokeyWrite p r v0 = case r .&. 0x0F of
  n | n < 8 -> set p (if even n then sAudf (fromIntegral n `shiftR` 1) else sAudc (fromIntegral n `shiftR` 1)) v
  0x08 -> set p sAudctl v
  0x09 -> do
    -- STIMER: reload in three cycles; cancels an underflow of this cycle;
    -- the channels' output flip-flops go to 1 (HRM 5.3, "Resetting the timers").
    set p sStimerIn 3
    forM_ [0 .. 3] $ \i -> set p (sOut i) 1
    forM_ [0 .. 3] $ \i -> do
      rl <- get p (sRel i)
      when (rl == 3) (set p (sRel i) 0)
  0x0A -> get p sSkstat >>= \s -> set p sSkstat (s .|. 0xE0) -- SKRES
  0x0B -> pure () -- POTGO (step 4.4)
  0x0D -> do
    set p sSerout v
    set p sSerValid 1
    n <- get p sSerCount
    when (n == 0) (set p sSerCount 1)
  0x0E -> do
    set p sIrqen v
    st <- get p sIrqst
    set p sIrqst ((st .|. (complement v .&. 0xF7)) .&. 0xFF)
  0x0F -> do
    old <- get p sSkctl
    set p sSkctl v
    t <- get p sTime
    let wasInit = initMode old
        isInit = initMode v
    when (isInit && not wasInit) $ do
      set p sInitAt t
      -- the serial state machines stop; a waiting byte is flushed, one
      -- coming in dropped
      set p sRxOn 0
      set p sSerCount 0
      set p sSerValid 0
      set p sSerTickIn 0
    when (wasInit && not isInit) $ do
      set p sPhase64 21
      set p sPhase15 80
      set p sPoly9 0
      set p sPoly17 0
    when (v .&. 0x70 == 0) (set p sSerPhase 0)
  _ -> pure ()
 where
  v = fromIntegral v0 :: Int
