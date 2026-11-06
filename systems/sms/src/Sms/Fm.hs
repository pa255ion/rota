{-# LANGUAGE StrictData #-}
{-|
The FM unit's YM2413 in Clash (M6 step 6.5): one function per internal
cycle ('fmCycle'), in lockstep with the reference model "Sms.Spec.Fm"
(@sms-lockstep@: every pin of every cycle). The rules are the reference's
(docs/systems/ym2413.md and its 10.5), written for hardware: one slot's work
per cycle, its state in registers, the tables as ROMs.

__Time.__ The spec works slot s of frame f with the registers its fetch saw
(cycle 18 f + s - 1) and looks at rhythm mode up to three cycles later
(its output stage). Hardware can't look ahead, so here every write is held
back three cycles before it reaches the registers (the same landing rules,
three cycles later) and a slot is worked three cycles after its fetch:
slot (c - 2) mod 18 in cycle c. Rhythm mode's bit is also kept as written,
undelayed, for the parts of the pipeline that read it after the fetch
(a short history of it, one bit per cycle). A cycle: work the slot, then
apply the delayed writes that land in it, then take the new write.

__Output__: each cycle, the pins (MO, RO) of the cycle two later (the slot
just worked is what they show then): so 'fmCycle' at cycle c returns the
spec's pins of cycle c + 2.
-}
module Sms.Fm
  ( FmC
  , fmPowerOn
  , fmCycle
  , FmWrite
  ) where

import Clash.Prelude

-- | A write strobe presented at this cycle: True for data (port 1), the byte.
type FmWrite = Maybe (Bool, Unsigned 8)

data Slot = Slot
  { slP    :: Unsigned 19   -- ^ the phase accumulator
  , slL    :: Unsigned 7    -- ^ the EG level
  , slSt   :: Unsigned 2    -- ^ the EG state (0 attack, 1 decay, 2 sustain, 3 release)
  , slY1   :: Signed 12     -- ^ the output kept for feedback
  , slY2   :: Signed 12     -- ^ the one before
  , slYCur :: Signed 12     -- ^ this frame's output
  , slEv   :: Bool          -- ^ carriers: an attack event last frame
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFDataX

data FmC = FmC
  { fRegs    :: Vec 64 (Unsigned 8)  -- ^ the registers as the (delayed) writes left them
  , fSlots   :: Vec 18 Slot
  , fDelay   :: Vec 3 FmWrite         -- ^ the writes of the last three cycles, oldest last
  , fAddr    :: Maybe (Unsigned 6)    -- ^ the selected register (delayed side)
  , fPend    :: Maybe (Unsigned 6, Unsigned 8, Unsigned 5) -- ^ a channel write: register, byte, cycles until it lands
  , fRAddr   :: Bool                  -- ^ the undelayed side's address is 0Eh
  , fRhy     :: Bool                  -- ^ 0Eh bit 5 as written (undelayed)
  , fRHist   :: Vec 4 Bool            -- ^ that bit at the start of this cycle and the three before (newest first)
  , fCyc     :: Index 18              -- ^ this cycle's number in its sample
  , fStarted :: Unsigned 2            -- ^ cycles since reset, up to 2 (no slot is worked before cycle 2)
  , fFrame   :: Unsigned 20           -- ^ the frame of the slot worked this cycle (mod 2^20)
  , fAmSub   :: Index 64              -- ^ the tremolo's counter: samples into its step
  , fAmStep  :: Index 210             -- ^ and its step
  , fAmPrimed :: Bool                 -- ^ the first sample is over (the tremolo counts from frame 1)
  , fNoise   :: Unsigned 23           -- ^ the noise LFSR, two steps behind the cycle
  , fGroup   :: Bool                  -- ^ slots 12-17 are drums this frame
  , fHhPhi   :: Unsigned 10
  , fTcyBits :: Unsigned 10
  , fHeld    :: Vec 5 (Signed 9)      -- ^ HH, TOM, BD, SD, TCY as latched for the next sample's RO copies
  , fPrevV   :: Vec 18 (Signed 9)     -- ^ every slot's v (y >> 3) as last worked
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFDataX

fmPowerOn :: FmC
fmPowerOn = FmC
  { fRegs = repeat 0
  , fSlots = repeat (Slot 0 127 3 0 0 0 False)
  , fDelay = repeat Nothing, fAddr = Nothing, fPend = Nothing
  , fRAddr = False, fRhy = False, fRHist = repeat False
  , fCyc = 0, fStarted = 0, fFrame = maxBound, fAmSub = 0, fAmStep = 0, fAmPrimed = False
  , fNoise = 0, fGroup = False, fHhPhi = 0, fTcyBits = 0
  , fHeld = repeat 0, fPrevV = repeat 0
  }

-- * Tables

patchRom :: Vec 152 (Unsigned 8)   -- row 0 unused, 1-15 the instruments, 16-18 the drums
patchRom = $(listToVecTH ([0, 0, 0, 0, 0, 0, 0, 0
  , 0x71, 0x61, 0x1E, 0x17, 0xD0, 0x78, 0x00, 0x17
  , 0x13, 0x41, 0x1A, 0x0D, 0xD8, 0xF7, 0x23, 0x13
  , 0x13, 0x01, 0x99, 0x00, 0xF2, 0xC4, 0x11, 0x23
  , 0x31, 0x61, 0x0E, 0x07, 0xA8, 0x64, 0x70, 0x27
  , 0x32, 0x21, 0x1E, 0x06, 0xE0, 0x76, 0x00, 0x28
  , 0x31, 0x22, 0x16, 0x05, 0xE0, 0x71, 0x00, 0x18
  , 0x21, 0x61, 0x1D, 0x07, 0x82, 0x81, 0x10, 0x07
  , 0x23, 0x21, 0x2D, 0x14, 0xA2, 0x72, 0x00, 0x07
  , 0x61, 0x61, 0x1B, 0x06, 0x64, 0x65, 0x10, 0x17
  , 0x41, 0x61, 0x0B, 0x18, 0x85, 0xF7, 0x71, 0x07
  , 0x13, 0x01, 0x83, 0x11, 0xFA, 0xE4, 0x10, 0x04
  , 0x17, 0xC1, 0x24, 0x07, 0xF8, 0xF8, 0x22, 0x12
  , 0x61, 0x50, 0x0C, 0x05, 0xC2, 0xF5, 0x20, 0x42
  , 0x01, 0x01, 0x55, 0x03, 0xC9, 0x95, 0x03, 0x02
  , 0x61, 0x41, 0x89, 0x03, 0xF1, 0xE4, 0x40, 0x13
  , 0x01, 0x01, 0x18, 0x0F, 0xDF, 0xF8, 0x6A, 0x6D
  , 0x01, 0x01, 0x00, 0x00, 0xC8, 0xD8, 0xA7, 0x48
  , 0x05, 0x01, 0x00, 0x00, 0xF8, 0xAA, 0x59, 0x55 ] :: [Unsigned 8]))

logsinRom :: Vec 256 (Unsigned 12)
logsinRom = $(listToVecTH [ round (negate (logBase 2 (sin ((fromIntegral i + 0.5) * pi / 512 :: Double))) * 256) :: Unsigned 12 | i <- [0 .. 255 :: Int] ])

expRom :: Vec 256 (Unsigned 11)
expRom = $(listToVecTH [ round ((2 :: Double) ** (fromIntegral (255 - i) / 256) * 1024) :: Unsigned 11 | i <- [0 .. 255 :: Int] ])

mult2 :: Vec 16 (Unsigned 5)
mult2 = 1 :> 2 :> 4 :> 6 :> 8 :> 10 :> 12 :> 14 :> 16 :> 18 :> 20 :> 20 :> 24 :> 24 :> 30 :> 30 :> Nil

kslRom :: Vec 16 (Unsigned 7)
kslRom = 0 :> 32 :> 40 :> 45 :> 48 :> 51 :> 53 :> 55 :> 56 :> 58 :> 59 :> 60 :> 61 :> 62 :> 63 :> 64 :> Nil

slotChannel :: Vec 18 (Index 9)
slotChannel = 0 :> 1 :> 2 :> 0 :> 1 :> 2 :> 3 :> 4 :> 5 :> 3 :> 4 :> 5 :> 6 :> 7 :> 8 :> 6 :> 7 :> 8 :> Nil

-- * A cycle

-- | One internal cycle: the write presented at it in, the pins of the cycle
-- two later out.
fmCycle :: FmWrite -> FmC -> (FmC, (Signed 10, Signed 10))
fmCycle w c0 =
  let c = fCyc c0
      -- rhythm mode as at the start of this cycle (undelayed)
      hist = fRhy c0 :> init (fRHist c0)
      c1 = c0 { fRHist = hist }
      -- the slot worked now: fetched three cycles ago; its frame starts with slot 0
      s = (fromIntegral (if c >= 2 then c - 2 else c + 16) :: Index 18)
      newFrame = s == 0
      frame = if newFrame then fFrame c1 + 1 else fFrame c1
      -- the tremolo's step is ((f - 1) div 64) mod 210, and 0 for frame 0
      -- ([6.7]); counted per frame (fAmSub = (f - 1) mod 64) from frame 1
      (amSub, amStep, primed)
        | not newFrame = (fAmSub c1, fAmStep c1, fAmPrimed c1)
        | not (fAmPrimed c1) = (0, 0, frame /= 0)
        | fAmSub c1 == maxBound = (0, if fAmStep c1 == maxBound then 0 else fAmStep c1 + 1, True)
        | otherwise = (fAmSub c1 + 1, fAmStep c1, True)
      c2 = c1 { fFrame = frame, fAmSub = amSub, fAmStep = amStep, fAmPrimed = primed }
      working = fStarted c0 == 2
      (c3, v) = if working then slotWork s c2 else (c2, 0)
      -- the pins of cycle c + 2 (the slot just worked shows then)
      pinsOut = pins c3 v
      -- the delayed writes landing now
      c4 = landWrites c3
      -- the noise LFSR, stepped (two behind the cycle: none for the first two)
      c5 = if working then c4 { fNoise = noiseStep (fNoise c4) } else c4
      -- the new write: the undelayed rhythm bit, and into the delay
      (rAddr, rhy) = case w of
        Just (False, a) -> (a == 0x0E, fRhy c5)
        Just (True, d) | fRAddr c5 -> (True, testBit d 5)
        _ -> (fRAddr c5, fRhy c5)
      delayed = last (fDelay c5)
      c6 = c5 { fRAddr = rAddr, fRhy = rhy, fDelay = w +>> fDelay c5 }
      c7 = takeWrite delayed c6
      c8 = c7 { fCyc = if c == maxBound then 0 else c + 1
              , fStarted = if fStarted c7 == 2 then 2 else fStarted c7 + 1 }
  in (c8, pinsOut)

noiseStep :: Unsigned 23 -> Unsigned 23
noiseStep r =
  let nb = if r == 0 then 1 else (r .&. 1) `xor` ((r `shiftR` 14) .&. 1)
  in (r `shiftR` 1) .|. (nb `shiftL` 22)

-- | A delayed write strobe reaching the registers ([3.4], 10.5): an address
-- selects a register (or none); data to a mode register lands now (seen by
-- the next slot's fetch), to a channel register at the first cycle after
-- the next whose number is the address's low nibble.
takeWrite :: FmWrite -> FmC -> FmC
takeWrite Nothing f = f
takeWrite (Just (False, a)) f = f { fAddr = if a .&. 0xC0 == 0 then Just (resize a) else Nothing }
takeWrite (Just (True, d)) f = case fAddr f of
  Nothing -> f
  Just a
    | a < 0x10 -> if a <= 7 || a == 0x0E || a == 0x0F then f { fRegs = replace a d (fRegs f) } else f
    | otherwise ->
        let cn = fromIntegral (a .&. 0x0F) :: Index 18
            -- the strobe's cycle is fCyc (the delayed write's own cycle,
            -- shifted by the delay); "at" is the cycle after. The landing
            -- cycle's number is the address's low nibble in real time, so
            -- three more here (the delay)
            atC = if fCyc f == maxBound then 0 else fCyc f + 1
            target = fromIntegral ((fromIntegral cn + 3 :: Unsigned 5) `mod` 18) :: Index 18
            dist = if target > atC then fromIntegral (target - atC) else fromIntegral target + 18 - fromIntegral atC :: Unsigned 5
            reg = (a .&. 0x30) .|. fromIntegral (cn `mod` 9)
            -- a second write to the same register before the first landed replaces it
        in f { fPend = Just (reg, d, dist + 1) }

-- | The pending channel write lands when its count runs out.
landWrites :: FmC -> FmC
landWrites f = case fPend f of
  Just (r, d, n) | n <= 1 -> f { fRegs = replace r d (fRegs f), fPend = Nothing }
                 | otherwise -> f { fPend = Just (r, d, n - 1) }
  Nothing -> f

-- | The EG clock's locks of a frame [6.4].
egLocks :: Unsigned 20 -> (Unsigned 5, Unsigned 2)
egLocks fr =
  let n = resize (fr `shiftR` 2) :: Unsigned 18
      z = countTrailingZeros n + 1
      sl = if n == 0 then 0 else if z <= 13 then fromIntegral z else 0
  in (sl, resize n)

-- | One slot's work for its frame: the operator, the phase and the EG, as
-- the reference; its v (y >> 3).
slotWork :: Index 18 -> FmC -> (FmC, Signed 9)
slotWork s f0 =
  let regs = fRegs f0
      rg i = regs !! (i :: Unsigned 6)
      ch = slotChannel !! s
      chI = fromIntegral ch :: Unsigned 6
      car = testBit ((fromIntegral s :: Unsigned 5) `div` 3) 0
      r0e = rg 0x0E
      test = rg 0x0F
      grp = if s == 12 then testBit r0e 5 else fGroup f0
      rBefore = fRHist f0 !! (3 :: Index 4)
      rNow = fRHist f0 !! (2 :: Index 4)
      rOut = fRHist f0 !! (0 :: Index 4)
      drum = s >= 12 && grp
      hhTom = drum && (s == 13 || s == 14)
      singleOp = (s == 13 || s == 14) && rBefore
      outCarrier = (s == 13 || s == 14) && rOut
      noMod = (s == 16 || s == 17) && rBefore
      f1 = rg (0x10 + chI)
      f2 = rg (0x20 + chI)
      f3 = rg (0x30 + chI)
      fnum = resize f1 .|. ((resize f2 .&. 1) `shiftL` 8) :: Unsigned 9
      block = resize ((f2 `shiftR` 1) .&. 7) :: Unsigned 3
      sus = testBit f2 5
      inst = resize (f3 `shiftR` 4) :: Unsigned 4
      vol = resize (f3 .&. 0x0F) :: Unsigned 4
      byte :: Unsigned 3 -> Unsigned 8
      byte i
        | drum = patchRom !! ((fromIntegral ch + 10 :: Unsigned 8) * 8 + resize i)
        | inst == 0 = rg (resize i)
        | otherwise = patchRom !! ((resize inst :: Unsigned 8) * 8 + resize i)
      side0 = byte (if car then 1 else 0)
      am = testBit side0 7
      vib = testBit side0 6
      egTyp = testBit side0 5
      ksrBit = testBit side0 4
      mult = resize side0 :: Unsigned 4
      ksl = resize ((if car then byte 3 else byte 2) `shiftR` 6) :: Unsigned 2
      tl = resize (byte 2 .&. 0x3F) :: Unsigned 6
      fb = resize (byte 3 .&. 7) :: Unsigned 3
      halfSine = if car || outCarrier then testBit (byte 3) 4 else testBit (byte 3) 3
      arDr = byte (if car then 5 else 4)
      slRr = byte (if car then 7 else 6)
      ar = resize (arDr `shiftR` 4) :: Unsigned 4
      dr = resize arDr :: Unsigned 4
      sl = resize (slRr `shiftR` 4) :: Unsigned 4
      rr = resize slRr :: Unsigned 4
      drumKeyBit = case s of
        12 -> 4
        15 -> 4
        13 -> 0
        16 -> 3
        14 -> 2
        _ -> 1 :: Int
      kon = testBit f2 4 || (drum && testBit r0e drumKeyBit)
      tlAtt :: Unsigned 8
      tlAtt | hhTom = 8 * resize inst
            | car = 8 * resize vol
            | otherwise = 2 * resize tl
      slot = fSlots f0 !! s
      l = slL slot
      st = slSt slot
      off = (l `shiftR` 2) == 31
      ev = st == 3 && kon && off
      rBase | ev = ar
            | st == 0 = ar
            | st == 1 = dr
            | st == 2 = if egTyp then 0 else rr
            | otherwise = if sus then 5 else rr
      r | not kon && not car && not hhTom = 0
        | kon && st == 3 && not off = 12
        | not kon && not sus && car && not egTyp = 7
        | otherwise = rBase
      zr = r == 0
      ksr4 = resize block * 2 + resize (fnum `shiftR` 8) :: Unsigned 4
      ksr = if ksrBit then ksr4 else ksr4 `shiftR` 2
      rr0 = 4 * resize r + resize ksr :: Unsigned 7
      rR = if rr0 >= 64 then 60 + resize (ksr .&. 3) else rr0
      rh = resize (rR `shiftR` 2) :: Unsigned 4
      rl = resize rR :: Unsigned 2
      fr = fFrame f0
      (shiftLock, lowLock) = egLocks fr
      sCount = resize (if s == 17 then fr + 1 else fr) :: Unsigned 2
      stepHi = (0x0 :> 0x1 :> 0x5 :> 0x7 :> Nil) :: Vec 4 (Unsigned 4)
      h = testBit (stepHi !! rl) (fromIntegral lowLock)
      lo = rh < 12 && not zr && case resize (resize shiftLock + resize rh :: Unsigned 6) :: Unsigned 4 of
        12 -> True
        13 -> testBit rl 1
        14 -> testBit rl 0
        _ -> False
      oddS = testBit sCount 0
      decayStep :: Unsigned 7
      decayStep
        | rh == 15 = 2
        | rh == 14 = if h then 2 else 1
        | rh == 13 = if h then 1 else (if oddS then 1 else 0)
        | rh == 12 = if h then (if oddS then 1 else 0) else (if sCount == 3 then 1 else 0)
        | otherwise = if lo && sCount == 3 then 1 else 0
      attackN :: Maybe (Index 8)
      attackN
        | rh < 12 = if lo then Just 4 else Nothing
        | rh <= 14 = Just (fromIntegral (5 - (resize rh - 11 + (if h then 1 else 0) :: Unsigned 5)))
        | otherwise = Nothing
      -- L - ceil ((L + 1) / 2^n), as (-L - 1) >> n added
      attackDelta :: Signed 9
      attackDelta = case attackN of
        Just n -> (negate (resize (bitCoerce (zeroExtend l) :: Signed 8)) - 1) `shiftR` fromIntegral n
        Nothing -> 0
      kslRaw = let k = fromIntegral (kslRom !! (fnum `shiftR` 5)) - 8 * (8 - fromIntegral block) :: Signed 9
               in if k < 0 then 0 else bitCoerce k :: Unsigned 9
      kslAtt :: Unsigned 9
      kslAtt = case ksl of
        0 -> 0
        1 -> (2 * kslRaw) `shiftR` 2
        2 -> (2 * kslRaw) `shiftR` 1
        _ -> 2 * kslRaw
      amAtt = if am then resize (amOut f0) else 0 :: Unsigned 9
      attSum = resize l + resize kslAtt + resize tlAtt + resize amAtt :: Unsigned 10
      att = if testBit test 0 then 0 else (if attSum > 127 then 127 else resize attSum) :: Unsigned 7
      -- the phase
      p = slP slot
      vv = (resize (fr `shiftR` 10) :: Unsigned 3)
      f0' = 2 * resize fnum :: Unsigned 11
      delta :: Signed 12
      delta | not vib = 0
            | otherwise = case vv of
                1 -> bitCoerce (resize (f0' `shiftR` 8))
                2 -> bitCoerce (resize (f0' `shiftR` 7))
                3 -> bitCoerce (resize (f0' `shiftR` 8))
                5 -> negate (bitCoerce (resize (f0' `shiftR` 8)))
                6 -> negate (bitCoerce (resize (f0' `shiftR` 7)))
                7 -> negate (bitCoerce (resize (f0' `shiftR` 8)))
                _ -> 0
      fv = bitCoerce (bitCoerce (resize f0' :: Unsigned 12) + delta) :: Unsigned 12
      inc = resize (((((resize fv :: Unsigned 20) `shiftL` fromIntegral block) `shiftR` 1) * resize (mult2 !! mult)) `shiftR` 1) :: Unsigned 19
      phi = resize (p `shiftR` 9) :: Unsigned 10
      evCar = not car && slEv (fSlots f0 !! (s + 3))
      reset = testBit test 2 || (if car || singleOp then ev else evCar)
      -- the operator's input
      hhPhi = if s == 13 then phi else fHhPhi f0
      tcyBits = fTcyBits f0
      tcyNow = if s == 17 && rNow then phi else tcyBits
      noiseBit = resize (fNoise f0 .&. 1) :: Unsigned 10
      hb i = (hhPhi `shiftR` i) .&. 1
      pBit t = (hb 2 `xor` hb 7) .|. (hb 3 `xor` ((t `shiftR` 5) .&. 1)) .|. (((t `shiftR` 3) .&. 1) `xor` ((t `shiftR` 5) .&. 1))
      modIn :: Unsigned 10
      modIn
        | car && not noMod = (resize (bitCoerce (slYCur (fSlots f0 !! (s - 3))) :: Unsigned 12) .&. 0x1FF) `shiftL` 1
        | not car && fb > 0 && not singleOp =
            let y1 = resize (slY1 slot) :: Signed 13
                y2 = resize (slY2 slot)
            in resize (bitCoerce (((y1 + y2) `shiftR` 1) `shiftR` fromIntegral (7 - fb)) :: Unsigned 13)
        | otherwise = 0
      idx0
        | rNow && s == 13 = let pb = pBit tcyBits in (pb `shiftL` 9) .|. (if (pb `xor` noiseBit) /= 0 then 0xD0 else 0x34)
        | rNow && s == 16 = let h8 = hb 8 in (h8 `shiftL` 9) .|. ((h8 `xor` noiseBit) `shiftL` 8)
        | rNow && s == 17 = (pBit tcyNow `shiftL` 9) .|. 0x100
        | otherwise = phi + modIn
      idx = if testBit idx0 8 then idx0 `xor` 0xFF else idx0
      neg = testBit idx 9
      a = let x = resize (logsinRom !! (resize idx :: Unsigned 8)) + 16 * resize att :: Unsigned 13
          in if x > 4095 then 4095 else x
      silent = l == 127
      sh = resize (a `shiftR` 8) .|. (if silent || (neg && halfSine) then 12 else 0) :: Unsigned 4
      y0 = bitCoerce (resize (expRom !! (resize a :: Unsigned 8)) `shiftR` fromIntegral sh :: Unsigned 12) :: Signed 12
      y = if neg && not silent then complement y0 else y0
      v = resize (y `shiftR` 3) :: Signed 9
      -- the EG's next level and state
      l1 = if st /= 0 && off && not ev then 127 else l
      l2 = if ev && rh == 15 then 0 else l1
      (step, next0)
        | st == 0 = (if rh /= 15 && kon && l /= 0 then attackDelta else 0, if l == 0 then 1 else 0)
        | st == 1 = (if not off && not ev && resize (l `shiftR` 3) /= sl then bitCoerce (resize decayStep) else 0, if resize (l `shiftR` 3) == sl then 2 else 1)
        | otherwise = (if not off && not ev then bitCoerce (resize decayStep) else 0, st)
      next1 = if not kon then 3 else next0
      next = if ev then 0 else next1
      lNew = let x = bitCoerce (resize l2 :: Unsigned 9) + step :: Signed 9
             in if x < 0 then 0 else if x > 127 then 127 else fromIntegral x :: Unsigned 7
      keepFb = not car && not outCarrier
      slot' = slot
        { slP = (if reset then 0 else p) + inc
        , slL = lNew, slSt = next
        , slYCur = y
        , slY1 = if keepFb then y else slY1 slot
        , slY2 = if keepFb then slY1 slot else slY2 slot
        , slEv = if car then ev else slEv slot
        }
      -- the drums' values latched for the next sample's RO copies
      held = case s of
        13 -> replace (0 :: Index 5) v (fHeld f0)
        14 -> replace (1 :: Index 5) v (fHeld f0)
        15 -> replace (2 :: Index 5) v (fHeld f0)
        16 -> replace (3 :: Index 5) v (fHeld f0)
        17 -> replace (4 :: Index 5) v (fHeld f0)
        _ -> fHeld f0
      f' = f0 { fSlots = replace s slot' (fSlots f0)
              , fGroup = grp, fHhPhi = hhPhi
              , fTcyBits = if s == 17 && rNow then phi else tcyBits
              }
  in (f' { fPrevV = replace s v (fPrevV f'), fHeld = held }, v)
 where
  -- the tremolo: a triangle 0..105..0, its step ((f - 1) div 64) mod 210
  amOut f = let n = fromIntegral (fAmStep f) :: Unsigned 8
                t = if n <= 105 then n else 210 - n
            in t `shiftR` 3

-- | The pins of the cycle two after this one (op stage k = this cycle):
-- the slot just worked (v), or on a modulator's cycle a drum's latched
-- value, gated by rhythm mode at this cycle ([2.3, 9.1], 10.5).
pins :: FmC -> Signed 9 -> (Signed 10, Signed 10)
pins f v =
  let ck = fCyc f
      rk = fRHist f !! (0 :: Index 4)
      ck5 = fromIntegral ck :: Unsigned 5
      isMod1 = if rk && (ck == 15 || ck == 16) then False else testBit ((ck5 + 1) `div` 3) 0
      held = case ck of
        2 -> Just (fHeld f !! (0 :: Index 5))
        3 -> Just (fHeld f !! (1 :: Index 5))
        4 -> Just (fHeld f !! (2 :: Index 5))
        8 -> Just (fHeld f !! (3 :: Index 5))
        9 -> Just (fHeld f !! (4 :: Index 5))
        _ -> Nothing
      chOut | isMod1 = if rk then maybe 0 id held else 0
            | otherwise = v
      cj = if ck == maxBound then 0 else ck + 1
      cj' = fromIntegral cj :: Unsigned 5
      muteM = testBit (cj' `div` 3) 0 || (rk && ((cj' + 15) `mod` 18) >= 12)
      muteR = not (rk && cj' `elem` (16 :> 17 :> 0 :> 1 :> 2 :> 3 :> 4 :> 5 :> 9 :> 10 :> Nil))
      chOut' = resize chOut :: Signed 10
      sign = if chOut < 0 then -1 else 1
      val = if chOut >= 0 then chOut' + 1 else chOut'
  in (if muteM then sign else val, if muteR then sign else val)
