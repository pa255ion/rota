{-# LANGUAGE StrictData #-}
{-|
POKEY in Clash (M4 step 4.5). It reproduces the reference model
("A800.Spec.Pokey") cycle for cycle; that module has the rules (clocks,
timers and their reload pipeline, two-tone resync, the serial output
shifter, IRQs, RANDOM, the audio channels, the keyboard scan).

__Ticks__ ('PokeyIn'): 'PTick' is the start of a machine cycle, then at
most one CPU access ('PRead', 'PWrite') in it, as in the spec. 'PKeys'
sets the keys held (from the platform, between cycles). Each tick is one
clock of combinational logic.

The spec's cycle counters become what the hardware needs: the cycles since
initialization mode began, saturating ('pInitAge', for RANDOM's forced
bits), instead of absolute times.
-}
module A800.Pokey
  ( PokeyIn (..)
  , PokeyOut (..)
  , Pokey (..)
  , pokeyInit
  , pokeyStep
  , pokeyLevelOf
  ) where

import Clash.Prelude

data PokeyIn
  = PTick
  | PRead (Unsigned 4)
  | PWrite (Unsigned 4) (Unsigned 8)
  | PKeys (BitVector 64) (BitVector 3) -- ^ keys held (bit n = code n); SHIFT, CONTROL, BREAK
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFDataX

data PokeyOut = PokeyOut
  { poRead  :: Unsigned 8          -- ^ a register read's value
  , poIrq   :: Bool                -- ^ the IRQ output
  , poLevel :: Unsigned 10         -- ^ the channels mixed (before "A800.Mixer"'s curve)
  , poSent  :: Maybe (Unsigned 8)  -- ^ a byte loaded into the serial output shifter
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFDataX

data Pokey = Pokey
  { pAudf      :: Vec 4 (Unsigned 8)
  , pAudc      :: Vec 4 (Unsigned 8)
  , pCnt       :: Vec 4 (Unsigned 8)
  , pRel       :: Vec 4 (Unsigned 2)   -- ^ reload countdown (underflow path)
  , pRelFire   :: Vec 4 Bool
  , pAudctl    :: Unsigned 8
  , pSkctl     :: Unsigned 8
  , pIrqen     :: Unsigned 8
  , pIrqst     :: Unsigned 8
  , pSkstat    :: Unsigned 8
  , pKbcode    :: Unsigned 8
  , pSerin     :: Unsigned 8
  , pSerout    :: Unsigned 8
  , pSerValid  :: Bool
  , pSerCount  :: Unsigned 4
  , pSerShift  :: Unsigned 8
  , pSerPhase  :: Bool
  , pSerTickIn :: Unsigned 2
  , pStimerIn  :: Unsigned 2
  , pPhase64   :: Unsigned 5
  , pPhase15   :: Unsigned 7
  , pPoly9     :: BitVector 9
  , pPoly17    :: BitVector 17
  , pInitAge   :: Unsigned 4           -- ^ cycles since initialization mode began (saturates)
  , pIrqLine   :: Bool
  , pFire      :: BitVector 4
  , pTwoTone   :: Bool
  , pSerBit    :: Bit
  , pOut       :: Vec 4 Bit
  , pHp1       :: Bit
  , pHp2       :: Bit
  , pPoly4     :: BitVector 4
  , pPoly5     :: BitVector 5
  , pHist4     :: BitVector 4           -- ^ the noise bits' last four values (bit 0 newest)
  , pHist5     :: BitVector 4
  , pHist917   :: BitVector 4
  , pKeyState  :: Unsigned 2
  , pKeyLatch  :: Unsigned 6
  , pKeyScan   :: Unsigned 6
  , pKeys      :: BitVector 64
  , pMods      :: BitVector 3
  , pModLatch  :: BitVector 3
  , pLoaded    :: Bool                 -- ^ the shifter loaded a SEROUT byte this cycle
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFDataX

-- | Power on, as the spec's 'A800.Spec.Pokey.newPokey'.
pokeyInit :: Pokey
pokeyInit = Pokey
  { pAudf = repeat 0, pAudc = repeat 0, pCnt = repeat 1, pRel = repeat 0, pRelFire = repeat False
  , pAudctl = 0, pSkctl = 0, pIrqen = 0, pIrqst = 0xF7, pSkstat = 0x7F, pKbcode = 0
  , pSerin = 0, pSerout = 0, pSerValid = False, pSerCount = 0, pSerShift = 0, pSerPhase = False
  , pSerTickIn = 0, pStimerIn = 0, pPhase64 = 28, pPhase15 = 114, pPoly9 = 0, pPoly17 = 0
  , pInitAge = maxBound, pIrqLine = False, pFire = 0, pTwoTone = False, pSerBit = 0
  , pOut = repeat 0, pHp1 = 0, pHp2 = 0, pPoly4 = 0, pPoly5 = 0, pHist4 = 0, pHist5 = 0, pHist917 = 0
  , pKeyState = 0, pKeyLatch = 0, pKeyScan = 0, pKeys = 0, pMods = 0, pModLatch = 0, pLoaded = False }

initMode :: Unsigned 8 -> Bool
initMode skctl = skctl .&. 3 == 0

pokeyStep :: Pokey -> PokeyIn -> (Pokey, PokeyOut)
pokeyStep p i = case i of
  PTick -> let p' = tick (p { pLoaded = False }) in (p', out p' 0 (pLoaded p'))
  PRead r -> (p, out p (readReg p r) False)
  PWrite r v -> let p' = writeReg p r v in (p', out p' 0 False)
  PKeys k m -> let p' = p { pKeys = k, pMods = m } in (p', out p' 0 False)
 where
  out q v sent = PokeyOut v (pIrqLine q) (pokeyLevelOf q) (if sent then Just (pSerShift q) else Nothing)

-- | One machine cycle.
tick :: Pokey -> Pokey
tick p0 =
  let -- the IRQ line, one cycle behind IRQST
      p1 = p0 { pIrqLine = pIrqen p0 .&. complement (pIrqst p0) /= 0, pInitAge = satInc (pInitAge p0) }
      tt = pTwoTone p1
      p2 = p1 { pTwoTone = False }
      skctl = pSkctl p2
      audctl = pAudctl p2
      fire = pFire p2
      p3 = if fire /= 0 then fired fire skctl (p2 { pFire = 0 }) else p2 { pFire = 0 }
      -- the serial output shifter
      p4 | pSerTickIn p3 > 0 =
             let p' = p3 { pSerTickIn = pSerTickIn p3 - 1 }
             in if pSerTickIn p3 == 1 then serialStep p' else p'
         | otherwise = p3
      -- asynchronous receive (SKCTL bit 4): timers 3 and 4 held reset
      -- while POKEY waits for a start bit, as the spec's; the board has
      -- no serial input (its line stays 1), so it always waits
      hold34 = testBit skctl 4
      p4h | hold34 = p4 { pCnt = replace (3 :: Index 4) (pAudf p4 !! (3 :: Index 4)) (replace (2 :: Index 4) (pAudf p4 !! (2 :: Index 4)) (pCnt p4))
                        , pRel = replace (3 :: Index 4) 0 (replace (2 :: Index 4) 0 (pRel p4)) }
          | otherwise = p4
      running = not (initMode skctl)
      (p64, ph64) = pulse (pPhase64 p4h) 28 running
      (p15, ph15) = pulse (pPhase15 p4h) 114 running
      p5 = p4h { pPhase64 = ph64, pPhase15 = ph15 }
      slow = if testBit audctl 0 then p15 else p64
      clk :: Index 4 -> Bool
      clk ch | ch == 0 && testBit audctl 6 = True
             | ch == 2 && testBit audctl 5 = True
             | otherwise = slow
      link12 = testBit audctl 4
      link34 = testBit audctl 3
      linkedLow ch = (ch == 0 && link12) || (ch == 2 && link34)
      linkedHigh ch = (ch == 1 && link12) || (ch == 3 && link34)
      -- two-tone resync
      p6 | tt = p5 { pCnt = replace (1 :: Index 4) (pAudf p5 !! (1 :: Index 4)) (replace (0 :: Index 4) (pAudf p5 !! (0 :: Index 4)) (pCnt p5))
                   , pRel = replace (1 :: Index 4) 0 (replace (0 :: Index 4) 0 (pRel p5)) }
         | otherwise = p5
      -- STIMER's reload
      stimerNow = pStimerIn p6 == 1
      p7 | pStimerIn p6 > 0 =
             let p' = p6 { pStimerIn = pStimerIn p6 - 1 }
             in if stimerNow then p' { pCnt = pAudf p', pRel = repeat 0 } else p'
         | otherwise = p6
      -- the reload pipelines, in channel order
      (p8, highs, reloaded) = foldl pipe (p7, 0 :: BitVector 4, (if tt then 3 else 0) .|. (if hold34 then 0xC else 0) :: BitVector 4) (indicesI :: Vec 4 (Index 4))
      pipe (q, hs, rl) ch =
        let r = pRel q !! ch
        in if r == 0 then (q, hs, rl) else
             let q1 = q { pRel = replace ch (r - 1) (pRel q) }
             in if r /= 1 then (q1, hs, rl) else
                  let f = pRelFire q1 !! ch
                  in if linkedLow ch
                       then if not f then (q1, hs, rl)
                            else (q1 { pFire = pFire q1 .|. bit (fromIntegral ch) }, hs .|. bit (fromIntegral ch + 1), rl)
                       else
                         let c1 = replace ch (pAudf q1 !! ch) (pCnt q1)
                             c2 = if linkedHigh ch then replace (ch - 1) (pAudf q1 !! (ch - 1)) c1 else c1
                             fr = if f then pFire q1 .|. bit (fromIntegral ch) else pFire q1
                             rl' = rl .|. bit (fromIntegral ch) .|. (if linkedHigh ch then bit (fromIntegral ch - 1) else 0)
                         in (q1 { pCnt = c2, pFire = fr }, hs, rl')
      -- a linked low timer's pulse clocks its high timer
      p9 = foldl (\q ch -> if testBit highs (fromIntegral ch) then countHigh q ch else q) p8 (indicesI :: Vec 4 (Index 4))
      countHigh q ch =
        if pRel q !! ch /= 0 then q else
          let c = pCnt q !! ch
          in if c == 0 then q { pRel = replace ch 3 (pRel q), pRelFire = replace ch True (pRelFire q) }
             else q { pCnt = replace ch (c - 1) (pCnt q) }
      -- the counters
      p10 = foldl count p9 (indicesI :: Vec 4 (Index 4))
      count q ch =
        let r = pRel q !! ch
        in if not (not (linkedHigh ch) && clk ch && (r == 0 || linkedLow ch) && not stimerNow && not (testBit reloaded (fromIntegral ch)))
             then q
             else let c = pCnt q !! ch
                  in if c == 0
                       then q { pCnt = if linkedLow ch then replace ch 0xFF (pCnt q) else pCnt q
                              , pRel = replace ch 3 (pRel q), pRelFire = replace ch True (pRelFire q) }
                       else q { pCnt = replace ch (c - 1) (pCnt q) }
      -- RANDOM's generators, the 4/5-bit ones, the noise history
      q9 = step9 (pPoly9 p10)
      q17 = step17 (pPoly17 p10)
      q4 = if running then step4 (pPoly4 p10) else 0
      q5 = if running then step5 (pPoly5 p10) else 0
      n917 = if testBit audctl 7 then lsb q9 else q17 ! (8 :: Int)
      b4 = if running then lsb q4 else 0
      b5 = if running then lsb q5 else 1
      b917 = if running then n917 else 0
      push h b = (h `shiftL` 1) .|. zeroExtend (pack b)
      p11 = p10 { pPoly9 = q9, pPoly17 = q17, pPoly4 = q4, pPoly5 = q5
                , pHist4 = push (pHist4 p10) b4, pHist5 = push (pHist5 p10) b5, pHist917 = push (pHist917 p10) b917 }
  in if p15 && testBit skctl 1 then keyScan skctl p11 else p11
 where
  satInc a = if a == maxBound then a else a + 1
  pulse :: (KnownNat n) => Unsigned n -> Unsigned n -> Bool -> (Bool, Unsigned n)
  pulse n period running
    | not running = (False, n)
    | n <= 1 = (True, period)
    | otherwise = (False, n - 1)

-- | What fires: the channels' audio, IRQST, two-tone, the serial clock.
fired :: BitVector 4 -> Unsigned 8 -> Pokey -> Pokey
fired fire skctl p0 =
  let audctl = pAudctl p0
      chan q ch =
        if not (testBit fire (fromIntegral ch)) then q else
          let c = pAudc q !! ch
              poly5 = testBit (pHist5 q) (fromIntegral ch)
              noise = if testBit c 6 then testBit (pHist4 q) (fromIntegral ch) else testBit (pHist917 q) (fromIntegral ch)
              o = pOut q !! ch
              o' | testBit c 7 || poly5 = if testBit c 5 then complement o else (if noise then 1 else 0)
                 | otherwise = o
              q1 = q { pOut = replace ch o' (pOut q) }
              q2 = if ch == 2 && testBit audctl 2 then q1 { pHp1 = pOut q1 !! (0 :: Index 4) } else q1
              q3 = if ch == 3 && testBit audctl 1 then q2 { pHp2 = pOut q2 !! (1 :: Index 4) } else q2
          in q3
      p1 = foldl chan p0 (indicesI :: Vec 4 (Index 4))
      en = pIrqen p1
      irqBits = (if testBit fire 0 && testBit en 0 then 1 else 0)
            .|. (if testBit fire 1 && testBit en 1 then 2 else 0)
            .|. (if testBit fire 3 && testBit en 2 then 4 else 0)
      p2 = p1 { pIrqst = pIrqst p1 .&. complement irqBits }
      two = testBit skctl 3 && ((testBit fire 0 && pSerBit p2 == 1 && not (testBit skctl 7)) || testBit fire 1)
      p3 = if two then p2 { pTwoTone = True } else p2
      mode = skctl .&. 0x60
      outClock = (testBit fire 3 && (mode == 0x20 || mode == 0x40)) || (testBit fire 1 && mode == 0x60)
      ph = not (pSerPhase p3)
  in if not outClock then p3 else
       p3 { pSerPhase = ph, pSerTickIn = if ph && pSerCount p3 > 0 then 2 else pSerTickIn p3 }

-- | The serial output shifter moves one bit cell on.
serialStep :: Pokey -> Pokey
serialStep p0 =
  let n = pSerCount p0
      p1 | n > 1 = p0 { pSerCount = n - 1 }
         | otherwise =
             let p' = p0 { pSerCount = 0 }
             in if pSerValid p'
                  then p' { pSerShift = pSerout p', pSerValid = False, pSerCount = 10, pLoaded = True
                          , pIrqst = (pIrqst p' .|. 0x08) .&. (if testBit (pIrqen p') 4 then complement 0x10 else 0xFF) }
                  else p' { pIrqst = pIrqst p' .&. complement 0x08 }
      c = pSerCount p1
      b | c > 9 = 0
        | c > 1 = lsb (pSerShift p1 `shiftR` fromIntegral (9 - c))
        | otherwise = 1
  in p1 { pSerBit = b }

-- | One key of the keyboard scan (the spec's state machine).
keyScan :: Unsigned 8 -> Pokey -> Pokey
keyScan skctl p0 =
  let kc = pKeyScan p0
      p1 = p0 { pKeyScan = kc + 1 }
      mods = pMods p1
      lat0 = pModLatch p1
      lat1 = if kc == 0 then replaceBit (1 :: Int) (mods ! (1 :: Int)) lat0 else lat0
      (lat2, sk1) = if kc == 0x10
                      then (replaceBit (0 :: Int) (mods ! (0 :: Int)) lat1
                           , if testBit mods 0 then pSkstat p1 .&. complement 0x08 else pSkstat p1 .|. 0x08)
                      else (lat1, pSkstat p1)
      breakEdge = kc == 0x30 && testBit mods 2 && not (testBit lat2 2)
      lat3 = if kc == 0x30 then replaceBit (2 :: Int) (mods ! (2 :: Int)) lat2 else lat2
      irq1 = if breakEdge && testBit (pIrqen p1) 7 then pIrqst p1 .&. complement 0x80 else pIrqst p1
      p2 = p1 { pModLatch = lat3, pSkstat = sk1, pIrqst = irq1 }
      held = testBit (pKeys p2) (fromIntegral kc)
      same = kc == pKeyLatch p2 || not (testBit skctl 0)
  in case pKeyState p2 of
       0 -> if held then p2 { pKeyLatch = kc, pKeyState = 1 } else p2
       1 | held && same ->
             let code = zeroExtend kc .|. (if testBit lat3 0 then 0x40 else 0) .|. (if testBit lat3 1 then 0x80 else 0)
                 p3 = p2 { pKeyState = 2, pKbcode = code, pSkstat = pSkstat p2 .&. complement 0x04 }
             in if not (testBit (pIrqen p3) 6) then p3
                else if not (testBit (pIrqst p3) 6) then p3 { pSkstat = pSkstat p3 .&. complement 0x40 }
                else p3 { pIrqst = pIrqst p3 .&. complement 0x40 }
         | held -> p2 { pKeyState = 0 }
         | same -> p2 { pKeyState = 0 }
         | otherwise -> p2
       2 -> if same && not held then p2 { pKeyState = 3 } else p2
       _ -> if not same then p2
            else if held then p2 { pKeyState = 2 }
            else p2 { pSkstat = pSkstat p2 .|. 0x04, pKeyState = 0 }

step9 :: BitVector 9 -> BitVector 9
step9 q = (q `shiftR` 1) .|. (zeroExtend (pack (complement (q ! (0 :: Int) `xor` q ! (5 :: Int)))) `shiftL` 8)

step17 :: BitVector 17 -> BitVector 17
step17 q = (q `shiftR` 1) .|. (zeroExtend (pack (complement (q ! (0 :: Int) `xor` q ! (5 :: Int)))) `shiftL` 16)

step4 :: BitVector 4 -> BitVector 4
step4 q = (q `shiftR` 1) .|. (zeroExtend (pack (complement (q ! (1 :: Int) `xor` q ! (0 :: Int)))) `shiftL` 3)

step5 :: BitVector 5 -> BitVector 5
step5 q = (q `shiftR` 1) .|. (zeroExtend (pack (complement (q ! (2 :: Int) `xor` q ! (0 :: Int)))) `shiftL` 4)

readReg :: Pokey -> Unsigned 4 -> Unsigned 8
readReg p r
  | r < 8 = 228
  | r == 8 = 0
  | r == 9 = pKbcode p
  | r == 0x0A =
      let v | testBit (pAudctl p) 7 = unpack (truncateB (pPoly9 p))
            | otherwise = unpack (truncateB (pPoly17 p `shiftR` 8))
          rnd = complement v :: Unsigned 8
          age = pInitAge p
          force = truncateB ((0xFFE00 :: Unsigned 20) `shiftR` fromIntegral age) :: Unsigned 8
      in if initMode (pSkctl p)
           then (if age > 10 then 0xFF else rnd .|. force)
           else rnd
  | r == 0x0D = pSerin p
  | r == 0x0E = pIrqst p
  | r == 0x0F = pSkstat p
  | otherwise = 0xFF

writeReg :: Pokey -> Unsigned 4 -> Unsigned 8 -> Pokey
writeReg p r v
  | r < 8 = let ch = fromIntegral (r `shiftR` 1) :: Index 4
            in if even r then p { pAudf = replace ch v (pAudf p) } else p { pAudc = replace ch v (pAudc p) }
  | r == 8 = p { pAudctl = v }
  | r == 9 = p { pStimerIn = 3, pOut = repeat 1, pRel = map (\x -> if x == 3 then 0 else x) (pRel p) }
  | r == 0x0A = p { pSkstat = pSkstat p .|. 0xE0 }
  | r == 0x0D = p { pSerout = v, pSerValid = True, pSerCount = if pSerCount p == 0 then 1 else pSerCount p }
  | r == 0x0E = p { pIrqen = v, pIrqst = pIrqst p .|. (complement v .&. 0xF7) }
  | r == 0x0F =
      let wasInit = initMode (pSkctl p)
          isInit = initMode v
          p1 = p { pSkctl = v }
          p2 = if isInit && not wasInit then p1 { pInitAge = 0, pSerCount = 0, pSerValid = False, pSerTickIn = 0 } else p1
          p3 = if wasInit && not isInit then p2 { pPhase64 = 21, pPhase15 = 80, pPoly9 = 0, pPoly17 = 0 } else p2
      in if v .&. 0x70 == 0 then p3 { pSerPhase = False } else p3
  | otherwise = p

-- | The channels mixed ('A800.Spec.Pokey.pokeyLevel').
pokeyLevelOf :: Pokey -> Unsigned 10
pokeyLevelOf p = sum (imap level (zip (pAudc p) (pOut p)))
 where
  audctl = pAudctl p
  level :: Index 4 -> (Unsigned 8, Bit) -> Unsigned 10
  level ch (c, o) =
    let hp | ch == 0 = if testBit audctl 2 then pHp1 p else 1
           | ch == 1 = if testBit audctl 1 then pHp2 p else 1
           | otherwise = 0
        on = testBit c 4 || (o `xor` hp) == 1
    in if on then weight (c .&. 15) else 0
  weight :: Unsigned 8 -> Unsigned 10
  weight vol = (if testBit vol 0 then 12 else 0) + (if testBit vol 1 then 26 else 0)
             + (if testBit vol 2 then 56 else 0) + (if testBit vol 3 then 112 else 0)
