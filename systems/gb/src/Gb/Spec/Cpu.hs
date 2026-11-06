{-# LANGUAGE MultiWayIf #-}
{-|
The SM83 CPU of the reference model (PLAN.md step 2.1): all 256 + 256
(CB) opcodes, each memory access in its own M-cycle in hardware order
(Pan Docs opcode tables; Gekkio, "Game Boy: Complete Technical
Reference"), interrupts, HALT and the HALT bug.

__Instruction boundary.__ 'step' starts with the opcode fetch, so an
instruction's M-cycles are its fetch followed by the rest. (On the chip the
fetch of the next opcode overlaps the last M-cycle of the current
instruction; the bus sequence is the same, only the boundary is drawn one
M-cycle earlier. The Clash core must draw it in the same place, step 2.4.)

__One 'step'__ is one of: an instruction (fetch included), an interrupt
dispatch (5 M-cycles), or one M-cycle of HALT. Before the fetch:

1. halted: if @IE & IF & 0x1F@ is zero, one idle M-cycle and done;
   otherwise leave HALT and go on;
2. IME set and an interrupt pending: dispatch (idle, idle, push PC high
   byte, choose the vector, push PC low byte, jump), clear IME and the
   IF bit; the vector is chosen after the high byte is pushed, so a push
   that overwrites IE (SP = 0) changes or cancels it (Mooneye @ie_push@;
   cancelled: PC = 0);
3. a pending EI takes effect (so the instruction after EI runs before any
   interrupt);
4. fetch and execute; after a HALT bug the fetch doesn't advance PC.

__HALT__ with IME = 0 and an interrupt pending doesn't halt and causes
the HALT bug. With EI just before it (IME became 1 in this step) and an
interrupt pending, PC is left on the HALT, so the interrupt returns to it
(Pan Docs, "halt").

__Illegal opcodes__ (D3 DB DD E3 E4 EB EC ED F4 FC FD) lock the CPU: every
later step is an idle M-cycle, interrupts included. __STOP__ (10) is
treated as a 2-byte NOP; real STOP behaviour is not modelled.
-}
module Gb.Spec.Cpu
  ( step
  ) where

import Prelude

import Control.Monad (when)
import Control.Monad.ST (ST)
import Data.Bits
import Data.Int (Int8)
import Data.Word

import Gb.Spec.Machine

fZ, fN, fH, fC :: Word8
fZ = 0x80
fN = 0x40
fH = 0x20
fC = 0x10

-- * Small helpers

getPC, getSP :: Machine s -> ST s Word16
getPC m = fromIntegral <$> getSt m iPC
getSP m = fromIntegral <$> getSt m iSP
{-# INLINE getPC #-}
{-# INLINE getSP #-}

setPC, setSP :: Machine s -> Word16 -> ST s ()
setPC m = setSt m iPC . fromIntegral
setSP m = setSt m iSP . fromIntegral
{-# INLINE setPC #-}
{-# INLINE setSP #-}

fetch :: Machine s -> ST s Word8
fetch m = do
  pc <- getPC m
  setPC m (pc + 1)
  busRead m pc
{-# INLINE fetch #-}

fetch16 :: Machine s -> ST s Word16
fetch16 m = do
  lo <- fetch m
  hi <- fetch m
  pure (w16 hi lo)
{-# INLINE fetch16 #-}

w16 :: Word8 -> Word8 -> Word16
w16 hi lo = fromIntegral hi `shiftL` 8 .|. fromIntegral lo
{-# INLINE w16 #-}

hiB, loB :: Word16 -> Word8
hiB x = fromIntegral (x `shiftR` 8)
loB = fromIntegral
{-# INLINE hiB #-}
{-# INLINE loB #-}

getPair :: Machine s -> Int -> Int -> ST s Word16
getPair m h l = w16 <$> getR m h <*> getR m l
{-# INLINE getPair #-}

setPair :: Machine s -> Int -> Int -> Word16 -> ST s ()
setPair m h l v = setR m h (hiB v) >> setR m l (loB v)
{-# INLINE setPair #-}

-- | rp: BC, DE, HL, SP.
getRp :: Machine s -> Int -> ST s Word16
getRp m p = case p of
  0 -> getPair m rB rC
  1 -> getPair m rD rE
  2 -> getPair m rH rL
  _ -> getSP m

setRp :: Machine s -> Int -> Word16 -> ST s ()
setRp m p v = case p of
  0 -> setPair m rB rC v
  1 -> setPair m rD rE v
  2 -> setPair m rH rL v
  _ -> setSP m v

-- | rp2: BC, DE, HL, AF (PUSH/POP).
getRp2 :: Machine s -> Int -> ST s Word16
getRp2 m 3 = getPair m rA rF
getRp2 m p = getRp m p

setRp2 :: Machine s -> Int -> Word16 -> ST s ()
setRp2 m 3 v = setR m rA (hiB v) >> setR m rF (loB v .&. 0xF0)
setRp2 m p v = setRp m p v

getHL :: Machine s -> ST s Word16
getHL m = getPair m rH rL

-- | Operand r (6 = (HL), one M-cycle).
getOp :: Machine s -> Int -> ST s Word8
getOp m 6 = getHL m >>= busRead m
getOp m r = getR m r
{-# INLINE getOp #-}

setOp :: Machine s -> Int -> Word8 -> ST s ()
setOp m 6 v = getHL m >>= \a -> busWrite m a v
setOp m r v = setR m r v
{-# INLINE setOp #-}

getF :: Machine s -> ST s Word8
getF m = getR m rF

setF :: Machine s -> Word8 -> ST s ()
setF m v = setR m rF (v .&. 0xF0)

flag :: Bool -> Word8 -> Word8
flag b f = if b then f else 0
{-# INLINE flag #-}

-- | Conditions NZ, Z, NC, C.
cond :: Word8 -> Int -> Bool
cond f c = case c of
  0 -> f .&. fZ == 0
  1 -> f .&. fZ /= 0
  2 -> f .&. fC == 0
  _ -> f .&. fC /= 0

push16 :: Machine s -> Word16 -> ST s ()
push16 m v = do
  sp <- getSP m
  busWrite m (sp - 1) (hiB v)
  busWrite m (sp - 2) (loB v)
  setSP m (sp - 2)

pop16 :: Machine s -> ST s Word16
pop16 m = do
  sp <- getSP m
  lo <- busRead m sp
  hi <- busRead m (sp + 1)
  setSP m (sp + 2)
  pure (w16 hi lo)

-- * Step

step :: Machine s -> ST s ()
step m = step1 m >> hdmaPending m

-- | 'step' without the HBlank DMA block due after it.
step1 :: Machine s -> ST s ()
step1 m = do
  locked <- getSt m iLOCK
  if locked /= 0 then busIdle m else do
    halted <- getSt m iHALT
    pc <- getPC m
    op <- busRead m pc
    p <- pendingInts m
    if halted /= 0 && p == 0 then pure () else do
      setSt m iHALT 0
      ime <- getSt m iIME
      if ime /= 0 && p /= 0 then dispatch m else do
        ei <- getSt m iEI
        when (ei /= 0) $ setSt m iEI 0 >> setSt m iIME 1
        bug <- getSt m iHALTBUG
        if bug /= 0 then setSt m iHALTBUG 0 else setPC m (pc + 1)
        exec m (ei /= 0) op

pendingInts :: Machine s -> ST s Int
pendingInts m = do
  ie <- getSt m iIE
  iflag <- getSt m iIF
  pure (ie .&. iflag .&. 0x1F)
{-# INLINE pendingInts #-}

-- | Interrupt dispatch: the M-cycles after the discarded opcode fetch
-- (5 M-cycles in all, the fetch included).
dispatch :: Machine s -> ST s ()
dispatch m = do
  setSt m iIME 0
  busIdle m
  pc <- getPC m
  sp <- getSP m
  busWrite m (sp - 1) (hiB pc)
  -- the vector is chosen, and its IF bit cleared, after the high byte
  pend <- pendingInts m
  let n = countTrailingZeros pend
  when (pend /= 0) $ getSt m iIF >>= setSt m iIF . (`clearBit` n)
  busWrite m (sp - 2) (loB pc)
  setSP m (sp - 2)
  setPC m (if pend == 0 then 0 else 0x40 + 8 * fromIntegral n)
  busIdle m

-- * Execute

exec :: Machine s -> Bool -> Word8 -> ST s ()
exec m eiNow op = case x of
  0 -> x0
  1 | op == 0x76 -> halt
    | otherwise -> do
        v <- getOp m z
        setOp m y v
        when (op == 0x40) $ getSt m iBREAK >>= setSt m iBREAK . (+ 1)
  2 -> getOp m z >>= alu m y
  _ -> x3
 where
  o = fromIntegral op :: Int
  x = o `shiftR` 6
  y = (o `shiftR` 3) .&. 7
  z = o .&. 7
  p = y `shiftR` 1
  q = y .&. 1

  halt = do
    ie <- getSt m iIE
    iflag <- getSt m iIF
    ime <- getSt m iIME
    let pending = ie .&. iflag .&. 0x1F /= 0
    if | ime /= 0 && eiNow && pending -> getPC m >>= setPC m . subtract 1
       | ime /= 0 -> setSt m iHALT 1
       | pending -> setSt m iHALTBUG 1
       | otherwise -> setSt m iHALT 1

  x0 = case z of
    0 -> case y of
      0 -> pure ()                                       -- NOP
      1 -> do                                            -- LD (nn),SP
        a <- fetch16 m
        sp <- getSP m
        busWrite m a (loB sp)
        busWrite m (a + 1) (hiB sp)
      2 -> fetch m >> stopInstr m                        -- STOP: the CGB's speed switch, else a 2-byte NOP
      3 -> jr True                                       -- JR e
      _ -> getF m >>= \f -> jr (cond f (y - 4))          -- JR cc,e
    1 | q == 0 -> fetch16 m >>= setRp m p                -- LD rp,nn
      | otherwise -> do                                  -- ADD HL,rp
          hl <- getHL m
          v <- getRp m p
          f <- getF m
          let r = fromIntegral hl + fromIntegral v :: Int
          busIdle m
          setPair m rH rL (fromIntegral r)
          setF m ((f .&. fZ) .|. flag ((hl .&. 0xFFF) + (v .&. 0xFFF) > 0xFFF) fH
                  .|. flag (r > 0xFFFF) fC)
    2 -> do                                              -- LD (rp),A / LD A,(rp)
      a <- case p of
        0 -> getPair m rB rC
        1 -> getPair m rD rE
        _ -> do
          hl <- getHL m
          setPair m rH rL (if p == 2 then hl + 1 else hl - 1)
          pure hl
      if q == 0 then getR m rA >>= busWrite m a else busRead m a >>= setR m rA
    3 -> do                                              -- INC rp / DEC rp
      v <- getRp m p
      busIdle m
      setRp m p (if q == 0 then v + 1 else v - 1)
    4 -> do                                              -- INC r
      v <- getOp m y
      f <- getF m
      let r = v + 1
      setOp m y r
      setF m (flag (r == 0) fZ .|. flag (v .&. 0xF == 0xF) fH .|. (f .&. fC))
    5 -> do                                              -- DEC r
      v <- getOp m y
      f <- getF m
      let r = v - 1
      setOp m y r
      setF m (flag (r == 0) fZ .|. fN .|. flag (v .&. 0xF == 0) fH .|. (f .&. fC))
    6 -> fetch m >>= setOp m y                           -- LD r,n
    _ -> do
      a <- getR m rA
      f <- getF m
      let c = f .&. fC /= 0
      case y of
        0 -> rotA (a `rotateL` 1) (testBit a 7)          -- RLCA
        1 -> rotA (a `rotateR` 1) (testBit a 0)          -- RRCA
        2 -> rotA (a `shiftL` 1 .|. flag c 1) (testBit a 7)   -- RLA
        3 -> rotA (a `shiftR` 1 .|. flag c 0x80) (testBit a 0) -- RRA
        4 -> do                                          -- DAA
          let n = f .&. fN /= 0
              h = f .&. fH /= 0
              (a', c') | not n = let (a1, c1) = if c || a > 0x99 then (a + 0x60, True) else (a, False)
                                     a2 = if h || a1 .&. 0x0F > 9 then a1 + 6 else a1
                                 in (a2, c1)
                       | otherwise = let a1 = if c then a - 0x60 else a
                                         a2 = if h then a1 - 6 else a1
                                     in (a2, c)
          setR m rA a'
          setF m (flag (a' == 0) fZ .|. (f .&. fN) .|. flag c' fC)
        5 -> setR m rA (complement a) >> setF m ((f .&. (fZ .|. fC)) .|. fN .|. fH) -- CPL
        6 -> setF m ((f .&. fZ) .|. fC)                                         -- SCF
        _ -> setF m ((f .&. fZ) .|. (if c then 0 else fC))                      -- CCF

  rotA r c = setR m rA r >> setF m (flag c fC)

  jr taken = do
    e <- fetch m
    when taken $ do
      busIdle m
      pc <- getPC m
      setPC m (pc + fromIntegral (fromIntegral e :: Int8))

  x3 = case z of
    0 -> case y of
      _ | y < 4 -> do                                    -- RET cc
            busIdle m
            f <- getF m
            when (cond f y) $ do
              a <- pop16 m
              busIdle m
              setPC m a
      4 -> do                                            -- LDH (n),A
        n <- fetch m
        getR m rA >>= busWrite m (0xFF00 + fromIntegral n)
      5 -> do                                            -- ADD SP,e
        (r, fl) <- spPlusE
        busIdle m
        busIdle m
        setSP m r
        setF m fl
      6 -> do                                            -- LDH A,(n)
        n <- fetch m
        busRead m (0xFF00 + fromIntegral n) >>= setR m rA
      _ -> do                                            -- LD HL,SP+e
        (r, fl) <- spPlusE
        busIdle m
        setPair m rH rL r
        setF m fl
    1 | q == 0 -> pop16 m >>= setRp2 m p                 -- POP rp2
      | otherwise -> case p of
          0 -> ret                                       -- RET
          1 -> ret >> setSt m iIME 1                     -- RETI
          2 -> getHL m >>= setPC m                       -- JP HL
          _ -> getHL m >>= \hl -> busIdle m >> setSP m hl -- LD SP,HL
    2 -> case y of
      _ | y < 4 -> do                                    -- JP cc,nn
            a <- fetch16 m
            f <- getF m
            when (cond f y) $ busIdle m >> setPC m a
      4 -> do                                            -- LD (FF00+C),A
        c <- getR m rC
        getR m rA >>= busWrite m (0xFF00 + fromIntegral c)
      5 -> do                                            -- LD (nn),A
        a <- fetch16 m
        getR m rA >>= busWrite m a
      6 -> do                                            -- LD A,(FF00+C)
        c <- getR m rC
        busRead m (0xFF00 + fromIntegral c) >>= setR m rA
      _ -> fetch16 m >>= busRead m >>= setR m rA         -- LD A,(nn)
    3 -> case y of
      0 -> do                                            -- JP nn
        a <- fetch16 m
        busIdle m
        setPC m a
      1 -> fetch m >>= cb m                              -- CB prefix
      6 -> setSt m iIME 0 >> setSt m iEI 0               -- DI
      7 -> setSt m iEI 1                                 -- EI
      _ -> lock
    4 | y < 4 -> do                                      -- CALL cc,nn
          a <- fetch16 m
          f <- getF m
          when (cond f y) (call a)
      | otherwise -> lock
    5 | q == 0 -> do                                     -- PUSH rp2
          v <- getRp2 m p
          busIdle m
          push16 m v
      | p == 0 -> fetch16 m >>= call                     -- CALL nn
      | otherwise -> lock
    6 -> fetch m >>= alu m y                             -- ALU A,n
    _ -> do                                              -- RST
      busIdle m
      getPC m >>= push16 m
      setPC m (fromIntegral (y * 8))

  ret = do
    a <- pop16 m
    busIdle m
    setPC m a

  call a = do
    busIdle m
    getPC m >>= push16 m
    setPC m a

  spPlusE = do
    e <- fetch m
    sp <- getSP m
    let r = sp + fromIntegral (fromIntegral e :: Int8)
        fl = flag ((sp .&. 0xF) + (fromIntegral e .&. 0xF) > 0xF) fH
         .|. flag ((sp .&. 0xFF) + fromIntegral e > 0xFF) fC
    pure (r, fl)

  lock = setSt m iLOCK 1

-- | The eight ALU operations on A: ADD ADC SUB SBC AND XOR OR CP.
alu :: Machine s -> Int -> Word8 -> ST s ()
alu m op b = do
  a <- getR m rA
  f <- getF m
  let c = if f .&. fC /= 0 then 1 else 0 :: Int
      ai = fromIntegral a :: Int
      bi = fromIntegral b :: Int
      lo x = x .&. 0xF
      sub cin = let r = ai - bi - cin
                in (fromIntegral r, fN .|. flag (lo ai - lo bi - cin < 0) fH .|. flag (r < 0) fC)
      add cin = let r = ai + bi + cin
                in (fromIntegral r, flag (lo ai + lo bi + cin > 0xF) fH .|. flag (r > 0xFF) fC)
      (res, fl) = case op of
        0 -> add 0
        1 -> add c
        2 -> sub 0
        3 -> sub c
        4 -> (a .&. b, fH)
        5 -> (a `xor` b, 0)
        6 -> (a .|. b, 0)
        _ -> sub 0
  when (op /= 7) (setR m rA res)
  setF m (flag (res == 0) fZ .|. fl)

-- | CB-prefixed opcodes.
cb :: Machine s -> Word8 -> ST s ()
cb m op = case x of
  0 -> do
    v <- getOp m z
    f <- getF m
    let c = f .&. fC /= 0
        (r, co) = case y of
          0 -> (v `rotateL` 1, testBit v 7)                  -- RLC
          1 -> (v `rotateR` 1, testBit v 0)                  -- RRC
          2 -> (v `shiftL` 1 .|. flag c 1, testBit v 7)      -- RL
          3 -> (v `shiftR` 1 .|. flag c 0x80, testBit v 0)   -- RR
          4 -> (v `shiftL` 1, testBit v 7)                   -- SLA
          5 -> (v `shiftR` 1 .|. (v .&. 0x80), testBit v 0)  -- SRA
          6 -> (v `rotateL` 4, False)                        -- SWAP
          _ -> (v `shiftR` 1, testBit v 0)                   -- SRL
    setOp m z r
    setF m (flag (r == 0) fZ .|. flag co fC)
  1 -> do                                                    -- BIT
    v <- getOp m z
    f <- getF m
    setF m (flag (not (testBit v y)) fZ .|. fH .|. (f .&. fC))
  2 -> getOp m z >>= setOp m z . (`clearBit` y)              -- RES
  _ -> getOp m z >>= setOp m z . (`setBit` y)                -- SET
 where
  o = fromIntegral op :: Int
  x = o `shiftR` 6
  y = (o `shiftR` 3) .&. 7
  z = o .&. 7
