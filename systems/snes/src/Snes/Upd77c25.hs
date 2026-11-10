{-# LANGUAGE StrictData #-}
{-|
The NEC µPD77C25 in Clash (M7 step 7.16c; docs/systems/snes.md, "The
DSP-1"): the DSP-1's chip, following the reference model
("Snes.Spec.Upd77c25") instruction for instruction. Its lockstep test
(snes-spec's "the uPD77C25 in Clash") runs both on our own programs and on
the user's firmware.

__A clock__ ('updCycle'): the memories are outside, read at the addresses
the state gives ('updReq': the program ROM at PC, the data ROM at RP, RAM
at DP and at DP | 40h), so their bytes arrive a clock later. An
instruction falls due on 'uiGo' (the chip's 7.6 MHz against the master
clock, the caller's); it runs in a clock where the memories' bytes are
the state's ('ucReady'), so two clocks at least, the next one the clock
after (its reads under way). An instruction due while not ready waits
('ucOwed'). M and N are K × L × 2 of the clock before, ready by the next
instruction.

__The SNES's side__ ('uiSnes'): a read of DR or SR, or a write of DR, in
the same clock as an instruction comes after it (and sees it).

RAM's writes ('uoRam', port A) are the instruction's MEM; port B only
reads (KLM's K).
-}
module Snes.Upd77c25
  ( UFlags (..)
  , uflags0
  , Upd (..)
  , UpdCore (..)
  , SnesAcc (..)
  , UpdIn (..)
  , UpdOut (..)
  , updPowerOn
  , updCorePowerOn
  , updReq
  , updCycle
  , ReqAddrs (..)
  ) where

import Clash.Prelude

-- | An accumulator's flags (as the spec's 'Snes.Spec.Upd77c25.Flags').
data UFlags = UFlags
  { ufS0 :: Bool, ufS1 :: Bool, ufC :: Bool, ufZ :: Bool, ufOv0 :: Bool, ufOv1 :: Bool
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (NFDataX, BitPack)

uflags0 :: UFlags
uflags0 = UFlags False False False False False False

-- | The chip's registers (as the spec's 'Snes.Spec.Upd77c25.Upd', RAM
-- outside).
data Upd = Upd
  { upPc :: Unsigned 11
  , upStack :: Vec 4 (Unsigned 11)
  , upDp :: Unsigned 8
  , upRp :: Unsigned 10
  , upK :: Unsigned 16
  , upL :: Unsigned 16
  , upM :: Unsigned 16
  , upN :: Unsigned 16
  , upA :: Unsigned 16
  , upB :: Unsigned 16
  , upFa :: UFlags
  , upFb :: UFlags
  , upTr :: Unsigned 16
  , upTrb :: Unsigned 16
  , upSr :: Unsigned 16
  , upDr :: Unsigned 16
  , upSo :: Unsigned 16
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (NFDataX, BitPack)

-- | The registers and the clock's bookkeeping.
data UpdCore = UpdCore
  { ucRegs :: Upd
  , ucReady :: Bool       -- ^ the memories' bytes are this state's
  , ucOwed :: Unsigned 2  -- ^ instructions due, not yet run
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (NFDataX, BitPack)

-- | At power-on (and reset: RP 3FFh).
updPowerOn :: Upd
updPowerOn = Upd 0 (repeat 0) 0 0x3FF 0 0 0 0 0 0 uflags0 uflags0 0 0 0 0 0

updCorePowerOn :: UpdCore
updCorePowerOn = UpdCore updPowerOn False 0

-- | An access of the SNES's.
data SnesAcc = SReadDr | SReadSr | SWriteDr (Unsigned 8)
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFDataX, BitPack)

data UpdIn = UpdIn
  { uiGo :: Bool                -- ^ an instruction falls due
  , uiOp :: BitVector 24        -- ^ the program ROM at last clock's PC
  , uiRo :: Unsigned 16         -- ^ the data ROM at last clock's RP
  , uiMem :: Unsigned 16        -- ^ RAM at last clock's DP
  , uiMemK :: Unsigned 16       -- ^ RAM at last clock's DP | 40h
  , uiSnes :: Maybe SnesAcc
  , uiReset :: Bool
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (NFDataX, BitPack)

-- | The addresses the memories read at (from the state).
data ReqAddrs = ReqAddrs
  { raPc :: Unsigned 11, raRp :: Unsigned 10, raDp :: Unsigned 8, raDpK :: Unsigned 8 }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFDataX, BitPack)

data UpdOut = UpdOut
  { uoRam :: Maybe (Unsigned 8, Unsigned 16)  -- ^ RAM's write (port A)
  , uoByte :: Unsigned 8                      -- ^ the SNES's read's byte
  , uoStep :: Bool                            -- ^ an instruction ran
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (NFDataX, BitPack)

updReq :: UpdCore -> ReqAddrs
updReq c = let u = ucRegs c in ReqAddrs (upPc u) (upRp u) (upDp u) (upDp u .|. 0x40)

bRqm, bDrs, bDrc :: Index 16
bRqm = 15
bDrs = 12
bDrc = 10

bitI :: Unsigned 16 -> Index 16 -> Bool
bitI v i = testBit v (fromIntegral i)


-- | A clock.
updCycle :: UpdCore -> UpdIn -> (UpdCore, UpdOut)
updCycle c i
  | uiReset i = (c { ucRegs = resetR (ucRegs c), ucReady = False, ucOwed = 0 }, UpdOut Nothing 0 False)
  | otherwise = (UpdCore regs' ready' owed'', UpdOut ramW byte run)
 where
  owed' = ucOwed c + (if uiGo i then 1 else 0)
  run = ucReady c && owed' > 0
  (u1, ramW) = if run then exec i (ucRegs c) else (ucRegs c, Nothing)
  (byte, u2) = snesAcc (uiSnes i) u1
  -- M and N of this clock's K and L (ready by the next instruction)
  p = (resize (unpack (pack (upK u2)) :: Signed 16) :: Signed 32) * resize (unpack (pack (upL u2)) :: Signed 16) * 2
  regs' = u2 { upM = unpack (slice d31 d16 p), upN = unpack (slice d15 d0 p) }
  ready' = not run
  owed'' = if run then owed' - 1 else owed'

resetR :: Upd -> Upd
resetR u = u { upPc = 0, upFa = uflags0, upFb = uflags0, upSr = 0, upRp = 0x3FF }

-- | The SNES's access (after this clock's instruction): the byte read and
-- the state after. (SR's bits by constant masks, not 'clrB'/'setB': with
-- @clrB bRqm (clrB bDrs sr)@ beside the DRC branch's @clrB bRqm sr@, Clash
-- 1.8.5's disjoint expression consolidation shared one @clrB@ whose bit it
-- chose by DRS, so the high byte's access cleared RQM twice and never DRS:
-- Haskell's simulation right, the Verilog wrong; step 7.16d.)
snesAcc :: Maybe SnesAcc -> Upd -> (Unsigned 8, Upd)
snesAcc acc u = case acc of
  Just SReadSr -> (resize (upSr u `shiftR` 8), u)
  Just SReadDr
    | bitI sr bDrc -> (lo, u { upSr = sr .&. 0x7FFF })
    | not (bitI sr bDrs) -> (lo, u { upSr = sr .|. 0x1000 })
    | otherwise -> (hi, u { upSr = sr .&. 0x6FFF })
  Just (SWriteDr v)
    | bitI sr bDrc -> (0, u { upDr = setLo v, upSr = sr .&. 0x7FFF })
    | not (bitI sr bDrs) -> (0, u { upDr = setLo v, upSr = sr .|. 0x1000 })
    | otherwise -> (0, u { upDr = (upDr u .&. 0x00FF) .|. (resize v `shiftL` 8), upSr = sr .&. 0x6FFF })
  Nothing -> (0, u)
 where
  sr = upSr u
  lo = resize (upDr u)
  hi = resize (upDr u `shiftR` 8)
  setLo v = (upDr u .&. 0xFF00) .|. resize v

-- | An instruction (the spec's 'Snes.Spec.Upd77c25.step').
exec :: UpdIn -> Upd -> (Upd, Maybe (Unsigned 8, Unsigned 16))
exec i u0 = case slice d23 d22 op of
  2 -> (jump op u, Nothing)
  3 -> store i (unpack (slice d21 d6 op)) (unpack (slice d3 d0 op)) u
  _ -> alu i op u
 where
  op = uiOp i
  u = u0 { upPc = upPc u0 + 1 }

-- | The SRC field's value (DR's read sets RQM).
source :: UpdIn -> Unsigned 4 -> Upd -> (Unsigned 16, Upd)
source i s u = case s of
  0 -> (upTrb u, u)
  1 -> (upA u, u)
  2 -> (upB u, u)
  3 -> (upTr u, u)
  4 -> (resize (upDp u), u)
  5 -> (resize (upRp u), u)
  6 -> (uiRo i, u)
  7 -> (if ufS1 (upFa u) then 0x7FFF else 0x8000, u)
  8 -> (upDr u, u { upSr = upSr u .|. 0x8000 })
  9 -> (upDr u, u)
  10 -> (upSr u, u)
  13 -> (upK u, u)
  14 -> (upL u, u)
  15 -> (uiMem i, u)
  _ -> (0, u)

-- | The DST field written (ALU and LD); RAM's write is the result's.
store :: UpdIn -> Unsigned 16 -> Unsigned 4 -> Upd -> (Upd, Maybe (Unsigned 8, Unsigned 16))
store i v d u = case d of
  1 -> (u { upA = v }, Nothing)
  2 -> (u { upB = v }, Nothing)
  3 -> (u { upTr = v }, Nothing)
  4 -> (u { upDp = resize v }, Nothing)
  5 -> (u { upRp = resize v }, Nothing)
  6 -> (u { upDr = v, upSr = upSr u .|. 0x8000 }, Nothing)
  7 -> (u { upSr = (upSr u .&. 0x907C) .|. (v .&. complement 0x907C) }, Nothing)
  8 -> (u { upSo = v }, Nothing)
  9 -> (u { upSo = v }, Nothing)
  10 -> (u { upK = v }, Nothing)
  11 -> (u { upK = v, upL = uiRo i }, Nothing)
  12 -> (u { upL = v, upK = uiMemK i }, Nothing)
  13 -> (u { upL = v }, Nothing)
  14 -> (u { upTrb = v }, Nothing)
  15 -> (u, Just (upDp u, v))
  _ -> (u, Nothing)

-- | An ALU instruction: SRC, the ALU, DST, the pointers, the return.
alu :: UpdIn -> BitVector 24 -> Upd -> (Upd, Maybe (Unsigned 8, Unsigned 16))
alu i op u0 = (if rt then pop u4 else u4, ramW)
 where
  rt = testBit op 22
  pSel = slice d21 d20 op
  code = unpack (slice d19 d16 op) :: Unsigned 4
  useB = testBit op 15
  dpl = slice d14 d13 op
  dph = unpack (slice d12 d9 op) :: Unsigned 4
  rpDec = testBit op 8
  (idb, u1) = source i (unpack (slice d7 d4 op)) u0
  p = case pSel of
    0 -> uiMem i
    1 -> idb
    2 -> upM u0
    _ -> upN u0
  u2 | code == 0 = u1
     | useB = let (r, f) = aluOp code (upB u1) p (ufC (upFa u1)) (upFb u1) in u1 { upB = r, upFb = f }
     | otherwise = let (r, f) = aluOp code (upA u1) p (ufC (upFb u1)) (upFa u1) in u1 { upA = r, upFa = f }
  (u3, ramW) = store i idb (unpack (slice d3 d0 op)) u2
  d = upDp u3
  lo = case dpl of
    1 -> (d + 1) .&. 0x0F
    2 -> (d - 1) .&. 0x0F
    3 -> 0
    _ -> d .&. 0x0F
  u4 = u3 { upDp = ((d .&. 0xF0) `xor` (resize dph `shiftL` 4)) .|. lo
          , upRp = if rpDec then upRp u3 - 1 else upRp u3 }

-- | The ALU (the spec's 'Snes.Spec.Upd77c25.aluOp'): 17-bit sums and
-- differences, a signed overflow from the operands' and the result's
-- signs.
aluOp :: Unsigned 4 -> Unsigned 16 -> Unsigned 16 -> Bool -> UFlags -> (Unsigned 16, UFlags)
aluOp code q p oc fl = case code of
  1 -> logic (q .|. p) False
  2 -> logic (q .&. p) False
  3 -> logic (q `xor` p) False
  4 -> subQ p 0
  5 -> addQ p 0
  6 -> subQ p ci
  7 -> addQ p ci
  8 -> subQ 1 0
  9 -> addQ 1 0
  10 -> logic (complement q) False
  11 -> logic ((q `shiftR` 1) .|. (q .&. 0x8000)) (testBit q 0)
  12 -> logic ((q `shiftL` 1) .|. resize ci) (testBit q 15)
  13 -> logic ((q `shiftL` 2) .|. 3) False
  14 -> logic ((q `shiftL` 4) .|. 15) False
  15 -> logic (rotateL q 8) False
  _ -> (q, fl)
 where
  ci = if oc then 1 else 0 :: Unsigned 17
  logic r cy = let s = testBit r 15 in (r, UFlags s s cy (r == 0) False False)
  addQ x c = let s = resize q + resize x + c :: Unsigned 17
                 r = resize s
             in arith r (testBit s 16) (testBit q 15 == testBit x 15 && testBit r 15 /= testBit q 15)
  subQ x c = let s = resize q - resize x - c :: Unsigned 17
                 r = resize s
             in arith r (testBit s 16) (testBit q 15 /= testBit x 15 && testBit r 15 /= testBit q 15)
  arith r cy ov =
    let sgn = testBit r 15
    in (r, UFlags sgn (if ov then sgn else ufS1 fl) cy (r == 0) ov (if ov then not (ufOv1 fl) else ufOv1 fl))

-- | A jump or call (the spec's BRCH table; an unknown one doesn't jump).
jump :: BitVector 24 -> Upd -> Upd
jump op u = case brch of
  0x140 -> (push u) { upPc = na }
  0x100 -> u { upPc = na }
  _ | slice d8 d6 brch == 0b010 && cond -> u { upPc = na }
    | otherwise -> u
 where
  brch = unpack (slice d21 d13 op) :: Unsigned 9
  na = unpack (slice d12 d2 op) :: Unsigned 11
  fa = upFa u
  fb = upFb u
  dl = upDp u .&. 0x0F
  -- 080h-0AEh: a flag (bits 4-2), the accumulator (bit 2 of the pair:
  -- bit 2), the sense (bit 1); 0B0h-0B3h DP's low bits; 0B4h-0BAh the
  -- serial ACKs (never set); 0BCh, 0BEh RQM
  sel = slice d5 d0 brch
  flagOf f = case slice d5 d3 sel of
    0 -> ufC f
    1 -> ufZ f
    2 -> ufOv0 f
    3 -> ufOv1 f
    4 -> ufS0 f
    _ -> ufS1 f
  cond = case sel of
    0x30 -> dl == 0
    0x31 -> dl /= 0
    0x32 -> dl == 0x0F
    0x33 -> dl /= 0x0F
    0x34 -> True   -- JNSIAK
    0x36 -> False  -- JSIAK
    0x38 -> True   -- JNSOAK
    0x3A -> False  -- JSOAK
    0x3C -> not (bitI (upSr u) bRqm)
    0x3E -> bitI (upSr u) bRqm
    _ | sel < 0x30 && not (testBit sel 0) ->
          let f = if testBit sel 2 then fb else fa
          in flagOf f == testBit sel 1
      | otherwise -> False

-- | The stack: a push shifts the levels down.
push :: Upd -> Upd
push u = u { upStack = upPc u +>> upStack u }

-- | A return: PC from the top, the levels up (the bottom kept).
pop :: Upd -> Upd
pop u = u { upPc = head s, upStack = tail s :< last s }
 where
  s = upStack u
