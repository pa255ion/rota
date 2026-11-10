{-|
The NEC µPD77C25 reference model (M7 step 7.16a; docs/systems/snes.md,
"The DSP-1"): the DSP-1's chip, one instruction a step, pure.

Sources: fullsnes ("SNES Cart DSP-n/ST010/ST011 - NEC uPD77C25": the
registers, flags, SR, the opcode encodings, the ALU, LD and JP tables,
reset; local/reference/snes/upd77c25.txt), NEC's data sheet for what it
leaves open. bsnes's uPD96050 core was read for values only (the
multiplier's shift, SR's writable bits), never copied.

__The chip__: 2,048 × 24-bit program ROM, 1,024 × 16-bit data ROM (the
firmware, 'Firmware'), 256 × 16-bit RAM. Every instruction takes one
cycle: an ALU instruction (with a move from SRC to DST, the pointers'
adjustments and an optional return), a jump or call, or a load of an
immediate. After each one the multiplier has K × L × 2 (signed) ready as
M (its high word) and N (its low word).

__The SNES's side__ is the data register DR and the status register SR's
high byte ('snesReadDr', 'snesWriteDr', 'snesReadSr'). RQM (SR bit 15)
is set when the chip reads or writes DR and cleared when the SNES has
finished with it: one byte in 8-bit mode (DRC set), the low byte then the
high one in 16-bit mode (DRS says which comes next).

__Left open__ (none of the DSP-1's programs depend on them, by fullsnes):
the serial ports (SI reads 0, SO is kept, their ACKs never set), the
interrupt input (never raised on the SNES), the ST010's opcodes (JMPSO,
the second 8 K of jumps: no jump), and the stack past four levels (a pop
shifts the levels up, the last kept).
-}
module Snes.Spec.Upd77c25
  ( Firmware (..)
  , firmwareOf
  , Flags (..)
  , flags0
  , Upd (..)
  , powerOn
  , reset
  , step
  , snesReadDr
  , snesWriteDr
  , snesReadSr
  , rqm
  ) where

import Prelude

import Data.Bits
import qualified Data.ByteString as B
import qualified Data.Vector.Unboxed as U
import Data.Int (Int16, Int32)
import Data.Word

-- | The firmware: 2,048 instructions (24 bits each) and 1,024 data words.
data Firmware = Firmware
  { fwProgram :: !(U.Vector Word32)
  , fwData    :: !(U.Vector Word16)
  } deriving stock (Show, Eq)

-- | A firmware file (fullsnes, "ROM-Images"): 8 KiB, the program then the
-- data ROM, little-endian ("newer" dumps) or big-endian ("old"); or 10
-- KiB big-endian with a pad byte after each opcode ("oldest"). The order
-- is told by the "JRQM $" opcode (97C00xh) all real ones have among their
-- first four; without it, little-endian.
firmwareOf :: B.ByteString -> Either String Firmware
firmwareOf f
  | B.length f == 0x2800 = Right (build 4 True)
  | B.length f == 0x2000 = Right (build 3 bigEndian)
  | otherwise = Left ("a firmware file of " <> show (B.length f) <> " bytes, not 8 KiB or 10 KiB")
 where
  byte i = fromIntegral (B.index f i) :: Word32
  le i = byte i .|. byte (i + 1) `shiftL` 8 .|. byte (i + 2) `shiftL` 16
  be i = byte i `shiftL` 16 .|. byte (i + 1) `shiftL` 8 .|. byte (i + 2)
  isJrqm w = w .&. 0xFFFFF3 == 0x97C000
  bigEndian = not (any (\k -> isJrqm (le (3 * k))) [0 .. 3]) && any (\k -> isJrqm (be (3 * k))) [0 .. 3]
  build w big =
    let op i = (if big then be else le) (w * i)
        d i = let o = 2048 * w + 2 * i
                  lo = fromIntegral (B.index f o) :: Word16
                  hi = fromIntegral (B.index f (o + 1))
              in if big then lo `shiftL` 8 .|. hi else hi `shiftL` 8 .|. lo
    in Firmware (U.generate 2048 op) (U.generate 1024 d)

-- | An accumulator's flags.
data Flags = Flags
  { fS0 :: !Bool -- ^ the sign (bit 15)
  , fS1 :: !Bool -- ^ the last overflow's direction
  , fC  :: !Bool -- ^ carry or borrow
  , fZ  :: !Bool -- ^ zero
  , fOv0 :: !Bool -- ^ overflow (this operation)
  , fOv1 :: !Bool -- ^ an odd number of overflows
  } deriving stock (Show, Eq)

flags0 :: Flags
flags0 = Flags False False False False False False

-- | The chip's registers and RAM.
data Upd = Upd
  { uPc    :: !Word16 -- ^ 11 bits
  , uStack :: ![Word16] -- ^ four levels, the top first
  , uDp    :: !Word8
  , uRp    :: !Word16 -- ^ 10 bits
  , uK     :: !Word16
  , uL     :: !Word16
  , uM     :: !Word16 -- ^ K × L × 2, its high word
  , uN     :: !Word16 -- ^ its low word
  , uA     :: !Word16
  , uB     :: !Word16
  , uFa    :: !Flags
  , uFb    :: !Flags
  , uTr    :: !Word16
  , uTrb   :: !Word16
  , uSr    :: !Word16
  , uDr    :: !Word16
  , uSo    :: !Word16 -- ^ the serial output (kept, not connected)
  , uRam   :: !(U.Vector Word16)
  } deriving stock (Show, Eq)

-- | At power-on: every register 0 but RP (as after a reset) and RAM 0.
powerOn :: Upd
powerOn = reset Upd
  { uPc = 0, uStack = [0, 0, 0, 0], uDp = 0, uRp = 0, uK = 0, uL = 0, uM = 0, uN = 0
  , uA = 0, uB = 0, uFa = flags0, uFb = flags0, uTr = 0, uTrb = 0, uSr = 0, uDr = 0, uSo = 0
  , uRam = U.replicate 256 0 }

-- | The reset (fullsnes): PC 0, both accumulators' flags clear, SR 0, RP
-- 3FFh; every other register and RAM as they were.
reset :: Upd -> Upd
reset u = u { uPc = 0, uFa = flags0, uFb = flags0, uSr = 0, uRp = 0x3FF }

-- SR's bits
bRqm, bDrs, bDrc :: Int
bRqm = 15
bDrs = 12
bDrc = 10

-- | RQM: the chip waits for the SNES (it has read or written DR).
rqm :: Upd -> Bool
rqm u = testBit (uSr u) bRqm

-- | SR's bits the chip can't write: RQM, DRS and the unused 6-2.
srKept :: Word16
srKept = 0x907C

-- | The SNES reads DR: its low byte, or in 16-bit mode the low then the
-- high one; RQM clears when it's done.
snesReadDr :: Upd -> (Word8, Upd)
snesReadDr u
  | testBit (uSr u) bDrc = (lo, u { uSr = clearBit (uSr u) bRqm })
  | not (testBit (uSr u) bDrs) = (lo, u { uSr = setBit (uSr u) bDrs })
  | otherwise = (hi, u { uSr = clearBit (clearBit (uSr u) bDrs) bRqm })
 where
  lo = fromIntegral (uDr u)
  hi = fromIntegral (uDr u `shiftR` 8)

-- | The SNES writes DR: as 'snesReadDr', the byte into DR's low (or in
-- 16-bit mode its low then its high) byte.
snesWriteDr :: Word8 -> Upd -> Upd
snesWriteDr v u
  | testBit (uSr u) bDrc = u { uDr = setLo, uSr = clearBit (uSr u) bRqm }
  | not (testBit (uSr u) bDrs) = u { uDr = setLo, uSr = setBit (uSr u) bDrs }
  | otherwise = u { uDr = (uDr u .&. 0x00FF) .|. (fromIntegral v `shiftL` 8), uSr = clearBit (clearBit (uSr u) bDrs) bRqm }
 where
  setLo = (uDr u .&. 0xFF00) .|. fromIntegral v

-- | The SNES reads SR: its high byte (no side effect).
snesReadSr :: Upd -> Word8
snesReadSr u = fromIntegral (uSr u `shiftR` 8)

-- | One instruction.
step :: Firmware -> Upd -> Upd
step fw u0 = multiply $ case op `shiftR` 22 of
  2 -> jump fw u op
  3 -> store fw (fromIntegral (op `shiftR` 6)) (fromIntegral (op .&. 0xF)) u
  _ -> alu fw op u
 where
  op = fwProgram fw U.! fromIntegral (uPc u0 .&. 0x7FF)
  u = u0 { uPc = (uPc u0 + 1) .&. 0x7FF }

-- | M and N from K and L, after every instruction.
multiply :: Upd -> Upd
multiply u =
  let p = 2 * (fromIntegral (fromIntegral (uK u) :: Int16) * fromIntegral (fromIntegral (uL u) :: Int16)) :: Int32
  in u { uM = fromIntegral (p `shiftR` 16), uN = fromIntegral p }

ram :: Upd -> Word8 -> Word16
ram u a = uRam u U.! fromIntegral a

dataRom :: Firmware -> Upd -> Word16
dataRom fw u = fwData fw U.! fromIntegral (uRp u .&. 0x3FF)

-- | The SRC field's value (and DR's read: RQM set).
source :: Firmware -> Int -> Upd -> (Word16, Upd)
source fw s u = case s of
  0 -> (uTrb u, u)
  1 -> (uA u, u)
  2 -> (uB u, u)
  3 -> (uTr u, u)
  4 -> (fromIntegral (uDp u), u)
  5 -> (uRp u, u)
  6 -> (dataRom fw u, u)
  7 -> (if fS1 (uFa u) then 0x7FFF else 0x8000, u)
  8 -> (uDr u, u { uSr = setBit (uSr u) bRqm })
  9 -> (uDr u, u)
  10 -> (uSr u, u)
  13 -> (uK u, u)
  14 -> (uL u, u)
  15 -> (ram u (uDp u), u)
  _ -> (0, u) -- SIM, SIL: the serial input, not connected

-- | The DST field written with a value (ALU and LD instructions).
store :: Firmware -> Word16 -> Int -> Upd -> Upd
store fw v d u = case d of
  1 -> u { uA = v }
  2 -> u { uB = v }
  3 -> u { uTr = v }
  4 -> u { uDp = fromIntegral v }
  5 -> u { uRp = v .&. 0x3FF }
  6 -> u { uDr = v, uSr = setBit (uSr u) bRqm }
  7 -> u { uSr = (uSr u .&. srKept) .|. (v .&. complement srKept) }
  8 -> u { uSo = v }
  9 -> u { uSo = v }
  10 -> u { uK = v }
  11 -> u { uK = v, uL = dataRom fw u }
  12 -> u { uL = v, uK = ram u (uDp u .|. 0x40) }
  13 -> u { uL = v }
  14 -> u { uTrb = v }
  15 -> u { uRam = uRam u U.// [(fromIntegral (uDp u), v)] }
  _ -> u

-- | An ALU instruction: SRC read, the ALU on P and the accumulator, DST
-- written, DP and RP adjusted, then the return if RT.
alu :: Firmware -> Word32 -> Upd -> Upd
alu fw op u0 =
  let f n w = fromIntegral ((op `shiftR` n) .&. w) :: Int
      rt = testBit op 22
      pSel = f 20 3
      code = f 16 15
      useB = testBit op 15
      dpl = f 13 3
      dph = f 9 15
      rpDec = testBit op 8
      (idb, u1) = source fw (f 4 15) u0
      p = case pSel of
        0 -> ram u0 (uDp u0)
        1 -> idb
        2 -> uM u0
        _ -> uN u0
      u2 = if code == 0 then u1 else
             let (q, fl, other) = if useB then (uB u1, uFb u1, uFa u1) else (uA u1, uFa u1, uFb u1)
                 (r, fl') = aluOp code q p (fC other) fl
             in if useB then u1 { uB = r, uFb = fl' } else u1 { uA = r, uFa = fl' }
      u3 = store fw idb (f 0 15) u2
      d = uDp u3
      lo = case dpl of
        1 -> (d + 1) .&. 0x0F
        2 -> (d - 1) .&. 0x0F
        3 -> 0
        _ -> d .&. 0x0F
      u4 = u3 { uDp = ((d .&. 0xF0) `xor` (fromIntegral dph `shiftL` 4)) .|. lo
              , uRp = if rpDec then (uRp u3 - 1) .&. 0x3FF else uRp u3 }
  in if rt then pop u4 else u4

-- | The ALU (fullsnes's table): the result and the accumulator's flags.
-- Logic operations and shifts clear C (but the shifts out), OV0 and OV1
-- and copy S0 to S1; arithmetic sets C from the carry or borrow, OV0 on a
-- signed overflow, and on one S1 to S0 and OV1 inverted.
aluOp :: Int -> Word16 -> Word16 -> Bool -> Flags -> (Word16, Flags)
aluOp code q p oc fl = case code of
  1 -> logic (q .|. p) False
  2 -> logic (q .&. p) False
  3 -> logic (q `xor` p) False
  4 -> arith (sq - sp) (iq - ip) (ip > iq)
  5 -> arith (sq + sp) (iq + ip) (iq + ip > 0xFFFF)
  6 -> arith (sq - sp - c) (iq - ip - c) (ip + c > iq)
  7 -> arith (sq + sp + c) (iq + ip + c) (iq + ip + c > 0xFFFF)
  8 -> arith (sq - 1) (iq - 1) (iq < 1)
  9 -> arith (sq + 1) (iq + 1) (iq + 1 > 0xFFFF)
  10 -> logic (complement q) False
  11 -> logic ((q `shiftR` 1) .|. (q .&. 0x8000)) (testBit q 0)
  12 -> logic ((q `shiftL` 1) .|. (if oc then 1 else 0)) (testBit q 15)
  13 -> logic ((q `shiftL` 2) .|. 3) False
  14 -> logic ((q `shiftL` 4) .|. 15) False
  15 -> logic ((q `shiftL` 8) .|. (q `shiftR` 8)) False
  _ -> (q, fl)
 where
  iq = fromIntegral q :: Int
  ip = fromIntegral p :: Int
  sq = fromIntegral (fromIntegral q :: Int16) :: Int
  sp = fromIntegral (fromIntegral p :: Int16) :: Int
  c = if oc then 1 else 0
  logic r cy = let s = testBit r 15 in (r, Flags s s cy (r == 0) False False)
  arith s full cy =
    let r = fromIntegral full :: Word16
        sgn = testBit r 15
        ov = s > 32767 || s < -32768
    in (r, Flags sgn (if ov then sgn else fS1 fl) cy (r == 0) ov (if ov then not (fOv1 fl) else fOv1 fl))

-- | A jump or call (fullsnes's BRCH table); an unknown one doesn't jump.
jump :: Firmware -> Upd -> Word32 -> Upd
jump _ u op =
  let brch = fromIntegral ((op `shiftR` 13) .&. 0x1FF) :: Int
      na = fromIntegral ((op `shiftR` 2) .&. 0x7FF) :: Word16
      fa = uFa u
      fb = uFb u
      d = uDp u .&. 0x0F
      cond = case brch of
        0x080 -> Just (not (fC fa))
        0x082 -> Just (fC fa)
        0x084 -> Just (not (fC fb))
        0x086 -> Just (fC fb)
        0x088 -> Just (not (fZ fa))
        0x08A -> Just (fZ fa)
        0x08C -> Just (not (fZ fb))
        0x08E -> Just (fZ fb)
        0x090 -> Just (not (fOv0 fa))
        0x092 -> Just (fOv0 fa)
        0x094 -> Just (not (fOv0 fb))
        0x096 -> Just (fOv0 fb)
        0x098 -> Just (not (fOv1 fa))
        0x09A -> Just (fOv1 fa)
        0x09C -> Just (not (fOv1 fb))
        0x09E -> Just (fOv1 fb)
        0x0A0 -> Just (not (fS0 fa))
        0x0A2 -> Just (fS0 fa)
        0x0A4 -> Just (not (fS0 fb))
        0x0A6 -> Just (fS0 fb)
        0x0A8 -> Just (not (fS1 fa))
        0x0AA -> Just (fS1 fa)
        0x0AC -> Just (not (fS1 fb))
        0x0AE -> Just (fS1 fb)
        0x0B0 -> Just (d == 0)
        0x0B1 -> Just (d /= 0)
        0x0B2 -> Just (d == 0x0F)
        0x0B3 -> Just (d /= 0x0F)
        0x0B4 -> Just True  -- JNSIAK: SI's ACK never set
        0x0B6 -> Just False
        0x0B8 -> Just True  -- JNSOAK
        0x0BA -> Just False
        0x0BC -> Just (not (rqm u))
        0x0BE -> Just (rqm u)
        0x100 -> Just True
        _ -> Nothing
  in case brch of
       0x140 -> (push u) { uPc = na }
       _ | cond == Just True -> u { uPc = na }
         | otherwise -> u

-- | The stack: four levels, a push shifts them down (the bottom lost).
push :: Upd -> Upd
push u = u { uStack = take 4 (uPc u : uStack u) }

-- | A return: PC from the top, the levels shifted up (the bottom kept).
pop :: Upd -> Upd
pop u = case uStack u of
  t : rest -> u { uPc = t .&. 0x7FF, uStack = take 4 (rest <> drop 3 (uStack u)) }
  [] -> u
