{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NumericUnderscores #-}
{-|
The 68000's pure operations, used by the Clash core (side track P;
"M68k.Datapath"): operand sizes and fields, the ALU
and its flag rules, the shifts and rotates in closed form, BCD, the bit
operations, the condition codes, DIVU's clocks, and the opcode decoders
(each in the spec's order). Written independently of the spec's (both are
checked against SingleStepTests).
-}
module M68k.Ops where

import Clash.Prelude

-- | An operand's size.
data Sz = SB | SW | SL
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFDataX, BitPack)

-- | Which of the opcode's effective-address fields an operand is: bits
-- 0-5 (mode 3-5, register 0-2), or MOVE's destination (register 9-11,
-- mode 6-8).
data Who
  = WSrc   -- ^ bits 0-5
  | WDst   -- ^ MOVE's destination
  | WImm   -- ^ an immediate (#<data>: mode 7, register 4)
  | WPostY -- ^ (Ay)+, Ay in bits 0-2 (CMPM)
  | WPostX -- ^ (Ax)+, Ax in bits 9-11
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFDataX, BitPack)

-- | SR's implemented bits (T, S, the mask, X N Z V C).
srMask :: Unsigned 16
srMask = 0xA71F

trunc :: Sz -> Unsigned 32 -> Unsigned 32
trunc SB v = v .&. 0xFF
trunc SW v = v .&. 0xFFFF
trunc SL v = v

top :: Sz -> Unsigned 32 -> Bool
top SB v = testBit v 7
top SW v = testBit v 15
top SL v = testBit v 31

sext8, sext16 :: Unsigned 32 -> Unsigned 32
sext8 v = if testBit v 7 then v .|. 0xFFFF_FF00 else v .&. 0xFF

sext16 v = if testBit v 15 then v .|. 0xFFFF_0000 else v .&. 0xFFFF

-- | A data register with its low byte or word replaced.
mergeD :: Sz -> Unsigned 32 -> Unsigned 32 -> Unsigned 32
mergeD SB old v = (old .&. 0xFFFF_FF00) .|. (v .&. 0xFF)
mergeD SW old v = (old .&. 0xFFFF_0000) .|. (v .&. 0xFFFF)
mergeD SL _ v = v

w32 :: Unsigned 16 -> Unsigned 32
w32 = resize

-- | A 3-bit field of the opcode.
field :: Unsigned 16 -> Int -> Unsigned 3
field op pos = resize (op `shiftR` pos)

-- | (An)+ and -(An)'s step: a byte on A7 moves it by two.
stepOf :: Sz -> Unsigned 3 -> Unsigned 32
stepOf SB 7 = 2
stepOf SB _ = 1
stepOf SW _ = 2
stepOf SL _ = 4

-- | The mode and register of an operand of the instruction.
eaOf :: Who -> Unsigned 16 -> (Unsigned 3, Unsigned 3)
eaOf WSrc op = (field op 3, field op 0)
eaOf WDst op = (field op 6, field op 9)
eaOf WImm _ = (7, 4)
eaOf WPostY op = (3, field op 0)
eaOf WPostX op = (3, field op 9)

moveSz :: Unsigned 16 -> Sz
moveSz op = case op `shiftR` 12 of
  1 -> SB
  3 -> SW
  _ -> SL

-- | The ALU's operations.
data Alu = AAdd | ASub | AAnd | AOr | AEor | ACmp
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFDataX, BitPack)

-- | An operand size's width in bits.
bitsOf :: Sz -> Int
bitsOf SB = 8
bitsOf SW = 16
bitsOf SL = 32

-- | SR with N, Z, V, C (and with @setX@ X: the carry) from a result;
-- @zKeep@: Z only ever cleared (ADDX, SUBX, NEGX).
arithFlags :: Bool -> Bool -> Sz -> Unsigned 32 -> Bool -> Bool -> Unsigned 16 -> Unsigned 16
arithFlags setX zKeep sz r v cy sr =
  let z = trunc sz r == 0 && (not zKeep || testBit sr 2)
      x = if setX then cy else testBit sr 4
      flag b i = if b then bit i else 0
  in (sr .&. 0xFFE0) .|. flag x 4 .|. flag (top sz r) 3 .|. flag z 2 .|. flag v 1 .|. flag cy 0

-- | SR with N and Z from a result, V and C cleared, X kept.
logicSr :: Sz -> Unsigned 32 -> Unsigned 16 -> Unsigned 16
logicSr sz r sr = arithFlags False False sz r False False sr

-- | @d + s + x@ or @d - s - x@ at a size: the result, the carry (borrow)
-- out of its top bit, the overflow.
addSub :: Bool -> Sz -> Unsigned 32 -> Unsigned 32 -> Unsigned 32 -> (Unsigned 32, Bool, Bool)
addSub isAdd sz s0 d0' x =
  let s = trunc sz s0
      d = trunc sz d0'
      wide :: Unsigned 33
      wide = if isAdd then resize d + resize s + resize x else resize d - resize s - resize x
      r = trunc sz (resize wide)
      cy = testBit wide (bitsOf sz)
      sm = top sz s
      dm = top sz d
      rm = top sz r
      v = if isAdd then sm == dm && rm /= sm else sm /= dm && rm /= dm
  in (r, cy, v)

-- | @d op s@ (Sub, Cmp: @d - s@) at a size: the result (Cmp: @d@) and SR.
{-# OPAQUE alu #-}
alu :: Alu -> Sz -> Unsigned 32 -> Unsigned 32 -> Unsigned 16 -> (Unsigned 32, Unsigned 16)
alu k sz s d sr = case k of
  AAdd -> let (r, cy, v) = addSub True sz s d 0 in (r, arithFlags True False sz r v cy sr)
  ASub -> let (r, cy, v) = addSub False sz s d 0 in (r, arithFlags True False sz r v cy sr)
  ACmp -> let (r, cy, v) = addSub False sz s d 0 in (d, arithFlags False False sz r v cy sr)
  AAnd -> logic (d .&. s)
  AOr -> logic (d .|. s)
  AEor -> logic (d `xor` s)
 where
  logic r0 = let r = trunc sz r0 in (r, logicSr sz r sr)

-- | ADDX, SUBX, NEGX: X in the sum, Z only ever cleared.
{-# OPAQUE aluX #-}
aluX :: Bool -> Sz -> Unsigned 32 -> Unsigned 32 -> Unsigned 16 -> (Unsigned 32, Unsigned 16)
aluX isAdd sz s d sr =
  let x = if testBit sr 4 then 1 else 0
      (r, cy, v) = addSub isAdd sz s d x
  in (r, arithFlags True True sz r v cy sr)

-- | What an opcode of the ALU group is (the spec's 'arith', in its order).
data Form
  = FNone
  | FToDn    -- ^ <ea>,Dn (ADD, SUB, CMP, AND, OR)
  | FToMem   -- ^ Dn,<ea> (ADD, SUB, AND, OR, EOR)
  | FEorDn   -- ^ EOR Dn,Dn
  | FAddr    -- ^ ADDA, SUBA, CMPA
  | FExt     -- ^ ADDX, SUBX
  | FCmpm    -- ^ CMPM
  | FImm     -- ^ ORI, ANDI, SUBI, ADDI, EORI, CMPI
  | FQuick   -- ^ ADDQ, SUBQ
  | FUnary   -- ^ NEGX, CLR, NEG, NOT
  | FTst     -- ^ TST
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFDataX, BitPack)

-- | The size field (bits 6-7): 3 is none.
sizeOk :: Unsigned 16 -> Bool
sizeOk op = (op `shiftR` 6) .&. 3 /= 3

szOf :: Unsigned 16 -> Sz
szOf op = case (op `shiftR` 6) .&. 3 of
  0 -> SB
  1 -> SW
  _ -> SL

-- | A mode and register that name an addressing mode.
eaValid :: Unsigned 3 -> Unsigned 3 -> Bool
eaValid m r = m /= 7 || r <= 4

-- | Memory alterable: memory, not PC-relative or an immediate.
memAlt :: Unsigned 3 -> Unsigned 3 -> Bool
memAlt m r = m >= 2 && (m /= 7 || r <= 1)

-- | Data alterable: Dn or memory alterable.
dataAlt :: Unsigned 3 -> Unsigned 3 -> Bool
dataAlt m r = m == 0 || memAlt m r

arithForm :: Unsigned 16 -> Form
arithForm op = case op `shiftR` 12 of
  0xD -> twoOp True
  0x9 -> twoOp True
  0xC -> twoOp False
  0x8 -> twoOp False
  0xB -> cmpEor
  0x0 -> immediate
  0x5 -> if sizeOk op && eaValid m r && (dataAlt m r || (m == 1 && szOf op /= SB)) then FQuick else FNone
  0x4 -> single
  _ -> FNone
 where
  (m, r) = eaOf WSrc op
  opmode = field op 6
  sz = szOf op
  okSrc addSubCmp = m /= 1 || (addSubCmp && sz /= SB)
  twoOp addSub'
    | opmode == 3 || opmode == 7 = if addSub' && eaValid m r then FAddr else FNone
    | opmode <= 2 && eaValid m r && okSrc addSub' = FToDn
    | opmode >= 4 && m <= 1 = if addSub' then FExt else FNone
    | opmode >= 4 && memAlt m r = FToMem
    | otherwise = FNone
  cmpEor
    | opmode == 3 || opmode == 7 = if eaValid m r then FAddr else FNone
    | opmode <= 2 && eaValid m r && okSrc True = FToDn
    | m == 1 = FCmpm
    | m == 0 = FEorDn
    | memAlt m r = FToMem
    | otherwise = FNone
  immediate
    | testBit op 8 = FNone
    | field op 9 == 4 || field op 9 == 7 = FNone
    | sizeOk op && dataAlt m r = FImm
    | otherwise = FNone
  single = case (op `shiftR` 8) .&. 0xF of
    0x0 -> unary
    0x2 -> unary
    0x4 -> unary
    0x6 -> unary
    0xA -> if sizeOk op && dataAlt m r then FTst else FNone
    _ -> FNone
  unary = if sizeOk op && dataAlt m r then FUnary else FNone

-- | The line's operation (ADD, SUB, AND, OR; line B: CMP for <ea>,Dn and
-- CMPA, else EOR).
lineAlu :: Unsigned 16 -> Alu
lineAlu op = case op `shiftR` 12 of
  0xD -> AAdd
  0x9 -> ASub
  0xC -> AAnd
  0x8 -> AOr
  _ -> if field op 6 <= 3 || field op 6 == 7 then ACmp else AEor

-- | ORI, ANDI, SUBI, ADDI, EORI, CMPI by bits 9-11.
immAlu :: Unsigned 16 -> Alu
immAlu op = case field op 9 of
  0 -> AOr
  1 -> AAnd
  2 -> ASub
  3 -> AAdd
  5 -> AEor
  _ -> ACmp

-- | ADDQ/SUBQ's data: 1-8.
quickData :: Unsigned 16 -> Unsigned 32
quickData op = let n = resize (field op 9) in if n == 0 then 8 else n

-- | NEGX, CLR, NEG, NOT of a value.
unaryOp :: Unsigned 16 -> Sz -> Unsigned 32 -> Unsigned 16 -> (Unsigned 32, Unsigned 16)
unaryOp op sz v sr = case (op `shiftR` 8) .&. 0xF of
  0x0 -> aluX False sz v 0 sr
  0x2 -> (0, logicSr sz 0 sr)
  0x4 -> alu ASub sz v 0 sr
  _ -> let r = trunc sz (complement v) in (r, logicSr sz r sr)

data Shift = SAs | SLs | SRox | SRo
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFDataX, BitPack)

-- | The kind in a 2-bit field.
shiftKind :: Unsigned 16 -> Shift
shiftKind f = case f .&. 3 of
  0 -> SAs
  1 -> SLs
  2 -> SRox
  _ -> SRo

-- | A shift or rotate by @n@ (0-63) at a size, in closed form (the spec
-- goes bit by bit): the result and SR. C: the last bit out (none: cleared;
-- ROXL/ROXR: X); X as C unless ROL/ROR or a count of 0; ASL's V: the sign
-- changed at a step (the top n + 1 bits, the zeros shifted in counted,
-- not all equal); ASR past the width: C and X cleared, as the tests
-- record.
{-# OPAQUE shiftOp #-}
shiftOp :: Shift -> Bool -> Sz -> Unsigned 6 -> Unsigned 32 -> Unsigned 16 -> (Unsigned 32, Unsigned 16)
shiftOp k left sz n v0 sr = case sz of
  SB -> shiftAt 8 k left sz n v0 sr
  SW -> shiftAt 16 k left sz n v0 sr
  SL -> shiftAt 32 k left sz n v0 sr

-- | @n mod d@ for @n@ below 64 and a constant @d@ of 8 or more, by
-- repeated subtraction (no divider: hardware).
modSmall :: Unsigned 7 -> Unsigned 7 -> Unsigned 7
modSmall d n0 = foldl (\x _ -> if x >= d then x - d else x) n0 (replicate d8 ())

-- | 'shiftOp' at a width @w@ (8, 16, 32: a constant in each use), in 32
-- bits (33 for ROXL/ROXR, X beside the operand): C the last bit out; ASL's
-- V when a bit of the top n + 1 differs from the next (the bits shifted
-- in at the bottom, zeros, counted), or for a count past the width when
-- the operand isn't zero; ASR past the width: C and X cleared, as the
-- tests record.
shiftAt :: Unsigned 7 -> Shift -> Bool -> Sz -> Unsigned 6 -> Unsigned 32 -> Unsigned 16 -> (Unsigned 32, Unsigned 16)
shiftAt w k left sz n v0 sr =
  let ni = resize n :: Unsigned 7
      wI = fromIntegral w :: Int
      nI = fromIntegral ni :: Int
      mask = if w == 32 then maxBound else (1 `shiftL` wI) - 1 :: Unsigned 32
      v = v0 .&. mask
      x0 = testBit sr 4
      sign = testBit v (wI - 1)
      past = ni >= w
      outL = ni >= 1 && not (ni > w) && testBit v (fromIntegral (w - ni))
      outR = ni >= 1 && not (ni > w) && testBit v (fromIntegral (ni - 1))
      -- a rotation left by @m@ (below @bits@) of @y@ within @bits@ bits
      rot32 m y = let mI = fromIntegral m :: Int
                  in ((y `shiftL` mI) .|. (y `shiftR` (wI - mI))) .&. mask
      rot33 m y = let mI = fromIntegral m :: Int
                      bits = wI + 1
                      ym = (1 `shiftL` bits) - 1 :: Unsigned 33
                  in ((y `shiftL` mI) .|. (y `shiftR` (bits - mI))) .&. ym
      (r, cy, ov) = case k of
        SRo ->
          let m = ni .&. (w - 1)
              r' = if left then rot32 m v else rot32 ((w - m) .&. (w - 1)) v
          in (r', ni > 0 && (if left then testBit r' 0 else testBit r' (wI - 1)), False)
        SRox ->
          let m = modSmall (w + 1) ni
              y0 = resize v .|. (if x0 then 1 `shiftL` wI else 0) :: Unsigned 33
              mr = if m == 0 then 0 else w + 1 - m
              y = if left then rot33 m y0 else rot33 mr y0
          in (resize y .&. mask, if ni == 0 then x0 else testBit y wI, False)
        _ | left ->
              let r' = if past then 0 else (v `shiftL` nI) .&. mask
                  -- the top n bits of v and the bit below each: differ
                  -- anywhere, the sign changed on the way
                  top = mask .&. complement (mask `shiftR` nI)
                  chg = (v `xor` (v `shiftL` 1)) .&. top
                  v' = if past then v /= 0 else chg /= 0
              in (r', outL, k == SAs && v')
          | k == SLs -> (if past then 0 else v `shiftR` nI, outR, False)
          | otherwise ->
              let sv = unpack (pack (if sign then v .|. complement mask else v)) :: Signed 32
                  r' | past = if sign then mask else 0
                     | otherwise = unpack (pack (sv `shiftR` nI)) .&. mask
              in (r', outR && ni <= w, False)
      x = if k == SRo || ni == 0 then x0 else cy
      flag b i = if b then bit i else 0
      _ = sz
  in (r, (sr .&. 0xFFE0) .|. flag x 4 .|. flag (testBit r (wI - 1)) 3 .|. flag (r == 0) 2 .|. flag ov 1 .|. flag cy 0)

-- | Line E: a shift or rotate, on Dn or (a word by one) on memory.
isShift :: Unsigned 16 -> Bool
isShift op =
  let (m, r) = eaOf WSrc op
  in op .&. 0xF000 == 0xE000 && ((op `shiftR` 6) .&. 3 /= 3 || (not (testBit op 11) && memAlt m r))

-- | BTST, BCHG, BCLR, BSET: the bit number from Dn (0000 rrr1 oo; mode 1
-- is MOVEP) or an extension word (0000 1000 oo).
isBitOp :: Unsigned 16 -> Bool
isBitOp op =
  let (m, r) = eaOf WSrc op
      fromExt = op .&. 0xFF00 == 0x0800
      dynamic = testBit op 8 && m /= 1
      btst = (op `shiftR` 6) .&. 3 == 0
      ok = eaValid m r && m /= 1
           && (not (m == 7 && r == 4) || (btst && not fromExt))
           && (not (m == 7 && (r == 2 || r == 3)) || btst)
  in op .&. 0xF000 == 0 && (dynamic || fromExt) && ok

bitApply :: Unsigned 16 -> Int -> Unsigned 32 -> Unsigned 32
bitApply k n d = case k of
  0 -> d
  1 -> complementBit d n
  2 -> clearBit d n
  _ -> setBit d n

-- | The 16 conditions on SR.
cond :: Unsigned 4 -> Unsigned 16 -> Bool
cond n sr =
  let cf = testBit sr 0
      vf = testBit sr 1
      zf = testBit sr 2
      nf = testBit sr 3
  in case n of
       0 -> True
       1 -> False
       2 -> not cf && not zf
       3 -> cf || zf
       4 -> not cf
       5 -> cf
       6 -> not zf
       7 -> zf
       8 -> not vf
       9 -> vf
       10 -> not nf
       11 -> nf
       12 -> nf == vf
       13 -> nf /= vf
       14 -> not zf && nf == vf
       _ -> zf || nf /= vf

-- | The control addressing modes.
controlEa :: Unsigned 3 -> Unsigned 3 -> Bool
controlEa m r = m == 2 || m == 5 || m == 6 || (m == 7 && r <= 3)

-- | An indexed mode ((d8,An,Xn), (d8,PC,Xn)).
idxEa :: Unsigned 3 -> Unsigned 3 -> Bool
idxEa m r = m == 6 || (m == 7 && r == 3)

data Sys
  = SNone | SCcrImm | SSrImm | STrapv | STrap | SLink | SUnlk | SToUsp | SFromUsp
  | SSwap | SExtW | SExtL | SPea | SLea | SChk | SFromSr | SToCcr | SToSr | SScc | STas | SExg
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFDataX, BitPack)

-- | The spec's 'system', in its order.
sysForm :: Unsigned 16 -> Sys
sysForm op
  | op == 0x003C || op == 0x023C || op == 0x0A3C = SCcrImm
  | op == 0x007C || op == 0x027C || op == 0x0A7C = SSrImm
  | op == 0x4E76 = STrapv
  | op .&. 0xFFF0 == 0x4E40 = STrap
  | op .&. 0xFFF8 == 0x4E50 = SLink
  | op .&. 0xFFF8 == 0x4E58 = SUnlk
  | op .&. 0xFFF8 == 0x4E60 = SToUsp
  | op .&. 0xFFF8 == 0x4E68 = SFromUsp
  | op .&. 0xFFF8 == 0x4840 = SSwap
  | op .&. 0xFFF8 == 0x4880 = SExtW
  | op .&. 0xFFF8 == 0x48C0 = SExtL
  | op .&. 0xFFC0 == 0x4840 && controlEa m r = SPea
  | op .&. 0xF1C0 == 0x41C0 && controlEa m r = SLea
  | op .&. 0xF1C0 == 0x4180 && dataEa = SChk
  | op .&. 0xFFC0 == 0x40C0 && eaValid m r && dataAlt m r = SFromSr
  | op .&. 0xFFC0 == 0x44C0 && dataEa = SToCcr
  | op .&. 0xFFC0 == 0x46C0 && dataEa = SToSr
  | op .&. 0xF0C0 == 0x50C0 && eaValid m r && dataAlt m r = SScc
  | op .&. 0xFFC0 == 0x4AC0 && op /= 0x4AFC && eaValid m r && dataAlt m r = STas
  | op .&. 0xF130 == 0xC100 && (exgMode == 0x08 || exgMode == 0x09 || exgMode == 0x11) = SExg
  | otherwise = SNone
 where
  (m, r) = eaOf WSrc op
  dataEa = eaValid m r && m /= 1
  exgMode = (op `shiftR` 3) .&. 0x1F

data Br = BNone | BBcc | BDbcc | BJmp | BJsr | BRts | BRtr | BRte | BReset
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFDataX, BitPack)

-- | The spec's 'branches', in its order.
brForm :: Unsigned 16 -> Br
brForm op
  | op .&. 0xF000 == 0x6000 = BBcc
  | op .&. 0xF0F8 == 0x50C8 = BDbcc
  | op .&. 0xFFC0 == 0x4EC0 && controlEa m r = BJmp
  | op .&. 0xFFC0 == 0x4E80 && controlEa m r = BJsr
  | op == 0x4E75 = BRts
  | op == 0x4E77 = BRtr
  | op == 0x4E73 = BRte
  | op == 0x4E70 = BReset
  | otherwise = BNone
 where
  (m, r) = eaOf WSrc op

-- | MULU, MULS, DIVU, DIVS, ABCD, SBCD, NBCD, MOVEP (the spec's 'divInstr'
-- and 'mulBcd', in its order).
data Md = MNone | MDiv | MMul | MAbcd | MSbcd | MNbcd | MMovep
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFDataX, BitPack)

mdForm :: Unsigned 16 -> Md
mdForm op
  | op .&. 0xF0C0 == 0x80C0 && dataEa = MDiv
  | op .&. 0xF0C0 == 0xC0C0 && dataEa = MMul
  | op .&. 0xF1F0 == 0xC100 = MAbcd
  | op .&. 0xF1F0 == 0x8100 = MSbcd
  | op .&. 0xFFC0 == 0x4800 && eaValid m r && dataAlt m r = MNbcd
  | op .&. 0xF138 == 0x0108 = MMovep
  | otherwise = MNone
 where
  (m, r) = eaOf WSrc op
  dataEa = eaValid m r && m /= 1

-- | ABCD and SBCD on a byte with X (NBCD: SBCD from 0), as flamewing's
-- analysis of the chip's BCD logic (the spec's formulas: random bytes,
-- not BCD, pin them down): the binary result and its per-nibble carries
-- give the correction; X and C the decimal carry, Z only cleared, N bit 7,
-- V the correction's effect on bit 7.
{-# OPAQUE bcd #-}
bcd :: Bool -> Unsigned 32 -> Unsigned 32 -> Unsigned 16 -> (Unsigned 32, Unsigned 16)
bcd isAdd s0 d0' sr =
  let x = if testBit sr 4 then 1 else 0 :: Unsigned 16
      s = resize (s0 .&. 0xFF) :: Unsigned 16
      d = resize (d0' .&. 0xFF) :: Unsigned 16
      (rr, cy, v)
        | isAdd =
            let ss = s + d + x
                bc = ((s .&. d) .|. (complement ss .&. d) .|. (s .&. complement ss)) .&. 0x88
                dc = (((ss + 0x66) `xor` ss) .&. 0x110) `shiftR` 1
                corf = (bc .|. dc) - ((bc .|. dc) `shiftR` 2)
                r' = ss + corf
            in (r', testBit (bc .|. (ss .&. complement r')) 7, testBit (complement ss .&. r') 7)
        | otherwise =
            let dd = d - s - x
                bc = ((complement d .&. s) .|. (dd .&. complement d) .|. (dd .&. s)) .&. 0x88
                corf = bc - (bc `shiftR` 2)
                r' = dd - corf
            in (r', testBit (bc .|. (complement dd .&. r')) 7, testBit (dd .&. complement r') 7)
      res = resize (rr .&. 0xFF) :: Unsigned 32
      flag b i = if b then bit i else 0
      z = res == 0 && testBit sr 2
  in (res, (sr .&. 0xFFE0) .|. flag cy 4 .|. flag (testBit res 7) 3 .|. flag z 2 .|. flag v 1 .|. flag cy 0)

-- | DIVU's clocks (Jorge Cwik's analysis of the chip's division): 10 on
-- overflow, else its 15 steps' cost.
divuClocks :: Unsigned 32 -> Unsigned 32 -> Unsigned 8
divuClocks dvd dvs
  | (dvd `shiftR` 16) >= dvs = 10
  | otherwise = snd (foldl stepF (dvd, 38) (replicate d15 ())) * 2
 where
  h = dvs `shiftL` 16
  stepF (dv, m) _ =
    let dv' = dv `shiftL` 1
    in if testBit dv 31 then (dv' - h, m)
       else if dv' >= h then (dv' - h, m + 1)
       else (dv', m + 2)

isMovem :: Unsigned 16 -> Bool
isMovem op =
  let (m, r) = eaOf WSrc op
      toMem = not (testBit op 10)
      ok | toMem = m == 4 || (controlEa m r && not (m == 7 && (r == 2 || r == 3)))
         | otherwise = m == 3 || controlEa m r
  in op .&. 0xFB80 == 0x4880 && eaValid m r && ok

movemSz :: Unsigned 16 -> Sz
movemSz op = if testBit op 6 then SL else SW
