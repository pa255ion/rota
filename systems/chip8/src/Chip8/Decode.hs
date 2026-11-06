{-|
Decoder for the hardware core (PLAN.md step 1.2): combinational, total
over all 16-bit words. 'encode' is its inverse; the tests check
@decode . encode == id@ for every 'Instr' and @encode . decode == id@ for
every word, so no word is lost or mangled.
-}
module Chip8.Decode
  ( decode
  , encode
  ) where

import Clash.Prelude

import Chip8.Types

decode :: BitVector 16 -> Instr
decode w = case (n3, n2, n1, n0) of
  (0x0, 0x0, 0xE, 0x0) -> Cls
  (0x0, 0x0, 0xE, 0xE) -> Ret
  (0x1, _, _, _)       -> Jp nnn
  (0x2, _, _, _)       -> Call nnn
  (0x3, _, _, _)       -> SeImm x kk
  (0x4, _, _, _)       -> SneImm x kk
  (0x5, _, _, 0x0)     -> SeReg x y
  (0x6, _, _, _)       -> LdImm x kk
  (0x7, _, _, _)       -> AddImm x kk
  (0x8, _, _, 0x0)     -> Alu AluMov x y
  (0x8, _, _, 0x1)     -> Alu AluOr x y
  (0x8, _, _, 0x2)     -> Alu AluAnd x y
  (0x8, _, _, 0x3)     -> Alu AluXor x y
  (0x8, _, _, 0x4)     -> Alu AluAdd x y
  (0x8, _, _, 0x5)     -> Alu AluSub x y
  (0x8, _, _, 0x6)     -> Alu AluShr x y
  (0x8, _, _, 0x7)     -> Alu AluSubn x y
  (0x8, _, _, 0xE)     -> Alu AluShl x y
  (0x9, _, _, 0x0)     -> SneReg x y
  (0xA, _, _, _)       -> LdI nnn
  (0xB, _, _, _)       -> JpOff nnn
  (0xC, _, _, _)       -> Rnd x kk
  (0xD, _, _, _)       -> Drw x y (unpack n0)
  (0xE, _, 0x9, 0xE)   -> Skp x
  (0xE, _, 0xA, 0x1)   -> Sknp x
  (0xF, _, 0x0, 0x7)   -> LdVxDt x
  (0xF, _, 0x0, 0xA)   -> LdKey x
  (0xF, _, 0x1, 0x5)   -> LdDtVx x
  (0xF, _, 0x1, 0x8)   -> LdStVx x
  (0xF, _, 0x1, 0xE)   -> AddI x
  (0xF, _, 0x2, 0x9)   -> LdFont x
  (0xF, _, 0x3, 0x3)   -> Bcd x
  (0xF, _, 0x5, 0x5)   -> Store x
  (0xF, _, 0x6, 0x5)   -> Load x
  _                    -> Illegal w
 where
  n3, n2, n1, n0 :: BitVector 4
  n3 = slice d15 d12 w
  n2 = slice d11 d8 w
  n1 = slice d7 d4 w
  n0 = slice d3 d0 w
  x = unpack n2 :: Reg
  y = unpack n1 :: Reg
  kk = slice d7 d0 w
  nnn = unpack (slice d11 d0 w) :: Addr

encode :: Instr -> BitVector 16
encode i = case i of
  Cls          -> 0x00E0
  Ret          -> 0x00EE
  Jp a         -> op 0x1 a
  Call a       -> op 0x2 a
  SeImm x k    -> xk 0x3 x k
  SneImm x k   -> xk 0x4 x k
  SeReg x y    -> xyn 0x5 x y 0x0
  LdImm x k    -> xk 0x6 x k
  AddImm x k   -> xk 0x7 x k
  Alu f x y    -> xyn 0x8 x y (aluNibble f)
  SneReg x y   -> xyn 0x9 x y 0x0
  LdI a        -> op 0xA a
  JpOff a      -> op 0xB a
  Rnd x k      -> xk 0xC x k
  Drw x y n    -> xyn 0xD x y (pack n)
  Skp x        -> xk 0xE x 0x9E
  Sknp x       -> xk 0xE x 0xA1
  LdVxDt x     -> xk 0xF x 0x07
  LdKey x      -> xk 0xF x 0x0A
  LdDtVx x     -> xk 0xF x 0x15
  LdStVx x     -> xk 0xF x 0x18
  AddI x       -> xk 0xF x 0x1E
  LdFont x     -> xk 0xF x 0x29
  Bcd x        -> xk 0xF x 0x33
  Store x      -> xk 0xF x 0x55
  Load x       -> xk 0xF x 0x65
  Illegal w    -> w
 where
  op :: BitVector 4 -> Addr -> BitVector 16
  op h a = h ++# pack a
  xk :: BitVector 4 -> Reg -> Byte -> BitVector 16
  xk h x k = h ++# pack x ++# k
  xyn :: BitVector 4 -> Reg -> Reg -> BitVector 4 -> BitVector 16
  xyn h x y n = h ++# pack x ++# pack y ++# n

aluNibble :: AluOp -> BitVector 4
aluNibble f = case f of
  AluMov  -> 0x0
  AluOr   -> 0x1
  AluAnd  -> 0x2
  AluXor  -> 0x3
  AluAdd  -> 0x4
  AluSub  -> 0x5
  AluShr  -> 0x6
  AluSubn -> 0x7
  AluShl  -> 0xE
