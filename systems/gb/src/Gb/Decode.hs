{-|
SM83 opcode decoder for the Clash core (PLAN.md step 2.3), plus the static
facts the core's sequencer needs per instruction: length in bytes and
M-cycles.

'decode' and 'decodeCb' are total over all 256 bytes; 'encode' and
'encodeCb' are their inverses. The tests check @encode . decode == id@ for
every byte, @decode . encode == id@ for every 'Instr' and 'CbInstr', and
'instrLength' / 'instrCycles' / 'cbCycles' against the reference model's
bus traces for every opcode.
-}
module Gb.Decode
  ( decode
  , encode
  , decodeCb
  , encodeCb
  , instrLength
  , instrCycles
  , cbCycles
  ) where

import Clash.Prelude

import Gb.Types

-- | x = bits 7-6, y = bits 5-3, z = bits 2-0, p = bits 5-4, q = bit 3.
decode :: BitVector 8 -> Instr
decode op = case x of
  0 -> case z of
    0 -> case y of
      0 -> Nop
      1 -> LdNnSp
      2 -> Stop
      3 -> Jr
      _ -> JrCc (unpack (slice d4 d3 op))
    1 -> if q == 0 then LdRpNn (unpack p) else AddHlRp (unpack p)
    2 -> if q == 0 then LdMemA (unpack p) else LdAMem (unpack p)
    3 -> if q == 0 then IncRp (unpack p) else DecRp (unpack p)
    4 -> Inc (unpack y)
    5 -> Dec (unpack y)
    6 -> LdRN (unpack y)
    _ -> Acc (unpack y)
  1 -> if op == 0x76 then Halt else LdRR (unpack y) (unpack z)
  2 -> Alu (unpack y) (unpack z)
  _ -> case z of
    0 -> case y of
      4 -> LdhNA
      5 -> AddSpE
      6 -> LdhAN
      7 -> LdHlSpE
      _ -> RetCc (unpack (slice d4 d3 op))
    1 -> if q == 0 then Pop (unpack p) else case p of
      0 -> Ret
      1 -> Reti
      2 -> JpHl
      _ -> LdSpHl
    2 -> case y of
      4 -> LdhCA
      5 -> LdNnA
      6 -> LdhAC
      7 -> LdANn
      _ -> JpCc (unpack (slice d4 d3 op))
    3 -> case y of
      0 -> Jp
      1 -> PrefixCb
      2 -> Ill IllD3
      3 -> Ill IllDB
      4 -> Ill IllE3
      5 -> Ill IllEB
      6 -> Di
      _ -> Ei
    4 -> case y of
      4 -> Ill IllE4
      5 -> Ill IllEC
      6 -> Ill IllF4
      7 -> Ill IllFC
      _ -> CallCc (unpack (slice d4 d3 op))
    5 -> if q == 0 then Push (unpack p) else case p of
      0 -> Call
      1 -> Ill IllDD
      2 -> Ill IllED
      _ -> Ill IllFD
    6 -> AluN (unpack y)
    _ -> Rst (unpack y)
 where
  x = slice d7 d6 op
  y = slice d5 d3 op
  z = slice d2 d0 op
  p = slice d5 d4 op
  q = slice d3 d3 op

-- | Opcode from x, y, z.
xyz :: BitVector 2 -> BitVector 3 -> BitVector 3 -> BitVector 8
xyz x y z = x ++# y ++# z

-- | Opcode from x, p, q, z.
xpqz :: BitVector 2 -> BitVector 2 -> BitVector 1 -> BitVector 3 -> BitVector 8
xpqz x p q z = x ++# p ++# q ++# z

encode :: Instr -> BitVector 8
encode i = case i of
  Nop -> 0x00
  LdNnSp -> 0x08
  Stop -> 0x10
  Jr -> 0x18
  JrCc c -> xyz 0 (0b100 .|. zeroExtend (pack c)) 0
  LdRpNn r -> xpqz 0 (pack r) 0 1
  AddHlRp r -> xpqz 0 (pack r) 1 1
  LdMemA r -> xpqz 0 (pack r) 0 2
  LdAMem r -> xpqz 0 (pack r) 1 2
  IncRp r -> xpqz 0 (pack r) 0 3
  DecRp r -> xpqz 0 (pack r) 1 3
  Inc r -> xyz 0 (pack r) 4
  Dec r -> xyz 0 (pack r) 5
  LdRN r -> xyz 0 (pack r) 6
  Acc o -> xyz 0 (pack o) 7
  Halt -> 0x76
  LdRR d s -> xyz 1 (pack d) (pack s)
  Alu o r -> xyz 2 (pack o) (pack r)
  RetCc c -> xyz 3 (zeroExtend (pack c)) 0
  LdhNA -> 0xE0
  AddSpE -> 0xE8
  LdhAN -> 0xF0
  LdHlSpE -> 0xF8
  Pop r -> xpqz 3 (pack r) 0 1
  Ret -> 0xC9
  Reti -> 0xD9
  JpHl -> 0xE9
  LdSpHl -> 0xF9
  JpCc c -> xyz 3 (zeroExtend (pack c)) 2
  LdhCA -> 0xE2
  LdNnA -> 0xEA
  LdhAC -> 0xF2
  LdANn -> 0xFA
  Jp -> 0xC3
  PrefixCb -> 0xCB
  Di -> 0xF3
  Ei -> 0xFB
  CallCc c -> xyz 3 (zeroExtend (pack c)) 4
  Push r -> xpqz 3 (pack r) 0 5
  Call -> 0xCD
  AluN o -> xyz 3 (pack o) 6
  Rst n -> xyz 3 (pack n) 7
  Ill o -> case o of
    IllD3 -> 0xD3
    IllDB -> 0xDB
    IllDD -> 0xDD
    IllE3 -> 0xE3
    IllE4 -> 0xE4
    IllEB -> 0xEB
    IllEC -> 0xEC
    IllED -> 0xED
    IllF4 -> 0xF4
    IllFC -> 0xFC
    IllFD -> 0xFD

-- | The byte after a CB prefix.
decodeCb :: BitVector 8 -> CbInstr
decodeCb op = case slice d7 d6 op of
  0 -> Rot (unpack y) r
  1 -> Bit (unpack y) r
  2 -> Res (unpack y) r
  _ -> Set (unpack y) r
 where
  y = slice d5 d3 op
  r = unpack (slice d2 d0 op)

encodeCb :: CbInstr -> BitVector 8
encodeCb c = case c of
  Rot o r -> xyz 0 (pack o) (pack r)
  Bit n r -> xyz 1 (pack n) (pack r)
  Res n r -> xyz 2 (pack n) (pack r)
  Set n r -> xyz 3 (pack n) (pack r)

-- | Bytes, opcode included (CB: 2, the prefix and its opcode). STOP is 2
-- bytes here: the spec skips the byte after it.
instrLength :: Instr -> Index 4
instrLength i = case i of
  LdNnSp -> 3
  Stop -> 2
  Jr -> 2
  JrCc _ -> 2
  LdRpNn _ -> 3
  LdRN _ -> 2
  LdhNA -> 2
  AddSpE -> 2
  LdhAN -> 2
  LdHlSpE -> 2
  JpCc _ -> 3
  LdNnA -> 3
  LdANn -> 3
  Jp -> 3
  PrefixCb -> 2
  CallCc _ -> 3
  Call -> 3
  AluN _ -> 2
  _ -> 1

-- | M-cycles, the opcode fetch included: (condition false, condition true);
-- the same twice for unconditional instructions. 'PrefixCb' counts only
-- the prefix's own fetch; 'cbCycles' gives the rest.
instrCycles :: Instr -> (Index 7, Index 7)
instrCycles i = case i of
  LdNnSp -> both 5
  Stop -> both 2
  Jr -> both 3
  JrCc _ -> (2, 3)
  LdRpNn _ -> both 3
  AddHlRp _ -> both 2
  LdMemA _ -> both 2
  LdAMem _ -> both 2
  IncRp _ -> both 2
  DecRp _ -> both 2
  Inc r -> both (if r == HLInd then 3 else 1)
  Dec r -> both (if r == HLInd then 3 else 1)
  LdRN r -> both (if r == HLInd then 3 else 2)
  LdRR d s -> both (if d == HLInd || s == HLInd then 2 else 1)
  Alu _ r -> both (if r == HLInd then 2 else 1)
  RetCc _ -> (2, 5)
  LdhNA -> both 3
  AddSpE -> both 4
  LdhAN -> both 3
  LdHlSpE -> both 3
  Pop _ -> both 3
  Ret -> both 4
  Reti -> both 4
  LdSpHl -> both 2
  JpCc _ -> (3, 4)
  LdhCA -> both 2
  LdNnA -> both 4
  LdhAC -> both 2
  LdANn -> both 4
  Jp -> both 4
  CallCc _ -> (3, 6)
  Push _ -> both 4
  Call -> both 6
  AluN _ -> both 2
  Rst _ -> both 4
  _ -> both 1 -- NOP, accumulator ops, HALT, JP HL, CB prefix, DI, EI, illegal
 where
  both n = (n, n)

-- | M-cycles of a CB instruction after the prefix: its opcode fetch, plus
-- the (HL) read and write.
cbCycles :: CbInstr -> Index 4
cbCycles c = case c of
  Bit _ HLInd -> 2
  Bit _ _ -> 1
  Rot _ HLInd -> 3
  Res _ HLInd -> 3
  Set _ HLInd -> 3
  _ -> 1
