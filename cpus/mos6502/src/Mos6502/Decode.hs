{-|
The 6502 opcode table (PLAN.md step 3.4): every one of the 256 opcodes,
the undocumented ones included, as an 'Instr'. The layout follows the
opcode's bit groups @aaabbbcc@ (nesdev wiki, "CPU unofficial opcodes",
whose table this matches):

* @cc = 01@: the ALU group (ORA AND EOR ADC STA LDA CMP SBC), modes from
  @bbb@ ((zp,X) zp # abs (zp),Y zp,X abs,Y abs,X); STA # is NOP #.
* @cc = 10@: shifts, STX/LDX, DEC/INC with modes from @bbb@ (X indexing
  becomes Y for STX/LDX), the transfers and DEX/NOP at @bbb = 010@ and
  @110@, and the JAMs at @x2@.
* @cc = 00@: control, flags, BIT, STY/LDY/CPY/CPX, branches and NOPs.
* @cc = 11@: the undocumented combinations of the other two groups.
-}
module Mos6502.Decode
  ( decode
  ) where

import Clash.Prelude

import Mos6502.Types

decode :: Unsigned 8 -> Instr
decode op = case cc of
  1 -> group01
  2 -> group10
  3 -> group11
  _ -> group00
 where
  aaa = slice d7 d5 op
  bbb = slice d4 d2 op
  cc = slice d1 d0 op :: BitVector 2

  mode01 = case bbb of
    0 -> IndX
    1 -> Zp
    2 -> Imm
    3 -> Abs
    4 -> IndY
    5 -> ZpX
    6 -> AbsY
    _ -> AbsX

  group01 = case aaa of
    0 -> Instr ORA mode01 KRead
    1 -> Instr AND mode01 KRead
    2 -> Instr EOR mode01 KRead
    3 -> Instr ADC mode01 KRead
    4 | bbb == 2 -> Instr NOP Imm KRead
      | otherwise -> Instr STA mode01 KWrite
    5 -> Instr LDA mode01 KRead
    6 -> Instr CMP mode01 KRead
    _ -> Instr SBC mode01 KRead

  -- STX/LDX (and SAX/LAX) index with Y.
  xy = aaa == 4 || aaa == 5
  mode10 = case bbb of
    1 -> Zp
    3 -> Abs
    5 -> if xy then ZpY else ZpX
    _ -> if xy then AbsY else AbsX

  group10 = case bbb of
    0 | aaa == 5 -> Instr LDX Imm KRead
      | aaa >= 4 -> Instr NOP Imm KRead      -- 82 C2 E2
      | otherwise -> Instr JAM Jam KRead
    4 -> Instr JAM Jam KRead
    2 -> case aaa of
      0 -> Instr ASL Imp KRead
      1 -> Instr ROL Imp KRead
      2 -> Instr LSR Imp KRead
      3 -> Instr ROR Imp KRead
      4 -> Instr TXA Imp KRead
      5 -> Instr TAX Imp KRead
      6 -> Instr DEX Imp KRead
      _ -> Instr NOP Imp KRead
    6 -> case aaa of
      4 -> Instr TXS Imp KRead
      5 -> Instr TSX Imp KRead
      _ -> Instr NOP Imp KRead               -- 1A 3A 5A 7A DA FA
    _ -> case aaa of
      0 -> Instr ASL mode10 KRmw
      1 -> Instr ROL mode10 KRmw
      2 -> Instr LSR mode10 KRmw
      3 -> Instr ROR mode10 KRmw
      4 | op == 0x9E -> Instr SHX AbsY KWrite
        | otherwise -> Instr STX mode10 KWrite
      5 -> Instr LDX mode10 KRead
      6 -> Instr DEC mode10 KRmw
      _ -> Instr INC mode10 KRmw

  group00 = case op of
    0x00 -> Instr BRK Brk KRead
    0x20 -> Instr JSR Jsr KRead
    0x40 -> Instr RTI Rti KRead
    0x60 -> Instr RTS Rts KRead
    0x08 -> Instr PHP Push KWrite
    0x28 -> Instr PLP Pull KRead
    0x48 -> Instr PHA Push KWrite
    0x68 -> Instr PLA Pull KRead
    0x4C -> Instr JMP JmpAbs KRead
    0x6C -> Instr JMP JmpInd KRead
    0x24 -> Instr BIT Zp KRead
    0x2C -> Instr BIT Abs KRead
    0x84 -> Instr STY Zp KWrite
    0x8C -> Instr STY Abs KWrite
    0x94 -> Instr STY ZpX KWrite
    0x9C -> Instr SHY AbsX KWrite
    0xA0 -> Instr LDY Imm KRead
    0xA4 -> Instr LDY Zp KRead
    0xAC -> Instr LDY Abs KRead
    0xB4 -> Instr LDY ZpX KRead
    0xBC -> Instr LDY AbsX KRead
    0xC0 -> Instr CPY Imm KRead
    0xC4 -> Instr CPY Zp KRead
    0xCC -> Instr CPY Abs KRead
    0xE0 -> Instr CPX Imm KRead
    0xE4 -> Instr CPX Zp KRead
    0xEC -> Instr CPX Abs KRead
    0x88 -> Instr DEY Imp KRead
    0xC8 -> Instr INY Imp KRead
    0xE8 -> Instr INX Imp KRead
    0x98 -> Instr TYA Imp KRead
    0xA8 -> Instr TAY Imp KRead
    0x18 -> Instr CLC Imp KRead
    0x38 -> Instr SEC Imp KRead
    0x58 -> Instr CLI Imp KRead
    0x78 -> Instr SEI Imp KRead
    0xB8 -> Instr CLV Imp KRead
    0xD8 -> Instr CLD Imp KRead
    0xF8 -> Instr SED Imp KRead
    0x10 -> Instr BPL Rel KRead
    0x30 -> Instr BMI Rel KRead
    0x50 -> Instr BVC Rel KRead
    0x70 -> Instr BVS Rel KRead
    0x90 -> Instr BCC Rel KRead
    0xB0 -> Instr BCS Rel KRead
    0xD0 -> Instr BNE Rel KRead
    0xF0 -> Instr BEQ Rel KRead
    _ -> case bbb of                          -- the undocumented NOPs
      0 -> Instr NOP Imm KRead                -- 80
      1 -> Instr NOP Zp KRead                 -- 04 44 64
      3 -> Instr NOP Abs KRead                -- 0C
      5 -> Instr NOP ZpX KRead                -- 14 34 54 74 D4 F4
      _ -> Instr NOP AbsX KRead               -- 1C 3C 5C 7C DC FC

  group11 = case op of
    0x0B -> Instr ANC Imm KRead
    0x2B -> Instr ANC Imm KRead
    0x4B -> Instr ALR Imm KRead
    0x6B -> Instr ARR Imm KRead
    0x8B -> Instr ANE Imm KRead
    0xAB -> Instr LXA Imm KRead
    0xCB -> Instr AXS Imm KRead
    0xEB -> Instr SBC Imm KRead
    0x9B -> Instr TAS AbsY KWrite
    0x93 -> Instr SHA IndY KWrite
    0x9F -> Instr SHA AbsY KWrite
    0xBB -> Instr LAS AbsY KRead
    _ -> case aaa of
      0 -> Instr SLO mode01 KRmw
      1 -> Instr RLA mode01 KRmw
      2 -> Instr SRE mode01 KRmw
      3 -> Instr RRA mode01 KRmw
      4 -> Instr SAX m11 KWrite
      5 -> Instr LAX m11 KRead
      6 -> Instr DCP mode01 KRmw
      _ -> Instr ISC mode01 KRmw
  -- SAX and LAX index zero page with Y, and LAX absolute with Y.
  m11 = case mode01 of
    ZpX -> ZpY
    AbsX | aaa == 5 -> AbsY
    m -> m
