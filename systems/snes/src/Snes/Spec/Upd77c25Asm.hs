{-|
A small µPD77C25 assembler (M7 step 7.16; fullsnes's encodings, as
"Snes.Spec.Upd77c25" decodes them): the tests' own programs and our DSP-1
test cartridge's firmware ('dspTestFirmware', @snes-dspfw@), never the
user's dump.
-}
module Snes.Spec.Upd77c25Asm
  ( -- * Instructions
    aluI, mov, op, ld, jp
    -- * SRC and DST
  , sTrb, sA, sB, sTr, sDp, sRp, sRo, sSgn, sDr, sDrnf, sSr, sK, sL, sMem
  , dNon, dA, dB, dTr, dDp, dRp, dDr, dSr, dK, dKlr, dKlm, dL, dTrb, dMem
    -- * ALU codes
  , aOr, aAnd, aXor, aSub, aAdd, aSbb, aAdc, aDec, aInc, aNot, aSar1, aRcl1, aSll2, aSll4, aXchg
    -- * Jumps
  , jJmp, jCall, jJrqm, jJza
    -- * Files
  , firmwareFile
  , dspTestFirmware
  ) where

import Prelude

import Data.Bits
import qualified Data.ByteString as B
import Data.Word

-- | An ALU instruction: RT, P (0 RAM, 1 IDB, 2 M, 3 N), the ALU code, B
-- (else A), DPL, DPH, RP decrement, SRC, DST.
aluI :: Bool -> Word32 -> Word32 -> Bool -> Word32 -> Word32 -> Bool -> Word32 -> Word32 -> Word32
aluI rt p code useB dpl dph rpd src dst =
  (if rt then bit 22 else 0) .|. p `shiftL` 20 .|. code `shiftL` 16 .|. (if useB then bit 15 else 0)
  .|. dpl `shiftL` 13 .|. dph `shiftL` 9 .|. (if rpd then bit 8 else 0) .|. src `shiftL` 4 .|. dst

-- | A move SRC → DST (an ALU instruction with NOP).
mov :: Word32 -> Word32 -> Word32
mov = aluI False 0 0 False 0 0 False

-- | The ALU on A (or B) with P = IDB from SRC, no move.
op :: Word32 -> Bool -> Word32 -> Word32
op code useB src = aluI False 1 code useB 0 0 False src 0

-- | LD: an immediate into DST.
ld :: Word16 -> Word32 -> Word32
ld v dst = bit 23 .|. bit 22 .|. fromIntegral v `shiftL` 6 .|. dst

-- | JP: a jump or call (BRCH) to an address.
jp :: Word32 -> Word32 -> Word32
jp brch na = bit 23 .|. brch `shiftL` 13 .|. na `shiftL` 2

sTrb, sA, sB, sTr, sDp, sRp, sRo, sSgn, sDr, sDrnf, sSr, sK, sL, sMem :: Word32
sTrb = 0; sA = 1; sB = 2; sTr = 3; sDp = 4; sRp = 5; sRo = 6; sSgn = 7; sDr = 8; sDrnf = 9; sSr = 10; sK = 13; sL = 14; sMem = 15
dNon, dA, dB, dTr, dDp, dRp, dDr, dSr, dK, dKlr, dKlm, dL, dTrb, dMem :: Word32
dNon = 0; dA = 1; dB = 2; dTr = 3; dDp = 4; dRp = 5; dDr = 6; dSr = 7; dK = 10; dKlr = 11; dKlm = 12; dL = 13; dTrb = 14; dMem = 15

aOr, aAnd, aXor, aSub, aAdd, aSbb, aAdc, aDec, aInc, aNot, aSar1, aRcl1, aSll2, aSll4, aXchg :: Word32
aOr = 1; aAnd = 2; aXor = 3; aSub = 4; aAdd = 5; aSbb = 6; aAdc = 7; aDec = 8; aInc = 9; aNot = 10; aSar1 = 11; aRcl1 = 12; aSll2 = 13; aSll4 = 14; aXchg = 15

jJmp, jCall, jJrqm, jJza :: Word32
jJmp = 0x100; jCall = 0x140; jJrqm = 0x0BE; jJza = 0x08A

-- | An 8 KiB firmware file ("newer" dumps' form: the program's 2,048
-- instructions little-endian, 3 bytes each, then the data ROM's 1,024
-- words), the rest of each zero.
firmwareFile :: [Word32] -> [Word16] -> B.ByteString
firmwareFile prog dat = B.pack (concatMap op3 (take 2048 (prog <> repeat 0)) <> concatMap w2 (take 1024 (dat <> repeat 0)))
 where
  op3 w = map (\s -> fromIntegral (w `shiftR` s)) [0, 8, 16]
  w2 w = map (\s -> fromIntegral (w `shiftR` s)) [0, 8]

-- | Our DSP-1 test cartridge's firmware (systems/snes/test/asm/dsp1.s):
-- 16-bit words throughout. DR 80h says ready; then X (read with DR, which
-- sets RQM: the SNES may write Y), Y (with DRNF: no RQM until the first
-- answer is there), and three answers, each waited for: X + Y, the
-- multiplier's M (X × Y × 2's high word), X XOR Y; then ready again. X =
-- FFFFh instead starts the timing probe: A goes up by 100h and into SR
-- every three instructions, never waiting, so each SR read of the SNES's
-- sees how many instructions have run by its master clock (snes-core
-- compares every read).
dspTestFirmware :: B.ByteString
dspTestFirmware = firmwareFile
  [ ld 0x0080 dDr                          -- 0: ready
  , jp jJrqm 1                             -- 1: wait for X
  , mov sDr dK                             -- 2: K = X (RQM: Y may come)
  , mov sK dA                              -- 3
  , op aInc False sTrb                     -- 4: A = X + 1
  , jp jJza 21                             -- 5: X = FFFFh: the probe
  , jp jJrqm 6                             -- 6: wait for Y
  , mov sDrnf dL                           -- 7: L = Y
  , mov sK dA                              -- 8: A = X
  , op aAdd False sL                       -- 9: A = X + Y
  , mov sA dDr                             -- 10: the sum out
  , jp jJrqm 11                            -- 11
  , ld 0 dB                                -- 12
  , aluI False 2 aOr True 0 0 False 0 dNon -- 13: B = M
  , mov sB dDr                             -- 14: the product's high word out
  , jp jJrqm 15                            -- 15
  , mov sK dA                              -- 16
  , op aXor False sL                       -- 17: A = X XOR Y
  , mov sA dDr                             -- 18: out
  , jp jJrqm 19                            -- 19
  , jp jJmp 0                              -- 20: ready again
  , ld 0x0100 dTr                          -- 21: the probe
  , ld 0 dA                                -- 22
  , op aAdd False sTr                      -- 23: A += 100h
  , mov sA dSr                             -- 24: into SR (its writable bits)
  , jp jJmp 23                             -- 25
  ] []
