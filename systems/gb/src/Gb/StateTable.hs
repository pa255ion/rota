{-|
The Game Boy's state table (side track L, PLAN.md L.6c): what a save
state keeps besides the memories, in one neutral byte layout. The spec
fills it from its machine ("Gb.Spec.SaveState".'specTable'), the core's
read window from its record (L.6c.2), and the PS turns it into the
restore routine that loads a state through the CPU
(@platform/ps/gb_restore.zig@, which has the same offsets; the golden
@systems/gb/test/golden/state-table.txt@ pins them for both).

The memories (VRAM, WRAM, HRAM, cartridge RAM) are not in it: they go
through the block RAMs' ports, as in L.6. OAM is, as the routine puts it
back by OAM DMA from the overlay.

Values are what the hardware holds, not what a CPU read returns: the I/O
registers as written (unused bits as the register keeps them), and the
hidden counters the routine needs to line the machine up (the system
counter behind DIV, the PPU's line and dot, the sound channels' length
timers and envelope volumes).
-}
module Gb.StateTable
  ( tableSize
  , tableFields
    -- * Offsets
  , tA, tF, tB, tC, tD, tE, tH, tL, tSp, tPc, tIme, tModel, tMbc
  , tDiv, tTima, tTma, tTac, tIf, tIe, tSb, tSc, tP1
  , tLy, tDot, tLcdc, tStat, tScy, tScx, tLyc, tBgp, tObp0, tObp1, tWy, tWx, tDma
  , tRomb, tBank2, tRamen, tMode, tRtcLatch, tRtc, tRtcL
  , tNr, tNr52, tChOn, tChLen, tChVol, tWave
  , tKey1, tVbk, tSvbk, tBcps, tOcps, tOpri, tFf72, tFf73, tFf74, tFf75, tRp, tHdmaSrc, tHdmaDst
  , tOam, tPal
  ) where

import Prelude

-- | Bytes in the table (the DMG's and the Game Boy Color's).
tableSize :: Int
tableSize = 0x220

tA, tF, tB, tC, tD, tE, tH, tL, tSp, tPc, tIme, tModel, tMbc :: Int
tA = 0x00
tF = 0x01
tB = 0x02
tC = 0x03
tD = 0x04
tE = 0x05
tH = 0x06
tL = 0x07
tSp = 0x08     -- 2 bytes, low first (as every 16-bit field)
tPc = 0x0A     -- 2: the next instruction's address
tIme = 0x0C    -- 1 if interrupts are enabled
tModel = 0x0D  -- bit 0 the Game Boy Color, bit 1 its CGB mode
tMbc = 0x0E    -- 0 none, 1 MBC1, 2 MBC2, 3 MBC3, 4 MBC3 with the clock, 5 MBC5

tDiv, tTima, tTma, tTac, tIf, tIe, tSb, tSc, tP1 :: Int
tDiv = 0x10    -- 2: the 16-bit system counter (DIV is its high byte)
tTima = 0x12
tTma = 0x13
tTac = 0x14    -- low 3 bits
tIf = 0x15     -- low 5 bits
tIe = 0x16
tSb = 0x17
tSc = 0x18     -- bits 7 and 0
tP1 = 0x19     -- the select bits 4 and 5

tLy, tDot, tLcdc, tStat, tScy, tScx, tLyc, tBgp, tObp0, tObp1, tWy, tWx, tDma :: Int
tLy = 0x1A     -- the PPU's line, 0 to 153 (0 with the LCD off)
tDot = 0x1B    -- 2: the next dot of the line, 0 to 455
tLcdc = 0x1D
tStat = 0x1E   -- the writable bits 3 to 6
tScy = 0x1F
tScx = 0x20
tLyc = 0x21
tBgp = 0x22
tObp0 = 0x23
tObp1 = 0x24
tWy = 0x25
tWx = 0x26
tDma = 0x27    -- the last value written to 0xFF46

tRomb, tBank2, tRamen, tMode, tRtcLatch, tRtc, tRtcL :: Int
tRomb = 0x28   -- 2: the ROM bank register (MBC1's BANK1; MBC5's 9 bits)
tBank2 = 0x2A  -- MBC1's BANK2; MBC3's RAM bank or clock register; MBC5's RAM bank
tRamen = 0x2B  -- 1 if cartridge RAM is enabled
tMode = 0x2C   -- MBC1's mode
tRtcLatch = 0x2D -- the last byte written to 0x6000-0x7FFF (MBC3)
tRtc = 0x2E    -- 5: the clock's live registers S, M, H, DL, DH
tRtcL = 0x33   -- 5: the latched ones (what reads return)

tNr, tNr52, tChOn, tChLen, tChVol, tWave :: Int
tNr = 0x40     -- 22: NR10 to NR51 (0xFF10-0xFF25) as written
tNr52 = 0x56   -- bit 7: the sound unit is on
tChOn = 0x57   -- bits 0 to 3: the channels that are on
tChLen = 0x58  -- 4 × 2: each channel's length timer (clocks left, up to 256)
tChVol = 0x60  -- 4: each channel's envelope volume (the third's: 0)
tWave = 0x70   -- 16: wave RAM

-- the Game Boy Color (L.6c.4; 0 on the DMG)
tKey1, tVbk, tSvbk, tBcps, tOcps, tOpri, tFf72, tFf73, tFf74, tFf75, tRp, tHdmaSrc, tHdmaDst :: Int
tKey1 = 0x80   -- bit 7 double speed, bit 0 armed
tVbk = 0x81    -- VRAM bank (bit 0)
tSvbk = 0x82   -- WRAM bank at 0xD000 (bits 0 to 2)
tBcps = 0x83   -- palette indexes and auto-increment
tOcps = 0x84
tOpri = 0x85   -- 1: objects by X (DMG), 0: by OAM order
tFf72 = 0x86
tFf73 = 0x87
tFf74 = 0x88
tFf75 = 0x89
tRp = 0x8A     -- the infrared port's written bits
tHdmaSrc = 0x8B -- 2: VRAM DMA's source
tHdmaDst = 0x8D -- 2: its destination in VRAM

tOam, tPal :: Int
tOam = 0x100   -- 160: OAM
tPal = 0x1A0   -- 128: palette RAM, 64 bytes BG, then 64 OBJ

-- | Every field with its offset and size, in offset order (the golden;
-- what the Zig side checks against).
tableFields :: [(String, Int, Int)]
tableFields =
  [ ("A", tA, 1), ("F", tF, 1), ("B", tB, 1), ("C", tC, 1), ("D", tD, 1), ("E", tE, 1)
  , ("H", tH, 1), ("L", tL, 1), ("SP", tSp, 2), ("PC", tPc, 2), ("IME", tIme, 1)
  , ("MODEL", tModel, 1), ("MBC", tMbc, 1)
  , ("DIV", tDiv, 2), ("TIMA", tTima, 1), ("TMA", tTma, 1), ("TAC", tTac, 1)
  , ("IF", tIf, 1), ("IE", tIe, 1), ("SB", tSb, 1), ("SC", tSc, 1), ("P1", tP1, 1)
  , ("LY", tLy, 1), ("DOT", tDot, 2), ("LCDC", tLcdc, 1), ("STAT", tStat, 1)
  , ("SCY", tScy, 1), ("SCX", tScx, 1), ("LYC", tLyc, 1), ("BGP", tBgp, 1)
  , ("OBP0", tObp0, 1), ("OBP1", tObp1, 1), ("WY", tWy, 1), ("WX", tWx, 1), ("DMA", tDma, 1)
  , ("ROMB", tRomb, 2), ("BANK2", tBank2, 1), ("RAMEN", tRamen, 1), ("MODE", tMode, 1)
  , ("RTCLATCH", tRtcLatch, 1), ("RTC", tRtc, 5), ("RTCL", tRtcL, 5)
  , ("NR", tNr, 22), ("NR52", tNr52, 1), ("CHON", tChOn, 1), ("CHLEN", tChLen, 8)
  , ("CHVOL", tChVol, 4), ("WAVE", tWave, 16)
  , ("KEY1", tKey1, 1), ("VBK", tVbk, 1), ("SVBK", tSvbk, 1), ("BCPS", tBcps, 1), ("OCPS", tOcps, 1)
  , ("OPRI", tOpri, 1), ("FF72", tFf72, 1), ("FF73", tFf73, 1), ("FF74", tFf74, 1), ("FF75", tFf75, 1)
  , ("RP", tRp, 1), ("HDMASRC", tHdmaSrc, 2), ("HDMADST", tHdmaDst, 2)
  , ("OAM", tOam, 160), ("PAL", tPal, 128) ]
