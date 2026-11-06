{-|
The Game Gear's and Master System's state table (side track N, PLAN.md
N.1): what a save state keeps besides the memories, in one neutral byte
layout, the Game Boy's way ("Gb.StateTable"). The spec fills it
("Gg.Spec".'specTable'), the core's read window from its record (N.2),
and the PS turns it into the restore routine that loads a state through
the Z80 (@platform/ps/systems/gg_restore.zig@, which has the same offsets; the
golden @systems/gg/test/golden/state-table.txt@ pins them for both).

The memories (RAM 8 KiB, VRAM 16 KiB, cartridge RAM 32 KiB) are not in
it: they go through the PS's windows. CRAM is, as the routine writes it
back through the VDP's data port.

Values are what the hardware holds: the registers as written, and the
hidden counters the routine lines the machine up with (the T-states
since power-on modulo two frames, which sets the VDP's position and the
PSG's clock).
-}
module Gg.StateTable
  ( tableSize
  , tableFields
  , tPhaseFrames
    -- * Offsets
  , tA, tF, tB, tC, tD, tE, tH, tL, tAF', tBC', tDE', tHL', tIX, tIY, tSP, tPC
  , tI, tR, tIFF1, tIFF2, tIM, tFlags, tWZ, tTime, tHalted
  , tMap, tCmRam, tMemCtl, tIoCtl, tRamTail, tGgRegs, tBiosMap
  , tVdpRegs, tVdpAddr, tVdpCode, tVdpLatched, tVdpLatch, tVdpBuf, tVdpCramLatch
  , tVdpStatus, tVdpLinePend, tVdpLineCnt, tVdpHLatch
  , tPsgPeriod, tPsgNoise, tPsgVol, tPsgLatch, tPsgLfsr
  , tFmCtl, tFmRegs
  , tCram
  ) where

import Prelude

-- | Bytes in the table.
tableSize :: Int
tableSize = 0x140

-- | The T-states the table's time ('tTime') counts modulo: two frames of
-- 59736 (262 lines of 228), as the PSG ticks every 16 and a frame isn't
-- a multiple of 16.
tPhaseFrames :: Int
tPhaseFrames = 2 * 228 * 262

tA, tF, tB, tC, tD, tE, tH, tL, tAF', tBC', tDE', tHL', tIX, tIY, tSP, tPC :: Int
tA = 0x00
tF = 0x01
tB = 0x02
tC = 0x03
tD = 0x04
tE = 0x05
tH = 0x06
tL = 0x07
tAF' = 0x08    -- 2 bytes, low first (as every 16-bit field): F' then A'
tBC' = 0x0A
tDE' = 0x0C
tHL' = 0x0E
tIX = 0x10
tIY = 0x12
tSP = 0x14
tPC = 0x16     -- the next instruction's address

tI, tR, tIFF1, tIFF2, tIM, tFlags, tWZ, tTime, tHalted :: Int
tI = 0x18
tR = 0x19
tIFF1 = 0x1A
tIFF2 = 0x1B
tIM = 0x1C
tFlags = 0x1D  -- bit 0 Master System mode, 1 the Master System II's hardware, 2 the Codemasters mapper, 3 a BIOS, 4 an FM unit
tWZ = 0x1E     -- 2: MEMPTR (approximate: the routine's JP sets it)
tTime = 0x20   -- 3: T-states since power-on modulo 'tPhaseFrames'
tHalted = 0x23 -- 1: the CPU is halted (on a HALT at PC - 1)

tMap, tCmRam, tMemCtl, tIoCtl, tRamTail, tGgRegs, tBiosMap :: Int
tMap = 0x24    -- 4: the Sega mapper's FFFCh-FFFFh (Codemasters: slots 0-2's banks in 1-3)
tCmRam = 0x28  -- Codemasters: cartridge RAM at A000h-BFFFh
tMemCtl = 0x29 -- port 3Eh
tIoCtl = 0x2A  -- port 3Fh
tRamTail = 0x2C -- 4: RAM's DFFCh-DFFFh (the routine's mapper writes land there too: written back after; the PS fills it from the slot's RAM)
tGgRegs = 0x30 -- 7: the Game Gear's ports 00h-06h
tBiosMap = 0x38 -- 3: the Master System BIOS's banks

tVdpRegs, tVdpAddr, tVdpCode, tVdpLatched, tVdpLatch, tVdpBuf, tVdpCramLatch :: Int
tVdpRegs = 0x40 -- 11: R0-R10
tVdpAddr = 0x4B -- 2
tVdpCode = 0x4D
tVdpLatched = 0x4E -- 1: the control port holds a first byte
tVdpLatch = 0x4F
tVdpBuf = 0x50     -- the read buffer
tVdpCramLatch = 0x51 -- the Game Gear's even CRAM byte

tVdpStatus, tVdpLinePend, tVdpLineCnt, tVdpHLatch :: Int
tVdpStatus = 0x52  -- bits 7-5
tVdpLinePend = 0x53
tVdpLineCnt = 0x54
tVdpHLatch = 0x55

tPsgPeriod, tPsgNoise, tPsgVol, tPsgLatch, tPsgLfsr :: Int
tPsgPeriod = 0x60  -- 3 × 2: the tone periods
tPsgNoise = 0x66
tPsgVol = 0x67     -- 4: attenuations
tPsgLatch = 0x6B   -- the latched register (0-7)
tPsgLfsr = 0x6C    -- 2

tFmCtl, tFmRegs :: Int
tFmCtl = 0x70      -- port F2h
tFmRegs = 0x80     -- 64: the YM2413's registers

tCram :: Int
tCram = 0x100      -- 64: CRAM as the VDP keeps it (----BBBBGGGGRRRR, low byte first)

-- | Every field with its offset and size, in offset order (the golden).
tableFields :: [(String, Int, Int)]
tableFields =
  [ ("A", tA, 1), ("F", tF, 1), ("B", tB, 1), ("C", tC, 1), ("D", tD, 1), ("E", tE, 1)
  , ("H", tH, 1), ("L", tL, 1), ("AF'", tAF', 2), ("BC'", tBC', 2), ("DE'", tDE', 2), ("HL'", tHL', 2)
  , ("IX", tIX, 2), ("IY", tIY, 2), ("SP", tSP, 2), ("PC", tPC, 2)
  , ("I", tI, 1), ("R", tR, 1), ("IFF1", tIFF1, 1), ("IFF2", tIFF2, 1), ("IM", tIM, 1)
  , ("FLAGS", tFlags, 1), ("WZ", tWZ, 2), ("TIME", tTime, 3), ("HALTED", tHalted, 1)
  , ("MAP", tMap, 4), ("CMRAM", tCmRam, 1), ("MEMCTL", tMemCtl, 1), ("IOCTL", tIoCtl, 1), ("RAMTAIL", tRamTail, 4)
  , ("GGREGS", tGgRegs, 7), ("BIOSMAP", tBiosMap, 3)
  , ("VDPREGS", tVdpRegs, 11), ("VDPADDR", tVdpAddr, 2), ("VDPCODE", tVdpCode, 1)
  , ("VDPLATCHED", tVdpLatched, 1), ("VDPLATCH", tVdpLatch, 1), ("VDPBUF", tVdpBuf, 1)
  , ("VDPCRAMLATCH", tVdpCramLatch, 1), ("VDPSTATUS", tVdpStatus, 1), ("VDPLINEPEND", tVdpLinePend, 1)
  , ("VDPLINECNT", tVdpLineCnt, 1), ("VDPHLATCH", tVdpHLatch, 1)
  , ("PSGPERIOD", tPsgPeriod, 6), ("PSGNOISE", tPsgNoise, 1), ("PSGVOL", tPsgVol, 4)
  , ("PSGLATCH", tPsgLatch, 1), ("PSGLFSR", tPsgLfsr, 2)
  , ("FMCTL", tFmCtl, 1), ("FMREGS", tFmRegs, 64)
  , ("CRAM", tCram, 64) ]
