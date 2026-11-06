{-|
The NES's state table (side track N, PLAN.md N.7): what a save state
keeps besides the memories, in one neutral byte layout, the Game Boy's,
the Game Gear's and the 800XL's way ("Gb.StateTable", "Gg.StateTable",
"A800.StateTable"). The spec fills it ("Nes.Spec.SaveState".'specTable'),
the core's read window from its record (N.8), and the PS turns it into
the restore routine that loads a state through the 2A03
(@platform/ps/systems/nes_restore.zig@, which has the same offsets; the
golden @systems/nes/test/golden/state-table.txt@ pins them for both).

Not in it: the console's RAM (2 KiB) and nametable RAM (2 KiB), OAM (256
bytes), the cartridge's PRG RAM, CHR RAM and MMC5's ExRAM; they go
through the PS's windows. PRG and CHR ROM are the game's, loaded again
first.

Values are what the hardware holds: the registers as they act now (the
APU's as the values that set them), the PPU's position (the line and dot
after the CPU's last cycle), and the hidden state the routine puts back
or that is only compared ('loose' in the tests: the PPU's
sprite flags; the APU's length counters and DMC IRQ flag; MMC5's IRQ
pending, in-frame flag and line counter).
-}
module Nes.StateTable
  ( tableSize
  , tableFields
    -- * Offsets
  , tA, tX, tY, tS, tP, tPC
  , tLine, tDot, tOdd, tParity, tStrobe
  , tCtrl, tMask, tStatus, tOamAddr, tV, tT, tFineX, tW, tBuf, tLatch
  , tPal
  , tApu, tEnabled, tFrame, tSeq, tFrameIrq, tDmcIrq, tLengths
  , tMapper, tMirror, tBoard, tMmc1, tBank, tMmc3, tMmc5, tMmc5Line
  , tDmcCount, tDmcBits, tDmcFull, tDmcBuf, tDmcShift, tDmcSilence
  ) where

import Prelude

-- | Bytes in the table.
tableSize :: Int
tableSize = 0x100

tA, tX, tY, tS, tP, tPC :: Int
tA = 0x00
tX = 0x01
tY = 0x02
tS = 0x03
tP = 0x04
tPC = 0x05     -- 2 bytes, low first (as every 16-bit field): the next instruction's address

tLine, tDot, tOdd, tParity, tStrobe :: Int
tLine = 0x08   -- 2: the PPU's line (0-261) after the CPU's last cycle
tDot = 0x0A    -- 2: its dot (0-340)
tOdd = 0x0C    -- the frame being drawn is odd
tParity = 0x0D -- the CPU's last cycle: 1 odd (a get), 0 even (a put), counting the first after power-on as 1
tStrobe = 0x0E -- the controllers' strobe line

tCtrl, tMask, tStatus, tOamAddr, tV, tT, tFineX, tW, tBuf, tLatch :: Int
tCtrl = 0x10
tMask = 0x11
tStatus = 0x12  -- VBlank (bit 7), sprite 0 hit (6), sprite overflow (5)
tOamAddr = 0x13
tV = 0x14       -- 2
tT = 0x16       -- 2
tFineX = 0x18
tW = 0x19
tBuf = 0x1A     -- the $2007 read buffer
tLatch = 0x1B   -- the I/O latch (what reads of write-only registers, and $2002's low bits, return)

tPal :: Int
tPal = 0x20     -- 32: palette RAM ($3F10, $3F14, $3F18, $3F1C: the bytes they mirror)

tApu, tEnabled, tFrame, tSeq, tFrameIrq, tDmcIrq, tLengths :: Int
tApu = 0x40       -- 20: $4000-$4013 as the values that set the channels now (the fourth registers: period bits only; $4009, $400D, $400F: 0)
tEnabled = 0x54   -- $4015's bits 0-3
tFrame = 0x55     -- the frame counter: mode (bit 7), IRQ inhibit (bit 6)
tSeq = 0x56       -- 2: CPU cycles since the sequencer's reset
tFrameIrq = 0x58
tDmcIrq = 0x59
tLengths = 0x5A   -- 4: the length counters (pulse 1, pulse 2, triangle, noise)

tMapper, tMirror, tBoard, tMmc1, tBank, tMmc3, tMmc5, tMmc5Line :: Int
tMapper = 0x60    -- the iNES mapper number
tMirror = 0x61    -- the header's mirroring: 0 horizontal, 1 vertical, 2 four-screen
tBoard = 0x62     -- bit 0: CHR RAM, bit 1: PRG RAM
tMmc1 = 0x64      -- 6: shift register, bits in it, control, CHR 0, CHR 1, PRG
tBank = 0x6A      -- UxROM, CNROM, AxROM: the bank register
tMmc3 = 0x70      -- 16: bank select, R0-R7, mirroring, IRQ latch, counter, reload, enable, pending, PPU A12 as last seen
tMmc5 = 0x80      -- 32: $5100-$5107, $5113-$5117, $5120-$512B, the last CHR set written ($5128-: 1), $5203, $5204's enable, IRQ pending, in frame, $5205, $5206
tMmc5Line = 0xA0  -- its scanline counter

tDmcCount, tDmcBits, tDmcFull, tDmcBuf, tDmcShift, tDmcSilence :: Int
tDmcCount = 0xA8   -- 2: the DMC's timer (it counts on odd cycles)
tDmcBits = 0xAA    -- its output unit's bits left (1-8)
tDmcFull = 0xAB    -- its sample buffer holds a byte
tDmcBuf = 0xAC     -- that byte (0 when empty)
tDmcShift = 0xAD   -- the output unit's shift register
tDmcSilence = 0xAE -- its silence flag

-- | Every field: name, offset, size (for the golden and the tests' messages).
tableFields :: [(String, Int, Int)]
tableFields =
  [ ("A", tA, 1), ("X", tX, 1), ("Y", tY, 1), ("S", tS, 1), ("P", tP, 1), ("PC", tPC, 2)
  , ("LINE", tLine, 2), ("DOT", tDot, 2), ("ODD", tOdd, 1), ("PARITY", tParity, 1), ("STROBE", tStrobe, 1)
  , ("CTRL", tCtrl, 1), ("MASK", tMask, 1), ("STATUS", tStatus, 1), ("OAMADDR", tOamAddr, 1)
  , ("V", tV, 2), ("T", tT, 2), ("FINEX", tFineX, 1), ("W", tW, 1), ("BUF", tBuf, 1), ("LATCH", tLatch, 1)
  , ("PAL", tPal, 32)
  , ("APU", tApu, 20), ("ENABLED", tEnabled, 1), ("FRAME", tFrame, 1), ("SEQ", tSeq, 2)
  , ("FRAMEIRQ", tFrameIrq, 1), ("DMCIRQ", tDmcIrq, 1), ("LENGTHS", tLengths, 4)
  , ("MAPPER", tMapper, 1), ("MIRROR", tMirror, 1), ("BOARD", tBoard, 1), ("MMC1", tMmc1, 6)
  , ("BANK", tBank, 1), ("MMC3", tMmc3, 16), ("MMC5", tMmc5, 32), ("MMC5LINE", tMmc5Line, 1)
  , ("DMCCOUNT", tDmcCount, 2), ("DMCBITS", tDmcBits, 1), ("DMCFULL", tDmcFull, 1), ("DMCBUF", tDmcBuf, 1)
  , ("DMCSHIFT", tDmcShift, 1), ("DMCSILENCE", tDmcSilence, 1)
  ]
