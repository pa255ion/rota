{-|
The Atari 800XL's state table (side track N, PLAN.md N.4): what a save
state keeps besides RAM, in one neutral byte layout, the Game Boy's and
the Game Gear's way ("Gb.StateTable", "Gg.StateTable"). The spec fills it
("A800.Spec.SaveState".'specTable'), the core's read window from its
record (N.5), and the PS turns it into the restore routine that loads a
state through the 6502 (@platform/ps/systems/a800_restore.zig@, which has
the same offsets; the golden @systems/a800xl/test/golden/state-table.txt@
pins them for both).

RAM (64 KiB) is not in it: it goes through the PS's window. The ROMs,
the cartridge and the executable are the game's, loaded again first.

Values are what the hardware holds: the registers as written, ANTIC's
position (the line and cycle of the CPU's last cycle), and the hidden
state the routine puts back or that is only compared ('loose' in the
tests: POKEY's IRQST, SKSTAT and KBCODE; GTIA's collisions and trigger
latch; ANTIC's light pen, previous instruction, memory scan counter and
hires flag).
-}
module A800.StateTable
  ( tableSize
  , tableFields
    -- * Offsets
  , tA, tX, tY, tS, tP, tPC
  , tLine, tCycle, tNmist, tDmactl, tChactl, tDlist, tHscrol, tVscrol, tPmbase, tChbase, tNmien
  , tPenh, tPenv, tIrPrev, tMsc, tHires, tLoader, tXexPos
  , tPactl, tPbctl, tOra, tDdra, tOrb, tDdrb, tEdges, tCb2
  , tAudf, tAudc, tAudctl, tSkctl, tIrqen, tIrqst, tSkstat, tKbcode, tSerout, tSerial
  , tGtia, tLatch, tColl, tTrigLatch
  ) where

import Prelude

-- | Bytes in the table.
tableSize :: Int
tableSize = 0x80

tA, tX, tY, tS, tP, tPC :: Int
tA = 0x00
tX = 0x01
tY = 0x02
tS = 0x03
tP = 0x04
tPC = 0x05     -- 2 bytes, low first (as every 16-bit field): the next instruction's address

tLine, tCycle, tNmist, tDmactl, tChactl, tDlist, tHscrol, tVscrol, tPmbase, tChbase, tNmien :: Int
tLine = 0x08   -- 2: ANTIC's line (0-261) of the CPU's last cycle
tCycle = 0x0A  -- its cycle in the line (0-113)
tNmist = 0x0B
tDmactl = 0x0C
tChactl = 0x0D
tDlist = 0x0E  -- 2: the display list counter
tHscrol = 0x10
tVscrol = 0x11
tPmbase = 0x12
tChbase = 0x13
tNmien = 0x14

tPenh, tPenv, tIrPrev, tMsc, tHires, tLoader, tXexPos :: Int
tPenh = 0x15
tPenv = 0x16
tIrPrev = 0x17 -- the display list's instruction before vertical blank (its vertical scroll bit counts at line 8)
tMsc = 0x18    -- 2: the memory scan counter
tHires = 0x1A  -- the last mode line was hires (2, 3, F)
tLoader = 0x1B -- the executable loader: bit 0 its stub cartridge there, bit 1 its page there
tXexPos = 0x1C -- 3: the loader port's position in the file

tPactl, tPbctl, tOra, tDdra, tOrb, tDdrb, tEdges, tCb2 :: Int
tPactl = 0x20  -- the PIA's controls, with their flags (bits 6, 7)
tPbctl = 0x21
tOra = 0x22
tDdra = 0x23
tOrb = 0x24
tDdrb = 0x25
tEdges = 0x26  -- bit 0: a CA2 edge waits to show, bit 1: a CB2 edge
tCb2 = 0x27    -- CB2 as driven: 0 floating, 1 high, 2 low

tAudf, tAudc, tAudctl, tSkctl, tIrqen, tIrqst, tSkstat, tKbcode, tSerout, tSerial :: Int
tAudf = 0x30   -- 4: AUDF1-4
tAudc = 0x34   -- 4: AUDC1-4
tAudctl = 0x38
tSkctl = 0x39
tIrqen = 0x3A
tIrqst = 0x3B
tSkstat = 0x3C
tKbcode = 0x3D
tSerout = 0x3E  -- the value last written to SEROUT
tSerial = 0x3F  -- bit 0: its byte waits (no serial clock sends it)

tGtia, tLatch, tColl, tTrigLatch :: Int
tGtia = 0x40   -- 32: per register $D000-$D01F the value in effect (sizes 2 bits, colours without bit 0; VDELAY, GRACTL, CONSOL as written; 0 for GRAFPn, GRAFM, HITCLR: the latches hold their data)
tLatch = 0x60  -- 8: the objects' data latches, players 0-3 then missiles 0-3 (a missile's two bits at 7-6)
tColl = 0x68   -- 8: collisions, missiles 0-3 then players 0-3 (playfield bits 0-3, players 4-7)
tTrigLatch = 0x70

-- | Every field: name, offset, size (for the golden and the tests' messages).
tableFields :: [(String, Int, Int)]
tableFields =
  [ ("A", tA, 1), ("X", tX, 1), ("Y", tY, 1), ("S", tS, 1), ("P", tP, 1), ("PC", tPC, 2)
  , ("LINE", tLine, 2), ("CYCLE", tCycle, 1), ("NMIST", tNmist, 1), ("DMACTL", tDmactl, 1)
  , ("CHACTL", tChactl, 1), ("DLIST", tDlist, 2), ("HSCROL", tHscrol, 1), ("VSCROL", tVscrol, 1)
  , ("PMBASE", tPmbase, 1), ("CHBASE", tChbase, 1), ("NMIEN", tNmien, 1)
  , ("PENH", tPenh, 1), ("PENV", tPenv, 1), ("IRPREV", tIrPrev, 1), ("MSC", tMsc, 2), ("HIRES", tHires, 1)
  , ("LOADER", tLoader, 1), ("XEXPOS", tXexPos, 3)
  , ("PACTL", tPactl, 1), ("PBCTL", tPbctl, 1), ("ORA", tOra, 1), ("DDRA", tDdra, 1)
  , ("ORB", tOrb, 1), ("DDRB", tDdrb, 1), ("EDGES", tEdges, 1), ("CB2", tCb2, 1)
  , ("AUDF", tAudf, 4), ("AUDC", tAudc, 4), ("AUDCTL", tAudctl, 1), ("SKCTL", tSkctl, 1)
  , ("IRQEN", tIrqen, 1), ("IRQST", tIrqst, 1), ("SKSTAT", tSkstat, 1), ("KBCODE", tKbcode, 1)
  , ("SEROUT", tSerout, 1), ("SERIAL", tSerial, 1)
  , ("GTIA", tGtia, 32), ("LATCH", tLatch, 8), ("COLL", tColl, 8), ("TRIGLATCH", tTrigLatch, 1)
  ]
