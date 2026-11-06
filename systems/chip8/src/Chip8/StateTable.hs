{-|
CHIP-8's state table (side track N, PLAN.md N.10): the interpreter's
registers in one neutral byte layout, as the other systems' ("Gb.StateTable",
"Nes.StateTable"). CHIP-8 has no machine code to load a state with, so the
path is direct: the spec fills the table ('Chip8.SpecState.specTable') and
loads one ('specLoad'); the core's hooks read it from the core's record and
write it back into the frozen core ("Chip8.StateHooks"); the PS copies it
as a slot's record. RAM (4 KiB) and the framebuffer (32 rows of 8 bytes,
bit 7 of a row's first byte column 0) go through the PS's windows.

Not exact (compared loosely in the tests): the reason the spec halted for
(the core keeps only that it did), and the core's latched ticks (the spec
has none: its timers tick between instructions).
-}
module Chip8.StateTable
  ( tableSize
  , tableFields
    -- * Offsets
  , tPc, tI, tV, tStack, tSp, tDelay, tSound, tRng, tVblank, tKeyWait, tHalted, tTickPend, tInstrPend
  ) where

import Prelude

-- | Bytes in the table.
tableSize :: Int
tableSize = 0x40

tPc, tI, tV, tStack, tSp, tDelay, tSound, tRng, tVblank, tKeyWait, tHalted, tTickPend, tInstrPend :: Int
tPc = 0x00        -- 2, low first (as every 16-bit field)
tI = 0x02         -- 2
tV = 0x04         -- 16: V0 to VF
tStack = 0x14     -- 32: 16 return addresses, the oldest first, 2 bytes each
tSp = 0x34        -- entries on the stack (0-16)
tDelay = 0x35
tSound = 0x36
tRng = 0x37       -- 2: the random generator (an LFSR)
tVblank = 0x39    -- the display wait: 0 none, 1 waiting for the tick, 2 released
tKeyWait = 0x3A   -- Fx0A: the key held (0-15), or 0xFF none
tHalted = 0x3B    -- 1: halted (an illegal opcode, a stack error)
tTickPend = 0x3C  -- the core's latched timer tick (the spec has none)
tInstrPend = 0x3D -- the core's latched instruction tick

-- | Every field: name, offset, size (for the golden and the tests' messages).
tableFields :: [(String, Int, Int)]
tableFields =
  [ ("PC", tPc, 2), ("I", tI, 2), ("V", tV, 16), ("STACK", tStack, 32), ("SP", tSp, 1)
  , ("DELAY", tDelay, 1), ("SOUND", tSound, 1), ("RNG", tRng, 2), ("VBLANK", tVblank, 1)
  , ("KEYWAIT", tKeyWait, 1), ("HALTED", tHalted, 1), ("TICKPEND", tTickPend, 1), ("INSTRPEND", tInstrPend, 1)
  ]
