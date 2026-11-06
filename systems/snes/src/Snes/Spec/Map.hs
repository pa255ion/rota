{-|
The 5A22's address space (PLAN.md step 7.3; fullsnes, "SNES Memory Map",
"SNES Cartridge LoROM/HiROM Mapping"): what a 24-bit address on the A bus
reaches, and how many master clocks the CPU's cycle there takes.

> banks 00-3F, 80-BF: 0000-1FFF WRAM (its first 8 KiB), 2100-21FF the
>   B bus (PPU, APU ports, WRAM port), 4000-41FF the old joypad ports,
>   4200-5FFF the CPU's registers, 6000-7FFF (HiROM: SRAM in 20-3F/A0-BF),
>   8000-FFFF ROM
> banks 40-7D, C0-FF: ROM (LoROM: SRAM at 70-7D/F0-FF 0000-7FFF)
> banks 7E-7F: WRAM, 128 KiB

Speeds: 6 master clocks for the B bus, the CPU registers and (with
MEMSEL set) ROM in banks 80-FF; 12 for 4000-41FF; 8 for the rest.
-}
module Snes.Spec.Map
  ( Target (..)
  , decode
  , speed
  , mirror
  ) where

import Prelude

import Data.Bits ((.&.), (.|.), shiftL, shiftR)

import Snes.Cart (Mapping (..))

data Target
  = TWram !Int  -- ^ offset into the 128 KiB
  | TRom !Int   -- ^ offset into the ROM (mirrored to its size)
  | TSram !Int  -- ^ offset into the SRAM (mirrored)
  | TBbus !Int  -- ^ 00h-FFh: 2100h + n
  | TJoy !Int   -- ^ 4016h, 4017h
  | TCpuReg !Int -- ^ 4200h-43FFh: the CPU's registers and DMA
  | TOpen        -- ^ nothing answers: open bus
  deriving stock (Show, Eq)

-- | Where an address goes, for a cartridge of a mapping, ROM size and SRAM
-- size (0: none).
decode :: Mapping -> Int -> Int -> Int -> Target
decode mapping romSize sramSize a
  | bank == 0x7E || bank == 0x7F = TWram (a .&. 0x1FFFF)
  | system, off < 0x2000 = TWram off
  | system, off >= 0x2100, off < 0x2200 = TBbus (off .&. 0xFF)
  | system, off == 0x4016 || off == 0x4017 = TJoy off
  | system, off >= 0x4200, off < 0x4400 = TCpuReg off
  | system, off < 0x6000 = TOpen
  | otherwise = case mapping of
      LoRom
        | sramSize > 0, (bank .&. 0x7F) >= 0x70, (bank .&. 0x7F) < 0x7E, off < 0x8000 ->
            TSram (mirror ((bank .&. 0x0F) `shiftL` 15 .|. off) sramSize)
        | off >= 0x8000 -> rom ((bank .&. 0x7F) `shiftL` 15 .|. (off .&. 0x7FFF))
        | not system, (bank .&. 0x7F) >= 0x40 -> rom ((bank .&. 0x7F) `shiftL` 15 .|. (off .&. 0x7FFF))
        | otherwise -> TOpen
      _
        | system, sramSize > 0, (bank .&. 0x7F) >= 0x20, (bank .&. 0x7F) < 0x40, off >= 0x6000, off < 0x8000 ->
            TSram (mirror (((bank .&. 0x1F) `shiftL` 13) .|. (off - 0x6000)) sramSize)
        | system, off < 0x8000 -> TOpen
        | otherwise -> rom (exhi .|. (bank .&. 0x3F) `shiftL` 16 .|. off)
 where
  bank = a `shiftR` 16
  off = a .&. 0xFFFF
  system = (bank .&. 0x7F) < 0x40
  -- ExHiROM: banks C0-FF are the first 4 MiB, 40-7D the next (later)
  exhi = if mapping == ExHiRom && bank < 0x80 then 0x400000 else 0
  rom o = if romSize == 0 then TOpen else TRom (mirror o romSize)

-- | Master clocks for a CPU cycle at an address; MEMSEL (420Dh bit 0)
-- makes ROM in banks 80-FF fast.
speed :: Bool -> Int -> Int
speed memsel a
  | bank >= 0x40 && bank < 0x80 = 8
  | bank >= 0xC0 = if memsel then 6 else 8
  | off < 0x2000 = 8
  | off < 0x4000 = 6
  | off < 0x4200 = 12
  | off < 0x6000 = 6
  | off < 0x8000 = 8
  | bank >= 0x80, memsel = 6
  | otherwise = 8
 where
  bank = a `shiftR` 16
  off = a .&. 0xFFFF

-- | An address into a memory of a size that may not be a power of two:
-- the largest power of two first, the rest mirrored after it (fullsnes;
-- a 3 MiB ROM is 2 MiB + 1 MiB twice).
mirror :: Int -> Int -> Int
mirror addr0 size0 = go addr0 size0 0 (1 `shiftL` 23)
 where
  go addr size base mask
    | size <= 0 = 0
    | addr < size = base + addr
    | otherwise =
        let m = until (\k -> addr .&. k /= 0 || k == 0) (`shiftR` 1) mask
            addr' = addr - m
            (size', base') = if size > m then (size - m, base + m) else (size, base)
        in go addr' size' base' (m `shiftR` 1)
