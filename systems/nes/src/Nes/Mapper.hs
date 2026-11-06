{-# LANGUAGE StrictData #-}
{-|
Cartridge boards in Clash (PLAN.md step 3.6): NROM, MMC1, UxROM, CNROM,
AxROM, MMC3, MMC5 (side track D), with the rules of "Nes.Spec.Mapper" (8
KiB PRG RAM on every board, MMC5's 64 KiB, MMC1's RAM-disable bit
ignored, no bus conflicts). The core asks for physical addresses: PRG ROM
or RAM byte ('prgTarget'), CHR byte (ROM or RAM; 'chrAddrK' with the
fetch's kind), and where a nametable byte is ('ntTarget': the console's
2 KiB nametable RAM, MMC5's ExRAM, or a constant).
-}
module Nes.Mapper
  ( MapperKind (..)
  , CartCfg (..)
  , MapState (..)
  , mapInit
  , prgAddr
  , chrAddr
  , ciramAddr
  , mapWrite
  , mapA12
  , mapCycle
  , mapIrq
  , cartCfgFromHeader
    -- * MMC5 (side track D)
  , M5 (..)
  , m5Init
  , PrgTarget (..)
  , prgTarget
  , NtTarget (..)
  , ntTarget
  , chrAddrK
  , m5Fetch
  , m5Ext
  , m5CpuRead
  , m5Vector
  , m5PpuReg
  , m5Write
  , m5ExramWrite
  , m5RamWritable
  ) where

import Clash.Prelude

import Nes.Ppu (Fetch (..))

import Nes.Cartridge (Header (..), Mirroring (..), checkSupported)

data MapperKind = MNrom | MMmc1 | MUxrom | MCnrom | MAxrom | MMmc3 | MMmc5
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFDataX, BitPack)

-- | From the header (the loader writes it, step 3.8).
data CartCfg = CartCfg
  { ccMapper   :: MapperKind
  , ccPrgMask  :: Unsigned 19 -- ^ PRG ROM bytes - 1 (a power of two)
  , ccChrMask  :: Unsigned 18 -- ^ CHR bytes - 1
  , ccChrRam   :: Bool
  , ccVertical :: Bool        -- ^ fixed mirroring (NROM, UxROM, CNROM)
  , ccPrgRam   :: Bool        -- ^ 8 KiB PRG RAM at $6000 (iNES: always; NES 2.0: if its size is not 0)
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (NFDataX, BitPack)

data MapState = MapState
  { msShift   :: Unsigned 5
  , msCount   :: Unsigned 3
  , msControl :: Unsigned 5
  , msChr0    :: Unsigned 5
  , msChr1    :: Unsigned 5
  , msPrg     :: Unsigned 5
  , msBank    :: Unsigned 8   -- ^ UxROM, CNROM, AxROM
  , msPrevWr  :: Bool         -- ^ the previous CPU cycle wrote $8000-$FFFF (MMC1)
  , msSel     :: Unsigned 8   -- ^ MMC3 bank select
  , msR       :: Vec 8 (Unsigned 8) -- ^ MMC3 R0 to R7
  , msMirror  :: Bool         -- ^ MMC3: horizontal
  , msIrqLatch :: Unsigned 8
  , msIrqCount :: Unsigned 8
  , msIrqReload :: Bool
  , msIrqEn   :: Bool
  , msIrqFlag :: Bool
  , msA12     :: Bool         -- ^ PPU A12 as last seen
  , msA12Low  :: Unsigned 4   -- ^ CPU cycles A12 has been low (the IRQ filter)
  , msM5      :: M5           -- ^ MMC5
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFDataX

mapInit :: MapState
mapInit = MapState 0 0 0x0C 0 0 0 0 False 0 (repeat 0) False 0 0 False False False False 0 m5Init

-- | MMC5's registers and its view of the PPU ("Nes.Spec.Mapper"'s slots).
data M5 = M5
  { m5PrgMode  :: Unsigned 2
  , m5ChrMode  :: Unsigned 2
  , m5Prot1    :: Unsigned 2
  , m5Prot2    :: Unsigned 2
  , m5ExMode   :: Unsigned 2
  , m5Nt       :: Unsigned 8
  , m5FillT    :: Unsigned 8
  , m5FillA    :: Unsigned 2
  , m5Prg      :: Vec 5 (Unsigned 8)   -- ^ $5113 to $5117
  , m5Chr      :: Vec 12 (Unsigned 8)  -- ^ $5120 to $512B
  , m5LastB    :: Bool                 -- ^ the last CHR register written was $5128-$512B
  , m5IrqCmp   :: Unsigned 8
  , m5IrqEn    :: Bool
  , m5IrqPend  :: Bool
  , m5InFrame  :: Bool
  , m5Line     :: Unsigned 8
  , m5Match    :: Unsigned 2           -- ^ reads in a row of one nametable address, minus one (stops at 3)
  , m5LastAddr :: Maybe (Unsigned 14)  -- ^ the last rendering fetch's address
  , m5Idle     :: Unsigned 2           -- ^ CPU cycles without a rendering fetch (stops at 3)
  , m5Fetched  :: Bool                 -- ^ a rendering fetch since the last CPU cycle
  , m5Mul1     :: Unsigned 8
  , m5Mul2     :: Unsigned 8
  , m5Ctrl     :: Unsigned 8           -- ^ the snooped $2000
  , m5Mask     :: Unsigned 8           -- ^ the snooped $2001
  , m5ExLatch  :: Unsigned 8           -- ^ the ExRAM byte of the last background nametable fetch
  , m5ExPend   :: Bool                 -- ^ ... on its way (the core reads it: 'kiExLatch')
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFDataX

-- | At power-on (as the spec: $5100 = 3, $5102 = 1, $5103 = 2, $5104 = 3,
-- $5114-$5117 = $FF, 1 KiB CHR mode).
m5Init :: M5
m5Init = M5
  { m5PrgMode = 3, m5ChrMode = 3, m5Prot1 = 1, m5Prot2 = 2, m5ExMode = 3, m5Nt = 0, m5FillT = 0, m5FillA = 0
  , m5Prg = 0 :> 0xFF :> 0xFF :> 0xFF :> 0xFF :> Nil, m5Chr = repeat 0, m5LastB = False
  , m5IrqCmp = 0, m5IrqEn = False, m5IrqPend = False, m5InFrame = False, m5Line = 0, m5Match = 0
  , m5LastAddr = Nothing, m5Idle = 0, m5Fetched = False, m5Mul1 = 0, m5Mul2 = 0, m5Ctrl = 0, m5Mask = 0
  , m5ExLatch = 0, m5ExPend = False }

-- | PRG ROM byte for a CPU address in $8000-$FFFF.
{-# OPAQUE prgAddr #-}
prgAddr :: CartCfg -> MapState -> Unsigned 16 -> Unsigned 19
prgAddr cfg m a = off .&. ccPrgMask cfg
 where
  hi = a >= 0xC000
  lo14 = resize (a .&. 0x3FFF) :: Unsigned 19
  lo15 = resize (a .&. 0x7FFF) :: Unsigned 19
  lastBank = ccPrgMask cfg `shiftR` 14
  bank16 :: Unsigned 19 -> Unsigned 19
  bank16 b = b `shiftL` 14
  off = case ccMapper cfg of
    MMmc1 ->
      let bank = resize (msPrg m .&. 0x0F) :: Unsigned 19
      in case (msControl m `shiftR` 2) .&. 3 of
           2 -> if hi then bank16 bank + lo14 else lo14
           3 -> if hi then bank16 lastBank + lo14 else bank16 bank + lo14
           _ -> bank16 (bank .&. 0x0E) + lo15
    MUxrom -> if hi then bank16 lastBank + lo14 else bank16 (resize (msBank m)) + lo14
    MAxrom -> (resize (msBank m .&. 7) `shiftL` 15) + lo15
    MMmc3 ->
      let slot = resize (a `shiftR` 13) .&. 3 :: Unsigned 2
          -- PRG mode 1 swaps $8000 and $C000
          slot' = if testBit (msSel m) 6 && even slot then 2 - slot else slot
          lastB = ccPrgMask cfg `shiftR` 13
          bank = case slot' of
            0 -> resize (msR m !! (6 :: Index 8))
            1 -> resize (msR m !! (7 :: Index 8))
            2 -> lastB - 1
            _ -> lastB
      in (bank `shiftL` 13) + resize (a .&. 0x1FFF)
    _ -> lo15

-- | CHR byte for a PPU address in $0000-$1FFF.
{-# OPAQUE chrAddr #-}
chrAddr :: CartCfg -> MapState -> Unsigned 14 -> Unsigned 18
chrAddr cfg m a = off .&. ccChrMask cfg
 where
  lo12 = resize (a .&. 0x0FFF) :: Unsigned 18
  lo13 = resize (a .&. 0x1FFF) :: Unsigned 18
  off = case ccMapper cfg of
    MMmc1
      | testBit (msControl m) 4 -> (resize (if a >= 0x1000 then msChr1 m else msChr0 m) `shiftL` 12) + lo12
      | otherwise -> (resize (msChr0 m .&. 0x1E) `shiftL` 12) + lo13
    MCnrom -> (resize (msBank m) `shiftL` 13) + lo13
    MMmc3 ->
      let a' = if testBit (msSel m) 7 then a `xor` 0x1000 else a
          k = resize (a' `shiftR` 10) .&. 7 :: Unsigned 3
          b | k < 4 = (msR m !! (bitCoerce (k `shiftR` 1) :: Index 8) .&. 0xFE) + resize (k .&. 1)
            | otherwise = msR m !! (bitCoerce (k - 2) :: Index 8)
      in (resize b `shiftL` 10) + resize (a .&. 0x3FF)
    _ -> lo13

-- | Byte of the console's nametable RAM for a PPU address in $2000-$3EFF.
{-# OPAQUE ciramAddr #-}
ciramAddr :: CartCfg -> MapState -> Unsigned 14 -> Unsigned 11
ciramAddr cfg m a = (resize page `shiftL` 10) .|. resize (a .&. 0x3FF)
 where
  nt = resize (a `shiftR` 10) .&. 3 :: Unsigned 2
  page :: Unsigned 1
  page = case ccMapper cfg of
    MMmc1 -> case msControl m .&. 3 of
      0 -> 0
      1 -> 1
      2 -> resize nt
      _ -> resize (nt `shiftR` 1)
    MAxrom -> if testBit (msBank m) 4 then 1 else 0
    MMmc3 -> if msMirror m then resize (nt `shiftR` 1) else resize nt
    _ | ccVertical cfg -> resize nt
      | otherwise -> resize (nt `shiftR` 1)

-- | A CPU write to $8000-$FFFF (MMC1's serial port, bank registers).
{-# OPAQUE mapWrite #-}
mapWrite :: CartCfg -> Unsigned 16 -> Unsigned 8 -> MapState -> MapState
mapWrite cfg a d m = case ccMapper cfg of
  MMmc1
    | msPrevWr m -> m
    | testBit d 7 -> m { msShift = 0, msCount = 0, msControl = msControl m .|. 0x0C }
    | otherwise ->
        let sh' = (msShift m `shiftR` 1) .|. (if testBit d 0 then 0x10 else 0)
        in if msCount m == 4
             then let m' = m { msShift = 0, msCount = 0 }
                  in case (a `shiftR` 13) .&. 3 of
                       0 -> m' { msControl = sh' }
                       1 -> m' { msChr0 = sh' }
                       2 -> m' { msChr1 = sh' }
                       _ -> m' { msPrg = sh' }
             else m { msShift = sh', msCount = msCount m + 1 }
  MUxrom -> m { msBank = d .&. 0x0F }
  MCnrom -> m { msBank = d .&. 0x03 }
  MAxrom -> m { msBank = d .&. 0x17 }
  MMmc3 -> case (a .&. 0xE000, testBit a 0) of
    (0x8000, False) -> m { msSel = d }
    (0x8000, True) -> m { msR = replace (bitCoerce (resize (msSel m) :: Unsigned 3) :: Index 8) d (msR m) }
    (0xA000, False) -> m { msMirror = testBit d 0 }
    (0xA000, True) -> m
    (0xC000, False) -> m { msIrqLatch = d }
    (0xC000, True) -> m { msIrqCount = 0, msIrqReload = True }
    (0xE000, False) -> m { msIrqEn = False, msIrqFlag = False }
    _ -> m { msIrqEn = True }
  MNrom -> m
  MMmc5 -> m  -- its registers are at $5000-$5BFF ('m5Write')

-- | The configuration for a parsed header ('Nothing' for what M3 doesn't
-- support; "Nes.Cartridge.checkSupported" says why).
cartCfgFromHeader :: Header -> Maybe CartCfg
cartCfgFromHeader h = case checkSupported h of
  Left _ -> Nothing
  Right () -> do
    kind <- case hdMapper h of
      0 -> Just MNrom
      1 -> Just MMmc1
      2 -> Just MUxrom
      3 -> Just MCnrom
      4 -> Just MMmc3
      5 -> Just MMmc5
      7 -> Just MAxrom
      _ -> Nothing
    let chrBytes = if hdChrRom h == 0 then max 0x2000 (hdChrRam h) else hdChrRom h
    pure CartCfg
      { ccMapper = kind
      , ccPrgMask = fromIntegral (hdPrgRom h - 1)
      , ccChrMask = fromIntegral (chrBytes - 1)
      , ccChrRam = hdChrRom h == 0
      , ccVertical = hdMirroring h == Vertical
      , ccPrgRam = hdPrgRam h > 0 || hdMapper h == 5
      }

-- | CPU cycles PPU A12 must have been low before a rise clocks the MMC3's
-- counter (as "Nes.Spec.Mapper").
a12Filter :: Unsigned 4
a12Filter = 3

-- | The PPU put this address on its bus: the MMC3 watches A12.
{-# OPAQUE mapA12 #-}
mapA12 :: CartCfg -> Unsigned 14 -> MapState -> MapState
mapA12 cfg a m
  | ccMapper cfg /= MMmc3 = m
  | testBit a 12 = (if not (msA12 m) && msA12Low m >= a12Filter then mmc3Clock m else m) { msA12 = True, msA12Low = 0 }
  | otherwise = m { msA12 = False }

-- | One CPU cycle: the A12 filter counts while A12 is low.
{-# OPAQUE mapCycle #-}
mapCycle :: CartCfg -> MapState -> MapState
mapCycle cfg m
  | ccMapper cfg == MMmc5 =
      -- MMC5: the frame ends after 3 CPU cycles without a rendering fetch
      let x = msM5 m
          n = if m5Fetched x then 0 else (if m5Idle x < 3 then m5Idle x + 1 else 3)
          x' = x { m5Fetched = False, m5Idle = n }
      in m { msM5 = if n == 3 then x' { m5InFrame = False, m5Match = 0, m5LastAddr = Nothing } else x' }
  | ccMapper cfg == MMmc3 && not (msA12 m) && msA12Low m < 15 = m { msA12Low = msA12Low m + 1 }
  | otherwise = m

mmc3Clock :: MapState -> MapState
mmc3Clock m = m { msIrqCount = c', msIrqReload = False, msIrqFlag = msIrqFlag m || (c' == 0 && msIrqEn m) }
 where
  c' = if msIrqCount m == 0 || msIrqReload m then msIrqLatch m else msIrqCount m - 1

-- | The cartridge's IRQ line (MMC3; MMC5: pending and enabled).
mapIrq :: MapState -> Bool
mapIrq m = msIrqFlag m || (m5IrqPend (msM5 m) && m5IrqEn (msM5 m))

-- * MMC5 (side track D; "Nes.Spec.Mapper" has the rules)

-- | Where a CPU address of $6000-$FFFF goes.
data PrgTarget = PrgRam (Unsigned 16) | PrgRom (Unsigned 19)
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFDataX

-- | PRG RAM (MMC5: 64 KiB, banked; the others 8 KiB at $6000) or ROM.
{-# OPAQUE prgTarget #-}
prgTarget :: CartCfg -> MapState -> Unsigned 16 -> PrgTarget
prgTarget cfg m a
  | ccMapper cfg /= MMmc5 = if a < 0x8000 then PrgRam (resize (a .&. 0x1FFF)) else PrgRom (prgAddr cfg m a)
  | a < 0x8000 = ram (head (m5Prg x))
  | isRom = PrgRom ((resize b `shiftL` 13 .|. resize lo13) .&. ccPrgMask cfg)
  | otherwise = ram b
 where
  x = msM5 m
  lo13 = a .&. 0x1FFF
  slot = resize (a `shiftR` 13) .&. 3 :: Unsigned 8
  a13 = slot .&. 1
  reg i = m5Prg x !! (i :: Index 5)
  (raw, b) = case m5PrgMode x of
    0 -> (reg 4, (reg 4 .&. 0x7C) .|. slot)
    1 -> if slot < 2 then (reg 2, (reg 2 .&. 0x7E) .|. a13) else (reg 4, (reg 4 .&. 0x7E) .|. a13)
    2 -> case slot of
      0 -> (reg 2, reg 2 .&. 0x7E)
      1 -> (reg 2, (reg 2 .&. 0x7E) .|. 1)
      2 -> (reg 3, reg 3 .&. 0x7F)
      _ -> (reg 4, reg 4 .&. 0x7F)
    _ -> (reg (bitCoerce (resize (slot + 1) :: Unsigned 3)), reg (bitCoerce (resize (slot + 1) :: Unsigned 3)) .&. 0x7F)
  mode = m5PrgMode x
  isRom = testBit raw 7 || mode == 0 || (mode == 1 && slot >= 2) || slot == 3
  ram bank = PrgRam ((resize (bank .&. 7) `shiftL` 13) .|. resize lo13)

-- | PRG RAM is writable ($5102 = 2, $5103 = 1).
{-# OPAQUE m5RamWritable #-}
m5RamWritable :: MapState -> Bool
m5RamWritable m = m5Prot1 (msM5 m) == 2 && m5Prot2 (msM5 m) == 1

-- | The snooped PPU state: 8x16 sprites with rendering on, rendering on.
m5Modes :: M5 -> (Bool, Bool)
m5Modes x = (on && testBit (m5Ctrl x) 5, on)
 where on = m5Mask x .&. 0x18 /= 0

-- | Extended attributes are on (ExRAM mode 1, rendering on).
{-# OPAQUE m5Ext #-}
m5Ext :: CartCfg -> MapState -> Bool
m5Ext cfg m = ccMapper cfg == MMmc5 && m5ExMode (msM5 m) == 1 && snd (m5Modes (msM5 m))

-- | The CHR address of a pattern address for a rendering fetch of this
-- kind, or $2007's ('Nothing').
{-# OPAQUE chrAddrK #-}
chrAddrK :: CartCfg -> MapState -> Maybe Fetch -> Unsigned 14 -> Unsigned 18
chrAddrK cfg m k a
  | ccMapper cfg /= MMmc5 = chrAddr cfg m a
  | k == Just FBgPat && m5Ext cfg m = ((resize (m5ExLatch x .&. 0x3F) `shiftL` 12) .|. resize (a .&. 0xFFF)) .&. ccChrMask cfg
  | otherwise = (case (m5ChrMode x, setB) of
      (0, False) -> bank 7 13 .|. lowBits 0x1FFF
      (1, False) -> bank (if a < 0x1000 then 3 else 7) 12 .|. lowBits 0xFFF
      (2, False) -> bank (1 + 2 * fromIntegral ((a `shiftR` 11) .&. 3)) 11 .|. lowBits 0x7FF
      (_, False) -> bank (fromIntegral ((a `shiftR` 10) .&. 7)) 10 .|. lowBits 0x3FF
      (0, True) -> bank 11 13 .|. lowBits 0x1FFF
      (1, True) -> bank 11 12 .|. lowBits 0xFFF
      (2, True) -> bank (9 + 2 * fromIntegral ((a `shiftR` 11) .&. 1)) 11 .|. lowBits 0x7FF
      (_, True) -> bank (8 + fromIntegral ((a `shiftR` 10) .&. 3)) 10 .|. lowBits 0x3FF) .&. ccChrMask cfg
 where
  x = msM5 m
  (tall, _) = m5Modes x
  setB = case k of
    Just FBgPat -> tall
    Just _ -> False
    Nothing -> tall && m5LastB x
  bank :: Index 12 -> Int -> Unsigned 18
  bank i sh = resize (m5Chr x !! i) `shiftL` sh
  lowBits :: Unsigned 14 -> Unsigned 18
  lowBits msk = resize (a .&. msk)

-- | Where a nametable byte ($2000-$2FFF, attribute bytes too) is.
data NtTarget = NtCiram (Unsigned 11) | NtExram (Unsigned 10) | NtConst (Unsigned 8)
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFDataX

-- | The nametable byte for a rendering fetch of this kind, or $2007's
-- ('Nothing'). MMC5: through $5105 (CIRAM pages, ExRAM, fill), extended
-- attributes for the attribute fetch.
{-# OPAQUE ntTarget #-}
ntTarget :: CartCfg -> MapState -> Maybe Fetch -> Unsigned 14 -> NtTarget
ntTarget cfg m k a
  | ccMapper cfg /= MMmc5 = NtCiram (ciramAddr cfg m a)
  | k == Just FAt && m5Ext cfg m = NtConst (resize (m5ExLatch x `shiftR` 6) * 0x55)
  | otherwise = case (m5Nt x `shiftR` (2 * fromEnum q)) .&. 3 of
      2 -> if m5ExMode x <= 1 then NtExram i else NtConst 0
      3 -> if i >= 0x3C0 then NtConst (resize (m5FillA x) * 0x55) else NtConst (m5FillT x)
      p -> NtCiram ((resize p `shiftL` 10) .|. resize i)
 where
  x = msM5 m
  q = resize (a `shiftR` 10) .&. 3 :: Unsigned 2
  i = resize (a .&. 0x3FF) :: Unsigned 10

-- | A rendering fetch (MMC5): the scanline detection (the third read in a
-- row of one nametable address starts a line), and with extended
-- attributes a background nametable fetch's ExRAM byte is wanted
-- ('m5ExPend': the core reads it).
{-# OPAQUE m5Fetch #-}
m5Fetch :: CartCfg -> Fetch -> Unsigned 14 -> MapState -> MapState
m5Fetch cfg k a m
  | ccMapper cfg /= MMmc5 = m
  | otherwise = m { msM5 = x2 { m5ExPend = m5ExPend x2 || (k == FNt && m5Ext cfg m) } }
 where
  x = msM5 m
  same = a >= 0x2000 && m5LastAddr x == Just a
  n = if same then (if m5Match x < 3 then m5Match x + 1 else 3) else 0
  x1 = x { m5Fetched = True, m5Match = n, m5LastAddr = Just a }
  l = m5Line x + 1
  x2 | same && n == 2 && not (m5InFrame x) = x1 { m5InFrame = True, m5Line = 0 }
     | same && n == 2 = x1 { m5Line = l, m5IrqPend = m5IrqPend x || (m5IrqCmp x /= 0 && l == m5IrqCmp x) }
     | otherwise = x1

-- | A CPU read of $5000-$5BFF (MMC5's registers): the value, if one drives
-- the bus ($5204 acknowledges the IRQ).
{-# OPAQUE m5CpuRead #-}
m5CpuRead :: Unsigned 16 -> MapState -> (MapState, Maybe (Unsigned 8))
m5CpuRead a m = case a of
  0x5204 -> ( m { msM5 = x { m5IrqPend = False } }
            , Just ((if m5IrqPend x then 0x80 else 0) .|. (if m5InFrame x then 0x40 else 0)) )
  0x5205 -> (m, Just (truncateB prod))
  0x5206 -> (m, Just (truncateB (prod `shiftR` 8)))
  _ -> (m, Nothing)
 where
  x = msM5 m
  prod = resize (m5Mul1 x) * resize (m5Mul2 x) :: Unsigned 16

-- | The NMI vector's read ($FFFA/$FFFB) ends the frame.
{-# OPAQUE m5Vector #-}
m5Vector :: CartCfg -> Unsigned 16 -> MapState -> MapState
m5Vector cfg a m
  | ccMapper cfg == MMmc5 && (a == 0xFFFA || a == 0xFFFB) = m { msM5 = (msM5 m) { m5InFrame = False, m5IrqPend = False, m5Line = 0 } }
  | otherwise = m

-- | A CPU write of $2000/$2001, fully decoded (MMC5 snoops them).
{-# OPAQUE m5PpuReg #-}
m5PpuReg :: CartCfg -> Unsigned 16 -> Unsigned 8 -> MapState -> MapState
m5PpuReg cfg a d m
  | ccMapper cfg /= MMmc5 = m
  | a == 0x2000 = m { msM5 = x { m5Ctrl = d } }
  | a == 0x2001 =
      let x' = if d .&. 0x18 == 0 || m5Mask x .&. 0x18 == 0
                 then x { m5InFrame = False, m5Match = 0, m5LastAddr = Nothing } else x
      in m { msM5 = x' { m5Mask = d } }
  | otherwise = m
 where x = msM5 m

-- | A CPU write of MMC5's registers ($5000-$5BFF).
{-# OPAQUE m5Write #-}
m5Write :: Unsigned 16 -> Unsigned 8 -> MapState -> MapState
m5Write a d m = m { msM5 = x' }
 where
  x = msM5 m
  x' = case a of
    0x5100 -> x { m5PrgMode = resize d }
    0x5101 -> x { m5ChrMode = resize d }
    0x5102 -> x { m5Prot1 = resize d }
    0x5103 -> x { m5Prot2 = resize d }
    0x5104 -> x { m5ExMode = resize d }
    0x5105 -> x { m5Nt = d }
    0x5106 -> x { m5FillT = d }
    0x5107 -> x { m5FillA = resize d }
    0x5203 -> x { m5IrqCmp = d }
    0x5204 -> x { m5IrqEn = testBit d 7 }
    0x5205 -> x { m5Mul1 = d }
    0x5206 -> x { m5Mul2 = d }
    _ | a >= 0x5113 && a <= 0x5117 -> x { m5Prg = replace (fromIntegral (a - 0x5113) :: Index 5) d (m5Prg x) }
      | a >= 0x5120 && a <= 0x512B -> x { m5Chr = replace (fromIntegral (a - 0x5120) :: Index 12) d (m5Chr x), m5LastB = a >= 0x5128 }
      | otherwise -> x

-- | A CPU write to ExRAM ($5C00-$5FFF): the byte it writes, if any (modes
-- 0 and 1: 0 outside the frame; mode 3: none).
{-# OPAQUE m5ExramWrite #-}
m5ExramWrite :: MapState -> Unsigned 8 -> Maybe (Unsigned 8)
m5ExramWrite m d = case m5ExMode x of
  2 -> Just d
  3 -> Nothing
  _ -> Just (if m5InFrame x then d else 0)
 where x = msM5 m
