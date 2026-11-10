{-# OPTIONS_GHC -fworker-wrapper -fspecialise-aggressively -funfolding-use-threshold=200 -funfolding-creation-threshold=1000 #-}
-- (the fast version is never synthesised: GHC's unboxing back on, which
-- clash-common's -fno-worker-wrapper turns off for Clash's sake)
-- Side track Q (PLAN.md Q.7): the desktop emulator's fast version of
-- Nes.Spec.Mapper, kept beside it (the original stays the reference the
-- core's lockstep checks; a fix in either goes to both). nes-specfast
-- runs both machines side by side and compares them.
{-|
Cartridge boards of the reference model (PLAN.md step 3.2; nesdev wiki,
"NROM", "MMC1", "UxROM", "INES Mapper 003", "AxROM"): PRG ROM and RAM on
the CPU side, CHR ROM or RAM and the nametable mirroring on the PPU side.

* __PRG RAM__: 8 KiB at $6000 unless an NES 2.0 header says there is none
  (iNES 1.0 headers always get it: Blargg's result protocol lives there;
  Holy Mapperel declares its RAM, step 3.10); without it, open bus. MMC1's RAM-disable bit is
  ignored (it varies between MMC1 revisions; documented approximation).
* __MMC1__: 5-bit serial port at $8000 to $FFFF, bit 7 resets it (and
  sets PRG mode 3); a write on the cycle right after the previous write is
  ignored (the second write of a read-modify-write instruction). PRG modes
  32 KiB / fix $8000 / fix $C000, CHR modes 8 KiB / 2 × 4 KiB, mirroring
  one-screen A/B, vertical, horizontal. Powers up in PRG mode 3.
* __UxROM__ (2): 16 KiB bank at $8000, last bank at $C000. __CNROM__ (3):
  8 KiB CHR bank. __AxROM__ (7): 32 KiB PRG bank, one-screen nametable
  chosen by bit 4. Bus conflicts are not modelled (submapper 0 boards).
* __MMC5__ (5; side track D, nesdev wiki "MMC5"): PRG modes 0 to 3 over
  ROM and 64 KiB of PRG RAM (the wiki's superset of every board; $5113 to
  $5116 choose ROM or RAM by bit 7, write protection $5102/$5103), CHR
  modes 8/4/2/1 KiB with the two register sets (with 8x16 sprites and
  rendering on, the snooped $2000/$2001: sprites from $5120-$5127, the
  background from $5128-$512B, $2007 from the set written last), 1 KiB
  ExRAM ($5104 modes: nametable, extended attributes, CPU RAM, ROM; in
  modes 0 and 1 a CPU write outside rendering writes 0), nametables
  ($5105: CIRAM pages, ExRAM, fill $5106/$5107), extended attributes (the
  background nametable fetch latches the ExRAM byte: its palette for the
  attribute fetch, its 4 KiB CHR bank for the pattern fetches), the
  scanline IRQ (the third read in a row of one nametable address starts a
  line: in-frame and the counter, $5203 compare, $5204 enable, pending
  and in-frame on read, a read acknowledges; in-frame ends after 3 CPU
  cycles without a rendering fetch, on $2001 rendering off, and on the
  NMI vector's read), the multiplier ($5205/$5206). Not modelled: the
  vertical split ($5200-$5202, two games' intro or ending), expansion
  audio (no NES cartridge carries it), $5130's upper CHR bits (CHR is at
  most 256 KiB here).
-}
module Nes.SpecFast.Mapper
  ( Mapper
  , mapperFields
  , newMapper
  , cpuReadCart
  , cpuWriteCart
  , chrRead
  , chrWrite
  , ciramIndex
  , ppuRead
  , ppuWrite
  , mapperCpuRead
  , mapperPpuReg
  , prgRamRead
  , mapperPrgRam
  , mapperA12
  , mapperCycle
  , mapperIrq
  , mapperClocks
  , mapperCounter
    -- * Save states (side track N)
  , MapperSnap (..)
  , mapperSnapshot
  , mapperImages
  , mapperLoad
  ) where

import Prelude

import Control.Monad (when)
import Control.Monad.ST (ST)
import Data.Bits
import qualified Data.ByteString as BS
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MV
import Platform.Snapshot (Field (..))
import Data.Word

import Nes.Cartridge
import Nes.Spec.Ppu (FetchKind (..))

data Mapper s = Mapper
  { mpKind    :: !Int
  , mpPrg     :: !(U.Vector Word8)
  , mpPrgMask :: !Int                    -- ^ PRG size - 1 (sizes are powers of two)
  , mpChr     :: !(MV.MVector s Word8)   -- ^ CHR ROM (copied) or CHR RAM
  , mpChrMask :: !Int
  , mpChrRam  :: !Bool
  , mpPrgRam  :: !(MV.MVector s Word8)   -- ^ 8 KiB at $6000 (MMC5: 64 KiB, banked)
  , mpExram   :: !(MV.MVector s Word8)   -- ^ MMC5's 1 KiB
  , mpHasRam  :: !Bool                   -- ^ the header gives PRG RAM (iNES: always; NES 2.0: size > 0)
  , mpFixed   :: !Mirroring              -- ^ from the header (boards without mirroring control)
  , mpRegs    :: !(MV.MVector s Int)
  }

-- Register slots.
rShift, rCount, rControl, rChr0, rChr1, rPrg, rLastWrite, rBank, rSel, rR0, rMirror, rIrqLatch, rIrqCount, rIrqReload, rIrqEn, rIrqFlag, rA12Low, rA12, rClocks :: Int
rShift = 0     -- MMC1 shift register
rCount = 1     -- MMC1 bits shifted in
rControl = 2   -- MMC1 control
rChr0 = 3
rChr1 = 4
rPrg = 5
rLastWrite = 6 -- MMC1: CPU cycle of the last write to $8000-$FFFF
rBank = 7      -- UxROM / CNROM / AxROM bank register
rSel = 8       -- MMC3 bank select ($8000)
rR0 = 9        -- MMC3 R0 to R7: 9 to 16
rMirror = 17   -- MMC3 $A000 bit 0 (1 = horizontal)
rIrqLatch = 18
rIrqCount = 19
rIrqReload = 20
rIrqEn = 21
rIrqFlag = 22
rA12Low = 23   -- CPU cycles PPU A12 has been low (the IRQ filter)
rA12 = 24      -- PPU A12 as last seen
rClocks = 25   -- MMC3 counter clocks so far (for tests)

-- MMC5 (side track D)
m5PrgMode, m5ChrMode, m5Prot1, m5Prot2, m5ExMode, m5Nt, m5FillT, m5FillA, m5Prg, m5Chr, m5LastB,
  m5IrqCmp, m5IrqEn, m5IrqPend, m5InFrame, m5Line, m5Match, m5LastAddr, m5Idle, m5Fetched,
  m5Mul1, m5Mul2, m5Ctrl, m5Mask, m5ExLatch :: Int
m5PrgMode = 26  -- $5100
m5ChrMode = 27  -- $5101
m5Prot1 = 28    -- $5102
m5Prot2 = 29    -- $5103
m5ExMode = 30   -- $5104
m5Nt = 31       -- $5105
m5FillT = 32    -- $5106
m5FillA = 33    -- $5107 (2 bits)
m5Prg = 34      -- $5113 to $5117: 34 to 38
m5Chr = 39      -- $5120 to $512B: 39 to 50 (with $5130's bits at the write)
m5LastB = 51    -- the last CHR register written was $5128-$512B
m5IrqCmp = 52   -- $5203
m5IrqEn = 53    -- $5204 bit 7
m5IrqPend = 54
m5InFrame = 55
m5Line = 56     -- the scanline counter
m5Match = 57    -- reads in a row of the same nametable address, minus one
m5LastAddr = 58 -- the last rendering fetch's address
m5Idle = 59     -- CPU cycles without a rendering fetch
m5Fetched = 60  -- a rendering fetch since the last CPU cycle
m5Mul1 = 61
m5Mul2 = 62
m5Ctrl = 63     -- the snooped $2000
m5Mask = 64     -- the snooped $2001
m5ExLatch = 65  -- the ExRAM byte of the last background nametable fetch

slots :: Int
slots = 66

newMapper :: Rom -> ST s (Either String (Mapper s))
newMapper rom = do
  let h = romHeader rom
  case checkSupported h of
    Left e -> pure (Left e)
    Right () -> do
      let prg = U.fromList (BS.unpack (romPrg rom))
          chrRom = BS.unpack (romChr rom)
          ram = null chrRom
          chrSize = if ram then max 0x2000 (hdChrRam h) else length chrRom
      chr <- if ram then MV.replicate chrSize 0 else U.thaw (U.fromList chrRom)
      prgRam <- MV.replicate (if hdMapper h == 5 then 0x10000 else 0x2000) 0
      exram <- MV.replicate 0x400 0
      regs <- MV.replicate slots 0
      MV.write regs rControl 0x0C
      MV.write regs rShift 0
      MV.write regs rLastWrite (-10)
      -- MMC3 power-on banks (unspecified on hardware; Blargg's mmc3_test_2
      -- runs from $8000 without setting them): identity CHR, R6 = 0, R7 = 1
      mapM_ (uncurry (MV.write regs)) (zip [rR0 ..] [0, 2, 4, 5, 6, 7, 0, 1])
      -- MMC5 at power-on (nesdev wiki: $5100 = 3, $5102 = 1, $5103 = 2,
      -- $5104 = 3, $5117 = $FF; the rest unspecified: $5114-$5116 ROM's
      -- last banks, CHR mode 1 KiB)
      mapM_ (uncurry (MV.write regs))
        [ (m5PrgMode, 3), (m5ChrMode, 3), (m5Prot1, 1), (m5Prot2, 2), (m5ExMode, 3)
        , (m5Prg + 1, 0xFF), (m5Prg + 2, 0xFF), (m5Prg + 3, 0xFF), (m5Prg + 4, 0xFF), (m5LastAddr, -1) ]
      pure (Right Mapper
        { mpKind = hdMapper h
        , mpPrg = prg
        , mpPrgMask = U.length prg - 1
        , mpChr = chr
        , mpChrMask = chrSize - 1
        , mpChrRam = ram
        , mpPrgRam = prgRam
        , mpExram = exram
        , mpHasRam = hdPrgRam h > 0 || hdMapper h == 5
        , mpFixed = hdMirroring h
        , mpRegs = regs
        })

reg :: Mapper s -> Int -> ST s Int
reg m = MV.unsafeRead (mpRegs m)
{-# INLINE reg #-}

-- | CPU read of $4020-$FFFF; -1 = nothing drives the bus (open bus).
-- (Q.7: inlined: no boxed byte per read)
{-# INLINE cpuReadCart #-}
cpuReadCart :: Mapper s -> Word16 -> ST s Int
cpuReadCart m a
  | mpKind m == 5 = mmc5Read m a
  | a >= 0x8000 = do
      off <- prgOffset m a
      pure (fromIntegral (U.unsafeIndex (mpPrg m) (off .&. mpPrgMask m)))
  | a >= 0x6000 && mpHasRam m = fromIntegral <$> MV.unsafeRead (mpPrgRam m) (fromIntegral a .&. 0x1FFF)
  | otherwise = pure (-1)

-- | PRG RAM without side effects (test hooks).
-- | The cartridge's PRG RAM ($6000-$7FFF, the MMC5's 64 KiB), if it has
-- any: what a battery keeps (side track W: the page's battery saves).
mapperPrgRam :: Mapper s -> Maybe (MV.MVector s Word8)
mapperPrgRam m = if mpHasRam m then Just (mpPrgRam m) else Nothing

prgRamRead :: Mapper s -> Word16 -> ST s Word8
prgRamRead m a = MV.unsafeRead (mpPrgRam m) (fromIntegral a .&. 0x1FFF)

prgOffset :: Mapper s -> Word16 -> ST s Int
prgOffset m a = case mpKind m of
  1 -> do
    ctl <- reg m rControl
    bank <- (.&. 0x0F) <$> reg m rPrg
    let lastBank = (mpPrgMask m + 1) `div` 0x4000 - 1
    pure $ case (ctl `shiftR` 2) .&. 3 of
      2 -> if hi then bank * 0x4000 + lo14 else lo14
      3 -> if hi then lastBank * 0x4000 + lo14 else bank * 0x4000 + lo14
      _ -> (bank .&. 0x0E) * 0x4000 + lo15
  2 -> do
    bank <- reg m rBank
    let lastBank = (mpPrgMask m + 1) `div` 0x4000 - 1
    pure (if hi then lastBank * 0x4000 + lo14 else bank * 0x4000 + lo14)
  7 -> do
    bank <- (.&. 7) <$> reg m rBank
    pure (bank * 0x8000 + lo15)
  4 -> do
    sel <- reg m rSel
    let banks = (mpPrgMask m + 1) `div` 0x2000
        slot = (fromIntegral a `shiftR` 13) .&. 3 :: Int
        -- PRG mode 1 swaps $8000 and $C000
        slot' = if testBit sel 6 && slot /= 1 && slot /= 3 then 2 - slot else slot
    bank <- case slot' of
      0 -> reg m (rR0 + 6)
      1 -> reg m (rR0 + 7)
      2 -> pure (banks - 2)
      _ -> pure (banks - 1)
    pure (bank * 0x2000 + (fromIntegral a .&. 0x1FFF))
  _ -> pure lo15
 where
  hi = a >= 0xC000
  lo14 = fromIntegral a .&. 0x3FFF
  lo15 = fromIntegral a .&. 0x7FFF

-- | CPU write of $4020-$FFFF, on CPU cycle @cyc@.
cpuWriteCart :: Mapper s -> Int -> Word16 -> Word8 -> ST s ()
cpuWriteCart m cyc a v
  | mpKind m == 5 = mmc5Write m a v
  | a >= 0x8000 = case mpKind m of
      1 -> mmc1Write m cyc a v
      2 -> MV.unsafeWrite (mpRegs m) rBank (fromIntegral v .&. 0x0F)
      3 -> MV.unsafeWrite (mpRegs m) rBank (fromIntegral v .&. 0x03)
      7 -> MV.unsafeWrite (mpRegs m) rBank (fromIntegral v .&. 0x17)
      4 -> mmc3Write m a v
      _ -> pure ()
  | a >= 0x6000 && mpHasRam m = MV.unsafeWrite (mpPrgRam m) (fromIntegral a .&. 0x1FFF) v
  | otherwise = pure ()

mmc1Write :: Mapper s -> Int -> Word16 -> Word8 -> ST s ()
mmc1Write m cyc a v = do
  lastW <- reg m rLastWrite
  MV.unsafeWrite (mpRegs m) rLastWrite cyc
  when (cyc /= lastW + 1) $
    if testBit v 7
      then do
        MV.unsafeWrite (mpRegs m) rShift 0
        MV.unsafeWrite (mpRegs m) rCount 0
        ctl <- reg m rControl
        MV.unsafeWrite (mpRegs m) rControl (ctl .|. 0x0C)
      else do
        sh <- reg m rShift
        n <- reg m rCount
        let sh' = (sh `shiftR` 1) .|. (if testBit v 0 then 0x10 else 0)
        if n == 4
          then do
            let target = case (a `shiftR` 13) .&. 3 of
                  0 -> rControl
                  1 -> rChr0
                  2 -> rChr1
                  _ -> rPrg
            MV.unsafeWrite (mpRegs m) target sh'
            MV.unsafeWrite (mpRegs m) rShift 0
            MV.unsafeWrite (mpRegs m) rCount 0
          else do
            MV.unsafeWrite (mpRegs m) rShift sh'
            MV.unsafeWrite (mpRegs m) rCount (n + 1)

-- | CHR index of a pattern table address ($0000-$1FFF).
chrIndex :: Mapper s -> Word16 -> ST s Int
chrIndex m a = case mpKind m of
  1 -> do
    ctl <- reg m rControl
    if testBit ctl 4
      then do
        b <- reg m (if a >= 0x1000 then rChr1 else rChr0)
        pure ((b * 0x1000 + lo12) .&. mpChrMask m)
      else do
        b <- reg m rChr0
        pure (((b .&. 0x1E) * 0x1000 + lo13) .&. mpChrMask m)
  3 -> do
    b <- reg m rBank
    pure ((b * 0x2000 + lo13) .&. mpChrMask m)
  4 -> do
    sel <- reg m rSel
    -- CHR A12 inversion swaps the 2 KiB and the 1 KiB halves
    let a' = if testBit sel 7 then a `xor` 0x1000 else a
        k = fromIntegral (a' `shiftR` 10) .&. 7 :: Int
    b <- if k < 4 then (\r -> (r .&. 0xFE) + (k .&. 1)) <$> reg m (rR0 + k `shiftR` 1)
                  else reg m (rR0 + k - 2)
    pure ((b * 0x400 + (fromIntegral a .&. 0x3FF)) .&. mpChrMask m)
  _ -> pure (lo13 .&. mpChrMask m)
 where
  lo12 = fromIntegral a .&. 0x0FFF
  lo13 = fromIntegral a .&. 0x1FFF

-- (Q.7: inlined: no boxed byte per read)
{-# INLINE chrRead #-}
chrRead :: Mapper s -> Word16 -> ST s Word8
chrRead m a = chrIndex m a >>= MV.unsafeRead (mpChr m)

chrWrite :: Mapper s -> Word16 -> Word8 -> ST s ()
chrWrite m a v = when (mpChrRam m) (chrIndex m a >>= \i -> MV.unsafeWrite (mpChr m) i v)

-- | A PPU read (a rendering fetch of this kind, or $2007's: 'Nothing'),
-- with the console's nametable RAM (CIRAM).
-- (Q.7: inlined: no boxed byte per read)
{-# INLINE ppuRead #-}
ppuRead :: Mapper s -> MV.MVector s Word8 -> Maybe FetchKind -> Word16 -> ST s Word8
ppuRead m ciram k a
  | mpKind m == 5 = mmc5PpuRead m ciram k a
  | otherwise = do
      mapperA12 m a
      if a < 0x2000 then chrRead m a else ciramIndex m a >>= MV.unsafeRead ciram

-- | A PPU write ($2007's).
ppuWrite :: Mapper s -> MV.MVector s Word8 -> Word16 -> Word8 -> ST s ()
ppuWrite m ciram a v
  | mpKind m == 5 = mmc5PpuWrite m ciram a v
  | otherwise = do
      mapperA12 m a
      if a < 0x2000 then chrWrite m a v else ciramIndex m a >>= \i -> MV.unsafeWrite ciram i v

-- | A CPU read the cartridge watches besides its own space (MMC5: the NMI
-- vector, $FFFA/$FFFB, ends the frame). Not for side-effect-free peeks.
mapperCpuRead :: Mapper s -> Word16 -> ST s ()
mapperCpuRead m a = when (mpKind m == 5 && (a == 0xFFFA || a == 0xFFFB)) $ do
  w m5InFrame 0
  w m5IrqPend 0
  w m5Line 0
 where
  w = MV.unsafeWrite (mpRegs m)

-- | A CPU write of $2000-$3FFF, fully decoded (MMC5 snoops $2000 and $2001,
-- not their mirrors).
mapperPpuReg :: Mapper s -> Word16 -> Word8 -> ST s ()
mapperPpuReg m a v = when (mpKind m == 5) $ case a of
  0x2000 -> w m5Ctrl d
  0x2001 -> do
    old <- reg m m5Mask
    -- rendering off: no substitutions, the frame ends; rendering on again:
    -- the line detection starts over
    when (d .&. 0x18 == 0 || old .&. 0x18 == 0) $ do
      w m5InFrame 0
      w m5Match 0
      w m5LastAddr (-1)
    w m5Mask d
  _ -> pure ()
 where
  d = fromIntegral v
  w = MV.unsafeWrite (mpRegs m)

-- * MMC5

-- | Where a CPU address of $6000-$FFFF goes: PRG RAM (Left, an index into
-- 64 KiB) or ROM (Right, before the size mask).
mmc5Prg :: Mapper s -> Word16 -> ST s (Either Int Int)
mmc5Prg m a
  | a < 0x8000 = (\b -> Left ((b .&. 7) * 0x2000 + lo13)) <$> reg m m5Prg
  | otherwise = do
      mode <- reg m m5PrgMode
      let slot = (fromIntegral a `shiftR` 13) .&. 3 :: Int
          a13 = slot .&. 1
          bankOf r mask low = (\b -> (b, (b .&. mask) .|. low)) <$> reg m (m5Prg + r)
      (raw, b) <- case mode of
        0 -> bankOf 4 0x7C slot
        1 -> if slot < 2 then bankOf 2 0x7E a13 else bankOf 4 0x7E a13
        2 -> case slot of
          0 -> bankOf 2 0x7E 0
          1 -> bankOf 2 0x7E 1
          2 -> bankOf 3 0x7F 0
          _ -> bankOf 4 0x7F 0
        _ -> bankOf (slot + 1) 0x7F 0
      -- $5117 (and mode 0's and mode 1's $C000) is always ROM
      let rom = testBit raw 7 || (mode == 0) || (mode == 1 && slot >= 2) || slot == 3
      pure (if rom then Right (b * 0x2000 + lo13) else Left ((b .&. 7) * 0x2000 + lo13))
 where
  lo13 = fromIntegral a .&. 0x1FFF

mmc5Read :: Mapper s -> Word16 -> ST s Int
mmc5Read m a
  | a >= 0x6000 = mmc5Prg m a >>= \t -> case t of
      Left i -> fromIntegral <$> MV.unsafeRead (mpPrgRam m) i
      Right i -> pure (fromIntegral (U.unsafeIndex (mpPrg m) (i .&. mpPrgMask m)))
  | a >= 0x5C00 = do
      mode <- reg m m5ExMode
      if mode >= 2 then fromIntegral <$> MV.unsafeRead (mpExram m) (fromIntegral a .&. 0x3FF) else pure (-1)
  | a == 0x5204 = do
      p <- reg m m5IrqPend
      f <- reg m m5InFrame
      MV.unsafeWrite (mpRegs m) m5IrqPend 0
      pure ((if p /= 0 then 0x80 else 0) .|. (if f /= 0 then 0x40 else 0))
  | a == 0x5205 = (\x y -> (x * y) .&. 0xFF) <$> reg m m5Mul1 <*> reg m m5Mul2
  | a == 0x5206 = (\x y -> (x * y) `shiftR` 8) <$> reg m m5Mul1 <*> reg m m5Mul2
  | otherwise = pure (-1)

mmc5Write :: Mapper s -> Word16 -> Word8 -> ST s ()
mmc5Write m a v
  | a >= 0x6000 = do
      p1 <- reg m m5Prot1
      p2 <- reg m m5Prot2
      t <- mmc5Prg m a
      case t of
        Left i | p1 .&. 3 == 2 && p2 .&. 3 == 1 -> MV.unsafeWrite (mpPrgRam m) i v
        _ -> pure ()
  | a >= 0x5C00 = do
      mode <- reg m m5ExMode
      f <- reg m m5InFrame
      let i = fromIntegral a .&. 0x3FF
      case mode of
        2 -> MV.unsafeWrite (mpExram m) i v
        3 -> pure ()
        -- nametable / extended attribute modes: written while rendering,
        -- 0 outside it (as most emulators; the hardware is unreliable then)
        _ -> MV.unsafeWrite (mpExram m) i (if f /= 0 then v else 0)
  | otherwise = case a of
      0x5100 -> w m5PrgMode (d .&. 3)
      0x5101 -> w m5ChrMode (d .&. 3)
      0x5102 -> w m5Prot1 (d .&. 3)
      0x5103 -> w m5Prot2 (d .&. 3)
      0x5104 -> w m5ExMode (d .&. 3)
      0x5105 -> w m5Nt d
      0x5106 -> w m5FillT d
      0x5107 -> w m5FillA (d .&. 3)
      0x5203 -> w m5IrqCmp d
      0x5204 -> w m5IrqEn (if testBit d 7 then 1 else 0)
      0x5205 -> w m5Mul1 d
      0x5206 -> w m5Mul2 d
      _ | a >= 0x5113 && a <= 0x5117 -> w (m5Prg + fromIntegral (a - 0x5113)) d
        | a >= 0x5120 && a <= 0x512B -> do
            w (m5Chr + fromIntegral (a - 0x5120)) d
            w m5LastB (if a >= 0x5128 then 1 else 0)
        | otherwise -> pure ()
 where
  d = fromIntegral v
  w = MV.unsafeWrite (mpRegs m)

-- | The snooped PPU state: 8x16 sprites with rendering on (separate
-- sprite and background CHR sets), rendering on (substitutions).
mmc5Modes :: Mapper s -> ST s (Bool, Bool)
mmc5Modes m = do
  c <- reg m m5Ctrl
  k <- reg m m5Mask
  let on = k .&. 0x18 /= 0
  pure (on && testBit c 5, on)

-- | CHR index for a pattern address with one register set (False: $5120
-- to $5127, True: $5128 to $512B).
mmc5ChrIndex :: Mapper s -> Bool -> Word16 -> ST s Int
mmc5ChrIndex m setB a = do
  mode <- reg m m5ChrMode
  let ai = fromIntegral a :: Int
      r i = reg m (m5Chr + i)
  i <- case (mode, setB) of
    (0, False) -> (\b -> b * 0x2000 + (ai .&. 0x1FFF)) <$> r 7
    (1, False) -> (\b -> b * 0x1000 + (ai .&. 0xFFF)) <$> r (if ai < 0x1000 then 3 else 7)
    (2, False) -> (\b -> b * 0x800 + (ai .&. 0x7FF)) <$> r (1 + 2 * ((ai `shiftR` 11) .&. 3))
    (_, False) -> (\b -> b * 0x400 + (ai .&. 0x3FF)) <$> r ((ai `shiftR` 10) .&. 7)
    (0, True) -> (\b -> b * 0x2000 + (ai .&. 0x1FFF)) <$> r 11
    (1, True) -> (\b -> b * 0x1000 + (ai .&. 0xFFF)) <$> r 11
    (2, True) -> (\b -> b * 0x800 + (ai .&. 0x7FF)) <$> r (9 + 2 * ((ai `shiftR` 11) .&. 1))
    (_, True) -> (\b -> b * 0x400 + (ai .&. 0x3FF)) <$> r (8 + ((ai `shiftR` 10) .&. 3))
  pure (i .&. mpChrMask m)

-- | A nametable read ($2000-$2FFF; attribute bytes too) through $5105.
mmc5Nt :: Mapper s -> MV.MVector s Word8 -> Word16 -> ST s Word8
mmc5Nt m ciram a = do
  nt <- reg m m5Nt
  ex <- reg m m5ExMode
  let q = (fromIntegral a `shiftR` 10) .&. 3 :: Int
      i = fromIntegral a .&. 0x3FF
  case (nt `shiftR` (2 * q)) .&. 3 of
    2 -> if ex <= 1 then MV.unsafeRead (mpExram m) i else pure 0
    3 -> if i >= 0x3C0 then (\p -> fromIntegral (p * 0x55)) <$> reg m m5FillA
                       else fromIntegral <$> reg m m5FillT
    p -> MV.unsafeRead ciram (p * 0x400 + i)

mmc5PpuRead :: Mapper s -> MV.MVector s Word8 -> Maybe FetchKind -> Word16 -> ST s Word8
mmc5PpuRead m ciram k a = do
  (tall, on) <- mmc5Modes m
  ex <- reg m m5ExMode
  let ext = on && ex == 1
  -- the scanline detection: rendering fetches only
  case k of
    Nothing -> pure ()
    Just _ -> do
      w m5Fetched 1
      lastA <- reg m m5LastAddr
      if a >= 0x2000 && fromIntegral a == lastA
        then do
          n <- (+ 1) <$> reg m m5Match
          w m5Match n
          when (n == 2) $ do
            f <- reg m m5InFrame
            if f == 0
              then w m5InFrame 1 >> w m5Line 0
              else do
                l <- (+ 1) <$> reg m m5Line
                w m5Line l
                c <- reg m m5IrqCmp
                when (c /= 0 && l == c) (w m5IrqPend 1)
        else w m5Match 0
      w m5LastAddr (fromIntegral a)
  case k of
    Just FkNt -> do
      when ext (MV.unsafeRead (mpExram m) (fromIntegral a .&. 0x3FF) >>= w m5ExLatch . fromIntegral)
      mmc5Nt m ciram a
    Just FkAt | ext -> (\b -> fromIntegral ((b `shiftR` 6) * 0x55)) <$> reg m m5ExLatch
    Just FkBgPat
      | ext -> do
          b <- reg m m5ExLatch
          MV.unsafeRead (mpChr m) (((b .&. 0x3F) * 0x1000 + (fromIntegral a .&. 0xFFF)) .&. mpChrMask m)
      | otherwise -> mmc5ChrIndex m tall a >>= MV.unsafeRead (mpChr m)
    Just FkSprPat -> mmc5ChrIndex m False a >>= MV.unsafeRead (mpChr m)
    Nothing | a < 0x2000 -> do
      lastB <- reg m m5LastB
      mmc5ChrIndex m (tall && lastB /= 0) a >>= MV.unsafeRead (mpChr m)
    _ -> mmc5Nt m ciram a
 where
  w = MV.unsafeWrite (mpRegs m)

mmc5PpuWrite :: Mapper s -> MV.MVector s Word8 -> Word16 -> Word8 -> ST s ()
mmc5PpuWrite m ciram a v
  | a < 0x2000 = when (mpChrRam m) $ do
      (tall, _) <- mmc5Modes m
      lastB <- reg m m5LastB
      i <- mmc5ChrIndex m (tall && lastB /= 0) a
      MV.unsafeWrite (mpChr m) i v
  | otherwise = do
      nt <- reg m m5Nt
      ex <- reg m m5ExMode
      let q = (fromIntegral a `shiftR` 10) .&. 3 :: Int
          i = fromIntegral a .&. 0x3FF
      case (nt `shiftR` (2 * q)) .&. 3 of
        2 -> when (ex <= 1) (MV.unsafeWrite (mpExram m) i v)
        3 -> pure ()
        p -> MV.unsafeWrite ciram (p * 0x400 + i) v

-- | Index into the 2 KiB of console nametable RAM (CIRAM) for a
-- nametable address ($2000-$3EFF): which 1 KiB page answers.
ciramIndex :: Mapper s -> Word16 -> ST s Int
ciramIndex m a = do
  page <- case mpKind m of
    1 -> do
      ctl <- reg m rControl
      pure $ case ctl .&. 3 of
        0 -> 0
        1 -> 1
        2 -> nt .&. 1
        _ -> nt `shiftR` 1
    7 -> (\b -> if testBit b 4 then 1 else 0) <$> reg m rBank
    4 -> (\h -> if h /= 0 then nt `shiftR` 1 else nt .&. 1) <$> reg m rMirror
    _ -> pure $ case mpFixed m of
      Vertical -> nt .&. 1
      _ -> nt `shiftR` 1
  pure (page * 0x400 + (fromIntegral a .&. 0x3FF))
 where
  nt = (fromIntegral a `shiftR` 10) .&. 3 :: Int

-- | MMC3 registers (nesdev wiki "MMC3"): $8000-$9FFF bank select / data,
-- $A000-$BFFF mirroring / PRG RAM protect (ignored, as MMC1's), $C000-$DFFF
-- IRQ latch / reload, $E000-$FFFF IRQ disable (acknowledge) / enable; even
-- and odd addresses by A0.
mmc3Write :: Mapper s -> Word16 -> Word8 -> ST s ()
mmc3Write m a v = case (a .&. 0xE000, testBit a 0) of
  (0x8000, False) -> w rSel d
  (0x8000, True) -> do
    sel <- reg m rSel
    w (rR0 + (sel .&. 7)) d
  (0xA000, False) -> w rMirror (d .&. 1)
  (0xA000, True) -> pure ()
  (0xC000, False) -> w rIrqLatch d
  (0xC000, True) -> w rIrqCount 0 >> w rIrqReload 1
  (0xE000, False) -> w rIrqEn 0 >> w rIrqFlag 0
  _ -> w rIrqEn 1
 where
  d = fromIntegral v
  w = MV.unsafeWrite (mpRegs m)

-- | CPU cycles PPU A12 must have been low before a rise clocks the MMC3's
-- scanline counter (the M2 filter; nesdev wiki "MMC3", "IRQ").
a12Filter :: Int
a12Filter = 3

-- | The PPU put this address on its bus (an access, or v while rendering is
-- off): the MMC3 watches A12.
mapperA12 :: Mapper s -> Word16 -> ST s ()
mapperA12 m a = when (mpKind m == 4) $ do
  prev <- reg m rA12
  if testBit a 12
    then do
      low <- reg m rA12Low
      when (prev == 0 && low >= a12Filter) (mmc3Clock m)
      w rA12 1
      w rA12Low 0
    else w rA12 0
 where
  w = MV.unsafeWrite (mpRegs m)

-- | One CPU cycle (an M2 edge): the A12 filter counts while A12 is low.
mapperCycle :: Mapper s -> ST s ()
mapperCycle m
  | mpKind m == 5 = do
      -- MMC5: the frame ends after 3 CPU cycles without a rendering fetch
      f <- reg m m5Fetched
      n <- if f /= 0 then pure 0 else (+ 1) <$> reg m m5Idle
      MV.unsafeWrite (mpRegs m) m5Fetched 0
      MV.unsafeWrite (mpRegs m) m5Idle (min 3 n)
      when (n >= 3) $ do
        MV.unsafeWrite (mpRegs m) m5InFrame 0
        MV.unsafeWrite (mpRegs m) m5Match 0
        MV.unsafeWrite (mpRegs m) m5LastAddr (-1)
  | otherwise = when (mpKind m == 4) $ do
  prev <- reg m rA12
  low <- reg m rA12Low
  when (prev == 0 && low < 15) (MV.unsafeWrite (mpRegs m) rA12Low (low + 1))

-- | The MMC3's scanline counter, clocked by a filtered A12 rise: reload
-- from the latch when 0 or asked to, else count down; at 0 the IRQ fires
-- if enabled.
mmc3Clock :: Mapper s -> ST s ()
mmc3Clock m = do
  reg m rClocks >>= w rClocks . (+ 1)
  c <- reg m rIrqCount
  rl <- reg m rIrqReload
  c' <- if c == 0 || rl /= 0 then reg m rIrqLatch else pure (c - 1)
  w rIrqCount c'
  w rIrqReload 0
  en <- reg m rIrqEn
  when (c' == 0 && en /= 0) (w rIrqFlag 1)
 where
  w = MV.unsafeWrite (mpRegs m)

-- | The cartridge's IRQ line (MMC3, MMC5).
mapperIrq :: Mapper s -> ST s Bool
mapperIrq m
  | mpKind m == 5 = (\p e -> p /= 0 && e /= 0) <$> reg m m5IrqPend <*> reg m m5IrqEn
  | otherwise = (/= 0) <$> reg m rIrqFlag

-- | MMC3 counter clocks so far (tests).
mapperClocks :: Mapper s -> ST s Int
mapperClocks m = reg m rClocks

-- | The MMC3's (MMC5's) scanline counter (the lockstep compares it after each step).
mapperCounter :: Mapper s -> ST s Int
mapperCounter m = reg m (if mpKind m == 5 then m5Line else rIrqCount)

-- * Save states (side track N, "Nes.Spec.SaveState")

-- | The board as a save state keeps it: the mapper number, the header's
-- mirroring (0 horizontal, 1 vertical, 2 four-screen), CHR RAM and PRG
-- RAM there, and the register slots (the names above).
data MapperSnap = MapperSnap
  { msKind   :: Int
  , msMirror :: Int
  , msChrRam :: Bool
  , msPrgRam :: Bool
  , msRegs   :: U.Vector Int
  } deriving stock (Show, Eq)

mapperSnapshot :: Mapper s -> ST s MapperSnap
mapperSnapshot m = MapperSnap (mpKind m) mir (mpChrRam m) (mpHasRam m) <$> U.freeze (mpRegs m)
 where
  mir = case mpFixed m of
    Horizontal -> 0
    Vertical -> 1
    FourScreen -> 2

-- | The board's memories a state keeps: PRG RAM (8 KiB, MMC5's 64 KiB;
-- empty without it), CHR RAM (empty with CHR ROM), MMC5's ExRAM (empty on
-- the others).
mapperImages :: Mapper s -> ST s (U.Vector Word8, U.Vector Word8, U.Vector Word8)
mapperImages m = (,,) <$> (if mpHasRam m then U.freeze (mpPrgRam m) else pure U.empty)
                      <*> (if mpChrRam m then U.freeze (mpChr m) else pure U.empty)
                      <*> (if mpKind m == 5 then U.freeze (mpExram m) else pure U.empty)

-- | The same written back (as the PS does through its windows while the
-- core is held); an empty or missing image leaves its memory.
mapperLoad :: Mapper s -> (U.Vector Word8, U.Vector Word8, U.Vector Word8) -> ST s ()
mapperLoad m (prg, chr, ex) = do
  put (mpPrgRam m) prg
  when (mpChrRam m) (put (mpChr m) chr)
  put (mpExram m) ex
 where
  put v img = U.imapM_ (\i b -> when (i < MV.length v) (MV.unsafeWrite v i b)) img

-- | The cartridge's state for a save state (Q.8, "Platform.Snapshot"):
-- its registers and RAM (CHR too when it is RAM).
mapperFields :: Mapper s -> [Field s]
mapperFields c =
  [ FInt (mpRegs c), FW8 (mpPrgRam c), FW8 (mpExram c) ] <> [ FW8 (mpChr c) | mpChrRam c ]
