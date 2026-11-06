{-|
State, memory map and M-cycle peripherals of the Game Boy reference model
(PLAN.md step 2.1). The CPU ("Gb.Spec.Cpu") drives it through three
operations, each one M-cycle long: 'busRead', 'busWrite' and 'busIdle'.
Each first advances the peripherals by one M-cycle ('tick': timer, serial
port, LCD timing) and then performs the CPU's access, so a value the
timer produces in an M-cycle is visible to an access in the same M-cycle.
Pan Docs' timer table ("Timer obscure behaviour", cycles A and B) comes
out of that order: TIMA reads 0 in the M-cycle in which it overflowed,
TMA is copied and IF bit 2 set in the next one.

State lives in unboxed mutable vectors in 'ST' (docs/systems/gameboy.md:
"mutable unboxed state behind a pure interface"); "Gb.Spec" wraps it into
a pure runner.

What is modelled here, and what waits for later steps:

* __Timer__ (DIV, TIMA, TMA, TAC): the 16-bit system counter, falling-edge
  increments (including the edges caused by writes to DIV and TAC, DMG
  behaviour), the delayed reload with Pan Docs' three rules.
* __Serial__: internal-clock transfers shift on falling edges of system
  counter bit 8 (8192 Hz); with nothing connected, 1s are shifted in, so
  SB reads 0xFF afterwards. After 8 bits SC bit 7 clears and IF bit 3 is
  set. Every byte sent is also captured ('Gb.Hooks.serialWrite').
* __PPU__: "Gb.Spec.Ppu", four dots per M-cycle. CPU accesses to VRAM
  are ignored (writes) or read 0xFF in mode 3, to OAM in modes 2 and 3
  and while OAM DMA copies.
* __OAM DMA__ (Pan Docs, "OAM DMA Transfer"; Mooneye @oam_dma_start@,
  @oam_dma_timing@): a write to 0xFF46 in M-cycle N starts the copy in
  N + 2 (OAM is still accessible in N + 1); one byte per M-cycle for 160
  M-cycles, OAM blocked meanwhile. A new write restarts it, the running
  copy continuing until the new one starts. Other DMA bus conflicts (the
  CPU reading the byte the DMA is moving) are not modelled.
* __Cartridges__: no MBC (with or without RAM) and MBC1 (without the
  multicart wiring).
* __Joypad__: P1 with no buttons pressed until the core gets inputs.
-}
module Gb.Spec.Machine
  ( -- * Machine
    module Gb.Spec.State
  , newMachine
  , newMachineModel
  , newMachineModelPal
  , cgbDefaultPalette
    -- * One M-cycle each
  , busRead
  , busWrite
  , busIdle
  , stopInstr
  , hdmaPending
  , doubleSpeed
    -- * No time (debugging, tests)
  , peek
  , poke
  ) where

import Prelude

import Control.Monad (when)
import Control.Monad.ST (ST)
import Data.Bits
import qualified Data.ByteString as BS
import Data.STRef
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import Data.Word

import Gb.Cartridge
import Gb.Hooks (serialIdle, serialWrite)
import Gb.Spec.Apu
import Gb.Spec.Ppu
import Gb.Spec.State

-- | A DMG right after the boot ROM hands over at PC = 0x0100 (Pan Docs,
-- "Power Up Sequence", DMG column). The system counter starts at 0xABC8,
-- which gives Pan Docs' DIV = 0xAB with the phase Mooneye's
-- @boot_div-dmgABCmgb@ expects. The PPU starts at 'handOffLine' /
-- 'handOffDot' (Pan Docs: LY = 0 and STAT = 0x85, so mode 1 and line 153
-- after its LY = 153 M-cycle). WRAM, HRAM and cartridge RAM start at 0.
newMachine :: BS.ByteString -> ST s (Either String (Machine s))
newMachine = newMachineModel Dmg

-- | A machine of the given model (side track I, phase 3). The Game Boy
-- Color starts as its boot ROM leaves it (Pan Docs, "Power Up Sequence",
-- CGB column; docs/systems/gameboy.md, "Phase 3, step 1"): in CGB mode
-- for a cartridge with bit 7 of 0x143 set, else in DMG compatibility mode
-- with our default palette ('cgbDefaultPalette').
newMachineModel :: Model -> BS.ByteString -> ST s (Either String (Machine s))
newMachineModel model = newMachineModelPal model (replicate 3 cgbDefaultPalette)

-- | 'newMachineModel' with the Game Boy Color's palettes for a DMG
-- cartridge given: BG 0, OBJ 0, OBJ 1, four 15-bit colours each (what the
-- PS app writes into Gb.Loader's palette window: the boot ROM's choice for
-- the title, or the player's; platform/ps/gbc_palette.zig).
newMachineModelPal :: Model -> [[Int]] -> BS.ByteString -> ST s (Either String (Machine s))
newMachineModelPal model pals rom = case parseHeader rom of
  Left e -> pure (Left e)
  Right h -> case cartType (hdCartType h) of
    Nothing -> pure (Left ("cartridge type " <> show (hdCartType h) <> " is not supported"))
    Just ct
      | ctMbc ct `notElem` [NoMbc, Mbc1, Mbc2, Mbc3, Mbc5] ->
          pure (Left (show (ctMbc ct) <> " is not supported yet"))
      | BS.length rom `mod` 0x4000 /= 0 ->
          pure (Left ("ROM image of " <> show (BS.length rom) <> " bytes is not a whole number of banks"))
      | otherwise -> do
          let ramSize
                | ctMbc ct == Mbc2 = 512 -- built in, 4 bits each (Pan Docs, "MBC2")
                | ctRam ct = maybe 0 id (ramSizeBytes (hdRamSize h))
                | otherwise = 0
          regs <- MU.replicate 8 0
          st <- MU.replicate iNST 0
          ram <- MU.replicate 0x8000 0
          cram <- MU.replicate (max 1 ramSize) 0
          fb <- MU.replicate (160 * 144) 0
          lastFb <- MU.replicate (160 * 144) 0
          spr <- MU.replicate 70 0
          apu <- MU.replicate apuStateSize 0
          tr <- newSTRef []
          ser <- newSTRef serialIdle
          hook <- newSTRef Nothing
          vram1 <- MU.replicate 0x2000 0
          wramx <- MU.replicate 0x6000 0
          pal <- MU.replicate 128 0
          fbc <- MU.replicate (160 * 144) 0x7FFF
          lastc <- MU.replicate (160 * 144) 0x7FFF
          let cgbMode = model == Cgb && testBit (BS.index rom 0x143) 7
              m = Machine regs st ram cram (U.fromList (BS.unpack rom)) (ctMbc ct)
                    (ramSize > 0) (BS.length rom `div` 0x4000) fb lastFb apu spr tr ser hook
                    model cgbMode vram1 wramx pal fbc lastc
              hc = hdHeaderSum h /= 0
              -- DMG compatibility mode (Pan Docs, "CPU registers", CGB (DMG
              -- mode)): B the title's sum for a Nintendo licensee, else 0
              nintendo = BS.index rom 0x14B == 0x01
                || (BS.index rom 0x14B == 0x33 && BS.index rom 0x144 == 0x30 && BS.index rom 0x145 == 0x31)
              compatB = if nintendo then BS.foldl' (+) 0 (BS.take 16 (BS.drop 0x134 rom)) else 0
              (compatH, compatL) = if compatB == 0x43 || compatB == 0x58 then (0x99, 0x1A) else (0x00, 0x7C)
          sequence_ [ setR m r v | (r, v) <- case model of
            Dmg ->
              [ (rA, 0x01), (rF, 0x80 .|. (if hc then 0x30 else 0)), (rB, 0x00), (rC, 0x13)
              , (rD, 0x00), (rE, 0xD8), (rH, 0x01), (rL, 0x4D) ]
            Cgb | cgbMode ->
              [ (rA, 0x11), (rF, 0x80), (rB, 0x00), (rC, 0x00)
              , (rD, 0xFF), (rE, 0x56), (rH, 0x00), (rL, 0x0D) ]
                | otherwise ->
              [ (rA, 0x11), (rF, 0x80), (rB, compatB), (rC, 0x00)
              , (rD, 0x00), (rE, 0x08), (rH, compatH), (rL, compatL) ] ]
          sequence_ [ setSt m i v | (i, v) <-
            [ (iPC, 0x0100), (iSP, 0xFFFE), (iDIV, 0xABC8), (iTAC, 0), (iIF, 0x01)
            , (iLCDC, 0x91), (iP1, 0x00), (iROMB, 1), (iJOYPREV, 0x0F)
            , (iLY, handOffLine), (iDOT, handOffDot), (iPPUMODE, 1), (iWINX, -1), (iDMAIDX, -1)
            , (iTIMER, if hasTimer (hdCartType h) then 1 else 0), (iHDMALEN, 0x7F) ] ]
          -- registers kept in the I/O backing store (Pan Docs table)
          sequence_ [ MU.write ram (a - 0x8000) v | (a, v) <-
            [ (0xFF10, 0x80), (0xFF11, 0xBF), (0xFF12, 0xF3), (0xFF13, 0xFF), (0xFF14, 0xBF)
            , (0xFF16, 0x3F), (0xFF17, 0x00), (0xFF18, 0xFF), (0xFF19, 0xBF)
            , (0xFF1A, 0x7F), (0xFF1B, 0xFF), (0xFF1C, 0x9F), (0xFF1D, 0xFF), (0xFF1E, 0xBF)
            , (0xFF20, 0xFF), (0xFF21, 0x00), (0xFF22, 0x00), (0xFF23, 0xBF)
            , (0xFF24, 0x77), (0xFF25, 0xF3), (0xFF26, 0xF1)
            , (0xFF46, 0xFF), (0xFF47, 0xFC), (0xFF48, 0xFF), (0xFF49, 0xFF) ] ]
          -- the Game Boy Color's registers and palettes at the hand-off
          when (model == Cgb) $ do
            -- SC: $7F in CGB mode (Pan Docs), $7E in compatibility mode
            -- (Mooneye boot_hwio-C, verified on hardware)
            setSt m iSC (if cgbMode then 0x01 else 0x00)
            -- P1: both groups deselected (reads $FF: Mooneye boot_hwio-C,
            -- verified on hardware; Pan Docs' table says $C7 or $CF)
            setSt m iP1 0x30
            -- the system counter: $2674, the only value (in steps of 4)
            -- from $2000 to $2FFC with which Mooneye's boot_div-cgbABCDE
            -- passes (a search on the spec, 2026-10-05). That test runs
            -- in compatibility mode; CGB mode's boot ROM takes another
            -- time, which no test pins down: the same value (Pan Docs:
            -- "just don't rely on these")
            setSt m iDIV 0x2674
            ramWrite m 0xFF46 0x00
            -- BCPS/OCPS: $C8/$D0 after the compatibility palettes
            -- (Mooneye boot_hwio-C); in CGB mode the boot ROM's last
            -- writes leave index 0 / 1 with auto-increment (ISSOtm's
            -- disassembly: all 64 BG bytes, then one OBJ byte)
            setSt m iBCPS (if cgbMode then 0x80 else 0x88)
            setSt m iOCPS (if cgbMode then 0x81 else 0x90)
            setSt m iOPRI (if cgbMode then 0 else 1)
            let put i c = MU.write pal i (fromIntegral c) >> MU.write pal (i + 1) (fromIntegral (c `shiftR` 8))
            if cgbMode
              then sequence_ [ put (2 * k) (0x7FFF :: Int) | k <- [0 .. 31] ] -- BG all white
              else sequence_ [ put (base + 2 * k) c | (base, p) <- zip [0, 64, 72] pals, (k, c) <- zip [0 ..] p ]
          -- the APU at the hand-off: on, CH1 still on (NR52 = 0xF1) with its
          -- envelope run down to 0 after the boot sound
          MU.write apu 0 1
          MU.write apu 8 1
          pure (Right m)

-- | Our palette for a DMG cartridge on the Game Boy Color (the user,
-- 2026-10-07; before, the boot ROM's for a game it doesn't recognise):
-- the boot demo "rota"'s Game Boy shades, lightest first, #F4ECD8 #F7A41D
-- #22A699 #0E0B14, each channel the nearest 5-bit value, as 15-bit BGR.
-- BG 0, OBJ 0 and OBJ 1 alike unless the PS app picks others
-- ('newMachineModelPal').
cgbDefaultPalette :: [Int]
cgbDefaultPalette = [0x6BBE, 0x129E, 0x4E84, 0x0822]

-- | Where the PPU is at the hand-off: line 153, dot 'handOffDot'.
-- Mooneye's @boot_hwio-dmgABCmgb@ (LY = 0x0A and STAT = 0x80 when it
-- reads them) passes for any dot from 256 to 452 (multiples of 4: the
-- M-cycle phase @hblank_ly_scx_timing-GS@ needs); 400 is within that
-- range, not a measured value.
handOffLine, handOffDot :: Int
handOffLine = 153
handOffDot = 400

-- * Bus operations (one M-cycle each)

record :: Machine s -> Access -> ST s ()
record m a = do
  t <- getSt m iTRACE
  when (t /= 0) $ modifySTRef' (mTrace m) (a :)
{-# INLINE record #-}

busRead :: Machine s -> Word16 -> ST s Word8
busRead m a = do
  tick m
  v <- cpuRead m a
  record m (ARead a v)
  pure v

busWrite :: Machine s -> Word16 -> Word8 -> ST s ()
busWrite m a v = do
  tick m
  cpuWrite m a v
  record m (AWrite a v)

busIdle :: Machine s -> ST s ()
busIdle m = do
  tick m
  record m AIdle

-- | The CPU's view: VRAM and OAM may be blocked.
cpuRead :: Machine s -> Word16 -> ST s Word8
cpuRead m a
  | a >= 0x8000 && a < 0xA000 = do
      b <- vramReadBlocked m
      if b then pure 0xFF else peek m a
  | a >= 0xFE00 && a < 0xFF00 = do
      b <- oamReadBlocked m
      if b then pure 0xFF else peek m a
  | otherwise = peek m a
{-# INLINE cpuRead #-}

cpuWrite :: Machine s -> Word16 -> Word8 -> ST s ()
cpuWrite m a v
  | a >= 0x8000 && a < 0xA000 = do
      b <- vramWriteBlocked m
      when (not b) (poke m a v)
  | a >= 0xFE00 && a < 0xFF00 = do
      b <- oamWriteBlocked m
      when (not b) (poke m a v)
  | otherwise = poke m a v
{-# INLINE cpuWrite #-}

-- * Peripherals

-- | The CPU in double speed (KEY1 bit 7, CGB mode).
doubleSpeed :: Machine s -> ST s Bool
doubleSpeed m
  | mCgbMode m = (`testBit` 7) <$> getSt m iKEY1
  | otherwise = pure False

-- | STOP after its second byte: in CGB mode with KEY1's switch armed, the
-- speed switch (Pan Docs, "KEY1"): the other speed, bit 0 cleared, and the
-- CPU stopped for 2050 M-cycles during which DIV doesn't tick. Otherwise
-- (a real stop, waiting for a button) it stays a no-op, as before.
stopInstr :: Machine s -> ST s ()
stopInstr m = do
  k <- getSt m iKEY1
  when (mCgbMode m && testBit k 0) $ do
    setSt m iKEY1 (complement k .&. 0x80)
    setSt m iDIVHOLD 1
    sequence_ (replicate 2050 (tick m))
    setSt m iDIVHOLD 0

-- | VRAM DMA (CGB mode; Pan Docs, "LCD VRAM DMA transfers"). A block is
-- 16 bytes from the source to VRAM (the bank VBK selects), while the CPU
-- waits 8 M-cycles (16 in double speed: the same time), a byte pair per
-- M-cycle (one byte in double speed). General purpose DMA runs all its
-- blocks at once, from the write to HDMA5; HBlank DMA runs one block per
-- HBlank, after the CPU's instruction ('hdmaPending': before the next
-- one), and not while the CPU is halted (the block waits). Sources 0xE000 and up read
-- 0xA000 and up, VRAM as a source reads $FF (Pan Docs: both invalid; our
-- choice). Decided here, not measured: no test ROM pins the timing down.
hdmaBlock :: Machine s -> ST s ()
hdmaBlock m = do
  ds <- doubleSpeed m
  src <- getSt m iHDMASRC
  dst <- getSt m iHDMADST
  let per = if ds then 1 else 2
      byte i = do
        let a = fromIntegral ((src + i) .&. 0xFFFF) :: Word16
        v <- if a >= 0x8000 && a < 0xA000 then pure 0xFF else peek m (if a >= 0xE000 then a - 0x4000 else a)
        vramAt m (0x8000 + fromIntegral ((dst + i) .&. 0x1FFF)) >>= \(vec, j) -> MU.unsafeWrite vec j v
  sequence_ [ mapM_ byte [k .. k + per - 1] >> tick m | k <- [0, per .. 15] ]
  setSt m iHDMASRC ((src + 16) .&. 0xFFF0)
  setSt m iHDMADST ((dst + 16) .&. 0x1FF0)
  n <- getSt m iHDMALEN
  if n == 0 then setSt m iHDMALEN 0x7F >> setSt m iHDMAACT 0 else setSt m iHDMALEN (n - 1)

-- | After an instruction: the HBlank DMA block due, if the CPU isn't
-- halted.
hdmaPending :: Machine s -> ST s ()
hdmaPending m = do
  r <- getSt m iHDMAREQ
  h <- getSt m iHALT
  when (r /= 0 && h == 0) $ do
    setSt m iHDMAREQ 0
    a <- getSt m iHDMAACT
    when (a /= 0) (hdmaBlock m)

-- | A write to HDMA5: general purpose DMA (bit 7 clear), HBlank DMA (bit 7
-- set), or, while an HBlank DMA runs, bit 7 clear stops it (HDMA5 then
-- reads bit 7 set and the blocks left).
hdmaStart :: Machine s -> Int -> ST s ()
hdmaStart m w = do
  a <- getSt m iHDMAACT
  if a /= 0 && not (testBit w 7) then setSt m iHDMAACT 0 else do
    setSt m iHDMALEN (w .&. 0x7F)
    setSt m iHDMAREQ 0
    if testBit w 7 then setSt m iHDMAACT 1 else do
      setSt m iHDMAACT 0
      let go = do
            hdmaBlock m
            n <- getSt m iHDMALEN
            when (n /= 0x7F) go
      go

-- | One M-cycle of everything that isn't the CPU.
tick :: Machine s -> ST s ()
tick m = do
  modSt m iCYC (+ 1)
  -- timer: the reload one M-cycle after an overflow (Pan Docs cycle B)
  rl <- getSt m iRELOAD
  case rl of
    1 -> do
      getSt m iTMA >>= setSt m iTIMA
      modSt m iIF (.|. 0x04)
      setSt m iRELOAD 2
    2 -> setSt m iRELOAD 0
    _ -> pure ()
  c <- getSt m iDIV
  tac <- getSt m iTAC
  hold <- getSt m iDIVHOLD
  let c' = if hold /= 0 then c else (c + 4) .&. 0xFFFF
  setSt m iDIV c'
  when (timerSignal tac c && not (timerSignal tac c')) (incTima m)
  sc <- getSt m iSC
  when (sc == 0x81 && testBit c 8 && not (testBit c' 8)) (serialShift m)
  apuTick m c c'
  rtcTick m
  joyTick m
  dmaTick m
  hk <- readSTRef (mHook m)
  case hk of
    Nothing -> ppuTick m
    Just h -> hkPrePpu h >> ppuTick m >> hkPostPpu h

-- | One M-cycle of OAM DMA.
dmaTick :: Machine s -> ST s ()
dmaTick m = do
  pend <- getSt m iDMAPEND
  when (pend > 0) $ do
    setSt m iDMAPEND (pend - 1)
    when (pend == 1) $ do
      getSt m iDMANEW >>= setSt m iDMASRC
      setSt m iDMAIDX 0
  i <- getSt m iDMAIDX
  if i == 160 then setSt m iDMAIDX (-1)
  else when (i >= 0) $ do
    src <- getSt m iDMASRC
    let a = fromIntegral (src * 0x100 + i) :: Word16
    v <- peek m (if a >= 0xE000 then a - 0x2000 else a)
    ramWrite m (0xFE00 + fromIntegral i) v
    setSt m iDMAIDX (i + 1)

-- | The input of the timer's falling-edge detector: TAC enable AND the
-- selected system counter bit (9, 3, 5, 7 for TAC 0 to 3).
timerSignal :: Int -> Int -> Bool
timerSignal tac c = testBit tac 2 && testBit c (sel (tac .&. 3))
 where
  sel 0 = 9
  sel 1 = 3
  sel 2 = 5
  sel _ = 7
{-# INLINE timerSignal #-}

incTima :: Machine s -> ST s ()
incTima m = do
  t <- getSt m iTIMA
  if t == 0xFF
    then setSt m iTIMA 0 >> setSt m iRELOAD 1
    else setSt m iTIMA (t + 1)

serialShift :: Machine s -> ST s ()
serialShift m = do
  modSt m iSB (\b -> ((b `shiftL` 1) .|. 1) .&. 0xFF)
  n <- (+ 1) <$> getSt m iSBITS
  if n == 8
    then do
      setSt m iSBITS 0
      setSt m iSC 0x01
      modSt m iIF (.|. 0x08)
    else setSt m iSBITS n

-- * Memory map, without time

romByte :: Machine s -> Int -> Word16 -> Word8
romByte m bank a = mRom m `U.unsafeIndex` (bank * 0x4000 + (fromIntegral a .&. 0x3FFF))
{-# INLINE romByte #-}

-- | What a read at this address returns now.
peek :: Machine s -> Word16 -> ST s Word8
peek m a
  | a < 0x4000 = case mMbc m of
      Mbc1 -> do
        md <- getSt m iMODE
        b2 <- getSt m iBANK2
        pure (romByte m (if md == 1 then (b2 `shiftL` 5) `mod` mBanks m else 0) a)
      _ -> pure (romByte m 0 a)
  | a < 0x8000 = case mMbc m of
      Mbc1 -> do
        b1 <- getSt m iROMB
        b2 <- getSt m iBANK2
        pure (romByte m (((b2 `shiftL` 5) .|. b1) `mod` mBanks m) a)
      NoMbc -> pure (romByte m 1 a)
      _ -> do -- MBC2, MBC3, MBC5: one ROM bank register
        b <- getSt m iROMB
        pure (romByte m (b `mod` mBanks m) a)
  | a < 0xA000 = vramAt m a >>= \(v, i) -> MU.unsafeRead v i
  | a < 0xC000 = do
      rtc <- rtcSelected m
      case rtc of
        Just r -> getSt m (iRTCL + r) >>= \v -> pure (fromIntegral (v .&. rtcMask r))
        Nothing -> do
          ix <- cramIx m a
          let nib = if mMbc m == Mbc2 then (.|. 0xF0) else id
          maybe (pure 0xFF) (fmap nib . MU.unsafeRead (mCram m)) ix
  | a < 0xFE00 = wramAt m (if a >= 0xE000 then a - 0x2000 else a) >>= \(v, i) -> MU.unsafeRead v i
  | a < 0xFEA0 = MU.unsafeRead (mRam m) (ramIx a)
  | a < 0xFF00 = pure 0x00 -- DMG, when not blocked (Pan Docs, "FEA0-FEFF range")
  | a < 0xFF80 = ioRead m a
  | a == 0xFFFF = fromIntegral <$> getSt m iIE
  | otherwise = MU.unsafeRead (mRam m) (ramIx a)

-- | Where the CPU's VRAM address is: bank 0 in 'mRam', bank 1 ('iVBK', CGB
-- mode) in 'mVram1'.
vramAt :: Machine s -> Word16 -> ST s (MU.MVector s Word8, Int)
vramAt m a
  | mCgbMode m = do
      b <- getSt m iVBK
      pure (if b .&. 1 == 1 then (mVram1 m, fromIntegral a - 0x8000) else (mRam m, ramIx a))
  | otherwise = pure (mRam m, ramIx a)

-- | Where a WRAM address (0xC000-0xDFFF) is: 0xD000-0xDFFF is bank 'iSVBK'
-- in CGB mode (0 means 1), banks 2 to 7 in 'mWramX'.
wramAt :: Machine s -> Word16 -> ST s (MU.MVector s Word8, Int)
wramAt m a
  | mCgbMode m && a >= 0xD000 = do
      b <- (.&. 7) <$> getSt m iSVBK
      pure (if b <= 1 then (mRam m, ramIx a) else (mWramX m, (b - 2) * 0x1000 + fromIntegral a - 0xD000))
  | otherwise = pure (mRam m, ramIx a)

-- | Cartridge RAM offset of an address in 0xA000-0xBFFF, if it is enabled.
cramIx :: Machine s -> Word16 -> ST s (Maybe Int)
cramIx m a
  | not (mCramOn m) = pure Nothing
  | otherwise = case mMbc m of
      Mbc1 -> do
        en <- getSt m iRAMEN
        md <- getSt m iMODE
        b2 <- getSt m iBANK2
        let bank = if md == 1 then b2 else 0
        pure (if en == 0 then Nothing
              else Just ((bank * 0x2000 + off) `mod` MU.length (mCram m)))
      Mbc5 -> do
        en <- getSt m iRAMEN
        bank <- getSt m iBANK2
        pure (if en == 0 then Nothing
              else Just ((bank * 0x2000 + off) `mod` MU.length (mCram m)))
      Mbc2 -> do -- 512 half-bytes, repeated through 0xA000-0xBFFF
        en <- getSt m iRAMEN
        pure (if en == 0 then Nothing else Just (off .&. 0x1FF))
      Mbc3 -> do -- RAM banks 0 to 3 (the clock's registers: 'rtcSelected')
        en <- getSt m iRAMEN
        bank <- getSt m iBANK2
        pure (if en == 0 || bank > 3 then Nothing
              else Just ((bank * 0x2000 + off) `mod` MU.length (mCram m)))
      _ -> pure (Just (off `mod` MU.length (mCram m)))
 where
  off = fromIntegral a - 0xA000

-- | MBC3: the clock register (0 to 4: S, M, H, DL, DH) that 0xA000-0xBFFF
-- reads and writes now, if one is selected (RAM bank register 0x08 to
-- 0x0C) and enabled. Other values above 3 select nothing (reads 0xFF).
rtcSelected :: Machine s -> ST s (Maybe Int)
rtcSelected m
  | mMbc m /= Mbc3 = pure Nothing
  | otherwise = do
      t <- getSt m iTIMER
      en <- getSt m iRAMEN
      b <- getSt m iBANK2
      pure (if t == 1 && en /= 0 && b >= 8 && b <= 12 then Just (b - 8) else Nothing)

-- | The bits each clock register has (Pan Docs, "MBC3"): seconds and
-- minutes 6, hours 5, day low 8, day high: bit 0 day bit 8, bit 6 halt,
-- bit 7 day carry. The others read 0.
rtcMask :: Int -> Int
rtcMask r = case r of
  0 -> 0x3F
  1 -> 0x3F
  2 -> 0x1F
  3 -> 0xFF
  _ -> 0xC1

-- | One M-cycle of MBC3's clock: a second every 1048576 M-cycles (2^22
-- T-cycles: the machine's own time, which the board keeps equal to real
-- time while it runs), unless halted. A register counts up to its limit
-- (60, 60, 24, 512 days) and carries; a value written above the limit
-- counts on to the top of its bits and wraps to 0 without a carry. Day
-- 511 + 1 sets the day carry, which stays until written.
rtcTick :: Machine s -> ST s ()
rtcTick m = do
  t <- getSt m iTIMER
  dh <- getSt m (iRTC + 4)
  when (t == 1 && not (testBit dh 6)) $ do
    sub <- (+ 1) <$> getSt m iRTCSUB
    if sub < 1048576 then setSt m iRTCSUB sub else do
      setSt m iRTCSUB 0
      rtcSecond m

rtcSecond :: Machine s -> ST s ()
rtcSecond m = do
  cs <- step 0 60 0x3F
  cm <- if cs then step 1 60 0x3F else pure False
  ch <- if cm then step 2 24 0x1F else pure False
  when ch $ do
    dl <- getSt m (iRTC + 3)
    dh <- getSt m (iRTC + 4)
    let d = ((dh .&. 1) `shiftL` 8 .|. dl) + 1
    setSt m (iRTC + 3) (d .&. 0xFF)
    setSt m (iRTC + 4) ((dh .&. 0xC0) .|. ((d `shiftR` 8) .&. 1) .|. (if d == 512 then 0x80 else 0))
 where
  -- the register counts; True if it carried into the next one
  step r lim bits = do
    v <- getSt m (iRTC + r)
    if v == lim - 1 then setSt m (iRTC + r) 0 >> pure True
    else setSt m (iRTC + r) ((v + 1) .&. bits) >> pure False

ioRead :: Machine s -> Word16 -> ST s Word8
ioRead m a = case a of
  0xFF00 -> do
    p <- getSt m iP1
    n <- joyLines m
    pure (fromIntegral (0xC0 .|. p .|. n))
  0xFF01 -> st iSB
  0xFF02 -> (.|. (if mCgbMode m then 0x7C else 0x7E)) <$> st iSC
  0xFF04 -> fromIntegral . (`shiftR` 8) <$> getSt m iDIV
  0xFF05 -> st iTIMA
  0xFF06 -> st iTMA
  0xFF07 -> (.|. 0xF8) <$> st iTAC
  0xFF0F -> (.|. 0xE0) <$> st iIF
  0xFF40 -> st iLCDC
  0xFF41 -> statRead m
  0xFF44 -> lyRead m
  0xFF45 -> st iLYC
  _ | a >= 0xFF10 && a < 0xFF30 -> (.|. apuReadMask a) <$> apuRead m a
    | a >= 0xFF30 && a < 0xFF40 -> apuRead m a -- wave RAM
    | a >= 0xFF42 && a < 0xFF4C -> MU.unsafeRead (mRam m) (ramIx a)
    | mModel m == Cgb -> cgbRead m a
    | otherwise -> pure 0xFF
 where
  st i = fromIntegral <$> getSt m i

-- | The Game Boy Color's registers from 0xFF4C (Pan Docs; Mooneye's
-- @boot_hwio-C@ and @unused_hwio-C@ for DMG compatibility mode, where
-- only VBK's unused bits, BCPS, OCPS and 0xFF72, 0xFF73, 0xFF75 to 0xFF77
-- answer). HDMA1 to HDMA4 read $FF (write-only), HDMA5 the DMA's state; PCM12
-- and PCM34 read 0 (the channels' outputs: step 3d).
cgbRead :: Machine s -> Word16 -> ST s Word8
cgbRead m a = case a of
  0xFF4D | c -> (.|. 0x7E) <$> st iKEY1
  0xFF4F -> (.|. 0xFE) <$> (if c then st iVBK else pure 0)
  0xFF55 | c -> (\act n -> (if act /= 0 then 0 else 0x80) .|. fromIntegral n) <$> getSt m iHDMAACT <*> getSt m iHDMALEN
  0xFF56 | c -> (\v -> (v .&. 0xC1) .|. 0x3E) <$> st iRP
  0xFF68 -> (.|. 0x40) <$> st iBCPS
  0xFF69 | c -> getSt m iBCPS >>= \i -> MU.unsafeRead (mPal m) (i .&. 0x3F)
  0xFF6A -> (.|. 0x40) <$> st iOCPS
  0xFF6B | c -> getSt m iOCPS >>= \i -> MU.unsafeRead (mPal m) (64 + i .&. 0x3F)
  0xFF6C | c -> (.|. 0xFE) <$> st iOPRI
  0xFF70 | c -> (.|. 0xF8) <$> st iSVBK
  0xFF72 -> st iFF72
  0xFF73 -> st iFF73
  0xFF74 | c -> st iFF74
  0xFF75 -> (.|. 0x8F) <$> st iFF75
  0xFF76 -> pure 0x00
  0xFF77 -> pure 0x00
  _ -> pure 0xFF
 where
  c = mCgbMode m
  st i = fromIntegral <$> getSt m i

-- | Writes to the Game Boy Color's registers from 0xFF4C ('cgbRead').
cgbWrite :: Machine s -> Word16 -> Word8 -> ST s ()
cgbWrite m a v = case a of
  0xFF4D | c -> modSt m iKEY1 (\k -> (k .&. 0x80) .|. (w .&. 1))
  0xFF4F | c -> setSt m iVBK (w .&. 1)
  0xFF51 | c -> modSt m iHDMASRC (\s -> (w `shiftL` 8) .|. (s .&. 0xF0))
  0xFF52 | c -> modSt m iHDMASRC (\s -> (s .&. 0xFF00) .|. (w .&. 0xF0))
  0xFF53 | c -> modSt m iHDMADST (\d -> ((w .&. 0x1F) `shiftL` 8) .|. (d .&. 0xF0))
  0xFF54 | c -> modSt m iHDMADST (\d -> (d .&. 0x1F00) .|. (w .&. 0xF0))
  0xFF55 | c -> hdmaStart m w
  0xFF56 | c -> setSt m iRP (w .&. 0xC1)
  0xFF68 -> setSt m iBCPS (w .&. 0xBF)
  0xFF69 | c -> palWrite iBCPS 0
  0xFF6A -> setSt m iOCPS (w .&. 0xBF)
  0xFF6B | c -> palWrite iOCPS 64
  0xFF6C | c -> setSt m iOPRI (w .&. 1)
  0xFF70 | c -> setSt m iSVBK (w .&. 7)
  0xFF72 -> setSt m iFF72 w
  0xFF73 -> setSt m iFF73 w
  0xFF74 | c -> setSt m iFF74 w
  0xFF75 -> setSt m iFF75 (w .&. 0x70)
  _ -> pure ()
 where
  c = mCgbMode m
  w = fromIntegral v
  -- the byte at the index, which moves on if auto-increment (bit 7) is set
  palWrite ix base = do
    i <- getSt m ix
    MU.unsafeWrite (mPal m) (base + i .&. 0x3F) v
    when (testBit i 7) (setSt m ix (0x80 .|. ((i + 1) .&. 0x3F)))

-- | Bits of the sound registers that read as 1 whatever was written
-- (write-only and unused bits; 0xFF15, 0xFF1F and 0xFF27-0xFF2F are
-- unmapped). Checked by Mooneye's @bits/unused_hwio-GS@ and
-- @boot_hwio-dmgABCmgb@; the sound registers themselves are step 2.9.
apuReadMask :: Word16 -> Word8
apuReadMask a = case a of
  0xFF10 -> 0x80
  0xFF11 -> 0x3F
  0xFF12 -> 0x00
  0xFF14 -> 0xBF
  0xFF16 -> 0x3F
  0xFF17 -> 0x00
  0xFF19 -> 0xBF
  0xFF1A -> 0x7F
  0xFF1C -> 0x9F
  0xFF1E -> 0xBF
  0xFF21 -> 0x00
  0xFF22 -> 0x00
  0xFF23 -> 0xBF
  0xFF24 -> 0x00
  0xFF25 -> 0x00
  0xFF26 -> 0x70
  _ -> 0xFF -- NR13, NR18, NR1B, NR1D, NR20 (write-only), unmapped

-- | A write at this address, now.
poke :: Machine s -> Word16 -> Word8 -> ST s ()
poke m a v
  | a < 0x8000 = mbcWrite m a v
  | a < 0xA000 = vramAt m a >>= \(vec, i) -> MU.unsafeWrite vec i v
  | a < 0xC000 = do
      rtc <- rtcSelected m
      case rtc of
        Just r -> do
          setSt m (iRTC + r) (fromIntegral v .&. rtcMask r)
          when (r == 0) (setSt m iRTCSUB 0) -- writing the seconds restarts the second
        Nothing -> cramIx m a >>= maybe (pure ()) (\i -> MU.unsafeWrite (mCram m) i
                     (if mMbc m == Mbc2 then v .&. 0x0F else v))
  | a < 0xFE00 = wramAt m (if a >= 0xE000 then a - 0x2000 else a) >>= \(vec, i) -> MU.unsafeWrite vec i v
  | a < 0xFEA0 = MU.unsafeWrite (mRam m) (ramIx a) v
  | a < 0xFF00 = pure ()
  | a < 0xFF80 = ioWrite m a v
  | a == 0xFFFF = setSt m iIE (fromIntegral v)
  | otherwise = MU.unsafeWrite (mRam m) (ramIx a) v

mbcWrite :: Machine s -> Word16 -> Word8 -> ST s ()
mbcWrite m a v = case mMbc m of
  Mbc1
    | a < 0x2000 -> setSt m iRAMEN (if v .&. 0x0F == 0x0A then 1 else 0)
    | a < 0x4000 -> setSt m iROMB (let b = fromIntegral v .&. 0x1F in if b == 0 then 1 else b)
    | a < 0x6000 -> setSt m iBANK2 (fromIntegral v .&. 3)
    | otherwise -> setSt m iMODE (fromIntegral v .&. 1)
  Mbc5 -- Pan Docs, "MBC5"
    | a < 0x2000 -> setSt m iRAMEN (if v == 0x0A then 1 else 0)
    | a < 0x3000 -> getSt m iROMB >>= \b -> setSt m iROMB ((b .&. 0x100) .|. fromIntegral v)
    | a < 0x4000 -> getSt m iROMB >>= \b -> setSt m iROMB ((b .&. 0xFF) .|. (fromIntegral v .&. 1) `shiftL` 8)
    | a < 0x6000 -> setSt m iBANK2 (fromIntegral v .&. 0x0F)
    | otherwise -> pure ()
  Mbc2 -- Pan Docs, "MBC2": address bit 8 picks the register
    | a < 0x4000, testBit a 8 -> setSt m iROMB (let b = fromIntegral v .&. 0x0F in if b == 0 then 1 else b)
    | a < 0x4000 -> setSt m iRAMEN (if v .&. 0x0F == 0x0A then 1 else 0)
    | otherwise -> pure ()
  Mbc3 -- Pan Docs, "MBC3"
    | a < 0x2000 -> setSt m iRAMEN (if v .&. 0x0F == 0x0A then 1 else 0)
    | a < 0x4000 -> setSt m iROMB (let b = fromIntegral v .&. 0x7F in if b == 0 then 1 else b)
    | a < 0x6000 -> setSt m iBANK2 (fromIntegral v .&. 0x0F)
    | otherwise -> do
        prev <- getSt m iRTCLATCH
        when (prev == 0 && v == 1) $ sequence_
          [ getSt m (iRTC + r) >>= setSt m (iRTCL + r) | r <- [0 .. 4] ]
        setSt m iRTCLATCH (fromIntegral v)
  _ -> pure ()

-- | The P1 input lines P10-P13 (active low) for the selected groups:
-- bit 4 of P1 clear selects the direction keys, bit 5 the buttons.
joyLines :: Machine s -> ST s Int
joyLines m = do
  p <- getSt m iP1
  k <- getSt m iJOY
  let dirs = if testBit p 4 then 0 else k .&. 0x0F
      btns = if testBit p 5 then 0 else (k `shiftR` 4) .&. 0x0F
  pure (complement (dirs .|. btns) .&. 0x0F)

-- | The joypad interrupt: requested when an input line goes low (checked
-- once per M-cycle).
joyTick :: Machine s -> ST s ()
joyTick m = do
  n <- joyLines m
  prev <- getSt m iJOYPREV
  when (prev .&. complement n .&. 0x0F /= 0) (modSt m iIF (.|. 0x10))
  setSt m iJOYPREV n

ioWrite :: Machine s -> Word16 -> Word8 -> ST s ()
ioWrite m a v = case a of
  0xFF00 -> setSt m iP1 (w .&. 0x30)
  0xFF01 -> setSt m iSB w >> hook
  0xFF02 -> do
    setSt m iSC (w .&. (if mCgbMode m then 0x83 else 0x81))
    when (testBit v 7) (setSt m iSBITS 0)
    hook
    when (v .&. 0x81 == 0x81) (setSt m iSERNEW 1)
  0xFF04 -> do
    c <- getSt m iDIV
    tac <- getSt m iTAC
    sc <- getSt m iSC
    setSt m iDIV 0
    apuDivWrite m c
    when (timerSignal tac c) (incTima m)
    when (sc == 0x81 && testBit c 8) (serialShift m)
  0xFF05 -> do
    rl <- getSt m iRELOAD
    case rl of
      2 -> pure ()                                -- cycle B: TMA wins
      1 -> setSt m iTIMA w >> setSt m iRELOAD 0   -- cycle A: the overflow is cancelled
      _ -> setSt m iTIMA w
  0xFF06 -> do
    setSt m iTMA w
    rl <- getSt m iRELOAD
    when (rl == 2) (setSt m iTIMA w)
  0xFF07 -> do
    c <- getSt m iDIV
    tac <- getSt m iTAC
    let tac' = w .&. 7
    setSt m iTAC tac'
    when (timerSignal tac c && not (timerSignal tac' c)) (incTima m)
  0xFF0F -> setSt m iIF (w .&. 0x1F)
  0xFF40 -> lcdcWrite m v >> hooked
  0xFF41 -> setSt m iSTAT (w .&. 0x78) >> updateStat m >> hooked
  0xFF44 -> pure ()
  0xFF45 -> setSt m iLYC w >> updateStat m >> hooked
  0xFF46 -> do
    ramWrite m a v
    setSt m iDMANEW w
    setSt m iDMAPEND 2
  _ | a >= 0xFF10 && a < 0xFF40 -> apuWrite m a v
    | a >= 0xFF4C && mModel m == Cgb -> cgbWrite m a v
    | otherwise -> MU.unsafeWrite (mRam m) (ramIx a) v
 where
  w = fromIntegral v
  hook = modifySTRef' (mSerial m) (serialWrite a v)
  hooked = readSTRef (mHook m) >>= maybe (pure ()) (\h -> hkWrite h a v)
