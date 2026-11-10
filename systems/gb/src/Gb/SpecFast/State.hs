{-# OPTIONS_GHC -fworker-wrapper -fspecialise-aggressively -funfolding-use-threshold=200 -funfolding-creation-threshold=1000 #-}
-- (the fast version is never synthesised: GHC's unboxing back on, which
-- clash-common's -fno-worker-wrapper turns off for Clash's sake)
-- The Game Boy's fast version (side track Q for side track W, the user
-- 2026-11-07: the browser's Game Boy Color too slow on a laptop): begun as
-- a copy of Gb.Spec.State, which stays the reference; changed here for speed.
-- The same machine: gb-specfast runs it beside the reference on every
-- input.
{-|
The mutable state of the Game Boy reference model, shared by the CPU
("Gb.SpecFast.Cpu"), the PPU ("Gb.SpecFast.Ppu") and the memory map and
peripherals ("Gb.SpecFast.Machine"). Scalars live in one unboxed vector
indexed by the @i*@ constants below; memories are unboxed byte vectors.
-}
module Gb.SpecFast.State
  ( Access (..)
  , Model (..)
  , Machine (..)
  , Hook (..)
    -- * Scalar state
  , StIx
  , getSt
  , setSt
  , modSt
  , iPC, iSP, iIME, iEI, iHALT, iLOCK, iCYC, iIF, iIE, iBREAK, iSERNEW, iTRACE
  , iDIV, iTIMA, iTMA, iTAC, iRELOAD, iSB, iSC, iSBITS
  , iROMB, iBANK2, iRAMEN, iMODE, iP1, iHALTBUG
  , iLY, iDOT, iLCDC, iSTAT, iLYC, iSTATLINE, iPPUMODE, iPX, iSTALL, iSCXF
  , iWINLINE, iWINX, iWINSHIFT, iWYTRIG, iNSPR, iSPRNEXT, iLASTTILE, iFRAMES, iLCDON1
  , iDMAIDX, iDMASRC, iDMAPEND, iDMANEW, iLYCFLAG, iJOY, iJOYPREV
  , iTIMER, iRTC, iRTCL, iRTCSUB, iRTCLATCH
  , iVBK, iSVBK, iBCPS, iOCPS, iKEY1, iFF72, iFF73, iFF74, iFF75, iOPRI, iRP
  , iDIVHOLD
  , iHDMASRC, iHDMADST, iHDMALEN, iHDMAACT, iHDMAREQ
  , iPLAG, iPQUIET, iTCKEY, iTCLO, iTCHI, iTCATTR
  , iNST
    -- * Registers (index = the r field of an opcode, F at the (HL) slot 6)
  , rB, rC, rD, rE, rH, rL, rF, rA
  , getR
  , setR
    -- * Memory
  , ramIx
  , ramRead
  , ramWrite
  ) where

import Prelude

import Control.Monad.ST (ST)
import Data.STRef
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import Data.Word

import Gb.Cartridge (Mbc)
import Gb.Hooks (Serial)

-- | The console the spec is (side track I, phase 3): the DMG, or the Game
-- Boy Color, in CGB mode for a cartridge whose header byte 0x143 has bit
-- 7 set and in DMG compatibility mode otherwise ('mCgbMode').
data Model = Dmg | Cgb
  deriving stock (Show, Eq)

-- | One M-cycle on the CPU's bus, as recorded by the trace.
data Access
  = ARead !Word16 !Word8
  | AWrite !Word16 !Word8
  | AIdle
  deriving stock (Show, Eq)

data Machine s = Machine
  { mRegs   :: !(MU.MVector s Word8) -- ^ B C D E H L F A
  , mSt     :: !(MU.MVector s Int)   -- ^ scalar state, indexed by the @i*@ constants
  , mRam    :: !(MU.MVector s Word8) -- ^ 0x8000-0xFFFF, flat (VRAM, WRAM, OAM, I/O backing, HRAM)
  , mCram   :: !(MU.MVector s Word8) -- ^ cartridge RAM (one dummy byte if none)
  , mRom    :: !(U.Vector Word8)
  , mMbc    :: !Mbc
  , mCramOn :: !Bool                 -- ^ the cartridge has RAM
  , mBanks  :: !Int                  -- ^ 16 KiB ROM banks in the image
  , mFb     :: !(MU.MVector s Word8) -- ^ the frame being drawn, 160 × 144 shades
  , mLast   :: !(MU.MVector s Word8) -- ^ the last complete frame
  , mApu    :: !(MU.MVector s Int)   -- ^ the sound unit's state ("Gb.SpecFast.Apu")
  , mSpr    :: !(MU.MVector s Int)   -- ^ this line's objects (copied at OAM scan), 7 fields each, in DMG drawing priority order (X, then OAM)
  , mTrace  :: !(STRef s [Access])   -- ^ newest first, while 'iTRACE' is 1
  , mSerial :: !(STRef s Serial)
  , mHook   :: !(STRef s (Maybe (Hook s)))
  , mModel  :: !Model
  , mCgbMode :: !Bool                -- ^ the GBC running a CGB cartridge (else DMG, or DMG compatibility mode)
  , mVram1  :: !(MU.MVector s Word8) -- ^ VRAM bank 1 (CGB; bank 0 is in 'mRam')
  , mWramX  :: !(MU.MVector s Word8) -- ^ WRAM banks 2 to 7 at 0xD000 (CGB; bank 1 is in 'mRam')
  , mPal    :: !(MU.MVector s Word8) -- ^ CGB palette RAM: 64 bytes of BG palettes, then 64 of OBJ
  , mFbC    :: !(MU.MVector s Word16) -- ^ the frame being drawn in colour (CGB: 15-bit BGR)
  , mLastC  :: !(MU.MVector s Word16) -- ^ the last complete colour frame
  , mObjLine :: !(MU.MVector s Int)  -- ^ the fast version's: this line's object pixel per x, packed ('Gb.SpecFast.Ppu.objLine')
  }

-- | Test instrumentation (layer 2): lets a test run hardware units next to
-- the spec's own, on the same memory and registers. Normal runs have none.
data Hook s = Hook
  { hkPrePpu  :: ST s ()                    -- ^ in each tick, before the PPU's dots
  , hkPostPpu :: ST s ()                    -- ^ right after them (and the STAT update)
  , hkWrite   :: Word16 -> Word8 -> ST s () -- ^ after a CPU write to LCDC, STAT or LYC
  }

type StIx = Int

iPC, iSP, iIME, iEI, iHALT, iLOCK, iCYC, iIF, iIE, iBREAK, iSERNEW, iTRACE :: StIx
iDIV, iTIMA, iTMA, iTAC, iRELOAD, iSB, iSC, iSBITS :: StIx
iROMB, iBANK2, iRAMEN, iMODE, iP1, iHALTBUG :: StIx
iLY, iDOT, iLCDC, iSTAT, iLYC, iSTATLINE, iPPUMODE, iPX, iSTALL, iSCXF :: StIx
iWINLINE, iWINX, iWINSHIFT, iWYTRIG, iNSPR, iSPRNEXT, iLASTTILE, iFRAMES, iLCDON1 :: StIx
iDMAIDX, iDMASRC, iDMAPEND, iDMANEW, iLYCFLAG, iJOY, iJOYPREV :: StIx
iTIMER, iRTC, iRTCL, iRTCSUB, iRTCLATCH, iNST :: StIx
iPC = 0        -- program counter
iSP = 1
iIME = 2       -- interrupt master enable
iEI = 3        -- EI executed: IME goes to 1 after the next instruction
iHALT = 4      -- halted
iLOCK = 5      -- an illegal opcode locked the CPU up
iCYC = 6       -- M-cycles since power-up (hand-off at 0)
iIF = 7        -- IF, low 5 bits
iIE = 8        -- IE, all 8 bits
iBREAK = 9     -- LD B,B executed (count)
iSERNEW = 10   -- a serial byte was sent since the runner last looked
iTRACE = 11    -- record 'Access'es
iDIV = 12      -- 16-bit system counter (T-cycles); DIV is its high byte
iTIMA = 13
iTMA = 14
iTAC = 15      -- low 3 bits
iRELOAD = 16   -- 0: none, 1: TIMA overflowed this M-cycle (A), 2: reloaded this M-cycle (B)
iSB = 17
iSC = 18       -- bits 7 and 0
iSBITS = 19    -- bits shifted in the current transfer
iROMB = 20     -- MBC1 BANK1 (5 bits); MBC2 ROM bank (4); MBC3 (7); MBC5 (9 bits)
iBANK2 = 21    -- MBC1 BANK2 (2 bits); MBC3 RAM bank or RTC register (4); MBC5 RAM bank (4 bits)
iRAMEN = 22    -- cartridge RAM enabled
iMODE = 23     -- MBC1 mode
iP1 = 24       -- P1 select bits 4 and 5
iHALTBUG = 25  -- the next fetch does not advance PC
iLY = 26       -- the line being processed, 0 to 153
iDOT = 27      -- the next dot of the line, 0 to 455
iLCDC = 28
iSTAT = 29     -- the writable bits 3 to 6
iLYC = 30
iSTATLINE = 31 -- the STAT interrupt line, for edge detection
iPPUMODE = 32  -- the mode the PPU is in (0 while the LCD is off)
iPX = 33       -- mode 3: the next pixel to output, 0 to 160
iSTALL = 34    -- mode 3: dots left to stall before the next pixel
iSCXF = 35     -- SCX & 7, latched at the start of mode 3
iWINLINE = 36  -- the window's internal line counter
iWINX = 37     -- the pixel at which the window started on this line, -1 if not
iWINSHIFT = 38 -- window columns skipped at its start (WX < 7)
iWYTRIG = 39   -- the window's Y condition (LY == WY seen this frame)
iNSPR = 40     -- objects selected for this line ('mSpr')
iSPRNEXT = 41  -- mode 3: the next object in 'mSpr' whose penalty is still due
iLASTTILE = 42 -- mode 3: the tile that the last object's penalty considered
iFRAMES = 43   -- frames completed (VBlank entries)
iLCDON1 = 44   -- the first line after the LCD was switched on (no OAM scan)
iDMAIDX = 45   -- OAM DMA: next byte, 0 to 160; -1 when idle
iDMASRC = 46   -- OAM DMA: source page
iDMAPEND = 47  -- OAM DMA: M-cycles until a requested transfer starts (0: none)
iDMANEW = 48   -- OAM DMA: source page of the requested transfer
iLYCFLAG = 49  -- the latched LY = LYC flag (STAT bit 2)
iJOY = 50      -- buttons held: bit 0 Right, 1 Left, 2 Up, 3 Down, 4 A, 5 B, 6 Select, 7 Start
iJOYPREV = 51  -- P1 input lines (low nibble) at the last tick
iTIMER = 52    -- the cartridge has MBC3's real-time clock (types 0x0F, 0x10)
iRTC = 53      -- MBC3 clock, 5 registers from here: S, M, H, DL, DH (live)
iRTCL = 58     -- the 5 latched registers (what reads return)
iRTCSUB = 63   -- M-cycles into the current second, 0 to 1048575
iRTCLATCH = 64 -- the last byte written to 0x6000-0x7FFF (0 then 1 latches)
-- the Game Boy Color's registers (side track I, phase 3; Pan Docs)
iVBK, iSVBK, iBCPS, iOCPS, iKEY1, iFF72, iFF73, iFF74, iFF75, iOPRI, iRP :: StIx
iVBK = 65      -- VRAM bank (bit 0; CGB mode)
iSVBK = 66     -- WRAM bank at 0xD000 (bits 0 to 2, 0 = 1; CGB mode)
iBCPS = 67     -- BG palette index (bits 0 to 5) and auto-increment (bit 7)
iOCPS = 68     -- OBJ palette index and auto-increment
iKEY1 = 69     -- speed switch: bit 7 double speed, bit 0 armed (CGB mode)
iFF72 = 70     -- undocumented registers (Pan Docs, "Undocumented registers")
iFF73 = 71
iFF74 = 72     -- read and written only in CGB mode
iFF75 = 73     -- bits 4 to 6
iOPRI = 74     -- object priority: 0 by OAM position (CGB), 1 by X (DMG)
iRP = 75       -- the infrared port's written bits (0, 6, 7; CGB mode)
iDIVHOLD :: StIx
iDIVHOLD = 76  -- 1 during the speed switch's pause: DIV doesn't tick
-- VRAM DMA (CGB mode; Pan Docs, "LCD VRAM DMA transfers")
iHDMASRC, iHDMADST, iHDMALEN, iHDMAACT, iHDMAREQ :: StIx
iHDMASRC = 77  -- source address (bits 4 to 15)
iHDMADST = 78  -- destination in VRAM (bits 4 to 12)
iHDMALEN = 79  -- blocks left - 1 (bits 0 to 6; 0x7F once done)
iHDMAACT = 80  -- 1 while an HBlank DMA runs
iHDMAREQ = 81  -- 1 when an HBlank started and its block hasn't run
-- The lazy PPU (the fast version's): the M-cycles of PPU work owed, run
-- when something looks ('Gb.SpecFast.Machine.syncPpu').
iPLAG, iPQUIET :: StIx
iPLAG = 82     -- M-cycles the PPU is behind
iPQUIET = 83   -- how many it may fall behind before it must run (-1: run at the next)
-- The fast version's background tile cache ('Gb.SpecFast.Ppu.tileColour'):
-- the last tile row read, valid until the PPU's next catch-up.
iTCKEY, iTCLO, iTCHI, iTCATTR :: StIx
iTCKEY = 84    -- its map address, row and tile data select (-1: none)
iTCLO = 85
iTCHI = 86
iTCATTR = 87
iNST = 88

rB, rC, rD, rE, rH, rL, rF, rA :: Int
rB = 0
rC = 1
rD = 2
rE = 3
rH = 4
rL = 5
rF = 6
rA = 7

getSt :: Machine s -> StIx -> ST s Int
getSt m = MU.unsafeRead (mSt m)
{-# INLINE getSt #-}

setSt :: Machine s -> StIx -> Int -> ST s ()
setSt m = MU.unsafeWrite (mSt m)
{-# INLINE setSt #-}

modSt :: Machine s -> StIx -> (Int -> Int) -> ST s ()
modSt m i f = getSt m i >>= setSt m i . f
{-# INLINE modSt #-}

getR :: Machine s -> Int -> ST s Word8
getR m = MU.unsafeRead (mRegs m)
{-# INLINE getR #-}

setR :: Machine s -> Int -> Word8 -> ST s ()
setR m = MU.unsafeWrite (mRegs m)
{-# INLINE setR #-}

ramIx :: Word16 -> Int
ramIx a = fromIntegral a - 0x8000
{-# INLINE ramIx #-}

-- | 'mRam' at a 0x8000-0xFFFF address, without any access rules.
ramRead :: Machine s -> Word16 -> ST s Word8
ramRead m a = MU.unsafeRead (mRam m) (ramIx a)
{-# INLINE ramRead #-}

ramWrite :: Machine s -> Word16 -> Word8 -> ST s ()
ramWrite m a = MU.unsafeWrite (mRam m) (ramIx a)
{-# INLINE ramWrite #-}
