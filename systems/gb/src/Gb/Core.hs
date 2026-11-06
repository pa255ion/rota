{-|
The whole Game Boy in Clash (PLAN.md step 2.6): the CPU ("Gb.Cpu"), the
PPU ("Gb.Ppu"), timer, serial port, joypad, OAM DMA, interrupt registers,
memory map and cartridge banking (no MBC, MBC1, MBC2, MBC3 with its
clock, MBC5). It reproduces the
reference model ("Gb.Spec.Machine") M-cycle for M-cycle.

__Structure.__ 'coreStep' is a pure function run once per clock. The
memories are outside it ('MemReq' out, the read data in next clock), so
the same function runs in the Haskell lockstep harness (memories in
vectors, fast) and in hardware ('gbCore': block RAMs).

__Timing.__ An M-cycle is a sequence of clocks ('Seq'), in the spec's
order: the tick (timer reload and increment, serial shift, joypad, OAM DMA
byte), four PPU dots (3 clocks each) and the STAT update, then the CPU's
bus access (and a PPU command after writes to LCDC, STAT or LYC), then
the CPU's 'cpuCycle'. That is 18 to 20 clocks; an M-cycle starts when four
dots of time have elapsed ('kiDot' adds one), so with the 25.175 MHz pixel
clock and a 4.194304 MHz dot rate (6 clocks a dot, 24 an M-cycle) the core
keeps real time.

__Memories__ ('Space'): cartridge ROM (up to 8 MiB, outside: the caller's
ROM port), cartridge RAM (up to 128 KiB; MBC2's 512 half-bytes),
WRAM (8 KiB), HRAM (128 B) and VRAM (8 KiB, as two banks of even and odd
bytes, so the PPU reads a tile row as one 16-bit pair; 'koVramPair').
OAM, the I/O registers, the sound registers and MBC3's clock are
flip-flops. The clock starts from 'kiRtcInit' (the PS's copy from the save
file) and comes out on 'koRtc' (side track I).
-}
module Gb.Core
  ( -- * Configuration
    CoreModel (..)
  , MbcKind (..)
  , CartCfg (..)
  , cartFromHeader
    -- * Memory ports
  , Space (..)
  , MemReq (..)
    -- * The core
  , CoreIn (..)
  , CoreOut (..)
  , McEvent (..)
  , CoreDbg (..)
  , coreDbg
  , Seq (..)
  , CoreState (..)
  , coreInit
  , coreStep
  , gbcDefaultPal
  , palColour
  , gbCore
  , gbCoreWith
  , gbCoreHooked
  , StateTap (..)
  , restoreable
  , overlayBase
  , overlayEndAddr
  , stateStatusAddr
  , restAt
  , CramPort
  , RomPort
  , romBlockRam
    -- * Pieces (for compile-time experiments)
  , readPath
  , cpuWrite
  , tickStart
  , oamView
  , ioRead
  , Rd (..)
  ) where

import Clash.Prelude
import Data.Maybe (fromMaybe, isJust, isNothing)

import Gb.Apu (ApuState, apuDivWrite, apuInit, apuMix, apuOutputs, apuRead, apuTick, apuWrite, regsInit)
import Gb.Cpu
import Gb.Ppu
import Gb.Types (Instr (Stop))
import Platform.SaveState (StateOp (..), stateOpWrite)
import qualified Prelude as P

-- | The console (side track I, phase 3): the DMG, or the Game Boy Color
-- (which runs a cartridge in CGB mode if its header byte 0x143 has bit 7
-- set, else in DMG compatibility mode). A value fixed by the top
-- ("Gb.Top" passes 'CoreDmg'), so synthesis drops what the other model
-- needs.
data CoreModel = CoreDmg | CoreCgb
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFDataX

data MbcKind = MbcNone | MbcOne | MbcTwo | MbcThree | MbcFive
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFDataX

-- | What the core knows about the cartridge, from its header (read at
-- reset, 'cartFromHeader').
data CartCfg = CartCfg
  { ccMbc     :: MbcKind
  , ccRomMask :: BitVector 9  -- ^ ROM banks (16 KiB) - 1, a power of two minus one
  , ccRamMask :: BitVector 17 -- ^ RAM bytes - 1 (unused without RAM)
  , ccHasRam  :: Bool
  , ccTimer   :: Bool         -- ^ MBC3's real-time clock (types 0x0F, 0x10)
  , ccCgbHw   :: Bool         -- ^ the console is a Game Boy Color ('CoreCgb')
  , ccCgb     :: Bool         -- ^ ... running the cartridge in CGB mode
  , ccHc      :: Bool         -- ^ header checksum non-zero (initial H and C flags)
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFDataX

-- | 'SpRomHint': a ROM read the core will make later in this M-cycle (its
-- address can't change before then: only the CPU's own writes move the
-- banks), sent early so the ROM port's wait is over when the read comes
-- (side track I, phase 3 step 2). No byte comes back for it; memories
-- other than the ROM port ignore it.
--
-- 'SpRomUse': the CPU's ROM read of the M-cycle whose first dot sent the
-- hint (the same address by construction), so the port's adapter needs no
-- address compare in the core's path (make bit: WNS -0.35 ns with one).
data Space = SpRom | SpCram | SpWram | SpHram | SpVram | SpRomHint | SpRomUse
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFDataX

-- | A memory access; reads return their byte in 'kiRd' the next clock.
data MemReq = MemReq
  { mqSpace :: Space
  , mqAddr  :: Unsigned 23
  , mqWrite :: Maybe (BitVector 8)
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFDataX

data CoreIn = CoreIn
  { kiDot   :: Bool         -- ^ one dot of time elapsed
  , kiRd    :: BitVector 8  -- ^ the byte of the previous clock's read
  , kiVram  :: BitVector 16 -- ^ the pair of the previous clock's 'koVramPair'
  , kiKeys  :: BitVector 8  -- ^ held: 0 Right, 1 Left, 2 Up, 3 Down, 4 A, 5 B, 6 Select, 7 Start
  , kiRtcInit :: Vec 10 (BitVector 8) -- ^ MBC3's clock at start: S M H DL DH, then the latched five (taken at the first clock)
  , kiVram1 :: BitVector 16 -- ^ VRAM bank 1's pair at the same address as 'kiVram' (the Game Boy Color)
  , kiPalInit :: Vec 24 (BitVector 8) -- ^ the Game Boy Color's palettes for a DMG cartridge: BG 0, OBJ 0, OBJ 1 (taken at the hand-off)
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFDataX

-- | The end of an M-cycle, for lockstep tests and debugging.
data McEvent = McEvent
  { meOp       :: BusOp
  , meAddr     :: BitVector 16
  , meData     :: BitVector 8 -- ^ written, or read
  , meStepDone :: Bool
  , meRegs     :: CpuRegs
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFDataX

-- | Internal state worth comparing with the spec.
data CoreDbg = CoreDbg
  { dbDiv :: BitVector 16, dbTima :: BitVector 8, dbIf :: BitVector 5, dbIe :: BitVector 8
  , dbLy :: Unsigned 8, dbDot :: Unsigned 9, dbMode :: Unsigned 2, dbDma :: Maybe (Unsigned 8)
  , dbApu :: Vec 4 (Unsigned 4, Bool) -- ^ the sound channels' levels and DACs
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFDataX

data CoreOut = CoreOut
  { koMem      :: Maybe MemReq
  , koVramPair :: Maybe (Unsigned 12)
  , koPixel    :: Maybe (Index 23040, BitVector 2)
  , koFrame    :: Bool
  , koSerial   :: Maybe (BitVector 8) -- ^ a byte sent on the link port
  , koEvent    :: Maybe McEvent
  , koDbg      :: CoreDbg
  , koCartOk   :: Bool                -- ^ the cartridge type is supported
  , koAudio    :: (Signed 12, Signed 12) -- ^ the mixed sound, left and right (±480)
  , koRtc      :: Vec 10 (BitVector 8)    -- ^ MBC3's clock: S M H DL DH, then the latched five
  , koPixelC   :: Maybe (Index 23040, BitVector 15) -- ^ the Game Boy Color's pixel: 15-bit BGR (red in bits 0 to 4)
  , koPixelP   :: Maybe (Index 23040, Index 128)    -- ^ ... as its first palette RAM byte ('koPixelC' = 'palColour' of it)
  , koPalette  :: Vec 128 (BitVector 8)             -- ^ palette RAM (for 'koPixelP': it changes only in the CPU's accesses)
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFDataX

-- | Where a read gets its byte.
data Rd = RdImm (BitVector 8) | RdMem MemReq
  | RdNib MemReq -- ^ MBC2's RAM: the upper half-byte reads as 1s
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFDataX

data Seq
  = QBoot
  | QHdr (Index 32)
  | QIdle
  | QDmaRd
  | QHdmaRd                -- ^ VRAM DMA (CGB): the source byte's read
  | QHdmaWr                -- ^ ... and its write into VRAM
  | QDot (Index 4) (Index 3)
  | QStat
  | QAcc
  | QAccRd
  | QPpuCmd PpuCmd
  | QCpu
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFDataX

data CoreState = CoreState
  { ksSeq    :: Seq
  , ksCfg    :: CartCfg
  , ksCartOk :: Bool                 -- ^ the cartridge type is supported
  , ksHdr    :: Vec 3 (BitVector 8)  -- ^ header bytes 0x147-0x149 while reading
  , ksCredit :: Unsigned 4
  , ksCpu    :: Cpu
  , ksReq    :: BusReq
  , ksRd     :: BitVector 8          -- ^ the CPU's read data this M-cycle
  , ksImm    :: Maybe (BitVector 8)  -- ^ a read answered without memory
  , ksPpu    :: (PpuState, Micro)
  , ksRegs   :: PpuRegs
  , ksOam    :: Vec 160 (BitVector 8)
  , ksApu    :: Vec 48 (BitVector 8) -- ^ 0xFF10-0xFF3F as written ("Gb.Apu")
  , ksApuSt  :: ApuState
  , ksDiv    :: BitVector 16
  , ksTima, ksTma :: BitVector 8
  , ksTac    :: BitVector 3
  , ksReload :: Index 3
  , ksSb     :: BitVector 8
  , ksSc     :: BitVector 8          -- ^ bits 7 and 0
  , ksSbits  :: Index 8
  , ksIf     :: BitVector 5
  , ksIe     :: BitVector 8
  , ksP1     :: BitVector 8          -- ^ select bits 4 and 5
  , ksJoyPrev :: BitVector 4
  , ksDmaReg :: BitVector 8
  , ksDmaIdx :: Maybe (Unsigned 8)   -- ^ 0 to 160 (160: ends at the next tick)
  , ksDmaSrc, ksDmaNew :: BitVector 8
  , ksDmaPend :: Index 3
  , ksRomb   :: BitVector 9          -- ^ MBC1 BANK1 / MBC5 ROM bank
  , ksBank2  :: BitVector 4          -- ^ MBC1 BANK2 / MBC5 RAM bank
  , ksRamEn  :: Bool
  , ksMode   :: Bool                 -- ^ MBC1 mode
  , ksNib    :: Bool                 -- ^ the read under way is MBC2's RAM ('RdNib')
  , ksRtc    :: Vec 5 (BitVector 8)  -- ^ MBC3's clock: S M H DL DH (live)
  , ksRtcL   :: Vec 5 (BitVector 8)  -- ^ ... latched (what reads see)
  , ksRtcSub :: Unsigned 20          -- ^ M-cycles into the second
  , ksRtcPrev :: BitVector 8         -- ^ the last byte written to 0x6000-0x7FFF
  -- the Game Boy Color (side track I, phase 3; Gb.Spec.Machine.cgbRead)
  , ksHdrX   :: Vec 4 (BitVector 8)  -- ^ header bytes 0x143, 0x14B, 0x144, 0x145 while reading
  , ksTitle  :: BitVector 8          -- ^ the sum of the title's 16 bytes while reading
  , ksVbk    :: Bool                 -- ^ VRAM bank 1 for the CPU
  , ksSvbk   :: BitVector 3          -- ^ WRAM bank at 0xD000 (0 = 1)
  , ksKey1   :: BitVector 8          -- ^ bit 7 double speed, bit 0 armed
  , ksBcps, ksOcps :: BitVector 8    -- ^ palette indexes (bits 0 to 5) and auto-increment (bit 7)
  , ksOpri   :: Bool                 -- ^ object priority by X (DMG) rather than OAM order
  , ksRp     :: BitVector 8          -- ^ the infrared port's written bits
  , ksFf72, ksFf73, ksFf74, ksFf75 :: BitVector 8
  , ksPal    :: Vec 128 (BitVector 8) -- ^ palette RAM: 64 bytes BG, then 64 OBJ
  , ksPause  :: Unsigned 12          -- ^ M-cycles of the speed switch's pause left (DIV held, no CPU)
  , ksHdmaSrc :: BitVector 16        -- ^ VRAM DMA: source (bits 4 to 15)
  , ksHdmaDst :: BitVector 13        -- ^ ... destination in VRAM (bits 4 to 12)
  , ksHdmaLen :: BitVector 7         -- ^ ... blocks left - 1 (0x7F once done)
  , ksHdmaAct :: Bool                -- ^ an HBlank DMA runs
  , ksHdmaReq :: Bool                -- ^ an HBlank began and its block hasn't run
  , ksHdmaRun :: Bool                -- ^ blocks are being copied (the CPU waits)
  , ksHdmaB   :: Index 16            -- ^ the next byte of the block
  , ksStallMc :: Bool                -- ^ this M-cycle has no CPU access (pause or DMA)
  , ksCycOdd :: Bool                 -- ^ the M-cycle count since the hand-off is odd (the APU in double speed)
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFDataX

-- | The hand-off state (Gb.Spec.Machine.newMachine); the CPU's flags are
-- set at the first clock from the cartridge configuration.
coreInit :: CoreState
coreInit = CoreState
  { ksSeq = QBoot, ksCfg = CartCfg { ccMbc = MbcNone, ccRomMask = 1, ccRamMask = 0, ccHasRam = False, ccTimer = False, ccHc = False
                                    , ccCgbHw = False, ccCgb = False }
  , ksCartOk = False, ksHdr = repeat 0
  , ksCredit = 0, ksCpu = fst (cpuInit False), ksReq = snd (cpuInit False)
  , ksRd = 0, ksImm = Nothing, ksPpu = (ppuInit, MIdle)
  , ksRegs = PpuRegs { prLcdc = 0x91, prStat = 0, prScy = 0, prScx = 0, prLyc = 0
                     , prBgp = 0xFC, prObp0 = 0xFF, prObp1 = 0xFF, prWy = 0, prWx = 0 }
  , ksOam = repeat 0
  , ksApu = regsInit, ksApuSt = apuInit
  , ksDiv = 0xABC8, ksTima = 0, ksTma = 0, ksTac = 0, ksReload = 0
  , ksSb = 0, ksSc = 0, ksSbits = 0, ksIf = 0x01, ksIe = 0, ksP1 = 0, ksJoyPrev = 0xF
  , ksDmaReg = 0xFF, ksDmaIdx = Nothing, ksDmaSrc = 0, ksDmaNew = 0, ksDmaPend = 0
  , ksRomb = 1, ksBank2 = 0, ksRamEn = False, ksMode = False
  , ksNib = False, ksRtc = repeat 0, ksRtcL = repeat 0, ksRtcSub = 0, ksRtcPrev = 0
  , ksHdrX = repeat 0, ksTitle = 0, ksVbk = False, ksSvbk = 0, ksKey1 = 0, ksBcps = 0, ksOcps = 0
  , ksOpri = False, ksRp = 0, ksFf72 = 0, ksFf73 = 0, ksFf74 = 0, ksFf75 = 0, ksPal = repeat 0
  , ksPause = 0, ksHdmaSrc = 0, ksHdmaDst = 0, ksHdmaLen = 0x7F, ksHdmaAct = False, ksHdmaReq = False
  , ksHdmaRun = False, ksHdmaB = 0, ksStallMc = False, ksCycOdd = False }

-- | The cartridge configuration from header bytes 0x147 (type), 0x148
-- (ROM size), 0x149 (RAM size) and 0x14D (header checksum); 'False' if the
-- type isn't supported (it then runs as a plain ROM). MBC2's RAM is its
-- own 512 half-bytes, whatever 0x149 says.
cartFromHeader :: BitVector 8 -> BitVector 8 -> BitVector 8 -> BitVector 8 -> (CartCfg, Bool)
cartFromHeader t romc ramc hsum =
  ( CartCfg { ccMbc = mbc, ccRomMask = romMask, ccRamMask = ramMask', ccHasRam = hasRam
            , ccTimer = t == 0x0F || t == 0x10, ccHc = hsum /= 0, ccCgbHw = False, ccCgb = False }
  , ok )
 where
  (mbc, ramT, ok) = case t of
    0x00 -> (MbcNone, False, True)
    0x01 -> (MbcOne, False, True)
    0x02 -> (MbcOne, True, True)
    0x03 -> (MbcOne, True, True)
    0x08 -> (MbcNone, True, True)
    0x09 -> (MbcNone, True, True)
    0x05 -> (MbcTwo, True, True)
    0x06 -> (MbcTwo, True, True)
    0x0F -> (MbcThree, False, True)
    0x10 -> (MbcThree, True, True)
    0x11 -> (MbcThree, False, True)
    0x12 -> (MbcThree, True, True)
    0x13 -> (MbcThree, True, True)
    0x19 -> (MbcFive, False, True)
    0x1A -> (MbcFive, True, True)
    0x1B -> (MbcFive, True, True)
    0x1C -> (MbcFive, False, True)
    0x1D -> (MbcFive, True, True)
    0x1E -> (MbcFive, True, True)
    _ -> (MbcNone, False, False)
  romMask = if romc <= 8 then shiftL 2 (fromEnum romc) - 1 else 1
  ramMask = case ramc of
    2 -> 0x1FFF
    3 -> 0x7FFF
    4 -> 0x1FFFF
    5 -> 0xFFFF
    _ -> 0
  ramMask' = if mbc == MbcTwo then 0x1FF else ramMask
  hasRam = ramT && ramMask' /= 0

-- | Bits of the sound registers that read as 1 (Gb.Spec.Machine.apuReadMask).
apuMask :: Index 48 -> BitVector 8
apuMask i = case i of
  0x00 -> 0x80
  0x01 -> 0x3F
  0x02 -> 0x00
  0x04 -> 0xBF
  0x06 -> 0x3F
  0x07 -> 0x00
  0x09 -> 0xBF
  0x0A -> 0x7F
  0x0C -> 0x9F
  0x0E -> 0xBF
  0x11 -> 0x00
  0x12 -> 0x00
  0x13 -> 0xBF
  0x14 -> 0x00
  0x15 -> 0x00
  0x16 -> 0x70
  _ | i >= 0x20 -> 0x00 -- wave RAM
    | otherwise -> 0xFF

-- * Peripherals

timerSignal :: BitVector 3 -> BitVector 16 -> Bool
timerSignal tac c = testBit tac 2 && testBit c sel
 where
  sel = case slice d1 d0 tac of
    0 -> 9
    1 -> 3
    2 -> 5
    _ -> 7

incTima :: CoreState -> CoreState
incTima s
  | ksTima s == 0xFF = s { ksTima = 0, ksReload = 1 }
  | otherwise = s { ksTima = ksTima s + 1 }

serialShift :: CoreState -> CoreState
serialShift s
  | ksSbits s == 7 = s' { ksSbits = 0, ksSc = 0x01, ksIf = ksIf s .|. 0x08 }
  | otherwise = s' { ksSbits = ksSbits s + 1 }
 where
  s' = s { ksSb = shiftL (ksSb s) 1 .|. 1 }

joyLines :: BitVector 8 -> CoreState -> BitVector 4
joyLines keys s = complement (dirs .|. btns)
 where
  dirs = if testBit (ksP1 s) 4 then 0 else slice d3 d0 keys
  btns = if testBit (ksP1 s) 5 then 0 else slice d7 d4 keys

-- | The tick's first part: timer, serial, joypad, OAM DMA bookkeeping
-- (the byte copy itself needs a read: 'dmaSource').
tickStart :: BitVector 8 -> CoreState -> CoreState
tickStart = tickStartWith False

-- | 'tickStart', with DIV held (the speed switch's pause: DIV and what
-- it drives don't move).
tickStartWith :: Bool -> BitVector 8 -> CoreState -> CoreState
tickStartWith hold keys s0 = s5
 where
  s1 = case ksReload s0 of
    1 -> s0 { ksTima = ksTma s0, ksIf = ksIf s0 .|. 0x04, ksReload = 2 }
    2 -> s0 { ksReload = 0 }
    _ -> s0
  c = ksDiv s1
  c' = if hold then c else c + 4
  s2 = let t = s1 { ksDiv = c' } in
       if timerSignal (ksTac t) c && not (timerSignal (ksTac t) c') then incTima t else t
  s3' = if ksSc s2 == 0x81 && testBit c 8 && not (testBit c' 8) then serialShift s2 else s2
  -- the M-cycle count's parity after this one (Gb.Spec.Machine.tick
  -- counts first), and double speed (the config is the model's constant)
  odd' = not (ksCycOdd s3')
  ds = ccCgb (ksCfg s3') && testBit (ksKey1 s3') 7
  s3a = let (rs, a) = apuTick ds odd' c c' (ksApu s3', ksApuSt s3') in s3' { ksApu = rs, ksApuSt = a, ksCycOdd = odd' }
  s3 = rtcTick s3a
  n = joyLines keys s3
  s4 = s3 { ksJoyPrev = n, ksIf = ksIf s3 .|. (if ksJoyPrev s3 .&. complement n /= 0 then 0x10 else 0) }
  -- OAM DMA: a requested transfer starts, a finished one ends
  s4' = case ksDmaPend s4 of
    0 -> s4
    1 -> s4 { ksDmaPend = 0, ksDmaSrc = ksDmaNew s4, ksDmaIdx = Just 0 }
    p -> s4 { ksDmaPend = p - 1 }
  s5 = case ksDmaIdx s4' of
    Just 160 -> s4' { ksDmaIdx = Nothing }
    _ -> s4'

-- * Memory map

romAddr :: CartCfg -> CoreState -> BitVector 16 -> Unsigned 23
romAddr cfg s a = unpack (bank ++# slice d13 d0 a)
 where
  m = ccRomMask cfg
  bank :: BitVector 9
  bank
    | a < 0x4000 = case ccMbc cfg of
        MbcOne | ksMode s -> shiftL (zeroExtend (slice d1 d0 (ksBank2 s))) 5 .&. m
        _ -> 0
    | otherwise = case ccMbc cfg of
        MbcOne -> (shiftL (zeroExtend (slice d1 d0 (ksBank2 s))) 5 .|. ksRomb s) .&. m
        MbcNone -> 1
        _ -> ksRomb s .&. m -- MBC2, MBC3, MBC5

-- | Cartridge RAM byte, if enabled.
cramAddr :: CartCfg -> CoreState -> BitVector 16 -> Maybe (Unsigned 23)
cramAddr cfg s a
  | not (ccHasRam cfg) = Nothing
  | otherwise = case ccMbc cfg of
      MbcNone -> Just (idx 0)
      MbcOne | ksRamEn s -> Just (idx (if ksMode s then zeroExtend (slice d1 d0 (ksBank2 s)) else 0))
      MbcTwo | ksRamEn s -> Just (idx 0) -- the mask: 0x1FF
      MbcThree | ksRamEn s, ksBank2 s < 4 -> Just (idx (ksBank2 s))
      MbcFive | ksRamEn s -> Just (idx (ksBank2 s))
      _ -> Nothing
 where
  off = slice d12 d0 a
  idx :: BitVector 4 -> Unsigned 23
  idx b = unpack (zeroExtend ((zeroExtend b `shiftL` 13 .|. zeroExtend off :: BitVector 17) .&. ccRamMask cfg))

-- | MBC3: the clock register that 0xA000-0xBFFF reads and writes now (0 to
-- 4: S M H DL DH), if one is selected (0x08 to 0x0C) and enabled.
rtcSel :: CartCfg -> CoreState -> Maybe (Index 5)
rtcSel cfg s
  | ccMbc cfg == MbcThree, ccTimer cfg, ksRamEn s, ksBank2 s >= 8, ksBank2 s <= 12 =
      Just (unpack (resize (ksBank2 s - 8)))
  | otherwise = Nothing

-- | The bits each clock register has (Gb.Spec.Machine.rtcMask).
rtcMask :: Index 5 -> BitVector 8
rtcMask r = case r of
  0 -> 0x3F
  1 -> 0x3F
  2 -> 0x1F
  3 -> 0xFF
  _ -> 0xC1

-- | One M-cycle of MBC3's clock (Gb.Spec.Machine.rtcTick): a second every
-- 2^20 M-cycles unless halted; a register carries at its limit, and a
-- value above it wraps at its bits without a carry.
rtcTick :: CoreState -> CoreState
rtcTick s
  | not (ccTimer (ksCfg s)) || testBit dh 6 = s
  | ksRtcSub s /= maxBound = s { ksRtcSub = ksRtcSub s + 1 }
  | otherwise = s { ksRtcSub = 0, ksRtc = rtcSecond (ksRtc s) }
 where
  dh = ksRtc s !! (4 :: Index 5)

rtcSecond :: Vec 5 (BitVector 8) -> Vec 5 (BitVector 8)
rtcSecond r = sec :> mn :> hr :> dl :> dh' :> Nil
 where
  step v lim bits = if v == lim - 1 then (0, True) else ((v + 1) .&. bits, False)
  (sec, cs) = step (r !! (0 :: Index 5)) 60 0x3F
  (mn, cm) = if cs then step (r !! (1 :: Index 5)) 60 0x3F else (r !! (1 :: Index 5), False)
  (hr, ch) = if cm then step (r !! (2 :: Index 5)) 24 0x1F else (r !! (2 :: Index 5), False)
  dh = r !! (4 :: Index 5)
  day = (slice d0 d0 dh ++# (r !! (3 :: Index 5))) :: BitVector 9
  day' = day + 1
  (dl, dh')
    | ch = (slice d7 d0 day', (dh .&. 0xC0) .|. zeroExtend (slice d8 d8 day') .|. (if day == maxBound then 0x80 else 0))
    | otherwise = (r !! (3 :: Index 5), dh)

oamView :: Vec 160 (BitVector 8) -> Vec 40 Oam
oamView o = map (\e -> Oam (e !! (0 :: Index 4)) (e !! (1 :: Index 4)) (e !! (2 :: Index 4)) (e !! (3 :: Index 4)))
                (unconcatI o :: Vec 40 (Vec 4 (BitVector 8)))

ppuState :: CoreState -> PpuState
ppuState = fst . ksPpu

-- | The CPU's VRAM byte: bank 1 (bit 13) if VBK says so, in CGB mode.
vramIx :: CartCfg -> CoreState -> BitVector 16 -> Unsigned 23
vramIx cfg s a = unpack ((if ccCgb cfg && ksVbk s then 0x2000 else 0) .|. zeroExtend (slice d12 d0 a))

-- | A WRAM byte (0xC000-0xDFFF, and its echo): 0xD000-0xDFFF is bank SVBK
-- in CGB mode (0 = 1), 4 KiB each.
wramIx :: CartCfg -> CoreState -> BitVector 16 -> Unsigned 23
wramIx cfg s a
  | ccCgb cfg && testBit a 12 =
      let b = if ksSvbk s == 0 then 1 else ksSvbk s
      in unpack (zeroExtend (b ++# slice d11 d0 a))
  | otherwise = unpack (zeroExtend (slice d12 d0 a))

-- | A read without the CPU's VRAM/OAM restrictions (spec: peek).
peekPath :: CartCfg -> CoreState -> BitVector 16 -> Rd
peekPath cfg s a
  | a < 0x8000 = RdMem (MemReq SpRom (romAddr cfg s a) Nothing)
  | a < 0xA000 = RdMem (MemReq SpVram (vramIx cfg s a) Nothing)
  | a < 0xC000 = case (rtcSel cfg s, cramAddr cfg s a) of
      (Just r, _) -> RdImm (ksRtcL s !! r .&. rtcMask r)
      (_, Just i) | ccMbc cfg == MbcTwo -> RdNib (MemReq SpCram i Nothing)
                  | otherwise -> RdMem (MemReq SpCram i Nothing)
      _ -> RdImm 0xFF
  | a < 0xFE00 = RdMem (MemReq SpWram (wramIx cfg s a) Nothing)
  | a < 0xFEA0 = RdImm (ksOam s !! (unpack (slice d7 d0 a) :: Unsigned 8))
  | a < 0xFF00 = RdImm 0x00
  | a < 0xFF80 = RdImm (ioRead s a)
  | a == 0xFFFF = RdImm (ksIe s)
  | otherwise = RdMem (MemReq SpHram (zeroExtend (unpack (slice d6 d0 a))) Nothing)

ioRead :: CoreState -> BitVector 16 -> BitVector 8
ioRead s a = case slice d7 d0 a of
  0x00 -> 0xC0 .|. (ksP1 s .&. 0x30) .|. zeroExtend (ksJoyPrev s) -- the lines as last ticked
  0x01 -> ksSb s
  0x02 -> ksSc s .|. (if ccCgb (ksCfg s) then 0x7C else 0x7E)
  0x04 -> slice d15 d8 (ksDiv s)
  0x05 -> ksTima s
  0x06 -> ksTma s
  0x07 -> zeroExtend (ksTac s) .|. 0xF8
  0x0F -> zeroExtend (ksIf s) .|. 0xE0
  0x40 -> prLcdc r
  0x41 -> statView r (ppuState s)
  0x42 -> prScy r
  0x43 -> prScx r
  0x44 -> pack (lyView (ppuState s))
  0x45 -> prLyc r
  0x46 -> ksDmaReg s
  0x47 -> prBgp r
  0x48 -> prObp0 r
  0x49 -> prObp1 r
  0x4A -> prWy r
  0x4B -> prWx r
  lo | lo >= 0x10 && lo < 0x40 -> let i = unpack (resize (lo - 0x10)) :: Index 48 in apuRead (ccCgbHw (ksCfg s)) (ksApu s) (ksApuSt s) i .|. apuMask i
     | lo >= 0x4C && ccCgbHw (ksCfg s) -> cgbRead s lo
     | otherwise -> 0xFF
 where
  r = ksRegs s

-- | The Game Boy Color's registers from 0xFF4C (Gb.Spec.Machine.cgbRead,
-- the same rules: DMG compatibility mode answers only VBK's unused bits,
-- BCPS, OCPS and 0xFF72, 0xFF73, 0xFF75 to 0xFF77).
cgbRead :: CoreState -> BitVector 8 -> BitVector 8
cgbRead s lo = case lo of
  0x4D | c -> ksKey1 s .|. 0x7E
  0x4F -> 0xFE .|. (if c && ksVbk s then 1 else 0)
  0x55 | c -> (if ksHdmaAct s then 0 else 0x80) .|. zeroExtend (ksHdmaLen s)
  0x56 | c -> (ksRp s .&. 0xC1) .|. 0x3E
  0x68 -> ksBcps s .|. 0x40
  0x69 | c -> ksPal s !! (unpack (resize (slice d5 d0 (ksBcps s))) :: Index 128)
  0x6A -> ksOcps s .|. 0x40
  0x6B | c -> ksPal s !! (64 + unpack (resize (slice d5 d0 (ksOcps s))) :: Index 128)
  0x6C | c -> 0xFE .|. (if ksOpri s then 1 else 0)
  0x70 | c -> 0xF8 .|. zeroExtend (ksSvbk s)
  0x72 -> ksFf72 s
  0x73 -> ksFf73 s
  0x74 | c -> ksFf74 s
  0x75 -> ksFf75 s .|. 0x8F
  0x76 -> 0x00
  0x77 -> 0x00
  _ -> 0xFF
 where
  c = ccCgb (ksCfg s)

-- | Writes to the Game Boy Color's registers from 0xFF4C ('cgbRead').
cgbWrite :: CoreState -> BitVector 8 -> BitVector 8 -> CoreState
cgbWrite s lo v = case lo of
  0x4D | c -> s { ksKey1 = (ksKey1 s .&. 0x80) .|. (v .&. 1) }
  0x4F | c -> s { ksVbk = testBit v 0 }
  0x51 | c -> s { ksHdmaSrc = v ++# slice d7 d4 (ksHdmaSrc s) ++# 0 }
  0x52 | c -> s { ksHdmaSrc = slice d15 d8 (ksHdmaSrc s) ++# slice d7 d4 v ++# 0 }
  0x53 | c -> s { ksHdmaDst = slice d4 d0 v ++# slice d7 d4 (ksHdmaDst s) ++# 0 }
  0x54 | c -> s { ksHdmaDst = slice d12 d8 (ksHdmaDst s) ++# slice d7 d4 v ++# 0 }
  -- HDMA5 (Gb.Spec.Machine.hdmaStart): bit 7 clear stops a running HBlank
  -- DMA, else starts general purpose DMA (the CPU waits from the next
  -- M-cycle); bit 7 set starts HBlank DMA
  0x55 | c -> if ksHdmaAct s && not (testBit v 7) then s { ksHdmaAct = False }
              else s { ksHdmaLen = slice d6 d0 v, ksHdmaReq = False, ksHdmaAct = testBit v 7
                     , ksHdmaRun = not (testBit v 7), ksHdmaB = 0 }
  0x56 | c -> s { ksRp = v .&. 0xC1 }
  0x68 -> s { ksBcps = v .&. 0xBF }
  0x69 | c -> let (i, n) = palNext (ksBcps s) in s { ksPal = replace i v (ksPal s), ksBcps = n }
  0x6A -> s { ksOcps = v .&. 0xBF }
  0x6B | c -> let (i, n) = palNext (ksOcps s) in s { ksPal = replace (64 + i) v (ksPal s), ksOcps = n }
  0x6C | c -> s { ksOpri = testBit v 0 }
  0x70 | c -> s { ksSvbk = slice d2 d0 v }
  0x72 -> s { ksFf72 = v }
  0x73 -> s { ksFf73 = v }
  0x74 | c -> s { ksFf74 = v }
  0x75 -> s { ksFf75 = v .&. 0x70 }
  _ -> s
 where
  c = ccCgb (ksCfg s)
  -- the index written, and the specification after it (auto-increment)
  palNext :: BitVector 8 -> (Index 128, BitVector 8)
  palNext ps = ( unpack (resize (slice d5 d0 ps))
               , if testBit ps 7 then 0x80 .|. ((ps + 1) .&. 0x3F) else ps )

-- | The CPU's read (spec: cpuRead).
cpuReadPath :: CartCfg -> CoreState -> BitVector 16 -> Rd
cpuReadPath cfg s a
  | a >= 0x8000 && a < 0xA000 && bkVramRd b = RdImm 0xFF
  | a >= 0xFE00 && a < 0xFF00 && bkOamRd b = RdImm 0xFF
  | otherwise = peekPath cfg s a
 where
  b = blocked (ksDmaIdx s /= Nothing) (ppuState s)

-- | The CPU's write (spec: cpuWrite / poke / ioWrite): the new state, a
-- memory write, a byte sent on the link port, and a PPU command to run.
cpuWrite :: CartCfg -> CoreState -> BitVector 16 -> BitVector 8 -> (CoreState, Maybe MemReq, Maybe (BitVector 8), Maybe PpuCmd)
cpuWrite cfg s a v
  | a < 0x8000 = (mbcWrite cfg s a v, Nothing, Nothing, Nothing)
  | a < 0xA000 = (s, if bkVramWr b then Nothing else Just (MemReq SpVram (vramIx cfg s a) (Just v)), Nothing, Nothing)
  | a < 0xC000 = case rtcSel cfg s of
      Just r -> ( s { ksRtc = replace r (v .&. rtcMask r) (ksRtc s)
                    , ksRtcSub = if r == 0 then 0 else ksRtcSub s } -- the seconds restart the second
                , Nothing, Nothing, Nothing )
      Nothing -> (s, (\i -> MemReq SpCram i (Just (if ccMbc cfg == MbcTwo then v .&. 0x0F else v))) <$> cramAddr cfg s a, Nothing, Nothing)
  | a < 0xFE00 = (s, Just (MemReq SpWram (wramIx cfg s a) (Just v)), Nothing, Nothing)
  | a < 0xFEA0 = (if bkOamWr b then s else s { ksOam = replace (unpack (slice d7 d0 a) :: Unsigned 8) v (ksOam s) }, Nothing, Nothing, Nothing)
  | a < 0xFF00 = (s, Nothing, Nothing, Nothing)
  | a < 0xFF80 = ioWrite s a v
  | a == 0xFFFF = (s { ksIe = v }, Nothing, Nothing, Nothing)
  | otherwise = (s, Just (MemReq SpHram (zeroExtend (unpack (slice d6 d0 a))) (Just v)), Nothing, Nothing)
 where
  b = blocked (ksDmaIdx s /= Nothing) (ppuState s)

mbcWrite :: CartCfg -> CoreState -> BitVector 16 -> BitVector 8 -> CoreState
mbcWrite cfg s a v = case ccMbc cfg of
  MbcOne
    | a < 0x2000 -> s { ksRamEn = slice d3 d0 v == 0xA }
    | a < 0x4000 -> s { ksRomb = let b = zeroExtend (slice d4 d0 v) in if b == 0 then 1 else b }
    | a < 0x6000 -> s { ksBank2 = zeroExtend (slice d1 d0 v) }
    | otherwise -> s { ksMode = testBit v 0 }
  MbcTwo -- address bit 8 picks the register
    | a < 0x4000, testBit a 8 -> s { ksRomb = let b = zeroExtend (slice d3 d0 v) in if b == 0 then 1 else b }
    | a < 0x4000 -> s { ksRamEn = slice d3 d0 v == 0xA }
    | otherwise -> s
  MbcThree
    | a < 0x2000 -> s { ksRamEn = slice d3 d0 v == 0xA }
    | a < 0x4000 -> s { ksRomb = let b = zeroExtend (slice d6 d0 v) in if b == 0 then 1 else b }
    | a < 0x6000 -> s { ksBank2 = slice d3 d0 v }
    | otherwise -> s { ksRtcPrev = v
                     , ksRtcL = if ksRtcPrev s == 0 && v == 1 then ksRtc s else ksRtcL s }
  MbcFive
    | a < 0x2000 -> s { ksRamEn = v == 0x0A }
    | a < 0x3000 -> s { ksRomb = slice d8 d8 (ksRomb s) ++# v }
    | a < 0x4000 -> s { ksRomb = slice d0 d0 v ++# slice d7 d0 (ksRomb s) }
    | a < 0x6000 -> s { ksBank2 = slice d3 d0 v }
    | otherwise -> s
  MbcNone -> s

ioWrite :: CoreState -> BitVector 16 -> BitVector 8 -> (CoreState, Maybe MemReq, Maybe (BitVector 8), Maybe PpuCmd)
ioWrite s a v = case slice d7 d0 a of
  0x00 -> st s { ksP1 = v .&. 0x30 }
  0x01 -> st s { ksSb = v }
  0x02 -> ( s { ksSc = v .&. (if ccCgb (ksCfg s) then 0x83 else 0x81), ksSbits = if testBit v 7 then 0 else ksSbits s }
          , Nothing, if v .&. 0x81 == 0x81 then Just (ksSb s) else Nothing, Nothing )
  0x04 ->
    let c = ksDiv s
        (ar, as) = apuDivWrite c (ksApu s, ksApuSt s)
        t0 = s { ksDiv = 0, ksApu = ar, ksApuSt = as }
        t1 = if timerSignal (ksTac s) c then incTima t0 else t0
        t2 = if ksSc s == 0x81 && testBit c 8 then serialShift t1 else t1
    in st t2
  0x05 -> st (case ksReload s of
    2 -> s
    1 -> s { ksTima = v, ksReload = 0 }
    _ -> s { ksTima = v })
  0x06 -> st s { ksTma = v, ksTima = if ksReload s == 2 then v else ksTima s }
  0x07 ->
    let tac' = slice d2 d0 v
        t = s { ksTac = tac' }
    in st (if timerSignal (ksTac s) (ksDiv s) && not (timerSignal tac' (ksDiv s)) then incTima t else t)
  0x0F -> st s { ksIf = slice d4 d0 v }
  0x40 -> (s { ksRegs = r { prLcdc = v } }, Nothing, Nothing, Just CmdLcdc)
  0x41 -> (s { ksRegs = r { prStat = v .&. 0x78 } }, Nothing, Nothing, Just CmdStat)
  0x42 -> st s { ksRegs = r { prScy = v } }
  0x43 -> st s { ksRegs = r { prScx = v } }
  0x44 -> st s
  0x45 -> (s { ksRegs = r { prLyc = v } }, Nothing, Nothing, Just CmdStat)
  0x46 -> st s { ksDmaReg = v, ksDmaNew = v, ksDmaPend = 2 }
  0x47 -> st s { ksRegs = r { prBgp = v } }
  0x48 -> st s { ksRegs = r { prObp0 = v } }
  0x49 -> st s { ksRegs = r { prObp1 = v } }
  0x4A -> st s { ksRegs = r { prWy = v } }
  0x4B -> st s { ksRegs = r { prWx = v } }
  lo | lo >= 0x10 && lo < 0x40 ->
         let (rs, ap) = apuWrite (ccCgbHw (ksCfg s)) (unpack (resize (lo - 0x10))) v (ksApu s, ksApuSt s)
         in st s { ksApu = rs, ksApuSt = ap }
     | lo >= 0x4C && ccCgbHw (ksCfg s) -> st (cgbWrite s lo v)
     | otherwise -> st s
 where
  r = ksRegs s
  st t = (t, Nothing, Nothing, Nothing)

-- * The clock step

noOut :: CoreState -> CoreOut
noOut s = CoreOut Nothing Nothing Nothing False Nothing Nothing (coreDbg s) (ksCartOk s)
                  (apuMix (ksApu s) (apuOutputs (ksApu s) (ksApuSt s))) (ksRtc s ++ ksRtcL s) Nothing Nothing (ksPal s)

-- | A colour from palette RAM: the two bytes from @i@ (little-endian).
palColour :: Vec 128 (BitVector 8) -> Index 128 -> BitVector 15
palColour pal i = slice d6 d0 (pal !! (i + 1)) ++# (pal !! i)

coreDbg :: CoreState -> CoreDbg
coreDbg s = CoreDbg (ksDiv s) (ksTima s) (ksIf s) (ksIe s) (psLy p) (psDot p) (psMode p) (ksDmaIdx s)
                    (apuOutputs (ksApu s) (ksApuSt s))
 where
  p = ppuState s

-- | Run the PPU one clock with this command; IF gets its requests.
ppuClock :: CoreIn -> Maybe PpuCmd -> CoreState -> (CoreState, PpuOut)
ppuClock inp cmd s = (s { ksPpu = st', ksIf = ksIf s .|. irq }, o)
 where
  pin = PpuIn cmd (ksRegs s) (oamView (ksOam s)) (ksDmaIdx s /= Nothing) (kiVram inp) (ccCgbHw (ksCfg s))
              (kiVram1 inp) (ccCgb (ksCfg s)) (ksOpri s)
  (st', o) = ppuStep (ksPpu s) pin
  irq = (if poStatIrq o then 0x02 else 0) .|. (if poVblank o then 0x01 else 0)

-- | The last 'QHdr' step: 3 on the DMG, 23 on the Game Boy Color.
lastHdr :: CoreModel -> Index 32
lastHdr CoreDmg = 3
lastHdr CoreCgb = 23

-- | The header byte the core reads at 'QHdr' step k (k = 0 is read in
-- 'QBoot'): on the DMG 0x147, 0x148, 0x149, then 0x14D at step 3, which
-- then ends; on the Game Boy Color also 0x143, 0x14B, 0x144, 0x145 and the
-- title, 0x14D at step 23.
hdrAddr :: CoreModel -> Index 32 -> Unsigned 23
hdrAddr model k = case model of
  CoreDmg -> case k of
    1 -> 0x148
    2 -> 0x149
    _ -> 0x14D
  CoreCgb
    | k == 1 -> 0x148
    | k == 2 -> 0x149
    | k == 3 -> 0x143
    | k == 4 -> 0x14B
    | k == 5 -> 0x144
    | k == 6 -> 0x145
    | k < 23 -> 0x134 + resize (unpack (pack (k - 7)) :: Unsigned 5)
    | otherwise -> 0x14D

-- | The Game Boy Color at the hand-off (Gb.Spec.Machine.newMachineModel,
-- the same values): P1, SC, DMA, the system counter, the palette indexes,
-- object priority and palette RAM.
cgbHandOff :: Bool -> Vec 24 (BitVector 8) -> CoreState -> CoreState
cgbHandOff cgbMode pal s = s
  { ksP1 = 0x30, ksSc = if cgbMode then 0x01 else 0x00, ksDmaReg = 0x00, ksDiv = 0x2674
  , ksBcps = if cgbMode then 0x80 else 0x88, ksOcps = if cgbMode then 0x81 else 0x90
  , ksOpri = not cgbMode
  , ksPal = if cgbMode then palCgbMode else palCompat }
 where
  -- constants (Template Haskell: a computed Vec left constant compares in
  -- the Verilog, which Verilator's lint rejects)
  -- CGB mode: the BG palettes white, OBJ palette RAM zero (as the spec)
  palCgbMode = $(listToVecTH (P.concat (P.replicate 32 [0xFF, 0x7F]) <> P.replicate 64 0 :: [BitVector 8]))
  -- compatibility mode: the PS's palettes (or 'gbcDefaultPal') in BG 0,
  -- OBJ 0 and OBJ 1, the rest zero (as the spec)
  palCompat = take d8 pal ++ repeat 0 ++ drop d8 pal ++ (repeat 0 :: Vec 48 (BitVector 8))

-- | The Game Boy Color's palettes for a DMG cartridge until the PS writes
-- others (Gb.Loader's palette window): our default, the boot demo's Game
-- Boy shades (#F4ECD8 #F7A41D #22A699 #0E0B14 as 0x6BBE, 0x129E, 0x4E84,
-- 0x0822; 'Gb.Spec.Machine.cgbDefaultPalette', platform/ps/gbc_palette.zig)
-- in BG 0, OBJ 0 and OBJ 1, little-endian.
gbcDefaultPal :: Vec 24 (BitVector 8)
gbcDefaultPal = $(listToVecTH (P.concat (P.replicate 3 [0xBE, 0x6B, 0x9E, 0x12, 0x84, 0x4E, 0x22, 0x08]) :: [BitVector 8]))

-- | Ask for a read: the memory request now, or the byte for next clock.
issue :: Rd -> CoreState -> (CoreState, Maybe MemReq)
issue (RdImm v) s = (s { ksImm = Just v, ksNib = False }, Nothing)
issue (RdMem q) s = (s { ksImm = Nothing, ksNib = False }, Just q)
issue (RdNib q) s = (s { ksImm = Nothing, ksNib = True }, Just q)

readResult :: CoreIn -> CoreState -> BitVector 8
readResult inp s = maybe (kiRd inp .|. (if ksNib s then 0xF0 else 0)) id (ksImm s)

coreStep :: CoreModel -> CoreState -> CoreIn -> (CoreState, CoreOut)
coreStep model s0 inp = case ksSeq s of
  QBoot -> let t = sP { ksSeq = QHdr 0, ksRtc = take d5 (kiRtcInit inp), ksRtcL = drop d5 (kiRtcInit inp) }
           in (t, (noOut t) { koMem = hdrRead 0x147 })
  -- the header, a byte a clock: 0x147, 0x148, 0x149 (and on the Game Boy
  -- Color 0x143, the licensee 0x14B, 0x144, 0x145, the 16 title bytes),
  -- then 0x14D ('hdrAddr')
  QHdr k
    | k /= lastHdr model ->
        let b = kiRd inp
            t = sP { ksSeq = QHdr (k + 1)
                   , ksHdr = if k < 3 then replace k b (ksHdr sP) else ksHdr sP
                   , ksHdrX = if k >= 3 && k < 7 then replace (resize (k - 3) :: Index 4) b (ksHdrX sP) else ksHdrX sP
                   , ksTitle = if k >= 7 then ksTitle sP + b else ksTitle sP }
        in (t, (noOut t) { koMem = hdrRead (hdrAddr model (k + 1)) })
    | otherwise ->
        let h = ksHdr sP
            (c0, okc) = cartFromHeader (h !! (0 :: Index 3)) (h !! (1 :: Index 3)) (h !! (2 :: Index 3)) (kiRd inp)
            cgbHw = model == CoreCgb
            hx = ksHdrX sP
            cgbMode = cgbHw && testBit (hx !! (0 :: Index 4)) 7
            c' = c0 { ccCgbHw = cgbHw, ccCgb = cgbMode }
            -- DMG compatibility mode: B the title's sum for a Nintendo
            -- licensee, else 0, and HL after it (Gb.Spec.Machine)
            nintendo = hx !! (1 :: Index 4) == 0x01
              || (hx !! (1 :: Index 4) == 0x33 && hx !! (2 :: Index 4) == 0x30 && hx !! (3 :: Index 4) == 0x31)
            cb = if nintendo then ksTitle sP else 0
            (ch, cl) = if cb == 0x43 || cb == 0x58 then (0x99, 0x1A) else (0x00, 0x7C)
            regs | not cgbHw = Nothing
                 | cgbMode = Just (0x00 :> 0x00 :> 0xFF :> 0x56 :> 0x00 :> 0x0D :> 0x80 :> 0x11 :> Nil)
                 | otherwise = Just (cb :> 0x00 :> 0x00 :> 0x08 :> ch :> cl :> 0x80 :> 0x11 :> Nil)
            (c, r) = maybe (cpuInit (ccHc c')) cpuInitRegs regs
            t0 = sP { ksCfg = c', ksCartOk = okc, ksCpu = c, ksReq = r, ksSeq = QIdle }
            t = if cgbHw then cgbHandOff cgbMode (kiPalInit inp) t0 else t0
        in (t, noOut t)
  QIdle
    | ksCredit s >= dpm -> case dmaAddr ticked of
        Just _ -> let t = rdState { ksSeq = QDmaRd } in (t, (noOut t) { koMem = rdReq })
        Nothing -> let t = ticked { ksSeq = afterIdle ticked } in (t, noOut t)
    | otherwise -> (sP, noOut sP)
  QDmaRd ->
    let i = maybe 0 id (ksDmaIdx sP)
        t = sP { ksOam = replace i readByte (ksOam sP), ksDmaIdx = Just (i + 1), ksSeq = afterIdle sP }
    in (t, noOut t)
  -- VRAM DMA: a source byte (sources from 0xE000 read 0xA000 and up, VRAM
  -- as a source reads $FF: Gb.Spec.Machine.hdmaBlock) ...
  QHdmaRd ->
    let a0 = ksHdmaSrc sP + zeroExtend (pack (ksHdmaB sP))
        a = if a0 >= 0xE000 then a0 - 0x4000 else a0
        rd' = if a >= 0x8000 && a < 0xA000 then RdImm 0xFF else peekPath cfg sP a
        (t0, q) = issue rd' sP
        t = t0 { ksSeq = QHdmaWr }
    in (t, (noOut t) { koMem = q })
  -- ... into VRAM (the bank VBK selects); a block ends after 16 bytes
  QHdmaWr ->
    let b = ksHdmaB sP
        d = ksHdmaDst sP + zeroExtend (pack b)
        bank = if ccCgb cfg && ksVbk sP then 0x2000 else 0 :: BitVector 14
        q = MemReq SpVram (unpack (zeroExtend (bank .|. zeroExtend d))) (Just readByte)
        blockDone = b == maxBound
        n = ksHdmaLen sP
        t0 | blockDone = sP { ksHdmaB = 0, ksHdmaSrc = ksHdmaSrc sP + 16, ksHdmaDst = ksHdmaDst sP + 16
                            , ksHdmaLen = if n == 0 then 0x7F else n - 1
                            , ksHdmaAct = ksHdmaAct sP && n /= 0
                              -- general purpose DMA goes on with its next block
                            , ksHdmaRun = not (ksHdmaAct sP) && n /= 0 }
           | otherwise = sP { ksHdmaB = b + 1 }
        -- a byte pair per M-cycle, one in double speed
        more = not ds && not blockDone && even (fromEnum b)
        t = t0 { ksSeq = if more then QHdmaRd else QDot 0 0 }
    in (t, (noOut t) { koMem = Just q })
  QDot k p ->
    let nxt | p < 2 = QDot k (p + 1)
            | k < (if ds then 1 else 3) = QDot (k + 1) 0
            | otherwise = QStat
        t = sP { ksSeq = nxt }
        -- the CPU's ROM read of this M-cycle, announced at its first dot
        hint = case (k, p, brOp req) of
          (0, 0, OpRead) | brAddr req < 0x8000, not (ksStallMc sP) -> Just (MemReq SpRomHint (romAddr cfg sP (brAddr req)) Nothing)
          _ -> Nothing
        -- the Game Boy Color's colour: two bytes of palette RAM
        palIx = fromIntegral (poPalByte po) :: Index 128
        colour = palColour (ksPal sP) palIx
    in (t, (noOut t) { koVramPair = if poBusy po then Just (poVramAddr po) else Nothing
                     , koPixel = poPixel po, koFrame = poFrame po, koMem = hint
                     , koPixelC = if ccCgbHw cfg then (\(i, _) -> (i, colour)) <$> poPixel po else Nothing
                     , koPixelP = if ccCgbHw cfg then (\(i, _) -> (i, palIx)) <$> poPixel po else Nothing })
  QStat
    | ksStallMc sP ->
        -- an M-cycle without the CPU ends; after the speed switch's pause,
        -- an HBlank DMA block that came due in it (Gb.Spec.Machine.hdmaPending)
        let p = ksPause sP
            t0 = sP { ksPause = if p > 0 then p - 1 else 0, ksSeq = QIdle }
            t = if p == 1 then hblankStart t0 else t0
        in (t, noOut t)
    | otherwise -> let t = sP { ksSeq = QAcc } in (t, noOut t)
  QAcc -> case brOp req of
    OpRead -> let t = rdState { ksSeq = QAccRd }
                  -- the hinted ROM read ('SpRomUse')
                  use = (\q -> if mqSpace q == SpRom then q { mqSpace = SpRomUse } else q) <$> rdReq
              in (t, (noOut t) { koMem = use })
    OpWrite ->
      let (t, q, ser, cmd) = cpuWrite cfg sP (brAddr req) (brData req)
          t' = t { ksSeq = maybe QCpu QPpuCmd cmd, ksRd = 0 }
      in (t', (noOut t') { koMem = q, koSerial = ser })
    OpIdle -> let t = sP { ksSeq = QCpu, ksRd = 0 } in (t, noOut t)
  QAccRd -> let t = sP { ksRd = readByte, ksSeq = QCpu } in (t, noOut t)
  QPpuCmd _ -> let t = sP { ksSeq = QCpu } in (t, noOut t)
  QCpu ->
    let cpu0 = ksCpu sP
        (cpu', o) = cpuCycle cpu0 (CpuIn (ksRd sP) (ksIe sP) (ksIf sP))
        iflag = maybe (ksIf sP) (\n -> ksIf sP .&. complement (shiftL 1 (fromEnum n))) (coAck o)
        t0 = sP { ksCpu = cpu', ksReq = coReq o, ksIf = iflag, ksSeq = QIdle }
        -- STOP's end (Gb.Spec.Machine.stopInstr): in CGB mode with the
        -- switch armed, the other speed and 2050 M-cycles' pause
        k1 = ksKey1 sP
        switch = ccCgb cfg && coStepDone o && cInstr cpu0 == Stop && cMc cpu0 == 1 && testBit k1 0
        t1 = if switch then t0 { ksKey1 = complement k1 .&. 0x80, ksPause = 2050 } else t0
        -- an instruction's end: the HBlank DMA block due (not while halted,
        -- nor before the pause: then at its end)
        t = if coStepDone o && not switch then hblankStart t1 else t1
        ev = McEvent (brOp req) (brAddr req) (if brOp req == OpWrite then brData req else ksRd sP)
                     (coStepDone o) (cpuRegs cpu')
    in (t, (noOut t) { koEvent = Just ev })
 where
  -- the model is a constant: on the DMG every Game Boy Color part of the
  -- state reads False here, so synthesis drops the CGB logic from gb.bit
  -- (a register alone didn't let Vivado see it: 47k LUTs, WNS -4.6 ns)
  hw = model == CoreCgb
  cfg = (ksCfg s0) { ccCgbHw = hw, ccCgb = hw && ccCgb (ksCfg s0) }
  -- double speed (CGB mode, KEY1 bit 7): an M-cycle is 2 dots
  ds = ccCgb cfg && testBit (ksKey1 s0) 7
  dpm = if ds then 2 else 4
  -- after the tick's first part: VRAM DMA's bytes (not during the pause)
  -- in an M-cycle without the CPU, then the dots
  afterIdle t = if ksHdmaRun t && ksPause t == 0 then QHdmaRd else QDot 0 0
  -- an HBlank DMA's block starts, if one is due and the CPU isn't halted
  hblankStart t
    | ksHdmaReq t && not (cHalt (ksCpu t)) = t { ksHdmaReq = False, ksHdmaRun = ksHdmaAct t, ksHdmaB = 0 }
    | otherwise = t
  hdrRead a = Just (MemReq SpRom a Nothing)
  s = s0 { ksCfg = cfg, ksCredit = if kiDot inp && ksCredit s0 < 15 then ksCredit s0 + 1 else ksCredit s0 }
  req = ksReq s
  -- the PPU runs once every clock: a command in its phases, else nothing
  ppuCmd = case ksSeq s of
    QDot _ 0 -> Just CmdDot
    QStat -> Just CmdStat
    QPpuCmd c -> Just c
    _ -> Nothing
  (sPp, po) = ppuClock inp ppuCmd s
  -- an HBlank begins (mode 3 to 0, the LCD on): an HBlank DMA's block is due
  mode0 = psMode (fst (ksPpu s)) == 3 && psMode (fst (ksPpu sPp)) == 0 && psOn (fst (ksPpu sPp))
  sP = if mode0 && ksHdmaAct sPp then sPp { ksHdmaReq = True } else sPp
  -- the tick's first part (QIdle); the M-cycle has no CPU access in the
  -- speed switch's pause or while VRAM DMA copies
  ticked = let t = tickStartWith (ksPause sP > 0) (kiKeys inp) sP { ksCredit = ksCredit sP - dpm }
           in t { ksStallMc = ksPause t > 0 || ksHdmaRun t }
  -- one read path: the OAM DMA's source in QIdle, the CPU's read in QAcc
  (rdFrom, rdAddrNow, cpuRules) = case ksSeq s of
    QIdle -> (ticked, maybe 0 id (dmaAddr ticked), False)
    _ -> (sP, brAddr req, True)
  (rdState, rdReq) = issue (readPath cfg rdFrom cpuRules rdAddrNow) rdFrom
  readByte = readResult inp sP

-- | The address the OAM DMA reads in this tick, if it copies a byte.
dmaAddr :: CoreState -> Maybe (BitVector 16)
dmaAddr s = case ksDmaIdx s of
  Just i | i < 160 -> let a = ksDmaSrc s ++# pack i in Just (if a >= 0xE000 then a - 0x2000 else a)
  _ -> Nothing

-- | A read: with the CPU's VRAM/OAM restrictions or without (DMA).
readPath :: CartCfg -> CoreState -> Bool -> BitVector 16 -> Rd
readPath cfg s cpuRules a
  | cpuRules = cpuReadPath cfg s a
  | otherwise = peekPath cfg s a
{-# OPAQUE readPath #-}

-- | A ROM port (side track I, phase 2): requests in (an address for one
-- clock, never while an earlier one is unanswered, the answer's own clock
-- included), answers out (the byte, once, any later clock). The block RAM
-- ROM answers the next clock ('romBlockRam'); DDR ("Platform.Mem.Ddr",
-- "Gb.Top") the next clock on a cache hit, later on a miss.
type RomPort dom = Signal dom (Maybe (Unsigned 23)) -> Signal dom (Maybe (BitVector 8))

-- | A block RAM of 256 KiB as a ROM port (the caller's RAM: read address,
-- loader write -> data), always answering the next clock.
romBlockRam
  :: HiddenClockResetEnable dom
  => (Signal dom (Unsigned 18) -> Signal dom (Maybe (Unsigned 18, BitVector 8)) -> Signal dom (BitVector 8))
  -> Signal dom (Maybe (Unsigned 18, BitVector 8))
  -> RomPort dom
romBlockRam ram w req = (\v q -> if v then Just q else Nothing) <$> register False (isJust <$> req)
                                                               <*> ram (maybe 0 resize <$> req) w

-- | The core with its memories: block RAMs for cartridge RAM, WRAM, HRAM
-- and VRAM (two banks), and the cartridge ROM given by the caller (read
-- address, loader write -> data; 256 KiB): a 'blockRamFile' in the top,
-- written by the loader. Without the PS's cartridge RAM port and the
-- clock's start values ('gbCoreWith').
gbCore
  :: forall dom. HiddenClockResetEnable dom
  => (Signal dom (Unsigned 18) -> Signal dom (Maybe (Unsigned 18, BitVector 8)) -> Signal dom (BitVector 8))
  -> Signal dom Bool                                  -- ^ run (False: hold in the hand-off state, e.g. while loading)
  -> Signal dom Bool                                  -- ^ one dot elapsed
  -> Signal dom (BitVector 8)                         -- ^ keys
  -> Signal dom (Maybe (Unsigned 18, BitVector 8))    -- ^ loader: ROM write
  -> Signal dom CoreOut
gbCore romRam run dot keys romW = out
 where
  (out, _, reqs) = gbCoreWith CoreDmg answers run dot keys (pure (repeat 0)) (pure gbcDefaultPal) (pure Nothing)
  answers = romBlockRam romRam romW reqs

-- | The PS's access to cartridge RAM (side track I, battery saves): a
-- byte to read (its value comes out the next clock) or to write.
type CramPort = Maybe (Unsigned 17, Maybe (BitVector 8))

-- | Whose answer the ROM port owes: the core's (it waits for it), a
-- hint's (its tag), or one from before a hold (dropped).
data RomKind = KCore | KHint Bit | KStale
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFDataX

-- | A request of the core to the ROM port: a read, a hint, or the read
-- the hint was for ('SpRomUse').
data RomReq = RRead | RHint | RUse
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFDataX

-- | What waits in the ROM port's queue (its address is in 'rwCoreAddr' or
-- 'rwHintAddr').
data RomQueued = QCore | QHint Bit
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFDataX

-- | The ROM port's bookkeeping in 'gbCoreWith'. The addresses go straight
-- into 'rwCoreAddr' and 'rwHintAddr' (enabled by the request's kind only),
-- and a hint's answer is matched by a one-bit tag, not by its address:
-- the core's address is the end of its longest path (make bit: an address
-- compare and the queue's muxes after it cost 0.5 ns).
data RomWait = RomWait
  { rwOut      :: Maybe RomKind        -- ^ a request at the port, unanswered: whose
  , rwQueue    :: Maybe RomQueued      -- ^ one waiting to go out (the port was busy)
  , rwCoreAddr :: Unsigned 23          -- ^ the core's last read's address
  , rwHintAddr :: Unsigned 23          -- ^ the last hint's address
  , rwWait     :: Bool                 -- ^ the core waits for its byte
  , rwWaitHint :: Bool                 -- ^ ... which is the hint's, still to come
  , rwHint     :: Bool                 -- ^ a hint is held for the core's next read
  , rwHintTag  :: Bit                  -- ^ the held hint's tag
  , rwHintData :: Maybe (BitVector 8)  -- ^ its byte, once in
  , rwLocal    :: Maybe (BitVector 8)  -- ^ the core's byte from the hint, given the next clock
  , rwMissed   :: Unsigned 8           -- ^ dots that went by while the core waited
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFDataX

romWaitInit :: RomWait
romWaitInit = RomWait Nothing Nothing 0 0 False False False 0 Nothing Nothing 0

-- | The core's byte this clock, from the state and the port's (registered)
-- answer: the hint's, kept or arriving, or its own request's.
romAnswer :: RomWait -> Maybe (BitVector 8) -> Maybe (BitVector 8)
romAnswer w a = rwLocal w <|> case rwOut w of
  Just KCore -> a
  Just (KHint t) | rwWaitHint w, rwHint w, t == rwHintTag w -> a
  _ -> Nothing

-- | One clock: the next state and the request to the port. @e@: the core
-- ran this clock (it may have made a request @cr@: its address and kind);
-- @a@: the port's answer; @d@: a dot went by.
romNext
  :: RomWait -> (Bool, Bool, Maybe (Unsigned 23, RomReq), Maybe (BitVector 8), Bool)
  -> (RomWait, Maybe (Unsigned 23))
romNext w (r, e, cr, a, d) = (if r then w' else hold, preq)
 where
  answered = isJust a
  out0 = if answered then Nothing else rwOut w
  coreReq = if e && r then cr else Nothing
  kind = snd <$> coreReq
  -- the addresses: straight in, by the request's kind only
  addr = maybe 0 fst cr
  coreAddr' = if kind == Just RRead || (kind == Just RUse && not (rwHint w)) then addr else rwCoreAddr w
  hintAddr' = if kind == Just RHint then addr else rwHintAddr w
  -- the held hint's byte, if it arrives now
  hintArrives = case rwOut w of
    Just (KHint t) | answered, rwHint w, t == rwHintTag w -> a
    _ -> Nothing
  hintData = hintArrives <|> rwHintData w
  newTag = complement (rwHintTag w)
  -- (a) a hint: queued, if there's room; (b) the hinted read: from the
  -- hint, at once or when it comes; (c) any other read: queued, the core
  -- waits
  (newItem, wait', waitHint', local')
    | kind == Just RHint = (Just (QHint newTag), False, False, Nothing)
    | kind == Just RUse, rwHint w = case hintData of
        Just v -> (Nothing, True, False, Just v)
        Nothing -> (Nothing, True, True, Nothing)
    | isJust kind = (Just QCore, True, False, Nothing)
    | e = (Nothing, False, False, Nothing)
    | otherwise = (Nothing, rwWait w, rwWaitHint w, Nothing)
  -- the queue: a real request goes before (and replaces) a queued hint; a
  -- hint finds no room if the queue already holds something
  queued = case (newItem, rwQueue w) of
    (Just QCore, _) -> Just QCore
    (Just n, Nothing) -> Just n
    (_, q) -> q
  hintAccepted = kind == Just RHint && rwQueue w == Nothing
  hintPushedOut = case (newItem, rwQueue w) of
    (Just QCore, Just (QHint _)) -> True
    _ -> False
  -- the held hint: a new one if it got into the queue (else none: the core
  -- then reads normally); ended when the hinted read takes its byte, or a
  -- real request pushes it out of the queue
  (hint', tag')
    | kind == Just RHint = (hintAccepted, if hintAccepted then newTag else rwHintTag w)
    | kind == Just RUse && rwHint w && isJust hintData = (False, rwHintTag w)
    | rwWaitHint w && isJust hintArrives = (False, rwHintTag w)
    | hintPushedOut = (False, rwHintTag w)
    | otherwise = (rwHint w, rwHintTag w)
  waitHint''
    | rwWaitHint w && isJust hintArrives = False
    | otherwise = waitHint'
  hintData'
    | kind == Just RHint = Nothing
    | hint' = hintData
    | otherwise = Nothing
  -- send when the port has nothing outstanding (rwOut is still set in the
  -- answer's own clock)
  canSend = rwOut w == Nothing
  sent = if canSend then queued else Nothing
  preq = case sent of
    Just QCore -> Just coreAddr'
    Just (QHint _) -> Just hintAddr'
    Nothing -> Nothing
  sentKind = case sent of
    Just QCore -> Just KCore
    Just (QHint t) -> Just (KHint t)
    Nothing -> Nothing
  w' = RomWait
    { rwOut = sentKind <|> out0
    , rwQueue = if canSend then Nothing else queued
    , rwCoreAddr = coreAddr'
    , rwHintAddr = hintAddr'
    , rwWait = wait'
    , rwWaitHint = waitHint''
    , rwHint = hint'
    , rwHintTag = tag'
    , rwHintData = hintData'
    , rwLocal = local'
    , rwMissed = missed }
  -- a hold: everything dropped; what is (or goes) out now is stale
  hold = romWaitInit { rwOut = fmap (const KStale) (sentKind <|> out0) }
  missed
    | not e && d = satSucc SatBound (rwMissed w)
    | e && not d && rwMissed w > 0 = rwMissed w - 1
    | otherwise = rwMissed w

-- | The core with its memories: block RAMs for cartridge RAM (true dual
-- port, up to 128 KiB: port A the core, port B the PS, so reading a save
-- never stops the game), WRAM, HRAM and VRAM (two banks), and the
-- cartridge ROM behind a ROM port: the port's answers in, its requests out
-- (the protocol of 'RomPort'; the port must not answer a request in its own
-- clock, so the loop has a register in it). Also returns port B's read
-- data.
--
-- __Waiting for ROM.__ The core and its memories run under a clock
-- enable that is low while the core waits for a ROM byte: the core's
-- state stands still, so it computes exactly what it would with the
-- answer at once (core = spec stays exact). A request the core makes
-- while the port is busy (the clock of an answer, or a stale answer from
-- before a hold still due) is queued and sent when the port is free;
-- answers to requests from before a hold are dropped. Dots that elapse
-- while the core waits are counted and given back one per clock later
-- (the core needs 18 to 20 of the 24 clocks of an M-cycle, so it catches
-- up), so a game keeps real time through short waits. The port's answers
-- and its requests are registered on the way, so every ROM read waits
-- at least two clocks more than with the block RAM (a cache hit: the
-- request a clock later, the answer the clock after, then a clock in
-- the register); the M-cycle's spare clocks take them.
gbCoreWith
  :: forall dom. HiddenClockResetEnable dom
  => CoreModel                                        -- ^ the console (fixed)
  -> Signal dom (Maybe (BitVector 8))                 -- ^ the ROM port's answers
  -> Signal dom Bool                                  -- ^ run (False: hold in the hand-off state, e.g. while loading)
  -> Signal dom Bool                                  -- ^ one dot elapsed
  -> Signal dom (BitVector 8)                         -- ^ keys
  -> Signal dom (Vec 10 (BitVector 8))                -- ^ MBC3's clock at start ('kiRtcInit')
  -> Signal dom (Vec 24 (BitVector 8))                -- ^ the GBC's palettes for a DMG cartridge ('kiPalInit')
  -> Signal dom CramPort                              -- ^ the PS: cartridge RAM
  -> (Signal dom CoreOut, Signal dom (BitVector 8), Signal dom (Maybe (Unsigned 23)))
gbCoreWith model answerIn run dot keys rtcInit palInit psCram = (outE, cramB, portReqOut)
 where
  -- the request one clock later too: no path from the core's state through
  -- its ROM address into the port (the DDR cache's tag compare; make bit:
  -- WNS -0.7 ns without it)
  portReqOut = register Nothing portReq
  -- the port's answer one clock later: no path from the port (the DDR
  -- cache's answer logic, the HP port) into the core's clock enable and
  -- read data (make bit: WNS -3.5 ns without it). A cache hit then costs
  -- the core a clock per ROM read; its 18 to 20 of 24 clocks per M-cycle
  -- leave room, and the missed dot comes back. The port sees its request
  -- answered a clock later too: only stricter for its one-at-a-time rule.
  answer = register Nothing answerIn
  en :: Signal dom Bool
  en = (\w a -> not (rwWait w) || isJust a) <$> rw <*> ans
  ans = romAnswer <$> rw <*> answer
  st = withEnable (toEnable en) (register coreInit (mux run st' (pure coreInit)))
  (st', out) = unbundle (coreStep model <$> st <*> inp)
  inp = CoreIn <$> kDot <*> rd <*> vpair <*> keys <*> rtcInit <*> vpair1 <*> palInit
  -- what the core does outside itself happens only on enabled clocks
  outE = (\e o -> if e then o else o { koMem = Nothing, koPixel = Nothing, koFrame = False
                                     , koSerial = Nothing, koEvent = Nothing, koVramPair = Nothing })
           <$> en <*> out
  req = koMem <$> outE

  -- the ROM port: the core's reads and hints ("romNext")
  romReq = (\r q -> case q of
    Just m | r, mqSpace m == SpRom -> Just (mqAddr m, RRead)
           | r, mqSpace m == SpRomHint -> Just (mqAddr m, RHint)
           | r, mqSpace m == SpRomUse -> Just (mqAddr m, RUse)
    _ -> Nothing) <$> run <*> req
  (rw', portReq) = unbundle (romNext <$> rw <*> bundle (run, en, romReq, answer, dot))
  rw = register romWaitInit rw'
  -- a dot for the core: this clock's, or one it missed while waiting
  kDot = (\d w -> d || rwMissed w > 0) <$> dot <*> rw

  sel sp = (\q -> case q of
    Just m | mqSpace m == sp -> Just m
    _ -> Nothing) <$> req
  rdAddr :: forall n. KnownNat n => Space -> Signal dom (Unsigned n)
  rdAddr sp = maybe 0 (resize . mqAddr) <$> sel sp
  wrOf :: forall n. KnownNat n => Space -> Signal dom (Maybe (Unsigned n, BitVector 8))
  wrOf sp = (\q -> q >>= \m -> (resize (mqAddr m),) <$> mqWrite m) <$> sel sp
  -- cartridge RAM: port A the core (a read whenever it doesn't write, as
  -- the single-port read address did), port B the PS. It has no enable:
  -- the core's requests are only there on enabled clocks (outE), and
  -- while the core waits, the byte it waits for is ROM's.
  ramOp :: Maybe (Unsigned 17, Maybe (BitVector 8)) -> RamOp 131072 (BitVector 8)
  ramOp (Just (a, Just d)) = RamWrite (fromIntegral a) d
  ramOp (Just (a, Nothing)) = RamRead (fromIntegral a)
  ramOp Nothing = RamRead 0
  coreCram = (\q -> (\m -> (resize (mqAddr m), mqWrite m)) <$> q) <$> sel SpCram
  (cram, cramB) = trueDualPortBlockRam (ramOp <$> coreCram) (ramOp <$> psCram)
  -- the rest stand still with the core (each under 'withEnable' itself:
  -- a binding of its own would take the hidden enable of its definition)
  wram = withEnable (toEnable en) (blockRam1 NoClearOnReset (SNat @32768) 0 (rdAddr @15 SpWram) (wrOf @15 SpWram))
  hram = withEnable (toEnable en) (blockRam1 NoClearOnReset (SNat @128) 0 (rdAddr @7 SpHram) (wrOf @7 SpHram))
  -- VRAM: bank 0 and bank 1 (the Game Boy Color) in block RAMs of their
  -- own, all four read at the same pair address, so the PPU gets a tile
  -- and its attributes (or a tile row from both banks) in one read
  vEven = withEnable (toEnable en) (blockRam1 NoClearOnReset (SNat @4096) 0 vAddr (vWr False False))
  vOdd = withEnable (toEnable en) (blockRam1 NoClearOnReset (SNat @4096) 0 vAddr (vWr False True))
  vEven1 = withEnable (toEnable en) (blockRam1 NoClearOnReset (SNat @4096) 0 vAddr (vWr True False))
  vOdd1 = withEnable (toEnable en) (blockRam1 NoClearOnReset (SNat @4096) 0 vAddr (vWr True True))
  lastSp = withEnable (toEnable en) (register Nothing ((fmap (\m -> (mqSpace m, testBit (mqAddr m) 0, testBit (mqAddr m) 13))) <$> req))
  -- VRAM: the PPU's pair reads and the CPU's/DMA's byte accesses never
  -- happen in the same clock
  vAddr :: Signal dom (Unsigned 12)
  vAddr = (\p q -> case (p, q) of
    (Just a, _) -> a
    (_, Just m) | mqSpace m == SpVram -> resize (shiftR (mqAddr m) 1)
    _ -> 0) <$> (koVramPair <$> outE) <*> req
  -- a CPU write to bank @b1@, even or odd byte (@oddB@)
  vWr b1 oddB = (\q -> q >>= \m -> if mqSpace m == SpVram && testBit (mqAddr m) 0 == oddB && testBit (mqAddr m) 13 == b1
                                     then (resize (shiftR (mqAddr m) 1) :: Unsigned 12,) <$> mqWrite m else Nothing) <$> req
  vpair = (++#) <$> vOdd <*> vEven
  vpair1 = (++#) <$> vOdd1 <*> vEven1
  rd = (\l r c w h (e, o, e1, o1) -> case l of
    Just (SpRom, _, _) -> r
    Just (SpRomHint, _, _) -> 0
    Just (SpRomUse, _, _) -> r
    Just (SpCram, _, _) -> c
    Just (SpWram, _, _) -> w
    Just (SpHram, _, _) -> h
    Just (SpVram, True, False) -> o
    Just (SpVram, False, False) -> e
    Just (SpVram, True, True) -> o1
    Just (SpVram, False, True) -> e1
    Nothing -> 0) <$> lastSp <*> (fromMaybe 0 <$> ans) <*> cram <*> wram <*> hram
                  <*> bundle (vEven, vOdd, vEven1, vOdd1)

-- | The state's bytes in the state space ('StateOp', side track L): the
-- state table ("Gb.StateTable", read only: 'StateTap') from 0x0000; the
-- status bytes at 'stateStatusAddr'; the ROM overlay's end address and
-- arm at 'overlayEndAddr' (written); HRAM at 0x1000 to 0x107F; VRAM bank
-- 0 at 0x2000 to 0x3FFF and bank 1 at 0x4000 to 0x5FFF; the overlay's
-- bytes at 'overlayBase' to 0x67FF (written); WRAM (all eight banks) at
-- 0x8000 to 0xFFFF. Cartridge RAM has its own port (the battery saves').
--
-- The status bytes, read any time, frozen or not: at 'stateStatusAddr' 1
-- while the core is frozen for state access (else 0); then the table's
-- size in bytes (low byte first) and its version: the PS keeps them in a
-- state file's header and refuses a state whose don't match the
-- bitstream's.
stateStatusAddr :: Unsigned 16
stateStatusAddr = 0x1F00

-- | The state table, for the hooks: given by "Gb.StateTap" (wiring from
-- the record's fields; nothing writes the record: a state is loaded
-- through the CPU and the ROM overlay).
data StateTap = StateTap
  { tapRead    :: CoreState -> Unsigned 16 -> BitVector 8 -- ^ byte n of the state table (0 past the end)
  , tapSize    :: Unsigned 16                              -- ^ how many
  , tapVersion :: BitVector 8                              -- ^ the table's version
  }

-- | Where a state can be taken (side track L, L.6c; the spec's
-- 'Gb.Spec.SaveState.specAtRest'): at an instruction's start, in
-- vertical blank from dot 56 (or the LCD off), the CPU not halted, no EI
-- pending, no interrupt about to be taken, no DMA, timer reload or speed
-- switch, TIMA at 8 or more while the timer runs. From there the restore
-- routine puts the machine back exactly (platform/ps/gb_restore.zig).
restoreable :: CoreState -> Bool
restoreable s =
  (not (psOn p) || (psLy p >= 144 && psDot p >= 56))
  && cPhase c == PhFetch && not (cHalt c) && not (cEi c) && not (cHaltBug c)
  && not (cIme c && (ksIe s .&. zeroExtend (ksIf s) .&. 0x1F) /= 0)
  && isNothing (ksDmaIdx s) && ksDmaPend s == 0 && ksReload s == 0
  && ksPause s == 0 && not (ksHdmaAct s) && not (ksHdmaRun s)
  && (not (testBit (ksTac s) 2) || ksTima s >= 8)
 where
  p = fst (ksPpu s)
  c = ksCpu s

-- | The ROM overlay's place in the state window (L.6c): its bytes (written
-- while frozen), and the address whose read ends it (low byte, high
-- byte), then 1 to arm it.
overlayBase, overlayEndAddr :: Unsigned 16
overlayBase = 0x6000
overlayEndAddr = 0x1F10

-- | At rest (side track L): between M-cycles with less than one M-cycle's
-- dots of credit (so, paused, it does nothing more: no memory access
-- under way, the PPU idle), and the ROM port with nothing outstanding.
restAt :: CoreModel -> CoreState -> RomWait -> Bool
restAt model s w = ksSeq s == QIdle && ksCredit s < dpm && rwOut w == Nothing && rwQueue w == Nothing
                   && not (rwWait w) && rwMissed w == 0
 where
  dpm = if model == CoreCgb && ccCgb (ksCfg s) && testBit (ksKey1 s) 7 then 2 else 4

-- | 'gbCoreWith' with the save-state hooks (side track L; "Platform.SaveState"),
-- built in when given the state table ('StateTap', "Gb.StateTap"): freeze
-- (the PS asks for state access; acted on once the core is at rest,
-- 'restAt', where a state can be taken, 'restoreable', or at once while
-- the PS holds it), the state accesses ('StateOp', only while frozen),
-- and the ROM overlay (L.6c): armed while frozen, it answers every ROM
-- read once the core has read the cartridge's header, until the routine
-- in it has read its last byte. Also out: the byte a read asked for, the
-- clock after, and the frozen flag. With 'Nothing' they are ignored and
-- the circuit is 'gbCoreWith''s.
gbCoreHooked
  :: forall dom. HiddenClockResetEnable dom
  => Maybe StateTap
  -> CoreModel
  -> Signal dom (Maybe (BitVector 8))
  -> Signal dom Bool
  -> Signal dom Bool
  -> Signal dom (BitVector 8)
  -> Signal dom (Vec 10 (BitVector 8))
  -> Signal dom (Vec 24 (BitVector 8))
  -> Signal dom CramPort
  -> Signal dom Bool                                  -- ^ freeze for state access (with the core paused)
  -> Signal dom (Maybe StateOp)                       -- ^ a state access
  -> ( Signal dom CoreOut, Signal dom (BitVector 8), Signal dom (Maybe (Unsigned 23))
     , Signal dom (Maybe (BitVector 8)), Signal dom Bool )
gbCoreHooked hooks model answerIn run dot keys rtcInit palInit psCram freezeIn opIn = (outE, cramB, portReqOut, stOut, frz)
 where
  -- the request one clock later too: no path from the core's state through
  -- its ROM address into the port (the DDR cache's tag compare; make bit:
  -- WNS -0.7 ns without it)
  portReqOut = register Nothing ((\o q -> if o then Nothing else q) <$> ovlOn <*> portReq)
  -- the port's answer one clock later: no path from the port (the DDR
  -- cache's answer logic, the HP port) into the core's clock enable and
  -- read data (make bit: WNS -3.5 ns without it). A cache hit then costs
  -- the core a clock per ROM read; its 18 to 20 of 24 clocks per M-cycle
  -- leave room, and the missed dot comes back. The port sees its request
  -- answered a clock later too: only stricter for its one-at-a-time rule.
  answer = register Nothing ((<|>) <$> ovlAns <*> answerIn)
  -- The ROM overlay (L.6c): the PS writes a restore routine into it while
  -- the core is frozen (and held: the reset), its end address, and arms
  -- it. Once the core has read the cartridge's header (its boot), every
  -- ROM port request is answered from the overlay's block RAM, at the
  -- address's low 11 bits (as a cache hit: two clocks), and none goes to
  -- the port. The routine ends with a JP whose last byte is at the end
  -- address: after a request for it (by the low 14 bits: bank-free), the
  -- next hint, the next M-cycle's ROM read, goes to the cartridge again,
  -- and the overlay is off until armed again.
  (ovlOn, ovlAns) = case hooks of
    Nothing -> (pure False, pure Nothing)
    Just _ ->
      let wr o = case o of
            Just (StWrite a d) | a >= overlayBase && a < overlayBase + 0x800 -> Just (resize (a - overlayBase) :: Unsigned 11, d)
            _ -> Nothing
          ram = blockRam1 NoClearOnReset (SNat @2048) 0 (maybe 0 resize <$> portReq) (wr <$> op)
          endLo = register 0 ((\o v -> case o of Just (StWrite a d) | a == overlayEndAddr -> d; _ -> v) <$> op <*> endLo)
          endHi = register 0 ((\o v -> case o of Just (StWrite a d) | a == overlayEndAddr + 1 -> d; _ -> v) <$> op <*> endHi)
          endA = (\h l -> slice d5 d0 h ++# l) <$> endHi <*> endLo :: Signal dom (BitVector 14)
          booted = (\x -> case ksSeq x of QBoot -> False; QHdr _ -> False; _ -> True) <$> st
          -- armed and the end seen: registers; off at the hint after the end
          armed = register False armed'
          seen = register False seen'
          isHint = (\q -> case q of Just (_, RHint) -> True; _ -> False) <$> romReq
          isEnd = (\q e -> case q of Just (a, _) -> slice d13 d0 (pack a) == e; _ -> False) <$> romReq <*> endA
          done = (&&) <$> seen <*> isHint
          on = (\a b d -> a && b && not d) <$> armed <*> booted <*> done
          armed' = (\o a d -> case o of
                     Just (StWrite x v) | x == overlayEndAddr + 2 -> testBit v 0
                     _ -> a && not d) <$> op <*> armed <*> done
          seen' = (\a d e n -> a && not d && (n || e)) <$> armed' <*> done <*> ((&&) <$> on <*> isEnd) <*> seen
          -- a request the overlay takes: its byte two clocks later
          hit = register False ((\o q -> o && isJust q) <$> on <*> portReq)
          ansO = register Nothing ((\h v -> if h then Just v else Nothing) <$> hit <*> ram)
      in (on, ansO)
  en :: Signal dom Bool
  en = (\w a -> not (rwWait w) || isJust a) <$> rw <*> ans
  ans = romAnswer <$> rw <*> answer
  st = case hooks of
    Nothing -> withEnable (toEnable en) (register coreInit (mux run st' (pure coreInit)))
    -- frozen: the record holds (nothing writes it: L.6c); held, it is
    -- reset, frozen or not (a load: the PS freezes the game, checks the
    -- slot, then holds the core; found on the board)
    Just _ -> withEnable (toEnable ((||) <$> en <*> frz))
      (register coreInit ((\z r s' s -> if not r then coreInit else if z then s else s')
                            <$> frz <*> run <*> st' <*> st))
  -- the DMG (side track L, make bit): its Game Boy Color parts never leave
  -- their reset values (nothing writes the record), so synthesis drops
  -- the logic behind them; a state written in mustn't reach the second
  -- VRAM bank and WRAM's banks 2 to 7 (they read as 0): with them the
  -- DMG's gb.bit didn't fit (55105 LUTs of 53200)
  dmg = model == CoreDmg
  dmgOp o
    | dmg = case o of
        Just (StRead a) | cgbOnly a -> Just (StRead 0x7000) -- no memory there: reads 0
        Just (StWrite a _) | cgbOnly a -> Nothing
        _ -> o
    | otherwise = o
  cgbOnly a = (a >= 0x4000 && a < 0x6000) || a >= 0xA000
  -- the hooks (side track L): frozen once asked and at rest, then as long
  -- as asked; accesses only while frozen
  (frz, op) = case hooks of
    Nothing -> (pure False, pure Nothing)
    Just _ ->
      let z = register False ((\f r q -> f && (q || r)) <$> freezeIn <*> atRest <*> z)
          -- (while the PS holds the core, at once: a load writes the
          -- memories into the reset core)
          atRest = (\r s w -> not r || (restAt model s w && restoreable s)) <$> run <*> st <*> rw
      -- (the status bytes are read any time)
      in (z, (\q o -> dmgOp (if q then o else (case o of
                Just (StRead a) | a >= stateStatusAddr && a < stateStatusAddr + 4 -> o
                _ -> Nothing))) <$> z <*> opIn)
  -- the byte a read asked for, the clock after: the record's (registered),
  -- the status, or a block RAM's output
  lastOp = register Nothing op
  recByte = case hooks of
    Nothing -> pure 0
    Just tp -> register 0 ((\o s -> case o of
                            Just (StRead a) | a == stateStatusAddr -> 0
                                            | a == stateStatusAddr + 1 -> resize (pack (tapSize tp))
                                            | a == stateStatusAddr + 2 -> slice d15 d8 (pack (tapSize tp))
                                            | a == stateStatusAddr + 3 -> tapVersion tp
                                            | a < 0x1000 -> tapRead tp s a
                                            | otherwise -> 0
                            _ -> 0) <$> op <*> st)
  stOut = case hooks of
    Nothing -> pure Nothing
    Just _ -> (\o f r h w (e, od, e1, o1) -> case o of
      Just (StRead a)
        | a == stateStatusAddr -> Just (if f then 1 else 0)
        | a < 0x1000 || (a > stateStatusAddr && a < stateStatusAddr + 4) -> Just r
        | a >= 0x1000 && a < 0x1080 -> Just h
        | a >= 0x2000 && a < 0x6000 -> Just (case (a >= 0x4000, testBit a 0) of
            (False, False) -> e
            (False, True) -> od
            (True, False) -> e1
            (True, True) -> o1)
        | a >= 0x8000 -> Just w
        | otherwise -> Just 0
      _ -> Nothing) <$> lastOp <*> frzNow <*> recByte <*> hram <*> wram <*> bundle (vEven, vOdd, vEven1, vOdd1)
  -- the status read sees the flag as it was when asked
  frzNow = register False frz
  -- while frozen the block RAMs are the hooks': each one's address (and
  -- write) from the access, if it falls in its range
  -- (the end in 17 bits: WRAM's is 0x10000)
  hookIn :: forall n. KnownNat n => Unsigned 16 -> Unsigned 17 -> Maybe StateOp -> Maybe (Unsigned n, Maybe (BitVector 8))
  hookIn lo hi o = case o of
    Just (StRead a) | a >= lo && resize a < hi -> Just (resize (a - lo), Nothing)
    Just (StWrite a b) | a >= lo && resize a < hi -> Just (resize (a - lo), Just b)
    _ -> Nothing
  hooked :: forall n. KnownNat n => Unsigned 16 -> Unsigned 17 -> Signal dom (Unsigned n) -> Signal dom (Maybe (Unsigned n, BitVector 8))
         -> (Signal dom (Unsigned n), Signal dom (Maybe (Unsigned n, BitVector 8)))
  hooked lo hi a w = case hooks of
    Nothing -> (a, w)
    Just _ ->
      ( (\z o x -> if z then maybe 0 fst (hookIn lo hi o) else x) <$> frz <*> op <*> a
      , (\z o x -> if z then (hookIn lo hi o >>= \(i, v) -> (i,) <$> v) else x) <$> frz <*> op <*> w )
  memEn = case hooks of
    Nothing -> en
    Just _ -> (||) <$> en <*> frz
  (st', out) = unbundle (coreStep model <$> st <*> inp)
  inp = CoreIn <$> kDot <*> rd <*> vpair <*> keys <*> rtcInit <*> vpair1 <*> palInit
  -- what the core does outside itself happens only on enabled clocks
  outE = (\e z o -> if e && not z then o else o { koMem = Nothing, koPixel = Nothing, koFrame = False
                                     , koSerial = Nothing, koEvent = Nothing, koVramPair = Nothing })
           <$> en <*> frz <*> out
  req = koMem <$> outE

  -- the ROM port: the core's reads and hints ("romNext")
  romReq = (\r q -> case q of
    Just m | r, mqSpace m == SpRom -> Just (mqAddr m, RRead)
           | r, mqSpace m == SpRomHint -> Just (mqAddr m, RHint)
           | r, mqSpace m == SpRomUse -> Just (mqAddr m, RUse)
    _ -> Nothing) <$> run <*> req
  (rw', portReq) = unbundle (romNext <$> rw <*> bundle (run, en, romReq, answer, dot))
  rw = case hooks of
    Nothing -> register romWaitInit rw'
    -- frozen: held; a state written in starts the port afresh (its hint
    -- was for the old state's next read)
    Just _ -> register romWaitInit ((\z o w w' -> if z then (if maybe False stateOpWrite o then romWaitInit else w) else w')
                                              <$> frz <*> op <*> rw <*> rw')
  -- a dot for the core: this clock's, or one it missed while waiting
  kDot = (\d w -> d || rwMissed w > 0) <$> dot <*> rw

  sel sp = (\q -> case q of
    Just m | mqSpace m == sp -> Just m
    _ -> Nothing) <$> req
  rdAddr :: forall n. KnownNat n => Space -> Signal dom (Unsigned n)
  rdAddr sp = maybe 0 (resize . mqAddr) <$> sel sp
  wrOf :: forall n. KnownNat n => Space -> Signal dom (Maybe (Unsigned n, BitVector 8))
  wrOf sp = (\q -> q >>= \m -> (resize (mqAddr m),) <$> mqWrite m) <$> sel sp
  -- cartridge RAM: port A the core (a read whenever it doesn't write, as
  -- the single-port read address did), port B the PS. It has no enable:
  -- the core's requests are only there on enabled clocks (outE), and
  -- while the core waits, the byte it waits for is ROM's.
  ramOp :: Maybe (Unsigned 17, Maybe (BitVector 8)) -> RamOp 131072 (BitVector 8)
  ramOp (Just (a, Just d)) = RamWrite (fromIntegral a) d
  ramOp (Just (a, Nothing)) = RamRead (fromIntegral a)
  ramOp Nothing = RamRead 0
  coreCram = (\q -> (\m -> (resize (mqAddr m), mqWrite m)) <$> q) <$> sel SpCram
  (cram, cramB) = trueDualPortBlockRam (ramOp <$> coreCram) (ramOp <$> psCram)
  -- the rest stand still with the core (each under 'withEnable' itself:
  -- a binding of its own would take the hidden enable of its definition)
  (wramA, wramW) = hooked 0x8000 0x10000 (rdAddr @15 SpWram) (wrOf @15 SpWram)
  (hramA, hramW) = hooked 0x1000 0x1080 (rdAddr @7 SpHram) (wrOf @7 SpHram)
  wram = withEnable (toEnable memEn) (blockRam1 NoClearOnReset (SNat @32768) 0 wramA wramW)
  hram = withEnable (toEnable memEn) (blockRam1 NoClearOnReset (SNat @128) 0 hramA hramW)
  -- VRAM: bank 0 and bank 1 (the Game Boy Color) in block RAMs of their
  -- own, all four read at the same pair address, so the PPU gets a tile
  -- and its attributes (or a tile row from both banks) in one read
  vEven = withEnable (toEnable memEn) (blockRam1 NoClearOnReset (SNat @4096) 0 vAddrH (vWrH False False))
  vOdd = withEnable (toEnable memEn) (blockRam1 NoClearOnReset (SNat @4096) 0 vAddrH (vWrH False True))
  vEven1 = withEnable (toEnable memEn) (blockRam1 NoClearOnReset (SNat @4096) 0 vAddrH (vWrH True False))
  vOdd1 = withEnable (toEnable memEn) (blockRam1 NoClearOnReset (SNat @4096) 0 vAddrH (vWrH True True))
  -- VRAM for the hooks: byte o of bank b at 0x2000 + 0x2000 b + o, the pair
  -- address o / 2 in each bank's even and odd block RAMs
  hookV :: Maybe StateOp -> Maybe (Bool, Bool, Unsigned 12, Maybe (BitVector 8))
  hookV o = case o of
    Just (StRead a) | a >= 0x2000 && a < 0x6000 -> Just (vb a, testBit a 0, vp a, Nothing)
    Just (StWrite a d) | a >= 0x2000 && a < 0x6000 -> Just (vb a, testBit a 0, vp a, Just d)
    _ -> Nothing
  vb a = a >= 0x4000
  vp a = resize (shiftR (a .&. 0x1FFF) 1)
  vAddrH = case hooks of
    Nothing -> vAddr
    Just _ -> (\z o x -> if z then maybe 0 (\(_, _, p, _) -> p) (hookV o) else x) <$> frz <*> op <*> vAddr
  vWrH b1 oddB = case hooks of
    Nothing -> vWr b1 oddB
    Just _ -> (\z o x -> if z then (case hookV o of
                                   Just (b, od, p, Just d) | b == b1 && od == oddB -> Just (p, d)
                                   _ -> Nothing) else x) <$> frz <*> op <*> vWr b1 oddB
  lastSp = withEnable (toEnable en) (register Nothing ((fmap (\m -> (mqSpace m, testBit (mqAddr m) 0, testBit (mqAddr m) 13))) <$> req))
  -- VRAM: the PPU's pair reads and the CPU's/DMA's byte accesses never
  -- happen in the same clock
  vAddr :: Signal dom (Unsigned 12)
  vAddr = (\p q -> case (p, q) of
    (Just a, _) -> a
    (_, Just m) | mqSpace m == SpVram -> resize (shiftR (mqAddr m) 1)
    _ -> 0) <$> (koVramPair <$> outE) <*> req
  -- a CPU write to bank @b1@, even or odd byte (@oddB@)
  vWr b1 oddB = (\q -> q >>= \m -> if mqSpace m == SpVram && testBit (mqAddr m) 0 == oddB && testBit (mqAddr m) 13 == b1
                                     then (resize (shiftR (mqAddr m) 1) :: Unsigned 12,) <$> mqWrite m else Nothing) <$> req
  vpair = (++#) <$> vOdd <*> vEven
  vpair1 = (++#) <$> vOdd1 <*> vEven1
  rd = (\l r c w h (e, o, e1, o1) -> case l of
    Just (SpRom, _, _) -> r
    Just (SpRomHint, _, _) -> 0
    Just (SpRomUse, _, _) -> r
    Just (SpCram, _, _) -> c
    Just (SpWram, _, _) -> w
    Just (SpHram, _, _) -> h
    Just (SpVram, True, False) -> o
    Just (SpVram, False, False) -> e
    Just (SpVram, True, True) -> o1
    Just (SpVram, False, True) -> e1
    Nothing -> 0) <$> lastSp <*> (fromMaybe 0 <$> ans) <*> cram <*> wram <*> hram
                  <*> bundle (vEven, vOdd, vEven1, vOdd1)
