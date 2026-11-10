{-# OPTIONS_GHC -fconstraint-solver-iterations=0 #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE StrictData #-}
{-|
The 5A22 in Clash (PLAN.md step 7.9a; docs/systems/snes.md, "The machine
in Clash"): the 65C816 core ("W65c816.Core") with its memory map and
access speeds, the registers at 4200h-437Fh (NMI, the H/V IRQ, multiply
and divide, the joypads and their auto-read, DMA's and HDMA's channels),
the WRAM port at 2180h-2183h, DMA and HDMA, the WRAM refresh, and the
master clock's H and V; "Snes.Spec.Machine" transcribed, a pixel clock a
step, with the memories, the PPU and the sound module's ports outside.

__Time.__ A pixel clock does at most one master clock's tick ('biRun':
the machine's enable), with the line's events as H reaches them, then at
most one zero-time step of the bus's owner: an access (a read's value is
there on the next clock), a cycle's end. The spec's order is kept: a
tick, its events, then the accesses at that master clock.

__Two owners.__ The background is the CPU's cycles (6, 8 or 12 master
clocks: a read 4 before the end, a write at the end) and the DMA a write
to 420Bh starts. The foreground is what the events start: the WRAM
refresh (40 clocks at H 538), HDMA's set-up (line 0, H 12) and its
transfers (H 1116). The foreground holds the background while it runs
(the spec's 'tick' recursing into an event's own ticks).

__A DMA or HDMA byte__ is its first access (the read) with the tick that
ends the chunk before it, its second (the write, at the same master
clock) on the next pixel clock, then 8 ticks.

__The sound module's ports__ ('boApu'): an access waits for the module to
have caught up ('biApuReady0', or 'biApuReady1' in a clock with a tick:
the tick may raise its target; "Snes.Apu"); its read comes back on the
next clock.
-}
module Snes.Bus
  ( -- * Configuration
    CartMap (..)
  , CartCfg (..)
    -- * Interface
  , BusIn (..)
  , BusOut (..)
  , BusEv (..)
  , busIdleIn
    -- * State
  , BgStep (..)
  , FgStep (..)
  , Src (..)
  , BusC (..)
  , busInit
  , busClock
    -- * The map
  , Target (..)
  , decode
  , speed
  ) where

import Clash.Prelude
#ifdef SNES_DSP1
import Snes.Upd77c25 (SnesAcc (..))
import Data.Maybe (isJust)
#endif

import Snes.Ppu (Access (..))
import W65c816.Core (Cpu (..), CpuIn (..), Req (..), BusOp (..), Kind (..), Stage (..), StepKind (..), cpuInit, cpuClock, coreW65c816)

-- * Configuration

data CartMap = CmLoRom | CmHiRom | CmExHiRom
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFDataX, BitPack)

-- | The cartridge as the PS describes it: the mapping; the ROM's size as
-- two powers of two (the larger, then the rest, 0 = none: 3 MiB is 2 + 1;
-- the spec's mirroring for any size of at most two such parts); SRAM's
-- size (a power of two, 0 = none).
data CartCfg = CartCfg
  { ccMap   :: CartMap
  , ccRomP  :: Unsigned 24
  , ccRomQ  :: Unsigned 24
  , ccSram  :: Unsigned 17
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (NFDataX, BitPack)

-- * The map ("Snes.Spec.Map")

data Target
  = TWram (Unsigned 17)
  | TRom (Unsigned 23)
  | TSram (Unsigned 15)
  | TBbus (Unsigned 8)
  | TJoy Bool             -- ^ 4017h?
  | TCpuReg (Unsigned 16) -- ^ 4200h-43FFh
  | TOpen
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFDataX, BitPack)

decode :: CartCfg -> Unsigned 24 -> Target
decode cfg a
  | bank == 0x7E || bank == 0x7F = TWram (resize a)
  | system && off < 0x2000 = TWram (resize off)
  | system && off >= 0x2100 && off < 0x2200 = TBbus (resize off)
  | system && (off == 0x4016 || off == 0x4017) = TJoy (off == 0x4017)
  | system && off >= 0x4200 && off < 0x4400 = TCpuReg off
  | system && off < 0x6000 = TOpen
  | otherwise = case ccMap cfg of
      CmLoRom
        | sram > 0 && b7 >= 0x70 && b7 < 0x7E && off < 0x8000 ->
            TSram (resize (mirrorP ((resize (b7 .&. 0x0F) `shiftL` 15) .|. resize off) sram))
        | off >= 0x8000 -> romAt ((resize b7 `shiftL` 15) .|. resize (off .&. 0x7FFF))
        | not system && b7 >= 0x40 -> romAt ((resize b7 `shiftL` 15) .|. resize (off .&. 0x7FFF))
        | otherwise -> TOpen
      _
        | system && sram > 0 && b7 >= 0x20 && b7 < 0x40 && off >= 0x6000 && off < 0x8000 ->
            TSram (resize (mirrorP ((resize (b7 .&. 0x1F) `shiftL` 13) .|. resize (off - 0x6000)) sram))
        | system && off < 0x8000 -> TOpen
        | otherwise -> romAt (exhi .|. (resize (b7 .&. 0x3F) `shiftL` 16) .|. resize off)
 where
  bank = resize (a `shiftR` 16) :: Unsigned 8
  b7 = bank .&. 0x7F
  off = resize a :: Unsigned 16
  system = b7 < 0x40
  sram = resize (ccSram cfg) :: Unsigned 24
  exhi = if ccMap cfg == CmExHiRom && bank < 0x80 then 0x400000 else 0 :: Unsigned 24
  romAt o = if ccRomP cfg == 0 then TOpen else TRom (resize (mirror2 o (ccRomP cfg) (ccRomQ cfg)))

-- | An address in a memory of a power of two's size.
mirrorP :: Unsigned 24 -> Unsigned 24 -> Unsigned 24
mirrorP o size = o .&. (size - 1)

-- | The spec's mirroring ('Snes.Spec.Map.mirror') for a size P + Q (powers
-- of two, Q < P or 0): within 2P; the first P as they are, the rest the
-- Q part mirrored after it.
mirror2 :: Unsigned 24 -> Unsigned 24 -> Unsigned 24 -> Unsigned 24
mirror2 o p q
  | q == 0 = o .&. (p - 1)
  | otherwise =
      let a = o .&. (2 * p - 1)
      in if a < p then a else p + ((a - p) .&. (q - 1))

-- | Master clocks for a CPU cycle at an address ("Snes.Spec.Map.speed").
speed :: Bool -> Unsigned 24 -> Unsigned 4
speed memsel a
  | bank >= 0x40 && bank < 0x80 = 8
  | bank >= 0xC0 = if memsel then 6 else 8
  | off < 0x2000 = 8
  | off < 0x4000 = 6
  | off < 0x4200 = 12
  | off < 0x6000 = 6
  | off < 0x8000 = 8
  | bank >= 0x80 && memsel = 6
  | otherwise = 8
 where
  bank = resize (a `shiftR` 16) :: Unsigned 8
  off = resize a :: Unsigned 16

-- * Interface

data BusIn = BusIn
  { biRun      :: Bool                   -- ^ a master clock may pass
  , biCfg      :: CartCfg
  , biPads     :: Vec 2 (Unsigned 12)    -- ^ B, Y, Select, Start, up, down, left, right, A, X, L, R
  , biWram     :: Unsigned 8             -- ^ WRAM at last clock's address
  , biSram     :: Unsigned 8
  , biRom      :: Maybe (Unsigned 8)     -- ^ the ROM read's byte, when it is there
  , biPpuRead  :: Maybe (Maybe (Unsigned 8)) -- ^ the PPU's 'Snes.Ppu.poRead'
  , biApuReady0 :: Bool                  -- ^ the sound module has caught up (no tick this clock)
  , biApuReady1 :: Bool                  -- ^ the same with a tick this clock
  , biApuRead  :: Unsigned 8             -- ^ its port's value, the clock after the read
  , biDma      :: Vec 16 (Unsigned 8)    -- ^ the DMA registers of the channel 'boDmaRd' asked for last clock
  , biOvlOn    :: Bool                   -- ^ a save state's ROM overlay is armed (step 7.15: its routine runs)
  , biOvl      :: Unsigned 8             -- ^ its byte at last clock's 'boOvl'
#ifdef SNES_DSP1
  , biDspReady0 :: Bool                  -- ^ (the DSP-1 build) the chip has caught up (no tick this clock)
  , biDspReady1 :: Bool                  -- ^ the same with a tick this clock
  , biDspRead  :: Unsigned 8             -- ^ its DR's or SR's byte, the clock after the read
#endif
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (NFDataX, BitPack)

busIdleIn :: CartCfg -> BusIn
#ifdef SNES_DSP1
busIdleIn cfg = BusIn False cfg (repeat 0) 0 0 Nothing Nothing True True 0 (repeat 0xFF) False 0 True True 0
#else
busIdleIn cfg = BusIn False cfg (repeat 0) 0 0 Nothing Nothing True True 0 (repeat 0xFF) False 0
#endif

-- | The bus work for the lockstep tests (the spec's 'Snes.Spec.Machine.BusEvent').
data BusEv
  = EvRead (Unsigned 24) (Unsigned 8)
  | EvWrite (Unsigned 24) (Unsigned 8)
  | EvIdle
  | EvDma (Unsigned 24) (Unsigned 8) (Unsigned 8) Bool
  | EvTable (Unsigned 24) (Unsigned 8)
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFDataX, BitPack)

data BusOut = BusOut
  { boTick    :: Bool                            -- ^ a master clock passed
  , boPpu     :: Maybe Access                    -- ^ an access to 2100h-213Fh (or the counters latched)
  , boWram    :: Unsigned 17
  , boWramW   :: Maybe (Unsigned 8)
  , boSram    :: Unsigned 15
  , boSramW   :: Maybe (Unsigned 8)
  , boRom     :: Maybe (Unsigned 23)             -- ^ a ROM read, for one clock
  , boApu     :: Maybe (Unsigned 2, Maybe (Unsigned 8)) -- ^ a port access: read, or write
  , boDmaRd   :: Index 8                         -- ^ the DMA registers: a channel's read (its 16 bytes next clock)
  , boDmaWCh  :: Index 8                         -- ^ and the channel written
  , boDmaW    :: Vec 16 (Maybe (Unsigned 8))     -- ^ its registers written
  , boLineEnd :: Bool                            -- ^ a line ended
  , boEv      :: Maybe (Unsigned 32, BusEv)      -- ^ for the tests: the work and its master clock
  , boStep    :: Maybe StepKind                  -- ^ the CPU finished a step
  , boOvl     :: Maybe (Unsigned 16)             -- ^ a read of the save state's overlay (its byte next clock: 'biOvl')
#ifdef SNES_DSP1
  , boDsp     :: Maybe SnesAcc                   -- ^ (the DSP-1 build) an access to its DR or SR (a read's byte next clock: 'biDspRead')
#endif
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (NFDataX, BitPack)

-- * State

-- | The background's step: a CPU cycle (its ticks so far), or DMA.
data BgStep
  = BgCpu
  | BgDAlign      -- ^ ticks to a multiple of 8 ('bLeft')
  | BgDStart      -- ^ the 8 before the first channel
  | BgDChan       -- ^ the channel's 8
  | BgDByteA      -- ^ a byte's first access
  | BgDByteB      -- ^ its second
  | BgDByteT      -- ^ its 8
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFDataX, BitPack)

data FgStep
  = FgNone
  | FgRefresh
  | FgHStart      -- ^ HDMA's 18 (set-up or transfers: 'fInit')
  | FgHChan       -- ^ a channel's 8
  | FgHByteA
  | FgHByteB
  | FgHByteT
  | FgRel1        -- ^ the table's count read
  | FgRel2        -- ^ its value
  | FgRel3        -- ^ indirect: the address's low byte
  | FgRel4        -- ^ its high byte
  | FgRelT        -- ^ indirect: the 16
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFDataX, BitPack)

-- | Where a read's value comes from on the next clock.
data Src = SrcNone | SrcNow | SrcWram | SrcSram | SrcRom | SrcPpu | SrcApu | SrcDma (Index 16) | SrcOvl
#ifdef SNES_DSP1
  | SrcDsp
#endif
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFDataX, BitPack)

-- | A DMA channel's registers as its engine works on them (loaded when
-- the channel starts; what changes is written back as it changes).
data Chan = Chan
  { chDmap :: Unsigned 8
  , chBbad :: Unsigned 8
  , chA1t  :: Unsigned 16
  , chBank :: Unsigned 8
  , chDas  :: Unsigned 16
  , chDasb :: Unsigned 8
  , chA2a  :: Unsigned 16
  , chNtrl :: Unsigned 8
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (NFDataX, BitPack)

chan0 :: Chan
chan0 = Chan 0xFF 0xFF 0xFFFF 0xFF 0xFFFF 0xFF 0xFFFF 0xFF

fromRegs :: Vec 16 (Unsigned 8) -> Chan
fromRegs r = Chan (r !! (0 :: Int)) (r !! (1 :: Int)) (w 2) (r !! (4 :: Int)) (w 5) (r !! (7 :: Int)) (w 8) (r !! (10 :: Int))
 where
  w :: Int -> Unsigned 16
  w k = (resize (r !! (k + 1)) `shiftL` 8) .|. resize (r !! k)

data BusC = BusC
  { bCpu       :: Cpu
    -- the background
  , bStep      :: BgStep
  , bTicks     :: Unsigned 4     -- ^ the CPU cycle's ticks so far
  , bAluDone   :: Bool           -- ^ a write cycle's ALU step (before its ticks) done
  , bReadDone  :: Bool           -- ^ the read cycle's read issued
  , bReadMc    :: Unsigned 32
  , bLeft      :: Unsigned 4     -- ^ ticks left of a DMA chunk
  , bChans     :: Unsigned 8     -- ^ DMA's channels still to run
  , bCh        :: Index 8
  , bK         :: Unsigned 2     -- ^ the byte's place in its transfer pattern
  , bW         :: Chan           -- ^ DMA's channel
  , bLoad      :: Bool           -- ^ its registers arrive this clock
    -- the foreground
  , fStep      :: FgStep
  , fLeft      :: Unsigned 6
  , fInit      :: Bool           -- ^ HDMA's set-up (else its transfers)
  , fChans     :: Unsigned 8
  , fCh        :: Index 8
  , fOff       :: Unsigned 2     -- ^ the byte in the channel's pattern
  , fIrqAfter  :: Bool           -- ^ the H IRQ at 538 sets TIMEUP after the refresh
  , fW         :: Chan           -- ^ HDMA's channel
  , fLoad      :: Bool
    -- a read's value
  , rSrc       :: Src
  , rMdr       :: Unsigned 8     -- ^ open bus at the read
  , rVal       :: Unsigned 8
  , rReady     :: Bool
  , rMc        :: Unsigned 32    -- ^ the master clock of the read (for 'boEv')
  , rAddr      :: Unsigned 24
  , rCpu       :: Bool           -- ^ the read in flight is the CPU's
  , bCpuVal    :: Unsigned 8     -- ^ the CPU's read: its own latch, as HDMA may read between it and the cycle's end
  , bCpuOk     :: Bool
    -- time
  , bHc        :: Unsigned 11
  , bVc        :: Unsigned 9
  , bField     :: Bool
  , bMc        :: Unsigned 32
  , bSetini    :: Unsigned 8
    -- the registers
  , bMdr       :: Unsigned 8
  , bNmitimen  :: Unsigned 8
  , bRdnmi     :: Bool
  , bTimeup    :: Bool
  , bWrio      :: Unsigned 8
  , bMpya      :: Unsigned 8
  , bDiv       :: Unsigned 16
  , bRddiv     :: Unsigned 16
  , bRdmpy     :: Unsigned 16
  , bMpyCtr    :: Unsigned 4
  , bDivCtr    :: Unsigned 5
  , bShift     :: Unsigned 24
  , bHtime     :: Unsigned 9
  , bVtime     :: Unsigned 9
  , bDmaPend   :: Unsigned 8
  , bHdmaen    :: Unsigned 8
  , bMemsel    :: Bool
  , bJoy       :: Vec 4 (Unsigned 16)
  , bStrobe    :: Bool
  , bSer       :: Vec 2 (Unsigned 16)
  , bJoyBusy   :: Unsigned 13
  , bWmadd     :: Unsigned 17
  , bHdmaDo    :: Unsigned 8
  , bHdmaDone  :: Unsigned 8
    -- for the trace only (the freeze hunt, 2026-10-20; "Snes.Machine"):
  , bLastOp    :: Unsigned 24    -- ^ the last opcode fetch's address
  , bBrk       :: Maybe (Unsigned 24, Unsigned 24, Unsigned 8)
    -- ^ the first opcode fetch that read BRK (00h) or COP (02h): the opcode
    -- before it, its address, the byte
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (NFDataX, BitPack)

busInit :: BusC
busInit = BusC
  { bCpu = cpuInit
  , bStep = BgCpu, bTicks = 0, bAluDone = False, bReadDone = False, bReadMc = 0
  , bLeft = 0, bChans = 0, bCh = 0, bK = 0, bW = chan0, bLoad = False
  , fStep = FgNone, fLeft = 0, fInit = False, fChans = 0, fCh = 0, fOff = 0, fIrqAfter = False
  , fW = chan0, fLoad = False
  , rSrc = SrcNone, rMdr = 0, rVal = 0, rReady = False, rMc = 0, rAddr = 0
  , rCpu = False, bCpuVal = 0, bCpuOk = False
  , bHc = 0, bVc = 0, bField = False, bMc = 0, bSetini = 0
  , bMdr = 0, bNmitimen = 0, bRdnmi = False, bTimeup = False, bWrio = 0xFF, bMpya = 0
  , bDiv = 0, bRddiv = 0, bRdmpy = 0, bMpyCtr = 0, bDivCtr = 0, bShift = 0
  , bHtime = 0, bVtime = 0, bDmaPend = 0, bHdmaen = 0, bMemsel = False
  , bJoy = repeat 0, bStrobe = False, bSer = repeat 0, bJoyBusy = 0, bWmadd = 0, bLastOp = 0, bBrk = Nothing
  , bHdmaDo = 0, bHdmaDone = 0
  }

-- * Time

vblankStart :: BusC -> Unsigned 9
vblankStart c = if testBit (bSetini c) 2 then 240 else 225

lineLength :: BusC -> Unsigned 11
lineLength c = if bVc c == 240 && bField c && not (testBit (bSetini c) 0) then 1360 else 1364

linesPerFrame :: BusC -> Unsigned 9
linesPerFrame c = if testBit (bSetini c) 0 && not (bField c) then 263 else 262

irqPosition :: BusC -> Maybe (Unsigned 11)
irqPosition c = case bNmitimen c .&. 0x30 of
  0 -> Nothing
  0x20 -> Just 10
  _ -> Just (resize (bHtime c) * 4 + 14)

-- | A transfer mode's B-bus offsets (k mod the pattern's length).
patOff :: Unsigned 8 -> Unsigned 2 -> Unsigned 8
patOff dmap k = case dmap .&. 7 of
  0 -> 0
  1 -> resize (k .&. 1)
  2 -> 0
  3 -> resize (k `shiftR` 1)
  4 -> resize k
  5 -> resize (k .&. 1)
  6 -> 0
  _ -> resize (k `shiftR` 1)

patLen :: Unsigned 8 -> Unsigned 3
patLen dmap = case dmap .&. 7 of
  0 -> 1
  1 -> 2
  2 -> 2
  5 -> 4
  6 -> 2
  _ -> 4

-- | A pad's 12 buttons as its 16-bit serial word.
padWord :: Unsigned 12 -> Unsigned 16
padWord p = resize p `shiftL` 4

-- | The multiply and divide units, a step a CPU cycle (bsnes's algorithm).
aluStep :: BusC -> BusC
aluStep c0 =
  let c1 = if bMpyCtr c0 > 0
        then let rd = bRddiv c0
                 sh = bShift c0
             in c0 { bMpyCtr = bMpyCtr c0 - 1
                   , bRdmpy = if testBit rd 0 then bRdmpy c0 + resize sh else bRdmpy c0
                   , bRddiv = rd `shiftR` 1
                   , bShift = sh `shiftL` 1 }
        else c0
  in if bDivCtr c1 > 0
       then let rd' = bRddiv c1 `shiftL` 1
                sh' = bShift c1 `shiftR` 1
                rm = resize (bRdmpy c1) :: Unsigned 24
            in if rm >= sh'
                 then c1 { bDivCtr = bDivCtr c1 - 1, bRdmpy = resize (rm - sh'), bRddiv = rd' .|. 1, bShift = sh' }
                 else c1 { bDivCtr = bDivCtr c1 - 1, bRddiv = rd', bShift = sh' }
       else c1

-- * Requests and the access unit

-- | An access a step asks for, which the one access unit carries out:
-- a read or write on the A bus or the B bus; a value known at once (open
-- bus where DMA can't reach).
data AReq
  = RqNone
  | RqReadA (Unsigned 24)
  | RqWriteA (Unsigned 24) (Unsigned 8)
  | RqReadB (Unsigned 8)
  | RqWriteB (Unsigned 8) (Unsigned 8)
  | RqNow (Unsigned 8)
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFDataX, BitPack)

-- | What the access unit drives.
data Acc = Acc
  { aPpu  :: Maybe Access
  , aWram :: Maybe (Unsigned 17, Maybe (Unsigned 8))
  , aSram :: Maybe (Unsigned 15, Maybe (Unsigned 8))
  , aRom  :: Maybe (Unsigned 23)
  , aApu  :: Maybe (Unsigned 2, Maybe (Unsigned 8))
  , aDmaRd :: Maybe (Index 8)
  , aDmaW :: Maybe (Index 8, Index 16, Unsigned 8)
  , aOvl  :: Maybe (Unsigned 16)  -- ^ a read of the save state's overlay (bank 00h's address)
#ifdef SNES_DSP1
  , aDsp  :: Maybe SnesAcc        -- ^ the DSP-1's DR or SR
#endif
  }

noAcc :: Acc
#ifdef SNES_DSP1
noAcc = Acc Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
#else
noAcc = Acc Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
#endif

-- | An address the save state's overlay answers while armed: bank 00h,
-- 0000h-1FFFh and 8000h-FFFFh.
ovlAddr :: Unsigned 24 -> Bool
ovlAddr a =
  let off = resize a :: Unsigned 16
  in a `shiftR` 16 == 0 && (off < 0x2000 || off >= 0x8000)

-- | An A-bus address of the sound module's ports (2140h-217Fh in a system
-- bank): its access waits until the module has caught up.
apuAddr :: Unsigned 24 -> Bool
apuAddr a =
  let bank = resize (a `shiftR` 16) :: Unsigned 8
      off = resize a :: Unsigned 16
  in (bank .&. 0x7F) < 0x40 && off >= 0x2140 && off < 0x2180

apuReg :: Unsigned 8 -> Bool
apuReg r = r >= 0x40 && r < 0x80

#ifdef SNES_DSP1
-- | (The DSP-1 build: "Snes.Spec.Map.dspWindow") the chip's DR (Just
-- False) or SR (Just True) at an address of the cartridge's.
dspWin :: CartCfg -> Unsigned 24 -> Maybe Bool
dspWin cfg a = case ccMap cfg of
  CmLoRom | b7 >= (if ccSram cfg > 0 then 0x20 else 0x30) && b7 < 0x40 && off >= 0x8000 -> Just (off >= 0xC000)
  CmHiRom | b7 < 0x20 && off >= 0x6000 && off < 0x8000 -> Just (off >= 0x7000)
  _ -> Nothing
 where
  b7 = resize (a `shiftR` 16) .&. 0x7F :: Unsigned 8
  off = resize a :: Unsigned 16

-- | Whether a request waits for the DSP-1 to catch up (exact, as the
-- spec's: its access sees every instruction due by its master clock).
waitsDsp :: CartCfg -> AReq -> Bool
waitsDsp cfg q = case q of
  RqReadA a -> isJust (dspWin cfg a)
  RqWriteA a _ -> isJust (dspWin cfg a)
  _ -> False
#endif

-- | Whether a request waits for the sound module.
waitsApu :: AReq -> Bool
waitsApu q = case q of
  RqReadA a -> apuAddr a
  RqWriteA a _ -> apuAddr a
  RqReadB r -> apuReg r
  RqWriteB r _ -> apuReg r
  _ -> False

-- | The access unit: the one address decode, the registers' reads and
-- writes. A read's value is there on the next clock ('rSrc').
perform :: CartCfg -> Vec 2 (Unsigned 12) -> Bool -> BusC -> AReq -> (BusC, Acc)
perform cfg pads ovlOn c q = case q of
  RqNone -> (c, noAcc)
  RqNow v -> (now c v, noAcc)
  RqReadB r -> readB c r
  RqWriteB r v -> writeB c r v
  -- (a save state's overlay, armed: bank 00h's 0000h-1FFFh and
  -- 8000h-FFFFh read from it, as "Snes.Spec.Machine"'s; its speed the
  -- address's as always)
  RqReadA a | ovlOn, ovlAddr a -> (from c SrcOvl, noAcc { aOvl = Just (resize a) })
#ifdef SNES_DSP1
  RqReadA a | Just sr <- dspWin cfg a -> (from c SrcDsp, noAcc { aDsp = Just (if sr then SReadSr else SReadDr) })
  RqWriteA a v | Just sr <- dspWin cfg a -> (c, noAcc { aDsp = if sr then Nothing else Just (SWriteDr v) })
#endif
  RqReadA a -> case decode cfg a of
    TWram o -> (from c SrcWram, noAcc { aWram = Just (o, Nothing) })
    TRom o -> (from c SrcRom, noAcc { aRom = Just o })
    TSram o -> (from c SrcSram, noAcc { aSram = Just (o, Nothing) })
    TBbus r -> readB c r
    TJoy hi -> let (c', v) = joyRead pads c hi in (now c' v, noAcc)
    TCpuReg r
      | r >= 0x4300 && r < 0x4380 ->
          (from c (SrcDma (fromIntegral (r .&. 0xF))), noAcc { aDmaRd = Just (fromIntegral ((r - 0x4300) `shiftR` 4)) })
      | otherwise -> let (c', v) = regRead c r in (now c' v, noAcc)
    TOpen -> (now c (bMdr c), noAcc)
  RqWriteA a v -> case decode cfg a of
    TWram o -> (c, noAcc { aWram = Just (o, Just v) })
    TRom _ -> (c, noAcc)
    TSram o -> (c, noAcc { aSram = Just (o, Just v) })
    TBbus r -> writeB c r v
    TJoy hi -> (joyWrite pads c hi v, noAcc)
    TCpuReg r
      | r >= 0x4300 && r < 0x4380 ->
          (c, noAcc { aDmaW = Just (fromIntegral ((r - 0x4300) `shiftR` 4), fromIntegral (r .&. 0xF), v) })
      | otherwise -> regWrite c r v
    TOpen -> (c, noAcc)

-- | A read on the B bus (2100h + r).
readB :: BusC -> Unsigned 8 -> (BusC, Acc)
readB c r
  | r < 0x40 = (from c SrcPpu, noAcc { aPpu = Just (ARead (resize r) (bMdr c)) })
  | r < 0x80 = (from c SrcApu, noAcc { aApu = Just (resize r, Nothing) })
  | r == 0x80 = (from c { bWmadd = bWmadd c + 1 } SrcWram, noAcc { aWram = Just (bWmadd c, Nothing) })
  | otherwise = (now c (bMdr c), noAcc)

writeB :: BusC -> Unsigned 8 -> Unsigned 8 -> (BusC, Acc)
writeB c r v
  | r < 0x40 = (if r == 0x33 then c { bSetini = v } else c, noAcc { aPpu = Just (AWrite (resize r) v) })
  | r < 0x80 = (c, noAcc { aApu = Just (resize r, Just v) })
  | r == 0x80 = (c { bWmadd = bWmadd c + 1 }, noAcc { aWram = Just (bWmadd c, Just v) })
  | r == 0x81 = (c { bWmadd = (bWmadd c .&. 0x1FF00) .|. resize v }, noAcc)
  | r == 0x82 = (c { bWmadd = (bWmadd c .&. 0x100FF) .|. (resize v `shiftL` 8) }, noAcc)
  | r == 0x83 = (c { bWmadd = (bWmadd c .&. 0x0FFFF) .|. (resize (v .&. 1) `shiftL` 16) }, noAcc)
  | otherwise = (c, noAcc)

from :: BusC -> Src -> BusC
from c s = c { rSrc = s, rReady = False, rMdr = bMdr c }

now :: BusC -> Unsigned 8 -> BusC
now c v = c { rSrc = SrcNow, rVal = v, rReady = True }

joyRead :: Vec 2 (Unsigned 12) -> BusC -> Bool -> (BusC, Unsigned 8)
joyRead pads c hi =
  let slot = if hi then 1 else 0 :: Index 2
      sr = bSer c !! slot
      pad = pads !! slot
      b = if bStrobe c then (if testBit pad 11 then 1 else 0) else (if testBit sr 15 then 1 else 0) :: Unsigned 8
      c' = if bStrobe c then c else c { bSer = replace slot ((sr `shiftL` 1) .|. 1) (bSer c) }
      v = (bMdr c .&. (if hi then 0xE0 else 0xFC)) .|. (if hi then 0x1C else 0) .|. b
  in (c', v)

joyWrite :: Vec 2 (Unsigned 12) -> BusC -> Bool -> Unsigned 8 -> BusC
joyWrite pads c hi v
  | hi = c
  | otherwise =
      let st' = testBit v 0
          c' = if bStrobe c && not st' then c { bSer = map padWord pads } else c
      in c' { bStrobe = st' }

regRead :: BusC -> Unsigned 16 -> (BusC, Unsigned 8)
regRead c r
  | r == 0x4210 = (c { bRdnmi = False }, (if bRdnmi c then 0x80 else 0) .|. (open .&. 0x70) .|. 0x02)
  | r == 0x4211 = (c { bTimeup = False }, (if bTimeup c then 0x80 else 0) .|. (open .&. 0x7F))
  | r == 0x4212 =
      let vb = bVc c >= vblankStart c
          hb = bHc c < 4 || bHc c >= 1096
      in (c, (if vb then 0x80 else 0) .|. (if hb then 0x40 else 0) .|. (open .&. 0x3E) .|. (if bJoyBusy c > 0 then 1 else 0))
  | r == 0x4213 = (c, bWrio c)
  | r == 0x4214 = (c, resize (bRddiv c))
  | r == 0x4215 = (c, resize (bRddiv c `shiftR` 8))
  | r == 0x4216 = (c, resize (bRdmpy c))
  | r == 0x4217 = (c, resize (bRdmpy c `shiftR` 8))
  | r >= 0x4218 && r <= 0x421F =
      let j = bJoy c !! ((r - 0x4218) `shiftR` 1)
      in (c, if even r then resize j else resize (j `shiftR` 8))
  | otherwise = (c, open)
 where
  open = bMdr c

regWrite :: BusC -> Unsigned 16 -> Unsigned 8 -> (BusC, Acc)
regWrite c r v = case r of
  0x4200 -> (c { bNmitimen = v, bTimeup = (v .&. 0x30 /= 0) && bTimeup c }, noAcc)
  0x4201 ->
    let latch = testBit (bWrio c) 7 && not (testBit v 7)
    in (c { bWrio = v }, if latch then noAcc { aPpu = Just ALatch } else noAcc)
  0x4202 -> (c { bMpya = v }, noAcc)
  0x4203 ->
    let c1 = c { bRdmpy = 0 }
    in if busy then (c1, noAcc)
       else (c1 { bRddiv = (resize v `shiftL` 8) .|. resize (bMpya c), bMpyCtr = 8, bShift = resize v }, noAcc)
  0x4204 -> (c { bDiv = (bDiv c .&. 0xFF00) .|. resize v }, noAcc)
  0x4205 -> (c { bDiv = (bDiv c .&. 0x00FF) .|. (resize v `shiftL` 8) }, noAcc)
  0x4206 ->
    let c1 = c { bRdmpy = bDiv c }
    in if busy then (c1, noAcc) else (c1 { bDivCtr = 16, bShift = resize v `shiftL` 16 }, noAcc)
  0x4207 -> (c { bHtime = (bHtime c .&. 0x100) .|. resize v }, noAcc)
  0x4208 -> (c { bHtime = (bHtime c .&. 0xFF) .|. (resize (v .&. 1) `shiftL` 8) }, noAcc)
  0x4209 -> (c { bVtime = (bVtime c .&. 0x100) .|. resize v }, noAcc)
  0x420A -> (c { bVtime = (bVtime c .&. 0xFF) .|. (resize (v .&. 1) `shiftL` 8) }, noAcc)
  0x420B -> (c { bDmaPend = v }, noAcc)
  0x420C -> (c { bHdmaen = v }, noAcc)
  0x420D -> (c { bMemsel = testBit v 0 }, noAcc)
  _ -> (c, noAcc)
 where
  busy = bMpyCtr c > 0 || bDivCtr c > 0

-- | A DMA or HDMA byte: to the A bus?, the A-bus address, the B-bus
-- register; whether the A side is one DMA can't reach (fullsnes).
data Xfer = Xfer
  { xToA  :: Bool
  , xA    :: Unsigned 24
  , xB    :: Unsigned 8
  , xForb :: Bool
  }

xfer :: Bool -> Unsigned 24 -> Unsigned 8 -> Xfer
xfer toA a b =
  let off = resize a :: Unsigned 16
      bank = resize (a `shiftR` 16) :: Unsigned 8
      system = (bank .&. 0x7F) < 0x40
      forb = system && ((off >= 0x2100 && off < 0x2200) || (off >= 0x4300 && off < 0x4380))
  in Xfer toA a b forb

-- | A byte's first access (the read), and its second (the write of the
-- value read).
xferRead :: BusC -> Xfer -> AReq
xferRead c x
  | xToA x = RqReadB (xB x)
  | xForb x = RqNow (bMdr c)
  | otherwise = RqReadA (xA x)

xferWrite :: Xfer -> Unsigned 8 -> AReq
xferWrite x v
  | xToA x = if xForb x then RqNone else RqWriteA (xA x) v
  | otherwise = RqWriteB (xB x) v

-- | The DMA channel's byte now.
dmaXfer :: BusC -> Xfer
dmaXfer c =
  let w = bW c
  in xfer (testBit (chDmap w) 7) ((resize (chBank w) `shiftL` 16) .|. resize (chA1t w)) (chBbad w + patOff (chDmap w) (bK c))

-- | The HDMA channel's byte now.
hdmaXfer :: BusC -> Xfer
hdmaXfer c =
  let w = fW c
      a = if testBit (chDmap w) 6
            then (resize (chDasb w) `shiftL` 16) .|. resize (chDas w)
            else (resize (chBank w) `shiftL` 16) .|. resize (chA2a w)
  in xfer (testBit (chDmap w) 7) a (chBbad w + patOff (chDmap w) (fOff c))

-- | HDMA's table address now (its bank, A2A).
tableAddr :: BusC -> Unsigned 24
tableAddr c = (resize (chBank (fW c)) `shiftL` 16) .|. resize (chA2a (fW c))

-- | The lowest channel of a set.
lowest :: Unsigned 8 -> Index 8
lowest m = fromIntegral (countTrailingZeros m)

-- * The clock

-- | What a zero-time step does: the state, the access it asks for, a
-- channel's registers to load (DMA's or HDMA's), the registers written
-- back, the event for the tests, and whether the CPU's cycle ended (its
-- read's value).
data Step = Step
  { sC    :: BusC
  , sReq  :: AReq
  , sCpuRd :: Bool
  , sLoad :: Maybe (Index 8)
  , sWb   :: Maybe (Index 8, Vec 16 (Maybe (Unsigned 8)))
  , sEv   :: Maybe (Unsigned 32, BusEv)
  , sEnd  :: Maybe (Unsigned 8)
  }

stay :: BusC -> Step
stay c = Step c RqNone False Nothing Nothing Nothing Nothing

busClock :: BusC -> BusIn -> (BusC, BusOut)
busClock c0 i = (cZ, out)
 where
  cfg = biCfg i
  pads = biPads i
  -- last clock's read lands (the CPU's in its own latch too); a channel's
  -- registers asked for last clock arrive
  c1a = case rSrc c0 of
    SrcNone -> c0
    SrcNow -> c0
    SrcWram -> c0 { rVal = biWram i, rReady = True, rSrc = SrcNone }
    SrcSram -> c0 { rVal = biSram i, rReady = True, rSrc = SrcNone }
    SrcApu -> c0 { rVal = biApuRead i, rReady = True, rSrc = SrcNone }
    SrcPpu -> c0 { rVal = maybe (rMdr c0) id (maybe Nothing id (biPpuRead i)), rReady = True, rSrc = SrcNone }
    SrcDma k -> c0 { rVal = biDma i !! k, rReady = True, rSrc = SrcNone }
    SrcOvl -> c0 { rVal = biOvl i, rReady = True, rSrc = SrcNone }
#ifdef SNES_DSP1
    SrcDsp -> c0 { rVal = biDspRead i, rReady = True, rSrc = SrcNone }
#endif
    SrcRom -> case biRom i of
      Just v -> c0 { rVal = v, rReady = True, rSrc = SrcNone }
      Nothing -> c0
  c1b = if rCpu c1a && rReady c1a then c1a { bCpuVal = rVal c1a, bCpuOk = True, rCpu = False } else c1a
  c1c = if bLoad c1b then c1b { bW = fromRegs (biDma i), bLoad = False } else c1b
  c1 = if fLoad c1c then c1c { fW = fromRegs (biDma i), fLoad = False } else c1c
  -- the tick, if the owner's next step is one
  wantTick = case fStep c1 of
    FgNone -> bgWantsTick c1
    _ -> fgWantsTick c1
  -- HDMA (the foreground) waits while a ROM read is still on its way
  -- (from DDR, "Snes.RomPort"): no master clock passes and its step waits
  -- until the byte lands. Otherwise the CPU's read, issued 4 ticks before
  -- its cycle's end (or DMA's), could still be out when HDMA started in
  -- those ticks: HDMA's read took the source and the ROM port's request,
  -- and the CPU got HDMA's byte, or 0 (the freeze hunt, 2026-10-21:
  -- Rockman & Forte ran into a BRK; snes-core with a slow DDR diverged on
  -- Mode7HDMA and RedSpaceIndirectHDMA). A hold passes no master clock, so
  -- the spec's timeline is kept; only the foreground's, as the CPU's own
  -- ROM reads (every opcode from ROM) must not hold the machine: that
  -- cost the display its pace.
  romWait c = fStep c /= FgNone && case rSrc c of
    SrcRom -> True
    _ -> False
  ticked = biRun i && wantTick && not (romWait c1)
  (c2, lineEnd) = if ticked then tick pads c1 else (c1, False)
  -- the owner's zero-time step: what it asks for waits for the sound
  -- module when it is its port
  apuOk = if ticked then biApuReady1 i else biApuReady0 i
  st0 = case fStep c2 of
    FgNone -> bgStep c2
    _ -> fgStep c2
#ifdef SNES_DSP1
  dspOk = if ticked then biDspReady1 i else biDspReady0 i
  st = if romWait c2 || (waitsApu (sReq st0) && not apuOk) || (waitsDsp cfg (sReq st0) && not dspOk) then stay c2 else st0
#else
  st = if romWait c2 || (waitsApu (sReq st0) && not apuOk) then stay c2 else st0
#endif
  -- the access unit
  (c3a, acc) = perform cfg pads (biOvlOn i) (sC st) (sReq st)
  c3b = if sCpuRd st then (if rReady c3a then c3a { bCpuVal = rVal c3a, bCpuOk = True } else c3a { rCpu = True, bCpuOk = False }) else c3a
  -- a channel load asked for
  c3c = case sLoad st of
    Just _ -> if fStep c3b == FgNone then c3b { bLoad = True } else c3b { fLoad = True }
    Nothing -> c3b
  -- the CPU's cycle end: the lines after the cycle's effects, then the
  -- next cycle, or DMA first (aligned to 8)
  (c3, endIn) = case sEnd st of
    Just d ->
      let inp = CpuIn d (nmiLine c3c) (irqLine c3c)
          c' = c3c { bTicks = 0, bAluDone = False, bReadDone = False }
          align = negate (resize (bMc c') :: Unsigned 3)
          c'' | bDmaPend c' == 0 = c'
              | align == 0 = c' { bStep = BgDStart, bLeft = 8, bChans = bDmaPend c', bDmaPend = 0 }
              | otherwise = c' { bStep = BgDAlign, bLeft = resize align, bChans = bDmaPend c', bDmaPend = 0 }
      in (c'', Just inp)
    Nothing -> (c3c, Nothing)
  -- the CPU core: its clock, with the cycle's end when there was one
  (cpu', stepK) = cpuClock coreW65c816 (bCpu c3) endIn
  cZ = c3 { bCpu = cpu' }
  -- the DMA registers' ports: one channel read (a step's load, or the
  -- access unit's), one channel written (the engine's write-back, or the
  -- access unit's)
  (wbCh, wbV) = case (sWb st, aDmaW acc) of
    (Just (ch, v), _) -> (ch, v)
    (Nothing, Just (ch, k, v)) -> (ch, replace k (Just v) (repeat Nothing))
    _ -> (0, repeat Nothing)
  out = BusOut
    { boTick = ticked
    , boPpu = aPpu acc
    , boWram = maybe 0 fst (aWram acc)
    , boWramW = maybe Nothing snd (aWram acc)
    , boSram = maybe 0 fst (aSram acc)
    , boSramW = maybe Nothing snd (aSram acc)
    , boRom = aRom acc
    , boApu = aApu acc
    , boDmaRd = maybe (maybe 0 id (aDmaRd acc)) id (sLoad st)
    , boDmaWCh = wbCh
    , boDmaW = wbV
    , boLineEnd = lineEnd
    , boEv = sEv st
    , boStep = stepK
    , boOvl = aOvl acc
#ifdef SNES_DSP1
    , boDsp = aDsp acc
#endif
    }

nmiLine, irqLine :: BusC -> Bool
nmiLine c = bRdnmi c && testBit (bNmitimen c) 7
irqLine = bTimeup

-- | The background's next step is a tick.
bgWantsTick :: BusC -> Bool
bgWantsTick c = case bStep c of
  BgCpu ->
    let cpu = bCpu c
        known = cStage cpu == StReady
        req = cReq cpu
        sp = cycleLen c
    in if not known then bTicks c < 2
       else case rqOp req of
         BRead _ -> if not (bReadDone c) then bTicks c < sp - 4 else bTicks c < sp
         _ -> bTicks c < sp
  BgDAlign -> bLeft c > 0
  BgDStart -> bLeft c > 0
  BgDChan -> bLeft c > 0
  BgDByteT -> bLeft c > 0
  _ -> False

fgWantsTick :: BusC -> Bool
fgWantsTick c = case fStep c of
  FgRefresh -> fLeft c > 0
  FgHStart -> fLeft c > 0
  FgHChan -> fLeft c > 0
  FgHByteT -> fLeft c > 0
  FgRelT -> fLeft c > 0
  _ -> False

-- | The CPU cycle's length (its request on the bus).
cycleLen :: BusC -> Unsigned 4
cycleLen c =
  let req = cReq (bCpu c)
  in case rqOp req of
       BRead _ -> speed (bMemsel c) (rqAddr req)
       BWrite _ -> speed (bMemsel c) (rqAddr req)
       _ -> 6

-- | A master clock: H, the line's events; the owner's tick counted.
tick :: Vec 2 (Unsigned 12) -> BusC -> (BusC, Bool)
tick pads c0 =
  let c1 = countTick c0
      hc' = bHc c1 + 1
      v = bVc c1
      vbs = vblankStart c1
      len = lineLength c1
      joyBusy = if bJoyBusy c1 > 0 then bJoyBusy c1 - 1 else 0
      c2 = c1 { bHc = hc', bMc = bMc c1 + 1, bJoyBusy = joyBusy }
  in if hc' == len
       then (newLine c2, True)
       else
         let c3 = if hc' == 2 && v == vbs then c2 { bRdnmi = True }
                  else if hc' == 2 && v == 0 then c2 { bRdnmi = False } else c2
             c4 = if hc' == 12 && v == 0 then hdmaInit c3 else c3
             c5 = if hc' == 130 && v == vbs then autoJoy pads c4 else c4
             c6 = if hc' == 538 then c5 { fStep = FgRefresh, fLeft = 40 } else c5
             c7 = if hc' == 1116 && v < vbs then hdmaRun c6 else c6
             irqHit = irqPosition c7 == Just hc' && (bNmitimen c7 .&. 0x20 == 0 || v == bVtime c7)
             c8 | irqHit && hc' == 538 = c7 { fIrqAfter = True }
                | irqHit = c7 { bTimeup = True }
                | otherwise = c7
         in (c8, False)

-- | The owner's tick counted (before the events, which may start the
-- foreground).
countTick :: BusC -> BusC
countTick c = case fStep c of
  FgNone -> case bStep c of
    BgCpu -> c { bTicks = bTicks c + 1 }
    _ -> c { bLeft = bLeft c - 1 }
  _ -> c { fLeft = fLeft c - 1 }

newLine :: BusC -> BusC
newLine c =
  let v' = bVc c + 1
  in if v' >= linesPerFrame c
       then c { bHc = 0, bVc = 0, bField = not (bField c) }
       else c { bHc = 0, bVc = v' }

autoJoy :: Vec 2 (Unsigned 12) -> BusC -> BusC
autoJoy pads c
  | testBit (bNmitimen c) 0 =
      c { bJoyBusy = 4224
        , bJoy = padWord (pads !! (0 :: Int)) :> padWord (pads !! (1 :: Int)) :> 0 :> 0 :> Nil
        , bSer = repeat 0xFFFF }
  | otherwise = c

-- | Line 0, H 12: HDMA's set-up.
hdmaInit :: BusC -> BusC
hdmaInit c0 =
  let c = c0 { bHdmaDo = 0, bHdmaDone = 0 }
  in if bHdmaen c /= 0 then c { fStep = FgHStart, fLeft = 18, fInit = True, fChans = bHdmaen c } else c

-- | H 1116 of a line before V-blank: HDMA's transfers.
hdmaRun :: BusC -> BusC
hdmaRun c =
  let active = bHdmaen c .&. complement (bHdmaDone c)
  in if active /= 0 then c { fStep = FgHStart, fLeft = 18, fInit = False, fChans = active } else c

-- * The background's zero-time step

-- | Registers written back: A1T (2, 3) and DAS (5, 6).
wbA1tDas :: Chan -> Vec 16 (Maybe (Unsigned 8))
wbA1tDas w =
  let lo, hi :: Unsigned 16 -> Maybe (Unsigned 8)
      lo x = Just (resize x)
      hi x = Just (resize (x `shiftR` 8))
  in replace (2 :: Int) (lo (chA1t w)) (replace (3 :: Int) (hi (chA1t w))
       (replace (5 :: Int) (lo (chDas w)) (replace (6 :: Int) (hi (chDas w)) (repeat Nothing))))

bgStep :: BusC -> Step
bgStep c0 = case bStep c0 of
  BgCpu -> cpuStep
  BgDAlign | bLeft c0 == 0 -> stay c0 { bStep = BgDStart, bLeft = 8 }
  BgDStart | bLeft c0 == 0 -> nextChan c0
  BgDChan | bLeft c0 == 0 -> byteA c0 { bStep = BgDByteA }
  BgDByteA -> byteA c0
  BgDByteB -> byteB
  BgDByteT | bLeft c0 == 0 ->
    if chDas (bW c0) == 0 then nextChan c0 else byteA c0 { bStep = BgDByteA }
  _ -> stay c0
 where
  -- the next channel (its registers loaded), or the CPU's cycle
  nextChan c
    | bChans c == 0 = stay c { bStep = BgCpu, bTicks = 0, bAluDone = False, bReadDone = False }
    | otherwise = let ch = lowest (bChans c)
                  in (stay c { bStep = BgDChan, bLeft = 8, bCh = ch, bChans = clearBit (bChans c) (fromEnum ch), bK = 0 })
                       { sLoad = Just ch }
  byteA c = (stay c { bStep = BgDByteB, rMc = bMc c }) { sReq = xferRead c (dmaXfer c) }
  byteB
    | not (rReady c0) = stay c0
    | otherwise =
        let x = dmaXfer c0
            v = rVal c0
            w = bW c0
            a = chA1t w
            a' | testBit (chDmap w) 3 = a
               | testBit (chDmap w) 4 = a - 1
               | otherwise = a + 1
            w' = w { chA1t = a', chDas = chDas w - 1 }
        in Step c0 { bStep = BgDByteT, bLeft = 8, bK = bK c0 + 1, bW = w' } (xferWrite x v) False Nothing
                (Just (bCh c0, wbA1tDas w')) (Just (rMc c0, EvDma (xA x) (xB x) v (xToA x))) Nothing
  -- the CPU's cycle
  cpu = bCpu c0
  known = cStage cpu == StReady
  req = cReq cpu
  sp = cycleLen c0
  isWrite = case rqOp req of
    BWrite _ -> True
    _ -> False
  -- a write's ALU step comes before its ticks
  cW = if known && isWrite && not (bAluDone c0) then (aluStep c0) { bAluDone = True } else c0
  cpuStep
    | not known = stay c0
    | otherwise = case rqOp req of
        BRead _
          | not (bReadDone cW) && bTicks cW == sp - 4 ->
              (stay cW { bReadDone = True, bReadMc = bMc cW, rAddr = rqAddr req }) { sReq = RqReadA (rqAddr req), sCpuRd = True }
          | bReadDone cW && bTicks cW == sp && bCpuOk cW ->
              let v = bCpuVal cW
                  -- (the trace's: the opcode fetches, the first BRK or COP)
                  opFetch = rqOp req == BRead KOpcode
                  brk = case bBrk cW of
                    Nothing | opFetch && (v == 0x00 || v == 0x02) -> Just (bLastOp cW, rqAddr req, v)
                    b -> b
                  lastOp = if opFetch then rqAddr req else bLastOp cW
              in (stay (aluStep cW) { bMdr = v, bCpuOk = False, bLastOp = lastOp, bBrk = brk })
                   { sEv = Just (bReadMc cW, EvRead (rqAddr req) v), sEnd = Just v }
          | otherwise -> stay cW
        BWrite _
          | bTicks cW == sp ->
              let v = rqData req
              in (stay cW { bMdr = v }) { sReq = RqWriteA (rqAddr req) v, sEv = Just (bMc cW, EvWrite (rqAddr req) v), sEnd = Just 0 }
          | otherwise -> stay cW
        _ | bTicks cW == 6 -> (stay (aluStep cW)) { sEv = Just (bMc cW, EvIdle), sEnd = Just 0 }
          | otherwise -> stay cW

-- * The foreground's zero-time step

fgStep :: BusC -> Step
fgStep c0 = case fStep c0 of
  FgRefresh | fLeft c0 == 0 ->
    stay c0 { fStep = FgNone, fIrqAfter = False, bTimeup = bTimeup c0 || fIrqAfter c0 }
  FgHStart | fLeft c0 == 0 -> nextChan c0
  FgHChan | fLeft c0 == 0 ->
    if fInit c0
      then rel1 c0 { fW = (fW c0) { chA2a = chA1t (fW c0) } }
      else if testBit (bHdmaDo c0) (fromEnum ch) then byteA c0 { fOff = 0 } else count c0
  FgHByteA -> byteA c0
  FgHByteB -> byteB
  FgHByteT | fLeft c0 == 0 ->
    if resize (fOff c0) + 1 < patLen (chDmap (fW c0)) then byteA c0 { fOff = fOff c0 + 1 } else count c0
  FgRel1 -> rel1 c0
  FgRel2 | rReady c0 ->
    let n = rVal c0
        ev = Just (rMc c0, EvTable (rAddr c0) n)
        w' = (fW c0) { chNtrl = n }
        c1 = c0 { fW = w' }
        wb = replace (10 :: Int) (Just n) (repeat Nothing)
    in if n == 0 then (withWb (nextChan c1 { bHdmaDone = setBit (bHdmaDone c1) (fromEnum ch) }) wb) { sEv = ev }
       else if testBit (chDmap w') 6
         then let st = tableRead c1 { fStep = FgRel3 } in (withWb st (mergeWb wb (wbA2a (fW (sC st))))) { sEv = ev }
         else (withWb (nextChan (doSet c1)) wb) { sEv = ev }
  FgRel3 | rReady c0 ->
    let lo = rVal c0
        ev = Just (rMc c0, EvTable (rAddr c0) lo)
        w' = (fW c0) { chDas = (chDas (fW c0) .&. 0xFF00) .|. resize lo }
        st = tableRead c0 { fW = w', fStep = FgRel4 }
    in (withWb st (replace (5 :: Int) (Just lo) (wbA2a (fW (sC st))))) { sEv = ev }
  FgRel4 | rReady c0 ->
    let hi = rVal c0
        ev = Just (rMc c0, EvTable (rAddr c0) hi)
        w' = (fW c0) { chDas = (chDas (fW c0) .&. 0x00FF) .|. (resize hi `shiftL` 8) }
    in (withWb (stay c0 { fW = w', fStep = FgRelT, fLeft = 16 }) (replace (6 :: Int) (Just hi) (repeat Nothing))) { sEv = ev }
  FgRelT | fLeft c0 == 0 -> nextChan (doSet c0)
  _ -> stay c0
 where
  ch = fCh c0
  nextChan c
    | fChans c == 0 = stay c { fStep = FgNone }
    | otherwise = let k = lowest (fChans c)
                  in (stay c { fStep = FgHChan, fLeft = 8, fCh = k, fChans = clearBit (fChans c) (fromEnum k) })
                       { sLoad = Just k }
  doSet c = c { bHdmaDo = setBit (bHdmaDo c) (fromEnum (fCh c)) }
  -- the registers written back are the channel's in work (fCh c0), even
  -- when the step goes on to the next channel
  withWb st v = st { sWb = Just (ch, v) }
  mergeWb a b = zipWith (\x y -> maybe y Just x) a b
  wbA2a w = replace (8 :: Int) (Just (resize (chA2a w))) (replace (9 :: Int) (Just (resize (chA2a w `shiftR` 8))) (repeat Nothing))
  -- a table byte: at A2A, which counts on
  tableRead c =
    let a = tableAddr c
        w' = (fW c) { chA2a = chA2a (fW c) + 1 }
    in (stay c { fW = w', rMc = bMc c, rAddr = a }) { sReq = RqReadA a }
  rel1 c = let st = tableRead c { fStep = FgRel2 } in withWb st (wbA2a (fW (sC st)))
  -- the line counter: down by one; the repeat bit; the next entry at 0
  count c =
    let n' = chNtrl (fW c) - 1
        k = fromEnum (fCh c)
        c1 = c { fW = (fW c) { chNtrl = n' }
               , bHdmaDo = if testBit n' 7 then setBit (bHdmaDo c) k else clearBit (bHdmaDo c) k }
        wb = replace (10 :: Int) (Just n') (repeat Nothing)
    in if n' .&. 0x7F == 0 then (let st = rel1 c1 in st { sWb = fmap (\(cc, v) -> (cc, mergeWb wb v)) (sWb st) })
       else withWb (nextChan c1) wb
  byteA c = (stay c { fStep = FgHByteB, rMc = bMc c }) { sReq = xferRead c (hdmaXfer c) }
  byteB
    | not (rReady c0) = stay c0
    | otherwise =
        let x = hdmaXfer c0
            v = rVal c0
            w = fW c0
            a' = resize (xA x) + 1 :: Unsigned 16
            (w', wb) = if testBit (chDmap w) 6
              then (w { chDas = a' }, replace (5 :: Int) (Just (resize a')) (replace (6 :: Int) (Just (resize (a' `shiftR` 8))) (repeat Nothing)))
              else (w { chA2a = a' }, wbA2a w { chA2a = a' })
        in Step c0 { fStep = FgHByteT, fLeft = 8, fW = w' } (xferWrite x v) False Nothing
                (Just (ch, wb)) (Just (rMc c0, EvDma (xA x) (xB x) v (xToA x))) Nothing
