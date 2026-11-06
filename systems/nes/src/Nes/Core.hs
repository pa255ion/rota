{-# LANGUAGE StrictData #-}
{-|
The whole NES in Clash (PLAN.md step 3.6): the 6502 ("Mos6502.Core"),
the PPU ("Nes.Ppu"), the APU's frame counter and length counters
("Nes.Apu"), the cartridge boards ("Nes.Mapper"), OAM DMA, controllers,
open bus and the memory map. It reproduces the reference model
("Nes.Spec.Machine") CPU cycle for CPU cycle and dot for dot.

__Structure.__ 'coreStep' is a pure function run once per clock. Every
memory is outside it: requests go out in 'CoreOut', the bytes come back
in the next clock's 'CoreIn' and stay valid until the next request on the
same port. So the same function runs in the Haskell lockstep harness
(memories in vectors) and in hardware (block RAMs, step 3.8).

__A CPU cycle__ is a sequence of clocks ('Seq'), in the spec's order
(alignment 1: two dots before the bus access, one after): dot, dot, the
access (the APU and the mapper's write tracking advance first; DMA takes
the cycle when RDY is low), the read's data clock, dot, then the CPU's
'cpuCycle' with that cycle's data and the NMI and IRQ lines. The
core keeps real time with a credit of dots ('kiDot' adds one, at most 63
are stored): a CPU cycle starts when three are there and the last cycle
has finished; it takes 7 or 8 clocks, three dots about 14.

__Waiting for PRG ROM__ (side track D, for Kirby's Adventure: PRG in
DDR): a memory read's byte comes with 'kiRdOk'; until then the read's
data clock waits, the whole machine with it (nothing else moves), while
the dots of time pile up in the credit. So the emulation is the same
cycle for cycle and dot for dot whatever the latency; the picture and the
sound only fall behind real time until the credit is spent (a CPU cycle
in 7 or 8 clocks against 14).

__Memory ports:__

* CPU side ('koMem'): PRG ROM (up to 512 KiB; in DDR, 'kiRdOk'), PRG RAM
  (8 KiB), CPU RAM (2 KiB). One access per CPU cycle.
* PPU port A ('koVramA', rendering fetches) and port B ('koVramB', $2007):
  CHR (ROM up to 256 KiB, or 8 KiB RAM) and the 2 KiB nametable RAM,
  already mapped by the cartridge board ('VramAddr').
* OAM ('koOamAddr', 'koOamWrite', 'koOamRow'): 256 bytes; 'kiOam' is the byte at the
  address of the previous clock.
-}
module Nes.Core
  ( -- * Memories
    Space (..)
  , MemReq (..)
  , VramAddr (..)
    -- * The core
  , CoreIn (..)
  , CoreOut (..)
  , CycleEvent (..)
  , Seq (..)
  , CoreState (..)
  , coreInit
  , coreStep
    -- * In hardware
  , prgRequest
  , PrgWait (..)
  , prgDdr
  , nesCore
    -- * With save states (side track N)
  , StMem (..)
  , StatePort
  , nesCoreWith
  ) where

import Clash.Prelude
import Data.Maybe (isJust)

import Mos6502.Core
import Nes.Apu
import Nes.Mapper
import Nes.Ppu
import Platform.Mem.Axi (AxiRdM2S, AxiRdS2M)
import Platform.Mem.Ddr (DdrAddr, ddrByte, ddrConfigDefault, ddrReadPort)

-- | The CPU side's memories; 'SpOvl' the ROM overlay of a state's load
-- (side track N: 2 KiB, while 'kiOvlOn').
data Space = SpPrg | SpPrgRam | SpRam | SpExram | SpOvl
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFDataX

data MemReq = MemReq
  { mqSpace :: Space
  , mqAddr  :: Unsigned 19
  , mqWrite :: Maybe (Unsigned 8)
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFDataX

-- | A PPU memory address after the cartridge board: a CHR byte, a
-- nametable RAM byte, an MMC5 ExRAM byte, or a byte the board makes itself
-- (MMC5's fill mode and extended attributes; it comes back as the others).
data VramAddr = VChr (Unsigned 18) | VNt (Unsigned 11) | VExram (Unsigned 10) | VConst (Unsigned 8)
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFDataX

data CoreIn = CoreIn
  { kiDot   :: Bool        -- ^ one PPU dot of time has elapsed
  , kiRd    :: Unsigned 8  -- ^ CPU-side memory: the byte of the last read
  , kiRdOk  :: Bool        -- ^ ... is there (False: still on its way from DDR)
  , kiVramA :: Unsigned 8  -- ^ PPU port A: the byte of the last read
  , kiVramB :: Unsigned 8  -- ^ PPU port B: the byte of the last read
  , kiOam   :: Unsigned 8  -- ^ OAM at the address of the previous clock
  , kiExLatch :: Unsigned 8 -- ^ MMC5: ExRAM at the last 'koExLatch' (the clock before)
  , kiPad1  :: Unsigned 8  -- ^ controller 1: bit 0 A, 1 B, 2 Select, 3 Start, 4 Up, 5 Down, 6 Left, 7 Right
  , kiPad2  :: Unsigned 8
  , kiOvlOn :: Bool        -- ^ side track N: the ROM overlay answers reads of $0000-$1FFF and $6000-$FFFF ('SpOvl')
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFDataX

-- | One CPU cycle's bus access (DMA cycles show the DMA's), and whether it
-- ended a step of the CPU; for the lockstep tests.
data CycleEvent = CycleEvent
  { ceAddr  :: Unsigned 16
  , ceData  :: Unsigned 8
  , ceWrite :: Bool
  , ceStep  :: Maybe StepKind
  , ceRegs  :: CpuRegs
  , ceApu   :: Channels         -- ^ the APU's outputs after the cycle
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFDataX

data CoreOut = CoreOut
  { koMem      :: Maybe MemReq
  , koVramA    :: Maybe VramAddr
  , koVramB    :: Maybe (VramAddr, Maybe (Unsigned 8))
  , koOamAddr  :: Unsigned 8
  , koOamWrite :: Maybe (Unsigned 8, Unsigned 8)
  , koOamRow   :: Maybe (Unsigned 5, Vec 8 (Unsigned 8)) -- ^ OAM corruption's row copy ('Nes.Ppu.oamRam')
  , koExLatch  :: Maybe (Unsigned 10) -- ^ MMC5: the ExRAM byte extended attributes want ('kiExLatch')
  , koPixel    :: Maybe (Unsigned 8, Unsigned 8, Unsigned 9)
  , koFrame    :: Bool
  , koEvent    :: Maybe CycleEvent
  , koIdle     :: Bool          -- ^ waiting for dot credit
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFDataX

-- | Which memory answers a PPU port ('nesCore').
data Src = SrcChr | SrcNt | SrcEx | SrcConst (Unsigned 8)
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFDataX

data Seq = QIdle | QDot1 | QDot2 | QAcc | QAccRd | QDot3 | QCpu
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFDataX

-- | What a DMA get also reads while the CPU is halted at $4000-$401F
-- ('getAt'): nothing, the status ($4015, its value, already read), or a
-- controller ($4016: True, $4017: False).
data Conflict = NoConflict | ConfStatus (Unsigned 8) | ConfJoy Bool
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFDataX

data CoreState = CoreState
  { ksCfg    :: CartCfg
  , ksCpu    :: Cpu
  , ksPpu    :: Ppu
  , ksApu    :: Apu
  , ksMap    :: MapState
  , ksSeq    :: Seq
  , ksBus    :: Unsigned 8      -- ^ open bus
  , ksOdd    :: Bool            -- ^ CPU cycles since power-on, this one included, are odd
  , ksHalted :: Bool            -- ^ the DMA unit holds the CPU (as 'Nes.Spec.Machine')
  , ksOamOn  :: Bool            -- ^ OAM DMA wanted or running
  , ksOamCount :: Unsigned 9    -- ^ OAM DMA accesses done (even: a get next)
  , ksDmcSt  :: Unsigned 2      -- ^ DMC DMA: 0 none, 1 needs its halt, 2 its dummy, 3 ready
  , ksDmaPage :: Unsigned 8
  , ksDmaVal :: Unsigned 8
  , ksStrobe :: Bool
  , ksShift1 :: Unsigned 8
  , ksShift2 :: Unsigned 8
  , ksData   :: Unsigned 8      -- ^ this cycle's read data for the CPU
  , ksStall  :: Bool            -- ^ this cycle is a DMA (stalled) cycle
  , ksAccess :: (Unsigned 16, Unsigned 8, Bool) -- ^ this cycle's access, for the event
  , ksMemRd  :: Bool            -- ^ the access was a memory read: take 'kiRd' next clock
  , ksDmaGet :: Bool            -- ^ this cycle is an OAM DMA get: its byte goes to 'ksDmaVal'
  , ksDmcGet :: Bool            -- ^ this cycle is the DMC's fetch: its byte goes to the APU
  , ksLastRd :: Maybe (Unsigned 16) -- ^ the address the previous cycle read (joypad /OE)
  , ksCurRd  :: Maybe (Unsigned 16)
  , ksJoyBit1 :: Bool           -- ^ the bit a run of consecutive reads of $4016 returns
  , ksJoyBit2 :: Bool
  , ksIntBus :: Unsigned 8      -- ^ the CPU's internal data bus (not the DMA's accesses)
  , ksDmaAcc :: Bool            -- ^ this cycle's access is the DMA's (get or put)
  , ksConflict :: Conflict      -- ^ this DMA get also reads a register: see 'getAt'
  , ksCredit :: Unsigned 6      -- ^ dots of time not used yet
  , ksStrobePend :: Maybe Bool  -- ^ the last $4016 write, for the controllers when a get cycle ends
  , ksStepped :: Bool           -- ^ the last CPU cycle ended a step of the CPU (side track N's rest)
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFDataX

-- | Power-on: the CPU starts its reset sequence, the PPU at (0, 0). The
-- very first CPU cycle has one dot before its access, not two: the PPU
-- clock starts one master tick behind (the spec's alignment), so the PPU
-- stays one dot behind three per cycle from then on.
coreInit :: CartCfg -> CoreState
coreInit cfg = CoreState
  { ksCfg = cfg
  , ksCpu = cpuReset (cpuPowerOn (CpuRegs 0 0 0 0 0x34 0))
  , ksPpu = ppuInit, ksApu = apuInit, ksMap = mapInit, ksSeq = QDot2, ksBus = 0, ksOdd = False
  , ksHalted = False, ksOamOn = False, ksOamCount = 0, ksDmcSt = 0, ksDmaPage = 0, ksDmaVal = 0, ksStrobe = False
  , ksShift1 = 0, ksShift2 = 0, ksData = 0, ksStall = False, ksAccess = (0, 0, False), ksMemRd = False
  , ksDmaGet = False, ksDmcGet = False, ksLastRd = Nothing, ksCurRd = Nothing
  , ksJoyBit1 = False, ksJoyBit2 = False, ksIntBus = 0, ksDmaAcc = False, ksConflict = NoConflict, ksCredit = 0
  , ksStrobePend = Nothing, ksStepped = False }

idleOut :: CoreState -> CoreOut
idleOut s = CoreOut Nothing Nothing Nothing (pOamAddr (ksPpu s)) Nothing Nothing Nothing Nothing False Nothing False

-- | A PPU tick in this clock, its memory requests mapped.
-- (OPAQUE, as 'access', 'readAt', 'cpuWrite' and the others: Clash
-- normalises each once and apart; side track D: with MMC5 the one big
-- 'coreStep' took Clash 20 minutes)
{-# OPAQUE ppuClock #-}
ppuClock :: CoreState -> Tick -> CoreIn -> (CoreState, PpuOut, CoreOut)
ppuClock s t inp = (s { ksPpu = p', ksMap = m3 }, po, out)
 where
  (p', po) = ppuTick (ksPpu s) t (PpuIn (kiVramA inp) (kiVramB inp) (kiOam inp))
  -- every address the PPU puts on its bus: the MMC3 watches A12
  cfg = ksCfg s
  watch m a = mapA12 cfg a m
  -- port A's fetches: MMC5 also sees what they are ('m5Fetch')
  m1 = maybe (ksMap s) (\a -> m5Fetch cfg (poFetch po) a (watch (ksMap s) a)) (poVramA po)
  m2 = maybe m1 (\(_, a, _) -> watch m1 a) (poVramB po)
  m3 = maybe m2 (watch m2) (poBus po)
  vmap k a
    | a < 0x2000 = VChr (chrAddrK cfg (ksMap s) k a)
    | otherwise = case ntTarget cfg (ksMap s) k a of
        NtCiram i -> VNt i
        NtExram i -> VExram i
        NtConst c -> VConst c
  chrWritable a = a >= 0x2000 || ccChrRam cfg
  out = CoreOut
    { koMem = Nothing
    , koVramA = vmap (Just (poFetch po)) <$> poVramA po
    , koVramB = case poVramB po of
        Just (True, a, d) | chrWritable a -> case vmap Nothing a of
                                               VConst _ -> Nothing
                                               tgt -> Just (tgt, Just d)
                          | otherwise -> Nothing
        Just (False, a, _) -> Just (vmap Nothing a, Nothing)
        Nothing -> Nothing
    , koOamAddr = poOamAddr po
    , koOamWrite = poOamWrite po
    , koOamRow = poOamRow po
    , koExLatch = case poVramA po of
        Just a | poFetch po == FNt && m5Ext cfg (ksMap s) -> Just (resize (a .&. 0x3FF))
        _ -> Nothing
    , koPixel = poPixel po
    , koFrame = poFrame po
    , koEvent = Nothing
    , koIdle = False }

coreStep :: CoreState -> CoreIn -> (CoreState, CoreOut)
coreStep s0 inp = case ksSeq s of
  QIdle
    | credit >= 3 -> (s { ksSeq = QDot1, ksCredit = credit - 3 }, (idleOut s) { koIdle = True })
    | otherwise -> (s, (idleOut s) { koIdle = True })
  QDot1 -> dot QDot2
  QDot2 -> dot QAcc
  QAcc -> access s inp
  QAccRd
    | not (kiRdOk inp) -> (s, idleOut s)
    | otherwise ->
    let (a, _, _) = ksAccess s
        (sj, d, b) = conflictRead s inp (kiRd inp)
    in (sj { ksSeq = QDot3, ksData = d, ksBus = b, ksAccess = (a, b, False)
          , ksDmaVal = if ksDmaGet s then d else ksDmaVal s
          , ksApu = if ksDmcGet s then apuDmcFill d (ksApu s) else ksApu s }, idleOut s)
  QDot3 -> dot QCpu
  QCpu ->
    let p = ksPpu s
        (a, dRaw, w) = ksAccess s
        -- $2002: the sprite flags as they are at the end of the cycle (M2
        -- falls), the VBlank flag from the access (as Nes.Spec.Machine)
        is2002 = not w && not (ksStall s) && a >= 0x2000 && a < 0x4000 && a .&. 7 == 2
        flags = (if pSpr0 p then 0x40 else 0) .|. (if pOvf p then 0x20 else 0) :: Unsigned 8
        patch x = if is2002 then (x .&. 0x9F) .|. flags else x
        d = patch dRaw
        cin = CpuIn (patch (ksData s)) (ksStall s) (ppuNmiOut p) (apuIrq (ksApu s) || mapIrq (ksMap s))
        (cpu', co) = cpuCycle coreRicoh2a03 (ksCpu s) cin
        ev = CycleEvent a d w (coStep co) (cpuRegs cpu') (apuChannels (ksApu s))
    in ( s { ksCpu = cpu', ksSeq = QIdle, ksStepped = isJust (coStep co), ksIntBus = if ksDmaAcc s then ksIntBus s else d
           , ksBus = if is2002 then patch (ksBus s) else ksBus s }
       , (idleOut s) { koEvent = Just ev } )
 where
  credit = if kiDot inp && ksCredit s0 < 63 then ksCredit s0 + 1 else ksCredit s0
  -- MMC5: the ExRAM byte asked for in the last clock ('koExLatch')
  m5x = msM5 (ksMap s0)
  map0 = if m5ExPend m5x then (ksMap s0) { msM5 = m5x { m5ExLatch = kiExLatch inp, m5ExPend = False } } else ksMap s0
  s = s0 { ksCredit = credit, ksMap = map0 }
  dot next = let (s', _, out) = ppuClock s TDot inp in (s' { ksSeq = next }, out)

-- | The bus access of this CPU cycle, or the DMA unit's (the same rules
-- as "Nes.Spec.Machine": a halt cycle, then per cycle the DMC's fetch on a
-- get once its halt and dummy cycles are done, else OAM DMA's get or put,
-- else the CPU's read again; RDY only stops read cycles).
{-# OPAQUE access #-}
access :: CoreState -> CoreIn -> (CoreState, CoreOut)
access s0 inp = result
 where
  odd' = not (ksOdd s0)
  apu1 = apuCycle odd' (ksApu s0)
  s1a = s0 { ksOdd = odd', ksApu = apu1, ksLastRd = ksCurRd s0, ksCurRd = Nothing
           , ksMap = mapCycle (ksCfg s0) (ksMap s0)
           , ksDmaGet = False, ksDmcGet = False, ksDmaAcc = False, ksConflict = NoConflict }
  -- the last cycle was a get: the controllers see the last $4016 write now
  -- (as the spec's end of a get cycle)
  s1 = case ksStrobePend s1a of
    Just b | not odd' ->
      let reload = ksStrobe s1a || b
      in s1a { ksStrobePend = Nothing, ksStrobe = b
             , ksShift1 = if reload then kiPad1 inp else ksShift1 s1a
             , ksShift2 = if reload then kiPad2 inp else ksShift2 s1a }
    _ -> s1a
  req = cpuRequest (ksCpu s1)
  cpuAddr = rqAddr req
  dmc0 = if ksDmcSt s1 == 0 && apuDmcWanted apu1 then 1 else ksDmcSt s1
  -- RDY is low (the DMA unit wants this cycle) and the CPU reads
  stall = not (rqWrite req) && (ksHalted s1 || ksOamOn s1 || dmc0 /= 0)
  cnt = ksOamCount s1
  -- a DMA cycle: it also counts as the DMC's halt, then its dummy
  -- an aborted DMC DMA: its halt cycle, then nothing (on a write cycle: nothing at all)
  aborting = dmc0 == 1 && apuDmcAborted apu1
  sD = s1 { ksStall = True, ksHalted = True
          , ksDmcSt = if aborting then 0 else if dmc0 == 1 || dmc0 == 2 then dmc0 + 1 else dmc0
          , ksApu = if aborting then apuDmcAbortDone apu1 else apu1 }
  oamAddr = resize (ksDmaPage s1) `shiftL` 8 .|. resize (cnt `shiftR` 1)
  result
    | rqWrite req = cpuWrite s1 { ksStall = False, ksApu = if aborting then apuDmcAbortDone apu1 else apu1 } inp cpuAddr (rqData req)
    | not stall = cpuRead s1 { ksStall = False } inp cpuAddr
    | not (ksHalted s1) = done (cpuRead sD inp cpuAddr)
    | odd' && dmc0 == 3 = done (getAt sD { ksDmcSt = 0, ksDmcGet = True } (apuDmcAddr apu1))
    | odd' && ksOamOn s1 && even cnt = done (getAt sD { ksDmaGet = True, ksOamCount = cnt + 1 } oamAddr)
    | not odd' && ksOamOn s1 && odd cnt =
        -- put: write $2004
        let v = ksDmaVal s1
            (s2, _, out) = ppuClock sD (TWrite 4 v) inp
            last' = cnt == 511
        in done ( s2 { ksBus = v, ksAccess = (0x2004, v, True), ksDmaAcc = True, ksSeq = QDot3, ksMemRd = False
                     , ksOamOn = not last', ksOamCount = if last' then 0 else cnt + 1 }
                , out )
    | otherwise = done (cpuRead sD inp cpuAddr)
  done (s, o) = (if not (ksOamOn s) && ksDmcSt s == 0 then s { ksHalted = False } else s, o)
  -- The registers $4000-$401F are active only while the CPU is halted
  -- reading there; then a DMA read elsewhere also reads register $4000 +
  -- (g & $1F) (as Nes.Spec.Machine.dmaRead).
  getAt s g =
    let inR x = x >= 0x4000 && x < 0x4020
        r = 0x4000 .|. (g .&. 0x1F)
        conf = inR cpuAddr && not (inR g)
        joyC = conf && (r == 0x4016 || r == 0x4017)
        (st, apuS) = apuStatus (ksApu s)
        s' = s { ksDmaAcc = True
               , ksApu = if conf && r == 0x4015 then apuS else ksApu s
               , ksConflict = if joyC then ConfJoy (r == 0x4016)
                              else if conf && r == 0x4015 then ConfStatus st else NoConflict }
        (s2, o) = readAt s' { ksCurRd = Just g } inp (inR cpuAddr) g
    in (if joyC then s2 { ksCurRd = Just r } else s2, o)

-- | A read cycle at an address: memory reads finish in the next clock.
{-# OPAQUE cpuRead #-}
cpuRead :: CoreState -> CoreIn -> Unsigned 16 -> (CoreState, CoreOut)
cpuRead s inp a = readAt s { ksCurRd = Just a } inp True a

-- | A read; @regs@: the registers $4015-$4017 answer (False: a DMA read
-- while the CPU is elsewhere sees open bus there, see 'getAt').
{-# OPAQUE readAt #-}
readAt :: CoreState -> CoreIn -> Bool -> Unsigned 16 -> (CoreState, CoreOut)
readAt s inp regs a
  -- a state's load (side track N): the ROM overlay answers, the CPU's and
  -- the DMA's reads alike, the cartridge seeing none of them (as
  -- Nes.Spec.Machine's)
  | kiOvlOn inp && (a < 0x2000 || a >= 0x6000) =
      ( s { ksSeq = QAccRd, ksAccess = (a, 0, False), ksMemRd = True, ksMap = (ksMap s) { msPrevWr = False } }
      , (idleOut s) { koMem = Just (MemReq SpOvl (resize (a .&. 0x7FF)) Nothing) } )
  | a < 0x2000 = mem SpRam (resize (a .&. 0x7FF))
  | a < 0x4000 =
      let (s', po, out) = ppuClock s (TRead (bitCoerce (resize a :: Unsigned 3))) inp
      in done s' (poRead po) out
  | not regs && a < 0x4020 = done s (ksBus s) (idleOut s)
  | a == 0x4015 =
      let (v, apu') = apuStatus (ksApu s)
          -- bit 5 floats: the CPU's internal bus
          v' = v .|. (ksIntBus s .&. 0x20)
      -- $4015 is inside the 2A03: it doesn't change the open bus.
      in (finish s { ksApu = apu' } v' False, idleOut s)
  | a == 0x4016 = joy True
  | a == 0x4017 = joy False
  | mmc5 && a >= 0x5C00 && a < 0x6000 =
      if m5ExMode (msM5 (ksMap s)) >= 2 then mem SpExram (resize (a .&. 0x3FF)) else done s (ksBus s) (idleOut s)
  | mmc5 && a >= 0x5000 && a < 0x6000 = case m5CpuRead a (ksMap s) of
      (m', Just v) -> done s { ksMap = m' } v (idleOut s)
      (_, Nothing) -> done s (ksBus s) (idleOut s)
  | a < 0x6000 = done s (ksBus s) (idleOut s)
  | mmc5 = case prgTarget (ksCfg s) (ksMap s) a of
      PrgRam i -> mem SpPrgRam (resize i)
      PrgRom r -> mem SpPrg r
  | a < 0x8000 = if ccPrgRam (ksCfg s) then mem SpPrgRam (resize (a .&. 0x1FFF)) else done s (ksBus s) (idleOut s)
  | otherwise = mem SpPrg (prgAddr (ksCfg s) (ksMap s) a)
 where
  mmc5 = ccMapper (ksCfg s) == MMmc5
  -- (MMC5: the NMI vector's read ends the frame)
  mem sp addr = ( s { ksSeq = QAccRd, ksAccess = (a, 0, False), ksMemRd = True, ksMap = (ksMap s) { msPrevWr = False } }
                    { ksMap = (m5Vector (ksCfg s) a (ksMap s)) { msPrevWr = False } }
                , (idleOut s) { koMem = Just (MemReq sp addr Nothing) } )
  done s' v out = (finish s' v True, out)
  finish s0' v0 latch =
    let (s', v, b) = conflictRead s0' inp v0
    in s' { ksSeq = QDot3, ksData = v, ksBus = if latch then b else ksBus s', ksAccess = (a, b, False)
          , ksMemRd = False, ksMap = (ksMap s') { msPrevWr = False }
          , ksDmaVal = if ksDmaGet s' then v else ksDmaVal s'
          , ksApu = if ksDmcGet s' then apuDmcFill v (ksApu s') else ksApu s' }
  joy one = let (s', v) = joyRead s inp one a (ksBus s) in done s' v (idleOut s)

-- | A read of $4016 (@) or $4017 with the data bus at  bit 0
-- from the controller, bits 5 to 7 open bus. While the strobe is high the
-- shift register follows the buttons; a run of reads on consecutive cycles
-- reads one bit and clocks the register once (/OE, as Nes.Spec.Machine).
{-# OPAQUE joyRead #-}
joyRead :: CoreState -> CoreIn -> Bool -> Unsigned 16 -> Unsigned 8 -> (CoreState, Unsigned 8)
joyRead s inp one a bus = (s', v)
 where
  sh = if one then ksShift1 s else ksShift2 s
  held = if one then ksJoyBit1 s else ksJoyBit2 s
  run = ksLastRd s == Just a
  b | ksStrobe s = testBit (if one then kiPad1 inp else kiPad2 inp) 0
    | run = held
    | otherwise = testBit sh 0
  v = (if b then 1 else 0) .|. (bus .&. 0xE0)
  sh' = (sh `shiftR` 1) .|. 0x80
  s' | ksStrobe s || run = s
     | one = s { ksShift1 = sh', ksJoyBit1 = b }
     | otherwise = s { ksShift2 = sh', ksJoyBit2 = b }

-- | A DMA get that also reads a register ('ksConflict'), given the byte
-- read: the value the reader gets and the one left on the bus. $4015: the
-- status, bit 5 from the byte, which stays on the bus; $4016/$4017: the
-- controller's bit over the byte, on the bus too.
{-# OPAQUE conflictRead #-}
conflictRead :: CoreState -> CoreIn -> Unsigned 8 -> (CoreState, Unsigned 8, Unsigned 8)
conflictRead s inp d = case ksConflict s of
  NoConflict -> (s, d, d)
  ConfStatus st -> (s { ksConflict = NoConflict }, st .|. (d .&. 0x20), d)
  ConfJoy one -> let (s', v) = joyRead s inp one (if one then 0x4016 else 0x4017) d in (s' { ksConflict = NoConflict }, v, v)

-- | A write cycle.
{-# OPAQUE cpuWrite #-}
cpuWrite :: CoreState -> CoreIn -> Unsigned 16 -> Unsigned 8 -> (CoreState, CoreOut)
cpuWrite s0 inp a v
  | a < 0x2000 = memW SpRam (resize (a .&. 0x7FF))
  | a < 0x4000 =
      -- (MMC5 snoops $2000 and $2001)
      let (s', _, out) = ppuClock s { ksMap = m5PpuReg (ksCfg s) a v (ksMap s) } (TWrite (bitCoerce (resize a :: Unsigned 3)) v) inp
      in (fin s' False, out)
  | a == 0x4014 = (fin s { ksDmaPage = v, ksOamCount = 0, ksOamOn = True } False, idleOut s)
  | a == 0x4016 =
      -- The controllers see it when a get cycle ends ('access'); the shift
      -- registers hold the buttons of the moment the strobe falls.
      (fin s { ksStrobePend = Just (testBit v 0) } False, idleOut s)
  | a < 0x4018 = (fin s { ksApu = apuWrite (ksOdd s) a v (ksApu s) } False, idleOut s)
  | mmc5 && a >= 0x5C00 && a < 0x6000 = case m5ExramWrite (ksMap s) v of
      Just b -> memWB SpExram (resize (a .&. 0x3FF)) b
      Nothing -> (fin s False, idleOut s)
  | mmc5 && a >= 0x5000 && a < 0x6000 = (fin s { ksMap = m5Write a v (ksMap s) } False, idleOut s)
  | a < 0x6000 = (fin s False, idleOut s)
  | mmc5 = case prgTarget (ksCfg s) (ksMap s) a of
      PrgRam i | m5RamWritable (ksMap s) -> memW SpPrgRam (resize i)
      _ -> (fin s False, idleOut s)
  | a < 0x8000 = if ccPrgRam (ksCfg s) then memW SpPrgRam (resize (a .&. 0x1FFF)) else (fin s False, idleOut s)
  | otherwise = (fin s { ksMap = mapWrite (ksCfg s) a v (ksMap s) } True, idleOut s)
 where
  mmc5 = ccMapper (ksCfg s0) == MMmc5
  s = s0 { ksBus = v, ksAccess = (a, v, True) }
  fin s' cart = s' { ksSeq = QDot3, ksMemRd = False, ksMap = (ksMap s') { msPrevWr = cart } }
  memW sp addr = memWB sp addr v
  memWB sp addr b = (fin s False, (idleOut s) { koMem = Just (MemReq sp addr (Just b)) })

-- * In hardware

-- | The PRG ROM read the core makes in this clock (in 'koMem'): an address
-- for one clock; the core asks again only after the answer, which comes
-- later, once (the byte: 'nesCore''s PRG input).
prgRequest :: CoreOut -> Maybe (Unsigned 19)
prgRequest o = case koMem o of
  Just (MemReq SpPrg a Nothing) -> Just a
  _ -> Nothing

-- | The PRG answers' bookkeeping ('prgDdr'): a read is out at DDR, a newer
-- one waits for it, the one out is stale (the core was reset meanwhile and
-- asked again), and the DDR address of the one out.
data PrgWait = PrgWait
  { pwOut   :: Bool
  , pwQueue :: Maybe (Unsigned 19)
  , pwStale :: Bool
  , pwAddr  :: DdrAddr
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFDataX

-- | PRG ROM in DDR ("Platform.Mem.Ddr", as "Gb.Machine"'s cartridge): the
-- whole @.nes@ file from @base@ (PRG after its 16-byte header; the PS
-- writes it while it holds the core), 32 cache lines with prefetch, the
-- cache emptied while @inval@. One read out at a time: a read the core
-- makes while another is out (it was reset by hold) waits, and the old
-- answer is dropped. The request and the answer are registered (timing):
-- a cache hit costs the CPU cycle 3 more clocks, still within its 14.
prgDdr
  :: HiddenClockResetEnable dom
  => DdrAddr                 -- ^ where the file starts
  -> Signal dom Bool         -- ^ empty the cache (the PS rewrites DDR)
  -> Signal dom AxiRdS2M     -- ^ HP0's answers
  -> Signal dom (Maybe (Unsigned 19))
  -> (Signal dom (Maybe (Unsigned 8)), Signal dom AxiRdM2S)
prgDdr base inval s2m req = (register Nothing ans, m2s)
 where
  (issue, ans) = unbundle (mealy prgWaitStep (PrgWait False Nothing False 0) (bundle (req, resp)))
  (resp, m2s, _status) = ddrReadPort (SNat @5) ddrConfigDefault inval (register Nothing issue) s2m
  prgWaitStep s (r, w) = case w of
    Just word
      | pwStale s -> go (r <|> pwQueue s) Nothing
      | otherwise -> (s { pwOut = False, pwQueue = r }, (Nothing, Just (unpack (ddrByte (pwAddr s) word))))
    Nothing
      | pwOut s -> (s { pwQueue = r <|> pwQueue s, pwStale = pwStale s || isJust r }, (Nothing, Nothing))
      | otherwise -> go (r <|> pwQueue s) Nothing
   where
    go n out = (PrgWait (isJust n) Nothing False (maybe (pwAddr s) full n), (full <$> n, out))
  full a = base + 16 + resize a

-- | The core with its memories: PRG ROM outside (the answers to its
-- 'prgRequest's; DDR, 'prgDdr'), CHR (256 KiB of block RAM from an image file: a cartridge
-- baked into the bitstream or the simulation; the loader writes it), PRG
-- RAM, CPU RAM, nametable RAM and OAM. A clock carries at most one PPU
-- tick, so PPU ports A and B never ask in the same clock and one simple
-- block RAM serves both; each port's byte is held until its next request,
-- as 'CoreIn' expects.
nesCore
  :: HiddenClockResetEnable dom
  => Signal dom CartCfg         -- ^ the cartridge (from its header; held while running)
  -> Signal dom (Maybe (Unsigned 8)) -- ^ PRG ROM: the answer to the last 'prgRequest'
  -> FilePath                   -- ^ CHR image, 256 KiB (binary text, one byte per line)
  -> Signal dom Bool            -- ^ run (low: held at power-on, the loader may write CHR)
  -> Signal dom (Maybe (Unsigned 18, Unsigned 8)) -- ^ CHR write from the loader
  -> Signal dom Bool            -- ^ one PPU dot of time has elapsed
  -> Signal dom (Unsigned 8)    -- ^ controller 1
  -> Signal dom (Unsigned 8)    -- ^ controller 2
  -> (Signal dom CoreOut, Signal dom Channels) -- ^ and the APU's outputs for the speakers
nesCore cfg prgAns chrFile run chrLoad dot pad1 pad2 = (out, chans)
 where
  (out, chans, _, _) = nesCoreWith cfg prgAns chrFile run chrLoad dot pad1 pad2
                         (pure False) (pure False) (pure Nothing) (pure Nothing)

-- | The memories a state's port reaches (side track N, "Nes.StateHooks"):
-- RAM, nametable RAM, OAM (read only: the restore routine's DMA fills
-- it), MMC5's ExRAM (both copies), CHR (RAM: the first 8 KiB), PRG RAM.
data StMem = StRam | StCiram | StOam | StExram | StChr | StPrgRam
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFDataX

-- | A state port access: the memory, the address, a byte to write.
type StatePort = Maybe (StMem, Unsigned 16, Maybe (Unsigned 8))

-- | 'nesCore' with save states (side track N): while @frozen@ the core
-- takes no step; while @ovlOn@ the ROM overlay (2 KiB of block RAM,
-- written through @ovlWr@) answers the CPU side's reads of $0000-$1FFF
-- and $6000-$FFFF ('SpOvl'); a state port access (while frozen) takes its
-- memory's address and write from the core, its byte out the clock after
-- (the last output). Also the core's state before each clock's step.
nesCoreWith
  :: forall dom. HiddenClockResetEnable dom
  => Signal dom CartCfg
  -> Signal dom (Maybe (Unsigned 8))
  -> FilePath
  -> Signal dom Bool
  -> Signal dom (Maybe (Unsigned 18, Unsigned 8))
  -> Signal dom Bool
  -> Signal dom (Unsigned 8)
  -> Signal dom (Unsigned 8)
  -> Signal dom Bool                              -- ^ frozen
  -> Signal dom Bool                              -- ^ the overlay answers
  -> Signal dom (Maybe (Unsigned 11, Unsigned 8)) -- ^ an overlay write
  -> Signal dom StatePort                         -- ^ the state port
  -> (Signal dom CoreOut, Signal dom Channels, Signal dom CoreState, Signal dom (Unsigned 8))
nesCoreWith cfg prgAns chrFile run chrLoad dot pad1 pad2 frozen ovlOn ovlWr port = (out, chans, coreSt, portRd)
 where
  (out, chans, coreSt) = unbundle (mealy step (coreInit cfg0) (bundle (cfg, run, frozen, inp)))
  -- (the state and the sound out before the guards: the hooks' freeze
  -- reads the state, and the step depends on the freeze)
  step s (c, r, f, i) = (next, (o, apuAudioChannels (ksApu s), s))
   where
    (next, o)
      | not r = (coreInit c, (idleOut s) { koIdle = True })
      | f = (s, (idleOut s) { koIdle = True })
      | otherwise = coreStep s { ksCfg = c } i
  cfg0 = CartCfg MNrom 0x7FFF 0x1FFF False False True
  inp = CoreIn <$> dot <*> rdData <*> rdOk <*> vramA <*> vramB <*> oam <*> exLatch <*> pad1 <*> pad2 <*> ovlOn

  -- the state port: which memory, its address and write
  portAt sm = (\p' -> case p' of Just (m', a, _) | m' == sm -> Just a; _ -> Nothing) <$> port
  portWr sm = (\p' -> case p' of Just (m', a, Just d) | m' == sm -> Just (a, d); _ -> Nothing) <$> port
  withPort :: KnownNat n => StMem -> Signal dom (Unsigned n) -> Signal dom (Unsigned n)
  withPort sm a = (\pa x -> maybe x resize pa) <$> portAt sm <*> a
  withPortW :: Signal dom (Maybe (Unsigned n, d)) -> Signal dom (Maybe (Unsigned n, d)) -> Signal dom (Maybe (Unsigned n, d))
  withPortW pw w = (\p' x -> case p' of Just _ -> p'; Nothing -> x) <$> pw <*> w
  pw8 :: KnownNat n => StMem -> Signal dom (Maybe (Unsigned n, Unsigned 8))
  pw8 sm = fmap (\(a, d) -> (resize a, d)) <$> portWr sm

  -- CPU side: one of three memories answers, chosen by the last request.
  memReq = koMem <$> out
  lastSpace = regEn SpRam (isJust <$> memReq) (maybe SpRam mqSpace <$> memReq)
  wantRd sp = (\m -> case m of Just (MemReq s _ Nothing) -> s == sp; _ -> False) <$> memReq
  addrOf sp = (\m -> case m of Just (MemReq s a _) | s == sp -> a; _ -> 0) <$> memReq
  wrOf sp = (\m -> case m of Just (MemReq s a (Just d)) | s == sp -> Just (a, d); _ -> Nothing) <$> memReq
  -- PRG ROM: its byte when it comes, held until the next read
  prgReq = prgRequest <$> out
  prgHeld = regMaybe 0 prgAns
  prg = (\a h -> maybe h id a) <$> prgAns <*> prgHeld
  prgOk = register True ((\q a ok -> if isJust q then False else ok || isJust a) <$> prgReq <*> prgAns <*> prgOk)
  rdOk = (\sp ok a -> sp /= SpPrg || ok || isJust a) <$> lastSpace <*> prgOk <*> prgAns
  -- PRG RAM: 64 KiB (MMC5's; 8 KiB used by the others). blockRam1, as the
  -- other zero-filled RAMs: Clash builds blockRam's initial contents as a
  -- literal (64 Ki elements took it 20 minutes)
  prgRam = blockRam1 NoClearOnReset (SNat @65536) 0 (withPort StPrgRam (resize <$> addrOf SpPrgRam))
             (withPortW (pw8 StPrgRam) (fmap (\(a, d) -> (resize a :: Unsigned 16, d)) <$> wrOf SpPrgRam))
  ram = blockRam1 NoClearOnReset (SNat @2048) 0 (withPort StRam (resize <$> addrOf SpRam))
          (withPortW (pw8 StRam) (fmap (\(a, d) -> (resize a :: Unsigned 11, d)) <$> wrOf SpRam))
  -- the ROM overlay (side track N)
  ovl = blockRam1 NoClearOnReset (SNat @2048) 0 (resize <$> addrOf SpOvl) ovlWr
  wasRd = register False (or <$> sequenceA [wantRd SpPrgRam, wantRd SpRam, wantRd SpExram, wantRd SpOvl])
  ramNow = (\sp r w e o -> case sp of SpPrgRam -> r; SpExram -> e; SpOvl -> o; _ -> w) <$> lastSpace <*> prgRam <*> ram <*> exram <*> ovl
  ramData = mux wasRd ramNow (register 0 ramData)
  rdData = (\sp p r -> if sp == SpPrg then p else r) <$> lastSpace <*> prg <*> ramData

  -- PPU side: CHR and nametable RAM, ports A and B on one RAM each.
  reqA = koVramA <$> out
  reqB = koVramB <$> out
  chrAddrS = withPort StChr ((\a b -> case (a, b) of
                 (Just (VChr x), _) -> x
                 (_, Just (VChr x, _)) -> x
                 _ -> 0) <$> reqA <*> reqB)
  chrWr = withPortW (fmap (\(a, d) -> (resize a, pack d)) <$> portWr StChr)
            ((\b l -> case b of Just (VChr x, Just d) -> Just (x, pack d); _ -> fmap pack <$> l) <$> reqB <*> chrLoad)
  ntAddrS = withPort StCiram ((\a b -> case (a, b) of
                (Just (VNt x), _) -> x
                (_, Just (VNt x, _)) -> x
                _ -> 0) <$> reqA <*> reqB)
  ntWr = withPortW (pw8 StCiram) ((\b -> case b of Just (VNt x, Just d) -> Just (x, d); _ -> Nothing) <$> reqB)
  chr = unpack <$> blockRamFile (SNat @262144) chrFile chrAddrS chrWr
  nt = blockRam1 NoClearOnReset (SNat @2048) 0 ntAddrS ntWr
  -- MMC5's ExRAM (1 KiB), twice with the same writes: one for the CPU and
  -- the PPU's nametable reads, one for extended attributes' latch
  -- ('koExLatch'), read in the same clock as a nametable fetch
  exAddrS = withPort StExram ((\m a b -> case (m, a, b) of
                (Just (MemReq SpExram x _), _, _) -> resize x
                (_, Just (VExram x), _) -> x
                (_, _, Just (VExram x, _)) -> x
                _ -> 0) <$> memReq <*> reqA <*> reqB)
  exWr = withPortW (pw8 StExram) ((\m b -> case (m, b) of
             (Just (MemReq SpExram x (Just d)), _) -> Just (resize x :: Unsigned 10, d)
             (_, Just (VExram x, Just d)) -> Just (x, d)
             _ -> Nothing) <$> memReq <*> reqB)
  exram = blockRam1 NoClearOnReset (SNat @1024) 0 exAddrS exWr
  exLatch = blockRam1 NoClearOnReset (SNat @1024) 0 (maybe 0 id . koExLatch <$> out) exWr
  -- Which memory and which port the last request was for.
  srcOf v = case v of
    VChr _ -> SrcChr
    VNt _ -> SrcNt
    VExram _ -> SrcEx
    VConst c -> SrcConst c
  lastA = register Nothing (fmap srcOf <$> reqA)
  lastB = register Nothing ((\b -> case b of Just (v, Nothing) -> Just (srcOf v); _ -> Nothing) <$> reqB)
  pick l c n e = case l of
    Just SrcChr -> c
    Just SrcEx -> e
    Just (SrcConst k) -> k
    _ -> n
  vramA = mux (isJust <$> lastA) (pick <$> lastA <*> chr <*> nt <*> exram) (register 0 vramA)
  vramB = mux (isJust <$> lastB) (pick <$> lastB <*> chr <*> nt <*> exram) (register 0 vramB)

  -- OAM
  oam = oamRam (withPort StOam (koOamAddr <$> out)) (koOamWrite <$> out) (koOamRow <$> out)

  -- the state port's byte: the memory it read, the clock after
  portLast = register Nothing ((fmap (\(m', _, _) -> m')) <$> port)
  portRd = (\l r n o e ch p' -> case l of
               Just StRam -> r
               Just StCiram -> n
               Just StOam -> o
               Just StExram -> e
               Just StChr -> ch
               Just StPrgRam -> p'
               Nothing -> 0) <$> portLast <*> ram <*> nt <*> oam <*> exram <*> chr <*> prgRam
