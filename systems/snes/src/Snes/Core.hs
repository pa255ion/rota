{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=0 #-}
{-|
The Super Nintendo as a circuit (PLAN.md step 7.10): "Snes.Bus" (the 5A22
with the 65C816), "Snes.Ppu", "Snes.Apu" (the SPC700 and the S-DSP) and
"Snes.RomPort", with their memories in block RAM: WRAM (128 KiB), SRAM
(32 KiB), the DMA registers (16 small memories), VRAM (two 32 KiB chips),
CGRAM, OAM's four lanes, the PPU's line buffer and column ring, ARAM (64
KiB, two ports: the SPC700's and the DSP's), the boot ROM (64 bytes, 'iplImage'), the DSP's samples and its
Gaussian table. The ROM comes from DDR ('scDdrReq', answered on
'scDdrAnswer' as "Platform.Mem.Ddr"'s port answers); the master clock runs
when 'run' says (the display's pace, "Snes.Display"). SRAM has a second
port for the PS (battery saves, step 7.11: 'scSramPs', 'scSramWrote').

What crosses between the parts in one clock (the PPU's read value, the
sound module's readiness and read, the ROM's byte) comes from their state
registers only ('ppuRead', 'apuStatus', 'rpOut'), never from the access
being driven, so there is no loop. @snes-core@ runs the same parts over
models of these memories against the spec.

__Save states__ (step 7.14, the direct path; "Snes.StateHooks" drives
'StCtl', 'stIdle' leaves the core as it was). Every register of the core
is one record ('CoreRegs'): the parts' states, the frame count, the ROM
read registered, and the latch of the bytes in flight ('Inflight': what
the memories answer for the addresses of the clock before, the next
step's inputs). Frozen ('stFreeze'):
- no step is taken;
- no memory is written but by the PS;
- no DDR request goes out;
- the memories' ports are the PS's ('stPort');
- the record is one chain of bytes, shifted a byte at a time
  ('stShift': the byte in, the top byte out on 'scTop'). A read shifts
  the top byte back in, so the PS's 'chainBytes' reads go round the
  chain once and leave it as it was.
On the first frozen clock the bytes in flight are latched, and on the
first clock after a freeze the step takes the latch in their place. A
state written back with its latch so resumes exactly where it was taken.
-}
module Snes.Core
  ( SnesCore (..)
  , snesCore
  , iplImage
    -- * Save states
  , StCtl (..)
  , stIdle
  , StPort (..)
  , CoreRegs (..)
  , Inflight (..)
  , chainBytes
  , atRest
    -- * Save states through the CPU (step 7.15: SNES_STATES=cpu)
  , OvlIn (..)
  , ovlIdle
  , atRestCpu
  , apuChainBytes
  ) where

import Clash.Prelude

import qualified Data.List as L
import Data.Maybe (isJust, isNothing)
import qualified Data.Vector.Unboxed as U
#ifdef SNES_SIM_STRICT
import Clash.Sized.Internal.BitVector (unsafeToNatural)
#endif

import Snes.Apu
import Snes.Bus
import Snes.Ppu
import Snes.RomPort
import Snes.Spec.Gauss (gauss)
import qualified Spc700.Core as S
import W65c816.Core (Cpu (..), Phase (..), Stage (..))
import Snes.StateTap (tableByte)
#ifdef SNES_DSP1
import Snes.Dsp1
import Snes.Upd77c25 (ReqAddrs (..))
#endif

-- | The boot ROM's image (64 lines of 8 binary digits, @make snes-image@
-- from build/snes/ipl.bin; the user's own dump in its place, as decided).
iplImage :: FilePath
iplImage = "build/snes/ipl.mem"

gaussTable :: Vec 512 (Unsigned 11)
gaussTable = $(listToVecTH (L.map fromIntegral (U.toList gauss) :: [Unsigned 11]))

data SnesCore dom = SnesCore
  { scPixel    :: Signal dom (Maybe (Unsigned 8, Unsigned 8, Unsigned 15, Unsigned 15)) -- ^ x, row, left and right half-dots
  , scFrame    :: Signal dom (Unsigned 2)     -- ^ the core's frame count (mod 4)
  , scV        :: Signal dom (Unsigned 9)     -- ^ its line
  , scH        :: Signal dom (Unsigned 11)    -- ^ its master clock in the line
  , scOverscan :: Signal dom Bool             -- ^ the 239-line mode
  , scSample   :: Signal dom (Maybe (Signed 16, Signed 16)) -- ^ the DSP's sample (32 kHz)
  , scDdrReq   :: Signal dom (Maybe (Unsigned 23))          -- ^ a ROM offset to read from DDR
  , scSramPs   :: Signal dom (Unsigned 8)     -- ^ SRAM's second port: the byte the PS's op read
  , scSramWrote :: Signal dom Bool            -- ^ the game writes SRAM this clock
  , scDebug    :: Signal dom (Vec 24 (Unsigned 8)) -- ^ for the trace ('debugBytes')
    -- save states
  , scRest     :: Signal dom Bool             -- ^ 'atRest': a freeze may begin
  , scTop      :: Signal dom (BitVector 8)    -- ^ the chain's top byte (frozen)
  , scPortRd   :: Signal dom (BitVector 16)   -- ^ the byte or word the PS's port read last clock
  , scRestCpu  :: Signal dom Bool             -- ^ 'atRestCpu': the CPU build's freeze may begin
  , scOvlDone  :: Signal dom Bool             -- ^ the overlay's last instruction ended (the hooks disarm it)
  }

-- * Save states

-- | A memory's byte (or word) through its port, frozen: read (the value on
-- 'scPortRd' the next clock), or write.
data StPort
  = PWram (Unsigned 17) (Maybe (Unsigned 8))
  | PVram Bool (Unsigned 15) (Maybe (Unsigned 8))  -- ^ the high chip, a word address
  | PCgram (Unsigned 8) (Maybe (Unsigned 15))
  | POam (Index 4) (Unsigned 7) (Maybe (Unsigned 8)) -- ^ a lane, its entry
  | PDma (Index 16) (Index 8) (Maybe (Unsigned 8))   -- ^ a register, its channel
  | PAram (Unsigned 16) (Maybe (Unsigned 8))
  | PSmp (Unsigned 8) (Maybe (Signed 16))
  | PTable (Unsigned 10)                           -- ^ the state table's byte (read only; step 7.15, the CPU build)
  | POvl (Unsigned 11) (Maybe (Unsigned 8))        -- ^ the ROM overlay's byte (written; the same)
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFDataX, BitPack)

-- | The ROM overlay of a load through the CPU (step 7.15): armed, its end
-- address (the routine's last instruction's opcode, bank 00h). The hooks
-- keep it (outside the core's reset: armed while held, it answers the
-- reset's vector).
data OvlIn = OvlIn
  { oiArmed :: Bool
  , oiEnd   :: Unsigned 16
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFDataX

ovlIdle :: OvlIn
ovlIdle = OvlIn False 0

data StCtl = StCtl
  { stFreeze :: Bool                 -- ^ no step, the ports the PS's
  , stShift  :: Maybe (BitVector 8)  -- ^ frozen: the chain shifted a byte, this one in
  , stPort   :: Maybe StPort         -- ^ frozen: a memory's port
  , stFinish :: Bool                 -- ^ (the CPU build's freeze, its clock before) a step with no master clock: the CPU's instruction finishes, its registers final, the time its end
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFDataX

stIdle :: StCtl
stIdle = StCtl False Nothing Nothing False

-- | What the memories answer for the addresses of the clock before: the
-- next step's inputs from them.
data Inflight = Inflight
  { lWram  :: Unsigned 8
  , lSram  :: Unsigned 8
  , lDma   :: Vec 16 (Unsigned 8)
  , lPpu   :: PpuIn                  -- ^ (its tick and access unused)
  , lAram  :: Unsigned 8
  , lDsp   :: Unsigned 8
  , lIpl   :: Unsigned 8
  , lSmp   :: Signed 16
  , lGauss :: Unsigned 11
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (NFDataX, BitPack)

-- | Every register of the core, the save state's chain (padded to whole
-- bytes, 'chainBytes').
data CoreRegs = CoreRegs
  { crBus   :: BusC
  , crPpu   :: PpuC
  , crApu   :: ApuC
  , crRp    :: RomPort
  , crFrame :: Unsigned 2
  , crRd    :: Maybe (Unsigned 23)    -- ^ the bus's ROM read, registered before the ROM port
  , crLatch :: Inflight
#ifdef SNES_DSP1
  , crDsp   :: DspRegs                -- ^ (the DSP-1 build) the cartridge's chip, "Snes.Dsp1"
#endif
  , crPad   :: BitVector CorePad
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (NFDataX, BitPack)

-- | The padding that makes the chain whole bytes.
#ifdef SNES_DSP1
type CoreRaw = BitSize BusC + BitSize PpuC + BitSize ApuC + BitSize RomPort + 2 + BitSize (Maybe (Unsigned 23)) + BitSize Inflight + BitSize DspRegs
#else
type CoreRaw = BitSize BusC + BitSize PpuC + BitSize ApuC + BitSize RomPort + 2 + BitSize (Maybe (Unsigned 23)) + BitSize Inflight
#endif
type CorePad = Mod (8 - Mod CoreRaw 8) 8

-- | The CPU build's chain: the sound module's record, padded likewise.
type ApuPad = Mod (8 - Mod (BitSize ApuC) 8) 8

apuChainBytes :: Int
apuChainBytes = fromIntegral (natToInteger @(BitSize ApuC + ApuPad)) `div` 8

-- | The chain's length in bytes (the state space's record; a test checks
-- the padding makes it whole).
chainBytes :: Int
chainBytes = fromIntegral (natToInteger @(BitSize CoreRegs)) `div` 8

coreInit :: CoreRegs
coreInit = CoreRegs busInit ppuInit apuInit romPortInit 0 Nothing
  (Inflight 0x55 0 (repeat 0xFF) ppuIdleIn 0 0 0 0 0)
#ifdef SNES_DSP1
  dspInit
#endif
  0

-- | A freeze may begin: no ROM read on its way or registered, the ROM
-- port idle (so no DDR answer is due), the 5A22 not waiting for a read.
atRest :: CoreRegs -> Bool
atRest c =
  isNothing (crRd c) && isNothing (rpAsked (crRp c)) && isNothing (rpWant (crRp c)) && isNothing (rpOut (crRp c))
    && rSrc (crBus c) /= SrcRom

-- | The record, forced every clock in Haskell's simulations (SNES_SIM_STRICT,
-- the snes library's cabal stanza): a lazy register of 8,800 bits over a
-- million clocks held its history in thunks and ran snes-savestate out of
-- its heap. The HDL (Clash's runs, without it) is the record itself.
strictRegs :: BitVector (BitSize CoreRegs) -> BitVector (BitSize CoreRegs)
#ifdef SNES_SIM_STRICT
strictRegs c = deepseqX c c
#else
strictRegs = id
#endif

-- | A register's bits forced each clock in Haskell's simulation (as
-- 'strictRegs').
strictBits :: KnownNat n => BitVector n -> BitVector n
#ifdef SNES_SIM_STRICT
strictBits b = deepseqX b b
#else
strictBits = id
#endif

-- | A memory's byte with its undefined bits 0 (SNES_SIM_STRICT only, as
-- 'simDefined': the overlay's, whose block RAM answers undefined before its
-- first read in Clash's simulation).
simByte :: Unsigned 8 -> Unsigned 8
#ifdef SNES_SIM_STRICT
simByte = fromIntegral . unsafeToNatural . pack
#else
simByte = id
#endif

#ifdef SNES_DSP1
-- | A word's undefined bits 0 (SNES_SIM_STRICT only, as 'simByte'): the
-- DSP-1's RAM in Clash's simulation answers undefined before it is
-- written.
simWord :: Unsigned 16 -> Unsigned 16
#ifdef SNES_SIM_STRICT
simWord = fromIntegral . unsafeToNatural . pack
#else
simWord = id
#endif

-- | The same for the program ROM's 24-bit words (its block RAM answers
-- undefined before its first read).
simOp24 :: BitVector 24 -> BitVector 24
#ifdef SNES_SIM_STRICT
simOp24 = fromIntegral . unsafeToNatural
#else
simOp24 = id
#endif
#endif

-- | The memories' bytes in flight with their undefined bits 0, as Verilator
-- has them (SNES_SIM_STRICT only: Clash's simulation of a block RAM answers
-- undefined before its first read, and the forced record took that as an
-- error). The HDL: the bytes themselves.
simDefined :: Inflight -> Inflight
#ifdef SNES_SIM_STRICT
simDefined = unpack . fromIntegral . unsafeToNatural . pack
#else
simDefined = id
#endif

-- | A true dual-port block RAM's op with an undefined address made 0
-- (SNES_SIM_STRICT only, as 'simDefined'): Clash's simulation of such a
-- RAM makes the whole memory undefined on an access at an undefined
-- address, and while the chain shifts the record is garbage (an 'Index'
-- out of range unpacks undefined), so the frozen core's addresses could
-- wipe ARAM, VRAM, CGRAM, OAM or SRAM; the HDL reads some address (no
-- writes frozen). An undefined op is a read of 0.
simOp :: KnownNat n => RamOp n a -> RamOp n a
#ifdef SNES_SIM_STRICT
simOp o = case isX o of
  Left _ -> RamRead 0
  Right (RamRead a) -> RamRead (simIx a)
  Right (RamWrite a v) -> RamWrite (simIx a) v
  Right op -> op
 where
  simIx a = if hasUndefined a then 0 else a
#else
simOp = id
#endif

-- | 'trueDualPortBlockRam' with 'simOp' on both ports.
tdpRam :: (HiddenClock dom, KnownNat n, NFDataX a) => Signal dom (RamOp n a) -> Signal dom (RamOp n a) -> (Signal dom a, Signal dom a)
tdpRam a b = trueDualPortBlockRam (simOp <$> a) (simOp <$> b)

-- | The CPU build's freeze: at rest ('atRest'), the CPU at an
-- instruction's end ('StDone'), not stopped, no DMA owed, the multiplier
-- and divider idle, the pads' auto-read done, no interrupt seen or owed
-- ("Snes.Spec.SaveState"'s 'specAtRest'; the hooks add V-blank).
atRestCpu :: CoreRegs -> Bool
atRestCpu c =
  let b = crBus c
      cpu = bCpu b
  in atRest c && cStage cpu == StDone && cPhase cpu /= PhStop && bDmaPend b == 0 && bMpyCtr b == 0
     && bDivCtr b == 0 && bJoyBusy b == 0 && not (cNmiNeed cpu) && cPollCur cpu == 0 && cPollPrev cpu == 0


#ifndef SNES_STATES_CPU
-- | The record's bits shifted a byte: this byte in at the bottom, the top
-- byte out (with the top byte itself in: a rotation, the read). (On the
-- bits, not the record: a record between two shifts would drop the bits
-- under a 'Nothing' in Haskell's simulation, and a round would not give
-- the record back; the HDL's register is the bits either way.)
shiftChain :: BitVector 8 -> BitVector (BitSize CoreRegs) -> BitVector (BitSize CoreRegs)
shiftChain b c = (c `shiftL` 8) .|. zeroExtend b

topByte :: BitVector (BitSize CoreRegs) -> BitVector 8
topByte c = truncateB (c `shiftR` (fromIntegral (natToInteger @(BitSize CoreRegs)) - 8))
#endif

-- | The machine from the master clock's enable, the cartridge, the pads,
-- the DDR port's answers, the PS's SRAM op (its second port) and the save
-- states' control ('stIdle' without them).
snesCore
  :: forall dom. HiddenClockResetEnable dom
  => Signal dom Bool -> Signal dom CartCfg -> Signal dom (Vec 2 (Unsigned 12))
  -> Signal dom (Maybe (Vec 8 (Unsigned 8)))
  -> Signal dom (RamOp 32768 (Unsigned 8))
  -> Signal dom StCtl
  -> Signal dom OvlIn
  -> SnesCore dom
snesCore run cfg pads ddrAnswer psSram st ovl = SnesCore
  { scPixel = gate (poPixel <$> po)
  , scFrame = crFrame <$> cs
  , scV = bVc . crBus <$> cs
  , scH = bHc . crBus <$> cs
  , scOverscan = (\c -> testBit (bSetini (crBus c)) 2) <$> cs
  , scSample = (\h v -> if h then Nothing else v) <$> hold <*> gate (aoSample <$> ao)
  , scDdrReq = gate (roRequest <$> ro)
  , scSramPs = sramB
  , scSramWrote = (\f o -> not f && isJust (boSramW o)) <$> frz <*> bo
  , scDebug = (\c -> debugBytes (crBus c) (crApu c)) <$> cs
  , scRest = atRest <$> cs
#ifdef SNES_STATES_CPU
  , scTop = (\a -> truncateB (a `shiftR` (fromIntegral (natToInteger @(BitSize ApuC + ApuPad)) - 8))) <$> apuBits
#else
  , scTop = topByte <$> csBits
#endif
  , scPortRd = portRd
  , scRestCpu = atRestCpu <$> cs
  , scOvlDone = ovlDone
  }
 where
  frz = stFreeze <$> st
  -- the first clock frozen (the latch), the first after (the prime)
  frzLast = register False frz
  capture = (&&) <$> frz <*> (not <$> frzLast)
  prime = (&&) <$> frzLast <*> (not <$> frz)
  gate :: Signal dom (Maybe a) -> Signal dom (Maybe a)
  gate x = (\f v -> if f then Nothing else v) <$> frz <*> x
  port = (\s -> if stFreeze s then stPort s else Nothing) <$> st
  -- the record
  -- (the register holds the record's bits: see 'shiftChain')
#ifdef SNES_STATES_CPU
  -- (the CPU build: the sound module's record in a register of its own,
  -- with its padding; its clock enable is the hold's and the freeze's, so
  -- the chain's only cost a bit is its shift. The record's register keeps
  -- the rest; its copy of the sound module's bits goes unused)
  csBits = register (pack coreInit) (strictRegs <$> csBits')
  apuBits = regEn (pack apuInit ++# (0 :: BitVector ApuPad)) apuEn (strictBits <$> apuBits')
  cs = (\b a -> (unpack b) { crApu = unpack (fst (split a :: (BitVector (BitSize ApuC), BitVector ApuPad))) }) <$> csBits <*> apuBits
  apuEn = (\s h -> if stFreeze s then isJust (stShift s) else not h) <$> st <*> hold
  apuBits' = (\s a n -> case stShift s of
                 Just v | stFreeze s -> (a `shiftL` 8) .|. zeroExtend v
                 _ -> pack (crApu n) ++# snd (split a :: (BitVector (BitSize ApuC), BitVector ApuPad))) <$> st <*> apuBits <*> stepped
  csBits' = (\s c n cap mi -> if not (stFreeze s) then pack n
                             else if cap then pack (c { crLatch = mi })
                             else pack c) <$> st <*> cs <*> stepped <*> capture <*> memIns
#else
  csBits = register (pack coreInit) (strictRegs <$> csBits')
  cs = unpack <$> csBits
  csBits' = (\s b c n cap mi h -> if not (stFreeze s) then pack (holdApu h c n)
                               else if cap then pack (c { crLatch = mi })
                               else maybe b (`shiftChain` b) (stShift s)) <$> st <*> csBits <*> cs <*> stepped <*> capture <*> memIns <*> hold
#endif
  -- the sound module held (its record kept, its memories unwritten) while
  -- a load's overlay is armed: it goes on from its written record from
  -- the clock its routine's last instruction ends (step 7.15)
#ifndef SNES_STATES_CPU
  holdApu h c n = if h then n { crApu = crApu c } else n
#endif
  frzA = (||) <$> frz <*> hold
  -- the ROM overlay (the CPU build): armed by the hooks, off once its last
  -- instruction (the one that read the end address) has finished
#ifdef SNES_STATES_CPU
  ovlOn = (\o d -> oiArmed o && not d) <$> ovl <*> ovlDoneR
  ovlSeen = register False ((\on sn r e -> on && (sn || r == Just e)) <$> ovlOn <*> ovlSeen <*> (boOvl <$> bo) <*> (oiEnd <$> ovl))
  ovlDone = (\on sn k -> on && sn && isJust k) <$> ovlOn <*> ovlSeen <*> (boStep <$> bo)
  ovlDoneR = register False ((\o d r -> oiArmed o && (d || r)) <$> ovl <*> ovlDone <*> ovlDoneR)
  hold = (\on d -> on && not d) <$> ovlOn <*> ovlDone
  -- (0000h-1FFFh with bit 10 inverted, as "Snes.Spec.Machine"'s)
  ovlOff a = if a < 0x2000 then (resize a .&. 0x7FF) `xor` 0x400 else resize a .&. 0x7FF :: Unsigned 11
  ovlD = blockRam (replicate (SNat @2048) (0 :: Unsigned 8)) (maybe 0 ovlOff . boOvl <$> bo)
           ((\p -> case p of
               Just (POvl a (Just v)) -> Just (a, v)
               _ -> Nothing) <$> port)
#else
  ovlOn = pure False
  ovlDone = pure False
  hold = pure False
  ovlD = pure 0
  _unusedOvl = ovl
#endif
#ifdef SNES_DSP1
  -- (while the firmware loads the 5A22 stays as it is: a ROM read of its
  -- own issued then would take the ROM port from the load, and its answer
  -- could go to either; it starts once the firmware is in)
  stepped = CoreRegs <$> busHeld <*> ppu' <*> apu' <*> rp' <*> frame' <*> (boRom <$> bo) <*> (crLatch <$> cs) <*> dsp' <*> (crPad <$> cs)
  busHeld = (\c b -> if dspLoaded (crDsp c) then b else crBus c) <$> cs <*> bus'
  -- the DSP-1 (its memories outside: the program and data ROMs, written
  -- by the load, and RAM; its RAM not written frozen)
  (dsp', dOut) = dspUnit (crDsp <$> cs) dIn
  dIn = (\o c op rd m k -> DspIn (boTick o) (boDsp o) (rpOut (crRp c)) (simOp24 op) (simWord rd) m k) <$> bo <*> cs <*> dProg <*> dDat <*> dMem <*> dMemK
  dRq = dspReq . crDsp <$> cs
  dProg = blockRam (replicate (SNat @2048) 0) (raPc <$> dRq) (doProgW <$> dOut)
  dDat = blockRam (replicate d1024 0) (raRp <$> dRq) (doDatW <$> dOut)
  (dMemR, dMemKR) = tdpRam ((\f r o -> case doRam o of
                               Just (a, v) | not f -> RamWrite (fromIntegral a) v
                               _ -> RamRead (fromIntegral (raDp r))) <$> frz <*> dRq <*> dOut)
                           ((\r -> RamRead (fromIntegral (raDpK r))) <$> dRq :: Signal dom (RamOp 256 (Unsigned 16)))
  dMem = simWord <$> dMemR
  dMemK = simWord <$> dMemKR
#else
  stepped = CoreRegs <$> bus' <*> ppu' <*> apu' <*> rp' <*> frame' <*> (boRom <$> bo) <*> (crLatch <$> cs) <*> (crPad <$> cs)
#endif
  -- the bytes in flight: the memories', or (the prime) the latch
  memIns = simDefined <$> (Inflight <$> wramD <*> sramD <*> dmaD <*> pinMem <*> aramD <*> aramDsp <*> iplD <*> smpD <*> gaussD)
  ins = (\p c m -> if p then crLatch c else m) <$> prime <*> cs <*> memIns
  -- the 5A22
  (bus', bo) = busUnit (crBus <$> cs) bin
#ifdef SNES_DSP1
  -- (no master clock until the DSP-1's firmware is in)
  bin = (\r cf pd i c pr (a0, a1, av) on ov ->
           let d = crDsp c in
           BusIn (r && dspLoaded d) cf pd (lWram i) (lSram i) (rpOut (crRp c)) pr a0 a1 av (lDma i) on ov (dspReady0 d) (dspReady1 d) (dsByte d))
          <$> runF <*> cfg <*> pads <*> ins <*> cs <*> ppuR <*> apuSt <*> ovlOn <*> (simByte <$> ovlD)
#else
  bin = (\r cf pd i c pr (a0, a1, av) on ov ->
           BusIn r cf pd (lWram i) (lSram i) (rpOut (crRp c)) pr a0 a1 av (lDma i) on ov)
          <$> runF <*> cfg <*> pads <*> ins <*> cs <*> ppuR <*> apuSt <*> ovlOn <*> (simByte <$> ovlD)
#endif
  -- (no master clock in the CPU build's finishing step, as in a pace stall)
  runF = (\r s -> r && not (stFinish s)) <$> run <*> st
  -- memory ops: the core's (no writes frozen), or the PS's port
  wramA = (\p o -> case p of
             Just (PWram a _) -> a
             _ -> boWram o) <$> port <*> bo
  wramW = (\f p o -> case p of
             Just (PWram a w) -> fmap (a,) w
             _ | f -> Nothing
               | otherwise -> fmap (boWram o,) (boWramW o)) <$> frz <*> port <*> bo
  wramD = blockRam1 NoClearOnReset (SNat @131072) 0x55 wramA wramW
  (sramD, sramB) = tdpRam ((\f o -> sramOp f o) <$> frz <*> bo) psSram
  sramOp f o = case boSramW o of
    Just v | not f -> RamWrite (fromIntegral (boSram o)) v
    _ -> RamRead (fromIntegral (boSram o))
  dmaD = bundle (map dmaReg indicesI)
  dmaReg :: Index 16 -> Signal dom (Unsigned 8)
  dmaReg k = blockRam (replicate d8 0xFF)
    ((\p o -> case p of
        Just (PDma _ ch _) -> ch
        _ -> boDmaRd o) <$> port <*> bo)
    ((\f p o -> case p of
        Just (PDma r ch w) | r == k -> fmap (ch,) w
        Just _ -> Nothing
        _ | f -> Nothing
          | otherwise -> fmap (boDmaWCh o,) (boDmaW o !! k)) <$> frz <*> port <*> bo)
  -- the frame count: line 0 begun
  frame' = (\c o b -> if boLineEnd o && bVc b == 0 then crFrame c + 1 else crFrame c) <$> cs <*> bo <*> bus'
  -- the ROM port
  -- (the bus's ROM read registered first, 'crRd': the 5A22's step and the
  -- ROM port's in one clock were the Arty's worst path at step 7.12,
  -- -0.456 ns; a read the ROM port answers costs a pixel clock more)
#ifdef SNES_DSP1
  -- (the DSP-1's load reads through the ROM port first)
  (rp', ro) = unbundle (romPortStep <$> (crRp <$> cs) <*> (RomIn <$> ((\c o -> if dspLoaded (crDsp c) then crRd c else doRomReq o) <$> cs <*> dOut) <*> ddrAnswer))
#else
  (rp', ro) = unbundle (romPortStep <$> (crRp <$> cs) <*> (RomIn <$> (crRd <$> cs) <*> ddrAnswer))
#endif
  -- the PPU and its memories
  (ppu', po, ppuR) = ppuUnit (crPpu <$> cs) pinFull (lPpu <$> ins)
  pinFull = (\i o -> (lPpu i) { piTick = boTick o, piAccess = boPpu o }) <$> ins <*> bo
  pinMem = PpuIn False Nothing <$> vA <*> vB <*> cgA <*> cgB <*> oamA <*> oamB <*> objs <*> ringE <*> ringW <*> ringH
  (vAlo, vBlo) = tdpRam (vramRead 0 <$> po) (vramOp False <$> frz <*> port <*> po)
  (vAhi, vBhi) = tdpRam (vramRead 1 <$> po) (vramOp True <$> frz <*> port <*> po)
  vA = (\a b -> a :> b :> Nil) <$> vAlo <*> vAhi
  vB = (\a b -> a :> b :> Nil) <$> vBlo <*> vBhi
  vramRead :: Index 2 -> PpuOut -> RamOp 32768 (Unsigned 8)
  vramRead k o = RamRead (bitCoerce (poVramA o !! k))
  vramOp hiChip f p o = case p of
    Just (PVram h a w) | h == hiChip -> maybe (RamRead (bitCoerce a)) (RamWrite (bitCoerce a)) w
    Just _ -> RamRead 0
    _ -> case poVramW o of
      Just (h, v) | h == hiChip && not f -> RamWrite (bitCoerce (poVramB o)) v
      _ -> RamRead (bitCoerce (poVramB o))
  (cgA, cgB) = tdpRam (cgRead <$> po) (cgOp <$> frz <*> port <*> po)
  cgRead :: PpuOut -> RamOp 256 (Unsigned 15)
  cgRead o = RamRead (bitCoerce (poCgramA o))
  cgOp f p o = case p of
    Just (PCgram a w) -> maybe (RamRead (bitCoerce a)) (RamWrite (bitCoerce a)) w
    Just _ -> RamRead 0
    _ -> case poCgramW o of
      Just v | not f -> RamWrite (bitCoerce (poCgramB o)) v
      _ -> RamRead (bitCoerce (poCgramB o))
  lanes = map lane indicesI
  lane :: Index 4 -> (Signal dom (Unsigned 8), Signal dom (Unsigned 8))
  lane l = tdpRam (oamRead <$> po) (oamOp l <$> frz <*> port <*> po)
  oamRead :: PpuOut -> RamOp 128 (Unsigned 8)
  oamRead o = RamRead (bitCoerce (poOamA o))
  oamOp l f p o = case p of
    Just (POam k a w) | k == l -> maybe (RamRead (bitCoerce a)) (RamWrite (bitCoerce a)) w
    Just _ -> RamRead 0
    _ -> case poOamW o !! l of
      Just v | not f -> RamWrite (bitCoerce (poOamB o)) v
      _ -> RamRead (bitCoerce (poOamB o))
  oamA = bundle (map fst lanes)
  oamB = bundle (map snd lanes)
  -- (the line buffer and the column ring: not in the state space; frozen,
  -- not written. A freeze in V-blank finds them out of use, rewritten
  -- before the next line shows: the save states' tests check it)
  noW :: Signal dom (Maybe b) -> Signal dom (Maybe b)
  noW = gate
  objs = bundle (map bank indicesI)
  bank :: Index 8 -> Signal dom (Unsigned 9)
  bank b = blockRam (replicate d32 0) (poObjA <$> po) (noW ((\o -> poObjW o !! b) <$> po))
  ringE = bundle (map entries indicesI)
  entries :: Index 4 -> Signal dom (Unsigned 16)
  entries n = blockRam (replicate d8 0) ((\o -> poRingA o !! n) <$> po) (noW (entryW n <$> po))
  entryW n o = case poRingWE o of
    Just (k, j, v) | k == n -> Just (j, v)
    _ -> Nothing
  ringW = bundle (map (\n -> bundle (map (charWords n) indicesI)) indicesI)
  charWords :: Index 4 -> Index 4 -> Signal dom (Unsigned 16)
  charWords n w = blockRam (replicate d8 0) ((\o -> poRingA o !! n) <$> po) (noW (wordW n w <$> po))
  wordW n w o = case poRingWW o of
    Just (k, l, j, v) | k == n && l == w -> Just (j, v)
    _ -> Nothing
  ringH = blockRam (replicate d32 (RingH 0 0 0)) (poRingH <$> po) (noW (poRingWH <$> po))
  -- the sound module and its memories
  (apu', ao, apuSt) = apuUnit (crApu <$> cs) ain
  ain = (\o i -> ApuIn (boTick o) (boApu o) (lAram i) (lDsp i) (lIpl i) (lSmp i) (lGauss i)) <$> bo <*> ins
  -- ARAM: the SPC700's port (the PS's, frozen) and the DSP's
  (aramD, aramDsp) = tdpRam (aramOp <$> frzA <*> port <*> ao) (ramOp aoDAram aoDAramW <$> frzA <*> ao)
  aramOp f p o = case p of
    Just (PAram a w) -> maybe (RamRead (fromIntegral a)) (RamWrite (fromIntegral a)) w
    Just _ -> RamRead 0
    _ -> ramOp aoAram aoAramW f o
  ramOp :: (ApuOut -> Unsigned 16) -> (ApuOut -> Maybe (Unsigned 8)) -> Bool -> ApuOut -> RamOp 65536 (Unsigned 8)
  ramOp addr w f o = case w o of
    Just v | not f -> RamWrite (fromIntegral (addr o)) v
    _ -> RamRead (fromIntegral (addr o))
  iplD = unpack <$> blockRamFile (SNat @64) iplImage (aoIpl <$> ao) (pure Nothing)
  smpD = blockRam (replicate d256 0)
    ((\p o -> case p of
        Just (PSmp a _) -> a
        _ -> aoSmpRd o) <$> port <*> ao)
    ((\f p o -> case p of
        Just (PSmp a w) -> fmap (a,) w
        Just _ -> Nothing
        _ | f -> Nothing
          | otherwise -> aoSmpW o) <$> frzA <*> port <*> ao)
  gaussD = blockRam gaussTable (aoGauss <$> ao) (pure Nothing)
  -- the PS's port reads: the memory it asked for last clock
  lastPort = register Nothing port
  portRd = (\p w v cg oa dm ar sm c -> case p of
              Just (PWram _ _) -> resize (pack w)
              Just (PVram h _ _) -> resize (pack (if h then v !! (1 :: Int) else v !! (0 :: Int)))
              Just (PCgram _ _) -> resize (pack cg)
              Just (POam k _ _) -> resize (pack (oa !! k))
              Just (PDma r _ _) -> resize (pack (dm !! r))
              Just (PAram _ _) -> resize (pack ar)
              Just (PSmp _ _) -> pack sm
              -- (in both builds: the direct build's hooks never ask, but
              -- answering 0 there made its bitstream 3,600 LUTs bigger,
              -- 40,867 against 37,256, 2026-11-02)
              Just (PTable n) -> resize (tableByte (crBus c) (crPpu c) n)
              Just (POvl _ _) -> 0
              Nothing -> 0) <$> lastPort <*> wramD <*> vB <*> cgB <*> oamB <*> dmaD <*> aramD <*> smpD <*> cs

-- With SNES_HIER defined (@make snes-ooc SNES_OOC=Snes.MachineOoc
-- SNES_OOC_TOP=snes_machine_ooc SNES_OOC_MODE=hier@ defines it), the three
-- state machines' steps are components of their own in the Verilog
-- (NOINLINE), so an out-of-context synthesis that keeps the hierarchy
-- measures each. Otherwise they are inlined, as before: each component's
-- outputs are one packed vector, so the sound module's status (from its
-- state) and its outputs (from the 5A22's access) look like a loop through
-- the 5A22 to Verilator, whose cycle breaker crashed on the DSP's
-- multiplier. (Their memories and, since 7.14, their registers ('CoreRegs')
-- stay in the core's body: with the PPU's memories inside its component,
-- Clash 1.8.5's normaliser failed on the RAM ports' addresses.)

-- | The 5A22's step: its next state, its outputs.
busUnit :: Signal dom BusC -> Signal dom BusIn -> (Signal dom BusC, Signal dom BusOut)
busUnit busS bin = unbundle (busClock <$> busS <*> bin)
#ifdef SNES_HIER
{-# NOINLINE busUnit #-}
#endif

-- | The PPU's step: its next state, its outputs, and its read value for the
-- 5A22 from its state and its memories' outputs ('ppuRead').
ppuUnit :: Signal dom PpuC -> Signal dom PpuIn -> Signal dom PpuIn -> (Signal dom PpuC, Signal dom PpuOut, Signal dom (Maybe (Maybe (Unsigned 8))))
ppuUnit ppuS pin pinMem = (ppuS', po, ppuRead <$> ppuS <*> pinMem)
 where
  (ppuS', po) = unbundle (ppuClock <$> ppuS <*> pin)
#ifdef SNES_HIER
{-# NOINLINE ppuUnit #-}
#endif

-- | The sound module's step (the SPC700 and the S-DSP): its next state, its
-- outputs, its status for the 5A22 ('apuStatus').
apuUnit :: Signal dom ApuC -> Signal dom ApuIn -> (Signal dom ApuC, Signal dom ApuOut, Signal dom (Bool, Bool, Unsigned 8))
apuUnit apuS ain = (apuS', ao, apuStatus <$> apuS)
 where
  (apuS', ao) = unbundle (apuClock <$> apuS <*> ain)
#ifdef SNES_HIER
{-# NOINLINE apuUnit #-}
#endif

#ifdef SNES_DSP1
-- | The DSP-1's step (a component of its own with SNES_HIER, as the three
-- above).
dspUnit :: Signal dom DspRegs -> Signal dom DspIn -> (Signal dom DspRegs, Signal dom DspOut)
dspUnit s i = unbundle (dspClock <$> s <*> i)
#ifdef SNES_HIER
{-# NOINLINE dspUnit #-}
#endif
#endif

-- | What the trace prints ("Snes.Machine", SNES_TRACE): the 65C816's
-- PBR, PC (high, low), P; NMITIMEN; the ports the CPU wrote (0-3) and
-- the SPC700 wrote (0-3); the SPC700's PC (high, low); and a flags byte:
-- bits 7-6 the SPC700's phase (0 between instructions, 1 a cycle's start,
-- 2 held, 3 completing), bit 5 a DSP sample in work, bit 3 the 5A22 in a
-- DMA or HDMA step (not the CPU's), bits 2-0 the DSP's queued writes.
debugBytes :: BusC -> ApuC -> Vec 24 (Unsigned 8)
debugBytes b a =
  let cpu = bCpu b
      spc = S.cpuRegs (aCpu a)
      ph = case aPhase a of
        ABoundary -> 0
        AStart -> 1
        AAccess -> 2
        AComplete -> 3 :: Unsigned 8
      flags = (ph `shiftL` 6) .|. (if aBusy a then 0x20 else 0) .|. (if bStep b /= BgCpu then 0x08 else 0)
              .|. fromIntegral (fromEnum (aRegN a))
  in cPBR cpu :> resize (cPC cpu `shiftR` 8) :> resize (cPC cpu) :> cP cpu :> bNmitimen b
     :> (aIn a ++ aOut a) ++ (resize (S.crPC spc `shiftR` 8) :> resize (S.crPC spc) :> flags :> Nil)
     ++ brk
 where
  -- the first BRK or COP fetched ('bBrk'): caught (1), the opcode before
  -- it (bank, high, low), its address, the byte
  brk = case bBrk b of
    Nothing -> repeat 0
    Just (prev, addr, v) -> 1 :> bytes3 prev ++ bytes3 addr ++ (v :> Nil)
  bytes3 x = resize (x `shiftR` 16) :> resize (x `shiftR` 8) :> resize x :> Nil
