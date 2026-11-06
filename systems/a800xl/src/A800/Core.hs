{-# LANGUAGE StrictData #-}
{-|
The whole 800XL in Clash (M4 step 4.6): the 6502C ("Mos6502.Core" with
'coreSally'), ANTIC ("A800.Antic"), GTIA ("A800.Gtia"), POKEY
("A800.Pokey"), the PIA, the memory map with PORTB banking, and the XEX
loader's page and port. It reproduces the reference model
("A800.Spec.Machine") machine cycle for machine cycle; that module has
the rules.

__Structure.__ 'coreStep' is a pure function run once per clock. Every
memory is outside it: a request goes out in 'koMem', its byte comes back
in the next clock's 'kiRd'. So the same function runs in the Haskell
lockstep harness (memories in vectors) and in hardware (block RAMs).

__A machine cycle__ is a sequence of clocks ('Seq'), in the spec's order:
the start (POKEY's tick, ANTIC's 'ABegin', its player/missile data to
GTIA), ANTIC's fetch, the rest of its special cycle and maybe a playfield
fetch; if the cycle is the CPU's: WSYNC's question before a read, the
CPU's access, its 'cpuCycle' (DMA cycles halt the CPU: it doesn't see
them at all, as the 6502C's HALT); then ANTIC's end (virtual fetches,
the display's two decode steps) and GTIA's two colour clocks. A cycle
takes at most 13 clocks; 'kiCycle' marks that a machine cycle of real
time has passed (a credit of up to 7 is kept).

__Memories__ ('Space'): RAM (64 KiB), the OS ROM (16 KiB, its $1000-$17FF
also the self-test at $5000), BASIC (8 KiB), a cartridge (8 or 16 KiB, or
an XEGS of 32 to 128 KiB: its bank register written at $D500-$D5FF, the
spec's 'A800.Media.cartBanks'),
the loader page (256 bytes) and the executable file (the loader port
reads it byte by byte). The I/O area and the stub cartridge's bytes are
answered inside.

__A slow memory__ (the cartridge from DDR on the Arty, side track Q): a
byte may come later than the next clock; 'kiRdOk' says when it is there.
The states that take a memory byte (ANTIC's fetches, the CPU's read)
wait for it, and the credit keeps counting real time meanwhile, so the
machine catches up (a DDR miss is far shorter than the 7 cycles of
credit).
-}
module A800.Core
  ( -- * Memories
    Space (..)
  , MemReq (..)
  , CartKind (..)
  , CoreCfg (..)
    -- * The core
  , CoreIn (..)
  , CoreOut (..)
  , CycleEvent (..)
  , Seq (..)
  , CoreState (..)
  , coreInit
  , coreStep
    -- * Save states (side track N)
  , coreIrqPia
    -- * In hardware
  , a800Core
  , a800CoreWith
  , Mems (..)
  , blockMems
  , cfgWord
  , cfgOfWord
  ) where

import Clash.Prelude
import Data.Maybe (isJust)

import A800.Antic
import A800.Gtia
import A800.Pokey
import Mos6502.Core
import A800.CasUnit (casUnit)

-- ('SpCas': the tape's answer to the OS's cassette call, "A800.CasUnit")
data Space = SpRam | SpOs | SpBasic | SpCart | SpLoader | SpXex | SpCas
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFDataX

data MemReq = MemReq
  { mqSpace :: Space
  , mqAddr  :: Unsigned 24
  , mqWrite :: Maybe (Unsigned 8)
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFDataX

-- | None, 8 KiB at $A000, 16 KiB at $8000, or XEGS ('CkXegs' n: 4 << n
-- banks of 8 KiB, n 0 to 2: 32, 64, 128 KiB); or a tape in the recorder
-- ('CkCas': no cartridge, the media is the tape's image, "A800.CasUnit").
data CartKind = CkNone | Ck8 | Ck16 | CkXegs (Unsigned 2) | CkCas
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFDataX

-- | What is plugged in (held while running).
data CoreCfg = CoreCfg
  { ccBasic  :: Bool
  , ccCart   :: CartKind
  , ccXex    :: Bool          -- ^ an executable through the loader
  , ccXexLen :: Unsigned 24   -- ^ its length
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFDataX

data CoreIn = CoreIn
  { kiCycle   :: Bool          -- ^ a machine cycle of time has passed
  , kiRd      :: Unsigned 8    -- ^ the byte of the last memory read
  , kiRdOk    :: Bool          -- ^ that byte is there (a slow memory: wait while False)
  , kiJoy     :: Unsigned 8    -- ^ PIA port A: sticks (0 = pushed)
  , kiTrig    :: BitVector 2   -- ^ joystick triggers pressed
  , kiConsole :: BitVector 3   -- ^ START, SELECT, OPTION held
  , kiKeys    :: BitVector 64  -- ^ keys held (bit n = key code n)
  , kiMods    :: BitVector 3   -- ^ SHIFT, CONTROL, BREAK
  , kiOvl     :: Maybe (Unsigned 8)
    -- ^ the ROM overlay's byte at the CPU's address (side track N, "A800.Hooks"):
    -- while there, it answers every CPU read outside the I/O area
  , kiCasOn   :: Bool
    -- ^ the tape's answer is armed ("A800.CasUnit"): 'casRoute' sends it
    -- $E45A, $E45B and the $D5 page too
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFDataX

-- | One machine cycle, for the lockstep tests: its bus access (address,
-- data, write, by the CPU), the step of the CPU it ended, the registers
-- after it, POKEY's level after its tick, the beam and GTIA's colours.
data CycleEvent = CycleEvent
  { ceAddr    :: Unsigned 16
  , ceData    :: Unsigned 8
  , ceWrite   :: Bool
  , ceCpu     :: Bool
  , ceStep    :: Maybe StepKind
  , ceRegs    :: CpuRegs
  , ceLevel   :: Unsigned 10
  , ceY       :: Unsigned 9
  , ceX       :: Unsigned 7
  , ceColours :: ((Unsigned 8, Unsigned 8), (Unsigned 8, Unsigned 8))
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFDataX

data CoreOut = CoreOut
  { koMem    :: Maybe MemReq
  , koPixel  :: Maybe (Unsigned 9, Unsigned 8, (Unsigned 8, Unsigned 8)) -- ^ line, colour clock, its two halves
  , koFrame  :: Bool               -- ^ a frame ended (line 261 to 0)
  , koLevel  :: Unsigned 10        -- ^ POKEY's mixed channels
  , koEvent  :: Maybe CycleEvent
  , koIdle   :: Bool
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFDataX

data Seq = QIdle | QBegin | QFetch1 | QPost | QFetch2 | QRdy | QCpu | QCpuRd | QCpuDone
         | QEnd | QDec | QG1 | QG2
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFDataX

data CoreState = CoreState
  { ksCfg    :: CoreCfg
  , ksCpu    :: Cpu
  , ksAntic  :: Antic
  , ksGtia   :: Gtia
  , ksPokey  :: Pokey
  , ksSeq    :: Seq
  , ksCredit :: Unsigned 3
  , ksBus    :: Unsigned 8      -- ^ the data bus (last value on it)
  , ksUse    :: BusUse          -- ^ whose cycle this is
  , ksHeld   :: Bool            -- ^ WSYNC holds this cycle's read
  , ksFromMem :: Bool           -- ^ the pending byte comes from 'kiRd' (else 'ksTmp')
  , ksTmp    :: Unsigned 8
  , ksAccess :: (Unsigned 16, Unsigned 8, Bool, Bool)
  , ksStep   :: Maybe StepKind
  , ksCodes  :: (Unsigned 3, Unsigned 3)
  , ksCol1   :: (Unsigned 8, Unsigned 8)
  , ksPactl, ksPbctl, ksOra, ksDdra, ksOrb, ksDdrb :: Unsigned 8
  , ksEdgeA, ksEdgeB :: Bool
  , ksCb2    :: Unsigned 2      -- ^ CB2 as driven: 0 floating, 1 high, 2 low
  , ksXexPos :: Unsigned 24
  , ksStub   :: Bool
  , ksPage   :: Unsigned 2      -- ^ the loader page: 0 gone, 1 there, 2 going at the next read outside it
  , ksFrame  :: Bool
  , ksLevel  :: Unsigned 10     -- ^ POKEY's level after this cycle's tick
  , ksCartBank :: Unsigned 4    -- ^ an XEGS cartridge's bank register
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFDataX

-- | Power-on: the CPU starts its reset sequence (registers as the spec's
-- 'newMachine'), the chips as theirs.
coreInit :: CoreCfg -> CoreState
coreInit cfg = CoreState
  { ksCfg = cfg
  , ksCpu = cpuReset (cpuPowerOn (CpuRegs 0 0 0 0 0x34 0))
  , ksAntic = anticInit, ksGtia = gtiaInit, ksPokey = pokeyInit
  , ksSeq = QIdle, ksCredit = 0, ksBus = 0xFF, ksUse = BusCpu, ksHeld = False, ksFromMem = False, ksTmp = 0
  , ksAccess = (0, 0, False, False), ksStep = Nothing, ksCodes = (0, 0), ksCol1 = (0, 0)
  , ksPactl = 0, ksPbctl = 0, ksOra = 0, ksDdra = 0, ksOrb = 0, ksDdrb = 0
  , ksEdgeA = False, ksEdgeB = False, ksCb2 = 0
  , ksXexPos = 0, ksStub = ccXex cfg, ksPage = if ccXex cfg then 1 else 0, ksFrame = False, ksLevel = 0
  , ksCartBank = 0 }

nothing :: CoreState -> CoreOut
nothing s = CoreOut Nothing Nothing False (pokeyLevelOf (ksPokey s)) Nothing False

-- * The memory map

-- | Where a non-I/O address reads from.
data Src = SrcMem Space (Unsigned 24) | SrcConst (Unsigned 8)

-- | PORTB as driven: inputs pulled up.
portB :: CoreState -> Unsigned 8
portB s = (ksOrb s .&. ksDdrb s) .|. complement (ksDdrb s)

srcOf :: CoreState -> Unsigned 16 -> Src
srcOf s a
  | a >= 0x5000 && a < 0x5800 && osOn && not (testBit pb 7) = SrcMem SpOs (resize (a - 0x5000 + 0x1000))
  | a >= 0x8000 && a < 0xC000 && cart = case ccCart cfg of
      Ck16 -> SrcMem SpCart (resize (a - 0x8000))
      -- XEGS: the bank at $8000, the last one at $A000
      CkXegs n
        | a < 0xA000 -> SrcMem SpCart ((resize (ksCartBank s .&. bankMask n) `shiftL` 13) .|. resize (a - 0x8000))
        | otherwise -> SrcMem SpCart ((resize (bankMask n) `shiftL` 13) .|. resize (a - 0xA000))
      _ -> SrcMem SpCart (resize (a - 0xA000))
  -- (a tape is no cartridge: the stub with an executable, cassio.s)
  | a >= 0xA000 && a < 0xC000 && (ccCart cfg == CkNone || ccCart cfg == CkCas) && ksStub s = SrcConst (stubCart a)
  | a >= 0xA000 && a < 0xC000 && not (testBit pb 1) && ccBasic cfg = SrcMem SpBasic (resize (a - 0xA000))
  | a >= 0xC000 && osOn && (a < 0xD000 || a >= 0xD800) = SrcMem SpOs (resize (a - 0xC000))
  | otherwise = SrcMem SpRam (resize a)
 where
  cfg = ksCfg s
  pb = portB s
  osOn = testBit pb 0
  cart = case ccCart cfg of
    CkNone -> False
    Ck8 -> a >= 0xA000
    Ck16 -> True
    CkXegs _ -> True
    CkCas -> False

-- | An XEGS cartridge's banks - 1 (4 << n banks).
bankMask :: Unsigned 2 -> Unsigned 4
bankMask n = (4 `shiftL` fromIntegral n) - 1

-- | The stub cartridge that starts the XEX loader: $FF but its header.
stubCart :: Unsigned 16 -> Unsigned 8
stubCart a = case a of
  0xBFFA -> 0x00
  0xBFFB -> 0xD5
  0xBFFC -> 0x00
  0xBFFD -> 0x04
  0xBFFE -> 0xEF
  0xBFFF -> 0xD5
  _ -> 0xFF

isIo :: Unsigned 16 -> Bool
isIo a = a >= 0xD000 && a < 0xD800

romWritable :: CoreState -> Unsigned 16 -> Bool
romWritable s a = case srcOf s a of
  SrcMem SpRam _ -> True
  _ -> False

-- | Ask for a byte from a source: a memory request, or the byte now.
ask :: CoreState -> Src -> (CoreState, Maybe MemReq)
ask s src = case src of
  SrcMem sp addr -> (s { ksFromMem = True }, Just (MemReq sp addr Nothing))
  SrcConst v -> (s { ksFromMem = False, ksTmp = v }, Nothing)

pending :: CoreState -> CoreIn -> Unsigned 8
pending s inp = if ksFromMem s then kiRd inp else ksTmp s

-- * The PIA

piaIrq :: CoreState -> Bool
piaIrq s = on (ksPactl s) || on (ksPbctl s)
 where on c = c .&. 0x68 == 0x48 || c .&. 0x81 == 0x81

-- | The PIA's IRQ output (for "A800.StateTap"'s rest condition).
coreIrqPia :: CoreState -> Bool
coreIrqPia = piaIrq

piaRead :: CoreState -> CoreIn -> Unsigned 2 -> (CoreState, Unsigned 8)
piaRead s inp r = case r of
  0 | testBit (ksPactl s) 2 -> (s { ksPactl = ksPactl s .&. 0x3F }, kiJoy inp .&. (ksOra s .|. complement (ksDdra s)))
    | otherwise -> (s, ksDdra s)
  1 | testBit (ksPbctl s) 2 ->
        let o = ksOrb s
            d = ksDdrb s
        in (s { ksPbctl = ksPbctl s .&. 0x3F }, (o .&. d) .|. (complement d .&. (0x83 .|. o)))
    | otherwise -> (s, ksDdrb s)
  2 -> (s, ksPactl s)
  _ -> (s, ksPbctl s)

piaWrite :: Unsigned 2 -> Unsigned 8 -> CoreState -> CoreState
piaWrite r v s = case r of
  0 -> if testBit (ksPactl s) 2 then s { ksOra = v } else s { ksDdra = v }
  1 -> if testBit (ksPbctl s) 2 then s { ksOrb = v } else s { ksDdrb = v }
  2 ->
    let m = v .&. 0x38
        (edge, flagUp)
          | m == 0x30 = (True, False)
          | m == 0x10 || m == 0x18 = (False, ksEdgeA s)
          | m == 0x20 || m == 0x38 = (ksEdgeA s, False)
          | otherwise = (False, False)
        c = if flagUp then ksPactl s .|. 0x40 else ksPactl s
        c' = if testBit v 5 then c .&. complement 0x40 else c
    in s { ksEdgeA = edge, ksPactl = (c' .&. 0xC0) .|. (v .&. 0x3F) }
  _ ->
    let m = v .&. 0x38
        (edge, flagUp, cb2)
          | m < 0x20 = (False, ksEdgeB s, 0)
          | m == 0x20 = (False, False, ksCb2 s)
          | m == 0x28 = (False, False, 1)
          | m == 0x30 = (False, False, 2)
          | otherwise = (ksEdgeB s || ksCb2 s == 2, False, 1)
        c = if flagUp then ksPbctl s .|. 0x40 else ksPbctl s
        c' = if testBit v 5 then c .&. complement 0x40 else c
    in s { ksEdgeB = edge, ksCb2 = cb2, ksPbctl = (c' .&. 0xC0) .|. (v .&. 0x3F) }

-- * The step

coreStep :: CoreState -> CoreIn -> (CoreState, CoreOut)
coreStep s0 inp = case ksSeq s of
  QIdle
    | ksCredit s > 0 ->
        -- the inputs, then the cycle starts next clock
        let (p, _) = pokeyStep (ksPokey s) (PKeys (kiKeys inp) (kiMods inp))
            (g, _) = gtiaStep (ksGtia s) (GInputs (kiTrig inp) (kiConsole inp))
        in (s { ksPokey = p, ksGtia = g, ksSeq = QBegin, ksCredit = ksCredit s - 1, ksStep = Nothing }, (nothing s) { koIdle = True })
    | otherwise -> (s, (nothing s) { koIdle = True })
  QBegin ->
    let (p, _) = pokeyStep (ksPokey s) PTick
        (a, o) = anticStep (ksAntic s) (ABegin (ksBus s))
        s1 = pm o s { ksPokey = p, ksLevel = pokeyLevelOf p, ksAntic = a, ksUse = aoBus o, ksHeld = False, ksFrame = aoY o == 0 && aoX o == 0 }
    in case aoReq o of
         Just addr -> let (s2, rq) = ask s1 (dmaSrc s1 addr) in (s2 { ksSeq = QFetch1, ksAccess = (addr, 0, False, False) }, (nothing s) { koMem = rq })
         Nothing -> (s1 { ksSeq = QPost }, nothing s)
  QFetch1 | waiting -> (s, nothing s)
          | otherwise -> (fetched QPost, nothing s)
  QPost ->
    let (a, o) = anticStep (ksAntic s) APost
        s1 = pm o s { ksAntic = a }
        after = afterAntic s1
    in case aoReq o of
         Just addr -> let (s2, rq) = ask s1 (dmaSrc s1 addr) in (s2 { ksSeq = QFetch2, ksAccess = (addr, 0, False, False) }, (nothing s) { koMem = rq })
         Nothing -> (after, nothing s)
  QFetch2 | waiting -> (s, nothing s)
          | otherwise -> (afterAntic (fetched QFetch2), nothing s)
  QRdy ->
    let req = cpuRequest (ksCpu s)
    in if rqWrite req || cPhase (ksCpu s) == PJam then (s { ksSeq = QCpu }, nothing s) else
         let (a, o) = anticStep (ksAntic s) ARdy
         in (s { ksAntic = a, ksHeld = not (aoRdy o), ksSeq = QCpu }, nothing s)
  QCpu ->
    let req = cpuRequest (ksCpu s)
        a = rqAddr req
        wrReq = if not (isIo a) && romWritable s a then Just (MemReq SpRam (resize a) (Just (rqData req))) else Nothing
    in if rqWrite req then (cpuWrite s a (rqData req), (nothing s) { koMem = wrReq }) else cpuRead s inp a
  QCpuRd | waiting -> (s, nothing s)
  QCpuRd ->
    let v = pending s inp
        (a, _, w, c) = ksAccess s
    in (s { ksBus = v, ksTmp = v, ksAccess = (a, v, w, c), ksSeq = QCpuDone }, nothing s)
  QCpuDone ->
    let cin = CpuIn (ksTmp s) (ksHeld s) (sNmiLine (ksAntic s)) (pIrqLine (ksPokey s) || piaIrq s)
        (cpu', co) = cpuCycle coreSally (ksCpu s) cin
    in (s { ksCpu = cpu', ksStep = coStep co, ksSeq = QEnd }, nothing s)
  QEnd ->
    let (a, _) = anticStep (ksAntic s) (AEnd (ksBus s))
    in (s { ksAntic = a, ksSeq = QDec }, nothing s)
  QDec ->
    let (a, o) = anticStep (ksAntic s) ADecode
    in (s { ksAntic = a, ksCodes = aoCodes o, ksSeq = QG1 }, nothing s)
  QG1 ->
    let (c1, c2) = ksCodes s
        x = sX (ksAntic s)
        pair = (resize (c1 .&. 3) `shiftL` 2) .|. resize (c2 .&. 3)
        (g, o) = gtiaStep (ksGtia s) (GClock (resize x `shiftL` 1) c1 pair (gPrevPair (ksGtia s)))
    in (s { ksGtia = g, ksCol1 = goColour o, ksSeq = QG2 }, (nothing s) { koPixel = Just (sY (ksAntic s), resize x `shiftL` 1, goColour o) })
  QG2 ->
    let (c1, c2) = ksCodes s
        an = ksAntic s
        x = sX an
        pair = (resize (c1 .&. 3) `shiftL` 2) .|. resize (c2 .&. 3)
        (g, o) = gtiaStep (ksGtia s) (GClock ((resize x `shiftL` 1) + 1) c2 pair pair)
        (ad, d, w, c) = ksAccess s
        ev = CycleEvent ad d w c (ksStep s) (cpuRegs (ksCpu s)) (ksLevel s) (sY an) x (ksCol1 s, goColour o)
    in ( s { ksGtia = g { gPrevPair = pair }, ksSeq = QIdle }
       , (nothing s) { koPixel = Just (sY an, (resize x `shiftL` 1) + 1, goColour o), koEvent = Just ev, koFrame = ksFrame s } )
 where
  credit = if kiCycle inp && ksCredit s0 < 7 then ksCredit s0 + 1 else ksCredit s0
  s = s0 { ksCredit = credit }
  -- a memory byte to take that isn't there yet (a slow memory)
  waiting = ksFromMem s && not (kiRdOk inp)
  -- player/missile data from ANTIC to GTIA
  pm o q = case aoPm o of
    PmNone -> q
    PmMissile od v -> q { ksGtia = fst (gtiaStep (ksGtia q) (GMissile (aoX o) od v)) }
    PmPlayer od n v -> q { ksGtia = fst (gtiaStep (ksGtia q) (GPlayer (aoX o) od n v)) }
  -- a DMA byte arrived: ANTIC takes it, it is on the bus
  fetched nxt =
    let v = pending s inp
        (a, _) = anticStep (ksAntic s) (AFetch v)
        (ad, _, _, _) = ksAccess s
    in s { ksAntic = a, ksBus = v, ksAccess = (ad, v, False, False), ksSeq = nxt }
  -- after ANTIC's part of the cycle: the CPU's access, or a refresh
  afterAntic q = case ksUse q of
    BusCpu -> q { ksSeq = QRdy }
    BusRefresh -> q { ksSeq = QEnd, ksBus = 0xFF, ksAccess = (0, 0xFF, False, False) }
    BusFetch -> q { ksSeq = QEnd }
  -- ANTIC reads memory as mapped; the I/O area reads $FF
  dmaSrc q addr = if isIo addr then SrcConst 0xFF else srcOf q addr

-- | A tape in ('CkCas'): the CPU's reads the tape's answer takes
-- ("A800.CasUnit", as the spec's "A800.Spec.Machine.casRead"): $E459,
-- SIOV, with the OS mapped; while it is armed, $E45A, $E45B and the $D5
-- page.
casRoute :: CoreState -> CoreIn -> Unsigned 16 -> Bool
casRoute s inp a =
  ccCart (ksCfg s) == CkCas
    && ((a == 0xE459 && testBit (portB s) 0)
        || (kiCasOn inp && (a == 0xE45A || a == 0xE45B || a `shiftR` 8 == 0xD5)))

-- | The CPU's read: I/O answered now, memory next clock.
cpuRead :: CoreState -> CoreIn -> Unsigned 16 -> (CoreState, CoreOut)
cpuRead s0 inp a
  | casRoute s inp a = mem s (MemReq SpCas (resize a) Nothing)
  | isIo a = case a `shiftR` 8 of
      0xD0 -> let (g, o) = gtiaStep (ksGtia s) (GRead (resize a) rd5) in now s { ksGtia = g } (goRead o)
      0xD2 -> let (p, o) = pokeyStep (ksPokey s) (PRead (resize a)) in now s { ksPokey = p } (poRead o)
      0xD3 -> let (s', v) = piaRead s inp (resize a) in now s' v
      0xD4 -> let (an, o) = anticStep (ksAntic s) (ARead (resize a)) in now s { ksAntic = an } (aoRead o)
      0xD5 | ccXex (ksCfg s) && ksPage s /= 0 -> case resize a :: Unsigned 8 of
               0xFE | ksXexPos s < ccXexLen (ksCfg s) ->
                        mem s { ksXexPos = ksXexPos s + 1 } (MemReq SpXex (ksXexPos s) Nothing)
                    | otherwise -> now s { ksXexPos = satSucc SatBound (ksXexPos s) } 0xFF
               0xFF -> now s (if ksXexPos s >= ccXexLen (ksCfg s) then 0xFF else 0x7F)
               lo -> mem s (MemReq SpLoader (resize lo) Nothing)
      _ -> now s 0xFF
  | Just v <- kiOvl inp = now s v
  | otherwise = case srcOf s a of
      SrcMem sp addr -> mem s (MemReq sp addr Nothing)
      SrcConst v -> now s v
 where
  -- the loader page goes at the first read outside it
  s = (if ksPage s0 == 2 && a `shiftR` 8 /= 0xD5 then s0 { ksPage = 0 } else s0) { ksAccess = (a, 0, False, True) }
  rd5 = (ccCart (ksCfg s) /= CkNone && ccCart (ksCfg s) /= CkCas) || ksStub s
  now q v = (q { ksBus = v, ksTmp = v, ksAccess = (a, v, False, True), ksSeq = QCpuDone }, nothing q)
  mem q rq = (q { ksFromMem = True, ksSeq = QCpuRd }, (nothing q) { koMem = Just rq })

-- | The CPU's write (memory writes go out now).
cpuWrite :: CoreState -> Unsigned 16 -> Unsigned 8 -> CoreState
cpuWrite s0 a v = s1 { ksBus = v, ksAccess = (a, v, True, True), ksSeq = QCpuDone, ksTmp = 0 }
 where
  s1
    | isIo a = case a `shiftR` 8 of
        0xD0 -> s0 { ksGtia = fst (gtiaStep (ksGtia s0) (GWrite (sX (ksAntic s0)) (resize a) v)) }
        0xD2 -> s0 { ksPokey = fst (pokeyStep (ksPokey s0) (PWrite (resize a) v)) }
        0xD3 -> piaWrite (resize a) v s0
        0xD4 -> s0 { ksAntic = fst (anticStep (ksAntic s0) (AWrite (resize a) v)) }
        0xD5 | ccXex (ksCfg s0) && ksPage s0 /= 0 -> case resize a :: Unsigned 8 of
                 0xFE -> s0 { ksStub = False }
                 0xFF -> s0 { ksPage = 2 }
                 _ -> s0
             -- an XEGS cartridge's bank register: the value AND (banks - 1)
             | CkXegs n <- ccCart (ksCfg s0) -> s0 { ksCartBank = resize v .&. bankMask n }
        _ -> s0
    | otherwise = s0

-- * In hardware

-- | The configuration as a 32-bit word (for an image file): bit 0 BASIC,
-- bits 1-2 the cartridge (0 none, 1 8 KiB, 2 16 KiB, 3 XEGS), bit 3 an
-- executable, bits 4-5 an XEGS's size (4 << n banks), bit 6 a tape
-- ('CkCas'), bits 8-31 the executable's length.
cfgWord :: CoreCfg -> BitVector 32
cfgWord c = pack (ccXexLen c) ++# (0 :: BitVector 1) ++# pack (ccCart c == CkCas) ++# xn ++# pack (ccXex c)
            ++# (case ccCart c of CkNone -> 0; Ck8 -> 1; Ck16 -> 2; CkXegs _ -> 3; CkCas -> 0 :: BitVector 2) ++# pack (ccBasic c)
 where
  xn = case ccCart c of CkXegs n -> pack n; _ -> 0

cfgOfWord :: BitVector 32 -> CoreCfg
cfgOfWord w = CoreCfg
  { ccBasic = testBit w 0
  , ccCart = if testBit w 6 then CkCas else case slice d2 d1 w of
      1 -> Ck8
      2 -> Ck16
      3 -> CkXegs (unpack (slice d5 d4 w))
      _ -> CkNone
  , ccXex = testBit w 3
  , ccXexLen = unpack (slice d31 d8 w) }

-- | The core with its memories in block RAM: RAM (64 KiB, cleared), the
-- OS, BASIC, a cartridge, the loader page and an executable (up to 32
-- KiB) from image files (binary text, one byte per line), the last two
-- also written by the loader ("A800.Loader", step 4.8). Each space's byte is held until
-- its next request, as 'CoreIn' expects.
a800Core
  :: HiddenClockResetEnable dom
  => Signal dom CoreCfg
  -> (FilePath, FilePath, FilePath, FilePath, FilePath) -- ^ OS (16 KiB), BASIC (8 KiB), cartridge (16 KiB), loader (256 B), executable (64 KiB)
  -> Signal dom (Maybe (Unsigned 14, BitVector 8)) -- ^ cartridge RAM write (the loader, while held)
  -> Signal dom (Maybe (Unsigned 16, BitVector 8)) -- ^ executable RAM write (the loader, while held)
  -> Signal dom Bool                         -- ^ run (low: held at power-on with the configuration given; hold it until that is valid)
  -> Signal dom Bool                         -- ^ a machine cycle of time has passed
  -> Signal dom (Unsigned 8)                 -- ^ joysticks (PIA port A)
  -> Signal dom (BitVector 2)                -- ^ triggers
  -> Signal dom (BitVector 3)                -- ^ console keys
  -> Signal dom (BitVector 64)               -- ^ keys
  -> Signal dom (BitVector 3)                -- ^ SHIFT, CONTROL, BREAK
  -> Signal dom CoreOut
a800Core cfg files cartWr xexWr run tick joy trig consol keys mods = out
 where
  (out, _, _, _) = a800CoreWith (coreInit (CoreCfg False CkNone False 0)) (blockMems files cartWr xexWr) cfg run tick joy trig consol keys mods
                  (pure False) (pure Nothing) (pure Nothing)

-- | The core's memories: each space's byte for an address (a clock
-- later, as a block RAM's), RAM with its write. The cartridge may be slow
-- (DDR on the Arty, side track Q): it takes a read request (for one
-- clock) and gives its byte, held, and whether it is there yet ('kiRdOk').
data Mems dom = Mems
  { msRam    :: Signal dom (Unsigned 24) -> Signal dom (Maybe (Unsigned 24, Unsigned 8)) -> Signal dom (Unsigned 8)
  , msOs, msBasic, msLoader, msXex :: Signal dom (Unsigned 24) -> Signal dom (Unsigned 8)
  , msCart :: Signal dom (Maybe (Unsigned 24)) -> (Signal dom (Unsigned 8), Signal dom Bool)
  }

-- | The memories in block RAM: RAM (64 KiB, cleared), the OS, BASIC, a
-- cartridge, the loader page and an executable from image files (binary
-- text, one byte per line), the cartridge and the executable also written
-- by the loader ("A800.Loader", step 4.8).
blockMems
  :: HiddenClockResetEnable dom
  => (FilePath, FilePath, FilePath, FilePath, FilePath) -- ^ OS (16 KiB), BASIC (8 KiB), cartridge (16 KiB), loader (256 B), executable (64 KiB)
  -> Signal dom (Maybe (Unsigned 14, BitVector 8)) -- ^ cartridge RAM write (the loader, while held)
  -> Signal dom (Maybe (Unsigned 16, BitVector 8)) -- ^ executable RAM write (the loader, while held)
  -> Mems dom
blockMems (osF, basicF, cartF, loaderF, xexF) cartWr xexWr = Mems
  { -- blockRam1: the same block RAM, all zero at power-on, but Clash
    -- doesn't build the initial value as a 64 Ki-element literal (side
    -- track D: on the NES that cost Clash 18 minutes of normalisation)
    msRam = blockRam1 NoClearOnReset (SNat @65536) 0
  , msOs = memFile (SNat @16384) osF
  , msBasic = memFile (SNat @8192) basicF
  , msCart = \rq ->
      -- (in block RAM always there the next clock; the byte held until
      -- the next request)
      let byte = unpack <$> blockRamFile (SNat @16384) cartF (maybe 0 id <$> rq) (fmap (\(x, d) -> (resize x :: Unsigned 24, d)) <$> cartWr)
          was = register False (isJust <$> rq)
          held = mux was byte (register 0 held)
      in (held, pure True)
  , msLoader = memFile (SNat @256) loaderF
  , msXex = \a -> unpack <$> blockRamFile (SNat @65536) xexF a (fmap (\(x, d) -> (resize x :: Unsigned 24, d)) <$> xexWr)
  }

-- | 'a800Core' with the save states' hooks (side track N, "A800.Hooks"):
-- frozen (the core takes no step: its state stands still), the ROM
-- overlay's byte ('kiOvl'), and RAM's port taken over while frozen (an
-- address, and a byte to write there); also the core's state (as it is
-- this clock, before its step) and RAM's output; from a state given (at
-- power-on: 'coreInit'; a test may start mid-run). Each space's byte is
-- held until its next request, as 'CoreIn' expects.
a800CoreWith
  :: HiddenClockResetEnable dom
  => CoreState
  -> Mems dom
  -> Signal dom CoreCfg
  -> Signal dom Bool                         -- ^ run (low: held at power-on with the configuration given; hold it until that is valid)
  -> Signal dom Bool                         -- ^ a machine cycle of time has passed
  -> Signal dom (Unsigned 8)                 -- ^ joysticks (PIA port A)
  -> Signal dom (BitVector 2)                -- ^ triggers
  -> Signal dom (BitVector 3)                -- ^ console keys
  -> Signal dom (BitVector 64)               -- ^ keys
  -> Signal dom (BitVector 3)                -- ^ SHIFT, CONTROL, BREAK
  -> Signal dom Bool                                 -- ^ frozen
  -> Signal dom (Maybe (Unsigned 8))                 -- ^ the ROM overlay's byte
  -> Signal dom (Maybe (Unsigned 16, Maybe (Unsigned 8))) -- ^ RAM's port while frozen
  -> (Signal dom CoreOut, Signal dom CoreState, Signal dom (Unsigned 8), Signal dom (Maybe (Unsigned 24)))
a800CoreWith s0 mems cfg run tick joy trig consol keys mods frozen ovl ramPort = (out, st, ram, cartReq)
 where
  (out, st) = unbundle (mealy step s0 (bundle (cfg, run, frozen, inp)))
  -- (the state goes out before any guard is decided: the hooks' freeze,
  -- an input here, is computed from it)
  step s (c, r, f, i) = (next, (o, s))
   where
    (next, o)
      | not r = (coreInit c, (nothing s) { koIdle = True })
      | f = (s, (nothing s) { koIdle = True })
      | otherwise = coreStep s { ksCfg = c } i
  inp = CoreIn <$> tick <*> rdData <*> rdOk <*> joy <*> trig <*> consol <*> keys <*> mods <*> ovl <*> casOn
  -- the tape's answer beside the core (its reads 'SpCas'; the tape image
  -- through the cartridge's reader, which has no cartridge then)
  casReq = (\m -> case m of Just (MemReq SpCas a Nothing) -> Just (resize a); _ -> Nothing) <$> memReq
  casEv = (\e -> case e of
             Just c | ceCpu c && not (ceWrite c) -> Just (ceAddr c, ceData c)
             _ -> Nothing) <$> (koEvent <$> out)
  (casOn, casAns, casTapeRq) = casUnit run casReq casEv (bundle (cartByte, cartOk))
  memReq = koMem <$> out
  addrOf sp = (\m -> case m of Just (MemReq x a _) | x == sp -> a; _ -> 0) <$> memReq
  wantRd = (\m -> case m of Just (MemReq _ _ Nothing) -> True; _ -> False) <$> memReq
  lastSpace = regEn SpRam (isJust <$> memReq) (maybe SpRam mqSpace <$> memReq)
  -- (RAM's port takes RAM whenever it has an address: the hooks' only
  -- while frozen, the system's RAM clear while the core is held at
  -- power-on, with no requests of its own; so the clear needs no freeze,
  -- whose hold on every state bit cost 8,600 LUTs in a build without the
  -- hooks, where 'frozen' is a constant)
  ramWr = (\f p m -> case p of
             Just (a, Just d) -> Just (resize a, d)
             Just (_, Nothing) -> Nothing
             Nothing | f -> Nothing
                     | otherwise -> case m of Just (MemReq SpRam a (Just d)) -> Just (a, d); _ -> Nothing)
          <$> frozen <*> ramPort <*> memReq
  ramAddr = (\p a -> case p of Just (x, _) -> resize x; Nothing -> a) <$> ramPort <*> addrOf SpRam
  ram = msRam mems ramAddr ramWr
  cartReq = (\m t -> case m of Just (MemReq SpCart a Nothing) -> Just a; _ -> t) <$> memReq <*> casTapeRq
  (cartByte, cartOk) = msCart mems cartReq
  rdNow = (\sp a b c x w -> case sp of
              SpRam -> a; SpOs -> b; SpBasic -> c; SpCart -> 0; SpLoader -> x; SpXex -> w; SpCas -> 0)
            <$> lastSpace <*> ram <*> msOs mems (addrOf SpOs) <*> msBasic mems (addrOf SpBasic)
            <*> msLoader mems (addrOf SpLoader) <*> msXex mems (addrOf SpXex)
  wasRd = register False wantRd
  -- (the cartridge's byte is held by its memory, and may come late)
  isCart = (== SpCart) <$> lastSpace
  isCas = (== SpCas) <$> lastSpace
  rdData = mux isCas (fst <$> casAns) (mux isCart cartByte (mux wasRd rdNow (register 0 rdData)))
  rdOk = (\ca ct cok aok -> if ca then aok else not ct || cok) <$> isCas <*> isCart <*> cartOk <*> (snd <$> casAns)

memFile :: (HiddenClockResetEnable dom, KnownNat n) => SNat n -> FilePath -> Signal dom (Unsigned 24) -> Signal dom (Unsigned 8)
memFile n f a = unpack <$> blockRamFile n f a (pure Nothing)
