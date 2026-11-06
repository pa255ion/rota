{-# LANGUAGE StrictData #-}
{-|
The whole Game Gear in Clash (PLAN.md step 5.7): "Z80.Core", the memory
map with the Sega or Codemasters mapper, the ports, "Gg.Vdp" and "Gg.Psg",
as one function per clock ('ggClock'), in lockstep with "Gg.Spec"
(@gg-lockstep@: the CPU's registers and T-states after every
instruction, the frames, the PSG).

__Clocks.__ Two T-states are twelve active clocks ('giRun'; the platform
leaves the others out to keep the real-time rate): T-states end at
clocks 0 and 6, VDP dots start at clocks 0, 4 and 8 (1.5 dots per
T-state, four clocks each). The VDP runs two dots ahead from power-on
(the spec's dot phase: dot D is over before T = (D - 1) / 1.5).

__Where the CPU meets the VDP__, as in the spec: a VDP port access (or a
TH edge on port 3Fh, which latches the H counter) in an I/O cycle that
starts at T-state t reaches the VDP at the start of dot
floor(1.5 (t + 3)) + 1, inside the cycle; /INT as an instruction ends at
T-state t is the VDP's /INT at the start of dot floor(1.5 (t - 1)) + 1
(after that dot's port access), from a short history. The PSG clocks on
every 16th T-state, and a PSG write lands at t + 3, after that T-state's
clock.

__The Master System II__ (M6, 'smsPowerOn'; the spec's 'Gg.Spec.MasterSystem2'):
port 3Eh enables the cartridge (bit 6 clear) and the BIOS (bit 3 clear),
the bytes ANDed when both are; bit 2 turns the I/O chip off (C0h-FFh read
FFh, but F2h, the FM unit's, when one is fitted); pad 2 on DCh/DDh; the
FM unit's ports F0h-F2h go out ('goFm', at the T-state the spec gives the
unit its write; F2h's two bits are kept here for its read-back). It starts
as a BIOS leaves it (3Eh = ABh, written at C000h during the pre-roll) or,
with a BIOS image, in the BIOS (3Eh = E0h). The BIOS is ROM too: at
'RomAddr' 100000h up (the media region after the 1 MiB cartridge), banked
by FFFDh-FFFFh like a Sega mapper; when both are enabled the cycle reads
two ROM bytes ('ggRomWant2', 'giRom2').

__Memories__ are outside: the ROM ('goRomAddr', up to 1 MiB), the 8 KiB
RAM, the 32 KiB cartridge RAM and VRAM (four byte RAMs, "Gg.Vdp"); each
read comes back on the next clock. The CPU's address is steady for the
whole machine cycle, so its data is there when the cycle ends; writes
happen then.
-}
module Gg.Core
  ( Mapper (..)
  , GgIn (..)
  , GgOut (..)
  , GgC (..)
  , ggPowerOn
  , smsPowerOn
  , ggClock
  , ggRomWant
  , ggRomWant2
  , RomAddr
  , ggTBoundary
  , TPhase
  , DotPhase
  , dotOf
  ) where

import Clash.Prelude

import Gg.Psg
import Gg.Vdp
import Z80.Core

data Mapper = MSega | MCodemasters
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFDataX

-- | A ROM byte's address: the cartridge from 0 (up to 1 MiB), the Master
-- System's BIOS from 100000h (up to 256 KiB).
type RomAddr = Unsigned 21

data GgIn = GgIn
  { giRun   :: Bool                -- ^ an active clock
  , giRom   :: Unsigned 8          -- ^ the ROM byte at the last clock's 'goRomAddr'
  , giRam   :: Unsigned 8
  , giCart  :: Unsigned 8
  , giVram  :: Vec 4 (Unsigned 8)
  , giPad   :: Unsigned 8          -- ^ pressed: bits 0-5 up, down, left, right, 1, 2
  , giStart :: Bool
  , giMapper :: Mapper              -- ^ the cartridge's mapper, taken as the machine powers on
  , giBanks  :: Unsigned 20         -- ^ the ROM's 16 KiB banks (a power of two), taken likewise
  , giRom2   :: Unsigned 8          -- ^ the BIOS byte at 'goRomAddr2' (cartridge and BIOS both on)
  , giPad2   :: Unsigned 8          -- ^ the Master System's pad 2, bits as 'giPad'
  , giBiosBanks :: Unsigned 5       -- ^ the BIOS image's 16 KiB banks (a power of two; 0: none), taken at power-on
  , giOvl    :: Maybe (Unsigned 8)  -- ^ the ROM overlay's byte at the last clock's 'goCpuAddr' (side track N's
                                    --   save states: answers any read below C000h, whatever the mapper and 3Eh
                                    --   say; 'Nothing' without save states, as synthesis then drops it)
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFDataX

data GgOut = GgOut
  { goRomAddr  :: RomAddr
  , goRamAddr  :: Unsigned 13
  , goRamWrite :: Maybe (Unsigned 13, Unsigned 8)
  , goCartAddr :: Unsigned 15
  , goCartWrite :: Maybe (Unsigned 15, Unsigned 8)
  , goVramRead :: Unsigned 12
  , goVramWrite :: Maybe (Unsigned 14, Unsigned 8)
  , goPixel    :: Maybe (Unsigned 8, Unsigned 8, Unsigned 12)
  , goFrameEnd :: Bool
  , goAudio    :: Maybe (Signed 16, Signed 16) -- ^ the PSG's output after each of its clocks
  , goInstrEnd :: Bool                          -- ^ the CPU finished an instruction (or sequence) this clock
  , goRomAddr2 :: RomAddr                       -- ^ the BIOS byte's address when cartridge and BIOS are both on
  , goFm       :: Maybe (Unsigned 8, Unsigned 8) -- ^ a write to the FM unit (port F0h-F2h, data), at its T-state
  , goTState   :: Bool                          -- ^ a T-state ended this clock (the FM unit's time: 4 per cycle)
  , goBorder   :: Unsigned 12                   -- ^ the backdrop's colour (the border, taken at 'goFrameEnd')
  , goCpuAddr  :: Unsigned 16                   -- ^ the pending machine cycle's address (the ROM overlay's read, side track N)
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFDataX

data GgC = GgC
  { gZ       :: Z80T
  , gVdp     :: VdpC
  , gPsg     :: PsgC
  , gMapper  :: Mapper
  , gSms     :: Bool
  , gBanks   :: Unsigned 20         -- ^ the ROM's 16 KiB banks - 1 (a power of two - 1)
  , gMap     :: Vec 4 (Unsigned 8)  -- ^ Sega: FFFCh-FFFFh; Codemasters: 1-3 the slots' banks
  , gCmRam   :: Bool
  , gGgRegs  :: Vec 7 (Unsigned 8)
  , gMemCtl  :: Unsigned 8
  , gIoCtl   :: Unsigned 8
  , gClk     :: Unsigned 4          -- ^ 0-11
  , gPreroll :: Unsigned 4          -- ^ clocks of the VDP's head start left
  , gT       :: TPhase              -- ^ T-states completed (mod 512)
  , gCycT    :: TPhase              -- ^ the T-state the pending machine cycle started at
  , gDot     :: DotPhase            -- ^ the next dot to start (mod 768)
  , gVdpOp   :: Maybe (DotPhase, PortOp) -- ^ a port access waiting for its dot
  , gVdpRes  :: Unsigned 8          -- ^ a VDP port read's result
  , gIntHist :: Vec 4 (DotPhase, Bool) -- ^ /INT at the start of the last dots
  , gPsgW    :: Maybe (TPhase, Unsigned 8) -- ^ a PSG write waiting for its T-state
  , gIssued  :: Bool                -- ^ the pending cycle's VDP/PSG access is scheduled
  , gVramRd  :: Unsigned 12         -- ^ the VDP's last VRAM read address (held on inactive clocks)
  , gSms2    :: Bool                -- ^ the Master System II (M6)
  , gHasBios :: Bool                -- ^ a BIOS image is there
  , gBiosBanks :: Unsigned 4        -- ^ its 16 KiB banks - 1
  , gBiosMap :: Vec 3 (Unsigned 8)  -- ^ its banks (FFFDh-FFFFh)
  , gHasFm   :: Bool                -- ^ an FM unit is fitted
  , gFmCtl   :: Unsigned 2          -- ^ F2h as last written
  , gFmW     :: Maybe (TPhase, Unsigned 8, Unsigned 8) -- ^ an FM unit write waiting for its T-state
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFDataX

-- | Power-on: as the spec's machine. @banks@: the ROM's 16 KiB banks (a
-- power of two).
ggPowerOn :: Bool -> Mapper -> Unsigned 20 -> GgC
ggPowerOn sms mapper banks = GgC
  { gZ = z80PowerOn { zCpu = (zCpu z80PowerOn) { cSP = 0xDFF0 } }
  , gVdp = vdpPowerOn sms, gPsg = psgPowerOn, gMapper = mapper, gSms = sms
  , gBanks = banks - 1
  , gMap = case mapper of { MSega -> 0 :> 0 :> 1 :> 2 :> Nil; MCodemasters -> 0 :> 0 :> 1 :> 0 :> Nil }
  , gCmRam = False
  , gGgRegs = 0xC0 :> 0x7F :> 0xFF :> 0x00 :> 0xFF :> 0x00 :> 0xFF :> Nil
  , gMemCtl = 0, gIoCtl = 0xFF, gClk = 0, gPreroll = 8, gT = 0, gCycT = 0, gDot = 0
  , gVdpOp = Nothing, gVdpRes = 0xFF, gIntHist = repeat (0, False), gPsgW = Nothing, gIssued = False
  , gVramRd = 0
  , gSms2 = False, gHasBios = False, gBiosBanks = 0, gBiosMap = 0 :> 1 :> 2 :> Nil
  , gHasFm = False, gFmCtl = 0, gFmW = Nothing
  }

-- | Power-on of the Master System II (M6): Master System mode on its own
-- hardware, with a BIOS image or none, an FM unit or none.
smsPowerOn :: Bool -> Bool -> Mapper -> Unsigned 20 -> GgC
smsPowerOn bios fm mapper banks = (ggPowerOn True mapper banks)
  { gSms2 = True, gHasBios = bios, gHasFm = fm, gMemCtl = if bios then 0xE0 else 0xAB }

withMapper :: Mapper -> GgC -> GgC
withMapper m g = g
  { gMapper = m
  , gMap = case m of { MSega -> 0 :> 0 :> 1 :> 2 :> Nil; MCodemasters -> 0 :> 0 :> 1 :> 0 :> Nil } }

-- | The ROM address the pending machine cycle reads (an opcode fetch or a
-- memory read of the cartridge ROM), if it does: the board's ROM port
-- ("Gg.RomPort") must have it before the cycle ends.
ggRomWant :: GgC -> Maybe RomAddr
ggRomWant g =
  let rq = cReq (zCpu (gZ g))
  in if gPreroll g > 0 || not (rqKind rq == KFetch || rqKind rq == KRead) then Nothing
     else case target g (rqAddr rq) of
       TRom r -> Just r
       TBothRom r _ -> Just r
       _ -> Nothing

-- | The second ROM byte of a cycle that reads both the cartridge and the
-- BIOS (the Master System II with both enabled), if it does.
ggRomWant2 :: GgC -> Maybe RomAddr
ggRomWant2 g =
  let rq = cReq (zCpu (gZ g))
  in if gPreroll g > 0 || not (rqKind rq == KFetch || rqKind rq == KRead) then Nothing
     else case target g (rqAddr rq) of
       TBothRom _ b -> Just b
       TBothCart _ b -> Just b
       _ -> Nothing

-- | Does the next active clock end a T-state (when a machine cycle can end
-- and use its data)?
ggTBoundary :: GgC -> Bool
ggTBoundary g = gPreroll g == 0 && (gClk g == 0 || gClk g == 6)

-- * Memory map

data Target
  = TRom RomAddr | TRam (Unsigned 13) | TCart (Unsigned 15) | TNone
  | TBothRom RomAddr RomAddr     -- ^ the Master System II: cartridge ROM AND BIOS
  | TBothCart (Unsigned 15) RomAddr -- ^ cartridge RAM AND BIOS
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFDataX

romAt :: GgC -> Unsigned 8 -> Unsigned 14 -> Target
romAt g bank off = TRom ((resize (resize bank .&. gBanks g)) `shiftL` 14 .|. resize off)

-- | Where an address goes: on the Master System II below C000h as port
-- 3Eh says (the cartridge, the BIOS, both or neither), else the cartridge.
target :: GgC -> Unsigned 16 -> Target
target g a
  | gSms2 g && a < 0xC000 =
      let ctl = gMemCtl g
          cartOn = not (testBit ctl 6)
          biosOn = gHasBios g && not (testBit ctl 3)
          bank = if a < 0x0400 then 0 else gBiosMap g !! (resize (a `shiftR` 14) :: Unsigned 2)
          bios = 0x100000 .|. (resize (resize bank .&. gBiosBanks g) `shiftL` 14) .|. resize (a .&. 0x3FFF)
      in case (cartOn, biosOn) of
           (True, False) -> cartTarget g a
           (False, True) -> TRom bios
           (True, True) -> case cartTarget g a of
             TRom r -> TBothRom r bios
             TCart c -> TBothCart c bios
             t -> t
           (False, False) -> TNone
  | otherwise = cartTarget g a

-- | The cartridge slot and work RAM.
cartTarget :: GgC -> Unsigned 16 -> Target
cartTarget g a = case gMapper g of
  MSega
    | a < 0x0400 -> romAt g 0 (resize a)
    | a < 0x4000 -> romAt g (gMap g !! (1 :: Index 4)) (resize a)
    | a < 0x8000 -> romAt g (gMap g !! (2 :: Index 4)) (resize a)
    | a < 0xC000 -> let ctl = gMap g !! (0 :: Index 4) in
        if testBit ctl 3
          then TCart ((if testBit ctl 2 then 0x4000 else 0) + resize (a - 0x8000))
          else romAt g (gMap g !! (3 :: Index 4)) (resize a)
    | otherwise -> TRam (resize a)
  MCodemasters
    | a < 0x4000 -> romAt g (gMap g !! (1 :: Index 4)) (resize a)
    | a < 0x8000 -> romAt g (gMap g !! (2 :: Index 4)) (resize a)
    | a < 0xC000 -> if gCmRam g && a >= 0xA000
        then TCart (resize (a - 0xA000))
        else romAt g (gMap g !! (3 :: Index 4)) (resize a)
    | otherwise -> TRam (resize a)

-- | A memory write at the end of its cycle: RAM, cartridge RAM, mapper.
memWrite :: Unsigned 16 -> Unsigned 8 -> GgC -> (GgC, Maybe (Unsigned 13, Unsigned 8), Maybe (Unsigned 15, Unsigned 8))
memWrite a v g
  | a >= 0xC000 =
      let g' = if a >= 0xFFFC && gMapper g == MSega
                 then g { gMap = replace (resize a :: Unsigned 2) v (gMap g) } else g
          -- the Master System's BIOS banks (FFFDh-FFFFh)
          g'' = if a >= 0xFFFD && gSms2 g then g' { gBiosMap = replace (fromIntegral (a - 0xFFFD) :: Index 3) v (gBiosMap g') } else g'
      in (g'', Just (resize a, v), Nothing)
  | gMapper g == MCodemasters = case a of
      0x0000 -> (g { gMap = replace (1 :: Index 4) v (gMap g) }, Nothing, Nothing)
      0x4000 -> if testBit v 7 then (g { gCmRam = True }, Nothing, Nothing)
                else (g { gCmRam = False, gMap = replace (2 :: Index 4) v (gMap g) }, Nothing, Nothing)
      0x8000 -> (g { gMap = replace (3 :: Index 4) v (gMap g) }, Nothing, Nothing)
      _ -> if gCmRam g && a >= 0xA000 then (g, Nothing, Just (resize (a - 0xA000), v)) else (g, Nothing, Nothing)
  | a >= 0x8000 = case cartTarget g a of
      TCart c -> (g, Nothing, Just (c, v))
      _ -> (g, Nothing, Nothing)
  | otherwise = (g, Nothing, Nothing)

-- * Ports

isVdpPort :: Unsigned 8 -> Bool
isVdpPort p = p >= 0x40 && p < 0xC0

-- | The VDP access of a port access (reads 40h-BFh, writes 80h-BFh;
-- writes 40h-7Fh are the PSG's).
vdpOpOf :: Bool -> Unsigned 8 -> Unsigned 8 -> Maybe PortOp
vdpOpOf write p v
  | p >= 0x80 && p < 0xC0 = Just (if write then (if even p then PDataW v else PCtrlW v) else (if even p then PDataR else PStatusR))
  | p >= 0x40 && p < 0x80 && not write = Just (if even p then PVCountR else PHCountR)
  | otherwise = Nothing

-- | Does a write to the I/O control register make a TH line go high?
thRises :: Unsigned 8 -> Unsigned 8 -> Bool
thRises old new =
  let th c dir lvl = testBit c dir || testBit c lvl
      rises dir lvl = not (th old dir lvl) && th new dir lvl
  in rises 1 5 || rises 3 7

-- | A port read that isn't the VDP's.
portRead :: GgIn -> GgC -> Unsigned 8 -> Unsigned 8
portRead gi g p
  | p <= 0x06 && not (gSms g) = case p of
      0 -> ((gGgRegs g !! (0 :: Index 7)) .&. 0x7F) .|. (if giStart gi then 0 else 0x80)
      6 -> 0xFF
      _ -> gGgRegs g !! p
  | p < 0x40 = 0xFF
  | p >= 0xC0 && gSms2 g =
      -- the Master System II: the I/O chip off gives FFh (but F2h, the FM
      -- unit's); else pads 1 and 2 on DCh (even) and DDh (odd)
      if testBit (gMemCtl g) 2
        then (if p == 0xF2 && gHasFm g then resize (gFmCtl g) else 0xFF)
        else let p2 = giPad2 gi
                 c = gIoCtl g
                 th dir lvl = testBit c dir || testBit c lvl
             in if even p
                  then complement ((giPad gi .&. 0x3F) .|. ((p2 .&. 0x03) `shiftL` 6))
                  else complement ((p2 `shiftR` 2) .&. 0x0F) .&. (0x3F .|. (if th 1 5 then 0x40 else 0) .|. (if th 3 7 then 0x80 else 0))
  | p >= 0xC0 && even p = complement (giPad gi) .|. 0xC0
  | p >= 0xC0 = let c = gIoCtl g
                    th dir lvl = testBit c dir || testBit c lvl
                in 0x3F .|. (if th 1 5 then 0x40 else 0) .|. (if th 3 7 then 0x80 else 0)
  | otherwise = 0xFF

-- | A port write that isn't the VDP's or the PSG's (done at the end of
-- its cycle).
portWrite :: Unsigned 8 -> Unsigned 8 -> GgC -> GgC
portWrite p v g
  | p <= 0x06 && not (gSms g) = if p /= 0 then g { gGgRegs = replace p v (gGgRegs g) } else g
  | p < 0x40 = if even p then g { gMemCtl = v } else g { gIoCtl = v }
  | otherwise = g

-- * The clock

-- | Time in the core is only ever compared a few T-states or dots apart
-- (a port access a few dots ahead, /INT a few dots back, a PSG write a few
-- T-states ahead), so the counters are phases with matched periods: T-states
-- modulo 512 and dots modulo 768. Three dots go by every two T-states, so
-- floor (1.5 (t + 512)) = floor (1.5 t) + 768 and 'dotOf' of a T-state's
-- phase is exactly its dot's phase: the counters wrap together, every few
-- hundred T-states, and every lockstep test crosses hundreds of wraps a
-- frame. (They were 32-bit counts from power-on, and broke the machine on
-- the board after minutes, 2026-10-08: 3t overflowed at t = 2^32 / 3, and
-- at 2^32 the T-state count wrapped while the dot count, 1.5 times as fast,
-- didn't; either way VDP port accesses and /INT waited for a dot that never
-- came, and the machine stood still for minutes.)
type TPhase = Unsigned 9

-- | The dot counter's phase (see 'TPhase').
type DotPhase = Index 768

-- | The dot of T-state t: floor (1.5 t) + 1 (at most 767, no wrap needed).
dotOf :: TPhase -> DotPhase
dotOf t = fromIntegral ((zeroExtend t * 3 :: Unsigned 11) `shiftR` 1 + 1)

-- | The next dot's phase.
nextDot :: DotPhase -> DotPhase
nextDot d = if d == maxBound then 0 else d + 1

ggClock :: GgIn -> GgC -> (GgC, GgOut)
ggClock gi g0
  -- an inactive clock: nothing moves, the read addresses stay (so the
  -- block RAMs answer the same addresses on the next active clock)
  | not (giRun gi) = (g0, fst (memOut (noOut g0) { goVramRead = gVramRd g0 } g0))
  | gPreroll g0 > 0 =
      -- the VDP's two-dot head start; the CPU waits. On the first clock the
      -- cartridge's mapper is taken (with its power-on banks).
      let dotStart = gPreroll g0 == 8 || gPreroll g0 == 4
          first = gPreroll g0 == 8
          -- the Master System II also takes its BIOS then (giBiosBanks: 0
          -- none; the board's word, as the mapper's): in it with 3Eh = E0h,
          -- or as a BIOS leaves the machine (3Eh = ABh)
          hasBios = gSms2 g0 && giBiosBanks gi /= 0
          gm = if first
                 then (withMapper (giMapper gi) g0)
                        { gBanks = giBanks gi - 1, gBiosBanks = resize (giBiosBanks gi - 1)
                        , gHasBios = hasBios
                        , gMemCtl = if not (gSms2 g0) then gMemCtl g0 else if hasBios then 0xE0 else 0xAB }
                 else g0
          (g1, vo) = vdpStep gi dotStart gm
          -- the Master System II without a BIOS: C000h holds the BIOS's copy
          -- of port 3Eh (written on the first clock, as the spec starts)
          ramInit = if first && gSms2 g0 && not hasBios then Just (0, 0xAB) else Nothing
      in ( g1 { gPreroll = gPreroll g0 - 1, gVramRd = voReadAddr vo }
         , fst (memOut (noOut g1) { goVramRead = voReadAddr vo, goVramWrite = voWrite vo, goRamWrite = ramInit } g1) )
  | otherwise =
      let c = gClk g0
          dotStart = c == 0 || c == 4 || c == 8
          (g1, vo) = vdpStep gi dotStart g0
          tEnd = c == 0 || c == 6
          (g2, audio, ended, ramW, cartW, fmW) = if tEnd then tState gi g1 else (g1, Nothing, False, Nothing, Nothing, Nothing)
          g3 = schedule g2
          out = (noOut g3) { goVramRead = voReadAddr vo, goVramWrite = voWrite vo
                           , goPixel = voPixel vo, goFrameEnd = voFrameEnd vo
                           , goAudio = audio, goInstrEnd = ended
                           , goRamWrite = ramW, goCartWrite = cartW
                           , goFm = fmW, goTState = tEnd }
          (out', g4) = memOut out g3
      in (g4 { gClk = if c == 11 then 0 else c + 1, gVramRd = voReadAddr vo }, out')

noOut :: GgC -> GgOut
noOut g = GgOut 0 0 Nothing 0 Nothing 0 Nothing Nothing False Nothing False 0 Nothing False (vdpBorderC (gVdp g)) (rqAddr (cReq (zCpu (gZ g))))

-- | The VDP's clock; at a dot's start its port access (if this is its
-- dot) and the /INT history.
vdpStep :: GgIn -> Bool -> GgC -> (GgC, VdpOut)
vdpStep gi dotStart g =
  let (op, g1) = case gVdpOp g of
        Just (d, o) | dotStart && d == gDot g -> (o, g { gVdpOp = Nothing })
        _ -> (PNone, g)
      (v', vo) = vdpClock (VdpIn dotStart op (giVram gi)) (gVdp g1)
      res = case voPortData vo of
        Just r -> r
        Nothing -> gVdpRes g1
      -- built strictly: a lazy Bool would keep this dot's whole VDP state
      -- alive in simulation (it changes nothing in hardware)
      i = vdpIntC v'
      hist = if dotStart
        then case gIntHist g1 of
          (h0 :> h1 :> h2 :> _) -> i `seq` ((gDot g1, i) :> h0 :> h1 :> h2 :> Nil)
        else gIntHist g1
      d' = if dotStart then nextDot (gDot g1) else gDot g1
  in (g1 { gVdp = v', gVdpRes = res, gIntHist = hist, gDot = d' }, vo)

-- | /INT as the CPU sees it at the end of an instruction at T-state t.
intAt :: GgC -> TPhase -> Bool
intAt g t =
  let want = dotOf (t - 1)
  in foldr (\(d, i) acc -> if d == want then i else acc) False (gIntHist g)

-- | A T-state ends: the PSG's clock (every 16th), a PSG write due now, the
-- CPU's T-state (and the end of its machine cycle).
tState :: GgIn -> GgC -> (GgC, Maybe (Signed 16, Signed 16), Bool, Maybe (Unsigned 13, Unsigned 8), Maybe (Unsigned 15, Unsigned 8), Maybe (Unsigned 8, Unsigned 8))
tState gi g0 =
  let t' = gT g0 + 1
      psgTicked = t' .&. 15 == 0
      psg1 = if psgTicked then psgTickC (gPsg g0) else gPsg g0
      stereo = if gSms g0 then 0xFF else gGgRegs g0 !! (6 :: Index 7)
      audio = if psgTicked then Just (psgOutC stereo psg1) else Nothing
      (psg2, psgW) = case gPsgW g0 of
        Just (tw, b) | tw == t' -> (psgWriteC b psg1, Nothing)
        other -> (psg1, other)
      -- an FM unit write due now (M6): out, and F2h's bits kept
      (fmOut, fmW', fmCtl) = case gFmW g0 of
        Just (tw, p, b) | tw == t' -> (Just (p, b), Nothing, if p == 0xF2 then resize b else gFmCtl g0)
        other -> (Nothing, other, gFmCtl g0)
      g1 = g0 { gT = t', gPsg = psg2, gPsgW = psgW, gFmW = fmW', gFmCtl = fmCtl }
      z = gZ g1
      cpu = zCpu z
      rq = cReq cpu
      completes = z80Done False z
      -- the data and the side effects of a completing cycle
      a = rqAddr rq
      port = resize a :: Unsigned 8
      (din, g2) = if not completes then (0, g1) else case rqKind rq of
        KFetch -> (memData gi g1 a, g1)
        KRead -> (memData gi g1 a, g1)
        KWrite -> (0, g1)
        KIn -> (if isVdpPort port then gVdpRes g1 else portRead gi g1 port, g1)
        KOut -> (0, if isVdpPort port then g1 else portWrite port (rqData rq) g1)
        KIdle -> (0, g1)
        KIntAck -> (0xFF, g1)
      ci = CpuIn din (intAt g2 t') (gSms g2 && giStart gi)
      z' = z80Tick False ci z
      ended = completes && cNew (zCpu z')
      g3 = g2 { gZ = z', gCycT = if completes then t' else gCycT g2, gIssued = if completes then False else gIssued g2 }
      (g4, ramW, cartW) = if completes && rqKind rq == KWrite
             then memWrite a (rqData rq) g3
             else (g3, Nothing, Nothing)
  in (g4, audio, ended, ramW, cartW, fmOut)

-- | The pending cycle's VDP or PSG access, scheduled once (its address
-- and data are known as the cycle starts).
schedule :: GgC -> GgC
schedule g
  | gIssued g = g
  | otherwise =
      let rq = cReq (zCpu (gZ g))
          port = resize (rqAddr rq) :: Unsigned 8
          tIo = gCycT g + 3
          write = rqKind rq == KOut
          isIo = rqKind rq == KIn || write
          g1 = g { gIssued = True }
      in if not isIo then g1
         else if write && port >= 0x40 && port < 0x80
           then g1 { gPsgW = Just (tIo, rqData rq) }
         else if write && gSms2 g && gHasFm g && port >= 0xF0 && port <= 0xF2
           then g1 { gFmW = Just (tIo, port, rqData rq) }
         else if write && port < 0x40 && odd port && (gSms g || port > 0x06) && thRises (gIoCtl g) (rqData rq)
           then g1 { gVdpOp = Just (dotOf tIo, PLatchH) }
         else case vdpOpOf write port (rqData rq) of
           Just op -> g1 { gVdpOp = Just (dotOf tIo, op) }
           Nothing -> g1

-- | Where the pending cycle's memory access goes.
memData :: GgIn -> GgC -> Unsigned 16 -> Unsigned 8
memData gi g a = case giOvl gi of
  Just b | a < 0xC000 -> b
  _ -> memData' gi g a

memData' :: GgIn -> GgC -> Unsigned 16 -> Unsigned 8
memData' gi g a = case target g a of
  TRom _ -> giRom gi
  TRam _ -> giRam gi
  TCart _ -> giCart gi
  TNone -> 0xFF
  TBothRom _ _ -> giRom gi .&. giRom2 gi
  TBothCart _ _ -> giCart gi .&. giRom2 gi

-- | The memory addresses for the pending cycle, and the writes of a
-- cycle that just completed.
memOut :: GgOut -> GgC -> (GgOut, GgC)
memOut out g =
  let rq = cReq (zCpu (gZ g))
      a = rqAddr rq
      o1 = case target g a of
        TRom r -> out { goRomAddr = r }
        TRam r -> out { goRamAddr = r }
        TCart r -> out { goCartAddr = r }
        TNone -> out
        TBothRom r b -> out { goRomAddr = r, goRomAddr2 = b }
        TBothCart c b -> out { goCartAddr = c, goRomAddr2 = b }
  in (o1, g)
