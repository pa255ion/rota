{-# LANGUAGE MultiWayIf #-}
{-|
The 800XL around the 6502 (M4 step 4.1): the memory map with the XL's
banking, the PIA, and how the machine cycle is shared between ANTIC and
the CPU. Everything runs in 'ST' behind "A800.Spec".

Sources: Altirra Hardware Reference Manual (HRM) chapter 2 ("Memory
system", "PIA", "Bank switching", "Miscellaneous connections").

__The machine cycle.__ Every cycle starts with POKEY's tick and ANTIC's
('anticBegin'); if ANTIC takes the bus (refresh, and from step 4.2 its
DMA), the CPU is halted for that cycle, whatever it wanted to do: the
6502C's HALT stops reads and writes alike (docs/systems/atari800xl.md,
"Accuracy target"). Otherwise the CPU's access is the cycle's bus access.
The shared 6502 model ("Mos6502.Spec") drives this through its bus
callbacks: before each of its accesses the machine runs the halted
cycles, so the CPU only ever sees its own cycles (and its interrupt
inputs are sampled at the end of those only, as a frozen 6502's are).
WSYNC is RDY ('busRdy'): it holds reads only, and the CPU repeats the
read meanwhile.

__Memory__ (HRM 2.6). PORTB (as driven: an input bit counts as 1, the
pull-ups) selects the ROMs: bit 0 = 1 the OS at $C000-$CFFF and
$D800-$FFFF, bit 1 = 0 BASIC at $A000-$BFFF, bit 7 = 0 the self-test at
$5000-$57FF (the OS image's $D000-$D7FF; only with the OS on). A
cartridge has priority over BASIC. Writes to a ROM go nowhere. I/O at
$D000-$D7FF: GTIA $D0, POKEY $D2, PIA $D3, ANTIC $D4; nothing at $D1,
$D5 to $D7: the XL's pull-ups read $FF (HRM 2.3, "Floating data bus").

__PIA__ (HRM 2.5). Port A is the joysticks (inputs, pulled up; an
output bit reads as output AND input); port B the banking (an output bit
reads back its output; the floating inputs 2-6 read their last output, as
they do right after switching, the decay to 0 isn't modelled). Bit 2 of
PACTL/PBCTL selects the data register over the direction register;
reading a data register clears that port's interrupt flags (bits 6-7).
CA1/CB1 (the SIO proceed and interrupt lines) never change without SIO
devices; CA2/CB2 written as outputs and switched back to inputs can raise
bit 6 (HRM 2.5, "Spurious interrupts", Altirra's edge rules), and an
enabled flag asserts the IRQ line with POKEY's.

__Reset__ (the XL's Reset key, HRM 2.4): CPU, ANTIC and PIA; POKEY and
GTIA have no reset line.

__Executables__ (docs/systems/atari800xl.md, "Loading";
@systems/a800xl/loader/xexload.s@). With an XEX attached, the loader's
256-byte page shows at $D500-$D5FF and a stub cartridge at $A000-$BFFF
(all $FF but its header: start $D500, flags 4, init $D5EF), so the OS
starts the loader after its own start-up. The page's last two bytes are
the port: reading $D5FE gives the file's next byte ($FF past its end),
$D5FF bit 7 says the end was reached; writing $D5FE removes the stub
cartridge, writing $D5FF removes the page and the port at the CPU's next
read outside the page.
-}
module A800.Spec.Machine
  ( Machine (..)
  , Config (..)
  , XexBoot (..)
  , Access (..)
  , newMachine
  , newMachineOverlay
  , resetMachine
  , stepCpu
  , machineCycles
  , peekRam
  , peekCpu
  , pokeRam
  , setJoystick
  , setConsole
  , setKeys
  , Event (..)
  , setHook
  , withTrace
  , debugLines
  , portB
    -- * Save states (side track N)
  , setOverlay
  , setTape
  , tapePosition
  , overlaySeen
  , machineQuiet
  , loaderSnapshot
  , piaSnapshot
  ) where

import Prelude

import Control.Monad (when)
import Control.Monad.ST (ST)
import Data.Bits
import qualified Data.ByteString as BS
import Data.STRef
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MV
import Data.Word
import Numeric (showHex)

import A800.Media (Cart (..), Cas (..), TapeRec, cartBanks, cartBase, cartByte, isXegs, sioChecksum, tapeLevel, tapeRecCycles)
import qualified Data.Vector as V
import A800.Spec.Antic
import A800.Spec.Gtia
import A800.Spec.Pokey
import Mos6502.Spec hiding (Config)

-- | What the machine is built with.
data Config = Config
  { cfgOs    :: !BS.ByteString         -- ^ 16 KiB OS ROM ($C000-$FFFF image)
  , cfgBasic :: !(Maybe BS.ByteString) -- ^ 8 KiB BASIC ROM (the 800XL has it built in)
  , cfgCart  :: !(Maybe Cart)
  , cfgXex   :: !(Maybe XexBoot)       -- ^ an executable, through the loader
  , cfgCas   :: !(Maybe Cas)           -- ^ a cassette in the recorder ('casRead')
  }

-- | An executable to load: the loader page (256 bytes, $D500-$D5FF, from
-- @make a800-asm@) and the file.
data XexBoot = XexBoot
  { xbLoader :: !BS.ByteString
  , xbFile   :: !BS.ByteString
  }

-- | What the chips were asked to do, for the lockstep tests of the Clash
-- units (step 4.5): each chip's calls with their results, in order.
data Event
  = EvPokeyTick                      -- ^ POKEY's cycle start
  | EvPokeyRead !Word16 !Word8       -- ^ a POKEY register read and its value
  | EvPokeyWrite !Word16 !Word8
  | EvPokeyKeys !Word64 !Word8
  | EvGtiaMissile !Int !Bool !Word8          -- ^ cycle, odd line, data
  | EvGtiaPlayer !Int !Bool !Int !Word8
  | EvGtiaRead !Word16 !Bool !Word8          -- ^ register, RD5, value
  | EvGtiaWrite !Int !Word16 !Word8          -- ^ cycle, register, value
  | EvGtiaCycle !Int !Int !Int !Int          -- ^ line, cycle, AN codes (after the spec rendered them)
  | EvGtiaInputs !Word8 !Word8               -- ^ triggers, console keys
  | EvAnticFetch !Word16 !Word8              -- ^ a DMA read (inside the cycle start)
  | EvAnticBegun !Word8 !Int                 -- ^ the cycle start done: the last bus value, 0 CPU / 1 fetch / 2 refresh
  | EvAnticRead !Word16 !Word8
  | EvAnticWrite !Word16 !Word8
  | EvAnticRdy !Bool                         -- ^ WSYNC asked before a CPU read: 'True' holds it
  | EvAnticEnd !Word8 !Int !Int              -- ^ the cycle end: the bus value, the AN codes
  | EvAnticReset
  deriving stock (Show, Eq)

-- | One machine cycle's bus access: address, data, write, by the CPU
-- ('True') or ANTIC.
data Access = Access !Word16 !Word8 !Bool !Bool
  deriving stock (Show, Eq)

data Machine s = Machine
  { mCpu   :: !(Cpu s)
  , mAntic :: !(Antic s)
  , mPokey :: !(Pokey s)
  , mGtia  :: !(Gtia s)
  , mRam   :: !(MV.MVector s Word8)
  , mCfg   :: !Config
  , mSt    :: !(MV.MVector s Int)
  , mTrace :: !(STRef s [Access])
  , mDebug :: !(STRef s (Maybe (Int, Int), [String])) -- ^ lines to log per cycle, the log (newest first)
  , mHook  :: !(STRef s (Maybe (Event -> ST s ())))      -- ^ the lockstep tests' listener
  , mOvl   :: !(STRef s (Maybe (U.Vector Word8, Word16, Bool))) -- ^ the ROM overlay: its bytes, end address, end read
  , mTape  :: !(STRef s (Maybe (V.Vector (TapeRec, Int)))) -- ^ the deck's tape ('setTape'): its records, each with its length in cycles
  }

-- State slots.
iTime, iBegun, iLastCpu, iBus, iPactl, iPbctl, iOra, iDdra, iOrb, iDdrb, iJoy, iTraceOn :: Int
iTime = 0     -- the cycle being run (cycles begun since power-on)
iBegun = 1    -- 1: cycle iTime has begun and is the CPU's
iLastCpu = 2  -- the last cycle the CPU had
iBus = 3      -- the data bus (last value on it)
iPactl = 4
iPbctl = 5
iOra = 6
iDdra = 7
iOrb = 8
iDdrb = 9
iJoy = 10     -- port A's inputs: bits 0-3 stick 1 (up, down, left, right; 0 = pushed), 4-7 stick 2
iTraceOn = 11
iXexPos, iStub, iPage :: Int
iXexPos = 12  -- the loader port's position in the file
iStub = 13    -- 1: the stub cartridge is there
iPage = 14    -- the loader page: 0 gone, 1 there, 2 going at the next read outside it
iEdgeA, iEdgeB, iCb2 :: Int
iEdgeA = 15   -- PIA: a CA2 edge waiting to show (spurious interrupts, HRM 2.5)
iEdgeB = 16   -- the same for CB2
iCb2 = 17     -- CB2 as driven: 0 floating, 1 high, 2 low (the SIO command line)

-- Save states (side track N): the 6502's interrupt poll, kept from its
-- bus calls (its own state isn't visible; "A800.Spec.SaveState").
iPoll1, iPoll2, iNmiPrev, iNmiOwed :: Int
iPoll1 = 18   -- the last cycle's poll: 1 an interrupt would be taken
iPoll2 = 19   -- the cycle before's (the 6502 acts on it at an instruction's end)
iNmiPrev = 20 -- the NMI line at the last cycle's end
iNmiOwed = 21 -- an NMI edge seen and not yet taken (a vector read takes it: the 800XL's 6502 drops one seen on its push of P)

-- The cartridge's bank register (XEGS, 'cartBanks').
iCartBank :: Int
iCartBank = 22

-- The cassette ('casRead'): the next record; the answer's program armed,
-- its page count, the DCB's bytes as the program read them, the record
-- it reads, its length, its checksum's verdict.
iCasPos, iCasArm, iCasLap, iCasDev, iCasRec, iCasL, iCasOk :: Int
iCasPos = 23
iCasArm = 24
iCasLap = 25
iCasDev = 26
iCasRec = 27
iCasL = 28
iCasOk = 29
-- DUNIT, DCOMND, DBUF (2), DBYT (2), DAUX1, DAUX2: slots 30 to 37
iCasDcb :: Int
iCasDcb = 30

-- The cassette deck (depth 2, 'setTape'): the record playing, cycles
-- into it.
iTapeRec, iTapeT :: Int
iTapeRec = 38
iTapeT = 39

slots :: Int
slots = 40

get :: Machine s -> Int -> ST s Int
get m = MV.unsafeRead (mSt m)
{-# INLINE get #-}

set :: Machine s -> Int -> Int -> ST s ()
set m = MV.unsafeWrite (mSt m)
{-# INLINE set #-}

-- | Power on (RAM clear) and run the CPU's reset sequence.
newMachine :: Config -> ST s (Machine s)
newMachine cfg = newMachineOverlay cfg Nothing

-- | The same with the ROM overlay armed from the start (its bytes, its
-- end: 'setOverlay'), so the reset sequence reads its vector from it, as
-- the core does when the PS releases it (side track N).
newMachineOverlay :: Config -> Maybe (U.Vector Word8, Word16) -> ST s (Machine s)
newMachineOverlay cfg ovl0 = do
  ram <- MV.replicate 0x10000 0
  st <- MV.replicate slots 0
  MV.write st iJoy 0xFF
  MV.write st iBus 0xFF
  MV.write st iLastCpu (-1)
  case cfgXex cfg of
    Just _ -> MV.write st iStub 1 >> MV.write st iPage 1
    Nothing -> pure ()
  pokey <- newPokey
  gtia <- newGtia
  tr <- newSTRef []
  dbg <- newSTRef (Nothing, [])
  hookRef <- newSTRef Nothing
  ovlRef <- newSTRef ((\(img, end) -> (img, end, False)) <$> ovl0)
  mref <- newSTRef (error "machine not built")
  antic <- newAntic AnticConn
    { acRead = \a -> readSTRef mref >>= \m -> do
        v <- dmaRead m a
        set m iBus (fromIntegral v)
        event m (EvAnticFetch a v)
        trace m (Access a v False False)
        pure v
    , acMissile = \odd' v -> readSTRef mref >>= \m -> do
        (_, x) <- anticPosition (mAntic m)
        gtiaMissile gtia x odd' v
        event m (EvGtiaMissile x odd' v)
    , acPlayer = \odd' n v -> readSTRef mref >>= \m -> do
        (_, x) <- anticPosition (mAntic m)
        gtiaPlayer gtia x odd' n v
        event m (EvGtiaPlayer x odd' n v)
    }
  let bus = Bus
        { busRead = \a -> readSTRef mref >>= \m -> cpuRead m a
        , busWrite = \a v -> readSTRef mref >>= \m -> cpuWrite m a v
        , busRdy = readSTRef mref >>= \m -> do
            t <- syncCpu m
            l <- get m iLastCpu
            h <- wsyncHold antic t l
            event m (EvAnticRdy h)
            pure (not h)
        , busNmi = readSTRef mref >>= \m -> do
            v <- anticNmi antic
            p <- get m iNmiPrev
            when (v && p == 0) (set m iNmiOwed 1)
            set m iNmiPrev (if v then 1 else 0)
            pure v
        , busIrq = readSTRef mref >>= \m -> do
            irq <- (||) <$> pokeyIrq pokey <*> piaIrq m
            -- the poll of this cycle (busIrq comes after busNmi), as the
            -- 6502 records it
            masked <- if irq then (\r -> rP r .&. flagI /= 0) <$> getRegs (mCpu m) else pure True
            owed <- get m iNmiOwed
            get m iPoll1 >>= set m iPoll2
            set m iPoll1 (if owed /= 0 || not masked then 1 else 0)
            pure irq
        }
  cpu <- newCpu sally6502 bus (Regs 0 0 0 0 0x34 0)
  tapeRef <- newSTRef Nothing
  let m = Machine cpu antic pokey gtia ram cfg st tr dbg hookRef ovlRef tapeRef
  writeSTRef mref m
  reset cpu
  pure m

-- | A tape in the 410 recorder (side track Q, cassettes depth 2; 'Nothing'
-- takes it out), wound back to its start: from now on it plays while the
-- motor runs (PIA CA2 driven low: PACTL bits 5-3 = 110), its data line
-- into POKEY's serial input; idle (1) otherwise and after its end. For a
-- tape played this way, leave 'cfgCas' (the instant answer) out.
setTape :: Machine s -> Maybe [TapeRec] -> ST s ()
setTape m t = do
  writeSTRef (mTape m) ((\rs -> V.fromList [ (r, tapeRecCycles r) | r <- rs ]) <$> t)
  set m iTapeRec 0
  set m iTapeT 0
  pokeySetSerialIn (mPokey m) True

-- | Where the tape is: the record playing (the count once past the end),
-- cycles into it.
tapePosition :: Machine s -> ST s (Int, Int)
tapePosition m = (,) <$> get m iTapeRec <*> get m iTapeT

-- | One machine cycle of the deck, before POKEY's: the data line as the
-- tape has it now, and the tape on one cycle while the motor runs.
deckStep :: Machine s -> ST s ()
deckStep m = readSTRef (mTape m) >>= \mt -> case mt of
  Nothing -> pure ()
  Just recs -> do
    ctl <- get m iPactl
    k <- get m iTapeRec
    if ctl .&. 0x38 /= 0x30 || k >= V.length recs
      then pokeySetSerialIn (mPokey m) True
      else do
        t <- get m iTapeT
        let (r, len) = recs V.! k
        pokeySetSerialIn (mPokey m) (tapeLevel r t)
        if t + 1 >= len then set m iTapeRec (k + 1) >> set m iTapeT 0 else set m iTapeT (t + 1)

-- | The Reset key (XL/XE): CPU, ANTIC and PIA.
resetMachine :: Machine s -> ST s ()
resetMachine m = do
  anticReset (mAntic m)
  event m EvAnticReset
  mapM_ (\i -> set m i 0) [iPactl, iPbctl, iOra, iDdra, iOrb, iDdrb, iEdgeA, iEdgeB, iCb2, iCasArm]
  reset (mCpu m)

stepCpu :: Machine s -> ST s Step
stepCpu m = step (mCpu m)

-- | The cassette, at the level of the OS's call (side track Q, depth 1 of
-- PLAN.md's cassette plan; atari800's SIO patch, its sio.c, for the
-- values; not the hardware: no recorder, no POKEY serial input). With a
-- tape in, the CPU's read of SIOV ($E459, the OS ROM mapped) answers
-- @JMP $D500@, and the empty cartridge-control page ($D5xx: the XL reads
-- $FF there) answers a program the CPU runs itself, so that the core can
-- do the same cycle for cycle ("A800.Core"): every byte follows from its
-- address (a 6502 interrupt's dummy reads see the same bytes again), the
-- record's bytes come as operands (from DDR on the board).
--
-- The program is groups of 5 bytes, 50 to a page, then @JMP $D500@ (the
-- page count goes up at that jump's last byte): it reads DDEVIC (@LDA
-- abs@, two NOPs), and for another device than the cassette ($60:
-- AltirraOS's handler stores it, $EE39) jumps to the real SIO (@JMP
-- ($E45A)@: the ROM's vector, the program gone); else it reads DUNIT,
-- DCOMND, DBUF, DBYT, DAUX1, DAUX2, then the next record's length (@LDA
-- #len / STA CRITIC@), copies a read's (DCOMND $52) first DBYT bytes to
-- DBUF (@LDA #byte / STA addr@: its sync bytes, control byte and data, as
-- SIO hands them over), reads the checksum after them if the record has
-- one, copies the command to CDEVIC-CAUX2 ($023A-$023D) as the real
-- routine, stores the status in DSTATS (1 done; 144 device error: no
-- record left, a bad checksum; 139 refused: anything but a read, nothing
-- to record on), CRITIC ($42) 0, and ends @LDY #status / SEC / RTS@: A 0,
-- Y the status, N set on an error, carry set. The program goes at reads
-- that happen once (an operand, the RTS's second cycle), never at an
-- opcode's: an interrupt reads the next opcode twice before taking it. A record is at most 255
-- bytes here (the board's tape image, 'A800.Media.casImage').
casRead :: Machine s -> Word16 -> ST s (Maybe Word8)
casRead m a = case cfgCas (mCfg m) of
  Nothing -> pure Nothing
  Just (Cas recs) -> do
    arm <- get m iCasArm
    if arm == 0
      then if a /= 0xE459 then pure Nothing else do
        pb <- portB m
        if not (testBit pb 0) then pure Nothing else do
          set m iCasArm 1
          set m iCasLap 0
          pure (Just 0x4C)
      else case a of
        0xE459 -> pure (Just 0x4C)
        0xE45A -> pure (Just 0x00)
        0xE45B -> pure (Just 0xD5)
        _ | a `shiftR` 8 == 0xD5 -> Just <$> casProgram m recs (fromIntegral (a .&. 0xFF))
          | otherwise -> pure Nothing

casProgram :: Machine s -> [BS.ByteString] -> Int -> ST s Word8
casProgram m recs o
  | o == 250 = pure 0x4C
  | o == 251 = pure 0x00
  | o == 252 = get m iCasLap >>= set m iCasLap . (+ 1) >> pure 0xD5
  | o > 252 = pure 0xFF
  | otherwise = do
      lap <- get m iCasLap
      dev <- get m iCasDev
      let g = lap * 50 + o `div` 5
          j = o `mod` 5
          pick = pure . (!! j)
      if | g == 0 -> do
             when (j == 2) (peekRam m 0x0300 >>= set m iCasDev . fromIntegral)
             pick [0xAD, 0x00, 0x03, 0xEA, 0xEA]
         | dev /= 0x60 -> do
             when (j == 2) (set m iCasArm 0)
             pick [0x6C, 0x5A, 0xE4, 0xEA, 0xEA]
         | g <= 8 -> do
             let lo = [0x01, 0x02, 0x04, 0x05, 0x08, 0x09, 0x0A, 0x0B] !! (g - 1)
             when (j == 2) (peekRam m (0x0300 + fromIntegral lo) >>= set m (iCasDcb + g - 1) . fromIntegral)
             pick [0xAD, lo, 0x03, 0xEA, 0xEA]
         | otherwise -> do
             dcb <- mapM (get m) [iCasDcb .. iCasDcb + 7]
             let unit = dcb !! 0; cmd = dcb !! 1; bufL = dcb !! 2; bufH = dcb !! 3
                 lenL = dcb !! 4; lenH = dcb !! 5; aux1 = dcb !! 6; aux2 = dcb !! 7
             if g == 9
               then if j /= 1 then pick [0xA9, 0, 0x8D, 0x42, 0x00] else do
                 k <- get m iCasPos
                 let len = if k < length recs then min 255 (BS.length (recs !! k)) else 0
                 set m iCasRec k
                 set m iCasL len
                 when (cmd == 0x52 && len > 0) (set m iCasPos (k + 1))
                 pure (fromIntegral len)
               else do
                 len <- get m iCasL
                 k <- get m iCasRec
                 ok <- get m iCasOk
                 let rec' = if k < length recs then recs !! k else BS.empty
                     n = lenL + 256 * lenH
                     buf = bufL + 256 * bufH
                     c = if cmd == 0x52 then min n len else 0
                     hasCk = cmd == 0x52 && len >= n + 1
                     status = if cmd /= 0x52 then 139 else if len == 0 then 144 else if hasCk && ok /= 0 then 1 else 144
                     e = g - 10 - c - (if hasCk then 1 else 0)
                     store v ad = pick [0xA9, fromIntegral v, 0x8D, fromIntegral ad, fromIntegral (ad `shiftR` 8)]
                 if | g - 10 < c -> let i = g - 10 in store (fromIntegral (BS.index rec' i)) (buf + i)
                    | hasCk && g - 10 == c -> do
                        when (j == 1) (set m iCasOk (if BS.index rec' n == sioChecksum (BS.take n rec') then 1 else 0))
                        store (fromIntegral (BS.index rec' n)) (0x0042 :: Int)
                    | e == 0 -> store (0x5F + unit) (0x023A :: Int)
                    | e == 1 -> store cmd (0x023B :: Int)
                    | e == 2 -> store aux1 (0x023C :: Int)
                    | e == 3 -> store aux2 (0x023D :: Int)
                    | e == 4 -> store status (0x0303 :: Int)
                    | e == 5 -> store 0 (0x0042 :: Int)
                    | otherwise -> do
                        -- gone at the RTS's second cycle (it reads the byte
                        -- after it, once; an interrupt's dummy reads would
                        -- read the RTS itself)
                        when (j == 4) (set m iCasArm 0)
                        pick [0xA0, fromIntegral status, 0x38, 0x60, 0xEA]

-- | Machine cycles since power-on.
machineCycles :: Machine s -> ST s Int
machineCycles m = get m iTime

setJoystick :: Machine s -> Word8 -> ST s ()
setJoystick m v = set m iJoy (fromIntegral v)

-- | Keys held: bit n = key code n ("A800.Keyboard"); @mods@ bit 0 SHIFT,
-- 1 CONTROL, 2 BREAK.
setKeys :: Machine s -> Word64 -> Word8 -> ST s ()
setKeys m k mods = pokeySetKeys (mPokey m) k mods >> event m (EvPokeyKeys k mods)

-- | Listen to the chips' calls ('Event'), for the lockstep tests.
setHook :: Machine s -> (Event -> ST s ()) -> ST s ()
setHook m h = writeSTRef (mHook m) (Just h)

event :: Machine s -> Event -> ST s ()
event m e = readSTRef (mHook m) >>= maybe (pure ()) ($ e)

-- | Console keys held (bit 0 START, 1 SELECT, 2 OPTION).
setConsole :: Machine s -> Word8 -> ST s ()
setConsole m = setConsoleKeys (mGtia m)

-- | Record every bus access while running the action (newest first).
withTrace :: Machine s -> ST s a -> ST s (a, [Access])
withTrace m act = do
  writeSTRef (mTrace m) []
  set m iTraceOn 1
  r <- act
  set m iTraceOn 0
  tr <- readSTRef (mTrace m)
  writeSTRef (mTrace m) []
  pure (r, reverse tr)

trace :: Machine s -> Access -> ST s ()
trace m a = do
  on <- get m iTraceOn
  when (on /= 0) (modifySTRef' (mTrace m) (a :))

-- * Cycles

-- | Run cycles until one is the CPU's; its number.
syncCpu :: Machine s -> ST s Int
syncCpu m = do
  b <- get m iBegun
  if b /= 0 then get m iTime else go
 where
  go = do
    t <- (+ 1) <$> get m iTime
    set m iTime t
    deckStep m
    pokeyTick (mPokey m)
    event m EvPokeyTick
    bus <- fromIntegral <$> get m iBus
    dma <- anticBegin (mAntic m) t bus
    event m (EvAnticBegun (fromIntegral bus) dma)
    case dma of
      0 -> set m iBegun 1 >> pure t
      _ -> do
        -- A refresh: nothing drives the bus; the XL's pull-ups read $FF.
        when (dma == 2) $ do
          set m iBus 0xFF
          trace m (Access 0 0xFF False False)
        endCycle m
        go

-- | The end of a machine cycle: ANTIC's virtual fetches and display, two
-- colour clocks of GTIA.
endCycle :: Machine s -> ST s ()
endCycle m = do
  bus <- fromIntegral <$> get m iBus
  (c1, c2) <- anticEnd (mAntic m) bus
  event m (EvAnticEnd (fromIntegral bus) c1 c2)
  pos <- anticPosition (mAntic m)
  gtiaClocks (mGtia m) pos c1 c2
  event m (EvGtiaCycle (fst pos) (snd pos) c1 c2)
  (rng, _) <- readSTRef (mDebug m)
  case rng of
    Just (y0, y1) | fst pos >= y0 && fst pos <= y1 -> do
      acc <- readSTRef (mTrace m)
      writeSTRef (mTrace m) []
      let (y, x) = pos
          what = case acc of
            Access a v w c : _ -> (if c then (if w then "CPU W " else "CPU R ") else if a == 0 && v == 0xFF then "refresh" else "DMA   ")
                                  <> hex4 a <> " " <> hex2 v
            [] -> "-"
      st <- anticDebug (mAntic m)
      modifySTRef' (mDebug m) (\(r, l) -> (r, (show y <> ":" <> show x <> " " <> what <> "  AN " <> show c1 <> show c2 <> "  " <> st) : l))
    _ -> pure ()
 where
  hex2 v = let t = showHex v "" in replicate (2 - length t) '0' <> t
  hex4 v = let t = showHex v "" in replicate (4 - length t) '0' <> t

-- | Log every cycle of lines @y0@ to @y1@ (bus access, AN codes) into
-- 'mDebug' (a debugging aid; tracing stays on meanwhile).
debugLines :: Machine s -> Maybe (Int, Int) -> ST s ()
debugLines m r = do
  writeSTRef (mDebug m) (r, [])
  set m iTraceOn (maybe 0 (const 1) r)

endCpuCycle :: Machine s -> Int -> ST s ()
endCpuCycle m t = do
  set m iBegun 0
  set m iLastCpu t
  endCycle m

cpuRead :: Machine s -> Word16 -> ST s Word8
cpuRead m a = do
  t <- syncCpu m
  pg <- get m iPage
  when (pg == 2 && a `shiftR` 8 /= 0xD5) (set m iPage 0)
  -- a vector read: the NMI owed is taken (or, read the cycle after the
  -- push of P, dropped)
  when (a == 0xFFFA || a == 0xFFFC || a == 0xFFFE) (set m iNmiOwed 0)
  ovl <- readSTRef (mOvl m)
  cas <- casRead m a
  v <- case cas of
   Just c -> pure c
   Nothing -> case ovl of
    Just (img, end, _) | not (isIo a) -> do
      when (a == end) (writeSTRef (mOvl m) (Just (img, end, True)))
      pure (img U.! (fromIntegral a .&. (U.length img - 1)))
    _ -> readBus m t a
  set m iBus (fromIntegral v)
  trace m (Access a v False True)
  endCpuCycle m t
  pure v

cpuWrite :: Machine s -> Word16 -> Word8 -> ST s ()
cpuWrite m a v = do
  t <- syncCpu m
  writeBus m t a v
  set m iBus (fromIntegral v)
  trace m (Access a v True True)
  endCpuCycle m t

-- * Memory map

-- | PORTB as driven: inputs pulled up.
portB :: Machine s -> ST s Word8
portB m = do
  o <- get m iOrb
  d <- get m iDdrb
  pure (fromIntegral ((o .&. d) .|. complement d))

cartAt :: Config -> Bool -> Int -> Word16 -> Maybe Word8
cartAt cfg stub bank a = case cfgCart cfg of
  Just c@(Cart t _) | a >= cartBase t && a < 0xC000 -> Just (cartByte c bank a)
  _ | stub && a >= 0xA000 && a < 0xC000 -> Just (stubCart a)
    | otherwise -> Nothing

-- | The stub cartridge that starts the XEX loader: $FF but its header.
stubCart :: Word16 -> Word8
stubCart a = case a of
  0xBFFA -> 0x00 -- start $D500
  0xBFFB -> 0xD5
  0xBFFC -> 0x00 -- a cartridge is present
  0xBFFD -> 0x04 -- start it; no disk boot
  0xBFFE -> 0xEF -- init: the loader's RTS at $D5EF
  0xBFFF -> 0xD5
  _ -> 0xFF

-- | What a ROM shows at @a@, if one is mapped there (@stub@: the XEX
-- loader's stub cartridge is in; @bank@: the cartridge's bank register).
romAt :: Config -> Bool -> Int -> Word8 -> Word16 -> Maybe Word8
romAt cfg stub bank pb a
  | a >= 0x5000 && a < 0x5800 && osOn && not (testBit pb 7) = Just (os (a - 0x5000 + 0x1000))
  | a >= 0x8000 && a < 0xC000, Just v <- cartAt cfg stub bank a = Just v
  | a >= 0xA000 && a < 0xC000 && not (testBit pb 1), Just b <- cfgBasic cfg = Just (BS.index b (fromIntegral (a - 0xA000)))
  | a >= 0xC000 && osOn && (a < 0xD000 || a >= 0xD800) = Just (os (a - 0xC000))
  | otherwise = Nothing
 where
  osOn = testBit pb 0
  os i = BS.index (cfgOs cfg) (fromIntegral i)

isIo :: Word16 -> Bool
isIo a = a >= 0xD000 && a < 0xD800

readBus :: Machine s -> Int -> Word16 -> ST s Word8
readBus m _t a
  | isIo a = case a `shiftR` 8 of
      0xD0 -> do
        c <- rd5 m
        v <- gtiaRead (mGtia m) c a
        event m (EvGtiaRead a c v)
        pure v
      0xD2 -> do
        v <- pokeyRead (mPokey m) a
        event m (EvPokeyRead a v)
        pure v
      0xD3 -> piaRead m a
      0xD4 -> do
        v <- anticRead (mAntic m) a
        event m (EvAnticRead a v)
        pure v
      0xD5 -> loaderRead m a
      _ -> pure 0xFF
  | otherwise = do
      pb <- portB m
      stub <- (/= 0) <$> get m iStub
      bank <- get m iCartBank
      case romAt (mCfg m) stub bank pb a of
        Just v -> pure v
        Nothing -> MV.unsafeRead (mRam m) (fromIntegral a)

-- | RD5 (TRIG3): a cartridge at $A000-$BFFF.
rd5 :: Machine s -> ST s Bool
rd5 m = case cfgCart (mCfg m) of
  Just _ -> pure True
  Nothing -> (/= 0) <$> get m iStub

loaderRead :: Machine s -> Word16 -> ST s Word8
loaderRead m a = do
  pg <- get m iPage
  case cfgXex (mCfg m) of
    Just (XexBoot img file) | pg /= 0 -> case a .&. 0xFF of
      0xFE -> do
        i <- get m iXexPos
        set m iXexPos (i + 1)
        pure (if i < BS.length file then BS.index file i else 0xFF)
      0xFF -> do
        i <- get m iXexPos
        pure (if i >= BS.length file then 0xFF else 0x7F)
      lo -> pure (BS.index img (fromIntegral lo))
    _ -> pure 0xFF

loaderWrite :: Machine s -> Word16 -> ST s ()
loaderWrite m a = do
  pg <- get m iPage
  when (pg /= 0) $ case a .&. 0xFF of
    0xFE -> set m iStub 0
    0xFF -> set m iPage 2
    _ -> pure ()

writeBus :: Machine s -> Int -> Word16 -> Word8 -> ST s ()
writeBus m t a v
  | isIo a = case a `shiftR` 8 of
      0xD0 -> do
        (_, x) <- anticPosition (mAntic m)
        gtiaWrite (mGtia m) x a v
        event m (EvGtiaWrite x a v)
      0xD2 -> pokeyWrite (mPokey m) a v >> event m (EvPokeyWrite a v)
      0xD3 -> piaWrite m a v
      0xD4 -> anticWrite (mAntic m) t a v >> event m (EvAnticWrite a v)
      -- an XEGS cartridge's bank register ('cartBanks'), else the loader page
      0xD5 -> case cfgCart (mCfg m) of
        Just (Cart ct _) | isXegs ct -> set m iCartBank (fromIntegral v .&. (cartBanks ct - 1))
        _ -> loaderWrite m a
      _ -> pure ()
  | otherwise = do
      pb <- portB m
      stub <- (/= 0) <$> get m iStub
      bank <- get m iCartBank
      case romAt (mCfg m) stub bank pb a of
        Just _ -> pure ()
        Nothing -> MV.unsafeWrite (mRam m) (fromIntegral a) v

piaRead :: Machine s -> Word16 -> ST s Word8
piaRead m a = fromIntegral <$> case a .&. 3 of
  0 -> do
    ctl <- get m iPactl
    if testBit ctl 2
      then do
        set m iPactl (ctl .&. 0x3F)
        o <- get m iOra
        d <- get m iDdra
        j <- get m iJoy
        pure ((j .&. (o .|. complement d)) .&. 0xFF)
      else get m iDdra
  1 -> do
    ctl <- get m iPbctl
    if testBit ctl 2
      then do
        set m iPbctl (ctl .&. 0x3F)
        o <- get m iOrb
        d <- get m iDdrb
        -- inputs: bits 0, 1, 7 pulled up; 2-6 float at their last output
        pure (((o .&. d) .|. (complement d .&. (0x83 .|. o))) .&. 0xFF)
      else get m iDdrb
  2 -> get m iPactl
  _ -> get m iPbctl

piaWrite :: Machine s -> Word16 -> Word8 -> ST s ()
piaWrite m a v0 = case a .&. 3 of
  0 -> get m iPactl >>= \c -> set m (if testBit c 2 then iOra else iDdra) v
  1 -> get m iPbctl >>= \c -> set m (if testBit c 2 then iOrb else iDdrb) v
  2 -> do
    -- CA2: an output pulled low (110) leaves an edge that a later
    -- positive-edge input mode (01x) sees (HRM 2.5, "Spurious interrupts";
    -- Altirra's rules)
    case v .&. 0x38 of
      0x30 -> set m iEdgeA 1
      x | x == 0x10 || x == 0x18 -> do
            e <- get m iEdgeA
            set m iEdgeA 0
            when (e /= 0) (get m iPactl >>= set m iPactl . (.|. 0x40))
        | x == 0x20 || x == 0x38 -> pure ()
        | otherwise -> set m iEdgeA 0
    c <- get m iPactl
    let c' = if testBit v 5 then c .&. complement 0x40 else c
    set m iPactl ((c' .&. 0xC0) .|. (v .&. 0x3F))
  _ -> do
    -- CB2 (the SIO command line): a low-to-high output step leaves an edge
    -- the next input mode sees
    cb2 <- get m iCb2
    case v .&. 0x38 of
      x | x < 0x20 -> do
            e <- get m iEdgeB
            set m iEdgeB 0
            when (e /= 0) (get m iPbctl >>= set m iPbctl . (.|. 0x40))
            set m iCb2 0
        | x == 0x20 -> set m iEdgeB 0
        | x == 0x28 -> set m iEdgeB 0 >> set m iCb2 1
        | x == 0x30 -> set m iEdgeB 0 >> set m iCb2 2
        | otherwise -> do
            when (cb2 == 2) (set m iEdgeB 1)
            set m iCb2 1
    c <- get m iPbctl
    let c' = if testBit v 5 then c .&. complement 0x40 else c
    set m iPbctl ((c' .&. 0xC0) .|. (v .&. 0x3F))
 where
  v = fromIntegral v0

-- | The PIA's IRQ output: a flag (bit 7 CA1/CB1, bit 6 CA2/CB2 in input
-- mode) with its enable (bit 0, bit 3).
piaIrq :: Machine s -> ST s Bool
piaIrq m = do
  a <- get m iPactl
  b <- get m iPbctl
  let on c = c .&. 0x68 == 0x48 || c .&. 0x81 == 0x81
  pure (on a || on b)

-- * Save states (side track N, "A800.Spec.SaveState")

-- | The ROM overlay (the PS's restore routine): while set, every CPU read
-- outside the I/O area answers its byte at the address modulo its size
-- (a power of two); a read of the end address is noted ('overlaySeen').
setOverlay :: Machine s -> Maybe (U.Vector Word8, Word16) -> ST s ()
setOverlay m o = writeSTRef (mOvl m) ((\(img, end) -> (img, end, False)) <$> o)

overlaySeen :: Machine s -> ST s Bool
overlaySeen m = maybe False (\(_, _, s) -> s) <$> readSTRef (mOvl m)

-- | Between two steps: no interrupt about to be taken or owed (the
-- 6502's poll of the last instruction's second-to-last cycle, an NMI
-- edge not taken, the IRQ line now), the CPU not jammed, the loader's
-- page not on its way out (it goes at the next read outside it).
machineQuiet :: Machine s -> ST s Bool
machineQuiet m = do
  p2 <- get m iPoll2
  owed <- get m iNmiOwed
  irq <- (||) <$> pokeyIrq (mPokey m) <*> piaIrq m
  jam <- cpuJammed (mCpu m)
  pg <- get m iPage
  pure (p2 == 0 && owed == 0 && not irq && not jam && pg /= 2)

-- | The executable loader: the stub cartridge there (1), the page there
-- (1; 2 going), the port's position in the file.
loaderSnapshot :: Machine s -> ST s [Int]
loaderSnapshot m = mapM (get m) [iStub, iPage, iXexPos]


-- | The PIA: PACTL, PBCTL, ORA, DDRA, ORB, DDRB, the edges (bit 0 CA2, bit
-- 1 CB2), CB2 as driven.
piaSnapshot :: Machine s -> ST s [Int]
piaSnapshot m = do
  v <- mapM (get m) [iPactl, iPbctl, iOra, iDdra, iOrb, iDdrb]
  ea <- get m iEdgeA
  eb <- get m iEdgeB
  cb2 <- get m iCb2
  pure (v <> [ea .|. (eb `shiftL` 1), cb2])

-- * Peeks (no side effects, no cycles)

-- | What ANTIC's DMA reads at @a@: memory as mapped (no side effects; the
-- I/O area reads $FF).
dmaRead :: Machine s -> Word16 -> ST s Word8
dmaRead m a
  | isIo a = pure 0xFF
  | otherwise = do
      pb <- portB m
      stub <- (/= 0) <$> get m iStub
      bank <- get m iCartBank
      maybe (peekRam m a) pure (romAt (mCfg m) stub bank pb a)

peekRam :: Machine s -> Word16 -> ST s Word8
peekRam m a = MV.unsafeRead (mRam m) (fromIntegral a)

-- | What the CPU would read, without side effects (I/O reads as $FF).
peekCpu :: Machine s -> Word16 -> ST s Word8
peekCpu m a
  | isIo a = pure 0xFF
  | otherwise = do
      pb <- portB m
      stub <- (/= 0) <$> get m iStub
      bank <- get m iCartBank
      maybe (peekRam m a) pure (romAt (mCfg m) stub bank pb a)

pokeRam :: Machine s -> Word16 -> Word8 -> ST s ()
pokeRam m a = MV.unsafeWrite (mRam m) (fromIntegral a)

