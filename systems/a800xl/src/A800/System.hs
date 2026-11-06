{-|
The 800XL between a board's inputs and its picture and sound (M4 steps
4.7 and 4.8): what "A800.Top" (the ZedBoard) and "A800.ArtyTop" (the Arty
Z7-20) share, so the two can't drift apart.

* __Software__: the OS, BASIC, the loader page and an executable come
  from the block RAM images @a800-image@ makes ('imageFiles'; the
  executable from @XEX=@, else none: BASIC's Ready prompt). The PS
  replaces the program over the PS link while holding the core
  ("A800.Loader": an XEX, a CAR or a raw 8/16 KiB cartridge from link
  address 0), from the SD card menu (@/a800xl@) or @make load
  SYSTEM=a800xl@; when hold falls the core starts from power-on with it
  (two clocks later: the loaded configuration must be there), or stays
  held if it was refused. With save states, a hold that writes no file
  (a state's load) keeps the program.
* __PS link__: the menu's text overlay over the picture; the control
  register's pause stops the cycle enable (the machine stands still) and
  releases every input.
* __Timing__: the core gets a machine-cycle enable ('a800CycleEnable',
  every 14 or 15 pixel clocks).
* __Input__: joystick 1 from the board (stick and trigger, given) ORed
  with the PS's (the @[a800xl]@ key map); the console keys from the board
  ORed with the PS's; the keyboard as the key matrix from the PS
  ("A800.Loader", 'keysBase').
* __Sound__: POKEY's level through the output curve and the filters
  ("A800.Audio"), signed 16-bit mono PCM.
* __Save states__ (side track N, "A800.Hooks"): with 'WithStateHooks'
  (the Arty's top) the core freezes for the PS, its table, RAM and the
  ROM overlay come through the link's windows, and the state's bytes go
  out on the byte channel ('soByte'); with 'NoStateHooks' (the
  ZedBoard's) the machine is as it was.
-}
module A800.System
  ( systemA800
  , imageFiles
  , cfgImage
  , Board (..)
  , SysOut (..)
  , CartMem (..)
  , a800System
  , ramClearStep
  ) where

import Clash.Prelude
import Data.Maybe (isJust)

import A800.Audio (audioFilter, pokeySample)
import A800.CartDdr (cartDdr)
import A800.Core
import A800.Display
import A800.Hooks (HookOut (..), a800StateHooks)
import Platform.Speed (speedEnable)
import A800.Loader
import Platform.Audio.Volume (volumeMono)
import Platform.Mem.Axi (AxiRdM2S, AxiRdS2M, axiRdM2SIdle, axiRdS2MIdle)
import Platform.Mem.Ddr (DdrAddr)
import Platform.Ps.Link
import Platform.SaveState (StateHooks (..))
import Platform.Video.TextOverlay (textOverlay)
import Platform.Video.VgaTiming

-- | The 800XL on @PS_IN@ (reserved in docs/ARCHITECTURE.md).
systemA800 :: SystemId
systemA800 = 5

imageFiles :: (FilePath, FilePath, FilePath, FilePath, FilePath)
imageFiles = ("build/a800xl/os.bin", "build/a800xl/basic.bin", "build/a800xl/cart.bin", "build/a800xl/loader.bin", "build/a800xl/xex.bin")

cfgImage :: FilePath
cfgImage = "build/a800xl/cfg.bin"

-- | Where the cartridge is (side track Q, 2026-10-21): block RAM (16
-- KiB: standard cartridges only; the ZedBoard), or DDR through HP0 (every
-- cartridge, XEGS too: the Arty), HP0's answers given.
data CartMem dom = CartBram | CartDdr (Signal dom AxiRdS2M)

-- | The media region (docs/ARCHITECTURE.md): the PS writes the loaded file
-- here, as for the NES's and the Game Boy's cartridges.
cartBase :: DdrAddr
cartBase = 0x02000000

-- | What a board gives: joystick 1 (up, down, left, right, trigger
-- pressed) and the console keys held (START, SELECT, OPTION).
data Board = Board
  { bUp, bDown, bLeft, bRight, bTrigger :: Bool
  , bConsole :: BitVector 3
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFDataX

data SysOut dom = SysOut
  { soTiming :: Signal dom VgaOut      -- ^ 640 × 480, the picture's timing
  , soRgb    :: Signal dom Rgb888      -- ^ its colour (the menu over it), as 'soTiming'
  , soPcm    :: Signal dom (Signed 16) -- ^ the sound, at the menu's volume (side track L)
  , soOk     :: Signal dom Bool        -- ^ the program was accepted (or nothing loaded yet)
  , soFrame  :: Signal dom Bool        -- ^ toggles every 800XL frame
  , soHold   :: Signal dom Bool        -- ^ the loader holds the core
  , soJoy    :: Signal dom (Unsigned 8) -- ^ PIA port A as the core sees it (0 = pushed)
  , soByte   :: Signal dom (Maybe (BitVector 8)) -- ^ the byte channel (save states' reads; nothing without them)
  , soDdr    :: Signal dom AxiRdM2S    -- ^ HP0's read requests (idle with the cartridge in block RAM)
  }

-- | The 800XL from the synchronised PS link lines and the board's inputs,
-- with save states or as before.
a800System :: HiddenClockResetEnable dom => StateHooks -> CartMem dom -> Signal dom PsGpio -> Signal dom Board -> SysOut dom
a800System hooks cartMem ps board = SysOut timing rgb (volumeMono vol pcm) cfgOk frameTog hold joy stateByte ddrOut
 where
  link  = psLink ps
  ctrl  = psControl link
  sets  = psSettings link
  vol   = psVolume <$> sets
  pause = pcPause <$> ctrl
  -- loading (A800.Loader): the file while held, what it is when hold
  -- falls; the keyboard while running
  hold  = pgHold <$> ps
  holdRose = (&&) <$> hold <*> (not <$> register False hold)
  holdFell = (&&) <$> register False hold <*> (not <$> hold)
  ldst  = register loadInit (loadStep <$> ldst <*> bundle (holdRose, hold, link))
  ldw   = loadRoute <$> ldst <*> hold <*> link
  -- the configuration after the last load: Nothing before the first (the
  -- built-in one runs), then what was loaded (Nothing: refused). With save
  -- states a hold that wrote no file keeps the program (a state's load
  -- holds the core and writes only through the state's windows; the
  -- cartridge and executable RAMs keep what they hold)
  newFile = (\l -> hooks == NoStateHooks || lsLen l > 0) <$> ldst
  fromDdr = case cartMem of CartBram -> False; CartDdr _ -> True
  loaded = register Nothing (mux ((&&) <$> holdFell <*> newFile) (Just . loadedCfgFor fromDdr <$> ldst) loaded)
  builtIn = cfgOfWord <$> blockRamFile (SNat @1) cfgImage (pure (0 :: Index 1)) (pure Nothing)
  cfg   = (\ld b -> case ld of Just (Just c') -> c'; _ -> b) <$> loaded <*> builtIn
  cfgOk = (\ld -> case ld of Just Nothing -> False; _ -> True) <$> loaded
  keys  = register keysInit (keysStep <$> keys <*> bundle (hold, link))
  -- PIA port A: stick 1 in bits 0-3 (up, down, left, right; 0 = pushed):
  -- the board's or the PS's; stick 2 released
  joy   = (\bd k p -> if p then 0xFF else
              let j = kJoy k
                  b i x = if x || testBit j i then bit i else 0
              in complement (b 0 (bUp bd) .|. b 1 (bDown bd) .|. b 2 (bLeft bd) .|. b 3 (bRight bd)) :: Unsigned 8)
            <$> board <*> keys <*> pause
  trig  = (\bd k p -> if (bTrigger bd || testBit (kJoy k) 4) && not p then 1 else 0) <$> board <*> keys <*> pause
  consol = (\bd k p b -> if p then 0 else bConsole bd .|. kConsole k .|. b) <$> board <*> keys <*> pause <*> bootCon
  keyBits = (\k p b -> if p then (0, 0) else (kKeys k .|. b, kMods k)) <$> keys <*> pause <*> bootKey
  -- a tape boots as the desktop's does ("A800.Desktop"; the OS's cassette
  -- boot): START held for its first 60 frames, then Space (key code 21h)
  -- at frames 120 to 125, counted from the core's start
  -- (a tape alone: with an executable, the executable runs)
  isTapeCfg = (\c -> ccCart c == CkCas && not (ccXex c)) <$> cfg
  bootF = register (0 :: Unsigned 8)
            (mux (not <$> run) 0 (mux (koFrame <$> core) (satSucc SatBound <$> bootF) bootF))
  bootCon = (\t f -> if t && f < 60 then 1 else 0) <$> isTapeCfg <*> bootF
  bootKey = (\t f -> if t && f >= 120 && f < 126 then bit 0x21 else 0) <$> isTapeCfg <*> bootF
  tick  = (&&) <$> speedEnable a800CycleIncrement (psSpeed <$> sets) <*> (not <$> stopped) -- the menu's speed (side track L)
  -- held for the first clocks, until the configuration word is read
  -- (while held the core is at power-on with it: the stub cartridge and
  -- the loader page depend on it), while the PS loads, and when the file
  -- was refused; and for two clocks after hold falls: the loaded
  -- configuration is out a clock after the fall, and the core's last held
  -- clock must already see it
  started = register False (register False (pure True))
  hold1 = register False hold
  hold2 = register False hold1
  holdD = (\a b c' -> a || b || c') <$> hold <*> hold1 <*> hold2
  run   = (\st h ok cl -> st && not h && ok && not cl) <$> started <*> holdD <*> cfgOk <*> clearing
  -- A new file: RAM cleared before the core starts, as switching off
  -- clears it (the board's block RAM keeps it over a load; the XL's OS
  -- took the kept power-up bytes, $033D-$033F, for a Reset key press: a
  -- warm start, no tape boot, and a tape program left in RAM run again
  -- through CASINI; found on the board, 2026-10-26). A save state's hold
  -- writes no file and keeps RAM. 64 Ki clocks, through RAM's port as the
  -- hooks' while frozen (the core held at power-on by 'run', not frozen)
  clr = register Nothing (ramClearStep <$> holdFell <*> newFile <*> clr)
  clearing = isJust <$> clr
  bram = blockMems imageFiles (lwCart <$> ldw) (lwXex <$> ldw)
  -- the cartridge from DDR: the file the PS wrote at 'cartBase' (a .car's
  -- image after its 16-byte header), the cache emptied while held. One
  -- reader, bound here (not inside a case on the board: Clash made a copy
  -- of a case branch per selector of its result, two readers, one's cache
  -- removed by Vivado as unreachable; the NES binds its prgDdr the same way)
  s2mIn = case cartMem of CartDdr s2m -> s2m; CartBram -> pure axiRdS2MIdle
  imgBase = (\l -> if isCar (lsHead l) || isTape (lsHead l) then cartBase + 16 else cartBase) <$> ldst
  -- the core's cartridge reads and the tape's (the tape's answer beside
  -- the core reads the tape image through this reader: "A800.CasUnit"),
  -- from its output (the answers and the ready flag are registers: no
  -- loop through the core)
  cartRq = coreCartRq
  ((cartB, cartOk), cartM2s) = cartDdr imgBase hold s2mIn cartRq
  mems = if fromDdr then bram { msCart = const (cartB, cartOk) } else bram
  ddrOut = if fromDdr then cartM2s else pure axiRdM2SIdle
  (core, coreSt, ramD, coreCartRq) = a800CoreWith (coreInit (CoreCfg False CkNone False 0)) mems cfg run tick joy trig consol (fst <$> keyBits) (snd <$> keyBits)
                           frozen ovl
                           ((\c p -> case c of Just a -> Just (a, Just 0); Nothing -> p) <$> clr <*> ramPort)
  -- save states (side track N, "A800.Hooks"): the credit, the core's
  -- steps, the overlay, RAM's port, the byte channel; without them each
  -- is as it was
  ho = a800StateHooks link pause (not <$> run) coreSt core ramD
  (stopped, frozen, ovl, ramPort, stateByte) = case hooks of
    NoStateHooks -> (pause, pure False, pure Nothing, pure Nothing, pure Nothing)
    WithStateHooks -> (hoStopped ho, hoFrozen ho, hoOvl ho, hoRam ho, hoByte ho)
  vga   = vgaTiming vga640x480
  fb    = a800FrameBuffers (koPixel <$> core) (koFrame <$> core) (vgaVBlankStart <$> vga) rdAddr
  (rdAddr, stream) = a800Stream vga fb
  -- the PS menu's text screen over the picture, while it's shown
  (timing, rgb) = textOverlay (overlayWrite <$> link) (pcOverlay <$> ctrl) stream
  -- sound: the curve and the filters
  pcm   = audioFilter (register 0 (pokeySample . koLevel <$> core))
  frameTog = register False (mux (koFrame <$> core) (not <$> frameTog) frameTog)

-- | The RAM clear's step (a800System): started when a held load of a new
-- file ends (hold falling, a file written), then one address a clock up to
-- the last; 'Nothing' while not clearing.
ramClearStep :: Bool -> Bool -> Maybe (Unsigned 16) -> Maybe (Unsigned 16)
ramClearStep fell newF c = case c of
  _ | fell && newF -> Just 0
  Just a | a == maxBound -> Nothing
         | otherwise -> Just (a + 1)
  Nothing -> Nothing
