{-|
Save states through the CPU on the reference model (step 7.15;
docs/systems/snes.md, "Save states through the CPU"): the state table, the
moment a state can be taken, the memories as the PS writes them, and a
fresh machine restored through a routine in the ROM overlay.

__The table__ ('tableFields'): everything a restore routine puts back, a
field of 4 bytes each (little-endian), whatever its width: the 65C816's
registers and its interrupt lines ('W65c816.Spec.cpuLines'); the 5A22's
and the PPU's internal slots ("Snes.Spec.Machine"; the multiplier's and
divider's shifter, not MPYB and DIVB, which the circuit doesn't keep;
not the pads, which are inputs); the PPU's registers as
last written (2100h-213Fh, the scrolls as 10-bit values, then M7HOFS,
M7VOFS and COLDATA at 40h-42h); the PPU unit's STAT77 flags; the master
clock's phase mod 8 (a DMA starts on a multiple of 8 clocks: a frame of the
odd field without interlace is 4 clocks short, so the same line and dot
come at either phase). The master clock itself is not in it: H, V and the
field place the moment, and the pads' auto-read deadline is kept relative
to it.

__At rest__ ('specAtRest'): between two instructions, in V-blank (from the
line after it begins to 259, as the circuit's freeze, "Snes.StateHooks"),
the pads' auto-read done, no DMA owed, the multiplier and divider idle, no
interrupt seen or owed.
-}
module Snes.Spec.SaveState
  ( tableFields
  , tableSize
  , specTable
  , specAtRest
  , Images (..)
  , images
  , runOverlay
  ) where

import Prelude

import Control.Monad (forM_, when)
import Control.Monad.ST (ST)
import Data.Bits (shiftR, (.&.))
import qualified Data.ByteString as B
import Data.STRef
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MV
import Data.Word (Word8)

import Snes.Cart (Cart)
import Snes.Spec.Apu (apuCopy)
import Snes.Spec.Machine
import Snes.Spec.Ppu (ppuStat77)
import qualified W65c816.Spec as Cpu
import W65c816.Types (Regs (..))

-- | The table's fields: name, offset, size (4 bytes each).
tableFields :: [(String, Int, Int)]
tableFields = zipWith (\i n -> (n, 4 * i, 4)) [0 ..] fieldNames

tableSize :: Int
tableSize = 4 * length fieldNames

cpuNames, slotNames, ppuNames :: [String]
cpuNames = ["A", "X", "Y", "S", "D", "DBR", "PBR", "PC", "P", "E", "run", "nmiLine", "nmiNeed", "pollCur", "pollPrev", "pending"]
ppuNames = [ "ppu" <> hex2 r | r <- [0 .. 0x42 :: Int] ] <> ["stat77", "mc8"]
fieldNames :: [String]
fieldNames = cpuNames <> slotNames <> ppuNames

-- | The machine's slots in the table, in order ('slotList').
slotList :: [(String, Int)]
slotList =
  [ ("hc", sHc), ("vc", sVc), ("field", sField), ("mdr", sMdr), ("nmitimen", sNmitimen), ("rdnmi", sRdnmi)
  , ("timeup", sTimeup), ("wrio", sWrio), ("mpya", sMpya), ("shift", sShift), ("div", sDiv)
  , ("htime", sHtime), ("vtime", sVtime), ("hdmaen", sHdmaen), ("memsel", sMemsel), ("rddiv", sRddiv)
  , ("rdmpy", sRdmpy), ("joy1", sJoyBase), ("joy2", sJoyBase + 1), ("joy3", sJoyBase + 2), ("joy4", sJoyBase + 3)
  , ("joyBusy", sJoyBusyUntil), ("strobe", sStrobe), ("ser1", sSer1), ("ser2", sSer2)
  , ("vaddr", sVaddr), ("vmain", sVmain), ("vlatch", sVlatch), ("cgadd", sCgadd), ("cgLatch", sCgLatch)
  , ("cgFlip", sCgFlip), ("oamadd", sOamadd), ("oamLatch", sOamLatch), ("oamRel", sOamRel), ("wmadd", sWmadd)
  , ("hdmaDo", sHdmaDo), ("hdmaDone", sHdmaDone), ("hlatch", sHlatch), ("vlatch2", sVlatch2), ("latched", sLatched)
  , ("m7Prev", sM7Prev), ("hflip", sHflip), ("vflip", sVflip), ("bgPrev", sBgPrev), ("bgPrev2", sBgPrev2)
  ]
slotNames = map fst slotList

hex2 :: Int -> String
hex2 n = [digit (n `shiftR` 4), digit (n .&. 15)]
 where digit d = "0123456789abcdef" !! d

-- | The table of a machine between two instructions.
specTable :: Machine s -> ST s (U.Vector Word8)
specTable m = do
  r <- Cpu.getRegs (mCpu m)
  (run, nl, nn, pc, pp, pe) <- Cpu.cpuLines (mCpu m)
  let cpuVals = map fromIntegral [rA r, rX r, rY r, rS r, rD r] <> map fromIntegral [rDBR r, rPBR r]
                <> [fromIntegral (rPC r), fromIntegral (rP r), if rE r then 1 else 0, run, nl, nn, pc, pp, pe]
  mc <- slotGet m sMc
  slotVals <- mapM (\(n, i) -> do
                      v <- slotGet m i
                      -- (the auto-read's deadline as clocks from now)
                      pure (if n == "joyBusy" then max 0 (v - mc) else v)) slotList
  regs <- mapM (MV.read (mPpu m)) [0 .. 0x42]
  flags <- ppuStat77 (mPicture m)
  -- (the master clock's phase mod 8: a DMA starts on a multiple of 8)
  pure (U.fromList (concatMap le32 (cpuVals <> slotVals <> regs <> [flags, mc `mod` 8])))
 where
  le32 v = [ fromIntegral (v `shiftR` (8 * k)) | k <- [0 .. 3 :: Int] ]

-- | A state can be taken here (between two instructions).
specAtRest :: Machine s -> ST s Bool
specAtRest m = do
  v <- slotGet m sVc
  vbs <- vblankStart m
  mc <- slotGet m sMc
  joy <- slotGet m sJoyBusyUntil
  dma <- slotGet m sDmaPending
  mpy <- slotGet m sMpyCtr
  dv <- slotGet m sDivCtr
  (_, _, nn, pc, _, pe) <- Cpu.cpuLines (mCpu m)
  pure (v > vbs && v <= 259 && joy <= mc && dma == 0 && mpy == 0 && dv == 0 && nn == 0 && pc == 0 && pe == 0)

-- | The memories as the PS writes them through its windows.
data Images = Images
  { imWram  :: !(U.Vector Word8)
  , imVram  :: !(U.Vector Word8)
  , imCgram :: !(U.Vector Word8)
  , imOam   :: !(U.Vector Word8)
  , imDma   :: !(U.Vector Word8)
  , imSram  :: !(U.Vector Word8)
  } deriving stock (Show, Eq)

images :: Machine s -> ST s Images
images m = Images <$> U.freeze (mWram m) <*> U.freeze (mVram m) <*> U.freeze (mCgram m)
                  <*> U.freeze (mOam m) <*> U.freeze (mDma m) <*> U.freeze (mSram m)

-- | A fresh machine (the boot ROM, the cartridge), restored: powered on
-- with its memories written (as the PS does while it holds the core), the
-- sound module moved whole from @from@ (until 7.15b: its clock's phase
-- made the saved machine's), then the routine run from the overlay until
-- it goes off (at most @limit@ instructions). The machine, or why not;
-- and where it was (frame, line, clock) at each address of @watch@ (the
-- first time it began an instruction there, bank 00h).
runOverlay :: B.ByteString -> Cart -> B.ByteString -> Machine s -> Images -> U.Vector Word8 -> Int -> Int -> [Int]
           -> ST s (Either String (Machine s), [(Int, (Int, Int, Int))])
runOverlay ipl cart file from im ovl end limit watch = do
  m <- newMachine ipl cart file
  let put v img = forM_ [0 .. U.length img - 1] $ \i -> MV.write v i (img U.! i)
  put (mWram m) (imWram im)
  put (mVram m) (imVram im)
  put (mCgram m) (imCgram im)
  -- (OAM's high table is registers in the circuit: the routine writes it)
  put (mOam m) (U.take 512 (imOam im))
  put (mDma m) (imDma im)
  when (U.length (imSram im) == MV.length (mSram m)) (put (mSram m) (imSram im))
  -- (armed while held, as the PS does: the reset's vector is the overlay's)
  writeSTRef (mOverlay m) (Just (Overlay ovl end False))
  reset m
  seen <- newSTRef []
  let go n = do
        o <- readSTRef (mOverlay m)
        rg <- Cpu.getRegs (mCpu m)
        let pc = fromIntegral (rPC rg) :: Int
        hits <- readSTRef seen
        when (rPBR rg == 0 && pc `elem` watch && pc `notElem` map fst hits) $ do
          at <- (,,) <$> slotGet m sFrames <*> slotGet m sVc <*> slotGet m sHc
          writeSTRef seen ((pc, at) : hits)
        case o of
          Nothing -> pure (Right ())
          Just _ | n >= limit -> do
                     r <- Cpu.getRegs (mCpu m)
                     hc <- slotGet m sHc
                     vc <- slotGet m sVc
                     fr <- slotGet m sFrames
                     pure (Left ("the routine didn't end in " <> show limit <> " instructions: PBR:PC "
                                 <> show (rPBR r, rPC r) <> ", E " <> show (rE r) <> ", frame " <> show fr
                                 <> ", line " <> show vc <> ", clock " <> show hc))
                 | otherwise -> stepCpu m >> go (n + 1)
  r <- go (0 :: Int)
  hits <- reverse <$> readSTRef seen
  case r of
    Left e -> pure (Left e, hits)
    Right () -> do
      moveApu from m
      pure (Right m, hits)

-- | The sound module moved whole from @from@ into @m@, its clock's phase
-- and count against @m@'s master clock made what they were against
-- @from@'s (the circuit's 'aRem' and target; 7.15b's routine replaces
-- the rest).
moveApu :: Machine s -> Machine s -> ST s ()
moveApu from m = do
  apuCopy (mApu from) (mApu m)
  mcF <- slotGet from sMc
  phF <- slotGet from sApuPhase
  baF <- slotGet from sApuBase
  mcT <- slotGet m sMc
  let numF = mcF * 5632 + phF
      tgtF = numF `div` 118125 + baF
      remF = numF `mod` 118125
      -- phase: mcT × 5632 + ph ≡ remF (mod 118125), ph in [0, 118125)
      ph = (remF - (mcT * 5632) `mod` 118125) `mod` 118125
      tgtT0 = (mcT * 5632 + ph) `div` 118125
  slotSet m sApuPhase ph
  slotSet m sApuBase (tgtF - tgtT0)
