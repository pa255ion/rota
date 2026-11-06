{-|
Save states on the reference model (side track N, PLAN.md N.7): the
state table ("Nes.StateTable") of a machine at rest, the rest condition,
the memories' images, and a run through the ROM overlay (the PS's
restore routine, @platform/ps/systems/nes_restore.zig@), as the 800XL's
("A800.Spec.SaveState"). The core's tap and rest condition (N.8) are
checked against these.
-}
module Nes.Spec.SaveState
  ( specTable
  , specAtRest
  , restReasons
  , Images (..)
  , images
  , runOverlay
  , runToFrame
  ) where

import Prelude

import Control.Monad.ST (ST)
import Data.Bits
import qualified Data.ByteString as BS
import Data.Maybe (fromMaybe)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MV
import Data.Word

import Mos6502.Spec (Regs (..), getRegs)
import Nes.Spec.Apu
import Nes.Spec.Machine
import Nes.Spec.Mapper
import Nes.Spec.Ppu
import Nes.StateTable

-- | The PPU's named state ('ppuSnap').
ppuField :: PpuSnap -> String -> Int
ppuField sn n = fromMaybe (error ("no PPU field " <> n)) (lookup n (snFields sn))

-- | The state table of a machine between two steps.
specTable :: forall s. Machine s -> ST s (U.Vector Word8)
specTable m = do
  r <- getRegs (mCpu m)
  tb <- MV.replicate tableSize 0
  let put :: Int -> Int -> ST s ()
      put o v = MV.write tb o (fromIntegral (v .&. 0xFF))
      put16 o v = put o v >> put (o + 1) (v `shiftR` 8)
      puts o = mapM_ (uncurry put) . zip [o ..]
  mapM_ (uncurry put)
    [ (tA, fromIntegral (rA r)), (tX, fromIntegral (rX r)), (tY, fromIntegral (rY r))
    , (tS, fromIntegral (rS r)), (tP, fromIntegral (rP r)) ]
  put16 tPC (fromIntegral (rPC r))
  sn <- ppuSnap (mPpu m)
  let f = ppuField sn
  put16 tLine (f "line")
  put16 tDot (f "dot")
  put tOdd (f "odd")
  n <- machineCycles m
  put tParity (n .&. 1)
  machineStrobe m >>= put tStrobe
  mapM_ (uncurry put)
    [ (tCtrl, f "ctrl"), (tMask, f "mask")
    , (tStatus, f "vbl" `shiftL` 7 .|. f "spr0" `shiftL` 6 .|. f "ovf" `shiftL` 5)
    , (tOamAddr, f "oamAddr"), (tFineX, f "x"), (tW, f "w"), (tBuf, f "buf") ]
  ppuLatch (mPpu m) >>= put tLatch
  put16 tV (f "v")
  put16 tT (f "t")
  pal <- mapM (ppuPeekPalette (mPpu m)) [0 .. 31]
  puts tPal (map fromIntegral pal)
  ap <- apuSnapshot (mApu m)
  puts tApu (apRegs ap)
  put tEnabled (apEnabled ap)
  put tFrame (apMode ap `shiftL` 7 .|. apInhibit ap `shiftL` 6)
  put16 tSeq (apSeq ap)
  put tFrameIrq (apIrq ap)
  put tDmcIrq (apDmcIrq ap)
  puts tLengths (apLengths ap)
  put16 tDmcCount (apDmcCount ap)
  -- (the buffer's byte only while it holds one: it keeps the last after)
  mapM_ (uncurry put) [ (tDmcBits, apDmcBits ap), (tDmcFull, apDmcFull ap), (tDmcBuf, apDmcBuf ap * apDmcFull ap)
                      , (tDmcShift, apDmcShift ap), (tDmcSilence, apDmcSilence ap) ]
  ms <- mapperSnapshot (mCart m)
  let k = msKind ms
      rg i = msRegs ms U.! i
  put tMapper k
  put tMirror (msMirror ms)
  put tBoard ((if msChrRam ms then 1 else 0) .|. (if msPrgRam ms then 2 else 0))
  -- (Nes.Spec.Mapper's register slots)
  case k of
    1 -> puts tMmc1 (map rg [0 .. 5])
    4 -> puts tMmc3 (map rg ([8 .. 22] <> [24]))
    5 -> puts tMmc5 (map rg ([26 .. 55] <> [61, 62])) >> put tMmc5Line (rg 56)
    _ | k `elem` [2, 3, 7] -> put tBank (rg 7)
      | otherwise -> pure ()
  U.freeze tb

-- | Whether a state can be taken here (between two steps): lines 243 to
-- 260 of the vertical blank (from 241's flag, room for the routine's late
-- writes), no $2006 or $2007 access on its way in the PPU (nor its sprite
-- flags, an OAM corruption of a row but 0), the APU with nothing on its way (no $4017
-- write, no flag clear, the DMC idle, its IRQ flag clear) and its frame
-- counter's flag as the routine can leave it (not on its last cycles, nor
-- just after an unacknowledged wrap), no interrupt about to be taken or
-- owed, no DMA, no $4016 write on its way.
specAtRest :: Machine s -> ST s Bool
specAtRest m = and <$> restReasons m

-- | 'specAtRest''s conditions one by one (vertical blank, PPU, APU, the
-- machine), for tests that look for a moment at rest.
restReasons :: Machine s -> ST s [Bool]
restReasons m = do
  sn <- ppuSnap (mPpu m)
  ap <- apuSnapshot (mApu m)
  quiet <- machineQuiet m
  let f = ppuField sn
      line = f "line"
      vblank = line >= 243 && line <= 260
      -- (an OAM corruption of row 0, pending, copies nothing)
      ppuQuiet = all ((== 0) . f) ["rdAle", "rdFill", "wrAle", "wrDo", "vPend", "flagPend"] && f "oamCor" <= 1
      sq = apSeq ap
      frameRisk = apMode ap == 0 && (sq >= 29824 || (apInhibit ap == 0 && apIrq ap == 0 && sq < 24))
      -- a full DMC buffer is fetched again late in the routine's tail,
      -- after the output unit last took a byte: two of its bits (timer
      -- periods, at least 54 cycles each) since then, as the core can tell
      -- (its bits go back to 8 when it takes one)
      dmcOk = apDmcFull ap == 0 || apDmcBits ap <= 6
      apuOk = apQuiet ap && apDmcIrq ap == 0 && not frameRisk && dmcOk
  pure [vblank, ppuQuiet, apuOk, quiet]

-- | The memories a state keeps besides the table: RAM, nametable RAM,
-- OAM, PRG RAM, CHR RAM, ExRAM (the last three empty where the board has
-- none).
data Images = Images
  { imRam, imCiram, imOam, imPrgRam, imChrRam, imExram :: U.Vector Word8 }
  deriving stock (Show, Eq)

images :: Machine s -> ST s Images
images m = do
  ram <- ramImage m
  ciram <- ciramImage m
  oam <- U.fromList <$> mapM (ppuOamAt (mPpu m)) [0 .. 255]
  (prg, chr, ex) <- mapperImages (mCart m)
  pure (Images ram ciram oam prg chr ex)

-- | Load a state as the PS does: the cartridge powered on with the ROM
-- overlay (the PS's restore routine) armed, the memories written but OAM
-- (the routine's DMA fills it), then run: every CPU read of $0000-$1FFF
-- and $6000-$FFFF answers the overlay's byte (at the address modulo its size),
-- the reset vector included, until the instruction that read @end@ has
-- ended; the machine then stands before its next fetch, from its own
-- memory. 'Left' if the routine didn't end within @limit@ steps; else the
-- machine and the CPU cycles it ran.
runOverlay :: BS.ByteString -> Images -> U.Vector Word8 -> Word16 -> Int -> ST s (Either String (Machine s, Int))
runOverlay rom im ovl end limit = do
  em <- newMachineOverlay rom (Just (ovl, end))
  case em of
    Left e -> pure (Left e)
    Right m -> do
      loadRam m (imRam im)
      loadCiram m (imCiram im)
      mapperLoad (mCart m) (imPrgRam im, imChrRam im, imExram im)
      let go k
            | k >= limit = setOverlay m Nothing >> pure (Left ("the restore routine didn't end within " <> show limit <> " steps"))
            | otherwise = do
                _ <- stepCpu m
                done <- overlaySeen m
                if done
                  then setOverlay m Nothing >> (Right . (m,) <$> machineCycles m)
                  else go (k + 1)
      go (0 :: Int)

-- | Run on until a frame has been drawn (line 239 finished).
runToFrame :: Machine s -> ST s ()
runToFrame m = do
  f0 <- ppuFramesDone (mPpu m)
  let go = do
        _ <- stepCpu m
        f <- ppuFramesDone (mPpu m)
        if f == f0 then go else pure ()
  go
