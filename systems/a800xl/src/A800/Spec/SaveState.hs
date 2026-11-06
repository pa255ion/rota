{-|
Save states on the reference model (side track N, PLAN.md N.4): the
state table ("A800.StateTable") of a machine at rest, the rest
condition, RAM's image, and a run through the ROM overlay (the PS's
restore routine, @platform/ps/systems/a800_restore.zig@), as the Game
Gear's ("Gg.Spec"). The core's tap and rest condition (N.5) are checked
against these.
-}
module A800.Spec.SaveState
  ( specTable
  , specAtRest
  , restReasons
  , runOverlay
  , ramImage
  , loadRam
  , runToFrame
  ) where

import Prelude

import Control.Monad.ST (ST)
import Data.Bits
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MV
import Data.Word

import A800.Spec.Antic
import A800.Spec.Gtia
import A800.Spec.Machine
import A800.Spec.Pokey
import A800.StateTable
import Mos6502.Spec (Regs (..), getRegs)

-- | The state table of a machine between two steps.
specTable :: forall s. Machine s -> ST s (U.Vector Word8)
specTable m = do
  r <- getRegs (mCpu m)
  tb <- MV.replicate tableSize 0
  let put :: Int -> Int -> ST s ()
      put o v = MV.write tb o (fromIntegral (v .&. 0xFF))
      put16 o v = put o v >> put (o + 1) (v `shiftR` 8)
  mapM_ (uncurry put)
    [ (tA, fromIntegral (rA r)), (tX, fromIntegral (rX r)), (tY, fromIntegral (rY r))
    , (tS, fromIntegral (rS r)), (tP, fromIntegral (rP r)) ]
  put16 tPC (fromIntegral (rPC r))
  an <- anticSnapshot (mAntic m)
  put16 tLine (asLine an)
  mapM_ (uncurry put)
    [ (tCycle, asCycle an), (tNmist, asNmist an), (tDmactl, asDmactl an), (tChactl, asChactl an)
    , (tHscrol, asHscrol an), (tVscrol, asVscrol an), (tPmbase, asPmbase an), (tChbase, asChbase an)
    , (tNmien, asNmien an), (tPenh, asPenh an), (tPenv, asPenv an), (tIrPrev, asIrPrev an), (tHires, asHires an) ]
  put16 tDlist (asDlist an)
  put16 tMsc (asMsc an)
  ldr <- loaderSnapshot m
  let (stub, pg, pos) = case ldr of
        [a, b, c] -> (a, b, c)
        _ -> (0, 0, 0)
  put tLoader (stub .|. (if pg /= 0 then 2 else 0))
  mapM_ (\i -> put (tXexPos + i) (pos `shiftR` (8 * i))) [0 .. 2]
  pia <- piaSnapshot m
  mapM_ (uncurry put) (zip [tPactl, tPbctl, tOra, tDdra, tOrb, tDdrb, tEdges, tCb2] pia)
  pk <- pokeySnapshot (mPokey m)
  mapM_ (uncurry put) (zip ([tAudf .. tAudf + 3] <> [tAudc .. tAudc + 3] <> [tAudctl, tSkctl, tIrqen, tIrqst, tSkstat, tKbcode, tSerout, tSerial]) pk)
  gs <- gtiaSnapshot (mGtia m)
  mapM_ (uncurry put) (zip [tGtia ..] (gsRegs gs))
  mapM_ (uncurry put) (zip [tLatch ..] (gsLatches gs))
  mapM_ (uncurry put) (zip [tColl ..] (gsColl gs))
  put tTrigLatch (gsTrigLatch gs)
  U.freeze tb

-- | Whether a state can be taken here (between two steps): vertical blank
-- (lines 248-261 and 0-7; on line 248 from cycle 64 on, after the NMI and
-- with room for the routine's late writes), no interrupt about to be
-- taken or owed (the 6502's poll, an NMI edge not serviced, the IRQ
-- line), no WSYNC armed or held, no CHBASE write on its way, GTIA's
-- write queue empty, POKEY's serial port idle and no STIMER on its way,
-- the executable loader's page not on its way out.
specAtRest :: Machine s -> ST s Bool
specAtRest m = do
  r <- restReasons m
  pure (and r)

-- | 'specAtRest''s conditions one by one (vertical blank, ANTIC, GTIA,
-- POKEY, the machine), for tests that look for a moment at rest.
restReasons :: Machine s -> ST s [Bool]
restReasons m = do
  an <- anticSnapshot (mAntic m)
  gs <- gtiaSnapshot (mGtia m)
  pk <- pokeyQuiet (mPokey m)
  quiet <- machineQuiet m
  let y = asLine an
      x = asCycle an
      vblank = (y >= 249 || y < 8) || (y == 248 && x >= 64)
  pure [vblank, asQuiet an, gsQueueEmpty gs, pk, quiet]

-- | Load a state as the PS does: a machine with this configuration
-- powered on with the ROM overlay (the PS's restore routine) armed, RAM
-- written, then run: every CPU read outside the I/O area answers the
-- overlay's byte (at the address modulo its size), the reset vector
-- included, until the instruction that read @end@ has ended; the machine
-- then stands before its next fetch, from its own memory. 'Left' if the
-- routine didn't end within @limit@ steps; else the machine and the
-- machine cycles it ran.
runOverlay :: Config -> U.Vector Word8 -> U.Vector Word8 -> Word16 -> Int -> ST s (Either String (Machine s, Int))
runOverlay cfg ram ovl end limit = do
  m <- newMachineOverlay cfg (Just (ovl, end))
  loadRam m ram
  let go k
        | k >= limit = setOverlay m Nothing >> pure (Left ("the restore routine didn't end within " <> show limit <> " steps"))
        | otherwise = do
            _ <- stepCpu m
            done <- overlaySeen m
            if done
              then setOverlay m Nothing >> (Right . (m,) <$> machineCycles m)
              else go (k + 1)
  go (0 :: Int)

-- | RAM, all 64 KiB (under the ROMs too).
ramImage :: Machine s -> ST s (U.Vector Word8)
ramImage m = U.generateM 0x10000 (peekRam m . fromIntegral)

-- | RAM written (as the PS does through its window while the core is held).
loadRam :: Machine s -> U.Vector Word8 -> ST s ()
loadRam m v = U.imapM_ (\i b -> pokeRam m (fromIntegral i) b) v

-- | Run on until a frame has ended (line 261 to 0).
runToFrame :: Machine s -> ST s ()
runToFrame m = do
  f0 <- anticFrames (mAntic m)
  let go = do
        _ <- stepCpu m
        f <- anticFrames (mAntic m)
        if f == f0 then go else pure ()
  go
