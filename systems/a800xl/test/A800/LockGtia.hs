{-|
Lockstep of the Clash GTIA ("A800.Gtia", as plain Haskell) with the
reference model's (M4 step 4.5): the spec machine's hook reports every
GTIA call (P/M DMA data, register access, each cycle's AN codes); the
Clash GTIA gets the same, and every register read and every picture byte
(two per colour clock) must agree.
-}
module A800.LockGtia
  ( GStepFn
  , lockstepGtia
  ) where

import Prelude

import Control.Monad (when)
import Control.Monad.ST (runST)
import Data.Bits ((.&.))
import Data.STRef

import qualified Clash.Prelude as C

import A800.Gtia
import A800.Spec.Gtia (gtiaPixels)
import A800.Spec.Machine
import Mos6502.Spec (step)

type GStepFn = Gtia -> GtiaIn -> (Gtia, GtiaOut)

-- | Run the machine for @cycles@ with the Clash GTIA alongside (its step
-- function given, for deliberate bugs); 'Left' with the first difference.
lockstepGtia :: GStepFn -> Int -> Config -> Either String Int
lockstepGtia stepFn cycles cfg = runST $ do
  m <- newMachine cfg
  st <- newSTRef gtiaInit
  err <- newSTRef Nothing
  n <- newSTRef (0 :: Int)
  let fail' msg = readSTRef err >>= maybe (writeSTRef err (Just msg)) (const (pure ()))
      run i = do
        g <- readSTRef st
        let (g', o) = stepFn g i
        writeSTRef st g'
        pure o
      cyc x (c1, c2) = do
        g <- readSTRef st
        -- 'gtiaCycle' with the step function under test
        let pair = fromIntegral (((c1 .&. 3) * 4) + (c2 .&. 3))
            h = fromIntegral (2 * x)
            (g1, o1) = stepFn g (GClock h (fromIntegral c1) pair (gPrevPair g))
            (g2, o2) = stepFn g1 (GClock (h + 1) (fromIntegral c2) pair pair)
        k <- readSTRef n
        let g3 = g2 { gPrevPair = pair }
        -- force the whole state every 256 cycles: lazy Vec elements
        -- would otherwise keep old states alive
        if k `mod` 256 == 0 then C.rnfX g3 `seq` writeSTRef st g3 else writeSTRef st g3
        pure (goColour o1, goColour o2)
      hook e = do
        bad <- readSTRef err
        when (bad == Nothing) $ case e of
          EvGtiaMissile x o v -> () <$ run (GMissile (fromIntegral x) o (fromIntegral v))
          EvGtiaPlayer x o k v -> () <$ run (GPlayer (fromIntegral x) o (fromIntegral k) (fromIntegral v))
          EvGtiaWrite x a v -> () <$ run (GWrite (fromIntegral x) (fromIntegral (a .&. 31)) (fromIntegral v))
          EvGtiaRead a c v -> do
            o <- run (GRead (fromIntegral (a .&. 31)) c)
            k <- readSTRef n
            when (fromIntegral (goRead o) /= v) $
              fail' ("cycle " <> show k <> ": read $D0" <> show (a .&. 31) <> " = " <> show (goRead o) <> ", spec " <> show v)
          EvGtiaCycle y x c1 c2 -> do
            modifySTRef' n (+ 1)
            k <- readSTRef n
            (a, b) <- cyc x (c1, c2)
            sa <- gtiaPixels (mGtia m) y (2 * x)
            sb <- gtiaPixels (mGtia m) y (2 * x + 1)
            let toW (p, q) = (fromIntegral p, fromIntegral q)
            when (toW a /= sa || toW b /= sb) $
              fail' ("cycle " <> show k <> " (line " <> show y <> ", cycle " <> show x <> "): colours "
                     <> show (a, b) <> ", spec " <> show (sa, sb))
          _ -> pure ()
  setHook m hook
  let go = do
        _ <- step (mCpu m)
        t <- machineCycles m
        bad <- readSTRef err
        if t >= cycles || bad /= Nothing then pure () else go
  go
  bad <- readSTRef err
  k <- readSTRef n
  pure (maybe (Right k) Left bad)
