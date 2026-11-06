{-|
Lockstep of the Clash ANTIC ("A800.Antic", as plain Haskell) with the
reference model's (M4 step 4.5): the spec machine's hook reports each
cycle's start (with the DMA read and the player/missile data it gave
GTIA), the CPU's ANTIC accesses and WSYNC questions, and the cycle's end;
the Clash ANTIC runs the same ticks ('ABegin', 'AFetch', 'APost', 'AEnd',
'ADecode' ...) and every output must agree: whose cycle it is, each DMA
address, the data for GTIA, register reads, WSYNC, the NMI line, the AN
codes.
-}
module A800.LockAntic
  ( AStepFn
  , lockstepAntic
  ) where

import Prelude

import Control.Monad (when)
import Control.Monad.ST (runST)
import Data.Bits ((.&.))
import Data.STRef

import A800.Antic
import A800.Spec.Antic (anticNmi, anticPosition)
import A800.Spec.Machine
import Mos6502.Spec (step)

type AStepFn = Antic -> AnticIn -> (Antic, AnticOut)

-- | Run the machine for @cycles@ with the Clash ANTIC alongside (its step
-- function given, for deliberate bugs); 'Left' with the first difference.
lockstepAntic :: AStepFn -> Int -> Config -> Either String Int
lockstepAntic stepFn cycles cfg = runST $ do
  m <- newMachine cfg
  st <- newSTRef anticInit
  err <- newSTRef Nothing
  n <- newSTRef (0 :: Int)
  fetched <- newSTRef []   -- this cycle start's DMA reads (address, data)
  pms <- newSTRef []       -- and its player/missile data
  let fail' msg = do
        k <- readSTRef n
        (y, x) <- anticPosition (mAntic m)
        readSTRef err >>= maybe (writeSTRef err (Just ("cycle " <> show k <> " (line " <> show y <> ", cycle " <> show x <> "): " <> msg))) (const (pure ()))
      run i = do
        a <- readSTRef st
        let (a', o) = stepFn a i
        writeSTRef st a'
        pure o
      -- a fetch the Clash ANTIC asks for: the spec must have made it
      fetch o = case aoReq o of
        Nothing -> pure ()
        Just addr -> do
          fs <- readSTRef fetched
          case fs of
            (a, v) : rest | fromIntegral addr == a -> do
              writeSTRef fetched rest
              () <$ run (AFetch (fromIntegral v))
            (a, _) : _ -> fail' ("fetch " <> show addr <> ", spec " <> show a)
            [] -> fail' ("fetch " <> show addr <> ", spec none")
      pmOf o = case aoPm o of
        PmNone -> []
        PmMissile od v -> [EvGtiaMissile 0 od (fromIntegral v)]
        PmPlayer od k v -> [EvGtiaPlayer 0 od (fromIntegral k) (fromIntegral v)]
      noX e = case e of
        EvGtiaMissile _ od v -> EvGtiaMissile 0 od v
        EvGtiaPlayer _ od k v -> EvGtiaPlayer 0 od k v
        _ -> e
      hook e = do
        bad <- readSTRef err
        when (bad == Nothing) $ case e of
          EvAnticFetch a v -> modifySTRef' fetched (<> [(a, v)])
          EvGtiaMissile {} -> modifySTRef' pms (<> [noX e])
          EvGtiaPlayer {} -> modifySTRef' pms (<> [noX e])
          EvAnticBegun bus dma -> do
            modifySTRef' n (+ 1)
            o1 <- run (ABegin (fromIntegral bus))
            (y, x) <- anticPosition (mAntic m)
            let use = case dma of
                  0 -> BusCpu
                  1 -> BusFetch
                  _ -> BusRefresh
            if (fromIntegral (aoY o1), fromIntegral (aoX o1)) /= (y, x)
              then fail' ("position " <> show (aoY o1, aoX o1))
              else if aoBus o1 /= use then fail' ("bus " <> show (aoBus o1) <> ", spec " <> show use)
              else do
                fetch o1
                o2 <- run APost
                fetch o2
                left <- readSTRef fetched
                when (left /= []) $ fail' ("spec fetched " <> show left <> ", Clash didn't")
                sp <- readSTRef pms
                let cp = pmOf o1 <> pmOf o2
                when (cp /= sp) $ fail' ("P/M data " <> show cp <> ", spec " <> show sp)
            writeSTRef fetched []
            writeSTRef pms []
          EvAnticRead a v -> do
            o <- run (ARead (fromIntegral (a .&. 15)))
            when (fromIntegral (aoRead o) /= v) $
              fail' ("read $D40" <> show (a .&. 15) <> " = " <> show (aoRead o) <> ", spec " <> show v)
          EvAnticWrite a v -> () <$ run (AWrite (fromIntegral (a .&. 15)) (fromIntegral v))
          EvAnticRdy h -> do
            o <- run ARdy
            when (aoRdy o == h) $ fail' ("WSYNC holds " <> show (not (aoRdy o)) <> ", spec " <> show h)
          EvAnticEnd bus c1 c2 -> do
            _ <- run (AEnd (fromIntegral bus))
            o <- run ADecode
            nmi <- anticNmi (mAntic m)
            let (d1, d2) = aoCodes o
            if (fromIntegral d1, fromIntegral d2) /= (c1, c2)
              then fail' ("AN codes " <> show (d1, d2) <> ", spec " <> show (c1, c2))
              else when (aoNmi o /= nmi) $ fail' ("NMI " <> show (aoNmi o) <> ", spec " <> show nmi)
          EvAnticReset -> () <$ run AReset
          _ -> pure ()
  -- the CPU's reset sequence ran in 'newMachine', before the hook: catch
  -- up (no DMA, no registers written yet)
  t0 <- machineCycles m
  let idle q = foldl (\a i -> fst (stepFn a i)) q [ABegin 0xFF, APost, AEnd 0xFF, ADecode]
  modifySTRef' st (\a -> iterate idle a !! t0)
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
