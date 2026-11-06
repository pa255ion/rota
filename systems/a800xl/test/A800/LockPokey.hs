{-|
Lockstep of the Clash POKEY ("A800.Pokey", as plain Haskell) with the
reference model's (M4 step 4.5). The spec machine runs as usual; its hook
reports every POKEY call (cycle start, register access, keys), the Clash
POKEY gets the same, and after each: a read must return the spec's value,
and the IRQ line, the mixed level and the bytes the serial port sent must
agree.
-}
module A800.LockPokey
  ( StepFn
  , lockstepPokey
  ) where

import Prelude

import Control.Monad.ST (runST)
import Data.Bits ((.&.))
import Data.STRef
import qualified Clash.Prelude as C

import A800.Pokey
import A800.Spec.Machine
import A800.Spec.Pokey (pokeyIrq, pokeyLevel, pokeySerialOut)
import Mos6502.Spec (step)

type StepFn = Pokey -> PokeyIn -> (Pokey, PokeyOut)

-- | Run the machine for @cycles@ with the Clash POKEY alongside, typing
-- @keys@ (machine cycle, keys, modifiers) as they come; 'Left' with the
-- first difference, else the number of POKEY cycles compared.
lockstepPokey :: StepFn -> Int -> [(Int, C.BitVector 64, C.BitVector 3)] -> Config -> Either String Int
lockstepPokey stepFn cycles keys cfg = runST $ do
  m <- newMachine cfg
  st <- newSTRef pokeyInit
  err <- newSTRef Nothing
  n <- newSTRef (0 :: Int)
  sent <- newSTRef (0 :: Int)
  let fail' msg = readSTRef err >>= maybe (writeSTRef err (Just msg)) (const (pure ()))
      run i = do
        p <- readSTRef st
        let (p', o) = stepFn p i
        writeSTRef st p'
        pure o
      hook e = do
        bad <- readSTRef err
        case (bad, e) of
          (Just _, _) -> pure ()
          (_, EvPokeyTick) -> do
            o <- run PTick
            modifySTRef' n (+ 1)
            k <- readSTRef n
            irq <- pokeyIrq (mPokey m)
            lvl <- pokeyLevel (mPokey m)
            out <- length <$> pokeySerialOut (mPokey m)
            case poSent o of
              Just _ -> modifySTRef' sent (+ 1)
              Nothing -> pure ()
            s <- readSTRef sent
            if poIrq o /= irq then fail' ("cycle " <> show k <> ": IRQ " <> show (poIrq o) <> ", spec " <> show irq)
              else if fromIntegral (poLevel o) /= lvl then fail' ("cycle " <> show k <> ": level " <> show (poLevel o) <> ", spec " <> show lvl)
              else if s /= out then fail' ("cycle " <> show k <> ": bytes sent " <> show s <> ", spec " <> show out)
              else pure ()
          (_, EvPokeyRead a v) -> do
            o <- run (PRead (fromIntegral (a .&. 15)))
            k <- readSTRef n
            if fromIntegral (poRead o) /= v
              then fail' ("cycle " <> show k <> ": read $D20" <> show (a .&. 15) <> " = " <> show (poRead o) <> ", spec " <> show v)
              else pure ()
          (_, EvPokeyWrite a v) -> () <$ run (PWrite (fromIntegral (a .&. 15)) (fromIntegral v))
          (_, EvPokeyKeys ks ms) -> () <$ run (PKeys (fromIntegral ks) (fromIntegral ms))
          _ -> pure ()
  setHook m hook
  let go pending = do
        t <- machineCycles m
        pending' <- case pending of
          (at, ks, ms) : rest | t >= at -> setKeys m (fromIntegral ks) (fromIntegral ms) >> pure rest
          _ -> pure pending
        _ <- step (mCpu m)
        bad <- readSTRef err
        if t >= cycles || bad /= Nothing then pure () else go pending'
  go keys
  bad <- readSTRef err
  k <- readSTRef n
  pure (maybe (Right k) Left bad)
