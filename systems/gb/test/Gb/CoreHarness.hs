{-# LANGUAGE MultiWayIf #-}
-- | Runs the Clash CPU core ("Gb.Cpu", as plain Haskell) on a reference
-- model 'Machine': the machine supplies memory and peripherals, with the
-- same tick-then-access M-cycles as the spec CPU sees, so the core and
-- the spec can be compared M-cycle by M-cycle.
module Gb.CoreHarness
  ( CoreRun
  , newCoreRun
  , coreStep
  , coreRegs
  , specRegs
  , coreMachine
  , runCoreRom
  , CoreResult (..)
  ) where

import Prelude

import Control.Monad.ST (ST, runST)
import Data.Bits (clearBit, (.&.))
import qualified Data.ByteString as BS
import Data.STRef
import Data.Word

import Clash.Prelude (BitVector)
import Gb.Cartridge (hdHeaderSum, parseHeader)
import Gb.Cpu
import Gb.Hooks
import Gb.Spec (Regs (..), regs)
import Gb.Spec.Machine

data CoreRun s = CoreRun
  { coreMachine :: Machine s
  , crCpu       :: STRef s (Cpu, BusReq)
  , crBreak     :: STRef s Bool
  }

newCoreRun :: BS.ByteString -> ST s (Either String (CoreRun s))
newCoreRun rom = do
  em <- newMachine rom
  case em of
    Left e -> pure (Left e)
    Right m -> do
      let hc = either (const False) ((/= 0) . hdHeaderSum) (parseHeader rom)
      r <- newSTRef (cpuInit hc)
      b <- newSTRef False
      pure (Right (CoreRun m r b))

bv8 :: Word8 -> BitVector 8
bv8 = fromIntegral

-- | Run M-cycles until the core ends a step; the bus accesses, oldest
-- first.
coreStep :: CoreRun s -> ST s [Access]
coreStep cr = go []
 where
  m = coreMachine cr
  go acc = do
    (cpu, req) <- readSTRef (crCpu cr)
    let a = fromIntegral (brAddr req) :: Word16
    (v, acc') <- case brOp req of
      OpRead -> do
        x <- busRead m a
        pure (x, ARead a x : acc)
      OpWrite -> do
        let x = fromIntegral (brData req)
        busWrite m a x
        pure (0, AWrite a x : acc)
      OpIdle -> busIdle m >> pure (0, AIdle : acc)
    ie <- getSt m iIE
    iflag <- getSt m iIF
    let (cpu', o) = cpuCycle cpu (CpuIn (bv8 v) (fromIntegral ie) (fromIntegral (iflag .&. 0x1F)))
    mapM_ (\n -> setSt m iIF (clearBit iflag (fromIntegral n))) (coAck o)
    writeSTRef (crCpu cr) (cpu', coReq o)
    if coBreak o then writeSTRef (crBreak cr) True else pure ()
    if coStepDone o then pure (reverse acc') else go acc'

-- | The core's registers in the spec's form.
coreRegs :: CoreRun s -> ST s Regs
coreRegs cr = do
  (cpu, _) <- readSTRef (crCpu cr)
  let r = cpuRegs cpu
      b = fromIntegral :: BitVector 8 -> Word8
  pure (Regs (b (crA r)) (b (crF r)) (b (crB r)) (b (crC r)) (b (crD r)) (b (crE r))
             (b (crH r)) (b (crL r)) (fromIntegral (crSp r)) (fromIntegral (crPc r)) (crIme r))

specRegs :: Machine s -> ST s Regs
specRegs = regs

data CoreResult = CoreResult
  { corSerial :: String
  , corVerdict :: Verdict
  , corCycles :: Int
  } deriving stock (Show, Eq)

-- | Run a ROM on the core alone until Blargg's serial verdict or
-- Mooneye's breakpoint ('True' = Mooneye), or the M-cycle limit.
runCoreRom :: Bool -> Int -> BS.ByteString -> Either String CoreResult
runCoreRom mooneye limit rom = runST $ do
  ecr <- newCoreRun rom
  case ecr of
    Left e -> pure (Left e)
    Right cr -> Right <$> loop cr
 where
  loop :: CoreRun s -> ST s CoreResult
  loop cr = do
    _ <- coreStep cr
    let m = coreMachine cr
    n <- getSt m iCYC
    brk <- readSTRef (crBreak cr)
    new <- getSt m iSERNEW
    setSt m iSERNEW 0
    txt <- if new /= 0 || n >= limit || brk then serialText <$> readSTRef (mSerial m) else pure ""
    let bv = blarggVerdict txt
    if | mooneye && brk -> do
           r <- coreRegs cr
           pure (CoreResult txt (mooneyeVerdict (MooneyeRegs (rgB r) (rgC r) (rgD r) (rgE r) (rgH r) (rgL r))) n)
       | not mooneye && bv /= Running -> pure (CoreResult txt bv n)
       | n >= limit -> pure (CoreResult txt Running n)
       | otherwise -> loop cr
