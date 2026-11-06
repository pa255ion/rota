{-|
Runs the reference model ("Mos6502.Spec") and the Clash core
("Mos6502.Core", as plain Haskell) on the same 64 KiB of memory and the
same NMI, IRQ and RDY schedules, step by step, for the lockstep tests.

A schedule gives a line's level at the end of cycle @n@ (0 = the first
cycle run); RDY is asked before each read cycle, with the number of the
cycle about to run, as the spec's 'busRdy' is.
-}
module Mos6502.Harness
  ( Env (..)
  , quietEnv
  , StepRec (..)
  , CoreFn
  , specSteps
  , coreSteps
  , firstDifference
  ) where

import Prelude

import Control.Monad (replicateM)
import Control.Monad.ST (ST, runST)
import Data.STRef
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MV
import Data.Word

import qualified Clash.Prelude as C
import Mos6502.Core
import Mos6502.Spec hiding (Cpu)

data Env = Env
  { envMem :: U.Vector Word8 -- ^ 65536 bytes
  , envNmi :: Int -> Bool
  , envIrq :: Int -> Bool
  , envRdy :: Int -> Bool
  }

quietEnv :: U.Vector Word8 -> Env
quietEnv m = Env m (const False) (const False) (const True)

-- | One step: what it was, its bus accesses (address, data, write), and
-- the registers after it.
data StepRec = StepRec
  { srKind :: String
  , srBus  :: [(Word16, Word8, Bool)]
  , srRegs :: Regs
  } deriving stock (Show, Eq)

type CoreFn = Cpu -> CpuIn -> (Cpu, CpuOut)

stepName :: Step -> String
stepName s = case s of
  Instr _ -> "instruction"
  Nmi -> "NMI"
  Irq -> "IRQ"
  Jammed -> "jam"

kindName :: StepKind -> String
kindName k = case k of
  SkInstr -> "instruction"
  SkNmi -> "NMI"
  SkIrq -> "IRQ"
  SkJam -> "jam"
  SkReset -> "reset"

-- | @n@ steps of the spec from these registers (fetch at PC first).
specSteps :: Config -> Env -> Regs -> Int -> [StepRec]
specSteps cfg env r0 n = runST $ do
  mem <- U.thaw (envMem env)
  cyc <- newSTRef (0 :: Int)
  bus <- newSTRef []
  let tick a v w = do
        modifySTRef' bus ((a, v, w) :)
        modifySTRef' cyc (+ 1)
      lineAt f = (\c -> f (c - 1)) <$> readSTRef cyc
      b = Bus
        { busRead = \a -> MV.read mem (fromIntegral a) >>= \v -> tick a v False >> pure v
        , busWrite = \a v -> MV.write mem (fromIntegral a) v >> tick a v True
        , busRdy = envRdy env <$> readSTRef cyc
        , busNmi = lineAt (envNmi env)
        , busIrq = lineAt (envIrq env)
        }
  cpu <- newCpu cfg b r0
  replicateM n $ do
    writeSTRef bus []
    s <- step cpu
    accs <- reverse <$> readSTRef bus
    r <- getRegs cpu
    pure (StepRec (stepName s) accs r)

-- | @n@ steps of a core function from these registers.
coreSteps :: CoreFn -> Env -> Regs -> Int -> [StepRec]
coreSteps fn env (Regs a x y s p pc) n = runST $ do
  mem <- U.thaw (envMem env)
  let go :: MV.MVector st Word8 -> Cpu -> Int -> Int -> [(Word16, Word8, Bool)] -> [StepRec] -> ST st [StepRec]
      go m cpu c k acc out
        | k >= n = pure (reverse out)
        | otherwise = do
            let req = cpuRequest cpu
                addr = fromIntegral (rqAddr req) :: Word16
            (v, stall, rec) <- if rqWrite req
              then do
                let d = fromIntegral (rqData req)
                MV.write m (fromIntegral addr) d
                pure (0, False, (addr, d, True))
              else do
                d <- MV.read m (fromIntegral addr)
                pure (d, not (envRdy env c), (addr, d, False))
            let inp = CpuIn (fromIntegral v) stall (envNmi env c) (envIrq env c)
                (cpu', o) = fn cpu inp
                acc' = rec : acc
            case coStep o of
              Just kind ->
                let r = cpuRegs cpu'
                    regs = Regs (w8 (crA r)) (w8 (crX r)) (w8 (crY r)) (w8 (crS r)) (w8 (crP r)) (fromIntegral (crPc r))
                in go m cpu' (c + 1) (k + 1) [] (StepRec (kindName kind) (reverse acc') regs : out)
              Nothing -> go m cpu' (c + 1) k acc' out
  go mem (cpuPowerOn (CpuRegs (u8 a) (u8 x) (u8 y) (u8 s) (u8 p) (fromIntegral pc))) 0 0 [] []
 where
  w8 :: C.Unsigned 8 -> Word8
  w8 = fromIntegral
  u8 :: Word8 -> C.Unsigned 8
  u8 = fromIntegral

-- | The first step where the two runs differ, with its number.
firstDifference :: [StepRec] -> [StepRec] -> Maybe (Int, StepRec, StepRec)
firstDifference xs ys = case [ (i, x, y) | (i, x, y) <- zip3 [0 ..] xs ys, x /= y ] of
  d : _ -> Just d
  [] -> Nothing
