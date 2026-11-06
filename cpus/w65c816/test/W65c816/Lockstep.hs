{-|
The Clash core ("W65c816.Core") in lockstep with the spec
("W65c816.Spec") on random programs (step 7.7): both start with /RES in a
16 MiB memory of pseudo-random bytes (a hash of the seed and the address;
STP's opcode mostly turned into NOP, so a run lasts), run the same number
of cycles with the same NMI and IRQ lines (random pulses and levels by
cycle number), and must make the same bus cycles: the kind of each (read
and its VPA/VDA/VPB, write and its VDA, internal, stopped), its address,
the data written and MLB; and end with the same registers. Both
configurations: the SNES's chip and SingleStepTests' model.
-}
module W65c816.Lockstep
  ( lockstepTests
  , SpecCycle (..)
  , specTrace
  ) where

import Prelude

import Control.Monad.ST (runST)
import Data.Bits
import qualified Data.IntMap.Strict as IM
import Data.STRef
import Data.Word
import Numeric (showHex)
import Test.Tasty
import Test.Tasty.HUnit

import qualified W65c816.Core as K
import W65c816.CoreStep (fromCore)
import qualified W65c816.Spec as S
import W65c816.Types

-- | A bus cycle as both models make it.
data SpecCycle
  = CyRead S.Kind Bool Int Word8   -- ^ kind, lock, address, data
  | CyWrite Bool Bool Int Word8    -- ^ VDA, lock, address, data
  | CyIdle Bool Int
  | CyStopped
  deriving stock (Show, Eq)

-- | The memory: a hash of the seed and the address; 0DBh (STP) is 0EAh
-- (NOP) but one in 8.
memByte :: Int -> Int -> Word8
memByte seed a =
  let h0 = (a * 0x9E3779B1 + seed * 0x85EBCA77) .&. 0xFFFFFFFF
      h1 = ((h0 `xor` (h0 `shiftR` 15)) * 0x2C1B3C6D) .&. 0xFFFFFFFF
      h2 = (h1 `xor` (h1 `shiftR` 12)) .&. 0xFF
      b = fromIntegral h2 :: Word8
  in if b == 0xDB && (h1 `shiftR` 20) .&. 7 /= 0 then 0xEA else b

-- | NMI: pulses of 1 to 4 cycles, about one in 3000 cycles; IRQ: on for
-- stretches, about a tenth of the time.
nmiAt, irqAt :: Int -> Int -> Bool
nmiAt seed k = let h = memByte (seed + 77) (k `shiftR` 2) in h == 3 && (memByte (seed + 5) (k `shiftR` 2) .&. 0xB) == 0
irqAt seed k = memByte (seed + 99) (k `shiftR` 6) < 26

-- | The spec's cycles from /RES for @n@ cycles, and its registers then.
specTrace :: S.Config -> Int -> Int -> ([SpecCycle], Regs)
specTrace cfg seed n = runST $ do
  mem <- newSTRef IM.empty
  trace <- newSTRef []
  count <- newSTRef (0 :: Int)
  let note cy = modifySTRef' trace (cy :) >> modifySTRef' count (+ 1)
      readM a = do
        m <- readSTRef mem
        pure (IM.findWithDefault (memByte seed a) a m)
      bus = S.Bus
        { S.busRead = \k l a -> do
            v <- readM (fromIntegral a)
            note (CyRead k l (fromIntegral a) v)
            pure v
        , S.busWrite = \vda l a v -> do
            modifySTRef' mem (IM.insert (fromIntegral a) v)
            note (CyWrite vda l (fromIntegral a) v)
        , S.busIdle = \l a -> note (CyIdle l (fromIntegral a))
        , S.busStopped = note CyStopped
        , S.busNmi = nmiAt seed <$> readSTRef count
        , S.busIrq = irqAt seed <$> readSTRef count
        }
  cpu <- S.newCpu cfg bus resetRegs
  S.reset cpu
  let go = do
        k <- readSTRef count
        if k >= n then pure () else S.step cpu >> go
  go
  r <- S.getRegs cpu
  cys <- reverse <$> readSTRef trace
  pure (cys, r)

-- | The core's run against the spec's cycles: the first difference.
coreAgainst :: K.CoreConfig -> Int -> [SpecCycle] -> Regs -> Maybe String
coreAgainst cfg seed want wantRegs = go (K.cpuReset cfg (K.cpuPowerOn (toK resetRegs))) IM.empty (0 :: Int) want
 where
  go c m k cys = case cys of
    [] ->
      -- at a step's end both; the spec stops at the step that passes n
      let r = fromCore (K.cpuRegs c)
      in if r == wantRegs then Nothing else Just ("registers " <> show r <> ", the spec " <> show wantRegs)
    w : rest ->
      let req = K.cReq c
          a = fromIntegral (K.rqAddr req) :: Int
          l = K.rqLock req
          readM = IM.findWithDefault (memByte seed a) a m
          (got, v, m') = case K.rqOp req of
            K.BRead kk -> (CyRead (kind kk) l a readM, readM, m)
            K.BWrite vda -> (CyWrite vda l a (fromIntegral (K.rqData req)), 0, IM.insert a (fromIntegral (K.rqData req)) m)
            K.BIdle -> (CyIdle l a, 0, m)
            K.BStopped -> (CyStopped, 0, m)
          k' = k + 1
          (c', _) = K.cpuCycle cfg c (K.CpuIn (fromIntegral v) (nmiAt seed k') (irqAt seed k'))
      in if got /= w
           then Just ("cycle " <> show k <> ": " <> show got <> ", the spec " <> show w
                      <> "; PC " <> showHex (K.cPC c) "" <> " op " <> show (K.iOp (K.cInfo c)))
           else go c' m' k' rest
  kind kk = case kk of
    K.KOpcode -> S.KOpcode
    K.KOperand -> S.KOperand
    K.KData -> S.KData
    K.KVector -> S.KVector
  toK r = K.CpuRegs (fromIntegral (rA r)) (fromIntegral (rX r)) (fromIntegral (rY r)) (fromIntegral (rS r))
                    (fromIntegral (rD r)) (fromIntegral (rPC r)) (fromIntegral (rDBR r)) (fromIntegral (rPBR r))
                    (fromIntegral (rP r)) (rE r)

lockstepTests :: Bool -> TestTree
lockstepTests full = testGroup "the core in lockstep with the spec (random programs)"
  [ testCase (name <> ", seed " <> show seed <> ", " <> show n <> " cycles") $ do
      let (cys, r) = specTrace scfg seed n
      maybe (pure ()) assertFailure (coreAgainst kcfg seed cys r)
  | (name, scfg, kcfg) <- [ ("the SNES's chip", S.w65c816, K.coreW65c816)
                          , ("SingleStepTests' model", S.singleStep65816, K.coreSingleStep) ]
  , seed <- [1 .. if full then 64 else 8]
  , let n = if full then 400000 else 50000 ]
