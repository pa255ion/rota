{-|
The Clash core ("Spc700.Core") in lockstep with the spec ("Spc700.Spec")
on random programs (step 7.8): both start at FFC0h in a 64 KiB memory of
pseudo-random bytes (a hash of the seed and the address; SLEEP's and
STOP's opcodes mostly turned into NOP, so a run lasts), run the same
number of cycles and must make the same bus cycles (each one's kind,
address and data) and end with the same registers.
-}
module Spc700.Lockstep
  ( lockstepTests
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

import qualified Spc700.Core as K
import Spc700.CoreStep (fromCore, toCore)
import qualified Spc700.Spec as S
import Spc700.Types

data Cy = CyRead Int Word8 | CyWrite Int Word8 | CyIdle
  deriving stock (Show, Eq)

-- | The memory: a hash of the seed and the address; EFh and FFh (SLEEP,
-- STOP) are 00h (NOP) but one in 16.
memByte :: Int -> Int -> Word8
memByte seed a =
  let h0 = (a * 0x9E3779B1 + seed * 0x85EBCA77) .&. 0xFFFFFFFF
      h1 = ((h0 `xor` (h0 `shiftR` 15)) * 0x2C1B3C6D) .&. 0xFFFFFFFF
      b = fromIntegral ((h1 `xor` (h1 `shiftR` 12)) .&. 0xFF) :: Word8
  in if (b == 0xEF || b == 0xFF) && (h1 `shiftR` 20) .&. 15 /= 0 then 0 else b

start :: Regs
start = Regs 0 0 0 0xEF 0xFFC0 0

-- | The spec's cycles for at least @n@ cycles (to the end of a step), and
-- its registers then.
specTrace :: Int -> Int -> ([Cy], Regs)
specTrace seed n = runST $ do
  mem <- newSTRef IM.empty
  trace <- newSTRef []
  let readM a = IM.findWithDefault (memByte seed a) a <$> readSTRef mem
      bus = S.Bus
        { S.busRead = \a -> do
            v <- readM (fromIntegral a)
            modifySTRef' trace (CyRead (fromIntegral a) v :)
            pure v
        , S.busWrite = \a v -> do
            modifySTRef' mem (IM.insert (fromIntegral a) v)
            modifySTRef' trace (CyWrite (fromIntegral a) v :)
        , S.busIdle = modifySTRef' trace (CyIdle :)
        }
  cpu <- S.newCpu bus start
  let go = do
        k <- S.cpuCycles cpu
        if k >= n then pure () else S.step cpu >> go
  go
  r <- S.getRegs cpu
  cys <- reverse <$> readSTRef trace
  pure (cys, r)

coreAgainst :: Int -> [Cy] -> Regs -> Maybe String
coreAgainst seed want wantRegs = go (K.cpuPowerOn (toCore start)) IM.empty (0 :: Int) want
 where
  go c m k cys = case cys of
    [] ->
      let r = fromCore (K.cpuRegs c)
      in if r == wantRegs then Nothing else Just ("registers " <> show r <> ", the spec " <> show wantRegs)
    w : rest ->
      let (got, v, m') = case K.cpuReq c of
            K.RRead a -> let x = IM.findWithDefault (memByte seed (fromIntegral a)) (fromIntegral a) m
                         in (CyRead (fromIntegral a) x, x, m)
            K.RWrite a d -> (CyWrite (fromIntegral a) (fromIntegral d), 0, IM.insert (fromIntegral a) (fromIntegral d) m)
            K.RIdle -> (CyIdle, 0, m)
          (c', _) = K.cpuCycle c (fromIntegral v)
      in if got /= w
           then Just ("cycle " <> show k <> ": " <> show got <> ", the spec " <> show w
                      <> "; PC " <> showHex (K.cPC c) "" <> " op " <> showHex (K.cOp c) "" <> " step " <> show (K.cStep c))
           else go c' m' (k + 1) rest

lockstepTests :: Bool -> TestTree
lockstepTests full = testGroup "the core in lockstep with the spec (random programs)"
  [ testCase ("seed " <> show seed <> ", " <> show n <> " cycles") $ do
      let (cys, r) = specTrace seed n
      maybe (pure ()) assertFailure (coreAgainst seed cys r)
  | seed <- [1 .. if full then 64 else 8]
  , let n = if full then 400000 else 50000 ]
