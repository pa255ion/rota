{-|
SingleStepTests on the Clash core ("Spc700.Core", step 7.8), as
"Spc700.SingleStep" runs them on the spec: the core starts at the opcode
fetch with the case's registers, every request is answered from the
case's RAM, and the run ends with the instruction (SLEEP and STOP run on,
halted, for the cycles the file records). 'judge' compares the outcome.
-}
module Spc700.CoreStep
  ( runCaseCore
  , toCore
  , fromCore
  ) where

import Prelude

import qualified Data.IntMap.Strict as IM

import Spc700.Core
import Spc700.SingleStep
import Spc700.Types

toCore :: Regs -> CpuRegs
toCore r = CpuRegs
  { crA = fromIntegral (rA r), crX = fromIntegral (rX r), crY = fromIntegral (rY r)
  , crSP = fromIntegral (rSP r), crPC = fromIntegral (rPC r), crPSW = fromIntegral (rPSW r) }

fromCore :: CpuRegs -> Regs
fromCore r = Regs
  { rA = fromIntegral (crA r), rX = fromIntegral (crX r), rY = fromIntegral (crY r)
  , rSP = fromIntegral (crSP r), rPC = fromIntegral (crPC r), rPSW = fromIntegral (crPSW r) }

runCaseCore :: Case -> [String]
runCaseCore tc =
  let ini = caseInitial tc
      ram0 = IM.fromList [ (fromIntegral a, v) | (a, v) <- csRam ini ]
      nWant = length (caseCycles tc)
      (cpu, ram, cycles) = go (cpuPowerOn (toCore (csRegs ini))) ram0 [] (0 :: Int)
      go c m acc n =
        let (v, cy, m') = case cpuReq c of
              RRead a -> let x = IM.findWithDefault 0 (fromIntegral a) m
                         in (x, Cycle (Just (fromIntegral a)) (Just x) Read, m)
              RWrite a d -> (0, Cycle (Just (fromIntegral a)) (Just (fromIntegral d)) Write, IM.insert (fromIntegral a) (fromIntegral d) m)
              RIdle -> (0, Cycle Nothing Nothing Wait, m)
            (c', out) = cpuCycle c (fromIntegral v)
            acc' = cy : acc
            n' = n + 1
            halted = cPhase c' `elem` [PHalt0, PHalt1]
        in if n' > 64 then (c', m', acc')
           else if coStep out && not (halted && n' < nWant) then (c', m', acc')
           else if halted && n' >= nWant then (c', m', acc')
           else go c' m' acc' n'
  in judge tc (fromCore (cpuRegs cpu)) ram (reverse cycles)
