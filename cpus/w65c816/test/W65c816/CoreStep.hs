{-|
SingleStepTests on the Clash core ("W65c816.Core", step 7.7), as
"W65c816.SingleStep" runs them on the spec: the core starts at the
opcode fetch with the case's registers, every request is answered from
the case's RAM (a read of an address the case doesn't list is an error),
and the run ends with the instruction (MVN and MVP at the end of the move;
WAI and STP after one stopped cycle). 'judge' compares the outcome.
-}
module W65c816.CoreStep
  ( runCaseCore
  , toCore
  , fromCore
  ) where

import Prelude

import qualified Data.IntMap.Strict as IM
import Numeric (showHex)

import W65c816.Core
import W65c816.SingleStep
import W65c816.Types

toCore :: Regs -> CpuRegs
toCore r = CpuRegs
  { crA = fromIntegral (rA r), crX = fromIntegral (rX r), crY = fromIntegral (rY r)
  , crS = fromIntegral (rS r), crD = fromIntegral (rD r), crPC = fromIntegral (rPC r)
  , crDBR = fromIntegral (rDBR r), crPBR = fromIntegral (rPBR r), crP = fromIntegral (rP r)
  , crE = rE r }

fromCore :: CpuRegs -> Regs
fromCore r = Regs
  { rA = fromIntegral (crA r), rX = fromIntegral (crX r), rY = fromIntegral (crY r)
  , rS = fromIntegral (crS r), rD = fromIntegral (crD r)
  , rDBR = fromIntegral (crDBR r), rPBR = fromIntegral (crPBR r)
  , rPC = fromIntegral (crPC r), rP = fromIntegral (crP r), rE = crE r }

runCaseCore :: Case -> [String]
runCaseCore tc =
  let ini = caseInitial tc
      ram0 = IM.fromList [ (fromIntegral a, v) | (a, v) <- csRam ini ]
      cpu0 = cpuPowerOn (toCore (csRegs ini))
      pc0 = rPC (csRegs ini)
      (cpu, ram, cycles, errs) = go cpu0 ram0 [] [] (0 :: Int) False
      go c m acc es n last =
        let req = cReq c
            a = fromIntegral (rqAddr req) :: Int
            (v, es', cy, m') = case rqOp req of
              BRead k -> case IM.lookup a m of
                Just x -> (x, es, rdCy k x, m)
                Nothing -> (0, ("read of unlisted address " <> showHex a "") : es, rdCy k 0, m)
              BWrite vda -> (0, es, blank { cyAddr = Just (fromIntegral a), cyData = Just (fromIntegral (rqData req))
                                          , cyVda = vda, cyWrite = True, cyMlb = rqLock req }
                            , IM.insert a (fromIntegral (rqData req)) m)
              BIdle -> (0, es, blank { cyAddr = Just (fromIntegral a), cyMlb = rqLock req }, m)
              BStopped -> (0, es, blank, m)
            rdCy k x = blank { cyAddr = Just (fromIntegral a), cyData = Just x
                             , cyVda = k /= KOperand, cyVpa = k `elem` [KOpcode, KOperand]
                             , cyVpb = k == KVector, cyMlb = rqLock req }
            (c', out) = cpuCycle coreSingleStep c (CpuIn (fromIntegral v) False False)
            acc' = cy : acc
        in if last then (c', m', acc', es')
           else if n > 1000000 then (c', m', acc', "no end" : es')
           else case coStep out of
             Just SkInstr
               | iOp (cInfo c') `elem` [OMVN, OMVP], cPC c' == fromIntegral pc0, n < 1000000 -> go c' m' acc' es' (n + 1) False
               | cPhase c' `elem` [PhWait, PhStop] -> go c' m' acc' es' (n + 1) True
               | otherwise -> (c', m', acc', es')
             _ -> go c' m' acc' es' (n + 1) False
  in reverse errs <> judge tc (fromCore (cpuRegs cpu)) ram (reverse cycles)
 where
  blank = Cycle Nothing Nothing False False False False False False False False
