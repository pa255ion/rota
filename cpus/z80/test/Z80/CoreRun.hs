{-# LANGUAGE RankNTypes #-}
{-|
"Z80.Core" driven through a "Z80.Spec" 'Bus' (tests only): each request
becomes the bus call the spec would make, so both models can run on the
same memory and ports, and the SingleStepTests harness can run the core.
-}
module Z80.CoreRun
  ( toCore
  , fromCore
  , coreInstr
  , coreRunner
  ) where

import Prelude

import Clash.Prelude (Unsigned)
import Control.Monad.ST (ST)
import Data.Word

import Z80.Core
import Z80.Spec (Bus (..), Regs (..))

u8 :: Word8 -> Unsigned 8
u8 = fromIntegral

u16 :: Word16 -> Unsigned 16
u16 = fromIntegral

w8 :: Unsigned 8 -> Word8
w8 = fromIntegral

w16' :: Unsigned 16 -> Word16
w16' = fromIntegral

-- | A core at the start of an instruction with these registers.
toCore :: Regs -> Cpu
toCore r = cpuPowerOn
  { cA = u8 (rA r), cF = u8 (rF r), cB = u8 (rB r), cC = u8 (rC r)
  , cD = u8 (rD r), cE = u8 (rE r), cH = u8 (rH r), cL = u8 (rL r)
  , cA' = hi (rAF' r), cF' = lo (rAF' r), cB' = hi (rBC' r), cC' = lo (rBC' r)
  , cD' = hi (rDE' r), cE' = lo (rDE' r), cH' = hi (rHL' r), cL' = lo (rHL' r)
  , cIX = u16 (rIX r), cIY = u16 (rIY r), cSP = u16 (rSP r), cPC = u16 (rPC r)
  , cWZ = u16 (rWZ r), cI = u8 (rI r), cR = u8 (rR r)
  , cIFF1 = rIFF1 r, cIFF2 = rIFF2 r, cIM = fromIntegral (rIM r)
  , cQ = u8 (rQ r), cEI = rEI r, cP = rP r, cHalted = rHalted r
  , cReq = Req KFetch (u16 (rPC r)) 0 4
  , cPhase = if rHalted r then PHalt else PFetch TMain
  }
 where
  hi v = fromIntegral (v `div` 256)
  lo v = fromIntegral (v `mod` 256)

fromCore :: Cpu -> Regs
fromCore c = Regs
  { rA = w8 (cA c), rF = w8 (cF c), rB = w8 (cB c), rC = w8 (cC c)
  , rD = w8 (cD c), rE = w8 (cE c), rH = w8 (cH c), rL = w8 (cL c)
  , rAF' = pair (cA' c) (cF' c), rBC' = pair (cB' c) (cC' c)
  , rDE' = pair (cD' c) (cE' c), rHL' = pair (cH' c) (cL' c)
  , rIX = w16' (cIX c), rIY = w16' (cIY c), rSP = w16' (cSP c), rPC = w16' (cPC c)
  , rI = w8 (cI c), rR = w8 (cR c), rIFF1 = cIFF1 c, rIFF2 = cIFF2 c
  , rIM = fromIntegral (cIM c), rWZ = w16' (cWZ c), rQ = w8 (cQ c)
  , rEI = cEI c, rP = cP c, rHalted = cHalted c
  }
 where
  pair h l = fromIntegral h * 256 + fromIntegral l

-- | Run the core's cycles until the instruction (or interrupt sequence,
-- or halted fetch) is over, each through the bus. The lines are what the
-- core sees at the end: the spec's next 'busInt' / 'busNmi'. Returns the
-- core and the T-states taken.
coreInstr :: Bus s -> Bool -> Bool -> Cpu -> ST s (Cpu, Int)
coreInstr bus int nmi = loop 0 True
 where
  loop t first c
    | not first && cNew c = pure (c, t)
    | otherwise = do
        let rq = cReq c
            a = w16' (rqAddr rq)
        v <- case rqKind rq of
          KFetch -> busFetch bus a
          KRead -> busRead bus a
          KWrite -> 0 <$ busWrite bus a (w8 (rqData rq))
          KIn -> busIn bus a
          KOut -> 0 <$ busOut bus a (w8 (rqData rq))
          KIdle -> 0 <$ busIdle bus a (fromIntegral (rqLen rq))
          KIntAck -> busIntAck bus
        let c' = cpuStep (CpuIn (u8 v) int nmi) c
        loop (t + fromIntegral (rqLen rq)) False c'

-- | The core as a SingleStepTests runner (no interrupts).
coreRunner :: Bus s -> Regs -> ST s (Regs, Int)
coreRunner bus rs = do
  (c, t) <- coreInstr bus False False (toCore rs)
  pure (fromCore c, t)
