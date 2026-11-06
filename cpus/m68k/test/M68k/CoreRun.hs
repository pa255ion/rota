{-|
"M68k.Core" on the model's bus: each request made as the call the spec
would make, until the instruction is over ('cpuFinish', at
the boundary). A step whose command was bad stops the run as the model's
Left.
-}
module M68k.CoreRun
  ( toCore
  , fromCore
  , coreRunner
  ) where

import Prelude

import Data.Bits
import Data.Maybe (fromMaybe, isNothing)

import Data.Array.Unboxed (elems, listArray)
import qualified Clash.Prelude as C
import qualified Clash.Sized.Vector as V

import M68k.Bus (CpuIn (..), Kind (..), Req (..))
import M68k.Core
import qualified M68k.Datapath as C2
import qualified M68k.Spec as S

toCore :: S.Cpu -> Cpu
toCore s = cpuPowerOn { cC = toDp s }

fromCore :: Cpu -> S.Cpu
fromCore = fromDp . cC

toDp :: S.Cpu -> C2.Cpu
toDp s =
  let sup = S.supervisor s
      a7 = if sup then S.cSsp s else S.cUsp s
      other = if sup then S.cUsp s else S.cSsp s
      k = C2.cK C2.cpuPowerOn
  in C2.cpuPowerOn
       { C2.cK = k { C2.kStopped = S.cStopped s, C2.kIplSeen = fromIntegral (S.cIplSeen s) }
       , C2.cRf = V.unsafeFromList (map fromIntegral (elems (S.cD s) <> elems (S.cA s) <> [a7]))
       , C2.cOsp = fromIntegral other
       , C2.cSr = fromIntegral (S.cSr s)
       , C2.cPc = fromIntegral (S.cPc s)
       , C2.cIr = fromIntegral (S.cIr s)
       , C2.cIrc = fromIntegral (S.cIrc s)
       }

fromDp :: C2.Cpu -> S.Cpu
fromDp c = S.Cpu
  { S.cD = listArray (0, 7) (map fromIntegral (C.toList (C2.regD c)))
  , S.cA = listArray (0, 6) (map fromIntegral (take 7 (C.toList (C2.regA c))))
  , S.cUsp = fromIntegral (C2.usp c)
  , S.cSsp = fromIntegral (C2.ssp c)
  , S.cSr = fromIntegral (C2.cSr c)
  , S.cPc = fromIntegral (C2.cPc c)
  , S.cIr = fromIntegral (C2.cIr c)
  , S.cIrc = fromIntegral (C2.cIrc c)
  , S.cStopped = C2.kStopped (C2.cK c)
  , S.cIplSeen = fromIntegral (C2.kIplSeen (C2.cK c))
  }


coreRunner :: Monad m => S.BusOps m -> S.Cpu -> m (Either S.Fault S.Cpu)
coreRunner b s = do
  ipl <- S.busIpl b
  let lvl = fromIntegral ipl
  loop lvl (0 :: Int) (begin lvl (toCore s))
 where
  loop lvl n c
    | badStep c = pure (Left (S.Unimplemented 0xBAD0))
    | atBound c =
        pure (if halted c then Left (S.Unimplemented (fromIntegral (C2.kOp (C2.cK (cC c))))) else Right (fromCore c))
    | n > 2000 = pure (Left (S.Unimplemented 0xBAD1))
    | otherwise = do
        let rq = C2.cReq (cC c)
            fc = fromIntegral (rqFc rq)
            sz = if rqByte rq then S.Byte else S.Word
            a = fromIntegral (rqAddr rq)
            ci d be au = CpuIn { ciData = d, ciBerr = be, ciIpl = lvl, ciAuto = au }
        inp <- case rqKind rq of
          KRead -> do
            v <- S.busRead b fc sz a
            be <- S.busBerr b
            pure (ci (fromIntegral v) be False)
          KWrite -> do
            S.busWrite b fc sz a (fromIntegral (rqData rq))
            be <- S.busBerr b
            pure (ci 0 be False)
          KTas -> do
            v <- S.busTas b fc a (.|. 0x80)
            pure (ci (fromIntegral v) False False)
          KIdle -> do
            S.busIdle b (fromIntegral (rqLen rq))
            pure (ci 0 False False)
          KIack -> do
            v <- S.busIack b (fromIntegral (rqLen rq))
            pure (ci (fromIntegral (fromMaybe 0 v)) False (isNothing v))
        loop lvl (n + 1) (cpuFinish inp c)
