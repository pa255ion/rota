{-|
What SingleStepTests can't show (it never raises an interrupt): /INT in
IM 0, 1 and 2, the NMI edge and RETN, EI's one-instruction delay, DI,
and leaving HALT (Zilog UM0080, "Interrupt Response"; Sean Young, "The
Undocumented Z80 Documented", sections 5 and 6). Each test runs a small
program in 64 KiB of RAM with the /INT and /NMI lines driven by step
number.
-}
module Z80.Interrupts
  ( interruptTests
  ) where

import Prelude

import Control.Monad (forM)
import Control.Monad.ST (runST)
import Data.STRef
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MV
import Data.Word
import Test.Tasty
import Test.Tasty.HUnit

import Z80.Spec

-- | Lines as a function of the step number (0 = the first step), and the
-- byte the interrupting device puts on the bus.
data Lines = Lines
  { lnInt, lnNmi :: Int -> Bool
  , lnVector     :: Word8
  }

quiet :: Lines
quiet = Lines (const False) (const False) 0xFF

data Run = Run
  { runSteps :: [(Step, Int, Regs)] -- ^ per step: what it did, its T-states, the registers after it
  , runMem   :: Word16 -> Word8     -- ^ RAM at the end
  }

-- | Program at 0000h (bytes @prog@, NOPs after them), extra bytes poked
-- at the given addresses, SP = F000h; runs @n@ steps from @regs@.
run :: Lines -> Regs -> [Word8] -> [(Word16, Word8)] -> Int -> Run
run ls regs prog pokes n = runST $ do
  mem <- MV.replicate 0x10000 0
  mapM_ (\(i, b) -> MV.write mem i b) (zip [0 ..] prog)
  mapM_ (\(a, b) -> MV.write mem (fromIntegral a) b) pokes
  stepNo <- newSTRef (0 :: Int)
  let rdM a = MV.read mem (fromIntegral a)
      bus = Bus
        { busFetch = rdM
        , busRead = rdM
        , busWrite = \a v -> MV.write mem (fromIntegral a) v
        , busIn = \_ -> pure 0xFF
        , busOut = \_ _ -> pure ()
        , busIdle = \_ _ -> pure ()
        , busIntAck = pure (lnVector ls)
        , busInt = lnInt ls <$> readSTRef stepNo
        , busNmi = lnNmi ls <$> readSTRef stepNo
        }
  cpu <- newCpu bus regs { rSP = 0xF000 }
  steps <- forM [0 .. n - 1] $ \i -> do
    writeSTRef stepNo i
    t0 <- cpuTStates cpu
    s <- step cpu
    t1 <- cpuTStates cpu
    r <- getRegs cpu
    pure (s, t1 - t0, r)
  frozen <- U.freeze mem
  pure (Run steps (\a -> frozen U.! fromIntegral a))

enabled :: Regs
enabled = zeroRegs { rIFF1 = True, rIFF2 = True, rIM = 1 }

nth :: Run -> Int -> (Step, Int, Regs)
nth r i = runSteps r !! i

pushed :: Run -> Regs -> Word16
pushed r rs = fromIntegral (runMem r (rSP rs)) + 256 * fromIntegral (runMem r (rSP rs + 1))

interruptTests :: TestTree
interruptTests = testGroup "interrupts"
  [ testCase "IM 1: RST 38h after the current instruction, 13 T, IFFs cleared" $ do
      let r = run quiet { lnInt = (>= 1) } enabled [0x00, 0x00, 0x00] [] 3
          (s0, _, _) = nth r 0
          (s1, t1, rs1) = nth r 1
      s0 @?= Instr 0x00
      s1 @?= Int
      t1 @?= 13
      rPC rs1 @?= 0x0038
      rWZ rs1 @?= 0x0038
      (rIFF1 rs1, rIFF2 rs1) @?= (False, False)
      pushed r rs1 @?= 0x0001
  , testCase "IM 2: vector read at I:data, 19 T" $ do
      let r = run quiet { lnInt = const True, lnVector = 0x10 } enabled { rIM = 2, rI = 0x80 } []
                  [(0x8010, 0x34), (0x8011, 0x12)] 1
          (s, t, rs) = nth r 0
      s @?= Int
      t @?= 19
      rPC rs @?= 0x1234
      pushed r rs @?= 0x0000
  , testCase "IM 0: an RST from the bus" $ do
      let r = run quiet { lnInt = const True, lnVector = 0xEF } enabled { rIM = 0 } [] [] 1
          (s, t, rs) = nth r 0
      s @?= Int
      t @?= 13
      rPC rs @?= 0x0028
  , testCase "/INT is masked by IFF1 (DI)" $ do
      let r = run quiet { lnInt = (>= 1) } enabled [0xF3, 0x00, 0x00] [] 3
      [ s | (s, _, _) <- runSteps r ] @?= [Instr 0xF3, Instr 0x00, Instr 0x00]
  , testCase "EI: no interrupt until after the next instruction, even after EI EI" $ do
      let r = run quiet { lnInt = const True } zeroRegs { rIM = 1 } [0xFB, 0xFB, 0x00, 0x00] [] 4
          steps = [ s | (s, _, _) <- runSteps r ]
      steps @?= [Instr 0xFB, Instr 0xFB, Instr 0x00, Int]
      let (_, _, rs) = nth r 3
      pushed r rs @?= 0x0003
  , testCase "no interrupt between a prefix and its opcode" $ do
      -- LD IX,nn is one step: the interrupt comes after the whole of it.
      let r = run quiet { lnInt = (>= 1) } enabled [0xDD, 0x21, 0x34, 0x12] [] 2
          (s0, t0, rs0) = nth r 0
          (s1, _, rs1) = nth r 1
      (s0, t0, rIX rs0) @?= (Instr 0xDD, 14, 0x1234)
      s1 @?= Int
      pushed r rs1 @?= 0x0004
  , testCase "NMI: on the edge only, 11 T, IFF2 keeps IFF1, RETN restores it" $ do
      -- 0066h: RETN.
      let r = run quiet { lnNmi = (>= 1) } enabled [0x00, 0x00, 0x00, 0x00] [(0x66, 0xED), (0x67, 0x45)] 4
          steps = [ s | (s, _, _) <- runSteps r ]
          (_, t1, rs1) = nth r 1
          (_, _, rs2) = nth r 2
      steps @?= [Instr 0x00, Nmi, Instr 0xED, Instr 0x00]
      t1 @?= 11
      (rPC rs1, rIFF1 rs1, rIFF2 rs1) @?= (0x0066, False, True)
      pushed r rs1 @?= 0x0001
      (rPC rs2, rIFF1 rs2) @?= (0x0001, True)
  , testCase "NMI beats /INT" $ do
      let r = run quiet { lnNmi = const True, lnInt = const True } enabled [] [] 1
          (s, _, _) = nth r 0
      s @?= Nmi
  , testCase "HALT: 4 T fetches with R counting, /INT pushes the address after HALT" $ do
      let r = run quiet { lnInt = (>= 3) } enabled [0x76] [] 4
          steps = [ (s, t) | (s, t, _) <- runSteps r ]
          (_, _, rs2) = nth r 2
          (_, _, rs3) = nth r 3
      steps @?= [(Instr 0x76, 4), (Halted, 4), (Halted, 4), (Int, 13)]
      (rPC rs2, rHalted rs2, rR rs2) @?= (0x0001, True, 3)
      (rHalted rs3, rPC rs3) @?= (False, 0x0038)
      pushed r rs3 @?= 0x0001
  ]
