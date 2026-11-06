{-|
What SingleStepTests can't show: interrupt polling, the NMI edge, the
BRK hijack, RDY and reset (nesdev wiki, "CPU interrupts"). Each test runs a
small program in 64 KiB of RAM with the NMI, IRQ and RDY lines driven by
cycle number. The NES ROMs of step 3.2 (Blargg @cpu_interrupts_v2@) check
the same rules against hardware timing.
-}
module Mos6502.Interrupts
  ( interruptTests
  ) where

import Prelude

import Control.Monad (replicateM)
import Control.Monad.ST (runST)
import Data.STRef
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MV
import Data.Word
import Test.Tasty
import Test.Tasty.HUnit

import Mos6502.Spec

-- | Lines as a function of the cycle number (0 = the first cycle run).
data Lines = Lines
  { lnNmi, lnIrq, lnRdy :: Int -> Bool }

quiet :: Lines
quiet = Lines (const False) (const False) (const True)

data Run = Run
  { runSteps  :: [Step]
  , runRegs   :: Regs
  , runTrace  :: [(Int, Word16, Bool)] -- ^ cycle, address, write
  , runMem    :: U.Vector Word8
  , runCycles :: Int
  }

-- | Program at $0200, NMI handler at $0300, IRQ/BRK handler at $0400
-- (both a NOP and RTI), reset vector $0200. Runs @n@ steps from the given
-- registers (PC $0200, S $FD).
run :: Lines -> Word8 -> [Word8] -> Int -> Run
run ls p prog n = runST $ do
  mem <- MV.replicate 0x10000 0xEA
  let poke a = MV.write mem a
  mapM_ (\(i, b) -> poke (0x200 + i) b) (zip [0 ..] prog)
  mapM_ (uncurry poke) [(0x300, 0xEA), (0x301, 0x40), (0x400, 0xEA), (0x401, 0x40)]
  mapM_ (uncurry poke) [(0xFFFA, 0x00), (0xFFFB, 0x03), (0xFFFC, 0x00), (0xFFFD, 0x02), (0xFFFE, 0x00), (0xFFFF, 0x04)]
  trace <- newSTRef []
  cyc <- newSTRef (0 :: Int)
  let now = readSTRef cyc
      tick a w = do
        c <- now
        modifySTRef' trace ((c, a, w) :)
        writeSTRef cyc (c + 1)
      -- the lines are sampled at the end of the cycle that just ran
      lineAt f = (\c -> f (c - 1)) <$> now
      bus = Bus
        { busRead = \a -> do
            v <- MV.read mem (fromIntegral a)
            tick a False
            pure v
        , busWrite = \a v -> MV.write mem (fromIntegral a) v >> tick a True
        , busRdy = lnRdy ls <$> now
        , busNmi = lineAt (lnNmi ls)
        , busIrq = lineAt (lnIrq ls)
        }
  cpu <- newCpu ricoh2a03 bus (Regs 0 0 0 0xFD p 0x200)
  steps <- replicateM n (step cpu)
  r <- getRegs cpu
  tr <- reverse <$> readSTRef trace
  m <- U.freeze mem
  c <- cpuCycles cpu
  pure (Run steps r tr m c)

-- I clear, unused bit set.
pClear, pI :: Word8
pClear = 0x20
pI = 0x24

nop, cli, sei, jmp :: Word8
nop = 0xEA
cli = 0x58
sei = 0x78
jmp = 0x4C

interruptTests :: TestTree
interruptTests = testGroup "interrupts, RDY, reset"
  [ testCase "IRQ masked while I is set" $
      runSteps (run quiet { lnIrq = const True } pI [nop, nop, nop] 3) @?= [Instr nop, Instr nop, Instr nop]
  , testCase "CLI: the IRQ comes one instruction late" $
      runSteps (run quiet { lnIrq = const True } pI [cli, nop, nop] 4)
        @?= [Instr cli, Instr nop, Irq, Instr nop]
  , testCase "SEI: an IRQ seen before it is still taken, with I set in the pushed P" $ do
      let r = run quiet { lnIrq = const True } pClear [sei, nop] 2
      runSteps r @?= [Instr sei, Irq]
      -- P pushed at $01FB: I set (SEI ran), B clear.
      runMem r U.! 0x1FB @?= pI
  , testCase "IRQ dispatch: 7 cycles, PC and P pushed, I set, PC from $FFFE" $ do
      let r = run quiet { lnIrq = (>= 0) } pClear [nop, nop] 2
      runSteps r @?= [Instr nop, Irq]
      runCycles r @?= 2 + 7
      (rPC (runRegs r), rS (runRegs r), rP (runRegs r)) @?= (0x400, 0xFA, pI)
      (runMem r U.! 0x1FD, runMem r U.! 0x1FC) @?= (0x02, 0x01)
  , testCase "NMI is edge triggered: held high, it fires once" $
      runSteps (run quiet { lnNmi = const True } pI [nop, nop, nop, nop] 5)
        @?= [Instr nop, Nmi, Instr nop, Instr 0x40, Instr nop]
  , testCase "an NMI edge in an instruction's last cycle waits for the next one" $
      -- NOP takes cycles 0 and 1; the edge at the end of cycle 1 is polled
      -- after the second NOP.
      runSteps (run quiet { lnNmi = (>= 1) } pI [nop, nop, nop] 3) @?= [Instr nop, Instr nop, Nmi]
  , testCase "a taken branch without page crossing delays a new IRQ by one instruction" $
      -- BNE +0 (Z clear) at $0200: cycles 0 to 2; IRQ from cycle 1 (its
      -- operand cycle).
      runSteps (run quiet { lnIrq = (>= 1) } pClear [0xD0, 0x00, nop, nop] 3)
        @?= [Instr 0xD0, Instr nop, Irq]
  , testCase "a taken branch across a page polls normally" $
      -- JMP $02FD; BNE +2 at $02FD crosses to $0301 (the NMI handler's RTI
      -- byte doesn't matter: the IRQ comes first).
      let prog = [jmp, 0xFD, 0x02] <> replicate (0xFD - 3) nop <> [0xD0, 0x02]
      in runSteps (run quiet { lnIrq = (>= 4) } pClear prog 3) @?= [Instr jmp, Instr 0xD0, Irq]
  , testCase "BRK hijacked by an NMI: NMI vector, B still pushed set" $ do
      let r = run quiet { lnNmi = (>= 2) } pI [0x00, 0x00, nop] 2
      runSteps r @?= [Instr 0x00, Instr nop]
      rPC (runRegs r) @?= 0x301
      runMem r U.! 0x1FB @?= (pI + 0x10)
  , testCase "the first handler instruction runs before a waiting NMI (after BRK)" $
      -- NMI edge in cycle 5 (vector low byte), after the vector was chosen:
      -- polled in BRK's second-to-last cycle, but not taken right after it.
      runSteps (run quiet { lnNmi = (>= 5) } pI [0x00, 0x00] 3) @?= [Instr 0x00, Instr nop, Nmi]
  , testCase "RDY low stalls a read cycle, repeating the address" $ do
      -- LDA $1234: reads at cycles 0, 1, 2, then 3; RDY low for cycles 3 to 5.
      let r = run quiet { lnRdy = \c -> c < 3 || c > 5 } pI [0xAD, 0x34, 0x12] 1
      runCycles r @?= 4 + 3
      [a | (_, a, _) <- runTrace r] @?= [0x200, 0x201, 0x202, 0x1234, 0x1234, 0x1234, 0x1234]
  , testCase "SHA: RDY low on the dummy read drops the & (H+1) (AccuracyCoin SH* 7, VICE)" $ do
      -- LDA #$FF, LDX #$FF, LDY #$10 (cycles 0-5), SHA $1E00,Y: opcode 6,
      -- operand 7 and 8, dummy read 9, write 10 at $1E10 (no page crossing).
      let prog = [0xA9, 0xFF, 0xA2, 0xFF, 0xA0, 0x10, 0x9F, 0x00, 0x1E]
          normal = run quiet pI prog 4
          held = run quiet { lnRdy = \c -> c /= 9 } pI prog 4
      runMem normal U.! 0x1E10 @?= 0x1F   -- $FF & $FF & ($1E + 1)
      runMem held U.! 0x1E10 @?= 0xFF
      runCycles held @?= runCycles normal + 1
  , testCase "RDY doesn't stall writes" $ do
      -- STA $1234: the write is cycle 3.
      let r = run quiet { lnRdy = \c -> c /= 3 } pI [0x8D, 0x34, 0x12] 1
      runCycles r @?= 4
  , testCase "reset: 7 cycles, S down by 3 with reads, I set, PC from $FFFC" $ do
      let r = runST $ do
            mem <- MV.replicate 0x10000 (0 :: Word8)
            MV.write mem 0xFFFC 0x34 >> MV.write mem 0xFFFD 0x12
            writes <- newSTRef (0 :: Int)
            let bus = Bus (MV.read mem . fromIntegral) (\_ _ -> modifySTRef' writes (+ 1))
                          (pure True) (pure False) (pure False)
            cpu <- newCpu ricoh2a03 bus (Regs 0 0 0 0 0x20 0)
            reset cpu
            (,,) <$> getRegs cpu <*> cpuCycles cpu <*> readSTRef writes
          (regs, cycles, nw) = r
      (rPC regs, rS regs, rP regs, cycles, nw) @?= (0x1234, 0xFD, pI, 7, 0)
  , testCase "JAM: stuck until reset, one $FFFF read per step" $ do
      let r = run quiet { lnNmi = const True } pI [0x02] 6
      runSteps r @?= [Instr 0x02, Jammed, Jammed, Jammed, Jammed, Jammed]
      [a | (_, a, _) <- runTrace r] @?= [0x200, 0x201, 0xFFFF, 0xFFFE, 0xFFFE, 0xFFFF, 0xFFFF]
  ]
