-- | "M68k.Run", the model as a library (PLAN.md, side track P, P.3): a
-- hand-assembled program's counts as a golden, the reset, the trace, the
-- limits, the illegal instruction's exception.
module Main (main) where

import Prelude

import qualified Data.ByteString as B
import Data.IORef
import Data.Word
import Test.Tasty
import Test.Tasty.HUnit

import M68k.Run
import M68k.Spec

-- | Vectors: SSP 0x8000, PC 0x1000.
vectors :: [Word8]
vectors = [0, 0, 0x80, 0, 0, 0, 0x10, 0]

-- | The sum of 99 down to 0 in D0, then STOP:
--
-- > 1000 7000       moveq #0,d0
-- > 1002 7263       moveq #99,d1
-- > 1004 D081       add.l d1,d0
-- > 1006 51C9 FFFC  dbra  d1,$1004
-- > 100A 4E72 2700  stop  #$2700
sumProgram :: [Word8]
sumProgram = [0x70, 0x00, 0x72, 0x63, 0xD0, 0x81, 0x51, 0xC9, 0xFF, 0xFC, 0x4E, 0x72, 0x27, 0x00]

machineWith :: [(Word32, [Word8])] -> IO Machine
machineWith parts = do
  m <- newMachine
  mapM_ (\(a, bs) -> loadImage m a (B.pack bs)) parts
  resetCpu m
  pure m

main :: IO ()
main = defaultMain $ testGroup "m68k-lib"
  [ testCase "the sum program: D0 4950, 203 instructions, 1820 cycles, ended by STOP" $ do
      m <- machineWith [(0, vectors), (0x1000, sumProgram)]
      s <- run m (Cycles 1000000)
      c <- readCpu m
      dReg c 0 @?= 4950
      dReg c 1 @?= 0xFFFF
      statInstructions s @?= 203
      -- 2 MOVEQ (4 each), 100 ADD.l Dn,Dn (8), 99 DBRA taken (10), one
      -- expired (14), STOP (8)
      statCycles s @?= 8 + 800 + 990 + 14 + 8
      statStop s @?= Stopped
      cPc c @?= 0x100E
  , testCase "the same program from GNU as (test/asm/sum.s, make m68k-asm): the same bytes, the same counts" $ do
      bin <- B.readFile "../../build/m68k/asm/sum.bin"
      B.unpack bin @?= sumProgram
      m <- machineWith [(0, vectors), (0x1000, B.unpack bin)]
      s <- run m (Cycles 1000000)
      (statInstructions s, statCycles s) @?= (203, 1820)
  , testCase "reset: SSP and PC from the vectors, supervisor, the queue filled" $ do
      m <- machineWith [(0, vectors), (0x1000, sumProgram)]
      c <- readCpu m
      (cSsp c, cPc c, cSr c, cIr c, cIrc c) @?= (0x8000, 0x1000, 0x2700, 0x7000, 0x7263)
  , testCase "limits: instructions, cycles (the crossing instruction whole)" $ do
      m <- machineWith [(0, vectors), (0x1000, sumProgram)]
      s <- run m (Instructions 10)
      (statInstructions s, statStop s) @?= (10, LimitReached)
      m2 <- machineWith [(0, vectors), (0x1000, sumProgram)]
      s2 <- run m2 (Cycles 13)
      (statInstructions s2, statCycles s2) @?= (3, 16)
  , testCase "trace: each instruction, then its bus cycles in order" $ do
      m <- machineWith [(0, vectors), (0x1000, sumProgram)]
      evs <- newIORef []
      setTrace m (Just (\e -> modifyIORef evs (e :)))
      _ <- run m (Instructions 3)
      es <- reverse <$> readIORef evs
      take 2 es @?= [Instruction 0x1000 0x7000, Read 6 Word 0x1004 0xD081]
      length [ () | Instruction {} <- es ] @?= 3
      -- ADD.l Dn,Dn: the prefetch, then 4 internal cycles
      drop 4 es @?= [Instruction 0x1004 0xD081, Read 6 Word 0x1008 0xFFFC, Idle 4]
  , testCase "ILLEGAL: vector 4, its own address in the frame (not in the SingleStepTests)" $ do
      m <- machineWith [(0, vectors), (0x10, [0, 0, 0x20, 0]), (0x1000, [0x4A, 0xFC]), (0x2000, [0x4E, 0x72, 0x27, 0x00])]
      s <- run m (Cycles 1000)
      c <- readCpu m
      statStop s @?= Stopped
      cSsp c @?= 0x8000 - 6
      frame <- mapM (peekByte m) [0x8000 - 6 .. 0x8000 - 1]
      frame @?= [0x27, 0x00, 0, 0, 0x10, 0x00]
  -- interrupts and trace (not in the SingleStepTests; YACHT's timing)
  , testCase "interrupt: level 4 above the mask, autovector 28, the frame, 44 cycles" $ do
      -- 1000: move #$2000,sr (mask 0); 1004: bra.s *; autovector 28 (0x70) -> 2000: stop #$2700
      m <- machineWith [(0, vectors), (0x70, [0, 0, 0x20, 0]), (0x1000, [0x46, 0xFC, 0x20, 0x00, 0x60, 0xFE]), (0x2000, [0x4E, 0x72, 0x27, 0x00])]
      _ <- run m (Instructions 1)
      evs <- newIORef []
      setTrace m (Just (\e -> modifyIORef evs (e :)))
      setIpl m 4
      s <- run m (Cycles 1000)
      c <- readCpu m
      statStop s @?= Stopped
      (statInstructions s, statCycles s) @?= (1, 44 + 8) -- the interrupt, then STOP
      cSsp c @?= 0x8000 - 6
      frame <- mapM (peekByte m) [0x8000 - 6 .. 0x8000 - 1]
      frame @?= [0x20, 0x00, 0, 0, 0x10, 0x04] -- SR (mask 0), the PC of the BRA
      es <- reverse <$> readIORef evs
      take 3 es @?= [Idle 6, Write 5 Word 0x7FFE 0x1004, Iack 4 Nothing]
  , testCase "interrupt: at or below the mask, none; a device's vector; level 7 on its edge only" $ do
      let prog = [(0, vectors), (0x1000, [0x60, 0xFE]), (0x100, [0, 0, 0x20, 0]), (0x7C, [0, 0, 0x30, 0])
                 , (0x2000, [0x4E, 0x72, 0x27, 0x00]), (0x3000, [0x4E, 0x73])] -- 3000: rte
      -- SR 0x2700 after the reset: level 6 masked
      m <- machineWith prog
      setIpl m 6
      s <- run m (Instructions 5)
      (statInstructions s, statStop s) @?= (5, LimitReached)
      -- level 6 with a vector from the device (64 at 0x100) once the mask is 5
      m2 <- machineWith prog
      writeCpu m2 . (\c -> c { cSr = 0x2500 }) =<< readCpu m2
      setIack m2 (\_ -> pure (Just 64))
      setIpl m2 6
      _ <- run m2 (Cycles 1000)
      cPc <$> readCpu m2 >>= (@?= 0x2004)
      -- level 7 (autovector 31 at 0x7C -> 3000: rte) whatever the mask, but once
      m3 <- machineWith prog
      setIpl m3 7
      s3 <- run m3 (Instructions 3) -- the RTE, then two BRAs: not taken again
      statInstructions s3 @?= 3
      c3 <- readCpu m3
      cPc c3 @?= 0x1000
  , testCase "STOP: an interrupt wakes it, past the STOP" $ do
      -- 1000: stop #$2000 (mask 0); 1004: bra.s *; autovector 26 (0x68) -> 2000: stop #$2700
      m <- machineWith [(0, vectors), (0x68, [0, 0, 0x20, 0]), (0x1000, [0x4E, 0x72, 0x20, 0x00, 0x60, 0xFE]), (0x2000, [0x4E, 0x72, 0x27, 0x00])]
      s <- run m (Cycles 1000)
      statStop s @?= Stopped
      setIpl m 2
      s2 <- run m (Cycles 1000)
      statStop s2 @?= Stopped
      frame <- mapM (peekByte m) [0x8000 - 6 .. 0x8000 - 1]
      frame @?= [0x20, 0x00, 0, 0, 0x10, 0x04] -- the PC after the STOP
  , testCase "trace: T set, each instruction followed by vector 9 (34 cycles), its PC the next one" $ do
      -- 1000: ori #$8000,sr (T on); 1004: moveq #1,d0 (traced); trace vector (0x24) -> 2000: stop #$2700
      m <- machineWith [(0, vectors), (0x24, [0, 0, 0x20, 0]), (0x1000, [0x00, 0x7C, 0x80, 0x00, 0x70, 0x01]), (0x2000, [0x4E, 0x72, 0x27, 0x00])]
      _ <- run m (Instructions 1) -- ORI sets T: it isn't traced itself (T was clear at its start)
      cSsp <$> readCpu m >>= (@?= 0x8000)
      s <- run m (Cycles 1000)
      c <- readCpu m
      dReg c 0 @?= 1
      statCycles s @?= 4 + 34 + 8 -- MOVEQ, the trace, STOP
      frame <- mapM (peekByte m) [0x8000 - 6 .. 0x8000 - 1]
      frame @?= [0xA7, 0x00, 0, 0, 0x10, 0x06]
  , testCase "bus error: a read where no device answers (an ST's probe): vector 2, the 14-byte frame" $ do
      -- 1000: move.w $FF8000,d0 (no device); vector 2 (0x08) -> 2000: moveq #1,d7; stop #$2700
      m <- machineWith [(0, vectors), (0x08, [0, 0, 0x20, 0]), (0x1000, [0x30, 0x39, 0x00, 0xFF, 0x80, 0x00]), (0x2000, [0x7E, 0x01, 0x4E, 0x72, 0x27, 0x00])]
      setBerr m (\a -> a >= 0xFF8000 && a < 0xFF9000)
      s <- run m (Cycles 1000)
      c <- readCpu m
      statStop s @?= Stopped
      dReg c 7 @?= 1
      cSsp c @?= 0x8000 - 14
      frame <- mapM (peekByte m) [0x8000 - 14 .. 0x8000 - 1]
      -- status (IR's top 11 bits, a read, data, FC 5), the address, IR, SR, the PC
      frame @?= [0x30, 0x35, 0x00, 0xFF, 0x80, 0x00, 0x30, 0x39, 0x27, 0x00, 0, 0, 0x10, 0x04]
  , testCase "STOP in user mode: a privilege violation" $ do
      m <- machineWith [(0, vectors), (0x20, [0, 0, 0x30, 0]), (0x1000, [0x46, 0xFC, 0x00, 0x00, 0x4E, 0x72, 0x27, 0x00]), (0x3000, [0x4E, 0x72, 0x27, 0x00])]
      -- MOVE #0,SR (user mode), then STOP: vector 8, which stops
      s <- run m (Cycles 1000)
      c <- readCpu m
      statStop s @?= Stopped
      cPc c @?= 0x3004 -- past the handler's STOP, as 0x100E above
  ]
