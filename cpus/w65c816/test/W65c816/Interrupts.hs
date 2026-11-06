{-|
What SingleStepTests can't show (PLAN.md step 7.1): interrupts, WAI and
STP, the reset. Each test runs a small program in bank 0 with the NMI
and IRQ lines driven by cycle number. The exact cycle an interrupt is
taken at is the 6502's rule here, *to verify* at step 7.3 against the
5A22 (the H/V IRQ timer tests).
-}
module W65c816.Interrupts
  ( interruptTests
  ) where

import Prelude

import Control.Monad (replicateM)
import Control.Monad.ST (runST)
import Data.Bits ((.&.))
import Data.STRef
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MV
import Data.Word
import Test.Tasty
import Test.Tasty.HUnit

import W65c816.Spec
import W65c816.Types

-- | Lines as a function of the cycle number (0 = the first cycle run).
data Lines = Lines { lnNmi, lnIrq :: Int -> Bool }

quiet :: Lines
quiet = Lines (const False) (const False)

data Out = Out
  { outSteps :: [Step]
  , outRegs  :: Regs
  , outMem   :: U.Vector Word8
  , outStops :: Int -- ^ stopped cycles seen
  }

-- | Bank 0 RAM: the program at 0200h, handlers (a NOP, then RTI) at 0300h
-- (NMI), 0400h (IRQ), 0500h (BRK), 0600h (COP); vectors for both modes.
-- Runs @n@ steps from the given registers (PC 0200h).
run :: Lines -> Regs -> [Word8] -> Int -> Out
run ls r0 prog n = runST $ do
  mem <- MV.replicate 0x10000 0xEA
  let poke a = MV.write mem a
      vec a (v :: Int) = poke a (fromIntegral (v .&. 0xFF)) >> poke (a + 1) (fromIntegral (v `div` 256))
  mapM_ (\(i, b) -> poke (0x200 + i) b) (zip [0 ..] prog)
  mapM_ (\h -> poke h 0xEA >> poke (h + 1) 0x40) [0x300, 0x400, 0x500, 0x600]
  -- native: COP FFE4, BRK FFE6, NMI FFEA, IRQ FFEE; emulation: COP FFF4,
  -- NMI FFFA, RESET FFFC, IRQ/BRK FFFE
  mapM_ (uncurry vec) [(0xFFE4, 0x600), (0xFFE6, 0x500), (0xFFEA, 0x300), (0xFFEE, 0x400)
                      , (0xFFF4, 0x600), (0xFFFA, 0x300), (0xFFFC, 0x200), (0xFFFE, 0x400)]
  cyc <- newSTRef (0 :: Int)
  stops <- newSTRef (0 :: Int)
  let tick = modifySTRef' cyc (+ 1)
      lineAt f = (\c -> f (c - 1)) <$> readSTRef cyc
      bus = Bus
        { busRead = \_ _ a -> tick >> MV.read mem (fromIntegral a .&. 0xFFFF)
        , busWrite = \vda _ a v -> tick >> if vda then MV.write mem (fromIntegral a .&. 0xFFFF) v else pure ()
        , busIdle = \_ _ -> tick
        , busStopped = tick >> modifySTRef' stops (+ 1)
        , busNmi = lineAt (lnNmi ls)
        , busIrq = lineAt (lnIrq ls)
        }
  cpu <- newCpu w65c816 bus r0 { rPC = 0x200 }
  steps <- replicateM n (step cpu)
  r <- getRegs cpu
  m <- U.freeze mem
  st <- readSTRef stops
  pure (Out steps r m st)

native, emulation :: Regs
native = resetRegs { rE = False, rP = 0x30, rS = 0x1FF }
emulation = resetRegs { rP = 0x30 }

interruptTests :: TestTree
interruptTests = testGroup "interrupts, WAI, STP, reset"
  [ testCase "NMI (native): PBR, PC, P pushed, I set, D cleared, vector FFEA" $ do
      -- asserted from the first cycle: the NOP's second-to-last cycle sees it
      let o = run (Lines (const True) (const False)) native { rP = 0x38, rPBR = 0 } [0xEA, 0xEA] 2
      take 2 (outSteps o) @?= [Instr 0xEA, Nmi]
      rPC (outRegs o) @?= 0x300
      rP (outRegs o) .&. 0x0C @?= 0x04
      -- 01FFh PBR, 01FEh PCH, 01FDh PCL, 01FCh P
      map (outMem o U.!) [0x1FF, 0x1FE, 0x1FD, 0x1FC] @?= [0x00, 0x02, 0x01, 0x38]
  , testCase "NMI is edge triggered: one NMI for a line held low" $ do
      let o = run (Lines (>= 1) (const False)) native [] 5
      length (filter (== Nmi) (outSteps o)) @?= 1
  , testCase "IRQ (emulation): B clear in the pushed P, vector FFFE" $ do
      let o = run (Lines (const False) (const True)) emulation { rP = 0x30 } [0xEA] 2
      outSteps o @?= [Instr 0xEA, Irq]
      rPC (outRegs o) @?= 0x400
      outMem o U.! 0x1FD @?= 0x20 -- P = 30h with B (bit 4) clear
  , testCase "a line raised in an instruction's last cycle waits one instruction" $ do
      let o = run (Lines (>= 1) (const False)) native [0xEA, 0xEA] 3
      outSteps o @?= [Instr 0xEA, Instr 0xEA, Nmi]
  , testCase "IRQ is masked by I" $ do
      let o = run (Lines (const False) (const True)) native { rP = 0x34 } [] 4
      Irq `elem` outSteps o @?= False
  , testCase "BRK and COP (native): their own vectors, signature byte skipped" $ do
      let b = run quiet native [0x00, 0x99] 1
          c = run quiet native [0x02, 0x99] 1
      (rPC (outRegs b), rPC (outRegs c)) @?= (0x500, 0x600)
      -- the return address is the byte after the signature
      map (outMem b U.!) [0x1FE, 0x1FD] @?= [0x02, 0x02]
  , testCase "WAI waits, then takes the IRQ" $ do
      let o = run (Lines (const False) (>= 10)) native [0xCB] 12
      take 1 (outSteps o) @?= [Instr 0xCB]
      Irq `elem` outSteps o @?= True
      outStops o > 0 @?= True
  , testCase "WAI with I set: an IRQ ends the wait without the handler" $ do
      let o = run (Lines (const False) (>= 10)) native { rP = 0x34 } [0xCB, 0xE8] 20
      Irq `elem` outSteps o @?= False
      Instr 0xE8 `elem` outSteps o @?= True
  , testCase "STP stops for good" $ do
      let o = run (Lines (>= 5) (>= 5)) native [0xDB, 0xE8] 10
      drop 1 (outSteps o) @?= replicate 9 Stop
  , testCase "reset: emulation mode, vector FFFC" $ do
      let o = runST $ do
            mem <- MV.replicate 0x10000 0
            MV.write mem 0xFFFC 0x34
            MV.write mem 0xFFFD 0x12
            let bus = Bus (\_ _ a -> MV.read mem (fromIntegral a .&. 0xFFFF)) (\_ _ _ _ -> pure ())
                          (\_ _ -> pure ()) (pure ()) (pure False) (pure False)
            cpu <- newCpu w65c816 bus native { rD = 0x1234, rDBR = 5, rPBR = 7, rP = 0x08 }
            reset cpu
            getRegs cpu
      (rPC o, rE o, rD o, rDBR o, rPBR o, rP o .&. 0x3C, rS o .&. 0xFF00) @?= (0x1234, True, 0, 0, 0, 0x34, 0x0100)
  , testCase "(d,x) in emulation mode with DL /= 0: the pointer's high byte wraps in its page" $ do
      -- gilyon's cputest README: E = 1, D = 011Ah, X = EEh, LDA (F7h,X):
      -- the pointer at 02FFh and 0200h
      -- the pointer at 02FFh and 0200h: 0400h, which holds 5Ah; read
      -- from 0300h instead it would be 0500h, which holds 11h
      runPtr @?= 0x5A
  ]
 where
  runPtr = runST $ do
    mem <- MV.replicate 0x10000 0
    mapM_ (uncurry (MV.write mem)) [(0x800, 0xA1), (0x801, 0xF7), (0x2FF, 0x00), (0x200, 0x04), (0x300, 0x05), (0x400, 0x5A), (0x500, 0x11)]
    let bus = Bus (\_ _ a -> MV.read mem (fromIntegral a .&. 0xFFFF)) (\_ _ _ _ -> pure ())
                  (\_ _ -> pure ()) (pure ()) (pure False) (pure False)
    cpu <- newCpu w65c816 bus emulation { rD = 0x011A, rX = 0xEE, rPC = 0x800 }
    _ <- step cpu
    fromIntegral . (.&. 0xFF) . rA <$> getRegs cpu
