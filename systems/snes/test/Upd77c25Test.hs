-- | snes-spec, the DSP-1's chip (M7 step 7.16a; "Snes.Spec.Upd77c25"):
-- every part of the µPD77C25 with our own programs (a small assembler
-- below, the encodings from fullsnes), then the DSP-1's own commands
-- through the SNES's side of DR and SR with the user's firmware dump,
-- skipped without it (docs/systems/snes.md, "The DSP-1": never fetched or
-- committed; SNES_DSP1=FILE, else local/firmware/ in the repository).
module Upd77c25Test (upd77c25Tests) where

import Prelude

import Control.Monad.ST (stToIO)
import Data.Bits
import Data.Int (Int16)
import qualified Data.ByteString as B
import qualified Data.Vector.Unboxed as U
import Data.Word
import System.Directory (doesFileExist)
import System.Environment (lookupEnv)
import Test.Tasty
import Test.Tasty.HUnit

import Snes.Cart (Mapping (..), parseCart)
import qualified Snes.Spec.Machine as SM
import Snes.Spec.Map (dspWindow)
import Snes.Spec.Upd77c25
import Snes.Spec.Upd77c25Asm
import Upd77c25Lockstep (lockstepTests)

-- | A firmware of these instructions (the rest NOPs) and data words.
fwOf :: [Word32] -> [Word16] -> Firmware
fwOf prog dat = Firmware (U.fromList (take 2048 (prog <> repeat 0))) (U.fromList (take 1024 (dat <> repeat 0)))

-- | The chip after n instructions from power-on.
run :: Firmware -> Int -> Upd
run fw n = iterate (step fw) powerOn !! n

-- | A program's A (and its flags) after it.
accAfter :: [Word32] -> (Word16, Flags)
accAfter prog = let u = run (fwOf prog []) (length prog) in (uA u, uFa u)

upd77c25Tests :: IO TestTree
upd77c25Tests = do
  dump <- userDump
  pure $ testGroup "the DSP-1's chip, the uPD77C25 (step 7.16a)" (chipTests <> [firmwareTests dump, lockstepTests dump, cartTest])

-- | Our DSP-1 test cartridge (dsp1.s) on the spec with our firmware
-- (step 7.16d): RESULT 01h, and its 64 products in WRAM as X * Y * 2's
-- high words.
cartTest :: TestTree
cartTest = testCase "dsp1.sfc with our firmware (step 7.16d): RESULT 01h after 30 frames, the 64 products X * Y * 2 / 10000h" $ do
  file <- B.readFile "../../build/snes/asm/dsp1.sfc"
  ipl <- B.readFile "../../build/snes/ipl.bin"
  fw <- either (\e -> assertFailure e >> fail "") pure (firmwareOf dspTestFirmware)
  c <- maybe (assertFailure "dsp1.sfc: no header" >> fail "") pure (parseCart file)
  (stopped, res, prods) <- stToIO $ do
    m <- SM.newMachine ipl c file
    SM.reset m
    SM.attachDsp m fw
    SM.runFrames m 30
    s <- pure True
    r <- SM.readWram m 0
    ps <- mapM (\k -> (\lo hi -> fromIntegral lo .|. fromIntegral hi `shiftL` 8 :: Word16) <$> SM.readWram m (0x1000 + 2 * k) <*> SM.readWram m (0x1001 + 2 * k)) [0 .. 63]
    pure (s, r, ps)
  (stopped, res) @?= (True, 1)
  let xs = take 64 (iterate (\x -> x * 5 + 0x3B41) (0x1234 :: Word16))
      ys = take 64 (iterate (\y -> y * 3 + 0x1357) (0x0F0F :: Word16))
      prod x y = fromIntegral ((2 * fromIntegral (fromIntegral x :: Int16) * fromIntegral (fromIntegral y :: Int16) :: Int) `shiftR` 16) :: Word16
  prods @?= zipWith prod xs ys

chipTests :: [TestTree]
chipTests =
  [ testGroup "the ALU"
      [ testCase "ADD: carry and a signed overflow, S1 and OV1 from it" $ do
          let (r, f) = accAfter [ld 0x7FFF dA, ld 1 dTr, op aAdd False sTr]
          r @?= 0x8000
          (fS0 f, fZ f, fC f, fOv0 f, fS1 f, fOv1 f) @?= (True, False, False, True, True, True)
          let (r2, f2) = accAfter [ld 0xFFFF dA, ld 1 dTr, op aAdd False sTr]
          r2 @?= 0
          (fZ f2, fC f2, fOv0 f2, fOv1 f2) @?= (True, True, False, False)
      , testCase "two overflows in opposite directions: OV1 back to 0" $ do
          let (_, f) = accAfter [ld 0x7FFF dA, ld 1 dTr, op aAdd False sTr, ld 0xFFFF dTr, op aAdd False sTr]
          (fOv0 f, fOv1 f, fS1 f) @?= (True, False, False)
      , testCase "SUB: a borrow; SBB and ADC take the other accumulator's carry" $ do
          let (r, f) = accAfter [ld 1 dA, ld 2 dTr, op aSub False sTr]
          (r, fC f, fS0 f) @?= (0xFFFF, True, True)
          -- B: FFFFh + 1 carries; A: 5 + 1 + carry(B) = 7, 5 - 1 - carry(B) = 3
          let prog = [ld 0xFFFF dB, ld 1 dTr, op aAdd True sTr, ld 5 dA, op aAdc False sTr]
          fst (accAfter prog) @?= 7
          fst (accAfter [ld 0xFFFF dB, ld 1 dTr, op aAdd True sTr, ld 5 dA, op aSbb False sTr]) @?= 3
      , testCase "INC, DEC; OR, AND, XOR and NOT clear C and OV" $ do
          fst (accAfter [ld 0xFFFF dA, op aInc False sTrb]) @?= 0
          fst (accAfter [ld 0 dA, op aDec False sTrb]) @?= 0xFFFF
          let (r, f) = accAfter [ld 0xFFFF dB, ld 1 dTr, op aAdd True sTr, ld 0x0F0F dA, ld 0x00FF dTr, op aAnd False sTr]
          (r, fC f, fOv0 f) @?= (0x000F, False, False)
          fst (accAfter [ld 0x0F0F dA, ld 0x00FF dTr, op aOr False sTr]) @?= 0x0FFF
          fst (accAfter [ld 0x0F0F dA, ld 0x00FF dTr, op aXor False sTr]) @?= 0x0FF0
          fst (accAfter [ld 0x0F0F dA, op aNot False sTrb]) @?= 0xF0F0
      , testCase "the shifts: SAR1 signed, RCL1 with the other carry, SLL2, SLL4, XCHG" $ do
          let (r, f) = accAfter [ld 0x8003 dA, op aSar1 False sTrb]
          (r, fC f) @?= (0xC001, True)
          fst (accAfter [ld 0xFFFF dB, ld 1 dTr, op aAdd True sTr, ld 0x4001 dA, op aRcl1 False sTrb]) @?= 0x8003
          fst (accAfter [ld 0x0001 dA, op aSll2 False sTrb]) @?= 0x0007
          fst (accAfter [ld 0x0001 dA, op aSll4 False sTrb]) @?= 0x001F
          fst (accAfter [ld 0x1234 dA, op aXchg False sTrb]) @?= 0x3412
      , testCase "SGN: 8000h - SA1 (7FFFh after an overflow upwards)" $ do
          fst (accAfter [ld 0x7FFF dA, ld 1 dTr, op aAdd False sTr, mov sSgn dA]) @?= 0x7FFF
          fst (accAfter [mov sSgn dA]) @?= 0x8000
      , testCase "a DST of the accumulator overwrites the ALU's result (its flags kept)" $ do
          let (r, f) = accAfter [ld 0xFFFF dA, ld 5 dTr, aluI False 1 aInc False 0 0 False sTr dA]
          (r, fZ f) @?= (5, True)
      ]
  , testGroup "the multiplier"
      [ testCase "K × L × 2: M the high word, N the low" $ do
          let u = run (fwOf [ld 0x4000 dK, ld 0x4000 dL] []) 2
          (uM u, uN u) @?= (0x2000, 0)
          let u2 = run (fwOf [ld 0xFFFF dK, ld 0x0003 dL] []) 2
          (uM u2, uN u2) @?= (0xFFFF, 0xFFFA)
      , testCase "P = M and N into the ALU" $ do
          let prog = [ld 0x1234 dK, ld 0x0100 dL, aluI False 2 aOr False 0 0 False sTrb dNon]
          fst (accAfter prog) @?= 0x0024
          fst (accAfter [ld 0x1234 dK, ld 0x0100 dL, aluI False 3 aOr False 0 0 False sTrb dNon]) @?= 0x6800
      , testCase "KLR: K from SRC, L from the data ROM at RP; KLM: L from SRC, K from RAM[DP | 40h]" $ do
          let u = run (fwOf [ld 2 dRp, ld 7 dTr, mov sTr dKlr] [0, 0, 0xBEEF]) 3
          (uK u, uL u) @?= (7, 0xBEEF)
          let u2 = run (fwOf [ld 0x43 dDp, ld 0x5555 dMem, ld 3 dDp, ld 9 dTr, mov sTr dKlm] []) 5
          (uK u2, uL u2) @?= (0x5555, 9)
      ]
  , testCase "moves: every register as SRC and as DST" $ do
      let prog = [ ld 0x1111 dA, mov sA dB, mov sB dTrb, mov sTrb dTr, ld 0x2A dDp, mov sDp dK, ld 0x155 dRp
                 , mov sRp dL, ld 0x0400 dSr, mov sSr dMem, mov sK dA, mov sL dB ]
          u = run (fwOf prog []) (length prog)
      (uA u, uB u, uTrb u, uTr u, uK u, uL u, uRam u U.! 0x2A) @?= (0x2A, 0x155, 0x1111, 0x1111, 0x2A, 0x155, 0x0400)
  , testGroup "the pointers"
      [ testCase "DPL: INC and DEC wrap in the low four bits, CLR; DPH XORs the high four" $ do
          let dpAfter prog = uDp (run (fwOf prog []) (length prog))
          dpAfter [ld 0x2F dDp, aluI False 0 0 False 1 0 False 0 0] @?= 0x20
          dpAfter [ld 0x20 dDp, aluI False 0 0 False 2 0 False 0 0] @?= 0x2F
          dpAfter [ld 0x27 dDp, aluI False 0 0 False 3 0 False 0 0] @?= 0x20
          dpAfter [ld 0x27 dDp, aluI False 0 0 False 0 5 False 0 0] @?= 0x77
      , testCase "RP: decremented in 10 bits, RO the data ROM there" $ do
          let u = run (fwOf [ld 0 dRp, aluI False 0 0 False 0 0 True 0 0] []) 2
          uRp u @?= 0x3FF
          uA (run (fwOf [ld 1 dRp, mov sRo dA] [0x1111, 0x2222]) 2) @?= 0x2222
      , testCase "MEM: RAM[DP] read and written" $ do
          let u = run (fwOf [ld 0x12 dDp, ld 0xCAFE dMem, ld 0 dDp, ld 0x12 dDp, mov sMem dB] []) 5
          uB u @?= 0xCAFE
      ]
  , testGroup "jumps and calls"
      [ testCase "CALL pushes the next address, RT in an ALU instruction returns" $ do
          let prog = [jp 0x140 4, ld 1 dB, jp 0x100 7, 0, aluI True 0 0 False 0 0 False 0 0, 0, 0, ld 2 dA]
              u = run (fwOf prog []) 5
          (uA u, uB u, uPc u) @?= (2, 1, 8)
      , testCase "each condition: jumps on its flag, not on the other" $ do
          let flagged = [ld 0xFFFF dA, ld 1 dTr, op aAdd False sTr] -- A: C, Z set
              takes brch = uPc (run (fwOf (flagged <> [jp brch 0x100]) []) 4) == 0x100
          map takes [0x082, 0x08A, 0x080, 0x088, 0x0A0, 0x090] @?= [True, True, False, False, True, True]
      , testCase "JDPL0, JDPLNF on DP's low bits" $ do
          let pcAfter dp brch = uPc (run (fwOf [ld dp dDp, jp brch 0x100] []) 2)
          (pcAfter 0x30 0x0B0, pcAfter 0x31 0x0B0, pcAfter 0x3F 0x0B3, pcAfter 0x3E 0x0B3) @?= (0x100, 2, 2, 0x100)
      , testCase "the reset: PC 0, the flags, SR 0, RP 3FFh; RAM and A kept" $ do
          let u = reset (run (fwOf [ld 0x55 dA, ld 0x7FFF dA, ld 1 dTr, op aAdd False sTr, ld 0x0400 dSr, ld 9 dRp] []) 6)
          (uPc u, uFa u, uSr u, uRp u, uA u) @?= (0, flags0, 0, 0x3FF, 0x8000)
      ]
  , testGroup "the SNES's side (DR and SR)"
      [ testCase "8-bit: the chip's write to DR sets RQM, the SNES's read clears it" $ do
          let u = run (fwOf [ld 0x0400 dSr, ld 0x12AB dDr] []) 2
          (rqm u, snesReadSr u .&. 0x80) @?= (True, 0x80)
          let (b, u') = snesReadDr u
          (b, rqm u') @?= (0xAB, False)
      , testCase "16-bit: low then high, DRS between, RQM cleared after the high" $ do
          let u = run (fwOf [ld 0xBEEF dDr] []) 1
              (lo, u1) = snesReadDr u
              (hi, u2) = snesReadDr u1
          (lo, testBit (snesReadSr u1) 4, rqm u1, hi, rqm u2, testBit (snesReadSr u2) 4) @?= (0xEF, True, True, 0xBE, False, False)
          let w = snesWriteDr 0x34 (snesWriteDr 0x12 u)
          (uDr w, rqm w) @?= (0x3412, False)
      , testCase "the chip's SR write keeps RQM, DRS and bits 6-2" $ do
          let u = run (fwOf [ld 0x1234 dDr, ld 0xFFFF dSr] []) 2
          uSr u @?= (0x8000 .|. (0xFFFF .&. complement 0x907C))
      , testCase "JRQM $: the chip waits while RQM is set; DR's read by the chip (SRC DR) sets it again, DRNF doesn't" $ do
          let prog = [ld 0x0400 dSr, ld 0 dDr, jp 0x0BE 2, mov sDr dA, jp 0x0BE 4, mov sDrnf dB]
              u = run (fwOf prog []) 5
          uPc u @?= 2
          let u2 = iterate (step (fwOf prog [])) (snesWriteDr 0x42 u) !! 3
          (uA u2, rqm u2, uPc u2) @?= (0x42, True, 4)
      ]
  , testCase "the cartridge's windows (step 7.16b): LoROM without SRAM from bank 30h, with from 20h; HiROM 00h-1Fh; mirrored at 80h" $ do
      let lo0 = dspWindow LoRom 0
          lo2 = dspWindow LoRom 0x800
          hi = dspWindow HiRom 0
      map lo0 [0x308000, 0x30C000, 0x3FBFFF, 0x2F8000, 0xB0C000, 0x307FFF] @?= [Just False, Just True, Just False, Nothing, Just True, Nothing]
      map lo2 [0x208000, 0x3FC000, 0x1F8000, 0x700000] @?= [Just False, Just True, Nothing, Nothing]
      map hi [0x006000, 0x007000, 0x1F6FFF, 0x206000, 0x805000, 0x9F7FFF] @?= [Just False, Just True, Just False, Nothing, Nothing, Just True]
  , testGroup "the firmware file"
      [ testCase "little-endian, big-endian and the padded 10 KiB form, told by JRQM" $ do
          let ops = [0x97C000, 0x123456] :: [Word32]
              le w = map (fromIntegral . (w `shiftR`)) [0, 8, 16]
              be w = map (fromIntegral . (w `shiftR`)) [16, 8, 0]
              pad n xs = B.pack (take n (xs <> repeat 0))
              fl = firmwareOf (pad 0x2000 (concatMap le ops))
              fb = firmwareOf (pad 0x2000 (concatMap be ops))
              f10 = firmwareOf (pad 0x2800 (concatMap (\w -> be w <> [0xFF]) ops))
              firstTwo = fmap (U.toList . U.take 2 . fwProgram)
          (firstTwo fl, firstTwo fb, firstTwo f10) @?= (Right ops, Right ops, Right ops)
          either (const True) (const False) (firmwareOf (B.replicate 100 0)) @? "a wrong size refused"
      ]
  ]

-- | The user's DSP-1 dump, if there.
userDump :: IO (Maybe Firmware)
userDump = do
  env <- lookupEnv "SNES_DSP1"
  let path = maybe "../../local/firmware/DSP1 B (Enhancement Chip).bin" id env
  ok <- doesFileExist path
  if not ok then pure Nothing else either (const Nothing) Just . firmwareOf <$> B.readFile path

-- | The DSP-1's commands through DR, as a game's 65C816 gives them: a
-- byte when RQM says the chip is ready; the command byte, then 16-bit
-- words low byte first.
firmwareTests :: Maybe Firmware -> TestTree
firmwareTests Nothing = testGroup "the DSP-1's commands (no firmware dump: skipped)" []
firmwareTests (Just fw) = testGroup "the DSP-1's commands (the user's dump)"
  -- (2Fh and 1Fh take one input word, of no use: the firmware waits for
  -- it before it answers)
  [ testCase "2Fh, the ROM's version: 0101h (DSP-1B)" $ do
      let (out, _) = command fw [0x2F] [0] 1
      out @?= [0x0101]
  , testCase "00h, a 16-bit multiplication: K × I / 8000h" $ do
      let (out, _) = command fw [0x00] [0x4000, 0x4000] 1
      out @?= [0x2000]
      let (out2, _) = command fw [0x00] [0x7FFF, 0xC000] 1
      out2 @?= [fromIntegral ((0x7FFF * (-0x4000) :: Int) `div` 0x8000)]
  , testCase "1Fh, the data ROM sent out: every word as in the file" $ do
      let (out, _) = command fw [0x1F] [0] 1024
      out @?= U.toList (fwData fw)
  ]

-- | A command into the chip from power-on, its 16-bit inputs, then n
-- 16-bit outputs read, at the SNES's pace: a game's access to the
-- cartridge takes some 8 master clocks, so the chip runs 'gap'
-- instructions (one every 2.83 master clocks) after each, and a game's
-- wait for RQM (LDA SR, BPL) polls it every 'gap' (at most 100,000
-- instructions a wait). The DSP-1 reads a command with SRC DR, which sets
-- RQM at once: only that time gives it its switch to 16-bit words.
command :: Firmware -> [Word8] -> [Word16] -> Int -> ([Word16], Upd)
command fw cmd ins n =
  let access f u = run' gap (f u)
      write b = access (snesWriteDr b) . waitRqm
      u0 = waitRqm (run' gap powerOn)
      u1 = foldl (flip write) u0 cmd
      u2 = foldl (\u w -> access (snesWriteDr (fromIntegral (w `shiftR` 8))) (write (fromIntegral w) u)) u1 ins
      readByte u = let (b, u') = snesReadDr (waitRqm u) in (b, run' gap u')
      go 0 u acc = (reverse acc, u)
      go k u acc =
        let (lo, ua) = readByte u
            (hi, ub) = let (b, u') = snesReadDr ua in (b, run' gap u')
        in go (k - 1 :: Int) ub ((fromIntegral hi `shiftL` 8 .|. fromIntegral lo) : acc)
  in go n u2 []
 where
  gap = 12
  run' k u = iterate (step fw) u !! k
  waitRqm u = case [ v | v <- take (100000 `div` gap) (iterate (run' gap) u), rqm v ] of
    v : _ -> v
    [] -> u

