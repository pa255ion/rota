-- | snes-spec, the uPD77C25 in Clash (M7 step 7.16c): "Snes.Upd77c25"
-- clock by clock against the spec ("Snes.Spec.Upd77c25") instruction by
-- instruction. The circuit's memories are modelled here as block RAMs (a
-- clock's address read the next clock), an instruction falls due every
-- third clock (the chip's pace against a master clock a clock), and the
-- SNES's accesses come between instructions. After each instruction
-- every register and the RAM must be the spec's. Random programs (every
-- instruction, random data ROM), and the user's firmware with a few
-- commands (skipped without it).
module Upd77c25Lockstep (lockstepTests) where

import Prelude

import qualified Clash.Prelude as C
import Data.Bits
import qualified Data.Vector.Unboxed as U
import Data.Int (Int16, Int32)
import Data.Word
import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.ByteString as B
import Snes.Dsp1
import qualified Snes.Upd77c25Ooc as Ooc
import Platform.Domains (DomPix)
import qualified Snes.Spec.Upd77c25 as S
import Snes.Spec.Upd77c25Asm (dspTestFirmware)
import Snes.Upd77c25

-- | A small generator (no dependencies): 64-bit LCG, the high bits.
next :: Word64 -> (Word32, Word64)
next s = let s' = s * 6364136223846793005 + 1442695040888963407 in (fromIntegral (s' `shiftR` 32), s')

randoms :: Word64 -> [Word32]
randoms = go
 where
  go s = let (x, s') = next s in x : go s'

-- | A random firmware: instructions of every kind (the program's jumps
-- inside it), random data words; without jumps ('straight') the chip
-- runs through all 2,048 in turn, each a different instruction on a
-- different state.
randomFirmware :: Bool -> Word64 -> S.Firmware
randomFirmware straight seed =
  let rs = randoms seed
      (ops, rest) = splitAt 2048 rs
      dat = take 1024 rest
      -- (jumps one in 16: else the programs soon run in short loops)
      shape r = let o = r .&. 0xFFFFFF
                in if o `shiftR` 22 == 2 && (straight || (r `shiftR` 24) .&. 3 /= 0) then o .&. 0x3FFFFF else o
  in S.Firmware (U.fromList (map shape ops)) (U.fromList (map fromIntegral dat))

-- | The circuit's registers as the spec's.
specOf :: Upd -> U.Vector Word16 -> S.Upd
specOf u ram = S.Upd
  { S.uPc = fromIntegral (upPc u), S.uStack = map fromIntegral (C.toList (upStack u)), S.uDp = fromIntegral (upDp u)
  , S.uRp = fromIntegral (upRp u), S.uK = w (upK u), S.uL = w (upL u), S.uM = w (upM u), S.uN = w (upN u)
  , S.uA = w (upA u), S.uB = w (upB u), S.uFa = fl (upFa u), S.uFb = fl (upFb u), S.uTr = w (upTr u)
  , S.uTrb = w (upTrb u), S.uSr = w (upSr u), S.uDr = w (upDr u), S.uSo = w (upSo u), S.uRam = ram }
 where
  w = fromIntegral
  fl f = S.Flags (ufS0 f) (ufS1 f) (ufC f) (ufZ f) (ufOv0 f) (ufOv1 f)

-- | The SNES's access between instructions: Nothing, or one of 'SnesAcc'
-- from a random word.
accOf :: Word32 -> Maybe SnesAcc
accOf r = case r `mod` 8 of
  0 -> Just SReadDr
  1 -> Just SReadSr
  2 -> Just (SWriteDr (fromIntegral (r `shiftR` 8)))
  _ -> Nothing

specAcc :: Maybe SnesAcc -> S.Upd -> (Word8, S.Upd)
specAcc a u = case a of
  Just SReadDr -> S.snesReadDr u
  Just SReadSr -> (S.snesReadSr u, u)
  Just (SWriteDr v) -> (0, S.snesWriteDr (fromIntegral v) u)
  Nothing -> (0, u)

-- | Both run n instructions, the SNES's accesses from the list between
-- them; the first difference, if any.
lockstep :: S.Firmware -> [Maybe SnesAcc] -> Int -> Either String Int
lockstep fw = lockstepFrom fw (updPowerOn, U.replicate 256 0)

-- | A random start, the same in both: every register, the flags, RAM
-- (M and N as K and L give them).
randomStart :: Word64 -> (Upd, U.Vector Word16)
randomStart seed =
  let r = randoms seed
      w i = fromIntegral (r !! i) :: C.Unsigned 16
      b i = testBit (r !! 20) i
      fl o = UFlags (b o) (b (o + 1)) (b (o + 2)) (b (o + 3)) (b (o + 4)) (b (o + 5))
      k = w 4
      l = w 5
      p = 2 * fromIntegral (fromIntegral k :: Int16) * fromIntegral (fromIntegral l :: Int16) :: Int32
      u = updPowerOn
        { upStack = C.map (\i -> fromIntegral (r !! (6 + fromIntegral i))) C.indicesI, upDp = fromIntegral (r !! 1)
        , upRp = fromIntegral (r !! 2), upK = k, upL = l, upM = fromIntegral (p `shiftR` 16), upN = fromIntegral p
        , upA = w 10, upB = w 11, upFa = fl 0, upFb = fl 6, upTr = w 12, upTrb = w 13
        , upSr = w 14 .&. 0x6F83, upDr = w 15, upSo = w 16 }
  in (u, U.fromList (map fromIntegral (take 256 (drop 30 r))))

-- | 'lockstep' from a start.
lockstepFrom :: S.Firmware -> (Upd, U.Vector Word16) -> [Maybe SnesAcc] -> Int -> Either String Int
lockstepFrom fw (u0, ram0) accs n = go 0 0 updCorePowerOn { ucRegs = u0 } ram0 Nothing (specOf u0 ram0) accs
 where
  prog = S.fwProgram fw
  dat = S.fwData fw
  -- (clock, instructions run, circuit, its RAM, last clock's addresses, spec, accesses)
  go :: Int -> Int -> UpdCore -> U.Vector Word16 -> Maybe ReqAddrs -> S.Upd -> [Maybe SnesAcc] -> Either String Int
  go clk k c ram prev s as
    | k >= n = Right k
    | clk > 4 * n + 100 = Left ("stuck after " <> show k <> " instructions")
    | otherwise =
        let ra = maybe (ReqAddrs 0 0 0 0) id prev
            -- (the SNES's access in a clock with no instruction due: between instructions)
            go3 = clk `mod` 3 == 0
            (acc, as') = case as of
              a : rest | not go3 && ucReady c && ucOwed c == 0 -> (a, rest)
              _ -> (Nothing, as)
            i = UpdIn { uiGo = go3, uiOp = fromIntegral (prog U.! fromIntegral (raPc ra))
                      , uiRo = fromIntegral (dat U.! fromIntegral (raRp ra)), uiMem = fromIntegral (ram U.! fromIntegral (raDp ra))
                      , uiMemK = fromIntegral (ram U.! fromIntegral (raDpK ra)), uiSnes = acc, uiReset = False }
            (c', o) = updCycle c i
            ram' = maybe ram (\(a, v) -> ram U.// [(fromIntegral a, fromIntegral v)]) (uoRam o)
            -- the spec: the instruction, then the access (the circuit's order)
            s1 = if uoStep o then S.step fw s else s
            (b, s2) = specAcc acc s1
            here = specOf (ucRegs c') ram'
            k' = if uoStep o then k + 1 else k
        in if here /= s2
             then Left ("instruction " <> show k' <> " (clock " <> show clk <> "): circuit " <> show (here { S.uRam = U.empty }) <> "\n spec " <> show (s2 { S.uRam = U.empty })
                        <> (if S.uRam here /= S.uRam s2 then "\n RAM differs" else ""))
             else if acc `elem` [Just SReadDr, Just SReadSr] && fromIntegral (uoByte o) /= b
               then Left ("instruction " <> show k' <> ": the SNES read " <> show (uoByte o) <> ", the spec " <> show b)
               else go (clk + 1) k' c' ram' (Just (updReq c)) s2 as'

lockstepTests :: Maybe S.Firmware -> TestTree
lockstepTests dump = testGroup "the uPD77C25 in Clash, against the spec (step 7.16c)" $
  [ loadTest, oocTest ] <>
  [ testCase ("random program " <> show seed <> (if straight then ", straight" else ", with jumps") <> ": 20,000 instructions alike from a random start, the SNES's accesses between") $
      case lockstepFrom (randomFirmware straight seed) (randomStart (seed + 7)) (map accOf (randoms (seed + 99))) 20000 of
        Right _ -> pure ()
        Left e -> assertFailure e
  | seed <- [1 .. 32], let straight = seed <= 16 ]
  <> [ testCase "the user's firmware: 2Fh, 00h and 1Fh's commands, 200,000 instructions alike" $
         case lockstep fw (cycle (Just (SWriteDr 0x2F) : replicate 40 Nothing <> [Just (SWriteDr 0), Just (SWriteDr 0)] <> replicate 40 Nothing
                                 <> [Just SReadSr, Just SReadDr, Just SReadDr] <> replicate 40 Nothing <> [Just (SWriteDr 0x00)] <> replicate 30 Nothing
                                 <> [Just (SWriteDr 0x00), Just (SWriteDr 0x40)] <> replicate 30 Nothing <> [Just (SWriteDr 0x00), Just (SWriteDr 0x40)]
                                 <> replicate 40 Nothing <> [Just SReadDr, Just SReadDr] <> replicate 60 Nothing)) 200000 of
           Right _ -> pure ()
           Left e -> assertFailure e
     | Just fw <- [dump] ]

-- | (step 7.16d) The DSP-1's firmware load ("Snes.Dsp1".'dspClock'): from
-- a model of the ROM port (a request answered two clocks later), every
-- program and data word written as the file has it, then the load done.
loadTest :: TestTree
loadTest = testCase "Snes.Dsp1's load: every program and data word as in the file, then done" $ do
  let file = dspTestFirmware
      byteAt a = fromIntegral (B.index file (fromIntegral (a - dspFwOffset))) :: C.Unsigned 8
      go :: Int -> DspRegs -> [Maybe (C.Unsigned 23)] -> ([(Int, Integer)], [(Int, Integer)]) -> Either String ([(Int, Integer)], [(Int, Integer)])
      go n d reqs (ps, ds)
        | dspLoaded d = Right (reverse ps, reverse ds)
        | n > 100000 = Left "the load never ended"
        | otherwise =
            let ans = case reqs of
                  _ : Just a : _ -> Just (byteAt a)
                  _ -> Nothing
                (d', o) = dspClock d (DspIn False Nothing ans 0 0 0 0)
                ps' = maybe ps (\(a, w) -> (fromIntegral a, toInteger w) : ps) (doProgW o)
                ds' = maybe ds (\(a, w) -> (fromIntegral a, toInteger w) : ds) (doDatW o)
            in go (n + 1) d' (doRomReq o : reqs) (ps', ds')
  case go 0 dspInit [Nothing, Nothing] ([], []) of
    Left e -> assertFailure e
    Right (ps, ds) -> do
      let fw = either error id (S.firmwareOf file)
      ps @?= [ (i, toInteger (S.fwProgram fw U.! i)) | i <- [0 .. 2047] ]
      ds @?= [ (i, toInteger (S.fwData fw U.! i)) | i <- [0 .. 1023] ]

-- | The out-of-context top's inputs: GO, ACC, ACCV, LOAD_EN, LOAD_DATA,
-- LOAD_ADDR, LOAD_WORD, UPD_RESET.
type OocIn = (Bool, C.Unsigned 2, C.Unsigned 8, Bool, Bool, C.Unsigned 11, C.BitVector 24, Bool)

-- | (step 7.16d bring-up) The out-of-context top ("Snes.Upd77c25Ooc") in
-- Clash's simulation, with the inputs the Verilator harness gives it: the
-- firmware through the load port, a reset, 300 clocks, then a round of
-- dsp1.s's (X, Y written, the three answers read); the same inputs in
-- Verilator found Clash's consolidation bug (Snes.Upd77c25.snesAcc).
oocTest :: TestTree
oocTest = testCase "Snes.Upd77c25Ooc in Clash: after the load, a round of dsp1.s's: its three answers" $ do
  let fw = dspTestFirmware
      b i = fromIntegral (B.index fw i) :: Word32
      progW i = b (3 * i) .|. b (3 * i + 1) `shiftL` 8 .|. b (3 * i + 2) `shiftL` 16
      dataW i = b (6144 + 2 * i) .|. b (6144 + 2 * i + 1) `shiftL` 8
      idle = (False, 0, 0, False, False, 0, 0, False) :: OocIn
      loads = [ (False, 0, 0, True, False, fromIntegral i, fromIntegral (progW i), False) | i <- [0 .. 2047] ]
           <> [ (False, 0, 0, True, True, fromIntegral i, fromIntegral (dataW i), False) | i <- [0 .. 1023] ]
      runK k = [ (j `mod` 3 == 0, 0, 0, False, False, 0, 0, False) | j <- [0 .. k - 1 :: Int] ]
      acc a = (False, a, 0, False, False, 0, 0, False)
      wr x = (False, 3, x, False, False, 0, 0, False)
      ins = replicate 4 idle <> loads <> replicate 4 (False, 0, 0, False, False, 0, 0, True)
            <> runK 300
            <> [ wr 0x34 ] <> runK 40 <> [ wr 0x12 ] <> runK 40 <> [ wr 0x0F ] <> runK 40 <> [ wr 0x0F ] <> runK 600
            <> concat (replicate 3 ([ acc 1 ] <> runK 11 <> [ acc 1 ] <> runK 40))
            <> replicate 4 idle
      n = length ins
      sig :: C.NFDataX a => (OocIn -> a) -> C.Signal DomPix a
      sig f = C.fromList (map f ins ++ repeat (f idle))
      outs = C.sampleN (n + 2) (Ooc.topEntity C.clockGen C.resetGen
        (sig (\(g, _, _, _, _, _, _, _) -> g)) (sig (\(_, a, _, _, _, _, _, _) -> a)) (sig (\(_, _, v, _, _, _, _, _) -> v))
        (sig (\(_, _, _, e, _, _, _, _) -> e)) (sig (\(_, _, _, _, d, _, _, _) -> d)) (sig (\(_, _, _, _, _, a, _, _) -> a))
        (sig (\(_, _, _, _, _, _, w, _) -> w)) (sig (\(_, _, _, _, _, _, _, r) -> r)))
  -- (the round: X 1234h, Y 0F0Fh; the answers 2143h, 0224h, 1D3Bh, low byte first)
  filter (/= 0) [ byte | (byte, _) <- drop (n - 163) outs ] @?= [0x43, 0x21, 0x24, 0x02, 0x3B, 0x1D]
