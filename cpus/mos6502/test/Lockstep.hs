-- | Layers 1 and 2 (PLAN.md step 3.4): the decoder, then the Clash core
-- against SingleStepTests and in lockstep with the reference model on
-- random programs with random NMI, IRQ and RDY schedules; deliberately
-- broken cores must be caught.
module Main (main) where

import Prelude

import Control.Exception (evaluate)
import Control.Monad (unless)
import Control.Monad.ST (runST)
import Data.Bits
import qualified Data.IntMap.Strict as IM
import Data.List (sort)
import Data.STRef
import qualified Data.Vector.Unboxed as U
import Data.Word
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath ((</>), takeBaseName, takeExtension)
import Test.Tasty
import Test.Tasty.Hedgehog (testProperty)
import Test.Tasty.HUnit

import qualified Clash.Prelude as C
import Mos6502.Core
import Mos6502.Decode
import Mos6502.Harness
import Mos6502.SingleStep
import Mos6502.Spec hiding (Cpu)
import Mos6502.Types

romRoot :: FilePath
romRoot = "../../roms/6502"

main :: IO ()
main = do
  nes <- jsonFiles (romRoot </> "nes6502")
  dec <- jsonFiles (romRoot </> "6502")
  full <- fullTests
  -- Quick tier: the first 500 cases per opcode; FULL=1: all of them.
  let n = if full then Nothing else Just quickCases
  defaultMain $ testGroup "mos6502-lockstep"
    [ decoderTests
    , testGroup "core on SingleStepTests nes6502 (2A03)" (map (coreOpcodeTest n coreSingleStep) nes)
    , testGroup "core on SingleStepTests 6502 (decimal-mode opcodes)" (map (coreOpcodeTest n coreNmos6502) dec)
    , lockstepTests
    , mutantTests
    ]

jsonFiles :: FilePath -> IO [FilePath]
jsonFiles d = do
  ok <- doesDirectoryExist d
  unless ok (fail (d <> " missing: run tools/fetch-roms.sh"))
  map (d </>) . sort . filter ((== ".json") . takeExtension) <$> listDirectory d

-- | The 2A03 as SingleStepTests generated it (LXA with $EE).
coreSingleStep :: CoreConfig
coreSingleStep = coreRicoh2a03 { ccLxaConst = 0xEE }

-- * Decoder

jams :: [Int]
jams = [0x02, 0x12, 0x22, 0x32, 0x42, 0x52, 0x62, 0x72, 0x92, 0xB2, 0xD2, 0xF2]

-- | The 151 documented opcodes.
official :: [Int]
official =
  [ 0x00, 0x01, 0x05, 0x06, 0x08, 0x09, 0x0A, 0x0D, 0x0E, 0x10, 0x11, 0x15, 0x16, 0x18, 0x19, 0x1D, 0x1E
  , 0x20, 0x21, 0x24, 0x25, 0x26, 0x28, 0x29, 0x2A, 0x2C, 0x2D, 0x2E, 0x30, 0x31, 0x35, 0x36, 0x38, 0x39, 0x3D, 0x3E
  , 0x40, 0x41, 0x45, 0x46, 0x48, 0x49, 0x4A, 0x4C, 0x4D, 0x4E, 0x50, 0x51, 0x55, 0x56, 0x58, 0x59, 0x5D, 0x5E
  , 0x60, 0x61, 0x65, 0x66, 0x68, 0x69, 0x6A, 0x6C, 0x6D, 0x6E, 0x70, 0x71, 0x75, 0x76, 0x78, 0x79, 0x7D, 0x7E
  , 0x81, 0x84, 0x85, 0x86, 0x88, 0x8A, 0x8C, 0x8D, 0x8E, 0x90, 0x91, 0x94, 0x95, 0x96, 0x98, 0x99, 0x9A, 0x9D
  , 0xA0, 0xA1, 0xA2, 0xA4, 0xA5, 0xA6, 0xA8, 0xA9, 0xAA, 0xAC, 0xAD, 0xAE, 0xB0, 0xB1, 0xB4, 0xB5, 0xB6, 0xB8, 0xB9, 0xBA, 0xBC, 0xBD, 0xBE
  , 0xC0, 0xC1, 0xC4, 0xC5, 0xC6, 0xC8, 0xC9, 0xCA, 0xCC, 0xCD, 0xCE, 0xD0, 0xD1, 0xD5, 0xD6, 0xD8, 0xD9, 0xDD, 0xDE
  , 0xE0, 0xE1, 0xE4, 0xE5, 0xE6, 0xE8, 0xE9, 0xEA, 0xEC, 0xED, 0xEE, 0xF0, 0xF1, 0xF5, 0xF6, 0xF8, 0xF9, 0xFD, 0xFE ]

undocumentedOps :: [Op]
undocumentedOps = [SLO, RLA, SRE, RRA, SAX, LAX, DCP, ISC, ANC, ALR, ARR, ANE, LXA, AXS, SHA, SHX, SHY, TAS, LAS, JAM]

decoderTests :: TestTree
decoderTests = testGroup "decoder"
  [ testCase "total: every opcode decodes (all fields defined)" $ do
      n <- evaluate (length [ () | op <- [0 .. 255 :: Int], let i = decode (fromIntegral op), iOp i == iOp i, iMode i == iMode i, iKind i == iKind i ])
      n @?= 256
  , testCase "the 12 JAMs and nothing else" $
      [ op | op <- [0 .. 255], iOp (decode (fromIntegral op)) == JAM ] @?= jams
  , testCase "151 documented opcodes, none decoded as an undocumented operation" $ do
      length official @?= 151
      [ op | op <- official, iOp (decode (fromIntegral op)) `elem` undocumentedOps ] @?= []
  , testCase "the other 105 are undocumented operations, NOPs or SBC $EB" $
      [ op | op <- [0 .. 255], op `notElem` official
           , let i = decode (fromIntegral op), iOp i `notElem` (NOP : undocumentedOps), op /= 0xEB ] @?= []
  , testCase "stores write, read-modify-writes are RMW" $ do
      [ op | op <- [0 .. 255 :: Int], let i = decode (fromIntegral op)
           , iOp i `elem` [STA, STX, STY, SAX, SHA, SHX, SHY, TAS], iKind i /= KWrite ] @?= []
      [ op | op <- [0 .. 255 :: Int], let i = decode (fromIntegral op)
           , iOp i `elem` [SLO, RLA, SRE, RRA, DCP, ISC] || (iOp i `elem` [ASL, LSR, ROL, ROR, INC, DEC] && iMode i /= Imp)
           , iKind i /= KRmw ] @?= []
  ]

-- * SingleStepTests on the core

coreOpcodeTest :: Maybe Int -> CoreConfig -> FilePath -> TestTree
coreOpcodeTest n cfg f = testCase (takeBaseName f) $ do
  cases <- loadCasesUpTo n f
  let failures = [ (caseName tc, es) | tc <- cases, let es = coreCase cfg tc, not (null es) ]
  nFail <- evaluate (length failures)
  unless (nFail == 0) $
    assertFailure (show nFail <> " of " <> show (length cases) <> " cases fail; first:\n"
                   <> unlines [ name <> ": " <> unlines es | (name, es) <- take 3 failures ])

-- | One case on the core: same checks as 'runCase' does for the spec.
coreCase :: CoreConfig -> Case -> [String]
coreCase cfg tc = runST $ do
  let ini = caseInitial tc
      fin = caseFinal tc
      want = length (caseCycles tc)
  ram <- newSTRef (IM.fromList [(fromIntegral a, v) | (a, v) <- csRam ini])
  let go cpu k acc
        | k >= want = pure (cpu, reverse acc)
        | otherwise = do
            let req = cpuRequest cpu
                a = fromIntegral (rqAddr req) :: Int
            (v, rec) <- if rqWrite req
              then do
                let d = fromIntegral (rqData req)
                modifySTRef' ram (IM.insert a d)
                pure (0, Access (fromIntegral a) d True)
              else do
                m <- readSTRef ram
                let d = IM.findWithDefault 0 a m
                pure (d, Access (fromIntegral a) d False)
            let (cpu', _) = cpuCycle cfg cpu (CpuIn (fromIntegral v) False False False)
            go cpu' (k + 1) (rec : acc)
      start = cpuPowerOn (CpuRegs (u (csA ini)) (u (csX ini)) (u (csY ini)) (u (csS ini)) (u (csP ini)) (fromIntegral (csPC ini)))
  (cpu, got) <- go start 0 []
  m <- readSTRef ram
  let r = cpuRegs cpu
      regs = [ (nm, g, w) | (nm, g, w) <-
                 [ ("PC", fromIntegral (crPc r), fromIntegral (csPC fin) :: Int)
                 , ("S", fromIntegral (crS r), fromIntegral (csS fin))
                 , ("A", fromIntegral (crA r), fromIntegral (csA fin))
                 , ("X", fromIntegral (crX r), fromIntegral (csX fin))
                 , ("Y", fromIntegral (crY r), fromIntegral (csY fin))
                 , ("P", fromIntegral (crP r), fromIntegral (csP fin)) ]
             , g /= w ]
      ramErrs = [ "RAM " <> show a <> " = " <> show g <> ", expected " <> show v
                | (a, v) <- csRam fin, let g = IM.findWithDefault 0 (fromIntegral a) m, g /= v ]
      busErr = [ "cycles: got " <> show got <> ", expected " <> show (caseCycles tc) | got /= caseCycles tc ]
      -- The instruction must end exactly after its cycles (JAM never ends).
      endErr = [ "core still in the instruction after " <> show want <> " cycles"
               | cPhase cpu `notElem` [PFetch, PSeq, PJam] ]
  pure ([ nm <> " = " <> show g <> ", expected " <> show w | (nm, g, w) <- regs ] <> ramErrs <> busErr <> endErr)
 where
  u :: Word8 -> C.Unsigned 8
  u = fromIntegral

-- * Lockstep on random programs

-- | SplitMix64, for big random memories from one seed.
splitmix :: Word64 -> (Word64, Word64)
splitmix s =
  let s' = s + 0x9E3779B97F4A7C15
      z1 = (s' `xor` (s' `shiftR` 30)) * 0xBF58476D1CE4E5B9
      z2 = (z1 `xor` (z1 `shiftR` 27)) * 0x94D049BB133111EB
  in (s', z2 `xor` (z2 `shiftR` 31))

randomBytes :: Word64 -> Int -> [Word8]
randomBytes seed n = take n (go seed)
 where
  go s = let (s', r) = splitmix s in [fromIntegral (r `shiftR` (8 * i)) | i <- [0 .. 7]] <> go s'

-- | A random machine: random memory (JAMs turned into NOPs unless
-- allowed), vectors into memory, and line schedules made of random
-- windows.
data Scenario = Scenario
  { scSeed :: Word64
  , scJams :: Bool
  , scNmi  :: [(Int, Int)] -- ^ windows (start, length) where NMI is asserted
  , scIrq  :: [(Int, Int)]
  , scRdy  :: [(Int, Int)] -- ^ windows where RDY is low
  , scRegs :: Regs
  } deriving stock (Show)

scenarioEnv :: Scenario -> Env
scenarioEnv sc = Env mem (inW (scNmi sc)) (inW (scIrq sc)) (not . inW (scRdy sc))
 where
  bytes = randomBytes (scSeed sc) 65536
  fix b = if not (scJams sc) && fromIntegral b `elem` jams then 0xEA else b
  mem = U.fromList (map fix bytes)
  inW ws c = any (\(s, l) -> c >= s && c < s + l) ws

genScenario :: H.Gen Scenario
genScenario = do
  seed <- Gen.word64 Range.constantBounded
  jamsOk <- Gen.bool
  let windows maxLen = Gen.list (Range.linear 0 12) ((,) <$> Gen.int (Range.linear 0 3000) <*> Gen.int (Range.linear 1 maxLen))
  nmi <- windows 400
  irq <- windows 600
  rdy <- windows 20
  regs <- Regs <$> b <*> b <*> b <*> b <*> b <*> Gen.word16 Range.constantBounded
  pure (Scenario seed jamsOk nmi irq rdy regs)
 where
  b = Gen.word8 Range.constantBounded

lockstepCount :: Int
lockstepCount = 600

lockstepTests :: TestTree
lockstepTests = testGroup "lockstep with Mos6502.Spec"
  [ testProperty "random programs with NMI, IRQ and RDY: 600 steps, identical buses and registers (2A03)" $
      H.withTests 400 $ H.property $ do
        sc <- H.forAll genScenario
        let env = scenarioEnv sc
        firstDifference (specSteps ricoh2a03 env (scRegs sc) lockstepCount)
                        (coreSteps (cpuCycle coreRicoh2a03) env (scRegs sc) lockstepCount) H.=== Nothing
  , testProperty "the same with decimal mode (NMOS 6502)" $
      H.withTests 200 $ H.property $ do
        sc <- H.forAll genScenario
        let env = scenarioEnv sc
        firstDifference (specSteps nmos6502 env (scRegs sc) lockstepCount)
                        (coreSteps (cpuCycle coreNmos6502) env (scRegs sc) lockstepCount) H.=== Nothing
  , testProperty "the same with the 800XL's interrupt rules (6502C SALLY)" $
      H.withTests 200 $ H.property $ do
        sc <- H.forAll genScenario
        let env = scenarioEnv sc
        firstDifference (specSteps sally6502 env (scRegs sc) lockstepCount)
                        (coreSteps (cpuCycle coreSally) env (scRegs sc) lockstepCount) H.=== Nothing
  ]

-- * Deliberately broken cores

-- | A broken core must differ from the spec on at least one of these
-- scenarios (fixed seeds, so the result doesn't depend on luck).
caught :: CoreFn -> Bool
caught = caughtBy ricoh2a03

caughtBy :: Config -> CoreFn -> Bool
caughtBy cfg = caughtOn cfg scenarios
 where
  scenarios =
    [ Scenario s False [(100 + i * 37, 200)] [(50 + i * 91, 900)] [(i * 13, 3), (300 + i, 8)]
               (Regs (fromIntegral i) 1 2 0xFD (if odd i then 0x2C else 0x20) (fromIntegral (i * 4099)))
    | (i, s) <- zip [0 :: Int ..] [1, 7, 42, 99, 1234, 5678, 31337, 4242, 777, 2026] ]

-- | For the 800XL's interrupt rules: short NMI pulses, irregularly spaced
-- (some land on an interrupt sequence's P push), an IRQ held throughout
-- (I clear: many sequences), short RDY windows (NMIs during stalls).
sallyScenarios :: [Scenario]
sallyScenarios =
  [ Scenario s False (zip (scanl1 (+) [ 4 + (k * (13 + i)) `mod` 23 | k <- [0 .. 300] ]) (repeat 2)) [(0, 4000)] [ (k * 17 + i, 2) | k <- [0 .. 200] ]
             (Regs 0 1 2 0xFD 0x20 (fromIntegral (i * 4099)))
  | (i, s) <- zip [0 :: Int ..] [3, 11, 58, 404, 9001, 65535] ]

caughtOn :: Config -> [Scenario] -> CoreFn -> Bool
caughtOn cfg scenarios fn = any differs scenarios
 where
  differs sc = let env = scenarioEnv sc
               in firstDifference (specSteps cfg env (scRegs sc) lockstepCount)
                                  (coreSteps fn env (scRegs sc) lockstepCount) /= Nothing

-- | Breaks: the IRQ line not seen at the end of opcode fetches (the poll
-- of every 2-cycle instruction); RDY ignored; ADC #imm decoded as SBC
-- #imm; decimal mode on a 2A03.
mutantTests :: TestTree
mutantTests = testGroup "deliberately broken cores are caught"
  [ testCase "the unbroken core passes these scenarios" $
      caught (cpuCycle coreRicoh2a03) @?= False
  , testCase "IRQ line missed at the end of opcode fetches" $
      caught (\c i -> cpuCycle coreRicoh2a03 c (if cPhase c == PFetch then i { ciIrq = False } else i)) @?= True
  , testCase "RDY ignored (stalled reads taken as real)" $
      caught (\c i -> cpuCycle coreRicoh2a03 c i { ciStall = False }) @?= True
  , testCase "ADC #imm decoded as SBC #imm" $
      caught (\c i -> cpuCycle coreRicoh2a03 c (if cPhase c == PFetch && ciData i == 0x69 then i { ciData = 0xE9 } else i)) @?= True
  , testCase "decimal mode on a 2A03" $ caught (cpuCycle coreNmos6502) @?= True
  , testCase "the unbroken 800XL core passes these scenarios" $ caughtOn sally6502 sallyScenarios (cpuCycle coreSally) @?= False
  , testCase "800XL: stalled cycles don't poll" $
      caughtOn sally6502 sallyScenarios (cpuCycle coreSally { ccStallPolls = False }) @?= True
  , testCase "800XL: an NMI at the P push isn't lost" $
      caughtOn sally6502 sallyScenarios (cpuCycle coreSally { ccPushBlocksNmi = False }) @?= True
  ]

