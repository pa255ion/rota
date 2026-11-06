-- | Layer 2 (M4 step 4.5): the Clash units in lockstep with the reference
-- model, on our programs and Acid800's tests, and with deliberate bugs that
-- the lockstep must catch.
module Main (main) where

import Prelude

import qualified Clash.Prelude as C
import Data.Bits (bit)
import qualified Data.ByteString as BS
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import Test.Tasty
import Test.Tasty.HUnit

import A800.Keyboard (charKeys)
import A800.Antic
import A800.Gtia
import A800.LockAntic
import A800.LockCore
import A800.LockState (stateTests)
import A800.Core
import Mos6502.Core (Req (..), cpuRequest)
import A800.LockGtia
import A800.LockPokey
import A800.Pokey
import A800.Spec (Config (..), casConfig, stockConfig, xexConfig)
import A800.TestRoms
import A800.Media (parseCart, parseCas)

main :: IO ()
main = do
 fullTier <- maybe False (not . null) <$> lookupEnv "EMU_FULL_TESTS"
 let full t = if fullTier then t else testGroup "(full tier: EMU_FULL_TESTS=1)" []
 defaultMain $ testGroup "a800xl-lockstep"
  [ testGroup "POKEY"
      [ pokeyCase "BASIC boots (SIO's serial output, the VBI's timers)" 6000000 [] stock
      , pokeyCase "BASIC typed into (the keyboard scan)" 12000000 typing stock
      , pokeyCase "tone.s (audio)" 3000000 [] (prog "tone.xex")
      ] 
  , testGroup "POKEY on Acid800 (full tier)" [ full (pokeyCase t 4500000 [] (acid t)) | t <- pokeyTests ]
  , testGroup "GTIA"
      [ gtiaCase "BASIC's Ready screen (hires text)" 4000000 stock
      , gtiaCase "scene.s (every mode, players, priority, GTIA mode 9)" 4000000 (prog "scene.xex")
      , gtiaCase "rasterbars.s (colour writes mid-line)" 3000000 (prog "rasterbars.xex")
      ]
  , testGroup "GTIA on Acid800 (full tier)" [ full (gtiaCase t 4500000 (acid t)) | t <- gtiaTests ]
  , testGroup "ANTIC"
      [ anticCase "BASIC's Ready screen" 4000000 stock
      , anticCase "scene.s (every mode, P/M DMA, DLIs, scrolling)" 4000000 (prog "scene.xex")
      ]
  , testGroup "ANTIC on Acid800 (full tier)" [ full (anticCase t 4500000 (acid t)) | t <- anticTests ]
  -- (The suite's heap is capped at 4 GB in a800xl.cabal: a leak fails
  -- the suite instead of filling the shared machine's memory.)
  , testGroup "the core (4.6)"
      [ seqGroup "the core on our programs"
          [ coreCase "scene.s" 3000000 [] (prog "scene.xex")
          , coreCase "rasterbars.s (WSYNC)" 3000000 [] (prog "rasterbars.xex")
          , coreCase "tone.s" 3000000 [] (prog "tone.xex")
          , coreCase "banking.s" 3000000 [] (prog "banking.xex")
          -- cartridges, read through the slow memory (side track Q: a
          -- random latency, as DDR's on the Arty)
          , coreCase "cart_pass.s (8 KiB)" 3000000 [] (cart "cart_pass.rom")
          , coreCase "cart_banking.s (8 KiB, PORTB)" 3000000 [] (cart "cart_banking.rom")
          , coreCase "xegs_banks.s (XEGS 32 KiB, bank switching)" 3000000 [] (cart "xegs_banks.car")
          -- the tape's answer to SIOV (side track Q, cassettes on the
          -- board), its image read through the slow memory
          , coreCase "cas_boot.s (booted from tape)" 6000000 [] (tape "cas_boot.cas")
          , coreCase "cas_irq.s (100 records through SIOV, a DLI on every line)" 9000000 [] (tape "cas_irq.cas")
          , coreCase "cassio.s (an executable reading cas_data.cas through SIOV)" 4500000 [] (xexTape "cassio.xex" "cas_data.cas")
          ]
      , seqGroup "the core on random programs (OS and BASIC images random, JAMs as NOP)"
          [ coreCase ("seed " <> show sd) 150000 [] (pure (randomConfig sd)) | sd <- [1, 2, 3, 4, 5 :: C.Unsigned 64] ]
      , seqGroup "deliberate bugs in the core are caught"
          [ testCase "the CPU ignores WSYNC" $
              caught (lockstepCore bugRdy 3000000 [] <$> prog "rasterbars.xex")
          , testCase "AUDC writes lost" $
              caught (lockstepCore bugAudc 3000000 [] <$> prog "tone.xex")
          ]
      , seqGroup "the core, long runs (full tier)"
          [ full (coreCase "BASIC boots" 6000000 [] stock)
          , full (coreCase "BASIC typed into" 12000000 typing stock) ]
      , seqGroup "the core on Acid800 (full tier)" [ full (coreCase t 4500000 [] (acid t)) | t <- coreTests ]
      ]
  , testGroup "deliberate bugs are caught"
      [ testCase "POKEY: the 64 kHz clock a cycle slow" $
          caught (lockstepPokey bugClock 3000000 [] <$> prog "tone.xex")
      , testCase "POKEY: the keyboard code drops SHIFT" $
          caught (lockstepPokey bugShift 12000000 typing <$> stock)
      , testCase "GTIA: colour writes a colour clock late" $
          caught (lockstepGtia bugColour 3000000 <$> prog "rasterbars.xex")
      , testCase "ANTIC: a DLI's NMI a cycle late" $
          caught (lockstepAntic bugNmi 4000000 <$> prog "scene.xex")
      , testCase "ANTIC: refresh from cycle 24" $
          caught (lockstepAntic bugRefresh 4000000 <$> stock)
      , testCase "ANTIC: CHACTL's inverse bit ignored" $
          caught (lockstepAntic bugInverse 4000000 <$> stock)
      , testCase "GTIA: priority mode 0 mixes P2/P3 with PF0/PF1" $
          caught (lockstepGtia bugPriority 4000000 <$> prog "scene.xex")
      ]
  , stateTests fullTier
  ]
 where
  seqGroup = testGroup
  stock = do
    os <- loadFile osRom
    basic <- loadFile basicRom
    pure (stockConfig os basic Nothing)
  prog f = xex (asmRoot </> f)
  acid t = xex (acidFile t)
  cart f = do
    os <- loadFile osRom
    basic <- loadFile basicRom
    c <- loadFile (asmRoot </> f)
    either (\e -> assertFailure e >> fail e) (pure . stockConfig os basic . Just) (parseCart f c)
  tape f = do
    os <- loadFile osRom
    basic <- loadFile basicRom
    c <- loadFile (asmRoot </> f)
    either (\e -> assertFailure e >> fail e) (pure . casConfig os basic) (parseCas c)
  xexTape f t = do
    cfg <- prog f
    c <- loadFile (asmRoot </> t)
    either (\e -> assertFailure e >> fail e) (\cas -> pure cfg { cfgCas = Just cas }) (parseCas c)
  xex f = do
    os <- loadFile osRom
    basic <- loadFile basicRom
    l <- loadFile loaderBin
    x <- loadFile f
    pure (xexConfig os basic l x)
  pokeyCase name cycles keys mk = testCase name $ do
    cfg <- mk
    case lockstepPokey pokeyStep cycles keys cfg of
      Left e -> assertFailure e
      Right _ -> pure ()
  caught act = act >>= \r -> case r of
    Left _ -> pure ()
    Right n -> assertFailure ("not caught in " <> show n <> " cycles")
  gtiaCase name cycles mk = testCase name $ do
    cfg <- mk
    case lockstepGtia gtiaStep cycles cfg of
      Left e -> assertFailure e
      Right _ -> pure ()
  gtiaTests =
    [ "gtia_default", "gtia_phantomdma", "gtia_consol", "gtia_vdelay", "gtia_collision"
    , "gtia_collision2", "gtia_pmretrigger", "gtia_pmresize", "gtia_pmoverlap"
    , "gtia_psuedomodee", "gtia_addrmirror", "antic_hiresbug", "antic_charcontrol", "antic_pmdma" ]
  anticCase name cycles mk = testCase name $ do
    cfg <- mk
    case lockstepAntic anticStep cycles cfg of
      Left e -> assertFailure e
      Right _ -> pure ()
  coreCase name cycles ks mk = testCase name $ do
    cfg <- mk
    case lockstepCore coreStep cycles ks cfg of
      Left e -> assertFailure e
      Right _ -> pure ()
  coreTests = anticTests <> gtiaTests <> pokeyTests <> ["antic_blockednmi", "antic_dlitiming", "cpu_insn", "cpu_illegal", "cpu_timing", "cpu_decimal", "pia_basic", "pia_irq", "mmu_xlbanking"]
  anticTests =
    [ "antic_default", "antic_nmist", "antic_vcount", "antic_wsync", "antic_addresswrap"
    , "antic_dlistwrap", "antic_addrmirror", "antic_pmdma", "antic_charcontrol", "antic_dmapattern"
    , "antic_hscrolbug", "antic_virtdma", "antic_vscroll", "antic_vscroldli", "antic_pfstarttiming"
    , "antic_pfstoptiming", "antic_linebuffering", "antic_hiresbug" ]
  pokeyTests =
    [ "pokey_default", "pokey_noise", "pokey_irqtiming", "pokey_timerirq", "pokey_timertiming"
    , "pokey_timergranularity", "pokey_twotone", "pokey_inittiming", "pokey_addrmirror", "pokey_seroc" ]

-- | PRINT 2+2, Return, after BASIC's start: (cycle, keys, modifiers).
typing :: [(Int, C.BitVector 64, C.BitVector 3)]
typing = concat
  [ [ (t0 + 6 * i * frame, maybe 0 (\(k, _) -> bit (fromIntegral k)) (charKeys c), maybe 0 (\(_, s) -> if s then 1 else 0) (charKeys c))
    , (t0 + (6 * i + 3) * frame, 0, 0) ]
  | (i, c) <- zip [0 ..] "PRINT \"2+2\"\n" ]
 where
  t0 = 7000000
  frame = 29868

-- | Colour register writes land one colour clock late (2 instead of 1).
bugColour :: GStepFn
bugColour g i = case i of
  GWrite x r v | r >= 0x12 && r < 0x1B ->
    let (g', o) = gtiaStep g i
    in (g' { gQueue = C.map (fmap (\q -> if qReg q == C.resize r then q { qWait = 2 } else q)) (gQueue g') }, o)
  _ -> gtiaStep g i

-- | PRIOR 1 (players over the playfield) taken as 0 (priority mode 0,
-- which mixes players and playfields): a wrong priority rule.
bugPriority :: GStepFn
bugPriority g i = let (g', o) = gtiaStep (g { gPrior = if gPrior g C..&. 15 == 1 then gPrior g C..&. 0xF0 else gPrior g }) i in (g', o)

-- | A machine whose OS (16 KiB) and BASIC (8 KiB) are random bytes (the
-- 12 JAM opcodes made NOPs): random code drives every chip.
randomConfig :: C.Unsigned 64 -> Config
randomConfig sd = stockConfig (BS.pack (take 16384 bytes)) (BS.pack (take 8192 (drop 16384 bytes))) Nothing
 where
  bytes = map fix (go sd)
  go x = let x' = x + 0x9E3779B97F4A7C15
             z1 = (x' `C.xor` (x' `C.shiftR` 30)) * 0xBF58476D1CE4E5B9
             z2 = (z1 `C.xor` (z1 `C.shiftR` 27)) * 0x94D049BB133111EB
             z = z2 `C.xor` (z2 `C.shiftR` 31)
         in [ fromIntegral (z `C.shiftR` (8 * k)) | k <- [0 .. 7 :: Int] ] <> go x'
  fix b = if b `elem` [0x02, 0x12, 0x22, 0x32, 0x42, 0x52, 0x62, 0x72, 0x92, 0xB2, 0xD2, 0xF2] then 0xEA else b

-- | WSYNC's hold not passed to the CPU.
bugRdy :: CStepFn
bugRdy s i = let (s', o) = coreStep s i in if ksSeq s == QRdy then (s' { ksHeld = False }, o) else (s', o)

-- | Writes to AUDC1-4 ($D201, 3, 5, 7) dropped.
bugAudc :: CStepFn
bugAudc s i = case ksSeq s of
  QCpu | rqWrite r && rqAddr r C..&. 0xFF09 == 0xD201 -> (s { ksSeq = QCpuDone, ksAccess = (rqAddr r, rqData r, True, True), ksBus = rqData r }, snd (coreStep s i))
  _ -> coreStep s i
 where r = cpuRequest (ksCpu s)

-- | A DLI's NMI asserted at cycle 9 instead of 8.
bugNmi :: AStepFn
bugNmi a i = case i of
  ABegin _ ->
    let (a', o) = anticStep a i
    in if sX a' == 8 && sNmiLine a' && not (sNmiLine a) && sPending a' == 0x80
         then (a' { sNmiLine = False, sLateNmi = True }, o { aoNmi = False })
         else (a', o)
  _ -> anticStep a i

-- | A refresh asked for at cycle 24 as well.
bugRefresh :: AStepFn
bugRefresh a i = case i of
  ABegin _ | sX a == 23 -> anticStep (a { sRefresh = True }) i
  _ -> anticStep a i

-- | CHACTL bit 1 (inverse video) dropped.
bugInverse :: AStepFn
bugInverse a i = case i of
  AWrite 1 v -> anticStep a (AWrite 1 (v C..&. 0xFD))
  _ -> anticStep a i

-- | The 64 kHz clock with a period of 29 cycles.
bugClock :: StepFn
bugClock p i = case i of
  PTick -> let (p', o) = pokeyStep p i
           in if pPhase64 p' == 28 then (p' { pPhase64 = 29 }, o) else (p', o)
  _ -> pokeyStep p i

-- | KBCODE without bit 6 (SHIFT).
bugShift :: StepFn
bugShift p i = let (p', o) = pokeyStep p i in (p' { pKbcode = pKbcode p' C..&. 0xBF }, o)

_unused :: BS.ByteString -> Config -> ()
_unused _ _ = ()
