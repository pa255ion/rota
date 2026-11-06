-- | Step 4.1: the machine without video. AltirraOS boots into Altirra
-- BASIC; executables load through the loader page; our banking programs;
-- the Acid800 tests the model passes so far (a ratchet: each later step
-- adds its own).
module A800.SpecMachine (machineTests) where

import Prelude

import qualified Data.ByteString as BS
import Data.List (isInfixOf)
import System.FilePath ((</>))
import Test.Tasty
import Test.Tasty.HUnit

import A800.Acid800
import A800.Media (Cas (..), TapeRec (..), parseCart, parseCas, parseTape)
import A800.Spec
import A800.TestRoms

-- | (@fullTier@: also the tapes played by the recorder itself, minutes each.)
machineTests :: Bool -> TestTree
machineTests fullTier = testGroup "machine (4.1)"
  [ testCase "AltirraOS boots into Altirra BASIC: banner and Ready" $ do
      (os, basic) <- system
      let r = runScreen ("Ready" `isInfixOf`) 20000000 (stockConfig os basic Nothing)
      assertBool ("no Ready after " <> show (srCycles r) <> " cycles:\n" <> unlines (srLines r)) (srDone r)
      take 2 (srLines r) @?= ["  Altirra 8K BASIC 1.59", "  Ready"]
  , testGroup "our programs" $
      [ program "pass.xex", program "banking.xex", program "cart_pass.rom", program "cart_banking.rom", program "xegs_banks.car" ]
  , testCase "cas_boot.cas: booted from the cassette (START held, then a key), every record in" $ do
      (os, basic) <- system
      c <- loadFile (asmRoot </> "cas_boot.cas")
      cas <- either (\e -> assertFailure e >> fail e) pure (parseCas c)
      length (casRecords cas) @?= 5
      let (v, r) = runCasBoot 40000000 (casConfig os basic cas)
      assertEqual ("verdict after " <> show (srCycles r) <> " cycles; screen:\n" <> unlines (srLines r)) Passed v
    , testCase "cas_boot.cas with a record's checksum broken: no Pass (the OS sees the read fail)" $ do
      (os, basic) <- system
      c <- loadFile (asmRoot </> "cas_boot.cas")
      Cas recs <- either (\e -> assertFailure e >> fail e) pure (parseCas c)
      let broken = case recs of
            r0 : r1 : rest -> r0 : (BS.init r1 `BS.snoc` (BS.last r1 + 1)) : rest
            _ -> recs
          (v, _) = runCasBoot 20000000 (casConfig os basic (Cas broken))
      assertBool "booted although a record was bad" (v /= Passed)
    , testCase "cas_irq.s: 100 records through SIOV with a DLI on every line, each done and in order" $ do
      (os, basic) <- system
      c <- loadFile (asmRoot </> "cas_irq.cas")
      cas <- either (\e -> assertFailure e >> fail e) pure (parseCas c)
      let (v, r) = runCasBoot 60000000 (casConfig os basic cas)
      assertEqual ("verdict after " <> show (srCycles r) <> " cycles; screen:\n" <> unlines (srLines r)) Passed v
    , depth2 $ testCase "cas_boot.cas through the 410 recorder itself (depth 2: POKEY's serial input at 600 baud; the leader cut to 10 s: the OS waits about that long after starting the motor)" $ do
      (os, basic) <- system
      c <- loadFile (asmRoot </> "cas_boot.cas")
      tape <- either (\e -> assertFailure e >> fail e) pure (parseTape c)
      map trBaud tape @?= replicate 5 600
      let short = case tape of t0 : rest -> t0 { trGapMs = 10000 } : rest; [] -> []
          (v, r) = runTapeBoot 60000000 (stockConfig os basic Nothing) short
      assertEqual ("verdict after " <> show (srCycles r) <> " cycles; screen:\n" <> unlines (srLines r)) Passed v
    , depth2 $ testCase "depth 2 with a record's checksum broken: no Pass" $ do
      (os, basic) <- system
      c <- loadFile (asmRoot </> "cas_boot.cas")
      tape <- either (\e -> assertFailure e >> fail e) pure (parseTape c)
      let bad = case tape of
            t0 : t1 : rest -> t0 { trGapMs = 10000 } : t1 { trBytes = BS.init (trBytes t1) `BS.snoc` (BS.last (trBytes t1) + 1) } : rest
            _ -> tape
          (v, _) = runTapeBoot 45000000 (stockConfig os basic Nothing) bad
      assertBool "booted although a record was bad" (v /= Passed)
    , testGroup "Acid800" (map acid passing)
  ]

  -- (a test of the full tier only)
 where
  depth2 t = if fullTier then t else testGroup "(depth 2 tapes: full tier, EMU_FULL_TESTS=1)" []

system :: IO (BS.ByteString, BS.ByteString)
system = (,) <$> loadFile osRom <*> loadFile basicRom

-- | Cartridges by their extension, the rest as executables.
configFor :: FilePath -> IO Config
configFor f = do
  (os, basic) <- system
  file <- loadFile f
  if ".rom" `isInfixOf` f || ".car" `isInfixOf` f
    then either assertFailure' (pure . stockConfig os basic . Just) (parseCart f file)
    else do
      l <- loadFile loaderBin
      pure (xexConfig os basic l file)
 where
  assertFailure' e = assertFailure e >> fail e

runsTo :: Verdict -> FilePath -> Assertion
runsTo want f = do
  cfg <- configFor f
  let (v, r) = runVerdict 12000000 cfg
  assertEqual ("verdict after " <> show (srCycles r) <> " cycles; screen:\n" <> unlines (srLines r)) want v

program :: FilePath -> TestTree
program n = testCase n (runsTo Passed (asmRoot </> n))

acid :: String -> TestTree
acid n = testCase n (runsTo Passed (acidFile n))

-- | The Acid800 tests the model passes (a ratchet: a test on this list
-- must keep passing; each step adds what it makes pass). As of step 4.5:
-- all 51 required (antic_blockednmi and antic_dlitiming with the shared
-- 6502's 800XL rules, 'sally6502') and pokey_seroc.
passing :: [String]
passing =
  [ "antic_addresswrap", "antic_addrmirror", "antic_blockednmi", "antic_charcontrol", "antic_default", "antic_dlistwrap", "antic_dlitiming", "antic_dmapattern", "antic_hiresbug", "antic_hscrolbug", "antic_linebuffering", "antic_nmist", "antic_pfstarttiming", "antic_pfstoptiming", "antic_pmdma", "antic_vcount", "antic_virtdma", "antic_vscroldli", "antic_vscroll", "antic_wsync", "cpu_bugs", "cpu_clisei", "cpu_decimal", "cpu_flags", "cpu_illegal", "cpu_illtiming", "cpu_insn", "cpu_timing", "gtia_addrmirror", "gtia_collision2", "gtia_collision", "gtia_consol", "gtia_default", "gtia_phantomdma", "gtia_pmoverlap", "gtia_pmresize", "gtia_pmretrigger", "gtia_psuedomodee", "gtia_vdelay", "mmu_xlbanking", "pia_basic", "pia_irq", "pokey_addrmirror", "pokey_default", "pokey_inittiming", "pokey_irqtiming", "pokey_noise", "pokey_asyncrecv", "pokey_seroc", "pokey_timergranularity", "pokey_timerirq", "pokey_timertiming", "pokey_twotone" ]
