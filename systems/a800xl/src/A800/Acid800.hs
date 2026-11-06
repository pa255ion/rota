{-|
Acid800 1.2 (Avery Lee, MIT; docs/systems/atari800xl.md, "Required
tests"): the hardware tests M4 tracks, frozen in step 4.0, and how a
standalone test reports.

A standalone test (@out/Release/Acid800/standalone/*.xex@) is an XEX run
after the OS has booted. It prints its title and @"..."@ through the
screen editor, runs, then prints @"Pass"@, @"FAIL."@ followed by the
reason, or @"Skipped"@ followed by why, and waits for a key
(@library.s@: @_testPassed@, @_testFailed@, @_testSkipped@, @_testEnd@).
So the verdict is screen text ('verdict', on "A800.Screen".'screenFlat':
the editor wraps lines, so a word can break across rows).

__Required__ ('required'): every test of the CPU, ANTIC, GTIA, the PIA, the
MMU and POKEY's audio, timers and IRQs (51). __Tracked__, not required:
the five tests of POKEY's serial port (SIO is out of M4's scope, PLAN.md);
they run and their result is recorded, but M4's exit doesn't wait for
them. Left out: @cpu_65c816@ and @pokey_serdirect@, which skip
themselves on a stock 800XL without a disk drive, and the @mod_*@ files,
which are helpers of the full suite, not tests.
-}
module A800.Acid800
  ( Group (..)
  , Test (..)
  , tests
  , required
  , tracked
  , Verdict (..)
  , verdict
  ) where

import Prelude

import Data.List (isPrefixOf, tails)

data Group = Cpu | Antic | Gtia | Pokey | PokeySerial | Pia | Mmu
  deriving stock (Show, Eq, Ord, Enum, Bounded)

data Test = Test
  { tName  :: String -- ^ the file name without @.xex@
  , tGroup :: Group
  } deriving stock (Show, Eq)

tests :: [Test]
tests =
  map (`Test` Cpu)
    [ "cpu_insn", "cpu_flags", "cpu_decimal", "cpu_timing", "cpu_bugs", "cpu_clisei"
    , "cpu_illegal", "cpu_illtiming" ]
  <> map (`Test` Antic)
    [ "antic_default", "antic_nmist", "antic_vcount", "antic_wsync", "antic_addresswrap"
    , "antic_dlistwrap", "antic_dlitiming", "antic_addrmirror", "antic_pmdma"
    , "antic_charcontrol", "antic_dmapattern", "antic_blockednmi", "antic_hscrolbug"
    , "antic_virtdma", "antic_vscroll", "antic_vscroldli", "antic_pfstarttiming"
    , "antic_pfstoptiming", "antic_linebuffering", "antic_hiresbug" ]
  <> map (`Test` Gtia)
    [ "gtia_default", "gtia_phantomdma", "gtia_consol", "gtia_vdelay", "gtia_collision"
    , "gtia_collision2", "gtia_pmretrigger", "gtia_pmresize", "gtia_pmoverlap"
    , "gtia_psuedomodee", "gtia_addrmirror" ]
  <> map (`Test` Pokey)
    [ "pokey_default", "pokey_noise", "pokey_irqtiming", "pokey_timerirq"
    , "pokey_timertiming", "pokey_timergranularity", "pokey_twotone", "pokey_inittiming"
    , "pokey_addrmirror" ]
  <> map (`Test` PokeySerial)
    [ "pokey_seroc", "pokey_serclock", "pokey_sertiming", "pokey_skstat", "pokey_asyncrecv" ]
  <> map (`Test` Pia) [ "pia_basic", "pia_irq" ]
  <> [ Test "mmu_xlbanking" Mmu ]

required, tracked :: [Test]
required = filter ((/= PokeySerial) . tGroup) tests
tracked = filter ((== PokeySerial) . tGroup) tests

data Verdict
  = Running           -- ^ no verdict on the screen yet
  | Passed
  | Failed String     -- ^ the rest of the line after "FAIL." and the next line
  | Skipped String
  deriving stock (Show, Eq)

-- | The verdict from the screen's text ("A800.Screen".'screenFlat').
-- The reason is the text after "FAIL." or "Skipped", blanks collapsed,
-- at most 80 characters.
verdict :: String -> Verdict
verdict flat = case firstMarker of
  Nothing -> Running
  Just ("Pass", _) -> Passed
  Just ("FAIL.", rest) -> Failed (reason rest)
  Just (_, rest) -> Skipped (reason rest)
 where
  markers = ["Pass", "FAIL.", "Skipped"]
  firstMarker = case [ (m, drop (length m) t) | t <- tails flat, m <- markers, m `isPrefixOf` t ] of
    x : _ -> Just x
    [] -> Nothing
  reason = take 80 . unwords . words
