-- | Layer 0 (M6): 'Gg.Spec''s Master System II ('newMasterSystem') on our
-- own SMS regression ROMs (build/sms/asm/, make sms-asm; the verdict
-- convention is gg.inc's, through sms.inc): each runs until its final HALT
-- and must leave 01h at C000h; biosgame.sms with our stand-in BIOS
-- (bios.sms) as the BIOS image. The pads' port bits. FluBBa's VDP test
-- (55 results) and ZEXDOC/ZEXALL-SMS (their first tests; more in the FULL
-- tier) on the Master System II. And the Sega headers ("Sms.Header") of our
-- ROMs, the Game Gear's and the fetched Master System ROMs.
module Main (main) where

import Prelude

import Control.Monad (forM_, unless)
import Data.Bits ((.&.), (.|.), xor)
import Control.Monad.ST (runST)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.List (isInfixOf, isSuffixOf, sort)
import qualified Data.Vector.Unboxed as U
import System.Environment (lookupEnv)
import Data.Word (Word64, Word8)
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath ((</>), takeBaseName, takeExtension)
import Test.Tasty
import Test.Tasty.HUnit
import Text.Printf (printf)

import FmOracle (oracleTests)
import Gg.Input (inputAt, parseInput)
import Gg.Screen (frameHash, frameSmsVgaPng)
import Gg.Spec
import Sms.Spec.FmUnit (newFmUnit)
import Gg.Text (screenText)
import Sms.Header
import Z80.Spec (Regs (..), Step (..), getRegs)

-- | Relative to the package directory, where cabal runs the tests.
asmDir, ggAsmDir, romDir :: FilePath
asmDir = "../../build/sms/asm"
ggAsmDir = "../../build/gg/asm"
romDir = "../../roms/sms"

-- | The fetched Master System ROMs (tools/fetch-roms.sh), all export.
fetched :: [FilePath]
fetched = map (romDir </>)
  [ "games/raposadosol_v20.sms", "games/waternet.sms", "zexall/zexall.sms", "zexall/zexdoc.sms" ]

main :: IO ()
main = do
  ok <- doesDirectoryExist asmDir
  unless ok (fail (asmDir <> " missing: run make sms-asm"))
  -- bios.sms is the stand-in BIOS, biosgame.sms its cartridge
  -- bios.sms is the stand-in BIOS, biosgame.sms its cartridge, fm.sms
  -- needs the FM unit (fmTests)
  roms <- filter ((`notElem` ["bios", "biosgame", "fm", "fmtune"]) . takeBaseName) <$> romsIn ".sms" asmDir
  ggRoms <- romsIn ".gg" ggAsmDir
  full <- (== Just "1") <$> lookupEnv "EMU_FULL_TESTS"
  defaultMain $ testGroup "sms-spec"
    [ testGroup "regression ROMs" (map (romTest Nothing) roms)
    , biosTests
    , padTests
    , vdpTest
    , zexTests full
    , raposaGolden
    , oracleTests full
    , fmTests
    , pictureGolden
    , headers roms ggRoms
    ]
 where
  romsIn ext d = map (d </>) . sort . filter ((== ext) . takeExtension) <$> listDirectory d

-- | Runs a ROM on the Master System II, with a BIOS image or none, until
-- DI / HALT (at most 10 frames); the verdict at C000h and whether it
-- halted.
runRom :: Maybe BS.ByteString -> BS.ByteString -> (Word8, Bool)
runRom bios rom = runST $ do
  m <- newMasterSystem bios (detectMapper rom) rom
  let limit = 10 * tPerFrame
      go = do
        s <- stepMachine m
        r <- getRegs (mCpu m)
        t <- machineTStates m
        if s == Halted || (rHalted r && not (rIFF1 r))
          then pure True
          else if t > limit then pure False else go
  halted <- go
  v <- peek m 0xC000
  pure (v, halted)

romTest :: Maybe FilePath -> FilePath -> TestTree
romTest bios f = testCase (takeBaseName f) $ do
  rom <- BS.readFile f
  b <- traverse BS.readFile bios
  let (v, halted) = runRom b rom
  assertBool "no final HALT within 10 frames" halted
  unless (v == 1) $
    assertFailure (if v >= 0x80 then "check " <> show (v - 0x80) <> " failed" else printf "verdict %02Xh" v)

-- | Our stand-in BIOS (bios.s) hands over to biosgame.sms as the real one
-- would; without a BIOS the same cartridge must see that none ran.
biosTests :: TestTree
biosTests = testGroup "BIOS"
  [ romTest (Just (asmDir </> "bios.sms")) (asmDir </> "biosgame.sms")
  , testCase "biosgame without a BIOS: check 2 fails (no BIOS ran)" $ do
      rom <- BS.readFile (asmDir </> "biosgame.sms")
      runRom Nothing rom @?= (0x82, True)
  ]

-- | The pads' bits on ports DCh and DDh (MacDonald: DCh bits 0-5 pad 1, 6-7
-- pad 2's up and down; DDh bits 0-3 pad 2's left, right, 1, 2; pressed =
-- 0), on the Master System II and, with pad 2 ignored, on a Game Gear in
-- its Master System mode: a ROM that reads both ports into C000h-C001h.
padTests :: TestTree
padTests = testGroup "pads"
  [ testCase "Master System II" $ do
      readPads True (Input 0 False 0) @?= (0xFF, 0xFF)
      readPads True (Input (padUp .|. pad2) False 0) @?= (0xDE, 0xFF)
      readPads True (Input 0 False padUp) @?= (0xBF, 0xFF)
      readPads True (Input 0 False padDown) @?= (0x7F, 0xFF)
      readPads True (Input 0 False (padLeft .|. pad2)) @?= (0xFF, 0xF6)
      readPads True (Input 0 False padRight) @?= (0xFF, 0xFD)
      readPads True (Input 0 False pad1) @?= (0xFF, 0xFB)
  , testCase "a Game Gear in Master System mode: no pad 2" $
      readPads False (Input pad1 False 0xFF) @?= (0xEF, 0xFF)
  ]
 where
  -- IN A,(DCh); LD (C000h),A; IN A,(DDh); LD (C001h),A; DI; HALT
  prog = BS.pack [0xDB, 0xDC, 0x32, 0x00, 0xC0, 0xDB, 0xDD, 0x32, 0x01, 0xC0, 0xF3, 0x76] <> BS.replicate (0x8000 - 12) 0
  readPads sms2 inp = runST $ do
    m <- if sms2 then newMasterSystem Nothing SegaMapper prog else newMachineWith True SegaMapper prog
    setInput m inp
    let go = stepMachine m >>= \st -> getRegs (mCpu m) >>= \r -> unless (st == Halted || rHalted r) go
    go
    (,) <$> peek m 0xC000 <*> peek m 0xC001

-- | FluBBa's SMS VDP Test 1.31 (no formal licence, fetched, never
-- committed) on the Master System II, as gg-spec runs it on the Game Gear's
-- Master System mode (the same input script): its three result pages must
-- say "Ok." for all 55 tests, and the X-scroll latch column (page 4, x
-- 16-23) be unbroken on lines 16 to 191.
vdpTest :: TestTree
vdpTest = testCase "FluBBa's SMS VDP Test: all 55 results Ok., the X-scroll latch column straight" $ do
  rom <- BS.readFile "../../roms/gg/vdptest/VDPTEST.sms"
  script <- either fail pure . parseInput =<< readFile "../gg/test/input/vdptest.txt"
  let pages = [1199, 1449, 1699]
      columnFrame = 1949 :: Int
      (results, column) = runST $ do
        m <- newMasterSystem Nothing (detectMapper rom) rom
        let go k acc
              | k > columnFrame = (,) acc <$> frameBuffer m
              | otherwise = do
                  setInput m (inputAt script k)
                  runFrame m
                  t <- if k `elem` pages then screenText m else pure []
                  go (k + 1) (acc <> t)
        go (0 :: Int) []
      bgColour = column U.! (100 * 256 + 100)
      broken = [ y | y <- [16 .. 191 :: Int], all (\x -> column U.! (y * 256 + x) == bgColour) [16 .. 23] ]
      lines' = filter (\l -> any (`isInfixOf` l) ["Ok.", "Error."]) results
      bad = filter (not . ("Ok." `isInfixOf`)) lines'
  assertBool ("failed:\n" <> unlines bad) (null bad)
  length lines' @?= 55
  assertBool ("X-scroll column broken on lines " <> show broken) (null broken)

-- | ZEXDOC and ZEXALL for the Master System (v0.21, GPL-2.0, fetched) on
-- the Master System II, read from the SDSC debug console: every test that
-- reports within the time must say "OK", and at least so many must. The
-- whole run takes hours (its longest test "well over an hour" on the
-- hardware; the spec runs at about 0.8 times real time), so the quick tier
-- runs 30 s of the machine's time (the first 22 tests) and the FULL tier 6
-- minutes. The Z80 itself passes the complete CP/M originals (z80-zex,
-- FULL tier); this checks the Master System around it.
zexTests :: Bool -> TestTree
zexTests full = testGroup "ZEXDOC/ZEXALL-SMS"
  [ zexTest "zexdoc", zexTest "zexall" ]
 where
  frames = if full then 60 * 360 else 60 * 30
  atLeast = if full then 30 else 22 :: Int
  zexTest name = testCase (name <> printf " (%d s of machine time)" (frames `div` 60)) $ do
    rom <- BS.readFile (romDir </> "zexall" </> name <> ".sms")
    let out = runST $ do
          m <- newMasterSystem Nothing (detectMapper rom) rom
          let go k acc
                | k >= frames = pure acc
                | otherwise = runFrame m >> takeConsole m >>= \c -> go (k + 1) (acc <> c)
          go (0 :: Int) []
        ls = lines (map (toEnum . fromIntegral) out)
        results = filter (\l -> " OK" `isSuffixOf` l || "expected" `isInfixOf` l || "CRC" `isInfixOf` l) ls
        bad = filter (not . (" OK" `isSuffixOf`)) results
    assertBool ("failed:\n" <> unlines bad) (null bad)
    assertBool (printf "only %d tests reported OK (want %d)" (length results) atLeast) (length results >= atLeast)

-- | Raposa do Sol (the exit game, MIT) on the Master System II, played
-- with test/input/raposa-start.txt: the frame hashes ("Gg.Screen.frameHash")
-- and border colours of the title (419), the mode menu (520), the ships
-- screen (780) and the game (899: its backdrop is white, so the border
-- and the blanked left column are), checked by eye and frozen
-- (test/golden/raposadosol.txt, 2026-10-11).
raposaGolden :: TestTree
raposaGolden = testCase "Raposa do Sol golden (title, mode menu, in game)" $ do
  rom <- BS.readFile (romDir </> "games/raposadosol_v20.sms")
  script <- either fail pure . parseInput =<< readFile "test/input/raposa-start.txt"
  want <- map words . lines <$> readFile "test/golden/raposadosol.txt"
  let frames = [ read k | (k : _) <- want ] :: [Int]
      got = runST $ do
        m <- newMasterSystem Nothing (detectMapper rom) rom
        let go k acc
              | k > maximum frames = pure (reverse acc)
              | otherwise = do
                  setInput m (inputAt script k)
                  runFrame m
                  if k `elem` frames
                    then do
                      h <- frameHash <$> frameBuffer m
                      b <- frameBorder m
                      go (k + 1) ([show k, printf "%016x" h, printf "%03x" b] : acc)
                    else go (k + 1) acc
        go (0 :: Int) []
  got @?= want

-- | picture.sms as the board shows it: the spec's 640 x 480 picture
-- ("Gg.Screen.frameSmsVgaPng": x2 at (64, 48), the border) equals the
-- frozen golden (test/golden/vga/picture.png, checked by eye 2026-10-11),
-- which layer 4 compares the hardware with.
pictureGolden :: TestTree
pictureGolden = testCase "picture.sms: the board's 640 x 480 picture = the golden" $ do
  rom <- BS.readFile (asmDir </> "picture.sms")
  want <- BL.readFile "test/golden/vga/picture.png"
  let got = runST $ do
        m <- newMasterSystem Nothing (detectMapper rom) rom
        mapM_ (const (runFrame m)) [0 .. 5 :: Int]
        frameSmsVgaPng <$> frameBorder m <*> frameBuffer m
  assertBool "picture.sms differs from test/golden/vga/picture.png" (got == want)

-- | The FM unit in the machine (M6 step 6.3): fm.sms finds it the way games
-- do and plays A4 on it (436.97 Hz: F-number 120h, block 4); without the
-- unit its detection fails (check 1). With the unit fitted but left off
-- (F2h at power-on: the PSG only) our PSG ROMs and Raposa do Sol sound and
-- look as without it: same verdicts, same audio.
fmTests :: TestTree
fmTests = testGroup "FM unit"
  [ testCase "fm.sms finds the FM unit and plays A4 at 436.97 Hz" $ do
      rom <- BS.readFile (asmDir </> "fm.sms")
      let (v, audio) = runST $ do
            fu <- newFmUnit
            m <- newMasterSystemFm Nothing (Just fu) (detectMapper rom) rom
            mapM_ (const (runFrame m)) [1 .. 75 :: Int]
            a <- takeAudio m
            (,) <$> peek m 0xC000 <*> pure a
          hz = pitch (map (fromIntegral . fst) (drop 24000 audio))
      v @?= 0x01
      assertBool (printf "pitch %.2f Hz, want 436.97" hz) (abs (hz - 436.97) < 0.2)
  , testCase "fmtune.sms: the audio heard by the user (2026-10-11), frozen" $ do
      rom <- BS.readFile (asmDir </> "fmtune.sms")
      want <- takeWhile (/= '\n') <$> readFile "test/golden/fmtune.txt"
      let audio = runST $ do
            fu <- newFmUnit
            m <- newMasterSystemFm Nothing (Just fu) (detectMapper rom) rom
            mapM_ (const (runFrame m)) [1 .. 1200 :: Int]
            takeAudio m
          fnv = foldl (\h (l, r) -> ((h `xor` (fromIntegral l .&. 0xFFFF)) * 0x100000001b3 `xor` (fromIntegral r .&. 0xFFFF)) * 0x100000001b3)
                  (0xcbf29ce484222325 :: Word64) audio
      printf "%016x" fnv @?= want
  , testCase "without the FM unit: check 1 fails" $ do
      rom <- BS.readFile (asmDir </> "fm.sms")
      fst (runRom Nothing rom) @?= 0x81
  , testCase "the unit fitted but off: Raposa do Sol's audio and frames unchanged" $ do
      rom <- BS.readFile (romDir </> "games/raposadosol_v20.sms")
      let run withFm = runST $ do
            fu <- if withFm then Just <$> newFmUnit else pure Nothing
            m <- newMasterSystemFm Nothing fu (detectMapper rom) rom
            mapM (\_ -> runFrame m >> ((,) <$> (frameHash <$> frameBuffer m) <*> takeAudio m)) [1 .. 300 :: Int]
      assertBool "differs with the FM unit fitted" (run True == run False)
  ]

-- | A tone's pitch (Hz) from 48 kHz samples: the autocorrelation's peak
-- between 300 and 600 Hz, refined by a parabola, over 0.5 s.
pitch :: [Double] -> Double
pitch xs0 = fromIntegral audioRate / (fromIntegral best + off)
 where
  xs = take 24000 xs0
  mu = sum xs / fromIntegral (length xs)
  v = U.fromList (map (subtract mu) xs) :: U.Vector Double
  n = U.length v
  ac l = U.sum (U.zipWith (*) (U.take (n - l) v) (U.drop l v))
  lags = [audioRate `div` 600 .. audioRate `div` 300]
  best = snd (maximum [ (ac l, l) | l <- lags ])
  (a, b, c) = (ac (best - 1), ac best, ac (best + 1))
  off = 0.5 * (a - c) / (a - 2 * b + c)

headers :: [FilePath] -> [FilePath] -> TestTree
headers roms ggRoms = testGroup "headers"
  [ testCase "our SMS ROMs: region 4 (SMS export), 64 KiB, WLA-DX's checksum" $
      -- (and biosgame.sms; bios.sms has no header, as a BIOS needs none)
      forM_ roms $ \f -> expect f SmsExport (Just 0xE)
  , testCase "the Game Gear's ROMs: region 7, unchanged by sms.inc" $
      forM_ (filter ((/= "codemasters") . takeBaseName) ggRoms) $ \f -> expect f GgInternational (Just 0xE)
  , testCase "the fetched ROMs: SMS export, checksums as devkitSMS, GBDK and ZEXALL wrote them" $
      forM_ fetched $ \f -> expect f SmsExport Nothing
  , testCase "checksum: the header's bytes left out, a short ROM sums what it has" $ do
      let blank = BS.replicate 0x8000 0
          withHeader = BS.take 0x7FF0 blank <> BS.replicate 16 0xFF
      checksum 0x8000 withHeader @?= 0
      checksum 0x8000 (BS.pack [1, 2, 3]) @?= 6
      checksum 0x10000 (blank <> BS.replicate 0x8000 1) @?= 0x8000
      checksum 0x2000 (BS.replicate 0x2000 1) @?= 0x1FF0   -- 8 KiB: its header at 1FF0h left out
  , testCase "size codes" $ do
      sizeOfCode 0xC @?= Just 0x8000
      sizeOfCode 0x0 @?= Just 0x40000
      sizeOfCode 0x3 @?= Nothing
  ]
 where
  expect f region size = do
    rom <- BS.readFile f
    case findHeader rom of
      Nothing -> assertFailure (f <> ": no TMR SEGA header")
      Just h -> do
        assertEqual (f <> ": region") region (hRegion h)
        forM_ size $ \c -> assertEqual (f <> ": size code") c (hSizeCode h)
        assertBool (f <> printf ": checksum %04Xh, computed %s" (hChecksum h)
                      (show (fmap (\n -> checksum n rom) (sizeOfCode (hSizeCode h)))))
          (checksumOk rom h)
