-- | Layer 0: 'Gg.Spec' on our own regression ROMs (build/gg/asm/, make
-- gg-asm; systems/gg/test/asm/gg.inc has the verdict convention): each
-- runs until its final HALT and must leave 01h at C000h; unit tests of the
-- VDP and the PSG ("Units"); and the Waternet golden: the frame hashes
-- (Gg.Screen.frameHash) of frames checked by eye, played with
-- test/input/waternet-start.txt (frozen 2026-10-07).
module Main (main) where

import Prelude

import Control.Monad (forM_, unless, when)
import Control.Monad.ST (runST)
import qualified Data.ByteString as BS
import Data.List (isInfixOf, sort)
import qualified Data.Vector.Unboxed as U
import Data.Word
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath ((</>), takeBaseName, takeExtension)
import Test.Tasty
import Test.Tasty.HUnit
import Text.Printf (printf)

import Gg.Input
import Gg.Screen (frameHash)
import Gg.Spec
import Gg.Text (screenText)
import qualified Units
import SpecRestore (localGames, restoreTests)
import System.Environment (lookupEnv)
import Z80.Spec (Regs (..), Step (..), getRegs)

-- | Relative to the package directory, where cabal runs the tests.
asmDir :: FilePath
asmDir = "../../build/gg/asm"

main :: IO ()
main = do
  ok <- doesDirectoryExist asmDir
  unless ok (fail (asmDir <> " missing: run make gg-asm"))
  -- raster.gg and stereo.gg never end (a layer 4 picture, a sound for the
  -- board; no verdict): not regression ROMs here
  roms <- map (asmDir </>) . sort . filter (\f -> takeExtension f == ".gg" && takeBaseName f `notElem` ["raster", "stereo"])
            <$> listDirectory asmDir
  local <- lookupEnv "EMU_RESTORE_DIR" >>= localGames
  defaultMain $ testGroup "gg-spec" $ local <>
    [ testGroup "regression ROMs" (map romTest roms)
    , restoreTests
    , unitTests
    , Units.unitTests
    , waternetGolden
    , vdpTest
    , mapperDetection roms
    , stereoTest
    ]

-- | 'detectMapper' (docs/systems/gamegear.md, "Codemasters mapper").
mapperDetection :: [FilePath] -> TestTree
mapperDetection roms = testGroup "mapper detection"
  [ testCase "Waternet and our Sega-mapper ROMs: Sega; codemasters.gg: Codemasters" $ do
      wn <- BS.readFile waternetRom
      detectMapper wn @?= SegaMapper
      found <- mapM (\f -> (,) (takeBaseName f) . detectMapper <$> BS.readFile f) roms
      [ n | (n, Codemasters) <- found ] @?= ["codemasters"]
  , testCase "a Codemasters header alone (checksum + complement = 0, zeros after)" $ do
      detectMapper blank @?= SegaMapper
      detectMapper (patch 0x7FE6 [0x34, 0x12, 0xCC, 0xED] blank) @?= Codemasters
      detectMapper (patch 0x7FE6 [0x34, 0x12, 0xCC, 0xEE] blank) @?= SegaMapper   -- not the complement
      detectMapper (patch 0x7FEA [1] (patch 0x7FE6 [0x34, 0x12, 0xCC, 0xED] blank)) @?= SegaMapper
  , testCase "bank writes: LD (8000h),A against LD (FFFFh),A in the first 32 KiB" $ do
      detectMapper (writes 1 0) @?= Codemasters
      detectMapper (writes 4 2) @?= SegaMapper   -- not more than 2 + 2
      detectMapper (writes 5 2) @?= Codemasters
      detectMapper (writes 0 3) @?= SegaMapper
  ]
 where
  blank = BS.replicate 0x10000 0
  patch at bs b = BS.take at b <> BS.pack bs <> BS.drop (at + length bs) b
  writes n8000 nFFFF = patch 0x100 (concat (replicate n8000 [0x32, 0x00, 0x80] <> replicate nFFFF [0x32, 0xFF, 0xFF])) blank

-- | FluBBa's SMS VDP Test 1.31 (Master System mode; no formal licence,
-- fetched, never committed): its three result pages, read from the name
-- table, must say "Ok." for every test (55; docs/systems/gamegear.md,
-- "VDP timing"); and on its fourth page (the X-scroll latch time) the
-- column at x 16-23 must be unbroken on lines 16 to 191.
vdpTest :: TestTree
vdpTest = testCase "FluBBa's SMS VDP Test: all 55 results Ok., the X-scroll latch column straight" $ do
  rom <- BS.readFile "../../roms/gg/vdptest/VDPTEST.sms"
  script <- either fail pure . parseInput =<< readFile "test/input/vdptest.txt"
  let pages = [1199, 1449, 1699]
      columnFrame = 1949 :: Int
      (results, column) = runST $ do
        m <- newMachineSms rom
        let go k acc
              | k > columnFrame = do
                  fb <- frameBuffer m
                  pure (acc, fb)
              | otherwise = do
                  setInput m (inputAt script k)
                  runFrame m
                  t <- if k `elem` pages then screenText m else pure []
                  go (k + 1) (acc <> t)
        go (0 :: Int) []
      bgColour = column U.! (100 * 256 + 100)
      broken = [ y | y <- [16 .. 191], all (\x -> column U.! (y * 256 + x) == bgColour) [16 .. 23] ]
      verdict l = any (`isInfixOf` l) ["Ok.", "Error."]
      lines' = filter verdict results
      bad = filter (not . ("Ok." `isInfixOf`)) lines'
  assertBool ("failed:\n" <> unlines bad) (null bad)
  length lines' @?= 55
  assertBool ("X-scroll column broken on lines " <> show broken) (null broken)

waternetRom, waternetScript, waternetHashes :: FilePath
waternetRom = "../../roms/gg/games/waternet.gg"
waternetScript = "test/input/waternet-start.txt"
waternetHashes = "test/golden/waternet.txt"

goldenLine :: String -> IO (Int, String)
goldenLine l = case words l of
  [k, h] -> pure (read k, h)
  _ -> fail (waternetHashes <> ": bad line: " <> l)

waternetGolden :: TestTree
waternetGolden = testCase "Waternet golden (menu, mode, level 1, two moves)" $ do
  rom <- BS.readFile waternetRom
  script <- either (fail . (waternetScript <>) . (":" <>)) pure . parseInput =<< readFile waternetScript
  want <- mapM goldenLine . lines =<< readFile waternetHashes
  let lastFrame = maximum (map fst want)
      got = runST $ do
        m <- newMachine rom
        let go k acc
              | k > lastFrame = pure (reverse acc)
              | otherwise = do
                  setInput m (inputAt script k)
                  runFrame m
                  h <- frameHash <$> frameBuffer m
                  go (k + 1) (if k `elem` map fst want then (k, printf "%016x" h) : acc else acc)
        go (0 :: Int) []
  got @?= want

-- | Runs a ROM until DI / HALT (at most 10 frames); the verdict at C000h,
-- whether it halted, and what it wrote to the debug console.
runRom :: BS.ByteString -> (Word8, Bool, [Word8])
runRom rom = runST $ do
  m <- newMachine rom
  let limit = 10 * tPerLine * linesPerFrame
      go = do
        s <- stepMachine m
        r <- getRegs (mCpu m)
        t <- machineTStates m
        if s == Halted || (rHalted r && not (rIFF1 r))
          then pure True
          else if t > limit then pure False else go
  halted <- go
  v <- peek m 0xC000
  c <- takeConsole m
  pure (v, halted, c)

romTest :: FilePath -> TestTree
romTest f = testCase (takeBaseName f) $ do
  rom <- BS.readFile f
  let (v, halted, console) = runRom rom
  assertBool "no final HALT within 10 frames" halted
  when (takeBaseName f == "console") $
    map (toEnum . fromIntegral) console @?= "GG console OK\n"
  unless (v == 1) $
    assertFailure (if v >= 0x80 then "check " <> show (v - 0x80) <> " failed" else printf "verdict %02Xh" v)

unitTests :: TestTree
unitTests = testGroup "units"
  [ testCase "V counter, NTSC 192 lines: 00h-DAh, then D5h-FFh" $ do
      map vCounter [0, 191, 0xDA, 0xDB, 261] @?= [0x00, 0xBF, 0xDA, 0xD5, 0xFF]
      tPerLine * linesPerFrame @?= 59736
  ]

-- | stereo.gg (M5 step 5.10, exit item 3): a tone on channel 0 routed by
-- port 06h through four phases of 120 frames (left, right, both, neither);
-- each side's level (RMS of the 48 kHz audio) in the middle second of each.
stereoTest :: TestTree
stereoTest = testCase "stereo: port 06h routes channel 0 left, right, both, neither (stereo.gg)" $ do
  rom <- BS.readFile (asmDir </> "stereo.gg")
  let audio = U.fromList . concatMap (\(l, r) -> [l, r]) $ runST $ do
        m <- newMachine rom
        let go = do
              _ <- stepMachine m
              t <- machineTStates m
              unless (t > 8 * 3579545) go
        go
        takeAudio m
      level :: Int -> Double -> Double
      level side from = let xs = [ fromIntegral (audio U.! (2 * i + side)) :: Double
                                 | i <- [round (from * 48000) .. round ((from + 1) * 48000) - 1] ]
                        in sqrt (sum (map (^ (2 :: Int)) xs) / fromIntegral (length xs))
      loud x = x > 2000
      quiet x = x < 50
      phases = [ ("left only", 0.5, True, False), ("right only", 2.5, False, True)
               , ("both", 4.5, True, True), ("neither", 6.5, False, False) ]
  forM_ phases $ \(name, from, wantL, wantR) -> do
    let l = level 0 from
        r = level 1 from
    assertBool (name <> ": left " <> show l <> ", right " <> show r)
      ((if wantL then loud l else quiet l) && (if wantR then loud r else quiet r))
