-- | The demo ROMs on the reference models (PLAN.md, side track J, layer 3
-- of steps 2 to 4). Needs `make demos` first (build/demos/).
--
-- Game Boy: every frame the spec shows, from the start well into the idle
-- loop, equals the drawing's model ('Rota.Runtime.simulate'), at one fixed
-- frame offset; golden PNGs of the first beat, the crash and an idle
-- frame; the whole machine's audio as a PCM summary golden.
module Main (main) where

import Control.Monad.ST (runST)
import Data.Bits (shiftR, (.&.))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Data.Word (Word8)
import Data.Int (Int16)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import Test.Tasty
import Test.Tasty.Golden (goldenVsFile, goldenVsString)
import Test.Tasty.HUnit

import qualified Chip8.Spec as C
import qualified Gb.Screen as GS
import qualified Nes.Screen as NS
import qualified Nes.Spec.Machine as NM
import qualified Nes.Spec.Ppu as NP
import qualified Gb.Spec as G
import Platform.Sim.Frame (writeFrame)

import Rota.Pic.Chip8 (chip8Code, chip8Program, frameOf)
import Rota.Chip8.Asm (labelAddr)
import Rota.Clock (ckIdle, ckLead, romFrames)
import Rota.Score (endBeat, lastBeat)
import Rota.Logo (letterPixels)
import Rota.Gb (gbClock)
import Rota.Gb.Rom (replay)
import Rota.Nes (nesClock)
import Rota.A800 (a800Clock)
import qualified Rota.A800 as A8
import qualified A800.Spec as AS
import qualified Gg.Spec as GS2
import Rota.Gg (ggClock)
import Rota.Gg.Rom (ggExpected, romFrameCosts, budget)
import Rota.Sms (smsClock, smsW, smsH)
import Rota.Sms.Rom (smsExpected, smsFrameCosts, smsBudget)
import Rota.Snes (snesClock, snesW, snesH, cgram15)
import Rota.Snes.Rom (snesExpected, snesFrameCosts, snesBudget)
import qualified Snes.Spec as SS
import Snes.Cart (parseCart)
import qualified A800.Spec.Machine as AM
import qualified A800.Spec.Antic as AA
import qualified A800.Spec.Gtia as AG
import qualified A800.Picture as AP
import qualified A800.Spec.Pokey as AP2
import qualified Rota.Nes.Rom as NR
import Rota.Sound.Nes (nesFps)
import Data.Word (Word16)
import Rota.Render (pcmSummary)
import Rota.Sound (frameOfBeat)
import Rota.Sound.Gb (gbFps)

main :: IO ()
main = defaultMain $ testGroup "rota-demos" [gbDemo, chip8Demo, nesDemo, a800Demo, ggDemo, smsDemo, snesDemo]

romDir, outDir, goldenDir :: FilePath
romDir = "../../build/demos"
outDir = "../../build/demos/test"
goldenDir = "test/golden"

-- | Frames to compare: the chime and the idle loop's longest part twice.
gbRun :: Int
gbRun = romFrames gbClock + 2 * 120

-- | The screens after each frame the spec finishes, and the ROM's debug
-- counters at the end: frames whose vertical-blank work ran past line 153,
-- and the most lines it took (demos/gb/rota.asm, wOverruns, wMaxLines).
gbRunRom :: BS.ByteString -> Int -> ([U.Vector Word8], (Int, Int, [Int]))
gbRunRom rom n = runST $ do
  em <- G.newMachine rom
  m <- either (error . ("gb: " <>)) pure em
  let go f acc
        | f >= n = do
            o <- G.peek m 0xD0F0
            l <- G.peek m 0xD0F1
            ats <- mapM (G.peek m) [0xD0F2 .. 0xD0F4]
            let fs = reverse acc
                over = [ k | (k, (a, b)) <- zip [0 :: Int ..] (zip (0 : map snd fs) (map snd fs)), b `div` 256 > a `div` 256 ]
                       <> [ 1000 + k | (k, (_, b)) <- zip [0 :: Int ..] fs, b `mod` 256 > 8 ]
            pure (map fst fs, (fromIntegral o, fromIntegral l, take 20 over <> map fromIntegral ats))
        | otherwise = do
            G.step m
            f' <- G.frames m
            if f' > f
              then do
                s <- G.screen m
                o <- G.peek m 0xD0F0
                l <- G.peek m 0xD0F5
                go f' ((shades s, fromIntegral o * 256 + fromIntegral l :: Int) : acc)
              else go f acc
  go 0 []

-- | The GBC model's colours (15-bit) after each frame.
gbcScreens :: BS.ByteString -> Int -> [U.Vector Word16]
gbcScreens rom n = runST $ do
  em <- G.newMachineModel G.Cgb rom
  m <- either (error . ("gbc: " <>)) pure em
  let go f acc
        | f >= n = pure (reverse acc)
        | otherwise = do
            G.step m
            f' <- G.frames m
            if f' > f then G.screenRgb m >>= \s -> go f' (s : acc) else go f acc
  go 0 []

shades :: GS.Screen -> U.Vector Word8
shades s = U.generate (GS.screenWidth * GS.screenHeight) (\i -> let (y, x) = i `divMod` GS.screenWidth in fromIntegral (GS.shadeAt s x y))

-- | Where two screens differ: (x, y, model, spec), the first few.
gbDiff :: U.Vector Word8 -> U.Vector Word8 -> [(Int, Int, Word8, Word8)]
gbDiff a b = take 12 [ (i `mod` 160, i `div` 160, a U.! i, b U.! i) | i <- [0 .. 160 * 144 - 1], a U.! i /= b U.! i ]

gbDemo :: TestTree
gbDemo = withResource (BS.readFile (romDir </> "rota.gb")) (const (pure ())) $ \getRom ->
  testGroup "Game Boy demo"
    [ testCase "every frame's vertical-blank work ends by line 152" $ do
        rom <- getRom
        let (_, (overruns, lns, at)) = gbRunRom rom (gbRun + 8)
        assertEqual ("frames overrunning (at " <> show at <> ")") 0 overruns
        -- line 153 is left alone: the Verilator model lost writes made there
        -- (2026-10-07; the spec accepts them)
        assertBool ("the work reaches line " <> show (144 + lns) <> " (at most 152; frames over, + 1000: " <> show at <> ")") (lns <= 8)
    , testCase "every frame as the model shows it (one offset), into the idle loop" $ do
        rom <- getRom
        let scr = V.fromList (fst (gbRunRom rom (gbRun + 8)))
            ex = V.fromList (replay gbRun)
        -- the spec's shades, frame by frame, for the layer 4 comparison
        -- (demos/compare-frames.py)
        createDirectoryIfMissing True outDir
        BS.writeFile (outDir </> "gb-spec-shades.bin") (BS.pack (concatMap U.toList (V.toList scr)))
        let bad d = [ k | k <- [0 .. gbRun - 1], scr V.! (k + d) /= ex V.! k ]
        case filter (null . bad) [0 .. 4] of
          (_ : _) -> pure ()
          [] -> assertFailure ("no frame offset matches; first differing frames at offsets 0 to 4: " <> show [ take 3 (bad o) | o <- [0 .. 4] ]
                  <> concat [ "\noffset " <> show o <> ", frame " <> show k <> ": (x, y, model, spec) " <> show (gbDiff (ex V.! k) (scr V.! (k + o))) | o <- [2], (k : _) <- [bad o] ])
    , testCase "on the Game Boy Color (DMG mode, side track I's default palette): every frame as the model, one colour per shade" $ do
        rom <- getRom
        let scr = V.fromList (gbcScreens rom (gbRun + 8))
            ex = V.fromList (replay gbRun)
            -- the model's shades onto the GBC's colours: one each, both ways
            fits d = let pairs = [ (ex V.! k U.! i, scr V.! (k + d) U.! i) | k <- [0, 7 .. gbRun - 1], i <- [0, 13 .. 160 * 144 - 1] ]
                         m = M.fromListWith (\a b -> if a == b then a else Nothing) [ (a, Just b) | (a, b) <- pairs ]
                         back = M.fromListWith (\a b -> if a == b then a else Nothing) [ (b, Just a) | (a, b) <- pairs ]
                     in all (/= Nothing) (M.elems m) && all (/= Nothing) (M.elems back)
                        && and [ U.map (\v -> maybe 0 id (m M.! v)) (ex V.! k) == scr V.! (k + d) | k <- [0 .. gbRun - 1] ]
        assertBool "no frame offset in 0 to 8 makes every frame agree" (any fits [0 .. 8])
    , testGroup "golden frames" [ pngGolden getRom n k | (n, k) <- goldenFrames ]
    , goldenVsString "audio (the whole machine)" (goldenDir </> "demo-gb-pcm.txt") $ do
        rom <- getRom
        let cycles = (romFrames gbClock + 30) * 17556
        either (error . ("gb: " <>)) (pure . BLC.pack . pcmSummary . U.fromList . map (\(l, r) -> fromIntegral ((l + r) * 16)))
          (G.audioRun cycles rom)
    ]

goldenFrames :: [(String, Int)]
goldenFrames =
  [ ("beat-1", frameOfBeat gbFps 0.25), ("roll", frameOfBeat gbFps 4.6), ("crash", frameOfBeat gbFps 6.05)
  , ("idle", romFrames gbClock + 60) ]

pngGolden :: IO BS.ByteString -> String -> Int -> TestTree
pngGolden getRom n k =
  goldenVsFile n (goldenDir </> ("demo-gb-" <> n <> ".png")) out $ do
    rom <- getRom
    createDirectoryIfMissing True outDir
    let s = runST $ do
          em <- G.newMachine rom
          m <- either (error . ("gb: " <>)) pure em
          let go f = do
                G.step m
                f' <- G.frames m
                if f' > k then G.screen m else go f
          go (0 :: Int)
    writeFrame out (GS.screenFrame s)
 where
  out = outDir </> ("demo-gb-" <> n <> ".png")

-- * CHIP-8 (step 4): the program on the spec with the Arty's VIP quirks,
-- at both speeds (SW0: about 700 and 10000 instructions a second)

-- | The display after each 60 Hz frame, and the buzzer.
chip8Run :: Int -> Int -> [(U.Vector Bool, Bool)]
chip8Run ipf n = go 0 (C.loadProgram chip8Program C.newMachine)
 where
  go k m
    | k >= n = []
    | otherwise =
        let m' = C.runFrames C.vipQuirks ipf (const 0) 1 m
        in (C.mDisplay m', C.soundOn m') : go (k + 1) m'

chip8Art :: U.Vector Bool -> String
chip8Art d = unlines [ [ if d U.! (y * 64 + x) then '#' else '.' | x <- [0 .. 63] ] | y <- [0 .. 31] ]

chip8Demo :: TestTree
chip8Demo = testGroup "CHIP-8 demo" $
  [ testGroup speed
      [ goldenVsString "frames" (goldenDir </> ("demo-chip8-" <> speed <> ".txt")) $ pure $ BLC.pack $
          concat [ "frame " <> show k <> " (" <> n <> "):\n" <> chip8Art (fst (run !! k)) | (n, k) <- frames ]
      , goldenVsString "buzzer" (goldenDir </> ("demo-chip8-buzzer-" <> speed <> ".txt")) $ pure $ BLC.pack $
          unlines [ show k <> " " <> (if on then "on" else "off") | (k, on) <- edges (map snd run) ]
      ]
  | (speed, ipf) <- [("slow", 12), ("fast", 167)]
  , let run = chip8Run ipf 1500 ]
  <> [ testCase ("the threads drawn as one union, " <> show rate <> " instructions a second at " <> show fps <> " Hz") $
         -- the screen, whenever the program is back at its main loop, is a
         -- picture that depends only on the timeline's place, plus the union
         -- of the bands its table says are drawn (2026-10-08: XOR-drawn
         -- threads left slivers where they crossed)
         case chip8Inconsistent rate fps 1500 of
           [] -> pure ()
           bad -> assertFailure ("the screen isn't the static picture plus the bands' union at (place, (x, y)): " <> show (take 3 bad))
     | (rate, fps) <- [(10020, 60), (10000, 59.94), (700, 60 :: Double)] ]
  <> [ testCase "the word stays whole: rows 10 to 20 beside the disc are the letters, for 2 minutes" $ do
         -- (2026-10-08: the threads, XORed over the letters, scrambled R O
         -- from the left as they flowed; now they pass behind the word)
         let run = chip8Run 167 7200
             word = S.fromList letterPixels
             bad = [ (k, (x, y)) | (k, (d, _)) <- zip [0 :: Int ..] run, k >= frameOf endBeat, k `mod` 10 == 0
                   , y <- [10 .. 20], x <- [0 .. 15] <> [48 .. 63]
                   , d U.! (y * 64 + x) /= S.member (x, y) word ]
         assertBool ("frames where the word isn't the letters (frame, (x, y)): " <> show (take 5 bad)) (null bad) ]
 where
  frames = [ ("first beat", frameOf 0.5), ("spinning out", frameOf 2.5), ("crash", frameOf (lastBeat + 0.2))
           , ("letters", frameOf (lastBeat + 1)), ("idle", frameOf endBeat + 200), ("idle, later", 1499) ]
  edges xs = [ (k, b) | (k, b, prev) <- zip3 [0 :: Int ..] xs (False : xs), b /= prev ]

-- | Places in the timeline where the screen minus the union of the bands
-- (the program's own table: lo, hi per thread at bands + 6 x, the disc's
-- rows left out) differs from what it was the first time at that place.
chip8Inconsistent :: Double -> Double -> Int -> [([Word8], (Int, Int))]
chip8Inconsistent rate fps n = go 0 (0 :: Double) (C.loadProgram chip8Program C.newMachine) M.empty
 where
  addr = labelAddr chip8Code
  mainA = fromIntegral (addr "main")
  place m = [ C.mRam m U.! a | a <- [addr "evPtr", addr "evPtr" + 1, addr "idle", addr "idle" + 2] ]
  r m a = fromIntegral (C.mRam m U.! a) :: Int
  bandsOf m = U.generate 2048 $ \i ->
    let (x, y) = (i `mod` 64, i `div` 64)
        top = r m (addr "hidden" + 2 * x)
        bot = r m (addr "hidden" + 2 * x + 1)
    in not (y >= top && y <= bot)
       && or [ lo /= 255 && y >= lo && y <= hi
             | v <- [0 .. 2], let lo = r m (addr "bands" + 6 * x + 2 * v), let hi = r m (addr "bands" + 6 * x + 2 * v + 1) ]
  steps 0 m seen = (m, seen, [])
  steps k m seen =
    let m' = C.step C.vipQuirks 0 m
        (seen', bad) = if C.mPc m' /= mainA then (seen, []) else
          let res = U.zipWith (/=) (C.mDisplay m') (bandsOf m')
              key = place m'
          in case M.lookup key seen of
               Nothing -> (M.insert key res seen, [])
               Just old -> (seen, [ (key, (i `mod` 64, i `div` 64)) | i <- take 1 (filter (\j -> old U.! j /= res U.! j) [0 .. 2047]) ])
        (m'', seen'', bad') = steps (k - 1 :: Int) m' seen'
    in (m'', seen'', bad <> bad')
  go k acc m seen
    | k >= n = []
    | otherwise =
        let acc' = acc + rate / fps
            (m', seen', bad) = steps (floor acc' - floor acc) m seen
        in bad <> go (k + 1) acc' (C.tick60 m') seen'

-- * NES (step 3): every frame against the model, no frame's work past the
-- next NMI

nesRun :: Int
nesRun = romFrames nesClock + 2 * 120

-- | The PPU's picture after each frame, and the overrun counter ($00FE).
nesScreens :: BS.ByteString -> Int -> ([U.Vector Word16], Int)
nesScreens rom n = runST $ do
  em <- NM.newMachine rom
  m <- either (error . ("nes: " <>)) pure em
  let go f acc
        | f >= n = do
            o <- NM.peekCpu m 0xFE
            pure (reverse acc, fromIntegral o)
        | otherwise = do
            _ <- NM.stepCpu m
            f' <- NP.ppuFramesDone (NM.mPpu m)
            if f' > f then NP.ppuFrame (NM.mPpu m) >>= \s -> go f' (s : acc) else go f acc
  go 0 []

nesDemo :: TestTree
nesDemo = testGroup "NES demo" [ nesRom "rota.nes" NR.OneTable, nesRom "rota-split.nes" NR.Split ]

nesRom :: FilePath -> NR.Layout -> TestTree
nesRom file layout = withResource (BS.readFile (romDir </> file)) (const (pure ())) $ \getRom ->
  testGroup (show layout)
    [ testCase "every frame's work fits in its frame" $ do
        rom <- getRom
        assertEqual "frames overrunning" 0 (snd (nesScreens rom (nesRun + 8)))
    , testCase "every frame as the model shows it (one offset), into the idle loop" $ do
        rom <- getRom
        let scr = V.fromList (fst (nesScreens rom (nesRun + 8)))
            ex = V.fromList (NR.replay layout nesRun)
            bad d = [ k | k <- [0 .. nesRun - 1], scr V.! (k + d) /= ex V.! k ]
            diffAt d k = take 12 [ (i `mod` 256, i `div` 256, ex V.! k U.! i, scr V.! (k + d) U.! i) | i <- [0 .. 256 * 240 - 1], ex V.! k U.! i /= scr V.! (k + d) U.! i ]
        createDirectoryIfMissing True outDir
        BS.writeFile (outDir </> ("nes-spec-colours" <> suffix <> ".bin")) (BS.pack (concatMap (map fromIntegral . U.toList) (V.toList scr)))
        case filter (null . bad) [0 .. 8] of
          (_ : _) -> pure ()
          [] -> assertFailure ("no frame offset matches; first differing frames at offsets 0 to 8: " <> show [ take 3 (bad o) | o <- [0 .. 8] ]
                  <> concat [ "\noffset " <> show o <> ", frame " <> show k <> ": (x, y, model, spec) " <> show (diffAt o k) | o <- [0 .. 8], (k : _) <- [bad o], k > 0 ])
    , testGroup "golden frames"
        [ goldenVsFile n (goldenDir </> ("demo-nes-" <> n <> suffix <> ".png")) out $ do
            rom <- getRom
            createDirectoryIfMissing True outDir
            either (error . ("nes: " <>)) (writeFrame out . NS.screenFrame) (NS.mkScreen (last (fst (nesScreens rom (k + 1)))))
        | (n, k) <- [("beat-1", frameOfBeat nesFps 0.25), ("roll", frameOfBeat nesFps 4.6), ("crash", frameOfBeat nesFps 6.05), ("idle", romFrames nesClock + 60)]
        , let out = outDir </> ("demo-nes-" <> n <> suffix <> ".png") ]
    ]
 where
  suffix = if layout == NR.Split then "-split" else ""

-- * Atari 800XL (step 6): the XEX through AltirraOS and the loader, every
-- frame from the first beat against the model (Rota.A800.renderShot)

a800Run :: Int
a800Run = romFrames a800Clock + 120

-- | The pictures after each frame (320 x 240 colour bytes).
a800Screens :: BS.ByteString -> Int -> IO [U.Vector Word8]
a800Screens xex n = do
  os <- BS.readFile "../../roms/a800xl/os/altirraos_xl.rom"
  basic <- BS.readFile "../../roms/a800xl/os/altirra_basic.rom"
  loader <- BS.readFile "../../build/a800xl/xexload.bin"
  pure $ runST $ do
    m <- AM.newMachine (AS.xexConfig os basic loader xex)
    let go f acc
          | f >= n = pure (reverse acc)
          | otherwise = do
              _ <- AM.stepCpu m
              f' <- AA.anticFrames (AM.mAntic m)
              if f' > f
                then do
                  full <- AG.gtiaFrame (AM.mGtia m)
                  go f' (AP.picturePixels (AP.cropPicture (U.fromList full)) : acc)
                else go f acc
    go 0 []

a800Demo :: TestTree
a800Demo = withResource (BS.readFile (romDir </> "rota.xex")) (const (pure ())) $ \getXex ->
  testGroup "800XL demo"
    [ testCase "every frame from the first beat as the model shows it (one offset), into the idle loop" $ do
        xex <- getXex
        let lead = ckLead a800Clock
            ex = V.fromList (A8.a800Expected a800Run)
        scr <- V.fromList <$> a800Screens xex (a800Run + 400)
        let bad d = [ k | k <- [lead .. a800Run - 1], k + d < V.length scr, scr V.! (k + d) /= ex V.! k ]
            fits = [ d | d <- [0 .. 400], k0 <- [lead], k0 + d < V.length scr, scr V.! (k0 + d) == ex V.! k0, null (bad d) ]
            firstOn = [ d | d <- [0 .. 400], lead + d < V.length scr, scr V.! (lead + d) == ex V.! lead ]
            diffAt d k = take 12 [ (i `mod` 320, i `div` 320, ex V.! k U.! i, scr V.! (k + d) U.! i) | i <- [0 .. 320 * 240 - 1], ex V.! k U.! i /= scr V.! (k + d) U.! i ]
        createDirectoryIfMissing True outDir
        BS.writeFile (outDir </> "a800-spec-colours.bin") (BS.pack (concatMap U.toList (V.toList scr)))
        case fits of
          (d : _) -> do
            -- the lead-in's last second dark: one colour (a line down the
            -- middle came from the missiles' graphics registers, 2026-10-08)
            let dark = [ (k, n) | k <- [lead + d - 60 .. lead + d - 1], let f = scr V.! k
                                , let n = U.length (U.filter (/= f U.! 0) f), n > 0 ]
            assertBool ("the lead-in isn't dark: (frame, pixels not the background) " <> show (take 5 dark)) (null dark)
          [] -> assertFailure ("no offset; offsets where the first beat's frame matches: " <> show (take 5 firstOn)
                  <> concat [ "\noffset " <> show d <> ": first differing frames " <> show (take 5 (bad d)) <> "; at the first (x, y, model, spec): " <> show (diffAt d k) | d <- take 2 firstOn, (k : _) <- [bad d] ])
    , goldenVsString "audio (POKEY's mixed level, the whole machine)" (goldenDir </> "demo-a800-pcm.txt") $ do
        xex <- getXex
        os <- BS.readFile "../../roms/a800xl/os/altirraos_xl.rom"
        basic <- BS.readFile "../../roms/a800xl/os/altirra_basic.rom"
        loader <- BS.readFile "../../build/a800xl/xexload.bin"
        let samples = runST $ do
              m <- AM.newMachine (AS.xexConfig os basic loader xex)
              -- a sample every 37 machine cycles (48 kHz), from the start
              let go n next acc
                    | n >= (48000 * 12 :: Int) = pure (reverse acc)
                    | otherwise = do
                        _ <- AM.stepCpu m
                        t <- AM.machineCycles m
                        if t >= next
                          then do
                            l <- AP2.pokeyLevel (AM.mPokey m)
                            go (n + 1) (next + 37) (l : acc)
                          else go n next acc
              go 0 0 []
        pure (BLC.pack (pcmSummary (U.fromList (map (\l -> fromIntegral (l * 64)) samples))))
    ]

-- * Game Gear (step 5): every frame against the model, the audio

ggRun :: Int
ggRun = ckLead ggClock + ckIdle ggClock + 2 * 120

-- | The spec's window after each frame, and its audio (left, 48 kHz).
ggScreens :: BS.ByteString -> Int -> ([U.Vector Word16], [Int16])
ggScreens rom n = runST $ do
  m <- GS2.newMachine rom
  let go 0 acc au = pure (reverse acc, concat (reverse au))
      go k acc au = do
        GS2.runFrame m
        fb <- GS2.frameBuffer m
        a <- GS2.takeAudio m
        go (k - 1 :: Int) (fb : acc) (map fst a : au)
  go n [] []

ggDemo :: TestTree
ggDemo = withResource (BS.readFile (romDir </> "rota.gg")) (const (pure ())) $ \getRom ->
  testGroup "Game Gear demo"
    [ testCase "every frame as the model shows it (one offset), through two idle loops" $ do
        rom <- getRom
        let (scr, _) = ggScreens rom ggRun
            sv = V.fromList scr
            lead = ckLead ggClock
            -- the spec's frame f shows picture frame f - d
            -- from 15 frames into the lead-in (the ROM's init clears VRAM first)
            bad d = [ f | f <- [max 0 (d - lead + 15) .. V.length sv - 1], f - d < ckIdle ggClock + 240, sv V.! f /= ggExpected (f - d) ]
            firstOn = [ d | d <- [0 .. lead + 60], d < V.length sv, sv V.! d == ggExpected 0 ]
            fits = [ d | d <- firstOn, null (bad d) ]
            diffAt d f = take 8 [ (i `mod` 160, i `div` 160, ggExpected (f - d) U.! i, sv V.! f U.! i) | i <- [0 .. 160 * 144 - 1], ggExpected (f - d) U.! i /= sv V.! f U.! i ]
            codes = M.fromList (zip (S.toList (S.fromList (concatMap U.toList scr))) [0 :: Word8 ..])
        -- layer 4's reference (demos/layer4.sh gg): each colour as a byte
        createDirectoryIfMissing True outDir
        BS.writeFile (outDir </> "gg-spec-colours.bin") (BS.pack (concat [ map (codes M.!) (U.toList f) | f <- scr ]))
        case fits of
          (_ : _) -> pure ()
          [] -> assertFailure ("no offset; picture 0 shows at spec frames " <> show (take 5 firstOn)
                  <> concat [ "\noffset " <> show d <> ": first differing frames " <> show (take 5 (bad d)) <> "; at the first (x, y, model, spec): " <> show (diffAt d f) | d <- take 2 firstOn, (f : _) <- [bad d] ])
    , testCase "every frame's work fits before the window's first line" $
        assertBool ("frames over " <> show budget <> " T-states: " <> show (take 5 [ (k, c) | (k, c) <- zip [0 :: Int ..] romFrameCosts, c > budget ])) (all (<= budget) romFrameCosts)
    , goldenVsString "audio (the PSG, left)" (goldenDir </> "demo-gg-pcm.txt") $ do
        rom <- getRom
        let (_, au) = ggScreens rom (ceiling (12 * 59.92 :: Double))
        pure (BLC.pack (pcmSummary (U.fromList au)))
    ]

-- * Master System (M6): every frame against the model, the audio

smsRun :: Int
smsRun = ckLead smsClock + ckIdle smsClock + 2 * 120

-- | The spec's Master System II (no BIOS) screen after each frame, and its
-- audio (left, 48 kHz).
smsScreens :: BS.ByteString -> Int -> ([U.Vector Word16], [Int16])
smsScreens rom n = runST $ do
  m <- GS2.newMasterSystem Nothing GS2.SegaMapper rom
  let go 0 acc au = pure (reverse acc, concat (reverse au))
      go k acc au = do
        GS2.runFrame m
        fb <- GS2.frameBuffer m
        a <- GS2.takeAudio m
        go (k - 1 :: Int) (fb : acc) (map fst a : au)
  go n [] []

smsDemo :: TestTree
smsDemo = withResource (BS.readFile (romDir </> "rota.sms")) (const (pure ())) $ \getRom ->
  testGroup "Master System demo"
    [ testCase "every frame as the model shows it (one offset), through two idle loops" $ do
        rom <- getRom
        let (scr, _) = smsScreens rom smsRun
            sv = V.fromList scr
            lead = ckLead smsClock
            n = smsW * smsH
            -- the spec's frame f shows picture frame f - d, from 15 frames
            -- into the lead-in (the ROM's init clears VRAM first)
            bad d = [ f | f <- [max 0 (d - lead + 15) .. V.length sv - 1], f - d < ckIdle smsClock + 240, sv V.! f /= smsExpected (f - d) ]
            firstOn = [ d | d <- [0 .. lead + 60], d < V.length sv, sv V.! d == smsExpected 0 ]
            fits = [ d | d <- firstOn, null (bad d) ]
            diffAt d f = take 8 [ (i `mod` smsW, i `div` smsW, smsExpected (f - d) U.! i, sv V.! f U.! i) | i <- [0 .. n - 1], smsExpected (f - d) U.! i /= sv V.! f U.! i ]
            codes = M.fromList (zip (S.toList (S.fromList (concatMap U.toList scr))) [0 :: Word8 ..])
        -- layer 4's reference (demos/layer4.sh sms): each colour as a byte
        createDirectoryIfMissing True outDir
        BS.writeFile (outDir </> "sms-spec-colours.bin") (BS.pack (concat [ map (codes M.!) (U.toList f) | f <- scr ]))
        case fits of
          (_ : _) -> pure ()
          [] -> assertFailure ("no offset; picture 0 shows at spec frames " <> show (take 5 firstOn)
                  <> concat [ "\noffset " <> show d <> ": first differing frames " <> show (take 5 (bad d)) <> "; at the first (x, y, model, spec): " <> show (diffAt d f) | d <- take 2 firstOn, (f : _) <- [bad d] ])
    , testCase "every frame's work fits before the next frame's first line" $
        assertBool ("frames over " <> show smsBudget <> " T-states: " <> show (take 5 [ (k, c) | (k, c) <- zip [0 :: Int ..] smsFrameCosts, c > smsBudget ])) (all (<= smsBudget) smsFrameCosts)
    , goldenVsString "audio (the PSG, left)" (goldenDir </> "demo-sms-pcm.txt") $ do
        rom <- getRom
        let (_, au) = smsScreens rom (ceiling (12 * 59.92 :: Double))
        pure (BLC.pack (pcmSummary (U.fromList au)))
    ]

-- * The Super Nintendo (M7 step 7.13)

snesRun :: Int
snesRun = ckLead snesClock + ckIdle snesClock + 2 * 120

-- | The spec's Super Nintendo screen after each frame (each row's left
-- half-dots: 256 x 224, 15-bit), and its audio (left, 32 kHz). The boot
-- ROM is ours (make snes-asm).
snesScreens :: BS.ByteString -> BS.ByteString -> Int -> ([U.Vector Word16], [Int16])
snesScreens ipl rom n = runST $ do
  let cart = maybe (error "rota.sfc: no header") id (parseCart rom)
  m <- SS.newMachine ipl cart rom
  SS.reset m
  SS.setPads m 0 0
  let go 0 acc = do
        au <- SS.audioSamples m
        pure (reverse acc, [ fromIntegral (au U.! i) | i <- [0, 2 .. U.length au - 1] ])
      go k acc = do
        SS.runFrames m 1
        pic <- SS.frameColors m
        let fb = U.generate (snesW * snesH) (\i -> pic U.! ((i `div` snesW) * 512 + 2 * (i `mod` snesW)) .&. 0x7FFF)
        go (k - 1 :: Int) (fb : acc)
  go n []

snesDemo :: TestTree
snesDemo = withResource ((,) <$> BS.readFile "../../build/snes/ipl.bin" <*> BS.readFile (romDir </> "rota.sfc")) (const (pure ())) $ \getRom ->
  testGroup "Super Nintendo demo"
    [ testCase "every frame as the model shows it (one offset), through two idle loops" $ do
        (ipl, rom) <- getRom
        let (scr, _) = snesScreens ipl rom snesRun
            sv = V.fromList scr
            lead = ckLead snesClock
            n = snesW * snesH
            -- (the lead-in: forced blank shows black, where the Sega
            -- VDP's display off shows the backdrop the model draws)
            want p = if p < 0 then U.replicate n 0 else U.map cgram15 (snesExpected p)
            -- the spec's frame f shows picture frame f - d, from 15 frames
            -- into the lead-in (the ROM's init runs first)
            bad d = [ f | f <- [max 0 (d - lead + 15) .. V.length sv - 1], f - d < ckIdle snesClock + 240, sv V.! f /= want (f - d) ]
            firstOn = [ d | d <- [0 .. lead + 60], d < V.length sv, sv V.! d == want 0 ]
            fits = [ d | d <- firstOn, null (bad d) ]
            diffAt d f = take 8 [ (i `mod` snesW, i `div` snesW, want (f - d) U.! i, sv V.! f U.! i) | i <- [0 .. n - 1], want (f - d) U.! i /= sv V.! f U.! i ]
            codes = M.fromList (zip (S.toList (S.fromList (concatMap U.toList scr))) [0 :: Word8 ..])
        -- layer 4's reference (demos/layer4.sh snes): each colour as a byte
        createDirectoryIfMissing True outDir
        BS.writeFile (outDir </> "snes-spec-colours.bin") (BS.pack (concat [ map (codes M.!) (U.toList f) | f <- scr ]))
        case fits of
          (_ : _) -> pure ()
          [] -> assertFailure ("no offset; picture 0 shows at spec frames " <> show (take 5 firstOn)
                  <> concat [ "\noffset " <> show d <> ": first differing frames " <> show (take 5 (bad d)) <> "; at the first (x, y, model, spec): " <> show (diffAt d f) | d <- take 2 firstOn, (f : _) <- [bad d] ])
    , testCase "every frame's work fits in V-blank" $
        assertBool ("frames over " <> show snesBudget <> " master clocks: " <> show (take 5 [ (k, c) | (k, c) <- zip [0 :: Int ..] snesFrameCosts, c > snesBudget ])) (all (<= snesBudget) snesFrameCosts)
    , goldenVsString "audio (the S-DSP, left)" (goldenDir </> "demo-snes-pcm.txt") $ do
        (ipl, rom) <- getRom
        let (_, au) = snesScreens ipl rom (ceiling (12 * 60.1 :: Double))
        pure (BLC.pack (pcmSummary (U.fromList au)))
    ]
