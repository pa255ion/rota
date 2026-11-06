-- | Step 1's gate (PLAN.md, side track J): the score's consonance, the
-- register streams' rules, the budgets, goldens of every emitted file and
-- of the logo, and the streams played through the specs' APUs (a golden
-- of the PCM and a pitch check on every note).
module Main (main) where

import Data.Bits (shiftL, testBit, (.&.), (.|.))
import qualified Data.ByteString.Lazy.Char8 as BLC
import Data.List (maximumBy, nub, sort)
import qualified Data.Map.Strict as M
import Data.Ord (comparing)
import Data.Word (Word8)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import System.FilePath ((</>))
import Test.Tasty
import Test.Tasty.Golden (goldenVsString)
import Test.Tasty.HUnit
import Text.Printf (printf)

import Rota.Emit (outputs)
import Rota.Clock (romFrames)
import Rota.Gb (gbClock)
import Rota.Nes (nesClock)
import qualified Rota.Nes.Rom as NR
import qualified Rota.Gb.Rom as GR
import Rota.Lab
import Rota.Logo
import Rota.Palette
import Rota.Render
import Rota.Score
import Rota.Sound
import Rota.Sound.Chip8
import Rota.Sound.Gb
import Rota.Sound.Nes
import qualified Rota.Sound.A800 as SA

main :: IO ()
main = defaultMain $ testGroup "rota-brand"
  [ scoreTests
  , gbTests
  , nesTests
  , chip8Tests
  , a800SoundTests
  , budgetTests
  , logoTests
  , labTests
  , goldens
  , pcmTests
  ]

goldenDir :: FilePath
goldenDir = "test/golden"

-- * The score

scoreTests :: TestTree
scoreTests = testGroup "score"
  [ testCase "each voice is one phrase" $
      mapM_ (\v -> length (phrases (notes v)) @?= 1) [minBound .. maxBound]
  , testCase "consonant on the eighth-note grid (the passing G5 at 5.5 the one exception)" $
      dissonances @?= [(5.5, Lead, Harm, 13), (5.5, Lead, Bass, 34)]
  , testCase "drums: a new hit every frame at most, the roll's volume 3 to 14" $ do
      let rolls = [ hVol h | h <- drums, isRoll (hDrum h) ]
      (take 1 rolls, drop (length rolls - 1) rolls) @?= ([3], [14])
      sort (map hBeat drums) @?= map hBeat drums
  ]
 where
  isRoll (Roll _) = True
  isRoll _ = False

-- | (beat, upper voice, lower voice, semitones) of every pair that isn't
-- a unison, third, fifth, sixth or octave (compound intervals reduced),
-- or a fourth between the upper two voices.
dissonances :: [(Double, Voice, Voice, Int)]
dissonances =
  [ (b, u, l, d)
  | b <- [0, 0.5 .. endBeat - 0.1]
  , (u, l) <- [(Lead, Harm), (Lead, Bass), (Harm, Bass)]
  , Just mu <- [at u b], Just ml <- [at l b]
  , let d = mu - ml
  , not (ok (u, l) (d `mod` 12)) ]
 where
  at v b = case [ nMidi n | n <- notes v, nStart n <= b, b < nEnd n ] of
    (m : _) -> Just m
    [] -> Nothing
  ok pair i = i `elem` [0, 3, 4, 7, 8, 9] || (pair == (Lead, Harm) && i == 5)

-- * Streams

-- | The value of each register after each frame's writes.
replay :: Stream -> [M.Map Int Int]
replay = drop 1 . scanl (\m ws -> foldl (\m' w -> M.insert (wReg w) (fromIntegral (wVal w)) m') m ws) M.empty

-- | Frames in which a write matches.
framesWith :: (Write -> Bool) -> Stream -> [Int]
framesWith p s = [ k | (k, ws) <- zip [0 ..] s, any p ws ]

phraseStarts, finalStarts :: Double -> Voice -> [Int]
phraseStarts fps v = [ spFrom sp | sp <- noteSpans fps v, spPhraseStart sp ]
finalStarts fps v = [ spFrom sp | sp <- noteSpans fps v, abs (nEnd (spNote sp) - endBeat) < 1e-6 ]

gbTests :: TestTree
gbTests = testGroup "Game Boy stream"
  [ testCase "triggers only at phrase starts and the final note (pulses, wave)" $
      mapM_ (\(v, nr4) -> framesWith (\w -> wReg w == nr4 && testBit (wVal w) 7) gbStream @?= allowed v)
        [(Lead, 0xFF14), (Harm, 0xFF19), (Bass, 0xFF1E)]
  , testCase "the noise channel triggers once per hit" $
      framesWith (\w -> wReg w == 0xFF23) gbStream @?= [ frameOfBeat gbFps (hBeat h) | h <- drums ]
  , testCase "every drum's noise setting gives the page's rate exactly" $
      mapM_ (\(d, hz) -> gbNoiseHz (fst (gbNoise d)) @?= hz)
        [(Kick, 8192), (Snare, 32768), (Hat, 262144), (Roll 0, 16384), (Roll 5, 65536), (Crash, 16384)]
  , testCase "the wave RAM is the page's sine" $
      gbWaveRam @?= [0x8A, 0xBC, 0xDE, 0xFF, 0xFF, 0xED, 0xCB, 0xA8, 0x75, 0x43, 0x21, 0x00, 0x00, 0x12, 0x34, 0x57]
  ]
 where
  -- the bass has no final retrigger
  allowed Bass = phraseStarts gbFps Bass
  allowed v = sort (nub (phraseStarts gbFps v <> finalStarts gbFps v))

nesTests :: TestTree
nesTests = testGroup "NES stream"
  [ testCase "every lead period, with glides and vibrato, is below 256" $
      assertBool "a lead period of 256 or more" (all (< 256) (periods 0x4002 0x4003))
  , testCase "$4003 is written only at the lead's phrase start" $
      framesWith (\w -> wReg w == 0x4003) nesStream @?= phraseStarts nesFps Lead
  , testCase "$4007 only at the harmony's start and where the score crosses 256 (F#4 and back)" $
      framesWith (\w -> wReg w == 0x4007) nesStream
        @?= phraseStarts nesFps Harm <> [ frameOfBeat nesFps 5, frameOfBeat nesFps 6 ]
  , testCase "the harmony's high byte only changes with the note, never inside one" $
      mapM_ (\sp -> length (nub [ p `div` 256 | (k, p) <- zip [0 ..] (periods 0x4006 0x4007), k >= spFrom sp, k < spTo sp ]) @?= 1)
        (noteSpans nesFps Harm)
  , testCase "the noise periods are table entries (the roll climbs 508 to 128)" $
      [ nesNoisePeriods !! fst (nesNoise (Roll r)) | r <- [0 .. 5] ] @?= [508, 380, 254, 202, 160, 128]
  ]
 where
  periods rl rh = [ M.findWithDefault 0 rl m .|. (M.findWithDefault 0 rh m .&. 7) `shiftL` 8 | m <- replay nesStream ]

chip8Tests :: TestTree
chip8Tests = testGroup "CHIP-8 beeps"
  [ testCase "one beep per lead note, 3 to 12 frames, 30 for the last" $ do
      length chip8Beeps @?= length (notes Lead)
      assertBool "lengths" (all (\(_, st) -> st >= 3 && st <= 12) (init chip8Beeps))
      snd (last chip8Beeps) @?= 30
  , testCase "a beep ends before the next starts" $
      assertBool "overlap" (and (zipWith (\(f, st) (f', _) -> f + st <= f') chip8Beeps (drop 1 chip8Beeps)))
  ]

-- * Budgets (docs/brand.md, "Budgets")

budgetTests :: TestTree
budgetTests = testGroup "budgets"
  [ testCase "Game Boy stream: 8 KiB, 600 frames" $ budget (encodeStream fromIntegral gbStream) gbStream
  , testCase "NES stream: 8 KiB, 600 frames" $ budget (encodeStream fromIntegral nesStream) nesStream
  ]
 where
  budget bs s = do
    assertBool ("stream of " <> show (length bs) <> " bytes") (length bs <= 8192)
    assertBool ("stream of " <> show (length s) <> " frames") (length s <= 600)

-- * The 800XL's POKEY stream

a800SoundTests :: TestTree
a800SoundTests = testGroup "800XL (POKEY)"
  [ testCase "held notes: lead and harmony within 25 cents of the score (the 64 kHz clock), the bass within 5" $ do
      let off v m = let f = midiHz m
                        got = if v == Bass then SA.bassNHz (SA.bassN f) else SA.audf64Hz (SA.audf64 f)
                    in abs (1200 * logBase 2 (got / f))
          worst v = maximum [ off v (nMidi n) | n <- notes v ]
      assertBool ("lead " <> show (worst Lead)) (worst Lead <= 25)
      assertBool ("harmony " <> show (worst Harm)) (worst Harm <= 25)
      assertBool ("bass " <> show (worst Bass)) (worst Bass <= 5)
  , testCase "every frame's writes fit the VBI (at most 12), the stream within 8 KiB" $ do
      let bs = encodeStream fromIntegral SA.a800Stream
      assertBool ("most writes " <> show (maximum (map length SA.a800Stream))) (maximum (map length SA.a800Stream) <= 12)
      assertBool ("bytes " <> show (length bs)) (length bs <= 8192)
  ]

-- * Rendition B: the port of the tile lab gives the lab's own figures
-- (docs/brand.md, "Measured in the tile lab")

labTests :: TestTree
labTests = testGroup "tile lab"
  [ testCase "NES: 214 unique tiles, 114 / 114, 16 writes, 12 sprites, 4 a line, every area 3 colours" $ do
      let st = labStats (geometry nesM) True False Nothing
      (lsUnique st, lsSplit st, lsMaxWrites st, lsSprMax st, lsSprLine st, lsAttrMax st) @?= (214, (114, 114), 16, 12, 4, 0)
  , testCase "Game Boy ROM: every frame shows the picture on time (two idle loops)" $ do
      let n = romFrames gbClock + 2 * 120
          late = [ k | (k, a, b) <- zip3 [0 :: Int ..] (GR.replay n) (GR.gbExpected n), a /= b ]
      take 10 late @?= []
  , testCase "Game Boy ROM: every frame's vertical-blank work in budget, the idle loop's too" $ do
      [ k | (k, f) <- zip [0 :: Int ..] (GR.runFrames (romFrames gbClock + 2 * 120)), GR.frameCost f > GR.budgetCycles ] @?= []
  , testCase "Game Boy: 252 background tiles of 256, 29 sprite tiles of 128" $
      (length GR.bgTiles, length GR.objTiles) @?= (252, 29)
  , testGroup "NES ROM"
      [ testGroup (show l)
          [ testCase "every frame shows the picture on time (two idle loops)" $ do
              let n = romFrames nesClock + 2 * 120
                  late = [ k | (k, a, b) <- zip3 [0 :: Int ..] (NR.replay l n) (NR.nesExpected n), a /= b ]
              take 10 late @?= []
          , testCase "every NMI's work in budget, the idle loop's too" $
              [ k | (k, f) <- zip [0 :: Int ..] (NR.runFrames l (romFrames nesClock + 2 * 120)), NR.nmiCost l f > NR.nmiBudget ] @?= []
          , testCase "every pattern table within 256 tiles" $
              assertBool "tiles" (all (<= 256) [length (NR.bgTiles l 0), length (NR.bgTiles l 1) + length (NR.objTiles l)])
          ]
      | l <- [NR.OneTable, NR.Split] ]
  , testCase "NES, one table: 249 background tiles, 15 sprite tiles" $
      (length (NR.bgTiles NR.OneTable 0), length (NR.objTiles NR.OneTable)) @?= (249, 15)
  , testCase "Game Boy: 150 unique tiles, 92 writes with the swaps, 8 sprites, 4 a line" $ do
      let st = labStats (geometry gbM) False False (Just gbLabShade)
      (lsUnique st, lsMaxWrites st, lsSprMax st, lsSprLine st) @?= (150, 92, 8, 4)
  ]

-- | The lab's Game Boy colours (@LOOK.gb@) as shades: 0 ink, 1 teal, 2 amber, 3 paper.
gbLabShade :: RoleState -> Word8 -> Int
gbLabShade st r
  | r >= 1 && r <= 5 = case rsRing st !! fromIntegral (r - 1) of
      RingOff -> 0
      RingOn -> 1
      RingHead -> 3
      RingMid -> 1
      RingBg -> 0
  | r == roleEmb = 2
  | r == roleCore = 3
  | r == roleLetters = if rsLettersLit st then 3 else 0
  | r == roleTeal = 2
  | r == roleViolet = 1
  | otherwise = 0

-- * Rendition A: the 64 × 32 logo (CHIP-8)

logoTests :: TestTree
logoTests = testGroup "logo"
  [ testCase "eight ring segments, mirror-symmetric pairs" $ do
      let seg k = sort [ (x, y) | x <- [0 .. 31], y <- [0 .. 31], segmentOf x y == Just k ]
          mirror = map (\(x, y) -> (31 - x, y))
      mapM_ (\k -> assertBool ("segment " <> show k) (not (null (seg k)))) [0 .. 7]
      mapM_ (\k -> sort (mirror (seg k)) @?= seg ((8 - k) `mod` 8)) [0 .. 7]
  , testCase "the emblem is four-fold symmetric when whole" $ do
      let e = emblemAt 1
          rot i = let (y, x) = i `divMod` 32 in (31 - x) * 32 + y
      V.toList (V.generate 1024 (\i -> e V.! rot i)) @?= V.toList e
  , testCase "letters: R, O, T, A in rows 10 to 20" $ do
      assertBool "rows" (all (\(_, y) -> y >= 10 && y <= 20) letterPixels)
      length letterPixels @?= 46 + 44 + 30 + 46
  ]

-- | A picture as text: one character per pixel ('.' unlit).
art :: (c -> Char) -> V.Vector (Maybe c) -> String
art ch v = unlines [ [ maybe '.' ch (v V.! (y * width + x)) | x <- [0 .. width - 1] ] | y <- [0 .. height - 1] ]

moment :: Maybe Double -> Moment
moment b = Moment b (\v -> case [ thOmega t | t <- threads, thVoice t == v ] of
  (w : _) -> w * (leadInSec + maybe 0 (* beatSec) b)
  [] -> 0)

logoFrames :: [(String, Maybe Double)]
logoFrames = [("lead-in", Nothing), ("beat-1", Just 0.25), ("roll", Just 5.2), ("grow", Just 5.8), ("crash", Just 6.05), ("idle", Just 9.3)]

-- * Goldens

goldens :: TestTree
goldens = testGroup "goldens"
  [ testGroup "emitted files"
      [ goldenVsString p (goldenDir </> flat p) (pure (BLC.pack s)) | (p, s) <- outputs ]
  , testGroup "logo (CHIP-8 style)"
      [ goldenVsString n (goldenDir </> ("logo-chip8-" <> n <> ".txt")) (pure (BLC.pack (art (const '#') (picture chip8Style (moment b))))) | (n, b) <- logoFrames ]
  ]
 where
  flat = map (\c -> if c == '/' then '-' else c)

-- * The streams through the specs' APUs

pcmTests :: TestTree
pcmTests = withResource (pure (renderGb gbStream 30, renderNes nesStream 30)) (const (pure ())) $ \get ->
  testGroup "through the specs' APUs"
    [ goldenVsString "Game Boy PCM" (goldenDir </> "pcm-gb.txt") (BLC.pack . pcmSummary . rPcm . fst <$> get)
    , goldenVsString "NES PCM" (goldenDir </> "pcm-nes.txt") (BLC.pack . pcmSummary . rPcm . snd <$> get)
    , testCase "Game Boy: every note within 5 cents of its register's pitch" $ do
        (g, _) <- get
        checkPitches gbFps (rEdges g) gbStream
          [ (Lead, 0, 0xFF13, 0xFF14), (Harm, 1, 0xFF18, 0xFF19), (Bass, 2, 0xFF1D, 0xFF1E) ]
          gbPeriod gbPeriodHz
    , testCase "NES: every note within 5 cents of its register's pitch" $ do
        (_, n) <- get
        checkPitches nesFps (rEdges n) nesStream
          [ (Lead, 0, 0x4002, 0x4003), (Harm, 1, 0x4006, 0x4007), (Bass, 2, 0x400A, 0x400B) ]
          nesPeriod nesPeriodHz
    ]

-- | For each note: the longest run of frames in which the stream holds
-- the note's own (unbent) period; the channel's frequency there, from its
-- rising edges, against the period's frequency.
checkPitches :: Double -> [U.Vector Double] -> Stream -> [(Voice, Int, Int, Int)]
             -> (Voice -> Double -> Int) -> (Voice -> Int -> Double) -> Assertion
checkPitches fps edges s chans period periodHz = do
  let regs = replay s
      reports =
        [ (v, nMidi (spNote sp), cents, n)
        | (v, c, rl, rh) <- chans
        , sp <- noteSpans fps v
        , let want = period v (midiHz (nMidi (spNote sp)))
              held = [ k | k <- [spFrom sp .. spTo sp - 1]
                         , let m = regs !! k
                         , (M.findWithDefault 0 rl m .|. (M.findWithDefault 0 rh m .&. 7) `shiftL` 8) == want ]
              (r0, r1) = longestRun held
              es = U.filter (\t -> t >= fromIntegral (r0 + 1) / fps && t <= fromIntegral (r1 + 1) / fps) (edges !! c)
              n = U.length es
              got = fromIntegral (n - 1) / (U.last es - U.head es)
              cents = if n < 3 then 1 / 0 else 1200 * logBase 2 (got / periodHz v want)
        ]
      bad = [ r | r@(_, _, c, _) <- reports, not (abs c <= 5) ]
  assertBool (unlines [ printf "%s %s: %.2f cents over %d edges" (show v) (noteName m) c n | (v, m, c, n) <- bad ]) (null bad)

-- | The longest run of consecutive frames (first, last).
longestRun :: [Int] -> (Int, Int)
longestRun [] = (0, -1)
longestRun ks = maximumBy (comparing (\(a, b) -> b - a)) (go ks)
 where
  go [] = []
  go (k : rest) = let run = k : map snd (takeWhile (uncurry (==)) (zip [k + 1 ..] rest))
                  in (k, last run) : go (drop (length run - 1) rest)
