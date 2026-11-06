-- | Layer 2 (docs/TESTING.md): the hardware core against the reference
-- model, in lockstep. The core runs in Clash simulation; every event on
-- its trace output ('coEvent') is replayed on "Chip8.Spec" (an instruction
-- with the keys the core saw, or a timer tick), and the architectural
-- states must agree after each one. At the end the framebuffer, read
-- through the video port, must equal the model's display.
module Main (main) where

import Prelude

import Codec.Picture (PixelRGB8 (..), pixelAt)
import Control.Exception (ErrorCall (..), throwIO)
import Data.Bits
import qualified Data.ByteString as BS
import Data.Maybe (isJust)
import qualified Data.Vector.Unboxed as U
import Data.Word
import System.Directory (doesFileExist)
import System.FilePath ((</>), (<.>))
import qualified Clash.Prelude as C
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog

import Chip8.Core
import Chip8.Decode (encode)
import qualified Chip8.Spec as S
import Chip8.SpecState (specFb, specRam, specTable)
import Chip8.StateTable (tableSize, tInstrPend, tTickPend)
import Chip8.StateTap (tableWrite, tapTable)
import Chip8.StateHooks (HookOut (..), chip8StateHooks)
import Chip8.Types
import Platform.Sim.Frame (readFrame)

main :: IO ()
main = defaultMain $ testGroup "chip8-lockstep"
  [ testProperty "random programs" $ withTests 300 $ property $ do
      sc <- forAll genScenario
      case lockstep sc of
        Right _  -> success
        Left err -> annotate err >> failure
  , romTests
  , stateTests
  ]

-- * Save states (side track N, N.10): the direct path in the core

stateTests :: TestTree
stateTests = testGroup "save states in the core (side track N, N.10)"
  [ testProperty "the core's table is the model's after a run" $ withTests 100 $ property $ do
      sc <- forAll genScenario
      case tableCheck sc of
        Right _ -> success
        Left err -> annotate err >> failure
  , testProperty "the framebuffer through the hooks' windows: read as the model's, written back whole" $ withTests 40 $ property $ do
      sc <- forAll genScenario
      case hookedFb sc of
        Right _ -> success
        Left err -> annotate err >> failure
  , testProperty "a state written into a frozen core runs on as the model" $ withTests 60 $ property $ do
      sc <- forAll genScenario
      case loadCheck sc of
        Right _ -> success
        Left err -> annotate err >> failure
  ]

-- | The core with its hooks ("Chip8.StateHooks", as Chip8.ArtyTop wires
-- them) over inputs and link writes (the menu's pause, the PS's writes):
-- the bytes on the byte channel.
simHooked :: [(Chip8In, Bool, Maybe (C.Unsigned 24, C.BitVector 8))] -> [Maybe (C.BitVector 8)]
simHooked ins = C.simulateN @C.System (length ins) f ins
 where
  f i = let (inp, pause, link) = C.unbundle i
            (_, st, ramD, fbD) = chip8CoreWith tableWrite (C.blockRam fontRam) inp (hoFrozen ho) (hoRecWr ho) (hoPort ho)
            ho = chip8StateHooks link pause (pure False) st ramD fbD
        in hoByte ho

-- | A scenario run, then paused: freeze (the state mode, the read-back
-- control), the framebuffer's 256 bytes read through the read window (as
-- the model's), another picture's bytes written through the write window,
-- read again (as written).
hookedFb :: Scenario -> Either String ()
hookedFb sc = do
  (_, m) <- lockstep sc
  let idle = (chip8InIdle, True, Nothing)
      w a d = (chip8InIdle, True, Just (a, d))
      -- one link write every 4 clocks, as the PS's are far apart
      spaced = concatMap (\x -> [x, idle, idle, idle])
      run = [ (i, False, Nothing) | i <- inputs sc ]
      freeze = spaced [w 0x140000 1, w 0x140050 1] <> replicate 40 idle
      readFb = spaced [ w (0x173000 + fromIntegral k) 0 | k <- [0 .. 255 :: Int] ]
      other = [ fromIntegral ((k * 37 + 11) `mod` 256) :: Word8 | k <- [0 .. 255 :: Int] ]
      writeFb = spaced [ w (0x153000 + fromIntegral k) (C.pack b) | (k, b) <- zip [0 :: Int ..] other ]
      ins = run <> freeze <> readFb <> writeFb <> readFb <> replicate 10 idle
      bytes = [ fromIntegral b :: Word8 | Just b <- simHooked ins ]
      want = U.toList (specFb m)
      (first, second) = splitAt 256 bytes
  if length bytes /= 512 then Left ("bytes read: " <> show (length bytes))
  else if first /= want then Left ("the framebuffer as read differs from the model's at bytes " <> show [ k | (k, a, b) <- zip3 [0 :: Int ..] first want, a /= b ])
  else if second /= other then Left ("the framebuffer written and read back differs at bytes " <> show [ k | (k, a, b) <- zip3 [0 :: Int ..] second other, a /= b ])
  else Right ()

-- | 'chip8CoreWith' over inputs with the freeze, table writes and the
-- state port: its outputs and its state each cycle.
simWith :: [(Chip8In, Bool, Maybe (C.Unsigned 6, C.BitVector 8), Maybe StPort)] -> [(Chip8Out, CoreState)]
simWith ins = C.simulateN @C.System (length ins) f ins
 where
  f i = let (inp, fr, w, p) = C.unbundle i
            (o, st, _, _) = chip8CoreWith tableWrite (C.blockRam fontRam) inp fr w p
        in C.bundle (o, st)

plain :: Chip8In -> (Chip8In, Bool, Maybe (C.Unsigned 6, C.BitVector 8), Maybe StPort)
plain i = (i, False, Nothing, Nothing)

-- | The table of the core's last state (after the quiet cycles: between
-- instructions) against the model's, the core's latched ticks aside.
tableCheck :: Scenario -> Either String ()
tableCheck sc = do
  (_, m) <- lockstep sc
  let outs = simWith (map plain (inputs sc))
      st = snd (last outs)
      core = [ fromIntegral (tapTable st (fromIntegral k)) :: Word8 | k <- [0 .. tableSize - 1] ]
      spec = U.toList (specTable m)
      diffs = [ (k, c, e) | (k, c, e) <- zip3 [0 :: Int ..] core spec, k /= tTickPend, k /= tInstrPend, c /= e ]
  if null diffs then Right () else Left ("table bytes differ (byte, core, model): " <> show diffs)

-- | The model's state after a scenario written into a fresh core, frozen
-- (the table byte by byte, RAM, the framebuffer row by row), then the
-- same scenario's running inputs again: the core goes on as the model
-- does from that state.
loadCheck :: Scenario -> Either String ()
loadCheck sc = do
  (_, mA) <- lockstep sc
  let tb = specTable mA
      frz w p = (chip8InIdle, True, w, p)
      setup = [ frz (Just (fromIntegral k, C.pack (tb U.! k))) Nothing | k <- [0 .. tableSize - 1] ]
           <> [ frz Nothing (Just (StRam (fromIntegral a) (Just (C.pack (specRam mA U.! a))))) | a <- [0 .. 4095] ]
           <> [ frz Nothing (Just (StFb (fromIntegral r) (Just (specRow mA r)))) | r <- [0 .. 31] ]
      cont = map plain (drop (length (scProgram sc) + length (scPreset sc)) (inputs sc))
      ins = setup <> cont
      outs = map fst (simWith ins)
      q = S.quirksFromConfig (scConfig sc)
      evs = [ e | Just e <- map coEvent (drop (length setup) outs) ]
  m <- replayFrom q mA evs
  let n = length ins
      -- coFbRow at cycle c is the row the video port addressed at c - 1
      addrs = [ fromIntegral (ciFbAddr i) :: Int | (i, _, _, _) <- ins ]
      rows = [ (addrs !! (c - 1), coFbRow o) | (c, o) <- zip [0 :: Int ..] outs, c >= n - 32 ]
      bad = [ (r, w) | (r, w) <- rows, w /= specRow m r ]
  if null bad then Right () else Left ("framebuffer rows differ after the load: " <> show (map fst bad))
 where
  replayFrom q m0 = go m0 (0 :: Int)
   where
    go m _ [] = Right m
    go m i (e : es) = case e of
      Ticked -> go (S.tick60 m) (i + 1) es
      Retired keys arch -> do
        let m' = S.step q (fromIntegral keys) m
        case archDiff arch m' of
          Nothing -> go m' (i + 1) es
          Just d -> Left ("after the load, event " <> show i <> ": " <> d)

-- * Scenarios

data Scenario = Scenario
  { scProgram :: [Word8]
  , scPreset  :: [(Int, Word8)]  -- ^ extra RAM bytes (for example 0x1FF)
  , scConfig  :: Word8           -- ^ quirk bits
  , scPeriod  :: Int             -- ^ cycles between timer ticks
  , scKeys    :: [Word16]        -- ^ keys for frame f (cycled)
  , scCycles  :: Int             -- ^ cycles of running
  }

instance Show Scenario where
  show sc = unlines
    [ "program: " <> unwords (map hex16 (pairs (scProgram sc)))
    , "preset: " <> show (scPreset sc)
    , "config: " <> show (scConfig sc) <> ", tick every " <> show (scPeriod sc)
      <> " cycles, " <> show (scCycles sc) <> " cycles"
    , "keys per frame: " <> show (scKeys sc) ]
   where
    pairs (a : b : r) = (fromIntegral a `shiftL` 8 .|. fromIntegral b :: Int) : pairs r
    pairs _ = []
    hex16 w = let s = "000" <> showHex' w in drop (length s - 4) s
    showHex' n = let d = "0123456789ABCDEF" in if n < 16 then [d !! n] else showHex' (n `div` 16) <> [d !! (n `mod` 16)]

-- | Cycles after the run with no new instructions, so the last one
-- finishes and the framebuffer can be read back.
quietCycles :: Int
quietCycles = 100

inputs :: Scenario -> [Chip8In]
inputs sc = zipWith (\c i -> i { ciFbAddr = fromIntegral (c `mod` 32) }) [0 :: Int ..] (loads ++ run ++ quiet)
 where
  writes = zip [0x200 ..] (scProgram sc) ++ scPreset sc
  loads  = [ chip8InIdle { ciRomWrite = Just (fromIntegral a, C.pack b) } | (a, b) <- writes ]
  keysAt f = case scKeys sc of
    [] -> 0
    ks -> ks !! (f `mod` length ks)
  run = [ chip8InIdle
            { ciRun = True, ciInstrTick = True
            , ciTimerTick = c `mod` scPeriod sc == scPeriod sc - 1
            , ciKeys = C.pack (keysAt (c `div` scPeriod sc))
            , ciConfig = C.pack (scConfig sc) }
        | c <- [0 .. scCycles sc - 1] ]
  quiet = replicate quietCycles chip8InIdle

-- | The model's machine for a scenario.
specStart :: Scenario -> S.Machine
specStart sc = m { S.mRam = S.mRam m U.// scPreset sc }
 where
  m = S.loadProgram (scProgram sc) S.newMachine

-- | Run the core and the model side by side. 'Right' carries the number
-- of instructions compared and the final model state.
lockstep :: Scenario -> Either String (Int, S.Machine)
lockstep sc = do
  let ins  = inputs sc
      n    = length ins
      outs = C.simulateN @C.System n (chip8Core (C.blockRam fontRam)) ins
      q    = S.quirksFromConfig (scConfig sc)
      evs  = [ e | Just e <- map coEvent outs ]
  (count, m) <- replay q (specStart sc) (0 :: Int) (0 :: Int) evs
  -- coFbRow at cycle c is the row addressed at cycle c - 1
  let rows = [ (fromIntegral ((c - 1) `mod` 32) :: Int, coFbRow o)
             | (c, o) <- zip [0 :: Int ..] outs, c >= n - 32 ]
      bad  = [ (r, w, specRow m r) | (r, w) <- rows, w /= specRow m r ]
  case bad of
    [] -> Right (count, m)
    (r, w, e) : _ -> Left ("framebuffer row " <> show r <> ": core " <> show w <> ", model " <> show e)
 where
  replay _ m k _ [] = Right (k, m)
  replay q m k i (e : es) = case e of
    Ticked -> replay q (S.tick60 m) k (i + 1) es
    Retired keys arch -> do
      let m' = S.step q (fromIntegral keys) m
      case archDiff arch m' of
        Nothing -> replay q m' (k + 1) (i + 1) es
        Just d  -> Left ("event " <> show i <> " (instruction " <> show k <> ", at PC "
                         <> show (S.mPc m) <> ", opcode " <> show (opAt m) <> "): " <> d)
  opAt m = let pc = fromIntegral (S.mPc m) in
    (fromIntegral (S.mRam m U.! pc) `shiftL` 8 .|. fromIntegral (S.mRam m U.! ((pc + 1) .&. 0xFFF)) :: Int)

specRow :: S.Machine -> Int -> C.BitVector 64
specRow m r = foldl (\acc x -> acc `shiftL` 1 .|. (if S.pixelAt m x r then 1 else 0)) 0 [0 .. 63]

-- | Differences between the core's state and the model's, if any.
archDiff :: Arch -> S.Machine -> Maybe String
archDiff a m = case filter (not . fst) checks of
  [] -> Nothing
  _  -> Just (unwords [ name | (False, name) <- map (\(ok, nm) -> (ok, nm)) checks ]
              <> "\n  core:  " <> show a <> "\n  model: " <> show (modelView m))
 where
  checks =
    [ (fromIntegral (aPc a) == S.mPc m, "PC")
    , (fromIntegral (aI a) == S.mI m, "I")
    , (map fromIntegral (C.toList (aV a)) == U.toList (S.mV m), "V")
    , (reverse (map fromIntegral (take (fromEnum (aSp a)) (C.toList (aStack a)))) == S.mStack m, "stack")
    , (fromIntegral (aDelay a) == S.mDelay m, "delay")
    , (fromIntegral (aSound a) == S.mSound m, "sound")
    , (fromIntegral (aRng a) == S.mRng m, "rng")
    , (vwait (aVblank a) == S.mVblank m, "vblank")
    , (keyWait (aKeyWait a) == S.mKeyWait m, "keywait")
    , (aHalted a == isJust (S.mHalt m), "halt")
    ]
  vwait VIdle     = S.VblankIdle
  vwait VWaiting  = S.VblankWaiting
  vwait VReleased = S.VblankReleased
  keyWait Nothing  = S.KeyWaitNone
  keyWait (Just k) = S.KeyWaitHeld (fromIntegral k)
  modelView mm = (S.mPc mm, S.mI mm, U.toList (S.mV mm), S.mStack mm, S.mDelay mm, S.mSound mm
                 , S.mRng mm, S.mVblank mm, S.mKeyWait mm, S.mHalt mm)

-- * Random programs

genScenario :: Gen Scenario
genScenario = do
  len <- Gen.int (Range.linear 1 48)
  ops <- Gen.list (Range.singleton len) (genOp len)
  cfg <- Gen.word8 (Range.constant 0 31)
  per <- Gen.int (Range.linear 20 300)
  ks  <- Gen.list (Range.linear 1 6) genKeys
  pure Scenario
    { scProgram = concatMap (\w -> [fromIntegral (w `shiftR` 8), fromIntegral w]) ops
    , scPreset  = []
    , scConfig  = cfg
    , scPeriod  = per
    , scKeys    = ks
    , scCycles  = 2500
    }

genKeys :: Gen Word16
genKeys = Gen.frequency
  [ (3, pure 0)
  , (3, bit <$> Gen.int (Range.constant 0 15))
  , (1, Gen.word16 Range.constantBounded) ]

-- | One instruction word for a program of @len@ instructions: jumps and
-- calls land inside the program, I points at the font or at scratch RAM.
genOp :: Int -> Gen Word16
genOp len = fromIntegral . encode <$> Gen.frequency
  [ (6, LdImm <$> reg <*> byte)
  , (4, AddImm <$> reg <*> byte)
  , (8, Alu <$> Gen.enumBounded <*> reg <*> reg)
  , (3, SeImm <$> reg <*> byte), (3, SneImm <$> reg <*> byte)
  , (2, SeReg <$> reg <*> reg), (2, SneReg <$> reg <*> reg)
  , (3, Jp <$> target), (2, Call <$> target), (2, pure Ret)
  , (1, JpOff <$> target)
  , (4, LdI <$> iAddr), (2, AddI <$> reg)
  , (3, Rnd <$> reg <*> byte)
  , (4, Drw <$> reg <*> reg <*> Gen.enumBounded), (1, pure Cls)
  , (2, Skp <$> reg), (2, Sknp <$> reg)
  , (2, LdVxDt <$> reg), (2, LdDtVx <$> reg), (1, LdStVx <$> reg)
  , (2, LdKey <$> reg)
  , (2, LdFont <$> reg), (2, Bcd <$> reg)
  , (2, Store <$> reg), (2, Load <$> reg)
  , (1, Illegal <$> Gen.element [0x0123, 0x5121, 0x812F, 0xE1FF, 0xF1FF])
  ]
 where
  reg    = Gen.enumBounded
  byte   = Gen.enumBounded
  target = Gen.element [ fromIntegral (0x200 + 2 * k) | k <- [0 .. len - 1] ]
  iAddr  = Gen.choice [ Gen.integral (Range.constant 0x050 0x09F)
                      , Gen.integral (Range.constant 0x300 0xFF0) ]

-- * Test ROMs

romDir :: FilePath
romDir = "../../roms/chip8/timendus"

romScenario :: FilePath -> [(Int, Word8)] -> [Word16] -> Int -> IO Scenario
romScenario name preset keys frames = do
  let p = romDir </> name
  ok <- doesFileExist p
  bytes <- if ok then BS.readFile p
                 else throwIO (ErrorCall (p <> " missing: run tools/fetch-roms.sh"))
  pure Scenario
    { scProgram = BS.unpack bytes, scPreset = preset, scConfig = 0
    , scPeriod = period, scKeys = keys, scCycles = frames * period }
 where
  period = 1000

-- | Lockstep on a ROM; optionally the final screen must also equal a
-- golden image of the reference model (layer 0).
romTest :: String -> FilePath -> [(Int, Word8)] -> [Word16] -> Int -> Maybe FilePath -> TestTree
romTest title rom preset keys frames golden = testCase title $ do
  sc <- romScenario rom preset keys frames
  case lockstep sc of
    Left err -> assertFailure err
    Right (k, m) -> do
      assertBool ("too few instructions: " <> show k) (k > 1000)
      S.mHalt m @?= Nothing
      case golden of
        Nothing -> pure ()
        Just g -> do
          img <- either error id <$> readFrame ("test/golden/spec" </> g <.> "png")
          let badPx = [ (x, y) | y <- [0 .. 31], x <- [0 .. 63]
                     , (pixelAt img x y == PixelRGB8 255 255 255) /= S.pixelAt m x y ]
          take 5 badPx @?= []

romTests :: TestTree
romTests = testGroup "Timendus ROMs (VIP quirks, 1000 cycles per frame)"
  [ romTest "1-chip8-logo" "1-chip8-logo.ch8" [] [0] 60 (Just "1-chip8-logo")
  , romTest "2-ibm-logo" "2-ibm-logo.ch8" [] [0] 20 (Just "2-ibm-logo")
  , romTest "3-corax+" "3-corax+.ch8" [] [0] 200 (Just "3-corax+")
  , romTest "4-flags" "4-flags.ch8" [] [0] 200 (Just "4-flags")
  , romTest "5-quirks (CHIP-8)" "5-quirks.ch8" [(0x1FF, 1)] [0] 300 Nothing
  , romTest "6-keypad Ex9E, keys 1 and 6" "6-keypad.ch8" [(0x1FF, 1)] [bit 1 .|. bit 6] 60 (Just "6-keypad-ex9e")
  , romTest "6-keypad Fx0A, key 5 pressed and released" "6-keypad.ch8" [(0x1FF, 3)]
      (replicate 30 0 ++ replicate 10 (bit 5) ++ replicate 200 0) 120 (Just "6-keypad-fx0a")
  , romTest "7-beep, B held" "7-beep.ch8" [] (replicate 100 0 ++ replicate 100 (bit 0xB)) 200 Nothing
  , romTest "Dino Run (CHIP-8 Archive), start and jumps on key 5" "../games/dinorun.ch8" []
      (replicate 30 0 ++ replicate 4 (bit 5) ++ concat (replicate 10 (replicate 6 (bit 5) ++ replicate 34 0))) 450 Nothing
  ]
