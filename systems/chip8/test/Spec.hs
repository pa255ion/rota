-- | Layer 0: the reference model, per opcode and on the Timendus test ROMs.
module Main (main) where

import Prelude

import Codec.Picture (PixelRGB8 (..), generateImage)
import Control.Exception (throwIO, ErrorCall (..))
import Data.Bits
import qualified Data.ByteString as BS
import qualified Data.Vector.Unboxed as U
import Data.Word
import System.Directory (doesFileExist)
import System.FilePath ((</>), (<.>))
import Test.Tasty
import Test.Tasty.Golden.Advanced (goldenTest)
import Test.Tasty.HUnit

import Chip8.Spec
import Chip8.SpecState
import Chip8.StateTable (tableSize, tHalted)
import Platform.Sim.Frame (Frame, frameDiff, readFrame, writeFrame)

main :: IO ()
main = defaultMain $ testGroup "chip8-spec"
  [ opcodeTests
  , quirkTests
  , machineTests
  , romTests
  , stateTests
  ]

-- * Save states (side track N, N.10)

-- | A ROM run for some frames, its state taken (table, RAM, framebuffer)
-- and loaded into a fresh machine: the same machine (but a halt's reason),
-- which then runs on alike.
stateTests :: TestTree
stateTests = testGroup "save states, the direct path (side track N, N.10)" $
  [ testCase (rom <> " after " <> show n <> " frames") $ do
      bytes <- BS.readFile (romDir </> rom)
      let m0 = loadProgram (BS.unpack bytes) newMachine
          q = vipQuirks
          m1 = runFrames q 15 (const 0) n m0
          tb = specTable m1
          m2 = specLoad tb (specRam m1) (specFb m1)
          plain m = m { mHalt = (\_ -> HaltIllegal 0) <$> mHalt m }
      U.length tb @?= tableSize
      specTable m2 @?= tb
      plain m2 @?= plain m1
      plain (runFrames q 15 (const 0) 30 m2) @?= plain (runFrames q 15 (const 0) 30 m1)
  | (rom, n) <- [ ("1-chip8-logo.ch8", 10), ("3-corax+.ch8", 40), ("4-flags.ch8", 120), ("5-quirks.ch8", 20) ] ]
  <> [ testCase "a halted machine stays halted" $ do
         let m = (prog [0x0000]) { mHalt = Just HaltStackUnderflow }
         specTable m U.! tHalted @?= 1
         mHalt (specLoad (specTable m) (specRam m) (specFb m)) @?= Just (HaltIllegal 0) ]

-- * Helpers

-- | A machine with these opcodes at 0x200 and some register setup.
prog :: [Word16] -> Machine
prog ops = loadProgram (concatMap (\o -> [fromIntegral (o `shiftR` 8), fromIntegral o]) ops) newMachine

withRegs :: [(Int, Word8)] -> Machine -> Machine
withRegs rs m = m { mV = mV m U.// rs }

steps :: Quirks -> Int -> Machine -> Machine
steps q n m = iterate (step q 0) m !! n

stepsK :: Quirks -> Keys -> Int -> Machine -> Machine
stepsK q k n m = iterate (step q k) m !! n

vReg :: Machine -> Int -> Word8
vReg m x = mV m U.! x

vip :: Quirks
vip = vipQuirks

-- | Run @ops@ with registers @rs@ for one step; return (Vx result, VF).
alu :: Quirks -> Word16 -> [(Int, Word8)] -> Int -> (Word8, Word8)
alu q op rs x = let m = steps q 1 (withRegs rs (prog [op])) in (vReg m x, vReg m 0xF)

-- * Opcodes

opcodeTests :: TestTree
opcodeTests = testGroup "opcodes"
  [ testCase "00E0 clears the display" $ do
      let noWait = vip { qDisplayWait = False }
          m = steps noWait 3 (prog [0xA050, 0xD015, 0x00E0])
      U.or (mDisplay (steps noWait 2 (prog [0xA050, 0xD015]))) @? "drawn"
      U.or (mDisplay m) @?= False
  , testCase "2nnn / 00EE call and return" $ do
      let m = prog [0x2206, 0x6101, 0x1204, 0x60AA, 0x00EE]
      mPc (steps vip 1 m) @?= 0x206
      mStack (steps vip 1 m) @?= [0x202]
      -- 0x206: 60AA, 0x208: 00EE
      let m3 = steps vip 3 m
      (mPc m3, mStack m3, vReg m3 0) @?= (0x202, [], 0xAA)
  , testCase "1nnn jumps" $
      mPc (steps vip 1 (prog [0x1ABC])) @?= 0xABC
  , testCase "3xkk / 4xkk skip on equal / not equal" $ do
      mPc (steps vip 1 (withRegs [(3, 7)] (prog [0x3307]))) @?= 0x204
      mPc (steps vip 1 (withRegs [(3, 7)] (prog [0x3308]))) @?= 0x202
      mPc (steps vip 1 (withRegs [(3, 7)] (prog [0x4307]))) @?= 0x202
      mPc (steps vip 1 (withRegs [(3, 7)] (prog [0x4308]))) @?= 0x204
  , testCase "5xy0 / 9xy0 skip on registers equal / different" $ do
      let rs = [(1, 5), (2, 5), (3, 6)]
      mPc (steps vip 1 (withRegs rs (prog [0x5120]))) @?= 0x204
      mPc (steps vip 1 (withRegs rs (prog [0x5130]))) @?= 0x202
      mPc (steps vip 1 (withRegs rs (prog [0x9120]))) @?= 0x202
      mPc (steps vip 1 (withRegs rs (prog [0x9130]))) @?= 0x204
  , testCase "6xkk loads, 7xkk adds without touching VF" $ do
      let m = steps vip 3 (prog [0x6AF0, 0x7A20, 0x6F05])
      vReg m 0xA @?= 0x10
      vReg (steps vip 2 (withRegs [(0xF, 9)] (prog [0x6AF0, 0x7A20]))) 0xF @?= 9
  , testCase "8xy0 copies" $
      fst (alu vip 0x8120 [(2, 0x42)] 1) @?= 0x42
  , testCase "8xy1/2/3: OR, AND, XOR" $ do
      fst (alu vip 0x8121 [(1, 0x0F), (2, 0x31)] 1) @?= 0x3F
      fst (alu vip 0x8122 [(1, 0x0F), (2, 0x31)] 1) @?= 0x01
      fst (alu vip 0x8123 [(1, 0x0F), (2, 0x31)] 1) @?= 0x3E
  , testCase "8xy4: add, VF = carry, VF last" $ do
      alu vip 0x8124 [(1, 200), (2, 100)] 1 @?= (44, 1)
      alu vip 0x8124 [(1, 20), (2, 100)] 1 @?= (120, 0)
      -- x = F: the flag overwrites the sum
      snd (alu vip 0x8F14 [(1, 200), (0xF, 100)] 0xF) @?= 1
      -- y = F: the old VF is the operand
      alu vip 0x81F4 [(1, 200), (0xF, 100)] 1 @?= (44, 1)
  , testCase "8xy5 / 8xy7: subtract, VF = no borrow (1 when equal)" $ do
      alu vip 0x8125 [(1, 10), (2, 3)] 1 @?= (7, 1)
      alu vip 0x8125 [(1, 3), (2, 10)] 1 @?= (249, 0)
      alu vip 0x8125 [(1, 5), (2, 5)] 1 @?= (0, 1)
      alu vip 0x8127 [(1, 3), (2, 10)] 1 @?= (7, 1)
      alu vip 0x8127 [(1, 10), (2, 3)] 1 @?= (249, 0)
  , testCase "8xy6 / 8xyE: shift Vy (VIP), VF = shifted-out bit" $ do
      alu vip 0x8126 [(1, 0xFF), (2, 0x05)] 1 @?= (0x02, 1)
      alu vip 0x812E [(1, 0x00), (2, 0x81)] 1 @?= (0x02, 1)
      alu vip 0x812E [(1, 0x00), (2, 0x41)] 1 @?= (0x82, 0)
  , testCase "Annn sets I; Fx1E adds to I without touching VF" $ do
      let m = steps vip 2 (withRegs [(3, 0x10), (0xF, 7)] (prog [0xAFF8, 0xF31E]))
      mI m @?= 0x1008
      vReg m 0xF @?= 7
  , testCase "Bnnn jumps to V0 + nnn (VIP)" $
      mPc (steps vip 1 (withRegs [(0, 4), (3, 9)] (prog [0xB300]))) @?= 0x304
  , testCase "Cxkk: next LFSR value masked with kk" $ do
      let m = steps vip 1 (prog [0xC50F])
          r = rngNext rngSeed
      vReg m 5 @?= fromIntegral r .&. 0x0F
      mRng m @?= r
  , testCase "Dxyn: draws a font glyph, XORs, VF = collision" $ do
      let noWait = vip { qDisplayWait = False }
          m1 = steps noWait 2 (withRegs [(1, 3), (2, 4)] (prog [0xA050, 0xD125, 0xD125]))
          m2 = steps noWait 1 m1
      -- '0' glyph: F0 90 90 90 F0
      [ pixelAt m1 x 4 | x <- [3 .. 10] ] @?= replicate 4 True ++ replicate 4 False
      [ pixelAt m1 x 5 | x <- [3 .. 10] ] @?= [True, False, False, True] ++ replicate 4 False
      vReg m1 0xF @?= 0
      U.or (mDisplay m2) @?= False
      vReg m2 0xF @?= 1
  , testCase "Dxyn: start position wraps (Vx mod 64, Vy mod 32)" $ do
      let m = steps schipQuirks 2 (withRegs [(1, 64 + 2), (2, 32 + 1)] (prog [0xA050, 0xD121]))
      [ pixelAt m x 1 | x <- [2 .. 5] ] @?= replicate 4 True
  , testCase "Ex9E / ExA1 skip on key down / up" $ do
      let m = withRegs [(4, 0xB)] (prog [0xE49E])
      mPc (stepsK vip (bit 0xB) 1 m) @?= 0x204
      mPc (stepsK vip 0 1 m) @?= 0x202
      let u = withRegs [(4, 0xB)] (prog [0xE4A1])
      mPc (stepsK vip (bit 0xB) 1 u) @?= 0x202
      mPc (stepsK vip 0 1 u) @?= 0x204
  , testCase "Fx07 / Fx15 / Fx18: timers" $ do
      let m = steps vip 3 (withRegs [(1, 30)] (prog [0xF115, 0xF118, 0xF207]))
      (mDelay m, mSound m, vReg m 2) @?= (30, 30, 30)
  , testCase "Fx0A waits for press and release, lowest key, timers run" $ do
      let m0 = withRegs [(9, 0x77)] (prog [0xF30A]) { mDelay = 10 }
          held = stepsK vip (bit 6 .|. bit 0xC) 5 m0
          tickd = tick60 held
          done = stepsK vip 0 1 tickd
      mPc held @?= 0x200
      mKeyWait held @?= KeyWaitHeld 6
      mDelay tickd @?= 9
      mPc done @?= 0x202
      vReg done 3 @?= 6
      -- still held: keeps waiting
      mPc (stepsK vip (bit 6) 3 held) @?= 0x200
  , testCase "Fx29 points I at the glyph of Vx's low nibble" $
      mI (steps vip 1 (withRegs [(2, 0x1A)] (prog [0xF229]))) @?= fromIntegral (fontAddr + 5 * 0xA)
  , testCase "Fx33 stores BCD" $ do
      let m = steps vip 2 (withRegs [(7, 254)] (prog [0xA400, 0xF733]))
      map (\a -> mRam m U.! a) [0x400, 0x401, 0x402] @?= [2, 5, 4]
  , testCase "Fx55 / Fx65 store and load V0..Vx; I += x + 1 (VIP)" $ do
      let m = steps vip 2 (withRegs [(0, 1), (1, 2), (2, 3), (3, 4)] (prog [0xA400, 0xF255]))
      map (\a -> mRam m U.! a) [0x400 .. 0x403] @?= [1, 2, 3, 0]
      mI m @?= 0x403
      let ld = steps vip 2 ((loadProgram [0xA4, 0x00, 0xF1, 0x65] (m { mPc = 0x200, mV = U.replicate 16 0 })))
      (vReg ld 0, vReg ld 1, vReg ld 2, mI ld) @?= (1, 2, 0, 0x402)
  , testCase "illegal opcodes halt the machine" $ do
      sequence_
        [ assertEqual (show op) (Just (HaltIllegal op)) (mHalt (steps vip 1 (prog [op])))
        | op <- [0x0123, 0x5121, 0x9129, 0x8128, 0x812D, 0x812F, 0xE19F, 0xF1FF] ]
      let h = steps vip 1 (prog [0x0123])
      steps vip 5 h @?= h
  , testCase "stack: 16 levels, then overflow; return on empty underflows" $ do
      let m = prog [0x2200]   -- calls itself forever
      length (mStack (steps vip 16 m)) @?= 16
      mHalt (steps vip 17 m) @?= Just HaltStackOverflow
      mHalt (steps vip 1 (prog [0x00EE])) @?= Just HaltStackUnderflow
  , testCase "PC and memory wrap at 4 KiB" $ do
      let m = (prog []) { mPc = 0xFFE, mRam = mRam newMachine U.// [(0xFFE, 0x60), (0xFFF, 0x12)] }
      mPc (steps vip 1 m) @?= 0x000
      vReg (steps vip 1 m) 0 @?= 0x12
  ]

-- * Quirks

quirkTests :: TestTree
quirkTests = testGroup "quirks"
  [ testCase "VF reset: 8xy1/2/3 clear VF (VIP) or leave it" $ do
      sequence_ [ assertEqual (show op) 0 (snd (alu vip op [(1, 3), (2, 5), (0xF, 9)] 1)) | op <- [0x8121, 0x8122, 0x8123] ]
      sequence_ [ assertEqual (show op) 9 (snd (alu schipQuirks op [(1, 3), (2, 5), (0xF, 9)] 1)) | op <- [0x8121, 0x8122, 0x8123] ]
  , testCase "memory: Fx55 leaves I unchanged without the quirk" $
      mI (steps schipQuirks 2 (prog [0xA400, 0xF255])) @?= 0x400
  , testCase "shift: 8xy6 / 8xyE shift Vx without the quirk" $ do
      alu schipQuirks 0x8126 [(1, 0x07), (2, 0xF0)] 1 @?= (0x03, 1)
      alu schipQuirks 0x812E [(1, 0x81), (2, 0x00)] 1 @?= (0x02, 1)
  , testCase "jump: Bxnn jumps to Vx + xnn without the quirk" $
      mPc (steps schipQuirks 1 (withRegs [(0, 4), (3, 9)] (prog [0xB300]))) @?= 0x309
  , testCase "display wait: Dxyn waits for the next tick" $ do
      let m0 = withRegs [(1, 0), (2, 0)] (prog [0xA050, 0xD125])
          w  = steps vip 5 m0
      mPc w @?= 0x202
      mVblank w @?= VblankWaiting
      U.or (mDisplay w) @?= False
      let d = steps vip 1 (tick60 w)
      mPc d @?= 0x204
      mVblank d @?= VblankIdle
      U.or (mDisplay d) @?= True
      -- without the quirk it draws at once
      U.or (mDisplay (steps schipQuirks 2 m0)) @?= True
  , testCase "clipping: sprites clip at the edges, or wrap without it" $ do
      let m0 = withRegs [(1, 62), (2, 30)] (prog [0xA050, 0xD125])
          noWait = vip { qDisplayWait = False }
          c = steps noWait 2 m0
          w = steps noWait { qClipping = False } 2 m0
      length (filter id (U.toList (mDisplay c))) @?= 2 + 1  -- rows 30, 31 of the '0' glyph at x 62, 63
      pixelAt w 0 30 @?= True    -- wrapped horizontally
      pixelAt w 62 0 @?= True    -- wrapped vertically (row 2 of the glyph)
  , testCase "quirksFromConfig: 0 = VIP, 0x1F = SUPER-CHIP" $ do
      quirksFromConfig 0 @?= vipQuirks
      quirksFromConfig 0x1F @?= schipQuirks
  ]

-- * Machine

machineTests :: TestTree
machineTests = testGroup "machine"
  [ testCase "power-on: font at 0x050, PC 0x200" $ do
      take 80 (U.toList (U.drop fontAddr (mRam newMachine))) @?= font
      mPc newMachine @?= 0x200
  , testCase "LFSR has the maximal period 65535" $ do
      let xs = takeWhile (/= rngSeed) (drop 1 (iterate rngNext rngSeed))
      length xs @?= 65534
  , testCase "tick60 decrements timers down to 0" $ do
      let m = newMachine { mDelay = 2, mSound = 1 }
      (mDelay (tick60 m), mSound (tick60 m)) @?= (1, 0)
      (mDelay (tick60 (tick60 (tick60 m))), mSound (tick60 (tick60 m))) @?= (0, 0)
  ]

-- * Test ROMs

romDir :: FilePath
romDir = "../../roms/chip8/timendus"

loadRom :: FilePath -> IO [Word8]
loadRom name = do
  let p = romDir </> name
  ok <- doesFileExist p
  if ok then BS.unpack <$> BS.readFile p
        else throwIO (ErrorCall (p <> " missing: run tools/fetch-roms.sh"))

-- | Display as a 64x32 image, lit pixels white.
screen :: Machine -> Frame
screen m = generateImage (\x y -> if pixelAt m x y then PixelRGB8 255 255 255 else PixelRGB8 0 0 0) 64 32

-- | Golden test of the screen after running a ROM.
romGolden :: String -> FilePath -> IO Machine -> TestTree
romGolden name golden run =
  goldenTest name
    (either error id <$> readFrame path)
    (screen <$> run)
    (\g t -> pure (frameDiff g t))
    (writeFrame path)
 where
  path = "test/golden/spec" </> golden <.> "png"

-- | Instructions per frame for the ROM tests. With the VIP display wait a
-- sprite costs a whole frame, so screens of many sprites need a few hundred
-- frames whatever this is. (the hardware's fast mode is
-- about 167; the quirks test reports SLOW below about 100).
ipf :: Int
ipf = 200

-- | Run a ROM with a value preselected at 0x1FF (the menus of 5-quirks and
-- 6-keypad), VIP quirks, a key schedule, for @n@ frames.
runRom :: FilePath -> Maybe Word8 -> (Int -> Keys) -> Int -> IO Machine
runRom = runRomWith vipQuirks

runRomWith :: Quirks -> FilePath -> Maybe Word8 -> (Int -> Keys) -> Int -> IO Machine
runRomWith q name sel keys n = do
  bytes <- loadRom name
  let m0 = loadProgram bytes newMachine
      m1 = maybe m0 (\v -> m0 { mRam = mRam m0 U.// [(0x1FF, v)] }) sel
      m  = runFrames q ipf keys n m1
  case mHalt m of
    Just h  -> throwIO (ErrorCall (name <> " halted: " <> show h <> " at PC " <> show (mPc m)))
    Nothing -> pure m

romTests :: TestTree
romTests = testGroup "Timendus ROMs (screens checked by eye against the suite's README)"
  [ romGolden "1-chip8-logo" "1-chip8-logo" (runRom "1-chip8-logo.ch8" Nothing (const 0) 60)
  , romGolden "2-ibm-logo" "2-ibm-logo" (runRom "2-ibm-logo.ch8" Nothing (const 0) 60)
  , romGolden "3-corax+" "3-corax+" (runRom "3-corax+.ch8" Nothing (const 0) 300)
  , romGolden "4-flags" "4-flags" (runRom "4-flags.ch8" Nothing (const 0) 300)
  , romGolden "5-quirks, CHIP-8 selected" "5-quirks-chip8" (runRom "5-quirks.ch8" (Just 1) (const 0) 400)
  , romGolden "5-quirks, CHIP-8 selected, all five quirk switches on (SUPER-CHIP behaviour)"
      "5-quirks-chip8-sw1f" (runRomWith (quirksFromConfig 0x1F) "5-quirks.ch8" (Just 1) (const 0) 400)
  , romGolden "6-keypad, Ex9E with keys 1 and 6 held" "6-keypad-ex9e"
      (runRom "6-keypad.ch8" (Just 1) (const (bit 1 .|. bit 6)) 60)
  , romGolden "6-keypad, Fx0A: key 5 pressed and released" "6-keypad-fx0a"
      (runRom "6-keypad.ch8" (Just 3) (\f -> if f >= 30 && f < 40 then bit 5 else 0) 120)
  , testCase "7-beep: holding B sounds the buzzer, releasing it stops" $ do
      bytes <- loadRom "7-beep.ch8"
      let start = runFrames vipQuirks ipf (const 0) 600 (loadProgram bytes newMachine)
          held  = runFrames vipQuirks ipf (const (bit 0xB)) 30 start
          rel   = runFrames vipQuirks ipf (const 0) 30 held
      mHalt held @?= Nothing
      soundOn held @? "buzzer on while B is held"
      not (soundOn rel) @? "buzzer off after release"
  ]
