-- | Layer 3 (docs/TESTING.md): the display path in Clash simulation, alone
-- and together with the core running a test ROM, checked against the
-- layer 0 goldens (64x32, from the reference model).
module Main (main) where

import Prelude

import Codec.Picture (PixelRGB8 (..), generateImage, pixelAt)
import Control.Exception (ErrorCall (..), throwIO)
import Data.Bits
import qualified Data.ByteString as BS
import Data.Word
import System.Directory (doesFileExist)
import System.FilePath ((</>), (<.>))
import System.Environment (lookupEnv)
import System.IO.Unsafe (unsafePerformIO)
import qualified Clash.Explicit.Prelude as CE
import qualified Clash.Prelude as C
import Test.Tasty
import Test.Tasty.Golden.Advanced (goldenTest)
import Test.Tasty.HUnit

import Chip8.Core
import Chip8.Display
import Chip8.Loader
import Platform.Ps.Link (PsGpio (..), psGpio)
import Chip8.Peripherals (buttonsToKeys)
import Platform.Input.Sync (synchronise)
import Platform.Sim.Adv7511 (expectedWrites, hdmiFrame, registerImage)
import Platform.Sim.Frame
import Platform.Sim.Oled
import Platform.Sim.Overlay (overlayImage, parseScript, scriptScreen)
import Platform.Video.Oled
import Platform.Video.VgaTiming

-- | Test tiers (docs/TESTING.md, "Tiers and parallel runs"):
-- EMU_FULL_TESTS=1 (make test FULL=1) runs everything.
fullTier :: Bool
fullTier = unsafePerformIO ((== Just "1") <$> lookupEnv "EMU_FULL_TESTS")
{-# NOINLINE fullTier #-}

main :: IO ()
main = defaultMain $ testGroup "chip8-golden"
  [ testCase "div10 is exact for 0..639" $
      [ v | v <- [0 .. 639], div10 v /= v `div` 10 ] @?= []

  , testGroup "VGA scaler on a test framebuffer"
    [ testCase "frame = 10x10 blocks in rows 80..399, black letterbox" $ do
        let f = vgaFrames (pure . testRow) !! 0
        frameDiff f (renderVga testPixel) @?= Nothing
    ]

  , testGroup "OLED adapter + driver on a test framebuffer"
    [ testCase "stretch: OLED (x, y) shows CHIP-8 (x / 2, y)" $
        oledDiff OledStretch (oledPanel OledStretch (pure . testRow)) testPixel @?= []
    , testCase "1:1: left half 1:1, right half dark" $
        oledDiff OledOneToOne (oledPanel OledOneToOne (pure . testRow)) testPixel @?= []
    ]

  , testGroup "VGA goldens for layer 4, from the layer 0 goldens (scaled and letterboxed)"
    [ goldenTest ("test/golden/vga/" <> g <> ".png (--accept to update)")
        (either error id <$> readFrame ("test/golden/vga" </> g <.> "png"))
        (renderVga . goldenPixel <$> golden64 g)
        (\a b -> pure (frameDiff a b))
        (writeFrame ("test/golden/vga" </> g <.> "png"))
    | g <- ["1-chip8-logo", "3-corax+", "4-flags", "5-quirks-chip8", "5-quirks-chip8-sw1f"] ]

  , testGroup "HDMI goldens for layer 4, from the VGA goldens (Ycbcr422 + the ADV7511 model)"
    [ goldenTest ("test/golden/hdmi/" <> g <> ".png (--accept to update)")
        (either error id <$> readFrame ("test/golden/hdmi" </> g <.> "png"))
        (hdmiOf <$> (either error id <$> readFrame ("test/golden/vga" </> g <.> "png")))
        (\a b -> pure (frameDiff a b))
        (writeFrame ("test/golden/hdmi" </> g <.> "png"))
    | g <- ["1-chip8-logo", "2-ibm-logo", "3-corax+", "4-flags", "5-quirks-chip8", "5-quirks-chip8-sw1f"] ]

  , testGroup "menu overlay goldens for layer 4 (make sim SIM_PS=test/ps/menu.ps)"
    [ goldenTest ("test/golden/" <> d <> "/menu-overlay.png (--accept to update)")
        (either error id <$> readFrame ("test/golden" </> d </> "menu-overlay.png"))
        (conv <$> menuOverlay)
        (\a b -> pure (frameDiff a b))
        (writeFrame ("test/golden" </> d </> "menu-overlay.png"))
    | (d, conv) <- [("vga", id), ("hdmi", hdmiOf)] ]

  , testGroup "ROM loading from the PS (EMIO GPIO -> loader -> core)"
    [ testCase "ramWrite: main RAM only, and only while held" $ do
        ramWrite True (Just (0x000200, 0xAA)) @?= Just (0x200, 0xAA)
        ramWrite True (Just (0x000FFF, 0x55)) @?= Just (0xFFF, 0x55)
        ramWrite False (Just (0x000200, 0xAA)) @?= Nothing
        ramWrite True (Just (0x001000, 0xAA)) @?= Nothing
        ramWrite True (Just (0xF00000, 0x41)) @?= Nothing
        ramWrite True Nothing @?= Nothing
    , testCase "loader: link bytes while held become RAM writes" $ do
        let w d a st h = psGpio ((if h then C.bit 26 else 0) C..|. (fromIntegral (d :: Int) `C.shiftL` 16)
                                 C..|. (if a then C.bit 25 else 0) C..|. (if st then C.bit 24 else 0))
            ws = [ w 0 False False True
                 , w 0x00 True True True, w 0x01 True False True, w 0xFF True True True  -- address 0x0001FF
                 , w 0x55 False False True                                               -- -> 0x1FF
                 , w 0x66 False True True                                                -- -> 0x200
                 , w 0x77 False False False ]                                            -- not held: nothing
            outs = C.simulateN @C.System (length ws + 1) loader (take 1 ws ++ ws)
        filter (/= Nothing) outs @?= [Just (0x1FF, 0x55), Just (0x200, 0x66)]
    , testCase "a halted core, loaded with 2-ibm-logo over the GPIO lines, shows its golden" $ do
        g <- golden64 "2-ibm-logo"
        bytes <- loadRom "2-ibm-logo.ch8"
        let outs = romOverGpio bytes
            fb = finalFb outs
        [ (x, y) | x <- [0 .. 63], y <- [0 .. 31], fb x y /= goldenPixel g x y ] @?= []
    ]

  , testGroup "input path: buttons -> debounce -> key table -> core"
    [ testCase "6-keypad Fx0A: BTNU (key 2) pressed and released gives ALL GOOD" $ do
        g <- golden64 "6-keypad-fx0a"
        bytes <- loadRom "6-keypad.ch8"
        let btns f = if f >= 30 && f < 40 then btnU else noBtn
            outs = romButtons bytes [(0x1FF, 3)] btns 120
            fb   = finalFb outs
        [ (x, y) | x <- [0 .. 63], y <- [0 .. 31], fb x y /= goldenPixel g x y ] @?= []
    , testCase "7-beep: BTNC + BTNR (key B) held sounds the buzzer" $ do
        bytes <- loadRom "7-beep.ch8"
        -- B held for frames 200 to 259, released afterwards
        let btns f = if f >= 200 && f < 260 then C.zipWith (C..|.) btnC btnR else noBtn
            frames xs a b = take ((b - a) * period) (drop (a * period) (drop (length bytes) xs))
            held  = map coSound (romButtons bytes [] btns 300)
            alone = map coSound (romButtons bytes [] (const noBtn) 300)
        assertBool "on all the time while B is held" (and (frames held 230 260))
        assertBool "off after B is released" (not (and (frames held 270 300)))
        -- control: without B the ROM's SOS has gaps in the same window
        assertBool "SOS alone is not continuous" (not (and (frames alone 230 260)))
    ]

  , testGroup "core + display, 2-ibm-logo" $
    -- The whole top in Clash simulation for three VGA frames (minutes). The
    -- quick tier (docs/TESTING.md) leaves these two out: make ci checks
    -- the same golden from the generated Verilog (make sim, 2-ibm-logo).
    -- With FULL=1 they run, on one simulation.
    [ withResource ((!! 2) <$> romVga "2-ibm-logo.ch8") (const (pure ())) $ \frame2 -> testGroup "VGA frame 2"
        [ testCase "VGA frame 2 downscales to the layer 0 golden" $ do
            g <- golden64 "2-ibm-logo"
            f <- frame2
            frameDiff f (renderVga (goldenPixel g)) @?= Nothing
        , goldenTest "VGA frame 2 matches test/golden/vga/2-ibm-logo.png (--accept to update)"
            (either error id <$> readFrame vgaGolden)
            frame2
            (\a b -> pure (frameDiff a b))
            (writeFrame vgaGolden)
        ]
    | fullTier ] <>
    [ testCase "OLED panel (stretch and 1:1) shows the layer 0 golden" $ do
        g <- golden64 "2-ibm-logo"
        bytes <- loadRom "2-ibm-logo.ch8"
        sequence_
          [ oledDiff mode (romOled mode bytes) (goldenPixel g) @?= []
          | mode <- [OledStretch, OledOneToOne] ]
    ]
  ]
 where
  vgaGolden = "test/golden/vga/2-ibm-logo.png"

-- * Test framebuffer and expected pictures

-- | An asymmetric pattern: a frame around the edge, a diagonal, and
-- column/row-dependent bits.
testPixel :: Int -> Int -> Bool
testPixel x y = x == 0 || y == 0 || x == 63 || y == 31 || x == 2 * y || (x * 7 + y * 3) `mod` 11 == 0

testRow :: C.Index 32 -> C.BitVector 64
testRow r = rowOf (testPixel `flip` fromIntegral r)
 where
  rowOf p = foldl (\acc x -> acc `shiftL` 1 .|. (if p x then 1 else 0)) 0 [0 .. 63]

renderVga :: (Int -> Int -> Bool) -> Frame
renderVga px = generateImage f 640 480
 where
  f x y | y >= 80 && y < 400 && px (x `div` 10) ((y - 80) `div` 10) = rgbPixel amber
        | otherwise = PixelRGB8 0 0 0

goldenPixel :: Frame -> Int -> Int -> Bool
goldenPixel g x y = pixelAt g x y == PixelRGB8 255 255 255

-- | Pixels where the OLED panel RAM differs from the expected picture.
oledDiff :: OledMode -> Panel -> (Int -> Int -> Bool) -> [(Int, Int)]
oledDiff mode p px =
  [ (x, y) | x <- [0 .. 127], y <- [0 .. 31], panelPixel p x y /= want x y ]
 where
  want x y = case mode of
    OledStretch  -> px (x `div` 2) y
    OledOneToOne -> x < 64 && px x y

-- * Simulations

-- | VGA frames of the scaler alone, fed by a framebuffer function (one
-- cycle of read latency, like the core's video port).
vgaFrames :: (C.Index 32 -> Maybe (C.BitVector 64)) -> [Frame]
vgaFrames rowFn = decodeFrames (vgaSink vga640x480) (C.sample (C.withClockResetEnable @C.System CE.clockGen CE.resetGen CE.enableGen dut))
 where
  dut :: C.HiddenClockResetEnable dom => C.Signal dom VgaPins
  dut = pins
   where
    (addr, pins) = vgaChip8 amber (vgaTiming vga640x480) row
    row = C.register 0 (maybe 0 id . rowFn <$> addr)

-- | Short OLED timing so a whole frame fits in a short simulation.
tiny :: OledTiming
tiny = OledTiming { otShort = 5, otLong = 20, otHalfBit = 4 }

-- | The panel after the first full OLED frame.
panelOf :: [OledPins] -> Panel
panelOf pins = foldl panelFeed panelInit (takeFrame (spiBytes pins))
 where
  takeFrame bs = let (pre, rest) = break ((== 1) . sbDc) bs in pre ++ take 512 rest

oledPanel :: OledMode -> (C.Index 32 -> Maybe (C.BitVector 64)) -> Panel
oledPanel mode rowFn = panelOf (CE.sampleN 50000 (C.withClockResetEnable @C.System CE.clockGen CE.resetGen CE.enableGen dut))
 where
  dut :: C.HiddenClockResetEnable dom => C.Signal dom OledPins
  dut = pins
   where
    (addr, pins) = oledDriver tiny (pure oledConfigDefault) byte
    (fbAddr, byte) = oledAdapter (pure mode) addr row
    row = C.register 0 (maybe 0 id . rowFn <$> fbAddr)

romDir :: FilePath
romDir = "../../roms/chip8/timendus"

loadRom :: FilePath -> IO [Word8]
loadRom name = do
  let p = romDir </> name
  ok <- doesFileExist p
  if ok then BS.unpack <$> BS.readFile p
        else throwIO (ErrorCall (p <> " missing: run tools/fetch-roms.sh"))

-- | What the monitor shows over HDMI for a VGA frame (12-bit colours,
-- 17 x the DAC level in the PNG) once the ADV7511 is configured.
-- | The picture the menu script leaves on screen: the text overlay only,
-- since the script turns it on (and pauses the game).
menuOverlay :: IO Frame
menuOverlay = do
  ops <- either error id . parseScript <$> readFile "test/ps/menu.ps"
  let (cells, ctl) = scriptScreen ops
  if ctl .&. 1 == 0 then throwIO (ErrorCall "test/ps/menu.ps leaves the overlay off") else pure ()
  pure (overlayImage cells)

hdmiOf :: Frame -> Frame
hdmiOf f = hdmiFrame (registerImage expectedWrites) 640 480 colour
 where
  colour x y = let PixelRGB8 r g b = pixelAt f x y
               in fromRgb444 (Rgb444 (fromIntegral (r `div` 17)) (fromIntegral (g `div` 17)) (fromIntegral (b `div` 17)))

golden64 :: String -> IO Frame
golden64 g = either error id <$> readFrame ("test/golden/spec" </> g <.> "png")

-- | Core + VGA path running a ROM.
romVga :: FilePath -> IO [Frame]
romVga rom = do
  bytes <- loadRom rom
  let n = length bytes
      dut :: C.HiddenClockResetEnable dom => C.Signal dom VgaPins
      dut = pins
       where
        vga = vgaTiming vga640x480
        (addr, pins) = vgaChip8 amber vga (coFbRow <$> out)
        loading = C.fromList (replicate n True ++ repeat False)
        inp = mkIn <$> C.fromList (map Just (zip [0x200 ..] bytes) ++ repeat Nothing)
                   <*> loading <*> (vgaVBlankStart <$> vga) <*> addr
        mkIn w l vb a = chip8InIdle
          { ciRomWrite = fmap (\(ad, b) -> (ad, C.pack b)) w, ciRun = not l, ciConfig = 0x10
          , ciInstrTick = not l, ciTimerTick = vb, ciFbAddr = a }
        out = chip8Core (C.blockRam fontRam) inp
  pure (decodeFrames (vgaSink vga640x480) (C.sample (C.withClockResetEnable @C.System CE.clockGen CE.resetGen CE.enableGen dut)))

-- | Core + OLED path running a ROM: the panel after the first full OLED
-- frame that starts once the ROM has drawn (the driver's start-up takes
-- longer than the ROM needs with no display wait).
romOled :: OledMode -> [Word8] -> Panel
romOled mode bytes = panelOf (CE.sampleN 60000 (C.withClockResetEnable @C.System CE.clockGen CE.resetGen CE.enableGen dut))
 where
  n = length bytes
  dut :: C.HiddenClockResetEnable dom => C.Signal dom OledPins
  dut = pins
   where
    (addr, pins) = oledDriver slow (pure oledConfigDefault) byte
    (fbAddr, byte) = oledAdapter (pure mode) addr (coFbRow2 <$> out)
    loading = C.fromList (replicate n True ++ repeat False)
    inp = mkIn <$> C.fromList (map Just (zip [0x200 ..] bytes) ++ repeat Nothing) <*> loading <*> fbAddr
    mkIn w l a = chip8InIdle
      { ciRomWrite = fmap (\(ad, b) -> (ad, C.pack b)) w, ciRun = not l, ciConfig = 0x10
      , ciInstrTick = not l, ciFbAddr2 = a }
    out = chip8Core (C.blockRam fontRam) inp
  -- long enough start-up for the logo to be drawn before the first frame
  slow = tiny { otLong = 8000 }

-- * Input path

-- | Buttons as the board gives them: element 0 = BTNC, then D, L, R, U.
noBtn, btnC, btnR, btnU :: C.Vec 5 C.Bit
noBtn = C.repeat 0
btnC = C.replace (0 :: Int) 1 noBtn
btnR = C.replace (3 :: Int) 1 noBtn
btnU = C.replace (4 :: Int) 1 noBtn

-- | Clocks per frame (timer tick) in the input tests; an instruction every
-- 16 clocks, VIP quirks.
period :: Int
period = 2000

-- | Run a ROM with a button schedule per frame (debounce 50 clocks).
romButtons :: [Word8] -> [(Int, Word8)] -> (Int -> C.Vec 5 C.Bit) -> Int -> [Chip8Out]
romButtons bytes preset btns frames =
  CE.sampleN (n + frames * period)
    (C.withClockResetEnable @C.System CE.clockGen CE.resetGen CE.enableGen dut)
 where
  writes = zip [0x200 ..] bytes ++ preset
  n = length writes
  dut :: C.HiddenClockResetEnable dom => C.Signal dom Chip8Out
  dut = out
   where
    cyc = C.fromList [0 :: Int ..]
    keys = buttonsToKeys (C.SNat @50) (btnAt <$> cyc)
    btnAt c = if c < n then noBtn else btns ((c - n) `div` period)
    inp = mkIn <$> cyc <*> keys <*> fbAddr
    fbAddr = (\c -> fromIntegral (c `mod` 32)) <$> cyc
    mkIn c k a
      | c < n = let (ad, b) = writes !! c in chip8InIdle { ciRomWrite = Just (fromIntegral (ad :: Int), C.pack b) }
      | otherwise = chip8InIdle
          { ciRun = True, ciKeys = k, ciFbAddr = a
          , ciInstrTick = (c - n) `mod` 16 == 0
          , ciTimerTick = (c - n) `mod` period == period - 1 }
    out = chip8Core (C.blockRam fontRam) inp

-- | The framebuffer at the end of a run, from the last 33 cycles of the
-- video port (address = cycle mod 32, data one cycle later).
finalFb :: [Chip8Out] -> Int -> Int -> Bool
finalFb outs x y = testBit (rowAt y) (63 - x)
 where
  total = length outs
  rowAt r = case [ coFbRow o | (c, o) <- zip [0 :: Int ..] outs, c >= total - 33, (c - 1) `mod` 32 == r ] of
    w : _ -> w
    []    -> error "run too short"

-- * ROM loading

-- | The 32-bit EMIO GPIO word stream the relay produces for a load: hold,
-- address 0x200 (two address bytes), the ROM, release; each byte as data
-- first, then 50 clocks later a strobe toggle, then 50 more (the relay
-- waits about 2 us at each step, about 50 pixel clocks).
gpioLoad :: [Word8] -> [C.BitVector 32]
gpioLoad bytes = concat (zipWith byteWords [0 :: Int ..] items) ++ replicate 200 0
 where
  hold = C.bit 26
  items = [(0x02, True), (0x00, True)] ++ [ (b, False) | b <- bytes ]
  word d a st = hold C..|. (fromIntegral d `C.shiftL` 16) C..|. (if a then C.bit 25 else 0)
                C..|. (if st then C.bit 24 else 0)
  -- before byte i the strobe has toggled i times, so its level is odd i
  byteWords i (d, a) = replicate 50 (word d a (odd i)) ++ replicate 50 (word d a (even i))

-- | Core (empty RAM: it halts on opcode 0000 at once) + loader, fed the
-- load sequence after 2000 clocks, then run with no display wait.
romOverGpio :: [Word8] -> [Chip8Out]
romOverGpio bytes =
  CE.sampleN total (C.withClockResetEnable @C.System CE.clockGen CE.resetGen CE.enableGen dut)
 where
  ws = replicate 2000 0 ++ gpioLoad bytes
  total = length ws + 60000
  dut :: C.HiddenClockResetEnable dom => C.Signal dom Chip8Out
  dut = out
   where
    cyc = C.fromList [0 :: Int ..]
    ps = psGpio <$> synchronise 0 (C.fromList (ws ++ repeat 0))
    load = loader ps
    inp = mkIn <$> cyc <*> ps <*> load
    mkIn c g w = chip8InIdle
      { ciRun = not (pgHold g), ciReset = pgHold g, ciRomWrite = w
      , ciConfig = 0x10, ciInstrTick = c `mod` 32 == 0, ciFbAddr = fromIntegral (c `mod` 32) }
    out = chip8Core (C.blockRam fontRam) inp
