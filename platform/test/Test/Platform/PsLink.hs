module Test.Platform.PsLink (tests) where

import Prelude
import qualified Data.List as L
import qualified Data.Map.Strict as M

import qualified Clash.Prelude as C
import qualified Clash.Explicit.Prelude as CE
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog

import Platform.Ps.Link
import Platform.Sim.Overlay
import Platform.Video.TextOverlay
import Platform.Video.VgaTiming

tests :: TestTree
tests = testGroup "PS link and text overlay"
  [ testGroup "link"
    [ testCase "address bytes shift in, data bytes write and advance, no edge: nothing" $ do
        let g d a st = PsGpio 0 d st a False
            steps = [ g 0 False False
                    , g 0xF0 True True, g 0x10 True False, g 0x00 True True  -- address 0xF01000
                    , g 0x03 False False                                     -- write 0xF01000
                    , g 0x04 False False                                     -- no edge
                    , g 0x05 False True                                      -- write 0xF01001
                    , g 0x00 True False, g 0x02 True True, g 0x00 True False -- address 0x000200
                    , (g 0xAA False True) { pgHold = True } ]                -- write while held, too
        runLink steps @?= [ Nothing, Nothing, Nothing, Nothing, Just (0xF01000, 0x03), Nothing
                          , Just (0xF01001, 0x05), Nothing, Nothing, Nothing, Just (0x000200, 0xAA) ]
    , testProperty "a byte stream as the PS sends it arrives as the same writes" propStream
    , testCase "psGpio decodes the documented bits" $ do
        psGpio 0x07AB1234 @?= PsGpio 0x1234 0xAB True True True
        psGpio 0x00000000 @?= PsGpio 0 0 False False False
    ]
  , testGroup "control register and PL -> PS word"
    [ testCase "only the control address changes it" $ do
        let cs = scanl psControlStep psControlInit
                   [Just (controlAddr, 0x03), Just (controlAddr + 1, 0x00), Nothing, Just (controlAddr, 0x02)]
        cs @?= [psControlInit, PsControl True True, PsControl True True, PsControl True True, PsControl False True]
    , testCase "overlayWrite takes exactly the 1200 cells" $ do
        overlayWrite (Just (overlayBase, 0x41)) @?= Just (0, 0x41)
        overlayWrite (Just (overlayBase + 1199, 0x42)) @?= Just (1199, 0x42)
        overlayWrite (Just (overlayBase + 1200, 0x42)) @?= Nothing
        overlayWrite (Just (overlayBase - 1, 0x42)) @?= Nothing
        overlayWrite Nothing @?= Nothing
    , testCase "psIn packs buttons, switches and the system ID" $ do
        psIn systemChip8 (True C.:> False C.:> False C.:> True C.:> True C.:> C.Nil) 0xA5 @?= 0x0001A519
        psIn systemGameBoy (C.repeat False) 0 @?= 0x00020000
    , testCase "the byte channel: byte in bits 24 to 31, toggle in bit 5, once per byte" $ do
        let ss = scanl psByteOutStep psByteOutInit [Just 0x50, Nothing, Just 0x61, Just 0x61, Nothing]
        map pbToggle ss @?= [False, True, True, False, True, True]
        map pbByte ss @?= [0, 0x50, 0x50, 0x61, 0x61, 0x61]
        psInWith systemGameBoy (C.repeat False) 0x01 (PsByteOut 0x50 True) @?= 0x50020120
        psInWith systemGameBoy (C.repeat False) 0x01 (PsByteOut 0x50 False) @?= 0x50020100
    ]
  , testGroup "settings registers (side track L)"
    [ testCase "neutral at reset: unity volume, 100 % speed" $
        psSettingsInit @?= PsSettings 10 4
    , testCase "each register decodes at its own address" $ do
        let ss = scanl psSettingsStep psSettingsInit
                   [Just (volumeAddr, 3), Just (speedAddr, 2), Just (speedAddr + 1, 7), Nothing, Just (volumeAddr, 0)]
        ss @?= [ psSettingsInit, PsSettings 3 4, PsSettings 3 2, PsSettings 3 2
               , PsSettings 3 2, PsSettings 0 2 ]
    , testCase "out-of-range bytes are clamped" $ do
        let at a d = psSettingsStep psSettingsInit (Just (a, d))
        psVolume (at volumeAddr 11) @?= 10
        psVolume (at volumeAddr 0xFF) @?= 10
        psSpeed (at speedAddr 0) @?= 2
        psSpeed (at speedAddr 1) @?= 2
        psSpeed (at speedAddr 9) @?= 8
        psSpeed (at speedAddr 0xFF) @?= 8
    , testProperty "control and settings never touch each other" $ property $ do
        ws <- forAll $ Gen.list (Range.linear 0 40) $ (,)
                <$> Gen.element [controlAddr, volumeAddr, speedAddr, controlAddr + 3, overlayBase]
                <*> Gen.integral (Range.constant 0 255)
        let ctl = L.foldl' psControlStep psControlInit . map Just
            set = L.foldl' psSettingsStep psSettingsInit . map Just
            isCtl (a, _) = a == controlAddr
            isSet (a, _) = a == volumeAddr || a == speedAddr
        ctl ws === ctl (filter isCtl ws)
        set ws === set (filter isSet ws)
    ]
  , testGroup "text overlay"
    [ testCase "cellIndex: 40 cells per row of 16 pixels" $ do
        cellIndex 0 0 @?= 0
        cellIndex 15 15 @?= 0
        cellIndex 16 0 @?= 1
        cellIndex 639 0 @?= 39
        cellIndex 0 16 @?= 40
        cellIndex 639 479 @?= 1199
        cellIndex 700 500 @?= 0
    , testCase "shown: the text over the picture, pixel for pixel (first 100 lines)" overlayFrame
    , testCase "PS link scripts (layer 4 --ps) parse into the cells they write" $ do
        let ops = either error id (parseScript "# menu\nbar 0 1 Title\ntext 2 3 a b\ninv 3 0 X\nctrl 3\nhold 1\n")
            (cells, ctl) = scriptScreen ops
        ctl @?= 3
        M.lookup 0 cells @?= Just 0xA0
        M.lookup 1 cells @?= Just (0x80 + 0x54)
        M.lookup 39 cells @?= Just 0xA0
        [ M.lookup (80 + 3 + i) cells | i <- [0 .. 2] ] @?= map Just [0x61, 0x20, 0x62]
        M.lookup 120 cells @?= Just (0x80 + 0x58)
        M.lookup 40 cells @?= Nothing
        either (const True) (const False) (parseScript "text 30 0 x") @? "row 30 is outside"
        either (const True) (const False) (parseScript "frob 1") @? "unknown op"
    ]
  ]

runLink :: [PsGpio] -> [Maybe (LinkAddr, C.BitVector 8)]
runLink = snd . L.mapAccumL linkStep linkInit

-- | What the PS does for one write: three address bytes, then the data
-- bytes; each byte as one GPIO word with the strobe toggled.
psWords :: LinkAddr -> [C.BitVector 8] -> Bool -> ([PsGpio], Bool)
psWords a ds st0 = (ws, stEnd)
 where
  items = [ (fromIntegral (a `C.shiftR` s), True) | s <- [16, 8, 0] ] ++ [ (d, False) | d <- ds ]
  sts = drop 1 (iterate not st0)
  ws = concat [ [PsGpio 0 d (not st) isA False, PsGpio 0 d st isA False] | ((d, isA), st) <- zip items sts ]
  stEnd = sts !! (length items - 1)

propStream :: Property
propStream = property $ do
  blocks <- forAll $ Gen.list (Range.linear 1 6)
    ((,) <$> (fromIntegral <$> Gen.int (Range.linear 0 0xFFFFFF))
         <*> Gen.list (Range.linear 0 8) (fromIntegral <$> Gen.int (Range.linear 0 255)))
  let go _ [] = []
      go st ((a, ds) : rest) = let (ws, st') = psWords a ds st in ws ++ go st' rest
      got = [ w | Just w <- runLink (PsGpio 0 0 False False False : go False blocks) ]
      want = concat [ zip (iterate (+ 1) a) ds | (a, ds) <- blocks ]
  got === want

-- | A known screen: a few rows of text, an inverse word, shown for the
-- first 64 lines and hidden after (then the picture passes through).
overlayFrame :: Assertion
overlayFrame = do
  let texts = [ (2, 1, "Hello, overlay!", False), (3, 0, "~{|}`_^", False)
              , (4, 3, "CURSOR", True), (29, 39, "Z", False) ]
      cells = M.fromList [ (r * 40 + c + i, fromIntegral (fromEnum ch) + (if inv then 0x80 else 0))
                         | (r, c, s, inv) <- texts, (i, ch) <- zip [0 ..] s ] :: Cells
      writes = [ Just (fromIntegral k, fromIntegral v) | (k, v) <- M.toList cells ]
      total = 100 * 800
      ws = (Nothing : writes) ++ repeat Nothing
      dut :: C.HiddenClockResetEnable dom => C.Signal dom ((VgaOut, Rgb888, Bool), (VgaOut, Rgb888))
      dut = C.bundle (C.bundle (timing, rgb, shown), C.bundle out)
       where
        timing = vgaTiming vga640x480
        rgb = (\o -> fromRgb444 (Rgb444 (fromIntegral (vgaX o)) (fromIntegral (vgaY o)) 7)) <$> timing
        shown = (\o -> vgaY o < 64) <$> timing
        out = textOverlay (C.fromList ws) shown (timing, rgb)
      samples = CE.sampleN total (C.withClockResetEnable @C.System CE.clockGen CE.resetGen CE.enableGen dut)
      ins = map fst samples
      outs = map snd samples
      expect (o, c, s)
        | s && vgaActive o = (o, fromRgb444 (overlayPixel cells (fromIntegral (vgaX o)) (fromIntegral (vgaY o))))
        | otherwise = (o, c)
      bad = [ (t, i, g) | (t, i, g) <- zip3 [overlayLatency + 2 ..] (drop 2 ins) (drop (overlayLatency + 2) outs)
                        , expect i /= g ]
  -- some text is actually lit, and it all matches
  assertBool "no lit text pixels" (any (\(_, c) -> c == fromRgb444 overlayFg) (take 64000 outs))
  take 3 bad @?= []
