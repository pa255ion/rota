{-|
Unit tests of the VDP's drawing rules and the PSG, driven through their
ports (docs/systems/gamegear.md; MacDonald's VDP notes, SMS Power's
"SN76489"). Each VDP test sets up VRAM, CRAM and registers, runs the VDP
over a frame and looks at pixels of the 160 × 144 window (window pixel
(0, 0) is screen pixel (48, 24)).
-}
module Units
  ( unitTests
  ) where

import Prelude

import Control.Monad (forM_, replicateM_)
import Control.Monad.ST (ST, runST)
import Data.Bits
import qualified Data.Vector.Unboxed as U
import Data.Word
import Test.Tasty
import Test.Tasty.HUnit

import Gg.Spec.Psg
import Gg.Spec.Vdp

-- * VDP helpers

setReg :: Vdp s -> Int -> Word8 -> ST s ()
setReg v r x = vdpControlWrite v x >> vdpControlWrite v (0x80 .|. fromIntegral r)

vram :: Vdp s -> Int -> [Word8] -> ST s ()
vram v a bs = do
  vdpControlWrite v (fromIntegral a)
  vdpControlWrite v (0x40 .|. fromIntegral (a `shiftR` 8))
  mapM_ (vdpDataWrite v) bs

-- | CRAM entry @i@ := @c@ (----BBBBGGGGRRRR).
cram :: Vdp s -> Int -> Word16 -> ST s ()
cram v i c = do
  vdpControlWrite v (fromIntegral (i * 2))
  vdpControlWrite v 0xC0
  vdpDataWrite v (fromIntegral c)
  vdpDataWrite v (fromIntegral (c `shiftR` 8))

-- | Name table at 3800h; screen cell (cx, cy) := entry.
cell :: Vdp s -> Int -> Int -> Int -> ST s ()
cell v cx cy e = vram v (0x3800 + (cy * 32 + cx) * 2) [fromIntegral e, fromIntegral (e `shiftR` 8)]

-- | Colours: 0 black, 1 green, 2 red, 17 blue (sprite/bg palette 1),
-- 16 + 3 = 19 white (backdrop with R7 = 3). Tile 1: only its top-left
-- pixel is colour 1. Tile 2: every pixel colour 2. Display on, Mode 4.
setup :: Vdp s -> ST s ()
setup v = do
  cram v 0 0x000
  cram v 1 0x0F0
  cram v 2 0x00F
  cram v 17 0xF00
  cram v 18 0x0FF
  cram v 19 0xFFF
  vram v 0x20 ([0x80, 0, 0, 0] <> replicate 28 0)
  vram v 0x40 (concat (replicate 8 [0, 0xFF, 0, 0]))
  setReg v 0 0x04
  setReg v 1 0x40
  setReg v 2 0xFF
  setReg v 5 0x7F -- SAT at 3F00h
  setReg v 6 0xFB
  setReg v 7 0x03
  vram v 0x3F00 [0xD0] -- no sprites

-- | Run to the end of frame @n@ (from the VDP's current dot).
frameAfter :: Vdp s -> ST s (U.Vector Word16)
frameAfter v = do
  d <- vdpDot v
  vdpSync v ((d `div` dotsPerFrame + 2) * dotsPerFrame)
  vdpFrame v

px :: U.Vector Word16 -> Int -> Int -> Word16
px fb x y = fb U.! (y * ggWidth + x)

render :: (forall s. Vdp s -> ST s ()) -> U.Vector Word16
render prep = runST $ do
  v <- newVdp
  setup v
  prep v
  frameAfter v

-- Window (0, 0) is screen (48, 24): cell (6, 3).
originCell :: (Int, Int)
originCell = (6, 3)

unitTests :: TestTree
unitTests = testGroup "units"
  [ testGroup "VDP drawing"
    [ testCase "display off: the backdrop (CRAM 16 + R7) everywhere" $ do
        let fb = render (\v -> setReg v 1 0x00)
        U.all (== 0xFFF) fb @?= True
    , testCase "a tile's pixel, the rest colour 0" $ do
        let fb = render (\v -> uncurry (cell v) originCell 1)
        (px fb 0 0, px fb 1 0, px fb 0 1) @?= (0x0F0, 0x000, 0x000)
    , testCase "horizontal flip (bit 9)" $ do
        let fb = render (\v -> uncurry (cell v) originCell (1 .|. 0x200))
        (px fb 0 0, px fb 7 0) @?= (0x000, 0x0F0)
    , testCase "vertical flip (bit 10)" $ do
        let fb = render (\v -> uncurry (cell v) originCell (1 .|. 0x400))
        (px fb 0 0, px fb 0 7) @?= (0x000, 0x0F0)
    , testCase "palette select (bit 11): colours 16-31" $ do
        let fb = render (\v -> uncurry (cell v) originCell (1 .|. 0x800))
        px fb 0 0 @?= 0xF00
    , testCase "horizontal scroll moves the background right" $ do
        let fb = render (\v -> uncurry (cell v) originCell 1 >> setReg v 8 1)
        (px fb 0 0, px fb 1 0) @?= (0x000, 0x0F0)
    , testCase "vertical scroll moves it up; R0 bit 7 locks columns 192 and up" $ do
        let fb = render $ \v -> do
              cell v 6 4 1   -- one row below the origin
              cell v 24 4 1  -- column 192
              setReg v 9 8
              setReg v 0 0x84
        (px fb 0 0, px fb 144 8, px fb 144 0) @?= (0x0F0, 0x0F0, 0x000)
    , testCase "vertical scroll wraps at 224 lines" $ do
        let fb = render (\v -> uncurry (cell v) originCell 1 >> setReg v 9 (224 - 8 + 0))
        -- the origin row now shows name-table row 2: empty; row 3 is 8 lines down
        (px fb 0 0, px fb 0 8) @?= (0x000, 0x0F0)
    , testCase "a sprite over the background; the priority bit puts the background in front" $ do
        let sprite v = do
              vram v 0x3F00 [24 + 24 - 1, 0xD0]   -- Y: shown from line 48 (window line 24)
              vram v 0x3F80 [48 + 8, 2]           -- X: window column 8, tile 2
            fb1 = render (\v -> sprite v >> cell v 7 6 2)
            fb2 = render (\v -> sprite v >> cell v 7 6 (2 .|. 0x1000))
        (px fb1 8 24, px fb2 8 24, px fb1 16 24) @?= (0x0FF, 0x00F, 0x000)
    , testCase "sprite zoom (R1 bit 0) doubles width and height" $ do
        let fb = render $ \v -> do
              vram v 0x3F00 [24 + 24 - 1, 0xD0]
              vram v 0x3F80 [48, 2]
              setReg v 1 0x41
        (px fb 15 24, px fb 16 24, px fb 0 39, px fb 0 40) @?= (0x0FF, 0x000, 0x0FF, 0x000)
    , testCase "overlapping sprites: the first in the table wins" $ do
        let fb = render $ \v -> do
              vram v 0x60 (concat (replicate 8 [0xFF, 0xFF, 0, 0]))  -- tile 3: colour 3
              cram v 19 0x0F0                                           -- sprite colour 3: green
              vram v 0x3F00 [24 + 24 - 1, 24 + 24 - 1, 0xD0]
              vram v 0x3F80 [48 + 8, 2, 48 + 12, 3]
        (px fb 8 24, px fb 12 24, px fb 16 24) @?= (0x0FF, 0x0FF, 0x0F0)
    , testCase "a sprite's X is fetched at the line's start: written later, it moves the next line" $ do
        let fb = runST $ do
              v <- newVdp
              setup v
              vram v 0x3F00 [24 + 24 - 1, 0xD0]   -- from window line 24
              vram v 0x3F80 [48 + 8, 2]
              d0 <- vdpDot v
              let frameStart = (d0 `div` dotsPerFrame + 1) * dotsPerFrame
              -- window line 26, after its sprite fetch (dots 0-15)
              vdpSync v (frameStart + (24 + 26) * dotsPerLine + 20)
              vram v 0x3F80 [48 + 40]
              vdpSync v (frameStart + dotsPerFrame)
              vdpFrame v
        (px fb 8 26, px fb 40 26, px fb 8 27, px fb 40 27) @?= (0x0FF, 0x000, 0x000, 0x0FF)
    , testCase "CRAM: an even byte alone changes nothing; the odd one commits both" $ do
        let (before, after') = runST $ do
              v <- newVdp
              cram v 5 0x123
              vdpControlWrite v 10
              vdpControlWrite v 0xC0
              vdpDataWrite v 0x56
              b <- (,) <$> vdpCram v 10 <*> vdpCram v 11
              vdpDataWrite v 0x04
              a <- (,) <$> vdpCram v 10 <*> vdpCram v 11
              pure (b, a)
        (before, after') @?= ((0x23, 0x01), (0x56, 0x04))
    , testCase "a CRAM write mid-line changes the pixels from its dot on" $ do
        let fb = runST $ do
              v <- newVdp
              setup v
              setReg v 1 0x00 -- the backdrop (CRAM 19) everywhere
              d0 <- vdpDot v
              let frameStart = (d0 `div` dotsPerFrame + 1) * dotsPerFrame
                  line = 24 + 10
                  -- window column 80 of window line 10
              vdpSync v (frameStart + line * dotsPerLine + pixelDot + 48 + 80)
              cram v 19 0x0F0
              vdpSync v (frameStart + dotsPerFrame)
              vdpFrame v
        (px fb 79 10, px fb 80 10, px fb 0 11, px fb 0 9) @?= (0xFFF, 0x0F0, 0x0F0, 0xFFF)
    , testCase "R8 written mid-line applies from the next line (latched at its start)" $ do
        let fb = runST $ do
              v <- newVdp
              setup v
              forM_ [3, 4] $ \cy -> cell v 6 cy 2  -- a red column at window x 0-7
              d0 <- vdpDot v
              let frameStart = (d0 `div` dotsPerFrame + 1) * dotsPerFrame
              vdpSync v (frameStart + (24 + 5) * dotsPerLine + 100)
              setReg v 8 8
              vdpSync v (frameStart + dotsPerFrame)
              vdpFrame v
        (px fb 0 5, px fb 8 5, px fb 0 6, px fb 8 6) @?= (0x00F, 0x000, 0x000, 0x00F)
    ]
  , testGroup "PSG"
    [ testCase "tone: period 254 flips every 254 clocks (440.4 Hz)" $ do
        let flips = runST $ do
              p <- newPsg
              psgWrite p 0x8E        -- tone 0, low 4 bits = E
              psgWrite p 0x0F        -- high 6 bits = 0F: period FEh = 254
              psgWrite p 0x90        -- volume 0 (full)
              let count n prev acc
                    | n == (0 :: Int) = pure acc
                    | otherwise = do
                        psgTick p
                        (l, _) <- psgSample p 0xFF
                        count (n - 1) (l > 0) (if (l > 0) /= prev then acc + 1 else acc)
              count (254 * 20) False (0 :: Int)
        flips @?= 20
    , testCase "white noise: the LFSR repeats after 57337 shifts" $
        noisePeriod 0x04 @?= 57337
    , testCase "periodic noise: 16 shifts" $
        noisePeriod 0x00 @?= 16
    , testCase "volume: 2 dB steps, 15 is silent" $ do
        (volumeTable 0, volumeTable 1, volumeTable 15) @?= (8191, 6506, 0)
    , testCase "a data byte writes the latched tone's high bits" $ do
        let regs = runST $ do
              p <- newPsg
              psgWrite p 0xA5        -- tone 1, low bits 5
              psgWrite p 0x3F        -- high bits 3F
              psgRegisters p
        take 2 regs @?= [0, 0x3F5]
    ]
  ]

-- | Shifts until the LFSR is back at 8000h, noise control @ctl@ (rate 0).
noisePeriod :: Word8 -> Int
noisePeriod ctl = runST $ do
  p <- newPsg
  psgWrite p (0xE0 .|. ctl)
  psgWrite p 0xF0 -- noise volume full
  let shifts = 0x10 * 2 -- PSG clocks per shift at rate 0
      go n = do
        replicateM_ shifts (psgTick p)
        s <- lfsrNow p
        if s == 0x8000 || n > 70000 then pure n else go (n + 1)
  go 1
 where
  lfsrNow = psgLfsr
