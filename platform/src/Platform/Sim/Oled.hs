{-|
Simulation-only model of the OLED side of "Platform.Video.Oled": an SPI
receiver that turns pin samples into the bytes an SSD1306 would see, and a
small model of the controller's display RAM, so tests can check what ends
up on the panel.

The panel model knows only what the driver uses: horizontal addressing
(0x20 0x00), column and page ranges (0x21, 0x22), segment remap (A0/A1),
COM scan direction (C0/C8), display on/off (AE/AF), and skips the
arguments of the other configuration commands (0x81, 0x8D, 0xD9, 0xDA). Any
other command is recorded in 'psUnknown'.
-}
module Platform.Sim.Oled
  ( SpiByte (..)
  , spiBytes
  , Panel (..)
  , panelInit
  , panelFeed
  , panelPixel
  , panelImage
  ) where

import Prelude

import Codec.Picture
import Data.Bits (testBit)
import qualified Data.Map.Strict as M

import qualified Clash.Prelude as C
import Platform.Video.Oled (OledPins (..))

-- | One received byte: clock of its first SCLK falling edge, D/C# and value.
data SpiByte = SpiByte
  { sbStart :: Int
  , sbDc    :: C.Bit
  , sbValue :: Integer
  } deriving stock (Show, Eq)

-- | Bytes as an SSD1306 receives them over 4-wire SPI with CS# held low:
-- SDIN is sampled on every rising SCLK edge, MSB first, and D/C# on the
-- 8th. A low pulse on RES# clears a partly received byte.
spiBytes :: [OledPins] -> [SpiByte]
spiBytes ps = go Nothing (0 :: Int) 0 (zip3 [0 ..] ps (drop 1 ps))
 where
  go _ _ _ [] = []
  go start k acc ((t, p0, p1) : rest)
    | opRes p1 == 0 = go Nothing 0 0 rest
    | opSclk p0 == 1 && opSclk p1 == 0 && k == 0 = go (Just (t + 1)) k acc rest
    | opSclk p0 == 0 && opSclk p1 == 1 =
        let acc' = acc * 2 + fromIntegral (opSdin p1)
        in if k == 7
             then SpiByte (maybe (-1) id start) (opDc p1) acc' : go Nothing 0 0 rest
             else go start (k + 1) acc' rest
    | otherwise = go start k acc rest

data Panel = Panel
  { psRam       :: M.Map (Int, Int) Integer  -- ^ (page, column) -> byte
  , psCol       :: Int
  , psPage      :: Int
  , psColRange  :: (Int, Int)
  , psPageRange :: (Int, Int)
  , psHorizontal :: Bool
  , psSegRemap  :: Bool
  , psComRemap  :: Bool
  , psOn        :: Bool
  , psPending   :: [Integer]  -- ^ command bytes waiting for their arguments
  , psUnknown   :: [Integer]
  , psDataBytes :: Int        -- ^ data bytes written so far
  } deriving stock (Show, Eq)

-- | Power-on state of the controller (page addressing, display off).
panelInit :: Panel
panelInit = Panel M.empty 0 0 (0, 127) (0, 7) False False False False [] [] 0

-- | Number of argument bytes after a command.
argCount :: Integer -> Int
argCount c
  | c `elem` [0x21, 0x22] = 2
  | c `elem` [0x20, 0x81, 0x8D, 0xD9, 0xDA, 0xD5, 0xA8, 0xD3, 0xDB] = 1
  | otherwise = 0

panelFeed :: Panel -> SpiByte -> Panel
panelFeed p (SpiByte _ dc v)
  | dc == 1   = writeData p v
  | otherwise = command p { psPending = psPending p ++ [v] }

command :: Panel -> Panel
command p = case psPending p of
  c : args | length args == argCount c -> run c args p { psPending = [] }
  _ -> p
 where
  run c args q = case (c, args) of
    (0x20, [m])    -> q { psHorizontal = m == 0 }
    (0x21, [a, b]) -> q { psColRange = (fromInteger a, fromInteger b), psCol = fromInteger a }
    (0x22, [a, b]) -> q { psPageRange = (fromInteger a, fromInteger b), psPage = fromInteger a }
    (0xA0, [])     -> q { psSegRemap = False }
    (0xA1, [])     -> q { psSegRemap = True }
    (0xC0, [])     -> q { psComRemap = False }
    (0xC8, [])     -> q { psComRemap = True }
    (0xAE, [])     -> q { psOn = False }
    (0xAF, [])     -> q { psOn = True }
    _ | c `elem` [0x81, 0x8D, 0xD9, 0xDA] -> q
      | otherwise -> q { psUnknown = psUnknown q ++ [c] }

-- | Horizontal addressing: column advances, wrapping into the next page of
-- the page range.
writeData :: Panel -> Integer -> Panel
writeData p v = p
  { psRam = M.insert (psPage p, psCol p) v (psRam p)
  , psCol = col', psPage = page', psDataBytes = psDataBytes p + 1 }
 where
  (c0, c1) = psColRange p
  (p0, p1) = psPageRange p
  (col', page')
    | psCol p < c1  = (psCol p + 1, psPage p)
    | psPage p < p1 = (c0, psPage p + 1)
    | otherwise     = (c0, p0)

-- | Pixel @(x, y)@ of the display RAM (bit @y mod 8@ of page @y div 8@).
panelPixel :: Panel -> Int -> Int -> Bool
panelPixel p x y = case M.lookup (y `div` 8, x) (psRam p) of
  Just b  -> testBit b (y `mod` 8)
  Nothing -> False

-- | The 128x32 display RAM as an image, each pixel @scale@ x @scale@,
-- lit pixels white. This is RAM order, not necessarily the panel's
-- physical orientation (that depends on the remap settings and mounting).
panelImage :: Int -> Panel -> Image PixelRGB8
panelImage scale p = generateImage px (128 * scale) (32 * scale)
 where
  px x y = if panelPixel p (x `div` scale) (y `div` scale)
             then PixelRGB8 255 255 255 else PixelRGB8 0 0 0
