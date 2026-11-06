{-|
Simulation-only model of the menu text overlay (layer 3,
"Platform.Video.TextOverlay"): what a screen of character cells looks like,
pixel for pixel, and PS link scripts, the small text files the Verilator
harness plays on the PS link (@vga-sim --ps FILE@, @platform/sim/ps.zig@),
so that layer 4 can be compared with a golden made here.

Script lines (@#@ starts a comment; text runs to the end of the line, after
one space):

> text R C some text     -- characters from cell (R, C)
> inv R C some text      -- the same in inverse video
> bar R C some text      -- row R all inverse spaces, then the text from C
> ctrl N                 -- the control register (bit 0 overlay, bit 1 pause)
> hold 0|1               -- the hold line
> bytes ADDR HEX         -- link writes from ADDR (ignored here: not the overlay)
-}
module Platform.Sim.Overlay
  ( Cells
  , ScriptOp (..)
  , parseScript
  , scriptScreen
  , overlayPixel
  , overlayImage
  ) where

import Prelude
import Codec.Picture (generateImage)
import Data.Bits (testBit, (.&.))
import qualified Data.Map.Strict as M
import Data.Word (Word8)

import qualified Clash.Prelude as C

import Platform.Sim.Frame (Frame, rgbPixel)
import Platform.Video.Font8x8 (font8x8)
import Platform.Video.TextOverlay (overlayBg, overlayFg)
import Platform.Video.VgaTiming (Rgb444)

-- | Character cells (index @40 * row + col@), blank where missing.
type Cells = M.Map Int Word8

data ScriptOp
  = OpText Int Int Bool String  -- ^ row, col, inverse, text
  | OpBar Int Int String
  | OpCtrl Word8
  | OpHold Bool
  deriving (Show, Eq)

parseScript :: String -> Either String [ScriptOp]
parseScript = fmap concat . traverse line . zip [1 :: Int ..] . lines
 where
  line (n, l) = case words l of
    [] -> Right []
    (('#' : _) : _) -> Right []
    ("text" : _) -> pure <$> cell n l (\r c t -> OpText r c False t)
    ("inv" : _)  -> pure <$> cell n l (\r c t -> OpText r c True t)
    ("bar" : _)  -> pure <$> cell n l OpBar
    ["ctrl", v]  -> pure . OpCtrl <$> num n v
    ["hold", v]  -> pure . OpHold . (/= 0) <$> (num n v :: Either String Int)
    -- link writes elsewhere (a cartridge load): nothing on the overlay
    ["bytes", _, _] -> Right []
    -- time passing (side track C's DDR test): nothing either
    ["wait", _] -> Right []
    _ -> Left ("line " <> show n <> ": " <> l)
  cell n l k = case words l of
    (_ : r : c : _) -> do
      r' <- num n r
      c' <- num n c
      -- the text: after the third word and one space
      let rest = drop 1 (dropWord (dropWord (dropWord (dropWhile (== ' ') l))))
      if r' < 30 && c' < 40 then Right (k r' c' rest) else Left ("line " <> show n <> ": outside the screen")
    _ -> Left ("line " <> show n <> ": " <> l)
  dropWord = dropWhile (/= ' ') . dropWhile (== ' ')
  num :: Read a => Int -> String -> Either String a
  num n s = case reads s of
    [(v, "")] -> Right v
    _ -> Left ("line " <> show n <> ": not a number: " <> s)

-- | The cells and the control register after a script.
scriptScreen :: [ScriptOp] -> (Cells, Word8)
scriptScreen = foldl step (M.empty, 0)
 where
  step (m, ctl) op = case op of
    OpText r c inv t -> (put r c inv t m, ctl)
    OpBar r c t      -> (put r c True t (put r 0 True (replicate 40 ' ') m), ctl)
    OpCtrl v         -> (m, v)
    OpHold _         -> (m, ctl)
  put r c inv t m = foldl (\acc (i, ch) -> M.insert (r * 40 + i) (code inv ch) acc) m
                          (zip [c .. 39] t)
  code inv ch = fromIntegral (fromEnum ch .&. 0x7F) + (if inv then 0x80 else 0)

-- | The overlay's colour at visible pixel (x, y).
overlayPixel :: Cells -> Int -> Int -> Rgb444
overlayPixel cells x y = if on /= (ch >= 0x80) then overlayFg else overlayBg
 where
  ch = M.findWithDefault 0x20 ((y `div` 16) * 40 + x `div` 16) cells
  glyphRow = font8x8 C.!! (fromIntegral (ch .&. 0x7F) * 8 + (y `div` 2) `mod` 8 :: Int)
  on = testBit glyphRow ((x `div` 2) `mod` 8)

-- | The whole 640 × 480 picture while the overlay is shown.
overlayImage :: Cells -> Frame
overlayImage cells = generateImage (\x y -> rgbPixel (overlayPixel cells x y)) 640 480
