-- | Layer 4 for the demos (a development tool, PLAN.md side track J):
-- Verilator's HDMI frames (640 × 480, the picture scaled and letterboxed)
-- against the spec's screens as layer 3 wrote them (one byte a pixel,
-- frame after frame).
--
-- The HDMI output runs at 60 Hz and the machine a little slower, so now
-- and then a machine frame is shown twice: each HDMI frame must equal a
-- spec frame, in order, the same one as the last or a later one (at most
-- eight on; the jumps are listed). Every HDMI colour must stand for one spec value throughout,
-- and back.
--
-- > rota-compare <frames dir> <spec.bin> <width> <height> <scale> <first> <last>
--
-- (the Game Boy: 160 144 3; the NES: 256 240 2)
module Main (main) where

import Codec.Picture (PixelRGB8 (..), convertRGB8, pixelAt, readImage)
import Control.Monad (forM)
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as M
import Data.Word (Word8)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import Text.Printf (printf)

type Rgb = (Word8, Word8, Word8)

main :: IO ()
main = do
  [dir, specPath, wA, hA, zA, aA, bA] <- getArgs
  spec <- BS.readFile specPath
  let w = read wA :: Int
      h = read hA :: Int
      z = read zA :: Int
      from = read aA :: Int
      to = read bA :: Int
      x0 = (640 - z * w) `div` 2
      y0 = (480 - z * h) `div` 2
      nSpec = BS.length spec `div` (w * h)
      specFrame j = [ BS.index spec (j * w * h + i) | i <- [0 .. w * h - 1] ]
  frames <- forM [from .. to] $ \k -> do
    e <- readImage (dir <> "/" <> printf "frame_%04d.png" k)
    case e of
      Left err -> error err
      Right img -> let rgb = convertRGB8 img
                   in pure [ (\(PixelRGB8 r g b) -> (r, g, b)) (pixelAt rgb (x0 + z * x + z `div` 2) (y0 + z * y + z `div` 2)) | y <- [0 .. h - 1], x <- [0 .. w - 1] ]
  let -- does an HDMI frame equal spec frame j, with the colours seen so far?
      match :: (M.Map Rgb Word8, M.Map Word8 Rgb) -> [Rgb] -> Int -> Maybe (M.Map Rgb Word8, M.Map Word8 Rgb)
      match cm f j = go cm (zip f (specFrame j))
       where
        go m [] = Just m
        go (fw, bw) ((c, v) : rest) = case (M.lookup c fw, M.lookup v bw) of
          (Just v', _) | v' /= v -> Nothing
          (_, Just c') | c' /= c -> Nothing
          _ -> go (M.insert c v fw, M.insert v c bw) rest
      follow _ _ [] acc = Right (reverse acc)
      follow cm j (f : fs) acc =
        case [ (jj, cm') | jj <- [j .. min (nSpec - 1) (j + 8)], Just cm' <- [match cm f jj] ] of
          ((jj, cm') : _) -> follow cm' jj fs (jj : acc)
          [] -> Left (length acc, j)
      starts = [ j | j <- [0 .. nSpec - 1], Just _ <- [match (M.empty, M.empty) (head frames) j] ]
      tries = [ (s, follow (M.empty, M.empty) s frames []) | s <- starts ]
  case [ (s, js) | (s, Right js) <- tries ] of
    ((_, js) : _) -> printf "HDMI frames %d to %d each equal a spec frame, in order (spec %d to %d; %d shown twice; jumps over frames: %s)\n"
                       from to (head js) (last js) (length (filter id (zipWith (==) js (drop 1 js))))
                       (show [ (a, b) | (a, b) <- zip js (drop 1 js), b > a + 1 ])
    [] -> do
      let (n, j) = maximum ((0, 0) : [ r | (_, Left r) <- tries ])
          f = frames !! n
          diffs jj = [ (i `mod` w, i `div` w, c, v) | (i, c, v) <- zip3 [0 :: Int ..] f (specFrame jj) ]
      putStrLn ("no start fits; furthest: " <> show (n, j) <> " (frames matched, spec frame)")
      putStrLn ("colours there against the spec's next frames (HDMI colour, spec value, pixels): "
        <> show [ (jj, M.toList (M.fromListWith (+) [ ((c, v), 1 :: Int) | (_, _, c, v) <- diffs jj ])) | jj <- [j .. min (nSpec - 1) (j + 2)] ])
      exitFailure
