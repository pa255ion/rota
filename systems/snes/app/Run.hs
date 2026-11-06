-- | snes-run ROM [--frames N] [--pad1 BITS] [--pad2 BITS] [--input FILE] [--shots DIR K] [--ipl FILE] [--wav FILE] [--png FILE] [--vga FILE] [--vga4 FILE]:
-- the reference model (Snes.Spec) headless. Runs N frames (default 60) or
-- until STP, then prints RESULT (WRAM 7E0000h, our test ROMs' verdict),
-- whether the CPU stopped, and BG1's tilemap as text (the test ROMs'
-- screens); --wav writes the DSP's output (32 kHz stereo), --png the last
-- frame (512 wide, its visible lines; low-res pixels twice), --vga the
-- last frame as the tops show it in 640 x 480 ("Snes.Display": 512 half-dots
-- from x 64, each row on two lines from y 16, from y 1 with overscan; the
-- layer 4 goldens, systems/snes/test/golden/vga/), --vga4 the same on
-- the ZedBoard's 4-bit VGA pins (each channel's top 4 bits, widened as the
-- harness's decoder does, c x 17: systems/snes/test/golden/vga4/). The SPC700's
-- boot ROM: build/snes/ipl.bin (ours, make snes-asm) unless --ipl names
-- another (the user's own dump). --input plays pad 1 from a file, a line
-- "FRAME BITS" each (the pad from that frame on, the bits as --pad1's:
-- 11 B, 10 Y, 9 Select, 8 Start, 7 up, 6 down, 5 left, 4 right, 3 A, 2 X,
-- 1 L, 0 R; # starts a comment); --shots writes the 640 x 480 picture
-- every K frames into DIR (frame_NNNNN.png), to follow a game;
-- --ppu-writes "0D 0E" FROM TO prints each frame's writes to those PPU
-- registers (2100h + n), with the line and dot they land on.
module Main (main) where

import Prelude

import Control.Monad (forM_, unless, when)
import Control.Monad.ST (stToIO)
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import qualified Data.Vector.Unboxed as U
import Data.Bits ((.&.), shiftR)
import Data.Int (Int16)
import Data.Word (Word16, Word8)
import qualified Codec.Picture as P
import Numeric (showHex)
import System.Environment (getArgs)
import System.Exit (exitFailure)

import Snes.Cart (parseCart)
import Snes.Spec

main :: IO ()
main = do
  args <- getArgs
  case args of
    rom : rest -> run rom (opts rest defaults)
    _ -> putStrLn "usage: snes-run ROM [--frames N] [--pad1 N] [--ipl FILE] [--wav FILE]" >> exitFailure
 where
  defaults = Opts 60 0 "build/snes/ipl.bin" Nothing Nothing False Nothing Nothing Nothing Nothing 0 Nothing
  opts ("--frames" : n : r) o = opts r o { oFrames = read n }
  opts ("--pad1" : n : r) o = opts r o { oPad = read n }
  opts ("--pad2" : n : r) o = opts r o { oPad2 = read n }
  opts ("--ppu-writes" : rs : a : b : r) o = opts r o { oPpuW = Just (map (\x -> read ("0x" <> x)) (words rs), read a, read b) }
  opts ("--ipl" : f : r) o = opts r o { oIpl = f }
  opts ("--wav" : f : r) o = opts r o { oWav = Just f }
  opts ("--vga" : f : r) o = opts r o { oVga = Just f }
  opts ("--vga4" : f : r) o = opts r o { oVga4 = Just f }
  opts ("--png" : f : r) o = opts r o { oPng = Just f }
  opts ("--regs" : r) o = opts r o { oRegs = True }
  opts ("--input" : f : r) o = opts r o { oInput = Just f }
  opts ("--shots" : d : k : r) o = opts r o { oShots = Just (d, read k) }
  opts _ o = o

data Opts = Opts { oFrames :: Int, oPad :: Int, oIpl :: FilePath, oWav :: Maybe FilePath, oPng :: Maybe FilePath, oRegs :: Bool, oVga :: Maybe FilePath, oVga4 :: Maybe FilePath
                 , oInput :: Maybe FilePath, oShots :: Maybe (FilePath, Int), oPad2 :: Int
                 , oPpuW :: Maybe ([Int], Int, Int) }

run :: FilePath -> Opts -> IO ()
run path o = do
  file <- B.readFile path
  ipl <- B.readFile (oIpl o)
  cart <- maybe (putStrLn (path <> ": no cartridge header") >> exitFailure) pure (parseCart file)
  inputs <- maybe (pure []) (fmap parseInput . readFile) (oInput o)
  m <- stToIO (newMachine ipl cart file)
  stToIO (reset m >> setPads m (oPad o) (oPad2 o))
  forM_ (oPpuW o) $ \_ -> stToIO (traceOn m)
  -- frame by frame: the pad's changes, the shots
  let step k
        | k >= oFrames o = pure ()
        | otherwise = do
            forM_ [ b | (f0, b) <- inputs, f0 == k ] $ \b -> stToIO (setPads m b (oPad2 o))
            stToIO (runFrames m 1)
            forM_ (oPpuW o) $ \(regs, lo, hi) -> do
              tr <- stToIO (takeTrace m)
              when (k >= lo && k <= hi) $
                putStrLn ("frame " <> show k <> ": " <> unwords [ "L" <> show v <> "." <> show h <> ":" <> showHex r "" <> "=" <> showHex x ""
                                                                 | PpuAt _ v h (PpuWrite r x) <- tr, r `elem` regs ])
            forM_ (oShots o) $ \(d, every) -> when ((k + 1) `mod` every == 0) $ do
              (pc, l) <- stToIO ((,) <$> frameColors m <*> visibleLines m)
              P.writePng (d <> "/frame_" <> pad5 (k + 1) <> ".png") (vga pc l)
            done <- stToIO (cpuStopped m)
            unless done (step (k + 1))
  step 0
  (stopped, res, text, f, audio, (pic, lns), regs) <- stToIO $ do
    s <- cpuStopped m
    r <- readWram m 0
    t <- screenText m
    fs <- frames m
    a <- audioSamples m
    pc <- frameColors m
    l <- visibleLines m
    rg <- ppuRegisters m
    cg <- cgramColors m
    pure (s, r, t, fs, a, (pc, l), (rg, cg))
  putStrLn ("frames " <> show f <> (if stopped then ", stopped (STP)" else "") <> ", RESULT $" <> showHex res ""
            <> ", " <> show (U.length audio `div` 2) <> " samples")
  forM_ text $ \l -> unless (all (== ' ') l) (putStrLn ("  |" <> l))
  when (oRegs o) $ do
    putStrLn ("PPU " <> unwords [ showHex i "" <> "=" <> showHex v "" | (i, v) <- zip [0 :: Int ..] (fst regs), v /= 0 ])
    putStrLn ("CGRAM " <> unwords [ showHex v "" | v <- take 48 (snd regs) ])
    cr <- stToIO (cpuRegs m)
    putStrLn ("CPU " <> show cr)
    dp <- stToIO (mapM (readWram m) [0 .. 0x15])
    putStrLn ("DP " <> unwords [ showHex v "" | v <- dp ])
    sr <- stToIO (apuCpuRegs (apuOf m))
    pin <- stToIO (apuInputs m)
    putStrLn ("SPC700 " <> show sr <> "; the CPU's side of the ports (SPC700 writes) " <> show pin)
  forM_ (oWav o) $ \w -> B.writeFile w (wav audio)
  forM_ (oPng o) $ \w -> P.writePng w (png pic lns)
  forM_ (oVga o) $ \w -> P.writePng w (vga pic lns)
  forM_ (oVga4 o) $ \w -> P.writePng w (P.pixelMap (\(P.PixelRGB8 r g b) -> P.PixelRGB8 (four r) (four g) (four b)) (vga pic lns))

-- | --input's lines: "FRAME BITS" (numbers as Haskell reads them: 0x... too).
parseInput :: String -> [(Int, Int)]
parseInput t = [ (read f, read b) | l <- lines t, (f : b : _) <- [words (takeWhile (/= '#') l)] ]

pad5 :: Int -> String
pad5 n = let s = show n in replicate (5 - length s) '0' <> s

-- | 15-bit colours (5 bits each, red lowest) as an 8-bit RGB image.
-- | The last frame in 640 x 480 as "Snes.Display" lays it out (the colours
-- widened as there: each 5 bits to 8, the top 3 again below).
vga :: U.Vector Word16 -> Int -> P.Image P.PixelRGB8
vga pic lns = P.generateImage at 640 480
 where
  top = if lns == 239 then 1 else 16
  at x y
    | x >= 64 && x < 576 && y >= top && y < top + 2 * lns =
        let c = pic U.! (((y - top) `div` 2) * 512 + (x - 64))
            ch k = let v = (fromIntegral c `shiftR` k) .&. 31 :: Int in fromIntegral ((v * 8) + (v `div` 4))
        in P.PixelRGB8 (ch 0) (ch 5) (ch 10)
    | otherwise = P.PixelRGB8 0 0 0

-- | An 8-bit channel on the ZedBoard's 4-bit VGA DAC, as the harness reads it.
four :: Word8 -> Word8
four v = (v `shiftR` 4) * 17

png :: U.Vector Word16 -> Int -> P.Image P.PixelRGB8
png pic lns = P.generateImage at 512 lns
 where
  at x y = let c = pic U.! (y * 512 + x)
               ch k = fromIntegral (((fromIntegral c `shiftR` k) .&. 31) * 255 `div` 31 :: Int)
           in P.PixelRGB8 (ch 0) (ch 5) (ch 10)

-- | A 32 kHz, 16-bit stereo WAV file.
wav :: U.Vector Int -> B.ByteString
wav s = B.toStrict (BB.toLazyByteString (header <> foldMap (BB.int16LE . (fromIntegral :: Int -> Int16)) (U.toList s)))
 where
  n = U.length s * 2
  header = BB.string7 "RIFF" <> BB.word32LE (fromIntegral (36 + n)) <> BB.string7 "WAVEfmt "
        <> BB.word32LE 16 <> BB.word16LE 1 <> BB.word16LE 2 <> BB.word32LE 32000
        <> BB.word32LE (32000 * 4) <> BB.word16LE 4 <> BB.word16LE 16
        <> BB.string7 "data" <> BB.word32LE (fromIntegral n)
