{-|
@snes-ddrmodel ROM [--frames N] [--input FILE] [--lines K] [--latency L]
[--hit H] [--dma-cost D] [--margin M] [--from F]@ (PLAN.md step 7.12): how far a game's
ROM reads from DDR set the line-locked machine back, frame by frame,
without the board or a hardware simulation. The spec (`Snes.Spec`) plays
the game (pad 1 from @--input@, as @snes-run@'s), its bus trace gives
every ROM read (the CPU's, DMA's, HDMA's tables) at its master clock, and
a model prices each one:

* "Snes.RomPort" keeps the last 8-byte word: a read in it costs nothing
  (the next clock, as from block RAM);
* "Platform.Mem.Ddr"'s cache: @2^K@ direct-mapped lines of 32 bytes
  (the machine's @SNat \@5@: K = 5), with the next line prefetched after
  each request when the bus is idle; a hit costs H pixel clocks (2: the port's clock and the
  ROM port's), a miss L until its beat arrives (15 measured on the
  board, docs/ARCHITECTURE.md; a line takes L + 4) plus H; one fill at a
  time.

And "Snes.Bus" itself holds the master clock's enable a pixel clock for
each access with no time of its own (D = 1: a DMA byte takes 9 pixel
clocks for its 8 master clocks; an HDMA table read). The stalls go into
each SNES line, and the core's lag behind the output
grows by them and shrinks by up to 236 pixel clocks a line (1600 a line
pair against 1364 master clocks: "Snes.Display"'s pace). A row is late
when the lag passes the ring's reserve, M pixel clocks (12000: a row is
shown 7.5 SNES lines after it is made, in the 224-line mode). Per frame:
the ROM reads, word changes, misses, the stall, the largest lag, the rows
late. An estimate for comparing caches, not a cycle-exact model: the
machine at layer 4 is.
-}
module Main (main) where

import Prelude

import Control.Monad (forM_, unless, when)
import Control.Monad.ST (stToIO)
import qualified Data.ByteString as B
import Data.IORef

import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MV
import System.Environment (getArgs)
import System.Exit (exitFailure)
import Text.Printf (printf)

import Snes.Cart (Cart (..), parseCart, stripCopier)
import Snes.Spec

data Opts = Opts
  { oFrames :: Int, oInput :: Maybe FilePath, oLines :: Int, oLatency :: Int
  , oHit :: Int, oDma :: Int, oMargin :: Int, oFrom :: Int }

main :: IO ()
main = do
  args <- getArgs
  case args of
    rom : rest -> run rom (opts rest (Opts 600 Nothing 5 15 2 1 12000 0))
    _ -> putStrLn "usage: snes-ddrmodel ROM [--frames N] [--input FILE] [--lines K] [--latency L] [--hit H] [--margin M] [--from F]" >> exitFailure
 where
  opts ("--frames" : n : r) o = opts r o { oFrames = read n }
  opts ("--input" : f : r) o = opts r o { oInput = Just f }
  opts ("--lines" : n : r) o = opts r o { oLines = read n }
  opts ("--latency" : n : r) o = opts r o { oLatency = read n }
  opts ("--hit" : n : r) o = opts r o { oHit = read n }
  opts ("--margin" : n : r) o = opts r o { oMargin = read n }
  opts ("--dma-cost" : n : r) o = opts r o { oDma = read n }
  opts ("--from" : n : r) o = opts r o { oFrom = read n }
  opts _ o = o

-- | The model's state: the ROM port's word, the cache's tags and when each
-- line is ready, when the bus is free, the time (pixel clocks: master
-- clocks plus the stalls so far).
data Port = Port !Int !Int !Int  -- ^ the word, the bus free from, the stalls so far

run :: FilePath -> Opts -> IO ()
run path o = do
  file <- B.readFile path
  ipl <- B.readFile "build/snes/ipl.bin"
  cart <- maybe (putStrLn (path <> ": no cartridge header") >> exitFailure) pure (parseCart file)
  inputs <- maybe (pure []) (fmap parseInput . readFile) (oInput o)
  let romSize = B.length (fst (stripCopier file))
      n = 2 ^ oLines o :: Int
      lat = oLatency o
      target = decode (cartMapping cart) romSize (cartSramSize cart)
  tags <- MV.replicate n (-1) :: IO (MV.IOVector Int)
  ready <- MV.replicate n 0 :: IO (MV.IOVector Int)
  port <- newIORef (Port (-1) 0 0)
  lagR <- newIORef (0 :: Int)
  totalLate <- newIORef (0 :: Int)
  m <- stToIO (newMachine ipl cart file)
  stToIO (reset m >> setPads m 0 0 >> busTraceOn m)
  printf "frame  reads  words  misses   stall  maxlag  late\n"
  forM_ [0 .. oFrames o - 1] $ \k -> do
    forM_ [ b | (f0, b) <- inputs, f0 == k ] $ \b -> stToIO (setPads m b 0)
    mc0 <- stToIO (masterClocks m)
    stToIO (runFrames m 1)
    evs <- stToIO (takeBusTrace m)  -- (oldest first)
    let romOf a = case target a of TRom off -> Just off; _ -> Nothing
        romReads = [ (mc, off) | e <- evs, Just (mc, a) <- [romRead e], Just off <- [romOf a] ]
    -- price each read; the stalls by line
    lineStall <- MV.replicate 263 0 :: IO (MV.IOVector Int)
    counts <- newIORef (0 :: Int, 0 :: Int)
    forM_ romReads $ \(mc, off) -> do
      Port w free st <- readIORef port
      let t = mc + st
          wd = off `div` 8
      unless (wd == w) $ do
        let ln = off `div` 32
            ix = ln `mod` n
            beat = (off `div` 8) `mod` 4
        tg <- MV.read tags ix
        rd <- MV.read ready ix
        (cost, free') <-
          if tg == ln
            then pure (max 0 (rd - t) + oHit o, free)
            else do
              let start = max t free
                  done = start + lat + 4
              MV.write tags ix ln
              MV.write ready ix done
              modifyIORef' counts (\(a, b) -> (a, b + 1))
              pure (start + lat + beat - t + oHit o, done)
        -- the next line, prefetched if the bus is idle once this read is
        -- answered (the port reads ahead only while otherwise idle; a
        -- miss never waits for a prefetch not yet begun)
        let nl = ln + 1
            nix = nl `mod` n
            at = t + cost
        ntg <- MV.read tags nix
        free'' <- if ntg == nl || free' > at then pure free' else do
          let done = at + lat + 4
          MV.write tags nix nl
          MV.write ready nix done
          pure done
        let v = min 262 (max 0 ((mc - mc0) `div` 1364))
        MV.modify lineStall (+ cost) v
        modifyIORef' counts (\(a, b) -> (a + 1, b))
        writeIORef port (Port wd free'' (st + cost))
    -- the bus's own holds: a pixel clock a DMA byte and HDMA table read
    forM_ evs $ \e -> case e of
      BeDma mc _ _ _ _ -> MV.modify lineStall (+ oDma o) (min 262 (max 0 ((mc - mc0) `div` 1364)))
      BeTable mc _ _ -> MV.modify lineStall (+ oDma o) (min 262 (max 0 ((mc - mc0) `div` 1364)))
      _ -> pure ()
    -- the lag, line by line; rows 0-223 need line row + 2 made
    stalls <- U.freeze lineStall
    lag0 <- readIORef lagR
    let step (lag, mx, lt) v =
          let lag' = max 0 (lag + stalls U.! v - 236)
              lt' = lt + (if v >= 2 && v < 226 && lag' > oMargin o then 1 else 0)
          in (lag', max mx lag', lt')
        (lagEnd, maxLag, late) = foldl' step (lag0, 0 :: Int, 0 :: Int) [0 .. 262]
    writeIORef lagR lagEnd
    modifyIORef' totalLate (+ late)
    (words', misses) <- readIORef counts
    when (k >= oFrom o && (misses > 0 || late > 0)) $
      printf "%5d %6d %6d %7d %7d %7d %5d\n" k (length romReads) words' misses (U.sum stalls) maxLag late
  tl <- readIORef totalLate
  printf "rows late in all: %d (cache 2^%d lines, latency %d, hit %d, DMA %d, margin %d)\n" tl (oLines o) lat (oHit o) (oDma o) (oMargin o)

-- | A ROM-side read in the trace: its master clock and its A-bus address
-- (the CPU's reads, DMA from the A bus, HDMA's table reads).
romRead :: BusEvent -> Maybe (Int, Int)
romRead e = case e of
  BeRead mc a _ -> Just (mc, a)
  BeDma mc a _ _ toA | not toA -> Just (mc, a)
  BeTable mc a _ -> Just (mc, a)
  _ -> Nothing

parseInput :: String -> [(Int, Int)]
parseInput t = [ (read f, read b) | l <- lines t, (f : b : _) <- [words (takeWhile (/= '#') l)] ]
