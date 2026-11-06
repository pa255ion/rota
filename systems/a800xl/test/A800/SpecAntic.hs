-- | Step 4.2: ANTIC's DMA, cycle by cycle, against the HRM (4.11, 4.13,
-- 4.14 and its DMA charts): which cycles of a scan line fetch what, for
-- the playfield modes, widths, horizontal scrolling and P/M graphics.
module A800.SpecAntic (anticTests) where

import Prelude

import Control.Monad (forM_)
import Control.Monad.ST (ST, runST)
import Data.List (sortOn)
import qualified Data.Map.Strict as M
import Data.STRef
import Data.Word
import Test.Tasty
import Test.Tasty.HUnit

import A800.Spec.Antic

-- | What ANTIC did on one cycle.
data Cyc = Fetch !Word16 | Refresh | Free
  deriving stock (Show, Eq)

-- | Run ANTIC alone over a memory (display list at $1000, screen at $2000)
-- after these register writes at power-on, and return what each cycle of
-- line @want@ did (114 entries), with the fetch addresses.
traceLine :: [(Word16, Word8)] -> [(Word16, Word8)] -> Int -> ST s [Cyc]
traceLine mem regs want = do
  let memory = M.fromList mem
  cur <- newSTRef Nothing
  let conn = AnticConn
        { acRead = \a -> writeSTRef cur (Just a) >> pure (M.findWithDefault 0 a memory)
        , acMissile = \_ _ -> pure ()
        , acPlayer = \_ _ _ -> pure ()
        }
  ant <- newAntic conn
  forM_ regs $ \(r, v) -> anticWrite ant 0 r v
  out <- newSTRef []
  let go t = do
        writeSTRef cur Nothing
        r <- anticBegin ant t 0
        (y, x) <- anticPosition ant
        f <- readSTRef cur
        _ <- anticEnd ant 0
        if y == want
          then do
            modifySTRef' out ((x, case (r, f) of
              (1, Just a) -> Fetch a
              (2, _) -> Refresh
              _ -> Free) :)
            if x == 113 then pure () else go (t + 1)
          else go (t + 1)
  go 1
  map snd . sortOn fst <$> readSTRef out

-- | The AN codes of line @want@ (two per cycle).
anLine :: [(Word16, Word8)] -> [(Word16, Word8)] -> Int -> [(Int, Int)]
anLine mem regs = anLineW mem regs []

-- | The same with register writes at (line, cycle) (after the cycle's start).
anLineW :: [(Word16, Word8)] -> [(Word16, Word8)] -> [(Int, Int, Word16, Word8)] -> Int -> [(Int, Int)]
anLineW mem regs writes want = runST $ do
  let memory = M.fromList mem
      conn = AnticConn
        { acRead = \a -> pure (M.findWithDefault 0 a memory)
        , acMissile = \_ _ -> pure ()
        , acPlayer = \_ _ _ -> pure ()
        }
  ant <- newAntic conn
  forM_ regs $ \(r, v) -> anticWrite ant 0 r v
  out <- newSTRef []
  let go t = do
        _ <- anticBegin ant t 0
        (y, x) <- anticPosition ant
        forM_ [ (r, v) | (wy, wx, r, v) <- writes, wy == y, wx == x ] $ \(r, v) -> anticWrite ant t r v
        c <- anticEnd ant 0
        if y == want
          then modifySTRef' out (c :) >> (if x == 113 then pure () else go (t + 1))
          else go (t + 1)
  go 1
  reverse <$> readSTRef out

-- | A display list at $1000: blank lines up to scan line 8 are not needed
-- (the list starts at line 8); @ir@ with LMS to $2000, repeated @n@ times,
-- then JVB.
dlist :: [Word8] -> [(Word16, Word8)]
dlist bytes = zip [0x1000 ..] (bytes <> [0x41, 0x00, 0x10])

regsFor :: Word8 -> [(Word16, Word8)]
regsFor dmactl = [(0xD402, 0x00), (0xD403, 0x10), (0xD400, dmactl)]

-- | Cycles fetching from an address range.
fetchesIn :: Word16 -> Word16 -> [Cyc] -> [Int]
fetchesIn lo hi cs = [ x | (x, Fetch a) <- zip [0 ..] cs, a >= lo, a < hi ]

refreshes :: [Cyc] -> [Int]
refreshes cs = [ x | (x, Refresh) <- zip [0 ..] cs ]

anticTests :: TestTree
anticTests = testGroup "ANTIC DMA (4.2)"
  [ testCase "mode 2 normal, first line: DL at 1, LMS at 6-7, names 18-96, data 21-99, one refresh at 98" $ do
      let cs = runST (traceLine (dlist [0x42, 0x00, 0x20, 0x02]) (regsFor 0x22) 8)
      fetchesIn 0x1000 0x1003 cs @?= [1, 6, 7]
      fetchesIn 0x2000 0x2100 cs @?= [18, 20 .. 96]
      fetchesIn 0x0000 0x0400 cs @?= [21, 23 .. 99] -- the font at CHBASE 0, names all 0
      refreshes cs @?= [98]
  , testCase "mode 2 normal, other lines: data only, refresh pushed one cycle (26, 30, ... 58)" $ do
      let cs = runST (traceLine (dlist [0x42, 0x00, 0x20, 0x02]) (regsFor 0x22) 9)
      fetchesIn 0x2000 0x2100 cs @?= []
      fetchesIn 0x0000 0x0400 cs @?= [21, 23 .. 99]
      refreshes cs @?= [26, 30 .. 58]
  , testCase "mode F normal: bitmap 20-98 every 2, refresh 25-57 every 4" $ do
      let cs = runST (traceLine (dlist [0x4F, 0x00, 0x20]) (regsFor 0x22) 8)
      fetchesIn 0x2000 0x2100 cs @?= [20, 22 .. 98]
      refreshes cs @?= [25, 29 .. 57]
  , testCase "mode 8 narrow: bitmap 28-84 every 8" $ do
      let cs = runST (traceLine (dlist [0x48, 0x00, 0x20]) (regsFor 0x21) 8)
      fetchesIn 0x2000 0x2100 cs @?= [28, 36 .. 84]
  , testCase "mode 6 wide: names 10-102 every 4, data 13-105, nothing past 105" $ do
      let cs = runST (traceLine (dlist [0x46, 0x00, 0x20]) (regsFor 0x23) 8)
      fetchesIn 0x2000 0x2100 cs @?= [10, 14 .. 102]
      fetchesIn 0x0000 0x0400 cs @?= [13, 17 .. 105]
  , testCase "mode 2 normal + HSCROL 4: wide fetch, two cycles later; past 105 virtual" $ do
      let cs = runST (traceLine (dlist [0x52, 0x00, 0x20]) (regsFor 0x22 <> [(0xD404, 4)]) 8)
      fetchesIn 0x2000 0x2100 cs @?= [12, 14 .. 104]
      fetchesIn 0x0000 0x0400 cs @?= [15, 17 .. 105]
  , testCase "P/M DMA: missile at 0, players at 2-5 (one-line resolution, PMBASE $30)" $ do
      let cs = runST (traceLine (dlist [0x70]) (regsFor 0x3C <> [(0xD407, 0x30)]) 20)
      fetchesIn 0x3000 0x3800 cs @?= [0, 2, 3, 4, 5]
      take 6 cs @?= [Fetch (0x3300 + 20), Free, Fetch (0x3400 + 20), Fetch (0x3500 + 20), Fetch (0x3600 + 20), Fetch (0x3700 + 20)]
  , testCase "mode E normal: $AA shows as PF1 from cycle 24 to 103" $ do
      let an = anLine (dlist [0x4E, 0x00, 0x20] <> [ (a, 0xAA) | a <- [0x2000 .. 0x2030] ]) (regsFor 0x22) 8
      take 2 (drop 22 an) @?= [(0, 0), (0, 0)]
      take 80 (drop 24 an) @?= replicate 80 (5, 5)
      take 2 (drop 104 an) @?= [(0, 0), (0, 0)]
  , testCase "an unstopped clock (HSCROL 0-2-0 mid-line) runs into the next line, which shows that line's data" $ do
      -- regression (antic_hscrolbug): the display read the line buffer ahead
      -- of its writes before the playfield start, showing the last line's data
      let mem = dlist [0x5E, 0x00, 0x20, 0x1E] <> [ (a, if a < 0x2030 then 0 else if even a then 0xAA else 0x55) | a <- [0x2000 .. 0x2090] ]
          an = anLineW mem (regsFor 0x21) [(8, 77, 0xD404, 2), (8, 106, 0xD404, 0)] 9
          region = take 64 (drop 32 an)
      assertBool ("line 9's playfield: " <> show region) (all (`elem` [(4, 4), (5, 5)]) region)
  , testCase "DMA is off without DMACTL: only refresh" $ do
      let cs = runST (traceLine (dlist [0x4F, 0x00, 0x20]) [] 8)
      [ x | (x, Fetch _) <- zip [0 :: Int ..] cs ] @?= []
      refreshes cs @?= [25, 29 .. 57]
  ]
