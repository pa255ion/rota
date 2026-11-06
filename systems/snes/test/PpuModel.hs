-- | The PPU's memories as the lockstep tests model them (block RAMs: one
-- clock's latency, read before write): VRAM's two chips, CGRAM, OAM's low
-- table, the line buffer, the column ring; the frame the pixels make.
-- Shared by "snes-lockstep" (the PPU alone, step 7.6) and "snes-core"
-- (the machine, step 7.9).
module PpuModel
  ( Mems (..)
  , newMems
  , readRom
  , compareFrame
  , writePpm
  , memories
  , vecOf
  , rand
  , hex
  ) where

import Prelude

import Control.Monad (forM_, unless)
import qualified Data.ByteString as B
import Data.Bits
import Data.IORef
import Data.Word (Word16, Word8)
import System.Directory (doesFileExist)
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import Test.Tasty.HUnit

import qualified Clash.Prelude as C
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MV
import qualified Data.Vector.Mutable as BV

import Snes.Ppu

newMems :: IO Mems
newMems = Mems <$> MV.replicate 0x8000 0 <*> MV.replicate 0x8000 0 <*> MV.replicate 256 0
               <*> MV.replicate 512 0 <*> MV.replicate 256 0 <*> MV.replicate (512 * 239) 0
               <*> MV.replicate 32 0 <*> MV.replicate 128 0 <*> BV.replicate 32 (RingH 0 0 0)

readRom :: FilePath -> IO B.ByteString
readRom p = do
  ok <- doesFileExist p
  unless ok (assertFailure (p <> " missing: run make snes-asm / tools/fetch-roms.sh"))
  B.readFile p

data Mems = Mems
  { mVlo, mVhi :: MV.IOVector Word8      -- ^ VRAM's two chips, 32 Ki words
  , mCg       :: MV.IOVector Word16      -- ^ 256 colours
  , mOamL     :: MV.IOVector Word8       -- ^ OAM's low table, entry * 4 + lane
  , mObjBuf   :: MV.IOVector Word16      -- ^ line buffer: bank * 32 + address
  , mFrame    :: MV.IOVector Word16      -- ^ 512 × 239
  , mRingE    :: MV.IOVector Word16      -- ^ the ring's map entries: layer * 8 + column
  , mRingW    :: MV.IOVector Word16      -- ^ its char words: (layer * 4 + word) * 8 + column
  , mRingH    :: BV.IOVector RingH       -- ^ its char headers
  }

compareFrame :: Mems -> Int -> U.Vector Word16 -> IO ()
compareFrame mems k fr = do
  got <- U.freeze (mFrame mems)
  let bad = [ i | i <- [0 .. U.length fr - 1], got U.! i /= fr U.! i ]
  case bad of
    [] -> pure ()
    (i : _) -> do
      dir <- lookupEnv "LOCKSTEP_DUMP"
      forM_ dir $ \d -> do
        writePpm (d </> "clash.ppm") got
        writePpm (d </> "spec.ppm") fr
      assertFailure ("frame " <> show k <> ": " <> show (length bad) <> " half-dots differ, first row "
                              <> show (i `div` 512) <> " x " <> show ((i `mod` 512) `div` 2) <> " half " <> show (i `mod` 2)
                              <> ": " <> hex (fromIntegral (got U.! i)) <> ", the spec " <> hex (fromIntegral (fr U.! i))
                              <> "; the first (row, x, half): " <> show [ (j `div` 512, (j `mod` 512) `div` 2, j `mod` 2) | j <- take 12 bad ])

-- | A frame as a PPM (each half-dot a pixel), for LOCKSTEP_DUMP=dir.
writePpm :: FilePath -> U.Vector Word16 -> IO ()
writePpm p fr = B.writeFile p (B.pack (map (fromIntegral . fromEnum) "P6\n512 239\n255\n") <> B.pack (concatMap rgb (U.toList fr)))
 where
  rgb w = [ fromIntegral (((w `shiftR` k) .&. 31) * 255 `div` 31) | k <- [0, 5, 10] ]

-- | The block RAMs' clock: this clock's reads (seen next clock), then the
-- writes.
memories :: Mems -> PpuOut -> IO PpuIn
memories mems o = do
  let aLo = fromIntegral (poVramA o C.!! (0 :: Int))
      aHi = fromIntegral (poVramA o C.!! (1 :: Int))
      b = fromIntegral (poVramB o)
  lo <- MV.read (mVlo mems) aLo
  hi <- MV.read (mVhi mems) aHi
  blo <- MV.read (mVlo mems) b
  bhi <- MV.read (mVhi mems) b
  cga <- MV.read (mCg mems) (fromIntegral (poCgramA o))
  cgb <- MV.read (mCg mems) (fromIntegral (poCgramB o))
  oa <- mapM (\l -> MV.read (mOamL mems) (fromIntegral (poOamA o) * 4 + l)) [0 .. 3]
  ob <- mapM (\l -> MV.read (mOamL mems) (fromIntegral (poOamB o) * 4 + l)) [0 .. 3]
  objs <- mapM (\k -> MV.read (mObjBuf mems) (k * 32 + fromIntegral (poObjA o))) [0 .. 7]
  let ra = map fromIntegral (C.toList (poRingA o)) :: [Int]
  re <- mapM (\(n, j) -> MV.read (mRingE mems) (n * 8 + j)) (zip [0 ..] ra)
  rw <- mapM (\(n, j) -> mapM (\w -> MV.read (mRingW mems) ((n * 4 + w) * 8 + j)) [0 .. 3]) (zip [0 ..] ra)
  rh <- BV.read (mRingH mems) (fromIntegral (poRingH o))
  forM_ (poRingWE o) $ \(n, j, v) -> MV.write (mRingE mems) (fromEnum n * 8 + fromIntegral j) (fromIntegral v)
  forM_ (poRingWW o) $ \(n, w, j, v) -> MV.write (mRingW mems) ((fromEnum n * 4 + fromEnum w) * 8 + fromIntegral j) (fromIntegral v)
  forM_ (poRingWH o) $ \(a, h) -> BV.write (mRingH mems) (fromIntegral a) h
  case poVramW o of
    Just (high, v) -> MV.write (if high then mVhi mems else mVlo mems) b (fromIntegral v)
    Nothing -> pure ()
  case poCgramW o of
    Just v -> MV.write (mCg mems) (fromIntegral (poCgramB o)) (fromIntegral v)
    Nothing -> pure ()
  forM_ (zip [0 ..] (C.toList (poOamW o))) $ \(l, w) -> case w of
    Just v -> MV.write (mOamL mems) (fromIntegral (poOamB o) * 4 + l) (fromIntegral v)
    Nothing -> pure ()
  forM_ (zip [0 ..] (C.toList (poObjW o))) $ \(k, w) -> case w of
    Just (a, v) -> MV.write (mObjBuf mems) (k * 32 + fromIntegral a) (fromIntegral v)
    Nothing -> pure ()
  pure PpuIn
    { piTick = False, piAccess = Nothing
    , piVramA = vecOf [fromIntegral lo, fromIntegral hi]
    , piVramB = vecOf [fromIntegral blo, fromIntegral bhi]
    , piCgramA = fromIntegral cga, piCgramB = fromIntegral cgb
    , piOamA = vecOf (map fromIntegral oa), piOamB = vecOf (map fromIntegral ob)
    , piObj = vecOf (map fromIntegral objs)
    , piRingE = vecOf (map fromIntegral re)
    , piRingW = vecOf (map (vecOf . map fromIntegral) rw)
    , piRingH = rh
    }

vecOf :: C.KnownNat k => [a] -> C.Vec k a
vecOf xs = C.map (\i -> xs !! fromEnum i) C.indicesI

rand :: IORef Int -> IO Int
rand ref = do
  x <- readIORef ref
  let x' = (x * 1103515245 + 12345) .&. 0x7FFFFFFF
  writeIORef ref x'
  pure (x' `shiftR` 16)

hex :: Int -> String
hex v = let s = go v "" in if null s then "0" else s
 where
  go 0 acc = acc
  go k acc = go (k `div` 16) ("0123456789ABCDEF" !! (k `mod` 16) : acc)
