-- | Side track Q (PLAN.md Q.5): the SNES's fast version ("Snes.SpecFast",
-- the desktop emulator's) is the same machine as the reference model
-- ("Snes.Spec"). Both run side by side on the same inputs: after every
-- instruction the master clock count and the CPU's registers; at every
-- frame the picture, all of WRAM and the sound so far. The pads change
-- every 8 frames from a seeded sequence, so menus and games are driven.
-- Quick tier: the free games and a few test ROMs briefly; full tier
-- (EMU_FULL_TESTS=1): longer, and every krom and OpenSNES ROM.
module Main (main) where

import Prelude

import Control.Monad.ST (runST)
import Data.Bits (shiftL, shiftR, xor, (.&.))
import qualified Data.ByteString as B
import Data.List (sort)
import qualified Data.Vector.Unboxed as U
import Data.Word (Word32)
import System.Directory (listDirectory)
import System.Environment (lookupEnv)
import System.FilePath (takeExtension, (</>))
import Test.Tasty
import Test.Tasty.HUnit

import Snes.Cart (Cart, parseCart)
import qualified Snes.Spec.Machine as S
import qualified Snes.SpecFast.Machine as F
import qualified Snes.SpecFast.Ppu as FP

romRoot :: FilePath
romRoot = "../../roms/snes"

iplFile :: FilePath
iplFile = "../../build/snes/ipl.bin"

main :: IO ()
main = do
  full <- maybe False (not . null) <$> lookupEnv "EMU_FULL_TESTS"
  let sfc d = sort . filter (\f -> takeExtension f == ".sfc") <$> listDirectory (romRoot </> d)
  krom <- sfc "krom"
  open <- sfc "opensnes"
  let n q f = if full then f else q
      pick xs = if full then xs else take 3 xs
  defaultMain $ testGroup "snes-specfast: the fast version equals the reference model"
    [ testCase "the PPU's fetch slots as cases equal its table (side track W)"
        (assertEqual "" [ FP.fetchTable m | m <- [0 .. 7] ] [ [ FP.fetchSlotOf m k | k <- [0 .. 7] ] | m <- [0 .. 7] ])
    , testCase "the unnamed tech demo" $ same (n 200 1200) ("games" </> "unnamed-tech-demo-v3.sfc")
    , testCase "the horizontal shooter, with pads" $ same (n 300 1800) ("games" </> "horizontal-shooter.sfc")
    , testGroup "krom" [ testCase f (same (n 40 200) ("krom" </> f)) | f <- pick krom ]
    , testGroup "OpenSNES" [ testCase f (same (n 60 300) ("opensnes" </> f)) | f <- pick open ]
    , testCase "the tech demo with the drawing inline (--no-render-thread)" $ sameWith False False (n 100 600) ("games" </> "unnamed-tech-demo-v3.sfc")
    , testCase "the shooter with every other frame's drawing skipped: the same CPU, WRAM and sound" $ sameWith True True (n 200 1200) ("games" </> "horizontal-shooter.sfc")
    , testCase "the same, drawing inline" $ sameWith False True (n 100 600) ("games" </> "horizontal-shooter.sfc")
    , testCase "the clocks rebased (the browser's 32-bit Int) against a twin without: the same pictures, WRAM and sound" $
        twinTest (n 900 1800) ("games" </> "horizontal-shooter.sfc")
    ]
 where
  same = sameWith True False
  sameWith threaded skips frames rel = do
    file <- B.readFile (romRoot </> rel)
    ipl <- B.readFile iplFile
    cart <- maybe (assertFailure (rel <> ": no cartridge header")) pure (parseCart file)
    either assertFailure (const (pure ())) (lockstep threaded skips frames ipl cart file)
  twinTest frames rel = do
    file <- B.readFile (romRoot </> rel)
    ipl <- B.readFile iplFile
    cart <- maybe (assertFailure (rel <> ": no cartridge header")) pure (parseCart file)
    either assertFailure (const (pure ())) (twin frames ipl cart file)

-- | The pads at frame @k@: every 8 frames new ones (12 bits each, as
-- 'S.setPads'), from a xorshift seed.
pads :: Int -> (Int, Int)
pads k =
  let r = rand (fromIntegral (k `div` 8) + 1)
  in (fromIntegral (r .&. 0xFFF), fromIntegral ((r `shiftR` 12) .&. 0xFFF))
 where
  rand :: Word32 -> Word32
  rand s0 = let a = s0 `xor` (s0 `shiftL` 13); b = a `xor` (a `shiftR` 17) in b `xor` (b `shiftL` 5)

-- | Both machines for @frames@ frames; Left with the first difference.
lockstep :: Bool -> Bool -> Int -> B.ByteString -> Cart -> B.ByteString -> Either String Int
lockstep threaded skips frames ipl cart file = runST $ do
  a <- S.newMachine ipl cart file
  b <- F.newMachineWith (if threaded then 2 else 0) ipl cart file
  S.reset a
  F.reset b
  let frame k
        | k >= frames = pure (Right k)
        | otherwise = do
            let (p1, p2) = pads k
            S.setPads a p1 p2 >> F.setPads b p1 p2
            -- frame skipping: every other frame not drawn (its picture then not compared)
            F.setSkipDraw b (skips && odd k)
            f0 <- S.frames a
            r <- steps k f0
            case r of
              Just e -> pure (Left e)
              Nothing -> do
                pa <- S.frameColors a
                pb <- F.frameColors b
                wa <- mapM (S.readWram a) [0 .. 0x1FFFF]
                wb <- mapM (F.readWram b) [0 .. 0x1FFFF]
                sa <- S.audioSamples a
                sb <- F.audioSamples b
                if pa /= pb && not skips then pure (Left ("frame " <> show k <> ": the pictures differ at " <> show (firstDiff (U.toList pa) (U.toList pb))))
                else if wa /= wb then pure (Left ("frame " <> show k <> ": WRAM differs at " <> show (firstDiff wa wb)))
                else if sa /= sb then pure (Left ("frame " <> show k <> ": the sound differs at sample " <> show (firstDiff (U.toList sa) (U.toList sb)) <> " of " <> show (U.length sa, U.length sb)))
                else frame (k + 1)
      steps k f0 = do
        stopped <- S.cpuStopped a
        if stopped then pure Nothing else do
          S.stepCpu a
          F.stepCpu b
          ca <- S.masterClocks a
          cb <- F.masterClocks b
          ra <- S.cpuRegs a
          rb <- F.cpuRegs b
          if ca /= cb || ra /= rb
            then pure (Just ("frame " <> show k <> ", master clock " <> show ca <> ": " <> show ra <> " vs " <> show (cb, rb)))
            else do
              f <- S.frames a
              fb <- F.frames b
              if f /= fb then pure (Just ("frame " <> show k <> ": the frame counts differ"))
              else if f == f0 then steps k f0 else pure Nothing
  frame 0

-- | The fast version with its clocks' rebase ('F.rebase' in 'F.runFrames',
-- after every 2^28 master ticks: about 12.5 s) and a twin without it, on
-- 64 bits, a frame at a time as the desktop runs it: each frame's picture,
-- WRAM and sound; and the rebase did happen.
twin :: Int -> B.ByteString -> Cart -> B.ByteString -> Either String Int
twin frames ipl cart file = runST $ do
  a <- F.newMachineWith 0 ipl cart file
  b <- F.newMachineWith 0 ipl cart file
  F.reset a
  F.reset b
  F.setRebase b False
  let frame k
        | k >= frames = do
            ca <- F.masterClocks a
            cb <- F.masterClocks b
            pure (if cb - ca >= 142 * 945000 then Right k else Left ("the clocks weren't rebased: " <> show (ca, cb)))
        | otherwise = do
            let (p1, p2) = pads k
            F.setPads a p1 p2 >> F.setPads b p1 p2
            F.runFrames a 1 >> F.runFrames b 1
            pa <- F.frameColors a
            pb <- F.frameColors b
            wa <- mapM (F.readWram a) [0 .. 0x1FFFF]
            wb <- mapM (F.readWram b) [0 .. 0x1FFFF]
            sa <- F.audioSamples a
            sb <- F.audioSamples b
            if pa /= pb then pure (Left ("frame " <> show k <> ": the pictures differ"))
            else if wa /= wb then pure (Left ("frame " <> show k <> ": WRAM differs"))
            else if sa /= sb then pure (Left ("frame " <> show k <> ": the sound differs"))
            else frame (k + 1)
  frame 0

firstDiff :: Eq a => [a] -> [a] -> Int
firstDiff xs ys = length (takeWhile id (zipWith (==) xs ys))
