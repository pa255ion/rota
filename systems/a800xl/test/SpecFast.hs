-- | Side track Q (PLAN.md Q.5): the 800XL's fast version ("A800.SpecFast",
-- the desktop emulator's) is the same machine as the reference model
-- ("A800.Spec"). Both run side by side on the same inputs: after every
-- instruction the machine cycle count, the CPU's registers and POKEY's
-- output level; at every frame the picture and all 64 KiB of RAM. The
-- inputs are a seeded sequence of joystick, trigger, console and key
-- changes, so menus and games are driven. Quick tier: BASIC, Scorch and
-- the GTIA tests briefly; full tier (EMU_FULL_TESTS=1): longer, and every
-- Acid800 test.
module Main (main) where

import Prelude

import Control.Monad.ST (runST)
import Data.Bits (shiftL, shiftR, xor, (.&.))
import Data.List (isPrefixOf, sort)
import qualified Data.Vector.Unboxed as U
import Data.Word (Word32, Word64, Word8)
import System.Directory (listDirectory)
import System.Environment (lookupEnv)
import System.FilePath (takeExtension, (</>))
import Test.Tasty
import Test.Tasty.HUnit

import A800.Spec (Config, casConfig, stockConfig, xexConfig)
import qualified A800.Spec.Gtia as G
import qualified A800.Spec.Machine as S
import A800.Spec.Antic (anticFrames)
import qualified A800.SpecFast.Antic as FA
import A800.Spec.Pokey (pokeyLevel)
import qualified A800.SpecFast.Pokey as FP
import qualified A800.SpecFast.Gtia as FG
import qualified A800.SpecFast.Machine as F
import A800.TestRoms
import A800.Media (parseCart, parseCas)
import qualified Mos6502.Spec as SC
import qualified Mos6502.SpecFast as FC

main :: IO ()
main = do
  full <- maybe False (not . null) <$> lookupEnv "EMU_FULL_TESTS"
  acid <- sort . filter (\f -> takeExtension f == ".xex") <$> listDirectory (romRoot </> "acid800")
  let n q f = if full then f else q
      chosen = if full then acid else filter ("gtia_" `isPrefixOf`) acid
  defaultMain $ testGroup "a800xl-specfast: the fast version equals the reference model"
    [ testCase "BASIC boots (stock)" $ same (n 150 600) =<< stock
    , testCase "Scorch with inputs" $ same (n 400 3000) =<< xex scorchXex
    , testCase "our scene.xex (priorities, P/M)" $ same (n 60 300) =<< xex (asmRoot </> "scene.xex")
    , testCase "our keys.xex" $ same (n 60 300) =<< xex (asmRoot </> "keys.xex")
    , testCase "our XEGS cartridge (xegs_banks.car, bank switching)" $ same (n 60 300) =<< cart (asmRoot </> "xegs_banks.car")
    , testCase "our cassette (cas_boot.cas, booted through the OS's SIO call)" $ do
        c <- loadFile (asmRoot </> "cas_boot.cas")
        cas <- either (\e -> assertFailure e >> fail e) pure (parseCas c)
        cfg <- casConfig <$> loadFile osRom <*> loadFile basicRom <*> pure cas
        either assertFailure (const (pure ())) (lockstepWith casInputs False (n 400 900) cfg)
    , testCase "our cassette under interrupts (cas_irq.cas: 100 records through SIOV, a DLI on every line)" $ do
        c <- loadFile (asmRoot </> "cas_irq.cas")
        cas <- either (\e -> assertFailure e >> fail e) pure (parseCas c)
        cfg <- casConfig <$> loadFile osRom <*> loadFile basicRom <*> pure cas
        either assertFailure (const (pure ())) (lockstepWith casInputs False (n 400 900) cfg)
    , testCase "Scorch with every other frame's picture skipped: the same CPU, RAM and POKEY" $ sameSkip (n 300 2000) =<< xex scorchXex
    , testGroup "Acid800" [ testCase f (same (n 80 300) =<< xex (romRoot </> "acid800" </> f)) | f <- chosen ]
    ]
 where
  stock = stockConfig <$> loadFile osRom <*> loadFile basicRom <*> pure Nothing
  cart f = do
    c <- loadFile f
    either (\e -> assertFailure e >> fail e) (\k -> stockConfig <$> loadFile osRom <*> loadFile basicRom <*> pure (Just k)) (parseCart f c)
  xex f = xexConfig <$> loadFile osRom <*> loadFile basicRom <*> loadFile loaderBin <*> loadFile f
  same frames cfg = either assertFailure (const (pure ())) (lockstep False frames cfg)
  sameSkip frames cfg = either assertFailure (const (pure ())) (lockstep True frames cfg)

-- | The inputs at frame @k@: every 8 frames a new joystick (directions
-- and trigger), console keys and sometimes a key, from a xorshift seed.
inputs :: Int -> (Word8, Word8, Word64)
inputs k =
  let r = rand (fromIntegral (k `div` 8) + 1)
      joy = fromIntegral (r .&. 0x1F) :: Word8
      con = fromIntegral ((r `shiftR` 5) .&. 7) :: Word8
      key = if (r `shiftR` 8) .&. 3 == 0 then 1 `shiftL` fromIntegral ((r `shiftR` 10) .&. 63) else 0
  in (joy, con, key)
 where
  rand :: Word32 -> Word32
  rand s0 = let a = s0 `xor` (s0 `shiftL` 13); b = a `xor` (a `shiftR` 17) in b `xor` (b `shiftL` 5)

-- | Both machines for @frames@ frames; Left with the first difference.
lockstep :: Bool -> Int -> Config -> Either String Int
lockstep = lockstepWith inputs

-- | A cassette boot's inputs (A800.Spec.runCasBoot's): START for a
-- second, then Space.
casInputs :: Int -> (Word8, Word8, Word64)
casInputs k = (0, if k < 60 then 1 else 0, if k >= 120 && k < 126 then 1 `shiftL` 0x21 else 0)

lockstepWith :: (Int -> (Word8, Word8, Word64)) -> Bool -> Int -> Config -> Either String Int
lockstepWith inputsOf skips frames cfg = runST $ do
  a <- S.newMachine cfg
  b <- F.newMachine cfg
  let frame k
        | k >= frames = pure (Right k)
        | otherwise = do
            let (joy, con, key) = inputsOf k
            S.setJoystick a joy >> F.setJoystick b joy
            S.setConsole a con >> F.setConsole b con
            S.setKeys a key 0 >> F.setKeys b key 0
            -- frame skipping: every other frame's picture not drawn (nor compared)
            FG.setGtiaSkip (F.mGtia b) (skips && odd k)
            f0 <- anticFrames (S.mAntic a)
            r <- steps k f0
            case r of
              Just e -> pure (Left e)
              Nothing -> do
                pa <- G.gtiaFrame (S.mGtia a)
                pb <- FG.gtiaFrame (F.mGtia b)
                ra <- U.freeze (S.mRam a)
                rb <- U.freeze (F.mRam b)
                if pa /= pb && not skips then pure (Left ("frame " <> show k <> ": the pictures differ at pixel " <> show (firstDiff pa pb)))
                else if ra /= rb then pure (Left ("frame " <> show k <> ": RAM differs at " <> show (firstDiff (U.toList ra) (U.toList rb))))
                else frame (k + 1)
      steps k f0 = do
        _ <- S.stepCpu a
        _ <- F.stepCpu b
        ca <- S.machineCycles a
        cb <- F.machineCycles b
        ga <- (\(SC.Regs ra rx ry rs rp pc) -> (ra, rx, ry, rs, rp, pc)) <$> SC.getRegs (S.mCpu a)
        gb <- (\(FC.Regs ra rx ry rs rp pc) -> (ra, rx, ry, rs, rp, pc)) <$> FC.getRegs (F.mCpu b)
        la <- pokeyLevel (S.mPokey a)
        lb <- FP.pokeyLevel (F.mPokey b)
        if ca /= cb || ga /= gb || la /= lb
          then pure (Just ("frame " <> show k <> ", cycle " <> show ca <> ": " <> show (ca, ga, la) <> " vs " <> show (cb, gb, lb)))
          else do
            f <- anticFrames (S.mAntic a)
            fb <- FA.anticFrames (F.mAntic b)
            if f /= fb then pure (Just ("frame " <> show k <> ": ANTIC's frame counts differ")) else if f == f0 then steps k f0 else pure Nothing
  frame 0

firstDiff :: Eq a => [a] -> [a] -> Int
firstDiff xs ys = length (takeWhile id (zipWith (==) xs ys))

