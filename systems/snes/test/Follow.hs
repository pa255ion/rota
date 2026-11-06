-- | @snes-follow ROM [--frames N] [--input FILE] [--ddr] [--every K]@
-- (PLAN.md step 7.12): the Clash machine's parts ("Snes.Bus", "Snes.Ppu",
-- "Snes.Apu"; with @--ddr@ also "Snes.RomPort" and a DDR of random
-- latency) against the spec ("Snes.Spec"), as @snes-core@ runs them (the
-- same memory models, the master clock's enable held at random), but a
-- frame at a time and for as long as a game runs: each frame's bus work
-- (every CPU cycle, DMA and HDMA byte, HDMA table read, at its master
-- clock) and its picture are compared, then dropped, so a game can be
-- followed for thousands of frames. Pad 1 from @--input@ (as @snes-run@'s:
-- "FRAME BITS" lines), set in both at the same frame's start. Stops at the
-- first difference: the frame, the event, both sides around it
-- (LOCKSTEP_DUMP=DIR writes both pictures when a frame differs). The
-- sound module's differences show as the CPU's port reads.
--
-- And the held clocks (the master clock's enable on but no tick: a wait
-- for the sound module, a DDR answer, the bus's own holds), per line,
-- through "Snes.Display"'s pace as snes-ddrmodel prices them (the lag
-- grows by a line's holds, shrinks by up to 236; a row late past
-- @--margin@, 48000 for the 32-row ring): per frame with any lag, the
-- holds, those waiting for the sound module, the largest lag, the rows
-- late. (The enable here is on 5 clocks in 6, so the holds are those
-- clocks' only.)
module Main (main) where

import Prelude

import Control.Exception (try)
import Control.Monad (forM_, unless, when)
import Control.Monad.ST (stToIO)
import qualified Data.ByteString as B
import Data.Bits
import Data.IORef
import Data.List (partition, sortOn)
import Data.Word (Word16, Word8)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (BufferMode (..), hSetBuffering, stdout)
import Test.Tasty.HUnit (HUnitFailure (..))

import qualified Clash.Prelude as C
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MV

import PpuModel
import Snes.Apu
import Snes.Bus
import Snes.Cart
import Snes.Ppu
import Snes.RomPort
import Snes.Spec hiding (frames)
import Snes.Spec.Gauss (gauss)

data Opts = Opts { oFrames :: Int, oInput :: Maybe FilePath, oDdr :: Bool, oEvery :: Int, oMargin :: Int }

main :: IO ()
main = do
  -- (a line at a time: a run stopped part way keeps what it printed)
  hSetBuffering stdout LineBuffering
  args <- getArgs
  case args of
    rom : rest -> follow rom (opts rest (Opts 600 Nothing False 100 48000))
    _ -> putStrLn "usage: snes-follow ROM [--frames N] [--input FILE] [--ddr] [--every K]" >> exitFailure
 where
  opts ("--frames" : n : r) o = opts r o { oFrames = read n }
  opts ("--input" : f : r) o = opts r o { oInput = Just f }
  opts ("--ddr" : r) o = opts r o { oDdr = True }
  opts ("--every" : n : r) o = opts r o { oEvery = read n }
  opts ("--margin" : n : r) o = opts r o { oMargin = read n }
  opts _ o = o

-- | The core's state between clocks.
data St = St BusC PpuC ApuC BusIn PpuIn ApuIn

follow :: FilePath -> Opts -> IO ()
follow path o = do
  file <- readRom path
  iplB <- readRom "build/snes/ipl.bin"
  cart <- maybe (putStrLn (path <> ": no header") >> exitFailure) pure (parseCart file)
  inputs <- maybe (pure []) (fmap parseInput . readFile) (oInput o)
  let rom = U.fromList (B.unpack (fst (stripCopier file)))
      romSize = U.length rom
      p = if romSize == 0 then 0 else bit (finiteBitSize romSize - 1 - countLeadingZeros romSize)
      q = romSize - p
      cfg = CartCfg
        { ccMap = case cartMapping cart of LoRom -> CmLoRom; HiRom -> CmHiRom; ExHiRom -> CmExHiRom
        , ccRomP = fromIntegral p, ccRomQ = fromIntegral q, ccSram = fromIntegral (cartSramSize cart) }
      ipl = vecOf (map fromIntegral (B.unpack iplB)) :: C.Vec 64 (C.Unsigned 8)
      ddr = oDdr o
  -- the spec, a frame at a time
  m <- stToIO (newMachine iplB cart file)
  stToIO (traceOn m >> busTraceOn m >> reset m >> setPads m 0 0)
  -- the core's memories
  aram <- MV.replicate 0x10000 (0 :: Word8)
  smp <- MV.replicate 256 (0 :: Int)
  wramK <- MV.replicate 0x20000 (0x55 :: Word8)
  dmaK <- MV.replicate 128 (0xFF :: Word8)
  sramK <- MV.replicate (max 1 (cartSramSize cart)) (0xFF :: Word8)
  pend <- newIORef (Nothing :: Maybe (Int, C.Unsigned 23))
  recent <- newIORef ([] :: [Int])
  ddrSeed <- newIORef (777 :: Int)
  romPort <- newIORef romPortInit
  romRead <- newIORef Nothing
  mems <- newMems
  seed <- newIORef (4242 :: Int)
  evs <- newIORef []
  frameErr <- newIORef Nothing
  -- the held clocks, by line of the frame (all, and waiting for the sound module)
  held <- MV.replicate 263 (0 :: Int)
  heldApu <- MV.replicate 263 (0 :: Int)
  frameMc0 <- newIORef (0 :: Int)
  lagR <- newIORef (0 :: Int)
  let clock :: St -> [(Int, U.Vector Word16)] -> Int -> Int -> IO (St, [(Int, U.Vector Word16)])
      clock st@(St bus ppu apuK bin pin ain) frs mcEnd nClk = do
        r <- rand seed
        let run = r `mod` 6 /= 0
            pre = snd (ppuClock ppu pin { piTick = False, piAccess = Nothing })
            apre = snd (apuClock apuK ain { aiTick = False, aiPort = Nothing })
            binA = bin { biApuReady0 = aoReady0 apre, biApuReady1 = aoReady1 apre, biApuRead = aoRead apre }
            (bus', bo) = busClock bus binA { biRun = run, biPpuRead = poRead pre }
            (ppu', po) = ppuClock ppu pin { piTick = boTick bo, piAccess = boPpu bo }
            (apuK', ao) = apuClock apuK ain { aiTick = boTick bo, aiPort = boApu bo }
            mc = fromIntegral (bMc bus') :: Int
        forM_ (boEv bo) $ \(t, e) -> modifyIORef' evs ((fromIntegral t, e) :)
        when (run && not (boTick bo)) $ do
          f0 <- readIORef frameMc0
          let v = min 262 (max 0 ((mc - f0) `div` 1364))
          MV.modify held (+ 1) v
          unless (aoReady0 apre) $ MV.modify heldApu (+ 1) v
        wv <- MV.read wramK (fromIntegral (boWram bo))
        forM_ (boWramW bo) $ \v -> MV.write wramK (fromIntegral (boWram bo)) (fromIntegral v)
        let sa = fromIntegral (boSram bo) `mod` MV.length sramK
        sv <- MV.read sramK sa
        forM_ (boSramW bo) $ \v -> MV.write sramK sa (fromIntegral v)
        dmaV <- mapM (\k -> MV.read dmaK (fromEnum (boDmaRd bo) * 16 + k)) [0 .. 15]
        forM_ (zip [0 ..] (C.toList (boDmaW bo))) $ \(k, w) ->
          forM_ w $ \v -> MV.write dmaK (fromEnum (boDmaWCh bo) * 16 + k) (fromIntegral v)
        romV <- if not ddr then pure (fmap (\a -> fromIntegral (rom U.! (fromIntegral a `mod` U.length rom))) (boRom bo)) else do
          pd <- readIORef pend
          ans <- case pd of
            Just (0, a) -> do
              writeIORef pend Nothing
              let base = fromIntegral a .&. complement 7
              pure (Just (vecOf [ fromIntegral (rom U.! ((base + k) `mod` U.length rom)) | k <- [0 .. 7] ]))
            Just (k, a) -> writeIORef pend (Just (k - 1, a)) >> pure Nothing
            Nothing -> pure Nothing
          -- the bus's read registered before the ROM port, as in Snes.Core
          rd <- readIORef romRead
          writeIORef romRead (boRom bo)
          rp <- readIORef romPort
          let (rp', ro) = romPortStep rp (RomIn rd ans)
          writeIORef romPort rp'
          forM_ (roRequest ro) $ \a -> do
            ls <- readIORef recent
            let line = fromIntegral a `div` 32
            lat <- if line `elem` ls then pure 0 else do
              writeIORef recent (take 16 (line : ls))
              rr <- rand ddrSeed
              pure (rr `mod` 30 + 5)
            writeIORef pend (Just (lat, a))
          pure (rpOut rp')
        -- ARAM's two ports (the SPC700's, the DSP's): both read, then both write
        aramV <- MV.read aram (fromIntegral (aoAram ao))
        dramV <- MV.read aram (fromIntegral (aoDAram ao))
        forM_ (aoAramW ao) $ \v -> MV.write aram (fromIntegral (aoAram ao)) (fromIntegral v)
        forM_ (aoDAramW ao) $ \v -> MV.write aram (fromIntegral (aoDAram ao)) (fromIntegral v)
        smpV <- MV.read smp (fromIntegral (aoSmpRd ao))
        forM_ (aoSmpW ao) $ \(a, v) -> MV.write smp (fromIntegral a) (fromIntegral v)
        let ain' = ApuIn False Nothing (fromIntegral aramV) (fromIntegral dramV) (ipl C.!! aoIpl ao) (fromIntegral smpV) (fromIntegral (gauss U.! fromIntegral (aoGauss ao)))
        case poPixel po of
          Just (x, row, l, rr) -> do
            let i = fromIntegral row * 512 + 2 * fromIntegral x
            MV.write (mFrame mems) i (fromIntegral l)
            MV.write (mFrame mems) (i + 1) (fromIntegral rr)
          Nothing -> pure ()
        frs' <- case frs of
          (t, fr) : rest | boTick bo && mc == t -> do
            res <- try (compareFrame mems 0 fr)
            case res of
              Left (HUnitFailure _ msg) -> modifyIORef' frameErr (\e -> maybe (Just msg) Just e)
              Right () -> pure ()
            pure rest
          _ -> pure frs
        pin' <- memories mems po
        let bin' = bin { biWram = fromIntegral wv, biSram = fromIntegral sv, biRom = romV, biApuRead = 0
                       , biDma = vecOf (map fromIntegral dmaV) }
            st' = St bus' ppu' apuK' bin' pin' ain'
        if mc >= mcEnd && boStep bo /= Nothing then pure (st', frs')
          else if nClk > 40 * 400000 then putStrLn "the core never reached the frame's end" >> exitFailure >> pure (st, frs)
          else clock st' frs' mcEnd (nClk + 1)
      frameLoop :: St -> Int -> IO ()
      frameLoop st k
        | k >= oFrames o = putStrLn ("snes-follow: " <> show k <> " frames, the core the same as the spec")
        | otherwise = do
            let padNow = [ b | (f0, b) <- inputs, f0 == k ]
            forM_ padNow $ \b -> stToIO (setPads m b 0)
            stToIO (runFrames m 1)
            mcEnd <- stToIO (masterClocks m)
            specEvs <- stToIO (takeBusTrace m)
            pt <- stToIO (takeTrace m)
            let specFrames = [ (t, fr) | PpuFrameDone t fr <- pt ]
                St b0 p0 a0 bin0 pin0 ain0 = st
                bin1 = case padNow of
                  b : _ -> bin0 { biPads = vecOf [fromIntegral b, 0] }
                  [] -> bin0
            (st', _) <- clock (St b0 p0 a0 bin1 pin0 ain0) specFrames mcEnd 0
            -- the frame's holds through the pace
            hs <- U.freeze held
            ha <- U.freeze heldApu
            MV.set held 0
            MV.set heldApu 0
            writeIORef frameMc0 mcEnd
            lag0 <- readIORef lagR
            let pstep (lag, mx, lt) v =
                  let lag' = max 0 (lag + hs U.! v - 236)
                  in (lag', max mx lag', lt + (if v >= 2 && v < 226 && lag' > oMargin o then 1 else 0 :: Int))
                (lagEnd, maxLag, late) = foldl' pstep (lag0, 0 :: Int, 0) [0 .. 262]
            writeIORef lagR lagEnd
            when (maxLag > 0) $
              putStrLn ("held: frame " <> show k <> ": " <> show (U.sum hs) <> " clocks, " <> show (U.sum ha) <> " waiting for the sound module; largest lag "
                        <> show maxLag <> ", rows late " <> show late)
            all' <- readIORef evs
            let (mine, later) = partition (\(t, _) -> t <= mcEnd) all'
                got = map (\(t, e) -> toSpec t e) (sortOn fst (reverse mine))
            writeIORef evs later
            let firstBad = [ i | (i, a, b) <- zip3 [0 :: Int ..] got specEvs, a /= b ]
            case firstBad of
              i : _ -> do
                putStrLn ("snes-follow: frame " <> show k <> ", bus event " <> show i <> ": the core " <> show (got !! i) <> ", the spec " <> show (specEvs !! i))
                putStrLn ("  the spec around: " <> show (take 30 (drop (max 0 (i - 20)) specEvs)))
                putStrLn ("  the core around: " <> show (take 30 (drop (max 0 (i - 20)) got)))
                exitFailure
              [] -> unless (length got == length specEvs) $ do
                putStrLn ("snes-follow: frame " <> show k <> ": " <> show (length got) <> " bus events, the spec " <> show (length specEvs)
                          <> "; the first extra or missing: " <> show (take 3 (drop (min (length got) (length specEvs)) (if length got > length specEvs then got else specEvs))))
                exitFailure
            fe <- readIORef frameErr
            forM_ fe $ \msg -> putStrLn ("snes-follow: frame " <> show k <> ", the picture: " <> msg) >> exitFailure
            when ((k + 1) `mod` oEvery o == 0) $ putStrLn ("snes-follow: frame " <> show (k + 1) <> " the same (" <> show (length specEvs) <> " bus events in the last)")
            frameLoop st' (k + 1)
  frameLoop (St busInit ppuInit apuInit (busIdleIn cfg) ppuIdleIn apuIdleIn) 0

-- | The core's event as the spec records it.
toSpec :: Int -> BusEv -> BusEvent
toSpec t e = case e of
  EvRead a v -> BeRead t (fromIntegral a) (fromIntegral v)
  EvWrite a v -> BeWrite t (fromIntegral a) (fromIntegral v)
  EvIdle -> BeIdle t
  EvDma a b v toA -> BeDma t (fromIntegral a) (fromIntegral b) (fromIntegral v) toA
  EvTable a v -> BeTable t (fromIntegral a) (fromIntegral v)

parseInput :: String -> [(Int, Int)]
parseInput t = [ (read f, read b) | l <- lines t, (f : b : _) <- [words (takeWhile (/= '#') l)] ]
