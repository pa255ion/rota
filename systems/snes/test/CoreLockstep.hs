{-# LANGUAGE CPP #-}
-- | Layer 1, step 7.9a: the Clash 5A22 ("Snes.Bus") with the Clash PPU
-- ("Snes.Ppu") against the spec machine ("Snes.Spec"). Each ROM runs in
-- the spec with its bus work recorded ('busTraceOn': every CPU cycle, DMA
-- and HDMA byte and HDMA table read, at its master clock) and its specFrames;
-- then in the core, a pixel clock at a time, over models of its memories
-- (block RAMs: one clock's latency, read before write; the ROM a clock
-- late), with the master clock's enable held at random, as the 25.175 MHz
-- domain has it. The sound module is the Clash one ("Snes.Apu", step
-- 7.9b) over models of ARAM, its samples and its Gaussian table; with
-- CORE_SPEC_APU=1 a spec sound module of its own ("Snes.Spec.Apu") serves
-- the core's port accesses instead (step 7.9a), caught up at each port
-- access and each line's end as the spec catches its own up.
--
-- Checked: the bus work, in master-clock order, event for event; each
-- frame's picture as line 0 begins; WRAM, VRAM, CGRAM and OAM at the end;
-- the SPC700's writes and the DSP's samples, one list a prefix of the
-- other (the Clash module runs ahead of the spec's catch-ups).
-- CORE_ONLY="lorom dma": only those ROMs.
module Main (main) where

import Snes.Spec.SaveState (specAtRest, specTable, tableFields)
import qualified Snes.Spec.Machine as SM
import Snes.StateTap (tableOf)
import W65c816.Core (Cpu (..), Stage (..), finished)
import Prelude

import Control.Exception (try)
import Control.Monad (forM_, unless, when)
import Control.Monad.ST (stToIO)
import qualified Data.ByteString as B
import Data.Bits
import Data.IORef
import Data.List (sortOn)
import Data.Word (Word16, Word8)
import System.Environment (lookupEnv)
import System.FilePath ((</>), takeFileName)
import Test.Tasty
import Test.Tasty.HUnit

import qualified Clash.Prelude as C
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MV

import PpuModel
import Snes.Apu
import Snes.Bus
import Snes.RomPort
import qualified Spc700.Core as S
import Snes.Cart
import Snes.Ppu
import Snes.Spec hiding (frames)
import Snes.Spec.Gauss (gauss)
#ifdef SNES_DSP1
import Snes.Dsp1
import Snes.Spec.Upd77c25 (Firmware (..), firmwareOf)
import Snes.Spec.Upd77c25Asm (dspTestFirmware)
import Snes.Upd77c25 (ReqAddrs (..))
#endif

asmDir, romDir, iplFile :: FilePath
asmDir = "../../build/snes/asm"
romDir = "../../roms/snes"
iplFile = "../../build/snes/ipl.bin"

main :: IO ()
main = do
  full <- (== Just "1") <$> lookupEnv "EMU_FULL_TESTS"
  only <- maybe [] words <$> lookupEnv "CORE_ONLY"
  let pick = if null only then roms else [ r | r@(_, f, _) <- roms, takeWhile (/= '.') f `elem` only ]
  defaultMain $ testGroup "snes-core"
    [ testCase (f <> ", " <> show k <> " frames" <> label) (coreTest ddr slow (dir </> f) k)
    | (dir, f, n) <- pick, let k = if full then max n 30 else n
    , let name = takeWhile (/= '.') f
    , (ddr, slow, label) <- (False, False, "") : [ (True, False, ", the ROM from DDR") | name `elem` ddrRoms ]
                              <> [ (True, True, ", the ROM from a slow DDR") | name `elem` slowDdrRoms ] ]

-- | The ROMs also run with the ROM behind "Snes.RomPort" and a DDR of
-- random latency (step 7.9c).
ddrRoms :: [String]
ddrRoms = ["dma", "busfuzz", "spcfuzz", "dsprace", "shmup_1942", "unnamed-tech-demo-v3", "Mode7HDMA"]

-- | And with every ROM read slow (5 to 35 clocks, no line answered at
-- once): a CPU read from ROM still out when HDMA starts (the freeze hunt,
-- 2026-10-21: the CPU got HDMA's byte or 0, and Rockman & Forte ran into a
-- BRK; Mode7HDMA and RedSpaceIndirectHDMA diverged here before the fix in
-- "Snes.Bus", busClock's romWait). CORE_DDR_SLOW=1 makes every DDR run slow.
slowDdrRoms :: [String]
slowDdrRoms = ["Mode7HDMA", "RedSpaceIndirectHDMA", "WaveHDMA"]

-- | Our register tests (they stop at their result), krom's CPU tests,
-- picture tests with DMA and HDMA, the two games. (The DSP-1 build, cabal
-- -f dsp1: our DSP-1 test cartridge alone, with our firmware: that build
-- gives every cartridge the chip's windows, step 7.16d.)
roms :: [(FilePath, FilePath, Int)]
#ifdef SNES_DSP1
roms = [ (asmDir, "dsp1.sfc", 8) ]

-- | Our DSP-1 test cartridge's firmware.
dspFw :: Firmware
dspFw = either error id (firmwareOf dspTestFirmware)
#else
roms =
  [ (asmDir, "lorom.sfc", 3)
  , (asmDir, "hirom.sfc", 3)
  , (asmDir, "map.sfc", 3)
  , (asmDir, "muldiv.sfc", 3)
  , (asmDir, "nmi.sfc", 4)
  , (asmDir, "virq.sfc", 4)
  , (asmDir, "joy.sfc", 4)
  , (asmDir, "sram.sfc", 3)
  , (asmDir, "dma.sfc", 4)
  , (asmDir, "ppuregs.sfc", 8)
  , (asmDir, "busfuzz.sfc", 6)
  , (romDir </> "krom", "CPUADC.sfc", 3)
  , (romDir </> "krom", "SPC700ADC.sfc", 3)
  , (romDir </> "gilyon", "spctest.sfc", 8)
  , (asmDir, "tone.sfc", 6)
  , (asmDir, "spcfuzz.sfc", 6)
  , (asmDir, "dsprace.sfc", 8)
  , (romDir </> "krom", "Mode7HDMA.sfc", 6)
  , (romDir </> "krom", "8x8BG1Map2BPP32x328PAL.sfc", 10)
  , (romDir </> "krom", "WindowMultiHDMA.sfc", 6)
    -- HDMA tables in ROM, HDMA beside the CPU's ROM fetches (the freeze
    -- hunt, 2026-10-21; with the ROM from DDR)
  , (romDir </> "krom", "WaveHDMA.sfc", 6)
  , (romDir </> "krom", "RedSpaceIndirectHDMA.sfc", 6)
  , (romDir </> "opensnes", "shmup_1942.sfc", 6)
  , (romDir </> "games", "unnamed-tech-demo-v3.sfc", 6)
  ]
#endif

coreTest :: Bool -> Bool -> FilePath -> Int -> Assertion
coreTest ddr slowCase path n = do
  slow <- (\e -> slowCase || e == Just "1") <$> lookupEnv "CORE_DDR_SLOW"
  file <- readRom path
  iplB <- readRom iplFile
  cart <- maybe (assertFailure (path <> ": no header") >> fail "") pure (parseCart file)
  -- the spec
  (specEvs, specFrames, wram, vram, cg, oam, mcEnd, apuEvs, specTables) <- stToIO $ do
    m <- newMachine iplB cart file
    traceOn m
    busTraceOn m
    apuTraceOn (apuOf m)
    reset m
#ifdef SNES_DSP1
    attachDsp m dspFw
#endif
    -- (runFrames, instruction by instruction: the state table at each
    -- frame's first moment at rest, for the core's tap, step 7.15c)
    f0 <- SM.frames m
    let run1 lastF acc = do
          f <- SM.frames m
          stopped <- cpuStopped m
          if f >= f0 + n || stopped then pure acc else do
            stepCpu m
            atRest' <- specAtRest m
            f' <- SM.frames m
            if atRest' && Just f' /= lastF
              then do
                t <- specTable m
                mc <- masterClocks m
                run1 (Just f') ((mc, t) : acc)
              else run1 lastF acc
    tabs <- reverse <$> run1 Nothing []
    bt <- takeBusTrace m
    pt <- takeTrace m
    w <- U.fromList <$> mapM (readWram m) [0 .. 0x1FFFF]
    v <- U.fromList <$> mapM (readVram m) [0 .. 0xFFFF]
    g <- cgramColors m
    o <- mapM (readOam m) [0 .. 543]
    mc <- masterClocks m
    ae <- apuTakeTrace (apuOf m)
    pure (bt, [ (t, fr) | PpuFrameDone t fr <- pt ], w, v, g, o, mc, ae, tabs)
  -- the core
  let rom = U.fromList (B.unpack (fst (stripCopier file)))
      romSize = U.length rom
      p = if romSize == 0 then 0 else bit (finiteBitSize romSize - 1 - countLeadingZeros romSize)
      q = romSize - p
  unless (q == 0 || popCount q == 1) $ assertFailure ("a ROM of " <> show romSize <> " bytes: not two powers of two")
  let cfg = CartCfg
        { ccMap = case cartMapping cart of LoRom -> CmLoRom; HiRom -> CmHiRom; ExHiRom -> CmExHiRom
        , ccRomP = fromIntegral p, ccRomQ = fromIntegral q, ccSram = fromIntegral (cartSramSize cart) }
  apu <- stToIO (newApu (U.fromList (B.unpack iplB)))
  specApu <- (== Just "1") <$> lookupEnv "CORE_SPEC_APU"
  dbgPorts <- (== Just "2") <$> lookupEnv "CORE_DEBUG"
  let ipl = vecOf (map fromIntegral (B.unpack iplB)) :: C.Vec 64 (C.Unsigned 8)
  aram <- MV.replicate 0x10000 (0 :: Word8)
  smp <- MV.replicate 256 (0 :: Int)
  apuW <- newIORef []
  apuS <- newIORef []
  wramK <- MV.replicate 0x20000 (0x55 :: Word8)
  dmaK <- MV.replicate 128 (0xFF :: Word8)
  -- the DDR port's stand-in (as gg-lockstep's): the 32-byte lines of the
  -- last 16 misses answer the next clock, other reads after 5 to 34
  pend <- newIORef (Nothing :: Maybe (Int, C.Unsigned 23))
  recent <- newIORef ([] :: [Int])
  ddrSeed <- newIORef (777 :: Int)
  romPort <- newIORef romPortInit
  romRead <- newIORef Nothing
  stalls <- newIORef (0 :: Int)
  sramK <- MV.replicate (max 1 (cartSramSize cart)) (0xFF :: Word8)
  mems <- newMems
  seed <- newIORef (4242 :: Int)
  evs <- newIORef []
  -- a frame's difference is reported after the bus work's, which says where
  frameErr <- newIORef Nothing
  tapPend <- newIORef specTables
  tapErr <- newIORef Nothing
  tapMissed <- newIORef (0 :: Int)
  tapDbg <- (== Just "1") <$> lookupEnv "CORE_TAPDBG"
#ifdef SNES_DSP1
  -- the DSP-1 (its firmware put in directly: the load through the ROM port
  -- is layer 4's), its memories as block RAMs (last clock's addresses)
  dspR <- newIORef dspInit { dsLoad = LDone }
  dspAddr <- newIORef (ReqAddrs 0 0 0 0)
  dspRam <- MV.replicate 256 (0 :: Word16)
#endif
  peek <- maybe [] (map read . words) <$> lookupEnv "CORE_PEEK"
  let (peekLo, peekHi) = case peek of
        [a, b] -> (a, b)
        _ -> (1, 0) :: (Int, Int)
      target mc = (mc * 5632) `div` 118125
      go :: BusC -> PpuC -> ApuC -> BusIn -> PpuIn -> ApuIn -> [(Int, U.Vector Word16)] -> Int -> IO (BusC, PpuC)
      go bus ppu apuK bin pin ain frs nClk = do
        r <- rand seed
#ifdef SNES_DSP1
        dsp <- readIORef dspR
#endif
        let run = r `mod` 6 /= 0
            -- the PPU's read value of last clock's access, the sound
            -- module's readiness and read (from their states)
            pre = snd (ppuClock ppu pin { piTick = False, piAccess = Nothing })
            apre = snd (apuClock apuK ain { aiTick = False, aiPort = Nothing })
            binA = if specApu then bin else bin { biApuReady0 = aoReady0 apre, biApuReady1 = aoReady1 apre, biApuRead = aoRead apre }
#ifdef SNES_DSP1
            (bus', bo) = busClock bus binA { biRun = run, biPpuRead = poRead pre
                                           , biDspReady0 = dspReady0 dsp, biDspReady1 = dspReady1 dsp, biDspRead = dsByte dsp }
#else
            (bus', bo) = busClock bus binA { biRun = run, biPpuRead = poRead pre }
#endif
            (ppu', po) = ppuClock ppu pin { piTick = boTick bo, piAccess = boPpu bo }
            (apuK', ao) = apuClock apuK ain { aiTick = boTick bo, aiPort = if specApu then Nothing else boApu bo }
            mc = fromIntegral (bMc bus') :: Int
        forM_ (boEv bo) $ \(t, e) -> modifyIORef' evs ((fromIntegral t, e) :)
        -- the state table's tap (step 7.15c): at the spec's moments at rest,
        -- the CPU at its instruction's end
        -- (CORE_TAPDBG=1: the CPU's stage and PC around the next moment)
        when tapDbg $ do
          pendD <- readIORef tapPend
          case pendD of
            (tmc, _) : _ | abs (mc - tmc) < 60 -> putStrLn ("TAPDBG mc " <> show mc <> " stage " <> show (cStage (bCpu bus'))
                <> " phase " <> show (cPhase (bCpu bus')) <> " PC " <> show (cPC (bCpu bus')) <> " A " <> show (cA (bCpu bus'))
                <> " tick " <> show (boTick bo) <> " target " <> show tmc)
            _ -> pure ()
        when (cStage (bCpu bus') == StDone) $ do
          pendT <- readIORef tapPend
          case pendT of
            (tmc, tw) : rest | tmc == mc -> do
              writeIORef tapPend rest
              let got' = U.fromList (concatMap le32 (C.toList (tableOf bus' { bCpu = finished (bCpu bus') } ppu')))
                  le32 v = [ fromIntegral (v `C.shiftR` (8 * k)) | k <- [0 .. 3 :: Int] ] :: [Word8]
                  bad = [ (fieldAt o, tw U.! o, got' U.! o) | o <- [0 .. U.length tw - 1], tw U.! o /= got' U.! o ]
              unless (null bad) $ modifyIORef' tapErr (\e -> maybe (Just ("the tap at master clock " <> show mc <> ": "
                <> unwords [ nm <> " spec " <> show a <> " core " <> show b' | (nm, a, b') <- take 8 bad ])) Just e)
            (tmc, _) : rest | tmc < mc -> writeIORef tapPend rest >> modifyIORef' tapMissed (+ 1)
            _ -> pure ()
        -- CORE_PEEK="from to": the core's HDMA state on each master clock in that range
        when (boTick bo && mc >= peekLo && mc <= peekHi) $
          putStrLn (show mc <> " H " <> show (bHc bus') <> " V " <> show (bVc bus') <> " fg " <> show (fStep bus') <> " left " <> show (fLeft bus')
                    <> " ch " <> show (fCh bus') <> " chans " <> show (fChans bus') <> " done " <> show (bHdmaDone bus') <> " do " <> show (bHdmaDo bus')
                    <> " bg " <> show (bStep bus') <> " bleft " <> show (bLeft bus'))
        -- the memories for the next clock
        wv <- MV.read wramK (fromIntegral (boWram bo))
        forM_ (boWramW bo) $ \v -> MV.write wramK (fromIntegral (boWram bo)) (fromIntegral v)
        let sa = fromIntegral (boSram bo) `mod` MV.length sramK
        sv <- MV.read sramK sa
        forM_ (boSramW bo) $ \v -> MV.write sramK sa (fromIntegral v)
        -- the DMA registers: a channel read (all 16), then the writes
        dmaV <- mapM (\k -> MV.read dmaK (fromEnum (boDmaRd bo) * 16 + k)) [0 .. 15]
        forM_ (zip [0 ..] (C.toList (boDmaW bo))) $ \(k, w) ->
          forM_ w $ \v -> MV.write dmaK (fromEnum (boDmaWCh bo) * 16 + k) (fromIntegral v)
        romV <- if not ddr then pure (fmap (\a -> fromIntegral (rom U.! fromIntegral a)) (boRom bo)) else do
          -- the answer due this clock, the port's step, a new request
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
            -- (CORE_DDR_SLOW=1: every read slow, no recent lines: a read
            -- in flight longer, as a real DDR miss; the freeze hunt)
            lat <- if line `elem` ls && not slow then pure 0 else do
              writeIORef recent (take 16 (line : ls))
              rr <- rand ddrSeed
              pure (rr `mod` 30 + 5)
            writeIORef pend (Just (lat, a))
          when (not (boTick bo) && run) $ modifyIORef' stalls (+ 1)
          pure (rpOut rp')
#ifdef SNES_DSP1
        -- the DSP-1's clock, its memories at last clock's addresses
        ra <- readIORef dspAddr
        mv <- MV.read dspRam (fromIntegral (raDp ra))
        mk <- MV.read dspRam (fromIntegral (raDpK ra))
        let dIn = DspIn (boTick bo) (boDsp bo) Nothing (fromIntegral (fwProgram dspFw U.! fromIntegral (raPc ra)))
                        (fromIntegral (fwData dspFw U.! fromIntegral (raRp ra))) (fromIntegral mv) (fromIntegral mk)
            (dsp', dOut) = dspClock dsp dIn
        forM_ (doRam dOut) $ \(a, v) -> MV.write dspRam (fromIntegral a) (fromIntegral v)
        writeIORef dspR dsp'
        writeIORef dspAddr (dspReq dsp)
#endif
        -- the Clash sound module's memories
        -- ARAM's two ports (the SPC700's, the DSP's): both read, then both write
        aramV <- MV.read aram (fromIntegral (aoAram ao))
        dramV <- MV.read aram (fromIntegral (aoDAram ao))
        forM_ (aoAramW ao) $ \v -> MV.write aram (fromIntegral (aoAram ao)) (fromIntegral v)
        forM_ (aoDAramW ao) $ \v -> MV.write aram (fromIntegral (aoDAram ao)) (fromIntegral v)
        smpV <- MV.read smp (fromIntegral (aoSmpRd ao))
        forM_ (aoSmpW ao) $ \(a, v) -> MV.write smp (fromIntegral a) (fromIntegral v)
        forM_ (aoWrite ao) $ \(n', a, v) -> modifyIORef' apuW ((fromIntegral n', fromIntegral a, fromIntegral v) :)
        when dbgPorts $ forM_ (boApu bo) $ \(port, w) -> case w of
          Just v -> putStrLn ("CPU writes port " <> show port <> " = " <> show v <> " at " <> show (aN apuK))
          Nothing -> pure ()
        -- CORE_DEBUG=2: the SPC700's reads of the ports
        when (dbgPorts && aPhase apuK == AComplete) $ case S.cpuReq (aCpu apuK) of
          S.RRead a | a >= 0xF4 && aN apuK > 20000 && (a <= 0xF7 || a >= 0xFD) -> putStrLn ("SPC reads " <> show a <> " = " <> show (aData apuK) <> " at " <> show (aN apuK) <> ", in " <> show (aIn apuK))
          _ -> pure ()
        forM_ (aoSample ao) $ \(l, rr) -> modifyIORef' apuS ((fromIntegral (aoSampleN ao), fromIntegral l, fromIntegral rr) :)
        let ain' = ApuIn False Nothing (fromIntegral aramV) (fromIntegral dramV) (ipl C.!! aoIpl ao) (fromIntegral smpV) (fromIntegral (gauss U.! fromIntegral (aoGauss ao)))
        -- or the spec's sound module at the ports
        apuV <- case boApu bo of
          Just (port, w) | specApu -> stToIO $ do
            runApuTo apu (target mc)
            case w of
              Just v -> apuPortWrite apu (fromIntegral port) (fromIntegral v) >> pure 0
              Nothing -> apuPortRead apu (fromIntegral port)
          _ -> pure 0
        when (boLineEnd bo && specApu) $ stToIO (runApuTo apu (target mc))
        -- the pixel
        case poPixel po of
          Just (x, row, l, rr) -> do
            let i = fromIntegral row * 512 + 2 * fromIntegral x
            MV.write (mFrame mems) i (fromIntegral l)
            MV.write (mFrame mems) (i + 1) (fromIntegral rr)
          Nothing -> pure ()
        -- a frame of the spec's begun: compare
        frs' <- case frs of
          (t, fr) : rest | boTick bo && mc == t -> do
            res <- try (compareFrame mems (length specFrames - length frs) fr)
            case res of
              Left (HUnitFailure _ msg) -> modifyIORef' frameErr (\e -> maybe (Just msg) Just e)
              Right () -> pure ()
            pure rest
          _ -> pure frs
        pin' <- memories mems po
        let bin' = bin { biWram = fromIntegral wv, biSram = fromIntegral sv, biRom = romV, biApuRead = fromIntegral apuV
                       , biDma = vecOf (map fromIntegral dmaV) }
            stop = mc >= mcEnd && boStep bo /= Nothing
        if stop then pure (bus', ppu')
          else if nClk > 40 * (mcEnd + 100000) then assertFailure "the core never reached the spec's end" >> pure (bus', ppu')
          else go bus' ppu' apuK' bin' pin' ain' frs' (nClk + 1)
  (busEnd, ppuEnd) <- go busInit ppuInit apuInit (busIdleIn cfg) ppuIdleIn apuIdleIn specFrames 0
  -- the bus work, in master-clock order
  got <- map (\(t, e) -> toSpec t e) . sortOn fst . reverse <$> readIORef evs
  -- CORE_DUMP="from to": both lists' events in that master-clock range
  dump <- maybe [] (map read . words) <$> lookupEnv "CORE_DUMP"
  case dump of
    [lo, hi] -> do
      let inR e = let t = evMc e in t >= lo && t <= hi
      putStrLn ("\nthe spec:\n" <> unlines (map show (filter inR specEvs)))
      putStrLn ("the core:\n" <> unlines (map show (filter inR got)))
    _ -> pure ()
  let firstBad = [ k | (k, a, b) <- zip3 [0 :: Int ..] got specEvs, a /= b ]
  case firstBad of
    k : _ -> assertFailure ("bus event " <> show k <> ": " <> show (got !! k) <> ", the spec " <> show (specEvs !! k)
                            <> "\n  the spec around: " <> show (take 30 (drop (max 0 (k - 20)) specEvs))
                            <> "\n  the core around: " <> show (take 30 (drop (max 0 (k - 20)) got)))
    [] -> unless (length got == length specEvs) $
            assertFailure (show (length got) <> " bus events, the spec " <> show (length specEvs)
                           <> "; the first extra or missing: " <> show (take 3 (drop (min (length got) (length specEvs)) (if length got > length specEvs then got else specEvs))))
  readIORef frameErr >>= maybe (pure ()) assertFailure
  -- the tap: every moment met, every field the spec's
  readIORef tapErr >>= maybe (pure ()) assertFailure
  missed <- readIORef tapMissed
  left <- length <$> readIORef tapPend
  unless (missed == 0 && left == 0) $ assertFailure ("the tap: " <> show missed <> " of the spec's moments at rest never met at an instruction's end, "
                                                     <> show left <> " left over (of " <> show (length specTables) <> ")")
  -- the DDR's latency held the machine (else this run tested nothing more)
  when ddr $ do
    k <- readIORef stalls
    dbgS <- lookupEnv "CORE_DEBUG"
    when (dbgS == Just "1") $ putStrLn ("held clocks: " <> show k)
    when (k == 0 && length specEvs > 1000) $ assertFailure "the DDR never held the machine"

  -- the sound module: its writes and samples, the shorter list a prefix of
  -- the longer
  unless specApu $ do
    gotW <- reverse <$> readIORef apuW
    gotS <- reverse <$> readIORef apuS
    let wantW = [ (n', a, fromIntegral v) | EvAram n' a v <- apuEvs ] :: [(Int, Int, Int)]
        wantS = [ (n', l, rr) | EvSample n' l rr <- apuEvs ]
        prefix what xs ys =
          case [ k | (k, x, y) <- zip3 [0 :: Int ..] xs ys, x /= y ] of
            k : _ -> assertFailure ("the sound module's " <> what <> " " <> show k <> ": " <> show (xs !! k) <> ", the spec " <> show (ys !! k)
                                    <> "; before: " <> show (take 8 (drop (max 0 (k - 8)) ys)))
            [] -> pure ()
    dbg <- lookupEnv "CORE_DEBUG"
    when (dbg == Just "1") $ putStrLn ("sound module: " <> show (length gotW) <> " writes, the spec " <> show (length wantW)
                                       <> "; the last: " <> show (take 12 (drop (length wantW - 12) wantW))
                                       <> "; log " <> show (length [ () | (_, a, _) <- wantW, a >= 0x1000, a < 0x2000 ])
                                       <> ", ports " <> show (length [ () | (_, a, _) <- wantW, a >= 0xF4, a <= 0xF7 ])
                                       <> ", control " <> show (take 30 [ v | (_, a, v) <- wantW, a == 0xF1 ])
                                       <> ", the log " <> show (take 120 [ v | (_, a, v) <- wantW, a >= 0x1000, a < 0x2000 ])
                                       <> "; CPU port writes " <> show (length [ () | BeWrite _ a _ <- specEvs, a >= 0x2140, a <= 0x2143 ])
                                       <> ", the last " <> show (reverse (take 6 (reverse [ (t, a, v) | BeWrite t a v <- specEvs, a >= 0x2140, a <= 0x2143 ])))
                                       <> ", the last CPU reads " <> show (reverse (take 6 (reverse [ (t, a, v) | BeRead t a v <- specEvs ]))))
    prefix "write" gotW wantW
    prefix "sample" gotS wantS
    when (length wantW > 100 && min (length gotW) (length wantW) * 10 < length wantW * 9) $
      assertFailure ("the sound module made " <> show (length gotW) <> " writes, the spec " <> show (length wantW))
  -- the memories at the end
  wramF <- U.freeze wramK
  let badW = [ a | a <- [0 .. 0x1FFFF], wramF U.! a /= wram U.! a ]
  unless (null badW) $ assertFailure (show (length badW) <> " WRAM bytes differ, the first at " <> show (take 4 badW))
  forM_ [0 .. 0x7FFF] $ \w -> do
    lo <- MV.read (mVlo mems) w
    hi <- MV.read (mVhi mems) w
    when ((lo, hi) /= (vram U.! (2 * w), vram U.! (2 * w + 1))) $
      assertFailure ("VRAM word " <> show w <> ": " <> show (lo, hi) <> ", the spec " <> show (vram U.! (2 * w), vram U.! (2 * w + 1)))
  cgs <- mapM (MV.read (mCg mems)) [0 .. 255]
  unless (map fromIntegral cgs == map (.&. 0x7FFF) cg) $ assertFailure "CGRAM differs"
  oams <- mapM (MV.read (mOamL mems)) [0 .. 511]
  unless (oams <> map fromIntegral (C.toList (pOamHigh ppuEnd)) == oam) $ assertFailure "OAM differs"
  _ <- pure (busEnd, takeFileName path)
  pure ()


-- | The core's event as the spec records it.
toSpec :: Int -> BusEv -> BusEvent
toSpec t e = case e of
  EvRead a v -> BeRead t (fromIntegral a) (fromIntegral v)
  EvWrite a v -> BeWrite t (fromIntegral a) (fromIntegral v)
  EvIdle -> BeIdle t
  EvDma a b v toA -> BeDma t (fromIntegral a) (fromIntegral b) (fromIntegral v) toA
  EvTable a v -> BeTable t (fromIntegral a) (fromIntegral v)

evMc :: BusEvent -> Int
evMc e = case e of
  BeRead t _ _ -> t
  BeWrite t _ _ -> t
  BeIdle t -> t
  BeDma t _ _ _ _ -> t
  BeTable t _ _ -> t

-- | The table's field at a byte offset (Snes.Spec.SaveState's names).
fieldAt :: Int -> String
fieldAt o = case [ nm | (nm, fo, sz) <- tableFields, o >= fo, o < fo + sz ] of
  nm : _ -> nm
  [] -> "?"
