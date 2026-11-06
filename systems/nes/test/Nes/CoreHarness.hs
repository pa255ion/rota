{-# LANGUAGE MultiWayIf #-}
{-|
Runs the whole Clash core ("Nes.Core", as plain Haskell) with its
memories in vectors, for the full-core tests of PLAN.md step 3.6: step by
step against the reference model (bus accesses of every CPU cycle and
the registers after every step), and alone, reading test results from
PRG RAM like the spec runners.

The harness answers the core's memory requests one clock later and holds
each port's byte until its next request, as the block RAMs will. PRG ROM
reads (in DDR on the boards) sometimes take longer: about one in 8 waits
1 to 31 more clocks ('kiRdOk' low, 'kiRd' a poison byte meanwhile), from
a fixed pseudo-random sequence, so every run checks that waiting changes
nothing.
-}
module Nes.CoreHarness
  ( CoreRun (..)
  , newCoreRun
  , coreRunStep
  , coreRunFor
  , corePeek
  , coreFrame
  , coreFramesWith
  , coreFramesPress
  , coreSetPads
  , coreCycles
  , StepRec (..)
  , lockstepCore
  , coreBlargg
  , coreOld
  , coreAccuracyCoin
  ) where

import Prelude

import Control.Monad (forM_, when)
import Control.Monad.ST (ST, runST)
import qualified Data.ByteString as BS
import Data.List (isInfixOf)
import Data.STRef
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MV
import Data.Word

import qualified Clash.Prelude as C
import Mos6502.Core (CpuRegs (..), StepKind (..))
import Mos6502.Spec (Regs (..), Step (..), getRegs)
import Nes.AccuracyCoin
import Nes.Cartridge
import Nes.Apu (Channels (..))
import Nes.Core
import Nes.Hooks
import Nes.Mapper (CartCfg (..), M5 (..), MapState (..), MapperKind (..), cartCfgFromHeader)
import Nes.Spec.Mapper (mapperCounter)
import qualified Nes.Spec.Apu as SA
import qualified Nes.Spec.Machine as S
import Nes.Ppu (Ppu (..))
import Nes.Spec.Ppu (ppuPosition)

data CoreRun s = CoreRun
  { crState  :: STRef s CoreState
  , crIn     :: STRef s CoreIn
  , crPrg    :: U.Vector Word8
  , crPrgRam :: MV.MVector s Word8
  , crRam    :: MV.MVector s Word8
  , crChr    :: MV.MVector s Word8
  , crCiram  :: MV.MVector s Word8
  , crExram  :: MV.MVector s Word8    -- ^ MMC5's 1 KiB
  , crOam    :: MV.MVector s Word8
  , crFrame  :: MV.MVector s Word16
  , crCycles :: STRef s Int
  , crSerial :: STRef s [Bool]       -- ^ bit 0 of $4016 writes, newest first
  , crFrames :: STRef s Int          -- ^ frames drawn
  , crWait   :: STRef s (Int, Word8)  -- ^ a PRG read's clocks still to wait, its byte
  , crRng    :: STRef s Word64
  , crOvl    :: MV.MVector s Word8    -- ^ side track N: the ROM overlay (2 KiB; 'kiOvlOn')
  }

newCoreRun :: BS.ByteString -> ST s (Either String (CoreRun s))
newCoreRun file = case parseRom file of
  Left e -> pure (Left e)
  Right rom -> case cartCfgFromHeader (romHeader rom) of
    Nothing -> pure (Left "unsupported cartridge")
    Just cfg -> do
      st <- newSTRef (coreInit cfg)
      inp <- newSTRef (CoreIn True 0 True 0 0 0 0 0 0 False)
      prgRam <- MV.replicate 0x10000 0
      ram <- MV.replicate 0x800 0
      let h = romHeader rom
          chrBytes = if hdChrRom h == 0 then max 0x2000 (hdChrRam h) else hdChrRom h
      chr <- if hdChrRom h == 0 then MV.replicate chrBytes 0 else U.thaw (U.fromList (BS.unpack (romChr rom)))
      ciram <- MV.replicate 0x800 0
      exram <- MV.replicate 0x400 0
      oam <- MV.replicate 256 0
      fr <- MV.replicate (256 * 240) 0
      cyc <- newSTRef 0
      ser <- newSTRef []
      frs <- newSTRef 0
      wt <- newSTRef (0, 0)
      rng <- newSTRef 0x2545F4914F6CDD1D
      ovl <- MV.replicate 0x800 0
      pure (Right (CoreRun st inp (U.fromList (BS.unpack (romPrg rom))) prgRam ram chr ciram exram oam fr cyc ser frs wt rng ovl))

coreSetPads :: CoreRun s -> Word8 -> Word8 -> ST s ()
coreSetPads cr a b = modifySTRef' (crIn cr) (\i -> i { kiPad1 = fromIntegral a, kiPad2 = fromIntegral b })

coreCycles :: CoreRun s -> ST s Int
coreCycles = readSTRef . crCycles

-- | One clock. Returns the CPU cycle event, if this clock ended one.
clock :: CoreRun s -> ST s (Maybe CycleEvent)
clock cr = do
  s <- readSTRef (crState cr)
  inp <- readSTRef (crIn cr)
  let (s', out) = coreStep s inp
  writeSTRef (crState cr) $! s'
  (waitLeft, waitByte) <- readSTRef (crWait cr)
  (rd, rdOk) <- case koMem out of
    Nothing
      | waitLeft == 1 -> writeSTRef (crWait cr) (0, 0) >> pure (fromIntegral waitByte, True)
      | waitLeft > 1 -> writeSTRef (crWait cr) (waitLeft - 1, waitByte) >> pure (kiRd inp, False)
      | otherwise -> pure (kiRd inp, kiRdOk inp)
    Just (MemReq sp a w) -> fmap (\x -> (x, True)) $ do
      let i = fromIntegral a
      case (sp, w) of
        (SpPrg, _) -> pure (fromIntegral (crPrg cr U.! (i `mod` U.length (crPrg cr))))
        (SpPrgRam, Just d) -> MV.write (crPrgRam cr) i (fromIntegral d) >> pure (kiRd inp)
        (SpPrgRam, Nothing) -> fromIntegral <$> MV.read (crPrgRam cr) i
        (SpRam, Just d) -> MV.write (crRam cr) i (fromIntegral d) >> pure (kiRd inp)
        (SpRam, Nothing) -> fromIntegral <$> MV.read (crRam cr) i
        (SpExram, Just d) -> MV.write (crExram cr) i (fromIntegral d) >> pure (kiRd inp)
        (SpExram, Nothing) -> fromIntegral <$> MV.read (crExram cr) i
        (SpOvl, _) -> fromIntegral <$> MV.read (crOvl cr) i
  va <- case koVramA out of
    Nothing -> pure (kiVramA inp)
    Just v -> fromIntegral <$> vram v
  vb <- case koVramB out of
    Nothing -> pure (kiVramB inp)
    Just (v, Just d) -> vramWrite v (fromIntegral d) >> pure (kiVramB inp)
    Just (v, Nothing) -> fromIntegral <$> vram v
  forM_ (koOamWrite out) $ \(a, d) -> MV.write (crOam cr) (fromIntegral a) (fromIntegral d)
  forM_ (koOamRow out) $ \(row, bs) -> forM_ (zip [0 ..] (C.toList bs)) $ \(j, d) ->
    MV.write (crOam cr) (fromIntegral row * 8 + j) (fromIntegral d)
  o <- MV.read (crOam cr) (fromIntegral (koOamAddr out))
  ex <- maybe (pure (kiExLatch inp)) (\x -> fromIntegral <$> MV.read (crExram cr) (fromIntegral x)) (koExLatch out)
  forM_ (koPixel out) $ \(x, y, c) -> MV.write (crFrame cr) (fromIntegral y * 256 + fromIntegral x) (fromIntegral c)
  when (koFrame out) (modifySTRef' (crFrames cr) (+ 1))
  -- a PRG read that waits ('crWait'): kiRdOk low, a poison byte meanwhile
  (rd', rdOk') <- case koMem out of
    Just (MemReq SpPrg _ Nothing) -> do
      x <- readSTRef (crRng cr)
      let x' = x * 6364136223846793005 + 1442695040888963407
          k = fromIntegral (x' `C.shiftR` 33) :: Int
      writeSTRef (crRng cr) x'
      if k `mod` 8 == 0
        then writeSTRef (crWait cr) (1 + (k `div` 8) `mod` 31, fromIntegral rd) >> pure (0xEE, False)
        else pure (rd, rdOk)
    _ -> pure (rd, rdOk)
  writeSTRef (crIn cr) inp { kiRd = rd', kiRdOk = rdOk', kiVramA = va, kiVramB = vb, kiOam = fromIntegral o, kiExLatch = ex }
  forM_ (koEvent out) $ \e -> do
    modifySTRef' (crCycles cr) (+ 1)
    when (ceWrite e && ceAddr e == 0x4016) (modifySTRef' (crSerial cr) (C.testBit (ceData e) 0 :))
  pure (koEvent out)
 where
  vram (VChr a) = MV.read (crChr cr) (fromIntegral a)
  vram (VNt a) = MV.read (crCiram cr) (fromIntegral a)
  vram (VExram a) = MV.read (crExram cr) (fromIntegral a)
  vram (VConst c) = pure (fromIntegral c)
  vramWrite (VChr a) d = MV.write (crChr cr) (fromIntegral a) d
  vramWrite (VNt a) d = MV.write (crCiram cr) (fromIntegral a) d
  vramWrite (VExram a) d = MV.write (crExram cr) (fromIntegral a) d
  vramWrite (VConst _) _ = pure ()

-- | One step of the CPU (an instruction, interrupt sequence, jam cycle or
-- the reset sequence): its kind, the bus accesses, the registers after.
data StepRec = StepRec
  { srKind :: String
  , srBus  :: [(Word16, Word8, Bool)]
  , srRegs :: Regs
  , srPpu  :: (Int, Int) -- ^ PPU line and dot after the step
  , srApu  :: [[Word8]]  -- ^ the APU's outputs after each cycle (pulse 1, 2, triangle, noise, DMC)
  , srMmc3 :: Int        -- ^ the MMC3's scanline counter after the step (step 3.10)
  } deriving stock (Show, Eq)

coreRunStep :: CoreRun s -> ST s StepRec
coreRunStep cr = go []
 where
  go acc = go2 acc []
  go2 acc aps = do
    me <- clock cr
    case me of
      Nothing -> go2 acc aps
      Just e ->
        let acc' = (fromIntegral (ceAddr e), fromIntegral (ceData e), ceWrite e) : acc
            aps' = coreChannels (ceApu e) : aps
        in case ceStep e of
             Nothing -> go2 acc' aps'
             Just k -> do
               let r = ceRegs e
               ks <- readSTRef (crState cr)
               let p = ksPpu ks
                   -- the MMC3's counter, or MMC5's line counter (as the spec's mapperCounter)
                   mmc3 = fromIntegral (if ccMapper (ksCfg ks) == MMmc5 then m5Line (msM5 (ksMap ks)) else msIrqCount (ksMap ks))
               pure (StepRec (kindName k) (reverse acc')
                       (Regs (w (crA r)) (w (crX r)) (w (crY r)) (w (crS r)) (w (crP r)) (fromIntegral (crPc r)))
                       (fromIntegral (pLine p), fromIntegral (pDot p)) (reverse aps') mmc3)
  w :: C.Unsigned 8 -> Word8
  w = fromIntegral
  kindName k = case k of
    SkInstr -> "instruction"
    SkNmi -> "NMI"
    SkIrq -> "IRQ"
    SkJam -> "jam"
    SkReset -> "reset"

-- | Run until at least @n@ CPU cycles have passed.
coreRunFor :: CoreRun s -> Int -> ST s ()
coreRunFor cr n = do
  start <- coreCycles cr
  let go = do
        _ <- clock cr
        c <- coreCycles cr
        when (c - start < n) go
  go

-- | CPU address space without side effects: RAM, PRG RAM and ROM.
corePeek :: CoreRun s -> Word16 -> ST s Word8
corePeek cr a
  | a < 0x2000 = MV.read (crRam cr) (fromIntegral a `mod` 0x800)
  | a >= 0x6000 && a < 0x8000 = MV.read (crPrgRam cr) (fromIntegral a - 0x6000)
  | otherwise = pure 0

-- | The core alone until @n@ frames are drawn, with these buttons held on
-- controller 1: the last frame (as "Nes.Spec".@runFramesWith@).
coreFramesWith :: Word8 -> Int -> BS.ByteString -> Either String (U.Vector Word16)
coreFramesWith = coreFramesPress 0

-- | The same with the buttons pressed from frame @from@ on (as
-- "Nes.Spec".@runFramesPress@).
coreFramesPress :: Int -> Word8 -> Int -> BS.ByteString -> Either String (U.Vector Word16)
coreFramesPress from pad n file = runST $ do
  ec <- newCoreRun file
  case ec of
    Left e -> pure (Left e)
    Right cr -> do
      let go = do
            f <- readSTRef (crFrames cr)
            coreSetPads cr (if f >= from then pad else 0) 0
            _ <- clock cr
            f' <- readSTRef (crFrames cr)
            when (f' < n) go
      go
      Right <$> coreFrame cr

coreFrame :: CoreRun s -> ST s (U.Vector Word16)
coreFrame cr = U.freeze (crFrame cr)

-- | Run the spec and the core side by side for @steps@ CPU steps; the
-- first step where they differ, if any.
lockstepCore :: Int -> BS.ByteString -> Either String (Maybe (Int, StepRec, StepRec))
lockstepCore steps file = runST $ do
  em <- S.newMachine file
  ec <- newCoreRun file
  case (em, ec) of
    (Left e, _) -> pure (Left e)
    (_, Left e) -> pure (Left e)
    (Right m, Right cr) -> do
      _ <- coreRunStep cr -- the reset sequence (the spec ran it in newMachine)
      let go i
            | i >= steps = pure (Right Nothing)
            | otherwise = do
                (st, accs) <- S.withTrace m (S.stepCpu m)
                r <- getRegs (S.mCpu m)
                pos <- ppuPosition (S.mPpu m)
                aps <- S.apuTrace m
                mc <- mapperCounter (S.mCart m)
                let spec = StepRec (kind st) [ (a, d, w) | S.Access a d w <- accs ] r pos (map specChannels aps) mc
                core <- coreRunStep cr
                if spec /= core then pure (Right (Just (i, spec, core))) else go (i + 1)
      go (0 :: Int)
 where
  kind st = case st of
    Instr _ -> "instruction"
    Nmi -> "NMI"
    Irq -> "IRQ"
    Jammed -> "jam"

-- | Run a Blargg ROM on the core alone, until its protocol (or the older
-- shell's serial line) reports a result; at most @limit@ CPU cycles.
coreBlargg :: Int -> BS.ByteString -> Either String (Blargg, Int)
coreBlargg limit file = runST $ do
  ec <- newCoreRun file
  case ec of
    Left e -> pure (Left e)
    Right cr -> do
      let go = do
            coreRunFor cr 4096
            hd <- mapM (corePeek cr) [0x6000 .. 0x6003]
            n <- coreCycles cr
            let final = drop 1 hd == blarggSignature && take 1 hd `notElem` [[0x80], [0x81]]
            res <- if final
              then do
                bs <- mapM (corePeek cr) [0x6000 .. 0x7FFF]
                let v = BS.pack bs
                pure (blargg (\a -> BS.index v (fromIntegral a - 0x6000)))
              else if drop 1 hd == blarggSignature then pure Running else do
                bits <- reverse <$> readSTRef (crSerial cr)
                pure (serialVerdict (serialText bits))
            case res of
              Done _ _ -> pure (Right (res, n))
              _ | n >= limit -> pure (Right (res, n))
                | otherwise -> go
      go

-- | The 2005 sprite tests on the core: run until the first nametable says
-- PASSED or FAILED; returns $F8 (1 = passed).
coreOld :: Int -> BS.ByteString -> Either String Word8
coreOld limit file = runST $ do
  ec <- newCoreRun file
  case ec of
    Left e -> pure (Left e)
    Right cr -> do
      let go = do
            coreRunFor cr 30000
            t <- nametableText cr
            n <- coreCycles cr
            if any (\l -> "PASSED" `isInfixOf` l || "FAILED" `isInfixOf` l) t || n >= limit
              then Right <$> corePeek cr 0xF8
              else go
      go
 where
  nametableText cr = mapM (\r -> mapM (\c -> do
      b <- MV.read (crCiram cr) (r * 32 + c)
      pure (if b >= 0x20 && b < 0x7F then toEnum (fromIntegral b) else ' ')) [0 .. 31]) [0 .. 29 :: Int]

-- | AccuracyCoin's "run all" on the core, as 'Nes.Spec.runAccuracyCoin':
-- Start after 60 frames, wait until $35 has been set and then stayed
-- clear for 3 s; the result bytes of every entry, and the CPU cycles.
coreAccuracyCoin :: Int -> BS.ByteString -> Either String ([(String, [(Entry, Word8)])], Int)
coreAccuracyCoin limit file = do
  rom <- parseRom file
  pages <- parsePages (romPrg rom)
  runST $ do
    ec <- newCoreRun file
    case ec of
      Left e -> pure (Left e)
      Right cr -> do
        let frames k = do
              start <- readSTRef (crFrames cr)
              let go = do
                    _ <- clock cr
                    f <- readSTRef (crFrames cr)
                    when (f - start < k) go
              go
            quiet = 3 * 1789773
            wait started clearSince = do
              coreRunFor cr 10000
              r <- corePeek cr runningAllTests
              n <- coreCycles cr
              if | n >= limit -> pure False
                 | r /= 0 -> wait True n
                 | started && n - clearSince >= quiet -> pure True
                 | otherwise -> wait started clearSince
        frames 60
        coreSetPads cr 0x08 0
        frames 5
        coreSetPads cr 0 0
        ok <- wait False 0
        if not ok then pure (Left "AccuracyCoin: run-all didn't finish on the core") else do
          frames 30
          n <- coreCycles cr
          res <- mapM (\pg -> (,) (pgName pg) <$> mapM (\e -> (,) e <$> corePeek cr (enResult e)) (pgEntries pg)) pages
          pure (Right (res, n))

coreChannels :: Channels -> [Word8]
coreChannels (Channels p1 p2 t n d) = [fromIntegral p1, fromIntegral p2, fromIntegral t, fromIntegral n, fromIntegral d]

specChannels :: SA.Channels -> [Word8]
specChannels (SA.Channels p1 p2 t n d) = [p1, p2, t, n, d]
