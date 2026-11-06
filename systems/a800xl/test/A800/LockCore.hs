{-|
Lockstep of the whole Clash 800XL ("A800.Core", as plain Haskell, its
memories in vectors) with the reference model (M4 step 4.6): after every
step of the CPU, the machine cycles of the step must agree one by one:
the bus access (address, data, write, by the CPU or ANTIC), POKEY's audio
level, the beam and GTIA's four half-clock colours; and the step's kind
and the registers after it.
-}
module A800.LockCore
  ( CStepFn
  , coreCfgOf
  , lockstepCore
  ) where

import Prelude

import Control.Monad (when)
import Control.Monad.ST (runST)
import qualified Data.ByteString as BS
import Data.Bits (bit, shiftR, (.&.))
import Data.STRef
import qualified Data.Vector.Unboxed.Mutable as MV
import Data.Word

import qualified Clash.Prelude as C

import A800.Core
import A800.Media (Cart (..), CartType (..), casImage)
import A800.CasUnit (CasAns (..), CasState (..), casEvent, casInit, casProg, casTape)
import A800.Spec.Gtia (gtiaPixels)
import A800.Spec.Machine
import A800.Spec.Pokey (pokeyLevel)
import Mos6502.Core (CpuRegs (..), StepKind (..))
import Mos6502.Spec (Regs (..), Step (..), getRegs, step)

type CStepFn = CoreState -> CoreIn -> (CoreState, CoreOut)

-- | The core's view of what is plugged in.
coreCfgOf :: Config -> CoreCfg
coreCfgOf cfg = CoreCfg
  { ccBasic = maybe False (const True) (cfgBasic cfg)
  , ccCart = case cfgCart cfg of
      Nothing | Just _ <- cfgCas cfg -> CkCas
              | otherwise -> CkNone
      Just (Cart Std8k _) -> Ck8
      Just (Cart Std16k _) -> Ck16
      Just (Cart Xegs32k _) -> CkXegs 0
      Just (Cart Xegs64k _) -> CkXegs 1
      Just (Cart Xegs128k _) -> CkXegs 2
  , ccXex = maybe False (const True) (cfgXex cfg)
  , ccXexLen = maybe 0 (fromIntegral . BS.length . xbFile) (cfgXex cfg) }

-- | One cycle of the spec, as the core reports it.
data SpecCycle = SpecCycle
  { scAccess :: (Word16, Word8, Bool, Bool)
  , scLevel  :: Int
  , scBeam   :: (Int, Int)
  , scColours :: ((Word8, Word8), (Word8, Word8))
  } deriving stock (Show, Eq)

-- | Run the machine for @cycles@ with the Clash core alongside (its step
-- function given, for deliberate bugs), the keys held as scheduled
-- (from a cycle on: keys, modifiers); 'Left' with the first difference.
lockstepCore :: CStepFn -> Int -> [(Int, C.BitVector 64, C.BitVector 3)] -> Config -> Either String Int
lockstepCore stepFn cycles keys cfg = runST $ do
  m <- newMachine cfg
  ram <- MV.replicate 65536 (0 :: Word8)
  let ccfg = coreCfgOf cfg
      rom sp a = case sp of
        SpOs -> idx (Just (cfgOs cfg)) a
        SpBasic -> idx (cfgBasic cfg) a
        SpCart -> idx (maybe (casImage <$> cfgCas cfg) (Just . cartImage) (cfgCart cfg)) a
        SpLoader -> idx (xbLoader <$> cfgXex cfg) a
        SpXex -> idx (xbFile <$> cfgXex cfg) a
        SpRam -> 0
        SpCas -> 0
      idx mb a = case mb of
        Just b | fromIntegral a < BS.length b -> BS.index b (fromIntegral a)
        _ -> 0xFF
  st <- newSTRef (coreInit ccfg)
  rdRef <- newSTRef (0 :: Word8)
  inRef <- newSTRef (CoreIn True 0 True 0xFF 0 0 0 0 Nothing False)
  -- the tape's answer beside the core, as A800.CasUnit's casUnit does it
  casRef <- newSTRef casInit
  -- the cartridge as a slow memory (DDR on the Arty, side track Q): about
  -- one read in eight waits 1 to 31 more clocks ('kiRdOk' False, a poison
  -- byte meanwhile), from a seeded LCG, so the core is checked for any
  -- latency
  waitRef <- newSTRef (0 :: Int)
  rng <- newSTRef (12345 :: Word32)
  forceN <- newSTRef (0 :: Int)
  -- one machine cycle of the core
  let cycleCore = do
        let go = do
              s <- readSTRef st
              rdv <- readSTRef rdRef
              base <- readSTRef inRef
              w <- readSTRef waitRef
              armed <- csArm <$> readSTRef casRef
              let (s', o) = stepFn s ((if w > 0 then base { kiRd = 0xEE, kiRdOk = False } else base { kiRd = fromIntegral rdv, kiRdOk = True }) { kiCasOn = armed })
              writeSTRef st s'
              when (w > 0) (writeSTRef waitRef (w - 1))
              -- the tape's answer: the program's byte, or the tape image's
              -- (then as slow as the cartridge's)
              tapeRead <- case koMem o of
                Just (MemReq SpCas a Nothing) -> do
                  c <- readSTRef casRef
                  case casProg c (fromIntegral a) of
                    (c', CaNow v) -> writeSTRef casRef c' >> pure (Just (fromIntegral v, False))
                    (c', CaTape off) -> do
                      let v = rom SpCart off
                      writeSTRef casRef (casTape (fromIntegral v) c')
                      pure (Just (v, True))
                _ -> pure Nothing
              case koEvent o of
                Just e | ceCpu e && not (ceWrite e) -> modifySTRef' casRef (casEvent (ceAddr e) (ceData e))
                _ -> pure ()
              let slow = case koMem o of
                    Just (MemReq SpCart _ Nothing) -> True
                    _ -> maybe False snd tapeRead
              case () of
                _ | slow -> do
                  x <- (\v -> v * 1103515245 + 12345) <$> readSTRef rng
                  writeSTRef rng x
                  writeSTRef waitRef (if (x `shiftR` 16) .&. 7 == 0 then 1 + fromIntegral ((x `shiftR` 19) .&. 31) else 0)
                _ -> pure ()
              case koMem o of
                Just (MemReq SpRam a (Just v)) -> MV.write ram (fromIntegral a) (fromIntegral v)
                Just (MemReq SpRam a Nothing) -> MV.read ram (fromIntegral a) >>= writeSTRef rdRef
                Just (MemReq SpCas _ _) -> mapM_ (writeSTRef rdRef . fst) tapeRead
                Just (MemReq sp a _) -> writeSTRef rdRef (rom sp a)
                Nothing -> pure ()
              case koEvent o of
                -- force the whole state every 256 machine cycles: lazy
                -- Vec elements would otherwise keep old states alive
                Just e -> do
                  k <- readSTRef forceN
                  writeSTRef forceN (k + 1)
                  if k `mod` 256 == 0 then C.rnfX s' `seq` pure e else pure e
                Nothing -> go
        go
  -- the core's reset sequence (the spec ran its own in newMachine)
  let warm = do
        e <- cycleCore
        when (ceStep e /= Just SkReset) warm
  warm
  specCycles <- newSTRef []
  pendLevel <- newSTRef 0
  let hook e = case e of
        EvPokeyTick -> pokeyLevel (mPokey m) >>= writeSTRef pendLevel
        EvGtiaCycle y x _ _ -> do
          a <- gtiaPixels (mGtia m) y (2 * x)
          b <- gtiaPixels (mGtia m) y (2 * x + 1)
          l <- readSTRef pendLevel
          modifySTRef' specCycles (SpecCycle (0, 0, False, False) l (y, x) (a, b) :)
        _ -> pure ()
  setHook m hook
  let run n ks = do
        t <- machineCycles m
        -- a tape boots as A800.Spec.runCasBoot: START held for the first
        -- 60 frames, Space pressed at frames 120 to 125
        case cfgCas cfg of
          Just _ | Nothing <- cfgXex cfg -> do
            let f = t `div` 29868
                con = if f < 60 then 1 else 0 :: Int
                key = if f >= 120 && f < 126 then bit 0x21 else 0 :: C.BitVector 64
            setConsole m (fromIntegral con)
            setKeys m (fromIntegral key) 0
            modifySTRef' inRef (\i -> i { kiConsole = fromIntegral con, kiKeys = key, kiMods = 0 })
          _ -> pure ()
        -- keys held from their cycle on
        ks' <- case ks of
          (c, k, md) : rest | t >= c -> do
            setKeys m (fromIntegral k) (fromIntegral md)
            modifySTRef' inRef (\i -> i { kiKeys = k, kiMods = md })
            pure rest
          _ -> pure ks
        writeSTRef specCycles []
        (stp, acc) <- withTrace m (step (mCpu m))
        sc <- reverse <$> readSTRef specCycles
        regs <- getRegs (mCpu m)
        if length sc /= length acc then pure (Left ("cycle " <> show t <> ": spec bookkeeping")) else do
          let spec = zipWith (\c (Access a v w b) -> c { scAccess = (a, v, w, b) }) sc acc
          r <- compareStep t stp regs spec
          case r of
            Just err -> pure (Left err)
            Nothing -> if t >= cycles then pure (Right n) else (run $! n + length spec) ks'
      compareStep t stp regs spec = do
        evs <- mapM (const cycleCore) spec
        let mine e = SpecCycle (fromIntegral (ceAddr e), fromIntegral (ceData e), ceWrite e, ceCpu e)
                               (fromIntegral (ceLevel e)) (fromIntegral (ceY e), fromIntegral (ceX e))
                               (bimapW (ceColours e))
            diffs = [ (i, g, w) | (i, g, w) <- zip3 [0 :: Int ..] (map mine evs) spec, g /= w ]
            lastEv = last evs
            kind = case stp of
              Instr _ -> Just SkInstr
              Nmi -> Just SkNmi
              Irq -> Just SkIrq
              Jammed -> Just SkJam
            CpuRegs a x y s p pc = ceRegs lastEv
            cregs = Regs (fromIntegral a) (fromIntegral x) (fromIntegral y) (fromIntegral s) (fromIntegral p) (fromIntegral pc)
            early = [ i | (i, e) <- zip [0 :: Int ..] (init evs), ceStep e /= Nothing ]
        pure $ case diffs of
          (i, g, w) : _ -> Just ("cycle " <> show (t - length spec + i + 1) <> " (step " <> show stp <> "): core " <> show g <> ", spec " <> show w)
          [] | ceStep lastEv /= kind -> Just ("cycle " <> show t <> ": core ended " <> show (ceStep lastEv) <> ", spec " <> show stp)
             | early /= [] -> Just ("cycle " <> show t <> ": core ended a step early (" <> show stp <> ")")
             | cregs /= regs -> Just ("cycle " <> show t <> " after " <> show stp <> ": core " <> show cregs <> ", spec " <> show regs)
             | otherwise -> Nothing
  run 0 keys
 where
  bimapW ((a, b), (c, d)) = ((fromIntegral a, fromIntegral b), (fromIntegral c, fromIntegral d))
