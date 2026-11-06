{-# LANGUAGE MultiWayIf #-}
-- | Lockstep of the whole Clash core ("Gb.Core") with the reference
-- model: 'coreStep' runs clock by clock with its memories in vectors (one
-- clock of read latency, like block RAM), next to a spec machine; after
-- every spec step the two must agree on the bus accesses of that step
-- (M-cycle by M-cycle), the CPU registers, the timer, IF, IE, the PPU's
-- position and OAM DMA, and every pixel written.
module Gb.FullHarness
  ( cartCfg
  , fullLockstep
  , fullLockstepOn
  , fullLockstepPal
  , CoreSim
  , newCoreSim
  , newCoreSimModel
  , runCoreFrames
  , harnessEvents
    -- * Clock by clock (side track A: the menu's pause)
  , clockDot
  , coreState
  , coreFb
  , coreSerial
  ) where

import Prelude

import Control.Applicative ((<|>))
import Control.Monad (forM_, when)
import Control.Monad.ST (ST, runST)
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import qualified Data.ByteString as BS
import Data.STRef
import qualified Data.Vector.Unboxed.Mutable as MU
import Data.Word

import Clash.Prelude (BitVector)
import Gb.Cartridge
import Gb.Core
import Gb.Cpu (BusOp (..), CpuRegs (..))
import Gb.Spec (Model (..), Regs (..), newMachineModel, newMachineModelPal, regs, setJoypad, step, withTrace)
import Gb.Spec.Machine
import Gb.Spec.Apu (channelOutputs)
import qualified Clash.Prelude as C
import qualified Clash.Sized.Vector as CV

-- | The core's cartridge configuration from a ROM image's header.
cartCfg :: BS.ByteString -> Either String CartCfg
cartCfg rom = do
  h <- parseHeader rom
  ct <- maybe (Left "cartridge type") Right (cartType (hdCartType h))
  mbc <- case ctMbc ct of
    NoMbc -> Right MbcNone
    Mbc1 -> Right MbcOne
    Mbc2 -> Right MbcTwo
    Mbc3 -> Right MbcThree
    Mbc5 -> Right MbcFive
    other -> Left (show other <> " not supported")
  let banks = BS.length rom `div` 0x4000
      ram | mbc == MbcTwo = 512
          | ctRam ct = maybe 0 id (ramSizeBytes (hdRamSize h))
          | otherwise = 0
  when (BS.length rom > 0x800000) (Left "ROM larger than 8 MiB")
  pure CartCfg { ccMbc = mbc, ccRomMask = fromIntegral (banks - 1)
               , ccRamMask = fromIntegral (max 1 ram - 1)
               , ccHasRam = ram > 0, ccHc = hdHeaderSum h /= 0
               , ccTimer = hasTimer (hdCartType h), ccCgbHw = False, ccCgb = False }

data CoreSim s = CoreSim
  { csState :: STRef s CoreState
  , csIn    :: STRef s CoreIn
  , csRom, csCram, csWram, csHram, csVram, csFb :: MU.MVector s Word8
  , csSerial :: STRef s [Word8] -- ^ newest first
  , csModel :: CoreModel
  , csPixC  :: STRef s [(Int, Word16)] -- ^ the Game Boy Color's pixels since the harness last looked, newest first
  , csFrames :: STRef s Int
  }

newCoreSim :: BS.ByteString -> ST s (Either String (CoreSim s))
newCoreSim = newCoreSimModel CoreDmg

-- | The core of a model (side track I, phase 3) with its memories in
-- vectors: WRAM 32 KiB and VRAM 16 KiB, as 'gbCoreWith' has them.
newCoreSimModel :: CoreModel -> BS.ByteString -> ST s (Either String (CoreSim s))
newCoreSimModel model rom = case cartCfg rom of
  Left e -> pure (Left e)
  Right cfg -> do
    r <- MU.replicate (max 0x8000 (BS.length rom)) 0
    forM_ [0 .. BS.length rom - 1] $ \i -> MU.write r i (BS.index rom i)
    c <- MU.replicate 0x20000 0
    w <- MU.replicate 0x8000 0
    h <- MU.replicate 0x80 0
    v <- MU.replicate 0x4000 0
    fb <- MU.replicate (160 * 144) 0
    st <- newSTRef coreInit
    _ <- pure cfg
    inp <- newSTRef (CoreIn True 0 0 0 (C.repeat 0) 0 gbcDefaultPal)
    ser <- newSTRef []
    fr <- newSTRef 0
    pixc <- newSTRef []
    pure (Right (CoreSim st inp r c w h v fb ser model pixc fr))

memOf :: CoreSim s -> Space -> MU.MVector s Word8
memOf cs sp = case sp of
  SpRom -> csRom cs
  SpRomHint -> csRom cs -- a hint (Gb.Core): nothing uses the byte
  SpRomUse -> csRom cs  -- the hinted read itself
  SpCram -> csCram cs
  SpWram -> csWram cs
  SpHram -> csHram cs
  SpVram -> csVram cs

-- | One clock; the M-cycle event, if one ended, and a pixel.
clock :: CoreSim s -> Word8 -> ST s (Maybe McEvent, Maybe (Int, Word8))
clock cs = clockDot cs True

-- | One clock with the dot enable given (Gb.Top withholds it while the
-- PS menu pauses the core).
clockDot :: CoreSim s -> Bool -> Word8 -> ST s (Maybe McEvent, Maybe (Int, Word8))
clockDot cs dot keys = do
  s <- readSTRef (csState cs)
  inp0 <- readSTRef (csIn cs)
  let inp = inp0 { kiKeys = fromIntegral keys, kiDot = dot }
      (s', o) = coreStep (csModel cs) s inp
  writeSTRef (csState cs) s'
  rd <- case koMem o of
    Just q -> do
      let mem = memOf cs (mqSpace q)
          a = fromIntegral (mqAddr q) `mod` MU.length mem
      case mqWrite q of
        Just v -> MU.write mem a (fromIntegral v) >> pure 0
        Nothing -> MU.read mem a
    Nothing -> pure 0
  -- the pair in bank 0 and, at the same address, in bank 1 (0x2000 up)
  let pairAt base p = do
        lo <- MU.read (csVram cs) (base + 2 * fromIntegral p)
        hi <- MU.read (csVram cs) (base + 2 * fromIntegral p + 1)
        pure (fromIntegral hi `shiftL` 8 .|. fromIntegral lo)
  (vp, vp1) <- case koVramPair o of
    Just p -> (,) <$> pairAt 0 p <*> pairAt 0x2000 p
    Nothing -> pure (0, 0)
  writeSTRef (csIn cs) inp { kiRd = fromIntegral rd, kiVram = vp, kiVram1 = vp1 }
  mapM_ (\(i, c) -> modifySTRef' (csPixC cs) ((fromEnum i, fromIntegral c) :)) (koPixelC o)
  pix <- case koPixel o of
    Just (i, sh) -> do
      MU.write (csFb cs) (fromEnum i) (fromIntegral sh)
      pure (Just (fromEnum i, fromIntegral sh))
    Nothing -> pure Nothing
  mapM_ (\b -> modifySTRef' (csSerial cs) (fromIntegral b :)) (koSerial o)
  when (koFrame o) (modifySTRef' (csFrames cs) (+ 1))
  pure (koEvent o, pix)

coreState :: CoreSim s -> ST s CoreState
coreState = readSTRef . csState

coreFb :: CoreSim s -> ST s [Word8]
coreFb cs = mapM (MU.read (csFb cs)) [0 .. MU.length (csFb cs) - 1]

-- | The bytes sent on the link port so far, oldest first.
coreSerial :: CoreSim s -> ST s [Word8]
coreSerial cs = reverse <$> readSTRef (csSerial cs)

-- | Clock until the core ends a spec step.
coreSpecStep :: CoreSim s -> Word8 -> ST s ([McEvent], [(Int, Word8)])
coreSpecStep cs keys = go [] []
 where
  go evs pxs = do
    (e, p) <- clock cs keys
    let pxs' = maybe pxs (: pxs) p
    case e of
      Just ev | meStepDone ev -> stalls (ev : evs) pxs'
              | otherwise -> go (ev : evs) pxs'
      Nothing -> go evs pxs'
  -- the spec's step includes the M-cycles after it without the CPU (the
  -- speed switch's pause, VRAM DMA): clock the core through them too
  stalls evs pxs = do
    st <- readSTRef (csState cs)
    -- (until an M-cycle's start with none due)
    if ksSeq st /= QIdle || ksPause st > 0 || ksHdmaRun st
      then do
        (_, p) <- clock cs keys
        stalls evs (maybe pxs (: pxs) p)
      else pure (reverse evs, reverse pxs)

toAccess :: McEvent -> Access
toAccess e = case meOp e of
  OpRead -> ARead (fromIntegral (meAddr e)) (fromIntegral (meData e))
  OpWrite -> AWrite (fromIntegral (meAddr e)) (fromIntegral (meData e))
  OpIdle -> AIdle

toRegs :: CpuRegs -> Regs
toRegs r = Regs (b (crA r)) (b (crF r)) (b (crB r)) (b (crC r)) (b (crD r)) (b (crE r))
                (b (crH r)) (b (crL r)) (fromIntegral (crSp r)) (fromIntegral (crPc r)) (crIme r)
 where
  b = fromIntegral :: BitVector 8 -> Word8

-- | Run spec and core for @n@ spec steps (or until the spec's LD B,B),
-- with these keys held from step @k@ on ('keysAt'); the first difference.
fullLockstep :: Int -> (Int -> Word8) -> BS.ByteString -> Maybe String
fullLockstep = fullLockstepOn Dmg

-- | 'fullLockstep' on a model (side track I, phase 3: the spec's and the
-- core's Game Boy Color).
fullLockstepOn :: Model -> Int -> (Int -> Word8) -> BS.ByteString -> Maybe String
fullLockstepOn model = fullLockstepPal model Nothing

-- | 'fullLockstepOn' with the Game Boy Color's palettes for a DMG
-- cartridge (BG 0, OBJ 0, OBJ 1: four 15-bit colours each) given to both.
fullLockstepPal :: Model -> Maybe [[Int]] -> Int -> (Int -> Word8) -> BS.ByteString -> Maybe String
fullLockstepPal model mpal n keysAt rom = runST $ do
  ma <- either error id <$> maybe (newMachineModel model rom) (\p -> newMachineModelPal model p rom) mpal
  cs <- either error id <$> newCoreSimModel (if model == Cgb then CoreCgb else CoreDmg) rom
  case mpal of
    Just p -> modifySTRef' (csIn cs) (\i -> i { kiPalInit = CV.unsafeFromList
                [ fromIntegral (c `shiftR` s') | c <- concat p, s' <- [0, 8] ] })
    Nothing -> pure ()
  let go k
        | k >= n = pure Nothing
        | otherwise = do
            let keys = keysAt k
            setJoypad ma keys
            r0 <- regs ma
            (_, ta) <- withTrace ma (step ma)
            ra <- regs ma
            (evs, pxs) <- coreSpecStep cs keys
            let tb = map toAccess evs
                rb = toRegs (meRegs (last evs))
            sd <- specDbg ma
            sa <- channelOutputs ma
            cs' <- readSTRef (csState cs)
            let cd = dbg' cs'
                ca = [ (fromIntegral l, d) | (l, d) <- C.toList (dbApu (coreDbg cs')) ]
            bad0 <- firstBadPixel ma pxs
            pxc <- readSTRef (csPixC cs) <* writeSTRef (csPixC cs) []
            badC <- firstBadColour ma (reverse pxc)
            let bad = bad0 <|> badC
            brk <- getSt ma iBREAK
            if | ta /= tb || ra /= rb ->
                   pure (Just (unlines [ "step " <> show k <> " from " <> show r0
                                       , " spec bus " <> show ta, " core bus " <> show tb
                                       , " spec " <> show ra, " core " <> show rb ]))
               | sd /= cd -> pure (Just ("step " <> show k <> " at PC " <> show (rgPC r0)
                                         <> ": state spec " <> show sd <> ", core " <> show cd))
               | sa /= ca -> pure (Just ("step " <> show k <> " at PC " <> show (rgPC r0)
                                         <> ": sound channels spec " <> show sa <> ", core " <> show ca))
               | Just e <- bad -> pure (Just ("step " <> show k <> ": " <> e))
               | brk /= 0 -> pure Nothing
               | otherwise -> go (k + 1)
  go 0
 where
  dbg' s = let d = coreDbg s
           in ( fromIntegral (dbDiv d) :: Int, fromIntegral (dbTima d) :: Int, fromIntegral (dbIf d) :: Int
              , fromIntegral (dbIe d) :: Int, fromIntegral (dbLy d) :: Int, fromIntegral (dbDot d) :: Int
              , fromIntegral (dbMode d) :: Int, maybe (-1) fromIntegral (dbDma d) :: Int )
  specDbg m = do
    let g = getSt m
    (,,,,,,,) <$> g iDIV <*> g iTIMA <*> ((.&. 0x1F) <$> g iIF) <*> g iIE <*> g iLY <*> g iDOT
              <*> g iPPUMODE <*> g iDMAIDX
  -- the Game Boy Color: each colour pixel the core made equals the spec's
  firstBadColour m pxs = case pxs of
    [] -> pure Nothing
    (i, c) : rest -> do
      sc <- MU.read (mFbC m) i
      if sc /= c then pure (Just ("colour pixel " <> show (i `mod` 160, i `div` 160) <> ": spec " <> show sc <> ", core " <> show c))
                 else firstBadColour m rest
  firstBadPixel m pxs = go' pxs
   where
    go' [] = pure Nothing
    go' ((i, v) : rest) = do
      sv <- MU.read (mFb m) i
      if sv /= v then pure (Just ("pixel " <> show (i `mod` 160, i `div` 160) <> ": spec " <> show sv <> ", core " <> show v))
                 else go' rest

-- | Run the core alone for @f@ frames; its frame buffer and serial text.
runCoreFrames :: Int -> BS.ByteString -> Either String ([Word8], String)
runCoreFrames f rom = runST $ do
  ecs <- newCoreSim rom
  case ecs of
    Left e -> pure (Left e)
    Right cs -> do
      let go = do
            fr <- readSTRef (csFrames cs)
            if fr >= f then pure () else clock cs 0 >> go
      go
      fb <- mapM (MU.read (csFb cs)) [0 .. 160 * 144 - 1]
      ser <- reverse <$> readSTRef (csSerial cs)
      pure (Right (fb, map (toEnum . fromIntegral) ser))

-- | The harness's M-cycle events for its first @n@ clocks.
harnessEvents :: Int -> BS.ByteString -> [McEvent]
harnessEvents n rom = runST $ do
  cs <- either error id <$> newCoreSim rom
  let go k acc
        | k >= n = pure (reverse acc)
        | otherwise = do
            (e, _) <- clock cs 0
            go (k + 1) (maybe acc (: acc) e)
  go (0 :: Int) []
