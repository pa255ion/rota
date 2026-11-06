{-# LANGUAGE MultiWayIf #-}
-- | Lockstep of the Clash PPU ("Gb.Ppu") with the reference model's PPU:
-- the spec machine runs a ROM with its own CPU and PPU; through its test
-- hooks ('Hook') the Clash PPU runs on the same VRAM, OAM and registers
-- right after the spec PPU's dots in every M-cycle, and after every write
-- to LCDC, STAT and LYC. After each, the two must agree on the PPU state
-- (line, dot, mode, LY = LYC flag, STAT line, mode 3 progress), on the
-- interrupt requests and on every pixel written.
module Gb.PpuHarness
  ( ppuLockstep
  ) where

import Prelude

import Control.Monad (forM, unless, when)
import Control.Monad.ST (ST, runST)
import Data.Bits (testBit)
import qualified Data.ByteString as BS
import Data.STRef
import qualified Data.Vector.Unboxed.Mutable as MU
import Data.Word

import Clash.Prelude (BitVector, Vec, pack, (++#))
import qualified Clash.Prelude as C
import qualified Clash.Sized.Vector as V
import Gb.Ppu
import Gb.Spec (step)
import Gb.Spec.Machine

-- | Run a ROM on the spec for @n@ M-cycles with the Clash PPU alongside;
-- the first disagreement, if any.
ppuLockstep :: Int -> BS.ByteString -> Maybe String
ppuLockstep n rom = runST $ do
  m <- either error id <$> newMachine rom
  st <- newSTRef (ppuInit, MIdle)
  err <- newSTRef Nothing
  pre <- newSTRef (0 :: Int, False)
  let fail' msg = do
        e <- readSTRef err
        case e of
          Nothing -> do
            ly <- getSt m iLY
            dot <- getSt m iDOT
            c <- getSt m iCYC
            writeSTRef err (Just (msg <> " (spec: M-cycle " <> show c <> ", line " <> show ly <> ", dot " <> show dot <> ")"))
          Just _ -> pure ()
      -- one command, clocked until done; returns the outputs
      run cmd = do
        regs <- ppuRegs m
        oam <- oamVec m
        dma <- (>= 0) <$> getSt m iDMAIDX
        (s0, mi0) <- readSTRef st
        let go s mi addr c acc = do
              v <- vramPair m addr
              let inp = PpuIn (if c then Just cmd else Nothing) regs oam dma v False 0 False False
                  ((s', mi'), o) = ppuStep (s, mi) inp
              if poBusy o then go s' mi' (poVramAddr o) False (o : acc)
                          else pure (s', mi', reverse (o : acc))
        (s', mi', outs) <- go s0 mi0 0 True []
        writeSTRef st (s', mi')
        pure (s', outs)
      checkPixels outs = mapM_ (\o -> case poPixel o of
          Nothing -> pure ()
          Just (i, sh) -> do
            v <- MU.read (mFb m) (fromEnum i)
            when (fromIntegral v /= (fromIntegral (pack sh) :: Int)) $
              fail' ("pixel " <> show (fromEnum i `mod` 160, fromEnum i `div` 160) <> ": Clash " <> show sh <> ", spec " <> show v)) outs
      compareState s = do
        e <- readSTRef err
        when (e == Nothing) $ do
          ly <- getSt m iLY
          dot <- getSt m iDOT
          md <- getSt m iPPUMODE
          lf <- getSt m iLYCFLAG
          sl <- getSt m iSTATLINE
          on <- (`testBit` 7) <$> getSt m iLCDC
          px <- getSt m iPX
          stall <- getSt m iSTALL
          nspr <- getSt m iNSPR
          let spec = (on, ly, dot, md, lf /= 0, sl /= 0, if md == 3 then (px, stall) else (0, 0), nspr)
              clash = ( psOn s, fromIntegral (psLy s), fromIntegral (psDot s), fromIntegral (psMode s)
                      , psLycFlag s, psStatLine s
                      , if psMode s == 3 then (fromIntegral (psPx s), fromIntegral (psStall s)) else (0, 0)
                      , fromEnum (psN s) )
          unless (spec == clash) $ fail' ("state: spec " <> show spec <> ", Clash " <> show clash)
      hook = Hook
        { hkPrePpu = do
            fr <- getSt m iFRAMES
            sl <- getSt m iSTATLINE
            writeSTRef pre (fr, sl /= 0)
        , hkPostPpu = do
            outs <- concat <$> forM [1 .. 4 :: Int] (const (snd <$> run CmdDot))
            (s, outs2) <- run CmdStat
            (fr0, sl0) <- readSTRef pre
            fr <- getSt m iFRAMES
            sl <- (/= 0) <$> getSt m iSTATLINE
            let all' = outs <> outs2
                vbl = any poVblank all'
                sirq = any poStatIrq all'
            when (vbl /= (fr /= fr0)) $ fail' ("VBlank request: Clash " <> show vbl)
            when (sirq /= (sl && not sl0)) $ fail' ("STAT request: Clash " <> show sirq)
            checkPixels all'
            compareState s
        , hkWrite = \a _ -> do
            (s, outs) <- run (if a == 0xFF40 then CmdLcdc else CmdStat)
            checkPixels outs
            compareState s
        }
  writeSTRef (mHook m) (Just hook)
  let loop = do
        c <- getSt m iCYC
        e <- readSTRef err
        b <- getSt m iBREAK
        if c >= n || e /= Nothing || b /= 0 then pure () else step m >> loop
  loop
  readSTRef err

-- | The machine's PPU registers.
ppuRegs :: Machine s -> ST s PpuRegs
ppuRegs m = do
  lcdc <- getSt m iLCDC
  stat <- getSt m iSTAT
  lyc <- getSt m iLYC
  let r a = fromIntegral <$> ramRead m a
  PpuRegs (fromIntegral lcdc) (fromIntegral stat) <$> r 0xFF42 <*> r 0xFF43 <*> pure (fromIntegral lyc)
    <*> r 0xFF47 <*> r 0xFF48 <*> r 0xFF49 <*> r 0xFF4A <*> r 0xFF4B

oamVec :: Machine s -> ST s (Vec 40 Oam)
oamVec m = do
  es <- forM [0 .. 39] $ \i -> do
    let a = 0xFE00 + 4 * fromIntegral (i :: Int)
        b k = fromIntegral <$> ramRead m (a + k)
    Oam <$> b 0 <*> b 1 <*> b 2 <*> b 3
  pure (maybe (error "oamVec") id (V.fromList es))

vramPair :: Machine s -> C.Unsigned 12 -> ST s (BitVector 16)
vramPair m p = do
  let a = 0x8000 + 2 * fromIntegral p :: Word16
  lo <- ramRead m a
  hi <- ramRead m (a + 1)
  pure ((fromIntegral hi :: BitVector 8) ++# (fromIntegral lo :: BitVector 8))
