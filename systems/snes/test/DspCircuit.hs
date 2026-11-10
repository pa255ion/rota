{-# OPTIONS_GHC -fconstraint-solver-iterations=0 #-}
{-# LANGUAGE FlexibleContexts #-}
-- | snes-savestate under cabal -f dsp1 (M7 step 7.16d): our DSP-1 test
-- cartridge on the whole circuit ("Snes.Core" in Clash's simulation, the
-- DSP-1 build): the firmware's load from DDR through the ROM port (our
-- firmware at offset 700000h, as "Snes.Machine" maps the companion), the
-- 64 rounds and the probe; its verdict the backdrop, green.
module DspCircuit (dspCircuitTest) where

import Prelude

import Clash.Prelude (Signal, Unsigned, Vec, register, sampleN, systemClockGen, withClockResetEnable)
import qualified Clash.Prelude as C
import Clash.Explicit.Reset (resetGen)
import qualified Data.ByteString as B
import qualified Data.Vector.Unboxed as U
import Data.Word (Word8)
import Test.Tasty.HUnit

import Snes.Bus (CartCfg (..), CartMap (..))
import Snes.Cart
import Snes.Core
import Snes.Spec.Upd77c25Asm (dspTestFirmware)

dspCircuitTest :: Assertion
dspCircuitTest = do
  file <- B.readFile "build/snes/asm/dsp1.sfc"
  cart <- maybe (assertFailure "dsp1.sfc: no header" >> fail "") pure (parseCart file)
  let rom = U.fromList (B.unpack (fst (stripCopier file)))
      fw = U.fromList (B.unpack dspTestFirmware)
      cfg = CartCfg CmLoRom (fromIntegral (U.length rom)) 0 0
      byte :: Int -> Word8
      byte a | a >= 0x700000 = fw U.! ((a - 0x700000) `mod` U.length fw)
             | otherwise = rom U.! (a `mod` U.length rom)
      ddr :: C.HiddenClockResetEnable C.System => Signal C.System (Maybe (Unsigned 23)) -> Signal C.System (Maybe (Vec 8 (Unsigned 8)))
      -- (answered 40 clocks later, as a DDR's latency: the request kept
      -- until then; the core asks for one word at a time)
      ddr req = iterate (register Nothing) (fmap (\a -> C.map (\k -> fromIntegral (byte (fromIntegral a `div` 8 * 8 + k))) (C.iterateI (+ 1) (0 :: Int))) <$> req) !! 40
      out = withClockResetEnable systemClockGen (resetGen @C.System) C.enableGen $
        -- (the master clock enabled five clocks in six, as the pace holds
        -- the core back at layer 4)
        let runE = C.fromList (cycle [True, True, True, False, True, True])
            core = snesCore runE (pure cfg) (pure (C.repeat 0)) (ddr (scDdrReq core)) (pure (C.RamRead 0)) (pure stIdle) (pure ovlIdle)
        in C.bundle (scPixel core, scDdrReq core, scV core)
      samples = sampleN 1200000 out
      -- (the first clock with an undefined output, if any: which one)
      firstX = [ (k, either (const "pixel") (const "") (C.isX px), either (const "ddr") (const "") (C.isX dr), either (const "line") (const "") (C.isX vv))
               | (k, (px, dr, vv)) <- zip [0 :: Int ..] (take 200000 samples), either (const True) (const False) (C.isX (px, dr, vv)) ]
      fwReqs = length [ () | (_, Just a, _) <- samples, a >= 0x700000 ]
      firstLine = length (takeWhile (\(_, _, v) -> v == 0) samples)
      mid = [ l | (Just (x, row, l, _), _, _) <- samples, x == 128, row == 100 ]
  case take 1 firstX of
    x : _ -> assertFailure ("undefined at clock " <> show x)
    [] -> pure ()
  case reverse mid of
    l : _ -> assertBool ("the backdrop at the end: " <> show l <> " (green is 03E0h); " <> show fwReqs <> " firmware reads from DDR, the first line after " <> show firstLine <> " clocks")
                        (l == 0x03E0)
    [] -> assertFailure ("no pixel at (128, 100); " <> show fwReqs <> " firmware reads from DDR, the first line after " <> show firstLine <> " clocks")
