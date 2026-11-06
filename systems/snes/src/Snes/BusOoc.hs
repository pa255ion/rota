{-|
The 5A22 alone with WRAM and SRAM, for its out-of-context synthesis
(PLAN.md step 7.9a; @make snes-ooc SNES_OOC=Snes.BusOoc
SNES_OOC_TOP=snes_bus_ooc@): "Snes.Bus" (the 65C816 core inside it) with
WRAM (128 KiB) and SRAM (32 KiB) as block RAMs, the PPU's, the ROM's and
the sound module's sides as the top's ports, registered. Its numbers go
in docs/systems/snes.md ("Measured").
-}
module Snes.BusOoc
  ( topEntity
  ) where

import Clash.Prelude

import Platform.Domains (DomPix)
import Snes.Bus
import Snes.Ppu (Access (..))

-- | In: the enable; the cartridge (the mapping 0-2, the ROM's two parts,
-- SRAM's size); the pads; the ROM's byte (valid, value); the PPU's read
-- (valid, open bus, value); the sound module (ready, its read).
-- Out: the tick; the PPU's access (kind 0 none, 1 write, 2 read, 3 the
-- latch; register; value); the ROM read (valid, address); the sound
-- module's access (kind, port, value); the line's end; the CPU's step.
topEntity
  :: Clock DomPix -> Reset DomPix
  -> Signal DomPix Bool
  -> Signal DomPix (Unsigned 2) -> Signal DomPix (Unsigned 24) -> Signal DomPix (Unsigned 24) -> Signal DomPix (Unsigned 17)
  -> Signal DomPix (Unsigned 12) -> Signal DomPix (Unsigned 12)
  -> Signal DomPix Bool -> Signal DomPix (Unsigned 8)
  -> Signal DomPix Bool -> Signal DomPix Bool -> Signal DomPix (Unsigned 8)
  -> Signal DomPix Bool -> Signal DomPix (Unsigned 8)
  -> Signal DomPix (Bool, Unsigned 2, Unsigned 6, Unsigned 8, Bool, Unsigned 23, Unsigned 2, Unsigned 2, Unsigned 8, Bool, Bool)
topEntity clk rst run cmap romP romQ sram p1 p2 romV romD ppuV ppuOpen ppuD apuOk apuD =
  withClockResetEnable clk rst enableGen $
  let r :: NFDataX a => a -> Signal DomPix a -> Signal DomPix a
      r = register
      cfg = (\m p q s -> CartCfg (case m of 1 -> CmHiRom; 2 -> CmExHiRom; _ -> CmLoRom) p q s)
              <$> r 0 cmap <*> r 0 romP <*> r 0 romQ <*> r 0 sram
      pads = (\a b -> a :> b :> Nil) <$> r 0 p1 <*> r 0 p2
      romIn = (\v d -> if v then Just d else Nothing) <$> r False romV <*> r 0 romD
      ppuR = (\v o d -> if v then Just (if o then Nothing else Just d) else Nothing) <$> r False ppuV <*> r False ppuOpen <*> r 0 ppuD
      inp = BusIn <$> r False run <*> cfg <*> pads <*> wramD <*> sramD <*> romIn <*> ppuR <*> r True apuOk <*> r True apuOk <*> r 0 apuD <*> dmaD
      st = register busInit st'
      (st', out) = unbundle (busClock <$> st <*> inp)
      wramD = blockRam1 NoClearOnReset (SNat @131072) 0 (boWram <$> out) (wramW <$> out)
      sramD = blockRam1 NoClearOnReset (SNat @32768) 0 (boSram <$> out) (sramW <$> out)
      -- the DMA registers: 16 memories (a register each), 8 channels deep
      dmaD = bundle (map dmaReg indicesI)
      dmaReg :: Index 16 -> Signal DomPix (Unsigned 8)
      dmaReg k = blockRam (replicate d8 0xFF) (boDmaRd <$> out) ((\o -> fmap (boDmaWCh o,) (boDmaW o !! k)) <$> out)
      wramW o = fmap (\v -> (boWram o, v)) (boWramW o)
      sramW o = fmap (\v -> (boSram o, v)) (boSramW o)
      flat o =
        let (pk, pr, pv) = case boPpu o of
              Just (AWrite a v) -> (1, a, v)
              Just (ARead a m) -> (2, a, m)
              Just ALatch -> (3, 0, 0)
              Nothing -> (0, 0, 0)
            (rv, ra) = maybe (False, 0) (True,) (boRom o)
            (ak, ap, av) = case boApu o of
              Just (port, Just v) -> (1, port, v)
              Just (port, Nothing) -> (2, port, 0)
              Nothing -> (0, 0, 0)
        in (boTick o, pk, pr, pv, rv, ra, ak, ap, av, boLineEnd o, boStep o /= Nothing)
  in register (False, 0, 0, 0, False, 0, 0, 0, 0, False, False) (flat <$> out)
{-# NOINLINE topEntity #-}
{-# ANN topEntity
  (Synthesize
    { t_name   = "snes_bus_ooc"
    , t_inputs = [ PortName "CLK", PortName "RST", PortName "RUN"
                 , PortName "CMAP", PortName "ROMP", PortName "ROMQ", PortName "SRAM"
                 , PortName "PAD1", PortName "PAD2"
                 , PortName "ROM_V", PortName "ROM_D"
                 , PortName "PPU_V", PortName "PPU_OPEN", PortName "PPU_D"
                 , PortName "APU_OK", PortName "APU_D" ]
    , t_output = PortProduct ""
                 [ PortName "TICK", PortName "PPU_K", PortName "PPU_R", PortName "PPU_W"
                 , PortName "ROM_RV", PortName "ROM_A"
                 , PortName "APU_K", PortName "APU_P", PortName "APU_W"
                 , PortName "LINE_END", PortName "STEP" ]
    }) #-}
