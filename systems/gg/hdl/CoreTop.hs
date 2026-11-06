{-|
A synthesis check for the whole Clash Game Gear (PLAN.md step 5.7; @make
gg-core-hdl@): 'Gg.Core.ggClock' with its RAM (8 KiB), cartridge RAM
(32 KiB) and VRAM (four 4 KiB byte RAMs) in block RAM, the ROM as a port
(DDR on the board, step 5.9). Not the board's top.
-}
module CoreTop (topEntity) where

import Clash.Prelude

import Gg.Core

-- | In: run (the clock enable of the 12-clocks-per-2-T-states schedule),
-- the ROM's byte, the pad and Start. Out: the ROM address, a window
-- pixel (valid, x, y, colour), the PSG's output after each of its clocks
-- (valid, left, right).
topEntity
  :: Clock System -> Reset System -> Enable System
  -> Signal System Bool -> Signal System (Unsigned 8) -> Signal System (Unsigned 8) -> Signal System Bool
  -> Signal System (Unsigned 21, Bool, Unsigned 8, Unsigned 8, Unsigned 12, Bool, Signed 16, Signed 16)
topEntity clk rst en runS romS padS startS = exposeClockResetEnable go clk rst en
 where
  go :: HiddenClockResetEnable System => Signal System (Unsigned 21, Bool, Unsigned 8, Unsigned 8, Unsigned 12, Bool, Signed 16, Signed 16)
  go =
    let gin = GgIn <$> runS <*> romS <*> ramD <*> cartD <*> vramD <*> padS <*> startS <*> pure MSega <*> pure 64 <*> pure 0xFF <*> pure 0 <*> pure 1 <*> pure Nothing
        out = mealy (\s i -> ggClock i s) (ggPowerOn False MSega 64) gin
        ramD = blockRam1 NoClearOnReset (SNat @8192) 0 (goRamAddr <$> out) (goRamWrite <$> out)
        cartD = blockRam1 NoClearOnReset (SNat @32768) 0 (goCartAddr <$> out) (goCartWrite <$> out)
        bank k = blockRam1 NoClearOnReset (SNat @4096) 0 (goVramRead <$> out) (bankWrite k . goVramWrite <$> out)
        vramD = bundle (bank 0 :> bank 1 :> bank 2 :> bank 3 :> Nil)
        result o =
          let (pv, px, py, pc) = case goPixel o of
                Just (x, y, c) -> (True, x, y, c)
                Nothing -> (False, 0, 0, 0)
              (av, al, ar) = case goAudio o of
                Just (l, r) -> (True, l, r)
                Nothing -> (False, 0, 0)
          in (goRomAddr o, pv, px, py, pc, av, al, ar)
    in result <$> out
  bankWrite :: Unsigned 2 -> Maybe (Unsigned 14, Unsigned 8) -> Maybe (Unsigned 12, Unsigned 8)
  bankWrite k w = case w of
    Just (a, b) | resize a == k -> Just (resize (a `shiftR` 2), b)
    _ -> Nothing
{-# OPAQUE topEntity #-}

{-# ANN topEntity
  (Synthesize
    { t_name = "gg_core_top"
    , t_inputs = [PortName "clk", PortName "rst", PortName "en", PortName "run", PortName "rom_data", PortName "pad", PortName "start"]
    , t_output = PortProduct "" [ PortName "rom_addr", PortName "pix_valid", PortName "pix_x", PortName "pix_y", PortName "pix_colour"
                                , PortName "aud_valid", PortName "aud_left", PortName "aud_right" ]
    }) #-}
