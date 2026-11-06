{-|
The PPU alone with its memories, for its out-of-context synthesis (PLAN.md
step 7.6; @make snes-ooc@): "Snes.Ppu" with VRAM (two 32 Ki × 8 chips),
CGRAM, OAM's low table (four byte lanes), the line buffer (8 banks) and
the column ring as block RAMs (the ring's are small: Vivado makes them LUT
RAM), its ports registered. Not a part of the machine: its numbers
(LUTs, block RAM, DSP48s, the worst path at 25.175 MHz) are recorded in
docs/systems/snes.md, for the go/no-go of step 7.7.

The two-port memories are Clash's 'trueDualPortBlockRam': port A the
PPU's reads, port B the CPU's reads and writes. The memories' contents at
power-on are not set (the machine's loader clears them, step 7.8).
-}
module Snes.PpuOoc
  ( topEntity
  , ppuWithMemories
  ) where

import Clash.Prelude

import Platform.Domains (DomPix)
import Snes.Ppu

-- | The PPU and its memories: a tick (a master clock) and its access in,
-- the PPU's outputs out.
ppuWithMemories
  :: forall dom. HiddenClockResetEnable dom
  => Signal dom Bool -> Signal dom (Maybe Access) -> Signal dom PpuOut
ppuWithMemories tick acc = out
 where
  st = register ppuInit st'
  (st', out) = unbundle (ppuClock <$> st <*> pin)
  pin = PpuIn <$> tick <*> acc <*> vA <*> vB <*> cgA <*> cgB <*> oamA <*> oamB <*> objs <*> ringE <*> ringW <*> ringH
  -- VRAM: the low and the high byte of each word
  (vAlo, vBlo) = trueDualPortBlockRam (vramRead 0 <$> out) (vramOp False <$> out)
  (vAhi, vBhi) = trueDualPortBlockRam (vramRead 1 <$> out) (vramOp True <$> out)
  vA = (\a b -> a :> b :> Nil) <$> vAlo <*> vAhi
  vB = (\a b -> a :> b :> Nil) <$> vBlo <*> vBhi
  vramRead :: Index 2 -> PpuOut -> RamOp 32768 (Unsigned 8)
  vramRead k o = RamRead (bitCoerce (poVramA o !! k))
  vramOp hiChip o = case poVramW o of
    Just (h, v) | h == hiChip -> RamWrite (bitCoerce (poVramB o)) v
    _ -> RamRead (bitCoerce (poVramB o))
  -- CGRAM
  (cgA, cgB) = trueDualPortBlockRam (cgRead <$> out) (cgOp <$> out)
  cgRead :: PpuOut -> RamOp 256 (Unsigned 15)
  cgRead o = RamRead (bitCoerce (poCgramA o))
  cgOp o = case poCgramW o of
    Just v -> RamWrite (bitCoerce (poCgramB o)) v
    Nothing -> RamRead (bitCoerce (poCgramB o))
  -- OAM's low table: lane l of each entry
  lanes = map lane indicesI
  lane :: Index 4 -> (Signal dom (Unsigned 8), Signal dom (Unsigned 8))
  lane l = trueDualPortBlockRam (oamRead <$> out) (oamOp l <$> out)
  oamRead :: PpuOut -> RamOp 128 (Unsigned 8)
  oamRead o = RamRead (bitCoerce (poOamA o))
  oamOp l o = case poOamW o !! l of
    Just v -> RamWrite (bitCoerce (poOamB o)) v
    Nothing -> RamRead (bitCoerce (poOamB o))
  oamA = bundle (map fst lanes)
  oamB = bundle (map snd lanes)
  -- the line buffer: bank b holds x = 8a + b at address a
  objs = bundle (map bank indicesI)
  bank :: Index 8 -> Signal dom (Unsigned 9)
  bank b = blockRam (replicate d32 0) (poObjA <$> out) ((\o -> poObjW o !! b) <$> out)
  -- the ring: each layer's map entries and char words (a memory a word),
  -- the char headers
  ringE = bundle (map entries indicesI)
  entries :: Index 4 -> Signal dom (Unsigned 16)
  entries n = blockRam (replicate d8 0) ((\o -> poRingA o !! n) <$> out) (entryW n <$> out)
  entryW n o = case poRingWE o of
    Just (k, j, v) | k == n -> Just (j, v)
    _ -> Nothing
  ringW = bundle (map (\n -> bundle (map (charWords n) indicesI)) indicesI)
  charWords :: Index 4 -> Index 4 -> Signal dom (Unsigned 16)
  charWords n w = blockRam (replicate d8 0) ((\o -> poRingA o !! n) <$> out) (wordW n w <$> out)
  wordW n w o = case poRingWW o of
    Just (k, l, j, v) | k == n && l == w -> Just (j, v)
    _ -> Nothing
  ringH = blockRam (replicate d32 (RingH 0 0 0)) (poRingH <$> out) (poRingWH <$> out)

-- | The synthesis top: the access as a kind (0 none, 1 write, 2 read, 3
-- the counters latched), the register, the value written and the CPU's
-- open bus; out the pixel (valid, x, row, left and right half-dots), a
-- read's value (valid, the value or the open bus as 0), H and V.
topEntity
  :: Clock DomPix -> Reset DomPix
  -> Signal DomPix Bool
  -> Signal DomPix (Unsigned 2) -> Signal DomPix (Unsigned 6)
  -> Signal DomPix (Unsigned 8) -> Signal DomPix (Unsigned 8)
  -> Signal DomPix (Bool, Unsigned 8, Unsigned 8, Unsigned 15, Unsigned 15, Bool, Unsigned 8, Unsigned 11, Unsigned 9)
topEntity clk rst tick kind r v mdr = withClockResetEnable clk rst enableGen $
  let acc = mk <$> kind <*> r <*> v <*> mdr
      out = ppuWithMemories (register False tick) (register Nothing acc)
  in register (False, 0, 0, 0, 0, False, 0, 0, 0) (flat <$> out)
 where
  mk :: Unsigned 2 -> Unsigned 6 -> Unsigned 8 -> Unsigned 8 -> Maybe Access
  mk k r' v' m = case k of
    1 -> Just (AWrite r' v')
    2 -> Just (ARead r' m)
    3 -> Just ALatch
    _ -> Nothing
  flat o =
    let (pv, (x, row, l, rr)) = maybe (False, (0, 0, 0, 0)) (True,) (poPixel o)
        (rv, rd) = case poRead o of
          Just val -> (True, maybe 0 id val)
          Nothing -> (False, 0)
    in (pv, x, row, l, rr, rv, rd, poHc o, poVc o)
{-# NOINLINE topEntity #-}
{-# ANN topEntity
  (Synthesize
    { t_name   = "snes_ppu_ooc"
    , t_inputs = [ PortName "CLK", PortName "RST", PortName "TICK"
                 , PortName "ACC_KIND", PortName "ACC_REG", PortName "ACC_VAL", PortName "ACC_MDR" ]
    , t_output = PortProduct ""
                 [ PortName "PIX_VALID", PortName "PIX_X", PortName "PIX_ROW"
                 , PortName "PIX_L", PortName "PIX_R"
                 , PortName "RD_VALID", PortName "RD_VAL"
                 , PortName "HC", PortName "VC" ]
    }) #-}
