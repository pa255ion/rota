{-# LANGUAGE CPP #-}
{-|
The µPD77C25 alone with its memories, for its out-of-context synthesis
(M7 step 7.16c; @make snes-ooc SNES_OOC=Snes.Upd77c25Ooc
SNES_OOC_TOP=snes_upd77c25_ooc@): "Snes.Upd77c25" with the program ROM
(2,048 × 24 bits) and the data ROM (1,024 × 16) as block RAMs written
through a load port (as the PS will write the user's firmware), RAM (256
× 16) as a true dual-port one, every port registered. Its numbers go in
docs/systems/snes.md.
-}
module Snes.Upd77c25Ooc
  ( topEntity
  ) where

import Clash.Prelude

import Platform.Domains (DomPix)
import Snes.Upd77c25
#ifdef SNES_SIM_STRICT
import Clash.Sized.Internal.BitVector (unsafeToNatural)
#endif

-- | (Haskell's strict simulation only: the memories' undefined bits 0, as
-- Verilator has them; the HDL: the bits themselves)
simDef :: BitPack a => a -> a
#ifdef SNES_SIM_STRICT
simDef = unpack . fromIntegral . unsafeToNatural . pack
#else
simDef = id
#endif

-- | An instruction due; the SNES's access (0 none, 1 DR read, 2 SR read, 3
-- DR write) and its byte; a load (enable, the data ROM else the program,
-- the address, the word); reset. Out: the SNES's byte, an instruction ran.
topEntity
  :: Clock DomPix -> Reset DomPix
  -> Signal DomPix Bool
  -> Signal DomPix (Unsigned 2) -> Signal DomPix (Unsigned 8)
  -> Signal DomPix Bool -> Signal DomPix Bool -> Signal DomPix (Unsigned 11) -> Signal DomPix (BitVector 24)
  -> Signal DomPix Bool
  -> Signal DomPix (Unsigned 8, Bool)
topEntity clk rst go acc v le ld la lw rs = withClockResetEnable clk rst enableGen $
  let go' = register False go
      acc' = register Nothing (accOf <$> acc <*> v)
      load = register Nothing ((\e d a w -> if e then Just (d, a, w) else Nothing) <$> le <*> ld <*> la <*> lw)
      rs' = register False rs
      c = register updCorePowerOn c'
      req = updReq <$> c
      prog = blockRam (replicate (SNat @2048) 0) (raPc <$> req) ((\l -> case l of
                                                              Just (False, a, w) -> Just (a, w)
                                                              _ -> Nothing) <$> load)
      dat = blockRam (replicate d1024 0) (raRp <$> req) ((\l -> case l of
                                                              Just (True, a, w) -> Just (resize a, unpack (slice d15 d0 w))
                                                              _ -> Nothing) <$> load)
      (memA, memK) = trueDualPortBlockRam
        ((\r o -> maybe (RamRead (fromIntegral (raDp r))) (\(a, x) -> RamWrite (fromIntegral a) x) (uoRam o) :: RamOp 256 (Unsigned 16)) <$> req <*> out)
        ((\r -> RamRead (fromIntegral (raDpK r)) :: RamOp 256 (Unsigned 16)) <$> req)
      inp = UpdIn <$> go' <*> (simDef <$> prog) <*> (simDef <$> dat) <*> (simDef <$> memA) <*> (simDef <$> memK) <*> acc' <*> rs'
      (c', out) = unbundle (updCycle <$> c <*> inp)
  in register (0, False) ((\o -> (uoByte o, uoStep o)) <$> out)
 where
  accOf a x = case a of
    1 -> Just SReadDr
    2 -> Just SReadSr
    3 -> Just (SWriteDr x)
    _ -> Nothing

{-# ANN topEntity
  (Synthesize
    { t_name = "snes_upd77c25_ooc"
    , t_inputs = [ PortName "CLK", PortName "RST", PortName "GO", PortName "ACC", PortName "ACCV"
                 , PortName "LOAD_EN", PortName "LOAD_DATA", PortName "LOAD_ADDR", PortName "LOAD_WORD", PortName "UPD_RESET" ]
    , t_output = PortProduct "" [PortName "BYTE", PortName "STEP"]
    }) #-}
