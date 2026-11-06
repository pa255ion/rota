{-|
ROM loading from the PS (milestone 1b): the CHIP-8 side of the PS link
("Platform.Ps.Link", which documents the EMIO GPIO lines; the PS side is
@platform/ps/@). Bits 0 to 15 of the PS → PL word are the 16 CHIP-8 keys
(bit k = key k).

Address map (below 'Platform.Ps.Link.sharedBase'): 0x000000 to 0x000FFF is
main RAM, written only while hold is high (the core is then in soft reset,
'ciReset', and doesn't use RAM). When hold falls the program starts from
the power-on state. Writes anywhere else below the shared range are
ignored.
-}
module Chip8.Loader
  ( ramWrite
  , loader
  ) where

import Clash.Prelude

import Platform.Ps.Link

-- | The main-RAM write for a link write, if it is one (and hold is high).
ramWrite :: Bool -> Maybe (LinkAddr, BitVector 8) -> Maybe (Unsigned 12, BitVector 8)
ramWrite True (Just (a, d)) | a < 0x1000 = Just (resize a, d)
ramWrite _ _ = Nothing

-- | Main-RAM writes from synchronised link lines.
loader
  :: HiddenClockResetEnable dom
  => Signal dom PsGpio -> Signal dom (Maybe (Unsigned 12, BitVector 8))
loader ps = ramWrite <$> (pgHold <$> ps) <*> psLink ps
