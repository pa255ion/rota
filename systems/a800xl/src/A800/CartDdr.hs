{-|
The 800XL's cartridge in DDR (side track Q, 2026-10-21: the user's XEGS
cartridges, up to 128 KiB, every cartridge from DDR on the Arty): the
file the PS wrote at the media region, read through "Platform.Mem.Ddr"
(32 cache lines with prefetch, as the NES's PRG ROM, "Nes.Core"'s
'prgDdr', whose bookkeeping this follows). It answers 'A800.Core.Mems''
cartridge interface: a read request for one clock, the byte held, and
whether it is there yet; one read out at a time, a newer request waiting
for it (the core was reset meanwhile and asked again), and that stale
answer dropped.
-}
module A800.CartDdr
  ( cartDdr
  ) where

import Clash.Prelude
import Data.Maybe (isJust)

import Platform.Mem.Axi (AxiRdM2S, AxiRdS2M)
import Platform.Mem.Ddr (DdrAddr, ddrByte, ddrConfigDefault, ddrReadPort)

data Wait = Wait
  { wOut   :: Bool
  , wQueue :: Maybe (Unsigned 24)
  , wStale :: Bool
  , wAddr  :: DdrAddr
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFDataX

-- | The cartridge from DDR at @base@ (the image's first byte: the file's
-- start, after a .car's 16-byte header), the cache emptied while @inval@
-- (the PS rewrites DDR while it holds the core). The request and the
-- answer are registered (timing), as 'prgDdr''s.
cartDdr
  :: HiddenClockResetEnable dom
  => Signal dom DdrAddr               -- ^ where the image starts
  -> Signal dom Bool                  -- ^ empty the cache
  -> Signal dom AxiRdS2M              -- ^ HP0's answers
  -> Signal dom (Maybe (Unsigned 24)) -- ^ a read (one clock)
  -> ((Signal dom (Unsigned 8), Signal dom Bool), Signal dom AxiRdM2S)
cartDdr base inval s2m req = ((held, not <$> pending), m2s)
 where
  (issue, ans0) = unbundle (mealy step (Wait False Nothing False 0) (bundle (base, req, resp)))
  ans = register Nothing ans0
  (resp, m2s, _status) = ddrReadPort (SNat @5) ddrConfigDefault inval (register Nothing issue) s2m
  held = register 0 (maybe <$> held <*> pure id <*> ans)
  -- a request made, its answer not back yet (the core waits meanwhile)
  pending = register False ((\p r a -> isJust r || (p && not (isJust a))) <$> pending <*> req <*> ans)
  step s (b, r, w) = case w of
    Just word
      | wStale s -> go (r <|> wQueue s) Nothing
      | otherwise -> (s { wOut = False, wQueue = r }, (Nothing, Just (unpack (ddrByte (wAddr s) word))))
    Nothing
      | wOut s -> (s { wQueue = r <|> wQueue s, wStale = wStale s || isJust r }, (Nothing, Nothing))
      | otherwise -> go (r <|> wQueue s) Nothing
   where
    go n out = (Wait (isJust n) Nothing False (maybe (wAddr s) full n), (full <$> n, out))
    full a = b + resize a
