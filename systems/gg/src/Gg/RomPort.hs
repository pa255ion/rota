{-# LANGUAGE StrictData #-}
{-|
The cartridge ROM from DDR, without changing the Game Gear's timing (PLAN.md
step 5.9, the user's option (a), 2026-10-07; docs/systems/gamegear.md, "ROM
in DDR").

"Platform.Mem.Ddr"'s port answers a hit the next clock and a miss after
about 15 pixel clocks, at worst 35 (docs/ARCHITECTURE.md, "DDR path");
a Game Gear memory read leaves about 21 from address to data. So a miss
can be late, and the core can't simply wait (the spec has no wait
states). Instead the whole machine waits: on a T-state boundary where
the pending ROM read's byte isn't there yet, the core gets no clock
('rpRun' false), which the lockstep tests show changes nothing for the
CPU, the VDP or the PSG (they all stop together; @gg-lockstep@ runs the
machine with a DDR of random latency against the spec). Real time is
made up afterwards: credits accrue at the core's rate (about 85 % of
clocks) up to a cap, and the core runs on every clock while it has
credit, so a stall of a few dozen clocks is caught up within a few
hundred.

One step per pixel clock ('romPortStep'): it asks the DDR port for the
pending ROM read's address as soon as the core shows it (one request
outstanding), keeps the answered byte with its address ('rpByte'), and
decides the core's clock.
-}
module Gg.RomPort
  ( RomPort (..)
  , RomIn (..)
  , RomOut (..)
  , romPortInit
  , romPortStep
  , creditCap
  ) where

import Clash.Prelude

data RomPort = RomPort
  { rpCredit :: Unsigned 48                    -- ^ active clocks owed, × 2^32
  , rpHave   :: Maybe (Unsigned 21, Unsigned 8) -- ^ a ROM byte and its address
  , rpAsked  :: Maybe (Unsigned 21)            -- ^ the request outstanding
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFDataX

data RomIn = RomIn
  { riWant     :: Maybe (Unsigned 21)  -- ^ 'Gg.Core.ggRomWant'
  , riBoundary :: Bool                 -- ^ 'Gg.Core.ggTBoundary'
  , riAnswer   :: Maybe (Vec 8 (Unsigned 8)) -- ^ the DDR port's answer: the 8 bytes holding the asked address
  , riInc      :: Unsigned 32          -- ^ credit per clock ('Gg.Display.runIncrement')
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFDataX

data RomOut = RomOut
  { roRequest :: Maybe (Unsigned 21)   -- ^ ask the DDR port (ROM offset)
  , roRun     :: Bool                  -- ^ give the core an active clock
  , roByte    :: Unsigned 8            -- ^ the core's ROM input
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFDataX

romPortInit :: RomPort
romPortInit = RomPort 0 Nothing Nothing

-- | At most this many active clocks owed (about 190 µs of catch-up); a
-- longer shortfall slows the machine down instead.
creditCap :: Unsigned 48
creditCap = 4096 `shiftL` 32

romPortStep :: RomPort -> RomIn -> (RomPort, RomOut)
romPortStep s i =
  let -- the answer to the request outstanding
      have = case (riAnswer i, rpAsked s) of
        (Just w, Just a) -> Just (a, w !! (resize a :: Unsigned 3))
        _ -> rpHave s
      asked = case riAnswer i of
        Just _ -> Nothing
        Nothing -> rpAsked s
      ready = case riWant i of
        Just a -> fmap fst have == Just a
        Nothing -> True
      stall = riBoundary i && not ready
      credit0 = min creditCap (rpCredit s + resize (riInc i))
      run = credit0 >= (1 `shiftL` 32) && not stall
      credit = if run then credit0 - (1 `shiftL` 32) else credit0
      -- a request: the wanted address, not had, none outstanding
      request = case riWant i of
        Just a | not ready && asked == Nothing && riAnswer i == Nothing -> Just a
        _ -> Nothing
      asked' = case request of
        Just a -> Just a
        Nothing -> asked
      byte = maybe 0xFF snd have
  in (RomPort credit have asked', RomOut request run byte)
