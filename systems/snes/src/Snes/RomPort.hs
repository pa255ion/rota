{-# OPTIONS_GHC -fconstraint-solver-iterations=0 #-}
{-# LANGUAGE StrictData #-}
{-|
The cartridge ROM from DDR (PLAN.md step 7.9c; docs/systems/snes.md,
"The machine in Clash"): between "Snes.Bus"'s ROM reads ('boRom', one
clock each, the byte expected on a later clock) and "Platform.Mem.Ddr"'s
port (one request outstanding, the 8-byte word holding the address in
answer).

The bus needs no more than that: a ROM read's value is waited for where
it is used (the CPU's cycle doesn't end, DMA's second access and HDMA's
table steps don't happen, and no master clock passes meanwhile), so a
slow answer holds the machine and changes nothing it does (@snes-core@
runs it against the spec with a DDR of random latency). The last word
answered is kept: a read in it (the next opcode, the next operand) is
answered on the next clock, as from block RAM.
-}
module Snes.RomPort
  ( RomPort (..)
  , RomIn (..)
  , RomOut (..)
  , romPortInit
  , romPortStep
  ) where

import Clash.Prelude

data RomPort = RomPort
  { rpWord  :: Maybe (Unsigned 20, Vec 8 (Unsigned 8)) -- ^ the last word answered (its address / 8)
  , rpAsked :: Maybe (Unsigned 23)    -- ^ the request outstanding at the DDR port
  , rpWant  :: Maybe (Unsigned 23)    -- ^ the bus's read waiting
  , rpOut   :: Maybe (Unsigned 8)     -- ^ its byte, for the bus on the next clock
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (NFDataX, BitPack)

data RomIn = RomIn
  { riRead   :: Maybe (Unsigned 23)            -- ^ 'Snes.Bus.boRom'
  , riAnswer :: Maybe (Vec 8 (Unsigned 8))     -- ^ the DDR port's answer to 'rpAsked'
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (NFDataX, BitPack)

data RomOut = RomOut
  { roRequest :: Maybe (Unsigned 23)    -- ^ ask the DDR port (a ROM offset)
  , roByte    :: Maybe (Unsigned 8)     -- ^ 'Snes.Bus.biRom'
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (NFDataX, BitPack)

romPortInit :: RomPort
romPortInit = RomPort Nothing Nothing Nothing Nothing

wordOf :: Unsigned 23 -> Unsigned 20
wordOf a = resize (a `shiftR` 3)

byteIn :: Vec 8 (Unsigned 8) -> Unsigned 23 -> Unsigned 8
byteIn w a = w !! (resize a :: Unsigned 3)

romPortStep :: RomPort -> RomIn -> (RomPort, RomOut)
romPortStep s i = (s', RomOut req (rpOut s))
 where
  -- the answer to the request outstanding: the word kept
  word = case (riAnswer i, rpAsked s) of
    (Just w, Just a) -> Just (wordOf a, w)
    _ -> rpWord s
  asked = case riAnswer i of
    Just _ -> Nothing
    Nothing -> rpAsked s
  -- the read waiting: a new one, or the one before
  want = case riRead i of
    Just a -> Just a
    Nothing -> rpWant s
  inWord a = case word of
    Just (k, w) | k == wordOf a -> Just (byteIn w a)
    _ -> Nothing
  (out, want') = case want of
    Just a | Just v <- inWord a -> (Just v, Nothing)
    _ -> (Nothing, want)
  -- a request: the read waiting, not in the word kept, none outstanding
  -- (the answer's own clock included: the port takes the next request
  -- from the clock after)
  req = case want' of
    Just a | asked == Nothing && riAnswer i == Nothing -> Just a
    _ -> Nothing
  s' = RomPort word (maybe asked Just req) want' out
