{-|
Save states (side track L, PLAN.md L.6): what the platform gives a core
so its whole state can be read out and written back while it is paused,
without the core depending on it ("Save states stay outside the cores",
PLAN.md).

A core with save states offers three hooks, all inert unless the PS
asks for them, and all left out of the circuit when its top chooses
'NoStateHooks' (the Haskell value decides which circuit is built, so a
top without save states gets the core exactly as before):

* its state record, read (as bytes, 'asBytes');
* that record written back ('fromBytes'), only while frozen;
* its block RAMs' existing ports taken over, only while frozen.

__Frozen__ means: the PS asked for state access ('stateFreeze', with the
core paused by the menu) and the core is __at rest__, a point it
defines where nothing is under way (no memory read whose byte is still
to be taken, no work owed). It stays frozen while asked, whatever the
record holds meanwhile, so a state written byte by byte never runs.

The state's bytes are addressed in a space of the system's own
('StateOp'); the PS reaches it through a write window and a read window
in the system's link map, as the battery saves' (the reads come back on
the byte channel).
-}
module Platform.SaveState
  ( StateHooks (..)
  , StateOp (..)
  , stateOpWrite
  , stateWindow
  , asBytes
  , fromBytes
  , byteAt
  , replaceByte
  ) where

import Clash.Prelude

import Platform.Ps.Link (LinkAddr)

-- | Built in or not: a Haskell value, so 'NoStateHooks' builds the core
-- as it was (no logic for the hooks at all).
data StateHooks = NoStateHooks | WithStateHooks
  deriving stock (Show, Eq)

-- | One access to the state's bytes: read byte n (its value comes back
-- the next clock), or write byte n.
data StateOp
  = StRead (Unsigned 16)
  | StWrite (Unsigned 16) (BitVector 8)
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFDataX

stateOpWrite :: StateOp -> Bool
stateOpWrite (StWrite _ _) = True
stateOpWrite _ = False

-- | The two windows on the link: a data byte written at @wbase + n@
-- writes byte n of the state; one written at @rbase + n@ (any value)
-- reads byte n. Each 64 KiB.
stateWindow :: LinkAddr -> LinkAddr -> Maybe (LinkAddr, BitVector 8) -> Maybe StateOp
stateWindow wbase rbase (Just (a, d))
  | a >= wbase && a < wbase + 0x10000 = Just (StWrite (resize (a - wbase)) d)
  | a >= rbase && a < rbase + 0x10000 = Just (StRead (resize (a - rbase)))
stateWindow _ _ _ = Nothing

-- | A record as @n@ bytes (zero padding in front; @n × 8@ must hold it:
-- each user checks it in a test, as the type checker can't always).
asBytes :: forall n a. (BitPack a, KnownNat n) => a -> Vec n (BitVector 8)
asBytes = unpack . (resize :: BitVector (BitSize a) -> BitVector (n * 8)) . pack

-- | The record back from its bytes ('asBytes'' inverse).
fromBytes :: forall n a. (BitPack a, KnownNat n) => Vec n (BitVector 8) -> a
fromBytes = unpack . (resize :: BitVector (n * 8) -> BitVector (BitSize a)) . pack

-- | Byte i of the bytes, 0 past their end.
byteAt :: KnownNat n => Vec n (BitVector 8) -> Unsigned 16 -> BitVector 8
byteAt v i
  | fromIntegral i < length v = v !! i
  | otherwise = 0

-- | Byte i replaced, nothing past their end.
replaceByte :: KnownNat n => Unsigned 16 -> BitVector 8 -> Vec n (BitVector 8) -> Vec n (BitVector 8)
replaceByte i b v
  | fromIntegral i < length v = replace i b v
  | otherwise = v
