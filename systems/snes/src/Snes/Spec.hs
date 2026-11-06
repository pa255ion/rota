{-|
The Super Nintendo reference model (docs/systems/snes.md): the machine
of step 7.3 ("Snes.Spec.Machine"), the memory map ("Snes.Spec.Map"), the
sound module of 7.4 ("Snes.Spec.Apu", "Snes.Spec.Dsp"); the picture (7.5)
to come.
-}
module Snes.Spec
  ( module Snes.Spec.Machine
  , module Snes.Spec.Map
  , module Snes.Spec.Apu
  ) where

import Snes.Spec.Apu
import Snes.Spec.Machine
import Snes.Spec.Map
