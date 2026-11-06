{-# LANGUAGE CPP #-}
{-|
The build's optional features (side track O; PLAN.md, "Side track O"):
each on unless the build turns it off, with `make ... MOVIES=0` or
`SCREENSHOTS=0`, which pass @-DPLATFORM_NO_MOVIES@ or
@-DPLATFORM_NO_SCREENSHOTS@ to Clash. The cabal builds (the test suites)
pass neither, so they see the defaults. Off means the feature's logic isn't
generated at all (its platform modules turn into wires).
-}
module Platform.Features
  ( moviesEnabled
  , screenshotsEnabled
  ) where

import Clash.Prelude

-- | Input movies (recording and playback, frame advance: O.1 to O.3).
moviesEnabled :: Bool
#ifdef PLATFORM_NO_MOVIES
moviesEnabled = False
#else
moviesEnabled = True
#endif

-- | Screenshots (the frame capture into DDR: O.4).
screenshotsEnabled :: Bool
#ifdef PLATFORM_NO_SCREENSHOTS
screenshotsEnabled = False
#else
screenshotsEnabled = True
#endif
