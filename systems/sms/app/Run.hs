-- | sms-run: the simulator (Gg.RunMain) with the Master System's FM unit
-- ("Sms.Spec.FmUnit"; --no-fm leaves it out).
module Main (main) where

import Prelude

import Gg.RunMain (runMain)
import Sms.Spec.FmUnit (newFmUnit)

main :: IO ()
main = runMain (Just newFmUnit)
