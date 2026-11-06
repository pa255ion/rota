-- | gg-run: the simulator (Gg.RunMain), without the Master System's FM
-- unit (the sms package's sms-run has it).
module Main (main) where

import Prelude

import Gg.RunMain (runMain)

main :: IO ()
main = runMain Nothing
