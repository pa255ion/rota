-- | The reactor's module (side track W): the page calls "Nes.Desktop"'s exports
-- (and Web.Alloc's) after hs_init; this main is never run (-no-hs-main).
module Main (main) where

import Prelude

import Nes.Desktop ()
import Web.Alloc ()

main :: IO ()
main = pure ()
