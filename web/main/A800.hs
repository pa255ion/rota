-- | The reactor's module (side track W): the page calls "A800.Desktop"'s exports
-- (and Web.Alloc's) after hs_init; this main is never run (-no-hs-main).
module Main (main) where

import Prelude

import A800.Desktop ()
import Web.Alloc ()

main :: IO ()
main = pure ()
