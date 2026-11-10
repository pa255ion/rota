-- | The reactor's module (side track W): the page calls "Chip8.Desktop"'s exports
-- (and Web.Alloc's) after hs_init; this main is never run (-no-hs-main).
module Main (main) where

import Prelude

import Chip8.Desktop ()
import Web.Alloc ()

main :: IO ()
main = pure ()
