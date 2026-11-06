-- | snes-cart FILE...: the cartridge header Snes.Cart finds in each file,
-- one line each, and the scores of every place it considered.
module Main (main) where

import Prelude

import Control.Monad (forM_)
import qualified Data.ByteString as B
import Numeric (showHex)
import System.Environment (getArgs)

import Snes.Cart

main :: IO ()
main = do
  files <- getArgs
  forM_ files $ \f -> do
    b <- B.readFile f
    let (rom, copier) = stripCopier b
    putStrLn $ f <> ": " <> case parseCart b of
      Nothing -> "too small for a header"
      Just c -> unwords
        [ show (cartMapping c) <> (if cartFastRom c then " fast" else " slow")
        , "title " <> show (cartTitle c)
        , "type " <> hex (cartType c) <> maybe "" (\ch -> " " <> show ch) (cartChip c)
        , if cartBattery c then "battery" else "no battery"
        , "ROM " <> show (cartRomSize c `div` 1024) <> " KiB"
        , "SRAM " <> show (cartSramSize c `div` 1024) <> " KiB"
        , "region " <> hex (cartRegion c)
        , if cartChecksumOk c then "checksum OK" else "checksum bad"
        , if copier then "(copier header)" else ""
        , "scores " <> show [ (show m, s) | ((_, m), s) <- candidates rom ]
        ]
 where
  hex v = "$" <> showHex v ""
