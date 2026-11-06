-- | Side track Q (the cassette's depth 1): our cassette boot programs as
-- tape images (make a800-asm):
--
-- > cabal run a800-cas -- <boot.bin> <out.cas>
--
-- the file (its boot header first, cas_boot.s) cut into 128-byte records
-- with an end-of-file record ("A800.Media"'s 'casBoot'), as a .cas
-- ('encodeCas').
module Main (main) where

import Prelude

import qualified Data.ByteString as BS
import System.Environment (getArgs)
import System.Exit (exitFailure)

import A800.Media (casBoot, encodeCas)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [inF, outF] -> BS.readFile inF >>= BS.writeFile outF . encodeCas . casBoot
    _ -> putStrLn "usage: a800-cas <boot.bin> <out.cas>" >> exitFailure
