{-|
@gg-image ROM DIR@: the cartridge word for the Game Gear's tops (PLAN.md
steps 5.8, 5.9; until the PS link brings it with the ROM), one word per
line in binary digits, as 'blockRamFile' reads it: @cart.bin@, 16 bits:
bit 0 the Codemasters mapper ('Gg.Spec.detectMapper'), bits 15-8 the
ROM's 16 KiB banks minus one (a power of two, up to 1 MiB). The ROM itself
goes to DDR (in simulation: @--ddr-file 0x02000000=ROM@).

@gg-image ROM DIR --bios FILE@ (the Master System, M6): bits 7-3 also give
the BIOS image's 16 KiB banks (a power of two, up to 256 KiB; 0 without
one); the BIOS goes to DDR after the cartridge's 1 MiB (0x02100000).
-}
module Main (main) where

import Prelude

import qualified Data.ByteString as BS
import Data.Bits (testBit, (.|.), shiftL)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)

import Gg.Spec (Mapper (..), detectMapper)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [rom, dir] -> image rom dir Nothing
    [rom, dir, "--bios", b] -> image rom dir (Just b)
    _ -> hPutStrLn stderr "usage: gg-image ROM DIR [--bios FILE]" >> exitFailure

image :: FilePath -> FilePath -> Maybe FilePath -> IO ()
image rom dir bios = do
  bytes <- BS.readFile rom
  biosN <- maybe (pure 0) (fmap BS.length . BS.readFile) bios
  let n = BS.length bytes
      banks = until (>= max 1 ((n + 0x3FFF) `div` 0x4000)) (* 2) 1 :: Int
      biosBanks = if biosN == 0 then 0 else until (>= max 1 ((biosN + 0x3FFF) `div` 0x4000)) (* 2) 1 :: Int
  if n == 0 || banks > 64
    then hPutStrLn stderr ("gg-image: " <> rom <> ": " <> show n <> " bytes; up to 1 MiB") >> exitFailure
    else if biosBanks > 16
      then hPutStrLn stderr ("gg-image: the BIOS: " <> show biosN <> " bytes; up to 256 KiB") >> exitFailure
      else do
        let word = (if detectMapper bytes == Codemasters then 1 else 0) .|. (biosBanks `shiftL` 3) .|. ((banks - 1) `shiftL` 8) :: Int
            bin w = [ if testBit w i then '1' else '0' | i <- [15, 14 .. 0 :: Int] ]
        writeFile (dir </> "cart.bin") (unlines [bin word])
