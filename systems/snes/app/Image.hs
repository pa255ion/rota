{-|
@snes-image ROM DIR [--ipl FILE]@: the Super Nintendo tops' block RAM
images (PLAN.md step 7.10), one word a line in binary digits, as
'blockRamFile' reads them: @cart.bin@, the power-on cartridge word
("Snes.Machine": the mapping, the ROM's size as two powers of two, SRAM's
code; 16 bits; until the PS brings one), and @ipl.mem@, the boot ROM's 64 bytes
(build/snes/ipl.bin, ours; or FILE). And @rom.bin@: the ROM without a
copier's header, for the harness's DDR (@--ddr-file 0x02000000=rom.bin@),
as the PS will load it.
-}
module Main (main) where

import Prelude

import Data.Bits (countLeadingZeros, finiteBitSize, popCount, shiftL, testBit, (.|.))
import qualified Data.ByteString as BS
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)

import Snes.Cart (Cart (..), Mapping (..), parseCart, stripCopier)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [rom, dir] -> image rom dir "build/snes/ipl.bin"
    [rom, dir, "--ipl", f] -> image rom dir f
    _ -> hPutStrLn stderr "usage: snes-image ROM DIR [--ipl FILE]" >> exitFailure

-- | The log of a power of two (0 for 0).
lg :: Int -> Int
lg n = if n <= 0 then 0 else finiteBitSize n - 1 - countLeadingZeros n

image :: FilePath -> FilePath -> FilePath -> IO ()
image rom dir iplF = do
  file <- BS.readFile rom
  ipl <- BS.readFile iplF
  cart <- maybe (hPutStrLn stderr ("snes-image: " <> rom <> ": no cartridge header") >> exitFailure) pure (parseCart file)
  let bytes = fst (stripCopier file)
      n = BS.length bytes
      p = if n == 0 then 0 else 1 `shiftL` lg n
      q = n - p
      sram = cartSramSize cart
  if n < 0x1000 || n > 0x800000 || not (q == 0 || popCount q == 1)
    then hPutStrLn stderr ("snes-image: " <> rom <> ": " <> show n <> " bytes: 4 KiB to 8 MiB, at most two powers of two") >> exitFailure
    else if BS.length ipl /= 64
      then hPutStrLn stderr ("snes-image: " <> iplF <> ": not 64 bytes") >> exitFailure
      else if sram /= 0 && popCount sram /= 1
        then hPutStrLn stderr ("snes-image: SRAM of " <> show sram <> " bytes: not a power of two") >> exitFailure
        else do
          let m = case cartMapping cart of LoRom -> 0; HiRom -> 1; ExHiRom -> 2
              -- (the same as platform/ps/systems/snes_cart.zig's; no copier
              -- header in rom.bin, bit 15 clear)
              word = m .|. ((lg p - 12) `shiftL` 2) .|. (lg q `shiftL` 6) .|. (code `shiftL` 11) :: Int
              code = if sram == 0 then 0 else lg sram - 10
              bin :: Int -> Int -> String
              bin k w = [ if testBit w i then '1' else '0' | i <- [k - 1, k - 2 .. 0 :: Int] ]
          writeFile (dir </> "cart.bin") (unlines [bin 16 word])
          writeFile (dir </> "ipl.mem") (unlines [ bin 8 (fromIntegral b) | b <- BS.unpack ipl ])
          BS.writeFile (dir </> "rom.bin") bytes
