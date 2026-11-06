-- | Block RAM images of a cartridge for a bitstream or a simulation with the
-- cartridge built in (make hdl / make sim SYSTEM=nes ROM=…):
--
-- > cabal run nes-image -- <rom.nes> <dir>
--
-- writes @<dir>/chr.bin@ (256 KiB; zeros for CHR RAM; PRG ROM is read from
-- DDR, the whole file: the harness's @--ddr-file@) and @<dir>/cart.bin@ (the cartridge configuration, 'CartCfg' packed),
-- one word per line in binary digits, as 'blockRamFile' reads them.
module Main (main) where

import Prelude

import qualified Data.ByteString as BS
import Data.Bits (testBit)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath ((</>))

import qualified Clash.Prelude as C
import Nes.Cartridge
import Nes.Mapper (cartCfgFromHeader)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [rom, dir] -> do
      file <- BS.readFile rom
      r <- either (\e -> putStrLn ("nes-image: " <> e) >> exitFailure) pure (parseRom file)
      let h = romHeader r
      cfg <- case (checkSupported h, cartCfgFromHeader h) of
        (Left e, _) -> putStrLn ("nes-image: " <> rom <> ": " <> e) >> exitFailure
        (_, Nothing) -> putStrLn ("nes-image: " <> rom <> ": not supported") >> exitFailure
        (_, Just c) -> pure c
      image (dir </> "chr.bin") 262144 (romChr r)
      let bits = C.pack cfg
      writeFile (dir </> "cart.bin") ([ if testBit bits i then '1' else '0' | i <- reverse [0 .. C.finiteBitSize bits - 1] ] <> "\n")
    _ -> putStrLn "usage: nes-image <rom.nes> <dir>" >> exitFailure
 where
  image path size bytes =
    writeFile path (unlines [ byte (if i < BS.length bytes then BS.index bytes i else 0) | i <- [0 .. size - 1] ])
  byte b = [ if testBit b i then '1' else '0' | i <- [7, 6 .. 0] ]
