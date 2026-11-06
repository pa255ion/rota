-- | Writes the CHIP-8 main RAM image for Chip8.Top's blockRamFile (loader
-- stage A, PLAN.md milestone 1b): 4096 lines of 8 binary digits, the font
-- of Chip8.Core at 0x050, the ROM at 0x200, and optional extra bytes.
--
--   chip8-ram OUT ROM [ADDR=VALUE ...]     e.g. chip8-ram ram.bin 5-quirks.ch8 0x1FF=1
--
-- Haskell rather than Zig (CLAUDE.md rule 9) because the font must come
-- from the same definition as the hardware's, Chip8.Core.fontRam.
module Main (main) where

import Prelude

import qualified Data.ByteString as BS
import System.Environment (getArgs)
import System.Exit (die)

import qualified Clash.Prelude as C
import Clash.Prelude.ROM.File (memFile)

import Chip8.Core (fontRam)

main :: IO ()
main = do
  args <- getArgs
  case args of
    out : rom : presets -> do
      prog <- BS.unpack <$> BS.readFile rom
      ps <- mapM parsePreset presets
      if length prog > 4096 - 0x200
        then die ("chip8-ram: " <> rom <> " is larger than 3584 bytes")
        else do
          let base  = C.toList fontRam
              img0  = take 0x200 base ++ map C.pack prog ++ drop (0x200 + length prog) base
              img   = foldl (\m (a, v) -> take a m ++ [v] ++ drop (a + 1) m) img0 ps
          writeFile out (memFile Nothing img)
    _ -> die "usage: chip8-ram OUT ROM [ADDR=VALUE ...]"
 where
  parsePreset s = case break (== '=') s of
    (a, '=' : v) | [(a', "")] <- reads a, [(v', "")] <- reads v, a' < 4096, v' < 256 ->
      pure (a' :: Int, fromIntegral (v' :: Int) :: C.BitVector 8)
    _ -> die ("chip8-ram: bad preset " <> s <> " (want ADDR=VALUE, e.g. 0x1FF=1)")
