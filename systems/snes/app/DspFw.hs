{-|
@snes-dspfw FILE@: our DSP-1 test cartridge's firmware (M7 step 7.16d;
"Snes.Spec.Upd77c25Asm".'dspTestFirmware': our own µPD77C25 program, not
the user's dump), 8 KiB as the newer dumps are, for systems/snes/test/asm/
dsp1.s (make snes-asm writes build/snes/asm/dsptest.rom).
-}
module Main (main) where

import Prelude

import qualified Data.ByteString as B
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import Snes.Spec.Upd77c25Asm (dspTestFirmware)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [f] -> B.writeFile f dspTestFirmware
    _ -> hPutStrLn stderr "usage: snes-dspfw FILE" >> exitFailure
