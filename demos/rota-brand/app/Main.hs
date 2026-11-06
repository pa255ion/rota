-- | Write the generator's files (Rota.Emit.outputs) under an output
-- directory, and optionally the chime as WAV files through the specs'
-- APUs (to listen to):
--
-- > rota-brand [--out DIR] [--wav DIR] [--stats]     (default --out build/demos/gen)
module Main (main) where

import Control.Monad (forM_)
import qualified Data.ByteString.Lazy as BL
import System.Directory (createDirectoryIfMissing)
import System.Environment (getArgs)
import System.FilePath (takeDirectory, (</>))

import qualified Data.ByteString as BS
import Rota.Emit (binaryOutputs, outputs)
import Rota.Gb.Rom (gbStats)
import Rota.Nes.Rom (nesStats)
import Rota.Render
import Rota.Sound.Gb (gbStream)
import Rota.Sound.Nes (nesStream)

main :: IO ()
main = do
  args <- getArgs
  let opt k d = case dropWhile (/= k) args of
        (_ : v : _) -> v
        _ -> d
      out = opt "--out" "build/demos/gen"
  forM_ outputs $ \(p, s) -> do
    createDirectoryIfMissing True (takeDirectory (out </> p))
    writeFile (out </> p) s
  forM_ binaryOutputs $ \(p, bs) -> do
    createDirectoryIfMissing True (takeDirectory (out </> p))
    BS.writeFile (out </> p) (BS.pack bs)
  if "--stats" `elem` args then putStr ("Game Boy\n" <> gbStats <> "NES\n" <> nesStats) else pure ()
  case dropWhile (/= "--wav") args of
    (_ : dir : _) -> do
      createDirectoryIfMissing True dir
      BL.writeFile (dir </> "rota-gb.wav") (wavFile (rPcm (renderGb gbStream 90)))
      BL.writeFile (dir </> "rota-nes.wav") (wavFile (rPcm (renderNes nesStream 90)))
    _ -> pure ()
