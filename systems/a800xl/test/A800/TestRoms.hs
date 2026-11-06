-- | Where the a800xl tests find their files.
module A800.TestRoms
  ( romRoot
  , asmRoot
  , osRom
  , basicRom
  , acidFile
  , scorchXex
  , loaderBin
  , loadFile
  ) where

import Prelude

import Control.Exception (ErrorCall (..), throwIO)
import qualified Data.ByteString as BS
import System.Directory (doesFileExist)
import System.FilePath ((</>), (<.>))

-- | Fetched files (tools/fetch-roms.sh), relative to the package
-- directory, where cabal runs the tests.
romRoot :: FilePath
romRoot = "../../roms/a800xl"

-- | Our own regression programs (make a800-asm).
asmRoot :: FilePath
asmRoot = "../../build/a800xl/asm"

-- | The XEX loader's page (make a800-asm).
loaderBin :: FilePath
loaderBin = "../../build/a800xl/xexload.bin"

osRom, basicRom, scorchXex :: FilePath
osRom = romRoot </> "os/altirraos_xl.rom"
basicRom = romRoot </> "os/altirra_basic.rom"
scorchXex = romRoot </> "games/scorch.xex"

acidFile :: String -> FilePath
acidFile n = romRoot </> "acid800" </> n <.> "xex"

loadFile :: FilePath -> IO BS.ByteString
loadFile p = do
  ok <- doesFileExist p
  if ok then BS.readFile p
        else throwIO (ErrorCall (p <> " missing: run make test SYSTEM=a800xl (fetches ROMs, assembles build/a800xl/asm)"))
