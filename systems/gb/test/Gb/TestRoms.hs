-- | Where the gb tests find their ROMs, and the screen golden helper.
module Gb.TestRoms
  ( romRoot
  , asmRoot
  , loadRomFile
  , fetchedRoms
  , screenGolden
  ) where

import Prelude

import Control.Exception (ErrorCall (..), throwIO)
import Control.Monad (filterM, forM)
import qualified Data.ByteString as BS
import Data.List (sort)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath ((<.>), (</>), takeExtension)
import Test.Tasty (TestTree)
import Test.Tasty.Golden.Advanced (goldenTest)

import Gb.Screen
import Platform.Sim.Frame (frameDiff, readFrame, writeFrame)

-- | Fetched test ROMs (tools/fetch-roms.sh), relative to the package
-- directory, where cabal runs the tests.
romRoot :: FilePath
romRoot = "../../roms/gb"

-- | Our own regression ROMs (make gb-asm).
asmRoot :: FilePath
asmRoot = "../../build/gb/asm"

loadRomFile :: FilePath -> IO BS.ByteString
loadRomFile p = do
  ok <- doesFileExist p
  if ok then BS.readFile p
        else throwIO (ErrorCall (p <> " missing: run make test (fetches ROMs, assembles build/gb/asm)"))

-- | Every @.gb@ file below 'romRoot', sorted; fails if there are none.
fetchedRoms :: IO [FilePath]
fetchedRoms = do
  fs <- sort <$> walk romRoot
  if null fs then throwIO (ErrorCall (romRoot <> ": no ROMs; run tools/fetch-roms.sh")) else pure fs
 where
  walk d = do
    es <- map (d </>) <$> listDirectory d
    dirs <- filterM doesDirectoryExist es
    sub <- forM dirs walk
    pure (filter ((== ".gb") . takeExtension) es <> concat sub)

-- | Golden test of a screen against @test/golden/spec/<name>.png@ (created
-- on the first run with --accept or when missing, then checked by eye and
-- frozen).
screenGolden :: String -> FilePath -> IO Screen -> TestTree
screenGolden name golden run =
  goldenTest name
    (either error id <$> readFrame path)
    (screenFrame <$> run)
    (\g t -> pure (frameDiff g t))
    (writeFrame path)
 where
  path = "test/golden/spec" </> golden <.> "png"
