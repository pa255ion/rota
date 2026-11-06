-- | Block RAM images for a bitstream or a simulation with the software
-- built in (make hdl / make sim SYSTEM=a800xl [XEX=…]):
--
-- > cabal run a800-image -- <os.rom> <basic.rom> <xexload.bin> <dir> [file.xex | cart.car | cart.rom] [tape.cas]
--
-- writes @<dir>/os.bin@ (16 KiB), @basic.bin@ (8 KiB), @cart.bin@ (16 KiB,
-- empty: cartridges come with the PS link, step 4.8), @loader.bin@ (256
-- bytes), @xex.bin@ (64 KiB, the executable or zeros) and @cfg.bin@ (the
-- configuration, 'A800.Core.cfgWord'), one word per line in binary
-- digits, as 'blockRamFile' reads them. A tape (side track Q; alone or
-- with an executable, not with a cartridge): its image
-- ("A800.Media.casImage", no header: the reader starts at the media
-- region with the configuration built in) in cart.raw for the Arty's DDR.
module Main (main) where

import Prelude

import qualified Data.ByteString as BS
import Data.Bits (testBit)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath ((</>))

import A800.Core (CartKind (..), CoreCfg (..), cfgWord)
import A800.Media (Cart (..), CartType (..), casImage, parseCart, parseCas)
import Data.List (isSuffixOf)

main :: IO ()
main = do
  args <- getArgs
  case args of
    os : basic : loader : dir : rest | length rest <= 2 -> do
      o <- BS.readFile os
      b <- BS.readFile basic
      l <- BS.readFile loader
      -- (a tape may come with an executable: cassio.s's layer 4 run)
      let tapeF = case filter (".cas" `isSuffixOf`) rest of [t] -> Just t; _ -> Nothing
          file = case filter (not . (".cas" `isSuffixOf`)) rest of [f] -> Just f; _ -> Nothing
          isCart = maybe False (\f -> any (`isSuffixOf` f) [".car", ".rom"]) file
          isCas = maybe False (const True) tapeF
      tape <- case tapeF of
        Nothing -> pure Nothing
        Just t -> BS.readFile t >>= either (\e -> putStrLn ("a800-image: " <> e) >> exitFailure) (pure . Just) . parseCas
      bytes <- maybe (pure BS.empty) BS.readFile file
      -- a cartridge (side track Q): its image in cart.bin (block RAM, 16
      -- KiB: the ZedBoard) and cart.raw (all of it, for the Arty's DDR:
      -- make sim's --ddr-file), its kind in the configuration
      cart <- if not isCart then pure Nothing else
        either (\e -> putStrLn ("a800-image: " <> e) >> exitFailure) (pure . Just) (parseCart (maybe "" id file) bytes)
      let x = if isCart then BS.empty else bytes
      check "OS" (BS.length o == 16384)
      check "BASIC" (BS.length b == 8192)
      check "loader" (BS.length l == 256)
      check "executable (64 KiB at most)" (BS.length x <= 65536)
      image (dir </> "os.bin") 16384 o
      image (dir </> "basic.bin") 8192 b
      image (dir </> "cart.bin") 16384 (maybe BS.empty cartImage cart)
      BS.writeFile (dir </> "cart.raw") (maybe (maybe BS.empty casImage tape) cartImage cart)
      image (dir </> "loader.bin") 256 l
      image (dir </> "xex.bin") 65536 x
      let kind = case cartType <$> cart of
            Nothing -> CkNone
            Just Std8k -> Ck8
            Just Std16k -> Ck16
            Just Xegs32k -> CkXegs 0
            Just Xegs64k -> CkXegs 1
            Just Xegs128k -> CkXegs 2
          cfg = CoreCfg True (if isCas then CkCas else kind) (not (BS.null x)) (fromIntegral (BS.length x))
          w = cfgWord cfg
      writeFile (dir </> "cfg.bin") ([ if testBit w i then '1' else '0' | i <- [31, 30 .. 0] ] <> "\n")
    _ -> putStrLn "usage: a800-image <os.rom> <basic.rom> <xexload.bin> <dir> [file.xex | cart.car | cart.rom] [tape.cas]" >> exitFailure
 where
  check what ok = if ok then pure () else putStrLn ("a800-image: wrong size: " <> what) >> exitFailure
  image path size bytes =
    writeFile path (unlines [ byte (if i < BS.length bytes then BS.index bytes i else 0) | i <- [0 .. size - 1] ])
  byte v = [ if testBit v i then '1' else '0' | i <- [7, 6 .. 0] ]
