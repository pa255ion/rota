-- | Debugging aid: run the 800XL reference model with AltirraOS and
-- Altirra BASIC (and optionally a cartridge) and print the last
-- instructions and the text screen.
--
--   cabal run a800-run -- <os.rom> <basic.rom> [cycles] [trace lines] [cart or .xex]
--
-- With A800_PNG=file, also writes the picture of the last whole frame.
-- A tape (.cas) boots as 'runCasBoot' (START held, then Space) for the
-- picture. An executable is loaded through the loader page (build/a800xl/xexload.bin,
-- make a800-asm).
module Main (main) where

import Prelude

import qualified Data.ByteString as BS
import System.Environment (getArgs, lookupEnv)
import A800.Picture (pictureFrame)
import Platform.Sim.Frame (writeFrame)
import System.Exit (exitFailure, exitSuccess)
import Numeric (readHex)
import System.IO (hPutStrLn, stderr)
import Numeric (showHex)
import Data.Word
import Data.Char (toLower)
import Data.List (isSuffixOf)

import A800.Media (parseCart, parseCas)
import A800.Spec

main :: IO ()
main = do
  args <- getArgs
  case args of
    os : basic : rest -> do
      o <- BS.readFile os
      b <- BS.readFile basic
      let cycles = case rest of n : _ -> read n; _ -> 3000000
          k = case rest of _ : n : _ -> read n; _ -> 40
      cfg <- case rest of
        _ : _ : f : _
          | ".cas" `isSuffixOf` map toLower f ->
              either (\e -> hPutStrLn stderr e >> exitFailure) (pure . casConfig o b) . parseCas =<< BS.readFile f
          | ".xex" `isSuffixOf` map toLower f -> do
              l <- BS.readFile "build/a800xl/xexload.bin"
              xexConfig o b l <$> BS.readFile f
          | otherwise -> either (\e -> hPutStrLn stderr e >> exitFailure) (pure . stockConfig o b . Just) . parseCart f =<< BS.readFile f
        _ -> pure (stockConfig o b Nothing)
      dbg <- lookupEnv "A800_LINES" -- y0-y1-reg (hex): per-cycle log of those lines
      case dbg of
        Just spec | (y0, '-' : r1) <- break (== '-') spec, (y1, '-' : r2) <- break (== '-') r1
                  , (reg, _) : _ <- readHex r2 -> do
          mapM_ putStrLn (debugDump cycles (read y0, read y1) reg cfg)
          exitSuccess
        _ -> pure ()
      png <- lookupEnv "A800_PNG"
      case png of
        Just f | Just _ <- cfgCas cfg -> writeFrame f (pictureFrame (runCasPicture (cycles `div` 29868) cfg))
               | otherwise -> writeFrame f (pictureFrame (runPicture (cycles `div` 29868) cfg))
        Nothing -> pure ()
      let (tr, sc, low) = traceTail k cycles cfg
      mapM_ putStrLn tr
      putStrLn "--- screen ---"
      mapM_ putStrLn sc
      putStrLn "--- RAM $0000-$03FF ---"
      mapM_ (\r -> putStrLn (hex4 (16 * r) <> ": " <> unwords [hex2 w | w <- take 16 (drop (16 * r) low)])) [0 .. 63]
    _ -> hPutStrLn stderr "usage: a800-run <os.rom> <basic.rom> [cycles] [trace lines] [cart]" >> exitFailure

hex2 :: Word8 -> String
hex2 v = let x = showHex v "" in replicate (2 - length x) '0' <> x

hex4 :: Int -> String
hex4 v = let x = showHex v "" in replicate (4 - length x) '0' <> x
