{-|
@gg-run@ and @sms-run@: run a Game Gear or Master System ROM on the
reference model ('Gg.Spec') and write what it shows and plays (sms-run:
the same, with the Master System's FM unit, "Sms.Spec.FmUnit").

> cabal run gg-run -- ROM [options]
>   --frames N        frames to run (default 300)
>   --png DIR         every frame as DIR/frame-NNNNN.png
>   --png-at K FILE   frame K only (repeatable)
>   --vga-at K FILE   frame K as the board shows it, 640 x 480 (layer 4 goldens;
>                     the Master System: x2 with the border in the backdrop colour)
>   --wav FILE        the audio, 48 kHz 16-bit stereo
>   --hashes FILE     one line per frame: number and frame hash
>   --input FILE      buttons per frame (below)
>   --movie FILE      an input movie (.mv1: the desktop's Movie menu, the
>                     board's make movie-pull): its words from power-on,
>                     frame by frame (side track Q, Q.8)
>   --commands FILE   command mode (side track Q, Q.8): no run of --frames;
>                     the machine is driven by the commands in FILE (- for
>                     standard input, one at a time: a session can step), one
>                     line of answer each ("ok ..." or "error ..."):
>                       frames N        run N frames (the script's or movie's input,
>                                       or the one 'input' set); step = frames 1
>                       png FILE        the current frame as a PNG
>                       peek ADDR [N]   N bytes (default 1) of the CPU's map, hex
>                       vram ADDR [N]   VRAM bytes; vdp: R0-R10
>                       regs            the Z80's registers, the frame number
>                       hash            the frame's hash (as --hashes)
>                       text            the name table as text (--text-at)
>                       save FILE       a save state ("Platform.Snapshot"; not with
>                       load FILE       the FM unit); loading one puts the frame back
>                       input BUTTONS   hold these from now on (letters as --input);
>                       input script    back to the script's or movie's input
>                       record FILE     record a movie from here: the state now
>                                       (not with the FM unit), then each frame's
>                                       input, until
>                       stop            writes it (ECMOVIE1, from a state: the
>                                       desktop's Movie menu and --movie play it)
>                       quit
>   --sram FILE       cartridge RAM: loaded if it exists, written at the end
>   --trace           one line per frame on stderr (frame, hash, PC)
>   --peek ADDR       print the byte at ADDR (hex) at the end (repeatable)
>   --console         print what the ROM writes to port FDh (the SDSC debug console)
>   --sms             the Master System II (M6; the default for a .sms file)
>   --gg              Game Gear mode (the default otherwise)
>   --gg-sms          a Game Gear in its Master System mode (M5's SMS mode)
>   --bios FILE       the Master System's BIOS image (your own dump; never committed)
>   --no-fm           sms-run: no FM unit (an export Master System as sold)
>   --mapper M        sega or codemasters (default: detected from the ROM)
>   --nametable       print the name table's tile numbers (32 x 24, hex) at the end
>   --text-at K       print the name table as text at frame K (tile + 20h: an ASCII font at tile 0)
>
> Input file: lines "FRAME BUTTONS"; the buttons hold from that frame
> until the next line. BUTTONS is "-" (none) or letters: U D L R (pad),
> 1 2 (buttons), S (Start; Pause on a Master System), u d l r 3 4 (the
> Master System's pad 2, 3 and 4 its buttons 1 and 2). '#' starts a comment.
>   0   -
>   120 S
>   126 -
>   200 R1
-}
module Gg.RunMain (runMain) where

import Prelude

import Control.Monad (forM_, unless, when)
import Control.Monad.ST (RealWorld, ST, stToIO)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.IORef
import qualified Data.Vector.Unboxed as U
import Data.Word
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hFlush, hPutStrLn, stderr, stdout)
import Data.Char (toLower)
import Data.Maybe (fromMaybe, isJust)
import Numeric (readHex)
import System.FilePath (takeExtension)
import Text.Printf (printf)

import Data.Bits (bit, shiftL, shiftR, testBit, (.&.), (.|.))
import Gg.Input
import Gg.Screen
import Gg.Text
import Gg.Spec
import Platform.Snapshot (Field (..), fnv1a, restore, snapshot)
import Data.STRef
import Data.List (intercalate)
import System.IO (hGetLine, hIsEOF, stdin, openFile, IOMode (ReadMode))
import Z80.Spec (Regs (..), getRegs)

data Opts = Opts
  { oRom    :: FilePath
  , oFrames :: Int
  , oPngDir :: Maybe FilePath
  , oPngAt  :: [(Int, FilePath)]
  , oWav    :: Maybe FilePath
  , oHashes :: Maybe FilePath
  , oInput  :: Maybe FilePath
  , oMovie  :: Maybe FilePath
  , oSram   :: Maybe FilePath
  , oTrace  :: Bool
  , oPeek   :: [Word16]
  , oConsole :: Bool
  , oMode   :: Maybe Mode -- ^ Nothing: from the extension
  , oBios   :: Maybe FilePath
  , oNoFm   :: Bool
  , oNames  :: Bool
  , oMapper :: Maybe Mapper
  , oTextAt :: [Int]
  , oVgaAt  :: [(Int, FilePath)]
  , oCommands :: Maybe FilePath
  }

parse :: [String] -> Either String Opts
parse (rom : rest) = go rest Opts
  { oRom = rom, oFrames = 300, oPngDir = Nothing, oPngAt = [], oWav = Nothing
  , oHashes = Nothing, oInput = Nothing, oMovie = Nothing, oSram = Nothing, oTrace = False
  , oPeek = [], oConsole = False, oMode = Nothing, oNames = False
  , oMapper = Nothing, oTextAt = [], oVgaAt = [], oBios = Nothing, oNoFm = False, oCommands = Nothing }
 where
  go [] o = Right o
  go ("--frames" : n : xs) o = go xs o { oFrames = read n }
  go ("--png" : d : xs) o = go xs o { oPngDir = Just d }
  go ("--png-at" : k : f : xs) o = go xs o { oPngAt = (read k, f) : oPngAt o }
  go ("--vga-at" : k : f : xs) o = go xs o { oVgaAt = (read k, f) : oVgaAt o }
  go ("--wav" : f : xs) o = go xs o { oWav = Just f }
  go ("--hashes" : f : xs) o = go xs o { oHashes = Just f }
  go ("--input" : f : xs) o = go xs o { oInput = Just f }
  go ("--movie" : f : xs) o = go xs o { oMovie = Just f }
  go ("--commands" : f : xs) o = go xs o { oCommands = Just f }
  go ("--sram" : f : xs) o = go xs o { oSram = Just f }
  go ("--trace" : xs) o = go xs o { oTrace = True }
  go ("--console" : xs) o = go xs o { oConsole = True }
  go ("--sms" : xs) o = go xs o { oMode = Just ModeSms }
  go ("--gg" : xs) o = go xs o { oMode = Just ModeGg }
  go ("--gg-sms" : xs) o = go xs o { oMode = Just ModeGgSms }
  go ("--bios" : f : xs) o = go xs o { oBios = Just f }
  go ("--no-fm" : xs) o = go xs o { oNoFm = True }
  go ("--nametable" : xs) o = go xs o { oNames = True }
  go ("--mapper" : "sega" : xs) o = go xs o { oMapper = Just SegaMapper }
  go ("--mapper" : "codemasters" : xs) o = go xs o { oMapper = Just Codemasters }
  go ("--mapper" : x : _) _ = Left ("--mapper: sega or codemasters, not " <> x)
  go ("--text-at" : k : xs) o = go xs o { oTextAt = read k : oTextAt o }
  go ("--peek" : a : xs) o = case readHex a of
    [(v, "")] -> go xs o { oPeek = oPeek o <> [v] }
    _ -> Left ("--peek: not a hex address: " <> a)
  go (x : _) _ = Left ("unknown option " <> x)
parse [] = Left "usage: gg-run ROM [--frames N] [--png DIR] [--png-at K FILE] [--wav FILE] [--hashes FILE] [--input FILE] [--sram FILE] [--trace] [--peek ADDR] [--console] [--sms|--gg|--gg-sms] [--bios FILE] [--mapper sega|codemasters]"

-- | Which machine: the Master System II, a Game Gear, or a Game Gear in
-- its Master System mode.
data Mode = ModeSms | ModeGg | ModeGgSms
  deriving stock Eq

-- | An input movie (platform/ps/movie.zig's format, "ECMOVIE1": a
-- 512-byte header, the words from 'words_at', two bytes each, frame 0
-- first) as the input script: each frame's word as the desktop's library
-- takes it (sms_frame: bits 0-5 Right, Left, Up, Down, button 2, button 1;
-- bit 7 Start; bits 8-13 the Master System's pad 2). A Game Gear's (7) or
-- Master System's (8), from power-on or from a save state (the format's
-- start = 1: the state area holds the desktop's state, the reference
-- model's layout, 'movieFields'), returned beside the input (empty: power-on).
readMovie :: FilePath -> IO ([(Int, Input)], BS.ByteString)
readMovie f = do
  b <- BS.readFile f
  let u32 i = foldr (\k a -> a * 256 + fromIntegral (BS.index b (i + k))) 0 [0 .. 3] :: Int
      u16 i = fromIntegral (BS.index b i) + 256 * fromIntegral (BS.index b (i + 1)) :: Int
      bad e = hPutStrLn stderr (f <> ": " <> e) >> exitFailure
  when (BS.length b < 512 || BS.take 8 b /= BS.pack (map (fromIntegral . fromEnum) "ECMOVIE1")) (bad "not an input movie")
  when (BS.index b 8 /= 7 && BS.index b 8 /= 8) (bad "not a Game Gear's or Master System's movie")
  when (BS.index b 9 /= 1 || BS.index b 10 > 1) (bad "another version of the format")
  when (BS.index b 11 /= 1) (bad "an empty movie (its recording was cut off)")
  let frames = u32 12
      stateLen = u32 16
      at = u32 20
  when (BS.length b < at + 2 * frames || at < 512 + stateLen) (bad "cut short")
  let pad x = (if testBit x 0 then padRight else 0) .|. (if testBit x 1 then padLeft else 0)
            .|. (if testBit x 2 then padUp else 0) .|. (if testBit x 3 then padDown else 0)
            .|. (if testBit x 4 then pad2 else 0) .|. (if testBit x 5 then pad1 else 0)
      inputOf w = Input (pad w) (testBit w 7) (pad (w `shiftR` 8 :: Int))
  pure ( [ (k, inputOf (u16 (at + 2 * k))) | k <- [0 .. frames - 1] ]
       , if BS.index b 10 == 1 then BS.take stateLen (BS.drop 512 b) else BS.empty )

-- | A frame's input as a movie's word ('readMovie''s inverse).
movieWord :: Input -> Word16
movieWord (Input p s q) = enc p .|. (if s then 0x80 else 0) .|. (enc q `shiftL` 8)
 where
  enc x = foldr (.|.) 0 [ bit i | (i, b) <- zip [0 ..] [padRight, padLeft, padUp, padDown, pad2, pad1], x .&. b /= 0 ]

-- | A movie from a state (platform/ps/movie.zig's 'encodeFile'): the header,
-- the state in whole 512-byte blocks, the words. System 8 the Master
-- System's, 7 otherwise; the game named by its size, as the desktop's.
movieFile :: Word8 -> Int -> BS.ByteString -> [Word16] -> BS.ByteString
movieFile system romSize state ws = BS.concat [header, state, BS.replicate (area - BS.length state) 0, BS.pack (concatMap le16 ws)]
 where
  area = (BS.length state + 511) `div` 512 * 512
  le16 w = [fromIntegral w, fromIntegral (w `shiftR` 8)]
  le32 :: Int -> [Word8]
  le32 v = [ fromIntegral (v `shiftR` (8 * k)) | k <- [0 .. 3] ]
  start = if BS.null state then 0 else 1
  fields = map (fromIntegral . fromEnum) "ECMOVIE1" <> [system, 1, start, 1]
        <> le32 (length ws) <> le32 (BS.length state) <> le32 (512 + area)
  header = BS.pack (take 512 (fields <> replicate 8 0 <> replicate 16 0 <> [0, 0] <> le32 romSize <> repeat 0))

-- | A movie's state's fields: the desktop's ("Sms.Desktop": the ROM alone
-- names the game, no frame number), so its movies' states load here.
movieFields :: BS.ByteString -> Machine s -> [Field s]
movieFields rom m = FCheck (fnv1a rom) : machineFields m

readInput :: FilePath -> IO [(Int, Input)]
readInput f = do
  src <- readFile f
  either (\e -> hPutStrLn stderr (f <> ":" <> e) >> exitFailure) pure (parseInput src)

-- | The simulator's main (M6: a library module, so that the sms package's
-- @sms-run@ can give it the Master System's FM unit): @fm@ makes the FM
-- unit for @--sms@, unless @--no-fm@ ('Nothing': gg-run, no FM unit).
runMain :: Maybe (ST RealWorld (FmUnit RealWorld)) -> IO ()
runMain fmNew = do
  args <- getArgs
  o <- either (\e -> hPutStrLn stderr e >> exitFailure) pure (parse args)
  rom <- BS.readFile (oRom o)
  script0 <- maybe (pure []) readInput (oInput o)
  (script, mvState) <- maybe (pure (script0, BS.empty)) readMovie (oMovie o)
  let mode = fromMaybe (if map toLower (takeExtension (oRom o)) == ".sms" then ModeSms else ModeGg) (oMode o)
  let mapper = fromMaybe (detectMapper rom) (oMapper o)
  bios <- traverse BS.readFile (oBios o)
  when (mode /= ModeSms && bios /= Nothing) $ hPutStrLn stderr "gg-run: --bios is for the Master System (--sms)" >> exitFailure
  fm <- if oNoFm o then pure Nothing else traverse stToIO fmNew
  m <- stToIO (case mode of
         ModeSms -> newMasterSystemFm bios fm mapper rom
         ModeGg -> newMachineWith False mapper rom
         ModeGgSms -> newMachineWith True mapper rom) :: IO (Machine RealWorld)
  when (oTrace o) $ hPutStrLn stderr ((case mode of
    ModeSms -> "Master System II" <> maybe "" (const ", BIOS") bios <> maybe "" (const ", FM unit") fm
    ModeGg -> "Game Gear"
    ModeGgSms -> "Game Gear, Master System mode") <> ", " <> show mapper)
  forM_ (oSram o) $ \f -> do
    ok <- doesFileExist f
    when ok (BS.readFile f >>= stToIO . loadCartRam m)
  unless (BS.null mvState) $ do
    -- a movie from a state: the machine as at its start
    when (mode == ModeSms && isJust fm) $ hPutStrLn stderr "gg-run: a movie from a state needs --no-fm (no states with the FM unit)" >> exitFailure
    r <- stToIO (restore (movieFields rom m) mvState)
    either (\e -> hPutStrLn stderr ("gg-run: the movie's state: " <> e) >> exitFailure) pure r
  forM_ (oPngDir o) (createDirectoryIfMissing True)
  forM_ (oCommands o) $ \f -> commandMode f o rom (if mode == ModeSms then 8 else 7) (mode == ModeSms && isJust fm) script m >> exitSuccess
  audio <- newIORef []
  hashes <- newIORef []
  forM_ [0 .. oFrames o - 1] $ \k -> do
    stToIO (setInput m (inputAt script k))
    stToIO (runFrame m)
    fb <- stToIO (frameBuffer m)
    a <- stToIO (takeAudio m)
    c <- stToIO (takeConsole m)
    when (oConsole o && not (null c)) $ putStr (map (toEnum . fromIntegral) c) >> hFlush stdout
    modifyIORef' audio (reverse a <>)
    let h = frameHash fb
    modifyIORef' hashes ((k, h) :)
    forM_ (oPngDir o) $ \d -> BL.writeFile (printf "%s/frame-%05d.png" d k) (framePng (frameWidth m) fb)
    forM_ [ f | (kk, f) <- oPngAt o, kk == k ] $ \f -> BL.writeFile f (framePng (frameWidth m) fb)
    border <- stToIO (frameBorder m)
    forM_ [ f | (kk, f) <- oVgaAt o, kk == k ] $ \f -> BL.writeFile f $
      if mode == ModeSms then frameSmsVgaPng border fb else frameVgaPng fb
    when (k `elem` oTextAt o) $ do
      rows <- stToIO (screenText m)
      printf "--- frame %d\n" k
      mapM_ putStrLn rows
    when (oTrace o) $ do
      r <- stToIO (getRegs (mCpu m))
      hPutStrLn stderr (printf "frame %5d  hash %016x  PC %04X" k h (rPC r))
  forM_ (oWav o) $ \f -> readIORef audio >>= BL.writeFile f . wavFile audioRate . reverse
  forM_ (oHashes o) $ \f -> do
    hs <- reverse <$> readIORef hashes
    writeFile f (unlines [ printf "%d %016x" k h | (k, h) <- hs ])
  when (oNames o) $ do
    rows <- stToIO (nameTable m)
    forM_ rows $ \ts -> putStrLn (unwords (map (printf "%03x") ts))
  forM_ (oPeek o) $ \a -> do
    v <- stToIO (peek m a)
    printf "%04X: %02X\n" a v
  forM_ (oSram o) $ \f -> do
    ram <- stToIO (cartRam m)
    unless (U.all (== 0) ram) (BS.writeFile f (BS.pack (U.toList ram :: [Word8])))


-- | gg-run --commands (side track Q, Q.8): the machine driven by commands,
-- one answer line each (see the module's header). @fm@: an FM unit is
-- fitted (no save states then: its state is behind its calls).
commandMode :: FilePath -> Opts -> BS.ByteString -> Word8 -> Bool -> [(Int, Input)] -> Machine RealWorld -> IO ()
commandMode file o rom system fm script m = do
  h <- if file == "-" then pure stdin else openFile file ReadMode
  frameRef <- stToIO (newSTRef (0 :: Int))
  override <- newIORef (Nothing :: Maybe Input)
  -- a recording: its file, its state, its words so far (newest first)
  recording <- newIORef (Nothing :: Maybe (FilePath, BS.ByteString, [Word16]))
  let game = fnv1a (BS.cons (if fm then 1 else 0) rom)
      fields = FCheck game : FRef frameRef : machineFields m
      say = putStrLn
      runFrames n = forM_ [1 .. n] $ \_ -> do
        k <- stToIO (readSTRef frameRef)
        ov <- readIORef override
        let i = fromMaybe (inputAt script k) ov
        modifyIORef' recording (fmap (\(f, st, ws) -> (f, st, movieWord i : ws)))
        stToIO (setInput m i)
        stToIO (runFrame m)
        _ <- stToIO (takeAudio m)
        c <- stToIO (takeConsole m)
        when (oConsole o && not (null c)) $ putStr (map (toEnum . fromIntegral) c) >> hFlush stdout
        stToIO (writeSTRef frameRef (k + 1))
      hex4 :: String -> Maybe Int
      hex4 t = case readHex t of [(v, "")] -> Just v; _ -> Nothing
      bytes rd a n = do
        vs <- mapM (\i -> stToIO (rd (a + i))) [0 .. n - 1]
        pure (unwords [ printf "%02X" v | v <- vs ])
      loop = do
        eof <- hIsEOF h
        unless eof $ do
          line <- hGetLine h
          continue <- command (words (takeWhile (/= '#') line))
          hFlush stdout
          when continue loop
      command ws = case ws of
        [] -> pure True
        ["quit"] -> say "ok" >> pure False
        ["step"] -> command ["frames", "1"]
        ["frames", n] | [(k, "")] <- reads n -> do
          runFrames (k :: Int)
          f <- stToIO (readSTRef frameRef)
          say ("ok frame " <> show f) >> pure True
        ["png", f] -> do
          fb <- stToIO (frameBuffer m)
          BL.writeFile f (framePng (frameWidth m) fb)
          say ("ok " <> f) >> pure True
        "peek" : a : rest | Just ad <- hex4 a -> do
          let n = case rest of [x] | [(v, "")] <- reads x -> v; _ -> 1
          b <- bytes (\i -> peek m (fromIntegral i)) ad n
          say (printf "ok %04X: %s" ad b) >> pure True
        "vram" : a : rest | Just ad <- hex4 a -> do
          let n = case rest of [x] | [(v, "")] <- reads x -> v; _ -> 1
          b <- bytes (vramByte m) ad n
          say (printf "ok %04X: %s" ad b) >> pure True
        ["vdp"] -> do
          rs <- mapM (stToIO . vdpRegister m) [0 .. 10]
          say ("ok " <> unwords [ printf "R%d=%02X" i v | (i, v) <- zip [0 :: Int ..] rs ]) >> pure True
        ["regs"] -> do
          r <- stToIO (getRegs (mCpu m))
          f <- stToIO (readSTRef frameRef)
          say (printf "ok frame %d  A=%02X F=%02X BC=%02X%02X DE=%02X%02X HL=%02X%02X IX=%04X IY=%04X SP=%04X PC=%04X I=%02X R=%02X IFF1=%s IM=%d halted=%s"
                 f (rA r) (rF r) (rB r) (rC r) (rD r) (rE r) (rH r) (rL r) (rIX r) (rIY r) (rSP r) (rPC r) (rI r) (rR r) (show (rIFF1 r)) (rIM r) (show (rHalted r))) >> pure True
        ["hash"] -> do
          fb <- stToIO (frameBuffer m)
          say (printf "ok %016x" (frameHash fb)) >> pure True
        ["text"] -> do
          rows <- stToIO (screenText m)
          say ("ok " <> intercalate "\\n" rows) >> pure True
        ["save", f]
          | fm -> say "error no save states with the FM unit (--no-fm)" >> pure True
          | otherwise -> do
              b <- stToIO (snapshot fields)
              BS.writeFile f b
              say ("ok " <> f) >> pure True
        ["load", f]
          | fm -> say "error no save states with the FM unit (--no-fm)" >> pure True
          | otherwise -> do
              b <- BS.readFile f
              r <- stToIO (restore fields b)
              k <- stToIO (readSTRef frameRef)
              say (either ("error " <>) (const ("ok frame " <> show k)) r) >> pure True
        ["record", f]
          | fm -> say "error no save states with the FM unit (--no-fm)" >> pure True
          | otherwise -> do
              st <- stToIO (snapshot (movieFields rom m))
              writeIORef recording (Just (f, st, []))
              say ("ok recording " <> f) >> pure True
        ["stop"] -> readIORef recording >>= \rec -> case rec of
          Nothing -> say "error not recording" >> pure True
          Just (f, st, ws) -> do
            BS.writeFile f (movieFile system (BS.length rom) st (reverse ws))
            writeIORef recording Nothing
            say ("ok " <> f <> " " <> show (length ws) <> " frames") >> pure True
        ["input", "script"] -> writeIORef override Nothing >> say "ok" >> pure True
        ["input", bs] -> case parseInput ("0 " <> bs) of
          Right [(_, i)] -> writeIORef override (Just i) >> say "ok" >> pure True
          _ -> say ("error buttons: " <> bs) >> pure True
        _ -> say ("error unknown command: " <> unwords ws) >> pure True
  loop
