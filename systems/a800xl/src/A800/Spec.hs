{-|
Reference model of the NTSC Atari 800XL (M4, steps 4.1 and on): the
shared 6502 ("Mos6502.Spec", NMOS with decimal mode) in the machine of
"A800.Spec.Machine".

Two interfaces, as the other systems:

* __'ST'__: "A800.Spec.Machine" for tests that look inside, and for the
  lockstep tests of steps 4.5 and 4.6.
* __Pure__: 'runScreen' runs until the OS's text screen shows something
  (or a cycle limit) and returns the screen; 'traceTail' is the
  debugging aid.
-}
module A800.Spec
  ( -- * Configuration
    Config (..)
  , XexBoot (..)
  , stockConfig
  , xexConfig
  , casConfig
    -- * Running
  , ScreenRun (..)
  , runScreen
  , runCycles
  , runVerdict
  , runCasBoot
  , runTapeBoot
  , runCasPicture
  , runPicture
  , runPictureKeys
  , runTone
  , runTyping
    -- * Debugging
  , traceTail
  , debugDump
  ) where

import Prelude

import Control.Monad.ST (ST, runST)
import qualified Data.ByteString as BS
import Data.Foldable (toList)
import qualified Data.Sequence as Seq
import Data.Word
import Data.Bits (bit)
import Data.List (isInfixOf)
import Data.STRef
import Numeric (showHex)

import A800.Acid800 (Verdict (..), verdict)
import A800.Keyboard (charKeys)
import A800.Media (Cart, Cas, TapeRec)
import A800.Picture
import A800.Spec.Antic (anticFrames)
import A800.Spec.Gtia (gtiaFrame)
import A800.Spec.Pokey (pokeyLevel)
import qualified Data.Vector.Unboxed as U
import A800.Screen
import A800.Spec.Machine
import Mos6502.Spec (Regs (..), getRegs)

-- | An 800XL with this OS and BASIC (the built-in BASIC is on unless
-- OPTION is held at power-on) and optionally a cartridge.
stockConfig :: BS.ByteString -> BS.ByteString -> Maybe Cart -> Config
stockConfig os basic cart = Config os (Just basic) cart Nothing Nothing

-- | The same with an executable loaded through the loader page (no
-- cartridge).
xexConfig :: BS.ByteString -> BS.ByteString -> BS.ByteString -> BS.ByteString -> Config
xexConfig os basic loader file = Config os (Just basic) Nothing (Just (XexBoot loader file)) Nothing

-- | A cassette in the recorder (side track Q): the OS boots it when
-- START is held at power-on, then a key pressed ('A800.Spec.Machine.casRead').
casConfig :: BS.ByteString -> BS.ByteString -> Cas -> Config
casConfig os basic cas = Config os (Just basic) Nothing Nothing (Just cas)

data ScreenRun = ScreenRun
  { srLines  :: [String] -- ^ the screen at the end ('screenLines')
  , srFlat   :: String   -- ^ its text between the margins ('screenFlat')
  , srCycles :: Int      -- ^ machine cycles run
  , srDone   :: Bool     -- ^ the condition held (else the limit was hit)
  } deriving stock (Show, Eq)

-- | Run until the screen's text ('screenFlat') satisfies the condition
-- (checked every 10000 cycles), or for at most @limit@ machine cycles.
runScreen :: (String -> Bool) -> Int -> Config -> ScreenRun
runScreen done limit cfg = runST $ do
  m <- newMachine cfg
  let go = do
        runFor m 10000
        (ls, flat) <- screenOf m
        n <- machineCycles m
        if done flat then pure (ScreenRun ls flat n True)
          else if n >= limit then pure (ScreenRun ls flat n False)
          else go
  go

-- | Run an Acid800 test (or one of our programs, which print the same
-- words) until it shows a verdict, or for at most @limit@ cycles.
runVerdict :: Int -> Config -> (Verdict, ScreenRun)
runVerdict limit cfg =
  let r = runScreen ((/= Running) . verdict) limit cfg
  in (verdict (srFlat r), r)

-- | 'runVerdict' for a cassette ('casConfig'), booted as a user does it:
-- START held from power-on for a second (the OS's cassette boot), then,
-- when the OS waits for a key (its beep), Space for a few frames.
runCasBoot :: Int -> Config -> (Verdict, ScreenRun)
runCasBoot limit cfg = runST $ do
  m <- newMachine cfg
  let go = do
        f <- anticFrames (mAntic m)
        setConsole m (if f < 60 then 1 else 0)
        setKeys m (if f >= 120 && f < 126 then bit 0x21 else 0) 0
        runFor m 5000
        (ls, flat) <- screenOf m
        n <- machineCycles m
        let v = verdict flat
        if v /= Running then pure (v, ScreenRun ls flat n True)
          else if n >= limit then pure (v, ScreenRun ls flat n False)
          else go
  go

-- | 'runCasBoot' with the tape played by the 410 recorder itself (depth 2,
-- 'setTape'; the configuration without 'cfgCas'): every byte through
-- POKEY's serial input at the tape's speed.
runTapeBoot :: Int -> Config -> [TapeRec] -> (Verdict, ScreenRun)
runTapeBoot limit cfg tape = runST $ do
  m <- newMachine cfg
  setTape m (Just tape)
  let go = do
        f <- anticFrames (mAntic m)
        setConsole m (if f < 60 then 1 else 0)
        setKeys m (if f >= 120 && f < 126 then bit 0x21 else 0) 0
        runFor m 5000
        (ls, flat) <- screenOf m
        n <- machineCycles m
        let v = verdict flat
        if v /= Running then pure (v, ScreenRun ls flat n True)
          else if n >= limit then pure (v, ScreenRun ls flat n False)
          else go
  go

-- | Run until @n@ frames are complete and return the last one's picture.
runPicture :: Int -> Config -> Picture
runPicture n cfg = runST $ do
  m <- newMachine cfg
  let go = do
        _ <- stepCpu m
        f <- anticFrames (mAntic m)
        if f < n then go else pure ()
  go
  cropPicture . U.fromList <$> gtiaFrame (mGtia m)

-- | 'runPicture' booting a tape as 'runCasBoot' does (START held for the
-- first 60 frames, then Space pressed at frames 120 to 125).
runCasPicture :: Int -> Config -> Picture
runCasPicture n cfg = runST $ do
  m <- newMachine cfg
  let go = do
        _ <- stepCpu m
        f <- anticFrames (mAntic m)
        setConsole m (if f < 60 then 1 else 0)
        setKeys m (if f >= 120 && f < 126 then bit 0x21 else 0) 0
        if f < n then go else pure ()
  go
  cropPicture . U.fromList <$> gtiaFrame (mGtia m)

-- | 'runPicture' with keys held (key bits, modifiers; "A800.Keyboard")
-- from frame @from@ on.
runPictureKeys :: Int -> (Word64, Word8) -> Int -> Config -> Picture
runPictureKeys from (ks, ms) n cfg = runST $ do
  m <- newMachine cfg
  let go pressed = do
        _ <- stepCpu m
        f <- anticFrames (mAntic m)
        p <- if not pressed && f >= from then setKeys m ks ms >> pure True else pure pressed
        if f < n then go p else pure ()
  go False
  cropPicture . U.fromList <$> gtiaFrame (mGtia m)

-- | Run @skip@ machine cycles, then measure POKEY's output for @n@ more:
-- the frequency of its rising edges (sampled at each CPU step), in Hz.
runTone :: Int -> Int -> Config -> Double
runTone skip n cfg = runST $ do
  m <- newMachine cfg
  let go prev edges = do
        _ <- stepCpu m
        t <- machineCycles m
        l <- pokeyLevel (mPokey m)
        let edges' = if t > skip && l > prev then t : edges else edges
        if t >= skip + n then pure (reverse edges') else go l edges'
  es <- go 0 []
  pure $ case es of
    a : _ : _ -> let b = last es
                 in 1789772.5 * fromIntegral (length es - 1) / fromIntegral (b - a)
    _ -> 0

-- | Run until the screen shows @ready@, type @text@ (each key held for 3
-- frames, released for 3), run one more second, and return the screen.
runTyping :: String -> String -> Config -> Either String ScreenRun
runTyping ready text cfg = runST $ do
  m <- newMachine cfg
  let frame = 29868
      untilReady = do
        runFor m 10000
        (_, flat) <- screenOf m
        n <- machineCycles m
        if ready `isInfixOf` flat then pure True else if n > 30000000 then pure False else untilReady
  ok <- untilReady
  if not ok then pure (Left ("no " <> show ready)) else do
    let typeKey c = case charKeys c of
          Nothing -> pure False
          Just (k, sh) -> do
            setKeys m (bit (fromIntegral k)) (if sh then 1 else 0)
            runFor m (3 * frame)
            setKeys m 0 0
            runFor m (3 * frame)
            pure True
    oks <- mapM typeKey text
    if not (and oks) then pure (Left "a character with no key") else do
      runFor m (60 * frame)
      (ls, flat) <- screenOf m
      n <- machineCycles m
      pure (Right (ScreenRun ls flat n True))

-- | Run for (at least) this many machine cycles; the screen then.
runCycles :: Int -> Config -> ScreenRun
runCycles n = runScreen (const False) n

-- | The screen, reading only what the hooks need (SAVMSC, the margins,
-- the 960 bytes of the text screen).
screenOf :: Machine s -> ST s ([String], String)
screenOf m = do
  lo <- peekRam m savmsc
  hi <- peekRam m (savmsc + 1)
  lm <- peekRam m 0x52
  rm <- peekRam m 0x53
  let base = fromIntegral lo + 256 * fromIntegral hi :: Word16
  txt <- BS.pack <$> mapM (\i -> peekRam m (base + i)) [0 .. 959]
  let rd a | a == savmsc = lo
           | a == savmsc + 1 = hi
           | a == 0x52 = lm
           | a == 0x53 = rm
           | a - base < 960 = BS.index txt (fromIntegral (a - base))
           | otherwise = 0
  pure (screenLines rd, screenFlat rd)

runFor :: Machine s -> Int -> ST s ()
runFor m k = do
  start <- machineCycles m
  let go = do
        _ <- stepCpu m
        n <- machineCycles m
        if n - start < k then go else pure ()
  go

-- | Debugging aid: run for @limit@ machine cycles and return the last
-- @k@ instructions (PC, opcode, registers before it, machine cycle, the
-- step), the screen and RAM $0000-$03FF.
traceTail :: Int -> Int -> Config -> ([String], [String], [Word8])
traceTail k limit cfg = runST $ do
  m <- newMachine cfg
  let go acc = do
        r <- getRegs (mCpu m)
        n <- machineCycles m
        op <- peekCpu m (rPC r)
        s <- stepCpu m
        let line = hex4 (rPC r) <> " " <> hex2 op <> "  A:" <> hex2 (rA r) <> " X:" <> hex2 (rX r)
                   <> " Y:" <> hex2 (rY r) <> " P:" <> hex2 (rP r) <> " S:" <> hex2 (rS r)
                   <> " CYC:" <> show n <> " " <> show s
            acc' = Seq.drop (Seq.length acc + 1 - k) (acc Seq.|> line)
        if n >= limit then pure (toList acc') else go $! acc'
  t <- go Seq.empty
  (sc, _) <- screenOf m
  low <- mapM (peekRam m) [0 .. 0x3FF]
  pure (t, sc, low)
 where
  hex2 :: Word8 -> String
  hex2 v = let x = showHex v "" in replicate (2 - length x) '0' <> x
  hex4 :: Word16 -> String
  hex4 v = let x = showHex v "" in replicate (4 - length x) '0' <> x

-- | Debugging aid: run for @limit@ cycles logging every cycle of lines
-- @y0@-@y1@; return the log of the last frame in which the CPU wrote
-- @reg@ on those lines (or the last frame's if none did).
debugDump :: Int -> (Int, Int) -> Word16 -> Config -> [String]
debugDump limit (y0, y1) reg cfg = runST $ do
  m <- newMachine cfg
  debugLines m (Just (y0, y1))
  let marker = " " <> hexW reg <> " "
      go best = do
        f0 <- anticFrames (mAntic m)
        let frame = do
              _ <- stepCpu m
              f <- anticFrames (mAntic m)
              n <- machineCycles m
              if f == f0 && n < limit then frame else pure n
        n <- frame
        (_, logRev) <- readSTRef (mDebug m)
        let l = reverse logRev
            hit = any (\s -> "CPU W" `isInfixOf` s && marker `isInfixOf` s) l
            best' = if hit || null best then l else best
        writeSTRef (mDebug m) (Just (y0, y1), [])
        if n >= limit then pure best' else go best'
  go []
 where
  hexW v = let t = showHex v "" in replicate (4 - length t) '0' <> t
