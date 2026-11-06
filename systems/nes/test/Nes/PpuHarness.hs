{-|
Lockstep of the Clash PPU ("Nes.Ppu", as plain Haskell) with the
reference model's PPU, dot by dot (PLAN.md step 3.5).

The spec machine runs as usual; its PPU hook reports every dot and every
register access, and the Clash PPU gets the same tick. Its memory reads
are answered from the spec machine's PPU bus at the moment it asks
(port A for fetches, port B for $2007); its $2007 writes must find the
spec's memory already holding the same byte; it keeps its own OAM. After
every register access and every 4th dot the two states are compared
('PpuSnap'), with the Clash PPU's pending fetch put away first; on
every tick register reads must return the same value, the NMI outputs
must agree, and every pixel must match the spec's frame.
-}
module Nes.PpuHarness
  ( lockstepRom
  , lockstepRomWith
  , lockstepFile
  , TickFn
  ) where

import Prelude

import Control.Monad (forM_, when)
import Data.Bits (testBit)
import Control.Monad.ST (ST, runST)
import qualified Data.ByteString as BS
import Data.STRef
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MV
import Data.Word

import qualified Clash.Prelude as C
import Nes.Ppu
import Nes.Spec.Machine
import Nes.Spec.Ppu (PpuSnap (..), ppuFramesDone, ppuPosition, ppuNmi, ppuOamAt, ppuPixelAt, ppuSnap, ppuSnapVec)

data H s = H
  { hPpu  :: STRef s Ppu
  , hA    :: STRef s Word8              -- ^ port A data held
  , hB    :: STRef s Word8              -- ^ port B data held
  , hOam  :: MV.MVector s Word8
  , hErr  :: STRef s (Maybe String)
  , hTick :: STRef s Int
  , hBus  :: STRef s (Maybe (Word16, Word8)) -- ^ the spec's last PPU bus write
  }

type TickFn = Ppu -> Tick -> PpuIn -> (Ppu, PpuOut)

-- | Run the ROM for @frames@ frames with the Clash PPU in lockstep;
-- 'Left' with the first difference.
lockstepRom :: Int -> BS.ByteString -> Either String Int
lockstepRom = lockstepRomWith ppuTick

-- | The same with another tick function (deliberately broken PPUs).
lockstepRomWith :: TickFn -> Int -> BS.ByteString -> Either String Int
lockstepRomWith tickFn frames rom = runST $ do
  em <- newMachine rom
  case em of
    Left e -> pure (Left ("ROM: " <> e))
    Right m -> do
      h <- H <$> newSTRef ppuInit <*> newSTRef 0 <*> newSTRef 0 <*> MV.replicate 256 0
             <*> newSTRef Nothing <*> newSTRef 0 <*> newSTRef Nothing
      -- newMachine already ran the reset sequence (no PPU register access):
      -- catch the Clash PPU up by plain dots first.
      (sl, d) <- ppuPosition (mPpu m)
      let caughtUp = iterate (\q -> fst (ppuTick q TDot (PpuIn 0 0 0))) ppuInit !! (sl * 341 + d)
      writeSTRef (hPpu h) caughtUp
      setPpuHook m (Just (onEvent tickFn m h))
      let go = do
            err <- readSTRef (hErr h)
            n <- readSTRef (hTick h)
            fr <- ppuFramesDone (mPpu m)
            case err of
              Just e -> pure (Left e)
              Nothing | fr >= frames -> pure (Right n)
                      | otherwise -> stepCpu m >> go
      go

-- | The same, for a file.
lockstepFile :: Int -> FilePath -> IO (Either String Int)
lockstepFile frames p = lockstepRom frames <$> BS.readFile p

onEvent :: TickFn -> Machine s -> H s -> PpuEvent -> ST s ()
onEvent tickFn m h ev = do
  err <- readSTRef (hErr h)
  case (err, ev) of
    (Just _, _) -> pure ()
    (_, PeBusWrite a v) -> writeSTRef (hBus h) (Just (a, v))
    _ -> do
      p <- readSTRef (hPpu h)
      a <- readSTRef (hA h)
      b <- readSTRef (hB h)
      o <- MV.read (hOam h) (fromIntegral (pOamAddr p))
      let inp = PpuIn (fromIntegral a) (fromIntegral b) (fromIntegral o)
          tick = case ev of
            PeDot -> TDot
            PeRead r _ -> TRead (fromIntegral r)
            PeWrite r v -> TWrite (fromIntegral r) (fromIntegral v)

          (p', out) = tickFn p tick inp
      n <- readSTRef (hTick h)
      writeSTRef (hTick h) (n + 1)
      writeSTRef (hPpu h) p'
      let bad msg = writeSTRef (hErr h) (Just ("tick " <> show n <> " (" <> show ev <> ", at line "
                                                <> show (pLine p') <> " dot " <> show (pDot p') <> "): " <> msg))
      -- Memory requests
      forM_ (poVramA out) $ \addr -> ppuBusPeek m (fromIntegral addr) >>= writeSTRef (hA h)
      specBus <- readSTRef (hBus h)
      writeSTRef (hBus h) Nothing
      let clashBus = case poVramB out of
            Just (True, addr, d) -> Just (fromIntegral addr, fromIntegral d)
            _ -> Nothing
      when (specBus /= clashBus) (bad ("PPU bus write: spec " <> show specBus <> ", Clash " <> show clashBus))
      forM_ (poVramB out) $ \(w, addr, _) ->
        when (not w) (ppuBusPeek m (fromIntegral addr) >>= writeSTRef (hB h))
      forM_ (poOamWrite out) $ \(addr, d) -> MV.write (hOam h) (fromIntegral addr) (fromIntegral d)
      forM_ (poOamRow out) $ \(row, bs) -> forM_ (zip [0 ..] (C.toList bs)) $ \(j, d) ->
        MV.write (hOam h) (fromIntegral row * 8 + j) (fromIntegral d)
      -- Reads
      case ev of
        PeRead r v | fromIntegral (poRead out) /= v ->
          bad ("read of register " <> show r <> ": Clash " <> show (poRead out) <> ", spec " <> show v)
        _ -> pure ()
      -- State
      a' <- readSTRef (hA h)
      b' <- readSTRef (hB h)
      let settled0 = ppuSettle (fromIntegral a') p'
          settled1 = if pBufPend settled0 then settled0 { pBuf = fromIntegral b', pBufPend = False } else settled0
          settled = if pBufFromA settled1 then settled1 { pBuf = fromIntegral a', pBufFromA = False } else settled1
      -- The whole state on every register access and every 4th dot (pixels,
      -- reads, NMI and bus writes on every tick).
      when (ev /= PeDot || n `mod` 4 == 0) $ do
        specV <- ppuSnapVec (mPpu m)
        when (specV /= clashSnapVec settled) $ do
          spec <- ppuSnap (mPpu m)
          bad (describe spec (clashSnap settled))
      nmiS <- ppuNmi (mPpu m)
      when (nmiS /= poNmi out) (bad ("NMI: spec " <> show nmiS <> ", Clash " <> show (poNmi out)))
      forM_ (poPixel out) $ \(x, y, c) -> do
        s <- ppuPixelAt (mPpu m) (fromIntegral x) (fromIntegral y)
        when (s /= fromIntegral c) (bad ("pixel " <> show (x, y) <> ": spec " <> show s <> ", Clash " <> show c))
      -- OAM, once per frame
      when (poFrame out) $ forM_ [0 .. 255] $ \i -> do
        s <- ppuOamAt (mPpu m) i
        c <- MV.read (hOam h) i
        when (s /= c) (bad ("OAM[" <> show i <> "]: spec " <> show s <> ", Clash " <> show c))

clashSnap :: Ppu -> PpuSnap
clashSnap p = PpuSnap
  [ ("line", i (pLine p)), ("dot", i (pDot p)), ("odd", bl (pOdd p)), ("ctrl", i (pCtrl p)), ("mask", i (pMask p))
  , ("vbl", bl (pVbl p)), ("spr0", bl (pSpr0 p)), ("ovf", bl (pOvf p)), ("suppress", bl (pSuppress p))
  , ("oamAddr", i (pOamAddr p)), ("v", i (pV p)), ("t", i (pT p)), ("x", i (pX p)), ("w", bl (pW p))
  , ("buf", i (pBuf p)), ("nt", i (pNt p)), ("at", i (pAt p)), ("patLo", i (pPatLo p)), ("patHi", i (pPatHi p))
  , ("bgLo", i (pBgLo p)), ("bgHi", i (pBgHi p)), ("atLo", i (pAtLo p)), ("atHi", i (pAtHi p))
  , ("oamBus", i (pOamBus p)), ("evM", i (pEvM p)), ("evFound", i (pEvFound p)), ("evSec", i (pEvSec p))
  , ("evDone", bl (pEvDone p)), ("spr0Next", bl (pSpr0Next p)), ("spr0Cur", bl (pSpr0Cur p))
  , ("rdAle", i (pRdAle p)), ("rdFill", i (pRdFill p)), ("rdAddr", i (pRdAddr p))
  , ("wrAle", i (pWrAle p)), ("wrDo", bl (pWrDo p)), ("wrAddr", i (pWrAddr p)), ("wrData", i (pWrData p))
  , ("oam2Ovf", bl (pOam2Ovf p)), ("fY", i (pFY p)), ("fT", i (pFT p)), ("fA", i (pFA p)), ("evPost", bl (pEvPost p))
  , ("flagPend", (if testBit (pFlagPend p) 1 then 0x40 else 0) + (if testBit (pFlagPend p) 0 then 0x20 else 0))
  , ("counting", i (pCounting p))
  , ("vPend", i (pVPend p)), ("vPendVal", i (pVPendVal p)), ("fetchA", i (pFetchA p)), ("oamCor", i (pOamCor p)) ]
  (map (fromIntegral) (C.toList (pSec p)))
  (concatMap (\u -> [i (uX u), i (uAttr u), i (uLo u), i (uHi u)]) (C.toList (pUnits p)))
  (map fromIntegral (C.toList (pPal p)))
 where
  i :: Integral a => a -> Int
  i = fromIntegral
  bl b = if b then 1 else 0

describe :: PpuSnap -> PpuSnap -> String
describe s c = unwords $
  [ n <> ": spec " <> show a <> ", Clash " <> show b | ((n, a), (_, b)) <- zip (snFields s) (snFields c), a /= b ]
  <> [ "secondary OAM differs: spec " <> show (snSec s) <> ", Clash " <> show (snSec c) | snSec s /= snSec c ]
  <> [ "sprite units differ: spec " <> show (snUnits s) <> ", Clash " <> show (snUnits c) | snUnits s /= snUnits c ]
  <> [ "palette differs: spec " <> show (snPal s) <> ", Clash " <> show (snPal c) | snPal s /= snPal c ]

clashSnapVec :: Ppu -> U.Vector Int
clashSnapVec p = let s = clashSnap p in U.fromList (map snd (snFields s) <> map fromIntegral (snSec s) <> snUnits s <> map fromIntegral (snPal s))
