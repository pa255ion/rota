-- | Layer 2 for the Game Gear's Clash units (PLAN.md step 5.6): each in
-- lockstep with its reference model. The PSG ("Gg.Psg" against
-- "Gg.Spec.Psg"): random writes and runs of clocks, the registers, the
-- LFSR and the stereo output compared after every operation; a broken
-- PSG must be caught. The VDP ("Gg.Vdp" against "Gg.Spec.Vdp"): the same
-- port accesses, dot by dot, replayed into both (random ones, and the
-- logs of our ROMs run on the machine); every port read, /INT at every
-- dot and every frame compared; a broken VDP must be caught.
module Main (main) where

import Prelude

import Clash.Prelude (Unsigned)
import Control.Monad (foldM, forM, forM_, when)
import Control.Monad.ST (ST, runST)
import qualified Data.ByteString as BS
import Data.List (sort)
import Data.STRef
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MV
import System.Directory (listDirectory)
import System.FilePath ((</>), takeBaseName, takeExtension)
import Data.Bits
import Data.Int (Int16)
import Data.Word
import qualified Clash.Prelude as C
import Clash.XException (rnfX)
import Test.Tasty
import Test.Tasty.HUnit

import Gg.Audio (filterInit, filterStep)
import Gg.Core
import Gg.Machine (cartAddr, cartByte, psKeys)
import Gg.Psg
import Gg.RomPort
import Gg.Display (runIncrement)
import qualified Gg.Spec as M
import qualified Gg.Spec.Psg as S
import qualified Gg.Spec.Vdp as SV
import Gg.Vdp
import qualified Z80.Core as Z
import qualified Z80.Spec as ZS

-- | The Master System's ROMs (systems/sms/test/asm, make sms-asm).
smsAsm :: FilePath
smsAsm = "../../build/sms/asm"

-- | Pad 1 and pad 2 buttons at instruction counts (padloop.sms).
padPresses :: [(Int, M.Input)]
padPresses =
  [ (k, M.Input p False q)
  | (k, (p, q)) <- zip [500, 1500 ..] [(0, 0), (M.padUp, 0), (0, M.padUp), (0, M.padDown), (0, M.padLeft), (0, M.padRight), (0, M.pad1), (0, M.pad2), (M.pad1, M.pad2), (0x3F, 0x3F), (0, 0)] ]

-- | The core with pad 2's up and down swapped (a planted bug).
brokenPad2 :: GgIn -> GgC -> (GgC, GgOut)
brokenPad2 gi = ggClock gi { giPad2 = swap (giPad2 gi) }
 where
  swap b = (b .&. 0xFC) .|. ((b .&. 1) `shiftL` 1) .|. ((b `shiftR` 1) .&. 1)

main :: IO ()
main = do
  let asmDir = "../../build/gg/asm"
  roms <- map (asmDir </>) . sort . filter ((== ".gg") . takeExtension) <$> listDirectory asmDir
  -- the Master System II's own regression ROMs (systems/sms, make sms-asm);
  -- bios/biosgame, fm, fmtune and padloop have their own cases
  smsRoms <- map (smsAsm </>) . sort . filter (\f -> takeExtension f == ".sms" && takeBaseName f `notElem` ["bios", "biosgame", "fm", "fmtune", "padloop"])
               <$> listDirectory smsAsm
  defaultMain $ testGroup "gg-lockstep"
   [ testGroup "the machine (Gg.Core = Gg.Spec)"
    ( [ testCase (takeBaseName f) $ do
          rom <- BS.readFile f
          check "" (machineCompare rom 8 [])
      | f <- roms ] <>
      [ testCase "Waternet, 40 frames" $ do
          rom <- BS.readFile "../../roms/gg/games/waternet.gg"
          check "" (machineCompare rom 40 [])
      , testCase "FluBBa's SMS VDP Test, 60 frames (Master System mode)" $ do
          rom <- BS.readFile "../../roms/gg/vdptest/VDPTEST.sms"
          check "" (machineCompareSms True rom 60 [])
      , testCase "Waternet, 12 frames, with inactive clocks (as on the board)" $ do
          rom <- BS.readFile "../../roms/gg/games/waternet.gg"
          check "" (machineCompareGaps rom 12)
      , testCaseInfo "Waternet, 10 frames, the ROM from a DDR of random latency (Gg.RomPort)" $ do
          rom <- BS.readFile "../../roms/gg/games/waternet.gg"
          case machineCompareDdr rom 10 of
            Left e -> assertFailure e >> pure ""
            Right (stalls, misses) -> do
              assertBool "no stall: the freeze path wasn't exercised" (stalls > 0)
              pure (show misses <> " DDR misses, " <> show stalls <> " clocks frozen")
      ] <>
      [ testGroup "the Master System II (M6)"
        ( [ testCase (takeBaseName f) $ do
              rom <- BS.readFile f
              check "" (machineCompareSms2 Nothing False rom 8 [])
          | f <- smsRoms ] <>
          [ testCase "biosgame.sms with our stand-in BIOS" $ do
              rom <- BS.readFile (smsAsm </> "biosgame.sms")
              bios <- BS.readFile (smsAsm </> "bios.sms")
              check "" (machineCompareSms2 (Just bios) False rom 8 [])
          , testCase "fm.sms with the FM unit (its writes compared)" $ do
              rom <- BS.readFile (smsAsm </> "fm.sms")
              check "" (machineCompareSms2 Nothing True rom 8 [])
          , testCase "fmtune.sms with the FM unit, 60 frames" $ do
              rom <- BS.readFile (smsAsm </> "fmtune.sms")
              check "" (machineCompareSms2 Nothing True rom 60 [])
          , testCase "padloop.sms: pad 1 and pad 2 pressed and released" $ do
              rom <- BS.readFile (smsAsm </> "padloop.sms")
              check "" (machineCompareSms2 Nothing False rom 4 padPresses)
          , testCase "FluBBa's SMS VDP Test, 60 frames" $ do
              rom <- BS.readFile "../../roms/gg/vdptest/VDPTEST.sms"
              check "" (machineCompareSms2 Nothing False rom 60 [])
          , testCase "Raposa do Sol, 60 frames" $ do
              rom <- BS.readFile "../../roms/sms/games/raposadosol_v20.sms"
              check "" (machineCompareSms2 Nothing False rom 60 [])
          , testCase "a broken Master System is caught (pad 2's up and down swapped)" $ do
              rom <- BS.readFile (smsAsm </> "padloop.sms")
              assertBool "not caught" (isJust' (machineCompareFull (Mach True True Nothing False) brokenPad2 (const False) rom 4 padPresses))
          ] )
      , testCase "a broken machine is caught (the VDP's ports a dot late)" $ do
          rom <- BS.readFile (asmDir </> "vdpint.gg")
          assertBool "not caught" (machineCompareWith brokenSchedule rom 8 [] /= Nothing)
      ] )
   , testGroup "VDP"
    [ testCase "40 random access programs, 3 frames each, GG mode: core = spec" $
        mapM_ (\seed -> check ("seed " <> show seed) (vdpCompare vdpClock False 3 (randomAccesses seed 3))) [1 .. 40 :: Int]
    , testCase "10 random access programs, Master System mode" $
        mapM_ (\seed -> check ("seed " <> show seed) (vdpCompare vdpClock True 3 (randomAccesses (1000 + seed) 3))) [1 .. 10 :: Int]
    , testGroup "our ROMs' VDP accesses (machine logs), 12 frames"
        [ testCase (takeBaseName f) $ do
            rom <- BS.readFile f
            let (lg, sms) = machineLog rom 12
            check "" (vdpCompare vdpClock sms 12 lg)
        | f <- roms ]
    , testCase "Waternet's first 120 frames (the menu drawn)" $ do
        rom <- BS.readFile "../../roms/gg/games/waternet.gg"
        let (lg, _) = machineLog rom 120
        check "" (vdpCompare vdpClock False 120 lg)
    , testCase "a broken VDP is caught (sprite zoom ignored)" $
        assertBool "not caught" (any (\seed -> vdpCompare brokenVdp False 3 (randomAccesses seed 3) /= Nothing) [1 .. 40 :: Int])
    ]
   , testGroup "board audio (Gg.Audio)"
    -- the board's noise (2026-10-07): the PSG's square edges point-sampled
    -- at 48 kHz alias; the filters must take a PSG tone at period 1 (about
    -- 112 kHz) down and leave an audible one alone
    [ testCase "a ~112 kHz square: under 5 % of its level after the filters" $
        assertBool "too loud" (squareRms 112 < 0.05 * 8191)
    , testCase "a ~1 kHz square: its level kept (peak over 90 %)" $
        assertBool "too quiet" (squarePeak 12588 > 0.9 * 8191)
    , testCase "PS keys (the Game Boy's bits) -> the pad and Start" $
        map (psKeys . C.bit) [0, 1, 2, 3, 4, 5, 6, 7]
          @?= [(0x08, False), (0x04, False), (0x01, False), (0x02, False), (0x20, False), (0x10, False), (0, False), (0, True)]
    , testCase "the cartridge word from link writes (low, then high; the file's word at first)" $
        ( cartByte Nothing 0x0300 (Just (cartAddr, 0x01))
        , cartByte (Just 0x0301) 0x0300 (Just (cartAddr + 1, 0x0F))
        , cartByte Nothing 0x0300 (Just (cartAddr + 2, 0xFF))
        , cartByte (Just 0x0F01) 0x0300 Nothing )
          @?= (Just 0x0301, Just 0x0F01, Nothing, Just 0x0F01)
    , testCase "silence stays silent" $
        snd (C.foldl (\(f, _) x -> filterStep f x) (filterInit, 0) (C.replicate (C.SNat @1000) 0)) @?= 0
    ]
   , testGroup "PSG"
    [ testCase "200 random programs of writes and clocks: core = spec" $
        mapM_ (\seed -> maybe (pure ()) (assertFailure . (("seed " <> show seed <> ": ") <>)) (psgLockstep psgTickC seed)) [1 .. 200]
    , testCase "a broken PSG is caught (periodic noise from the wrong bit)" $
        assertBool "not caught" (any (\s -> psgLockstep brokenTick s /= Nothing) [1 .. 50 :: Int])
    ]
   ]
 where
  check what r = maybe (pure ()) (\e -> assertFailure (what <> ": " <> e)) r

brokenTick :: PsgC -> PsgC
brokenTick p = let p' = psgTickC p in
  if testBit (pNoise p) 2 then p' else p' { pLfsr = pLfsr p' `xor` (pLfsr p' .&. 0x4000) }

rand :: Word64 -> Word64
rand x0 = let x1 = x0 `xor` (x0 `shiftR` 12)
              x2 = x1 `xor` (x1 `shiftL` 25)
              x3 = x2 `xor` (x2 `shiftR` 27)
          in x3 * 2685821657736338717

-- | 400 operations: a write (a latch or a data byte, every register) or
-- 1-200 clocks; the first difference.
psgLockstep :: (PsgC -> PsgC) -> Int -> Maybe String
psgLockstep tick seed = runST $ do
  sp <- S.newPsg
  let ops = take 400 (drop 1 (iterate rand (fromIntegral seed * 0x9E3779B97F4A7C15 + 7)))
      check i core = do
        regs <- S.psgRegisters sp
        lf <- S.psgLfsr sp
        let st = fromIntegral (ops !! i `shiftR` 40) :: Word8
        (l, r) <- S.psgSample sp st
        let coreRegs = map fromIntegral (C.toList (pPeriod core)) <> [fromIntegral (pNoise core)]
                       <> map fromIntegral (C.toList (pVol core))
            (cl, cr) = psgOutC (fromIntegral st) core
        pure $ if regs /= coreRegs then Just ("op " <> show i <> ": registers spec " <> show regs <> ", core " <> show coreRegs)
          else if lf /= fromIntegral (pLfsr core) then Just ("op " <> show i <> ": LFSR spec " <> show lf <> ", core " <> show (pLfsr core))
          else if (l, r) /= (toI cl, toI cr) then Just ("op " <> show i <> ": output spec " <> show (l, r) <> ", core " <> show (cl, cr))
          else Nothing
      toI = fromIntegral :: C.Signed 16 -> Int16
      run (Just e, core) _ = pure (Just e, core)
      run (Nothing, core) (i, w) = do
        core' <- if w .&. 3 == 0
          then do
            let n = fromIntegral ((w `shiftR` 8) `mod` 200) + 1 :: Int
            mapM_ (const (S.psgTick sp)) [1 .. n]
            pure (iterate tick core !! n)
          else do
            let b = fromIntegral (w `shiftR` 16) :: Word8
            S.psgWrite sp b
            pure (psgWriteC (fromIntegral b :: Unsigned 8) core)
        e <- check i core'
        pure (e, core')
  fst <$> foldM run (Nothing, psgPowerOn) (zip [0 ..] ops)

-- * The VDP

brokenVdp :: VdpIn -> VdpC -> (VdpC, VdpOut)
brokenVdp vin v = let (v', o) = vdpClock vin v in
  (v' { vSlots = fmap (\sl -> sl { slZoom = False }) (vSlots v') }, o)

toOp :: M.PortAccess -> PortOp
toOp a = case a of
  M.AccCtrlW b -> PCtrlW (fromIntegral b)
  M.AccDataW b -> PDataW (fromIntegral b)
  M.AccDataR -> PDataR
  M.AccStatusR -> PStatusR
  M.AccVCountR -> PVCountR
  M.AccHCountR -> PHCountR
  M.AccLatchH -> PLatchH

-- | The machine's VDP accesses running a ROM for @frames@ frames (its
-- mode from the file name is GG: our ROMs and Waternet are GG ROMs).
machineLog :: BS.ByteString -> Int -> ([(Int, M.PortAccess)], Bool)
machineLog rom frames = runST $ do
  m <- M.newMachine rom
  M.setVdpLog m True
  forM_ [1 .. frames] $ \_ -> M.runFrame m
  lg <- M.takeVdpLog m
  pure (lg, False)

-- | Random accesses: VRAM filled with random bytes and CRAM set, then
-- random register settings, VRAM and CRAM writes and every kind of read,
-- at random dots, over @frames@ frames.
randomAccesses :: Int -> Int -> [(Int, M.PortAccess)]
randomAccesses seed frames =
  let rs = drop 1 (iterate rand (fromIntegral seed * 0x9E3779B97F4A7C15 + 3))
      byte k = fromIntegral (rs !! k `shiftR` 32) :: Word8
      fill = [M.AccCtrlW 0, M.AccCtrlW 0x40] <> [ M.AccDataW (byte k) | k <- [0 .. 16383] ]
             <> [M.AccCtrlW 0, M.AccCtrlW 0xC0] <> [ M.AccDataW (byte (20000 + k)) | k <- [0 .. 63] ]
             <> concat [ [M.AccCtrlW v, M.AccCtrlW (0x80 + r)] | (r, v) <- [(0, 0x04), (1, 0x40), (2, 0xFF), (5, 0x7F), (6, 0xFB)] ]
      start = length fill
      gens = drop 30000 rs
      ops = randomOps gens
      dots = scanl1 (+) [ fromIntegral (g `mod` 40) + 2 | g <- drop 5000 rs ]
      limit = frames * SV.dotsPerFrame
  in zip [0 ..] fill <> takeWhile ((< limit) . fst) (zip (map (+ start) dots) ops)

-- | An endless random mix of accesses (register writes in pairs, VRAM and
-- CRAM writes after an address, single control bytes, reads).
randomOps :: [Word64] -> [M.PortAccess]
randomOps (g : gs) =
  let k = g `mod` 100
      v = fromIntegral (g `shiftR` 8) :: Word8
      r = fromIntegral ((g `shiftR` 16) `mod` 11) :: Word8
      a = fromIntegral (g `shiftR` 24) :: Word16
      regVal = case r of
        0 -> (v .&. 0xFB) .|. 0x04 .|. (if v .&. 0x10 /= 0 then 0 else 0)
        1 -> v .|. 0x40
        _ -> v
      here
        | k < 25 = [M.AccCtrlW regVal, M.AccCtrlW (0x80 + r)]
        | k < 45 = [M.AccCtrlW (fromIntegral a), M.AccCtrlW (0x40 .|. fromIntegral (a `shiftR` 8) .&. 0x7F), M.AccDataW v, M.AccDataW (v + 1)]
        | k < 55 = [M.AccCtrlW (fromIntegral a .&. 0x3F), M.AccCtrlW 0xC0, M.AccDataW v, M.AccDataW (v `xor` 0x55)]
        | k < 62 = [M.AccDataR]
        | k < 72 = [M.AccStatusR]
        | k < 80 = [M.AccVCountR]
        | k < 85 = [M.AccHCountR]
        | k < 90 = [M.AccLatchH]
        | k < 95 = [M.AccCtrlW v]
        | otherwise = [M.AccCtrlW (fromIntegral a), M.AccCtrlW (fromIntegral (a `shiftR` 8) .&. 0x3F), M.AccDataR, M.AccDataR]
  in here <> randomOps gs
randomOps [] = []

-- | Replay @acc@ into the spec and into the Clash VDP (with that clock
-- function), dot by dot, for @frames@ frames: the port reads and /INT
-- compared at every dot, the frames as both have made them; the first
-- difference. Interleaved, so memory stays small however long the run.
vdpCompare :: (VdpIn -> VdpC -> (VdpC, VdpOut)) -> Bool -> Int -> [(Int, M.PortAccess)] -> Maybe String
vdpCompare clk sms frames acc = runST $ do
  sv <- if sms then SV.newVdpSms else SV.newVdp
  banks <- MV.replicate 0x4000 (0 :: Word8) -- byte address; word w is bytes 4w .. 4w + 3
  let w = if sms then 256 else 160
  fb <- MV.replicate (w * (if sms then 192 else 144)) (0 :: Word16)
  qS <- newSTRef []        -- frames made by one side, not yet compared
  qC <- newSTRef []
  nS <- newSTRef (0 :: Int)
  coreRes <- newSTRef []
  let total = frames * SV.dotsPerFrame
      dedupe = go (-1) where
        go _ [] = []
        go p ((d, a) : xs) = if d == p then go p xs else (d, a) : go d xs
      readWord a = forM [0 .. 3] $ \q -> fromIntegral <$> MV.read banks (fromIntegral a * 4 + q)
      clock vin v = do
        let (v', o) = clk vin v
        forM_ (voWrite o) $ \(a, b) -> MV.write banks (fromIntegral a) (fromIntegral b)
        forM_ (voPixel o) $ \(x, y, c) -> MV.write fb (fromIntegral y * w + fromIntegral x) (fromIntegral c)
        when (voFrameEnd o) $ U.freeze fb >>= \f -> modifySTRef' qC (<> [f])
        forM_ (voPortData o) $ \x -> modifySTRef' coreRes (<> [fromIntegral x :: Word8])
        rd <- readWord (voReadAddr o)
        pure (v', toVec4 rd)
      specFrames = do
        n0 <- readSTRef nS
        nf <- SV.vdpFrames sv
        when (nf > n0) $ do
          writeSTRef nS nf
          SV.vdpFrame sv >>= \f -> modifySTRef' qS (<> [f])
      drainFrames = do
        a <- readSTRef qS
        b <- readSTRef qC
        case (a, b) of
          (x : xs, y : ys) -> do
            writeSTRef qS xs
            writeSTRef qC ys
            k' <- subtract (length xs + 1) <$> readSTRef nS
            if x == y then drainFrames
              else let j' = head [ q | q <- [0 ..], x U.! q /= y U.! q ] in
                pure (Just ("frame " <> show k' <> " differs first at x " <> show (j' `mod` w) <> ", y " <> show (j' `div` w)
                            <> ": spec " <> show (x U.! j') <> ", core " <> show (y U.! j')))
          _ -> pure Nothing
      loop d v rd as
        | d >= total = do
            SV.vdpSync sv total
            specFrames
            e <- drainFrames
            a <- readSTRef qS
            pure $ case e of
              Just m -> Just m
              Nothing | not (null a) -> Just ("the core made " <> show (frames - length a) <> " frames, the spec more")
                      | otherwise -> Nothing
        | otherwise = do
            -- the spec: dots before d, then this dot's access
            SV.vdpSync sv d
            specFrames
            let (here, as') = case as of
                  ((d', a) : rest) | d' == d -> (Just a, rest)
                  _ -> (Nothing, as)
            rS <- case here of
              Nothing -> pure Nothing
              Just a -> case a of
                M.AccCtrlW b -> Nothing <$ SV.vdpControlWrite sv b
                M.AccDataW b -> Nothing <$ SV.vdpDataWrite sv b
                M.AccDataR -> Just <$> SV.vdpDataRead sv
                M.AccStatusR -> Just <$> SV.vdpStatusRead sv
                M.AccVCountR -> Just <$> SV.vdpVCounter sv
                M.AccHCountR -> Just <$> SV.vdpHCounter sv
                M.AccLatchH -> Nothing <$ SV.vdpLatchHCounter sv
            iS <- SV.vdpInt sv
            -- the core: this dot's four clocks
            writeSTRef coreRes []
            (v1, rd1) <- clock (VdpIn True (maybe PNone toOp here) rd) v
            let iC = vdpIntC v1
            (v2, rd2) <- clock (VdpIn False PNone rd1) v1
            (v3, rd3) <- clock (VdpIn False PNone rd2) v2
            (v4, rd4) <- clock (VdpIn False PNone rd3) v3
            rC <- readSTRef coreRes
            e <- drainFrames
            case () of
              _ | rS /= listToMaybe' rC -> pure (Just ("dot " <> show d <> ", " <> show here <> ": port read spec " <> show rS <> ", core " <> show rC))
                | iS /= iC -> pure (Just ("dot " <> show d <> ": /INT spec " <> show iS <> ", core " <> show iC))
                | Just m <- e -> pure (Just m)
                | otherwise -> rnfX v4 `seq` loop (d + 1) v4 rd4 as'
  loop 0 (vdpPowerOn sms) (C.repeat 0) (dedupe acc)

listToMaybe' :: [a] -> Maybe a
listToMaybe' (x : _) = Just x
listToMaybe' [] = Nothing

toVec4 :: [Unsigned 8] -> C.Vec 4 (Unsigned 8)
toVec4 xs = case xs of
  [a, b, c, d] -> a C.:> b C.:> c C.:> d C.:> C.Nil
  _ -> C.repeat 0

-- * The machine

-- | Every VDP access a dot late.
brokenSchedule :: GgIn -> GgC -> (GgC, GgOut)
brokenSchedule gi g =
  let (g', o) = ggClock gi g
  in (g' { gVdpOp = fmap (\(d, op) -> if gVdpOp g == Nothing then (if d == maxBound then 0 else d + 1, op) else (d, op)) (gVdpOp g') }, o)

-- | What the two are compared on after each instruction: the registers
-- (WZ, Q and HALT included) and the T-states, unboxed (nothing kept alive).
type CpuView = U.Vector Int

specView :: ZS.Regs -> Int -> CpuView
specView r t = U.fromList
  [ fromIntegral (ZS.rA r), fromIntegral (ZS.rF r), fromIntegral (ZS.rB r), fromIntegral (ZS.rC r)
  , fromIntegral (ZS.rD r), fromIntegral (ZS.rE r), fromIntegral (ZS.rH r), fromIntegral (ZS.rL r)
  , fromIntegral (ZS.rIX r), fromIntegral (ZS.rIY r), fromIntegral (ZS.rSP r), fromIntegral (ZS.rPC r)
  , fromIntegral (ZS.rI r), fromIntegral (ZS.rR r), fromEnum (ZS.rIFF1 r), fromEnum (ZS.rIFF2 r)
  , fromIntegral (ZS.rIM r), fromIntegral (ZS.rWZ r), fromIntegral (ZS.rQ r), fromEnum (ZS.rHalted r), t ]

coreView :: Z.Cpu -> Int -> CpuView
coreView c t = U.fromList
  [ fromIntegral (Z.cA c), fromIntegral (Z.cF c), fromIntegral (Z.cB c), fromIntegral (Z.cC c)
  , fromIntegral (Z.cD c), fromIntegral (Z.cE c), fromIntegral (Z.cH c), fromIntegral (Z.cL c)
  , fromIntegral (Z.cIX c), fromIntegral (Z.cIY c), fromIntegral (Z.cSP c), fromIntegral (Z.cPC c)
  , fromIntegral (Z.cI c), fromIntegral (Z.cR c), fromEnum (Z.cIFF1 c), fromEnum (Z.cIFF2 c)
  , fromIntegral (Z.cIM c), fromIntegral (Z.cWZ c), fromIntegral (Z.cQ c), fromEnum (Z.cHalted c), t ]

machineCompare :: BS.ByteString -> Int -> [(Int, M.Input)] -> Maybe String
machineCompare = machineCompareWith ggClock

machineCompareSms :: Bool -> BS.ByteString -> Int -> [(Int, M.Input)] -> Maybe String
machineCompareSms sms = machineCompareMode sms ggClock

machineCompareWith :: (GgIn -> GgC -> (GgC, GgOut)) -> BS.ByteString -> Int -> [(Int, M.Input)] -> Maybe String
machineCompareWith = machineCompareMode False

machineCompareMode :: Bool -> (GgIn -> GgC -> (GgC, GgOut)) -> BS.ByteString -> Int -> [(Int, M.Input)] -> Maybe String
machineCompareMode sms clk = machineCompareFull (if sms then gameGearSms else gameGear) clk (const False)

-- | Which machine the two run (M6): the Game Gear, in its own mode or the
-- Master System mode, or the Master System II with a BIOS image or none and
-- an FM unit or none.
data Mach = Mach
  { maSms  :: Bool                    -- ^ Master System mode (the VDP, the ports)
  , maSms2 :: Bool                    -- ^ the Master System II's own hardware
  , maBios :: Maybe BS.ByteString
  , maFm   :: Bool
  }

gameGear, gameGearSms :: Mach
gameGear = Mach False False Nothing False
gameGearSms = Mach True False Nothing False

masterSystem :: Maybe BS.ByteString -> Bool -> Mach
masterSystem = Mach True True

-- | The Master System II's lockstep: as 'machineCompare'.
machineCompareSms2 :: Maybe BS.ByteString -> Bool -> BS.ByteString -> Int -> [(Int, M.Input)] -> Maybe String
machineCompareSms2 bios fm = machineCompareFull (masterSystem bios fm) ggClock (const False)

-- | A spec FM unit that only records its writes (T-state, port, byte) and
-- answers F2h's read-back, for comparing the core's 'goFm' (the chip
-- itself has its own tests in the sms package).
recordingFm :: STRef s [(Int, Int, Int)] -> ST s (M.FmUnit s)
recordingFm rec = do
  ctl <- newSTRef (0 :: Word8)
  pure M.FmUnit
    { M.fuWrite = \t p v -> do
        modifySTRef' rec (<> [(t, fromIntegral p, fromIntegral v)])
        when (p == 0xF2) $ writeSTRef ctl (v .&. 3)
    , M.fuRead = \_ -> readSTRef ctl
    , M.fuLevel = \_ -> pure 0
    , M.fuPsgOn = pure True
    }

-- | With an inactive clock after every active one whose number is a
-- multiple of 6 or 7 (about the board's rate): the memories read the
-- inactive clock's addresses for the next active one, as block RAMs do.
machineCompareGaps :: BS.ByteString -> Int -> Maybe String
machineCompareGaps rom frames = machineCompareFull gameGear ggClock (\k -> k `mod` 6 == 0 || k `mod` 7 == 0) rom frames []

-- | Run the spec machine and the Clash machine on a ROM for @frames@
-- frames, instruction by instruction (inputs change at instruction
-- indices): the views compared after every instruction, the frames and
-- the PSG writes (with the PSG clocks before each) as both have made
-- them; the first difference. Interleaved, so memory stays small.
machineCompareFull :: Mach -> (GgIn -> GgC -> (GgC, GgOut)) -> (Int -> Bool) -> BS.ByteString -> Int -> [(Int, M.Input)] -> Maybe String
machineCompareFull mach clk gaps rom frames inputs = runST $ do
  let sms = maSms mach
  fmS <- newSTRef []         -- FM unit writes not yet compared (spec, core)
  fmC <- newSTRef []
  fu <- if maFm mach then Just <$> recordingFm fmS else pure Nothing
  m <- if maSms2 mach then M.newMasterSystemFm (maBios mach) fu (M.detectMapper rom) rom
       else (if sms then M.newMachineSms else M.newMachine) rom
  M.setPsgLog m True
  let banks = max 1 ((BS.length rom + 0x3FFF) `div` 0x4000)
      pow2 = until (>= banks) (* 2) 1
      romV = U.fromList (BS.unpack rom <> replicate (pow2 * 0x4000 - BS.length rom) 0xFF)
      -- the BIOS image, mirrored to a power of two of 16 KiB banks
      biosBanks = maybe 0 (\b -> until (>= max 1 ((BS.length b + 0x3FFF) `div` 0x4000)) (* 2) 1) (maBios mach)
      biosV = maybe (U.replicate 0x4000 0xFF) (\b -> U.fromList (take (biosBanks * 0x4000) (cycle (BS.unpack b)))) (maBios mach)
      romByte a = if a >= 0x100000 then biosV U.! (a - 0x100000) else romV U.! a
      mapper = if M.detectMapper rom == M.Codemasters then MCodemasters else MSega
      w = if sms then 256 else 160
  ram <- MV.replicate 0x2000 (0 :: Word8)
  cart <- MV.replicate 0x8000 (0 :: Word8)
  vram <- MV.replicate 0x4000 (0 :: Word8)
  fb <- MV.replicate (w * (if sms then 192 else 144)) (0 :: Word16)
  fS <- newSTRef []          -- frames not yet compared
  fC <- newSTRef []
  pS <- newSTRef []          -- PSG writes not yet compared
  pC <- newSTRef []
  nFrames <- newSTRef (0 :: Int)
  clocks <- newSTRef (0 :: Int)
  -- the core's T-states since power-on (its counter is a phase, mod 512)
  tAbs <- newSTRef (0 :: Int)
  let readFor o = do
        let romB = romByte (fromIntegral (goRomAddr o))
            romB2 = romByte (fromIntegral (goRomAddr2 o))
        ramB <- MV.read ram (fromIntegral (goRamAddr o))
        cartB <- MV.read cart (fromIntegral (goCartAddr o))
        vr <- forM [0 .. 3] $ \q -> MV.read vram (fromIntegral (goVramRead o) * 4 + q)
        pure (romB, ramB, cartB, vr, romB2)
      inputAt i = case [ x | (q, x) <- inputs, q <= i ] of
        [] -> M.noInput
        xs -> last xs
      mk i (rb, mb, cb, v, rb2) = let inp = inputAt i in
        GgIn True (fromIntegral rb) (fromIntegral mb) (fromIntegral cb)
             (toVec4 (map fromIntegral v)) (fromIntegral (M.inPad inp)) (M.inStart inp) mapper (fromIntegral pow2)
             (fromIntegral rb2) (fromIntegral (M.inPad2 inp)) (fromIntegral biosBanks) Nothing
      -- core clocks until it ends an instruction; i: instructions done
      coreInstr g gi i = do
        let (g', o) = clk gi g
        modifySTRef' tAbs (+ fromIntegral (gT g' - gT g))
        rd <- readFor o
        forM_ (goRamWrite o) $ \(a, v) -> MV.write ram (fromIntegral a) (fromIntegral v)
        forM_ (goCartWrite o) $ \(a, v) -> MV.write cart (fromIntegral a) (fromIntegral v)
        forM_ (goVramWrite o) $ \(a, v) -> MV.write vram (fromIntegral a) (fromIntegral v)
        forM_ (goPixel o) $ \(x, y, c) -> MV.write fb (fromIntegral y * w + fromIntegral x) (fromIntegral c)
        -- a frame and its border colour (M6)
        when (goFrameEnd o) $ U.freeze fb >>= \f -> modifySTRef' fC (<> [f `U.snoc` fromIntegral (goBorder o)])
        forM_ (goFm o) $ \(p, b) -> readSTRef tAbs >>= \t -> modifySTRef' fmC (<> [(t, fromIntegral p, fromIntegral b)])
        case (gPsgW g, gPsgW g') of
          (Just (_, b), Nothing) -> readSTRef tAbs >>= \t -> modifySTRef' pC (<> [(t `div` 16, fromIntegral b) :: (Int, Int)])
          _ -> pure ()
        let i' = if goInstrEnd o then i + 1 else i
            gi' = mk i' rd
        n <- readSTRef clocks
        writeSTRef clocks (n + 1)
        (g2, gi2) <- if gaps n
          then do
            let (gI, oI) = clk gi' { giRun = False } g'
            when (gI /= g') $ error "an inactive clock changed the state"
            when (isJust' (goRamWrite oI) || isJust' (goVramWrite oI) || isJust' (goCartWrite oI)) $
              error "an inactive clock wrote"
            rdI <- readFor oI
            pure (gI, mk i' rdI)
          else pure (g', gi')
        -- the whole state forced at each instruction's end: Clash's vectors
        -- would otherwise grow chains of thunks over a long run
        if goInstrEnd o then rnfX g2 `seq` pure (g2, gi2) else g2 `seq` coreInstr g2 gi2 i
      drain qa qb what = do
        a <- readSTRef qa
        b <- readSTRef qb
        case (a, b) of
          (x : xs, y : ys) -> do
            writeSTRef qa xs
            writeSTRef qb ys
            if x == y then drain qa qb what else pure (Just (what x y))
          _ -> pure Nothing
      loop i g gi = do
        nf <- readSTRef nFrames
        if nf >= frames then do
          a <- readSTRef fS
          pure (if null a then Nothing else Just ("the core made fewer frames than the spec's " <> show nf))
        else do
          forM_ [ x | (q, x) <- inputs, q == i ] (M.setInput m)
          f0 <- M.machineFrames m
          _ <- M.stepMachine m
          r <- ZS.getRegs (M.mCpu m)
          t <- M.machineTStates m
          let vS = specView r t
          f1 <- M.machineFrames m
          when (f1 > f0) $ do
            f <- M.frameBuffer m
            b <- M.frameBorder m
            modifySTRef' fS (<> [f `U.snoc` b])
            writeSTRef nFrames f1
          pw <- M.takePsgLog m
          modifySTRef' pS (<> map (\(q, b) -> (q, fromIntegral b)) pw)
          (g', gi') <- coreInstr g gi i
          tNow <- readSTRef tAbs
          let vC = coreView (Z.zCpu (gZ g')) tNow
          eF <- drain fS fC (\_ _ -> "frame " <> show nf <> " (or so) differs")
          eP <- drain pS pC (\a b -> "PSG write: spec " <> show a <> ", core " <> show b)
          eM <- drain fmS fmC (\a b -> "FM unit write (T-state, port, byte): spec " <> show a <> ", core " <> show b)
          case () of
            _ | vS /= vC -> pure (Just ("instruction " <> show i <> ": spec " <> show (U.toList vS) <> "\n  core " <> show (U.toList vC)))
              | Just e <- eF -> pure (Just e)
              | Just e <- eP -> pure (Just e)
              | Just e <- eM -> pure (Just e)
              | otherwise -> g' `seq` loop (i + 1) g' gi'
  let g0 = if maSms2 mach then smsPowerOn (isJust' (maBios mach)) (maFm mach) mapper (fromIntegral pow2)
           else ggPowerOn sms mapper (fromIntegral pow2)
  loop (0 :: Int) g0 (GgIn True 0 0 0 (C.repeat 0) 0 False mapper (fromIntegral pow2) 0xFF 0 (fromIntegral biosBanks) Nothing)

isJust' :: Maybe a -> Bool
isJust' = maybe False (const True)

-- * The ROM from DDR

-- | The machine with its ROM behind "Gg.RomPort" and a fake DDR port: the
-- 32-byte lines of the last 16 misses answer the next clock, other reads
-- after 6 to 35 clocks (the measured range); the core gets the clocks the
-- port decides. Compared with the spec as 'machineCompareFull'; the
-- number of frozen clocks and of misses.
machineCompareDdr :: BS.ByteString -> Int -> Either String (Int, Int)
machineCompareDdr rom frames = runST $ do
  m <- M.newMachine rom
  M.setPsgLog m True
  let banks = max 1 ((BS.length rom + 0x3FFF) `div` 0x4000)
      pow2 = until (>= banks) (* 2) 1
      romV = U.fromList (BS.unpack rom <> replicate (pow2 * 0x4000 - BS.length rom) 0xFF)
      mapper = if M.detectMapper rom == M.Codemasters then MCodemasters else MSega
  ram <- MV.replicate 0x2000 (0 :: Word8)
  cart <- MV.replicate 0x8000 (0 :: Word8)
  vram <- MV.replicate 0x4000 (0 :: Word8)
  fb <- MV.replicate (160 * 144) (0 :: Word16)
  fS <- newSTRef []
  fC <- newSTRef []
  nFrames <- newSTRef (0 :: Int)
  stalls <- newSTRef (0 :: Int)
  misses <- newSTRef (0 :: Int)
  lines' <- newSTRef ([] :: [Int])
  pend <- newSTRef (Nothing :: Maybe (Int, Unsigned 21)) -- clocks left, address
  rng <- newSTRef (12345 :: Word64)
  tAbs <- newSTRef (0 :: Int)      -- the core's T-states (its counter is a phase)
  let word8 a = let base = fromIntegral a .&. complement 7 in
        toVec8 [ fromIntegral (romV U.! ((base + k) `mod` U.length romV)) | k <- [0 .. 7] ]
      -- the answer due this clock (from an earlier request)
      ddrAnswer = do
        p <- readSTRef pend
        case p of
          Just (0, a) -> writeSTRef pend Nothing >> pure (Just (word8 a))
          Just (n, a) -> writeSTRef pend (Just (n - 1, a)) >> pure Nothing
          Nothing -> pure Nothing
      -- a new request: a recent line answers the next clock, others later
      ddrRequest req =
        forM_ req $ \a -> do
          ls <- readSTRef lines'
          let line = fromIntegral a `div` 32
          lat <- if line `elem` ls then pure 0 else do
            modifySTRef' misses (+ 1)
            writeSTRef lines' (take 16 (line : ls))
            r <- readSTRef rng
            let r' = rand r
            writeSTRef rng r'
            pure (fromIntegral (r' `mod` 30) + 5)
          writeSTRef pend (Just (lat, a))
      readFor o = do
        ramB <- MV.read ram (fromIntegral (goRamAddr o))
        cartB <- MV.read cart (fromIntegral (goCartAddr o))
        vr <- forM [0 .. 3] $ \q -> MV.read vram (fromIntegral (goVramRead o) * 4 + q)
        pure (ramB, cartB, vr)
      -- pixel clocks until the core ends an instruction
      coreInstr g rp (ramB, cartB, vr) = do
        ans <- ddrAnswer
        let (rp', ro) = romPortStep rp (RomIn (ggRomWant g) (ggTBoundary g) ans runIncrement)
        ddrRequest (roRequest ro)
        let gi = GgIn (roRun ro) (roByte ro) (fromIntegral ramB) (fromIntegral cartB) (toVec4 (map fromIntegral vr)) 0 False mapper (fromIntegral pow2) 0xFF 0 1 Nothing
            (g', o) = ggClock gi g
        -- frozen: the core had credit for this clock but its ROM byte wasn't there
        modifySTRef' tAbs (+ fromIntegral (gT g' - gT g))
        let hadCredit = min creditCap (rpCredit rp + C.resize runIncrement) >= (1 `shiftL` 32)
        when (not (roRun ro) && hadCredit) $ modifySTRef' stalls (+ 1)
        when (roRun ro) $ do
          forM_ (goRamWrite o) $ \(a, v) -> MV.write ram (fromIntegral a) (fromIntegral v)
          forM_ (goCartWrite o) $ \(a, v) -> MV.write cart (fromIntegral a) (fromIntegral v)
          forM_ (goVramWrite o) $ \(a, v) -> MV.write vram (fromIntegral a) (fromIntegral v)
          forM_ (goPixel o) $ \(x, y, c) -> MV.write fb (fromIntegral y * 160 + fromIntegral x) (fromIntegral c)
          when (goFrameEnd o) $ U.freeze fb >>= \f -> modifySTRef' fC (<> [f])
        when (not (roRun ro) && g' /= g) $ error "an inactive clock changed the state"
        rd <- readFor o
        if roRun ro && goInstrEnd o then rnfX g' `seq` pure (g', rp', rd) else g' `seq` coreInstr g' rp' rd
      loop i g rp rd = do
        nf <- readSTRef nFrames
        if nf >= frames then (\a b -> Right (a, b)) <$> readSTRef stalls <*> readSTRef misses else do
          f0 <- M.machineFrames m
          _ <- M.stepMachine m
          r <- ZS.getRegs (M.mCpu m)
          t <- M.machineTStates m
          let vS = specView r t
          f1 <- M.machineFrames m
          when (f1 > f0) $ do
            M.frameBuffer m >>= \f -> modifySTRef' fS (<> [f])
            writeSTRef nFrames f1
          _ <- M.takePsgLog m
          (g', rp', rd') <- coreInstr g rp rd
          tNow <- readSTRef tAbs
          let vC = coreView (Z.zCpu (gZ g')) tNow
          a <- readSTRef fS
          b <- readSTRef fC
          frameBad <- case (a, b) of
            (x : xs, y : ys) -> writeSTRef fS xs >> writeSTRef fC ys >> pure (x /= y)
            _ -> pure False
          if vS /= vC then pure (Left ("instruction " <> show i <> ": spec " <> show (U.toList vS) <> "\n  core " <> show (U.toList vC)))
          else if frameBad then pure (Left ("a frame differs near frame " <> show nf))
          else loop (i + 1 :: Int) g' rp' rd'
  loop 0 (ggPowerOn False mapper (fromIntegral pow2)) romPortInit (0, 0, [0, 0, 0, 0])

toVec8 :: [Unsigned 8] -> C.Vec 8 (Unsigned 8)
toVec8 xs = case xs of
  [a, b, c, d, e, f, g, h] -> a C.:> b C.:> c C.:> d C.:> e C.:> f C.:> g C.:> h C.:> C.Nil
  _ -> C.repeat 0

-- * Board audio

-- | The filtered output of a +-8191 square wave flipping every @half@ pixel
-- clocks, after 50000 clocks to settle, over the next 150000.
filteredSquare :: Int -> [Double]
filteredSquare half = drop 50000 (take 200000 (go filterInit 0))
 where
  go f n = let x = if (n `div` half) `mod` 2 == 0 then 8191 else -8191
               (f', o) = filterStep f x
           in o `seq` (fromIntegral o : go f' (n + 1))

squareRms :: Int -> Double
squareRms half = let xs = filteredSquare half in sqrt (sum (map (^ (2 :: Int)) xs) / fromIntegral (length xs))

squarePeak :: Int -> Double
squarePeak = maximum . filteredSquare
