-- | Rendition B's figures per machine as the generator lays them out:
-- tiles, tilemap writes, sprites, stream sizes (a development tool; the
-- tests check the limits). With a frame range, each Game Boy frame's
-- work; with "nes-areas", the NES's 16 × 16 areas and their roles.
module Main (main) where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Vector.Unboxed as U
import System.Environment (getArgs)

import Codec.Picture (PixelRGB8 (..), generateImage, writePng)
import Rota.Gb.Rom
import Data.Bits (popCount, setBit, shiftR, testBit, (.&.))
import Data.List (minimumBy, sortOn)
import Data.Ord (comparing)
import Rota.Lab
import qualified Rota.Nes as N
import qualified Rota.A800 as A
import qualified Rota.Gg as GG
import qualified Rota.Gg.Rom as GR
import qualified Rota.Sms.Rom as SR
import Rota.Clock (Clock (..))
import Control.Monad.ST (runST)
import qualified A800.Spec as AS
import qualified A800.Spec.Machine as AM
import qualified A800.Spec.Antic as AA
import qualified A800.Spec.Gtia as AG
import qualified A800.Picture as AP
import Mos6502.Spec (Regs (..), getRegs)
import Numeric (readHex)
import qualified Data.ByteString as BS
import qualified Rota.A800.Rom as AR
import A800.Palette (paletteRgb)
import Rota.Score (jsRound)
import Nes.Palette (ntscPalette)
import Text.Printf (printf)
import qualified Rota.Nes.Rom as NR
import Rota.Clock (ckIdle, ckLead, weaveCapped)
import Data.Maybe (catMaybes, isJust)
import Data.Word (Word8)
import qualified Chip8.Spec as C
import Rota.Pic.Chip8 (chip8Program, chip8Code)
import Rota.Chip8.Asm (labelAddr, labels)
import Rota.Sound.Chip8 (chip8Beeps)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["nes-areas"] -> nesAreas
    ["gg-rom"] -> putStr GR.ggStats
    ["sms-rom"] -> putStr SR.smsStats
    -- picture frames side by side, 2x
    ["gg-preview", out, fs] -> do
      let frames = read fs :: [Int]
          imgs = map GR.ggExpected frames
          px x y = let (k, xx) = (x `div` 2) `divMod` 164
                   in if xx >= 160 then PixelRGB8 40 40 40 else
                      let c = (imgs !! k) U.! ((y `div` 2) * 160 + xx)
                          ch sh = fromIntegral ((c `shiftR` sh) .&. 15) * 17
                      in PixelRGB8 (ch 0) (ch 4) (ch 8)
      writePng out (generateImage px (2 * 164 * length frames) (2 * 144))
    ["gg-stats"] -> do
      let ck = GG.ggClock
          n = ckIdle ck + 600
          logos p = GG.shLogo (GG.shotAt p)
          changes = [ (p, length [ () | (a, b) <- zip (logos (p - 1)) (logos p), a /= b ]) | p <- [1 .. n] ]
          sprs p = GG.shSprites (GG.shotAt p)
          perLine p = maximum (0 : [ length [ () | s <- sprs p, GG.spY0 s <= y, y < GG.spY0 s + 8 ] | y <- [0 .. 143] ])
      putStrLn ("lead " <> show (ckLead ck) <> ", idle " <> show (ckIdle ck))
      putStrLn ("fabric slots " <> show (length GG.fabricSlots) <> ", logo cells " <> show (length [ () | GG.Logo <- GG.cells ]))
      putStrLn ("distinct logo tiles " <> show (S.size (S.fromList (concat [ [ t | Just t <- logos p ] | p <- [0 .. n] ]))))
      putStrLn ("most logo tile changes in a frame " <> show (maximum (map snd changes)) <> " at " <> show [ p | (p, c) <- changes, c == maximum (map snd changes) ])
      putStrLn ("frames with logo changes " <> show (length (filter ((> 0) . snd) changes)))
      putStrLn ("most sprites " <> show (maximum [ length (sprs p) | p <- [0 .. n] ]) <> ", most on a line " <> show (maximum [ perLine p | p <- [0 .. n] ]))
      putStrLn ("sprite pixels lost " <> show (sum [ GG.spLost s | p <- [0 .. n], s <- sprs p ]))
      putStrLn ("distinct sprite patterns " <> show (S.size (S.fromList [ GG.spPat s | p <- [0 .. n], s <- sprs p ])))
    -- the CHIP-8 demo on the spec (VIP quirks): frames from..to by step
    -- the screen XOR the program's own bands table, per frame from the
    -- idle on: one picture per token state if the threads stay consistent;
    -- "rate" instructions a second, at "fps" frames a second
    ["chip8-check", rateA, fpsA, nA] -> chip8Check (read rateA) (read fpsA) (read nA)
    -- the frames at which the threads' sweep starts again (VD back to 0)
    ["chip8-sweeps", ipfA, nA] -> do
      let ms = take (read nA) (drop 1 (iterate (C.runFrames C.vipQuirks (read ipfA) (const 0) 1) (C.loadProgram chip8Program C.newMachine)))
          xs = map (\m -> C.mV m U.! 0xD) ms
      print [ k | (k, a, b) <- zip3 [1 :: Int ..] xs (drop 1 xs), b < a ]
    ["chip8-beeps"] -> print (map fst chip8Beeps)
    -- the longest runs of instructions between two calls of "clock" (the
    -- beeps' check), with the PC that started each run
    ["chip8-gaps", nA] -> do
      let clockA = fromIntegral (labelAddr chip8Code "clock")
          go :: Int -> C.Machine -> Int -> Int -> [(Int, Int)] -> [(Int, Int)]
          go 0 _ _ _ acc = acc
          go k m g pc0 acc =
            let m' = C.step C.vipQuirks 0 m
            in if C.mPc m' == clockA then go (k - 1) m' 0 (fromIntegral (C.mPc m)) ((g, pc0) : acc)
               else go (k - 1) m' (g + 1) pc0 acc
          gaps = go (read nA) (C.loadProgram chip8Program C.newMachine) 0 0 []
          names = M.fromList [ (a, l) | (l, a) <- M.toList (labels chip8Code) ]
          near a = maybe "?" snd (M.lookupLE a names)
      mapM_ (\(g, pc) -> putStrLn (show g <> " instructions after " <> near pc)) (take 12 (sortOn (negate . fst) [ (g, pc) | (pc, g) <- M.toList (M.fromListWith max [ (pc, g) | (g, pc) <- gaps ]) ]))
    ["chip8-bands", ipfA, frameA] -> do
      let m = C.runFrames C.vipQuirks (read ipfA) (const 0) (read frameA) (C.loadProgram chip8Program C.newMachine)
          bands = labelAddr chip8Code "bands"
      mapM_ (\x -> putStrLn (show x <> ": " <> show [ C.mRam m U.! (bands + 6 * x + k) | k <- [0 .. 5] ])) [0 .. 8 :: Int]
    ["chip8-frames", ipfA, fromA, toA, stepA] -> do
      let run = chip8Displays (read ipfA) (read toA + 1)
      mapM_ (\k -> putStr ("frame " <> show k <> ":\n" <> chip8Art (run !! k))) [read fromA, read fromA + read stepA .. read toA]
    ["a800-lines"] -> a800Lines
    ["a800-model", out] -> do
      let ps = [ jsRound (b * 0.4 * A.a800Fps) | b <- [1.42, 2.45, 4.05, 7.0, 9.3 :: Double] ]
          pics = [ A.renderShot (Just (A.shotAt p)) | p <- ps ]
          px x y = let (row, yy) = y `divMod` 240
                       (r, g, b) = paletteRgbA ((pics !! row) U.! (yy * 320 + x))
                   in PixelRGB8 r g b
      writePng out (generateImage (\x y -> px (x `div` 2) (y `div` 2)) 640 (2 * 240 * length ps))
    ["a800-bytes"] -> do
      let bytesOf p = let bm = A.shBitmap (A.shotAt p) in [ foldl (\acc i -> acc * 4 + fromIntegral (bm U.! (y * 160 + 4 * xb + i))) (0 :: Int) [0 .. 3] | y <- [0 .. 191], xb <- [0 .. 39] ]
          fr = [0 .. 260]
          bs = map bytesOf fr
          diffs = zipWith (\a b -> length (filter id (zipWith (/=) a b))) bs (drop 1 bs)
      putStrLn ("changed bytes per frame (most " <> show (maximum diffs) <> "): " <> show [ (p + 1, d) | (p, d) <- zip fr diffs, d > 0 ])
    ["a800-dli"] -> putStr AR.a800Stats
    ["a800-steps"] -> do
      print (ckLead A.a800Clock, ckIdle A.a800Clock)
      print [ (k, AR.stepsAt k, weaveCapped A.a800Clock (k - ckLead A.a800Clock)) | k <- [88 .. 112] ]
    ["a800-spec", bin, out, fs] -> do
      bs <- BS.readFile bin
      let frames = read fs :: [Int]
          px x y = let (row, yy) = y `divMod` 240
                       (r, g, b) = paletteRgbA (BS.index bs ((frames !! row) * 76800 + yy * 320 + x))
                   in PixelRGB8 r g b
      writePng out (generateImage px 320 (240 * length frames))
    ["a800-debug", xexPath, lblPath, nA, names] -> do
      os <- BS.readFile "roms/a800xl/os/altirraos_xl.rom"
      basic <- BS.readFile "roms/a800xl/os/altirra_basic.rom"
      loader <- BS.readFile "build/a800xl/xexload.bin"
      xex <- BS.readFile xexPath
      lbl <- readFile lblPath
      let n = read nA :: Int
          addrs = [ (nm, fst (head (readHex a))) | nm <- words names, l <- lines lbl, [_, a, '.' : nm'] <- [words l], nm' == nm ] :: [(String, Int)]
          out = runST $ do
            m <- AM.newMachine (AS.xexConfig os basic loader xex)
            let go f acc
                  | f >= n = pure (reverse acc)
                  | otherwise = do
                      _ <- AM.stepCpu m
                      f' <- AA.anticFrames (AM.mAntic m)
                      if f' > f
                        then do
                          r <- getRegs (AM.mCpu m)
                          vs <- mapM (AM.peekRam m . fromIntegral . snd) addrs
                          go f' ((f', rPC r, vs) : acc)
                        else go f' acc
            go 0 []
      print (map fst addrs)
      mapM_ print out
    ["a800-diff", bin, nA, dA] -> do
      bs <- BS.readFile bin
      let n = read nA :: Int
          nSpec = BS.length bs `div` 76800
          frameS k = U.generate 76800 (\i -> BS.index bs (k * 76800 + i))
          lead = ckLead A.a800Clock
          ex = A.a800Expected n
          d = read dA :: Int
      putStrLn ("offset " <> show d)
      let report (k, e) =
            let sf = frameS (k + d)
                diffs = [ (i `mod` 320, i `div` 320, e U.! i, sf U.! i) | i <- [0 .. 76799], e U.! i /= sf U.! i ]
            in if null diffs then pure () else putStrLn (show k <> ": " <> show (length diffs) <> " " <> show (take 6 diffs))
      mapM_ report [ (k, e) | (k, e) <- zip [0 :: Int ..] ex, k >= lead, k + d < nSpec ]
      let fab c = c == A.colFabric
          split k dd = let e = ex !! k; sf = frameS (k + d + dd)
                           ds = [ (e U.! i, sf U.! i) | i <- [0 .. 76799], e U.! i /= sf U.! i ]
                       in (length [ () | (a, b) <- ds, fab a || fab b ], length [ () | (a, b) <- ds, not (fab a || fab b) ])
      mapM_ (\k -> putStrLn ("frame " <> show k <> " (fabric, other) at offsets -1, 0, +1: " <> show [ split k dd | dd <- [-1, 0, 1] ])) [92 .. 100]
    ["a800-fabmap", bin] -> do
      bs <- BS.readFile bin
      let frameS k = U.generate 76800 (\i -> BS.index bs (k * 76800 + i))
          ex = A.a800Expected 140
          fabOnly f = U.map (\c -> if c == A.colFabric then 1 else 0 :: Int) f
          modelF = [ (k, fabOnly (ex !! k)) | k <- [90 .. 110] ]
      let row b =
            let sf = fabOnly (frameS b)
                best = [ k | (k, mf) <- modelF, U.length (U.filter id (U.zipWith (/=) sf mf)) < 60 ]
            in putStrLn ("bin " <> show b <> ": model frames " <> show best)
      mapM_ row [158 .. 180]
      let rowsVs b k1 k2 = [ (y, m1, m2) | y <- [24 .. 215], let f = fabOnly (frameS b); a = fabOnly (ex !! k1); c = fabOnly (ex !! k2)
                                       , let m1 = and [ f U.! (y * 320 + x) == a U.! (y * 320 + x) | x <- [0 .. 319] ]
                                       , let m2 = and [ f U.! (y * 320 + x) == c U.! (y * 320 + x) | x <- [0 .. 319] ], m1 /= m2 ]
      putStrLn ("bin 169 rows (y, = step 1, = step 2): " <> show (rowsVs 169 97 98))
    ["a800-cell", xexPath, aA, xA, yA] -> do
      os <- BS.readFile "roms/a800xl/os/altirraos_xl.rom"
      basic <- BS.readFile "roms/a800xl/os/altirra_basic.rom"
      loader <- BS.readFile "build/a800xl/xexload.bin"
      xex <- BS.readFile xexPath
      let addr = read aA :: Int
          px = read xA :: Int
          py = read yA :: Int
          out = runST $ do
            m <- AM.newMachine (AS.xexConfig os basic loader xex)
            let go f acc
                  | f >= 180 = pure (reverse acc)
                  | otherwise = do
                      _ <- AM.stepCpu m
                      f' <- AA.anticFrames (AM.mAntic m)
                      if f' > f
                        then do
                          full <- AG.gtiaFrame (AM.mGtia m)
                          let pic = AP.picturePixels (AP.cropPicture (U.fromList full))
                          v <- AM.peekRam m (fromIntegral addr)
                          go f' ((f', v, pic U.! (py * 320 + px)) : acc)
                        else go f' acc
            go 0 []
      mapM_ print (drop 160 out)
    ["a800-write", xexPath, aA] -> do
      os <- BS.readFile "roms/a800xl/os/altirraos_xl.rom"
      basic <- BS.readFile "roms/a800xl/os/altirra_basic.rom"
      loader <- BS.readFile "build/a800xl/xexload.bin"
      xex <- BS.readFile xexPath
      let addr = read aA :: Int
          out = runST $ do
            m <- AM.newMachine (AS.xexConfig os basic loader xex)
            let go prev acc = do
                  _ <- AM.stepCpu m
                  f <- AA.anticFrames (AM.mAntic m)
                  v <- AM.peekRam m (fromIntegral addr)
                  pos <- AA.anticPosition (AM.mAntic m)
                  let acc' = if v /= prev then (f, pos, v) : acc else acc
                  if f >= 180 then pure (reverse acc') else go v acc'
            go 0 []
      mapM_ print out
    ["a800-colours"] -> mapM_ (\c -> let (r, g, b) = paletteRgbA c in putStrLn (printf "%02X #%02X%02X%02X" c r g b)) [0, 2 .. 254 :: Word8]
    ["a800-pf"] -> do
      mapM_ (\(y, rs) -> putStrLn (show y <> " " <> show rs)) [ x | x@(y, _) <- zip [0 :: Int ..] A.lineRoles, y `mod` 4 == 0 ]
      putStrLn "-- loads (earliest, line, register, role)"
      mapM_ print A.pfWrites
    ["a800-preview", out] -> a800Preview out
    ["a800-preview-b", out] -> a800PreviewB False False out
    ["a800-preview-b1", out] -> a800PreviewB True False out
    ["a800-preview-b1-solid", out] -> a800PreviewB True True out
    ["a800-preview-a", out] -> a800PreviewA out
    ["nes"] -> nesCount >> putStr NR.nesStats
    ["nes-split-worst"] -> mapM_ (\(k, f) -> putStrLn (show k <> ": cost " <> show (NR.nmiCost NR.Split f) <> " palette " <> show (isJust (NR.rfPalette f)) <> " nt " <> show (length (NR.rfNt f)) <> " steps " <> show (NR.rfSteps f)))
                     [ x | x@(_, f) <- zip [0 :: Int ..] (NR.runFrames NR.Split 700), NR.nmiCost NR.Split f > 1400 ]
    ["nes-colours"] -> mapM_ (\c -> let (r, g, b) = ntscPalette U.! c in putStrLn (printf "%02X #%02X%02X%02X" c r g b)) [0 .. 0x3F :: Int]
    ["nes-palettes"] -> nesPalettes 1
    ["nes-palettes", wt] -> nesPalettes (read wt)
    ["nes-preview", out] -> nesPreview out
    [a, b] -> mapM_ (\(k, f) -> putStrLn (show k <> ": " <> show (rfCtrl f) <> " writes " <> show [ (m, length [ () | (m', _, _) <- rfBg f, m' == m ]) | m <- [0, 1 :: Int] ] <> " steps " <> show (rfSteps f) <> " cost " <> show (frameCost f)))
                (take (read b - read a + 1) (drop (read a) (zip [0 :: Int ..] romIntro)))
    _ -> putStr ("Game Boy\n" <> gbStats)

nesAreas :: IO ()
nesAreas = do
  let g = geometry nesM
      w = 256
      buf = compose g 7 WeaveCost
      roles x y = S.fromList [ r | yy <- [y .. y + 15], xx <- [x .. x + 15], let r = buf U.! (yy * w + xx), r /= 0 ]
      areas = M.fromListWith (+) [ (S.toList rs, 1 :: Int) | y <- [0, 16 .. 239], x <- [0, 16 .. 255], let rs = roles x y, not (S.null rs) ]
  mapM_ print (M.toList areas)
  -- fabric tiles sharing an area with logo pixels, and fabric tiles in all
  let fabricTiles = [ (tx, ty) | ty <- [0, 8 .. 239], tx <- [0, 8 .. 255], isTraceCell g tx ty
                    , not (or [ gLogo g U.! (y * w + x) | y <- [ty .. ty + 7], x <- [tx .. tx + 7] ]) ]
      areaHasLogo tx ty = let ax = tx - tx `mod` 16; ay = ty - ty `mod` 16 in or [ gLogo g U.! (y * w + x) | y <- [ay .. min 239 (ay + 15)], x <- [ax .. ax + 15] ]
  let ringArea tx ty = let ax = tx - tx `mod` 16; ay = ty - ty `mod` 16 in or [ gRing g U.! j /= 0 || gEmb g U.! j || gCore g U.! j | y <- [ay .. min 239 (ay + 15)], x <- [ax .. ax + 15], let j = y * w + x ]
  putStrLn ("fabric tiles in areas with ring, lambda or core pixels " <> show (length (filter (uncurry ringArea) fabricTiles)) <> ": " <> show (filter (uncurry ringArea) fabricTiles))
  putStrLn ("fabric tiles " <> show (length fabricTiles) <> ", in areas with logo pixels " <> show (length (filter (uncurry areaHasLogo) fabricTiles)))
  -- two ring stages in one area: the minority's pixels
  let two = [ (x, y, M.toList cnt) | y <- [0, 16 .. 239], x <- [0, 16 .. 255]
            , let cnt = M.fromListWith (+) [ (r, 1 :: Int) | yy <- [y .. min 239 (y + 15)], xx <- [x .. x + 15], let r = gRing g U.! (yy * w + xx), r /= 0 ]
            , M.size cnt > 1 ]
  mapM_ print two

-- | Every choice of four 3-role palettes, scored by the pixels areas lose
-- (a role missing from the area's best palette; the fabric's bright
-- drawn dim where only dim is there counts nothing): the best few.
nesPalettes :: Int -> IO ()
nesPalettes fabricWeight = do
  let g = geometry nesM
      w = 256
      buf = compose g 7 WeaveCost
      fab r = r == roleTeal || r == roleViolet
      roleBit r = case r of
        _ | r >= 1 && r <= 5 -> fromIntegral r - 1
        6 -> 5
        7 -> 6
        8 -> 7
        10 -> 8
        _ -> 9 :: Int
      areas = [ M.fromListWith (+) [ (roleBit r, 1 :: Int) | yy <- [y .. min 239 (y + 15)], xx <- [x .. x + 15], let r = buf U.! (yy * w + xx), r /= 0, not (fab r) || True ]
              | y <- [0, 16 .. 239], x <- [0, 16 .. 255] ]
      areas' = filter (\m -> M.size m > 0) areas
      pals = [ foldl setBit (0 :: Int) [a, b, c] | a <- [0 .. 9], b <- [a + 1 .. 9], c <- [b + 1 .. 9] ]
      -- the fabric: every fabric pixel is lost without the dim colour;
      -- with dim but not bright, its pulses go dim (half a pixel each)
      lost m p = sum [ n | (r, n) <- M.toList m, r < 8, not (testBit p r) ]
               + (let f = M.findWithDefault 0 8 m + M.findWithDefault 0 9 m
                  in if not (testBit p 9) then f * fabricWeight else if not (testBit p 8) then f `div` 2 else 0)
      costs = [ U.fromList [ lost m p | p <- pals ] | m <- areas' ]
      np = length pals
      score (i, j, k, l) = sum [ minimum [c U.! i, c U.! j, c U.! k, c U.! l] | c <- costs ]
      combos = [ (i, j, k, l) | i <- [0 .. np - 1], j <- [i + 1 .. np - 1], k <- [j + 1 .. np - 1], l <- [k + 1 .. np - 1] ]
      best = take 8 (sortOn fst [ (score q, q) | q <- combos ])
      name p = [ nm | (b, nm) <- zip [0 ..] ["s1", "s2", "s3", "s4", "s5", "lam", "core", "let", "fabB", "fabD"], testBit p b ]
  mapM_ (\(sc, (i, j, k, l)) -> putStrLn (show sc <> " " <> show (map (name . (pals !!)) [i, j, k, l]))) best
  let _ = (popCount (0 :: Int), (.&.) (0 :: Int) 0, minimumBy (comparing id) [0 :: Int])
  pure ()

-- | The NES picture at a few moments with the lab's colours (LOOK.nes),
-- side by side: as designed, and with the fabric left out of the areas
-- that hold ring, lambda or core pixels.
nesPreview :: FilePath -> IO ()
nesPreview out = do
  let g = geometry nesM
      w = 256
      h = 240
      beats = [2.2, 6.1, 9.3]
      hex c = PixelRGB8 (fromIntegral (c `div` 65536)) (fromIntegral (c `div` 256 `mod` 256)) (fromIntegral (c `mod` 256))
      ringArea x y = let ax = x - x `mod` 16; ay = y - y `mod` 16 in or [ gRing g U.! j /= 0 || gEmb g U.! j || gCore g U.! j | yy <- [ay .. min 239 (ay + 15)], xx <- [ax .. ax + 15], let j = yy * w + xx ]
      ringAreas = U.generate (16 * 15) (\i -> let (ay, ax) = i `divMod` 16 in ringArea (ax * 16) (ay * 16))
      frame b cut =
        let buf = compose g b (WeaveAt (floor (b * 24 / 4)))
            st = roleState b
            spr = M.fromList [ ((spX p, spY p), spRole p) | p <- sparkPixels g b <> lightPixels g b (compose g 0 WeaveCost) ]
            col x y =
              let r = buf U.! (y * w + x)
                  fabricHere = r == roleTeal || r == roleViolet
                  r' = if cut && fabricHere && ringAreas U.! ((y `div` 16) * 16 + x `div` 16) then 0 else r
              in case M.lookup (x, y) spr of
                   Just sr | not (gLogo g U.! (y * w + x)) -> sprCol sr
                   _ -> bgCol st r'
        in col
      ring look = case look of
        RingOff -> 0x3C1E80
        RingOn -> 0x00A0A8
        RingHead -> 0xFCFCFC
        RingMid -> 0x78DCE8
        RingBg -> 0x000000
      bgCol st r
        | r >= 1 && r <= 5 = hex (ring (rsRing st !! (fromIntegral r - 1)))
        | r == roleEmb = hex 0xFCA044
        | r == roleCore = hex 0xFCFCFC
        | r == roleLetters = hex (if rsLettersLit st then 0xFCFCFC else 0x24188C)
        | r == roleTeal = hex 0x00788A
        | r == roleViolet = hex 0x24188C
        | otherwise = hex 0
      sprCol r = hex (case r of
        30 -> 0xFCE0A8
        31 -> 0xE45C10
        32 -> 0xB8F8F8
        33 -> 0x00A8A8
        34 -> 0xD8B8F8
        35 -> 0x6844FC
        27 -> 0xA4E4FC
        28 -> 0x0088B8
        _ -> 0xFCFCFC)
      frames = [ (frame b False, frame b True) | b <- beats ]
      pick x y =
        let (row, yy) = y `divMod` (h + 8)
            (a, c) = frames !! row
        in if yy >= h then hex 0x404040
           else if x < w then a x yy else if x < w + 8 then hex 0x404040 else c (x - w - 8) yy
      img = generateImage pick (2 * w + 8) (length beats * (h + 8))
  writePng out img

nesCount :: IO ()
nesCount = do
  let frames = [0 .. ckIdle N.nesClock + 72 - 1]
      shots = map N.shotAt frames
      logos = map N.shLogo shots
      uniq = S.fromList (concatMap catMaybes logos)
      writes = zipWith (\a b -> length (filter id (zipWith (/=) a b))) logos (drop 1 logos)
      slots = S.fromList [ (ph, sh) | N.Fabric ph sh <- N.cells ]
      sprs = map N.shSprites shots
  putStrLn ("NES: logo tiles " <> show (S.size uniq) <> " (with blank), fabric slots " <> show (S.size slots)
    <> "; most nametable writes " <> show (maximum writes) <> "; sprites at most " <> show (maximum (map length sprs))
    <> ", sprite patterns " <> show (S.size (S.fromList [ N.spPat s | ss <- sprs, s <- ss ])))
  putStrLn ("palettes: " <> show (map (map (\c -> showHex2 c)) (N.objColours)) <> " bg at 9.0: " <> show (N.bgColours (roleState 9.0)))
 where
  showHex2 c = c

-- | The 800XL picture (the lab's 320 × 192): the colour roles each line
-- needs (the fabric's two as one), with and without the letters.
a800Lines :: IO ()
a800Lines = do
  let g = geometry a800M
      w = 320
      buf = compose g 7 WeaveCost
      role r = if r == roleTeal then roleViolet else r
      line y lets = S.fromList [ role r | x <- [0 .. w - 1], let r = buf U.! (y * w + x), r /= 0, lets || r /= roleLetters ]
      shared y = S.fromList [ if r == roleLetters then roleViolet else role r | x <- [0 .. w - 1], let r = buf U.! (y * w + x), r /= 0, r /= roleCore ]
      count lets = [ (y, S.toList (line y lets)) | y <- [0 .. 191] ]
      hist lets = M.fromListWith (+) [ (length rs, 1 :: Int) | (_, rs) <- count lets ]
  putStrLn ("lines by colour count (letters in the playfield): " <> show (M.toList (hist True)))
  putStrLn ("lines by colour count (letters as players): " <> show (M.toList (hist False)))
  mapM_ print [ x | x@(_, rs) <- count False, length rs > 3 ]
  putStrLn ("lines by colour count (letters sharing the fabric's register, the core a player): " <> show (M.toList (M.fromListWith (+) [ (S.size (shared y), 1 :: Int) | y <- [0 .. 191] ])))
  putStrLn ("letter rows " <> show (minimum [ y | y <- [0 .. 191], x <- [0 .. w - 1], gLetters g U.! (y * w + x) ], maximum [ y | y <- [0 .. 191], x <- [0 .. w - 1], gLetters g U.! (y * w + x) ])
    <> ", core rows " <> show (minimum [ y | y <- [0 .. 191], x <- [0 .. w - 1], gCore g U.! (y * w + x) ], maximum [ y | y <- [0 .. 191], x <- [0 .. w - 1], gCore g U.! (y * w + x) ]))
  putStrLn ("ring rows " <> show (minimum [ y | y <- [0 .. 191], x <- [0 .. w - 1], gRing g U.! (y * w + x) /= 0 ], maximum [ y | y <- [0 .. 191], x <- [0 .. w - 1], gRing g U.! (y * w + x) /= 0 ])
    <> ", letters " <> show (minimum [ (x, y) | y <- [0 .. 191], x <- [0 .. w - 1], gLetters g U.! (y * w + x) ], maximum [ (x, y) | y <- [0 .. 191], x <- [0 .. w - 1], gLetters g U.! (y * w + x) ])
    <> ", square " <> show (gSquare g))

-- | The 800XL picture with the lab's 800XL colours: as designed (320
-- wide), and in ANTIC mode E (each pixel two wide: the left one's colour,
-- or the right one's where the left is background).
a800Preview :: FilePath -> IO ()
a800Preview out = do
  let g = geometry a800M
      w = 320
      h = 192
      beats = [2.2, 6.1, 9.3]
      hex c = PixelRGB8 (fromIntegral (c `div` 65536)) (fromIntegral (c `div` 256 `mod` 256)) (fromIntegral (c `mod` 256))
      frame b modeE =
        let buf = compose g b (WeaveAt (floor (b * 24 / 4)))
            st = roleState b
            spr = M.fromList [ ((spX p, spY p), spRole p) | p <- sparkPixels g b <> lightPixels g b (compose g 0 WeaveCost) ]
        in \x y ->
             let xl = x - x `mod` 2
                 r | modeE = let a = buf U.! (y * w + xl); c = buf U.! (y * w + xl + 1) in if a /= 0 then a else c
                   | otherwise = buf U.! (y * w + x)
             in case M.lookup (x, y) spr of
                  Just sr | not (gLogo g U.! (y * w + x)) -> sprCol sr
                  _ -> bgCol st y b r
      ring look = case look of
        RingOff -> 0x3B2E5A
        RingOn -> 0x2CB5A8
        RingHead -> 0xE8F8F0
        RingMid -> 0x7FD8CC
        RingBg -> 0x0E0B14
      mix a c t = let ch s = round (fromIntegral ((a `div` s) `mod` 256) * (1 - t) + fromIntegral ((c `div` s) `mod` 256) * (t :: Double)) :: Int
                  in ch 65536 * 65536 + ch 256 * 256 + ch 1
      bgCol st y b r
        | r >= 1 && r <= 5 = hex (ring (rsRing st !! (fromIntegral r - 1)))
        | r == roleEmb = hex 0xF2A23A
        | r == roleCore = hex 0xE8F8F0
        | r == roleLetters = if rsLettersLit st
            then let u = 0.5 + 0.5 * sin (fromIntegral y * 0.33 - b * 4) in hex (mix 0xF2A23A 0x2CB5A8 (fromIntegral (round (u * 6) :: Int) / 6))
            else hex 0x2A2340
        | r == roleTeal || r == roleViolet = hex 0x1E6E68
        | otherwise = hex 0x0E0B14
      sprCol r = hex (case r of
        30 -> 0xFFE0A0
        31 -> 0xC78A2C
        32 -> 0xB8F8F0
        33 -> 0x22A699
        34 -> 0xD8C8FF
        35 -> 0x7A68B8
        27 -> 0xC8F8F0
        28 -> 0x2CB5A8
        _ -> 0xFFFFFF)
      frames = [ (frame b False, frame b True) | b <- beats ]
      pick x y =
        let (row, yy) = y `divMod` (h + 8)
            (a, c) = frames !! row
        in if yy >= h then hex 0x404040
           else if x < w then a x yy else if x < w + 8 then hex 0x404040 else c (x - w - 8) yy
  writePng out (generateImage pick (2 * w + 8) (length beats * (h + 8)))

-- | Variant B of the 800XL layout beside the lab's picture: mode E (each
-- pixel two wide), the letters in the bitmap sharing one colour register
-- with the fabric on their rows (80 to 111: the letters' colour, dim
-- then the per-line gradient), the sparks as players (one colour each, the
-- head's), the lights as the fifth player (one colour), the core white.
-- With @cut@ (fix 1), the fabric (and its lights) is left out of the
-- letters' rows; with @solid@, mode E's letters have no groove.
a800PreviewB :: Bool -> Bool -> FilePath -> IO ()
a800PreviewB cut solid out = do
  let g = geometry a800M
      gE = if solid then geometrySolid a800M else g
      w = 320
      h = 192
      beats = [2.2, 6.1, 7.0, 9.3]
      hex c = PixelRGB8 (fromIntegral (c `div` 65536)) (fromIntegral (c `div` 256 `mod` 256)) (fromIntegral (c `mod` 256))
      ring look = case look of
        RingOff -> 0x3B2E5A
        RingOn -> 0x2CB5A8
        RingHead -> 0xE8F8F0
        RingMid -> 0x7FD8CC
        RingBg -> 0x0E0B14
      mix a c t = let ch s = round (fromIntegral ((a `div` s) `mod` 256) * (1 - t) + fromIntegral ((c `div` s) `mod` 256) * (t :: Double)) :: Int
                  in ch 65536 * 65536 + ch 256 * 256 + ch 1
      letterCol st y b
        | rsLettersLit st = let u = 0.5 + 0.5 * sin (fromIntegral y * 0.33 - b * 4) in mix 0xF2A23A 0x2CB5A8 (fromIntegral (round (u * 6) :: Int) / 6)
        | otherwise = 0x2A2340
      band y = y >= 80 && y <= 111
      roleCol st y b r
        | r >= 1 && r <= 5 = ring (rsRing st !! (fromIntegral r - 1))
        | r == roleEmb = 0xF2A23A
        | r == roleCore = 0xE8F8F0
        | r == roleLetters = letterCol st y b
        | r == roleTeal || r == roleViolet = 0x1E6E68
        | otherwise = 0x0E0B14
      sparkCol r = case r of
        _ | r `elem` [30, 31] -> 0xFFE0A0
          | r `elem` [32, 33] -> 0xB8F8F0
          | r `elem` [34, 35] -> 0xD8C8FF
          | r `elem` [roleLightB, roleLightD] -> 0xC8F8F0
          | otherwise -> 0xFFFFFF
      labCol r = case r of
        30 -> 0xFFE0A0
        31 -> 0xC78A2C
        32 -> 0xB8F8F0
        33 -> 0x22A699
        34 -> 0xD8C8FF
        35 -> 0x7A68B8
        27 -> 0xC8F8F0
        28 -> 0x2CB5A8
        _ -> 0xFFFFFF
      frame b variant =
        let buf = compose g b (WeaveAt (floor (b * 24 / 4)))
            bufE = compose gE b (WeaveAt (floor (b * 24 / 4)))
            st = roleState b
            sprPx = sparkPixels g b <> lightPixels g b (compose g 0 WeaveCost)
            spr = M.fromList [ ((spX p, spY p), spRole p) | p <- sprPx ]
            -- players and missiles: one bit per colour clock (two pixels);
            -- with fix 1 no lights in the letters' rows
            lightHere p = spRole p == roleLightB || spRole p == roleLightD
            sprE = M.fromList [ ((spX p - spX p `mod` 2, spY p), spRole p) | p <- sprPx, not (cut && lightHere p && band (spY p)) ]
        in \x y ->
             if not variant
               then case M.lookup (x, y) spr of
                      Just sr | not (gLogo g U.! (y * w + x)) -> hex (labCol sr)
                      _ -> hex (roleCol st y b (buf U.! (y * w + x)))
               else
                 let xl = x - x `mod` 2
                     pick a c = if a /= 0 then a else c
                     r = pick (bufE U.! (y * w + xl)) (bufE U.! (y * w + xl + 1))
                     logo = gLogo gE U.! (y * w + xl) || gLogo gE U.! (y * w + xl + 1)
                     fab = r == roleTeal || r == roleViolet
                     bgc | fab && band y = if cut then 0x0E0B14 else letterCol st y b
                         | otherwise = roleCol st y b r
                 in case M.lookup (xl, y) sprE of
                      Just sr | not logo -> hex (sparkCol sr)
                      _ -> hex bgc
      frames = [ (frame b False, frame b True) | b <- beats ]
      pick x y =
        let (row, yy) = y `divMod` (h + 8)
            (a, c) = frames !! row
        in if yy >= h then hex 0x404040
           else if x < w then a x yy else if x < w + 8 then hex 0x404040 else c (x - w - 8) yy
  writePng out (generateImage pick (2 * w + 8) (length beats * (h + 8)))

-- | Layout A of the 800XL beside the lab's picture: mode E (each pixel two
-- wide) for the ring, lambdas and fabric (through the letter band too);
-- the four solid letters as players (their own colours: dim, then the
-- per-line gradient); the sparks as players (one colour each, the head's)
-- outside the letter band (lines 80 to 111), as PF3 missiles inside it
-- (four pixels wide, one colour a line: the core's white on its lines,
-- else the first spark's on that line); the core and the lights as PF3
-- missiles (the light colour on lines without a spark or the core).
a800PreviewA :: FilePath -> IO ()
a800PreviewA out = do
  let g = geometry a800M
      gE = geometrySolid a800M
      w = 320
      h = 192
      beats = [1.42, 2.45, 4.05, 7.0]
      hex c = PixelRGB8 (fromIntegral (c `div` 65536)) (fromIntegral (c `div` 256 `mod` 256)) (fromIntegral (c `mod` 256))
      ring look = case look of
        RingOff -> 0x3B2E5A
        RingOn -> 0x2CB5A8
        RingHead -> 0xE8F8F0
        RingMid -> 0x7FD8CC
        RingBg -> 0x0E0B14
      mix a c t = let ch s' = round (fromIntegral ((a `div` s') `mod` 256) * (1 - t) + fromIntegral ((c `div` s') `mod` 256) * (t :: Double)) :: Int
                  in ch 65536 * 65536 + ch 256 * 256 + ch 1
      letterCol st y b
        | rsLettersLit st = let u = 0.5 + 0.5 * sin (fromIntegral y * 0.33 - b * 4) in mix 0xF2A23A 0x2CB5A8 (fromIntegral (round (u * 6) :: Int) / 6)
        | otherwise = 0x2A2340
      band y = y >= 80 && y <= 111
      coreLine y = y >= 92 && y <= 99
      roleCol st y b r
        | r >= 1 && r <= 5 = ring (rsRing st !! (fromIntegral r - 1))
        | r == roleEmb = 0xF2A23A
        | r == roleCore = 0xE8F8F0
        | r == roleLetters = letterCol st y b
        | r == roleTeal || r == roleViolet = 0x1E6E68
        | otherwise = 0x0E0B14
      sparkCol r
        | r `elem` [30, 31] = 0xFFE0A0
        | r `elem` [32, 33] = 0xB8F8F0
        | otherwise = 0xD8C8FF
      isLight r = r == roleLightB || r == roleLightD
      labCol r = case r of
        30 -> 0xFFE0A0
        31 -> 0xC78A2C
        32 -> 0xB8F8F0
        33 -> 0x22A699
        34 -> 0xD8C8FF
        35 -> 0x7A68B8
        27 -> 0xC8F8F0
        28 -> 0x2CB5A8
        _ -> 0xFFFFFF
      frame b variant =
        let buf = compose g b (WeaveAt (floor (b * 24 / 4)))
            st = roleState b
            sprPx = sparkPixels g b <> lightPixels g b (compose g 0 WeaveCost)
            spr = M.fromList [ ((spX p, spY p), spRole p) | p <- sprPx ]
            sparks = [ p | p <- sprPx, not (isLight (spRole p)) ]
            lights = [ p | p <- sprPx, isLight (spRole p) ]
            -- players: sparks outside the band, two pixels a bit
            players = M.fromList [ ((spX p - spX p `mod` 2, spY p), spRole p) | p <- sparks, not (band (spY p)) ]
            -- PF3 missiles: sparks in the band (four pixels a bit), the lights
            bandSparks = [ p | p <- sparks, band (spY p) ]
            missiles = S.fromList ([ (spX p - spX p `mod` 4, spY p) | p <- bandSparks ] <> [ (spX p - spX p `mod` 2, spY p) | p <- lights ])
            pf3 y | coreLine y = 0xE8F8F0
                  | otherwise = case [ spRole p | p <- bandSparks, spY p == y ] of
                      (r : _) -> sparkCol r
                      [] -> 0xC8F8F0
            -- the letters: players over the playfield, two pixels a bit
            letterAt x y = let xl = x - x `mod` 2 in gLetters gE U.! (y * w + xl) || gLetters gE U.! (y * w + xl + 1)
        in \x y ->
             if not variant
               then case M.lookup (x, y) spr of
                      Just sr | not (gLogo g U.! (y * w + x)) -> hex (labCol sr)
                      _ -> hex (roleCol st y b (buf U.! (y * w + x)))
               else
                 let xl = x - x `mod` 2
                     at xx = let r = buf U.! (y * w + xx) in if r == roleLetters then 0 else r
                     r = let a = at xl in if a /= 0 then a else at (xl + 1)
                     logo = r /= 0 && not (r == roleTeal || r == roleViolet)
                     inMissile = S.member (xl, y) missiles || S.member (x - x `mod` 4, y) (S.fromList [ (spX p - spX p `mod` 4, spY p) | p <- bandSparks ])
                 in if letterAt x y then hex (letterCol st y b)
                    else case M.lookup (xl, y) players of
                      Just sr | not logo -> hex (sparkCol sr)
                      _ | inMissile && not logo -> hex (pf3 y)
                        | otherwise -> hex (roleCol st y b r)
      frames = [ (frame b False, frame b True) | b <- beats ]
      pick x y =
        let (row, yy) = y `divMod` (h + 8)
            (a, c) = frames !! row
        in if yy >= h then hex 0x404040
           else if x < w then a x yy else if x < w + 8 then hex 0x404040 else c (x - w - 8) yy
  writePng out (generateImage (\x y -> pick (x `div` 2) (y `div` 2)) (2 * (2 * w + 8)) (2 * length beats * (h + 8)))

paletteRgbA :: Word8 -> (Word8, Word8, Word8)
paletteRgbA = paletteRgb

-- | The CHIP-8 demo's display after each 60 Hz frame (as rota-demos).
chip8Displays :: Int -> Int -> [U.Vector Bool]
chip8Displays ipf n = take n (map C.mDisplay (drop 1 (iterate (C.runFrames C.vipQuirks ipf (const 0) 1) (C.loadProgram chip8Program C.newMachine))))

chip8Art :: U.Vector Bool -> String
chip8Art d = unlines [ [ if d U.! (y * 64 + x) then '#' else '.' | x <- [0 .. 63] ] | y <- [0 .. 31] ]

-- | The threads' layer from the program's bands table: the union of the
-- column's bands (rows lo .. hi; lo 255: none), the disc's rows left out.
chip8Bands :: C.Machine -> U.Vector Bool
chip8Bands m = U.generate 2048 (\i -> or [ onBand v (i `mod` 64) (i `div` 64) | v <- [0 .. 2] ])
 where
  r a = fromIntegral (C.mRam m U.! a) :: Int
  bands = labelAddr chip8Code "bands"
  disc = labelAddr chip8Code "hidden"
  onBand v x y =
    let lo = r (bands + 6 * x + 2 * v)
        hi = r (bands + 6 * x + 2 * v + 1)
        top = r (disc + 2 * x)
        bot = r (disc + 2 * x + 1)
    in lo /= 255 && y >= lo && y <= hi && not (y >= top && y <= bot)

chip8Check :: Double -> Double -> Int -> IO ()
chip8Check rate fps n = go 0 (0 :: Double) (C.loadProgram chip8Program C.newMachine) M.empty
 where
  idleA = labelAddr chip8Code "idle"
  mainA = fromIntegral (labelAddr chip8Code "main")
  evA = labelAddr chip8Code "evPtr"
  -- the frame's steps, each checked when the program is back at "main"
  -- (every draw of the last step done)
  frame 0 m seen = pure (m, seen)
  frame k m seen = do
    let m' = C.step C.vipQuirks 0 m
    seen' <- if C.mPc m' /= mainA then pure seen else do
      let res = U.zipWith (/=) (C.mDisplay m') (chip8Bands m')
          -- the timeline's place and the token's state
          key = [ C.mRam m' U.! a | a <- [evA, evA + 1, idleA, idleA + 2] ]
      case M.lookup key seen of
        Just r | r == res -> pure seen
        Just r -> do
          putStrLn ("residual changed at the same timeline place " <> show key <> "; differing (x, y): " <> show (take 12 [ (i `mod` 64, i `div` 64) | i <- [0 .. 2047], r U.! i /= res U.! i ]))
          putStr (chip8Art res)
          pure (M.insert key res seen)
        Nothing -> pure (M.insert key res seen)
    frame (k - 1 :: Int) m' seen'
  go k acc m seen
    | k >= n = putStrLn ("frames " <> show n <> ": " <> show (M.size seen) <> " timeline places")
    | otherwise = do
        let acc' = acc + rate / fps
        (m', seen') <- frame (floor acc' - floor acc) m seen
        go (k + 1) acc' (C.tick60 m') seen'
