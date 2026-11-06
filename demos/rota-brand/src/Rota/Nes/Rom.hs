{-|
The NES demo's data (PLAN.md, side track J, step 3): "Rota.Nes"'s
pictures as the streams the ROM plays (@demos/nes/rota.s@), and 'replay',
the exact model of what the ROM then shows.

__Two layouts__ (the user, 2026-10-07: a toggle, one table by default):

* 'OneTable': background tiles in pattern table 0 (index 0 blank, then
  the fabric slots, then the logo's tiles), sprite tiles in table 1.
* 'Split': the design's mid-screen split. The top half's tiles in table 0,
  the bottom half's (from tile row 15) in table 1 with the sprites; both
  start with the blank tile and the fabric slots, so every fabric step
  writes both tables. Sprite 0 is a dot behind a fabric pixel that is
  always drawn ((8, 115)); its hit tells the ROM to switch the background
  to table 1 for the bottom half.

One nametable ($2000) and its attributes, set before rendering starts.

__Each frame__ (ROM frame @k@; the first beat at 'ckLead'), in the NMI:
OAM by DMA (the shadow holds frame k's sprites), the palette if it
changed, the nametable writes (the lambdas' reveal), the fabric's step
(generated code per step: three phases' slots). Rendering is off in the
lead-in (the backdrop, $0F, shows). Then the sound, and the sparks' and
lights' records of frame k + 1 into the shadow.

__Streams__ loop as the Game Boy's ("Rota.Gb.Rom").
-}
module Rota.Nes.Rom
  ( Layout (..)
  , bgTiles
  , objTiles
  , RomFrame (..)
  , romIntro
  , runFrames
  , nmiCost
  , nmiBudget
  , replay
  , nesExpected
  , nesRomInc
  , nesStats
  ) where

import Data.Bits (shiftL, shiftR, testBit, (.&.), (.|.))
import Data.List (foldl')
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, isJust)
import qualified Data.Set as S
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Data.Word (Word16, Word8)
import Text.Printf (printf)

import Rota.Clock
import Rota.Emit.Util
import Rota.Lab (Part (..), Shape (..), weaveL, weaveLevel)
import Rota.Nes

data Layout = OneTable | Split
  deriving stock (Eq, Show)

-- * Tiles

nCells :: Int
nCells = 960

-- | The first tile row of the bottom half ('Split').
splitRow :: Int
splitRow = 15

blank :: Tile
blank = U.replicate 64 0

fabricSlots :: [(Int, Shape)]
fabricSlots = M.keys (M.fromList [ ((ph, sh), ()) | Fabric ph sh <- cells ])

slotIndex :: M.Map (Int, Shape) Word8
slotIndex = M.fromList (zip fabricSlots [1 ..])

-- | Every logo tile, by the half it shows in.
logoTiles :: (S.Set Tile, S.Set Tile)
logoTiles = (half (< splitRow), half (>= splitRow))
 where
  shots = [ shLogo (shotAt p) | p <- [0 .. ckIdle nesClock] ]
  half inHalf = S.delete blank (S.fromList (concat [ catMaybes [ t | (i, t) <- zip [0 :: Int ..] sh, inHalf (i `div` 32) ] | sh <- shots ]))

-- | The logo tiles of a table: 0 (the top half, or all) or 1.
logoSet :: Layout -> Int -> S.Set Tile
logoSet OneTable 0 = uncurry S.union logoTiles
logoSet OneTable _ = S.empty
logoSet Split 0 = fst logoTiles
logoSet Split _ = snd logoTiles

-- | A table's background tiles: blank, the fabric slots (at the first
-- beat's levels), the logo's tiles ('OneTable''s table 1 has none).
bgTiles :: Layout -> Int -> [Tile]
bgTiles OneTable 1 = []
bgTiles l t = blank : [ fabricTile s (min 2 ph) | (ph, s) <- fabricSlots ] <> S.toList (logoSet l t)

logoIndex :: Layout -> Int -> M.Map Tile Word8
logoIndex l t = M.insert blank 0 (M.fromList (zip (S.toList (logoSet l t)) [fromIntegral (1 + length fabricSlots) ..]))

-- | The dot sprite 0 is ('Split').
dot :: Tile
dot = U.replicate 64 0 U.// [(0, 1)]

objSet :: S.Set Tile
objSet = S.fromList [ spPat s | p <- [0 .. ckIdle nesClock + 480], s <- shSprites (shotAt p) ]

-- | The sprite tiles, in table 1 after its background tiles.
objTiles :: Layout -> [Tile]
objTiles OneTable = S.toList objSet
objTiles Split = S.toList objSet <> [dot]

objBase :: Layout -> Int
objBase l = length (bgTiles l 1)

objIx :: Layout -> M.Map Tile Word8
objIx l = M.fromList (zip (objTiles l) [fromIntegral (objBase l) ..])

-- | A cell's tile index (in its half's table).
cellIndex :: Layout -> Int -> Cell -> Maybe Tile -> Word8
cellIndex _ _ (Fabric ph s) _ = slotIndex M.! (ph, s)
cellIndex l row Logo (Just t) = logoIndex l (tableOf l row) M.! t
cellIndex _ _ Logo Nothing = 0

tableOf :: Layout -> Int -> Int
tableOf Split row | row >= splitRow = 1
tableOf _ _ = 0

-- * Frames

-- | y, tile, attributes, x (as in OAM; y $FF hides).
type OamEntry = (Word8, Word8, Word8, Word8)

hidden :: OamEntry
hidden = (0xFF, 0, 0, 0)

data RomFrame = RomFrame
  { rfOn :: Bool
  , rfPalette :: Maybe [Word8]       -- ^ 32 bytes from $3F00
  , rfNt :: [(Int, Word8)]           -- ^ (cell, tile)
  , rfSteps :: Int
  , rfStepAt :: Int
  , rfSparks :: [(Int, OamEntry)]
  , rfLights :: [(Int, OamEntry)]
  } deriving stock (Eq, Show)

lead, idleStart :: Int
lead = ckLead nesClock
idleStart = ckIdle nesClock

nametable :: Layout -> Int -> U.Vector Word8
nametable l p = U.fromList [ cellIndex l (i `div` 32) c t | (i, c, t) <- zip3 [0 ..] cells (shLogo (shotAt (max 0 p))) ]

paletteAt :: Int -> [Word8]
paletteAt p = concat [ 0x0F : c | c <- shBg (shotAt p) ] <> concat [ 0x0F : c | c <- objColours ]

entry :: Layout -> Sprite -> OamEntry
entry l s = (fromIntegral (spY0 s - 1), objIx l M.! spPat s, fromIntegral (spPal s) .|. (if spBehind s then 0x20 else 0), fromIntegral (spX0 s))

-- | Sprite 0 for the split: the dot at (8, 115), behind the background.
sprite0 :: Layout -> OamEntry
sprite0 l = case cells !! (14 * 32 + 1) of
  Fabric _ sh | sh /= SV -> (114, objIx l M.! dot, 0x20, 8)  -- row 3 of a horizontal trace: (8, 115) drawn at every level
  c -> error ("Rota.Nes.Rom: sprite 0's cell is " <> show c)

-- | The first OAM slot the sparks take (after sprite 0 for the split).
slot0 :: Layout -> Int
slot0 OneTable = 0
slot0 Split = 1

sparkOam :: Layout -> Int -> M.Map Int OamEntry
sparkOam l p = M.fromList [ (slot0 l + 2 * v + (if part == PartHead then 0 else 1), entry l s) | s <- shSprites (shotAt p), let (k, v, _, part) = spKey s, k == 0 ]

lightSprites :: Layout -> Int -> M.Map (Int, Int) OamEntry
lightSprites l p = M.fromList [ ((x, y), entry l s) | s <- shSprites (shotAt p), let (k, x, y, _) = spKey s, k == 1 ]

-- | Light slots: each crossing gets the lowest slot free when it lights.
lightOam :: Layout -> [M.Map (Int, Int) OamEntry] -> [M.Map Int OamEntry]
lightOam l = go M.empty
 where
  go _ [] = []
  go assigned (m : ms) =
    let kept = M.filterWithKey (\k _ -> M.member k m) assigned
        free = [ s | s <- [slot0 l + 6 .. 63], s `notElem` M.elems kept ]
        new = M.fromList (zip [ k | k <- M.keys m, not (M.member k kept) ] free)
        slots = M.union kept new
    in M.fromList [ (slots M.! k, e) | (k, e) <- M.toList m ] : go slots ms

oamDiff :: M.Map Int OamEntry -> M.Map Int OamEntry -> [(Int, OamEntry)]
oamDiff a b =
  [ (s, e) | (s, e) <- M.toList b, M.lookup s a /= Just e ]
  <> [ (s, hidden) | s <- M.keys a, not (M.member s b), a M.! s /= hidden ]

unionDiff :: M.Map Int OamEntry -> M.Map Int OamEntry -> M.Map Int OamEntry -> [(Int, OamEntry)]
unionDiff a a' b = M.toList (M.fromList (oamDiff a b <> oamDiff a' b))

steps :: Int -> Int
steps k = let p = k - lead in if p >= 1 then weaveCapped nesClock p - weaveCapped nesClock (p - 1) else 0

stepAt :: Int -> Int
stepAt k = weaveCapped nesClock (k - lead - 1)

-- | The intro, ROM frames 0 to the idle start - 1.
romIntro :: Layout -> [RomFrame]
romIntro l = zipWith3 mk [0 ..] sparks lights
 where
  n = lead + idleStart
  ps = [ k - lead | k <- [0 .. n - 1] ]
  nts = V.fromList [ nametable l p | p <- [0 .. idleStart] ]
  pals = V.fromList [ paletteAt p | p <- [0 .. idleStart] ]
  sparks = diffs [ if p < 0 then M.empty else sparkOam l p | p <- ps ]
  lights = diffs (lightOam l [ if p < 0 then M.empty else lightSprites l p | p <- ps ])
  diffs ms = zipWith oamDiff (M.empty : ms) ms
  mk k sp li =
    let p = k - lead
        pal | p < 0 = Nothing
            | p == 0 = Just (pals V.! 0)
            | pals V.! p /= pals V.! (p - 1) = Just (pals V.! p)
            | otherwise = Nothing
        nt | p >= 1 = [ (c, nts V.! p U.! c) | c <- [0 .. nCells - 1], nts V.! p U.! c /= nts V.! (p - 1) U.! c ]
           | otherwise = []
    in RomFrame (p >= 0) pal nt (steps k) (stepAt k) sp li

-- | The idle loops: (palettes, fabric steps, sparks, lights).
romLoops :: Layout -> ([Maybe [Word8]], [Int], [[(Int, OamEntry)]], [[(Int, OamEntry)]])
romLoops l = (palLoop, fabLoop, sparkLoop, lightLoop)
 where
  (ringP, sparkP, lightP, fabP) = idlePeriods
  i0 = idleStart
  palLoop = [ if q == 0 || paletteAt (i0 + q) /= paletteAt (i0 + q - 1) then Just (paletteAt (i0 + q)) else Nothing | q <- [0 .. ringP - 1] ]
  fabLoop = [ weaveCapped nesClock (i0 + q) - weaveCapped nesClock (i0 + q - 1) | q <- [0 .. fabP - 1] ]
  sp q = sparkOam l (i0 + q)
  sparkLoop = [ if q == 0 then unionDiff (sp (-1)) (sp (sparkP - 1)) (sp 0) else oamDiff (sp (q - 1)) (sp q) | q <- [0 .. sparkP - 1] ]
  lightsAll = lightOam l [ lightSprites l p | p <- [0 .. i0 + 2 * lightP - 1] ]
  li q = lightsAll !! (i0 + lightP + q)
  lightLoop = [ if q == 0 then unionDiff (lightsAll !! (i0 - 1)) (li (lightP - 1)) (li 0) else oamDiff (li (q - 1)) (li q) | q <- [0 .. lightP - 1] ]

-- | Every ROM frame's records through @n@ frames (the loops repeating).
runFrames :: Layout -> Int -> [RomFrame]
runFrames l n = take n (romIntro l <> loopFrom 0)
 where
  (palLoop, fabLoop, sparkLoop, lightLoop) = romLoops l
  at xs q = xs !! (q `mod` length xs)
  loopFrom q = RomFrame True (at palLoop q) [] (at fabLoop q) (weaveCapped nesClock (idleStart + q - 1)) (at sparkLoop q) (at lightLoop q) : loopFrom (q + 1)

-- * Budget

-- | CPU cycles of the NMI's work after OAM DMA (demos/nes/rota.s, "NMI"):
-- the palette 270, a nametable write 38, a step's generated code 15 an
-- address, 4 a byte and 2 a load (a byte the same as the last needs none),
-- in each table it writes.
nmiCost :: Layout -> RomFrame -> Int
nmiCost l f = maybe 0 (const 270) (rfPalette f) + 38 * length (rfNt f) + sum [ stepCost l (rfStepAt f + i) | i <- [0 .. rfSteps f - 1] ]

stepCost :: Layout -> Int -> Int
stepCost l st = 16 + tables * sum [ 15 + 4 * length bs + 2 * loads bs | (_, bs) <- stepRuns (st `mod` weaveL) ]
 where
  tables = if l == Split then 2 else 1
  loads bs = length [ () | (i, b) <- zip [0 :: Int ..] bs, i == 0 || bs !! (i - 1) /= b ]

-- | A step's writes in pattern table 0: (PPU address, bytes) runs (a
-- horizontal slot only its rows 3 and 4 in each plane). The code takes
-- the table's address high bits from @tableHi@, so the split runs it
-- once a table.
stepRuns :: Int -> [(Int, [Word8])]
stepRuns st =
  [ (16 * fromIntegral i + o, take n (drop o chr))
  | (_, lv, sh, i) <- stepEntries st
  , let chr = tileChr (fabricTile sh lv)
  , (o, n) <- if sh == SH then [(3, 2), (11, 2)] else [(0, 16)] ]

-- | The vertical blank is 2273 cycles: the NMI's entry, its pushes and
-- OAM DMA take about 545 of them, the scroll, PPUCTRL and PPUMASK writes
-- at its end 23; 25 are kept in hand.
nmiBudget :: Int
nmiBudget = 1680

-- | The slots a step from s to s + 1 redraws: (phase, level, shape, slot).
stepEntries :: Int -> [(Int, Int, Shape, Word8)]
stepEntries st =
  [ (ph, lv, sh, i)
  | (ph, lv) <- [((st + 1) `mod` weaveL, 0), ((st + 2) `mod` weaveL, 1), (st, 2)]
  , sh <- [SH, SV, SX]
  , Just i <- [M.lookup (ph, sh) slotIndex] ]

-- * Model

data Ppu = Ppu
  { pNt :: U.Vector Word8
  , pPal :: [Word8]
  , pOn :: Bool
  , pStep :: Int
  , pOam :: M.Map Int OamEntry
  }

-- | The PPU's picture after each ROM frame's records.
replay :: Layout -> Int -> [U.Vector Word16]
replay l n = map render (drop 1 (scanl step p0 (runFrames l n)))
 where
  oam0 = if l == Split then M.singleton 0 (sprite0 l) else M.empty
  p0 = Ppu (nametable l 0) (replicate 32 0x0F) False 0 oam0
  step s f = Ppu (pNt s U.// rfNt f) (maybe (pPal s) id (rfPalette f)) (rfOn f) (pStep s + rfSteps f)
                 (foldl' (\o (k, e) -> M.insert k e o) (pOam s) (rfSparks f <> rfLights f))
  tables = V.fromList [ V.fromList (bgTiles l 0), V.fromList (bgTiles l 1 <> objTiles l) ]
  objV = V.fromList (replicate (objBase l) blank <> objTiles l)
  slotShape = M.fromList [ (i, (ph, sh)) | ((ph, sh), i) <- M.toList slotIndex ]
  render s
    | not (pOn s) = U.replicate (256 * 240) 0x0F
    | otherwise = U.generate (256 * 240) px
   where
    tileOf t i = case M.lookup i slotShape of
      Just (ph, sh) -> fabricTile sh (min 2 (weaveLevel ph (pStep s)))
      Nothing -> (tables V.! t) V.! fromIntegral i
    pal = V.fromList (pPal s)
    sprs = [ e | (_, e@(y, _, _, _)) <- M.toList (pOam s), y < 0xEF ]
    px j =
      let (y, x) = j `divMod` 256
          row = y `div` 8
          bg = tileOf (tableOf l row) (pNt s U.! (row * 32 + x `div` 8)) U.! ((y `mod` 8) * 8 + x `mod` 8)
          area = areaPalette U.! ((y `div` 16) * 16 + x `div` 16)
          bgCol = if bg == 0 then pal V.! 0 else pal V.! (4 * area + fromIntegral bg)
          obj = [ (c, a) | (oy, t, a, ox) <- sprs
                         , let dx = x - fromIntegral ox
                               dy = y - (fromIntegral oy + 1)
                         , dx >= 0, dx < 8, dy >= 0, dy < 8
                         , let c = (objV V.! fromIntegral t) U.! (dy * 8 + dx), c /= 0 ]
      in fromIntegral $ case take 1 obj of
           [(c, a)] | not (testBit a 5 && bg /= 0) -> pal V.! (16 + 4 * fromIntegral (a .&. 3) + fromIntegral c)
           _ -> bgCol

-- | The ideal picture of each ROM frame ("Rota.Nes").
nesExpected :: Int -> [U.Vector Word16]
nesExpected n = [ renderShot (if k < lead then Nothing else Just (shotAt (k - lead))) | k <- [0 .. n - 1] ]

-- * Output

-- | 2bpp in NES order: the low plane's 8 rows, then the high plane's.
tileChr :: Tile -> [Word8]
tileChr t = [ plane 0 r | r <- [0 .. 7] ] <> [ plane 1 r | r <- [0 .. 7] ]
 where
  plane b r = foldl' (\acc x -> (acc `shiftL` 1) .|. (if testBit (t U.! (r * 8 + x)) b then 1 else 0)) 0 [0 .. 7 :: Int]

data Streams = Streams
  { stBg :: [Word8], stBgLoop :: [Word8]
  , stFab :: [Word8], stFabLoop :: [Word8]
  , stSparks :: [Word8], stSparksLoop :: [Word8]
  , stLights :: [Word8], stLightsLoop :: [Word8] }

-- | Background: per frame a flags byte (bit 7 rendering on, bit 6 a
-- palette follows), the 32 palette bytes if so, a count and (address
-- high, low, tile) per nametable write; $FF jumps to the loop. Fabric: per
-- frame the steps. Sprites: a count, then (OAM offset, y, tile,
-- attributes, x); $FF loops.
streams :: Layout -> Streams
streams l = Streams
  { stBg = concatMap bgRec intro <> [0xFF]
  , stBgLoop = concatMap (\pal -> bgRec (RomFrame True pal [] 0 0 [] [])) palLoop <> [0xFF]
  , stFab = map (fromIntegral . rfSteps) intro <> [0xFF]
  , stFabLoop = map fromIntegral fabLoop <> [0xFF]
  , stSparks = concatMap (oamRec . rfSparks) intro <> [0xFF]
  , stSparksLoop = concatMap oamRec sparkLoop <> [0xFF]
  , stLights = concatMap (oamRec . rfLights) intro <> [0xFF]
  , stLightsLoop = concatMap oamRec lightLoop <> [0xFF] }
 where
  intro = romIntro l
  (palLoop, fabLoop, sparkLoop, lightLoop) = romLoops l
  bgRec f = ((if rfOn f then 0x80 else 0) .|. (if isJust (rfPalette f) then 0x40 else 0))
    : maybe [] id (rfPalette f)
    <> [fromIntegral (length (rfNt f))]
    <> concat [ [fromIntegral (a `shiftR` 8), fromIntegral (a .&. 0xFF), v] | (c, v) <- rfNt f, let a = 0x2000 + c ]
  oamRec es = fromIntegral (length es) : concat [ [fromIntegral (4 * s), y, t, a, x] | (s, (y, t, a, x)) <- es ]

-- | rota_pic.inc (or rota_pic_split.inc) for ca65.
nesRomInc :: Layout -> String
nesRomInc l = header ";" ["The NES demo's pictures (Rota.Nes, Rota.Nes.Rom), layout " <> show l <> "."]
  <> "ROTA_BG0_TILES = " <> show (length (bgTiles l 0)) <> "\nROTA_TABLE1_TILES = " <> show (length (bgTiles l 1) + length (objTiles l)) <> "\n"
  <> ".segment \"RODATA\"\n"
  <> "RotaTable0:\n" <> ca65Bytes (concatMap tileChr (bgTiles l 0))
  <> "RotaTable1:\n" <> ca65Bytes (concatMap tileChr (bgTiles l 1 <> objTiles l))
  <> "; the first beat's nametable, then its attributes\nRotaNametable:\n" <> ca65Bytes (U.toList (nametable l 0)) <> ca65Bytes attributes
  <> "; sprite 0 (y, tile, attributes, x)\nRotaSprite0:\n" <> ca65Bytes (if l == Split then let (a, b, c, d) = sprite0 l in [a, b, c, d] else [0xFF, 0, 0, 0])
  <> ".segment \"BANK0\"\n"
  <> stream "RotaBg" stBg <> stream "RotaBgLoop" stBgLoop
  <> stream "RotaFab" stFab <> stream "RotaFabLoop" stFabLoop
  <> ".segment \"RODATA\"\n"
  <> stream "RotaSparks" stSparks <> stream "RotaSparksLoop" stSparksLoop
  <> ".segment \"BANK0\"\n"
  <> stream "RotaLights" stLights <> stream "RotaLightsLoop" stLightsLoop
  <> "; per step s (0 to 15), the code redrawing the slots of its move to s + 1\nRotaStepsLo:\n    .byte " <> commas [ "<RotaStep" <> show st | st <- [0 .. weaveL - 1] ]
  <> "\nRotaStepsHi:\n    .byte " <> commas [ ">RotaStep" <> show st | st <- [0 .. weaveL - 1] ] <> "\n"
  <> concat [ "RotaStep" <> show st <> ":\n" <> stepCode st <> "    rts\n" | st <- [0 .. weaveL - 1] ]
 where
  ss = streams l
  stream name f = name <> ":\n" <> ca65Bytes (f ss)
  commas = foldr1 (\a b -> a <> ", " <> b)
  attributes = [ fromIntegral (q 0 0 .|. (q 1 0 `shiftL` 2) .|. (q 0 1 `shiftL` 4) .|. (q 1 1 `shiftL` 6)) :: Word8
               | ay <- [0 .. 7], ax <- [0 .. 7]
               , let q dx dy = let y = 2 * ay + dy; x = 2 * ax + dx in if y < 15 then areaPalette U.! (y * 16 + x) else 0 ]
  stepCode st = concat [ addr a <> concat [ (if i == 0 || bs !! (i - 1) /= b then "    lda #$" <> printf "%02X" b <> "\n" else "") <> "    sta PPUDATA\n"
                                         | (i, b) <- zip [0 :: Int ..] bs ]
                        | (a, bs) <- stepRuns st ]
  addr a = "    lda tableHi\n    ora #$" <> printf "%02X" (a `shiftR` 8) <> "\n    sta PPUADDR\n    lda #$" <> printf "%02X" (a .&. 0xFF) <> "\n    sta PPUADDR\n"

nesStats :: String
nesStats = concat [ stats l | l <- [OneTable, Split] ]
 where
  stats l = unlines
    [ printf "%s: table 0 %d tiles, table 1 %d (sprites %d); %d fabric slots" (show l) (length (bgTiles l 0)) (length (bgTiles l 1) + length (objTiles l)) (length (objTiles l)) (length fabricSlots)
    , printf "  most NMI cycles after DMA %d of %d; most nametable writes %d" (maximum (map (nmiCost l) fs)) nmiBudget (maximum (map (length . rfNt) fs))
    , printf "  stream bytes: background %d + %d, fabric %d + %d, sparks %d + %d, lights %d + %d"
        (length (stBg s)) (length (stBgLoop s)) (length (stFab s)) (length (stFabLoop s)) (length (stSparks s)) (length (stSparksLoop s)) (length (stLights s)) (length (stLightsLoop s)) ]
   where
    s = streams l
    fs = runFrames l (lead + idleStart + 480)
