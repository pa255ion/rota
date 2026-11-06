{-|
The Game Boy demo's data (PLAN.md, side track J, step 2): "Rota.Gb"'s
pictures as the streams the ROM plays (@demos/gb/rota.asm@), and 'replay',
the exact model of what the ROM then shows.

__VRAM.__ Background tiles with LCDC bit 4 clear: index @i@ < 128 at
$9000 + 16i, the rest at $8800 + 16(i - 128). Index 0 is blank, then the
fabric slots, then the logo's tiles. Sprite tiles at $8000 (indices 0 to
127, which the background then doesn't see). Two tilemaps, $9800 and
$9C00 (LCDC bit 3 picks one), both set to the first beat's picture before
the LCD goes on.

__Each frame__ (ROM frame @k@; the first beat at 'ckLead'), in the
vertical blank: OAM by DMA from the shadow, then LCDC (display on from the
first beat: BGP $1B and sprites on; the tilemap shown), then the tilemap
writes, then the fabric's steps (each redraws the slots of three phases).
The sound, sparks and lights streams then update the APU and the shadow
OAM for the next frame.

__Two tilemaps.__ The scheduler keeps the map not shown ready for the
next different picture, so a change of the whole ring (the roll's dips,
the crash's flicker) is one LCDC write. Writes are budgeted per frame
('budgetCycles', with the costs the ROM's loops take); a frame's needed
writes always fit (a test checks), and what is left goes to the hidden
map. The idle loop shows map 0 only.

__Streams__ loop: each has its intro (ROM frames 0 to the idle start) and
an idle loop of its own period; the first loop frame writes everything
that differs from either the intro's last frame or the loop's last, so
the loop can follow both.
-}
module Rota.Gb.Rom
  ( -- * Tiles
    bgTiles
  , bgIndex
  , objTiles
    -- * Frames
  , Ctrl (..)
  , RomFrame (..)
  , romIntro
  , romLoops
  , Streams (..)
  , streams
    -- * Budget
  , budgetCycles
  , frameCost
    -- * Model
  , replay
  , gbExpected
  , runFrames
    -- * Output
  , gbRomInc
  , gbStats
  ) where

import Data.Bits (shiftL, shiftR, testBit, (.&.), (.|.))
import Data.List (foldl', sortOn)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, fromMaybe)
import qualified Data.Set as S
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Data.Word (Word8)
import Text.Printf (printf)

import Rota.Clock
import Rota.Emit.Util
import Rota.Gb
import Rota.Lab (Part (..), weaveL, weaveLevel)
import Rota.Sound (Write (..), encodeStream)
import Rota.Sound.Gb (gbStream)

-- * Tiles

nCells :: Int
nCells = 360

-- | Every background tile, by index: blank, the fabric slots (each at
-- its first level, as set up), the logo's tiles.
bgTiles :: [Tile]
bgTiles = U.replicate 64 0 : [ fabricTile s (min 2 ph) | (ph, s) <- fabricSlots ] <> S.toList logoSet

logoSet :: S.Set Tile
logoSet = S.fromList (concatMap catMaybes [ shLogo (shotAt p) | p <- [0 .. ckIdle gbClock + 71] ])

logoIndex :: M.Map Tile Word8
logoIndex = M.fromList (zip (S.toList logoSet) [fromIntegral (1 + length fabricSlots) ..])

slotIndex :: M.Map (Int, Shape) Word8
slotIndex = M.fromList (zip fabricSlots [1 ..])

-- | A cell's tile index in a picture.
bgIndex :: Cell -> Maybe Tile -> Word8
bgIndex (Fabric ph s) _ = slotIndex M.! (ph, s)
bgIndex Logo (Just t) = logoIndex M.! t
bgIndex Logo Nothing = 0

-- | The sprites' patterns, by index.
objTiles :: [Tile]
objTiles = S.toList objSet

objSet :: S.Set Tile
objSet = S.fromList [ spPat s | p <- [0 .. ckIdle gbClock + 120 * 4], s <- shSprites (shotAt p) ]

objIndex' :: M.Map Tile Word8
objIndex' = M.fromList (zip objTiles [0 ..])

-- * Frames

data Ctrl = Ctrl { ctOn :: Bool, ctMap1 :: Bool }
  deriving stock (Eq, Show)

-- | What the ROM does in one frame.
data RomFrame = RomFrame
  { rfCtrl :: Ctrl
  , rfBg :: [(Int, Int, Word8)]       -- ^ (map, cell, tile)
  , rfSteps :: Int                     -- ^ fabric steps
  , rfStepAt :: Int                    -- ^ the fabric's step before them
  , rfSparks :: [(Int, OamEntry)]      -- ^ (slot, entry)
  , rfLights :: [(Int, OamEntry)]
  , rfSound :: [Write]
  } deriving stock (Eq, Show)

-- | y, x, tile, attributes (as in OAM: y + 16, x + 8; y 0 hides).
type OamEntry = (Word8, Word8, Word8, Word8)

hidden :: OamEntry
hidden = (0, 0, 0, 0)

-- | The picture's tile indices at picture frame p.
target :: Int -> U.Vector Word8
target p = U.fromList (zipWith bgIndex cells (shLogo (shotAt (max 0 p))))

idleStart :: Int
idleStart = ckIdle gbClock

lead :: Int
lead = ckLead gbClock

-- | The sprites in OAM slots: sparks 0 to 5 (voice × head/trail), lights
-- from 6 on in a stable order.
sparkOam :: Int -> M.Map Int OamEntry
sparkOam p = M.fromList [ (slot s, entry s) | s <- shSprites (shotAt p), isSpark s ]
 where
  isSpark s = let (k, _, _, _) = spKey s in k == 0
  slot s = let (_, v, _, part) = spKey s in 2 * v + (if part == PartHead then 0 else 1)

lightSprites :: Int -> M.Map (Int, Int) OamEntry
lightSprites p = M.fromList [ ((x, y), entry s) | s <- shSprites (shotAt p), let (k, x, y, _) = spKey s, k == 1 ]

entry :: Sprite -> OamEntry
entry s
  | spX0 s <= -8 || spX0 s >= 160 || spY0 s <= -8 || spY0 s >= 144 = hidden
  | otherwise = (fromIntegral (spY0 s + 16), fromIntegral (spX0 s + 8), objIndex' M.! spPat s, if spBehind s then 0x80 else 0)

-- | Light slots: each crossing gets the lowest slot free when it lights.
lightOam :: [M.Map (Int, Int) OamEntry] -> [M.Map Int OamEntry]
lightOam = go M.empty
 where
  go _ [] = []
  go assigned (m : ms) =
    let kept = M.filterWithKey (\k _ -> M.member k m) assigned
        free = [ s | s <- [6 .. 39], s `notElem` M.elems kept ]
        new = M.fromList (zip [ k | k <- M.keys m, not (M.member k kept) ] free)
        slots = M.union kept new
    in M.fromList [ (slots M.! k, e) | (k, e) <- M.toList m ] : go slots ms

-- | Changed slots from one OAM picture to the next (slots gone are hidden).
oamDiff :: M.Map Int OamEntry -> M.Map Int OamEntry -> [(Int, OamEntry)]
oamDiff a b =
  [ (s, e) | (s, e) <- M.toList b, M.lookup s a /= Just e ]
  <> [ (s, hidden) | s <- M.keys a, not (M.member s b), a M.! s /= hidden ]

-- | The cost of a frame's vertical-blank work, in M-cycles: the ROM's
-- loops (demos/gb/rota.asm, "VBlank work"; measured with rota-profile).
frameCost :: RomFrame -> Int
frameCost f = bgCost (rfBg f) + sum [ stepCost (rfStepAt f + i) | i <- [0 .. rfSteps f - 1] ]

-- | A frame's tilemap writes as generated code: a call, then per tile
-- value @ld a, n@ (2) and per cell @ld [nn], a@ (4).
bgCost :: [(Int, Int, Word8)] -> Int
bgCost [] = 0
bgCost ws = 16 + 4 * length ws + 2 * S.size (S.fromList [ v | (_, _, v) <- ws ])

-- | The fabric's step from s to s + 1: three phases' slots, a horizontal
-- slot costing less (only its rows 3 and 4 change).
stepCost :: Int -> Int
stepCost st = 60 + sum [ if sh == SH then 34 else 94 | (_, _, sh, _) <- stepEntries (st `mod` weaveL) ]

-- | The slots a step from s to s + 1 redraws: (phase, look, shape, slot):
-- phase s + 1 becomes the pulse, s + 2 its tail, s dotted.
stepEntries :: Int -> [(Int, Int, Shape, Word8)]
stepEntries st =
  [ (ph, lv, sh, i)
  | (ph, lv) <- [((st + 1) `mod` weaveL, 0), ((st + 2) `mod` weaveL, 1), (st, 2)]
  , sh <- [SH, SV, SX]
  , Just i <- [M.lookup (ph, sh) slotIndex] ]

budgetCycles :: Int
budgetCycles = 640

-- | The intro, ROM frames 0 to the idle start - 1, with the scheduled
-- tilemap writes.
--
-- First the map each frame shows: the same as the last frame's if the
-- picture is the same; else the other map if the picture it last showed
-- is closer to the new one (the roll's dips and the crash's flicker swing
-- between two pictures), or if more than 16 cells change (the other map
-- can be prepared for it while hidden); map 0 from the last intro frame
-- on. Then each
-- map's changes are jobs: a cell's new tile is due when its map next
-- shows it, and may be written from the frame after the map was last
-- shown (a map shown on consecutive frames gets each change in its own
-- frame). Each frame takes its own due changes, then the earliest due of
-- the rest that its budget allows ('budgetCycles' after the fabric).
romIntro :: [RomFrame]
romIntro = [ RomFrame (Ctrl (k >= lead) (shown V.! k == 1)) (M.findWithDefault [] k sched) (steps k) (stepAt k) [] [] [] | k <- [0 .. n - 1] ]
 where
  t0 = target 0
  n = lead + idleStart
  targets = V.generate (idleStart + 1) target
  tgt k = targets V.! max 0 (min idleStart (k - lead))
  diffN a b = length (filter id (zipWith (/=) (U.toList a) (U.toList b)))
  -- the map each frame shows
  shown = V.fromList (pick 0 (t0, t0) 0)
  pick k (last0, last1) cur
    | k >= n = []
    | otherwise =
        let t = tgt k
            lastOf i = if i == 0 then last0 else last1
            c | k >= n - 1 = 0
              | k > 0 && t == tgt (k - 1) = cur
              | diffN (lastOf (1 - cur)) t < diffN (lastOf cur) t = 1 - cur
              | diffN (lastOf cur) t > 16 = 1 - cur  -- a big change: on the map prepared for it
              | otherwise = cur
            lasts = if c == 0 then (t, last1) else (last0, t)
        in c : pick (k + 1) lasts c
  steps k = let p = k - lead in if p >= 1 then weaveCapped gbClock p - weaveCapped gbClock (p - 1) else 0
  stepAt k = weaveCapped gbClock (k - lead - 1)
  capacity k = (budgetCycles - 16 - frameCost (RomFrame (Ctrl True False) [] (steps k) (stepAt k) [] [] [])) `div` 6
  -- the jobs: (release, due, map, cell, tile)
  jobs = concat [ mapJobs m | m <- [0, 1] ]
  mapJobs m = go (-1) t0 [ k | k <- [0 .. n - 1], shown V.! k == m ]
   where
    go _ _ [] = []
    go prevK prevT (k : ks) =
      let t = tgt k
          release = if prevK == k - 1 then k else prevK + 1
      in [ (release, k, m, c, t U.! c) | c <- [0 .. nCells - 1], t U.! c /= prevT U.! c ] <> go k t ks
  sched = edf 0 (sortOn (\(r, _, _, _, _) -> r) jobs) [] M.empty
  edf :: Int -> [(Int, Int, Int, Int, Word8)] -> [(Int, Int, Int, Int, Word8)] -> M.Map Int [(Int, Int, Word8)] -> M.Map Int [(Int, Int, Word8)]
  edf k pending ready acc
    | k >= n = if null ready && null pending then acc else error ("Rota.Gb.Rom: tilemap writes left at the idle start: " <> show (length ready + length pending))
    | otherwise =
        let (now, later) = span (\(r, _, _, _, _) -> r <= k) pending
            ready' = sortOn (\(_, d, _, _, _) -> d) (ready <> now)
            due = [ j | j@(_, d, _, _, _) <- ready', d <= k ]
            rest = [ j | j@(_, d, _, _, _) <- ready', d > k ]
            room = max 0 (capacity k - length due)
            (early, keep) = splitAt room rest
            done = due <> early
            late = [ j | j@(_, d, _, _, _) <- due, d < k ]
        in if not (null late) then error ("Rota.Gb.Rom: a tilemap write missed its frame " <> show k)
           else edf (k + 1) later keep (M.insert k [ (m, c, v) | (_, _, m, c, v) <- done ] acc)

-- | The idle loops: (tilemap writes, fabric steps, sparks, lights), each
-- of its own period, starting at the idle start.
romLoops :: ([[(Int, Int, Word8)]], [Int], [[(Int, OamEntry)]], [[(Int, OamEntry)]])
romLoops = (bgLoop, fabLoop, sparkLoop, lightLoop)
 where
  (ringP, sparkP, lightP, fabP) = idlePeriods
  i0 = idleStart
  tg q = target (i0 + q)
  bgLoop = [ [ (0, c, tg q U.! c) | c <- [0 .. nCells - 1], changed q c ] | q <- [0 .. ringP - 1] ]
  changed 0 c = tg 0 U.! c /= tg (-1) U.! c || tg 0 U.! c /= tg (ringP - 1) U.! c
  changed q c = tg q U.! c /= tg (q - 1) U.! c
  fabLoop = [ ptWeave (picTime gbClock (i0 + q)) - ptWeave (picTime gbClock (i0 + q - 1)) | q <- [0 .. fabP - 1] ]
  sp q = sparkOam (i0 + q)
  sparkLoop = [ if q == 0 then unionDiff (sp (-1)) (sp (sparkP - 1)) (sp 0) else oamDiff (sp (q - 1)) (sp q) | q <- [0 .. sparkP - 1] ]
  lightsAll = lightOam [ lightSprites p | p <- [0 .. i0 + 2 * lightP - 1] ]
  -- the loop's slots: the second period's (its slots settled), its first
  -- frame against both the intro's last and the loop's last
  li q = lightsAll !! (i0 + lightP + q)
  lightLoop = [ if q == 0 then unionDiff (lightsAll !! (i0 - 1)) (li (lightP - 1)) (li 0) else oamDiff (li (q - 1)) (li q) | q <- [0 .. lightP - 1] ]
  unionDiff a a' b = M.toList (M.fromList (oamDiff a b <> oamDiff a' b))

-- | The intro's sprite and sound records.
introSprites :: ([[(Int, OamEntry)]], [[(Int, OamEntry)]])
introSprites = (diffs sparks, diffs lights)
 where
  n = lead + idleStart
  ps = [ k - lead | k <- [0 .. n - 1] ]
  -- nothing shows before the first beat (the sprites are off anyway)
  sparks = [ if p < 0 then M.empty else sparkOam p | p <- ps ]
  lights = lightOam [ if p < 0 then M.empty else lightSprites p | p <- ps ]
  diffs ms = zipWith oamDiff (M.empty : ms) ms

data Streams = Streams
  { stBg :: [Maybe Int]       -- ^ per frame, the code routine (by number), with the ctrl byte below
  , stBgCtrl :: [Word8]
  , stBgLoop :: [Maybe Int]
  , stBgCode :: [[(Int, Int, Word8)]]
  , stFab :: [Word8]
  , stFabLoop :: [Word8]
  , stSparks :: [Word8]
  , stSparksLoop :: [Word8]
  , stLights :: [Word8]
  , stLightsLoop :: [Word8]
  , stSound :: [Word8]
  }

-- | The encoded streams. Tilemap: per frame a header (count, bit 7 map 1
-- shown, bit 6 display on), then (address high, low, tile) per write; $FF
-- jumps to the loop. Fabric: per frame the steps; $FF loops. Sprites: per
-- frame a count, then (OAM offset, y, x, tile, attributes); $FF loops.
streams :: Streams
streams = Streams
  { stBg = map (code . rfBg) romIntro
  , stBgCtrl = map (ctrlByte . rfCtrl) romIntro
  , stBgLoop = map code bgLoop
  , stBgCode = codes
  , stFab = map (fromIntegral . rfSteps) romIntro <> [0xFF]
  , stFabLoop = map fromIntegral fabLoop <> [0xFF]
  , stSparks = concatMap oamRec (fst introSprites) <> [0xFF]
  , stSparksLoop = concatMap oamRec sparkLoop <> [0xFF]
  , stLights = concatMap oamRec (snd introSprites) <> [0xFF]
  , stLightsLoop = concatMap oamRec lightLoop <> [0xFF]
  , stSound = encodeStream (fromIntegral . (.&. 0xFF)) gbStream }
 where
  (bgLoop, fabLoop, sparkLoop, lightLoop) = romLoops
  ctrlByte c = (if ctMap1 c then 0x80 else 0) .|. (if ctOn c then 0x40 else 0) :: Word8
  codes = S.toList (S.fromList (filter (not . null) (map rfBg romIntro <> bgLoop)))
  codeIx = M.fromList (zip codes [0 ..])
  code [] = Nothing
  code ws = Just (codeIx M.! ws)
  oamRec es = fromIntegral (length es) : concat [ [fromIntegral (4 * s), y, x, t, a] | (s, (y, x, t, a)) <- es ]

mapAddr :: Int -> Int -> Int
mapAddr m c = (if m == 0 then 0x9800 else 0x9C00) + (c `div` 20) * 32 + c `mod` 20

-- * Model

-- | The VRAM the ROM keeps.
data Vram = Vram
  { vMaps :: (U.Vector Word8, U.Vector Word8)
  , vCtrl :: Ctrl
  , vStep :: Int
  , vOam :: M.Map Int OamEntry
  }

-- | Every ROM frame's records, through @n@ frames (the loops repeating).
runFrames :: Int -> [RomFrame]
runFrames n = take n (romIntro' <> cycle' 0)
 where
  (sparksI, lightsI) = introSprites
  romIntro' = zipWith3 (\f s l -> f { rfSparks = s, rfLights = l }) romIntro sparksI lightsI
  (bgLoop, fabLoop, sparkLoop, lightLoop) = romLoops
  at xs q = xs !! (q `mod` length xs)
  cycle' q = RomFrame (Ctrl True False) (at bgLoop q) (at fabLoop q) (weaveCapped gbClock (idleStart + q - 1)) (at sparkLoop q) (at lightLoop q) [] : cycle' (q + 1)

-- | The screen after each ROM frame's records (DMG shades).
replay :: Int -> [U.Vector Word8]
replay n = map render (drop 1 (scanl step v0 (runFrames n)))
 where
  t0 = target 0
  v0 = Vram (t0, t0) (Ctrl False False) 0 M.empty
  step v f =
    let (m0, m1) = vMaps v
        upd i m = m U.// [ (c, x) | (j, c, x) <- rfBg f, j == i ]
    in Vram (upd 0 m0, upd 1 m1) (rfCtrl f) (vStep v + rfSteps f)
            (foldl' (\o (s, e) -> M.insert s e o) (vOam v) (rfSparks f <> rfLights f))
  tilesV = V.fromList bgTiles
  objV = V.fromList objTiles
  render v
    | not (ctOn (vCtrl v)) = U.replicate (160 * 144) 3
    | otherwise = U.generate (160 * 144) px
   where
    mp = (if ctMap1 (vCtrl v) then snd else fst) (vMaps v)
    slotLevel = M.fromList [ (i, min 2 (weaveLevel ph (vStep v))) | ((ph, _), i) <- zip fabricSlots [1 :: Word8 ..] ]
    tileOf i = case M.lookup i slotLevel of
      Just lv -> fabricTile (snd (fabricSlots !! (fromIntegral i - 1))) lv
      Nothing -> tilesV V.! fromIntegral i
    sprs = sortOn (\(s, (_, x, _, _)) -> (x, s)) [ (s, e) | (s, e@(y, _, _, _)) <- M.toList (vOam v), y /= 0 ]
    shade pal c = (pal `shiftR` (2 * fromIntegral c)) .&. 3
    px j =
      let (y, x) = j `divMod` 160
          bg = tileOf (mp U.! ((y `div` 8) * 20 + x `div` 8)) U.! ((y `mod` 8) * 8 + x `mod` 8)
          obj = [ (c, testBit a 7) | (_, (oy, ox, t, a)) <- sprs
                                   , let dx = x - (fromIntegral ox - 8)
                                         dy = y - (fromIntegral oy - 16)
                                   , dx >= 0, dx < 8, dy >= 0, dy < 8
                                   , let c = (objV V.! fromIntegral t) U.! (dy * 8 + dx), c /= 0 ]
      in case take 1 obj of
           [(c, behind)] | not (behind && bg /= 0) -> shade gbObp0 c
           _ -> shade gbBgp bg

-- | The ideal screen of each ROM frame ("Rota.Gb"'s pictures).
gbExpected :: Int -> [U.Vector Word8]
gbExpected n = [ renderShot (if k < lead then Nothing else Just (shotAt (k - lead))) | k <- [0 .. n - 1] ]

-- * Output

-- | rota_pic.inc for rgbasm.
gbRomInc :: String
gbRomInc = header ";" ["The Game Boy demo's pictures (Rota.Gb, Rota.Gb.Rom)."]
  <> "DEF ROTA_BG_TILES EQU " <> show (length bgTiles) <> "\n"
  <> "DEF ROTA_OBJ_TILES EQU " <> show (length objTiles) <> "\n"
  <> "DEF ROTA_STEP0 EQU " <> show (ptWeave (picTime gbClock 0) `mod` weaveL) <> "\n"
  <> "RotaBgTiles::\n" <> rgbdsBytes (concatMap tile2bpp bgTiles)
  <> "RotaObjTiles::\n" <> rgbdsBytes (concatMap tile2bpp objTiles)
  <> "; the first beat's tilemap, 20 x 18\nRotaMap::\n" <> rgbdsBytes (U.toList (target 0))
  <> "SECTION \"rota fabric\", ROM0, ALIGN[8]\n; the fabric's looks: (shape H, V, X) x (pulse, tail, dotted)\nRotaFabric::\n"
  <> rgbdsBytes (concat [ tile2bpp (fabricTile sh lv) | sh <- [SH, SV, SX], lv <- [0, 1, 2] ])
  <> "SECTION \"rota steps\", ROM0, ALIGN[8]\n; per step s (0 to 15), the slots its move to s + 1 redraws: a count of\n; horizontal slots (rows 3 and 4 only), their (slot, look) addresses,\n; then a count of whole slots and theirs\nRotaSteps::\n"
  <> concat [ "    dw RotaStep" <> show st <> "\n" | st <- [0 .. weaveL - 1] ]
  <> concat [ "RotaStep" <> show st <> ":\n" <> list hs 6 <> list ws 0
            | st <- [0 .. weaveL - 1]
            , let es = stepEntries st
                  hs = [ e | e@(_, _, SH, _) <- es ]
                  ws = [ e | e@(_, _, sh, _) <- es, sh /= SH ] ]
  <> "SECTION \"rota streams\", ROM0\n"
  <> "; the tilemap: per frame (ctrl: bit 7 map 1 shown, bit 6 display on;\n; the frame's writes as code, or 0); ctrl $FF jumps to the loop\nRotaBg::\n"
  <> concat [ "    db $" <> printf "%02X" c <> "\n    dw " <> maybe "0" codeName r <> "\n" | (c, r) <- zip (stBgCtrl streams) (stBg streams) ]
  <> "    db $FF\nRotaBgLoop::\n"
  <> concat [ "    db $40\n    dw " <> maybe "0" codeName r <> "\n" | r <- stBgLoop streams ]
  <> "    db $FF\n"
  <> concat [ codeName i <> ":\n" <> concat [ "    ld a, $" <> printf "%02X" v <> "\n" <> concat [ "    ld [$" <> hex4 (mapAddr m c) <> "], a\n" | (m, c, _) <- grp ] | grp <- byValue ws, let (_, _, v) = head grp ] <> "    ret\n"
            | (i, ws) <- zip [0 :: Int ..] (stBgCode streams) ]
  <> stream "RotaFab" stFab <> stream "RotaFabLoop" stFabLoop
  <> stream "RotaSparks" stSparks <> stream "RotaSparksLoop" stSparksLoop
  <> stream "RotaLights" stLights <> stream "RotaLightsLoop" stLightsLoop
 where
  stream name f = name <> "::\n" <> rgbdsBytes (f streams)
  hex4 a = printf "%04X" a :: String
  codeName i = "RotaBgCode" <> show i
  byValue ws = M.elems (M.fromListWith (flip (<>)) [ (v, [w']) | w'@(_, _, v) <- ws ])
  list es off = "    dw " <> show (length es) <> "\n"
    <> concat [ "    dw $" <> hex4 (slotAddr i + off) <> ", RotaFabric + " <> show ((fromEnum sh * 3 + lv) * 16 + off) <> "\n" | (_, lv, sh, i) <- es ]
  slotAddr i = if i < 128 then 0x9000 + 16 * fromIntegral i else 0x8800 + 16 * (fromIntegral i - 128) :: Int

-- | 2bpp: per row, the low bit plane then the high.
tile2bpp :: Tile -> [Word8]
tile2bpp t = concat [ [plane 0 r, plane 1 r] | r <- [0 .. 7] ]
 where
  plane b r = foldl' (\acc x -> (acc `shiftL` 1) .|. (if testBit (t U.! (r * 8 + x)) b then 1 else 0)) 0 [0 .. 7 :: Int]

gbStats :: String
gbStats = unlines
  [ printf "background tiles %d (blank, %d fabric slots, %d logo); sprite tiles %d" (length bgTiles) (length fabricSlots) (S.size logoSet) (length objTiles)
  , printf "most vertical-blank cycles %d of %d (frame %d); most tilemap writes %d" worst budgetCycles worstAt (maximum (map (length . rfBg) romIntro))
  , printf "frames needing more than the budget: %s" (show [ k | (k, f) <- zip [0 :: Int ..] romIntro, needCost k f > budgetCycles ])
  , printf "stream bytes: tilemap %d + %d, fabric %d + %d, sparks %d + %d, lights %d + %d, sound %d"
      (3 * length (stBg s) + sum [ 4 * length ws + 3 | ws <- stBgCode s ]) (3 * length (stBgLoop s)) (length (stFab s)) (length (stFabLoop s)) (length (stSparks s)) (length (stSparksLoop s))
      (length (stLights s)) (length (stLightsLoop s)) (length (stSound s))
  , printf "tilemap code routines %d" (length (stBgCode s))
  , printf "map switches %d" (length (filter id (zipWith (/=) (map (ctMap1 . rfCtrl) romIntro) (drop 1 (map (ctMap1 . rfCtrl) romIntro)))))
  ]
 where
  s = streams
  costs = map frameCost romIntro
  worst = maximum costs
  worstAt = fromMaybe 0 (lookup worst (zip costs [0 :: Int ..]))
  needCost _ f = frameCost f { rfBg = [ x | x@(m, _, _) <- rfBg f, m == (if ctMap1 (rfCtrl f) then 1 else 0) ] }
