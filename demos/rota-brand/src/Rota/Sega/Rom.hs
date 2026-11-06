{-|
A Sega demo's data ("Rota.Sega"'s pictures as the streams the ROM plays:
@demos/gg/rota.s@, @demos/sms/rota.s@, WLA-DX), for either machine
('RomTarget'). (Rota.Gg.Rom's code, made a target's in M6; the Game Gear's
emitted data is unchanged.)

__VRAM.__ Background tiles at 32 i: 0 blank, then the fabric slots, then
one tile per logo cell; sprite patterns at tiles 256 + i (register 6 bit
2). The name table at 3800h (the Game Gear's window, 20 × 18 cells at
columns 6 to 25 and rows 3 to 20; the Master System's whole 32 × 24), set
once: a logo cell's entry has the priority bit. The sprite table at 3F00h.
Before the display goes on (the first beat) the ROM uploads every tile as
picture frame 0 has it, the patterns, and the name table.

__Each frame__ (ROM frame @k@, from the frame interrupt at line 193 to the
picture's first line: the Game Gear's window at 24, the Master System's
line 0 of the next frame), the ROM plays one record of each stream: the
sound (bytes to port 7Fh), the colours (CRAM writes, and the display's
register when it goes on), the tiles (32 bytes from the tile library to a
tile), the sparks' and the lights' sprite entries. A record is a count and
that many operations; a count of FEh and an address jump to the stream's
idle loop, FFh ends a stream. The streams' idle loops have their own
periods ("Rota.Clock", 'idlePeriods': colours 72 frames, sparks 96, lights
120, tiles 64): the loop's first frame writes what differs from either the
intro's last frame or the loop's last.

Every record is the difference between consecutive pictures, so the ROM
shows exactly 'expected' as long as a frame's work ends before the
picture's first line ('frameCost' against 'rtBudget', checked by the
tests).
-}
module Rota.Sega.Rom
  ( RomSpec (..)
  , RomTarget
  , romTarget
  , rSpec
    -- * Tiles
  , bgTiles
  , cellTile
  , spritePatterns
    -- * Frames
  , TileOp
  , CramOp (..)
  , SatOp
  , frameCost
  , romFrameCosts
    -- * Model
  , expected
    -- * Output
  , romInc
  , romStats
    -- * For other machines' emitters (M7: "Rota.Snes.Rom", the same
    -- records in its own formats)
  , Records (..)
  , tileRecs
  , cramRecs
  , sparkRecs
  , lightRecs
  , library
  , libIndex
  , sparkSlots
  , lightSlots
  ) where

import Data.Bits (shiftL, testBit, (.|.))
import Data.List (nub, sortOn)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import qualified Data.Vector.Unboxed as U
import Data.Word (Word8)
import Text.Printf (printf)

import Rota.Clock
import Rota.Lab (weaveLevel)
import Rota.Sega

-- | A machine's ROM: its pictures ('Target'), the sound stream's bytes,
-- the T-states a frame's work may take, a colour RAM write as its record's
-- three bytes (from the colour's index), the sprite palette's bytes in
-- colour RAM, a sprite's (Y, X) from its top-left pixel (Nothing: off the
-- screen), whether the sprite patterns come first in VRAM (tiles 0 on,
-- register 6 bit 2 clear, the background tiles after them: the Master
-- System's background needs more than 256 tiles) or at tiles 256 on (the
-- Game Gear), and the include file's first lines and its name table
-- comment.
data RomSpec = RomSpec
  { rtPatternsFirst :: Bool
  , rtTarget    :: Target
  , rtSound     :: [Word8]
  , rtBudget    :: Int
  , rtCramOp    :: Int -> Rgb12 -> (Word8, Word8, Word8)
  , rtCramBytes :: Rgb12 -> [Word8]
  , rtSpritePos :: Int -> Int -> Maybe (Word8, Word8)
  , rtHeader    :: String
  , rtNameNote  :: String
  }

-- | A spec with what follows from it, made once ('romTarget': a target
-- is a top-level value, so its library and indices are shared as
-- Rota.Gg.Rom's were).
data RomTarget = RomTarget
  { rSpec     :: RomSpec
  , rCellTile :: [Int]
  , rPatterns :: [Tile]
  , rPatIdx   :: M.Map Tile Int
  , rLibrary  :: [Tile]
  , rLibIdx   :: M.Map Tile Int
  , rSparkN   :: Int
  , rLightN   :: Int
  }

romTarget :: RomSpec -> RomTarget
romTarget sp = check r
 where
  r = RomTarget sp (map (+ base) (cellTile0 r)) pats (M.fromList (zip pats [0 ..])) lib (M.fromList (zip lib [0 ..])) (sparkSlots0 r) (lightSlots0 r)
  pats = spritePatterns0 r
  lib = library0 r
  base = bgBase r
  nBg = 1 + length (fabricSlots (rtTarget sp)) + length [ () | Logo <- cells (rtTarget sp) ]
  check x
    | rtPatternsFirst sp && length pats + nBg > 448 = error "romTarget: more than 448 tiles"
    | not (rtPatternsFirst sp) && (nBg > 256 || length pats > 192) = error "romTarget: the background over 256 tiles, or the patterns over the name table"
    | otherwise = x

-- | The first background tile (the blank one).
bgBase :: RomTarget -> Int
bgBase r = if rtPatternsFirst (rSpec r) then length (rPatterns r) else 0

lead, idleStart :: RomTarget -> Int
lead = ckLead . tClock . rtTarget . rSpec
idleStart = ckIdle . tClock . rtTarget . rSpec

-- | Picture frames that cover the intro and every idle loop's first period.
horizon :: RomTarget -> Int
horizon r = idleStart r + 120

shot :: RomTarget -> Int -> Shot
shot r = shotAt (rtTarget (rSpec r))

-- * Tiles

-- | Every cell's tile index in VRAM: blank 0, the fabric slots, the logo
-- cells in order.
cellTile :: RomTarget -> [Int]
cellTile = rCellTile

cellTile0 :: RomTarget -> [Int]
cellTile0 r = go (cells t) (1 + length slots)
 where
  t = rtTarget (rSpec r)
  slots = fabricSlots t
  go [] _ = []
  go (c : cs) n = case c of
    Fabric ph s -> (1 + fromMaybe (error "cellTile") (lookup (ph, s) (zip slots [0 ..]))) : go cs n
    Logo -> n : go cs (n + 1)
    Blank -> 0 : go cs n

-- | Each background tile's graphics at a picture frame (index = VRAM tile).
bgTiles :: RomTarget -> Int -> M.Map Int Tile
bgTiles r p = M.fromList $
  (bgBase r, U.replicate 64 0)
  : [ (bgBase r + i + 1, fabricTile s (min 2 (weaveLevel ph (shWeave sh)))) | (i, (ph, s)) <- zip [0 ..] (fabricSlots t) ]
  <> [ (ti, lt) | (c, ti, Just lt) <- zip3 (cells t) (cellTile r) (shLogo sh), c == Logo ]
 where
  t = rtTarget (rSpec r)
  sh = shot r (max 0 p)

-- | The sprites' patterns, by index (tile 256 + index).
spritePatterns :: RomTarget -> [Tile]
spritePatterns = rPatterns

spritePatterns0 :: RomTarget -> [Tile]
spritePatterns0 r = S.toList (S.fromList [ spPat s | p <- [0 .. horizon r], s <- shSprites (shot r p) ])

patternIndex :: RomTarget -> M.Map Tile Int
patternIndex = rPatIdx

-- | Every graphic the tile stream can upload (the library).
library :: RomTarget -> [Tile]
library = rLibrary

library0 :: RomTarget -> [Tile]
library0 r = S.toList (S.fromList (concat [ M.elems (bgTiles r p) | p <- [0 .. horizon r] ]))

libIndex :: RomTarget -> M.Map Tile Int
libIndex = rLibIdx

-- * Frames

-- | A tile upload: the VRAM tile and the library's graphic.
type TileOp = (Int, Int)

-- | A colour (CRAM index, 12-bit colour) or a VDP register (number, value).
data CramOp = Cram Int Int | Reg Int Word8
  deriving stock (Eq, Ord, Show)

-- | A sprite entry: slot, Y, X, pattern.
type SatOp = (Int, Word8, Word8, Word8)

tileOps :: RomTarget -> Int -> Int -> [TileOp]
tileOps r a b = [ (ti, li M.! g) | (ti, g) <- M.toList (bgTiles r b), M.lookup ti (bgTiles r a) /= Just g ]
 where
  li = libIndex r

cramState :: RomTarget -> Int -> Maybe [Int]
cramState r p
  | p < 0 = Nothing
  | otherwise = Just (map fromIntegral (shBg (shot r p)))

cramOps :: RomTarget -> Int -> Int -> [CramOp]
cramOps r a b = case (cramState r a, cramState r b) of
  (_, Nothing) -> []
  (Nothing, Just new) -> [ Cram i c | (i, c) <- zip [0 ..] new ] <> [Reg 1 0x60]
  (Just old, Just new) -> [ Cram i c | (i, c, o) <- zip3 [0 ..] new old, c /= o ]

-- | The sprite table's slots: the sparks first, then the lights, each in
-- their groups' order.
sparkSlots, lightSlots, sparkSlots0, lightSlots0 :: RomTarget -> Int
sparkSlots = rSparkN
lightSlots = rLightN
sparkSlots0 r = maximum (0 : [ length [ () | s <- shSprites (shot r p), isSpark s ] | p <- [0 .. horizon r + 96] ])
lightSlots0 r = maximum (0 : [ length [ () | s <- shSprites (shot r p), not (isSpark s) ] | p <- [0 .. horizon r + 120] ])

isSpark :: Sprite -> Bool
isSpark s = let (g, _, _, _) = spKey s in g == 0

-- | A sprite's (Y, X, pattern), or hidden (Y E0h).
entry :: RomTarget -> Maybe Sprite -> (Word8, Word8, Word8)
entry _ Nothing = (0xE0, 0, 0)
entry r (Just s) = case rtSpritePos (rSpec r) (spY0 s) (spX0 s) of
  Nothing -> (0xE0, 0, 0)
  Just (y, x) -> (y, x, fromIntegral (patternIndex r M.! spPat s))

satState :: RomTarget -> Bool -> Int -> [(Word8, Word8, Word8)]
satState r sparks p =
  let ss = sortOn spKey [ s | p >= 0, s <- shSprites (shot r p), isSpark s == sparks ]
      n = if sparks then sparkSlots r else lightSlots r
  in [ entry r (if i < length ss then Just (ss !! i) else Nothing) | i <- [0 .. n - 1] ]

satOps :: RomTarget -> Bool -> Int -> Int -> [SatOp]
satOps r sparks a b =
  [ (base + i, y, x, n) | (i, (y, x, n), o) <- zip3 [0 ..] (satState r sparks b) (satState r sparks a), (y, x, n) /= o ]
 where
  base = if sparks then 0 else sparkSlots r

-- | A stream's records: the intro (ROM frames 0 to the idle start, picture
-- frame k - lead) and the loop (its period from the idle start).
data Records a = Records [[a]] [[a]]

records :: Ord a => RomTarget -> Int -> (Int -> Int -> [a]) -> Records a
records r period ops = Records intro loop
 where
  pic k = k - lead r
  is = idleStart r
  intro = [ ops (pic k - 1) (pic k) | k <- [0 .. lead r + is - 1] ]
  loop = [ if q == 0 then nub (sortOn id (ops (is - 1) is <> ops (is + period - 1) is))
                     else ops (is + q - 1) (is + q)
         | q <- [0 .. period - 1] ]

tileRecs :: RomTarget -> Records TileOp
tileRecs r = records r 64 (tileOps r)

cramRecs :: RomTarget -> Records CramOp
cramRecs r = records r 72 (cramOps r)

sparkRecs, lightRecs :: RomTarget -> Records SatOp
sparkRecs r = records r 96 (satOps r True)
lightRecs r = records r 120 (satOps r False)

-- * Budget

-- | A ROM frame's cost in T-states, as the ROM's loops take them (a
-- generous count: the player's fixed work, then per operation).
frameCost :: Int -> Int -> Int -> Int -> Int -> Int
frameCost sound cram tiles sparks lights =
  2500 + 40 * sound + 120 * cram + 760 * tiles + 140 * (sparks + lights)

-- | Every ROM frame's cost, the intro and one period of each loop.
romFrameCosts :: RomTarget -> [Int]
romFrameCosts r =
  [ frameCost (soundAt k) (len (cramRecs r) k) (len (tileRecs r) k) (len (sparkRecs r) k) (len (lightRecs r) k) | k <- [0 .. lead r + idleStart r + 120] ]
 where
  soundAt k = (frameLens (rtSound (rSpec r)) <> repeat 0) !! k
  len (Records i l) k = length (if k < length i then i !! k else l !! ((k - length i) `mod` length l))

-- | The sound stream's bytes per frame.
frameLens :: [Word8] -> [Int]
frameLens (0xFF : _) = []
frameLens (n : rest) = fromIntegral n : frameLens (drop (fromIntegral n) rest)
frameLens [] = []

-- * Model

-- | What the screen shows at picture frame p (before 0: the lead-in).
expected :: RomTarget -> Int -> U.Vector Rgb12
expected r p
  | p < 0 = renderShot (rtTarget (rSpec r)) Nothing
  | otherwise = renderShot (rtTarget (rSpec r)) (Just (shot r p))

-- * Output

-- | A tile in the VDP's format: per row, four bit planes (bit 7 the
-- leftmost pixel).
tileBytes :: Tile -> [Word8]
tileBytes t = concat
  [ [ foldl (\acc x -> acc `shiftL` 1 .|. (if testBit (t U.! (y * 8 + x)) pl then 1 else 0)) 0 [0 .. 7] | pl <- [0 .. 3] ]
  | y <- [0 .. 7] ]

-- | The include file for the ROM.
romInc :: RomTarget -> String
romInc r = unlines $
  [ rtHeader (rSpec r)
  , printf ".DEFINE SPARK_SLOTS %d" (sparkSlots r)
  , printf ".DEFINE LIGHT_SLOTS %d" (lightSlots r)
  , printf ".DEFINE N_PATTERNS %d" (length (spritePatterns r))
  , ""
  , rtNameNote (rSpec r)
  , "NameTable:" ]
  <> [ ".DW " <> commaSep [ printf "$%04X" (ti .|. (if c == Logo then 0x1000 else 0)) | (c, ti) <- row ] | row <- chunks (tCols t) (zip (cells t) (cellTile r)) ]
  <> [ "", "; picture frame 0's tiles: VRAM tile, library graphic", "InitTiles:" ]
  <> [ printf ".DW %d" (M.size t0) ]
  <> [ printf ".DW $%04X, GgLib%d" (0x4000 + 32 * ti) (li M.! g) | (ti, g) <- M.toList t0 ]
  <> [ "", "; colour RAM from index 16: the backdrop (the background), then the sprite palette 1 to 9", "SpriteCram:" ]
  <> [ ".DB " <> commaSep (map hex2 (concatMap (rtCramBytes (rSpec r)) (take 1 (shBg (shot r 0)) <> spriteColours t))) ]
  <> [ "", if rtPatternsFirst (rSpec r) then "; the sprite patterns (tiles 0 on)" else "; the sprite patterns (tiles 256 on)", "Patterns:" ]
  <> [ ".DB " <> commaSep (map hex2 (tileBytes p)) | p <- spritePatterns r ]
  <> [ "", "; the tile library" ]
  <> concat [ [ printf "GgLib%d:" i, ".DB " <> commaSep (map hex2 (tileBytes g)) ] | (i, g) <- zip [0 :: Int ..] (library r) ]
  <> stream "GgTiles" (tileRecs r) (\(ti, g) -> printf ".DW $%04X, GgLib%d" (0x4000 + 32 * ti) g)
  <> stream "GgCram" (cramRecs r) (\o -> case o of
        Cram i c -> let (a, b, d) = rtCramOp (rSpec r) i (fromIntegral c) in printf ".DB $%02X, $%02X, $%02X" a b d
        Reg rg v -> printf ".DB $%02X, $%02X, 0" (0x80 .|. rg) v)
  <> stream "GgSparks" (sparkRecs r) satLine
  <> stream "GgLights" (lightRecs r) satLine
  <> [ "", "GgSound:" ]
  <> [ ".DB " <> commaSep (map hex2 c) | c <- chunks 32 (rtSound (rSpec r)) ]
 where
  t = rtTarget (rSpec r)
  t0 = bgTiles r 0
  li = libIndex r
  satLine (s, y, x, n) = printf ".DB %d, $%02X, $%02X, $%02X" s y x n
  stream :: String -> Records a -> (a -> String) -> [String]
  stream name (Records intro loop) line =
    [ "", name <> ":" ] <> concatMap (rec line) intro
    <> [ name <> "Loop:" ] <> concatMap (rec line) loop
    <> [ ".DB $FE", ".DW " <> name <> "Loop" ]
  rec line ops
    | length ops >= 0xFE = error "romInc: too many operations in a frame"
    | otherwise = printf ".DB %d" (length ops) : map line ops

hex2 :: Word8 -> String
hex2 = printf "$%02X"

commaSep :: [String] -> String
commaSep [] = ""
commaSep xs = foldr1 (\a b -> a <> ", " <> b) xs

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n xs = take n xs : chunks n (drop n xs)

-- | The figures (rota-stats).
romStats :: RomTarget -> String
romStats r = unlines
  [ printf "lead %d, idle start %d (picture frames)" (lead r) (idleStart r)
  , printf "background tiles %d, library %d, sprite patterns %d" (1 + length (fabricSlots t) + length [ () | Logo <- cells t ]) (length (library r)) (length (spritePatterns r))
  , printf "sprite slots: sparks %d, lights %d" (sparkSlots r) (lightSlots r)
  , printf "frame cost: most %d of %d T-states" (maximum (romFrameCosts r)) (rtBudget (rSpec r)) ]
 where
  t = rtTarget (rSpec r)
