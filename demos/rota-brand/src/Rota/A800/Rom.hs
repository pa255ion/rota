{-|
The Atari 800XL demo's data (PLAN.md, side track J, step 6): "Rota.A800"'s
frames as what the XEX (@demos/a800xl/rota.s@) loads and plays.

__Memory.__ The mode E screen in two blocks so no 8-line cell crosses
ANTIC's 4 KiB boundary: lines 0 to 95 at $8000, 96 to 191 at $9000 (a
second LMS). Player and missile memory at $A000 (one-line resolution:
missiles $A300, players $A400 + $100 n, indexed by scanline; screen line
y is scanline 32 + y), the display list at $A800. The XEX loads the
first beat's bitmap, the letters' and the core's graphics and the display
list straight there.

__Registers__ change down the screen by display-list interrupts on a
fixed schedule ('dliPlan'): each write has a window of lines it may land
before (from the line after the register last showed its old value to
the first line that needs the new one); writes are placed by deadline, at
most five a handler. The values come from constants or from slots the
main loop fills for each frame; line 0's come from the vertical blank
('vbiWrites': the OS's shadows and our deferred VBI).

__Each frame__ (ROM frame k; the first beat at 'ckLead'): while frame
k - 1 shows, the main loop decodes frame k's records into buffers; after
the playfield (VCOUNT 112) it writes the bitmap (the lambdas' reveal, the
fabric's step), the sparks' and lights' graphics, the slots and the
shadows; the deferred VBI then sets the line-0 positions and the first
DLI.

__Streams__ (fixed-size records; a first byte of $FF jumps to the loop):
looks (stage colours, flags), sparks, lights, the fabric's steps; the
reveal's bitmap writes (intro only).
-}
module Rota.A800.Rom
  ( -- * Layout
    lineAddr
  , pmBase
    -- * Display-list interrupts
  , Src (..)
  , Write (..)
  , dliPlan
  , vbiWrites
    -- * Bitmap
  , bitmapBytes
  , fabricCells
  , stepCells
  , kindRows
    -- * Frames
  , lookRec
  , sparkRec
  , lightRec
  , revealWrites
  , stepsAt
    -- * Output
  , a800RomInc
  , a800Stats
  ) where

import Data.Bits (shiftL, (.&.), (.|.))
import Data.List (sortOn)
import qualified Data.Map.Strict as M
import qualified Data.Vector.Unboxed as U
import Data.Word (Word8)
import Text.Printf (printf)

import Rota.A800
import Rota.Clock
import Rota.Emit.Util (ca65Bytes, header)
import Rota.Lab

-- * Layout

-- | The address of screen line y's first byte.
lineAddr :: Int -> Int
lineAddr y
  | y < 96 = 0x8000 + 40 * y
  | otherwise = 0x9000 + 40 * (y - 96)

pmBase :: Int
pmBase = 0xA000

-- * Display-list interrupts

-- | Where a register write's value comes from.
data Src = Const Word8 | Slot String
  deriving stock (Eq, Ord, Show)

-- | A register write: the register, the value, and its window: the first
-- line it may hold for (earliest) and the line it must hold for.
data Write = Write
  { wReg :: String
  , wSrc :: Src
  , wEarliest :: Int
  , wLine :: Int
  } deriving stock (Eq, Show)

light60, light132 :: Int
(light60, light132) = case lightRows of
  [a, b] -> (a, b)
  rs -> error ("Rota.A800.Rom: light rows " <> show rs)

roleSrc :: PfRole -> Src
roleSrc r = case r of
  Stage s -> Slot ("stage+" <> show (s - 1))
  Lambdas -> Const colLambda
  Fabric -> Const colFabric

ccOf :: Int -> Word8
ccOf x = fromIntegral (48 + x `div` 2)

-- | Every write a frame needs below line 0, in their windows.
frameWrites :: [Write]
frameWrites =
  -- the ring's and lambdas' playfield registers
  [ Write ("COLPF" <> show reg) (roleSrc role) e l | (e, l, reg, role) <- pfWrites, l > 0 ]
  -- the letters' players: positions in the four lines above the band,
  -- their colour (the gradient) on every band line
  <> [ Write ("HPOSP" <> show n) (Const (ccOf (obX0 o))) sparkTop bandTop | (n, o) <- zip [0 :: Int ..] letterObjs ]
  <> [ Write ("COLPM" <> show n) (Slot ("grad+" <> show (y - bandTop))) (if y == bandTop then sparkTop else y) y | y <- [bandTop .. bandBottom], n <- [0 .. 3 :: Int] ]
  -- below the band: the sparks' players back
  <> [ Write ("HPOSP" <> show n) (Slot ("belowX+" <> show n)) (bandBottom + 1) (sparkBottom + 1) | n <- [0 .. 2 :: Int] ]
  <> [ Write ("COLPM" <> show n) (Const (sparkColour v)) (bandBottom + 1) (sparkBottom + 1) | (n, v) <- zip [0 :: Int ..] [SLead, SHarm, SBass] ]
  -- PF3: the sparks' colour in their rows, the core's white, the lights'
  <> [ Write "COLPF3" (Slot "bandCol") (light60 + 2) sparkTop
     , Write "COLPF3" (Const colCore) coreTop coreTop
     , Write "COLPF3" (Slot "bandCol") (coreBottom + 1) (coreBottom + 1)
     , Write "COLPF3" (Const colLight) (sparkBottom + 1) (light132 - 1) ]
  -- M2, M3: the band's sparks (double width), then the lower lights
  <> [ Write "HPOSM2" (Slot "bandX+0") (light60 + 2) sparkTop, Write "HPOSM3" (Slot "bandX+1") (light60 + 2) sparkTop
     , Write "SIZEM" (Const 0x50) (light60 + 2) sparkTop
     , Write "HPOSM2" (Slot "lightX+2") (sparkBottom + 1) (light132 - 1), Write "HPOSM3" (Slot "lightX+3") (sparkBottom + 1) (light132 - 1)
     , Write "SIZEM" (Const 0) (sparkBottom + 1) (light132 - 1) ]

-- | What the vertical blank sets for line 0: colours and priority through
-- the OS's shadows, positions and sizes in our deferred VBI.
vbiWrites :: [(String, Src)]
vbiWrites =
  [ ("COLPF0", Const colFabric), ("COLPF3", Const colLight), ("COLBK", Const colBk) ]
  <> [ ("COLPM" <> show n, Const (sparkColour v)) | (n, v) <- zip [0 :: Int ..] [SLead, SHarm, SBass] ]
  <> [ ("HPOSP" <> show n, Slot ("aboveX+" <> show n)) | n <- [0 .. 2 :: Int] ]
  <> [ ("HPOSP3", Const 0), ("HPOSM0", Const (ccOf 156)), ("HPOSM1", Const (ccOf 160))
     , ("HPOSM2", Slot "lightX+0"), ("HPOSM3", Slot "lightX+1"), ("SIZEM", Const 0) ]

-- | The writes placed on DLI lines: a write for line l lands in the
-- horizontal blank before l, so it runs in the handler of line l - 1.
-- Earliest deadline first, each at the earliest line its window and the
-- handlers' room allow (five stores), so a line's horizontal blank isn't
-- crowded with writes that could have gone before.
dliPlan :: M.Map Int [Write]
dliPlan = foldl place M.empty (sortOn wLine frameWrites)
 where
  place plan wr =
    let candidates = [ t | t <- [wEarliest wr - 1 .. wLine wr - 1], t >= 0, length (M.findWithDefault [] t plan) < 5 ]
    in case candidates of
         (t : _) -> M.insertWith (flip (<>)) t [wr] plan
         [] -> error ("Rota.A800.Rom: no room for " <> show wr)

-- | Where a fabric step's cells split: those above are written behind
-- the beam in the frame before, those from here after the next VBI, ahead
-- of the beam (the beam reaches the bottom too late for them all).
splitLine :: Int
splitLine = 128

-- | The DLI lines in runs of consecutive lines: one handler per run (the
-- display list's DLI bit on its first line), stepping down the run with
-- WSYNC (a handler a line would not finish within a mode E line).
dliRuns :: [(Int, [[Write]])]
dliRuns = go (M.toList dliPlan)
 where
  go [] = []
  go ((t, ws) : rest) =
    let (run, rest') = spanRun (t + 1) rest
    in (t, ws : map snd run) : go rest'
  spanRun _ [] = ([], [])
  spanRun n xs@((t, ws) : rest)
    | t == n = let (r, rest') = spanRun (n + 1) rest in ((t, ws) : r, rest')
    | otherwise = ([], xs)

-- * Bitmap

-- | The mode E bitmap as bytes, line by line (40 a line, four pixels a
-- byte, the leftmost in bits 7 and 6).
bitmapBytes :: Shot -> U.Vector Word8
bitmapBytes sh = U.generate (40 * screenLines) $ \i ->
  let (y, xb) = i `divMod` 40
  in foldl (\acc e -> (acc `shiftL` 2) .|. (shBitmap sh U.! (y * 160 + 4 * xb + e))) 0 [0 .. 3]

-- | The fabric's cells: (tile x, tile y) in our pixels, phase, shape: trace
-- cells with no logo pixel (the lab's rule; letters solid).
fabricCells :: [((Int, Int), Int, Shape)]
fabricCells =
  [ ((tx, ty), weavePhase g tx ty, shape)
  | ty <- [0, 8 .. screenLines - 1], tx <- [0, 8 .. 319]
  , isTraceCell g tx ty
  , not (or [ gLogo g U.! (y * 320 + x) | y <- [ty .. ty + 7], x <- [tx .. tx + 7] ])
  , let shape = case (traceH g ty, traceV g tx) of
          (True, True) -> SX
          (True, False) -> SH
          _ -> SV ]
 where
  g = a800Geo

-- | A fabric cell's eight bytes at a level (0 pulse, 1 tail, 2 dotted),
-- the fabric being PF0 (pixel value 1).
kindRows :: Shape -> Int -> [Word8]
kindRows sh lv = [ foldl (\acc e -> (acc `shiftL` 2) .|. (if on ly (2 * e) || on ly (2 * e + 1) then 1 else 0)) 0 [0 .. 3] | ly <- [0 .. 7] ]
 where
  hz = sh /= SV
  vt = sh /= SH
  on ly lx =
    let onH = hz && (if lv <= 1 then ly == 3 || (lv == 0 && ly == 4) else ly == 3 && even lx)
        onV = vt && (if lv <= 1 then lx == 3 || (lv == 0 && lx == 4) else lx == 3 && even ly)
        node = hz && vt && lx >= 2 && lx <= 4 && ly >= 2 && ly <= 4
    in onH || onV || node

-- | The cells a step from s to s + 1 redraws, by kind (shape, level):
-- phase s + 1 becomes the pulse, s + 2 its tail, s dotted.
stepCells :: Int -> [((Shape, Int), [(Int, Int)])]
stepCells st = M.toList (M.fromListWith (flip (<>)) [ ((sh, lv), [xy]) | (ph, lv) <- [((st + 1) `mod` weaveL, 0), ((st + 2) `mod` weaveL, 1), (st, 2)], (xy, ph', sh) <- fabricCells, ph' == ph ])

-- * Frames

lead, idleStart, nIntro :: Int
lead = ckLead a800Clock
idleStart = ckIdle a800Clock
nIntro = lead + idleStart

shotK :: Int -> Maybe Shot
shotK k = if k < lead then Nothing else Just (shotAt (k - lead))

-- | Looks: the five stages' colours, and flags (bit 0 display on, bit 1
-- letters lit).
lookRec :: Int -> [Word8]
lookRec k = case shotK k of
  Nothing -> [0, 0, 0, 0, 0, 0]
  Just sh -> shStages sh <> [1 .|. (if isLit sh then 2 else 0)]
 where
  isLit sh = case shLetters sh of
    Just _ -> True
    Nothing -> False

-- | The fabric's steps in ROM frame k.
stepsAt :: Int -> Int
stepsAt k = let p = k - lead in if p >= 1 then weaveCapped a800Clock p - weaveCapped a800Clock (p - 1) else 0

-- | The shapes the sparks take: players' (8-bit rows), missiles' (2-bit).
playerShapes, missileShapes :: M.Map [Word8] Int
playerShapes = M.fromList (zip (uniq [ obRows o | sh <- sparkShots, Just o <- shPlayers sh ]) [0 ..])
missileShapes = M.fromList (zip (uniq [ obRows o | sh <- sparkShots, let (l, r) = shBand sh, Just o <- [l, r] ]) [0 ..])

uniq :: Ord a => [a] -> [a]
uniq = M.keys . M.fromList . map (\x -> (x, ()))

-- | Every shot the sparks' streams use: the intro and the idle loop.
sparkShots :: [Shot]
sparkShots = [ shotAt p | p <- [0 .. idleStart + 96 - 1] ]

-- | Sparks: per voice its player (y0 or $FE, colour clock, shape), the
-- band's left and right missiles (the same), the band's colour.
sparkRec :: Int -> [Word8]
sparkRec k = case shotK k of
  Nothing -> concat (replicate 5 [0xFE, 0, 0]) <> [colLight]
  Just sh -> concat [ obj playerShapes o | o <- shPlayers sh ] <> obj missileShapes (fst (shBand sh)) <> obj missileShapes (snd (shBand sh)) <> [shBandColour sh]
 where
  obj _ Nothing = [0xFE, 0, 0]
  obj shapes (Just o) = [fromIntegral (obY0 o), ccOf (obX0 o), fromIntegral (shapes M.! obRows o)]

-- | Lights: the colour clocks of row 60's left and right, row 132's (0:
-- none). Each light is the same shape (rows 3, 3, 3 from the line above).
lightRec :: Int -> [Word8]
lightRec k = case shotK k of
  Nothing -> [0, 0, 0, 0]
  Just sh -> concat [ [x l ry, x r ry] | (ry, l, r) <- shLights sh ]
 where
  x Nothing _ = 0
  x (Just o) ry
    | obRows o == [3, 3, 3] && obY0 o == ry - 1 = ccOf (obX0 o)
    | otherwise = error ("Rota.A800.Rom: a light of another shape " <> show o)

-- | The bitmap writes other than the fabric's steps (the lambdas'
-- reveal) in ROM frame k: (address, value).
revealWrites :: Int -> [(Int, Word8)]
revealWrites k
  | k <= lead || k >= nIntro = []
  | otherwise =
      [ (lineAddr y + xb, b) | i <- [0 .. 40 * screenLines - 1], let a = before U.! i; b = after U.! i, a /= b
                             , let (y, xb) = i `divMod` 40, not (fabricByte y xb) ]
 where
  before = bitmapBytes (shotAt (k - lead - 1))
  after = bitmapBytes (shotAt (k - lead))
  fabricByte y xb = (8 * xb, 8 * (y `div` 8)) `elem` fabricXY
  fabricXY = [ xy | (xy, _, _) <- fabricCells ]

-- * Output

-- | rota_pic.inc for ca65.
a800RomInc :: String
a800RomInc = header ";" ["The Atari 800XL demo's pictures (Rota.A800, Rota.A800.Rom)."]
  <> ".segment \"SCREEN1\"\n" <> ca65Bytes (take (96 * 40) (U.toList bm0))
  <> ".segment \"SCREEN2\"\n" <> ca65Bytes (drop (96 * 40) (U.toList bm0))
  <> ".segment \"PMSTATIC\"\n; missiles ($A300) and players ($A400 to $A7FF) from scanline 0\n" <> ca65Bytes pmStatic
  <> ".segment \"DLIST\"\n" <> displayList
  <> ".segment \"RODATA\"\n"
  <> printf "ROTA_LEAD = %d\nROTA_INTRO = %d\nSPARK_TOP = %d\nROTA_LIGHT_SCAN0 = %d\nROTA_LIGHT_SCAN1 = %d\n" lead nIntro sparkTop (32 + light60 - 1) (32 + light132 - 1)
  <> printf "ROTA_COL_FABRIC = $%02X\nROTA_COL_LIGHT = $%02X\nROTA_COL_BK = $%02X\nROTA_COL_LEAD = $%02X\nROTA_COL_HARM = $%02X\nROTA_COL_BASS = $%02X\n"
       colFabric colLight colBk (sparkColour SLead) (sparkColour SHarm) (sparkColour SBass)
  <> "; the gradient: level by phase (256), colour by level (7)\nRotaGradLevel:\n" <> ca65Bytes [ fromIntegral (gradPhaseLevel ph) | ph <- [0 .. 255 :: Int] ]
  <> "RotaGradColour:\n" <> ca65Bytes [ letterColour (Just lv) | lv <- [0 .. 6] ]
  <> printf "ROTA_DIM = $%02X\n" (letterColour Nothing)
  <> stream "RotaLooks" 6 [ lookRec k | k <- [0 .. nIntro - 1] ] [ lookRec (nIntro + q) | q <- [0 .. 71] ]
  <> stream "RotaSparks" 16 [ sparkRec k | k <- [0 .. nIntro - 1] ] [ sparkRec (nIntro + q) | q <- [0 .. 95] ]
  <> stream "RotaLights" 4 [ lightRec k | k <- [0 .. nIntro - 1] ] [ lightRec (nIntro + q) | q <- [0 .. 119] ]
  <> stream "RotaFab" 1 [ [fromIntegral (stepsAt k)] | k <- [0 .. nIntro - 1] ] [ [fromIntegral (stepsAt (nIntro + q))] | q <- [0 .. 63] ]
  <> "; the reveal: per intro frame a count, then (address low, high, value)\nRotaReveal:\n"
  <> ca65Bytes (concat [ fromIntegral (length ws) : concat [ [fromIntegral (a .&. 0xFF), fromIntegral (a `div` 256), v] | (a, v) <- ws ] | k <- [0 .. nIntro - 1], let ws = revealWrites k ])
  <> "; sprite shapes: per shape its rows' count and its rows, and tables of their addresses\n"
  <> shapeTable "RotaPlayerShape" playerShapes <> shapeTable "RotaMissileShape" missileShapes
  <> "; the fabric's moves: per shape, a cell's rows that change as it goes\n; tail to pulse, dotted to tail, pulse to dotted (immediate values, ptr = the cell)\n"
  <> concat [ moveCode sh to | sh <- [SH, SV, SX], to <- [0, 1, 2] ]
  <> "; per step s, the cells of its move to s + 1, top to bottom, in two parts: above line "
  <> show splitLine <> ", written behind the beam\n; in the frame before (VCOUNT once the beam is past it, the cell's address low, high, its move's\n"
  <> "; address low, high; $FF ends); from line " <> show splitLine <> ", written after the next VBI, ahead of the beam\n; (a count, then the cell's address and its move's)\n"
  <> "RotaStepsLo:\n    .byte " <> commas [ "<RotaStep" <> show st | st <- [0 .. weaveL - 1] ] <> "\n"
  <> "RotaStepsHi:\n    .byte " <> commas [ ">RotaStep" <> show st | st <- [0 .. weaveL - 1] ] <> "\n"
  <> "RotaStepsBLo:\n    .byte " <> commas [ "<RotaStepB" <> show st | st <- [0 .. weaveL - 1] ] <> "\n"
  <> "RotaStepsBHi:\n    .byte " <> commas [ ">RotaStepB" <> show st | st <- [0 .. weaveL - 1] ] <> "\n"
  <> concat [ "RotaStep" <> show st <> ":\n"
              <> concat [ printf "    .byte %d, $%02X, $%02X, <%s, >%s\n" (passed y) (a .&. 0xFF) (a `div` 256) (moveName sh lv) (moveName sh lv)
                        | (y, a, sh, lv) <- cellsOf st, y < splitLine ]
              <> "    .byte $FF\n"
              <> "RotaStepB" <> show st <> ":\n"
              <> printf "    .byte %d\n" (length [ () | (y, _, _, _) <- cellsOf st, y >= splitLine ])
              <> concat [ printf "    .byte $%02X, $%02X, <%s, >%s\n" (a .&. 0xFF) (a `div` 256) (moveName sh lv) (moveName sh lv)
                        | (y, a, sh, lv) <- cellsOf st, y >= splitLine ]
            | st <- [0 .. weaveL - 1] ]
  <> "; display-list interrupts: a handler per line, chained\n"
  <> concat [ dliCode i t ws | (i, (t, ws)) <- zip [0 :: Int ..] dlis ]
 where
  bm0 = bitmapBytes (shotAt 0)
  -- a step's cells top to bottom: (line, address, shape, level)
  cellsOf st = sortOn (\(y, a, _, _) -> (y, a)) [ (y, lineAddr y + x `div` 8, sh, lv) | ((sh, lv), xys) <- stepCells st, (x, y) <- xys ]
  moveName sh to = "RotaMove" <> show sh <> show (to :: Int)
  -- the level a cell had before moving to @to@
  from to = case to of
    0 -> 1
    1 -> 2
    _ -> 0
  moveCode sh to =
    let old = kindRows sh (from to)
        new = kindRows sh to
        rows = [ (r, v) | (r, (o, v)) <- zip [0 :: Int ..] (zip old new), o /= v ]
        top = [ q | q@(r, _) <- rows, r < 6 ]
        low = [ q | q@(r, _) <- rows, r >= 6 ]
        store (r, v) = printf "    ldy #%d\n    lda #$%02X\n    sta (ptr), y\n" (40 * r) v
        store6 (r, v) = printf "    ldy #%d\n    lda #$%02X\n    sta (ptr), y\n" (40 * (r - 6)) v
    in moveName sh to <> ":\n" <> concatMap store top
       <> (if null low then "" else "    clc\n    lda ptr\n    adc #240\n    sta ptr\n    bcc :+\n    inc ptr + 1\n:\n" <> concatMap store6 low)
       <> "    rts\n"
  -- VCOUNT (scanline / 2) once the beam has drawn a cell's 8 lines
  passed ty = (32 + ty + 8 + 1) `div` 2
  commas = foldr1 (\a b -> a <> ", " <> b)
  stream :: String -> Int -> [[Word8]] -> [[Word8]] -> String
  stream name n intro loop =
    printf "; %s: %d-byte records, intro %d frames, loop %d; a first byte of $FF jumps to the loop\n" name n (length intro) (length loop)
    <> name <> ":\n" <> ca65Bytes (concat intro <> [0xFF]) <> name <> "Loop:\n" <> ca65Bytes (concat loop <> [0xFF])
  shapeTable name shapes =
    let ss = sortOn snd (M.toList shapes)
    in concat [ printf "%s%d:\n" name i <> ca65Bytes (fromIntegral (length rows) : rows) | (rows, i) <- ss ]
       <> name <> "Lo:\n    .byte " <> commas [ printf "<%s%d" name i | (_, i) <- ss ] <> "\n"
       <> name <> "Hi:\n    .byte " <> commas [ printf ">%s%d" name i | (_, i) <- ss ] <> "\n"
  dlis = dliRuns
  dliCode i t lines' =
    printf "RotaDli%d: ; lines %d to %d\n    pha\n" i t (t + length lines' - 1)
    <> concat [ lineCode ws | ws <- lines' ]
    <> (if i + 1 < length dlis then printf "    lda #<RotaDli%d\n    sta VDSLST\n    lda #>RotaDli%d\n    sta VDSLST + 1\n" (i + 1) (i + 1) else "")
    <> "    pla\n    rti\n"
  -- one line's writes: the first value, the wait for the horizontal
  -- blank, the stores
  lineCode ws = concat [ load src <> (if j == 0 then "    sta WSYNC\n" else "") <> concat [ "    sta " <> wReg w' <> "\n" | w' <- grp ]
                       | (j, (src, grp)) <- zip [0 :: Int ..] (M.toList (M.fromListWith (flip (<>)) [ (wSrc w', [w']) | w' <- ws ])) ]
  load (Const v) = printf "    lda #$%02X\n" v
  load (Slot s) = "    lda " <> s <> "\n"
  displayList = "RotaDl:\n" <> ca65Bytes ([0x70, 0x70, 0x70] <> concat [ line y | y <- [0 .. screenLines - 1] ]) <> "    .byte $41\n    .word RotaDl\n"
  line y =
    let dli = if y `elem` map fst dliRuns then 0x80 else 0 :: Word8
    in if y == 0 || y == 96 then [0x4E .|. dli, fromIntegral (lineAddr y .&. 0xFF), fromIntegral (lineAddr y `div` 256)] else [0x0E .|. dli]
  pmStatic =
    [ missile s | s <- [0 .. 255] ] <> concat [ [ player n s | s <- [0 .. 255] ] | n <- [0 .. 3] ]
  missile s = let y = s - 32 in if y >= coreTop && y <= coreBottom then 0x0F else 0
  player n s = let y = s - 32; o = letterObjs !! n; i = y - obY0 o in if i >= 0 && i < length (obRows o) then obRows o !! i else 0

a800Stats :: String
a800Stats = unlines
  [ printf "DLI lines %d, writes %d (most on a line %d), handlers %d: %s" (length dlis) (sum (map length dlis)) (maximum (map length dlis)) (length dliRuns) (show [ (t, length ls) | (t, ls) <- dliRuns ])
  , "lines: " <> show [ (t, length ws) | (t, ws) <- M.toList dliPlan ]
  , printf "fabric cells %d; cells a step: %s" (length fabricCells) (show [ sum (map (length . snd) (stepCells st)) | st <- [0 .. weaveL - 1] ])
  , printf "shapes: players %d, missiles %d" (M.size playerShapes) (M.size missileShapes)
  , printf "reveal writes %d" (sum [ length (revealWrites k) | k <- [0 .. nIntro - 1] ]) ]
 where
  dlis = [ ws | (_, ws) <- M.toList dliPlan ]
