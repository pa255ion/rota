{-|
The CHIP-8 demo (PLAN.md, side track J, step 4; docs/brand.md, "How the
ROMs draw"): one program, its bytes from this generator ('chip8Program').

__XOR, the threads as one union behind the word.__ On CHIP-8 every layer
is XOR-drawn: the ring, the emblem and the letters never overlap. The
three threads are one layer, their union (the user's choice, 2026-10-08:
XORed over each other, two threads a row apart cancelled to 1-row
slivers), left out of the disc and of the word's band (the user's
choice, 2026-10-08: the page XORs them over the letters, and as they
flowed they scrambled R O). So no layer overlaps another, the screen is
their XOR, and any layer can be changed at any time by XORing its
difference.

__The limits (the user's choice, 2026-10-07).__ On the Arty the quirks
are VIP: every DXYN waits for the next 60 Hz tick, so at most 60 draws a
second, whatever the speed switch says. The program therefore plays a
timeline of events (a sprite piece, a beep, a thread's start or flip,
the idle start) from the delay timer, events first, and the beeps from a
timeline of their own (never waiting behind a draw); draws the threads
in the free time, one column at a time (the lead's, the harmony's and
the bass's band computed, then the column): if a band changed, the
column's old union and new union are marked in two 0x80/0 sprites
(colA, colB; a band is 5 or 6 rows, so two 5-byte stores), the hidden
rows (the disc's, the word's) cleared, and their sum (0x80 + 0x80 wraps to 0: their XOR) drawn
once over the rows of the threads that changed (one draw of up to 15
rows; a sweep takes about 11 s at the fast setting); and
moves a thread's flow on at the start of each of its sweeps by the
frames since the last (rate × frames, by shift and add). Concessions: no
roll dips, the crash a single dark-then-lit blink of the ring, the
emblem in 3 steps, each flip in one step (at its middle). The beeps
cost no draws: exact at the fast setting, up to 7 frames late at the
slow one (the "clock" calls between the long steps).

Quirk-proof: Fx55/Fx65 never rely on I afterwards, shifts are 8xx6. The
program patches its own LD I and DRW instructions (the only way to point
I at a computed address or draw a computed height); data embeds LD I
instructions as pointers.
-}
module Rota.Pic.Chip8
  ( chip8Program
  , chip8Code
  , chip8LeadIn
  , frameOf
  ) where

import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.List (nub, sortOn)
import qualified Data.Set as S
import qualified Data.Vector as V
import Data.Word (Word8)

import Rota.Chip8.Asm
import Rota.Logo
import Rota.Score
import Rota.Sound.Chip8 (chip8Beeps)

-- | Frames (60 Hz) before the first beat.
chip8LeadIn :: Int
chip8LeadIn = jsRound (leadInSec * 60)

-- | The 60 Hz frame of a beat.
frameOf :: Double -> Int
frameOf b = chip8LeadIn + jsRound (b * beatSec * 60)

-- * Shapes

type Pixels = S.Set (Int, Int)

segPixels :: Int -> Pixels
segPixels k = S.fromList [ (x + ringOrigin, y) | x <- [0 .. 31], y <- [0 .. 31], segmentOf x y == Just k ]

corePixels :: Pixels
corePixels = S.fromList [ (x + ringOrigin, y) | x <- [15, 16], y <- [15, 16] ]

lambdaPixels :: Double -> Pixels
lambdaPixels p = S.fromList [ (i `mod` 32 + ringOrigin, i `div` 32) | i <- [0 .. 1023], emblemAt p V.! i == 1 ]

-- | A sprite piece: x, y and its rows (8 pixels wide, 1 to 15 rows).
data Piece = Piece Int Int [Word8]
  deriving stock (Eq, Ord, Show)

-- | Pixels as pieces: 8-wide bands from the leftmost pixel, rows trimmed,
-- at most 15 high.
pieces :: Pixels -> [Piece]
pieces px
  | S.null px = []
  | otherwise = concat [ band x0 | x0 <- [minX, minX + 8 .. maxX] ]
 where
  xs = map fst (S.toList px)
  minX = minimum xs
  maxX = maximum xs
  band x0 =
    let rows = [ (y, foldl (\acc i -> acc `shiftL` 1 .|. (if S.member (x0 + i, y) px then 1 else 0)) 0 [0 .. 7]) | y <- [0 .. 31 :: Int] ]
        used = [ y | (y, r) <- rows, r /= (0 :: Int) ]
    in if null used then [] else
         let y0 = minimum used
             y1 = maximum used
         in [ Piece x0 ys [ fromIntegral r | (y, r) <- rows, y >= ys, y <= min y1 (ys + 14) ] | ys <- [y0, y0 + 15 .. y1] ]

-- * Thread data

-- | A thread's rows for one flip, 64 entries (a 64th of a turn each).
threadTable :: Thread -> Double -> [Word8]
threadTable th f = [ fromIntegral (jsRound (thCy th + f * thA th * sin (2 * pi * fromIntegral i / 64))) | i <- [0 .. 63 :: Int] ]

-- | Column x's phase offset, in 256ths of a turn: round (256 x / L).
colOffsets :: Thread -> [Word8]
colOffsets th = [ fromIntegral (jsRound (256 * fromIntegral x / thL th) `mod` 256) | x <- [0 .. 64 :: Int] ]

-- | 16-bit phase (65536 a turn): the start and the rate per frame.
phase0, rate :: Thread -> Int
phase0 th = jsRound (thPhi th / (2 * pi) * 65536) `mod` 65536
rate th = jsRound (thOmega th / (60 * 2 * pi) * 65536) `mod` 65536

-- | The rows of column x the threads leave out (top, bottom): the disc's,
-- and the word's band (rows 10 to 20) across the whole width, so the
-- threads pass behind the letters (the user's choice, 2026-10-08: XORed
-- over them, they scrambled R O as they flowed; left out of the letters'
-- columns only, they showed as 1-column strips beside the word). The two
-- overlap in every disc column, so it is one range.
hiddenRows :: Int -> (Word8, Word8)
hiddenRows x
  | maximum ys - minimum ys + 1 /= length ys = error ("hiddenRows: column " <> show x <> " has two hidden ranges")
  | otherwise = (fromIntegral (minimum ys), fromIntegral (maximum ys))
 where
  wordRows = map snd letterPixels
  ys = [ y | y <- [0 .. 31], inDisc x y || (y >= minimum wordRows && y <= maximum wordRows) ]

travelFrames :: Int
travelFrames = jsRound (1.5 * beatSec * 60)

-- | Columns out d frames after a thread starts (the page's reveal).
revealTable :: [Word8]
revealTable = [ fromIntegral (jsRound (64 * min 1 (fromIntegral d / fromIntegral travelFrames :: Double))) | d <- [0 .. travelFrames] ]

-- * The timeline

data Event
  = EvSprite Piece
  | EvTable Int String   -- ^ thread, its table's label
  | EvStart Int
  | EvIdle

-- | (frame, event) in time order; one frame's events in the order listed.
timelineEvents :: [(Int, Event)]
timelineEvents = sortOn fst $ concat
  [ [ (frameOf 0, EvSprite p) | p <- pieces corePixels ]
    -- the ring, a stage a beat (no roll dips)
  , [ (frameOf (fromIntegral st), EvSprite p) | st <- [0 .. 4 :: Int], k <- [0 .. 7], min k (8 - k) == st, p <- pieces (segPixels k) ]
  , [ (frameOf (thStart th), EvStart v) | (v, th) <- zip [0 ..] threads ]
    -- each flip in one step, at its middle
  , [ (frameOf (inv + 0.25), EvTable v (tableLabel v True)) | (v, th) <- zip [0 ..] threads, Just inv <- [thInv th] ]
    -- the emblem in 3 steps (where the page's growth reaches 1/3, 2/3, 1)
  , [ (frameOf (growBeat + u * (lastBeat - growBeat)), EvSprite p)
    | (k, u) <- zip [1 :: Int ..] [1 - sqrt (2 / 3), 1 - sqrt (1 / 3), 1]
    , let now = lambdaPixels (fromIntegral k / 3)
          before = if k == 1 then S.empty else lambdaPixels (fromIntegral (k - 1) / 3)
    , p <- pieces (S.union (S.difference now before) (S.difference before now)) ]
    -- the crash: the ring dark, then lit again
  , [ (frameOf (lastBeat + 0.125), EvSprite p) | k <- [0 .. 7], p <- pieces (segPixels k) ]
  , [ (frameOf (lastBeat + 0.375), EvSprite p) | k <- [0 .. 7], p <- pieces (segPixels k) ]
  , [ (frameOf (lastBeat + 0.4), EvSprite p) | p <- pieces (S.fromList letterPixels) ]
    -- idle: only the token (segment 0) stays lit, then it turns
  , [ (frameOf endBeat, EvSprite p) | k <- [1 .. 7], p <- pieces (segPixels k) ]
  , [ (frameOf endBeat, EvIdle) ]
  ]

tableLabel :: Int -> Bool -> String
tableLabel v flipped = "tbl" <> show v <> (if flipped then "f" else "")

-- | Every piece the program draws, each with a label.
allPieces :: [Piece]
allPieces = nub ([ p | (_, EvSprite p) <- timelineEvents ] <> concatMap (pieces . segPixels) [0 .. 7])

pieceLabel :: Piece -> String
pieceLabel p = case lookup p (zip allPieces [0 :: Int ..]) of
  Just i -> "pc" <> show i
  Nothing -> error "pieceLabel"

-- * The program

-- Registers: V0 to V9 temporaries (an event record loads into V0..V7);
-- VA, VB the frame counter (lo, hi); VC the delay timer as last read; VD
-- the column, VE the thread of the round-robin; VF flags.
vFlo, vFhi, vLast, vX, vV :: Reg
vFlo = 0xA
vFhi = 0xB
vLast = 0xC
vX = 0xD
vV = 0xE

flag :: Reg
flag = 0xF

-- | The whole program, data included.
chip8Code :: [Ins]
chip8Code = concat [mainLoop, clock, events, token, threadStep, dataBlock]

mainLoop :: [Ins]
mainLoop =
  [ CLS
  , LDk 0 255, SETDT 0, LDk vLast 255
  , LDk vFlo 0, LDk vFhi 0, LDk vX 0, LDk vV 0
  , Label "main"
  , CALL (L "clock")
  , CALL (L "evLoad")                                  -- V0 V1: the next event's frame
  , LDr 8 vFhi, SUBr 8 0, SEk flag 1, JP (L "noEvent") -- frame hi < its hi: not yet
  , SEk 8 0, JP (L "event")                            -- frame hi > its hi: due
  , LDr 8 vFlo, SUBr 8 1, SEk flag 1, JP (L "noEvent") -- frame lo < its lo: not yet
  , Label "event"
  , CALL (L "doEvent")
  , JP (L "main")
  , Label "noEvent"
    -- idle: the token's step due? ((frame - next) mod 256 < 128)
  , LDI (L "idle"), LOAD 1                             -- V0 idle, V1 next step
  , SEk 0 1, JP (L "threads")
  , LDr 2 vFlo, SUBr 2 1, LDk 3 128, SUBr 2 3, SEk flag 0, JP (L "threads")
  , CALL (L "tokenStep")
  , JP (L "main")
  , Label "threads"
  , CALL (L "threadStep")
  , JP (L "main")
  ]

-- | The frames since the last call into the counter; the delay timer
-- kept at 64 or more.
clock :: [Ins]
clock =
  [ Label "clock"
  , GETDT 0
  , LDr 1 vLast, SUBr 1 0                    -- elapsed = last - now
  , LDr vLast 0
  , ADDr vFlo 1, ADDr vFhi flag               -- counter += elapsed
  , LDk 2 64, LDr 3 0, SUBr 3 2, SEk flag 0, JP (L "beepCheck") -- now >= 64: fine
  , ADDk 0 128, SETDT 0, LDr vLast 0
    -- a beep due? (a timeline of their own, checked at every clock: beeps
    -- never wait behind draws); uses V0 to V3 only
  , Label "beepCheck"
  , Label "bpPtr", LDI (L "beeps"), LOAD 2                 -- V0 V1 its frame, V2 the sound timer
  , LDr 3 vFhi, SUBr 3 0, SEk flag 1, RET                  -- frame hi < its hi
  , SEk 3 0, JP (L "beep")                                 -- frame hi > its hi
  , LDr 3 vFlo, SUBr 3 1, SEk flag 1, RET                  -- frame lo < its lo
  , Label "beep"
  , SETST 2
  , LDI (L "bpPtr"), LOAD 1, LDk 2 3, ADDr 1 2, ADDr 0 flag, LDI (L "bpPtr"), STORE 1
  , RET
  ]

-- | The timeline: the pointer lives in the LD I at "evPtr"; a record is
-- [frame hi, frame lo, kind, a, b, c, d, e].
events :: [Ins]
events =
  [ Label "evLoad"
  , Label "evPtr", LDI (L "timeline")
  , LOAD 7
  , RET
  , Label "doEvent"
  , SEk 2 0, JP (L "evNotSprite")
    -- 0 sprite: (V3 x, V4 y, V5 rows, V6 V7 its LD I)
  , LDr 0 6, LDr 1 7, LDI (L "spPtr"), STORE 1
  , LDr 0 5, LDk 1 0x40, ORr 0 1, LDI (LO "spDrw" 1), STORE 0
  , Label "spPtr", LDI (A 0x200)
  , Label "spDrw", DRW 3 4 1
  , CALL (L "clock")
  , JP (L "evAdvance")
  , Label "evNotSprite"
  , SEk 2 1, JP (L "evNotBeep")
    -- 1 beep: V3 frames
  , SETST 3
  , JP (L "evAdvance")
  , Label "evNotBeep"
  , SEk 2 2, JP (L "evNotTable")
    -- 2 a thread's table: block bytes 8, 9 = (V6, V7), its LD I
  , CALL (L "thrI"), LDk 0 8, ADDI 0
  , LDr 0 6, LDr 1 7, STORE 1
  , JP (L "evAdvance")
  , Label "evNotTable"
  , SEk 2 3, JP (L "evNotStart")
    -- 3 a thread starts: started = 1, start = the event's frame
  , LDr 8 0, LDr 9 1
  , CALL (L "thrI"), LDk 0 2, ADDI 0
  , LDk 0 1, LDr 1 8, LDr 2 9, STORE 2
  , JP (L "evAdvance")
  , Label "evNotStart"
    -- 4 idle: the token's first step 12 frames on, segment 0
  , LDk 0 1, LDr 1 vFlo, LDk 2 12, ADDr 1 2, LDk 2 0
  , LDI (L "idle"), STORE 2
  , Label "evAdvance"
  , LDI (L "evPtr"), LOAD 1, LDk 2 8, ADDr 1 2, ADDr 0 flag, LDI (L "evPtr"), STORE 1
  , RET
    -- thrI: I = thread V3's block (32 bytes each); uses V0, V1
  , Label "thrI"
  , LDI (L "thr"), LDr 0 3, LDk 1 32
  , Label "thrILoop"
  , SEk 0 0, JP (L "thrIAdd")
  , RET
  , Label "thrIAdd"
  , ADDI 1, ADDk 0 0xFF, JP (L "thrILoop")
  ]

-- | The idle token: the lit segment off, the next one on.
token :: [Ins]
token =
  [ Label "tokenStep"
  , LDI (L "idle"), LOAD 2                   -- V0 1, V1 next step, V2 segment
  , LDk 3 12, ADDr 1 3
  , LDr 8 2, ADDk 2 1, LDk 3 7, ANDr 2 3
  , LDI (L "idle"), STORE 2
  , LDr 3 8, CALL (L "segDraw")              -- the old segment off
  , LDI (L "idle"), LOAD 2, LDr 3 2, CALL (L "segDraw") -- the new one on
  , RET
    -- segDraw: segment V3's pieces (2 records of 5 bytes, rows = 0: none)
  , Label "segDraw"
  , LDr 9 3
  , LDI (L "segPieces"), LDk 0 0
  , Label "segFind"
  , SEr 0 9
  , JP (L "segNext")
  , JP (L "segFound")
  , Label "segNext"
  , LDk 1 10, ADDI 1, ADDk 0 1, JP (L "segFind")
  , Label "segFound"
  , LOAD 9
  , LDI (L "scratch"), STORE 9
  , LDI (L "scratch"), LOAD 4, CALL (L "pieceDraw")
  , LDI (LO "scratch" 5), LOAD 4, CALL (L "pieceDraw")
  , RET
    -- pieceDraw: (V0 x, V1 y, V2 rows, V3 V4 its LD I)
  , Label "pieceDraw"
  , SNEk 2 0, RET
  , LDr 8 0, LDr 9 1
  , LDr 0 3, LDr 1 4, LDI (L "pcPtr"), STORE 1
  , LDr 0 2, LDk 1 0x90, ORr 0 1, LDI (LO "pcDrw" 1), STORE 0
  , Label "pcPtr", LDI (A 0x200)
  , Label "pcDrw", DRW 8 9 1
  , JP (L "clock")
  ]

-- | One column (VD) of one thread (VE): its band again, XOR-redrawn if
-- it changed; then the next (thread, column).
threadStep :: [Ins]
threadStep =
  [ Label "threadStep"
  , SNEk vX 0, CALL (L "phaseUpdate")         -- a sweep starts: the flow moves on
  , LDr 3 vV, CALL (L "thrI"), LOAD 9         -- V0 Phi, V2 started, V3 V4 start, V8 V9 table LD I
  , LDr 6 0
  , LDr 0 8, LDr 1 9, LDI (L "tbPtr"), STORE 1, LDI (L "tbPtr2"), STORE 1
    -- the reveal (V7): 0 before the start, revealTab[frame - start], 64 from travel on
  , LDk 7 0
  , SEk 2 1, JP (L "tsRev")
  , LDr 8 vFlo, SUBr 8 4, LDr 9 vFhi, SEk flag 1, ADDk 9 0xFF, SUBr 9 3
  , LDk 7 64
  , SEk 9 0, JP (L "tsRev")
  , LDk 0 (fromIntegral travelFrames), LDr 1 8, SUBr 1 0, SEk flag 0, JP (L "tsRev")
  , LDI (L "revealTab"), ADDI 8, LOAD 0, LDr 7 0
  , Label "tsRev"
  , CALL (L "clock")                                           -- (V0 to V3 free here)
    -- the new band (V8 lo, V9 hi; 255 255: none)
  , LDk 8 255, LDk 9 255
  , LDr 0 vX, SUBr 0 7, SEk flag 0, JP (L "tsOld")  -- x >= reveal: none
  , LDI (L "colOff"), LDr 0 vV, LDk 1 65
  , Label "tsOff", SEk 0 0, JP (L "tsOffAdd")
  , ADDI vX, LOAD 1                                  -- V0 off[x], V1 off[x + 1]
  , ADDr 0 6, SHRr 0, SHRr 0, ADDr 1 6, SHRr 1, SHRr 1
  , LDr 5 1
  , Label "tbPtr", LDI (A 0x200)
  , ADDI 0, LOAD 0, LDr 8 0                          -- y0
  , Label "tbPtr2", LDI (A 0x200)
  , ADDI 5, LOAD 0, LDr 9 0                          -- y1
  , LDr 0 9, SUBr 0 8, SEk flag 1, JP (L "tsSwap")   -- y1 < y0: swap
  , JP (L "tsOrd")
  , Label "tsSwap", LDr 0 8, LDr 8 9, LDr 9 0
  , Label "tsOrd"                                    -- lo = min - 2, hi = max (lo, max - 3) + 4
  , ADDk 8 0xFE, ADDk 9 0xFD
  , LDr 0 9, SUBr 0 8, SEk flag 0, JP (L "tsHiOk")
  , LDr 9 8
  , Label "tsHiOk"
  , ADDk 9 4
  , Label "tsOld"
    -- this thread's new band into newb; the column is drawn once all three
    -- are in, the threads as one union (crossings don't cancel)
  , LDI (L "newb"), ADDI vV, ADDI vV, LDr 0 8, LDr 1 9, STORE 1
  , SEk vV 2, JP (L "tsMore")
    -- the column's old bands (bands + 6 x: lo, hi per thread) into oldb
  , CALL (L "colI"), LOAD 5, LDI (L "oldb"), STORE 5
    -- the span (V8 min, V9 max) of the threads that changed: old and new rows
  , LDk 8 255, LDk 9 0
  ] <> concat
  [ [ LDI (LO "oldb" (2 * k)), LOAD 1, LDr 2 0, LDr 3 1
    , LDI (LO "newb" (2 * k)), LOAD 1
    , SEr 0 2, CALL (L "widen"), SEr 1 3, CALL (L "widen") ]
  | k <- [0 .. 2 :: Int] ] <>
  [ SNEk 8 255, JP (L "tsNext")                      -- nothing changed
  , LDr 0 8, LDr 1 9, LDI (L "span"), STORE 1
  , LDI (L "newb"), LOAD 5, CALL (L "colI"), STORE 5
    -- the old union in colA, the new in colB: 0x80 on its rows
  , LDk 0 0, LDk 1 0, LDk 2 0, LDk 3 0, LDk 4 0, LDk 5 0, LDk 6 0, LDk 7 0
  ] <> concat [ [ LDI (LO "colA" o), STORE 7 ] | o <- [0, 8 .. 64] ] <> [ CALL (L "clock") ] <> concat
  [ [ LDI (LO buf (2 * k)), LOAD 1, LDr 5 0, LDr 6 1
    , LDk 0 0x80, LDk 1 0x80, LDk 2 0x80, LDk 3 0x80, LDk 4 0x80, LDk 7 base
    , CALL (L "markAt") ]
    -- (the clock between the old marks and the new: beeps stay on time)
    <> [ CALL (L "clock") | k == 2, buf == "oldb" ]
  | (buf, base) <- [("oldb", 0), ("newb", 36)], k <- [0 .. 2 :: Int] ] <>
  [ CALL (L "clock")
    -- the hidden rows (the disc's, the word's) inside the span: off in both
  , LDI (L "hidden"), ADDI vX, ADDI vX, LOAD 1, LDr 6 0, LDr 7 1
  , LDI (L "span"), LOAD 1
  , LDr 3 0, LDr 2 6, SUBr 2 0, SEk flag 0, LDr 3 6   -- from max (top, min)
  , LDr 4 1, LDr 2 1, SUBr 2 7, SEk flag 0, LDr 4 7   -- to min (bottom, max)
  , LDr 2 4, SUBr 2 3, SEk flag 1, JP (L "dzDone")
  , LDk 0 0
  , Label "dzRow"
  , LDI (L "colA"), ADDI 3, STORE 0, LDI (L "colB"), ADDI 3, STORE 0
  , SNEr 3 4, JP (L "dzDone")
  , ADDk 3 1, JP (L "dzRow")
  , Label "dzDone"
  , CALL (L "clock")
    -- colA = colA + colB over the span, 4 rows at a time: 0x80 + 0x80 wraps
    -- to 0, so it is their XOR (the rows that change)
  , LDI (L "span"), LOAD 1, LDr 8 0, LDr 9 1
  , Label "cbRows"
  , LDI (L "colA"), ADDI 8, LOAD 3, LDr 4 0, LDr 5 1, LDr 6 2, LDr 7 3
  , LDI (L "colB"), ADDI 8, LOAD 3
  , ADDr 0 4, ADDr 1 5, ADDr 2 6, ADDr 3 7
  , LDI (L "colA"), ADDI 8, STORE 3
  , ADDk 8 4, LDr 0 9, SUBr 0 8, SEk flag 0, JP (L "cbRows")  -- max >= next row: more
  , CALL (L "clock")
    -- XOR the span's rows of colA onto column VD, 15 rows a draw
  , LDI (L "span"), LOAD 1, LDr 4 0, LDr 5 1
  , Label "cdChunk"
  , LDr 0 5, SUBr 0 4, ADDk 0 1
  , LDr 1 0, LDk 2 16, SUBr 1 2, SEk flag 0, LDk 0 15  -- at most 15 rows
  , LDk 1 0x40, ORr 0 1, LDI (LO "cdDrw" 1), STORE 0  -- DRW VD, V4, rows
  , LDI (L "colA"), ADDI 4
  , Label "cdDrw", DRW vX 4 1
  , CALL (L "clock")
  , ADDk 4 15
  , LDr 0 5, SUBr 0 4, SEk flag 0, JP (L "cdChunk")   -- max >= V4: more rows
  , Label "tsNext"
  , LDk vV 0, ADDk vX 1, LDk 0 63, ANDr vX 0
  , RET
  , Label "tsMore", ADDk vV 1, RET
  , Label "tsOffAdd", ADDI 1, ADDk 0 0xFF, JP (L "tsOff")
    -- colI: I = bands + 6 x
  , Label "colI"
  , LDI (L "bands"), ADDI vX, ADDI vX, ADDI vX, ADDI vX, ADDI vX, ADDI vX
  , RET
    -- widen: the span (V8, V9) over the bands V0 V1 and V2 V3 (lo 255: none);
    -- uses V4
  , Label "widen"
  , SNEk 0 255, JP (L "wOld")
  , LDr 4 0, SUBr 4 8, SEk flag 1, LDr 8 0           -- lo < min: min = lo
  , LDr 4 9, SUBr 4 1, SEk flag 1, LDr 9 1           -- max < hi: max = hi
  , Label "wOld"
  , SNEk 2 255, RET
  , LDr 4 2, SUBr 4 8, SEk flag 1, LDr 8 2
  , LDr 4 9, SUBr 4 3, SEk flag 1, LDr 9 3
  , RET
    -- markAt: rows V5 (lo; 255: none) .. V6 (hi) of colA + V7 set to 0x80
    -- (V0 to V4): a band is 5 or 6 rows, so two 5-row stores, at lo and hi - 4
  , Label "markAt"
  , SNEk 5 255, RET
  , LDI (L "colA"), ADDI 7, ADDI 5, STORE 4
  , LDr 8 6, ADDk 8 0xFC
  , LDI (L "colA"), ADDI 7, ADDI 8, STORE 4
  , RET
    -- phaseUpdate: thread VE's phase += rate × E (E: frames since its last
    -- update, at most 255), by shift and add over E's bits
  , Label "phaseUpdate"
  , LDr 3 vV, CALL (L "thrI"), LOAD 6                -- V0 V1 phase, V5 V6 last update
  , LDr 8 vFlo, SUBr 8 6, LDr 9 vFhi, SEk flag 1, ADDk 9 0xFF, SUBr 9 5
  , SEk 9 0, LDk 8 255
  , LDr 7 0, LDr 4 1                                 -- V7 V4 the phase
  , LDk 9 1, LDk 2 0                                 -- the bit, the rate table's offset
  , Label "puBit"
  , LDr 0 8, ANDr 0 9, SEk 0 0, CALL (L "puAdd")
  , ADDk 2 2, LDr 0 9, ADDr 9 0, SEk 9 0, JP (L "puBit")
  , LDr 3 vV, CALL (L "thrI"), LDr 0 7, LDr 1 4, STORE 1
  , LDr 3 vV, CALL (L "thrI"), LDk 0 5, ADDI 0, LDr 0 vFhi, LDr 1 vFlo, STORE 1
  , JP (L "clock")                                   -- (a long step: the beeps' check)
  , Label "puAdd"                                    -- (V7 V4) += the rate << (V2 / 2)
  , LDr 3 vV, CALL (L "thrI"), LDk 0 16, ADDI 0, ADDI 2, LOAD 1
  , ADDr 4 1, ADDr 7 flag, ADDr 7 0
  , RET
  ]

dataBlock :: [Ins]
dataBlock = concat
  [ [ Label "idle", Bytes [0, 0, 0] ]          -- idle flag, next step (frame lo), segment
  , [ Label "scratch", Bytes (replicate 10 0) ]
  , [ Label "newb", Bytes (replicate 6 0) ]    -- the column's new bands (lo, hi per thread)
  , [ Label "oldb", Bytes (replicate 6 0) ]    -- and its old ones
  , [ Label "span", Bytes [0, 0] ]             -- the rows that change (min, max)
    -- the column's old union (colA) and new (colB), 0x80 on its rows, then
    -- colA their XOR, the column's sprite; 4 rows of slack after each, for
    -- the 4-row adds past the span
  , [ Label "colA", Bytes (replicate 36 0) ]
  , [ Label "colB", Bytes (replicate 36 0) ]
  , [ Label "revealTab", Bytes revealTable ]
  , [ Label "hidden", Bytes (concat [ [t, b] | x <- [0 .. 63], let (t, b) = hiddenRows x ]) ]
  , [ Label "colOff", Bytes (concatMap colOffsets threads) ]
  , [ Label "thr" ] <> concat [ thrBlock v th | (v, th) <- zip [0 ..] threads ]
  , [ Label "bands", Bytes (replicate 384 255) ]   -- per column x at 6 x: lo, hi per thread (255: none)
  , concat [ [ Label (tableLabel v False), Bytes (threadTable th 1) ]
             <> concat [ [ Label (tableLabel v True), Bytes (threadTable th (-1)) ] | Just _ <- [thInv th] ]
           | (v, th) <- zip [0 ..] threads ]
  , [ Label "segPieces" ] <> concat [ segRecord k | k <- [0 .. 7] ]
  , concat [ [ Label (pieceLabel p), Bytes rows ] | p@(Piece _ _ rows) <- allPieces ]
  , [ Label "timeline" ] <> concatMap eventRecord timelineEvents <> [ Bytes (replicate 8 0xFF) ]
  , [ Label "beeps", Bytes (concat [ [hi f, lo f, fromIntegral st] | (f, st) <- chip8Beeps ] <> [0xFF, 0xFF, 0]) ]
  ]
 where
  hi x = fromIntegral (x `shiftR` 8)
  lo x = fromIntegral (x .&. 0xFF)
  -- [phase hi, lo, started, start hi, lo, last hi, lo, -, table LD I (2), -, ...] then the rate << k (k = 0 to 7)
  thrBlock v th =
    [ Bytes [hi (phase0 th), lo (phase0 th), 0, 0, 0, 0, 0, 0]
    , LDI (L (tableLabel v False))
    , Bytes (replicate 6 0)
    , Bytes (concat [ [hi r, lo r] | k <- [0 .. 7 :: Int], let r = (rate th `shiftL` k) .&. 0xFFFF ]) ]
  segRecord k =
    let ps = take 2 (pieces (segPixels k))
    in if length (pieces (segPixels k)) > 2 then error "a segment in more than 2 pieces" else
       concat [ [ Bytes [fromIntegral x, fromIntegral y, fromIntegral (length rows)], LDI (L (pieceLabel p)) ] | p@(Piece x y rows) <- ps ]
       <> replicate (2 - length ps) (Bytes [0, 0, 0, 0, 0])
  eventRecord (f, e) = Bytes [hi f, lo f] : case e of
    EvSprite p@(Piece x y rows) -> [ Bytes [0, fromIntegral x, fromIntegral y, fromIntegral (length rows)], LDI (L (pieceLabel p)) ]
    EvTable v l -> [ Bytes [2, fromIntegral v, 0, 0], LDI (L l) ]
    EvStart v -> [ Bytes [3, fromIntegral v, 0, 0, 0, 0] ]
    EvIdle -> [ Bytes [4, 0, 0, 0, 0, 0] ]

-- | The program's bytes, from 0x200.
chip8Program :: [Word8]
chip8Program
  | length bytes > 0x1000 - 0x200 = error ("chip8Program: " <> show (length bytes) <> " bytes, more than 3584")
  | otherwise = bytes
 where
  bytes = assemble chip8Code
