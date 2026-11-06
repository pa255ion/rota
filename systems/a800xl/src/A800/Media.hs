{-|
Program files for the 800XL (M4 step 4.0; docs/systems/atari800xl.md,
"Loading"): executables (XEX) and cartridges (CAR, raw ROM). Pure parsers,
shared by the tests, the reference model and the desktop tools; the
machine itself only ever sees the bytes (a cartridge image in its
cartridge memory, an XEX file read by our 6502 loader stub).

__XEX__ (Atari DOS 2 binary load file; "Mapping the Atari", De Re Atari
ch. 10, cross-checked with Altirra's loader): a sequence of segments, each
a start address, an end address (both little endian, end inclusive) and
@end - start + 1@ bytes. The file starts with the marker $FFFF; the marker
may also precede any later segment. Two addresses have a meaning to the
loader: a segment writing INITAD ($02E2) makes the loader call that
routine right after the segment ('segInit'), and RUNAD ($02E0) is where
it jumps when the file ends. Without RUNAD the loader runs the first
segment's start (Altirra's rule; DOS itself returns to its menu).

A file that ends inside a segment's header or data is an error ('XexError'
says where); trailing bytes too few for a header (some files end in a
stray byte or two) are tolerated and reported in 'xexTrailing'.

__CAR__ (Atari800/Altirra cartridge image): a 16-byte header, "CART", the
type (32 bits, big endian), a checksum (32 bits, big endian: the sum of
every data byte), 4 unused bytes, then the image. M4 runs the two standard
left-slot types: 1 (8 KiB at $A000) and 2 (16 KiB at $8000). A raw @.rom@
of exactly 8 or 16 KiB is taken as the same two types.
-}
module A800.Media
  ( -- * XEX
    Segment (..)
  , Xex (..)
  , XexError (..)
  , parseXex
  , encodeXex
  , runad
  , initad
  , segInit
  , xexRunAddress
    -- * Cartridges
  , CartType (..)
  , Cart (..)
  , parseCar
  , parseRawCart
  , parseCart
  , cartBase
  , cartSize
  , cartBanks
  , isXegs
  , cartByte
  , carChecksum
    -- * Cassettes (side track Q, 2026-10-21)
  , Cas (..)
  , parseCas
  , encodeCas
  , casBoot
  , casImage
  , casImageFile
  , sioChecksum
    -- * The tape as a signal (side track Q, cassettes depth 2)
  , TapeRec (..)
  , parseTape
  , tapeRecCycles
  , tapeLevel
  , machineHz
  ) where

import Prelude

import Data.Bits (shiftL, shiftR, testBit, (.&.), (.|.))
import qualified Data.ByteString as BS
import Data.Char (toLower)
import Data.Word

-- * XEX

data Segment = Segment
  { segStart :: !Word16
  , segData  :: !BS.ByteString -- ^ never empty; end = start + length - 1
  } deriving stock (Show, Eq)

data Xex = Xex
  { xexSegments :: [Segment]
  , xexTrailing :: !Int -- ^ bytes after the last segment (fewer than a header)
  } deriving stock (Show, Eq)

data XexError
  = XexNoMarker                 -- ^ the file doesn't start with $FFFF
  | XexEmpty                    -- ^ the marker and no segment
  | XexBadRange !Int !Word16 !Word16 -- ^ offset of the header; end < start
  | XexTruncated !Int !Word16 !Int   -- ^ offset of the header; start; bytes missing
  deriving stock (Show, Eq)

runad, initad :: Word16
runad = 0x02E0
initad = 0x02E2

parseXex :: BS.ByteString -> Either XexError Xex
parseXex f
  | BS.length f < 2 || word 0 /= 0xFFFF = Left XexNoMarker
  | otherwise = go 2 []
 where
  len = BS.length f
  word o = fromIntegral (BS.index f o) .|. (fromIntegral (BS.index f (o + 1)) `shiftL` 8) :: Word16
  go o acc
    | len - o < 4 = done o acc
    | word o == 0xFFFF = if len - o < 6 then done o acc else seg o (o + 2) acc
    | otherwise = seg o o acc
  seg h o acc
    | e < s = Left (XexBadRange h s e)
    | len - (o + 4) < n = Left (XexTruncated h s (n - (len - (o + 4))))
    | otherwise = go (o + 4 + n) (Segment s (BS.take n (BS.drop (o + 4) f)) : acc)
   where
    s = word o
    e = word (o + 2)
    n = fromIntegral e - fromIntegral s + 1
  done o acc
    | null acc = Left XexEmpty
    | otherwise = Right (Xex (reverse acc) (len - o))

-- | A file the parser reads back as the same segments (the marker once,
-- at the start).
encodeXex :: [Segment] -> BS.ByteString
encodeXex segs = BS.concat (BS.pack [0xFF, 0xFF] : map one segs)
 where
  one (Segment s d) =
    let e = s + fromIntegral (BS.length d) - 1
    in BS.pack [lo s, hi s, lo e, hi e] <> d
  lo w = fromIntegral (w .&. 0xFF)
  hi w = fromIntegral (w `shiftR` 8)

-- | The init address a segment sets, if it writes both bytes of INITAD.
-- (A segment writing only one of them doesn't count, as in DOS: the
-- loader resets INITAD before each segment and compares afterwards; the
-- stub does the same, so such files behave alike on both.)
segInit :: Segment -> Maybe Word16
segInit = wordAt initad

-- | Where the file starts: RUNAD as the last segment writing it leaves it,
-- else the first segment's start.
xexRunAddress :: Xex -> Word16
xexRunAddress (Xex segs _) = case [a | Just a <- map (wordAt runad) (reverse segs)] of
  a : _ -> a
  []    -> case segs of
    s : _ -> segStart s
    []    -> 0

wordAt :: Word16 -> Segment -> Maybe Word16
wordAt a (Segment s d)
  | a >= s && fromIntegral a - fromIntegral s + 1 < BS.length d =
      let i = fromIntegral (a - s)
      in Just (fromIntegral (BS.index d i) .|. (fromIntegral (BS.index d (i + 1)) `shiftL` 8))
  | otherwise = Nothing

-- * Cartridges

data CartType
  = Std8k     -- ^ CAR type 1: 8 KiB at $A000-$BFFF
  | Std16k    -- ^ CAR type 2: 16 KiB at $8000-$BFFF
  | Xegs32k   -- ^ CAR type 12: XEGS, 4 banks of 8 KiB ('cartBanks')
  | Xegs64k   -- ^ CAR type 13: XEGS, 8 banks
  | Xegs128k  -- ^ CAR type 14: XEGS, 16 banks
  deriving stock (Show, Eq, Enum, Bounded)

-- | XEGS cartridges (the XE Game System's bank-switched ones; values from
-- the atari800 emulator's cartridge.c, CARTRIDGE_XEGS_32/_07_64/_128,
-- and the Altirra HRM 8.1 for the CCTL region): $8000-$9FFF shows the
-- selected 8 KiB bank, $A000-$BFFF the last one, always; a write anywhere
-- in $D500-$D5FF selects bank (value AND (banks - 1)), reads change
-- nothing; bank 0 at power-on, kept by the Reset key (the cartridge port
-- has no reset line, HRM 8.1). 1 for the standard types.
cartBanks :: CartType -> Int
cartBanks t = case t of
  Xegs32k -> 4
  Xegs64k -> 8
  Xegs128k -> 16
  _ -> 1

isXegs :: CartType -> Bool
isXegs t = cartBanks t > 1

data Cart = Cart
  { cartType  :: !CartType
  , cartImage :: !BS.ByteString -- ^ exactly 'cartSize' bytes
  } deriving stock (Show, Eq)

cartSize :: CartType -> Int
cartSize Std8k = 0x2000
cartSize Std16k = 0x4000
cartSize t = cartBanks t * 0x2000

cartBase :: CartType -> Word16
cartBase Std8k = 0xA000
cartBase _ = 0x8000

-- | The cartridge's byte at @a@ ($8000-$BFFF, from 'cartBase'), with
-- XEGS bank @bank@ selected.
cartByte :: Cart -> Int -> Word16 -> Word8
cartByte (Cart t img) bank a
  | isXegs t = if a < 0xA000
      then BS.index img (bank * 0x2000 + fromIntegral (a - 0x8000))
      else BS.index img ((cartBanks t - 1) * 0x2000 + fromIntegral (a - 0xA000))
  | otherwise = BS.index img (fromIntegral (a - cartBase t))

carChecksum :: BS.ByteString -> Word32
carChecksum = BS.foldl' (\a b -> a + fromIntegral b) 0

-- | A @.car@ file.
parseCar :: BS.ByteString -> Either String Cart
parseCar f
  | BS.length f < 16 || BS.take 4 f /= BS.pack [0x43, 0x41, 0x52, 0x54] = Left "no CART header"
  | otherwise = do
      t <- case be 4 of
        1 -> Right Std8k
        2 -> Right Std16k
        12 -> Right Xegs32k
        13 -> Right Xegs64k
        14 -> Right Xegs128k
        n -> Left ("cartridge type " <> show n <> " is not supported (types 1, 2 and the XEGS 12-14)")
      let img = BS.drop 16 f
      if BS.length img /= cartSize t
        then Left ("type " <> show t <> " needs " <> show (cartSize t) <> " bytes, the file has " <> show (BS.length img))
        else if carChecksum img /= be 8
          then Left "checksum mismatch"
          else Right (Cart t img)
 where
  be o = foldl (\a i -> a `shiftL` 8 .|. fromIntegral (BS.index f (o + i))) 0 [0 .. 3] :: Word32

-- | A raw image: 8 or 16 KiB.
parseRawCart :: BS.ByteString -> Either String Cart
parseRawCart f = case BS.length f of
  0x2000 -> Right (Cart Std8k f)
  0x4000 -> Right (Cart Std16k f)
  n -> Left ("a raw cartridge is 8192 or 16384 bytes, not " <> show n)

-- | By file name: @.car@ has a header, @.rom@ / @.bin@ are raw.
parseCart :: FilePath -> BS.ByteString -> Either String Cart
parseCart name f
  | ext == ".car" = parseCar f
  | ext `elem` [".rom", ".bin"] = parseRawCart f
  | otherwise = Left (name <> ": not a cartridge (.car, .rom, .bin)")
 where
  ext = case break (== '.') (reverse name) of
    (e, '.' : _) -> '.' : map toLower (reverse e)
    _ -> ""

-- * Cassettes

-- | A cassette image (@.cas@): its data records in tape order, each as the
-- file has it: the two sync bytes ($55 $55), the control byte ($FC a full
-- record, $FA a partial one, $FE the end of a file), 128 data bytes, then
-- the checksum ('sioChecksum' of all before it). The format is Atari800's
-- and Altirra's (chunks of a 4-byte type, a 16-bit length and a 16-bit aux
-- value, little-endian: "FUJI" first, "baud" the rate, "data" a record
-- with its leading gap in ms as aux); other chunks (FSK and turbo ones) are
-- not kept: the OS's cassette routine can't read them either.
newtype Cas = Cas { casRecords :: [BS.ByteString] }
  deriving stock (Show, Eq)

parseCas :: BS.ByteString -> Either String Cas
parseCas f
  | BS.take 4 f /= fuji = Left "not a cassette image (no FUJI chunk)"
  | otherwise = Cas <$> go f
 where
  fuji = BS.pack [0x46, 0x55, 0x4A, 0x49]
  dataTag = BS.pack [0x64, 0x61, 0x74, 0x61]
  go b
    | BS.null b = Right []
    | BS.length b < 8 = Left "a cassette chunk cut short"
    | BS.length body < n = Left "a cassette chunk longer than the file"
    | BS.take 4 b == dataTag = (body :) <$> go rest
    | otherwise = go rest
   where
    n = fromIntegral (BS.index b 4) .|. (fromIntegral (BS.index b 5) `shiftL` 8) :: Int
    body = BS.take n (BS.drop 8 b)
    rest = BS.drop (8 + n) b

-- | The tape as the board's PL reads it (side track Q: the PS writes it
-- at the media region; "A800.Core.casAnswer"): record k's slot at k * 256,
-- its length first (at most 255: a longer record is cut), then its bytes;
-- a slot of length 0 after the last.
casImage :: Cas -> BS.ByteString
casImage (Cas recs) = BS.concat (map slot recs) <> BS.replicate 256 0
 where
  slot r = let d = BS.take 255 r in BS.take 256 (BS.cons (fromIntegral (BS.length d)) d <> BS.replicate 256 0)

-- | The tape image as the board's PS writes it into DDR ('casImage' after
-- a 16-byte header, "FUJI" and zeros, by which the PL's loader knows a
-- tape: "A800.Loader"; the reader starts after it, as after a .car's).
casImageFile :: Cas -> BS.ByteString
casImageFile c = BS.pack [0x46, 0x55, 0x4A, 0x49] <> BS.replicate 12 0 <> casImage c

-- | The SIO bus's checksum: an 8-bit sum with the carry added back (HRM,
-- "Serial I/O (SIO) Bus": the carry wrap-around checksum).
sioChecksum :: BS.ByteString -> Word8
sioChecksum = BS.foldl' (\a b -> let t = fromIntegral a + fromIntegral b :: Int in fromIntegral (if t > 0xFF then t - 0xFF else t)) 0

-- | A cassette image of records (for our test tapes): a FUJI chunk, 600
-- baud, each record a data chunk (a long leader before the first).
encodeCas :: [BS.ByteString] -> BS.ByteString
encodeCas recs = BS.concat (chunk "FUJI" 0 BS.empty : chunk "baud" 600 BS.empty : zipWith rec' [0 :: Int ..] recs)
 where
  rec' i body = chunk "data" (if i == 0 then 20000 else 250) body
  chunk :: String -> Int -> BS.ByteString -> BS.ByteString
  chunk tag aux body = BS.concat
    [ BS.pack (map (fromIntegral . fromEnum) tag), le (BS.length body), le aux, body ]
  le v = BS.pack [fromIntegral v, fromIntegral (v `shiftR` 8)]

-- | A boot file as tape records (the OS's cassette boot): 128-byte full
-- records ($FC), the last padded, then an end-of-file record ($FE); each
-- with its sync bytes and checksum. The file starts with the boot header
-- (a flag byte, the number of records, the load and the init address).
casBoot :: BS.ByteString -> [BS.ByteString]
casBoot img = map (record 0xFC) (blocks img) <> [record 0xFE (BS.replicate 128 0)]
 where
  blocks b
    | BS.null b = []
    | otherwise = let (h, t) = BS.splitAt 128 b in BS.take 128 (h <> BS.replicate 128 0) : blocks t
  record ctl d = let r = BS.concat [BS.pack [0x55, 0x55, ctl], d] in BS.snoc r (sioChecksum r)

-- * The tape as a signal

-- | A record as the 410 recorder plays it (depth 2: the real recorder,
-- PLAN.md "Cassette images"): its leading gap of mark tone (the chunk's
-- aux, in ms), the baud rate it was written at (the last "baud" chunk
-- before it; 600 if none), its bytes (sync bytes and checksum included),
-- each a start bit 0, eight data bits LSB first, a stop bit 1. The 410
-- turns the FSK tones into these levels on SIO's data line: the machine
-- sees only them.
data TapeRec = TapeRec
  { trGapMs :: !Int
  , trBaud  :: !Int
  , trBytes :: !BS.ByteString
  } deriving stock (Show, Eq)

parseTape :: BS.ByteString -> Either String [TapeRec]
parseTape f
  | BS.take 4 f /= BS.pack [0x46, 0x55, 0x4A, 0x49] = Left "not a cassette image (no FUJI chunk)"
  | otherwise = go 600 f
 where
  go baud b
    | BS.null b = Right []
    | BS.length b < 8 = Left "a cassette chunk cut short"
    | BS.length body < n = Left "a cassette chunk longer than the file"
    | tag == "data" = (TapeRec aux baud body :) <$> go baud rest
    | tag == "baud" && aux > 0 = go aux rest
    | otherwise = go baud rest
   where
    tag = map (toEnum . fromIntegral) (BS.unpack (BS.take 4 b)) :: String
    n = le 4
    aux = le 6
    le i = fromIntegral (BS.index b i) .|. (fromIntegral (BS.index b (i + 1)) `shiftL` 8) :: Int
    body = BS.take n (BS.drop 8 b)
    rest = BS.drop (8 + n) b

-- | The NTSC 800XL's machine clock.
machineHz :: Int
machineHz = 1789773

-- | Machine cycles a record takes on the tape, its gap included.
tapeRecCycles :: TapeRec -> Int
tapeRecCycles r = gapCycles r + (10 * BS.length (trBytes r) * machineHz + trBaud r - 1) `div` trBaud r

gapCycles :: TapeRec -> Int
gapCycles r = trGapMs r * machineHz `div` 1000

-- | The data line @t@ machine cycles into a record ('True' = 1, mark).
tapeLevel :: TapeRec -> Int -> Bool
tapeLevel r t
  | t < g = True
  | byte >= BS.length (trBytes r) = True
  | i == 0 = False
  | i == 9 = True
  | otherwise = testBit (BS.index (trBytes r) byte) (i - 1)
 where
  g = gapCycles r
  (byte, i) = ((t - g) * trBaud r `div` machineHz) `divMod` 10
