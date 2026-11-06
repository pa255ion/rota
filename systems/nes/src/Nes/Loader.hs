{-|
Cartridge loading from the PS (PLAN.md step 3.8): the NES's side of the PS
link ("Platform.Ps.Link"; the PS side is @platform/ps/@, the SD card menu,
and @make load SYSTEM=nes@ over J14).

The PS streams the @.nes@ file unchanged from link address 0, the core
held: the 16-byte header (iNES or NES 2.0), then PRG ROM, then CHR ROM.
The PL routes each byte by its file offset ('loadRoute'): the header into
a register, PRG from offset 16 into the PRG block RAM, CHR after PRG
(the header's PRG size) into the CHR block RAM. When hold falls, the
header is decoded again here ('headerCfg', the same rules as
"Nes.Cartridge".@parseHeader@, @checkSupported@ and
"Nes.Mapper".'cartCfgFromHeader', checked against them by @nes-spec@), and
the core starts the cartridge only if it is supported. Writes anywhere
else below 'Platform.Ps.Link.sharedBase' are ignored, as are writes while
the core runs.

Bits 0 to 7 of the PS → PL word are the desktop's buttons in the Game
Boy's order (Right Left Up Down A B Select Start; the desktop uses the
same keys for both, docs/systems/nes.md "Input"): 'padFromLink' turns them
into the NES controller's order.
-}
module Nes.Loader
  ( HeaderBytes
  , LoadWrite (..)
  , loadRoute
  , headerCfg
  , headerPrgBytes
  , padFromLink
  ) where

import Clash.Prelude

import Nes.Mapper (CartCfg (..), MapperKind (..))
import Platform.Ps.Link (LinkAddr)

-- | The 16 header bytes of the file.
type HeaderBytes = Vec 16 (BitVector 8)

-- | Where a byte of the file goes.
data LoadWrite
  = LwHeader (Index 16) (BitVector 8)
  | LwPrg (Unsigned 19) (BitVector 8)
  | LwChr (Unsigned 18) (BitVector 8)
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFDataX

-- | A link write while the core is held, routed by the file offset; the
-- header so far (the PRG size comes from it, and the PS sends the header
-- first).
loadRoute :: HeaderBytes -> Bool -> Maybe (LinkAddr, BitVector 8) -> Maybe LoadWrite
loadRoute _ False _ = Nothing
loadRoute _ _ Nothing = Nothing
loadRoute h True (Just (a, d))
  | a < 16 = Just (LwHeader (bitCoerce (resize a :: Unsigned 4)) d)
  | off < prg = if off < 0x80000 then Just (LwPrg (resize off) d) else Nothing
  | chr < 0x40000 = Just (LwChr (resize chr) d)
  | otherwise = Nothing
 where
  off = resize a - 16 :: Unsigned 32
  prg = maybe 0 id (headerPrgBytes h)
  chr = off - prg

byte :: HeaderBytes -> Index 16 -> BitVector 8
byte = (!!)

nes20 :: HeaderBytes -> Bool
nes20 h = (byte h 7) .&. 0x0C == 0x08

-- | iNES: bytes 12 to 15 not zero means a header from an old tool
-- (DiskDude!); its byte 7 and 8 are then ignored.
junk :: HeaderBytes -> Bool
junk h = any (/= 0) (drop d12 h)

-- | A ROM size in bytes (NES 2.0: 12 bits of units, or 2^E × (2M + 1)),
-- 'Nothing' above 4 MiB (more than any supported cartridge; the
-- exponent form can be huge).
romSize :: BitVector 8 -> BitVector 4 -> Unsigned 32 -> Maybe (Unsigned 32)
romSize lo8 hi4 unit
  | hi4 == 0xF = let e = unpack (slice d7 d2 lo8) :: Unsigned 6
                     m = unpack (resize (slice d1 d0 lo8)) :: Unsigned 32
                 in if e > 22 then Nothing else Just ((1 `shiftL` fromEnum e) * (2 * m + 1))
  | otherwise = Just (unpack (resize (hi4 ++# lo8)) * unit)

shiftSize :: BitVector 4 -> Unsigned 32
shiftSize 0 = 0
shiftSize n = 64 `shiftL` fromEnum (unpack n :: Unsigned 4)

-- | The PRG ROM size the header says ('Nothing': too big to be one we run).
headerPrgBytes :: HeaderBytes -> Maybe (Unsigned 32)
headerPrgBytes h
  | nes20 h = romSize (byte h 4) (slice d3 d0 (byte h 9)) 0x4000
  | otherwise = Just (unpack (resize (byte h 4)) * 0x4000)

-- | The cartridge configuration, if M3 supports the cartridge
-- ('Nes.Cartridge.checkSupported': mappers 0, 1, 2, 3, 4, 5, 7; PRG ≤ 512 KiB
-- (MMC1 ≤ 256 KiB), CHR ROM ≤ 256 KiB, CHR RAM ≤ 8 KiB, PRG RAM ≤ 8 KiB (MMC5 64 KiB); no four-screen, no
-- trainer).
headerCfg :: HeaderBytes -> Maybe CartCfg
headerCfg h
  | sig /= 0x4E45531A = Nothing
  | otherwise = do
      kind <- case mapper of
        0 -> Just MNrom
        1 -> Just MMmc1
        2 -> Just MUxrom
        3 -> Just MCnrom
        4 -> Just MMmc3
        5 -> Just MMmc5
        7 -> Just MAxrom
        _ -> Nothing
      prg <- headerPrgBytes h
      chrRom <- if nes20 h then romSize (byte h 5) (slice d7 d4 (byte h 9)) 0x2000
                           else Just (unpack (resize (byte h 5)) * 0x2000)
      let chrRam | nes20 h = shiftSize (slice d3 d0 (byte h 11)) + shiftSize (slice d7 d4 (byte h 11))
                 | chrRom == 0 = 0x2000
                 | otherwise = 0
          prgRam | nes20 h = shiftSize (slice d3 d0 (byte h 10)) + shiftSize (slice d7 d4 (byte h 10))
                 | otherwise = 0x2000 * max 1 (if junk h then 0 else unpack (resize (byte h 8)))
          chrBytes = if chrRom == 0 then max 0x2000 chrRam else chrRom
      if prg > 0x80000 || (mapper == 1 && prg > 0x40000) || chrRom > 0x40000 || chrRam > 0x2000
           || prgRam > (if mapper == 5 then 0x10000 else 0x2000)
           || testBit f6 3 || testBit f6 2
        then Nothing
        else Just CartCfg
          { ccMapper = kind
          , ccPrgMask = truncateB (prg - 1)
          , ccChrMask = truncateB (chrBytes - 1)
          , ccChrRam = chrRom == 0
          , ccVertical = testBit f6 0
          , ccPrgRam = prgRam > 0 || mapper == 5
          }
 where
  sig = pack (take d4 h)
  f6 = byte h 6
  f7 = byte h 7
  lo = unpack (resize (slice d7 d4 f6)) :: Unsigned 12
  mid = unpack (resize (slice d7 d4 f7)) `shiftL` 4 :: Unsigned 12
  mapper
    | nes20 h = lo .|. mid .|. (unpack (resize (slice d3 d0 (byte h 8))) `shiftL` 8)
    | junk h = lo
    | otherwise = lo .|. mid

-- | The desktop's buttons (PS link bits 0 to 7, the Game Boy's order:
-- Right Left Up Down A B Select Start) in the controller's order (bit 0
-- A, 1 B, 2 Select, 3 Start, 4 Up, 5 Down, 6 Left, 7 Right).
padFromLink :: BitVector 8 -> Unsigned 8
padFromLink k = unpack (pack (reverse (map (boolToBit . testBit k) from)))
 where
  -- the link bit for controller bits 0 to 7
  from = 4 :> 5 :> 6 :> 7 :> 2 :> 3 :> 1 :> 0 :> Nil :: Vec 8 Int
