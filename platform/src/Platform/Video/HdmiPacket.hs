{-|
HDMI data islands, the packet layer (side track G phase 3, step 1): what
the Arty's HDMI output sends in the blanking to carry sound, as pure
functions. "Platform.Video.Hdmi" (step 2) places them on the lines.

Sources: the HDMI 1.4b specification, section 5.2 (data island period,
packets, TERC4, guard bands, preambles) and 8.2 (InfoFrames; their
layouts are CEA-861's), as open HDMI implementations describe them (the
specification itself isn't freely published); values copied, not code.
The board check (a monitor taking the stream as HDMI with sound) is the
final judge.

* __TERC4__ ('terc4'): during a data island each channel carries 4 bits
  per pixel as one of 16 10-bit symbols.
* __Packets__ ('Packet'): a 3-byte header and four 7-byte subpackets,
  each with an 8-bit BCH ECC ('ecc', generator 1 + x^6 + x^7 + x^8, fed
  bit 0 first; HDMI 1.4b 5.2.3.5), sent over 32 pixels: channel 0 bit 2
  carries the header's bit i in pixel i, channel 1 bit k subpacket k's
  bit 2i, channel 2 bit k its bit 2i + 1; channel 0 bits 0 and 1 are
  HSYNC and VSYNC, bit 3 is 0 in a packet's first pixel and 1 after
  ('islandNibbles').
* __Packet types__: Null (0), Audio Clock Regeneration (1: N and CTS, so
  the sink rebuilds the 128 x fs clock from the TMDS clock), Audio Sample
  (2: up to four 2-channel samples, IEC 60958 V, U, C, P bits, the B flag
  at a 192-frame block's start), the AVI InfoFrame (0x82: RGB, VIC 1 =
  640x480p60, 4:3) and the Audio InfoFrame (0x84: 2-channel L-PCM).
-}
module Platform.Video.HdmiPacket
  ( -- * Symbols
    terc4
  , terc4Decode
  , videoGuard
  , islandGuard
    -- * Packets
  , Packet (..)
  , ecc
  , headerBits
  , subpacketBits
  , islandNibbles
    -- * Packet types
  , nullPacket
  , acrPacket
  , AudioFrame (..)
  , audioSamplePacket
  , infoFrame
  , aviInfoFrame
  , audioInfoFrame
  ) where

import Clash.Prelude

import Platform.Audio.Spdif (channelStatus)

-- | TERC4 (HDMI 1.4b table 5-5): the symbol for 4 data bits, bit 0 sent
-- first.
terc4 :: BitVector 4 -> BitVector 10
terc4 d = terc4Table !! d

terc4Table :: Vec 16 (BitVector 10)
terc4Table =
     0b1010011100 :> 0b1001100011 :> 0b1011100100 :> 0b1011100010
  :> 0b0101110001 :> 0b0100011110 :> 0b0110001110 :> 0b0100111100
  :> 0b1011001100 :> 0b0100111001 :> 0b0110011100 :> 0b1011000110
  :> 0b1010001110 :> 0b1001110001 :> 0b0101100011 :> 0b1011000011 :> Nil

-- | The data bits of a TERC4 symbol, if it is one.
terc4Decode :: BitVector 10 -> Maybe (BitVector 4)
terc4Decode s = findIndex (== s) terc4Table >>= Just . pack

-- | The video guard band (2 pixels before active video): channels 0, 1, 2.
videoGuard :: Vec 3 (BitVector 10)
videoGuard = 0b1011001100 :> 0b0100110011 :> 0b1011001100 :> Nil

-- | A data island's guard band (2 pixels before and after it): channel 0 is
-- TERC4 of 1, 1, VSYNC, HSYNC (bits 3 to 0), channels 1 and 2 a fixed
-- symbol.
islandGuard :: Bit -> Bit -> Vec 3 (BitVector 10)
islandGuard hs vs = terc4 (0b11 ++# pack vs ++# pack hs) :> 0b0100110011 :> 0b0100110011 :> Nil

-- | A packet: header bytes HB0 (the type), HB1, HB2 and four subpackets of
-- seven bytes (SB0 to SB6); the ECCs are computed when it is sent.
data Packet = Packet
  { pHeader :: Vec 3 (BitVector 8)
  , pSub    :: Vec 4 (Vec 7 (BitVector 8))
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFDataX

-- | The BCH ECC of bits sent bit 0 first (an LFSR, HDMI 1.4b figure 5-5's
-- form): each bit shifts in, and the generator 1 + x^6 + x^7 + x^8 feeds
-- back as 0x83.
ecc :: KnownNat n => BitVector n -> BitVector 8
ecc bits = foldl step 0 (reverse (bv2v bits))
 where
  step e b = (e `shiftR` 1) `xor` (if lsb e `xor` b == 1 then 0x83 else 0)

-- | The header as sent: bit i in pixel i, HB0's bit 0 first, the ECC in
-- bits 24 to 31.
headerBits :: Packet -> BitVector 32
headerBits p = ecc d ++# d
 where
  d = pack (reverse (pHeader p))

-- | A subpacket as sent: SB0's bit 0 first, the ECC in bits 56 to 63.
subpacketBits :: Vec 7 (BitVector 8) -> BitVector 64
subpacketBits sb = ecc d ++# d
 where
  d = pack (reverse sb)

-- | The three channels' 4 data bits in pixel @i@ of a packet (before
-- TERC4), with the sync levels.
islandNibbles :: Bit -> Bit -> Packet -> Index 32 -> Vec 3 (BitVector 4)
islandNibbles hs vs p i = ch0 :> ch1 :> ch2 :> Nil
 where
  hdr = headerBits p
  subs = map subpacketBits (pSub p)
  ch0 = pack (i /= 0) ++# pack (hdr ! i) ++# pack vs ++# pack hs
  bitOf k j = (subs !! k) ! (2 * fromEnum i + j)
  ch1 = pack (map (\k -> bitOf k 0) (3 :> 2 :> 1 :> 0 :> Nil :: Vec 4 Int))
  ch2 = pack (map (\k -> bitOf k 1) (3 :> 2 :> 1 :> 0 :> Nil :: Vec 4 Int))

nullPacket :: Packet
nullPacket = Packet (repeat 0) (repeat (repeat 0))

-- | Audio Clock Regeneration: N and CTS (20 bits each), the same in all
-- four subpackets (HDMI 1.4b 5.3.3: SB1 to SB3 CTS, SB4 to SB6 N, big
-- end first).
acrPacket :: Unsigned 20 -> Unsigned 20 -> Packet
acrPacket n cts = Packet (0x01 :> 0 :> 0 :> Nil) (repeat sb)
 where
  c = pack cts
  nn = pack n
  sb = 0 :> zeroExtend (slice d19 d16 c) :> slice d15 d8 c :> slice d7 d0 c
         :> zeroExtend (slice d19 d16 nn) :> slice d15 d8 nn :> slice d7 d0 nn :> Nil

-- | One stereo sample for an Audio Sample packet, with its frame's number
-- within the 192-frame block (channel status bit, the B flag at 0).
data AudioFrame = AudioFrame
  { afLeft  :: Signed 16
  , afRight :: Signed 16
  , afIndex :: Index 192
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFDataX

-- | An Audio Sample packet (HDMI 1.4b 5.3.4, layout 0: 2 channels): up to
-- four frames, one per subpacket. HB1 bits 3 to 0: which subpackets hold
-- a sample; HB2 bits 7 to 4: B (a block's first frame). Each sample as
-- 24 bits with the 16-bit value in the top two bytes, then V = 0, U = 0,
-- C = channel status bit, P = even parity over the 24 bits and V, U, C,
-- for left (SB6 bits 0 to 3) and right (bits 4 to 7).
audioSamplePacket :: Vec 4 (Maybe AudioFrame) -> Packet
audioSamplePacket fs = Packet (0x02 :> hb1 :> hb2 :> Nil) (map subOf fs)
 where
  present = map (maybe 0 (const 1)) fs :: Vec 4 Bit
  blockStart = map (maybe 0 (\f -> boolToBit (afIndex f == 0))) fs :: Vec 4 Bit
  hb1 = 0b0000 ++# pack (reverse present)
  hb2 = pack (reverse blockStart) ++# 0b0000
  subOf Nothing = repeat 0
  subOf (Just f) = 0 :> lo l :> hi l :> 0 :> lo r :> hi r :> flags :> Nil
   where
    l = pack (afLeft f)
    r = pack (afRight f)
    cs = channelStatus (afIndex f)
    par x = boolToBit (odd (popCount x)) `xor` cs   -- V = U = 0
    flags = pack (par r) ++# pack cs ++# (0 :: BitVector 2) ++# pack (par l) ++# pack cs ++# (0 :: BitVector 2)
  lo x = slice d7 d0 x
  hi x = slice d15 d8 x

-- | An InfoFrame of a type, version and payload: PB0 is the checksum
-- (header and payload summing to 0), PB1 onwards the payload; subpacket k
-- holds PB(7k) to PB(7k + 6).
infoFrame :: KnownNat n => BitVector 8 -> BitVector 8 -> Vec n (BitVector 8) -> Packet
infoFrame ty ver payload = Packet hdr subs
 where
  n = length payload
  len = fromIntegral n :: BitVector 8
  hdr = ty :> ver :> len :> Nil
  checksum = negate (sum hdr + sum payload)
  pb :: Index 28 -> BitVector 8
  pb i
    | i == 0 = checksum
    | fromEnum i <= n = payload !! (fromEnum i - 1)
    | otherwise = 0
  subs = unconcatI (map pb indicesI) :: Vec 4 (Vec 7 (BitVector 8))

-- | The AVI InfoFrame (CEA-861 version 2, 13 bytes): RGB, no active
-- format or bars or scan information (PB1 = 0); 4:3 picture, active
-- format as the picture (PB2 = 0x18); VIC 1, 640x480p60 (PB4); no pixel
-- repetition.
aviInfoFrame :: Packet
aviInfoFrame = infoFrame 0x82 0x02 (0x00 :> 0x18 :> 0x00 :> 0x01 :> 0x00 :> repeat 0 :: Vec 13 (BitVector 8))

-- | The Audio InfoFrame (CEA-861, version 1, 10 bytes): coding type and
-- sample rate "refer to the stream header", 2 channels (CC = 1),
-- channel allocation 0 (front left, front right).
audioInfoFrame :: Packet
audioInfoFrame = infoFrame 0x84 0x01 (0x01 :> 0x00 :> 0x00 :> 0x00 :> 0x00 :> repeat 0 :: Vec 10 (BitVector 8))
