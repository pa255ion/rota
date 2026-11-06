{-|
The ADV7511 register table (PLAN.md M0d step 2), kept as data: each entry
is one register update with the source of its value. The sequencer in
"Platform.Video.Adv7511" applies the entries in order after every
hot-plug; this module is separate so that the sequencer can turn the list
into a 'Vec' with Template Haskell.

__Configuration:__ input ID 1 (16-bit YCbCr 4:2:2 on separate syncs, 1×
clock, style 1, right-justified, 8 bits per component), which puts Cb/Cr
on the chip's D[23:16] and Y on D[15:8] (HW Table 7), i.e. HD_D[15:8] and
HD_D[7:0] on the ZedBoard; the colour space converter turning BT.601
("SDTV") limited-range YCbCr, as "Platform.Video.Ycbcr422" produces it,
into full-range RGB; DVI mode (no infoframes); RGB 4:4:4 out.

__Sources__ (values copied, not code; files in the git-ignored
@local/reference/linux-adv7511/@):

* [PG] ADV7511W Programming Guide, Rev. B (the ADV7511W's register map;
  ADI's public programming guide for the ADV7511 family);
* [HW] ADV7511 Hardware User's Guide, Rev. D;
* cross-checks: [LX] Linux @drivers/gpu/drm/bridge/adv7511/@ and [ZED]
  ADI's ZedBoard device tree @zynq-zed-adv7511.dtsi@ (address 0x39,
  yuv422, style 1, right-justified, clock delay 0).

__Where we differ from [LX]__ (checked 2026-10-02): its CSC coefficients
are [PG] Table 33, for /HDTV/ (BT.709) input; ours are Table 35 (SDTV),
matching our BT.601 converter. [PG] Table 14 fixes only 0x9A[7:1] and
0x9D[1:0], so those are masked here ([LX] writes 0x9D = 0x61). [LX] also
writes 0xD0 = 0x0C, which sets 0xD0[6:4] (DDR negative-edge delay) to
-1200 ps instead of the "no delay" 0b011; [PG] says 0xD0[3:2] can be any
value for ID 1 with a 1× clock, so we leave 0xD0 alone. 0x55 (AVI
InfoFrame byte, [LX] fixed list) is dropped: DVI mode sends no infoframes.
-}
module Platform.Video.Adv7511.Table
  ( TableEntry (..)
  , adv7511Addr
  , adv7511Table
  , adv7511HdmiAudioTable
  , regPower
  , regStatus
  , regRevision
  ) where

import Clash.Prelude
import qualified Prelude as P

-- | One update: @reg := (old .&. complement mask) .|. (value .&. mask)@.
-- A mask of 0xFF is a plain write (no read first).
data TableEntry = TableEntry
  { teReg     :: BitVector 8
  , teMask    :: BitVector 8
  , teValue   :: BitVector 8
  , teSource  :: String
  , teChecked :: Bool          -- ^ checked against [PG] / [HW]
  }

-- | 7-bit address of the main register map: 0x72 as an 8-bit write
-- address, PD/AD low at power-up ([PG] 4.1, [HW] 6.1.5); [ZED] @hdmi\@39@.
adv7511Addr :: BitVector 7
adv7511Addr = 0x39

-- | Power: bit 6 = power down ([PG] 4.7.1). It is in 0x00..0x93, which is
-- reset to defaults while HPD is low ([PG] Table 74), and its default has
-- the bit set, so reading it back set means "the chip lost its setup".
regPower :: BitVector 8
regPower = 0x41

-- | Status: bit 6 = HPD state, bit 5 = monitor sense ([PG] Table 2).
regStatus :: BitVector 8
regStatus = 0x42

-- | Chip revision, read only ([PG] section 5, main map 0x00).
regRevision :: BitVector 8
regRevision = 0x00

entry :: BitVector 8 -> BitVector 8 -> BitVector 8 -> String -> TableEntry
entry r m v s = TableEntry r m v s True

-- | [PG] Table 35, SDTV YCbCr (limited range) to RGB (full range), for
-- 0x19..0x2F; 0x18 (A1 high bits with CSC enable and mode) comes last.
csc :: [(BitVector 8, BitVector 8)]
csc = P.zip [0x19 ..]
  [       0x69, 0x04, 0xAC, 0x00, 0x00, 0x1C, 0x81
  , 0x1C, 0xBC, 0x04, 0xAD, 0x1E, 0x6E, 0x02, 0x20
  , 0x1F, 0xFE, 0x04, 0xAD, 0x08, 0x1A, 0x1B, 0xA9 ]

adv7511Table :: [TableEntry]
adv7511Table =
  [ entry regPower 0x40 0x00 "[PG] 3, 4.2.1, 4.7.1: 0x41[6] = 0 powers up (HPD must be high)"
  -- [PG] 4.2.9 Table 14, "Fixed Registers That Must Be Set" after power-up
  , entry 0x98 0xFF 0x03 "[PG] Table 14: 0x98 = 0x03"
  , entry 0x9A 0xFE 0xE0 "[PG] Table 14: 0x9A[7:1] = 0b1110000"
  , entry 0x9C 0xFF 0x30 "[PG] Table 14: 0x9C = 0x30"
  , entry 0x9D 0x03 0x01 "[PG] Table 14: 0x9D[1:0] = 0b01"
  , entry 0xA2 0xFF 0xA4 "[PG] Table 14: 0xA2 = 0xA4"
  , entry 0xA3 0xFF 0xA4 "[PG] Table 14: 0xA3 = 0xA4"
  , entry 0xE0 0xFF 0xD0 "[PG] Table 14: 0xE0 = 0xD0"
  , entry 0xF9 0xFF 0x00 "[PG] Table 14, 4.1: 0xF9 (fixed I2C address) = 0x00"
  -- input format, [PG] 4.3.2
  , entry 0x15 0x0F 0x01 "[PG] 4.3.2: 0x15[3:0] = 0001, YCbCr 4:2:2 separate syncs; [HW] Table 4"
  , entry 0x16 0xFF 0x38 "[PG] 4.3.2, Table 45: 0x16 [7] 4:4:4 out, [5:4] 11 = 8 bit, [3:2] 10 = style 1, [1] 0, [0] 0 = RGB out"
  , entry 0x48 0x18 0x08 "[PG] 4.3.2: 0x48[4:3] = 01 right justified; [HW] Table 7: Cb/Cr D[23:16], Y D[15:8]"
  , entry 0xBA 0xE0 0x60 "[PG] 4.3.2: 0xBA[7:5] = 011, no clock delay ([HW]: setup/hold movable by 0.4 ns steps)"
  , entry 0x17 0x62 0x00 "[PG] 4.3.2, 4.3.3: 0x17[6:5] sync polarities passed through, [1] 4:3"
  , entry 0xFB 0x06 0x00 "[PG] 4.3.3: 0xFB[2:1] = 00, no low refresh rate"
  ]
  -- colour space converter, [PG] 4.3.7.1 Table 35
  <> [ entry r 0xFF v "[PG] Table 35: SDTV YCbCr limited to RGB full, coefficient byte" | (r, v) <- csc ]
  <> [ entry 0x18 0xFF 0xE6 "[PG] Table 35, Table 45: 0x18 = 0xE6, CSC on, mode 11 (x4), A1 high bits"
     -- [PG] 4.2.2
     , entry 0xAF 0x02 0x00 "[PG] 4.2.2: 0xAF[1] = 0, DVI mode"
     ]

-- | HDMI mode with stereo PCM audio from the S/PDIF input at 48 kHz (PLAN.md
-- step 2.9, the Game Boy): 'adv7511Table' without its last entry (DVI
-- mode), then the audio set-up and the infoframes, then HDMI mode last
-- (no packets go out before it). Researched 2026-10-03 from [PG] Rev. B
-- (page numbers) and cross-checked with Linux's adv7511_audio.c, which
-- differs only in writes that don't apply to S/PDIF (I2S channel status
-- 0x12-0x14, an unused manual CTS 0x07-0x09). With S/PDIF the chip takes
-- the sample rate and word length from the stream's channel status
-- ([PG] 4.4.1.2, 4.4.3) and needs no MCLK ([HW] 6.1.3.3).
adv7511HdmiAudioTable :: [TableEntry]
adv7511HdmiAudioTable = P.init adv7511Table <>
  [ entry 0x0A 0xFF 0x11 "[PG] 4.4.1 Table 53 (p.69), Table 61: [7] 0 CTS automatic, [6:4] 001 S/PDIF, [3:2] 00, [1:0] 01 MCLK ratio (unused)"
  , entry 0x01 0xFF 0x00 "[PG] Table 60 (p.79): N = 6144 for 48 kHz, CTS measured; N[19:16]"
  , entry 0x02 0xFF 0x18 "[PG] Table 60: N[15:8]"
  , entry 0x03 0xFF 0x00 "[PG] Table 60: N[7:0]"
  , entry 0x15 0xF0 0x20 "[PG] 4.3.4 (p.40), 4.4.1.2: 0x15[7:4] = 0010, 48 kHz (used for pixel repetition while 0x0C[7] = 1)"
  , entry 0x0B 0x80 0x80 "[PG] 4.4.1.2 (p.75), map 0x0B: [7] = 1 S/PDIF receiver on; [6:5] 0 MCLK internal, [4:1] fixed, kept"
  , entry 0x4A 0x60 0x60 "[PG] 4.2.5 (p.20), Table 47, Table 65: AVI and audio infoframe update on while writing them"
  , entry 0x55 0xFF 0x00 "[PG] 4.3.8.1, Table 47 (p.60): AVI byte 1, Y1Y0 = 00 RGB (as 0x16[7:6]), no AFD, bars, scan info"
  , entry 0x56 0xFF 0x18 "[PG] Table 47 (p.60-61): AVI byte 2, C = 00, M = 01 4:3, R = 1000 same as picture"
  , entry 0x57 0xFF 0x00 "[PG] Table 47 (p.61): AVI byte 3, Q = 00 default range (full RGB for VIC 1)"
  , entry 0x73 0xFF 0x01 "[PG] 4.4.4 (p.86): audio infoframe byte 1, CT = 0, CC = 001 two channels"
  , entry 0x74 0xFF 0x00 "[PG] Table 64/65 (p.87-88): audio infoframe byte 2, SF/SS from the stream"
  , entry 0x75 0xFF 0x00 "[PG] Table 65: audio infoframe byte 3"
  , entry 0x76 0xFF 0x00 "[PG] Table 65: audio infoframe byte 4, CA = 0x00 FL/FR"
  , entry 0x77 0xFF 0x00 "[PG] Table 65: audio infoframe byte 5, no level shift"
  , entry 0x4A 0x60 0x00 "[PG] 4.2.5: update bits off, the new infoframes go out"
  , entry 0x44 0x78 0x78 "[PG] Table 47/61/65, map (p.150): N/CTS, audio sample, AVI and audio infoframe packets on (all 1 at reset)"
  , entry 0xAF 0x02 0x02 "[PG] 4.2.2 Table 4 (p.18): 0xAF[1] = 1, HDMI mode"
  ]
