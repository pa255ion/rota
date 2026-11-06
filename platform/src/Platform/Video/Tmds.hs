{-|
DVI 1.0 TMDS encoding (side track G step 4), for the Arty Z7-20's HDMI
output, which is wired straight to FPGA pins (the ZedBoard's goes through
the ADV7511, which does this itself).

Each of the three channels turns one 8-bit colour component, or two
control bits during blanking, into a 10-bit symbol per pixel clock (DVI
1.0, section 3.2.2, "T.M.D.S. encoding algorithm", figure 3-5):

* __Data (DE high).__ Stage 1 minimises transitions: @q_m@ is the byte
  chained with XOR, or with XNOR when it has more than four ones (or
  exactly four and bit 0 is 0); @q_m[8]@ says which (1 = XOR). Stage 2
  balances DC: depending on the running disparity @cnt@ (ones minus
  zeros sent so far) and on @q_m@'s own balance, the low 8 bits are sent
  inverted or not, and bit 9 says which (1 = inverted).
* __Control (DE low).__ One of four fixed symbols for (C1, C0), and the
  disparity restarts at 0.

Channel 0 (blue) carries HSYNC as C0 and VSYNC as C1; channels 1 and 2
(green, red) send C1 = C0 = 0 ('tmdsChannels'). The sync levels are sent
as they are, so 640×480's negative syncs stay negative, as on VGA.

'tmdsEncode' is the pure transition function (unit-tested against a
reference written from the specification, Test.Platform.Tmds);
'tmdsDecode' is the receiver's side, for tests and the harness's TMDS
sink to agree on. The 10:1 serialisers and output buffers are in
"Platform.Board.ArtyZ7".
-}
module Platform.Video.Tmds
  ( TmdsIn (..)
  , Disparity
  , tmdsEncode
  , tmdsControlSymbol
  , tmdsDecode
  , tmdsChannels
  , tmdsEncoder
  , tmdsVideo
  ) where

import Clash.Prelude

import Platform.Video.VgaTiming (Rgb888 (..), VgaOut (..))

-- | What a channel sends in one pixel clock.
data TmdsIn
  = TmdsData (BitVector 8)  -- ^ a colour component (DE high)
  | TmdsControl Bit Bit     -- ^ C1, C0 (DE low)
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFDataX

-- | The running disparity: ones minus zeros sent since the last control
-- period. Always even; the tests show it stays within -8 .. 8, so 5 bits
-- would do; 6 leave room.
type Disparity = Signed 6

-- | The control symbols, bit 9 first as DVI 1.0 writes them (3.2.2):
-- (C1, C0) = 00 → 1101010100, 01 → 0010101011, 10 → 0101010100,
-- 11 → 1010101011.
tmdsControlSymbol :: Bit -> Bit -> BitVector 10
tmdsControlSymbol c1 c0 = case (c1, c0) of
  (0, 0) -> 0b1101010100
  (0, _) -> 0b0010101011
  (_, 0) -> 0b0101010100
  _      -> 0b1010101011

-- | One encoder step: the disparity before the symbol, the input, and
-- the disparity after it with the symbol (bit 0 is sent first).
tmdsEncode :: Disparity -> TmdsIn -> (Disparity, BitVector 10)
tmdsEncode _ (TmdsControl c1 c0) = (0, tmdsControlSymbol c1 c0)
tmdsEncode cnt (TmdsData d) = (cnt', q)
 where
  ones8 :: BitVector 8 -> Disparity
  ones8 = sum . map (\b -> if b == 1 then 1 else 0) . bv2v
  -- Stage 1: XOR (qm8 = 1) or XNOR (qm8 = 0) chain.
  useXnor = ones8 d > 4 || (ones8 d == 4 && d ! (0 :: Int) == 0)
  qm8 :: Bit
  qm8 = if useXnor then 0 else 1
  qm :: BitVector 8
  qm = v2bv (reverse (scanl1 link (reverse (bv2v d))))
  link prev b = if useXnor then complement (prev `xor` b) else prev `xor` b
  -- Stage 2: DC balance.
  n1 = ones8 qm
  n0 = 8 - n1
  qm8i :: Disparity
  qm8i = if qm8 == 1 then 1 else 0
  (q, cnt')
    | cnt == 0 || n1 == n0 =
        ( pack (complement qm8) ++# pack qm8 ++# (if qm8 == 1 then qm else complement qm)
        , if qm8 == 0 then cnt + (n0 - n1) else cnt + (n1 - n0) )
    | (cnt > 0 && n1 > n0) || (cnt < 0 && n0 > n1) =
        ( (1 :: BitVector 1) ++# pack qm8 ++# complement qm
        , cnt + 2 * qm8i + (n0 - n1) )
    | otherwise =
        ( (0 :: BitVector 1) ++# pack qm8 ++# qm
        , cnt - 2 * (1 - qm8i) + (n1 - n0) )

-- | A receiver's view of a symbol: the four control symbols, else data
-- (bit 9 undoes the inversion, bit 8 picks XOR or XNOR). Every symbol
-- decodes to something; an encoder only sends 460 of the 1024 (and the
-- receiver can't tell the rest apart, which the tests don't need).
tmdsDecode :: BitVector 10 -> TmdsIn
tmdsDecode s
  | s == tmdsControlSymbol 0 0 = TmdsControl 0 0
  | s == tmdsControlSymbol 0 1 = TmdsControl 0 1
  | s == tmdsControlSymbol 1 0 = TmdsControl 1 0
  | s == tmdsControlSymbol 1 1 = TmdsControl 1 1
  | otherwise = TmdsData (v2bv (reverse (head bs :> zipWith unlink (init bs) (tail bs))))
 where
  lo8 = slice d7 d0 s
  qm  = if s ! (9 :: Int) == 1 then complement lo8 else lo8
  bs  = reverse (bv2v qm)            -- q_m[0..7], bit 0 first
  isXor = s ! (8 :: Int) == 1
  -- bit 0 is sent as it is; bit i is q_m[i] xor/xnor q_m[i-1]
  unlink :: Bit -> Bit -> Bit
  unlink prev b = if isXor then prev `xor` b else complement (prev `xor` b)

-- | The three channels' inputs for one pixel clock: blue, green, red in
-- the visible area, the syncs on channel 0 outside it.
tmdsChannels :: VgaOut -> Rgb888 -> Vec 3 TmdsIn
tmdsChannels o (Rgb888 r g b)
  | vgaActive o = TmdsData b :> TmdsData g :> TmdsData r :> Nil
  | otherwise   = TmdsControl (vgaVSync o) (vgaHSync o)
               :> TmdsControl 0 0 :> TmdsControl 0 0 :> Nil

-- | One channel's encoder; the symbol is registered (control symbol
-- 00 during reset).
tmdsEncoder :: HiddenClockResetEnable dom => Signal dom TmdsIn -> Signal dom (BitVector 10)
tmdsEncoder = mealy step (0, tmdsControlSymbol 0 0)
 where
  step (cnt, q) i = let (cnt', q') = tmdsEncode cnt i in ((cnt', q'), q)

-- | The video stream (timing and colour of the same clock, as
-- "M0b.Pattern".m0bStream gives it) to the three channels' symbols,
-- one clock later.
tmdsVideo
  :: HiddenClockResetEnable dom
  => Signal dom VgaOut -> Signal dom Rgb888 -> Signal dom (Vec 3 (BitVector 10))
tmdsVideo o c = bundle (map tmdsEncoder (unbundle (tmdsChannels <$> o <*> c)))
