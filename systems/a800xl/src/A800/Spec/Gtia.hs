{-# LANGUAGE MultiWayIf #-}
{-|
GTIA in the reference model (M4 steps 4.1 and 4.3): the read registers,
players and missiles, collisions, priority, colours, hires and the GTIA
modes, rendered one colour clock at a time from ANTIC's AN codes.

Sources: Altirra Hardware Reference Manual (HRM) chapter 6 and the
register reference (14.3); Altirra's GTIA as the cross-check for the
pipeline delays and the P/M shifter (values and rules, not code).

__Coordinates.__ A line is 228 colour clocks; colour clock /h/ is the
P/M position register value, and the two colour clocks of machine cycle
/x/ are /h/ = 2x and 2x + 1 ('gtiaClocks'). A normal playfield covers
$30-$CF; collisions and the picture are only processed in $22-$DD and
outside blanking (ANTIC sends blanking codes there and in vertical
blank).

__Register writes__ land a few colour clocks after the write's cycle
2x (HRM 6.10, Altirra's queue): colours +1, PRIOR +2, GRAFPn/GRAFM,
SIZEPn/SIZEM, HITCLR and P/M DMA data +3, HPOSPn/HPOSMn +5. The colour
registers drop bit 0.

__Players and missiles__ (HRM 6.5). Each of the eight objects has a
data latch (GRAFPn/GRAFM, or ANTIC's DMA if GRACTL bits 1/0 allow it and,
on even lines, VDELAY doesn't mask it), a shift register and a two-bit
shift state. When the colour clock equals its position the latch is ORed
into the shift register (a running shift state first shifts once and
resets). Each colour clock shows the register's top bit, then the state
moves to (state + 1) AND size (the %10 size locks up from states 1 and 2,
table 15) and the register shifts when it reaches 0. They run through
blanking too.

__Collisions__ (HRM 6.6) are latched for any pair of objects present on
the same colour clock with playfield or each other; in hires modes a
playfield collision is PF2 where a hires pixel is on. GTIA modes 9 and 11
have no playfield collisions; mode 10 has them for its PF codes.

__Priority__ (HRM 6.7): the HRM's equations, the mixing of priority
mode 0, the fifth player (missiles as PF3), multicolor players; the
selected colour registers are ORed. Hires (ANTIC modes 2, 3, F, unless a
GTIA mode is on): the playfield is PF2, and each half colour clock
whose pixel is on takes PF1's luminance, over everything.

__GTIA modes__ (HRM 6.9): two colour clocks' AN0-1 bits form a 4-bit
pixel. Mode 9: COLBK's hue, the pixel's luminance (16 levels). Mode 11:
the pixel's hue with COLBK's luminance (0 for pixel 0). Mode 10: pixels
0-3 the player colours (as players for priority, without collisions),
4-7 and 12-15 PF0-PF3, 8-11 the background; one colour clock later.

The picture is a byte per half colour clock: the Atari colour (hue in
bits 4-7, luminance in bits 0-3).
-}
module A800.Spec.Gtia
  ( Gtia
  , newGtia
  , gtiaRead
  , gtiaWrite
  , gtiaReg
  , setTriggers
  , setConsoleKeys
  , gtiaMissile
  , gtiaPlayer
  , gtiaClocks
  , gtiaAnFrame
  , gtiaFrame
  , gtiaPixels
  , anWidth
  , frameWidth
  , GtiaSnap (..)
  , gtiaSnapshot
  ) where

import Prelude

import Control.Monad (forM_, when)
import Control.Monad.ST (ST)
import Data.Bits
import Data.STRef
import qualified Data.Vector.Unboxed.Mutable as MV
import Data.Word

data Gtia s = Gtia
  { gtRegs  :: !(MV.MVector s Word8) -- ^ the last value written to each of the 32 addresses (as the CPU wrote it)
  , gtAct   :: !(MV.MVector s Int)   -- ^ the registers in effect (after their delay)
  , gtInput :: !(MV.MVector s Word8) -- ^ 0: triggers (bit n = TRIGn pressed), 1: console keys pressed (bits 0-2)
  , gtObj   :: !(MV.MVector s Int)   -- ^ per object (0-3 players, 4-7 missiles): latch, shifter, state
  , gtColl  :: !(MV.MVector s Int)   -- ^ 0-3 missiles, 4-7 players: playfield bits 0-3, players 4-7
  , gtSt    :: !(MV.MVector s Int)
  , gtQueue :: !(STRef s [(Int, Int, Int)]) -- ^ pending register changes (colour clock, register, value), sorted
  , gtAn    :: !(MV.MVector s Word8) -- ^ the AN codes of the frame, 'anWidth' per line
  , gtPic   :: !(MV.MVector s Word8) -- ^ the picture, 'frameWidth' per line
  }

-- | Colour clocks per line.
anWidth :: Int
anWidth = 228

-- | Picture bytes per line (two per colour clock).
frameWidth :: Int
frameWidth = 2 * anWidth

-- Registers in effect (gtAct): 0-7 positions, 8-11 player sizes, 12
-- missile sizes, 13-16 COLPM0-3, 17-20 COLPF0-3, 21 COLBK, 22 PRIOR.
aPos, aSizeP, aSizeM, aColPm, aColPf, aColBk, aPrior :: Int
aPos = 0
aSizeP = 8
aSizeM = 12
aColPm = 13
aColPf = 17
aColBk = 21
aPrior = 22

-- Scalar state (gtSt).
sHires, sLastAn, sTrigLatch, sPrevPair :: Int
sHires = 0      -- hires interpretation on this line
sLastAn = 1     -- the last AN code seen (blank detection)
sTrigLatch = 2  -- latched triggers (bit n = TRIGn was pressed)
sPrevPair = 3   -- the previous cycle's 4-bit pixel (mode 10's delay)

newGtia :: ST s (Gtia s)
newGtia = do
  g <- Gtia <$> MV.replicate 32 0 <*> MV.replicate 23 0 <*> MV.replicate 2 0
            <*> MV.replicate 24 0 <*> MV.replicate 16 0 <*> MV.replicate 5 0
            <*> newSTRef [] <*> MV.replicate (anWidth * 262) 0
            <*> MV.replicate (frameWidth * 262) 0
  pure g

-- | The value last written to register @r@.
gtiaReg :: Gtia s -> Int -> ST s Word8
gtiaReg g r = MV.unsafeRead (gtRegs g) (r .&. 0x1F)

-- * Reads

-- | Read register @r@; @cart@ is the cartridge sense (RD5).
gtiaRead :: Gtia s -> Bool -> Word16 -> ST s Word8
gtiaRead g cart r = case r .&. 0x1F of
  n | n < 0x08 -> lo n                           -- MnPF, PnPF
    | n < 0x0C -> hi (n - 0x08)                  -- MnPL
    | n < 0x10 -> hi (n - 0x08)                  -- PnPL
    | n <= 0x13 -> do
        trig <- MV.unsafeRead (gtInput g) 0
        latched <- MV.unsafeRead (gtSt g) sTrigLatch
        gractl <- MV.unsafeRead (gtRegs g) 0x1D
        let i = fromIntegral n - 0x10
            pressed = testBit trig i || (testBit gractl 2 && testBit latched i)
            released | i == 2 = True
                     | i == 3 = cart
                     | otherwise = not pressed
        pure (if released then 1 else 0)
    | n == 0x14 -> pure 0x0F
    | n == 0x1F -> do
        out <- MV.unsafeRead (gtRegs g) 0x1F
        keys <- MV.unsafeRead (gtInput g) 1
        pure (complement (out .|. (keys .&. 7)) .&. 0x0F)
    | otherwise -> pure 0x0F
 where
  -- per object (0-3 missiles, 4-7 players): playfield bits 0-3, player bits 4-7
  lo n = fromIntegral . (.&. 15) <$> MV.unsafeRead (gtColl g) (fromIntegral n)
  hi n = fromIntegral . (`shiftR` 4) <$> MV.unsafeRead (gtColl g) (fromIntegral n)

-- * Writes

-- | Write register @r@ on cycle @x@ (machine cycle within the line).
gtiaWrite :: Gtia s -> Int -> Word16 -> Word8 -> ST s ()
gtiaWrite g x r v = do
  let n = fromIntegral (r .&. 0x1F) :: Int
      h = 2 * x
      val = fromIntegral v :: Int
  oldGractl <- MV.unsafeRead (gtRegs g) 0x1D
  MV.unsafeWrite (gtRegs g) n v
  if | n == 0x1D && not (testBit oldGractl 2 && testBit val 2) && testBit val 2 -> do
         trig <- MV.unsafeRead (gtInput g) 0
         MV.unsafeWrite (gtSt g) sTrigLatch (fromIntegral trig)
     | n < 0x08 -> queue g (h + 5) n val
     | n < 0x0D -> queue g (h + 3) n val
     | n < 0x12 -> queue g (h + 3) n val
     | n < 0x1B -> queue g (h + 1) n val
     | n == 0x1B -> queue g (h + 2) n val
     | n == 0x1E -> queue g (h + 3) n val
     | otherwise -> pure ()

queue :: Gtia s -> Int -> Int -> Int -> ST s ()
queue g h n v = modifySTRef' (gtQueue g) (insertBy3 (h, n, v))
 where
  insertBy3 e@(p, _, _) xs = let (a, b) = span (\(q, _, _) -> q <= p) xs in a <> (e : b)

-- | Apply a register change.
apply :: Gtia s -> Int -> Int -> ST s ()
apply g n v
  | n < 0x08 = act (aPos + n) v
  | n < 0x0C = act (aSizeP + n - 0x08) (v .&. 3)
  | n == 0x0C = act aSizeM v
  | n < 0x11 = obj (n - 0x0D) 0 v                    -- GRAFP0-3: the data latch
  | n == 0x11 = forM_ [0 .. 3] $ \i -> obj (4 + i) 0 ((v `shiftL` (6 - 2 * i)) .&. 0xC0)
  | n < 0x16 = act (aColPm + n - 0x12) (v .&. 0xFE)
  | n < 0x1A = act (aColPf + n - 0x16) (v .&. 0xFE)
  | n == 0x1A = act aColBk (v .&. 0xFE)
  | n == 0x1B = do
      act aPrior v
      when (v .&. 0xC0 /= 0) (MV.unsafeWrite (gtSt g) sHires 0)
  | n == 0x1E = forM_ [0 .. 7] $ \i -> MV.unsafeWrite (gtColl g) i 0
  | otherwise = pure ()
 where
  act i = MV.unsafeWrite (gtAct g) i
  obj i k = MV.unsafeWrite (gtObj g) (3 * i + k)

-- * P/M DMA

-- | Missile DMA data, from ANTIC's cycle 0 of line @y@ (odd?).
gtiaMissile :: Gtia s -> Int -> Bool -> Word8 -> ST s ()
gtiaMissile g x odd' v = do
  gractl <- MV.unsafeRead (gtRegs g) 0x1D
  vdelay <- MV.unsafeRead (gtRegs g) 0x1C
  when (testBit gractl 0) $ do
    let mask = if odd' then 0x0F else complement vdelay .&. 0x0F
        val = fromIntegral v :: Int
    forM_ [0 .. 3] $ \i -> when (testBit mask i) $
      queue g (2 * x + 3) (0x40 + i) ((val `shiftL` (6 - 2 * i)) .&. 0xC0)

-- | Player @n@ DMA data.
gtiaPlayer :: Gtia s -> Int -> Bool -> Int -> Word8 -> ST s ()
gtiaPlayer g x odd' n v = do
  gractl <- MV.unsafeRead (gtRegs g) 0x1D
  vdelay <- MV.unsafeRead (gtRegs g) 0x1C
  when (testBit gractl 1 && (odd' || not (testBit vdelay (4 + n)))) $
    queue g (2 * x + 3) (0x0D + n) (fromIntegral v)

-- * Inputs

-- | Joystick triggers: bit 0 = TRIG0 pressed, bit 1 = TRIG1.
setTriggers :: Gtia s -> Word8 -> ST s ()
setTriggers g t = do
  MV.unsafeWrite (gtInput g) 0 t
  MV.unsafeModify (gtSt g) (.|. fromIntegral t) sTrigLatch

-- | Console keys held: bit 0 START, bit 1 SELECT, bit 2 OPTION.
setConsoleKeys :: Gtia s -> Word8 -> ST s ()
setConsoleKeys g = MV.unsafeWrite (gtInput g) 1

-- * Rendering

-- | The two colour clocks of machine cycle @x@ on line @y@.
gtiaClocks :: Gtia s -> (Int, Int) -> Int -> Int -> ST s ()
gtiaClocks g (y, x) c1 c2 = do
  -- a new line: changes due past its end (a write late in the last line)
  -- move to this one's colour clocks
  when (x == 0) $ modifySTRef' (gtQueue g) (map (\(p, n, v) -> (p - anWidth, n, v)))
  let i = y * anWidth + 2 * x
  MV.unsafeWrite (gtAn g) i (fromIntegral c1)
  MV.unsafeWrite (gtAn g) (i + 1) (fromIntegral c2)
  -- the 4-bit pixel of this cycle (GTIA modes)
  let pair = ((c1 .&. 3) `shiftL` 2) .|. (c2 .&. 3)
  prevPair <- MV.unsafeRead (gtSt g) sPrevPair
  MV.unsafeWrite (gtSt g) sPrevPair pair
  clock g y (2 * x) c1 pair prevPair
  clock g y (2 * x + 1) c2 pair pair

-- | One colour clock @h@ with AN code @an@; @pix@ is the 4-bit pixel of
-- this cycle, @pix10@ the one mode 10 shows here (one colour clock late).
clock :: Gtia s -> Int -> Int -> Int -> Int -> Int -> ST s ()
clock g y h an pix pix10 = do
  -- 1. the hires flag at the end of horizontal blank, from PRIOR as it
  -- was before this colour clock's changes (Altirra latches it with the
  -- changes due before the first visible colour clock)
  lastAn <- MV.unsafeRead (gtSt g) sLastAn
  MV.unsafeWrite (gtSt g) sLastAn an
  prior0 <- MV.unsafeRead (gtAct g) aPrior
  let blank = an == 1 || an == 2 || an == 3
  when (not blank && (lastAn == 2 || lastAn == 3)) $
    MV.unsafeWrite (gtSt g) sHires (if lastAn == 3 && prior0 .&. 0xC0 == 0 then 1 else 0)
  -- 2. register changes due by now (P/M DMA data uses registers $40+)
  q <- readSTRef (gtQueue g)
  let (due, rest) = span (\(p, _, _) -> p <= h) q
  writeSTRef (gtQueue g) rest
  forM_ due $ \(_, n, v) -> if n >= 0x40 then MV.unsafeWrite (gtObj g) (3 * (4 + n - 0x40)) v else apply g n v
  prior <- MV.unsafeRead (gtAct g) aPrior
  hires <- (/= 0) <$> MV.unsafeRead (gtSt g) sHires
  -- 3. object triggers, outputs (bits 0-3 players, 4-7 missiles)
  outs <- objects h
  let pm = outs .&. 15
      mm = outs `shiftR` 4
      visible = not blank && h >= 34 && h < 222
      gmode = prior .&. 0xC0
      region = an >= 4
      -- the playfield as one-hot PF0-PF3 bits, for priority and collisions
      pfPri | gmode == 0x40 || gmode == 0xC0 = 0
            | gmode == 0x80 = mode10Pf pix10
            | hires && region = 4
            | otherwise = case an of
                4 -> 1
                5 -> 2
                6 -> 4
                7 -> 8
                _ -> 0
      pfColl | hires = if region && an .&. 3 /= 0 then 4 else 0
             | otherwise = pfPri
  -- 4. collisions
  when (visible && outs /= 0) $ do
    forM_ [0 .. 3] $ \i -> when (testBit pm i) $ do
      MV.unsafeModify (gtColl g) (.|. (pfColl .|. (pm .&. complement (bit i)) `shiftL` 4)) (4 + i)
    forM_ [0 .. 3] $ \i -> when (testBit mm i) $
      MV.unsafeModify (gtColl g) (.|. (pfColl .|. pm `shiftL` 4)) i
  -- 5. the picture
  col <- if not visible then pure (0, 0) else do
    c <- colourOf g prior pm mm pfPri gmode pix pix10
    if hires && region && gmode == 0
      then do
        pf1 <- MV.unsafeRead (gtAct g) (aColPf + 1)
        let lumOn b = if b then (c .&. 0xF0) .|. (pf1 .&. 0x0E) else c
        pure (lumOn (testBit an 1), lumOn (testBit an 0))
      else pure (c, c)
  let o = y * frameWidth + 2 * h
  MV.unsafeWrite (gtPic g) o (fromIntegral (fst col))
  MV.unsafeWrite (gtPic g) (o + 1) (fromIntegral (snd col))
  -- 6. the shifters move on
  forM_ [0 .. 7] advance
 where
  objects hh = go 0 0
   where
    go i acc
      | i >= 8 = pure acc
      | otherwise = do
          pos <- MV.unsafeRead (gtAct g) (aPos + i)
          when (pos == hh) $ do
            st <- MV.unsafeRead (gtObj g) (3 * i + 2)
            sh <- MV.unsafeRead (gtObj g) (3 * i + 1)
            latch <- MV.unsafeRead (gtObj g) (3 * i)
            let sh' = if st /= 0 then (sh `shiftL` 1) .&. 0xFF else sh
            MV.unsafeWrite (gtObj g) (3 * i + 2) 0
            MV.unsafeWrite (gtObj g) (3 * i + 1) (sh' .|. latch)
          sh <- MV.unsafeRead (gtObj g) (3 * i + 1)
          go (i + 1) (if testBit sh 7 then setBit acc i else acc)
  advance i = do
    sh <- MV.unsafeRead (gtObj g) (3 * i + 1)
    st <- MV.unsafeRead (gtObj g) (3 * i + 2)
    -- an empty, resting shifter stays as it is
    when (sh /= 0 || st /= 0) $ do
      size <- if i < 4 then MV.unsafeRead (gtAct g) (aSizeP + i)
                       else (\z -> (z `shiftR` (2 * (i - 4))) .&. 3) <$> MV.unsafeRead (gtAct g) aSizeM
      let st' = transition size st
      MV.unsafeWrite (gtObj g) (3 * i + 2) st'
      when (st' == 0) $ MV.unsafeWrite (gtObj g) (3 * i + 1) ((sh `shiftL` 1) .&. 0xFF)
  -- (state + 1) AND size, with the %10 lockup (HRM table 15, Altirra's table)
  transition size st = case size of
    0 -> 0
    1 -> if st == 0 || st == 2 then 1 else 0
    2 -> if st == 1 || st == 2 then 2 else 0
    _ -> (st + 1) .&. 3

-- | Mode 10's playfield for priority and collisions: PF0-PF3 for
-- 4-7 and 12-15.
mode10Pf :: Int -> Int
mode10Pf v
  | v .&. 4 /= 0 = bit (v .&. 3)
  | otherwise = 0

-- | The colour of a colour clock from the priority logic (HRM 6.7, 6.9);
-- @pm@, @mm@: players and missiles present (bits 0-3).
colourOf :: Gtia s -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> ST s Int
colourOf g prior pm mm pf gmode pix pix10 = do
  let reg = MV.unsafeRead (gtAct g)
  cpm0 <- reg aColPm
  cpm1 <- reg (aColPm + 1)
  cpm2 <- reg (aColPm + 2)
  cpm3 <- reg (aColPm + 3)
  cpf0 <- reg aColPf
  cpf1 <- reg (aColPf + 1)
  cpf2 <- reg (aColPf + 2)
  cpf3 <- reg (aColPf + 3)
  bk <- reg aColBk
  let fifth = testBit prior 4
      multi = testBit prior 5
      -- missiles join their players, or PF3 as the fifth player
      pl i = testBit pm i || (not fifth && testBit mm i)
      -- mode 10's player codes act as players
      p10 i = gmode == 0x80 && pix10 < 4 && pix10 == i
      pp i = pl i || p10 i
      p0 = pp 0
      p1 = pp 1
      p2 = pp 2
      p3 = pp 3
      p5 = fifth && mm /= 0
      pf0 = testBit pf 0
      pf1 = testBit pf 1
      pf2 = testBit pf 2
      pf3 = testBit pf 3 || p5
      pri0 = testBit prior 0
      pri1 = testBit prior 1
      pri2 = testBit prior 2
      pri3 = testBit prior 3
      pri01 = pri0 || pri1
      pri12 = pri1 || pri2
      pri23 = pri2 || pri3
      pri03 = pri0 || pri3
      p01 = p0 || p1
      p23 = p2 || p3
      pf01 = pf0 || pf1
      pf23 = pf2 || pf3
      sp0 = p0 && not (pf01 && pri23) && not (pri2 && pf23)
      sp1 = p1 && not (pf01 && pri23) && not (pri2 && pf23) && (not p0 || multi)
      sp2 = p2 && not p01 && not (pf23 && pri12) && not (pf01 && not pri0)
      sp3 = p3 && not p01 && not (pf23 && pri12) && not (pf01 && not pri0) && (not p2 || multi)
      sf3 = pf3 && not (p23 && pri03) && not (p01 && not pri2)
      sf0 = pf0 && not (p23 && pri0) && not (p01 && pri01) && not sf3
      sf1 = pf1 && not (p23 && pri0) && not (p01 && pri01) && not sf3
      sf2 = pf2 && not (p23 && pri03) && not (p01 && not pri2) && not sf3
      sb = not p01 && not p23 && not pf01 && not pf23
      pick b c = if b then c else 0
      pfCol = pick sf0 cpf0 .|. pick sf1 cpf1 .|. pick sf2 cpf2 .|. pick sf3 cpf3
      pmCol = pick sp0 cpm0 .|. pick sp1 cpm1 .|. pick sp2 cpm2 .|. pick sp3 cpm3
      anyPm = sp0 || sp1 || sp2 || sp3
      anyP = p01 || p23 -- modes 9/11: the playfield drops out under any player
  pure $ case gmode of
    0x40 | anyP -> pmCol
         | p5 -> cpf3 .|. pix
         | otherwise -> bk .|. pix
    0xC0 | anyP -> pmCol
         | p5 && sf3 -> (cpf3 .|. (pix `shiftL` 4)) .&. (if pix == 0 then 0xF0 else 0xFF)
         | otherwise -> (bk .|. (pix `shiftL` 4)) .&. (if pix == 0 then 0xF0 else 0xFF)
    0x80 | pix10 >= 8 && pix10 < 12 && not anyPm && not (p5 && sf3) -> bk
    _ -> pmCol .|. pfCol .|. pick sb bk

-- * Views

-- | What a save state takes from GTIA (side track N,
-- "A800.Spec.SaveState"): per register $D000-$D01F the value in effect
-- (positions, sizes, colours without bit 0, PRIOR; VDELAY, GRACTL and
-- CONSOL as written; 0 for GRAFPn, GRAFM and HITCLR, whose data is the
-- latches'), the objects' data latches (players 0-3, missiles 0-3), the
-- collisions (missiles 0-3, players 0-3), the trigger latch, and whether
-- no write is on its way.
data GtiaSnap = GtiaSnap
  { gsRegs       :: ![Int]
  , gsLatches    :: ![Int]
  , gsColl       :: ![Int]
  , gsTrigLatch  :: !Int
  , gsQueueEmpty :: !Bool
  }

gtiaSnapshot :: Gtia s -> ST s GtiaSnap
gtiaSnapshot g = GtiaSnap
  <$> mapM reg [0 .. 31]
  <*> mapM (\i -> MV.unsafeRead (gtObj g) (3 * i)) [0 .. 7]
  <*> mapM (MV.unsafeRead (gtColl g)) [0 .. 7]
  <*> MV.unsafeRead (gtSt g) sTrigLatch
  <*> (null <$> readSTRef (gtQueue g))
 where
  act = MV.unsafeRead (gtAct g)
  raw r = fromIntegral <$> MV.unsafeRead (gtRegs g) r
  reg r
    | r < 0x08 = act (aPos + r)
    | r < 0x0C = act (aSizeP + r - 0x08)
    | r == 0x0C = act aSizeM
    | r < 0x12 = pure 0
    | r < 0x16 = act (aColPm + r - 0x12)
    | r < 0x1A = act (aColPf + r - 0x16)
    | r == 0x1A = act aColBk
    | r == 0x1B = act aPrior
    | r == 0x1E = pure 0
    | otherwise = raw r

-- | The AN codes of the last frame (line-major, 'anWidth' per line).
gtiaAnFrame :: Gtia s -> ST s [Word8]
gtiaAnFrame g = mapM (MV.unsafeRead (gtAn g)) [0 .. anWidth * 262 - 1]

-- | The picture (line-major, 'frameWidth' bytes per line, 262 lines).
gtiaFrame :: Gtia s -> ST s [Word8]
gtiaFrame g = mapM (MV.unsafeRead (gtPic g)) [0 .. frameWidth * 262 - 1]

-- | The two picture bytes of colour clock @h@ on line @y@.
gtiaPixels :: Gtia s -> Int -> Int -> ST s (Word8, Word8)
gtiaPixels g y h = do
  let o = y * frameWidth + 2 * h
  (,) <$> MV.unsafeRead (gtPic g) o <*> MV.unsafeRead (gtPic g) (o + 1)
