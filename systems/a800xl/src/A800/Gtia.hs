{-# LANGUAGE StrictData #-}
{-|
GTIA in Clash (M4 step 4.5). It reproduces the reference model
("A800.Spec.Gtia") colour clock for colour clock; that module has the
rules (register delays, the P/M shifters, collisions, priority, hires,
GTIA modes).

__Ticks__ ('GtiaIn'), in a machine cycle's order: ANTIC's P/M DMA data
('GMissile', 'GPlayer', at its cycle start), the CPU's access ('GRead',
'GWrite'), then the cycle's two colour clocks ('GClock', each with its
AN code and the cycle's 4-bit pixels). 'GInputs' sets the triggers and
console keys.

__The register queue.__ The spec keeps pending changes at absolute colour
clocks; here each entry counts down the colour clocks still to wait
(a write lands 1 to 5 colour clocks after its cycle's first, so there is
never a backlog: entries fall due one clock at a time, in the order they
came, as in the spec's sorted queue). Eight entries are enough: at most
one CPU write and one DMA byte a cycle, each due within three cycles.
-}
module A800.Gtia
  ( GtiaIn (..)
  , GtiaOut (..)
  , Gtia (..)
  , QEntry (..)
  , gtiaInit
  , gtiaStep
  , gtiaCycle
  ) where

import Clash.Prelude

data GtiaIn
  = GMissile (Unsigned 7) Bool (Unsigned 8)          -- ^ cycle, odd line, data
  | GPlayer (Unsigned 7) Bool (Index 4) (Unsigned 8)
  | GRead (Unsigned 5) Bool                          -- ^ register, cartridge sense (RD5)
  | GWrite (Unsigned 7) (Unsigned 5) (Unsigned 8)    -- ^ cycle, register, value
  | GClock (Unsigned 8) (Unsigned 3) (Unsigned 4) (Unsigned 4)
      -- ^ colour clock, AN code, this cycle's 4-bit pixel, mode 10's pixel here
  | GInputs (BitVector 2) (BitVector 3)              -- ^ triggers pressed, console keys held
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFDataX

data GtiaOut = GtiaOut
  { goRead   :: Unsigned 8                 -- ^ a register read's value
  , goColour :: (Unsigned 8, Unsigned 8)   -- ^ a colour clock's two half-clock colours (0 outside the picture)
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFDataX

-- | A pending register change: colour clocks still to wait, register
-- ($00-$1F, or $40+n: missile n's DMA data), value.
data QEntry = QEntry
  { qWait :: Unsigned 3
  , qReg  :: Unsigned 7
  , qVal  :: Unsigned 8
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFDataX

data Gtia = Gtia
  { gGractl  :: Unsigned 8
  , gVdelay  :: Unsigned 8
  , gConsol  :: Unsigned 8
  , gPos     :: Vec 8 (Unsigned 8)
  , gSizeP   :: Vec 4 (Unsigned 2)
  , gSizeM   :: Unsigned 8
  , gColPm   :: Vec 4 (Unsigned 8)
  , gColPf   :: Vec 4 (Unsigned 8)
  , gColBk   :: Unsigned 8
  , gPrior   :: Unsigned 8
  , gLatch   :: Vec 8 (Unsigned 8)
  , gShift   :: Vec 8 (Unsigned 8)
  , gState   :: Vec 8 (Unsigned 2)
  , gColl    :: Vec 8 (Unsigned 8)   -- ^ 0-3 missiles, 4-7 players: playfield bits 0-3, players 4-7
  , gHires   :: Bool
  , gLastAn  :: Unsigned 3
  , gTrig    :: BitVector 2
  , gTrigLat :: BitVector 2
  , gKeys    :: BitVector 3
  , gPrevPair :: Unsigned 4
  , gQueue   :: Vec 8 (Maybe QEntry)
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFDataX

gtiaInit :: Gtia
gtiaInit = Gtia
  { gGractl = 0, gVdelay = 0, gConsol = 0, gPos = repeat 0, gSizeP = repeat 0, gSizeM = 0
  , gColPm = repeat 0, gColPf = repeat 0, gColBk = 0, gPrior = 0
  , gLatch = repeat 0, gShift = repeat 0, gState = repeat 0, gColl = repeat 0
  , gHires = False, gLastAn = 0, gTrig = 0, gTrigLat = 0, gKeys = 0, gPrevPair = 0
  , gQueue = repeat Nothing }

gtiaStep :: Gtia -> GtiaIn -> (Gtia, GtiaOut)
gtiaStep g i = case i of
  GMissile _ odd' v ->
    if not (testBit (gGractl g) 0) then nothing g else
      let mask = if odd' then 0x0F else complement (gVdelay g) .&. 0x0F
          one gq n = if testBit mask (fromIntegral n) then enqueue gq 3 (0x40 + fromIntegral n) ((v `shiftL` (6 - 2 * fromIntegral n)) .&. 0xC0) else gq
      in nothing (foldl one g (indicesI :: Vec 4 (Index 4)))
  GPlayer _ odd' n v ->
    if testBit (gGractl g) 1 && (odd' || not (testBit (gVdelay g) (4 + fromIntegral n)))
      then nothing (enqueue g 3 (0x0D + fromIntegral n) v)
      else nothing g
  GRead r cart -> (g, GtiaOut (readReg g r cart) (0, 0))
  GWrite _ r v -> nothing (writeReg g r v)
  GClock h an pix pix10 -> clock g h an pix pix10
  GInputs t k -> nothing (g { gTrig = t, gTrigLat = gTrigLat g .|. t, gKeys = k })
 where
  nothing q = (q, GtiaOut 0 (0, 0))

-- | A machine cycle's two colour clocks (cycle @x@, AN codes): the 4-bit
-- pixel is this cycle's two codes' low bits; mode 10 shows the last
-- cycle's on the first colour clock (the spec's 'gtiaClocks').
gtiaCycle :: Gtia -> Unsigned 7 -> (Unsigned 3, Unsigned 3) -> (Gtia, ((Unsigned 8, Unsigned 8), (Unsigned 8, Unsigned 8)))
gtiaCycle g x (c1, c2) =
  let pair = (resize (c1 .&. 3) `shiftL` 2) .|. resize (c2 .&. 3) :: Unsigned 4
      h = resize x `shiftL` 1 :: Unsigned 8
      (g1, o1) = gtiaStep g (GClock h c1 pair (gPrevPair g))
      (g2, o2) = gtiaStep g1 (GClock (h + 1) c2 pair pair)
  in (g2 { gPrevPair = pair }, (goColour o1, goColour o2))

-- | Queue a change @d@ colour clocks after the cycle's first (the queue
-- is kept packed at the front, in arrival order).
enqueue :: Gtia -> Unsigned 3 -> Unsigned 7 -> Unsigned 8 -> Gtia
enqueue g d r v =
  let n = used (gQueue g)
  in if n >= 8 then g else g { gQueue = replace n (Just (QEntry d r v)) (gQueue g) }

used :: Vec 8 (Maybe QEntry) -> Unsigned 4
used = foldl (\n e -> maybe n (const (n + 1)) e) 0

readReg :: Gtia -> Unsigned 5 -> Bool -> Unsigned 8
readReg g r cart
  | r < 0x08 = gColl g !! r .&. 15
  | r < 0x10 = gColl g !! (r - 8) `shiftR` 4
  | r <= 0x13 =
      let n = r - 0x10
          pressed = (n < 2 && testBit (gTrig g) (fromIntegral n))
                 || (testBit (gGractl g) 2 && n < 2 && testBit (gTrigLat g) (fromIntegral n))
          released | n == 2 = True
                   | n == 3 = cart
                   | otherwise = not pressed
      in if released then 1 else 0
  | r == 0x14 = 0x0F
  | r == 0x1F = complement (gConsol g .|. (zeroExtend (unpack (gKeys g)) :: Unsigned 8)) .&. 0x0F
  | otherwise = 0x0F

writeReg :: Gtia -> Unsigned 5 -> Unsigned 8 -> Gtia
writeReg g r v
  | r == 0x1D = let g' = g { gGractl = v }
                in if testBit v 2 && not (testBit (gGractl g) 2) then g' { gTrigLat = gTrig g } else g'
  | r == 0x1C = g { gVdelay = v }
  | r == 0x1F = g { gConsol = v }
  | r < 0x08 = enqueue g 5 (resize r) v
  | r < 0x12 = enqueue g 3 (resize r) v
  | r < 0x1B = enqueue g 1 (resize r) v
  | r == 0x1B = enqueue g 2 (resize r) v
  | r == 0x1E = enqueue g 3 (resize r) v
  | otherwise = g

-- | Apply a due change.
apply :: Gtia -> QEntry -> Gtia
apply g (QEntry _ n v)
  | n >= 0x40 = g { gLatch = replace (4 + fromIntegral (n - 0x40) :: Index 8) v (gLatch g) }
  | n < 0x08 = g { gPos = replace (fromIntegral n :: Index 8) v (gPos g) }
  | n < 0x0C = g { gSizeP = replace (fromIntegral (n - 0x08) :: Index 4) (resize v) (gSizeP g) }
  | n == 0x0C = g { gSizeM = v }
  | n < 0x11 = g { gLatch = replace (fromIntegral (n - 0x0D) :: Index 8) v (gLatch g) }
  | n == 0x11 = g { gLatch = fst (splitAt d4 (gLatch g)) ++ map (\sh -> (v `shiftL` sh) .&. 0xC0) (6 :> 4 :> 2 :> 0 :> Nil) }
  | n < 0x16 = g { gColPm = replace (fromIntegral (n - 0x12) :: Index 4) (v .&. 0xFE) (gColPm g) }
  | n < 0x1A = g { gColPf = replace (fromIntegral (n - 0x16) :: Index 4) (v .&. 0xFE) (gColPf g) }
  | n == 0x1A = g { gColBk = v .&. 0xFE }
  | n == 0x1B = g { gPrior = v, gHires = if v .&. 0xC0 /= 0 then False else gHires g }
  | n == 0x1E = g { gColl = repeat 0 }
  | otherwise = g

-- | One colour clock.
clock :: Gtia -> Unsigned 8 -> Unsigned 3 -> Unsigned 4 -> Unsigned 4 -> (Gtia, GtiaOut)
clock g0 h an pix pix10 =
  let -- 1. the hires flag at the end of horizontal blank (PRIOR before this clock's changes)
      blank = an == 1 || an == 2 || an == 3
      lastAn = gLastAn g0
      g1a = g0 { gLastAn = an }
      g1 = if not blank && (lastAn == 2 || lastAn == 3)
             then g1a { gHires = lastAn == 3 && gPrior g0 .&. 0xC0 == 0 } else g1a
      -- 2. changes due now, in arrival order; the others wait a clock less
      due e = maybe False (\q -> qWait q == 0) e
      g2a = foldl (\q e -> if due e then maybe q (apply q) e else q) g1 (gQueue g1)
      g2 = g2a { gQueue = compact (map (\e -> if due e then Nothing else fmap (\q -> q { qWait = qWait q - 1 }) e) (gQueue g1)) }
      prior = gPrior g2
      hires = gHires g2
      -- 3. object triggers (position = this colour clock), outputs
      trig k = gPos g2 !! k == h
      st0 = gState g2
      sh0 = gShift g2
      sh1 = imap (\k s -> if trig k then ((if st0 !! k /= 0 then s `shiftL` 1 else s) .|. gLatch g2 !! k) else s) sh0
      st1 = imap (\k s -> if trig k then 0 else s) st0
      outs = map (\s -> testBit s 7) sh1
      pm = pack (reverse (takeI outs :: Vec 4 Bool)) :: BitVector 4
      mm = pack (reverse (dropI outs :: Vec 4 Bool)) :: BitVector 4
      visible = not blank && h >= 34 && h < 222
      gmode = prior .&. 0xC0
      region = an >= 4
      pfPri :: BitVector 4
      pfPri | gmode == 0x40 || gmode == 0xC0 = 0
            | gmode == 0x80 = if testBit pix10 2 then bit (fromIntegral (pix10 .&. 3)) else 0
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
      -- (missiles in 0-3, players in 4-7)
      (collM, collP) = splitAt d4 (gColl g2)
      hit cl others = cl .|. zeroExtend (unpack pfColl :: Unsigned 4) .|. (zeroExtend (unpack others :: Unsigned 4) `shiftL` 4)
      coll | not visible || (pm .|. mm) == 0 = gColl g2
           | otherwise =
               imap (\k cl -> if testBit mm (fromIntegral k) then hit cl pm else cl) collM
               ++ imap (\k cl -> if testBit pm (fromIntegral k) then hit cl (pm .&. complement (bit (fromIntegral k))) else cl) collP
      -- 5. the picture
      c = colourOf g2 prior pm mm pfPri gmode pix pix10
      pf1 = gColPf g2 !! (1 :: Index 4)
      lumOn b = if b then (c .&. 0xF0) .|. (pf1 .&. 0x0E) else c
      col | not visible = (0, 0)
          | hires && region && gmode == 0 = (lumOn (testBit an 1), lumOn (testBit an 0))
          | otherwise = (c, c)
      -- 6. the shifters move on
      sizes = gSizeP g2 ++ map (\sh -> resize ((gSizeM g2 `shiftR` sh) .&. 3)) (0 :> 2 :> 4 :> 6 :> Nil)
      adv sz (s, st) =
        if s == 0 && st == 0 then (s, st) else
          let st' = transition sz st
          in (if st' == 0 then s `shiftL` 1 else s, st')
      moved = zipWith adv sizes (zip sh1 st1)
      g3 = g2 { gShift = map fst moved, gState = map snd moved, gColl = coll }
  in (g3, GtiaOut 0 col)

transition :: Unsigned 2 -> Unsigned 2 -> Unsigned 2
transition size st = case size of
  0 -> 0
  1 -> if st == 0 || st == 2 then 1 else 0
  2 -> if st == 1 || st == 2 then 2 else 0
  _ -> st + 1

-- | Remove the gaps due entries leave, keeping the order.
compact :: Vec 8 (Maybe QEntry) -> Vec 8 (Maybe QEntry)
compact = snd . foldl put (0 :: Unsigned 4, repeat Nothing)
 where
  put (n, acc) e = case e of
    Nothing -> (n, acc)
    Just x -> (n + 1, replace n (Just x) acc)

-- | The priority logic and the GTIA modes' colours ('A800.Spec.Gtia').
colourOf :: Gtia -> Unsigned 8 -> BitVector 4 -> BitVector 4 -> BitVector 4 -> Unsigned 8 -> Unsigned 4 -> Unsigned 4 -> Unsigned 8
colourOf g prior pm mm pf gmode pix pix10 =
  let fifth = testBit prior 4
      multi = testBit prior 5
      pl k = testBit pm k || (not fifth && testBit mm k)
      p10 k = gmode == 0x80 && pix10 < 4 && fromIntegral pix10 == k
      pp k = pl k || p10 k
      p0 = pp (0 :: Int)
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
      cpm k = gColPm g !! (k :: Index 4)
      cpf k = gColPf g !! (k :: Index 4)
      bk = gColBk g
      pfCol = pick sf0 (cpf 0) .|. pick sf1 (cpf 1) .|. pick sf2 (cpf 2) .|. pick sf3 (cpf 3)
      pmCol = pick sp0 (cpm 0) .|. pick sp1 (cpm 1) .|. pick sp2 (cpm 2) .|. pick sp3 (cpm 3)
      anyPm = sp0 || sp1 || sp2 || sp3
      anyP = p01 || p23
      pix8 = zeroExtend pix :: Unsigned 8
  in case gmode of
       0x40 | anyP -> pmCol
            | p5 -> cpf 3 .|. pix8
            | otherwise -> bk .|. pix8
       0xC0 | anyP -> pmCol
            | p5 && sf3 -> (cpf 3 .|. (pix8 `shiftL` 4)) .&. (if pix == 0 then 0xF0 else 0xFF)
            | otherwise -> (bk .|. (pix8 `shiftL` 4)) .&. (if pix == 0 then 0xF0 else 0xFF)
       0x80 | pix10 >= 8 && pix10 < 12 && not anyPm && not (p5 && sf3) -> bk
       _ -> pmCol .|. pfCol .|. pick sb bk
