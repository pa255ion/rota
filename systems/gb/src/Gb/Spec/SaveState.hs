{-|
Save states in the reference model (side track L, PLAN.md L.5): a
'Snapshot' is a copy of everything the spec's 'Machine' holds that a run
depends on (registers, scalar state, the memories, cartridge RAM, the
sound unit, this line's objects, both frames, the serial port), with what
it belongs to (the model, the cartridge mode, the ROM). 'loadState' puts
it back into a machine made from the same ROM and model, after which the
run goes on exactly as it would have from the moment of 'saveState'.

What is not state: the ROM (it must be the same: checked), the test
instrumentation ('mTrace', 'mHook' and the trace switch, which stay as the
loading machine has them).

The hardware's states have their own layout (the core's registers are
not the spec's); this is the definition of what a state must restore,
and the properties in gb-spec check it.

__L.6c__ (loading through the CPU): 'specTable' is the state table
("Gb.StateTable") of a machine at rest ('specAtRest'), and
'runOverlay' runs a machine from its reset through a restore routine
(the ROM overlay the PS builds from a table: @platform/ps/gb_restore.zig@)
until the routine's last byte is read, as the hardware's overlay does.
-}
module Gb.Spec.SaveState
  ( Snapshot (..)
  , RomId (..)
  , romId
  , saveState
  , loadState
    -- * Loading through the CPU (L.6c)
  , specTable
  , specAtRest
  , runOverlay
  ) where

import Prelude

import Control.Monad (when)
import Control.Monad.ST (ST)
import Data.Bits
import Data.STRef
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import Data.Word

import Gb.Cartridge (Mbc (..))
import Gb.Hooks (Serial)
import Gb.Spec.Apu (apuChannel)
import Gb.Spec.Cpu (step)
import Gb.Spec.Machine (peek)
import Gb.Spec.State
import Gb.StateTable

-- | Which cartridge a state belongs to: its title bytes (0x134 to
-- 0x143), its length and the sum of its bytes as the global checksum
-- computes it (of the image itself, not the header's claim).
data RomId = RomId
  { riTitle :: !(U.Vector Word8)
  , riSize  :: !Int
  , riSum   :: !Word16
  } deriving stock (Show, Eq)

romId :: U.Vector Word8 -> RomId
romId rom = RomId (U.slice 0x134 16 rom) (U.length rom) (fromIntegral (U.sum (U.map fromIntegral (U.ifilter notSum rom)) :: Int))
 where
  notSum i _ = i /= 0x14E && i /= 0x14F

data Snapshot = Snapshot
  { snModel   :: !Model
  , snCgbMode :: !Bool
  , snRom     :: !RomId
  , snRegs    :: !(U.Vector Word8)
  , snSt      :: !(U.Vector Int)
  , snRam     :: !(U.Vector Word8)
  , snCram    :: !(U.Vector Word8)
  , snFb      :: !(U.Vector Word8)
  , snLast    :: !(U.Vector Word8)
  , snApu     :: !(U.Vector Int)
  , snSpr     :: !(U.Vector Int)
  , snVram1   :: !(U.Vector Word8)
  , snWramX   :: !(U.Vector Word8)
  , snPal     :: !(U.Vector Word8)
  , snFbC     :: !(U.Vector Word16)
  , snLastC   :: !(U.Vector Word16)
  , snSerial  :: !Serial
  } deriving stock (Show, Eq)

saveState :: Machine s -> ST s Snapshot
saveState m =
  Snapshot (mModel m) (mCgbMode m) (romId (mRom m))
    <$> U.freeze (mRegs m) <*> U.freeze (mSt m) <*> U.freeze (mRam m) <*> U.freeze (mCram m)
    <*> U.freeze (mFb m) <*> U.freeze (mLast m) <*> U.freeze (mApu m) <*> U.freeze (mSpr m)
    <*> U.freeze (mVram1 m) <*> U.freeze (mWramX m) <*> U.freeze (mPal m)
    <*> U.freeze (mFbC m) <*> U.freeze (mLastC m) <*> readSTRef (mSerial m)

-- | Into a machine of the same model and ROM; refused (nothing changed)
-- otherwise, or if a memory's size differs.
loadState :: Machine s -> Snapshot -> ST s (Either String ())
loadState m s
  | snModel s /= mModel m = pure (Left ("a state of the " <> show (snModel s) <> ", this is the " <> show (mModel m)))
  | snCgbMode s /= mCgbMode m = pure (Left "a state of the other cartridge mode")
  | snRom s /= romId (mRom m) = pure (Left "a state of another cartridge")
  | not sizesOk = pure (Left "a state of another layout")
  | otherwise = do
      trace <- getSt m iTRACE
      copy (mRegs m) (snRegs s)
      copy (mSt m) (snSt s)
      copy (mRam m) (snRam s)
      copy (mCram m) (snCram s)
      copy (mFb m) (snFb s)
      copy (mLast m) (snLast s)
      copy (mApu m) (snApu s)
      copy (mSpr m) (snSpr s)
      copy (mVram1 m) (snVram1 s)
      copy (mWramX m) (snWramX s)
      copy (mPal m) (snPal s)
      copy (mFbC m) (snFbC s)
      copy (mLastC m) (snLastC s)
      writeSTRef (mSerial m) (snSerial s)
      -- the instrumentation is the loading machine's
      setSt m iTRACE trace
      pure (Right ())
 where
  sizesOk = and
    [ MU.length (mRegs m) == U.length (snRegs s), MU.length (mSt m) == U.length (snSt s)
    , MU.length (mRam m) == U.length (snRam s), MU.length (mCram m) == U.length (snCram s)
    , MU.length (mFb m) == U.length (snFb s), MU.length (mLast m) == U.length (snLast s)
    , MU.length (mApu m) == U.length (snApu s), MU.length (mSpr m) == U.length (snSpr s)
    , MU.length (mVram1 m) == U.length (snVram1 s), MU.length (mWramX m) == U.length (snWramX s)
    , MU.length (mPal m) == U.length (snPal s)
    , MU.length (mFbC m) == U.length (snFbC s), MU.length (mLastC m) == U.length (snLastC s) ]
  copy :: MU.Unbox a => MU.MVector s a -> U.Vector a -> ST s ()
  copy dst src = U.copy dst src

-- * Loading through the CPU (L.6c)

-- | The state table of a machine ("Gb.StateTable"; the DMG's fields).
specTable :: forall s. Machine s -> ST s (U.Vector Word8)
specTable m = do
  t <- MU.replicate tableSize 0
  let put :: Int -> Int -> ST s ()
      put o v = MU.write t o (fromIntegral v)
      put16 o v = put o (v .&. 0xFF) >> put (o + 1) ((v `shiftR` 8) .&. 0xFF)
      st i = getSt m i
      io a = fromIntegral <$> ramRead m a
  mapM_ (\(o, r) -> fromIntegral <$> getR m r >>= put o)
    [(tA, rA), (tF, rF), (tB, rB), (tC, rC), (tD, rD), (tE, rE), (tH, rH), (tL, rL)]
  st iSP >>= put16 tSp
  st iPC >>= put16 tPc
  st iIME >>= put tIme
  put tModel ((if mModel m == Cgb then 1 else 0) .|. (if mCgbMode m then 2 else 0))
  timer <- st iTIMER
  put tMbc (case mMbc m of
    NoMbc -> 0
    Mbc1 -> 1
    Mbc2 -> 2
    Mbc3 -> if timer /= 0 then 4 else 3
    Mbc5 -> 5
    _ -> 0xFF)
  st iDIV >>= put16 tDiv
  mapM_ (\(o, i) -> st i >>= put o)
    [ (tTima, iTIMA), (tTma, iTMA), (tTac, iTAC), (tIf, iIF), (tIe, iIE), (tSb, iSB), (tSc, iSC)
    , (tP1, iP1), (tLy, iLY), (tLcdc, iLCDC), (tStat, iSTAT), (tLyc, iLYC)
    , (tBank2, iBANK2), (tRamen, iRAMEN), (tMode, iMODE), (tRtcLatch, iRTCLATCH) ]
  st iDOT >>= put16 tDot
  mapM_ (\(o, a) -> io a >>= put o)
    [ (tScy, 0xFF42), (tScx, 0xFF43), (tBgp, 0xFF47), (tObp0, 0xFF48), (tObp1, 0xFF49)
    , (tWy, 0xFF4A), (tWx, 0xFF4B), (tDma, 0xFF46) ]
  st iROMB >>= put16 tRomb
  mapM_ (\r -> st (iRTC + r) >>= put (tRtc + r) >> st (iRTCL + r) >>= put (tRtcL + r)) [0 .. 4]
  mapM_ (\k -> io (0xFF10 + fromIntegral k) >>= put (tNr + k)) [0 .. 21]
  chans <- mapM (apuChannel m) [0 .. 3]
  put tNr52 (if or [p | (_, _, _, p) <- chans] then 0x80 else 0)
  put tChOn (foldr (\(k, (o, _, _, _)) acc -> if o then setBit acc k else acc) (0 :: Int) (zip [0 ..] chans))
  mapM_ (\(k, (_, l, v, _)) -> put16 (tChLen + 2 * k) l >> put (tChVol + k) v) (zip [0 ..] chans)
  mapM_ (\k -> io (0xFF30 + fromIntegral k) >>= put (tWave + k)) [0 .. 15]
  mapM_ (\k -> io (0xFE00 + fromIntegral k) >>= put (tOam + k)) [0 .. 159]
  -- the Game Boy Color's (0 on the DMG)
  when (mModel m == Cgb) $ do
    mapM_ (\(o, i) -> st i >>= put o)
      [ (tKey1, iKEY1), (tVbk, iVBK), (tSvbk, iSVBK), (tBcps, iBCPS), (tOcps, iOCPS), (tOpri, iOPRI)
      , (tFf72, iFF72), (tFf73, iFF73), (tFf74, iFF74), (tFf75, iFF75), (tRp, iRP) ]
    st iHDMASRC >>= put16 tHdmaSrc
    st iHDMADST >>= put16 tHdmaDst
    mapM_ (\k -> fromIntegral <$> MU.read (mPal m) k >>= put (tPal + k)) [0 .. 127]
  U.freeze t

-- | Whether a state can be taken now (between two 'step's), as the
-- hardware's freeze waits for: in vertical blank (or the LCD off), the
-- CPU not halted, no EI pending, no interrupt about to be taken, no OAM
-- DMA, no timer reload, no speed switch, no VRAM DMA. Then the routine
-- can put back everything that matters ("Gb.StateTable").
--
-- Two more keep the routine's tail exact: its IF write is 11 M-cycles
-- (44 dots) before the game resumes, so nothing may set an IF bit in
-- between: a line's events (VBlank's start, LY = LYC, line 153's early
-- LY = 0) are in its first dots, hence dot 56 or later; and TIMA at 8 or
-- more while the timer runs (at most 5 increments in the tail), so it
-- doesn't overflow there.
specAtRest :: Machine s -> ST s Bool
specAtRest m = do
  let st = getSt m
  lcdc <- st iLCDC
  ly <- st iLY
  halt <- st iHALT
  ei <- st iEI
  bug <- st iHALTBUG
  lock <- st iLOCK
  ime <- st iIME
  ie <- st iIE
  iflag <- st iIF
  dmaIdx <- st iDMAIDX
  dmaPend <- st iDMAPEND
  reload <- st iRELOAD
  hold <- st iDIVHOLD
  hdma <- st iHDMAACT
  dot <- st iDOT
  tac <- st iTAC
  tima <- st iTIMA
  pure $ (not (testBit lcdc 7) || (ly >= 144 && dot >= 56))
    && (not (testBit tac 2) || tima >= 8)
    && halt == 0 && ei == 0 && bug == 0 && lock == 0
    && not (ime /= 0 && (ie .&. iflag .&. 0x1F) /= 0)
    && dmaIdx < 0 && dmaPend == 0 && reload == 0 && hold == 0 && hdma == 0

-- | Run a machine through a ROM overlay: every read of 0x0000-0x7FFF (the
-- CPU's, and OAM DMA's) answers the overlay's byte at the address modulo
-- its size (a power of two) instead of the cartridge's, until the read
-- of @end@; then the machine goes on with its cartridge. The machine is
-- the one the PS released from its hold (its reset), with the memories
-- written in. 'Left' if the routine didn't end within @limit@
-- instructions; else the M-cycles it ran.
runOverlay :: Machine s -> U.Vector Word8 -> Word16 -> Int -> ST s (Either String Int)
runOverlay m ovl end limit = do
  trace <- getSt m iTRACE
  c0 <- getSt m iCYC
  setSt m iTRACE 1
  let n = U.length ovl
      bank = U.generate 0x4000 (\i -> ovl U.! (i .&. (n - 1)))
      m' = m { mRom = U.concat (replicate (max 1 (U.length (mRom m) `div` 0x4000)) bank) }
      go k
        | k >= limit = pure (Left ("the restore routine didn't end within " <> show limit <> " instructions"))
        | otherwise = do
            writeSTRef (mTrace m) []
            step m'
            acc <- readSTRef (mTrace m)
            if any isEnd acc then Right <$> ((subtract c0) <$> getSt m iCYC) else go (k + 1)
      isEnd a = case a of
        ARead x _ -> x == end
        _ -> False
  r <- go (0 :: Int)
  writeSTRef (mTrace m) []
  setSt m iTRACE trace
  _ <- peek m 0 -- (nothing: the machine's own cartridge again)
  pure r
