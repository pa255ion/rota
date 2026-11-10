{-# OPTIONS_GHC -fconstraint-solver-iterations=0 #-}
{-# LANGUAGE FlexibleContexts #-}
-- | snes-savestate, the CPU build (step 7.15c; cabal -f states-cpu, Clash's
-- -DSNES_STATES_CPU): a load through the 65C816's restore routine, on the
-- circuit itself ("Snes.Core" in Clash's simulation), in two passes.
--
-- 1. Core A runs a ROM to a moment it can be saved at ('atRestCpu', in
--    V-blank) and freezes. The table window (the table and OAM's high
--    table, "Snes.StateTap") is read through the port, the sound module's
--    chain round once, every memory. A thaws, and its outputs for the next
--    clocks are kept.
-- 2. The PS's routine builder (build/host/snes-restore) makes the overlay
--    from the table, the DMA registers and OAM's high table.
-- 3. Core B, frozen from its reset (power-on), gets the chain, the
--    memories and the overlay, is armed and let go. It runs the routine;
--    from the clock its last instruction ends ('scOvlDone') its outputs
--    must be A's from its thaw, clock for clock.
module LoadCpu (loadTest) where

import Prelude

import Clash.Sized.Internal.BitVector (unsafeToNatural)
import Clash.Prelude (BitVector, Signal, Unsigned, Vec, bundle, mealy, register, sampleN,
                      systemClockGen, unbundle, withClockResetEnable)
import qualified Clash.Prelude as C
import Clash.Explicit.Reset (resetGen)
import Control.Monad (unless, when)
import qualified Data.List as L
import qualified Data.Bits as Bits
import qualified Data.ByteString as B
import qualified Data.Map.Strict as M
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Data.Word (Word8)
import GHC.Generics (Generic)
import System.Directory (createDirectoryIfMissing)
import System.Exit (ExitCode (..))
import System.FilePath ((</>), takeBaseName)
import System.Process (readProcessWithExitCode)
import Test.Tasty.HUnit

import Snes.Bus (CartCfg (..), CartMap (..))
import Snes.Cart
import Snes.Core
import Snes.StateTap (tableWindow)

tool, scratch :: FilePath
tool = "build/host/snes-restore"
scratch = "build/snes/loadcpu"

type Seen = (Maybe (Unsigned 8, Unsigned 8, Unsigned 15, Unsigned 15), Maybe (C.Signed 16, C.Signed 16), Maybe (Unsigned 23), Unsigned 9, Unsigned 11)

bitsOf :: Seen -> Integer
bitsOf = toInteger . unsafeToNatural . C.pack

nat :: C.KnownNat n => BitVector n -> Integer
nat = toInteger . unsafeToNatural

-- | The memories the PS writes directly (OAM's low table; its high table
-- is the routine's): read with Nothing, written with the byte.
memOps :: [Maybe (BitVector 16) -> StPort]
memOps =
  [ \w -> PWram a (C.unpack . C.truncateB <$> w) | a <- [0 .. 0x1FFFF] ]
  <> [ \w -> PVram h a (C.unpack . C.truncateB <$> w) | h <- [False, True], a <- [0 .. 0x7FFF] ]
  <> [ \w -> PCgram a (C.unpack . C.truncateB <$> w) | a <- [0 .. 255] ]
  <> [ \w -> POam l a (C.unpack . C.truncateB <$> w) | l <- [0 .. 3], a <- [0 .. 127] ]
  <> [ \w -> PDma r ch (C.unpack . C.truncateB <$> w) | r <- [0 .. 15], ch <- [0 .. 7] ]
  <> [ \w -> PAram a (C.unpack . C.truncateB <$> w) | a <- [0 .. 0xFFFF] ]
  <> [ \w -> PSmp a (C.unpack <$> w) | a <- [0 .. 255] ]

-- | The DMA registers' ops: their place in memOps (register r, channel ch).
dmaBase :: Int
dmaBase = 0x20000 + 0x10000 + 256 + 512

data P1 = A1Run | A1Cap | A1Tab Int | A1Chain Int | A1Mem Int | A1After Int
  deriving stock (Show, Eq, Generic)
  deriving anyclass C.NFDataX

data P2 = B2Cap | B2Chain Int | B2Mem Int | B2Ovl Int | B2Run Int | B2After Int
  deriving stock (Show, Eq, Generic)
  deriving anyclass C.NFDataX

cartOf :: FilePath -> IO (CartCfg, U.Vector Word8)
cartOf path = do
  file <- B.readFile path
  cart <- maybe (assertFailure (path <> ": no header") >> fail "") pure (parseCart file)
  let rom = U.fromList (B.unpack (fst (stripCopier file)))
      romSize = U.length rom
      p = if romSize == 0 then 0 else C.bit (C.finiteBitSize romSize - 1 - C.countLeadingZeros romSize)
  pure ( CartCfg { ccMap = case cartMapping cart of LoRom -> CmLoRom; HiRom -> CmHiRom; ExHiRom -> CmExHiRom
                 , ccRomP = fromIntegral p, ccRomQ = fromIntegral (romSize - p), ccSram = fromIntegral (cartSramSize cart) }
       , rom )

ddrOf :: U.Vector Word8 -> (C.HiddenClockResetEnable C.System => Signal C.System (Maybe (Unsigned 23)) -> Signal C.System (Maybe (Vec 8 (Unsigned 8))))
ddrOf rom req = register Nothing (fmap (\a -> C.unpack (C.pack (C.map (\k -> fromIntegral (rom U.! ((fromIntegral a `div` 8 * 8 + k) `mod` U.length rom))) (C.iterateI (+ 1) (0 :: Int)) :: Vec 8 (Unsigned 8)))) <$> req)

-- | The load on a ROM, saved after the frames given; the outputs compared
-- for the clocks given after.
loadTest :: FilePath -> Int -> Int -> Assertion
loadTest path frames after = do
  (cfg, rom) <- cartOf path
  let opsV = V.fromList memOps
      nOps = V.length opsV
      nChain = apuChainBytes
      minClocks = frames * 357368
      -- pass 1
      circuit1 :: Signal C.System (Seen, P1, BitVector 8, BitVector 16)
      circuit1 = withClockResetEnable systemClockGen (resetGen @C.System) C.enableGen $
        let core = snesCore (pure True) (pure cfg) (pure (C.repeat 0)) (ddrOf rom (scDdrReq core)) (pure (C.RamRead 0)) st (pure ovlIdle)
            (st, ph) = unbundle (mealy ctl1 (A1Run, 0 :: Int) (bundle (scRestCpu core, scV core, scTop core)))
            seen = bundle (scPixel core, scSample core, scDdrReq core, scV core, scH core)
        in bundle (seen, ph, scTop core, scPortRd core)
      frozen sh pt = StCtl True sh pt False
      ctl1 (phase, n) (rest, v, top) = case phase of
        -- (at rest: a finishing clock, no master clock, then frozen)
        A1Run | n >= minClocks && rest && v >= 226 && v <= 250 -> ((A1Cap, n + 1), (stIdle { stFinish = True }, phase))
              | otherwise -> ((A1Run, n + 1), (stIdle, phase))
        A1Cap -> ((A1Tab 0, n + 1), (frozen Nothing Nothing, phase))
        A1Tab k | k < tableWindow -> ((A1Tab (k + 1), n + 1), (frozen Nothing (Just (PTable (fromIntegral k))), phase))
                | otherwise -> ((A1Chain 0, n + 1), (frozen Nothing Nothing, phase))
        A1Chain k | k < nChain -> ((A1Chain (k + 1), n + 1), (frozen (Just top) Nothing, phase))
                  | otherwise -> ((A1Mem 0, n + 1), (frozen Nothing Nothing, phase))
        A1Mem i | i < nOps -> ((A1Mem (i + 1), n + 1), (frozen Nothing (Just ((opsV V.! i) Nothing)), phase))
                | otherwise -> ((A1After 0, n + 1), (frozen Nothing Nothing, phase))
        A1After k -> ((A1After (k + 1), n + 1), (stIdle, phase))
      -- (the chain read round: each shift takes the top byte back in; the
      -- byte out is the top before it)
      -- (rest in V-blank can be most of a frame away: atRestCpu holds on
      -- some 1500 of its clocks, tone.sfc's 310662 clocks on)
      total1 = minClocks + 2 * 357368 + tableWindow + nChain + nOps + after + 10
      step1 (acc@(tab, ch, mem, aft), prevPh) (sn, ph, top, prd) =
        C.deepseqX sn $ prevPh `seq`
        let acc' = case (prevPh, ph) of
              -- a port read's value: the clock after its op
              (Just (A1Tab k), _) | k < tableWindow -> (M.insert k (fromIntegral (nat prd) :: Int) tab, ch, mem, aft)
              (Just (A1Mem i), _) | i < nOps -> (tab, ch, M.insert i (nat prd) mem, aft)
              _ -> acc
            acc'' = case ph of
              A1Chain k | k < nChain -> let (t, c, m, a) = acc' in (t, M.insert k (fromIntegral (nat top) :: Int) c, m, a)
              A1After k | k < after -> let (t, c, m, a) = acc' in (t, c, m, bitsOf sn : a)
              _ -> acc'
        in tab `seq` (acc'', Just ph)
  -- (the chain's top byte as each shift takes it: a rotation, so the read
  -- is the record's bytes from the top, which B shifts in in that order)
  let ((tabM, chM, memM, aftA), _) = L.foldl' (\s x -> let r = step1 s x in fst r `seq` r) ((M.empty, M.empty, M.empty, []), Nothing) (sampleN total1 circuit1)
  unless (M.size tabM == tableWindow && M.size chM == nChain && M.size memM == nOps) $
    assertFailure ("pass 1 read " <> show (M.size tabM, M.size chM, M.size memM) <> ", not " <> show (tableWindow, nChain, nOps) <> ": no freeze?")
  -- the routine
  -- (a folder per ROM: the tests may run side by side)
  let dir = scratch </> takeBaseName path
  createDirectoryIfMissing True dir
  let tbl = B.pack [ fromIntegral (tabM M.! k) | k <- [0 .. 519] ]
      dma = [ fromIntegral (memM M.! (dmaBase + r * 8 + ch)) | ch <- [0 .. 7], r <- [0 .. 15] ]
      oamHi = [ fromIntegral (tabM M.! k) | k <- [520 .. tableWindow - 1] ]
  B.writeFile (dir </> "table") tbl
  B.writeFile (dir </> "fill") (B.pack (dma <> oamHi))
  (code, _, err) <- readProcessWithExitCode tool [dir </> "table", dir </> "fill", dir </> "overlay"] ""
  when (code /= ExitSuccess) (assertFailure ("snes-restore: " <> err))
  ovl <- U.fromList . B.unpack <$> B.readFile (dir </> "overlay")
  end <- case words err of
    ["end", e, "cycles", _] -> pure (read e :: Int)
    _ -> assertFailure ("snes-restore said: " <> err) >> pure 0
  -- pass 2
  let chainV = V.fromList [ chM M.! k | k <- [0 .. nChain - 1] ]
      memV = V.fromList [ memM M.! i | i <- [0 .. nOps - 1] ]
      limit = 3000000
      circuit2 :: Signal C.System (Seen, P2, Bool)
      circuit2 = withClockResetEnable systemClockGen (resetGen @C.System) C.enableGen $
        let core = snesCore (pure True) (pure cfg) (pure (C.repeat 0)) (ddrOf rom (scDdrReq core)) (pure (C.RamRead 0)) st ov
            (st, ov, ph) = unbundle (mealy ctl2 B2Cap (scOvlDone core))
            seen = bundle (scPixel core, scSample core, scDdrReq core, scV core, scH core)
        in bundle (seen, ph, scOvlDone core)
      armed = OvlIn True (fromIntegral end)
      ctl2 phase done = case phase of
        B2Cap -> (B2Chain 0, (frozen Nothing Nothing, ovlIdle, phase))
        B2Chain k | k < nChain -> (B2Chain (k + 1), (frozen (Just (fromIntegral (chainV V.! k))) Nothing, ovlIdle, phase))
                  | otherwise -> (B2Mem 0, (frozen Nothing Nothing, ovlIdle, phase))
        B2Mem i | i < nOps -> (B2Mem (i + 1), (frozen Nothing (Just ((opsV V.! i) (Just (fromInteger (memV V.! i))))), ovlIdle, phase))
                | otherwise -> (B2Ovl 0, (frozen Nothing Nothing, ovlIdle, phase))
        B2Ovl j | j < 0x800 -> (B2Ovl (j + 1), (frozen Nothing (Just (POvl (fromIntegral j) (Just (fromIntegral (ovl U.! j))))), armed, phase))
                | otherwise -> (B2Run 0, (frozen Nothing Nothing, armed, phase))
        -- (the outputs outside the test of done: done is the core's, from
        -- this clock's overlay input, and a guard around them made a
        -- combinational loop, which hung the simulation)
        B2Run k -> (if done then B2After 1 else B2Run (k + 1), (stIdle, armed, phase))
        B2After k -> (B2After (k + 1), (stIdle, ovlIdle, phase))
      total2 = nChain + nOps + 0x800 + limit + after + 10
      -- B's outputs from the clock its routine's last instruction ends
      collect (acc, n) (sn, ph, done) = C.deepseqX sn $ case ph of
        B2Run _ | done -> (bitsOf sn : acc, n + 1)
        B2After _ | n < after -> (bitsOf sn : acc, n + 1)
        _ -> (acc, n)
      (aftB, nB) = L.foldl' (\s x -> let r = collect s x in snd r `seq` r) ([], 0 :: Int) (takeWhile (\(_, ph, _) -> case ph of B2After k -> k < after; _ -> True) (sampleN total2 circuit2))
  unless (nB >= after) $ assertFailure ("the routine didn't end (B's outputs: " <> show nB <> " clocks)")
  let a = reverse aftA
      b = take after (reverse aftB)
  -- (one difference allowed: a DDR request of B's where A has none, the
  -- ROM's line buffer refilled; it is a cache of the circuit's, not the
  -- console's state, and a miss stalls the whole core, time unchanged)
  -- (the request's field: bits 20 to 43, above the line and dot; tag 43)
  let ddrMask = (Bits.bit 24 - 1) `Bits.shiftL` 20 :: Integer
      refill x y = x /= y && Bits.xor x y Bits..&. Bits.complement ddrMask == 0
                   && x Bits..&. ddrMask == 0 && Bits.testBit y 43
      differs x y = x /= y && not (refill x y)
  case [ k | (k, x, y) <- zip3 [0 :: Int ..] a b, differs x y ] of
    k : _ -> assertFailure ("clock " <> show k <> " after the load: A " <> show (a !! k) <> ", B " <> show (b !! k)
                            <> " (" <> show (length (filter id (zipWith differs a b))) <> " of " <> show (length b) <> " clocks differ, "
                            <> show (length (filter id (zipWith refill a b))) <> " B's line refills)")
    [] -> pure ()
