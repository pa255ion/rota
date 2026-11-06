{-# LANGUAGE BangPatterns, FlexibleContexts #-}
-- | Layer 2, side track N (PLAN.md N.5): the 800XL's save states in Clash
-- simulation: the core with the save states' hooks ('a800CoreWith',
-- "A800.Hooks"), wired as "A800.System" wires them, its memories
-- behavioural (the ROMs from the files, RAM a map; each byte a clock
-- afterSave its address, as a block RAM's).
--
-- The OS's start-up would take millions of clocks, so the circuit starts
-- mid-run: from the core's state afterSave n steps, run as plain Haskell
-- ("A800.LockState"), RAM as that run left it.
--
-- * Save: the circuit is asked to freeze (the state mode, the menu's
--   pause), goes on to a moment a state can be taken and freezes; its
--   status, state table and RAM are read through the windows onto the
--   byte channel. They must be the plain core's afterSave as many steps; let
--   go, the circuit goes on as the plain core, register for register.
-- * Load: a fresh circuit, held and frozen, gets RAM and the restore
--   routine (build/host/a800-restore, from the saved table) through the
--   windows, its overlay armed, and is let go. After the routine its
--   steps are the saved core's, register for register, and its table is
--   the saved one (but for the loose fields).
module Main (main) where

import Prelude

import qualified Clash.Prelude as C
import Clash.Prelude (BitVector, Signal, Unsigned)
import Control.Monad (forM_, unless)
import qualified Data.ByteString as BS
import Data.Bits ((.&.))
import qualified Data.IntMap.Strict as IM
import Data.IORef
import Data.Maybe (isJust)
import qualified Data.Vector.Unboxed as U
import Data.Word (Word8)
import System.FilePath ((</>))
import Test.Tasty
import Test.Tasty.HUnit

import A800.Antic (Antic (..))
import A800.Core
import A800.Hooks
import A800.LockCore (coreCfgOf)
import A800.LockState
import A800.Media (Cart (..))
import A800.Spec.Machine (Config (..), XexBoot (..))
import A800.StateTable (tP, tableSize)
import A800.StateTap (tapTable)
import A800.TestRoms
import Mos6502.Core (Cpu (..))
import Platform.Ps.Link (LinkAddr)

data TbIn = TbIn
  { tiLink  :: Maybe (LinkAddr, BitVector 8)
  , tiPause :: Bool
  , tiHeld  :: Bool
  } deriving stock (C.Generic)
    deriving anyclass C.NFDataX

data TbOut = TbOut
  { toState  :: CoreState
  , toOut    :: CoreOut
  , toByte   :: Maybe (BitVector 8)
  , toFrozen :: Bool
  , toOvl    :: Bool
  } deriving stock (C.Generic)
    deriving anyclass C.NFDataX

-- | RAM as a map, a register's state in the simulation.
newtype Ram = Ram (IM.IntMap Word8)

instance C.NFDataX Ram where
  deepErrorX = error
  hasUndefined _ = False
  ensureSpine = id
  rnfX (Ram m) = m `seq` ()

-- | The memories, behavioural: RAM a map (from an image), the ROMs from
-- the configuration's files.
mems :: C.HiddenClockResetEnable dom => Config -> IM.IntMap Word8 -> Mems dom
mems cfg ram0 = Mems
  { msRam = \a w -> C.register 0 (C.mealy ramStep (Ram ram0) (C.bundle (a, w)))
  , msOs = rom (Just (cfgOs cfg))
  , msBasic = rom (cfgBasic cfg)
  -- (the cartridge's interface since side track Q's XEGS work: a request,
  -- its byte held, always there the next clock)
  , msCart = \rq ->
      let b = rom (cartImage <$> cfgCart cfg) (maybe 0 id <$> rq)
          was = C.register False (isJust <$> rq)
          held = C.mux was b (C.register 0 held)
      in (held, pure True)
  , msLoader = rom (xbLoader <$> cfgXex cfg)
  , msXex = rom (xbFile <$> cfgXex cfg)
  }
 where
  -- the byte at the address before this clock's write (a block RAM's)
  ramStep (Ram m) (a, w) = (Ram (maybe m (\(x, v) -> IM.insert (fromIntegral x) (fromIntegral v) m) w), fromIntegral (IM.findWithDefault 0 (fromIntegral a) m))
  rom mb a = C.register 0 ((\x -> case mb of
                              Just b | fromIntegral x < BS.length b -> fromIntegral (BS.index b (fromIntegral x))
                              _ -> 0xFF) <$> a)

-- | The core and its hooks, as "A800.System" wires them: credit unless
-- the hooks stop it, no inputs.
tb :: C.HiddenClockResetEnable C.System => Config -> CoreState -> IM.IntMap Word8 -> Signal C.System TbIn -> Signal C.System TbOut
tb cfg s0 ram0 inp = TbOut <$> st <*> out <*> hoByte ho <*> hoFrozen ho <*> (isJust <$> hoOvl ho)
 where
  held = tiHeld <$> inp
  ho = a800StateHooks (tiLink <$> inp) (tiPause <$> inp) held st out ramD
  (out, st, ramD, _) = a800CoreWith s0 (mems cfg ram0) (pure (coreCfgOf cfg)) (not <$> held) (not <$> hoStopped ho)
                      (pure 0xFF) (pure 0) (pure 0) (pure 0) (pure 0) (hoFrozen ho) (hoOvl ho) (hoRam ho)

runs :: Config -> CoreState -> IM.IntMap Word8 -> [TbIn] -> [TbOut]
runs cfg s0 ram0 ins = C.simulate (C.withClockResetEnable C.clockGen C.resetGen C.enableGen (tb cfg s0 ram0)) ins

quiet :: Bool -> Bool -> TbIn
quiet p h = TbIn Nothing p h

wr :: Bool -> Bool -> LinkAddr -> BitVector 8 -> TbIn
wr p h a d = TbIn (Just (a, d)) p h

-- | Whether a clock's event ended a step.
stepped :: TbOut -> Bool
stepped o = case koEvent (toOut o) of
  Just e -> isJust (ceStep e)
  Nothing -> False

-- | The registers of a state.
regs :: CoreState -> [Int]
regs s = let c = ksCpu s in map fromIntegral [cA c, cX c, cY c, cS c, cP c .&. 0xCF] <> [fromIntegral (cPc c)]

-- The outputs are walked once each, as they come (a clock's output holds
-- the core's whole state: kept, a run's would fill the heap).

-- | When the circuit froze (its clock) and the steps it took before.
scanFreeze :: [TbOut] -> Maybe (Int, Int)
scanFreeze = go 0 0
 where
  go !i !st (o : os)
    | toFrozen o = Just (i, st)
    | i > 3000000 = Nothing
    | otherwise = go (i + 1) (if stepped o then st + 1 else st) os
  go _ _ [] = Nothing

-- | The first @nb@ bytes on the byte channel, and the registers after each
-- of the @k@ steps that follow the first @sb@ (the state a clock after a
-- step's event).
scanSave :: Int -> Int -> Int -> [TbOut] -> ([Word8], [[Int]])
scanSave nb sb k outs = go 0 [] 0 0 [] (zip outs (drop 1 outs))
 where
  go !nbs bs !st !nr rs ((a, b) : rest)
    | nbs >= nb && nr >= k = (reverse bs, reverse rs)
    | otherwise =
        let (nbs', bs') = case toByte a of
              Just x | nbs < nb -> (nbs + 1, fromIntegral (C.unpack x :: Unsigned 8) : bs)
              _ -> (nbs, bs)
            (st', nr', rs') = if stepped a
              then (st + 1, if st >= sb && nr < k then nr + 1 else nr, if st >= sb && nr < k then regs (toState b) : rs else rs)
              else (st, nr, rs)
        in go nbs' bs' st' nr' rs' rest
  go _ bs _ _ rs [] = (reverse bs, reverse rs)

-- | A load's run: the state at the routine's end (the overlay going off),
-- and the registers after each of the @k@ steps after it.
scanLoad :: Int -> [TbOut] -> (Maybe CoreState, [[Int]])
scanLoad k outs = go False Nothing 0 [] (zip outs (drop 1 outs))
 where
  go seenOn endSt !nr rs ((a, b) : rest)
    | nr >= k = (endSt, reverse rs)
    | toOvl a && not (toOvl b) = go True (Just (toState b)) nr rs rest
    | toOvl a = go True endSt nr rs rest
    | seenOn && stepped a = go True endSt (nr + 1) (regs (toState b) : rs) rest
    | otherwise = go seenOn endSt nr rs rest
  go _ endSt _ rs [] = (endSt, reverse rs)

-- | Save, go on, load, go on: the case above, after @n@ plain steps,
-- compared for @k@ steps after each.
check :: FilePath -> String -> Int -> Int -> Assertion
check file name n k = do
  cfg <- xexCfg file
  r <- newRun cfg
  warm r
  forM_ [1 .. n] (const (stepOf r))
  -- on to the middle of a frame, so the circuit has a way to go to rest
  let midFrame = do
        y <- sY . ksAntic <$> readIORef (rState r)
        unless (y >= 40 && y < 200) (stepOf r >> midFrame)
  midFrame
  s0 <- readIORef (rState r)
  ram0 <- IM.fromList . zip [0 ..] . U.toList <$> U.freeze (rRam r)
  -- probe: when does the circuit freeze, asked from its first clock?
  let ask = [wr True False readBackAddr 1, wr True False stateModeAddr 1]
  (frzAt, stepsBefore) <- case scanFreeze (runs cfg s0 ram0 (ask <> repeat (quiet True False))) of
    Just x -> pure x
    Nothing -> assertFailure "the circuit didn't freeze" >> fail ""
  assertBool ("froze after only " <> show stepsBefore <> " steps (the circuit started mid-frame)") (stepsBefore > 1000)
  -- the plain core to the same step: it must be at rest, and its table is
  -- what the windows must show
  forM_ [1 .. stepsBefore] (const (stepOf r))
  rest <- atRest r
  assertBool ("the plain core isn't at rest where the circuit froze (" <> show stepsBefore <> " steps)") rest
  tbl <- tableOf r
  ram <- U.freeze (rRam r)
  -- the plain core's next k steps
  afterSave <- mapM (const (stepOf r >> regsOf r)) [1 .. k]
  -- the save: status, table, RAM through the read window; then let go
  let readOps = [ wr True False (stateRBase + a) 0 | a <- [0x1F00 .. 0x1F03] <> [0 .. fromIntegral tableSize - 1] <> [0x10000 .. 0x1FFFF] ]
      saveIns = ask <> replicate (frzAt + 4) (quiet True False) <> readOps <> replicate 4 (quiet True False)
                <> [wr True False stateModeAddr 0, wr False False readBackAddr 0] <> repeat (quiet False False)
      (bytes, goneOn) = scanSave (length readOps) stepsBefore k (runs cfg s0 ram0 saveIns)
      (status, rest') = splitAt 4 bytes
      (tblW, ramW) = splitAt tableSize rest'
  status @?= [1, fromIntegral tableSize, 0, 1]
  unless (U.fromList tblW == tbl) (assertFailure ("the table read differs from the plain core's: " <> diffText tbl (U.fromList tblW)))
  assertBool "RAM read differs from the plain core's" (U.fromList ramW == ram)
  case [ (i, a, b) | (i, a, b) <- zip3 [0 :: Int ..] goneOn afterSave, a /= b ] of
    (i, a, b) : _ -> assertFailure ("after the save, the circuit and the plain core part at step " <> show i <> ": " <> show a <> " / " <> show b)
    [] -> pure ()
  assertBool "the save's run took fewer steps than compared" (length goneOn == k)
  -- the load: held and frozen, RAM, the routine, armed; let go
  (ovl, end) <- buildOverlay ("clash-" <> name) tbl
  let w a v = wr True True (stateWBase + a) v
      loadIns = [wr True True stateModeAddr 1] <> replicate 4 (quiet True True)
                <> [ w (0x10000 + fromIntegral i) (fromIntegral (ram U.! i)) | i <- [0 .. 65535] ]
                <> [ w (0x6000 + fromIntegral i) (fromIntegral (ovl U.! i)) | i <- [0 .. U.length ovl - 1] ]
                <> [ w 0x1F10 (fromIntegral (end .&. 0xFF)), w 0x1F11 (fromIntegral (end `div` 256)), w 0x1F12 1 ]
                <> replicate 4 (quiet True False) <> [wr True False stateModeAddr 0] <> repeat (quiet False False)
      (endState, lsteps) = scanLoad k (runs cfg (coreInit (coreCfgOf cfg)) IM.empty loadIns)
  case [ (i, a, b) | (i, a, b) <- zip3 [0 :: Int ..] lsteps afterSave, a /= b ] of
    (i, a, b) : _ -> assertFailure ("after the load, the circuit and the saved core part at step " <> show i <> ": " <> show a <> " / " <> show b)
    [] -> pure ()
  assertBool "the load ran fewer steps than compared" (length lsteps == k)
  -- the restored table, at the routine's end
  es <- maybe (assertFailure "the overlay never went off" >> fail "") pure endState
  let tbl2 = U.generate tableSize (\i -> fromIntegral (tapTable es (fromIntegral i))) :: U.Vector Word8
      bad = [ o | o <- [0 .. tableSize - 1], o `notElem` loose tbl
                , (if o == tP then (.&. 0xCF) else id) (tbl U.! o) /= (if o == tP then (.&. 0xCF) else id) (tbl2 U.! o) ]
  unless (null bad) (assertFailure ("the restored table differs: " <> diffText tbl tbl2))

main :: IO ()
main = defaultMain $ testGroup "a800xl-savestate (side track N, N.5)"
  [ testCase "regs.xex, from the middle of a frame" $ check (asmRoot </> "regs.xex") "regs" 600000 3000
  ]
