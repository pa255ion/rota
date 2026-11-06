-- | A profiler for the demo ROMs (a development tool, PLAN.md side track
-- J): runs a ROM on its spec and adds each instruction's cycles to the
-- label before its address, over the whole run and in the frame that took
-- longest (a frame: from one vertical blank interrupt to the next, the
-- time spent waiting for it left out).
--
-- > rota-profile gb  <rom.gb>  <rom.sym> <frames>     (rgblink -n; M-cycles)
-- > rota-profile nes <rom.nes> <rom.lbl> <frames>     (ld65 -Ln, ca65 -g; CPU cycles)
module Main (main) where

import Control.Monad.ST (runST)
import qualified Data.ByteString as BS
import Data.List (isPrefixOf, sortOn)
import qualified Data.Map.Strict as M
import Data.Ord (Down (..))
import Numeric (readHex)
import System.Environment (getArgs)
import Text.Printf (printf)

import qualified Gb.Spec as G
import qualified Mos6502.Spec as C
import qualified Nes.Spec.Machine as NM
import qualified Nes.Spec.Ppu as NP

main :: IO ()
main = do
  [kind, romPath, symPath, framesArg] <- getArgs
  rom <- BS.readFile romPath
  syms <- (if kind == "nes" then parseLbl else parseSym) <$> readFile symPath
  let n = read framesArg :: Int
      r = if kind == "nes" then profileNes rom syms n else profileGb rom syms n
      (total, worst, worstFrame, worstCycles, hits) = r
  printf "worst frame %d: %d cycles\n" worstFrame worstCycles
  putStrLn "-- worst frame"
  mapM_ (\(l, c) -> printf "%8d  %s\n" c l) (take 25 (sortOn (Down . snd) (M.toList worst)))
  putStrLn "-- whole run (cycles, times entered, per entry)"
  mapM_ (\(l, c) -> let h = M.findWithDefault 0 l hits in
                      printf "%8d %7d %7.1f  %s\n" c h (fromIntegral c / fromIntegral (max 1 h) :: Double) l)
    (take 45 (sortOn (Down . snd) (M.toList total)))

-- | rgblink's .sym: "00:abcd Label".
parseSym :: String -> M.Map Int String
parseSym s = M.fromList
  [ (a, drop 1 rest)
  | l <- lines s, not (null l), take 1 l /= ";"
  , let (addr, rest) = break (== ' ') l
  , (_, ':' : off) <- [break (== ':') addr]
  , [(a, "")] <- [readHex off]
  , a < 0x8000 ]

-- | ld65's -Ln: "al 00abcd .label".
parseLbl :: String -> M.Map Int String
parseLbl s = M.fromList (qualify "" (M.toAscList raw))
 where
  raw = M.fromListWith keep
    [ (a, dropWhile (== '.') name)
    | l <- lines s, "al " `isPrefixOf` l
    , [_, addr, name] <- [words l]
    , [(a, "")] <- [readHex addr]
    , a >= 0x8000 ]
  -- a routine's label over its cheap local ones at the same address
  keep a b = if take 1 a == "@" then b else a
  -- cheap locals (@x) named after their routine: routine@x
  qualify _ [] = []
  qualify r ((a, l) : rest)
    | take 1 l == "@" = (a, r <> l) : qualify r rest
    | otherwise = (a, l) : qualify l rest

type Result = (M.Map String Int, M.Map String Int, Int, Int, M.Map String Int)

-- | The common loop: @stepper@ runs one instruction and says (its
-- address, its cycles, whether a frame starts here, whether it waits).
profileWith :: Monad m => m (Int, Int, Bool, Bool) -> m Int -> Int -> M.Map Int String -> m Result
profileWith stepper framesDone n syms = go 0 M.empty M.empty 0 M.empty 0 0 M.empty
 where
  label pc = maybe "?" snd (M.lookupLE pc syms)
  go f total cur curLen worst wf wl hits
    | f >= n = pure (total, worst, wf, wl, hits)
    | otherwise = do
        (pc, d, starts, waiting) <- stepper
        f' <- framesDone
        let l = label pc
            total' = M.insertWith (+) l d total
            hits' = if M.member pc syms then M.insertWith (+) l 1 hits else hits
        if starts
          then let (worst', wf', wl') = if curLen > wl && f > 4 then (cur, f, curLen) else (worst, wf, wl)
               in go f' total' (M.singleton l d) d worst' wf' wl' hits'
          else go f' total' (M.insertWith (+) l d cur) (curLen + (if waiting then 0 else d)) worst wf wl hits'

profileGb :: BS.ByteString -> M.Map Int String -> Int -> Result
profileGb rom syms n = runST $ do
  em <- G.newMachine rom
  m <- either error pure em
  let stepper = do
        r <- G.regs m
        c0 <- G.cycles m
        G.step m
        c1 <- G.cycles m
        let pc = fromIntegral (G.rgPC r)
        pure (pc, c1 - c0, pc == 0x40, maybe False ((== "Main") . snd) (M.lookupLE pc syms))
  profileWith stepper (G.frames m) n syms

profileNes :: BS.ByteString -> M.Map Int String -> Int -> Result
profileNes rom syms n = runST $ do
  em <- NM.newMachine rom
  m <- either error pure em
  let stepper = do
        r <- C.getRegs (NM.mCpu m)
        c0 <- NM.machineCycles m
        s <- NM.stepCpu m
        c1 <- NM.machineCycles m
        let pc = fromIntegral (C.rPC r)
            waiting = maybe False ((== "main@wait") . snd) (M.lookupLE pc syms)
        pure (pc, c1 - c0, s == C.Nmi, waiting)
  profileWith stepper (NP.ppuFramesDone (NM.mPpu m)) n syms
