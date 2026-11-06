-- | The Clash PPU's sprite evaluation on its own (layer 1), with
-- misaligned OAM. Regressions (side track D, 2026-10-22): AccuracyCoin
-- d103cbd's "Misaligned OAM behavior" 2 and 4 (OAMADDR realigned, AND $FC,
-- after a Y byte out of range (+4) and after an X byte out of range (+1)).
module Nes.SpecEval
  ( evalTests
  ) where

import Prelude

import Clash.Prelude (complement)
import qualified Data.Map.Strict as M
import Data.Word (Word8)
import Test.Tasty
import Test.Tasty.HUnit

import Nes.Ppu

-- | Line 0 from dot 0 with rendering on and OAMADDR at @start@, OAM as
-- @bytes@ (from address 0, $FF after them); the overflow flag after the
-- line's evaluation.
overflowAfter :: Int -> [Word8] -> Bool
overflowAfter start bytes = pOvf (go (0 :: Int) p0)
 where
  oam = M.fromList (zip [0 ..] bytes)
  at a = maybe 0xFF fromIntegral (M.lookup (fromIntegral a :: Int) oam)
  p0 = ppuInit { pLine = 0, pDot = 0, pMask = 0x18, pRenHist = complement 0, pOamAddr = fromIntegral start }
  go n p
    | n > 260 = p
    | otherwise = go (n + 1) (fst (ppuTick p TDot (PpuIn 0 0 (at (pOamAddr p)))))

obj :: [Word8]
obj = [0x00, 0xE3, 0x00, 0x00]

evalTests :: TestTree
evalTests = testGroup "Clash PPU sprite evaluation, misaligned OAM (AccuracyCoin d103cbd)"
  [ testCase "Y out of range: OAMADDR += 4, AND $FC (test 2): 9 sprites, overflow" $
      overflowAfter 1 (0xFF : concat (replicate 6 obj) ++ [0x80, 0xFF, 0xFF] ++ concat (replicate 3 obj)) @?= True
  , testCase "X out of range: OAMADDR += 1, AND $FC (test 4): 9 sprites, overflow" $
      overflowAfter 1 (0xFF : concat (replicate 6 obj) ++ [0x00, 0xE3, 0x00, 0x80, 0xE3, 0xFF, 0xFF] ++ concat (replicate 3 obj)) @?= True
  , testCase "aligned, 8 sprites: no overflow" $
      overflowAfter 0 (concat (replicate 8 obj)) @?= False
  ]
