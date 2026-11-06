-- | Layer 0: the spec CPU on hand-written programs: the M-cycle bus order of
-- the instructions whose order matters (Pan Docs opcode tables; Gekkio,
-- "Game Boy: Complete Technical Reference"), interrupt dispatch, HALT and
-- a few flag cases. Cycle counts of every opcode are checked by Blargg's
-- instr_timing (Gb.SpecRoms).
module Gb.SpecCpu (cpuTests, joypadTests) where

import Prelude

import Control.Monad (replicateM_)
import Control.Monad.ST (runST)
import qualified Data.ByteString as BS
import Data.Word
import Test.Tasty
import Test.Tasty.HUnit

import Gb.Spec

-- | A 32 KiB image with no MBC and this code at 0x0100.
progRom :: [Word8] -> BS.ByteString
progRom code = BS.concat [BS.replicate 0x100 0, BS.pack code, BS.replicate (0x8000 - 0x100 - length code) 0]

-- | Poke memory, run @n@ steps untraced, then trace one more; the trace
-- and the registers regsAfter it.
traceStep :: [(Word16, Word8)] -> [Word8] -> Int -> ([Access], Regs)
traceStep pokes code n = runST $ do
  m <- either error id <$> newMachine (progRom code)
  mapM_ (uncurry (poke m)) pokes
  replicateM_ n (step m)
  (_, t) <- withTrace m (step m)
  r <- regs m
  pure (t, r)

-- | Just the trace.
tr :: [(Word16, Word8)] -> [Word8] -> Int -> [Access]
tr p c n = fst (traceStep p c n)

-- | Registers after running @n@ steps.
regsAfter :: [(Word16, Word8)] -> [Word8] -> Int -> Regs
regsAfter p c n = snd (traceStep p c (n - 1))

cpuTests :: TestTree
cpuTests = testGroup "spec CPU"
  [ testGroup "bus order per M-cycle (the fetch is the first M-cycle)"
      [ testCase "NOP" $ tr [] [0x00] 0 @?= [ARead 0x100 0x00]
      , testCase "LD (HL),n: fetch, n, write" $
          tr [] [0x21, 0x00, 0xC0, 0x36, 0x5A] 1
            @?= [ARead 0x103 0x36, ARead 0x104 0x5A, AWrite 0xC000 0x5A]
      , testCase "INC (HL): fetch, read, write" $
          tr [(0xC000, 0x41)] [0x21, 0x00, 0xC0, 0x34] 1
            @?= [ARead 0x103 0x34, ARead 0xC000 0x41, AWrite 0xC000 0x42]
      , testCase "PUSH BC: fetch, idle, high byte at SP-1, low byte at SP-2" $
          tr [] [0xC5] 0 @?= [ARead 0x100 0xC5, AIdle, AWrite 0xFFFD 0x00, AWrite 0xFFFC 0x13]
      , testCase "POP BC: fetch, low byte, high byte" $
          tr [(0xC000, 0x34), (0xC001, 0x12)] [0x31, 0x00, 0xC0, 0xC1] 1
            @?= [ARead 0x103 0xC1, ARead 0xC000 0x34, ARead 0xC001 0x12]
      , testCase "CALL nn: fetch, lo, hi, idle, push PC high, push PC low" $
          tr [] [0xCD, 0x34, 0x12] 0
            @?= [ ARead 0x100 0xCD, ARead 0x101 0x34, ARead 0x102 0x12, AIdle
                , AWrite 0xFFFD 0x01, AWrite 0xFFFC 0x03 ]
      , testCase "CALL NZ,nn not taken (Z set at hand-off): 3 M-cycles" $
          tr [] [0xC4, 0x34, 0x12] 0 @?= [ARead 0x100 0xC4, ARead 0x101 0x34, ARead 0x102 0x12]
      , testCase "RET: fetch, lo, hi, idle" $
          tr [(0xC000, 0x34), (0xC001, 0x12)] [0x31, 0x00, 0xC0, 0xC9] 1
            @?= [ARead 0x103 0xC9, ARead 0xC000 0x34, ARead 0xC001 0x12, AIdle]
      , testCase "RET Z taken: fetch, idle, lo, hi, idle" $
          tr [(0xC000, 0x34), (0xC001, 0x12)] [0x31, 0x00, 0xC0, 0xC8] 1
            @?= [ARead 0x103 0xC8, AIdle, ARead 0xC000 0x34, ARead 0xC001 0x12, AIdle]
      , testCase "RET NZ not taken: fetch, idle" $
          tr [] [0xC0] 0 @?= [ARead 0x100 0xC0, AIdle]
      , testCase "RST 38: fetch, idle, push PC high, push PC low" $
          tr [] [0xFF] 0 @?= [ARead 0x100 0xFF, AIdle, AWrite 0xFFFD 0x01, AWrite 0xFFFC 0x01]
      , testCase "LD (nn),SP: fetch, lo, hi, write SP low, write SP high" $
          tr [] [0x08, 0x00, 0xC0] 0
            @?= [ ARead 0x100 0x08, ARead 0x101 0x00, ARead 0x102 0xC0
                , AWrite 0xC000 0xFE, AWrite 0xC001 0xFF ]
      , testCase "JR e taken: fetch, e, idle" $
          tr [] [0x18, 0x05] 0 @?= [ARead 0x100 0x18, ARead 0x101 0x05, AIdle]
      , testCase "JP nn: fetch, lo, hi, idle" $
          tr [] [0xC3, 0x00, 0x02] 0 @?= [ARead 0x100 0xC3, ARead 0x101 0x00, ARead 0x102 0x02, AIdle]
      , testCase "JP HL: fetch only" $ tr [] [0xE9] 0 @?= [ARead 0x100 0xE9]
      , testCase "ADD SP,e: fetch, e, idle, idle" $
          tr [] [0xE8, 0x01] 0 @?= [ARead 0x100 0xE8, ARead 0x101 0x01, AIdle, AIdle]
      , testCase "LD HL,SP+e: fetch, e, idle" $
          tr [] [0xF8, 0x01] 0 @?= [ARead 0x100 0xF8, ARead 0x101 0x01, AIdle]
      , testCase "BIT 0,(HL): fetch, CB opcode, read" $
          tr [(0xC000, 0x01)] [0x21, 0x00, 0xC0, 0xCB, 0x46] 1
            @?= [ARead 0x103 0xCB, ARead 0x104 0x46, ARead 0xC000 0x01]
      , testCase "SET 7,(HL): fetch, CB opcode, read, write" $
          tr [(0xC000, 0x01)] [0x21, 0x00, 0xC0, 0xCB, 0xFE] 1
            @?= [ARead 0x103 0xCB, ARead 0x104 0xFE, ARead 0xC000 0x01, AWrite 0xC000 0x81]
      ]
  , testGroup "interrupts"
      [ testCase "dispatch: the fetch is discarded, idle, push PC, idle; PC = vector" $ do
          let pokes = [(0xFFFF, 0x01), (0xFF0F, 0x01)]
              code = [0xFB, 0x00, 0x00] -- EI, NOP, NOP
          tr pokes code 2
            @?= [ARead 0x102 0x00, AIdle, AWrite 0xFFFD 0x01, AWrite 0xFFFC 0x02, AIdle]
          rgPC (regsAfter pokes code 3) @?= 0x40
          rgIME (regsAfter pokes code 3) @?= False
      , testCase "EI: the next instruction runs before the interrupt" $
          rgPC (regsAfter [(0xFFFF, 0x01), (0xFF0F, 0x01)] [0xFB, 0x00, 0x00] 2) @?= 0x102
      , testCase "EI then DI: no interrupt" $
          rgPC (regsAfter [(0xFFFF, 0x01), (0xFF0F, 0x01)] [0xFB, 0xF3, 0x00, 0x00] 3) @?= 0x103
      , testCase "the lowest pending bit wins (timer before serial)" $
          rgPC (regsAfter [(0xFFFF, 0x0C), (0xFF0F, 0x0C)] [0xFB, 0x00, 0x00] 3) @?= 0x50
      , testCase "while halted the CPU keeps fetching at PC" $
          tr [(0xFFFF, 0x00)] [0x76, 0x00] 1 @?= [ARead 0x101 0x00]
      , testCase "HALT bug: IME = 0 and a pending interrupt, the next byte runs twice" $ do
          let r = regsAfter [(0xFFFF, 0x01), (0xFF0F, 0x01)] [0x76, 0x04, 0x00] 3 -- HALT, INC B
          (rgB r, rgPC r) @?= (2, 0x102)
      ]
  , testGroup "flags"
      [ testCase "DAA regsAfter 0x45 + 0x38 gives 0x83" $
          let r = regsAfter [] [0x3E, 0x45, 0xC6, 0x38, 0x27] 3 in (rgA r, rgF r) @?= (0x83, 0x00)
      , testCase "ADD SP,-1 from 0xFFFE: H and C from the low byte, Z and N clear" $
          let r = regsAfter [] [0xE8, 0xFF] 1 in (rgSP r, rgF r) @?= (0xFFFD, 0x30)
      , testCase "POP AF clears the low nibble of F" $
          let r = regsAfter [(0xC000, 0xFF), (0xC001, 0x12)] [0x31, 0x00, 0xC0, 0xF1] 2
          in (rgA r, rgF r) @?= (0x12, 0xF0)
      , testCase "SUB A,A: Z and N" $ let r = regsAfter [] [0x97] 1 in (rgA r, rgF r) @?= (0, 0xC0)
      ]
  ]

-- | Steps until PC reaches @pc@ (at most @n@), with keys from a schedule.
runWithKeys :: BS.ByteString -> (Int -> Word8) -> Int -> Regs
runWithKeys rom keysAt n = runST $ do
  m <- either error id <$> newMachine rom
  let go k = do
        setJoypad m (keysAt k)
        step m
        r <- regs m
        if k >= n then pure r else go (k + 1)
  go 0

joypadTests :: TestTree
joypadTests = testCase "joypad.gb: P1 per selection (Pan Docs), interrupt when a line goes low" $ do
  rom <- BS.readFile "../../build/gb/asm/joypad.gb"
  -- Right and B held, released at step 300, B pressed again at 600
  let keysAt k | k < 300 = 0x21
               | k < 600 = 0x00
               | otherwise = 0x20
      r = runWithKeys rom keysAt 2000
  (rgB r, rgC r, rgD r, rgE r, rgH r) @?= (0xEE, 0xDD, 0xCC, 0xFF, 0x10)
