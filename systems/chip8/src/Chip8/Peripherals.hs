{-|
CHIP-8 input, tick generation and sound (PLAN.md step 1.5), between the
board's buttons and switches and the core ("Chip8.Core").

__Keys (stage 1, buttons).__ Provisional table, to be confirmed by the
user (docs/systems/chip8.md, "Input"):

> BTNU 2   BTND 8   BTNL 4   BTNR 6       (alone)
> BTNU E   BTND F   BTNL A   BTNR B       (with BTNC held)

2/4/6/8 are the usual directions; E/F/A move and select in the Timendus
menus (5-quirks, 6-keypad), and B is the key of 7-beep. BTNC alone is no
key. A Pmod keypad (stage 2) will give all 16.

__Ticks.__ Instructions run at about 700 Hz, or about 10 kHz with SW5
('instrTicker'); the 60 Hz timer tick is the VGA vertical blank (the top
takes it from 'vgaVBlankStart', so 59.94 Hz with the 25.176 MHz pixel
clock).

__Sound.__ A 440 Hz square wave while the sound timer runs ('beeper').
-}
module Chip8.Peripherals
  ( buttonKeys
  , buttonsToKeys
  , debouncedButtons
  , keysOf
  , SlowPeriod
  , FastPeriod
  , instrTicker
  , BeepHalfPeriod
  , beeper
  ) where

import Clash.Prelude

import Platform.Input.Debounce (syncDebounce)

-- | Key bits (bit k = key k) for the debounced buttons, given in the
-- order BTNC, BTND, BTNL, BTNR, BTNU (as in "M0a.Top").
buttonKeys :: Bool -> Bool -> Bool -> Bool -> Bool -> BitVector 16
buttonKeys c d l r u = key u 0x2 0xE .|. key d 0x8 0xF .|. key l 0x4 0xA .|. key r 0x6 0xB
 where
  key b plain shifted
    | not b     = 0
    | c         = bit shifted
    | otherwise = bit plain

-- | Raw button pins (BTNC, BTND, BTNL, BTNR, BTNU) to key bits: each button
-- synchronised and debounced over @db@ clocks ('debouncedButtons'), then
-- 'buttonKeys'.
buttonsToKeys
  :: forall db dom
   . (HiddenClockResetEnable dom, KnownNat db, 1 <= db)
  => SNat db -> Signal dom (Vec 5 Bit) -> Signal dom (BitVector 16)
buttonsToKeys db = fmap keysOf . debouncedButtons db

-- | Raw button pins, each synchronised and debounced over @db@ clocks
-- (True = pressed), in the same order. The top also hands these to the PS
-- for the menu ("Platform.Ps.Link.psIn").
debouncedButtons
  :: forall db dom
   . (HiddenClockResetEnable dom, KnownNat db, 1 <= db)
  => SNat db -> Signal dom (Vec 5 Bit) -> Signal dom (Vec 5 Bool)
debouncedButtons db btns = bundle (map (syncDebounce db . fmap bitToBool) (unbundle btns))

-- | 'buttonKeys' of debounced buttons in pin order (C, D, L, R, U).
keysOf :: Vec 5 Bool -> BitVector 16
keysOf v = buttonKeys (v !! (0 :: Int)) (v !! (1 :: Int)) (v !! (2 :: Int)) (v !! (3 :: Int)) (v !! (4 :: Int))

-- | 25.176 MHz / 700 Hz and / 10 kHz: clocks per instruction in @DomPix@.
type SlowPeriod = 35966
type FastPeriod = 2518

-- | One pulse every @slow@ clocks, or every @fast@ clocks while @fastSel@
-- is high. Switching speed restarts the count.
instrTicker
  :: forall slow fast dom
   . (HiddenClockResetEnable dom, KnownNat slow, KnownNat fast, 1 <= slow, 1 <= fast, fast <= slow)
  => SNat slow -> SNat fast -> Signal dom Bool -> Signal dom Bool
instrTicker _ _ fastSel = mealy step (0 :: Index slow) fastSel
 where
  step c fs =
    let top = if fs then natToNum @(fast - 1) else maxBound
    in if c >= top then (0, True) else (c + 1, False)

-- | @DomPix@ clocks per half period of 440 Hz.
type BeepHalfPeriod = 28609

-- | A square wave with half period @n@ clocks while @on@, low otherwise.
beeper
  :: forall n dom
   . (HiddenClockResetEnable dom, KnownNat n, 1 <= n)
  => SNat n -> Signal dom Bool -> Signal dom Bit
beeper _ on = moore step (\(_, b) -> b) (0 :: Index n, 0 :: Bit) on
 where
  step (c, b) o
    | not o           = (0, 0)
    | c == maxBound   = (0, complement b)
    | otherwise       = (c + 1, b)
