{-|
Reference model of CHIP-8 (COSMAC VIP behaviour, with switch-selectable
quirks), PLAN.md step 1.1. Plain Haskell, written for clarity: one call of
'step' executes one instruction, 'tick60' is the 60 Hz timer interrupt.
It never looks at clock cycles. Once it passes the test ROMs, it is the
oracle for the hardware core (layer 2 lockstep tests, docs/TESTING.md).

Decisions that the hardware core must copy exactly, because lockstep
testing compares architectural state:

* __Random numbers__ (@Cxnn@): a 16-bit Galois LFSR ('rngNext', taps
  0xB400, seed 0xACE1), stepped once per @Cxnn@; the byte is its low 8 bits.
* __Display wait__ (quirk on): @Dxyn@ waits for the /next/ 60 Hz tick, as
  the VIP does: the first time it is reached it only starts waiting
  ('VblankWaiting'), 'tick60' releases it, and the next 'step' draws.
* __Key wait__ (@Fx0A@): VIP behaviour. The instruction waits until a key is
  pressed (the lowest-numbered one if several are), then until that key is
  released, and only then stores it in Vx and moves on. Timers keep running.
* __Illegal opcodes__ (@0nnn@ other than 00E0/00EE, @5xyN@ and @9xyN@ with
  N ≠ 0, @8xyN@ for N in 8 to D and F, unknown @Ex@/@Fx@ forms), a return
  with an empty stack and a call with a full one (16 entries) all stop the
  machine: 'mHalt' is set and every later 'step' does nothing.
* __Memory__ is 4 KiB and wraps: every RAM access uses its address mod
  4096. I is 16 bits (@Fx1E@ never sets VF); PC is 12 bits.
* __Flags__: for @8xy4@, @8xy5@, @8xy6@, @8xy7@ and @8xyE@ the result is
  written first and VF last, so with x = F the flag wins. The flag is
  computed from the operands as they were before the instruction.
-}
module Chip8.Spec
  ( -- * Configuration
    Quirks (..)
  , vipQuirks
  , schipQuirks
  , quirksFromConfig
    -- * Machine state
  , Machine (..)
  , VblankWait (..)
  , KeyWait (..)
  , Halt (..)
  , Keys
  , newMachine
  , loadProgram
  , fontAddr
  , font
  , rngSeed
  , rngNext
    -- * Execution
  , step
  , tick60
  , runFrames
  , pixelAt
  , soundOn
  ) where

import Prelude

import Data.Bits
import qualified Data.Vector.Unboxed as U
import Data.Word

-- | Quirk switches. 'True' means the COSMAC VIP behaviour, except
-- 'qClipping', which is on for both VIP and SUPER-CHIP.
data Quirks = Quirks
  { qVfReset       :: Bool  -- ^ 8xy1/2/3 reset VF to 0
  , qMemoryIncI    :: Bool  -- ^ Fx55/Fx65 leave I = I + x + 1
  , qShiftVy       :: Bool  -- ^ 8xy6/8xyE shift Vy (otherwise Vx)
  , qJumpV0        :: Bool  -- ^ Bnnn jumps to V0 + nnn (otherwise Vx + xnn)
  , qDisplayWait   :: Bool  -- ^ Dxyn waits for the next 60 Hz tick
  , qClipping      :: Bool  -- ^ sprites clip at the edges (otherwise wrap)
  } deriving stock (Show, Eq)

vipQuirks :: Quirks
vipQuirks = Quirks True True True True True True

schipQuirks :: Quirks
schipQuirks = Quirks False False False False False True

-- | Quirks from the core's @ciConfig@ byte (docs/systems/chip8.md): bit
-- @i@ = 0 selects the VIP behaviour of quirk @i@ (0 VF reset, 1 memory,
-- 2 shift, 3 jump, 4 display wait); clipping is always on.
quirksFromConfig :: Word8 -> Quirks
quirksFromConfig c = Quirks
  { qVfReset     = not (testBit c 0)
  , qMemoryIncI  = not (testBit c 1)
  , qShiftVy     = not (testBit c 2)
  , qJumpV0      = not (testBit c 3)
  , qDisplayWait = not (testBit c 4)
  , qClipping    = True
  }

-- | Keypad state: bit @k@ set while key @k@ (0x0 to 0xF) is held.
type Keys = Word16

data VblankWait = VblankIdle | VblankWaiting | VblankReleased
  deriving stock (Show, Eq)

-- | Progress of an @Fx0A@: nothing pressed yet, or this key is held.
data KeyWait = KeyWaitNone | KeyWaitHeld Word8
  deriving stock (Show, Eq)

data Halt
  = HaltIllegal Word16     -- ^ illegal opcode
  | HaltStackUnderflow     -- ^ 00EE with an empty stack
  | HaltStackOverflow      -- ^ 2nnn with 16 entries on the stack
  deriving stock (Show, Eq)

data Machine = Machine
  { mRam     :: U.Vector Word8   -- ^ 4096 bytes
  , mV       :: U.Vector Word8   -- ^ V0 to VF
  , mI       :: Word16
  , mPc      :: Word16           -- ^ 12 bits
  , mStack   :: [Word16]         -- ^ return addresses, top first
  , mDelay   :: Word8
  , mSound   :: Word8
  , mDisplay :: U.Vector Bool    -- ^ 64 × 32, row-major
  , mRng     :: Word16
  , mVblank  :: VblankWait
  , mKeyWait :: KeyWait
  , mHalt    :: Maybe Halt
  } deriving stock (Show, Eq)

fontAddr :: Int
fontAddr = 0x050

-- | The usual 4×5 hex digits, 5 bytes each, at 'fontAddr'.
font :: [Word8]
font =
  [ 0xF0, 0x90, 0x90, 0x90, 0xF0, 0x20, 0x60, 0x20, 0x20, 0x70  -- 0 1
  , 0xF0, 0x10, 0xF0, 0x80, 0xF0, 0xF0, 0x10, 0xF0, 0x10, 0xF0  -- 2 3
  , 0x90, 0x90, 0xF0, 0x10, 0x10, 0xF0, 0x80, 0xF0, 0x10, 0xF0  -- 4 5
  , 0xF0, 0x80, 0xF0, 0x90, 0xF0, 0xF0, 0x10, 0x20, 0x40, 0x40  -- 6 7
  , 0xF0, 0x90, 0xF0, 0x90, 0xF0, 0xF0, 0x90, 0xF0, 0x10, 0xF0  -- 8 9
  , 0xF0, 0x90, 0xF0, 0x90, 0x90, 0xE0, 0x90, 0xE0, 0x90, 0xE0  -- A B
  , 0xF0, 0x80, 0x80, 0x80, 0xF0, 0xE0, 0x90, 0x90, 0x90, 0xE0  -- C D
  , 0xF0, 0x80, 0xF0, 0x80, 0xF0, 0xF0, 0x80, 0xF0, 0x80, 0x80  -- E F
  ]

rngSeed :: Word16
rngSeed = 0xACE1

-- | One step of the 16-bit Galois LFSR (taps 16, 14, 13, 11).
rngNext :: Word16 -> Word16
rngNext r = (r `shiftR` 1) `xor` (if testBit r 0 then 0xB400 else 0)

-- | Power-on state: font loaded, everything else zero, PC = 0x200.
newMachine :: Machine
newMachine = Machine
  { mRam     = U.replicate 4096 0 U.// zip [fontAddr ..] font
  , mV       = U.replicate 16 0
  , mI       = 0
  , mPc      = 0x200
  , mStack   = []
  , mDelay   = 0
  , mSound   = 0
  , mDisplay = U.replicate (64 * 32) False
  , mRng     = rngSeed
  , mVblank  = VblankIdle
  , mKeyWait = KeyWaitNone
  , mHalt    = Nothing
  }

-- | Copy a program to 0x200 (truncated at the end of RAM).
loadProgram :: [Word8] -> Machine -> Machine
loadProgram bytes m = m { mRam = mRam m U.// zip [0x200 .. 0xFFF] bytes }

pixelAt :: Machine -> Int -> Int -> Bool
pixelAt m x y = mDisplay m U.! (y * 64 + x)

-- | The buzzer sounds while the sound timer is non-zero.
soundOn :: Machine -> Bool
soundOn m = mSound m /= 0

-- | The 60 Hz interrupt: decrement both timers, release a display wait.
tick60 :: Machine -> Machine
tick60 m = m
  { mDelay  = dec (mDelay m)
  , mSound  = dec (mSound m)
  , mVblank = if mVblank m == VblankWaiting then VblankReleased else mVblank m
  }
 where
  dec t = if t == 0 then 0 else t - 1

-- | Run @n@ frames: in each, the keys for that frame number, @ipf@ steps,
-- then 'tick60'.
runFrames :: Quirks -> Int -> (Int -> Keys) -> Int -> Machine -> Machine
runFrames q ipf keys n m0 = go 0 m0
 where
  go f m
    | f >= n    = m
    | otherwise = go (f + 1) (tick60 (iterateN ipf (step q (keys f)) m))
  iterateN k g x = if k <= 0 then x else iterateN (k - 1) g (g x)

ram :: Machine -> Int -> Word8
ram m a = mRam m U.! (a .&. 0xFFF)

reg :: Machine -> Int -> Word8
reg m x = mV m U.! x

setRegs :: [(Int, Word8)] -> Machine -> Machine
setRegs rs m = m { mV = mV m U.// rs }

-- | Execute one instruction.
step :: Quirks -> Keys -> Machine -> Machine
step q keys m
  | Just _ <- mHalt m = m
  | otherwise = exec q keys m op (m { mPc = (mPc m + 2) .&. 0xFFF })
 where
  pc = fromIntegral (mPc m)
  op = fromIntegral (ram m pc) `shiftL` 8 .|. fromIntegral (ram m (pc + 1)) :: Word16

-- | @exec q keys m op m'@: @m@ is the state before the instruction, @m'@ the
-- same with PC already advanced past it.
exec :: Quirks -> Keys -> Machine -> Word16 -> Machine -> Machine
exec q keys m op m' = case (n3, n2, n1, n0) of
  (0x0, 0x0, 0xE, 0x0) -> m' { mDisplay = U.replicate (64 * 32) False }
  (0x0, 0x0, 0xE, 0xE) -> case mStack m of
    r : rs -> m' { mPc = r, mStack = rs }
    []     -> halt HaltStackUnderflow
  (0x0, _, _, _)       -> illegal
  (0x1, _, _, _)       -> m' { mPc = nnn }
  (0x2, _, _, _)
    | length (mStack m) >= 16 -> halt HaltStackOverflow
    | otherwise               -> m' { mPc = nnn, mStack = mPc m' : mStack m }
  (0x3, _, _, _)       -> skipIf (vx == kk)
  (0x4, _, _, _)       -> skipIf (vx /= kk)
  (0x5, _, _, 0x0)     -> skipIf (vx == vy)
  (0x6, _, _, _)       -> setRegs [(x, kk)] m'
  (0x7, _, _, _)       -> setRegs [(x, vx + kk)] m'
  (0x8, _, _, 0x0)     -> setRegs [(x, vy)] m'
  (0x8, _, _, 0x1)     -> logic (.|.)
  (0x8, _, _, 0x2)     -> logic (.&.)
  (0x8, _, _, 0x3)     -> logic xor
  (0x8, _, _, 0x4)     -> withFlag (vx + vy) (fromIntegral vx + fromIntegral vy > (0xFF :: Int))
  (0x8, _, _, 0x5)     -> withFlag (vx - vy) (vx >= vy)
  (0x8, _, _, 0x6)     -> let s = shiftSrc in withFlag (s `shiftR` 1) (testBit s 0)
  (0x8, _, _, 0x7)     -> withFlag (vy - vx) (vy >= vx)
  (0x8, _, _, 0xE)     -> let s = shiftSrc in withFlag (s `shiftL` 1) (testBit s 7)
  (0x9, _, _, 0x0)     -> skipIf (vx /= vy)
  (0xA, _, _, _)       -> m' { mI = nnn }
  (0xB, _, _, _)       -> m' { mPc = (nnn + fromIntegral (if qJumpV0 q then reg m 0 else vx)) .&. 0xFFF }
  (0xC, _, _, _)       -> let r = rngNext (mRng m)
                          in setRegs [(x, fromIntegral r .&. kk)] m' { mRng = r }
  (0xD, _, _, _)
    | qDisplayWait q && mVblank m /= VblankReleased -> m { mVblank = VblankWaiting }
    | otherwise -> draw (m' { mVblank = VblankIdle })
  (0xE, _, 0x9, 0xE)   -> skipIf (keyDown vx)
  (0xE, _, 0xA, 0x1)   -> skipIf (not (keyDown vx))
  (0xF, _, 0x0, 0x7)   -> setRegs [(x, mDelay m)] m'
  (0xF, _, 0x0, 0xA)   -> case mKeyWait m of
    KeyWaitNone -> case [k | k <- [0 .. 15], keyDown k] of
      k : _ -> m { mKeyWait = KeyWaitHeld k }
      []    -> m
    KeyWaitHeld k
      | keyDown k -> m
      | otherwise -> setRegs [(x, k)] m' { mKeyWait = KeyWaitNone }
  (0xF, _, 0x1, 0x5)   -> m' { mDelay = vx }
  (0xF, _, 0x1, 0x8)   -> m' { mSound = vx }
  (0xF, _, 0x1, 0xE)   -> m' { mI = mI m + fromIntegral vx }
  (0xF, _, 0x2, 0x9)   -> m' { mI = fromIntegral (fontAddr + 5 * fromIntegral (vx .&. 0xF)) }
  (0xF, _, 0x3, 0x3)   -> writeRam [(i, vx `div` 100), (i + 1, vx `div` 10 `mod` 10), (i + 2, vx `mod` 10)] m'
  (0xF, _, 0x5, 0x5)   -> incI (writeRam [ (i + r, reg m r) | r <- [0 .. x] ] m')
  (0xF, _, 0x6, 0x5)   -> incI (setRegs [ (r, ram m (i + r)) | r <- [0 .. x] ] m')
  _                    -> illegal
 where
  n3 = fromIntegral (op `shiftR` 12) :: Int
  n2 = fromIntegral (op `shiftR` 8 .&. 0xF) :: Int
  n1 = fromIntegral (op `shiftR` 4 .&. 0xF) :: Int
  n0 = fromIntegral (op .&. 0xF) :: Int
  x = n2
  y = n1
  nnn = op .&. 0xFFF
  kk = fromIntegral op :: Word8
  vx = reg m x
  vy = reg m y
  i = fromIntegral (mI m) :: Int

  halt h = m { mHalt = Just h }
  illegal = halt (HaltIllegal op)
  skipIf c = if c then m' { mPc = (mPc m' + 2) .&. 0xFFF } else m'
  keyDown k = testBit keys (fromIntegral (k .&. 0xF))

  logic f = setRegs ((x, f vx vy) : [ (0xF, 0) | qVfReset q ]) m'
  -- result first, then VF, so that VF wins when x = F
  withFlag r c = setRegs [(x, r), (0xF, if c then 1 else 0)] m'
  shiftSrc = if qShiftVy q then vy else vx

  incI mm = if qMemoryIncI q then mm { mI = mI m + fromIntegral x + 1 } else mm

  writeRam ws mm = mm { mRam = mRam mm U.// [ (a .&. 0xFFF, v) | (a, v) <- ws ] }

  -- Dxyn: n rows from I, at (Vx mod 64, Vy mod 32); XOR, VF = collision.
  draw mm =
    let x0 = fromIntegral vx `mod` 64
        y0 = fromIntegral vy `mod` 32
        pixels = [ (px, py)
                 | row <- [0 .. n0 - 1], col <- [0 .. 7]
                 , testBit (ram m (i + row)) (7 - col)
                 , let rx = x0 + col
                       ry = y0 + row
                 , not (qClipping q) || (rx < 64 && ry < 32)
                 , let px = rx `mod` 64
                       py = ry `mod` 32 ]
        idxs = [ py * 64 + px | (px, py) <- pixels ]
        disp = mDisplay m
        hit  = any (disp U.!) idxs
        disp' = U.accum (\b _ -> not b) disp [ (k, ()) | k <- idxs ]
    in setRegs [(0xF, if hit then 1 else 0)] mm { mDisplay = disp' }
