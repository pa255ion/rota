{-|
A small CHIP-8 assembler for the boot splash's CHIP-8 program (PLAN.md,
side track J, step 4: "the program's bytes come from the generator, so
no CHIP-8 assembler joins the flake"). Two passes: addresses from 0x200,
then the bytes. Labels are strings; 'Ref' adds an offset.
-}
module Rota.Chip8.Asm
  ( Reg
  , Ref (..)
  , Ins (..)
  , assemble
  , labelAddr
  , labels
  ) where

import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import qualified Data.Map.Strict as M
import Data.Word (Word8)

-- | V0 to VF.
type Reg = Int

-- | An address: a label plus an offset, or an absolute one.
data Ref = L String | LO String Int | A Int
  deriving stock (Eq, Show)

data Ins
  = Label String
  | Bytes [Word8]
  | CLS                 -- ^ 00E0
  | RET                 -- ^ 00EE
  | JP Ref              -- ^ 1nnn
  | CALL Ref            -- ^ 2nnn
  | SEk Reg Word8       -- ^ 3xkk: skip if Vx = kk
  | SNEk Reg Word8      -- ^ 4xkk: skip if Vx /= kk
  | SEr Reg Reg         -- ^ 5xy0
  | LDk Reg Word8       -- ^ 6xkk
  | ADDk Reg Word8      -- ^ 7xkk (no carry)
  | LDr Reg Reg         -- ^ 8xy0
  | ORr Reg Reg         -- ^ 8xy1
  | ANDr Reg Reg        -- ^ 8xy2
  | ADDr Reg Reg        -- ^ 8xy4: VF = carry
  | SUBr Reg Reg        -- ^ 8xy5: Vx -= Vy, VF = no borrow
  | SHRr Reg            -- ^ 8xx6 (x = y: the same under both shift quirks)
  | SUBNr Reg Reg       -- ^ 8xy7: Vx = Vy - Vx, VF = no borrow
  | SNEr Reg Reg        -- ^ 9xy0
  | LDI Ref             -- ^ Annn
  | DRW Reg Reg Int     -- ^ Dxyn
  | GETDT Reg           -- ^ Fx07
  | SETDT Reg           -- ^ Fx15
  | SETST Reg           -- ^ Fx18
  | ADDI Reg            -- ^ Fx1E
  | STORE Reg           -- ^ Fx55: V0 to Vx at I (I afterwards: quirk-dependent, never relied on)
  | LOAD Reg            -- ^ Fx65
  deriving stock (Eq, Show)

size :: Ins -> Int
size i = case i of
  Label _ -> 0
  Bytes bs -> length bs
  _ -> 2

labels :: [Ins] -> M.Map String Int
labels = go 0x200 M.empty
 where
  go _ m [] = m
  go a m (Label l : rest)
    | M.member l m = error ("assemble: label " <> l <> " twice")
    | otherwise = go a (M.insert l a m) rest
  go a m (i : rest) = go (a + size i) m rest

labelAddr :: [Ins] -> String -> Int
labelAddr prog l = M.findWithDefault (error ("no label " <> l)) l (labels prog)

-- | The program's bytes, loaded at 0x200.
assemble :: [Ins] -> [Word8]
assemble prog = concatMap enc prog
 where
  ls = labels prog
  ref r = case r of
    L l -> M.findWithDefault (error ("assemble: no label " <> l)) l ls
    LO l o -> M.findWithDefault (error ("assemble: no label " <> l)) l ls + o
    A a -> a
  w :: Int -> [Word8]
  w x = [fromIntegral (x `shiftR` 8), fromIntegral (x .&. 0xFF)]
  nnn op r = let a = ref r in if a > 0xFFF then error "assemble: address above 0xFFF" else w (op `shiftL` 12 .|. a)
  xkk op x k = w (op `shiftL` 12 .|. x `shiftL` 8 .|. fromIntegral k)
  xy op x y n = w (op `shiftL` 12 .|. x `shiftL` 8 .|. y `shiftL` 4 .|. n)
  fx x k = w (0xF000 .|. x `shiftL` 8 .|. k)
  enc i = case i of
    Label _ -> []
    Bytes bs -> bs
    CLS -> w 0x00E0
    RET -> w 0x00EE
    JP r -> nnn 1 r
    CALL r -> nnn 2 r
    SEk x k -> xkk 3 x k
    SNEk x k -> xkk 4 x k
    SEr x y -> xy 5 x y 0
    LDk x k -> xkk 6 x k
    ADDk x k -> xkk 7 x k
    LDr x y -> xy 8 x y 0
    ORr x y -> xy 8 x y 1
    ANDr x y -> xy 8 x y 2
    ADDr x y -> xy 8 x y 4
    SUBr x y -> xy 8 x y 5
    SHRr x -> xy 8 x x 6
    SUBNr x y -> xy 8 x y 7
    SNEr x y -> xy 9 x y 0
    LDI r -> nnn 0xA r
    DRW x y n
      | n < 1 || n > 15 -> error "assemble: DRW height 1 to 15"
      | otherwise -> xy 0xD x y n
    GETDT x -> fx x 0x07
    SETDT x -> fx x 0x15
    SETST x -> fx x 0x18
    ADDI x -> fx x 0x1E
    STORE x -> fx x 0x55
    LOAD x -> fx x 0x65
