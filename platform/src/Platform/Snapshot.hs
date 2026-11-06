{-# LANGUAGE ExistentialQuantification #-}
{-|
Save states for the desktop emulators (side track Q, Q.8; the user,
2026-10-25: save and load states from the desktop's menu). A model's state
lives in mutable vectors and references; a system lists them as 'Field's
(every one that changes while it runs), and 'snapshot' writes them into
one blob, 'restore' puts a blob back into the same list.

The blob: "ECS1", the number of fields, then per field its kind, its
payload's length and the payload (little-endian: vectors as their
elements, a reference as its value's 'show'). 'restore' checks every
field's kind and length (and that each value reads back) before it writes
anything, so a blob from another build or another game is refused whole.
A blob belongs to the build that made it: the field lists change with
the models.
-}
module Platform.Snapshot
  ( Field (..)
  , snapshot
  , restore
  , fnv1a
  ) where

import Prelude

import Control.Monad (forM, forM_)
import Control.Monad.ST (ST)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import Data.Bits (shiftL, xor, (.|.))
import Data.Int (Int16, Int32, Int64)
import Data.STRef
import qualified Data.Vector.Unboxed.Mutable as MU
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Float (castWord64ToDouble)
import Text.Read (readMaybe)

-- | One piece of a model's state.
data Field s
  = FW8 (MU.MVector s Word8)
  | FW16 (MU.MVector s Word16)
  | FW32 (MU.MVector s Word32)
  | FW64 (MU.MVector s Word64)
  | FI16 (MU.MVector s Int16)
  | FI32 (MU.MVector s Int32)
  | FInt (MU.MVector s Int)
  | FBool (MU.MVector s Bool)
  | FDouble (MU.MVector s Double)
  | forall a. (Show a, Read a) => FRef (STRef s a)
  | FCheck Word64
    -- ^ a value the blob must have here (the game's 'fnv1a', say): compared, never written

kind :: Field s -> Word8
kind f = case f of
  FW8 _ -> 1
  FW16 _ -> 2
  FW32 _ -> 3
  FW64 _ -> 4
  FI16 _ -> 5
  FI32 _ -> 6
  FInt _ -> 7
  FBool _ -> 8
  FDouble _ -> 9
  FRef _ -> 10
  FCheck _ -> 11

magic :: BS.ByteString
magic = BC.pack "ECS1"

-- | The fields' values, as one blob.
snapshot :: [Field s] -> ST s BS.ByteString
snapshot fs = do
  parts <- mapM payload fs
  let one f p = B.word8 (kind f) <> B.word32LE (fromIntegral (BL.length p)) <> B.lazyByteString p
  pure (BL.toStrict (B.toLazyByteString (B.byteString magic <> B.word32LE (fromIntegral (length fs)) <> mconcat (zipWith one fs parts))))
 where
  vec :: MU.Unbox a => (a -> B.Builder) -> MU.MVector s a -> ST s BL.ByteString
  vec put v = B.toLazyByteString . mconcat <$> forM [0 .. MU.length v - 1] (fmap put . MU.read v)
  payload f = case f of
    FW8 v -> vec B.word8 v
    FW16 v -> vec B.word16LE v
    FW32 v -> vec B.word32LE v
    FW64 v -> vec B.word64LE v
    FI16 v -> vec B.int16LE v
    FI32 v -> vec B.int32LE v
    FInt v -> vec (B.int64LE . fromIntegral) v
    FBool v -> vec (B.word8 . fromIntegral . fromEnum) v
    FDouble v -> vec B.doubleLE v
    FRef r -> B.toLazyByteString . B.stringUtf8 . show <$> readSTRef r
    FCheck w -> pure (B.toLazyByteString (B.word64LE w))

-- | A blob back into the fields; 'Left' (nothing written) when it doesn't
-- fit them.
restore :: [Field s] -> BS.ByteString -> ST s (Either String ())
restore fs blob
  | BS.take 4 blob /= magic = pure (Left "not a save state")
  | otherwise = case chunks (u32 (BS.drop 4 blob)) (BS.drop 8 blob) of
      Nothing -> pure (Left "a save state cut short")
      Just cs
        | length cs /= length fs -> pure (Left ("a save state of " <> show (length cs) <> " fields, this machine has " <> show (length fs)))
        | otherwise -> do
            checks <- sequence (zipWith check fs cs)
            case sequence checks of
              Left e -> pure (Left e)
              Right writes -> Right <$> sequence_ writes
 where
  u32 b = foldr (\i a -> (a `shiftL` 8) .|. fromIntegral (BS.index b i)) 0 [0 .. 3] :: Int
  chunks :: Int -> BS.ByteString -> Maybe [(Word8, BS.ByteString)]
  chunks 0 _ = Just []
  chunks n b
    | BS.length b < 5 = Nothing
    | BS.length body < len = Nothing
    | otherwise = ((BS.index b 0, body) :) <$> chunks (n - 1) (BS.drop (5 + len) b)
   where
    len = u32 (BS.drop 1 b)
    body = BS.take len (BS.drop 5 b)
  -- the field's write, if its kind and length fit
  check :: Field s -> (Word8, BS.ByteString) -> ST s (Either String (ST s ()))
  check f (k, p)
    | k /= kind f = pure (Left ("a save state's field of kind " <> show k <> " where this machine has " <> show (kind f)))
    | otherwise = pure $ case f of
        FW8 v -> vec 1 (\i -> BS.index p i) v
        FW16 v -> vec 2 (\i -> fromIntegral (le 2 i)) v
        FW32 v -> vec 4 (\i -> fromIntegral (le 4 i)) v
        FW64 v -> vec 8 (\i -> fromIntegral (le 8 i)) v
        FI16 v -> vec 2 (\i -> fromIntegral (fromIntegral (le 2 i) :: Word16)) v
        FI32 v -> vec 4 (\i -> fromIntegral (fromIntegral (le 4 i) :: Word32)) v
        FInt v -> vec 8 (\i -> fromIntegral (fromIntegral (le 8 i) :: Int64)) v
        FBool v -> vec 1 (\i -> BS.index p i /= 0) v
        FDouble v -> vec 8 (\i -> castWord64ToDouble (le 8 i)) v
        FRef r -> case readMaybe (BC.unpack p) of
          Just x -> Right (writeSTRef r x)
          Nothing -> Left "a save state's value that doesn't read back"
        FCheck w
          | BS.length p == 8 && le 8 0 == w -> Right (pure ())
          | otherwise -> Left "a save state of another game"
   where
    le :: Int -> Int -> Word64
    le n i = foldr (\j a -> (a `shiftL` 8) .|. fromIntegral (BS.index p (n * i + j))) 0 [0 .. n - 1]
    vec :: MU.Unbox a => Int -> (Int -> a) -> MU.MVector s a -> Either String (ST s ())
    vec size get v
      | BS.length p /= size * MU.length v = Left ("a save state's field of " <> show (BS.length p) <> " bytes where this machine has " <> show (size * MU.length v))
      | otherwise = Right (forM_ [0 .. MU.length v - 1] (\i -> MU.write v i (get i)))

-- | FNV-1a, 64 bits: a game's identity for 'FCheck'.
fnv1a :: BS.ByteString -> Word64
fnv1a = BS.foldl' (\h b -> (h `xor` fromIntegral b) * 0x100000001B3) 0xCBF29CE484222325
