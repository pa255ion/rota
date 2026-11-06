-- | Platform.Snapshot (side track Q, Q.8: the desktop's save states):
-- every kind of field round-trips, and a blob that doesn't fit is refused
-- without writing anything.
module Test.Platform.Snapshot (tests) where

import Prelude

import Control.Monad.ST (ST, runST)
import qualified Data.ByteString as BS
import Data.Int (Int16)
import Data.STRef
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import Data.Word (Word16, Word8)
import Test.Tasty
import Test.Tasty.HUnit

import Platform.Snapshot

data Box s = Box (MU.MVector s Word8) (MU.MVector s Word16) (MU.MVector s Int) (MU.MVector s Int16) (MU.MVector s Bool) (MU.MVector s Double) (STRef s [(Int, Maybe Bool)])

newBox :: Int -> ST s (Box s)
newBox n = Box <$> MU.replicate n 0 <*> MU.replicate n 0 <*> MU.replicate n 0 <*> MU.replicate n 0 <*> MU.replicate n False <*> MU.replicate n 0 <*> newSTRef []

fields :: Box s -> [Field s]
fields (Box a b c d e f g) = [FW8 a, FW16 b, FInt c, FI16 d, FBool e, FDouble f, FRef g]

contents :: Box s -> ST s ([Word8], [Word16], [Int], [Int16], [Bool], [Double], [(Int, Maybe Bool)])
contents (Box a b c d e f g) = (,,,,,,) <$> l a <*> l b <*> l c <*> l d <*> l e <*> l f <*> readSTRef g
 where
  l :: MU.Unbox x => MU.MVector s x -> ST s [x]
  l v = U.toList <$> U.freeze v

tests :: TestTree
tests = testGroup "Platform.Snapshot (the desktop's save states)"
  [ testCase "every kind of field comes back as it was" $ do
      let (want, got) = runST $ do
            x <- newBox 3
            let Box a b c d e f g = x
            MU.write a 1 0xAB >> MU.write b 2 0xBEEF >> MU.write c 0 (-123456789012) >> MU.write d 1 (-2)
            MU.write e 2 True >> MU.write f 0 (-1.5e-300) >> writeSTRef g [(7, Just True), (-1, Nothing)]
            blob <- snapshot (fields x)
            w <- contents x
            y <- newBox 3
            r <- restore (fields y) blob
            g' <- contents y
            pure (Right w, r >> Right g')
      got @?= want
  , testCase "a blob of another size is refused, nothing written" $ do
      let (r, untouched) = runST $ do
            x <- newBox 3
            blob <- snapshot (fields x)
            y <- newBox 4
            let Box a _ _ _ _ _ _ = y
            MU.write a 0 9
            res <- restore (fields y) blob
            v <- MU.read a 0
            pure (res, v)
      assertBool "accepted" (either (const True) (const False) r)
      untouched @?= 9
  , testCase "a check field: the same value restores, another is refused" $ do
      let (same, other) = runST $ do
            x <- newBox 2
            blob <- snapshot (FCheck (fnv1a (BS.pack [1, 2])) : fields x)
            a <- restore (FCheck (fnv1a (BS.pack [1, 2])) : fields x) blob
            b <- restore (FCheck (fnv1a (BS.pack [1, 3])) : fields x) blob
            pure (a, b)
      same @?= Right ()
      assertBool "accepted" (either (const True) (const False) other)
  , testCase "not a save state, or cut short: refused" $ do
      let rs = runST $ do
            x <- newBox 2
            blob <- snapshot (fields x)
            a <- restore (fields x) (BS.pack [1, 2, 3, 4])
            b <- restore (fields x) (BS.take (BS.length blob - 1) blob)
            pure [a, b]
      mapM_ (\r -> assertBool "accepted" (either (const True) (const False) r)) rs
  ]
