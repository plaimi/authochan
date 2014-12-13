{- |
Module      :  $Header$
Description :  Typeclass for generating mutations for QuickCheck.
Copyright   :  (c) plaimi 2014
License     :  AGPL-3

Maintainer  :  tempuhs@plaimi.net
-} module Test.MutableGen where

import Control.Applicative
  (
  (<$>),
  (<*>),
  )
import Control.Monad
  (
  liftM2,
  liftM3,
  liftM4,
  liftM5,
  )
import Data.Bits
  (
  Bits,
  (.&.),
  (.|.),
  complement,
  complementBit,
  bitSizeMaybe,
  rotate,
  shift,
  xor,
  )
import qualified Data.ByteString as B
import Data.Int
  (
  Int64,
  )
import Data.List
  (
  inits,
  tails,
  )
import qualified Data.Tuple.All as Tu
import Data.Word
  (
  Word8,
  )
import Test.QuickCheck
  (
  Gen,
  arbitrary,
  choose,
  elements,
  listOf,
  listOf1,
  oneof,
  resize,
  sized
  )


-- | Instances of 'MutableGen' use 'Gen' to provide varying degrees of
-- random mutations of values.
class MutableGen a where
  mutate :: a -> Gen a
  -- | @'mutate' a@ generates random mutations of 'a'.
  mutate a = oneof $ newMutable : mutations a
  mutations :: a -> [Gen a]
  -- | @'mutations' a@ is a list of mutation generators for 'a'. It is used in
  -- the default implementation of 'mutate'.
  mutations _ = []
  mutateList :: [a] -> Gen [a]
  -- | @'mutateList' a@ generates random mutations of the list 'a'. Instances
  -- may define this to provide custom list mutations, e.g. 'String's for the
  -- 'Char' instance.
  mutateList = mutateListDefault
  -- | 'newMutable' is the generator to use for new mutable values.
  newMutable :: Gen a

instance (MutableGen a) => MutableGen [a] where
  mutate = mutateList
  newMutable = listOf newMutable

instance MutableGen Int64 where
  mutations a = [mutateBits a, mutateNum a]
  newMutable = arbitrary

instance MutableGen Word8 where
  mutations a = [mutateBits a, mutateNum a]
  newMutable = arbitrary

ap1 :: (Tu.Sel1 t a, Tu.Upd1 b t t1, Functor f) => (a -> f b) -> t -> f t1
ap1 f a = flip Tu.upd1 a <$> f (Tu.sel1 a)
ap2 :: (Tu.Sel2 t a, Tu.Upd2 b t t1, Functor f) => (a -> f b) -> t -> f t1
ap2 f a = flip Tu.upd2 a <$> f (Tu.sel2 a)
ap3 :: (Tu.Sel3 t a, Tu.Upd3 b t t1, Functor f) => (a -> f b) -> t -> f t1
ap3 f a = flip Tu.upd3 a <$> f (Tu.sel3 a)
ap4 :: (Tu.Sel4 t a, Tu.Upd4 b t t1, Functor f) => (a -> f b) -> t -> f t1
ap4 f a = flip Tu.upd4 a <$> f (Tu.sel4 a)
ap5 :: (Tu.Sel5 t a, Tu.Upd5 b t t1, Functor f) => (a -> f b) -> t -> f t1
ap5 f a = flip Tu.upd5 a <$> f (Tu.sel5 a)

instance (MutableGen a, MutableGen b) => MutableGen (a, b) where
  mutations a = [g ap1, g ap2]
    where g f = f mutate a
  newMutable = liftM2 (,) newMutable newMutable

instance (MutableGen a, MutableGen b, MutableGen c)
      => MutableGen (a, b, c) where
  mutations a = [g ap1, g ap2, g ap3]
    where g f = f mutate a
  newMutable = liftM3 (,,) newMutable newMutable newMutable

instance (MutableGen a, MutableGen b, MutableGen c, MutableGen d)
      => MutableGen (a, b, c, d) where
  mutations a = [g ap1, g ap2, g ap3, g ap4]
    where g f = f mutate a
  newMutable = liftM4 (,,,) newMutable newMutable newMutable newMutable

instance (MutableGen a
         ,MutableGen b
         ,MutableGen c
         ,MutableGen d
         ,MutableGen e)
      => MutableGen (a, b, c, d, e) where
  mutations a = [g ap1, g ap2, g ap3, g ap4, g ap5]
    where g f = f mutate a
  newMutable = liftM5 (,,,,) newMutable newMutable newMutable newMutable
                             newMutable

instance MutableGen B.ByteString where
  mutations a = [B.pack <$> mutateListDefault (B.unpack a)]
  newMutable = B.pack <$> newMutable

mutateSized :: MutableGen a => a -> Gen a
-- | @'mutateSized' a@ generates recursive mutations of 'a', depending on
-- QuickCheck's 'size' parametre.
mutateSized a = sized $ \i -> choose (-i `div` 2, i) >>= go (mutate a)
  where go a' n | n > 0     = a' >>= resize n . mutateSized
                | otherwise = a'

mutateBits :: (Bits a, MutableGen a) => a -> Gen a
-- | @'mutateBits' a@ generates bitwise mutations of 'a'.
mutateBits a =
  oneof [return $ complement a
        ,oneof [f a <$> newMutable | f <- [(.&.), (.|.), xor]]
        ,oneof $ [rotate a <$> choose (-b, b) | Just b <- [bs]]
              ++ [shift a <$> elements [-1, 1]]
        ,complementBit a <$> maybe arbitrary (\b -> choose (0, b - 1)) bs]
  where bs = bitSizeMaybe a

mutateNum :: (MutableGen a, Num a) => a -> Gen a
-- | @'mutateNum' a@ generates arithmetic mutations of 'a'.
mutateNum a = oneof [return $ negate a
                    ,(a -) <$> newMutable
                    ,(a *) <$> newMutable]

mutateListDefault :: MutableGen a => [a] -> Gen [a]
-- | 'mutateListDefault' is the default implementation of 'mutateList',
-- combining 'mutateListStructure' and 'mutateElement'.
mutateListDefault a = oneof [mutateListStructure a, mutateElement a]

mutateElement :: MutableGen a => [a] -> Gen [a]
-- | @'mutateElement' a@ mutates a random element of 'a'.
mutateElement [] = return []
mutateElement a  = do
  (x, e:y) <- flip splitAt a <$> choose (0, length a - 1)
  e'       <- mutate e
  return $ x ++ e' : y

swapElements :: [a] -> Int -> Int -> [a]
-- | @'swapElements' l a b@ swaps the elements at position 'a' and 'b' in 'l'.
swapElements l a b | a > b  = swapElements l b a
                   | a == b = l
swapElements l 0 b = (y : xs) ++ x : ys where (x:xs, y:ys) = splitAt b l
swapElements l a b = head l : swapElements (tail l) (a - 1) (b - 1)

mutateListStructure :: MutableGen a => [a] -> Gen [a]
-- | @'mutateListStructure' a@ generates mutations of 'a' where elements may
-- be added, removed or reordered, and subsequences may be mutated.
mutateListStructure a | not (null a) = oneof [return $ reverse a
                                             ,splitMut a
                                             ,swapElements a <$> ri <*> ri]
                      | otherwise    = return a
  where ri = choose (0, length a - 1)

splitMut :: MutableGen a => [a] -> Gen [a]
-- | 'splitMut' splits a list of @'MutableGen' a@ at a random index, and then
-- 'mutateMut's the result into a tuple.'splitMut' then returns either
-- the first element of the tuple, the tuple concatenated, the tuple with
-- inserted elements either from the list of @'MutableGen' a@s or completely
-- new elements, or the tuple with one or several removed elements.
splitMut a = do
  s        <- flip splitAt a <$> choose (0, length a)
  (lh, rh) <- mutateMut s
  oneof [return lh, return $ lh ++ rh, insMut a (lh, rh), delMut (lh, rh)]

mutateMut :: MutableGen a => ([a], [a]) -> Gen ([a], [a])
-- | 'mutateMut' mutates or doesn't mutate the elements of the passed in
-- tuple, and replace or doesn't replace one element of the tuple with the
-- other.
mutateMut (lh, rh) = oneof [(,) <$> x <*> y | x <- m, y <- m]
  where m = [f v | f <- [return, mutateList], v <- [lh, rh]]

insMut :: MutableGen a => [a] -> ([a], [a]) -> Gen [a]
-- | 'insMut' inserts list elements between the tuple elements, and then
-- concatenates them.
insMut a (lh, rh) = do
  let m = [last lh | not $ null lh] ++ [head rh | not $ null rh]
  r <- case m of
         [] -> return []
         _  -> return <$> elements m
  g <- elements $ newMutable : [elements a | not $ null a] ++
                               (return <$> r)
  x <- oneof [sized $ \n -> replicate <$> choose (1, n) <*> g
             ,listOf1 g]
  return $ concat [lh, x, rh]

delMut :: MutableGen a => ([a], [a]) -> Gen [a]
-- | 'delMut' either deletes a random number of elements from the end of the
-- first list in the passed in tuple, or a random number of elements from the
-- beginning of the second list, and then concatenates them.
delMut (lh, rh) = do
  (lf, rf) <- elements [(inits lh, [rh]), ([lh], tails rh)]
  (++) <$> elements lf <*> elements rf
