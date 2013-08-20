{-# LANGUAGE DeriveDataTypeable, BangPatterns #-}

-- | A type-safe list that has at least one element.
module Data.List.NonEmpty(
                           NonEmpty,
                           -- * Accessors
                           neHead,
                           neTail,
                           -- * Constructors
                           nonEmpty,
                           (|:),
                           toNonEmpty,
                           unsafeToNonEmpty,
                           (.:),
                           -- * List functions
                           suml,
                           sumr,
                           reverse,
                           scanl,
                           scanl1,
                           scanr,
                           scanr1,
                           iterate,
                           cycle,
                           inits,
                           tails,
                           sort,
                           insert,
                           unzip,
                           -- * Tests
                           prop_neHead,
                           prop_neTail,
                           prop_nonEmpty,
                           prop_nonEmptyAlias,
                           prop_toNonEmpty,
                           prop_unsafeNonEmpty,
                           prop_cons,
                           prop_append,
                           prop_reverse
                         ) where

import Control.Applicative
import Control.Monad
import Control.Arrow
import Data.Foldable
import Data.Maybe
import Data.Traversable
import Data.Typeable (Typeable)
import Data.Data (Data)
import Data.Semigroup
import qualified Data.List as L
import Prelude hiding (foldr, reverse, scanl, scanl1, scanr, scanr1, iterate, repeat, cycle, zip, unzip)
import Test.QuickCheck hiding (NonEmpty)

-- | An list with at least one element.
data NonEmpty a = NonEmpty {
  neHead :: a, -- ^ The head of the non-empty list.
  neTail :: [a] -- ^ The tail of the non-empty list.
} deriving (Eq, Ord, Typeable, Data)

instance Functor NonEmpty where
  fmap f (NonEmpty h t) = NonEmpty (f h) (fmap f t)

instance Applicative NonEmpty where
  pure = return
  (<*>) = ap

instance Monad NonEmpty where
  return = flip NonEmpty []
  NonEmpty h t >>= f = let NonEmpty a b = f h
                           k = t >>= toList . f
                       in NonEmpty a (b ++ k)

instance Foldable NonEmpty where
  foldr f x (NonEmpty h t) = f h (foldr f x t)
  foldl f x (NonEmpty h t) = foldl' f x (h:t)

instance Traversable NonEmpty where
  traverse f a = NonEmpty <$> head <*> tail <$> traverse f (toList a)

instance (Show a) => Show (NonEmpty a) where
  show (NonEmpty h t) = '|' : show (h:t) ++ "|"

instance Semigroup (NonEmpty a) where
  NonEmpty a b .++. NonEmpty c d = NonEmpty a (b ++ c:d)

-- | Constructs a non-empty list with the given head and tail.
nonEmpty :: a -- ^ The head.
            -> [a] -- ^ The tail.
            -> NonEmpty a
nonEmpty = (|:)

-- | Constructs a non-empty list with the given head and tail (an alias for @nonEmpty@).
(|:) :: a -- ^ The head.
        -> [a] -- ^ The tail.
        -> NonEmpty a
h |: ts = snf ts `seq` NonEmpty h ts

snf :: [a] -> ()
snf [] = ()
snf (_:bs) = snf bs

-- | Tries to convert a list to a @NonEmpty@ returning @Nothing@ if the given list is empty.
toNonEmpty :: [a] -- ^ The list to convert.
              -> Maybe (NonEmpty a)
toNonEmpty [] = Nothing
toNonEmpty (h:t) = Just (h |: t)

-- | /WARNING: Fails if given the empty list./
-- Tries to convert a list to a @NonEmpty@.
unsafeToNonEmpty :: [a] -- ^ The list to convert (must not be empty).
                    -> NonEmpty a
unsafeToNonEmpty = fromMaybe (error "unsafeToNonEmpty on empty list") . toNonEmpty

-- | Prepends a value to a non-empty list.
(.:) :: a -- ^ The value to prepend.
        -> NonEmpty a -- ^ The non-empty list to prepend to.
        -> NonEmpty a
a .: NonEmpty h t = let !c = h:t in NonEmpty a c

-- | Reduce left on the given non-empty list using a semigroup.
suml :: (Semigroup a) => NonEmpty a -> a
suml (NonEmpty h t) = L.foldl1' (.++.) (h:t)

-- | Reduce right on the given non-empty list using a semigroup.
sumr :: (Semigroup a) => NonEmpty a -> a
sumr (NonEmpty h t) = L.foldr1 (.++.) (h:t)

-- | Reverses the elements of the (finite) non-empty list.
reverse :: NonEmpty a
           -> NonEmpty a
reverse = list L.reverse

scanl :: (b -> a -> b)
         -> b
         -> NonEmpty a
         -> NonEmpty b
scanl = (list .) . L.scanl

scanl1 :: (a -> a -> a)
          -> NonEmpty a
          -> NonEmpty a
scanl1 = list . L.scanl1

scanr :: (a -> b -> b)
         -> b
         -> NonEmpty a
         -> NonEmpty b
scanr = (list .) . L.scanr

scanr1 :: (a -> a -> a)
          -> NonEmpty a
          -> NonEmpty a
scanr1 = list . L.scanr1

iterate :: (a -> a)
           -> a
           -> NonEmpty a
iterate = (unsafeToNonEmpty .) . L.iterate

cycle :: (Foldable f) =>
         f a
         -> NonEmpty a
cycle = list L.cycle

inits :: [a]
         -> NonEmpty [a]
inits = unsafeToNonEmpty . L.inits

tails :: [a]
         -> NonEmpty [a]
tails = unsafeToNonEmpty . L.tails

sort :: (Ord a) =>
        NonEmpty a
        -> NonEmpty a
sort = list L.sort

insert :: (Ord a) =>
          a ->
          NonEmpty a ->
          NonEmpty a
insert a = unsafeToNonEmpty . L.insert a . toList

unzip :: NonEmpty (a, b) ->
         (NonEmpty a, NonEmpty b)
unzip = (unsafeToNonEmpty *** unsafeToNonEmpty) . L.unzip . toList

-----------
-- TESTS --
-----------

instance (Arbitrary a) => Arbitrary (NonEmpty a) where
  arbitrary = nonEmpty <$> arbitrary <*> arbitrary
  shrink = (unsafeToNonEmpty <$>) . shrink . toList

prop_neHead :: String -> [String] -> Bool
prop_neHead h t = neHead (nonEmpty h t) == h

prop_neTail :: String -> [String] -> Bool
prop_neTail h t = neTail (nonEmpty h t) == t

prop_nonEmpty :: String -> [String] -> Bool
prop_nonEmpty h t = toList (nonEmpty h t) == h:t

prop_nonEmptyAlias :: String -> [String] -> Bool
prop_nonEmptyAlias h t = nonEmpty h t == h |: t

prop_toNonEmpty :: [String] -> Bool
prop_toNonEmpty x = toNonEmpty x == case x of [] -> Nothing
                                              (h:t) -> Just (nonEmpty h t)

prop_unsafeNonEmpty :: [String] -> Property
prop_unsafeNonEmpty x = not (null x) ==> prop_toNonEmpty x

prop_cons :: String -> NonEmpty String -> Bool
prop_cons a as = toList (a .: as) == a : toList as

prop_append :: NonEmpty String -> NonEmpty String -> Bool
prop_append a b = toList (a .++. b) == neHead a : neTail a ++ neHead b : neTail b

prop_reverse :: NonEmpty String -> Bool
prop_reverse x = (reverse . reverse) x == x

------------------
-- Not exported --
------------------

list :: (Foldable f) => ([a] -> [b]) -> f a -> NonEmpty b
list = (unsafeToNonEmpty .) . (. toList)
