{-# LANGUAGE DeriveDataTypeable #-}

-- | A wrapper of @NonEmpty@ that has a zip-like @Applicative@ instance.
module Data.List.ZipNonEmpty(
                              ZipNonEmpty,
                              -- * Accessors
                              ne,
                              zipNe,
                              -- * Combinators
                              usingNe,
                              usingZne
                            ) where

import Data.List
import Data.Semigroup
import Data.Foldable
import Data.Typeable (Typeable)
import Data.Data (Data)
import Data.List.NonEmpty
import Control.Applicative

-- | A wrapper of @NonEmpty@ that has a zip-like @Applicative@ instance.
newtype ZipNonEmpty a = Z {
  -- | Unwraps a zip-like non-empty list.
  ne :: NonEmpty a
} deriving (Eq, Ord, Typeable, Data)

-- | Wraps a non-empty list.
zipNe :: NonEmpty a ->
         ZipNonEmpty a
zipNe = Z

-- | Runs a function for non-empty lists on zip-like non-empty lists.
usingNe :: (NonEmpty a ->
           NonEmpty b) ->
           ZipNonEmpty a ->
           ZipNonEmpty b
usingNe = (zipNe .) . (. ne)

-- | Runs a function for zip-like non-empty lists on non-empty lists.
usingZne :: (ZipNonEmpty a ->
            ZipNonEmpty b) ->
            NonEmpty a ->
            NonEmpty b
usingZne = (ne .) . (. zipNe)

instance (Show a) => Show (ZipNonEmpty a) where
  show = show . ne

instance Semigroup (ZipNonEmpty a) where
  Z a .++. Z b = Z (a .++. b)

instance Functor ZipNonEmpty where
  fmap = usingNe . fmap

instance Applicative ZipNonEmpty where
  pure = zipNe . unsafeToNonEmpty . repeat
  f <*> a = let z = toList . ne
            in zipNe . unsafeToNonEmpty $ zipWith id (z f) (z a)
