{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-} -- For the IsList and IsString instances
{-# LANGUAGE BangPatterns #-} -- For nonEmptyFromList
{-# LANGUAGE PatternSynonyms #-} -- For Cons
{-# LANGUAGE ViewPatterns #-} -- For Cons

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.DList.DNonEmpty.Internal
-- Copyright   :  (c) 2017 Sean Leather, 2017 Oleg Grenrus
-- License     :  See LICENSE file
--
-- Maintainer  :  Sean Leather <sean.leather@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- This module exists only to export the 'DNonEmpty' constructor.
--
-- Note that unexpected things can happen if you don't construct values
-- carefully.
--
-- @DNonEmpty a@ is isomorphic to @NonEmpty a@ via 'fromNonEmpty'/'toNonEmpty',
-- but the type underlying @DNonEmpty@, @[a] -> NonEmpty a@, is not isomorphic
-- to @DNonEmpty a@. That is, all @NonEmpty@s are @DNonEmpty@s, but all
-- @DNonEmpty@s are @NonEmpty@s /only/ if there is no way to construct an
-- arbitrary @y :: DNonEmpty a@ from an "unsafe" @x :: [a] -> NonEmpty a@.
--
-- Consider this GHCi session:
--
-- >>> let unexpected x = DNonEmpty $ const $ x :| [x]
-- >>> let a = unexpected 'a'
-- >>> toList a
-- "aa"
-- >>> toList $ a `append` fromList "b"
-- "aa"
-- >>> toList $ (fromList . toList) a `append` fromList "b"
-- "aab"
--
-- Clearly, @a@ and @(fromList . toList) a@ are not equivalent.
--
-----------------------------------------------------------------------------

module Data.DList.DNonEmpty.Internal where

import Prelude hiding (map, head, tail, replicate)

import Control.DeepSeq (NFData(..))
import Control.Monad (ap)
import Data.Function (on)
import Data.List.NonEmpty (NonEmpty(..))
import Data.String (IsString(..))

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Foldable as F

import Data.Semigroup (Semigroup(..))

import Text.Read (Lexeme(Ident), lexP, parens, prec, readPrec, readListPrec,
                  readListPrecDefault)

import GHC.Exts (IsList)
import qualified GHC.Exts -- Avoid conflicts with fromList and toList

import Data.DList.Internal (DList(..))

-- | A difference list is a function that, given a list, returns the original
-- contents of the difference list prepended to the given list.

newtype DNonEmpty a = DNonEmpty { applyDNonEmpty :: [a] -> NonEmpty a }

-- | Convert a @NonEmpty@ to a @DNonEmpty@

fromNonEmpty :: NonEmpty a -> DNonEmpty a
fromNonEmpty (x :| xs) = DNonEmpty $ (x :|) . (xs ++)
{-# INLINE fromNonEmpty #-}

-- | Convert a @DNonEmpty@ to a @NonEmpty@

toNonEmpty :: DNonEmpty a -> NonEmpty a
toNonEmpty = ($[]) . applyDNonEmpty
{-# INLINE toNonEmpty #-}

-- | This is a version of 'NonEmpty.fromList' that allows us to substitute our
-- own message for the error. This helps the user to know where the problem is
-- coming from instead of redirecting them to 'NonEmpty.fromList'.

nonEmptyFromList :: String -> [a] -> NonEmpty a
nonEmptyFromList !msg = go
  where
    go (a : as) = a :| as
    go []       = errorWithoutStackTrace msg
{-# INLINE nonEmptyFromList #-}

-- | Convert a @DList@ to a @DNonEmpty@
--
-- Raises an error if the @DList@ is empty.

fromDList :: DList a -> DNonEmpty a
fromDList (DList f) =
  DNonEmpty $ nonEmptyFromList "Data.DList.DNonEmpty.fromDList: empty list" . f
{-# INLINE fromDList #-}

-- | Convert a @DNonEmpty@ to a @DList@

toDList :: DNonEmpty a -> DList a
toDList (DNonEmpty f) = DList $ NonEmpty.toList . f
{-# INLINE toDList #-}

-- | Convert a list to a @DNonEmpty@
--
-- Raises an error if the list is empty.

fromList :: [a] -> DNonEmpty a
fromList = fromNonEmpty . nonEmptyFromList "Data.DList.DNonEmpty.fromList: empty list"
{-# INLINE fromList #-}

-- | Convert a @DNonEmpty@ to a list

toList :: DNonEmpty a -> [a]
toList = NonEmpty.toList . toNonEmpty
{-# INLINE toList #-}

-- | A unidirectional pattern synonym using 'toNonEmpty' in a view pattern and
-- matching on @x :| xs@ such that you have the pattern @Cons x xs@

pattern Cons :: a -> [a] -> DNonEmpty a
pattern Cons x xs <- (toNonEmpty -> x :| xs)

-- | Create a @DNonEmpty@ with a single element

singleton :: a -> DNonEmpty a
singleton = DNonEmpty . (:|)
{-# INLINE singleton #-}

-- | /O(1)/. Prepend a single element to a @DNonEmpty@

cons :: a -> DNonEmpty a -> DNonEmpty a
cons x xs = DNonEmpty $ NonEmpty.cons x . applyDNonEmpty xs
{-# INLINE cons #-}
infixr `cons`

-- | /O(1)/. Append a single element to a @DNonEmpty@

snoc :: DNonEmpty a -> a -> DNonEmpty a
snoc xs x = DNonEmpty $ applyDNonEmpty xs . (x:)
{-# INLINE snoc #-}
infixl `snoc`

-- | /O(1)/. Append one @DNonEmpty@ to another @DNonEmpty@

append :: DNonEmpty a -> DNonEmpty a -> DNonEmpty a
append xs ys = DNonEmpty $ applyDNonEmpty xs . NonEmpty.toList . applyDNonEmpty ys
{-# INLINE append #-}
infixr 5 `append`

-- | /O(n)/. Create a @DNonEmpty@ of the given number of elements.
--
-- The resulting @DNonEmpty@ always has at least one element.

replicate :: Int -> a -> DNonEmpty a
replicate n x = DNonEmpty $ \xs ->
  let go m | m <= 1    = x :| xs
           | otherwise = NonEmpty.cons x $ go $ pred m
  in go n
{-# INLINE replicate #-}

-- | /O(n)/. Return the first element of a @DNonEmpty@

head :: DNonEmpty a -> a
head = NonEmpty.head . toNonEmpty
{-# INLINE head #-}

-- | /O(n)/. Return the elements after the head of a @DNonEmpty@

tail :: DNonEmpty a -> [a]
tail = NonEmpty.tail . toNonEmpty
{-# INLINE tail #-}

-- | /O(n)/. unfoldr for @DNonEmpty@

unfoldr :: (b -> (a, Maybe b)) -> b -> DNonEmpty a
unfoldr pf b = case pf b of
  (a, Nothing) -> singleton a
  (a, Just b') -> cons a $ unfoldr pf b'

-- | /O(n)/. map for @DNonEmpty@

map :: (a -> b) -> DNonEmpty a -> DNonEmpty b
map f = fromNonEmpty . fmap f . toNonEmpty
{-# INLINE map #-}

instance Eq a => Eq (DNonEmpty a) where
  (==) = (==) `on` toNonEmpty

instance Ord a => Ord (DNonEmpty a) where
  compare = compare `on` toNonEmpty

-- The Read and Show instances were adapted from Data.Sequence.

instance Read a => Read (DNonEmpty a) where
  readPrec = parens $ prec 10 $ do
    Ident "fromNonEmpty" <- lexP
    xs <- readPrec
    return $ fromNonEmpty xs
  readListPrec = readListPrecDefault

instance Show a => Show (DNonEmpty a) where
  showsPrec p xs = showParen (p > 10) $
    showString "fromNonEmpty " . showsPrec 11 (toNonEmpty xs)

instance Functor DNonEmpty where
  fmap = map
  {-# INLINE fmap #-}

instance Applicative DNonEmpty where
  pure = singleton
  {-# INLINE pure #-}
  (<*>) = ap
  {-# INLINE (<*>) #-}

instance Monad DNonEmpty where
  m >>= k = sconcat $ fmap k $ toNonEmpty m
  {-# INLINE (>>=) #-}

  return = pure
  {-# INLINE return #-}

instance Foldable DNonEmpty where
  fold = F.fold . toNonEmpty
  {-# INLINE fold #-}
  foldMap f = F.foldMap f . toNonEmpty
  {-# INLINE foldMap #-}
  foldr f x = F.foldr f x . toNonEmpty
  {-# INLINE foldr #-}
  foldl f x = F.foldl f x . toNonEmpty
  {-# INLINE foldl #-}
  foldr1 f = F.foldr1 f . toNonEmpty
  {-# INLINE foldr1 #-}
  foldl1 f = F.foldl1 f . toNonEmpty
  {-# INLINE foldl1 #-}
  foldl' f x = F.foldl' f x . toNonEmpty
  {-# INLINE foldl' #-}
  foldr' f x = F.foldr' f x . toNonEmpty
  {-# INLINE foldr' #-}

instance NFData a => NFData (DNonEmpty a) where
  rnf = rnf . toNonEmpty
  {-# INLINE rnf #-}

-- The 'IsString' instance is _not_ a flexible instance to allow certain uses of
-- overloaded strings. See tests/OverloadedStrings.hs for an example and
-- https://git.haskell.org/ghc.git/commitdiff/b225b234a6b11e42fef433dcd5d2a38bb4b466bf
-- for the same change made to the 'IsString' instance for lists.

-- | In this instance, 'fromString' is a partial function defined with
-- 'fromList'.

instance a ~ Char => IsString (DNonEmpty a) where
  fromString = fromList
  {-# INLINE fromString #-}

-- | In this instance, 'GHC.Exts.fromList' is a partial function.

instance IsList (DNonEmpty a) where
  type Item (DNonEmpty a) = a
  fromList = fromList
  {-# INLINE fromList #-}
  toList = toList
  {-# INLINE toList #-}

instance Semigroup (DNonEmpty a) where
  (<>) = append
  {-# INLINE (<>) #-}
