{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-} -- For the IsList and IsString instances

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 708
{-# LANGUAGE PatternSynonyms #-} -- For Nil and Cons
{-# LANGUAGE ViewPatterns #-} -- For Nil and Cons
#endif

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.DList.Internal
-- Copyright   :  (c) 2006-2009 Don Stewart, 2013-2017 Sean Leather
-- License     :  See LICENSE file
--
-- Maintainer  :  Sean Leather <sean.leather@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- This module exists only to export the 'DList' constructor.
--
-- Note that unexpected things can happen if you don't construct values
-- carefully.
--
-- @DList a@ is isomorphic to @[a]@ via 'fromList'/'toList', but the type
-- underlying @DList@, @[a] -> [a]@, is not isomorphic to @[a]@. That is, all
-- lists are @DList@s, but all @DList@s are lists /only/ if there is no way to
-- construct an arbitrary @y :: DList a@ from an "unsafe" @x :: [a] -> [a]@.
--
-- Consider this GHCi session:
--
-- >>> let unexpected x = DList $ const $ [x, x]
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

module Data.DList.Internal where

import Prelude hiding (concat, foldr, map, head, tail, replicate)

import Control.DeepSeq (NFData(..))
import Control.Monad (ap, MonadPlus(..))
import Data.Function (on)
import Data.String (IsString(..))

import qualified Data.Foldable as F

#if !MIN_VERSION_base(4,8,0)
import Data.Monoid
import Data.Foldable (Foldable)
import Control.Applicative(Applicative(..))
#endif

#if MIN_VERSION_base(4,9,0)
import Data.Semigroup (Semigroup(..))
#endif

#ifdef __GLASGOW_HASKELL__

import Text.Read (Lexeme(Ident), lexP, parens, prec, readPrec, readListPrec,
                  readListPrecDefault)

#if __GLASGOW_HASKELL__ >= 708
import GHC.Exts (IsList)
import qualified GHC.Exts -- Avoid conflicts with fromList and toList
#endif

#endif

import Control.Applicative(Alternative, (<|>))
import qualified Control.Applicative (empty)

-- | A difference list is a function that, given a list, returns the original
-- contents of the difference list prepended to the given list.
--
-- This structure supports /O(1)/ append and snoc operations on lists, making it
-- very useful for append-heavy uses (esp. left-nested uses of 'List.++'), such
-- as logging and pretty printing.
--
-- Here is an example using DList as the state type when printing a tree with
-- the Writer monad:
--
-- > import Control.Monad.Writer
-- > import Data.DList
-- >
-- > data Tree a = Leaf a | Branch (Tree a) (Tree a)
-- >
-- > flatten_writer :: Tree x -> DList x
-- > flatten_writer = snd . runWriter . flatten
-- >     where
-- >       flatten (Leaf x)     = tell (singleton x)
-- >       flatten (Branch x y) = flatten x >> flatten y
--

newtype DList a = DList { applyDList :: [a] -> [a] }

-- | Convert a list to a @DList@

fromList :: [a] -> DList a
fromList = DList . (++)
{-# INLINE fromList #-}

-- | Convert a @DList@ to a list

toList :: DList a -> [a]
toList = ($[]) . applyDList
{-# INLINE toList #-}

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 708

-- | A unidirectional pattern synonym using 'toList' in a view pattern and
-- matching on @[]@

#if __GLASGOW_HASKELL__ >= 710
pattern Nil :: DList a
#endif
pattern Nil <- (toList -> [])

-- | A unidirectional pattern synonym using 'toList' in a view pattern and
-- matching on @x:xs@ such that you have the pattern @Cons x xs@

#if __GLASGOW_HASKELL__ >= 710
pattern Cons :: a -> [a] -> DList a
#endif
pattern Cons x xs <- (toList -> x : xs)

#endif

-- | Create a @DList@ with no elements

empty :: DList a
empty = DList id
{-# INLINE empty #-}

-- | Create a @DList@ with a single element

singleton :: a -> DList a
singleton = DList . (:)
{-# INLINE singleton #-}

-- | /O(1)/. Prepend a single element to a @DList@

cons :: a -> DList a -> DList a
cons x xs = DList $ (x:) . applyDList xs
{-# INLINE cons #-}
infixr `cons`

-- | /O(1)/. Append a single element to a @DList@

snoc :: DList a -> a -> DList a
snoc xs x = DList $ applyDList xs . (x:)
{-# INLINE snoc #-}
infixl `snoc`

-- | /O(1)/. Append one @DList@ to another @DList@

append :: DList a -> DList a -> DList a
append xs ys = DList $ applyDList xs . applyDList ys
{-# INLINE append #-}
infixr 5 `append`

-- | /O(spine)/. Concatenate a list of @DList@s

concat :: [DList a] -> DList a
concat = F.foldr append empty
{-# INLINE concat #-}

-- | /O(n)/. Create a @DList@ of the given number of elements

replicate :: Int -> a -> DList a
replicate n x = DList $ \xs ->
  let go m | m <= 0    = xs
           | otherwise = x : go (pred m)
  in go n
{-# INLINE replicate #-}

-- | /O(n)/. List elimination for @DList@

list :: b -> (a -> DList a -> b) -> DList a -> b
list n c dl = case toList dl of
  []     -> n
  x : xs -> c x $ fromList xs

-- | /O(n)/. Return the first element of a @DList@
--
-- Raises an error if the @DList@ is empty.

head :: DList a -> a
head = list (error "Data.DList.head: empty DList") const
{-# INLINE head #-}

-- | /O(n)/. Return the elements after the head of a @DList@
--
-- Raises an error if the @DList@ is empty.

tail :: DList a -> DList a
tail = list (error "Data.DList.tail: empty DList") (flip const)
{-# INLINE tail #-}

-- | /O(n)/. unfoldr for @DList@

unfoldr :: (b -> Maybe (a, b)) -> b -> DList a
unfoldr pf b = case pf b of
  Nothing      -> empty
  Just (a, b') -> cons a $ unfoldr pf b'

-- | /O(n)/. foldr for @DList@

foldr :: (a -> b -> b) -> b -> DList a -> b
foldr f b = F.foldr f b . toList
{-# INLINE foldr #-}

-- | /O(n)/. map for @DList@

map :: (a -> b) -> DList a -> DList b
map f = foldr (cons . f) empty
{-# INLINE map #-}

instance Eq a => Eq (DList a) where
  (==) = (==) `on` toList

instance Ord a => Ord (DList a) where
  compare = compare `on` toList

-- The Read and Show instances were adapted from Data.Sequence.

instance Read a => Read (DList a) where
#ifdef __GLASGOW_HASKELL__
  readPrec = parens $ prec 10 $ do
    Ident "fromList" <- lexP
    xs <- readPrec
    return $ fromList xs
  readListPrec = readListPrecDefault
#else
  readsPrec p = readParen (p > 10) $ \r -> do
    ("fromList", s) <- lex r
    (xs, t) <- readsPrec 11 s
    return (fromList xs, t)
#endif

instance Show a => Show (DList a) where
  showsPrec p xs = showParen (p > 10) $
    showString "fromList " . showsPrec 11 (toList xs)

instance Functor DList where
  fmap = map
  {-# INLINE fmap #-}

instance Applicative DList where
  pure = singleton
  {-# INLINE pure #-}
  (<*>) = ap
  {-# INLINE (<*>) #-}

instance Alternative DList where
  empty = empty
  {-# INLINE empty #-}
  (<|>) = append
  {-# INLINE (<|>) #-}

instance Monad DList where
  m >>= k
    -- = concat (toList (fmap k m))
    -- = (concat . toList . fromList . List.map k . toList) m
    -- = concat . List.map k . toList $ m
    -- = List.foldr append empty . List.map k . toList $ m
    -- = List.foldr (append . k) empty . toList $ m
    = foldr (append . k) empty m
  {-# INLINE (>>=) #-}

  return = pure
  {-# INLINE return #-}

  fail _ = empty
  {-# INLINE fail #-}

instance MonadPlus DList where
  mzero = empty
  {-# INLINE mzero #-}
  mplus = append
  {-# INLINE mplus #-}

instance Foldable DList where
  fold = mconcat . toList
  {-# INLINE fold #-}
  foldMap f = F.foldMap f . toList
  {-# INLINE foldMap #-}
  foldr f x = F.foldr f x . toList
  {-# INLINE foldr #-}
  foldl f x = F.foldl f x . toList
  {-# INLINE foldl #-}
  foldr1 f = F.foldr1 f . toList
  {-# INLINE foldr1 #-}
  foldl1 f = F.foldl1 f . toList
  {-# INLINE foldl1 #-}
-- CPP: foldl', foldr' added to Foldable in 7.6.1
-- http://www.haskell.org/ghc/docs/7.6.1/html/users_guide/release-7-6-1.html
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 706
  foldl' f x = F.foldl' f x . toList
  {-# INLINE foldl' #-}
  foldr' f x = F.foldr' f x . toList
  {-# INLINE foldr' #-}
#endif

instance NFData a => NFData (DList a) where
  rnf = rnf . toList
  {-# INLINE rnf #-}

-- The 'IsString' instance is _not_ a flexible instance to allow certain uses of
-- overloaded strings. See tests/OverloadedStrings.hs for an example and
-- https://git.haskell.org/ghc.git/commitdiff/b225b234a6b11e42fef433dcd5d2a38bb4b466bf
-- for the same change made to the 'IsString' instance for lists.

instance a ~ Char => IsString (DList a) where
  fromString = fromList
  {-# INLINE fromString #-}

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 708
instance IsList (DList a) where
  type Item (DList a) = a
  fromList = fromList
  {-# INLINE fromList #-}
  toList = toList
  {-# INLINE toList #-}
#endif

instance Monoid (DList a) where
  mempty  = empty
  {-# INLINE mempty #-}
  mappend = append
  {-# INLINE mappend #-}

#if MIN_VERSION_base(4,9,0)
instance Semigroup (DList a) where
  (<>) = append
  {-# INLINE (<>) #-}
  stimes n x
    | n < 0     = error "Data.DList.stimes: negative multiplier"
    | otherwise = rep n
    where
      rep 0 = empty
      rep i = x <> rep (pred i)
#endif
