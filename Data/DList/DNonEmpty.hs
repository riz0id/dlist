
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.DList.DNonEmpty
-- Copyright   :  (c) 2006-2009 Don Stewart, 2013-2017 Sean Leather
-- License     :  See LICENSE file
--
-- Maintainer  :  Sean Leather <sean.leather@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- Non-empty difference lists: a data structure for /O(1)/ append on non-empty
-- lists.
--
-----------------------------------------------------------------------------

module Data.DList.DNonEmpty

  ( DNonEmpty(Cons)

  -- * Construction
  , fromNonEmpty
  , toNonEmpty
  , fromDList
  , toDList
  , fromList
  , toList
  , apply

  -- * Basic functions
  , singleton
  , cons
  , snoc
  , append
  , replicate
  , head
  , tail
  , unfoldr
  , map
  ) where

-- Hide conflicting names from Prelude
import Prelude ()

import Data.List.NonEmpty (NonEmpty)

import Data.DList.DNonEmpty.Internal

-- | Apply a @DNonEmpty@ to a list to get the underlying @NonEmpty@
-- extended with the given list
--
-- For a non-empty list @xs@, @apply@ obeys the following rule:
--
-- > apply ('fromList' xs) ys = xs ++ ys

apply :: DNonEmpty a -> [a] -> NonEmpty a
-- The reason for this function's existence is that we don't want to export
-- 'applyDNonEmpty' because that allows one to construct an arbitrary
-- @DNonEmpty@.
apply = applyDNonEmpty
{-# INLINE apply #-}
