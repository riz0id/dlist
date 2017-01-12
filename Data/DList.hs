#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 708
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
#endif

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.DList
-- Copyright   :  (c) 2006-2009 Don Stewart, 2013-2017 Sean Leather
-- License     :  See LICENSE file
--
-- Maintainer  :  Sean Leather <sean.leather@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- Difference lists: a data structure for /O(1)/ append on lists.
--
-----------------------------------------------------------------------------

module Data.DList

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 800
  ( DList(Nil, Cons)
#else
  ( DList
#endif

  -- * Construction
  , fromList
  , toList
  , apply

  -- * Basic functions
  , empty
  , singleton
  , cons
  , snoc
  , append
  , concat
  , replicate
  , list
  , head
  , tail
  , unfoldr
  , foldr
  , map

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 708 && __GLASGOW_HASKELL__ < 800
  -- * Pattern Synonyms
  , pattern Nil
  , pattern Cons
#endif

  ) where

-- Hide conflicting names from Prelude
import Prelude ()

import Data.DList.Internal

-- | Apply a @DList@ to a list to get the underlying list
-- extended with the given list
--
-- > apply ('fromList' xs) ys = xs ++ ys

apply :: DList a -> [a] -> [a]
-- The reason for this function's existence is that we don't want to export
-- 'applyDList' because that allows one to construct an arbitrary @DList@.
apply = applyDList
{-# INLINE apply #-}
