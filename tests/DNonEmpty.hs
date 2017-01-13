{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedLists #-} -- For the IsList test
{-# LANGUAGE PatternSynonyms #-} -- For Cons

--------------------------------------------------------------------------------

module DNonEmpty (props) where

--------------------------------------------------------------------------------

import Prelude hiding (head, map, replicate, tail)

import Text.Show.Functions ()
import Test.QuickCheck (Property, property)

import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty

import Data.List.NonEmpty (NonEmpty(..), (<|))
import Data.Semigroup (Semigroup(..))

import qualified Data.DList as DList

import Data.DList.DNonEmpty
import Util

--------------------------------------------------------------------------------

prop_NonEmpty_iso :: NonEmpty Int -> Bool
prop_NonEmpty_iso = eqWith id (toNonEmpty . fromNonEmpty)

prop_DList_iso :: NonEmpty Int -> Bool
prop_DList_iso = eqWith id $
  NonEmpty.fromList . DList.toList . toDList . fromDList . DList.fromList . NonEmpty.toList

prop_singleton :: Int -> Bool
prop_singleton = eqWith (:| []) (toNonEmpty . singleton)

prop_cons :: Int -> NonEmpty Int -> Bool
prop_cons c = eqWith (c <|) (toNonEmpty . cons c . fromNonEmpty)

prop_snoc :: NonEmpty Int -> Int -> Bool
prop_snoc xs c = xs <> (c :| []) == toNonEmpty (fromNonEmpty xs `snoc` c)

prop_append :: NonEmpty Int -> NonEmpty Int -> Bool
prop_append xs ys =
  xs <> ys == toNonEmpty (fromNonEmpty xs `append` fromNonEmpty ys)

-- The condition reduces the size of replications and thus the eval time.
prop_replicate :: Int -> Int -> Property
prop_replicate n =
  eqOn (const (n > 0 && n < 100)) (NonEmpty.fromList . List.replicate n)
    (toNonEmpty . replicate n)

prop_head :: NonEmpty Int -> Bool
prop_head = eqWith NonEmpty.head (head . fromNonEmpty)

prop_tail :: NonEmpty Int -> Bool
prop_tail = eqWith NonEmpty.tail (tail . fromNonEmpty)

prop_unfoldr :: (Int -> (Int, Maybe Int)) -> Int -> Int -> Property
prop_unfoldr f n =
  eqOn (const (n >= 0)) (NonEmpty.take n . NonEmpty.unfoldr f)
    (NonEmpty.take n . toNonEmpty . unfoldr f)

prop_map :: (Int -> Int) -> NonEmpty Int -> Bool
prop_map f = eqWith (NonEmpty.map f) (toNonEmpty . map f . fromNonEmpty)

prop_map_map :: (Int -> Int) -> (a -> Int) -> NonEmpty a -> Bool
prop_map_map f g = eqWith (NonEmpty.map f . NonEmpty.map g) $
  toNonEmpty . map f . map g . fromNonEmpty

prop_show_read :: NonEmpty Int -> Bool
prop_show_read = eqWith id (read . show)

prop_read_show :: NonEmpty Int -> Bool
prop_read_show x = eqWith id (show . f . read) $
  "fromNonEmpty (" ++ show x ++ ")"
  where
    f :: DNonEmpty Int -> DNonEmpty Int
    f = id

-- | Test that the IsList instance methods compile and work with simple lists
prop_IsList :: Bool
prop_IsList = test_fromList [1,2,3] && test_toList (fromList [1,2,3])
  where
    test_fromList, test_toList :: DNonEmpty Int -> Bool
    test_fromList x = x == fromList [1,2,3]
    test_toList [1,2,3] = True
    test_toList _       = False

prop_patterns :: NonEmpty Int -> Bool
prop_patterns xs = case fromNonEmpty xs of
  Cons y ys -> xs == y :| ys
  _         -> False

prop_Semigroup_append :: NonEmpty Int -> NonEmpty Int -> Bool
prop_Semigroup_append xs ys = xs <> ys == toNonEmpty (fromNonEmpty xs <> fromNonEmpty ys)

prop_Semigroup_sconcat :: NonEmpty (NonEmpty Int) -> Bool
prop_Semigroup_sconcat xs = sconcat xs == toNonEmpty (sconcat (fmap fromNonEmpty xs))

prop_Semigroup_stimes :: Int -> NonEmpty Int -> Bool
prop_Semigroup_stimes n xs =
  n <= 0 || stimes n xs == toNonEmpty (stimes n (fromNonEmpty xs))

--------------------------------------------------------------------------------

props :: [(String, Property)]
props =
  [ ("NonEmpty isomorphism", property prop_NonEmpty_iso)
  , ("DList isomorphism",    property prop_DList_iso)
  , ("singleton",            property prop_singleton)
  , ("cons",                 property prop_cons)
  , ("snoc",                 property prop_snoc)
  , ("append",               property prop_append)
  , ("replicate",            property prop_replicate)
  , ("head",                 property prop_head)
  , ("tail",                 property prop_tail)
  , ("unfoldr",              property prop_unfoldr)
  , ("map",                  property prop_map)
  , ("map . map",            property (prop_map_map (+1) (+1)))
  , ("read . show",          property prop_show_read)
  , ("show . read",          property prop_read_show)
  , ("IsList",               property prop_IsList)
  , ("patterns",             property prop_patterns)
  , ("Semigroup <>",         property prop_Semigroup_append)
  , ("Semigroup sconcat",    property prop_Semigroup_sconcat)
  , ("Semigroup stimes",     property prop_Semigroup_stimes)
  ]
