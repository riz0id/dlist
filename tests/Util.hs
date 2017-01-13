module Util where

import Test.QuickCheck (Property, (==>))

eqWith :: Eq b => (a -> b) -> (a -> b) -> a -> Bool
eqWith f g x = f x == g x

eqOn :: Eq b => (a -> Bool) -> (a -> b) -> (a -> b) -> a -> Property
eqOn c f g x = c x ==> f x == g x
