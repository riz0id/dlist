
module Main (main) where

import Test.QuickCheck (conjoin, label, quickCheck)

import qualified DList
#if MIN_VERSION_base(4,9,0)
import qualified DNonEmpty
#endif
import qualified OverloadedStrings

main :: IO ()
main = do
  OverloadedStrings.test
  quickCheck $ conjoin $ map (uncurry label) DList.props
#if MIN_VERSION_base(4,9,0)
  quickCheck $ conjoin $ map (uncurry label) DNonEmpty.props
#endif
